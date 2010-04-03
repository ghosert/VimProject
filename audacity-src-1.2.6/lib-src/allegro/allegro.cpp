// Allegro: music representation system, with
//      extensible in-memory sequence structure
//      upward compatible with MIDI
//      implementations in C++ and Serpent
//      external, text-based representation
//      compatible with Aura

#include "assert.h"
#include "stdlib.h"
#include "allegro.h"
#include "string.h"
//#include "memory.h"
#include "trace.h"

Atoms symbol_table;

bool within(double d1, double d2, double epsilon)
{
    d1 -= d2;
    return d1 < epsilon && d1 > -epsilon;
}


void Events::expand()
{
    max = (max + 5); // extra growth for small sizes
    max += (max >> 2); // add 25%
    Allegro_event_ptr *new_events = new Allegro_event_ptr[max];
    // now do copy
    memcpy(new_events, events, len * sizeof(Allegro_event_ptr));
    if (events) delete[] events;
    events = new_events;
}


Events::~Events()
{
    if (events) {
        delete[] events;
    }
}


void Events::insert(Allegro_event_ptr event)
{
    if (max <= len) {
        expand();
    }
    events[len] = event;
    len++;
    // find insertion point:
    for (int i = 0; i < len; i++) {
        if (events[i]->time > event->time) {
            // insert event at i
            memmove(&events[i + 1], &events[i], 
                    sizeof(Allegro_event_ptr) * (len - i - 1));
            events[i] = event;
            return;
        }
    }
}


void Events::append(Allegro_event_ptr event)
{
    if (max <= len) {
        expand();
    }
    events[len++] = event;
}


void Atoms::expand()
{
    max = (max + 5); // extra growth for small sizes
    max += (max >> 2); // add 25%
    char **new_atoms = new Attribute[max];
    // now do copy
    memcpy(new_atoms, atoms, len * sizeof(Attribute));
    if (atoms) delete[] atoms;
    atoms = new_atoms;
}


char *heapify(char *s)
{
    char *h = new char[strlen(s) + 1];
    strcpy(h, s);
    return h;
}


Attribute Atoms::insert_new(const char *name, char attr_type)
{
    if (len == max) expand();
    char *h = new char[strlen(name) + 2];
    strcpy(h + 1, name);
    *h = attr_type;
    atoms[len++] = h;
    return h;
}


Attribute Atoms::insert_attribute(Attribute attr)
{
    for (int i = 0; i < len; i++) {
        if (strcmp(attr, atoms[i]) == 0) {
            return atoms[i];
        }
    }
    return insert_new(attr + 1, attr[0]);
}


Attribute Atoms::insert_string(const char *name)
{
    char attr_type = name[strlen(name) - 1];
    for (int i = 0; i < len; i++) {
        if (attr_type == atoms[i][0] &&
            strcmp(name, atoms[i] + 1) == 0) {
            return atoms[i];
        }
    }
    return insert_new(name, attr_type);
}


void Parameters::insert_real(Parameters **list, char *name, double r)
{
    Parameters_ptr a = new Parameters(*list);
    *list = a;
    a->parm.set_attr(symbol_table.insert_string(name));
    a->parm.r = r;
    assert(a->parm.attr_type() == 'r');
}


void Parameters::insert_string(Parameters **list, char *name, char *s)
{
    Parameters_ptr a = new Parameters(*list);
    *list = a;
    a->parm.set_attr(symbol_table.insert_string(name));
    // string is deleted when parameter is deleted
    a->parm.s = heapify(s);
    assert(a->parm.attr_type() == 's');
}

void Parameters::insert_integer(Parameters **list, char *name, long i)
{
    Parameters_ptr a = new Parameters(*list);
    *list = a;
    a->parm.set_attr(symbol_table.insert_string(name));
    a->parm.i = i;
    assert(a->parm.attr_type() == 'i');
}

void Parameters::insert_logical(Parameters **list, char *name, bool l)
{
    Parameters_ptr a = new Parameters(*list);
    *list = a;
    a->parm.set_attr(symbol_table.insert_string(name));
    a->parm.l = l;
    assert(a->parm.attr_type() == 'l');
}

void Parameters::insert_atom(Parameters **list, char *name, char *s)
{
    Parameters_ptr a = new Parameters(*list);
    *list = a;
    a->parm.set_attr(symbol_table.insert_string(name));
    a->parm.a = symbol_table.insert_string(s);
    assert(a->parm.attr_type() == 'a');
}


Parameters *Parameters::remove_key(Parameters **list, char *name)
{
    while (*list) {
        if (strcmp((*list)->parm.attr_name(), name) == 0) {
            Parameters_ptr p = *list;
            *list = p->next;
            p->next = NULL;
            return p; // caller should free this pointer
        }
        *list = (*list)->next;
    }
    return NULL;
}


Parameter::~Parameter()
{
    if (attr_type() == 's' && s) delete[] s;
}


Allegro_note::~Allegro_note()
{
    while (parameters) {
        Parameters_ptr to_delete = parameters;
        parameters = parameters->next;
        delete to_delete;
    }
}


void Beats::expand()
{
    max = (max + 5); // extra growth for small sizes
    max += (max >> 2); // add 25%
    Beat_ptr new_beats = new Beat[max];
    // now do copy
    memcpy(new_beats, beats, len * sizeof(Beat));
    if (beats) delete[] beats;
    beats = new_beats;
}


void Beats::insert(long i, Beat_ptr beat)
{
    assert(i >= 0 && i <= len);
    if (max <= len) {
        expand();
    }
    memmove(&beats[i], &beats[i + 1], sizeof(Beat) * (len - i));
    memcpy(&beats[i], beat, sizeof(Beat));
    len++;
}




long Time_map::locate_time(double time)
{
    int i = 0;
    while ((i < beats.len) && (time > beats[i].time)) {
        i++;
    }
    return i;
}


long Time_map::locate_beat(double beat)
{
    int i = 0;
    while (( i < beats.len) && (beat > beats[i].beat)) {
        i++;
    }
    return i;
}


double Time_map::beat_to_time(double beat)
{
    Beat_ptr mbi;
    Beat_ptr mbi1;
    if (beat <= 0) {
        return beat;
    }
    int i = locate_beat(beat);
    if (i == beats.len) {
        if (last_tempo_flag) {
            return beats[i - 1].time + 
                   (beat - beats[i - 1].beat) / last_tempo;
        } else if (i == 1) {
            return beat * 0.6;
        } else {
            mbi = &beats[i - 2];
            mbi1 = &beats[i - 1];
        }
    } else {
        mbi = &beats[i - 1];
        mbi1 = &beats[i];
    }
    // whether w extrapolate or interpolate, the math is the same
    double time_dif = mbi1->time - mbi->time;
    double beat_dif = mbi1->beat - mbi->beat;
    return mbi->time + (beat - mbi->beat) * time_dif / beat_dif;
}


double Time_map::time_to_beat(double time)
{
    Beat_ptr mbi;
    Beat_ptr mbi1;
    if (time <= 0.0) return time;
    int i = locate_time(time);
    if (i == beats.len) {
        if (last_tempo_flag) {
            return beats[i - 1].beat + 
                   (time - beats[i - 1].time) * last_tempo;
        } else if (i == 1) {
            return time / 0.6;
        } else {
            mbi = &beats[i - 2];
            mbi1 = &beats[i - 1];
        }
    } else {
        mbi = &beats[i - 1];
        mbi1 = & beats[i];
    }
    double time_dif = mbi1->time - mbi->time;
    double beat_dif = mbi1->beat - mbi->beat;
    return mbi->beat + (time - mbi->time) * beat_dif / time_dif;
}
         

void Time_sigs::expand()
{
    max = (max + 5); // extra growth for small sizes
    max += (max >> 2); // add 25%
    Time_sig_ptr new_time_sigs = new Time_sig[max];
    // now do copy
    memcpy(new_time_sigs, time_sigs, len * sizeof(Time_sig));
    if (time_sigs) delete[] time_sigs;
    time_sigs = new_time_sigs;
}


void Time_sigs::insert(double beat, double num, double den)
{
    if (max <= len) {
        expand();
    }
    time_sigs[len].beat = beat;
    time_sigs[len].num = num;
    time_sigs[len].den = den;
    len++;
    // find insertion point:
    for (int i = 0; i < len; i++) {
        if (time_sigs[i].beat > beat) {
            // insert event at i
            memmove(&time_sigs[i], &time_sigs[i + 1], 
                    sizeof(Time_sig) * (len - i));
            time_sigs[i].beat = beat;
            time_sigs[i].num = num;
            time_sigs[i].den = den;
        }
        return;
    }
}


Seq::~Seq()
{
    for (int i = 0; i < notes.len; i++)
        delete notes.events[i];
}


long Seq::seek_time(double time)
// find index of first score event after time
{
    long i;

    for (i = 0; i < notes.len; i++) {
        if (notes[i]->time > time) {
            break;
        }
    }
    return i;
}


void Seq::convert_to_beats()
// modify all times and durations in notes to beats
{
    if (units_are_seconds) {
        units_are_seconds = false;
        for (long i = 0; i < notes.len; i++) {
            Allegro_event_ptr e = notes[i];
            double beat = map.time_to_beat(e->time);
            if (e->type == 'n') {
                Allegro_note_ptr n = (Allegro_note_ptr) e;
                n->dur = map.time_to_beat(n->time + n->dur) - beat;
                n->time = beat;
            }
        }
    }
}


void Seq::convert_to_seconds()
// modify all times and durations in notes to seconds
{
    if (!units_are_seconds) {
        units_are_seconds = true;
        for (long i = 0; i < notes.len; i++) {
            Allegro_event_ptr e = notes[i];
            double time = map.beat_to_time(e->time);
            if (e->type == 'n') {
                Allegro_note_ptr n = (Allegro_note_ptr) e;
                n->dur = map.beat_to_time(n->time + n->dur) - time;
                n->time = time;
            }
        }
    }
}


bool Seq::insert_beat(double time, double beat)
// insert a time,beat pair
// return true or false (false indicates an error, no update)
// it is an error to imply a negative tempo or to insert at
// a negative time
{
    if (time < 0 || beat < 0) return false;
    if (time == 0.0 && beat > 0)
        time = 0.000001; // avoid infinite tempo, offset time by 1us
    if (time == 0.0 && beat == 0.0)
        return true; // (0,0) is already in the map!
    convert_to_beats(); // beats are invariant when changing tempo
    int i = map.locate_time(time); // i is insertion point
    if (i < map.beats.len && within(map.beats[i].time, time, 0.000001)) {
        // replace beat if time is already in the map
        map.beats[i].beat = beat;
    } else {
        Beat point;
        point.beat = beat;
        point.time = time;
        map.beats.insert(i, &point);
    }
    // beats[i] contains new beat
    // make sure we didn't generate a zero tempo.
    // if so, space beats by one microbeat as necessary
    long j = i;
    while (j < map.beats.len &&
        map.beats[j - 1].beat + 0.000001 >= map.beats[j].beat) {
        map.beats[j].beat = map.beats[j - 1].beat + 0.000001;
        j++;
    }
    return true;
}


bool Seq::insert_tempo(double tempo, double beat)
{
    tempo = tempo / 60.0; // convert to beats per second
    // change the tempo at the given beat until the next beat event
    if (beat < 0) return false;
    convert_to_beats(); // beats are invariant when changing tempo
    double time = map.beat_to_time(beat);
    long i = map.locate_time(time);
    if (i >= map.beats.len || !within(map.beats[i].time, time, 0.000001)) {
        insert_beat(time, beat);
    }
    // now i is index of beat where tempo will change
    if (i == map.beats.len - 1) {
        map.last_tempo = tempo;
        map.last_tempo_flag = true;
    } else { // adjust all future beats
        // compute the difference in beats
        double diff = map.beats[i + 1].beat - map.beats[i].beat;
        // convert beat difference to seconds at new tempo
        diff = diff /tempo;
        // figure out old time difference:
        double old_diff = map.beats[i + 1].time - time;
        // compute difference too
        diff = diff - old_diff;
        // apply new_diff to score and beats
        while (i < map.beats.len) {
            map.beats[i].time = map.beats[i].time + diff;
            i++;
        }
    }
    return true;
}


void Seq::add_event(Allegro_event_ptr event)
{
    convert_to_seconds();
    notes.insert(event);
/*
    if (event->type == 'n') {
        Allegro_note_ptr n = (Allegro_note_ptr) event;
        trace("note %d at %g for %g\n", n->key, n->time, n->dur);
    }
 */
}


bool Seq::set_tempo(double tempo, double start_beat, double end_beat)
// set tempo from start_beat to end_beat
{
    if (start_beat >= end_beat) return false;
    convert_to_beats();
    // algorithm: insert a beat event if necessary at start_beat
    //    and at end_beat
    // delete intervening map elements
    // change the tempo
    insert_beat(map.beat_to_time(start_beat), start_beat);
    insert_beat(map.beat_to_time(end_beat), end_beat);
    long start_x = map.locate_beat(start_beat) + 1;
    long stop_x = map.locate_beat(end_beat) + 1;
    while (stop_x < map.beats.len) {
        map.beats[start_x] = map.beats[stop_x];
        start_x++;
        stop_x++;
    }
    map.beats.len = start_x; // truncate the map to new length
    return insert_tempo(tempo, start_beat);
}


void Seq::set_time_sig(double beat, double num, double den)
{
    time_sig.insert(beat, num, den);
}


void Seq::beat_to_measure(double beat, long *measure, double *m_beat,
                          double *num, double *den)
{
    // return [measure, beat, num, den]
    double m = 0; // measure number
    double bpm;
    int tsx;

    for (tsx = 0; tsx < time_sig.len; tsx++) {
        bpm = 4;
        // assume 4/4 if no time signature
        double prev_beat = 0;
        double prev_num = 4;
        double prev_den = 4;
        if (tsx > 0) {
            bpm = time_sig[tsx].num * 4 / time_sig[tsx].den;
            prev_beat = time_sig[tsx].beat;
            prev_num = time_sig[tsx].num;
            prev_den = time_sig[tsx].den;
        }
        if (time_sig[tsx].beat > beat) {
            m = m + (beat - prev_beat) / bpm;
            *measure = (long) m;
            *m_beat = (m - *measure) * bpm * prev_den * 0.25;
            *num = prev_num;
            *den = prev_den;
            return;
        }
        // round m up to an integer (but allow for a small
        // numerical inaccuracy)
        m = m + (long) (0.99 + (time_sig[tsx].beat - prev_beat) / bpm);
    }
    // if we didn't return yet, compute after last time signature
    Time_sig initial(0, 4, 4);
    Time_sig_ptr prev = &initial;
    if (tsx > 0) { // use last time signature
        prev = &time_sig[time_sig.len - 1];
    }
    bpm = prev->num * 4 / prev->den;
    m = m + (beat - prev->beat) / bpm;
    *measure = (long) m;
    *m_beat = (m - *measure) * bpm * prev->den * 0.25;
    *num = prev->num;
    *den = prev->den;
}


void Seq::set_events(Allegro_event_ptr *events, long len, long max)
{
    convert_to_seconds(); // because notes are in seconds
    notes.set_events(events, len, max);
}


// sr_letter_to_type = {"i": 'Integer', "r": 'Real', "s": 'String',
//                     "l": 'Logical', "a": 'Symbol'}
