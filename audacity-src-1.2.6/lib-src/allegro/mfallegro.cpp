// midifile reader for serpent

#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "assert.h"
#include "allegro.h"
#include "mfmidi.h"
#include "mfallegro.h"
#include "trace.h"
#include "memory.h" // for memcpy()


void Tracks::expand()
{
    max = (max + 5); // extra growth for small sizes
    max += (max >> 2); // add 25%
    Events_ptr *new_tracks = new Events_ptr[max];
    // now do copy
    memcpy(new_tracks, tracks, len * sizeof(Events_ptr));
    if (tracks) delete[] tracks;
    tracks = new_tracks;
}


void Tracks::append(Events_ptr track)
{
    if (max <= len) {
        expand();
    }
    tracks[len] = track;
    len++;
}


void Tracks::reset()
{
    // all track events are incorporated into the seq,
    // so all we need to delete are the arrays of pointers
    for (int i = 0; i < len; i++) {
        delete tracks[i];
    }
    if (tracks) delete [] tracks;
    tracks = NULL;
    len = 0;
}


Allegro_midifile_reader::~Allegro_midifile_reader()
{
    if (seq) delete seq;
    while (pending) {
        Pending_ptr to_be_freed = pending;
        pending = pending->next;
        delete to_be_freed;
    }
    tracks.reset();
}


void Allegro_midifile_reader::merge_tracks()
{
    double next;
    int track = -1;
    // keep an array of indexes into tracks
    long *current = new long[tracks.len];
    long sum = 0;
    long i;
    for (i = 0; i < tracks.len; i++) {
        current[i] = 0;
        sum = sum + tracks[i]->len;
    }
    // preallocate array for efficiency:
    Allegro_event_ptr *notes = new Allegro_event_ptr[sum];
    long notes_index = 0;

    bool done = false;
    while (!done) {
        Events_ptr tr;  // a track
        long cur;       // a track index
        // find lowest next time of any track:
        next = 1000000.0;
        for (i = 0; i < tracks.len; i++) {
            tr = tracks[i];
            cur = current[i];
            if (cur < tr->len && (*tr)[cur]->time < next) {
                next = (*tr)[cur]->time;
                track = i;
            }
        }
        // insert a track event
        if (next < 1000000.0) {
            notes[notes_index++] = (*tracks[track])[current[track]++];
        } else {
            done = true;
        }
    }
    tracks.reset(); // don't need them any more
    seq->set_events(notes, sum, sum);
    delete[] current;
}


void Allegro_midifile_reader::initialize(FILE *f)
{
    file = f;
    seq = new Seq;
    track_num = 0;
    midifile();
    merge_tracks();
}


void Allegro_midifile_reader::Mf_starttrack()
{
    printf("starting new track\n");
    track = new Events;
}


void Allegro_midifile_reader::Mf_endtrack()
{
    tracks.append(track);
    printf("finished track, length %d number %d\n", track->len, track_num / 100);
    track_num += 100;
    track = NULL;
}


int Allegro_midifile_reader::Mf_getc()
{
    return getc(file);
}


void Allegro_midifile_reader::Mf_eot()
{
}


void Allegro_midifile_reader::Mf_error(char *msg)
{
    fprintf(stdout, "Midifile reader error: %s\n", msg);
}


void Allegro_midifile_reader::Mf_header(int format, int ntrks, int division)
{
    if (format > 1) {
        char msg[80];
        sprintf(msg, "file format %d not implemented", format);
        Mf_error(msg);
    }
    divisions = division;
}


double Allegro_midifile_reader::get_time()
{
    double beat = ((double) get_currtime()) / divisions;
    return seq->map.beat_to_time(beat);
}


void Allegro_midifile_reader::Mf_on(int chan, int key, int vel)
{
    if (vel == 0) {
        Mf_off(chan, key, vel);
        return;
    }
    Allegro_note_ptr note = new Allegro_note();
    pending = new Pending(note, pending);
    /*trace("on: %d at %g\n", key, get_time());*/
    note->time = get_time();
    note->chan = chan;
    note->dur = 0;
    note->key = key;
    note->pitch = key;
    note->loud = vel;
    track->append(note);
}


void Allegro_midifile_reader::Mf_off(int chan, int key, int vel)
{
    double time = get_time();
    Pending_ptr *p = &pending;
    while (*p) {
        if ((*p)->note->key == key && (*p)->note->chan == chan) {
            (*p)->note->dur = time - (*p)->note->time;
            /*trace("updated %d dur %g\n", (*p)->note->key, (*p)->note->dur);*/
            Pending_ptr to_be_freed = *p;
            *p = to_be_freed->next;
            delete to_be_freed;
        } else {
            p = &((*p)->next);
        }
    }
}


void Allegro_midifile_reader::update(int chan, int key, Parameter_ptr param)
{
    Allegro_update_ptr update = new Allegro_update;
    update->time = get_time();
    update->chan = chan;
    update->key = key;
    update->parameter = *param;
    // prevent the destructor from destroying the string twice!
    // the new Update takes the string from param
    if (param->attr_type() == 's') param->s = NULL;
    track->append(update);
}


void Allegro_midifile_reader::Mf_pressure(int chan, int key, int val)
{
    Parameter parameter;
    parameter.set_attr(symbol_table.insert_string("pressurer"));
    parameter.r = val / 127.0;
    update(chan, key, &parameter);
}


void Allegro_midifile_reader::Mf_controller(int chan, int control, int val)
{
    Parameter parameter;
    char name[32];
    sprintf(name, "control%dr", control);
    parameter.set_attr(symbol_table.insert_string(name));
    parameter.r = val / 127.0;
    update(chan, -1, &parameter);
}


void Allegro_midifile_reader::Mf_pitchbend(int chan, int c1, int c2)
{
    Parameter parameter;
    parameter.set_attr(symbol_table.insert_string("bendr"));
    parameter.r = ((c1 << 7) + c2) / 8192.0 - 1.0;
    update(chan, -1, &parameter);
}


void Allegro_midifile_reader::Mf_program(int chan, int program)
{
    Parameter parameter;
    parameter.set_attr(symbol_table.insert_string("programi"));
    parameter.i = program;
    update(chan, -1, &parameter);
}


void Allegro_midifile_reader::Mf_chanpressure(int chan, int val)
{
    Parameter parameter;
    parameter.set_attr(symbol_table.insert_string("pressurr"));
    parameter.r = val / 127.0;
    update(chan, -1, &parameter);
}


void Allegro_midifile_reader::Mf_sysex(int len, char *msg)
{
    Mf_error("sysex message ignored - not implemented");
}


void Allegro_midifile_reader::Mf_arbitrary(int len, char *msg)
{
    Mf_error("arbitrary data ignored");
}


void Allegro_midifile_reader::Mf_metamisc(int type, int len, char *msg)
{
    Mf_error("metamisc data ignored");
}


void Allegro_midifile_reader::Mf_seqnum(int n)
{
    Mf_error("seqnum data ignored");
}


void Allegro_midifile_reader::Mf_smpte(int i1, int i2, int i3, int i4, int i5)
{
    Mf_error("SMPTE data ignored");
}


void Allegro_midifile_reader::Mf_timesig(int i1, int i2, int i3, int i4)
{
    Parameter num;
    num.set_attr(symbol_table.insert_string("timesig_numr"));
    num.r = i1;
    update(-1, -1, &num);
    Parameter den;
    den.set_attr(symbol_table.insert_string("timesig_denr"));
    den.r = 1 << i2;
    update(-1, -1, &den);
}


void Allegro_midifile_reader::Mf_tempo(int tempo)
{
    double beat = get_currtime();
    beat = beat / divisions; // convert to quarters
    // 6000000 us/min / n us/beat => beat / min
    double bps = 60000000.0 / tempo;
    seq->insert_tempo(bps, beat);
}


void Allegro_midifile_reader::Mf_keysig(int key, int mode)
{
    Parameter key_parm;
    key_parm.set_attr(symbol_table.insert_string("keysigi"));
    // use 0 for C major, 1 for G, -1 for F, etc., that is,
    // the number of sharps, where flats are negative sharps
    key_parm.i = key; //<<<---- fix this
    // use -1 to mean "all channels"
    update(-1, -1, &key_parm);
    Parameter mode_parm;
    mode_parm.set_attr(symbol_table.insert_string("modea"));
    mode_parm.a = (mode == 0 ? symbol_table.insert_string("majora") :
                               symbol_table.insert_string("minora"));
    update(-1, -1, &mode_parm);
}


void Allegro_midifile_reader::Mf_sqspecific(int len, char *msg)
{
    Mf_error("sq specific data ignored");
}


char *heapify2(int len, char *s)
{
    char *h = new char[len + 1];
    memcpy(h, s, len);
    h[len] = 0;
    return h;
}


void Allegro_midifile_reader::Mf_text(int type, int len, char *msg)
{
    Parameter text;
    text.s = heapify2(len, msg);
    char *attr = "miscs";
    if (type == 1) attr = "texts";
    else if (type == 2) attr = "copyrights";
    else if (type == 3) attr = "names";
    else if (type == 4) attr = "instruments";
    else if (type == 5) attr = "lyrics";
    else if (type == 6) attr = "markers";
    else if (type == 7) attr = "cues";
    text.set_attr(symbol_table.insert_string(attr));
    update(-1, -1, &text);
}


