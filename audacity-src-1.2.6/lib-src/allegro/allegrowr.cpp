// alegrowr.cpp -- write sequence to an Allegro file (text)

#include "stdlib.h"
#include "stdio.h"
#include "assert.h"
#include "allegro.h"
#include "allegrowr.h"
#include "string.h"
#include "strparse.h"


void parameter_print(FILE *file, Parameter_ptr p)
{
    char str[256];
    fprintf(file, " -%s:", p->attr_name());
    switch (p->attr_type()) {
    case 'a':
        fprintf(file, "%s", p->a);
        break;
    case 'i':
        fprintf(file, "%d", p->i);
        break;
    case 'l':
        fprintf(file, "%s", p->l ? "true" : "false");
        break;
    case 'r':
        fprintf(file, "%g", p->r);
        break;
    case 's':
        string_escape(str, p->s, "\"");
        fprintf(file, "%s", str);
        break;
    }
}


void allegro_write(Seq_ptr seq, FILE *file)
{
    int i;
    // first write the tempo map
    Beats &beats = seq->map.beats;
    for (i = 0; i < beats.len - 1; i++) {
        Beat_ptr b = &(beats[i]);
        fprintf(file, "TW%g ", seq->map.time_to_beat(b->time) / 4);
        double tempo = (beats[i + 1].beat - beats[i].beat) /
                       (beats[i + 1].time - beats[i].time);
        fprintf(file, "-tempor:%g\n", tempo * 60);
    }
    if (seq->map.last_tempo_flag) { // we have final tempo:
        double time = seq->map.time_to_beat(beats[beats.len - 1].time) / 4;
        fprintf(file, "TW%g ", time);
        fprintf(file, "-tempor:%g\n", seq->map.last_tempo * 60.0);
    }

    // now write the notes at beat positions
    for (i = 0; i < seq->notes.len; i++) {
        Allegro_event_ptr e = seq->notes[i];
        double start = seq->map.time_to_beat(e->time);
        fprintf(file, "TW%g", start / 4);
        if (e->chan != -1) {
            fprintf(file, " V%d", e->chan);
        }
        if (e->type == 'n') {
            Allegro_note_ptr n = (Allegro_note_ptr) e;
            double dur = seq->map.time_to_beat(n->time + n->dur) - start;
            fprintf(file, " K%d P%g Q%g L%g", n->key, n->pitch, dur, n->loud);
            Parameters_ptr p = n->parameters;
            while (p) {
                parameter_print(file, &(p->parm));
                p = p->next;
            }
        } else { // an update
            Allegro_update_ptr u = (Allegro_update_ptr) e;
            if (u->key != -1) {
                fprintf(file, " K%d", u->key);
            }
            parameter_print(file, &(u->parameter));
        }
        fprintf(file, "\n");
    }
}



