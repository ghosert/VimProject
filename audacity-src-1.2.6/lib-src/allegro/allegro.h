// Allegro: music representation system, with
//      extensible in-memory sequence structure
//      upward compatible with MIDI
//      implementations in C++ and Serpent
//      external, text-based representation
//      compatible with Aura

#ifndef __ALLEGRO__
#define __ALLEGRO__
#include <assert.h>

#ifdef NEED_BOOL
#define bool unsigned char
#define true 1
#define false 0
#endif

// are d1 and d2 within epsilon of each other?
bool within(double d1, double d2, double epsilon);

// abstract superclass of Allegro_note and Allegro_update:
typedef class Allegro_event {
public:
    char type; // 'e' event, 'n' note, 'u' update
    double time;
    long chan;
    virtual ~Allegro_event() {}
} *Allegro_event_ptr;


// a sequence of Event objects
typedef class Events {
private:
    long max;
    void expand();
public:
    long len;
    Allegro_event_ptr *events; // events is array of pointers
    Allegro_event_ptr &operator[](int i) {
       /* assert(i >= 0 && i < len);  dmazzoni */
       if (i<0) i=0;
       if (i>=len) i=len-1;
       return events[i];
    }
    Events() {
        max = 0;
        len = 0;
        events = NULL;
    }
    ~Events();
    void insert(Allegro_event_ptr event);
    void append(Allegro_event_ptr event);
    void set_events(Allegro_event_ptr *e, long l, long m) {
        if (events) delete [] events;
        events = e; len = l; max = m; }
} *Events_ptr;


// Attribute is an atom in the symbol table
// with the special addition that the last
// character is prefixed to the string; thus,
// the attribute 'tempor' (a real) is stored
// as 'rtempor'. To get the string name, just
// use attribute+1.
typedef char *Attribute;


// Atoms is a symbol table of Attributes
class Atoms {
public:
    Atoms() {
        max = len = 0;
        atoms = NULL;
    }
    // insert/lookup an atttribute
    Attribute insert_attribute(Attribute attr);
    // insert/lookup attribute by name (without prefixed type)
    Attribute insert_string(const char *name);
private:
    long max;
    long len;
    char **atoms;

    // insert an Attriubute not in table after moving attr to heap
    Attribute insert_new(const char *name, char attr_type);
    void expand(); // make more space
};

extern Atoms symbol_table;


// an attribute/value pair. Since Attribute names imply type,
// we try to keep attributes and values packaged together as
// Parameter class
typedef class Parameter {
private:
    Attribute attr;
public:
    virtual ~Parameter();
    union {
        double r;// real
        char *s; // string
        long i;  // integer
        bool l;  // logical
        char *a; // symbol (atom)
    }; // anonymous union
    char attr_type() { return attr[0]; }
    char *attr_name() { return attr + 1; }
    void set_attr(Attribute a) { attr = a; }
} *Parameter_ptr;


// a list of attribute/value pairs
typedef class Parameters {
public:
    class Parameters *next;
    Parameter parm;

    Parameters(Parameters *list) {
        next = list;
    }
    // each of these routines takes address of pointer to the list
    static void insert_real(Parameters **list, char *name, double r);
    // insert string will copy string to heap
    static void insert_string(Parameters **list, char *name, char *s);
    static void insert_integer(Parameters **list, char *name, long i);
    static void insert_logical(Parameters **list, char *name, bool l);
    static void insert_atom(Parameters **list, char *name, char *s);
    static Parameters *remove_key(Parameters **list, char *name);
} *Parameters_ptr;



typedef class Allegro_note: public Allegro_event {
public:
    virtual ~Allegro_note();
    long key;     // note identifier
    double pitch; // pitch in semitones (69 = A440)
    double dur;   // duration in seconds (normally to release point)
    double loud;  // dynamic corresponding to MIDI velocity
    Parameters_ptr parameters; // attribute/value pair list

    Allegro_note() { type = 'n'; parameters = NULL; }
} *Allegro_note_ptr;


typedef class Allegro_update: public Allegro_event {
public:
    virtual ~Allegro_update() {};
    long key;     // note identifier (what sound is to be updated?)
    Parameter parameter; // an update contains one attr/value pair
   
    Allegro_update() { type = 'u'; }
} *Allegro_update_ptr;

// Beat is used to contruct a tempo map
typedef class Beat {
public:
    double time;
    double beat;
} *Beat_ptr;


// Beats is a list of Beat objects used in Seq
typedef class Beats {
private:
    long max;
    void expand();
public:
    long len;
    Beat_ptr beats;
    Beat &operator[](int i) {
       if (i<0) i=0;
       if (i>=len) i=len-1;
       /* assert(i >= 0 && i < len); dmazzoni*/
       return beats[i];
    }
    Beats() {
        max = len = 0;
        beats = NULL;
        expand();
        beats[0].time = 0;
        beats[0].beat = 0;
        len = 1;
    }
    ~Beats() {
        if (beats) delete[] beats;
    }
    void insert(long i, Beat_ptr beat);
} *Beats_ptr;


class Time_map {
public:
    Beats beats; // array of Beat
    double last_tempo;
    bool last_tempo_flag;
    Time_map() {
        last_tempo = 1.0;
        last_tempo_flag = false;
    };
    long locate_time(double time);
    long locate_beat(double beat);
    double beat_to_time(double beat);
    double time_to_beat(double time);
};


// Time_sig represents a single time signature;
// although not recommended, time_signatures may have arbitrary
// floating point values, e.g. 4.5 beats per measure
typedef class Time_sig {
public:
    double beat; // when does this take effect?
    double num;  // what is the "numerator" (top number?)
    double den;  // what is the "denominator" (bottom number?)
    Time_sig(double b, double n, double d) {
        beat = b; num = n; den = d;
    }
    Time_sig() {
        beat = 0; num = 0; den = 0;
    }
    void beat_to_measure(double beat, double *measure, double *m_beat,
                         double *num, double *den);

} *Time_sig_ptr;


// Time_sigs is a dynamic array of time signatures
class Time_sigs {
private:
    long max;
    void expand(); // make more space
public:
    long len;
    Time_sig_ptr time_sigs;
    Time_sigs() {
        max = len = 0;
        time_sigs = NULL;
    }
    Time_sig &operator[](int i) {
        assert(i >= 0 && i < len);
        return time_sigs[i];
    }
    ~Time_sigs() {
        if (time_sigs) delete[] time_sigs;
    }
    void insert(double beat, double num, double den);
};

// A Seq is a sequence of Event, with a tempo map and 
// a sequence of time signatures
//
typedef class Seq {
public:
    Events notes; // array of Event
    Time_map map;
    Time_sigs time_sig;
    int beat_x;
    bool units_are_seconds;
    Seq() {
        units_are_seconds = true;
    }
    ~Seq();

    long seek_time(double time);
    void convert_to_beats();
    void convert_to_seconds();
    bool insert_beat(double time, double beat);
    bool insert_tempo(double tempo, double beat);
    void add_event(Allegro_event_ptr event);
    bool set_tempo(double tempo, double start_beat, double end_beat);
    void set_time_sig(double beat, double num, double den);
    void beat_to_measure(double beat, long *measure, double *m_beat,
                         double *num, double *den);
    void set_events(Allegro_event_ptr *events, long len, long max);
} *Seq_ptr;

char *heapify(char *s);  // put a string on the heap

#endif
