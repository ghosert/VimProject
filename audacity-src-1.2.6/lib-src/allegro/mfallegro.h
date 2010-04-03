// midifile reader interface for serpent

// a sequence of Events objects
class Tracks {
private:
    long max;
    void expand();
public:
    long len;
    Events_ptr *tracks; // tracks is array of pointers
    Events_ptr &operator[](int i) {
        assert(i >= 0 && i < len);
        return tracks[i];
    }
    Tracks() {
        max = len = 0;
        tracks = NULL;
    }
    void append(Events_ptr track);
    void reset();
};


typedef class Pending {
public:
    Allegro_note_ptr note;
    class Pending *next;
    Pending(Allegro_note_ptr n, class Pending *list) { note = n; next = list; }
} *Pending_ptr;


class Allegro_midifile_reader: public Midifile_reader {
public:
    FILE *file;
    Seq_ptr seq;
    int divisions;
    Pending_ptr pending;
    Tracks tracks;
    Events_ptr track;
    int track_num;

    Allegro_midifile_reader() { file = NULL; pending = NULL; }
    // delete destroys the seq member as well, so set it to NULL if you
    // copied the pointer elsewhere
    virtual ~Allegro_midifile_reader();
    // the following is used to load the Seq from the file:
    void initialize(FILE *file);

    void set_nomerge(bool flag) { Mf_nomerge = flag; }
    void set_skipinit(bool flag) { Mf_skipinit = flag; }
    long get_currtime() { return Mf_currtime; }



protected:
    void merge_tracks();
    double get_time();
    void update(int chan, int key, Parameter_ptr param);
    void *Mf_malloc(size_t size) { return malloc(size); }
    void Mf_free(void *obj, size_t size) { free(obj); }
    /* Methods to be called while processing the MIDI file. */
    void Mf_starttrack();
    void Mf_endtrack();
    int Mf_getc();
    void Mf_eot();
    void Mf_error(char *);
    void Mf_header(int,int,int);
    void Mf_on(int,int,int);
    void Mf_off(int,int,int);
    void Mf_pressure(int,int,int);
    void Mf_controller(int,int,int);
    void Mf_pitchbend(int,int,int);
    void Mf_program(int,int);
    void Mf_chanpressure(int,int);
    void Mf_sysex(int,char*);
    void Mf_arbitrary(int,char*);
    void Mf_metamisc(int,int,char*);
    void Mf_seqnum(int);
    void Mf_smpte(int,int,int,int,int);
    void Mf_timesig(int,int,int,int);
    void Mf_tempo(int);
    void Mf_keysig(int,int);
    void Mf_sqspecific(int,char*);
    void Mf_text(int,int,char*);
};

