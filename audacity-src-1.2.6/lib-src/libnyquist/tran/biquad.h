sound_type snd_make_biquad(sound_type s, double b0, double b1, double b2, double a1, double a2, double z1init, double z2init);
sound_type snd_biquad(sound_type s, double b0, double b1, double b2, double a1, double a2, double z1init, double z2init);
    /* LISP: (snd-biquad SOUND ANYNUM ANYNUM ANYNUM ANYNUM ANYNUM ANYNUM ANYNUM) */
