// Microsoft Windows specific include file

// These settings are used for official stable releases of Audacity
// with all of its support libraries included.  However, if you
// downloaded the source tarball of Audacity, some of these libraries
// might be missing, so there might be lines later in this file that
// disable support for these libraries.

// To disable an option, just comment it out, or #undef the flag.
// Setting a value to '0' will not work!

#define MP3SUPPORT 1
#define USE_LADSPA 1
#define USE_LIBID3TAG 1
#define USE_LIBMAD 1
#define USE_LIBRESAMPLE 1
// #define USE_LIBSAMPLERATE 1
#define USE_LIBVORBIS 1
#define USE_NYQUIST 1
#define USE_PORTMIXER 1
#define USE_SOUNDTOUCH 1
// #define USE_VST 1

#define INSTALL_PREFIX "."

#define rint(x)   (floor((x)+0.5f)) 


// The Audacity source tarball does NOT come with
// libmad, libid3tag, libogg, or libvorbis.

// Delete the following lines if you install them manually.

#undef MP3SUPPORT
#undef USE_LIBID3TAG
#undef USE_LIBMAD
#undef USE_LIBVORBIS
