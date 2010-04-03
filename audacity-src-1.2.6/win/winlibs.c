// Configures linker settings on Microsoft Windows / MSW, depending
// on which libraries are configure in "configwin.h".

#include "configwin.h"

#if USE_LIBRESAMPLE
#ifdef _DEBUG
#pragma comment(lib, "libresampled.lib")
#else
#pragma comment(lib, "libresample.lib")
#endif
#endif

#if USE_SOUNDTOUCH
#ifdef _DEBUG
#pragma comment(lib, "soundtouchd.lib")
#else
#pragma comment(lib, "soundtouch.lib")
#endif
#endif

#if USE_PORTMIXER
#ifdef _DEBUG
#pragma comment(lib, "portmixerd.lib")
#else
#pragma comment(lib, "portmixer.lib")
#endif
#endif

#if USE_NYQUIST
#ifdef _DEBUG
#pragma comment(lib, "libnyquistd.lib")
#else
#pragma comment(lib, "libnyquist.lib")
#endif
#endif

#if USE_LIBID3TAG
#ifdef _DEBUG
#pragma comment(lib, "libid3tagd.lib")
#else
#pragma comment(lib, "libid3tag.lib")
#endif
#endif

#if USE_LIBMAD
#ifdef _DEBUG
#pragma comment(lib, "madd.lib")
#else
#pragma comment(lib, "mad.lib")
#endif
#endif

#if USE_LIBVORBIS
#ifdef _DEBUG
#pragma comment(lib, "ogg_static_d.lib")
#pragma comment(lib, "vorbis_static_d.lib")
#pragma comment(lib, "vorbisfile_static_d.lib")
#else
#pragma comment(lib, "ogg_static.lib")
#pragma comment(lib, "vorbis_static.lib")
#pragma comment(lib, "vorbisfile_static.lib")
#endif
#endif
