// Macintosh-specific include file

// Audacity's four-character "creator" code and project type code
#define AUDACITY_CREATOR      'auDy'
#define AUDACITY_PROJECT_TYPE 'auDp'

#ifdef __MACH__

# define __MACOSX__
# include <wx/defs.h>
# include "../src/configunix.h"

#else

# define __MACOS9__
# define MP3SUPPORT        // Use libmad for importing MP3 on the mac
# define USE_LIBMAD
# define USE_LIBVORBIS     // Ogg Vorbis support (?)
# define USE_LIBID3TAG     // ID3 support

# define USE_NYQUIST

#define INSTALL_PREFIX "."

#endif
