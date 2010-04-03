/**********************************************************************

  Audacity: A Digital Audio Editor

  FileFormats.h

  Dominic Mazzoni

**********************************************************************/

#include <wx/list.h>
#include <wx/string.h>

#include "Audacity.h"

#include "sndfile.h"

//
// enumerating headers
//

int sf_num_headers();
wxString sf_header_index_name(int format_num);
unsigned int sf_header_index_to_type(int format_num);

//
// enumerating encodings
//

int sf_num_encodings();
wxString sf_encoding_index_name(int encoding_num);
unsigned int sf_encoding_index_to_subtype(int encoding_num);

//
// getting info about an actual SF format
//

wxString sf_header_name(int format);
wxString sf_header_shortname(int format);
wxString sf_header_extension(int format);
wxString sf_encoding_name(int encoding_num);

//
// simple formats
//

int sf_num_simple_formats();
SF_FORMAT_INFO *sf_simple_format(int i);

//
// other utility functions
//

bool sf_subtype_more_than_16_bits(unsigned int format);
bool sf_subtype_is_integer(unsigned int format);

void sf_get_all_extensions(wxStringList exts);

//
// Mac OS 4-char type
//

#ifdef __WXMAC__
# ifdef __UNIX__
#  include <CoreServices/CoreServices.h>
# else
#  include <Types.h>
# endif

OSType sf_header_mactype(int format);
#endif


