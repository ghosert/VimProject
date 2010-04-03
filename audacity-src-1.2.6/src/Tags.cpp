/**********************************************************************

  Audacity: A Digital Audio Editor

  Tags.cpp

  Dominic Mazzoni

  This class holds a few informational tags, such as Title, Author,
  etc. that can be associated with a project or other audio file.
  It is modeled after the ID3 format for MP3 files, and it can
  both import ID3 tags from MP3 files, and export them as well.

  It can present the user with a dialog for editing this information.

  It only keeps track of the fields that are standard in ID3v1
  (title, author, artist, track num, year, genre, and comments),
  but it can export both ID3v1 or the newer ID3v2 format.  The primary
  reason you would want to export ID3v2 tags instead of ID3v1,
  since we're not supporting any v2 fields, is that ID3v2 tags are
  inserted at the BEGINNING of an mp3 file, which is far more
  useful for streaming.
  
  Use of this functionality requires that libid3tag be compiled in
  with Audacity.
  
**********************************************************************/

#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>

#include "Audacity.h"
#include "../Internat.h"

#ifdef USE_LIBID3TAG 
   #include <id3tag.h>
   // DM: the following functions were supposed to have been
   // included in id3tag.h - should be fixed in the next release
   // of mad.
   extern "C" {
      struct id3_frame *id3_frame_new(char const *);
      id3_length_t id3_latin1_length(id3_latin1_t const *);
      void id3_latin1_decode(id3_latin1_t const *, id3_ucs4_t *);
   } 
#endif

#include "Tags.h"

Tags::Tags()
{
   mTrackNum = -1;
   mGenre = -1;
   
   mID3V2 = true;

   mEditTitle = true;
   mEditTrackNumber = true;
}

Tags::~Tags()
{
}

bool Tags::IsEmpty()
{
   // At least one of these should be filled in, otherwise
   // it's assumed that the tags have not been set...
   if (mTitle.Length()==0 &&
       mArtist.Length()==0 &&
       mAlbum.Length()==0)
      return true;
   else
      return false;
}

void Tags::AllowEditTitle(bool editTitle)
{
   mEditTitle = editTitle;
}

void Tags::SetTitle(wxString title)
{
   mTitle = title;
}

wxString Tags::GetTitle()
{
   return mTitle;
}

void Tags::AllowEditTrackNumber(bool editTrackNumber)
{
   mEditTrackNumber = editTrackNumber;
}

void Tags::SetTrackNumber(int num)
{
   mTrackNum = num;
}

int Tags::GetTrackNumber()
{
   return mTrackNum;
}

bool Tags::HandleXMLTag(const char *tag, const char **attrs)
{
   if (strcmp(tag, "tags") != 0)
      return false;

   // loop through attrs, which is a null-terminated list of
   // attribute-value pairs
   while(*attrs) {
      const char *attr = *attrs++;
      const char *value = *attrs++;

      if (!value)
         break;

      if (!strcmp(attr, "title"))
         mTitle = value;
      else if (!strcmp(attr, "artist"))
         mArtist = value;
      else if (!strcmp(attr, "album"))
         mAlbum = value;
      else if (!strcmp(attr, "track"))
         mTrackNum = atoi(value);
      else if (!strcmp(attr, "year"))
         mYear = value;
      else if (!strcmp(attr, "genre"))
         mGenre = atoi(value);
      else if (!strcmp(attr, "comments"))
         mComments = value;
      else if (!strcmp(attr, "id3v2"))
         mID3V2 = atoi(value);         
   } // while
   
   return true;
}

XMLTagHandler *Tags::HandleXMLChild(const char *)
{
   return NULL;
}

void Tags::WriteXML(int depth, FILE *fp)
{
   int i;

   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "<tags ");
   fprintf(fp, "title=\"%s\" ", XMLEsc(mTitle).c_str());
   fprintf(fp, "artist=\"%s\" ", XMLEsc(mArtist).c_str());
   fprintf(fp, "album=\"%s\" ", XMLEsc(mAlbum).c_str());
   fprintf(fp, "track=\"%d\" ", mTrackNum);
   fprintf(fp, "year=\"%s\" ", XMLEsc(mYear).c_str());
   fprintf(fp, "genre=\"%d\" ", mGenre);
   fprintf(fp, "comments=\"%s\" ", XMLEsc(mComments).c_str());
   fprintf(fp, "id3v2=\"%d\" ", (int)mID3V2);
   fprintf(fp, "/>\n"); // XML shorthand for childless tag
}

bool Tags::ShowEditDialog(wxWindow *parent, wxString title)
{
#ifdef USE_LIBID3TAG 

   Tags theCopy;
   theCopy.mTitle = mTitle;
   theCopy.mArtist = mArtist;
   theCopy.mAlbum = mAlbum;
   theCopy.mTrackNum = mTrackNum;
   theCopy.mYear = mYear;
   theCopy.mGenre = mGenre;
   theCopy.mComments = mComments;
   theCopy.mID3V2 = mID3V2;

   TagsDialog dlog(parent, -1, title, mEditTitle, mEditTrackNumber);
   dlog.mTags = this;
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode()) {
      mTitle = theCopy.mTitle;
      mArtist = theCopy.mArtist;
      mAlbum = theCopy.mAlbum;
      mTrackNum = theCopy.mTrackNum;
      mYear = theCopy.mYear;
      mGenre = theCopy.mGenre;
      mComments = theCopy.mComments;
      mID3V2 = theCopy.mID3V2;

      return false;
   }

#endif // ifdef LIBID3TAG

   return true;
}

#ifdef USE_LIBID3TAG

/* Declare Static functions */
static wxString GetID3FieldStr(struct id3_tag *tp, const char *name);
static int GetNumGenres();
static wxString GetGenreNum(int i);

wxString GetID3FieldStr(struct id3_tag *tp, const char *name)
{
   struct id3_frame *frame;

   frame = id3_tag_findframe(tp, name, 0);
   if (frame) {
      const id3_ucs4_t *ustr;

      if (strcmp(name, ID3_FRAME_COMMENT) == 0)
	 ustr = id3_field_getfullstring(&frame->fields[3]);
      else
	 ustr = id3_field_getstrings(&frame->fields[1], 0);

      if (ustr) {
	 char *str = (char *)id3_ucs4_latin1duplicate(ustr);
	 wxString s = str;
	 free(str);
	 return s;
      }
   }

   return "";
}

#endif // ifdef USE_LIBID3TAG 

int GetNumGenres()
{
   return 148;
}

wxString GetGenreNum(int i)
{
#ifdef USE_LIBID3TAG
  id3_latin1_t      i_latin1[50];
  id3_ucs4_t       *i_ucs4;
  const id3_ucs4_t *genre_ucs4;

  sprintf((char *)i_latin1, "%d", i);
  i_ucs4 =
     (id3_ucs4_t *)malloc((id3_latin1_length(i_latin1) + 1) *
                          sizeof(*i_ucs4));
  if (i_ucs4) {
    id3_latin1_decode(i_latin1, i_ucs4);
    genre_ucs4 = id3_genre_name(i_ucs4);
    char *genre_char = (char *)id3_ucs4_latin1duplicate(genre_ucs4);
    wxString genreStr = genre_char;
    free(genre_char);
    free(i_ucs4);
    return genreStr;    
  }   

#endif // ifdef USE_LIBID3TAG 

  return "";
}

void Tags::ImportID3(wxString fileName)
{
#ifdef USE_LIBID3TAG 

   struct id3_file *fp = id3_file_open((const char *)FILENAME(fileName),
                                       ID3_FILE_MODE_READONLY);
   if (!fp) return;

   struct id3_tag *tp = id3_file_tag(fp);
   if (!tp) return;

   mTitle = GetID3FieldStr(tp, ID3_FRAME_TITLE);
   mArtist = GetID3FieldStr(tp, ID3_FRAME_ARTIST);
   mAlbum = GetID3FieldStr(tp, ID3_FRAME_ALBUM);   
   mYear = GetID3FieldStr(tp, ID3_FRAME_YEAR);
   mComments = GetID3FieldStr(tp, ID3_FRAME_COMMENT);

   long l;
   wxString s;
   if ((s = GetID3FieldStr(tp, ID3_FRAME_TRACK)).ToLong(&l))
      mTrackNum = l;

   s = GetID3FieldStr(tp, ID3_FRAME_GENRE);

   int numGenres = GetNumGenres();
   for(int i=0; i<numGenres; i++)
      if (0 == s.CmpNoCase(GetGenreNum(i)))
         mGenre = i;

   id3_file_close(fp);
#endif // ifdef USE_LIBID3TAG 
}

#ifdef USE_LIBID3TAG 

/* Declare Static functions */
static struct id3_frame *MakeID3Frame(const char *name, const char *data);

struct id3_frame *MakeID3Frame(const char *name, const char *data)
{
  struct id3_frame *frame;
  id3_latin1_t     *latin1;
  id3_ucs4_t       *ucs4;

  frame = id3_frame_new(name);

  latin1 = (id3_latin1_t *)data;
  ucs4 = (id3_ucs4_t *)malloc((id3_latin1_length(latin1) + 1) * sizeof(*ucs4));
  if (ucs4) {
    id3_latin1_decode(latin1, ucs4);

    if (strcmp(name, ID3_FRAME_COMMENT) == 0)
       id3_field_setfullstring(&frame->fields[3], ucs4);
    else
       id3_field_setstrings(&frame->fields[1], 1, &ucs4);

    free(ucs4);
  }

  return frame;
} 

#endif //ifdef USE_LIBID3TAG 

// returns buffer len; caller frees
int Tags::ExportID3(char **buffer, bool *endOfFile)
{
#ifdef USE_LIBID3TAG 
   struct id3_tag *tp = id3_tag_new();
   
   if (mTitle != "")
      id3_tag_attachframe(tp, MakeID3Frame(ID3_FRAME_TITLE, mTitle));

   if (mArtist != "")
      id3_tag_attachframe(tp, MakeID3Frame(ID3_FRAME_ARTIST, mArtist));

   if (mAlbum != "")
      id3_tag_attachframe(tp, MakeID3Frame(ID3_FRAME_ALBUM, mAlbum));

   if (mYear != "")
      id3_tag_attachframe(tp, MakeID3Frame(ID3_FRAME_YEAR, mYear));

   if (mComments != "")
      id3_tag_attachframe(tp, MakeID3Frame(ID3_FRAME_COMMENT, mComments));

   if (mTrackNum >= 0) {
      wxString trackNumStr;
      trackNumStr.Printf("%d", mTrackNum);
      id3_tag_attachframe(tp, MakeID3Frame(ID3_FRAME_TRACK, trackNumStr));
   }

   if (mGenre >= 0) {
      if (mID3V2) {
         wxString genreStr = GetGenreNum(mGenre);
         id3_tag_attachframe(tp, MakeID3Frame(ID3_FRAME_GENRE, genreStr));
      }
      else {
         wxString genreStr;
         genreStr.Printf("%d", mGenre);
         id3_tag_attachframe(tp, MakeID3Frame(ID3_FRAME_GENRE, genreStr));
      }
   }

   if (mID3V2) {
      tp->options &= (~ID3_TAG_OPTION_COMPRESSION); // No compression

      // If this version of libid3tag supports it, use v2.3 ID3
      // tags instead of the newer, but less well supported, v2.4
      // that libid3tag uses by default.
      #ifdef ID3_TAG_HAS_TAG_OPTION_ID3V2_3
      tp->options |= ID3_TAG_OPTION_ID3V2_3;
      #endif

      *endOfFile = false;      
   }
   else {
      tp->options |= ID3_TAG_OPTION_ID3V1;
      *endOfFile = true;
   }

   id3_length_t len;
   
   len = id3_tag_render(tp, 0);
   *buffer = (char *)malloc(len);
   len = id3_tag_render(tp, (id3_byte_t *)*buffer);

   id3_tag_delete(tp);

   return len;
#else //ifdef USE_LIBID3TAG 
   return 0;
#endif
}

//
// TagsDialog
//

BEGIN_EVENT_TABLE(TagsDialog, wxDialog)
    EVT_BUTTON(wxID_OK, TagsDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, TagsDialog::OnCancel)
END_EVENT_TABLE()

TagsDialog::TagsDialog(wxWindow * parent, wxWindowID id,
                       const wxString & title,
                       bool editTitle, bool editTrackNumber):
   wxDialog(parent, id, title)
{
   MakeTagsDialog(this, TRUE, TRUE);
   
   if (!editTitle)
      GetTitleText()->Enable(false);

   if (!editTrackNumber)
      GetTrackNumText()->Enable(false);
}

bool TagsDialog::Validate()
{
   wxString errorString =
      _("Maximum length of attribute '%s' is %d characters. Data was truncated.");

   if(!mTags->mID3V2)
   {
      if(mTags->mTitle.Length() > 30)
      {
         wxMessageBox(wxString::Format(errorString, _("Title"), 30));

         mTags->mTitle = mTags->mTitle.Left(30);
         TransferDataToWindow();

         return FALSE;
      }

      if(mTags->mArtist.Length() > 30)
      {
         wxMessageBox(wxString::Format(errorString, _("Artist"), 30));

         mTags->mArtist = mTags->mArtist.Left(30);
         TransferDataToWindow();

         return FALSE;
      }

      if(mTags->mAlbum.Length() > 30)
      {
         wxMessageBox(wxString::Format(errorString, _("Album"), 30));

         mTags->mAlbum = mTags->mAlbum.Left(30);
         TransferDataToWindow();

         return FALSE;
      }

      if(mTags->mYear.Length() > 4)
      {
         wxMessageBox(wxString::Format(errorString, _("Year"), 4));

         mTags->mYear = mTags->mYear.Left(4);
         TransferDataToWindow();

         return FALSE;
      }

      if(mTags->mComments.Length() > 30)
      {
         wxMessageBox(wxString::Format(errorString, _("Comments"), 30));

         mTags->mComments = mTags->mComments.Left(30);
         TransferDataToWindow();

         return FALSE;
      }
   }

   return TRUE;
}

bool TagsDialog::TransferDataToWindow()
{
   wxTextCtrl *text = GetTitleText();
   if (text) {
      text->SetValue(mTags->mTitle);
   }

   text = GetArtistText();
   if (text) {
      text->SetValue(mTags->mArtist);
   }
   
   text = GetAlbumText();
   if (text) {
      text->SetValue(mTags->mAlbum);
   }
   
   text = GetTrackNumText();
   if (text && mTags->mTrackNum != -1) {
      wxString numStr;
      numStr.Printf("%d", mTags->mTrackNum);
      text->SetValue(numStr);
   }

   text = GetYearText();
   if (text) {
      text->SetValue(mTags->mYear);
   }
   
   wxChoice *genre = GetGenreChoice();
   int numGenres = GetNumGenres();
   if (genre && mTags->mGenre>=0 && mTags->mGenre<numGenres)
      genre->SetSelection(mTags->mGenre);
   
   wxRadioBox *format = GetFormatRadioBox();
   if (format) {
      format->SetSelection((int)mTags->mID3V2);
   }
   
   text = GetCommentsText();
   if (text) {
      text->SetValue(mTags->mComments);
   }


   return TRUE;
}

bool TagsDialog::TransferDataFromWindow()
{
   wxTextCtrl *c = GetTitleText();
   if (c) {
      mTags->mTitle = c->GetValue();
   }

   c = GetArtistText();
   if (c) {
      mTags->mArtist = c->GetValue();
   }

   c = GetAlbumText();
   if (c) {
      mTags->mAlbum = c->GetValue();
   }

   c = GetTrackNumText();
   if (c) {
      wxString str = c->GetValue();
      if (str == "")
         mTags->mTrackNum = -1;
      else {
         long i;
         str.ToLong(&i);
         mTags->mTrackNum = i;
      }
   }

   c = GetYearText();
   if (c) {
      mTags->mYear = c->GetValue();
   }

   c = GetCommentsText();
   if (c) {
      mTags->mComments = c->GetValue();
   }
   
   wxChoice *genre = GetGenreChoice();
   if (genre)
      mTags->mGenre = genre->GetSelection();
   
   wxRadioBox *format = GetFormatRadioBox();
   if (format) {
      mTags->mID3V2 = (bool)format->GetSelection();
   }

   return TRUE;
}

void TagsDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();

   if (Validate())
      EndModal(true);
}

void TagsDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

wxSizer *MakeTagsDialog(wxWindow * parent, bool call_fit,
                        bool set_sizer)
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);

   /***/

   wxString formats[2];
   formats[0] = _("ID3v1 (more compatible)");
   formats[1] = _("ID3v2 (more flexible)");

   wxRadioBox *format = new wxRadioBox(parent, ID_FORMAT, _("Format:"),
                                       wxDefaultPosition, wxDefaultSize,
                                       2, formats,
                                       0, wxRA_VERTICAL);
   mainSizer->Add(format, 1, wxEXPAND | wxALL, 5);

   /***/
   
   wxFlexGridSizer *gridSizer = new wxFlexGridSizer(2, 0, 0);

   wxStaticText *item3 =
       new wxStaticText(parent, ID_TEXT, _("Title:"),
                        wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxTextCtrl *item4 =
       new wxTextCtrl(parent, ID_TITLE_TEXT, "", wxDefaultPosition,
                      wxSize(200, -1), 0);
   gridSizer->Add(item4, 1, wxEXPAND | wxALL, 5);

   wxStaticText *item5 =
       new wxStaticText(parent, ID_TEXT, _("Artist:"),
                        wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item5, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxTextCtrl *item6 =
       new wxTextCtrl(parent, ID_ARTIST_TEXT, "", wxDefaultPosition,
                      wxSize(200, -1), 0);
   gridSizer->Add(item6, 1, wxEXPAND | wxALL, 5);

   wxStaticText *item7 =
       new wxStaticText(parent, ID_TEXT, _("Album:"),
                        wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item7, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxTextCtrl *item8 =
       new wxTextCtrl(parent, ID_ALBUM_TEXT, "", wxDefaultPosition,
                      wxSize(200, -1), 0);
   gridSizer->Add(item8, 1, wxEXPAND | wxALL, 5);

   mainSizer->Add(gridSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   /***/
   
   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *item9 =
       new wxStaticText(parent, ID_TEXT, _("Track Number:"),
                        wxDefaultPosition, wxDefaultSize, 0);
   hSizer->Add(item9, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item10 =
       new wxTextCtrl(parent, ID_TRACK_NUM_TEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   hSizer->Add(item10, 0, wxALIGN_CENTRE | wxALL, 5);

   wxStaticText *item11 =
       new wxStaticText(parent, ID_TEXT, _("Year:"),
                        wxDefaultPosition, wxDefaultSize, 0);
   hSizer->Add(item11, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item12 =
       new wxTextCtrl(parent, ID_YEAR_TEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   hSizer->Add(item12, 0, wxALIGN_CENTRE | wxALL, 5);
   
   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   /***/
   gridSizer = new wxFlexGridSizer(2, 0, 0);

   wxStaticText *item20 =
       new wxStaticText(parent, ID_TEXT, _("Genre:"),
                        wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item20, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   int numGenres = GetNumGenres();
   wxString *genres = new wxString[numGenres];
   for(int i=0; i<numGenres; i++)
      genres[i] = GetGenreNum(i);

   wxChoice *item21 =
       new wxChoice(parent, ID_GENRE,
                    wxDefaultPosition, wxSize(-1, -1),
                    numGenres, genres);
   item21->SetSelection(0);
   gridSizer->Add(item21, 1, wxEXPAND | wxALL, 5);
   delete[] genres;
   
   wxStaticText *item22 =
       new wxStaticText(parent, ID_TEXT, _("Comments:"),
                        wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item22, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxTextCtrl *item23 =
       new wxTextCtrl(parent, ID_COMMENTS_TEXT, "", wxDefaultPosition,
                      wxSize(200, -1), 0);
   gridSizer->Add(item23, 1, wxEXPAND | wxALL, 5);
   
   mainSizer->Add(gridSizer, 0, wxALIGN_CENTRE | wxALL, 5);
   
   /***/
   
   wxBoxSizer *okSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton *item15 =
       new wxButton(parent, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
   okSizer->Add(item15, 0, wxALIGN_CENTRE | wxALL, 5);

   wxButton *item14 =
       new wxButton(parent, wxID_OK, _("OK"), wxDefaultPosition,
                    wxDefaultSize, 0);
   item14->SetDefault();
   item14->SetFocus();
   okSizer->Add(item14, 0, wxALIGN_CENTRE | wxALL, 5);

   mainSizer->Add(okSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   if (set_sizer) {
      parent->SetAutoLayout(TRUE);
      parent->SetSizer(mainSizer);
      if (call_fit) {
         mainSizer->Fit(parent);
         mainSizer->SetSizeHints(parent);
      }
   }
   
   return mainSizer;
}

