/**********************************************************************

  Audacity: A Digital Audio Editor

  Tags.h

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

#include "Audacity.h"

#ifndef __AUDACITY_TAGS__
#define __AUDACITY_TAGS__

#include <wx/string.h>
#include <wx/choice.h>
#include <wx/radiobox.h>
#include <wx/textctrl.h>
#include <wx/panel.h>
#include <wx/dialog.h>

#include "xml/XMLTagHandler.h"

class Tags: public XMLTagHandler {

   friend class TagsDialog;

public:
   Tags();  // constructor
   virtual ~Tags();
   
   bool ShowEditDialog(wxWindow *parent, wxString title);
   
   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

   // Import any ID3 tags from this file   
   void ImportID3(wxString fileName);
   
   // returns buffer len; caller frees
   // if endOfFile is true, should put the ID3 tags at
   // the END, rather than the beginning, of the MP3 file
   int ExportID3(char **buffer, bool *endOfFile); 
   
   void AllowEditTitle(bool editTitle);
   void SetTitle(wxString title);
   wxString GetTitle();

   void AllowEditTrackNumber(bool editTrackNumber);
   void SetTrackNumber(int trackNumber);
   int GetTrackNumber();
   
   bool IsEmpty();

private:
   wxString  mTitle;
   wxString  mArtist;
   wxString  mAlbum;
   int       mTrackNum;
   wxString  mYear;
   int       mGenre;
   wxString  mComments;
   bool      mID3V2;

   bool      mEditTitle;
   bool      mEditTrackNumber;
};

wxSizer *MakeTagsDialog(wxWindow * parent, bool call_fit,
                        bool set_sizer);
                        
#define ID_TEXT 10000
#define ID_TITLE_TEXT 10001
#define ID_ARTIST_TEXT 10002
#define ID_ALBUM_TEXT 10003
#define ID_TRACK_NUM_TEXT 10004
#define ID_YEAR_TEXT 10005
#define ID_COMMENTS_TEXT 10006
#define ID_GENRE 10007
#define ID_FORMAT 10008

class TagsDialog:public wxDialog {

 public:
   // constructors and destructors
   TagsDialog(wxWindow * parent, wxWindowID id,
              const wxString & title,
              bool editTitle, bool editTrackNumber);

   wxTextCtrl *GetTitleText() {
      return (wxTextCtrl *) FindWindow(ID_TITLE_TEXT);
   }
   wxTextCtrl *GetArtistText() {
      return (wxTextCtrl *) FindWindow(ID_ARTIST_TEXT);
   }
   wxTextCtrl *GetAlbumText() {
      return (wxTextCtrl *) FindWindow(ID_ALBUM_TEXT);
   }
   wxTextCtrl *GetTrackNumText() {
      return (wxTextCtrl *) FindWindow(ID_TRACK_NUM_TEXT);
   }
   wxTextCtrl *GetYearText() {
      return (wxTextCtrl *) FindWindow(ID_YEAR_TEXT);
   }
   wxTextCtrl *GetCommentsText() {
      return (wxTextCtrl *) FindWindow(ID_COMMENTS_TEXT);
   }
   wxChoice *GetGenreChoice() {
      return (wxChoice *) FindWindow(ID_GENRE);
   }
   wxRadioBox *GetFormatRadioBox() {
      return (wxRadioBox *) FindWindow(ID_FORMAT);
   }
   
   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   DECLARE_EVENT_TABLE()

 public:
   Tags *mTags;
};

#endif
