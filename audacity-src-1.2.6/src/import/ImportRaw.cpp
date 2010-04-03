/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportRaw.cpp

  Dominic Mazzoni

**********************************************************************/

#include "ImportRaw.h"
#include "Import.h"

#include "RawAudioGuess.h"

#include "../DirManager.h"
#include "../FileFormats.h"
#include "../Internat.h"
#include "../Prefs.h"
#include "../WaveTrack.h"

#include <math.h>

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/panel.h>
#include <wx/progdlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/timer.h>

#include "sndfile.h"

class ImportRawDialog:public wxDialog {

  public:
   ImportRawDialog(wxWindow * parent,
                   int encoding, int channels,
                   int offset, double rate);
   ~ImportRawDialog();

   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnPlay(wxCommandEvent & event);
   void OnChoice(wxCommandEvent & event);

   // in and out
   int mEncoding;
   int mChannels;
   int mOffset;
   double mRate;
   double mPercent;

 private:

   wxButton   *mOK;
   wxChoice   *mEncodingChoice;
   wxChoice   *mEndianChoice;
   wxChoice   *mChannelChoice;
   wxTextCtrl *mOffsetText;
   wxTextCtrl *mPercentText;
   wxTextCtrl *mRateText;

   DECLARE_EVENT_TABLE()
};

int ImportRaw(wxWindow *parent, wxString fileName,
              TrackFactory *trackFactory, Track ***outTracks,
              progress_callback_t progressCallback, void *userData)
{
   int encoding = 0; // Guess Format
   int numChannels = 0;
   sampleFormat format;
   sf_count_t offset = 0;
   int int_offset = 0;
   longSampleCount totalFrames;
   double rate = 44100.0;
   double percent = 100.0;
   SNDFILE *sndFile;
   SF_INFO sndInfo;
   int result;

   encoding = RawAudioGuess((const char *)fileName,
                            &int_offset, &numChannels);
   offset = (sf_count_t)int_offset;

   if (encoding <= 0) {
      // Unable to guess.  Use mono, 16-bit samples with CPU endianness
      // as the default.
      encoding = SF_FORMAT_RAW | SF_ENDIAN_CPU | SF_FORMAT_PCM_16;
      numChannels = 1;
      offset = 0;
   }

   ImportRawDialog dlog(parent, encoding, numChannels, (int)offset, rate);
   dlog.ShowModal();
   if (!dlog.GetReturnCode())
      return false;

   encoding = dlog.mEncoding;
   numChannels = dlog.mChannels;
   rate = dlog.mRate;
   offset = (sf_count_t)dlog.mOffset;
   percent = dlog.mPercent;

   memset(&sndInfo, 0, sizeof(SF_INFO));
   sndInfo.samplerate = (int)rate;
   sndInfo.channels = (int)numChannels;
   sndInfo.format = encoding | SF_FORMAT_RAW;
   sndFile = sf_open((const char *)FILENAME(fileName), SFM_READ, &sndInfo);
   if (!sndFile) {
      // TODO: Handle error
      char str[1000];
      sf_error_str((SNDFILE *)NULL, str, 1000);
      printf("%s\n", str);

      return 0;
   }

   result = sf_command(sndFile, SFC_SET_RAW_START_OFFSET, &offset, sizeof(offset));
   if (result != 0) {
      char str[1000];
      sf_error_str(sndFile, str, 1000);
      printf("%s\n", str);
   }

   sf_seek(sndFile, 0, SEEK_SET);
   
   totalFrames = (longSampleCount)(sndInfo.frames * percent / 100.0);

   //
   // Sample format:
   //
   // In general, go with the user's preferences.  However, if
   // the file is higher-quality, go with a format which preserves
   // the quality of the original file.
   //
   
   format = (sampleFormat)
      gPrefs->Read("/SamplingRate/DefaultProjectSampleFormat", floatSample);

   if (format != floatSample &&
       sf_subtype_more_than_16_bits(encoding))
      format = floatSample;

   WaveTrack **channels = new WaveTrack *[numChannels];

   int c;
   for (c = 0; c < numChannels; c++) {
      channels[c] = trackFactory->NewWaveTrack(format);
      channels[c]->SetRate(rate);

      if (numChannels > 1)
         switch (c) {
         case 0:
            channels[c]->SetChannel(Track::LeftChannel);
            break;
         case 1:
            channels[c]->SetChannel(Track::RightChannel);
            break;
         default:
            channels[c]->SetChannel(Track::MonoChannel);
         }
   }

   if (numChannels == 2)
      channels[0]->SetLinked(true);

   sampleCount maxBlockSize = channels[0]->GetMaxBlockSize();
   bool cancelled = false;

   samplePtr srcbuffer = NewSamples(maxBlockSize * numChannels, format);
   samplePtr buffer = NewSamples(maxBlockSize, format);
   
   longSampleCount framescompleted = 0;
   
   long block;
   do {
      block = maxBlockSize;

      if (block + framescompleted > totalFrames)
         block = totalFrames - framescompleted;
      
      if (format == int16Sample)
         block = sf_readf_short(sndFile, (short *)srcbuffer, block);
      else
         block = sf_readf_float(sndFile, (float *)srcbuffer, block);

      if (block) {
         for(c=0; c<numChannels; c++) {
            if (format==int16Sample) {
               for(int j=0; j<block; j++)
                  ((short *)buffer)[j] =
                     ((short *)srcbuffer)[numChannels*j+c];
            }
            else {
               for(int j=0; j<block; j++)
                  ((float *)buffer)[j] =
                     ((float *)srcbuffer)[numChannels*j+c];
            }
            
            channels[c]->Append(buffer, format, block);
         }
         framescompleted += block;
      }

      if( progressCallback )
         cancelled = progressCallback(userData, framescompleted*1.0 / totalFrames);
      if (cancelled)
         break;
      
   } while (block > 0 && framescompleted < totalFrames);

   sf_close(sndFile);

   if (cancelled) {
      for (c = 0; c < numChannels; c++)
         delete channels[c];
      delete[] channels;

      return 0;
   }
   else {
      for (c = 0; c < numChannels; c++)
         channels[c]->Flush();

      *outTracks = (Track **)channels;

      return numChannels;
   }
}

//
// ImportRawDialog
//

enum {
   ChoiceID = 9000,
   PlayID
};

BEGIN_EVENT_TABLE(ImportRawDialog, wxDialog)
   EVT_BUTTON(wxID_OK, ImportRawDialog::OnOK)
   EVT_BUTTON(wxID_CANCEL, ImportRawDialog::OnCancel)
   EVT_BUTTON(PlayID, ImportRawDialog::OnPlay)
   EVT_CHOICE(ChoiceID, ImportRawDialog::OnChoice)
END_EVENT_TABLE()

ImportRawDialog::ImportRawDialog(wxWindow * parent,
                                 int encoding, int channels,
                                 int offset, double rate)
   :wxDialog(parent, -1, _("Import Raw Data"),
             wxDefaultPosition, wxDefaultSize,
             wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    mEncoding(encoding), mChannels(channels),
    mOffset(offset), mRate(rate)
{
   wxBoxSizer *mainSizer, *hSizer, *vSizer;
   wxFlexGridSizer *gridSizer;
   wxButton *button;
   int selection, i;

   mainSizer = new wxBoxSizer(wxVERTICAL);
   
   hSizer = new wxBoxSizer(wxHORIZONTAL);

   //
   // Left-Side Controls (for manually selecting format)
   //

   vSizer = new wxBoxSizer(wxVERTICAL);

   // Encoding Choice

   wxString *encodingStrings = new wxString[sf_num_encodings()];
   selection = 0;
   for(i=0; i<sf_num_encodings(); i++) {
      encodingStrings[i] = sf_encoding_index_name(i);
      if ((mEncoding & SF_FORMAT_SUBMASK) == (int)sf_encoding_index_to_subtype(i))
         selection = i;
   }
   mEncodingChoice = 
      new wxChoice(this, ChoiceID,
                   wxDefaultPosition, wxDefaultSize,
                   sf_num_encodings(), encodingStrings);
   mEncodingChoice->SetSelection(selection);
   vSizer->Add(mEncodingChoice, 0, wxALIGN_LEFT | wxALL, 5);
   delete[] encodingStrings;

   // Endian choice

   wxString endianStrings[4] =
      {/* i18n-hint: Refers to byte-order.  Don't translate "endianness" if you don't
          know the correct technical word. */
         _("No endianness"),
         /* i18n-hint: Refers to byte-order.  Don't translate this if you don't
          know the correct technical word. */
       _("Little-endian"),
         /* i18n-hint: Refers to byte-order.  Don't translate this if you don't
            know the correct technical word. */
       _("Big-endian"),
         /* i18n-hint: Refers to byte-order.  Don't translate "endianness" if you don't
            know the correct technical word. */
       _("Default endianness")};
   mEndianChoice =
      new wxChoice(this, ChoiceID,
                   wxDefaultPosition, wxDefaultSize,
                   4, endianStrings);
   switch(mEncoding & (SF_FORMAT_ENDMASK)) {
   case SF_ENDIAN_FILE:
      mEndianChoice->SetSelection(0);
      break;
   case SF_ENDIAN_LITTLE:
      mEndianChoice->SetSelection(1);
      break;
   case SF_ENDIAN_BIG:
      mEndianChoice->SetSelection(2);
      break;
   case SF_ENDIAN_CPU:
      mEndianChoice->SetSelection(3);
      break;
   }
   vSizer->Add(mEndianChoice, 0, wxALIGN_LEFT | wxALL, 5);

   // Channels choice

   wxString channelStrings[16];
   channelStrings[0].Printf(_("1 Channel (Mono)"));
   channelStrings[1].Printf(_("2 Channels (Stereo)"));
   for(i=2; i<16; i++) {
      channelStrings[i].Printf(_("%d Channels"), i+1);
   }
   mChannelChoice =
      new wxChoice(this, ChoiceID,
                   wxDefaultPosition, wxDefaultSize,
                   16, channelStrings);
   mChannelChoice->SetSelection(mChannels-1);
   vSizer->Add(mChannelChoice, 0, wxALIGN_LEFT | wxALL, 5);

   // Offset text

   gridSizer = new wxFlexGridSizer(3, 0, 0);
   
   gridSizer->Add(new wxStaticText(this, 0, _("Start offset:")),
                  0, wxALIGN_LEFT | wxALL, 5);

   mOffsetText = new wxTextCtrl(this, 0, wxString::Format("%d", mOffset));
   gridSizer->Add(mOffsetText, 0, wxALIGN_LEFT | wxALL, 5);

   gridSizer->Add(new wxStaticText(this, 0, _("bytes")),
                  0, wxALIGN_LEFT | wxALL, 5);

   // Percent text

   gridSizer->Add(new wxStaticText(this, 0, _("Amount to import:")),
                  0, wxALIGN_LEFT | wxALL, 5);

   mPercentText = new wxTextCtrl(this, 0, "100");
   gridSizer->Add(mPercentText, 0, wxALIGN_LEFT | wxALL, 5);

   gridSizer->Add(new wxStaticText(this, 0, "%"),
                  0, wxALIGN_LEFT | wxALL, 5);

   // Rate text

   gridSizer->Add(new wxStaticText(this, 0, _("Sample rate:")),
                  0, wxALIGN_LEFT | wxALL, 5);

   mRateText = new wxTextCtrl(this, 0, wxString::Format("%d", (int)mRate));
   gridSizer->Add(mRateText, 0, wxALIGN_LEFT | wxALL, 5);

   /* i18n-hint: This is the abbreviation for "Hertz", or
      cycles per second. */
   gridSizer->Add(new wxStaticText(this, 0, _("Hz")),
                  0, wxALIGN_LEFT | wxALL, 5);

   vSizer->Add(gridSizer, 0, wxALIGN_LEFT | wxALL, 0);

   hSizer->Add(vSizer, 0, wxALIGN_TOP | wxALL, 5);

   //
   // Preview Pane goes here
   //

   wxPanel *p = new wxPanel(this, 0, wxDefaultPosition, wxSize(100, 100));
   hSizer->Add(p, 0, wxALIGN_TOP | wxALL, 5);

   mainSizer->Add(hSizer, 0, wxALIGN_CENTER | wxALL, 5);

   //
   // Button row
   //

   hSizer = new wxBoxSizer(wxHORIZONTAL);

   /*
   button = new wxButton(this, PlayID, _("Play"));
   hSizer->Add(button, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
   */

   button = new wxButton(this, wxID_CANCEL, _("Cancel"));
   hSizer->Add(button, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   mOK = new wxButton(this, wxID_OK, _("Import"));
   hSizer->Add(mOK, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   mainSizer->Add(hSizer, 0, wxALIGN_CENTER | wxALL, 5);

   SetAutoLayout(true);
   SetSizer(mainSizer);

   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);

   Centre(wxBOTH);
}

ImportRawDialog::~ImportRawDialog()
{
}

void ImportRawDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   long l;
   
   mEncoding = sf_encoding_index_to_subtype(mEncodingChoice->GetSelection());
   mEncoding += (mEndianChoice->GetSelection() * 0x10000000);
   mChannels = mChannelChoice->GetSelection() + 1;
   mOffsetText->GetValue().ToLong(&l);
   mOffset = l;
   mPercentText->GetValue().ToDouble(&mPercent);
   mRateText->GetValue().ToDouble(&mRate);

   if (mChannels < 1 || mChannels > 16)
      mChannels = 1;
   if (mOffset < 0)
      mOffset = 0;
   if (mPercent < 0.0)
      mPercent = 0.0;
   if (mPercent > 100.0)
      mPercent = 100.0;
   if (mRate < 100.0)
      mRate = 100.0;
   if (mRate > 100000.0)
      mRate = 100000.0;

   EndModal(true);
}

void ImportRawDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(false);
}

void ImportRawDialog::OnPlay(wxCommandEvent & WXUNUSED(event))
{
}

void ImportRawDialog::OnChoice(wxCommandEvent & WXUNUSED(event))
{
   SF_INFO info;

   memset(&info, 0, sizeof(SF_INFO));

   mEncoding = sf_encoding_index_to_subtype(mEncodingChoice->GetSelection());
   mEncoding += (mEndianChoice->GetSelection() * 0x10000000);

   info.format = mEncoding | SF_FORMAT_RAW;
   info.channels = mChannelChoice->GetSelection() + 1;
   info.samplerate = 44100;

   if (sf_format_check(&info)) {
      mOK->Enable(true);
      return;
   }

   // Try it with 1-channel
   info.channels = 1;
   if (sf_format_check(&info)) {
      mChannelChoice->SetSelection(0);
      mOK->Enable(true);
      return;
   }

   // Otherwise, this is an unsupported format
   mOK->Enable(false);
}

