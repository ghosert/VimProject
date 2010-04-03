/**********************************************************************

  Audacity: A Digital Audio Editor

  Benchmark.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/log.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/valgen.h>
#include <wx/valtext.h>
#include <wx/intl.h>

#include "Benchmark.h"
#include "Project.h"
#include "WaveTrack.h"
#include "Sequence.h"

class BenchmarkDialog: public wxDialog
{
public:
   // constructors and destructors
   BenchmarkDialog( wxWindow *parent );
   
   wxSizer *MakeBenchmarkDialog( wxWindow *parent, bool call_fit = TRUE,
                                 bool set_sizer = TRUE );
   
private:
   // WDR: handler declarations
   void OnRun( wxCommandEvent &event );
   void OnSave( wxCommandEvent &event );
   void OnClear( wxCommandEvent &event );
   void OnClose( wxCommandEvent &event );

   void Printf(const char *format, ...);
   void HoldPrint(bool hold);
   void FlushPrint();

   bool      mHoldPrint;
   wxString  mToPrint;

   wxString  mBlockSizeStr;
   wxString  mDataSizeStr;
   wxString  mNumEditsStr;
   wxString  mRandSeedStr;

   bool      mBlockDetail;
   bool      mEditDetail;

   wxTextCtrl  *mText;
   
private:
   DECLARE_EVENT_TABLE()
};

void RunBenchmark(wxWindow *parent)
{
   /*
   int action = wxMessageBox(_("This will close all project windows "
                               "(without saving)\n"
                               "and open the Audacity Benchmark dialog.\n\n"
                               "Are you sure you want to do this?"),
                             _("Benchmark"),
                             wxYES_NO | wxICON_EXCLAMATION,
                             NULL);

   if (action != wxYES)
      return;

   CloseAllProjects();
   */

   BenchmarkDialog dlog(parent);

   dlog.CentreOnParent();

   dlog.ShowModal();
}

//
// BenchmarkDialog
//

enum {
   RunID = 1000,
   BSaveID,
   ClearID,
   BCloseID,
   StaticTextID,
   BlockSizeID,
   DataSizeID,
   NumEditsID,
   RandSeedID
};

BEGIN_EVENT_TABLE(BenchmarkDialog,wxDialog)
   EVT_BUTTON( RunID,   BenchmarkDialog::OnRun )
   EVT_BUTTON( BSaveID,  BenchmarkDialog::OnSave )
   EVT_BUTTON( ClearID, BenchmarkDialog::OnClear )
   EVT_BUTTON( BCloseID, BenchmarkDialog::OnClose )
END_EVENT_TABLE()

BenchmarkDialog::BenchmarkDialog(wxWindow *parent):
      wxDialog( parent, 0, _("Benchmark"),
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE |
                wxDIALOG_MODAL |
                wxRESIZE_BORDER)
{
   mBlockSizeStr = "64";
   mNumEditsStr = "100";
   mDataSizeStr = "32";
   mRandSeedStr = "234657";

   mBlockDetail = false;
   mEditDetail = false;

   HoldPrint(false);

   MakeBenchmarkDialog( this );
}

// WDR: handler implementations for BenchmarkDialog

void BenchmarkDialog::OnClose(wxCommandEvent &event)
{
   EndModal(0);
}

wxSizer *BenchmarkDialog::MakeBenchmarkDialog( wxWindow *parent, bool call_fit, bool set_sizer )
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxFlexGridSizer *gridSizer = new wxFlexGridSizer(4, 0, 0);

   wxControl *item;
   
   // Strings don't need to be translated because this class doesn't
   // ever get used in a stable release.

   item = new wxStaticText(parent, StaticTextID, "Disk Block Size (KB):");
   gridSizer->Add(item, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   item = new wxTextCtrl(parent, BlockSizeID, "", wxDefaultPosition,
                         wxSize(100, -1), 0,
                         wxTextValidator(wxFILTER_NUMERIC,
                                         &mBlockSizeStr));
   gridSizer->Add(item, 0, wxALL, 5);

   item = new wxStaticText(parent, StaticTextID, "Number of Edits :");
   gridSizer->Add(item, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   item = new wxTextCtrl(parent, NumEditsID, "", wxDefaultPosition,
                         wxSize(100, -1), 0,
                         wxTextValidator(wxFILTER_NUMERIC,
                                         &mNumEditsStr));
   gridSizer->Add(item, 0, wxALL, 5);

   item = new wxStaticText(parent, StaticTextID, "Test Data Size (MB):");
   gridSizer->Add(item, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   item = new wxTextCtrl(parent, DataSizeID, "", wxDefaultPosition,
                         wxSize(100, -1), 0,
                         wxTextValidator(wxFILTER_NUMERIC,
                                         &mDataSizeStr));
   gridSizer->Add(item, 0, wxALL, 5);

   item = new wxStaticText(parent, StaticTextID, "Random Seed :");
   gridSizer->Add(item, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   item = new wxTextCtrl(parent, RandSeedID, "", wxDefaultPosition,
                         wxSize(100, -1), 0,
                         wxTextValidator(wxFILTER_NUMERIC,
                                         &mRandSeedStr));
   gridSizer->Add(item, 0, wxALL, 5);

   mainSizer->Add(gridSizer, 0, wxALL, 5);

   item = new wxCheckBox(parent, 0,
                         "Show detailed info about each block file",
                         wxDefaultPosition, wxDefaultSize, 0,
                         wxGenericValidator(&mBlockDetail));
   mainSizer->Add(item, 0, wxALL, 5);

   item = new wxCheckBox(parent, 0,
                         "Show detailed info about each editing operation",
                         wxDefaultPosition, wxDefaultSize, 0,
                         wxGenericValidator(&mEditDetail));
   mainSizer->Add(item, 0, wxALL, 5);

   mText = new wxTextCtrl(parent, StaticTextID, "",
                          wxDefaultPosition,
                          wxSize(-1, 200),
                          wxTE_MULTILINE |
                          wxTE_READONLY);
   mainSizer->Add(mText, 1, wxALL | wxEXPAND, 5);

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   item = new wxButton( parent, RunID, "Run");
   hSizer->Add(item, 0, wxALL, 5 );

   item = new wxButton( parent, BSaveID, "Save");
   hSizer->Add(item, 0, wxALL, 5 );

   item = new wxButton( parent, ClearID, "Clear");
   hSizer->Add(item, 0, wxALL, 5 );

   hSizer->Add(5, 5, 1, wxEXPAND);

   item = new wxButton( parent, BCloseID, _("Close"));
   hSizer->Add(item, 0, wxALL, 5 );

   mainSizer->Add(hSizer, 0, wxEXPAND, 5);   

   if (set_sizer) {
      parent->SetAutoLayout( TRUE );
      parent->SetSizer( mainSizer );
      if (call_fit) {
         mainSizer->Fit( parent );
         mainSizer->SetSizeHints( parent );
      }
   }
    
   return mainSizer;
}

void BenchmarkDialog::OnSave( wxCommandEvent &event )
{
   wxString fName = "benchmark.txt";

   fName = wxFileSelector("Export Benchmark Data As:",
                          NULL, fName, "txt", "*.txt", wxSAVE, this);

   if (fName == "")
      return;

   mText->SaveFile(fName);
}

void BenchmarkDialog::OnClear(wxCommandEvent &event)
{
   mText->Clear();
}

void BenchmarkDialog::Printf(const char *format, ...)
{
   va_list argptr;
   va_start(argptr, format);

   wxString s = wxString::FormatV(format, argptr);
   mToPrint += s;
   if (!mHoldPrint)
      FlushPrint();

   va_end(argptr);
}

void BenchmarkDialog::HoldPrint(bool hold)
{
   mHoldPrint = hold;

   if (!mHoldPrint)
      FlushPrint();
}

void BenchmarkDialog::FlushPrint()
{
   while(mToPrint.Length() > 100) {
      mText->AppendText(mToPrint.Left(100));
      mToPrint = mToPrint.Right(mToPrint.Length() - 100);
   }
   if (mToPrint.Length() > 0)
      mText->AppendText(mToPrint);
   mToPrint = "";
}

void BenchmarkDialog::OnRun( wxCommandEvent &event )
{
   TransferDataFromWindow();
 
   if (!Validate())
      return;

   // This code will become part of libaudacity,
   // and this class will be phased out.

   long blockSize, numEdits, dataSize, randSeed;

   mBlockSizeStr.ToLong(&blockSize);
   mNumEditsStr.ToLong(&numEdits);
   mDataSizeStr.ToLong(&dataSize);
   mRandSeedStr.ToLong(&randSeed);

   if (blockSize < 1 || blockSize > 1024) {
      wxMessageBox("Block size should be in the range 1 - 1024 KB.");
      return;
   }

   if (numEdits < 1 || numEdits > 10000) {
      wxMessageBox("Number of edits should be in the range 1 - 10000.");
      return;
   }

   if (dataSize < 1 || dataSize > 2000) {
      wxMessageBox("Test data size should be in the range 1 - 2000 MB.");
      return;
   }

   Sequence::SetMaxDiskBlockSize(blockSize * 1024);

   wxBusyCursor busy;

   HoldPrint(true);

   DirManager *d = new DirManager();
   TrackFactory *fact = new TrackFactory(d);
   WaveTrack *t = fact->NewWaveTrack(int16Sample);
   Track *tmp = NULL;

   t->SetRate(1.0);

   srand(randSeed);

   int len, scale;
   //scale = 7500 + (rand() % 1000);
   scale = 200 + (rand() % 100);
   len = (dataSize * 1048576) / (scale*sizeof(short));
   while(len < 20 || scale > (blockSize*1024)/4) {
      scale = (scale / 2) + (rand() % 100);
      len = (dataSize * 1048576) / (scale*sizeof(short));
   }

   Printf("Using %d blocks of %d samples each, for a total of "
          "%.1f MB.\n",
          len, scale, len*scale*sizeof(short)/1048576.0);

   int trials = numEdits;

   short *small = new short[len];
   short *small2 = new short[len];
   short *block = new short[scale];

   Printf("Preparing...\n");

   wxYield();
   FlushPrint();

   int i, b, v;
   int bad;
   int z;
   long elapsed;
   wxString tempStr;
   wxStopWatch timer;

   for (i = 0; i < len; i++) {
      v = short(rand());
      small[i] = v;
      for (b = 0; b < scale; b++)
         block[b] = v;

      t->Append((samplePtr)block, int16Sample, scale);
   }
   t->Flush();

   // This forces the WaveTrack to flush all of the appends (which is
   // only necessary if you want to access the Sequence class directly,
   // as we're about to do).
   t->GetEndTime();

   if (t->GetSequence()->GetNumSamples() != (sampleCount)len * scale) {
      Printf("Expected len %d, track len %d.\n", len * scale,
             t->GetSequence()->GetNumSamples());
      goto fail;
   }
   //t->Debug();

   Printf("Performing %d edits...\n", trials);
   wxYield();
   FlushPrint();

   timer.Start();
   for (z = 0; z < trials; z++) {
      int x0 = rand() % len;
      int xlen = 1 + (rand() % (len - x0));
      if (mEditDetail)
         Printf("Cut: %d - %d \n", x0 * scale, (x0 + xlen) * scale);

      t->Cut(double (x0 * scale), double ((x0 + xlen) * scale), &tmp);
      if (!tmp) {
         Printf("Trial %d\n", z);
         Printf("Cut (%d, %d) failed.\n", (x0 * scale),
                (x0 + xlen) * scale);
         Printf("Expected len %d, track len %d.\n", len * scale,
                t->GetSequence()->GetNumSamples());
         goto fail;
      }

      int y0 = rand() % (len - xlen);
      if (mEditDetail)
         Printf("Paste: %d\n", y0 * scale);

      t->Paste(double (y0 * scale), tmp);

      if (t->GetSequence()->GetNumSamples() != (sampleCount)len * scale) {
         Printf("Trial %d\n", z);
         Printf("Expected len %d, track len %d.\n", len * scale,
                t->GetSequence()->GetNumSamples());
         goto fail;
      }
      // Copy
      for (i = 0; i < xlen; i++)
         small2[i] = small[x0 + i];
      // Delete
      for (i = 0; i < (len - x0 - xlen); i++)
         small[x0 + i] = small[x0 + xlen + i];
      // Insert
      for (i = 0; i < (len - xlen - y0); i++)
         small[len - i - 1] = small[len - i - 1 - xlen];
      // Paste
      for (i = 0; i < xlen; i++)
         small[y0 + i] = small2[i];

      delete tmp;
   }

   elapsed = timer.Time();

   if (mBlockDetail) {
      t->GetSequence()->DebugPrintf(&tempStr);
      mToPrint += tempStr;
   }
   Printf("Time to perform %d edits: %ld ms\n", trials, elapsed);
   FlushPrint();
   wxYield();


#if 0
   Printf("Checking file pointer leaks:\n");
   Printf("Track # blocks: %d\n", t->GetBlockArray()->Count());
   Printf("Disk # blocks: \n");
   system("ls .audacity_temp/* | wc --lines");
#endif

   Printf("Doing correctness check...\n");
   FlushPrint();
   wxYield();

   bad = 0;
   timer.Start();
   for (i = 0; i < len; i++) {
      v = small[i];
      t->Get((samplePtr)block, int16Sample, i * scale, scale);
      for (b = 0; b < scale; b++)
         if (block[b] != v) {
            bad++;
            if (bad < 10)
               Printf("Bad: block %d sample %d\n", i, b);
            b = scale;
         }
   }
   if (bad == 0)
      Printf("Passed correctness check!\n");
   else
      Printf("Errors in %d/%d blocks\n", bad, len);

   elapsed = timer.Time();

   Printf("Time to check all data: %ld ms\n", elapsed);
   Printf("Reading data again...\n");

   wxYield();
   FlushPrint();

   timer.Start();

   for (i = 0; i < len; i++) {
      v = small[i];
      t->Get((samplePtr)block, int16Sample, i * scale, scale);
      for (b = 0; b < scale; b++)
         if (block[b] != v)
            bad++;
   }

   elapsed = timer.Time();

   Printf("Time to check all data (2): %ld ms\n", elapsed);
   
   Printf("At 44100 Hz, 16-bits per sample, the estimated number of\n"
          "simultaneous tracks that could be played at once: %.1f\n",
          (len*scale/44100.0)/(elapsed/1000.0));

   delete t;

   delete[]small;
   delete[]small2;
   delete[]block;

   delete fact;
   delete d;

   Sequence::SetMaxDiskBlockSize(1048576);
   HoldPrint(false);

   return;

 fail:
   Printf("TEST FAILED!!!\n");

   delete t;

   delete[]small;
   delete[]small2;
   delete[]block;

   delete d;

   Sequence::SetMaxDiskBlockSize(1048576);
   HoldPrint(false);
}



