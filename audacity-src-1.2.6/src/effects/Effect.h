/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT__
#define __AUDACITY_EFFECT__

#include <wx/dynarray.h>
#include <wx/intl.h> 
#include <wx/string.h>

class wxWindow;
class wxFrame;
class wxProgressDialog;

#include "../WaveTrack.h"

class Effect;

WX_DEFINE_ARRAY(Effect *, EffectArray);

#define PLUGIN_EFFECT   0x0001
#define BUILTIN_EFFECT  0x0002

#define INSERT_EFFECT   0x0010
#define PROCESS_EFFECT  0x0020
#define ANALYZE_EFFECT  0x0040
#define ALL_EFFECTS     0x00FF

// Flag used to disable prompting for configuration
// parameteres.
#define CONFIGURED_EFFECT 0x8000

class Effect {

 //
 // public static methods
 //
 // Used by the outside program to register the list of effects and retrieve
 // them by index number, usually when the user selects one from a menu.
 //
 public:
   static void RegisterEffect(Effect *f);
   static void UnregisterEffects();
   static Effect *GetEffect(int ID);
   static int GetNumEffects();

   // Returns a sorted array of effects, which may be filtered
   // using the flags parameter.  The caller should dispose
   // of the array when done.
   static EffectArray *GetEffects(int flags = ALL_EFFECTS);

 // 
 // public methods
 //
 // Used by the outside program to determine properties of an effect and
 // apply the effect to one or more tracks.
 // 
 public:
   // Each subclass of Effect should override this method.
   // This name will go in the menu bar;
   // append "..." if your effect pops up a dialog
   virtual wxString GetEffectName() = 0;
   
   // Each subclass of Effect should override this method.
   // This name will go in the progress dialog, but can be used
   // elsewhere, and it should describe what is being done.
   // For example, if the effect is "Filter", the action is
   // "Filtering", or if the effect is "Bass Boost", the action
   // is "Boosting Bass Frequencies".
   virtual wxString GetEffectAction() = 0;

   // Each subclass of Effect should override this method. 
   // This description will go in the History state. 
   // Override to include effect parameters, so typically useful only after PromptUser. 
   virtual wxString GetEffectDescription() { 
      // Default provides effect name. 
      return wxString::Format(_("Applied effect: %s"), 
                              (const char *)(this->GetEffectName())); 
   } 
	 
   // Return flags which tell you what kind of effect this is.
   // It will be either a built-in or a plug-in effect, and it
   // will be one of Insert, Process, or Analyze.
   virtual int GetEffectFlags() {
      // Default - covers most built-in effects.
      return BUILTIN_EFFECT | PROCESS_EFFECT;
   }

   // The Effect class fully implements the Preview method for you.
   // Only override it if you need to do preprocessing or cleanup.
   virtual void Preview();

   // Get an unique ID assigned to each registered effect.
   // The first effect will have ID zero.
   int GetID() {
      return mID;
   }

   // Returns true on success.  Will only operate on tracks that
   // have the "selected" flag set to true, which is consistent with
   // Audacity's standard UI.
   bool DoEffect(wxWindow *parent, int flags, TrackList *list,
                 TrackFactory *factory, double *t0, double *t1);

   wxString GetPreviewName();

 private:
    static int LastType;
    static int LastIndex;
    static Effect * pLastEffect;
 public:
    static void SetLastEffect(int type, int index, Effect * pEffect){
       LastType=type;
       LastIndex=index;
       pLastEffect = pEffect;
    }
    static int GetLastEffectType(){ return LastType;}
    static int GetLastEffectIndex(){ return LastIndex;}
    static Effect * GetLastEffect(){ return pLastEffect;}

 //
 // protected virtual methods
 //
 // Each subclass of Effect overrides one or more of these methods to
 // do its processing.
 //
 protected:
   // The constructor.  Called once at the beginning of the program.
   // Avoid allocating memory or doing time-consuming processing here.
   Effect();

   //The destructor.
   virtual ~Effect() {}
 
   // Called once each time an effect is called.  Perform any initialization;
   // make sure that the effect can be performed on the selected tracks and
   // return false otherwise
   virtual bool Init() {
      return true;
   }

   // If necessary, open a dialog to get parameters from the user.
   // This method will not always be called (for example if a user
   // repeats an effect) but if it is called, it will be called
   // after Init.
   virtual bool PromptUser() {
      return true;
   }
      
   // Actually do the effect here.
   virtual bool Process() = 0;

   // clean up any temporary memory
   virtual void End() {
   }

 //
 // protected data
 //
 // The Effect base class will set these variables, some or all of which
 // may be needed by any particular subclass of Effect.
 //
 protected:
   wxWindow     *mParent;
   TrackFactory *mFactory;
   TrackList    *mTracks;      // the complete list of all tracks
   TrackList    *mWaveTracks;  // effects which do not add or remove tracks
                               // should use this
   double      mT0;
   double      mT1;

 //
 // protected methods
 //
 // These methods can be used by subclasses of Effect in order to display a
 // progress dialog or perform common calculations
 //
 protected:
   // The Progress methods all return true if the user has cancelled;
   // you should exit immediately if this happens (cleaning up memory
   // is okay, but don't try to undo).
 
   // Pass a fraction between 0.0 and 1.0
   bool TotalProgress(double frac);
   
   // Pass a fraction between 0.0 and 1.0, for the current track
   // (when doing one track at a time)
   bool TrackProgress(int whichTrack, double frac);
 
   // Pass a fraction between 0.0 and 1.0, for the current track group
   // (when doing stereo groups at a time)
   bool TrackGroupProgress(int whichGroup, double frac);

   int GetNumWaveTracks() { return mNumTracks; }

 //
 // protected static data
 //
 // Preferences shared by all effects
 //
 protected:
   static double sDefaultGenerateLen;
 
 //
 // private methods
 //
 // Used only by the base Effect class
 //
 private:
   void CountWaveTracks();
 
 //
 // private data
 //
 // Used only by the base Effect class
 //
 private:
   static EffectArray mEffects;
   
   int mNumTracks;
   int mNumGroups;

   int mID;
   static int sNumEffects;
   
   wxProgressDialog *mProgress;

};


// Utility functions

float TrapFloat(float x, float min, float max);
double TrapDouble(double x, double min, double max);
long TrapLong(long x, long min, long max);

#endif
