Audacity: A Free, Cross-Platform Digital Audio Editor

Version 1.2.6

For changelog, see the bottom of this document.

WWW:   http://audacity.sourceforge.net/

Email: audacity-help@lists.sourceforge.net

Lead Developers:
   Dominic Mazzoni (project leader)
   Matt Brubeck
   James Crook
   Vaughan Johnson
   Markus Meyer

Developers:
   Joshua Haberman
   Monty Montgomery
   Shane Mueller

Contributors:
   William Bland (Time Tracks)
   Roger Dannenberg
   Brian Gunlogson
   Greg Mekkes
   Abe Milde
   Paul Nasca
   Tony Oetzmann
   Augustus Saunders  
   Tom Woodhams

The Audacity Logo:
   Harvey Lubin
   http://www.agrapha.com/

Audacity is based on code from the following projects::
   expat
   FLAC
   LAME
   libmad
   libsndfile
   Nyquist
   Ogg Vorbis
   PortAudio
   Resample
   SoundTouch
   wxWidgets

Special Thanks:
   Richard Ash
   Dave Beydler
   Jason Cohen
   Dave Fancella
   Steve Harris
   Daniel James
   Daniil Kolpakov
   Robert Leidle
   Logan Lewis
   David Luff
   Jason Pepas
   Mark Phillips
   Jonathan Ryshpan
   Patrick Shirkey
   David R. Sky
   Tuomas Suutari
   Mark Tomlinson
   David Topper
   Rudy Trubitt
   StreetIQ.com
   Verilogix, Inc.

-------------------------------------------------------------

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program (in a file called LICENSE.txt); if not, go
to http://www.gnu.org/copyleft/gpl.html or write to 

  Free Software Foundation, Inc.
  59 Temple Place - Suite 330
  Boston, MA 02111-1307 USA

-------------------------------------------------------------

Source code to this program is always available; for more
information visit our website at:

  http://audacity.sourceforge.net/

Audacity is built upon other free libraries; some of
these libraries may have come with Audacity in the lib-src
directory.  Others you are expected to install first if
you want Audacity to have certain capabilities.  Most
of these libraries are not distributed under the terms
of the GPL, but rather some other free, GPL-compatible
license.  Specifically:

  wxWidgets: LGPL
    Cross-platform GUI library - must be downloaded and
    compiled separately.

  expat: BSD-like license.
    Provides XML parsing.  Included with Audacity

  iAVC: LGPL
    Part of the code to the AVC Compressor effect.
    Included with Audacity.

  libid3tag: GPL
    Reads/writes ID3 tags in MP3 files.  Optional
    separate download as part of libmad.

  libmad: GPL
    Decodes MP3 files.  Optional separate download.

  libnyquist: BSD-like license.
    Functional language for manipulating audio; available
    within Audacity for effects processing.

  libogg: BSD-like license.
    Optional separate download, along with libvorbis.

  libflac: BSD-like license.
    Optional separate download for libsndfile

  libresample: LGPL
    High-quality audio resampling.
    
  libsndfile: LGPL
    Reads and writes uncompressed PCM audio files.
    Included with Audacity.

  libvorbis: BSD-like license.
    Decodes and encodes Ogg Vorbis files.  Optional
    separate download.

  SoundTouch: LGPL
    Changes tempo without changing pitch and vice versa.
    Included in audacity

For more information, see the documentation inside
each library's source code directory.

-------------------------------------------------------------------------
Additional copyright information:
-------------------------------------------------------------------------

Nyquist

Copyright (c) 2000-2002, by Roger B. Dannenberg
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

Redistributions of source code must retain the copyright notice, the
list of conditions, and the disclaimer, all three of which appear below under
"COPYRIGHT AND LICENSE INFORMATION FOR XLISP."

Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

Redistributions in binary form must reproduce the copyright notice, the
list of conditions, and the disclaimer, all three of which appear below under
"COPYRIGHT AND LICENSE INFORMATION FOR XLISP," in the documentation and/or
other materials provided with the distribution.

Neither the name of Roger B. Dannenberg, Carnegie Mellon University, nor the 
names of any contributors may be used to endorse or promote products derived 
from this software without specific prior written permission.

COPYRIGHT AND LICENSE INFORMATION FOR XLISP (part of Nyquist):

Copyright (c) 1984-2002, by David Michael Betz
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer. 

Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

Neither the name of David Michael Betz nor the names of any contributors may be
used to endorse or promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-------------------------------------------------------------

Compilation instructions:

First you must download wxWidgets 2.4.x from:

  http://www.wxWidgets.org/

If you install the RPM, make sure you install the devel RPM
as well, otherwise, you won't be able to compile Audacity
from source.

To compile on Linux, Mac OS X, and other Unix systems,
simply execute these commands:

  ./configure
  make
  make install  # as root

To see compile-time options you can set, you can type
"./configure --help".
  
If you want to do any development, you might want to generate
a configure cache and header dependencies:

  ./configure -C
  make dep

To compile on Windows using MSVC++, please follow the
instructions found in compile.txt in the "win" subdirectory.

For more information on compilation please email
audacity-help@lists.sourceforge.net or see the audacity forum
http://www.audacityteam.org/forum/

-------------------------------------------------------------

Known issues/problems:

  * Audacity can import and display MIDI files, but they cannot be played
    or edited.

  * On Pentium-II based systems effects using the Soundtouch libraries may
    cause Audacity to crash. This is a problem with the upstream library.

  * Windows only: If you installed one of the 1.1.x beta versions,
    you must uninstall it before installing Audacity 1.2.
    (You do not need to uninstall earlier 1.2 or 1.0 releases.)

  * Linux only: Recording in full duplex on some Linux systems causes
    mono recordings to sound slowed-down or low-pitched.  To work around
    this problem (a bug in the ALSA OSS emulation code), set Audacity to
    record in stereo.

  * Mac OS X only: Audacity cannot work with files or folders that
    are contained inside folders with international characters
    (accent marks, etc.) in their names.  Note that files with
    accented characters work, and Audacity projects with
    accented characters work.  Only files inside of folders
    with accented characters in their names will cause problems. This is fixed
    in unicode-aware builds on 1.3.2 and higher.

  * MacOS X only: Some users find that after running Audacity other media
    players don't produce any sound or crash. Audacity tries to select the best
    quality settings your system is capable of, to give the best recordings
    possible. Some sound drivers also retain these settings as defaults for
    other applications, which can cause these symptoms
 
    To get round this, enable the option "Do not modify audio device settings"
    on the Audio I/O tab of the preferences, and make sure that your sound
    device is set up (in the Apple Sound and Midi Setup utility) to work in
    stereo, 16bits, with a sample rate of 44100Hz or 48000Hz.  See also 
      http://docs.info.apple.com/article.html?artnum=300832


-------------------------------------------------------------

Changes in 1.2.6:

  * Fix memory leaks on Windows.

  * Correct font size problems.

  * Enable missing FLAC support.

Changes in 1.2.5:

  * An official Intel Mac version is now available.

  * Fixed bug in Generate Silence which caused it to apply to all tracks
    instead of just the selected ones.

  * Mac OS X: audio device opening code has been rewritten.  First, it
    is much more conservative about changing device settings; it will
    not change settings when you open the program or close the
    preferences dialog anymore, and it will not change the settings when
    you begin playback/recording if the current settings are adequate.
    When it does change the settings, it should work much better on
    devices such as the Griffin iMic, ART USB Phono Plus,
    and Ion iMX02 USB.

  * Mac OS X: added new Audio I/O preference that lets you tell Audacity
    to never change any audio device settings.

  * Newer libsndfile supports FLAC import and export

  * Updated soundtouch to current version which is faster and better quality

  * Modified configure script prefers system libraries to local copies to
    reduce compilation times and memory usage.

  * Minor updates to help files.

  * New or updated translations: Bulgarian (bg), Galician (gl),
    Traditional Chinese (zh_TW), Simplified Chinese (zh), Slovenian (sl),
    Swedish (sv), Bangladeshi (bn), Slovakian (sk), Romanian (ro),
    Lithuanian (lt), Welsh (cy), and Turkish (tr).

Changes in 1.2.4:

  * The File menu now includes a list of recent files.

  * The "Generate Silence" effect now prompts for a length.

  * Audacity is now built with Vorbis 1.1, which features better encoding
    quality and file compression.

  * Dragging sound files into the Audacity window now works on Mac OS X
    and Linux, as well as Windows.  (Before, it worked only on Windows.)

  * Better support for certain audio devices on Mac OS X 10.4 "Tiger"

  * The "View History" window can now discard old undo levels to save disk
    space on Windows.  (This previously worked only on Linux and Mac.)

  * "Preferences" command is now in Edit menu.

  * "Plot Spectrum" command is now in Analyze menu.

  * Opening a project file saved by a later version of Audacity displays
    an intelligent error message.  Also, trying to import a project file
    (instead of open it) displays an intelligent error message.

  * Audacity now compiles in Visual C++ .NET 2003.

  * Other minor bug fixes.

  * New or updated translations: Arabic (ar), Czech (cs), Finnish (fi),
    Hungarian (hu), Japanese (ja), Norwegian (nb), Slovenian (sl), 
    Simplified Chinese (zh_CN), Traditional Chinese (zh_TW).

Changes in 1.2.3:

  * Fixed a bug that caused recording to stop or display incorrectly
    after about 50 minutes on some Windows systems.  (This was partly
    fixed in Audacity 1.2.2, but still didn't work on some systems.)

  * The Change Pitch and Change Tempo effects have been upgraded to
    use a new version of the SoundTouch library by Olli Parviainen,
    with better speed and higher quality.

  * libsndfile has been upgraded to version 1.0.11.

  * Fixed a bug that caused the program to run slowly when using the
    Envelope tool.

  * Shift-clicking on a mute or solo button now un-mutes (or un-solos)
    all other tracks.

  * Nyquist plug-ins can now accept strings as input.  Also, a "Debug"
    button has been added to Nyquist effect dialogs, which allows you
    to see all of the output produced by Nyquist, for aid in debugging.

  * When the audio file referenced ("aliased") by an Audacity project is
    missing, Audacity will now always play silence.  Before, Audacity
    would sometimes repeat the most recent audio that was played previously.

  * VU Meters will now always reset when audio I/O has stopped.

  * Fixed a major Mac-only bug that was causing Audacity to crash at seemingly
    random times, but especially during audio playback and recording.

  * New or updated translations: Italian (it), Hungarian (hu),
    Ukrainian (uk), Spanish (es). Polish (pl), Simplified Chinese (zh),
    Norsk-Bokmal (nb), French (fr).

Changes in 1.2.2:

  * VU Meters added for both playback and recording.  Click on
    the recording meter to monitor the input without recording.

  * Export Multiple - new feature that lets you export multiple
    files at once, either by track, or split based on labels.

  * Attempt to automatically correct latency in full-duplex recordings.
    (This does not work perfectly, and is not yet supported on all
    systems.  It will improve in future versions.)

  * Fixed a serious bug that could cause data loss when you save and
    then reload and re-edit an Audacity project containing repeated
    or duplicate data.

  * MP3 tags dialog will only pop up the first time you export as
    MP3; after that it will not pop up again as long as you have
    filled in at least one tag.

  * You can now add a label at the current playback position - in
    the Project menu, with a shortcut of Ctrl+M.

  * Clicking on a label now selects all of the tracks, making it
    easier to use the label track to recall selections.

  * Windows: Fixed a crash in the Time Track "Set Rate" command.

  * Fixed a bug that caused problems with recordings over 45 minutes
    on some Windows systems.

  * Mac OS X: Improved support for the Griffin iMic by fixing a bug
    that was causing it to always record in mono instead of stereo.

  * Added support for Software Playthrough (listen to what you're
    recording while recording it, or while monitoring using a VU
    meter) - this makes it possible, for example, to record using one
    audio device while listening to it play through a separate device.

  * Unix/Linux: Fixed freeze caused by captured mouse when audio
    device hangs.  (Audacity may not respond, but it will no longer
    freeze all of X.)

  * Fixed a cosmetic bug that caused improper waveform display if
    you tried to open an Audacity project saved on a different
    platform (e.g., copying a project from a Mac to a PC).

  * Fixed bug that could cause instability when pasting, splitting,
    or duplicating a label track.

  * You can now change the font of a label track by choosing "Font..."
    from the label track's pop-up menu.

  * Basic printing support has been added.  Currently it scales the
    entire project to fit exactly on one page.  Try printing in
    landscape orientation for best results.

  * Mac OS X and Windows: Audacity ships with a newer version (1.0.1)
    of the Ogg Vorbis encoder.  Vorbis compression will now have higher
    quality and smaller file sizes.

  * Fix a bug that occasionally caused crashes when applying effects
    to split tracks.

  * Zoom In / Zoom Out now properly disable when they're not available.

  * Fixed disk memory leak in Preview

  * Other minor bug fixes and performance improvements.

Changes in 1.2.1:

  * The following translations have been added or updated:  Finnish,
    French, Hungarian, Italian, Japanese, Norwegian, Polish, Russian.

  * Fix a bug that could cause data to be lost when pasting audio
    from one project into another, after the first project has been
    saved and closed.

  * Fix a possible crash when opening or resizing the Equalization
    window, especially when using large system fonts.

  * Don't allow percentages less than -100% in Change Pitch/Speed/Tempo
    effects (fixes a possible crash).

  * Fix a crash when the temporary directory is not available on startup.

  * Correctly load ID3 tags saved in Audacity project files.

  * On Linux and OS X, store lockfiles in the temp directory instead of
    the user's home directory.  This fixes problems in lab environments
    where users have restricted or network-mounted home directories.

  * Fix a bug that prevented Nyquist effects from running when certain
    regional settings were activated.

  * Fix a bug in the Quick Mix command that could cause old temporary
    files to not be deleted.

  * Linux: Fix endianness problems in playback on PowerPC.

  * Linux: Fix compilation problem in Nyquist on MIPS.

  * Linux: Include a more recent PortAudio v19 snapshot (fixes compilation
    problems when building with the --with-portaudio=v19 option).

  * Two new Nyquist plug-ins: "Cross Fade In" and "Cross Fade Out."
  
  * Other minor bug-fixes.

Changes in 1.2.0:

  * New cross-fade effects.

  * Fix problem where samples were drawn in the wrong position
    when zoomed all the way in.  This caused the drawing tool
    to move a different sample than the one under the cursor.

  * Don't use id3v2.4 tags, which are not yet supported by
    most players.  (This was fixed in 1.2.0-pre2, but appeared
    again by accident in 1.2.0-pre3.)

  * Correctly display translated messages in the status bar.

  * When the cursor is on-screen, the Zoom In button now zooms
    to the area around the cursor.

  * Mac OS X: Fixed audio problems on the Apple PowerMac G5.

  * Linux/ALSA: Work around a bug in ALSA's OSS emulation that
    caused Audacity's playback cursor to move too quickly.

  * Microsoft Windows: The Audacity source code should now
    compile out of the box on Windows.

  * Many new/updated translations.

Changes in 1.2.0-pre4:

  * Fixed problems that could occur when importing certain
    non-seekable PCM audio files, such as GSM610.

  * Fixed bug that was causing the samples to shift off-screen
    horizonally when zoomed in very far and the track had a
    time-shift offset.

  * Fixed bugs in the new resampler that added noise to resampled
    audio on some systems. If you experienced noise when exporting
    to a WAV, MP3 or OGG file you may have been bitten by this bug.
  
  * Fixed bug that led to occasional crashes when using the
    time-shift tool in conjunction with high zoom factors.
    
  * Dithering is now only applied on export when it is really
    necessary (e.g. when converting float samples to 16-bit).
    
  * Files that only contain mono tracks are now automatically
    exported to stereo files when they contain tracks which are
    panned to the left or the right.
    
  * The Delete key can now be used to delete the current selection,
    in addition to the Backspace key.

  * Fixed bug where Audacity didn't ask whether to save 
    changes if you close the project or exit while recording.

  * Mac OS X: Supports Playthrough (listen to what you're recording
    while recording it) if your hardware device supports it.

  * Mac OS X: Audacity is now a package (you can right-click on
    Audacity.app and select 'Show Package Contents').  Launch time
    has improved significantly.

  * MS Windows: Fixed problem that caused Windows XP to use 
    the short name of a file ("TESTFI~1.AUP"), which led to 
    problems when the file was later opened again using the 
    long file name.
    
  * MS Windows: Fixed bug that caused file exports to fail 
    if the destination directory was the root folder of a 
    Windows drive.

  * MS Windows: Audacity's application information which 
    is written to the Windows registry now always contains 
    the full path to the executable. 

  * MS Windows: Fixed problems in trying to set the Windows 
    registry as non-admin user, for file-type associations.

  * Make sure the "Save" command is enabled after changing
    gain and pan sliders.

  * Updated translations.  Added translator credits to the
    "About" window in localized versions.

Changes in 1.2.0-pre3:

  * Fixed bug where Export is grayed out when nothing is
    selected.

  * Fixed crash caused by opening Audacity on a computer with
    a high-end sound card with no mixer support.

  * Fixed crash in Import Raw.

  * Fixed New Stereo Track.

  * Cosmetic fixes for Mac OS X.

  * Support for the VST Enabler on Windows added.

  * Fixed crash if you close Audacity while the Preferences
    dialog is open.

  * Fixed duplicate-character bug in Mac OS X Label Tracks.

  * The recording level control on Linux now adjusts the IGAIN,
    rather than the playthrough level of the recording source.

  * Fixed bug that caused corruption to 16-bit stereo recordings.

  * Fixed bug that caused data loss if you deleted all tracks in
    a saved project and then open a new file into the same window.

  * Added support for alternate audio button order (in Interface
    preferences)

  * Added preliminary support for wxX11

  * Added fully transparent Windows XP icon

  * Fixed crash if you try to record (or play) and no audio
    devices exist, or if the audio device doesn't support the
    mode you selected.

  * Audacity no longer sets the process priority to high while
    recording on Windows.  Users can still do this manually
    using the Task Manager.

  * Fixed bug that caused last ~100 ms of the selection to get
    cut off on Windows.

  * Fixed FFT Filter and Equalization effects dialogs.

  * Fixed bugs in Unix build system (DESTDIR in locale directory,
    choosing libsamplerate instead of libresample)

  * Support for LADSPA plug-ins on Windows added, and 
    three open source LADSPA plug-ins ported to Windows
    (GVerb reverb, SC4 compressor, and Hard Limiter)

Changes in 1.2.0-pre2:

  * Online help completed.  The full manual is nearly complete
    and will be posted to the website for online browsing shortly.

  * Audacity will no longer let you do unsafe editing operations
    while playing or recording.  This eliminates many potential
    crashes.

  * Fixed ability to cancel Quit button.

  * New resampling library, with no restrictions on the maximum or
    minimum rate of resampling.

  * Audacity now supports LADSPA plug-ins on all platforms, and
    supports VST plug-ins through an optional LADSPA plug-in
    called the "VST Enabler", which you can download separately.
    Because of licensing issues, Audacity cannot be distributed
    with VST support built-in.

  * Mac OS X keyboard shortcut problems have been fixed.

  * Mac OS X audio muting problems have been fixed.

  * Mac OS X playback/recording cursor sync problems have been fixed.

  * Silence now displays a straight line again, instead of nothing.

  * Added a vertical ruler to the Waveform dB display.

  * Fixed crash in Change Pitch.

  * You can now Paste if nothing is selected.

  * Canceling an Import operation doesn't cause an extra error
    dialog to appear.

  * Audacity now handles filenames with international characters
    correctly.

  * Now outputs ID3v2.3 tags (instead of ID3v2.4), to be
    compatible with more MP3 players.

  * Minor improvements to build system on Unix systems.

New features in Audacity 1.2:
  * User Interface
    - Vertical zooming of tracks.
    - Improved look and placement of toolbars.
    - New custom mouse cursors.
    - Complete implementation of editable keyboard shortcuts.
    - Find zero-crossings.
    - Mouse wheel can be used to zoom in and out.
    - Multi-Tool mode.
    - Amplify using envelope.
    - Labels can store selections (like Audacity 1.0.0).

  * Effects
    - Repeat Last Effect command
    - Improved VST plug-in support
    - Most effects now have a Preview button
    - Compressor (Dynamic Range Compressor)
    - Change Pitch (without changing tempo)
    - Change Tempo (without changing pitch)
    - Change Speed (changing both pitch and tempo)
    - Repeat (useful for creating loops)
    - Normalize (adjust volume and DC bias)

  * Audio I/O
    - 1-second preview command.
    - Looped play.

  * File I/O
    - Audacity 1.2.0 opens project files from all previous versions
      of Audacity from 0.98 through 1.1.3.
    - Open multiple files from the same dialog.
    - Use a text file to specify a list of audio files to open with offsets.

  * Updated user manual

  * Bug fixes
    - Project files with special characters are no longer invalid.
    - "Scratchy" noises caused by bad clipping are fixed.
    - Audacity no longer exports invalid Ogg files, and does not cut off the
      last few seconds of exported Ogg files.
    - Mono MP3 files now export at the correct speed.
    - Many incorrect results from the Envelope tool have been fixed.
    - The "Export Labels" command now overwrites existing files correctly.
    - The "Plot Spectrum" window displays the correct octave numbers for
      notes.
    - Several memory leaks are fixed.

New features in Audacity 1.1.3:
  * User Interface
    - New Mixer toolbar allows you to control the output
      volume, input volume, and input source directly
      from Audacity.
    - Every track now has its own gain and pan controls.

  * File I/O
    - Uses improved project file format.  (Unfortunately reading
      previous formats, including 1.1.1, is not supported.)
    - Block files (stored in Audacity project directories) now
      use the standard AU format.  Though some Audacity
      meta-information is in these files, they can now be
      read by many other popular audio programs as well.
    - Fixed some bugs relating to reading/writing audio
      files with more than 16 bits per sample.
    - Import RAW is functional again, with a simpler GUI
      but support for far more file formats.  The
      autodetection algorithms are much more accurate than
      in 1.0.

  * Audio I/O
    - Completely rewritten audio I/O, with lower latency
      and minimal chance of buffer underruns while
      recording.

  * Resampling
    - Using high quality resampling algorithms, with the
      option of better quality for mixing than for real-time
      playback

    - Preliminary support for Time Tracks, for changing
      playback speed over time.

  * Many more bug fixes and new features

New features in Audacity 1.1.2:
  * User Interface
    - Fixed bug in Windows version, for track menu commands 
	  "Name..." and "Split Stereo Track"/"Make Stereo Track".
  * Effects
    - Nyquist support on Windows (supports plug-ins written 
	  in Nyquist, an interpreted functional language based 
	  on Lisp).

New features in Audacity 1.1.1:

  * User Interface
    - Tooltips appear in Statusbar.
    - Vertical cursor follows play/record
    - Pause button
    - Drawing tool (with three different modes)
    - Vertical Resizing of stereo tracks is more fun.
    - Adjust selection by click-dragging selection boundary
    - Toolbar button context-sensitive enabling/disabling
    - Better zooming functionality (centers region)
    - Multiple ways to display the cursor position and selection
    - Snap-to selection mode
    - Drag tracks up and down
    - Align and group align functions
    - Cursor save/restore
    - Working history window
  * Effects
    - Effects broken down into three menus: Generate, Effect, and
      Analyze
    - Generate menu lets you generate silence, noise, or a tone
    - Nyquist support (supports plug-ins written in Nyquist,
      an interpreted functional language based on Lisp)
  * Localization
    - Improved localization support
    - More languages available
    - Language selection dialog on startup
  * Mac OS X
    - Support for more audio hardware
    - Support for full-duplex (play while recording)
    - Support for MP3 exporting using LameLib Carbon
  * Unix
    - Audacity now has a man page (it describes command-line
      options and how to set the search path)
  * File Formats
    - Uses libsndfile 1.0, which fixes some bugs and
      improves performance
  * Searching for Files:
    - On Windows and Mac OS, Audacity now looks for
      translations in the "Languages" folder and all plug-ins
      in the "Plug-ins" folder, relative to the program.
    - On Unix, Audacity looks for translations in
      <prefix>/share/locale and looks for everything else
      in <prefix>/share/audacity and also in any paths in
      the AUDACITY_PATH environment variable

New features in Audacity 1.1.0:

  * Core audio processing:
    - Support for 24-bit and 32-bit sample formats
    - Automatic real-time resampling (using linear
        interpolation)
  * Effects:
    - Support LADSPA plugins on Linux / Unix
  * File formats:
    - New XML-based Audacity project format
    - Full Ogg Vorbis support now (importing and exporting)
    - Export to any command-line programs on Unix
    - Support for reading and writing many more types of
        uncompressed audio files, including ADPCM WAV files.
  * Toolbars
    - New toolbar drawing code; automatically adopts your
        operating system's colors
    - New toolbar buttons (Skip to Start, Skip to End)
    - New Edit toolbar
    - Toolbar buttons disable when they're not available
  * User Interface
    - Fully customizable keyboard commands
    - Autoscroll while playing or recording
    - New Ruler, used in main view and in
        FFT Filter effect
    - The waveform now displays the average value in a lighter
        color inside the peak values
  * Localization
    - Audacity can now be localized to different foreign
      languages.

New libraries in Audacity 1.1:

  * libmad for fast MP3 importing
  * libid3tag for editing MP3 file information
  * libsndfile to read and write more audio file formats
  * PortAudio for cross-platform audio playing and recording
