/**********************************************************************

  Audacity: A Digital Audio Editor

  Help.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_HELP__
#define __AUDACITY_HELP__

void InitHelp(wxWindow * parent);
void ShowHelp(wxWindow * parent);
void ShowHelp(wxWindow * parent, wxString topic);
void SearchHelp(wxWindow * parent);
void ShowHelpIndex(wxWindow * parent);

void QuitHelp();

#endif
