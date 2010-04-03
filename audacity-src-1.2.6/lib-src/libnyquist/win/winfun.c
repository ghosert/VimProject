/* winfun.c - windows user interface functions for xlisp */
/* Written by Brian Kendig. */
/*Windows Console and DirectSound code added by Morgan Green and Chris Tchou*/

//#include <Quickdraw.h>
//#include <Windows.h>
//#include <Memory.h>
#include <windows.h>
#include <switches.h>
#include "xlisp.h"
#include "sound.h"



/* osbgetc - get a character from a binary file ======  added for console*/
/* int osbgetc(fp) FILE *fp; {return (getc(fp));} */
int osbgetc(FILE *fp) 
{ 
  int c; c = (getc(fp));
/*  if (dbgflg) printf("osbgetc: got %d from FILE %x\n", c, fp); */
  return c;
}



LVAL xsystem (V) { return NIL; }
LVAL xgetkey (V) { return NIL; }


void ossymbols() 
{
    HWND mywin;
    HANDLE myhandle;
    COORD winsize, origin;
    WORD textattrib;
    DWORD n;
    mywin = GetForegroundWindow();
    SetConsoleTitle("Nyquist");

#ifdef WIN32_SNAZZY_CONSOLE     // -eub
    myhandle = GetStdHandle(STD_OUTPUT_HANDLE);
    origin.X = 0;
    origin.Y = 0;
    winsize.X = 100;
    winsize.Y = 40;
    textattrib = BACKGROUND_RED | BACKGROUND_BLUE | BACKGROUND_GREEN | BACKGROUND_INTENSITY;

    FillConsoleOutputAttribute(myhandle, textattrib, winsize.X * winsize.Y, origin, &n);
    SetConsoleScreenBufferSize(myhandle, winsize);
    FillConsoleOutputAttribute(myhandle, textattrib, winsize.X * winsize.Y, origin, &n);
    SetConsoleTextAttribute(myhandle, textattrib); 
#endif

    setvbuf(stdout, NULL, _IONBF, 0);   // makes it work under NT emacs 20.3   -eub

}


LVAL xsetupconsole()
{
    HWND mywin;
    HANDLE myhandle;
    COORD winsize, origin;
    WORD textattrib;
    DWORD n;
    mywin = GetForegroundWindow();
    SetConsoleTitle("Nyquist");

    myhandle = GetStdHandle(STD_OUTPUT_HANDLE);
    origin.X = 0;
    origin.Y = 0;
    winsize.X = 100;
    winsize.Y = 40;
    textattrib = BACKGROUND_RED | BACKGROUND_BLUE | BACKGROUND_GREEN | BACKGROUND_INTENSITY;

    FillConsoleOutputAttribute(myhandle, textattrib, winsize.X * winsize.Y, origin, &n);
    SetConsoleScreenBufferSize(myhandle, winsize);
    FillConsoleOutputAttribute(myhandle, textattrib, winsize.X * winsize.Y, origin, &n);
    SetConsoleTextAttribute(myhandle, textattrib);
    return NIL;
}


void get_xlisp_path(char *p, long p_max)
{
    HKEY hkey;
    DWORD dwType;

    if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE", 0, KEY_READ, &hkey) !=
        ERROR_SUCCESS) {
        *p = 0;
        return;
    }
    if (RegOpenKeyEx(hkey, "CMU", 0, KEY_READ, &hkey) !=
        ERROR_SUCCESS) {
        *p = 0;
        return;
    }
    if (RegOpenKeyEx(hkey, "Nyquist", 0, KEY_READ, &hkey) !=
        ERROR_SUCCESS) {
        *p = 0;
        return;
    }
    if (RegQueryValueEx(hkey, "XLISPPATH", NULL, &dwType, p, &p_max) !=
           ERROR_SUCCESS) {
           *p = 0;	
        return;
    }
}

#ifdef WINGUI
/* NOTE: define WINGUI in the Project Settings for the NyqWin projects.
 * Do not define WINGUI for console versions of Nyquist
 */

/****************************************************************************
*               fileopen
* Inputs:
*    char *deflt: the default file name (e.g. from command line)
*    char *extension: default extension
*    char *mode: read ("r") or write ("w")
*    char *prompt: prompt for user
* Returns:
*    opened file pointer
* Effect: 
*    opens file, prompts for user input if necessary and warns about
*    possible confusion.  If deflt is a null string or NULL, the user will
*    be prompted for a name.     The routine loops until a file is opened.
*    If the mode is "r", a check is made to see if the file exists
*    with and without the extension.     If both exist a warning is given.
*    For mode "w", a check is made to see if the file will be overwritten.
*    The extension is automatically added if the default or user-typed
*    file has no "."     At the bottom of the loop body, if no file has
*    been opened, the user is prompted for another file name.
****************************************************************************/

char fileopen_name[100];        /* name of the opened file */

HINSTANCE hInst;            /* current instance */
HWND hMainWindow;           /* main window handle */


char *getfilename(char *deflt, char *extension, char *mode, char *prompt)
{
    char filter[64];
    char *filter_ptr = NULL;
    OPENFILENAME open_file_name;

    if (extension && extension[0] == 0) extension = NULL;
    if (strcmp(extension, "lsp") == 0) {
        strcpy(filter, "Lisp files");
    } else if (extension) {
        sprintf(filter, "%s files", extension);
    }
    if (extension) {
        sprintf(filter + strlen(filter), "%c*.%s%cAll files%c*.*%c",
                0, extension, 0, 0, 0);
        filter_ptr = filter;
    }
    if (!deflt) deflt = "";     /* treat NULL as the empty string */
    strcpy(fileopen_name, deflt);

    open_file_name.lStructSize = sizeof(OPENFILENAME);
    open_file_name.hwndOwner = hMainWindow;

    open_file_name.hInstance = hInst;
    open_file_name.lpstrFilter = filter_ptr; 
    open_file_name.lpstrCustomFilter = NULL; 
    open_file_name.nMaxCustFilter = 0; 
    open_file_name.nFilterIndex = 0; 
    open_file_name.lpstrFile = fileopen_name; 
    open_file_name.nMaxFile = 100; 
    open_file_name.lpstrFileTitle = NULL;
    open_file_name.nMaxFileTitle = 0; 
    open_file_name.lpstrInitialDir = NULL; 
    open_file_name.lpstrTitle = prompt;
    open_file_name.Flags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST; 
    open_file_name.nFileOffset = 0; 
    open_file_name.nFileExtension = 0; 
    open_file_name.lpstrDefExt = extension; 
    open_file_name.lCustData = 0; 
    open_file_name.lpfnHook = 0; 
    open_file_name.lpTemplateName = 0;
    if (((*mode == 'r') && GetOpenFileName(&open_file_name)) ||
        ((*mode == 'w') && GetSaveFileName(&open_file_name))) {
        return open_file_name.lpstrFile;
    }
    return NULL;
}



FILE *fileopen(char *deflt, char *extension, char *mode, char *prompt)
{
    FILE *fp = NULL;            /* file corresponding to filename */
    if (getfilename(deflt, extension, mode, prompt)) {
        fp = fopen(fileopen_name, mode);
    }
    return fp;
}

#endif


