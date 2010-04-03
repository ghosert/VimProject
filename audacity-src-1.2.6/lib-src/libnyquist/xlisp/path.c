/*
 * New xlisp_path code by Dominic Mazzoni
 *
 * There is now a function provided to set the xlisp_path.
 * This is particularly useful for external programs
 * (e.g. Audacity, or a Nyquist GUI) that have their own
 * mechanism of setting/finding the path.  If xlisp_path
 * is NULL, the old platform-specific methods are still
 * used.
 */

#include <string.h>

#include "switches.h"
#include "xlisp.h"

static char *g_xlisp_path = NULL;

void set_xlisp_path(const char *p)
{
    if (g_xlisp_path) {
        free(g_xlisp_path);
        g_xlisp_path = NULL;
    }

    if (p) {
        g_xlisp_path = malloc(strlen(p)+1);
        strcpy(g_xlisp_path, p);
    }
}

#ifdef UNIX
const char *unix_return_xlisp_path()
{
    char *paths = getenv("XLISPPATH");
    if (!paths || !*paths) {
        char msg[512];
        sprintf(msg, "\n%s\n%s\n%s\n%s\n%s\n",
                "Warning: XLISP failed to find XLISPPATH in the environment.",
                "If you are using Nyquist, probably you should cd to the",
                "nyquist directory and type:",
                "    setenv XLISPPATH `pwd`/runtime:`pwd`/lib",
                "or set XLISPPATH in your .login or .cshrc file.");
        errputstr(msg);
    }
    return paths;
}
#endif

#ifdef WINDOWS
#include "winfun.h"

const char *windows_return_xlisp_path()
{
    #define paths_max 1024
    static char paths[paths_max];
    get_xlisp_path(paths, paths_max);
    /* make sure we got paths, and the list is not empty */
    if (!paths[0]) {
        sprintf(paths, "\n%s\n%s\n%s\n",
           "Warning: XLISP failed to find XLISPPATH in the Registry.",
           "You should follow the installation instructions. Enter an",
           "empty string if you really want no search path.");
        errputstr(paths);
    }

    return paths;
}
#endif

#ifdef MACINTOSH
const char *mac_return_xlisp_path()
{
    #define paths_max 1024
    static char paths[paths_max];
    int prefs_found = false;
    get_xlisp_path(paths, paths_max, &prefs_found);
    if (!paths[0]) {
        if (prefs_found) {
            sprintf(paths, "\n%s\n%s\n%s\n",
             "Warning: XLISP failed to find XLISPPATH in XLisp Preferences.",
             "You should probably delete XLisp Preferences and let XLisp",
             "create a new one for you.");
        }
        else {
           sprintf(paths, "\n%s\n%s\n%s\n%s\n%s\n",
            "Warning: XLISP failed to find XLisp Preferences.",
            "You should manually locate and load the file runtime:init.lsp",
            "Nyquist will create an XLisp Preferences file to automatically",
            "find the file next time. You may edit XLisp Preferences to add",
            "additional search paths, using a comma as separator.");
        }
        errputstr(paths);
    }

    return paths;
}
#endif

const char *return_xlisp_path()
{
    if (g_xlisp_path)
        return g_xlisp_path;

#ifdef WINDOWS
    return windows_return_xlisp_path();
#endif
#ifdef MACINTOSH
    return mac_return_xlisp_path();
#endif
#ifdef UNIX
    return unix_return_xlisp_path();
#endif
}

#ifdef UNIX
static const char pathchar = '/';
static const char sepchar = ':';
#endif

#ifdef WINDOWS
static const char pathchar = '\\';
static const char sepchar = ',';
#endif

#ifdef MACINTOSH
static const char pathchar = ':';
static const char sepchar = ',';
#endif

char *g_xlptemp = NULL;

const char *find_in_xlisp_path(const char *fname)
{
    const char *paths = return_xlisp_path();
    if (!paths)
        return NULL;

    while (paths && *paths) {
        FILE *fp;
        const char *start;
        int len;

        /* skip over separator */
        while (*paths == sepchar) paths++;

        /* find next directory */
        start = paths;
        while (*paths && (*paths != sepchar))
            paths++;

        if (g_xlptemp) {
           free(g_xlptemp);
           g_xlptemp = NULL;
        }

        len = paths - start;
        g_xlptemp = malloc(len + strlen(fname) + 10);
        memcpy(g_xlptemp, start, len);

        if (len == 0)
           continue;

        /* add "/" if needed */
        if (g_xlptemp[len-1] != pathchar)
           g_xlptemp[len++] = pathchar;
        
        /* append the file name */
        memcpy(&g_xlptemp[len], fname, strlen(fname));
        len += strlen(fname);
        g_xlptemp[len] = 0;

        /* append the .lsp extension */
        if (needsextension(g_xlptemp))
           strcat(g_xlptemp, ".lsp");

        fp = osaopen(g_xlptemp, "r");
        if (fp) {
           fclose(fp);

           #ifdef MACINTOSH
           /* We found the file ok, call setup_preferences to create
            * XLisp Preferences file (this only happens if previous
            * attempt to find the file failed
            */
           setup_preferences(g_xlptemp);
           #endif

           return g_xlptemp;
        }
    }

    /* It wasn't found */
    return NULL;
}
