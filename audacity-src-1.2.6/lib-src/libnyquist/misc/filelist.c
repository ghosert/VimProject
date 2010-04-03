/* filelist.c -- program to convert a DOS DIR listing into an input
 * file for the packer program
 *
 * To use the program, first run DIR *.* /s > ..\xxx
 * Then run filelist.exe ..\xxx files
 * to create "files"
 * Then you can run packer.exe files myproject.pac
 * to create the pac file.
 */

/* usage: filelist <dir filename> <packer filename> */

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "cext.h"
#include "cmdline.h"

#define EOS '\0'

FILE *inf;
FILE *out;

#ifdef MAYBE_THIS_IS_FOR_WIN95_DIRECTORY_LISTINGS
/* findfilename -- look for pattern "*-*-*:* " in string */
/**/
int findfilename(char *line, char *rslt)
{
    char *ptr;
    if (ptr = strchr(line, '-')) {
        if (ptr = strchr(ptr, '-')) {
            if (ptr = strchr(ptr, ':')) {
                if (ptr = strchr(ptr, ' ')) {
                    strcpy(rslt, ptr + 1);
                    if (strcmp(rslt, ".") == 0 ||
                        strcmp(rslt, "..") == 0 ||
                        strstr(line, "<DIR>")) return 0;
                    return 1;
                }
            }
        }
    }
    return 0;
}
#endif

/* findfilename -- look for pattern "*<slash>*<slash>*:* " in string */
/*
 * NOTE: the <slash> is "/", part of the file date; ":" is in the file time
 *   lines without these characters are not lines that contain filenames
 */
int findfilename(char *line, char *rslt)
{
    char *ptr;
    if (ptr = strchr(line, '/')) {
        if (ptr = strchr(ptr + 1, '/')) {
            if (ptr = strchr(ptr + 1, ':')) {
                if (ptr = strrchr(line, ' ')) {
                    // now ptr points to space before filename
                    strcpy(rslt, ptr + 1);
                    // make sure this is not directory
                    if (strstr(line, "<DIR>")) return 0;
                    return 1;
                }
            }
        }
    }
    return 0;
}


int directory_filter(char *directory)
{
    int skip = false;;
    if (strstr(directory, "WinDebug") != NULL) skip = true;
    if (strstr(directory, "trash") != NULL) skip = true;
    return skip;
}

/* fixup -- convert \ to / */
/**/
void fixup(char *s)
{
    while (*s) {
        if (*s == '\\') *s = '/';
        s++;
    }
}


/* giveup -- quit the program */
/**/
void giveup()
{
    printf("Type return.");
    exit(1);
}


/* 
/* main -- */
/**/
int main(int argc, char **argv)
{
#define stringmax 128
    char in_file[stringmax];
    char out_file[stringmax];
    char inline[stringmax];
    char basedir[stringmax];
    char filename[stringmax];
    char directory[stringmax];
    char wholename[stringmax];
    char *s;
    int skip_directory = false;

    basedir[0] = 0;	/* empty string */

    cl_init(NULL, 0, NULL, 0, argv, argc);
    if ((s = cl_arg(1)) != NULL) {
        strcpy(in_file, s);
        inf = fopen(in_file, "r");
        if (inf == NULL) {
            fprintf(stdout, "Error: couldn't open %s\n", in_file);
            exit(1);
        }
    } 
    if ((s = cl_arg(2)) != NULL) {
        strcpy(out_file, s);
        out = fopen(out_file, "w");
        if (out == NULL) {
            fprintf(stdout, "Error: couldn't open %s\n", out_file);
            exit(1);
        }
    } else {
        fprintf(stdout, "Error: no output file specified\n");
        exit(1);
    }

    printf("writing %s ...\n", out_file);
    
    /* read a line at a time.
     *   if the line has "Directory of", then
     *        if you don't have base directory, grab it
     *        otherwise search for base directory and grab remainder
     *   if the line matches "*-*-*:* "	then 
     *        grab remainder as filename
     *        prepend directory and type ("a") and output
     */
    while (fgets(inline, stringmax, inf)) {
        inline[strlen(inline) - 1] = EOS;   /* remove newline at end */
        if (inline[0] == EOS) continue;     /* skip blank lines */
        /* search for Directory */
        s = strstr(inline, "Directory of ");
        if (s) {
            s += strlen("Directory of ");
            if (!basedir[0]) {
                strcpy(basedir, s);
                strcat(basedir, "\\");	/* append a slash to complete directory name */
                strcpy(directory, "");
            } else {
                s = strstr(s, basedir);
                if (!s) {
                    printf("Expected %s at beginning of directory.\n");
                    printf("Input line: %s\n");
                    giveup();
                } else {
                    strcpy(directory, s + strlen(basedir));
                    fixup(directory);
                    strcat(directory, "/");
                }
            skip_directory = directory_filter(directory);
            }
        } else if (!skip_directory && findfilename(inline, filename)) {
            char type_of_file = 'a';
            sprintf(wholename, "%s%s", directory, filename);
            /* strlwr(wholename); */
            s = strchr(wholename, '.');
            if (s) s++;
            else s = "";
            if (strcmp(s, "nh") == 0 ||
                strcmp(s, "rsrc") == 0 ||
                strcmp(s, "dsp") == 0 ||
                strcmp(s, "dsw") == 0 ||
                strcmp(s, "cod") == 0 ||
                strcmp(s, "tab") == 0 ||
                strcmp(s, "pcm") == 0 ||
                strcmp(s, "mp3") == 0 ||
                strcmp(s, "mid") == 0 ||
                strcmp(s, "aiff") == 0 ||
                false)
                type_of_file = 'b';
            if (strcmp(s, "pac") == 0 ||
                strcmp(s, "ncb") == 0 ||
                strcmp(s, "opt") == 0 ||
                strcmp(s, "plg") == 0 ||
                strcmp(s, "tar") == 0 ||
                strcmp(s, "obj") == 0 ||
                strcmp(s, "vcp") == 0 ||
                strcmp(s, "exe") == 0 ||
                strcmp(s, "vcp") == 0 ||
                strcmp(s, "pdb") == 0 ||
                strcmp(s, "sbr") == 0 ||
                strcmp(s, "ilk") == 0 ||
                strcmp(s, "bsc") == 0 ||
                /* this last one says "only .lsp files in runtime directory 
                (strcmp(directory, "runtime/") == 0 && strcmp(s, "lsp") != 0) || */
                strstr(directory, "CVS/") ||
                false
               ) {
                /* do nothing */
            } else {
                fprintf(out, "%c %s\n", type_of_file, wholename);
            }
        }
    }
    
    fclose(inf);
    fclose(out);
    return 0;
}

                    
