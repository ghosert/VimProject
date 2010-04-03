/* unpacker -- a program to unpack a set of files */
/* See packer.c for a description of the input file format. */

#include "switches.h"
#ifdef MACINTOSH
#include <Files.h>
#include <Script.h>
#endif

#include "cext.h"
#include "convert.h"
#include "string.h"

/* since we aren't using the cleanup package, expose exit(): */
#undef exit

#include "stdio.h"
#ifdef THINK_C
#include "console.h"
#endif

#define string_max 500
#ifndef EOS
#define EOS 0
#endif

void escape();
void unpack_ascii();
void unpack_binary();
void put_run();

#ifdef UNIX
#define FILESEP '/'
#include <sys/types.h>
#include <dirent.h>

int dir_isvaliddir(char *path)
{
    DIR *dir = opendir(path);
    if (!dir) return false;
    closedir(dir);
    return true;
}


#include <sys/stat.h>
int _mkdir(char *p)
{
    int rslt = mkdir(p, S_IRUSR | S_IWUSR | S_IXUSR | 
                        S_IXGRP | S_IWGRP | S_IROTH | S_IXOTH);
    printf("_mkdir: %s, rslt %d\n", p, rslt);
    if (rslt == -1) {
        printf("mkdir error\n");
        perror("unpacker mkdir");
    }
    return rslt;
}

#endif

#ifdef WINDOWS
#define FILESEP '\\'

// deal with some incompatible definitions
#undef byte
#undef boolean
#include "windows.h"
#include <direct.h>
/*
 * Function: dir_isvaliddir
 *
 * Purpose:
 *
 * Is this a valid directory ? 
 */
BOOL dir_isvaliddir(LPSTR path)
{
    DWORD dwAttrib;
    dwAttrib = GetFileAttributes(path);
    if (dwAttrib == -1) {
        return(FALSE);         }
    if (dwAttrib & FILE_ATTRIBUTE_DIRECTORY) {
        return(TRUE);
    }
    return(FALSE);
} /* dir_isvaliddir */ 
#endif

/* early_eof -- print error message and exit */
void early_eof()
{
    fprintf(stderr, "Unexpected end of file while unpacking\n");
    exit(1);
}
#ifdef MACINTOSH

#define FILESEP ':'

int dir_isvaliddir(char *path)
{
    char filename[256];
    OSErr err;
    FSSpec spec;
    strcpy(filename, path);
    c2pstr(filename);
    err = FSMakeFSSpec(0, 0, (unsigned char *) filename, &spec);
    if (err == noErr) {
        /* if we can open this as a file, it's not a directory */
        SInt16 refnum;
        err = FSpOpenDF(&spec, fsCurPerm, &refnum);
        if (err == noErr) {
            FSClose(refnum);
            return false;
        }
        return true;
    }  
    return false;
}


int _mkdir(char *p)
{
    OSErr err;
    FSSpec spec;
    SInt32 dirid;
    spec.vRefNum = 0;
    spec.parID = 0;
    strcpy((char *) spec.name, p);
    c2pstr(spec.name);
    err = FSpDirCreate(&spec, smSystemScript, &dirid);
    if (err == noErr) {
        return 0;
    }

    if (err != noErr) {
        printf("mkdir error %d\n", err);
    }
    return -1;
}
#endif


void make_path(char *full_name)
// make directories as needed
{
    char directory[256];
    char *ptr = full_name;
    while (ptr = strchr(ptr, FILESEP)) {
        strcpy(directory, full_name);
        directory[ptr - full_name] = 0;
        if (!dir_isvaliddir(directory)) {
            if (_mkdir(directory) != 0) {
                printf("Could not create %s\n", directory);
                return;
            } else {
                printf("Created directory %s\n", directory);
            }
        }
        // now directory is valid, so move to next one
        ptr++;
    }
}


/* main -- unpack a set of files */
/**/
int main(argc, argv)
  int argc;
  char *argv[];
{
    FILE *inf;  /* input file: a packed set of files */
    FILE *outf; /* a file to unpack */
    char filename[string_max];  /* holds names of inptu files */
#ifdef MACINTOSH
    argc = ccommand(&argv);
#endif
    if (argc != 2) {
        fprintf(stderr, "Usage: unpack input-file\n");
        exit(1);
    }
    inf = fopen(argv[1], "r");
    if (!inf) {
        fprintf(stderr, "Couldn't open |%s|\n", argv[1]);
        exit(1);
    }
    while (fgets(filename, string_max, inf)) {
        char *filetype = "w";
        filename[strlen(filename) - 1] = EOS;   /* remove newline at end */
        puts(filename + 1); /* don't print the leading ! or # */
        convert(filename + 1);  /* convert to local filename conventions */
        if (filename[0] == '#') filetype = "wb";
        outf = fopen(filename + 1, filetype);
        if (!outf) {
            make_path(filename + 1);
            outf = fopen(filename + 1, filetype);
            if (!outf) {
                fprintf(stderr, "Couldn't open |%s|\n", filename + 1);
                exit(1);
            }
        }
        if (filename[0] == '!') {
            unpack_ascii(inf, outf, filename + 1);
        } else if (filename[0] == '#') {
            unpack_binary(inf, outf);
        }
        if (outf) fclose(outf);
    }
    fclose(inf);
    return 0;
}


/* put_run -- output a run of characters */
/**/
void put_run(f, c, n)
    FILE *f;
    int c;
    int n;
{
    while (n--) putc(c, f);
}


/* unpack_ascii -- from inf to outf */
/**/
void unpack_ascii(inf, outf, filename)
    FILE *inf;
    FILE *outf;
    char *filename;
{
    for (;;) {
        int c = getc(inf);
        if (c == EOF) return;
        else if (c > 127) {
            fprintf(stderr, "Non-ascii char 0x%x found while unpacking %s.\n", c, filename);
            return;
        } else if (c >= 'A' && c <= '~') {
            int n = (c - 'A');
            //DO NOT OUTPUT LEADING TABS -- USE SPACES INSTEAD
            // int tabs = (n / TAB_WIDTH);
            // n -= tabs * TAB_WIDTH;
            // put_run(outf, '\t', tabs);
            put_run(outf, ' ', n);
        } else if (c >= '0' && c <= '9') {
            put_run(outf, '\n', c - '0');
        } else if (c == '!' || c == '#') {
            ungetc(c, inf);
            return;
        } else {
            fprintf(stderr, "Unexpected char in col 1 (%c) while unpacking %s.\n",
                                                        c, filename);
            return;
        }
        
        /* now get rest of the line */
        while ((c = getc(inf)) != EOF) {
            if (c == '$') {
                c = getc(inf);
                if (c == EOF) {
                    early_eof();
                } else if (c == '$') {
                    putc('$', outf);
                } else if (c >= '@' && c <= '_') {
                    putc(c - '@', outf);
                } else if (c == '\n') {
                    ; /* do nothing */
                } else {
                    fprintf(stderr, "Bad char (%c) after '$' while unpacking %s.\n",
                                                c, filename);
                }
            } else {
                putc(c, outf);
                if (c == '\n') break;   /* go up and process col. 1 char */
            }
        }
    }
}


/* unpack_binary -- from inf to outf */
/**/
void unpack_binary(inf, outf)
    FILE *inf;
    FILE *outf;
{
    for (;;) {
        long l;
        int c = getc(inf);
        if (c == EOF) {
            early_eof();
        } else if (c == '.') {
            break;
        } else if (c == '\n') {
            ; /* do nothing */
        } else {
            l = c - '0';
            c = getc(inf);
            if (c == EOF) {
                    early_eof();
            } else {
                l = (l << 6) + (c - '0');
                c = getc(inf);
                if (c == EOF) {
                    early_eof();
                } else if (c == '.') {
                    putc(l >> 4, outf);
                    break;
                } else {
                    l = (l << 6) + (c - '0');
                    c = getc(inf);
                    if (c == EOF) {
                            early_eof();
                    } else if (c == '.') {
                        putc((l >> 10) & 0xFF, outf);
                        putc((l >> 2) & 0xFF, outf);
                        break;
                    } else {
                        l = (l << 6) + (c - '0');
                        putc((l >> 16) & 0xFF, outf);
                        putc((l >> 8) & 0xFF, outf);            
                        putc(l & 0xFF, outf);
                    }
                }
            }
        }
    }
    getc(inf);  /* read the final newline */
}
