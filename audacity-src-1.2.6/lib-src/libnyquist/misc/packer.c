/* packer -- a program to pack a list of files into one */

/* The input is a file with one line per file to be packed.
   Each line starts with either 'a' or 'b' for ascii or binary.
   The second character is a space or tab (it's ignored).
   The third character to the end of the line is the file to be packed.

   The encoding is as follows:
   A file starts with '!' for ascii or '#' for binary in column one.
   The remainder of the line is a file name/path.
   Ascii files have a special character in column 1:
       'A'-'~' indicates 0 to n blanks.  Leading Tabs are converted to blanks
           and included in this count.
     '0'-'9' indicates 0 to 9 blank lines (always followed by an additional newline).
   Otherwise, all characters are just copied to output except:
   '$' is an escape character:
        $$ indicates $,
        $@ through $_ indicate 0x00 through 0x1F,
        $\n indicates an empty string (useful for avoiding long lines on output)
        After $\n, the following character is not treated specially even though
        it is in column 1.

   Should we be doing more to compress files?  It looks like the special
   handling of leading blanks compresses files about 4%.  This is not much,
   but the encoding allows us to put markers (! and #) in column 1 to
   separate files. Originally, leading blank handling also converted between
   8 and 4 character tab stops, but you can no longer assume tab stops under,
   say, Windows, are 8 characters wide. Source files should not have
   tabs.

   Further simple encoding such as run-length encoding and word
   substitution doesn't buy too much and was deemed not worth the effort.
   Run-length encoding seems to buy another couple of percent.
   Substitution for common words like int, print, return, the, register,
   etc. buys maybe .5% per word, but it seems unlikely this will buy
   more than a total of 10%, so we're looking at a max of 15% to 20%
   compression without starting to huffman encode at the bit level.

   For binary files, every 3 bytes are used to form a 24-bit number which is
   split into 4 fields of 6 bits.  Each field is encoded by adding ascii '0'.
   If only one or two bytes are left at the end of the file, the encoding is
   as if zeros were appended to the file, but only 2 or 3 ascii characters
   (instead of the usual 4) are output.  The ascii file encoding is terminated
   with a period ('.').  Newlines are inserted to keep line lengths down but
   should be ignored by the reader.
 */


#include "switches.h"
#include "stdlib.h"
#include "string.h"
#include "cext.h"
#include "convert.h"
/* since we aren't using the cleanup package, expose exit(): */
#undef exit

#include "stdio.h"
#ifdef MACINTOSH
#include "console.h"
#endif

#define EOS 0

#define string_max 500

void pack_newline();
void pack_ascii();
void pack_binary();
void put_binary();

/* main -- pack a list of files */
/**/
int main(argc, argv)
  int argc;
  char *argv[];
{
    FILE *inf;  /* input file: a list of file names to pack */
    FILE *outf; /* the packed output */
    char filename[string_max];  /* holds names of input files */
    char convname[string_max];  /* filename converted to local syntax */
    int base = 1;
#ifdef MACINTOSH
    argc = ccommand(&argv);
#endif
    if (argc != 3 && argc != 4) {
        fprintf(stderr, "Usage: packer [-p] input-list-file output-file\n");
        exit(1);
    }
    if (argc == 4) {
        base = 2;
        if (strcmp(argv[1], "-p") == 0) {
            pauseflag = 1;
        } else {
            fprintf(stderr, "Expected \"-p\" as 1st argument.\n");
            exit(1);
        }
    }
    inf = fopen(argv[base], "r");
    if (!inf) {
        fprintf(stderr, "Couldn't open |%s|\n", argv[base]);
        exit(1);
    }
    outf = fopen(argv[base+1], "w");
    if (!outf) {
        fclose(inf);
        fprintf(stderr, "Couldn't open |%s|\n", argv[base + 1]);
        exit(1);
    }
    printf("Using tab width of %d\n", TAB_WIDTH);
    while (fgets(filename, string_max, inf)) {
        filename[strlen(filename) - 1] = EOS;   /* remove newline at end */
        if (filename[0] == EOS) continue;       /* skip blank lines */
        puts(filename);
        strcpy(convname, filename + 2);
        convert(convname);
        if (filename[0] == 'a') pack_ascii(filename + 2, convname, outf);
        else if (filename[0] == 'b') pack_binary(filename + 2, convname, outf);
        else {
            fprintf(stderr, "Bad file spec (expecting a or b in col 1): %s\n",
                     filename);
            if (PAUSE) getchar();
        }

    }
    fclose(outf);
    fclose(inf);
    return 0;
}


/* pack_ascii -- open filename and append its encoding to outf */
/**/
void pack_ascii(filename, convname, outf)
  char *filename;
  char *convname;
  FILE *outf;
{
    int line_len = 0;
    int c;
    FILE *inf;

    inf = fopen(convname, "r");
    /* printf("opened %lx\n", inf); */
    if (!inf) {
        fprintf(stderr, "Couldn't open |%s| - skipped\n", convname);
        if (PAUSE) getchar();
        return;
    }
    fprintf(outf, "!%s\n", filename);

    pack_newline(inf, outf, &line_len);
    while ((c = getc(inf)) != EOF) {
        if (c > 127) {
            fprintf(stderr, "non-ascii char 0x%x in %s.\n", c, convname);
            exit(1);
        } else if (c == '\n') {
            putc(c, outf);
            line_len = 0;
            pack_newline(inf, outf, &line_len);
        } else if (c == '$') {
            putc('$', outf);
            putc('$', outf);
            line_len += 2;
        } else if (c < 32) {
            putc('$', outf);
            putc('@' + c, outf);
            line_len += 2;
        } else {
            putc(c, outf);
            line_len++;
        }
        if (line_len > 70) {
            putc('$', outf);
            putc('\n', outf);
            line_len = 0;
        }
    }
    if (line_len) {
        fprintf(stderr, "missing newline added to the end of %s\n", convname);
        putc('\n', outf);
        if (PAUSE) getchar();
    }
    /* printf("closing %lx\n", inf); */
    fclose(inf);
}


/* pack_binary -- open binary filename and append its encoding to outf */
/**/
void pack_binary(filename, convname, outf)
  char *filename;
  char *convname;
  FILE *outf;
{
    int line_len = 0;
    int c;
    long data;
    int n;
    FILE *inf;
    boolean isbinary = false;

    inf = fopen(convname, "rb");
    /* printf("opened %lx\n", inf); */
    if (!inf) {
        fprintf(stderr, "Couldn't open |%s| - skipped\n", convname);
        if (PAUSE) getchar();
        return;
    }
    fprintf(outf, "#%s\n", filename);

    n = 0;
    data = 0;
    while ((c = getc(inf)) != EOF) {
        if (c > 127) isbinary = true;
        data = (data << 8) | c;
        n++;
        if (n == 3) {
            put_binary(data, outf);
            n = 0;
            data = 0;
            line_len += 4;
            if (line_len >= 70) {
                putc('\n', outf);
                line_len = 0;
            }
        }
    }
    if (n == 1) {
        data = data << 16;
        putc('0' + ((data >> 18) & 0x3F), outf);
        putc('0' + ((data >> 12) & 0x3F), outf);
    }
    if (n == 2) {
        data = data << 8;
        putc('0' + ((data >> 18) & 0x3F), outf);
        putc('0' + ((data >> 12) & 0x3F), outf);
        putc('0' + ((data >> 6) & 0x3F), outf);
    }
    putc('.', outf);
    putc('\n', outf);
    if (!isbinary) {
        fprintf(stderr, "%s seems to be an ascii file.\n", convname);
        if (PAUSE) getchar();
    }
    /* printf("closing %lx\n", inf); */
    fclose(inf);
}


/* pack_newline -- newline sequence encoding to outf */
/**/
void pack_newline(inf, outf, line_len)
  FILE *inf;    /* input file */
  FILE *outf;   /* where to write output */
  int *line_len;
{
    int c;
    int count = 0;
    int outc;
    
    while (((c = getc(inf)) != EOF) && (c == '\n')) {
         count++;                       
    }
    while (count >= 10) {
        fprintf(outf, "9\n");
        *line_len = 0;
        count -= 10;
    }
    if (count > 0) {
        fprintf(outf, "%c\n", '0' + count - 1);
        *line_len = 0;
    }
    
    /* now run-length encode leading blanks... */
    count = 0;
    while (c != EOF) {
        if (c == ' ') count++;
        /* we no longer convert tabs to spaces...
         else if (c == '\t') count += TAB_WIDTH;
         */
        else break;
        c = getc(inf);
    }
    if (c != EOF || count) {
        outc = 'A' + count;
        if (outc > '~') outc = '~';
        putc(outc, outf);
        (*line_len) += 1;
        count -= (outc - 'A');
        while (count > 0) {
            putc(' ', outf);
            (*line_len) += 1;
            count--;
        }
    }
    /* now do the rest of the line */
    if (c != EOF) ungetc(c, inf);
}


/* put_binary -- write 3 binary bytes as 4 ascii bytes */
/**/    
void put_binary(data, outf)
  long data;
  FILE *outf;
{
    putc('0' + ((data >> 18) & 0x3F), outf);
    putc('0' + ((data >> 12) & 0x3F), outf);
    putc('0' + ((data >> 6) & 0x3F), outf);
    putc('0' + (data & 0x3F), outf);
}
