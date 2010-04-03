/* intgen.c -- an interface generator for xlisp */

/* (c) Copyright Carnegie Mellon University 1991
 * For a statement of limited permission to use, see Permission.doc
 *
 * HISTORY
 *
 *  5-Jul-95	Roger Dannenberg
 *	strip directory prefixes from files before writing include statements
 * 24-Oct-88	Roger Dannenberg at CMU
 *	Changed so that if C routine returns void and has result parameters,
 *	then result parameters are returned by the lisp subr as well as
 *	assigned to *RSLT*.
 *
 * 13-Apr-88	Roger Dannenberg at CMU
 *	Modified for xlisp version 2.0
 *
 * 22-Dec-87    Roger Dannenberg at NeXT
 *	Added FILE type.
 *
 * 21-May-87	Dale Amon at CMU CSD
 *	Included use of NODE *s_true under SCORE_EDITOR conditional. Standard
 *	xlisp code use NODE *true instead.
 *
 * 13-May-87	Dale Amon at CMU CSD
 *	Added conditional compilation switch SCORE_EDITOR so that this
 *	program will work with both standard XLISP sources and with Roger's
 *	(ahem) modified version. Also put in error checking for case where
 *	user does not specifiy an output file so program will exit instead
 *	of coredump.
 */


/* Summary and Design: (NOTE THAT AN INTGEN MANUAL IS AVAILABLE)
 *    The first command line argument gives the name of
 * the .c file to generate.  All following arguments are
 * .h files to read and use as interface specifications.
 *
 *    The main program opens the output file, calls
 * write_prelude, and then process_file for each input
 * file.  Then call write_postlude and close the file.
 *
 *    process_file opens an input file and reads each line
 * into current_line.
 *    if the first characters of the file are "ih", then
 * the rest of the file is processed as normal, except the
 * .h extension of the file is replaced by .ih before the
 * filename is written into an include statement in the 
 * output file.  This is done to handle ".ih" files generated
 * by the Andrew Toolkit Class processor.
 *    In any case, the first line of EVERY .h file is discarded.
 * If #define is found, save the following identifier as
 *    macro_name.
 * If "LISP:" is found, then see if it is preceded by one
 * or two identifiers and an open paren.
 *    If yes, call routine_call,
 *    else call macro_call.
 *
 *    routine_call gets the first one or two identifiers off the
 * line into type_name and routine_name.  If there is just one id,
 * assign it to routine_name and make type_name = "void". 
 * If the routine_name starts with *, remove the * from 
 * routine_name and append "*" to type_name.
 * Call write_interface with type_name, routine_name, and location
 * of parameter type description after "LISP:".
 *
 *    macro_call gets a type_name from the input line after
 * "LISP:".
 * Then call write_interface with type_name, macro_name, and
 * location of parameter type description.
 *
 *    lisp function names are saved in a table, and an
 * initialization routine is written to install the new
 * SUBRs into the xlisp symbol table, as well as to lookup
 * RSLT_sym, the atom on which results are placed
 *
 *
 */

/* Turn on special handling for Roger's Score Editor if the following #define
 *    is uncommented:
 */
/* #define SCORE_EDITOR */

/* Turn on special handling for Chris's Sound Editor if the following #define
 *    is uncommented:
 */
/* #define SOUND_EDITOR */

/* Turn on special handling for Nyquist if the following #define
 *    is uncommented:
 */
#define NYQUIST

/* atom 't is named s_true if this is defined, o.w. named true: */
#define S_TRUE 1

/* Turn on special handling for CMU MIDI Toolkit seq_type:
 */
#define SEQ_TYPE

#define errfile stdout

#define ident_max 100
#define line_max 200
#define subr_max 500

/* prefix for include files not to be included in interface module */
#define no_include_prefix '~'

#define false 0
#define true 1

#include "switches.h"
#include "stdlib.h"
#include "cext.h"
#include <string.h>
#ifndef boolean
typedef int boolean;
#endif
#include "stdio.h"
#include "ctype.h"
#include "cmdline.h"
#ifdef MACINTOSH
#include "console.h"
#endif

#ifdef MACINTOSH
#define FILESEP ':'
#else
#ifdef WINDOWS
#define FILESEP '\\'
#else
#define FILESEP '/'
#endif
#endif

static char *sindex();

#define whitep(c) ((c) == ' ' || (c) == '\t')
#define symbolp(c) (isalpha(c) || (c) == '*' || (c) == '_' || (c) == '-' ||\
                    (c) == ':' || isdigit(c) || (c) == '^' || (c) == '*')

/* c and Lisp parameters are encoded in the same table.
 * Field type_id is the string name of the type.
 * For c types (return types of C routines), code is 'C',
 *   convert gives the routine for making a lisp node from
 *   the c datum.
 *   listtype_or_special is "v" for types that should be
 *   returned as LISP NIL (e.g. "void"), "s" for types
 *   that when NULL should be returned as NIL, "r"
 *   for normal types, and "" to raise an error.
 *   ctype is not used and should be NULL.
 * For Lisp types (from parameter specs), code is 'L'.
 *   convert gives the routine that extracts a C value
 *   from a lisp node whose type is given by the field
 *   getarg_or_special.
 *   c_type is the type of the local C variable which is
 *   passed as a parameter to the C routine.
 *   initializer is the initial value for result only parameters
 *
 * End of table is marked by a NULL type_id.
 *
 * Location 0 is reserved to indicate errors.
 * Location 1 MUST BE type ANY
 *
 */

#define any_index 1
struct {
    char *type_id;
    char code;
    char *convert;
    char *getarg_or_special;
    char *ctype;
    char *makenode;
    char *initializer;
} type_table[] = {
        {" ", ' ', NULL, NULL, NULL, NULL, NULL},
        {"ANY", 'L', "", "", "LVAL", "", "NIL"},
        {"ATOM", 'L', "", "xlgasymbol", "LVAL", "", "NIL"},
        {"FILE", 'L', "getfile", "xlgastream", "FILE *", "cvfile", "NULL"},
        {"FIXNUM", 'L', "getfixnum", "xlgafixnum", "long", "cvfixnum", "0"},
        {"FIXNUM", 'L', "getfixnum", "xlgafixnum", "int", "cvfixnum", "0"},
        {"FLOAT", 'L', "getflonum", "xlgaflonum", "float", "cvflonum", "0.0"},
        {"FLONUM", 'L', "getflonum", "xlgaflonum", "double", "cvflonum", "0.0"},
        {"ANYNUM", 'L', "testarg2", "xlgaanynum", "double", "cvflonum", "0.0"},
        {"STRING", 'L', "getstring", "xlgastring", "unsigned char *", "cvstring", "NULL"},
        {"BOOLEAN", 'L', "getboolean", "xlgetarg", "int", "cvboolean", "0"},
        {"atom_type", 'C', "", "r", NULL, NULL, NULL},
        {"LVAL", 'C', "", "r", NULL, NULL, "NIL"},

#ifdef SOUND_EDITOR
        /* Extensions for Sound Type: */
        {"SOUND", 'L', "getsound", "xlgasound", "SoundPtr", "cvsound", "NULL"},
        {"SoundPtr", 'C', "cvsound", "r", NULL, NULL, NULL},
#endif

#ifdef NYQUIST
        {"SOUND", 'L', "getsound", "xlgasound", "sound_type", "cvsound", "NULL"},
        {"sound_type", 'C', "cvsound", "r", NULL, NULL, NULL},
#endif
#ifdef SEQ_TYPE
         {"SEQ", 'L', "getseq", "xlgaseq", "seq_type", "cvseq", "NULL"},
        {"seq_type", 'C', "cvseq", "r", NULL, NULL, NULL}, 
/* look out! event_type is treated as void to eliminate
 * warning messages ... */
        {"event_type", 'C', "", "v", NULL, NULL, NULL},
#endif
#ifdef SCORE_EDITOR
        {"VALUE",  'L', "getval", "xlgaval", "value_type", "cvval", "NULL"},
        {"value_type", 'C', "cvval", "r", NULL, NULL, NULL},
        {"EVENT",  'L', "getevent", "xlgaevent", "event_type", "cvevent", "NULL"},
        {"event_type", 'C', "cvevent", "r", NULL, NULL, NULL},
        {"score_type", 'C', "cvevent", "r", NULL, NULL, NULL},
#endif
#ifdef DMA_EXTENSIONS
 /* begin DMA entries */
         {"DEXT", 'L', "getdext", "xlgadext", "ext_type", "cvdext", "NULL"},
         {"DEXT", 'C', "cvdext", "r", NULL, NULL, NULL},
         {"SEXT", 'L', "getsext", "xlgasext", "ext_type", "cvsext", "NULL"},
         {"SEXT", 'C', "cvsext", "r", NULL, NULL, NULL},
 /* end DMA entries */
#endif
        {"int", 'C', "cvfixnum", "r", NULL, NULL, NULL},
        {"long", 'C', "cvfixnum", "r", NULL, NULL, NULL},
        {"boolean", 'C', "cvboolean", "r", NULL, NULL, NULL},
        {"float", 'C', "cvflonum", "r", NULL, NULL, NULL},
        {"double", 'C', "cvflonum", "r", NULL, NULL, NULL},
        {"string", 'C', "cvstring", "s", NULL, NULL, NULL},
        {"char*", 'C', "cvstring", "s", NULL, NULL, NULL},
        {"char", 'C', "cvfixnum", "r", NULL, NULL, NULL},
        {"string_type", 'C', "cvstring", "s", NULL, NULL, NULL},
        {"FILE*", 'C', "cvfile", "s", NULL, NULL, NULL},
        {"void", 'C', "", "v", NULL, NULL, NULL},
/*eot*/	{NULL, ' ', NULL, NULL, NULL, NULL, NULL}};

/* subr names get saved here: */
char *subr_table[subr_max];
int subr_table_x;

#define get_c_special(i) type_table[(i)].getarg_or_special[0]
#define get_c_conversion(i) type_table[(i)].convert
#define get_lisp_extract(i) type_table[(i)].convert
#define get_lisp_getarg(i) type_table[(i)].getarg_or_special
#define get_lisp_ctype(i) type_table[(i)].ctype
#define get_lisp_makenode(i) type_table[(i)].makenode
#define get_lisp_initializer(i) type_table[(i)].initializer

static void lisp_code();
static int lookup();
static void process_file();
static void routine_call();
static void write_interface();
static void write_postlude();
static void write_prelude();
static void write_ptrfile();

char source_file[ident_max];	/* source file */
char current_line[4 * line_max];    /* current line in source file */
char out_file[ident_max];	/* output file name */
char ptr_file[ident_max];	/* ptr.h file name */
char def_file[ident_max];	/* def.h file name */

FILE *lispout = NULL;		/* output for lisp source code (if any) */

#define EOS '\000'

/* getarg -- get an identifier from a string */
/**/
int getarg(start, result, pos)
    char *start;	/* where to start scanning */
    char *result;	/* where to put the identifier */
    char **pos;		/* ptr to char after identifier in source */
{
    *result = EOS;
    while (whitep(*start) && *start != EOS) start++;
    if (*start == EOS) return false;
    if (!symbolp(*start)) return false;
    while (symbolp(*start) && *start != EOS) {
        *result = *start;
        result++;
        start++;
    }
    *result = EOS;
    *pos = start;
    return true;
}


/* error() -- print source file and line */
/**/
void error()
{
    fprintf(errfile, "\n%s: |%s|\n", source_file, current_line);
}


/* lisp_code -- write lisp code to file */
/*
 * read from inp if necessary until close comment found
 */
static void lisp_code(inp, s)
  FILE *inp;
  char *s;
{
    char lisp[line_max];
    char *endcomment;
    char *inputline;   /* for end of file detection */

    if (lispout == NULL) {
        char lisp_file_name[ident_max];
        char *extension;
        strcpy(lisp_file_name, out_file);
        extension = sindex(lisp_file_name, ".c");
        strcpy(extension, ".lsp"); /* overwrite .c with .lsp */
        lispout = fopen(lisp_file_name, "w");
        if (lispout == NULL) {
            fprintf(stdout, "Error: couldn't open %s\n", lisp_file_name);
            exit(1);
        }
        printf("writing %s ...\n", lisp_file_name);
    }

    strcpy(lisp, s);	/* don't modify s */
    inputline = lisp;
    while (inputline != NULL &&
           (endcomment = sindex(lisp, "*/")) == NULL) {
        fputs(lisp, lispout);
        inputline = fgets(lisp, line_max, inp);
    }
    strcpy(endcomment, "\n\n");
    fputs(lisp, lispout);
}


/* lookup -- find type data */
/**/
static int lookup(s, t)
    char *s;
    char t;
{
    int i = 1;
    while (type_table[i].type_id != NULL) {
        if (type_table[i].code == t &&
            strcmp(type_table[i].type_id, s) == 0)
            return i;
        i++;
    }
    return 0;
}

/* macro_call -- generate xlisp interface for C routine */
/**/
void macro_call(in, out, curline, macro_name, arg_loc)
    FILE *in;		/* input file */
    FILE *out;		/* output file */
    char *curline;	/* input line */
    char *macro_name;	/* name of the macro to call */
    char *arg_loc;	/* location after "LISP:" */
{
    char type_name[ident_max];
    if (!getarg(arg_loc, type_name, &arg_loc)) {
        error();
        fprintf(errfile, "no type given for macro.\n");
    } else {
        write_interface(in, out, type_name, macro_name, arg_loc, true);
    }
}



/* main -- generate an xlisp to c interface file */
/**/
int main(argc, argv)
    int argc;
    char *argv[];
{
    char *s;
    FILE *out;
    FILE *ptrfile;
    FILE *deffile;
    int n;

#ifdef MACINTOSH
        argc = ccommand(&argv);
#endif

    for (n = 0; n < subr_max; n++) 
    {
            subr_table[n] = (char *) malloc(ident_max);
            subr_table[n][0] = EOS;
    }
    subr_table_x = 0;

    cl_init(NULL, 0, NULL, 0, argv, argc);
    if ((s = cl_arg(1)) != NULL) {
             strcpy(out_file, s);
           if (sindex(out_file, ".") == 0) 
               strcat(out_file, ".c");
           else fprintf(stderr, 
               "1st command line argument should be a legal c identifier\n");
           out = fopen(out_file, "w");
           if (out == NULL) {
               fprintf(stdout, "Error: couldn't open %s\n", out_file);
               exit(1);
           }
             strcpy(ptr_file, s);
           strcat(ptr_file, "ptrs.h");
           ptrfile = fopen(ptr_file, "w");
           if (ptrfile == NULL) {
               fprintf(stdout, "Error: couldn't open %s\n", ptr_file);
               exit(1);
           }
             strcpy(def_file, s);
           strcat(def_file, "defs.h");
           deffile = fopen(def_file, "w");
           if (deffile == NULL) {
               fprintf(stdout, "Error: couldn't open %s\n", def_file);
               exit(1);
           }
    } else {
           fprintf(stdout, "Error: no output file specified\n");
           exit(1);
    }

    printf("writing %s ...\n", out_file);
    
    write_prelude(out, out_file);
    n = 2;
    while ((s = cl_arg(n)) != NULL) {
        printf("  %s\n", s);
        process_file(s, out);
        n++;
    }
    write_postlude(out);
    fclose(out);
    write_ptrfile(ptrfile, deffile);
    fclose(ptrfile);
    fclose(deffile);
    if (lispout != NULL) fclose(lispout);
    return 0;
}


static void process_file(fname, out)
    char *fname;
    FILE *out;
{
    FILE *in;
    char *cp;
    char *pos;
    char incl_file[ident_max];	/* name of file to include */
    char type_name[ident_max];		/* the type of the routine */
    char routine_name[ident_max];	/* the name of the routine or macro */
    char flag = fname[0];
    boolean reading_parameters = false;	/* says we've got a routine, and
        we're skipping over parameter declarations */

    if (flag == no_include_prefix) fname++;

    strcpy(source_file, fname);	/* for error reporting */
    in = fopen(fname, "r");
    if (in == NULL) {
        fprintf(errfile, "couldn't open %s\n", fname);
        return;
    }

    /* first check out the first line: if the first two characters are
        "ih", then replace fname with file.ih so that the CLASS ".ih"
        file will be included instead of this ".h" file.  This is a 
        hack to allow calls into Andrew Tool Kit objects.
     */

    strcpy(incl_file, fname);
    if (fgets(current_line, line_max, in) != NULL) {
        if (current_line[0] == 'i' && current_line[1] == 'h') {
            cp = sindex(incl_file, ".h");
            if (cp != NULL) {
                strcpy(cp, ".ih");
            }
        }
    }

    /* strip off leading directory prefix, if any */
    cp = strrchr(incl_file, FILESEP);	/* find the last slash */
    if (cp) {
        strcpy(incl_file, cp + 1 /* skip the slash */);
    }

    if (flag != no_include_prefix) fprintf(out, "#include \"%s\"\n\n", incl_file);

    while (fgets(current_line, line_max, in) != NULL) {
        cp = sindex(current_line, "#define");
        if (cp != NULL) {
            cp += strlen("#define");
            if (!getarg(cp, routine_name, &cp)) {
                error();
                fprintf(errfile, "#define not followed by identifier\n");
            }
            /* watch out for multi-line macros: */
            while (sindex(current_line, "\\\n")) {
                if (fgets(current_line, line_max, in) == NULL) return;
            }
        } else if ((cp = sindex(current_line, "LISP:")) != NULL) {
            char type_str[ident_max];
            char routine_str[ident_max];
            if (!reading_parameters &&
                getarg(current_line, type_str, &pos) &&
                getarg(pos, routine_str, &pos) &&
                pos < cp) {
                routine_call(in, out, current_line, type_str, routine_str,
                     cp + strlen("LISP:"));
            } else if (getarg(cp + strlen("LISP:"), type_str, &pos)) {
                macro_call(in, out, current_line, routine_name,
                           cp + strlen("LISP:"));
            } else routine_call(in, out, current_line, type_name, routine_name,
                     cp + strlen("LISP:"));
        } else if ((cp = sindex(current_line, "LISP-SRC:")) != NULL) {
            lisp_code(in, cp + strlen("LISP-SRC:"));
        } else if (reading_parameters && sindex(current_line, ")")) {
            reading_parameters = false;
        } else if (reading_parameters) { /* skip */ ;
        } else if (getarg(current_line, type_name, &pos) &&
                   getarg(pos, routine_name, &pos)) {
            /* we grabbed the type and routine name.  Check to see if the
             * parameter list is open but not closed on this line: */
            if (sindex(current_line, "(") && !sindex(current_line, ")")) {
                reading_parameters = true;
            }
            /* printf("saw %s %s\n", type_name, routine_name);*/
        } else {  /* wipe out names for safety: */
            type_name[0] = EOS;
            routine_name[0] = EOS;
        }
    }

    fclose(in);
}


/* routine_call -- generate xlisp interface for C routine */
/**/
static void routine_call(in, out, curline, type_name, routine_name, arg_loc)
    FILE *in;		/* input file */
    FILE *out;		/* output file */
    char *curline;	/* input line */
    char *type_name;	/* type id */
    char *routine_name;	/* routine id */
    char *arg_loc;	/* location after "LISP:" */
{

    if (*routine_name == EOS) {
        routine_name = type_name;
        type_name = "void";
    }
    if (*routine_name == '*') {
        char *r = routine_name;
        while (*r != EOS) {   /* shift left */
            *r = *(r+1);
            r++;
         }
        strcat(type_name, "*");
    }
    write_interface(in, out, type_name, routine_name, arg_loc, false);
}


/* sindex -- find substring */
/**/
static char *sindex(sup, sub)
  char *sup;	/* the containing string */
  char *sub;	/* the substring */
{
    int i;
    for ( ; *sup != EOS; sup++) {
        for (i = 0; true; i++) {
            if (*(sub+i) == EOS) return sup;
            if (*(sup+i) != *(sub+i)) break;
        }
    }
    return EOS;
}


/* write_interface -- write SUBR for xlisp */
/*
 * NOTE: if is_macro and there are no arguments, then
 *    do not write parens: e.g. "foo" instead of "foo()"
 */
static void write_interface(in, out, type_name, fn_name, arg_loc, is_macro)
    FILE *in;		/* input file */
    FILE *out;		/* output file */
    char *type_name;	/* c type for return value */
    char *fn_name;	/* c function to be called */
    char *arg_loc;	/* LISP arg type are described here */
    int is_macro;	/* true if this is a macro */
{
    char lispfn[ident_max];	/* lisp fn name */
    char *cp;		/* a temporary */
    int len;		/* a string length */
#define args_max 20
    struct {
        int index;	/* table location for this type */
        int res_flag;	/* is a result returned? */
    } args[args_max];
    char arg_type[ident_max];	/* the original type spec */
    char *c_type;	/* c type for an argument */
    char *c_str;	/* temp for a c code line */
    int argcnt = 0;	/* counts arguments */
    int i;		/* argument index */
    int result_flag = false;	/* true if there are result parameters */
    int result_x;	/* index of result type */
    char newline[line_max];	/* place to read continuation lines */


/*    printf("write_interface: %s %s %s", type_name, fn_name, arg_loc);*/
    if (*type_name == EOS || *fn_name == EOS) {
        error();
        fprintf(errfile, "Error: bad syntax, maybe missing type\n");
        return;
    }

    while (*arg_loc != '(' && *arg_loc != EOS) arg_loc++;
    if (*arg_loc == EOS) {
        error();
        fprintf(errfile, "Error: '(' expected after 'LISP:'\n");
        return;
    } else arg_loc++;
    if (!getarg(arg_loc, lispfn, &arg_loc)) {
        error();
        fprintf(stdout, "Error: lisp function name expected\n");
        return;
    }
    /* make it upper case: */
    for (cp = lispfn; *cp != EOS; cp++) {
        if (islower(*cp)) *cp = toupper(*cp);
    }

    /* save SUBR name */
    strcpy(subr_table[subr_table_x], lispfn);
    subr_table_x++;

    /* make lispfn lower case, dash, colon -> underscore: */
    for (cp = lispfn; *cp != EOS; cp++) {
        if (isupper(*cp)) *cp = tolower(*cp);
        if (*cp == '-' || *cp == ':') *cp = '_';
    }
    
    /* append continuation lines to arg_loc to handle multi-line specs */
    while (sindex(arg_loc, "*/") == NULL) {
        /* remove newline */
        if (strlen(arg_loc) > 0) 
            arg_loc[strlen(arg_loc) - 1] = EOS;
        if (fgets(newline, line_max, in) == NULL) {
            error();
            fprintf(stdout, "Error: end of file unexpected\n");
            exit(1);
        }
        if ((strlen(arg_loc) + strlen(newline)) > (3 * line_max)) {
            error();
            fprintf(stdout, 
                "Error: specification too long or missing end of comment.\n");
            exit(1);
        }
        strcat(arg_loc, newline);
    }

    fprintf(out, "/%c xlc_%s -- interface to C routine %s */\n/**/\n",
        '*', lispfn, fn_name);

    fprintf(out, "LVAL xlc_%s(void)\n{\n", lispfn);
    while (getarg(arg_loc, arg_type, &arg_loc)) {
        int result_only_flag = false;

        if (argcnt >= args_max) {
            error();
            fprintf(errfile, 
                "Internal error: too many args, increase args_max\n");
        }
        len = strlen(arg_type);
        if (arg_type[len-1] == '*') {
            arg_type[len-1] = EOS;
            args[argcnt].res_flag = true;
            result_flag = true;
        } else if (arg_type[len-1] == '^') {
            arg_type[len-1] = EOS;
            args[argcnt].res_flag = true;
            result_flag = true;
            result_only_flag = true;
        } else args[argcnt].res_flag = false;
        
        args[argcnt].index = lookup(arg_type, 'L');
        c_type = get_lisp_ctype(args[argcnt].index);

        if (c_type == NULL) {
            error();
            fprintf(errfile, "Error: %s undefined, using int.\n",
                arg_type);
            c_type = "int";
            args[argcnt].index = lookup("FIXNUM", 'L');
        }
        fprintf(out, "    %s arg%d = ", c_type,	argcnt+1);
        if (result_only_flag) {
            fprintf(out, "%s;\n",
                    get_lisp_initializer(args[argcnt].index));
         } else if (args[argcnt].index == any_index) {
            fprintf(out, "xlgetarg();\n");
        } else {
            c_str = "%s(%s());\n";
            fprintf(out,c_str,
                    get_lisp_extract(args[argcnt].index),
                    get_lisp_getarg(args[argcnt].index));
        }
        argcnt++;
    }

    if (*arg_loc != ')') {
        fprintf(errfile,
            "Warning: paren expected immediately after last arg of %s\n",
            lispfn);
    }

    /* check for close paren and close comment: */
    cp = sindex(arg_loc, ")");
    if (cp == NULL || sindex(cp+1, "*/") == NULL) {
        error();
        fprintf(errfile, "Warning: close paren and close comment expected\n");
    }

    /* lookup result type */
    result_x = lookup(type_name, 'C');
    if (result_x == 0) {
        fprintf(errfile, "(Warning) unknown type: %s, assuming void\n", 
            type_name);
        result_x = lookup("void", 'C');
    }

    /* if there are result parameters then return them rather than NIL
     * when the type is void
     */
    if (get_c_special(result_x) == 'v' && result_flag) {
        fprintf(out, "    LVAL result;\n");
    }

    if (get_c_special(result_x) != 'v') {
        /* declare result: */
        fprintf(out, "    %s result;\n", type_name);
    }

    /* check for end of argument list: */
    fprintf(out, "\n    xllastarg();\n");

    /* if there are results, we'll call cons, so
     * protect the result from garbage collection
     * if necessary
     */
    if (result_flag && strcmp(type_name, "LVAL") == 0) {
        fprintf(out, "    xlprot1(result);\n");
    }

    /* call the c routine */
    if (get_c_special(result_x) != 'v') {
        fprintf(out, "    result = ");
    } else fprintf(out, "    ");
    fprintf(out, "%s", fn_name);
    if (!is_macro || argcnt > 0) fprintf(out, "(");

    /* pass arguments: */
    for (i = 0; i < argcnt; i++) {
        if (i > 0) fprintf(out, ", ");
        if (args[i].res_flag) fprintf(out, "&");
        fprintf(out, "arg%d", i+1);
    }
    if (!is_macro || argcnt > 0) fprintf(out, ")");
    fprintf(out, ";\n");

    /* put results (if any) on *RSLT* */
    if (result_flag) {
        int wrote_one_flag = false;
        fprintf(out, "    {\tLVAL *next = &getvalue(RSLT_sym);\n");
        for (i = 0; i < argcnt; i++) {
            if (args[i].res_flag) {
                if (wrote_one_flag)
                    fprintf(out, "\tnext = &cdr(*next);\n");
                wrote_one_flag = true;
                fprintf(out, "\t*next = cons(NIL, NIL);\n");
                fprintf(out, "\tcar(*next) = %s(arg%d);",
                    get_lisp_makenode(args[i].index), i+1);
            }
        }
        fprintf(out, "\n    }\n");

        /* copy *RSLT* to result if appropriate */
        if (get_c_special(result_x) == 'v') {
            fprintf(out, "    result = getvalue(RSLT_sym);\n");
        }
    }


    /* generate xlpop() if there was an xlprot1() */
    if (result_flag && strcmp(type_name, "LVAL") == 0) {
        fprintf(out, "    xlpop();\n");
    }


    /* now return actual return value */
    if (get_c_special(result_x) == EOS) {
        error();
        fprintf(errfile, "Warning: unknown type from C, coercing to int.\n");
        fprintf(out, "    return cvfixnum((int) result);\n");
    } else if (get_c_special(result_x) == 'v' && !result_flag) {
        fprintf(out, "    return NIL;\n");
    } else if (get_c_special(result_x) == 'v' && result_flag) {
        fprintf(out, "    return result;\n");
    } else if (get_c_special(result_x) == 's') {
        fprintf(out, "    if (result == NULL) return NIL;\n");
        fprintf(out, "    else return %s(result);\n",
            get_c_conversion(result_x));
    } else {
        fprintf(out, "    return %s(result);\n",
            get_c_conversion(result_x));
    }
    fprintf(out, "}\n\n\n");
}


/* write_postlude -- write stuff at end of file */
/**/
static void write_postlude(out)
    FILE *out;
{
        /* nothing to do for version 2 */
}


/* write_ptrfile -- write function definition table */
/**/
static void write_ptrfile(pf, df)
  FILE *pf;
  FILE *df;
{
    int n;
    char *cp;
    char cname[ident_max];

    for (n = 0; n < subr_table_x; n++) {
        strcpy(cname, subr_table[n]);
        /* make cname lower case, dash,colon -> underscore: */
        for (cp = cname; *cp != EOS; cp++) {
            if (isupper(*cp)) *cp = tolower(*cp);
            if (*cp == '-' || *cp == ':') *cp = '_';
        }
        fprintf(df, "extern LVAL xlc_%s(void);\n", cname);
        fprintf(pf, "  { \"%s\",  S, xlc_%s}, \n", subr_table[n], cname);
    }
    printf("	Add %s to localdefs.h and add %s to localptrs.h\n",
        def_file, ptr_file);
}


/* write_prelude -- write stuff at head of file */
/**/
static void write_prelude(out, out_file)
    FILE *out;
    char *out_file;
{
    int i = 2;
    int col = strlen(out_file) + 21;
    char *s;
    fprintf(out, "/%c %s -- interface to  ",
         '*', out_file);
    while ((s = cl_arg(i)) != NULL) {
        if (i > 2) {
            fprintf(out, ", ");
            col += 2;
        }
        col += strlen(s) + 2;
        if (col > 65) {
            fprintf(out, "\n * ");
            col = 4 + strlen(s) + 2;
        }
        fprintf(out, "%s", s);
        i++;
    }
    fprintf(out, " */\n\n%cifndef mips\n%cinclude \"stdlib.h\"\n", '#', '#');
    fprintf(out, "%cendif\n%cinclude \"xlisp.h\"\n\n", '#', '#');
#ifdef S_TRUE
    fprintf(out, "extern LVAL s_true;\n");
    fprintf(out, "%cdefine cvboolean(i) ((i) ? s_true : NIL)\n", '#');
#else
    fprintf(out, "extern LVAL true;\n");
    fprintf(out, "%cdefine cvboolean(i) ((i) ? true : NIL)\n", '#');
#endif

    fprintf(out, "%c%s\n",
        '#',
        "define testarg2(e) (moreargs() ? (e) : (getflonum(xltoofew())))");

    fprintf(out, "%c%s\n%s\n%s\n",
        '#',
        "define xlgaanynum() (floatp(*xlargv) ? getflonum(nextarg()) : \\",
        "    (fixp(*xlargv) ? (double) getfixnum(nextarg()) : \\",
/* note: getflonum never gets called here, but this makes typechecking happy */
        "        getflonum(xlbadtype(*xlargv))))");
 
    fprintf(out, "%cdefine getboolean(lval) ((lval) != NIL)\n\n", '#');
    fprintf(out, "extern LVAL RSLT_sym;\n\n\n");
}
