/* cmdline.c -- command line parsing routines */
/*
 * This module is designed to allow various modules to scan (and rescan)
 * the command line for applicable arguments.  The goal is to hide as
 * much information about switches and their names as possible so that
 * switches become more consistent across applications and so that the
 * author of an application need not do a lot of work to provide numerous
 * options.  Instead, each module scans the command line for its own
 * arguments.
 *
 * Command lines are of the following form:
 *	command -s1 -s2 opt2 -s3 arg1 arg2 -s4 opt4 arg3
 *      command @filename
 * The @filename form reads commands of the first form from filename
 * Note that there are three kinds of command line parameters:
 * (1) A Switch is a "-" followed by a name, e.g. "-s1"
 * (2) An Option is a Switch followed by a space and name, e.g. "-s2 opt2"
 * (3) An Argument is a name by itself, e.g. "arg1"
 * Note also that a switch followed by an argument looks just like an
 * option, so a list of valid option names is necessary to disambiguate.
 *
 * A main program that uses cmdline.c should do the following:
 *	(1) create an array of pointers to strings (char *names[]) that
 *		contains every possible option name
 *	(2) create another array of pointers to strings that contains
 *		every possible switch name
 *	(2) call cl_init(switches, nsw, options, nopt, argv, argc)
 * cl_init will report an error (to stderr) if it finds any illegal
 * switch or option names.
 *
 * Afterward, switches, options, and arguments can be accessed by
 * calling cl_switch, cl_option, and cl_arg.  If cl_switch or cl_option
 * is called with a switch name that was not mentioned in the call to 
 * cl_init, an error will result.  This indicates that the application
 * author omitted a valid switch or option name when calling cl_init.
 * This is an error because the full set of names is needed for error
 * checking and to distinguish arguments from options.
 *
 * cl_nswitch and cl_noption are similar to cl_switch and cl_option,
 * except they each take a list of equivalent switch or option names.  
 * This makes it simple to allow both verbose (-debug) and terse (-d) names.
 */

/*****************************************************************************
*	    Change Log
*  Date	    | Change
*-----------+-----------------------------------------------------------------
* 13-Jun-86 | Created Change Log
*  6-Aug-86 | Modified for Lattice 3.0 -- use "void" to type some routines
* 27-Dec-93 | "@file" as first arg reads command line args from file
*****************************************************************************/

#include "stdlib.h"
#include "cext.h"
#include "stdio.h"
#include "ctype.h"
#include "cmdline.h"
#include "string.h"

private char **voptions;	/* valid options */
private int noptions;		/* number of options */
private char **vswitches;	/* valid switches */
private int nswitches;		/* number of switches */
private char **argv;		/* command line argument vector */
private int argc;		/* length of argv */

private int cl_rdy = false;	/* set to true when initialized */

/*****************************************************************************
*	Routines local to this module
*****************************************************************************/
private	void	check_names();
private int	find_match();
private int	find_string();
private	void	ready_check();
void indirect_command(char *filename, char ***argvp, int *argcp, char *oldarg0);


/****************************************************************
*			check_names
* Inputs:
*	char *names[]:	array of alternative switch or option names
*	int nnames:	number of alternative switch or option names
*	char *valid[]:	array of valid names
*	int nvalid:	number of valid names
* Effect:
*	Checks that all names are in validnames.  If not, print
*	an error message.
*****************************************************************/

private void check_names(names, nnames, valid, nvalid)
    char *names[];
    int nnames;
    char *valid[];
    int nvalid;
{
    int i;	/* loop counters */
    for (i = 0; i < nnames; i++) {
        if (find_string(names[i], valid, nvalid) >= nvalid) {
            fprintf(stderr, "internal error detected by cmdline module:\n");
            fprintf(stderr, "\t'%s' should be in valid lists\n", names[i]);
        }
    }
}

/****************************************************************
*			cl_arg
* Inputs:
*	n: the index of the arg needed
* Results:
*	pointer to the nth arg, or NULL if none exists
*	arg 0 is the command name
*****************************************************************/

char *cl_arg(n)
    int n;
{
    int i = 1;
    if (n <= 0) return argv[0];
    while (i < argc) {
        if (*argv[i] == '-') {
            if (find_string(argv[i], voptions, noptions) < noptions)
                i += 2; /* skip name and option */
            else i += 1; /* skip over switch name */
        } else if (n == 1) {
            return argv[i];
        } else { /* skip over argument */
            n--;
            i++;
        }
    }
    return NULL;
}

/*****************************************************************************
*			cl_init
* Inputs:
*	char *switches[]:	array of switch names
*	int nsw:		number of switch names
*	char *options[]:	array of option names
*	int nopt:		number of option names
*	char *av:		array of command line fields (argv)
*	int ac:			number of command line fields (argc)
* Effect:
*	Checks that all command line entries are valid.
*	Saves info for use by other routines.
* Returns:
*	True if syntax checks OK, otherwise false
*****************************************************************************/

boolean cl_init(switches, nsw, options, nopt, av, ac)
    char *switches[];
    int nsw;
    char *options[];
    int nopt;
    char *av[];
    int ac;
{
    int i;	/* index into argv */
    boolean result = true;

    vswitches = switches;	nswitches = nsw;
    voptions = options;		noptions = nopt;
    argv = av;			argc = ac;
    
    if (ac == 2 && *(av[1]) == '@') {
            /* read new args from file */
            indirect_command(av[1] + 1, &argv, &argc, av[0]);
    }

    for (i = 1; i < argc; i++) {  /* case fold lower */
        size_t j;
        for (j = 0; j < strlen(argv[i]); j++)
            if (isupper(argv[i][j])) 
                argv[i][j] = tolower(argv[i][j]);
    }

    /* check command line syntax: */
    i = 1;
    while (i < argc) {
        if (*argv[i] == '-') {
            if (find_string(argv[i], voptions, noptions) < noptions) {
                i += 1; /* skip name and option */
                if (i < argc && *argv[i] == '-') {
                    fprintf(stderr, "missing argument after %s\n", argv[i-1]);
                    result = false;
                    i += 1;
                }
            } else if (find_string(argv[i], vswitches, nswitches) < 
                       nswitches) {
                i += 1; /* skip over switch name */
            } else {
                fprintf(stderr, "invalid switch: %s\n", argv[i]);
                i += 1;
                result = false;
            }
        } else i++; /* skip over argument */
    }
    cl_rdy = true;
    return result;
}

/****************************************************************
*			cl_noption
* Inputs:
*	char *names[]:	array of alternative switch names
*	int nnames:	number of alternative switch names
* Result:
*	returns pointer to  if one exists, otherwise null
* Effect:
*	looks for pattern in command line of the form "-n s",
*	where n is a member of names.  Returns pointer to s.
* Implementation:
*	find the option name, then
*	see if the switch is followed by a string that does
*	not start with "-"
*****************************************************************/

char *cl_noption(names, nnames)
    char *names[];
    int nnames;
{
    int i;	/* index of switch */

    ready_check();
    check_names(names, nnames, voptions, noptions);
    i = find_match(names, nnames) + 1; /* point at the option */
    if (i < argc) { /* make sure option exists */
        if (*(argv[i]) != '-') return argv[i];
    }
    return NULL;
}

/*****************************************************************
*			cl_nswitch
* Inputs:
*	char *names[]:	array of alternative switch names
*	int nnames:	number of alternative switch names
* Effect:
*	Checks that names is valid.
*	Finds a pattern in command line of the form "-n", where
*	n is a member of names.
* Result:
*	returns pointer to command line switch if one exists,
*	otherwise null
*****************************************************************/

char *cl_nswitch(names, nnames)
    char *names[];
    int nnames;
{
    int i;	/* index of switch */

    ready_check();
    check_names(names, nnames, vswitches, nswitches);
    i = find_match(names, nnames);
    if (i < argc) return argv[i];
    /* else */ return NULL;
}

/****************************************************************
*			cl_option
* Inputs:
*	char *name:	option name
* Outputs:
*	returns char *: the option string if found, otherwise null
****************************************************************/

char *cl_option(name)
    char *name;
{
    char *names[1];	/* array to hold name */

    names[0] = name;
    return cl_noption(names, 1);
}

/****************************************************************
*			cl_switch
* Inputs:
*	char *name:	switch name
* Outputs:
*	boolean:	true if switch found
****************************************************************/

boolean cl_switch(name)
    char *name;
{
    char *names[1];	/* array to hold name */

    names[0] = name;
    return cl_nswitch(names, 1) != NULL;
}

/****************************************************************
*			find_match
* Inputs:
*	char *names[]:	array of alternative switch or option names
*	int nnames:	number of alternative switch or option names
* Effect:
*	Looks for command line switch that matches one of names.
* Returns:
*	Index of switch if found, argc if not found.
*****************************************************************/

private int find_match(names, nnames)
    char *names[];
    int nnames;
{
    int j;	/* loop counter */
    for (j = 0; j < argc; j++) {
        if (find_string(argv[j], names, nnames) < nnames) return j;
    }
    return argc;
}

/****************************************************************
*			find_string
* Inputs:
*	char *s:	string to find
*	char *names[]:	array of strings
*	int nnames:	number of strings
* Effect:
*	Looks for s in names
* Returns:
*	Index of s in names if found, nnames if not found
*****************************************************************/

private int find_string(s, names, nnames)
    char *s;
    char *names[];
    int nnames;
{
    int i; /* loop counter */
    for (i = 0; i < nnames; i++) {
        if (strcmp(s, names[i]) == 0) {
            return i;
        }
    }
    return nnames;
}

boolean is_whitespace(int c)
{
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

boolean get_arg(file, arg)
  FILE *file;
  char *arg;
{
    int c;
    while ((c = getc(file)) != EOF && is_whitespace(c)) ;
    if (c == EOF) return false;
    ungetc(c, file);
    while ((c = getc(file)) != EOF && !is_whitespace(c)) {
            *arg++ = c;
    }
    *arg = 0;
    return true;
}


void indirect_command(filename, argvp, argcp, oldarg0)
        char *filename;
    char ***argvp;
    int *argcp;
    char *oldarg0;
{
    FILE *argfile = fopen(filename, "r");
    if (!argfile) {
            *argvp = (char **) malloc(sizeof(char *));
        (*argvp)[0] = oldarg0;
        *argcp = 1;
    } else {
            int i = 1;
        char arg[100];
        while (get_arg(argfile, arg)) i++;
        fclose(argfile);
        argfile = fopen(filename, "r");
        *argvp = (char **) malloc(sizeof(char *) * i);
        (*argvp)[0] = oldarg0;
        *argcp = i;
        i = 1;
        while (get_arg(argfile, arg)) {
            (*argvp)[i] = (char *) malloc(strlen(arg) + 1);
            strcpy((*argvp)[i], arg);
            i++;
        }
    }
}


/****************************************************************
*			ready_check
* Effect:
*	Halt program if cl_rdy is not true.
*****************************************************************/
private void ready_check()
{
    if (!cl_rdy) {
        fprintf(stderr,
            "Internal error: cl_init was not called, see cmdline.c\n");
        exit(1);
    }
}
