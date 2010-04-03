import os, sys, twill

def set_is_html(bool):
    "If you find not viewing HTML error, try set this method as true."
    if bool:
        twill.commands.run('import twill;b = twill.get_browser();b._browser._factory.is_html = True;twill.browser = b')
    else:
        twill.commands.run('import twill;b = twill.get_browser();b._browser._factory.is_html = False;twill.browser = b')

def open_html(filename):
    """ Add new clause 'open_html filename' for twill script
        >>open_html "temp.hml"
    """
    globals_dict, locals_dict = twill.namespaces.get_twill_glocals()
    enable_open_html = eval('enable_open_html', globals_dict, locals_dict)
    if enable_open_html:
        twill.commands.save_html(filename)
        os.system('start ' + filename)


if __name__ == '__main__':
    """
        Usage:

        'wintwill.py' to start twill interpreter
        'wintwill.py  [ -u initial_url ] [ -o ] script(s)' to execute the script(s) which is a list of script file names.
        -u option will start wintwill.py with initial url, this option is a twill built-in feature.
        -o option will search the clause 'open_html temp.html' in script, when finding it, save the current html to temp.html and open it (self-made feature).
        'wintwill.py -h' to see more options.

    """

    import getopt

    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hqifnvu:o')
    except getopt.GetoptError:
        print 'You are inputting wrong options, please follow the format below:'
        print 'wintwill [ -u initial_url ] [ -o ] script(s)'
        print 'Or "wintwill -h" to see more options'
        sys.exit(1)

    # import the module itself so that it actually extend the functions for twill script, here is 'open_html' clause.
    twill.commands.extend_with('wintwill')
    twill.commands.setglobal('enable_open_html', False)

    # If there is -o option in command line, search 'open_html temp.html' clause, when finding, save and open temp.html.
    for opt in opts:
        if '-o' in opt:
            twill.commands.setglobal('enable_open_html', True)
            # get rid of '-o', since twill.shell.main() below can not support self-definition option -o, which will throw errors.
            sys.argv.remove('-o')


    # I have to use try-finally clause for the 'twill.shell.main()' will return with sys.exit().
    try:
        twill.shell.main()
    finally:
        # if '-h' in sys.argv, add self defined option '-o'.
        if '-h' in sys.argv:
            print '  -o        check "open_html temp.html" clause in twill, when finding the clause, save and open current html to the temp.html'
            print
            print 'Important notice: If you find "not viewing HTML error", try set this method as true in twill script: "set_is_html true"' 

