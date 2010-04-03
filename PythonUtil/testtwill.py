if __name__=='__main__':
    # interpreter mode for twill.
    # import twill.shell
    # twill.shell.main()

    #get browser from twill, rarely to use.
    #from twill import get_browser
    #b = get_browser()
    #b.go("http://www.google.com")

    # execute a twill script in python, result like show() for HTML can be got in python.
    # also, list(showlinks()) will return link object list. That means you can get more useful info in python with twill method.
    #import twill
    #from twill.commands import *
    #go("http://www.google.com")
    #showforms()
    #links = showlinks()
    #print '=========Get and Print the first link informations in python.============='
    #print list(links)[0]
    #print '=========Get and Print the html text informations.========================'
    #html = show()
    #print html

    #execute a twill script in a string or in a file
    import twill
    twill_script = """setlocal username "ghost_e"
                      setlocal password "13916939847273450"
                      # debug "http" 1
                      go https://developer.emetrix.com
                      fv 1 UserName $username
                      fv 1 Password $password
                      submit
                      follow "Windows Audio Recorder Professional"
                      follow "Send Download Link"
                      fv 2 Format "PRINTLINK"
                      # can not submit directly here, for the formaction url contains space which should be encoded first,
                      # so I rewrite the form action attributes as below:
                      fa 2 "https://developer.emetrix.com/storefront/send_DL.asp?PID=11784033&UN=ghost_e&FFN=Official_Recorder_Professional.exe&PN=Windows%20Audio%20Recorder%20Professional&FFS=5119714"
                      submit
                   """
    twill.execute_string(twill_script)
    # and then use twill in python to show & analyze the result.
    # twill.commands.* contains all the twill script command, below is 'show' command.
    html = twill.commands.show()
    print '============================================'
    print html
    print '============================================'
    links = list(twill.commands.showlinks())
    print links
    print
    print '===============Way 1 showlinks() method to Get the link you want.============================='
    print
    for link in links:
        if 'Official_Recorder_Professional.exe' in link.url:
            print link.url
            break
    print
    print '===============Way 2 regular expression to Get the link you want.============================='
    print
    import re
    pattern=r"(?<=href=\")(http://.*?Official_Recorder_Professional\.exe)"
    print re.search(pattern, html).group()
    print
    # Conclution: This is a best pratice to use twill & python.

# -- TWILL POST WITHOUT FORM
# from twill import get_browser
# from twill import commands as c
# import urllib

# b = get_browser()

# # log in
# b.go('http://www.blah.com/');
# c.fv(1, 'username', 'test')
# c.fv(1, 'password', 'test')
# c.submit()
# c.find('Welcome')

# # now post a request without a form
# mb = b._browser
# data = urllib.urlencode({'some_var':'blah','another_var':'foo'})
# r = mb.open("http://www.blah.com/postRequestHere/", data)

# # print the response
# print r.read()
# --


