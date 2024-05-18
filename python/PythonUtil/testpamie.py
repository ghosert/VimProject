if __name__ == '__main__':

    import cPAMIE

    ie = cPAMIE.PAMIE()

    # Start Script:

    ie.navigate('https://login.yahoo.com/config/mail?.intl=us')
    ie.textBoxSet('login','ghoster_e')
    ie.textBoxSet('passwd','273450')
    ie.buttonClick('.save')

    # get html
    str = ie.pageGetText()
    print str.encode('utf-8')

