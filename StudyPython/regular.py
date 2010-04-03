if __name__ == '__main__':
    import re
    str = 'jiaSweiFzhangFjing'
    # I want to know all the words between 'S' and 'F'
    print re.search(r'S(.*)F', str).groups()[0]
    print re.search(r'S(.*)F', str).group()
    # Wait! I just want to get the words between 'S' and FIRST 'F'
    print re.search(r'S(.*?)F', str).groups()[0]
    print re.search(r'S(.*?)F', str).group()
    # CONCULUTION: .* HERE MEANS ANY WORDS WHILE ? MEANS LAZY SEARCHING.

    str = 'jiaSw\neiFzhangFjing'
    matcher = re.search(r'S(.*?)F', str)
    if not matcher:
        print 'Can not find matches with the way before if contains "\\n"'
        print re.search(r'S([\w\W]*?)F', str).groups()[0]



