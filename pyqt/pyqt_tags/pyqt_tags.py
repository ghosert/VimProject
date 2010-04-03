import os, sys, re
from pyqt_data import *

class PyQtTags:
    def __init__(self, result_location):
        self.classInfoMap = None
        self.result_location = result_location

    def __start_tags(self, html):

        # Find class name.
        pattern = re.compile(r'<h1.*?>(.*?) Class Reference')
        matcher = pattern.search(html, 0)
        if not matcher:
            return
        endpos = matcher.end()
        className = matcher.groups()[0]
        #print 'Class Name:'
        #print className
        #print

        # Find module info.
        #pattern = re.compile(r'\[.*?>(.*?)</a> module\]')
        #matcher = pattern.search(html, endpos)
        #if not matcher:
        #    return
        #endpos = matcher.end()
        #moduleInfo = matcher.groups()[0]

        # Find parent objects info and get a parent objects list
        pattern = re.compile(r'<p>Inherits(.*?)</p>')
        matcher = pattern.search(html, endpos)
        parentClassNameList = []
        if matcher:
            endpos = matcher.end()
            parentClassNameList = re.findall(r'>(Q.*?)</a>', matcher.groups()[0])
            #print 'Parent Class Name List:'
            #print parentClassNameList
            #print

        # Ready to set methodInfoMap
        methodInfoMap = MethodInfoMap()

        # Find methods info
        pattern = re.compile(r'<h3>Methods</h3>')
        matcher = pattern.search(html, endpos)
        if matcher:
            endpos = matcher.end()
            matcher = self.__setMethodInfoMap(methodInfoMap, html, endpos, 'Qt Method')

        # Find static methods info
        pattern = re.compile(r'<h3>Static Methods</h3>')
        matcher = pattern.search(html, endpos)
        if matcher:
            endpos = matcher.end()
            matcher = self.__setMethodInfoMap(methodInfoMap, html, endpos, 'Qt Static Method')

        # Find Qt Signals info
        pattern = re.compile(r'<h3>Qt Signals</h3>')
        matcher = pattern.search(html, endpos)
        if matcher:
            endpos = matcher.end()
            matcher = self.__setMethodInfoMap(methodInfoMap, html, endpos, 'Qt Signal')

        # Set classInfoMap
        self.classInfoMap.setValue(className, methodInfoMap, parentClassNameList)

    def __setMethodInfoMap(self, methodInfoMap, html, endpos, type):

        pattern = re.compile(r'<ul>(.*?)</ul>')
        matcher = pattern.search(html, endpos)
        methodMatchedHtml = matcher.groups()[0]
        methodList = re.findall(r'/>(.*?)<b><a.*?#(.*?)".*?\((.*?)\)</li>', methodMatchedHtml) # [('returnValue ', 'methodName', 'parameterList'), ...]"
        for methodInfo in methodList:
            methodName = self.__cleanHtml(methodInfo[1])
            args = self.__cleanHtml(methodInfo[2])
            args = self.__convertParameterListFormat(args)
            doc = self.__cleanHtml(self.__getFunctionDescription(methodName, html))
            doclines = doc.splitlines()
            doc = doclines[0] + '\n\n' + '\n'.join(doclines[1:])
            doc = '{0} \n\n{1}'.format(type, doc)
            #print 'Method Signature:'
            #print methodName + args
            #print
            #print 'Method Description:'
            #print doc
            #print
            methodInfoMap.setValue(methodName, args, doc)
        return matcher

    def __convertParameterListFormat(self, args):
        "Change args format to meet pydev such as: 'QAction action, int style = 1 ' to '(QAction_action, int_style=1)' "
        argsList = args.split(', ')
        newArgs = ''
        for args in argsList:
            args = re.sub(' = ', '=', args)
            args = re.sub(' ', '_', args)
            if len(newArgs) > 0:
                newArgs = newArgs + ', '
            newArgs = newArgs + args
        newArgs = '({0})'.format(newArgs)
        return newArgs

    def __getFunctionDescription(self, methodName, html):
        matcher = re.search(r'<a name="{0}" />([\w\W]*?)(<h3|<address>)'.format(methodName), html)
        if matcher:
            return matcher.groups()[0]
        else:
            return ''

    def __cleanHtml(self, html):
        html = re.sub('<p>', '\n', html)
        html = re.sub('<.*?>', '', html)
        html = re.sub('&#160;', ' ', html)
        html = re.sub('&amp;', '&', html)
        html = re.sub('(self, |self)', '', html)
        return html

    def produce(self, doc_location):
        self.classInfoMap = ClassInfoMap()
        for root, folders, files in os.walk(doc_location):
            if root == doc_location:
                for file in files:
                    filename = root + os.sep + file
                    print 'Handling {0}'.format(filename)
                    with open(filename) as file:
                        html = file.read()
                        # Start Tags here.
                        self.__start_tags(html)

        # Save tag info into classInfoMap.qt.
        with open(self.result_location, 'w') as file:
            for className, (methodInfoMap, parentClassNameList) in self.classInfoMap.map.iteritems():
                file.write('{0}!!!!!'.format(className))
                file.write('{0}!!!!!'.format(';'.join(parentClassNameList)))
                for methodName, (args, doc) in methodInfoMap.map.iteritems():
                    file.write('{0}@@@@@{1}@@@@@{2}#####'.format(methodName, args, doc))
                file.write('*****')

    def __loadClassInfoMap(self):
        if not self.classInfoMap:
            if os.path.isfile(self.result_location):
                with open(self.result_location) as file:
                    self.classInfoMap = ClassInfoMap()
                    tag_content = file.read()
                    classInfoList = tag_content.split('*****')
                    for classInfo in classInfoList:
                        if classInfo == '':
                            continue
                        className, parentClassNames, methodInfos = classInfo.split('!!!!!')
                        if parentClassNames == '':
                            parentClassNameList = []
                        else:
                            parentClassNameList = parentClassNames.split(';')

                        # Set the values to methodInfoMap
                        methodInfoMap = MethodInfoMap()
                        methodInfoList = methodInfos.split('#####')
                        for methodInfo in methodInfoList:
                            if methodInfo == '':
                                continue
                            methodName, args, doc = methodInfo.split('@@@@@')
                            methodInfoMap.setValue(methodName, args, doc)

                        # Set the values to classInfoMap
                        self.classInfoMap.setValue(className, methodInfoMap, parentClassNameList)

        if self.classInfoMap:
            return True
        else:
            return False

    def getArgsDoc(self, className, methodName):
        """
        className: QApplication
        methodName: aboutQt
        Return: (args, doc) or None if no available value, or ('(Please produce the tag file first.)', 'Please produce the tag file first.')
        """
        if not self.__loadClassInfoMap():
            return ('(Please produce the tag file first.)', 'Please produce the tag file first.')

        classInfo = self.classInfoMap.getValue(className)
        if not classInfo:
            return None
        
        methodInfoMap, parentClassNameList = classInfo
        methodInfo = methodInfoMap.getValue(methodName)
        if methodInfo:
            return methodInfo

        for className in parentClassNameList:
            argsDoc = self.getArgsDoc(className, methodName)
            if argsDoc:
                return argsDoc

        return None

if __name__ == '__main__':

    PYTHONPATH = os.sep.join(sys.executable.split(os.sep)[0:-1])
    doc_location = PYTHONPATH + '\Lib\site-packages\PyQt4\doc\html'
    result_location = 'classInfoMap.qt'

    if not os.path.isdir(doc_location):
        print 'Make sure you have already installed PyQt4'
        raw_input('Press any key to exit.')
        exit(1)

    tags = PyQtTags(result_location)

    prompt = "\nProduce PyQt Tags: press '1' \nTest PyQt Tags: press '2' \nExit System: press '3'\n\nYour choice:"
    while True:
        answer = raw_input(prompt)
        if answer == '1':
            tags.produce(doc_location)
        if answer == '2':
            while True:
                prompt_getArgsDoc = "\nInput className.methodName like 'QApplication.aboutQt' , press 'n' to exit:\n\nYour input:"
                answer_getArgsDoc = raw_input(prompt_getArgsDoc)
                if answer_getArgsDoc == 'n':
                    break
                try:
                    className, methodName = answer_getArgsDoc.split('.')
                except:
                    print 'Make sure you are inputting correct infos.'
                    continue
                argsDoc = tags.getArgsDoc(className, methodName)
                if argsDoc:
                    args, doc = argsDoc
                    print
                    print '================================================================='
                    print 'Method Signature:'
                    print answer_getArgsDoc + args
                    print
                    print 'Method Description:'
                    print doc
                    print '================================================================='
                    print
                else:
                    print
                    print 'No information for this method.'
        if answer == '3':
            exit(0)

