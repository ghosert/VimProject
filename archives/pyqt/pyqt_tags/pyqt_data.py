class MethodInfoMap:
    """ methodName: 'aboutQt'
        args: '(int, bool)'
        doc: 'Qt Signal \\n\\n QWidget QApplication.widgetAt (int x, int y) \\n\\n Detail description.'
    """
    def __init__(self):
        self.map = {}

    def setValue(self, methodName, args, doc):
        self.map[methodName] = (args, doc)
    
    def getValue(self, methodName):
        if self.map.has_key(methodName):
            return self.map[methodName]
        else:
            return None

class ClassInfoMap:
    """ className: 'QApplication'
        methodInfoMap: MethodInfoMap
        parentClassNameList: ['QApplication', 'QDialog']
    """
    def __init__(self):
        self.map = {}

    def setValue(self, className, methodInfoMap, parentClassNameList):
        self.map[className] = (methodInfoMap, parentClassNameList)

    def getValue(self, className):
        if self.map.has_key(className):
            return self.map[className]
        else:
            return None

