Make the pydev support:
1) pyqt code completion for method arguments tips and full api explanation when press "." after object such as: "QMainWindow()."
2) show doc string when hover on QMainWindow().show()



=============================================================================================================================================================
How to use:
1. Unzip & Copy the original pydev plugin from ResultFile\org.python.pydev.feature-*.zip to eclipse.(Download this zip file from pydev official site)
2. Unzip & Copy the hacked pydev plugin from ResultFile\org.python.pydev_*.jar to eclipse to replace the original one and restart eclipse.
=============================================================================================================================================================



=============================================================================================================================================================
To develop hacked pydev plugin:
1. Double click on pyqt_tags.py to produce the tag file classInfoMap.qt
2. Copy the whole D:\eclipse_cdt into another folder, open this eclipse, create a new workspace "TestPydevWorkSpace".
3. Unzip JavaPart\org.python.pydev.feature-1.5.0.1251989166-sources.zip and import all the eclipse projects to "TestPydevWorkSpace",
   Copy classInfoMap.qt to the project named org.python.pydev\PySrc
   Compared/Replace JavaPart\PythonShell.java and JavaPart\PyTextHover.java with the original ones in org.python.pydev project(search "Jiawei" in these two java files to see the hacked changes)
   To debug/test your hacked plugins, right click on any project you imported just now, select "Run/Debug as Eclipse Application". In the following new eclipse, create python project for testing.
   Export the org.python.pydev project in using plugin.xml which will produce a hacked org.python.pydev_*.jar(Put this resulting file to ResultFile folder above.)
4. When update PyQt version, you don't need follow the steps above, just:
   1) Uninstall old PyQt, reinstall new one.
   2) Produce the new classInfoMap.qt(repeat step 1 above) and then put it into both \JavaPart and \ResultFile\org.python.pydev_*.jar
   3) Repeat "How to use" step 2 above.
=============================================================================================================================================================



=============================================================================================================================================================
#TODO: add function method in Module html document(Such as QtGui Module), make pydev support module function calltips.

#TODO: When F2 pressed on the text in hover, the text should be formatted.

#TODO: Hover doc string doesn't work well if the Qt object is defined in a self-defined class like:
class Test:
    def __init__(self):
        app = QApplication
        app.aboutQt()
In this case hover on "aboutQt()" can not present the doc string.
Work around for this problem: Use QApplication.aboutQt() to show the hover doc string.

#TODO: The connect clause below does not work, because the error from the document. Phonon.AudioOutput Class inherits "Phonon.AbstractAudioOutput", but the pyqt document says it's "AbstractAudioOutput"
from PyQt4.phonon import Phonon
Phonon.AudioOutput.connect()

#TODO: This tags can not support overload method for pyqt classes, including most __init__() method call tips and some methods for example:
QTableWidget Class:
__init__ (self, QWidget parent = None)
__init__ (self, int rows, int columns, QWidget parent = None)
QListWidget Class:
addItem (self, QListWidgetItem aitem)
addItem (self, QString label)

#TODO: QWidget.addActions(QList&lt;QAction_*&gt;list-of-QAction) should be QWidget::addActions ( QList<QAction *> actions ), convert &lt; &gt; to < > in pyqt_tags.py when generating classInfoMap.qt?
=============================================================================================================================================================
