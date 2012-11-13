/**
 * Copyright (c) 2005-2011 by Appcelerator, Inc. All Rights Reserved.
 * Licensed under the terms of the Eclipse Public License (EPL).
 * Please see the license.txt included with this distribution for details.
 * Any modifications to this file must keep this entire header intact.
 */
/*
 * Created on Aug 16, 2004
 *
 * @author Fabio Zadrozny
 */
package org.python.pydev.editor.codecompletion.shell;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.python.pydev.core.IInterpreterInfo;
import org.python.pydev.core.IInterpreterManager;
import org.python.pydev.core.log.Log;
import org.python.pydev.plugin.PydevPlugin;
import org.python.pydev.runners.SimplePythonRunner;
import org.python.pydev.runners.SimpleRunner;

import com.aptana.shared_core.io.FileUtils;
import com.aptana.shared_core.structure.Tuple;

/**
 * @author Fabio Zadrozny
 */
public class PythonShell extends AbstractShell {

    // Jiawei Zhang add begin.
    private static PyQtTags pyQtTags = null;
    
    public static String[] getQtArgsDoc(String className, String methodName) {
        return PythonShell.pyQtTags.getQtArgsDoc(className, methodName);
    }
    // Jiawei Zhang add end.
    
    /**
     * Initialize with the default python server file.
     * 
     * @throws IOException
     * @throws CoreException
     */
    public PythonShell() throws IOException, CoreException {
        super(PydevPlugin.getScriptWithinPySrc("pycompletionserver.py"));
        
        // Jiawei Zhang add begin.
        if (PythonShell.pyQtTags == null) {
            PythonShell.pyQtTags = new PyQtTags(PydevPlugin.getScriptWithinPySrc("classInfoMap.qt"));
            PythonShell.pyQtTags.loadClassMapInfo();
        }
        // Jiawei Zhang add end.
    }

    @Override
    protected synchronized ProcessCreationInfo createServerProcess(IInterpreterInfo interpreter, int pWrite, int pRead)
            throws IOException {
        File file = new File(interpreter.getExecutableOrJar());
        if (file.exists() == false) {
            throw new RuntimeException("The interpreter location found does not exist. " + interpreter);
        }
        if (file.isDirectory() == true) {
            throw new RuntimeException("The interpreter location found is a directory. " + interpreter);
        }

        String[] parameters = SimplePythonRunner.preparePythonCallParameters(interpreter.getExecutableOrJar(),
                FileUtils.getFileAbsolutePath(serverFile), new String[] { "" + pWrite, "" + pRead });

        IInterpreterManager manager = PydevPlugin.getPythonInterpreterManager();

        String[] envp = null;
        try {
            envp = SimpleRunner.getEnvironment(null, interpreter, manager);
        } catch (CoreException e) {
            Log.log(e);
        }

        File workingDir = serverFile.getParentFile();
        process = SimpleRunner.createProcess(parameters, envp, workingDir);

        return new ProcessCreationInfo(parameters, envp, workingDir, process);
    }
    
    
    // Jiawei Zhang add begin.
    @Override
    public synchronized Tuple<String, List<String[]>> getImportCompletions(
            String str, List<String> pythonpath) throws CoreException {
        // TODO Auto-generated method stub
        Tuple<String, List<String[]>> tuple = super.getImportCompletions(str, pythonpath);
        if (str.indexOf("PyQt4") == 0) {
            List<String[]> completions = tuple.o2;
            Iterator<String[]> it = completions.iterator();
            while (it.hasNext()) {
                String[] element = it.next();
                 if(element.length >= 4){//it might be a server error
                     // element[0]: attribute of python object // element[1]: doc
                     // element[2]: args // element[3]: object type, 2 means function type.
                     // if ( element[3].equals("2") && element[1].trim().length() == 0 && element[2].equals("()")) { // This line fail to work, since PyQt 4.7 contains its own doc string.
                     if ( element[3].equals("2")) { // My doc string is better than original one, so use my own.
                         // Convert the class full name to class name such as: PyQt4.QtGui.QApplication -> QApplication or PyQt4.phonon.Phonon.AudioOutput -> Phonon.AudioOutput
                         int index = str.indexOf(".");
                         if (index == -1) break;
                         index = str.indexOf(".", index + 1);
                         if (index == -1) break;
                         String className = str.substring(index + 1);
                         
                         // Pass in the class name and method name such as: QApplication, aboutQt
                         String[] argsDoc = PythonShell.pyQtTags.getQtArgsDoc(className, element[0]);
                         if (argsDoc != null) {
                             element[1] = argsDoc[1];
                             element[2] = argsDoc[0];
                         } else {
                             // TODO: To see why the method of className below can not be found in my tags.
                             // System.out.println(className);
                             // System.out.println(element[0]);
                         }
                     }
                 }
            }
        }
        return tuple;
    }
    // Jiawei Zhang add end.


    // Jiawei Zhang add begin.
    class PyQtTags {
        
        private ClassInfoMap classInfoMap = null;

        /**
         * methodName: 'aboutQt'
         * args: '(int, bool)'
         * doc: 'Qt Signal \\n\\n QWidget QApplication.widgetAt (int x, int y) \\n\\n Detail description.'
         */
        class MethodInfoMap {
            private HashMap<String, String[]> map  = null;
            
            public MethodInfoMap() {
                this.map = new HashMap<String, String[]>();
            }

            public void setValue(String methodName, String args, String doc) {
                String[] argsDoc = new String[] {args, doc};
                this.map.put(methodName, argsDoc);
            }
            
            public String[] getValue(String methodName) {
                if (this.map.containsKey(methodName)) {
                    return this.map.get(methodName);
                } else {
                    return null;
                }
            }
        }

        /**
         * className: 'QApplication'
         * methodInfoMap: MethodInfoMap
         * parentClassNameList: ['QApplication', 'QDialog']
         */
        class ClassInfoMap {
            private HashMap<String, Object[]> map  = null;
            
            public ClassInfoMap() {
                this.map = new HashMap<String, Object[]>();
            }
            
            public void setValue(String className, MethodInfoMap methodInfoMap, String[] parentClassNameList) {
                this.map.put(className, new Object[]{methodInfoMap, parentClassNameList});
            }

            public Object[] getValue(String className) {
                if (this.map.containsKey(className)) {
                    return this.map.get(className);
                } else {
                    return null;
                }
            }
        }
        
        private File tagsFile = null;
        
        public PyQtTags(File tagsFile) {
            this.tagsFile = tagsFile;
        }

        public boolean loadClassMapInfo() {
            if (this.classInfoMap == null) {
                try {
                    this.classInfoMap = new ClassInfoMap();
                    FileReader fr = new FileReader(this.tagsFile);
                    BufferedReader br = new BufferedReader(fr);
                    String line = null;
                    StringBuffer tag_content = new StringBuffer();
                    while ((line = br.readLine()) != null) {
                        tag_content.append(line + "\n");
                    }
                    br.close();
                    String[] classInfoList = tag_content.toString().split("\\*\\*\\*\\*\\*");
                    for (String classInfo : classInfoList) {
                        if (classInfo.equals("")) continue;
                        String[] infos = classInfo.split("!!!!!");
                        if (infos.length == 1) {
                            infos = new String[]{infos[0], "", ""};
                        }
                        if (infos.length == 2) {
                            infos = new String[]{infos[0], infos[1], ""};
                        }
                        String className = infos[0];
                        String parentClassNames = infos[1];
                        String methodInfos = infos[2];
                        String[] parentClassNameList = null;
                        if (parentClassNames.equals("")) {
                            parentClassNameList = new String[]{};
                        } else {
                            parentClassNameList = parentClassNames.split(";");
                        }

                        // Set the values to methodInfoMap
                        MethodInfoMap methodInfoMap = new MethodInfoMap();
                        String[] methodInfoList = methodInfos.split("#####");
                        for (String methodInfo : methodInfoList) {
                            if (methodInfo.equals("")) continue;
                            String methodName = methodInfo.split("@@@@@")[0];
                            String args = methodInfo.split("@@@@@")[1];
                            String doc = methodInfo.split("@@@@@")[2];
                            methodInfoMap.setValue(methodName, args, doc);
                        }

                        // Set the values to classInfoMap
                        this.classInfoMap.setValue(className, methodInfoMap, parentClassNameList);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                    this.classInfoMap = null;
                }
            }
            return this.classInfoMap == null ? false : true;
        }
        
        public String[] getQtArgsDoc(String className, String methodName) {
            
            if (!this.loadClassMapInfo()) return new String[] {"(Please produce the tag file first.)", "Please produce the tag file first."};
            Object[] classInfo = this.classInfoMap.getValue(className);
            if (classInfo == null) return null;
            
            MethodInfoMap methodInfoMap = (MethodInfoMap) classInfo[0];
            String[] parentClassNameList = (String[]) classInfo[1];
            String[] methodInfo = methodInfoMap.getValue(methodName);
            if (methodInfo != null) return methodInfo;
            
            for (String tempClassName : parentClassNameList) {
                String[] argsDoc = this.getQtArgsDoc(tempClassName, methodName);
                if (argsDoc != null) return argsDoc;
            }

            return null;
        }
        
//      public static void main(String[] args) {
//          PyQtTags pyQtTags = new PyQtTags();
//          String[] argsDoc = pyQtTags.getQtArgsDoc("QWidget", "connect");
//          if (argsDoc != null) {
//              System.out.println(argsDoc[0]);
//              System.out.println(argsDoc[1]);
//          } else {
//              System.out.println("No information for this method.");
//          }
//      }
    }

    // Jiawei Zhang add end.
}
