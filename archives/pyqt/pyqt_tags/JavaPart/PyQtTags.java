import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.HashMap;


public class PyQtTags {
	
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
	
	public static void main(String[] args) {
		File tagsFile = new File("classInfoMap.qt");
		PyQtTags pyQtTags = new PyQtTags(tagsFile);
		String[] argsDoc = pyQtTags.getQtArgsDoc("QWidget", "widthMM");
		if (argsDoc != null) {
			System.out.println(argsDoc[0]);
			System.out.println(argsDoc[1]);
		} else {
			System.out.println("No information for this method.");
		}
	}
}
