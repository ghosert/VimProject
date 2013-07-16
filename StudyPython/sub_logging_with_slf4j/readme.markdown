# SLF4J Migrator

This is a Python script which allows you to migrate Jakarta Commons Logging & Log4j in your Java source code to SLF4J Logging.

## What will be changed after running this migrator ?

**1. "import" clause will be replaced with SLF4J**

```java
-import org.apache.log4j.Logger;
+import org.slf4j.Logger;
+import org.slf4j.LoggerFactory;
```

**2. log instance will be created with SLF4J**

```java
-private final static Logger log = Logger.getLogger(StubhubUserSessionFilter.class);
+private final static Logger log = LoggerFactory.getLogger(StubhubUserSessionFilter.class);
```

**3. log clause will be standard SLF4J placeholder based instead of string conjunction**

```java
-log.info("UserName="+username+" had login attempted within " + delaySeconds + " seconds by same IP="+ipAddress+" address combination");
+log.info("UserName={} had login attempted within {} seconds by same IP={} address combination", username, delaySeconds, ipAddress);
```
**4. Remove LogSF in Log4j**
```java
-LogSF.debug(log, "Input displayFileName={} originalFileName={}", uiTicTemp.getDisplayName(), uiTicTemp.getOriginalFileName());
+log.debug("Input displayFileName={} originalFileName={}", uiTicTemp.getDisplayName(), uiTicTemp.getOriginalFileName());
```

## How to use this Migrator ?

### 1. Run the demo
```shell
python refine_logging.py
```
This will make the changes over all the sample java files in the current working folder. You could diff the changes by running
```shell
git diff
```
If you want to revert the java changes, running:
```shell
git reset --hard
```

### 2. Run the migrator in your java code base
```shell
cp refine_logging.py YOUR_JAVA_SRC_PATH
python refine_logging.py
```
The migrator will recursively scan and change your java files under YOUR_JAVA_SRC_PATH, take attention on the output of the script, it will prompt issues/infos if any.

