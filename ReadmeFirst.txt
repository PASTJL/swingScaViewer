This tool transforms dated logs ( as http acces logs, Was logs, Log4J logs, JVM GC logs ...) into csv files.
It permits to visualize the corresponding Chart with the framework JFreeChart

Important :
To use this tool, you must know basics on Pattern Matching with regular expression (Perl regex for example). At the end of the document, I give some examples of regex that are currently used in swingScaViewer. 
The general mechanism used in this tool is to parse in 2 phases :
First phase : first regex extracts from a source that contains the interesting information in a result
Second  phase, if necessary : second regex extracts, from precedent result, the final information
This mechanism can handle almost all cases 

The product SwingScaViewer is a kind of workbench that groups several tools :
parsing dated  logs ( system logs, Web servers,WAS, application ...) and converting them into csv files
visualisation of csv files, or direct visualisation with certain types of logs ( JVM GC logs for example).
Integration of others tools like   AspectPerf( packaging AspectJ LTW Weaving for profiling Java application), JDBC Requests, statFilesAdvanced …
upload and download of files
Utilities tools => date <=> dateInMillis, aggregation of files, regex expression tester ...

Below for the impatients the rapid installation :
2.1 Packaging
Le packaging is done in a form of zip file  SwingScaViewer.zip which the root is swingScaViewer .
2.2 Installation  of  SwingScaViewer
2.2.1 Requirements
A Sun  JDK 1.6.0+ must be installed on your desktop.
2.2.2 Create a deployment directory
For all the document, we suppose that you use a Windows System ( there is no difficulty to adapt for a Linux box) and that the installation directory is c:\opt .
It is not mandatory to create a directory swingScaViewer.
2.2.3 De-compaction
After having downloaded  SwingScaViewer.zip    in  c:\opt  le de-compact-it in this directory.
2.2.4 Configuration
The  configuration is set in the start script of SwingScaViewer :
File c:\opt\swingScaViewer\scripts\swingScaViewer.cmd
set root=C:\opt\swingScaViewer
set workspace=C:\opt\workspaceLP
set CLASSPATH=%root%\scaViewer.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/jcommon-1.0.20.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/jfreechart-1.0.16.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/scala-library.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/scala-swing_2.12-2.0.0.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/scala-actors.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/commons-math-1.2.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/jlpApis.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/jsch-0.1.54.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/jtds-1.2.2.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/myaspectjweaver.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/mysql-connector-java-5.1.7-bin.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/ojdbc14.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/postgresql-8.3-604.jdbc3.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/springmyaspectjweaver.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/akka-actor_2.12-2.4.20.jar
set  CLASSPATH=%CLASSPATH%;%root%/libExt/config-1.2.1.jar
set  CLASSPATH=%CLASSPATH%;%root%/config
set  CLASSPATH=%CLASSPATH%;%root%/myPlugins
set  CLASSPATH=%CLASSPATH%;%root%/myPlugins/myPlugins.jar
Set GC_OPTS=-server -Xms1024M -Xmx1024M -XX:NewRatio=2 -XX:+UseParallelGC -XX:MaxPermSize=512M -XX:PermSize=512M -XX:-UseAdaptiveSizePolicy
REM Set GC_LOGS=-verbose:gc  -XX:+PrintGC -XX:+PrintGCDetails -XX:+PrintGCDateStamps -XX:+PrintGCTimeStamps -Xloggc:%root%\logs\GC.logs 

java %GC_OPTS% %GC_LOGS% -Dworkspace=%workspace% -Droot=%root%  -Dconfig.file=%root%\config\scaViewer.properties -Xms1024M -Xmx1024M com.jlp.scaviewer.ui.SwingScaViewer



At the beginning of the file ( in bold characters), 2 environment variables must be set, according to your installation.
The Window Environment Variable PATH has to make reachable the executable java of the JVM.
Important : The manual creation of the directory pointed by %workspace%  is mandatory.
We can after create a link Windows on the desktop to launch swingScaViewer by a click on the mouse.
