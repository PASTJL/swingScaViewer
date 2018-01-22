set root=C:\opt\swingScaViewer
set workspace=C:\opt\workspaceLP
set CLASSPATH=%root%\swingScaViewer.jar

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
Set GC_OPTS=-Xms1524M -Xmx1524M -XX:NewRatio=2 -XX:+UseParallelGC -XX:-UseAdaptiveSizePolicy
Set GC_LOGS=-verbose:gc  -XX:+PrintGC -XX:+PrintGCDetails -XX:+PrintGCDateStamps -XX:+PrintGCTimeStamps -Xloggc:%root%\logs\GC.logs 

java %GC_OPTS% -classpath %CLASSPATH% -Dsun.lang.ClassLoader.allowArraySyntax=true  -Dworkspace=%workspace% -Droot=%root%  -Dconfig.file=%root%\config\scaViewer.properties com.jlp.scaviewer.ui.SwingScaViewer
