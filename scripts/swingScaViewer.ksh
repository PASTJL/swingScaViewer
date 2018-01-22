#!/bin/bash
root=/opt/swingScaViewer
workspace=/opt/workspaceLP
CLASSPATH=$root/swingScaViewer.jar
 CLASSPATH=$CLASSPATH:$root/libExt/jcommon-1.0.20.jar
 CLASSPATH=$CLASSPATH:$root/libExt/jfreechart-1.0.16.jar
 CLASSPATH=$CLASSPATH:$root/libExt/scala-library.jar
 CLASSPATH=$CLASSPATH:$root/libExt/scala-swing_2.12-2.0.0.jar
 CLASSPATH=$CLASSPATH:$root/libExt/scala-actors.jar
 CLASSPATH=$CLASSPATH:$root/libExt/commons-math-1.2.jar
 CLASSPATH=$CLASSPATH:$root/libExt/jlpApis.jar
 CLASSPATH=$CLASSPATH:$root/libExt/jsch-0.1.54.jar
 CLASSPATH=$CLASSPATH:$root/libExt/jtds-1.2.2.jar
 CLASSPATH=$CLASSPATH:$root/libExt/myaspectjweaver.jar
 CLASSPATH=$CLASSPATH:$root/libExt/mysql-connector-java-5.1.7-bin.jar
 CLASSPATH=$CLASSPATH:$root/libExt/ojdbc14.jar
 CLASSPATH=$CLASSPATH:$root/libExt/postgresql-8.3-604.jdbc3.jar
 CLASSPATH=$CLASSPATH:$root/libExt/springmyaspectjweaver.jar
 CLASSPATH=$CLASSPATH:$root/libExt/akka-actor_2.12-2.4.20.jar
 CLASSPATH=$CLASSPATH:$root/libExt/config-1.2.1.jar
 CLASSPATH=$CLASSPATH:$root/config
 CLASSPATH=$CLASSPATH:$root/myPlugins
 CLASSPATH=$CLASSPATH:$root/myPlugins/myPlugins.jar
 #GC_OPTS="-DsuppressSwingDropSupport=true -Dsun.java2d.d3d=false -Xms1024M -Xmx1024M -XX:NewRatio=2 -XX:+UseParallelGC -XX:MaxPermSize=512M -XX:PermSize=512M -XX:-UseAdaptiveSizePolicy "
 GC_OPTS=" -Dsun.java2d.d3d=false -Xms1524M -Xmx1524M -XX:NewRatio=2 -XX:+UseParallelGC  -XX:-UseAdaptiveSizePolicy "	
 GC_LOGS="-verbose:gc  -XX:+PrintGC -XX:+PrintGCDetails -XX:+PrintGCDateStamps -XX:+PrintGCTimeStamps -Xloggc:$root/logs/GC.logs "

java $GC_OPTS -classpath $CLASSPATH  -Dworkspace=$workspace -Droot=$root  -Dconfig.file=$root/config/scaViewer.properties  com.jlp.scaviewer.ui.SwingScaViewer
