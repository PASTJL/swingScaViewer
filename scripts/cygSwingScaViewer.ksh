root=/opt/swingScaViewer
workspace=/opt/swingScaViewer/workspace
CLASSPATH=$root/swingScaViewer.jar
 CLASSPATH=$CLASSPATH:$root/libExt/jcommon-1.0.17.jar
 CLASSPATH=$CLASSPATH:$root/libExt/jfreechart-1.0.14.jar
 CLASSPATH=$CLASSPATH:$root/libExt/scala-dbc.jar
 CLASSPATH=$CLASSPATH:$root/libExt/scala-library.jar
 CLASSPATH=$CLASSPATH:$root/libExt/scala-swing.jar
 CLASSPATH=$CLASSPATH:$root/libExt/commons-math-1.2.jar
 CLASSPATH=$CLASSPATH:$root/libExt/jlpApis.jar
 CLASSPATH=$CLASSPATH:$root/libExt/jsch-0.1.46.jar
 CLASSPATH=$CLASSPATH:$root/libExt/jtds-1.2.2.jar
 CLASSPATH=$CLASSPATH:$root/libExt/myaspectjweaver.jar
 CLASSPATH=$CLASSPATH:$root/libExt/mysql-connector-java-5.1.7-bin.jar
 CLASSPATH=$CLASSPATH:$root/libExt/ojdbc14.jar
 CLASSPATH=$CLASSPATH:$root/libExt/postgresql-8.3-604.jdbc3.jar
 CLASSPATH=$CLASSPATH:$root/libExt/springmyaspectjweaver.jar
 CLASSPATH=$CLASSPATH:$root/libExt/akka-actor-2.0.5.jar
 CLASSPATH=$CLASSPATH:$root/libExt/config-1.0.0-SNAPSHOT.jar
 CLASSPATH=$CLASSPATH:$root/config
 CLASSPATH=$CLASSPATH:$root/myPlugins
 CLASSPATH=$CLASSPATH:$root/myPlugins/myPlugins.jar
 GC_OPTS="-server -Dsun.java2d.d3d=false -Xms1024M -Xmx1024M -XX:NewRatio=2 -XX:+UseParallelGC -XX:MaxPermSize=512M -XX:PermSize=512M -XX:-UseAdaptiveSizePolicy "
 GC_LOGS="-verbose:gc  -XX:+PrintGC -XX:+PrintGCDetails -XX:+PrintGCDateStamps -XX:+PrintGCTimeStamps -Xloggc:$root/logs/GC.logs "

java $GC_OPTS `cygpath -wp $CLASSPATH`  -Dworkspace=$workspace -Droot=$root  -Dconfig.file=$root/config/scaViewer.properties  com.jlp.scaviewer.ui.SwingScaViewer