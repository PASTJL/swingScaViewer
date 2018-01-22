<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html>
<head>
	<meta http-equiv="content-type" content="text/html; charset=utf-8"/>
	
</head>
<body lang="fr-FR" dir="ltr">
<p style="margin-top: 0.42cm; page-break-after: avoid"><font face="Albany, sans-serif"><font size="4" style="font-size: 14pt"><b>SwingScaViewer</b><br/>
==============</font></font></p>
<p><br/>
This tool transforms dated logs ( as http acces logs, Was
logs, Log4J logs, JVM GC logs ...) into csv files.<br/>
It permits to
visualize the corresponding Chart with the framework JFreeChart<br/>
<br/>
The
whole binaries are in the archive swingScaViewer.zip<br/>
<br/>
Important
:<br/>
To use this tool, you must know basics on Pattern Matching
with regular expression (Perl regex for example).<br/>
At the end of
the document, I give some examples of regex that are currently used
in swingScaViewer.<br/>
<br/>
The general mechanism used in this tool
is to parse in 2 phases :<br/>
First phase : first regex extracts
from a source that contains the interesting information in a
result<br/>
Second phase, if necessary : second regex extracts, from
precedent result, the final information<br/>
This mechanism can
handle almost all cases <br/>
<br/>
The product SwingScaViewer is a
kind of workbench that groups several tools :<br/>
parsing dated logs
( system logs, Web servers,WAS, application ...) and converting them
into csv files<br/>
visualisation of csv files, or direct
visualisation with certain types of logs ( JVM GC logs for example).
Integration of others tools like AspectPerf( packaging AspectJ LTW
Weaving for profiling Java application),<br/>
JDBC Requests,
statFilesAdvanced …<br/>
upload and download of files<br/>
Utilities
tools =&gt; date &lt;=&gt; dateInMillis, aggregation of files, regex
expression tester ...</p>
</body>
</html>
