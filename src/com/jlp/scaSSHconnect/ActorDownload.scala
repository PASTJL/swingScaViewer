package com.jlp.scaSSHconnect
import akka.actor.Actor
import java.io.File
import com.jlp.scaviewer.ui.SwingScaViewer
class ActorDownload(rank: String) extends Actor {
  def receive = {

    case MessageDnl(tabFilesDistants, prefixScenario, dlFile, scaSSH, withPrefix) =>

      download(tabFilesDistants, prefixScenario, dlFile, scaSSH, withPrefix)
      scaSSH.close()
      self ! "stop"
    case "stop" =>
      DownloadDialog.tabBool(rank.toInt) = true
      context.stop(self)
  }

  def download(tabFilesDistants: Array[String], prefixScenario: String, dlFile: DownloadFile, scaSSH: ScaSSH,
    withPrefix: Boolean) {

    var len2 = tabFilesDistants.length;
    var fullNameLocal: Array[String] = Array.ofDim(len2)

    // System.out.println("debut download() listFiles.toArray()  "
    // + listFiles.toArray().length);
    // System.out.println("debut download() withPrefix  " + withPrefix);
    var rootProjet: File = new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject)
    // all the directories must begin bty tir
    // determiner le plus recent
    var files: Array[File] = rootProjet.listFiles();
    var mostYoung: File = null;
    var dateMostYoung = 0L
    for (i <- 0 until files.length) {
      var tmpFile: File = files(i)
      if (tmpFile.isDirectory()
        && (tmpFile.getName()).toUpperCase().startsWith(
          prefixScenario.toUpperCase())) {
        var date: Long = tmpFile.lastModified();
        if (date > dateMostYoung) {
          mostYoung = tmpFile;
          dateMostYoung = date;
        }
      }
    }

    var prefix: String = new StringBuilder().append(mostYoung.getAbsolutePath())
      .append(File.separator).append(dlFile.target)
      .append(File.separator).toString();
    if (withPrefix) {
      prefix += dlFile.idServer + "_";
    }

    if (dlFile.typeFileOrDir.startsWith("Files")
      || dlFile.typeFileOrDir.equals("Explicit_Cmd")) {

      for (k <- 0 until len2) {
        var index: Int = tabFilesDistants(k).lastIndexOf('/');
        var nameOnlyTmp: String = tabFilesDistants(k).substring(index + 1);
        fullNameLocal(k) = new StringBuilder().append(prefix)
          .append(nameOnlyTmp).toString();
      }

      val compress = {
        if (dlFile.compress.toUpperCase() == "COMPRESS") { true }
        else false
      }
      if (tabFilesDistants.length > 0) {

        for (k <- 0 until len2) {
          scaSSH.use { sess =>

            scaSSH.download(sess, fullNameLocal(k), tabFilesDistants(k), compress, MainScaSSHconnect.scpCommand + " -f ")
            if (dlFile.compress.toUpperCase() == "DELETE" && (tabFilesDistants(k).endsWith(".gz") || tabFilesDistants(k).endsWith(".jar"))) {
              scaSSH.executeCommand(sess, "rm -f " + tabFilesDistants(k))
            }
          }
        }
      }

    } else {
      // traitement directory

      for (k <- 0 until len2) {

        var index = tabFilesDistants(k).lastIndexOf('/');
        var nameOnly = tabFilesDistants(k).substring(index + 1);

        var indexAvantDernier = tabFilesDistants(k).substring(0, index).lastIndexOf('/');
        var prefix2 = tabFilesDistants(k).substring(0, index).substring(indexAvantDernier + 1);
        fullNameLocal(k) = mostYoung.getAbsolutePath() + File.separator +
          dlFile.target + File.separator +
          dlFile.idServer + "_" + prefix2 + File.separator +
          nameOnly;

        var f: File = new File(mostYoung.getAbsolutePath() + File.separator +
          dlFile.target + File.separator +
          dlFile.idServer + "_" + prefix2);

        if (!f.exists()) {

          f.mkdir();
        }

      }

      val compress = {
        if (dlFile.compress.toUpperCase() == "COMPRESS") { true }
        else false
      }

      if (tabFilesDistants.length > 0) {

        for (k <- 0 until len2) {
          scaSSH.use { sess =>
          println("tabFilesDistants(k)="+tabFilesDistants(k))
            scaSSH.download(sess, fullNameLocal(k), tabFilesDistants(k), compress, MainScaSSHconnect.scpCommand + " -f ")
            if (dlFile.compress.toUpperCase() == "DELETE" && (tabFilesDistants(k).endsWith(".gz") || tabFilesDistants(k).endsWith(".jar"))) {
             println("deleting :"+tabFilesDistants(k))
              scaSSH.executeCommand(sess, "rm -f " + tabFilesDistants(k))
            }
          }
        }

      }

    }

  }

}