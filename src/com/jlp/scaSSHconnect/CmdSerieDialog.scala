package com.jlp.scaSSHconnect
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Font
import java.util.ArrayList
import java.util.Iterator
import java.util.List
import java.util.Set
import java.util.regex.Pattern
import javax.swing.JDialog
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JTextArea

import java.io.File
import java.util.StringTokenizer
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import scala.collection.immutable.TreeMap
import java.util.jar.JarFile
import com.jlp.scaviewer.commons.utils.SearchDirFile
import com.jlp.scaviewer.ui.SwingScaViewer
import scala.collection.JavaConverters._
case class CmdSerieDialog(currentProject: String, prefixScenario: String) extends JDialog with Runnable {
  var jtaContent = ""
  var myPrincipalThread: Thread = null;
  var myThread: MyCmdSerieThread = null;
  private var jspJtextArea: JScrollPane = null;
  var jta: JTextArea = null;
  private var jpContentPane: JPanel = null;
  setModal(false);

  this.setPreferredSize(new Dimension(900, 600));

  jpContentPane = new JPanel();
  jpContentPane.setLayout(new BorderLayout());
  this.setContentPane(jpContentPane);
  jspJtextArea = new javax.swing.JScrollPane();
  jta = new JTextArea();
  jta.setLineWrap(true);

  this.jspJtextArea.setViewportView(jta);
  jspJtextArea.setAutoscrolls(true);
  jta.setText("");
  jta.setFont(new Font("Arial", Font.BOLD, 12));
  jpContentPane.add(jspJtextArea, BorderLayout.CENTER);
  this.pack();
  this.setVisible(true);
  def myFilter(file: File): Boolean =
    {
      if (file.isFile && (file.getAbsolutePath().contains(File.separator + "csv" + File.separator) ||
        file.getAbsolutePath().contains(File.separator + "reports" + File.separator) || file.getAbsolutePath().contains(File.separator + "logs" + File.separator)) && file.getAbsolutePath.contains("temp") && file.getAbsolutePath.endsWith(".jar"))
        true
      else
        false
    }
  def extract(jf: String) = {

    import collection.JavaConverters._
    val file = new File(jf)
    val rootPar = file.getParentFile()
    val jarF = new JarFile(new File(jf))

    jarF.entries().asScala.foreach { jarEntry =>

      val f = new java.io.File(rootPar + java.io.File.separator + jarEntry.getName())
      jarEntry.isDirectory() match {
        case false =>
          val is = jarF.getInputStream(jarEntry); // get the input stream
          if (f.exists()) f.delete
          if (!f.getParentFile().exists()) f.getParentFile().mkdirs()
          val fos = new java.io.FileOutputStream(f);
          val tabByte = new Array[Byte](10240)
          while (is.available() > 0) {
            // write contents of 'is' to 'fos'
            val num = is.read(tabByte)
            fos.write(tabByte, 0, num);
          }
          fos.close();
          is.close();
        case true => if (!f.exists) f.mkdirs()
      }

    }
    jarF.close()
  }

  myThread = new MyCmdSerieThread(this);
  myPrincipalThread = new Thread(this);
  myThread.start();
  myPrincipalThread.start();
  override def run() {

    while (myThread.isAlive()) {
      try {

        if (Thread.currentThread() == myPrincipalThread) {

          jtaContent = new StringBuilder(jtaContent).append(". ")
            .toString(); ;
          jta.setText(jtaContent);
          this.jspJtextArea.getVerticalScrollBar().setValue(
            this.jspJtextArea.getVerticalScrollBar()
              .getMaximum());

        }
        Thread.sleep(1000);

      } catch {
        // TODO Auto-generated catch block
        case e1: InterruptedException =>
          e1.printStackTrace();

      }

    }
    jta.setText(jtaContent);
    println("Fin generale de serie de commandes")
    jtaContent = new StringBuilder(jtaContent).append("\n Start un-jaring eventual jar files. Please wait ... ")
      .toString();
    this.jspJtextArea.getVerticalScrollBar().setValue(
      this.jspJtextArea.getVerticalScrollBar().getMaximum());
    // on cherles fichiers qui sont sous csv et qui se termine par jar

    val jarFiles = SearchDirFile.recursiveListFiles(new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject)).filter(myFilter)

    // decompacter les jar files
    jta.setText(jtaContent);
    jarFiles.foreach { jf =>
      extract(jf.getAbsolutePath())
      jf.delete()
      SearchDirFile.deleteDir(new File(jf.getParent() + File.separator + "META-INF"))
    }
    this.jspJtextArea.getVerticalScrollBar().setValue(
      this.jspJtextArea.getVerticalScrollBar().getMaximum());
    jta.setText(jtaContent+"\nEnd of un-jaring\n");
    Thread.sleep(5000)
    myThread = null;
    this.dispose();
  }
}

class MyCmdSerieThread(cmdDg: CmdSerieDialog) extends Thread {

  var listFiles: java.util.List[String] = new ArrayList[String]();

  override def run() {

    var deb = System.currentTimeMillis();
    cmdDg.jtaContent = new StringBuffer(
      cmdDg.jtaContent).append(
      "\n Begin  series of command (Upload/Download) :\n ").toString();
    cmdDg.jta.setText(cmdDg.jtaContent);
    // remplir la treeMap avec les UplDnlFile

    var sortedMap: TreeMap[Int, UplDnlFile] = new TreeMap()
    for (up <- MainScaSSHconnect.tabUpload; if (up.cmdRank > 0)) {
      sortedMap = sortedMap + ((up.cmdRank, up))
      println("tabUpload : adding " + up.cmdRank + " servers=" + up.idServer)
    }
    for (dl <- MainScaSSHconnect.tabDownLoad; if (dl.cmdRank > 0)) {
      sortedMap = sortedMap + ((dl.cmdRank, dl))
      println("tabDownLoad : adding " + dl.cmdRank + " servers=" + dl.idServer)
    }
    var nbLines = sortedMap.size
    var set: Set[String] = MainScaSSHconnect.hmConn.keySet();
    val prefixScenario = cmdDg.prefixScenario

    sortedMap foreach { tup =>

      tup._2 match {
        case dnl: DownloadFile => {

          traiterDownload(tup._2.asInstanceOf[DownloadFile])
        }
        case upl: UploadFile => {

          traiterUpload(tup._2.asInstanceOf[UploadFile])
        }
        case uplDnl: UplDnlFile => println("i=" + tup._1 + " uplDnl indetermine")
      }

    }
    var fin = System.currentTimeMillis();

    var nbMin = (fin - deb) / (1000 * 60);
    var nbSecs = ((fin - deb) / 1000) - nbMin * 60;
    var millis = (fin - deb) - (nbSecs * 1000) - nbMin * 60 * 1000;
    var mesg = "Treated in " + nbMin + " mn " + nbSecs + " secs "
    +millis + " ms ";
    cmdDg.jtaContent = new StringBuffer(
      cmdDg.jtaContent)
      .append("\n End of  serie of commands \n\t").append(mesg)
      .toString();
    cmdDg.jta.setText(cmdDg.jtaContent);
  }

  private def traiterDownload(dlFile: DownloadFile) {
    // TO DO JLP
    println("traiterDownload")
    val lstServ: List[String] = recupererListServer(dlFile)
    val tabActor: Array[ActorRef] = Array.ofDim(lstServ.asScala.length)
    val systemDnl: ActorSystem = ActorSystem("MySystemDnl")
    DownloadDialog.tabBool = Array.ofDim(lstServ.asScala.length)
    for (j <- 0 until lstServ.asScala.length) {
      tabActor(j) = systemDnl.actorOf(Props(new ActorDownload(j.toString)), "ActorDownload_" + j.toString)
      DownloadDialog.tabBool(j) = false
    }
    val patternRemoteFileInit: String = dlFile.patternFiles
    val tabDlFile: Array[DownloadFile] = Array.ofDim(lstServ.asScala.length)
    val tabMsg: Array[MessageDnl] = Array.ofDim(lstServ.asScala.length)
    var rank = 0
    val tabFiles: Array[Array[String]] = Array.ofDim(lstServ.asScala.length)
    cmdDg.jtaContent = new StringBuilder(
      cmdDg.jtaContent).append("\nDownload " + dlFile.patternFiles + " from servers : " + lstServ.asScala.mkString("(", ", ", ")") + " to " + dlFile.target + "\n").toString
    cmdDg.jta.setText(cmdDg.jtaContent);
    for (srvBis <- lstServ.asScala) {

      tabDlFile(rank) = new DownloadFile(0, srvBis, dlFile.typeFileOrDir, dlFile.patternFiles,
        dlFile.howmany, dlFile.target, dlFile.compress)
      //tabDlFile(rank) = MainScaSSHconnect.tabDownLoad(i)
      tabDlFile(rank).idServer = srvBis

      tabDlFile(rank).patternFiles = patternRemoteFileInit.replaceAll("<server>", srvBis);

      listFiles.clear();
      if (tabDlFile(rank).typeFileOrDir.startsWith("Files")) {
        println("on traite ici")

        var withPrefix: Boolean = true;
        if (tabDlFile(rank).typeFileOrDir.equals("FilesNoPrefix")) {
          withPrefix = false;
        }
        tabFiles(rank) = returnTabFiles(tabDlFile(rank));

        val cnx = MainScaSSHconnect.hmConn.get(srvBis)
        // val scaSSH = 

        // download(tabFilesDist, dlFile, scaSSH, withPrefix);
        // replaced by Actor

        tabMsg(rank) = new MessageDnl(tabFiles(rank), cmdDg.prefixScenario, tabDlFile(rank), ScaSSH(new MySSHConnection(cnx.iPAddress, cnx.iPPort, cnx.login, cnx.password)), withPrefix)
        //  tabActor(rank) ! tabMsg(rank)
        rank += 1

      } else if (tabDlFile(rank).typeFileOrDir.startsWith("Directory") && tabDlFile(rank).howmany > 0) {
        // traitement repertoire

        println("traitement par repertoire")
        tabFiles(rank) = returnTabFilesFromDir(tabDlFile(rank));

        if (null != tabFiles(rank)) {

          val cnx = MainScaSSHconnect.hmConn.get(srvBis)
          val withPrefix = true

          tabMsg(rank) = new MessageDnl(tabFiles(rank), cmdDg.prefixScenario, tabDlFile(rank), ScaSSH(new MySSHConnection(cnx.iPAddress, cnx.iPPort, cnx.login, cnx.password)), withPrefix)

          rank += 1
        }

      } else if (tabDlFile(rank).typeFileOrDir.equals("Explicit_Cmd")) {
        tabFiles(rank) = returnTabFilesExplicit(tabDlFile(rank));
        if (null != tabFiles(rank)) {

          val withPrefix = true
          val cnx = MainScaSSHconnect.hmConn.get(srvBis)

          tabMsg(rank) = new MessageDnl(tabFiles(rank), cmdDg.prefixScenario, tabDlFile(rank), ScaSSH(new MySSHConnection(cnx.iPAddress, cnx.iPPort, cnx.login, cnx.password)), withPrefix)

          rank += 1

        }
      }

    }

    var nbActors = lstServ.asScala.length
    println("CmdSerieCommand nbActor=" + nbActors);
    for (i <- 0 until nbActors) {
      println("tabMsg(" + i + "):" + tabMsg(i).dlFile.patternFiles)
      tabActor(i) ! tabMsg(i)

    }

    //atendre la fin des actors
    var boolStopGen = false;
    while (boolStopGen == false) {
      // On attend la fin de tous les thraeds
      boolStopGen = true;
      try {
        // //
        for (k <- 0 until nbActors) {
          // //
          boolStopGen = boolStopGen && DownloadDialog.tabBool(k)
          // //
        }
        // //
        // println("lancerParsing coucou 2 boolStopGen=" + boolStopGen)

        Thread.sleep(1000);

      } catch { case e: InterruptedException => e.printStackTrace() }

    }
    System.out.println("Avant fin download");
  var fut: scala.concurrent.Future[akka.actor.Terminated]=   systemDnl.terminate()
  while (!fut.isCompleted){
    
  }
    System.out.println("Apres fin download");

  }
  private def traiterUpload(upFile1: UploadFile) {
    // TO DO JLP
    println("traiterUpload")
    val lstServ: List[String] = recupererListServer(upFile1)
    println("lstServ recupere =" + lstServ.asScala.mkString("(", " , ", ")"))
    var patternRemoteFileInit: String = upFile1.remoteFile;
    val cmdScp = MainScaSSHconnect.scpCommand + " -p -t "
    var ips: java.util.List[String] = new ArrayList[String]();

    var tabActor: Array[ActorRef] = Array.ofDim(lstServ.asScala.length)
    val systemUpl: ActorSystem = ActorSystem("MySystem")
    UploadDialog.tabBool = Array.ofDim(lstServ.asScala.length)
    for (j <- 0 until lstServ.asScala.length) {
      tabActor(j) = systemUpl.actorOf(Props(new ActorUpload(j)), "ActorUpload_" + j)
      UploadDialog.tabBool(j) = false
    }
    var rank = 0
    cmdDg.jtaContent = new StringBuilder(
      cmdDg.jtaContent).append("\nUpload " + upFile1.localFile + " to servers  : " + lstServ.asScala.mkString("(", ", ", ")") + " at " + upFile1.remoteFile + "\n").toString
    cmdDg.jta.setText(cmdDg.jtaContent);
    for (srvBis <- lstServ.asScala) {

      var patternRemoteFile: String = patternRemoteFileInit.replaceAll("<server>", srvBis);

      var upFile: UploadFile = new UploadFile(upFile1.cmdRank, upFile1.idServer,
        upFile1.localFile, upFile1.remoteFile, upFile1.execute)
      upFile.remoteFile = patternRemoteFile
      upFile.idServer = srvBis
      // cas ou le dernier caractere est / ou \
      var car: String = upFile.remoteFile.substring(
        upFile.remoteFile.length() - 1);
      var repDist = "";
      var fileOnlyRemote = "";

      if (car.equalsIgnoreCase("/") || car.equalsIgnoreCase("\\")) {
        repDist = upFile.remoteFile
        if (upFile.localFile.contains("/")) {
          fileOnlyRemote = upFile.localFile.substring(
            upFile.localFile.lastIndexOf("/") + 1);
        }
        if (upFile.localFile.contains("\\")) {
          fileOnlyRemote = upFile.localFile.substring(
            upFile.localFile.lastIndexOf("\\") + 1);
        }

      } else {
        if (upFile.remoteFile.contains("/")) {
          repDist = upFile.remoteFile.substring(0,
            upFile.remoteFile.lastIndexOf("/"));
          fileOnlyRemote = upFile.remoteFile.substring(
            upFile.remoteFile.lastIndexOf(
              "/") + 1);
        }
        if (upFile.remoteFile.contains("\\")) {
          repDist = upFile.remoteFile.substring(0,
            upFile.remoteFile.lastIndexOf("\\"));
          fileOnlyRemote = upFile.remoteFile.substring(
            upFile.remoteFile.lastIndexOf(
              "\\") + 1);
        }

      }

      // creation de la connexion scaSSH
      //
      val cnx = MainScaSSHconnect.hmConn.get(srvBis)
      val cmd = "cd " + repDist + ";pwd"

      var scaSSH = ScaSSH(new MySSHConnection(cnx.iPAddress, cnx.iPPort, cnx.login, cnx.password))
      var rep = scaSSH.use { sess => scaSSH.executeCommand(sess, cmd) }.replaceAll("\\s", "")
      //          String rep = ExecCommand.executeCommand(upFile,
      //            "cd " + repDist + ";pwd").replaceAll("\\n", "");

      if (repDist.indexOf(rep) >= 0) {
        //              upDg.jtaContent = new StringBuilder(
        //                upDg.jtaContent)
        //                .append("\n OK  Remote  Directory  found at :")
        //                .append(repDist)
        //                .append("\n Beginning uploads\n").toString();
        //              upDg.jta.setText(upDg.jtaContent);
        var rfile1 = "";
        var car = upFile.remoteFile.substring(patternRemoteFile.length() - 1);

        var fileOnlyRemote = "";
        if (car.equalsIgnoreCase("/") || car.equalsIgnoreCase("\\")) {
          repDist = upFile.remoteFile;
          if (upFile.localFile.contains("/")) {
            fileOnlyRemote = upFile.localFile.substring(
              upFile.localFile.lastIndexOf("/") + 1);
          }
          if (upFile.localFile.contains("\\")) {
            fileOnlyRemote = upFile.localFile.substring(
              upFile.localFile.lastIndexOf("\\") + 1);
          }
          rfile1 = repDist + fileOnlyRemote;

        } else {
          rfile1 = upFile.remoteFile

          if (upFile.remoteFile.contains("/")) {
            repDist = upFile.remoteFile.substring(0,
              upFile.remoteFile.lastIndexOf("/"));
            fileOnlyRemote = upFile.remoteFile.substring(
              upFile.remoteFile.lastIndexOf("/") + 1);
          }
          if (upFile.remoteFile.contains("\\")) {
            repDist = upFile.remoteFile.substring(0,
              upFile.remoteFile.lastIndexOf("\\"));
            fileOnlyRemote = upFile.remoteFile.substring(
              upFile.remoteFile.lastIndexOf("\\") + 1);
          }
        }

        //            scaSSH.use { sess =>
        //              scaSSH.upload(sess, upFile.localFile, rfile1, cmdScp)
        //              scaSSH.close(scaSSH.getSession)
        //            }

        tabActor(rank) ! new MessageUpl(scaSSH, upFile, repDist, rfile1, cmdScp)
        rank += 1
        //  ScpTo.scpTo(upFile);

      } else {
        cmdDg.jtaContent = new StringBuilder(
          cmdDg.jtaContent)
          .append("\n Error  Remote  Directory not found at :")
          .append(repDist).toString();
        cmdDg.jta.setText(cmdDg.jtaContent);
      }

    }

    var nbActors = lstServ.asScala.length
    //atendre la fin des actors
    var boolStopGen = false;
    while (boolStopGen == false) {
      // On attend la fin de tous les thraeds
      boolStopGen = true;
      try {
        // //
        for (k <- 0 until nbActors) {
          // //
          boolStopGen = boolStopGen && UploadDialog.tabBool(k)
          // //
        }
        // //
        // println("lancerParsing coucou 2 boolStopGen=" + boolStopGen)

        Thread.sleep(1000);

      } catch { case e: InterruptedException => e.printStackTrace() }

    }
      var fut: scala.concurrent.Future[akka.actor.Terminated]=   systemUpl.terminate()
  while (!fut.isCompleted){
    
  }
  

  }

  private def recupererListServer(dlUp: UplDnlFile): List[String] =
    {
      var lstServ: List[String] = new ArrayList()
      var serveurs: Array[String] = null
      dlUp match {
        case dnl: DownloadFile => {
          serveurs = dnl.idServer.split("\\s")
        }

        case upl: UploadFile => {
          serveurs = upl.idServer.split("\\s")

        }
        case uplDnl: UplDnlFile =>
      }
      var set: Set[String] = MainScaSSHconnect.hmConn.keySet();
      var ips: List[String] = new ArrayList[String]();
      //			System.out.print ("Liste initiale :");
      //			for (String srv : serveurs) {
      //				System.out.print(srv+" ");
      //			}
      //			System.out.println ();
      lstServ.clear();
      println("serveurs =" + serveurs.mkString("(", " , ", ")"))
      for (srv <- serveurs) {

        if (srv.toLowerCase().equals("allip")) {
          // One prend que les IP differents pour eviter les doublons
          for (key <- set.asScala) {
            var ip = MainScaSSHconnect.hmConn.get(key).iPAddress;
            if (!ips.contains(ip)) {
              if (key.trim().length() > 0) lstServ.add(key);
              ips.add(ip);
            }
          }
        } else if (srv.equals("*")) {

          // On prend tous les IDServer Doublons IP possibles
          for (key <- set.asScala) {
            if (key.trim().length() > 0) lstServ.add(key);
          }

        } else if (srv.trim().length() > 1 && srv.contains("*")) {
          // on replace chaque * par la regexp .*

          var len = srv.length();
          var newSrv = "";
          for (j <- 0 until len) {
            var ch: Char = srv.charAt(j);
            if (ch != '*') {
              newSrv += ch;
            } else {
              newSrv += ".*";
            }
          }
          var pat: Pattern = Pattern.compile(newSrv);
          //System.out.println("passage par pattern pat="+pat.toString());
          for (key <- set.asScala) {
            if (pat.matcher(key).find()) {
              lstServ.add(key);
            }
          }

        } else {
          if (srv.trim().length() > 0 && set.contains(srv.trim())) lstServ.add(srv)
        }

        //				for(String srvTer:lstServ){
        //					System.out.println("srvTer="+srvTer);
        //				}
      }

      lstServ
    }
  private def returnTabFilesExplicit(dlFile: DownloadFile): Array[String] = {
    if (dlFile.howmany == 0) {
      return null;
    }
    val cnx = MainScaSSHconnect.hmConn.get(dlFile.idServer)
    val scaSSH = ScaSSH(new MySSHConnection(cnx.iPAddress, cnx.iPPort, cnx.login, cnx.password))

    var ret = scaSSH.use { sess => scaSSH.executeCommand(sess, dlFile.patternFiles) }.replaceAll("\n", " ");
    scaSSH.close()

    //var  ret = new Exec().executeExplicitCommand(dlFile);
    val strTk = new StringTokenizer(ret, " ");
    var tabStr: Array[String] = null;
    var nb = strTk.countTokens();
    tabStr = Array.ofDim(nb)
    for (i <- 0 until nb) {
      tabStr(i) = strTk.nextToken().trim();
    }

    return tabStr;
  }

  private def returnTabFilesFromDir(dlFile: DownloadFile): Array[String] = {
    if (dlFile.howmany == 0) {
      return null;
    }

    val cnx = MainScaSSHconnect.hmConn.get(dlFile.idServer)
    val scaSSH = ScaSSH(new MySSHConnection(cnx.iPAddress, cnx.iPPort, cnx.login, cnx.password))
    var cmd = MainScaSSHconnect.lsCommand + " -d " + dlFile.patternFiles + " | head -n" + dlFile.howmany
    var ret = scaSSH.use { sess => scaSSH.executeCommand(sess, cmd) };

    //String ret = new Exec().executeCommand(dlFile, false);

    if (ret.length() < 1) {
      return null;
    }
    var strTk = new StringTokenizer(ret, "\n");

    var nb = strTk.countTokens();
    var tabStr: Array[String] = Array.ofDim(nb)
    for (i <- 0 until nb) {
      tabStr(i) = strTk.nextToken().trim();

    }

    var ret2 = "";
    var remJar = ""
    if (dlFile.typeFileOrDir == "Directory.jar") {
      // recherche d un jar pour compresser en zip
      cmd = "find / -name jar 2>/dev/null | grep /bin/jar | head -1"
      var ret3 = scaSSH.use { sess => scaSSH.executeCommand(sess, cmd) }.trim;
      if (null != ret3 && ret3.endsWith("/bin/jar")) {
        remJar = ret3
      } else {
        // bin/jar not found forcing classic download of directory
        dlFile.typeFileOrDir = "Directory"
      }
    }
    if (dlFile.typeFileOrDir == "Directory") {
      dlFile.howmany = 10000
      for (i <- 0 until tabStr.length) {
        //dlFile.patternFiles = tabStr(i) + "/*"
        if (!tabStr(i).endsWith("/")) tabStr(i) = tabStr(i) + "/"
        cmd = MainScaSSHconnect.lsCommand + " " + tabStr(i) + "*"
        println("cmd to recup file in rep :" + cmd)
        var tmpRet = scaSSH.use { sess => scaSSH.executeCommand(sess, cmd) } + "\n"
        //var  tmpRet = new Exec().executeCommand(dlFile, true) + "\n";
        var strTk2 = new StringTokenizer(tmpRet, "\n");
        var nb2 = strTk2.countTokens();
        for (j <- 0 until nb2) {
          var tmpStr = strTk2.nextToken();
          if (!tmpStr.trim().endsWith(":") && tmpStr.contains(tabStr(i))) {
            ret2 += tmpStr + "\n";
          }

        }

      }
    } else {

      // JLP TODO Compression des repertoires
      for (i <- 0 until tabStr.length) {
        if (!tabStr(i).endsWith("/")) tabStr(i) = tabStr(i) + "/"
        // trouver le nom du dernier repertoire
        val reg = """/[^/]+/$""".r
        val reg2 = """[^/]+""".r
        val ext1 = reg.findFirstIn(tabStr(i)).get
        val ext2 = reg2.findFirstIn(ext1).get
        val rootDir = tabStr(i).substring(0, tabStr(i).indexOf(ext1))

        //val cmd2 = "cd " + rootDir + " ; " + remJar + " -cf  temp" + ext2 + ".jar ./" + ext2 + "/*.*"
        val cmd2 = "cd " + rootDir + " ; " + remJar + " -cf  temp" + ext2 + ".jar ./" + ext2 + "/"
        var tmpRet20 = scaSSH.use { sess => scaSSH.executeCommand(sess, cmd2) }
        println("test apres compression distante tmpret20=" + tmpRet20)
        ret2 += rootDir + "/" + "temp" + ext2 + ".jar" + "\n"
      }

    }
    scaSSH.close()
    var strTk2 = new StringTokenizer(ret2, "\n");
    var nb2 = strTk2.countTokens();
    var tabStr2: Array[String] = Array.ofDim(nb2)
    for (i <- 0 until nb2) {
      tabStr2(i) = strTk2.nextToken().trim();

    }
    return tabStr2;

  }

  def returnTabFiles(dlFile: DownloadFile): Array[String] = {
    if (dlFile.howmany == 0) {
      return null;
    }

    val cnx = MainScaSSHconnect.hmConn.get(dlFile.idServer)
    val scaSSH = ScaSSH(new MySSHConnection(cnx.iPAddress, cnx.iPPort, cnx.login, cnx.password))
    var cmd = MainScaSSHconnect.lsCommand + " " + dlFile.patternFiles + " | head -n" + dlFile.howmany
    println("lsCmd=" + cmd)
    var ret = scaSSH.use { sess => scaSSH.executeCommand(sess, cmd) };
    //String ret = new Exec().executeCommand(dlFile);
    System.out.println("returnTabFiles ret=" + ret);
    if (ret.length() < 1) {
      scaSSH.close()
      return null;
    }
    var strTk = new StringTokenizer(ret, "\n");
    System.out.println("returnTabFiles strTk.countTokens="
      + strTk.countTokens());
    var nb = strTk.countTokens();
    var tabStr: Array[String] = Array.ofDim(nb)
    for (i <- 0 until nb) {
      tabStr(i) = strTk.nextToken().trim();
      // System.out.println("tabStr[" + i + "] = " + tabStr[i].trim());
    }
    scaSSH.close()
    return tabStr;
  }

}

object CmdSerieDialog {
  var tabBool: Array[Boolean] = null;
}