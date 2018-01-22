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
import scala.collection.JavaConverters._
import java.io.File
import java.util.StringTokenizer
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import com.jlp.scaviewer.commons.utils.SearchDirFile
import com.jlp.scaviewer.ui.SwingScaViewer
import java.util.jar.JarFile

case class DownloadDialog(currentProject: String, prefixScenario: String) extends JDialog with Runnable {
  var jtaContent = "";
  var myPrincipalThread: Thread = null;
  var myThread: MyDownloadThread = null;
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
      if (file.isFile &&  (file.getAbsolutePath().contains(File.separator+"csv"+File.separator) || 
          file.getAbsolutePath().contains(File.separator+"reports"+File.separator) || file.getAbsolutePath().contains(File.separator+"logs"+File.separator)) && file.getAbsolutePath.contains("temp") && file.getAbsolutePath.endsWith(".jar"))
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
          if (f.exists())f.delete
          if (!f.getParentFile().exists()) f.getParentFile().mkdirs()
          val fos = new java.io.FileOutputStream(f);
          val tabByte=new Array[Byte](10240)
          while (is.available() > 0) {
            // write contents of 'is' to 'fos'
            val num=is.read(tabByte)
            fos.write(tabByte,0,num);
          }
          fos.close();
          is.close();
        case true=> if(!f.exists)f.mkdirs()
      }
	 
    }
     jarF.close()
  }
  myThread = new MyDownloadThread(this);
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
    println("General end of download. If any, un-jar jarfiles in csv folder and afterwards delete them")
     jtaContent = new StringBuilder(jtaContent).append("\n Start un-jaring eventual jar files. Please wait ... ")
            .toString(); 
    this.jspJtextArea.getVerticalScrollBar().setValue(
      this.jspJtextArea.getVerticalScrollBar().getMaximum());
    // on cherles fichiers qui sont sous csv et qui se termine par jar

    val jarFiles = SearchDirFile.recursiveListFiles(new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject)).filter(myFilter)

    // decompacter les jar files

    jarFiles.foreach { jf =>
      extract(jf.getAbsolutePath())
      jf.delete()
      SearchDirFile.deleteDir(new File(jf.getParent() + File.separator + "META-INF"))
    }

    myThread = null;
     jtaContent = new StringBuilder(jtaContent).append("\n End un-jaring eventual jar files.\n Definitive End of downloads ")
            .toString(); 
    Thread.sleep(5000)
    this.dispose();
  }

}
class MyDownloadThread(downDg: DownloadDialog) extends Thread {

  var listFiles: java.util.List[String] = new ArrayList[String]();

  override def run() {

    var deb = System.currentTimeMillis();
    downDg.jtaContent = new StringBuffer(
      downDg.jtaContent).append(
      "\n Begin  Downloading Files :\n ").toString();
    downDg.jta.setText(downDg.jtaContent);
    var nbLines = MainScaSSHconnect.tabDownLoad.length;
    var set: Set[String] = MainScaSSHconnect.hmConn.keySet();
    val prefixScenario = downDg.prefixScenario

    for (i <- 0 until nbLines) {

      // traiter ici les cas de regexp et boucler : TODO
      // traiter ici les cas de regexp et boucler : TODO
      // on ne prend en upload pur que les lignes dont le rang =0 1i�re colonne)

      if (MainScaSSHconnect.tabDownLoad(i).cmdRank == 0) {
        val patternRemoteFileInit: String = MainScaSSHconnect.tabDownLoad(i).patternFiles
        var serveurs: Array[String] = MainScaSSHconnect.tabDownLoad(i).idServer.split("\\s+");
        // pour chaque item on peut avoir des caractere joker
        var lstServ: List[String] = new ArrayList[String]();
        var ips: List[String] = new ArrayList[String]();
        //			System.out.print ("Liste initiale :");
        //			for (String srv : serveurs) {
        //				System.out.print(srv+" ");
        //			}
        //			System.out.println ();
        lstServ.clear();
        for (srv <- serveurs) {

          ips.clear();
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
            if (srv.trim().length() > 0 && set.contains(srv.trim())) lstServ.add(srv);
          }

          //				for(String srvTer:lstServ){
          //					System.out.println("srvTer="+srvTer);
          //				}
        }

        val tabActor: Array[ActorRef] = Array.ofDim(lstServ.asScala.length)
        val systemDnl: ActorSystem = ActorSystem("MySystemDnl")
        DownloadDialog.tabBool = Array.ofDim(lstServ.asScala.length)
        for (j <- 0 until lstServ.asScala.length) {
          tabActor(j) = systemDnl.actorOf(Props(new ActorDownload(j.toString)), "ActorDownload_" + j.toString)
          DownloadDialog.tabBool(j) = false
        }
        val tabDlFile: Array[DownloadFile] = Array.ofDim(lstServ.asScala.length)
        val tabMsg: Array[MessageDnl] = Array.ofDim(lstServ.asScala.length)
        var rank = 0
        val tabFiles: Array[Array[String]] = Array.ofDim(lstServ.asScala.length)
        for (srvBis <- lstServ.asScala) {

          tabDlFile(rank) = new DownloadFile(0, srvBis, MainScaSSHconnect.tabDownLoad(i).typeFileOrDir, MainScaSSHconnect.tabDownLoad(i).patternFiles,
            MainScaSSHconnect.tabDownLoad(i).howmany, MainScaSSHconnect.tabDownLoad(i).target, MainScaSSHconnect.tabDownLoad(i).compress)
          //tabDlFile(rank) = MainScaSSHconnect.tabDownLoad(i)
          tabDlFile(rank).idServer = srvBis

          tabDlFile(rank).patternFiles = patternRemoteFileInit.replaceAll("<server>", srvBis);

          listFiles.clear();
          if (tabDlFile(rank).typeFileOrDir.startsWith("Files")) {

            var withPrefix: Boolean = true;
            if (tabDlFile(rank).typeFileOrDir.equals("FilesNoPrefix")) {
              withPrefix = false;
            }

            tabFiles(rank) = returnTabFiles(tabDlFile(rank));
            if (null != tabFiles(rank)) {
              val cnx = MainScaSSHconnect.hmConn.get(srvBis)
              // val scaSSH = 

              // download(tabFilesDist, dlFile, scaSSH, withPrefix);
              // replaced by Actor

              tabMsg(rank) = new MessageDnl(tabFiles(rank), prefixScenario, tabDlFile(rank), ScaSSH(new MySSHConnection(cnx.iPAddress, cnx.iPPort, cnx.login, cnx.password)), withPrefix)

              rank += 1
            }

          } else if (tabDlFile(rank).typeFileOrDir.startsWith("Directory") && tabDlFile(rank).howmany > 0) {
            // traitement repertoire

            tabFiles(rank) = returnTabFilesFromDir(tabDlFile(rank));

            System.out.println("nb de rep to download ="
              + tabFiles.length);

            if (null != tabFiles(rank)) {

              val cnx = MainScaSSHconnect.hmConn.get(srvBis)
              val withPrefix = true

              tabMsg(rank) = new MessageDnl(tabFiles(rank), prefixScenario, tabDlFile(rank), ScaSSH(new MySSHConnection(cnx.iPAddress, cnx.iPPort, cnx.login, cnx.password)), withPrefix)

              rank += 1
            }

          } else if (tabDlFile(rank).typeFileOrDir.equals("Explicit_Cmd")) {
            tabFiles(rank) = returnTabFilesExplicit(tabDlFile(rank));
            if (null != tabFiles(rank)) {

              val withPrefix = true
              val cnx = MainScaSSHconnect.hmConn.get(srvBis)

              tabMsg(rank) = new MessageDnl(tabFiles(rank), prefixScenario, tabDlFile(rank), ScaSSH(new MySSHConnection(cnx.iPAddress, cnx.iPPort, cnx.login, cnx.password)), withPrefix)

              rank += 1

            }
          }
        }

        downDg.jtaContent = new StringBuilder(
          downDg.jtaContent).append("\nDownload " + MainScaSSHconnect.tabDownLoad(i).patternFiles + " from servers : " + lstServ.asScala.mkString("(", ", ", ")") + " to " + MainScaSSHconnect.tabDownLoad(i).target + "\n").toString
        downDg.jta.setText(downDg.jtaContent);
        var nbActors = lstServ.asScala.length
        for (i <- 0 until nbActors) {
          if (null != tabMsg(i)) {
            println("tabMsg(" + i + "):" + tabMsg(i).dlFile.patternFiles)
            tabActor(i) ! tabMsg(i)
          } else {
            tabActor(i) ! "stop"
          }

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
        
         var fut: scala.concurrent.Future[akka.actor.Terminated]=   systemDnl.terminate()
  while (!fut.isCompleted){
    
  }
       

      }
    }

    var fin = System.currentTimeMillis();

    var nbMin = (fin - deb) / (1000 * 60);
    var nbSecs = ((fin - deb) / 1000) - nbMin * 60;
    var millis = (fin - deb) - (nbSecs * 1000) - nbMin * 60 * 1000;
    var mesg = "Treated in " + nbMin + " mn " + nbSecs + " secs "
    +millis + " ms ";
    downDg.jtaContent = new StringBuffer(
      downDg.jtaContent)
      .append("\n End of  Downloading Files \n\t").append(mesg)
      .toString();
    downDg.jta.setText(downDg.jtaContent);

    this.interrupt();
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
    println("DownloadDialog ret=" + ret)
    var strTk = new StringTokenizer(ret, "\n");

    var nb = strTk.countTokens();
    // tabStr contient la liste des repertoires a downloader
    var tabStr: Array[String] = Array.ofDim(nb)
    for (i <- 0 until nb) {
      tabStr(i) = strTk.nextToken().trim();
      println("tabStr(" + i + ")=" + tabStr(i))
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

    // JLP TODO Compression du repertoire
    if (dlFile.typeFileOrDir == "Directory") {

      dlFile.howmany = 10000
      for (i <- 0 until tabStr.length) {
        //dlFile.patternFiles = tabStr(i) + "/*"

        if (!tabStr(i).endsWith("/")) tabStr(i) = tabStr(i) + "/"
        cmd = MainScaSSHconnect.lsCommand + " " + tabStr(i) + "*"
        println(" Dir cmd to recup file in rep :" + cmd)
        var tmpRet = scaSSH.use { sess => scaSSH.executeCommand(sess, cmd) }
        // println("tmpRet="+tmpRet)
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

        val cmd2 = "cd " + rootDir + " ; " + remJar + " -cf  temp" + ext2 + ".jar ./" + ext2 + "/"
        var tmpRet20 = scaSSH.use { sess => scaSSH.executeCommand(sess, cmd2) }
        println("test apres compression distante tmpret20=" + tmpRet20)
        ret2 += rootDir + "/" + "temp" + ext2 + ".jar" + "\n"
      }
    }
    scaSSH.close()
    // println("ret2="+ret2)
    // println("retour des fichiers de tous les rep :")

    var strTk2 = new StringTokenizer(ret2, "\n");
    var nb2 = strTk2.countTokens();
    var tabStr2: Array[String] = Array.ofDim(nb2)
    for (i <- 0 until nb2) {
      tabStr2(i) = strTk2.nextToken().trim();
      // println("tabStr2(" + i + ")=" + tabStr2(i))
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
    // System.out.println("returnTabFiles ret=" + ret);
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

object DownloadDialog {
  var tabBool: Array[Boolean] = null;
}