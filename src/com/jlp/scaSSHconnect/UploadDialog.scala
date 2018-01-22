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
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props

class UploadDialog(currentProjct: String) extends JDialog with Runnable {
  var jtaContent = "";
  var myPrincipalThread: Thread = null;
  var myThread: MyUploadThread = null;
  private var jspJtextArea: JScrollPane = null;
  var jta: JTextArea = null;
  private var jpContentPane: JPanel = null;
  var boolSpring: Boolean = false;
  var boolJmxAspectsOnly: Boolean = false;
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
  jta.setText("");
  myThread = new MyUploadThread(this);
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
    println("Fin generale des Uploads")
    this.jspJtextArea.getVerticalScrollBar().setValue(
      this.jspJtextArea.getVerticalScrollBar().getMaximum());

    myThread = null;
    Thread.sleep(5000)
    this.dispose();

  }

}
class MyUploadThread(upDg: UploadDialog) extends Thread {

  override def run() {

    var deb: Long = System.currentTimeMillis();

    upDg.jtaContent = new StringBuffer(upDg.jtaContent)
      .append("\n Begin  Uploading Files :\n ").toString();
    upDg.jta.setText(upDg.jtaContent);
    var nbLines: Int = MainScaSSHconnect.tabUpload.length;
    println("Nb Lines to treat=" + nbLines)
    var set: Set[String] = MainScaSSHconnect.hmConn.keySet();
    val cmdScp = MainScaSSHconnect.scpCommand + " -p -t "
    for (i <- 0 until nbLines) {

      // traiter ici les cas de regexp et boucler : TODO
      // on ne prend en upload pur que les lignes dont le rang =0 1ière colonne)
      if (MainScaSSHconnect.tabUpload(i).cmdRank == 0) {
        println("Treat Line=" + i)
        var patternRemoteFileInit: String = MainScaSSHconnect.tabUpload(i).remoteFile;
        var serveurs: Array[String] = MainScaSSHconnect.tabUpload(i).idServer.split("\\s+");
        // pour chaque item on peut avoir des caractere joker
        var lstServ: java.util.List[String] = new ArrayList[String]();
        var ips: java.util.List[String] = new ArrayList[String]();
        //			 System.out.print ("Liste initiale :");
        //			 for (String srv : serveurs) {
        //			 System.out.print(srv+" ");
        //			 }
        //			 System.out.println ();
        lstServ.clear();
        for (srv <- serveurs) {

          ips.clear();
          if (srv.toLowerCase().equals("allip")) {
            // One prend que les IP differents pour eviter les doublons
            for (key <- set.asScala) {
              var ip: String = MainScaSSHconnect.hmConn.get(key).iPAddress;
              if (!ips.contains(ip)) {
                if (key.trim().length() > 0)
                  lstServ.add(key);
                ips.add(ip);
              }
            }
          } else if (srv.equals("*")) {
            println("remplissage *")
            // On prend tous les IDServer Doublons IP possibles
            for (key <- set.asScala) {
              if (key.trim().length() > 0)
                lstServ.add(key);
            }

          } else if (srv.trim().length() > 1 && srv.contains("*")) {
            // on replace chaque * par la regexp .*

            var len: Int = srv.length();
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
            // System.out.println("passage par pattern pat="+pat.toString());
            for (key <- set.asScala) {
              if (pat.matcher(key).find()) {
                lstServ.add(key);
              }
            }

          } else {
            if (srv.trim().length() > 0 && set.contains(srv.trim()))
              lstServ.add(srv);
          }

          //          				for ( srvTer <- lstServ) {
          //          					System.out.println("srvTer=" + srvTer);
          //          				}
          //				System.exit(0);

        }

        var tabActor: Array[ActorRef] = Array.ofDim(lstServ.asScala.length)
        val systemUpl: ActorSystem = ActorSystem("MySystem")
        UploadDialog.tabBool = Array.ofDim(lstServ.asScala.length)
        for (j <- 0 until lstServ.asScala.length) {
          tabActor(j) = systemUpl.actorOf(Props(new ActorUpload(j)), "ActorUpload_" + j)
          UploadDialog.tabBool(j) = false
        }
        var rank = 0
        upDg.jtaContent = new StringBuilder(
          upDg.jtaContent).append("\nUpload " + MainScaSSHconnect.tabUpload(i).localFile + " to servers  : " + lstServ.asScala.mkString("(", ", ", ")") + " at " + MainScaSSHconnect.tabUpload(i).remoteFile + "\n").toString
        upDg.jta.setText(upDg.jtaContent);
        for (srvBis <- lstServ.asScala) {
          //MainScaSSHconnect.tabUpload(i).idServer = srvBis

          var patternRemoteFile: String = patternRemoteFileInit.replaceAll("<server>", srvBis);

          //MainScaSSHconnect.tabUpload(i).remoteFile = patternRemoteFile
          var upFile: UploadFile = new UploadFile(MainScaSSHconnect.tabUpload(i).cmdRank, MainScaSSHconnect.tabUpload(i).idServer,
            MainScaSSHconnect.tabUpload(i).localFile, MainScaSSHconnect.tabUpload(i).remoteFile, MainScaSSHconnect.tabUpload(i).execute)
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
            upDg.jtaContent = new StringBuilder(
              upDg.jtaContent)
              .append("\n Error  Remote  Directory not found at :")
              .append(repDist).toString();
            upDg.jta.setText(upDg.jtaContent);
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

    }

    // ate
    var fin = System.currentTimeMillis();

    var nbMin = (fin - deb) / (1000 * 60);
    var nbSecs = ((fin - deb) / 1000) - nbMin * 60;
    var millis = (fin - deb) - (nbSecs * 1000) - nbMin * 60 * 1000;
    var mesg = "Treated in " + nbMin + " mn " + nbSecs + " secs "
    +millis + " ms ";
    upDg.jtaContent = new StringBuilder(upDg.jtaContent)
      .append("\n End of  Uploading Files \n\t").append(mesg)
      .toString();
    upDg.jta.setText(upDg.jtaContent);

    this.interrupt();
  }

}
object UploadDialog {
  var tabBool: Array[Boolean] = null;
}
