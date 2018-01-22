package com.jlp.scaSSHconnect

import com.jcraft.jsch.Session
import com.jcraft.jsch.UserInfo
import com.jcraft.jsch.UIKeyboardInteractive
import com.jcraft.jsch.JSch
import com.jcraft.jsch.Channel
import scala.None
import scala.None
import com.jcraft.jsch.ChannelExec
import java.io.OutputStream
import java.io.InputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

case class MySSHConnection(host: String, port: String, user: String, password: String) extends UserInfo with UIKeyboardInteractive {

  def promptPassword(message: String): Boolean =
    {
      true
    }
  def getPassword(): String = {
    password
  }

  def promptPassphrase(message: String): Boolean =
    {
      true
    }

  def getPassphrase(): String = {

    null
  }
  def promptYesNo(message: String): Boolean = {
    true
  }
  def showMessage(message: String) {

  }
  def promptKeyboardInteractive(destination: String, name: String, instruction: String, prompt: Array[String], echo: Array[Boolean]): Array[String] = {

    Array[String](password)

  }
}

abstract class ScaSSH {

  var sess: Session = null
  def getSession: Session
  def use[T](action: Session => T): T = {
    val sess = getSession
    if (null != sess) {
      try {

        val ret = action(sess)
        ret
      } finally {
        close()
      }
    } else null.asInstanceOf[T]
  }

  def close() {

    if (null != sess && sess.isConnected) {
      sess.disconnect()

    }
    sess = null
  }
  def upload(sess: Session, localFile: String, remoteFile: String, scpUploadCmd: String = "scp -p -t "): Seq[Any] = {
    if (null != sess) {
      var channel: Channel = null

      channel = sess.openChannel("exec")

      var command: String = scpUploadCmd + " " + remoteFile
      println("command=" + command)
      channel.asInstanceOf[ChannelExec].setCommand(command)
      val out: OutputStream = channel.getOutputStream();
      val in: InputStream = channel.getInputStream();
      if (!channel.isConnected) channel.connect()
      if (ScaSSH.checkAck(in) != 0) {
        System.exit(0);
      }
      val filesize: Long = (new File(localFile)).length();
      println("filesize=" + filesize)
      command = "C0644 " + filesize + " "
      if (localFile.lastIndexOf('/') > 0) {
        command += localFile.substring(localFile.lastIndexOf('/') + 1);
      } else {
        command += localFile;
      }
      command += "\n";

      out.write(command.getBytes())
      out.flush()

      if (ScaSSH.checkAck(in) != 0) {
        System.exit(0)
      }

      val fis = new FileInputStream(localFile);
      var buf: Array[Byte] = new Array[Byte](10240)
      var bool = true
      while (bool) {
        var len: Int = fis.read(buf, 0, buf.length);
        if (len <= 0)
          bool = false
        out.write(buf, 0, len); // out.flush();
      }
      fis.close();

      // send '\0'
      buf(0) = 0;
      out.write(buf, 0, 1);
      out.flush();
      if (ScaSSH.checkAck(in) != 0) {
        System.exit(0);
      }
      out.close();
      if (null != channel && !channel.isClosed) {
        channel.disconnect
        channel = null
      }
      
      null
    } else null
  }

  def download(sess: Session, localFile: String, remoteFile: String, compress: Boolean = false, scpDownloadCmd: String = "scp -p -t "): Seq[Any] = {
    if (null != sess) {
      var channel: Channel = null

      var remoteNew = remoteFile
      var localNew = localFile
      var fos: FileOutputStream = null;
      println("ScaSSH download compress=" + compress)
      println("ScaSSH download remoteFile=" + remoteFile)
      println("ScaSSH download localFile=" + localFile)
      if ((!remoteFile.endsWith(".gz")) && compress) {
        var dir = remoteFile.substring(0, remoteFile.lastIndexOf("/"));
        var file = remoteFile.substring(remoteFile.lastIndexOf("/") + 1);
        var command = "cat  " + remoteFile + "| gzip > /tmp/" + file + ".sca.gz"
        channel = sess.openChannel("exec")
        channel.asInstanceOf[ChannelExec].setCommand(command);

        val out = channel.getOutputStream();
        val in = channel.getInputStream();

        channel.connect();
        out.write('\u0003');
        out.flush();
        val tmp = new Array[Byte](1024000)
        var jj = 0
        var ret = ""
        var bool = true
        while (bool) {
          while (in.available() > 0) {
            var i = in.read(tmp, jj, 1024);
            // int i=in.read(tmp, j, 8);
            if (i < 0)
              bool = false
            // System.out.print(new String(tmp, j, j+i));
            jj = jj + i;
            ret = new String(tmp, 0, jj);
          }
          if (channel.isClosed()) {
            // System.out.println("exit-status: "+channel.getExitStatus());
            bool = false
          }
          try {
            Thread.sleep(1000);
          } catch {
            case ee: Exception =>
          }
        }
        channel.disconnect();

        System.out.println("Compression reussie de : " + remoteFile);
        remoteNew = "/tmp/" + file + ".sca.gz";
        localNew = localFile + ".gz"
      }

      channel = sess.openChannel("exec")
      var command = scpDownloadCmd + " " + remoteNew
      println("SSH commandscp=" + command)
      channel.asInstanceOf[ChannelExec].setCommand(command)

      var out = channel.getOutputStream()
      var in = channel.getInputStream()

      channel.connect()
      var buf = new Array[Byte](2048)
      buf(0) = 0
      out.write(buf, 0, 1);
      out.flush();
      var bool = true
      while (bool) {

        var c = ScaSSH.checkAck(in);
        if (c != 'C') {
          bool = false
        }
        if (bool) {

          // read '0644 '
          in.read(buf, 0, 5);

          var filesize = 0L;
          var bool2 = true
          while (bool2) {

            if (in.read(buf, 0, 1) < 0) {
              // error
              bool2 = false
            }
            if (buf(0) == ' ') {

              bool2 = false
            } else
              filesize = filesize * 10L + (buf(0) - '0').asInstanceOf[Long]
          }

          var file: String = null;
          var bool3 = true
          var i = 0
          while (bool3) {
            in.read(buf, i, 1);
            if (buf(i) == 0x0a) {
              file = new String(buf, 0, i);
              bool3 = false
            }
            i += 1
          }

          // System.out.println("filesize="+filesize+", file="+file);

          // send '\0'
          buf(0) = 0;
          out.write(buf, 0, 1);
          out.flush();

          // read a content of lfile
          fos = new FileOutputStream(localNew);
          var foo = 0;
          var bool4 = true
          while (bool4) {
            if (buf.length < filesize)
              foo = buf.length;
            else
              foo = filesize.toInt;
            foo = in.read(buf, 0, foo);
            if (foo < 0) {
              // error
              bool4 = false
            }
            if (bool4) {
              fos.write(buf, 0, foo);
              filesize -= foo;
              if (filesize == 0L)
                bool4 = false
            }
          }
          fos.close();
          fos = null;

          if (ScaSSH.checkAck(in) != 0) {
            bool = false
          }

          // send '\0'
          if (bool) {
            buf(0) = 0;
            out.write(buf, 0, 1);
            out.flush();
          }
        }
      }

      if (compress && remoteNew.endsWith(".sca.gz")) {

        // on efface le fichier .sca.gz
        command = "rm -f " + remoteNew
        channel = sess.openChannel("exec");
        channel.asInstanceOf[ChannelExec].setCommand(command)

        out = channel.getOutputStream();
        in = channel.getInputStream();

        channel.connect();
        out.write('\u0003');
        out.flush();

        // byte[] tmp=new byte[1024];
        // Augmente pour pouvoir lister un plus grand nombre de
        // fichiers
        // avec la commande ls
        var tmp = new Array[Byte](1024000)
        var jj = 0;
        var ret = ""
        var bool = true
        while (bool) {
          while (in.available() > 0) {
            var i = in.read(tmp, jj, 1024);
            // int i=in.read(tmp, j, 8);
            if (i < 0)
              bool = false
            // System.out.print(new String(tmp, j, j+i));
            jj = jj + i;
            ret = new String(tmp, 0, jj);
          }
          if (channel.isClosed()) {
            // System.out.println("exit-status: "+channel.getExitStatus());
            bool = false
          }
          try {
            Thread.sleep(1000);
          } catch {
            case ee: Exception =>
          }
        }
        System.out.println("ret =" + ret + " ; effacement de " + remoteNew)

      }
      channel.disconnect()
      try {
        if (fos != null)
          fos.close()
      } catch {
        case ee: Exception =>
      }
     
      null
    } else null
  }
  def executeCommand(sess: Session, command: String): String = {
    if (null != sess) {
      var channel: Channel = null

      channel = sess.openChannel("exec")

      channel.asInstanceOf[ChannelExec].setCommand(command)
      println("ScaSSH commande a executer :" + command)
      channel.setInputStream(null);

      val in: InputStream = channel.getInputStream();
      channel.asInstanceOf[ChannelExec].setErrStream(System.err)
      // channel.asInstanceOf[ChannelExec].setOutputStream(System.out)
      channel.asInstanceOf[ChannelExec].setPty(true)
      if (!channel.isConnected) {

        channel.connect()
      }
      var bool = true
      var tmp = new Array[Byte](10240000)
      var j = 0;
      var ret = ""

      while (bool) {

        while (in.available() > 0) {
          var i = in.read(tmp, j, 1024);
          // int i=in.read(tmp, j, 8);
          if (i < 0)
            bool = false
          //System.out.print(new String(tmp, j, j + i));
          j = j + i;
          ret = new String(tmp, 0, j);

        }
        if (channel.isClosed()) {
          // System.out.println("exit-status: "+channel.getExitStatus());
          bool = false
        }
        try {
          Thread.sleep(1000);
        } catch {
          case ee: Exception =>
        }
      }
      if (null != channel && !channel.isClosed) {
        channel.disconnect
        channel = null
      }

      // println("SSH Execute commande  ret="+ret)
       
      ret
    } else null
  }

}

object ScaSSH {
  var scpCommandDownload = "scp -f"
  def apply(myCon: MySSHConnection) = new ScaSSH {
    override def getSession = {
      if (null != sess) sess
      else {
        val jSch: JSch = new JSch()

        try {
          sess = jSch.getSession(myCon.user, myCon.host, myCon.port.toInt)
          val ui: UserInfo = myCon
          sess.setUserInfo(ui)
          // session.setServerAliveInterval(1000)

          sess.connect
          sess
        } catch { // second Chance with timeout
          case _: Throwable =>
            try {
              sess = jSch.getSession(myCon.user, myCon.host, myCon.port.toInt)
              val ui: UserInfo = myCon
              sess.setUserInfo(ui)
              // session.setServerAliveInterval(1000)

              sess.connect(1000)
              sess
            } catch {
              case _: Throwable => println("Can't open a session for " + myCon.host + " : " + myCon.port.toInt); null
            }
        }
      }

    }
  }
  def checkAck(in: InputStream): Int = {
    val b: Int = in.read();
    // b may be 0 for success,
    // 1 for error,
    // 2 for fatal error,
    // -1
    if (b == 0)
      return b;
    if (b == -1)
      return b;

    if (b == 1 || b == 2) {
      var sb: StringBuilder = new StringBuilder();
      var c: Int = 0;
      do {
        c = in.read();
        sb.append(c.asInstanceOf[Char]);
      } while (c != '\n');
      if (b == 1) { // error
        System.out.print(sb.toString());
      }
      if (b == 2) { // fatal error
        System.out.print(sb.toString());
      }
    }
    return b;
  }

  def main(args: Array[String]) {
    val scaSSH = ScaSSH(new MySSHConnection("localhost", "22", "JLP", "JLP"))
    val got = scaSSH.use { sess =>
      scaSSH.upload(sess, args(0), "/tmp/test.jlp")
      println("upload termine")
      scaSSH.download(sess, args(0) + "_local", "/tmp/test.jlp", true)
      println("download termine")
      scaSSH.executeCommand(sess, "ls /tmp/*")

    }
    if (null != got && got.isEmpty) println(" got is empty")
    else {
      println(" got is not empty")

      println("files=" + got.replaceAll("\n", " "))
    }

  }
}

