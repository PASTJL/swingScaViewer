package com.jlp.scaSSHconnect
import akka.actor.Actor
class ActorUpload(rank: Int) extends Actor {
  def receive = {

    case MessageUpl(scaSSH, uplFile, repDist, rfile1, cmdScp) =>
      scaSSH.use { sess =>
        scaSSH.upload(sess, uplFile.localFile, rfile1, cmdScp)
        if (uplFile.execute.toUpperCase() == "YES") {
          // execution du script:
          val fileOnly = rfile1.substring(repDist.length)

          // Verification que le repertoire est executatble
          val test = "cd " + repDist + "; echo JLP > test.sh;chmod 755 test.sh;./test.sh 2>&1"
          val ret = scaSSH.executeCommand(sess, test)
          if (ret.trim.toUpperCase().contains("JLP")) {

            val cmd = "cd " + repDist + ";chmod 775 " + fileOnly + ";./" + fileOnly
            scaSSH.executeCommand(sess, cmd)

          } else {
            println("The directory " + repDist + " is mounted as noexec on the server " + uplFile.idServer)
          }
        }
        // On efface test.sh
        scaSSH.executeCommand(sess, "cd " + repDist + ";rm -f test.sh")
        scaSSH.close()

      }
      UploadDialog.tabBool(rank) = true
      context.stop(self)
  }
}