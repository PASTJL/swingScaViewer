/*Copyright 2012 Jean-Louis PASTUREL 
*
*   Licensed under the Apache License, Version 2.0 (the "License");
*  you may not use this file except in compliance with the License.
*  You may obtain a copy of the License at
*
*       http://www.apache.org/licenses/LICENSE-2.0
*
*   Unless required by applicable law or agreed to in writing, software
*  distributed under the License is distributed on an "AS IS" BASIS,
*   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*   See the License for the specific language governing permissions and
*  limitations under the License.
*/
package com.jlp.scaviewer.filestats

import java.io.BufferedReader
import java.io.File
import akka.actor.ActorRef
import akka.actor.PoisonPill
import akka.actor.ActorSystem
import akka.actor.Props
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicLong
import akka.actor.Actor
import com.typesafe.config._

class ParserStatFileActor extends Actor {
  var entete = ""
  var props = MyDialogStatsFile.currentProps
  var nbActors = props.getProperty("scaviewer.filestats.nbActors").toInt
  var tabBuf: Array[BufferedReader] = new Array(nbActors)
  var diagg: MyDialogStatsFile = null
  var pas: Double = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.pasValuePercentile").toDouble
 
  props = MyDialogStatsFile.currentProps
  nbActors = props.getProperty("scaviewer.filestats.nbActors").toInt
  var tabActors: Array[ActorRef] = Array.ofDim(nbActors)
  tabBuf = new Array(nbActors)

  MyDialogStatsFile.tabIsterminated = new Array(nbActors)

  MyDialogStatsFile.tabHm = new Array(nbActors)
  for (i <- 0 until nbActors) {
    tabBuf(i) = MyDialogStatsFile.initReader(new File(props.getProperty("scaviewer.filestats.nameFile")))
    MyDialogStatsFile.tabIsterminated(i) = false
  }

  entete = " Parsing file : " + props.getProperty("scaviewer.filestats.nameFile") + "\n"
  //  val config= ConfigFactory.load.getConfig("scaviewer.mailbox")
  val system = ActorSystem("MySystem")

  def receive = {
    case ("start", diag: MyDialogStatsFile) =>
      this.diagg = diag
      execute()
    case "stop" =>
      diagg.bAnalyse.enabled = true
      diagg.bSave.enabled = true

      for (i <- 0 until nbActors) {
        tabActors(i) ! PoisonPill
      }

      context.stop(self)
        var fut: scala.concurrent.Future[akka.actor.Terminated]=   system.terminate()
  while (!fut.isCompleted){
    
  }
     
  }

  def execute() =
    {
      // println("avant lancement des actors")
      var deb = System.currentTimeMillis
      MyDialogStatsFile.ta.text = ""
      MyDialogStatsFile.ta.repaint

      for (i <- 0 until nbActors) {
        tabActors(i) = system.actorOf(Props[FilesStatsActor], "action_" + i.toString)
        tabActors(i) ! new Message(i, nbActors, tabBuf(i))
        // println("lancement actor : " + "action_" + i.toString)
      }
      // println("après  lancement des actors")
      // Surveillance mettre bool à false
      var bool = false
      var j = 0;
      var linePoints = ""
      var textTa = entete
      var lineEnreg = ""
      //lineEnreg = "Treated records : " + MyDialogStatsFile.nbEnrTraites.get + " / " + props.getProperty("scaviewer.filestats.nbRecords")
        lineEnreg = "Treated records : " + MyDialogStatsFile.nbEnrTraites.get + " / " + MyDialogStatsFile.nbRecords
      textTa = entete + lineEnreg + "\n" + linePoints
      MyDialogStatsFile.ta.text = textTa
      MyDialogStatsFile.ta.repaint
      while (!bool) {
        bool = true
        for (i <- 0 until MyDialogStatsFile.tabIsterminated.length) {

          bool &&= MyDialogStatsFile.tabIsterminated(i)
        }
        if (j == 10) {
          linePoints = ""
          lineEnreg = "Treated records : " + MyDialogStatsFile.nbEnrTraites.get + " / " + MyDialogStatsFile.nbRecords
       //   lineEnreg = "Treated records : " + MyDialogStatsFile.nbEnrTraites.get + " / " + props.getProperty("scaviewer.filestats.nbRecords")
          textTa = entete + lineEnreg + "\n" + linePoints
          MyDialogStatsFile.ta.text = textTa
          MyDialogStatsFile.ta.repaint
          j = 0
          // Afficher le nombre d'enreg
        } else {
          // afficher un point
          // println("afficher un point")
          linePoints += " ."
        //  lineEnreg = "Treated records : " + MyDialogStatsFile.nbEnrTraites.get + " / " + props.getProperty("scaviewer.filestats.nbRecords")
         lineEnreg = "Treated records : " + MyDialogStatsFile.nbEnrTraites.get + " / " + MyDialogStatsFile.nbRecords
          textTa = entete + lineEnreg + "\n" + linePoints
          MyDialogStatsFile.ta.peer.setText(textTa)
          MyDialogStatsFile.ta.repaint
        }
        Thread.sleep(1000)
        j += 1

      }
     
       if (!MyDialogStatsFile.errorOccurs) {
   //   lineEnreg = "Treated records : " + MyDialogStatsFile.nbEnrTraites.get + " / " + props.getProperty("scaviewer.filestats.nbRecords")
     lineEnreg = "Treated records : " + MyDialogStatsFile.nbEnrTraites.get + " / " + MyDialogStatsFile.nbRecords
      textTa = entete + lineEnreg + "\n" + linePoints + "\nfilling tabHm in " + (System.currentTimeMillis - deb) + " ms"
      deb = System.currentTimeMillis
     

        MyDialogStatsFile.ta.peer.setText(textTa)
      }
      MyDialogStatsFile.ta.repaint

      // On concentre tout sur tabHm(0)
      if (MyDialogStatsFile.tabHm.length > 1) {
        for (i <- 1 until MyDialogStatsFile.tabHm.length; if null != MyDialogStatsFile.tabHm(i)) {
          for ((key, cEnr) <- MyDialogStatsFile.tabHm(i)) {
            if (MyDialogStatsFile.tabHm(0) contains (key)) {

              MyDialogStatsFile.tabHm(0).put(key, MyDialogStatsFile.tabHm(0).get(key).get.mergeEnr(cEnr, pas))
            } else {
              MyDialogStatsFile.tabHm(0).put(key, cEnr)
            }
          }
          MyDialogStatsFile.tabHm(i).clear
        }
      }
      if (null != MyDialogStatsFile.tabHm(0)) {
       
        textTa += "\n Merging duration : " + (System.currentTimeMillis - deb) + " ms"
        // On clos les enregistrement
        deb = System.currentTimeMillis
        MyDialogStatsFile.tabHm(0) = MyDialogStatsFile.tabHm(0) map (tup => (tup._1, tup._2.closeEnr(pas)))

        textTa += "\n Closing enregs duration : " + (System.currentTimeMillis - deb) + " ms"
        MyDialogStatsFile.ta.text = textTa

        // On tiens compte de echelle
        deb = System.currentTimeMillis
        var multScale = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.scaleValue").toDouble
        if (multScale != 1.0) {
          MyDialogStatsFile.tabHm(0) = MyDialogStatsFile.tabHm(0) map (tup => (tup._1, tup._2.scale(multScale)))
        }
        textTa += "\n Scaling enregs duration : " + (System.currentTimeMillis - deb) + " ms"
        MyDialogStatsFile.ta.text = textTa
        diagg.bAnalyse.enabled = true
        diagg.bSave.enabled = true
        new MyDialogResultStatsFiles(diagg)
      }
      else
      {
        diagg.bAnalyse.enabled = true
        diagg.bSave.enabled = true
        println("MyDialogStatsFile.tabHm(0) is null")
      }

    }

}
