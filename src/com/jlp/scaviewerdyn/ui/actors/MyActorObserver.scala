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
package com.jlp.scaviewerdyn.ui.actors
import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
//import akka.dispatch.Future
import akka.actor.ActorSystem
import com.jlp.scaviewer.ui.ScaCharting
import com.jlp.scaviewerdyn.ui.ScalaChartingDyn
import java.io.File
class MyActorObserver extends Actor {
  val log = Logging(context.system, this)
  var bool: Boolean = true
  def receive = {
    case "hello" =>
      log.info("received test")
      sender ! "retour"

    case MyMessage(actor) =>
      if (!ScaCharting.listFiles.isEmpty) {
       // log.info("receiving a message: listFile=" + ScaCharting.listFiles)
        // surveiller que le fichier n a pas change
        while (ScalaChartingDyn.boolExamine && !ScaCharting.listFiles.isEmpty) {
          var idx = 0
          for (file <- ScaCharting.listFiles) {

            var oldModified: Long = ScaCharting.listChartingInfo(idx).lastModified
            var newModified: Long = file.lastModified()
            if (oldModified != newModified) {
              // on fait le traitement
            //  println("Avant modif file =" + file.getAbsolutePath() + " oldModified=" + oldModified + " newModified=" + newModified)
              actor ! (file, idx)

            }
            idx += 1
           // println("file =" + file.getAbsolutePath() + " oldModified=" + oldModified + " newModified=" + newModified)
          }
          Thread.sleep(ScaCharting.tmpProps.getProperty("scaviewer.dyn.timeout").toLong)
        }

      }
    case _ => log.info("received unknown message")
  }

}
