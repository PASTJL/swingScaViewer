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

class MyActorReaction extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case "retour" => log.info("received retour :" + sender.toString())
    case "stop" =>
      {
        log.info("stopping actor")
        context.stop(self)
      }
    case (file: java.io.File, idx: Int) =>
     new MAJChart(file, idx).execute
     

    case _ => log.info("received unknown message")
  }
}