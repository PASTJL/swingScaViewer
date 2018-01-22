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
package com.jlp.scaviewerdyn.ui
import akka.actor.Actor
import com.jlp.scaviewer.ui.ScaCharting
import akka.event.Logging
import com.jlp.scaviewerdyn.ui.actors.MyActorReaction
import com.jlp.scaviewerdyn.ui.actors.MyMessage
import akka.actor.ActorContext
import akka.actor.Kill
import akka.actor.ActorRef
import akka.actor.PoisonPill

object ScalaChartingDyn {

  var boolExamine = true
  var isRunning = false
  //println("object ScalaChartingDyn statique")
  import akka.actor.ActorSystem
  import akka.actor.Props
  import com.jlp.scaviewerdyn.ui.actors.MyActorObserver

  var system: ActorSystem = null

  var actorAction: ActorRef = null
  var observer: ActorRef = null
  def start() {
    stop
    system = ActorSystem("MySystemDyn")
    println("object ScalaChartingDyn.starting ")
    if (!isRunning) {
    //  println("object ScalaChartingDyn.starting not Running ")
      actorAction = system.actorOf(Props[MyActorReaction], "action")
      observer = system.actorOf(Props[MyActorObserver], "observer")
     // println("object ScalaChartingDyn.starting after Running ")
    }
    boolExamine = true
    isRunning = true
    //  var msg=new MyMessage(actorAction,List.empty)
    //  // myActor ! "hello"
    //    observer ! msg
    //  // myActor ! "stop"

  }

  def stop() {
    boolExamine = false
    // println("object ScalaChartingDyn.stop ")
    if (null != system) {
      if (null != actorAction)

        actorAction ! PoisonPill
       // system.stop(actorAction)

      if (null != observer) {
         observer ! PoisonPill
       // system.stop(observer)
        //       }
      }
      var fut: scala.concurrent.Future[akka.actor.Terminated] = system.terminate()
          while (!fut.isCompleted) {

          }
      
    }

    isRunning = false
  }
}