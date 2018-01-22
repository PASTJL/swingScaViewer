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
package com.jlp.scaviewer.scalogparser

import akka.actor.Actor
import akka.actor.PoisonPill

class LoggerActor extends Actor {
 var str = ""
   var  nbBoucles = 0L
    var bool=true
  def receive = {
    case "stop" =>
      bool=false
      System.out.println("Stop ActorLogger");

      str = new StringBuilder(ScaParserMain.ta.text).
        append("\n").
        append("Tous threads termines lines treated : ").
        append(ScaParserMain.nbLinesOfFile).append(" / ").
        append(ScaParserMain.nbLinesOfFile).append(" in ").
        append((System.currentTimeMillis() - ScaParserMain.deb)).
        append(" ms \n").append(" .").toString()
      if (!ScaParserMain.modeDebug) {
        ScaParserMain.ta.text += str
      } else {
        System.out.println(str);
      }
      self ! PoisonPill
    case "start" => run

  }

  private def run() {

    System.out.println("Demarrage ActorLogger");

    while (bool) {

      try {
        Thread.sleep(1000);
        
      

      } catch {
        case e:InterruptedException =>  e.printStackTrace()
      }
      if (nbBoucles % 10 == 0) {

        if (ScaParserMain.modeDebug) {
          str = new StringBuilder( ScaParserMain.ta.text ).toString
          ScaParserMain.logTrace.append( str+"\n")
          
        } else {
          str = "";
          // str = new StringBuilder(
          // MultiGenericLogFileListenerGo.parser.getJta()
          // .getText()).toString();
        }
       
//          

      }

      nbBoucles += 1
    }

  }

}