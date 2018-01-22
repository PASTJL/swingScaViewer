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
import akka.actor.ActorRef
import akka.actor.PoisonPill
import akka.AkkaException
//import akka.dispatch.MessageQueueAppendFailedException

class ReaderActor(solvers: Array[ActorRef], myReader: MyReader) extends Actor {

  val nbActors = solvers.length
  def receive = {

    case "start" => reading()

    case "stop" => context.stop(self)

  }
  private def reading() {
    // Lire et envoyer le message a chaque acteur

    ScaParserMain.typeRead match {
      case ScaParserMain.WITHOUT_INCLUDE_WITHOUT_EXCLUDE =>
        {
          var bool = true
          var i = 0
          var lect = ("", "")

          while (bool) {
            lect = myReader.read(lect._1, ScaParserMain.reader)
            if (ScaParserMain.extractBool) {
              ScaParserMain.writer.write(lect._2 + "\n")
            }
            ScaParserMain.compteurGlobalRead += 1

            if (lect._2 != "") {
              try {
              solvers(i) ! new LectEnr(lect._2)
              	} catch {
                    case e: AkkaException =>
                      e.printStackTrace
                      println(e.getMessage)
                      Thread.sleep(100)
              	}
              i = (i + 1) % nbActors

            } else {
              for (j <- 0 until nbActors) {
                var bool2 = true
                while (bool2) {
                  try {
                    solvers(j) ! "stop"
                    bool2 = false
                  } catch {
                    case e: Exception =>
                      e.printStackTrace
                      println(e.getMessage)
                      Thread.sleep(100)
                   
                    case e: AkkaException =>
                      e.printStackTrace
                      println(e.getMessage)
                      Thread.sleep(100)
                  }
                }
              }
              bool = false
            }
          }
        }
      case ScaParserMain.WITH_INCLUDE_WITHOUT_EXCLUDE =>
        {
          var bool = true
          var i = 0
          val filterIncl = ScaParserMain.props.getProperty("fileIn.inclEnrReg")
          var lect = ("", "")

          while (bool) {
            lect = myReader.readWithInclFilter(lect._1, ScaParserMain.reader, filterIncl)
            ScaParserMain.compteurGlobalRead += 1
            if (ScaParserMain.extractBool) {
              ScaParserMain.writer.write(lect._2 + "\n")
            }
            if (lect._2 != "") {
              solvers(i) ! new LectEnr(lect._2)

              i = (i + 1) % nbActors
            } else {
              // on envoie un message de stop
              for (j <- 0 until nbActors) {
                var bool2 = true
                while (bool2) {
                  try {
                    solvers(j) ! "stop"
                    bool2 = false
                  } catch {
                    case e: Exception =>
                      e.printStackTrace
                      println(e.getMessage)
                      Thread.sleep(100)
                  
                    case e: AkkaException =>
                      e.printStackTrace
                      println(e.getMessage)
                      Thread.sleep(100)
                  }
                }

              }
              bool = false
            }

          }
        }

      case ScaParserMain.WITHOUT_INCLUDE_WITH_EXCLUDE =>
        {
          var bool = true
          var i = 0
          val filterExcl = ScaParserMain.props.getProperty("fileIn.exclEnrReg")
          var lect = ("", "")
          while (bool) {
            lect = myReader.readWithExclFilter(lect._1, ScaParserMain.reader, filterExcl)
            if (ScaParserMain.extractBool) {
              ScaParserMain.writer.write(lect._2 + "\n")
            }
            ScaParserMain.compteurGlobalRead += 1
            //              while(ScaParserMain.compteurGlobalRead -ScaParserMain.compteurGlobalTreated.get > ScaParserMain.waitingEnrMax )
            //             {
            //             //    Thread.`yield`
            //              Thread.sleep(ScaParserMain.waitingTime)
            //             }
            if (lect._2 != "") {
              solvers(i) ! new LectEnr(lect._2)

              i = (i + 1) % nbActors
            } else {
              for (j <- 0 until nbActors) {
                var bool2 = true
                while (bool2) {
                  try {
                    solvers(j) ! "stop"
                    bool2 = false
                  } catch {
                    case e: Exception =>
                      e.printStackTrace
                      println(e.getMessage)
                      Thread.sleep(100)
                    
                    case e: AkkaException =>
                      e.printStackTrace
                      println(e.getMessage)
                      Thread.sleep(100)
                  }
                }

              }
              bool = false
            }

          }
        }
      case ScaParserMain.WITH_INCLUDE_WITH_EXCLUDE =>
        {
          var bool = true
          var i = 0
          val filterIncl = ScaParserMain.props.getProperty("fileIn.inclEnrReg")
          val filterExcl = ScaParserMain.props.getProperty("fileIn.exclEnrReg")

          var lect = ("", "")
          while (bool) {
            lect = myReader.readWith2Filter(lect._1, ScaParserMain.reader, filterIncl, filterExcl)
            ScaParserMain.compteurGlobalRead += 1
            if (ScaParserMain.extractBool) {
              ScaParserMain.writer.write(lect._2 + "\n")
            }
            if (lect._2 != "") {
              solvers(i) ! new LectEnr(lect._2)

              i = (i + 1) % nbActors
            } else {
              for (j <- 0 until nbActors) {
                var bool2 = true
                while (bool2) {
                  try {
                    solvers(j) ! "stop"
                    bool2 = false
                  } catch {
                    case e: Exception =>
                      e.printStackTrace
                      println(e.getMessage)
                      Thread.sleep(100)
                   
                    case e: AkkaException =>
                      e.printStackTrace
                      println(e.getMessage)
                      Thread.sleep(100)
                  }
                }

              }
              bool = false
            }

          }
        }
    }
  }

}