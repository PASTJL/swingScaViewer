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

import akka.actor.Actor
import java.io.BufferedReader
import scala.collection._
import javax.swing.JOptionPane
import java.text.SimpleDateFormat
import java.util.Calendar
//import org.joda.time.DateTime
//import org.joda.time.format.DateTimeFormatter
//import org.joda.time.format.DateTimeFormat

class FilesStatsActor extends Actor {
  var mapEnreg: mutable.Map[String, CumulEnregistrementStatMemory] = mutable.Map()
  var msg: Message = null
  var regDate = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.regexpDate")
  var formatDate = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.formatDate")
  //var dtf: DateTimeFormatter = null
  // var dateDeb: DateTime = null
  //var dateFin: DateTime = null

  var sdf: SimpleDateFormat = null
  var dateDeb: Long = 0L
  var dateFin: Long = 0L
  var sep = ";"
  val pasCountMax = 1000L
  var pasCount = 0L
  var nbEnrLocal = 0L
  var pas: Double = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.pasValuePercentile").toDouble
  if (MyDialogStatsFile.isDatedFile) {
    if (!formatDate.toLowerCase.contains("timein")) {
      // dtf = DateTimeFormat.forPattern(MyDialogStatsFile.regsDate._2)
      sdf = new SimpleDateFormat(MyDialogStatsFile.regsDate._2, MyDialogStatsFile.currentLocale)

    } else {

      // dtf = MyDialogStatsFile.dtfMillis
      //   sdf = MyDialogStatsFile.sdfMillis
      sdf = new SimpleDateFormat("yyyy/MM/dd:HH:mm:ss.SSS")
    }
    //  dateDeb = dtf.withLocale(MyDialogStatsFile.currentLocale).parseDateTime(MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.debOfAnalyse"))
    //  dateFin = dtf.withLocale(MyDialogStatsFile.currentLocale).parseDateTime(MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.finOfAnalyse"))

    dateDeb = sdf.parse(MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.debOfAnalyse")).getTime
    dateFin = sdf.parse(MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.finOfAnalyse")).getTime

  }
  def receive = {

    case Message(rang, nbActor, buff) => traiterFichier(Message(rang, nbActor, buff))
    case "shutdown" =>
      //  println("Actor : " + msg.rang + " as treated " + nbEnrLocal + " enr")

      context.stop(self)
  }

  private def traiterFichier(mes: Message) =
    {
      this.msg = mes
      MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.regexpColumn") match {
        case "true" => analyzeByColumnNumber
        case _ => analyzeByRegexp
      }
      // On dit que le thread est fini

      MyDialogStatsFile.tabIsterminated(mes.rang) = true
      // on arrete l'actor
      // println("FilesStatsActor num :" + mes.rang + "mapEnreg.length =" + mapEnreg.size)
      self ! "shutdown"
    }

  private def analyzeByColumnNumber {
    var colPiv1 = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.numColRegexp1Pivot")
    var regPiv2 = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.regexp2Pivot")
    var colVal1 = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.numColRegexp1Value")
    var regVal2 = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.regexp2Value")
    sep = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.csvSeparator")
    if (colPiv1.length == 0 && colVal1.length == 0) {
      JOptionPane.showMessageDialog(null, "At least, the number of column for Pivot or  Value must be filled ")

    } else if (colPiv1.length == 0 && colVal1.length > 0) {
      // traitement de la valeur uniquement
      if (regVal2.length > 0) {
        traiterSansColPivColRegValue(colVal1, regVal2)
      } else {
        traiterSansColPivColValue(colVal1)
      }
    } else if (colPiv1.length > 0) {
      if (regPiv2.length > 0 && regVal2.length > 0) {
        traiterColRegPivotsColregValue(colPiv1, regPiv2, colVal1, regVal2)
      } else if (regPiv2.length == 0 && regVal2.length > 0) {
        traiterColPivotsColRegValue(colPiv1, colVal1, regVal2)
      } else if (colVal1.length > 0 && regPiv2.length == 0 && regVal2.length == 0) {
        traiterColPivotsColValue(colPiv1, colVal1)
      } else if (colVal1.length > 0 && regPiv2.length > 0 && regVal2.length == 0) {
        traiterColRegPivotsColValue(colPiv1, regPiv2, colVal1)
      } else if (colVal1.length == 0 && regPiv2.length > 0) {
        traiterColRegPivotsSansValue(colPiv1, regPiv2)
      } else if (colVal1.length == 0 && regPiv2.length == 0) {
        traiterColPivotsSansValue(colPiv1)
      }
    }

  }

  private def analyzeByRegexp {
    var regPiv1 = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.numColRegexp1Pivot")
    var regPiv2 = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.regexp2Pivot")
    var regVal1 = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.numColRegexp1Value")
    var regVal2 = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.regexp2Value")

    if (regPiv1.length == 0 && regVal1.length == 0) {
      JOptionPane.showMessageDialog(null, "At least, the first regex for Pivot or  Value must be filled ")
    } else if (regPiv1.length == 0 && regVal1.length > 0) {
      // traitement de la valeur uniquement
      if (regVal2.length > 0) {
        traiterSansRegPiv2RegValue(regVal1, regVal2)
      } else {
        traiterSansReg1RegValue(regVal1)
      }
    } else if (regPiv1.length > 0) {
      if (regPiv2.length > 0 && regVal2.length > 0) {
        traiter2regPivots2regValue(regPiv1, regPiv2, regVal1, regVal2)
      } else if (regPiv2.length == 0 && regVal2.length > 0) {
        traiter1regPivots2regValue(regPiv1, regVal1, regVal2)
      } else if (regVal1.length > 0 && regPiv2.length == 0 && regVal2.length == 0) {
        traiter1regPivots1regValue(regPiv1, regVal1)
      } else if (regVal1.length > 0 && regPiv2.length > 0 && regVal2.length == 0) {
        traiter2regPivots1regValue(regPiv1, regPiv2, regVal1)
      } else if (regVal1.length == 0 && regPiv2.length > 0) {
        traiter1RegPivotsSansValue(regPiv1, regPiv2)
      } else if (regVal1.length == 0 && regPiv2.length == 0) {
        traiter1RegPivotsSansValue(regPiv1)
      }

    }
  }

  def traiter1RegPivotsSansValue(regPiv1: String) {

    //  println("traiter1RegPivotsSansValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr

      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg

      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var piv1 = regPiv1.r.findFirstIn(enr).orNull
            //  println(" FilesStatsActor.traiter2regPivots2regValue piv1 ="+piv1)
            if (null != piv1) {

              if (mapEnreg.contains(piv1)) {
                var cE = mapEnreg.get(piv1).get
                cE = cE.add(0, pas)
                mapEnreg.put(piv1, cE)
              } else {
                mapEnreg.put(piv1, new CumulEnregistrementStatMemory().add(0, pas))
              }
              if (mapEnreg.contains("Total")) {
                var cE = mapEnreg.get("Total").get
                cE = cE.add(0, pas)
                mapEnreg.put("Total", cE)
              } else {
                mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(0, pas))
              }

            }
          } catch {
            case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }

  }

  def traiter1RegPivotsSansValue(regPiv1: String, regPiv2: String) {
    // println("traiter1RegPivotsSansValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr

      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg

      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var piv1 = regPiv1.r.findFirstIn(enr).orNull
            //  println(" FilesStatsActor.traiter2regPivots2regValue piv1 ="+piv1)
            if (null != piv1) {
              var piv2 = regPiv2.r.findFirstIn(piv1).orNull
              if (piv2 != null) {
                if (mapEnreg.contains(piv2)) {
                  var cE = mapEnreg.get(piv2).get
                  cE = cE.add(0, pas)
                  mapEnreg.put(piv2, cE)
                } else {
                  mapEnreg.put(piv2, new CumulEnregistrementStatMemory().add(0, pas))
                }
                if (mapEnreg.contains("Total")) {
                  var cE = mapEnreg.get("Total").get
                  cE = cE.add(0, pas)
                  mapEnreg.put("Total", cE)
                } else {
                  mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(0, pas))
                }

              }
            }
          } catch {
            case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }
  }

  def traiterSansReg1RegValue(regVal1: String) {

    //  println("traiterSansReg1RegValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr

      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg

      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var val1 = regVal1.r.findFirstIn(enr).orNull
            if (null != val1) {

              if (mapEnreg.contains("WithoutPivot")) {
                var cE = mapEnreg.get("WithoutPivot").get
                cE = cE.add(val1.toDouble, pas)
                mapEnreg.put("WithoutPivot", cE)
              } else {
                mapEnreg.put("WithoutPivot", new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
              }
              if (mapEnreg.contains("Total")) {
                var cE = mapEnreg.get("Total").get
                cE = cE.add(val1.toDouble, pas)
                mapEnreg.put("Total", cE)
              } else {
                mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
              }

            }
          } catch {
            case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }

  }
  def traiterSansRegPiv2RegValue(regVal1: String, regVal2: String) {

    // println("traiterSansRegPiv2RegValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr

      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg

      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var val1 = regVal1.r.findFirstIn(enr).orNull
            if (null != val1) {
              var val2 = regVal2.r.findFirstIn(val1).orNull
              if (null != val2) {
                // on remplit la mat
                //  println("Piv2 ="+piv2+ " val2="+val2)
                if (mapEnreg.contains("WithoutPivot")) {
                  var cE = mapEnreg.get("WithoutPivot").get
                  cE = cE.add(val2.toDouble, pas)
                  mapEnreg.put("WithoutPivot", cE)
                } else {
                  mapEnreg.put("WithoutPivot", new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                }
                if (mapEnreg.contains("Total")) {
                  var cE = mapEnreg.get("Total").get
                  cE = cE.add(val2.toDouble, pas)
                  mapEnreg.put("Total", cE)
                } else {
                  mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                }

              }
            }
          } catch {
            case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }

  }

  def traiter2regPivots2regValue(regPiv1: String, regPiv2: String, regVal1: String, regVal2: String) {
    // println("traiter2regPivots2regValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr

      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg

      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            // println(" FilesStatsActor.traiter2regPivots2regValue enr ="+enr)
            var piv1 = regPiv1.r.findFirstIn(enr).orNull
            //  println(" FilesStatsActor.traiter2regPivots2regValue piv1 ="+piv1)
            if (null != piv1) {
              var piv2 = regPiv2.r.findFirstIn(piv1).orNull
              if (piv2 != null) {
                // traiter la valeur
                var val1 = regVal1.r.findFirstIn(enr).orNull
                if (null != val1) {
                  var val2 = regVal2.r.findFirstIn(val1).orNull
                  if (null != val2) {
                    // on remplit la mat
                    //  println("Piv2 ="+piv2+ " val2="+val2)
                    if (mapEnreg.contains(piv2)) {
                      var cE = mapEnreg.get(piv2).get
                      cE = cE.add(val2.toDouble, pas)
                      mapEnreg.put(piv2, cE)
                    } else {
                      mapEnreg.put(piv2, new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                    }
                    if (mapEnreg.contains("Total")) {
                      var cE = mapEnreg.get("Total").get
                      cE = cE.add(val2.toDouble, pas)
                      mapEnreg.put("Total", cE)
                    } else {
                      mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                    }
                  }
                }
              }
            }
          } catch {
             case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }
  }
  def traiter1regPivots2regValue(regPiv1: String, regVal1: String, regVal2: String) {
    // println("traiter1regPivots2regValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr

      if (enr == null) {
        bool = false
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg

      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {

            var piv1 = regPiv1.r.findFirstIn(enr).orNull
            if (null != piv1) {
              var val1 = regVal1.r.findFirstIn(enr).orNull
              if (null != val1) {

                var val2 = regVal2.r.findFirstIn(val1).orNull
                if (null != val2) {
                  // on remplit la mat
                  if (mapEnreg.contains(piv1)) {
                    var cE = mapEnreg.get(piv1).get
                    cE = cE.add(val2.toDouble, pas)
                    mapEnreg.put(piv1, cE)
                  } else {
                    mapEnreg.put(piv1, new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                  }
                  if (mapEnreg.contains("Total")) {
                    var cE = mapEnreg.get("Total").get
                    cE = cE.add(val2.toDouble, pas)
                    mapEnreg.put("Total", cE)
                  } else {
                    mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                  }
                }

              }
            }
          } catch {
            case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }
  }
  def traiter1regPivots1regValue(regPiv1: String, regVal1: String) {
    // println("traiter1regPivots1regValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr
      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg
      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var piv1 = regPiv1.r.findFirstIn(enr).orNull
            if (null != piv1) {
              var val1 = regVal1.r.findFirstIn(enr).orNull
              if (null != val1) {
                if (mapEnreg.contains(piv1)) {
                  var cE = mapEnreg.get(piv1).get
                  cE = cE.add(val1.toDouble, pas)
                  mapEnreg.put(piv1, cE)
                } else {
                  mapEnreg.put(piv1, new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
                }

                if (mapEnreg.contains("Total")) {
                  var cE = mapEnreg.get("Total").get
                  cE = cE.add(val1.toDouble, pas)
                  mapEnreg.put("Total", cE)
                } else {
                  mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
                }
              }
            }

          } catch {
             case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }
  }
  def traiter2regPivots1regValue(regPiv1: String, regPiv2: String, regVal1: String) {
    // println("traiter2regPivots1regValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr
      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg
      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var piv1 = regPiv1.r.findFirstIn(enr).orNull
            if (null != piv1) {
              var piv2 = regPiv2.r.findFirstIn(piv1).orNull
              if (piv2 != null) {
                var val1 = regVal1.r.findFirstIn(enr).orNull
                if (null != val1) {

                  if (mapEnreg.contains(piv2)) {
                    var cE = mapEnreg.get(piv2).get
                    cE = cE.add(val1.toDouble, pas)
                    mapEnreg.put(piv2, cE)
                  } else {
                    mapEnreg.put(piv2, new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
                  }
                  if (mapEnreg.contains("Total")) {
                    var cE = mapEnreg.get("Total").get
                    cE = cE.add(val1.toDouble, pas)
                    mapEnreg.put("Total", cE)
                  } else {
                    mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
                  }
                }

              }

            }
          } catch {
             case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }
  }

  def traiterColPivotsSansValue(colPiv1: String) {
    // println("traiterSansColPivColValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr
      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg
      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {

            var piv1 = enr.split(sep)(colPiv1.toInt)
            if (null != piv1) {

              if (mapEnreg.contains(piv1)) {
                var cE = mapEnreg.get(piv1).get
                cE = cE.add(0, pas)
                mapEnreg.put(piv1, cE)
              } else {
                mapEnreg.put(piv1, new CumulEnregistrementStatMemory().add(0, pas))
              }
              if (mapEnreg.contains("Total")) {
                var cE = mapEnreg.get("Total").get
                cE = cE.add(0, pas)
                mapEnreg.put("Total", cE)
              } else {
                mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(0, pas))
              }

            }
          } catch {
           case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }

  }

  def traiterColRegPivotsSansValue(colPiv1: String, regPiv2: String) {
    // println("traiterSansColPivColValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr
      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg
      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var piv1 = enr.split(sep)(colPiv1.toInt)
            if (null != piv1) {
              var piv2 = regPiv2.r.findFirstIn(piv1).orNull
              if (piv2 != null) {
                if (mapEnreg.contains(piv2)) {
                  var cE = mapEnreg.get(piv2).get
                  cE = cE.add(0, pas)
                  mapEnreg.put(piv2, cE)
                } else {
                  mapEnreg.put(piv2, new CumulEnregistrementStatMemory().add(0, pas))
                }
                if (mapEnreg.contains("Total")) {
                  var cE = mapEnreg.get("Total").get
                  cE = cE.add(0, pas)
                  mapEnreg.put("Total", cE)
                } else {
                  mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(0, pas))
                }

              }
            }
          } catch {
            case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }
  }

  def traiterSansColPivColValue(colVal1: String) {
    //println("traiterSansColPivColValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr
      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg
      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var val1 = enr.split(sep)(colVal1.toInt)
            if (null != val1) {
              if (mapEnreg.contains("WithoutPivot")) {
                var cE = mapEnreg.get("WithoutPivot").get
                cE = cE.add(val1.toDouble, pas)
                mapEnreg.put("WithoutPivot", cE)
              } else {
                mapEnreg.put("WithoutPivot", new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
              }
              if (mapEnreg.contains("Total")) {
                var cE = mapEnreg.get("Total").get
                cE = cE.add(val1.toDouble, pas)
                mapEnreg.put("Total", cE)
              } else {
                mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
              }
            }
          } catch {
             case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }

  }

  def traiterSansColPivColRegValue(colVal1: String, regVal2: String) {
    //  println("traiterSansColPivColRegValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr
      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg
      } else {

        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var val1 = enr.split(sep)(colVal1.toInt)
            if (null != val1) {
              var val2 = regVal2.r.findFirstIn(val1).orNull
              if (null != val2) {
                if (mapEnreg.contains("WithoutPivot")) {
                  var cE = mapEnreg.get("WithoutPivot").get
                  cE = cE.add(val2.toDouble, pas)
                  mapEnreg.put("WithoutPivot", cE)
                } else {
                  mapEnreg.put("WithoutPivot", new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                }
                if (mapEnreg.contains("Total")) {
                  var cE = mapEnreg.get("Total").get
                  cE = cE.add(val2.toDouble, pas)
                  mapEnreg.put("Total", cE)
                } else {
                  mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                }

              }

            }
          } catch {
           case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }

      }
    }
  }

  def traiterColRegPivotsColregValue(colPiv1: String, regPiv2: String, colVal1: String, regVal2: String) {
    //  println("traiterColRegPivotsColregValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr
      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg
      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var piv1 = enr.split(sep)(colPiv1.toInt)
            if (null != piv1) {
              var piv2 = regPiv2.r.findFirstIn(piv1).orNull
              if (piv2 != null) {
                var val1 = enr.split(sep)(colVal1.toInt)
                if (null != val1) {
                  var val2 = regVal2.r.findFirstIn(val1).orNull
                  if (null != val2) {

                    // on remplit la mat
                    if (mapEnreg.contains(piv2)) {
                      var cE = mapEnreg.get(piv2).get
                      cE = cE.add(val2.toDouble, pas)
                      mapEnreg.put(piv2, cE)
                    } else {
                      mapEnreg.put(piv2, new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                    }
                    if (mapEnreg.contains("Total")) {
                      var cE = mapEnreg.get("Total").get
                      cE = cE.add(val2.toDouble, pas)
                      mapEnreg.put("Total", cE)
                    } else {
                      mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                    }
                  }
                }
              }

            }
          } catch {
             case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }
  }

  def traiterColPivotsColRegValue(colPiv1: String, colVal1: String, regVal2: String) {
    // println("traiterColPivotsColRegValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr
      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg
      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var piv1 = enr.split(sep)(colPiv1.toInt)
            if (null != piv1) {
              var val1 = enr.split(sep)(colVal1.toInt)
              if (null != val1) {

                var val2 = regVal2.r.findFirstIn(val1).orNull
                if (null != val2) {

                  // on remplit la mat
                  if (mapEnreg.contains(piv1)) {
                    var cE = mapEnreg.get(piv1).get
                    cE = cE.add(val2.toDouble, pas)
                    mapEnreg.put(piv1, cE)
                  } else {
                    mapEnreg.put(piv1, new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                  }
                  if (mapEnreg.contains("Total")) {
                    var cE = mapEnreg.get("Total").get
                    cE = cE.add(val2.toDouble, pas)
                    mapEnreg.put("Total", cE)
                  } else {
                    mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val2.toDouble, pas))
                  }
                }

              }

            }
          } catch {
            case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }
  }

  def traiterColPivotsColValue(colPiv1: String, colVal1: String) {
    // println("traiterColPivotsColValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr
      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg
      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var piv1 = enr.split(sep)(colPiv1.toInt)
            if (null != piv1) {
              var val1 = enr.split(sep)(colVal1.toInt)
              if (null != val1) {

                // on remplit la mat
                if (mapEnreg.contains(piv1)) {
                  var cE = mapEnreg.get(piv1).get
                  cE = cE.add(val1.toDouble, pas)
                  mapEnreg.put(piv1, cE)
                } else {
                  mapEnreg.put(piv1, new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
                }
                if (mapEnreg.contains("Total")) {
                  var cE = mapEnreg.get("Total").get
                  cE = cE.add(val1.toDouble, pas)
                  mapEnreg.put("Total", cE)
                } else {
                  mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
                }
              }

            }
          } catch {
           case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }
  }

  def traiterColRegPivotsColValue(colPiv1: String, regPiv2: String, colVal1: String) {
    // println("traiterColRegPivotsColValue")
    var bool = true
    while (bool) {
      var enr: String = lireEnr
      if (enr == null) {
        bool = false
        // rajouter le Map Local au  ParserStatFile.tabHm
        MyDialogStatsFile.tabHm(msg.rang) = mapEnreg
      } else {

        //  Construire le Map Local
        if (!MyDialogStatsFile.isDatedFile || isBetweenDates(enr)) {
          try {
            var piv1 = enr.split(sep)(colPiv1.toInt)
            if (null != piv1) {
              var piv2 = regPiv2.r.findFirstIn(piv1).orNull
              if (piv2 != null) {
                var val1 = enr.split(sep)(colVal1.toInt)
                if (null != val1) {

                  // on remplit la mat
                  if (mapEnreg.contains(piv2)) {
                    var cE = mapEnreg.get(piv2).get
                    cE = cE.add(val1.toDouble, pas)
                    mapEnreg.put(piv2, cE)
                  } else {
                    mapEnreg.put(piv2, new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
                  }
                  if (mapEnreg.contains("Total")) {
                    var cE = mapEnreg.get("Total").get
                    cE = cE.add(val1.toDouble, pas)
                    mapEnreg.put("Total", cE)
                  } else {
                    mapEnreg.put("Total", new CumulEnregistrementStatMemory().add(val1.toDouble, pas))
                  }
                }

              }

            }
          } catch {
            case e: Throwable =>
              MyDialogStatsFile.ta.text += "\n an error occurs . Stop the parsing\n+"+e.getMessage()
              e.printStackTrace(); 
              bool=false
                MyDialogStatsFile.errorOccurs=true
                println()
              Thread.sleep(1000)
              context.stop(self)
          }
        }
      }
    }
  }

  def lireEnr(): String =
    {
      var cpt = 0
      var bool = true
      var ret: String = null

      while (bool) {
        var str = msg.buff.readLine

        if (str == null) {
          bool = false
          msg.buff.close

        } else if (str.length > 2) {
          if (cpt == msg.rang) {
            bool = false

            pasCount += 1
            nbEnrLocal += 1
            if (pasCount >= pasCountMax) {
              MyDialogStatsFile.nbEnrTraites.addAndGet(pasCountMax)
              //println("read"+nb)

              pasCount = 0
            }

            ret = str
            // on doit lire jusqu'à nbActor -1
            var bool2 = true
            while (bool2 && cpt < msg.nbActor - 1) {
              str = msg.buff.readLine
              if (str == null) {
                bool2 = false

              } else if (str.length > 2) {
                cpt += 1
              }

            }

          } else {
            cpt += 1
          }

        }

      }
      ret
    }

  def isBetweenDates(line: String): Boolean =
    {
      if (!MyDialogStatsFile.isDatedFile) {

        true
      } else {
        var ret = false

        if (formatDate.toLowerCase.contains("timein")) {
          var date = regDate.r.findFirstIn(line).orNull
          if (null == date) {
            //  println("refuted line="+line)
            return false
          } // traiter les cas timeIn
          else {
            // println("accepted line="+line)
            formatDate match {
              case "timeInMillis" =>
                //val dtMillis = new DateTime(date.toLong)
                val dtMillis = date.toLong
                if (dtMillis >= dateDeb && dtMillis <= dateFin) {
                  ret = true
                } else {
                  ret = false
                }
              case "timeInSecond" =>
                // val dtMillis = new DateTime(date.toLong * 1000)
                val dtMillis = date.toLong * 1000
                if (dtMillis >= dateDeb && dtMillis <= dateFin) {
                  ret = true
                } else {
                  ret = false
                }
              case "timeInSecondDotMillis" =>
                //  val dtMillis = new DateTime(date.split("\\.")(0).toLong * 1000 + (date.split("\\.")(1) + "000").substring(0, 3).toLong)
                val dtMillis = date.split("\\.")(0).toLong * 1000 + (date.split("\\.")(1) + "000").substring(0, 3).toLong
                if (dtMillis >= dateDeb && dtMillis <= dateFin) {
                  ret = true
                } else {
                  ret = false
                }
              case "timeInSecondCommaMillis" =>
                // val dtMillis = new DateTime(date.split("\\.")(0).toLong * 1000 + (date.split(",")(1) + "000").substring(0, 3).toLong)
                val dtMillis = date.split("\\.")(0).toLong * 1000 + (date.split(",")(1) + "000").substring(0, 3).toLong
                if (dtMillis >= dateDeb && dtMillis <= dateFin) {
                  ret = true
                } else {
                  ret = false
                }

            }
          }

        } else {
          // traiter les cas date formaté
          var date = regDate.r.findFirstIn(line).orNull
          if (null == date) {
            ret = false
          } else {

            //var dtMillis: DateTime = dtf.withLocale(MyDialogStatsFile.currentLocale).parseDateTime(date)
            var dtMillis = sdf.parse(date).getTime
            if (dtMillis >= dateDeb && dtMillis <= dateFin) {
              ret = true
            } else {
              ret = false
            }
          }
        }
        ret
      }
    }
}
