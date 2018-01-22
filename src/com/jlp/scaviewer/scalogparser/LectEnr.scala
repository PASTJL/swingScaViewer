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

import java.text.ParsePosition
import java.util.Date
import java.util.concurrent.atomic.AtomicLong
import java.text.SimpleDateFormat
import scala.util.matching.Regex

case class LectEnr(var line: String) extends ParsingModes {
  var dateInMillis: Long = 0L
  var localSimpleDateFormat: SimpleDateFormat = null
  var dateInputReg: Regex = null
  var reg2ForDateInMillis: Regex = null
  val decalTimeZone=ScaParserMain.decalTimeZone
  
  if (ScaParserMain.props.getProperty("fileIn.dateFormatIn", "") != "" && !ScaParserMain.props.getProperty("fileIn.dateFormatIn").contains("dateInMillis")) {
    // dateTimeFormatter = DateTimeFormat.forPattern(props.getProperty("fileIn.dateFormatIn")).withLocale(currentLocaleIn)
    localSimpleDateFormat = new SimpleDateFormat(ScaParserMain.props.getProperty("fileIn.dateFormatIn"), ScaParserMain.currentLocaleIn)
    dateInputReg = ScaParserMain.props.getProperty("fileIn.dateRegex").r
  } else if (ScaParserMain.props.getProperty("fileIn.dateFormatIn").contains("dateInMillis")) {
    val tabReg = ScaParserMain.props.getProperty("fileIn.dateRegex")
    if (tabReg.contains(" ")) {
      reg2ForDateInMillis = ScaParserMain.props.getProperty("fileIn.dateRegex").split(" ")(0).r
      dateInputReg = ScaParserMain.props.getProperty("fileIn.dateRegex").split(" ")(1).r
    } else {
      dateInputReg = ScaParserMain.props.getProperty("fileIn.dateRegex").r
      reg2ForDateInMillis = dateInputReg
    }
  }
  var take = true
  def withDate() {
    // Mettre a jour dateInMillis
    if (ScaParserMain.boolExplicit) {
      miseAjourDateExplicit()
    } else {
      miseAjourDateImplicit()
    }

  }
  def withCorrectedDate(mult: Long) {
    withDate
    if (!take) return
    if (mult != -1 && mult != 1)
      return
    else {
      // Chercher le gap en ms  a ajouter ou retrancher
      val vals = ScaParserMain.tabActorsFilesGenerated(0)(0).getValCols
      if (vals.isDurations(0)) // La premiere valeur est une duree accessible par regexp seulement sinon on sort
      {
        var gap = 0D
        ScaParserMain.tabFilterVal(0) match {
          case VAL_REGEXP1_SANSREGEXP2 =>
            var scale = vals.scaleValues(0).toDouble

            var ext1 = vals.ext1Values(0).r.findFirstIn(line)

            if (None != ext1) {
              var gap = scale * ext1.get.toDouble
              vals.unitValues(0) match {
                case "s" => gap = gap * 1000
                case "micros" => gap = gap / 1000
                case "nanos" => gap = gap / 1000000
                case _ =>
              }
              mult match {
                case -1 => dateInMillis += (-1) * gap.toLong
                case 1 => dateInMillis += gap.toLong
              }
              return

            } else {
              return

            }
          case VAL_REGEXP1_REGEXP2 =>
            var scale = vals.scaleValues(0).toDouble

            var ext1 = vals.ext1Values(0).r.findFirstIn(line)
            if (None != ext1) {
              var ext2 = vals.pat2Values(0).findFirstIn(ext1.get)
              if (None != ext1) {
                var gap = scale * ext2.get.toDouble
                vals.unitValues(0) match {
                  case "s" => gap = gap * 1000
                  case "micros" => gap = gap / 1000
                  case "nanos" => gap = gap / 1000000
                  case _ =>
                }
                mult match {
                  case -1 => dateInMillis += (-1) * gap.toLong
                  case 1 => dateInMillis += gap.toLong
                }
                return
              } else {
                return
              }
            } else {
              return
            }

          case _ => return

        }
      } else return

    }
    //
  }

  private def miseAjourDateExplicit() {

    // Extraction de la date avec la regexp + Joda Format
    if (ScaParserMain.isTimeInMillis) {
      if (ScaParserMain.modeDebug) {
        ScaParserMain.logTrace.append("LectEnr dateExplicit date InMillis line=" + line + "\n")
        ScaParserMain.logTrace.append("LectEnr dateExplicit date InMillis reg2ForDateInMillis=" + reg2ForDateInMillis + "\n")
        ScaParserMain.logTrace.append("LectEnr dateExplicit date InMillis dateInputReg=" + dateInputReg + "\n")
      }
      // extraction du long donnant les millissecondes � ajouter
      try {
        // var lg = reg2ForDateInMillis.findFirstIn(dateInputReg.findFirstIn(line).get).get
        if (ScaParserMain.modeDebug) {
          ScaParserMain.logTrace.append("LectEnr  reg2ForDateInMillis.findFirstIn(line).get=" + reg2ForDateInMillis.findFirstIn(line).get + "\n")
          ScaParserMain.logTrace.append("LectEnr  dateInputReg.findFirstIn(reg2ForDateInMillis.findFirstIn(line).get).get=" +
            dateInputReg.findFirstIn(reg2ForDateInMillis.findFirstIn(line).get).get + "\n")
        }
        var lg = dateInputReg.findFirstIn(reg2ForDateInMillis.findFirstIn(line).get)
      
        if (lg != None) {
         
          dateInMillis = ScaParserMain.dateIndice0 + (lg.get.replaceAll(",",".").toDouble * ScaParserMain.multTms).toLong
          	
           if (ScaParserMain.modeDebug) {
             
               ScaParserMain.logTrace.append("LectEnr  dateInMillis ="+dateInMillis)
              ScaParserMain.logTrace.append("LectEnr  ScaParserMain.dateStartParsingDate ="+ScaParserMain.dateStartParsingDate)
                ScaParserMain.logTrace.append("LectEnr  ScaParserMain.dateEndParsingDate ="+ScaParserMain.dateEndParsingDate)
           }
          if (ScaParserMain.controlDate) {
            if (ScaParserMain.dateStartParsingDate > dateInMillis || dateInMillis > ScaParserMain.dateEndParsingDate) {
              take = false
            }
          }
        } else take = false
      } catch {
        case e: IllegalArgumentException => take = false
        case _ :Throwable => take = false
      }
      if (ScaParserMain.modeDebug) { ScaParserMain.logTrace.append("LectEnr take=" + take + "\n") }
    } else {
      var tmp: Option[String] = Some("")

      tmp = dateInputReg.findFirstIn(line)

      if (tmp != None) {

        dateInMillis = localSimpleDateFormat.parse(tmp.get).getTime+decalTimeZone

        if (ScaParserMain.controlDate) {
          if (ScaParserMain.dateStartParsingDate > dateInMillis ||
            dateInMillis > ScaParserMain.dateEndParsingDate) {
            take = false
          }
        }
      } else {
        if (ScaParserMain.modeDebug) {
          ScaParserMain.logTrace.append(" None\n")
          //  ScaParserMain.logTrace.append("ScaParserMain.dateTimeFormatter=" + ScaParserMain.dateTimeFormatter.toString + "\n")
          ScaParserMain.logTrace.append("LectEnr dateExplicitlocal SimpleDateFormat=" + localSimpleDateFormat.toPattern + "\n")
        }
        take = false

      }
    }
  }
  private def miseAjourDateImplicit() {

    if (ScaParserMain.isStepWithValEquals) {
      //cas de l'extraction par val
      // on se sert de la date de lastImplicitDate
      //  println("MAJ Date implicit :="+ScaParserMain.lgStep)
      dateInMillis = LectEnr.lastImplicitDate + ScaParserMain.lgStep
      LectEnr.lastImplicitDate = dateInMillis
    } else {
      // cas de l'extraction par regex
      // on se sert de la date de depart dateIndice0
      var tmp = ScaParserMain.stepWithinEnreg.r.findFirstIn(line)
      if (tmp != None) {
        var tmp2 = ScaParserMain.reg2ForDateInMillis.findFirstIn(tmp.get)
        if (tmp2 != None) {
          dateInMillis = ScaParserMain.dateIndice0 + (tmp2.get.toDouble * ScaParserMain.multTms).toLong
          if (ScaParserMain.controlDate) {
            if (ScaParserMain.dateStartParsingDate > dateInMillis || dateInMillis > ScaParserMain.dateEndParsingDate) {
              take = false
            }
          }

        } else {
          take = false

        }
      } else {
        take = false
      }

    }

  }

}
object LectEnr {
  var lastImplicitDate: Long = ScaParserMain.dateIndice0
}