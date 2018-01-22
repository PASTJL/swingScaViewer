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
import java.util.regex.Pattern
import scala.util.matching.Regex
import scala.collection.mutable.HashMap
import java.util.Locale
import java.util.StringTokenizer
import java.lang.reflect.Method
import java.lang.reflect.InvocationTargetException
import java.text.SimpleDateFormat

class ParserActor(idActor: Int) extends Actor with ParsingModes {

  // Debut creation
  var nbCumulCall = 0
  val patFinNumber: Regex = "\\d+\\.?\\d*\\s*$".r
  private var indexMetric = 0
  private val stepAgg = ScaParserMain.props.getProperty("fileIn.stepAgg","1000").toLong

 // private val dateInputReg = ScaParserMain.dateInputReg

  private var pivCol: PivotColumn = null
  private var valCols: ValuesColumns = null

  var dateDebImplicit = ScaParserMain.dateIndice0
  var multTms = ScaParserMain.multTms
  private var strSigne = ""

  val isTimeInMillis = ScaParserMain.isTimeInMillis
  var typFilter = 0
 // val dateTimeFormatter = ScaParserMain.dateTimeFormatter
 var simpleDateFormat:SimpleDateFormat =null
  
   if (ScaParserMain.props.getProperty("fileIn.dateFormatIn", "") != "" &&  !ScaParserMain.props.getProperty("fileIn.dateFormatIn") .contains("dateInMillis")) {
     // dateTimeFormatter = DateTimeFormat.forPattern(props.getProperty("fileIn.dateFormatIn")).withLocale(currentLocaleIn)
      simpleDateFormat = new SimpleDateFormat(ScaParserMain.props.getProperty("fileIn.dateFormatIn"),ScaParserMain.currentLocaleIn)
    }
  private var countLocal = 0
  var diff = 0L
  private var dateEnreg: Long = 0L
  private var rangEnreg: Long = 0L

  private var cumulEnr: MyCumulEnregistrement = null

  private var nbMetrics = ScaParserMain.tabFilterPiv.length
  var pas = 1

  var nbVals = ScaParserMain.tabFilterVal.length
  var valsRet: Array[Double] = Array.ofDim(nbVals)
  private val pivotExhaustifParsing = ScaParserMain.pivotExhaustifParsing

  private var found = false
  // Fin creation
  def receive = {
    case lect: LectEnr =>
      ScaParserMain.correctDate match {
        case 0 => lect.withDate
        case _ => lect.withCorrectedDate(ScaParserMain.correctDate)
      }
      if (ScaParserMain.modeDebug) {
        ScaParserMain.logTrace.append("ScaParserMain.correctDate="+ScaParserMain.correctDate+" ;take=" + lect.take + " ;lect=" + lect.line + "\n")
      }
      if (lect.take) {
        countLocal += 1
        traiterEnr(lect)
        if (countLocal % ScaParserMain.gapinfo == 0) {
          ScaParserMain.compteurGlobalTreated.addAndGet(ScaParserMain.gapinfo)
        }

      }
    case "stop" =>
      ScaParserMain.tabBoolSolversStopped(idActor) = true
      ScaParserMain.compteurGlobalTreated.addAndGet(countLocal % ScaParserMain.gapinfo)
      System.out.println("ParserActor actor :"
        + idActor + " terminated");

      context.stop(self)
    // Todo
  }
  final def traiterEnr(lect: LectEnr) {

    // Nombre de threads tunes
    valsRet = Array.ofDim(nbVals)
    if (ScaParserMain.modeDebug && idActor == 0) {

      ScaParserMain.logTrace.
        append("ParserActor i="
          + idActor
          + " enr :"
          + lect.line + "\n")

      ScaParserMain.logTrace.
        append("ParserActor i="
          + idActor
          + "ParserActor.compteurActor0 ="
          + countLocal + "\n")
    }

    if (pivotExhaustifParsing) {
      //  println("Traitement pivot exhaustif")

      for (nb <- 0 until nbMetrics) {
        indexMetric = nb;
        traiterEnregistrement(lect)

      }
    } else {
      // println("Traitement pivot non exhaustif")
      for (nb <- 0 until nbMetrics; if (!found)) {
        indexMetric = nb;
        traiterEnregistrement(lect)

      }
    }
    found = false;

  }

  final private def traiterEnregistrement(enr: LectEnr) {
    if (ScaParserMain.modeDebug) {
      ScaParserMain.logTrace.append("traiterEnregistrement enr=" + enr.line + "\n")
    }
    // var diff = enr.dateInMillis - ScaParserMain.dateIndice0;

 //   println("traiterEnregistrement enr=" + enr.line + "enr.dateInMillis="+enr.dateInMillis+"\n")
    //  rangEnreg = diff / stepAgg
    rangEnreg = (enr.dateInMillis / stepAgg) * stepAgg
    if (ScaParserMain.modeDebug) {
      ScaParserMain.logTrace.append("traiterEnregistrement  rangEnreg =" + rangEnreg + "\n")
    }
   //  println("traiterEnregistrement  rangEnreg =" + rangEnreg + "\n")
    cumul(enr, rangEnreg);
  }
@specialized(Double, Long, Int)
  final private def cumul(enr: LectEnr, rang: Long) {

    //BooleanValuesGo ret = null;
    pivCol = ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).getPivCol()
    valCols = ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).getValCols()
    valsRet = filterOpt(enr, valCols, pivCol, ScaParserMain.tabFilterVal, ScaParserMain.tabFilterPiv(indexMetric))

    if (null != valsRet) {
      if (ScaParserMain.modeDebug) {
        nbCumulCall += 1
        ScaParserMain.logTrace.append("cumul : nbCumulCall=" + nbCumulCall + " ; traitement enr =" + enr.line + " ; rang=" + rang + " ; dateInMillis=" + enr.dateInMillis + "\n")
      }
      // tabCumul = ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr

      if (!ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.contains(rang)) {
        ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.
          put(
            rang,
            new MyCumulEnregistrement(ScaParserMain.tabStrFilesCsv(indexMetric), this.stepAgg, valCols.isDurations, nbVals).addValues(valsRet))
      } else {

        cumulEnr = ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.get(rang).get

        cumulEnr = cumulEnr.addValues(valsRet)

        ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.put(rang, cumulEnr);
      }
      // mise a jour des periodes anterieures quand elles existent
      // // pour le parallelisme
      //	System.out.println("cumul avant gestion periode");

      for (i <- 0 until nbVals) {
        if ( (!ScaParserMain.allAveragesOnly) && valCols.isDurations(i) == true
          && valsRet(i) > stepAgg.toDouble) {
         // System.out.println("Traitement value duree")

          var nbPeriods = (valsRet(i) / stepAgg).toInt
          if (ScaParserMain.modeDebug) {

            ScaParserMain.logTrace.append("cumul : valsRet(" + i + ")=" + valsRet(i) + "; nbPeriods =" + nbPeriods + "\n")
          }
          // log.log(Level.INFO,"ret.retour[i]="+ret.retour[i]+" ;period="+period+" ;nbPeriods="+nbPeriods);

          for (j <- 1 until nbPeriods) {
            if (ScaParserMain.isDebDate) { // A rajouter dans le choix TODO
              if (ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.contains((rang + j * stepAgg))) {
                var cEnr = ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.get((rang + j * stepAgg)).get

                cEnr.incrementCountParallel(i)

                ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.put(rang + j * stepAgg, cEnr)

              } else {

                // Creer enregistrement vide et incrementer le parallele
                var cEnr = new MyCumulEnregistrement(ScaParserMain.tabStrFilesCsv(indexMetric), stepAgg, valCols.isDurations, nbVals)
                cEnr.reInit
                cEnr.incrementCountParallel(i)
                ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.put((rang + j * stepAgg), cEnr)
              }
            } else {
              if (ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.contains((rang - j * stepAgg))) {
                var cEnr = ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.get((rang - j * stepAgg)).get

                cEnr.incrementCountParallel(i)

                ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.put((rang - j * stepAgg), cEnr)

              } else {

                // Creer enregistrement vide et incrementer le parallele
                var cEnr = new MyCumulEnregistrement(ScaParserMain.tabStrFilesCsv(indexMetric), stepAgg, valCols.isDurations, nbVals)
                  cEnr.reInit
                cEnr.incrementCountParallel(i)
                ScaParserMain.tabActorsFilesGenerated(idActor)(indexMetric).hmCumulEnr.put((rang - j * stepAgg), cEnr)
              }
            }

          }

        }
      }

    }

  }
  private final def filterOpt(enr: LectEnr, valCols: ValuesColumns, pivCol: PivotColumn, typFilterVal: Array[Int], typFilterPiv: Int): Array[Double] =
    {
      var retour: Array[Double] = Array.ofDim(nbVals)

      Locale.setDefault(ScaParserMain.currentLocaleIn)

      typFilterPiv match {

        case PIV_REGEXP1_SANSREGEXP2 =>
         // println("passage PIV_REGEXP1_SANSREGEXP2")
          retour = traiterPIV_REGEXP1_SANSREGEXP2(enr, valCols, typFilterVal)

        case PIV_REGEXP1_REGEXP2 =>
        //  println("passage PIV_REGEXP1_REGEXP2")
          retour = traiterPIV_REGEXP1_REGEXP2(enr, valCols, typFilterVal)

        case FASTPIV1STRING =>
         // println("passage FASTPIV1STRING")
          retour = traiterGoPIV1STRING(enr, valCols, typFilterVal)

        case SANSPIV =>
         // println("passage SANSPIV ")
          retour = traiterValuesOnly(enr, valCols, typFilterVal)
        // log.log(Level.INFO,"traitement sans PIV retour="+retour.retour[0]);

      }

      retour

    }

  final private def traiterPIV_REGEXP1_SANSREGEXP2(enr2: LectEnr, valCols: ValuesColumns, typFilterVal: Array[Int]): Array[Double] =
    {

      var ext1 = pivCol.ext1Piv.r.findFirstIn(enr2.line)

      if (None == ext1) {
        
        null
      } else
        // Sinon on passe aux valeurs
      //  System.out.println("OK PivCol="+pivCol.ext1Piv+" ; pour :"+enr2.line)
        traiterValuesOnly(enr2, valCols, typFilterVal);
    }

  final private def traiterPIV_REGEXP1_REGEXP2(enr2: LectEnr, valCols: ValuesColumns, typFilterVal: Array[Int]): Array[Double] =
    {

      var ext1 = pivCol.ext1Piv.r.findFirstIn(enr2.line)
      if (None == ext1) {

        null

      } else {
        var ext2 = pivCol.pat2Piv.findFirstIn(ext1.get)
        if (None == ext2) {

          null
        } else traiterValuesOnly(enr2, valCols, typFilterVal);
      }

      // Sinon on passe aux valeurs

    }
  final private def traiterGoPIV1STRING(enr2: LectEnr, valCols: ValuesColumns, typFilterVal: Array[Int]): Array[Double] =
    {
      var x = enr2.line.indexOf(pivCol.ext1Piv);
      // int x = so.searchChars(strEnr.toCharArray(), 0, pivCol.getNoColumn()
      // .toCharArray());
      // if (strEnr.contains(pivCol.getNoColumn())) {
      // x = 0;
      //
      // }

      if (x < 0) {

        null
      } else
        // Sinon on passe aux valeurs
        traiterValuesOnly(enr2, valCols, typFilterVal);
    }
  
  @specialized(Double, Long, Int)
  final private def traiterValuesOnly(enr2: LectEnr, valCols: ValuesColumns, typFilterVal: Array[Int]): Array[Double] =
    {

      var len = typFilterVal.length;
      var scale: Double = 1
      var noColVal = 0
      var str = "";
      var tabBool: Array[Boolean] = new Array(len)
      val valsRet: Array[Double] = Array.ofDim(nbVals)
      for (j <- 0 until len) tabBool(j) = false
      for (i <- 0 until nbVals) {
        valsRet(i) = Double.NaN
      }

      for (i <- 0 until len) {
        typFilterVal(i) match {

          case VAL_REGEXP1_SANSREGEXP2 =>

            scale = valCols.scaleValues(i).toDouble

            var ext1 = valCols.ext1Values(i).r.findFirstIn(enr2.line)

            if (None != ext1) {
              valsRet(i) = scale * ext1.get.toDouble

              if (indexMetric != 0)
                found = true;
            } else {
              valsRet(i) = Double.NaN

            }

          case VAL_REGEXP1_REGEXP2 =>

            scale = valCols.scaleValues(i).toDouble

            var ext1 = valCols.ext1Values(i).r.findFirstIn(enr2.line)

            if (None != ext1) {

              var ext2 = valCols.pat2Values(i).findFirstIn(ext1.get)

              if (None != ext2) {
                valsRet(i) = scale * ext2.get.toDouble
                if (indexMetric != 0)
                  found = true;
              } else {
                valsRet(i) = Double.NaN
              }
            } else {
              valsRet(i) = Double.NaN
            }

          case VAL_FUNCTION =>

            scale = valCols.scaleValues(i).toDouble
            var nomClasse: String = valCols.ext1Values(i).split("=")(1)
            var col2: String = valCols.pat2Values(i).toString
            // Le premier caractere est potentielement le separatur
            // s il est dans la liste " " "," ";"
            var sep = col2.substring(0, 1)
            var strTk: StringTokenizer = null;
            if (!sep.equals(" ") && !sep.equals(";") && !sep.equals(",")) {
              sep = " ";
              strTk = new StringTokenizer(col2, sep);
            } else {
              strTk = new StringTokenizer(col2.substring(1), sep);
            }
            var nbTokens = strTk.countTokens();
            var paramsMethod: Array[String] = new Array(nbTokens + 1)
            paramsMethod(0) = enr2.line
            for (j <- 0 until nbTokens) {
              paramsMethod(j + 1) = strTk.nextToken();
            }
            var met: Method = ScaParserMain.hmapMethod.get(nomClasse + "_" + i).get
            var obj: Object = ScaParserMain.hmapClass.get(nomClasse + "_" + i).get
            var ret: Double = Double.NaN
            try {
              // ret = met.invoke(obj, Array[Object](paramsMethod.asInstanceOf[Array[String]])).asInstanceOf[Double];
            // println("Nom_classe="+nomClasse + "_" + i)
              ret = met.invoke(obj, paramsMethod).asInstanceOf[Double];
            } catch {
              case e: IllegalArgumentException => e.printStackTrace()
              case e: IllegalAccessException => e.printStackTrace()
              case e: InvocationTargetException => // e.printStackTrace()
            }
            if (!ret.isNaN) {
              valsRet(i) = scale * ret

            }

        }

      }

      valsRet
    }
}