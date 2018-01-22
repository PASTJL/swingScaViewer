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
package com.jlp.scaviewer.timeseries

import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.immutable.SortedMap
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.data.time.TimeSeries
import java.util.Calendar
import org.jfree.data.time.Millisecond
import scala.math._
import scala.collection.JavaConverters._
import org.jfree.data.time.TimeSeriesDataItem
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Buffer
import com.jlp.scaviewer.commons.utils.Unites
import java.io.File
import com.jlp.scaviewer.ui.ScaCharting
import language.postfixOps

import com.jlp.scaviewer.csvutils.CsvFileInMemoryNew

case class TimeSeriesCollectionFromCsvInMemoryNew(csvFileInMemory: CsvFileInMemoryNew) {

  var arrLines: ArrayBuffer[(String, ArrayBuffer[String])] = csvFileInMemory.pivotsArrayValues
  //println(" TimeSeriesCollectionFromCsvInMemory arrLines="+arrLines)
  val titleColums: ArrayBuffer[String] = new ArrayBuffer() ++ csvFileInMemory.firstLine.split(csvFileInMemory.sepCourant)
  val idxValues = csvFileInMemory.values

  val titleColumsValues = {
    var tmp = new ArrayBuffer[(String, Int)](idxValues.length)
    for (i <- 0 until idxValues.length) {
      tmp += ((titleColums(idxValues(i)), i))
    }
    tmp
  }

  val sepTs = CsvFileInMemoryNew.scaChartProps.getProperty("timeseries.nameTimeSeries.separator")
  //arrTimeSeries est le champ important
  var arrTimesSeries = new ArrayBuffer[(TimeSeries, ArrayBuffer[Double])]()
  // le nom de la timeseries est <nom_pivot ou "global>_<titre_de_la_colonne>

  //on va enrichir cette structure en rajoutant le nom du pivot le nom de la colonne pour faciliter les recherche ( a extraire du nom de la serie) et le nom de lunite
  var enrichedArrTimeSeries = new ArrayBuffer[StructTs]();

  def addEnenrichedArrTimeSeries(that: TimeSeriesCollectionFromCsvInMemoryNew): ArrayBuffer[StructTs] = {
    enrichedArrTimeSeries ++ that.enrichedArrTimeSeries
  }

  def supprimerunite(str: String): String =
    {

      var ret = str.trim
      var pat1 = "\\(.*\\)".r
      pat1 findFirstIn str match {
        case Some(v) =>
          ret = str.substring(0, str.indexOf(v)).trim
        case None =>
      }

      ret
    }

  def trouverUnite(str: String): String =
    {
      var ret = "unit" // unit� par defaut

      // chercher des parantenses dans le string
      var pat = "\\([\\w=\\*/\\s]+\\)".r
      pat findFirstIn str match {
        case Some(v) =>
          // partir de la parenthese fermee et trouver les caracteres des unites geree
          // TODO
          var pat2 = "[a-zA-Z\\s]+/*\\**[a-zA-Z\\s]*\\)$".r
          pat2 findFirstIn v match {
            case Some(w) =>

              ret = w.substring(0, w.indexOf(")")).trim
              if (!Unites.isCorrectUnit(ret)) {
                ret = "unit"
              }
            case None => ret = "unit"
          }

        case None => ret = "unit"
      }

      ret
    }

  def enrichArrTimeSeries {
    // println("enrichArrTimeSeries demarrage : length=" + enrichedArrTimeSeries.length)
    if (!arrTimesSeries.isEmpty) {
      for (arr <- arrTimesSeries.par) {

        var str = trouverUnite(arr._1.getKey.asInstanceOf[String].split(sepTs)(1))
        enrichedArrTimeSeries += new StructTs(arr._1, (arr._1).getKey.asInstanceOf[String].split(sepTs)(0), arr._1.getKey.asInstanceOf[String].split(sepTs)(1),
          trouverUnite((arr._1).getKey.asInstanceOf[String]), GroupUnit.retrouverGroup((trouverUnite((arr._1).getKey.asInstanceOf[String].trim))), arr._2, csvFileInMemory.name)
      }

    }
  }

  var step: Long = 1000

  def retrieveAllColumnsValues() {}

  def timeGap(line1: String, line2: String) =
    {
      csvFileInMemory.dateInMillis(line2) - csvFileInMemory.dateInMillis(line1)
    }

  def trouverIndex(arrB: ArrayBuffer[String], dateString: String): Int =
    {
      var i: Int = 0
      if (csvFileInMemory.inf(dateString, arrB(i).split(csvFileInMemory.sepCourant)(0))) {
        //println("sortie directe i="+i)
        0
      } else {
        var len = arrB.length
        while (i < len - 1 && csvFileInMemory.inf(arrB(i).split(csvFileInMemory.sepCourant)(0), dateString)) {
          //        println("dateString="+dateString+" ;longDateString="+csvFile.dateInMillis(dateString)+" longdate="+csvFile.dateInMillis(arrB(i).split(csvFile.sepCourant)(0))+" ;bool="
          //            +(csvFile.inf(arrB(i).split(csvFile.sepCourant)(0),dateString))+" ;arrB(i).split(csvFile.sepCourant)(0)="+arrB(i).split(csvFile.sepCourant)(0))
          i += 1
        }
        // println("len="+len+" ;sortie i="+i+" ;dateString="+dateString+"bool="+(csvFile.inf(arrB(i).split(csvFile.sepCourant)(0),dateString))+" ;arrB(i).split(csvFile.sepCourant)(0)="+arrB(i).split(csvFile.sepCourant)(0))
        i
      }
    }

  @specialized(Double, Long, Int)
  def createAllTimeSeries(boolTimeSeries: Boolean = true, boolNbPointMax: Boolean = true, nbPointsMaxOrTimeGap: Long, dateDeb: String, dateFin: String, strategie: String): TimeSeriesCollectionFromCsvInMemoryNew = {

    def extractMapWithPas(arrlns: ArrayBuffer[String]): SortedMap[Long, ArrayBuffer[String]] =
      {
        val debIndex = trouverIndex(arrlns, dateDeb)
        val finIndex = trouverIndex(arrlns, dateFin)
        //  val gap = timeGap(arrlns(debIndex), arrlns(finIndex))

        //  var mewArrayBuffer = arrLines.slice(debIndex, finIndex)
        // Cas ou le nombre de points voulue est superieur au nombre de mesures disponible
        if (debIndex != finIndex) {
          ScaCharting.listPasInMillis = nbPointsMaxOrTimeGap :: ScaCharting.listPasInMillis
          SortedMap[Long, ArrayBuffer[String]]() ++ (arrlns slice (debIndex, finIndex + 1) groupBy { line: String => (csvFileInMemory.dateInMillis(line) / nbPointsMaxOrTimeGap) * nbPointsMaxOrTimeGap })
        } else {
          ScaCharting.listPasInMillis = nbPointsMaxOrTimeGap :: ScaCharting.listPasInMillis
          //  println("TimeSeriesCollectionFrom Csv fin construction map duree="+(System.currentTimeMillis()-deb) )
          SortedMap[Long, ArrayBuffer[String]]()
        }

      }
    def extractMap(arrlns: ArrayBuffer[String]): SortedMap[Long, ArrayBuffer[String]] =
      {
        //      println( "extractMap: size="+arrlns.size)
        //       println( "extractMap:arrlns="+arrlns)
        //      println("dateDeb="+dateDeb)
        //       println("dateFin="+dateFin)
        val debIndex = trouverIndex(arrlns, dateDeb)
        //         println("debIndex="+debIndex)
        val finIndex = trouverIndex(arrlns, dateFin)

        //      println("finIndex="+finIndex)
        val gap = timeGap(arrlns(debIndex), arrlns(finIndex))
        var nbPointsVoulus = 0

        var mewArrayBuffer = arrlns.slice(debIndex, finIndex + 1)
        // Cas ou le nombre de points voulue est superieur au nombre de mesures disponible
        if (nbPointsMaxOrTimeGap >= finIndex - debIndex) {
          //       	println("sans groupage length="+(mewArrayBuffer groupBy { line: String => csvFile.dateInMillis(line.split(csvFile.sepCourant)(0)) }).size)
          if (finIndex != debIndex) {
            ScaCharting.listPasInMillis = ScaCharting.listPasInMillis :+ (gap / (finIndex - debIndex).toLong)
            SortedMap[Long, ArrayBuffer[String]]() ++ (mewArrayBuffer groupBy { line: String => csvFileInMemory.dateInMillis(line) })
          } else {
            ScaCharting.listPasInMillis = ScaCharting.listPasInMillis :+ (0L)
            SortedMap[Long, ArrayBuffer[String]]()
          }

          //  

        } else {
          // group by a faire
          // calcul du pas
          //         println("avec groupage dateDeb="+arrlns(debIndex).split(csvFile.sepCourant)(0))
          //          println("avec groupage dateFin="+arrlns(finIndex).split(csvFile.sepCourant)(0))
          val dateDebInMillis = csvFileInMemory.dateInMillis(arrlns(debIndex).split(csvFileInMemory.sepCourant)(0));
          val dateFinInMillis = csvFileInMemory.dateInMillis(arrlns(finIndex).split(csvFileInMemory.sepCourant)(0))
          // println("avec groupage diffdtae="+(dateFinInMillis-dateDebInMillis))
          val pas: Long = (dateFinInMillis - dateDebInMillis) / nbPointsMaxOrTimeGap
          ScaCharting.listPasInMillis = ScaCharting.listPasInMillis :+ pas
          //  println("avec groupage"+" pas="+pas+" length="+(mewArrayBuffer groupBy { line: String => csvFile.dateInMillis(line.split(csvFile.sepCourant)(0)) }).keys.size)
          SortedMap[Long, ArrayBuffer[String]]() ++ (mewArrayBuffer groupBy { line: String => (csvFileInMemory.dateInMillis(line) / pas) * pas })

        }

      }

    def createArraySortedMap(): ArrayBuffer[(String, SortedMap[Long, ArrayBuffer[String]])] =
      {
        var sortedMaps: ArrayBuffer[(String, SortedMap[Long, ArrayBuffer[String]])] = new ArrayBuffer()
        for (arr <- arrLines) {
          // println("createArraySortedMap :"+(arr _1)+" -> size= "+((arr _2 ) length))
          // println((arr _2 ))

          if (boolTimeSeries) {
            if (boolNbPointMax) {
              sortedMaps += ((arr._1, extractMap(arr._2)))
            } else {
              sortedMaps += ((arr._1, extractMapWithPas(arr._2)))
            }

          } else {
            //TO DO 
          }

        }
        sortedMaps
      }

    val sorted = createArraySortedMap
    for (sm <- sorted.par) {
      // sm._1 est le nom du pivot
      // sm._2 est une sortedMap dont la cl� est le temps en milliseconde et la valeur untableau de String (ligne de csvFile regroup�es par groupBy)
      arrTimesSeries ++= arrTimesSeries ++ createTimeSeriesWithOutPivots(sm._2, sm._1, strategie, nbPointsMaxOrTimeGap)

    }
    // for (item <- arrTimesSeries) { println("Timesesrie =" + item._1.getKey + " => value avgPond=" + item._2(0)) }
    enrichArrTimeSeries

    this
    //  for(ts <- arrTimesSeries )
    //      println("Ts createad ="+(ts._1.getKey) +" size="+ts._1.getTimePeriods.size)
  }

  @specialized(Double, Long)
  def createTimeSeriesWithOutPivots(sortedMap: SortedMap[Long, ArrayBuffer[String]], prefix: String, strategie: String, nbPointsMaxOrTimeGap: Long): ArrayBuffer[(TimeSeries, ArrayBuffer[Double])] =
    {
      @specialized(Double, Long, Int)
      def createTuples4(tup: Tuple2[Long, ArrayBuffer[String]]): ArrayBuffer[(Long, Double, Int, Double)] =
        {
          // The tuple4 constaints (TimeInMillis, valueOfAgglo,nbOfValuesAgg,maxOfAgglo)
          // println("Tupl2="+tup)
          var list = ArrayBuffer[(Long, Double, Int, Double)]()

          //println("tup=" + tup)
          // println("csvFile.values=" + csvFile.values)
          for (i <- csvFileInMemory.values) {
            var count: Int = 0
            var sum = 0.0
            var xMin: Double = Double.MaxValue
            var xMax: Double = Double.MinValue
            var xMaxMax: Double = Double.MinValue

            var percentValue: ArrayBuffer[Double] = new ArrayBuffer[Double]()
            for (strs <- (tup _2)) {

              val arrayValue = strs.split(csvFileInMemory.sepCourant)

              if (arrayValue.isDefinedAt(i) && arrayValue(i).length > 0) {

                strategie match {
                  case "AVERAGE" => {
                    sum += arrayValue(i).toDouble
                    count += 1

                  }
                  case "MAX" =>
                    {
                      count = 1
                      if (arrayValue(i).toDouble > xMax) {
                        xMax = arrayValue(i).toDouble
                        sum = xMax
                      }
                    }

                  case "MIN" =>
                    {
                      count = 1
                      if (arrayValue(i).toDouble < xMin) {
                        xMin = arrayValue(i).toDouble
                        sum = xMin
                      }
                    }
                  case "MEDIANE" | "PERCENTILE90" =>
                    {
                      count += 1
                      percentValue += arrayValue(i).toDouble

                    }
                  case "SUM" =>
                    {
                      count = 1
                      sum += arrayValue(i).toDouble

                    }

                }
                xMaxMax = math.max(arrayValue(i).toDouble, xMaxMax)
                // println(" arrayValue("+i+")=|"+ arrayValue(i)+"|")
              }
            }

            if (count > 0 && strategie != "MEDIANE" && strategie != "PERCENTILE90" && strategie != "SUM") {
              var tuple = (tup _1, sum / count, count, xMaxMax)
              list += tuple

            } else if (count > 0 && strategie == "MEDIANE") {
              if (count == 1) list += ((tup _1, percentValue(0), 1, xMaxMax))
              else
                list += ((tup _1, (percentValue sortWith (_ < _))(count / 2), 1, xMaxMax))

            } else if (count > 0 && strategie == "PERCENTILE90") {
              if (count == 1) list += ((tup _1, percentValue(0), 1, xMaxMax))

              else
                list += ((tup _1, (percentValue sortWith (_ < _))((count * 9 / 10) - 1), 1, xMaxMax))

            } else if (count > 0 && strategie == "SUM") {
              list += ((tup _1, sum, 1, sum))

            } else
              list += ((tup _1, 0.0, 0, 0.0))
            count = 0
            sum = 0.0
            xMin = Double.MaxValue
            xMax = Double.MinValue
            xMaxMax = Double.MinValue
            percentValue = new ArrayBuffer[Double]()
          }
          // println("list.length=" + list.length)
          list
        }

      //   println("sortedMap="+sortedMap)
      val arrBuff = new ArrayBuffer[(TimeSeries, ArrayBuffer[Double])]()
      // Le tableau ArrayBuffer[Double] va contenir :
      // la moyenne ponderee du TimeSeries
      // la moyenne des agglomerations
      // le min de la collection
      // le max de la collection
      // le nombre total de mesure (count)
      // le nombre d'agglomeration
      // la deviation standard ( Ecart Type)
      // la pente par la methode des moindre carr�s
      //
      val idxValue = csvFileInMemory.values
      var arrTs = new ArrayBuffer[TimeSeries]()
      for (i <- idxValue) {
        //  arrTs += new TimeSeries((csvFile.shortname + "_" + strategie + nbPointsMaxOrTimeGap.toString() + "_" + prefix + sepTs + titleColums(i)).replaceAll(" ", ""))
        //  arrTs += new TimeSeries((csvFileInMemory.name + "_" + strategie + nbPointsMaxOrTimeGap.toString() + "_" + prefix + sepTs + titleColums(i)).replaceAll(" ", ""))
        println("key="+(csvFileInMemory.name + "_" + strategie + "_" + prefix + sepTs + titleColums(i)).replaceAll(" ", ""))
        arrTs += new TimeSeries((csvFileInMemory.name + "_" + strategie + "_" + prefix + sepTs + titleColums(i)).replaceAll(" ", ""))
      }
      //  arrTs foreach ((arg: TimeSeries) => println(arg.getKey))
      var arrGlobalValue: ArrayBuffer[Double] = new ArrayBuffer[Double](arrTs.size)
      // creation d'une structure SortedMap[Long,BufferArray[Tuple5] pour chaque cl� du SortedMap en entree
      // println("sortedMap=" + sortedMap)
      var myListTuple = sortedMap map createTuples4
      //println("myListTuple=" + myListTuple)
      //  //println("myListTuple.size=" + myListTuple.size)
      strategie match {
        case "AVERAGE" => {
          // TODO
          //  println("case Average")
          var cal: Calendar = Calendar.getInstance
          var i = 0
          for (ts <- arrTs) {
            var sumPond: Double = 0
            var sum: Double = 0
            var countAll: Long = 0
            var countVal: Long = 0

            var moyenneX: Double = 0
            var sommeXCarre: Double = 0
            var prodXY: Double = 0
            var sommeX: Double = 0
            var xmin = Double.MaxValue
            var xmax = Double.MinValue
            var xMaxMax = Double.MinValue
            val decalX: Long = myListTuple.head(0)._1
            for (lstVal <- myListTuple) {

              cal.setTimeInMillis(lstVal(i)._1)
              if ((lstVal(i) _3) > 0) {

                //  println("lstVal=" + lstVal)

                // ts.add(new Millisecond(cal.getTime), lstVal(i)._2)
                ts.add(MyTimeSeriesItem(new Millisecond(cal.getTime), lstVal(i)._2, (lstVal(i) _3), (lstVal(i) _4)))
                sumPond += lstVal(i)._2 * lstVal(i)._3
                countAll += lstVal(i)._3
                countVal += 1
                sommeX += lstVal(i)._1 - decalX
                sum += lstVal(i)._2
                sommeXCarre += pow(lstVal(i)._1 - decalX, 2)
                prodXY += (lstVal(i)._1 - decalX) * lstVal(i)._2
                xmax = scala.math.max(xmax, lstVal(i) _2)
                xmin = scala.math.min(xmin, lstVal(i) _2)
                xMaxMax = scala.math.max(xMaxMax, lstVal(i) _4)
              }
            }
            // println("sumPond=" + sumPond)
            //println("CountAll=" + countAll)
            var avgPond: Double = sumPond / countAll
            var avg = sum / countVal
            var stdDev: Double = 0
            var sumCarre: Double = 0
            moyenneX = sommeX / countVal

            var varX = (sommeXCarre / countVal) - pow(moyenneX, 2)
            var coVarXY = (prodXY / countVal) - moyenneX * avg
            var irslope = coVarXY / varX

            import scala.collection.JavaConversions._

           // var lst: Buffer[TimeSeriesDataItem] = asScalaBuffer(ts.getItems.asInstanceOf[java.util.List[TimeSeriesDataItem]])
    var lst = ts.getItems.asInstanceOf[List[TimeSeriesDataItem]]

            for (value <- lst) {
              sumCarre += pow((value.getValue.doubleValue - avg), 2)
              // println(ts.getKey + " value=" + value.getValue.doubleValue)
            }
            stdDev = sqrt(sumCarre / (countVal - 1))
            //   println("fin Average de " + ts.getKey + " ; stdDev=" + stdDev + " ;countVal=" + countVal + " ;countAll=" + countAll + " ;irslope=" + irslope + " ;avgPond=" + avgPond + " ;avg=" + avg + " ; min=" + xmin + " ;max=" + xmax)

            if (ts.getItemCount() > 0) {
              var tmpTab = new ArrayBuffer[Double]();
              tmpTab.asJava.add(avgPond)
              tmpTab.asJava.add(avg)
              tmpTab.asJava.add(xmin)
              tmpTab.asJava.add(xmax)
              tmpTab.asJava.add(countAll)
              tmpTab.asJava.add(countVal)
              tmpTab.asJava.add(stdDev)
              tmpTab.asJava.add(irslope)
              tmpTab.asJava.add(sumPond)
              tmpTab.asJava.add(sum)
              tmpTab.asJava.add(xMaxMax)
              arrBuff.asJava.add((ts, tmpTab))
            }
            i += 1
          }
        }
        case _ => {

          var cal: Calendar = Calendar.getInstance
          var i = 0
          for (ts <- arrTs) {
            var sumPond: Double = 0
            var sum: Double = 0
            var countAll: Long = 0
            var countVal: Long = 0

            var moyenneX: Double = 0
            var sommeXCarre: Double = 0
            var prodXY: Double = 0
            var sommeX: Double = 0
            var min = Double.MaxValue
            var max = Double.MinValue
            var xMaxMax = Double.MinValue
            val decalX: Long = myListTuple.head(0)._1
            for (lstVal <- myListTuple) {
              cal.setTimeInMillis(lstVal(i)._1)
              if (lstVal(i)._3 > 0) {
                // ts.add(new Millisecond(cal.getTime), lstVal(i)._2)
                ts.add(MyTimeSeriesItem(new Millisecond(cal.getTime), lstVal(i)._2, 1, lstVal(i)._4))
                sumPond += lstVal(i)._2
                countAll += 1
                countVal += 1
                sommeX += lstVal(i)._1 - decalX
                sum += lstVal(i)._2
                sommeXCarre += pow(lstVal(i)._1 - decalX, 2)
                prodXY += (lstVal(i)._1 - decalX) * lstVal(i)._2
                max = scala.math.max(max, lstVal(i) _2)
                min = scala.math.min(min, lstVal(i) _2)
                xMaxMax = scala.math.max(xMaxMax, lstVal(i) _4)
              }
            }

            var avgPond: Double = sumPond / countAll
            var avg = sum / countVal
            var stdDev: Double = 0
            var sumCarre: Double = 0
            moyenneX = sommeX / countVal

            var varX = (sommeXCarre / countVal) - pow(moyenneX, 2)
            var coVarXY = (prodXY / countVal) - moyenneX * avg
            var irslope = coVarXY / varX

            var lst: Buffer[TimeSeriesDataItem] = asScalaBuffer(ts.getItems.asInstanceOf[java.util.List[TimeSeriesDataItem]])

            for (value <- lst) {
              sumCarre += pow((value.getValue.doubleValue - avg), 2)
              //  println(ts.getKey + " value=" + value.getValue.doubleValue)
            }
            stdDev = sqrt(sumCarre / (countVal - 1))
            // println("fin MAX de " + ts.getKey + " stdDev=" + stdDev + " ;countVal=" + countVal + " ;countAll=" + countAll + " ;irslope=" + irslope + " ;avgPond=" + avgPond + " ;avg=" + avg)
            var tmpTab = new ArrayBuffer[Double]();
            if (ts.getItemCount() > 0) {
              tmpTab.asJava.add(avgPond)
              tmpTab.asJava.add(avg)
              tmpTab.asJava.add(min)
              tmpTab.asJava.add(max)
              tmpTab.asJava.add(countAll)
              tmpTab.asJava.add(countVal)
              tmpTab.asJava.add(stdDev)
              tmpTab.asJava.add(irslope)

              tmpTab.asJava.add(sumPond)
              tmpTab.asJava.add(sum)
              tmpTab.asJava.add(xMaxMax)
              arrBuff.asJava.add((ts, tmpTab))
            }
            i += 1
          }
        }

      }

      arrBuff
    }

  @specialized(Double, Long, Int)
  def createAllTimeSeriesOnly(boolTimeSeries: Boolean = true, boolNbPointMax: Boolean = true, nbPointsMaxOrTimeGap: Long, dateDeb: String, dateFin: String, strategie: String): TimeSeriesCollectionFromCsvInMemoryNew = {

    def extractMapWithPas(arrlns: ArrayBuffer[String]): SortedMap[Long, ArrayBuffer[String]] =
      {
        val debIndex = trouverIndex(arrlns, dateDeb)
        val finIndex = trouverIndex(arrlns, dateFin)
        //  val gap = timeGap(arrlns(debIndex), arrlns(finIndex))
        // println("TimeSeriesCollectionFromCsvInMemoryextractMapWithPas debIndex="+debIndex+" finIndex="+finIndex)
        val map = SortedMap[Long, ArrayBuffer[String]]()

        //  var mewArrayBuffer = arrLines.slice(debIndex, finIndex)
        // Cas ou le nombre de points voulue est superieur au nombre de mesures disponible
        if (debIndex != finIndex && nbPointsMaxOrTimeGap>0 ) {
          ScaCharting.listPasInMillis = nbPointsMaxOrTimeGap :: ScaCharting.listPasInMillis
          map ++ (arrlns slice (debIndex, finIndex + 1) groupBy { line: String => (csvFileInMemory.dateInMillis(line) / nbPointsMaxOrTimeGap) * nbPointsMaxOrTimeGap })
        } else {
          ScaCharting.listPasInMillis = nbPointsMaxOrTimeGap :: ScaCharting.listPasInMillis
          //  println("TimeSeriesCollectionFrom Csv fin construction map duree="+(System.currentTimeMillis()-deb) )
          map
        }

      }
    def extractMap(arrlns: ArrayBuffer[String]): SortedMap[Long, ArrayBuffer[String]] =
      {
        //      println( "extractMap: size="+arrlns.size)
        //       println( "extractMap:arrlns="+arrlns)
        //      println("dateDeb="+dateDeb)
        //       println("dateFin="+dateFin)
        val debIndex = trouverIndex(arrlns, dateDeb)
        //         println("debIndex="+debIndex)
        val finIndex = trouverIndex(arrlns, dateFin)

        // println("TimeSeriesCollectionFromCsvInMemory extractMap debIndex="+debIndex+" finIndex="+finIndex)
        //      println("finIndex="+finIndex)
        val gap = timeGap(arrlns(debIndex), arrlns(finIndex))
        var nbPointsVoulus = 0
        val map = SortedMap[Long, ArrayBuffer[String]]()
        var mewArrayBuffer = arrlns.slice(debIndex, finIndex + 1)
        // Cas ou le nombre de points voulue est superieur au nombre de mesures disponible
        if (nbPointsMaxOrTimeGap >= finIndex - debIndex) {
          //       	println("sans groupage length="+(mewArrayBuffer groupBy { line: String => csvFile.dateInMillis(line.split(csvFile.sepCourant)(0)) }).size)

          if (finIndex != debIndex) {
            ScaCharting.listPasInMillis = ScaCharting.listPasInMillis :+ (gap / (finIndex - debIndex).toLong)
            map ++ (mewArrayBuffer groupBy { line: String => csvFileInMemory.dateInMillis(line) })
          } else {
            ScaCharting.listPasInMillis = ScaCharting.listPasInMillis :+ (0L)
            map
          }

          //  

        } else {
          // group by a faire
          // calcul du pas
          //         println("avec groupage dateDeb="+arrlns(debIndex).split(csvFile.sepCourant)(0))
          //          println("avec groupage dateFin="+arrlns(finIndex).split(csvFile.sepCourant)(0))
          val dateDebInMillis = csvFileInMemory.dateInMillis(arrlns(debIndex).split(csvFileInMemory.sepCourant)(0));
          val dateFinInMillis = csvFileInMemory.dateInMillis(arrlns(finIndex).split(csvFileInMemory.sepCourant)(0))
          // println("avec groupage diffdtae="+(dateFinInMillis-dateDebInMillis))
          val pas: Long = (dateFinInMillis - dateDebInMillis) / nbPointsMaxOrTimeGap
          ScaCharting.listPasInMillis = ScaCharting.listPasInMillis :+ pas
          //  println("avec groupage"+" pas="+pas+" length="+(mewArrayBuffer groupBy { line: String => csvFile.dateInMillis(line.split(csvFile.sepCourant)(0)) }).keys.size)
          map ++ (mewArrayBuffer groupBy { line: String => (csvFileInMemory.dateInMillis(line) / pas) * pas })

        }

      }

    def createArraySortedMap(): ArrayBuffer[(String, SortedMap[Long, ArrayBuffer[String]])] =
      {
        var sortedMaps: ArrayBuffer[(String, SortedMap[Long, ArrayBuffer[String]])] = new ArrayBuffer()
        //  println("TimeSeriesCollectionFromCsvInMemory  arrLines in sortedMaps="+  arrLines)
        for (arr <- arrLines) {
          // println("createArraySortedMap :"+(arr _1)+" -> size= "+((arr _2 ) length))
          // println((arr _2 ))

          if (boolTimeSeries) {
            if (boolNbPointMax) {
              sortedMaps += ((arr._1, extractMap(arr._2)))
            } else {
              sortedMaps += ((arr._1, extractMapWithPas(arr._2)))
            }

          } else {
            //TO DO 
          }

        }
        //  println("TimeSeriesCollectionFromCsvInMemory sortedMaps="+ sortedMaps )
        sortedMaps
      }

    for (sm <- createArraySortedMap) {
      // sm._1 est le nom du pivot
      // sm._2 est une sortedMap dont la cl� est le temps en milliseconde et la valeur untableau de String (ligne de csvFile regroup�es par groupBy)
      arrTimesSeries = arrTimesSeries ++ createTimeSeriesOnlyWithOutPivots(sm._2, sm._1, strategie, nbPointsMaxOrTimeGap)

    }
    // for (item <- arrTimesSeries) { println("Timesesrie =" + item._1.getKey + " => value avgPond=" + item._2(0)) }
    enrichArrTimeSeries

    this
    //  for(ts <- arrTimesSeries )
    //      println("Ts createad ="+(ts._1.getKey) +" size="+ts._1.getTimePeriods.size)
  }

  @specialized(Double, Long)
  def createTimeSeriesOnlyWithOutPivots(sortedMap: SortedMap[Long, ArrayBuffer[String]], prefix: String, strategie: String, nbPointsMaxOrTimeGap: Long): ArrayBuffer[(TimeSeries, ArrayBuffer[Double])] =
    {
      @specialized(Double, Long, Int)
      def createTuples4(tup: Tuple2[Long, ArrayBuffer[String]]): ArrayBuffer[(Long, Double, Int, Double)] =
        {
          // The tuple4 constaints (TimeInMillis, valueOfAgglo,nbOfValuesAgg,maxOfAgglo)
          // println("Tupl2="+tup)
          var list = ArrayBuffer[(Long, Double, Int, Double)]()

          //println("tup=" + tup)
          // println("csvFile.values=" + csvFile.values)
          for (i <- csvFileInMemory.values) {
            var count: Int = 0
            var sum = 0.0
            var xMin: Double = Double.MaxValue
            var xMax: Double = Double.MinValue
            var xMaxMax: Double = Double.MinValue

            var percentValue: ArrayBuffer[Double] = new ArrayBuffer[Double]()
            for (strs <- (tup _2)) {

              val arrayValue = strs.split(csvFileInMemory.sepCourant)

              if (arrayValue.isDefinedAt(i) && arrayValue(i).length > 0) {

                strategie match {
                  case "AVERAGE" => {
                    sum += arrayValue(i).toDouble
                    count += 1

                  }
                  case "MAX" =>
                    {
                      count = 1
                      if (arrayValue(i).toDouble > xMax) {
                        xMax = arrayValue(i).toDouble
                        sum = xMax
                      }
                    }

                  case "MIN" =>
                    {
                      count = 1
                      if (arrayValue(i).toDouble < xMin) {
                        xMin = arrayValue(i).toDouble
                        sum = xMin
                      }
                    }
                  case "MEDIANE" | "PERCENTILE90" =>
                    {
                      count += 1
                      percentValue += arrayValue(i).toDouble

                    }
                  case "SUM" =>
                    {
                      count = 1
                      sum += arrayValue(i).toDouble

                    }

                }
                xMaxMax = math.max(arrayValue(i).toDouble, xMaxMax)
                // println(" arrayValue("+i+")=|"+ arrayValue(i)+"|")
              }
            }

            if (count > 0 && strategie != "MEDIANE" && strategie != "PERCENTILE90" && strategie != "SUM") {
              var tuple = (tup _1, sum / count, count, xMaxMax)
              list += tuple

            } else if (count > 0 && strategie == "MEDIANE") {
              if (count == 1) list += ((tup _1, percentValue(0), 1, xMaxMax))
              else
                list += ((tup _1, (percentValue sortWith (_ < _))(count / 2), 1, xMaxMax))

            } else if (count > 0 && strategie == "PERCENTILE90") {
              if (count == 1) list += ((tup _1, percentValue(0), 1, xMaxMax))

              else
                list += ((tup _1, (percentValue sortWith (_ < _))((count * 9 / 10) - 1), 1, xMaxMax))

            } else if (count > 0 && strategie == "SUM") {
              list += ((tup _1, sum, 1, sum))

            } else
              list += ((tup _1, 0.0, 0, 0.0))
            count = 0
            sum = 0.0
            xMin = Double.MaxValue
            xMax = Double.MinValue
            xMaxMax = Double.MinValue
            percentValue = new ArrayBuffer[Double]()
          }
          // println("list.length=" + list.length)
          list
        }

      //   println("sortedMap="+sortedMap)
      val arrBuff = new ArrayBuffer[(TimeSeries, ArrayBuffer[Double])]()
      val idxValue = csvFileInMemory.values
      var arrTs = new ArrayBuffer[TimeSeries]()
      for (i <- idxValue) {
        //  arrTs += new TimeSeries((csvFile.shortname + "_" + strategie + nbPointsMaxOrTimeGap.toString() + "_" + prefix + sepTs + titleColums(i)).replaceAll(" ", ""))
        //  arrTs += new TimeSeries((csvFileInMemory.name + "_" + strategie + nbPointsMaxOrTimeGap.toString() + "_" + prefix + sepTs + titleColums(i)).replaceAll(" ", ""))
        arrTs += new TimeSeries((csvFileInMemory.name + "_" + strategie + "_" + prefix + sepTs + titleColums(i)).replaceAll(" ", ""))
      }
      //  arrTs foreach ((arg: TimeSeries) => println(arg.getKey))
      var arrGlobalValue: ArrayBuffer[Double] = new ArrayBuffer[Double](arrTs.size)
      // creation d'une structure SortedMap[Long,BufferArray[Tuple5] pour chaque cl� du SortedMap en entree
      // println("sortedMap=" + sortedMap)
      var myListTuple = sortedMap map createTuples4
      //println("myListTuple=" + myListTuple)
      //  //println("myListTuple.size=" + myListTuple.size)
      var i = 0
      for (ts <- arrTs) {
        for (lstVal <- myListTuple) {
          var cal: Calendar = Calendar.getInstance
          cal.setTimeInMillis(lstVal(i)._1)
          if ((lstVal(i) _3) > 0) {

            //  println("lstVal=" + lstVal)

            // ts.add(new Millisecond(cal.getTime), lstVal(i)._2)
            ts.add(MyTimeSeriesItem(new Millisecond(cal.getTime), lstVal(i)._2, (lstVal(i) _3), (lstVal(i) _4)))

          }
        }
        if (ts.getItemCount() > 0) arrBuff.asJava.add((ts, null))
        i += 1
      }
      arrBuff
    }

}

