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
package com.jlp.scaviewer.csvutils
import java.io.File
import scala.io.Source
//import scala.util.parsing.combinator._
import scala.io.StdIn
import scala.util.matching._
import scala.sys.SystemProperties
import java.util.Properties
import java.io.FileInputStream
import scala.collection.immutable.HashMap
import java.util.Enumeration
import scala.collection.mutable.ArrayBuffer
import java.text.SimpleDateFormat
import com.jlp.scaviewer.commons.utils._
import com.jlp.scaviewer.timeseries.TimeSeriesCollectionFromCsvNew
import com.jlp.scaviewer.timeseries.TransformTsCollectionNew
import com.jlp.scaviewer.ui.tableandchart.CreateChartAndTable
import com.jlp.scaviewer.timeseries.StructTs
import com.jlp.scaviewer.timeseries.TimeSeriesCollectionFromCsvInMemoryNew
import com.jlp.scaviewer.timeseries.TransformTsCollectionInMemoryNew
import language.postfixOps
import com.jlp.scaviewer.ui.ScaCharting

class CsvFileInMemoryNew {
  var name: String = ""
  var title: List[String] = Nil
  var sepCourant = ";"
  var nbColumns = 0;
  var listLines: ArrayBuffer[String] = new ArrayBuffer()
  var firstLine: String = ""
  var secondLine: String = ""
  var regex: Regex = "".r
  var file: ArrayBuffer[String] = null;
  var regexDateFormat: (String, String) = ("", "")
  var pivots = List[Int]()
  var values = List[Int]();
  var isTimeSeries = true

  var pivotsArrayValues = ArrayBuffer[(String, ArrayBuffer[String])]()
  var sdf: SimpleDateFormat = null
  override def toString(): String = title.toString()
  //var fmt: DateTimeFormatter = null

  var shortname: String = ""
  def inf(line1: String, line2: String) =
    {
      (line1 compareTo line2) < 0
      //(line1.split(sepCourant)(0) compareTo line2.split(sepCourant)(0)) < 0
      // fmt.parseDateTime(line1.split(sepCourant)(0)).isBefore(fmt.parseDateTime(line2.split(sepCourant)(0)))
      //sdf.parse(line1.split(sepCourant)(0)).getTime<sdf.parse(line2.split(sepCourant)(0)).getTime
    }

  def dateInMillis(line: String) = {
    //fmt.parseMillis(line.split(sepCourant)(0))
    // sdf.parse(line.split(sepCourant)(0)).getTime
    //sdf.parse((regexDateFormat _1).r findFirstIn line getOrElse ("")).getTime
    line.split(sepCourant)(0).toLong
  }

}

object CsvFileInMemoryNew {
  val propsDate: java.util.Properties = new Properties
  propsDate.load(new FileInputStream(new File(System
    .getProperty("root")
    + File.separator
    + "config"
    + File.separator
    + "scaViewerDates.properties")));

  val scaChartProps: java.util.Properties = new Properties
  scaChartProps.load(new FileInputStream(new File(System
    .getProperty("root")
    + File.separator
    + "config"
    + File.separator
    + "scaViewer.properties")));
  // val seps = List(",", ";", ":", ",", "\\|", "\\s\\s+")
  val seps = scaChartProps.get("csvFile.separators").asInstanceOf[String].split(scaChartProps.get("csvFile.separatorOfSeparators").asInstanceOf[String]).toList

  def apply(file: ArrayBuffer[String], name: String, isTimeSeries: Boolean = true, idx: Int = 0): CsvFileInMemoryNew = {
    // val deb = System.currentTimeMillis
    var nbItems = 0
    def findSep(csvFileInMemory: CsvFileInMemoryNew, line: String): String = {
      var countMax = 0
      var sepMax = ";"

      for (sep <- seps) {
        var lineBis = line.replaceAll(sep, "")

        var countTokens = 0
        if (lineBis.length() < line.length()) {
          val ext = sep.r.findFirstIn(line).get
          countTokens = (line.length() - lineBis.length()) / ext.length()
        }
        if (countTokens > countMax) {
          countMax = countTokens

          sepMax = sep
        }
      }

      csvFileInMemory.nbColumns = countMax

      println("CsvFileInMemory findSep : sepMax =" + sepMax)
      sepMax
    }
    def setTitle(csvFile: CsvFileInMemoryNew, file: ArrayBuffer[String]) =
      {
        // read the first line of the file

        csvFile.title = csvFile.file(0).split(csvFile.sepCourant).toList

      }

    def getFormatDate(input: String): (String, String) =
      {

        // println("getFormatDate input=" + input)
        var tabDateTimeRegexp = new HashMap[String, String]
        var keys = propsDate.keys();
        while (keys.hasMoreElements()) {
          var key: String = keys.nextElement().asInstanceOf[String]
          var value: String = propsDate.getProperty(key)
          // if(!key.contains("format.")) 
          tabDateTimeRegexp += (key -> value)

        }
        //  println("tabDateTimeRegexp=" + tabDateTimeRegexp)
        var regexLongestr = ""
        var formatDate = "";
        var kkeys = tabDateTimeRegexp.keys
        var kkeyLongest = ""
        for (kkey <- kkeys) {
          if (!kkey.contains("format.")) {
            var regex = tabDateTimeRegexp.getOrElse(kkey, "").r
            if (regex.findFirstIn(input) != None) {
              if (tabDateTimeRegexp.getOrElse(kkey, "").length > regexLongestr.length) {
                regexLongestr = tabDateTimeRegexp.getOrElse(kkey, "")
                kkeyLongest = kkey
              }
            }
          }

        }

        (regexLongestr, tabDateTimeRegexp.getOrElse("format." + kkeyLongest, ""))
      }

    def detectPivotsValues(line: String, nbCol: Int, sep: String): (List[Int], List[Int]) = {
      var lst1 = List[Int]()
      var lst2 = List[Int]()
      var cptField = 0
      val strings = line.split(sep)
      var int = 0
      //val reg = "^[a-zA-Z]+".r
      val reg = "^[a-zA-Z]+".r
      for (str <- strings) {

        if (int != 0) {

          if (reg.findFirstIn(str) != None) {

            lst1 = int :: lst1
          } else {

            lst2 = int :: lst2
          }

        }
        int += 1
        cptField += 1
      }
      // Completer la fin de ligne si champ vide
      for (ii <- cptField until nbCol) {
        lst2 = ii :: lst2
      }
      (lst1.reverse, lst2.reverse)
    }
 
    def createArrayBufferByPivots(csvFile: CsvFileInMemoryNew): ArrayBuffer[(String, ArrayBuffer[String])] = {

      var lst = ArrayBuffer[(String, ArrayBuffer[String])]()
      for (i <- csvFile.pivots.par) {
        var aarB = new ArrayBuffer[String]()
        var tmpMap = csvFile.listLines groupBy (_.split(csvFile.sepCourant)(i))
        for (key <- tmpMap.keys) {
          // lst= ((csvFile.firstLine.split(csvFile.sepCourant)(i) + "_" + key, tmpMap(key) sortWith (csvFile.inf(_, _)))) +: lst
          lst = ((csvFile.firstLine.split(csvFile.sepCourant)(i + 1) + "_" + key, tmpMap(key))) +: lst
          // println("key ="+key+" ;sizeArray="+(tmpMap(key) sortWith(csvFile.inf(_,_))).length)
        }
      }

      lst.reverse

    }

    val deb = System.currentTimeMillis

    val csvFileInMemory = new CsvFileInMemoryNew
    csvFileInMemory.file = file
    csvFileInMemory.shortname = name
    csvFileInMemory.name = name
    // System.out.println(" apres CsvFile point 1")
    setTitle(csvFileInMemory, file)
    csvFileInMemory.listLines = file
    // csvFile.listLines = asScalaBuffer(Source.fromFile(file).getLines)
    csvFileInMemory.firstLine = csvFileInMemory.listLines.head
    csvFileInMemory.listLines = csvFileInMemory.listLines.tail
    csvFileInMemory.secondLine = csvFileInMemory.listLines.head
   

    if (isTimeSeries) {
       var strDate =""
  
    nbItems = ScaCharting.listChartingInfo(idx).nbItems;
    var sep = ScaCharting.listChartingInfo(idx).sep
    
    csvFileInMemory.sepCourant = sep
    csvFileInMemory.regexDateFormat=(ScaCharting.listChartingInfo(idx).regexDate,ScaCharting.listChartingInfo(idx).dateFormat)
    csvFileInMemory.pivots = ScaCharting.listChartingInfo(idx).posPivots
    csvFileInMemory.values = ScaCharting.listChartingInfo(idx).posValues
    
      //println("csvFile.regexDateFormat=" + csvFile.regexDateFormat)
      if (!csvFileInMemory.regexDateFormat._2.toLowerCase().contains("timein")) {

        //   println("strDate="+strDate+" ;csvFile.regexDateFormat=" + csvFileInMemory.regexDateFormat)
        // csvFileInMemory.fmt = org.joda.time.format.DateTimeFormat.forPattern(csvFileInMemory.regexDateFormat _2);

        csvFileInMemory.sdf = new SimpleDateFormat(csvFileInMemory.regexDateFormat _2)
        //csvFile.sdf = new SimpleDateFormat(csvFile.regexDateFormat _2)

        var deb4 = System.currentTimeMillis

        csvFileInMemory.listLines = new ArrayBuffer[String] ++ csvFileInMemory.listLines filter (line => (line.length > 10 && line.split(sep).length >= nbItems)
            && { csvFileInMemory.regexDateFormat._1.r.findFirstIn(line) != None  }) map { line =>
          //   line.replaceFirst((csvFileInMemory.regexDateFormat _1), csvFileInMemory.fmt.parseMillis(line.split(csvFileInMemory.sepCourant)(0)).toString)
//
//          println("line="+line)
//           println("csvFileInMemory.regexDateFormat _1="+(csvFileInMemory.regexDateFormat _1))
//           println("csvFileInMemory.sdf="+(csvFileInMemory.sdf).toString())
          line.replaceFirst((csvFileInMemory.regexDateFormat _1), csvFileInMemory.sdf.parse(line.split(csvFileInMemory.sepCourant)(0)).getTime.toString)

        }
        csvFileInMemory.listLines = csvFileInMemory.listLines sortWith (csvFileInMemory.inf(_, _))

      } else {
        // date millis
        // println("traitement du cas millis")

        csvFileInMemory.regexDateFormat._2 match {

          case "timeinSecond" =>
            csvFileInMemory.listLines = csvFileInMemory.listLines filter (line => (line.length > 10 && line.split(sep).length >= nbItems)
            && { csvFileInMemory.regexDateFormat._1.r.findFirstIn(line) != None  }) map { line =>
              line.replaceFirst((csvFileInMemory.regexDateFormat _1), (line.split(csvFileInMemory.sepCourant)(0).toLong * 1000).toString)
            }

          case "timeinSecondDotMillis" =>
            // println("traitement du cas : timeinSecond.millis")
            csvFileInMemory.listLines = csvFileInMemory.listLines filter (line => (line.length > 10 && line.split(sep).length >= nbItems)
            && { csvFileInMemory.regexDateFormat._1.r.findFirstIn(line) != None  }) map { line =>
              line.replaceFirst((csvFileInMemory.regexDateFormat _1), ((line.split(csvFileInMemory.sepCourant)(0).split("\\.")(0).toLong * 1000) +
                (line.split(csvFileInMemory.sepCourant)(0).split("\\.")(1).toLong * 1000).toString.substring(0, 3).toLong).toString)
            }

          case "timeinSecondCommaMillis" =>
            csvFileInMemory.listLines = csvFileInMemory.listLines filter (line => (line.length > 10 && line.split(sep).length >= nbItems)
            && { csvFileInMemory.regexDateFormat._1.r.findFirstIn(line) != None  }) map { line =>
              line.replaceFirst((csvFileInMemory.regexDateFormat _1), ((line.split(csvFileInMemory.sepCourant)(0).split(",")(0).toLong * 1000) +
                (line.split(csvFileInMemory.sepCourant)(0).split(",")(1).toLong * 1000).toString.substring(0, 3).toLong).toString)
            }

          case _ => // les temps sont d�ja en millisecondes

        }
        csvFileInMemory.listLines = csvFileInMemory.listLines sortWith (_.split(csvFileInMemory.sepCourant)(0).toLong < _.split(csvFileInMemory.sepCourant)(0).toLong)

      }

      //testArr=testArr sortWith ({(line1,line2) => line1.split(csvFile.sepCourant)(0).toLong<line2.split(csvFile.sepCourant)(0).toLong})

      //println("csvFile.listLines =" + csvFile.listLines)

      //csvFile.listLines = csvFile.listLines sortWith (csvFile.inf(_, _))

      csvFileInMemory.isTimeSeries = isTimeSeries
      // Rechercher la date format de la premiere colonne
      //    println(" csvFile. duree tri initial= "+(System.currentTimeMillis-deb2))
      //    println(" csvFile.regexDateFormat =" + csvFile.regexDateFormat)
      //  println("duree point 1 =" + (System.currentTimeMillis - deb))
    
      // println("csvFile.pivots="+csvFile.pivots )
      // println("csvFile.values="+csvFile.values )

      // Creation des arrayBuffer par pivot si pivots
      val deb3 = System.currentTimeMillis
      if (csvFileInMemory.pivots.length > 0) {
        //   println("nb Pivots =" + pivots.length)

        csvFileInMemory.pivotsArrayValues = createArrayBufferByPivots(csvFileInMemory)
        //    println("duree createArrayBufferByPivots =" + (System.currentTimeMillis - deb))
      }
      csvFileInMemory.pivotsArrayValues += (("global", csvFileInMemory.listLines))
      //       
      //       for(i <-0 until csvFileInMemory.listLines.length)
      //       println("line "+i+" ="+csvFileInMemory.listLines(i))
      //  println("duree createArrayBufferByPivots yc global =" + (System.currentTimeMillis - deb))
      //        for(tupArr<-csvFile.pivotsArrayValues)
      //        {
      //         
      //          println((tupArr _1)+" -> size= "+((tupArr _2 ) length))
      //          println((tupArr _2 ))
      //        }
      val fin = System.currentTimeMillis
      // println(" finFactory csv File duree =" + (fin - deb));

    } else {
      // TODO with XYSeries

    }
    csvFileInMemory
  }

  // trier le tableau par ordre croissant de date

  def main(args: Array[String]) {

    // Pour permettre de profiler par jvisualvm
    var ok = true
    //  var ok = false
    while (ok) {
     // var char = readChar()
      var char=StdIn.readChar()
      ok = false

    }

    val deb0 = System.currentTimeMillis
    println("main avant creation CsvFileInMemory")
    val csvFile1 = CsvFileInMemoryNew(new ArrayBuffer[String] ++ Source.fromFile(new File(args(1))).getLines, new File(args(1)).getAbsolutePath(), true)
    println("duree creation csvFileInMemory=" + (System.currentTimeMillis - deb0))
    //println(" pivotsArrayValues=="+csvFile1.pivotsArrayValues)

    val tsC = TimeSeriesCollectionFromCsvInMemoryNew(csvFile1)
    println("duree creation TimeSeriesCollectionFromCsvInMemory=" + (System.currentTimeMillis - deb0))

    tsC.createAllTimeSeries(true, true, 1000, 0.toString, Long.MaxValue.toString, "AVERAGE")
    println("duree creation tsC.createAllTimeSeries=" + (System.currentTimeMillis - deb0))

    val ttsc = new TransformTsCollectionInMemoryNew(tsC)
    println("duree creation ttsc TransformTsCollectionInMemory(tsC)=" + (System.currentTimeMillis - deb0))

    ttsc.normalizeTs()
    println("duree creation ttsc  ttsc.normalizeTs())=" + (System.currentTimeMillis - deb0))
    val tab = (ttsc.mapByGroupUnit flatMap (_._2)).asInstanceOf[ArrayBuffer[StructTs]]
    var datasets = ttsc.createDatasets(tab)
    println("duree creation datasets=" + (System.currentTimeMillis - deb0))

    //    //     for (enr <- ttsc.mapByGroupUnit) {
    //    //     
    //    //      for (enr2 <- enr._2)
    //    //      {
    //    //        println("apres Normalisation :"+enr2.ts.getKey + " => "+enr2.unite)
    //    //       
    //    //        println("apres Normalisation :" +enr2.ts.getKey,enr2.pivot,enr2.columnName,enr2.unite,enr2.grp,enr2.rowTable)
    //    //      }
    //    //     
    //    //     
    //    //    }
    //    var cct = ttsc.affectCoulToGrpAndTs()
    //    //    println("Couleurs des groupes: ")
    //    //    for(enr <- cct.mapGrpToColor) println(enr _1,enr _2)
    //    //     println("Couleurs des TimeSeries : ")
    //    //    for(enr <- cct.mapStructTsToColor)println(enr._1.ts.getKey,enr _2)
    //    //    
    //    //      println("####################################################################################################")
    //    // test extraction unite
    //    //    var libelle = "Duree   ( mmmm/millioctet)"
    //    //    var unite = tsC.trouverUnite(libelle)
    //    //    println("unite=" + unite)
    //    //    println("lbelle sans unite=" + tsC.supprimerunite(libelle) + "|")
    //    //    println("isCorrect(" + unite + ")=" + Unites.isCorrectUnit(unite))
    //
    //    println("fin creation Timeseries duree=" + (System.currentTimeMillis - deb))
    //    // tsC.createAllTimeSeries(true, false, 1000, "2010/05/12 15:41:00", "2010/05/12 17:41:16", "MAX")
    //    //tsC.createAllTimeSeries(true, false, 1000, "2010/05/12 15:41:00", "2010/05/12 17:41:16", "MIN")
    //    // tsC.createAllTimeSeries(true,1000,tsC.arrLines(0)._2(0).split(csvFile1.sepCourant)(0),tsC.arrLines((tsC.arrLines.length)-1)._2(0).split(csvFile1.sepCourant)(0),"MAX")
    //    //tsC.createAllTimeSeries(true,1000,tsC.arrLines(0)._2(0).split(csvFile1.sepCourant)(0),tsC.arrLines((tsC.arrLines.length)-1)._2(0).split(csvFile1.sepCourant)(0),"MIN")
    //    //csvFile setTitle file
    //    println("fin  duree=" + (System.currentTimeMillis - deb0))
    //    //    for(ts <- tsC.arrTimesSeries)
    //    //    {
    //    //      for(i  <- 1 until (ts _1).getItemCount)
    //    //      {
    //    //        print((ts _1).getValue(i)+", ")
    //    //      }
    //    //      println
    //    //    }
  }
}