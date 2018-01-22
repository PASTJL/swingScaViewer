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
import com.jlp.scaviewer.ui.tableandchart.CreateChartAndTable
import java.io.BufferedReader
import java.io.IOException
import com.jlp.scaviewer.ui.ScaCharting
import com.jlp.scaviewer.timeseries.TimeSeriesCollectionFromCsvNew
import com.jlp.scaviewer.timeseries.TransformTsCollectionNew
import java.util.zip.GZIPInputStream
import java.io.InputStreamReader
import language.postfixOps

class CsvFileNew {
  var title: List[String] = Nil
  var sepCourant = ";"
  var nbColumns = 0;
  var listLines: ArrayBuffer[String] = new ArrayBuffer()
  var listStructLines: ArrayBuffer[StructLine] = new ArrayBuffer()
  var firstLine: String = ""
  var secondLine: String = ""
  var regex: Regex = "".r
  var file: File = null;
  var regexDateFormat: (String, String) = ("", "")
  var pivots = List[Int]()
  var values = List[Int]();
  var isTimeSeries = true
  var pivotsArrayValues = ArrayBuffer[(String, ArrayBuffer[StructLine])]()

  var sdf: SimpleDateFormat = null
  override def toString(): String = title.toString()
  //var fmt: DateTimeFormatter = null
  var shortname: String = ""
  def inf(line1: String, line2: String) =
    {
      if (null == line1 || null == line2) false
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

object CsvFileNew {
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
  def apply(file: File, isTimeSeries: Boolean = true): CsvFileNew = {
    // val deb = System.currentTimeMillis

    def findSep(csvFile: CsvFileNew, line: String): String = {
      var countMax = 0
      var sepMax = ";"

      for (sep <- seps) {

        // var countTokens = line.split(sep).toList.size
        var lineBis = line.replaceAll(sep, "")
        var countTokens = 0
        if (lineBis.length() < line.length()) {
          val ext = sep.r.findFirstIn(line).get
          countTokens = (line.length() - lineBis.length()) / ext.length()
        }

        //        println("line=" + line)
        //        println("sep=|" + sep + "| ;countTokens=" + countTokens)
        //        	println("sep.length="+java.util.regex.Pattern.compile(sep).toString.length)
        //	
        if (countTokens > countMax) {
          countMax = countTokens
          sepMax = sep
        }
      }

      csvFile.nbColumns = countMax

      println("sepMax=|" + sepMax + "|" + " countMax=" + countMax)
      sepMax
    }
    def setTitle(csvFile: CsvFileNew, file: File) =
      {
        // for now  read the first line of the file => to be improved for file witout title

        csvFile.title = csvFile.firstLine.split(csvFile.sepCourant).toList

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
        //   println("tabDateTimeRegexp=" + tabDateTimeRegexp)
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
      if (null != line) {
        var lst1 = List[Int]()
        //lst1 => pivots
        // lst2 => values
        println("nbCol=" + nbCol)
        var lst2 = List[Int]()
        var cptField = 0
        val strings = line.split(sep)
        var int = 0
        //val reg = "^[a-zA-Z]+".r
        val reg = "^[a-zA-Z]+[a-zA-Z\\(\\)0-9_,\\+\\-]*".r
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
        println(lst2.reverse)
        (lst1.reverse, lst2.reverse)

      } else {
        (null, null)
      }
    }
    //    }

    def createArrayBufferByPivots(csvFile: CsvFileNew): ArrayBuffer[(String, ArrayBuffer[String])] = {

      var lst = ArrayBuffer[(String, ArrayBuffer[String])]()
      for (i <- csvFile.pivots) {
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

    def createArrayBufferByPivotsNew(csvFile: CsvFileNew): ArrayBuffer[(String, ArrayBuffer[StructLine])] = {

      var lst = ArrayBuffer[(String, ArrayBuffer[StructLine])]()
      for (i <- 0 until csvFile.pivots.length) {
        var aarB = new ArrayBuffer[String]()
        var tmpMap = csvFile.listStructLines groupBy (_.tabPivots(i))
        for (key <- tmpMap.keys) {
          // lst= ((csvFile.firstLine.split(csvFile.sepCourant)(i) + "_" + key, tmpMap(key) sortWith (csvFile.inf(_, _)))) +: lst
          // on complete la cle avec le nom de la colonne correspondante
          lst = ((csvFile.firstLine.split(csvFile.sepCourant)(i + 1) + "_" + key, tmpMap(key))) +: lst
          // println("key ="+key+" ;sizeArray="+(tmpMap(key) sortWith(csvFile.inf(_,_))).length)
        }
      }

      lst.reverse

    }

    def lineToStructLineTimeMillis(timeIn: String, csvFile: CsvFileNew, line: String, modelLine: StructLine): StructLine =
      {
        val tabString = line.split(csvFile.sepCourant)
        timeIn match {

          case "timeInMillis" => modelLine.timeInMillis = line.split(csvFile.sepCourant)(0).toLong
          case "timeInSecondDotMillis" =>
            var tabLong = line.split(csvFile.sepCourant)(0).split("\\.")
            modelLine.timeInMillis = tabLong(0).toLong * 1000 + tabLong(1).toLong
          case "timeInSecond" => modelLine.timeInMillis = line.split(csvFile.sepCourant)(0).toLong * 1000
          case "timeInSecondCommaMillis" =>
            var tabLong = line.split(csvFile.sepCourant)(0).split(",")
            modelLine.timeInMillis = tabLong(0).toLong * 1000 + tabLong(1).toLong
          case _ => modelLine.timeInMillis = line.split(csvFile.sepCourant)(0).toLong
        }

        for (i <- 0 until modelLine.tabPivots.length) {
          if (tabString.isDefinedAt(csvFile.pivots(i)) && tabString(csvFile.pivots(i)).length() > 0) {
            modelLine.tabPivots(i) = tabString(csvFile.pivots(i))
          } else {
            modelLine.tabPivots(i) = ""
          }
        }
        for (i <- 0 until modelLine.tabValues.length) {
          if (tabString.isDefinedAt(csvFile.values(i)) && tabString(csvFile.values(i)).length() > 0) {
            modelLine.tabValues(i) = tabString(csvFile.values(i)).toDouble
          } else {
            modelLine.tabValues(i) = Double.NaN
          }
        }
        modelLine
      }

    def lineToStructLineDate(csvFile: CsvFileNew, line: String, modelLine: StructLine): StructLine =
      {
        val tabString = line.split(csvFile.sepCourant)

        //modelLine.timeInMillis = csvFile.fmt.parseMillis(line.split(csvFile.sepCourant)(0))
        modelLine.timeInMillis = csvFile.sdf.parse(line.split(csvFile.sepCourant)(0)).getTime
        for (i <- 0 until modelLine.tabPivots.length) {
          if (tabString.isDefinedAt(csvFile.pivots(i)) && tabString(csvFile.pivots(i)).length() > 0) {
            modelLine.tabPivots(i) = tabString(csvFile.pivots(i))
          } else {
            modelLine.tabPivots(i) = ""
          }
        }
        for (i <- 0 until modelLine.tabValues.length) {
          if (tabString.isDefinedAt(csvFile.values(i)) && tabString(csvFile.values(i)).length() > 0) {
            modelLine.tabValues(i) = tabString(csvFile.values(i)).toDouble
          } else {
            modelLine.tabValues(i) = Double.NaN
          }
        }
        modelLine
      }
    // var deb = System.currentTimeMillis
    // System.out.println(" avant CsvFile point 0")
    val csvFileNew = new CsvFileNew
    csvFileNew.file = file
    csvFileNew.shortname = file.getName()
    // System.out.println(" apres CsvFile point 1")

    setTitle(csvFileNew, file)

    // println("duree setTitle(csvFile, file)=" + (System.currentTimeMillis() - deb))
    var deb = System.currentTimeMillis
    var buffReader: BufferedReader = null
    try {
      if (file.getAbsolutePath().endsWith(".gz")) {
        val gzip: GZIPInputStream = new GZIPInputStream(new FileInputStream(file.getAbsolutePath()))
        buffReader = new BufferedReader(new InputStreamReader(gzip));
      } else {
        buffReader = Source.fromFile(file, ScaCharting.tmpProps.getProperty("scaviewer.filesstat.sizeBuffer").toInt).bufferedReader()
      }

      csvFileNew.firstLine = buffReader.readLine
      buffReader.mark(10000)
      csvFileNew.secondLine = ""

      while (null != csvFileNew.secondLine && csvFileNew.secondLine.length() < 10) {
        csvFileNew.secondLine = buffReader.readLine
      }
    } finally {
      buffReader.reset
    }

    //    csvFileNew.listLines = new ArrayBuffer[String] ++ Source.fromFile(file)
    //    .getLines
    println("duree  csvFile.listLines=" + (System.currentTimeMillis() - deb))
    //   deb = System.currentTimeMillis
    // csvFile.listLines = asScalaBuffer(Source.fromFile(file).getLines)

    if (null != csvFileNew.secondLine && csvFileNew.secondLine.length > 10) {
      csvFileNew.sepCourant = findSep(csvFileNew, csvFileNew.secondLine)
    } else {
      csvFileNew.sepCourant = findSep(csvFileNew, csvFileNew.firstLine)
    }

    println(" csvFileNew.sepCourant =" + csvFileNew.sepCourant + "|")
    val (pivots, values) = detectPivotsValues(csvFileNew.secondLine, csvFileNew.nbColumns, csvFileNew.sepCourant)
    if(null==values){
      null
    }
    csvFileNew.pivots = pivots
    csvFileNew.values = values
    println("csvFileNew.values.length=" + csvFileNew.values.length)
    println("csvFileNew.values(0)=" + csvFileNew.values(0))
    // csvFile.listLines = csvFile.listLines.tail

    // println("duree  construction nouveau listline=" + (System.currentTimeMillis() - deb))
    //  deb = System.currentTimeMillis

    //  println("duree  trouver le separateur=" + (System.currentTimeMillis() - deb))
    if (isTimeSeries) {

      // println("JLP0 : sepCourant=" + csvFile.sepCourant)
      var strDate = csvFileNew.secondLine.split(csvFileNew.sepCourant)(0)
      println("strDate=" + strDate)
      csvFileNew.regexDateFormat = getFormatDate(strDate)
      println("csvFileNew.regexDateFormat=" + csvFileNew.regexDateFormat)

      if (!csvFileNew.regexDateFormat._2.toLowerCase().contains("timein")) {

        // println("csvFile.regexDateFormat=" + csvFileNew.regexDateFormat)
        //csvFileNew.fmt = org.joda.time.format.DateTimeFormat.forPattern(csvFileNew.regexDateFormat _2);
        csvFileNew.sdf = new SimpleDateFormat(csvFileNew.regexDateFormat _2)
        //csvFile.sdf = new SimpleDateFormat(csvFile.regexDateFormat _2)

        var deb4 = System.currentTimeMillis
        // var res=(csvFile.listLines filter (_.length > 10)).par        map { line =>          line.replaceFirst((csvFile.regexDateFormat _1), csvFile.fmt.parseMillis(line.split(csvFile.sepCourant)(0)).toString)}
        //        val buff = csvFile.listLines.par filter (_.length > 10) map { line => line.replaceFirst((csvFile.regexDateFormat _1), csvFile.fmt.parseMillis(line.split(csvFile.sepCourant)(0)).toString) }
        //        csvFile.listLines = new ArrayBuffer[String] ++ buff
        var bool = true
        while (bool) {
          var line = buffReader.readLine
          if (line != null) {
            if (line.length() > 10) {
              var structLine: StructLine = StructLine(0L, new Array[String](pivots.length), new Array[Double](values.length))
              csvFileNew.listStructLines.append(lineToStructLineDate(csvFileNew, line, structLine))
            }
          } else {
            bool = false
            try {
              buffReader.close()
            } catch {
              case e: IOException => e.printStackTrace()
            }
          }
        }

        // csvFile.listLines = ((csvFile.listLines filter (_.length > 10))) map { line => line.replaceFirst((csvFile.regexDateFormat _1), csvFile.fmt.parseMillis(line.split(csvFile.sepCourant)(0)).toString) }
        //            

        println("duree  remplacement date par Long=" + (System.currentTimeMillis() - deb4))

      } else {
        // date millis
        // println("traitement du cas millis")

        csvFileNew.regexDateFormat._2 match {

          case "timeInSecond" => {
            //  println("listStrucLine with timeInSecond")
            var bool = true
            while (bool) {
              var line = buffReader.readLine
              if (line != null) {
                if (line.length > 10) {
                  var structLine: StructLine = StructLine(0L, new Array[String](pivots.length), new Array[Double](values.length))
                  csvFileNew.listStructLines.append(lineToStructLineTimeMillis("timeInSecond", csvFileNew, line, structLine))
                }
              } else {
                bool = false
                try {
                  buffReader.close()
                } catch {
                  case e: IOException => e.printStackTrace()
                }
              }
            }

          }

          case "timeInSecondDotMillis" =>
            {
              var bool = true
              while (bool) {
                var line = buffReader.readLine
                if (line != null) {
                  if (line.length() > 10) {
                    var structLine: StructLine = StructLine(0L, new Array[String](pivots.length), new Array[Double](values.length))
                    csvFileNew.listStructLines.append(lineToStructLineTimeMillis("timeInSecondDotMillis", csvFileNew, line, structLine))
                  }
                } else {
                  bool = false
                  try {
                    buffReader.close()
                  } catch {
                    case e: IOException => e.printStackTrace()
                  }
                }
              }

            }
          case "timeInSecondCommaMillis" => {
            var bool = true
            while (bool) {
              var line = buffReader.readLine
              if (line != null) {
                if (line.length() > 10) {
                  var structLine: StructLine = StructLine(0L, new Array[String](pivots.length), new Array[Double](values.length))
                  csvFileNew.listStructLines.append(lineToStructLineTimeMillis("timeInSecondCommaMillis", csvFileNew, line, structLine))
                }
              } else {
                bool = false
                try {
                  buffReader.close()
                } catch {
                  case e: IOException => e.printStackTrace()
                }
              }
            }

          }

          case _ => // les temps sont d�ja en millisecondes
            {
              var bool = true
              while (bool) {
                var line = buffReader.readLine
                if (line != null) {
                  if (line.length() > 10) {
                    var structLine: StructLine = StructLine(0L, new Array[String](pivots.length), new Array[Double](values.length))
                    csvFileNew.listStructLines.append(lineToStructLineTimeMillis("timeInMillis", csvFileNew, line, structLine))
                  }
                } else {
                  bool = false
                  try {
                    buffReader.close()
                  } catch {
                    case e: IOException => e.printStackTrace()
                  }
                }
              }

            }

        }

      }
      var deb = System.currentTimeMillis()
      csvFileNew.listStructLines = csvFileNew.listStructLines sortWith (_.timeInMillis < _.timeInMillis)
      println("duree tri=" + (System.currentTimeMillis() - deb))
      //testArr=testArr sortWith ({(line1,line2) => line1.split(csvFile.sepCourant)(0).toLong<line2.split(csvFile.sepCourant)(0).toLong})

      //println("csvFile.listLines =" + csvFile.listLines)

      //csvFile.listLines = csvFile.listLines sortWith (csvFile.inf(_, _))

      csvFileNew.isTimeSeries = isTimeSeries
      // Rechercher la date format de la premiere colonne
      //    println(" csvFile. duree tri initial= "+(System.currentTimeMillis-deb2))
      //    println(" csvFile.regexDateFormat =" + csvFile.regexDateFormat)
      //  println("duree point 1 =" + (System.currentTimeMillis - deb))
      // val deb = System.currentTimeMillis

      // println("duree   detectPivotsValues(csvFile.secondLine, csvFile.nbColumns, csvFile.sepCourant)=" + (System.currentTimeMillis() - deb))

      // println("csvFile.pivots="+csvFile.pivots )
      // println("csvFile.values="+csvFile.values )

      // Creation des arrayBuffer par pivot si pivots
      val deb3 = System.currentTimeMillis
      if (pivots.length > 0) {
        //   println("nb Pivots =" + pivots.length)

        csvFileNew.pivotsArrayValues = createArrayBufferByPivotsNew(csvFileNew)

        //    println("duree createArrayBufferByPivots =" + (System.currentTimeMillis - deb))
      }

      csvFileNew.pivotsArrayValues += (("global", csvFileNew.listStructLines))
      println("duree   createArrayBufferByPivots(csvFile)=" + (System.currentTimeMillis() - deb3))
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
    csvFileNew
  }

  // trier le tableau par ordre croissant de date

  def main(args: Array[String]) {

    // Pour permettre de profiler par jvisualvm
    var ok = true
    //  var ok = false
    while (ok) {
    //  var char = readChar()
      var char=StdIn.readChar()
      ok = false

    }

    import java.lang.management.ManagementFactory;
    import java.lang.management.RuntimeMXBean;
    import com.sun.management._

    val osMBean: OperatingSystemMXBean =
      ManagementFactory.getOperatingSystemMXBean().asInstanceOf[OperatingSystemMXBean]

    val runtimeMBean: RuntimeMXBean = ManagementFactory.getRuntimeMXBean();

    System.out.println("Operating system:\t" + osMBean.getName());
    System.out.println("Architecture:\t\t" + osMBean.getArch());
    System.out.println("Number of processors:\t" + osMBean.getAvailableProcessors());
    System.out.println("Process CPU time:\t" + osMBean.getProcessCpuTime());
    System.out.println("Total physical memory:\t" + osMBean.getTotalPhysicalMemorySize() / 1024 + " kB");
    System.out.println("Free physical memory:\t" + osMBean.getFreePhysicalMemorySize() / 1024 + " kB");
    System.out.println("Comm. virtual memory:\t" + osMBean.getProcessCpuTime() / 1024 + " kB");

    System.out.println("Total swap space:\t" + osMBean.getTotalSwapSpaceSize() / 1024 + " kB");
    System.out.println("Free swap space:\t" + osMBean.getFreeSwapSpaceSize() / 1024 + " kB");

    val deb0 = System.currentTimeMillis
    println("main avant creation CsvFile :" + args(0))
    val csvFile1 = CsvFileNew(new File(args(0)), true)
    println("duree creation csvFile=" + (System.currentTimeMillis - deb0))
    //println(" pivotsArrayValues=="+csvFile1.pivotsArrayValues)

    val tsC = TimeSeriesCollectionFromCsvNew(csvFile1)
    //  println("csvFile1.secondLine="+csvFile1.secondLine)

    //var map1= tsC.extractMap(tsC.arrLines,1000,0,tsC.arrLines.length-1)
    // map1 map (println _)
    //  println(map1.size)
    val deb = System.currentTimeMillis
    println("debut creation TimeSeries")
    tsC.createAllTimeSeries(true, true, 1000, 0.toString, Long.MaxValue.toString, "AVERAGE")
    println("fin creation Timeseries duree=" + (System.currentTimeMillis - deb))
    //tsC.createAllTimeSeries(true, false, 1000, 0.toString, Long.MaxValue.toString.toString, "MAX")
    //tsC.createAllTimeSeries(true, false, 1000, 0.toString, Long.MaxValue.toString, "MIN")

    //    for (enr <- tsC.enrichedArrTimeSeries) {
    //      //println((enr._1.getKey, enr._1.getItemCount(), enr._2, enr._3, enr._4, enr _5, enr._6.size).toString())
    //      println(enr.ts.getKey(), enr.ts.getItemCount(), enr.pivot, enr.columnName, enr.unite, enr.grp, enr.rowTable.size)
    //    }
    var deb2 = System.currentTimeMillis
    val ttsc = new TransformTsCollectionNew(tsC)
    println("fin  TransformTsCollections duree=" + (System.currentTimeMillis - deb2))
    deb2 = System.currentTimeMillis
    ttsc.normalizeTs()
    println("fin  TransformTsCollections.normalize duree=" + (System.currentTimeMillis - deb2))
    //    for (enr <- ttsc.mapByGroupUnit) {
    //      print(enr._1 + " => ")
    //      for (enr2 <- enr._2)
    //        print(enr2.ts.getKey + ", ")
    //      println()
    //     
    //    }

    //    println("####################################################################################################")

    //    println("meilleur choix  :"+Unites.bestConversion(800,1000,0,"s*Goctets"))
    //    println(Unites.convert("s/Goctet","ms/octet"))
    //    for (enr <- ttsc.mapByGroupUnit) {
    //     
    //      for (enr2 <- enr._2)
    //      {
    //        println("Avant  Normalisation: "+enr2.ts.getKey + " => "+enr2.unite)
    //       
    //          println("Avant  Normalisation :"+enr2.ts.getKey,enr2.pivot,enr2.columnName,enr2.unite,enr2.grp,enr2.rowTable)
    //      }
    //    }
    //    
    //    ttsc.normalizeTs()
    //     for (enr <- ttsc.mapByGroupUnit) {
    //     
    //      for (enr2 <- enr._2)
    //      {
    //        println("apres Normalisation :"+enr2.ts.getKey + " => "+enr2.unite)
    //       
    //        println("apres Normalisation :" +enr2.ts.getKey,enr2.pivot,enr2.columnName,enr2.unite,enr2.grp,enr2.rowTable)
    //      }
    //     
    //     
    //    }
    //  var cct = ttsc.affectCoulToGrpAndTs()

    //    println("Couleurs des groupes: ")
    //    for(enr <- cct.mapGrpToColor) println(enr _1,enr _2)
    //     println("Couleurs des TimeSeries : ")
    //    for(enr <- cct.mapStructTsToColor)println(enr._1.ts.getKey,enr _2)
    //    
    //      println("####################################################################################################")
    // test extraction unite
    //    var libelle = "Duree   ( mmmm/millioctet)"
    //    var unite = tsC.trouverUnite(libelle)
    //    println("unite=" + unite)
    //    println("lbelle sans unite=" + tsC.supprimerunite(libelle) + "|")
    //    println("isCorrect(" + unite + ")=" + Unites.isCorrectUnit(unite))

    // tsC.createAllTimeSeries(true, false, 1000, "2010/05/12 15:41:00", "2010/05/12 17:41:16", "MAX")
    //tsC.createAllTimeSeries(true, false, 1000, "2010/05/12 15:41:00", "2010/05/12 17:41:16", "MIN")
    // tsC.createAllTimeSeries(true,1000,tsC.arrLines(0)._2(0).split(csvFile1.sepCourant)(0),tsC.arrLines((tsC.arrLines.length)-1)._2(0).split(csvFile1.sepCourant)(0),"MAX")
    //tsC.createAllTimeSeries(true,1000,tsC.arrLines(0)._2(0).split(csvFile1.sepCourant)(0),tsC.arrLines((tsC.arrLines.length)-1)._2(0).split(csvFile1.sepCourant)(0),"MIN")
    //csvFile setTitle file
    println("fin  duree=" + (System.currentTimeMillis - deb0))
    //    for(ts <- tsC.arrTimesSeries)
    //    {
    //      for(i  <- 1 until (ts _1).getItemCount)
    //      {
    //        print((ts _1).getValue(i)+", ")
    //      }
    //      println
    //    }
  }
}