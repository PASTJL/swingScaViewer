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
import scala.collection.mutable.HashMap
import com.jlp.scaviewer.scalogparser.ui.ConfigParser
import java.io.BufferedWriter
import java.net.URLClassLoader
import java.text.SimpleDateFormat
import java.io.RandomAccessFile
import java.io.File
import java.util.regex.Matcher
import java.util.regex.Pattern
import com.jlp.scaviewer.ui.SwingScaViewer
import java.util.Calendar
import java.util.StringTokenizer
import java.io.BufferedReader
import java.io.OutputStreamWriter
import java.util.zip.GZIPOutputStream
import java.io.FileOutputStream
import java.util.Locale
import java.net.URL
import java.net.MalformedURLException
import java.lang.reflect.Method
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.IOException
import java.util.Date
import java.text.ParseException
import java.io.FileReader
import scala.util.matching.Regex
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
//import org.joda.time.format.DateTimeFormatter
//import org.joda.time.format.DateTimeFormat
import java.io.PrintStream
import scala.swing.TextArea
import scala.swing.ScrollPane
import java.util.concurrent.atomic.AtomicInteger
import akka.actor.Actor
import java.awt.Font
import com.jlp.scaviewerdyn.ui.ScalaChartingDyn
import com.jlp.scaviewer.ui.ScaCharting
import org.jfree.chart.ChartFactory
import com.jlp.scaviewer.ui.tableandchart.CreateChartAndTable
import com.jlp.scaviewer.ui.tableandchart.ScaChartingListener
import scala.collection.mutable.ArrayBuffer
import com.jlp.scaviewer.timeseries.StructTs
import com.typesafe.config._
import com.jlp.scaviewer.commons.utils.SearchDirFile
import language.postfixOps

class ScaParserMain extends akka.actor.Actor {
  def receive = {
    case conf: java.util.Properties =>
      ScaParserMain.apply(conf)

    case "stop" =>

      context.stop(self)
  }
}
object ScaParserMain extends ParsingModes {
  var longDeb: Long = Long.MaxValue
  var longFin: Long = 0
  val hmapClass: HashMap[String, Object] = new HashMap()
  val hmapMethod: HashMap[String, Method] = new HashMap()
  var tabBoolSolversStopped: Array[Boolean] = null
  var tabBoolStopped: Array[Boolean] = null
  var tabLongDeb: Array[Long] = null
  var tabLongFin: Array[Long] = null
  var myReader: MyReader = null
  var reader: BufferedReader = null
  var compteurGlobalRead: Int = 0
  var compteurGlobalTreated: AtomicInteger = new AtomicInteger(0)

  var pivotExhaustifParsing = false
  var correctDate = 0L
  var isWithFunctions: Boolean = false
  var sizeBufReader: Int = 1000000
  var contenuJta: String = "";
  var nblinesForRapidDatation: Int = 100000;
  var props: java.util.Properties = null;
  var isDebDate: Boolean = true

  var currentLocaleIn: java.util.Locale = java.util.Locale.ENGLISH
  var currentLocaleOut: java.util.Locale = java.util.Locale.ENGLISH

  // controle des dates des enregistrements dans une fenetre de temps
  var controlDate: Boolean = false
  var startParsingDate = "1970/01/01 00:00:00"
  var endParsingDate = "1970/01/01 00:00:00"
  var dateStartParsingDate: Long = 0
  var dateEndParsingDate: Long = 0
  var sdfWindow: SimpleDateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")

  var extractBool = false;
  var showAllAverages = false;
  var allAveragesOnly = false;
  var writer: BufferedWriter = null;
  var debEqualsFin: Boolean = false;
  var fileInIncludeReg: String = null
  var fileInExcludeReg: String = null
  var urlClassLoader: URLClassLoader = null;

  var hasPivots = true;
  var sizePivots = 0
  var boolExplicit = true;

  var carFinLigne = 1;
  var dateIndice0 = 0L;
  var modeDebug = false;
  val linesToSkip = 0;
  final val tabRegexp: Array[String] = Array("\\", "]+", "*", ".+", ".?", "++", "+?", "]?", "^", "|")

  var tabAllAverages: Array[HashMap[Int, String]] = null
  // First indice is for Actor, second indice is for csv files
  var tabActorsFilesGenerated: Array[Array[GeneratedDatasForCSVFiles]] = null

  var boolStopLect = false
  var boolStopGen = false
  var deb: Long = 0
  private var allAverage: RandomAccessFile = null
  var count: Long = 0
  var nbActors = 2
  var tabStrFilesCsv: Array[String] = null
  var tabFilterPiv: Array[Int] = null
  var tabFilterVal: Array[Int] = null
  var patternDeb: Regex = null
  var dateInputRegexp: Pattern = null
  var dateInputReg: Regex = null
  //var dateTimeFormatter: DateTimeFormatter = null
  var simpleDateFormat: SimpleDateFormat = null
  var decalTimeZone: Long = 0

  var patternFin: Regex = null
  var scaViewerProperties = SwingScaViewer.tmpProps

  nblinesForRapidDatation = scaViewerProperties.getProperty("scaviewer.filesstat.nblinesForRapidDatation", "100000").toInt
  var waitingEnrMax: Int = scaViewerProperties.getProperty("scaviewer.scalogparser.waitingenrs", "100000").toInt
  var waitingTime: Long = scaViewerProperties.getProperty("scaviewer.scalogparser.waitingTime", "100").toLong
  val gapinfo = scaViewerProperties.getProperty("scaviewer.scalogparser.gapinfo", "1000").toInt
  var sizeValues = 0;
  var stepAgg: Long = 1000
  var csvSeparator = ";"
  var sdfCsv: SimpleDateFormat = null
  var strSdfCsv: String = ""
  var stepWithinEnreg: String = ""
  private var dateF = ""
  var reportCsvDirectory = ""
  private var listValues = ""
  private var listPivots = ""
  var fsOut = ";"
  var fileIn: File = null
  var fileOutFilter: File = null;
  var boolMultiCsvFiles = false
  var estimated = false
  var nbLinesOfFile: Int = 0;
  var gap: Int = 1
  var typeRead = 0
  var solvers: Array[ActorRef] = null
  var multTms = 1L
  var isTimeInMillis = false
  var reg2ForDateInMillis = """\d+\.?\d*""".r
  var isStepWithValEquals = false
  var lgStep = 0L // must be in ms
  var unitStep = ""
  //var parser: ConfigParser = null

  var logTrace: PrintStream = null
  final val WITHOUT_INCLUDE_WITHOUT_EXCLUDE = 0
  final val WITH_INCLUDE_WITHOUT_EXCLUDE = 1
  final val WITHOUT_INCLUDE_WITH_EXCLUDE = 2
  final val WITH_INCLUDE_WITH_EXCLUDE = 3
  val ta = new TextArea(1000, 300)
  val sp = new ScrollPane(ta)
  ta.font = new Font("Arial", Font.BOLD, 14)

  //def apply(conf: ConfigParser) {
  def apply(conf: java.util.Properties) {
    tabAllAverages = null
    // First indice is for Actor, second indice is for csv files
    tabActorsFilesGenerated = null
    compteurGlobalRead = 0
    compteurGlobalTreated = new AtomicInteger(0)

    longDeb = Long.MaxValue
    longFin = 0
    hmapClass.clear
    hmapMethod.clear
    //parser = conf
    //props = conf.props
    props = conf
    nbLinesOfFile = 0
    SwingScaViewer.mainPanel.contents.clear

    SwingScaViewer.mainPanel.contents += sp
    SwingScaViewer.mainPanel.visible = false
    SwingScaViewer.mainPanel.visible = true
    ta.text = "Demarrage Parsing"

    startParsingDate = props.getProperty("fileIn.startParsingDate", "1970/01/01 00:00:00")
    endParsingDate = props.getProperty("fileIn.endParsingDate", "1970/01/01 00:00:00")
    if (sdfWindow.parse(startParsingDate).getTime() >= sdfWindow.parse(endParsingDate).getTime()) {
      controlDate = false
    } else {
      controlDate = true

      dateStartParsingDate = sdfWindow.parse(startParsingDate).getTime()
      dateEndParsingDate = sdfWindow.parse(endParsingDate).getTime()
    }

    decalTimeZone = props.getProperty("advanced.decalTimeZone", "0").toLong
    System.out.println("ScaParsingMain.controlDate=" + controlDate)
    fileInIncludeReg = props.getProperty("fileIn.inclEnrReg", "")
    fileInExcludeReg = props.getProperty("fileIn.exclEnrReg", "")
    isWithFunctions = false
    sizeBufReader = scaViewerProperties.getProperty("scaviewer.filesstat.sizeBuffer", "100000").toInt
    correctDate = props.getProperty("advanced.correctDate", "0").toLong
    if (props.getProperty("advanced.pivotExhaustifParsing") == "true") {

      pivotExhaustifParsing = true

    } else {

      pivotExhaustifParsing = false
    }

    if (props.getProperty("advanced.debugMode") == "true") {

      modeDebug = true
      var strTrace = System.getProperty("root") + File.separator + "logs" + File.separator + "scaViewer.log"
      var fileLog = new File(strTrace)
      if (fileLog.exists) fileLog.delete
      logTrace = new PrintStream(fileLog)
    } else {

      modeDebug = false
    }
    if (props.getProperty("fileIn.localEnglish") == "true") {

      currentLocaleIn = java.util.Locale.UK

    } else {

      currentLocaleIn = java.util.Locale.FRANCE
    }

    if (props.getProperty("filesOut.localeEnglishOut") == "true") {

      currentLocaleOut = java.util.Locale.UK

    } else {

      currentLocaleOut = java.util.Locale.FRANCE
    }

    if (props.getProperty("advanced.generateEnrToFile", "false") == "true") {
      extractBool = true;
    } else {
      extractBool = false;
    }

    if (props.getProperty("advanced.viewAllAverages", "false") == "false") {
      showAllAverages = false;
    } else {
      showAllAverages = true;
    }
    nbActors = props.getProperty(
      "advanced.nbActors", "2").toInt
    if (nbActors < 1) nbActors = 1
    if (props.getProperty("fileIn.explicitDate").equals(
      "true")) {
      boolExplicit = true;
    } else {
      boolExplicit = false;
      stepWithinEnreg = props.getProperty("fileIn.stepWithinEnreg", "")
      unitStep = props.getProperty("fileIn.unitStep")
      // En cas de pas constant entre 2 enregistrements obligation de travailler en mono-thread
      if (stepWithinEnreg.startsWith("val=")) {
        isStepWithValEquals = true
        nbActors = 1
        // calcul du pas
        unitStep match {
          case "ms" => lgStep = stepWithinEnreg.substring(4).trim.toLong
          case "seconds" => lgStep = stepWithinEnreg.substring(4).trim.toLong * 1000
          case "s" => lgStep = stepWithinEnreg.substring(4).trim.toLong * 1000
          case "second" => lgStep = stepWithinEnreg.substring(4).trim.toLong * 1000
          case "nanos" => lgStep = stepWithinEnreg.substring(4).trim.toLong / 1000000
          case "micros" => lgStep = stepWithinEnreg.substring(4).trim.toLong / 1000
          case _ => lgStep = stepWithinEnreg.substring(4).trim.toLong
        }

      } else {
        // On reutilise ici multTms
        isStepWithValEquals = false
        unitStep match {

          case "ms" => multTms = 1
          case "s" => multTms = 1000
          case "seconds" => multTms = 1000
          case "second" => multTms = 1000
          case "mn" => multTms = 60000
          case "h" => multTms = 3600000
          case _ => multTms = 1
        }
      }
    }

    if (props.getProperty("filesOut.generateAllAveragesOnly", "false") == "false") {
      allAveragesOnly = false
    } else {
      allAveragesOnly = true
    }
    patternDeb = props.getProperty("fileIn.startEnrReg").r
    patternFin = props.getProperty("fileIn.finEnrReg", "").r
    // System.out.println("patternFin =" + patternFin.toString());
    // System.out.println("patternDeb =" + patternDeb.toString());
    if (null != patternFin
      && patternDeb.toString().equals(patternFin.toString())) {
      debEqualsFin = true
    }

    stepAgg = props.getProperty("fileIn.stepAgg").toInt
    csvSeparator = props.getProperty("filesOut.fsOut", ";");

    strSdfCsv = props.getProperty("filesOut.dateFormatOut")
    sdfCsv = new SimpleDateFormat(
      props.getProperty("filesOut.dateFormatOut"))
    println("ScaParserMain.strSdfCsv=" + strSdfCsv)
    dateInputRegexp = Pattern.compile(props
      .getProperty("fileIn.dateRegex"))
    dateInputReg = props.getProperty("fileIn.dateRegex").r
    //println("fileIn.dateFormatIn=" + props.getProperty("fileIn.dateFormatIn"))
    if (props.getProperty("fileIn.dateFormatIn", "") != "" && !props.getProperty("fileIn.dateFormatIn").contains("dateInMillis")) {
      // dateTimeFormatter = DateTimeFormat.forPattern(props.getProperty("fileIn.dateFormatIn")).withLocale(currentLocaleIn)
      simpleDateFormat = new SimpleDateFormat(props.getProperty("fileIn.dateFormatIn"), currentLocaleIn)
    }

    var cal: Calendar = Calendar.getInstance()
    var sdf1 = new SimpleDateFormat("yyyyMMdd-HHmmss");
    dateF = sdf1.format(cal.getTime());
    reportCsvDirectory = props
      .getProperty("filesOut.pathDir")

    val dirCsv: File = new File(reportCsvDirectory);
    if (!dirCsv.exists()) {
      dirCsv.mkdir()

    }
    listValues = props.getProperty("values.names", "")
    chargementFunctions(listValues)
    listPivots = props.getProperty("pivots.names", "")
    var strtkVal = new StringTokenizer(listValues, " ")
    var strtkPiv = new StringTokenizer(listPivots, " ")
    //   System.out.println("listPivots=" + listPivots)
    //   System.out.println("listValues=" + listValues)

    sizeValues = strtkVal.countTokens();
    sizePivots = strtkPiv.countTokens();

    if (sizeValues > 0) {
      tabStrFilesCsv = new Array[String](1 + sizePivots)
      tabFilterPiv = new Array[Int](1 + sizePivots)
      tabFilterVal = new Array[Int](sizeValues)

    } else {

      tabStrFilesCsv = new Array[String](sizePivots)
      tabFilterPiv = new Array[Int](sizePivots)
      tabFilterVal = null;
    }

    tabActorsFilesGenerated = Array.ofDim(nbActors, tabStrFilesCsv.length)

    var tmpRaf: RandomAccessFile = null;
    fsOut = props.getProperty("filesOut.fsOut", ";")

    if (sizeValues > 0) {
      var tmpNamesValues: Array[String] = new Array(sizeValues)
      var tmpExt1Values: Array[String] = new Array(sizeValues)
      var tmpExt2Values: Array[String] = new Array(sizeValues)
      var tmpUnitValues: Array[String] = new Array(sizeValues)
      var tmpUnitScales: Array[String] = new Array(sizeValues)
      var tmpPat2Values: Array[Regex] = new Array(sizeValues)

      if ((sizeValues + sizePivots) >= 1) {

        allAverage = new RandomAccessFile(reportCsvDirectory
          + File.separator + "allAverages.csv", "rw")

        allAverage.writeBytes("Time")
      }
      var i = 0;

      // JLP Debut Modif Creation fichier CSV + entete

      // Creation du fichier CSV sans Pivot avec toutes les valeurs

      var nameOfFile: String = "fic_";
      var strVal: Array[String] = new Array(sizeValues)
      var unites: Array[String] = new Array(sizeValues)
      var kk = 0;
      strtkVal = new StringTokenizer(listValues, " ");

      while (strtkVal.hasMoreTokens()) {
        strVal(kk) = strtkVal.nextToken();

        tmpNamesValues(kk) = strVal(kk)

        tmpExt1Values(kk) = props
          .getProperty("values.reg1."
            + strVal(kk), "")
        tmpExt2Values(kk) = props
          .getProperty("values.reg2."
            + strVal(kk), "");
        //        System.out.println("Val pure i=" + i + "; tmpNamesValues="
        //          + tmpNamesValues(kk) + " ; tmpExt1Values="
        //          + tmpExt1Values(kk) + " ;tmpExt2Values="
        //          + tmpExt2Values(kk));

        if (null != tmpExt2Values(kk)
          && tmpExt2Values(kk).length() > 1) {
          tmpPat2Values(kk) = tmpExt2Values(kk).r
        } else {
          tmpPat2Values(kk) = null;
        }
        tmpUnitValues(kk) = props
          .getProperty("values.unit."
            + strVal(kk), "");
        tmpUnitScales(kk) = props
          .getProperty("values.scale."
            + strVal(kk), "");
        unites(kk) = " (" + tmpUnitValues(kk) + ")";

        nameOfFile += "_" + strVal(kk);
        kk += 1

      }

      if (nameOfFile.length > scaViewerProperties.getProperty("scaviewer.filenamelength", "100").toInt) nameOfFile = "fic_AllValues"
      nameOfFile += ".csv";
      tabStrFilesCsv(i) = nameOfFile;
      if (!allAveragesOnly) {

        tmpRaf = new RandomAccessFile(reportCsvDirectory
          + File.separator + tabStrFilesCsv(i), "rw")
        var title: String = "Time" + fsOut
        val lenVal = tmpNamesValues.length
        for (ij <- 0 until lenVal) {

          if (tmpUnitValues(ij).equals("ms")
            || tmpUnitValues(ij).equals("millis")
            || tmpUnitValues(ij).indexOf("second") >= 0
            || tmpUnitValues(ij).indexOf("micros") >= 0) {
            title += strVal(ij) + "_Avg" + unites(ij) + fsOut +
              strVal(ij) + "_Max" + unites(ij) + fsOut +
              strVal(ij) + "_Min" + unites(ij) + fsOut +
              strVal(ij) + "_Rate (req/s)" + fsOut +
              strVal(ij) + "_Count_" +
              (this.stepAgg / 1000) + "s (req)" + fsOut +
              strVal(ij) + "_Sum" + fsOut + strVal(ij) +
              "_Concurrent_count (req)" + fsOut

          } else {

            title += strVal(ij) + "_Avg" + unites(ij) + fsOut +
              strVal(ij) + "_Max" + unites(ij) + fsOut +
              strVal(ij) + "_Min" + unites(ij) + fsOut +
              strVal(ij) + "_Rate (req/s)" + fsOut +
              strVal(ij) + "_Count_" +
              (this.stepAgg / 1000) + "s (req)" + fsOut +
              strVal(ij) + "_Sum" + fsOut
          }

        }
        for (ij <- 0 until lenVal) {
          allAverage.writeBytes(fsOut + strVal(ij) + "_Avg"
            + unites(ij))
        }
        title = title
          .substring(0, title.length() - fsOut.length()) + "\n"

        tmpRaf.writeBytes(title);

        tmpRaf.close();

      } else {
        // Allaverage Only
        val lenVal = tmpNamesValues.length
        for (ij <- 0 until lenVal) {
          allAverage.writeBytes(fsOut + strVal(ij) + "_Avg" + unites(ij));
        }

      }

      //   Fin Modif Creation fichier CSV Sans PIV et en-tete
      // associe

      i += 1 // On gere les fichiers CSV avec Pivots
      // Debut Creation des Fichiers CSV avec Pivots

      var tmpNamesPivots: Array[String] = new Array(1 + sizePivots)
      var tmpExt1Pivs: Array[String] = new Array(1 + sizePivots)
      var tmpExt2Pivs: Array[String] = new Array(1 + sizePivots)
      var tmpPat2Pivs: Array[Regex] = new Array(1 + sizePivots)
      strtkPiv = new StringTokenizer(listPivots, " ");
      var m = 1;
      val tabPiv: Array[String] = new Array(1 + sizePivots)
      strtkPiv = new StringTokenizer(listPivots, " ")
      tabPiv(0) = null
      while (strtkPiv.hasMoreTokens()) {

        tabPiv(m) = strtkPiv.nextToken();
        tmpNamesPivots(m) = tabPiv(m)
        tmpExt1Pivs(m) = props
          .getProperty("pivots.reg1."
            + tabPiv(m), "")
        tmpExt2Pivs(m) = props
          .getProperty("pivots.reg2."
            + tabPiv(m), "")

        if (null != tmpExt2Pivs && tmpExt2Pivs(m).length() > 0) {
          tmpPat2Pivs(m) = tmpExt2Pivs(m).r
        } else {
          tmpPat2Pivs(m) = null
        }

        var nameOfFile2 = "fic_" + tabPiv(m)

        var strValues = ""
        for (mm <- 0 until sizeValues) {
          strValues += "_" + strVal(mm)
        }
        if ((nameOfFile2 + strValues).length > scaViewerProperties.getProperty("scaviewer.filenamelength", "100").toInt) {
          nameOfFile2 += "_AllValues"
        }
        nameOfFile2 += ".csv";
        this.tabStrFilesCsv(i) = nameOfFile2
        m += 1
        i += 1;
      }

      // Le tableau des noms de fichier est cree et rempli

      // Creation des fichiers et des titres associ�s
      // Le fichier sans PIV a �t� cree plus haut On part de de la
      // valeur 1

      for (i1 <- 1 until (1 + sizePivots)) {
        if (!allAveragesOnly) {

          tmpRaf = new RandomAccessFile(reportCsvDirectory
            + File.separator + tabStrFilesCsv(i1), "rw")
          var title = "Time" + fsOut
          for (ij <- 0 until sizeValues) {
            if (tmpUnitValues(ij).equals("ms")
              || tmpUnitValues(ij).equals("millis")
              || tmpUnitValues(ij).indexOf("second") >= 0
              || tmpUnitValues(ij).indexOf("micros") >= 0) {

              title += tabPiv(i1) + "_" + strVal(ij) + "_Avg" +
                unites(ij) + fsOut + tabPiv(i1) +
                "_" + strVal(ij) + "_Max" +
                unites(ij) + fsOut + tabPiv(i1) +
                "_" + strVal(ij) + "_Min" +
                unites(ij) + fsOut + tabPiv(i1) +
                "_" + strVal(ij) + "_Rate (req/s)" +
                fsOut + tabPiv(i1) + "_" +
                strVal(ij) + "_Count_" +
                (this.stepAgg / 1000) + "s (req)" + fsOut +
                tabPiv(i1) + "_" + strVal(ij) +
                "_Sum" + fsOut + tabPiv(i1) + "_" +
                strVal(ij) + "_Concurrent_count (req) " + fsOut

            } else {
              title += tabPiv(i1) + "_" + strVal(ij) + "_Avg" +
                unites(ij) + fsOut + tabPiv(i1) +
                "_" + strVal(ij) + "_Max" +
                unites(ij) + fsOut + tabPiv(i1) +
                "_" + strVal(ij) + "_Min" +
                unites(ij) + fsOut + tabPiv(i1) +
                "_" + strVal(ij) + "_Rate (req/s)" +
                fsOut + tabPiv(i1) + "_" +
                strVal(ij) + "_Count_" +
                (this.stepAgg / 1000) + "s (req)" + fsOut +
                tabPiv(i1) + "_" + strVal(ij) + "_Sum" + fsOut

            }

          }
          // for (ij <- 0 until tmpNamesPivots.length) {
          var lenVal = tmpNamesValues.length
          for (ij <- 0 until lenVal) {
            allAverage.writeBytes(fsOut + tabPiv(i1) + "_" +
              strVal(ij) + "_Avg" + unites(ij))
          }
          title = title.substring(0,
            title.length() - fsOut.length()) + "\n"

          tmpRaf.writeBytes(title);

          tmpRaf.close();

        } else {

          // Remplir titre allAverage seulement
          // for (ij <- 0 until tmpNamesPivots.length) {
          var lenVal = tmpNamesValues.length
          for (ij <- 0 until lenVal) {
            allAverage.writeBytes(fsOut + tabPiv(i1) + "_" + strVal(ij) + "_Avg" + unites(ij));
          }
        }
      }
      allAverage.writeBytes("\n");
      allAverage.close();
      // Fin Creation Fichiers CSV avec Pivots
      // En cas de pas constant entre 2 enregistrement obligation de travailler en mono-thread
      if (stepWithinEnreg.startsWith("val=")) nbActors = 1
      //System.out        .println("ScaViewver Parser Logs: Fin de creation des structures des  fichiers csv");
      // Debut creation des structures d accumulation des donnees par
      // pivot

      // Creation de la structure sans Pivot indice 0
      // A creer ici pour tous les threads
      for (jk <- 0 until nbActors) {
        tabActorsFilesGenerated(jk)(0) = new GeneratedDatasForCSVFiles(new ValuesColumns(tmpNamesValues, tmpExt1Values, tmpPat2Values, tmpUnitValues, tmpUnitScales), null,
          tmpNamesPivots(0), new HashMap[Long, MyCumulEnregistrement]());

        // Creation des autres structures
        for (i2 <- 1 until (1 + sizePivots)) {

          tabActorsFilesGenerated(jk)(i2) = new GeneratedDatasForCSVFiles(new ValuesColumns(tmpNamesValues, tmpExt1Values, tmpPat2Values, tmpUnitValues, tmpUnitScales),
            new PivotColumn(tmpNamesPivots(i2),
              tmpExt1Pivs(i2), tmpPat2Pivs(i2)),
            tmpNamesPivots(i2), new HashMap[Long, MyCumulEnregistrement]())

        }
      }

    } else {
      // On ne traite pas sans valeur creer une structure Bidon
      System.out
        .println("It is mandatory to have at least a value. It can be a useless value as date of the enregistrement");
      null
    }

    stepAgg = props.getProperty("fileIn.stepAgg").toLong
    // System.out.println("fieldSeparator =|"+fieldSeparator+"|");
    fileIn = new File(
      props.getProperty("fileIn.pathFile"))
    var rootFileIn = fileIn.getAbsolutePath.substring(0, fileIn.getAbsolutePath.lastIndexOf(File.separator))
    var fileOnly = fileIn.getAbsolutePath.substring(fileIn.getAbsolutePath.lastIndexOf(File.separator) + 1)

    if (extractBool) {
      fileOutFilter = new File(rootFileIn + File.separator + "filtered_" + fileOnly)
    }

    if (props.getProperty("advanced.multiCsvFiles", "true").equals(
      "true")) {
      boolMultiCsvFiles = true;
    } else {
      boolMultiCsvFiles = false;
    }

    if (fileIn.getName().endsWith(".gz")) {

      this.determinerLineEnd(fileIn);
      reader = initReader(fileIn);
      var raf = new RandomAccessFile(fileIn, "r");
      if (patternFin.toString().length() < 1
        && raf.length() > 10000000) {
        fileOutFilter = new File(rootFileIn+ File.separator + "filtered_" + fileOnly)
        writer = new BufferedWriter(
          new OutputStreamWriter(new GZIPOutputStream(
            new FileOutputStream(fileOutFilter))));
        estimated = true;
        estimationFichier(reader, writer);
        raf.close();

      } else {

        while (reader.readLine() != null) {
          nbLinesOfFile += 1
        }
      }
      gap = scala.math.max(1, this.nbLinesOfFile / 20);
      reader.close();

      if (extractBool) {
        writer = new BufferedWriter(
          new OutputStreamWriter(new GZIPOutputStream(
            new FileOutputStream(fileOutFilter))));
      }
    } else {

      this.determinerLineEnd(fileIn);
      reader = initReader(fileIn);
      var raf = new RandomAccessFile(fileIn, "r");
      if (patternFin.toString().length() < 1
        && raf.length() > 100000000) {
        fileOutFilter = new File(rootFileIn + File.separator+ "filtered_" + fileOnly)
        estimated = true;
        writer = new BufferedWriter(new OutputStreamWriter(
          (new FileOutputStream(fileOutFilter))));
        estimationFichier(reader, writer);
        raf.close();

      } else {
        while (reader.readLine() != null) {
          nbLinesOfFile += 1
        }
      }
      gap = scala.math.max(1, nbLinesOfFile / 20);
      reader.close();

      if (extractBool) {
        writer = new BufferedWriter(new OutputStreamWriter(
          (new FileOutputStream(fileOutFilter))));
      }
    }
    //    for (i <- 0 until this.tabStrFilesCsv.length) {
    //      System.out.println("ScaParserMain : tabStrFilesCsv[" + i + "]=" + tabStrFilesCsv(i))
    //    }
    //    System.out.println("ParserMainInMemoryGo DefaultLocale = " + Locale.getDefault());
    // positionnement de la date de debut de decalage
    if (props.getProperty("fileIn.explicitDate").equalsIgnoreCase("true")) {

      if (props.getProperty("fileIn.dateFormatIn").contains("dateInMillis")) {
        isTimeInMillis = true
        positionnerDate0InMillis();
        // System.out.println("dateInMillis dateIndice0 = " + dateIndice0);
      } else {
        isTimeInMillis = false
        positionnerDate0ExplicitFormat()
        //System.out.println("explicitDate dateIndice0 = " + dateIndice0);
      }

    } else {
      // cas impicit avec date de debut positionne
      isTimeInMillis = false
      positionnerDate0ImplicitFormat();
      //System.out.println("Implicit Date dateIndice0 = " + dateIndice0);
    }
    System.out.println("ScaParserMain.isTimeInMillis=" + isTimeInMillis)
    construireTabTypFilters();

    val lenVal = this.tabFilterVal.length;
    val lenPiv = this.tabFilterPiv.length;

    correctDate match {

      case -1 => isDebDate = true

      case 1 => isDebDate = false

      case 0 => isDebDate = props.getProperty("advanced.isDebDate", "true").toBoolean

    }

    // conf.rbisDebDate.selected = isDebDate

    if (modeDebug) {
      this.logTrace.append("ParserMainInMemoryParser : avant Parsing\n")
    } else
      System.out.println("ParserMainInMemoryParser : avant Parsing");
    lancerParsing(conf)

  }

  def lancerParsing(conf: java.util.Properties) {

    compteurGlobalRead = 0;
    compteurGlobalTreated = new AtomicInteger(0)
    val len = tabFilterPiv.length;
    deb = System.currentTimeMillis();
    if (nbActors < 1) {
      nbActors = 1;
    }
    //println("lancerParsing coucou 0")
    tabBoolSolversStopped = Array.ofDim(nbActors)
    solvers = Array.ofDim(nbActors)
    //	readers = new BufferedReader[nbThreads];
    // nouveau en Scala initialiser un reader unique

    // Traiter ici le cas gz 
    var fReader: FileReader = null
    var gzipReader: GZIPInputStream = null
    if (!fileIn.getAbsolutePath.endsWith(".gz")) {
      fReader = new FileReader(fileIn)
      //scaviewer.filesstat.sizeBuffer
      //reader = new BufferedReader(fReader)
      reader = new BufferedReader(fReader, this.props.getProperty("scaviewer.filesstat.sizeBuffer", "1024000").toInt)
    } else {
      var fis = new FileInputStream(fileIn)
      gzipReader = new GZIPInputStream(fis)
      reader = new BufferedReader(new InputStreamReader(gzipReader), this.props.getProperty("scaviewer.filesstat.sizeBuffer", "1024000").toInt)
    }
    myReader = getMyReader()
    // Trouver la bonne methode de read

    if (fileInIncludeReg == "" && fileInExcludeReg == "") {
      typeRead = WITHOUT_INCLUDE_WITHOUT_EXCLUDE
      // methode read(relicat: String = "", bufReader: BufferedReader):
    } else if (fileInIncludeReg.length > 0 && fileInExcludeReg == "") {
      typeRead = WITH_INCLUDE_WITHOUT_EXCLUDE
      // methode readWithInclFilter(relicat: String, bufReader: BufferedReader,filtIncl:String)
    } else if (fileInIncludeReg == "" && fileInExcludeReg.length > 0) {
      typeRead = WITHOUT_INCLUDE_WITH_EXCLUDE
      // methode readWithExclFilter(relicat: String, bufReader: BufferedReader,filtExcl:String)
    } else if (fileInIncludeReg.length > 0 && fileInExcludeReg.length > 0) {
      typeRead = WITH_INCLUDE_WITH_EXCLUDE
      // methode readWith2Filter(relicat: String, bufReader: BufferedReader,filtIncl:String,filtExcl:String)
    }

    //JLP 
    //  println("lancerParsing coucou 1")

    // test avec modification de la taille des mailBox

    val config = ConfigFactory.load.getConfig("scaviewer.mailbox")
    val system = ActorSystem("MySystem", config)

    for (j <- 0 until nbActors) {

      solvers(j) = system.actorOf(Props(new ParserActor(j)), "parser_" + j.toString)

    }
    val readerActor = system.actorOf(Props(new ReaderActor(solvers, myReader)), "readerActor")
    readerActor ! "start"

    val logger = system.actorOf(Props[LoggerActor], "logger")
    logger ! "start"
    boolStopGen = false;
    while (boolStopGen == false) {
      // On attend la fin de tous les thraeds
      boolStopGen = true;
      try {
        // //
        for (i <- 0 until nbActors) {
          // //
          boolStopGen = boolStopGen && tabBoolSolversStopped(i)
          // //
        }
        // //
        // println("lancerParsing coucou 2 boolStopGen=" + boolStopGen)
        if (!boolStopGen) {
          ta.text = "number of read records  =" + compteurGlobalRead + " / " + nbLinesOfFile +
            "\nnumber of treated records =" + compteurGlobalTreated.get + " / " + nbLinesOfFile
        } else {
          ta.text = "number of read records  =" + compteurGlobalRead + " / " + nbLinesOfFile +
            "\nnumber of treated records =" + compteurGlobalTreated.get + " / " + nbLinesOfFile
          ta.text += "\nAll Parser Threads terminated in " + (System.currentTimeMillis - deb) + " milliseconds"
        }
        Thread.sleep(1000);

      } catch { case e: InterruptedException => e.printStackTrace() }

    }
    readerActor ! "stop"
    // Arreter le logger
    logger ! "stop"

    //println("lancerParsing coucou 3")
    //logger.stop();
    if (extractBool) {
      try {
        writer.close()
      } catch {
        case e: IOException => e.printStackTrace()
      }

    }
    println("lancerParsing Avant generation csv files ")
    if (ScaParserMain.modeDebug) {
      ScaParserMain.logTrace.append("lancerParsing Avant generation csv files\n ")
      for (idActor <- 0 until nbActors) {
        for (idMetric <- 0 until tabStrFilesCsv.length) {
          var myLen = ScaParserMain.tabActorsFilesGenerated(idActor)(idMetric).hmCumulEnr.size
          var iter = ScaParserMain.tabActorsFilesGenerated(idActor)(idMetric).hmCumulEnr.iterator
          ScaParserMain.logTrace.append(" ScaParserMain.tabActorsFilesGenerated(" + idActor + ")(" + idMetric + ").hmCumulEnr.size=" + ScaParserMain.tabActorsFilesGenerated(idActor)(idMetric).hmCumulEnr.size + "\n")
          iter foreach { tuple =>
            // while(iter.hasNext){

            ScaParserMain.logTrace.append(" ScaParserMain.tabActorsFilesGenerated(" + idActor + ")(" + idMetric + ").hmCumulEnr =>" + tuple._1 + "\n")
          }
        }
      }
    }

    // fermer le fichier
    reader.close
    val debCsv = System.currentTimeMillis
    system.shutdown

    if (nbActors == 1) {
      // println("lancerParsing coucou 4")
      genererFichiersCsv()
      //  println("lancerParsing coucou 4bis")
    } else {
      // Ramener toutes les donn�es dans les structures du thread 0
      //println("lancerParsing coucou 5")

      genererFichiersCsvActor0()
      // println("lancerParsing coucou 5bis")
    }

    // Nettoyer les structures

    for (i <- 0 until nbActors) {
      for (j <- 0 until tabStrFilesCsv.length) {
        tabActorsFilesGenerated(i)(j).hmCumulEnr.clear()
        tabActorsFilesGenerated(i)(j) = null

      }
    }
    tabActorsFilesGenerated = null;
    ta.text += "\nCreation csv files in  " + (System.currentTimeMillis - debCsv) + " milliseconds"
    if (modeDebug) {
      if (null != logTrace) logTrace.close
    }
    //  println("lancerParsing coucou 6")
    //  conf.configSystem.shutdown

    // nettoyer les fichiers CSV avec 1 seule ligne 

    for (fileCsv <- this.tabStrFilesCsv) {
      var tmpraf: RandomAccessFile = null
    
      try {
        tmpraf = new RandomAccessFile(reportCsvDirectory
          + File.separator +fileCsv, "r");
        val line1 = tmpraf.readLine()
        val line2 = tmpraf.readLine()
        tmpraf.close()
        if (null == line2){
          new File(reportCsvDirectory
          + File.separator +fileCsv).delete()
          ta.text += "\nDeleting file with no data : "+ reportCsvDirectory  + File.separator +fileCsv+"\n"
          println("Deleting file with no data : "+reportCsvDirectory    + File.separator + fileCsv+"\n")
        } 
        
        
      } catch {
        case _: Throwable =>
      }

    }
    ta.text += "\nGlobal parsing duration = " + (System.currentTimeMillis - deb) + " milliseconds"
  }

  private def genererFichiersCsv() {
    var existsAllAverage = false
    var fullPathAllAverages = ""
    var buf: StringBuffer = null
    var line: String = null
    var tmpRaf: RandomAccessFile = null;
    //
    // // on clos tous les enregistrements
    val debCloture = System.currentTimeMillis()
    val len = this.tabStrFilesCsv.length

    var tabActorsCloreEnr: Array[ActorRef] = new Array(len)
    tabBoolStopped = new Array(len)
    val system = ActorSystem("MySystem")
    println("Avant  creation cloture Enregistrement de " + len + " actors")
    for (i <- 0 until len) {
      tabBoolStopped(i) = false
      tabActorsCloreEnr(i) = system.actorOf(Props(new ActorCloreEnr(i)), "closer_" + i.toString)
      println(" creation cloture Enregistrement actor =" + i)
    }
    //
    for (i <- 0 until len) {
      tabActorsCloreEnr(i) ! "start"
      println("demarrage cloture Enregistrement actor =" + i)
    }
    var allstopped = false
    while (allstopped == false) {
      allstopped = true;
      for (i <- 0 until len) {
        allstopped = allstopped && tabBoolStopped(i)
      }
      try {
        Thread.sleep(100);
      } catch {
        case e: InterruptedException => e.printStackTrace()
      }
    }
    for (i <- 0 until len) {
      tabActorsCloreEnr(i) ! "stop"
      println("stop cloture Enregistrement actor =" + i)
    }
    System.out.println("Fin de cloture des enregistements : "
      + (System.currentTimeMillis() - debCloture));
    if (allAveragesOnly == true) {
      println(" generation AllAverage seul")
      genererAllAverageSeul();

    } else {
      // on genere tout
      // genererAllAverageSeul();
      println(" generation de tout")
      var tabActorsGenererTout: Array[ActorRef] = new Array(len)
      tabLongDeb = new Array(len)
      tabLongFin = new Array(len)
      for (i <- 0 until len) {
        tabBoolStopped(i) = false
        tabLongDeb(i) = Long.MaxValue
        tabLongFin(i) = 0

        tabActorsGenererTout(i) = system.actorOf(Props(new ActorGenererTout(i)), "genAll_" + i.toString)

      }
      for (i <- 0 until len) {
        tabBoolStopped(i) = false

        tabActorsGenererTout(i) ! "start"

      }
      allstopped = false;
      while (allstopped == false) {
        allstopped = true;
        for (i <- 0 until len) {
          allstopped = allstopped && tabBoolStopped(i)
        }
        try {
          Thread.sleep(100)
        } catch {
          case e: InterruptedException => e.printStackTrace()
        }
      }
      system.shutdown

      genererAllAverageSeul();
    }

    if (this.showAllAverages == true
      && new File(reportCsvDirectory + File.separator
        + "allAverages.csv").exists() && !modeDebug) {
      fullPathAllAverages = reportCsvDirectory + File.separator + "allAverages.csv"

      afficherAllAverages(fullPathAllAverages);

    }

  }
  final private def genererFichiersCsvActor0() {

    var existsAllAverage = false
    var fullPathAllAverages = ""
    var buf: StringBuffer = null
    var line: String = ""
    var tmpRaf: RandomAccessFile = null
    //
    // // on clos tous les enregistrements
    var debCloture = System.currentTimeMillis()
    val len = this.tabStrFilesCsv.length
    var tabActorsCloreEnr: Array[ActorRef] = new Array(len)
    tabBoolStopped = new Array(len)
    val system = ActorSystem("MySystem")
    //println("Avant  creation cloture Enregistrement de " + len + " actors")
    for (i <- 0 until len) {
      tabBoolStopped(i) = false
      // println(" creation cloture Enregistrement actor =" + i)
      tabActorsCloreEnr(i) = system.actorOf(Props(new ActorCloreEnr(i)), "closer_" + i.toString)

    }
    //
    for (i <- 0 until len) {
      // println("demarrage cloture Enregistrement actor =" + i)
      tabActorsCloreEnr(i) ! "start"
    }
    var allstopped = false
    while (allstopped == false) {
      allstopped = true;
      for (i <- 0 until len) {
        allstopped = allstopped && tabBoolStopped(i)
      }
      try {
        Thread.sleep(100);
      } catch {
        case e: InterruptedException => e.printStackTrace()
      }
    }
    for (i <- 0 until len) {
      tabActorsCloreEnr(i) ! "stop"
      println("stop cloture Enregistrement actor =" + i)
    }
    System.out.println("Fin de cloture des enregistements : "
      + (System.currentTimeMillis() - debCloture));

    // merger les enregistrements d'autres threads

    for (jj <- 1 until nbActors) {
      // on mets tout dans la structure de  l Actor 0
      for (j <- 0 until tabStrFilesCsv.length) {
        System.out.println("Traitement fichier "
          + this.tabStrFilesCsv(j));
        // On prend les tableaux du

        print("len  tabActorsFilesGenerated(" + jj + ")=")
        println(tabActorsFilesGenerated(jj).length)
        if (null == tabActorsFilesGenerated(jj)(j)) println("tabActorsFilesGenerated(" + jj + ")(" + j + ") is null")
        if (tabActorsFilesGenerated(jj).length > 0) {
          val iter = tabActorsFilesGenerated(jj)(j).hmCumulEnr.iterator

          iter.foreach { tuple =>
            // while (iter.hasNext)  {
            //var mapEntry=iter.next
            var cumul1: MyCumulEnregistrement = tuple._2

            var cumul0: MyCumulEnregistrement = null
            var tmp = tabActorsFilesGenerated(0)(j).hmCumulEnr.get(tuple._1)
            if (None != tmp) {
              cumul0 = tmp.get.merge(cumul1);
            } else {
              cumul0 = cumul1;
            }
            tabActorsFilesGenerated(0)(j).hmCumulEnr.put(tuple._1, cumul0);

          }
        }
      }

    }
    System.out.println("Fin de merge dans Actor 0 des enregistements");

    if (allAveragesOnly == true) {
      genererAllAverageSeul();

    } else {
      // on genere tout
      // genererAllAverageSeul();
      var tabActorsGenererTout: Array[ActorRef] = new Array(len)
      tabLongDeb = new Array(len)
      tabLongFin = new Array(len)
      for (i <- 0 until len) {
        tabBoolStopped(i) = false
        tabLongDeb(i) = Long.MaxValue
        tabLongFin(i) = 0L

        tabActorsGenererTout(i) = system.actorOf(Props(new ActorGenererTout(i)), "genAll_" + i.toString)

      }
      for (i <- 0 until len) {
        tabBoolStopped(i) = false

        tabActorsGenererTout(i) ! "start"

      }
      allstopped = false;
      while (allstopped == false) {
        allstopped = true;
        for (i <- 0 until len) {
          allstopped = allstopped && tabBoolStopped(i)
        }
        try {
          Thread.sleep(100);
        } catch {
          case e: InterruptedException => e.printStackTrace()
        }
      }

      genererAllAverageSeul();
    }
    system.shutdown
    if (this.showAllAverages == true && new File(reportCsvDirectory + File.separator + "allAverages.csv").exists() && !modeDebug) {
      fullPathAllAverages = reportCsvDirectory + File.separator + "allAverages.csv"

      afficherAllAverages(fullPathAllAverages)

    }

  }

  private def  now():Long={
    return System.currentTimeMillis();
  }
  final private def genererAllAverageSeul() {

    var tabAllAverages: Array[HashMap[Long, Array[Double]]] = new Array(tabStrFilesCsv.length)
    longDeb = Long.MaxValue
    longFin = 0
   // var deb=now()
    for (i <- 0 until this.tabStrFilesCsv.length) {

      tabAllAverages(i) = new HashMap()
      var len = tabActorsFilesGenerated(0)(i).hmCumulEnr.size
      var dateInMillis: Long = 0
      var compteurAllAverageSeul: Int = 0

      // Classer les enregistrement par cle croissante

      var hmTmp = tabActorsFilesGenerated(0)(i).hmCumulEnr

      var iter = hmTmp.iterator

      iter foreach { tuple =>
        //while (iter.hasNext){

        dateInMillis = tuple._1

        compteurAllAverageSeul += 1

        if (dateInMillis < this.longDeb) {
          longDeb = dateInMillis;
        }
        if (dateInMillis > this.longFin) {
          longFin = dateInMillis;
        }

        var enr = hmTmp.get(dateInMillis);

        tabAllAverages(i).put(dateInMillis, tuple._2.averages);

      }
      hmTmp.clear();
      hmTmp = null;
      // System.out.println(tabStrFilesCsv[i] + "compteurAllAverageSeul= "
      // + compteurAllAverageSeul);

    }
  //  println("traitement initial recuperer tous les averages =>" +(now()-deb));
  //  deb=now
    // Reprise de AllAverage pour boucher les trous des valeurs
    var tabDoubles: Array[Array[Double]] = Array.ofDim(this.tabStrFilesCsv.length, sizeValues)
    var len = this.tabStrFilesCsv.length

    for (i <- 0 until len) {
      for (j <- 0 until sizeValues) {

        tabDoubles(i)(j) = Double.NaN
      }
    }

    // deb=now()
    // for (varPeriod <- longDeb until longFin by stepAgg) {
    for (varPeriod <- longDeb to longFin by stepAgg) {
      var sum: Double = 0
      for (j <- 0 until len) {
        // System.out.println("tab[" + j + "]=" + tab[j]);

        if (None == tabAllAverages(j).get(varPeriod)) {

          // tabAllAverages[j].put(nbDates, tabStr[j]);
          tabAllAverages(j).put(varPeriod, tabDoubles(j))

          // tabAllAverages[j].put(nbDates, tabDoubles[j]);
        } else {

          for (kk <- 0 until sizeValues) {
            if (None != tabAllAverages(j).get(varPeriod))
              sum += scala.math.abs(tabAllAverages(j).get(varPeriod).get(kk))
          }

        }

      }
      if (sum == 0) {
        // on supprime la valeur
        for (j <- 0 until len) {
          tabAllAverages(j).remove(varPeriod)
        }
      }

    }

    //  println("traitement boucher les trous si sum !=0  averages =>" +(now()-deb));
    // Ecriture All Average
    val fullName = this.reportCsvDirectory + File.separator + "allAverages.csv";

    var raf: RandomAccessFile = null;
    try {
      raf = new RandomAccessFile(fullName, "rw");
      var size = raf.length();
      raf.seek(size);
    } catch {
      case e: FileNotFoundException => e.printStackTrace()
      case e: IOException => e.printStackTrace()
    }

  //  deb=now()
    // Traitement All Average
    //  for (varPeriod <- longDeb until longFin by stepAgg) {
    var totalNomVar = tabStrFilesCsv.length
    for (varPeriod <- longDeb to longFin by stepAgg) {
      var dateStr: String = this.sdfCsv.format(varPeriod)

      var strBuild: StringBuilder = new StringBuilder(dateStr).append(this.csvSeparator);
      var suppress = false;
      
      for (nomVar <- 0 until totalNomVar) {

        if (None != tabAllAverages(nomVar).get(varPeriod)) {
          suppress = false

          for (l <- 0 until sizeValues) {

            var value = tabAllAverages(nomVar).get(varPeriod).get(l)

            if (value.isNaN) {
              strBuild.append(this.csvSeparator);
            } else {
              strBuild.append(value).append(this.csvSeparator);
            }

          }

        } else {
          suppress = true
        }
      }
      strBuild.append("\n");

      try {
        if (!suppress && null != strBuild ) {

          raf.writeBytes(strBuild.toString());
        }
      } catch {

        case e: IOException => e.printStackTrace()
      }

    }
 //   println("Fin ecriture AllAverage =>"+(now()-deb))

    // destruct
//    deb=now()
   // var totalNomVar = this.tabStrFilesCsv.length
    for (nomVar <- 0 until totalNomVar) {
      tabAllAverages(nomVar).clear();
    }
    try {
      if (null != raf) {
        raf.close();
      }
    } catch {

      case e: IOException => e.printStackTrace()
    }
 //   println("Fin  AllAverageSeul =>"+(now()-deb))

  }

  private def nettoyagePanel() {
    SwingScaViewer.mainPanel.contents.clear()
    // println(" miNewProject choosed")
    ScalaChartingDyn.boolExamine = false
    ScalaChartingDyn.stop()
    ScaCharting.listFiles = List.empty
    if (!ScaCharting.listRaf.isEmpty) {
      ScaCharting.listRaf foreach ((raf) => if (null != raf) raf.close)
      ScaCharting.listRaf = List.empty
    }

    // ScaCharting.arrEnrichised = null
    SwingScaViewer.mainPanel.visible = false
    SwingScaViewer.mainPanel.visible = true
  }
  private def afficherAllAverages(fullPathAllAverages: String) {
    //TODO
    // println("afficherAllAverages")
    nettoyagePanel
    //ScaCharting.arrEnrichised = null
    // mainPanel.contents map { mainPanel.contents -= _ }
    ScaCharting.mainPanel = SwingScaViewer.mainPanel
    var path1 = SearchDirFile.searchYoungestDir(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject, SwingScaViewer.tmpProps.getProperty("scaviewer.prefixscenario").r).getAbsolutePath
    ScaCharting.mainPanel.contents += ScaCharting(path1, false)
    //    ScaCharting.chartPanel.setChart(ChartFactory.createTimeSeriesChart(null,
    //      null,
    //      null,
    //      null, // Mettre ici le dataSet
    //      false,
    //      true,
    //      true))
    ScaCharting.mainPanel.visible = false
    ScaCharting.mainPanel.visible = true
    var file = new File(fullPathAllAverages)
    //println("AffichageAll Average =" + fullPathAllAverages)
    ScaChartingListener.deapClear
    ScaCharting.arrEnrichised = new ArrayBuffer[StructTs]()
    var chartingInfo: ScaCharting.ChartingInfo = ScaCharting.ChartingInfo("", file.length(), file.lastModified(), props.getProperty("nbPoints", "300").toInt,
      true,
      "AVERAGE", true,";",0,"","",null,null,-1L)
    ScaCharting.listChartingInfo = chartingInfo :: Nil
    ScaCharting.listFiles = file :: Nil
    if (!ScaCharting.listRaf.isEmpty) {
      ScaCharting.listRaf foreach ((raf) => if (null != raf) raf.close)
      ScaCharting.listRaf = List.empty
    }
    var cca: CreateChartAndTable = new CreateChartAndTable(ScaCharting.listFiles)
    ScaCharting.tfSample.text = props.getProperty("nbPoints", "300")
    cca.createChartPanel
    ScaCharting.mainPanel.visible = false
    ScaCharting.mainPanel.visible = true
  }

  private def getMyReader(): MyReader = {
    if (null == patternFin || patternFin.toString == "") {
      //println("myReader  MyReaderLineByLine")
      new MyReaderLineByLine()
    } else if (null != patternFin && patternDeb.toString != patternFin.toString) {
      println("myReader   MyReaderMulti")
      println("patternDeb=|" + patternDeb.toString + "|")
      println("patternFin=|" + patternFin.toString + "|")
      new MyReaderMulti(patternDeb, patternFin)
    } else {
      println("myReader   MyReaderMultiDebEqFin")
      println("patternDeb=|" + patternDeb.toString + "|")
      new MyReaderMultiDebEqFin(patternDeb)
    }

  }
  def chargementFunctions(listValues: String): Unit =

    {

      if (listValues.length == 0) return
      val strtkVal = new StringTokenizer(listValues, " ");
      if (new File(System.getProperty("root") + File.separator + "myPlugins").exists()) {
        try {

          urlClassLoader = new URLClassLoader(Array[URL](new URL("file",
            "localhost", System.getProperty("root") + File.separator + "myPlugins"), new URL("file",
            "localhost", System.getProperty("root") + File.separator + "myPlugins" + File.separator + "myPlugins.jar")))
        } catch {
          case e: MalformedURLException => println(e.getMessage)
        }

      }
      var tmpNumColumn = ""
      var i: Int = 0
      while (strtkVal.hasMoreTokens()) {
        var strVal = strtkVal.nextToken();

        tmpNumColumn = props.getProperty("values.reg1." + strVal);

        if (tmpNumColumn.startsWith("function=")) {

          // on cree la classe et la methode
          var key = tmpNumColumn.split("=")(1)
          if (!key.startsWith("Conc")) {
            // Un nom de classe donne apres Function qui ne commence pas par Conc 
            // implique un seul Parsor Actor ( pas de concurrence possible)
            this.nbActors = 1
          }
          try {
            System.out
              .println("ScaParserMain Chargement Classe :|"
                + key + "|");
            System.out.println("ScaParserMain  Class.forName :|"
              + Class.forName(key, true, urlClassLoader) + "|");

            // Constructor[] cts= Class.forName(key,
            // true,urlClassLoader).getConstructors();

            var objUtil: Any = null;
            var objUtilBis: Any = null; // Pour les valeurs sans Pivot
            try {

              objUtil = Class.forName(key, true, urlClassLoader).newInstance();
              objUtilBis = Class.forName(key, true, urlClassLoader).newInstance();
            } catch {
              case e: IllegalArgumentException => e.printStackTrace()
              case e: InstantiationException => e.printStackTrace()
              case e: IllegalAccessException => e.printStackTrace()
            }
            // this.hmapClass.put(key, Class.forName(key,
            // true,urlClassLoader));
            key = key + "_" + i;
            this.hmapClass.put(key, objUtil.asInstanceOf[Object]);
            this.hmapClass.put(key + "Bis", objUtilBis.asInstanceOf[Object]);
            val tmp: Array[String] = Array()
            hmapMethod.put(key, hmapClass.get(key).get.getClass().getDeclaredMethod("retour", classOf[Array[String]]))
            //hmapMethod.put(key + "Bis", hmapClass.get(key).get.getClass().getDeclaredMethod("retour", classOf[Array[String]]))
            val metInit: Method = hmapClass.get(key).get.getClass().getDeclaredMethod("metInit", classOf[Array[String]])
            metInit.invoke(objUtil, null)

          } catch {
            case e: ClassNotFoundException => e.printStackTrace()
            case e: SecurityException => e.printStackTrace()
            case e: NoSuchMethodException => e.printStackTrace()
          }

        }
        i += 1
      }

    }
  def determinerLineEnd(fileIn: File) {

    var byte1: Byte = 0;
    var nbCar = 0;
    var buff: BufferedReader = null;
    val fis = new FileInputStream(fileIn)
    try {
      if (fileIn.getName().endsWith(".gz")) {

        buff = new BufferedReader(new InputStreamReader(
          new GZIPInputStream(fis)));

      } else {
        buff = new BufferedReader(new InputStreamReader(fis));

      }
    } catch {
      case e: FileNotFoundException => e.printStackTrace()
      case e: IOException => e.printStackTrace()
    }
    try {
      var bool = true
      while (bool) {
        byte1 = buff.read().asInstanceOf[Byte]
        if (byte1 == '\r' || byte1 == '\n') {
          nbCar += 1
          //System.out.println("Premier caractere " + byte1);
          byte1 = buff.read().asInstanceOf[Byte]
          if (byte1 == '\r' || byte1 == '\n') {
            nbCar += 1
            // System.out.println("deuxieme caractere " + byte1);
            //  System.out.println("Fichier DOS");

          }

          bool = false
        }

      }
      fis.close
      buff.close();

    } catch {
      case e: FileNotFoundException => e.printStackTrace()
      case e: IOException => e.printStackTrace()
    } finally {
      if (null != fis) {
        fis.close
      }
      if (buff != null) {

        buff.close();

      }
    }
    carFinLigne = nbCar
    // System.out.println(" fin de ligne : " + carFinLigne + " caractere(s)");

  }
  private def initReader(file: File): BufferedReader =
    {

      try {
        if (fileIn.getName().endsWith(".gz")) {

          return new BufferedReader(new InputStreamReader(
            new GZIPInputStream(new FileInputStream(fileIn))),
            sizeBufReader);

        } else {
          return new BufferedReader(new InputStreamReader(
            new FileInputStream(fileIn)), sizeBufReader);

        }
      } catch {
        case e: FileNotFoundException => e.printStackTrace()
        case e: IOException => e.printStackTrace()
      }
      return null;
      //JLP
    }
  def estimationFichier(reader: BufferedReader, writer: BufferedWriter) {

    var line = "";

    try {
      var lcount = 0
      var bool = true
      for (j <- 0 until nblinesForRapidDatation; if (bool)) {

        line = reader.readLine()
        if (null != line) {
          writer.write(line);
          writer.newLine();
          lcount += 1
        } else {
          bool = false
        }

      }
      writer.close();
      reader.close();
      val raf1 = new RandomAccessFile(fileIn, "r");
      val raf2 = new RandomAccessFile(fileOutFilter, "r");
      val lgRaf1 = raf1.length();
      val lgRaf2 = raf2.length();
      raf1.close();
      raf2.close();
      fileOutFilter.delete();
      nbLinesOfFile = (lcount * lgRaf1 / lgRaf2).toInt
    } catch {

      case e: IOException => e.printStackTrace()
    }

  }
  def positionnerDate0InMillis() {

    // reader = initReader(fileIn);
    var trouve = false;
    var line = "";
    //    val pat = Pattern.compile(props
    //      .getProperty("fileIn.dateRegex"));
    val str = props.getProperty("fileIn.dateFormatIn")
    multTms = 1L;
    val dateDebIncrement = props.getProperty("fileIn.startDate", "")
    var sdf: SimpleDateFormat = null
    val regTz = """\s(\+|-)\d{4}""".r
    if (None != regTz.findFirstIn(dateDebIncrement)) {
      sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss Z")
    } else {
      sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
    }

    var dateDebImplicit: Date = null

    if (str.indexOf(",") >= 0) {
      multTms = str.split(",")(1).toLong
    }

    //    while (!trouve) {
    //      try {
    //        line = reader.readLine();
    //      } catch {
    //
    //        case e: IOException => e.printStackTrace()
    //      }
    //
    //      val matcher = pat.matcher(line);
    //      if (matcher.find()) {
    //        println ("dateDebIncrement="+dateDebIncrement)
    //         println ("dateDebImplicit.getTime="+dateDebImplicit.getTime)
    //        System.exit(1)
    if (dateDebIncrement == "0" || dateDebIncrement == "1970/01/01 00:00:00" || dateDebIncrement == "") {
      //absolute time
      dateIndice0 = 0
    } else {
      //relative time
      try {

        dateDebImplicit = sdf.parse(dateDebIncrement);
      } catch {

        case e: ParseException => e.printStackTrace()
      }
      //dateIndice0 = dateDebImplicit.getTime() + multTms * matcher.group().toLong
      dateIndice0 = dateDebImplicit.getTime()
    }
    //        trouve = true;
    //        if (reader != null) {
    //          try {
    //            reader.close();
    //          } catch {
    //
    //            case e: IOException => e.printStackTrace()
    //          }

    // }
    // }

    // }

  }
  def positionnerDate0ExplicitFormat() {

    reader = initReader(fileIn);
    var trouve = false;
    var line = "";
    if (modeDebug) {
      System.out
        .println("props.getProperty(\"fileIn.dateFormatIn\")"
          + props.getProperty("fileIn.dateFormatIn",
            " no regexp date found"));
    }
    val pat = Pattern.compile(props
      .getProperty("fileIn.dateRegex"));

    //println("positionnerDate0ExplicitFormat:fileIn.dateRegex=" + pat.toString)
    while (!trouve) {
      try {
        line = reader.readLine();

      } catch {

        case e: IOException => e.printStackTrace()
      }

      val matcher = pat.matcher(line);
      if (matcher.find()) {

        val extract = matcher.group();
        val sdfExp = new SimpleDateFormat(props.getProperty("fileIn.dateFormatIn"), currentLocaleIn)
        if (modeDebug) {
          System.out
            .println("props.getProperty(\"fileIn.dateFormatIn\")"
              + props.getProperty("fileIn.dateFormatIn"));
          System.out.println("line=" + line);
          System.out.println("extract=" + extract);
          System.out.println("sdfExp =" + sdfExp + " ;currentLocaleIn=" + currentLocaleIn.toString)
        }

        try {
          // extract = extract.replaceAll("\\s+", " ");
          var date = sdfExp.parse(extract);
          dateIndice0 = date.getTime();
          trouve = true;

        } catch {

          case e: ParseException =>
            trouve = false
            e.printStackTrace()
        }

      }

    }
    if (reader != null) {
      try {
        reader.close();
        reader = null;
      } catch {

        case e: IOException => e.printStackTrace()
      }

    }

  }
  def positionnerDate0ImplicitFormat() {

    var trouve = false;

    var sdfImp = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss",
      currentLocaleIn);
    var date: Date = null;
    try {
      date = sdfImp.parse(props
        .getProperty("fileIn.startDate"));
      dateIndice0 = date.getTime();
    } catch {

      case e: ParseException => e.printStackTrace()
    }

  }
  def construireTabTypFilters() {

    // Traitement des Pivots

    val len = tabFilterVal.length

    for (jk <- 0 until nbActors) {
      for (i <- 0 until len) {

        var tabVal: ValuesColumns = tabActorsFilesGenerated(jk)(0).getValCols()
        if (null != tabVal.ext1Values(i)) {
          val str = tabVal.ext1Values(i)

          if (str.startsWith("function=")) {
            isWithFunctions = true
            // Cas de l acces en function
            tabFilterVal(i) = this.VAL_FUNCTION
          } else {
            // Cas de l acces en regexp pur sur la colonne1
            if (null != tabVal.pat2Values(i) && tabVal.pat2Values(i).toString.length > 0) {
              // cas val regexp 1 et extra regexp
              tabFilterVal(i) = this.VAL_REGEXP1_REGEXP2;
            } else {
              // cas val regexp 1 sans extraregexp
              tabFilterVal(i) = this.VAL_REGEXP1_SANSREGEXP2;
            }

          }

        }

      }

      val len2 = tabFilterPiv.length;

      for (i <- 0 until len2) {

        if (null != tabActorsFilesGenerated(jk)(i).getPivCol()) {
          var pivCol1 = tabActorsFilesGenerated(jk)(i).getPivCol().ext1Piv

          if (null != tabActorsFilesGenerated(jk)(i).getPivCol().pat2Piv) {

            tabFilterPiv(i) = PIV_REGEXP1_REGEXP2;

          } else {
            // Pas de deuxieme regexp pour PIV
            var isRegexp = false;
            var bool = true
            for (j <- 0 until tabRegexp.length; if (bool)) {
              if (pivCol1.contains(tabRegexp(j))) {
                isRegexp = true;
                bool = false
              }
            }
            if (!isRegexp) {
              // Une seule valeur Pure String
              tabFilterPiv(i) = FASTPIV1STRING;
            } else {
              // 1 seule valeur regexp
              tabFilterPiv(i) = PIV_REGEXP1_SANSREGEXP2;

            }
          }

        } else {
          // Sans PIV => Valeur seule
          tabFilterPiv(i) = SANSPIV;
        }

      }
    }

  }
}

