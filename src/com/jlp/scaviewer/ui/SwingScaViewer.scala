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
package com.jlp.scaviewer.ui

import scala.swing._
import scala.swing.SimpleSwingApplication
import javax.swing.JPanel
import javax.swing.ScrollPaneConstants
import scala.swing.ScrollPane.BarPolicy.Value
import scala.swing.Menu
import scala.swing.event.ActionEvent
//import scala.actors.Reaction
import scala.swing.event.ButtonClicked
import scala.swing.event.MouseClicked
import org.jfree.chart.ChartFactory
import scala.collection.mutable.ArrayBuffer
import com.jlp.scaviewerdyn.ui.ScalaChartingDyn
import com.jlp.scaviewer.ui.dialogs.NewProjectDialog
import java.io.File
import java.util.Properties
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.IOException
import java.awt.Font
import com.jlp.scaviewer.ui.dialogs.OpenProjectDialog
import com.jlp.scaviewer.ui.dialogs.CSVLogsDialog
import com.jlp.scaviewer.filestats.MyDialogStatsFile
import scala.swing.event.MouseEntered
import scala.swing.event.SelectionChanged
import javax.swing.event.MenuListener
import javax.swing.event.MenuEvent
import com.jlpapis.tools.MyDialogRegexp
import com.jlpapis.tools.MyDialogTimeInMillis
import com.jlpapis.tools.MyLogFilesChooser

import com.jlpapis.ssh.gui.CreateConnectionsSSH
import com.jlpapis.perf.aspects.gui.AspectsPerf
import com.jlpapis.jdbcConnect.JDBCConnect
import com.jlp.scaviewer.scalogparser.ui.MyDialogOpenLog
import com.jlp.scaviewer.scalogparser.ui.MyDialogOpenDirectParse
import com.jlp.scaviewer.scalogparser.ui.ConfigParser

import com.jlpapis.utils.MostYoungFolder
import com.jlp.scaviewer.scalogparser.DirectParserMain
import com.jlp.scaviewer.ui.dialogs.InfoScaViewer
import com.jlp.scaviewer.commons.utils.SearchDirFile
import com.jlp.scaviewer.ui.dialogs.MyDialogLogsFormat
import com.jlp.scaviewer.scalogparser.ScaParserMain
import javax.swing.JEditorPane
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.datatransfer.Clipboard
import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import javax.activation.DataHandler
import java.io.InputStream
import javax.swing.JOptionPane
import java.awt.Color
import com.jlp.scaviewer.ui.dialogs.MyDialogMyCommands
import com.jlp.scaviewer.tools.MyDialogSortLinesInFile
import com.jlpapis.tools.MyDialogDecToAndFromHex
import com.jlp.scaSSHconnect._
import com.jlp.scaviewer.tools.MyDialogSimpleDateFormat
import com.jlp.scaviewer.tools.CsvClean
import com.jlp.scaviewer.tools.CsvCleanAll
import com.jlp.scaviewer.tools.php._

object SwingScaViewer extends SimpleSwingApplication {

  var propsDate: Properties = new Properties();
  var boolShowHtml = false
  var dir = System.getProperty("root") + File.separator + "config";

  var f = new File(dir + File.separator + "scaViewer.properties");
  println("dir=" + dir)
  var tmpProps: Properties = new Properties();
  try {
    tmpProps.load(new FileInputStream(f));

  } catch {
    case e: FileNotFoundException =>
      // TODO Auto-generated catch block
      e.printStackTrace();
    case e: IOException =>

      // TODO Auto-generated catch block
      e.printStackTrace();

  }
  val pref: String = tmpProps.getProperty("scaviewer.prefixscenario")
  val sepName = tmpProps.getProperty("timeseries.nameTimeSeries.separator", ":_:")
  // static values
  var currentProject: String = ""
  // definition des chemins de config
  val initialTitle = "swingScaViewer Application " + Version.version

  // Definition des menus et menuItems
  val mainPanel = new BoxPanel(Orientation.Vertical)
  val mFile = new Menu("Files")
  val miNewProject = new MenuItem("New Project")
  val miOpenProject = new MenuItem("Open Project")
  val miCloseProject = new MenuItem("Close Project")
  val miAddCsvLogs = new MenuItem("Add dir CSV/Logs")
  val miSSH = new MenuItem("SSH Cnx uploads/downloads")
  miSSH.tooltip = "Handling connexions, uploads, downloads over SSH protocol"
  val miAspect = new MenuItem("Perf with AspectJ/Java Agent ")

  val miJdbc = new MenuItem("JDBC Requests")
  miJdbc.tooltip = "Connecting to Database ( Oracle, SqlServer,MySql,Postgres,Sybase) and request them"
  miAspect.tooltip = "Packaging a java agent, by weaving AspectJ. For performance infos"
  val miExit = new MenuItem("Exit")

  mFile.contents += miNewProject
  mFile.contents += miOpenProject
  mFile.contents += miCloseProject
  mFile.contents += miAddCsvLogs
  mFile.contents += miSSH
  mFile.contents += miAspect
  mFile.contents += miJdbc
  mFile.contents += miExit

  // Menu scaLogParser
  val miScaLogParser = new Button("ScaLogParser")
  miScaLogParser.tooltip = "Parsing horodated log files"
  miScaLogParser.borderPainted = false
  miScaLogParser.contentAreaFilled = false

  // Menu Viewer CSV
  val mViewer = new Menu("Viewers")
  mViewer.tooltip = "Charting CSV Files (TimeSeries for now)"
  val miScaCharting = new MenuItem("ScaCharting")
  miScaCharting.tooltip = "Static CVS file "
  val miScaChartingDyn = new MenuItem("ScaChartingDyn")
  miScaChartingDyn.tooltip = "Pseudo real time for modified CVS file "
  val miDirectView = new MenuItem("ParseAndView")
  miDirectView.tooltip = "Parse and Chart known format logs (JVM GC) "
  mViewer.contents += miScaCharting
  mViewer.contents += miScaChartingDyn
  mViewer.contents += miDirectView

  // Menu ScaFileStats
  val mScaFileStats = new Menu("ScaFileStats")
  mScaFileStats.tooltip = "<html>Parsing an horodated file <br/>and get statistics of a value filtered by a pivot</html>"
  val miStatDatas = new MenuItem("StatDatas")
  miStatDatas.tooltip = "Create a statistic from scrash"
  val mGenTemplStatDatas = new Menu("GenTemplStatDatas")

  // mGenTemplStatDatas.action=new Action("GenTemplStatDatas"){ def apply(){ "println Action for mGenTemplStatDatas" }}
  mGenTemplStatDatas.tooltip = "Create a statistic from a general template"
  val mLocalTemplStatDatas = new Menu("LocalTemplStatDatas")
  mLocalTemplStatDatas.tooltip = "Create a statistic from a local template"
  mScaFileStats.contents += (miStatDatas, mGenTemplStatDatas, mLocalTemplStatDatas)

  // Menu tools
  val mTools = new Menu("Tools")
  val miTestRegex = new MenuItem("Test Regex")
  miTestRegex.tooltip = "Testing regular expression against a text"
  val miDateInMillis = new MenuItem("Translate Date")
  miDateInMillis.tooltip = "Translate Date In Millis <=> Formated Date"
  val miConcatFile = new MenuItem("Concat Files")
  miConcatFile.tooltip = "Concat files and Gzip the result file"
  val miMergeLines = new MenuItem("Merge Lines")
  miMergeLines.tooltip = "Merge Lines In File"

  val miHorodateLogs = new MenuItem("Horodate logs")
  miHorodateLogs.tooltip = "Format record multi-lines logs, the logs must be horodated for each record at the beginning of the record"

  val miSortLinesInFile = new MenuItem("Sort Lines in File")
  miSortLinesInFile.tooltip = "Sorting lines in a file with one or more indexes using java regex"

  val miHexToFromDec = new MenuItem("Hex <=> Dec")
  miHexToFromDec.tooltip = "Translate in both direction Hexdecimal Number and Decimal Number"

  val miSdfTester = new MenuItem("SimpleDateFormat tester")
  miSdfTester.tooltip = "Tester of java SimpleDateFormat"
  val miCsvClean = new MenuItem("Clean current Csv repository ")
  miCsvClean.tooltip = "Keep only the last parsing in csv directory of the currrent project"
  val miCsvCleanAll = new MenuItem("Compact all Csv")
  miCsvCleanAll.tooltip = "Keep only the last parsing in csv directory of all projects. Can take long time"
  mTools.contents += (miTestRegex, miDateInMillis, miConcatFile, miHorodateLogs, miSortLinesInFile, miMergeLines, miHexToFromDec, miSdfTester, miCsvClean, miCsvCleanAll)

  // Menu MyCommands
  val mMyCommands = new Menu("MyCommands")
  // mMyCommands.foreground = Color.RED
  val miFromScratch = new MenuItem("From Scrash")
  miFromScratch.tooltip = "Create a Java/native command from scrash : Native Command/JVM parameters / Main Class or Jar archive / Application parameters"
  // miFromScratch.foreground = Color.RED
  val mGeneralCommands = new Menu("General Commands")
  mGeneralCommands.tooltip = "Launch a pre-defined commands shared by all projects"
  // mGeneralCommands.foreground = Color.RED
  val mLocalCommands = new Menu("Local Command")
  // mLocalCommands.foreground = Color.RED
  mLocalCommands.tooltip = "Launch a pre-defined commands for the current opened project"
  mMyCommands.contents += (miFromScratch, mGeneralCommands, mLocalCommands)

  // Menu PHP_Tools
  val mPHPTools = new Menu("PHP Tools")
  val miXDebug = new MenuItem("XDebug_Trace_Format_1")
  miXDebug.tooltip = "Analyse XDebug Trace with Format log = 1."
  val miXDebugProf = new MenuItem("XDebug_Profile")
  miXDebugProf.tooltip = "Analyse XDebugProfile with Format cacheGrind"
  val miXHprof = new MenuItem("XHprof Profile")
  miXHprof.tooltip = "Analyse XHprof Profile logs"
  mPHPTools.contents += (miXDebug, miXDebugProf, miXHprof)

  // Menu Help
  val miHelp = new Button("ScaViewer Infos")
  miHelp.tooltip = "ScaViewer Versions and infos corrected bugs"
  miHelp.borderPainted = false
  miHelp.contentAreaFilled = false

  var root = System.getProperty("root")

  // Definition de la Frame
  def top = new MainFrame {
    title = initialTitle

    menuBar = new MenuBar

    menuBar.contents += mFile
    menuBar.contents += miScaLogParser
    menuBar.contents += mViewer
    menuBar.contents += mScaFileStats
    menuBar.contents += mMyCommands
    menuBar.contents += mTools
    menuBar.contents += mPHPTools
    menuBar.contents += miHelp

    contents = mainPanel
    disableAll

    size = new Dimension(toolkit.getScreenSize.width - 30, toolkit.getScreenSize.height - 50)
    //  println("size" + size)
    //     miNewProject.action= Action(" miNewProject") {
    //     println(" miNewProject choosed")
    //    }
    //    mScaCharting.action= Action("mScaCharting") {
    //     println("ScaCharting choosed")
    //    }

    def enableAll() =
      {
        mMyCommands.enabled = true

        mMyCommands.repaint
        mViewer.enabled = true
        miAddCsvLogs.enabled = true
        miCloseProject.enabled = true
        mScaFileStats.enabled = true
        miSSH.enabled = true
        miAspect.enabled = true
        mTools.enabled = true
        miJdbc.enabled = true
        miScaLogParser.enabled = true
        miCsvClean.enabled = true
        mPHPTools.enabled = true
        miXDebug.enabled = true
        miXDebugProf.enabled = true
        miXHprof.enabled = true
        menuBar.repaint

      }

    def disableAll() =
      {
        mMyCommands.enabled = false
        mViewer.enabled = false
        miAddCsvLogs.enabled = false
        miCloseProject.enabled = false
        mScaFileStats.enabled = false
        mTools.enabled = true
        miSSH.enabled = false
        miAspect.enabled = false
        miJdbc.enabled = false
        miScaLogParser.enabled = false
        mPHPTools.enabled = false
        miCsvClean.enabled = false
        miXHprof.enabled = false
      }

    def nettoyagePanel() =
      {
        mainPanel.contents.clear()
        // println(" miNewProject choosed")
        ScalaChartingDyn.boolExamine = false
        ScalaChartingDyn.stop()
        ScaCharting.listFiles = List.empty
        if (!ScaCharting.listRaf.isEmpty) {
          ScaCharting.listRaf foreach ((raf) => if (null != raf) raf.close)
          ScaCharting.listRaf = List.empty
        }
        // ScaCharting.arrEnrichised = null
        mainPanel.visible = false
        mainPanel.visible = true
      }

    // reaction des menus a remplissage dynamique

    mGenTemplStatDatas.peer.addMenuListener(
      new MenuListener() {
        def menuSelected(ev: MenuEvent) = {
          // println("menu mGenTemplStatDatas selected")
          ajouterMiTemplateGen
        }
        def menuDeselected(ev: MenuEvent) = {}
        def menuCanceled(ev: MenuEvent) = {}
      })

    mLocalTemplStatDatas.peer.addMenuListener(
      new MenuListener() {
        def menuSelected(ev: MenuEvent) = {
          // println("menu mGenTemplStatDatas selected")
          ajouterMiTemplateLoc
        }
        def menuDeselected(ev: MenuEvent) = {}
        def menuCanceled(ev: MenuEvent) = {}
      })

    miFromScratch.reactions += {
      case ActionEvent(`miFromScratch`) =>
        nettoyagePanel
        new MyDialogMyCommands(null)

    }

    // reaction des menus de MyCommands necessitant un remplissage dynamique
    mGeneralCommands.peer.addMenuListener(
      new MenuListener() {
        def menuSelected(ev: MenuEvent) = {
          // println("menu mGenTemplStatDatas selected")
          ajouterGeneralCommands
        }
        def menuDeselected(ev: MenuEvent) = {}
        def menuCanceled(ev: MenuEvent) = {}
      })

    mLocalCommands.peer.addMenuListener(
      new MenuListener() {
        def menuSelected(ev: MenuEvent) = {
          // println("menu mGenTemplStatDatas selected")
          ajouterLocalCommands
        }
        def menuDeselected(ev: MenuEvent) = {}
        def menuCanceled(ev: MenuEvent) = {}
      })
    //listenTo(mFile, mViewer,mGenTemplStatDatas)
    // reactions des MenuItem

    // New Project
    miNewProject.reactions += {
      case ActionEvent(`miNewProject`) =>
        // nettoyage
        nettoyagePanel

        new NewProjectDialog(this)
        if (currentProject.length() > 0) {

          enableAll
        } else
          disableAll

    }

    miOpenProject.reactions += {
      case ActionEvent(`miOpenProject`) =>
        // nettoyage

        nettoyagePanel
        // new OpenProjectDialog(this)
        new OpenProjectDialog(this)
        if (currentProject.length() > 0)
          enableAll
        else
          disableAll

      // Complementation des menu
    }
    miAddCsvLogs.reactions += {
      case ActionEvent(`miAddCsvLogs`) =>
        // nettoyage
        nettoyagePanel
        new CSVLogsDialog(this)

    }

    miCloseProject.reactions += {
      case ActionEvent(`miCloseProject`) =>
        // nettoyage
        nettoyagePanel
        currentProject = ""
        title = initialTitle
        disableAll

    }
    miExit.reactions += {

      case ActionEvent(`miExit`) =>
        nettoyagePanel
        //  println(" miExit choosed")
        System.exit(0)

    }
    miScaCharting.reactions += {
      case ActionEvent(`miScaCharting`) =>
        nettoyagePanel
        //ScaCharting.arrEnrichised = null
        // mainPanel.contents map { mainPanel.contents -= _ }
        ScaCharting.mainPanel = mainPanel
        if (null != ScaParserMain.props) ScaParserMain.props.clear
        ScaCharting.listChartingInfo = List.empty
        ScaCharting.tfSample.text = tmpProps.getProperty("scaviewer.sampling.nbPoints", "300")
        var path1 = SearchDirFile.searchYoungestDir(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject, SwingScaViewer.tmpProps.getProperty("scaviewer.prefixscenario").r).getAbsolutePath
        mainPanel.contents += ScaCharting(path1, false)
        ScaCharting.chartPanel.setChart(ChartFactory.createTimeSeriesChart(
          null,
          null,
          null,
          null, // Mettre ici le dataSet
          false,
          true,
          true))
        mainPanel.visible = false
        mainPanel.visible = true

    }

    miScaChartingDyn.reactions += {
      case ActionEvent(`miScaChartingDyn`) =>
        nettoyagePanel
        if (null != ScaParserMain.props) ScaParserMain.props.clear
        //  println("start charting dyn depuis SwingScaViewer")
        ScalaChartingDyn.start
        //   println("Apres start charting dyn depuis SwingScaViewer")
        ScalaChartingDyn.boolExamine = true
        //ScaCharting.arrEnrichised = null
        ScaCharting.listChartingInfo = List.empty
        ScaCharting.mainPanel = mainPanel
        ScaCharting.tfSample.text = tmpProps.getProperty("scaviewer.sampling.nbPoints", "300")
        var path1 = SearchDirFile.searchYoungestDir(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject, SwingScaViewer.tmpProps.getProperty("scaviewer.prefixscenario").r).getAbsolutePath
        mainPanel.contents += ScaCharting.apply(path1, true)
        ScaCharting.chartPanel.setChart(ChartFactory.createTimeSeriesChart(
          null,
          null,
          null,
          null, // Mettre ici le dataSet
          false,
          true,
          true))
        mainPanel.visible = false
        mainPanel.visible = true

    }
    miDirectView.reactions += {
      case ActionEvent(`miDirectView`) =>
        nettoyagePanel
        var diag = new MyDialogOpenDirectParse(true)
        val popularTemplates = System.getProperty("root") + File.separator + "templates" + File.separator + "scaparser" + File.separator + "popular"
        if (!new File(popularTemplates).exists) new File(popularTemplates).mkdir
        // JLP TODO
        if (MyDialogOpenDirectParse.fileLog != "") {
          //  println("traitement direct du fichier :"+MyDialogOpenDirectParse.fileLog)
          // recherche du template adapte au fichier de log
          var propsPopular = new java.util.Properties
          var fis = new FileInputStream(new File(popularTemplates + File.separator + "popular.properties"))
          propsPopular.load(fis)
          fis.close
          new DirectParserMain(MyDialogOpenDirectParse.fileLog, propsPopular)
        }
    }

    miStatDatas.reactions += {
      case ActionEvent(`miStatDatas`) =>
        nettoyagePanel
        if (null != MyDialogStatsFile.system) {
          if (null != MyDialogStatsFile.actorMonitor) {
            MyDialogStatsFile.actorMonitor ! "stop"
          }
          var fut: scala.concurrent.Future[akka.actor.Terminated] = MyDialogStatsFile.system.terminate()
          while (!fut.isCompleted) {

          }

        }
        MyDialogStatsFile(null)

    }
    def ajouterMiTemplateGen() {
      val path = System.getProperty("root") + File.separator + "templates" + File.separator + "filestats"
      val lisFiles = new File(path).listFiles filter (file => file.isFile && file.getName.endsWith(".properties"))
      mGenTemplStatDatas.contents.clear
      if (null != lisFiles && lisFiles.length > 0) {
        for (file <- lisFiles) {
          val nameMi = file.getName.split("\\.")(0)
          val mi = new MenuItem(nameMi)
          mGenTemplStatDatas.contents += mi
          mi.reactions += {
            case ActionEvent(`mi`) => //println(mi.text + " selected")
              MyDialogStatsFile(path + File.separator + mi.text + ".properties")
          }
        }
      }
    }
    def ajouterMiTemplateLoc() {
      val path = System.getProperty("workspace") + File.separator + currentProject + File.separator + "templates" + File.separator + "filestats"
      // println("ajouterMiTemplateLoc path=" + path)
      val lisFiles = new File(path).listFiles filter (file => file.isFile && file.getName.endsWith(".properties"))
      mLocalTemplStatDatas.contents.clear
      if (null != lisFiles && lisFiles.length > 0) {
        for (file <- lisFiles) {
          val nameMi = file.getName.split("\\.")(0)
          val mi = new MenuItem(nameMi)
          mLocalTemplStatDatas.contents += mi
          mi.reactions += {
            case ActionEvent(`mi`) => //println(mi.text + " selected")
              MyDialogStatsFile(path + File.separator + mi.text + ".properties")
          }
        }
      }
    }

    def ajouterGeneralCommands() {

      val path = System.getProperty("root") + File.separator + "myCommands"
      val lisFiles = new File(path).listFiles filter (file => file.isFile && file.getName.endsWith(".properties"))
      mGeneralCommands.contents.clear
      if (null != lisFiles && lisFiles.length > 0) {
        for (file <- lisFiles) {
          val nameMi = file.getName.split("\\.")(0)
          val mi = new MenuItem(nameMi)
          mGeneralCommands.contents += mi
          mi.reactions += {
            case ActionEvent(`mi`) => //println(mi.text + " selected")
              new MyDialogMyCommands(path + File.separator + mi.text + ".properties")
          }
        }
      }

    }

    def ajouterLocalCommands() {
      val path = System.getProperty("workspace") + File.separator + currentProject + File.separator + "myCommands"
      // println("ajouterMiTemplateLoc path=" + path)
      val lisFiles = new File(path).listFiles filter (file => file.isFile && file.getName.endsWith(".properties"))
      mLocalCommands.contents.clear
      if (null != lisFiles && lisFiles.length > 0) {
        for (file <- lisFiles) {
          val nameMi = file.getName.split("\\.")(0)
          val mi = new MenuItem(nameMi)
          mLocalCommands.contents += mi
          mi.reactions += {
            case ActionEvent(`mi`) => //println(mi.text + " selected")
              new MyDialogMyCommands(path + File.separator + mi.text + ".properties")
          }
        }
      }
    }
    miTestRegex.reactions += {
      case ActionEvent(`miTestRegex`) =>

        new MyDialogRegexp(this.peer, false)

    }
    miDateInMillis.reactions += {
      case ActionEvent(`miDateInMillis`) =>

        new MyDialogTimeInMillis(this.peer, false)

    }
    miConcatFile.reactions += {
      case ActionEvent(`miConcatFile`) => {
        nettoyagePanel
        val jta = new TextArea(100, 200)
        jta.editable = true

        val sp1 = new ScrollPane(jta)

        mainPanel.contents += sp1

        contents = mainPanel
        size = new Dimension(toolkit.getScreenSize.width - 30, toolkit.getScreenSize.height - 50)

        new MyLogFilesChooser(jta.peer, currentProject)
      }

    }
    miHorodateLogs.reactions += {
      case ActionEvent(`miHorodateLogs`) => {
        nettoyagePanel
        // println("Traitement SSH")
        // determination prefixe du scenario dans l arborescence: tir, test ...

        new MyDialogLogsFormat(true)
      }
    }

    miSortLinesInFile.reactions += {
      case ActionEvent(`miSortLinesInFile`) => {
        nettoyagePanel
        // println("Traitement SSH")
        // determination prefixe du scenario dans l arborescence: tir, test ...

        new MyDialogSortLinesInFile(true)
      }
    }
    miMergeLines.reactions += {
      case ActionEvent(`miMergeLines`) => {
        nettoyagePanel
        // println("Traitement SSH")
        // determination prefixe du scenario dans l arborescence: tir, test ...

        new com.jlp.scaviewer.tools.MyDialogMergeLinesInFile(true)
      }
    }
    miMergeLines.reactions += {
      case ActionEvent(`miMergeLines`) => {
        nettoyagePanel
        // println("Traitement SSH")
        // determination prefixe du scenario dans l arborescence: tir, test ...

        new com.jlp.scaviewer.tools.MyDialogMergeLinesInFile(true)
      }
    }

    miHexToFromDec.reactions += {
      case ActionEvent(`miHexToFromDec`) => {
        nettoyagePanel
        // println("Traitement SSH")
        // determination prefixe du scenario dans l arborescence: tir, test ...

        new MyDialogDecToAndFromHex(this.peer, false)
      }
    }

    miSdfTester.reactions += {
      case ActionEvent(`miSdfTester`) => {
        nettoyagePanel
        // println("Traitement SSH")
        // determination prefixe du scenario dans l arborescence: tir, test ...

        new MyDialogSimpleDateFormat(false)
      }
    }

    miCsvClean.reactions += {
      case ActionEvent(`miCsvClean`) => {
        nettoyagePanel
        // println("Traitement SSH")
        // determination prefixe du scenario dans l arborescence: tir, test ...

        CsvClean.clean
      }
    }
    miCsvCleanAll.reactions += {
      case ActionEvent(`miCsvCleanAll`) => {
        nettoyagePanel
        // println("Traitement SSH")
        // determination prefixe du scenario dans l arborescence: tir, test ...

        CsvCleanAll.clean
      }
    }

    miSSH.reactions += {
      case ActionEvent(`miSSH`) => {
        nettoyagePanel
        // println("Traitement SSH")
        // determination prefixe du scenario dans l arborescence: tir, test ...

        //  new CreateConnectionsSSH(currentProject, pref)
        new MainScaSSHconnect(currentProject, pref)
      }
    }
    miAspect.reactions += {
      case ActionEvent(`miAspect`) => {
        nettoyagePanel
        //println("Traitement AspectJ")

        new AspectsPerf(this.peer, currentProject)

      }

    }

    miJdbc.reactions += {
      case ActionEvent(`miJdbc`) => {
        nettoyagePanel
        // println("Traitement JDBC Connect currentProject=" + currentProject)

        new JDBCConnect(currentProject, pref)

      }

    }
    miScaLogParser.reactions += {
      case ActionEvent(`miScaLogParser`) => {

        nettoyagePanel

        // println("Traitement miScaLogParser currentProject=" + currentProject)

        var diag = new MyDialogOpenLog(true)
        //  println("MyDialogOpenLog.template=" + MyDialogOpenLog.template)
        // println("MyDialogOpenLog.fileLog=" + MyDialogOpenLog.fileLog)

        if (null != MyDialogOpenLog.fileLog && MyDialogOpenLog.fileLog.trim.length > 0) {
          val cfgP = new ConfigParser(MyDialogOpenLog.template, MyDialogOpenLog.fileLog)
          mainPanel.contents += cfgP

          contents = mainPanel
          size = new Dimension(toolkit.getScreenSize.width - 30, toolkit.getScreenSize.height - 50)
        }
      }

    }
    miHelp.reactions += {
      case ActionEvent(`miHelp`) => {

        //   nettoyagePanel

        // println("Traitement miScaLogParser currentProject=" + currentProject)

        var diag = new InfoScaViewer(this)
        //  println("MyDialogOpenLog.template=" + MyDialogOpenLog.template)
        // println("MyDialogOpenLog.fileLog=" + MyDialogOpenLog.fileLog)

      }

    }

    miXDebug.reactions += {

      case ActionEvent(`miXDebug`) =>
        nettoyagePanel

        new com.jlp.scaviewer.tools.php.MyDialogFormatPHPTrace(true)
    }
    miXDebugProf.reactions += {

      case ActionEvent(`miXDebugProf`) =>
        nettoyagePanel

        new com.jlp.scaviewer.tools.php.MyDialogFormatPHPProfile(true)

    }

    miXHprof.reactions += {

      case ActionEvent(`miXHprof`) =>
        nettoyagePanel

        new com.jlp.scaviewer.tools.php.MyDialogFormatPHPXHprofProfile(true)

    }

    repaint
  }
}