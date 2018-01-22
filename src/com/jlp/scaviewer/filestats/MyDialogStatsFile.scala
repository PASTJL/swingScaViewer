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
import scala.swing.Dialog
import javax.swing.JPopupMenu
import javax.swing.JMenuItem
import scala.swing.ScrollPane
import scala.swing.GridBagPanel
import javax.swing.JTable
import scala.swing.Button
import scala.swing.Label
import javax.swing.table.JTableHeader
import scala.collection.mutable.HashMap
import java.io.File
import scala.swing.TextField
import scala.swing.RadioButton
import scala.swing.Panel
import scala.swing.TextArea
import java.io.BufferedReader
import java.util.regex.Pattern
import java.text.SimpleDateFormat
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.IOException
import java.awt.Color
import java.util.Properties
import java.awt.Toolkit
import java.awt.Dimension
import scala.swing.BoxPanel
import scala.swing.GridPanel
import java.awt.Insets
import java.awt.Point
import scala.swing.event.ActionEvent
import scala.swing.FileChooser
import com.jlp.scaviewer.ui.SwingScaViewer
import javax.swing.JFileChooser
import com.jlp.scaviewer.commons.utils.SearchDirFile
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
import java.util.Date
import java.util.Locale
import java.awt.Font
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import java.util.zip.GZIPOutputStream
import java.io.RandomAccessFile
//import org.joda.time.DateTime
//import org.joda.time.format.DateTimeFormat
// import org.joda.time.format.DateTimeFormatter
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
import java.util.concurrent.atomic.AtomicLong
import java.text.DecimalFormatSymbols
import java.text.DecimalFormat
import language.postfixOps

class MyDialogStatsFile extends Dialog {

  val bCancel = new Button("Cancel");
  val bSave = new Button("Save as template");
  bSave.enabled = false
  val bAnalyse = new Button("Analyse");
  bAnalyse.enabled = false
  val bFicIn: Button = new Button("Browse");
  modal = true

  bCancel.reactions += {
    case ActionEvent(`bCancel`) =>
      if (null != MyDialogStatsFile.system) {
        if (null != MyDialogStatsFile.actorMonitor) {
          MyDialogStatsFile.actorMonitor ! "stop"
        }
           var fut: scala.concurrent.Future[akka.actor.Terminated]=    MyDialogStatsFile.system.terminate()
  while (!fut.isCompleted){
    
  }
      
      }

      dispose
  }
  bSave.reactions += {
    case ActionEvent(`bSave`) =>
      {
        var strFile = MyDialogStatsFile.saveLocal
        new NewDialogFilestatsTemplateLoc(new File(strFile))
      }
  }
  bFicIn.reactions +=
    {

      case ActionEvent(`bFicIn`) =>
        MyDialogStatsFile.nettoyageDialog
        MyDialogStatsFile.nbEnrTraites.set(0)
        MyDialogStatsFile.tabHm = null
        MyDialogStatsFile.currentProps = null
        var dir = SearchDirFile.searchYoungestDir(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject, ("""^""" + SwingScaViewer.tmpProps.getProperty("scaviewer.prefixscenario")).r)
        // println("Button browse with dir=" + dir)
        val fc = new JFileChooser(new File(dir.getAbsolutePath() + File.separator + "logs"))
        // fc.peer.setDialogType(JFileChooser.CUSTOM_DIALOG);
        fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES)
        fc.setControlButtonsAreShown(true)
        fc.setMultiSelectionEnabled(false)
         val dimScreen= Toolkit.getDefaultToolkit().getScreenSize()
        fc.setLocation((dimScreen.width-fc.getSize().width)/2, (dimScreen.height-fc.getSize().height)/2)
        
        val ret = fc.showOpenDialog(null)
       if (ret == JFileChooser.APPROVE_OPTION) {
          MyDialogStatsFile.currentFile = fc.getSelectedFile()
          MyDialogStatsFile.tfFicIn.text = MyDialogStatsFile.currentFile.getAbsolutePath()
        }
        MyDialogStatsFile.print10FirstLine()
        //println("lineTest="+MyDialogStatsFile.lineTest)
        MyDialogStatsFile.regsDate = MyDialogStatsFile.detecterFormatDate(MyDialogStatsFile.lineTest)
        // println("point 1 MyDialogStatsFile.regsDate._2="+ MyDialogStatsFile.regsDate._2)
        // verifier si un fichier local existe deja
        var dirConfig = MyDialogStatsFile.tfFicIn.text.substring(0, MyDialogStatsFile.tfFicIn.text.lastIndexOf(File.separator)) +
          File.separator + "config" + File.separator + "filestats"
        var nameFile = MyDialogStatsFile.currentFile.getName
        var prefix = nameFile
        if (nameFile.contains(".")) {
          val idx = nameFile.lastIndexOf(".")
          prefix = nameFile.substring(0, idx)
        }

        // println("recherche config :"+dirConfig + File.separator + prefix + ".properties")
        if (null != MyDialogStatsFile.template) {
          // println("remplisage par template =" + MyDialogStatsFile.template)
          MyDialogStatsFile.remplirTableau(MyDialogStatsFile.template)

          if (MyDialogStatsFile.isDatedFile) {
            if (!(MyDialogStatsFile.regsDate _2).toLowerCase().contains("timein")) {
              //    MyDialogStatsFile.dtf = DateTimeFormat.forPattern(MyDialogStatsFile.regsDate._2)
              MyDialogStatsFile.sdf = new SimpleDateFormat(MyDialogStatsFile.regsDate._2, MyDialogStatsFile.currentLocale)
            } else {
              // MyDialogStatsFile.dtf = MyDialogStatsFile.dtfMillis
              MyDialogStatsFile.sdf = MyDialogStatsFile.sdfMillis
            }
          }
          // println("RegexFormat=" + (regsDate _1) + " FormatDate=" + (regsDate _2))

          if ((!MyDialogStatsFile.currentFile.getAbsolutePath().endsWith(".gz") && MyDialogStatsFile.currentFile.length() < SwingScaViewer.tmpProps.getProperty("scaviewer.filesstat.normalFileSizeRapidDatation").toLong)
            || (MyDialogStatsFile.currentFile.getAbsolutePath().endsWith(".gz") && MyDialogStatsFile.currentFile.length() < SwingScaViewer.tmpProps.getProperty("scaviewer.filesstat.gzipFileSizeRapidDatation").toLong)) {
            if (MyDialogStatsFile.isDatedFile) {

              //val (dateDeb, dateFin) =   MyDialogStatsFile.detecterDateDebFin
              val (dateDeb, dateFin) = {
                if (!new File(dir + File.separator + "testDate.properties").exists)
                  MyDialogStatsFile.detecterDateDebFin
                else {
                  val propsDate = new Properties()
                  propsDate.load(new FileInputStream(new File(dir + File.separator + "testDate.properties")))
                  val sdfDateTest = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
                  (sdfDateTest.parse(propsDate.getProperty("beginTestDate")).getTime(), sdfDateTest.parse(propsDate.getProperty("endTestDate")).getTime())
                }
              }

              //  MyDialogStatsFile.tfDebOfAnalyse.text = MyDialogStatsFile.dtf.withLocale(MyDialogStatsFile.currentLocale).print(dateDeb)
              //  MyDialogStatsFile.tfFinOfAnalyse.text = MyDialogStatsFile.dtf.withLocale(MyDialogStatsFile.currentLocale).print(dateFin)
              MyDialogStatsFile.tfDebOfAnalyse.text = MyDialogStatsFile.sdf.format(new Date(dateDeb))
              MyDialogStatsFile.tfFinOfAnalyse.text = MyDialogStatsFile.sdf.format(new Date(dateFin))

            } else {
             
              MyDialogStatsFile.tfDebOfAnalyse.text = "noDateInFile"
              MyDialogStatsFile.tfFinOfAnalyse.text = "noDateInFile"

            }
          } else {
            if (MyDialogStatsFile.isDatedFile) {

              val (dateDeb, dateFin) = {
                if (!new File(dir + File.separator + "testDate.properties").exists)
                  MyDialogStatsFile.detecterDateDebFinEstimation
                else {
                  val propsDate = new Properties()
                  propsDate.load(new FileInputStream(new File(dir + File.separator + "testDate.properties")))
                  val sdfDateTest = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
                  (sdfDateTest.parse(propsDate.getProperty("beginTestDate")).getTime(), sdfDateTest.parse(propsDate.getProperty("endTestDate")).getTime())
                }
              }

              //  val (dateDeb, dateFin) = MyDialogStatsFile.detecterDateDebFinEstimation
              //  MyDialogStatsFile.tfDebOfAnalyse.text = MyDialogStatsFile.dtf.withLocale(MyDialogStatsFile.currentLocale).print(dateDeb)
              //   MyDialogStatsFile.tfFinOfAnalyse.text = MyDialogStatsFile.dtf.withLocale(MyDialogStatsFile.currentLocale).print(dateFin)

              MyDialogStatsFile.tfDebOfAnalyse.text = MyDialogStatsFile.sdf.format(new Date(dateDeb))
              MyDialogStatsFile.tfFinOfAnalyse.text = MyDialogStatsFile.sdf.format(new Date(dateFin))
            } else {
              
              MyDialogStatsFile.tfDebOfAnalyse.text = "noDateInFile"
              MyDialogStatsFile.tfFinOfAnalyse.text = "noDateInFile"
            }
          }
        } else if (new File(dirConfig + File.separator + prefix + ".properties").exists) {
          //  println("on remplit avec les props existant")
          MyDialogStatsFile.remplirTableau(dirConfig + File.separator + prefix + ".properties")
          

        } else {
          // println("on remplit from scrash")
          if (MyDialogStatsFile.isDatedFile) {
            if (!(MyDialogStatsFile.regsDate _2).toLowerCase().contains("timein")) {
             // println("MyDialogStatsFile.regsDate _2=" + (MyDialogStatsFile.regsDate _2))
              //   MyDialogStatsFile.dtf = DateTimeFormat.forPattern(MyDialogStatsFile.regsDate._2)
              MyDialogStatsFile.sdf = new SimpleDateFormat(MyDialogStatsFile.regsDate._2, MyDialogStatsFile.currentLocale)
            } else {
              // MyDialogStatsFile.dtf = MyDialogStatsFile.dtfMillis
              MyDialogStatsFile.sdf = MyDialogStatsFile.sdfMillis
            }
          }
          // println("RegexFormat=" + (regsDate _1) + " FormatDate=" + (regsDate _2))

          if ((!MyDialogStatsFile.currentFile.getAbsolutePath().endsWith(".gz") && MyDialogStatsFile.currentFile.length() < SwingScaViewer.tmpProps.getProperty("scaviewer.filesstat.normalFileSizeRapidDatation").toLong)
            || (MyDialogStatsFile.currentFile.getAbsolutePath().endsWith(".gz") && MyDialogStatsFile.currentFile.length() < SwingScaViewer.tmpProps.getProperty("scaviewer.filesstat.gzipFileSizeRapidDatation").toLong)) {

            if (MyDialogStatsFile.isDatedFile) {
              //val (dateDeb, dateFin) = MyDialogStatsFile.detecterDateDebFin
              val (dateDeb, dateFin) = {
                if (!new File(dir + File.separator + "testDate.properties").exists)
                  MyDialogStatsFile.detecterDateDebFin
                else {
                  val propsDate = new Properties()
                  propsDate.load(new FileInputStream(new File(dir + File.separator + "testDate.properties")))
                  val sdfDateTest = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
                  (sdfDateTest.parse(propsDate.getProperty("beginTestDate")).getTime(), sdfDateTest.parse(propsDate.getProperty("endTestDate")).getTime())
                }
              }
              //   MyDialogStatsFile.tfDebOfAnalyse.text = MyDialogStatsFile.dtf.withLocale(MyDialogStatsFile.currentLocale).print(dateDeb)
              //  MyDialogStatsFile.tfFinOfAnalyse.text = MyDialogStatsFile.dtf.withLocale(MyDialogStatsFile.currentLocale).print(dateFin)
              MyDialogStatsFile.tfDebOfAnalyse.text = MyDialogStatsFile.sdf.format(new Date(dateDeb))
              MyDialogStatsFile.tfFinOfAnalyse.text = MyDialogStatsFile.sdf.format(new Date(dateFin))

            } else {
              
              MyDialogStatsFile.tfDebOfAnalyse.text = "noDateInFile"
              MyDialogStatsFile.tfFinOfAnalyse.text = "noDateInFile"
            }
          } else {
            if (MyDialogStatsFile.isDatedFile) {
             // val (dateDeb, dateFin) = MyDialogStatsFile.detecterDateDebFinEstimation
               val (dateDeb, dateFin) = {
                if (!new File(dir + File.separator + "testDate.properties").exists)
                  MyDialogStatsFile.detecterDateDebFinEstimation
                else {
                  val propsDate = new Properties()
                  propsDate.load(new FileInputStream(new File(dir + File.separator + "testDate.properties")))
                  val sdfDateTest = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
                  (sdfDateTest.parse(propsDate.getProperty("beginTestDate")).getTime(), sdfDateTest.parse(propsDate.getProperty("endTestDate")).getTime())
                }
              }
              //  MyDialogStatsFile.tfDebOfAnalyse.text = MyDialogStatsFile.dtf.withLocale(MyDialogStatsFile.currentLocale).print(dateDeb)
              // MyDialogStatsFile.tfFinOfAnalyse.text = MyDialogStatsFile.dtf.withLocale(MyDialogStatsFile.currentLocale).print(dateFin)
              MyDialogStatsFile.tfDebOfAnalyse.text = MyDialogStatsFile.sdf.format(new Date(dateDeb))
              MyDialogStatsFile.tfFinOfAnalyse.text = MyDialogStatsFile.sdf.format(new Date(dateFin))

            } else {
             
              MyDialogStatsFile.tfDebOfAnalyse.text = "noDateInFile"
              MyDialogStatsFile.tfFinOfAnalyse.text = "noDateInFile"
            }
          }

        }

        bSave.enabled = true
        bAnalyse.enabled = true
    }
  bAnalyse.reactions +=
    {
      case ActionEvent(`bAnalyse`) =>
       MyDialogStatsFile. errorOccurs=false
        MyDialogStatsFile.nbEnrTraites.set(0)
        MyDialogStatsFile.saveLocal
        bAnalyse.enabled = false
        bSave.enabled = false

        //  println("dans jbAnalyse")

        MyDialogStatsFile.system = ActorSystem("MySystem")
        this.modal = true
        MyDialogStatsFile.actorMonitor = MyDialogStatsFile.system.actorOf(Props[ParserStatFileActor], "ActorMonitor")
         MyDialogStatsFile.countRecordsEstimation
        MyDialogStatsFile.actorMonitor ! ("start", this)

      // Creation de la table de resultat

      //              while(MyDialogStatsFile.allTerminated==false)
      //              {
      //                Thread.sleep(1000)
      //              }
      //             this.dispose
      //             ParserStatFile().execute

    }

}
object MyDialogStatsFile {
  
 var errorOccurs=false
  var system: ActorSystem = null
  var actorMonitor: ActorRef = null
  var result: MyDialogResultStatsFiles = null
  var nbEnrTraites: AtomicLong = new AtomicLong(0)
  var isDatedFile: Boolean = true;
  var tabIsterminated: Array[Boolean] = null
  var currentProps: Properties = null
  var currentLocale: Locale = Locale.ENGLISH
  // println("MyDialogStatsFile ; debut static")
  var isGzFile = false
  val ftBold: Font = new Font("Arial", Font.BOLD, 12)
  var boolDateInMillis = false;
  var tabHm: Array[scala.collection.mutable.Map[String, CumulEnregistrementStatMemory]] = null
  var jtaContent: String = ""
  var nbLinesByActor: Long = 0
  var fileIn: File = null
  var linesToSkeep: Int = 0

  var boolRapidDatation = false;
  var normalFileSizeRapidDatation = 50000000L
  var gzipFileSizeRapidDatation = 5000000L
  var boolReadFirtsLine = true;
 // var nameTemplate = "";
  var typTemplate = "general";

  val lFicIn = new Label(" File chosen  :");
  val dimlittleTextField = 30
  val dimLargeTextField = 80
  val tfFicIn: TextField = new TextField(dimLargeTextField)
  tfFicIn.font = ftBold

  val sizeLitleField = new Dimension(150, 20)
  var template: String = null

  var currentFile: File = null;

  val lDebOfAnalyse = new Label("Beginning Of Analysis");
  var tfDebOfAnalyse: TextField = new TextField(dimlittleTextField);
  tfDebOfAnalyse.maximumSize = sizeLitleField
  tfDebOfAnalyse.minimumSize = sizeLitleField
  tfDebOfAnalyse.font = ftBold

  val lFinOfAnalyse: Label = new Label("End Of Analysis");
  var tfFinOfAnalyse: TextField = new TextField(dimlittleTextField);
  tfFinOfAnalyse.maximumSize = sizeLitleField
  tfFinOfAnalyse.minimumSize = sizeLitleField
  tfFinOfAnalyse.font = ftBold

  val lPercentile: Label = new Label("percentile (0<per<100) ");
  var tfPercentile: TextField = new TextField("90", dimlittleTextField);
  tfPercentile.font = ftBold

  val lPasValue = new Label("StepValue ");
  var tfPasValue: TextField = new TextField("0.0", dimlittleTextField);
  tfPasValue.font = ftBold

  val lRegexpOuColumn = new Label("Analyse by column number ?");
  val rbRegexpColumn = new RadioButton();

  val lNbActors = new Label("Number of Actors :");
  val tfNbActors = new TextField("2", dimlittleTextField);
  tfNbActors.font = ftBold

  var nbActors: Int = 2
  var nbRecords: Long = -1L

  val rbLocale: RadioButton = new RadioButton();
  rbLocale.selected = true

  val lLocale = new Label(
    "Locale ENGLISH ? (decimal separator => . )");

  val lCsvSeparator = new Label("CSV Separator :");
  var jtfCsvSeparator: TextField = new TextField(";", dimlittleTextField);
  jtfCsvSeparator.font = ftBold
  jtfCsvSeparator.editable = false

  val lRegexppourTfPivot = new Label(
    "First Regexp for Filter in Pivot ");
  var tfNumColRegexp1Pivot: TextField = new TextField(dimLargeTextField);
  tfNumColRegexp1Pivot.font = ftBold

  val lRegexp2Pivot = new Label(
    "Second Regexp for Filter in Pivot ");
  var tfRegexp2Pivot: TextField = new TextField(dimLargeTextField);
  tfRegexp2Pivot.font = ftBold

  val lRegexppourTfValue = new Label(
    "First Regexp for Filter in Value ");
  var tfNumColRegexp1Value: TextField = new TextField(dimLargeTextField);
  tfNumColRegexp1Value.font = ftBold

  val lRegexp2Value = new Label(
    "Second Regexp for Filter in Value ");
  var tfRegexp2Value: TextField = new TextField(dimLargeTextField);
  tfRegexp2Value.font = ftBold

  val lScaleValue = new Label(
    "Scale of Value ( ex : 1 , 10, 0.001) :");
  val tfScaleValue: TextField = new TextField("1", dimlittleTextField);
  tfScaleValue.font = ftBold

  val lNumberTop = new Label("Number of Items for the Top n");
  var tfNumberTop: TextField = new TextField("20", dimlittleTextField);
  tfNumberTop.font = ftBold

  val ta: TextArea = new TextArea
  val spJtextArea: ScrollPane = new ScrollPane(ta)

  var buff: BufferedReader = null
  var properties = new java.util.Properties();

  var nbLignes: Int = 0

  var datePattern: Pattern = null
  var tabCurrentLine: Array[String] = null;

  var tabCurrentDateTime: Array[String] = null
  var currentRegexpDateTime = "";

  var currentRegexpDateTimeName = "";
  // private var formatSdf = "";
  var locale = "UK";
  // //private var sdf = new SimpleDateFormat("", Locale.ENGLISH);
  // var dtf: DateTimeFormatter = null
  var sdf: SimpleDateFormat = null
  //var dtfMillis = DateTimeFormat.forPattern("yyyy/MM/dd:HH:mm:ss.SSS")

  // Pour la suppression de JODA
  var sdfMillis = new SimpleDateFormat("yyyy/MM/dd:HH:mm:ss.SSS")

  var tabDateTimeRegexp: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
  var propsDate = new java.util.Properties()

  var curColor: Color = null
  // println("MyDialogStatsFile ; point 10 static")

  var propsScaViewer: java.util.Properties = new Properties();
  var nblinesForRapidDatation = 1000000;
  var sizeBuffer = 1000000;

  var lineTest = ""
  var regsDate = ("", "")
  var df: DecimalFormat = null;
  ta.font = new Font("Arial", Font.BOLD, 14)
  //println("MyDialogStatsFile ; fin static")
  var myDiag: MyDialogStatsFile = null

  def apply(_template: String):  MyDialogStatsFile = {
    //println("apply execute")
	  errorOccurs=false
    try {
      propsScaViewer.load(new FileInputStream(new File(System
        .getProperty("root")
        + File.separator
        + "config"
        + File.separator
        + "scaViewer.properties")));
      if (propsScaViewer.containsKey("scaviewer.filesstat.normalFileSizeRapidDatation")) {
        normalFileSizeRapidDatation = propsScaViewer
          .getProperty("scaviewer.filesstat.normalFileSizeRapidDatation").toLong

      }
      if (propsScaViewer.containsKey("scaviewer.filesstat.gzipFileSizeRapidDatation")) {
        gzipFileSizeRapidDatation = propsScaViewer
          .getProperty("scaviewer.filesstat.gzipFileSizeRapidDatation").toLong

      }
      nblinesForRapidDatation = propsScaViewer
        .getProperty("scaviewer.filesstat.nblinesForRapidDatation", "50000").toInt
      sizeBuffer = propsScaViewer.getProperty(
        "scaviewer.filesstat.sizeBuffer", "10240000")toInt
    } catch {
      // TODO Auto-generated catch block
      case e: FileNotFoundException => e.printStackTrace();
      case e: IOException =>
        // TODO Auto-generated catch block
        e.printStackTrace();
    }
    try {
      propsDate.load(new FileInputStream(new File(System
        .getProperty("root")
        + File.separator
        + "config"
        + File.separator
        + "scaViewerDates.properties")))
    } catch {
      case e: FileNotFoundException => e.printStackTrace()
      case e: IOException => e.printStackTrace()
    }
    val keys: java.util.Enumeration[Object] = propsDate.keys();
    while (keys.hasMoreElements()) {
      var key = keys.nextElement().asInstanceOf[String]
      tabDateTimeRegexp.put(key, propsDate.getProperty(key));
      // System.out.println("key = "+key);
      // System.out.println("value = "+props.getProperty(key));
    }

    nettoyageDialog
    template = _template
    myDiag = null
    myDiag = new MyDialogStatsFile

    myDiag.title = "Advanced analyze  of datas in a text File"
    val jpContentPane = new GridBagPanel

    var gbc1 = new jpContentPane.Constraints
    gbc1.weightx = 1.0
    gbc1.weighty = 0.0
    myDiag.contents = jpContentPane
    val gbp: GridBagPanel = new GridBagPanel
    gbc1.gridx = 0
    gbc1.gridy = 0
    gbc1.fill = GridBagPanel.Fill.Both
    jpContentPane.layout += ((gbp -> gbc1))
    var gbc = new gbp.Constraints
    gbc.anchor = GridBagPanel.Anchor.West
    gbc.weightx = 1.0
    gbc.weighty = 0.0
    gbc.insets = new Insets(5, 5, 5, 5)

    // Choix du fichier
    gbc.gridx = 0
    gbc.gridy = 0
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lFicIn -> gbc))
    gbc.gridx = 1
    gbc.gridwidth = 2
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((tfFicIn -> gbc))
    gbc.gridx = 3
    gbc.gridwidth = 1
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.Center
    gbp.layout += ((myDiag.bFicIn -> gbc))
    gbc.fill = GridBagPanel.Fill.Horizontal

    // date debut/fin
    gbc.gridx = 0
    gbc.gridy = 1
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lDebOfAnalyse -> gbc))
    gbc.gridx = 1
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((tfDebOfAnalyse -> gbc))
    gbc.gridx = 2
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lFinOfAnalyse -> gbc))
    gbc.anchor = GridBagPanel.Anchor.West
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.gridx = 3
    gbp.layout += ((tfFinOfAnalyse -> gbc))

    // Percentile et pas d'aggregation
    gbc.gridx = 0
    gbc.gridy = 2
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lPercentile -> gbc))
    gbc.gridx = 1
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((tfPercentile -> gbc))
    gbc.gridx = 2
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lPasValue -> gbc))
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.gridx = 3
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((tfPasValue -> gbc))

    // Analyze par column or regexp
    gbc.gridx = 0
    gbc.gridy = 3
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lRegexpOuColumn -> gbc))
    gbc.gridx = 1
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((rbRegexpColumn -> gbc))
    gbc.gridx = 2
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lCsvSeparator -> gbc))
    gbc.gridx = 3
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((jtfCsvSeparator -> gbc))

    // Locale and nb actors
    gbc.gridx = 0
    gbc.gridy = 4
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lLocale -> gbc))
    gbc.gridx = 1
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((rbLocale -> gbc))

    gbc.gridx = 2
    gbc.anchor = GridBagPanel.Anchor.East
    gbc.fill = GridBagPanel.Fill.None
    gbp.layout += ((lNbActors -> gbc))
    gbc.gridx = 3
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((tfNbActors -> gbc))

    //Number1/regex1 Pivot
    gbc.gridx = 0
    gbc.gridy = 5
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbc.gridwidth = 1
    gbp.layout += ((lRegexppourTfPivot -> gbc))
    gbc.gridx = 1
    gbc.gridwidth = 3
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((tfNumColRegexp1Pivot -> gbc))

    //regex2 Pivot
    gbc.gridwidth = 1
    gbc.gridx = 0
    gbc.gridy = 6
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lRegexp2Pivot -> gbc))
    gbc.gridx = 1
    gbc.gridwidth = 3
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((tfRegexp2Pivot -> gbc))

    //regex1 Colum Value
    gbc.gridwidth = 1
    gbc.gridx = 0
    gbc.gridy = 7
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lRegexppourTfValue -> gbc))
    gbc.gridx = 1
    gbc.gridwidth = 3
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((tfNumColRegexp1Value -> gbc))

    //regex2 Value
    gbc.gridwidth = 1
    gbc.gridx = 0
    gbc.gridy = 8
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lRegexp2Value -> gbc))
    gbc.gridx = 1
    gbc.gridwidth = 3
    gbc.fill = GridBagPanel.Fill.Horizontal
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((tfRegexp2Value -> gbc))

    //scale
    gbc.gridwidth = 1
    gbc.gridx = 0
    gbc.gridy = 9
    gbc.fill = GridBagPanel.Fill.None
    gbc.anchor = GridBagPanel.Anchor.East
    gbp.layout += ((lScaleValue -> gbc))
    gbc.gridx = 1
    gbc.fill = GridBagPanel.Fill.Horizontal
    // gbc.gridwidth = 3
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((tfScaleValue -> gbc))

    //Top n
    gbc.gridwidth = 1
    gbc.gridx = 0
    gbc.gridy = 10
    gbc.anchor = GridBagPanel.Anchor.East
    gbc.fill = GridBagPanel.Fill.None
    gbp.layout += ((lNumberTop -> gbc))
    gbc.gridx = 1
    gbc.fill = GridBagPanel.Fill.Horizontal
    // gbc.gridwidth = 3
    gbc.anchor = GridBagPanel.Anchor.West
    gbp.layout += ((tfNumberTop -> gbc))

    gbc.fill = GridBagPanel.Fill.None

    //3 button
    gbc.gridwidth = 2
    gbc.gridx = 0
    gbc.gridy = 11

    gbc.anchor = GridBagPanel.Anchor.Center

    gbp.layout += ((myDiag.bCancel -> gbc))
    gbc.gridwidth = 1
    gbc.gridx = 2
    gbp.layout += ((myDiag.bSave -> gbc))
    gbc.gridx = 3
    gbp.layout += ((myDiag.bAnalyse -> gbc))

    // Mise en place du textArea
    gbc1.gridy = 1
    gbc1.weighty = 1.0
    gbc1.fill = GridBagPanel.Fill.Both
    jpContentPane.layout += ((spJtextArea -> gbc1))

    // Ajout reactions aux bouttons

    rbRegexpColumn.reactions += {
      case ActionEvent(`rbRegexpColumn`) =>

        if (rbRegexpColumn.selected) {
          jtfCsvSeparator.editable = true
          lRegexppourTfPivot.text = "Index of Column for pivot (start at 0)"

          lRegexppourTfValue.text = "Index of Column for valuet (start at 0)"
        } else {
          jtfCsvSeparator.editable = false

          lRegexppourTfPivot.text = "First Regexp for Filter in Pivot "

          lRegexppourTfValue.text = "First Regexp for Filter in Value "
        }
    }

    rbLocale.reactions += {
      case ActionEvent(`rbLocale`) =>

        if (rbLocale.selected) {
          currentLocale = Locale.ENGLISH

          var dfs: DecimalFormatSymbols = new DecimalFormatSymbols(Locale.ENGLISH);
          dfs.setDecimalSeparator('.');

          df = new DecimalFormat("###0.###");
          df.setDecimalFormatSymbols(dfs);
          df.setGroupingSize(0);
        } else {
          currentLocale = Locale.FRENCH
          var dfs: DecimalFormatSymbols = new DecimalFormatSymbols(Locale.FRENCH);
          dfs.setDecimalSeparator(',');
          df = new DecimalFormat("###0,###");
          df.setDecimalFormatSymbols(dfs);
          df.setGroupingSize(0);
        }

    }

    val dim = Toolkit.getDefaultToolkit().getScreenSize()
    val dimDialog = new Dimension(dim.width * 3 / 4, dim.height * 3 / 4)
    // val dimDialog = new Dimension(600, 700)
    myDiag.preferredSize = dimDialog
    //myDiag.minimumSize = dimDialog

    myDiag.resizable = true
    myDiag.pack
    myDiag.location = new Point((dim.getWidth().toInt - dimDialog.getWidth().toInt) / 2, (dim.getHeight().toInt - dimDialog.getHeight().toInt) / 2)
    myDiag.visible = true

    myDiag
  }

  def remplirTableau(fileProps: String) =
    {
      var props: Properties = new Properties()
      // System.out.println( "On remplit a partir de :"+fileProps)
      var fin: FileInputStream = new FileInputStream(new File(fileProps))
      props.load(fin)
      fin.close
      tfDebOfAnalyse.text = props.getProperty("scaviewer.filestats.debOfAnalyse")
      tfFinOfAnalyse.text = props.getProperty("scaviewer.filestats.finOfAnalyse")
      
      tfPercentile.text = props.getProperty("scaviewer.filestats.percentile")
      tfPasValue.text = props.getProperty("scaviewer.filestats.pasValuePercentile")
      rbRegexpColumn.selected = props.getProperty("scaviewer.filestats.regexpColumn").toBoolean
      if (props.getProperty("scaviewer.filestats.csvSeparator.editable", "true").toBoolean) {
        jtfCsvSeparator.editable = true
        lRegexppourTfPivot.text = "Index of Column for pivot (start at 0)"

        lRegexppourTfValue.text = "Index of Column for valuet (start at 0)"
        jtfCsvSeparator.text = props.getProperty("scaviewer.filestats.csvSeparator")
      } else {
        jtfCsvSeparator.editable = true

        lRegexppourTfPivot.text = "First Regexp for Filter in Pivot "

        lRegexppourTfValue.text = "First Regexp for Filter in Value "

        jtfCsvSeparator.text = props.getProperty("scaviewer.filestats.csvSeparator")
        jtfCsvSeparator.editable = false
      }
      rbLocale.selected = props.getProperty("scaviewer.filestats.decimalLocale").toBoolean
      if (rbLocale.selected) {
        currentLocale = Locale.ENGLISH

        var dfs: DecimalFormatSymbols = new DecimalFormatSymbols(Locale.ENGLISH);
        dfs.setDecimalSeparator('.');

        df = new DecimalFormat("###0.###");
        df.setDecimalFormatSymbols(dfs);
        df.setGroupingSize(0);
      } else {
        currentLocale = Locale.FRENCH
        var dfs: DecimalFormatSymbols = new DecimalFormatSymbols(Locale.FRENCH);
        dfs.setDecimalSeparator(',');
        df = new DecimalFormat("###0,###");
        df.setDecimalFormatSymbols(dfs);
        df.setGroupingSize(0);
      }

      //  currentLocale = { if (props.getProperty("scaviewer.filestats.currentLocale") == "ENGLISH") Locale.ENGLISH else Locale.FRENCH }
      tfNbActors.text = props.getProperty("scaviewer.filestats.nbActors")
      tfNumColRegexp1Pivot.text = props.getProperty("scaviewer.filestats.numColRegexp1Pivot")
      tfRegexp2Pivot.text = props.getProperty("scaviewer.filestats.regexp2Pivot")
      tfNumColRegexp1Value.text = props.getProperty("scaviewer.filestats.numColRegexp1Value")
      tfRegexp2Value.text = props.getProperty("scaviewer.filestats.regexp2Value")
      tfScaleValue.text = props.getProperty("scaviewer.filestats.scaleValue")
      tfNumberTop.text = props.getProperty("scaviewer.filestats.numberTop")
      //nbRecords = props.getProperty("scaviewer.filestats.nbRecords", "-1").toLong

    }
  def saveLocal(): String =
    {
      // construire le nom de fichier de properties
      var nomFic = tfFicIn.text.substring(tfFicIn.text.lastIndexOf(File.separator) + 1)
      var dir = tfFicIn.text.substring(0, tfFicIn.text.lastIndexOf(File.separator))
      var prefixNomFic = nomFic
      if (nomFic.contains(".")) {
        prefixNomFic = nomFic.substring(0, nomFic.lastIndexOf("."))
      }
      //  println("nomFic=" + nomFic + " ;prefixNomFic=" + prefixNomFic)

      // Verifions que le r�pertoire config/filestats existes
      if (!new File(dir + File.separator + "config" + File.separator + "filestats").exists) {
        new File(dir + File.separator + "config").mkdir
        new File(dir + File.separator + "config" + File.separator + "filestats").mkdir
      }
      // creation du fichier de properties
      var props: Properties = new Properties()
      props.put("scaviewer.filestats.nameFile", tfFicIn.text)
      props.put("scaviewer.filestats.debOfAnalyse", tfDebOfAnalyse.text)
      props.put("scaviewer.filestats.finOfAnalyse", tfFinOfAnalyse.text)
      props.put("scaviewer.filestats.percentile", tfPercentile.text)
      props.put("scaviewer.filestats.pasValuePercentile", tfPasValue.text)
      props.put("scaviewer.filestats.regexpColumn", rbRegexpColumn.selected.toString)
      var editable = jtfCsvSeparator.editable
      jtfCsvSeparator.editable = true
      props.put("scaviewer.filestats.csvSeparator.editable", editable.toString)
      props.put("scaviewer.filestats.csvSeparator", jtfCsvSeparator.text)
      jtfCsvSeparator.editable = editable
      props.put("scaviewer.filestats.decimalLocale", rbLocale.selected.toString)
      if (rbLocale.selected) {
        currentLocale = Locale.ENGLISH

        var dfs: DecimalFormatSymbols = new DecimalFormatSymbols(Locale.ENGLISH);
        dfs.setDecimalSeparator('.');

        df = new DecimalFormat("###0.###");
        df.setDecimalFormatSymbols(dfs);
        df.setGroupingSize(0);
      } else {
        currentLocale = Locale.FRENCH
        var dfs: DecimalFormatSymbols = new DecimalFormatSymbols(Locale.FRENCH);
        dfs.setDecimalSeparator(',');
        df = new DecimalFormat("###0,###");
        df.setDecimalFormatSymbols(dfs);
        df.setGroupingSize(0);
      }
      props.put("scaviewer.filestats.nbActors", tfNbActors.text)
      props.put("scaviewer.filestats.numColRegexp1Pivot", tfNumColRegexp1Pivot.text)
      props.put("scaviewer.filestats.regexp2Pivot", tfRegexp2Pivot.text)
      props.put("scaviewer.filestats.numColRegexp1Value", tfNumColRegexp1Value.text)
      props.put("scaviewer.filestats.regexp2Value", tfRegexp2Value.text)
      props.put("scaviewer.filestats.scaleValue", tfScaleValue.text)
      props.put("scaviewer.filestats.numberTop", tfNumberTop.text)
      //props.put("scaviewer.filestats.nbRecords", nbRecords.toString)
      if (MyDialogStatsFile.isDatedFile) {
        props.put("scaviewer.filestats.regexpDate", (MyDialogStatsFile.regsDate _1))
        props.put("scaviewer.filestats.formatDate", (MyDialogStatsFile.regsDate _2))
      } else {
        props.put("scaviewer.filestats.regexpDate", "")
        props.put("scaviewer.filestats.formatDate", "")
      }
      if (rbLocale.selected)
        props.put("scaviewer.filestats.currentLocale", "ENGLISH")
      else
        props.put("scaviewer.filestats.currentLocale", "FRENCH")
      var strFileProperties = dir + File.separator + "config" + File.separator + "filestats" + File.separator + prefixNomFic + ".properties"
      var fout: FileOutputStream = new FileOutputStream(new File(strFileProperties))
      props.store(fout, "Created on " + new SimpleDateFormat("yyyy MMM dd HH:mm:ss", currentLocale).format(new Date()))
      fout.close
      currentProps = props
      strFileProperties
    }
  def nettoyageDialog() =
    {
      ta.text = ""
      tfDebOfAnalyse.text = ""
      tfFicIn.text = ""
      tfFinOfAnalyse.text = ""
      tfPercentile.text = "90"
      tfPasValue.text = "0.0"
      jtfCsvSeparator.editable = true
      jtfCsvSeparator.text = ";"
      jtfCsvSeparator.editable = false
      rbRegexpColumn.selected = false
      tfNbActors.text = "2"
      rbLocale.selected = true
      lRegexppourTfPivot.text = "First Regexp for Filter in Pivot "
      tfNumColRegexp1Pivot.text = ""
      tfRegexp2Pivot.text = ""
      lRegexppourTfValue.text = "First Regexp for Filter in Value "
      tfNumColRegexp1Value.text = ""
      tfRegexp2Value.text = ""
      tfScaleValue.text = "1"
      tfNumberTop.text = "20"
      lineTest=""  
    }

  def print10FirstLine() {
    val strFile = tfFicIn.text
    this.ta.text = ""
      lineTest=""
    val reader = initReader(new File(tfFicIn.text))
    for (i <- 0 to 10) {
      var str = reader.readLine()
      if (null != str) {
        if ( str.length() > 10) {
          lineTest +=" "+ str
        }

        this.ta.text += str + "\n"
      }
    }

    reader.close()
  }
  def detecterDateDebFinEstimation: (Long, Long) = {
   // println("Estimation date de fin")
    val reader = initReader(new File(tfFicIn.text))
    var dateFin = 0L
    var dateDeb = Long.MaxValue
    var bool = true
    var str: String = ""
    // sauter une �ventuelle ligne de titre 
    reader.readLine()
    var nbLinesToRead = SwingScaViewer.tmpProps.getProperty("scaviewer.filesstat.nblinesForRapidDatation").toLong

    var fileOut: File = new File(tfFicIn.text + ".out")
    var writer: BufferedWriter = null;
    if (isGzFile) {
      try {

        writer = new BufferedWriter(new OutputStreamWriter(
          new GZIPOutputStream(new FileOutputStream(
            fileOut))));
      } catch {
        // TODO Auto-generated catch block
        case e: FileNotFoundException => e.printStackTrace();

        case e: IOException =>
          e.printStackTrace();
      }
    } else {
      try {
        writer = new BufferedWriter(new OutputStreamWriter(
          new FileOutputStream(fileOut)));
      } catch {

        case e: FileNotFoundException => e.printStackTrace();

        case e: IOException => e.printStackTrace();
      }
    }

    // sauter une �ventuelle ligne de titre 
    reader.readLine()
    var counterLines = 0L
    if (!(regsDate _2).toLowerCase().contains("timein")) {
      while (bool) {
        str = reader.readLine()

        if (str == null || counterLines > nbLinesToRead) {
          bool = false
        } else if (str.length() > 10) {

          // println ("str="+str+ ", regsDate._1="+regsDate._1)

          // Suppression des doubles espaces dans les dates
          var ext = (regsDate _1).r findFirstIn (str)
          if (None != ext) {
            counterLines += 1
            writer.write(str);
            writer.newLine();
            var strDate: String = ext.get

            //          println("strDate="+strDate)
            //           println ( ", regsDate._2="+regsDate._2)
            //           println("currentLocale="+currentLocale.toString)

            // var dtMillis: DateTime = dtf.withLocale(currentLocale).parseDateTime(strDate)
            var dtMillis = sdf.parse(strDate).getTime
            if (dtMillis <= dateDeb) dateDeb = dtMillis
            if (dtMillis >= dateFin) dateFin = dtMillis
          }
        }
      }
    } else {
      (regsDate _2) match {
        case "timeInMillis" => {

          while (bool) {
            str = reader.readLine()

            if (str == null || counterLines > nbLinesToRead) {
              bool = false
            } else if (str.length() > 10) {

              //              println("estimation str=" + str)
              //              println("regsDate._1=" + regsDate._1)
              var ext = ((regsDate _1).r findFirstIn (str))
              if (None != ext) {
                var strDate: String = ext.get
                counterLines += 1
                writer.write(str);
                writer.newLine();
                //println("strDate="+strDate)

                //var dtMillis: DateTime = new DateTime(strDate.toLong)
                var dtMillis = strDate.toLong
                if (dtMillis <= dateDeb) dateDeb = dtMillis
                if (dtMillis >= dateFin) dateFin = dtMillis
              }
            }
          }

        }
        case "timeInSecond" => {
          while (bool) {
            str = reader.readLine()

            if (str == null || counterLines > nbLinesToRead) {
              bool = false
            } else if (str.length() > 10) {
              var ext = ((regsDate _1).r findFirstIn (str))
              if (None != ext) {
                counterLines += 1
                writer.write(str);
                writer.newLine();
                var strDate: String = ext.get

                //println("strDate="+strDate)

                // var dtMillis: DateTime = new DateTime(strDate.toLong * 1000)
                var dtMillis = strDate.toLong * 1000

                if (dtMillis <= dateDeb) dateDeb = dtMillis
                if (dtMillis >= dateFin) dateFin = dtMillis
              }
            }
          }

        }
        case "timeInSecondDotMillis" => {
          while (bool) {
            str = reader.readLine()

            if (str == null || counterLines > nbLinesToRead) {
              bool = false
            } else if (str.length() > 10) {
              var ext = ((regsDate _1).r findFirstIn (str))
              if (None != ext) {
                counterLines += 1
                writer.write(str);
                writer.newLine();
                var strDate: String = ext.get

                //println("strDate="+strDate)

                // var dtMillis: DateTime = new DateTime(strDate.split("\\.")(0).toLong * 1000 + (strDate.split("\\.")(1) + "000").substring(0, 3).toLong)

                var dtMillis = strDate.split("\\.")(0).toLong * 1000 + (strDate.split("\\.")(1) + "000").substring(0, 3).toLong

                if (dtMillis <= dateDeb) dateDeb = dtMillis
                if (dtMillis >= dateFin) dateFin = dtMillis
              }
            }
          }

        }
        case "timeInSecondCommaMillis" => {
          while (bool) {
            str = reader.readLine()

            if (str == null || counterLines > nbLinesToRead) {
              bool = false
            } else if (str.length() > 10) {

              var ext = ((regsDate _1).r findFirstIn (str))
              if (None != ext) {
                writer.write(str);
                writer.newLine();
                counterLines += 1
                var strDate: String = ext.get

                //println("strDate="+strDate)

                // var dtMillis: DateTime = new DateTime(strDate.split(",")(0).toLong * 1000 +(strDate.split(",")(1) + "000").substring(0, 3).toLong)

                var dtMillis = strDate.split(",")(0).toLong * 1000 + (strDate.split(",")(1) + "000").substring(0, 3).toLong

                if (dtMillis <= dateDeb) dateDeb = dtMillis
                if (dtMillis >= dateFin) dateFin = dtMillis
              }
            }

          }
        }
      }

    }
    writer.close()
    reader.close()
    var raf1: RandomAccessFile = new RandomAccessFile(new File(tfFicIn.text), "r");
    var raf2: RandomAccessFile = new RandomAccessFile(fileOut, "r");
    val lgRaf1: Long = raf1.length();
    val lgRaf2: Long = raf2.length();
    raf1.close();
    raf2.close();
    fileOut.delete();
    nbRecords = counterLines * lgRaf1 / lgRaf2
    // println("counterLine=" + counterLines + " ;lgRaf1=" + lgRaf1 + " ;lgRaf2=" + lgRaf2 + " ;nbRecords=" + nbRecords)
    // ecart de date
    // var gap = dateFin.getMillis - dateDeb.getMillis

    var gap = dateFin - dateDeb
    var gapTotal = gap * nbRecords / counterLines
    // println("gap=" + gap + " ; gapTotal=" + gapTotal)
    // var estimatedDate = new DateTime(dateDeb.getMillis + gapTotal)
    var estimatedDate = dateDeb + gapTotal
    lFinOfAnalyse.text = "End of Analysis (Estimated)"
    (dateDeb, estimatedDate)
  }

  def detecterDateDebFin: (Long, Long) = {
    val reader = initReader(new File(tfFicIn.text))
    var dateFin = 0L
    var dateDeb = Long.MaxValue
    var bool = true
    var str: String = ""
    nbRecords = 0L
    // sauter une �ventuelle ligne de titre 
    reader.readLine()
    if (!(regsDate _2).toLowerCase().contains("timein")) {
      while (bool) {
        str = reader.readLine()

        if (str == null) {
          bool = false
        } else if (str.length() > 10) {
          nbRecords += 1

          while (bool && None == (regsDate _1).r.findFirstIn(str)) {
            str = reader.readLine()
            if (str == null) {
              bool = false
            }
          }
          //println("strDate="+strDate)
          var strDate: String = ((regsDate _1).r findFirstIn (str)).get
          // var dtMillis: DateTime = dtf.withLocale(currentLocale).parseDateTime(strDate)

          // println("sdbis="+sdf.toPattern())

          var dtMillis = sdf.parse(strDate).getTime

          if (dtMillis <= dateDeb) dateDeb = dtMillis
          if (dtMillis >= dateFin) dateFin = dtMillis
        }

      }
    } else {
      (regsDate _2) match {
        case "timeInMillis" => {

          while (bool) {
            str = reader.readLine()
            if (str == null) {
              bool = false
            } else if (str.length() > 10) {
              nbRecords += 1

              while (bool && None == (regsDate _1).r.findFirstIn(str)) {
                str = reader.readLine()
                if (str == null) {
                  bool = false
                }
              }
              var strDate: String = ((regsDate _1).r findFirstIn (str))get
              //println("strDate="+strDate)

              // var dtMillis: DateTime = new DateTime(strDate.toLong)

              var dtMillis = strDate.toLong

              if (dtMillis <= dateDeb) dateDeb = dtMillis
              if (dtMillis >= dateFin) dateFin = dtMillis
            }
          }

        }
        case "timeInSecond" => {
          while (bool) {
            str = reader.readLine()
            if (str == null) {
              bool = false
            } else if (str.length() > 10) {
              nbRecords += 1
              while (bool && None == (regsDate _1).r.findFirstIn(str)) {
                str = reader.readLine()
                if (str == null) {
                  bool = false
                }
              }
              var strDate: String = ((regsDate _1).r findFirstIn (str))get

              //println("strDate="+strDate)

              // var dtMillis: DateTime = new DateTime(strDate.toLong * 1000)

              var dtMillis = strDate.toLong * 1000
              if (dtMillis <= dateDeb) dateDeb = dtMillis
              if (dtMillis >= dateFin) dateFin = dtMillis
            }
          }

        }
        case "timeInSecondDotMillis" => {
          while (bool) {
            str = reader.readLine()
            if (str == null) {
              bool = false
            } else if (str.length() > 10) {
              nbRecords += 1
              while (bool && None == (regsDate _1).r.findFirstIn(str)) {
                str = reader.readLine()
                if (str == null) {
                  bool = false
                }
              }
              var strDate: String = ((regsDate _1).r findFirstIn (str))get

              //println("strDate="+strDate)

              // var dtMillis: DateTime = new DateTime(strDate.split("\\.")(0).toLong * 1000 +       (strDate.split("\\.")(1) + "000").substring(0, 3).toLong)
              var dtMillis = strDate.split("\\.")(0).toLong * 1000 + (strDate.split("\\.")(1) + "000").substring(0, 3).toLong

              if (dtMillis <= dateDeb) dateDeb = dtMillis
              if (dtMillis >= dateFin) dateFin = dtMillis
            }
          }

        }
        case "timeInSecondCommaMillis" => {
          while (bool) {
            str = reader.readLine()
            if (str == null) {
              bool = false
            } else if (str.length() > 10) {
              nbRecords += 1
              while (bool && None == (regsDate _1).r.findFirstIn(str)) {
                str = reader.readLine()
                if (str == null) {
                  bool = false
                }
              }
              var strDate: String = ((regsDate _1).r findFirstIn (str))get

              //println("strDate="+strDate)

              //  var dtMillis: DateTime = new DateTime(strDate.split(",")(0).toLong * 1000 +       (strDate.split(",")(1) + "000").substring(0, 3).toLong)

              var dtMillis = strDate.split(",")(0).toLong * 1000 + (strDate.split(",")(1) + "000").substring(0, 3).toLong

              if (dtMillis <= dateDeb) dateDeb = dtMillis
              if (dtMillis >= dateFin) dateFin = dtMillis
            }
          }

        }
      }

    }
    // println(" ;nbRecords=" + nbRecords)
    reader.close()
    lFinOfAnalyse.text = "End of Analysis "
    (dateDeb, dateFin)
  }

  private def detecterFormatDate(input: String): (String, String) =
    {
    regsDate=("","")
   // println ( " detection format date avec la ligne :\n"+input)
      var regexLongestr = ""
      var ret: (String, String) = (null, null)
     // println("getFormatDate input=" + input)
      var tabDateTimeRegexp = new HashMap[String, String]
      var keys = propsDate.keys();
      // println("propsDate.size="+propsDate.size)
      //println("input="+input)
      while (keys.hasMoreElements()) {
        var key: String = keys.nextElement().asInstanceOf[String]
        var value: String = propsDate.getProperty(key)
        // if(!key.contains("format.")) 
        tabDateTimeRegexp += (key -> value)

      }
      //   println("tabDateTimeRegexp=" + tabDateTimeRegexp)

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

      if (regexLongestr.length > 4) {
        var str: String = regexLongestr.r.findFirstIn(input).get
        if (null != str && str.length > 4 && (str.contains("Feb")
          || str.contains("Apr")
          || str.contains("May")
          || str.contains("Jun")
          || str.contains("Jul")
          || str.contains("Aug"))) {
          currentLocale = Locale.ENGLISH
        } else {
          if (!rbLocale.selected)
            currentLocale = Locale.FRENCH
        }
      }
      if (regexLongestr.length > 4) {
        isDatedFile = true
       //  println("retour=("+regexLongestr+","+tabDateTimeRegexp.getOrElse("format." + kkeyLongest, "")+")")
        (regexLongestr, tabDateTimeRegexp.getOrElse("format." + kkeyLongest, ""))
      } else {
        isDatedFile = false
      //   println("retour=(null,null)")
        (null, null)
      }
    }

  def initReader(fileIn: File): BufferedReader = {

    try {
      if (fileIn.getName().endsWith(".gz")) {
        isGzFile = true
         System.out.println("filein="+fileIn.getAbsolutePath())
        new BufferedReader(new InputStreamReader(
          new GZIPInputStream(new FileInputStream(fileIn))),
          SwingScaViewer.tmpProps.getProperty("scaviewer.filesstat.sizeBuffer").toInt)

      } else {
        isGzFile = false
        System.out.println("filein="+fileIn.getAbsolutePath())
        new BufferedReader(new InputStreamReader(
          new FileInputStream(fileIn)), SwingScaViewer.tmpProps.getProperty("scaviewer.filesstat.sizeBuffer").toInt);

      }
    } catch {
      // TODO Auto-generated catch block
      case e: FileNotFoundException => e.printStackTrace(); null

      // TODO Auto-generated catch block
      case e: IOException => e.printStackTrace(); null
    }

  }
  def countRecords() {
    val reader = initReader(new File(tfFicIn.text))
    nbRecords = 0L
    // sauter une �ventuelle ligne de titre 
    reader.readLine()
    var bool = true;
    while (bool) {
      var str = reader.readLine()
      if (str == null) {
        bool = false
      } else
        nbRecords += 1
    }
    reader.close
  }
  def countRecordsEstimation() {
    val reader = initReader(new File(tfFicIn.text))
    nbRecords = 0L
    var bool = true
    var str: String = ""
    // sauter une �ventuelle ligne de titre 
    reader.readLine()
    var nbLinesToRead = SwingScaViewer.tmpProps.getProperty("scaviewer.filesstat.nblinesForRapidDatation").toLong

    var fileOut: File = new File(tfFicIn.text + ".out")
    var writer: BufferedWriter = null;
    if (isGzFile) {
      try {

        writer = new BufferedWriter(new OutputStreamWriter(
          new GZIPOutputStream(new FileOutputStream(
            fileOut))));
      } catch {
        // TODO Auto-generated catch block
        case e: FileNotFoundException => e.printStackTrace();

        case e: IOException =>
          e.printStackTrace();
      }
    } else {
      try {
        writer = new BufferedWriter(new OutputStreamWriter(
          new FileOutputStream(fileOut)));
      } catch {

        case e: FileNotFoundException => e.printStackTrace();

        case e: IOException => e.printStackTrace();
      }
    }
    var counterLines = 0L

    while (bool) {
      str = reader.readLine()
//      println("Avant str");
//      println("str="+str);
//      println("counterLines="+counterLines);
//      println("nbLinesToRead="+nbLinesToRead);
      if (null == str || counterLines > nbLinesToRead) {
        bool = false
      }else
      {
        counterLines += 1
      writer.write(str);
      writer.newLine();
      }
      

    }

    reader.close
    writer.close
    var raf1: RandomAccessFile = new RandomAccessFile(new File(tfFicIn.text), "r");
    var raf2: RandomAccessFile = new RandomAccessFile(fileOut, "r");
    val lgRaf1: Long = raf1.length();
    val lgRaf2: Long = raf2.length();
    raf1.close();
    raf2.close();
    fileOut.delete();
    nbRecords = counterLines * lgRaf1 / lgRaf2

  }
  
  
  // 
  
     
  
  
  
  //
}