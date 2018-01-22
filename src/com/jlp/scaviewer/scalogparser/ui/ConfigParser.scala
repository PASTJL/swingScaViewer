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
package com.jlp.scaviewer.scalogparser.ui

import scala.swing.TabbedPane
import scala.swing.GridBagPanel
import java.awt.Insets
import scala.swing.TextArea
import scala.swing.ScrollPane
import scala.swing.SplitPane
import scala.swing.Orientation
import java.awt.Toolkit
import java.awt.Dimension
import java.io.RandomAccessFile
import java.io.File
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import com.jlp.scaviewer.ui.SwingScaViewer
import java.io.FileNotFoundException
import java.io.IOException
import javax.swing.border.LineBorder
import java.awt.Color
import javax.swing.border.TitledBorder
import java.awt.Font
import scala.swing.GridPanel
import scala.swing.Button
//import scala.swing.event.ComponentResized
import javax.swing.JEditorPane
import scala.swing.TextField
import scala.swing.Label
import scala.swing.RadioButton
import scala.swing.event.ActionEvent
import scala.swing.ComboBox
import javax.swing.table.DefaultTableModel
import javax.swing.JTable
import javax.swing.JOptionPane
import java.text.SimpleDateFormat
import java.util.Calendar
import com.jlp.scaviewer.scalogparser.ui.saveandload._
import com.jlp.scaviewer.scalogparser.ScaParserMain
import java.awt.event.MouseListener
import java.awt.event.KeyListener
import java.awt.event.MouseEvent
import com.jlp.scaviewer.commons.utils._
import javax.swing.JFileChooser
import java.util.regex.Pattern
import java.awt.event.KeyEvent
import akka.actor.ActorSystem
import akka.actor.Props
import java.util.TimeZone
import java.net.URL
import javax.swing.event.HyperlinkListener
import javax.swing.event.HyperlinkEvent
import javax.swing.text.html.HTMLFrameHyperlinkEvent
import javax.swing.text.html.HTMLDocument
import com.jlpapis.tools.MyDialogRegexp

case class ConfigParser(template: String, logFile: String) extends SplitPane with MouseListener with KeyListener
  with HyperlinkListener {

  var configSystem: ActorSystem = null
  var props = new java.util.Properties
  val currentFont = new Font("Arial", Font.BOLD, 12)

  val prefixFile =
    {
      var fileOnly = logFile.substring(logFile.lastIndexOf(File.separator) + 1)
      var prefix = fileOnly
      if (prefix.contains(".")) {
        prefix = prefix.substring(0, prefix.indexOf("."))
      }
      prefix
    }
  val path = {
    if (logFile.contains("logs" + File.separator))
      logFile.substring(0, logFile.lastIndexOf("logs" + File.separator))
    else ""
  }

  this.orientation = Orientation.Horizontal
  oneTouchExpandable = true
  val ta = new TextArea(20, 300)
  ta.font = currentFont
  val sp = new ScrollPane(ta)
  this.topComponent = sp
  sp.minimumSize = new Dimension(this.size.width, 100)
  sp.preferredSize = new Dimension(this.size.width, 200)
  sp.maximumSize = this.maximumSize
  // 
  var isGzFile = false
  val tabPane = new TabbedPane()
  val enrPanel = new GridBagPanel
  val pivotPanel = new GridBagPanel
  val valuePanel = new GridBagPanel
  // val taDoc = new JEditorPane("text/html", "")
  // println("file://" + System.getProperty("root") + File.separator + "manuals" + File.separator + "scaLogparser.html")
  var url: java.net.URL = null
  try {
    if (System.getProperty("os.name").toLowerCase.contains("windows")) {
      if (SwingScaViewer.tmpProps.getProperty("scaviewer.lang.help", "FR") == "FR") {
        url = new java.net.URL("file:///" + System.getProperty("root") + "/manuals/scaLogparser_FR.html")
      } else {
        url = new java.net.URL("file:///" + System.getProperty("root") + "/manuals/scaLogparser_EN.html")
      }
    } else {
      if (SwingScaViewer.tmpProps.getProperty("scaviewer.lang.help", "FR") == "FR") {
        url = new java.net.URL("file://" + System.getProperty("root") + "/manuals/scaLogparser_FR.html")
      } else {
        url = new java.net.URL("file://" + System.getProperty("root") + "/manuals/scaLogparser_EN.html")
      }

    }
  } catch {
    case e: java.net.MalformedURLException => e.printStackTrace
  }

  val taDoc = new JEditorPane()
  taDoc.setEditable(false);
  taDoc.addHyperlinkListener(this)
  def hyperlinkUpdate(e: HyperlinkEvent) {
    if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
      var pane: JEditorPane = e.getSource().asInstanceOf[JEditorPane]
      if (e.isInstanceOf[HTMLFrameHyperlinkEvent]) {
        var evt: HTMLFrameHyperlinkEvent = e.asInstanceOf[HTMLFrameHyperlinkEvent]
        var doc: HTMLDocument = pane.getDocument().asInstanceOf[HTMLDocument]
        doc.processHTMLFrameHyperlinkEvent(evt);
      } else {
        try {
          pane.setPage(e.getURL());
        } catch {
          case t: java.lang.Throwable => t.printStackTrace
        }
      }
    }
  }

  val docPanel = new ScrollPane()
  docPanel.peer.setViewportView(taDoc)
  taDoc.setPage(url)
  tabPane.pages += new TabbedPane.Page("Enregistrements", enrPanel)
  tabPane.pages += new TabbedPane.Page("Pivots", pivotPanel)
  tabPane.pages += new TabbedPane.Page("Values", valuePanel)
  tabPane.pages += new TabbedPane.Page("Help", docPanel)
  //taDoc.setText("<html><body><b>This is the documentation</b></body></html>")
  //  val raf=new RandomAccessFile(System.getProperty("root")+File.separator+"manuals"+
  //     File.separator+"scaLogparser.html","r" )
  //  val tab:Array[Byte]=Array.ofDim(raf.length.toInt)
  //  raf.readFully(tab)
  //  println (new String(tab))
  //    taDoc.setText(new String(tab))
  //    raf.close
  val screenSize = Toolkit.getDefaultToolkit.getScreenSize
  this.maximumSize = new Dimension(screenSize.width - 20, screenSize.height - 50)
  this.minimumSize = new Dimension(0, 0)
  this.preferredSize = new Dimension(screenSize.width - 20, screenSize.height - 50)

  // ecriture de 20 lignes du file dans le TextArea
  var reader = initReader(new File(logFile))
  var str = ""
  for (i <- 0 until 20) {
    str += reader.readLine + "\n"
  }
  reader.close
  ta.text = str
  ta.caret.position = 0
  // Jtable Pivots et Values
  val columnValues: java.util.Vector[String] = new java.util.Vector(0, 4)
  for (item <- List("Name", "First Regex / Function", "Second Regex / Parameters", "Unit", "Scale")) {
    columnValues.add(item)
  }
  val tmValues = new DefaultTableModel(columnValues, 300)

  val jtValues = new JTable(tmValues)
  jtValues.setFont(currentFont)
  jtValues.setAutoResizeMode(JTable.AUTO_RESIZE_SUBSEQUENT_COLUMNS)
  jtValues.getTableHeader.setFont(new Font("Arial", Font.BOLD, 14))
  val scpValue = new ScrollPane()
  //scpValue.preferredSize=this.preferredSize
  scpValue.peer.setViewportView(jtValues)
  jtValues.setPreferredScrollableViewportSize(scpValue.preferredSize)
  jtValues.addKeyListener(this)
  
  val columnPivots: java.util.Vector[String] = new java.util.Vector(0, 2)
  for (item <- List("Name", "First Regex", "Second Regex")) {
    columnPivots.add(item)
  }
  val tmPivots = new DefaultTableModel(columnPivots, 300)

  val jtPivots = new JTable(tmPivots)
  jtPivots.setFont(currentFont)
  jtPivots.setAutoResizeMode(JTable.AUTO_RESIZE_SUBSEQUENT_COLUMNS)
  jtPivots.getTableHeader.setFont(new Font("Arial", Font.BOLD, 14))
  val scpPivot = new ScrollPane()
  //scpValue.preferredSize=this.preferredSize
  scpPivot.peer.setViewportView(jtPivots)
  jtPivots.setPreferredScrollableViewportSize(scpPivot.preferredSize)

  //rajout de la table values
  var gbc4 = new valuePanel.Constraints
  gbc4.insets = new Insets(5, 5, 5, 5)
  gbc4.fill = GridBagPanel.Fill.Both
  gbc4.gridx = 0
  gbc4.gridy = 0
  gbc4.weightx = 1.0
  gbc4.weighty = 1.0
  valuePanel.layout += ((scpValue -> gbc4))
  val totalWidht = this.preferredSize.width
  jtValues.getColumnModel.getColumn(jtValues.getColumnModel.getColumnIndex("Name")).setPreferredWidth(3 * totalWidht / 13);
  jtValues.getColumnModel.getColumn(jtValues.getColumnModel.getColumnIndex("First Regex / Function")).setPreferredWidth(4 * totalWidht / 13);
  jtValues.getColumnModel.getColumn(jtValues.getColumnModel.getColumnIndex("Second Regex / Parameters")).setPreferredWidth(4 * totalWidht / 13);
  jtValues.getColumnModel.getColumn(jtValues.getColumnModel.getColumnIndex("Unit")).setPreferredWidth(totalWidht / 13);
  jtValues.getColumnModel.getColumn(jtValues.getColumnModel.getColumnIndex("Scale")).setPreferredWidth(totalWidht / 13);

  //rajout de la table pivots
  var gbc5 = new pivotPanel.Constraints
  gbc5.insets = new Insets(5, 5, 5, 5)
  gbc5.fill = GridBagPanel.Fill.Both
  gbc5.gridx = 0
  gbc5.gridy = 0
  gbc5.weightx = 1.0
  gbc5.weighty = 1.0
  pivotPanel.layout += ((scpPivot -> gbc5))

  jtPivots.getColumnModel.getColumn(jtPivots.getColumnModel.getColumnIndex("Name")).setPreferredWidth(3 * totalWidht / 11);
  jtPivots.getColumnModel.getColumn(jtPivots.getColumnModel.getColumnIndex("First Regex")).setPreferredWidth(4 * totalWidht / 11);
  jtPivots.getColumnModel.getColumn(jtPivots.getColumnModel.getColumnIndex("Second Regex")).setPreferredWidth(4 * totalWidht / 11);
  jtPivots.addMouseListener(this)
  jtPivots.addKeyListener(this)

  // Remplissage Onglets Enreg
  var insets1 = new Insets(5, 5, 5, 5)
  var gbc1 = new enrPanel.Constraints
  gbc1.insets = insets1
  gbc1.fill = GridBagPanel.Fill.Both
  gbc1.gridx = 0
  gbc1.gridy = 0
  gbc1.weightx = 1.0
  gbc1.weighty = 0.0

  // Panel de parametrage Fichier IN
  val pan1 = new GridBagPanel()
  pan1.border = new TitledBorder(new LineBorder(Color.RED, 2, true), "FileIn", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, new Font("Arial", Font.BOLD, 14))
  enrPanel.layout += ((pan1 -> gbc1))

  // Panel de parametrage fichier Out
  val pan2 = new GridBagPanel()
  pan2.border = new TitledBorder(new LineBorder(Color.BLACK, 2, true), "FilesOut", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, new Font("Arial", Font.BOLD, 14))
  gbc1.gridy = 1
  enrPanel.layout += ((pan2 -> gbc1))

  // Panel de parametrages additionnels
  val pan3 = new GridBagPanel()
  pan3.border = new TitledBorder(new LineBorder(Color.GRAY, 2, true), "Advanced", TitledBorder.LEFT, TitledBorder.DEFAULT_POSITION, new Font("Arial", Font.BOLD, 14))
  gbc1.gridy = 2
  enrPanel.layout += ((pan3 -> gbc1))

  gbc1.weighty = 1.0
  // Panel des boutons
  gbc1.fill = GridBagPanel.Fill.Horizontal
  gbc1.weightx = 1.0

  gbc1.gridy = 3

  val panButs = new GridBagPanel()
  var gbc2 = new panButs.Constraints
  var insets2 = new Insets(10, 80, 10, 10)
  gbc2.insets = insets2
  gbc2.fill = GridBagPanel.Fill.Both
  gbc2.gridx = 0
  gbc2.gridy = 0
  gbc2.weightx = 1.0
  gbc2.weighty = 0.0

  val saveBut = new Button("Save")

  val cancBut = new Button("Cancel")
  val tplBut = new Button("SaveAsTemplate")
  val regBut = new Button("TestRegex")
  val parseBut = new Button("Parse")
  saveBut.font = new Font("Arial", Font.BOLD, 14)
  cancBut.font = new Font("Arial", Font.BOLD, 14)
  tplBut.font = new Font("Arial", Font.BOLD, 14)
  regBut.font = new Font("Arial", Font.BOLD, 14)
  parseBut.font = new Font("Arial", Font.BOLD, 14)

  // calcul de 
  val dimBut = new Dimension(180, 30)
  val dimButMin = new Dimension(100, 20)

  parseBut.maximumSize = dimBut
  parseBut.minimumSize = dimButMin
  parseBut.preferredSize = dimBut
  parseBut.maximumSize = dimBut
  panButs.layout += (parseBut -> gbc2)

  gbc2.gridx = 1
  saveBut.maximumSize = dimBut
  saveBut.minimumSize = dimButMin
  saveBut.preferredSize = dimBut
  saveBut.maximumSize = dimBut
  panButs.layout += (saveBut -> gbc2)

  gbc2.gridx = 2
  tplBut.maximumSize = dimBut
  tplBut.minimumSize = dimButMin
  tplBut.preferredSize = dimBut
  tplBut.maximumSize = dimBut
  panButs.layout += (tplBut -> gbc2)

  gbc2.gridx = 3
  regBut.maximumSize = dimBut
  regBut.minimumSize = dimButMin
  regBut.preferredSize = dimBut
  regBut.maximumSize = dimBut
  panButs.layout += (regBut -> gbc2)

  gbc2.gridx = 4
  cancBut.maximumSize = dimBut
  cancBut.minimumSize = dimButMin
  cancBut.preferredSize = dimBut
  cancBut.maximumSize = dimBut
  panButs.layout += (cancBut -> gbc2)

  enrPanel.layout += ((panButs -> gbc1))

  val botPan = new GridBagPanel()
  var gbcBot = new botPan.Constraints
  gbcBot.fill = GridBagPanel.Fill.Both
  gbcBot.gridx = 0
  gbcBot.gridy = 0
  gbcBot.weightx = 1.0
  gbcBot.weighty = 1.0
  botPan.layout += (tabPane -> gbcBot)
  gbcBot.gridy = 1
  botPan.layout += (panButs -> gbcBot)

  this.bottomComponent = botPan

  // Composents dont la visibilite est necessaire
  val minSize = new Dimension(180, 20)
  val min2Size = new Dimension(100, 20)
  // FicIn
  val tfFileIn = new TextField(50)
  tfFileIn.font = currentFont
  tfFileIn.minimumSize = minSize
  tfFileIn.text = logFile
  tfFileIn.editable = false

  val tfStepAggr = new TextField(12)
  tfStepAggr.text = "1000"
  tfStepAggr.font = currentFont
  tfStepAggr.minimumSize = min2Size
  val tfDebEnr = new TextField(30)
  tfDebEnr.font = currentFont
  tfDebEnr.minimumSize = minSize
  val tfFinEnr = new TextField(30)
  tfFinEnr.font = currentFont

  tfFinEnr.minimumSize = minSize
  val tfInclEnr = new TextField(30)
  tfInclEnr.font = currentFont
  tfInclEnr.minimumSize = minSize
  val tfExclEnr = new TextField(30)
  tfExclEnr.font = currentFont
  tfExclEnr.minimumSize = minSize
  val tfDateRegex = new TextField(30)
  tfDateRegex.font = currentFont
  tfDateRegex.minimumSize = minSize
  val tfDateFormat = new TextField(30)
  tfDateRegex.font = currentFont
  tfDateFormat.minimumSize = minSize
  val tfStartDate = new TextField(30)
  tfStartDate.font = currentFont
  tfStartDate.minimumSize = minSize
  tfStartDate.tooltip = "<html><b> the format must be yyyy/MM/dd HH:mm:ss</b></html>"
  val tfEndDate = new TextField(30)
  tfEndDate.font = currentFont
  tfEndDate.minimumSize = minSize
  tfEndDate.tooltip = "<html><b> the format must be yyyy/MM/dd HH:mm:ss</b></html>"
  val rbExplicitDate = new RadioButton("<= Explicit Date ?")
  rbExplicitDate.font = currentFont
  rbExplicitDate.selected = true
  val rbLocaleIn = new RadioButton("<= Locale English ?")
  rbLocaleIn.font = currentFont
  rbLocaleIn.selected = true
  val tfStep2Enr = new TextField(50)
  tfStep2Enr.font = currentFont
  tfStep2Enr.minimumSize = minSize
  tfStep2Enr.editable = false
  val tfDateImplDebut = new TextField(30)
  tfDateImplDebut.font = currentFont
  tfDateImplDebut.minimumSize = minSize
  var tz: TimeZone = TimeZone.getDefault

  tfDateImplDebut.text = "1970/01/01 00:00:00"
  tfDateImplDebut.editable = false
  val tfStepUnit = new TextField(12)
  tfStepUnit.font = currentFont
  tfStepUnit.minimumSize = min2Size
  tfStepUnit.text = "ms"
  tfStepUnit.editable = false

  val tfFileOut = new TextField(50)
  tfFileOut.font = currentFont
  tfFileOut.minimumSize = minSize
  val dtf = new SimpleDateFormat("_yyyyMMdd_HHmmss")
  var cal = Calendar.getInstance

  val date = dtf.format(cal.getTime)
  println("avant path csv path=" + path)
  tfFileOut.text = path + "csv" + File.separator + prefixFile + date + File.separator
  tfFileOut.editable = false
  val tfFsOut = new TextField(12)
  tfFsOut.font = currentFont
  tfFsOut.text = ";"
  tfFsOut.minimumSize = min2Size

  val rbLocaleOut = new RadioButton("<= Locale English ?")
  rbLocaleOut.font = currentFont
  rbLocaleOut.selected = true
  val tfDateFormatOut = new TextField(30)
  tfDateFormatOut.font = currentFont
  tfDateFormatOut.text = "yyyy/MM/dd:HH:mm:ss"
  tfDateFormatOut.minimumSize = minSize
  val rbAllAveragesOnly = new RadioButton("<= Generate AllAverages Only ?")
  rbAllAveragesOnly.font = currentFont
  rbAllAveragesOnly.selected = false

  val rbisDebDate = new RadioButton("<= Is start date ?")
  rbisDebDate.font = currentFont
  rbisDebDate.selected = true

  var seq = scala.collection.immutable.Seq[Int]()
  seq = seq :+ -1
  seq = seq :+ 0
  seq = seq :+ +1
  val cbxDuration = new ComboBox(seq)
  cbxDuration.selection.item = 0
  val rbViewAllAverages = new RadioButton("<= Show AllAverages")
  rbViewAllAverages.font = currentFont
  rbViewAllAverages.selected = false
  val tfActors = new TextField(12)
  tfActors.font = currentFont
  tfActors.text = "4"
    tfActors.minimumSize = min2Size
  val tfDecalTimeZone=new TextField(12)
  
  val rbDebug = new RadioButton(" <= Debug Mode ?")
  rbDebug.font = currentFont
  rbDebug.selected = false
  val rbPivotParsingExhaustif = new RadioButton("<= Exhaustive Parsing ?")
  rbPivotParsingExhaustif.font = currentFont
  rbPivotParsingExhaustif.selected = false
  val rbGenerateEnrToFile = new RadioButton("<= Trace records ?")
  rbGenerateEnrToFile.font = currentFont
  rbGenerateEnrToFile.selected = false

  initPanFileIn
  initPanFileOut
  initPanAdvanced
  load(this)

  visible = true
  this.repaint()
  def initReader(fileIn: File): BufferedReader = {

    try {
      if (fileIn.getName().endsWith(".gz")) {
        isGzFile = true
        new BufferedReader(new InputStreamReader(
          new GZIPInputStream(new FileInputStream(fileIn))),
          SwingScaViewer.tmpProps.getProperty("scaviewer.filesstat.sizeBuffer").toInt)

      } else {
        isGzFile = false
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

  def initPanFileIn {
    var gbc3 = new pan1.Constraints
    gbc3.insets = insets1
    gbc3.fill = GridBagPanel.Fill.None
    gbc3.gridx = 0
    gbc3.gridy = 0
    gbc3.weightx = 1.0
    gbc3.weighty = 0.0

    // name of the file and step of concatenation
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("Path File In") -> gbc3)
    gbc3.gridx = 1
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfFileIn -> gbc3)
    gbc3.gridx = 2
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("Step of Aggr in ms") -> gbc3)
    gbc3.gridx = 3
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfStepAggr -> gbc3)

    // Debut et Fin d'enregistrement
    gbc3.gridy = 1
    gbc3.gridx = 0
    var lab1 = new Label("Start of Enreg (Regex)")
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (lab1 -> gbc3)
    gbc3.gridx = 1
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfDebEnr -> gbc3)
    gbc3.gridx = 2
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("End of Enreg (Regex)") -> gbc3)
    gbc3.gridx = 3
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfFinEnr -> gbc3)

    // Inclusion exclusion  enregistrement
    gbc3.gridy = 2
    gbc3.gridx = 0

    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("Regex including enregs") -> gbc3)
    gbc3.gridx = 1
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfInclEnr -> gbc3)
    gbc3.gridx = 2
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("Regex excluding enregs") -> gbc3)
    gbc3.gridx = 3
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfExclEnr -> gbc3)

    // Explicit date et  pas d'increment eventuel
    gbc3.gridy = 3
    gbc3.gridx = 0
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("<html>Step/enregs<br/>regex/step </html>") -> gbc3)
    gbc3.gridx = 1
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfStep2Enr -> gbc3)
    gbc3.gridx = 2
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (this.rbLocaleIn -> gbc3)
    gbc3.gridx = 3
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (rbExplicitDate -> gbc3)

    // Date de debut implicite  et Locale
    gbc3.gridy = 4
    gbc3.gridx = 0
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("Start Date (DateInMillis or Implicit case)") -> gbc3)
    gbc3.gridx = 1
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfDateImplDebut -> gbc3)
    gbc3.gridx = 2
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("Unit of step (ex:ms)") -> gbc3)
    gbc3.gridx = 3
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfStepUnit -> gbc3)

    // Date regex et java format de la date
    gbc3.gridy = 5
    gbc3.gridx = 0
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("Regex of Date") -> gbc3)
    gbc3.gridx = 1
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfDateRegex -> gbc3)
    gbc3.gridx = 2
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("Java Format of Date") -> gbc3)
    gbc3.gridx = 3
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfDateFormat -> gbc3)

    //Date de debut et Fin d'analyse format de la date = Java Format Of date si pas date in millis
    // sinon yyyy/MM/dd:HH:mm:ss
    gbc3.gridy = 6
    gbc3.gridx = 0
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("Parsing Start Date") -> gbc3)
    gbc3.gridx = 1
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfStartDate -> gbc3)
    gbc3.gridx = 2
    gbc3.anchor = GridBagPanel.Anchor.East
    pan1.layout += (new Label("Parsing End Date") -> gbc3)
    gbc3.gridx = 3
    gbc3.anchor = GridBagPanel.Anchor.West
    pan1.layout += (tfEndDate -> gbc3)
    repaint
  }

  def initPanFileOut {
    var gbc3 = new pan2.Constraints
    gbc3.insets = insets1
    gbc3.fill = GridBagPanel.Fill.None
    gbc3.gridx = 0
    gbc3.gridy = 0
    gbc3.weightx = 1.0
    gbc3.weighty = 0.0
    // name of the file and field separator
    gbc3.anchor = GridBagPanel.Anchor.East
    pan2.layout += (new Label("Folder for Files Out") -> gbc3)
    gbc3.gridx = 1
    gbc3.anchor = GridBagPanel.Anchor.West
    pan2.layout += (tfFileOut -> gbc3)
    gbc3.gridx = 2
    gbc3.anchor = GridBagPanel.Anchor.East
    pan2.layout += (new Label("Field separator") -> gbc3)
    gbc3.gridx = 3
    gbc3.anchor = GridBagPanel.Anchor.West
    pan2.layout += (tfFsOut -> gbc3)

    //Format date out, locale annd All AverageOnly et  pas d'increment eventuel
    gbc3.gridy = 3
    gbc3.gridx = 0
    gbc3.anchor = GridBagPanel.Anchor.East
    pan2.layout += (new Label("Java Date Format out") -> gbc3)
    gbc3.gridx = 1
    gbc3.anchor = GridBagPanel.Anchor.West
    pan2.layout += (tfDateFormatOut -> gbc3)
    gbc3.gridx = 2
    gbc3.anchor = GridBagPanel.Anchor.East
    pan2.layout += (this.rbLocaleOut -> gbc3)
    gbc3.gridx = 3
    gbc3.anchor = GridBagPanel.Anchor.West
    pan2.layout += (rbAllAveragesOnly -> gbc3)

  }
  def initPanAdvanced {
    var gbc3 = new pan3.Constraints
    gbc3.insets = insets1
    gbc3.fill = GridBagPanel.Fill.None
    gbc3.gridx = 0
    gbc3.gridy = 0
    gbc3.weightx = 1.0
    gbc3.weighty = 0.0

    // Number of akka Actors in //
    // comboBox for correcting date ( 0 => no correction, +1 =>  add duration if exist to date, -1 => minus duration if exist to date
    // Affichage automatique de AllAverages en fin de parsing
    gbc3.anchor = GridBagPanel.Anchor.East
    pan3.layout += (new Label("Number of Actors") -> gbc3)
    gbc3.gridx = 1
    gbc3.anchor = GridBagPanel.Anchor.West
    pan3.layout += (tfActors -> gbc3)

    gbc3.gridx = 2
     gbc3.anchor = GridBagPanel.Anchor.East
    pan3.layout += (new Label("decalTimeZone(ms)") -> gbc3)
    gbc3.gridx = 3
    gbc3.anchor = GridBagPanel.Anchor.West
    pan3.layout += (tfDecalTimeZone -> gbc3) 

    gbc3.gridx = 4
    gbc3.anchor = GridBagPanel.Anchor.East
    pan3.layout += (rbisDebDate -> gbc3)
    
    
    gbc3.gridy = 1
    gbc3.gridx = 0
    gbc3.anchor = GridBagPanel.Anchor.Center
    pan3.layout += (rbViewAllAverages -> gbc3)
    gbc3.gridx = 1
    gbc3.anchor = GridBagPanel.Anchor.Center
    pan3.layout += (rbDebug -> gbc3)
    gbc3.gridx = 2
    gbc3.anchor = GridBagPanel.Anchor.Center
    pan3.layout += (rbPivotParsingExhaustif -> gbc3)
    gbc3.gridx = 3
    pan3.layout += (rbGenerateEnrToFile -> gbc3)
     gbc3.gridx = 4
     gbc3.anchor = GridBagPanel.Anchor.East
    pan3.layout += (new Label("Correction of date") -> gbc3)
    gbc3.gridx = 5
    gbc3.anchor = GridBagPanel.Anchor.West
    pan3.layout += (cbxDuration -> gbc3)


  }

  // reactions
  this.rbExplicitDate.reactions += {
    case ActionEvent(`rbExplicitDate`) =>
      if (!rbExplicitDate.selected) {
        tfStep2Enr.editable = true
        tfDateRegex.editable = false
        tfDateImplDebut.editable = true
        tfStepUnit.editable = true
        this.repaint
      } else {
        tfStep2Enr.editable = false
        tfDateRegex.editable = true
        tfDateImplDebut.editable = true
        tfStepUnit.editable = false
        this.repaint
      }
  }

  this.saveBut.reactions += {
    case ActionEvent(`saveBut`) =>
      println("saving config prefixFile=" + prefixFile)
      save(this)

  }
  this.tplBut.reactions += {
    case ActionEvent(`tplBut`) =>
      save(this)
      val file = new File(path + File.separator + "logs" + File.separator + "config" + File.separator + "scaparser" + File.separator + prefixFile + ".properties")
      var diag = new DialogScaParserTemplate(file)

  }
  this.cancBut.reactions += {
    case ActionEvent(`cancBut`) =>
      visible = false
      SwingScaViewer.mainPanel.contents.clear

  }
  this.regBut.reactions += {
    case ActionEvent(`regBut`) =>
      visible = true
      new MyDialogRegexp(null, false)

  }
  this.parseBut.reactions += {
    case ActionEvent(`parseBut`) =>

      // des champs debut/fin de filtrage
      val reg = """\d{4}/\d\d/\d\d\s+\d\d:\d\d:\d\d""".r
      var message = ""
      if (None == reg.findFirstIn(tfStartDate.text)) {
        message += "Date for the beginning of parsing is not correctly formatted (yyyy/MM/dd HH:mm:ss)\n"
      }
      if (None == reg.findFirstIn(tfEndDate.text)) {
        message += "Date for the end of parsing is not correctly formatted (yyyy/MM/dd HH:mm:ss)\n"
      }
      if (message.length() > 10) {
        JOptionPane.showMessageDialog(null, message, "alert", JOptionPane.ERROR_MESSAGE);
      } else {
        visible = false
        if (null != configSystem){
            var fut: scala.concurrent.Future[akka.actor.Terminated]=   configSystem.terminate()
  while (!fut.isCompleted){
    
  }
        
        }
        save(this)
        configSystem = ActorSystem("MyConfSystem")
        var arRef = configSystem.actorOf(Props[ScaParserMain], "scaParser")
        // ScaParserMain(this)
        arRef ! this.props
      }

  }
  //mouseListener
  def mouseClicked(e: MouseEvent) {

    if (e.getSource().isInstanceOf[JTable]) {
      val tmpJtable= e.getSource().asInstanceOf[JTable];
      if(jtPivots == tmpJtable){
      
      if (e.getButton() == MouseEvent.BUTTON2
        || e.getButton() == MouseEvent.BUTTON3) {
        System.out.println("mouseClicked on JTable: "
          + e.getButton());
        var chooser = new javax.swing.JFileChooser()
        chooser.setFileSelectionMode(javax.swing.JFileChooser.FILES_ONLY)
        var filter: javax.swing.filechooser.FileNameExtensionFilter = new javax.swing.filechooser.FileNameExtensionFilter(
          "Config Pivots from Files", "csv", "txt")
        chooser.setFileFilter(filter);
        var dirWork = SearchDirFile.searchYoungestDir(System.getProperty("workspace") +
          File.separator +
          SwingScaViewer.currentProject, SwingScaViewer.tmpProps.getProperty("scaviewer.prefixscenario").r)
        chooser.setCurrentDirectory(dirWork)
        var returnVal = chooser.showOpenDialog(null);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
          var file = chooser.getSelectedFile().getAbsolutePath()
          System.out.println("You chose to open this file: "
            + chooser.getSelectedFile().getAbsolutePath());
          fillPivotsFromCsvFile(chooser.getSelectedFile().getAbsolutePath(), e.getSource().asInstanceOf[JTable]);

        }
      }
    }
    }
    

  }
  def fillPivotsFromCsvFile(strFile: String, jtable: JTable) {

    // TODO Auto-generated method stub
    try {
      val raf: RandomAccessFile = new RandomAccessFile(new File(strFile), "rw");
      var line: String = null;
      var row = 0;
      var tabCar: Array[String] = Array("(", ")", "?", "[", "]", "}", "}", "*", "+", "$", ".")
      var bool = true
      while (bool) {
        //while ((line = raf.readLine()) != null) {
        line = raf.readLine()
        if (null != line) {

          var tabStr = line.split(";")
          val matcher = Pattern.compile("\\d+").matcher(tabStr(0))
          if (tabStr.length > 1 && tabStr(1).length() > 0 && !tabStr(1).equalsIgnoreCase("Total") && matcher.find()) {
            // on prend et on mets dans la table en faisant les
            // remplacements corrects:
            // Colonne 0, tous les / et les ? ( ) { } [ ]sont remplac�
            // par l'underscore
            // Colonne 1 il faut echapper ces caracteres speciaux

            var len = tabCar.length

            var column0 = ""
            var column1 = ""
            column1 = tabStr(1)
            column0 = tabStr(1).replaceAll("\\s+", "_")
            column0 = column0.replaceAll("/", "_")
            column0 = column0.replaceAll("\"", "_")
            column0 = column0.replaceAll(":", "_")
            column0 = column0.replaceAll(";", "_")
            column0 = column0.replaceAll(",", "_")
            column0 = column0.replaceAll("<", "_")
            column0 = column0.replaceAll(">", "_")
            column0 = column0.replaceAll("%", "_")
            column0 = column0.replaceAll("&", "_")
            column0 = column0.replaceAll("$", "_")
            column0 = column0.replaceAll("'", "_")
            column0 = column0.replaceAll("\"", "_")
            column0 = column0.replaceAll("`", "_")
            for (i <- 0 until len) {

              column0 = column0.replaceAll("\\" + tabCar(i), "_")
              column1 = column1.replaceAll("\\" + tabCar(i), "\\\\" + tabCar(i))

            }
            jtable.setValueAt(column0, row, 0)
            jtable.setValueAt(column1, row, 1)
            row += 1
          }
        } else {
          bool = false
        }
      }

    } catch {

      case e: FileNotFoundException => e.printStackTrace()
      case e: IOException => e.printStackTrace()
    }

  }

  def mouseEntered(e: MouseEvent) {}

  def mouseExited(e: MouseEvent) {}

  def mousePressed(e: MouseEvent) {}

  def mouseReleased(e: MouseEvent) {}

  // keyListener
  def keyPressed(e: KeyEvent) {

    if (e.getSource().isInstanceOf[JTable]) {
      
      if (e.getKeyCode() == KeyEvent.VK_DELETE) {

        val jt = e.getSource().asInstanceOf[JTable]

        val tm: DefaultTableModel = jt.getModel().asInstanceOf[DefaultTableModel]

        val rows = jt.getSelectedRows()
        // Commencer par la derniere ligne a supprimer
        for (i <- rows.length until 0 by -1) {
          tm.removeRow(rows(i - 1))
        }
        // ajouter les lignes en fin de tables
        for (i <- rows.length until 0 by -1) {
          tm.addRow(new Array[Object](jt.getColumnCount))
        }
      }
    }

  }

  def keyReleased(e: KeyEvent) {}

  def keyTyped(e: KeyEvent) {}

}