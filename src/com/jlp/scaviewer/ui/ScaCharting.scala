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
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.SplitPane
import scala.swing.ScrollPane
import scala.swing.ScrollPane.BarPolicy.Value
import javax.swing.ScrollPaneConstants
import java.awt.Dimension
import scala.swing.Component
import java.io.File
import org.jfree.chart.ChartPanel
import scala.swing.Table
import javax.swing.ToolTipManager
import scala.swing.Button
import com.jlp.scaviewer.ui._
import scala.collection.mutable.ListBuffer
import java.util.Properties
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.IOException
import javax.swing.JPanel
import org.jfree.chart.ChartFactory
import scala.swing.event.ActionEvent
import java.awt.Insets
import scala.swing.Alignment
import scala.swing.FlowPanel
import java.awt.Font
import javax.swing.tree.TreeSelectionModel
import javax.swing.DropMode
import javax.swing.JTree
import scala.swing.GridPanel
import scala.swing.GridBagPanel
import scala.Enumeration
import javax.swing.border.LineBorder
import java.awt.Color
import scala.swing.MenuBar
import java.util.ArrayList
import javax.swing.table.DefaultTableModel
import java.awt.GridBagConstraints
import javax.swing.table.TableRowSorter
import java.util.Comparator
import javax.swing.JTable
import javax.swing.JScrollPane
import scala.swing.Panel
import scala.swing.TextField
import scala.swing.RadioButton
import scala.swing.Label
import scala.swing.ComboBox
import com.jlp.scaviewer.ui.tableandchart.ScaChartingListener
import scala.collection.mutable.ArrayBuffer
import com.jlp.scaviewer.timeseries.StructTs
import com.jlp.scaviewer.ui.tableandchart.Distances
import com.jlp.scaviewer.commons.utils.Couleurs
import scala.collection.mutable.HashMap
import com.jlp.scaviewer.timeseries.MapDatasetToTs
import javax.swing.JFileChooser
import com.jlp.scaviewer.ui.tableandchart.MyChartPanel
import akka.actor.Actor
import com.jlp.scaviewerdyn.ui.actors.MyActorReaction
import com.jlp.scaviewerdyn.ui.actors.MyMessage
import akka.actor.ActorRef
import akka.actor.Kill
import com.jlp.scaviewerdyn.ui._
import java.io.RandomAccessFile
import javax.swing.JRadioButton
import com.jlp.scaviewer.ui.tableandchart.ScaChartingListener

class ScaCharting extends SplitPane {
  val gp = new GridBagPanel()
  leftComponent = gp
  leftComponent.border = new LineBorder(Color.BLACK)
  var gbc = new gp.Constraints()
  this.oneTouchExpandable = true
  val rightTop = new BoxPanel(Orientation.Vertical)
  val rightBottom = new GridBagPanel()
  val splPaneRight = new SplitPane(Orientation.Horizontal, rightTop, rightBottom)
  splPaneRight.oneTouchExpandable = true
  val dimButton = new Dimension(100, 30)
  private val bRoot = new Button("New Root")
  private val bParent = new Button("../")

  // val splPaneContents=new SplitPane(Orientation.Vertical,left,splPaneRight)
  //	var listFiles:List[File] = List[File]();

  //var prefixes:String = "";
  var myTooltipManager = ToolTipManager
    .sharedInstance();

  var dimThis: Dimension = null;
  var transfertHandler: FileTransfertHandler = null;
  private var rootRep: String = "";

  //left.verticalScrollBarPolicy = new Value(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
  //left.horizontalScrollBarPolicy = left.verticalScrollBarPolicy

  val taille = new Dimension(this.toolkit.getScreenSize.width - 30, this.toolkit.getScreenSize.height - 50)
  this.dividerLocation = taille.width / 4
  splPaneRight.dividerLocation = 2 * taille.height / 3
  listenTo(bRoot, bParent)
  reactions += {
    case ActionEvent(`bRoot`) =>
      {
        //      println("bRoot Pressed")

        val jfc: JFileChooser = new JFileChooser(ScaCharting.root);
        jfc.setDialogType(JFileChooser.CUSTOM_DIALOG);
        jfc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        jfc.setControlButtonsAreShown(true);
        jfc.setMultiSelectionEnabled(false);
        val ret: Int = jfc.showOpenDialog(null);
        if (ret == JFileChooser.APPROVE_OPTION) {

          var file: File = jfc.getSelectedFile();

          ScaCharting.mainPanel.contents.clear()
          ScaCharting.mainPanel.contents += ScaCharting(file.getAbsolutePath(), ScaCharting.dyn)
          ScaCharting.chartPanel.setChart(ChartFactory.createTimeSeriesChart(null,
            null,
            null,
            null, // Mettre ici le dataSet
            false,
            true,
            true))
          ScaCharting.mainPanel.visible = false
          ScaCharting.mainPanel.visible = true
        }
      }

    case ActionEvent(`bParent`) =>
      {
        // println("bParent Pressed File.separator=" + File.separator)
        var sep = File.separator
        val oldRoot = ScaCharting.root
        if (!oldRoot.contains(sep)) {
          sep = "/"
        }
        var idx = oldRoot.lastIndexOf(sep)
        var newRoot = oldRoot
        if (idx > 0) {
          newRoot = oldRoot.substring(0, idx)
          if (newRoot.charAt(newRoot.length() - 1) == ':') {
            newRoot += sep
          }

        }
        //  println("oldRoot =" + oldRoot + " idx=" + idx + " ,newRoot=" + newRoot)

        ScaCharting.mainPanel.contents.clear()
        //  println("  mScaCharting clicked mainPanel.contents.length=" + SwingScaViewer.mainPanel.contents.length)
        ScaCharting.mainPanel.repaint()

        ScaCharting.mainPanel.contents += ScaCharting(newRoot, ScaCharting.dyn)
        ScaCharting.chartPanel.setChart(ChartFactory.createTimeSeriesChart(null,
          null,
          null,
          null, // Mettre ici le dataSet
          false,
          true,
          true))
        ScaCharting.mainPanel.visible = false
        ScaCharting.mainPanel.visible = true
      }
  }

}

object ScaCharting {
 

  case class ChartingInfo(var title: String, var lenFile: Long, var lastModified: Long, nbPoints: Int, isNbPointsMax: Boolean, strategy: String, isTimeSeries: Boolean = true, var sep:String,var nbItems:Int,var regexDate:String,
      var dateFormat:String,var posPivots: List[Int],var posValues:List[Int],var pasInMillis:Long)
  var root: String = ""
  var suffixes = "";
  var dir = System.getProperty("root") + File.separator + "config";
  var f = new File(dir + File.separator + "scaViewer.properties");
  var tmpProps: Properties = new Properties();
  try {
    tmpProps.load(new FileInputStream(f));
    suffixes = tmpProps.getProperty("jtree.suffixes");
  } catch {
    case e: FileNotFoundException =>
      // TODO Auto-generated catch block
      e.printStackTrace();
    case e: IOException =>

      // TODO Auto-generated catch block
      e.printStackTrace();
  }
   val sepSeries= tmpProps.getProperty("timeseries.nameTimeSeries.separator")
  var myTable: MyTable = null
  var listener: ScaChartingListener = null
  var listFiles: List[File] = List.empty
  var arrEnrichised = new ArrayBuffer[StructTs]()
  var listChartingInfo: List[ChartingInfo] = List.empty
  var listPasInMillis: List[Long] = List.empty
  var listRaf: List[RandomAccessFile] = List.empty

  var hiddenTs: scala.collection.mutable.HashMap[String, MapDatasetToTs] = new HashMap()
  var sampling = false

  val bRefresh = new Button("ReLoad")
  val bClear = new Button("Clear")
  val bSample = new Button("Sample")
  val rdbMaxPointsOrGap = new RadioButton("By points ?")

  rdbMaxPointsOrGap.selected = true

  //val rdbTimeSeries = new RadioButton("TimeSeries ?")

  //  rdbTimeSeries.selected = true
  //  rdbTimeSeries.tooltip = "<html> When checked, TimeSerie => first column are the dates <br/>When unchecked, XYSeries => first column is the X Axis <b>Not yet implemented</b></html>"
  //  rdbTimeSeries.enabled=false

  val bCompare = new Button("Align-")
  bCompare.tooltip = "<html> Permits to compare 2 or more series <br/> by translating the begining of the right series<br/> to the beginning of the more left serie</html>"

  val bComparePlus = new Button("Align+")
  bComparePlus.tooltip = "<html> Permits to compare 2 or more series <br/> by translating the begining of the lefts series<br/> to the beginning of the more ritgth serie</html>"

    
   val rbShortName = new RadioButton ("Short name ?")
   rbShortName.selected= true
  
  val tfSample = new TextField(tmpProps.getProperty("scaviewer.sampling.nbPoints"), 10)
  val dim1 = new Dimension(60, 20)
  tfSample.preferredSize = dim1
  tfSample.minimumSize = dim1
  val colForLine: Couleurs = new Couleurs
  val colForRangeAxis: Couleurs = new Couleurs

  // used when zooming
  var lowerDateInMillis = 0L
  var upperDateInMillis = 0L

  val items = Seq("AVERAGE", "MAX", "MIN", "MEDIANE", "PERCENTILE90", "SUM")
  val cbStrategie = new ComboBox(items)
   val dim2 = new Dimension(100,20)
  cbStrategie.minimumSize=dim2
  cbStrategie.preferredSize=dim2
  val sp = "     "
  val chartPanel = new ChartPanel(ChartFactory.createTimeSeriesChart(null, null, null, null, false, true, true)) with MyChartPanel;

  chartPanel.setDisplayToolTips(true)
  rdbMaxPointsOrGap.tooltip = "<html> When checked, maximal number of points for series<br/>When unchecked, grouping by gap of " + ScaCharting.tfSample.text + " milliseconds </html>"

  val dist: Distances = new Distances()
  var tableInit: JTable = null;
  var mainPanel: BoxPanel = null

  // Partie dynamique
  var dyn = false
  // println("on passe  dans statique")

  def apply(path: String, dynParam: Boolean): ScaCharting =
    {
   
      dyn = dynParam

      val scaCharting = new ScaCharting()

      if (dyn) {
        ScalaChartingDyn.boolExamine = true
        if (!ScalaChartingDyn.isRunning) ScalaChartingDyn.start()
        // myActor ! "stop"
      } else {
        //verifier que les actors ne marchent plus
        ScalaChartingDyn.boolExamine = true
        ScalaChartingDyn.stop()
      }

     listener =  new ScaChartingListener()
      root = path
      scaCharting.orientation = Orientation.Vertical
      // println("scaCharting.leftComponent.size=" + scaCharting.leftComponent.size)

      scaCharting.leftComponent.preferredSize = new Dimension(scaCharting.taille.width / 4, scaCharting.taille.height)

      scaCharting.rightComponent = scaCharting.splPaneRight

      // Inserion d'un chart vide

      scaCharting.transfertHandler = new FileTransfertHandler(chartPanel);
      chartPanel.setTransferHandler(scaCharting.transfertHandler);

      scaCharting.rightComponent.asInstanceOf[SplitPane].leftComponent.peer.asInstanceOf[JPanel].add(chartPanel)
      // Insertion button et JTree

      scaCharting.bRoot.preferredSize = scaCharting.dimButton
      scaCharting.bParent.preferredSize = scaCharting.dimButton
      scaCharting.bRoot.minimumSize = scaCharting.dimButton
      var insets1 = new Insets(10, 10, 10, 10)
      // var insets2=new Insets(10,30,10,30)
      scaCharting.bRoot.margin = insets1
      scaCharting.bParent.margin = insets1
      scaCharting.gbc.fill = GridBagPanel.Fill.Horizontal
      scaCharting.gbc.insets = new Insets(0, 0, 0, 0)
      scaCharting.gbc.gridx = 0
      scaCharting.gbc.gridy = 0
      scaCharting.gbc.gridwidth = GridBagConstraints.RELATIVE

      // gbc.insets=new Insets(10,10,10,10)
      scaCharting.gbc.anchor = GridBagPanel.Anchor.Center
      val fp = new FlowPanel()
      fp.border = new LineBorder(Color.BLACK)

      fp.hGap = 20
      fp.vGap = 20

      fp.contents += scaCharting.bRoot
      fp.contents += scaCharting.bParent
      scaCharting.gp.layout += ((fp -> scaCharting.gbc))

      // Rajouter le Jtree

      scaCharting.font = new Font("Arial", Font.BOLD, 12)
      var mode = TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION;

      var myPanelJtree: MyPanelJtree = MyPanelJtree(path, chartPanel)
      myPanelJtree.preferredSize = new Dimension((scaCharting.taille.width / 4) - 20, 3 * scaCharting.taille.height / 4)

      myPanelJtree.verticalScrollBarPolicy = new Value(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS, ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
      myPanelJtree.horizontalScrollBarPolicy = myPanelJtree.verticalScrollBarPolicy
      scaCharting.gbc.gridy = 1
      scaCharting.gbc.weightx = 1.0;
      scaCharting.gbc.weighty = 1.0;
      scaCharting.gbc.anchor = GridBagPanel.Anchor.NorthWest
      scaCharting.gbc.gridheight = GridBagConstraints.REMAINDER
      scaCharting.gbc.fill = GridBagPanel.Fill.Both
      scaCharting.gp.layout += ((myPanelJtree -> scaCharting.gbc))

      // Mise en place menubar + table
      var gbc2 = new scaCharting.rightBottom.Constraints()
      gbc2.insets = insets1
      gbc2.weightx = 0.0;
      gbc2.weighty = 0.0;

      gbc2.gridy = 0
      gbc2.gridx = 0
      gbc2.gridwidth = GridBagConstraints.REMAINDER

      gbc2.anchor = GridBagPanel.Anchor.NorthWest
      val menuBar = new MenuBar()
      menuBar.contents += bRefresh
      menuBar.contents += new Label(sp)
      menuBar.contents += bClear
      menuBar.contents += new Label(sp)
      menuBar.contents += bSample
      menuBar.contents += new Label("=>")
      menuBar.contents += tfSample
      menuBar.contents += new Label("<=")
      menuBar.contents += rdbMaxPointsOrGap
      menuBar.contents += new Label(sp)
      menuBar.contents += new Label("strategy => ")

      menuBar.contents += cbStrategie
      menuBar.contents += new Label(sp)
      //  menuBar.contents += rdbTimeSeries
      menuBar.contents += bCompare
      menuBar.contents += bComparePlus
      menuBar.contents += rbShortName
      scaCharting.rightBottom.layout += ((menuBar -> gbc2))

      gbc2.fill = GridBagPanel.Fill.Horizontal
      gbc2.weightx = 1.0;
      gbc2.weighty = 1.0;
      gbc2.gridy = 1
      gbc2.gridx = GridBagConstraints.REMAINDER
      gbc2.fill = GridBagPanel.Fill.Both
      gbc2.anchor = GridBagPanel.Anchor.NorthWest
      //tabTs.repaint()
      //  tabTs.xLayoutAlignment=0
      //  tabTs.yLayoutAlignment=0
      myTable = new MyTable()
      scaCharting.rightBottom.layout += ((myTable -> gbc2))
      // Test table
      //  var list: List[List[String]] = List(List("aaa", "bbb", "ccc", "aaa","vvv", "bbb", "ccc", "aaa", "bbb", "ccc", "aaa", "bbb", "ccc"), List("cccc", "aaa", "ccc", "aaa", "bbb", "ccc", "aaa", "bbb", "ccc", "aaa", "bbb", "ccc"))
      //var tabObject:Array[Array[Object]]=Array[Array[Object]](Array[String]("aaa","bbb","ccc", "aaa","bbb","ccc", "aaa","bbb","ccc", "aaa","bbb","ccc") , Array[String]("cccc","aaa","ccc", "aaa","bbb","ccc", "aaa","bbb","ccc", "aaa","bbb","ccc" ))

      //myTable.tabModel.addRow(Array[Object]("aaa", "bbb", "ccc", new java.lang.Boolean(false), 21L.asInstanceOf[java.lang.Long], Color.red, "aaa", "vvv","bbb", "ccc", "aaa", "bbb", "ccc"))
      //myTable.tabModel.addRow(Array[Object]("cccc", "aaa", "ccc", new java.lang.Boolean(true), 22L.asInstanceOf[java.lang.Long], Color.black, "aaa","vvv", "bbb", "ccc", "aaa", "bbb", "ccc"))
      scaCharting.repaint()

      scaCharting
    }

}