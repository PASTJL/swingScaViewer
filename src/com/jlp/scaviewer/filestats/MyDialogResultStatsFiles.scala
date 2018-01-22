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
import javax.swing.JTable
import scala.swing.ScrollPane
import javax.swing.table.DefaultTableModel
import java.awt.Toolkit
import java.awt.Dimension
import scala.collection.immutable.HashMap
import java.io.File
import java.util.Properties
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.IOException
import javax.swing.table.TableColumn
import java.awt.Font
import java.awt.Color
import java.awt.event.MouseListener
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.table.JTableHeader
import java.awt.Point
import scala.swing.GridPanel
import scala.swing.CheckBox
import scala.swing.event.WindowClosing
import javax.swing.JPopupMenu
import javax.swing.JMenuItem
import com.jlp.scaviewer.commons.utils.SearchDirFile
import com.jlp.scaviewer.ui.SwingScaViewer
import scala.swing.FileChooser
import java.io.RandomAccessFile
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.table.TableColumnModel


class MyDialogResultStatsFiles(diag: Dialog) extends Dialog(diag) {

  val nbLinesTotal = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.numberTop").toInt + 1;
  var strPercentile = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.percentile")
  val scp = new ScrollPane
   val colorSelected=new Color(170, 242, 249)
  var columnsName = Array[String](
    "Num Row", "Criteria", "Count",
    "Percent", "Sum", "Average", "Minimum", "Maximum", "Mediane",
    "Percentile (" + strPercentile + " %)", "StdDev")
  MyDialogResultStatsFiles.percentile = "Percentile (" + strPercentile + " %)"
  var nbLinesTable=0;
  var tModel: MyDialogResultStatsFiles.MyTableModel = new MyDialogResultStatsFiles.MyTableModel(columnsName,
    nbLinesTotal);
  val table = new JTable(tModel)
  scp.peer.setViewportView(table)
  val th = table.getTableHeader
  val listener = new MyMouseAdapter(this)
  th.addMouseListener(listener)
  table.addMouseListener(listener)
  table.setAutoResizeMode(JTable.AUTO_RESIZE_SUBSEQUENT_COLUMNS)
  //table.setDefaultRenderer(classOf[scala.Double], new MyStatsDoubleCellRenderer())
  table.setDefaultRenderer(classOf[java.lang.Double], new MyStatsDoubleCellRenderer(Color.WHITE,Color.BLACK))
  table.setDefaultRenderer(classOf[java.lang.Integer], new MyStatsIntCellRenderer(Color.WHITE,Color.BLACK))
  table.setDefaultRenderer(classOf[java.lang.String], new MyStatsStringCellRenderer(Color.WHITE,Color.RED))
  
  table.setRowSelectionAllowed(true)
  table.getTableHeader.setFont(new Font("Arial", Font.BOLD, 14))
  table.getTableHeader.setBackground(Color.LIGHT_GRAY)
  this.contents = scp
  val dim = Toolkit.getDefaultToolkit().getScreenSize()
  val dimDialog = new Dimension(dim.width * 3 / 4, dim.height * 3 / 4)
  preferredSize = dimDialog
  //myDiag.minimumSize = dimDialog

  this.title = "Parsing :" + MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.nameFile") + " " +
    MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.debOfAnalyse") + " -> " +
    MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.finOfAnalyse")
  var dir = System.getProperty("root") + File.separator + "config";
  var f = new File(dir + File.separator + "scaViewer.properties");
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

  val width = 2 * math.min(50, tmpProps.getProperty("scaviewer.unitWidth").toInt)
  var sizeColumnVisible: HashMap[String, (Int, Int, Int)] = new HashMap
  sizeColumnVisible += ("Num Row" -> (0, width, width))
  sizeColumnVisible += ("Criteria" -> (0, 4 * width + 30, 50 * width + 20))
  sizeColumnVisible += ("Count" -> (0, width, width))
  sizeColumnVisible += ("Percent" -> (0, width, width))
  sizeColumnVisible += ("Sum" -> (0, width, width))
  sizeColumnVisible += ("Average" -> (0, width, width))
  sizeColumnVisible += ("Minimum" -> (0, width, width))
  sizeColumnVisible += ("Maximum" -> (0, width, width))
  sizeColumnVisible += ("Mediane" -> (0, width, width))
  sizeColumnVisible += ("Percentile (" + strPercentile + " %)" -> (0, width, 2 * width))
  sizeColumnVisible += ("StdDev" -> (0, width, width))

  val nbCols = table.getColumnCount()
  for (i <- 0 until nbCols) {
    var col: TableColumn = table.getColumnModel().getColumn(i)
    var name = table.getColumnName(i)
    col.setMinWidth(sizeColumnVisible.get(name).get._1)
    col.setPreferredWidth(sizeColumnVisible.get(name).get._2)
    col.setMaxWidth(sizeColumnVisible.get(name).get._3)

  }
  //println("MyDialogStatsFile.tabHm(0)keys=" + MyDialogStatsFile.tabHm(0).keySet)
  var res = (MyDialogStatsFile.tabHm(0) filter (_._1 != "Total") map (tup => tup._2.setName(tup._1))).toList
  //println("MyDialogStatsFile.res:" + res)
  var topN = MyDialogStatsFile.currentProps.getProperty("scaviewer.filestats.numberTop").toInt
  resizable = true

  this.visible = true

  this.pack
  this.modal = false
  tModel.maxRow = res.length
  fillTable("Count")
  listener.setBackground(table,table.getColumnModel.getColumnIndex("Count"))
  def fillTable(col: String) {
    var lst = tri(col)
    nbLinesTable=lst.size
    if(nbLinesTable>0)
    {
    var numEnr = 0
    var countGlobal = MyDialogStatsFile.tabHm(0).get("Total").get.count
    for (cEnr <- lst) {
      table.setValueAt(numEnr, numEnr, table.getColumnModel.getColumnIndex("Num Row"))
      table.setValueAt(cEnr.name, numEnr, table.getColumnModel.getColumnIndex("Criteria"))
      table.setValueAt(cEnr.count, numEnr, table.getColumnModel.getColumnIndex("Count"))
      table.setValueAt(cEnr.count.toDouble * 100 / countGlobal.toDouble, numEnr, table.getColumnModel.getColumnIndex("Percent"))
      table.setValueAt(cEnr.sum, numEnr, table.getColumnModel.getColumnIndex("Sum"))
      table.setValueAt(cEnr.mean, numEnr, table.getColumnModel.getColumnIndex("Average"))
      table.setValueAt(cEnr.min, numEnr, table.getColumnModel.getColumnIndex("Minimum"))
      table.setValueAt(cEnr.max, numEnr, table.getColumnModel.getColumnIndex("Maximum"))
      table.setValueAt(cEnr.mediane, numEnr, table.getColumnModel.getColumnIndex("Mediane"))
      table.setValueAt(cEnr.percentile, numEnr, table.getColumnModel.getColumnIndex("Percentile (" + strPercentile + " %)"))
      table.setValueAt(cEnr.stdDev, numEnr, table.getColumnModel.getColumnIndex("StdDev"))
      numEnr += 1

    }
    }
    
   
    
  }
  def tri(col: String): List[CumulEnregistrementStatMemory] =
    {
      var ret: List[CumulEnregistrementStatMemory] = List.empty
      if(res.size > 0)
      {
      col match {
        case "Count" =>
          {

           // println("tri Count res.length =" + res.length)
           // println("tri Count MyDialogStatsFile.tabHm(0).length =" + MyDialogStatsFile.tabHm(0).size)
            ret = res sortWith (_.count > _.count) take (topN)
            ret = ret :+ MyDialogStatsFile.tabHm(0).get("Total").get.setName("Total")
            
          }

        case "Percent" =>
          {

           // println("tri Percent res.length =" + res.length)
          //  println("tri Percent MyDialogStatsFile.tabHm(0).length =" + MyDialogStatsFile.tabHm(0).size)
            ret = res sortWith (_.count > _.count) take (topN)
            ret = ret :+ MyDialogStatsFile.tabHm(0).get("Total").get.setName("Total")
          }

        case "Sum" =>
          {

            //println("tri Sum res.length =" + res.length)
           // println("tri Sum MyDialogStatsFile.tabHm(0).length =" + MyDialogStatsFile.tabHm(0).size)
            ret = res sortWith (_.sum > _.sum) take (topN)
            ret = ret :+ MyDialogStatsFile.tabHm(0).get("Total").get.setName("Total")
          }

        case "Average" =>
          {

         //   println("tri Average res.length =" + res.length)
         //   println("tri Average MyDialogStatsFile.tabHm(0).length =" + MyDialogStatsFile.tabHm(0).size)
            ret = res sortWith (_.mean > _.mean) take (topN)
            ret = ret :+ MyDialogStatsFile.tabHm(0).get("Total").get.setName("Total")
          }

        case "Minimum" =>
          {

        //    println("tri Minimum res.length =" + res.length)
         //   println("tri Minimum MyDialogStatsFile.tabHm(0).length =" + MyDialogStatsFile.tabHm(0).size)
            ret = res sortWith (_.min < _.min) take (topN)
            ret = ret :+ MyDialogStatsFile.tabHm(0).get("Total").get.setName("Total")

          }
        case "Maximum" =>
          {

         //   println("tri Maximum res.length =" + res.length)
          //  println("tri Maximum MyDialogStatsFile.tabHm(0).length =" + MyDialogStatsFile.tabHm(0).size)
            ret = res sortWith (_.max > _.max) take (topN)
            ret = ret :+ MyDialogStatsFile.tabHm(0).get("Total").get.setName("Total")
          }
        case "Mediane" =>
          {

          //  println("tri Mediane res.length =" + res.length)
          //  println("tri Mediane MyDialogStatsFile.tabHm(0).length =" + MyDialogStatsFile.tabHm(0).size)
            ret = res sortWith (_.mediane > _.mediane) take (topN)
            ret = ret :+ MyDialogStatsFile.tabHm(0).get("Total").get.setName("Total")
          }

        //        case percentile =>
        //          {
        //
        //            println("tri Percentile res.length =" + res.length)
        //            println("tri Percentile MyDialogStatsFile.tabHm(0).length =" + MyDialogStatsFile.tabHm(0).size)
        //            ret = res sortWith (_.percentile > _.percentile) take (topN)
        //            ret = ret :+ MyDialogStatsFile.tabHm(0).get("Total").get.setName("Total")
        //          }

        case "StdDev" =>
          {

         //   println("tri StdDev res.length =" + res.length)
        //    println("tri StdDev MyDialogStatsFile.tabHm(0).length =" + MyDialogStatsFile.tabHm(0).size)
            ret = res sortWith (_.stdDev > _.stdDev) take (topN)
            ret = ret :+ MyDialogStatsFile.tabHm(0).get("Total").get.setName("Total")
          }

        case "Criteria" =>
          {

           // println("tri Criteria res.length =" + res.length)
          //  println("tri Criteria MyDialogStatsFile.tabHm(0).length =" + MyDialogStatsFile.tabHm(0).size)
            ret = res sortWith ((er1, er2) => (er1.name).compareToIgnoreCase(er2.name) < 0) take (topN)
            ret = ret :+ MyDialogStatsFile.tabHm(0).get("Total").get.setName("Total")
          }
        case _ =>
          if (col.startsWith("Percentile")) {

          //  println("tri Percentile res.length =" + res.length)
          //  println("tri Percentile MyDialogStatsFile.tabHm(0).length =" + MyDialogStatsFile.tabHm(0).size)
            ret = res sortWith (_.percentile > _.percentile) take (topN)
            ret = ret :+ MyDialogStatsFile.tabHm(0).get("Total").get.setName("Total")
          }
      }
      }

      ret
    }

}
object MyDialogResultStatsFiles {
  var percentile = ""
  class MyTableModel(columnNames: Array[String], rowCount: Int)
    extends DefaultTableModel(columnNames.asInstanceOf[Array[Object]], rowCount) {

    /*
         * JTable uses this method to determine the default renderer/
         * editor for each cell.  If we didn't implement this method,
         * then the last column would contain text ("true"/"false"),
         * rather than a check box.
         */
    var maxRow: Int = 0;
    override def getColumnClass(c: Int): java.lang.Class[_] = {

      if (this.rowCount > 0 && null != getValueAt(0, c)) {
        getValueAt(0, c).getClass()
      } else {
        AnyRef.getClass
      }

    }

    /*
         * Don't need to implement this method unless your table's
         * editable.
         */
    override def isCellEditable(row: Int, col: Int): Boolean = {
      //Note that the data/cell address is constant,
      //no matter where the cell appears onscreen.
      if (col < 0) {
        false;
      } else {
        true;
      }
    }
    /*
         * Don't need to implement this method unless your table's
         * data can change.
         */
    override def setValueAt(value: Object, row: Int, col: Int) {

      //      super.setValueAt(value: Object, row: Int, col: Int)
      //if (col == 4) {
      // super.setValueAt(value.asInstanceOf[Percent], row, col)
      // } else {
      super.setValueAt(value, row, col)
      // }
      //outer.table.setModel(this)
      // Normally, one should call fireTableCellUpdated() when
      // a value is changed.  However, doing so in this demo
      // causes a problem with TableSorter.  The tableChanged()
      // call on TableSorter that results from calling
      // fireTableCellUpdated() causes the indices to be regenerated
      // when they shouldn't be.  Ideally, TableSorter should be
      // given a more intelligent tableChanged() implementation,
      // and then the following line can be uncommented.
      fireTableCellUpdated(row, col);

    }

  }
}
class MyMouseAdapter(myDiagResult: MyDialogResultStatsFiles) extends MouseAdapter {
  override def mouseClicked(ev: MouseEvent) {
    val x = ev.getXOnScreen()
    val y = ev.getYOnScreen()
    if (ev.getSource.isInstanceOf[JTableHeader] && ev.getButton == MouseEvent.BUTTON1) {
     // println("clické sur tableHeader et button 1")
      val jth = ev.getSource.asInstanceOf[JTableHeader]
     // println("Titre Colonne=" + jth.getColumnModel.getColumn(jth.getColumnModel.getColumnIndexAtX(ev.getX)).getHeaderValue.toString)
      myDiagResult.fillTable(jth.getColumnModel.getColumn(jth.getColumnModel.getColumnIndexAtX(ev.getX)).getHeaderValue.toString)
     
       val selectedHeader = myDiagResult.table.convertColumnIndexToModel(myDiagResult.table
                    .columnAtPoint(ev.getPoint()));

      if ( myDiagResult.table.getColumnName(selectedHeader) != "Num Row")
      setBackground(myDiagResult.table,selectedHeader)
      
    } // System.out.println("Clicked at Row = " + row + ", Column = " + table.getColumnName(col));
    else if (ev.getButton() == MouseEvent.BUTTON3 && ev.getSource().isInstanceOf[JTableHeader]) {

      val dialog: Dialog = new Dialog(myDiagResult)

      dialog.location = new Point(x, y + 100)
      val gp: GridPanel = new GridPanel(8, 2)
      dialog.contents = gp
      var columnsName = Array[String]()
      for (
        libel <- List("Num Row", "Criteria", "Count",
          "Percent", "Sum", "Average", "Minimum", "Maximum", "Mediane",
          MyDialogResultStatsFiles.percentile, "StdDev")
      ) {
        var chbx = new CheckBox(libel)
        if (myDiagResult.table.getColumn(libel).getPreferredWidth() != 0) {
          chbx.selected = true
        } else {
          chbx.selected = false
        }
        gp.contents += (chbx)

      }

      dialog.reactions += {

        case WindowClosing(`dialog`) =>

          for (chbx <- gp.contents) {
            resizeColumn(chbx.asInstanceOf[CheckBox])
          }

      }
      dialog.modal = false
      dialog.pack()
      dialog.visible = true

    } else if (ev.getButton() == MouseEvent.BUTTON3 && ev.getSource().isInstanceOf[JTable]) {
     // println("cliqué on Jtable")
      val pop: JPopupMenu = new JPopupMenu()
      pop.setLocation(x, y)
      var csvExport = new JMenuItem("Export CSV")
      var pieRelatifChart = new JMenuItem("Export relatif PieChart ")
      var pieAbsoluteChart = new JMenuItem("Export Absolute PieChart ")
      csvExport.setName("csvExport")
      pieRelatifChart.setName("pieRelatifChart")
      pieAbsoluteChart.setName("pieAbsoluteChart")
      pop.add(csvExport)
      pop.add(pieRelatifChart)
      pop.add(pieAbsoluteChart)
      val listen: PopupListener = new PopupListener(myDiagResult, pop)
      csvExport.addMouseListener(listen)
      pieRelatifChart.addMouseListener(listen)
      pieAbsoluteChart.addMouseListener(listen)
      //  myDiagResult.table.addMouseListener(listen);
      pop.show(ev.getSource.asInstanceOf[JTable], ev.getX, ev.getY)
      pop.setLabel("Exports")

    }

  }
   def setBackground(table:JTable,selectedHeader:Int)
  {
    
    
       var colModel:TableColumnModel = table.getColumnModel();
     for (i <-0 until table.getColumnCount)
     {
       if(i == selectedHeader) {
        
        table.getColumnName(i) match {
          case "Criteria" =>
            colModel.getColumn(i).setCellRenderer(new  MyStatsStringCellRenderer(new Color(170, 242, 249),Color.RED))
          case "Count"  =>  colModel.getColumn(i).setCellRenderer(new  MyStatsIntCellRenderer(new Color(170, 242, 249),Color.BLACK))
          case "Num Row" => // On ne fait rien 
          case _ => colModel.getColumn(i).setCellRenderer(new  MyStatsDoubleCellRenderer(new Color(170, 242, 249),Color.BLACK))
        }
       }
       else
       {
         table.getColumnName(i) match {
          case "Criteria" =>
            colModel.getColumn(i).setCellRenderer(new  MyStatsStringCellRenderer(Color.WHITE,Color.RED))
          case "Count"  =>  colModel.getColumn(i).setCellRenderer(new  MyStatsIntCellRenderer(Color.WHITE,Color.BLACK))
          case "Num Row" => // On ne fait rien 
          case _ => colModel.getColumn(i).setCellRenderer(new  MyStatsDoubleCellRenderer(Color.WHITE,Color.BLACK))
        }
       }
      
     }
   
  }
  private def resizeColumn(chbx: CheckBox) {

    val libel = chbx.text

    if (chbx.selected) {

      // println("resizeColumn with " + MyTable.sizeColumnVisible.get(libel))
      // Ordre important quand on agrandit max d'abord
      myDiagResult.table.getColumn(libel).setMaxWidth(myDiagResult.sizeColumnVisible.get(libel).get._3)
      myDiagResult.table.getColumn(libel).setPreferredWidth(myDiagResult.sizeColumnVisible.get(libel).get._2)
      myDiagResult.table.getColumn(libel).setMinWidth(myDiagResult.sizeColumnVisible.get(libel).get._1)

    } else {
      // println("remise a 0")
      // Ordre important quand on diminue  min d'abord
      myDiagResult.table.getColumn(libel).setMinWidth(0)
      myDiagResult.table.getColumn(libel).setPreferredWidth(0)
      myDiagResult.table.getColumn(libel).setMaxWidth(0)

    }

    myDiagResult.table.repaint()
  }

}
class PopupListener(myDiagResult: MyDialogResultStatsFiles, pop: JPopupMenu) extends MouseAdapter {
  override def mousePressed(ev: MouseEvent) {
    //    if(ev.getSource.isInstanceOf[JTable] && ev.getButton() == MouseEvent.BUTTON3  )
    //    {
    //      println("popup Trigger")
    //    
    //    }
    //    else 
    if (ev.getSource.isInstanceOf[JMenuItem]) {
      ev.getSource.asInstanceOf[JMenuItem].getName match {
        case "csvExport" =>
          val prefix=MyDialogStatsFile.propsScaViewer.getProperty("scaviewer.prefixscenario")
          val baseDir = SearchDirFile.searchYoungestDir(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject , prefix.r)
          
          val fc = new FileChooser(new File(baseDir.getAbsolutePath+ File.separator + "logs"))
          // fc.peer.setDialogType(JFileChooser.CUSTOM_DIALOG);
          fc.fileSelectionMode = FileChooser.SelectionMode.FilesAndDirectories
          fc.controlButtonsAreShown = true
          fc.multiSelectionEnabled = false
          val ret = fc.showSaveDialog(null)
          var fileTosave: File = null
          if (ret == FileChooser.Result.Approve) {
            fileTosave = fc.selectedFile

          }
          if (null != fileTosave) {
            if(fileTosave.exists) fileTosave.delete
            val raf: RandomAccessFile = new RandomAccessFile(fileTosave, "rw")

            val titre: String = myDiagResult.columnsName.toList.foldLeft("") { _ + _ + ";" }
          
            raf.writeBytes(titre + "\n")
            val rowCount = myDiagResult.table.getRowCount
            val columnCount = myDiagResult.table.getColumnCount
            for (i <- 0 until rowCount) {
              var str = ""
              for (j <- 0 until columnCount) {
                if(null != myDiagResult.table.getValueAt(i, j))
                str += myDiagResult.table.getValueAt(i, j).toString + ";"
              }
              if(str.length>10)
              raf.writeBytes(str + "\n")
            }
            raf.close()
          }
         // println("csvExport basedir=" + baseDir)
        case "pieRelatifChart" => //println("pieRelatifChart")
        new  PieChartCSVRelative(myDiagResult,"Relative Pie Chart",true) 
        case "pieAbsoluteChart" => //println("pieAbsoluteChart")
         new  PieChartCSVAbsolute(myDiagResult,"Absolute Pie Chart",true) 
      }
    }

  }
}
