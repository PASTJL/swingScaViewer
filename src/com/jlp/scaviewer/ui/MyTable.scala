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

import scala.swing.ScrollPane
import javax.swing.JTable
import javax.swing.table.DefaultTableModel
import javax.swing.table.TableCellEditor
import java.awt.event.MouseListener
import com.jlp.scaviewer.ui.tableandchart.MouseAdapterJTable
import javax.swing.JTextField
import com.jlp.scaviewer.ui.tableandchart.MyScale
import javax.swing.table.TableColumn
import javax.swing.JSlider
import scala.collection.immutable.HashMap
import java.awt.Dimension
import com.jlp.scaviewer.ui.tableandchart.MouseAdapterJTableHeader
class MyTable extends ScrollPane {

  val titleColumns = new java.util.Vector[String]()

  var tabModel = new MyTable.MyTableModel(MyTable.columnNames, 0)
  var table: JTable = new JTable(tabModel)
  table.setAutoCreateRowSorter(false)
  //table.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS)
  //  table.setAutoResizeMode(JTable.AUTO_RESIZE_NEXT_COLUMN)
  table.setAutoResizeMode(JTable.AUTO_RESIZE_SUBSEQUENT_COLUMNS)
  //table.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN)
  //table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF)

  table.setDefaultEditor(classOf[java.awt.Color], com.jlp.scaviewer.ui.ColorEditor())

  table.setDefaultEditor(classOf[java.lang.String], new javax.swing.DefaultCellEditor(new JTextField()))
  table.setDefaultEditor(classOf[java.lang.Double], new javax.swing.DefaultCellEditor(new JTextField()))
  table.setDefaultEditor(classOf[java.lang.Long], new javax.swing.DefaultCellEditor(new JTextField()))

  table.setDefaultEditor(classOf[MyScale], new javax.swing.DefaultCellEditor(new JTextField()))
  table.setDefaultRenderer(classOf[java.lang.String], new MyDefaultCellRenderer())
  table.setDefaultRenderer(classOf[java.lang.Double], new MyDoubleCellRenderer())
  table.setDefaultRenderer(classOf[java.lang.Long], new MyDefaultCellRenderer())
  table.setDefaultRenderer(classOf[MyScale], new MyScaleCellRenderer())

  table.setDefaultEditor(classOf[Percent], MyJSliderEditor())
  table.setDefaultRenderer(classOf[Percent], com.jlp.scaviewer.ui.MyJSliderCellRenderer())
  table.setDefaultRenderer(classOf[java.awt.Color], com.jlp.scaviewer.ui.ColorRenderer(true))

  table.setRowSelectionAllowed(true)

  val width = math.min(50, ScaCharting.tmpProps.getProperty("scaviewer.unitWidth").toInt)
  // sizing the columns
  val nbCols = table.getColumnCount()
  for (i <- 0 until nbCols) {
    var col: TableColumn = table.getColumnModel().getColumn(i)
    var name = table.getColumnName(i)
    col.setMinWidth(MyTable.sizeColumnInit.get(name).get._1)
    col.setPreferredWidth(MyTable.sizeColumnInit.get(name).get._2)
    col.setMaxWidth(MyTable.sizeColumnInit.get(name).get._3)

  }
  //  
  //  for (i <- 0 until 3) {
  //    var col: TableColumn = table.getColumnModel().getColumn(i);
  //
  //    col.setPreferredWidth(width);
  //    col.setMaxWidth(width)
  //  }
  //  // column of scale
  //  var col: TableColumn = table.getColumnModel().getColumn(3);
  //  col.setPreferredWidth(width + 10)
  //  col.setMaxWidth(width + 30)
  //  //columns of source
  //  col = table.getColumnModel().getColumn(5);
  //  col.setPreferredWidth(2 * width + 30)
  //  col.setMaxWidth(3 * width + 20)
  //
  //  //# width of doubles
  //  for (i <- 7 until 12) {
  //    col = table.getColumnModel().getColumn(i);
  //    col.setPreferredWidth(width + 40)
  //    col.setMaxWidth(2 * width + 10)
  //  }
  //  // width irslope
  //  col = table.getColumnModel().getColumn(12);
  //  col.setPreferredWidth(3 * width)
  //  col.setMaxWidth(3 * width + 10)
  //  // width of JSlider
  //  col = table.getColumnModel().getColumn(4);
  //  col.setPreferredWidth(4 * width)
  //  col.setMaxWidth(4 * width)
  //  col.setMinWidth(0)
  val mouseAd = new MouseAdapterJTable(table)
  val mouseAd2 = new MouseAdapterJTableHeader(table)
  table.addMouseListener(mouseAd)
  table.getTableHeader().addMouseListener(mouseAd2)
  this.peer.setViewportView(table)
  //  table.setPreferredScrollableViewportSize(new Dimension(4000,400));

}
object MyTable {
  val columnNames: Array[String] = Array[String]("shown", "marked", "color", "scale",
    "translate", "source", "name", "avg", "avgPond", "min", "max", "maxMax", "stdv", "irslope", "countPts", "countVal", "sum", "sumTotal")
  val width = ScaCharting.tmpProps.getProperty("scaviewer.unitWidth").toInt

  var sizeColumnInit: HashMap[String, (Int, Int, Int)] = new HashMap
  sizeColumnInit += ("shown" -> (0, width, width))
  sizeColumnInit += ("marked" -> (0, width, width))
  sizeColumnInit += ("color" -> (0, width, width))
  sizeColumnInit += ("scale" -> (0, width + 10, width + 30))
  sizeColumnInit += ("translate" -> (0, 0, 0))
  //sizeColumnInit += ("source" -> (0, 2 * width + 30, 50 * width + 20))
  sizeColumnInit += ("source" -> (0, 0, 0))
  sizeColumnInit += ("name" -> (0, 4 * width + 30, 50 * width + 20))
  sizeColumnInit += ("avg" -> (0, width + 40, 2 * width + 10))
  sizeColumnInit += ("avgPond" -> (0, width + 40, 2 * width + 10))
  sizeColumnInit += ("min" -> (0, width + 40, 2 * width + 10))
  sizeColumnInit += ("max" -> (0, width + 40, 2 * width + 10))
  sizeColumnInit += ("maxMax" -> (0, width + 40, 2 * width + 10))
  sizeColumnInit += ("stdv" -> (0, width + 40, 2 * width + 10))
  sizeColumnInit += ("irslope" -> (0, 3 * width, 3 * width + 10))
  sizeColumnInit += ("countPts" -> (0, 0, 0))
  sizeColumnInit += ("countVal" -> (0, 0, 0))
  sizeColumnInit += ("sum" -> (0, 0, 0))
  sizeColumnInit += ("sumTotal" -> (0, 0, 0))

  var sizeColumnVisible: HashMap[String, (Int, Int, Int)] = new HashMap
  sizeColumnVisible += ("shown" -> (0, width, width))
  sizeColumnVisible += ("marked" -> (0, width, width))
  sizeColumnVisible += ("color" -> (0, width, width))
  sizeColumnVisible += ("scale" -> (0, width + 10, width + 30))
  sizeColumnVisible += ("translate" -> (4 * width, 4 * width, 4 * width))
  sizeColumnVisible += ("source" -> (0, 2 * width + 30, 50 * width + 20))
  sizeColumnVisible += ("name" -> (0, 4 * width + 30, 50 * width + 20))
  sizeColumnVisible += ("avg" -> (0, width + 40, 2 * width + 10))
  sizeColumnVisible += ("avgPond" -> (0, width + 40, 2 * width + 10))
  sizeColumnVisible += ("min" -> (0, width + 40, 2 * width + 10))
  sizeColumnVisible += ("max" -> (0, width + 40, 2 * width + 10))
  sizeColumnVisible += ("maxMax" -> (0, width + 40, 2 * width + 10))
  sizeColumnVisible += ("stdv" -> (0, width + 40, 2 * width + 10))
  sizeColumnVisible += ("irslope" -> (0, 3 * width, 3 * width + 10))
  sizeColumnVisible += ("countPts" -> (width, width, width + 10))
  sizeColumnVisible += ("countVal" -> (width, width, width + 10))
  sizeColumnVisible += ("sum" -> (width, width + 40, 2 * width + 10))
  sizeColumnVisible += ("sumTotal" -> (width, width + 40, 2 * width + 10))

  //...

  class MyTableModel(columnNames: Array[String], rowCount: Int)
    extends DefaultTableModel(columnNames.asInstanceOf[Array[Object]], rowCount) {

    /*
         * JTable uses this method to determine the default renderer/
         * editor for each cell.  If we didn't implement this method,
         * then the last column would contain text ("true"/"false"),
         * rather than a check box.
         */
    override def getColumnClass(c: Int): java.lang.Class[_] = {

      getValueAt(0, c).getClass()

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