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
package com.jlp.scaviewer.ui.tableandchart

import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.JTable
import javax.swing.event.TableModelEvent
import javax.swing.event.RowSorterEvent
import javax.swing.event.RowSorterListener
import javax.swing.table.DefaultTableModel
import com.jlp.scaviewer.ui.MyDefaultCellRenderer
import java.awt.Font
import java.awt.Color
import java.awt.Component
import javax.swing.JLabel
import com.jlp.scaviewer.ui.ScaCharting
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.BasicStroke
import com.jlp.scaviewer.timeseries.MapDatasetToTs
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.TimeSeriesCollection
import java.text.NumberFormat
import java.util.Locale
import org.jfree.chart.labels.StandardXYToolTipGenerator
import java.awt.geom.Ellipse2D
import java.text.SimpleDateFormat
import javax.swing.JPopupMenu
import java.awt.Dimension
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JMenuItem
import javax.swing.JFrame
import org.jfree.chart.axis.DateAxis
import java.util.Calendar
import java.util.Date
import scala.collection.mutable.ArrayBuffer
import com.jlp.scaviewer.timeseries.StructTs
import javax.swing.table.AbstractTableModel
import javax.swing.table.JTableHeader
import scala.swing.Window
import scala.swing.Dialog
import java.awt.Point
import scala.swing.GridPanel
import scala.swing.CheckBox
import scala.swing.event.WindowClosing
import com.jlp.scaviewer.ui.MyTable
import scala.swing.event.WindowClosed
import scala.swing.event.SelectionChanged
import scala.swing.CheckBox
import scala.swing.event.MouseClicked
import javax.swing.JDialog
import java.awt.GridLayout
import javax.swing.JPanel
import javax.swing.JCheckBox
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent

class MouseAdapterJTableHeader(table: JTable) extends MouseAdapter {
  // override def mouseClicked(event: MouseEvent) {
  override def mouseClicked(event: MouseEvent) {

    // System.out.println("Clicked at Row = " + row + ", Column = " + table.getColumnName(col));
    val x = event.getXOnScreen()
    val y = event.getYOnScreen()
    if (event.getButton() == MouseEvent.BUTTON3 && event.getSource().isInstanceOf[JTableHeader]) {

      
      val dialog: Dialog = new Dialog()

      
      dialog.modal = true
      dialog.location = new Point(x, y - 100)
      val gp: GridPanel = new GridPanel(9, 2)
      dialog.contents = gp

      for (libel <- List("shown", "marked", "color", "scale", "translate", "source", "avg", "avgPond", "min", "max","maxMax", "stdv", "irslope", "countPts", "countVal", "sum", "sumTotal")) {
        var chbx = new CheckBox(libel)
        if (ScaCharting.myTable.table.getColumn(libel).getPreferredWidth() != 0) {
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
      dialog.pack()
      dialog.visible = true

    }
  }

  private def resizeColumn(chbx: CheckBox) {
   
    val libel = chbx.text

    if (chbx.selected) {

      // println("resizeColumn with " + MyTable.sizeColumnVisible.get(libel))
      // Ordre important quand on agrandit max d'abord
      ScaCharting.myTable.table.getColumn(libel).setMaxWidth(MyTable.sizeColumnVisible.get(libel).get._3)
      ScaCharting.myTable.table.getColumn(libel).setPreferredWidth(MyTable.sizeColumnVisible.get(libel).get._2)
      ScaCharting.myTable.table.getColumn(libel).setMinWidth(MyTable.sizeColumnVisible.get(libel).get._1)

    } else {
      // println("remise a 0")
      // Ordre important quand on diminue  min d'abord
      ScaCharting.myTable.table.getColumn(libel).setMinWidth(0)
      ScaCharting.myTable.table.getColumn(libel).setPreferredWidth(0)
      ScaCharting.myTable.table.getColumn(libel).setMaxWidth(0)

    }

  }

  def lightClear =
    {
      // nettoyage des Couleurs
      ScaCharting.colForLine.restoreAllColors()
      ScaCharting.colForRangeAxis.restoreAllColors()
      val plot = ScaCharting.chartPanel.getChart().getXYPlot()
      for (i <- 0 until plot.getRangeAxisCount()) {

        plot.setRangeAxis(i, null)
      }

      for (i <- 0 until plot.getDatasetCount()) {
        plot.setDataset(i, null)

      }
      plot.setDataset(null)
      for (i <- 0 until plot.getDomainAxisCount()) {
        plot.setDomainAxis(i, null)

      }

      ScaCharting.arrEnrichised = new ArrayBuffer[StructTs]()

      // nettoyage de la table

      var bool = true
      while (bool) {
        if (ScaCharting.myTable.tabModel.getRowCount() > 0) {
          // On supprime la premiere ligne
          ScaCharting.myTable.tabModel.removeRow(0)
        } else
          bool = false
      }
      // println("ScaCharting.myTable.table.getRowCount="+ScaCharting.myTable.tabModel.getRowCount())
      ScaCharting.myTable.table.setRowSorter(null)
      
      ScaCharting.arrEnrichised = new ArrayBuffer[StructTs]()

      // Fin nettoyage
    }

  def retrouverRow(keyTs: String): Int =
    {
      var ret = 0
      var bool = true
      while (bool) {

        for (j <- 0 until table.getRowCount()) {
          if (table.getValueAt(j, table.getColumn("name").getModelIndex()).toString == keyTs) {
            bool = false
            ret = j
          }
        }

      }
      ret
    }

  def retrouverCouleurDataSet(keyTs: String): Color =
    {

      val (icxDataset, idxTs) = retouverdatasetEtTs(keyTs)

      var renderer = ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(icxDataset).asInstanceOf[XYLineAndShapeRenderer]
      renderer.getSeriesPaint(icxDataset).asInstanceOf[Color]
    }

  def retouverdatasetEtTs(keyTs: String): (Int, Int) =
    {
      var icxDataset = -1
      var idxTs = 0

      var bool = true
      while (bool) {
        icxDataset += 1
        var dataset = ScaCharting.chartPanel.getChart.getXYPlot.getDataset(icxDataset)
        for (j <- 0 until dataset.getSeriesCount()) {
          if (dataset.getSeriesKey(j).toString == keyTs) {
            bool = false
            idxTs = j
          }
        }

      }

      (icxDataset, idxTs)
    }

}