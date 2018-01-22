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
import javax.swing.table.DefaultTableCellRenderer
import javax.swing._
import java.awt.Component
import java.awt.Font
import java.awt.Color
import javax.swing.JTextField._
import java.awt.Insets
import java.awt.FontMetrics
import com.jlp.scaviewer.ui.tableandchart.MouseAdapterJTable
import com.jlp.scaviewer.commons.utils.Couleurs
import org.jfree.chart.plot.XYPlot

class MyScaleCellRenderer extends DefaultTableCellRenderer {

  override def getTableCellRendererComponent(table: JTable,
    value: Object,
    isSelected: Boolean,
    hasFocus: Boolean,
    row: Int,
    column: Int): Component = {
    var c: Component =
      super.getTableCellRendererComponent(table, value,
        isSelected, hasFocus,
        row, column);

    val col = (retouverColorYAxis(ScaCharting.myTable.table.getValueAt(row, column).toString))._1
    setBackground(col)
    setForeground(Couleurs.invertColor(col, 100))
    setFont(c.getFont.deriveFont(Font.BOLD))
    //this.setBackground(col)
    //this.setForeground(Couleurs.invertColor(col, 150))
    setText(value.toString())

    this.setHorizontalAlignment(javax.swing.SwingConstants.CENTER)

    c
  }
  def retouverColorYAxis(name: String): (Color, Int) =
    {
      val plot: XYPlot = ScaCharting.chartPanel.getChart().getXYPlot
      var i = 0
      var bool = true
      val nbDatasets = plot.getDatasetCount()
      var ret: (Color, Int) = (null, -1)
      while (bool) {
        if (i < nbDatasets) {
          val axis = plot.getRangeAxis(i)
          if (axis.getLabel() == name) {
            bool = false
            ret = ((axis.getLabelPaint.asInstanceOf[Color], i))
          } else {
            i += 1
          }

        } else {
          bool = false
        }
      }

      ret
    }
}