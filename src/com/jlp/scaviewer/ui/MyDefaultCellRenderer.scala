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

class MyDefaultCellRenderer extends DefaultTableCellRenderer {

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

    // Only for specific cell

    if (table.getColumnName(column) == "name" || table.getColumnName(column) == "source") {

      var availableWidth: Int = table.getColumnModel().getColumn(column).getWidth();
      availableWidth = (availableWidth - table.getIntercellSpacing().getWidth()).asInstanceOf[Int];
      val borderInsets: Insets = getBorder().getBorderInsets(this);
      availableWidth -= borderInsets.left + borderInsets.right;
      var cellText: String = value.asInstanceOf[String];
      if (table.getValueAt(row, table.getColumn("marked").getModelIndex()) == true) {

        c.setFont(c.getFont.deriveFont(Font.BOLD, 13f));

        c.setBackground(Color.lightGray)
      } else {
        c.setFont(c.getFont.deriveFont(Font.PLAIN, 12f));

        c.setBackground(Color.white)
      }
      var fm: FontMetrics = getFontMetrics(getFont());
      // cas du shortName
      if (ScaCharting.rbShortName.selected && table.getColumnName(column) == "name") {
       // println("entering CellText="+cellText)
        if (cellText.contains(SwingScaViewer.sepName)) {
          val pref = cellText.split(SwingScaViewer.sepName)(0)

          val suff = cellText.split(SwingScaViewer.sepName)(1)
          var prefToAdd = "";
          if (pref.contains("_")) {
            val reg = """_[^_]+$""".r
            val ext1 = reg.findFirstIn(pref)
            if (None != ext1) {
              prefToAdd = ext1.get.substring(1)

            }

          }
         if (prefToAdd.trim().length() > 0 && !prefToAdd.equals("global") ) {
            cellText = prefToAdd +"_" + suff
          } else {
            cellText = suff

          }
        }
      } else if (ScaCharting.rbShortName.selected && table.getColumnName(column) == "source") {

        val reg = """(\\|/)[^/^\\]+$""".r
        val ext1 = reg.findFirstIn(cellText)

        if (None != ext1) {
          cellText = ext1.get

        }

      }

      if (fm.stringWidth(cellText) > availableWidth) {
        this.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT)
        val dots = "...";
        var textWidth = fm.stringWidth(dots);
        var nChars = cellText.length() - 1;
        var bool = true
        while (bool) {
          //for (; nChars > 0; nChars--) {

          textWidth += fm.charWidth(cellText.charAt(nChars));

          if (textWidth > availableWidth || nChars == 0) {
            bool = false
          }

          setText(dots + cellText.substring(nChars + 1));
          nChars -= 1
        }

      } else {
        this.setHorizontalAlignment(javax.swing.SwingConstants.LEFT)
        setText(cellText)
      }

    } //   
    else {

      if (table.getValueAt(row, table.getColumn("marked").getModelIndex()) == true) {

        c.setFont(c.getFont.deriveFont(Font.BOLD, 13f));

        c.setBackground(Color.lightGray)
      } else {
        c.setFont(c.getFont.deriveFont(Font.PLAIN, 12f));

        c.setBackground(Color.white)
      }

      this.setHorizontalAlignment(javax.swing.SwingConstants.LEFT)
      var str = value.toString()

      //      // supprimer les exposants nuls
      if (str.contains(" 10^00")) {

        str = str.replaceAll(" 10\\^00", "")
      }
      setText(str)
    }

    setToolTipText(table.getValueAt(row, column).toString);

    return c;
  }

}