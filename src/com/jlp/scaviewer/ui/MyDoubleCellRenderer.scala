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
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.util.Locale

class MyDoubleCellRenderer extends DefaultTableCellRenderer {

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

    var dfs = new DecimalFormatSymbols(Locale.ENGLISH)
    dfs.setExponentSeparator(" 10^")
    val df: DecimalFormat = new DecimalFormat(ScaCharting.tmpProps.getProperty("scaviewer.df"), dfs)

    if (value == null || value.toString().trim().equals("")||value.toString().contains("NaN")  ) {
      setText("")
      setToolTipText("No value for this interval");
    } else {
      //this.setForeground(Couleurs.invertColor(col, 150))
      // ne pas afficher 10^00
      var str = df.format(value.asInstanceOf[Double]).toString()
      // On va garder 7 chiffres et le . separateur
      if (str.endsWith(" 10^00")) {
        str = str.substring(0, str.indexOf(" 10^00"))
        val tabStr = str.split("""\.""")
        val lenMan = tabStr(0).length()
        if (lenMan > 7) {
          str = tabStr(0) + "." + tabStr(1).substring(0, 1)
        } else {
          val dec = tabStr(1).substring(0,  Math.min(tabStr(1).length,8 - lenMan))
          str = tabStr(0) + "." + dec
        }
      } else if (str.endsWith(" 10^03")) {
        str = ("""\d+\.\d+""".r.findFirstIn(str).get.toDouble * 1000).toString
        // On garde 5 chiffre maxi apres la virgule
        str = """\d+\.\d{1,5}""".r.findFirstIn(str).get
        val tabStr = str.split("""\.""")
        val lenMan = tabStr(0).length()
        if (lenMan > 7) {
          str = tabStr(0) + "." + tabStr(1).substring(0, 1)
        } else {
          val dec = tabStr(1).substring(0,  Math.min(tabStr(1).length,8 - lenMan))
          str = tabStr(0) + "." + dec
        }
      } else if (str.endsWith(" 10^-03")) {
        str = ("""\d+\.\d+""".r.findFirstIn(str).get.toDouble / 1000).toString
       
        val tabStr = str.split("""\.""")
        val lenMan = tabStr(0).length()
        if (lenMan > 7) {
          str = tabStr(0) + "." + tabStr(1).substring(0, 1)
        } else {
          val dec = tabStr(1).substring(0, Math.min(tabStr(1).length,8 - lenMan))
          str = tabStr(0) + "." + dec
        }
      }
      setText(str)

      this.setHorizontalAlignment(javax.swing.SwingConstants.LEFT)

      if (table.getValueAt(row, table.getColumn("marked").getModelIndex()) == true) {

        c.setFont(c.getFont.deriveFont(Font.BOLD, 13f));

        c.setBackground(Color.lightGray)
      } else {
        c.setFont(c.getFont.deriveFont(Font.PLAIN, 12f));

        c.setBackground(Color.white)
      }
      setToolTipText(df.format(table.getValueAt(row, column).asInstanceOf[Double]));
    }

    c
  }

}