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

import javax.swing.table.DefaultTableCellRenderer
import javax.swing.JTable
import java.awt.Component
import java.text.DecimalFormatSymbols
import java.util.Locale
import java.text.DecimalFormat
import java.awt.Font
import com.jlp.scaviewer.filestats.MyDialogResultStatsFiles._
import java.awt.Color


class MyStatsDoubleCellRenderer(bgColor:Color,fgColor:Color) extends DefaultTableCellRenderer {
  
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
   c.setFont(c.getFont.deriveFont(Font.BOLD, 12f));
    c.setForeground(fgColor)
  
    c.setBackground(bgColor)
    
   
    var str = MyDialogStatsFile.df.format(value.asInstanceOf[Double]).toString()
    if ( row > table.getModel.asInstanceOf[MyTableModel].maxRow && value.asInstanceOf[Double]== 0  ) str=""
    setText(str)
      if(str.length>0) setToolTipText(MyDialogStatsFile.df.format(table.getValueAt(row, column).asInstanceOf[Double]));
   else  setToolTipText("")
    c
    

  }
  
}