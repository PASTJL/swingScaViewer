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
import javax.swing.table.TableCellRenderer

class  MyJSliderCellRenderer extends JSlider with TableCellRenderer {
  

  override def getTableCellRendererComponent(table: JTable,
    value: Object,
    isSelected: Boolean,
    hasFocus: Boolean,
    row: Int,
    column: Int): Component = {
   if (value == null) {
      this
    }
   else
    if (value.isInstanceOf[Percent]) {
      setValue(value.asInstanceOf[Percent].getPercent);
       setToolTipText(getValue.toString)
      // println("redifining new value JSliderRenderer value ="+value.asInstanceOf[Percent].getPercent)
       this
    } else {
      
      setValue(0);
       setToolTipText(getValue.toString)
      this
    }
  
 
   
  }

}
object MyJSliderCellRenderer {
  
  def apply():MyJSliderCellRenderer=
  {
   // var renderer=new JSlider(SwingConstants.HORIZONTAL) with MyJSliderCellRenderer
    
    var renderer=new  MyJSliderCellRenderer
    renderer.setOrientation(SwingConstants.HORIZONTAL)
    renderer.setEnabled(true)
    
    renderer
  }
  
}