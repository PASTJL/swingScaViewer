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
import javax.swing.JTable
import java.awt.Color
import javax.swing.JLabel
import javax.swing.table.TableCellRenderer
import javax.swing.border.Border
import java.awt.Component
import javax.swing.BorderFactory
import javax.swing.event.CellEditorListener
import javax.swing.event.ChangeEvent
class ColorRenderer extends JLabel with TableCellRenderer {
                            
   var unselectedBorder:Border = null;
    var  selectedBorder:Border = null;
    var isBordered:Boolean = true;

//    def editingStopped(e:ChangeEvent){
//      println("editing stopped")
//    }
//    def editingCanceled(e:ChangeEvent){}
    def getTableCellRendererComponent(
                           table:JTable, color:Object,
                            isSelected:Boolean, hasFocus:Boolean,
                             row:Int,  column:Int):Component= {
        var newColor:Color = color.asInstanceOf[Color];
        setBackground(newColor);
        if (isBordered) {
            if (isSelected) {
                if (selectedBorder == null) {
                    selectedBorder = BorderFactory.createMatteBorder(2,5,2,5,
                                              table.getSelectionBackground());
                }
                setBorder(selectedBorder);
            } else {
                if (unselectedBorder == null) {
                    unselectedBorder = BorderFactory.createMatteBorder(2,5,2,5,
                                              table.getBackground());
                }
                setBorder(unselectedBorder);
            }
        }
      

        setToolTipText("RGB value: " + newColor.getRed() + ", "
                                     + newColor.getGreen() + ", "
                                     + newColor.getBlue());
       this
    }
  

}
object ColorRenderer
{
  def apply(isBordered:Boolean):ColorRenderer={
    val colorRenderer=new ColorRenderer()
    
    colorRenderer.isBordered=isBordered
    colorRenderer.setOpaque(true)//MUST do this for background to show up.
    colorRenderer
  }
  }