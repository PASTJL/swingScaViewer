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
import java.awt.Color
import javax.swing.AbstractCellEditor
import java.util.EventObject
import javax.swing.event.CellEditorListener
import javax.swing.table.TableCellEditor
import java.awt.Component
import javax.swing.JTable
import java.awt.event.ActionListener
import javax.swing.JButton
import javax.swing.JColorChooser
import javax.swing.JDialog
import java.awt.event.ActionEvent
import javax.swing.JLabel
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.BasicStroke
import com.jlp.scaviewer.commons.utils.Couleurs

class ColorEditor extends AbstractCellEditor with ActionListener with TableCellEditor {
  var currentColor: Color = null;
  var oldColor:Color=null;
  var button: JButton = null;
  var colorChooser: JColorChooser = null;
  var dialog: JDialog = null;

  override def isCellEditable(anEvent: EventObject): Boolean = { true }

  override def shouldSelectCell(anEvent: EventObject): Boolean = { true }

  override def stopCellEditing(): Boolean = { true }

  var rowSelected = -1
 

  val EDIT = "edit";
  override def actionPerformed(e: ActionEvent) {

    if (EDIT.equals(e.getActionCommand())) {

      //The user has clicked the cell, so
      //bring up the dialog.
      button.setBackground(oldColor);
      colorChooser.setColor(oldColor);
      
      dialog.setVisible(true);

      //Make the renderer reappear.
      fireEditingStopped();

    } else { //User pressed dialog's "OK" button.

      
      currentColor = colorChooser.getColor();
      // modifier la couleur dans le Plot
      ScaCharting.colForLine.restoreColor(oldColor)
      
        
      
      val tsKey=ScaCharting.myTable.table.getValueAt(rowSelected,ScaCharting.myTable.table.getColumn("name").getModelIndex()).asInstanceOf[String]
       println("row="+rowSelected +" ,tsKey="+tsKey)
         var (indexDataset, indexSeries) = retouverdatasetEtTs(tsKey)
         ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(indexDataset).asInstanceOf[XYLineAndShapeRenderer].setSeriesPaint(indexSeries,currentColor)
         if( ScaCharting.colForLine.colorExists(currentColor))
         {
            ScaCharting.colForLine.setBusyColor(currentColor)
         }
         
         
    }
  }

  //Implement the one CellEditor method that AbstractCellEditor doesn't.
  override def getCellEditorValue(): Object = {
    currentColor;
  }
  def getTableCellEditorComponent(table: JTable,
    value: Object,
    isSelected: Boolean,
    row: Int,
    column: Int): java.awt.Component = {
    rowSelected = row
  
   oldColor = value.asInstanceOf[Color]
    button
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
object ColorEditor {

  def apply(): ColorEditor = {

    val colorEditor = new ColorEditor()
    colorEditor.button = new JButton();
    colorEditor.button.setActionCommand(colorEditor.EDIT);
    colorEditor.button.addActionListener(colorEditor);
    colorEditor.button.setBorderPainted(false);
    colorEditor.colorChooser = new JColorChooser();
    colorEditor.dialog = JColorChooser.createDialog(colorEditor.button,
      "Pick a Color",
      true, //modal
      colorEditor.colorChooser,
      colorEditor, //OK button handler
      null); //no CANCEL button handle
    colorEditor
  }

}