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
import javax.swing.table.TableCellEditor
import javax.swing.JSlider
import javax.swing.SwingConstants
import java.util.Vector
import javax.swing.event.CellEditorListener
import javax.swing.JWindow
import javax.swing.JButton
import java.awt.Color
import javax.swing.JPanel
import java.awt.GridLayout
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JTable
import java.awt.Component
import java.awt.Point
import java.awt.Rectangle
import java.util.EventObject
import javax.swing.event.ChangeEvent
import java.awt.FontMetrics
import java.awt.event.MouseAdapter
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import com.jlp.scaviewer.ui.tableandchart.Translate

class MyJSliderEditor extends JSlider with TableCellEditor with MouseListener {

  protected var listeners: Vector[CellEditorListener] = new Vector();

  protected var originalValue: Int = 0;
  private var rowChosen = -1
  protected var editing: Boolean = true;
  this.addMouseListener(this)
  private var percentOld: Int = 0
  def mouseClicked(e: MouseEvent) {}

  def mouseEntered(e: MouseEvent) {}

  def mouseExited(e: MouseEvent) {
    
    fireEditingCanceled()
    
  }

  def mousePressed(e: MouseEvent) {

    percentOld = this.getCellEditorValue().asInstanceOf[Percent].getPercent()
  }

  def mouseReleased(e: MouseEvent) {
  //  println("JSlider mouse released value=" + this.getCellEditorValue() + " at row=" + rowChosen)
    if (this.getCellEditorValue().asInstanceOf[Percent].getPercent() > 0) {
      Translate(percentOld, this.getCellEditorValue().asInstanceOf[Percent].getPercent(), rowChosen).execute
    }
    fireEditingStopped()
   
  }

  def getTableCellEditorComponent(table: JTable, value: Object, isSelected: Boolean,
    row: Int, column: Int): Component = {
    rowChosen = row
    if (value == null) {
      return this;
    }
    if (value.isInstanceOf[Percent]) {
      setValue(value.asInstanceOf[Percent].getPercent());
    } else {
      setValue(0);
    }
    table.setRowSelectionInterval(row, row);
    table.setColumnSelectionInterval(column, column);

    editing = true;

    return this;
  }

  def getCellEditorValue(): Object = {
    new Percent(getValue());
  }

  def isCellEditable(eo: EventObject): Boolean = {
    true;
  }

  def shouldSelectCell(eo: EventObject): Boolean = {
    true;
  }

  def stopCellEditing(): Boolean = {
    fireEditingStopped();
    editing = false;

    return true;
  }

  def addCellEditorListener(cel: CellEditorListener) = {
    listeners.addElement(cel);
  }

  def removeCellEditorListener(cel: CellEditorListener) = {
    listeners.removeElement(cel);
  }
  def cancelCellEditing() = {
    fireEditingCanceled();
    editing = false;

  }

  protected def fireEditingCanceled() = {
  setValue(percentOld);
     val ce:ChangeEvent = new ChangeEvent(this);
       for (i <- (0 until listeners.size()).reverse ){
   // for (int i = listeners.size() - 1; i >= 0; i--) {
        ( listeners.elementAt(i)).asInstanceOf[CellEditorListener].editingCanceled(ce);
        }
  }

  protected def fireEditingStopped() = {
    val ce: ChangeEvent = new ChangeEvent(this);

    for (i <- (0 until listeners.size()).reverse) {
      (listeners.elementAt(i)).asInstanceOf[CellEditorListener].editingStopped(ce);
    }
  }

}
object MyJSliderEditor {

  def apply(): MyJSliderEditor =
    {
      // var renderer=new JSlider(SwingConstants.HORIZONTAL) with MyJSliderCellRenderer

      var editor = new MyJSliderEditor
      editor.setOrientation(SwingConstants.HORIZONTAL)
      editor.setEnabled(true)

      editor
    }

}