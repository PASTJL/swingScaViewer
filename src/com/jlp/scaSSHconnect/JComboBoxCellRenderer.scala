package com.jlp.scaSSHconnect
import java.awt.Component;
import java.io.Serializable;

import javax.swing.JComboBox;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

class JComboBoxCellRenderer(items: Array[String]) extends JComboBox[String] with TableCellRenderer with Serializable {

  items.foreach(this.addItem(_))
  
  def getTableCellRendererComponent(table: JTable, value: Object,
    isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component = {
    if (isSelected) {
      setForeground(table.getSelectionForeground())
      super.setBackground(table.getSelectionBackground())
    } else {
      setForeground(table.getForeground())
      setBackground(table.getBackground())
    }

    // Select the current value
    setSelectedItem(value)

    this
  }

}