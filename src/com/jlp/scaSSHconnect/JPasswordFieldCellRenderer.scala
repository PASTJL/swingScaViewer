package com.jlp.scaSSHconnect

import java.awt.Component;
import java.io.Serializable;

import javax.swing.JPasswordField;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

class JPasswordFieldCellRenderer(password: String) extends JPasswordField with TableCellRenderer with Serializable {
  def getTableCellRendererComponent(table: JTable, value: Object,
    isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component = {
    if (isSelected) {
      setForeground(table.getSelectionForeground());
      super.setBackground(table.getSelectionBackground());
    } else {
      setForeground(table.getForeground());
      setBackground(table.getBackground());
    }

    // Select the current value
    setText(value.asInstanceOf[String]);
    this
  }

}