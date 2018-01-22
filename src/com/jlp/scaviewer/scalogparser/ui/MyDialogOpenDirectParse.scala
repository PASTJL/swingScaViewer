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
package com.jlp.scaviewer.scalogparser.ui

import java.io.File
import com.jlp.scaviewer.ui.SwingScaViewer
import com.jlp.scaviewer.commons.utils.SearchDirFile
import java.awt.Insets
import javax.swing.JDialog
import javax.swing.JPanel
import javax.swing.JFileChooser
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import java.awt.event.MouseListener
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.awt.event.MouseEvent
import java.awt.Label
import javax.swing.JLabel
import java.awt.Font
import javax.swing.JRadioButton
import javax.swing.JComboBox
import java.io.FileFilter
import java.awt.Toolkit
import java.awt.Point
import javax.swing.JButton
import java.awt.Dimension
import javax.swing.JOptionPane

// Oblige de passer par Java pour le FileChooser
class MyDialogOpenDirectParse(modal: Boolean) extends JDialog with ActionListener {

  this.setModal(modal)
  
  MyDialogOpenDirectParse.fileLog = ""
  this.setTitle("Open a new log file to chart directly")

  val gbp: JPanel = new JPanel()
  // Chercher le dossier scenario le plus recent
  var path = System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject
  
  
  var dir = SearchDirFile.searchYoungestDir(path, SwingScaViewer.pref.r).getAbsolutePath + File.separator + "logs"
  val fc: JFileChooser = new JFileChooser(new File(dir))
  fc.setDialogType(JFileChooser.CUSTOM_DIALOG)
  fc.addActionListener(this);

  fc.setControlButtonsAreShown(false)
  fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES)

  fc.setMultiSelectionEnabled(false)

  var mlst = fc.getListeners(classOf[MouseListener])

  gbp.setLayout(new GridBagLayout)

  val gbc = new GridBagConstraints
  var insets1 = new Insets(10, 10, 10, 10)
  gbc.insets = insets1
  gbc.gridx = 0
  gbc.gridy = 0
  gbc.fill=GridBagConstraints.BOTH
  gbc.weightx = 1.0
  gbc.weighty = 1.0
  gbc.gridwidth = 2
  gbp.add(fc, gbc)

  

 

  gbc.fill=GridBagConstraints.NONE

  gbc.weightx = 1.0
  val okB = new JButton("OK")
  gbc.gridy = 1
  gbc.gridx = 0
  gbc.gridwidth = 1
  okB.addActionListener(this)
  gbp.add(okB, gbc)

  val clB = new JButton("Cancel")
  gbc.gridy = 1
  gbc.gridx = 1
  gbc.gridwidth = 1
  clB.addActionListener(this)
  gbp.add(clB, gbc)

  this.setContentPane(gbp)
  this.pack
  
  var dimScreen = Toolkit.getDefaultToolkit().getScreenSize
  this.setLocation(new Point((dimScreen.getWidth - this.getWidth).toInt / 2, (dimScreen.getHeight - this.getHeight).toInt / 2))
  this.setVisible(true)

 
  def actionPerformed(arg0: ActionEvent) {

    
    if (arg0.getSource.isInstanceOf[JButton]) {
      var jb = arg0.getSource.asInstanceOf[JButton]
      if (jb == clB) {
      
        MyDialogOpenDirectParse.fileLog = ""
        dispose
      } else {
       
        if (null != fc.getSelectedFile) {
          MyDialogOpenDirectParse.fileLog = fc.getSelectedFile.getAbsolutePath
          dispose
        } else {
          JOptionPane.showMessageDialog(null, "You must choose , at least, a file in the file chooser")
        }
      }
    }
  }

}
object MyDialogOpenDirectParse {
  var fileLog=""
}