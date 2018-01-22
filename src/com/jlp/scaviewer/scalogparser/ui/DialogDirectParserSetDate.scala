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
import scala.swing.Dialog
import scala.swing.GridBagPanel
import java.awt.Dimension
import java.awt.Insets
import scala.swing.Label
import java.awt.Font
import java.awt.GridBagConstraints
import java.awt.Point
import java.io.File
import java.io.FileFilter
import scala.collection._
import com.jlp.scaviewer.ui.SwingScaViewer
import com.jlp.scaviewer.commons.utils.CopyFile
import javax.swing.JDialog
import java.awt.event.ActionListener
import javax.swing.JPanel
import java.awt.GridBagLayout
import javax.swing.JLabel
import javax.swing.JTextField
import javax.swing.JButton
import java.awt.event.ActionEvent
import javax.swing.JOptionPane
import java.text.SimpleDateFormat
import com.jlp.scaviewer.scalogparser.DirectParserMain
import java.awt.Toolkit

class DialogDirectParser(prefixStep: String, props: java.util.Properties) extends JDialog {
  this.setModal(true)
  val dimScreen = Toolkit.getDefaultToolkit.getScreenSize()

  val dtf3 = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
  val dtf2 = new SimpleDateFormat("yyyyMMdd_HHmmss")
  val contentPane: JPanel = new JPanel()
  contentPane.setLayout(new GridBagLayout())
  val gbc = new GridBagConstraints()

  // Title Panel
  val jpTitle = new JPanel()
  jpTitle.setLayout(new GridBagLayout())
  val jlTitle = new JLabel(" Setting Start date and Step ")
  jlTitle.setFont(new Font("Arial", Font.BOLD, 20))
  val gbc1 = new GridBagConstraints()
  gbc1.anchor = GridBagConstraints.CENTER
  gbc1.fill = GridBagConstraints.HORIZONTAL
  jpTitle.add(jlTitle, gbc1)

  // Data Panel
  val jpData = new JPanel()
  jpData.setLayout(new GridBagLayout())
  val gbc2 = new GridBagConstraints()

  val jlDateFormat = new JLabel("Date Format")
  val jlPropsDateFormat = new JLabel("yyyyMMdd_HHmmss")
  val jlDate = new JLabel("Date")
  val jtfDate = new JTextField(50)
  val jlStepUnit = new JLabel("Step Unit")
  val jlPropsStepUnit = new JLabel(props.getProperty("fileIn.unitStep", "ms"))
  val jlStep = new JLabel("Step")
  val jtfStep = new JTextField(50)
  val jbOK = new JButton("OK")
  val jbCancel = new JButton("Cancel")

  jlDateFormat.setFont(new Font("Arial", Font.BOLD, 14))
  jlPropsDateFormat.setFont(new Font("Arial", Font.BOLD, 14))
  jlDate.setFont(new Font("Arial", Font.BOLD, 14))
  jtfDate.setFont(new Font("Arial", Font.BOLD, 14))
  jlStepUnit.setFont(new Font("Arial", Font.BOLD, 14))
  jlPropsStepUnit.setFont(new Font("Arial", Font.BOLD, 14))
  jlStep.setFont(new Font("Arial", Font.BOLD, 14))
  jtfStep.setFont(new Font("Arial", Font.BOLD, 14))
  jbOK.setFont(new Font("Arial", Font.BOLD, 14))
  jbCancel.setFont(new Font("Arial", Font.BOLD, 14))

  gbc2.insets = new Insets(10, 10, 10, 10)
  gbc2.weightx = 1.0
  gbc2.gridx = 0
  gbc2.gridy = 0
  jpData.add(jlDateFormat, gbc2)

  gbc2.gridx = 1
  jpData.add(jlPropsDateFormat, gbc2)

  gbc2.gridx = 0
  gbc2.gridy = 1
  jpData.add(jlDate, gbc2)

  gbc2.gridx = 1
  jpData.add(jtfDate, gbc2)

  gbc2.gridx = 0
  gbc2.gridy = 2
  jpData.add(jlStepUnit, gbc2)

  gbc2.gridx = 1
  jpData.add(jlPropsStepUnit, gbc2)

  gbc2.gridx = 0
  gbc2.gridy = 3
  jpData.add(jlStep, gbc2)

  gbc2.gridx = 1
  jpData.add(jtfStep, gbc2)

  gbc2.gridx = 0
  gbc2.gridy = 4
  jpData.add(jbOK, gbc2)

  gbc2.gridx = 1
  jpData.add(jbCancel, gbc2)

  gbc.gridx = 0
  gbc.gridy = 0
  gbc.fill = GridBagConstraints.BOTH
  contentPane.add(jpTitle, gbc)

  gbc.gridy = 1
  contentPane.add(jpData, gbc)

  jbOK.addActionListener(new ActionListener {
    def actionPerformed(aev: ActionEvent) {
      // Check date format
      val reg = """\d{8}_\d{6}""".r
      val ext = reg.findFirstIn(jtfDate.getText().trim)
      if (None == ext) {
        JOptionPane.showMessageDialog(null, "Incorrect date format, it must match : yyyyMMdd_HHmmss")
        return
      } else {
        // date correct
        var date1 = dtf2.parse(ext.get)

        props.put("fileIn.startDate", dtf3.format(date1))
      }
      // check step is a number if only  prefixStep.length>0
      if (prefixStep.trim().length() > 0) {
        val reg2 = """\d+$""".r
        val ext2 = reg2.findFirstIn(jtfStep.getText().trim)
        if (None == ext2) {
          JOptionPane.showMessageDialog(null, "Incorrect Step, it must be an integer")
          return

        } else {
          props.put("fileIn.stepWithinEnreg", prefixStep + jtfStep.getText().trim)
        }
      }
      DirectParserMain.cancel = false
      dispose()
    }

  })
  jbCancel.addActionListener(new ActionListener {
    def actionPerformed(aev: ActionEvent) {

      DirectParserMain.cancel = true
      dispose()
    }
  })
  this.setContentPane(contentPane)
  pack
  val myDim = this.getSize()
  this.setLocation((dimScreen.width - myDim.width) / 2, (dimScreen.height - myDim.height) / 2)
  setVisible(true)
}