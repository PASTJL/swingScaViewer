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
package com.jlp.scaviewer.ui.dialogs
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
import scala.swing.ComboBox
import scala.swing.Window
import scala.swing.TextField
import scala.swing.Button
import scala.swing.event.ActionEvent
import com.jlp.scaviewer.ui.SwingScaViewer
import scala.swing.MainFrame
import scala.swing.event.MouseClicked
import scala.swing.event.SelectionChanged
import scala.swing.event.ListSelectionChanged
import scala.swing.event.ValueChanged
import javax.swing.JOptionPane
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Calendar
import java.util.Properties
import java.io.FileOutputStream

class CSVLogsDialog(win: MainFrame) extends Dialog(win) {
  val gbp = new GridBagPanel
  val regDate = """\d{4}/\d\d/\d\d \d\d:\d\d:\d\d""".r
  modal = true
  this.preferredSize = new Dimension(500, 400)
  this.maximumSize = new Dimension(800, 500)
  val font1 = new Font("Arial", Font.BOLD, 14)
  val font2 = new Font("Arial", Font.BOLD, 16)
  var gbc = new gbp.Constraints()
  var insets1 = new Insets(10, 10, 10, 10)
  gbc.insets = insets1
  // titre du dialog
  this.title = "Project : " + SwingScaViewer.currentProject
  val ltitle = new Label("Creating new Test for project :" + SwingScaViewer.currentProject)
  ltitle.font = new java.awt.Font("Arial", Font.BOLD, 18)
  gbc.gridx = 0
  gbc.gridy = 0
  gbc.weightx = 1.0
  gbc.gridwidth = 2
  gbc.gridwidth = GridBagConstraints.REMAINDER
  gbc.fill = GridBagPanel.Fill.Horizontal
  gbc.anchor = GridBagPanel.Anchor.Center
  gbp.layout += ((ltitle -> gbc))

  //Liste des projets existants
  gbc.gridwidth = 1
  gbc.gridx = 0
  gbc.gridy = 1

  val pEx = new Label("List of existing Tests")
  pEx.font = font2

  gbp.layout += ((pEx -> gbc))

  gbc.gridx = 1
  gbc.gridy = 1
  val prefix = SwingScaViewer.tmpProps.getProperty("scaviewer.prefixscenario", "tir_")
  var listTest = List(new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject).listFiles(new FileFilter { def accept(file: File) = { file.isDirectory && file.getName.toLowerCase.startsWith(prefix) } }): _*) map { _.getName() }
  val cbTests = new ComboBox(listTest)
  cbTests.font = font1
  gbp.layout += ((cbTests -> gbc))

  // val cbPEx=new 

  // Saisie du nouveau projet et creation de l'arborescence
  gbc.gridx = 0
  gbc.gridy = 2
  insets1 = new Insets(10, 10, 20, 10)
  gbc.insets = insets1
  val pNew = new Label("New folder test")
  pNew.font = font2
  gbp.layout += ((pNew -> gbc))

  gbc.gridx = 1
  gbc.gridy = 2

  val tfPNew = new TextField(50)
  tfPNew.font = font1
  tfPNew.editable = true
  gbp.layout += ((tfPNew -> gbc))

  // Date et Debut du test
  insets1 = new Insets(0, 10, 0, 10)
  gbc.insets = insets1
  val lBeginDate = new Label("Begin Date")
  val lEndDate = new Label("End Date")
  lBeginDate.font = font2
  lEndDate.font = font2
  gbc.fill = GridBagPanel.Fill.None
  gbc.gridx = 0
  gbc.gridy = 3
  gbp.layout += ((lBeginDate -> gbc))
  gbc.fill = GridBagPanel.Fill.None
  gbc.gridx = 1
  gbc.gridy = 3
  gbp.layout += ((lEndDate -> gbc))

  val lFormatBeginDate = new Label("yyyy/MM/dd HH:mm:ss")
  val lFormatEndDate = new Label("yyyy/MM/dd HH:mm:ss")
  lFormatBeginDate.font = font2
  lFormatEndDate.font = font2
  gbc.fill = GridBagPanel.Fill.None
  gbc.gridx = 0
  gbc.gridy = 4
  gbp.layout += ((lFormatBeginDate -> gbc))
  gbc.fill = GridBagPanel.Fill.None
  gbc.gridx = 1
  gbc.gridy = 4
  gbp.layout += ((lFormatEndDate -> gbc))

  val df = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
  val cal = Calendar.getInstance
  val year = cal.get(Calendar.YEAR).toString
  val month = (cal.get(Calendar.MONTH) + 101).toString.substring(1)
  val day = (cal.get(Calendar.DAY_OF_MONTH) + 100).toString.substring(1)

  val tfBeginDate = new TextField(50)
  tfBeginDate.text = year + "/" + month + "/" + day + " 00:00:00"
  tfBeginDate.font = font1
  tfBeginDate.editable = true
  val tfEndDate = new TextField(50)
  tfEndDate.text = year + "/" + month + "/" + day + " 23:59:59"
  tfEndDate.font = font1
  tfEndDate.editable = true
  gbc.fill = GridBagPanel.Fill.None
  gbc.gridx = 0
  gbc.gridy = 5
  gbc.fill = GridBagPanel.Fill.Horizontal
  gbp.layout += ((tfBeginDate -> gbc))
  gbc.gridx = 1
  gbc.gridy = 5
  gbp.layout += ((tfEndDate -> gbc))

  // Button Cancel / OK
  val bOK = new Button("OK")
  bOK.font = font1
  val dimBut = new Dimension(140, 30)
  bOK.maximumSize = dimBut
  bOK.minimumSize = dimBut
  bOK.preferredSize = dimBut
  bOK.maximumSize = dimBut
  insets1 = new Insets(20, 10, 20, 10)
  gbc.insets = insets1
  gbc.fill = GridBagPanel.Fill.None
  gbc.gridx = 0
  gbc.gridy = 6
  gbp.layout += ((bOK -> gbc))

  val bCancel = new Button("Cancel")
  bCancel.font = font1
  bCancel.maximumSize = dimBut
  bCancel.minimumSize = dimBut
  bCancel.preferredSize = dimBut
  bCancel.maximumSize = dimBut
  gbc.weightx = 0
  gbc.gridx = 1
  gbc.gridy = 6
  gbp.layout += ((bCancel -> gbc))

  this.contents = gbp
  var screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
  location = new Point((screenSize.width - size.width) / 2, (screenSize.height - size.height) / 2)

  this.resizable = true

  bOK.reactions += {

    case ActionEvent(`bOK`) =>
      if (tfPNew.text.length() >= 4) {

        var testFinal = tfPNew.text
        var booldateFormat = true
        var message = ""
        if (None == regDate.findFirstIn(this.tfBeginDate.text)) {
          booldateFormat = false
          message = "\"Begin Date\" format is not correct "
        }
        if (None == regDate.findFirstIn(this.tfEndDate.text)) {
          booldateFormat = false
          if (message.length == 0)
            message = "\"End Date\" format is not correct "
          else
            message = " \"Begin Date\" and \"End Date\" formats are not correct "
        }
        if (!testFinal.toLowerCase().startsWith(SwingScaViewer.tmpProps.getProperty("scaviewer.prefixscenario"))) {
          testFinal = SwingScaViewer.tmpProps.getProperty("scaviewer.prefixscenario") + testFinal
        }
        if (listTest.contains(testFinal)) {
          JOptionPane.showMessageDialog(null, "This test folder already exists for project " + SwingScaViewer.currentProject)
          tfPNew.text = ""
        } else if (!booldateFormat) {
          JOptionPane.showMessageDialog(null, message)
        } else {

          val path = System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject + File.separator + testFinal
          new File(path).mkdir()
          new File(path + File.separator + "logs").mkdir()
          new File(path + File.separator + "logs" + File.separator + "config").mkdir()
          new File(path + File.separator + "logs" + File.separator + "config" + File.separator + "filestats").mkdir()
          new File(path + File.separator + "logs" + File.separator + "config" + File.separator + "scaparser").mkdir()
          new File(path + File.separator + "csv").mkdir()
          new File(path + File.separator + "reports").mkdir()
          val propsDate = new Properties()
          propsDate.put("beginTestDate", this.tfBeginDate.text)
          propsDate.put("endTestDate", this.tfEndDate.text)
         SwingScaViewer. propsDate.put("beginTestDate", this.tfBeginDate.text)
          SwingScaViewer. propsDate.put("endTestDate", this.tfEndDate.text)
          var fos = new FileOutputStream(new File(path + File.separator + "testDate.properties"))
          propsDate.store(fos, "Saved " + new Date().toString)
          fos.close

          dispose
        }

      } else {
        JOptionPane.showMessageDialog(null, "The length of the string must be >= 4")
        tfPNew.text = ""
      }

  }
  bCancel.reactions += {

    case ActionEvent(`bCancel`) =>

      dispose

  }
  visible = true
  pack

  repaint

}