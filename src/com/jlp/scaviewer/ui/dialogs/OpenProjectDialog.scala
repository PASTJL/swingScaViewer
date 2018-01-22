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
import scala.swing.MainFrame
import com.jlp.scaviewer.commons.utils.CopyFile
import com.jlp.scaviewer.commons.utils.SearchDirFile
import java.util.Properties
import java.io.FileInputStream
import java.text.SimpleDateFormat
import java.io.FileOutputStream
import java.util.Date
import java.util.Calendar
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.JDialog
import javax.swing.JPanel
import java.awt.GridBagLayout
import javax.swing.JLabel
import javax.swing.JComboBox
import javax.swing.JTextField
import javax.swing.JButton
import java.awt.event.ItemListener
import java.awt.event.ItemEvent
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import language.postfixOps

class OpenProjectDialog(win: MainFrame) extends JDialog {
  val gbp = new JPanel
  val regDate = """\d{4}/\d\d/\d\d \d\d:\d\d:\d\d""".r
  val font1 = new Font("Arial", Font.BOLD, 14)
  val font2 = new Font("Arial", Font.BOLD, 16)
  this.setModal(true)
  gbp.setLayout(new GridBagLayout)
  this.setMinimumSize(new Dimension(600, 300))
  this.setPreferredSize(new Dimension(700, 300))
  this.setMaximumSize(new Dimension(700, 500))
  var gbc = new GridBagConstraints()
  var insets1 = new Insets(10, 10, 10, 10)
  gbc.insets = insets1
  // titre du dialog
  val ltitle = new JLabel("Opening an existing project")
  ltitle.setFont(new java.awt.Font("Arial", Font.BOLD, 16))
  gbc.gridx = 0
  gbc.gridy = 0
  gbc.gridwidth = 2
  gbc.weightx = 1.0

  gbc.anchor = GridBagConstraints.CENTER
  gbp.add(ltitle, gbc)
  gbc.fill = GridBagConstraints.HORIZONTAL

  //Liste des projets existants
  gbc.gridwidth = 1
  gbc.gridx = 0
  gbc.gridy = 1
  val tfPNew = new JTextField(50)
  println("os.name=" + System.getProperty("os.name"))

  tfPNew.setEditable(false)
  val pEx = new JLabel("Existing Project")
  gbp.add(pEx, gbc)

  var listProjets = List(new File(System.getProperty("workspace")).listFiles(new FileFilter { def accept(file: File) = { file.isDirectory } }): _*) map { _.getName() }
  gbc.gridx = 1
  gbc.gridy = 1

  val cbxEx = new JComboBox(listProjets.toArray.asInstanceOf[Array[Object]])
  gbp.add(cbxEx, gbc)
  // val cbPEx=new 

  // Saisie du nouveau projet et creation de l'arborescence
  gbc.gridx = 0
  gbc.gridy = 2
  val pNew = new JLabel("Selected Project")
  gbp.add(pNew, gbc)

  gbc.gridx = 1
  gbc.gridy = 2

  gbp.add(tfPNew, gbc)

  // Date et Debut du test
  insets1 = new Insets(0, 10, 0, 10)
  gbc.insets = insets1
  val lBeginDate = new JLabel("Begin Date")
  val lEndDate = new JLabel("End Date")
  lBeginDate.setFont(font2)
  lEndDate.setFont(font2)
  gbc.fill = GridBagConstraints.NONE
  gbc.gridx = 0
  gbc.gridy = 3
  gbp.add(lBeginDate, gbc)
  gbc.fill = GridBagConstraints.NONE
  gbc.gridx = 1
  gbc.gridy = 3
  gbp.add(lEndDate, gbc)

  val lFormatBeginDate = new JLabel("yyyy/MM/dd HH:mm:ss")
  val lFormatEndDate = new JLabel("yyyy/MM/dd HH:mm:ss")
  lFormatBeginDate.setFont(font2)
  lFormatEndDate.setFont(font2)
  gbc.fill = GridBagConstraints.NONE
  gbc.gridx = 0
  gbc.gridy = 4
  gbp.add(lFormatBeginDate, gbc)
  gbc.fill = GridBagConstraints.NONE
  gbc.gridx = 1
  gbc.gridy = 4
  gbp.add(lFormatEndDate, gbc)

  val df = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
  val cal = Calendar.getInstance
  //  

  val tfBeginDate = new JTextField(50)

  tfBeginDate.setFont(font1)
  tfBeginDate.setEditable(true)
  val tfEndDate = new JTextField(50)

  tfEndDate.setFont(font1)
  tfEndDate.setEditable(true)
  gbc.fill = GridBagConstraints.NONE
  gbc.gridx = 0
  gbc.gridy = 5
  gbc.fill = GridBagConstraints.HORIZONTAL
  gbp.add(tfBeginDate, gbc)
  gbc.gridx = 1
  gbc.gridy = 5
  gbp.add(tfEndDate, gbc)

  // Button Cancel / OK
  val bOK = new JButton("OK")
  val dimBut = new Dimension(100, 30)
  bOK.setMaximumSize(dimBut)
  bOK.setMinimumSize(dimBut)
  bOK.setPreferredSize(dimBut)
  bOK.setMaximumSize(dimBut)
  gbc.fill = GridBagConstraints.NONE
  gbc.gridx = 0
  gbc.gridy = 6
  gbp.add(bOK, gbc)
  insets1 = new Insets(10, 10, 10, 10)
  val bCancel = new JButton("Cancel")
  bCancel.setMaximumSize(dimBut)
  bCancel.setMinimumSize(dimBut)
  bCancel.setPreferredSize(dimBut)
  bCancel.setMaximumSize(dimBut)
  gbc.weightx = 0
  gbc.gridx = 1
  gbc.gridy = 6
  gbp.add(bCancel, gbc)

  this.setContentPane(gbp)
  var screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
  this.setLocation(new Point((screenSize.width - this.getSize().width) / 2, (screenSize.height - this.getSize().height) / 2))

  this.setResizable(true)
  if (listProjets.size < 1) {
    listProjets = "Choose a project" :: listProjets
  } else {
    tfPNew.setEditable(true)
    tfPNew.setText((listProjets.head) toString)
    tfPNew.setEditable(false)
    // On remplit les champs avec ce projet

    tfPNew.setEditable(true)

    // recherche du dernier repertoire de test :
    var dir = SearchDirFile.searchYoungestDir(System.getProperty("workspace") + File.separator + tfPNew.getText, ("""^""" + SwingScaViewer.tmpProps.getProperty("scaviewer.prefixscenario")).r)

    println("dir=" + dir)
    if (null != dir) {
      if (!new File(dir + File.separator + "testDate.properties").exists) {

        tfBeginDate.setText("1970/01/01 00:00:00")
        tfEndDate.setText("2050/01/01 00:00:00")
        val propsDate = new Properties()
        propsDate.put("beginTestDate", tfBeginDate.getText)
        propsDate.put("endTestDate", tfEndDate.getText)
        var fos = new FileOutputStream(new File(dir + File.separator + "testDate.properties"))
        propsDate.store(fos, "Saved " + new Date().toString)
        fos.close

      } else {
        val propsDate = new Properties()
        propsDate.load(new FileInputStream(new File(dir + File.separator + "testDate.properties")))
        val sdfDateTest = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
        tfBeginDate.setText(propsDate.getProperty("beginTestDate"))
        tfEndDate.setText(propsDate.getProperty("endTestDate"))

      }
    }

    tfPNew.setEditable(false)

  }

  this.cbxEx.addItemListener(new ItemListener() {

    override def itemStateChanged(e: ItemEvent) {

      if (cbxEx.getSelectedItem().toString != "Choose a project") {
        tfPNew.setEditable(true)
        tfPNew.setText(cbxEx.getSelectedItem().toString)
        tfPNew.setEditable(false)
        // recherche du dernier repertoire de test :
        var dir = SearchDirFile.searchYoungestDir(System.getProperty("workspace") + File.separator + tfPNew.getText, ("""^""" + SwingScaViewer.tmpProps.getProperty("scaviewer.prefixscenario")).r)

        if (null != dir) {
          if (!new File(dir + File.separator + "testDate.properties").exists) {

            tfBeginDate.setText("1970/01/01 00:00:00")
            tfEndDate.setText("2050/01/01 00:00:00")
            val propsDate = new Properties()
            propsDate.put("beginTestDate", tfBeginDate.getText)
            propsDate.put("endTestDate", tfEndDate.getText)
            var fos = new FileOutputStream(new File(dir + File.separator + "testDate.properties"))
            propsDate.store(fos, "Saved " + new Date().toString)
            fos.close

          } else {
            val propsDate = new Properties()
            propsDate.load(new FileInputStream(new File(dir + File.separator + "testDate.properties")))
            val sdfDateTest = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
            tfBeginDate.setText(propsDate.getProperty("beginTestDate"))
            tfEndDate.setText(propsDate.getProperty("endTestDate"))

          }
        }

        tfPNew.setEditable(false)

      }
    }

  })

  this.bOK.addActionListener(new ActionListener() {
    override def actionPerformed(e: ActionEvent) {
      if (tfPNew.getText.length() > 0) {
        SwingScaViewer.currentProject = tfPNew.getText
        win.title = SwingScaViewer.initialTitle

        win.title += " current Project = " + tfPNew.getText
        if (!new File(System.getProperty("workspace") + File.separator +
          SwingScaViewer.currentProject + File.separator + "myCommands").exists()) {
          new File(System.getProperty("workspace") + File.separator +
            SwingScaViewer.currentProject + File.separator + "myCommands").mkdir
        }
        if (!new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject +
          File.separator + "myCommands" + File.separator + "Windows").exists) {
          new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject +
            File.separator + "myCommands" + File.separator + "Windows").mkdir
        }
        if (!new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject +
          File.separator + "myCommands" + File.separator + "unix").exists) {
          new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject +
            File.separator + "myCommands" + File.separator + "unix").mkdir
        }

        // Vidage locale et general myCommands et recuperation de l os
        var dirLocMyCommands = new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject +
          File.separator + "myCommands")

        dirLocMyCommands.listFiles filter (_ isFile) filter (_.getAbsolutePath().endsWith(".properties")) foreach (_.delete())

        var dirGenMyCommands = new File(System.getProperty("root") + File.separator +
          File.separator + "myCommands")

        dirGenMyCommands.listFiles filter (_ isFile) filter (_.getAbsolutePath().endsWith(".properties")) foreach (_.delete())

        // copy Os dependantes
        System.getProperty("os.name").toLowerCase().contains("window") match {
          case true =>
            {
              val dirLocWindows = new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject +
                File.separator + "myCommands" + File.separator + "Windows")
              dirLocWindows.listFiles filter (_ isFile) filter (_.getAbsolutePath().endsWith(".properties")) foreach { f =>
                CopyFile.copy(f, new File(dirLocMyCommands + File.separator + f.getName))
              }
              val dirGenWindows = new File(dirGenMyCommands + File.separator + "Windows")
              dirGenWindows.listFiles filter (_ isFile) filter (_.getAbsolutePath().endsWith(".properties")) foreach { f =>
                CopyFile.copy(f, new File(dirGenMyCommands + File.separator + f.getName))
              }
            }
          case _ =>
            {

              val dirLocUnix = new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject +
                File.separator + "myCommands" + File.separator + "unix")
              dirLocUnix.listFiles filter (_ isFile) filter (_.getAbsolutePath().endsWith(".properties")) foreach { f =>
                CopyFile.copy(f, new File(dirLocMyCommands + File.separator + f.getName))
              }
              val dirGenUnix = new File(dirGenMyCommands + File.separator + "unix")
              dirGenUnix.listFiles filter (_ isFile) filter (_.getAbsolutePath().endsWith(".properties")) foreach { f =>
                CopyFile.copy(f, new File(dirGenMyCommands + File.separator + f.getName))
              }

            }
        }

        var dir = SearchDirFile.searchYoungestDir(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject, ("""^""" + SwingScaViewer.tmpProps.getProperty("scaviewer.prefixscenario")).r)

        if (null != dir) {

          val propsDate = new Properties()
          propsDate.put("beginTestDate", tfBeginDate.getText)
          propsDate.put("endTestDate", tfEndDate.getText)
          SwingScaViewer.propsDate.put("beginTestDate", tfBeginDate.getText);
           SwingScaViewer.propsDate.put("endTestDate", tfEndDate.getText)
          var fos = new FileOutputStream(new File(dir + File.separator + "testDate.properties"))
          propsDate.store(fos, "Saved " + new Date().toString)
          fos.close

        }
        dispose
      }
    }
  })

  bCancel.addActionListener(new ActionListener() {
    override def actionPerformed(e: ActionEvent) {
      dispose
    }

  })

  this.setVisible(true)
  pack

  this.repaint()

}