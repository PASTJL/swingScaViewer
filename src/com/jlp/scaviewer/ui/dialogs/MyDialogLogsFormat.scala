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
import javax.swing.JTextArea
import javax.swing.JScrollPane
import javax.swing.ScrollPaneConstants
import java.io.RandomAccessFile
import javax.swing.JTextField
import java.util.zip.GZIPOutputStream
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.InputStreamReader
import java.io.FileNotFoundException
import java.io.IOException
import java.io.InputStream
import java.io.BufferedReader
import java.io.FileOutputStream
import java.io.BufferedOutputStream

// Oblige de passer par Java pour le FileChooser
class MyDialogLogsFormat(modal: Boolean) extends JDialog with ActionListener {
  var BUFFER_SIZE = 10 * 1024
  var gzipFile = false
  this.setModal(modal)
  MyDialogOpenLog.template = ""
  MyDialogOpenLog.fileLog = ""
  this.setTitle("Open a new log file")
  var regexCorrect = ""
  val gbp: JPanel = new JPanel()
  // Chercher le dossier scenario le plus recent
  var path = ""
  var dir = ""
  if (SwingScaViewer.currentProject != "") {
    path = System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject
    dir = SearchDirFile.searchYoungestDir(path, SwingScaViewer.pref.r).getAbsolutePath + File.separator + "logs"

  } else {
    path = System.getProperty("workspace")
    dir = path
  }

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
  gbc.weightx = 0.0
  gbc.weighty = 0.0
  gbc.gridwidth = 2
  gbp.add(fc, gbc)

  // Construction de la partie choix de template
  val jbFill = new JButton("Extract of 200 first lines ")
  jbFill.setFont(new Font("Arial", Font.BOLD, 14))
  jbFill.addActionListener(this)
  gbc.gridy = 1
  gbp.add(jbFill, gbc)

  val jta = new JTextArea()
  val jsp = new JScrollPane(jta)
  jsp.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED)
  jsp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)
  jta.setRows(10)
  jsp.setMaximumSize(new Dimension(800, 300))
  jsp.setPreferredSize(new Dimension(300, 200))
  jsp.setMinimumSize(new Dimension(200, 150))
  gbc.gridy = 2
  gbc.gridwidth = 2

  gbc.fill = GridBagConstraints.BOTH
  gbp.add(jsp, gbc)

  val jl1 = new JLabel("java Date Format (SimpleDateFormat)")
  val jtf = new JTextField
  jtf.setMaximumSize(new Dimension(300, 20))
  jtf.setPreferredSize(new Dimension(200, 20))
  jtf.setMinimumSize(new Dimension(150, 20))
  gbc.gridwidth = 1

  gbc.fill = GridBagConstraints.NONE

  gbc.gridy = 3
  gbc.gridx = 0
  gbp.add(jl1, gbc)
  gbc.gridx = 1
  gbp.add(jtf, gbc)
  gbc.weightx = 1.0
  val okB = new JButton("OK")
  gbc.gridy = 4
  gbc.gridx = 0
  gbc.gridwidth = 1
  okB.addActionListener(this)
  gbp.add(okB, gbc)
  okB.setEnabled(false)

  val clB = new JButton("Cancel")
  gbc.gridy = 4
  gbc.gridx = 1
  gbc.gridwidth = 1
  clB.addActionListener(this)
  gbp.add(clB, gbc)

  this.setContentPane(gbp)
  this.pack
  fillCbx()
  var dimScreen = Toolkit.getDefaultToolkit().getScreenSize
  this.setLocation(new Point((dimScreen.getWidth - this.getWidth).toInt / 2, (dimScreen.getHeight - this.getHeight).toInt / 2))
  this.setVisible(true)

  private def fillCbx() {

  }
  def actionPerformed(arg0: ActionEvent) {

    if (arg0.getSource.isInstanceOf[JRadioButton]) {

      fillCbx
    }
    if (arg0.getSource.isInstanceOf[JButton]) {
      var jb = arg0.getSource.asInstanceOf[JButton]
      if (jb == jbFill) {
        jta.setText("")
        // On teste si le fichier est gz d abord
        // sinon on formate sans zipper
        // si oui on dezippe on formate et on rezippe le fichier formaté, on elimine les fichiers inutiles

        //TODO
        if (null != fc.getSelectedFile && fc.getSelectedFile.isFile && SearchDirFile.isText(fc.getSelectedFile, true)) {
          okB.setEnabled(true)
          val reader = initReader(fc.getSelectedFile)
          var str = ""
          var firstLine = ""
          try {

            for (i <- 0 until 200) {

              if (i == 0) {
                firstLine = reader.readLine
                str += firstLine + "\n"
              } else { str += reader.readLine + "\n" }
            }
            jta.setText(str)
            jta.setCaretPosition(0)
            findDateFormat(firstLine)
          } finally {
            reader.close
          }
        }
      }
      if (jb == okB) {

        if (null == jtf.getText || jtf.getText.length == 0) {
          JOptionPane.showMessageDialog(null, "You must select a file and  fill the regex for the date")
        } else {
          if (isCorrectRegex) {
            dateFile

          } else {
            JOptionPane.showMessageDialog(null, "The regex is unknown for parsing a date")
          }

        }
      }
      if (jb == clB) {
        dispose
      }
    }
  }
  private def dateFile {
    val reader = initReader(fc.getSelectedFile)
    var currentDate = ""
    var fullname = fc.getSelectedFile.getAbsolutePath
    var fileOnly = fc.getSelectedFile.getName
    var path = fullname.subSequence(0, fullname.indexOf(fileOnly))
    var bos: BufferedOutputStream = null
    if (new File(path + "dated" + fileOnly).exists) new File(path + "dated" + fileOnly).delete
    if (gzipFile) {
      bos = new BufferedOutputStream(new GZIPOutputStream(new FileOutputStream(new File(path + "dated" + fileOnly))))
    } else {
      bos = new BufferedOutputStream(new FileOutputStream(new File(path + "dated" + fileOnly)))
    }
    var bool = true
    var line = ""
    while (bool) {
      var line = reader.readLine
      if (null == line) {
        bool = false

      } else {
        // tester si c"zst une ligne de date
        var ext = regexCorrect.r.findFirstIn(line)
        if (None != ext) {
          currentDate = ext.get
        } else {
          if (currentDate != "" && line.length > currentDate.length)
            //  ligne a horodater
            bos.write((currentDate + " " + line + "\n").getBytes)
        }

      }

    }
    bos.flush
    bos.close
    reader.close
    JOptionPane.showMessageDialog(null, "Dated File => " + path + "dated" + fileOnly)
    dispose
  }

  private def findDateFormat(str: String) {
    var propsDates = new java.util.Properties()
    val fis = new FileInputStream(new File(System.getProperty("root") + File.separator + "config" + File.separator + "scaViewerDates.properties"))
    propsDates.load(fis)
    fis.close
    var moreLongParsing = 0
    var dateFormat = ""
    val iter = propsDates.keySet.iterator
    while (iter.hasNext) {
      var key = iter.next.asInstanceOf[String]
      if (!key.contains("format.")) {
        var tmpReg = propsDates.getProperty(key)
        if (None != tmpReg.r.findFirstIn(str)) {
          var ext = tmpReg.r.findFirstIn(str).get
          if (ext.length > moreLongParsing) {
            moreLongParsing = ext.length
            dateFormat = propsDates.getProperty("format." + key)
          }
        }
      }
    }
    if (moreLongParsing > 0) {
      jtf.setText(dateFormat)
    } else {
      jtf.setText("yyyy/mm/dd HH:mm:ss")
    }
  }
  private def isCorrectRegex =
    {
      var reg1 = jtf.getText
      regexCorrect = ""
      var propsDates = new java.util.Properties()
      val fis = new FileInputStream(new File(System.getProperty("root") + File.separator + "config" + File.separator + "scaViewerDates.properties"))
      propsDates.load(fis)
      fis.close
      val iter = propsDates.keySet.iterator
      while (iter.hasNext) {
        var key = iter.next.asInstanceOf[String]
        if (key.contains("format.")) {
          var tmpReg = propsDates.getProperty(key)
        //  println("tmpReg=|" + tmpReg + "| ,reg1=|" + reg1 + "|")
          if (tmpReg == reg1) {
           // println("correct cle=" + key.substring(7))
            regexCorrect = propsDates.getProperty(key.substring(7))
          //  println("Ok found => regexCorrect=" + regexCorrect)
          }
        }

      }

      if (regexCorrect == "")
        false
      else true
    }
  private def initReader(file: File): BufferedReader =
    {

      try {
        if (file.getName().endsWith(".gz")) {
          gzipFile = true
          return new BufferedReader(new InputStreamReader(
            new GZIPInputStream(new FileInputStream(file))),
            BUFFER_SIZE);

        } else {
          gzipFile = false
          return new BufferedReader(new InputStreamReader(
            new FileInputStream(file)), BUFFER_SIZE);

        }
      } catch {
        case e: FileNotFoundException => e.printStackTrace()
        case e: IOException => e.printStackTrace()
      }
      return null;
      //JLP
    }
}
object MyDialogOpenLog {
  var template = ""
  var fileLog = ""
}