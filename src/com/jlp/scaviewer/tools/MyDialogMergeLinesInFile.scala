package com.jlp.scaviewer.tools

import javax.swing.JDialog
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import scala.swing.GridBagPanel
import java.awt.Font
import scala.swing.MainFrame
import scala.swing.Dialog
import java.awt.Dimension
import java.awt.Insets
import scala.swing.Label
import java.awt.GridBagConstraints
import java.io.File
import com.jlp.scaviewer.ui.SwingScaViewer
import com.jlp.scaviewer.commons.utils.SearchDirFile
import javax.swing.JFileChooser
import javax.swing.JButton
import javax.swing.JDialog
import javax.swing.JPanel
import java.awt.GridBagLayout
import javax.swing.JLabel
import javax.swing.JTextArea
import javax.swing.JScrollPane
import javax.swing.ScrollPaneConstants
import javax.swing.JTextField
import java.io.BufferedReader
import java.io.FileInputStream
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
import java.io.FileNotFoundException
import java.io.IOException
import javax.swing.JOptionPane
import java.io.InputStream
import java.io.RandomAccessFile
import java.io.OutputStream
import java.io.FileOutputStream
import java.util.zip.GZIPOutputStream
import language.postfixOps
class MyDialogMergeLinesInFile(modal:Boolean) extends JDialog with ActionListener {
  
  val gbp: JPanel = new JPanel()
  var gzipFile = false
  var BUFFER_SIZE = 10 * 1024
  var regexCorrect = ""
  var strSdf = ""
  val arrayReg: Array[(String, String)] = Array.ofDim(3)
  var mergedLines: File = null
  gbp.setLayout(new GridBagLayout)
  val regDate = """\d{4}/\d\d/\d\d \d\d:\d\d:\d\d""".r
  val font1 = new Font("Arial", Font.BOLD, 14)
  val font2 = new Font("Arial", Font.BOLD, 16)
  this.setMinimumSize(new Dimension(700, 600))
  this.setPreferredSize(new Dimension(700, 700))
  this.setMaximumSize(new Dimension(700, 800))
  val gbc = new GridBagConstraints
  var insets1 = new Insets(5, 5, 5, 5)
  var insets2 = new Insets(0, 0, 0, 0)
  gbc.insets = insets1
  // titre du dialog

  gbc.gridx = 0
  gbc.gridy = 0
  gbc.weightx = 1.0
  gbc.gridwidth = 2
  gbc.gridwidth = GridBagConstraints.REMAINDER
  gbc.fill = GridBagConstraints.HORIZONTAL
  gbc.anchor = GridBagConstraints.CENTER
  this.setTitle("Sorting Lines in a File")

  // Installation et initialisation du file chhoser
  var dir = "."
  if ("" != SwingScaViewer.currentProject) {

    val path = System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject
    dir = SearchDirFile.searchYoungestDir(path, SwingScaViewer.pref.r).getAbsolutePath + File.separator + "logs"
  } else {
    dir = System.getProperty("workspace")
  }

  val fc: JFileChooser = new JFileChooser(new File(dir))
  fc.setDialogType(JFileChooser.CUSTOM_DIALOG)
  // fc.addActionListener(this)

  fc.setControlButtonsAreShown(false)
  fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES)

  fc.setMultiSelectionEnabled(false)

  gbp.add(fc, gbc)

  // Regex date et java format date
  val jbFill = new JButton("Extract of 200 first lines ")
  jbFill.setFont(new Font("Arial", Font.BOLD, 14))
  jbFill.addActionListener(this)
  gbc.gridy = 1
  gbc.insets = insets2
  gbc.fill = GridBagConstraints.NONE
  gbp.add(jbFill, gbc)

  val jta = new JTextArea()
  val jsp = new JScrollPane(jta)
  jsp.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED)
  jsp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)
  jta.setRows(5)
  jsp.setMaximumSize(new Dimension(800, 300))
  jsp.setPreferredSize(new Dimension(200, 200))
  jsp.setMinimumSize(new Dimension(100, 150))
  gbc.gridy = 2
  gbc.gridwidth = 2

  gbc.fill = GridBagConstraints.BOTH
  gbp.add(jsp, gbc)
  gbc.fill = GridBagConstraints.NONE
  val jl1 = new JLabel("Nb Lines to Merge")

  val jtf = new JTextField
  jtf.setMaximumSize(new Dimension(300, 20))
  jtf.setPreferredSize(new Dimension(200, 20))
  jtf.setMinimumSize(new Dimension(150, 20))
  gbc.gridwidth = 1
  gbc.gridy = 3
  gbc.gridx = 0
  gbp.add(jl1, gbc)
  gbc.gridx = 1
  gbp.add(jtf, gbc)

  gbc.insets = insets1
  val okB = new JButton("OK")
  gbc.gridy = 4
  gbc.gridx = 0
  gbc.gridwidth = 1
  okB.addActionListener(this)

  okB.setEnabled(false)
  gbp.add(okB, gbc)

  val clB = new JButton("Cancel")
  gbc.gridy = 4
  gbc.gridx = 1
  gbc.gridwidth = 1
  clB.addActionListener(this)
  gbp.add(clB, gbc)

  this.setContentPane(gbp)
  this.pack
  this.setVisible(true)
  def actionPerformed(arg0: ActionEvent) {
    if (arg0.getSource.isInstanceOf[JButton]) {
      var jb = arg0.getSource.asInstanceOf[JButton]
      if (jb == jbFill) {
        jta.setText("")
        // On teste si le fichier est gz d abord
        // sinon on formate sans zipper
        // si oui on dezippe on formate et on rezippe le fichier format�, on elimine les fichiers inutiles

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

          } finally {
            reader.close
          }
        }
      }
      if (jb == okB) {
       fileLog = fc.getSelectedFile().getAbsolutePath()
        if (null == jtf.getText || jtf.getText.length == 0) {
          JOptionPane.showMessageDialog(null, "You must select a file and a number of lines to merge")
        } else {

          var deb = System.currentTimeMillis()
          mergeLines
          var fin = System.currentTimeMillis()
          JOptionPane.showMessageDialog(null, "<html>Sorted in " + (fin - deb) + " ms <br/>The file with merged lines is : <br/><b>" +
            mergedLines.getAbsolutePath() + "</b></html>")
          dispose

        }

      }
      if (jb == clB) {

        dispose

      }
    }
  }
  private def mergeLines {
    mergedLines = new File(fc.getSelectedFile().getParent() + File.separator + "mergedLines_" + fc.getSelectedFile().getName())
    if (mergedLines.exists) mergedLines.delete
    val outputStream = initOutputStream(mergedLines)
    val inputStream = initFileInputStream(fc.getSelectedFile)
    val nbLines = jtf.getText().trim().toInt
    var count: Int = 1
    var accu: String = ""
    new scala.io.BufferedSource(inputStream, BUFFER_SIZE).
      getLines.foreach { line =>
       
        if (count % nbLines == 0) {
          accu =accu +" "+ line
          outputStream.write((accu + "\n").getBytes())
          accu = ""
        } else {
          
          accu =accu + line+" "
        }
         count += 1
      }
    outputStream.close
    inputStream.close
  }

  private def initOutputStream(file: File): OutputStream = {

    if (file.getName().endsWith(".gz")) {
      gzipFile = true
      new GZIPOutputStream(new FileOutputStream(file))

    } else {
      gzipFile = false
      new FileOutputStream(file)

    }

  }
  private def initFileInputStream(file: File): InputStream = {

    if (file.getName().endsWith(".gz")) {
      gzipFile = true
      new GZIPInputStream(new FileInputStream(file))

    } else {
      gzipFile = false
      new FileInputStream(file)

    }

  }
  private def initReader(file: File): BufferedReader =
    {

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

    }

  var fileLog = ""
}