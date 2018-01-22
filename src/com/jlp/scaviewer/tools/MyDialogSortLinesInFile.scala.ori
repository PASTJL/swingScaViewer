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

class MyDialogSortLinesInFile(modal: Boolean) extends JDialog with ActionListener {
  val gbp: JPanel = new JPanel()
  var gzipFile = false
  var BUFFER_SIZE = 10 * 1024
  var regexCorrect = ""
  var strSdf = ""
  val arrayReg: Array[(String, String)] = Array.ofDim(3)
  var sortedFile: File = null
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
  val jl1 = new JLabel("java Date Format (SimpleDateFormat)")
  val jtf = new JTextField
  jtf.setMaximumSize(new Dimension(300, 20))
  jtf.setPreferredSize(new Dimension(200, 20))
  jtf.setMinimumSize(new Dimension(150, 20))
  gbc.gridwidth = 1

  // gbc.fill = GridBagConstraints.NONE
  gbc.insets = insets1
  gbc.gridy = 3
  gbc.gridx = 0
  gbp.add(jl1, gbc)
  gbc.insets = insets2
  gbc.gridy = 4
  gbc.gridx = 0
  gbp.add(jtf, gbc)

  val jl2 = new JLabel("Index Rank of the date")
  jl2.setToolTipText("<html>If the date must be part of the sorting, put it rank in the index, <br/>a negative value means that the date is not in the index</html>")
  val jtf2 = new JTextField("-1")
  jtf2.setMaximumSize(new Dimension(300, 20))
  jtf2.setPreferredSize(new Dimension(200, 20))
  jtf2.setMinimumSize(new Dimension(150, 20))
  gbc.gridy = 3
  gbc.gridx = 1
  gbp.add(jl2, gbc)
  gbc.gridy = 4
  gbc.gridx = 1
  gbp.add(jtf2, gbc)

  val jl3 = new JLabel("Regex(s) 1 of the indexes")
  jl3.setToolTipText("<html>The first regex por the indexes to sort the line</html>")

  val jl4 = new JLabel("Regex(s) 2 of the indexes")
  jl4.setToolTipText("<html>The second regex por the indexes to sort the line</html>")

  gbc.gridy = 5
  gbc.gridx = 0
  gbp.add(jl3, gbc)
  gbc.gridy = 5
  gbc.gridx = 1
  gbp.add(jl4, gbc)
  gbc.insets = insets2
  val jtfIdx10 = new JTextField("")
  jtfIdx10.setMaximumSize(new Dimension(300, 20))
  jtfIdx10.setPreferredSize(new Dimension(200, 20))
  jtfIdx10.setMinimumSize(new Dimension(150, 20))

  val jtfIdx11 = new JTextField("")
  jtfIdx11.setMaximumSize(new Dimension(300, 20))
  jtfIdx11.setPreferredSize(new Dimension(200, 20))
  jtfIdx11.setMinimumSize(new Dimension(150, 20))

  val jtfIdx20 = new JTextField("")
  jtfIdx20.setMaximumSize(new Dimension(300, 20))
  jtfIdx20.setPreferredSize(new Dimension(200, 20))
  jtfIdx20.setMinimumSize(new Dimension(150, 20))

  val jtfIdx21 = new JTextField("")
  jtfIdx21.setMaximumSize(new Dimension(300, 20))
  jtfIdx21.setPreferredSize(new Dimension(200, 20))
  jtfIdx21.setMinimumSize(new Dimension(150, 20))

  val jtfIdx30 = new JTextField("")
  jtfIdx30.setMaximumSize(new Dimension(300, 20))
  jtfIdx30.setPreferredSize(new Dimension(200, 20))
  jtfIdx30.setMinimumSize(new Dimension(150, 20))

  val jtfIdx31 = new JTextField("")
  jtfIdx31.setMaximumSize(new Dimension(300, 20))
  jtfIdx31.setPreferredSize(new Dimension(200, 20))
  jtfIdx31.setMinimumSize(new Dimension(150, 20))

  jtfIdx10.setText(MyDialogSortLinesInFile.saveArrayReg(0) _1)
  jtfIdx11.setText(MyDialogSortLinesInFile.saveArrayReg(0) _2)
  jtfIdx20.setText(MyDialogSortLinesInFile.saveArrayReg(1) _1)
  jtfIdx21.setText(MyDialogSortLinesInFile.saveArrayReg(1) _2)
  jtfIdx30.setText(MyDialogSortLinesInFile.saveArrayReg(2) _1)
  jtfIdx31.setText(MyDialogSortLinesInFile.saveArrayReg(2) _2)

  gbc.gridy = 6
  gbc.gridx = 0
  gbp.add(jtfIdx10, gbc)
  gbc.gridy = 6
  gbc.gridx = 1
  gbp.add(jtfIdx11, gbc)

  gbc.gridy = 7
  gbc.gridx = 0
  gbp.add(jtfIdx20, gbc)
  gbc.gridy = 7
  gbc.gridx = 1
  gbp.add(jtfIdx21, gbc)

  gbc.gridy = 8
  gbc.gridx = 0
  gbp.add(jtfIdx30, gbc)
  gbc.gridy = 8
  gbc.gridx = 1
  gbp.add(jtfIdx31, gbc)

  gbc.insets = insets1
  val okB = new JButton("OK")
  gbc.gridy = 9
  gbc.gridx = 0
  gbc.gridwidth = 1
  okB.addActionListener(this)
  gbp.add(okB, gbc)
  okB.setEnabled(false)

  val clB = new JButton("Cancel")
  gbc.gridy = 9
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
            findDateFormat(firstLine)
          } finally {
            reader.close
          }
        }
      }
      if (jb == okB) {
        MyDialogSortLinesInFile.fileLog = fc.getSelectedFile().getAbsolutePath()
        if (null == jtf.getText || jtf.getText.length == 0) {
          JOptionPane.showMessageDialog(null, "You must select a file and  fill the regex for the date")
        } else {

          if (isCorrectRegex) {
            // remplir l'array des regexp
            def tup(tf1: JTextField, tf2: JTextField): (String, String) = {
              if ("" != tf1.getText) {
                if ("" != tf2.getText) {
                  (tf1.getText, tf2.getText)
                } else {
                  (tf1.getText, "")
                }
              } else {
                ("", "")
              }
            }

            this.arrayReg(0) = tup(this.jtfIdx10, this.jtfIdx11)

            this.arrayReg(1) = tup(this.jtfIdx20, this.jtfIdx21)
            this.arrayReg(2) = tup(this.jtfIdx30, this.jtfIdx31)
            MyDialogSortLinesInFile.saveArrayReg(0) = this.arrayReg(0)
            MyDialogSortLinesInFile.saveArrayReg(1) = this.arrayReg(1)
            MyDialogSortLinesInFile.saveArrayReg(2) = this.arrayReg(2)
            var deb=System.currentTimeMillis()
            sortFile
            var fin=System.currentTimeMillis()
            JOptionPane.showMessageDialog(null, "<html>Sorted in "+(fin-deb)+" ms <br/>The sorted file is : <br/><b>" +
              sortedFile.getAbsolutePath() + "</b></html>")
            dispose

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
  private def sortFile {

    // on efface un fichie trie existant
    sortedFile = new File(fc.getSelectedFile().getParent() + File.separator + "sorted_" + fc.getSelectedFile().getName())
    if (sortedFile.exists) sortedFile.delete
    val outputStream = initOutputStream(sortedFile)
    println("sortedFile=" + sortedFile)
    val inputStream = initFileInputStream(fc.getSelectedFile)
    println("fc.getSelectFile=" + fc.getSelectedFile.getAbsolutePath())
    if (null == inputStream) println("inputStream is null")
    new scala.io.BufferedSource(inputStream, BUFFER_SIZE).
      getLines.
      filter 
      { line =>     
        if(line.trim.length <4 || None == arrayReg(0)._1.r.findFirstIn(line)) false
        
        else true
      }.
      toList.
      map(line => new LineSortable(line, arrayReg, (regexCorrect, strSdf), jtf2.getText.toInt)).
      sortWith(_.lt(_)).foreach(xline =>
        outputStream.write((xline.line + "\n").getBytes()))
    outputStream.close
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
  private def findDateFormat(str: String) {
    var propsDates = new java.util.Properties()
    val fis = new FileInputStream(new File(System.getProperty("root") + File.separator + "config" + File.separator + "scaViewerDates.properties"))
    propsDates.load(fis)

    fis.close
    //  println("str=" + str)
    var moreLongParsing = 0
    var dateFormat = ""
    var locRegDate = ""
    val iter = propsDates.keySet.iterator
    while (iter.hasNext) {
      var key = iter.next.asInstanceOf[String]
      if (!key.contains("format.")) {
        var tmpReg = propsDates.getProperty(key)
        //   println("testing regex:" + tmpReg + "\nagainst :" + str + "\n")
        if (None != tmpReg.r.findFirstIn(str)) {
          var ext = tmpReg.r.findFirstIn(str).get
          // println("ext =" + ext)
          if (ext.length > moreLongParsing) {
            moreLongParsing = ext.length
            dateFormat = propsDates.getProperty("format." + key)
            locRegDate = tmpReg
          }
        }
      }
    }
    if (moreLongParsing > 0) {
      jtf.setText(dateFormat)
      regexCorrect = locRegDate
      strSdf = dateFormat
    } else {
      jtf.setText("")

    }
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
}
object MyDialogSortLinesInFile {

  var saveArrayReg: Array[(String, String)] = Array.ofDim(3)
  saveArrayReg(0) = ("", "")
  saveArrayReg(1) = ("", "")
  saveArrayReg(2) = ("", "")

  var fileLog = ""
}