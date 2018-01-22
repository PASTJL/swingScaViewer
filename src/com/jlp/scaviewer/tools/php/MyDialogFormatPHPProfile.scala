package com.jlp.scaviewer.tools.php

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
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Calendar
import scala.collection.mutable.HashMap
import java.util.TreeMap
import scala.collection.mutable.ListBuffer
import javax.swing.JTextPane
import java.awt.Toolkit
import java.text.DecimalFormatSymbols
import java.util.Locale
import java.text.DecimalFormat
class MyDialogFormatPHPProfile(modal: Boolean) extends JDialog with ActionListener {
 val dfs:DecimalFormatSymbols=new DecimalFormatSymbols(Locale.ENGLISH)
 // dfs.setGroupingSeparator('')
  dfs.setDecimalSeparator('.')
  val df:DecimalFormat=new DecimalFormat("#############0.###",dfs)

  val gbp: JPanel = new JPanel()
  var gzipFile = false
  var BUFFER_SIZE = 10 * 1024*1024
  var regexCorrect = ""
  var strSdf = ""
  val arrayReg: Array[(String, String)] = Array.ofDim(3)
  var mergedLines: File = null
  gbp.setLayout(new GridBagLayout)

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
  this.setTitle("Parsing XDebug Profile file")

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

  //  val jtf = new JTextField
  //  jtf.setMaximumSize(new Dimension(300, 20))
  //  jtf.setPreferredSize(new Dimension(200, 20))
  //  jtf.setMinimumSize(new Dimension(150, 20))
  //  gbc.gridwidth = 1
  //  gbc.gridy = 3
  //  gbc.gridx = 0
  //  gbp.add(jl1, gbc)
  //  gbc.gridx = 1
  //  gbp.add(jtf, gbc)

  gbc.insets = insets1
  val okB = new JButton("OK")
  gbc.gridy = 4
  gbc.gridx = 0
  gbc.gridwidth = 1
  okB.addActionListener(this)

  okB.setEnabled(true)
  gbp.add(okB, gbc)

  val clB = new JButton("Cancel")
  gbc.gridy = 4
  gbc.gridx = 1
  gbc.gridwidth = 1
  clB.addActionListener(this)
  gbp.add(clB, gbc)

  this.setContentPane(gbp)
   val tk = Toolkit.getDefaultToolkit()
  val screenDim = tk.getScreenSize()

  this.setLocation((screenDim.width - this.getSize().width) / 2, (screenDim.height - this.getSize().height) / 2)
  this.setModal(modal)
  this.pack
  this.setVisible(true)
 
  var fileLog:String=""
  
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

          val reader = initReader(fc.getSelectedFile)
          var str = ""
          var firstLine = ""
          try {
            var currentLine = ""
            var bool: Boolean = true
            for (i <- 0 until 200 if (bool)) {

              if (i == 0) {
                firstLine = reader.readLine
                str += firstLine + "\n"
              } else {
                currentLine = reader.readLine
                if (null != currentLine) {
                  str += currentLine + "\n"
                } else { bool = false }
              }
            }
            jta.setText(str)
            jta.setCaretPosition(0)

          } finally {
            reader.close
          }
        }
      }
      if (jb == okB) {
       

        class MyRunnable() extends Runnable {

          override def run {

            //              JOptionPane.showMessageDialog(MyDialogFormatPHPTrace.this, "<html>Formated in " + (fin - deb) + " ms <br/>The new formated file with merged lines is : <br/><b>" +
            //                mergedLines.getAbsolutePath() + "</b><br/>Please wait during creation call tree and tables. Close this dialog</html>")
            MyDialogFormatPHPProfile.jdia.setVisible(false)
            MyDialogFormatPHPProfile.jdia.setTitle("Please wait while building tree and tables.treatement of file : " + mergedLines.getName())
            MyDialogFormatPHPProfile.jdia.setVisible(true)
            MyDialogFormatPHPProfile.jdia.pack()
            MyDialogFormatPHPProfile.jdia.setLocation(300, 300)
          }

        }
        fileLog = fc.getSelectedFile().getAbsolutePath()
        var deb = System.currentTimeMillis()
        if (null == fileLog) {
          JOptionPane.showMessageDialog(null, "You must select a file ")
        } else {
          if (fc.getSelectedFile().getName().startsWith("newFormatProfile_")) {
            mergedLines = new File(fc.getSelectedFile().getParent() + File.separator + fc.getSelectedFile().getName())
          } else {
            mergedLines = new File(fc.getSelectedFile().getParent() + File.separator + "newFormatProfile_" + fc.getSelectedFile().getName())
          }
          if (!(mergedLines.exists()) || (mergedLines.exists() && new File(fileLog).lastModified() > mergedLines.lastModified())) {
            if (mergedLines.exists()) mergedLines.delete()

            CacheGrindToTree.main(Array(fileLog))
           
              var fin = System.currentTimeMillis()

           

            //          JOptionPane.showMessageDialog(null, "<html>Formated in " + (fin - deb) + " ms <br/>The new formated file with merged lines is : <br/><b>" +
            //            mergedLines.getAbsolutePath() + "</b></html>")

            dispose
            new Thread(new MyRunnable()).start

          } else if (mergedLines.exists() && new File(fileLog).lastModified() <= mergedLines.lastModified()) {
            dispose
            new Thread(new MyRunnable()).start

          }
          val args: Array[String] = Array(mergedLines.getAbsolutePath())
          class MyRunnableBis extends Runnable {

            override def run {

              PHPJPanelForTreeProfile.insideSwingScaViewer(args)
            }

          }
          new Thread(new MyRunnableBis()).start
        }

      }
      if (jb == clB) {

        dispose

      }
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
 

 
object MyDialogFormatPHPProfile {
  val jdia: JDialog = new JDialog(null.asInstanceOf[JDialog], "Please wait while building tree and tables")
  jdia.setModal(false)
  jdia.setMinimumSize(new Dimension(900, 20))
  jdia.setPreferredSize(new Dimension(900, 20))
  jdia.setMaximumSize(new Dimension(900, 20))



}
