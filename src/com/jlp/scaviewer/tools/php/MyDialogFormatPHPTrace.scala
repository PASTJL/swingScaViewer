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
import java.util.ArrayList
import scala.collection.JavaConverters._

class MyDialogFormatPHPTrace(modal: Boolean) extends JDialog with ActionListener {
  val dfs: DecimalFormatSymbols = new DecimalFormatSymbols(Locale.ENGLISH)
  // dfs.setGroupingSeparator('')
  dfs.setDecimalSeparator('.')
  val df: DecimalFormat = new DecimalFormat("#############0.###", dfs)

  val gbp: JPanel = new JPanel()
  var gzipFile = false
  var BUFFER_SIZE = 20 * 1024 * 1024
  var regexCorrect = ""
  var strSdf = ""
  val arrayReg: Array[(String, String)] = Array.ofDim(3)
  var mergedLines: File = null
  var mergedLinesAgg: File = null
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
  this.setTitle("Parsing XDebug trace file with format 1")

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

  this.setModal(modal)
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
        StructRead.clean()

        class MyRunnable() extends Runnable {

          override def run {

            //              JOptionPane.showMessageDialog(MyDialogFormatPHPTrace.this, "<html>Formated in " + (fin - deb) + " ms <br/>The new formated file with merged lines is : <br/><b>" +
            //                mergedLines.getAbsolutePath() + "</b><br/>Please wait during creation call tree and tables. Close this dialog</html>")
            MyDialogFormatPHPTrace.jdia.setVisible(false)
            MyDialogFormatPHPTrace.jdia.setTitle("Please wait while building tree and tables.treatement of file : " + mergedLines.getName())
            MyDialogFormatPHPTrace.jdia.setVisible(true)
            MyDialogFormatPHPTrace.jdia.pack()
            MyDialogFormatPHPTrace.jdia.setLocation(300, 300)
          }

        }
        fileLog = fc.getSelectedFile().getAbsolutePath()
        var deb = System.currentTimeMillis()
        if (null == fileLog) {
          JOptionPane.showMessageDialog(null, "You must select a file ")
        } else {
          if (fc.getSelectedFile().getName().startsWith("newFormat1Trace_") || fc.getSelectedFile().getName().startsWith("newFormat1TraceAgg_")) {
            mergedLines = new File(fc.getSelectedFile().getParent() + File.separator + fc.getSelectedFile().getName())
          } else {
            mergedLines = new File(fc.getSelectedFile().getParent() + File.separator + "newFormat1Trace_" + fc.getSelectedFile().getName())
            mergedLinesAgg = new File(fc.getSelectedFile().getParent() + File.separator + "newFormat1TraceAgg_" + fc.getSelectedFile().getName())
          }
          if (!(mergedLines.exists()) || (mergedLines.exists() && new File(fileLog).lastModified() > mergedLines.lastModified())) {
            if (mergedLines.exists()) mergedLines.delete()
            if (mergedLinesAgg.exists()) mergedLinesAgg.delete()
            if (mergeLines) {
              var fin = System.currentTimeMillis()

            } else {
              println("return=false")
              return
            }

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

              PHPJPanelForTree.insideSwingScaViewer(args)
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
  private def mergeLines: Boolean = {
    // mergedLines = new File(fc.getSelectedFile().getParent() + File.separator + "newFormatTrace_" + fc.getSelectedFile().getName())
    // if (mergedLines.exists) mergedLines.delete
    val outputStream = initOutputStream(mergedLines)
    val outputStreamAgg = initOutputStream(mergedLinesAgg)
    val inputStream = initFileInputStream(fc.getSelectedFile)

    var countLineDeb: Int = 0
    var countLineFin: Int = 0
    StructRead.clean()
    val buffReader = new scala.io.BufferedSource(inputStream, BUFFER_SIZE).bufferedReader
    var bool: Boolean = true
    // Tuple  temoin (id Fonction Parent, Niveau fonction Parent)
    var tupIdFonctionLevel: (Int, Int) = (0, 0)

    // to treat several traces in a single file
    var compteurForOffset: Int = 0;
    var numline = 0
    var boolTraceXdebug = false
    while (bool) {
      var line = buffReader.readLine()
      if (numline > 3 && boolTraceXdebug == false) {
        JOptionPane.showMessageDialog(null, "This is not a correct xdebug trace file  ")
        return false
      }
      if (numline <= 3 && line.contains("TRACE START")) boolTraceXdebug = true

      numline += 1

      if (null != line) {
        line match {
          case MyDialogFormatPHPTrace.patternFirstLine() => {
            var struc = new StructRead()
            struc.line = line
            struc.fill(StructRead.offset, "patternFirstLine")
            StructRead.hm.put(StructRead.offset , struc)
            countLineDeb += 1
            compteurForOffset += 1

          }
          case MyDialogFormatPHPTrace.patternLastLine() =>
            {
              var struc = StructRead.hm.get(StructRead.offset )
              struc.line = line
              struc.fill(StructRead.offset, "patternLastLine")
              var idFctCur = struc.idFct

              // deduire le temps des inner fonctions
              for (numFct <- struc.children) {
                struc.selfDurationInMillis = struc.selfDurationInMillis - StructRead.hm.get(numFct).durationInMillis
              }

              StructRead.hm.put(StructRead.offset , struc)

              // traitement des fonctions non terminees
              StructRead.listNotClosed.foreach { idFunc =>

                val tmpStruc = StructRead.hm.get(idFunc)
                println("closing : idFunc =" + idFunc + " fonction= " + tmpStruc.nameFonction)
                val innerCal: Calendar = Calendar.getInstance()

                tmpStruc.memAfter = MyDialogFormatPHPTrace.getMemSize(line)
                tmpStruc.deltaMem = tmpStruc.memAfter - tmpStruc.memBefore
                tmpStruc.durationInMillis = (MyDialogFormatPHPTrace.getIndexTime(line) * 1000).toLong - tmpStruc.indexTimeDeb
                tmpStruc.durationGlobalInMillis = tmpStruc.durationInMillis
                // Calculer la selfDuration 

                // deduire le temps des inner fonctions
                for (numFct <- tmpStruc.children) {
                  tmpStruc.selfDurationInMillis = struc.selfDurationInMillis - StructRead.hm.get(idFunc).durationInMillis
                }

                StructRead.hm.put(idFunc, tmpStruc)

              }
              StructRead.listNotClosed.clear()
              compteurForOffset += 1
              StructRead.offset += compteurForOffset
              tupIdFonctionLevel = (0, 0)
              compteurForOffset = 0
              countLineFin += 1
            }

          case MyDialogFormatPHPTrace.debFonction() => {
            var struc = new StructRead()
            compteurForOffset += 1
            struc.line = line
            struc.fill(StructRead.offset, "debFonction")
            StructRead.listNotClosed = StructRead.listNotClosed += (struc.idFct)
            struc.level match {
              case x if x == (tupIdFonctionLevel._2 + 1) => // direct parent in tuple
                {

                  var tmpStruc = StructRead.hm.get(tupIdFonctionLevel._1)
                  tmpStruc.children += struc.idFct
                  tmpStruc.lastChild = struc.idFct
                    StructRead.hm.put(tupIdFonctionLevel._1, tmpStruc)
                  struc.parent = tupIdFonctionLevel._1
                }
              case x if x == (tupIdFonctionLevel._2 + 2) => // grand-parent in tuple 
                {
                  // on cree un nouveau tuple avec le dernier fils du grandParent qui est donc le parent de cette fonction
                  var tmpStruc = StructRead.hm.get(tupIdFonctionLevel._1) // tmpStruc Grand parent
                  var idParent = tmpStruc.lastChild // last Fils du grand parent= parent
                  var tmpStruc2 = StructRead.hm.get(idParent)
                  tmpStruc2.children += struc.idFct
                  tmpStruc2.lastChild = struc.idFct
                  tmpStruc2.parent = tupIdFonctionLevel._1
                  struc.parent=idParent //JLP
                  StructRead.hm.put(idParent, tmpStruc2)
                  tupIdFonctionLevel = (idParent, StructRead.hm.get(idParent).level)

                }

              case x if x == tupIdFonctionLevel._2 => // same level in tuple
                {

                 var idParent = StructRead.hm.get(tupIdFonctionLevel._1).parent
                 
                  var tmpPar = StructRead.hm.get(idParent)
                  tmpPar.children += struc.idFct
                  tmpPar.lastChild = struc.idFct
                  StructRead.hm.put(idParent, tmpPar)
                 // struc.parent=tupIdFonctionLevel._1
                  struc.parent=idParent //JLP
                  tupIdFonctionLevel = (idParent, tmpPar.level)

                }

              case x =>

            }

            StructRead.hm.put(struc.idFct, struc)
            countLineDeb += 1
          }

          case MyDialogFormatPHPTrace.finFonction() => {

            val idFct = MyDialogFormatPHPTrace.getidFunc(line) + StructRead.offset
            if (StructRead.hm.get(idFct)== null) {
              println(" line in Exception = " + line)

            }

            var struc = StructRead.hm.get(idFct)
            struc.line = line
            struc.closed = true
            struc.fill(StructRead.offset, "finFonction")
            StructRead.listNotClosed = StructRead.listNotClosed -= (struc.idFct)
            var idFctCur = struc.idFct
            // deduire le temps des inner fonctions
            for (numFct <- struc.children) {
              struc.selfDurationInMillis = struc.selfDurationInMillis - StructRead.hm.get(numFct).durationInMillis
            }
            StructRead.hm.put(struc.idFct, struc)
            // on verifie que on est au bon niveau d imbrication
            struc.level match {
              case x if x == (tupIdFonctionLevel._2 - 1) => // Decrease the level
                {

                  tupIdFonctionLevel = (struc.idFct, struc.level)
                }
              case _ =>
            }
            countLineFin += 1
          }
          case _ => println("Not treated line : " + line)

        }

      } else bool = false
    }
    println("Number of deb fonction  " + countLineDeb)
    println("Number of fin  fonction  " + countLineFin)
    println("StructRead.hm has " + StructRead.hm.size + " entries")
    mergeFonctions
    println("StructRead.hmExec has " + StructRead.hmExec.size + " entries")

    val title = new StringBuilder().
      append("date;rank;nameFunction;fileNameRow;level;nbExec;durationInMillis;selfDurationInMillis;memBefore;memAfter;deltaMem);\n").toString
    outputStream.write(title.getBytes())

    val it = StructRead.hmExec.values().iterator()
    while (it.hasNext()) {
      val struc = it.next()
      outputStream.write(struc.toString.getBytes());
    }
    // Traitement Agg
    var boolAgg = true

    val titleAgg = new StringBuilder().
      append("dateDebScript;rank;nameParent;nameFunction;level;nbExec;totalDurationInMillis;totalSelfDurationInMillis;avgMemBefore;avgMemAfter;avgDeltaMem);\n").toString
    outputStreamAgg.write(titleAgg.getBytes())
    var nbFct: Int = 1
    while (boolAgg) {
      println("AggStructRead entree nbFct=" + nbFct)
      nbFct = aggFunction(nbFct)
      println("AggStructRead retour nbFct=" + nbFct)
      if (nbFct == -1) boolAgg = false
    }
    println("AggStructRead.hmAgg.size =" + AggStructRead.hmAgg.size())
    // filling the agg file
    var addToOffsetAgg:Int=0
     val itAgg = AggStructRead.hmAgg.values().iterator()
    while (itAgg.hasNext()) {
      val struc = itAgg.next()
      struc.rank=AggStructRead.offset+addToOffsetAgg
      outputStreamAgg.write(struc.toString.getBytes())
      addToOffsetAgg +=1
    }
    AggStructRead.offset += addToOffsetAgg
    outputStreamAgg.close()
    outputStream.close
    inputStream.close
    StructRead.clean()
    return true

  }

  private def aggFunction(idFct: Int): Int = {
    var returnInt = -1
    var idFctCurr = idFct
    println("aggFunction begin  idFct=" + idFctCurr)
    val level = StructRead.hm.get(idFct).level
    var bool = true
    var int = 1
    val nameFunction = StructRead.hm.get(idFct).nameFonction
    val nameParent = StructRead.hm.get(StructRead.hm.get(idFct).parent).nameFonction

    if (!AggStructRead.hmAgg.containsKey(nameParent + ":_:" + nameFunction + ":_:" + level)) {
      AggStructRead.hmAgg.put(nameParent + ":_:" + nameFunction + ":_:" + level, AggStructRead(StructRead.hm.get(idFct)))
      AggStructRead.al.add(nameParent + ":_:" + nameFunction + ":_:" + level)
    }

    while (bool) {
      val structReadNext = StructRead.hm.get(idFctCurr + 1)
      if (null != structReadNext) {

        val nameFunctionNext = structReadNext.nameFonction
        val nameParentNext = StructRead.hm.get(structReadNext.parent).nameFonction

        if (idFctCurr + 1 >= StructRead.hm.size()) {
          bool = false
          println("idFctCurr = " + idFctCurr + " ; StructRead.hm.size() =" + StructRead.hm.size())
          returnInt = -1
        } else {
          val levelNext = StructRead.hm.get(idFctCurr + 1).level
          if (levelNext == 1) {
            // A voir
            returnInt = idFctCurr + 1
            println("nameFonction = " + StructRead.hm.get(idFctCurr + 1).nameFonction)
            bool = false
          } else {
            if (!AggStructRead.hmAgg.containsKey(nameParentNext + ":_:" + nameFunctionNext + ":_:" + levelNext)) {
              AggStructRead.hmAgg.put(nameParentNext + ":_:" + nameFunctionNext + ":_:" + levelNext, AggStructRead(structReadNext))
            } else {
              val aggStructReadOld = AggStructRead.hmAgg.get(nameParentNext + ":_:" + nameFunctionNext + ":_:" + levelNext)
              aggStructReadOld.agg(AggStructRead(structReadNext))
              AggStructRead.hmAgg.put(nameParentNext + ":_:" + nameFunctionNext + ":_:" + levelNext, aggStructReadOld)

            }

          }
        }
        idFctCurr += 1
      } else {
        returnInt = -1

        bool = false
      }
    }
    if (!AggStructRead.hmAgg.isEmpty()) {
      // fill AggStructRead.hmExecAgg
      var idxBegin = AggStructRead.hmExecAgg.size()
      var idxOffset = 0
      for (str <- AggStructRead.al.asScala) {
        AggStructRead.hmExecAgg.put(idxBegin + idxOffset, AggStructRead.hmAgg.get(str))
        idxOffset += 1

      }
    }
    returnInt
  }

  private def mergeFonctions {
    val iter = StructRead.hm.keySet.iterator()
    var currentIdFunc: Int = -1
    var idCompactage = 0
    var currentStruct: StructRead = null
    while (iter.hasNext()) {
      val id: Int = iter.next()
      val struct = StructRead.hm.get(id)

      if (null != currentStruct && !struct.isEqual(currentStruct)) {
        cloreFunc(idCompactage, currentStruct)
        // ouvrir une autre fonction

        currentIdFunc = id
        currentStruct = struct
        idCompactage += 1
      } else {
        if (null == currentStruct)
          currentStruct = struct
        else
          currentStruct = mergeStructRead(idCompactage, currentStruct, struct)

      }

    }
    // clore la derniere fonction
    cloreFunc(idCompactage, currentStruct)
  }

  private def mergeStructRead(idCompactage: Int, currentStruct: StructRead, toMerge: StructRead): StructRead = {

    currentStruct.nbExec += 1
    currentStruct.memBefore += toMerge.memBefore
    currentStruct.memAfter += toMerge.memAfter
    currentStruct.durationGlobalInMillis += toMerge.durationGlobalInMillis
    currentStruct.selfDurationInMillis += toMerge.selfDurationInMillis

    currentStruct
  }
  private def cloreFunc(idCompactage: Int, currentStruct: StructRead) {

    currentStruct.rank = idCompactage
    currentStruct.memBefore = currentStruct.memBefore / currentStruct.nbExec
    currentStruct.memAfter = currentStruct.memAfter / currentStruct.nbExec
    currentStruct.deltaMem = currentStruct.memAfter - currentStruct.memBefore
    currentStruct.durationInMillis = ((currentStruct.durationGlobalInMillis / ((currentStruct.nbExec).toDouble)) * 1000D).toLong.toDouble / 1000D
    currentStruct.selfDurationInMillis = ((currentStruct.selfDurationInMillis / ((currentStruct.nbExec).toDouble)) * 1000D).toLong.toDouble / 1000D
    StructRead.hmExec.put(idCompactage, currentStruct)

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
object MyDialogFormatPHPTrace {
  val jdia: JDialog = new JDialog(null.asInstanceOf[JDialog], "Please wait while building tree and tables")
  jdia.setModal(false)
  jdia.setMinimumSize(new Dimension(900, 20))
  jdia.setPreferredSize(new Dimension(900, 20))
  jdia.setMaximumSize(new Dimension(900, 20))

  val patternFirstLine = """TRACE START\s*\[[^\]]+\]""".r
  //  val patternLastLine = """TRACE END\s*\[[^\]]+\]""".r
  val patternLastLine = """^\s{3}\s*\d+\.\d+\s+\d+""".r
  val debFonction = """^\d+\s+\d+\s+0\s+.*$""".r
  val finFonction = """^\d+\s+\d+\s+1\s+\d+\.\d+\s+\d+\s*$""".r
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  val patIndexTime = """\d+\.\d+""".r
  val patMemSize = """\s+\d+\.\d+\s+\d+""".r
  val patIdFunc = """^\d+\s+\d+""".r
  val patIncludeFile = """(?:require|include|require_once|include_once)\s+[01]\s+[^\s]+""".r

  //5	287	0	0.135000	4368408	PMA_Config->getSource	1		C:\opt\wamp\apps\phpmyadmin4.1.14\libraries\Config.class.php	836
  val patNameFunc = """^\d+\s+\d+\s+[01]\s+\d+\.\d+\s+\d+\s+[^\s]+""".r

  val patfileNameNumRow = """[^\s]+\s+\d+\s*$""".r
  def getDateDebTrace(line: String): java.util.Date =
    {
      dateFormat.parse("""\d{4}-\d\d-\d\d \d\d:\d\d:\d\d""".r.findFirstIn(line).get)
    }
  def getDateFinTrace(line: String): java.util.Date =
    {
      dateFormat.parse("""\d{4}-\d\d-\d\d \d\d:\d\d:\d\d""".r.findFirstIn(line).get)
    }
  def getIndexTime(line: String): Double =
    {
      patIndexTime.findFirstIn(line).get.toDouble
    }

  // 3	5	0	0.008000	566688
  def getMemSize(line: String): Int = {
    """\d+$""".r.findFirstIn(patMemSize.findFirstIn(line).get).get.toInt
  }

  def getLevel(line: String): Int = {
    """^\d+""".r.findFirstIn(line).get.toInt
  }

  def getidFunc(line: String): Int = {
    """\d+$""".r.findFirstIn(patIdFunc.findFirstIn(line).get).get.toInt
  }

  def getNameFunc(line: String): String = {
    """[^\s]+$""".r.findFirstIn(patNameFunc.findFirstIn(line).get).get
  }

  def getFileNameNumRow(line: String): String = {

    patfileNameNumRow.findFirstIn(line).get.replaceAll("""\s+""", "_")

  }

  def getIncludeFile(line: String): String = {
    """[^\s]+$""".r.findFirstIn(patIncludeFile.findFirstIn(line).get).get
  }
  def getIndexFinMain(line: String): Double = {
    """\d+\.\d+""".r.findFirstIn(line).get.toDouble
  }
  def getMemoryFinMain(line: String): Int = {
    ("""\d+\s*$""".r.findFirstIn(line).get).trim.toInt
  }

}
class AggStructRead {
   var rank: Int = -1
  val dfs: DecimalFormatSymbols = new DecimalFormatSymbols(Locale.ENGLISH)
  // dfs.setGroupingSeparator('')
  dfs.setDecimalSeparator('.')
  val df: DecimalFormat = new DecimalFormat("#############0.###", dfs)
  var dateDebFunc: Date = null
  var nbExec: Int = 1
  var nameFonction: String = null
  var memBefore: Int = 0
  var memAfter: Int = 0
  var deltaMem: Int = 0
  var fileNameRow: String = null
  var level: Int = 0
  var durationInMillis: Double = 0
  var durationGlobalInMillis: Double = 0
  var selfDurationInMillis: Double = 0

  var nameParent: String = ""
  var children: ListBuffer[Int] = new ListBuffer()
  var lastChild: Int = 0
  var closed: Boolean = false

  def agg(that: AggStructRead) {
    // unitaire faire un pro-rata

//    durationInMillis = (that.durationInMillis * that.nbExec + durationInMillis * nbExec) 
//    selfDurationInMillis = (that.selfDurationInMillis * that.nbExec + selfDurationInMillis * nbExec)
    durationInMillis = (that.durationInMillis  + durationInMillis ) 
    selfDurationInMillis = (that.selfDurationInMillis + selfDurationInMillis )


    memBefore = (that.memBefore * that.nbExec + memBefore * nbExec) / (nbExec + that.nbExec)
    memAfter = (that.memAfter * that.nbExec + memAfter * nbExec) / (nbExec + that.nbExec)
    deltaMem = (that.deltaMem * that.nbExec + deltaMem * nbExec) / (nbExec + that.nbExec)
    nbExec += that.nbExec
  }
   override def toString(): String = {
    var strBuild: StringBuilder = new StringBuilder()
    strBuild.append(MyDialogFormatPHPTrace.dateFormat.format(dateDebFunc)).
      append(";").append(rank).append(";").append(nameParent).append(";").append(nameFonction).append(";").append(level).append(";").
      append(nbExec).append(";").append(df.format(durationInMillis)).append(";").
      append(df.format(selfDurationInMillis)).append(";").
      append(df.format(memBefore)).append(";").append(df.format(memAfter)).append(";").
      append(df.format(deltaMem)).append(";\n").toString
  }

}
class StructRead {
  val dfs: DecimalFormatSymbols = new DecimalFormatSymbols(Locale.ENGLISH)
  // dfs.setGroupingSeparator('')
  dfs.setDecimalSeparator('.')
  val df: DecimalFormat = new DecimalFormat("#############0.###", dfs)

  var rank: Int = -1
  var line: String = null
  var dateDebFunc: Date = null
  var idFct: Int = 0
  var level: Int = 0
  var indexTimeDeb: Double = 0
  var nbExec: Int = 1
  var nameFonction: String = null
  var memBefore: Int = 0
  var memAfter: Int = 0
  var deltaMem: Int = 0
  var fileNameRow: String = null
  var dateFinFunc: Date = null
  var durationInMillis: Double = 0
  var durationGlobalInMillis: Double = 0
  var selfDurationInMillis: Double = 0
  var parent: Int = -1
  var children: ListBuffer[Int] = new ListBuffer()
  var lastChild: Int = 0
  var closed: Boolean = false

  def isEqual(that: StructRead): Boolean = {

    this.nameFonction.equals(that.nameFonction) && this.fileNameRow.equals(that.fileNameRow) && this.level == that.level

  }
  def fill(offset: Int, kind: String): Unit =
    {
      kind match {
        case "patternFirstLine" =>
          {
            dateDebFunc = MyDialogFormatPHPTrace.getDateDebTrace(line)
            StructRead.dateDebScript = dateDebFunc
            StructRead.cal = Calendar.getInstance()
            StructRead.cal.setTime(StructRead.dateDebScript)
            idFct = offset - 1
            level = 0
            nameFonction = "page_script_php"
            
          }

        case "patternLastLine" =>
          {
            println("lastline =" + line)

            durationInMillis = ((MyDialogFormatPHPTrace.getIndexTime(line) * 1000000).toLong).toDouble / (1000.toDouble)
            memAfter = MyDialogFormatPHPTrace.getMemSize(line)
            deltaMem = memAfter
            durationGlobalInMillis = durationInMillis
            selfDurationInMillis = durationInMillis

          }

        case "debFonction" =>
          {
            idFct = MyDialogFormatPHPTrace.getidFunc(line) + offset
            level = MyDialogFormatPHPTrace.getLevel(line)
            nameFonction = MyDialogFormatPHPTrace.getNameFunc(line)
            indexTimeDeb = ((MyDialogFormatPHPTrace.getIndexTime(line) * 1000000).toLong).toDouble / 1000
            val innerCal: Calendar = Calendar.getInstance()

            innerCal.setTimeInMillis(StructRead.cal.getTimeInMillis() + (MyDialogFormatPHPTrace.getIndexTime(line) * 1000).toLong)
            dateDebFunc = innerCal.getTime()
            fileNameRow = MyDialogFormatPHPTrace.getFileNameNumRow(line)
            memBefore = MyDialogFormatPHPTrace.getMemSize(line)
            if (nameFonction.equals("{main}")) {
              println("MyDialog traitement de main")
              var tmpNameFonction = nameFonction
              if (fileNameRow.contains("/")) {
                tmpNameFonction = fileNameRow.substring(fileNameRow.lastIndexOf("/") + 1).split("_")(0)
              } else if (fileNameRow.contains("\\")) {

                tmpNameFonction = fileNameRow.substring(fileNameRow.lastIndexOf("\\") + 1).split("_")(0)
              }
              
              val scriptFileNameRow = fileNameRow.substring(0, fileNameRow.lastIndexOf("_")) + "_0"
              val tmpStruc = StructRead.hm.get(StructRead.offset )
              tmpStruc.nameFonction = tmpNameFonction
              tmpStruc.fileNameRow = scriptFileNameRow
              StructRead.hm.put(idFct, tmpStruc)
             
            }

          }
        case "finFonction" =>
          {

            val innerCal: Calendar = Calendar.getInstance()

            memAfter = MyDialogFormatPHPTrace.getMemSize(line)
            deltaMem = memAfter - memBefore
            durationInMillis = ((MyDialogFormatPHPTrace.getIndexTime(line) * 1000000).toLong).toDouble / 1000 - indexTimeDeb
            durationGlobalInMillis = durationInMillis
            selfDurationInMillis = durationInMillis

          }
        case _ => throw new Exception("case not treated")
      }

    }

  override def toString(): String = {
    var strBuild: StringBuilder = new StringBuilder()
    strBuild.append(MyDialogFormatPHPTrace.dateFormat.format(dateDebFunc)).
      append(";").append(rank).append(";").append(nameFonction).append(";").append(fileNameRow).append(";").append(level).append(";").
      append(nbExec).append(";").append(df.format(durationInMillis)).append(";").
      append(df.format(selfDurationInMillis)).append(";").
      append(df.format(memBefore)).append(";").append(df.format(memAfter)).append(";").
      append(df.format(deltaMem)).append(";\n").toString
  }
}
object AggStructRead {
  var offset:Int=0
  // key String = <parent>:_:<child>
  var hmAgg: java.util.HashMap[String, AggStructRead] = new java.util.HashMap()
  var al: ArrayList[String] = new ArrayList() // to keep the order of functions
  var hmExecAgg: java.util.TreeMap[Int, AggStructRead] = new java.util.TreeMap()
  def apply(structRead: StructRead): AggStructRead = {
    val aggStructRead = new AggStructRead()
     aggStructRead.dateDebFunc=structRead.dateDebFunc
    aggStructRead.nameFonction = structRead.nameFonction

    aggStructRead.children = structRead.children
    aggStructRead.nameParent = StructRead.hm.get(structRead.parent).nameFonction
    aggStructRead.level = structRead.level
    aggStructRead.durationInMillis = structRead.durationInMillis
    aggStructRead.selfDurationInMillis = structRead.durationInMillis
    aggStructRead.memBefore = structRead.memBefore
    aggStructRead.memAfter = structRead.memAfter
    aggStructRead.deltaMem = structRead.deltaMem
    aggStructRead
  }
}
object StructRead {
  var dateDebScript: Date = null
  var cal: Calendar = null
  var hm: TreeMap[Int, StructRead] = new TreeMap()

  var hmExec: java.util.TreeMap[Int, StructRead] = new java.util.TreeMap()
  var listNotClosed: ListBuffer[Int] = new ListBuffer()
  var offset = 0

  def clean() {
    hm.clear()
    hmExec.clear()
    listNotClosed.clear()
    dateDebScript = null
    cal = null
    offset = 0
  }

}