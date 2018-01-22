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
package com.jlp.scaviewer.scalogparser
import java.io.RandomAccessFile
import java.io.File
import java.io.FileInputStream
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar
import javax.swing.JOptionPane
import akka.actor.ActorSystem
import akka.actor.Props
import scala.io.Source
import java.util.Properties
import scala.util.matching.Regex
import com.jlp.scaviewer.tools.CsvClean
import com.jlp.scaviewer.scalogparser.ui.DialogDirectParser
import java.util.zip.GZIPInputStream
import java.io.OutputStream
import java.io.IOException
import java.io.FileOutputStream
import com.jlp.scaviewer.ui.SwingScaViewer

class DirectParserMain(fileLog: String, propsPopular: java.util.Properties) {

  var mySystem: ActorSystem = null
  val listTemplates = propsPopular.getProperty("popular.list").split(";").toList
  println("DirectParserMain: popular.list =" + propsPopular.getProperty("popular.list"))
  var fit: String = ""

  var zipped = false;
  //listTemplates foreach { templ =>
  var isFit = false
  var localExplicitDate = true
  println("filelog=" + fileLog)
  var fileLogUnzipped = fileLog;

  if (fileLog.endsWith(".gz")) {
    System.out.println("on dezippe")
    // decompacter le gz.
    zipped = true;
    val idx: Int = fileLog.lastIndexOf(".gz")
    fileLogUnzipped = fileLog.substring(0, idx)
    dzip(fileLog);
  }

  for (templ <- listTemplates; if (isFit == false)) {
    // var raf = new RandomAccessFile(new File(fileLog), "r")
    var raf = new RandomAccessFile(new File(fileLogUnzipped), "r")
    val debEnr = propsPopular.getProperty("popular." + templ + ".debEnr").r
    val finEnr = propsPopular.getProperty("popular." + templ + ".finEnr", "")
    val isDateExplicit = propsPopular.getProperty("popular." + templ + ".isDateExplicit").toBoolean
    val reg1 = propsPopular.getProperty("popular." + templ + ".reg1").r
    val excl: Regex = {
      if (propsPopular.getProperty("popular." + templ + ".excl", "").length() > 0) {
        propsPopular.getProperty("popular." + templ + ".excl", "").r
      } else
        null
    }

    val nbPoints = propsPopular.getProperty("popular." + templ + ".nbPoints", "500").toInt
    val boolExcl = null != excl
    println(templ + " => boolExcl=" + boolExcl)
    if (finEnr == "") {
      raf.seek(0)
      // cas monoligne
      println("traitement templ " + templ + " en Monoligne")

      // lecture de 10 lignes pour verif debut et reg1
      for (i <- 0 until 100; if (!isFit)) {
        var line = raf.readLine
        if (null != line) {
          //          println ("MonoLine line="+line)
          //          println ("MonoLine debEnr="+debEnr)
          //          println ("MonoLine reg1="+reg1)
          if (!boolExcl || (boolExcl && None == excl.findFirstIn(line))) {
            var ext1 = debEnr.findFirstIn(line)
            var ext2 = reg1.findFirstIn(line)
            if (None == ext1 || None == ext2) {
              isFit = false
            } else {
              isFit = true
            }
          }
        }
      }
      if (isFit) {
        fit = templ
        localExplicitDate = isDateExplicit
      }

      raf.close
    } else {
      // cas multiligne
      // creation de 1 enregistrement
      println("traitement templ " + templ + " en multiligne")
      var properties: java.util.Properties = new Properties()
      println("avant de charger proprietes")
      println(System.getProperty("root") + File.separator + "templates" + File.separator +
        "scaparser" + File.separator + "popular" + File.separator + templ + ".properties")
      properties.load(new FileInputStream(new File(System.getProperty("root") + File.separator + "templates" + File.separator +
        "scaparser" + File.separator + "popular" + File.separator + templ + ".properties")))
      println("fileIn.finEnrReg=" + properties.getProperty("fileIn.finEnrReg", ""))
      if (properties.getProperty("fileIn.finEnrReg", "") != "") {
        raf.seek(0)
        var enr = ""
        var bool = true
        var i = 0
        var debTrouve = false
        var finTrouve = false
        while (bool && i < 1000) {

          // trouver le debut
          var line = raf.readLine

          if (null != line) {
            if (!boolExcl || (boolExcl && None == excl.findFirstIn(line))) {
              i += 1
              var ext1 = debEnr.findFirstIn(line)
              if (None != ext1 && !debTrouve) {

                enr = line.substring(line.indexOf(ext1.get) + ext1.get.length)

                debTrouve = true
              } else if (debTrouve) {
                // chercher la fin

                var ext2 = finEnr.r.findFirstIn(enr)
                if (None != ext2) {
                  bool = false
                  finTrouve = true

                  //enr = enr + " " + line.substring(line.indexOf(ext2.get) + ext2.get.length)
                } else {
                  enr = enr + " " + line
                }
              }
              if (finTrouve) bool = false
            }
          } else {
            bool = false
          }
        }
        if (finTrouve && enr != "") {
          var ext3 = reg1.findFirstIn(enr)
          if (None != ext3) {
            fit = templ
            isFit = true
            localExplicitDate = isDateExplicit
          } else {
            println("pas trouve reg1 " + reg1 + " dans " + enr)
          }
        }
        raf.close
      }
    }
  }

  if (fit != "") {
    val nbPoints = propsPopular.getProperty("popular." + fit + ".nbPoints", "500")
    println("Le template correct de :" + fileLog + " est :" + fit)
    // on va crrer le template a partir de ce fichier fit
    val popularTemplates = System.getProperty("root") + File.separator + "templates" + File.separator + "scaparser" + File.separator + "popular"
    var props = new java.util.Properties
    var fis = new FileInputStream(new File(popularTemplates + File.separator + fit + ".properties"))
    props.load(fis)
    fis.close
    props.put("fileIn.pathFile", fileLog)
    props.put("nbPoints", nbPoints)
    val dtf = new SimpleDateFormat("_yyyyMMdd_HHmmss")
    val dtfbis = new SimpleDateFormat("_yyMMdd_HHmmss")
    var cal = Calendar.getInstance

    val date = dtf.format(cal.getTime)
    val prefixFile =
      {
        var fileOnly = fileLog.substring(fileLog.lastIndexOf(File.separator) + 1)
        var prefix = fileOnly
        if (prefix.contains(".")) {
          prefix = prefix.substring(0, prefix.indexOf("."))
        }
        prefix
      }
    val path = fileLog.substring(0, fileLog.lastIndexOf("logs" + File.separator)) + "csv" + File.separator + prefixFile + date + File.separator
    props.put("filesOut.pathDir", path)
    props.put("advanced.viewAllAverages", "true")
    props.put("filesOut.generateAllAveragesOnly", "true")
    props.put("fileIn.endParsingDate", SwingScaViewer.propsDate.getProperty("endTestDate", "3000/01/01 00:00:00"))
    props.put("fileIn.startParsingDate", SwingScaViewer.propsDate.getProperty("beginTestDate", "1970/01/01 00:00:00"))
    // val isExplicitDate = props.getProperty("fileIn.explicitDate").toBoolean
    var fileOnly = fileLog.substring(fileLog.lastIndexOf(File.separator) + 1)

    if (!localExplicitDate) {

      // essayer de trouver la date  dans le nom du fichier
      var dtf3 = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
      // props.put("fileIn.dateFormatIn", "yyyy/MM/dd HH:mm:ss")

      var dtf2 = new SimpleDateFormat("yyyyMMdd_HHmmss")
      var dtf2bis = new SimpleDateFormat("yyMMdd_HHmmss")
      val reg = """\d{8}_\d{6}""".r
      val reg2 = """\d{6}_\d{6}""".r
      var ext1 = reg.findFirstIn(fileOnly)

      var ext2 = reg2.findFirstIn(fileOnly)

      if (None != ext1) {
        // Le nom du fichier porte la date au format reg
        var date1 = dtf2.parse(ext1.get)

        props.put("fileIn.startDate", dtf3.format(date1))
        // On peut parser

        parser(props)
        CsvClean.clean
      } else if (None != ext2) {
        //  // Le nom du fichier porte la date au format reg2
        var date1 = dtf2bis.parse(ext2.get)

        props.put("fileIn.startDate", dtf3.format(date1))

        parser(props)
        CsvClean.clean
      } else if (props.getProperty("fileIn.stepWithinEnreg", "").length > 0 && !props.getProperty("fileIn.stepWithinEnreg").contains("val=") && props.getProperty("fileIn.finEnrReg") == "") {
        // On on ne peut avoir que la date de derniere modification pour tous les OS
        var file = new File(fileLogUnzipped)
        val lastModified = file.lastModified
        val regDeb = props.getProperty("fileIn.startEnrReg").r
        // on lit toute les lignes du fichiers

        var line = ""
        var lastLine = ""
        var debRead = System.currentTimeMillis()
        val raf = new RandomAccessFile(file, "r")

        debRead = System.currentTimeMillis()

        // method tail the best :
        //50000 lines / 4.5 Mo => lastLine in 4 ms
        // tail(file,10) return the lastLine where length is > 10
        lastLine = tail(file, 10)

        val regDecal = props.getProperty("fileIn.stepWithinEnreg").r
        val decalExt = regDecal.findFirstIn(lastLine)
        if (None != decalExt) {
          var mutMs = {
            props.getProperty("fileIn.unitStep") match {
              case "ms" => 1L
              case "seconds" => 1000L
              case "secs" => 1000L
              case "micros" => 0.001
              case "mn" => 60000L
              case _ => 1
            }
          }
          val decal = (decalExt.get.replace(",", ".").toDouble * mutMs).toLong
          var dateCreation = lastModified - decal
          val newDate = new Date(dateCreation)
          props.put("fileIn.startDate", dtf3.format(newDate))
          // On peut parser
          parser(props)
          CsvClean.clean
        } else {
          //     //JLP modif
          DirectParserMain.cancel = false
          new DialogDirectParser("", props)
          // JOptionPane.showMessageDialog(null, "Rename file :" + fileOnly + " including dateOfCreation with the format : yyyyMMdd_HHmmss")

          if (!DirectParserMain.cancel) {
            parser(props)
            CsvClean.clean
          }
        }

      } else {
        // envoyer un message de datation du fichier

        //JLP modif
        DirectParserMain.cancel = false
        new DialogDirectParser("val=", props)
        // JOptionPane.showMessageDialog(null, "Rename file :" + fileOnly + " including dateOfCreation with the format : yyyyMMdd_HHmmss")

        if (!DirectParserMain.cancel) {
          parser(props)
          CsvClean.clean
        }

      }

    } else {
      // on peut directement parser
      parser(props)
      CsvClean.clean
    }

  } else {
    JOptionPane.showMessageDialog(null, "Unknown file format , create a template and add the config in the <swingScaViewer_Home>/templates/scaparser/popular directory")

  }
  if (zipped) {
    //effacer le fichier si dezippe
    new File(fileLogUnzipped).delete()
  }
  // minLengthLine is the minimal longer of a valid line
  private def tail(file: File, minLengthLine: Int): String =
    {
      var lastLine = ""

      val raf = new RandomAccessFile(file, "r")
      val lengthFile = raf.length()
      var pointer = lengthFile - 1

      var boolContinue = true

      var readByte: Byte = '0'
      var lengthLineCurrent = 0
      while (boolContinue) {
        raf.seek(pointer)
        readByte = raf.readByte();
        if (readByte == 0xA || readByte == 0xD) {
          if (lengthLineCurrent > minLengthLine) {
            lengthLineCurrent = 0

            boolContinue = false
          } else {
            lengthLineCurrent += 1
            pointer -= 1
          }
        } else {
          lengthLineCurrent += 1
          pointer -= 1
        }
      }

      lastLine = raf.readLine()
      raf.close
      lastLine
    }
  private def parser(props: java.util.Properties) {
    if (null != mySystem) {
      var fut: scala.concurrent.Future[akka.actor.Terminated] = mySystem.terminate()
      while (!fut.isCompleted) {

      }

    }

    mySystem = ActorSystem("MyConfSystem")
    var actorRef = mySystem.actorOf(Props[ScaParserMain], "scaParser")
    // ScaParserMain(this)
    actorRef ! props
  }
  private def dzip(file: String): Unit = {
    if (zipped) {
      var in: GZIPInputStream = null;
      var out: OutputStream = null;
      try {
        in = new GZIPInputStream(new FileInputStream(file));
        out = new FileOutputStream(fileLogUnzipped);
        var buf: Array[Byte] = Array.ofDim(1024 * 4);
        var len: Int = 0;
        var bool: Boolean = true;
        while (bool) {
          len = in.read(buf)
          if (len > 0) {
            out.write(buf, 0, len);
          } else bool = false
        }
      } catch {
        case e: IOException =>

          e.printStackTrace();
      } finally {
        if (in != null)
          try {
            in.close();
          } catch {
            case _: Throwable =>
          }
        if (out != null)
          try {
            out.close();
          } catch {
            case _: Throwable =>
          }
      }
    }
  }
}
object DirectParserMain {
  var cancel: Boolean = false
}