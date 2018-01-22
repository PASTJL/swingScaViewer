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
import java.io.FileOutputStream
import java.io.FileInputStream
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar
import com.jlp.scaviewer.ui.SwingScaViewer
import java.util.Properties

package object saveandload {

  def save(cfp: ConfigParser) {
    var pathConfig = cfp.path + "logs" + File.separator + "config" + File.separator + "scaparser"
    println("save :  cfp.path=" + cfp.path)
    println("save :  pathConfig=" + pathConfig)
    if (!new File(pathConfig).exists) new File(pathConfig).mkdir
    var propsFile = new File(pathConfig + File.separator + cfp.prefixFile + ".properties")
    cfp.props.clear
    cfp.props.put("fileIn.pathFile", cfp.tfFileIn.text)
    cfp.props.put("fileIn.stepAgg", cfp.tfStepAggr.text)
    cfp.props.put("fileIn.startEnrReg", cfp.tfDebEnr.text)
    cfp.props.put("fileIn.finEnrReg", cfp.tfFinEnr.text)
    cfp.props.put("fileIn.inclEnrReg", cfp.tfInclEnr.text)
    cfp.props.put("fileIn.exclEnrReg", cfp.tfExclEnr.text)
    cfp.props.put("fileIn.stepWithinEnreg", cfp.tfStep2Enr.text)
    if (cfp.rbLocaleIn.selected) cfp.props.put("fileIn.localEnglish", "true")
    else cfp.props.put("fileIn.localEnglish", "false")
    if (cfp.rbExplicitDate.selected) cfp.props.put("fileIn.explicitDate", "true")
    else cfp.props.put("fileIn.explicitDate", "false")
    if (cfp.tfDateImplDebut.text.length > 6) {
      cfp.props.put("fileIn.startDate", cfp.tfDateImplDebut.text)
    
    } else {
      cfp.props.put("fileIn.startDate", "1970/01/01 00:00:00")
    }
    cfp.props.put("fileIn.unitStep", cfp.tfStepUnit.text)
    cfp.props.put("fileIn.dateRegex", cfp.tfDateRegex.text)
    cfp.props.put("fileIn.dateFormatIn", cfp.tfDateFormat.text)
    if (cfp.tfStartDate.text.length == 19) cfp.props.put("fileIn.startParsingDate", cfp.tfStartDate.text)
    else
      cfp.props.put("fileIn.startParsingDate", "1970/01/01 00:00:00")
    if (cfp.tfEndDate.text.length == 19) cfp.props.put("fileIn.endParsingDate", cfp.tfEndDate.text)
    else
      cfp.props.put("fileIn.endParsingDate", "1970/01/01 00:00:00")
    cfp.props.put("filesOut.pathDir", cfp.tfFileOut.text)
    cfp.props.put("filesOut.fsOut", cfp.tfFsOut.text)
    cfp.props.put("filesOut.dateFormatOut", cfp.tfDateFormatOut.text)
    if(null != cfp.tfDecalTimeZone.text && cfp.tfDecalTimeZone.text.length() >0 )
    {
      cfp.props.put("advanced.decalTimeZone",cfp.tfDecalTimeZone.text)
    }
    else
    {
       cfp.props.put("advanced.decalTimeZone","0")
    }
    if (cfp.rbLocaleOut.selected) cfp.props.put("filesOut.localeEnglishOut", "true")
    else cfp.props.put("filesOut.localeEnglishOut", "false")
    if (cfp.rbAllAveragesOnly.selected) cfp.props.put("filesOut.generateAllAveragesOnly", "true")
    else cfp.props.put("filesOut.generateAllAveragesOnly", "false")

    cfp.props.put("advanced.nbActors", cfp.tfActors.text)
    cfp.props.put("advanced.correctDate", cfp.cbxDuration.selection.item.asInstanceOf[Int].toString)
    if (cfp.rbViewAllAverages.selected) cfp.props.put("advanced.viewAllAverages", "true")
    else cfp.props.put("advanced.viewAllAverages", "false")
    if (cfp.rbDebug.selected) cfp.props.put("advanced.debugMode", "true")
    else cfp.props.put("advanced.debugMode", "false")
    if (cfp.rbPivotParsingExhaustif.selected) cfp.props.put("advanced.pivotExhaustifParsing", "true")
    else cfp.props.put("advanced.pivotExhaustifParsing", "false")
    if (cfp.rbGenerateEnrToFile.selected) cfp.props.put("advanced.generateEnrToFile", "true")
    else cfp.props.put("advanced.generateEnrToFile", "false")

    if (cfp.rbisDebDate.selected) cfp.props.put("advanced.isDebDate", "true")
    else cfp.props.put("advanced.isDebDate", "false")

    var names = ""
    for (i <- 0 until cfp.jtValues.getRowCount) {
      if (null != cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Name")) &&
        cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Name")).toString.length > 1) {
        names += cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Name")).toString + " "
        cfp.props.put("values.reg1." + cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Name")).toString, cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("First Regex / Function")).toString)
        if (null != cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Second Regex / Parameters")) &&
          cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Second Regex / Parameters")).toString.length > 1) {
          cfp.props.put("values.reg2." + cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Name")).toString, cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Second Regex / Parameters")).toString)
        } else {
          cfp.props.put("values.reg2." + cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Name")).toString, "")
        }

        cfp.props.put("values.unit." + cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Name")).toString, cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Unit")).toString)
        cfp.props.put("values.scale." + cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Name")).toString, cfp.jtValues.getValueAt(i, cfp.jtValues.getColumnModel.getColumnIndex("Scale")).toString)
      }
    }
    cfp.props.put("values.names", names.trim)
    names = ""
    for (i <- 0 until cfp.jtPivots.getRowCount) {
      if (null != cfp.jtPivots.getValueAt(i, cfp.jtPivots.getColumnModel.getColumnIndex("Name")) &&
        cfp.jtPivots.getValueAt(i, cfp.jtPivots.getColumnModel.getColumnIndex("Name")).toString.length > 1) {
        names += cfp.jtPivots.getValueAt(i, cfp.jtPivots.getColumnModel.getColumnIndex("Name")).toString + " "
        cfp.props.put("pivots.reg1." + cfp.jtPivots.getValueAt(i, cfp.jtPivots.getColumnModel.getColumnIndex("Name")).toString, cfp.jtPivots.getValueAt(i, cfp.jtPivots.getColumnModel.getColumnIndex("First Regex")).toString)
        if (null != cfp.jtPivots.getValueAt(i, cfp.jtPivots.getColumnModel.getColumnIndex("Second Regex")) &&
          cfp.jtPivots.getValueAt(i, cfp.jtPivots.getColumnModel.getColumnIndex("Second Regex")).toString.length > 1) {
          cfp.props.put("pivots.reg2." + cfp.jtPivots.getValueAt(i, cfp.jtPivots.getColumnModel.getColumnIndex("Name")).toString, cfp.jtPivots.getValueAt(i, cfp.jtPivots.getColumnModel.getColumnIndex("Second Regex")).toString)
        } else {
          cfp.props.put("pivots.reg2." + cfp.jtPivots.getValueAt(i, cfp.jtPivots.getColumnModel.getColumnIndex("Name")).toString, "")
        }

      }
    }
    cfp.props.put("pivots.names", names.trim)

    var fos = new FileOutputStream(propsFile)
    cfp.props.store(fos, "Saved " + new Date().toString)
    fos.close

  }

  def load(cfp: ConfigParser) {
    if (null != cfp.template && cfp.template.length > 10) {
      loadWithFile(cfp, cfp.template)
    } else {

      var pathConfig = cfp.path + File.separator + "logs" + File.separator + "config" + File.separator + "scaparser"
      if (new File(pathConfig + File.separator + cfp.prefixFile + ".properties").exists) {
        // on charge ce fichier
       
        loadWithFile(cfp, pathConfig + File.separator + cfp.prefixFile + ".properties")
      } else {
        if (new File(cfp.path + File.separator + "testDate.properties").exists) {
          val propsDate = new Properties()
          propsDate.load(new FileInputStream(new File(cfp.path + File.separator + "testDate.properties")))

          cfp.tfStartDate.text = propsDate.getProperty("beginTestDate")
          cfp.tfEndDate.text = propsDate.getProperty("endTestDate")
        } else {
          cfp.tfStartDate.text = "1970/01/01 00:00:00"
          cfp.tfEndDate.text = "1970/01/01 00:00:00"
        }
      }
    }

  }
  private def loadWithFile(cfp: ConfigParser, strFile: String) {
    var fis = new FileInputStream(new File(strFile))
    cfp.props.clear
    cfp.props.load(fis)
    fis.close

    // cfp.tfFileIn.text = cfp.props.getProperty("fileIn.pathFile")
    cfp.tfFileIn.text = MyDialogOpenLog.fileLog
    cfp.props.setProperty("fileIn.pathFile", MyDialogOpenLog.fileLog)
    cfp.tfStepAggr.text = cfp.props.getProperty("fileIn.stepAgg")
    cfp.tfDebEnr.text = cfp.props.getProperty("fileIn.startEnrReg")
    cfp.tfFinEnr.text = cfp.props.getProperty("fileIn.finEnrReg")
    cfp.tfInclEnr.text = cfp.props.getProperty("fileIn.inclEnrReg")
    cfp.tfExclEnr.text = cfp.props.getProperty("fileIn.exclEnrReg")

    val pathFile = new File(strFile).getAbsolutePath();
    println("pathFile=" + pathFile)
    println("path root template=" + System.getProperty("root") + File.separator + "templates")
    println("path local template=" + System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject + File.separator + "templates")
    if (pathFile.contains( "templates") ) {
      println("testDate.properties="+cfp.path + File.separator + "testDate.properties")
      if (new File(cfp.path + File.separator + "testDate.properties").exists) {
        val propsDate = new Properties()
        propsDate.load(new FileInputStream(new File(cfp.path + File.separator + "testDate.properties")))

        cfp.tfStartDate.text = propsDate.getProperty("beginTestDate")
        cfp.tfEndDate.text = propsDate.getProperty("endTestDate")
      } else {
        cfp.tfStartDate.text = "1970/01/01 00:00:00"
        cfp.tfEndDate.text = "1970/01/01 00:00:00"
      }

    } else {
      cfp.tfStartDate.text = cfp.props.getProperty("fileIn.startParsingDate", "1970/01/01 00:00:00")
      cfp.tfEndDate.text = cfp.props.getProperty("fileIn.endParsingDate", "1970/01/01 00:00:00")
    }

    if (cfp.props.getProperty("fileIn.localEnglish") == "true") {
      cfp.rbLocaleIn.selected = true
    } else {
      cfp.rbLocaleIn.selected = false
    }

    if (cfp.props.getProperty("fileIn.explicitDate") == "true") {
      cfp.rbExplicitDate.selected = true
      cfp.tfDateImplDebut.editable = true
      cfp.tfDateRegex.editable = true
      cfp.tfStep2Enr.editable = false
      cfp.tfStepUnit.editable = false
      cfp.tfDateRegex.text = cfp.props.getProperty("fileIn.dateRegex")
       if (cfp.props.getProperty("fileIn.startDate", "1970/01/01 00:00:00").length > 6)
        {
         cfp.tfDateImplDebut.text = cfp.props.getProperty("fileIn.startDate", "1970/01/01 00:00:00")
        }
      else
      { 
        cfp.tfDateImplDebut.text = "1970/01/01 00:00:00"
        
      }
    } else {
      cfp.tfDateRegex.editable = false
      cfp.rbExplicitDate.selected = false
      cfp.tfDateImplDebut.editable = true
      cfp.tfStep2Enr.editable = true
      cfp.tfStepUnit.editable = true
      cfp.tfStep2Enr.text = cfp.props.getProperty("fileIn.stepWithinEnreg")
      if (cfp.props.getProperty("fileIn.startDate", "1970/01/01 00:00:00").length > 6)
        cfp.tfDateImplDebut.text = cfp.props.getProperty("fileIn.startDate", "1970/01/01 00:00:00")
      else
        cfp.tfDateImplDebut.text = "1970/01/01 00:00:00"
      cfp.tfStepUnit.text = cfp.props.getProperty("fileIn.unitStep")

    }

    cfp.tfDateFormat.text = cfp.props.getProperty("fileIn.dateFormatIn")
    val dtf = new SimpleDateFormat("_yyyyMMdd_HHmmss")
    var cal = Calendar.getInstance

    val date = dtf.format(cal.getTime)
    cfp.tfFileOut.editable = true

    val prefixFile =
      {
        var fileOnly = MyDialogOpenLog.fileLog.substring(MyDialogOpenLog.fileLog.lastIndexOf(File.separator) + 1)
        var prefix = fileOnly
        if (prefix.contains(".")) {
          prefix = prefix.substring(0, prefix.indexOf("."))
        }
        prefix
      }
    val path = MyDialogOpenLog.fileLog.substring(0, MyDialogOpenLog.fileLog.lastIndexOf("logs" + File.separator))
    cfp.tfFileOut.text = path + "csv" + File.separator + prefixFile + date + File.separator
    cfp.props.setProperty("filesOut.pathDir", cfp.tfFileOut.text)
    cfp.tfFileOut.editable = false

    cfp.tfFsOut.text = cfp.props.getProperty("filesOut.fsOut")
    cfp.tfDateFormatOut.text = cfp.props.getProperty("filesOut.dateFormatOut")

    if (cfp.props.getProperty("filesOut.localeEnglishOut") == "true")
      cfp.rbLocaleOut.selected = true
    else
      cfp.rbLocaleOut.selected = false

    if (cfp.props.getProperty("filesOut.generateAllAveragesOnly") == "true")
      cfp.rbAllAveragesOnly.selected = true
    else
      cfp.rbAllAveragesOnly.selected = false

    cfp.tfActors.text = cfp.props.getProperty("advanced.nbActors")
    cfp.cbxDuration.selection.item = cfp.props.getProperty("advanced.correctDate").toInt

    if (cfp.props.getProperty("advanced.viewAllAverages") == "true")
      cfp.rbViewAllAverages.selected = true
    else
      cfp.rbViewAllAverages.selected = false

    if (cfp.props.getProperty("advanced.debugMode") == "true")
      cfp.rbDebug.selected = true
    else
      cfp.rbDebug.selected = false

    if (cfp.props.getProperty("advanced.pivotExhaustifParsing") == "true")
      cfp.rbPivotParsingExhaustif.selected = true
    else
      cfp.rbPivotParsingExhaustif.selected = false

    if (cfp.props.getProperty("advanced.generateEnrToFile") == "true")
      cfp.rbGenerateEnrToFile.selected = true
    else
      cfp.rbGenerateEnrToFile.selected = false

    if (cfp.props.getProperty("advanced.isDebDate", "true") == "true")
      cfp.rbisDebDate.selected = true
    else
      cfp.rbisDebDate.selected = false

      cfp.tfDecalTimeZone.text=cfp.props.getProperty("advanced.decalTimeZone","0")
    
    if (null != cfp.props.getProperty("values.names") && cfp.props.getProperty("values.names").length > 1) {
      var namesValues = cfp.props.getProperty("values.names").split("\\s+")
      var i = 0
      for (name <- namesValues) {
        cfp.jtValues.setValueAt(name, i, cfp.jtValues.getColumnModel.getColumnIndex("Name"))
        cfp.jtValues.setValueAt(cfp.props.getProperty("values.reg1." + name, ""), i, cfp.jtValues.getColumnModel.getColumnIndex("First Regex / Function"))
        cfp.jtValues.setValueAt(cfp.props.getProperty("values.reg2." + name, ""), i, cfp.jtValues.getColumnModel.getColumnIndex("Second Regex / Parameters"))
        cfp.jtValues.setValueAt(cfp.props.getProperty("values.unit." + name, ""), i, cfp.jtValues.getColumnModel.getColumnIndex("Unit"))
        cfp.jtValues.setValueAt(cfp.props.getProperty("values.scale." + name, ""), i, cfp.jtValues.getColumnModel.getColumnIndex("Scale"))
        i += 1
      }
    }

    if (null != cfp.props.getProperty("pivots.names") && cfp.props.getProperty("pivots.names").length > 1) {
      var i = 0
      var namesPivots = cfp.props.getProperty("pivots.names").split("\\s+")

      for (name <- namesPivots) {
        cfp.jtPivots.setValueAt(name, i, cfp.jtPivots.getColumnModel.getColumnIndex("Name"))
        cfp.jtPivots.setValueAt(cfp.props.getProperty("pivots.reg1." + name, ""), i, cfp.jtPivots.getColumnModel.getColumnIndex("First Regex"))
        cfp.jtPivots.setValueAt(cfp.props.getProperty("pivots.reg2." + name, ""), i, cfp.jtPivots.getColumnModel.getColumnIndex("Second Regex"))

        i += 1
      }

    }

  }
}