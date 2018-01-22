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
package com.jlp.scaviewer.ui
import javax.swing.TransferHandler
import org.jfree.chart.ChartPanel
import javax.swing.JComponent
import java.awt.datatransfer.Transferable
import java.awt.datatransfer.DataFlavor
import java.io.File
import org.jfree.data.time.TimeSeriesCollection

import org.jfree.chart.JFreeChart
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYItemRenderer
import java.awt.BasicStroke
import java.awt.RenderingHints
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.geom.Ellipse2D
import java.awt.Color
import java.awt.datatransfer.UnsupportedFlavorException
import java.io.IOException
import javax.swing.JTree
import javax.swing.tree.TreePath
import javax.swing.tree.DefaultMutableTreeNode
import com.jlp.scaviewer.ui.tableandchart.CreateChartAndTable
import com.jlp.scaviewer.ui.tableandchart.ScaChartingListener
import com.jlp.scaviewerdyn.ui.ScalaChartingDyn
import com.jlp.scaviewerdyn.ui.actors.MyMessage
import java.io.LineNumberReader
import java.io.FileReader
import java.io.RandomAccessFile

class FileTransfertHandler(cp: ChartPanel) extends TransferHandler {

  /**
   *
   */

  var fl: FilesTransferable = null;

  
  // public FileTransferHandler(TextArea ta)
  @Override
  override def importData(c: JComponent, t: Transferable): Boolean = {
    if (!canImport(c, t.getTransferDataFlavors())) {
      false
    } else {

      // Traitement du File dans l'objet cible. JLP � Faire
      // 
      val df: Array[DataFlavor] = t.getTransferDataFlavors();

      try {
        val lstFile: java.util.List[File] = t
          .getTransferData(java.awt.datatransfer.DataFlavor.javaFileListFlavor).asInstanceOf[java.util.List[File]]
        //System.out.println ("FileTransferHandler : importData Creation TimeSeries Collections");
        //println("Dans import Data :lstFiles ="+ lstFile.toString)

        // Construction pour chaque file de listChartingInfo
        ScaChartingListener.lightClear

        val len = lstFile.size()
        for (i <- 0 until len) {
          var file = lstFile.get(i)

          //          var chartingInfo: ScaCharting.ChartingInfo = ScaCharting.ChartingInfo("", file.length(), file.lastModified(), ScaCharting.tfSample.text.toInt, ScaCharting.rdbMaxPointsOrGap.selected,
          //            ScaCharting.cbStrategie.selection.item, ScaCharting.rdbTimeSeries.selected)
          var chartingInfo: ScaCharting.ChartingInfo = ScaCharting.ChartingInfo("", file.length(), file.lastModified(), ScaCharting.tfSample.text.toInt, ScaCharting.rdbMaxPointsOrGap.selected,
            ScaCharting.cbStrategie.selection.item, true,";",0,"","",null,null,-1L)

          ScaCharting.listChartingInfo = ScaCharting.listChartingInfo :+ chartingInfo
          ScaCharting.listFiles = ScaCharting.listFiles :+ file

          //println("Dans import Data :add Files ="+ file.getAbsolutePath())
        }

        if (ScaCharting.dyn) {
          if (!ScaCharting.listRaf.isEmpty) {
            ScaCharting.listRaf foreach ((raf) => if (null != raf) raf.close)
            ScaCharting.listRaf = List.empty
          }
          val len = ScaCharting.listFiles.length
          for (i <- 0 until len) {
            var file = ScaCharting.listFiles(i)
            var raf: RandomAccessFile = new RandomAccessFile(file, "r")
            raf.seek(raf.length())
            ScaCharting.listRaf = ScaCharting.listRaf :+ raf
          }
          ScalaChartingDyn.stop
          ScalaChartingDyn.start

          ScalaChartingDyn.observer ! new MyMessage(ScalaChartingDyn.actorAction)
        } else {
          // On ferme tous les Raf s'ils existent
          if (!ScaCharting.listRaf.isEmpty) {
            ScaCharting.listRaf foreach ((raf) => if (null != raf) raf.close)
            ScaCharting.listRaf = List.empty
          }

        }

        var cca: CreateChartAndTable = new CreateChartAndTable(ScaCharting.listFiles)

        cca.createChartPanel

      } catch {
        case e: UnsupportedFlavorException =>
          // TODO Auto-generated catch block
          e.printStackTrace();
        case e: IOException =>

          // TODO Auto-generated catch block
          e.printStackTrace();
      }

      true
    }

  }

  /**
   * for the JTree
   */

  override def createTransferable(c: JComponent): Transferable = {
    if (!c.isInstanceOf[MyJTree])
      null;
    else {
      val tree: JTree = c.asInstanceOf[JTree];
      // get selection and return as a transferable
      var paths: Array[TreePath] = tree.getSelectionPaths();
      var lstF: List[File] = List[File]();
      for (i <- 0 until paths.length) {

        var node: DefaultMutableTreeNode = paths(i)
          .getLastPathComponent().asInstanceOf[DefaultMutableTreeNode];
        var file: MyFile = node.getUserObject().asInstanceOf[MyFile];
        if (!file.isDirectory()) {
          lstF = new MyFile(file.getAbsolutePath()) +: lstF
        }
      }
      lstF = lstF.reverse
      if (lstF.size > 0) {

        fl = new FilesTransferable(java.util.Arrays.asList(lstF.toArray: _*))
        fl
      } else
        null
    }
  }

  override def getSourceActions(c: JComponent): Int = {
    TransferHandler.COPY;
  }

  override def canImport(c: JComponent, flavors: Array[DataFlavor]): Boolean = {

    var ret: Boolean = false
    for (j <- 0 until flavors.length) {
      if (java.awt.datatransfer.DataFlavor.javaFileListFlavor.equals(flavors(j)) || FilesTransferable.localFileFlavor.equals(flavors(j))) //	if(fl.isDataFlavorSupported(flavors(j)))
      {
        //println("Transfert handler : flavors="+flavors(j).toString)
        ret = true
      }
    }
    // println("passage dans canImport ret="+ret)
    ret
  }

}