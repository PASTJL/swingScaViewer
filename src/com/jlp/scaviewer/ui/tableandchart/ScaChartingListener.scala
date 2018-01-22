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
package com.jlp.scaviewer.ui.tableandchart
import scala.swing.Component
import com.jlp.scaviewer.ui.ScaCharting
import scala.swing.event.ButtonClicked
import scala.swing.event.EditDone
import scala.swing.event.SelectionChanged
import scala.swing.TextField
import scala.swing.event.ValueChanged
import org.jfree.chart.ChartPanel
import org.jfree.chart.ChartFactory
import scala.swing.SplitPane
import javax.swing.JPanel
import scala.collection.mutable.ArrayBuffer
import com.jlp.scaviewer.timeseries.StructTs
import org.jfree.chart.axis.NumberAxis
import java.util.Calendar
import java.text.SimpleDateFormat
import org.jfree.chart.axis.DateAxis
import java.util.Date
import com.jlp.scaviewer.ui._
import com.jlp.scaviewer.ui.MyTable._
import javax.swing.table.TableColumn
import com.jlp.scaviewerdyn.ui.ScalaChartingDyn
import com.jlp.scaviewerdyn.ui.actors.MyMessage
import javax.swing.JOptionPane
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.data.time.TimeSeries
import org.jfree.chart.plot.XYPlot
import scala.swing.event.MouseEvent
import java.awt.Color
import java.awt.BasicStroke
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.text.NumberFormat
import java.util.Locale
import java.awt.geom.Ellipse2D
import org.jfree.chart.labels.StandardXYToolTipGenerator
import scala.language.postfixOps
import java.awt.Font

class ScaChartingListener extends Component {

  listenTo(ScaCharting.bRefresh)
  listenTo(ScaCharting.bClear)
  listenTo(ScaCharting.bSample)
  listenTo(ScaCharting.tfSample)
  listenTo(ScaCharting.cbStrategie.selection)
  listenTo(ScaCharting.bCompare)
  listenTo(ScaCharting.bComparePlus)
  listenTo(ScaCharting.rbShortName)
  reactions += {
    case ButtonClicked(ScaCharting.bCompare) =>
      {

        println("bCompare clicke")
        // On verifie qu'il y a une table non vide
        var rows = ScaCharting.myTable.tabModel.getRowCount()
        if (rows == 0) {
          JOptionPane.showMessageDialog(null, "There is no shown series ", "ScaViewer", JOptionPane.ERROR_MESSAGE)
        } else {

          ScaChartingListener.markedSeries = Nil
          for (row <- 0 until rows) {
            //  println (ScaCharting.myTable.tabModel.getValueAt(row, ScaCharting.myTable.table.getColumnModel().getColumnIndex("marked")))
            if (true == ScaCharting.myTable.tabModel.getValueAt(row, ScaCharting.myTable.table.getColumnModel().getColumnIndex("marked"))) {
              ScaChartingListener.markedSeries = (row, ScaCharting.myTable.tabModel.getValueAt(row, ScaCharting.myTable.table.getColumnModel().getColumnIndex("name")).asInstanceOf[String]) :: ScaChartingListener.markedSeries
            }
          }
          if (ScaChartingListener.markedSeries.size < 2) {
            JOptionPane.showMessageDialog(null, "At least 2 series must be \"marked\" ", "ScaViewer", JOptionPane.ERROR_MESSAGE)
            ScaChartingListener.boolDecalPlus = false
            ScaChartingListener.boolDecalMoins = false
          } else {
            ScaChartingListener.boolDecalPlus = false
            ScaChartingListener.boolDecalMoins = true
            lancerDecalage((ScaChartingListener.markedSeries map { tup => tup._1 }) toList, "moins")
          }
        }

      }
    case ButtonClicked(ScaCharting.bComparePlus) =>
      {
        ScaChartingListener.markedSeries = Nil
        // On verifie qu'il y a une table non vide
        var rows = ScaCharting.myTable.tabModel.getRowCount()
        if (rows == 0) {
          JOptionPane.showMessageDialog(null, "There is no shown series ", "ScaViewer", JOptionPane.ERROR_MESSAGE)
        } else {

          for (row <- 0 until rows) {
            //  println (ScaCharting.myTable.tabModel.getValueAt(row, ScaCharting.myTable.table.getColumnModel().getColumnIndex("marked")))
            if (true == ScaCharting.myTable.tabModel.getValueAt(row, ScaCharting.myTable.table.getColumnModel().getColumnIndex("marked"))) {
              ScaChartingListener.markedSeries = (row, ScaCharting.myTable.tabModel.getValueAt(row, ScaCharting.myTable.table.getColumnModel().getColumnIndex("name")).asInstanceOf[String]) :: ScaChartingListener.markedSeries
            }
          }
          if (ScaChartingListener.markedSeries.size < 2) {
            JOptionPane.showMessageDialog(null, "At least 2 series must be \"marked\" ", "ScaViewer", JOptionPane.ERROR_MESSAGE)
            ScaChartingListener.boolDecalPlus = false
            ScaChartingListener.boolDecalMoins = false
          } else {
            ScaChartingListener.boolDecalPlus = true
            ScaChartingListener.boolDecalMoins = false
            lancerDecalage((ScaChartingListener.markedSeries map { tup => tup._1 }) toList, "plus")
          }
        }

      }

    case ButtonClicked(ScaCharting.bSample) =>
      {
        // println("ScaCharting.bSample")
        ScaCharting.sampling = true
        ScalaChartingDyn.stop
        // recherche des mini et maxi de l axe des X 
        val plot = ScaCharting.chartPanel.getChart().getXYPlot()
        val xAxis: DateAxis = plot.getDomainAxis().asInstanceOf[DateAxis]

        val sdf: SimpleDateFormat = new SimpleDateFormat("yyyy/MM/dd:HH:mm:ss.SSS")
        val cal: Calendar = Calendar.getInstance()
        val dateMin: Date = xAxis.getMinimumDate()
        val dateMax: Date = xAxis.getMaximumDate()
        //  print("xAxis debut =" + sdf.format(xAxis.getMinimumDate()))

        //  println(" xAxis fin =" + sdf.format(xAxis.getMaximumDate()))
        // nettoyage du Chart: mais on garde la liste des fichiers

        ScaChartingListener.lightClear

       
          cal.setTime(dateMin);
      

        val minString = cal.getTimeInMillis().toString()
        
          cal.setTime(dateMax);
       

        val maxString = cal.getTimeInMillis().toString()
        var cca: CreateChartAndTable = new CreateChartAndTable(ScaCharting.listFiles)
        //println("ScaChartingListener Avant Appel  cca.createPanelWithBound" )

        //cca.createPanelWithBounds(minString, maxString, ScaCharting.sampling)
       
       
        if(ScaChartingListener.boolDecalPlus) {
          cca.createPanelWithBounds("0", maxString, true)
        }
        else if (ScaChartingListener.boolDecalMoins){
           cca.createPanelWithBounds("0", Long.MaxValue.toString(), true)
        }
        else
        {
            cca.createPanelWithBounds(minString, maxString, true)
        }
        // ScaCharting.sampling=false
        var order = 0
        var nb = ScaCharting.chartPanel.getChart.getXYPlot.getRangeAxisCount()

        for (i <- 0 until nb) {
          ScaCharting.chartPanel.getChart.getXYPlot.setRangeAxis(i, null)
        }

        ScaCharting.colForRangeAxis.restoreAllColors
        for (x <- CreateChartAndTable.mapDatasets) {
          if (x._2.getSeriesCount() > 0) {
            var axis: NumberAxis = new NumberAxis(x._1);
            var col: Color = ScaCharting.colForRangeAxis.pickColor()
            axis.setLabelPaint(col);
            axis.setAxisLinePaint(col);
            axis.setTickLabelPaint(col);
            var strokeAxis = new BasicStroke(1.0F, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
            axis.setAxisLineStroke(strokeAxis)
            axis.setAutoRangeIncludesZero(false);

            ScaCharting.chartPanel.getChart.getXYPlot.setRangeAxis(order, axis);
            //plot.setDomainAxis(order,axis)
            ScaCharting.chartPanel.getChart.getXYPlot.setDataset(order, x._2)
            ScaCharting.chartPanel.getChart.getXYPlot.mapDatasetToRangeAxis(order, order);
            //plot.mapDatasetToDomainAxis(order, order)
            order += 1
          }
        }

        //restituer les couleurs
        for (indexDataset <- 0 until ScaCharting.chartPanel.getChart.getXYPlot.getDatasetCount) {
          var renderer = ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(indexDataset).asInstanceOf[XYLineAndShapeRenderer]
          val stroke: BasicStroke = new BasicStroke(1.0F, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
          val nf: NumberFormat = NumberFormat.getInstance(Locale.ENGLISH);
          if (null != ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset)) {
            for (indexS <- 0 until ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].getSeriesCount()) {

              renderer.setSeriesStroke(indexS, stroke);
              renderer.setSeriesOutlineStroke(indexS, stroke);
              renderer.setSeriesShape(indexS, new Ellipse2D.Double(-2.0D, -2.0D, 4.0D, 4.0D));
              renderer.setSeriesShapesVisible(indexS, true);
              renderer.setSeriesShapesFilled(indexS, true);
              renderer.setDrawOutlines(true);
              renderer.setUseFillPaint(true);
              renderer.setSeriesFillPaint(indexS, Color.WHITE);
              renderer.setSeriesToolTipGenerator(indexS, new StandardXYToolTipGenerator("<html> {0}:<br/> ({1}, <b>{2})</b>" +
                "" + "</html>", new SimpleDateFormat("yyyy/MM/dd:HH:mm:ss.SSS"), nf))
          
              //  renderer.setSeriesPaint(indexS, table.getValueAt(table.convertRowIndexToView(sel(i)),              table.getColumn("color").getModelIndex()).asInstanceOf[Color])
              renderer.setSeriesPaint(indexS, ScaCharting.myTable.table.getValueAt(retrouverRow(ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].getSeries(indexS).getKey.toString),
                ScaCharting.myTable.table.getColumn("color").getModelIndex()).asInstanceOf[Color])

            }
          }
        }
        // JLP

        if (ScaChartingListener.markedSeries.size >= 2) {
          if (ScaChartingListener.boolDecalPlus) {

            lancerDecalageByName((ScaChartingListener.markedSeries map { tup => tup._2 }) toList, "plus")
          } else if (ScaChartingListener.boolDecalMoins) lancerDecalageByName((ScaChartingListener.markedSeries map { tup => tup._2 }) toList, "moins")

        }

      }

    case ButtonClicked(ScaCharting.bRefresh) => {
      ScaChartingListener.lightClear
      ScaChartingListener.boolDecalPlus = false
      ScaChartingListener.boolDecalMoins = false
     
      ScaChartingListener.markedSeries = Nil
      if (ScaCharting.dyn == true) {

        ScalaChartingDyn.boolExamine = true
        ScalaChartingDyn.start()
        if (!ScaCharting.listFiles.isEmpty)
          ScalaChartingDyn.observer ! new MyMessage(ScalaChartingDyn.actorAction)

      } else {
        if (!ScaCharting.listRaf.isEmpty) {
          ScaCharting.listRaf foreach ((raf) => if (null != raf) raf.close)
          ScaCharting.listRaf = List.empty
        }

      }
      ScaCharting.hiddenTs.clear()
      ScaCharting.sampling = false

      ScaCharting.arrEnrichised = new ArrayBuffer[StructTs]()

      val cca = new CreateChartAndTable(ScaCharting.listFiles)
      cca.createPanelWithBounds("0", Long.MaxValue.toString, false)
    }

    case ButtonClicked(ScaCharting.bClear) => {
      ScaChartingListener.boolDecalPlus = false
      ScaChartingListener.boolDecalMoins = false
     
      //  println("ScaCharting.bClear")
      ScalaChartingDyn.boolExamine = false
      ScaChartingListener.markedSeries = Nil
      ScaChartingListener.deapClear
    }

    case EditDone(ScaCharting.tfSample) => {
      //println("ScaCharting.tfSample=" + ScaCharting.tfSample.text)
      ScaCharting.rdbMaxPointsOrGap.tooltip = "<html> When checked, maximal number of points for series<br/>When unchecked, grouping by gap of " + ScaCharting.tfSample.text + " milliseconds </html>"
    }

    case SelectionChanged(ScaCharting.cbStrategie) => // println("ScaCharting.cbStrategie=" + ScaCharting.cbStrategie.selection.item)
    case ButtonClicked(ScaCharting.rbShortName) => ScaCharting.myTable.repaint()
  }

  private def lancerDecalage(markedSeries: List[Int], moinsPlus: String) =
    {
      // trouver la date minimum de debut parmis les N series

      if (moinsPlus == "moins") {
        val minDate =
          {
            var minDateTmp = Long.MaxValue
            markedSeries foreach { idSerie =>
              val (ds, ts) = retrouverDsEtTs(idSerie)
              if (ts.getDataItem(0).getPeriod().getFirstMillisecond() < minDateTmp) {
                minDateTmp = ts.getDataItem(0).getPeriod().getFirstMillisecond()
              }
            }
            minDateTmp
          }
        // on laance les decalages

        markedSeries foreach { idSerie =>
          val (ds, ts) = retrouverDsEtTs(idSerie)
          val decalageTmp: Long = ts.getDataItem(0).getPeriod().getFirstMillisecond() - minDate
         
          if (decalageTmp != 0) {
            new TranslateLong(-decalageTmp, idSerie).execute
          }

        }
      } else {
       // val minDate =
           val maxDate =
          {
            var maxDateTmp = 0L
            markedSeries foreach { idSerie =>
              val (ds, ts) = retrouverDsEtTs(idSerie)
             // if (ts.getDataItem(0).getPeriod().getFirstMillisecond() > minDateTmp) {
               if (ts.getDataItem(0).getPeriod().getLastMillisecond() > maxDateTmp) {
                maxDateTmp = ts.getDataItem(0).getPeriod().getLastMillisecond()
              }
            }
           maxDateTmp
          }
        // on laance les decalages

        markedSeries foreach { idSerie =>
          val (ds, ts) = retrouverDsEtTs(idSerie)
         // val decalageTmp: Long = ts.getDataItem(0).getPeriod().getFirstMillisecond() - minDate
          val decalageTmp: Long = maxDate - ts.getDataItem(0).getPeriod().getLastMillisecond() 
        
          if (decalageTmp != 0) {
            new TranslateLong(decalageTmp, idSerie).execute
          }

        }
      }

    }
  private def lancerDecalageByName(markedSeries: List[String], moinsPlus: String) =
    {
      // trouver la date minimum de debut parmis les N series
      if (moinsPlus == "moins") {
        val minDate =
          {
            var minDateTmp = Long.MaxValue
            markedSeries foreach { nameTs =>
              val (ds, ts, idSerie) = retrouverDsEtTsByName(nameTs)
              if (ts.getDataItem(0).getPeriod().getFirstMillisecond() < minDateTmp) {
                minDateTmp = ts.getDataItem(0).getPeriod().getFirstMillisecond()
              }
            }
            minDateTmp
          }
        // on laance les decalages

        markedSeries foreach { nameTs =>
          val (ds, ts, idSerie) = retrouverDsEtTsByName(nameTs)
          val decalageTmp: Long = ts.getDataItem(0).getPeriod().getFirstMillisecond() - minDate
          if (decalageTmp != 0) {
            new TranslateLong(-decalageTmp, idSerie).execute
          }

        }
      } else {
        val maxDate =
          {
            var maxDateTmp = 0L
            markedSeries foreach { nameTs =>
            
              val (ds, ts, idSerie) = retrouverDsEtTsByName(nameTs)
              
              if (ts.getDataItem(0).getPeriod().getLastMillisecond() > maxDateTmp) {
                maxDateTmp = ts.getDataItem(0).getPeriod().getLastMillisecond()
              }
            }
            maxDateTmp
          }
        // on laance les decalages

        markedSeries foreach { nameTs =>
          val (ds, ts, idSerie) = retrouverDsEtTsByName(nameTs)
          val decalageTmp: Long = maxDate - ts.getDataItem(0).getPeriod().getLastMillisecond() 
         
          if (decalageTmp != 0) {
            new TranslateLong(decalageTmp, idSerie).execute
          }

        }
      }
    }

  
  def retrouverRow(keyTs: String): Int =
    {
      var ret = -1

      for (j <- 0 until ScaCharting.myTable.table.getRowCount()) {
        if (ScaCharting.myTable.table.getValueAt(j, ScaCharting.myTable.table.getColumn("name").getModelIndex()).toString == keyTs) {

          ret = j
        }

      }

      ret
    }

  private def retrouverDsEtTs(row: Int): (TimeSeriesCollection, TimeSeries) =
    {

      val nameTs = ScaCharting.myTable.table.getValueAt(row, ScaCharting.myTable.table.getColumn("name").getModelIndex())
      // println("name=" + nameTs)
      val plot: XYPlot = ScaCharting.chartPanel.getChart().getXYPlot();
      val nbDataset = plot.getDatasetCount()
      var bool = true
      var bool1 = true
      var idxDs = 0
      var ts: TimeSeries = null
      var ds: TimeSeriesCollection = null
      while (bool && idxDs < nbDataset) {

        var dataset = plot.getDataset(idxDs)
        var nbTs = dataset.asInstanceOf[TimeSeriesCollection].getSeriesCount()
        var idTs = 0
        while (bool1 && idTs < nbTs) {

          //  println("nbTs=" + nbTs + " ; idTs=" + idTs)

          if (dataset.asInstanceOf[TimeSeriesCollection].getSeries(idTs).getKey().toString() == nameTs) {
            bool = false
            bool1 = false
            //println("key=" + dataset.asInstanceOf[TimeSeriesCollection].getSeries(idTs).getKey().toString() + " name=" + nameTs)
            ds = dataset.asInstanceOf[TimeSeriesCollection]
            ts = dataset.asInstanceOf[TimeSeriesCollection].getSeries(idTs)

          }
          idTs += 1

        }
        idxDs += 1
      }
      (ds, ts)
    }
  private def retrouverDsEtTsByName(nameTs: String): (TimeSeriesCollection, TimeSeries, Int) =
    {

      // val nameTs = ScaCharting.myTable.table.getValueAt(row, ScaCharting.myTable.table.getColumn("name").getModelIndex())
      // println("name=" + nameTs)
      var row: Int = {
        val rows = ScaCharting.myTable.table.getRowCount()
        var row = -1
        for (i <- 0 until rows) {
          if (nameTs == ScaCharting.myTable.table.getValueAt(i, ScaCharting.myTable.table.getColumn("name").getModelIndex())) {
            
            row = i
             ScaCharting.myTable.table.setValueAt(true, row, ScaCharting.myTable.table.getColumn("marked").getModelIndex())
             val col=ScaCharting.myTable.table.getColumn("marked").getModelIndex()
            if ( ScaCharting.myTable.table.getValueAt(row, ScaCharting.myTable.table.getColumn("shown").getModelIndex()).toString == "true") {
              //          println("Ts :" + table.getValueAt(row, table.getColumn("name").getModelIndex()))
              //          println("marked=" + table.getValueAt(row, col))
              var keyTs: String =ScaCharting.myTable. table.getValueAt(row, ScaCharting.myTable.table.getColumn("name").getModelIndex()).toString

              var (indexDataset, indexSeries) = retouverdatasetEtTs(keyTs)
              if (ScaCharting.myTable.table.getValueAt(row, col).toString == "true") {
                // true bold line, axis and row table
                ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(indexDataset).asInstanceOf[XYLineAndShapeRenderer].setSeriesStroke(indexSeries, new BasicStroke(2.5F))
                var axis = ScaCharting.chartPanel.getChart.getXYPlot.getRangeAxisForDataset(indexDataset)
                axis.setAxisLineStroke(new BasicStroke(2.5F))
                var labelAxisFont: Font = axis.getLabelFont()
                labelAxisFont = labelAxisFont.deriveFont(Font.BOLD)
                axis.setLabelFont(labelAxisFont)

                var tickFont = axis.getTickLabelFont()
                tickFont = tickFont.deriveFont(Font.BOLD)
                axis.setTickLabelFont(tickFont)

                // mark and bold row
                //  ScaCharting.myTable.table.setValueAt(true,row,  table.getColumn("marked").getModelIndex())

                ScaCharting.myTable.repaint()
              } else {
                // true bold line, axis and row table
                ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(indexDataset).asInstanceOf[XYLineAndShapeRenderer].setSeriesStroke(indexSeries, new BasicStroke(1.0F))
                var axis = ScaCharting.chartPanel.getChart.getXYPlot.getRangeAxisForDataset(indexDataset)
                axis.setAxisLineStroke(new BasicStroke(1.0F))
                var labelAxisFont: Font = axis.getLabelFont()
                labelAxisFont = labelAxisFont.deriveFont(Font.PLAIN)
                axis.setLabelFont(labelAxisFont)

                var tickFont = axis.getTickLabelFont()
                tickFont = tickFont.deriveFont(Font.PLAIN)
                axis.setTickLabelFont(tickFont)

                // mark and bold row

                ScaCharting.myTable.repaint()

              }
            }
          
          
          }
        }
        row
      }
      val plot: XYPlot = ScaCharting.chartPanel.getChart().getXYPlot();
      val nbDataset = plot.getDatasetCount()
      var bool = true
      var bool1 = true
      var idxDs = 0
      var ts: TimeSeries = null
      var ds: TimeSeriesCollection = null
      while (bool && idxDs < nbDataset) {

        var dataset = plot.getDataset(idxDs)
        var nbTs = dataset.asInstanceOf[TimeSeriesCollection].getSeriesCount()
        var idTs = 0
        while (bool1 && idTs < nbTs) {

          //  println("nbTs=" + nbTs + " ; idTs=" + idTs)

          if (dataset.asInstanceOf[TimeSeriesCollection].getSeries(idTs).getKey().toString() == nameTs) {
            bool = false
            bool1 = false
            //println("key=" + dataset.asInstanceOf[TimeSeriesCollection].getSeries(idTs).getKey().toString() + " name=" + nameTs)
            ds = dataset.asInstanceOf[TimeSeriesCollection]
            ts = dataset.asInstanceOf[TimeSeriesCollection].getSeries(idTs)

          }
          idTs += 1

        }
        idxDs += 1
      }
      (ds, ts, row)
    }
     def retouverdatasetEtTs(keyTs: String): (Int, Int) =
    {
      var icxDataset = -1
      var idxTs = 0

      var bool = true
      while (bool) {
        icxDataset += 1
        var dataset = ScaCharting.chartPanel.getChart.getXYPlot.getDataset(icxDataset)
        for (j <- 0 until dataset.getSeriesCount()) {
          if (dataset.getSeriesKey(j).toString == keyTs) {
            bool = false
            idxTs = j
          }
        }

      }

      (icxDataset, idxTs)
    }
}
object ScaChartingListener {
  var boolDecalPlus = false
  var boolDecalMoins = false
  var markedSeries: List[(Int, String)] = Nil
 
  def deapClear =
    {

      lightClear

      // ScaCharting(ScaCharting.root)
      ScaCharting.hiddenTs.clear()
      ScaCharting.sampling = false
      ScaCharting.listChartingInfo = List.empty
      ScaCharting.listFiles = List.empty
      ScaCharting.listPasInMillis = List.empty
      if (!ScaCharting.listRaf.isEmpty) {
        ScaCharting.listRaf foreach ((raf) => if (null != raf) raf.close)
        ScaCharting.listRaf = List.empty
      }
      val width = math.min(50, ScaCharting.tmpProps.getProperty("scaviewer.unitWidth").toInt)
      // sizing the columns
      val nbCols = ScaCharting.myTable.table.getColumnCount()

      for (i <- 0 until nbCols) {
        var col: TableColumn = ScaCharting.myTable.table.getColumnModel().getColumn(i)
        var name = ScaCharting.myTable.table.getColumnName(i)
        col.setResizable(true)
        col.setMinWidth(MyTable.sizeColumnInit.get(name).get._1)
        col.setPreferredWidth(MyTable.sizeColumnInit.get(name).get._2)
        col.setMaxWidth(MyTable.sizeColumnInit.get(name).get._3)

      }
      //ScaCharting.tmpProps.clear
    }
  def lightClear =
    {
      // nettoyage des Couleurs
      ScaCharting.colForLine.restoreAllColors()
      ScaCharting.colForRangeAxis.restoreAllColors()

      val plot = ScaCharting.chartPanel.getChart().getXYPlot()
      for (i <- 0 until plot.getRangeAxisCount()) {

        plot.setRangeAxis(i, null)
      }

      for (i <- 0 until plot.getDatasetCount()) {
        plot.setDataset(i, null)

      }
      plot.setDataset(null)
      for (i <- 0 until plot.getDomainAxisCount()) {
        plot.setDomainAxis(i, null)

      }

      ScaCharting.arrEnrichised = new ArrayBuffer[StructTs]()

      // nettoyage de la table

      var bool = true
      while (bool) {
        if (ScaCharting.myTable.tabModel.getRowCount() > 0) {
          // On supprime la premiere ligne
          ScaCharting.myTable.tabModel.removeRow(0)
        } else
          bool = false
      }
      // println("ScaCharting.myTable.table.getRowCount="+ScaCharting.myTable.tabModel.getRowCount())
      ScaCharting.myTable.table.setRowSorter(null)
      // ScaCharting.myTable=new MyTable()
      ScaCharting.arrEnrichised = new ArrayBuffer[StructTs]()

      // Fin nettoyage
    }

}