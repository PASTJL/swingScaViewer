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
import org.jfree.chart.ChartPanel
import java.awt.event.MouseListener
import org.jfree.chart.ChartMouseListener
import org.jfree.chart.ChartMouseEvent
import java.awt.geom.Point2D
import java.awt.geom.Rectangle2D
import org.jfree.chart.plot.XYPlot
import java.awt.event.MouseEvent
import org.jfree.data.general.Series
import org.jfree.data.general.Dataset
import com.jlp.scaviewer.ui.ScaCharting
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.Millisecond
import java.util.Calendar
import scala.collection.JavaConverters._
import java.awt.event.InputEvent
import org.jfree.chart.renderer.xy.XYItemRenderer
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.BasicStroke
import java.awt.Font
import javax.swing.JTextField
import javax.swing.JLabel
import com.jlp.scaviewer.ui.MyDefaultCellRenderer

class Distances extends ChartMouseListener {

  ScaCharting.chartPanel.addChartMouseListener(this)
  def retrieveNearestTimePeriod(ts: TimeSeries, millis: Long): Millisecond =
    {

      var cal: Calendar = Calendar.getInstance

      var timePeriod1 = 0L
      var timePeriod2 = 0L
      var bool = true
      val periods: java.util.Collection[Millisecond] = ts.getTimePeriods.asInstanceOf[java.util.Collection[Millisecond]]
      var previousPeriod: Millisecond = null
      // println("debut millis=" + millis + " traitement de ts=" + ts.getKey + " avec :" + periods.size() + " periods")
      val iter = periods.iterator()
      while (bool && iter.hasNext()) {
        //  println ("coucou millis="+millis)
        var period = iter.next()

        // print ("period="+period+ " traduction Last Milliseconde="+period.getLastMillisecond())
        // println("  ;traduction Millissecond :"+period.getMillisecond())
        if (bool && period.getLastMillisecond > millis) {

          timePeriod1 = period.getLastMillisecond
          //  println ("trouve millis="+millis + "  ;timePeriod1="+timePeriod1)
          if (null != previousPeriod) {
            timePeriod2 = previousPeriod.getLastMillisecond
            // println("trouve millis=" + millis + "  ;timePeriod1=" + timePeriod1 + " previuos=" + previousPeriod.getLastMillisecond)
          } else {
            timePeriod2 = timePeriod1
          }
          bool = false
        }
        previousPeriod = period
      }
      //      cal.setTimeInMillis(millis)
      //      println("date millis=" + cal.getTime())
      if (timePeriod1 == 0L) {
        // pas moyen d'atteindre une valeur
        cal.setTimeInMillis(0)
        new Millisecond(cal.getTime())
      } else if ((timePeriod1 - millis).abs < (timePeriod2 - millis).abs) {

        cal.setTimeInMillis(timePeriod1)
        new Millisecond(cal.getTime())
      } else {
        cal.setTimeInMillis(timePeriod2)
        new Millisecond(cal.getTime())
      }

    }

  def retrieveMinDistance(event: ChartMouseEvent): (Dataset, Int, Int, Double) =
    {

      // val p: Point2D = cp.translateScreenToJava2D(event.getTrigger().getPoint());
      val p: Point2D = event.getTrigger().getPoint();
      val plotArea: Rectangle2D = ScaCharting.chartPanel.getScreenDataArea();
      val plot: XYPlot = ScaCharting.chartPanel.getChart().getPlot().asInstanceOf[XYPlot]; // your plot
      var dist = Double.MaxValue
      var datasetCandidate: TimeSeriesCollection = null
      var indexOfTimeSeries: Int = -1
      // println("chartX double=" + plot.getDomainAxis(0).java2DToValue(p.getX(), plotArea, plot.getDomainAxisEdge(0)))
      // var chartX: Long = plot.getDomainAxis(0).java2DToValue(p.getX(), plotArea, plot.getDomainAxisEdge(0)).toLong;
      if (null != plot.getDomainAxis(0)) {
        var chartX: Long = plot.getDomainAxis(0).java2DToValue(p.getX(), plotArea, plot.getDomainAxisEdge(0)).toLong;
        var numdataset = 0
        var numdatasetCandidate = 0
        //println("Distances nombre de dataset trait�s ="+plot.getRangeAxisCount+ " ScaCharting.stepMillis="+ScaCharting.stepMillis)
        for (i <- 0 until plot.getRangeAxisCount) {

          var chartY: Double = plot.getRangeAxis(i).java2DToValue(p.getY(), plotArea, plot.getRangeAxisEdge(i));
          var lowerBound = plot.getRangeAxis(i).getLowerBound()
          // println("Axis getLowerBound =" + plot.getRangeAxis(i).getLowerBound())
          // if (ScaCharting.rdbTimeSeries.selected) {
          if (true) {
            var dataset: TimeSeriesCollection = plot.getDataset(i).asInstanceOf[TimeSeriesCollection]

            for (j <- 0 until dataset.getSeriesCount()) {
              var ts: TimeSeries = dataset.getSeries(j).asInstanceOf[TimeSeries]
              // trouver l'absice la plus proch
              // println("dataset en traitement :"+dataset.getSeries(j).getKey)
              // var millis: Long = (chartX.toLong / ScaCharting.stepMillis) * ScaCharting.stepMillis
              var millis: Long = chartX
              var bool: Boolean = true
              var period = retrieveNearestTimePeriod(ts, chartX)
              // println("period=" + period.getLastMillisecond())
              if (period.getLastMillisecond() > chartX) {
                var dist1 = ((ts.getDataItem(period).getValue().doubleValue() - chartY) / (chartY - lowerBound)).abs
                //            println("chartY=" + chartY + " Value de la serie " + ts.getKey + " =" + ts.getDataItem(period).getValue().doubleValue() + " periodFirst=" + period.getFirstMillisecond()
                //              + "periodLast=" + period.getLastMillisecond() + " dist1=" + dist1)
                if (dist1 < dist) {
                  dist = dist1
                  numdatasetCandidate = numdataset
                  indexOfTimeSeries = j
                  datasetCandidate = dataset
                }
              }
            }
          } else {
            // A faire pour XYSeries
          }
          //  println("Traitement dataset numero=" + numdataset + " dist=" + dist)
          numdataset += 1
        }

        //      println("Timeseies to bold =" + numdatasetCandidate)
        //      print(" indexOfTs=" + indexOfTimeSeries + " => ")
        //      print(datasetCandidate.getSeries(indexOfTimeSeries).getKey)
        //      print(" numDataset=" + numdatasetCandidate)
        //      println(" , distRelative=" + dist)
        (datasetCandidate, numdatasetCandidate, indexOfTimeSeries, dist)
      } else (null, -1, -1, 0)
    }

  def chartMouseClicked(event: ChartMouseEvent) {
    if (event.getTrigger.getButton() == (MouseEvent.BUTTON1)) {
      //      println("modifier ="+event.getTrigger.getModifiers())
      //      println("nom event ="+event.getTrigger.paramString())
      //      println("nId event ="+event.getTrigger.getID)
      val (dataset, indexDataset, indexSeries, dist) = retrieveMinDistance(event)
      if (null != dataset) {
        if (dist <= ScaCharting.tmpProps.getProperty("scaviewer.distrelative").toDouble) {
          // modifier le marquage de la ligne de la table
          // nom de la serie
          var nameSeries: String = ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].getSeries(indexSeries).getKey.asInstanceOf[String]
          var nbColumn: Int = ScaCharting.myTable.table.getColumn("name").getModelIndex()
          var nbLigne = -1
          var bool = true
          while (bool) {
            nbLigne += 1
            if (nameSeries == ScaCharting.myTable.table.getValueAt(nbLigne, nbColumn).toString) {
              bool = false
            }

          }
          var nbColumnMark: Int = ScaCharting.myTable.table.getColumn("marked").getModelIndex()
          //println("ScaCharting.myTable.table.getValueAt(0,3).getClass.toString="+ScaCharting.myTable.table.getValueAt(nbLigne,3).getClass.toString)
          //println("ScaCharting.myTable.table.getCellEditor(nbLigne,3).getClass="+ScaCharting.myTable.table.getCellEditor(nbLigne,3).getClass)
          var font = ScaCharting.myTable.table.getCellRenderer(nbLigne, 3).asInstanceOf[javax.swing.table.DefaultTableCellRenderer].asInstanceOf[JLabel].getFont
          var lstColumnString: List[Int] = List.empty
          var ii = 0
          for (i <- 0 until ScaCharting.myTable.table.getColumnCount()) {
            if (ScaCharting.myTable.table.getValueAt(0, i).getClass.toString == "class java.lang.String") {
              lstColumnString = ii :: lstColumnString
            }
            ii += 1
          }
          lstColumnString = lstColumnString.reverse
          //  println(" lstColumnString="+ lstColumnString)
          //        println("Application de la modification")
          //        println("taille du stroke =" + ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(indexDataset).asInstanceOf[XYLineAndShapeRenderer].getSeriesStroke(indexSeries).asInstanceOf[BasicStroke].getLineWidth)
          if (ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(indexDataset).asInstanceOf[XYLineAndShapeRenderer].getSeriesStroke(indexSeries).asInstanceOf[BasicStroke].getLineWidth == 2.5F) {
            ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(indexDataset).asInstanceOf[XYLineAndShapeRenderer].setSeriesStroke(indexSeries, new BasicStroke(1.0F))
            var axis = ScaCharting.chartPanel.getChart.getXYPlot.getRangeAxisForDataset(indexDataset)
            axis.setAxisLineStroke(new BasicStroke(1.0F))
            var labelAxisFont: Font = axis.getLabelFont()
            labelAxisFont = labelAxisFont.deriveFont(Font.PLAIN)
            axis.setLabelFont(labelAxisFont)

            var tickFont = axis.getTickLabelFont()
            tickFont = tickFont.deriveFont(Font.PLAIN)
            axis.setTickLabelFont(tickFont)
            // unmark and Plain row
            ScaCharting.myTable.table.setValueAt(false, nbLigne, nbColumnMark)

            ScaCharting.myTable.repaint()

          } else {
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
            ScaCharting.myTable.table.setValueAt(true, nbLigne, nbColumnMark)

            ScaCharting.myTable.repaint()
          }
        }
      }
    }
  }

  def chartMouseMoved(event: ChartMouseEvent) {}

}