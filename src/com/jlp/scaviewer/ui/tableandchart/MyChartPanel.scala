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
import java.awt.event.MouseEvent
import com.jlp.scaviewer.ui._
import org.jfree.chart.axis._
import scala.collection.mutable._
import com.jlp.scaviewer.timeseries._
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.data.time.Millisecond
import java.util.Calendar
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.util.Locale
import com.jlp.scaviewerdyn.ui.ScalaChartingDyn
import com.jlp.scaviewerdyn.ui.actors.MyMessage
import scala.collection.JavaConverters._


trait MyChartPanel extends ChartPanel {
  private var oldLowerRange = Long.MaxValue
  private var oldUpperRange = 0L

  private var xAxis: Long = 0L
  private var currentUpper = 0l

  override def mousePressed(event: MouseEvent) {
    super.mousePressed(event)
    xAxis = event.getX()
  }
  override def mouseReleased(event: MouseEvent) {
    super.mouseReleased(event)
     if (null != event.getSource().asInstanceOf[ChartPanel].getChart().getXYPlot().getDomainAxis()) {
    //  println("Mouse released")
    var releasedLower = event.getSource().asInstanceOf[ChartPanel].getChart().getXYPlot().getDomainAxis().getRange().asInstanceOf[org.jfree.data.time.DateRange].getLowerMillis
    var releasedUpper = event.getSource().asInstanceOf[ChartPanel].getChart().getXYPlot().getDomainAxis().getRange().asInstanceOf[org.jfree.data.time.DateRange].getUpperMillis
    if (event.getX() > xAxis) {
      refreshTableNew(releasedLower, releasedUpper)
      if (ScaCharting.dyn) {
        ScalaChartingDyn.stop

      }
    } else if (event.getX() < xAxis - 10) {
      // recuperer la table initiale
      // DateFin =>2033/01/05:19:20:06.823
      refreshTableNew(0L, 1988562006823L)

      if (ScaCharting.dyn) {
        ScalaChartingDyn.stop
        ScalaChartingDyn.start
        ScalaChartingDyn.observer ! new MyMessage(ScalaChartingDyn.actorAction)
      }
    }

    }
    //    println("nouveau domaine:" + event.getSource().asInstanceOf[ChartPanel].getChart().getXYPlot().getDomainAxis().getRange().asInstanceOf[org.jfree.data.time.DateRange].getLowerMillis +
    //      " -> " + event.getSource().asInstanceOf[ChartPanel].getChart().getXYPlot().getDomainAxis().getRange().asInstanceOf[org.jfree.data.time.DateRange].getUpperMillis())

  }

  def refreshTableNew(lowerMillis: Long, upperMillis: Long) {
    // recuperer les datasets
    var dfs = new DecimalFormatSymbols(Locale.ENGLISH)
    dfs.setExponentSeparator(" 10^")
    val df: DecimalFormat = new DecimalFormat(ScaCharting.tmpProps.getProperty("scaviewer.df"), dfs)

    val plot = ScaCharting.chartPanel.getChart().getXYPlot()
    var cal1: Calendar = Calendar.getInstance
    var cal2: Calendar = Calendar.getInstance
    cal1.setTimeInMillis(lowerMillis)
    cal2.setTimeInMillis(upperMillis)
    val start = cal1.getTime()
    val stop = cal2.getTime()
    for (i <- 0 until plot.getDatasetCount()) {
      var dataset = plot.getDataset(i)

      for (j <- 0 until dataset.getSeriesCount()) {

        var ts = dataset.asInstanceOf[TimeSeriesCollection].getSeries(j)
        var tsBis = ts.createCopy(new Millisecond(start), new Millisecond(stop))
        var row = retrouverRow(ts.getKey().toString)
        if (tsBis.getItemCount() > 0) {
          var (avg, avgPond, min, max, stdv, irslope, countPts, countVal, sum, sumPond, maxMax) = calculNewRow(tsBis)
          // mise a jour de la table
          // retrouver la ligne avec le nom de la TS

          // mise a jour avg
          ScaCharting.myTable.table.setValueAt(avg, row, ScaCharting.myTable.table.getColumn("avg").getModelIndex())
          // mise a jour avgPond
          ScaCharting.myTable.table.setValueAt(avgPond, row, ScaCharting.myTable.table.getColumn("avgPond").getModelIndex())
          // mise a jour min
          ScaCharting.myTable.table.setValueAt(min, row, ScaCharting.myTable.table.getColumn("min").getModelIndex())
          // mise a jour max
          ScaCharting.myTable.table.setValueAt(max, row, ScaCharting.myTable.table.getColumn("max").getModelIndex())
          // mise a jour maxMax
          ScaCharting.myTable.table.setValueAt(maxMax, row, ScaCharting.myTable.table.getColumn("maxMax").getModelIndex())
          // mise a jour stdv
          ScaCharting.myTable.table.setValueAt(stdv, row, ScaCharting.myTable.table.getColumn("stdv").getModelIndex())
          // remise a 0 des sliders
          ScaCharting.myTable.table.setValueAt(new Percent(0), row, ScaCharting.myTable.table.getColumn("translate").getModelIndex())
          // mise a jour irslode
          ScaCharting.myTable.table.setValueAt(df.format(irslope * 3600 * 1000) + " " +
            ScaCharting.myTable.table.getValueAt(row, ScaCharting.myTable.table.getColumn("scale").getModelIndex()) + "/H", row, ScaCharting.myTable.table.getColumn("irslope").getModelIndex())

          //Mise a jour compteurs et sommes
          ScaCharting.myTable.table.setValueAt(countPts, row, ScaCharting.myTable.table.getColumn("countPts").getModelIndex())
          ScaCharting.myTable.table.setValueAt(countVal, row, ScaCharting.myTable.table.getColumn("countVal").getModelIndex())
          ScaCharting.myTable.table.setValueAt(sumPond, row, ScaCharting.myTable.table.getColumn("sumTotal").getModelIndex())
          ScaCharting.myTable.table.setValueAt(sum, row, ScaCharting.myTable.table.getColumn("sum").getModelIndex())
        } else {

          // mise a jour avg
          ScaCharting.myTable.table.setValueAt(Double.NaN, row, ScaCharting.myTable.table.getColumn("avg").getModelIndex())
          // mise a jour avgPond
          ScaCharting.myTable.table.setValueAt(Double.NaN, row, ScaCharting.myTable.table.getColumn("avgPond").getModelIndex())
          // mise a jour min
          ScaCharting.myTable.table.setValueAt(Double.NaN, row, ScaCharting.myTable.table.getColumn("min").getModelIndex())
          // mise a jour max
          ScaCharting.myTable.table.setValueAt(Double.NaN, row, ScaCharting.myTable.table.getColumn("max").getModelIndex())
          // mise a jour maxMax
          ScaCharting.myTable.table.setValueAt(Double.NaN, row, ScaCharting.myTable.table.getColumn("maxMax").getModelIndex())
          // mise a jour stdv
          ScaCharting.myTable.table.setValueAt(Double.NaN, row, ScaCharting.myTable.table.getColumn("stdv").getModelIndex())
          // remise a 0 des sliders
          ScaCharting.myTable.table.setValueAt(new Percent(0), row, ScaCharting.myTable.table.getColumn("translate").getModelIndex())
          // mise a jour irslode
          ScaCharting.myTable.table.setValueAt("", row, ScaCharting.myTable.table.getColumn("irslope").getModelIndex())

          //Mise a jour compteurs et sommes
          ScaCharting.myTable.table.setValueAt(Double.NaN, row, ScaCharting.myTable.table.getColumn("countPts").getModelIndex())
          ScaCharting.myTable.table.setValueAt(Double.NaN, row, ScaCharting.myTable.table.getColumn("countVal").getModelIndex())
          ScaCharting.myTable.table.setValueAt(Double.NaN, row, ScaCharting.myTable.table.getColumn("sumTotal").getModelIndex())
          ScaCharting.myTable.table.setValueAt(Double.NaN, row, ScaCharting.myTable.table.getColumn("sum").getModelIndex())
        }
      }
    }

  }

  def retrouverRow(tsName: String): Int =
    {
      var index = -1
      var bool = true

      while (bool && index < ScaCharting.myTable.table.getRowCount()) {
        index += 1
        if (tsName == ScaCharting.myTable.table.getValueAt(index, ScaCharting.myTable.table.getColumn("name").getModelIndex())) {
          bool = false
        }
      }
      index
    }

  def calculNewRow(ts: TimeSeries): (Double, Double, Double, Double, Double, Double, Long, Long, Double, Double, Double) =
    {
      // retour => (avg, avgPond, min, max, stdv, irslope)
      var sumPond: Double = 0
      var sum: Double = 0
      var countAll: Long = 0
      var countVal: Long = 0

      var moyenneX: Double = 0
      var sommeXCarre: Double = 0
      var prodXY: Double = 0
      var sommeX: Double = 0
      var xmin = Double.MaxValue
      var xmax = Double.MinValue
      var xmaxMax = Double.MinValue
      val nbItems = ts.getItemCount()
      val decalX: Long = ts.getTimePeriod(0).getLastMillisecond()
      // on decale l 'origine des X pour ne pas avoir de Pb de d�cimale
      for (i <- 0 until nbItems) {
        var myTimeSeriesItem = ts.getDataItem(i).asInstanceOf[MyTimeSeriesItem]

        sumPond += myTimeSeriesItem.getValue().doubleValue() * myTimeSeriesItem.count
        countAll += myTimeSeriesItem.count
        countVal += 1
        sommeX += myTimeSeriesItem.getPeriod().getLastMillisecond - decalX
        sum += myTimeSeriesItem.getValue().doubleValue()
        sommeXCarre += scala.math.pow(myTimeSeriesItem.getPeriod().getLastMillisecond() - decalX, 2)

        prodXY += (myTimeSeriesItem.getPeriod().getLastMillisecond - decalX) * myTimeSeriesItem.getValue().doubleValue()
        xmax = scala.math.max(xmax, myTimeSeriesItem.getValue().doubleValue())
        xmin = scala.math.min(xmin, myTimeSeriesItem.getValue().doubleValue())
        xmaxMax = scala.math.max(xmaxMax, myTimeSeriesItem.max)
      }
      var avgPond: Double = sumPond / countAll
      var avg = sum / countVal
      var stdDev: Double = 0
      var sumCarre: Double = 0
      moyenneX = sommeX / countVal

      var varX = (sommeXCarre / countVal) - scala.math.pow(moyenneX, 2)
      var coVarXY = (prodXY / countVal) - moyenneX * avg
      var irslope = coVarXY / varX

    

     // var lst: Buffer[MyTimeSeriesItem] = asScalaBuffer(ts.getItems.asInstanceOf[java.util.List[MyTimeSeriesItem]])
      var lst=ts.getItems.asInstanceOf[java.util.List[MyTimeSeriesItem]]

      for (value <- lst.asScala) {
        sumCarre += scala.math.pow((value.getValue.doubleValue - avg), 2)
        // println(ts.getKey + " value=" + value.getValue.doubleValue)
      }
      stdDev = scala.math.sqrt(sumCarre / (countVal - 1))

      (avg, avgPond, xmin, xmax, stdDev, irslope, countVal, countAll, sum, sumPond, xmaxMax)
    }

  def refreshTable(lowerMillis: Long, upperMillis: Long) {

    // on va faire un sampling avec un nombre de points egal
    ScaCharting.sampling = true
    // recherche des mini et maxi de l axe des X 
    val plot = ScaCharting.chartPanel.getChart().getXYPlot()
    val xAxis: DateAxis = plot.getDomainAxis().asInstanceOf[DateAxis]

    // nettoyage du Chart: mais on garde la liste des fichiers

    lightClear

    var cca: CreateChartAndTable = new CreateChartAndTable(ScaCharting.listFiles)
    cca.createPanelWithBounds(lowerMillis.toString(), upperMillis.toString, ScaCharting.sampling)
    ScaCharting.sampling = false

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