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
import org.jfree.chart.plot.XYPlot
import com.jlp.scaviewer.ui.ScaCharting
import org.jfree.data.time.TimeSeriesCollection
import org.jfree.data.time.TimeSeries
import org.jfree.chart.axis.DateAxis
import org.jfree.data.time.Millisecond
import java.util.Calendar
import com.jlp.scaviewer.timeseries.MyTimeSeriesItem
import org.jfree.data.general.SeriesChangeListener
import org.jfree.data.general.SeriesChangeEvent
import org.jfree.data.general.DatasetChangeListener
import org.jfree.data.general.DatasetChangeEvent

case class Translate(percentOld:Int,percent: Int, row: Int) {
  val plot: XYPlot = ScaCharting.chartPanel.getChart().getXYPlot();
  var indexDs = -1
  var indexTs = -1
  var dsCurrent: TimeSeriesCollection = null
  def execute() {

    var (ds, ts) = retrouverDsEtTs(row)
    dsCurrent = ds
    val per=percent-percentOld
    val newts = decal(ts,percentOld, per)
    var arrTs:Array[TimeSeries]= new Array(ds.getSeriesCount)
    for(i <- 0 until arrTs.length)
    {
      arrTs(i)=ds.getSeries(i).clone().asInstanceOf[TimeSeries]
    }
  
   plot.getDataset(indexDs).asInstanceOf[TimeSeriesCollection].removeAllSeries()
    
  //  plot.getDataset(indexDs).asInstanceOf[TimeSeriesCollection].removeSeries(indexTs)
   //  plot.getDataset(indexDs).asInstanceOf[TimeSeriesCollection].addSeries(newts)
   
    for(i <- 0 until  arrTs.length )
    {
     
      if (i ==  indexTs )
          plot.getDataset(indexDs).asInstanceOf[TimeSeriesCollection].addSeries(newts)
         else  plot.getDataset(indexDs).asInstanceOf[TimeSeriesCollection].addSeries( arrTs(i))
    }
    
    
   
   //plot.setDataset(indexDs,ds)
    
    plot.mapDatasetToRangeAxis(indexDs, indexDs);

    ScaCharting.chartPanel.getChart().fireChartChanged()
    ScaCharting.chartPanel.repaint()

  }
 
  private def decal(ts: TimeSeries, percentOld:Int,per: Int): TimeSeries =
    {

      var ret: TimeSeries = new TimeSeries(ts.getKey())
      var intervalle: Long = (plot.getDomainAxis(0).asInstanceOf[DateAxis].getUpperBound() - plot.getDomainAxis(0).asInstanceOf[DateAxis].getLowerBound()).toLong
     // println("intervalle=" + intervalle +" percentOld ="+percentOld+" percent="+per)
      
      val gapTranslate: Long = intervalle * per / 100
      var len = ts.getItemCount()
     

      
      var cal: Calendar = Calendar.getInstance

      var periodDecal: Long = 0L
      for (i <- 0 until len) {

        periodDecal = ts.getTimePeriod(i).asInstanceOf[Millisecond].getLastMillisecond() + gapTranslate * (per-percentOld)/100
     //   if (periodDecal < plot.getDomainAxis(0).asInstanceOf[DateAxis].getUpperBound().toLong) {
          // println("translate timeperiod ="+i)
          cal.setTimeInMillis(ts.getTimePeriod(i).asInstanceOf[Millisecond].getLastMillisecond() + gapTranslate)
          ret.add(MyTimeSeriesItem(new Millisecond(cal.getTime()), ts.getDataItem(i).asInstanceOf[MyTimeSeriesItem].getValue().doubleValue(), ts.getDataItem(i).asInstanceOf[MyTimeSeriesItem].count, ts.getDataItem(i).asInstanceOf[MyTimeSeriesItem].max))
      //  } else bool = false

      }

      ret

    }

  private def retrouverDsEtTs(row: Int): (TimeSeriesCollection, TimeSeries) =
    {

      val nameTs = ScaCharting.myTable.table.getValueAt(row, ScaCharting.myTable.table.getColumn("name").getModelIndex())
     // println("name=" + nameTs)
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
            indexDs = idxDs
            indexTs = idTs
          }
          idTs += 1

        }
        idxDs += 1
      }
      (ds, ts)
    }

}

