package com.jlp.scaviewer.ui.tableandchart

import org.jfree.chart.plot.XYPlot
import org.jfree.data.time.TimeSeriesCollection
import com.jlp.scaviewer.ui.ScaCharting
import org.jfree.data.time.TimeSeries
import java.util.Calendar
import org.jfree.data.time.Millisecond
import com.jlp.scaviewer.timeseries.MyTimeSeriesItem
case class TranslateLong(decal: Long, row: Int) {
  val plot: XYPlot = ScaCharting.chartPanel.getChart().getXYPlot();
  var indexDs = -1
  var indexTs = -1
  var dsCurrent: TimeSeriesCollection = null
  def execute() {
    var (ds, ts) = retrouverDsEtTs(row)
    dsCurrent = ds
    val newts = decalage(ts, decal)
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

  private def decalage(ts: TimeSeries, decal: Long): TimeSeries =
    {
      var ret: TimeSeries = new TimeSeries(ts.getKey())
  		var len = ts.getItemCount()
  		var cal: Calendar = Calendar.getInstance
  		for (i <- 0 until len) {
  		  
  		  cal.setTimeInMillis(ts.getTimePeriod(i).asInstanceOf[Millisecond].getLastMillisecond() +decal)
  		   ret.add(MyTimeSeriesItem(new Millisecond(cal.getTime()), ts.getDataItem(i).asInstanceOf[MyTimeSeriesItem].getValue().doubleValue(), ts.getDataItem(i).asInstanceOf[MyTimeSeriesItem].count, ts.getDataItem(i).asInstanceOf[MyTimeSeriesItem].max))
  		}
  		
      ret

    }
  private  def retrouverDsEtTs(row: Int): (TimeSeriesCollection, TimeSeries) =
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