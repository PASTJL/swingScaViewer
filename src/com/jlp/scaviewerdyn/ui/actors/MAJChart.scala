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
package com.jlp.scaviewerdyn.ui.actors
import java.io.File
import com.jlp.scaviewer.ui.ScaCharting
import java.io.LineNumberReader
import java.io.FileReader
import java.io.RandomAccessFile
import java.io.FileNotFoundException
import scala.collection.mutable.ArrayBuffer
import java.io.IOException
import com.jlp.scaviewer.timeseries.StructTs
import org.jfree.data.time.TimeSeriesCollection
import com.jlp.scaviewer.commons.utils.Unites
import com.jlp.scaviewer.timeseries.MyTimeSeriesItem
import org.jfree.data.time.Millisecond
import java.text.DecimalFormatSymbols
import java.util.Locale
import java.text.DecimalFormat
import java.util.Calendar
import com.jlp.scaviewer.ui.Percent
import org.jfree.data.time.TimeSeries
import scala.collection.mutable.Buffer
import com.jlp.scaviewer.csvutils.CsvFileInMemoryNew
import com.jlp.scaviewer.timeseries.TimeSeriesCollectionFromCsvInMemoryNew
import scala.collection.JavaConverters._

case class MAJChart(file: File, idx: Int) {

  def execute() =
    {
      var chartingInfo = ScaCharting.listChartingInfo(idx)
    //  var pasInMillis = ScaCharting.listPasInMillis(idx)
      var oldLen = ScaCharting.listChartingInfo(idx).lenFile
      var newLen = file.length()
      // constitution du nom de fichier_StrategyNbPoints
      var prefixName = file.getAbsolutePath() + "_" + chartingInfo.strategy + chartingInfo.nbPoints

      val firstLine = ScaCharting.listChartingInfo(idx).title
      var arrBuf: ArrayBuffer[String] = new ArrayBuffer[String]()
      arrBuf += firstLine
      var raf: RandomAccessFile = null
      var count = 0
      
      try {
        var bool = true

        while (bool) {
          var line = ScaCharting.listRaf(idx).readLine()
          //  println("OldLen="+oldLen+" ; newLen="+newLen+" ;line="+line)
          if (null != line) {
            if (line.length() > 10) {
              //  println("line="+line)
              arrBuf += line
              count += 1
            }
          } else bool = false
        }
      } catch {
        case (e: FileNotFoundException) => e.printStackTrace()
        case (e: IOException) => e.printStackTrace()
      }

   
      if (count >= 1) {
        // println("on met a jour "+count+" lines")
        ScaCharting.listChartingInfo(idx).lastModified = file.lastModified()
        ScaCharting.listChartingInfo(idx).lenFile = file.length()
     
        val deb0 = System.currentTimeMillis

        val csvFile1 = CsvFileInMemoryNew(arrBuf, file.getAbsolutePath(), true,idx)

        //    //println(" pivotsArrayValues=="+csvFile1.pivotsArrayValues)

        val tsC = TimeSeriesCollectionFromCsvInMemoryNew(csvFile1)

        //
        //tsC.createAllTimeSeriesOnly(true, false, ScaCharting.listPasInMillis(idx), 0.toString, Long.MaxValue.toString, ScaCharting.listChartingInfo(idx).strategy)
        tsC.createAllTimeSeriesOnly(true, true, ScaCharting.listChartingInfo(idx).nbPoints.toLong, 0.toString, Long.MaxValue.toString, ScaCharting.listChartingInfo(idx).strategy)

        // println("MAJChart tsC.enrichedArrTimeSeries="+tsC.enrichedArrTimeSeries)
        for (strucTs <- tsC.enrichedArrTimeSeries) {
          var nameTs = strucTs.ts.getKey().toString
          var nameTsSansUnite = tsC.supprimerunite(nameTs)
          // Trouver la serie dans la table
          var bool = true
          for (row <- 0 until ScaCharting.myTable.table.getRowCount(); if (bool)) {

            if (tsC.supprimerunite(ScaCharting.myTable.table.getValueAt(row, ScaCharting.myTable.table.getColumn("name").getModelIndex()).toString()) == nameTsSansUnite) {
              // Recuperer la TimeSeries dans le dataset

              //               println("\n cela fite => nom dans table :"+tsC.supprimerunite(ScaCharting.myTable.table.getValueAt(row,ScaCharting.myTable.table.getColumn("name").getModelIndex()).toString())+
              //                " nom local="+nameTsSansUnite+"\n")
              bool = false
              var group: String = ScaCharting.myTable.table.getValueAt(row, ScaCharting.myTable.table.getColumn("scale").getModelIndex()).toString()

              var (icxDataset, idts) = retouverdatasetEtTs(ScaCharting.myTable.table.getValueAt(row, ScaCharting.myTable.table.getColumn("name").getModelIndex()).toString())

              val dataset = ScaCharting.chartPanel.getChart.getXYPlot.getDataset(icxDataset)

              var ts = dataset.asInstanceOf[TimeSeriesCollection].getSeries(idts)

              var (mult, from, to) = Unites.convert(strucTs.unite, group)

              val dataItems = strucTs.ts.getItems().asInstanceOf[java.util.List[MyTimeSeriesItem]]
              val len = dataItems.size()
              for (i <- 0 until len) {
                var dataItem = dataItems.get(i)
                var newDataItem: MyTimeSeriesItem = MyTimeSeriesItem(dataItem.getPeriod().asInstanceOf[Millisecond], dataItem.getValue().doubleValue() * mult, dataItem.count, dataItem.max * mult)

                ts.addOrUpdate(newDataItem)
              }

              //ScaCharting.chartPanel.getChart.getXYPlot.setDataset(icxDataset,dataset)

            }
          }
        }
        refreshTableNew(0L, 1988562006823L)
        //       val ttsc = new TransformTsCollectionInMemory(tsC)
        //        println("duree creation ttsc TransformTsCollectionInMemory(tsC)=" + (System.currentTimeMillis - deb0))
        //      //
        //        // creer le tableau des cibles
        //        
        //         ttsc.normalizeTs()
        //         // A modifier pour trouver les bonnes valeurs
        //         
        //           println("duree creation ttsc  ttsc.normalizeTs())=" + (System.currentTimeMillis - deb0))
        //        val tab = (ttsc.mapByGroupUnit flatMap (_._2)).asInstanceOf[ArrayBuffer[StructTs]]
        //          var datasets=ttsc.createDatasets(tab)
        //           println("duree creation datasets=" + (System.currentTimeMillis - deb0))
      }
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
        var (avg, avgPond, min, max, stdv, irslope, countPts, countVal, sumPond, sum, maxMax) = calculNewRow(tsBis)
        // mise a jour de la table
        // retrouver la ligne avec le nom de la TS
        var row = retrouverRow(ts.getKey().toString)
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

    

    //  var lst: Buffer[MyTimeSeriesItem] = asScalaBuffer(ts.getItems.asInstanceOf[java.util.List[MyTimeSeriesItem]])
var lst=ts.getItems.asInstanceOf[java.util.List[MyTimeSeriesItem]]
      for (value <- lst.asScala) {
        sumCarre += scala.math.pow((value.getValue.doubleValue - avg), 2)
        // println(ts.getKey + " value=" + value.getValue.doubleValue)
      }
      stdDev = scala.math.sqrt(sumCarre / (countVal - 1))

      (avg, avgPond, xmin, xmax, stdDev, irslope, countVal, countAll, sum, sumPond, xmaxMax)
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

