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

import com.jlp.scaviewer.commons.utils.Couleurs
import com.jlp.scaviewer.timeseries.GroupUnit
import java.awt.Color
import com.jlp.scaviewer.timeseries.StructTs
import org.jfree.chart.JFreeChart
import com.jlp.scaviewer.ui.MyTable
import org.jfree.chart.ChartPanel
import java.io.File
import org.jfree.data.time.TimeSeriesCollection
import com.jlp.scaviewer.ui.ScaCharting

import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.XYPlot
import java.awt.BasicStroke
import java.awt.RenderingHints
import org.jfree.chart.renderer.xy.XYItemRenderer
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.geom.Ellipse2D
import scala.collection.mutable.ArrayBuffer
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.labels.StandardXYToolTipGenerator
import java.text.SimpleDateFormat
import java.text.NumberFormat
import java.util.Locale
import org.jfree.data.time.TimeSeries
import javax.swing.JSlider
import com.jlp.scaviewer.ui.MyDefaultCellRenderer
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import com.jlp.scaviewer.ui.Percent
import com.jlp.scaviewer.timeseries.TimeSeriesCollectionFromCsvNew
import com.jlp.scaviewer.csvutils.CsvFileNew
import com.jlp.scaviewer.timeseries.TransformTsCollectionNew
import language.postfixOps

case class CreateChartAndTable(listFile: List[File]) {

  // creation d'une concatenation
  // println("Constructeur de CreateChartAndTable=" + ScaCharting.listFiles.toString())
  var mapDatasets = Map[String, TimeSeriesCollection]()
  var tabXYLineAndShapeRenderer: Array[XYLineAndShapeRenderer] = null

  def createTimeSeriesCollections(arrStructTs: ArrayBuffer[StructTs]): TimeSeriesCollection =
    {
      var tsC = new TimeSeriesCollection
      //      println("createTimeSeriesCollections ScaCharting.sampling="+ScaCharting.sampling)
      //       println("createTimeSeriesCollections ScaCharting.hiddenTs="+ScaCharting.hiddenTs.keySet)
      if (ScaCharting.sampling == true && !ScaCharting.hiddenTs.isEmpty) {

        var keys = ScaCharting.hiddenTs.keySet

        for (strucTs <- arrStructTs) {
          //          println("strucTs.ts.getKey.toString"+strucTs.ts.getKey.toString)
          if (!keys.contains(strucTs.ts.getKey.toString)) tsC.addSeries(strucTs.ts)
        }

      } else {
        for (strucTs <- arrStructTs) {
          tsC.addSeries(strucTs.ts)

          // Voir ici pour le Sampling
        }
      }
      tsC
    }
  def createDatasets(): Map[String, TimeSeriesCollection] =
    {

      ScaCharting.arrEnrichised groupBy (_.unite) map ((x) => ((x._1, createTimeSeriesCollections(x._2))))

    }

  def createChartPanel =
    {

      createPanelWithBounds("0", Long.MaxValue.toString, false)
    }

  def createPanelWithBounds(dateDebInMillis: String, dateFinInMillis: String, sampling: Boolean = false) =

    {
    println("sampling="+sampling)
      //println("Debut de CreateChartAndTable.createPanelWithBounds="+ScaCharting.listFiles.toString() )
      var numFile = 0
      for (file <- listFile) {

        //println("traitement file point 1=" + file)

        // test avec CsvFileNew
        //var csvFile = CsvFile(file, ScaCharting.rdbTimeSeries.selected)
        //  var csvFile = CsvFileNew(file, ScaCharting.rdbTimeSeries.selected)
        var csvFile = CsvFileNew(file, true)
        // 
        ScaCharting.listChartingInfo(numFile).title = csvFile.firstLine
        ScaCharting.listChartingInfo(numFile).nbItems=csvFile.nbColumns
        ScaCharting.listChartingInfo(numFile).posPivots=csvFile.pivots
        ScaCharting.listChartingInfo(numFile).posValues=csvFile.values
        ScaCharting.listChartingInfo(numFile).sep=csvFile.sepCourant
        ScaCharting.listChartingInfo(numFile).regexDate=csvFile.regexDateFormat._1
        ScaCharting.listChartingInfo(numFile).dateFormat=csvFile.regexDateFormat._2
        
        
        //println("traitement file point 2=" + file)
        // var deb=System.currentTimeMillis()		

        // Test avec New
        //var tsC = TimeSeriesCollectionFromCsv(csvFile)
        var tsC = TimeSeriesCollectionFromCsvNew(csvFile)

        // println("CreateChartAnadTable construction TimeSeriesCollectionFromCsv="+(System.currentTimeMillis()-deb	))
        // deb=System.currentTimeMillis()
        if (sampling) {

          tsC = tsC.createAllTimeSeries(ScaCharting.listChartingInfo(numFile).isTimeSeries, ScaCharting.listChartingInfo(numFile).isNbPointsMax, ScaCharting.tfSample.text.toInt, dateDebInMillis, dateFinInMillis, ScaCharting.listChartingInfo(numFile).strategy)
          // tsC = tsC.createAllTimeSeries(ScaCharting.listChartingInfo(numFile).isTimeSeries, ScaCharting.listChartingInfo(numFile).isNbPointsMax, ScaCharting.listChartingInfo(numFile).nbPoints, dateDebInMillis, dateFinInMillis, ScaCharting.listChartingInfo(numFile).strategy)
          // ScaCharting.tfSample.text = ScaCharting.listChartingInfo(numFile).nbPoints.toString
          numFile += 1

        } else {
          tsC = tsC.createAllTimeSeries(ScaCharting.listChartingInfo(numFile).isTimeSeries, ScaCharting.listChartingInfo(numFile).isNbPointsMax, ScaCharting.listChartingInfo(numFile).nbPoints, dateDebInMillis, dateFinInMillis, ScaCharting.listChartingInfo(numFile).strategy)
          ScaCharting.tfSample.text = ScaCharting.listChartingInfo(numFile).nbPoints.toString
          numFile += 1
        }
        // println("CreateChartAnadTable construction propre time series="+(System.currentTimeMillis()-deb	))
        //         println("tsC tsC.arrTimesSeries.length="+tsC.arrTimesSeries.length+" ;tsC.enrichedArrTimeSeries.length= "+tsC.enrichedArrTimeSeries.length)
        //        println("normalize appele depuis CreateChartAndTable")
        // deb=System.currentTimeMillis()

        // test avec TransformTsCollectionNew
        // val ttsc = new TransformTsCollection(tsC)
       
        val ttsc = new TransformTsCollectionNew(tsC)
        
        // println("CreateChartAnadTable construction TransformTsCollection ="+(System.currentTimeMillis()-deb	))
        // deb=System.currentTimeMillis()
        ttsc.normalizeTs()
        
        //println("CreateChartAnadTable construction TransformTsCollection.normalize="+(System.currentTimeMillis()-deb	))

        val tab = (ttsc.mapByGroupUnit flatMap (_._2)).asInstanceOf[ArrayBuffer[StructTs]]
        ScaCharting.arrEnrichised = ScaCharting.arrEnrichised ++ tab

      }
      // var  deb=System.currentTimeMillis()

      mapDatasets = createDatasets

     
         CreateChartAndTable.mapDatasets =mapDatasets
      
      

      // println("CreateChartAnadTable construction createDataset ="+(System.currentTimeMillis()-deb	))
      val jfChart: JFreeChart = ChartFactory.createTimeSeriesChart(null,
        null,
        null,
        null, // Mettre ici le dataSet
        false,
        true,
        true)
      //      
      ScaCharting.chartPanel.setChart(jfChart)

      ScaCharting.chartPanel.setDisplayToolTips(true)
      val plot: XYPlot = jfChart.getXYPlot();

      jfChart.getRenderingHints().clear()
      ScaCharting.chartPanel.setMouseZoomable(true, false);
      // traiter les dataSets
      var order = 0
      for (x <- CreateChartAndTable.mapDatasets) {
        var axis: NumberAxis = new NumberAxis(x._1);
        var col: Color = ScaCharting.colForRangeAxis.pickColor()
        axis.setLabelPaint(col);
        axis.setAxisLinePaint(col);
        axis.setTickLabelPaint(col);
        var strokeAxis = new BasicStroke(1.0F, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
        axis.setAxisLineStroke(strokeAxis)
        axis.setAutoRangeIncludesZero(false);
        plot.setRangeAxis(order, axis);
        //plot.setDomainAxis(order,axis)
        plot.setDataset(order, x._2);
        plot.mapDatasetToRangeAxis(order, order);
        //plot.mapDatasetToDomainAxis(order, order)
        order += 1
      }

      //recuperer tous les renderers

      //      System.out.println("Nombre de Datasets traitees =" + mapDatasets.size)
      //      System.out.println("Nombre de renderers  =" + plot.getRendererCount());
      val stroke: BasicStroke = new BasicStroke(1.0F, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)

      val rh: java.util.Map[java.awt.RenderingHints.Key, java.lang.Object] = new java.util.HashMap()
      rh.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      rh.put(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_SPEED);
      rh.put(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_SPEED);
      rh.put(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_DISABLE);
      rh.put(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR);
      rh.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_SPEED);
      rh.put(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
      rh.put(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);

      jfChart.getRenderingHints().asInstanceOf[java.util.Map[java.awt.RenderingHints.Key, java.lang.Object]].putAll(rh)

      // creation d'un renderer par dataset

      tabXYLineAndShapeRenderer = new Array[XYLineAndShapeRenderer](mapDatasets.size)
      for (j <- 0 until mapDatasets.size) {
        tabXYLineAndShapeRenderer(j) = new XYLineAndShapeRenderer
      }

      var j = 0
      val nf: NumberFormat = NumberFormat.getInstance(Locale.ENGLISH);

      for (x <- mapDatasets) {
        for (i <- 0 until x._2.getSeriesCount()) {
          tabXYLineAndShapeRenderer(j).setSeriesPaint(i, ScaCharting.colForLine.pickColor())
          tabXYLineAndShapeRenderer(j).setSeriesStroke(i, stroke);
          tabXYLineAndShapeRenderer(j).setSeriesOutlineStroke(i, stroke);
          tabXYLineAndShapeRenderer(j).setSeriesShape(i, new Ellipse2D.Double(-2.0D, -2.0D, 4.0D, 4.0D));
          tabXYLineAndShapeRenderer(j).setSeriesShapesVisible(i, true);
          tabXYLineAndShapeRenderer(j).setSeriesShapesFilled(i, true);
          tabXYLineAndShapeRenderer(j).setDrawOutlines(true);
          tabXYLineAndShapeRenderer(j).setUseFillPaint(true);
          tabXYLineAndShapeRenderer(j).setSeriesFillPaint(i, Color.WHITE);
          tabXYLineAndShapeRenderer(j).setSeriesToolTipGenerator(i, new StandardXYToolTipGenerator("<html> {0}:<br/> ({1}, <b>{2})</b>" +
            "" + "</html>", new SimpleDateFormat("yyyy/MM/dd:HH:mm:ss.SSS"), nf))
        }
        j += 1
      }

      plot.setRenderers(tabXYLineAndShapeRenderer.asInstanceOf[Array[XYItemRenderer]]);

      ScaCharting.chartPanel.getChart().fireChartChanged()
      ScaCharting.myTable.table.setAutoCreateRowSorter(true)
      fillTable

      ScaCharting.tableInit = ScaCharting.myTable.table

      ScaCharting.myTable.table.repaint()
      //        
      //      
      //    }

      //   println("Fin de CreateChartAndTable.createPanelWithBounds="+ScaCharting.listFiles.toString() )
    }

  def fillTable =
    {
      var colRangeAxis: Color = null
      var colTs: Color = null
      var range: Int = -1

      var dfs = new DecimalFormatSymbols(Locale.ENGLISH)
      dfs.setExponentSeparator(" 10^")
      val df: DecimalFormat = new DecimalFormat(ScaCharting.tmpProps.getProperty("scaviewer.df"), dfs)
      for (dataset <- mapDatasets) {

        var tsC = (dataset _2)
        var ret = retouverColorYAxis(dataset _1)

        colRangeAxis = (ret._1)
        range = (ret._2)
        //   println(" Couleur des dataset :"+(dataset _1)+" => "+ colRangeAxis)
        for (i <- 0 until tsC.getSeriesCount()) {
          var ts = tsC.getSeries(i)

          colTs = retouverColorTs(range, ts)

          //	 println(" Couleur de la serie  :"+ts.getKey()+" => "+ colTs)
          //          println("#######################################")
          //           println("current ts("+i+")="+ts.getKey)
          //             println("#######################################")
          //          for (strucTmp <- ScaCharting.arrEnrichised)
          //          {
          //            println("ScaCharting.arrEnrichised.ts.getKey ="+strucTmp.ts.getKey)
          //          }

          var structTs: StructTs = (ScaCharting.arrEnrichised find (_.ts.getKey().asInstanceOf[String].trim == ts.getKey().asInstanceOf[String].trim)).get
          // println("CreateChartAndTable key="+ structTs.ts.getKey.asInstanceOf[String])
          var rowTable: Array[Object] = new StructRowMyTable(true, false, colTs, new MyScale((dataset _1)), new Percent(0), structTs.source, structTs.ts.getKey.asInstanceOf[String],
            structTs.rowTable(0), structTs.rowTable(1), structTs.rowTable(2), structTs.rowTable(3), structTs.rowTable(6), df.format(structTs.rowTable(7)) + " " + (dataset _1) + "/H", structTs.rowTable(5).toLong, structTs.rowTable(4).toLong, structTs.rowTable(9), structTs.rowTable(8), structTs.rowTable(10)).toArray
          ScaCharting.myTable.tabModel.addRow(rowTable)
          //	println("coucou0 : dataSet ="+(dataset _1)+" serie ="+ ts.getKey())
        }
      }

    }

  def retouverColorTs(range: Int, ts: TimeSeries): Color =
    {

      val plot: XYPlot = ScaCharting.chartPanel.getChart().getXYPlot
      var i = 0
      var bool = true

      var ret: Color = null
      val dataset = plot.getDataset(range)
      val nbTs = dataset.getSeriesCount()
      while (bool) {
        if (i < nbTs) {
          if (ts.getKey() == dataset.getSeriesKey(i)) {
            bool = false
            ret = tabXYLineAndShapeRenderer(range).getSeriesPaint(i).asInstanceOf[Color]
          }
        }
        i += 1
      }
      ret
    }
  def retouverColorYAxis(name: String): (Color, Int) =
    {
      val plot: XYPlot = ScaCharting.chartPanel.getChart().getXYPlot
      var i = 0
      var bool = true
      val nbDatasets = plot.getDatasetCount()
      var ret: (Color, Int) = (null, -1)
      while (bool) {
        if (i < nbDatasets) {
          val axis = plot.getRangeAxis(i)
          if (axis.getLabel() == name) {
            bool = false
            ret = ((axis.getLabelPaint.asInstanceOf[Color], i))
          } else {
            i += 1
          }

        } else {
          bool = false
        }
      }

      ret
    }
}
object CreateChartAndTable {

  var mapDatasets = Map[String, TimeSeriesCollection]()
}