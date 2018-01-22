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

import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.JTable
import javax.swing.event.TableModelEvent
import javax.swing.event.RowSorterEvent
import javax.swing.event.RowSorterListener
import javax.swing.table.DefaultTableModel
import com.jlp.scaviewer.ui.MyDefaultCellRenderer
import java.awt.Font
import java.awt.Color
import java.awt.Component
import javax.swing.JLabel
import com.jlp.scaviewer.ui.ScaCharting
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import java.awt.BasicStroke
import com.jlp.scaviewer.timeseries.MapDatasetToTs
import org.jfree.data.time.TimeSeries
import org.jfree.data.time.TimeSeriesCollection
import java.text.NumberFormat
import java.util.Locale
import org.jfree.chart.labels.StandardXYToolTipGenerator
import java.awt.geom.Ellipse2D
import java.text.SimpleDateFormat
import javax.swing.JPopupMenu
import java.awt.Dimension
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import javax.swing.JMenuItem
import javax.swing.JFrame
import org.jfree.chart.axis.DateAxis
import java.util.Calendar
import java.util.Date
import scala.collection.mutable.ArrayBuffer
import com.jlp.scaviewer.timeseries.StructTs
import javax.swing.table.AbstractTableModel
import javax.swing.table.JTableHeader
import scala.swing.Window
import scala.swing.Dialog
import java.awt.Point
import scala.swing.GridPanel
import scala.swing.CheckBox
import scala.swing.event.WindowClosing
import com.jlp.scaviewer.ui.MyTable
import scala.swing.event.WindowClosed
import scala.swing.event.SelectionChanged
import scala.swing.CheckBox
import scala.swing.event.MouseClicked
import org.jfree.chart.axis.NumberAxis

class MouseAdapterJTable(table: JTable) extends MouseAdapter {
  // override def mouseClicked(event: MouseEvent) {
  //override def mouseReleased(event: MouseEvent) { } 
  override def mousePressed(event: MouseEvent) {}

  override def mouseReleased(event: MouseEvent) {
    val x = event.getXOnScreen()
    val y = event.getYOnScreen()

    if (event.getButton() == MouseEvent.BUTTON3 && event.getSource().isInstanceOf[JTable]) {
      var table: JTable = event.getSource.asInstanceOf[JTable]

      val row: Int = table.convertRowIndexToModel(table.rowAtPoint(event.getPoint()));
      val col: Int = table.columnAtPoint(event.getPoint());

      //model_row = table.convertRowIndexToModel(view_row)
      //view_row  = table.convertRowIndexToView(model_row)

      // println(" mousePressed right button sourceClass :" + event.getSource().getClass().getName())
      //  System.out.println("Right Clicked at Row = " + row + ", Column = " + table.getColumnName(col));
      // on enleve la selection de la table
      // on tagge les show à false
      var sel = table.getSelectedRows()

      if (sel.length > 0) {
       
       
        var atLeastOne = false
        // for (i <- 0 until sel.length; if (table.getValueAt(sel(i), table.getColumn("shown").getModelIndex()) == true))
        for (i <- 0 until sel.length) {

          atLeastOne = true

          table.setValueAt(false, sel(i), table.getColumn("shown").getModelIndex())
          var keyTs: String = table.getValueAt(sel(i), table.getColumn("name").getModelIndex()).toString

          var (indexDataset, indexSeries) = retouverdatasetEtTs(keyTs)

          // On cache
          ScaCharting.hiddenTs += ((keyTs, new MapDatasetToTs(indexDataset, ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].getSeries(indexSeries))))
         
          // on enleve du dataset

          // On doit reconstruire toutes les couleurs des series
          var scale = table.getValueAt(sel(i), table.getColumn("scale").getModelIndex()).toString

          ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].removeSeries(indexSeries)
          // val ts = CreateChartAndTable.mapDatasets.get(table.getValueAt(sel(i), table.getColumn("scale").getModelIndex()).toString).get.removeSeries(indexSeries)

          if (None == CreateChartAndTable.mapDatasets.get(scale) || CreateChartAndTable.mapDatasets.get(scale).get.getSeriesCount == 0) {
            CreateChartAndTable.mapDatasets = CreateChartAndTable.mapDatasets - scale
          }

//          for (indexS <- 0 until ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].getSeriesCount()) {
//
//            renderer.setSeriesStroke(indexS, stroke);
//            renderer.setSeriesOutlineStroke(indexS, stroke);
//            renderer.setSeriesShape(indexS, new Ellipse2D.Double(-2.0D, -2.0D, 4.0D, 4.0D));
//            renderer.setSeriesShapesVisible(indexS, true);
//            renderer.setSeriesShapesFilled(indexS, true);
//            renderer.setDrawOutlines(true);
//            renderer.setUseFillPaint(true);
//            renderer.setSeriesFillPaint(indexS, Color.WHITE);
//            renderer.setSeriesToolTipGenerator(indexS, new StandardXYToolTipGenerator("{0}: ({1}, {2})", new SimpleDateFormat("yyyy/MM/dd:HH:mm:ss.SSS"), nf))
//            println("repeindre la series")
//            // renderer.setSeriesPaint(indexS, table.getValueAt(table.convertRowIndexToView(sel(i)),              table.getColumn("color").getModelIndex()).asInstanceOf[Color])
////            renderer.setSeriesPaint(indexS, table.getValueAt(retrouverRow(ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].getSeries(indexS).getKey.toString), 
////                table.getColumn("color").getModelIndex()).asInstanceOf[Color])
//
//          }

        }
        if (atLeastOne) {
          for (i <- (0 until sel.length).reverse) {
            table.getModel().asInstanceOf[DefaultTableModel].removeRow(table.convertRowIndexToModel(sel(i)))
            
          }

        }

        var order = 0
        val nb = ScaCharting.chartPanel.getChart.getXYPlot.getRangeAxisCount()

        for (i <- 0 until nb) {
          ScaCharting.chartPanel.getChart.getXYPlot.setRangeAxis(i, null)
        }

        ScaCharting.colForRangeAxis.restoreAllColors
        for (x <- CreateChartAndTable.mapDatasets) {
           if(x._2.getSeriesCount()>0){
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
      }
      //restituer les couleurs
      for (indexDataset <- 0 until ScaCharting.chartPanel.getChart.getXYPlot.getDatasetCount){
         var renderer = ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(indexDataset).asInstanceOf[XYLineAndShapeRenderer]
         val stroke: BasicStroke = new BasicStroke(1.0F, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
         val nf: NumberFormat = NumberFormat.getInstance(Locale.ENGLISH);
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
            println("repeindre la series")
        //  renderer.setSeriesPaint(indexS, table.getValueAt(table.convertRowIndexToView(sel(i)),              table.getColumn("color").getModelIndex()).asInstanceOf[Color])
            renderer.setSeriesPaint(indexS, table.getValueAt(retrouverRow(ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].getSeries(indexS).getKey.toString), 
                table.getColumn("color").getModelIndex()).asInstanceOf[Color])

          }
      }
     

      //      val jpopup:JPopupMenu=new JPopupMenu()
      //     
      //      jpopup.setBorderPainted(true)
      //      jpopup.setLabel("Table gestion")
      //      jpopup.setPreferredSize(new Dimension(100,100))
      //      
      //      
      //      val remove=jpopup.add("Remove")
      //      remove.addActionListener(this)
      //     
      //       jpopup.setPreferredSize(new Dimension(100,100))
      //      
      //        jpopup.setLocation(x.toInt,y.toInt)
      //     
      //       jpopup.setVisible(true)
      //       
    } else if (event.getButton() == MouseEvent.BUTTON1 && event.getSource().isInstanceOf[JTable]) {
      var table: JTable = event.getSource.asInstanceOf[JTable]
      val row: Int = table.rowAtPoint(event.getPoint());
      val col: Int = table.columnAtPoint(event.getPoint());

      // System.out.println("Left Clicked at Row = " + row + ", Column = " + table.getColumnName(col));
      table.getColumnName(col) match {

        case "shown" =>
          {
            table.repaint()
            // println("Ts :" + table.getValueAt(row, table.getColumn("name").getModelIndex()))
            var keyTs: String = table.getValueAt(row, table.getColumn("name").getModelIndex()).toString
            val nf: NumberFormat = NumberFormat.getInstance(Locale.ENGLISH);
            val stroke: BasicStroke = new BasicStroke(1.0F, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
            if (table.getValueAt(row, col).toString == "false") {
              var (indexDataset, indexSeries) = retouverdatasetEtTs(keyTs)

              // On cache
              ScaCharting.hiddenTs += ((keyTs, new MapDatasetToTs(indexDataset, ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].getSeries(indexSeries))))
              var renderer = ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(indexDataset).asInstanceOf[XYLineAndShapeRenderer]

              // on enleve du dataset
              ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].removeSeries(indexSeries)

              // On doit reconstruire toutes les couleurs des series
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
                renderer.setSeriesPaint(indexS, table.getValueAt(retrouverRow(ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].getSeries(indexS).getKey.toString), table.getColumn("color").getModelIndex()).asInstanceOf[Color])
              }

            } else {

              // on remet dans le dataset
              var mapDatasetToTs = ScaCharting.hiddenTs.get(keyTs)
              var renderer = ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(mapDatasetToTs.get.idxDataset).asInstanceOf[XYLineAndShapeRenderer]
              ScaCharting.chartPanel.getChart.getXYPlot.getDataset(mapDatasetToTs.get.idxDataset).asInstanceOf[TimeSeriesCollection].addSeries(mapDatasetToTs.get.ts)

              var (indexDataset, indexSeries) = retouverdatasetEtTs(mapDatasetToTs.get.ts.getKey.toString)
              // On doit reconstruire toutes les couleurs des series
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
                println("indexS=" + indexS)
                println("retrouverRow=" + retrouverRow(ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].getSeries(indexS).getKey.toString), table.getColumn("color").getModelIndex())
                renderer.setSeriesPaint(indexS, table.getValueAt(retrouverRow(ScaCharting.chartPanel.getChart.getXYPlot.getDataset(indexDataset).asInstanceOf[TimeSeriesCollection].getSeries(indexS).getKey.toString), table.getColumn("color").getModelIndex()).asInstanceOf[Color])
              }

              // On enleve du cache
              ScaCharting.hiddenTs.remove(keyTs)
            }

          }
        case "marked" =>
          {
            if (table.getValueAt(row, table.getColumn("shown").getModelIndex()).toString == "true") {
              //          println("Ts :" + table.getValueAt(row, table.getColumn("name").getModelIndex()))
              //          println("marked=" + table.getValueAt(row, col))
              var keyTs: String = table.getValueAt(row, table.getColumn("name").getModelIndex()).toString

              var (indexDataset, indexSeries) = retouverdatasetEtTs(keyTs)
              if (table.getValueAt(row, col).toString == "true") {
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
        case _ =>

      }
    }

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

  def retrouverRow(keyTs: String): Int =
    {
      var ret = -1

      for (j <- 0 until table.getRowCount()) {
        if (table.getValueAt(j, table.getColumn("name").getModelIndex()).toString == keyTs) {

          ret = j
        }

      }

      ret
    }

  def retrouverCouleurDataSet(keyTs: String): Color =
    {

      val (icxDataset, idxTs) = retouverdatasetEtTs(keyTs)

      var renderer = ScaCharting.chartPanel.getChart.getXYPlot.getRenderer(icxDataset).asInstanceOf[XYLineAndShapeRenderer]
      renderer.getSeriesPaint(icxDataset).asInstanceOf[Color]
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