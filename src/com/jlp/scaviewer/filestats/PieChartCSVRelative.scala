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
package com.jlp.scaviewer.filestats
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Font
import java.awt.Toolkit
import java.text.ParseException
import javax.swing.JDialog
import javax.swing.JPanel
import javax.swing.JSplitPane
import javax.swing.table.TableColumnModel
import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart
import org.jfree.chart.labels.StandardPieSectionLabelGenerator
import org.jfree.chart.plot.PiePlot3D
import org.jfree.data.general.DefaultPieDataset
import org.jfree.ui.RefineryUtilities

class PieChartCSVRelative(myDialogStatResultAdvanced: MyDialogResultStatsFiles, title: String, modal: Boolean) extends JDialog(myDialogStatResultAdvanced.peer, title, modal) {

  var nbLignes: Int = 0;
  var colCount = -1;
  var colSum = -1;
  var colTitle = -1;

  val labGen: StandardPieSectionLabelGenerator = new StandardPieSectionLabelGenerator("{0} = {1} ({2})");
  this.setTitle(title);
  nbLignes = myDialogStatResultAdvanced.nbLinesTable

  var colModel: TableColumnModel = myDialogStatResultAdvanced.table
    .getColumnModel();
  //for (int col = 0; col < colModel.getColumnCount(); col++) {
  for (col <- 0 until colModel.getColumnCount()) {
    if (colModel.getColumn(col).getHeaderValue().asInstanceOf[String]
      .equals("Count")) {
      colCount = col;
    }
    if (colModel.getColumn(col).getHeaderValue().asInstanceOf[String]
      .equals("Sum")) {
      colSum = col;
    }
    if (colModel.getColumn(col).getHeaderValue().asInstanceOf[String]
      .startsWith("Criteria")) {
      colTitle = col;
    }
  }

  if (myDialogStatResultAdvanced.table.getValueAt(nbLignes - 1, colSum).asInstanceOf[Double] != 0) {
    creerDoublePie();
  } else {
    creerSimplePie();
  }

  private def creerSimplePie() {
    // TODO Auto-generated method stub
    var dataset: DefaultPieDataset = new DefaultPieDataset();
    // Si sum!=0 on cree deux pie

    var current: Double = 0d

    for (i <- 0 until nbLignes - 1) {

      current = myDialogStatResultAdvanced.table.getValueAt(i, colCount).asInstanceOf[Int]

      dataset.setValue(myDialogStatResultAdvanced.table.getValueAt(i, colTitle).asInstanceOf[String], current);

    }

    val chart: JFreeChart = ChartFactory.createPieChart3D(
      "Relative Percent Count Pie Chart", dataset, false, true,
      MyDialogStatsFile.currentLocale);
    val plot: PiePlot3D = chart.getPlot().asInstanceOf[PiePlot3D]
    plot.setLabelGenerator(this.labGen);
    plot.setLabelFont(new Font("SansSerif", Font.PLAIN, 12));
    plot.setNoDataMessage("No data available");
    plot.setCircular(false);
    plot.setForegroundAlpha(0.6f);

    this.getContentPane().add(new ChartPanel(chart));
    this.setPreferredSize(new Dimension(800, 600));
    this.pack();
    this.setVisible(true);
    RefineryUtilities.centerFrameOnScreen(this);

  }

  private def creerDoublePie() {

    var jp1: JPanel = new JPanel();
    var jp2 = new JPanel();
    jp1.setLayout(new BorderLayout());
    jp2.setLayout(new BorderLayout());

    var jsp: JSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
    this.setContentPane(jsp);
    // jp.setLayout(gl);
    var dataset1 = new DefaultPieDataset();
    var dataset2 = new DefaultPieDataset();

    var others = 0d

    var current = 0d
    for (i <- 0 until nbLignes - 1) {

      current = myDialogStatResultAdvanced.table.getValueAt(i, colCount).asInstanceOf[Int]

      dataset1.setValue(myDialogStatResultAdvanced.table.getValueAt(i, colTitle).asInstanceOf[String], current);

    }

    var chart1 = ChartFactory.createPieChart3D(
      "Relative Percent Count Pie Chart", dataset1, false, true,
      MyDialogStatsFile.currentLocale);
    var plot: PiePlot3D = chart1.getPlot().asInstanceOf[PiePlot3D]
    plot.setLabelGenerator(this.labGen);
    plot.setLabelFont(new Font("SansSerif", Font.PLAIN, 12));
    plot.setNoDataMessage("No data available");
    plot.setCircular(false);
    plot.setForegroundAlpha(0.6f);

    current = 0f;
    for (i <- 0 until nbLignes - 1) {

      current = myDialogStatResultAdvanced.table.getValueAt(i, colSum).asInstanceOf[Double]
      others = others - current;

      dataset2.setValue(myDialogStatResultAdvanced.table.getValueAt(i, colTitle).asInstanceOf[String], current);

    }

    var chart2 = ChartFactory.createPieChart3D(
      "Relative Percent Sum Pie Chart", dataset2, false, true,
      MyDialogStatsFile.currentLocale);
    var plot2: PiePlot3D = chart2.getPlot().asInstanceOf[PiePlot3D]
    plot2.setLabelGenerator(this.labGen);
    plot2.setLabelFont(new Font("SansSerif", Font.PLAIN, 12));
    plot2.setNoDataMessage("No data available");
    plot2.setCircular(false);
    plot2.setForegroundAlpha(0.6f);

    jsp.setTopComponent(jp1);

    jsp.setBottomComponent(jp2);
    jsp.setDividerSize(5);

    jp1.add(new ChartPanel(chart1), BorderLayout.CENTER);

    jp2.add(new ChartPanel(chart2), BorderLayout.CENTER);
    this.pack();
    this.setPreferredSize(Toolkit.getDefaultToolkit().getScreenSize());
    jsp.setDividerLocation(0.5);
    this.setVisible(true);
    System.out.println("creerDoublePie() coucou8");
    RefineryUtilities.centerFrameOnScreen(this);
    jsp.setDividerLocation((Toolkit.getDefaultToolkit()
      .getScreenSize().getWidth() / 2).asInstanceOf[Int]);

  }

}
