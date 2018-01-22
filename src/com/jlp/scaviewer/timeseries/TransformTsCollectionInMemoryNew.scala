
package com.jlp.scaviewer.timeseries
import com.jlp.scaviewer.commons.utils.Unites
import scala.collection.mutable.ArrayBuffer
import com.jlp.scaviewer.csvutils.CsvFileNew
import org.jfree.data.time.TimeSeries
import scala.collection.mutable._
import com.jlp.scaviewer.commons.utils.Couleurs
import java.awt.Color
import org.jfree.data.time.TimeSeriesCollection
import com.jlp.scaviewer.ui.ScaCharting
import language.postfixOps

case class TransformTsCollectionInMemoryNew(tsC: TimeSeriesCollectionFromCsvInMemoryNew) {

  // Regroupement des Ts par groupUnit
  val mapByGroupUnit: Map[GroupUnit, ArrayBuffer[StructTs]] = Map() ++ (tsC.enrichedArrTimeSeries groupBy (_.grp)).asInstanceOf[scala.collection.immutable.Map[GroupUnit, ArrayBuffer[StructTs]]]

  // pour chaque groupe on va chercher la moyenne, l'unit� brute, on transforme en unit� basique 
  // en supprimant les coeffMultiplicateurs sur les unites basiques et en ramenant � la millisecondes les unites de temps

  val coulGrp = new Couleurs()
  val coulTs = new Couleurs()

  var mapGrpToColor: Map[GroupUnit, Color] = Map[GroupUnit, Color]() // background of cells scale of the table and color of the Y Axis
  var mapStructTsToColor: Map[StructTs, Color] = Map[StructTs, Color]() // background of the cell color and color of the timeseries

  def affectCoulToGrpAndTs(): TransformTsCollectionInMemoryNew =
    {
      for (enr <- mapByGroupUnit) {
        mapGrpToColor += ((enr _1, coulGrp.pickColor()))
        for (enr2 <- enr._2) {

          mapStructTsToColor += ((enr2, coulTs.pickColor()))
        }

      }
      this
    }

  def findBasicUnit(structTs: StructTs): (String, String) =
    {
      (structTs.unite, Unites.returnBasicUnit(structTs.unite))
    }

  /**
   * Recherche par groupUnit du Min et du max de l'average ramene au unit� de base
   *
   */
  def trouverMultGroupFromBasicUnit(arrTs: ArrayBuffer[StructTs]): (Double, String, String) =
    {

      var maxAvg = Double.MinValue
      var unitForMax: String = null
      var mult = 1.0
      var maxAvgRet = Double.MinValue
      for (structTs <- arrTs) {
        /*
   * Structure rowtable
   *  (avgPond, avg, min, max, countAll, countVal, stdDev, irslope)
   */
        unitForMax = Unites.returnBasicUnit(structTs.unite)
        var mult2 = Unites.convert(structTs.unite, Unites.returnBasicUnit(structTs.unite))._1.asInstanceOf[Double]
        //  println("Value passed="+structTs.rowTable(0)+" ; Unites.returnBasicUnit("+structTs.unite+")="+ Unites.returnBasicUnit(structTs.unite)+ " mult2="+mult2)
        if ((structTs.rowTable(0) * mult2) > maxAvg) {
          maxAvg = structTs.rowTable(0) * mult2
          unitForMax = structTs.unite
          maxAvgRet = structTs.rowTable(0)
          mult = mult2
        }

      }
      //  println("GroupUnit="+arrTs(0).grp+" ;maxAvg="+maxAvg)
      // Unites.bestConversion(maxAvg,CsvFile.scaViewerProps.getProperty("scaviewer.yAxis.valueMax").toLong,
      //     CsvFile.scaViewerProps.getProperty("scaviewer.yAxis.valueMax").toLong, Unites.returnBasicUnit(basicUnit)) 
      //  print("BestConversion entry="+(maxAvg,CsvFile.scaViewerProps.getProperty("scaviewer.yAxis.valueMax").toLong,
      //     CsvFile.scaViewerProps.getProperty("scaviewer.yAxis.valueMin").toLong,Unites.returnBasicUnit(basicUnit)))
      //     println(" => "+ Unites.bestConversion(maxAvg,CsvFile.scaViewerProps.getProperty("scaviewer.yAxis.valueMax").toLong,
      //     CsvFile.scaViewerProps.getProperty("scaviewer.yAxis.valueMin").toLong,Unites.returnBasicUnit(basicUnit) ) )
      //  

      Unites.bestConversion(maxAvgRet, CsvFileNew.scaChartProps.getProperty("scaviewer.yAxis.valueMax").toLong,
        CsvFileNew.scaChartProps.getProperty("scaviewer.yAxis.valueMin").toLong, unitForMax)
    }
  def multiplyTs(strucTs: StructTs, conv: (Double, String, String)): StructTs =
    {
      val nbItems = strucTs.ts.getItemCount()

      // println("nbItems="+nbItems)
      var newTs = strucTs.ts.createCopy(0, scala.math.max(0, nbItems - 1))
      // provisoire
      // modifier le nom
      var name = newTs.getKey().asInstanceOf[String]
      //println("name =" + name)
      if (name.contains(conv _2)) {
        name = name.substring(0, name.indexOf(conv _2)) + conv._3 + ")"
      } else {
        name = name + "(" + conv._3 + ")"
      }

      var retTs = new TimeSeries(name)
      // modifier la colonne
      var col = strucTs.columnName
      if (col.contains(conv _2)) {
        col = col.substring(0, col.indexOf(conv _2)) + conv._3 + ")"
      } else {
        col = col + "(" + conv._3 + ")"
      }

      for (i <- (0 until nbItems)) {

        var myItem: MyTimeSeriesItem = newTs.getDataItem(i).asInstanceOf[MyTimeSeriesItem]
        myItem.max = myItem.max * conv._1
        myItem.setValue(myItem.getValue().doubleValue() * conv._1)
        retTs.add(myItem)

      }

      var newRow = strucTs.rowTable map (_ * conv._1)
      // Ramener l'irslope � l'heure ms => multiplier par 3600*1000
      newRow(7) = strucTs.rowTable(7) * conv._1 * 3600 * 1000
      newRow(4) = strucTs.rowTable(4)
      newRow(5) = strucTs.rowTable(5)

      new StructTs(retTs, strucTs.pivot, col, conv _3, strucTs.grp, newRow, tsC.csvFileInMemory.name)

    }

  def normalizeTs(to: Array[String]): TransformTsCollectionInMemoryNew =
    {
      var tmp: Map[GroupUnit, ArrayBuffer[StructTs]] = Map[GroupUnit, ArrayBuffer[StructTs]]()
      for (enr <- mapByGroupUnit) {
        var i = 0
        var j = 0
        var transf = Unites.convert((enr _1).toString(), to(j))

        for (enr2 <- enr._2) {
          //        println("Dans Normalisation : " + enr2.ts.getKey + " GroupUnit =" + enr._1 + " convert(" + (enr2.unite, transf._3) + ")  =" + (Unites.convert(enr2.unite, transf._3)._1, enr2.unite, transf._3))
          //        print("enr2.unite=")
          // println(enr2.unite)

          enr._2(i) = multiplyTs(enr2, (Unites.convert(enr2.unite, transf._3)._1, enr2.unite, transf._3))
          i += 1

        }
        j += 1
        //		  println("Dans Normalisation : Traitement group Unit ="+enr._1)
        //		  	  println("######################################################\n")
        tmp += ((enr _1, enr _2))
      }
      tmp map { (x) => mapByGroupUnit.update(x._1, x._2) }
      this
    }

  def normalizeTs(): TransformTsCollectionInMemoryNew = {
    // Pour chaque groupUnit
    // Trouver la meuilleure conversion � partir de l'unite de base( trouverMultGroupFromBasicUnit) => donne unite cible
    // pour chaque Timeseries appliquer la  conversion : unit� actuelle -> unite cible

    var tmp: Map[GroupUnit, ArrayBuffer[StructTs]] = Map[GroupUnit, ArrayBuffer[StructTs]]()

    for (enr <- mapByGroupUnit) {
      // Trouver la meilleure transformation
      var transf = trouverMultGroupFromBasicUnit(enr _2)
      //       println("######################################################")
      //    		  println("Dans Normalisation : Deb Traitement group Unit ="+enr._1 +" leng="+enr._2.length)
      var i = 0
      for (enr2 <- enr._2) {
        //        println("Dans Normalisation : " + enr2.ts.getKey + " GroupUnit =" + enr._1 + " convert(" + (enr2.unite, transf._3) + ")  =" + (Unites.convert(enr2.unite, transf._3)._1, enr2.unite, transf._3))
        //        print("enr2.unite=")
        // println(enr2.unite)

        enr._2(i) = multiplyTs(enr2, (Unites.convert(enr2.unite, transf._3)._1, enr2.unite, transf._3))
        i += 1
      }
      //		  println("Dans Normalisation : Traitement group Unit ="+enr._1)
      //		  	  println("######################################################\n")
      tmp += ((enr _1, enr _2))
    }
    tmp map { (x) => mapByGroupUnit.update(x._1, x._2) }
    this

  }
  def createDatasets(tab: ArrayBuffer[StructTs]): scala.collection.immutable.Map[String, TimeSeriesCollection] =
    {

      tab groupBy (_.unite) map ((x) => ((x._1, createTimeSeriesCollections(x._2))))

    }

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

        }
      }
      tsC
    }
}