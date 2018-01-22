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
package com.jlp.scaviewer.timeseries
import com.jlp.scaviewer.commons.utils.Unites
import scala.collection.mutable.ArrayBuffer

import org.jfree.data.time.TimeSeries
import scala.collection.mutable._
import com.jlp.scaviewer.commons.utils.Couleurs
import java.awt.Color
import com.jlp.scaviewer.csvutils.CsvFileNew
import language.postfixOps

case class TransformTsCollectionNew(tsC: TimeSeriesCollectionFromCsvNew) {

  // Regroupement des Ts par groupUnit
  val mapByGroupUnit: Map[GroupUnit, ArrayBuffer[StructTs]] = Map() ++ (tsC.enrichedArrTimeSeries groupBy (_.grp)).asInstanceOf[scala.collection.immutable.Map[GroupUnit, ArrayBuffer[StructTs]]]

  // pour chaque groupe on va chercher la moyenne, l'unit� brute, on transforme en unit� basique 
  // en supprimant les coeffMultiplicateurs sur les unites basiques et en ramenant � la millisecondes les unites de temps

  val coulGrp = new Couleurs()
  val coulTs = new Couleurs()

  var mapGrpToColor: Map[GroupUnit, Color] = Map[GroupUnit, Color]() // background of cells scale of the table and color of the Y Axis
  var mapStructTsToColor: Map[StructTs, Color] = Map[StructTs, Color]() // background of the cell color and color of the timeseries

  def affectCoulToGrpAndTs(): TransformTsCollectionNew =
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
      //var deb=System.currentTimeMillis()
      val nbItems = strucTs.ts.getItemCount()
      var newTs = strucTs.ts.createCopy(0, scala.math.max(0, nbItems - 1)) // provisoire

      // println("TransFormTsCollection multiply pour serie :"+strucTs.ts.getKey+" creation copy ="+(System.currentTimeMillis()-deb))
      // modifier le nom
      var name = newTs.getKey().asInstanceOf[String]
      // Trouver la premiere parenthese ouvrante
      var idxOpen = name.lastIndexOf("(")
     
        //println("name =" + name)
        if (idxOpen >0 && name .substring(idxOpen).contains(conv _2)) {
          name = name.substring(0,idxOpen)+"(" + conv._3 + ")"
        } else {
          name = name + "(" + conv._3 + ")"
        }
     

      newTs.setKey(name)
      var retTs = new TimeSeries(name)
      // modifier la colonne
      var col = strucTs.columnName
       var idxOpenCol =  col.lastIndexOf("(")
      if (idxOpenCol> 0 && col.substring(idxOpenCol).contains(conv _2)) {
        col = col.substring(0, idxOpenCol)+"(" + conv._3 + ")"
      } else {
        col = col + "(" + conv._3 + ")"
      }
      // deb=System.currentTimeMillis()
      for (i <- (0 until nbItems)) {

        var myItem: MyTimeSeriesItem = newTs.getDataItem(i).asInstanceOf[MyTimeSeriesItem]
        myItem.max = myItem.max * conv._1
        myItem.setValue(myItem.getValue().doubleValue() * conv._1)
        retTs.add(myItem)
        //newTs.delete( myItem.getPeriod());

        // newTs.add(myItem)

      }
      // println("TransFormTsCollection multiply pour serie :"+strucTs.ts.getKey+" fin creation items="+(System.currentTimeMillis()-deb))
      var newRow = strucTs.rowTable map (_ * conv._1)
      // Ramener l'irslope � l'heure ms => multiplier par 3600*1000
      newRow(7) = strucTs.rowTable(7) * conv._1 * 3600 * 1000
      newRow(4) = strucTs.rowTable(4)
      newRow(5) = strucTs.rowTable(5)

      new StructTs(retTs, strucTs.pivot, col, conv _3, strucTs.grp, newRow, tsC.csvFile.file.getAbsolutePath())
    }

  def normalizeTs(): TransformTsCollectionNew = {
    // Pour chaque groupUnit
    // Trouver la meuilleure conversion � partir de l'unite de base( trouverMultGroupFromBasicUnit) => donne unite cible
    // pour chaque Timeseries appliquer la  conversion : unit� actuelle -> unite cible

    var tmp: Map[GroupUnit, ArrayBuffer[StructTs]] = Map[GroupUnit, ArrayBuffer[StructTs]]()
    // var deb=System.currentTimeMillis()
    for (enr <- mapByGroupUnit.par) {
      //  var deb=System.currentTimeMillis()
      // Trouver la meilleure transformation
      var transf = trouverMultGroupFromBasicUnit(enr _2)
      //    println("TransFormTsCollection.normalize traitement transformation  goupe "+(enr _1)+" duree="+(System.currentTimeMillis()-deb))
      //       println("######################################################")
      //    		  println("Dans Normalisation : Deb Traitement group Unit ="+enr._1 +" leng="+enr._2.length)
      var i = 0
      for (enr2 <- enr._2) {
        //        println("Dans Normalisation : " + enr2.ts.getKey + " GroupUnit =" + enr._1 + " convert(" + (enr2.unite, transf._3) + ")  =" + (Unites.convert(enr2.unite, transf._3)._1, enr2.unite, transf._3))
        //        print("enr2.unite=")
        // println(enr2.unite)
        //var deb=System.currentTimeMillis()
        enr._2(i) = multiplyTs(enr2, (Unites.convert(enr2.unite, transf._3)._1, enr2.unite, transf._3))
        // println("TransFormTsCollection.normalize traitement multiplication serie   "+(enr2.ts.getKey)+" duree="+(System.currentTimeMillis()-deb))
        i += 1
      }
      //		  println("Dans Normalisation : Traitement group Unit ="+enr._1)
      //		  	  println("######################################################\n")
      tmp += ((enr _1, enr _2))
    }
    tmp map { (x) => mapByGroupUnit.update(x._1, x._2) }
    this

  }

}