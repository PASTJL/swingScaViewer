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
import scala.collection.mutable._
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import org.apache.commons.math.stat.StatUtils;
import language.postfixOps

class CumulEnregistrementStatMemory {
  var arraydouble: Array[Double] = null;
  var sum: Double = 0;
  var count: Int = 0;
  var mediane: Double = 0;
  var mean: Double = 0;
  var min: Double = 0;
  var max: Double = 0;
  var stdDev: Double = 0;
  var percentile: Double = 0;
  var arrayList: List[Double] = List.empty
  var name = ""
  var hmPas: Map[Long, StructPasCumul] = Map()
  var sortedMap: SortedMap[Long, StructPasCumul] = SortedMap.empty
  var keySet: SortedSet[Long] = null
  final def add(dd: Double, pas: Double): CumulEnregistrementStatMemory = {
    count += 1
    sum += dd;
    if (pas == 0.0) {

      arrayList = dd :: arrayList
    } else {

      var dbl: Double = dd / pas;
      var index: Long = dbl.longValue();

      //  println("traitement du double :" + dd + " avec pas de :" + pas + " index=" + index)
      if (hmPas.contains(index)) {
        hmPas.put(index, hmPas.get(index).get.add(dd));
      } else {

        hmPas.put(index, new StructPasCumul(1, dd));
      }

    }
    this
  }

  final def getMean(): Double = {
    mean = sum / count;
    mean;
  }

  final def getMediane(pas: Double): Double= {

    if (pas == 0.0) {
      arrayList = arrayList sortWith (_ < _)
      mediane = arrayList((50 * arrayList.length / 100))
    } else {

      var rang: Int = 0;
      var indx: Long = 0L;
      // set = hmPas.keySet();
      // treeSet = new TreeSet<Long>(set);
      var struc: StructPasCumul = null;
      var bool = true
      // println("calcul mediane count=" + count)
      // println("mediane KeySet.lenght =" + keySet.size)
      for (key <- keySet; if (bool)) {

        struc = sortedMap.get(key).get;

        rang = rang + struc.nbCount;
        if (rang >= (count / 2)) {
          //  println("mediane =" + struc.moyenne + " trouve a rang =" + rang + " pour key = " + key + " pour count=" + count)
          mediane = struc.moyenne;
          bool = false
        }

      }
    }
    mediane

  }
  final def getPercentile(percent: Int, pas: Double): Double = {
    if (pas == 0.0) {
      percentile = arrayList((percent * arrayList.length / 100))
    } else {

      var rang: Int = 0;

      var struc: StructPasCumul = null;
      var bool = true

      for (key <- keySet if (bool)) {

        struc = sortedMap.get(key).get
        rang = rang + struc.nbCount;
        if (rang >= (percent * count / 100)) {
          percentile = struc.moyenne;
          // println("percentile trouve a rang =" + rang)
          bool = false
        }

      }
    }
    return percentile;
  }

  final def getMin(pas: Double): Double = {
    if (pas == 0.0) {
      min = arrayList(0)
    } else {

      min = (sortedMap.head _2).moyenne;
    }
    min;
  }

  final def getMax(pas: Double): Double = {
    if (pas == 0.0) {
      max = arrayList(arrayList.length - 1);
    } else {
      max = (sortedMap.last _2).moyenne;
    }

    return max;
  }

  final def getStdDev(pas: Double): Double = {
    if (pas == 0.0) {
      this.stdDev = scala.math.sqrt(StatUtils.variance(arrayList.toArray));

    } else {

      var variance: Double = 0.0;

      var struc: StructPasCumul = null;
      // set = hmPas.keySet();
      // treeSet = new TreeSet<Long>(set);

      for (key <- keySet) {

        struc = hmPas.get(key).get
        variance += struc.nbCount * scala.math.pow((struc.moyenne - mean), 2);

      }
      stdDev = scala.math.sqrt(variance / (count - 1));
    }
    stdDev
  }
  override final def finalize() {

    hmPas.clear();
    hmPas = null;
    sortedMap = SortedMap.empty
    sortedMap = null
    if (null != arrayList) {
      arrayList = List.empty
      arrayList = null;
    }

  }

  final def closeEnr(pas: Double): CumulEnregistrementStatMemory = {
    if (pas != 0.0) {
      //  System.out.println("taille hmPas =" + hmPas.size)
      sortedMap = sortedMap ++ hmPas
      keySet = sortedMap.keySet

    } else {
      arrayList = arrayList sortWith (_ < _)
    }
    getMediane(pas);
    getPercentile(MyDialogStatsFile.tfPercentile.text.toInt, pas);
    getMin(pas);
    getMax(pas);
    getMean();
    getStdDev(pas);

    finalize();
    this
  }

  final def mergeEnr(enr2: CumulEnregistrementStatMemory, pas: Double): CumulEnregistrementStatMemory = {

    if (pas == 0.0) {
      count += enr2.count;
      sum += enr2.sum;
      arrayList ++= enr2.arrayList
      enr2.arrayList = List.empty
      enr2.arrayList = null;
    } else {
      var keySet2 = enr2.hmPas.keySet

      var idx: Long = 0L;
      var struc: StructPasCumul = null;

      for (key <- keySet2) {

        struc = enr2.hmPas.get(key).get;
        if (hmPas.contains(key)) {
          hmPas += ((key, this.hmPas.get(key).get.merge(struc)))
        } else {
          // On rajoute
          hmPas += ((key, struc));
        }

      }
      count += enr2.count;
      sum += enr2.sum;
      enr2.finalize;

    }

    this
  }
  final def setName(name: String): CumulEnregistrementStatMemory = {
    this.name = name
    this
  }
  final def scale(mult: Double): CumulEnregistrementStatMemory = {
    sum *= mult

    mediane *= mult
    mean *= mult
    min *= mult
    max *= mult
    stdDev *= mult
    percentile *= mult
    this
  }
}