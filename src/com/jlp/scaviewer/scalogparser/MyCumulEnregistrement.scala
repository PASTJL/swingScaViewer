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
package com.jlp.scaviewer.scalogparser

import java.util.concurrent.atomic.AtomicInteger

case class MyCumulEnregistrement(name: String, period: Long,
  var boolDurations: Array[Boolean], nbVal: Int) {
  var counts: Array[Int] = Array()
  var averages: Array[Double] = Array()
  var mins: Array[Double] = Array()
  var maxs: Array[Double] = Array()
  var sums: Array[Double] = Array()
  var rates: Array[Double] = Array()
  var countParallels: Array[AtomicInteger] = Array()
  val pivot: String = ""
  var isDuration: Boolean = false
  reInit

  final def reInit() {
    counts = Array.ofDim(nbVal)
    averages = Array.ofDim(nbVal)
    mins = Array.ofDim(nbVal)
    maxs =Array.ofDim(nbVal)
    sums = Array.ofDim(nbVal)
    rates = Array.ofDim(nbVal)
    countParallels = Array.ofDim(nbVal)

    for (i <- 0 until nbVal) {
      counts(i) = 0
      mins(i) = Double.MaxValue
      averages(i) =Double.NaN
      maxs(i) = Double.MinValue
      sums(i) = 0
      rates(i) =Double.NaN
      countParallels(i) = new AtomicInteger(0)
    }

  }
  final def closeEnr():MyCumulEnregistrement = {
    for (i <- 0 until nbVal) {
      if (counts(i) == 0) {
        averages(i) = Double.NaN;
         rates(i) =Double.NaN
      } else {
        averages(i) = sums(i) / counts(i);
        // period in ms
         rates(i) = 1000 * counts(i).asInstanceOf[Double] / period
      }
      // System.out.println("close enr count ="+count+" ; period ="+period);
      // la period est en ms
     
    }
    this
  }
  final def incrementCountParallel(i: Int): MyCumulEnregistrement = {

    countParallels(i) = new AtomicInteger(countParallels(i).addAndGet(1))
    this
  }
  final def addValues(
    values: Array[Double]): MyCumulEnregistrement = {

    for (indexValue <- 0 until nbVal) {
      if (!values(indexValue).isNaN && !values(indexValue).isInfinity) {
        counts(indexValue) += 1
        if(sums(indexValue).isNaN)
        sums(indexValue) = values(indexValue)
        else
          sums(indexValue) += values(indexValue)
        mins(indexValue) = scala.math.min(mins(indexValue),
          values(indexValue))
        maxs(indexValue) = scala.math.max(maxs(indexValue),
          values(indexValue))
        incrementCountParallel(indexValue)
      }
    }
    this
  }
  final def merge(
    that: MyCumulEnregistrement): MyCumulEnregistrement = {
    for (i <- 0 until nbVal) {
      sums(i) = this.sums(i) + that.sums(i)
      counts(i) = this.counts(i) + that.counts(i)
      countParallels(i) = new AtomicInteger(this.countParallels(i).get
        + that.countParallels(i).get)
      if (counts(i) > 0) {
        averages(i) = sums(i) / counts(i)
        rates(i) = 1000 * counts(i).asInstanceOf[Double] / period
        mins(i) = scala.math.min(this.mins(i), that.mins(i))
        maxs(i) = scala.math.max(this.maxs(i), that.maxs(i))
      } else {
        averages(i) = Double.NaN
        rates(i) = Double.NaN
        mins(i) = Double.NaN
        maxs(i) = Double.NaN
      }

    }
    this
  }

}