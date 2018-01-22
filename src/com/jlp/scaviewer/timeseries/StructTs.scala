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
import org.jfree.data.time.TimeSeries
import scala.collection.mutable.ArrayBuffer

case class StructTs(ts: TimeSeries, pivot: String, columnName: String, unite: String, grp: GroupUnit, rowTable: ArrayBuffer[Double],source:String) {

  /*
   * Structure rowtable
   *  (avgPond, avg, min, max, countAll, countVal, stdDev, irslope,sumPond,sum,maxMax)
   */
}