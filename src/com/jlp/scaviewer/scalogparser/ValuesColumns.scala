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

import scala.util.matching.Regex

case class ValuesColumns(namesValues: Array[String], var ext1Values: Array[String], pat2Values: Array[Regex], unitValues: Array[String], scaleValues: Array[String]) {
  var nbVals = namesValues.length
  var isDurations: Array[Boolean] = new Array(nbVals)
  // remplir isDuration
  for (i <- 0 until unitValues.length) {
    if (namesValues(i).toLowerCase().contains("period")) {
      isDurations(i) = false
    } else {

      unitValues(i) match {

        case "ms" => isDurations(i) = true
        case "millis" => isDurations(i) = true
        case "s" => isDurations(i) = true
        case "seconds" => isDurations(i) = true
        case "mn" => isDurations(i) = true
        case "H" => isDurations(i) = true
        case "d" => isDurations(i) = true
        case "Y" => isDurations(i) = true
        case "micros" => isDurations(i) = true
        case "nanos" => isDurations(i) = true
        case _ => isDurations(i) = false
      }
    }

  }

}