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
class TraiterJRockit {
  def metInit(tab: Array[String] = null) {
    // To reinitialise static variable if necessary

    TraiterJRockit.structJRockit = new StructJRockit
    TraiterJRockit.enrCurrent = null
    TraiterJRockit.dateInMillis = 0

    TraiterJRockit.dateInMillisGC = 0
  
    TraiterJRockit.circleArray=new CircleArray(10)
  }
  def retour(tabStr: Array[String]): Double =
    {

      if (null == TraiterJRockit.enrCurrent ) {
        TraiterJRockit.enrCurrent = tabStr(0)
      
        TraiterJRockit.dateInMillis = 0

        TraiterJRockit.dateInMillisGC = 0
        traiterEnr(tabStr)
      } else   if (!TraiterJRockit.enrCurrent.equals( tabStr(0))) {
         
          traiterEnr(tabStr)
        
      }

      // Faire les retours ici
      tabStr(1) match {

        case "sizeHeapBefore" => TraiterJRockit.structJRockit.sizeHeapBefore
        case "sizeHeapAfter" => TraiterJRockit.structJRockit.sizeHeapAfter
         case "fullGCsizeHeapBefore" => TraiterJRockit.structJRockit.fullGCsizeHeapBefore
        case "fullGCsizeHeapAfter" => TraiterJRockit.structJRockit.fullGCsizeHeapAfter
        case "nurseryGCsizeHeapBefore" => TraiterJRockit.structJRockit.nurseryGCsizeHeapBefore
        case "nurseryGCsizeHeapAfter" => TraiterJRockit.structJRockit.nurseryGCsizeHeapAfter
        case "GCDuration" => TraiterJRockit.structJRockit.gcDuration
         case "nurseryGCDuration" => TraiterJRockit.structJRockit.nurseryGCDuration
        case "throughput" => TraiterJRockit.structJRockit.throughput
        case "memThroughput" => TraiterJRockit.structJRockit.memThroughput
        case "fullGCDuration" => TraiterJRockit.structJRockit.fullGCDuration
        case _ => Double.NaN

      }

    }

  private def traiterEnr(tabStr: Array[String]) {
    // trouver la date:

    val regDate = """\d+(\.|,)\d+(-|:)""".r
    var match0 = regDate.findFirstIn(tabStr(0))
    if (None != match0) {
      // extraire la date
      var ext1 = match0.get.replace(",",".").replace(":","-")
      ext1 = ext1.substring(0, ext1.indexOf("-")) // supprimer le dernier caractere
      //var dateCurrentInMillis = ext1.split("\\.")(0).toLong * 1000 + ext1.split("\\.")(1).toLong
      var dateCurrentInMillis = (ext1.toDouble * 1000).toLong
      remplirStruct(dateCurrentInMillis, tabStr)

    }
  }

  private def remplirStruct(dateCurrent: Long, tabStr: Array[String]) {
    var ext1: Option[String] = None
    var reg1 = "".r

   

    // remplissage  sizeHeapBefore GC 1572864K
     TraiterJRockit.structJRockit.sizeHeapBefore = Double.NaN
    reg1 = """GC\s+\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        //   println("sizeHeapBefore : ext2="+ext2.get)
        TraiterJRockit.structJRockit.sizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterJRockit.structJRockit.sizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
    // remplissage  sizeHeapAfter GC 1572864K->929672K
     TraiterJRockit.structJRockit.sizeHeapAfter =Double.NaN
    reg1 = """\s+\d+(K|M)->\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterJRockit.structJRockit.sizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterJRockit.structJRockit.sizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
    
    // remplissage  fullGCsizeHeapBefore GC 1572864K
      TraiterJRockit.structJRockit.fullGCsizeHeapBefore=Double.NaN
    reg1 = """:\s+GC\s+\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        //   println("sizeHeapBefore : ext2="+ext2.get)
        TraiterJRockit.structJRockit.fullGCsizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterJRockit.structJRockit.fullGCsizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
    // remplissage  fullGCsizeHeapAfter GC 1572864K->929672K
    
       TraiterJRockit.structJRockit.fullGCsizeHeapAfter=Double.NaN
    reg1 = """:\s+GC\s+\d+(K|M)->\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterJRockit.structJRockit.fullGCsizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterJRockit.structJRockit.fullGCsizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
    
    
    
  // Remplissage nurseryGCsizeHeapBefore=Double.NaN nursery GC 166970K
        TraiterJRockit.structJRockit.nurseryGCsizeHeapBefore =Double.NaN
     reg1 = """nursery\s+GC\s+\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterJRockit.structJRockit.nurseryGCsizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterJRockit.structJRockit.nurseryGCsizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
     
     
     // Remplissage nurseryGCsizeHeapAfter=Double.NaN ursery GC 166970K->67706K
        TraiterJRockit.structJRockit.nurseryGCsizeHeapAfter =Double.NaN
     reg1 = """nursery\s+GC\s+\d+(K|M)->\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterJRockit.structJRockit.nurseryGCsizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterJRockit.structJRockit.nurseryGCsizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
     
  
   
   // remplissage   var nurseryGCDuration=Double.NaN
    TraiterJRockit.structJRockit.nurseryGCDuration = Double.NaN
    reg1 = """nursery\s+GC.+?\d+(\.|,)\d+\s+ms""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      //  println("ext1.get="+ext1.get)
      var ext2 = """\d+\.\d+""".r.findFirstIn(ext1.get.replace(",", "."))

      TraiterJRockit.structJRockit.nurseryGCDuration = ext2.get.toDouble

    }
   
     // remplissage Throughput Memoire
     // println("Avant Traitement throughput memor"+TraiterHotspot5And6.structHotSpot.sizeHeapBefore+ " " +TraiterHotspot5And6.structHotSpot.sizeHeapAfter)
       TraiterJRockit.structJRockit.memThroughput=Double.NaN
      if( !(TraiterJRockit.structJRockit.sizeHeapBefore.isNaN()) && !(TraiterJRockit.structJRockit.sizeHeapAfter.isNaN()))
      {
      var sweeped= TraiterJRockit.structJRockit.sizeHeapBefore-TraiterJRockit.structJRockit.sizeHeapAfter
     // println("Traitement throughput memor")
      TraiterJRockit.circleArray.put((dateCurrent,sweeped))
     TraiterJRockit.structJRockit.memThroughput=TraiterJRockit.circleArray.throughput
      }

        // remplissage   fullGCgcDuration
    TraiterJRockit.structJRockit.fullGCDuration = Double.NaN
    reg1 = """:\s+GC.+\d+(\.|,)\d+\s+ms""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      //  println("ext1.get="+ext1.get)
      var ext2 = """\d+\.\d+""".r.findFirstIn(ext1.get.replace(",", "."))

      TraiterJRockit.structJRockit.fullGCDuration = ext2.get.toDouble

    }
       
       
    // remplissage   gcDuration
    TraiterJRockit.structJRockit.gcDuration = Double.NaN
    reg1 = """\d+(\.|,)\d+\s+ms""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      //  println("ext1.get="+ext1.get)
      var ext2 = """\d+\.\d+""".r.findFirstIn(ext1.get.replace(",", "."))

      TraiterJRockit.structJRockit.gcDuration = ext2.get.toDouble

    }

    // remplissage  throughput
    if (!TraiterJRockit.structJRockit.gcDuration.isNaN) {
      // println("throughput minorGC minorDuration="+TraiterJRockit.structJRockit.minorGCDuration+" ; dateCurrent ="+dateCurrent+ " ; datePrev="+TraiterJRockit.dateInMillis)
      TraiterJRockit.structJRockit.throughput = 100 * (1 - (TraiterJRockit.structJRockit.gcDuration / (dateCurrent - TraiterJRockit.dateInMillisGC).toDouble))
      if (tabStr(0).contains("GC") ) TraiterJRockit.dateInMillisGC = dateCurrent
    }

    // On ne mesure les dates  qu entre les GC
    if (tabStr(0).contains("GC") ) TraiterJRockit.dateInMillis = dateCurrent
    TraiterJRockit.enrCurrent = tabStr(0)
   
  }

}
object TraiterJRockit {

  var structJRockit: StructJRockit = new StructJRockit
  var circleArray:CircleArray=null
  var enrCurrent: String = null
  
  var dateInMillis: Long = 0

  var dateInMillisGC = 0L
}

class StructJRockit {
var memThroughput=Double.NaN
  var sizeHeapBefore = Double.NaN
  var sizeHeapAfter = Double.NaN
  var gcDuration = Double.NaN
  var throughput = Double.NaN
   var nurseryGCsizeHeapBefore=Double.NaN
   var nurseryGCsizeHeapAfter=Double.NaN
   var nurseryGCDuration=Double.NaN
    var fullGCsizeHeapBefore=Double.NaN
      var fullGCsizeHeapAfter=Double.NaN
    var  fullGCDuration=Double.NaN;
}

