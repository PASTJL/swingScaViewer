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
class TraiterHotspot5And6Light {
  def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary

    TraiterHotspot5And6Light.structHotSpot = null
    TraiterHotspot5And6Light.enrCurrent = null
    TraiterHotspot5And6Light.dateInMillis = 0
    TraiterHotspot5And6Light.dateInMillisFullGC=0
    TraiterHotspot5And6Light.dateInMillisMinorGC=0
    TraiterHotspot5And6Light.isStructFilled = false
    TraiterHotspot5And6Light.circleArray=new CircleArray(10)
  }
  def retour(tabStr: Array[String]): Double =
    {

      if (TraiterHotspot5And6Light.enrCurrent == null) {
        TraiterHotspot5And6Light.enrCurrent = tabStr(0)
        TraiterHotspot5And6Light.isStructFilled = false
         TraiterHotspot5And6Light.dateInMillis = 0
         TraiterHotspot5And6Light.dateInMillisFullGC=0
         TraiterHotspot5And6Light.dateInMillisMinorGC=0
        traiterEnr(tabStr)
      } else {
        if (TraiterHotspot5And6Light.enrCurrent != tabStr(0)) {
          TraiterHotspot5And6Light.isStructFilled = false
          traiterEnr(tabStr)
        }
      }

      // Faire les retours ici
      tabStr(1) match {
        
        case "sizeHeapBefore" => TraiterHotspot5And6Light.structHotSpot.sizeHeapBefore
        case "sizeHeapAfter" => TraiterHotspot5And6Light.structHotSpot.sizeHeapAfter
        
        case "minorGCDuration" => TraiterHotspot5And6Light.structHotSpot.minorGCDuration
        case "throughput" => TraiterHotspot5And6Light.structHotSpot.throughput
        case "fullGCDuration" => TraiterHotspot5And6Light.structHotSpot.fullGCDuration
         case "memThroughput" => TraiterHotspot5And6Light.structHotSpot.memThroughput
        case _ => Double.NaN

      }

    }

  private def traiterEnr(tabStr: Array[String]) {
    // trouver la date:

    val regDate = """\d+(\.|,)\d+:""".r
    var match0 = regDate.findFirstIn(tabStr(0))
    if (None != match0) {
      // extraire la date
      var ext1 = match0.get
      ext1 = ext1.substring(0, ext1.indexOf(":")) // supprimer le dernier caractere
      //var dateCurrentInMillis = ext1.split("\\.")(0).toLong * 1000 + ext1.split("\\.")(1).toLong
      var dateCurrentInMillis = (ext1.replace(",",".").toDouble * 1000) .toLong
      remplirStruct(dateCurrentInMillis, tabStr)

    }
  }

  private def remplirStruct(dateCurrent: Long, tabStr: Array[String]) {
	  var ext1:Option[String]=None
	  var reg1="".r
 
 TraiterHotspot5And6Light.structHotSpot = new StructHotspot5And6Light
   

    // remplissage  sizeHeapBefore
    reg1 = """\[(GC|Full GC)\s+\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
     //   println("sizeHeapBefore : ext2="+ext2.get)
        TraiterHotspot5And6Light.structHotSpot.sizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6Light.structHotSpot.sizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
    // remplissage  sizeHeapAfter
    reg1 = """\[(GC|Full GC)\s+\d+(K|M)->\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterHotspot5And6Light.structHotSpot.sizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6Light.structHotSpot.sizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }

   // remplissage Throughput Memoire
     // println("Avant Traitement throughput memor"+TraiterHotspot5And6.structHotSpot.sizeHeapBefore+ " " +TraiterHotspot5And6.structHotSpot.sizeHeapAfter)
       TraiterHotspot5And6Light.structHotSpot.memThroughput=Double.NaN
      if( !(TraiterHotspot5And6Light.structHotSpot.sizeHeapBefore.isNaN()) && !(TraiterHotspot5And6Light.structHotSpot.sizeHeapAfter.isNaN()))
      {
      var sweeped= TraiterHotspot5And6Light.structHotSpot.sizeHeapBefore-TraiterHotspot5And6Light.structHotSpot.sizeHeapAfter
     // println("Traitement throughput memor")
      TraiterHotspot5And6Light.circleArray.put((dateCurrent,sweeped))
      TraiterHotspot5And6Light.structHotSpot.memThroughput=TraiterHotspot5And6Light.circleArray.throughput
      }

  
    // remplissage   minorGCDuration
    TraiterHotspot5And6Light.structHotSpot.minorGCDuration=Double.NaN
    reg1 = """\[GC.+?secs""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
    //  println("ext1.get="+ext1.get)
      var ext2 = """\d+(\.|,)\d+\s*secs""".r.findFirstIn(ext1.get)
      var ext3 = """\d+(\.|,)\d+""".r.findFirstIn(ext2.get)
      TraiterHotspot5And6Light.structHotSpot.minorGCDuration = ext3.get.replace(",",".").toDouble * 1000 

    }
    // remplissage   fullGCDuration
 TraiterHotspot5And6Light.structHotSpot.fullGCDuration=Double.NaN
    reg1 = """\[Full\s+GC.+?secs""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
    //  println("FullGCDuration: line="+tabStr(0));
      var ext2 = """\d+(\.|,)\d+\s+secs""".r.findFirstIn(ext1.get)
      var ext3 = """\d+(\.|,)\d+""".r.findFirstIn(ext2.get)
//        println("fullGC ext3.get="+ext3.get)
//       println("Double eext3.get.toDouble * 1000="+ext3.get.toDouble * 1000D)
//      System.exit(0)
       TraiterHotspot5And6Light.structHotSpot.fullGCDuration = ext3.get.replace(",",".").toDouble * 1000

    }
    // remplissage  throughput
    if (!TraiterHotspot5And6Light.structHotSpot.minorGCDuration.isNaN) {
     // println("throughput minorGC minorDuration="+TraiterHotspot5And6Light.structHotSpot.minorGCDuration+" ; dateCurrent ="+dateCurrent+ " ; datePrev="+TraiterHotspot5And6Light.dateInMillis)
      TraiterHotspot5And6Light.structHotSpot.throughput = 100 *(1- (TraiterHotspot5And6Light.structHotSpot.minorGCDuration / (dateCurrent - TraiterHotspot5And6Light.dateInMillisMinorGC).toDouble))
     if(tabStr(0).contains("[GC") ||tabStr(0).contains("[Full GC") )    TraiterHotspot5And6Light.dateInMillisMinorGC = dateCurrent
    }
    if (!TraiterHotspot5And6Light.structHotSpot.fullGCDuration.isNaN) {
     //  println("throughput majorGC fullGCDuration="+TraiterHotspot5And6Light.structHotSpot.fullGCDuration+" ; dateCurrent ="+dateCurrent+ " ; datePrev="+TraiterHotspot5And6Light.dateInMillis)
     
      TraiterHotspot5And6Light.structHotSpot.throughput = 100 * (1 - (TraiterHotspot5And6Light.structHotSpot.fullGCDuration / (dateCurrent - TraiterHotspot5And6Light.dateInMillisFullGC).toDouble))
      if(tabStr(0).contains("[GC") ||tabStr(0).contains("[Full GC") )    TraiterHotspot5And6Light.dateInMillisFullGC = dateCurrent
    }

    // On ne mesure les dates  qu entre les GC
    if(tabStr(0).contains("[GC") ||tabStr(0).contains("[Full GC") )    TraiterHotspot5And6Light.dateInMillis = dateCurrent
    TraiterHotspot5And6Light.enrCurrent = tabStr(0)
    TraiterHotspot5And6Light.isStructFilled = true
  }

}
object TraiterHotspot5And6Light {

  var structHotSpot: StructHotspot5And6Light = null
   var circleArray:CircleArray=null
  var enrCurrent: String = null
  var isStructFilled = false
  var dateInMillis: Long = 0
 var dateInMillisFullGC=0L
    var dateInMillisMinorGC=0L
}

class StructHotspot5And6Light {
  
  var sizeHeapBefore = Double.NaN
  var sizeHeapAfter = Double.NaN
   var minorGCDuration = Double.NaN
  var throughput = Double.NaN
  var fullGCDuration = Double.NaN
 var memThroughput=Double.NaN
 
}

