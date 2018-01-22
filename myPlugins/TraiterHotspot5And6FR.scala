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
import java.util.Locale
class TraiterHotspot5And6FR {
  def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary

    TraiterHotspot5And6FR.structHotSpot = null
    TraiterHotspot5And6FR.enrCurrent = null
    TraiterHotspot5And6FR.dateInMillis = 0
    TraiterHotspot5And6FR.dateInMillisFullGC=0
    TraiterHotspot5And6FR.dateInMillisMinorGC=0
    TraiterHotspot5And6FR.isStructFilled = false
    TraiterHotspot5And6FR.circleArray=new CircleArray(10)
  }
  def retour(tabStr: Array[String]): Double =
    {

      if (TraiterHotspot5And6FR.enrCurrent == null) {
        TraiterHotspot5And6FR.enrCurrent = tabStr(0)
        TraiterHotspot5And6FR.isStructFilled = false
         TraiterHotspot5And6FR.dateInMillis = 0
         TraiterHotspot5And6FR.dateInMillisFullGC=0
         TraiterHotspot5And6FR.dateInMillisMinorGC=0
        traiterEnr(tabStr)
      } else {
        if (TraiterHotspot5And6FR.enrCurrent != tabStr(0)) {
          TraiterHotspot5And6FR.isStructFilled = false
          traiterEnr(tabStr)
        }
      }

      // Faire les retours ici
      tabStr(1) match {
        case "sizeYoungGenerationBefore" => TraiterHotspot5And6FR.structHotSpot.sizeYoungGenerationBefore
        case "sizeYoungGenerationAfter" => TraiterHotspot5And6FR.structHotSpot.sizeYoungGenerationAfter
        case "sizeOldGenerationBefore" => TraiterHotspot5And6FR.structHotSpot.sizeOldGenerationBefore
        case "sizeOldGenerationAfter" => TraiterHotspot5And6FR.structHotSpot.sizeOldGenerationAfter
        case "sizeHeapBefore" => TraiterHotspot5And6FR.structHotSpot.sizeHeapBefore
        case "sizeHeapAfter" => TraiterHotspot5And6FR.structHotSpot.sizeHeapAfter
        case "sizePermGenBefore" => TraiterHotspot5And6FR.structHotSpot.sizePermGenBefore
        case "sizePermGenAfter" => TraiterHotspot5And6FR.structHotSpot.sizePermGenAfter
        case "minorGCDuration" => TraiterHotspot5And6FR.structHotSpot.minorGCDuration
        case "throughput" => TraiterHotspot5And6FR.structHotSpot.throughput
        case "fullGCDuration" => TraiterHotspot5And6FR.structHotSpot.fullGCDuration
        case "cmsConcurrentMarkDuration" => TraiterHotspot5And6FR.structHotSpot.cmsConcurrentMarkDuration
        case "cmsConcurrentSweep" => TraiterHotspot5And6FR.structHotSpot.cmsConcurrentSweep
        case "cmsConcurrentPreclean" => TraiterHotspot5And6FR.structHotSpot.cmsConcurrentPreclean
        case "fullGCSizeYoungGenerationBefore" => TraiterHotspot5And6FR.structHotSpot.fullGCSizeYoungGenerationBefore
        case "fullGCSizeYoungGenerationAfter" => TraiterHotspot5And6FR.structHotSpot.fullGCSizeYoungGenerationAfter
        case "fullGCSizeOldGenerationBefore" => TraiterHotspot5And6FR.structHotSpot.fullGCSizeOldGenerationBefore
        case "fullGCSizeOldGenerationAfter" => TraiterHotspot5And6FR.structHotSpot.fullGCSizeOldGenerationAfter
        case "fullGCSizeHeapBefore" => TraiterHotspot5And6FR.structHotSpot.fullGCSizeHeapBefore
        case "fullGCSizeHeapAfter" => TraiterHotspot5And6FR.structHotSpot.fullGCSizeHeapAfter
        case "sysTimeSpent" => TraiterHotspot5And6FR.structHotSpot.sysTimeSpent
        case "userTimeSpent" => TraiterHotspot5And6FR.structHotSpot.userTimeSpent
        case "realTimeSpent" => TraiterHotspot5And6FR.structHotSpot.realTimeSpent
        case "memThroughput" => TraiterHotspot5And6FR.structHotSpot.memThroughput
        case _ => Double.NaN

      }

    }

  private def traiterEnr(tabStr: Array[String]) {
    // trouver la date:

    val regDate = """\d+,\d+:""".r
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

    //Locale.setDefault(Locale.FRENCH)
	 // println("enr="+tabStr(0))
    TraiterHotspot5And6FR.structHotSpot = new StructHotspot5And6FR
    // remplissage  sizeYoungGenerationBefore
    var reg1 = """\[GC.+?(DefNew:|ParNew:|PSYoungGen:)\s+\d+(K|M)""".r
    var ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterHotspot5And6FR.structHotSpot.sizeYoungGenerationBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6FR.structHotSpot.sizeYoungGenerationBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }

    // remplissage  sizeYoungGenerationAfter
    reg1 = """\[GC.+?(DefNew:|ParNew:|PSYoungGen:)\s+\d+(K|M)->\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterHotspot5And6FR.structHotSpot.sizeYoungGenerationAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6FR.structHotSpot.sizeYoungGenerationAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }

    // remplissage  sizeHeapBefore
    reg1 = """\[(GC|Full\s+GC).+?(DefNew:|ParNew:|PSYoungGen:|Tenured:|PSOldGen:)[^\]]+\]\s+\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterHotspot5And6FR.structHotSpot.sizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6FR.structHotSpot.sizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
    // remplissage  sizeHeapAfter
    reg1 = """\[(GC|Full\s+GC).+?(DefNew:|ParNew:|PSYoungGen:|Tenured:|PSOldGen:)[^\]]+\]\s+\d+(K|M)->\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterHotspot5And6FR.structHotSpot.sizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6FR.structHotSpot.sizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
  // remplissage Throughput Memoire
     // println("Avant Traitement throughput memor"+TraiterHotspot5And6.structHotSpot.sizeHeapBefore+ " " +TraiterHotspot5And6.structHotSpot.sizeHeapAfter)
       TraiterHotspot5And6FR.structHotSpot.memThroughput=Double.NaN
      if( !(TraiterHotspot5And6FR.structHotSpot.sizeHeapBefore.isNaN()) && !(TraiterHotspot5And6FR.structHotSpot.sizeHeapAfter.isNaN()))
      {
      var sweeped= TraiterHotspot5And6FR.structHotSpot.sizeHeapBefore-TraiterHotspot5And6FR.structHotSpot.sizeHeapAfter
     // println("Traitement throughput memor")
      TraiterHotspot5And6FR.circleArray.put((dateCurrent,sweeped))
      TraiterHotspot5And6FR.structHotSpot.memThroughput=TraiterHotspot5And6FR.circleArray.throughput
      }
    // remplissage   sizeOldGenerationBefore
    if (!TraiterHotspot5And6FR.structHotSpot.sizeHeapBefore.isNaN && !TraiterHotspot5And6FR.structHotSpot.sizeYoungGenerationBefore.isNaN) {

      TraiterHotspot5And6FR.structHotSpot.sizeOldGenerationBefore = TraiterHotspot5And6FR.structHotSpot.sizeHeapBefore - TraiterHotspot5And6FR.structHotSpot.sizeYoungGenerationBefore
    }

    // remplissage   sizeOldGenerationAfter
    if (!TraiterHotspot5And6FR.structHotSpot.sizeHeapAfter.isNaN && !TraiterHotspot5And6FR.structHotSpot.sizeYoungGenerationAfter.isNaN) {

      TraiterHotspot5And6FR.structHotSpot.sizeOldGenerationAfter = TraiterHotspot5And6FR.structHotSpot.sizeHeapAfter - TraiterHotspot5And6FR.structHotSpot.sizeYoungGenerationAfter
    }

    // remplissage   sizePermGenBefore

    reg1 = """(CMS Perm :|PSPermGen:|Perm\s+:)\s+\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterHotspot5And6FR.structHotSpot.sizePermGenBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6FR.structHotSpot.sizePermGenBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
    // remplissage   sizePermGenAfter
    reg1 = """(CMS Perm :|PSPermGen:|Perm\s+:)\s+\d+(K|M)->\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterHotspot5And6FR.structHotSpot.sizePermGenAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6FR.structHotSpot.sizePermGenAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }

    // remplissage   minorGCDuration
    TraiterHotspot5And6FR.structHotSpot.minorGCDuration=Double.NaN
    reg1 = """\[GC.+?(\[Times|\]\s*$)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
    //  println("ext1.get="+ext1.get)
      var ext2 = """\d+,\d+\s*secs\]\s*(\[Times$|$)""".r.findFirstIn(ext1.get)
      var ext3 = """\d+,\d+""".r.findFirstIn(ext2.get)
      TraiterHotspot5And6FR.structHotSpot.minorGCDuration = ext3.get.replace(",",".").toDouble * 1000 

    }
    // remplissage   fullGCDuration
 TraiterHotspot5And6FR.structHotSpot.fullGCDuration=Double.NaN
    reg1 = """Full\s+GC.+?(\[Times|\]\s*$)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+,\d+\s+secs\]\s*(\[Times$|$)""".r.findFirstIn(ext1.get)
      var ext3 = """\d+,\d+""".r.findFirstIn(ext2.get)
      //  println("fullGC ext3.get="+ext3.get)
      // println("Double eext3.get.toDouble * 1000="+ext3.get.toDouble * 1000D)
      TraiterHotspot5And6FR.structHotSpot.fullGCDuration = ext3.get.replace(",",".").toDouble * 1000

    }
    // remplissage  throughput
    if (!TraiterHotspot5And6FR.structHotSpot.minorGCDuration.isNaN) {
     // println("throughput minorGC minorDuration="+TraiterHotspot5And6FR.structHotSpot.minorGCDuration+" ; dateCurrent ="+dateCurrent+ " ; datePrev="+TraiterHotspot5And6FR.dateInMillis)
      TraiterHotspot5And6FR.structHotSpot.throughput = 100 *(1- (TraiterHotspot5And6FR.structHotSpot.minorGCDuration / (dateCurrent - TraiterHotspot5And6FR.dateInMillisMinorGC).toDouble))
     if(tabStr(0).contains("[GC") ||tabStr(0).contains("[Full GC") )    TraiterHotspot5And6FR.dateInMillisMinorGC = dateCurrent
    }
    if (!TraiterHotspot5And6FR.structHotSpot.fullGCDuration.isNaN) {
     //  println("throughput majorGC fullGCDuration="+TraiterHotspot5And6FR.structHotSpot.fullGCDuration+" ; dateCurrent ="+dateCurrent+ " ; datePrev="+TraiterHotspot5And6FR.dateInMillis)
     
      TraiterHotspot5And6FR.structHotSpot.throughput = 100 * (1 - (TraiterHotspot5And6FR.structHotSpot.fullGCDuration / (dateCurrent - TraiterHotspot5And6FR.dateInMillisFullGC).toDouble))
      if(tabStr(0).contains("[GC") ||tabStr(0).contains("[Full GC") )    TraiterHotspot5And6FR.dateInMillisFullGC = dateCurrent
    }

    // remplissage    cmsConcurrentMarkDuration

    reg1 = """CMS-concurrent-mark:\s+[^\]]+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+,\d+\s+secs""".r.findFirstIn(ext1.get)

      TraiterHotspot5And6FR.structHotSpot.cmsConcurrentMarkDuration = """\d+,\d+""".r.findFirstIn(ext2.get).get.replace(",",".").toDouble * 1000

    }
    // remplissage    cmsConcurrentSweep
    reg1 = """CMS-concurrent-sweep:\s+[^\]]+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+,\d+\s+secs""".r.findFirstIn(ext1.get)

      TraiterHotspot5And6FR.structHotSpot.cmsConcurrentSweep = """\d+,\d+""".r.findFirstIn(ext2.get).get.replace(",",".").toDouble * 1000

    }
    // remplissage   cmsConcurrentPreclean
    reg1 = """CMS-concurrent-preclean:\s+[^\]]+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+\.\d+\s+secs""".r.findFirstIn(ext1.get)

      TraiterHotspot5And6FR.structHotSpot.cmsConcurrentPreclean = """\d+,\d+""".r.findFirstIn(ext2.get).get.replace(",",".").toDouble * 1000

    }

    // remplissage  fullGCSizeOldGenerationBefore
    reg1 = """Full GC.+(PSOldGen:|CMS:|Tenured:)\s+\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterHotspot5And6FR.structHotSpot.fullGCSizeOldGenerationBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6FR.structHotSpot.fullGCSizeOldGenerationBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
    // remplissage  fullGCSizeOldGenerationAfter
    reg1 = """Full GC.+(PSOldGen:|CMS:|Tenured:)\s+\d+(K|M)->\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterHotspot5And6FR.structHotSpot.fullGCSizeOldGenerationAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6FR.structHotSpot.fullGCSizeOldGenerationAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
    // remplissage  fullGCSizeHeapBefore
    reg1 = """Full GC.+(PSOldGen:|CMS:|Tenured:)[^\]]+?\]\s+\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterHotspot5And6FR.structHotSpot.fullGCSizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6FR.structHotSpot.fullGCSizeHeapBefore = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }

    // remplissage  fullGCSizeHeapAfter
    reg1 = """Full GC.+(PSOldGen:|CMS:|Tenured:)[^\]]+?\]\s+\d+(K|M)->\d+(K|M)""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+(K|M)$""".r.findFirstIn(ext1.get)
      if (ext2.get.contains("K")) {
        TraiterHotspot5And6FR.structHotSpot.fullGCSizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024
      } else {
        TraiterHotspot5And6FR.structHotSpot.fullGCSizeHeapAfter = """\d+""".r.findFirstIn(ext2.get).get.toDouble * 1024 * 1024
      }
    }
    // remplissage fullGCSizeYoungGenerationBefore
      TraiterHotspot5And6FR.structHotSpot.fullGCSizeYoungGenerationBefore =Double.NaN
    if (!TraiterHotspot5And6FR.structHotSpot.fullGCSizeHeapBefore.isNaN && !TraiterHotspot5And6FR.structHotSpot.fullGCSizeOldGenerationBefore.isNaN) {

      TraiterHotspot5And6FR.structHotSpot.fullGCSizeYoungGenerationBefore = TraiterHotspot5And6FR.structHotSpot.fullGCSizeHeapBefore - TraiterHotspot5And6FR.structHotSpot.fullGCSizeOldGenerationBefore
    }
    // remplissage fullGCSizeYoungGenerationAfter
       TraiterHotspot5And6FR.structHotSpot.fullGCSizeYoungGenerationAfter  =Double.NaN
    if (!TraiterHotspot5And6FR.structHotSpot.fullGCSizeHeapAfter.isNaN && !TraiterHotspot5And6FR.structHotSpot.fullGCSizeOldGenerationAfter.isNaN) {

      TraiterHotspot5And6FR.structHotSpot.fullGCSizeYoungGenerationAfter = TraiterHotspot5And6FR.structHotSpot.fullGCSizeHeapAfter - TraiterHotspot5And6FR.structHotSpot.fullGCSizeOldGenerationAfter
    }

    // Test de presence des temps syst, user et real
    reg1 = """\[Times[^\]]+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      // remplissage  sysTimeSpent
      // [Times: user=0.13 sys=0.00, real=0.03 secs] 
      var reg2 = """sys=\d+,\d+""".r
      var ext2 = reg2.findFirstIn(ext1.get)
      TraiterHotspot5And6FR.structHotSpot.sysTimeSpent = """\d+,\d+""".r.findFirstIn(ext2.get).get.replace(",",".").toDouble * 1000
      // remplissage  userTimeSpent
      reg2 = """user=\d+,\d+""".r
      ext2 = reg2.findFirstIn(ext1.get)
      TraiterHotspot5And6FR.structHotSpot.userTimeSpent = """\d+,\d+""".r.findFirstIn(ext2.get).get.replace(",",".").toDouble * 1000
      // remplissage  realTimeSpent
      reg2 = """real=\d+,\d+""".r
      ext2 = reg2.findFirstIn(ext1.get)
      TraiterHotspot5And6FR.structHotSpot.realTimeSpent = """\d+,\d+""".r.findFirstIn(ext2.get).get.replace(",",".").toDouble * 1000
    }

    // On ne mesure les dates  qu entre les GC
    if(tabStr(0).contains("[GC") ||tabStr(0).contains("[Full GC") )    TraiterHotspot5And6FR.dateInMillis = dateCurrent
    TraiterHotspot5And6FR.enrCurrent = tabStr(0)
    TraiterHotspot5And6FR.isStructFilled = true
  }

}
object TraiterHotspot5And6FR{

  var structHotSpot: StructHotspot5And6FR = null
  var circleArray:CircleArray=null
  var enrCurrent: String = null
  var isStructFilled = false
  var dateInMillis: Long = 0
 var dateInMillisFullGC=0L
    var dateInMillisMinorGC=0L
}

class StructHotspot5And6FR {
  var sizeYoungGenerationBefore = Double.NaN
  var sizeYoungGenerationAfter = Double.NaN
  var sizeOldGenerationBefore = Double.NaN
  var sizeOldGenerationAfter = Double.NaN
  var sizeHeapBefore = Double.NaN
  var sizeHeapAfter = Double.NaN
  var sizePermGenBefore = Double.NaN
  var sizePermGenAfter = Double.NaN
  var minorGCDuration = Double.NaN
  var throughput = Double.NaN
  var fullGCDuration = Double.NaN

  var cmsConcurrentMarkDuration = Double.NaN
  var cmsConcurrentSweep = Double.NaN
  var cmsConcurrentPreclean = Double.NaN
  var fullGCSizeYoungGenerationBefore = Double.NaN
  var fullGCSizeYoungGenerationAfter = Double.NaN
  var fullGCSizeOldGenerationBefore = Double.NaN
  var fullGCSizeOldGenerationAfter = Double.NaN
  var fullGCSizeHeapBefore = Double.NaN
  var fullGCSizeHeapAfter = Double.NaN
  var sysTimeSpent = Double.NaN
  var userTimeSpent = Double.NaN
  var realTimeSpent = Double.NaN
   var memThroughput=Double.NaN
}

