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
import java.text.SimpleDateFormat

class TraiterJVMIBM16gencon {
  def metInit(tab: Array[String] = null) {
    // To reinitialise static variable if necessary

    TraiterJVMIBM16gencon.structJVMIBM16gencon = null
    TraiterJVMIBM16gencon.enrCurrent = null
    TraiterJVMIBM16gencon.dateInMillis = 0
    TraiterJVMIBM16gencon.circleArray = new CircleArray(10)
    TraiterJVMIBM16gencon.circleArrayGlobalGC = new CircleArray(10)
    TraiterJVMIBM16gencon.circleArrayScavengerGC = new CircleArray(10)
  }
  def retour(tabStr: Array[String]): Double =
    {

      /* argument 0 l'enregistrement a traiter
     * argument 1 le nom de la valeur a obtenir
     * argument 2 la regex de la date
     * argument 3 la java date format de  enregistrement
     */
      if (TraiterJVMIBM16gencon.enrCurrent == null) {
        TraiterJVMIBM16gencon.enrCurrent = tabStr(0)

        TraiterJVMIBM16gencon.dateInMillis = 0
        TraiterJVMIBM16gencon.numEnr += 1
        traiterEnr(tabStr)

      } else {
        if (TraiterJVMIBM16gencon.enrCurrent != tabStr(0)) {
          TraiterJVMIBM16gencon.numEnr += 1
          traiterEnr(tabStr)
          TraiterJVMIBM16gencon.enrCurrent = tabStr(0)
        }
      }

      // Faire les retours ici
      tabStr(1) match {
        case "sizeNurseryBefore" => TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryBefore
        case "sizeNurseryAfter" => TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryAfter
        case "sizeTenuredBefore" => TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredBefore
        case "sizeTenuredAfter" => TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredAfter
        case "sizeHeapBefore" => TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapBefore
        case "sizeHeapAfter" => TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapAfter
        case "globalGCDuration" => TraiterJVMIBM16gencon.structJVMIBM16gencon.globalGCDuration
        case "scavengerGCDuration" => TraiterJVMIBM16gencon.structJVMIBM16gencon.scavengerGCDuration
        case "throughput" => TraiterJVMIBM16gencon.structJVMIBM16gencon.throughput
        case "softReferencesBefore" => TraiterJVMIBM16gencon.structJVMIBM16gencon.softReferencesBefore
        case "weakReferencesBefore" => TraiterJVMIBM16gencon.structJVMIBM16gencon.weakReferencesBefore
        case "phantomReferencesBefore" => TraiterJVMIBM16gencon.structJVMIBM16gencon.phantomReferencesBefore
        case "softReferencesAfter" => TraiterJVMIBM16gencon.structJVMIBM16gencon.softReferencesAfter
        case "weakReferencesAfter" => TraiterJVMIBM16gencon.structJVMIBM16gencon.weakReferencesAfter
        case "phantomReferencesAfter" => TraiterJVMIBM16gencon.structJVMIBM16gencon.phantomReferencesAfter
        case "memThroughput" => TraiterJVMIBM16gencon.structJVMIBM16gencon.memThroughput
        case "gcFrequency" => TraiterJVMIBM16gencon.structJVMIBM16gencon.gcFrequency
        case "gcGlobalFrequency" => TraiterJVMIBM16gencon.structJVMIBM16gencon.gcGlobalFrequency
        case "gcScavengerFrequency" => TraiterJVMIBM16gencon.structJVMIBM16gencon.gcScavengerFrequency
        case _ => Double.NaN

      }

    }

  private def traiterEnr(tabStr: Array[String]) {
    // trouver la date:

    // trouver intervalms
    var intervalms: Long = 0
    val regInterv = """<gc.+?intervalms="\d+\.\d+">""".r
    val match1 = regInterv.findFirstIn(tabStr(0))
    if (None != match1) {

      val ext1 = match1.get
      val tmpReg = """\d+\.\d+""".r
      val match3 = tmpReg.findFirstIn(ext1)

      if (None != match3) {

        intervalms = match3.get.toDouble.toLong
      }
    }
    // val regDate = """\d+\.\d+:""".r
    var dateCurrentInMillis: Long = 0

    if (TraiterJVMIBM16gencon.dateInMillis == 0) {
      val regDate = tabStr(2).r
      val sdf = new SimpleDateFormat(tabStr(3))
      var match0 = regDate.findFirstIn(tabStr(0))
      if (None != match0) {
        // extraire la date
        var ext1 = match0.get

        dateCurrentInMillis = sdf.parse(ext1).getTime()

      }

    } else {
      dateCurrentInMillis = TraiterJVMIBM16gencon.dateInMillisPrecedent + intervalms

    }
    TraiterJVMIBM16gencon.dateInMillisPrecedent = dateCurrentInMillis
    remplirStruct(dateCurrentInMillis, tabStr)
  }

  private def remplirStruct(dateCurrent: Long, tabStr: Array[String]) {

    // println("enr="+tabStr(0))
    TraiterJVMIBM16gencon.structJVMIBM16gencon = new StructJVMIBM16gencon

    // remplissage  sizeNurseryBefore
    TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryBefore = Double.NaN
    var reg1 = """nursery freebytes="\d+""".r
    var reg2 = """nursery freebytes=.+?totalbytes="\d+""".r
    var ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {

      var ext2 = reg2.findFirstIn(tabStr(0)).get
      TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryBefore = """\d+$""".r.findFirstIn(ext2).get.toDouble -
        """\d+$""".r.findFirstIn(ext1.get).get.toDouble

    } else {
      TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryBefore = Double.NaN
    }

    // remplissage  sizeNurseryAfter
    TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryAfter = Double.NaN
    reg1 = """</gc>.+?nursery freebytes="\d+""".r
    reg2 = """</gc>.+?nursery.+?totalbytes="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = reg2.findFirstIn(tabStr(0)).get
      TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryAfter = """\d+$""".r.findFirstIn(ext2).get.toDouble -
        """\d+$""".r.findFirstIn(ext1.get).get.toDouble
    } else {
      TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryAfter = Double.NaN
    }
    // remplissage  sizeTenuredBefore
    TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredBefore = Double.NaN
    reg1 = """tenured freebytes="\d+""".r
    reg2 = """tenured freebytes=.+?totalbytes="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = reg2.findFirstIn(tabStr(0)).get
      TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredBefore = """\d+$""".r.findFirstIn(ext2).get.toDouble -
        """\d+$""".r.findFirstIn(ext1.get).get.toDouble

    } else {
      TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredBefore = Double.NaN
    }

    // remplissage  sizeTenuredAfter
    TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredAfter = Double.NaN
    reg1 = """</gc>.+?tenured freebytes="\d+""".r
    reg2 = """</gc>.+?tenured freebytes=.+?totalbytes="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = reg2.findFirstIn(tabStr(0)).get
      TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredAfter = """\d+$""".r.findFirstIn(ext2).get.toDouble -
        """\d+$""".r.findFirstIn(ext1.get).get.toDouble

    } else {
      TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredAfter = Double.NaN
    }

    // remplissage   sizeHeapBefore
    TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapBefore = Double.NaN
    // Traitement du cas Single Heap ( monolithic)
    if (!TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredBefore.isNaN) {

      if (!TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryBefore.isNaN) {
        TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapBefore = TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryBefore +
          TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredBefore
      } else {
        TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapBefore = TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredBefore
      }
    } else {
      TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapBefore = Double.NaN
    }

    // remplissage   sizeHeapAfter
    TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapAfter = Double.NaN
    if (!TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredAfter.isNaN) {
      if (!TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryAfter.isNaN) {

        TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapAfter = TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeNurseryAfter +
          TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredAfter
      } else {
        TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapAfter = TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeTenuredAfter
      }
    } else {
      TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapAfter = Double.NaN
    }

    // remplissage Throughput Memoire et gcFrequency

    // println("Avant Traitement throughput memor"+TraiterHotspot5And6.structHotSpot.sizeHeapBefore+ " " +TraiterHotspot5And6.structHotSpot.sizeHeapAfter)
    TraiterJVMIBM16gencon.structJVMIBM16gencon.memThroughput = Double.NaN
    TraiterJVMIBM16gencon.structJVMIBM16gencon.gcFrequency = Double.NaN

    if (!(TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapBefore.isNaN()) && !(TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapAfter.isNaN())) {
      var sweeped = TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapBefore - TraiterJVMIBM16gencon.structJVMIBM16gencon.sizeHeapAfter
      // println("Traitement throughput memor")
      TraiterJVMIBM16gencon.circleArray.put((dateCurrent, sweeped))
      TraiterJVMIBM16gencon.structJVMIBM16gencon.memThroughput = TraiterJVMIBM16gencon.circleArray.throughput
      TraiterJVMIBM16gencon.structJVMIBM16gencon.gcFrequency = TraiterJVMIBM16gencon.circleArray.freq
    }

    // remplissage   scavengerGCDuration
    TraiterJVMIBM16gencon.structJVMIBM16gencon.scavengerGCDuration = Double.NaN
    TraiterJVMIBM16gencon.structJVMIBM16gencon.gcScavengerFrequency = Double.NaN
    reg1 = """gc type="scavenger".+?</gc>.+?totalms="\d+""".r

    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      //  println("ext1.get="+ext1.get)
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

      TraiterJVMIBM16gencon.structJVMIBM16gencon.scavengerGCDuration = ext2.get.toDouble
      TraiterJVMIBM16gencon.circleArrayScavengerGC.put((dateCurrent, 1))
      TraiterJVMIBM16gencon.structJVMIBM16gencon.gcScavengerFrequency = TraiterJVMIBM16gencon.circleArrayScavengerGC.freq

    } else {
      TraiterJVMIBM16gencon.structJVMIBM16gencon.scavengerGCDuration = Double.NaN
    }

    // remplissage   globalGCDuration
    TraiterJVMIBM16gencon.structJVMIBM16gencon.globalGCDuration = Double.NaN
    TraiterJVMIBM16gencon.structJVMIBM16gencon.gcGlobalFrequency = Double.NaN
    reg1 = """gc type="global".+?</gc>.+?totalms="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

      TraiterJVMIBM16gencon.structJVMIBM16gencon.globalGCDuration = ext2.get.toDouble
      TraiterJVMIBM16gencon.circleArrayGlobalGC.put((dateCurrent, 1))
      TraiterJVMIBM16gencon.structJVMIBM16gencon.gcGlobalFrequency = TraiterJVMIBM16gencon.circleArrayGlobalGC.freq

    } else {
      TraiterJVMIBM16gencon.structJVMIBM16gencon.globalGCDuration = Double.NaN
    }

    // remplissage  throughput
    var dateInMillisOld: Long = 0
    TraiterJVMIBM16gencon.structJVMIBM16gencon.throughput = Double.NaN
    if (!TraiterJVMIBM16gencon.structJVMIBM16gencon.scavengerGCDuration.isNaN) {
      // println("throughput minorGC minorDuration="+TraiterJVMIBM16gencon.structJVMIBM16gencon.minorGCDuration+" ; dateCurrent ="+dateCurrent+ " ; datePrev="+TraiterJVMIBM16gencon.dateInMillis)
      TraiterJVMIBM16gencon.structJVMIBM16gencon.throughput = 100 * (1 - (TraiterJVMIBM16gencon.structJVMIBM16gencon.scavengerGCDuration / (TraiterJVMIBM16gencon.structJVMIBM16gencon.scavengerGCDuration + dateCurrent - TraiterJVMIBM16gencon.dateInMillis).toDouble))
      TraiterJVMIBM16gencon.dateInMillis = dateCurrent + TraiterJVMIBM16gencon.structJVMIBM16gencon.scavengerGCDuration.toLong
    }
    if (!TraiterJVMIBM16gencon.structJVMIBM16gencon.globalGCDuration.isNaN) {
      //  println("throughput majorGC fullGCDuration="+TraiterJVMIBM16gencon.structJVMIBM16gencon.fullGCDuration+" ; dateCurrent ="+dateCurrent+ " ; datePrev="+TraiterJVMIBM16gencon.dateInMillis)

      TraiterJVMIBM16gencon.structJVMIBM16gencon.throughput = 100 * (1 - (TraiterJVMIBM16gencon.structJVMIBM16gencon.globalGCDuration / (TraiterJVMIBM16gencon.structJVMIBM16gencon.globalGCDuration + dateCurrent - TraiterJVMIBM16gencon.dateInMillis).toDouble))
      dateInMillisOld = TraiterJVMIBM16gencon.dateInMillis
      TraiterJVMIBM16gencon.dateInMillis = dateCurrent + TraiterJVMIBM16gencon.structJVMIBM16gencon.globalGCDuration.toLong
    }

    if (TraiterJVMIBM16gencon.structJVMIBM16gencon.throughput < 0) {
      // on mets a 0
      TraiterJVMIBM16gencon.structJVMIBM16gencon.throughput = 0
    }
    if (TraiterJVMIBM16gencon.structJVMIBM16gencon.throughput > 100) {
      // on mets a 100
      TraiterJVMIBM16gencon.structJVMIBM16gencon.throughput = 100
    }
    // remplissage   softReferencesBefore
    TraiterJVMIBM16gencon.structJVMIBM16gencon.softReferencesBefore = Double.NaN
    reg1 = """refs soft="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {

      TraiterJVMIBM16gencon.structJVMIBM16gencon.softReferencesBefore = """\d+$""".r.findFirstIn(ext1.get).get.toDouble
    }
    // remplissage   weakReferencesBefore
    TraiterJVMIBM16gencon.structJVMIBM16gencon.weakReferencesBefore = Double.NaN
    reg1 = """refs soft.+?weak="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {

      TraiterJVMIBM16gencon.structJVMIBM16gencon.weakReferencesBefore = """\d+$""".r.findFirstIn(ext1.get).get.toDouble
    }
    // remplissage   phantomReferencesBefore
    TraiterJVMIBM16gencon.structJVMIBM16gencon.phantomReferencesBefore = Double.NaN
    reg1 = """refs soft.+?phantom="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {

      TraiterJVMIBM16gencon.structJVMIBM16gencon.phantomReferencesBefore = """\d+$""".r.findFirstIn(ext1.get).get.toDouble
    }

    // remplissage   softReferencesAfter
    TraiterJVMIBM16gencon.structJVMIBM16gencon.softReferencesAfter = Double.NaN
    reg1 = """</gc>.+?refs soft="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {

      TraiterJVMIBM16gencon.structJVMIBM16gencon.softReferencesAfter = """\d+$""".r.findFirstIn(ext1.get).get.toDouble
    }
    // remplissage   weakReferencesAfter
    TraiterJVMIBM16gencon.structJVMIBM16gencon.weakReferencesAfter = Double.NaN
    reg1 = """</gc>.+?refs soft.+?weak="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {

      TraiterJVMIBM16gencon.structJVMIBM16gencon.weakReferencesAfter = """\d+$""".r.findFirstIn(ext1.get).get.toDouble
    }
    // remplissage   phantomReferencesAfter
    TraiterJVMIBM16gencon.structJVMIBM16gencon.phantomReferencesAfter = Double.NaN
    reg1 = """</gc>.+?refs soft.+?phantom="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {

      TraiterJVMIBM16gencon.structJVMIBM16gencon.phantomReferencesAfter = """\d+$""".r.findFirstIn(ext1.get).get.toDouble
    }

  }

}
object TraiterJVMIBM16gencon {

  var structJVMIBM16gencon: StructJVMIBM16gencon = null
  var enrCurrent: String = null
  var circleArray: CircleArray = null
  var circleArrayGlobalGC: CircleArray = null;
  var circleArrayScavengerGC: CircleArray = null

  var dateInMillis: Long = 0
  var dateInMillisPrecedent: Long = 0
  var numEnr = 0;
}

class StructJVMIBM16gencon {
  var sizeNurseryBefore = Double.NaN
  var sizeNurseryAfter = Double.NaN
  var sizeTenuredBefore = Double.NaN
  var sizeTenuredAfter = Double.NaN

  var sizeHeapBefore = Double.NaN
  var sizeHeapAfter = Double.NaN
  var scavengerGCDuration = Double.NaN
  var globalGCDuration = Double.NaN
  var gcFrequency = Double.NaN
  var gcGlobalFrequency = Double.NaN
  var gcScavengerFrequency = Double.NaN
  var throughput = Double.NaN
  var memThroughput = Double.NaN
  var softReferencesBefore = Double.NaN
  var weakReferencesBefore = Double.NaN
  var phantomReferencesBefore = Double.NaN
  var softReferencesAfter = Double.NaN
  var weakReferencesAfter = Double.NaN
  var phantomReferencesAfter = Double.NaN

}

