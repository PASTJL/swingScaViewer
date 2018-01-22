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

class TraiterJVMIBM16genconR26 {
  def metInit(tab: Array[String] = null) {
    // To reinitialise static variable if necessary

    TraiterJVMIBM16genconR26.structJVMIBM16genconR26 = null
    TraiterJVMIBM16genconR26.enrCurrent = null
    TraiterJVMIBM16genconR26.dateInMillis = 0
    TraiterJVMIBM16genconR26.circleArray = new CircleArray(10)
    TraiterJVMIBM16genconR26.circleArrayGlobalGC = new CircleArray(10)
    TraiterJVMIBM16genconR26.circleArrayScavengerGC = new CircleArray(10)
  }
  def retour(tabStr: Array[String]): Double =
    {

      /* argument 0 l'enregistrement a traiter
     * argument 1 le nom de la valeur a obtenir
     * argument 2 la regex de la date
     * argument 3 la java date format de  enregistrement
     */
      if (TraiterJVMIBM16genconR26.enrCurrent == null) {
        TraiterJVMIBM16genconR26.enrCurrent = tabStr(0)

        TraiterJVMIBM16genconR26.dateInMillis = 0

        traiterEnr(tabStr)

      } else {
        if (TraiterJVMIBM16genconR26.enrCurrent != tabStr(0)) {

          traiterEnr(tabStr)
          TraiterJVMIBM16genconR26.enrCurrent = tabStr(0)
        }
      }

      // Faire les retours ici
      tabStr(1) match {
        case "sizeNurseryBefore" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryBefore
        case "sizeNurseryAfter" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryAfter
        case "sizeTenuredBefore" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredBefore
        case "sizeTenuredAfter" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredAfter
        case "sizeHeapBefore" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapBefore
        case "sizeHeapAfter" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapAfter
        case "globalGCDuration" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.globalGCDuration
        case "scavengerGCDuration" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.scavengerGCDuration
        case "throughput" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.throughput
        case "softReferencesCandidates" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesCandidates;
        case "softReferencesCleared" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesCleared;
        case "softReferencesEnqueued" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesEnqueued;
        case "weakReferencesCandidates" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesCandidates;
        case "weakReferencesCleared" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesCleared;
        case "weakReferencesEnqueued" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesEnqueued;
        case "phantomReferencesCandidates" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesCandidates;
        case "phantomReferencesCleared" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesCleared;
        case "phantomReferencesEnqueued" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesEnqueued;
        case "memThroughput" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.memThroughput
        case "gcFrequency" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.gcFrequency
        case "gcGlobalFrequency" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.gcGlobalFrequency
        case "gcScavengerFrequency" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.gcScavengerFrequency
        case "mark" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.mark
        case "classunload" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.classunload
        case "sweep" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sweep
        case "compact" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.compact
        case "card-cleaning" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.card_cleaning
        case "tracing" => TraiterJVMIBM16genconR26.structJVMIBM16genconR26.tracing
        case _ => Double.NaN

      }

    }

  private def traiterEnr(tabStr: Array[String]) {
    // trouver la date:

    // val regDate = """\d+\.\d+:""".r
    val regDate = tabStr(2).r
    val sdf = new SimpleDateFormat(tabStr(3))
    var match0 = regDate.findFirstIn(tabStr(0))
    if (None != match0) {
      // extraire la date
      var ext1 = match0.get

      var dateCurrentInMillis = sdf.parse(ext1).getTime()

      remplirStruct(dateCurrentInMillis, tabStr)

    }
  }

  private def remplirStruct(dateCurrent: Long, tabStr: Array[String]) {

    // println("enr="+tabStr(0))
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26 = new StructJVMIBM16genconR26
    // remplissage  sizeNurseryBefore
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryBefore = Double.NaN
    var reg1 = """<gc-start.+?nursery" free="\d+""".r
    var reg2 = """<gc-start.+?nursery" free=.+?total="\d+""".r
    var ext1 = reg1.findFirstIn(tabStr(0))

    if (None != ext1) {

      var ext2 = reg2.findFirstIn(tabStr(0)).get

      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryBefore = """\d+$""".r.findFirstIn(ext2).get.toDouble -
        """\d+$""".r.findFirstIn(ext1.get).get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryBefore = Double.NaN
    }

    // remplissage  sizeNurseryAfter
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryAfter = Double.NaN
    reg1 = """<gc-end.+?nursery" free="\d+""".r
    reg2 = """<gc-end.+?nursery" free=.+?total="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = reg2.findFirstIn(tabStr(0)).get
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryAfter = """\d+$""".r.findFirstIn(ext2).get.toDouble -
        """\d+$""".r.findFirstIn(ext1.get).get.toDouble
    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryAfter = Double.NaN
    }
    // remplissage  sizeTenuredBefore
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredBefore = Double.NaN
    reg1 = """<gc-start.+?tenure.?" free="\d+""".r
    reg2 = """<gc-start.+?tenure.?" free=.+?total="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = reg2.findFirstIn(tabStr(0)).get
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredBefore = """\d+$""".r.findFirstIn(ext2).get.toDouble -
        """\d+$""".r.findFirstIn(ext1.get).get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredBefore = Double.NaN
    }

    // remplissage  sizeTenuredAfter
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredAfter = Double.NaN
    reg1 = """<gc-end.+?tenure.?" free="\d+""".r
    reg2 = """<gc-end.+?tenure.?" free=.+?total="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = reg2.findFirstIn(tabStr(0)).get
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredAfter = """\d+$""".r.findFirstIn(ext2).get.toDouble -
        """\d+$""".r.findFirstIn(ext1.get).get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredAfter = Double.NaN
    }

    // remplissage   sizeHeapBefore
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapBefore = Double.NaN
    if (!TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredBefore.isNaN) {
      if (!TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryBefore.isNaN) {
        TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapBefore = TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryBefore +
          TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredBefore
      } else {
        TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapBefore = TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredBefore
      }

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapBefore = Double.NaN
    }

    // remplissage   sizeHeapAfter
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapAfter = Double.NaN
    if (!TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredAfter.isNaN) {
      if (!TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryAfter.isNaN) {
        TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapAfter = TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeNurseryAfter +
          TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredAfter
      } else {
        TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapAfter = TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeTenuredAfter
      }

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapAfter = Double.NaN
    }

    // remplissage Throughput Memoire et gcFrequency

    // println("Avant Traitement throughput memor"+TraiterHotspot5And6.structHotSpot.sizeHeapBefore+ " " +TraiterHotspot5And6.structHotSpot.sizeHeapAfter)
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.memThroughput = Double.NaN
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.gcFrequency = Double.NaN
    if (!(TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapBefore.isNaN()) && !(TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapAfter.isNaN())) {
      var sweeped = TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapBefore - TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sizeHeapAfter
      // println("Traitement throughput memor")
      TraiterJVMIBM16genconR26.circleArray.put((dateCurrent, sweeped))
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.memThroughput = TraiterJVMIBM16genconR26.circleArray.throughput
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.gcFrequency = TraiterJVMIBM16genconR26.circleArray.freq
    }

    // remplissage   scavengerGCDuration
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.scavengerGCDuration = Double.NaN
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.gcScavengerFrequency = Double.NaN
    reg1 = """<gc-end.+?type="scavenge".+?durationms="\d+""".r

    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      //  println("ext1.get="+ext1.get)
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)
      TraiterJVMIBM16genconR26.circleArrayScavengerGC.put((dateCurrent, 1))
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.scavengerGCDuration = ext2.get.toDouble
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.gcScavengerFrequency = TraiterJVMIBM16genconR26.circleArrayScavengerGC.freq

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.scavengerGCDuration = Double.NaN
    }

    // remplissage   globalGCDuration et gcGlobalFrequency
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.globalGCDuration = Double.NaN
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.gcGlobalFrequency = Double.NaN
    reg1 = """<gc-end.+?type="global".+?durationms="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)
      TraiterJVMIBM16genconR26.circleArrayGlobalGC.put((dateCurrent, 1))
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.globalGCDuration = ext2.get.toDouble
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.gcGlobalFrequency = TraiterJVMIBM16genconR26.circleArrayGlobalGC.freq

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.globalGCDuration = Double.NaN
    }

    // remplissage  throughput
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.throughput = Double.NaN
    if (!TraiterJVMIBM16genconR26.structJVMIBM16genconR26.scavengerGCDuration.isNaN) {
      // println("throughput minorGC minorDuration="+TraiterJVMIBM16gencon.structJVMIBM16genconR26.minorGCDuration+" ; dateCurrent ="+dateCurrent+ " ; datePrev="+TraiterJVMIBM16gencon.dateInMillis)
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.throughput = 100 * (1 - (TraiterJVMIBM16genconR26.structJVMIBM16genconR26.scavengerGCDuration / (TraiterJVMIBM16genconR26.structJVMIBM16genconR26.scavengerGCDuration + dateCurrent - TraiterJVMIBM16genconR26.dateInMillis).toDouble))
      TraiterJVMIBM16genconR26.dateInMillis = dateCurrent + TraiterJVMIBM16genconR26.structJVMIBM16genconR26.scavengerGCDuration.toLong
    }
    if (!TraiterJVMIBM16genconR26.structJVMIBM16genconR26.globalGCDuration.isNaN) {
      //  println("throughput majorGC fullGCDuration="+TraiterJVMIBM16gencon.structJVMIBM16genconR26.fullGCDuration+" ; dateCurrent ="+dateCurrent+ " ; datePrev="+TraiterJVMIBM16gencon.dateInMillis)

      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.throughput = 100 * (1 - (TraiterJVMIBM16genconR26.structJVMIBM16genconR26.globalGCDuration / (TraiterJVMIBM16genconR26.structJVMIBM16genconR26.globalGCDuration + dateCurrent - TraiterJVMIBM16genconR26.dateInMillis).toDouble))
      TraiterJVMIBM16genconR26.dateInMillis = dateCurrent + TraiterJVMIBM16genconR26.structJVMIBM16genconR26.globalGCDuration.toLong
    }
    if (TraiterJVMIBM16genconR26.structJVMIBM16genconR26.throughput < 0) {
      // on mets a 0
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.throughput = 0
    }
    if (TraiterJVMIBM16genconR26.structJVMIBM16genconR26.throughput > 100) {
      // on mets a 100
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.throughput = 100
    }

    //Remplissage softReferences candidates gc
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesCandidates = Double.NaN;

    reg1 = """type="soft".+?candidates="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesCandidates = ext2.get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesCandidates = Double.NaN
    }

    //Remplissage softReferences cleared gc
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesCleared = Double.NaN;
    reg1 = """type="soft".+?cleared="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesCleared = ext2.get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesCleared = Double.NaN
    }

    //Remplissage softReferences enqueued
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesEnqueued = Double.NaN;
    reg1 = """type="soft".+?enqueued="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesEnqueued = ext2.get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.softReferencesEnqueued = Double.NaN
    }

    //Remplissage weakReferences candidates gc
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesCandidates = Double.NaN;
    reg1 = """type="weak".+?candidates="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesCandidates = ext2.get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesCandidates = Double.NaN
    }

    //Remplissage weakReferences cleared gc
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesCleared = Double.NaN;
    reg1 = """type="weak".+?cleared="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesCleared = ext2.get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesCleared = Double.NaN
    }

    //Remplissage weakReferences enqueued
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesEnqueued = Double.NaN;
    reg1 = """type="weak".+?enqueued="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesEnqueued = ext2.get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.weakReferencesEnqueued = Double.NaN
    }

    //Remplissage phantomReferences candidates gc
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesCandidates = Double.NaN;
    reg1 = """type="phantom".+?candidates="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesCandidates = ext2.get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesCandidates = Double.NaN
    }

    //Remplissage phantomReferences cleared gc
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesCleared = Double.NaN;
    reg1 = """type="phantom".+?cleared="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesCleared = ext2.get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesCleared = Double.NaN
    }

    //Remplissage phantomReferences enqueued
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesEnqueued = Double.NaN;
    reg1 = """type="phantom".+?enqueued="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesEnqueued = ext2.get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.phantomReferencesEnqueued = Double.NaN
    }
    
    //TraiterJVMIBM16genconR26.structJVMIBM16genconR26.mark
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.mark = Double.NaN;
    reg1 = """type="mark".+?timems="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

     TraiterJVMIBM16genconR26.structJVMIBM16genconR26.mark = ext2.get.toDouble

    } else {
      TraiterJVMIBM16genconR26.structJVMIBM16genconR26.mark  = Double.NaN
    }
    
     //TraiterJVMIBM16genconR26.structJVMIBM16genconR26.classunload
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.classunload = Double.NaN;
    reg1 = """type="classunload".+?timems="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

     TraiterJVMIBM16genconR26.structJVMIBM16genconR26.classunload = ext2.get.toDouble

    } else {
     TraiterJVMIBM16genconR26.structJVMIBM16genconR26.classunload  = Double.NaN
    }
    
     // TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sweep
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sweep= Double.NaN;
    reg1 = """type="sweep".+?timems="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

     TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sweep= ext2.get.toDouble

    } else {
     TraiterJVMIBM16genconR26.structJVMIBM16genconR26.sweep= Double.NaN
    }
    
     // TraiterJVMIBM16genconR26.structJVMIBM16genconR26.compact
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.compact= Double.NaN;
    reg1 = """type="compact".+?timems="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

     TraiterJVMIBM16genconR26.structJVMIBM16genconR26.compact= ext2.get.toDouble

    } else {
     TraiterJVMIBM16genconR26.structJVMIBM16genconR26.compact= Double.NaN
    }
    
       //TraiterJVMIBM16genconR26.structJVMIBM16genconR26.card_cleaning
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.card_cleaning= Double.NaN;
    reg1 = """type="compact".+?timems="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

     TraiterJVMIBM16genconR26.structJVMIBM16genconR26.card_cleaning= ext2.get.toDouble

    } else {
     TraiterJVMIBM16genconR26.structJVMIBM16genconR26.card_cleaning= Double.NaN
    }  
       
      //TraiterJVMIBM16genconR26.structJVMIBM16genconR26.card_cleaning
   TraiterJVMIBM16genconR26.structJVMIBM16genconR26.card_cleaning= Double.NaN;
    reg1 = """type="card-cleaning".+?timems="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.card_cleaning= ext2.get.toDouble

    } else {
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.card_cleaning= Double.NaN
    }  
           
       // TraiterJVMIBM16genconR26.structJVMIBM16genconR26.tracing
    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.tracing= Double.NaN;
    reg1 = """type="tracing".+?timems="\d+""".r
    ext1 = reg1.findFirstIn(tabStr(0))
    if (None != ext1) {
      var ext2 = """\d+$""".r.findFirstIn(ext1.get)

    TraiterJVMIBM16genconR26.structJVMIBM16genconR26.tracing= ext2.get.toDouble

    } else {
     TraiterJVMIBM16genconR26.structJVMIBM16genconR26.tracing= Double.NaN
    }     
       
  }

}
object TraiterJVMIBM16genconR26 {

  var structJVMIBM16genconR26: StructJVMIBM16genconR26 = null
  var enrCurrent: String = null
  var circleArray: CircleArray = null
  var dateInMillis: Long = 0
  var circleArrayGlobalGC: CircleArray = null;
  var circleArrayScavengerGC: CircleArray = null

}

class StructJVMIBM16genconR26 {
  var sizeNurseryBefore = Double.NaN
  var sizeNurseryAfter = Double.NaN
  var sizeTenuredBefore = Double.NaN
  var sizeTenuredAfter = Double.NaN
  var gcFrequency = Double.NaN
  var gcGlobalFrequency = Double.NaN
  var gcScavengerFrequency = Double.NaN
  var sizeHeapBefore = Double.NaN
  var sizeHeapAfter = Double.NaN
  var scavengerGCDuration = Double.NaN
  var globalGCDuration = Double.NaN
  var throughput = Double.NaN

  var softReferencesCandidates = Double.NaN;
  var softReferencesCleared = Double.NaN;
  var softReferencesEnqueued = Double.NaN;
  var weakReferencesCandidates = Double.NaN;
  var weakReferencesCleared = Double.NaN;
  var weakReferencesEnqueued = Double.NaN;
  var phantomReferencesCandidates = Double.NaN;

  var phantomReferencesCleared = Double.NaN;
  var phantomReferencesEnqueued = Double.NaN; ;

  var memThroughput = Double.NaN
  var mark = Double.NaN
  var classunload = Double.NaN
  var sweep = Double.NaN
  var compact = Double.NaN
  var card_cleaning = Double.NaN
  var tracing = Double.NaN

}

