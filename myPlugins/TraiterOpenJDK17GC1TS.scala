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
import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.text.ParseException
import java.util.Locale

class TraiterOpenJDK17GC1TS {
  def metInit(tab: Array[String] = null) {
    // To reinitialise static variable if necessary
    TraiterOpenJDK17GC1TS.oldDateInMillis = 0L
    TraiterOpenJDK17GC1TS.structGC1 = null
    TraiterOpenJDK17GC1TS.circleArray = new CircleArray(10)
  }
  def retour(tabStr: Array[String]): Double =
    {
      //println("tabStr(0) dans plugin=\n" + tabStr(0))
      var retour = Double.NaN
      // System.out.println(" enr =|" + tabString[0] + "|");
      // String[0] => Enreg a traiter
      // String[1] parametre a retourner

      // Creation d'une structure d'acceuil pour cet enregistrement
      //
      if (tabStr(0) != TraiterOpenJDK17GC1TS.lastEnrg) {
        // println("tabStr(0) dans plugin= avant de remplir la structure")

        val tabDateStr = TraiterOpenJDK17GC1TS.patDate.findFirstIn(tabStr(0)).get.split("(,|\\.)")
        val date1 = tabDateStr(0).toLong * 1000 + tabDateStr(1).toLong

        TraiterOpenJDK17GC1TS.ecartDateMillis = date1 - TraiterOpenJDK17GC1TS.oldDateInMillis
        TraiterOpenJDK17GC1TS.oldDateInMillis = date1
        remplirStructure(tabStr)
        // calcul de l'ecart de temps entre le nouveau et l ancien enregistrement

        TraiterOpenJDK17GC1TS.lastEnrg = tabStr(0)

      }
      retour = Double.NaN

      if (tabStr(1).equals("minorGCEdenSizeBefore")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.minorGCEdenSizeBefore;
      } else if (tabStr(1).equals("minorGCEdenSizeAfter")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.minorGCEdenSizeAfter;
      } else if (tabStr(1).equals("minorGCduration")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.minorGCduration;
      } else if (tabStr(1).equals("parallelTime")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.parallelTime;

      } else if (tabStr(1).equals("minorGCSurvivorSizeBefore")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.minorGCSurvivorSizeBefore;
      } else if (tabStr(1).equals("minorGCSurvivorSizeAfter")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.minorGCSurvivorSizeAfter;
      } else if (tabStr(1).equals("minorGCHeapSizeBefore")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeBefore;
      } else if (tabStr(1).equals("minorGCHeapSizeAfter")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeAfter;
      } else if (tabStr(1).equals("fullGCHeapSizeBefore")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeBefore;
      } else if (tabStr(1).equals("fullGCHeapSizeAfter")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeAfter;
      } else if (tabStr(1).equals("fullGCduration")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.fullGCduration;
      } else if (tabStr(1).equals("throughput")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.throughput;

      }
      else if (tabStr(1).equals("memThroughput")) {

        retour = TraiterOpenJDK17GC1TS.structGC1.memThroughput;

      }
      retour

    }
  private def remplirStructure(tabString: Array[String]) {

    // System.out.println("debut remplir structure");
    // nettoyer la structure
    TraiterOpenJDK17GC1TS.structGC1 = new StructGC1TS
    if (tabString(0).contains("GC pause")) {

      // System.out.println("debut remplir structure Young Size Before");
      // Heap Size Before
      // var pat = "\\[\\s+\\d+(M|K)".r
      //  [Eden: 51M(51M)->0B(44M) Survivors: 0B->7168K Heap: 51M(256M)->9572K(256M)]
      var pat = """Heap:\s+\d+(M|K|B)""".r
      var match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get;
        var size = ext1.split("\\s+")(1)
        if (size.endsWith("M")) {

          TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeBefore = (1000 * Integer.parseInt(size.substring(0, size.indexOf("M")))).toDouble
        } else if (size.endsWith("K")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeBefore = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        } else if (size.endsWith("B")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeBefore = (Integer
            .parseInt(size.substring(0, size.indexOf("B")))).toDouble / 1000
        }
      } else {
        TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeBefore = Double.NaN
      }
      // System.out.println("debut remplir structure Young Size After");
      // // Heap Size After

      pat = """Heap:\s+\d+(M|K|B).+?->\d+(M|K|B)""".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var size = ext1.split(">")(1)
        if (size.endsWith("M")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeAfter = (1000 * Integer
            .parseInt(size.substring(0, size.indexOf("M")))).toDouble
        } else if (size.endsWith("K")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeAfter = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        } else if (size.endsWith("B")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeAfter = (Integer
            .parseInt(size.substring(0, size.indexOf("B")))).toDouble / 1000
        }
      } else {
        TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeAfter = Double.NaN
      }
      // System.out.println("debut remplir structure Young Duration");

      //      minorGCEdenSizeBefore=Double.NaN
      pat = """Eden:\s+\d+(M|K|B)""".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var size = ext1.split("\\s+")(1)
        if (size.endsWith("M")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCEdenSizeBefore = (1000 * Integer
            .parseInt(size.substring(0, size.indexOf("M")))).toDouble
        } else if (size.endsWith("K")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCEdenSizeBefore = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        } else if (size.endsWith("B")) {

          TraiterOpenJDK17GC1TS.structGC1.minorGCEdenSizeBefore = (Integer
            .parseInt(size.substring(0, size.indexOf("B")))).toDouble / 1000
        }
      } else {
        TraiterOpenJDK17GC1TS.structGC1.minorGCEdenSizeBefore = Double.NaN
      }

      //  var minorGCEdenSizeAfter=Double.NaN
      pat = """Eden:\s+\d+(M|K|B).+?->\d+(M|K|B)""".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var size = ext1.split(">")(1)
        if (size.endsWith("M")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCEdenSizeAfter = (1000 * Integer
            .parseInt(size.substring(0, size.indexOf("M")))).toDouble
        } else if (size.endsWith("K")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCEdenSizeAfter = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        } else if (size.endsWith("B")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCEdenSizeAfter = (Integer
            .parseInt(size.substring(0, size.indexOf("B")))).toDouble / 1000
        }
      } else {
        TraiterOpenJDK17GC1TS.structGC1.minorGCEdenSizeAfter = Double.NaN
      }

      //  var minorGCSurvivorSizeBefore=Double.NaN

      pat = """Survivors:\s+\d+(M|K|B)""".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var size = ext1.split("\\s+")(1)
        if (size.endsWith("M")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCSurvivorSizeBefore = (1000 * Integer
            .parseInt(size.substring(0, size.indexOf("M")))).toDouble
        } else if (size.endsWith("K")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCSurvivorSizeBefore = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        } else if (size.endsWith("B")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCSurvivorSizeBefore = (Integer
            .parseInt(size.substring(0, size.indexOf("B")))).toDouble / 1000
        }
      } else {
        TraiterOpenJDK17GC1TS.structGC1.minorGCSurvivorSizeBefore = Double.NaN
      }

      //  var minorGCSurvivorSizeAfter=Double.NaN
      pat = """Survivors:\s+\d+(M|K|B)->\d+(M|K|B)""".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var size = ext1.split(">")(1)
        if (size.endsWith("M")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCSurvivorSizeAfter = (1000 * Integer
            .parseInt(size.substring(0, size.indexOf("M")))).toDouble
        } else if (size.endsWith("K")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCSurvivorSizeAfter = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        } else if (size.endsWith("B")) {
          TraiterOpenJDK17GC1TS.structGC1.minorGCSurvivorSizeAfter = (Integer
            .parseInt(size.substring(0, size.indexOf("B")))).toDouble / 1000
        }
      } else {
        TraiterOpenJDK17GC1TS.structGC1.minorGCSurvivorSizeAfter = Double.NaN
      }

      // minorGCduration
      //[GC pause (young), 0,02696300 secs]

      pat = "GC\\s+pause\\s+\\(young\\),\\s+[^\\s]+ ".r

      var pat2 = "\\d+(\\.|,)\\d+".r
      match0 = pat.findFirstIn(tabString(0));
      if (None != match0) {
        var ext1 = match0.get
        var match2 = pat2.findFirstIn(ext1)
        if (None != match2) {
          var duration = match2.get.replaceAll(",", ".");

          try {
            TraiterOpenJDK17GC1TS.structGC1.minorGCduration = TraiterOpenJDK17GC1TS.df.parse(duration).doubleValue() * 1000;

          } catch {

            case e: ParseException => e.printStackTrace()
          }
        } else {
          TraiterOpenJDK17GC1TS.structGC1.minorGCduration = Double.NaN
        }
      } else {
        TraiterOpenJDK17GC1TS.structGC1.minorGCduration = Double.NaN
      }
      // System.out.println("debut remplir structure Young throughput");
      // throughput

      // Parrallel Time
      // [Parallel Time:  36,3 ms]
      pat = "Parallel\\s+Time:\\s+\\d+(\\.|,)?\\d*\\s+ms".r
      pat2 = "\\d+(\\.|,)?\\d*".r

      match0 = pat.findFirstIn(tabString(0))
      var parallelTime = 0.0;
      if (None != match0) {
        var ext2 = match0.get

        var match2 = pat2.findFirstIn(ext2)
        if (None != match2) {
          try {
            parallelTime = (TraiterOpenJDK17GC1TS.df.parse(match2.get
              .replaceAll(",", ".")).doubleValue()).toDouble
          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }

      }
      TraiterOpenJDK17GC1TS.structGC1.parallelTime = parallelTime

      // calcul du throughput avec le parallel time pour les mineurs GC
      if (TraiterOpenJDK17GC1TS.ecartDateMillis > 0) {
        val str1 = (100.toDouble - (100 * parallelTime.toDouble / (parallelTime.toDouble + TraiterOpenJDK17GC1TS.ecartDateMillis.toDouble))).toString
        TraiterOpenJDK17GC1TS.structGC1.throughput = str1.substring(0, scala.math.min(5, str1.length())).toDouble
      } else {

        TraiterOpenJDK17GC1TS.structGC1.throughput = Double.NaN
      }
      if (TraiterOpenJDK17GC1TS.structGC1.parallelTime != Double.NaN) {
        TraiterOpenJDK17GC1TS.oldDateInMillis += TraiterOpenJDK17GC1TS.structGC1.parallelTime.toLong
      }

      // System.out.println("Fin  remplir structure Young apres throughput");
      // System.out
      // .println("Fin  remplir structure Young structGC1.sizeBefore = "
      // + structGC1.sizeBefore);
      // System.out
      // .println("Fin  remplir structure Young structGC1.sizeAfter = "
      // + structGC1.sizeAfter);
      // System.out
      // .println("Fin  remplir structure Young structGC1.duration = "
      // + structGC1.duration);
      // System.out
      // .println("Fin  remplir structure Young structGC1.throughput = "
      // + structGC1.throughput);
      
      
       // remplissage Throughput Memoire
      // println("Avant Traitement throughput memor"+TraiterHotspot5And6.structHotSpot.sizeHeapBefore+ " " +TraiterHotspot5And6.structHotSpot.sizeHeapAfter)
      TraiterOpenJDK17GC1TS.structGC1.memThroughput = Double.NaN
      if (!(TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeBefore.isNaN()) && !(TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeAfter.isNaN())) {
        var sweeped = TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeBefore - TraiterOpenJDK17GC1TS.structGC1.minorGCHeapSizeAfter
        // println("Traitement throughput memor")
        TraiterOpenJDK17GC1TS.circleArray.put((TraiterOpenJDK17GC1TS.oldDateInMillis, sweeped))
        TraiterOpenJDK17GC1TS.structGC1.memThroughput = TraiterOpenJDK17GC1TS.circleArray.throughput
      }

    } else if (tabString(0).contains("[Full GC ")) {
      //var fullGCduration=Double.NaN
      //  
      //  var fullGCHeapSizeBefore=Double.NaN
      //  var fullGCHeapSizeAfter=Double.NaN
      // Size fullGCHeapSizeBefore
      //182,027: [Full GC 250M->69M(256M), 0,6050490 secs]
      var pat = "Full GC.+?\\d+(M|K|B)".r
      var match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get

        var pat3 = "\\d+(M|K|B)$".r
        var match3 = pat3.findFirstIn(ext1)
        if (None != match3) {

          var size = match3.get

          if (size.endsWith("M")) {
            TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeBefore = (1000 * Integer.parseInt(size
              .substring(0, size.indexOf("M")))).toDouble
          } else if (size.endsWith("K")) {
            TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeBefore = (Integer.parseInt(size.substring(0,
              size.indexOf("K")))).toDouble
          } else if (size.endsWith("B")) {
            TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeBefore = (Integer.parseInt(size.substring(0,
              size.indexOf("B")))).toDouble / 1000
          }
        } else {
          TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeBefore = Double.NaN
        }

      }

      //  
      // 
      //  var fullGCHeapSizeAfter=Double.NaN
      // Size After
      //182,027: [Full GC 250M->69M(256M), 0,6050490 secs]
      pat = "Full GC.+?\\d+(M|K|B)->\\d+(K|M|B)".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get

        var pat3 = "\\d+(M|K|B)$".r
        var match3 = pat3.findFirstIn(ext1)
        if (None != match3) {

          var size = match3.get

          if (size.endsWith("M")) {
            TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeAfter = (1000 * Integer.parseInt(size
              .substring(0, size.indexOf("M")))).toDouble
          } else if (size.endsWith("M")) {
            TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeAfter = (Integer.parseInt(size.substring(0,
              size.indexOf("K")))).toDouble
          } else if (size.endsWith("B")) {
            TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeAfter = (Integer.parseInt(size.substring(0,
              size.indexOf("B")))).toDouble / 1000
          }
        } else {
          TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeAfter = Double.NaN
        }

      }

      //var fullGCduration=Double.NaN
      //182,027: [Full GC 250M->69M(256M), 0,6050490 secs]
      var pat2 = "\\d+(\\.|,)\\d+".r
      pat = "Full GC[^,]+?,\\s+[^\\s]+ ".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var match2 = pat2.findFirstIn(ext1)
        if (None != match2) {
          var duration = match2.get.replaceAll(",", ".");

          try {
            TraiterOpenJDK17GC1TS.structGC1.fullGCduration = (TraiterOpenJDK17GC1TS.df.parse(duration).doubleValue() * 1000).toDouble

            TraiterOpenJDK17GC1TS.structGC1.parallelTime = TraiterOpenJDK17GC1TS.structGC1.fullGCduration;
          } catch {

            case e: ParseException => e.printStackTrace()
          }
        } else {
          TraiterOpenJDK17GC1TS.structGC1.fullGCduration = Double.NaN
          TraiterOpenJDK17GC1TS.structGC1.parallelTime = TraiterOpenJDK17GC1TS.structGC1.fullGCduration;
        }

      } else {
        TraiterOpenJDK17GC1TS.structGC1.fullGCduration = Double.NaN
        TraiterOpenJDK17GC1TS.structGC1.parallelTime = TraiterOpenJDK17GC1TS.structGC1.fullGCduration;
      }
      // throughput

      // calcul du throughput avec la full GC duration time pour les majeurs GC
      if (TraiterOpenJDK17GC1TS.ecartDateMillis > 0) {
        val str2 = ((TraiterOpenJDK17GC1TS.structGC1.fullGCduration.toDouble + TraiterOpenJDK17GC1TS.ecartDateMillis.toDouble)).toString
        TraiterOpenJDK17GC1TS.structGC1.throughput = 100.toDouble - (100 * TraiterOpenJDK17GC1TS.structGC1.fullGCduration.toDouble /
          (str2.substring(0, scala.math.min(5, str2.length())).toDouble))
      } else {

        TraiterOpenJDK17GC1TS.structGC1.throughput = Double.NaN
      }
      if (TraiterOpenJDK17GC1TS.structGC1.parallelTime != Double.NaN) {
        TraiterOpenJDK17GC1TS.oldDateInMillis += TraiterOpenJDK17GC1TS.structGC1.parallelTime.toLong
      }
        // remplissage Throughput Memoire
      // println("Avant Traitement throughput memor"+TraiterHotspot5And6.structHotSpot.sizeHeapBefore+ " " +TraiterHotspot5And6.structHotSpot.sizeHeapAfter)
      TraiterOpenJDK17GC1TS.structGC1.memThroughput = Double.NaN
      if (!(TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeBefore.isNaN()) && !(TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeAfter.isNaN())) {
        var sweeped = TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeBefore - TraiterOpenJDK17GC1TS.structGC1.fullGCHeapSizeAfter
        // println("Traitement throughput memor")
        TraiterOpenJDK17GC1TS.circleArray.put((TraiterOpenJDK17GC1TS.oldDateInMillis, sweeped))
        TraiterOpenJDK17GC1TS.structGC1.memThroughput = TraiterOpenJDK17GC1TS.circleArray.throughput
      }

    } else {

      // traitements des autres types d evenements GC a voir
    }

  }

}

class StructGC1TS {

  var comtpeur = 0;
  var whattype = ""

  var minorGCEdenSizeBefore = Double.NaN
  var minorGCEdenSizeAfter = Double.NaN
  var minorGCduration = Double.NaN
  var minorGCSurvivorSizeBefore = Double.NaN
  var minorGCSurvivorSizeAfter = Double.NaN
  var minorGCHeapSizeBefore = Double.NaN
  var minorGCHeapSizeAfter = Double.NaN
  var parallelTime = Double.NaN
  var memThroughput = Double.NaN
  var fullGCduration = Double.NaN

  var fullGCHeapSizeBefore = Double.NaN
  var fullGCHeapSizeAfter = Double.NaN

  var throughput = Double.NaN

}

object TraiterOpenJDK17GC1TS {
  val patDate = """^(\d+(,|\.)\d+)""".r
  var circleArray: CircleArray = null
  var oldDateInMillis = 0L
  var ecartDateMillis = 0L
  var lastEnrg = ""
  var structGC1: StructGC1TS = null;
  var dfn = new DecimalFormatSymbols(
    Locale.ENGLISH);
  var df = new DecimalFormat("0.0", dfn);
  var dfToShow = new DecimalFormat("0.##", dfn);

}