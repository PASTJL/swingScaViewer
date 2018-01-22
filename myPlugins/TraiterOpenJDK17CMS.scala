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

class TraiterOpenJDK17CMS {
  def metInit(tab: Array[String] = null) {
    // To reinitialise static variable if necessary
    TraiterOpenJDK17CMS.structGCCMS = null
    TraiterOpenJDK17CMS.oldDateInMillis = 0L
    TraiterOpenJDK17CMS.circleArray = new CircleArray(10)
     TraiterOpenJDK17CMS.enrCurrent = null
    
  }
  def retour(tabStr: Array[String]): Double =
    {

      var retour = Double.NaN
      // System.out.println(" enr =|" + tabString[0] + "|");
      // String[0] => Enreg a traiter

      // String[1] parametre a retourner
      // Creation d'une structure d'acceuil pour cet enregistrement
      //
      
       if (TraiterOpenJDK17CMS.enrCurrent == null) {
        TraiterOpenJDK17CMS.enrCurrent = tabStr(0)

        TraiterOpenJDK17CMS.oldDateInMillis = 0

        remplirStructure(tabStr)
         
      } else {
        if (TraiterOpenJDK17CMS.enrCurrent != tabStr(0)) {

          remplirStructure(tabStr)
          TraiterOpenJDK17CMS.enrCurrent=tabStr(0)
        }
      }
      

      retour = Double.NaN
      

        if (tabStr(1).equals("sizeBeforeYoung")) {
          TraiterOpenJDK17CMS.structGCCMS.comtpeur += 1
          retour = TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung
        } else if (tabStr(1).equals("sizeAfterYoung")) {
          TraiterOpenJDK17CMS.structGCCMS.comtpeur += 1
          retour = TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung
        } else if (tabStr(1).equals("sizeBeforeTenured")) {
          TraiterOpenJDK17CMS.structGCCMS.comtpeur += 1
          retour = TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured
        } else if (tabStr(1).equals("sizeAfterTenured")) {
          TraiterOpenJDK17CMS.structGCCMS.comtpeur += 1
          retour = TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured
        } else if (tabStr(1).equals("duration")) {
          TraiterOpenJDK17CMS.structGCCMS.comtpeur += 1
          retour = TraiterOpenJDK17CMS.structGCCMS.duration
        } else if (tabStr(1).equals("threadsStopped")) {
          TraiterOpenJDK17CMS.structGCCMS.comtpeur += 1
          retour = TraiterOpenJDK17CMS.structGCCMS.threadsStopped

        } else if (tabStr(1).equals("moyStopped")) {
          TraiterOpenJDK17CMS.structGCCMS.comtpeur += 1
          retour = TraiterOpenJDK17CMS.structGCCMS.moyStopped

        } else if (tabStr(1).equals("maxStopped")) {
          TraiterOpenJDK17CMS.structGCCMS.comtpeur += 1
          retour = TraiterOpenJDK17CMS.structGCCMS.maxStopped;

        }
        else if(tabStr(1).equals("sizeHeapBefore")) {
          retour = TraiterOpenJDK17CMS.structGCCMS.sizeHeapBefore;
        }
        else if(tabStr(1).equals("sizeHeapAfter")) {
          retour = TraiterOpenJDK17CMS.structGCCMS.sizeHeapAfter;
        }
        else if(tabStr(1).equals("memThroughput")) {
          retour = TraiterOpenJDK17CMS.structGCCMS.memThroughput;
        }
      
       else if(tabStr(1).equals("throughput")) {
          retour = TraiterOpenJDK17CMS.structGCCMS.throughput;
        }
        
        
      
      

      return retour;

    }

  private def remplirStructure(tabString: Array[String]) {
    val patTS = """\d+(\.|,)\d+: \[GC""".r

    val ext1 = patTS.findFirstIn(tabString(0)).get
    val ext2 = """\d+(\.|,)\d+""".r.findFirstIn(ext1).get
    TraiterOpenJDK17CMS.oldDateInMillis = ext2.replace(""",""", """\.""").toLong

    if (tabString(0).contains("[ParNew: ")
      || tabString(0).contains("[DefNew: ")) {
      TraiterOpenJDK17CMS.structGCCMS.whattype = "GCMinor";

      // Young Size Before
      var pat = "(Par|Def)New[^\\-]+".r
      var match0 = pat.findFirstIn(tabString(0))

      if (None != match0) {
        var ext1 = match0.get

        var size = ext1.split(":")(1).trim();
        if (size.endsWith("M")) {
          TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung = (1000 * Integer.parseInt(size.substring(0,
            size.indexOf("M")))).toDouble
        } else {
          TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        }
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung = Double.NaN
      }

      // // Young Size Size After
      pat = "(Par|Def)New[^\\-]+?->\\d+(M|K)".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var size = ext1.split(">")(1)
        if (size.endsWith("M")) {
          TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung = (1000 * Integer.parseInt(size.substring(0,
            size.indexOf("M")))).toDouble
        } else {
          TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        }
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung = Double.NaN
      }

      // Tenured Size Before
      pat = "(Par|Def)New[^\\]]+?\\]\\s+\\d[^\\-]+".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var pat3 = "\\d+(K|M)$".r
        var match3 = pat3.findFirstIn(ext1);
        if (None != match3) {

          var size = match3.get
          if (size.endsWith("M")) {
            TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured = (1000 * Integer.parseInt(size
              .substring(0, size.indexOf("M")))).toDouble
          } else {
            TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured = (Integer.parseInt(size.substring(0,
              size.indexOf("K")))).toDouble
          }
        }
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured = Double.NaN
      }

      // // Tenured Size Size After
      pat = "(Par|Def)New[^\\]]+?\\]\\s+\\d[^\\-]+-[^\\(]+".r
      match0 = pat.findFirstIn(tabString(0));
      if (None != match0) {
        var ext1 = match0.get

        var pat3 = "\\d+(K|M)$".r
        var match3 = pat3.findFirstIn(ext1)
        if (None != match3) {
          var size = match3.get

          if (size.endsWith("M")) {
            TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured = (1000 * Integer.parseInt(size
              .substring(0, size.indexOf("M")))).toDouble
          } else {
            TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured = (Integer.parseInt(size.substring(0,
              size.indexOf("K")))).toDouble
          }
        }
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured = Double.NaN
      }

      // remplissage   sizeHeapBefore
      if (!TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung.isNaN &&
        !TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured.isNaN) {

        TraiterOpenJDK17CMS.structGCCMS.sizeHeapBefore = TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung +
          TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeHeapBefore = Double.NaN
      }

      // remplissage   sizeHeapAfter
      if (!TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung.isNaN &&
        !TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured.isNaN) {

        TraiterOpenJDK17CMS.structGCCMS.sizeHeapAfter = TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung +
          TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeHeapAfter = Double.NaN
      }

      // remplissage Throughput Memoire
      // println("Avant Traitement throughput memor"+TraiterHotspot5And6.structHotSpot.sizeHeapBefore+ " " +TraiterHotspot5And6.structHotSpot.sizeHeapAfter)
      TraiterOpenJDK17CMS.structGCCMS.memThroughput = Double.NaN
      if (!(TraiterOpenJDK17CMS.structGCCMS.sizeHeapBefore.isNaN()) && !(TraiterOpenJDK17CMS.structGCCMS.sizeHeapAfter.isNaN())) {
        var sweeped = TraiterOpenJDK17CMS.structGCCMS.sizeHeapBefore - TraiterOpenJDK17CMS.structGCCMS.sizeHeapAfter
        // println("Traitement throughput memor")
        TraiterOpenJDK17CMS.circleArray.put((TraiterOpenJDK17CMS.oldDateInMillis, sweeped))
        TraiterOpenJDK17CMS.structGCCMS.memThroughput = TraiterOpenJDK17CMS.circleArray.throughput
      }

      // Duration
      pat = "GC.+(Par|Def)New[^\\]]+\\][^\\]]+".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var pat3 = "\\d+(\\.|,)\\d+\\s+secs$".r
        var match3 = pat3.findFirstIn(ext1)
        if (None != match3) {
          var duration = match3.get.split("\\s+")(0)
            .replaceAll(",", ".");
          // System.out.println("duration =" + duration);
          var db = Double.NaN
          try {

            db = (TraiterOpenJDK17CMS.df.parse(duration).doubleValue()).toDouble * 1000;

            TraiterOpenJDK17CMS.structGCCMS.duration = db

          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }
      } else {
        TraiterOpenJDK17CMS.structGCCMS.duration = Double.NaN
      }

      // ThreadsStopped
      pat = "Total time.+?seconds".r
      var pat2 = "\\d+(\\.|,)\\d+".r
      var copy = new String(tabString(0))
      match0 = pat.findFirstIn(tabString(0))
      var totalStopped = 0.0;
      var count = 0;
      var max = 0.0;
      while (None != match0) {
        var ext2 = match0.get
        copy = copy.substring(ext2.length());
        var match2 = pat2.findFirstIn(ext2)
        if (None != match2) {
          count += 1
          try {
            var res = (TraiterOpenJDK17CMS.df.parse(match2.get
              .replaceAll(",", ".")).doubleValue()).toDouble
            if (res > max) {
              max = res;
            }
            totalStopped += res;
          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }
        match0 = pat.findFirstIn(copy)
      }
      TraiterOpenJDK17CMS.structGCCMS.threadsStopped = (totalStopped * 1000).toDouble
      if (count > 0) {
        TraiterOpenJDK17CMS.structGCCMS.moyStopped = (totalStopped * 1000 / count).toDouble
      } else {
        TraiterOpenJDK17CMS.structGCCMS.moyStopped = Double.NaN
      }
      TraiterOpenJDK17CMS.structGCCMS.maxStopped = (max * 1000).toDouble
      //

      // throughput

      // Ajouter tous les total application
      pat = "Application time.+?seconds".r

      copy = new String(tabString(0))
      match0 = pat.findFirstIn(tabString(0))
      var totalApplication = 0.0;
      while (None != match0) {
        var ext2 = match0.get
        copy = copy.substring(ext2.length())
        var match2 = pat2.findFirstIn(ext2)
        if (None != match2) {
          try {
            totalApplication += (TraiterOpenJDK17CMS.df.parse(
              match2.get.replaceAll(",", "."))
              .doubleValue()).toDouble
          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }
        match0 = pat.findFirstIn(copy)
      }

      if ((totalApplication + totalStopped) == 0) {
        TraiterOpenJDK17CMS.structGCCMS.throughput = Double.NaN
      } else {
        TraiterOpenJDK17CMS.structGCCMS.throughput = (100
          * (totalApplication / (totalApplication + totalStopped))).toDouble

      }

    } else if (tabString(0).contains("[Full GC")) {

      TraiterOpenJDK17CMS.structGCCMS.whattype = "GCFull";

      // Young Size Before
      var pat = "\\[CMS:\\s+\\d+(K|M)".r
      var match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var size = ext1.split(":")(1).trim();
        if (size.endsWith("M")) {
          TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung = (1000 * Integer.parseInt(size.substring(0,
            size.indexOf("M")))).toDouble
        } else {
          TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        }
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung = Double.NaN
      }

      // // Young Size Size After
      pat = "\\[CMS:\\s+\\d+(K|M)->\\d+(M|K)".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var size = ext1.split(">")(1)
        if (size.endsWith("M")) {
          TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung = (1000 * Integer.parseInt(size.substring(0,
            size.indexOf("M")))).toDouble
        } else {
          TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        }
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung = Double.NaN
      }

      // Tenured Size Before
      pat = "\\[CMS:[^\\]]+\\][^\\-]+".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        val pat3 = "\\d+(K|M)$".r
        val match3 = pat3.findFirstIn(ext1)
        if (None != match3) {

          var size = match3.get
          if (size.endsWith("M")) {
            TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured = (1000 * Integer.parseInt(size
              .substring(0, size.indexOf("M")))).toDouble
          } else {
            TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured = (Integer.parseInt(size.substring(0,
              size.indexOf("K")))).toDouble
          }
        }
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured = Double.NaN
      }

      // // Tenured Size Size After
      pat = "\\[CMS:[^\\]]+\\][^\\-]+->[^\\(]+".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get

        val pat3 = "\\d+(K|M)$".r
        var match3 = pat3.findFirstIn(ext1)
        if (None != match3) {
          var size = match3.get

          if (size.endsWith("M")) {
            TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured = (1000 * Integer.parseInt(size
              .substring(0, size.indexOf("M")))).toDouble
          } else {
            TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured = (Integer.parseInt(size.substring(0,
              size.indexOf("K")))).toDouble
          }
        }
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured = Double.NaN
      }

      // remplissage   sizeHeapBefore
      if (!TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung.isNaN &&
        !TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured.isNaN) {

        TraiterOpenJDK17CMS.structGCCMS.sizeHeapBefore = TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung +
          TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeHeapBefore = Double.NaN
      }

      // remplissage   sizeHeapAfter
      if (!TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung.isNaN &&
        !TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured.isNaN) {

        TraiterOpenJDK17CMS.structGCCMS.sizeHeapAfter = TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung +
          TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured
      } else {
        TraiterOpenJDK17CMS.structGCCMS.sizeHeapAfter = Double.NaN
      }

      // remplissage Throughput Memoire
      // println("Avant Traitement throughput memor"+TraiterHotspot5And6.structHotSpot.sizeHeapBefore+ " " +TraiterHotspot5And6.structHotSpot.sizeHeapAfter)
      TraiterOpenJDK17CMS.structGCCMS.memThroughput = Double.NaN
      if (!(TraiterOpenJDK17CMS.structGCCMS.sizeHeapBefore.isNaN()) && !(TraiterOpenJDK17CMS.structGCCMS.sizeHeapAfter.isNaN())) {
        var sweeped = TraiterOpenJDK17CMS.structGCCMS.sizeHeapBefore - TraiterOpenJDK17CMS.structGCCMS.sizeHeapAfter
        // println("Traitement throughput memor")
        TraiterOpenJDK17CMS.circleArray.put((TraiterOpenJDK17CMS.oldDateInMillis, sweeped))
        TraiterOpenJDK17CMS.structGCCMS.memThroughput = TraiterOpenJDK17CMS.circleArray.throughput
      }

      // Duration
      pat = "\\[Full GC[^\\]]+\\][^\\]]+\\][^\\]]+".r
      match0 = pat.findFirstIn(tabString(0));
      if (None != match0) {
        var ext1 = match0.get
        val pat3 = "\\d+(\\.|,)\\d+\\s+secs$".r
        var match3 = pat3.findFirstIn(ext1)
        if (None != match3) {
          var duration = match3.get.split("\\s+")(0).replaceAll(",", ".")

          try {
            TraiterOpenJDK17CMS.structGCCMS.duration = (TraiterOpenJDK17CMS.df.parse(duration).doubleValue() * 1000).toDouble

          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }
      } else {
        TraiterOpenJDK17CMS.structGCCMS.duration = Double.NaN

      }
      // ThreadsStopped
      pat = "Total time.+?seconds".r
      var pat2 = "\\d+(\\.|,)\\d+".r
      var copy = new String(tabString(0))
      match0 = pat.findFirstIn(tabString(0))
      var totalStopped = 0.0;
      var count = 0;
      var max = 0.0;
      while (None != match0) {
        var ext2 = match0.get
        copy = copy.substring(ext2.length());
        var match2 = pat2.findFirstIn(ext2)
        if (None != match2) {
          count += 1
          try {
            var res = (TraiterOpenJDK17CMS.df.parse(match2.get
              .replaceAll(",", ".")).doubleValue()).toDouble
            totalStopped += res
            if (res > max) {
              max = res;
            }
          } catch {

            case e: ParseException => e.printStackTrace()
          }

        }
        match0 = pat.findFirstIn(copy)
      }
      TraiterOpenJDK17CMS.structGCCMS.threadsStopped = (totalStopped * 1000).toDouble
      if (count > 0) {
        TraiterOpenJDK17CMS.structGCCMS.moyStopped = (totalStopped * 1000 / count).toDouble
      } else {
        TraiterOpenJDK17CMS.structGCCMS.moyStopped = Double.NaN
      }
      TraiterOpenJDK17CMS.structGCCMS.maxStopped = (max * 1000).toDouble
      // throughput

      // Ajouter tous les total application stopped
      pat = "Application time.+?seconds".r

      copy = new String(tabString(0))
      match0 = pat.findFirstIn(tabString(0));
      var totalApplication = 0.0;
      while (None != match0) {
        var ext2 = match0.get
        copy = copy.substring(ext2.length());
        var match2 = pat2.findFirstIn(ext2)
        if (None != match2) {
          try {
            totalApplication += (TraiterOpenJDK17CMS.df.parse(
              match2.get.replaceAll(",", "."))
              .doubleValue()).toDouble
          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }
        match0 = pat.findFirstIn(copy)
      }

      TraiterOpenJDK17CMS.structGCCMS.throughput = 100 * (totalApplication / (totalApplication + totalStopped));

    } else {
      TraiterOpenJDK17CMS.structGCCMS.duration = Double.NaN
      TraiterOpenJDK17CMS.structGCCMS.sizeAfterYoung = Double.NaN
      TraiterOpenJDK17CMS.structGCCMS.sizeBeforeYoung = Double.NaN
      TraiterOpenJDK17CMS.structGCCMS.sizeAfterTenured = Double.NaN
      TraiterOpenJDK17CMS.structGCCMS.sizeBeforeTenured = Double.NaN
      TraiterOpenJDK17CMS.structGCCMS.threadsStopped = Double.NaN
       TraiterOpenJDK17CMS.structGCCMS.sizeHeapBefore = Double.NaN
       TraiterOpenJDK17CMS.structGCCMS.sizeHeapAfter = Double.NaN
        TraiterOpenJDK17CMS.structGCCMS.memThroughput = Double.NaN
      // Calcul du throughput autre operation GC
      // throughput
      // Ajouter tous les total application stopped
      var pat = "Total time.+?seconds".r
      var pat2 = "\\d+(\\.|,)\\d+".r
      var copy = new String(tabString(0))
      var match0 = pat.findFirstIn(tabString(0));
      var totalStopped = 0.0;
      var count = 0;
      var max = 0.0;
      while (None != match0) {
        var ext2 = match0.get
        copy = copy.substring(ext2.length());
        var match2 = pat2.findFirstIn(ext2)
        if (None != match2) {
          count += 1
          try {
            var res = (TraiterOpenJDK17CMS.df.parse(
              match2.get.replaceAll(",", "."))
              .doubleValue()).toDouble
            totalStopped += res;
            if (res > max) {
              max = res;
            }
          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }
        match0 = pat.findFirstIn(copy)
      }

      TraiterOpenJDK17CMS.structGCCMS.threadsStopped = (totalStopped * 1000).toDouble
      if (count > 0) {
        TraiterOpenJDK17CMS.structGCCMS.moyStopped = (totalStopped * 1000 / count).toDouble
      } else {
        TraiterOpenJDK17CMS.structGCCMS.moyStopped = Double.NaN
      }
      TraiterOpenJDK17CMS.structGCCMS.maxStopped = (max * 1000).toDouble
      // Ajouter tous les total application stopped
      pat = "Application time.+?seconds".r

      copy = new String(tabString(0))
      match0 = pat.findFirstIn(tabString(0));
      var totalApplication = 0.0;
      while (None != match0) {
        var ext2 = match0.get
        copy = copy.substring(ext2.length());
        var match2 = pat2.findFirstIn(ext2)
        if (None != match2) {
          try {
            totalApplication += (TraiterOpenJDK17CMS.df.parse(
              match2.get.replaceAll(",", "."))
              .doubleValue()).toDouble
          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }
        match0 = pat.findFirstIn(copy)
      }
      if ((totalApplication + totalStopped) == 0) {
        TraiterOpenJDK17CMS.structGCCMS.throughput = Double.NaN
      } else {
        TraiterOpenJDK17CMS.structGCCMS.throughput = (100
          * (totalApplication / (totalApplication + totalStopped))).toDouble

      }

    }

  }
}

class StructGCCMS(nbMetrics: Int) {
  var maxCompteur = nbMetrics;
  var comtpeur = 0;
  var whattype = "";
  var sizeBeforeYoung = Double.NaN
  var sizeAfterYoung = Double.NaN
  var sizeBeforeTenured = Double.NaN
  var sizeAfterTenured = Double.NaN
  var threadsStopped = Double.NaN
  var moyStopped = Double.NaN
  var maxStopped = Double.NaN
  var sizeHeapBefore = Double.NaN
  var sizeHeapAfter = Double.NaN
  var duration = Double.NaN
  var memThroughput = Double.NaN
  var throughput = Double.NaN

}

object TraiterOpenJDK17CMS {
  var oldDateInMillis = 0L
    var enrCurrent: String = null
  var circleArray: CircleArray = null
  var structGCCMS: StructGCCMS = null;
  var dfn = new DecimalFormatSymbols(
    Locale.ENGLISH);
  var df = new DecimalFormat("0.0", dfn);
  var dfToShow = new DecimalFormat("0.##", dfn);
}