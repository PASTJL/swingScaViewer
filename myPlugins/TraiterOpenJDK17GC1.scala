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

class TraiterOpenJDK17GC1 {
  def metInit(tab: Array[String] = null) {
    // To reinitialise static variable if necessary
    TraiterOpenJDK17GC1.oldDateInMillis = 0L
    TraiterOpenJDK17GC1.structGC1 = null
     TraiterOpenJDK17GC1.circleArray=new CircleArray(10)
  }
  def retour(tabStr: Array[String]): Double =
    {

      var retour = Double.NaN
      // System.out.println(" enr =|" + tabString[0] + "|");
      // String[0] => Enreg � traiter

      // 2 type de temps � recup�rer sur un Young GC ou sur un full GC
      // Le premier champ est l'enregistrement, le deuxieme champ est le type
      // (
      // Young GC/FullGC),le troisi�mee champ la valeur a renvoyer, le
      // quatrieme champ est le nombre de parametres
      // geres pour le parsing defaut 4.
      // Creation d'une structure d'acceuil pour cet enregistrement
      //

      if (tabStr(0) != TraiterOpenJDK17GC1.lastEnrg) {
        //println("tabStr(0) dans plugin= avant de remplir la structure :\n" +tabStr(0) )

        val tabDateStr = TraiterOpenJDK17GC1.patDate1.findFirstIn(tabStr(0)).get.split("(,|\\.)")
        val secs = """\d+""".r.findFirstIn(tabDateStr(0)).get
        val millis = """\d+""".r.findFirstIn(tabDateStr(1)).get
        val date1 = secs.toLong * 1000 + millis.toLong

        TraiterOpenJDK17GC1.ecartDateMillis = date1 - TraiterOpenJDK17GC1.oldDateInMillis
        TraiterOpenJDK17GC1.oldDateInMillis = date1
        remplirStructure(tabStr)
        // calcul de l'ecart de temps entre le nouveau et l ancien enregistrement

        TraiterOpenJDK17GC1.lastEnrg = tabStr(0)

      }

      retour = Double.NaN
      if (tabStr(1).contains("YoungPause")
        && tabStr(0).contains("GC pause")) {

        if (tabStr(2).equals("sizeBefore")) {

          retour = TraiterOpenJDK17GC1.structGC1.sizeBefore;
        } else if (tabStr(2).equals("sizeAfter")) {

          retour = TraiterOpenJDK17GC1.structGC1.sizeAfter;
        } else if (tabStr(2).equals("duration")) {

          retour = TraiterOpenJDK17GC1.structGC1.duration;
        } else if (tabStr(2).equals("threadsStopped")) {

          retour = TraiterOpenJDK17GC1.structGC1.threadsStopped;

        } else if (tabStr(2).equals("parallelTime")) {

          retour = TraiterOpenJDK17GC1.structGC1.parallelTime;

        } else if (tabStr(2).equals("moyStopped")) {

          retour = TraiterOpenJDK17GC1.structGC1.moyStopped;

        } else if (tabStr(2).equals("maxStopped")) {

          retour = TraiterOpenJDK17GC1.structGC1.maxStopped;

        }

      } else if (tabStr(1).contains("FullGC")
        && tabStr(0).contains("Full GC")) {
        if (tabStr(2).equals("sizeBefore")) {

          retour = TraiterOpenJDK17GC1.structGC1.sizeBefore;
        } else if (tabStr(2).equals("sizeAfter")) {

          retour = TraiterOpenJDK17GC1.structGC1.sizeAfter;
        } else if (tabStr(2).equals("duration")) {

          retour = TraiterOpenJDK17GC1.structGC1.duration;
        } else if (tabStr(2).equals("threadsStopped")) {

          retour = TraiterOpenJDK17GC1.structGC1.threadsStopped;

        } else if (tabStr(2).equals("parallelTime")) {

          retour = TraiterOpenJDK17GC1.structGC1.parallelTime;

        } else if (tabStr(2).equals("moyStopped")) {

          retour = TraiterOpenJDK17GC1.structGC1.moyStopped;

        } else if (tabStr(2).equals("maxStopped")) {

          retour = TraiterOpenJDK17GC1.structGC1.maxStopped;

        }

      } else if (tabStr(1).contains("AllGen")) {

        if (tabStr(2).equals("throughput")) {

          retour = TraiterOpenJDK17GC1.structGC1.throughput;
        } else if (tabStr(2).equals("threadsStopped")) {

          retour = TraiterOpenJDK17GC1.structGC1.threadsStopped;

        } else if (tabStr(2).equals("moyStopped")) {

          retour = TraiterOpenJDK17GC1.structGC1.moyStopped;

        } else if (tabStr(2).equals("maxStopped")) {

          retour = TraiterOpenJDK17GC1.structGC1.maxStopped;

        }

      } else {
        if(tabStr(2).equals("memThroughput"))
        {
           retour = TraiterOpenJDK17GC1.structGC1.memThroughput;
        }
      }

      retour

    }
  private def remplirStructure(tabString: Array[String]) {
    TraiterOpenJDK17GC1.structGC1 = new StructGC1
    // System.out.println("debut remplir structure");
    if (tabString(0).contains("GC pause")) {
      TraiterOpenJDK17GC1.structGC1.whattype = "YoungPause";
      // System.out.println("debut remplir structure Young Size Before");
      // Size Before
      var pat = "\\[\\s+\\d+(M|K)".r
      var match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get;
        var size = ext1.split("\\s+")(1)
        if (size.endsWith("M")) {
          TraiterOpenJDK17GC1.structGC1.sizeBefore = (1000 * Integer.parseInt(size.substring(0, size.indexOf("M")))).toDouble
        } else {
          TraiterOpenJDK17GC1.structGC1.sizeBefore = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        }
      } else {
        TraiterOpenJDK17GC1.structGC1.sizeBefore = Double.NaN
      }
      // System.out.println("debut remplir structure Young Size After");
      // // Size After
      pat = "\\[\\s+\\d+(M|K)->\\d+(M|K)".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var size = ext1.split(">")(1)
        if (size.endsWith("M")) {
          TraiterOpenJDK17GC1.structGC1.sizeAfter = (1000 * Integer
            .parseInt(size.substring(0, size.indexOf("M")))).toDouble
        } else {
          TraiterOpenJDK17GC1.structGC1.sizeAfter = (Integer
            .parseInt(size.substring(0, size.indexOf("K")))).toDouble
        }
      } else {
        TraiterOpenJDK17GC1.structGC1.sizeAfter = Double.NaN
      }
      // System.out.println("debut remplir structure Young Duration");
      // Duration
      pat = "GC\\s+pause\\s+\\(young\\),\\s+[^s]+ ".r
      var pat2 = "\\d+(\\.|,)\\d+".r
      match0 = pat.findFirstIn(tabString(0));
      if (None != match0) {
        var ext1 = match0.get
        var match2 = pat2.findFirstIn(ext1)
        if (None != match2) {
          var duration = match2.get.replaceAll(",", ".");

          try {
            TraiterOpenJDK17GC1.structGC1.duration = TraiterOpenJDK17GC1.df.parse(duration).doubleValue() * 1000;

          } catch {

            case e: ParseException => e.printStackTrace()
          }
        } else {
          TraiterOpenJDK17GC1.structGC1.duration = Double.NaN
        }
      } else {
        TraiterOpenJDK17GC1.structGC1.duration = Double.NaN
      }
      // System.out.println("debut remplir structure Young throughput");
      // throughput
      // Ajouter tous les total application stopped
      pat = "Total time.+?seconds".r

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
            var res = (TraiterOpenJDK17GC1.df.parse(
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
      TraiterOpenJDK17GC1.structGC1.threadsStopped = (totalStopped * 1000).toDouble
      if (count > 0) {
        TraiterOpenJDK17GC1.structGC1.moyStopped = (totalStopped * 1000 / count).toDouble
      } else {
        TraiterOpenJDK17GC1.structGC1.moyStopped = Double.NaN
      }
      TraiterOpenJDK17GC1.structGC1.maxStopped = (max * 1000).toDouble
      // Parrallel Time
      pat = "Parallel\\s+Time:\\s+\\d+\\.?\\d*\\s+ms".r
      pat2 = "\\d+(\\.|,)?\\d*".r

      match0 = pat.findFirstIn(tabString(0))
      var parallelTime = 0.0;
      if (None != match0) {
        var ext2 = match0.get

        var match2 = pat2.findFirstIn(ext2)
        if (None != match2) {
          try {
            parallelTime = (TraiterOpenJDK17GC1.df.parse(match2.get
              .replaceAll(",", ".")).doubleValue()).toDouble
          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }

      }
      TraiterOpenJDK17GC1.structGC1.parallelTime = parallelTime

      // Ajouter tous les total application
      pat = "Application time.+?seconds".r

      copy = new String(tabString(0))
      match0 = pat.findFirstIn(tabString(0))
      var totalApplication = 0.0;
      while (None != match0) {
        var ext2 = match0.get
        copy = copy.substring(ext2.length());
        var match2 = pat2.findFirstIn(ext2)
        if (None != match2) {
          try {
            totalApplication += (TraiterOpenJDK17GC1.df.parse(
              match2.get.replaceAll(",", "."))
              .doubleValue()).toDouble
          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }
        match0 = pat.findFirstIn(copy)
      }
      // Calcul du throughput avec parallel time

      //      if ((totalApplication + totalStopped) == 0) {
      //        TraiterOpenJDK17GC1.structGC1.throughput = Double.NaN
      //      } else {
      //        TraiterOpenJDK17GC1.structGC1.throughput = (100 * (totalApplication / (totalApplication + totalStopped))).toDouble
      //
      //      }

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

    } else if (tabString(0).contains("[Full GC ")) {

      // Size Before
      var pat = "Full GC.+?\\d+(M|K)".r
      var match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get

        var pat3 = "\\d+(M|K)$".r
        var match3 = pat3.findFirstIn(ext1)
        if (None != match3) {

          var size = match3.get

          if (size.endsWith("M")) {
            TraiterOpenJDK17GC1.structGC1.sizeBefore = (1000 * Integer.parseInt(size
              .substring(0, size.indexOf("M")))).toDouble
          } else {
            TraiterOpenJDK17GC1.structGC1.sizeBefore = (Integer.parseInt(size.substring(0,
              size.indexOf("K")))).toDouble
          }
        } else {
          TraiterOpenJDK17GC1.structGC1.sizeBefore = Double.NaN
        }

      }

      // Size After
      pat = "Full GC.+?\\d+(M|K)->\\d+(K|M)".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get

        var pat3 = "\\d+(M|K)$".r
        var match3 = pat3.findFirstIn(ext1)
        if (None != match3) {

          var size = match3.get

          if (size.endsWith("M")) {
            TraiterOpenJDK17GC1.structGC1.sizeAfter = (1000 * Integer.parseInt(size
              .substring(0, size.indexOf("M")))).toDouble
          } else {
            TraiterOpenJDK17GC1.structGC1.sizeAfter = (Integer.parseInt(size.substring(0,
              size.indexOf("K")))).toDouble
          }
        } else {
          TraiterOpenJDK17GC1.structGC1.sizeBefore = Double.NaN
        }

      }

      // Duration
      var pat2 = "\\d+(\\.|,)\\d+".r
      pat = "Full GC[^,]+?,\\s+[^s]+ ".r
      match0 = pat.findFirstIn(tabString(0))
      if (None != match0) {
        var ext1 = match0.get
        var match2 = pat2.findFirstIn(ext1)
        if (None != match2) {
          var duration = match2.get.replaceAll(",", ".");

          try {
            TraiterOpenJDK17GC1.structGC1.duration = (TraiterOpenJDK17GC1.df.parse(duration).doubleValue() * 1000).toDouble

            TraiterOpenJDK17GC1.structGC1.parallelTime = TraiterOpenJDK17GC1.structGC1.duration;
          } catch {

            case e: ParseException => e.printStackTrace()
          }
        } else {
          TraiterOpenJDK17GC1.structGC1.duration = Double.NaN
          TraiterOpenJDK17GC1.structGC1.parallelTime = TraiterOpenJDK17GC1.structGC1.duration;
        }

      } else {
        TraiterOpenJDK17GC1.structGC1.duration = Double.NaN
        TraiterOpenJDK17GC1.structGC1.parallelTime = TraiterOpenJDK17GC1.structGC1.duration;
      }
      // throughput
      // Ajouter tous les total application stopped
      pat = "Total time.+?seconds".r

      var copy = new String(tabString(0))
      match0 = pat.findFirstIn(tabString(0))
      var totalStopped = 0.0;
      var count = 0;
      var max = 0.0;
      while (None != match0) {
        var ext2 = match0.get
        copy = copy.substring(ext2.length());
        var match2 = pat2.findFirstIn(ext2);
        if (None != match2) {
          count += 1
          try {
            var res = (TraiterOpenJDK17GC1.df.parse(
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
      TraiterOpenJDK17GC1.structGC1.threadsStopped = (totalStopped * 1000).toDouble
      if (count > 0) {
        TraiterOpenJDK17GC1.structGC1.moyStopped = (totalStopped * 1000 / count).toDouble
      } else {
        TraiterOpenJDK17GC1.structGC1.moyStopped = Double.NaN
      }
      TraiterOpenJDK17GC1.structGC1.maxStopped = (max * 1000).toDouble
      // Ajouter tous les total application
      pat = "Application time.+?seconds".r

      copy = new String(tabString(0))
      match0 = pat.findFirstIn(tabString(0))
      var totalApplication = 0.0;
      while (None != match0) {
        var ext2 = match0.get
        copy = copy.substring(ext2.length());
        var match2 = pat2.findFirstIn(ext2)
        if (None != match2) {
          try {
            totalApplication += (TraiterOpenJDK17GC1.df.parse(
              match2.get.replaceAll(",", "."))
              .doubleValue()).toDouble
          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }
        match0 = pat.findFirstIn(copy)
      }

      //TraiterOpenJDK17GC1.structGC1.throughput = 100 * (totalApplication / (totalApplication + totalStopped))

    } else {
      TraiterOpenJDK17GC1.structGC1.duration = Double.NaN
      TraiterOpenJDK17GC1.structGC1.sizeAfter = Double.NaN
      TraiterOpenJDK17GC1.structGC1.sizeBefore = Double.NaN

      TraiterOpenJDK17GC1.structGC1.parallelTime = Double.NaN
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
          try {
            count += 1
            var res = (TraiterOpenJDK17GC1.df.parse(
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
      TraiterOpenJDK17GC1.structGC1.threadsStopped = (totalStopped * 1000);
      if (count > 0) {
        TraiterOpenJDK17GC1.structGC1.moyStopped = (totalStopped * 1000 / count);
      } else {
        TraiterOpenJDK17GC1.structGC1.moyStopped = Double.NaN
      }
      TraiterOpenJDK17GC1.structGC1.maxStopped = (max * 1000);

      // Ajouter tous les total application stopped
      pat = "Application time.+?seconds".r

      copy = new String(tabString(0))
      match0 = pat.findFirstIn(tabString(0))
      var totalApplication = 0.0;

      while (None != match0) {
        var ext2 = match0.get
        copy = copy.substring(ext2.length());
        var match2 = pat2.findFirstIn(ext2)
        if (None != match2) {

          try {
            var res = (TraiterOpenJDK17GC1.df.parse(
              match2.get.replaceAll(",", "."))
              .doubleValue()).toDouble
            totalApplication += res;

          } catch {

            case e: ParseException => e.printStackTrace()
          }
        }
        match0 = pat.findFirstIn(copy)
      }

      if ((totalApplication + totalStopped) == 0) {
        TraiterOpenJDK17GC1.structGC1.throughput = Double.NaN
      } else {
        TraiterOpenJDK17GC1.structGC1.throughput = 100 * (totalApplication / (totalApplication + totalStopped))

      }

    }

    if (TraiterOpenJDK17GC1.ecartDateMillis > 0) {
      val str1 = (100.toDouble - (100 * TraiterOpenJDK17GC1.structGC1.parallelTime.toDouble /
        (TraiterOpenJDK17GC1.structGC1.parallelTime.toDouble + TraiterOpenJDK17GC1.ecartDateMillis.toDouble))).toString
      TraiterOpenJDK17GC1.structGC1.throughput = str1.substring(0, scala.math.min(5, str1.length())).toDouble
    } else {

      TraiterOpenJDK17GC1.structGC1.throughput = Double.NaN
    }
    if (TraiterOpenJDK17GC1.structGC1.parallelTime != Double.NaN) {
      TraiterOpenJDK17GC1.oldDateInMillis += TraiterOpenJDK17GC1.structGC1.parallelTime.toLong
    }
 // remplissage Throughput Memoire
     // println("Avant Traitement throughput memor"+TraiterHotspot5And6.structHotSpot.sizeHeapBefore+ " " +TraiterHotspot5And6.structHotSpot.sizeHeapAfter)
       TraiterOpenJDK17GC1.structGC1.memThroughput=Double.NaN
      if( !( TraiterOpenJDK17GC1.structGC1.sizeBefore.isNaN()) && !( TraiterOpenJDK17GC1.structGC1.sizeAfter.isNaN()))
      {
      var sweeped=TraiterOpenJDK17GC1.structGC1.sizeBefore-TraiterOpenJDK17GC1.structGC1.sizeAfter
     // println("Traitement throughput memor")
      TraiterOpenJDK17GC1.circleArray.put(( TraiterOpenJDK17GC1.oldDateInMillis,sweeped))
      TraiterOpenJDK17GC1.structGC1.memThroughput=TraiterOpenJDK17GC1.circleArray.throughput
      }

    
    
    
    
  }

}

class StructGC1 {
  var maxStopped = Double.NaN
  var moyStopped = Double.NaN
var memThroughput=Double.NaN
  var comtpeur = 0;
  var whattype = ""
  var sizeBefore = Double.NaN
  var sizeAfter = Double.NaN
  var duration = Double.NaN
  var threadsStopped = Double.NaN
  var parallelTime = Double.NaN
  var throughput = Double.NaN

}

object TraiterOpenJDK17GC1 {
  // : 0.377: 
  val patDate1 = """\d+(,|\.)\d+:\s+""".r
  var circleArray:CircleArray=null
  var oldDateInMillis = 0L
  var ecartDateMillis = 0L
  var lastEnrg = ""
  var structGC1: StructGC1 = null;
  var dfn = new DecimalFormatSymbols(
    Locale.ENGLISH);
  var df = new DecimalFormat("0.0", dfn);
  var dfToShow = new DecimalFormat("0.##", dfn);

}