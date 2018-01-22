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


class TraiterAZULZing {
def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    TraiterAZULZing. oldDateInMillis = 0L

	TraiterAZULZing.structZing = null
  }
  def retour(tabStr: Array[String]): Double =
    {
    
		
		// System.out.println(" enr =|" + tabString[0] + "|");
		// String[0] => Enreg à traiter

		// 2 type de temps à recupérer sur un Young GC ou sur un full GC
		// Le premier champ est l'enregistrement, le deuxieme champ est le type
		// (
		// Young GC/FullGC),le troisièmee champ la valeur a renvoyer, le
		// quatrieme champ est le nombre de parametres
		// geres pour le parsing defaut 4.
		// Creation d'une structure d'acceuil pour cet enregistrement
		//
		if (null == TraiterAZULZing.structZing) {
			if (tabStr.length == 4) {
				TraiterAZULZing.structZing = new StructZing(tabStr(3).toInt)
			} else {
				TraiterAZULZing.structZing = new StructZing(9);
			}
			remplirStructure(tabStr);
		}
		var retour = Double.NaN

		/*
		 * public String liveObjects = ""; public String peakBefore = ""; public
		 * String newUsage = ""; public int maxCompteur = 9; public int
		 * compteur=0; public String permUsage = ""; public String oldUsage =
		 * ""; public String gcThreads = ""; public String totalThreads = "";
		 * public String pauseTime = ""; public String throughput = "";
		 */

		if (tabStr(1).contains("GC NTO") && tabStr(0).contains("GC NTO")) {

			if (tabStr(2).equals("liveObjects")) {
				TraiterAZULZing.structZing.compteur += 1
				retour = TraiterAZULZing.structZing.liveObjects
			} else if (tabStr(2).equals("peakBefore")) {
				TraiterAZULZing.structZing.compteur += 1 
				retour = TraiterAZULZing.structZing.peakBefore
			} else if (tabStr(2).equals("newUsage")) {
				TraiterAZULZing.structZing.compteur += 1
				retour = TraiterAZULZing.structZing.newUsage
			} else if (tabStr(2).equals("permUsage")) {
				TraiterAZULZing.structZing.compteur += 1
				retour = TraiterAZULZing.structZing.permUsage

			} else if (tabStr(2).equals("oldUsage")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.oldUsage

			} else if (tabStr(2).equals("gcThreads")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.gcThreads

			} else if (tabStr(2).equals("totalThreads")) {
				TraiterAZULZing.structZing.compteur += 1
				retour = TraiterAZULZing.structZing.totalThreads

			} else if (tabStr(2).equals("pauseTime")) {
				TraiterAZULZing.structZing.compteur  +=1
				retour = TraiterAZULZing.structZing.pauseTime;

			} else if (tabStr(2).equals("throughput")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.throughput;

			}
			if (TraiterAZULZing.structZing.compteur == TraiterAZULZing.structZing.maxCompteur) {
				TraiterAZULZing.structZing = null;
			}

		} else if (tabStr(1).contains("GC Old")
				&& tabStr(0).contains("GC Old")) {
			if (tabStr(2).equals("liveObjects")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.liveObjects;
			} else if (tabStr(2).equals("peakBefore")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.peakBefore;
			} else if (tabStr(2).equals("newUsage")) {
				TraiterAZULZing.structZing.compteur +=1 
				retour = TraiterAZULZing.structZing.newUsage;
			} else if (tabStr(2).equals("permUsage")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.permUsage;

			} else if (tabStr(2).equals("oldUsage")) {
				TraiterAZULZing.structZing.compteur += 1
				retour = TraiterAZULZing.structZing.oldUsage;

			} else if (tabStr(2).equals("gcThreads")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.gcThreads;

			} else if (tabStr(2).equals("totalThreads")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.totalThreads;

			} else if (tabStr(2).equals("pauseTime")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.pauseTime;

			} else if (tabStr(2).equals("throughput")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.throughput;

			}
			if (TraiterAZULZing.structZing.compteur == TraiterAZULZing.structZing.maxCompteur) {
				TraiterAZULZing.structZing = null;
			}
		} else if (tabStr(1).contains("GC New")
				&& tabStr(0).contains("GC New")) {
			if (tabStr(2).equals("liveObjects")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.liveObjects;
			} else if (tabStr(2).equals("peakBefore")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.peakBefore;
			} else if (tabStr(2).equals("newUsage")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.newUsage;
			} else if (tabStr(2).equals("permUsage")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.permUsage;

			} else if (tabStr(2).equals("oldUsage")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.oldUsage;

			} else if (tabStr(2).equals("gcThreads")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.gcThreads;

			} else if (tabStr(2).equals("totalThreads")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.totalThreads;

			} else if (tabStr(2).equals("pauseTime")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.pauseTime;

			} else if (tabStr(2).equals("throughput")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.throughput;

			}
			if (TraiterAZULZing.structZing.compteur == TraiterAZULZing.structZing.maxCompteur) {
				TraiterAZULZing.structZing = null;

			}
		}

		else if (tabStr(1).contains("AllGen")) {

			if (tabStr(2).equals("liveObjects")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.liveObjects;
			} else if (tabStr(2).equals("peakBefore")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.peakBefore;
			} else if (tabStr(2).equals("newUsage")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.newUsage;
			} else if (tabStr(2).equals("permUsage")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.permUsage;

			} else if (tabStr(2).equals("oldUsage")) {
				TraiterAZULZing.structZing.compteur+=1
				retour = TraiterAZULZing.structZing.oldUsage;

			} else if (tabStr(2).equals("gcThreads")) {
				TraiterAZULZing.structZing.compteur +=1 
				retour = TraiterAZULZing.structZing.gcThreads;

			} else if (tabStr(2).equals("totalThreads")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.totalThreads;

			} else if (tabStr(2).equals("pauseTime")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.pauseTime;

			} else if (tabStr(2).equals("throughput")) {
				TraiterAZULZing.structZing.compteur +=1
				retour = TraiterAZULZing.structZing.throughput;

			}
			if (TraiterAZULZing.structZing.compteur == TraiterAZULZing.structZing.maxCompteur) {
				TraiterAZULZing.structZing = null;
			}
		} else {
			TraiterAZULZing.structZing = null;
		}

		return retour;
	
    }
  
  	private def remplirStructure( tabString:Array[String])
  	{
  	  
		var found = false;
		var  echelle = 1D
		var  pat = "\\d+\\.\\d+:".r
		var  match0 = pat.findFirstIn(tabString(0))

		var  newDate = 0L
		if (None != match0) {
			var ext1 = match0.get
			// System.out.println("ext1 =" + ext1);

			try {
				newDate =  (1000 *  TraiterAZULZing.df.parse(
						ext1.substring(0, ext1.indexOf(":")).replaceAll(",",
								".")).doubleValue()).toLong
			} catch {
			  case e:ParseException => e.printStackTrace()
			}

		}

		var  patCrochets = "\\[[^\\]]+\\]".r
		var match1 = patCrochets.findFirstIn(tabString(0))
		var totalPause = 0L
		if (None != match1) {
			var ext = match1.get
			// liveObjects
			TraiterAZULZing.structZing.liveObjects = ext.split(":")(3).trim().split("\\s+")(0).toDouble
			// peakBefore
			TraiterAZULZing.structZing.peakBefore = ext.split(":")(1).trim().split("\\s+")(1).toDouble
			// newUsage
			TraiterAZULZing.structZing.newUsage = ext.split(":")(2).trim().split("\\s+")(0).toDouble
			// permUsage
			TraiterAZULZing.structZing.permUsage = ext.split(":")(2).trim().split("\\s+")(2).toDouble
			// oldUsage
			TraiterAZULZing.structZing.oldUsage = ext.split(":")(2).trim().split("\\s+")(1).toDouble
			// gcThreads
			TraiterAZULZing.structZing.gcThreads = ext.split(":")(6).trim().split("\\s+")(0).toDouble
			// totalThreads
			TraiterAZULZing.structZing.totalThreads = ext.split(":")(7).trim().split("\\s+")(0).toDouble
			// totalPause
			var pauses = ext.split(":")(8).trim();
			// System.out.println("pauses=" + pauses);
			var tabPauses = pauses.split("\\s+");
			try {
				totalPause =  (1000
						*  TraiterAZULZing.df.parse(tabPauses(1).replaceAll(",", ".")).doubleValue()
						+ 1000*  TraiterAZULZing.df.parse(tabPauses(3).replaceAll(",", "."))
								.doubleValue()
						+ 1000
						* TraiterAZULZing.df.parse(tabPauses(5).replaceAll(",", "."))
								.doubleValue() + 1000
						*  TraiterAZULZing.df.parse(tabPauses(8).replaceAll(",", "."))
								.doubleValue()
						* TraiterAZULZing.df.parse(tabPauses(6).replaceAll(",", "."))
								.doubleValue()).toLong
				// System.out.println("totalPauses=" + totalPause);
			} catch 
			{
			   case e:ParseException => e.printStackTrace()
			}

			TraiterAZULZing.structZing.pauseTime = totalPause.toDouble
			// throughput
			var res:Double = 100 * (1 - (totalPause /  (newDate - TraiterAZULZing.oldDateInMillis).toDouble))
			TraiterAZULZing.oldDateInMillis = newDate
			// System.out.println("res ="+res);
			var retour = res.toString
			if (res < 0 || res > 100) {
				// elimination despb d'ecriture dans le fichier.
				TraiterAZULZing.structZing.throughput=Double.NaN
			}
			TraiterAZULZing.structZing.throughput = res
			// System.out.println(" Avant Retour ThroughPut : retour "+retour);
		}

	
  	}
  
}

object TraiterAZULZing{
var  oldDateInMillis = 0L

	var  structZing:StructZing = null
	val  dfn = new DecimalFormatSymbols(
			Locale.ENGLISH)
	val df = new DecimalFormat("0.0", dfn);
	val dfToShow = new DecimalFormat("0.##", dfn);

}
class StructZing(nbMetrics:Int) {
	var liveObjects = Double.NaN
	var peakBefore =  Double.NaN
	var newUsage =  Double.NaN
	var maxCompteur = nbMetrics
	var compteur = 0;
	var permUsage =  Double.NaN
	var oldUsage =  Double.NaN
	var gcThreads =  Double.NaN
	var  totalThreads =  Double.NaN
	var pauseTime =  Double.NaN
	var throughput =  Double.NaN

	
	}