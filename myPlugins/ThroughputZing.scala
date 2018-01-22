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
class ThroughputZing {
def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
   ThroughputZing.oldDateInMillis=0L
  }
  def retour(tabStr: Array[String]): Double =
    {
    
		
		// System.out.println(" enr =|" + tabString[0] + "|");
		// String[0] => Enreg à traiter

		// 2 type de temps à recupérer sur un Young GC ou sur un full GC

		var found = false
		var echelle = 1D
		var pat ="\\d+\\.\\d+:".r
		var  match0 = pat.findFirstIn(tabStr(0))

		var newDate = 0L
		if (None != match0) {
			var ext1 = match0.get
			// System.out.println("ext1 =" + ext1);

			try {
				newDate = (1000 *  ThroughputZing.df.parse(
						ext1.substring(0, ext1.indexOf(":")).replaceAll(",",
								".")).doubleValue()).toLong;
			} catch {
			  case e:ParseException => e.printStackTrace
			} 

		}

		// cas du Young GC => mot cle "GC pause (young)"
		var val0 = " "

		// on recherche l dernier
		var  totalPause = 0L
		val  pat2 ="\\[[^\\]]+\\]".r
		var  match2 = pat2.findFirstIn(tabStr(0))
		if (None != match2) {
			var ext = match2.get
			
			var pauses = ext.split(":")(8).trim();
			
			var tabPauses:Array[String] = pauses.split("\\s+");
			try {
				totalPause =  (1000
						*  ThroughputZing.df.parse(tabPauses(1).replaceAll(",", "."))
								.doubleValue()
						+ 1000
						*  ThroughputZing.df.parse(tabPauses(3).replaceAll(",", "."))
								.doubleValue()
						+ 1000
						*  ThroughputZing.df.parse(tabPauses(5).replaceAll(",", "."))
								.doubleValue() + 1000
						*  ThroughputZing.df.parse(tabPauses(8).replaceAll(",", "."))
								.doubleValue()
						*  ThroughputZing.df.parse(tabPauses(6).replaceAll(",", "."))
								.doubleValue()).toLong
				
			} catch {
			   case e:ParseException => e.printStackTrace
			}

		}

		/*
		 * System.out.println("retour 6"); System.out.println("newDate =" +
		 * newDate); System.out.println("oldDateInMillis =" + oldDateInMillis);
		 * System.out.println("val =" + val); System.out.println("echelle =" +
		 * echelle);
		 */
		else {
			System.out.println("not found");
			System.out.println("enr=|" + tabStr(0) + "|");

			Double.NaN
		}
		var  res = 100 * (1 - (totalPause /  (newDate - ThroughputZing.oldDateInMillis).toDouble));
		ThroughputZing.oldDateInMillis = newDate;
		// System.out.println("res ="+res);
		
		if (res < 0 || res > 100) {
			// elimination despb d'ecriture dans le fichier.
				Double.NaN
		}
		// System.out.println(" Avant Retour ThroughPut : retour "+retour);
		res
	
    }
}
object ThroughputZing{
  
  var  oldDateInMillis = 0L
	val dfn = new DecimalFormatSymbols(
			Locale.ENGLISH);
	val df = new DecimalFormat("0.0", dfn);
	val  dfToShow = new DecimalFormat("0.##", dfn);
}