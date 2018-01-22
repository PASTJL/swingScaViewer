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
import java.text.ParseException
import java.text.SimpleDateFormat

class ThroughputHotspot15 {
def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    ThroughputHotspot15.oldDateInMillis = 0L
  }
  def retour(tabStr: Array[String]): Double =
    {
    
		
		// System.out.println(" enr =|" + tabString[0] + "|");
		// String[0] => Enreg à traiter

		// 2 type de temps à recupérer sur un Young GC ou sur un full GC

		var found = false
		val echelle = 1D
		var  pat = "\\d{4}-\\d{2}-\\d{2}T\\d\\d:\\d\\d:\\d\\d\\.\\d+".r
		var  match0 = pat.findFirstIn(tabStr(0))
		val  javaDateFormat = "yyyy-MM-dd\'T\'HH:mm:ss.S"
		var  newDate = 0L
		if (None != match0) {
			var ext1 = match0.get
			// System.out.println("ext1 =" + ext1);

			val sdf = new SimpleDateFormat(javaDateFormat);
			try {
				newDate = sdf.parse(ext1).getTime();
			} catch {
			  case pe:ParseException => pe.printStackTrace
			  
			}

		}

		// cas du Young GC => mot cle "GC pause (young)"
		var val0 = " "

		// System.out.println("GC pause (young)");
		pat ="real=\\d+\\.\\d+".r
		match0 = pat.findFirstIn(tabStr(0))
		if (None != match0) {
			found = true;
			var ext1 = match0.get
			val pat1 = "\\d+\\.\\d+".r
			var match1 = pat1.findFirstIn(ext1)
			// System.out.println("ext1=|" + ext1 + "|");
			if (None != match1) {
				val0 = match1.get
			} else {
				val0 = "0"
			}

		}

		/*
		 * System.out.println("retour 6"); System.out.println("newDate =" +
		 * newDate); System.out.println("oldDateInMillis =" + oldDateInMillis);
		 * System.out.println("val =" + val); System.out.println("echelle =" +
		 * echelle);
		 */
		if (!found) {
			System.out.println("not found");
			System.out.println("enr=|" + tabStr(0) + "|");

			Double.NaN
		}
		var res = 100 * (1 - (val0.toDouble * echelle)
				/  (newDate - ThroughputHotspot15.oldDateInMillis).toDouble)
		ThroughputHotspot15.oldDateInMillis = newDate;
		// System.out.println("res ="+res);
		
		if (res < 0 || res > 100) {
			// elimination despb d'ecriture dans le fichier.
			Double.NaN
		}
		// System.out.println(" Avant Retour ThroughPut : retour "+retour);
		res
	
    
    }
}
object ThroughputHotspot15 {
 var oldDateInMillis = 0L
}