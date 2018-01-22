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
class ConcAdd2Values {
def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    // Nothing to do here
  }

/**
	 * Conseil de principe a utiliser tabString[0] est l enregistrement a
	 * traiter ensuite on a des regexp par doublet pour une extraction en double
	 * passe des donnees a traiter val_1 ess extraite en 2 passes avec les
	 * regexp tabString[1] et tabString[2] puis on peut apsser des parametre
	 * pour taiter les valeurs
	 * 
	 * @param tabStr
	 * @return
	 */
  def retour(tabStr:Array[String]):Double=
  {
    
		var retour=Double.NaN
		// for (int i = 0, len = tabString.length; i < len; i++) {
		// System.out.println("tabString[" + i + "]=" + tabString[i]);
		// }

		// recuperation premiere valeur
		var regex1=tabStr(1).r
		var ext1=regex1.findFirstIn(tabStr(0))
		var val1=0D
		var val2=0D
		if (None != ext1) {
			val regex2=tabStr(2).r
			val ext2=regex2.findFirstIn(ext1.get)
			if (None != ext2) {
				val1 = ext2.get.toDouble
			} else {
				return Double.NaN
			}
		} else {
			return Double.NaN
		}
		regex1=tabStr(3).r
		ext1=regex1.findFirstIn(tabStr(0))

		if (None != ext1) {
			val regex2=tabStr(4).r
			val ext2=regex2.findFirstIn(ext1.get)
			if (None != ext2) {
				val2 = ext2.get.toDouble
			} else {
				return Double.NaN
			}
		} else {
			return Double.NaN
		}

		// return the somme
		val1+val2
	
    
  }

}
