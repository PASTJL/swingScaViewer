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
class ConcSumValues {
def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    // Nothing to do
  }
  def retour(tabStr: Array[String]): Double =
    {
    
    
		
		var cumul = 0.0;
		var boolMatch = false;

		// recuperation premiere valeur
		var enr = tabStr(0)
		// deux regexp pour extrraire les valeurs.
		// 1 iere regexp pour extraire tous les groupes
		// 2ieme regexp pour extraire la valeur finale de chaque groupe
		// System.out.println("enr ="+enr);
		// System.out.println("regexp="+tabString[1]);

		val pat = tabStr(1).r
		val pat2 = tabStr(2).r
	//	System.out.println("pat="+pat);
		//   	System.out.println("pat2="+pat2);
		if (null == pat) {
			Double.NaN
		}
		var  match0 = pat.findFirstIn(enr)
    
		while (None != match0) {
              // System.out.println("Le groupe principal est trouve");
		

				var ext1 = match0.get
				//    System.out.println("ext1="+ext1);
				
				val match2 = pat2.findFirstIn(ext1)
				if (None != match2) {
					boolMatch = true;
					val ext2=  match2.get
					      //System.out.println("ext2="+ext2);
					var dbl = ext2.toDouble
					cumul += dbl
			

			}
			enr=enr.substring(enr.indexOf(ext1)+ext1.length());
			match0=pat.findFirstIn(enr);

		}

		if (boolMatch) {
			cumul.toDouble
			
		} else {
			Double.NaN
		}
	
    
    }
}