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
class ConcMaxSizeRss {
def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    // Nothing to do
  }
  def retour(tabStr: Array[String]): Double =
    {
    
    
		

		// recuperation premiere valeur
		var  enr = tabStr(0).substring(18);
		// System.out.println("enr ="+enr);
		// System.out.println("regexp="+tabString[1]);

		val pat ="(\\d+\\s+){3}\\d+".r
		
		var  match0 = pat.findFirstIn(enr)
		var  maxRSS:Int = 0;

		var ext1="";
		while (None != match0) {
			ext1 = match0.get
			//System.out.println("Max ext1="+ext1);
			enr = enr.substring(enr.indexOf(ext1) + ext1.length());
			
			
				maxRSS=scala.math.max(maxRSS, ext1.split("\\s+")(1).toInt)
			

			match0 = pat.findFirstIn(enr)

		}

		maxRSS.toDouble
	
    
    
    }
}