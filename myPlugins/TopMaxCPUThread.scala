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
class TopMaxCPUThread {
def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    // Nothing to do
  }
  def retour(tabStr: Array[String]): Double =
    {
    
		

		var enr = tabStr(0)
		var nbFieldForCPU=detectCPU(enr);
		
		// System.out.println("enr ="+enr);
		// System.out.println("regexp="+tabString[1]);
		// Motif du Time+
		var patLigne = "\\d+\\s+([^\\s]+\\s+){11}".r
		var pat = ("([^\\s]+\\s+){"+ nbFieldForCPU+"}").r
		var pat2 = "([^\\s]+\\s+)$".r
		var match0 = patLigne.findFirstIn(enr)
		

		var ext1 = "";
		var perCent = "0";
		var percentMax = 0;
		while (None != match0) {
			
			ext1 = match0.get
			enr = enr.substring(enr.indexOf(ext1) + ext1.length());
			val match2 = pat.findFirstIn(ext1)
			if (None != match2) {
				
				var ext2 = match2.get
				//System.out.println("ext2="+ext2);
				// System.out.println("Max ext1="+ext1);
				var  match3 = pat2.findFirstIn(ext2)
				if (None != match3) {
					var ext3 = match3.get
					var  lg =ext3.trim().toInt
					if (lg > percentMax) {
						percentMax = lg;

					}
				}
			}
			match0=patLigne.findFirstIn(enr)
		}

		percentMax.toDouble
	
    
    
    }
  private def  detectCPU( enr:String):Int=
	{
		var ret = -1 
		val pat="^.+?CPU\\s".r
		val  match0=pat.findFirstIn(enr)
		if(None != match0)
		{
			val tabStr1:Array[String]=match0.get.split("\\s+");
			ret=tabStr1.length;
		}
		ret	}
}