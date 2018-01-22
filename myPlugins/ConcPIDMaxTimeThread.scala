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
class ConcPIDMaxTimeThread {
  def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    // Nothing to do
  }
  def retour(tabStr: Array[String]): Double =
    {
    
		  
		var enr = tabStr(0)
		// System.out.println("enr ="+enr);
		// System.out.println("regexp="+tabString[1]);
		// Motif du Time+
		val patLigne="\\d+\\s+([^\\s]+\\s+){11}".r
		val pat = "\\s\\d+\\:\\d+\\.\\d+\\s+".r
		val pat2= "\\d+\\s+".r
		var match0 = patLigne.findFirstIn(enr)
		var  maxRSS:Int = 0;

		var ext1="";
		var pidMax="0";
		var timePlus=0L;
		while (None != match0) {
			ext1 = match0.get
			enr = enr.substring(enr.indexOf(ext1) + ext1.length());
			val match2=pat.findFirstIn(ext1)
			if(None != match2)
			{
			val  ext2=match2.get
			//System.out.println("Max ext1="+ext1);
			val tabStr1:Array[String]=ext2.split("\\:");
			var  lg=(tabStr1(0).trim()).toLong*60+(tabStr1(1).split("\\.")(0).trim()).toLong
			if(lg>timePlus)
				{
				timePlus=lg;
				var match3=pat2.findFirstIn(ext1)
				if(None != match3)
					pidMax=match3.get.trim();
				}
			}
			match0 =  patLigne.findFirstIn(enr)
		}

		
		 pidMax.toDouble
	
    
    }

}