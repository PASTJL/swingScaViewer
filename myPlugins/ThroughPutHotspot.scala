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

import java.text.SimpleDateFormat
import java.text.ParseException
class ThroughPutHotspot {

def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    // Nothing to do
   ThroughPutHotspot. oldDateInMillis=0
  }
  def retour(tabStr: Array[String]): Double =
    {
    
		
		//System.out.println(" enr =|"+tabString[0]+"|");
		// String[0] => Enreg to treat
		// String[1] =>regexp1 to extract date of current enreg
		// String[2] => Java forma date or dateInMillis=<millis>
		//String[3] => regexp1 to extract the value
		//String[4] => regexp2 to extract the value from extraction with regexp1
		//String[5] => scale to transform unit of date
		// recuperation premiere valeur
		
		var  pat = tabStr(1).r
		var match0 = pat.findFirstIn(tabStr(0))
		var val1=" "
		var val2=" "
		var newDate=0L
		
		if(None != match0)
		{
			var ext1=match0.get
			//System.out.println("ext1 ="+ext1);
			
			if(tabStr(2).equals("dateInMillis"))
			{
				
			 newDate= (ext1.toDouble*tabStr(5).toDouble).toLong
			}
			else
			{
				val sdf=new SimpleDateFormat(tabStr(2))
				try
				{
				newDate=sdf.parse(ext1).getTime();
				}
				catch {
				  case pe:ParseException => pe.printStackTrace
				}
				
				
				
			}
			
			
		}
		pat = tabStr(3).r
		match0 = pat.findFirstIn(tabStr(0))
		
		if(None != match0)
		{
			var  ext1=match0.get
			val  pat1 = tabStr(4).r
			val match1 = pat1.findFirstIn(ext1)
		
			if(None != match1)
			{
				val2=match1.get
			}
			else
			{
				val2="0";
			}
		}
		/*System.out.println("retour 6");
		System.out.println("newDate ="+newDate);
		System.out.println("oldDateInMillis ="+oldDateInMillis);
		System.out.println("val2 ="+val2);
		System.out.println("echelle ="+tabString[5]);*/
		
		var  res:Double=100*(1- (val2.toDouble)* tabStr(5).toDouble)/(newDate- ThroughPutHotspot.oldDateInMillis);
		 ThroughPutHotspot.oldDateInMillis=newDate
		//System.out.println("res ="+res);
		
		if (res <0 || res > 100)
		{
			// elimination despb d'ecriture dans le fichier.
			Double.NaN
		}
//	System.out.println(" Avant Retour ThroughPut : retour "+retour);
		res
	
    
    }


}
object ThroughPutHotspot {
  var oldDateInMillis=0L
}
