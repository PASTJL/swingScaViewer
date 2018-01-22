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

class ConcDiffFirstDate_LastDate {
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
		// exemple pour 
		//2012-09-19 15:40:08,189;0004464b-1e67-4eac-98a0-9fb8e834c43d;1;NEC;PCL;Advise;get_message;START;;; 2012-09-19 15:40:08,219;0004464b-1e67-4eac-98a0-9fb8e834c43d;1;NEC;PCL;Advise;get_message;STOP;OK;;
		// tabStr1(1) Contient la regex des 2 dates
		//\d{4}-[^:]+[^;]+
		var regex1=tabStr(1).r
		// tabStr2 contient la jabva Date Format 
		// ex :yyyy-MM-dd HH:mm:ss,SSS
		val sdf=new SimpleDateFormat(tabStr(2))
	//	println("tabStr0="+tabStr(0))
		var ext1=regex1.findFirstIn(tabStr(0))
		var date1=0L
		var date2=0L
		if (None != ext1) {
		   val ext1Str=ext1.get
		  // println("ext1Str="+ext1Str)
		    date1=sdf.parse(ext1Str).getTime
		   // On tronque a partir de la fin de la premiere date touvee
		   
		    var idx=tabStr(0).indexOf(ext1Str)
			var newStr=tabStr(0).substring(idx+ext1Str.length())
		    var bool=true
		    var ext2=""
		    while(None != regex1.findFirstIn(newStr)){
		   
			//println("newStr="+newStr)
			ext2=regex1.findFirstIn(newStr).get
			idx=newStr.indexOf(ext2)
			newStr=newStr.substring(idx+ext2.length())
		    }
			if (ext2 !="") {
				
				//println("ext2Str="+ext2Str)
				date2= sdf.parse( ext2).getTime
			} else {
				return Double.NaN
			}
		 
		}else {
			return Double.NaN
		}
		

		

		// return the diff
		(date1-date2).abs.toDouble
		
	
    
  }

}
