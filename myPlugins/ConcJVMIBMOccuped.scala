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
class ConcJVMIBMOccuped {
def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    // Nothing to do
  }
  def retour(tabStr: Array[String]): Double =
    {
    
	val patByte="\\d+$";
		
	val pat1NurseryAvant="<nursery[^\"]+?\"\\d+\"[^\"]+\"\\d+";
    val pat2NurseryAvant="<nursery[^\"]+?\"\\d+";
   val pat3NurseryApres="</gc>\\s+<nursery[^\"]+?\"\\d+\"[^\"]+\"\\d+";
   val pat4NurseryApres="</gc>\\s+<nursery[^\"]+?\"\\d+";

    val pat1TenuredAvant="<tenured[^\"]+?\"\\d+\"[^\"]+\"\\d+";
    val pat2TenuredAvant="<tenured[^\"]+?\"\\d+";
    val pat3TenuredApres="</gc>[^<]+<[^<]+<tenured[^\"]+?\"\\d+\"[^\"]+\"\\d+";
   val pat4TenuredApres="</gc>[^<]+<[^<]+<tenured[^\"]+?\"\\d+";

val retour=" ";
		
var val1=" ";
var  val2=" ";
var val3=" ";
var  val4=" ";
		
		// recuperation  valeur Nursery  "AVANT"
		var pat =pat1NurseryAvant.r
		var  match0 = pat.findFirstIn(tabStr(0))
		
		if(None != match0)
		{
			val  ext1=match0.get
			val  pat1 = patByte.r
			val  match1 = pat1.findFirstIn(ext1)
			if(None != match1)
			{
				val1 = match1.get
			}
			else
			{
				val1="0"
			}
		}
		else
		{
			val1="0"
		}
		pat = pat2NurseryAvant.r
		match0 = pat.findFirstIn(tabStr(0))
		
		if(None != match0)
		{
			val  ext1=match0.get
			val pat1 = patByte.r
			var match1 = pat1.findFirstIn(ext1)
			if(None != match1)
			{
				val2=match1.get
			}
			else
			{
				val2="0";
			}
		}
		else
		{
			val2="0";
		}
		var nurseryAvant=val1.toDouble-val2.toDouble
		
		// recuperation   valeur Nursery  "APRES"
		 pat =pat3NurseryApres.r
		 match0 = pat.findFirstIn(tabStr(0))
		
		if(None != match0)
		{
			val  ext1=match0.get
			val  pat1 = patByte.r
			val  match1 = pat1.findFirstIn(ext1)
			if(None != match1)
			{
				val1=match1.get
			}
			else
			{
				val1="0";
			}
		}
		else
		{
			val1="0";
		}
		pat = pat4NurseryApres.r
		match0 = pat.findFirstIn(tabStr(0))
		
		if(None != match0)
		{
			val ext1=match0.get 
			val  pat1 =patByte.r
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
		else
		{
			val2="0";
		}
		
		val  nurseryApres=val1.toDouble-val2.toDouble
		
		// recuperation   valeur Tenured   "AVANT"
		 pat = pat1TenuredAvant.r
		 match0 = pat.findFirstIn(tabStr(0))
		
		if(None != match0)
		{
			val ext1=match0.get
			val pat1 = patByte.r
			val  match1 = pat1.findFirstIn(ext1)
			if(None != match1)
			{
				val1=match1.get
			}
			else
			{
				val1="0";
			}
		}
		else
		{
			val1="0";
		}
		pat = pat2TenuredAvant.r
		match0 = pat.findFirstIn(tabStr(0))
		
		if( None != match0)
		{
			val  ext1=match0.get
			val pat1 =patByte.r
			val  match1 = pat1.findFirstIn(ext1)
			if(None != match1)
			{
				val2=match1.get
			}
			else
			{
				val2="0";
			}
		}
		else
		{
			val2="0";
		}
		
		val  tenuredAvant=val1.toDouble-val2.toDouble		
		// recuperation   valeur Tenured   "APRES"
		 pat =pat3TenuredApres.r
		 match0 = pat.findFirstIn(tabStr(0));
		
		if(None != match0)
		{
			val  ext1=match0.get
			val  pat1 = patByte.r
			val  match1 = pat1.findFirstIn(ext1)
			if(None != match1)
			{
				val1=match1.get
			}
			else
			{
				val1="0";
			}
		}
		else
		{
			val1="0";
		}
		pat = pat4TenuredApres.r
		match0 = pat.findFirstIn(tabStr(0))
		
		if(None != match0)
		{
			val  ext1=match0.get
			val pat1 = patByte.r
			val  match1 = pat1.findFirstIn(ext1)
			if(None != match1)
			{
				val2=match1.get
			}
			else
			{
				val2="0";
			}
		}
		else
		{
			val2="0";
		}
		
		val tenuredApres=val1.toDouble-val2.toDouble
		
		
		var  res=0D
		if(tabStr(1).toLowerCase().equals("before"))
		{
			nurseryAvant+tenuredAvant;
			
		}
		else if(tabStr(1).toLowerCase().equals("after"))
		{
			nurseryApres+tenuredApres;
			
		}
		
		else Double.NaN
		
    
    
    }
}