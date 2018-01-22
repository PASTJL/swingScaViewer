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
class ConcPmap {
  
  def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    // Nothing to do
  }
  def retour(tabStr: Array[String]): Double =
    {
    
		
		// recuperation premiere valeur
			var  enr=tabStr(0)
		//System.out.println("enr ="+enr);
		//System.out.println("regexp="+tabString[1]);
	
		val pat = tabStr(1).r
		val  patTaille1="[0-9a-f]{16}\\s+\\d+".r
		val patTaille2="\\d+$".r
		if(null == pat)
		{
		Double.NaN
    }
		var  match0 = pat.findFirstIn(enr)
		var counter=0L
		
		while(None != match0)
		{
		 
		  
		 
			var  ext1=match0.get
			
			var  match1=patTaille1.findFirstIn(ext1)
			if(None != match1)
			 {
			   var  ext2=match1.get
			var match2=patTaille2.findFirstIn(ext2)
			   if(None != match2)
			   {
			   var  add=match2.get
			   counter+=add.toLong
			  // System.out.println("add="+add);
			  // System.out.println("counter="+counter);
         }
        
       }
			
			enr=enr.substring(enr.indexOf(ext1)+ext1.length());
			match0 = pat.findFirstIn(enr)
			
			
      }
			
		
		//System.out.println("retour="+retour);
		counter.toDouble
	
    
    
    }
  

}