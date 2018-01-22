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
class ConcCompute2Values {
def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    // Nothing to do here
  }

	/**
	*tabStr(0) is the record to be treated. Afterwards the regex by tuple of 2 items for an extraction in two phases
	* regex tabStr(1) and tabStr(2) to extract the first value
	* regex tabStr(3) and tabStr(4) to extract the second value
	* tabStr(5) is the operand (+,-,*,/) 
	* @param tabStr
	* @return
	*/
  def retour(tabStr:Array[String]):Double=
  {
    
		var retour=Double.NaN
		
		// extract first value
		var regex1=tabStr(1).r
		var ext1=regex1.findFirstIn(tabStr(0))
		
		var val1=0D
		var val2=0D
		if (None != ext1) {
		//  println("ext1Val1="+ext1.get)
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

		// extract second value
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
		//println ("tabStr(0) ="+tabStr(0))
		//println ("val1="+val1+" ;val2="+val2)
		// return the result of the operation
		tabStr(5) match {
		case "+" =>	val1 + val2
		case "-" => val1-val2
		case "/" => if (val2 !=0) val1 / val2 else Double.NaN
		case "*" => val1 * val2
		case _ => Double.NaN
	
    
  }
  }

}