
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
// Exemple de classe concurrente car le nom  commence  par Conc
// Concurrent classes because the name  starts with Conc. Several Actors are possible

class ConcTuxCloptr {
def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
    //nothing to do
  }
  def retour(tabStr:Array[String]):Double=
  {
    // Show parameters do that only for debug
    val tabStrLine=tabStr(0).split("\\s+")
    if(tabStrLine.length != 6)     Double.NaN
   // println(tabStrLine(0)+" duration in cs ="+(tabStrLine(5).toLong-tabStrLine(3).toLong))
    (tabStrLine(5).toLong-tabStrLine(3).toLong).toDouble
    // Do some stuff with enreg
    
    // return a result as Double
    
  }
}