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
// Exemple de classe non concurrente car le nom ne commence pas par Conc
// Non concurrent classes because the name doesn't start with Conc. One Actor is only possible
class SamplePlugin {
  
  def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
     SamplePlugin.count=0
  }
  def retour(tabStr:Array[String]):Double=
  {
    // Show parameters do that only for debug
    println("enr="+tabStr(0))
    for(i <- 1 until tabStr.length )
    {
      println("tabStr("+i+")="+tabStr(i))
    }
     
    SamplePlugin.count+=1
    // Do some stuff with enreg
    
    // return a result as Double
    42000D
  }

}
object SamplePlugin
{
// Put here static variable as counter ...
  var count=0
}