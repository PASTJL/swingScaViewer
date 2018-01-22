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
// Exemple de classe  concurrente car le nom  commence  par Conc
//  concurrent classes because the name starts with Conc. Many Actors are possible

import java.util.concurrent.atomic.AtomicInteger
class ConcSamplePlugin {
  
 def metInit(tab:Array[String]=null) {
    // To reinitialise static variable if necessary
     ConcSamplePlugin.count=new AtomicInteger(0)
  }
  def retour(tabStr:Array[String]):Double=
  {
    // Show parameters do that only for debug
    println("enr="+tabStr(0))
    for(i <- 1 until tabStr.length )
    {
      println("tabStr("+i+")="+tabStr(i))
    }
     
    ConcSamplePlugin.count.getAndAdd(1)
    // Do some stuff with enreg
    
    // get a Double from the stuff and return a result as Double
    42000D
  }

}
object  ConcSamplePlugin 
{
// Put here static variable as counter ...
  // In concurrent mode be care to handle thread safe attribute ( get/set)
  var count:AtomicInteger= new AtomicInteger(0)
}