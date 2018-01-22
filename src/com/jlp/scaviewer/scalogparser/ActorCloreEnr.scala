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
package com.jlp.scaviewer.scalogparser

import akka.actor.Actor
import akka.actor.PoisonPill
class ActorCloreEnr (idActorCE:Int) extends Actor {
  
   def receive = {
    case "start" => cloreEnrs(idActorCE)
      
    case "stop" => self ! PoisonPill
   }

   final private def cloreEnrs(idActor:Int)
       {
     
		System.out.println("Demarrage ActorClodeEnr :"+idActor);
		for ( kj <- 0 until ScaParserMain.nbActors) {
			// cloture Enregistrement
			
			
			val iter=ScaParserMain.tabActorsFilesGenerated(kj)(idActor).hmCumulEnr.iterator
			iter foreach { tuple =>
			//while (iter.hasNext()) {
				//val mapEntry=iter.next
			
					ScaParserMain.tabActorsFilesGenerated(kj)(idActor).hmCumulEnr.put(tuple._1,tuple._2.closeEnr())

				
			}

		}
		ScaParserMain.tabBoolStopped(idActor) = true;
	
     
     
       }
}