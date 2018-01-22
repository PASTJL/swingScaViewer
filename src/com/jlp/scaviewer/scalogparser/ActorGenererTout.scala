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
import java.io.File
import java.io.RandomAccessFile
import java.io.FileNotFoundException
import java.io.IOException
import java.util.Calendar
import java.text.SimpleDateFormat

class ActorGenererTout(idActorGT:Int) extends Actor {
  
  def receive = {
    case "start" => genTout(idActorGT)
      
    case "stop" => context.stop(self)
   }

   final private def genTout(idActor:Int)
       {
    
	  	val  sdfCsv=new SimpleDateFormat(ScaParserMain.strSdfCsv)
		var fullName = ScaParserMain.reportCsvDirectory + File.separator	+ ScaParserMain.tabStrFilesCsv(idActor)

		//var trace=ScaParserMain.reportCsvDirectory + File.separator	+ ScaParserMain.tabStrFilesCsv(idActor)+".trc"
		System.out.println("IdActor="+idActor+" ;Remplissage du fichier :fullName =" + fullName);
		//var rafTrace:RandomAccessFile = null;
		var	 raf:RandomAccessFile = null;
		
		try {
			raf = new RandomAccessFile(fullName, "rw");
		//	rafTrace = new RandomAccessFile(trace, "rw");
			val size = raf.length();
			raf.seek(size);
		} catch  {
		  case e:FileNotFoundException => e.printStackTrace()
		 case e:IOException => e.printStackTrace()
			
		} 

		var len = ScaParserMain.tabActorsFilesGenerated(0)(idActor).hmCumulEnr.size

		

		val hmTmp = ScaParserMain.tabActorsFilesGenerated(0)(idActor).hmCumulEnr
		val iter=ScaParserMain.tabActorsFilesGenerated(0)(idActor).hmCumulEnr.iterator
		val cal=Calendar.getInstance
		
		
		iter foreach { tuple =>
		 
			if (tuple._1 < ScaParserMain.tabLongDeb(idActor)) {
				ScaParserMain.tabLongDeb(idActor) = tuple._1
			}
			if (tuple._1 > ScaParserMain.tabLongFin(idActor)) {
				ScaParserMain.tabLongFin(idActor) = tuple._1
			}

			
			cal.setTimeInMillis(tuple._1)
			

			
	//		rafTrace.writeBytes(tuple._1+";"+ sdfCsv.format(cal.getTime())+";"+sdfCsv.toPattern()+"\n")

			// System.out.println("chargement de dateStr " +
			// dateStr);
			var str = "";
			var strB = new StringBuilder();
			// traitement par valeur
			strB.append( sdfCsv.format(cal.getTime())).append(ScaParserMain.csvSeparator);
			// on va tester si la valeur a un sens

			for ( i <- 0 until ScaParserMain.sizeValues) {
				if(tuple._2.counts(i)>0)
				{
					strB.append(tuple._2.averages(i)).append(ScaParserMain.csvSeparator).
							append(tuple._2.maxs(i)).append(ScaParserMain.csvSeparator).
							append(tuple._2.mins(i)).append(ScaParserMain.csvSeparator).
							append(tuple._2.rates(i)).append(ScaParserMain.csvSeparator).
							append(tuple._2.counts(i)).append(ScaParserMain.csvSeparator).
							append(tuple._2.sums(i)).append(ScaParserMain.csvSeparator)
				}
				else
				{
					strB.append(ScaParserMain.csvSeparator).
					append(ScaParserMain.csvSeparator).
					append(ScaParserMain.csvSeparator).
					append(ScaParserMain.csvSeparator).
					append(ScaParserMain.csvSeparator).
					append(ScaParserMain.csvSeparator)
				}
				
				if (tuple._2.boolDurations(i))
					if(tuple._2.countParallels(i).get>0) {
					strB.append(tuple._2.countParallels(i).get).append(ScaParserMain.csvSeparator)
					}
					else
					{
						strB.append(ScaParserMain.csvSeparator)
				}

			}
			str = strB.append("\n").toString();

			try {
				raf.writeBytes(str);

			} catch  {
			  
			  case e:IOException => e.printStackTrace()
			  
			}

		}

		// hmTmp.clear();
		// hmTmp = null;
		try {
			if (null != raf) {
				raf.close();
				
			}
//			if (null != rafTrace) {
//				
//				rafTrace.close()
//			}
		}  catch  {
			  
			  case e:IOException => e.printStackTrace()
			  
			}

		System.out.println("Actor gen ="+idActor+" ; timeMini="+ScaParserMain.tabLongDeb(idActor)+
		   " ; timeMax="+ScaParserMain.tabLongFin(idActor) )
		ScaParserMain.tabBoolStopped(idActor) = true;
		self !  "stop"
       }

}