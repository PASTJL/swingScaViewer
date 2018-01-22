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
package com.jlp.scaviewer.filestats

case class StructPasCumul( count0:Int, moyen:Double) {
  var moyenne:Double=moyen
  var nbCount=count0
def  add( value:Double) :StructPasCumul={
		moyenne = ((moyenne * nbCount + value) / (nbCount + 1))
		nbCount = nbCount + 1;
		this
	}

def  merge( other:StructPasCumul) :StructPasCumul={
		var sommeGlobal:Double = nbCount * moyenne + other.nbCount * other.moyenne
		nbCount += other.nbCount
		moyenne = sommeGlobal / nbCount
		this
	}
}