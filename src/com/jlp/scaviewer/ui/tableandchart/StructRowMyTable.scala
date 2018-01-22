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
package com.jlp.scaviewer.ui.tableandchart
import java.awt.Color
import javax.swing.JSlider
import com.jlp.scaviewer.ui.Percent

case class StructRowMyTable ( shown:java.lang.Boolean=true,marked:java.lang.Boolean=false, color:Color, scale:MyScale,translate:Percent,source:String,tsName:String,avgPond:java.lang.Double,avg:java.lang.Double,
    min:java.lang.Double,max:java.lang.Double,stdv:java.lang.Double,irslope:java.lang.String,countPts:java.lang.Long,countAll:java.lang.Long,sumPond:java.lang.Double,sum:java.lang.Double,maxMax:java.lang.Double)
{

def toArray:Array[java.lang.Object]= 
  {
  val ret=new Array[java.lang.Object](18)
  ret(0)= shown
  ret(1)=marked
  ret(2)=color
  ret(3)=scale
  ret (4)=translate
  ret(5)=source
  ret(6)=tsName
  ret(7)=avgPond
  ret(8)=avg
  ret(9)=min
  ret(10)=max
  ret(11)=maxMax
  ret(12)=stdv
  ret(13)=irslope
  ret(14)=countPts
  ret(15)=countAll
  ret(16)=sumPond
  ret(17)=sum
  
  ret
  }
  
  
}