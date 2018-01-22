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
package com.jlp.scaviewer.ui

case class Percent(var perCent:Int) {


  
  def  setPercent(per:Int)= {
    perCent = 0
    if(per < 0){
      perCent=0
    } else if (per > 100)
      {
      perCent=100
      }
    else
    {
      perCent=per
    }
  }

 def setPercent( any:Any) {
    if (any.isInstanceOf[String]) {
      setPercent(any.asInstanceOf[String].toInt);
    } else if (any.isInstanceOf[Number]) {
      setPercent((any.asInstanceOf[Number].intValue()))
    } else if (any.isInstanceOf[Percent]) {
      setPercent(any.asInstanceOf[Percent].getPercent)
    }
  }

  def getPercent():Int= {
    perCent;
  }

  override def toString() ={
    String.valueOf(perCent);
  }
}