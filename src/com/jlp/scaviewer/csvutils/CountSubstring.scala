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
package com.jlp.scaviewer.csvutils
import java.lang.System
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
class CountSubstring {
  def countItems(in: String, subString: String): Int =
    {
      val reg = subString.r
      val iter = reg.findAllIn(in);
      iter.size
    }

  def split(line: String, sep: String): List[String] =
    {
   
    var ret=ListBuffer[String]()
    var strBuilder=new StringBuilder(line)
    val lengthSep=sep.length
    while(!strBuilder.isEmpty)
    {
      var index=strBuilder.indexOf(sep)
      var strBtmp= new StringBuilder()
     // var strBtmp= new ArrayBuffer()
      for(i <- 0 until index)
        {
        strBtmp+=strBuilder.head
        strBuilder=strBuilder.tail
      
        }
      ret=ret:+strBtmp.toString
      for(j <- 0 until lengthSep)
      {
       
        strBuilder=strBuilder.tail
      }
    }

      ret.toList
    }
}
object CountSubstring {
  def main(args: Array[String]): Unit = {

    val line = "2010/05/12 15:41:14;8432.598881355932;4830.459241379311;;10972.653499999999;7427.22225;;16277.9205;;;"
    val line2 = "2010/05/12 15:41:14;8432.598881355932;4830.459241379311;;10972.653499999999;7427.22225;;16277.9205;; ;"
      
    println("numberSep=" + new CountSubstring().countItems(line, ";"))
    println("numberFields=" + ";".r.split(line).length)
    println("numberFields1=" + ";".r.split(line2).length)
   for(i <- 0 until 10)
   {
    var deb=System.nanoTime
    val list= new CountSubstring().split(line, ";")
    println("duree list1="+(System.nanoTime-deb))
    deb=System.nanoTime
    val list2= new CountSubstring().split(line2, ";")
     println("duree list2="+(System.nanoTime-deb))
     deb=System.nanoTime
    val list3=line2.split(";").toList
     println("duree list3="+(System.nanoTime-deb))
     println("list=" +list)
     println("list2=" +list2)
     println("list3=" +list2)
     println("list.size="+list.size)
     println("list2.size="+list2.size)
     println("list3.size="+list3.size)
   }
  }

}