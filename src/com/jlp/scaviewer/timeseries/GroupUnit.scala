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
package com.jlp.scaviewer.timeseries
import com.jlp.scaviewer.commons.utils.Unites

case class GroupUnit(val name: String)
{
override def toString()={ name}  
}

object GroupUnit {
  def retrouverGroup(str: String): GroupUnit =
    {
      var ret:GroupUnit = null
      val pat1 = """[a-zA-Z]+/[a-zA-Z]+""".r
      pat1 findFirstIn str match {

        case Some(v) =>
          {
            val arr = str.split("/")
            if (Unites.isCorrectUnit(arr(0)) && Unites.isCorrectUnit(arr(0))) {
              if (Unites.isTimeUnit(arr(0))) {
                if (Unites.isTimeUnit(arr(1))) {
                  ret = GroupUnit("timeUnit/timeUnit")
                } else {
                  ret = GroupUnit("timeUnit/" + Unites.returnBasicUnit(arr(1)))
                }

              } else {

                if (Unites.isTimeUnit(arr(1))) {
                  ret = GroupUnit(Unites.returnBasicUnit(arr(0)) + "/timeUnit")
                } else {
                  ret = GroupUnit(Unites.returnBasicUnit(arr(0)) + "/" + Unites.returnBasicUnit(arr(1)))
                }
              }
            } else {
              ret = null
            }

          }

        case None =>
          {
            val pat2 = """[a-zA-Z]+\*[a-zA-Z]+""".r
            pat2 findFirstIn str match {

              case Some(v) =>
                {

                  val arr = str.split("\\*")
                  if (Unites.isCorrectUnit(arr(0)) && Unites.isCorrectUnit(arr(0))) {
                    if (Unites.isTimeUnit(arr(0))) {
                      if (Unites.isTimeUnit(arr(1))) {
                        ret = GroupUnit("timeUnit*timeUnit")
                      } else {
                        ret = GroupUnit("timeUnit*" + Unites.returnBasicUnit(arr(1)))
                      }

                    } else {

                      if (Unites.isTimeUnit(arr(1))) {
                        ret = GroupUnit(Unites.returnBasicUnit(arr(0)) + "*timeUnit")
                      } else {
                        ret = GroupUnit(Unites.returnBasicUnit(arr(0)) + "*" + Unites.returnBasicUnit(arr(1)))
                      }
                    }
                  } else {
                    ret = null
                  }
                }

              case None =>
                if (Unites.isCorrectUnit(str)) {
                  if(Unites.isTimeUnit(str)) {
                     ret = GroupUnit("timeUnit")
                  }
                  else
                  {
                    ret=GroupUnit(Unites.returnBasicUnit(str))
                  }

                } else {
                  ret = null
                }

            }

          }

      }
      ret
    }

  def main(arr:Array[String]){
    
    var name="ms"
    println( retrouverGroup(name))
    
  }
}