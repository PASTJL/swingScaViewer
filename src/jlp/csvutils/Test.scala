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
package jlp.csvutils

class Test
{
  def coucou(lg:Long)={
    Thread.sleep(lg.toLong)
    println("coucou")}
}
object Test {
  def main(args: Array[String]): Unit={
  println("hello")
  new Test().coucou(args(0).toLong)
  }
  

}