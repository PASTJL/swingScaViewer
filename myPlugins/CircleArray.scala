
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

/**
 * This class is not thread-safe for performance.
 *  Each thread must handle only one instance of this class
 *  Be care when using it in multi-threaded context
 *  The datas are a tuple ( Long, Double) where
 *  the key Long must be naturally sorted => ex Timestamps
 */
import language._

case class CircleArray(var taille: Int) {
  if(taille <2) taille=2
  val array: Array[(Long, Double)] = Array.ofDim(taille)
  for (i <- 0 until taille) {
    array(i) = (0, 0.0)
  }
  var pos = 0
  var sum: Double = 0
  var throughput: Double = 0
  var avg: Double = 0
  var max: Double = 0
  var freq: Double = 0
  var period: Double = 0
  def put(tup: (Long, Double)) {
    array(pos) = ((tup _1, tup _2))
    fill
    pos = (pos + 1) % taille
  }

  def fill(): Unit = {
    sum = array.foldLeft(0: Double)(_ + _._2)
    avg = sum.toDouble / taille.toDouble
    val diffAbs = (array(pos)._1) - array((pos + 1) % taille)._1
    max = 0
    // a reactiver si besoin
    // array foreach ( tup => if (tup ._2 > max ) max=tup._2)
    if (diffAbs == 0) {
      throughput = 0
      freq = 0
      period = 0
    } else {
      throughput = sum / diffAbs.toDouble
      //Frequence in hertz 1/ms in (taille -1) intervals 
       period = diffAbs.toDouble / ( taille -1).toDouble
       freq =  1.toDouble/ period
     

    }
  }
}
object CircleArray {
  def main(args: Array[String]) {
    val crcarr = new CircleArray(args(0).toInt)
    val deb = System.currentTimeMillis()
    for (i <- 0 until args(1).toInt) {
      crcarr.put((i, 20 * i))
    }
    println("array=")
    for (tup <- crcarr.array) {
      println(tup._2)
    }
    println("crcarr.sum =" + crcarr.sum)
    println("crcarr.slope =" + crcarr.throughput)
    println("duration =" + (System.currentTimeMillis() - deb))
  }
}
