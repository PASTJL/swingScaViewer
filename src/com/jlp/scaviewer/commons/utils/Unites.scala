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
package com.jlp.scaviewer.commons.utils
import java.util.Properties
import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.io.IOException
import scala.collection.mutable.ArrayBuffer
import scala.math._
import language.postfixOps

object Unites {

  val unites = new Properties()
  var dir = System.getProperty("root") + File.separator + "config";
  var f = new File(dir + File.separator + "scaViewer.properties");

  try {
    unites.load(new FileInputStream(f));

  } catch {
    case e: FileNotFoundException =>
      // TODO Auto-generated catch block
      e.printStackTrace();
    case e: IOException =>

      // TODO Auto-generated catch block
      e.printStackTrace();
  }
  val listUnit = unites.get("scaviewer.basic.unit").asInstanceOf[String].split(";").toList
  val listUnitTime = unites.get("scaviewer.basic.unit.time").asInstanceOf[String].split(";").toList
  val listUnitMult = unites.get("scaviewer.unit.mult.list").asInstanceOf[String].split(";").toList
  val listTradTime: List[String] = {
    var buf: ArrayBuffer[String] = new ArrayBuffer[String](7)
    var en = unites.keys

    while (en.hasMoreElements()) {
      var str: String = en.nextElement.asInstanceOf[String]
      var idx = str.lastIndexOf(".")
      str = str.substring(idx + 1)
      if (str.contains("To"))
        buf += str
    }
    buf.toList
  }
  val pat = """\w+""".r
  val patBasicUnit = {
    var str = ""
    for (unite <- listUnit) {
      if (str.length > 0)
        str = str + """|^""" + unite
      else
        str = """^""" + unite

    }
    str.r
  }
  val patBasicUnitSuff = {
    var str = ""
    for (unite <- listUnit) {
      if (str.length > 0)
        str = str + """|""" + unite + """$"""
      else
        str = unite + """$"""

    }
    str.r
  }

  val patUnitTime = {
    var str = ""
    for (unite <- listUnitTime) {
      if (str.length > 0)
        str = str + """|^""" + unite
      else
        str = """^""" + unite

    }
    str.r
  }

  /**
   * return for DecimalUnit the unit without the suffix multiplicator
   *
   * returm "ms" for timeUnit
   */
  def returnBasicUnit(strUnit: String): String = {
    var ret = strUnit

    if (ret.contains("/") || ret.contains("*")) {

      if (ret.contains("/")) {
        ret = returnBasicUnit(strUnit.split("/")(0)) + "/" + returnBasicUnit(strUnit.split("/")(1))
      } else if (ret.contains("*")) {
        ret = returnBasicUnit(strUnit.split("\\*")(0)) + "*" + returnBasicUnit(strUnit.split("\\*")(1))
      }

    } else if (!isTimeUnit(strUnit)) {
      // chercher prefixe
      var i = 0

      var bool = true
      var w = strUnit
      while (bool && i < listUnitMult.length) {
        if (strUnit contains listUnitMult(i)) {
          w = strUnit.substring(listUnitMult(i).length)

          bool = false
        }
        i += 1
      }
      ret = w
    } else {

      ret = "ms"
    }

    ret

  }

  def isCorrectUnit(str: String): Boolean =
    {
      var ret = false
      val pat1 = """[a-zA-Z]+/*\**[a-zA-Z]*""".r

      pat1 findFirstIn str match {
        case Some(v) =>
          if (v contains "*") {
            var spl = v.split("\\*")
            ret = isCorrectUnit(spl(0)) && isCorrectUnit(spl(1))
          } else if (v contains "/") {

            var spl = v.split("/")
            ret = isCorrectUnit(spl(0)) && isCorrectUnit(spl(1))
          } else {
            // cas de l unite dec avec prefix mult => K,M,G,T,milli,micro
            if (isTimeUnit(str)) {
              ret = listUnitTime.contains(v)
            } else {
              // retirer les eventuels prefixe multiplicatif
              var i = 0

              var bool = true
              var w = v
              while (bool && i < listUnitMult.length) {
                if (v contains listUnitMult(i)) {
                  w = v.substring(listUnitMult(i).length)

                  bool = false
                }
                i += 1
              }
              ret = listUnit.contains(w)

            }
          }

        case None => ret = false
      }

      ret

    }
  def convert(source: String, target: String): (Double, String, String) =
    {
      var ret = (1d, "", "")
      // println("point 0 :" + source + " => " + target)
      if (source == target) {
        ret = (1.0d, source, target)
      } else {

        if (!source.contains("/") && !source.contains("*")) {
          patBasicUnit findFirstIn source match {
            case Some(v) =>
              // println("Basic unit v=" + v)
              ret = convertSimpleDec(source, target)
            case None =>
              patBasicUnit findFirstIn target match {
                case Some(v) if (!target.contains("/") && !target.contains("*")) =>
                  ret = convertSimpleDec(target, source)
                  ret = (1 / (ret _1), source, target)
                case None =>
              }
          }

          patUnitTime findFirstIn source match {

            case Some(v) =>
              //   println("Time Unit v=" + v)
              ret = convertSimpleTime(source, target)
            case None =>
          }

        }
        if (ret == (1d, "", "") && !target.contains("/") && !target.contains("*") && !source.contains("/") && !source.contains("*")) {
          // cas hybride genre Km <=> Mm
          // decouper en prefix
          // println(source + " => " + target)
          var suffSource = patBasicUnitSuff findFirstIn source get

          var ret1 = convert(source, suffSource)
          var ret2 = convert(suffSource, target)

          //        println("suffSource=" + suffSource)
          //
          //        println("ret1=" + ret1)
          //        println("ret2=" + ret2)
          ret = ((ret1 _1) * (ret2 _1), source, target)
        } else if (ret == (1d, "", "") && (target.contains("/") || target.contains("*") || source.contains("/") || source.contains("*"))) {
          // Complex modification
          // cas du by 
          if (source.contains("/")) {
            var sourceUnapply = (source.substring(0, source.indexOf("/")), "/", source.substring(source.indexOf("/") + 1))
            var targetUnapply = (target.substring(0, target.indexOf("/")), "/", target.substring(target.indexOf("/") + 1))

            //          println("sourceUnapply=" + sourceUnapply)
            //          println("targetUnapply=" + targetUnapply)
            var ret1 = convert(sourceUnapply _1, targetUnapply _1)
            var ret2 = convert(sourceUnapply _3, targetUnapply _3)
            ret = ((ret1 _1) / (ret2 _1), source, target)

          } else if (source.contains("*")) {
            var sourceUnapply = (source.substring(0, source.indexOf("*")), "*", source.substring(source.indexOf("*") + 1))
            var targetUnapply = (target.substring(0, target.indexOf("*")), "*", target.substring(target.indexOf("*") + 1))

            var ret1 = convert(sourceUnapply _1, targetUnapply _1)
            var ret2 = convert(sourceUnapply _3, targetUnapply _3)

            ret = ((ret1 _1) * (ret2 _1), source, target)

          }
        }
      }
      ret
    }

  def convertSimpleDec(source: String, target: String): (Double, String, String) =
    {
      var ret = (0d, "", "")

      source match {
        case pat() if (listUnit.contains(source)) => // conversion from basic unit if (listUnit.contains(unit))

          // println("unit=" + source)
          // extration du prefixe multiplicateur
          if (source == target) {
            ret = (1, source, target)
          } else if (unites.containsKey("scaviewer.unit.mult." + source + "To" + target)) {
            // println("traduction simple")
            ret = (unites.get("scaviewer.unit.mult." + source + "To" + target).asInstanceOf[String].toDouble, source, target)
          } else if (source != target) {
            val prefixe = target.substring(0, target.indexOf(source))

            val multiplicator: String = unites.get("scaviewer.unit.mult." + prefixe).asInstanceOf[String]
            //          println("scaviewer.unit.mult." + target)
            //          println("multiplicator=" + multiplicator)
            if (multiplicator.contains("/")) {
              val multi = 1 / (multiplicator.split("/")(1)).toDouble
              val unite = target

              ret = (multi, source, unite)
            } else {

              ret = (multiplicator.toDouble, source, target)
            }
          }

        case _ => {
          // println("other case")

        }

      }
      // println((tmpD, tmpS))
      ret
    }

  def convertSimpleTime(source: String, target: String): (Double, String, String) =
    {

      var ret = (0d, "", "")

      // retrouver la chaine des multiplicateurs
      // si traduction du plus petit vers plus grand

      if (listUnitTime.indexOf(source) < listUnitTime.indexOf(target)) {
        // rechercher la chaine unit"To"target
        var chaine = source + "To" + target
        if (listTradTime.contains(chaine)) {
          val multiplicator = unites.get("scaviewer.basic.unit.time." + chaine).asInstanceOf[String]

          ret = (1 / (multiplicator.split("/")(1)).toDouble, source, target)
        } else {
          //recherche de la chaine
          var idx1 = listUnitTime.indexOf(source)
          var idx2 = listUnitTime.indexOf(target)
          var mult = 1d
          // println("chaine normale")
          // println("idx1=" + idx1 + " ;idx2=" + idx2)
          var i = idx1
          while (i < idx2) {
            var j: Int = 0
            var multiplicator: String = null
            while (null == multiplicator) {
              j += 1
              multiplicator = unites.get("scaviewer.basic.unit.time." + listUnitTime(i) + "To" + listUnitTime(i + j)).asInstanceOf[String]
              // println("multiplicator=" + multiplicator)

            }
            mult = mult * 1 / multiplicator.split("/")(1).toDouble
            i = i + j
          }

          ret = (mult, source, target)
        }
      } else {

        var chaine = target + "To" + source
        if (listTradTime.contains(chaine)) {
          val multiplicator = unites.get("scaviewer.basic.unit.time." + chaine).asInstanceOf[String]

          ret = ((multiplicator.split("/")(1)).toDouble, source, target)
        } else {
          // println("chaine invers�e")
          //recherche de la chaine
          var idx1 = listUnitTime.indexOf(source)
          var idx2 = listUnitTime.indexOf(target)
          //println("idx1="+idx1+" ;idx2="+idx2)
          var mult = 1d
          var i = idx2
          while (i < idx1) {
            var multiplicator: String = null
            var j: Int = 0
            while (null == multiplicator) {

              j += 1
              //   println("source=" + source + " ;target=" + target)
              multiplicator = unites.get("scaviewer.basic.unit.time." + listUnitTime(i) + "To" + listUnitTime(i + j)).asInstanceOf[String]
              //println("multiplicator=" + multiplicator)

            }

            mult = mult * multiplicator.split("/")(1).toDouble
            i = i + j
          }
          ret = (mult, source, target)

        }
      }

      // println((tmpD, tmpS))
      ret
    }

  def isTimeUnit(str: String): Boolean =
    {
      //println("isTimeUnit => str=" + str + " => " + (listUnitTime exists (str.contains(_))))
      listUnitTime exists (str.contains(_))
    }

  def bestConversion(valuePassed: Double, valueReferenceMax: Long, valueReferenceMin: Long, unite: String): (Double, String, String) =
    {
      var ret = (1d, "", "")
     // println("conversion pour ValuePassed=" + valuePassed + " unite=" + unite)
      // cas ou on est d�ja dans les clous
     // println(" valueReferenceMax.abs=" + valueReferenceMax.abs)
      //println("  valueReferenceMin.abs=" + valueReferenceMin.abs)
      if (valuePassed.toLong.abs <= valueReferenceMax.abs && valuePassed.toLong.abs > valueReferenceMin.abs) {
        ret = (1.0, unite, unite)
        //println("pas de conversion pour ValuePassed=" + valuePassed + " unite=" + unite)
        ret
      } else if (unite.contains("/")) {
        //println("conversion pour ValuePassed=" + valuePassed + " unite=" + unite + " with a / ")
        var numSource = unite.split("/")(0)
        var denumSource = unite.split("/")(1)
        isTimeUnit(numSource) match {
          case true =>
            {
              isTimeUnit(denumSource) match {

                case true =>
                  {
                    // println(" // cas num et denum time")
                    var idx1Num = listUnitTime.indexOf(numSource)
                    var idx1Denum = listUnitTime.indexOf(denumSource)
                    var i = idx1Num
                    var j = idx1Denum
                    var mult = 1d
                    var mult2 = 1d
                    var bool = true
                    var bool2 = true
                    if (valuePassed.abs > valueReferenceMax.abs) {
                      //  println(" // cas num et denum time max depasse")
                      while (bool || bool2) {

                        var ret1 = ret

                        if (i < listUnitTime.length - 1) {
                          ret1 = convert(numSource, listUnitTime(i))

                          mult = (ret1 _1)
                          if (valuePassed.abs * mult / mult2 < valueReferenceMax.abs && bool) {
                            bool = false
                            bool2 = false
                          } else {
                            if (i < listUnitTime.length - 1) {
                              i = i + 1
                              ret1 = convert(numSource, listUnitTime(i))

                              mult = (ret1 _1)

                            }
                          }
                        } else {
                          i = idx1Num
                          if (j > 0) {
                            j = j - 1
                            var ret2 = convert(denumSource, listUnitTime(j))

                            mult2 = (ret2 _1)
                          }
                        }

                      }
                      //  println("value finale=" + mult * valuePassed / mult2)
                      ret = (mult / mult2, unite, listUnitTime(min(i, listUnitTime.length - 1)) + "/" + listUnitTime(max(j, 0)))

                    } else if (valuePassed.abs.toLong <= valueReferenceMin.abs) {
                      //toDo

                      // println("// cas num et denum time   minima a/b non atteint")
                      bool = true
                      bool2 = true
                      while (bool || bool2) {

                        var ret1 = ret

                        if ((valuePassed.abs * mult / mult2).toLong > valueReferenceMin.abs && bool) {
                          bool = false
                          bool2 = false
                        } else {

                          while (bool2) {
                            if (j < listUnitTime.length - 1) {

                              var ret2 = convert(denumSource, listUnitTime(j))

                              mult2 = (ret2 _1)
//                              println("mult=" + mult + " mult2 =" + mult2)
//                              println("source=" + (ret2 _2) + " target=" + (ret2 _3))
//                              println("numSource=" + numSource)
                              if ((valuePassed.abs * mult / mult2).toLong > valueReferenceMin.abs && bool2) {

                                bool2 = false
                                bool = false
                              } else {
                                j += 1
                              }
                            } else {
                              bool2 = false
                              j = idx1Denum;
                            }

                          }

                        }

                        if (i > 0 && (bool || bool2)) {
                          ret1 = convert(numSource, listUnitTime(i))

                          mult = (ret1 _1)
                          i -= 1
                        } else {
                          bool = false
                        }

                      }
                      //  println("minima value finale=" + mult * valuePassed / mult2)
                      ret = (mult / mult2, unite, listUnitTime(max(i, 0)) + "/" + listUnitTime(min(j, listUnitTime.length - 1)))

                    }
                  }
                case false =>
                  {
                    // println(" // cas num time  et denum unit dec")

                    var idx1Num = listUnitTime.indexOf(numSource)

                    var suffDenum = patBasicUnitSuff findFirstIn denumSource get
                    var prefDenum = ""
                    var idx1Denum = listUnitMult.indexOf("\"\"")
                    if (suffDenum.length != denumSource.length) {
                      prefDenum = denumSource.substring(0, denumSource.indexOf(suffDenum))
                      idx1Denum = listUnitMult.indexOf(prefDenum)
                    }

                    var i = idx1Num
                    var j = idx1Denum
                    var mult = 1d
                    var mult2 = 1d
                    var bool = true
                    var bool2 = true
                    if (valuePassed.abs.toLong > valueReferenceMax.abs) {

                      while (bool || bool2) {

                        var ret1 = ret

                        if (i < listUnitTime.length - 1) {
                          ret1 = convert(numSource, listUnitTime(i))

                          mult = (ret1 _1)
                          if (valuePassed.abs * mult / mult2 < valueReferenceMax.abs && bool) {
                            bool = false
                            bool2 = false
                          } else {
                            if (i < listUnitTime.length - 1) {
                              i = i + 1
                              ret1 = convert(numSource, listUnitTime(i))

                              mult = (ret1 _1)

                            }
                          }
                        } else {
                          i = idx1Num
                          if (j > 0) {
                            j = j - 1
                            var ret2 = convert(denumSource, listUnitMult(j)+suffDenum)

                            mult2 = (ret2 _1)
                          }
                        }

                      }
                      var pref = ""
                      if (listUnitMult(max(j, 0)) != "\"\"") {
                        pref = listUnitMult(max(j, 0))
                      }
                      ret = (mult / mult2, unite, listUnitTime(min(i, listUnitTime.length - 1)) + "/" + pref + suffDenum)
                    } else if (valuePassed.abs.toLong <= valueReferenceMin.abs) {

                      bool = true
                      bool2 = true
                      while (bool || bool2) {

                        var ret1 = ret

                        if ((valuePassed.abs * mult / mult2).toLong > valueReferenceMin.abs && bool) {
                          bool = false
                          bool2 = false
                        } else {

                          while (bool2) {
                            if (j < listUnitMult.length - 1) {

                              var ret2 = convert(denumSource, listUnitMult(j)+suffDenum)

                              mult2 = (ret2 _1)

                              if ((valuePassed.abs * mult / mult2).toLong > valueReferenceMin.abs && bool2) {

                                bool2 = false
                                bool = false
                              } else {
                                j += 1
                              }
                            } else {
                              bool2 = false
                              j = idx1Denum;
                            }

                          }

                        }

                        if (i > 0 && (bool || bool2)) {
                          ret1 = convert(numSource, listUnitTime(i))

                          mult = (ret1 _1)
                          i -= 1
                        } else {
                          bool = false
                        }

                      }
                      var pref = ""
                      if (listUnitMult(min(j, listUnitMult.length - 1)) != "\"\"") {
                        pref = listUnitMult(min(j, listUnitMult.length - 1))
                      }
                      ret = (mult / mult2, unite, listUnitTime(max(i, 0)) + "/" + pref + suffDenum)

                    }
                  }
              }
            }
          case false =>
            // cas numSource Dec 
            //            println("numSource dec et /")
            //            println("numSource dec =>" + numSource + " denumSource => " + denumSource + " et sep = / ")
            isTimeUnit(denumSource) match {
              case true =>

                var idx2Denum = listUnitTime.indexOf(denumSource)
                var suffNum = patBasicUnitSuff findFirstIn numSource get
                var prefNum = ""

                var idx1Num = listUnitMult.indexOf("\"\"")
                if (suffNum.length != numSource.length) {
                  prefNum = numSource.substring(0, numSource.indexOf(suffNum))
                  idx1Num = listUnitMult.indexOf(prefNum)
                }

                var i = idx1Num
                var j = idx2Denum
                var mult = 1d
                var mult2 = 1d
                var bool = true
                var bool2 = true

                if (valuePassed.abs.toLong > valueReferenceMax.abs) {
                  while (bool || bool2) {
                    var ret1 = ret

                    if (i < listUnitMult.length - 1) {
                      ret1 = convert(numSource, listUnitMult(i) + suffNum)

                      mult = (ret1 _1)
                      if (valuePassed.abs * mult / mult2 < valueReferenceMax.abs && bool) {
                        bool = false
                        bool2 = false
                      } else {
                        if (i < listUnitMult.length - 1) {
                          i = i + 1
                          ret1 = convert(numSource, listUnitMult(i) + suffNum)

                          mult = (ret1 _1)

                        }
                      }
                    } else {
                      i = idx1Num
                      if (j > 0) {
                        j = j - 1
                        var ret2 = convert(denumSource, listUnitTime(j))

                        mult2 = (ret2 _1)
                      }
                    }

                  }

                  var pref = ""
                  if (listUnitMult(min(i, listUnitMult.length - 1)) != "\"\"") {
                    pref = listUnitMult(min(i, listUnitMult.length - 1))
                  }
                  ret = (mult / mult2, unite, pref + suffNum + "/" + listUnitTime(max(j, 0)))

                } else if (valuePassed.abs.toLong <= valueReferenceMin.abs) {
                  bool = true;
                  bool2 = true
                  while (bool || bool2) {
                    var ret1 = ret

                    if ((valuePassed.abs * mult / mult2).toLong > valueReferenceMin.abs && bool) {
                      bool = false
                      bool2 = false
                    } else {

                      while (bool2) {
                        if (j < listUnitTime.length - 1) {

                          var ret2 = convert(denumSource, listUnitTime(j))

                          mult2 = (ret2 _1)

                          if ((valuePassed.abs * mult / mult2).toLong > valueReferenceMin.abs && bool2) {

                            bool2 = false
                            bool = false
                          } else {
                            j += 1
                          }
                        } else {
                          bool2 = false
                          j = idx2Denum;
                        }

                      }

                    }

                    if (i > 0 && (bool || bool2)) {
                      ret1 = convert(numSource, listUnitMult(i) + suffNum)

                      mult = (ret1 _1)
                      i -= 1
                    } else {
                      bool = false
                    }

                  }
                  var pref = ""
                  if (listUnitMult(max(i, 0)) != "\"\"") {
                    pref = listUnitMult(max(i, 0))
                  }

                  ret = (mult / mult2, unite, pref + suffNum + "/" + listUnitTime(min(j, listUnitTime.length - 1)))

                }
              case false =>
                // println("numSource dec denumSource dec ")

                var suffDenum = patBasicUnitSuff findFirstIn denumSource get
                var prefDenum = ""
                var idxDenum = listUnitMult.indexOf("\"\"")
                if (suffDenum.length != denumSource.length) {
                  prefDenum = denumSource.substring(0, denumSource.indexOf(suffDenum))
                  idxDenum = listUnitMult.indexOf(prefDenum)
                }

                var suffNum = patBasicUnitSuff findFirstIn numSource get
                var prefNum = ""
                var idxNum = listUnitMult.indexOf("\"\"")
                if (suffNum.length != numSource.length) {
                  prefNum = numSource.substring(0, numSource.indexOf(suffNum))
                  idxNum = listUnitMult.indexOf(prefNum)
                }
                var i = idxNum
                var j = idxDenum
                var mult = 1d
                var mult2 = 1d
                var bool = true
                var bool2 = true

                if (valuePassed.abs.toLong > valueReferenceMax.abs) {

                  while (bool || bool2) {

                    var ret1 = ret

                    if (i < listUnitMult.length - 1) {
                      ret1 = convert(numSource, listUnitMult(i) + suffNum)

                      mult = (ret1 _1)
                      if (valuePassed.abs * mult / mult2 < valueReferenceMax.abs && bool) {
                        bool = false
                        bool2 = false
                      } else {
                        if (i < listUnitMult.length - 1) {
                          i = i + 1
                          ret1 = convert(numSource, listUnitMult(i) + suffNum)

                          mult = (ret1 _1)

                        }
                      }
                    } else {
                      i = idxNum
                      if (j > 0) {
                        j = j - 1
                        var ret2 = convert(denumSource, listUnitMult(j)+suffDenum)

                        mult2 = (ret2 _1)
                      }
                    }

                  }
                  var prefNum = ""
                  if (listUnitMult(min(i, listUnitMult.length - 1)) != "\"\"") {
                    prefNum = listUnitMult(min(i, listUnitMult.length - 1))
                  }
                  var prefDenum = ""
                  if (listUnitMult(max(j, 0)) != "\"\"") {
                    prefDenum = listUnitMult(max(j, 0))
                  }
                  ret = (mult / mult2, unite, prefNum + suffNum + "/" + prefDenum + suffDenum)

                } else if (valuePassed.abs.toLong <= valueReferenceMin.abs) {
                  bool = true;
                  bool2 = true
                  while (bool || bool2) {

                    var ret1 = ret

                    if ((valuePassed.abs * mult / mult2).toLong > valueReferenceMin.abs && bool) {
                      bool = false
                      bool2 = false
                    } else {

                      while (bool2) {
                        if (j < listUnitMult.length - 1) {

                          var ret2 = convert(denumSource, listUnitMult(j)+suffDenum)

                          mult2 = (ret2 _1)

                          if ((valuePassed.abs * mult / mult2).toLong > valueReferenceMin.abs && bool2) {

                            bool2 = false
                            bool = false
                          } else {
                            j += 1
                          }
                        } else {
                          bool2 = false
                          j = idxDenum;
                        }

                      }

                    }

                    if (i > 0 && (bool || bool2)) {
                      ret1 = convert(numSource, listUnitMult(i) + suffNum)

                      mult = (ret1 _1)
                      i -= 1
                    } else {
                      bool = false
                    }

                  }
                  var prefNum = ""
                  if (listUnitMult(max(i, 0)) != "\"\"") {
                    prefNum = listUnitMult(max(i, 0))
                  }
                  var prefDenum = ""
                  if (listUnitMult(min(j, listUnitMult.length - 1)) != "\"\"") {
                    prefDenum = listUnitMult(min(j, listUnitMult.length - 1))
                  }
                  ret = (mult / mult2, unite, prefNum + suffNum + "/" + prefDenum + suffDenum)

                }
            }
        }

      } else if (unite.contains("*")) {
        // println("conversion pour ValuePassed="+valuePassed+" unite="+unite)
        //println("traitement *")
        var numSource = unite.split("\\*")(0)
        var denumSource = unite.split("\\*")(1)
        isTimeUnit(numSource) match {
          case true =>
            isTimeUnit(denumSource) match {

              case true =>
                {
                  // println(" // cas num et denum time")
                  var idx1Num = listUnitTime.indexOf(numSource)
                  var idx1Denum = listUnitTime.indexOf(denumSource)
                  var i = idx1Num
                  var j = idx1Denum
                  var mult = 1d
                  var mult2 = 1d
                  var bool = true
                  var bool2 = true
                  if (valuePassed.abs > valueReferenceMax.abs) {
                    // println(" // cas num et denum time max depasse")
                    while (bool || bool2) {

                      var ret1 = ret

                      if (i < listUnitTime.length - 1) {
                        ret1 = convert(numSource, listUnitTime(i))

                        mult = (ret1 _1)
                        if (valuePassed.abs * mult * mult2 < valueReferenceMax.abs && bool) {
                          bool = false
                          bool2 = false
                        } else {
                          if (i < listUnitTime.length - 1) {
                            i = i + 1
                            ret1 = convert(numSource, listUnitTime(i))

                            mult = (ret1 _1)

                          }
                        }
                      } else {
                        i = idx1Num
                        if (j > 0) {
                          j = j - 1
                          var ret2 = convert(denumSource, listUnitTime(j))

                          mult2 = (ret2 _1)
                        }
                      }

                    }
                    // println("value finale=" + mult * valuePassed / mult2)
                    ret = ((mult * mult2), unite, listUnitTime(min(i, listUnitTime.length - 1)) + "*" + listUnitTime(min(j, listUnitTime.length - 1)))

                  } else if (valuePassed.abs.toLong <= valueReferenceMin.abs) {
                    //toDo

                    // println("// cas num et denum time   minima a/b non atteint")

                    while (bool || bool2) {

                      var ret1 = ret

                      if ((valuePassed.abs * mult * mult2).toLong > valueReferenceMin.abs && bool) {
                        bool = false
                        bool2 = false
                      } else {

                        while (bool2) {
                          if (j < listUnitTime.length - 1) {

                            var ret2 = convert(denumSource, listUnitTime(j))

                            mult2 = (ret2 _1)

                            if ((valuePassed.abs * mult * mult2).toLong > valueReferenceMin.abs && bool2) {

                              bool2 = false
                              bool = false
                            } else {
                              j += 1
                            }
                          } else {
                            bool2 = false
                            j = idx1Denum;
                          }

                        }

                      }

                      if (i > 0 && (bool || bool2)) {
                        ret1 = convert(numSource, listUnitTime(i))

                        mult = (ret1 _1)
                        i -= 1
                      } else {
                        bool = false
                      }

                    }
                    //println("minima value finale=" + mult * valuePassed / mult2)
                    ret = (mult * mult2, unite, listUnitTime(max(i, 0)) + "*" + listUnitTime(max(j, 0)))

                  }
                }
              case false =>
                {
                  //println(" // cas num time  et denum unit dec")

                  var idx1Num = listUnitTime.indexOf(numSource)

                  var suffDenum = patBasicUnitSuff findFirstIn denumSource get
                  var prefDenum = ""
                  var idx1Denum = listUnitMult.indexOf("\"\"")
                  if (suffDenum.length != denumSource.length) {
                    prefDenum = denumSource.substring(0, denumSource.indexOf(suffDenum))
                    idx1Denum = listUnitMult.indexOf(prefDenum)
                  }

                  var i = idx1Num
                  var j = idx1Denum
                  var mult = 1d
                  var mult2 = 1d
                  var bool = true
                  var bool2 = true
                  if (valuePassed.abs.toLong > valueReferenceMax.abs) {

                    while (bool || bool2) {

                      var ret1 = ret

                      if (i < listUnitTime.length - 1) {
                        ret1 = convert(numSource, listUnitTime(i))

                        mult = (ret1 _1)
                        if (valuePassed.abs * mult * mult2 < valueReferenceMax.abs && bool) {
                          bool = false
                          bool2 = false
                        } else {
                          if (i < listUnitTime.length - 1) {
                            i = i + 1
                            ret1 = convert(numSource, listUnitTime(i))

                            mult = (ret1 _1)

                          }
                        }
                      } else {
                        i = idx1Num
                        if (j > 0) {
                          j = j - 1
                          var ret2 = convert(denumSource, listUnitMult(j)+suffDenum)

                          mult2 = (ret2 _1)
                        }
                      }

                    }
                    var pref = ""
                    if (listUnitMult(min(j, listUnitMult.length - 1)) != "\"\"") {
                      pref = listUnitMult(min(j, listUnitMult.length - 1))
                    }
                    ret = (mult * mult2, unite, listUnitTime(min(i, listUnitTime.length - 1)) + "*" + pref + suffDenum)

                  } else if (valuePassed.abs.toLong <= valueReferenceMin.abs) {

                    while (bool || bool2) {

                      var ret1 = ret

                      if ((valuePassed.abs * mult * mult2).toLong > valueReferenceMin.abs && bool) {
                        bool = false
                        bool2 = false
                      } else {

                        while (bool2) {
                          if (j < listUnitMult.length - 1) {

                            var ret2 = convert(denumSource, listUnitMult(j)+suffDenum)

                            mult2 = (ret2 _1)

                            if ((valuePassed.abs * mult * mult2).toLong > valueReferenceMin.abs && bool2) {

                              bool2 = false
                              bool = false
                            } else {
                              j += 1
                            }
                          } else {
                            bool2 = false
                            j = idx1Denum;
                          }

                        }

                      }

                      if (i > 0 && (bool || bool2)) {
                        ret1 = convert(numSource, listUnitTime(i))

                        mult = (ret1 _1)
                        i -= 1
                      } else {
                        bool = false
                      }

                    }
                    var pref = ""
                    if (listUnitMult(max(j, 0)) != "\"\"") {
                      pref = listUnitMult(max(j, 0))
                    }
                    ret = (mult * mult2, unite, listUnitTime(max(i, 0)) + "*" + pref + suffDenum)

                  }
                }
            }
          case false =>
            {

              isTimeUnit(denumSource) match {
                case true =>
                  // println("numSource Time denumSource Dec ")

                  var ret1 = bestConversion(valuePassed, 1000, 0, denumSource + "*" + numSource)
                  // println("ret initial ="+ret1)
                  ret = (ret1 _1, unite, (ret1 _3).split("\\*")(1) + "*" + (ret1 _3).split("\\*")(0))
                //  println("ret final ="+ret)

                case false =>
                  // println("numSource dec denumSource dec ")

                  var suffDenum = patBasicUnitSuff findFirstIn denumSource get
                  var prefDenum = ""
                  var idxDenum = listUnitMult.indexOf("\"\"")
                  if (suffDenum.length != denumSource.length) {
                    prefDenum = denumSource.substring(0, denumSource.indexOf(suffDenum))
                    idxDenum = listUnitMult.indexOf(prefDenum)
                  }

                  var suffNum = patBasicUnitSuff findFirstIn numSource get
                  var prefNum = ""
                  var idxNum = listUnitMult.indexOf("\"\"")
                  if (suffNum.length != numSource.length) {
                    prefNum = numSource.substring(0, numSource.indexOf(suffNum))
                    idxNum = listUnitMult.indexOf(prefNum)
                  }
                  var i = idxNum
                  var j = idxDenum
                  var mult = 1d
                  var mult2 = 1d
                  var bool = true
                  var bool2 = true

                  if (valuePassed.abs.toLong > valueReferenceMax.abs) {

                    while (bool || bool2) {

                      var ret1 = ret

                      if (i < listUnitMult.length - 1) {
                        ret1 = convert(numSource, listUnitMult(i) + suffNum)

                        mult = (ret1 _1)
                        if (valuePassed.abs * mult * mult2 < valueReferenceMax.abs && bool) {
                          bool = false
                          bool2 = false
                        } else {
                          if (i < listUnitMult.length - 1) {
                            i = i + 1
                            ret1 = convert(numSource, listUnitMult(i) + suffNum)

                            mult = (ret1 _1)

                          }
                        }
                      } else {
                        i = idxNum
                        if (j > 0) {
                          j = j - 1
                          var ret2 = convert(denumSource, listUnitMult(j + 1) + suffDenum)

                          mult2 = (ret2 _1)
                        }
                      }

                    }
                    var prefNum = ""
                    if (listUnitMult(min(i, listUnitMult.length - 1)) != "\"\"") {
                      prefNum = listUnitMult(min(i, listUnitMult.length - 1))
                    }
                    var prefDenum = ""
                    if (listUnitMult(min(j, listUnitMult.length - 1)) != "\"\"") {
                      prefDenum = listUnitMult(min(j, listUnitMult.length - 1))
                    }
                    ret = (mult * mult2, unite, prefNum + suffNum + "*" + prefDenum + suffDenum)
                    //JLP
                  } else if (valuePassed.abs.toLong <= valueReferenceMin.abs) {

                    while (bool || bool2) {

                      var ret1 = ret

                      if ((valuePassed.abs * mult * mult2).toLong > valueReferenceMin.abs && bool) {
                        bool = false
                        bool2 = false
                      } else {

                        while (bool2) {
                          if (j < listUnitMult.length - 1) {

                            var ret2 = convert(denumSource, listUnitMult(j) + suffDenum)

                            mult2 = (ret2 _1)

                            if ((valuePassed.abs * mult * mult2).toLong > valueReferenceMin.abs && bool2) {

                              bool2 = false
                              bool = false
                            } else {
                              j += 1
                            }
                          } else {
                            bool2 = false
                            j = idxDenum;
                          }

                        }

                      }

                      if (i > 0 && (bool || bool2)) {
                        ret1 = convert(numSource, listUnitMult(i) + suffNum)

                        mult = (ret1 _1)
                        i -= 1
                      } else {
                        bool = false
                      }

                    }
                    var prefNum = ""
                    if (listUnitMult(max(i, 0)) != "\"\"") {
                      prefNum = listUnitMult(max(i, 0))
                    }
                    var prefDenum = ""
                    if (listUnitMult(max(j, 0)) != "\"\"") {
                      prefDenum = listUnitMult(max(j, 0))
                    }
                    ret = (mult * mult2, unite, prefNum + suffNum + "*" + prefDenum + suffDenum)

                  }
              }
            }
        }

      } else if (isTimeUnit(unite)) {
        // println("conversion pour ValuePassed="+valuePassed+" unite="+unite)
        // traitement unite simple
        var index = listUnitTime.indexOf(unite)
        var bool = true
        var i = index
        var mult = 1d
        if (valuePassed.toLong.abs > valueReferenceMax.abs) {
          while (bool) {
            if (i < listUnitTime.length - 1) {
              var multiplicator = unites.get("scaviewer.basic.unit.time." + listUnitTime(i) + "To" + listUnitTime(i + 1)).asInstanceOf[String]
              mult = mult * multiplicator.split("/")(1).toDouble
              if (valuePassed.toLong.abs / mult < valueReferenceMax.abs) {
                bool = false
                ret = (1 / mult, unite, listUnitTime(i + 1))

              }
            } else {
              bool = false
              // println("i=" + i)
              ret = (1 / mult, unite, listUnitTime(i))
            }
            i += 1
          }
        } else if (valuePassed.toLong.abs <= valueReferenceMin.abs) {
          // println("passage Minimum non atteint")
          while (bool) {
            if (i > 0) {
              var multiplicator = unites.get("scaviewer.basic.unit.time." + listUnitTime(i - 1) + "To" + listUnitTime(i)).asInstanceOf[String]

              mult = mult / multiplicator.split("/")(1).toDouble

              if ((valuePassed.abs / mult).toLong > valueReferenceMin.abs) {
                bool = false
                ret = (1 / mult, unite, listUnitTime(i - 1))

              }
            } else {
              bool = false
              //println("i=" + i)
              ret = (1 / mult, unite, listUnitTime(i))

            }
            i -= 1
          }

        }
      } else {
        //  println("conversion pour ValuePassed="+valuePassed+" unite="+unite)
        // simple Dec
        val suffSource = patBasicUnitSuff findFirstIn unite get
        var prefSource = ""

        // println("suffSource=" + suffSource)
        var idx: Int = 2
        var mult = 1
        if (suffSource.length != unite.length) {
          prefSource = unite.substring(0, unite.indexOf(suffSource))
          idx = listUnitMult.indexOf(prefSource)
        }

        if (valuePassed.toLong.abs > valueReferenceMax.abs) {

          var bool = true

          while (bool) {

            if (idx < listUnitMult.length - 1) {
              ret = convert(unite, listUnitMult(idx) + suffSource)
              //var multiplicator = unites.get("scaviewer.basic.unit.time." + listUnitTime(idx - 1) + "To" + listUnitTime(idx)).asInstanceOf[String]

              //              println("ret=" + ret)
              //              println((ret _1) * valuePassed.toLong)
              //              println(valueReferenceMax.abs)
              if ((ret _1) * valuePassed.toLong < valueReferenceMax.abs) {

                bool = false

              }
            } else {
              bool = false
              //ret = convert(unite, listUnitMult(idx) + suffSource)
            }
            idx += 1
          }
          var pref = ""
          if (listUnitMult(max(idx - 1, 0)) != "\"\"") {
            pref = listUnitMult(max(idx - 1, 0))
          }
          ret = convert(unite, pref + suffSource)

        } else if (valuePassed.toLong.abs <= valueReferenceMin.abs) {
          //  println("passage dec minimal")
          var bool = true
          while (bool) {
            if (idx > 0) {
              ret = convert(unite, listUnitMult(idx) + suffSource)
              // println(valuePassed * (ret _1))

              if ((valuePassed * (ret _1)).toLong > valueReferenceMin.abs) {
                bool = false
                ret = ((ret _1), unite, ret _3)
              }
            } else {
              bool = false
              ret = convert(unite, listUnitMult(idx) + suffSource)
              ret = ((ret _1), unite, ret _3)
            }
            idx -= 1
          }
          var pref = ""
          if (listUnitMult(max(idx + 1, 0)) != "\"\"") {
            pref = listUnitMult(max(idx + 1, 0))
          }
          //  ret = convert(unite, pref + suffSource)

        }
      }

      ret
    }

  def main(arr: Array[String]) {
    var unite = this
    println(unite.listUnit)
    println("unite.listUnitTime=" + unite.listUnitTime)
    println("listUnitMult=" + unite.listUnitMult)
    println(unite.listTradTime)
    //     println(   unite.convert( "mt", "Kmt"))
    //      println(    unite.convert( "H", "millis"))
    //        println(  unite.convert( "millis", "H"))
    //     println(unite.convert("Kmetre", "metre"))
    //        println(unite.convert("mn", "millis"))
    //       println(unite.convert("microoctet", "millioctet"))
    //   println(unite.convert("s", "H"))
    // println(unite.bestConversion(100000,1000,0,"s"))
    //   println(unite.bestConversion(0.000100000,1000,0,"s"))
    // println(unite.bestConversion(100000,1000,0,"mt"))
    // println(unite.bestConversion(0.000100000, 1000, 0, "mt"))
    var valuePassed = 100000d
    var ret = unite.bestConversion(valuePassed, 1000, 0, "req/s")
    println(ret)
    println("value=" + valuePassed + " value transformed=" + (ret _1) * valuePassed)
    //println(unite.convert( "s/ms","H/millis"))
    //    println(unite.convert("s/Goctet", "s/octet"))
    //    println(unite.convert("s*Goctet", "s*octet"))
    //    println(unite.convert("s*Goctet", "ms*octet"))
    //    println(unite.convert("req/s", "req/ms"))
  }
}