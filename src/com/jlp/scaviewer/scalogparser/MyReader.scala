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
import java.io.BufferedReader
import java.io.FileReader
import java.io.File
import java.io.FileWriter
import java.util.regex.Pattern
import scala.util.matching.Regex

sealed trait MyReader {
  def read(relicat: String, bufReader: BufferedReader): (String, String)
  def readWith2Filter(relicat: String, bufReader: BufferedReader, filtIncl: String, filtExcl: String): (String, String) =
    {
      var bool = true
      var ret = (relicat, "")
      while (bool) {
        ret = read(ret._1, bufReader)
        if (ret._2 == "") bool = false
        else {
          if (filtIncl.r.findFirstIn(ret._2) != None && filtExcl.r.findFirstIn(ret._2) == None) {
            bool = false
          }
        }
      }
      ret
    }
  def readWithInclFilter(relicat: String, bufReader: BufferedReader, filtIncl: String): (String, String) =
    {
      var bool = true
      var ret = (relicat, "")
      while (bool) {
        ret = read(ret._1, bufReader)
        if (ret._2 == "") bool = false
        else {
          if (None != filtIncl.r.findFirstIn(ret._2)) {
            // println("readWithInclFilter inclusion  relicat ="+ret._1+ "\nenr="+ret._2)
            bool = false
          }

        }
      }
      ret
    }
  def readWithExclFilter(relicat: String, bufReader: BufferedReader, filtExcl: String): (String, String) =
    {
      var bool = true
      var ret = (relicat, "")
      while (bool) {
        ret = read(ret._1, bufReader)
        if (ret._2 == "") bool = false
        else {
          if (filtExcl.r.findFirstIn(ret._2) == None) {
            bool = false
          }
        }
      }
      ret
    }
}
case class MyReaderLineByLine() extends MyReader {
  override def read(relicat: String = "", bufReader: BufferedReader): (String, String) = {
    var ret = ("", "")
    var bool = true
    while (bool) {
      var line = bufReader.readLine

      if (null == line) {
        ret = ("", "")
        bool = false
      } else if (null != line && line.length > 4) {
        ret = ("", line)
        bool = false
      }
    }
    ret
  }
}
case class MyReaderMulti(debReg: Regex, finReg: Regex) extends MyReader {
  override def read(relicat: String, bufReader: BufferedReader): (String, String) =
    {
      //println("calling read in MyReaderMulti" )
      var isDebDetected = false
      var isEndDetected = false
      // var enr = relicat
      var bool = true
      var ret = ("", "")
      var strB = new StringBuilder(relicat)
      while ((!isDebDetected || !isEndDetected) && bool) {
        if (!isDebDetected) {

          var line = ""
          // MODIF  var deb = debReg.findFirstIn(enr)
          var intraBool = true
          while (intraBool) {
            var deb = debReg.findFirstIn(line)
            if (None == deb) {
              line = bufReader.readLine
              if (null == line) {
                intraBool = false
                bool = false
              }

            } else {
              strB=new StringBuilder(line)
              isDebDetected = true
              intraBool = false
              var newreg = (debReg.toString + """.*""").r
              //MODIF enr = newreg.findFirstIn(enr).get
              strB = new StringBuilder(newreg.findFirstIn(strB.toString).get)
              //println("deb detecte strB="+strB.toString)
            }
          }
        } else if (!isEndDetected && isDebDetected) {
          //println("entree fin 0 enr="+enr)
          // MODIFvar fin = finReg.findFirstIn(enr)
          var intraBool = true
          var line = ""
          while (intraBool) {
            var fin = finReg.findFirstIn(line)
            if (fin == None) {
              
              line = bufReader.readLine
              if (null == line) {
                intraBool = false
                bool = false
                ret = ("", strB.toString)
              } else strB.append(" ").append(line)
            } else {
              isEndDetected = true
              var enr = strB.toString
              var retEnr =  enr.substring(0, enr.indexOf(fin.get) + fin.get.length)
              var relicat = enr.substring(enr.indexOf(fin.get) + fin.get.length)
              ret = (relicat, retEnr)
              bool = false
              intraBool = false
              // construire le retour (relicat,enr)

            }
          }
        }

      }
      ret

    }
}
case class MyReaderMultiDebEqFin(debFinReg: Regex) extends MyReader {
  override def read(relicat: String, bufReader: BufferedReader): (String, String) =
    {
      //println("calling read in MyReaderMultiDebEqFin" )
      var isDebDetected = false
      var isEndDetected = false
      // var enr = relicat
      var strB = new StringBuilder(relicat)
      var bool = true
      var ret = ("", "")
      var deb = Option[String]("")
      while ((!isDebDetected || !isEndDetected) && bool) {

        if (!isDebDetected) {
          // MODIF deb = debFinReg.findFirstIn(enr)
        
          var intraBool = true
          var line = relicat
          while (intraBool) {
            deb = debFinReg.findFirstIn(line)
            if (deb == None) {
            
              line = bufReader.readLine
             
              if (null == line) {
                intraBool = false
                bool = false
              }
            } else {
             
             
              isDebDetected = true
              intraBool = false
              // MODIF enr += " " + line
              
              strB=new StringBuilder(line)

            }
          }

          
          var newreg = (debFinReg.toString + """.*""").r
         if (null != line){
          // MODIF enr = newreg.findFirstIn(enr).get
          strB = new StringBuilder(newreg.findFirstIn(strB.toString).get)
          
         }
        } else if (!isEndDetected && isDebDetected) {

          // MODIFvar fin = debFinReg.findFirstIn(enr.substring(deb.get.length))
        	
          var line = ""
          var intraBool = true

          while (intraBool) {
            
            var fin = debFinReg.findFirstIn(strB.toString.substring(deb.get.length))
//           println("debFinReg="+debFinReg.toString)
//           println("strB.toString.substring(deb.get.length)="+strB.toString.substring(deb.get.length))
            if (fin == None) {

              line = bufReader.readLine
              if (null == line) {
                intraBool = false
                bool = false
                // Modif ret = ("", enr)
                ret = ("", strB.toString)
              }
              else
              {
                 strB.append(" ").append(line)
              }
            } else {
              // MODIF enr += " " + line
             
              intraBool = false
              var cutDebEnr = strB.toString.substring(deb.get.length)
              
              var retEnr = deb.get + cutDebEnr.substring(0, cutDebEnr.indexOf(fin.get))
              var relicat = cutDebEnr.substring(cutDebEnr.indexOf(fin.get))
             
              ret = (relicat, retEnr)
            }
          }

          isEndDetected = true
          bool = false
          // construire le retour (relicat,enr)
          // MODIF  var cutDebEnr = enr.substring(deb.get.length)

        }
      }

     // println("ret=(relicat : "+ret._1+",retEnr : "+ret._2+")")
      ret

    }
}

object Test {

  def main(args: Array[String]) {
    val deb = System.currentTimeMillis
    var fReader = new FileReader(new File(args(0)))
    //  var fileOutLine=new FileWriter(new File(args(0)+".Ligne.out"))
    var fileOutMulti = new FileWriter(new File(args(0) + ".Multi.out"))

    val buff: BufferedReader = new BufferedReader(fReader)

    // val reader: MyReader = new MyReaderMulti("""\d{4}-\d\d-\d+T\d+""", """ seconds""")
    // val reader: MyReader = new MyReaderLineByLine
    //val reader: MyReader = new  MyReaderMulti("""\d{4}-\d\d-\d\dT""","""seconds""")
    val reader: MyReader = new MyReaderMultiDebEqFin("""\d{4}-\d\d-\d\dT""".r)
    var boolean = true
    var ret: (String, String) = ("", "")
    while (boolean) {
      //ret = reader.read(ret._1, buff,"(GET|RoleSpecification)")
      ret = reader.readWithInclFilter(ret._1, buff, "GC pause \\(young\\)")
      // ret = reader.read(ret._1, buff)
     
      if (ret._2.length > 4) {
        fileOutMulti.write(ret._2 + "\n")
      } else
        boolean = false

    }

    fReader.close
    //  fileOutLine.close
    fileOutMulti.close
    println("Duration =" + (System.currentTimeMillis - deb))
  }
}