package com.jlp.scaviewer.tools.php

import java.io.File
import java.util.HashMap
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.util.ArrayList
import java.io.OutputStream
import java.util.zip.GZIPOutputStream
import java.io.FileOutputStream
import java.io.InputStream
import java.util.TreeMap
import scala.annotation.tailrec
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.Collections
import java.util.Comparator
import java.text.DecimalFormatSymbols
import java.util.Locale
import java.text.DecimalFormat

class XHprofFormatToTree(file: File) {
  var currentRank: Int = 0
  var boolEndFile = false
  var currentLevel = 2
  var rankForCSV: Int = 0
  var gzipFile = false
  val BUFFER_SIZE = 20 * 1024 * 1024
  var documentRoot = ""
  val hmFunc: java.util.TreeMap[Int, MyTuple4] = new TreeMap()
  
  val dfs:DecimalFormatSymbols=new DecimalFormatSymbols(Locale.ENGLISH)
 // dfs.setGroupingSeparator('')
  dfs.setDecimalSeparator('.')
  val df:DecimalFormat=new DecimalFormat("#############0.###",dfs)

  // key Name of the function 
  // Value ( Global duration of the fonction in the chunk analyzed,
  // Global duration of the children,
  // nb exec of the function )
  // used to compute the self duration of the fonction ( Excluded time)
  // MyTuple3 (Double,Double,Int)
  val hmGlobalCosts: java.util.HashMap[String, MyTuple3] = new HashMap()

  // hmparent : Key String at the left of ==>
  // fisrt array list, rank where the parent appears like child
  // second list , id where parent is parent ( aka id of child)

  val hmParent: java.util.HashMap[String, ArrayList[Int]] = new HashMap()
  val hmParentWithNochild: java.util.HashMap[Int, String] = new HashMap()
  var newProfingFile = false
  var isValidChunk = true
  var withChild: Int = 0
  var withoutChild: Int = 0
  var dateFinChunk = ""
  var mainDuration: Long = 0
  var dateDebChunk = ""
  // 2014-11-21:08:53:17.406
  val sdf: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd:HH:mm:ss.SSS")

  var offset = 0
  // Structure of XHprofKey
  //  => name of the function
  // => name of the file containing the function
  // => rank of the function to construct the tree

  // Structure of XHprofCost
  //var rank: Int => repeat the rank of the key
  // level: Int, level of the function to construct the tree
  // nbExecs: Int = 1, nb excecution of the functions ( acse of boucles)
  // selfDurationInMillis: Double,
  // durationInMillis: Double including duration of inner calls
  // memoryUsed : Double ( bytes)
  // memoryPeak : Double

  var cmdFile: String = ""
  var lastDirectoryAndCmdFile = ""
  // csv file
  var sep = "/"
  println("file.getAbsolutePath()=" + file.getAbsolutePath())
  if (file.getAbsolutePath().contains("/")) {
    sep = "/"
  } else {
    sep = """\"""
  }
  println("file.getAbsolutePath()=" + file.getAbsolutePath() + " sep => " + sep)
  val title = new StringBuilder().
    append("date;rank;nameParent;nameFunction;level;nbExec;durationInMillis;selfDurationInMillis;cpuDurationInMillis;selfCpuDurationInMillis;memoryUsed,peakMemory;\n").toString
  val csvFile: File = new File(file.getParentFile().getAbsolutePath() + sep + "newFormatXHprof_" + file.getName())
  if (csvFile.exists()) csvFile.delete() // for dev only must be improved
  // println("csvFile = " + csvFile.getAbsolutePath())
  val outputStream = initOutputStream(csvFile)

  outputStream.write(title.getBytes())

  def treat() {
    val reader = initReader(file)

    treatFile(reader)
    reader.close()
    // compactCsvFile()
  }

  private def initReader(file: File): BufferedReader =
    {

      if (file.getName().endsWith(".gz")) {
        gzipFile = true
        return new BufferedReader(new InputStreamReader(
          new GZIPInputStream(new FileInputStream(file))),
          BUFFER_SIZE);

      } else {
        gzipFile = false
        return new BufferedReader(new InputStreamReader(
          new FileInputStream(file)), BUFFER_SIZE);

      }

    }

  def treatFile(reader: BufferedReader) {
    var numChucnk = 0
    boolEndFile = false
    while (!boolEndFile) {
      readAsEventsFirstArray(reader)
      var bool: Boolean = true

      isValidChunk = true

      while (bool) {
        val arr6Lines = read6lines(reader)
        // test if the chunck is always correct

        if (null != arr6Lines && !boolEndFile && (arr6Lines(0) != "END OF PROFILE") && this.isValidChunk) {
          //println("treatFile : loading : " + arr6Lines(0))
          loadHmFunc(arr6Lines)

          // TO BE CONTINUED

        } else if (null == arr6Lines || boolEndFile || (arr6Lines(0) == "END OF FILE")) {
          bool = false
        } else {
          // read until end of this chunk
          bool = readUntilNewChunk(reader)
          this.isValidChunk = true
          if (boolEndFile) bool = false
        }

      }
      numChucnk += 1
      println("hmFunc has : " + hmFunc.size() + " entries")
      println("hmParent has : " + hmParent.size() + " entries")
      println("hmGlobalCosts has : " + hmGlobalCosts.size() + " entries")
      //  println("hmFunc lastEntries =  : " + hmFunc.get(hmFunc.size() + offset - 1).xhprofCost.nameFunction)

      if (hmFunc.size() > 0) {
        val tup3 = hmGlobalCosts.get("main()")
        if (null != tup3) {
          println("GlobalTimeParent = " + tup3.globalDurationParent)
          println("GlobalTimeChild = " + tup3.globalDurationChildren)
          println("NbExecParent = " + tup3.nbExecutionParent)
        }
        computeLevelCostChildren()

        //    println("withChild =" + withChild)
        //    println("withoutChild =" + withoutChild)
        //    println(" dateFinChunk =" + dateFinChunk)
        val dateFin = sdf.parse(dateFinChunk)
        val calendar: Calendar = Calendar.getInstance()
        calendar.setTime(dateFin)
        val dateInMillis = calendar.getTimeInMillis() - this.mainDuration
        calendar.setTimeInMillis(dateInMillis)
        dateDebChunk = sdf.format(calendar.getTime())
        //    println(" mainDuration =" + mainDuration)
        //    println(" datedebChunk =" + dateDebChunk)

        writeRecursiveChunkToCsvFile(hmFunc.size() + offset - 1)
        println("treated chunk number : " + numChucnk + " with an offset of " + offset)
        offset += this.hmFunc.size()
      }

      clearAll()

    }

  }

  private def clearAll() {

    hmFunc.clear()
    hmParentWithNochild.clear()
    isValidChunk = true
    dateFinChunk = ""
    mainDuration = 0
    dateDebChunk = ""
    hmGlobalCosts.clear()
    hmParent.clear()
  }
  private def writeRecursiveChunkToCsvFile(key: Int) {
    val xhCost = hmFunc.get(key)
    outputStream.write(returLineForCsv(key, xhCost).getBytes())
    if (!xhCost.xhprofCost.children.isEmpty()) {
      val children = xhCost.xhprofCost.children
      val len = children.size()
      for (i <- 0 until len) {
        writeRecursiveChunkToCsvFile(children.get(i))
      }

    }

  }

  private def returLineForCsv(key: Int, xhCost: MyTuple4): String = {
    //"date;rank;nameFunction;level;nbExec;durationInMillis;selfDurationInMillis;cpuDurationInMillis;memoryUsed,peakMemory;\n

    (new StringBuilder().append(this.dateDebChunk).append(";").append(key).append(";").
      append(xhCost.xhprofCost.parent).append(";").append(xhCost.xhprofCost.nameFunction).append(";").append(xhCost.xhprofCost.level).append(";").
      append(xhCost.xhprofCost.nbExecs).append(";").append(df.format(xhCost.xhprofCost.durationInMillis)).append(";").
      append(df.format(xhCost.xhprofCost.selfDurationInMillis)).append(";").append(df.format(xhCost.xhprofCost.cpuDurationInMillis)).append(";").
      append(df.format(xhCost.xhprofCost.selfCpuDurationInMillis)).append(";").
      append(df.format(xhCost.xhprofCost.memoryUsed)).append(";").append(df.format(xhCost.xhprofCost.memoryPeak)).append(";\n")).toString
  }
  private def computeLevelCostChildren() {
    // algorith:
    // find the main method and 
    //  take the String child of the 4-tuple
    // search this String as key of hmParent
    // get the arrayList of Id ( keys of hmFunc)
    // for each key in the list if the 4-tuple is Orphan true()
    //  add the child to the parent  compute self duration  Of the parent, level of child and parent 
    // mark the 4-Tuple non ORPHAN => false
    // 

    // On cherche le main STOP qui est le dernier index

    var tup_4 = hmFunc.get(hmFunc.size() + offset - 1)
    println(" is parent STOP ? => " + tup_4.parent + " is child main ? => " + tup_4.child)
    tup_4.xhprofCost.level = 1
    tup_4.orphan = false
    hmFunc.put(hmFunc.size() + offset - 1, tup_4)

    // Le premier main
    tup_4 = hmFunc.get(hmFunc.size() + offset - 2)
    traiterRecursifFill(hmFunc.size() + offset - 1, "STOP", tup_4)

  }

  private class MyIntComparator extends Comparator[Int] {

    override def compare(aInt: Int, that: Int): Int = {
      aInt - that
    }

  }

  private def traiterRecursifFill(idParent: Int, parent: String, tup4Parent: MyTuple4) {
    // test if the idParent has chilren

    if (hmParent.containsKey(parent)) {
      var ids = this.hmParent.get(parent)

      //  
      val len = ids.size()
      var tmpArray = new ArrayList[Int]()
      for (i <- 0 until len) {
        val id = ids.get(i)
        val tup4Child = hmFunc.get(id)

        if (tup4Child.orphan) {
          //  if ((id != hmFunc.size() -1) && ( alias.isEmpty()  || id < alias.get(0))) {
          //   println(" traiterRecursif parent =" + parent)
          withChild += 1
          tup4Parent.xhprofCost.children.add(id)
          // tup4Parent.xhprofCost.cpuDurationInMillis -= tup4Child.xhprofCost.cpuDurationInMillis
          // tup4Parent.xhprofCost.selfDurationInMillis -= tup4Child.xhprofCost.durationInMillis
          val tup3Parent = hmGlobalCosts.get(parent)

          tup4Parent.xhprofCost.selfDurationInMillis = (tup3Parent.globalDurationParent - tup3Parent.globalDurationChildren) * tup4Parent.xhprofCost.nbExecs / tup3Parent.nbExecutionParent
          // tup4Parent.xhprofCost.selfDurationInMillis = tup4Parent.xhprofCost.selfDurationInMillis.toLong.toDouble / 1000
          tup4Parent.xhprofCost.selfCpuDurationInMillis = (tup3Parent.globalDurationCPUParent - tup3Parent.globalDurationCPUChildren) * tup4Parent.xhprofCost.nbExecs / tup3Parent.nbExecutionParent

          tup4Child.xhprofCost.level = tup4Parent.xhprofCost.level + 1
          tup4Child.orphan = false

          //          if ( tup4Child.xhprofCost.nameFunction.contains("op@3")  )
          //          //if (parent.contains( "op@3"))
          //              //&& tup4Child.xhprofCost.nameFunction.contains("op@3"))
          //            {
          //             println("parent => child : " +  parent+ " => "+tup4Child.xhprofCost.nameFunction)
          //            println("tup3Parent.globalDurationParent=" + tup3Parent.globalDurationParent)
          //            println("tup3Parent.globalDurationChildren=" + tup3Parent.globalDurationChildren)
          //             println("tup3Parent.globalDurationCPUParent=" + tup3Parent.globalDurationCPUParent)
          //            println("tup3Parent.globalDurationCPUChildren=" + tup3Parent.globalDurationCPUChildren)
          //            println("tup3Parent.nbExecutionParent=" + tup3Parent.nbExecutionParent)
          //            println("tup4Parent.xhprofCost.nbExecs=" + tup4Parent.xhprofCost.nbExecs)
          //            println("--------------------------------------------------------------")
          //             println("tup4Parent.xhprofCost.durationInMillis=" + tup4Parent.xhprofCost.durationInMillis)
          //            println("tup4Parent.xhprofCost.selfDurationInMillis=" + tup4Parent.xhprofCost.selfDurationInMillis)
          //             println("tup4Parent.xhprofCost.CpuDurationInMillis=" + tup4Parent.xhprofCost.cpuDurationInMillis)
          //            println("tup4Parent.xhprofCost.selfCpuDurationInMillis=" + tup4Parent.xhprofCost.selfCpuDurationInMillis)
          //            println("--------------------------------------------------------------")
          //          }
          // On remet dans le map
          hmFunc.put(id, tup4Child)
          hmFunc.put(idParent, tup4Parent)
          tmpArray.add(id)
        }
        // To avoid Loop infinite recursion

        hmParent.put(parent, tmpArray)
        traiterRecursifFill(id, tup4Child.child, tup4Child)
      }

    } else {

      if (!this.hmParentWithNochild.containsKey(idParent)) {
        this.withoutChild += 1
        //println("No child for " + idParent + " => " + parent)
        this.hmParentWithNochild.put(idParent, parent)
      } else {
        // println(" yet in the hashMapNo child for "+idParent+ " => "+parent)
      }
    }

  }
  private def readUntilNewChunk(reader: BufferedReader): Boolean = {
    var bool: Boolean = true
    var returnBool = true
    while (bool) {
      val line = reader.readLine()
      if (null == line) {
        this.boolEndFile = true
        returnBool = false
        bool = false
      } else if (line.contains("NEW PROFILE FILE BEGIN")) {
        returnBool = false
        bool = false
      }
      // println("readUntilNewChunk")
    }
    returnBool // never reached return in the while loop
  }

  def readAsEventsFirstArray(reader: BufferedReader) = {

    var bool: Boolean = true
    while (bool) {
      val line = reader.readLine()
      if (null == line) {
        bool = false
        this.boolEndFile = true
      } else {
        if (line.startsWith("cmd=")) {
          cmdFile = line.split("cmd=")(1).trim().replaceAll("\\\\", "/").replaceAll("//", "/")
          println("find cmdFile=" + cmdFile)
          var tmp1 = cmdFile.substring(0, cmdFile.lastIndexOf("/"))
          var idx = tmp1.lastIndexOf("/")
          lastDirectoryAndCmdFile = cmdFile.substring(idx + 1)
        }
        if (line.trim.startsWith("Array")) bool = false
      }
    }
  }

  def read6lines(reader: BufferedReader): Array[String] = {
    val returnArr: Array[String] = Array.ofDim(6)
    // recherche de la ligne de function
    var bool: Boolean = true
    while (bool) {
      var line = reader.readLine()
      if (null == line) {
        returnArr(0) = "END OF FILE"
        this.boolEndFile = true
        bool = false
        returnArr
      }
      // println("line =" + line)
      if (null != line) {
        if (None != """\[main\(\)\] => Array""".r.findFirstIn(line)) { returnArr(0) = "STOP==>main()" } //[main()] => Array}
        else if (None != """[^=]+==>[^\s]+""".r.findFirstIn(line)) {
          returnArr(0) = """[^=]+==>[^\s]+""".r.findFirstIn(line).get.trim().replaceAll("""\[""", "").replaceAll("""\]""", "").replaceAll("\\\\", "/").replaceAll("//", "/")

        } else if (None != """\[ct\]\s+=>\s+\d+""".r.findFirstIn(line)) returnArr(1) = line.split("=>")(1).trim() // [ct] => 1   nbExecs
        else if (None != """\[wt\]\s+=>\s+[-]?\d+""".r.findFirstIn(line)) {
          returnArr(2) = line.split("=>")(1).trim()
          if (returnArr(0).equals("STOP==>main()")) mainDuration = returnArr(2).toLong / 1000
          //  [wt] => 9338  global time in micro-secondes 
          if (returnArr(2).toLong < 0) this.isValidChunk = false

        } else if (None != """\[cpu\]\s+=>\s+[-]?\d+""".r.findFirstIn(line)) returnArr(3) = line.split("=>")(1).trim() //  [cpu] => 0 Cpu Time
        else if (None != """\[mu\]\s+=>\s+[-]?\d+""".r.findFirstIn(line)) returnArr(4) = line.split("=>")(1).trim() //  [mu] => 5320 memory used in bytes
        else if (None != """\[pmu\]\s+=>\s+[-]?\d+""".r.findFirstIn(line)) {
          //   println("traet pmu line =" + line)
          returnArr(5) = line.split("=>")(1).trim() //   [pmu] => 6984 peak memory used in bytes
          bool = false
          // returnArr // must return last value of the array
        } else if (None != """NEW PROFILE FILE END""".r.findFirstIn(line)) {
          // Date with millis
          dateFinChunk = line.split("=")(1).split("\\.")(0) + "." + line.split("=")(1).split("\\.")(1).substring(0, 3)
          returnArr(0) = "END OF PROFILE"
          bool = false
          // returnArr // must return also
        } else {
          //println("no treated line => " + line) // nothing to do
        }
      } else {
        bool = false
        returnArr(0) != "END OF FILE"
        boolEndFile = true
        // returnArr
      }
    }

    returnArr // returnArr must be filled in the while loop

  }

  protected def returnCallerCalled(line: String): (String, String) = {

    val tab = line.trim().split("==>")
    (tab(0).trim(), tab(1).trim())

  }
  protected def loadHmFunc(arr6Lines: Array[String]): Unit = {

    // types of lines for functions
    //  [run_init::C:\opt\wamp\apps\phpmyadmin4.1.14\index.php==>load::C:\opt\wamp\apps\phpmyadmin4.1.14\libraries\common.inc.php] 
    // [???_op==>getcwd] 
    //  [main()==>load::phpmyadmin4.1.14/index.php]
    // [???_op==>load::libraries/Error_Handler.class.php] 
    //  [???_op@1==>defined] @1 level of recursion
    //  [PMA_Config::loadDefaults==>load::libraries/config.default.php]
    // [main()] fonction main

    // cut caller ==> calleed
    //    if (arr6Lines(0) == "main()" {
    //      // To be done maj main
    //     
    //    }else

    //    arr6Lines(0).split("==>")(0) match {
    //      case s if s.matches("""main\(\)""") => currentLevel = 1
    //      case s if s.matches("""_op$""") => currentLevel = 2
    //      case s if s.matches("""_op@\d+""") => currentLevel = 2 + """\d+$""".r.findFirstIn(s.trim()).get.toInt
    //      case _ =>
    //
    //    }

    val (parent, child) = returnCallerCalled(arr6Lines(0))
    val costChild = new XHprofCost(child, 1, arr6Lines(1).toInt, arr6Lines(2).toDouble, arr6Lines(2).toDouble,
      arr6Lines(3).toDouble, 0D, arr6Lines(4).toDouble, arr6Lines(5).toDouble)
    costChild.parent = parent
    hmFunc.put(this.currentRank, new MyTuple4(parent, child, costChild, true)) // true parent is filled with the cost

    // arr6Lines(0) : line of parent => child
    // arr6Lines(1) : nb Exec
    // arr6Lines(2) : Duration in micros
    // arr6Lines(3) : CpuDuration in micros
    // arr6Lines(4) : memory used in bytes
    // arr6Lines(5) : peak memory in bytes

    // MyTuple3(var globalDurationParent: Double, var globalDurationChildren: Double, var nbExecutionParent: Int)

    //  if (parent == "STOP") println(" yeap STOP parent")
    // if (parent.contains("main()")) println(" yeap main parent =>" + parent + " child =" + child)
    if (hmParent.containsKey(parent)) {
      val myList = hmParent.get(parent)

      myList.add(currentRank)

      hmParent.put(parent, myList)
    } else {
      //  if (parent == "main()") println(" yeap main() parent bis currentRank =" + currentRank)
      val myList = new ArrayList[Int]()

      myList.add(currentRank)
      hmParent.put(parent, myList)
    }
    // Search if child is already parent
    //    if (hmParent.containsKey(child)) {
    //      val myList = hmParent.get(child)
    //      myList.add(currentRank)
    //
    //      hmParent.put(child, myList)
    //    }

    // Tuple3
    //(globalDurationParent,globalDurationChildren,nbExecutionParent,globalDurationCPUParent,globalDurationCPUChildren,orphan) 
    if (hmGlobalCosts.containsKey(parent)) {
      var tuple3 = hmGlobalCosts.get(parent)
      tuple3.globalDurationChildren = tuple3.globalDurationChildren + arr6Lines(2).toDouble
      tuple3.globalDurationCPUChildren = tuple3.globalDurationCPUChildren + arr6Lines(3).toDouble
      tuple3.orphan = false
      hmGlobalCosts.put(parent, tuple3)

    } else {
      val tuple3 = new MyTuple3(0D, arr6Lines(2).toDouble, 0, 0D, arr6Lines(3).toDouble, false)
      hmGlobalCosts.put(parent, tuple3)
    }

    if (hmGlobalCosts.containsKey(child)) {
      var tuple3 = hmGlobalCosts.get(child)
      tuple3.globalDurationParent = tuple3.globalDurationParent + arr6Lines(2).toDouble
      tuple3.globalDurationCPUParent = tuple3.globalDurationCPUParent + arr6Lines(3).toDouble
      tuple3.nbExecutionParent = tuple3.nbExecutionParent + arr6Lines(1).toInt

      hmGlobalCosts.put(child, tuple3)

    } else {
      val tuple3 = new MyTuple3(arr6Lines(2).toDouble, 0D, arr6Lines(1).toInt, arr6Lines(3).toDouble, 0D, false)
      hmGlobalCosts.put(child, tuple3)
    }
    currentRank += 1
  }

  //  
  private def initOutputStream(file: File): OutputStream = {

    if (file.getName().endsWith(".gz")) {
      gzipFile = true
      new GZIPOutputStream(new FileOutputStream(file))

    } else {
      gzipFile = false
      new FileOutputStream(file)

    }

  }
  private def initFileInputStream(file: File): InputStream = {

    if (file.getName().endsWith(".gz")) {
      gzipFile = true
      new GZIPInputStream(new FileInputStream(file))

    } else {
      gzipFile = false
      new FileInputStream(file)

    }

  }

}

object XHprofFormatToTree {
  def main(args: Array[String]) {

    // args(0) is the file to treat

    val toTreat = new File(args(0))
    if (toTreat.exists()) {
      new XHprofFormatToTree(toTreat).treat()
    }
  }

}

case class XHprofCost(nameFunction: String, var level: Int, var nbExecs: Int = 1, var selfDurationInMillis: Double, var durationInMillis: Double, var cpuDurationInMillis: Double, var selfCpuDurationInMillis: Double = 0D, var memoryUsed: Double, var memoryPeak: Double) {

  // Structure of XHprofCost
  //var rank: Int => repeat the rank of the key
  // level: Int, level of the function to construct the tree
  // nbExecs: Int = 1, nb excecution of the functions ( acse of boucles)
  // selfDurationInMillis: Double,
  // durationInMillis: Double including duration of inner calls
  // memoryUsed : Double ( bytes)
  // memoryPeak : Double
  var children: java.util.List[Int] = new ArrayList[Int]()
  var parent: String = ""

}
case class XHprofCaller(var rank: Int, var level: Int, nameFunction: String) extends java.lang.Comparable[XHprofCaller] {

  override def compareTo(that: XHprofCaller): Int = {
    rank.compareTo(that.rank)
  }

  def equals(that: XHprofCaller): Boolean = {
    nameFunction.equals(that.nameFunction)
  }

}

case class MyTuple3(var globalDurationParent: Double, var globalDurationChildren: Double, var nbExecutionParent: Int, var globalDurationCPUParent: Double, var globalDurationCPUChildren: Double, var orphan: Boolean = true) {}

case class MyTuple4(var parent: String, var child: String, var xhprofCost: XHprofCost, var orphan: Boolean) {}
case class XHprofCostFungHeader(func1: String, func2: String) {
  val isMain: Boolean = {
    if ((func2 == "") && func1 == "main") {
      true
    }
    false
  }

}
