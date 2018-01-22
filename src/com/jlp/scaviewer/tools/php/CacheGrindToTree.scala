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
import java.text.DecimalFormatSymbols
import java.util.Locale
import java.text.DecimalFormat

class CacheGrindToTree(file: File) {
  
   val dfs:DecimalFormatSymbols=new DecimalFormatSymbols(Locale.ENGLISH)
 // dfs.setGroupingSeparator('')
  dfs.setDecimalSeparator('.')
  val df:DecimalFormat=new DecimalFormat("#############0.000",dfs)

  var currentRank: Int = 0
  var rankForCSV: Int = 0
  var gzipFile = false
  var BUFFER_SIZE = 20 * 1024 * 1024
  val hmFunc: java.util.HashMap[MyKey, Cost] = new HashMap()
  var aliveFunctions: java.util.HashMap[String, ArrayList[MyKey]] = new HashMap()
  var aliveFunctionsHorsMainAssoc: java.util.HashMap[String, MyKey] = new HashMap()
  var keyMain: MyKey = null
  var summaryDuration: Double = -1
  var newProfingFile = true
  var hasMainFunc = false
  var newMain = true
  // Structure of Key
  //  => name of the function
  // => name of the file containing the function
  // => rank of the function to construct the tree

  // Structure of Cost
  //var rank: Int => repeat the rank of the key
  // level: Int, level of the function to construct the tree
  // nbExecs: Int = 1, nb excecution of the functions ( acse of boucles)
  // selfDurationInMillis: Double,
  // durationInMillis: Double including duration of inner calls

  var cmdFile: String = ""
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
    append("rank;nameFunction;fileNameRow;level;nbExec;durationInMillis;selfDurationInMillis);\n").toString
  val csvFile: File = new File(file.getParentFile().getAbsolutePath() + sep + "newFormatProfile_" + file.getName())
  //if (csvFile.exists()) csvFile.delete() // for dev only must be improved
  // println("csvFile = " + csvFile.getAbsolutePath())
  val outputStream = initOutputStream(csvFile)

  outputStream.write(title.getBytes())

  def treat() {
    val reader = initReader(file)
  

    // on recommence mais e coup si pas de fonction main  on regroupe au mieux

    while (this.newProfingFile) {
       readAsEventsTime(reader)
        newMain = true
      while (newMain) {
        
        hmFunc.clear()
        keyMain = null
        hasMainFunc = false
        aliveFunctions.clear()
        aliveFunctionsHorsMainAssoc.clear
        hasMainFunc = false
        
        treatMain(reader)
        hmFunc.clear()
        keyMain = null
        hasMainFunc = false
        aliveFunctions.clear()
        aliveFunctionsHorsMainAssoc.clear
        hasMainFunc = false
      }
    }

    reader.close()
    outputStream.close()

    compactCsvFile()
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

  def treatMain(reader: BufferedReader) {
    keyMain = null;
    hasMainFunc = false
    aliveFunctionsHorsMainAssoc.clear
    var bool: Boolean = true
    while (bool) {
      val arr4Lines = read4lines(reader)
      if (arr4Lines(0) != "END OF FILE") {

        if (arr4Lines(0).contains("NEW PROFILING FILE") ||
          arr4Lines(1).contains("NEW PROFILING FILE") ||
          arr4Lines(2).contains("NEW PROFILING FILE") ||
          arr4Lines(3).contains("NEW PROFILING FILE")) {
          println("found NEW PROFILING FILE exiting")
          if (arr4Lines(0).trim().startsWith("cmd:")) cmdFile = arr4Lines(0).split("cmd:")(1).trim()
          if (arr4Lines(1).trim().startsWith("cmd:")) cmdFile = arr4Lines(1).split("cmd:")(1).trim()
          if (arr4Lines(2).trim().startsWith("cmd:")) cmdFile = arr4Lines(2).split("cmd:")(1).trim()
          if (arr4Lines(3).trim().startsWith("cmd:")) cmdFile = arr4Lines(3).split("cmd:")(1).trim()
          bool = false
          newProfingFile=true
        } else {

          // if the 4th lines is empty and first line contains php:internal do nothing
          if (arr4Lines(3).trim().length() < 3 && arr4Lines(0).contains("php:internal")) {
            //nothing to do
          } // if the foursth line is empty on mets la fonction dans le hashmap
          else if (arr4Lines(3).trim().length() < 3 && !arr4Lines(0).contains("php:internal")) {

            loadHmFunc(arr4Lines)
          } else if (arr4Lines(3).trim().length() > 3) {
            // stopping main analyse when encounting summary tag
            if (arr4Lines(3).contains("summary")) {
              println("found summary exiting")
              // filling summaryCost
              summaryDuration = arr4Lines(3).split(":")(1).trim().toDouble
              bool = false
              hasMainFunc = true
            } else {
              // treatement off association of calls
              treatAssocCalls(reader, arr4Lines)
            }

          }
        }

      } else {
        bool = false
        newProfingFile=false		
      }
    }
    if (hasMainFunc) {
      //traiter la partie main pour integrer les functions ci-dessus
      val localArr: Array[String] = Array.ofDim(4)
      localArr(0) = "fl=" + this.cmdFile
      localArr(1) = "fn={main}"
      localArr(3) = reader.readLine() // ligne vide
      localArr(2) = reader.readLine() // ligne des couts
      localArr(3) = reader.readLine() // first line of incluses functions
      keyMain = new MyKey(currentRank, "{main}", localArr(0).split("=")(1))
      println("before treatment of main hmFunc has " + this.hmFunc.size() + " entries")
      //    val keySet: java.util.Set[MyKey] = this.hmFunc.keySet()
      //    val it = keySet.iterator()
      //    while (it.hasNext()) {
      // println(it.next)
      //  }

      treatAssocCalls(reader, localArr)
      println("keyMain=" + keyMain.toString())
      // TO DO traiter la partie hors main
      println("cmdFile => " + cmdFile)
      println("hmFunc has " + this.hmFunc.size() + " entries")

      // Create csv file with this structure :
      // rank;nameFunction;fileNameRow;level;nbExec;durationInMillis;selfDurationInMillis
      // rank here is a new rank starting to 0 for the cmde

      // ecriture cmd
      var sep2: String = "/"
      if (cmdFile.contains("/")) sep2 = "/" else sep2 = """\"""
      var nameFunction = cmdFile.substring(cmdFile.lastIndexOf(sep2) + 1)
      var rank = this.rankForCSV
      var level: Int = 0
      var fileNameRow = cmdFile + "_1"
      var nbExec = 1
    var durationInMillis = ((this.summaryDuration*1000).toLong.toDouble/1000).toDouble
      // do not include functions out of main functions
      // do not include functions out of main functions

      var selfDuration = this.summaryDuration - hmFunc.get(keyMain).durationInMillis
      val line = new StringBuilder().
        append(rank).append(";").append(nameFunction).append(";").
        append(fileNameRow).append(";").append(level).append(";").
        append(nbExec).append(";").append(df.format(durationInMillis)).append(";").
        append(df.format(selfDuration)).append(";\n").toString
      outputStream.write(line.getBytes())
      this.rankForCSV += 1

      // System.exit(0)
      // treamentArboMain(outputStream)
      treamentArbo(keyMain, outputStream)
      keyMain = null
      hasMainFunc = false
      hmFunc.clear()
      newMain = true
    } else {
      // TODO un traitement sans fonction main a faire
      newMain = false
       hasMainFunc = true
      println("before treatment of Hors main hmFunc has " + hmFunc.size() + " entries")

      println("before treatment of Horsmain aliveFunctionsHorsMainAssoc has " + this.aliveFunctionsHorsMainAssoc.size() + " entries")
      val iter = aliveFunctionsHorsMainAssoc.keySet().iterator()
      while (iter.hasNext()) {
        val key = iter.next()
        println("key => " + key + " assoc => " + aliveFunctionsHorsMainAssoc.get(key))
        treamentArbo(aliveFunctionsHorsMainAssoc.get(key), outputStream)

      }
    }

  }

  def treamentArbo(aKey: MyKey, outputStream: OutputStream) {
    // TODO
    // starting from keyMain/costMain
    val mainCost = hmFunc.get(aKey)

    var bool: Boolean = true
    outputStream.write(lineForCsvFile(aKey, mainCost, 1).getBytes())
    this.rankForCSV += 1

    var innerlevel = 2
    var ln = hmFunc.get(aKey).children.size()
    // println("ln main children=" + hmFunc.get(keyMain).children.size())

    for (i <- 0 until ln) {

      recursWriteLine(hmFunc.get(aKey).children.get(i), hmFunc.get(hmFunc.get(aKey).children.get(i)), 2, outputStream)
    }

  }

  def compactCsvFile() {
    // TODO 
    // Normalizing also in milliseconds
    val readerCsv = initReader(csvFile)
    // read the title
    var line: String = readerCsv.readLine()
    var idCompcat: Int = 0
    val tmpCsvFile = new File(csvFile.getParent() + sep + "tmp_" + csvFile.getName())
    if (tmpCsvFile.exists()) tmpCsvFile.delete()
    println("tmpCsvFile =>" + tmpCsvFile.getAbsolutePath())
    val outputStream = initOutputStream(tmpCsvFile)
    outputStream.write(title.getBytes())
    var bool: Boolean = true
    line = readerCsv.readLine()
    var line2 = readerCsv.readLine()
    var struct1: StructReadProfile = new StructReadProfile(line)
    var struct2: StructReadProfile = new StructReadProfile(line2)
    while (bool) {

      if (struct1.isTheSame(struct2)) {
        struct1.mergeStruct(struct2)

      } else {
        struct1.cloreStruct()
        outputStream.write((idCompcat.toString() + struct1.returnLineForCsvFile() + "\n").getBytes())
        struct1 = new StructReadProfile(line2)
        idCompcat += 1
      }
      line2 = readerCsv.readLine()
      if (null == line2) {
        bool = false
        // on clot struct1
        struct1.cloreStruct()
        outputStream.write((idCompcat.toString() + struct1.returnLineForCsvFile() + "\n").getBytes())
      } else {
        struct2 = new StructReadProfile(line2)
      }
    }
    outputStream.close()
    readerCsv.close()
    csvFile.delete()
    tmpCsvFile.renameTo(csvFile)
  }
  //
  //  def treamentArboMain(outputStream: OutputStream) {
  //    // TODO
  //    // starting from keyMain/costMain
  //    val mainCost = hmFunc.get(keyMain)
  //    var bool: Boolean = true
  //    outputStream.write(lineForCsvFile(this.keyMain, mainCost, 1).getBytes())
  //    this.rankForCSV += 1
  //
  //    var innerlevel = 2
  //    var ln = hmFunc.get(keyMain).children.size()
  //    // println("ln main children=" + hmFunc.get(keyMain).children.size())
  //
  //    for (i <- 0 until ln) {
  //
  //      recursWriteLine(hmFunc.get(keyMain).children.get(i), hmFunc.get(hmFunc.get(keyMain).children.get(i)), 2, outputStream)
  //    }
  //
  //  }

  def recursWriteLine(aKey: MyKey, aCost: Cost, aLevel: Int, outputStream: OutputStream) {
    outputStream.write(lineForCsvFile(aKey, aCost, aLevel).getBytes())

    rankForCSV += 1
    var ln = hmFunc.get(aKey).children.size()

    for (i <- 0 until ln) {
      recursWriteLine(hmFunc.get(aKey).children.get(i), hmFunc.get(hmFunc.get(aKey).children.get(i)), aLevel + 1, outputStream)
    }

  }

  def lineForCsvFile(myKey: MyKey, cost: Cost, level: Int): String = {
    //  println("myKey ="+myKey.toString)
    var fileNameRow = myKey.fileOfFunction + "_" + cost.line

    new StringBuilder().
      append(this.rankForCSV).append(";").append(myKey.nameFunction).append(";").
      append(fileNameRow).append(";").append(level).append(";").
      append(cost.nbExecs).append(";").append(df.format(cost.durationInMillis)).append(";").
      append(df.format(cost.selfDurationInMillis)).append(";\n").toString

  }

  def readAsEventsTime(reader: BufferedReader) = {

    var bool: Boolean = true
    while (bool) {
      val line = reader.readLine()
      if (line.startsWith("cmd:")) cmdFile = line.split("cmd:")(1).trim()
      if (line.contains("events: Time")) bool = false
    }
  }

  def read4lines(reader: BufferedReader): Array[String] = {
    val returnArr: Array[String] = Array.ofDim(4)
    var bool: Boolean = true
    var line: String = null
    while (bool) {
      line = reader.readLine()
      if (null == line) {
        returnArr(0) = "END OF FILE"
        bool = false
        returnArr
      }

      if (bool && line.trim().length() > 3) bool = false

    }
    returnArr(0) = line
    returnArr(1) = reader.readLine()
    if (null == returnArr(1)) { returnArr(0) = "END OF FILE"; returnArr }
    returnArr(2) = reader.readLine()
    if (null == returnArr(2)) { returnArr(0) = "END OF FILE"; returnArr }
    returnArr(3) = reader.readLine()
    if (null == returnArr(3)) { returnArr(0) = "END OF FILE"; returnArr }
    returnArr
  }

  def loadHmFunc(arr4Lines: Array[String]) = {

    // 2 possible formats 
    // fl=C:\opt\wamp\apps\phpmyadmin4.1.14\libraries\Config.class.php
    // fn=PMA_Config->get
    // 1207 0
    //
    // or
    //
    // fl=C:\opt\wamp\apps\phpmyadmin4.1.14\libraries\Message.class.php
    // fn=require_once::C:\opt\wamp\apps\phpmyadmin4.1.14\libraries\Message.class.php
    // 1 0
    var nameFunction = arr4Lines(1).split("=")(1)
    if (arr4Lines(1).contains("include") || arr4Lines(1).contains("require")) {
      nameFunction = nameFunction.split("::")(0)
    }
    val key: MyKey = new MyKey(currentRank, nameFunction, arr4Lines(0).split("=")(1))
    if (aliveFunctions.containsKey(nameFunction + ":_:" + arr4Lines(0).split("=")(1))) {
      val myList = aliveFunctions.get(nameFunction + ":_:" + arr4Lines(0).split("=")(1))
      myList.add(key)

      aliveFunctions.put(nameFunction + ":_:" + arr4Lines(0).split("=")(1), myList)
    } else {

      val myList: ArrayList[MyKey] = new ArrayList()
      myList.add(key)

      aliveFunctions.put(nameFunction + ":_:" + arr4Lines(0).split("=")(1), myList)
    }

    val duration: Double = arr4Lines(2).split("""\s+""")(1).toDouble
    val numLine: Int = arr4Lines(2).split("""\s+""")(0).toInt
    val cost = new Cost(numLine, 1, 1, duration, duration)
    hmFunc.put(key, cost)
    currentRank += 1

  }

  def treatAssocCalls(reader: BufferedReader, arr4Lines: Array[String]): Unit = {
    //fl=C:\opt\wamp\apps\phpmyadmin4.1.14\libraries\Error.class.php
    //fn=require_once::C:\opt\wamp\apps\phpmyadmin4.1.14\libraries\Error.class.php
    //1 47999
    //cfl=php:internal
    //cfn=php::defined
    //calls=1 0 0
    //9 0
    //cfl=C:\opt\wamp\apps\phpmyadmin4.1.14\libraries\Message.class.php
    //cfn=require_once::C:\opt\wamp\apps\phpmyadmin4.1.14\libraries\Message.class.php
    //calls=1 0 0
    //16 0

    // The 3 first line define a new function with her self duration add to hmFunc
    // println("nameFunction arr4Lines(1) =" + arr4Lines(1));
    // println("nameFunction=" + arr4Lines(1).split("=")(1))
    var nameFunction = arr4Lines(1).split("=")(1)
    if (arr4Lines(1).contains("include") || arr4Lines(1).contains("require")) {
      nameFunction = nameFunction.split("::")(0)
    }

    val key: MyKey = new MyKey(currentRank, nameFunction, arr4Lines(0).split("=")(1))
    currentRank += 1
    // println("treatAssocCalls 0 : key = " + key)
    val duration: Double = arr4Lines(2).split("""\s+""")(1).toDouble
    val numLine: Int = arr4Lines(2).split("""\s+""")(0).toInt

    val cost = new Cost(numLine, 1, 1, duration, duration)
    hmFunc.put(key, cost)

    if (aliveFunctions.containsKey(nameFunction + ":_:" + arr4Lines(0).split("=")(1))) {
      // println("treatAssocCalls 1 :  trouve nameFunction + \":_:\" + arr4Lines(0).split(\"=\")(1)) = " + nameFunction + ":_:" + arr4Lines(0).split("=")(1))
      val myList = aliveFunctions.get(nameFunction + ":_:" + arr4Lines(0).split("=")(1))
      myList.add(key)
      aliveFunctions.put(nameFunction + ":_:" + arr4Lines(0).split("=")(1), myList)

    } else {
      val myList = new ArrayList[MyKey]()
      myList.add(key)
      // println("treatAssocCalls 1 :  ajout aliveFunction de nameFunction + \":_:\" + arr4Lines(0).split(\"=\")(1)) = " + nameFunction + ":_:" + arr4Lines(0).split("=")(1))
      aliveFunctions.put(nameFunction + ":_:" + arr4Lines(0).split("=")(1), myList)
    }

    // the other function are php:internal or custom function yet in hmFunc
    // we have to swich arr4Lines(0) with arrLines(3) and read 3 more line

    // For anticipate case of  {main} not found 
    this.aliveFunctionsHorsMainAssoc.put(nameFunction + ":_:" + arr4Lines(0).split("=")(1), key)

    arr4Lines(0) = arr4Lines(3)
    if (null == arr4Lines(0) || arr4Lines(0).trim().length() < 3) {
      println("no Children for " + key)
      return
    }

    arr4Lines(1) = reader.readLine()
    arr4Lines(2) = reader.readLine()
    arr4Lines(3) = reader.readLine()

    // treatement of children functions

    nameFunction = arr4Lines(1).split("=")(1)
    if (arr4Lines(1).contains("include") || arr4Lines(1).contains("require")) {
      nameFunction = nameFunction.split("::")(0)
      // verification si contenue dans function alive
      var innerKey: MyKey = null
      var innerCost: Cost = null

      if (aliveFunctions.containsKey(nameFunction + ":_:" + arr4Lines(0).split("=")(1))) {

        val myList = aliveFunctions.get(nameFunction + ":_:" + arr4Lines(0).split("=")(1))
        innerKey = myList.get(0)

        myList.remove(0)

        if (myList.isEmpty()) {
          aliveFunctions.remove(nameFunction + ":_:" + arr4Lines(0).split("=")(1))
        } else {
          aliveFunctions.put(nameFunction + ":_:" + arr4Lines(0).split("=")(1), myList)
        }

        innerCost = hmFunc.get(innerKey)

      } else {
        innerKey = new MyKey(currentRank, nameFunction, arr4Lines(0).split("=")(1))
        currentRank += 1
        val numLine: Int = arr4Lines(3).split("""\s+""")(0).toInt
        innerCost = new Cost(numLine, 2, 1, duration, duration)
      }

      // For anticipate case of  {main} not found 
      if (this.aliveFunctionsHorsMainAssoc.containsKey(nameFunction + ":_:" + arr4Lines(0).split("=")(1))) {
        // On la supprime car incluse
        this.aliveFunctionsHorsMainAssoc.remove(nameFunction + ":_:" + arr4Lines(0).split("=")(1))

      }
      val innerDuration: Double = arr4Lines(3).split("""\s+""")(1).toDouble

      innerCost.parent = key
      cost.children.add(innerKey)

      cost.durationInMillis = cost.durationInMillis + innerDuration
      hmFunc.put(innerKey, innerCost)
      hmFunc.put(key, cost)

    } else if (arr4Lines(1).contains("php:")) {
      nameFunction = nameFunction.split("::")(1)
      val innerKey: MyKey = new MyKey(currentRank, nameFunction, arr4Lines(0).split("=")(1))
      currentRank += 1

      val innerDuration: Double = arr4Lines(3).split("""\s+""")(1).toDouble
      val numLine: Int = arr4Lines(3).split("""\s+""")(0).toInt
      val innerCost = new Cost(numLine, 2, 1, duration, duration)
      innerCost.parent = key
      cost.children.add(innerKey)
      cost.durationInMillis = cost.durationInMillis + innerDuration
      hmFunc.put(innerKey, innerCost)
      hmFunc.put(key, cost)
    } else {
      // treatement of alive Function
      var innerKey: MyKey = null
      var innerCost: Cost = null
      if (aliveFunctions.containsKey(nameFunction + ":_:" + arr4Lines(0).split("=")(1))) {

        val myList = aliveFunctions.get(nameFunction + ":_:" + arr4Lines(0).split("=")(1))
        innerKey = myList.get(0)
        myList.remove(0)
        if (myList.isEmpty()) {
          aliveFunctions.remove(nameFunction + ":_:" + arr4Lines(0).split("=")(1))
        } else {
          aliveFunctions.put(nameFunction + ":_:" + arr4Lines(0).split("=")(1), myList)
        }

        innerCost = hmFunc.get(innerKey)
      } else {
        innerKey = new MyKey(currentRank, nameFunction, arr4Lines(0).split("=")(1))
        currentRank += 1
        val numLine: Int = arr4Lines(3).split("""\s+""")(0).toInt
        innerCost = new Cost(numLine, 2, 1, duration, duration)
      }

      // For anticipate case of  {main} not found 
      if (this.aliveFunctionsHorsMainAssoc.containsKey(nameFunction + ":_:" + arr4Lines(0).split("=")(1))) {
        // On la supprime car incluse
        this.aliveFunctionsHorsMainAssoc.remove(nameFunction + ":_:" + arr4Lines(0).split("=")(1))

      }
      val innerDuration: Double = arr4Lines(3).split("""\s+""")(1).toDouble
      val numLine: Int = arr4Lines(3).split("""\s+""")(0).toInt
      innerCost.parent = key
      innerCost.line = numLine
      cost.children.add(innerKey)
      cost.durationInMillis = cost.durationInMillis + innerDuration
      hmFunc.put(innerKey, innerCost)
      hmFunc.put(key, cost)
      // supprimer la fonction de la liste des fonction vivantes car consommee
      // aliveFunctions.remove(nameFunction + ":_:" + arr4Lines(0).split("=")(1))
    }
    //       val myList = new ArrayList[MyKey]()
    //      myList.add(key)
    //      aliveFunctions.put(nameFunction + ":_:" + arr4Lines(0).split("=")(1), myList)
    //    hmFunc.put(key, cost)

    // Continuer a lire jusqu au prochain saut de ligne
    var bool: Boolean = true
    var localArray: Array[String] = Array.ofDim(4)
    // println("avant lecture de la prochaine ligne arr4Lines(3) = "+arr4Lines(3))
    while (bool) {
      localArray(0) = reader.readLine()
      // println("currentRank="+currentRank+" localArray(0)="+   localArray(0))
      if (localArray(0).trim().length() > 3) {
        localArray(1) = reader.readLine()
        localArray(2) = reader.readLine()
        localArray(3) = reader.readLine()
        nameFunction = localArray(1).split("=")(1)
        if (localArray(1).contains("include") || localArray(1).contains("require")) {
          nameFunction = nameFunction.split("::")(0)
          var innerKey: MyKey = null
          var innerCost: Cost = null
          if (aliveFunctions.containsKey(nameFunction + ":_:" + localArray(0).split("=")(1))) {

            val myList = aliveFunctions.get(nameFunction + ":_:" + localArray(0).split("=")(1))
            innerKey = myList.get(0)
            myList.remove(0)
            if (myList.isEmpty()) {
              aliveFunctions.remove(nameFunction + ":_:" + localArray(0).split("=")(1))
            } else {
              aliveFunctions.put(nameFunction + ":_:" + localArray(0).split("=")(1), myList)
            }

            innerCost = hmFunc.get(innerKey)

          } else {
            innerKey = new MyKey(currentRank, nameFunction, localArray(0).split("=")(1))
            currentRank += 1
            val numLine: Int = localArray(3).split("""\s+""")(0).toInt
            innerCost = new Cost(numLine, 2, 1, duration, duration)
          }

          // For anticipate case of  {main} not found 
          if (this.aliveFunctionsHorsMainAssoc.containsKey(nameFunction + ":_:" + localArray(0).split("=")(1))) {
            // On la supprime car incluse
            this.aliveFunctionsHorsMainAssoc.remove(nameFunction + ":_:" + localArray(0).split("=")(1))

          }
          val innerDuration: Double = localArray(3).split("""\s+""")(1).toDouble
          val numLine: Int = localArray(3).split("""\s+""")(0).toInt
          cost.children.add(innerKey)
          innerCost.line = numLine
          cost.durationInMillis = cost.durationInMillis + innerDuration
          innerCost.parent = key
          hmFunc.put(innerKey, innerCost)
          hmFunc.put(key, cost)
        } else if (localArray(1).contains("php:")) {
          nameFunction = nameFunction.split("::")(1)
          val innerKey: MyKey = new MyKey(currentRank, nameFunction, localArray(0).split("=")(1))
          currentRank += 1
          val innerDuration: Double = localArray(3).split("""\s+""")(1).toDouble
          val numLine: Int = localArray(3).split("""\s+""")(0).toInt
          val innerCost = new Cost(numLine, 2, 1, innerDuration, innerDuration)
          innerCost.parent = key
          innerCost.line = numLine
          cost.children.add(innerKey)
          cost.durationInMillis = cost.durationInMillis + innerDuration
          hmFunc.put(innerKey, innerCost)
          hmFunc.put(key, cost)
        } else {
          // treatement of alive Function
          //  println()
          //println("nameFunction + \":_:\" + localArray(0).split(\"=\")(1)="+nameFunction + ":_:" + localArray(0).split("=")(1))

          var innerKey: MyKey = null
          var innerCost: Cost = null
          if (aliveFunctions.containsKey(nameFunction + ":_:" + localArray(0).split("=")(1))) {
            val myList = aliveFunctions.get(nameFunction + ":_:" + localArray(0).split("=")(1))
            innerKey = myList.get(0)
            myList.remove(0)
            if (myList.isEmpty()) {
              aliveFunctions.remove(nameFunction + ":_:" + localArray(0).split("=")(1))
            } else {
              aliveFunctions.put(nameFunction + ":_:" + localArray(0).split("=")(1), myList)
            }

            innerCost = hmFunc.get(innerKey)
          } else {
            val numLine: Int = localArray(3).split("""\s+""")(0).toInt
            innerKey = new MyKey(currentRank, nameFunction, localArray(0).split("=")(1))
            currentRank += 1

            innerCost = new Cost(numLine, 2, 1, duration, duration)
          }

          // For anticipate case of  {main} not found 
          if (this.aliveFunctionsHorsMainAssoc.containsKey(nameFunction + ":_:" + localArray(0).split("=")(1))) {
            // On la supprime car incluse
            this.aliveFunctionsHorsMainAssoc.remove(nameFunction + ":_:" + localArray(0).split("=")(1))

          }

          val innerDuration: Double = localArray(3).split("""\s+""")(1).toDouble
          innerCost.parent = key

          cost.durationInMillis = cost.durationInMillis + innerDuration
          cost.children.add(innerKey)
          hmFunc.put(innerKey, innerCost)
          hmFunc.put(key, cost)
          // supprimer la fonction de la liste des fonction vivantes car consommee
          // aliveFunctions.remove(nameFunction + ":_:" + localArray(0).split("=")(1))
        }

      } else {
        bool = false
      }

    }

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

object CacheGrindToTree {
  def main(args: Array[String]) {

    // args(0) is the file to treat

    val toTreat = new File(args(0))
    if (toTreat.exists()) {
      new CacheGrindToTree(toTreat).treat()
    }
  }

}

case class Cost(var line: Int, var level: Int, var nbExecs: Int = 1, selfDurationInMillis: Double, var durationInMillis: Double) {
  var children: java.util.List[MyKey] = new ArrayList[MyKey]()
  var parent: MyKey = null
}
case class MyKey(var rank: Int, nameFunction: String, fileOfFunction: String) extends Comparable[MyKey] {

  def compareTo(that: MyKey): Int = {
    this.toString().compareTo(that.toString)
  }
  override def hashCode(): Int = {
    toString().hashCode()
  }

  def toShortString(): String = {
    nameFunction + ":_:" + fileOfFunction + ":_:" + rank
  }

  override def toString(): String = {
    nameFunction + ":_:" + fileOfFunction + ":_:" + rank
  }

  def equals(that: MyKey): Boolean = {

    this.toString().equals(that.toString())
  }
}

case class StructReadProfile(line: String) {
    val dfs:DecimalFormatSymbols=new DecimalFormatSymbols(Locale.ENGLISH)
 // dfs.setGroupingSeparator('')
  dfs.setDecimalSeparator('.')
  val df:DecimalFormat=new DecimalFormat("#############0.###",dfs)
  val tabArray: Array[String] = line.split(";")
  val nameFunction = tabArray(1)
  val file_row = tabArray(2)
  val level = tabArray(3)
  var nbExec = tabArray(4).toInt
  // Durations are given in micro-second 

  var durationInMillis: Double = ((tabArray(5).toDouble.toLong.toDouble) / 1000).toDouble
  var selfDurationInMillis: Double = ((tabArray(6).toDouble.toLong.toDouble) / (1000L)).toDouble

  def mergeStruct(that: StructReadProfile) {
    nbExec += that.nbExec
    durationInMillis += that.durationInMillis
    selfDurationInMillis += that.selfDurationInMillis
  }
  def cloreStruct() {
    durationInMillis = ((durationInMillis / nbExec) * 1000).toLong.toDouble / 1000
    selfDurationInMillis = ((selfDurationInMillis / nbExec) * 1000).toLong.toDouble / 1000
  }
  def isTheSame(that: StructReadProfile): Boolean = {
    (nameFunction + file_row + level).equals(that.nameFunction + that.file_row + level)
  }
  def returnLineForCsvFile(): String = {
    ";" + nameFunction + ";" + file_row + ";" + level + ";" + nbExec + ";" + df.format(durationInMillis) + ";" + df.format(selfDurationInMillis) + ";"
  }

}