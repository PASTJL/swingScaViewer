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
import scala.util.matching.Regex
import java.io.File
import java.io.RandomAccessFile
import java.util.zip.GZIPOutputStream
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.InputStreamReader
import java.io.FileNotFoundException
import java.io.IOException
import java.io.InputStream
import language.postfixOps

final object SearchDirFile {
  var BUFFER_SIZE = 10 * 1024
  def searchYoungestDir(prefix: String, reg: Regex): File =
    {
      val root = new File(prefix)
      val list = List(root.listFiles(): _*) filter (file => file.isDirectory && (reg.findFirstIn(file.getName) isDefined))
      list.length match {
        case 0 => null
        case 1 => list(0)
        case _ => (list sortWith (yt(_, _))).head
      }

    }
  def searchOldestDir(prefix: String, reg: Regex): File =
    {
      val root = new File(prefix)
      val list = List(root.listFiles(): _*) filter (file => file.isDirectory && (reg.findFirstIn(file.getName) isDefined))
      list.length match {
        case 0 => null
        case 1 => list(0)
        case _ => (list sortWith (yt(_, _))).reverse.head

      }
    }

  def searchYoungestFile(prefix: String, reg: Regex): File =
    {
      val root = new File(prefix)
      val list = List(root.listFiles(): _*) filter (file => file.isFile && (reg.findFirstIn(file.getName) isDefined))
      list.length match {
        case 0 => null
        case 1 => list(0)
        case _ => (list sortWith (yt(_, _))).head

      }

    }
  def searchOldestFile(prefix: String, reg: Regex): File =
    {
      val root = new File(prefix)
      val list = List(root.listFiles(): _*) filter (file => file.isFile && (reg.findFirstIn(file.getName) isDefined))
      list.length match {
        case 0 => null
        case 1 => list(0)
        case _ => (list sortWith (yt(_, _))).reverse.head

      }
    }

  def recursiveListFiles(f: File): List[File] = {
    val these = f.listFiles.toList
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles).toList
  }

  def yt(f1: File, f2: File): Boolean =
    {
      f1.lastModified > f2.lastModified
    }

  def isText(file: File, acceptUTF8encoding: Boolean): Boolean =
    {

      var isBoolText = true
      var buffer: Array[Byte] = Array.ofDim(BUFFER_SIZE)
      val reader = initReader(file);

      try {

        val read = reader.read(buffer);
        var lastByteTranslated = 0;
        // for (int i = 0; i < read && isText; i++)
        var i = 0;
        while (i < read && isBoolText) {
          val b = buffer(i)
          var ub = b & (0xff) // unsigned
          var utf8value = lastByteTranslated + ub
          lastByteTranslated = (ub) << 8;

          if (ub == 0x09 /*(tab)*/
            || ub == 0x0A /*(line feed)*/
            || ub == 0x0C /*(form feed)*/
            || ub == 0x0D /*(carriage return)*/
            || (ub >= 0x20 && ub <= 0x7E) /* Letters, Numbers and other "normal synbols" */
            || (ub >= 0xA0 && ub <= 0xEE) /* Symbols of Latin-1 */
            || (acceptUTF8encoding && (utf8value >= 0x2E2E && utf8value <= 0xC3BF)) /* Latin-1 in UTF-8 encoding */ ) {
            // ok
          } else {
            isBoolText = false;
          }
          i += 1
        }
      } finally {
        try {
          reader.close();
        } finally {

        }
      }

      isBoolText;
    }
  private def initReader(file: File): InputStream =
    {

      try {
        if (file.getName().endsWith(".gz")) {

          return new BufferedInputStream(
            new GZIPInputStream(new FileInputStream(file)),
            BUFFER_SIZE);

        } else {
          return new BufferedInputStream(
            new FileInputStream(file), BUFFER_SIZE);

        }
      } catch {
        case e: FileNotFoundException => e.printStackTrace()
        case e: IOException => e.printStackTrace()
      }
      return null;
      //JLP
    }
  def deleteDir(dir: File): Unit =
    {
      if (!dir.isDirectory()) {
        throw new IOException("Not a directory " + dir);
      }

      val files = dir.listFiles();
      for (file <- files) {

        if (file.isDirectory()) {
          deleteDir(file);
        } else {
          val deleted = file.delete();
          if (!deleted) {
            throw new IOException("Unable to delete file" + file);
          }
        }
      }

      dir.delete();
    }
  def main(arrs: Array[String]) {
    println("youngest=" + searchYoungestDir("D:\\eclipse\\workspace\\scaViewer\\workspaceScaViewer\\projet0", """^test_"""r))
    println("oldest=" + searchOldestDir("D:\\eclipse\\workspace\\scaViewer\\workspaceScaViewer\\projet0", """^test_"""r))
    println("youngestFile=" + searchYoungestFile("D:\\eclipse\\workspace\\scaViewer\\workspaceScaViewer\\projet0\\test_20120128\\logs", """.*"""r))
    println("oldestFile=" + searchOldestFile("D:\\eclipse\\workspace\\scaViewer\\workspaceScaViewer\\projet0\\test_20120128\\logs", """.*"""r))
  }
}
