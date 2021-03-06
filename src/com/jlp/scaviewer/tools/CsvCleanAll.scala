package com.jlp.scaviewer.tools

import com.jlp.scaviewer.ui.SwingScaViewer
import javax.swing.JOptionPane
import java.io.File
import com.jlp.scaviewer.commons.utils.SearchDirFile
import scala.collection.JavaConverters
object CsvCleanAll {

  def clean() {
    println("Begin compacting all csv directories")
    val deb = System.currentTimeMillis()
    SwingScaViewer.miCsvCleanAll.enabled = false
    // Nettoyage du csv current
    val regex = """(_|-)?\d{8}(_|-)\d{6}$""".r
    //Trouver tous les projets

    val pathsProjs = new File(System.getProperty("workspace")).listFiles() filter (file => file.isDirectory)
    pathsProjs foreach { proj =>

      // trouver tous les scenarios
      val scenarios = proj.listFiles() filter (file => file.isDirectory() && file.getName().startsWith(SwingScaViewer.pref))
      if (null != scenarios) {
        scenarios foreach { scenario =>

          val csv = new File(scenario.getAbsolutePath() + File.separator + "csv")
          // trouver tous les prefix en enlevant la date de format :
          // _20130110_082311
          // recuperer tous les repertoires dont le prefix est au moins en doublon

          val dirs = csv.listFiles filter (_.isDirectory()) groupBy { file =>
            val suffix = regex.findFirstIn(file.getName()).getOrElse("")
            file.getName().substring(0, file.getName().indexOf(suffix))
          } filter { tub => tub._2.length > 1 }
          dirs foreach { tup =>
            println("tup._1=" + tup._1)
            val pathFile = csv.getAbsolutePath() + File.separator
            val reg = (tup._1 + """.*""").r
            val mostYoung = SearchDirFile.searchYoungestDir(pathFile, reg)
            tup._2 foreach { rep =>
              if (rep != mostYoung) {
                // controle que le repertoire contient allAverage.csv, creation locale
                // evite des destructions de repertoires telecharges avec un ordre inconnu
                val files = rep.listFiles() map (file => file.getName()) filter (_ == "allAverages.csv")
                if (files.length == 1) {
                  removeDirectory(rep)
                }
              }

            }

          }
        }
      }

    }
    println("End compacting all csv directories duration=" + (System.currentTimeMillis() - deb) + " ms")
    SwingScaViewer.miCsvCleanAll.enabled = true
  }

  def removeDirectory(directory: File): Boolean = {

    // System.out.println("removeDirectory " + directory);

    if (directory == null)
      false
    if (!directory.exists())
      true
    if (!directory.isDirectory())
      false

    val list: Array[String] = directory.list();

    if (list != null) {
      for (i <- 0 until list.length) {
        var entry: File = new File(directory, list(i));
        if (entry.isDirectory()) {
          if (!removeDirectory(entry))
            false
        } else {
          if (!entry.delete())
            false
        }
      }
    }

    directory.delete();
  }

}