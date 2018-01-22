package com.jlp.scaviewer.tools

import java.text.SimpleDateFormat

case class LineSortable(line: String, tabRegex: Array[(String, String)], tupDate: (String, String) = ("", ""), rankDate: Int = -1) {

  val strDate = {
    if (rankDate >= 0 && tupDate._1.length > 0 && tupDate._2.length > 0) {
      val ext1 = tupDate._1.r.findFirstIn(line)
      if (None != ext1) {
        val sdf = new SimpleDateFormat(tupDate._2)
        sdf.parse(ext1.get).getTime().toString
      } else {
        ""
      }
    } else {
      ""
    }
  }
  val comparator: String = {
    var strComp: String = ""
    var idx = 0
    if (rankDate < 0) {
      // La date ne fait pas partie du comparateur

      tabRegex foreach { tubReg =>
        //	for(tubReg <-tabRegex ){
        if ("" != tubReg._1) {
          val ext1 = tubReg._1.r.findFirstIn(line)
          if (None != ext1) {
            if (tubReg._2.length > 0) {
              // 2 regexp
              val ext2 = tubReg._2.r.findFirstIn(ext1.get)
              if (None != ext2) {
                if (strComp == "") {
                  strComp = ext2.get
                } else {
                  strComp += "_" + ext2.get
                }
              }
            } else {
              // une seule regexp
              if (strComp == "") {
                strComp = ext1.get
              } else {
                strComp += "_" + ext1.get
              }
            }

          } else {
            // Pas de regexp
            strComp = ""
          }

        }
      }

    } else if (null != tupDate && tupDate._1.length > 0 && tupDate._2.length() > 0) {
      // la date fait partie du comparateur

      // Le premier champ a la valeur 0

      tabRegex foreach { tubReg =>
        //	for(tubReg <-tabRegex ){
        if (idx == this.rankDate) {
          if (strComp == "") {
            strComp = strDate
          } else {
            strComp += "_" + strDate
          }
        }
        if ("" != tubReg._1) {
          val ext1 = tubReg._1.r.findFirstIn(line)
          if (None != ext1) {
            if (tubReg._2.length > 0) {
              // 2 regexp
              val ext2 = tubReg._2.r.findFirstIn(ext1.get)
              if (None != ext2) {
                if (strComp == "") {
                  strComp = ext2.get
                } else {
                  strComp += "_" + ext2.get
                }
              }
            } else {
              // une seule regexp
              if (strComp == "") {
                strComp = ext1.get
              } else {
                strComp += "_" + ext1.get
              }
            }

          } else {
            // Pas de regexp

          }
          idx += 1
        }

      }
    }

    if (idx == this.rankDate) {
      if (strComp == "") {
        strComp = strDate
      } else {
        strComp += "_" + strDate
      }
    }

    strComp
  }

  def compareTo(that: LineSortable): Int = {

    this.comparator.compareTo(that.comparator)
  }
  def lt(that: LineSortable): Boolean = {

    if (this.compareTo(that) < 0) {
      //println(this.comparator + "<" + that.comparator)
      true
    } else {
      // println(this.comparator + ">" + that.comparator)
      false
    }
  }

}