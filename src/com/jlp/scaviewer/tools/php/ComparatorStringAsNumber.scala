package com.jlp.scaviewer.tools.php

import java.util.Comparator
import java.text.DecimalFormatSymbols
import java.util.Locale
import java.text.DecimalFormat

class ComparatorStringAsLong extends Comparator[String] {
  

  def compare(o1: String, o2: String): Int = {
    o1.toLong.compare(o2.toLong)
  }

  def equals(that: String): Boolean = {
    this.equals(that)
  }

}
class ComparatorStringAsDouble extends Comparator[String] {
val dfs:DecimalFormatSymbols=new DecimalFormatSymbols(Locale.ENGLISH)
  dfs.setGroupingSeparator(' ')
  dfs.setDecimalSeparator('.')
  val df:DecimalFormat=new DecimalFormat("##############.###",dfs)
  df.setGroupingUsed(true)
  df.setGroupingSize(3)
  def compare(o1: String, o2: String): Int = {
    df.parse(o1).doubleValue().compare(df.parse(o2).doubleValue())
  }

  def equals(that: String): Boolean = {
    this.equals(that)
  }

}