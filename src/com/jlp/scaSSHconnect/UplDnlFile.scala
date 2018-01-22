package com.jlp.scaSSHconnect

sealed class UplDnlFile
case class UploadFile(cmdRank: Int, var idServer: String,
  localFile: String, var remoteFile: String,
  execute: String) extends UplDnlFile
case class DownloadFile(cmdRank: Int, var idServer: String, var typeFileOrDir: String,
  var patternFiles: String, var howmany: Int, target: String, var compress: String) extends UplDnlFile