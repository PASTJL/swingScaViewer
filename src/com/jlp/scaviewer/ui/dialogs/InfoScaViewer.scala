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
package com.jlp.scaviewer.ui.dialogs
import scala.swing.GridBagPanel
import scala.swing.Dialog
import scala.swing.MainFrame
import scala.swing.TextArea
import scala.swing.ScrollPane
import java.awt.Dimension
import java.awt.Insets
import java.awt.Point
import java.io.File
import java.io.RandomAccessFile
import com.jlp.scaviewer.ui.Version
import java.awt.Font

class InfoScaViewer(win: MainFrame) extends Dialog(win) {
  val textTa = new TextArea()
  val scPane = new ScrollPane(textTa)
  this.contents = scPane
  textTa.text = "Current Version =" + Version.version + "\n"
  textTa.text += "########################################################\n"

  this.minimumSize = new Dimension(500, 300)
  this.preferredSize = new Dimension(500, 300)
  this.maximumSize = new Dimension(700, 400)
  var screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
  location = new Point((screenSize.width - size.width) / 2, (screenSize.height - size.height) / 2)

  this.resizable = true
  this.title = Version.version
  val evol = System.getProperty("root") + File.separator + "config" + File.separator + "evolutions.txt"
  var raf = new RandomAccessFile(evol, "r")
  var tabByte: Array[Byte] = Array.ofDim(raf.length.toInt)
  raf.readFully(tabByte)
  raf.close
  textTa.text += new String(tabByte)
  textTa.font = new Font("Arial", Font.BOLD, 12)
  textTa.peer.setCaretPosition(0);

  textTa.editable = false
  this.visible = true
}