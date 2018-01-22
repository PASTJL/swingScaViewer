package com.jlp.scaviewer.ui
import javax.swing.tree.DefaultMutableTreeNode
import java.io.File
import java.io.RandomAccessFile
import java.io.FileNotFoundException
import java.io.IOException

class ToolTipTreeNode extends DefaultMutableTreeNode {
  var toolTipText: String = "";
 
  var booldir: Boolean = false;
 var file:File=null
  def isBooldir():Boolean= {
     booldir;
  }
  def getToolTipText(): String = {
    toolTipText;
  }
}
object ToolTipTreeNode {
  def apply(file: File) =
    {
      var tttn = new ToolTipTreeNode()
      tttn.file=file
      if (file.isFile()) {
        tttn.booldir = false;
        var raf: RandomAccessFile = null
        try {
          raf = new RandomAccessFile(file, "r");
          // lecture premiere ligne de titre
          tttn.toolTipText = raf.readLine();

        } catch {
          // TODO Auto-generated catch block
          case e: FileNotFoundException => println("FileNotFoundException caught: " + e);
          case e: IOException => println("IOException caught: " + e);

        } finally {
          if (null != raf)
            raf.close()
        }
      } else {
        tttn.booldir = true;
      }
      tttn

    }
}