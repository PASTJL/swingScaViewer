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
package com.jlp.scaviewer.ui
import java.awt.datatransfer.Transferable
import java.io.File
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.UnsupportedFlavorException

class FilesTransferable(lstFile: java.util.List[File]) extends Transferable {

  // var tabDataFlavor: Array[DataFlavor] = new Array[DataFlavor](3);

  try {
    FilesTransferable.localFileFlavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType +
      ";class=java.io.File");
    FilesTransferable.javaFileListFlavor = DataFlavor.javaFileListFlavor
  } catch {
    // TODO Auto-generated catch block
    case e: ClassNotFoundException => e.printStackTrace();
  }

  //@Override
  @throws(classOf[java.io.IOException])
  @throws(classOf[UnsupportedFlavorException])
  override def getTransferData(flavor: DataFlavor): Object =

    {
      //  println("flavor in =" + flavor.toString)
      if (flavor.equals(java.awt.datatransfer.DataFlavor.javaFileListFlavor) || flavor.equals(FilesTransferable.localFileFlavor)) {
        //   println(" dataflavor detected:" + flavor.toString)
        return lstFile;
      } else {
        //  println("no dataflavor detected")
        null
      }

      // TODO Auto-generated method stub

    }

  override def getTransferDataFlavors(): Array[DataFlavor] = {
    try {
      FilesTransferable.localFileFlavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType +
        ";class=java.io.File");
    } catch {
      // TODO Auto-generated catch block
      case e: ClassNotFoundException => e.printStackTrace();
    }
    var arr = new Array[DataFlavor](2)
    arr(0) = java.awt.datatransfer.DataFlavor.javaFileListFlavor
    arr(1) = FilesTransferable.localFileFlavor
    arr
    //{java.awt.datatransfer.DataFlavor.javaFileListFlavor,localFileFlavor};
  }

  override def isDataFlavorSupported(flavor: DataFlavor): Boolean = {
    if (flavor.equals(java.awt.datatransfer.DataFlavor.javaFileListFlavor) || flavor.equals(FilesTransferable.localFileFlavor)) {
      //println("transfert OK flavor =" + flavor.toString())
      true
    } else {
      // println("transfert KO flavor =" + flavor.toString())
      false
    }
  }

}
object FilesTransferable {
  var localFileFlavor: DataFlavor = null;
  var javaFileListFlavor: DataFlavor = null;
  def apply(lstFile: java.util.List[File]) {

    new FilesTransferable(lstFile)
  }
}