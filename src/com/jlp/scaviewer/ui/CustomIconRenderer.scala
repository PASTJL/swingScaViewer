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
import javax.swing.tree.DefaultTreeCellRenderer
import javax.swing.ImageIcon
import javax.swing.JTree
import java.awt.Component
import java.io.File
import javax.swing.tree.DefaultMutableTreeNode

class CustomIconRenderer
  extends DefaultTreeCellRenderer {

  var directoryIcon = this.getOpenIcon().asInstanceOf[ImageIcon];
  var leafIcon1= this.getLeafIcon()
  override def getTreeCellRendererComponent(tree: JTree, value: Object, sel: Boolean, expanded: Boolean, leaf: Boolean, row: Int, hasFocus: Boolean): Component = {

    var com=super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);

    var nodeObj: Object = (value.asInstanceOf[ToolTipTreeNode]).getUserObject();

    // check whatever you need to on the node user object
    if (null == nodeObj) {
     // println("nodeObj is null")
      this
    } else {
      if ((nodeObj.asInstanceOf[File]).isDirectory()) {
    	// println("nodeObj is directory") 
        setIcon(directoryIcon);

      } else {
	 //println("nodeObj is file") 
        setIcon(leafIcon1);

      }

       this
    }

  }

}