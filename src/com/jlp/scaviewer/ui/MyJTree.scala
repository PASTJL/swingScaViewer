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
import java.awt.event.MouseEvent;

import javax.swing.JTree;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
class MyJTree(model: DefaultTreeModel) extends JTree(model) {

  override def getToolTipText(evt:MouseEvent ):String= {
		if (getRowForLocation(evt.getX(), evt.getY()) == -1)
		  {null		
		  }
		else
		{
		 
		val curPath:TreePath  = getPathForLocation(evt.getX(), evt.getY())
		curPath.getLastPathComponent().asInstanceOf[ToolTipTreeNode]
				.getToolTipText();
		 }
	}
  
}

