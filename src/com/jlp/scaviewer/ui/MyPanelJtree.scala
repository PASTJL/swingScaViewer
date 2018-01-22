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

import scala.swing.Panel
import javax.swing.event.TreeSelectionListener
import javax.swing.event.TreeExpansionListener
import org.jfree.chart.ChartPanel
import scala.collection.mutable.WeakHashMap
import javax.swing.tree.TreePath
import scala.swing.GridBagPanel
import java.awt.GridBagConstraints
import scala.swing.Button
import java.awt.Font
import java.io.File
import javax.swing.ToolTipManager
import javax.swing.tree.DefaultTreeModel
import javax.swing.tree.TreeSelectionModel
import javax.swing.tree.MutableTreeNode
import javax.swing.event.TreeSelectionEvent
import javax.swing.JTree
import javax.swing.tree.TreeNode
import javax.swing.event.TreeExpansionEvent
import javax.swing.DropMode
import scala.swing.FlowPanel
import scala.swing.ScrollPane
import java.util.Collections
import javax.swing.tree.DefaultTreeCellRenderer

class MyPanelJtree(root: String, cp: ChartPanel) extends ScrollPane with TreeSelectionListener with TreeExpansionListener {

  var tree: MyJTree = null;

  //var cp: ChartPanel = null;
  var transferHandler: FileTransfertHandler = null;
  // DefaultMutableTreeNode root = new DefaultMutableTreeNode(file);
  var rootNode: ToolTipTreeNode = null;

  //	public MyPanelJTree(String root, ChartPanel cp) {
  //
  //		Vers l'objet companion
  //	}

  def addSubTree(nodeName: String, nodePrev: ToolTipTreeNode, lastPath: TreePath) {
    //JLP TODO

    var parent: ToolTipTreeNode = nodePrev.getParent().asInstanceOf[ToolTipTreeNode];
    var indx: Int = parent.getIndex(nodePrev);
    // System.out.println(" addSubTree : currentFolder="+nodeName);
    var file: MyFile = new MyFile(nodeName);

    // DefaultMutableTreeNode root = new DefaultMutableTreeNode(file);
    var root: ToolTipTreeNode = ToolTipTreeNode(file);
    root.setAllowsChildren(true);
    var myFileRoot = new MyFile(nodeName)

    root.setUserObject(myFileRoot);

    var files: Array[File] = file.listFiles()
    // System.out.println(" addSubTree :files.length="+files.length);
    // DefaultMutableTreeNode node;
    var node: ToolTipTreeNode = null
    var jj: Int = 0;
    //    for (j <- 0 until files.length) {
    //      
    //      println("add subTree file.name ="+files(j).getName)
    //    }
    for (j <- 0 until files.length) {

      // node = new DefaultMutableTreeNode(new
      // MyFile(files[j].getAbsolutePath()));
      // System.out.println(" addSubTree :file="+files[j].getAbsolutePath());
      node = ToolTipTreeNode(new MyFile(files(j).getAbsolutePath()));
      node.setUserObject(new MyFile(files(j).getAbsolutePath()));
      //println("file(j)="+files(j).getAbsolutePath())

      if (files(j).isFile() && files(j).getAbsolutePath().contains(".")
        && ScaCharting.suffixes
        .indexOf(files(j).getAbsolutePath()
          .substring(
            files(j).getAbsolutePath()
              .lastIndexOf('.'))) >= 0) {
        //        println("ScaCharting.suffixes="+ScaCharting.suffixes)
        //    	  println("AddsubTree inserting File ="+files(j).getName)
        root.insert(node, jj);
        jj += 1;
      }
      if (files(j).isDirectory()) {
        // System.out.println("ajout repertoire :"+files[j].getAbsolutePath());
        root.insert(node, jj);
        var tp = getPath(node);
        MyPanelJtree.hmTreePathBool.put(tp, false);
        jj += 1;
      }

    }

    // modif si jj >0
    if (jj > 0) {
      parent.remove(indx);
      parent.insert(root, indx);

      var model: DefaultTreeModel = new DefaultTreeModel(rootNode);

      tree.setModel(model);
      tree.repaint()

    }

  }
  def getPath(node: TreeNode): TreePath = {
    var tmpNode = node
    var list = new java.util.ArrayList[TreeNode]();
    // Add all nodjava.util.es to list
    while (tmpNode != null) {
      list.add(tmpNode);
      tmpNode = tmpNode.getParent();
    }
    Collections.reverse(list);
    // Convert array of nodes to TreePath
    return new TreePath(list.toArray());
  }

  def createTree1rstLevel(nodeName: String) {

    if (null != this.tree) {
      this.contents = null
      this.repaint();
      tree = null;

    }

    var currentFolder: String = nodeName;
    var file = new MyFile(currentFolder);
    file.isRoot=true
    rootNode = ToolTipTreeNode(file);
    
    rootNode.setUserObject(file)
    //    
    //    tree=new MyJTree(model)

    // DefaultMutableTreeNode root = new DefaultMutableTreeNode(file);

    var files: Array[File] = file.listFiles();

    // DefaultMutableTreeNode node;
    var node: ToolTipTreeNode = null;
    var jj = 0;
    //    for (j <- 0 until files.length) {
    //      
    //      println("file.name ="+files(j).getName)
    //    }
    for (j <- 0 until files.length) {

      // node = new DefaultMutableTreeNode(new
      // MyFile(files[j].getAbsolutePath()));
      node = ToolTipTreeNode(new MyFile(files(j).getAbsolutePath()));
      node.setUserObject(new MyFile(files(j).getAbsolutePath()));
      if (!files(j).isDirectory() && files(j).getAbsolutePath().contains(".")
        && ScaCharting.suffixes
        .indexOf(files(j).getAbsolutePath()
          .substring(
            files(j).getAbsolutePath()
              .lastIndexOf('.'))) >= 0) {

        rootNode.insert(node, jj);
        jj += 1;
      }
      if (files(j).isDirectory()) {
        rootNode.insert(node, jj);
        jj += 1;
      }

    }

    var mode: Int = TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION;
    var model: DefaultTreeModel = new DefaultTreeModel(rootNode);
    tree = new MyJTree(model);
    tree.setDragEnabled(true);
    tree.setFont(new Font("Arial", Font.BOLD, 12));
    // int mode = TreeSelectionModel.SINGLE_TREE_SELECTION;

    tree.getSelectionModel().setSelectionMode(mode);
    tree.setCellRenderer(new CustomIconRenderer());

    tree.setDropMode(DropMode.ON);

    MyPanelJtree.myTooltipManager.registerComponent(tree);
    tree.addTreeSelectionListener(this);
    tree.addTreeExpansionListener(this);

    var transferHandler = new FileTransfertHandler(cp);
    tree.setTransferHandler(transferHandler);

    tree.repaint()
  }

  def createTree() {

    var currentFolder: String = root;

    // dragFiles can be any local directory, ie,
    // a folder in the current directory
    // File file = new File(currentFolder, "dragFiles");
    var file = new MyFile(currentFolder);
    // DefaultMutableTreeNode root = new DefaultMutableTreeNode(file);
    file.isRoot=true
    var files: Array[File] = file.listFiles()

    // DefaultMutableTreeNode node;
    var node: ToolTipTreeNode = null;
    var jj: Int = 0;
    for (j <- 0 until files.length) {

      // node = new DefaultMutableTreeNode(new
      // MyFile(files[j].getAbsolutePath()));
      node = ToolTipTreeNode(new MyFile(files(j).getAbsolutePath()));
      //node.setUserObject(new MyFile(files(j).getAbsolutePath()));
      node.setUserObject(new MyFile(files(j).getAbsolutePath).toString);
      if (files(j).isFile()
        && ScaCharting.suffixes
        .indexOf(files(j).getAbsolutePath()
          .substring(
            files(j).getAbsolutePath()
              .lastIndexOf('.'))) >= 0) {

        rootNode.insert(node, jj);
        jj += 1;
      }
      if (files(j).isDirectory()) {
        rootNode.insert(exploreNode(files(j), node), jj);
        jj += 1;
      }
    }
    var model: DefaultTreeModel = new DefaultTreeModel(rootNode);
    // int mode = TreeSelectionModel.SINGLE_TREE_SELECTION;
    var mode: Int = TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION;
    tree = new MyJTree(model);

    tree.getSelectionModel().setSelectionMode(mode);

  }

  private def exploreNode(folder: File, parent: MutableTreeNode): MutableTreeNode = {
    var files: Array[MyFile] = folder.listFiles().asInstanceOf[Array[MyFile]];
    var node: ToolTipTreeNode = null;
    var jj: Int = 0;

    for (j <- 0 until files.length) {

      // node = new DefaultMutableTreeNode(new
      // MyFile(files[j].getAbsolutePath()).toString());
      node = ToolTipTreeNode(new MyFile(files(j).getAbsolutePath()));
      node.setUserObject(new MyFile(files(j).getAbsolutePath()));

      if (files(j).isFile()
        && ScaCharting.suffixes
        .indexOf(files(j).getAbsolutePath()
          .substring(
            files(j).getAbsolutePath()
              .lastIndexOf('.'))) >= 0) {

        parent.insert(node, jj);

        jj += 1;
      }
      if (files(j).isDirectory()) {

        exploreNode(files(j), node);
        parent.insert(node, jj);
        jj += 1;
      }
    }

    return parent;
  }

  // not used
  private def getFileNames(parent: File): Array[String] = {
    var files: Array[File] = parent.listFiles();
    var names: Array[String] = null;
    if (files != null) {
      names = new Array[String](files.length);
      var count = 0;
      for (f <- files)
        names(count) = f.getName();
      count += 1
    }
    return names;
  }

  //	public static void main(String[] args) {
  //		// new MyPanelJTree(args[0]);
  //	}

  override def valueChanged(e: TreeSelectionEvent) {
    // TODO Auto-generated method stub

    var node: ToolTipTreeNode = tree
      .getLastSelectedPathComponent().asInstanceOf[ToolTipTreeNode];
    if (null == node) {
      return ;
    }
    var mf: MyFile = node.getUserObject().asInstanceOf[MyFile];
    var lastPath: TreePath = tree.getSelectionPath();
    // System.out.println("noeud selectionne ="+mf.getAbsolutePath());
    if (null != mf && mf.isDirectory()
      && !MyPanelJtree.hmTreePathBool.contains(lastPath)) {
      // On l'inserre en position ferme
      MyPanelJtree.hmTreePathBool.put(lastPath, false);
    }
    if (null != mf && mf.isDirectory()
      && node.getChildCount() == 0) {
      // JLP Test
      // ScaCharting.listFiles=List.empty

      // System.out.println(" Avant noeud lastPath= ="+lastPath.toString());
      // On tente d'en trouver
      tree.removeTreeSelectionListener(this);
      // System.out.println(" Avant noeud selectionne ="+mf.getAbsolutePath());

      addSubTree(mf.getAbsolutePath(), node, lastPath);

      // System.out.println(" Apres noeud selectionne ="+mf.getAbsolutePath());
      tree.addTreeSelectionListener(this);

    }
    if (null != mf && mf.isDirectory()) {
      expandAllNew(tree, true);
      // JLP
      // ScaCharting.listFiles=List.empty
    }

    if (null != mf && mf.isFile()) {
      // traitement de la liste

      var tabPath: Array[TreePath] = e.getPaths();

      if (tabPath == null || tabPath.length == 0) {

        // System.out.println("tabPath is null or 0 len");
        return ;
      }
      var len = tabPath.length
      for (i <- 0 until len) {

        var myF = (tabPath(i).getLastPathComponent().asInstanceOf[ToolTipTreeNode]).getUserObject().asInstanceOf[MyFile];

        if (e.isAddedPath(tabPath(i))) {

          if (myF.isFile()
            && ScaCharting.suffixes.indexOf(myF.getName
              .substring(myF.getName.lastIndexOf('.'))) >= 0) {
            // on ajoute s'il n'y est pas deja
            if (!ScaCharting.listFiles.contains(myF)) {
              //JLP Test
              // ScaCharting.listFiles =  ScaCharting.listFiles :+ myF
              // System.out.println( "len = "+
              // len+" ;CompositeChannel : Ajout File :"+myF.shortName);
            }
          }
        } else {

          if (ScaCharting.listFiles.contains(myF)) {
            // ScaCharting.listFiles =ScaCharting.listFiles filterNot( _ == myF)

            // System.out.println( "len = "+
            // len+" ;CompositeChannel : suppression File :"+myF.shortName);
          }

        }
      }

    }

    tree.repaint();
  }

  private def expandAllNew(tree: MyJTree, parent: TreePath, expand: Boolean): Unit = {
    // Traverse children
    var node: TreeNode = parent.getLastPathComponent().asInstanceOf[TreeNode];

    if (node.getChildCount() >= 0) {
      var e: java.util.Enumeration[_] = node.children()
      while (e.hasMoreElements()) {
        var n: TreeNode = e.nextElement().asInstanceOf[TreeNode];
        var path: TreePath = parent.pathByAddingChild(n);
        // expandAllNew(tree, path, expand);
        if (!MyPanelJtree.hmTreePathBool.contains(path)) {
          MyPanelJtree.hmTreePathBool.put(path, true);
        }
        expandAllNew(tree, path, MyPanelJtree.hmTreePathBool.get(path).getOrElse(true));
      }
    }

    // Expansion or collapse must be done bottom-up
    if (expand) {
      tree.expandPath(parent);

      MyPanelJtree.hmTreePathBool.put(parent, true);
      tree.fireTreeExpanded(parent);
    } else {

      MyPanelJtree.hmTreePathBool.put(parent, false);
      tree.collapsePath(parent);
      tree.fireTreeCollapsed(parent);
    }
  }

  def expandAllNew(tree: MyJTree, expand: Boolean): Unit = {
    var root: TreeNode = tree.getModel().getRoot().asInstanceOf[TreeNode];
    var parent: TreePath = new TreePath(root);
    if (!MyPanelJtree.hmTreePathBool.contains(parent)) {
      MyPanelJtree.hmTreePathBool.put(parent, true);
    }
    // Traverse tree from root
    expandAllNew(tree, parent, true);
  }

  override def treeExpanded(event: TreeExpansionEvent) {
    var parent: TreePath = event.getPath();
    // System.out.println("Expending :" + parent.toString());
    MyPanelJtree.hmTreePathBool.put(parent, true);

  }

  override def treeCollapsed(event: TreeExpansionEvent) {
    var parent: TreePath = event.getPath();
    // System.out.println("Collapsing :" + parent.toString());
    MyPanelJtree.hmTreePathBool.put(parent, false);

  }

}
object MyPanelJtree {
  val myTooltipManager = ToolTipManager
    .sharedInstance();

  val hmTreePathBool = new WeakHashMap[TreePath, Boolean]();
  val jbRoot: Button = new Button("New root");
  def apply(root: String, cp: ChartPanel): MyPanelJtree = {
    val mpjt = new MyPanelJtree(root, cp)

    ScaCharting.listFiles = Nil;

    hmTreePathBool.clear();

    var file: MyFile = new MyFile(root);
    mpjt.rootNode = ToolTipTreeNode(file);

    // mpjt.tree = mpjt.createTree();
    mpjt.createTree1rstLevel(root);

    var fl = new FlowPanel()
    fl.peer.add(mpjt.tree)
    mpjt.contents = fl;
    mpjt

  }

}