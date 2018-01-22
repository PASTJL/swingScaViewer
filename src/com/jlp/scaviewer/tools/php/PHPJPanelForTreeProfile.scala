package com.jlp.scaviewer.tools.php

import javax.swing.JPanel
import javax.swing._
import javax.swing.tree._
import javax.swing.event._
import java.awt._
import java.io.File
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.io.RandomAccessFile
import scala.collection.mutable.HashMap
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import javax.swing.table.DefaultTableModel
import javax.swing.table.TableRowSorter
import scala.collection.mutable.ListBuffer
import java.awt.event.FocusListener
import java.awt.event.FocusEvent
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import com.jlp.scaviewer.ui.SwingScaViewer
import java.util.ArrayList
import java.text.DecimalFormatSymbols
import java.util.Locale
import java.text.DecimalFormat

//class PHPJPanelForTreeProfile(toShow: File) extends JPanel with MouseListener {
  //with ActionListener
class PHPJPanelForTreeProfile(toShow: File)  extends scala.swing.Panel with MouseListener {
   val fontRow = new Font("Arial", Font.BOLD, 12)
  val fontHeader = new Font("Arial", Font.BOLD, 13)
  
  
  // HashMap to construct the JTree Int is the level
  val hmLevels: scala.collection.mutable.HashMap[Int, MyNodeForTreeProfile] = new HashMap()

  //HashMap to construct the Table1 Int is the rank of the function
  val hmTable1: scala.collection.mutable.HashMap[Int, MyNodeForTreeProfile] = new HashMap()

  //"rank", "nameFunction", "fileNameRow", "level", "nbExec", "durationInMillis", "selfDurationInMillis"
  val titleTable1: Array[Object] = Array("rank", "nameFunction", "fileNameRow", "level", "nbExec", "durationInMillis", "selfDurationInMillis")

  // HashMap to construct the Table2 ( concatenation of fonctions)  String  is nameFunction + fileNameRow
  val hmTable2: scala.collection.mutable.HashMap[String, MyNodeForTreeProfile] = new HashMap()
  //"nameFunction", "fileNameRow", "nbExec", "durationInMillis", "selfDurationInMillis"
  val titleTable2: Array[Object] = Array("nameFunction", "fileNameRow", "nbExec", "durationInMillis", "selfDurationInMillis")

  // Definition d un root 
  val root = MyNodeForTreeFactoryProfile(toShow)
  hmLevels.put(-1, root) // for the root of the Jtree

  // Construction de l'arbre
  val dfs:DecimalFormatSymbols=new DecimalFormatSymbols(Locale.ENGLISH)
  dfs.setGroupingSeparator(' ')
  dfs.setDecimalSeparator('.')
  val df:DecimalFormat=new DecimalFormat("##############.###",dfs)
  df.setGroupingUsed(true)
  df.setGroupingSize(3)
  val df1:DecimalFormat=new DecimalFormat("##########0.000",dfs)
  df1.setGroupingUsed(true)
  df1.setGroupingSize(3)
  val df2=new DecimalFormat("##0.000E0",dfs)
  df2.setGroupingUsed(true)
  df2.setGroupingSize(3)
  
 
 
 
  var BUFFER_SIZE = 10 * 1024*1024
    private def initReader(file: File): BufferedReader =
    {

      if (file.getName().endsWith(".gz")) {
        
        return new BufferedReader(new InputStreamReader(
          new GZIPInputStream(new FileInputStream(file))),
          BUFFER_SIZE);

      } else {
       
        return new BufferedReader(new InputStreamReader(
          new FileInputStream(file)), BUFFER_SIZE);

      }

    }
  val tmpreader=initReader(toShow)
  // lecture de la premiere ligne => titre
   var line = tmpreader.readLine()
   
 
  println("treatement of file : " + toShow + " with title :\n" + line)
 
   var bool = true
  while (bool) {
    line = tmpreader.readLine()
   
    if (null != line) {
      
      val newNode = MyNodeForTreeFactoryProfile(line)
      
      hmTable1.put(newNode.rank, newNode)
      if (hmTable2.contains(newNode.nameFunction + newNode.fileNameRow)) {
        val tmp1Node = hmTable2.get(newNode.nameFunction + newNode.fileNameRow).get
        val concatNode = createConcatNode(tmp1Node, newNode)
        hmTable2.put(newNode.nameFunction + newNode.fileNameRow, concatNode)
      } else {
        hmTable2.put(newNode.nameFunction + newNode.fileNameRow, newNode)
      }

      val level = newNode.level
      hmLevels.put(level, newNode)
      val parent = hmLevels.get(level - 1).get
      parent.add(newNode)
      parent.idsChildren.add(newNode.rank)
      newNode.idParent=parent.rank
      newNode.nameParent=parent.nameFunction

    } else {
      bool = false
    }

  }
  tmpreader.close()
//  println("taille hmLevels=" + hmLevels.size)
  val tree: JTree = new JTree(root)
  tree.setFont(new Font("Arial", Font.PLAIN, 13))

  // Construction data Jtabl1
  val datas: Array[Array[Object]] = Array.ofDim(hmTable1.size, titleTable1.size)
  // Filling datas
  //println("hmTable1.size="+hmTable1.size)
  hmTable1.foreach { tup =>
    
    datas(tup._1) = returnArrayForTable1(tup._2)

  }
  // construction datas pour table 2
  val datas2: Array[Array[Object]] = Array.ofDim(hmTable2.size, titleTable2.size)
  // Filling datas
  var i: Int = 0
  hmTable2 foreach { tup =>

    datas2(i) = returnArrayForTable2(tup._2)
    i += 1

  }

  class MyTableModel(data: Array[Array[Object]], title: Array[Object]) extends DefaultTableModel(data, title) {

    override def isCellEditable(row: Int, col: Int): Boolean = false
  }

  val model1: MyTableModel = new MyTableModel(datas, titleTable1)
  val model2: MyTableModel = new MyTableModel(datas2, titleTable2)
  val table1: JTable = new JTable(model1)
  val table2: JTable = new JTable(model2)
  val sorter1 = new TableRowSorter(model1);
  val sorter2 = new TableRowSorter(model2);
  // table1 =>"rank", "nameFunction", "fileNameRow", "level", "nbExec", "durationInMillis", "selfDurationInMillis"
  sorter1.setComparator(0, new ComparatorStringAsLong())
  sorter1.setComparator(3, new ComparatorStringAsLong())
  sorter1.setComparator(4, new ComparatorStringAsLong())
  sorter1.setComparator(5, new ComparatorStringAsDouble())
  sorter1.setComparator(6, new ComparatorStringAsDouble())
    table1.setRowSorter(sorter1)

  // table2 => "nameFunction", "fileNameRow", "nbExec", "durationInMillis", "selfDurationInMillis"
  sorter2.setComparator(2, new ComparatorStringAsLong())
  sorter2.setComparator(3, new ComparatorStringAsDouble())
  sorter2.setComparator(4, new ComparatorStringAsDouble())
  table2.setRowSorter(sorter2)
  table1.setFont(new Font("Arial", Font.BOLD, 13))
  table2.setFont(new Font("Arial", Font.BOLD, 13))
  val menuJPanel: JPanel = new JPanel()

  // size to 0 column rank
  var idx=table1.getColumnModel().getColumnIndex("rank")
 table1.getColumnModel().getColumn(idx).setMinWidth(0);
   table1.getColumnModel().getColumn(idx).setMaxWidth(0);
   table1.getColumnModel().getColumn(idx).setWidth(0);
  
 // size to 0 column level
   idx=table1.getColumnModel().getColumnIndex("level")
 table1.getColumnModel().getColumn(idx).setMinWidth(0);
   table1.getColumnModel().getColumn(idx).setMaxWidth(0);
   table1.getColumnModel().getColumn(idx).setWidth(0);
  
  
  tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
 tree.setFont(new Font("Arial", Font.BOLD, 13))
  table1.getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
 

  table1.setBackground(new Color(202, 255, 216))
  table2.setBackground(new Color(241, 247, 176))

  val jLabel = new JLabel("Current View: Duration")
  val jLabelDesc1 = new JLabel("rank:nameFunction ( nbExec , durationInMillis , selfDurationInMillis )")

  jLabel.setFont(new Font("Arial", Font.BOLD, 15))
  jLabel.setForeground(Color.RED)
  jLabelDesc1.setFont(new Font("Arial", Font.BOLD, 12))

//  menuJPanel.setMaximumSize(new Dimension(40, 2000))
//  menuJPanel.setMinimumSize(new Dimension(30, 800))
//  val jbutMemory: JButton = new JButton("Memory View")
//
//  val jbutDuration: JButton = new JButton("Duration View")
//  jbutDuration.setMinimumSize(new Dimension(20, 80))
//  jbutMemory.setMinimumSize(new Dimension(20, 80))

  var gbl = new GridBagLayout()
  var gbc = new GridBagConstraints()

  menuJPanel.setLayout(gbl)
  gbc.fill = GridBagConstraints.BOTH
  val ins = new Insets(5, 20, 5, 20)

  gbc.insets = ins
  gbc.gridx = 0
  gbc.gridy = 0
  menuJPanel.add(jLabel, gbc)

//  gbc.gridx = 1
//  menuJPanel.add(jbutDuration, gbc)
//  gbc.gridx = 2
//  menuJPanel.add(jbutMemory, gbc)

  gbc.gridx = 0
  gbc.gridy = 1
  gbc.gridwidth = 3
  menuJPanel.add(jLabelDesc1, gbc)
  //tree.addMouseListener(this)
  val treeSelectionListener: TreeSelectionListener = new TreeSelectionListener() {
    def valueChanged(e: TreeSelectionEvent) {
      val node: MyNodeForTreeProfile =
        tree.getLastSelectedPathComponent().asInstanceOf[MyNodeForTreeProfile]

      //   val selectedNode: MyNodeForTree =    e.getNewLeadSelectionPath().getLastPathComponent().asInstanceOf[MyNodeForTree]

      val model: DefaultTreeModel = tree.getModel().asInstanceOf[DefaultTreeModel]
      model.nodeChanged(node);

      // put the selection on table1
      
      val rankSelected = { if (node.rank>=0) node.rank else -1}
     
      if (rankSelected >= 0) {
        // trouver rankSelected dan sla premiere colonne

        val columRank = table1.getColumnModel().getColumnIndex("rank")

        var bool: Boolean = true
        var rowSelected = -1
        var row = 0
        
        while (bool && row < table1.getRowCount()) {
          val tmpValue = table1.getValueAt(row, columRank).asInstanceOf[String].toInt
          if (rankSelected == tmpValue) {
            rowSelected = row
            bool = false
          }
          row += 1
        }

        if (rowSelected >= 0) {
          table1.setRowSelectionInterval(rowSelected, rowSelected)
          table1.scrollRectToVisible(new Rectangle(table1.getCellRect(rowSelected, 0, true)));
        }
      }
    }
    
  }
  tree.addTreeSelectionListener(treeSelectionListener)

  def getPaths(tree: JTree, parent: TreePath, expanded: Boolean, list: java.util.List[TreePath]) {
    if (expanded && !tree.isVisible(parent)) {
      return
    }
    list.add(parent)

    val node: MyNodeForTreeProfile = parent.getLastPathComponent().asInstanceOf[MyNodeForTreeProfile];
    if (node.getChildCount() >= 0) {
      val e = node.children()
      while (e.hasMoreElements()) {
        val n: MyNodeForTreeProfile = e.nextElement().asInstanceOf[MyNodeForTreeProfile]
        val path: TreePath = parent.pathByAddingChild(n);
        getPaths(tree, path, expanded, list);
      }
    }
  }

  def getPath(node: TreeNode): TreePath = {
    // Get node depth
    var depth: Int = 0
    var currentNode: TreeNode = null
    var bool = true
    val list: ListBuffer[TreeNode] = new ListBuffer()
    list += node
    currentNode = node
    while (bool) {
      currentNode = currentNode.getParent()
      if (null != currentNode) {
        list += currentNode
      } else bool = false
    }
    val path: Array[Object] = list.reverse.toArray
    
    new TreePath(path)
  }

  val listSelectionModel: ListSelectionModel = table1.getSelectionModel()
  val listSelectionListener: ListSelectionListener = new ListSelectionListener() {
    def valueChanged(e: ListSelectionEvent) {
      val lsm: ListSelectionModel = e.getSource().asInstanceOf[ListSelectionModel]
      if (!lsm.isSelectionEmpty()) {
        val selectedRow = lsm.getMinSelectionIndex()
         val columRank = table1.getColumnModel().getColumnIndex("rank")
        val rankSelected = table1.getValueAt(selectedRow, columRank).asInstanceOf[String].toInt

        
        val listPath: java.util.List[TreePath] = new java.util.ArrayList[TreePath]()

        //         tree.setSelectionRow(rankSelected)
        //         tree.getPathForRow(rankSelected)
        //         tree.expandRow(rankSelected)
        //         
        val tpath = getPath(hmTable1.get(rankSelected).get)

        tree.expandPath(tpath)
        tree.setSelectionPath(tpath)
        tree.scrollPathToVisible(tpath)
      }
    }
  }

  listSelectionModel.addListSelectionListener(listSelectionListener)

  val mlis: MouseListener = new MouseListener() {
    
    def mouseClicked(e: MouseEvent) {
   
      //println("Mouse Cliked on "+e.getX()+";"+e.getY())
      // tree.repaint()
      if (e.getSource() == table1) {
        if (e.getButton() > 1) {
          //          println("clicked on table1 with modifier =" + e.getButton)
          //          println("numRow=" + table1.rowAtPoint(e.getPoint()))
          val idx = table1.getColumnModel().getColumnIndex("rank")
          val rank = table1.getValueAt(table1.rowAtPoint(e.getPoint()), idx).toString.toInt
          //  println("rank = " + rank)
          val node = hmTable1.get(rank).get
          //          val parentreadDirect = node.nameParent
          //          val parent = hmTable1.get(node.idParent).get.nameFunction
          //          //          println("parent of " + node.nameFunction + " is " + parent)
          //          //          println("direct read parent of " + node.nameFunction + " is " + parentreadDirect)
          //          val idsChildrens = node.idsChildren
          //          val len = idsChildrens.size()
          //          //          for ( i<- 0 until len){
          //          //            println("child("+i+") / rank ("+idsChildrens.get(i)+")= "+hmTable1.get(idsChildrens.get(i)).get )
          //          //          }
          val idxTab = jTP.indexOfTab("Drill Down")

          jTP.remove(idxTab)
          val split = new MyDrillDownXDebugProfileScroll(tree, hmTable1, node)
          jTP.addTab("Drill Down", split)
          jTP.setSelectedIndex(2)

        }
      } else if (e.getSource() == tree) {
        if (e.getButton() > 1) {
         
          val node: MyNodeForTreeProfile =
            tree.getLastSelectedPathComponent().asInstanceOf[MyNodeForTreeProfile]
         
          val idxTab = jTP.indexOfTab("Drill Down")

          jTP.remove(idxTab)
          val split = new MyDrillDownXDebugProfileScroll(tree, hmTable1, node)
          jTP.addTab("Drill Down", split)
          jTP.setSelectedIndex(2)
        }
      }
    
  }

  def mouseEntered(e: MouseEvent) {
    MyDialogFormatPHPProfile.jdia.setVisible(false)
    if (e.getSource().isInstanceOf[JTable] && e.getSource().asInstanceOf[JTable] == table1) {
        // Destruction de tous les listener
        listSelectionModel.removeListSelectionListener(listSelectionListener)
        tree.removeTreeSelectionListener(treeSelectionListener)
        // activation du listener de la table
        listSelectionModel.addListSelectionListener(listSelectionListener)
       
      } else if (e.getSource().isInstanceOf[JTree] && e.getSource().asInstanceOf[JTree] == tree) {
        // Destruction de tous les listener
        listSelectionModel.removeListSelectionListener(listSelectionListener)
        tree.removeTreeSelectionListener(treeSelectionListener)
        // activation du listener de l arbre
        tree.addTreeSelectionListener(treeSelectionListener)
       
      }
    
    
  }

  def mouseExited(e: MouseEvent) {}

  def mousePressed(e: MouseEvent) {}

  def mouseReleased(e: MouseEvent) {}
   
   }

  tree.addMouseListener(mlis)
  table1.addMouseListener(mlis)

//  jbutDuration.addActionListener(this)
//  jbutMemory.addActionListener(this)

  val jTP: JTabbedPane = new JTabbedPane()
  val jSP1: JScrollPane = new JScrollPane(table1)
  val jSP2: JScrollPane = new JScrollPane(table2)
  
   // Tab Drill Down
  val jSplit: JSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT)
  jSplit.setOneTouchExpandable(true);
  jSplit.setDividerLocation(500);
  jSplit.setMinimumSize(new Dimension(800, 800))
  
  jTP.addTab("View Tree as Table", jSP1)
  jTP.addTab("Concat functions", jSP2)
  jTP.addTab("Drill Down", jSplit)
  
  
  val newLine = "\n"

  this.peer.setMinimumSize(new Dimension(900, 500))
  val minimalSize = new Dimension(200, 400);

  val lJScrollPane: JScrollPane = new JScrollPane(tree)
   val color = new Color(255, 255, 223)
  lJScrollPane.setBackground(color)
  lJScrollPane.getViewport().setBackground(color)
  tree.setBackground(color)
   tree.setOpaque(false)
  tree.setVisible(true)
 
  
  val rJScrollPane: JTabbedPane = jTP

  lJScrollPane.setMinimumSize(minimalSize)
  rJScrollPane.setMinimumSize(minimalSize)
  val jSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, lJScrollPane, rJScrollPane)
  jSplitPane.setOneTouchExpandable(true);
  jSplitPane.setDividerLocation(400);
  jSplitPane.setMinimumSize(new Dimension(800, 400))
  this.peer.setLayout(new BorderLayout())
  this.peer.add(jSplitPane, BorderLayout.CENTER)
  this.peer.add(menuJPanel, BorderLayout.NORTH)

  this.peer.setVisible(true)
  tree.setLargeModel(true)
//  def actionPerformed(e: ActionEvent) {
//    e.getSource() match {
//      case `jbutDuration` =>
//        jLabel.setText("Current View : Duration")
//        jLabelDesc1.setText("rank:nameFunction(nbExec,durationInMillis,selfDurationInMillis")
//        PHPJPanelForTree.isDurationView = true
//
//        tree.repaint()
//      case `jbutMemory` =>
//        jLabel.setText("Current View : Memory")
//        jLabelDesc1.setText("rank:nameFunction(nbExec,memBefore(bytes),memAfter(bytes),deltaMem(bytes))")
//        PHPJPanelForTree.isDurationView = false
//
//        tree.repaint()
//
//    }
//  }

  def mouseClicked(e: MouseEvent) {
    //println("Mouse Cliked on "+e.getX()+";"+e.getY())
    // tree.repaint()
  }

  def mouseEntered(e: MouseEvent) {}

  def mouseExited(e: MouseEvent) {}

  def mousePressed(e: MouseEvent) {}

  def mouseReleased(e: MouseEvent) {}

  def createConcatNode(tmp1Node: MyNodeForTreeProfile, newNode: MyNodeForTreeProfile) = {
   
    val rank = tmp1Node.rank
    val nameFunction = tmp1Node.nameFunction
    val fileNameRow = tmp1Node.fileNameRow
    val level = tmp1Node.level
    val nbExec = tmp1Node.nbExec + newNode.nbExec
    val durationInMillis = ((((tmp1Node.nbExec * tmp1Node.durationInMillis + newNode.nbExec * newNode.durationInMillis)*1000).toLong / (nbExec))/1000).toDouble
    val selfDurationInMillis = ((((tmp1Node.selfDurationInMillis * tmp1Node.nbExec + newNode.nbExec * newNode.selfDurationInMillis)*1000).toLong / nbExec)/1000).toDouble
   
    new MyNodeForTreeProfile(rank, nameFunction, fileNameRow, level, nbExec, durationInMillis, selfDurationInMillis     )
  }
  def returnArrayForTable1(aNode: MyNodeForTreeProfile): Array[Object] = {
    //Array("rank", "nameFunction", "fileNameRow", "level", "nbExec", "durationInMillis", "selfDurationInMillis")
    var returnArray: Array[Object] = Array.ofDim(10)
    returnArray(0) = aNode.rank.toString
    returnArray(1) = aNode.nameFunction
    returnArray(2) = aNode.fileNameRow
    returnArray(3) = aNode.level.toString
    returnArray(4) = aNode.nbExec.toString
    returnArray(5) = df1.format(aNode.durationInMillis)
    returnArray(6) = df1.format(aNode.selfDurationInMillis)
     returnArray

  }
  def returnArrayForTable2(aNode: MyNodeForTreeProfile): Array[Object] = {
    //Array( "nameFunction", "fileNameRow",  "nbExec", "durationInMillis", "selfDurationInMillis")
    var returnArray: Array[Object] = Array.ofDim(8)
    returnArray(0) = aNode.nameFunction
    returnArray(1) = aNode.fileNameRow
    returnArray(2) = aNode.nbExec.toString
    returnArray(3) = df1.format(aNode.durationInMillis)
    returnArray(4) = df1.format(aNode.selfDurationInMillis)
    returnArray

  }
val myRenderer: MyDefaultCellXDebugProfileRenderer = new MyDefaultCellXDebugProfileRenderer()
  //  val idxNameFunction = table1.getColumnModel().getColumnIndex("nameFunction")
  //  table1.getColumnModel().getColumn(idxNameFunction).setCellRenderer(myRenderer)

  //  val idxNameParent = table1.getColumnModel().getColumnIndex("nameParent")
  //  table1.getColumnModel().getColumn(idxNameParent).setCellRenderer(myRenderer)

  table1.setDefaultRenderer(classOf[Object], myRenderer)
  table2.setDefaultRenderer(classOf[Object], myRenderer)
  table1.repaint()
   table2.repaint()
}

object PHPJPanelForTreeProfile {
  var isDurationView = true

  val workspace = System.getProperty("workspace", "/opt/workspaceLP")
//  def main(args: Array[String]) {
//    //Schedule a job for the event-dispatching thread:
//    //creating and showing this application's GUI.
//    javax.swing.SwingUtilities.invokeLater(new Runnable() {
//      def run() {
//        createAndShowGUI(new File(args(0)));
//      }
//    })
//
//  }

//// inside of swingScaViewer
  def insideSwingScaViewer(args: Array[String]) {
    //Schedule a job for the event-dispatching thread:
    //creating and showing this application's GUI.
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      def run() {
        createAndShowGUIInside(new File(args(0)));
      }
    })

  }
  
// inside of swingScaViewer
  def createAndShowGUIInside(file: File) {
    // use the contentPane of swingScaViewer
    // val frame = new JFrame("PHP Trace and Profile analysis");
    //frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    //Create and set up the content pane.PHPJPanelForTreeXHprofProfile
    val newContentPane = new PHPJPanelForTreeProfile(file);
    // newContentPane.setOpaque(true); //content panes must be opaque
    SwingScaViewer.mainPanel.contents.clear()
    SwingScaViewer.mainPanel.contents += newContentPane.asInstanceOf[scala.swing.Component]
    SwingScaViewer.mainPanel.enabled
    SwingScaViewer.mainPanel.visible = false
    SwingScaViewer.mainPanel.visible = true

  }
  
  
}

case class MyNodeForTreeProfile(rank: Int,  nameFunction: String, fileNameRow: String, level: Int, nbExec: Int, durationInMillis: Double, selfDurationInMillis: Double ) extends DefaultMutableTreeNode {
  //date;nameFunction;fileNameRow;level;nbExec;durationInMillis;selfDurationInMillis;memBefore;memAfter;deltaMem);
  var  nameParent:String = ""
  var idParent: Int = 0
  val idsChildren: ArrayList[Int] = new ArrayList()
  
  def toStringDuration(): String = {
    rank + ":" + nameFunction + " ( " + nbExec + " , " + durationInMillis + " , " + selfDurationInMillis + " )"
  }
 override def toString(): String = {toStringDuration() }
  
  def isEquals(that: MyNodeForTreeProfile): Boolean = {

    (this.nameFunction + this.fileNameRow).equals(that.nameFunction + that.fileNameRow)
  }
}
object MyNodeForTreeFactoryProfile {
  //nameFunction;fileNameRow;level;nbExec;durationInMillis;selfDurationInMillis);
  def apply(line: String): MyNodeForTreeProfile =
    {
      val tab: Array[String] = line.split(";")
      val rank = tab(0).toInt
      val nameFunction = tab(1)
      val fileNameRow = tab(2)
      val level = tab(3).toInt
      val nbExec = tab(4).toInt
      val durationInMillis = tab(5).toDouble
      val selfDurationInMillis = tab(6).toDouble
      new MyNodeForTreeProfile(rank, nameFunction, fileNameRow, level, nbExec, durationInMillis, selfDurationInMillis)

    }
  def apply(file: File): MyNodeForTreeProfile =
    {

     
      val rank = -1
      val nameFunction = file.getAbsolutePath()
      val fileNameRow = ""
      val level = -1
      val nbExec = 1
      val durationInMillis = -1D
      val selfDurationInMillis = -1D
           new MyNodeForTreeProfile(rank,  nameFunction, fileNameRow, level, nbExec, durationInMillis, selfDurationInMillis )

    }

}



