package com.jlp.scaviewer.tools.php

import javax.swing.JDialog
import javax.swing.JScrollPane
import java.awt.BorderLayout
import javax.swing.JSplitPane
import javax.swing.table.DefaultTableModel
import javax.swing.JTable
import javax.swing.table.TableRowSorter
import java.awt.event.MouseListener
import java.awt.event.MouseEvent
import javax.swing.JPanel
import java.awt.GridBagLayout
import java.awt.GridBagConstraints
import javax.swing.JLabel
import java.awt.Font
import java.awt.Insets
import java.awt.Dimension
import java.awt.Color
import scala.language.postfixOps
import java.awt.Toolkit
import javax.swing.ScrollPaneConstants
import javax.swing.ListSelectionModel
import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent
import javax.swing.tree.TreePath
import javax.swing.tree.TreeNode
import scala.collection.mutable.ListBuffer
import javax.swing.JTree
import java.text.DecimalFormatSymbols
import java.util.Locale
import java.text.DecimalFormat

class MyDrillDownXDebugProfileScroll(tree: JTree, hmTable1: scala.collection.mutable.HashMap[Int, MyNodeForTreeProfile],
  node: MyNodeForTreeProfile) extends JScrollPane with MouseListener {

  // retrieve the screen dimension
  val fontRow = new Font("Arial", Font.BOLD, 12)
  val fontHeader = new Font("Arial", Font.BOLD, 13)
  val tk: Toolkit = Toolkit.getDefaultToolkit()
  val screenDim = tk.getScreenSize()
  val hScreen = screenDim.width
  val vScreen = screenDim.height
  val panel: JPanel = new JPanel()
  val dfs: DecimalFormatSymbols = new DecimalFormatSymbols(Locale.ENGLISH)
  dfs.setGroupingSeparator(' ')

  dfs.setDecimalSeparator('.')
  val df: DecimalFormat = new DecimalFormat("#############.###", dfs)
  df.setGroupingUsed(true)
  df.setGroupingSize(3)
  val df1: DecimalFormat = new DecimalFormat("############0.000", dfs)
  df1.setGroupingUsed(true)
  df1.setGroupingSize(3)
  val df2 = new DecimalFormat("####0.000E0", dfs)
  df2.setGroupingUsed(true)
  df2.setGroupingSize(3)
  //
  // to Be filled
  var dataParent: Array[Array[Object]] = Array.ofDim(1, 8) // to Be filled
  var dataFunction: Array[Array[Object]] = Array.ofDim(1, 8) // to Be filled

  // "rank", "nameFunction", "fileNameRow", "level", "nbExec", "durationInMillis", "selfDurationInMillis
  val titleTableParent: Array[Object] = Array("rank", "fileNameRow", "nbExec", "durationInMillis", "selfDurationInMillis")
  val titleTableFunction: Array[Object] = Array("rank", "fileNameRow", "nbExec", "durationInMillis", "selfDurationInMillis")

  val titleTable1: Array[Object] = Array("rank", "fileNameRow", "nameFunction", "nbExec", "durationInMillis", "selfDurationInMillis")
  // size to 0 column rank

  class MyTableModel(data: Array[Array[Object]], title: Array[Object]) extends DefaultTableModel(data, title) {

    override def isCellEditable(row: Int, col: Int): Boolean = false
  }

  var modelChild: MyTableModel = null
  val modelParent: MyTableModel = new MyTableModel(dataParent, titleTableParent)

  val modelFunction: MyTableModel = new MyTableModel(dataFunction, titleTableFunction)
  val tableParent: JTable = new JTable(modelParent)
  val tableFunction: JTable = new JTable(modelFunction)
  // size to 0 column rank
  var idx = tableParent.getColumnModel().getColumnIndex("rank")
  tableParent.getColumnModel().getColumn(idx).setMinWidth(0);
  tableParent.getColumnModel().getColumn(idx).setMaxWidth(0);
  tableParent.getColumnModel().getColumn(idx).setWidth(0);

  // size to 0 column rank
  idx = tableFunction.getColumnModel().getColumnIndex("rank")
  tableFunction.getColumnModel().getColumn(idx).setMinWidth(0);
  tableFunction.getColumnModel().getColumn(idx).setMaxWidth(0);
  tableFunction.getColumnModel().getColumn(idx).setWidth(0);

  // construction du JPanel haut
  //val hPanel = new JPanel()

  panel.setLayout(new GridBagLayout())
  var gbc = new GridBagConstraints()
  gbc.fill = GridBagConstraints.BOTH
  gbc.weightx = 1.0;

  gbc.gridx = 0
  gbc.gridy = 0
  gbc.insets = new Insets(10, 10, 10, 10)

  val jlParent: JLabel = new JLabel("Parent Function : " + node.nameParent)
  jlParent.setFont(new Font("Arial", Font.BOLD, 18))
  jlParent.setMinimumSize(new Dimension(hScreen * 1 / 2, 20))
  panel.add(jlParent, gbc)

  gbc.gridy = 1

  val jspParent = new JScrollPane(tableParent)
  tableParent.setMinimumSize(new Dimension(hScreen * 1 / 2, 60))
  tableParent.setMaximumSize(new Dimension(hScreen, 100))
  tableParent.setPreferredSize(new Dimension(hScreen * 1 / 2, 60))
  tableParent.setFont(fontRow)
  tableParent.setRowHeight(16)
  tableParent.getTableHeader().setFont(fontHeader);
  jspParent.setMinimumSize(new Dimension(hScreen * 1 / 2, 60))
  jspParent.setMaximumSize(new Dimension(hScreen, 100))
  jspParent.setPreferredSize(new Dimension(hScreen * 1 / 2, 60))
  tableParent.setBackground(new Color(153, 255, 204))

  panel.add(jspParent, gbc)

  gbc.gridy = 2
  val jlFunction: JLabel = new JLabel("Current Function : " + node.nameFunction)
  jlFunction.setFont(new Font("Arial", Font.BOLD, 18))
  jlFunction.setMinimumSize(new Dimension(hScreen * 1 / 2, 20))
  panel.add(jlFunction, gbc)

  gbc.gridy = 3
  val jspFunction = new JScrollPane(tableFunction)
  tableFunction.setMinimumSize(new Dimension(hScreen * 1 / 2, 60))
  tableFunction.setMaximumSize(new Dimension(hScreen, 100))
  tableFunction.setPreferredSize(new Dimension(hScreen * 1 / 2, 60))
  tableFunction.setFont(fontRow)
  tableFunction.setRowHeight(16)
  tableFunction.getTableHeader().setFont(fontHeader);
  jspFunction.setMinimumSize(new Dimension(hScreen * 1 / 2, 60))
  jspFunction.setMaximumSize(new Dimension(hScreen, 100))
  jspFunction.setPreferredSize(new Dimension(hScreen * 1 / 2, 60))
  tableFunction.setBackground(new Color(153, 255, 51))

  panel.add(jspFunction, gbc)

  gbc.gridy = 4
  val jlChild: JLabel = new JLabel("Child Functions of : " + node.nameFunction)
  jlChild.setFont(new Font("Arial", Font.BOLD, 18))
  jlChild.setMinimumSize(new Dimension(hScreen * 1 / 2, 20))
  panel.add(jlChild, gbc)

  gbc.gridy = 5
  // first fill of tables
  fillAgain(node)
  val tableChild: JTable = new JTable(modelChild)

  val sorter1 = new TableRowSorter(modelChild);

  // table1 =>""rank", "fileNameRow", "nameFunction","nbExec", "durationInMillis", "selfDurationInMillis"
  sorter1.setComparator(0, new ComparatorStringAsLong())
  sorter1.setComparator(3, new ComparatorStringAsLong())
  sorter1.setComparator(4, new ComparatorStringAsDouble())
  sorter1.setComparator(5, new ComparatorStringAsDouble())

  tableChild.setRowSorter(sorter1)

  // size to 0 column rank
  idx = tableChild.getColumnModel().getColumnIndex("rank")
  tableChild.getColumnModel().getColumn(idx).setMinWidth(0);
  tableChild.getColumnModel().getColumn(idx).setMaxWidth(0);
  tableChild.getColumnModel().getColumn(idx).setWidth(0);
  tableChild.setFont(fontRow)
  tableChild.setRowHeight(16)
  tableChild.getTableHeader().setFont(fontHeader)
  val jspTableChild = new JScrollPane(tableChild)
  tableChild.setMinimumSize(new Dimension(hScreen * 1 / 2, 120))

  jspTableChild.setMinimumSize(new Dimension(hScreen * 1 / 2, 500))
  jspTableChild.setMinimumSize(new Dimension(hScreen * 1 / 2, 500))
  jspTableChild.setMaximumSize(new Dimension(hScreen, 4000))
  tableChild.setBackground(new Color(153, 255, 255))
  panel.add(jspTableChild, gbc)

  //this.add(panel)
  setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)

  this.setViewportView(panel)

  this.setVisible(true)
  tableChild.addMouseListener(this)
  tableParent.addMouseListener(this)

  val myRenderer: MyDefaultCellXDebugProfileRenderer = new MyDefaultCellXDebugProfileRenderer()
  tableChild.setDefaultRenderer(classOf[Object], myRenderer)
  tableChild.repaint()
  this.tableFunction.setDefaultRenderer(classOf[Object], myRenderer)
  this.tableParent.setDefaultRenderer(classOf[Object], myRenderer)
  tableFunction.repaint()
  tableParent.repaint()

  private def fillDataOneLine(idNode: Int, aTable: JTable) {
    val node = hmTable1.get(idNode).get
    aTable.getModel().asInstanceOf[MyTableModel].removeRow(0)
    // val titleTableParent: Array[Object] = Array("rank","nbExec", "durationInMillis", "selfDurationInMillis", "cpuDurationInMillis", "selfCpuDurationInMillis", "memoryUsed", "peakMemory")
    val locDatas: Array[Object] = Array(node.rank.toString, node.fileNameRow, node.nbExec.toString, df1.format(node.durationInMillis), df1.format(node.selfDurationInMillis))
    aTable.getModel().asInstanceOf[MyTableModel].addRow(locDatas)
  }

  private def fillDataChild(idNode: Int) {
    val node = hmTable1.get(idNode).get
    val idsChildrens = node.idsChildren
    val len = idsChildrens.size()
    val datas: Array[Array[Object]] = Array.ofDim(len, 9)

    for (i <- 0 until len) {
      val tmpVal = returnArrayForTableChildren(hmTable1.get(idsChildrens.get(i)).get)
      for (j <- 0 until 6) {

        datas(i)(j) = tmpVal(j)
        //  println("tmpVal(" + j + ")=" + tmpVal(j))
      }
      // for the next updates
      if (null != modelChild) {
        modelChild.addRow(datas(i))
      }
    }
    // for the first instanciation
    if (null == modelChild) modelChild = new MyTableModel(datas, titleTable1)

  }

  def returnArrayForTableChildren(aNode: MyNodeForTreeProfile): Array[Object] = {
    //// table1 =>"rank","fileNameRow", "nameFunction", "nbExec", "durationInMillis", "selfDurationInMillis""
    var returnArray: Array[Object] = Array.ofDim(6)
    returnArray(0) = aNode.rank.toString
    returnArray(1) = aNode.fileNameRow
    returnArray(2) = aNode.nameFunction
    returnArray(3) = aNode.nbExec.toString
    returnArray(4) = df1.format(aNode.durationInMillis)
    returnArray(5) = df1.format(aNode.selfDurationInMillis)

    returnArray

  }

  def fillAgain(newNode: MyNodeForTreeProfile) {
    fillDataOneLine(hmTable1.get(newNode.rank).get.idParent, tableParent)
    fillDataOneLine(newNode.rank, tableFunction)
    fillDataChild(newNode.rank)

    tableFunction.setBackground(new Color(153, 255, 51))
    tableParent.setBackground(new Color(153, 255, 204))
    if (null != tableChild) {
      // println("tableChild is not null")
      tableChild.setBackground(new Color(153, 255, 255))
      tableChild.setModel(modelChild)
      tableChild.repaint()
      jspTableChild.setViewportView(tableChild)
      jspTableChild.revalidate()
      jspTableChild.setVisible(true)
      jspTableChild.repaint()
      //  println("modelChild.size=" + modelChild.getRowCount())

    }
    this.revalidate()
    this.setVisible(true)
    this.repaint()
  }

  // Mouse Listener

  def mouseClicked(e: MouseEvent) {
    //   println("Mouse Cliked on " + e.getX() + ";" + e.getY())

    // tree.repaint()
    if (e.getSource().isInstanceOf[JTable]) {
      val tmpTable = e.getSource().asInstanceOf[JTable]
      if (tmpTable == tableParent || tmpTable == tableChild) {
        if (e.getButton() > 1) {
          // desactiver les listener
          tableParent.removeMouseListener(this)
          tableChild.removeMouseListener(this)
          //   println("clicked on table1 with modifier =" + e.getButton)
          //      println("numRow=" + tmpTable.rowAtPoint(e.getPoint()))
          val idx = tmpTable.getColumnModel().getColumnIndex("rank")
          val rank = tmpTable.getValueAt(tmpTable.rowAtPoint(e.getPoint()), idx).toString.toInt
          //   println("rank = " + rank)
          val node = hmTable1.get(rank).get
          val parentreadDirect = node.nameParent

          val tmp = hmTable1.get(node.idParent)
          if (None != tmp) {
            val parent = hmTable1.get(node.idParent).get.nameFunction
            //          println("parent of " + node.nameFunction + " is " + parent)
            //          println("direct read parent of " + node.nameFunction + " is " + parentreadDirect)
            val idsChildrens = node.idsChildren
            val len = idsChildrens.size()
            //          for (i <- 0 until len) {
            //            println("mouseClicked : child(" + i + ") / rank (" + idsChildrens.get(i) + ")= " + hmTable1.get(idsChildrens.get(i)).get)
            //          }
            // empty table child
            val lenDatas = modelChild.getRowCount()
            for (i <- 0 until lenDatas reverse) {
              modelChild.removeRow(i)
            }
            //this.tableChild.setModel(modelChild)
            this.jlFunction.setText("Current Function : " + node.nameFunction)
            this.jlParent.setText("Parent Function : " + node.nameParent)
            this.jlChild.setText("Child Functions of : " + node.nameFunction)

            // Updating
            fillAgain(node)
          }
          // activate agin the mouse listeners
          tableParent.addMouseListener(this)
          tableChild.addMouseListener(this)
        }
      }
    }
  }

  def mouseEntered(e: MouseEvent) {}

  //    

  def mouseExited(e: MouseEvent) {}

  def mousePressed(e: MouseEvent) {}

  def mouseReleased(e: MouseEvent) {}

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

  // Code below, synchronize every mouse click on Parent table or child table with the tree on the left side

  // tablmParent
  val listSelectionModel1: ListSelectionModel = tableParent.getSelectionModel()
  val listSelectionListener1: ListSelectionListener = new ListSelectionListener() {
    def valueChanged(e: ListSelectionEvent) {
      val lsm: ListSelectionModel = e.getSource().asInstanceOf[ListSelectionModel]
      if (!lsm.isSelectionEmpty()) {
        val selectedRow = lsm.getMinSelectionIndex()
        val columRank = tableParent.getColumnModel().getColumnIndex("rank")
        val rankSelected = tableParent.getValueAt(selectedRow, columRank).asInstanceOf[String].toInt

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

  listSelectionModel1.addListSelectionListener(listSelectionListener1)

  // Tablechild
  val listSelectionModel2: ListSelectionModel = tableChild.getSelectionModel()
  val listSelectionListener2: ListSelectionListener = new ListSelectionListener() {
    def valueChanged(e: ListSelectionEvent) {
      val lsm: ListSelectionModel = e.getSource().asInstanceOf[ListSelectionModel]
      if (!lsm.isSelectionEmpty()) {
        val selectedRow = lsm.getMinSelectionIndex()
        val columRank = tableChild.getColumnModel().getColumnIndex("rank")
        val rankSelected = tableChild.getValueAt(selectedRow, columRank).asInstanceOf[String].toInt

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

  listSelectionModel2.addListSelectionListener(listSelectionListener2)

  // TableCurrent
  val listSelectionModel3: ListSelectionModel = this.tableFunction.getSelectionModel()
  val listSelectionListener3: ListSelectionListener = new ListSelectionListener() {
    def valueChanged(e: ListSelectionEvent) {
      val lsm: ListSelectionModel = e.getSource().asInstanceOf[ListSelectionModel]
      if (!lsm.isSelectionEmpty()) {
        val selectedRow = lsm.getMinSelectionIndex()
        val columRank = tableFunction.getColumnModel().getColumnIndex("rank")
        val rankSelected = tableFunction.getValueAt(selectedRow, columRank).asInstanceOf[String].toInt

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

  listSelectionModel3.addListSelectionListener(listSelectionListener3)

}