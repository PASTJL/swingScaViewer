package com.jlp.scaviewer.tools
import javax.swing.JDialog
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import scala.swing.GridBagPanel
import java.awt.Font
import java.awt.Dimension
import java.awt.Insets
import scala.swing.Label
import java.awt.GridBagConstraints
import java.io.File
import com.jlp.scaviewer.ui.SwingScaViewer
import com.jlp.scaviewer.commons.utils.SearchDirFile
import javax.swing.JFileChooser
import javax.swing.JButton
import javax.swing.JDialog
import javax.swing.JPanel
import java.awt.GridBagLayout
import javax.swing.JLabel
import javax.swing.JTextArea
import javax.swing.JTextField
import javax.swing.JScrollPane
import javax.swing.ScrollPaneConstants
import java.text.SimpleDateFormat
import java.text.ParseException
import java.util.Locale
class MyDialogSimpleDateFormat(modal: Boolean) extends JDialog with ActionListener {

  setModal(modal)
  val gbp: JPanel = new JPanel()
  gbp.setLayout(new GridBagLayout)
  val font1 = new Font("Arial", Font.BOLD, 14)
  val font2 = new Font("Arial", Font.BOLD, 16)
  this.setMinimumSize(new Dimension(700, 300))
  this.setPreferredSize(new Dimension(700, 400))
  this.setMaximumSize(new Dimension(700, 800))
  val gbc = new GridBagConstraints
  var insets1 = new Insets(10,10, 10, 10)
  var insets2 = new Insets(0, 0, 0, 0)
  gbc.insets = insets1
  gbc.gridx = 0
  gbc.gridy = 0
  gbc.weightx = 1.0
  gbc.gridwidth = 2
  
  gbc.anchor = GridBagConstraints.CENTER
  this.setTitle("Testing SimpleDateFormat")
  val jlTitle = new JLabel("Testing SimpleDateFormat")
  jlTitle.setFont(font2)
  gbp.add(jlTitle, gbc)

  gbc.fill = GridBagConstraints.NONE
  val jl1 = new JLabel("java Date Format (SimpleDateFormat)")
  jl1.setFont(font1)
  val jtf = new JTextField
  jtf.setMaximumSize(new Dimension(500, 20))
  jtf.setPreferredSize(new Dimension(400, 20))
  jtf.setMinimumSize(new Dimension(350, 20))
  gbc.gridwidth = 1
  gbc.gridx = 0
  gbc.gridy = 1
  gbc.anchor = GridBagConstraints.EAST
  gbp.add(jl1, gbc)
  gbc.gridx = 1
  gbc.anchor = GridBagConstraints.WEST
  gbp.add(jtf, gbc)

  // String to parse
  val jl2 = new JLabel("Date to parse")
  jl2.setFont(font1)
  val jtf2 = new JTextField
  jtf2.setMaximumSize(new Dimension(500, 20))
  jtf2.setPreferredSize(new Dimension(400, 20))
  jtf2.setMinimumSize(new Dimension(350, 20))
  gbc.gridwidth = 1
  gbc.gridx = 0
  gbc.gridy = 2
   gbc.anchor = GridBagConstraints.EAST
  gbp.add(jl2, gbc)
  gbc.gridx = 1
   gbc.anchor = GridBagConstraints.WEST
  gbp.add(jtf2, gbc)

  // result OK/KO of the parsing
  val jl3 = new JLabel("Result of the parsing")
  jl3.setFont(font1)
  val jta = new JTextArea()
  val jsp = new JScrollPane(jta)
  jsp.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED)
  jsp.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)
  jta.setRows(5)
  jsp.setMaximumSize(new Dimension(800, 300))
  jsp.setPreferredSize(new Dimension(200, 200))
  jsp.setMinimumSize(new Dimension(100, 150))
  gbc.gridwidth = 2
  gbc.weightx = 1.0
  gbc.fill=GridBagConstraints.HORIZONTAL
  gbc.gridx = 0
  gbc.gridy = 3
  gbc.anchor = GridBagConstraints.CENTER
  gbp.add(jl3, gbc)
  gbc.gridx = 0
  gbc.gridy = 4
  gbc.gridwidth = 2
  gbp.add(jsp, gbc)

  val jbOK = new JButton("OK")
  jbOK.setFont(font1)
  val jbCancel = new JButton("Cancel")
  jbCancel.setFont(font1)
  gbc.gridwidth = 1
  gbc.gridx = 0
  gbc.gridy = 5
  gbp.add(jbOK, gbc)
  gbc.gridx = 1
  gbc.gridy = 5

  gbp.add(jbCancel, gbc)
  jbOK.addActionListener(this)
  jbCancel.addActionListener(this)

  this.setContentPane(gbp)
  this.pack
  this.setVisible(true)
  def actionPerformed(arg0: ActionEvent) {
    if (arg0.getSource.isInstanceOf[JButton]) {
      var jb = arg0.getSource.asInstanceOf[JButton]
      if (jb == jbCancel)
        dispose
      if (jb == jbOK) parse()
    }
  }
  private def parse() {
    val toParse: String = this.jtf2.getText()
    val reg="""Fev|Avr|Mai|Jui|Lun|Mer|Jeu|Ven|Sam|Dim""".r
    var loc:Locale=Locale.ENGLISH
    if( None != reg.findFirstIn(toParse))
    {
      loc=Locale.FRENCH
    }
    
    val sdf: SimpleDateFormat = new SimpleDateFormat(jtf.getText(),loc)

    try {
      val date = sdf.parse(toParse)
      this.jta.setText("OK, the parsing is right :\n" + date.toString)
    } catch {
      case e: ParseException =>
        var result: String = "KO, the parsing is not correct ParsingException :\n" +
          e.getMessage()
        this.jta.setText(result)
      case _ :Throwable =>
        var result: String = "KO, the parsing is not correct Unknown error :\n"
        this.jta.setText(result)
    }
  }

}