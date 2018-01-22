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
package com.jlp.scaviewer.filestats
import scala.swing.Dialog
import scala.swing.GridBagPanel
import java.awt.Dimension
import java.awt.Insets
import scala.swing.Label
import java.awt.Font
import java.awt.GridBagConstraints
import java.awt.Point
import java.io.File
import java.io.FileFilter
import scala.collection._
import scala.swing.ComboBox
import scala.swing.Window
import scala.swing.TextField
import scala.swing.Button
import scala.swing.event.ActionEvent
import com.jlp.scaviewer.ui.SwingScaViewer
import scala.swing.MainFrame
import scala.swing.event.MouseClicked
import javax.swing.JOptionPane
import scala.swing.RadioButton
import com.jlp.scaviewer.commons.utils.CopyFile
import javax.swing.JComboBox
import javax.swing.DefaultComboBoxModel
import scala.swing.event.SelectionChanged

class NewDialogFilestatsTemplateLoc(file: File) extends Dialog {

  val gbp = new GridBagPanel
  modal = true
  this.preferredSize = new Dimension(500, 300)
  this.maximumSize = new Dimension(700, 400)
  var gbc = new gbp.Constraints()
  var insets1 = new Insets(10, 10, 10, 10)
  gbc.insets = insets1
  // titre du dialog
  val ltitle = new Label("Creating a new FileStats template")
  ltitle.font = new java.awt.Font("Arial", Font.BOLD, 16)
  gbc.gridx = 0
  gbc.gridy = 0
  gbc.weightx = 1.0
  gbc.weighty = 1.0
  gbc.gridwidth = 2

  gbc.fill = GridBagPanel.Fill.Both
  gbc.anchor = GridBagPanel.Anchor.Center
  gbp.layout += ((ltitle -> gbc))

  // rajout d'un bouton de selection

  val lbTemp = new Label("Local or General template")
  val rbGenLoc: RadioButton = new RadioButton("General template ?")
  rbGenLoc.selected = true
  gbc.gridx = 0
  gbc.gridy = 1
  // gbc.weightx = 1.0
  gbc.gridwidth = 1
  gbc.anchor = GridBagPanel.Anchor.East
  gbp.layout += ((lbTemp -> gbc))
  gbc.gridx = 1
  gbc.anchor = GridBagPanel.Anchor.West
  gbp.layout += ((rbGenLoc -> gbc))

  //Liste des templates existants
  gbc.gridwidth = 1
  gbc.gridx = 0
  gbc.gridy = 2

  var template: String = { if (rbGenLoc.selected) "general" else "local" }
  val pEx = new Label("Existing " + template + " template")
  gbc.anchor = GridBagPanel.Anchor.East
  gbp.layout += ((pEx -> gbc))

  val pathFilesLoc = new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject + File.separator + "templates" +

    File.separator + "filestats")
  if (!pathFilesLoc.exists) {
    pathFilesLoc.mkdir
  }
  println("pathFilesLoc=" + pathFilesLoc)
  val files = pathFilesLoc.listFiles(new FileFilter { def accept(file: File) = { file.getName.endsWith(".properties") } })
  println("files=" + files)

  val listAllFilesLoc = if (null != files && files.length > 0) List(files: _*) else List.empty

  val listTemplateLoc = listAllFilesLoc map { _.getName().split("\\.")(0) }

  val pathFilesGen = new File(System.getProperty("root") + File.separator + "templates" +
    File.separator + "filestats")
  if (!pathFilesGen.exists) {
    pathFilesGen.mkdir
  }
  val filesGen = pathFilesGen.listFiles(new FileFilter { def accept(file: File) = { file.getName.endsWith(".properties") } })
  println("filesGen=" + filesGen)
  val listAllFilesGen = if (null != filesGen && filesGen.length > 0) List(filesGen: _*) else List.empty

  var listTemplateGen = listAllFilesGen map { _.getName().split("\\.")(0) }

  gbc.gridx = 1
  gbc.gridy = 2
  var listProjets = { if (rbGenLoc.selected) listTemplateGen else listTemplateLoc }

 val cbxEx = new ComboBox(List.empty[String])
val model = new  DefaultComboBoxModel[String]
for (item <- listProjets)  model.addElement(item)

cbxEx.peer.setModel(model)

//  val cbxEx = new ComboBox(listProjets.asInstanceOf[List[String]]) {
//    peer.setModel(new DefaultComboBoxModel[String])
//
//    for (item <- listProjets) peer.addItem(item)
//  }
  //cbxEx.peer.asInstanceOf[JComboBox[String]].setModel(new DefaultComboBoxModel)
  gbc.anchor = GridBagPanel.Anchor.West
  gbp.layout += ((cbxEx -> gbc))
  // val cbPEx=new 

  // Saisie du nouveau projet et creation de l'arborescence
  gbc.gridx = 0
  gbc.gridy = 3
  val pNew = new Label("New Template")
  gbc.anchor = GridBagPanel.Anchor.East
  gbp.layout += ((pNew -> gbc))

  gbc.gridx = 1
  gbc.gridy = 3
  gbc.anchor = GridBagPanel.Anchor.West
  val tfPNew = new TextField(80)
  gbp.layout += ((tfPNew -> gbc))
  gbc.anchor = GridBagPanel.Anchor.Center
  // Button Cancel / OK
  val bOK = new Button("OK")
  val dimBut = new Dimension(80, 30)
  bOK.maximumSize = dimBut
  bOK.minimumSize = dimBut
  bOK.preferredSize = dimBut
  bOK.maximumSize = dimBut
  gbc.fill = GridBagPanel.Fill.None
  gbc.gridx = 0
  gbc.gridy = 4
  gbp.layout += ((bOK -> gbc))
  insets1 = new Insets(10, 10, 10, 10)
  val bCancel = new Button("Cancel")
  bCancel.maximumSize = dimBut
  bCancel.minimumSize = dimBut
  bCancel.preferredSize = dimBut
  bCancel.maximumSize = dimBut
  gbc.weightx = 0
  gbc.gridx = 1
  gbc.gridy = 4
  gbp.layout += ((bCancel -> gbc))

  this.contents = gbp
  var screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
  location = new Point((screenSize.width - size.width) / 2, (screenSize.height - size.height) / 2)
  this.resizable = true
  rbGenLoc.reactions += {
    case ActionEvent(`rbGenLoc`) =>
      cbxEx.peer.removeAllItems
      if (!rbGenLoc.selected) {
        for (item <- listTemplateLoc) {
          cbxEx.peer.addItem(item)

          listProjets = listTemplateLoc
        }
      } else {
        for (item <- listTemplateGen) {
          cbxEx.peer.asInstanceOf[JComboBox[String]].addItem(item)
          listProjets = listTemplateGen
        }
      }
  }
  listenTo(cbxEx.selection)
  reactions += {
    case SelectionChanged(`cbxEx`) =>
    
      this.tfPNew.text = cbxEx.selection.item
  }
  bOK.reactions += {

    case ActionEvent(`bOK`) => if (tfPNew.text.length() >= 4) {
      var templ = tfPNew.text
      if (templ.endsWith(".properties")) {
        templ = templ.substring(0, templ.lastIndexOf("."))
      }
      println("templ=" + templ)
      println("listProjets=" + listProjets)

      val path = {
        if (!rbGenLoc.selected) {
          System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject + File.separator + "templates" + File.separator + "filestats"
        } else {
          System.getProperty("root") + File.separator + "templates" + File.separator + "filestats"
        }
      }

      CopyFile.copy(file, new File(path + File.separator + templ + ".properties"))
      dispose

    } else {
      JOptionPane.showMessageDialog(null, "The length of the string must be >= 4")
      tfPNew.text = ""
    }

  }
  bCancel.reactions += {

    case ActionEvent(`bCancel`) =>

      dispose

  }

  cbxEx.peer.removeAllItems
  if (!rbGenLoc.selected) {
    for (item <- listTemplateLoc) {
      cbxEx.peer.addItem(item)

      listProjets = listTemplateLoc
    }
  } else {
    for (item <- listTemplateGen) {
      cbxEx.peer.asInstanceOf[JComboBox[String]].addItem(item)
      listProjets = listTemplateGen
    }
  }
  this.tfPNew.text = listProjets.head
  visible = true
  pack

  //listenTo(bOK,bCancel)
  repaint

}