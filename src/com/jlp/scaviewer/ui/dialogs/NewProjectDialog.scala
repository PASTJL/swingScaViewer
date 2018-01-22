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
import com.jlp.scaviewer.commons.utils.CopyFile
import language.postfixOps

class NewProjectDialog(win: MainFrame) extends Dialog(win) {
  val gbp = new GridBagPanel
  modal = true
  this.preferredSize = new Dimension(500, 300)
  this.maximumSize = new Dimension(700, 400)
  var gbc = new gbp.Constraints()
  var insets1 = new Insets(10, 10, 10, 10)
  gbc.insets = insets1
  // titre du dialog
  val ltitle = new Label("Creating a new project")
  ltitle.font = new java.awt.Font("Arial", Font.BOLD, 16)
  gbc.gridx = 0
  gbc.gridy = 0
  gbc.weightx = 1.0
  gbc.gridwidth = 2
  gbc.gridwidth = GridBagConstraints.REMAINDER
  gbc.fill = GridBagPanel.Fill.Horizontal
  gbc.anchor = GridBagPanel.Anchor.Center
  gbp.layout += ((ltitle -> gbc))

  //Liste des projets existants
  gbc.gridwidth = 1
  gbc.gridx = 0
  gbc.gridy = 1

  val pEx = new Label("Existing Project")
  gbp.layout += ((pEx -> gbc))

  var listProjets = List(new File(System.getProperty("workspace")).listFiles(new FileFilter { def accept(file: File) = { file.isDirectory } }): _*) map { _.getName() }
  gbc.gridx = 1
  gbc.gridy = 1
  val cbxEx = new ComboBox(listProjets)
  gbp.layout += ((cbxEx -> gbc))
  // val cbPEx=new 

  // Saisie du nouveau projet et creation de l'arborescence
  gbc.gridx = 0
  gbc.gridy = 2
  val pNew = new Label("New Project")
  gbp.layout += ((pNew -> gbc))

  gbc.gridx = 1
  gbc.gridy = 2

  val tfPNew = new TextField(50)
  gbp.layout += ((tfPNew -> gbc))

  // Button Cancel / OK
  val bOK = new Button("OK")
  val dimBut = new Dimension(80, 30)
  bOK.maximumSize = dimBut
  bOK.minimumSize = dimBut
  bOK.preferredSize = dimBut
  bOK.maximumSize = dimBut
  gbc.fill = GridBagPanel.Fill.None
  gbc.gridx = 0
  gbc.gridy = 3
  gbp.layout += ((bOK -> gbc))
  insets1 = new Insets(10, 10, 10, 10)
  val bCancel = new Button("Cancel")
  bCancel.maximumSize = dimBut
  bCancel.minimumSize = dimBut
  bCancel.preferredSize = dimBut
  bCancel.maximumSize = dimBut
  gbc.weightx = 0
  gbc.gridx = 1
  gbc.gridy = 3
  gbp.layout += ((bCancel -> gbc))

  this.contents = gbp
  var screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
  location = new Point((screenSize.width - size.width) / 2, (screenSize.height - size.height) / 2)

  this.resizable = true
  bOK.reactions += {

    case ActionEvent(`bOK`) => if (tfPNew.text.length() >= 4) {
      if (!listProjets.contains(tfPNew.text)) {

        
        SwingScaViewer.currentProject = tfPNew.text
        win.title = SwingScaViewer.initialTitle

        win.title += " current Project = " + tfPNew.text
        val path = System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject
        new File(path).mkdir()
        new File(path + File.separator + "templates").mkdir()
        new File(path + File.separator + "templates" + File.separator + "filestats").mkdir()
        new File(path + File.separator + "templates" + File.separator + "scaparser").mkdir()
        new File(path + File.separator + "myCommands").mkdir()

        new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject +
          File.separator + "myCommands" + File.separator + "Windows").mkdir
        new File(System.getProperty("workspace") + File.separator + SwingScaViewer.currentProject +
          File.separator + "myCommands" + File.separator + "unix").mkdir

        new File(path + File.separator + "tmp").mkdir()
        
        if (! new File(System.getProperty("root")+File.separator+"myCommands"+File.separator+"Windows").exists)
         new File(System.getProperty("root")+File.separator+"myCommands"+File.separator+"Windows"). mkdir()
         
          if (! new File(System.getProperty("root")+File.separator+"myCommands"+File.separator+"unix").exists)
         new File(System.getProperty("root")+File.separator+"myCommands"+File.separator+"unix"). mkdir()
          var dirGenMyCommands=new File(System.getProperty("root") + File.separator + 
          File.separator + "myCommands")
        
         dirGenMyCommands.listFiles filter (_ isFile) filter ( _.getAbsolutePath().endsWith(".properties")) foreach (_.delete())
         
           // copy Os dependantes
         System.getProperty("os.name").toLowerCase().contains("window") match {
            
            
          case true =>
            {
             
              val dirGenWindows=new File(dirGenMyCommands+File.separator+"Windows")
               dirGenWindows.listFiles filter (_ isFile) filter ( _.getAbsolutePath().endsWith(".properties")) foreach {f =>
                CopyFile.copy(f,new File(dirGenMyCommands+File.separator+f.getName))
              }
            }
          case _ => 
            {
              
              
              val dirGenUnix=new File(dirGenMyCommands+File.separator+"unix")
               dirGenUnix.listFiles filter (_ isFile) filter ( _.getAbsolutePath().endsWith(".properties")) foreach {f =>
                CopyFile.copy(f,new File(dirGenMyCommands+File.separator+f.getName))
              }
            
            }
        
            
          }
         
         
        dispose
      } else {
        JOptionPane.showMessageDialog(null, "This project already exists")
        tfPNew.text = ""
      }
    } else {
      JOptionPane.showMessageDialog(null, "The length of the string must be >= 4")
      tfPNew.text = ""
    }

  }
  bCancel.reactions += {

    case ActionEvent(`bCancel`) =>

      dispose

  }
  visible = true
  pack

  //listenTo(bOK,bCancel)
  repaint

}