package com.jlp.scaviewer.ui.dialogs

import scala.swing.Dialog
import scala.swing.Label
import scala.swing.RadioButton
import java.awt.Font
import scala.swing.TextField
import scala.swing.TextArea
import scala.swing.ScrollPane
import scala.swing.Button
import scala.swing.event.ActionEvent
import scala.swing.GridBagPanel
import java.awt.Insets
import java.awt.Toolkit
import java.awt.Dimension
import java.awt.Point
import java.io.File
import com.jlp.scaviewer.ui.SwingScaViewer
import com.jlp.scaviewer.commons.utils.SearchDirFile
import scala.collection.immutable.HashMap
import java.util.Properties
import java.io.FileInputStream
import java.util.ArrayList
import java.io.IOException

class MyDialogMyCommands(pathFile: String) extends Dialog {

  private var commands: java.util.List[String] = new ArrayList[String]()
  val dimlittleTextField = 30
  val dimLargeTextField = 80
  val font1 = new Font("Arial", Font.BOLD, 14)
  val font2 = new Font("Arial", Font.BOLD, 16)
  val font3 = new Font("Arial", Font.BOLD, 20)
  val sizeLitleField = new Dimension(150, 20)
  val sizeLargeField = new Dimension(400, 20)
  val sizePreferredField = new Dimension(300, 20)
 

  var hMotsCles = new HashMap[String, String]()
  // Substituable variable whepattern is <nameVariable>
  // as <libCommands>
  val libCommands = System.getProperty("root") + File.separator + "libCommands"

  var sep = System.getProperty("file.separator")
  
 
  var root = System.getProperty("root")
  //root = root.replaceAll("""\\""", "/")
  println("root=" + root)
  val currentProject = SwingScaViewer.currentProject
  val workspace = System.getProperty("workspace")
  val pathYoungTirDir = SearchDirFile.searchYoungestDir(workspace + File.separator + currentProject + File.separator, """(tir|test)_.*""".r).getAbsolutePath()
  var reports = pathYoungTirDir + File.separator + "reports"
  //  

  reports = reports.replaceAll("""\\""", "/")
  println("reports=" + reports)
  hMotsCles +=("workspace" -> System.getProperty("workspace"))
  hMotsCles += ("libCommands" -> libCommands)
  hMotsCles += ("root" -> root)
  hMotsCles += ("currentProject" -> currentProject)
  hMotsCles += ("pathYoungTirDir" -> pathYoungTirDir)
  hMotsCles += ("reports" -> reports)

  
  
  //TextField System Specific Command
  var osCommand = {
    if (System.getProperty("os.name").toLowerCase.contains("windows")) {

      "cmd /C start "
    } else {
      "ksh "
    }
  }
  val lOSSpecific = new Label(" First OS Specific Command")
  lOSSpecific.font = font2
  val tfOSSpecific = new TextField("", dimLargeTextField)
  tfOSSpecific.font = font1
  tfOSSpecific.maximumSize = sizeLitleField
  tfOSSpecific.minimumSize = sizeLargeField
  tfOSSpecific.preferredSize = sizePreferredField

  // Textfield Java Home
  val lJavaExe = new Label("JAVA_EXE")
  lJavaExe.font = font2
  val tfJavaExe = new TextField("", dimLargeTextField)
  tfJavaExe.font = font1
  tfJavaExe.maximumSize = sizeLitleField
  tfJavaExe.minimumSize = sizeLargeField
  tfJavaExe.preferredSize = sizePreferredField

  // Textfield Working DirectoryJava Home
  val lWorkDir = new Label("Working Directory")
  lWorkDir.font = font2
  val tfWorkDir = new TextField("", dimLargeTextField)
  tfWorkDir.maximumSize = sizeLitleField
  tfWorkDir.minimumSize = sizeLargeField
  tfWorkDir.preferredSize = sizePreferredField
  tfWorkDir.font = font1

  // TextArea parametres JVM
  val lJVMParameters = new Label("JVM parameters")
  lJVMParameters.font = font2
  val taJVMParameters = new TextArea
  taJVMParameters.rows = 10
  taJVMParameters.columns = 80
  taJVMParameters.lineWrap = true
  val spJtextJVM: ScrollPane = new ScrollPane(taJVMParameters)
  spJtextJVM.minimumSize = new Dimension(300, 100)
  spJtextJVM.maximumSize = new Dimension(600, 300)
  spJtextJVM.preferredSize = new Dimension(400, 200)
  taJVMParameters.font = font1

  // TextField Main Class ou archive jar
  val lMainClass = new Label("Main Class or jar archive")
  lMainClass.font = font2
  val tfMainClass = new TextField("", dimLargeTextField)
  tfMainClass.maximumSize = sizeLitleField
  tfMainClass.minimumSize = sizeLargeField
  tfMainClass.preferredSize = sizePreferredField
  tfMainClass.font = font1

  // TextArea parametres Application
  val lAppliParameters = new Label("Application parameters")
  lAppliParameters.font = font2
  val taAppliParameters = new TextArea
  taAppliParameters.rows = 10
  taAppliParameters.columns = 80
  taAppliParameters.lineWrap = true
  val spJtextAppli: ScrollPane = new ScrollPane(taAppliParameters)
  spJtextAppli.minimumSize = new Dimension(300, 100)
  spJtextAppli.maximumSize = new Dimension(600, 300)
  spJtextAppli.preferredSize = new Dimension(400, 200)
  taAppliParameters.font = font1

  // TextArea full commande traduite
  var fullCmd = "";
  val bRefreshCmd = new Button("Refresh cmd")
  bRefreshCmd.font = font2
  val taFullCmd = new TextArea
  taFullCmd.rows = 10
  taFullCmd.columns = 80
  taFullCmd.lineWrap = true
  taFullCmd.editable = false
  val spFullCmd: ScrollPane = new ScrollPane(taFullCmd)
  spFullCmd.minimumSize = new Dimension(300, 100)
  spFullCmd.maximumSize = new Dimension(600, 300)
  spFullCmd.preferredSize = new Dimension(400, 200)

  taFullCmd.font = font1

  val bCancel = new Button("Cancel");
  val bSave = new Button("Save as template");

  val bRun = new Button("Run Programm");

  bCancel.reactions += {
    case ActionEvent(`bCancel`) =>

      dispose
  }
  bSave.reactions += {
    case ActionEvent(`bSave`) =>
      {

        new DialogTemplateMyCommands(tfOSSpecific.text, tfJavaExe.text, tfWorkDir.text, taJVMParameters.text, tfMainClass.text, taAppliParameters.text)
      }
  }
  bRun.reactions += {
    case ActionEvent(`bRun`) =>
      {

        val wkDir=refreshFullCommand
         this.taFullCmd.editable = true
        this.taFullCmd.text = osCommand +tfJavaExe.text+ fullCmd+"\n Work Dir :"+wkDir
        this.taFullCmd.editable = false
        
        commands.clear()
       println("this.tfOSSpecific.text="+this.tfOSSpecific.text)
        for(str <- this.tfOSSpecific.text.split("""\s+""")){commands.add(str)}
        
        
        if (this.tfJavaExe.text.length()> 3 )
        {
         
          println("tfJavaExe.text="+tfJavaExe.text)
          tfJavaExe.text.split("\\s+") foreach( commands.add(_ ) )
          
          
        }
        
        
        for (str <- this.fullCmd.split("""\s+""")){commands.add(str)}
        
        val pb: ProcessBuilder = new ProcessBuilder(commands)
      //  val pb: ProcessBuilder = new ProcessBuilder(this.taFullCmd.text)
        var workDir = this.tfWorkDir.text
        hMotsCles.foreach(tup =>
          workDir = workDir.replaceAll("<" + tup._1 + ">", tup._2.replaceAll("""\\""", "/")))
          println("lancement programme : " +osCommand+" "+" "+ tfJavaExe.text+" "+ fullCmd)
          println("workDir="+workDir)
          if(!new File(workDir).exists()){
            new File(workDir).mkdirs()
          }
        pb.directory(new File(workDir))
        
          val p: Process = pb.start()
         

        dispose

      }
  }
  bRefreshCmd.reactions += {

    case ActionEvent(`bRefreshCmd`) =>
      {
        osCommand = this.tfOSSpecific.text
       val wkDir= refreshFullCommand
        this.taFullCmd.editable = true
        this.taFullCmd.text = osCommand +" "+tfJavaExe.text+" "+ fullCmd+"\n Work Dir :"+wkDir
        this.taFullCmd.editable = false
      }
  }
  // Construction du dialogue

  // Construction du Dialogue
  title = "Launching an external application"
  val jpContentPane = new GridBagPanel
  var gbc1 = new jpContentPane.Constraints
  gbc1.weightx = 1.0
  gbc1.weighty = 0.0
  contents = jpContentPane
  val gbp: GridBagPanel = new GridBagPanel
  gbc1.gridx = 0
  gbc1.gridy = 0
  gbc1.fill = GridBagPanel.Fill.Both
  jpContentPane.layout += ((gbp -> gbc1))
  var gbc = new gbp.Constraints
  gbc.anchor = GridBagPanel.Anchor.West
  gbc.weightx = 1.0
  gbc.weighty = 0.0
  gbc.insets = new Insets(5, 5, 5, 5)

  // Titre
  val lab = new Label("Launching an external application")
  lab.font = font3
  gbc.gridx = 0
  gbc.gridy = 0
  gbc.gridwidth = 3
  gbc.fill = GridBagPanel.Fill.None
  gbc.anchor = GridBagPanel.Anchor.Center
  gbp.layout += ((lab -> gbc))

  //OS Specific command
  gbc.gridx = 0

  gbc.gridy = 1
  gbc.gridwidth = 1
  gbc.fill = GridBagPanel.Fill.None
  gbc.anchor = GridBagPanel.Anchor.East
  gbp.layout += ((lOSSpecific -> gbc))
  gbc.fill = GridBagPanel.Fill.Horizontal
  gbc.gridx = 1
  gbc.gridwidth = 2
  gbc.anchor = GridBagPanel.Anchor.West
  gbp.layout += ((tfOSSpecific -> gbc))

  //JavaHome
  gbc.gridx = 0

  gbc.gridy = 2
  gbc.gridwidth = 1
  gbc.fill = GridBagPanel.Fill.None
  gbc.anchor = GridBagPanel.Anchor.East
  gbp.layout += ((lJavaExe -> gbc))
  gbc.fill = GridBagPanel.Fill.Horizontal
  gbc.gridx = 1
  gbc.gridwidth = 2
  gbc.anchor = GridBagPanel.Anchor.West
  gbp.layout += ((tfJavaExe -> gbc))

  //WorkDir 
  gbc.gridx = 0

  gbc.gridy = 3
  gbc.gridwidth = 1
  gbc.fill = GridBagPanel.Fill.None
  gbc.anchor = GridBagPanel.Anchor.East
  gbp.layout += ((lWorkDir -> gbc))
  gbc.fill = GridBagPanel.Fill.Horizontal
  gbc.gridx = 1
  gbc.gridwidth = 2
  gbc.anchor = GridBagPanel.Anchor.West
  gbp.layout += ((tfWorkDir -> gbc))

  //lJVMParameters
  gbc.gridx = 0

  gbc.gridy = 4
  gbc.gridwidth = 1
  gbc.fill = GridBagPanel.Fill.None
  gbc.anchor = GridBagPanel.Anchor.East
  gbp.layout += ((lJVMParameters -> gbc))
  gbc.fill = GridBagPanel.Fill.Both
  gbc.gridx = 1
  gbc.gridwidth = 2
  gbc.anchor = GridBagPanel.Anchor.West
  gbp.layout += ((spJtextJVM -> gbc))

  // lMainClass
  gbc.gridx = 0

  gbc.gridy = 5
  gbc.gridwidth = 1
  gbc.fill = GridBagPanel.Fill.None
  gbc.anchor = GridBagPanel.Anchor.East
  gbp.layout += ((lMainClass -> gbc))
  gbc.fill = GridBagPanel.Fill.Horizontal
  gbc.gridx = 1
  gbc.gridwidth = 2
  gbc.anchor = GridBagPanel.Anchor.West
  gbp.layout += ((tfMainClass -> gbc))

  //  lAppliParameters
  gbc.gridx = 0

  gbc.gridy = 6
  gbc.gridwidth = 1
  gbc.fill = GridBagPanel.Fill.None
  gbc.anchor = GridBagPanel.Anchor.East
  gbp.layout += ((lAppliParameters -> gbc))
  gbc.fill = GridBagPanel.Fill.Both
  gbc.gridx = 1
  gbc.gridwidth = 2
  gbc.anchor = GridBagPanel.Anchor.West
  gbp.layout += ((spJtextAppli -> gbc))

 
  // Ligne fulll command
  gbc.gridx = 0
  gbc.gridwidth = 1
  gbc.fill = GridBagPanel.Fill.None
  gbc.gridy = 7
  gbc.anchor = GridBagPanel.Anchor.Center
  gbp.layout += ((bRefreshCmd -> gbc))
  gbc.fill = GridBagPanel.Fill.Both
  gbc.gridx = 1
  gbc.gridwidth = 2
  gbc.anchor = GridBagPanel.Anchor.West
  gbp.layout += ((spFullCmd -> gbc))

  // Boutons
  gbc.gridx = 0
  gbc.gridwidth = 1
  gbc.fill = GridBagPanel.Fill.None
  gbc.gridy = 8
  gbc.anchor = GridBagPanel.Anchor.Center
  gbp.layout += ((bCancel -> gbc))
  gbc.gridx = 1
  gbp.layout += ((bSave -> gbc))
  gbc.gridx = 2
  gbp.layout += ((bRun -> gbc))
  // Fin construction

  val dim = Toolkit.getDefaultToolkit().getScreenSize()
  val dimDialog = new Dimension(dim.width * 3 / 4, dim.height * 3 / 4)
  // val dimDialog = new Dimension(600, 700)

  maximumSize = new Dimension(dim.width * 3 / 4, dim.height * 3 / 4)
  minimumSize = new Dimension(dim.width * 1 / 4, dim.height * 1 / 4)
  preferredSize = new Dimension(dim.width * 2 / 3, dim.height * 2 / 3)
  //myDiag.minimumSize = dimDialog

  // traitement du fichier passé
  if (null != pathFile && new File(pathFile).exists) {
    remplirWithFile
  } else {
    remplirDefault
  }

  modal = true
  resizable = true

  location = new Point((dim.getWidth().toInt - dimDialog.getWidth().toInt) / 2, (dim.getHeight().toInt - dimDialog.getHeight().toInt) / 2)
  pack
  visible = true

  private def remplirWithFile {
    val propsCommands = new java.util.Properties()
    propsCommands.load(new FileInputStream(pathFile))
    this.tfJavaExe.text = propsCommands.getProperty("JAVA_EXE", "")
    this.tfOSSpecific.text = propsCommands.getProperty("FIRST_OS_SPECIFIC_COMMAND", this.osCommand)
    this.osCommand = this.tfOSSpecific.text
    this.tfWorkDir.text = propsCommands.getProperty("workDir", "")
    this.tfMainClass.text = propsCommands.getProperty("mainClass", "")
    this.taJVMParameters.text = propsCommands.getProperty("jvmParameters", "")
    this.taAppliParameters.text = propsCommands.getProperty("appliParameters", "")
    
    val wkDir=refreshFullCommand
    this.taFullCmd.editable = true
    this.taFullCmd.text = osCommand+" "+tfJavaExe.text+" " + fullCmd +"\n Work Dir :"+wkDir
    this.taFullCmd.editable = false
  }

  private def remplirDefault {
    this.tfJavaExe.text = System.getProperty("java.home")+"/bin/java"
    this.tfOSSpecific.text = this.osCommand
    println("System.getProperty(\"java.home\")=" + System.getProperty("java.home"))
    this.tfWorkDir.text = "<reports>"

    this.taJVMParameters.text = " -Xms128M -Xmx128M "
     val wkDir=refreshFullCommand
    this.taFullCmd.editable = true
    this.taFullCmd.text = osCommand + fullCmd +"\n Work Dir :"+wkDir
    this.taFullCmd.editable = false
  }
  private def refreshFullCommand () :String={
   
       osCommand=this.tfOSSpecific.text
       val worDirTlanslated={
         var tmpString=this.tfWorkDir.text
         hMotsCles.foreach(tup =>
          tmpString= tmpString.replaceAll("<" + tup._1 + ">", tup._2.replaceAll("""\\""", "/")))
       tmpString
       }
    fullCmd = 
      taJVMParameters.text + " " + tfMainClass.text + " " + taAppliParameters.text
    fullCmd = fullCmd.replaceAll("""\\""", "/")
    hMotsCles.foreach(tup =>
      fullCmd = fullCmd.replaceAll("<" + tup._1 + ">", tup._2.replaceAll("""\\""", "/")))
      
      worDirTlanslated
  }
}