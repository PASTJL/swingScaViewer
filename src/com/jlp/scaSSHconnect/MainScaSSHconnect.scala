package com.jlp.scaSSHconnect
import java.awt.Dimension
import java.awt.Font
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import java.awt.Insets
import java.awt.Toolkit
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.ItemEvent
import java.awt.event.ItemListener
import java.awt.event.KeyEvent
import java.awt.event.KeyListener
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.io.File
import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.IOException
import java.nio.channels.FileChannel
import java.util.HashMap
import javax.swing.SwingConstants
import javax.swing.DefaultCellEditor
import java.awt.event.ActionListener
import javax.swing.JDialog
import java.awt.event.MouseListener
import java.awt.event.ItemListener
import java.awt.event.KeyListener
import javax.swing.JPanel
import javax.swing.JLabel
import javax.swing.JTabbedPane
import javax.swing.JButton
import javax.swing.JScrollPane
import javax.swing.JTable
import javax.swing.JTextField
import java.util.HashMap
import java.awt.Toolkit
import java.awt.Dimension
import java.io.File
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.parsers.DocumentBuilder
import org.w3c.dom.Document
import org.w3c.dom.NodeList
import org.w3c.dom.Node
import javax.xml.parsers.ParserConfigurationException
import java.io.IOException
import org.xml.sax.SAXException
import javax.swing.table.DefaultTableModel
import javax.swing.table.TableModel
import javax.swing.JPasswordField
import javax.swing.JComboBox
import javax.swing.table.TableColumnModel
import javax.swing.JFileChooser
import javax.swing.filechooser.FileNameExtensionFilter

case class MainScaSSHconnect(currentProject: String, prefixScenario: String) extends JDialog with ActionListener with ItemListener with MouseListener with KeyListener {

  private val serialVersionUID: Long = 1L;

  private var jContentPane: JPanel = null;

  private var jlCreationSSHConnections: JLabel = null;

  private var jtpSsh: JTabbedPane = null;

  private var jbDownload: JButton = null;

  private var jbUpload: JButton = null;

  private var jbSeries: JButton = null;

  private var jbCancel: JButton = null;

  private var jbSave: JButton = null;

  private var jScrollConnexion: JScrollPane = null;

  private var jTableConnexions: JTable = null;

  private var jScrollPaneDownload: JScrollPane = null;
  private var jScrollPaneUpload: JScrollPane = null;

  private var jTableDownloads: JTable = null;
  private var jTableUploads: JTable = null;

  private var jtfLs: JTextField = new JTextField("ls -t");
  private var jtfScp: JTextField = new JTextField("scp");

  initialize();

  this.setVisible(true);
  this.pack();
  repaint();
  private def initialize() {
    val tk: Toolkit = Toolkit.getDefaultToolkit();
    val dm: Dimension = tk.getScreenSize();
    this.setPreferredSize(new Dimension(dm.width - 30, dm.height - 30));
    this.setSize(new Dimension(dm.width - 30, dm.height - 30));
    this.setTitle("Parameter SSH Connections");
    this.setModal(true);
    this.setContentPane(getJContentPane());
    fill();
    jTableUploads.addMouseListener(this);
    jTableConnexions.addKeyListener(this);
    jTableDownloads.addKeyListener(this);
    jTableUploads.addKeyListener(this);
  }

  def getJContentPane(): JPanel = {

    if (jContentPane == null) {

      var gridBagConstraints1 = new GridBagConstraints();
      gridBagConstraints1.fill = GridBagConstraints.BOTH;
      gridBagConstraints1.gridwidth = 4;
      gridBagConstraints1.gridx = 0;
      gridBagConstraints1.gridy = 1;

      gridBagConstraints1.weightx = 1.0;
      gridBagConstraints1.weighty = 1.0;
      gridBagConstraints1.insets = new Insets(12, 31, 19, 46);

      var gridBagConstraints = new GridBagConstraints();
      // gridBagConstraints.insets = new Insets(19, 138, 12, 185);
      gridBagConstraints.gridx = 0;
      gridBagConstraints.gridy = 0;
      gridBagConstraints.ipadx = 5;
      gridBagConstraints.ipady = 5;
      gridBagConstraints.gridwidth = 4;
      gridBagConstraints.fill = GridBagConstraints.BOTH;

      val title = new JPanel();
      title.setLayout(new GridBagLayout());
      var gridBagConstraints5 = new GridBagConstraints();
      // gridBagConstraints5.insets = new Insets(19, 138, 12, 185);
      gridBagConstraints5.gridx = 0;
      gridBagConstraints5.gridy = 0;
      gridBagConstraints5.gridwidth = 4;

      jtfLs.setPreferredSize(new Dimension(200, 20));
      jtfScp.setPreferredSize(new Dimension(200, 20));

      jlCreationSSHConnections = new JLabel();

      jlCreationSSHConnections.setFont(new Font("Times New Roman",
        Font.BOLD, 18));
      jlCreationSSHConnections
        .setHorizontalTextPosition(SwingConstants.CENTER);
      jlCreationSSHConnections
        .setHorizontalAlignment(SwingConstants.CENTER);
      jlCreationSSHConnections.setText("Parameters for SSH Connections ");

      title.add(jlCreationSSHConnections, gridBagConstraints5);
      this.jtfLs.setFont(new Font("Arial", Font.BOLD, 12));
      this.jtfScp.setFont(new Font("Arial", Font.BOLD, 12));
      val jl1 = new JLabel("LS command :");
      jl1.setFont(new Font("Arial", Font.BOLD, 14));
      gridBagConstraints5.gridx = 0;
      gridBagConstraints5.gridy = 1;
      gridBagConstraints5.gridwidth = 1;

      title.add(jl1, gridBagConstraints5);
      gridBagConstraints5.gridx = 1;
      gridBagConstraints5.gridwidth = 2;
      title.add(this.jtfLs, gridBagConstraints5);
      val jl2 = new JLabel("SCP command :");
      jl2.setFont(new Font("Arial", Font.BOLD, 14));
      gridBagConstraints5.gridx = 0;
      gridBagConstraints5.gridy = 2;
      gridBagConstraints5.gridwidth = 1;
      title.add(jl2, gridBagConstraints5);
      gridBagConstraints5.gridx = 1;
      gridBagConstraints5.gridy = 2;
      gridBagConstraints5.gridwidth = 2;
      title.add(this.jtfScp, gridBagConstraints5);

      jContentPane = new JPanel();
      jContentPane.setLayout(new GridBagLayout());
      jContentPane.add(title, gridBagConstraints);
      jContentPane.add(getJtpSsh(), gridBagConstraints1);

      val jpButtons = new JPanel();
      jpButtons.setLayout(new GridBagLayout());
      var gbc1 = new GridBagConstraints();
      gbc1.gridwidth = 4;
      gbc1.fill = GridBagConstraints.BOTH;
      gbc1.gridy = 2;
      gbc1.gridx = 0;
      jContentPane.add(jpButtons, gbc1);

      val gbc2 = new GridBagConstraints();
      gbc2.fill = GridBagConstraints.BOTH;
      gbc2.weightx = 1.0
      gbc2.insets = new Insets(20, 50, 20, 50);
      gbc2.gridy = 0;
      gbc2.gridx = 0;
      jpButtons.add(getJbSave(), gbc2);

      gbc2.gridx = 1;
      jpButtons.add(getJbDownload(), gbc2);
      gbc2.gridx = 2;
      jpButtons.add(getJbUpload(), gbc2);
      gbc2.gridx = 3
      jpButtons.add(getJbSeries(), gbc2);
      gbc2.gridx = 4
      jpButtons.add(getJbCancel(), gbc2);

    }
    jContentPane

  }
  private def getJbCancel(): JButton = {
    if (jbCancel == null) {
      try {
        jbCancel = new JButton();
        jbCancel.setText("Cancel");
        jbCancel.addActionListener(this);
      } catch {
        case e: java.lang.Throwable =>
      }
    }
    return jbCancel;
  }

  private def getJbSeries(): JButton = {
    if (jbSeries == null) {
      try {
        jbSeries = new JButton();
        jbSeries.setText("ChainUplDnl (rank>0)");
        jbSeries.addActionListener(this);
      } catch {
        case e: java.lang.Throwable =>
      }
    }
    return jbSeries;
  }

  private def getJbUpload(): JButton = {
    if (jbUpload == null) {
      try {
        jbUpload = new JButton();
        jbUpload.setText("UploadsOnly (rank=0)");
        jbUpload.addActionListener(this);
      } catch {
        case e: java.lang.Throwable =>
      }
    }
    return jbUpload;
  }

  private def getJbDownload(): JButton = {
    if (jbDownload == null) {
      try {
        jbDownload = new JButton();
        jbDownload.setText("DownloadsOnly (rank=0)");
        jbDownload.addActionListener(this);
      } catch {
        case e: java.lang.Throwable =>
      }
    }
    return jbDownload;
  }

  private def getJbSave(): JButton = {
    if (jbSave == null) {
      try {
        jbSave = new JButton();
        jbSave.setText("Save");
        jbSave.addActionListener(this); // TODO Auto-generated Event
        // stub actionPerformed()

      } catch {
        case e: java.lang.Throwable =>
      }
    }
    return jbSave;
  }

  def getJtpSsh(): JTabbedPane = {

    if (jtpSsh == null) {
      try {
        jtpSsh = new JTabbedPane();

        jtpSsh.addTab("Connections", null, getJScrollConnexion(), "<html><b>List of available connections</b></html>");

        jtpSsh.addTab("Files To upload", null, getJScrollPaneUpload(), "<html><b>Files to upload and optionnaly to execute</b></html>");
        jtpSsh.addTab("Files to download", null, getJScrollPaneDownload(), "<html><b>Files to download and optionnaly compress before download, and/or delete</b></html>");
      } catch {
        case e: java.lang.Throwable =>
      }
    }
    return jtpSsh;

  }
  def getJScrollPaneUpload(): JScrollPane = {

    if (jScrollPaneUpload == null) {
      try {
        jScrollPaneUpload = new JScrollPane();
        jScrollPaneUpload.setViewportView(getJTableUploads());
      } catch {
        case e: java.lang.Throwable =>
      }
    }
    jScrollPaneUpload
  }

  def getJTableUploads(): JTable = {

    if (jTableUploads == null) {
      try {
        val columns: Array[String] = Array("Rank", "Ident Connections", "Local File",
          "Remote file", "Execute file?");
        val dataModel: TableModel = new DefaultTableModel(columns.asInstanceOf[Array[Object]], 100);
        jTableUploads = new JTable(dataModel);
        jTableUploads.getTableHeader().setFont(
          new Font("Arial", Font.BOLD, 14));

        jTableUploads
          .getColumnModel()
          .getColumn(4)
          .setCellRenderer(
            new JComboBoxCellRenderer(Array[String]("No",
              "Yes")));
        jTableUploads
          .getColumnModel()
          .getColumn(4)
          .setCellEditor(
            new DefaultCellEditor(new JComboBox(
              Array[Object]("No", "Yes"))));
        for (i <- 0 until 100) {
          jTableUploads.setValueAt("No", i, 4);
          jTableUploads.setValueAt("0", i, 0);
        }

        val tk = Toolkit.getDefaultToolkit();
        val dm = tk.getScreenSize();
        jScrollPaneUpload.setPreferredSize(new Dimension(dm.width - 40,
          dm.height - 40));

        val dimJsp = this.jScrollPaneUpload.getPreferredSize();
        System.out.println("\nUpload jsp.width= " + dimJsp.width + "\n");
        val dimTable = new Dimension(dimJsp.width - 10,
          dimJsp.height - 10);

        var cMod1: TableColumnModel = jTableUploads.getColumnModel();
        cMod1.getColumn(0).setResizable(true);
        cMod1.getColumn(1).setResizable(true);
        cMod1.getColumn(2).setResizable(true);
        cMod1.getColumn(3).setResizable(true);
        cMod1.getColumn(4).setResizable(true);

        cMod1.getColumn(0).setPreferredWidth(dimTable.width * 2 / 50);
        cMod1.getColumn(1).setPreferredWidth(dimTable.width * 15 / 50);
        cMod1.getColumn(2).setPreferredWidth(dimTable.width * 15 / 50);
        cMod1.getColumn(3).setPreferredWidth(dimTable.width * 15 / 50);
        cMod1.getColumn(4).setPreferredWidth(dimTable.width * 3 / 50);

        /*
				 * cMod.getColumn(0).setWidth(dimTable.width/5);
				 * cMod.getColumn(1).setWidth(dimTable.width/10);
				 * cMod.getColumn(2).setWidth(dimTable.width/2);
				 * cMod.getColumn(3).setWidth(dimTable.width/10);
				 * cMod.getColumn(4).setWidth(dimTable.width/10);
				 */
        jTableUploads.setRowHeight(20);

        jTableUploads.setColumnModel(cMod1);
        jTableUploads.setFont(new Font("Arial", Font.BOLD, 12));

        jTableUploads.repaint();
        this.repaint();
      } catch {
        case e: java.lang.Throwable =>
      }

    }
    jTableUploads

  }
  def getJScrollConnexion(): JScrollPane = {

    if (jScrollConnexion == null) {
      try {
        jScrollConnexion = new JScrollPane();
        jScrollConnexion.setViewportView(getJTableConnexions());
      } catch {
        case e: java.lang.Throwable =>
      }
    }
    return jScrollConnexion;

  }

  def getJTableConnexions(): JTable = {

    if (jTableConnexions == null) {
      try {
        val columns: Array[String] = Array("Ident Connections", "Remote IP Address",
          "Port", "user", "password")
        val dataModel: TableModel = new DefaultTableModel(columns.asInstanceOf[Array[Object]], 100);

        jTableConnexions = new JTable(dataModel);
        jTableConnexions
          .getColumnModel()
          .getColumn(4)
          .setCellRenderer(
            new JPasswordFieldCellRenderer("Password"));
        jTableConnexions
          .getColumnModel()
          .getColumn(4)
          .setCellEditor(
            new DefaultCellEditor(new JPasswordField()));
        jTableConnexions.getTableHeader().setFont(
          new Font("Arial", Font.BOLD, 14));

        val tk = Toolkit.getDefaultToolkit();
        val dm = tk.getScreenSize();
        jScrollConnexion.setPreferredSize(new Dimension(dm.width - 40,
          dm.height - 40));

        val dimJsp = this.jScrollConnexion.getPreferredSize();
        System.out.println("\ncnx jsp.width= " + dimJsp.width + "\n");
        val dimTable = new Dimension(dimJsp.width - 10,
          dimJsp.height - 10);

        var cMod1: TableColumnModel = jTableConnexions.getColumnModel();
        cMod1.getColumn(0).setResizable(true);
        cMod1.getColumn(1).setResizable(true);
        cMod1.getColumn(2).setResizable(true);
        cMod1.getColumn(3).setResizable(true);
        cMod1.getColumn(4).setResizable(true);

        cMod1.getColumn(0).setPreferredWidth(dimTable.width * 3 / 9);
        cMod1.getColumn(1).setPreferredWidth(dimTable.width * 3 / 9);
        cMod1.getColumn(2).setPreferredWidth(dimTable.width * 1 / 9);
        cMod1.getColumn(3).setPreferredWidth(dimTable.width * 1 / 9);
        cMod1.getColumn(4).setPreferredWidth(dimTable.width * 1 / 9);

        /*
				 * cMod.getColumn(0).setWidth(dimTable.width/5);
				 * cMod.getColumn(1).setWidth(dimTable.width/10);
				 * cMod.getColumn(2).setWidth(dimTable.width/2);
				 * cMod.getColumn(3).setWidth(dimTable.width/10);
				 * cMod.getColumn(4).setWidth(dimTable.width/10);
				 */
        jTableConnexions.setRowHeight(20);

        jTableConnexions.setColumnModel(cMod1);

        jTableConnexions.setFont(new Font("Arial", Font.BOLD, 12));

        jTableConnexions.repaint();
        this.repaint();

      } catch {
        case e: java.lang.Throwable =>
      }
    }
    return jTableConnexions;

  }

  def getJScrollPaneDownload(): JScrollPane = {

    if (jScrollPaneDownload == null) {
      try {
        jScrollPaneDownload = new JScrollPane();
        jScrollPaneDownload.setViewportView(getJTableDownloads());
      } catch {
        case e: java.lang.Throwable =>
      }
    }
    return jScrollPaneDownload;

  }

  def getJTableDownloads(): JTable = {

    if (jTableDownloads == null) {
      try {
        val columns: Array[String] = Array("Rank", "Ident Connections", "Files/dir",
          "Pattern of research", "How-many", "Target Dir",
          "Action ?")
        val dataModel: TableModel = new DefaultTableModel(columns.asInstanceOf[Array[Object]], 100);
        jTableDownloads = new JTable(dataModel);
        jTableDownloads.getTableHeader().setFont(
          new Font("Arial", Font.BOLD, 14));
        jTableDownloads
          .getColumnModel()
          .getColumn(2)
          .setCellRenderer(
            new JComboBoxCellRenderer(Array[String](
              "Files", "FilesNoPrefix", "Directory","Directory.jar", "Explicit_Cmd")));
        (jTableDownloads.getColumnModel().getColumn(2)
          .getCellRenderer()).asInstanceOf[JComboBox[String]].addItemListener(this);
        jTableDownloads
          .getColumnModel()
          .getColumn(2)
          .setCellEditor(
            new DefaultCellEditor(new JComboBox(
              Array[Object]("Files", "FilesNoPrefix", "Directory","Directory.jar",
                "Explicit_Cmd"))))

        jTableDownloads
          .getColumnModel()
          .getColumn(5)
          .setCellRenderer(
            new JComboBoxCellRenderer(Array[String](
              "logs", "csv", "reports")));
        jTableDownloads
          .getColumnModel()
          .getColumn(5)
          .setCellEditor(
            new DefaultCellEditor(
              new JComboBox(Array[Object]("logs",
                "csv", "reports"))));

        jTableDownloads
          .getColumnModel()
          .getColumn(6)
          .setCellRenderer(
            new JComboBoxCellRenderer(Array[String]("Nothing", "Compress","Delete")));
        jTableDownloads
          .getColumnModel()
          .getColumn(6)
          .setCellEditor(
            new DefaultCellEditor(new JComboBox(
              Array[Object]("Nothing", "Compress","Delete"))));
        for (i <- 0 until 100) {
          jTableDownloads.setValueAt("0", i, 0);
          jTableDownloads.setValueAt("Files", i, 2);
          jTableDownloads.setValueAt("logs", i, 5);
          jTableDownloads.setValueAt("1", i, 4);
          jTableDownloads.setValueAt("Nothing", i, 6);
        }
        val tk = Toolkit.getDefaultToolkit();
        val dm = tk.getScreenSize();
        jScrollPaneDownload.setPreferredSize(new Dimension(
          dm.width - 40, dm.height - 40));

        val dimJsp = this.jScrollPaneDownload.getPreferredSize();
        System.out.println("jsp.width= " + dimJsp.width);
        val dimTable = new Dimension(dimJsp.width - 10,
          dimJsp.height - 10);
        // jTableDownloads.setPreferredSize(dimTable);

        var cMod: TableColumnModel = jTableDownloads.getColumnModel();
        cMod.getColumn(0).setResizable(true);
        cMod.getColumn(1).setResizable(true);
        cMod.getColumn(2).setResizable(true);
        cMod.getColumn(3).setResizable(true);
        cMod.getColumn(4).setResizable(true);
        cMod.getColumn(5).setResizable(true);
        cMod.getColumn(6).setResizable(true);
        cMod.getColumn(0).setPreferredWidth(dimTable.width / 21)
        cMod.getColumn(1).setPreferredWidth(dimTable.width * 6 / 21);
        cMod.getColumn(2).setPreferredWidth(dimTable.width * 2 / 21);
        cMod.getColumn(3).setPreferredWidth(dimTable.width * 6 / 21);
        cMod.getColumn(4).setPreferredWidth(dimTable.width * 2 / 21);
        cMod.getColumn(5).setPreferredWidth(dimTable.width * 2 / 21);
        cMod.getColumn(6).setPreferredWidth(dimTable.width * 2 / 21);
        /*
				 * cMod.getColumn(0).setWidth(dimTable.width/5);
				 * cMod.getColumn(1).setWidth(dimTable.width/10);
				 * cMod.getColumn(2).setWidth(dimTable.width/2);
				 * cMod.getColumn(3).setWidth(dimTable.width/10);
				 * cMod.getColumn(4).setWidth(dimTable.width/10);
				 */
        jTableDownloads.setRowHeight(20);

        jTableDownloads.setColumnModel(cMod);
        jTableDownloads.setFont(new Font("Arial", Font.BOLD, 12));
        jTableDownloads.repaint();
        this.repaint();
      } catch {
        case e: java.lang.Throwable =>
      }

    }
    jTableDownloads

  }

  def fill() {
    MainScaSSHconnect.hmConn.clear();
    var fullPathConnexionsXml = new StringBuffer(
      System.getProperty("workspace")).append(File.separator)
      .append(currentProject).append(File.separator)
      .append("connections.xml").toString();
    if (new File(fullPathConnexionsXml).exists()) {
      System.out.println("Le fichier des connexions existe, on remplit");
      var fabrique: DocumentBuilderFactory = DocumentBuilderFactory
        .newInstance();

      // creation d'un constructeur de documents
      var constructeur: DocumentBuilder = null
      try {
        constructeur = fabrique.newDocumentBuilder();
        var xml: File = new File(fullPathConnexionsXml);
        var document: Document = constructeur.parse(xml);

        // Mise a jour Commandes ls et scp
        var ndList: NodeList = document.getElementsByTagName("LsCommand");

        System.out.println("lsCommand = "
          + ndList.item(0).getFirstChild().getNodeValue());
        this.jtfLs.setText(ndList.item(0).getFirstChild()
          .getNodeValue());

        MainScaSSHconnect.lsCommand = this.jtfLs.getText();
        ndList = document.getElementsByTagName("ScpCommand");
        this.jtfScp.setText(ndList.item(0).getFirstChild()
          .getNodeValue());

        MainScaSSHconnect.scpCommand = this.jtfScp.getText();

        System.out.println("scpCommand = "
          + ndList.item(0).getFirstChild().getNodeValue());
        // remplissage table des connections
        ndList = document.getElementsByTagName("Connection");
        var taille: Int = ndList.getLength();
        System.out.println("ndList1.lenght =" + ndList.getLength());
        MainScaSSHconnect.hmConn.clear();

        for (i <- 0 until taille) {
          System.out.println("ndList.item(" + i + ") = "
            + ndList.item(i).getNodeName());
          var nd: Node = ndList.item(i);
          var ndList2: NodeList = nd.getChildNodes();
          System.out
            .println("ndList2.lenght =" + ndList2.getLength());
          var serv = "";
          var remote = "";
          var port = "";
          var login = "";
          var passwd = "";
          for (j <- 0 until ndList2.getLength()) {
            System.out.println("ndList2.item(" + j + ") = "
              + ndList2.item(j).getNodeName());
            if (ndList2.item(j).getNodeName()
              .equals("IdConnection")) {
              jTableConnexions.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 0);
              serv = jTableConnexions.getValueAt(i, 0).asInstanceOf[String];

            }
            if (ndList2.item(j).getNodeName()
              .equals("RemoteAddress")) {
              jTableConnexions.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 1);
              remote = jTableConnexions.getValueAt(i, 1).asInstanceOf[String];

            }
            if (ndList2.item(j).getNodeName().equals("IpPort")) {
              jTableConnexions.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 2);
              port = jTableConnexions.getValueAt(i, 2).asInstanceOf[String];

            }
            if (ndList2.item(j).getNodeName().equals("Login")) {
              jTableConnexions.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 3);
              login = jTableConnexions.getValueAt(i, 3).asInstanceOf[String];
            }
            if (ndList2.item(j).getNodeName().equals("Password")) {
              // Gestion sans mot de passe
              if (null != ndList2.item(j)
                .getFirstChild()) {

                jTableConnexions.setValueAt(ndList2.item(j)
                  .getFirstChild().getNodeValue(), i, 4);

                passwd = jTableConnexions.getValueAt(i, 3).asInstanceOf[String];
              } else passwd = "";

            }
          }
          MainScaSSHconnect.hmConn.put(serv, new ConnectionServer(serv,
            remote, port, login, passwd));

        }

        jTableConnexions.repaint();
        var countLines: Int = 0;
        // remplissage table des downloads
        ndList = document.getElementsByTagName("Download");
        taille = ndList.getLength();
        System.out.println("ndList1.lenght =" + ndList.getLength());
        for (i <- 0 until taille) {
          System.out.println("ndList.item(" + i + ") = "
            + ndList.item(i).getNodeName());
          var nd: Node = ndList.item(i);
          var ndList2: NodeList = nd.getChildNodes();
          System.out
            .println("ndList2.lenght =" + ndList2.getLength());
          for (j <- 0 until ndList2.getLength()) {
            System.out.println("ndList2.item(" + j + ") = "
              + ndList2.item(j).getNodeName());

            if (ndList2.item(j).getNodeName()
              .equals("rankCmdDownload")) {
              jTableDownloads.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 0);

            }
            if (ndList2.item(j).getNodeName()
              .equals("IdConnectionDownload")) {
              jTableDownloads.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 1);
              countLines += 1;
            }
            if (ndList2.item(j).getNodeName().equals("DirFile")) {
              jTableDownloads.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 2);

            }
            if (ndList2.item(j).getNodeName().equals("Pattern")) {
              jTableDownloads.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 3);

            }
            if (ndList2.item(j).getNodeName().equals("HowMany")) {
              jTableDownloads.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 4);

            }
            if (ndList2.item(j).getNodeName().equals("Target")) {
              jTableDownloads.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 5);

            }
            if (ndList2.item(j).getNodeName().equals("Compress")) {
              // Compatibilite if NO => Nothing
              // if Yes => Compress
              if (ndList2.item(j)
                .getFirstChild().getNodeValue().toUpperCase()== "NO" )
              jTableDownloads.setValueAt("Nothing", i, 6);
              else if (ndList2.item(j)
                .getFirstChild().getNodeValue().toUpperCase()== "YES")
              {
                 jTableDownloads.setValueAt("Compress", i, 6);
              }
              else
              {
                jTableDownloads.setValueAt(ndList2.item(j).getFirstChild().getNodeValue(),i,6)
              }

            }
          }
        }
        jTableDownloads.repaint();

        System.out.println("countLines= " + countLines);
        if (null != MainScaSSHconnect.tabDownLoad) {
          var len: Int = MainScaSSHconnect.tabDownLoad.length;
          for (k <- 0 until len) {
            MainScaSSHconnect.tabDownLoad(k) = null;
          }
        }
        MainScaSSHconnect.tabDownLoad = null;
        MainScaSSHconnect.tabDownLoad = Array.ofDim(countLines);
        for (i <- 0 until countLines) {

          MainScaSSHconnect.tabDownLoad(i) = new DownloadFile(
            Integer.parseInt(jTableDownloads.getValueAt(i, 0).asInstanceOf[String].trim),

            jTableDownloads.getValueAt(i, 1).asInstanceOf[String],
            jTableDownloads.getValueAt(i, 2).asInstanceOf[String],
            jTableDownloads.getValueAt(i, 3).asInstanceOf[String],
            Integer.parseInt(jTableDownloads.getValueAt(i, 4).asInstanceOf[String]),
            jTableDownloads.getValueAt(i, 5).asInstanceOf[String], jTableDownloads.getValueAt(i, 6).asInstanceOf[String]);

        }

        // Debut JLP
        countLines = 0;
        // remplissage table des upLoads
        ndList = document.getElementsByTagName("Upload");
        taille = ndList.getLength();
        System.out.println("ndList1.lenght =" + ndList.getLength());
        for (i <- 0 until taille) {
          System.out.println("ndList.item(" + i + ") = "
            + ndList.item(i).getNodeName());
          var nd: Node = ndList.item(i);
          var ndList2: NodeList = nd.getChildNodes();
          System.out
            .println("ndList2.lenght =" + ndList2.getLength());
          for (j <- 0 until ndList2.getLength()) {
            System.out.println("ndList2.item(" + j + ") = "
              + ndList2.item(j).getNodeName());

            if (ndList2.item(j).getNodeName()
              .equals("rankCmdUpload")) {
              jTableUploads.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 0);

            }
            if (ndList2.item(j).getNodeName()
              .equals("IdConnectionUpload")) {
              jTableUploads.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 1);
              countLines += 1;
            }
            if (ndList2.item(j).getNodeName().equals("LocalFile")) {
              jTableUploads.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 2);

            }
            if (ndList2.item(j).getNodeName().equals("RemoteFile")) {
              jTableUploads.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 3);

            }

            if (ndList2.item(j).getNodeName().equals("Execute")) {
              jTableUploads.setValueAt(ndList2.item(j)
                .getFirstChild().getNodeValue(), i, 4);

            }
          }
        }
        jTableUploads.repaint();

        System.out.println("countLines= " + countLines);
        if (null != MainScaSSHconnect.tabUpload) {
          var len = MainScaSSHconnect.tabUpload.length;
          for (k <- 0 until len) {
            MainScaSSHconnect.tabUpload(k) = null;
          }
        }
        MainScaSSHconnect.tabUpload = null;
        MainScaSSHconnect.tabUpload = Array.ofDim(countLines);
        for (i <- 0 until countLines) {

          MainScaSSHconnect.tabUpload(i) = new UploadFile(jTableUploads.getValueAt(i, 0).asInstanceOf[String].trim.toInt,
            jTableUploads.getValueAt(i, 1).asInstanceOf[String],
            jTableUploads.getValueAt(i, 2).asInstanceOf[String],
            jTableUploads.getValueAt(i, 3).asInstanceOf[String],
            jTableUploads.getValueAt(i, 4).asInstanceOf[String]);

          // FIN JLP

        }

      } catch {
        case e: ParserConfigurationException => e.printStackTrace();
        case e: SAXException => e.printStackTrace();
        case e: IOException => e.printStackTrace();
      }

      // lecture du contenu d'un fichier XML avec DOM

    } else {
      System.out
        .println("Le fichier des connexions n existe pas, on le cree");
    }

  }

  def actionPerformed(e: ActionEvent) {

    if (e.getSource().isInstanceOf[JButton]) {
      var jb = e.getSource().asInstanceOf[JButton]
      if (jb == this.jbCancel) {
        this.dispose();
      }
      if (jb == this.jbSave) {
        save();
      }
      if (jb == this.jbDownload) {
        save();
        downloads();
      }
      if (jb == this.jbUpload) {
        System.out.println("Clique sur Upload");
        save();
        uploads();
      }
      if (jb == this.jbSeries) {
        System.out.println("Clique sur cmdSerie");
        save();
        cmdSeries();
      }
    }

  }
  def cmdSeries() {
    save();
    setVisible(false);
    this.dispose();

    new CmdSerieDialog(currentProject, prefixScenario);

  }
  def downloads() {
    save();
    setVisible(false);
    this.dispose();

    new DownloadDialog(currentProject, prefixScenario);

  }

  def uploads() {

    // TODO Auto-generated method stub
    save();
    setVisible(false);
    this.dispose();

    new UploadDialog(currentProject);
  }

  def save() {

    // TODO Auto-generated method stub
    var fullPathConnexionsXml = new StringBuffer(
      System.getProperty("workspace")).append(File.separator)
      .append(currentProject).append(File.separator)
      .append("connections.xml").toString();

    System.out.println("Fichier a sauver : " + fullPathConnexionsXml);
    var fos: FileOutputStream = null;
    MainScaSSHconnect.lsCommand = this.getJtfLs().getText();
    MainScaSSHconnect.scpCommand = this.getJtfScp().getText();
    try {

      fos = new FileOutputStream(fullPathConnexionsXml);
      val fChan: FileChannel = fos.getChannel();

      fChan.truncate(0);

      fos.write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
        .getBytes());
      fos.write("<ssh>\n".getBytes());
      fos.write("\t<LsCommand>".getBytes());
      fos.write(this.getJtfLs().getText().getBytes());
      fos.write("</LsCommand>\n".getBytes());
      fos.write("\t<ScpCommand>".getBytes());
      fos.write(this.getJtfScp().getText().getBytes());
      fos.write("</ScpCommand>\n".getBytes());
      fos.write("\t<Connections>\n".getBytes());
      MainScaSSHconnect.hmConn.clear();

      for (i <- 0 until 100) {
        var serv = "";
        var remote = "";
        var port = "";
        var login = "";
        var passwd = "";
        if (null != this.jTableConnexions.getValueAt(i, 0)
          && (this.jTableConnexions.getValueAt(i, 0)).asInstanceOf[String].length() > 0) {
          fos.write("\t\t<Connection>\n".getBytes());
          fos.write("\t\t\t<IdConnection>".getBytes());
          serv = jTableConnexions.getValueAt(i, 0).asInstanceOf[String]
          fos.write(this.jTableConnexions.getValueAt(i, 0).asInstanceOf[String].getBytes());
          fos.write("</IdConnection>\n".getBytes());

          fos.write("\t\t\t<RemoteAddress>".getBytes());
          fos.write(this.jTableConnexions.getValueAt(i, 1).asInstanceOf[String].getBytes());
          remote = jTableConnexions.getValueAt(i, 1).asInstanceOf[String]
          fos.write("</RemoteAddress>\n".getBytes());

          fos.write("\t\t\t<IpPort>".getBytes());
          fos.write(this.jTableConnexions.getValueAt(i, 2).asInstanceOf[String].getBytes());
          port = jTableConnexions.getValueAt(i, 2).asInstanceOf[String]
          fos.write("</IpPort>\n".getBytes());

          fos.write("\t\t\t<Login>".getBytes());
          var logintmp = this.jTableConnexions.getValueAt(i, 3).asInstanceOf[String]
          if (logintmp.contains("&")) {
            logintmp = logintmp.replaceAll("&", "&amp;");
          }
          if (logintmp.contains("<")) {
            logintmp = logintmp.replaceAll("<", "&lt;");
          }
          if (logintmp.contains(">")) {
            logintmp = logintmp.replaceAll(">", "&gt;");
          }
          fos.write(logintmp.getBytes());
          login = jTableConnexions.getValueAt(i, 3).asInstanceOf[String];
          fos.write("</Login>\n".getBytes());

          fos.write("\t\t\t<Password>".getBytes());
          var passtmp = this.jTableConnexions.getValueAt(
            i, 4).asInstanceOf[String]
          if (passtmp.contains("&")) {
            passtmp = passtmp.replaceAll("&", "&amp;");
          }
          if (passtmp.contains("<")) {
            passtmp = passtmp.replaceAll("<", "&lt;");
          }
          if (passtmp.contains(">")) {
            passtmp = passtmp.replaceAll(">", "&gt;");
          }
          fos.write(passtmp.getBytes());
          passwd = jTableConnexions.getValueAt(i, 4).asInstanceOf[String]
          fos.write("</Password>\n".getBytes());
          fos.write("\t\t</Connection>\n".getBytes());

        }
        MainScaSSHconnect.hmConn.put(serv, new ConnectionServer(serv,
          remote, port, login, passwd));
      }
      fos.write("\t</Connections>\n".getBytes());
      fos.write("\t<Downloads>\n".getBytes());

      var countLines: Int = 0;
      for (i <- 0 until 100) {
        if (null != this.jTableDownloads.getValueAt(i, 1)
          && this.jTableDownloads.getValueAt(i, 1).asInstanceOf[String].length() > 0) {
          fos.write("\t\t<Download>\n".getBytes());
          fos.write("\t\t\t<rankCmdDownload>".getBytes());
          fos.write(this.jTableDownloads.getValueAt(i, 0).asInstanceOf[String].trim.getBytes());
          fos.write("</rankCmdDownload>\n".getBytes());
          fos.write("\t\t\t<IdConnectionDownload>".getBytes());
          fos.write(this.jTableDownloads.getValueAt(i, 1).asInstanceOf[String].getBytes());
          fos.write("</IdConnectionDownload>\n".getBytes());
          fos.write("\t\t\t<DirFile>".getBytes());
          fos.write(this.jTableDownloads.getValueAt(i, 2).asInstanceOf[String].getBytes());
          fos.write("</DirFile>\n".getBytes());
          fos.write("\t\t\t<Pattern>".getBytes());
          var cmd = this.jTableDownloads.getValueAt(i, 3).asInstanceOf[String]
          if (cmd.contains("&")) {
            cmd = cmd.replaceAll("&", "&amp;");
          }
          if (cmd.contains("<")) {
            cmd = cmd.replaceAll("<", "&lt;");
          }
          if (cmd.contains(">")) {
            cmd = cmd.replaceAll(">", "&gt;");
          }

          fos.write(cmd.getBytes());
          fos.write("</Pattern>\n".getBytes());

          fos.write("\t\t\t<HowMany>".getBytes());
          fos.write(this.jTableDownloads.getValueAt(i, 4).asInstanceOf[String].trim.getBytes());
          fos.write("</HowMany>\n".getBytes());
          fos.write("\t\t\t<Target>".getBytes());
          fos.write(this.jTableDownloads.getValueAt(i, 5).asInstanceOf[String].getBytes());
          fos.write("</Target>\n".getBytes());
          fos.write("\t\t\t<Compress>".getBytes());
          fos.write(this.jTableDownloads.getValueAt(i, 6).asInstanceOf[String].getBytes());
          fos.write("</Compress>\n".getBytes());
          fos.write("\t\t</Download>\n".getBytes());
          countLines += 1;
        }
      }
      fos.write("\t</Downloads>\n".getBytes());

      // JLP
      var countLines2: Int = 0;
      fos.write("\t<Uploads>\n".getBytes());
      for (i <- 0 until 100) {
        if (null != this.jTableUploads.getValueAt(i, 1)
          && this.jTableUploads.getValueAt(i, 1).asInstanceOf[String].length() > 0) {

          fos.write("\t\t<Upload>\n".getBytes());
          fos.write("\t\t\t<rankCmdUpload>".getBytes());
          fos.write(this.jTableUploads.getValueAt(i, 0).asInstanceOf[String].trim.getBytes());

          fos.write("</rankCmdUpload>\n".getBytes());
          fos.write("\t\t\t<IdConnectionUpload>".getBytes());
          fos.write(this.jTableUploads.getValueAt(i, 1).asInstanceOf[String].getBytes());
          fos.write("</IdConnectionUpload>\n".getBytes());
          fos.write("\t\t\t<LocalFile>".getBytes());
          fos.write(this.jTableUploads.getValueAt(i, 2).asInstanceOf[String].getBytes());
          fos.write("</LocalFile>\n".getBytes());
          fos.write("\t\t\t<RemoteFile>".getBytes());
          var remoteFile = jTableUploads.getValueAt(i, 3).asInstanceOf[String]
          if (remoteFile.contains("&")) {
            remoteFile = remoteFile.replaceAll("&", "&amp;");
          }
          if (remoteFile.contains("<")) {
            remoteFile = remoteFile.replaceAll("<", "&lt;");
          }
          if (remoteFile.contains(">")) {
            remoteFile = remoteFile.replaceAll(">", "&gt;");
          }
          fos.write(remoteFile.getBytes());
          fos.write("</RemoteFile>\n".getBytes());
          fos.write("\t\t\t<Execute>".getBytes());
          fos.write(jTableUploads.getValueAt(i, 4).asInstanceOf[String].getBytes());
          fos.write("</Execute>\n".getBytes());

          fos.write("\t\t</Upload>\n".getBytes());
          countLines2 += 1
        }
      }
      fos.write("\t</Uploads>\n".getBytes());

      // JLP

      fos.write("</ssh>".getBytes());
      fChan.close();
      fos.close();

      // remplissage du tableau des uploads;
      if (null != MainScaSSHconnect.tabUpload) {
        var len = MainScaSSHconnect.tabUpload.length;
        for (k <- 0 until len) {
          MainScaSSHconnect.tabUpload(k) = null;
        }
      }
      MainScaSSHconnect.tabUpload = null;
      MainScaSSHconnect.tabUpload = Array.ofDim(countLines2)
      for (i <- 0 until countLines2) {
        MainScaSSHconnect.tabUpload(i) = new UploadFile(jTableUploads.getValueAt(i, 0).asInstanceOf[String].trim.toInt,
          jTableUploads.getValueAt(i, 1).asInstanceOf[String],
          jTableUploads.getValueAt(i, 2).asInstanceOf[String],
          jTableUploads.getValueAt(i, 3).asInstanceOf[String],
          jTableUploads.getValueAt(i, 4).asInstanceOf[String]);
      }

        // remplissage du tableau des downloads;
        if (null != MainScaSSHconnect.tabDownLoad) {
          var len: Int = MainScaSSHconnect.tabDownLoad.length;
          for (k <- 0 until len) {
            MainScaSSHconnect.tabDownLoad(k) = null;
          }
        }
        MainScaSSHconnect.tabDownLoad = null;
        MainScaSSHconnect.tabDownLoad = Array.ofDim(countLines)
        for (i <- 0 until countLines) {
          MainScaSSHconnect.tabDownLoad(i) = new DownloadFile(jTableDownloads.getValueAt(i, 0).asInstanceOf[String].trim.toInt,
            jTableDownloads.getValueAt(i, 1).asInstanceOf[String],

            jTableDownloads.getValueAt(i, 2).asInstanceOf[String],
            jTableDownloads.getValueAt(i, 3).asInstanceOf[String],
            jTableDownloads.getValueAt(i, 4).asInstanceOf[String].toInt,
            jTableDownloads.getValueAt(i, 5).asInstanceOf[String],
            jTableDownloads.getValueAt(i, 6).asInstanceOf[String])
        }

      
    } catch {
      case e: FileNotFoundException =>
        e.printStackTrace();
      case e: IOException =>

        e.printStackTrace();
    }

  }

  def getJtfLs(): JTextField = {
    return jtfLs;
  }

  def setJtfLs(jtfLs: JTextField) {
    this.jtfLs = jtfLs;
  }

  def getJtfScp(): JTextField = {
    return jtfScp;
  }

  def setJtfScp(jtfScp: JTextField) {
    this.jtfScp = jtfScp;
  }

  def itemStateChanged(e: ItemEvent) {

    // TODO Auto-generated method stub
    if (e.getSource().isInstanceOf[JComboBox[_]]) {
      val jcb = e.getSource().asInstanceOf[JComboBox[String]];
      val value = jcb.getSelectedItem();

      if (value.toString().startsWith("Files")) {
        jcb.setToolTipText("<html><font face=\"sansserif\" color=\"black\" size=\"4\" >Full path to the file,  * character is allowed everywhere in the chain" +
          " <br/> ex : /exec/applis/toto/*/log/accessLog_jonas*  where accessLog_jonas* are some files" +
          "<br/>FilesNoPrefix doesn't prefix the downloaded file with the name of the remote server</font></html>");

      } else if (value.toString().equals("Directory")) {
        jcb.setToolTipText("<html><font face=\"sansserif\" color=\"black\" size=\"4\" >Full path to the directory,  * character is allowed everywhere in the chain" +
          "<br/>ex : /exec/applis/toto/*/log/output* where output* are some directories</font></html>");

      } else if (value.toString().equals("Explicit_Cmd")) {
        jcb.setToolTipText("<html><font face=\"sansserif\" color=\"black\" size=\"4\" >Full command  as below : "

          + "<br><b>/usr/bin/find  /tmp/&lt;aDirectory&gt;/  -type f  -name \"*\"  -print</b> </br></font></html>");

      }

    }

  }

  @Override
  def mouseClicked(e: MouseEvent) {

    if (e.getSource().isInstanceOf[JTable]) {
      val jTable = e.getSource().asInstanceOf[JTable]
      var col = jTable.getSelectedColumn();

      if (col != 2)
        return ;

      if (e.getButton() != MouseEvent.BUTTON1) {

        var row = jTable.getSelectedRow();

        var chooser = new JFileChooser();
        chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        val filter: FileNameExtensionFilter = new FileNameExtensionFilter(
          "Files to Upload", "sh", "bash", "ksh", "class", "jar", "zip", "gz");
        chooser.setFileFilter(filter);
        // String dirWork = System.getProperty("workspace")
        // + File.separator + SwingLogParser.currentProject
        // + File.separator + "uploads";
        var dirWork = System.getProperty("root") + File.separator + "uploads";
        chooser.setCurrentDirectory(new File(dirWork));
        var returnVal = chooser.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
          var file = chooser.getSelectedFile().getAbsolutePath();
          System.out.println("You chose to open this file: "
            + chooser.getSelectedFile().getName());
          jTable.setValueAt(file, row, 2);
        }

        System.out.println("Mouse Clicked on row= " + row + " Column ="
          + col);
      }
    }

  }
  def mousePressed(e: MouseEvent) {
    // TODO Auto-generated method stub

  }

  @Override
  def mouseReleased(e: MouseEvent) {
    // TODO Auto-generated method stub

  }

  @Override
  def mouseEntered(e: MouseEvent) {
    // TODO Auto-generated method stub

  }

  @Override
  def mouseExited(e: MouseEvent) {
    // TODO Auto-generated method stub

  }

  def keyTyped(e: KeyEvent) {
    // TODO Auto-generated method stub

  }

  def keyPressed(e: KeyEvent) {

    if (e.getSource().isInstanceOf[JTable]) {
      if (e.getKeyCode() == KeyEvent.VK_DELETE) {

        val jt = e.getSource().asInstanceOf[JTable];

        val tm = jt.getModel().asInstanceOf[DefaultTableModel]

        val rows: Array[Int] = jt.getSelectedRows();
        for (i <- rows.length until 0 by -1) {
          tm.removeRow(rows(i - 1));
        }

        // ajouter les lignes en fin de tables en fonction de la table
        // jTableConnexions jTableDownloads jTableUploads
        var tabObject: Array[Object] = Array.ofDim(jt.getColumnCount())
        if (jt == jTableConnexions) {
          var str = "";
          tabObject(0) = str;
          tabObject(1) = str;
          tabObject(2) = str;
          tabObject(3) = str;
          tabObject(4) = str;
        }

        if (jt == jTableDownloads) {
          var str = "";
          tabObject(0) = "0";
          tabObject(1) = str
          tabObject(2) = "Files";
          tabObject(3) = str;
          tabObject(4) = "1";
          tabObject(5) = "logs";
          tabObject(6) = "Nothing";
        }
        if (jt == jTableUploads) {
          var str = "";
          tabObject(0) = "0";
          tabObject(1) = str;
          tabObject(2) = str;
          tabObject(3) = str;
          tabObject(4) = "No";

        }
        for (i <- rows.length until 0 by -1) {
          tm.addRow(tabObject);
        }
      }

    }

  }

  def keyReleased(e: KeyEvent) {
    // TODO Auto-generated method stub

  }

}
object MainScaSSHconnect {
  var tabDownLoad: Array[DownloadFile] = null;
  var tabUpload: Array[UploadFile] = null;
  var hmConn: java.util.HashMap[String, ConnectionServer] = new HashMap();
  var lsCommand: String = "ls -t";
  var scpCommand: String = "scp -p ";
}