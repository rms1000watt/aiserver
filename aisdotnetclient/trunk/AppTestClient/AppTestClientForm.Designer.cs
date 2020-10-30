namespace AppTestClient
{
    partial class AppTestClientForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.uCommandLabel = new System.Windows.Forms.Label();
            this.uCommandTextBox = new System.Windows.Forms.TextBox();
            this.uEnterButton = new System.Windows.Forms.Button();
            this.uConsoleTextBox = new System.Windows.Forms.TextBox();
            this.uStatusBar = new System.Windows.Forms.StatusStrip();
            this.uStatusError = new System.Windows.Forms.ToolStripStatusLabel();
            this.uStatusOut = new System.Windows.Forms.ToolStripStatusLabel();
            this.uStatusConnection = new System.Windows.Forms.ToolStripStatusLabel();
            this.uAppMenu = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.connectToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.disconnectToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.logonToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.logoffToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.editToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getContextsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getSessionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.setContextToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.openSessionToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.closeSessionToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.connectSessionToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.openContextToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.closeContextToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.addUserToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.editUserToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.listUsersToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.removeUserToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator4 = new System.Windows.Forms.ToolStripSeparator();
            this.getExtentNamesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.setCurrentExtentToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getExtentTypesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getNextLevelToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getPreviousLevelToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getRootLevelToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator5 = new System.Windows.Forms.ToolStripSeparator();
            this.showAllPropertiesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.todayToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator6 = new System.Windows.Forms.ToolStripSeparator();
            this.getExtentTypeOptionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getFullNodeNameToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getNodeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getNodeActionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getNodePathTreeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getNodeSymbolNameToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getNodeTypeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getNodeType2ToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getNumNodesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getNumNodes2ToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.functions2ToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getRequestStatsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getSessionStatsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getServerTypeActionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getSubscriptionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getWorkspaceStatsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getDirInfoToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.uResponseGroup = new System.Windows.Forms.GroupBox();
            this.uOutputGroup = new System.Windows.Forms.GroupBox();
            this.uOutputTextBox = new System.Windows.Forms.TextBox();
            this.uPrefixLabel = new System.Windows.Forms.Label();
            this.uPrefixTextBox = new System.Windows.Forms.TextBox();
            this.getContextParamsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.getContextParamToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.uStatusBar.SuspendLayout();
            this.uAppMenu.SuspendLayout();
            this.uResponseGroup.SuspendLayout();
            this.uOutputGroup.SuspendLayout();
            this.SuspendLayout();
            // 
            // uCommandLabel
            // 
            this.uCommandLabel.AutoSize = true;
            this.uCommandLabel.Location = new System.Drawing.Point(12, 59);
            this.uCommandLabel.Name = "uCommandLabel";
            this.uCommandLabel.Size = new System.Drawing.Size(57, 13);
            this.uCommandLabel.TabIndex = 14;
            this.uCommandLabel.Text = "Command:";
            // 
            // uCommandTextBox
            // 
            this.uCommandTextBox.AcceptsReturn = true;
            this.uCommandTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uCommandTextBox.Location = new System.Drawing.Point(75, 56);
            this.uCommandTextBox.Name = "uCommandTextBox";
            this.uCommandTextBox.Size = new System.Drawing.Size(450, 20);
            this.uCommandTextBox.TabIndex = 0;
            // 
            // uEnterButton
            // 
            this.uEnterButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.uEnterButton.Location = new System.Drawing.Point(531, 54);
            this.uEnterButton.Name = "uEnterButton";
            this.uEnterButton.Size = new System.Drawing.Size(75, 23);
            this.uEnterButton.TabIndex = 1;
            this.uEnterButton.Text = "&Send";
            this.uEnterButton.UseVisualStyleBackColor = true;
            this.uEnterButton.Click += new System.EventHandler(this.uEnterButton_Click);
            // 
            // uConsoleTextBox
            // 
            this.uConsoleTextBox.AcceptsReturn = true;
            this.uConsoleTextBox.AcceptsTab = true;
            this.uConsoleTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uConsoleTextBox.Location = new System.Drawing.Point(6, 19);
            this.uConsoleTextBox.Multiline = true;
            this.uConsoleTextBox.Name = "uConsoleTextBox";
            this.uConsoleTextBox.ReadOnly = true;
            this.uConsoleTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.uConsoleTextBox.Size = new System.Drawing.Size(579, 201);
            this.uConsoleTextBox.TabIndex = 3;
            this.uConsoleTextBox.WordWrap = false;
            // 
            // uStatusBar
            // 
            this.uStatusBar.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.uStatusError,
            this.uStatusOut,
            this.uStatusConnection});
            this.uStatusBar.Location = new System.Drawing.Point(0, 657);
            this.uStatusBar.Name = "uStatusBar";
            this.uStatusBar.Size = new System.Drawing.Size(618, 22);
            this.uStatusBar.TabIndex = 18;
            this.uStatusBar.Text = "Status Bar";
            // 
            // uStatusError
            // 
            this.uStatusError.BorderSides = ((System.Windows.Forms.ToolStripStatusLabelBorderSides)((((System.Windows.Forms.ToolStripStatusLabelBorderSides.Left | System.Windows.Forms.ToolStripStatusLabelBorderSides.Top)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Right)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Bottom)));
            this.uStatusError.Name = "uStatusError";
            this.uStatusError.Size = new System.Drawing.Size(29, 17);
            this.uStatusError.Text = "Err:";
            // 
            // uStatusOut
            // 
            this.uStatusOut.BorderSides = ((System.Windows.Forms.ToolStripStatusLabelBorderSides)((((System.Windows.Forms.ToolStripStatusLabelBorderSides.Left | System.Windows.Forms.ToolStripStatusLabelBorderSides.Top)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Right)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Bottom)));
            this.uStatusOut.Name = "uStatusOut";
            this.uStatusOut.Size = new System.Drawing.Size(33, 17);
            this.uStatusOut.Text = "Out:";
            // 
            // uStatusConnection
            // 
            this.uStatusConnection.BorderSides = ((System.Windows.Forms.ToolStripStatusLabelBorderSides)((((System.Windows.Forms.ToolStripStatusLabelBorderSides.Left | System.Windows.Forms.ToolStripStatusLabelBorderSides.Top)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Right)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Bottom)));
            this.uStatusConnection.Name = "uStatusConnection";
            this.uStatusConnection.Size = new System.Drawing.Size(34, 17);
            this.uStatusConnection.Text = "Con:";
            // 
            // uAppMenu
            // 
            this.uAppMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.editToolStripMenuItem,
            this.functions2ToolStripMenuItem});
            this.uAppMenu.Location = new System.Drawing.Point(0, 0);
            this.uAppMenu.Name = "uAppMenu";
            this.uAppMenu.Size = new System.Drawing.Size(618, 24);
            this.uAppMenu.TabIndex = 27;
            this.uAppMenu.Text = "Main Menu";
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.connectToolStripMenuItem,
            this.disconnectToolStripMenuItem,
            this.logonToolStripMenuItem,
            this.logoffToolStripMenuItem,
            this.exitToolStripMenuItem});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(35, 20);
            this.fileToolStripMenuItem.Text = "File";
            // 
            // connectToolStripMenuItem
            // 
            this.connectToolStripMenuItem.Name = "connectToolStripMenuItem";
            this.connectToolStripMenuItem.Size = new System.Drawing.Size(126, 22);
            this.connectToolStripMenuItem.Text = "&Connect";
            this.connectToolStripMenuItem.Click += new System.EventHandler(this.connectToolStripMenuItem_Click);
            // 
            // disconnectToolStripMenuItem
            // 
            this.disconnectToolStripMenuItem.Name = "disconnectToolStripMenuItem";
            this.disconnectToolStripMenuItem.Size = new System.Drawing.Size(126, 22);
            this.disconnectToolStripMenuItem.Text = "&Disconnect";
            this.disconnectToolStripMenuItem.Click += new System.EventHandler(this.disconnectToolStripMenuItem_Click);
            // 
            // logonToolStripMenuItem
            // 
            this.logonToolStripMenuItem.Name = "logonToolStripMenuItem";
            this.logonToolStripMenuItem.Size = new System.Drawing.Size(126, 22);
            this.logonToolStripMenuItem.Text = "&Logon";
            this.logonToolStripMenuItem.Click += new System.EventHandler(this.logonToolStripMenuItem_Click);
            // 
            // logoffToolStripMenuItem
            // 
            this.logoffToolStripMenuItem.Name = "logoffToolStripMenuItem";
            this.logoffToolStripMenuItem.Size = new System.Drawing.Size(126, 22);
            this.logoffToolStripMenuItem.Text = "Log&off";
            this.logoffToolStripMenuItem.Click += new System.EventHandler(this.logoffToolStripMenuItem_Click);
            // 
            // exitToolStripMenuItem
            // 
            this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
            this.exitToolStripMenuItem.Size = new System.Drawing.Size(126, 22);
            this.exitToolStripMenuItem.Text = "E&xit";
            this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
            // 
            // editToolStripMenuItem
            // 
            this.editToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.getContextsToolStripMenuItem,
            this.getSessionsToolStripMenuItem,
            this.setContextToolStripMenuItem,
            this.toolStripSeparator1,
            this.openSessionToolStripMenuItem,
            this.closeSessionToolStripMenuItem,
            this.connectSessionToolStripMenuItem,
            this.toolStripSeparator2,
            this.openContextToolStripMenuItem,
            this.closeContextToolStripMenuItem,
            this.toolStripSeparator3,
            this.addUserToolStripMenuItem,
            this.editUserToolStripMenuItem,
            this.listUsersToolStripMenuItem,
            this.removeUserToolStripMenuItem,
            this.toolStripSeparator4,
            this.getExtentNamesToolStripMenuItem,
            this.setCurrentExtentToolStripMenuItem,
            this.getExtentTypesToolStripMenuItem,
            this.getNextLevelToolStripMenuItem,
            this.getPreviousLevelToolStripMenuItem,
            this.getRootLevelToolStripMenuItem,
            this.toolStripSeparator5,
            this.showAllPropertiesToolStripMenuItem,
            this.todayToolStripMenuItem,
            this.toolStripSeparator6,
            this.getExtentTypeOptionsToolStripMenuItem,
            this.getFullNodeNameToolStripMenuItem,
            this.getNodeToolStripMenuItem,
            this.getNodeActionsToolStripMenuItem,
            this.getNodePathTreeToolStripMenuItem,
            this.getNodeSymbolNameToolStripMenuItem,
            this.getNodeTypeToolStripMenuItem,
            this.getNodeType2ToolStripMenuItem,
            this.getNumNodesToolStripMenuItem,
            this.getNumNodes2ToolStripMenuItem});
            this.editToolStripMenuItem.Name = "editToolStripMenuItem";
            this.editToolStripMenuItem.Size = new System.Drawing.Size(74, 20);
            this.editToolStripMenuItem.Text = "Functions 1";
            // 
            // getContextsToolStripMenuItem
            // 
            this.getContextsToolStripMenuItem.Name = "getContextsToolStripMenuItem";
            this.getContextsToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getContextsToolStripMenuItem.Text = "Get Contexts";
            this.getContextsToolStripMenuItem.Click += new System.EventHandler(this.getContextsToolStripMenuItem_Click);
            // 
            // getSessionsToolStripMenuItem
            // 
            this.getSessionsToolStripMenuItem.Name = "getSessionsToolStripMenuItem";
            this.getSessionsToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getSessionsToolStripMenuItem.Text = "Get Sessions";
            this.getSessionsToolStripMenuItem.Click += new System.EventHandler(this.getSessionsToolStripMenuItem_Click);
            // 
            // setContextToolStripMenuItem
            // 
            this.setContextToolStripMenuItem.Name = "setContextToolStripMenuItem";
            this.setContextToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.setContextToolStripMenuItem.Text = "Set Context";
            this.setContextToolStripMenuItem.Click += new System.EventHandler(this.setContextToolStripMenuItem_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(190, 6);
            // 
            // openSessionToolStripMenuItem
            // 
            this.openSessionToolStripMenuItem.Name = "openSessionToolStripMenuItem";
            this.openSessionToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.openSessionToolStripMenuItem.Text = "Open Session";
            this.openSessionToolStripMenuItem.Click += new System.EventHandler(this.openSessionToolStripMenuItem_Click);
            // 
            // closeSessionToolStripMenuItem
            // 
            this.closeSessionToolStripMenuItem.Name = "closeSessionToolStripMenuItem";
            this.closeSessionToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.closeSessionToolStripMenuItem.Text = "Close Session";
            this.closeSessionToolStripMenuItem.Click += new System.EventHandler(this.closeSessionToolStripMenuItem_Click);
            // 
            // connectSessionToolStripMenuItem
            // 
            this.connectSessionToolStripMenuItem.Name = "connectSessionToolStripMenuItem";
            this.connectSessionToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.connectSessionToolStripMenuItem.Text = "Connect Session";
            this.connectSessionToolStripMenuItem.Click += new System.EventHandler(this.connectSessionToolStripMenuItem_Click);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(190, 6);
            // 
            // openContextToolStripMenuItem
            // 
            this.openContextToolStripMenuItem.Name = "openContextToolStripMenuItem";
            this.openContextToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.openContextToolStripMenuItem.Text = "Open Context";
            this.openContextToolStripMenuItem.Click += new System.EventHandler(this.openContextToolStripMenuItem_Click);
            // 
            // closeContextToolStripMenuItem
            // 
            this.closeContextToolStripMenuItem.Name = "closeContextToolStripMenuItem";
            this.closeContextToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.closeContextToolStripMenuItem.Text = "Close Context";
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(190, 6);
            // 
            // addUserToolStripMenuItem
            // 
            this.addUserToolStripMenuItem.Name = "addUserToolStripMenuItem";
            this.addUserToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.addUserToolStripMenuItem.Text = "Add User";
            this.addUserToolStripMenuItem.Click += new System.EventHandler(this.addUserToolStripMenuItem_Click);
            // 
            // editUserToolStripMenuItem
            // 
            this.editUserToolStripMenuItem.Name = "editUserToolStripMenuItem";
            this.editUserToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.editUserToolStripMenuItem.Text = "Edit User";
            this.editUserToolStripMenuItem.Click += new System.EventHandler(this.editUserToolStripMenuItem_Click);
            // 
            // listUsersToolStripMenuItem
            // 
            this.listUsersToolStripMenuItem.Name = "listUsersToolStripMenuItem";
            this.listUsersToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.listUsersToolStripMenuItem.Text = "List Users";
            this.listUsersToolStripMenuItem.Click += new System.EventHandler(this.listUsersToolStripMenuItem_Click);
            // 
            // removeUserToolStripMenuItem
            // 
            this.removeUserToolStripMenuItem.Name = "removeUserToolStripMenuItem";
            this.removeUserToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.removeUserToolStripMenuItem.Text = "Remove User";
            this.removeUserToolStripMenuItem.Click += new System.EventHandler(this.removeUserToolStripMenuItem_Click);
            // 
            // toolStripSeparator4
            // 
            this.toolStripSeparator4.Name = "toolStripSeparator4";
            this.toolStripSeparator4.Size = new System.Drawing.Size(190, 6);
            // 
            // getExtentNamesToolStripMenuItem
            // 
            this.getExtentNamesToolStripMenuItem.Name = "getExtentNamesToolStripMenuItem";
            this.getExtentNamesToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getExtentNamesToolStripMenuItem.Text = "Get Extent Names";
            this.getExtentNamesToolStripMenuItem.Click += new System.EventHandler(this.getExtentNamesToolStripMenuItem_Click);
            // 
            // setCurrentExtentToolStripMenuItem
            // 
            this.setCurrentExtentToolStripMenuItem.Name = "setCurrentExtentToolStripMenuItem";
            this.setCurrentExtentToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.setCurrentExtentToolStripMenuItem.Text = "Set Current Extent";
            this.setCurrentExtentToolStripMenuItem.Click += new System.EventHandler(this.setCurrentExtentToolStripMenuItem_Click);
            // 
            // getExtentTypesToolStripMenuItem
            // 
            this.getExtentTypesToolStripMenuItem.Name = "getExtentTypesToolStripMenuItem";
            this.getExtentTypesToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getExtentTypesToolStripMenuItem.Text = "Get Extent Types";
            this.getExtentTypesToolStripMenuItem.Click += new System.EventHandler(this.getExtentTypesToolStripMenuItem_Click);
            // 
            // getNextLevelToolStripMenuItem
            // 
            this.getNextLevelToolStripMenuItem.Name = "getNextLevelToolStripMenuItem";
            this.getNextLevelToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getNextLevelToolStripMenuItem.Text = "Get Next Level";
            this.getNextLevelToolStripMenuItem.Click += new System.EventHandler(this.getNextLevelToolStripMenuItem_Click);
            // 
            // getPreviousLevelToolStripMenuItem
            // 
            this.getPreviousLevelToolStripMenuItem.Name = "getPreviousLevelToolStripMenuItem";
            this.getPreviousLevelToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getPreviousLevelToolStripMenuItem.Text = "Get Previous Level";
            this.getPreviousLevelToolStripMenuItem.Click += new System.EventHandler(this.getPreviousLevelToolStripMenuItem_Click);
            // 
            // getRootLevelToolStripMenuItem
            // 
            this.getRootLevelToolStripMenuItem.Name = "getRootLevelToolStripMenuItem";
            this.getRootLevelToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getRootLevelToolStripMenuItem.Text = "Get Root Level";
            this.getRootLevelToolStripMenuItem.Click += new System.EventHandler(this.getRootLevelToolStripMenuItem_Click);
            // 
            // toolStripSeparator5
            // 
            this.toolStripSeparator5.Name = "toolStripSeparator5";
            this.toolStripSeparator5.Size = new System.Drawing.Size(190, 6);
            // 
            // showAllPropertiesToolStripMenuItem
            // 
            this.showAllPropertiesToolStripMenuItem.Name = "showAllPropertiesToolStripMenuItem";
            this.showAllPropertiesToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.showAllPropertiesToolStripMenuItem.Text = "Show All Properties";
            this.showAllPropertiesToolStripMenuItem.Click += new System.EventHandler(this.showAllPropertiesToolStripMenuItem_Click);
            // 
            // todayToolStripMenuItem
            // 
            this.todayToolStripMenuItem.Name = "todayToolStripMenuItem";
            this.todayToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.todayToolStripMenuItem.Text = "Today";
            this.todayToolStripMenuItem.Click += new System.EventHandler(this.todayToolStripMenuItem_Click);
            // 
            // toolStripSeparator6
            // 
            this.toolStripSeparator6.Name = "toolStripSeparator6";
            this.toolStripSeparator6.Size = new System.Drawing.Size(190, 6);
            // 
            // getExtentTypeOptionsToolStripMenuItem
            // 
            this.getExtentTypeOptionsToolStripMenuItem.Name = "getExtentTypeOptionsToolStripMenuItem";
            this.getExtentTypeOptionsToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getExtentTypeOptionsToolStripMenuItem.Text = "Get Extent Type Options";
            this.getExtentTypeOptionsToolStripMenuItem.Click += new System.EventHandler(this.getExtentTypeOptionsToolStripMenuItem_Click);
            // 
            // getFullNodeNameToolStripMenuItem
            // 
            this.getFullNodeNameToolStripMenuItem.Name = "getFullNodeNameToolStripMenuItem";
            this.getFullNodeNameToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getFullNodeNameToolStripMenuItem.Text = "Get Full Node Name";
            this.getFullNodeNameToolStripMenuItem.Click += new System.EventHandler(this.getFullNodeNameToolStripMenuItem_Click);
            // 
            // getNodeToolStripMenuItem
            // 
            this.getNodeToolStripMenuItem.Name = "getNodeToolStripMenuItem";
            this.getNodeToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getNodeToolStripMenuItem.Text = "Get Node";
            this.getNodeToolStripMenuItem.Click += new System.EventHandler(this.getNodeToolStripMenuItem_Click);
            // 
            // getNodeActionsToolStripMenuItem
            // 
            this.getNodeActionsToolStripMenuItem.Name = "getNodeActionsToolStripMenuItem";
            this.getNodeActionsToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getNodeActionsToolStripMenuItem.Text = "Get Node Actions";
            this.getNodeActionsToolStripMenuItem.Click += new System.EventHandler(this.getNodeActionsToolStripMenuItem_Click);
            // 
            // getNodePathTreeToolStripMenuItem
            // 
            this.getNodePathTreeToolStripMenuItem.Name = "getNodePathTreeToolStripMenuItem";
            this.getNodePathTreeToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getNodePathTreeToolStripMenuItem.Text = "Get Node Path Tree";
            this.getNodePathTreeToolStripMenuItem.Click += new System.EventHandler(this.getNodePathTreeToolStripMenuItem_Click);
            // 
            // getNodeSymbolNameToolStripMenuItem
            // 
            this.getNodeSymbolNameToolStripMenuItem.Name = "getNodeSymbolNameToolStripMenuItem";
            this.getNodeSymbolNameToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getNodeSymbolNameToolStripMenuItem.Text = "Get Node Symbol Name";
            this.getNodeSymbolNameToolStripMenuItem.Click += new System.EventHandler(this.getNodeSymbolNameToolStripMenuItem_Click);
            // 
            // getNodeTypeToolStripMenuItem
            // 
            this.getNodeTypeToolStripMenuItem.Name = "getNodeTypeToolStripMenuItem";
            this.getNodeTypeToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getNodeTypeToolStripMenuItem.Text = "Get Node Type";
            this.getNodeTypeToolStripMenuItem.Click += new System.EventHandler(this.getNodeTypeToolStripMenuItem_Click);
            // 
            // getNodeType2ToolStripMenuItem
            // 
            this.getNodeType2ToolStripMenuItem.Name = "getNodeType2ToolStripMenuItem";
            this.getNodeType2ToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getNodeType2ToolStripMenuItem.Text = "Get Node Type (2)";
            this.getNodeType2ToolStripMenuItem.Click += new System.EventHandler(this.getNodeType2ToolStripMenuItem_Click);
            // 
            // getNumNodesToolStripMenuItem
            // 
            this.getNumNodesToolStripMenuItem.Name = "getNumNodesToolStripMenuItem";
            this.getNumNodesToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getNumNodesToolStripMenuItem.Text = "Get Num Nodes";
            this.getNumNodesToolStripMenuItem.Click += new System.EventHandler(this.getNumNodesToolStripMenuItem_Click);
            // 
            // getNumNodes2ToolStripMenuItem
            // 
            this.getNumNodes2ToolStripMenuItem.Name = "getNumNodes2ToolStripMenuItem";
            this.getNumNodes2ToolStripMenuItem.Size = new System.Drawing.Size(193, 22);
            this.getNumNodes2ToolStripMenuItem.Text = "Get Num Nodes (2)";
            this.getNumNodes2ToolStripMenuItem.Click += new System.EventHandler(this.getNumNodes2ToolStripMenuItem_Click);
            // 
            // functions2ToolStripMenuItem
            // 
            this.functions2ToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.getRequestStatsToolStripMenuItem,
            this.getSessionStatsToolStripMenuItem,
            this.getServerTypeActionsToolStripMenuItem,
            this.getSubscriptionsToolStripMenuItem,
            this.getWorkspaceStatsToolStripMenuItem,
            this.getDirInfoToolStripMenuItem,
            this.getContextParamsToolStripMenuItem,
            this.getContextParamToolStripMenuItem});
            this.functions2ToolStripMenuItem.Name = "functions2ToolStripMenuItem";
            this.functions2ToolStripMenuItem.Size = new System.Drawing.Size(74, 20);
            this.functions2ToolStripMenuItem.Text = "Functions 2";
            // 
            // getRequestStatsToolStripMenuItem
            // 
            this.getRequestStatsToolStripMenuItem.Name = "getRequestStatsToolStripMenuItem";
            this.getRequestStatsToolStripMenuItem.Size = new System.Drawing.Size(191, 22);
            this.getRequestStatsToolStripMenuItem.Text = "Get Request Stats";
            this.getRequestStatsToolStripMenuItem.Click += new System.EventHandler(this.getRequestStatsToolStripMenuItem_Click_1);
            // 
            // getSessionStatsToolStripMenuItem
            // 
            this.getSessionStatsToolStripMenuItem.Name = "getSessionStatsToolStripMenuItem";
            this.getSessionStatsToolStripMenuItem.Size = new System.Drawing.Size(191, 22);
            this.getSessionStatsToolStripMenuItem.Text = "Get Session Stats";
            this.getSessionStatsToolStripMenuItem.Click += new System.EventHandler(this.getSessionStatsToolStripMenuItem_Click);
            // 
            // getServerTypeActionsToolStripMenuItem
            // 
            this.getServerTypeActionsToolStripMenuItem.Name = "getServerTypeActionsToolStripMenuItem";
            this.getServerTypeActionsToolStripMenuItem.Size = new System.Drawing.Size(191, 22);
            this.getServerTypeActionsToolStripMenuItem.Text = "Get Server Type Actions";
            this.getServerTypeActionsToolStripMenuItem.Click += new System.EventHandler(this.getServerTypeActionsToolStripMenuItem_Click);
            // 
            // getSubscriptionsToolStripMenuItem
            // 
            this.getSubscriptionsToolStripMenuItem.Name = "getSubscriptionsToolStripMenuItem";
            this.getSubscriptionsToolStripMenuItem.Size = new System.Drawing.Size(191, 22);
            this.getSubscriptionsToolStripMenuItem.Text = "Get Subscriptions";
            this.getSubscriptionsToolStripMenuItem.Click += new System.EventHandler(this.getSubscriptionsToolStripMenuItem_Click);
            // 
            // getWorkspaceStatsToolStripMenuItem
            // 
            this.getWorkspaceStatsToolStripMenuItem.Name = "getWorkspaceStatsToolStripMenuItem";
            this.getWorkspaceStatsToolStripMenuItem.Size = new System.Drawing.Size(191, 22);
            this.getWorkspaceStatsToolStripMenuItem.Text = "Get Workspace Stats";
            this.getWorkspaceStatsToolStripMenuItem.Click += new System.EventHandler(this.getWorkspaceStatsToolStripMenuItem_Click);
            // 
            // getDirInfoToolStripMenuItem
            // 
            this.getDirInfoToolStripMenuItem.Name = "getDirInfoToolStripMenuItem";
            this.getDirInfoToolStripMenuItem.Size = new System.Drawing.Size(191, 22);
            this.getDirInfoToolStripMenuItem.Text = "Get Dir Info";
            this.getDirInfoToolStripMenuItem.Click += new System.EventHandler(this.getDirInfoToolStripMenuItem_Click);
            // 
            // uResponseGroup
            // 
            this.uResponseGroup.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uResponseGroup.Controls.Add(this.uConsoleTextBox);
            this.uResponseGroup.Location = new System.Drawing.Point(15, 91);
            this.uResponseGroup.Name = "uResponseGroup";
            this.uResponseGroup.Size = new System.Drawing.Size(591, 226);
            this.uResponseGroup.TabIndex = 28;
            this.uResponseGroup.TabStop = false;
            this.uResponseGroup.Text = "Response";
            // 
            // uOutputGroup
            // 
            this.uOutputGroup.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uOutputGroup.Controls.Add(this.uOutputTextBox);
            this.uOutputGroup.Location = new System.Drawing.Point(15, 323);
            this.uOutputGroup.Name = "uOutputGroup";
            this.uOutputGroup.Size = new System.Drawing.Size(591, 331);
            this.uOutputGroup.TabIndex = 29;
            this.uOutputGroup.TabStop = false;
            this.uOutputGroup.Text = "Output";
            // 
            // uOutputTextBox
            // 
            this.uOutputTextBox.AcceptsReturn = true;
            this.uOutputTextBox.AcceptsTab = true;
            this.uOutputTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uOutputTextBox.Location = new System.Drawing.Point(7, 20);
            this.uOutputTextBox.Multiline = true;
            this.uOutputTextBox.Name = "uOutputTextBox";
            this.uOutputTextBox.ReadOnly = true;
            this.uOutputTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.uOutputTextBox.Size = new System.Drawing.Size(578, 305);
            this.uOutputTextBox.TabIndex = 4;
            this.uOutputTextBox.WordWrap = false;
            // 
            // uPrefixLabel
            // 
            this.uPrefixLabel.AutoSize = true;
            this.uPrefixLabel.Location = new System.Drawing.Point(13, 28);
            this.uPrefixLabel.Name = "uPrefixLabel";
            this.uPrefixLabel.Size = new System.Drawing.Size(36, 13);
            this.uPrefixLabel.TabIndex = 30;
            this.uPrefixLabel.Text = "Prefix:";
            // 
            // uPrefixTextBox
            // 
            this.uPrefixTextBox.Location = new System.Drawing.Point(75, 28);
            this.uPrefixTextBox.Name = "uPrefixTextBox";
            this.uPrefixTextBox.Size = new System.Drawing.Size(450, 20);
            this.uPrefixTextBox.TabIndex = 31;
            // 
            // getContextParamsToolStripMenuItem
            // 
            this.getContextParamsToolStripMenuItem.Name = "getContextParamsToolStripMenuItem";
            this.getContextParamsToolStripMenuItem.Size = new System.Drawing.Size(191, 22);
            this.getContextParamsToolStripMenuItem.Text = "Get Context Params";
            this.getContextParamsToolStripMenuItem.Click += new System.EventHandler(this.getContextParamsToolStripMenuItem_Click);
            // 
            // getContextParamToolStripMenuItem
            // 
            this.getContextParamToolStripMenuItem.Name = "getContextParamToolStripMenuItem";
            this.getContextParamToolStripMenuItem.Size = new System.Drawing.Size(191, 22);
            this.getContextParamToolStripMenuItem.Text = "Get Context Param";
            this.getContextParamToolStripMenuItem.Click += new System.EventHandler(this.getContextParamToolStripMenuItem_Click);
            // 
            // AppTestClientForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(618, 679);
            this.Controls.Add(this.uPrefixTextBox);
            this.Controls.Add(this.uPrefixLabel);
            this.Controls.Add(this.uOutputGroup);
            this.Controls.Add(this.uResponseGroup);
            this.Controls.Add(this.uEnterButton);
            this.Controls.Add(this.uCommandTextBox);
            this.Controls.Add(this.uCommandLabel);
            this.Controls.Add(this.uStatusBar);
            this.Controls.Add(this.uAppMenu);
            this.MainMenuStrip = this.uAppMenu;
            this.Name = "AppTestClientForm";
            this.Text = "AppTestClientForm";
            this.uStatusBar.ResumeLayout(false);
            this.uStatusBar.PerformLayout();
            this.uAppMenu.ResumeLayout(false);
            this.uAppMenu.PerformLayout();
            this.uResponseGroup.ResumeLayout(false);
            this.uResponseGroup.PerformLayout();
            this.uOutputGroup.ResumeLayout(false);
            this.uOutputGroup.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label uCommandLabel;
        private System.Windows.Forms.TextBox uCommandTextBox;
        private System.Windows.Forms.Button uEnterButton;
        private System.Windows.Forms.TextBox uConsoleTextBox;
        private System.Windows.Forms.StatusStrip uStatusBar;
        private System.Windows.Forms.ToolStripStatusLabel uStatusError;
        private System.Windows.Forms.MenuStrip uAppMenu;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem connectToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem disconnectToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem logonToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem logoffToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem editToolStripMenuItem;
        private System.Windows.Forms.GroupBox uResponseGroup;
        private System.Windows.Forms.ToolStripStatusLabel uStatusConnection;
        private System.Windows.Forms.ToolStripStatusLabel uStatusOut;
        private System.Windows.Forms.ToolStripMenuItem getSessionsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getContextsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem setContextToolStripMenuItem;
        private System.Windows.Forms.GroupBox uOutputGroup;
        private System.Windows.Forms.TextBox uOutputTextBox;
        private System.Windows.Forms.ToolStripMenuItem openSessionToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem closeSessionToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem connectSessionToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripMenuItem openContextToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem closeContextToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
        private System.Windows.Forms.ToolStripMenuItem addUserToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem editUserToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem listUsersToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem removeUserToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator4;
        private System.Windows.Forms.ToolStripMenuItem getExtentNamesToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem todayToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem setCurrentExtentToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem showAllPropertiesToolStripMenuItem;
        private System.Windows.Forms.Label uPrefixLabel;
        private System.Windows.Forms.TextBox uPrefixTextBox;
        private System.Windows.Forms.ToolStripMenuItem getExtentTypesToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getNextLevelToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator5;
        private System.Windows.Forms.ToolStripMenuItem getExtentTypeOptionsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getFullNodeNameToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getNodeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getNodeActionsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getNodePathTreeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getNodeSymbolNameToolStripMenuItem;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator6;
        private System.Windows.Forms.ToolStripMenuItem getNodeTypeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getNodeType2ToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getNumNodesToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getNumNodes2ToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getPreviousLevelToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getRootLevelToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem functions2ToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getRequestStatsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getServerTypeActionsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getSessionStatsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getSubscriptionsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getWorkspaceStatsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getDirInfoToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getContextParamsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem getContextParamToolStripMenuItem;

    }
}