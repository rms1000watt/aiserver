namespace AppClientExcel
{
    partial class AppClientExcelControl
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.uStatusBar = new System.Windows.Forms.StatusStrip();
            this.uStatusMessage = new System.Windows.Forms.ToolStripStatusLabel();
            this.uConnectState = new System.Windows.Forms.ToolStripStatusLabel();
            this.uEngineState = new System.Windows.Forms.ToolStripStatusLabel();
            this.uAppTabs = new System.Windows.Forms.TabControl();
            this.uConnectionTab = new System.Windows.Forms.TabPage();
            this.uAcctSettingsGroup = new System.Windows.Forms.GroupBox();
            this.uUsernameLabel = new System.Windows.Forms.Label();
            this.uPasswordText = new System.Windows.Forms.TextBox();
            this.uPasswordLabel = new System.Windows.Forms.Label();
            this.uConnSettingsGroup = new System.Windows.Forms.GroupBox();
            this.uCloseModeLabel = new System.Windows.Forms.Label();
            this.uCloseModeCombo = new System.Windows.Forms.ComboBox();
            this.uHostLabel = new System.Windows.Forms.Label();
            this.uPortText = new System.Windows.Forms.TextBox();
            this.uPortLabel = new System.Windows.Forms.Label();
            this.uLogoffButton = new System.Windows.Forms.Button();
            this.uLogonButton = new System.Windows.Forms.Button();
            this.uSessionTab = new System.Windows.Forms.TabPage();
            this.uActiveSessionsGroup = new System.Windows.Forms.GroupBox();
            this.uCurrentSessionLabel = new System.Windows.Forms.Label();
            this.uCurrentSessionText = new System.Windows.Forms.TextBox();
            this.uConnectSessionButton = new System.Windows.Forms.Button();
            this.uNewSessionButton = new System.Windows.Forms.Button();
            this.uCurrentContextText = new System.Windows.Forms.TextBox();
            this.uCurrentContextLabel = new System.Windows.Forms.Label();
            this.uRefreshSessionsButton = new System.Windows.Forms.Button();
            this.uActiveSessionList = new System.Windows.Forms.ListView();
            this.uColHeader1 = new System.Windows.Forms.ColumnHeader();
            this.uColHeader2 = new System.Windows.Forms.ColumnHeader();
            this.uColHeader3 = new System.Windows.Forms.ColumnHeader();
            this.uColHeader4 = new System.Windows.Forms.ColumnHeader();
            this.uColHeader5 = new System.Windows.Forms.ColumnHeader();
            this.uAvailContextsGroup = new System.Windows.Forms.GroupBox();
            this.uRefreshContextsButton = new System.Windows.Forms.Button();
            this.uAvailContextsList = new System.Windows.Forms.ListView();
            this.uConsoleTab = new System.Windows.Forms.TabPage();
            this.uConsoleText = new System.Windows.Forms.TextBox();
            this.uCommandCombo = new System.Windows.Forms.ComboBox();
            this.uCommandLabel = new System.Windows.Forms.Label();
            this.uEnterButton = new System.Windows.Forms.Button();
            this.uPrefixText = new System.Windows.Forms.TextBox();
            this.uPrefixLabel = new System.Windows.Forms.Label();
            this.uCabinetsTab = new System.Windows.Forms.TabPage();
            this.uAgentsGroup = new System.Windows.Forms.GroupBox();
            this.uNodePathLabel = new System.Windows.Forms.Label();
            this.uNodePathCombo = new System.Windows.Forms.ComboBox();
            this.uCurrentCabinetText = new System.Windows.Forms.TextBox();
            this.uCurrentCabinetLabel = new System.Windows.Forms.Label();
            this.uNodesList = new System.Windows.Forms.ListView();
            this.uColNameHdr = new System.Windows.Forms.ColumnHeader();
            this.uColTypeHdr = new System.Windows.Forms.ColumnHeader();
            this.uColSizeHdr = new System.Windows.Forms.ColumnHeader();
            this.uColDateHdr = new System.Windows.Forms.ColumnHeader();
            this.uColTimeHdr = new System.Windows.Forms.ColumnHeader();
            this.uColVerHdr = new System.Windows.Forms.ColumnHeader();
            this.uColIdHdr = new System.Windows.Forms.ColumnHeader();
            this.uCabinetsGroup = new System.Windows.Forms.GroupBox();
            this.uCabinetsList = new System.Windows.Forms.ListView();
            this.uRefreshCabinetsButton = new System.Windows.Forms.Button();
            this.uLogsTab = new System.Windows.Forms.TabPage();
            this.uLogText = new System.Windows.Forms.TextBox();
            this.uUsernameText = new System.Windows.Forms.TextBox();
            this.uHostText = new System.Windows.Forms.TextBox();
            this.uStatusBar.SuspendLayout();
            this.uAppTabs.SuspendLayout();
            this.uConnectionTab.SuspendLayout();
            this.uAcctSettingsGroup.SuspendLayout();
            this.uConnSettingsGroup.SuspendLayout();
            this.uSessionTab.SuspendLayout();
            this.uActiveSessionsGroup.SuspendLayout();
            this.uAvailContextsGroup.SuspendLayout();
            this.uConsoleTab.SuspendLayout();
            this.uCabinetsTab.SuspendLayout();
            this.uAgentsGroup.SuspendLayout();
            this.uCabinetsGroup.SuspendLayout();
            this.uLogsTab.SuspendLayout();
            this.SuspendLayout();
            // 
            // uStatusBar
            // 
            this.uStatusBar.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.uStatusMessage,
            this.uConnectState,
            this.uEngineState});
            this.uStatusBar.Location = new System.Drawing.Point(0, 636);
            this.uStatusBar.Name = "uStatusBar";
            this.uStatusBar.Size = new System.Drawing.Size(493, 22);
            this.uStatusBar.TabIndex = 1;
            this.uStatusBar.Text = "statusStrip1";
            // 
            // uStatusMessage
            // 
            this.uStatusMessage.BorderSides = ((System.Windows.Forms.ToolStripStatusLabelBorderSides)((((System.Windows.Forms.ToolStripStatusLabelBorderSides.Left | System.Windows.Forms.ToolStripStatusLabelBorderSides.Top)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Right)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Bottom)));
            this.uStatusMessage.Name = "uStatusMessage";
            this.uStatusMessage.Size = new System.Drawing.Size(42, 17);
            this.uStatusMessage.Text = "Status";
            // 
            // uConnectState
            // 
            this.uConnectState.BorderSides = ((System.Windows.Forms.ToolStripStatusLabelBorderSides)((((System.Windows.Forms.ToolStripStatusLabelBorderSides.Left | System.Windows.Forms.ToolStripStatusLabelBorderSides.Top)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Right)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Bottom)));
            this.uConnectState.Name = "uConnectState";
            this.uConnectState.Size = new System.Drawing.Size(75, 17);
            this.uConnectState.Text = "Disconnected";
            // 
            // uEngineState
            // 
            this.uEngineState.BorderSides = ((System.Windows.Forms.ToolStripStatusLabelBorderSides)((((System.Windows.Forms.ToolStripStatusLabelBorderSides.Left | System.Windows.Forms.ToolStripStatusLabelBorderSides.Top)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Right)
                        | System.Windows.Forms.ToolStripStatusLabelBorderSides.Bottom)));
            this.uEngineState.Name = "uEngineState";
            this.uEngineState.Size = new System.Drawing.Size(29, 17);
            this.uEngineState.Text = "Idle";
            // 
            // uAppTabs
            // 
            this.uAppTabs.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uAppTabs.Controls.Add(this.uConnectionTab);
            this.uAppTabs.Controls.Add(this.uSessionTab);
            this.uAppTabs.Controls.Add(this.uConsoleTab);
            this.uAppTabs.Controls.Add(this.uCabinetsTab);
            this.uAppTabs.Controls.Add(this.uLogsTab);
            this.uAppTabs.Location = new System.Drawing.Point(3, 3);
            this.uAppTabs.Name = "uAppTabs";
            this.uAppTabs.SelectedIndex = 0;
            this.uAppTabs.Size = new System.Drawing.Size(480, 630);
            this.uAppTabs.TabIndex = 4;
            // 
            // uConnectionTab
            // 
            this.uConnectionTab.Controls.Add(this.uAcctSettingsGroup);
            this.uConnectionTab.Controls.Add(this.uConnSettingsGroup);
            this.uConnectionTab.Controls.Add(this.uLogoffButton);
            this.uConnectionTab.Controls.Add(this.uLogonButton);
            this.uConnectionTab.Location = new System.Drawing.Point(4, 22);
            this.uConnectionTab.Name = "uConnectionTab";
            this.uConnectionTab.Padding = new System.Windows.Forms.Padding(3);
            this.uConnectionTab.Size = new System.Drawing.Size(472, 604);
            this.uConnectionTab.TabIndex = 0;
            this.uConnectionTab.Text = "Connection";
            this.uConnectionTab.UseVisualStyleBackColor = true;
            // 
            // uAcctSettingsGroup
            // 
            this.uAcctSettingsGroup.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uAcctSettingsGroup.Controls.Add(this.uUsernameText);
            this.uAcctSettingsGroup.Controls.Add(this.uUsernameLabel);
            this.uAcctSettingsGroup.Controls.Add(this.uPasswordText);
            this.uAcctSettingsGroup.Controls.Add(this.uPasswordLabel);
            this.uAcctSettingsGroup.Location = new System.Drawing.Point(5, 113);
            this.uAcctSettingsGroup.Name = "uAcctSettingsGroup";
            this.uAcctSettingsGroup.Size = new System.Drawing.Size(461, 78);
            this.uAcctSettingsGroup.TabIndex = 19;
            this.uAcctSettingsGroup.TabStop = false;
            this.uAcctSettingsGroup.Text = "Account Settings";
            // 
            // uUsernameLabel
            // 
            this.uUsernameLabel.AutoSize = true;
            this.uUsernameLabel.Location = new System.Drawing.Point(6, 22);
            this.uUsernameLabel.Name = "uUsernameLabel";
            this.uUsernameLabel.Size = new System.Drawing.Size(58, 13);
            this.uUsernameLabel.TabIndex = 12;
            this.uUsernameLabel.Text = "Username:";
            // 
            // uPasswordText
            // 
            this.uPasswordText.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uPasswordText.Location = new System.Drawing.Point(67, 45);
            this.uPasswordText.Name = "uPasswordText";
            this.uPasswordText.Size = new System.Drawing.Size(388, 20);
            this.uPasswordText.TabIndex = 14;
            this.uPasswordText.UseSystemPasswordChar = true;
            // 
            // uPasswordLabel
            // 
            this.uPasswordLabel.AutoSize = true;
            this.uPasswordLabel.Location = new System.Drawing.Point(6, 48);
            this.uPasswordLabel.Name = "uPasswordLabel";
            this.uPasswordLabel.Size = new System.Drawing.Size(56, 13);
            this.uPasswordLabel.TabIndex = 15;
            this.uPasswordLabel.Text = "Password:";
            // 
            // uConnSettingsGroup
            // 
            this.uConnSettingsGroup.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uConnSettingsGroup.Controls.Add(this.uCloseModeLabel);
            this.uConnSettingsGroup.Controls.Add(this.uCloseModeCombo);
            this.uConnSettingsGroup.Controls.Add(this.uHostLabel);
            this.uConnSettingsGroup.Controls.Add(this.uHostText);
            this.uConnSettingsGroup.Controls.Add(this.uPortText);
            this.uConnSettingsGroup.Controls.Add(this.uPortLabel);
            this.uConnSettingsGroup.Location = new System.Drawing.Point(6, 6);
            this.uConnSettingsGroup.Name = "uConnSettingsGroup";
            this.uConnSettingsGroup.Size = new System.Drawing.Size(460, 101);
            this.uConnSettingsGroup.TabIndex = 18;
            this.uConnSettingsGroup.TabStop = false;
            this.uConnSettingsGroup.Text = "Connection Settings";
            // 
            // uCloseModeLabel
            // 
            this.uCloseModeLabel.AutoSize = true;
            this.uCloseModeLabel.Location = new System.Drawing.Point(6, 74);
            this.uCloseModeLabel.Name = "uCloseModeLabel";
            this.uCloseModeLabel.Size = new System.Drawing.Size(66, 13);
            this.uCloseModeLabel.TabIndex = 13;
            this.uCloseModeLabel.Text = "Close Mode:";
            // 
            // uCloseModeCombo
            // 
            this.uCloseModeCombo.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.uCloseModeCombo.FormattingEnabled = true;
            this.uCloseModeCombo.Items.AddRange(new object[] {
            "Default",
            "Disconnect",
            "Soft",
            "Firm",
            "Hard"});
            this.uCloseModeCombo.Location = new System.Drawing.Point(78, 71);
            this.uCloseModeCombo.Name = "uCloseModeCombo";
            this.uCloseModeCombo.Size = new System.Drawing.Size(120, 21);
            this.uCloseModeCombo.TabIndex = 12;
            // 
            // uHostLabel
            // 
            this.uHostLabel.AutoSize = true;
            this.uHostLabel.Location = new System.Drawing.Point(6, 22);
            this.uHostLabel.Name = "uHostLabel";
            this.uHostLabel.Size = new System.Drawing.Size(32, 13);
            this.uHostLabel.TabIndex = 9;
            this.uHostLabel.Text = "Host:";
            // 
            // uPortText
            // 
            this.uPortText.DataBindings.Add(new System.Windows.Forms.Binding("Text", global::AppClientExcel.Properties.Settings.Default, "Port", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.uPortText.Location = new System.Drawing.Point(44, 45);
            this.uPortText.Name = "uPortText";
            this.uPortText.Size = new System.Drawing.Size(102, 20);
            this.uPortText.TabIndex = 10;
            this.uPortText.Text = global::AppClientExcel.Properties.Settings.Default.Port;
            // 
            // uPortLabel
            // 
            this.uPortLabel.AutoSize = true;
            this.uPortLabel.Location = new System.Drawing.Point(6, 48);
            this.uPortLabel.Name = "uPortLabel";
            this.uPortLabel.Size = new System.Drawing.Size(29, 13);
            this.uPortLabel.TabIndex = 11;
            this.uPortLabel.Text = "Port:";
            // 
            // uLogoffButton
            // 
            this.uLogoffButton.Location = new System.Drawing.Point(84, 197);
            this.uLogoffButton.Name = "uLogoffButton";
            this.uLogoffButton.Size = new System.Drawing.Size(75, 23);
            this.uLogoffButton.TabIndex = 17;
            this.uLogoffButton.Text = "Log&off";
            this.uLogoffButton.UseVisualStyleBackColor = true;
            // 
            // uLogonButton
            // 
            this.uLogonButton.Location = new System.Drawing.Point(3, 197);
            this.uLogonButton.Name = "uLogonButton";
            this.uLogonButton.Size = new System.Drawing.Size(75, 23);
            this.uLogonButton.TabIndex = 16;
            this.uLogonButton.Text = "&Logon";
            this.uLogonButton.UseVisualStyleBackColor = true;
            // 
            // uSessionTab
            // 
            this.uSessionTab.Controls.Add(this.uActiveSessionsGroup);
            this.uSessionTab.Controls.Add(this.uAvailContextsGroup);
            this.uSessionTab.Location = new System.Drawing.Point(4, 22);
            this.uSessionTab.Name = "uSessionTab";
            this.uSessionTab.Padding = new System.Windows.Forms.Padding(3);
            this.uSessionTab.Size = new System.Drawing.Size(472, 604);
            this.uSessionTab.TabIndex = 3;
            this.uSessionTab.Text = "Session";
            this.uSessionTab.UseVisualStyleBackColor = true;
            // 
            // uActiveSessionsGroup
            // 
            this.uActiveSessionsGroup.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uActiveSessionsGroup.Controls.Add(this.uCurrentSessionLabel);
            this.uActiveSessionsGroup.Controls.Add(this.uCurrentSessionText);
            this.uActiveSessionsGroup.Controls.Add(this.uConnectSessionButton);
            this.uActiveSessionsGroup.Controls.Add(this.uNewSessionButton);
            this.uActiveSessionsGroup.Controls.Add(this.uCurrentContextText);
            this.uActiveSessionsGroup.Controls.Add(this.uCurrentContextLabel);
            this.uActiveSessionsGroup.Controls.Add(this.uRefreshSessionsButton);
            this.uActiveSessionsGroup.Controls.Add(this.uActiveSessionList);
            this.uActiveSessionsGroup.Location = new System.Drawing.Point(7, 128);
            this.uActiveSessionsGroup.Name = "uActiveSessionsGroup";
            this.uActiveSessionsGroup.Size = new System.Drawing.Size(459, 223);
            this.uActiveSessionsGroup.TabIndex = 1;
            this.uActiveSessionsGroup.TabStop = false;
            this.uActiveSessionsGroup.Text = "Active Sessions";
            // 
            // uCurrentSessionLabel
            // 
            this.uCurrentSessionLabel.AutoSize = true;
            this.uCurrentSessionLabel.Location = new System.Drawing.Point(7, 47);
            this.uCurrentSessionLabel.Name = "uCurrentSessionLabel";
            this.uCurrentSessionLabel.Size = new System.Drawing.Size(84, 13);
            this.uCurrentSessionLabel.TabIndex = 7;
            this.uCurrentSessionLabel.Text = "Current Session:";
            // 
            // uCurrentSessionText
            // 
            this.uCurrentSessionText.Enabled = false;
            this.uCurrentSessionText.Location = new System.Drawing.Point(96, 44);
            this.uCurrentSessionText.Name = "uCurrentSessionText";
            this.uCurrentSessionText.Size = new System.Drawing.Size(117, 20);
            this.uCurrentSessionText.TabIndex = 6;
            // 
            // uConnectSessionButton
            // 
            this.uConnectSessionButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.uConnectSessionButton.Location = new System.Drawing.Point(169, 194);
            this.uConnectSessionButton.Name = "uConnectSessionButton";
            this.uConnectSessionButton.Size = new System.Drawing.Size(75, 23);
            this.uConnectSessionButton.TabIndex = 5;
            this.uConnectSessionButton.Text = "Connect";
            this.uConnectSessionButton.UseVisualStyleBackColor = true;
            // 
            // uNewSessionButton
            // 
            this.uNewSessionButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.uNewSessionButton.Location = new System.Drawing.Point(88, 194);
            this.uNewSessionButton.Name = "uNewSessionButton";
            this.uNewSessionButton.Size = new System.Drawing.Size(75, 23);
            this.uNewSessionButton.TabIndex = 4;
            this.uNewSessionButton.Text = "New";
            this.uNewSessionButton.UseVisualStyleBackColor = true;
            // 
            // uCurrentContextText
            // 
            this.uCurrentContextText.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uCurrentContextText.Enabled = false;
            this.uCurrentContextText.Location = new System.Drawing.Point(96, 17);
            this.uCurrentContextText.Name = "uCurrentContextText";
            this.uCurrentContextText.Size = new System.Drawing.Size(357, 20);
            this.uCurrentContextText.TabIndex = 3;
            // 
            // uCurrentContextLabel
            // 
            this.uCurrentContextLabel.AutoSize = true;
            this.uCurrentContextLabel.Location = new System.Drawing.Point(7, 20);
            this.uCurrentContextLabel.Name = "uCurrentContextLabel";
            this.uCurrentContextLabel.Size = new System.Drawing.Size(83, 13);
            this.uCurrentContextLabel.TabIndex = 2;
            this.uCurrentContextLabel.Text = "Current Context:";
            // 
            // uRefreshSessionsButton
            // 
            this.uRefreshSessionsButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.uRefreshSessionsButton.Location = new System.Drawing.Point(7, 194);
            this.uRefreshSessionsButton.Name = "uRefreshSessionsButton";
            this.uRefreshSessionsButton.Size = new System.Drawing.Size(75, 23);
            this.uRefreshSessionsButton.TabIndex = 1;
            this.uRefreshSessionsButton.Text = "Refresh";
            this.uRefreshSessionsButton.UseVisualStyleBackColor = true;
            // 
            // uActiveSessionList
            // 
            this.uActiveSessionList.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uActiveSessionList.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.uColHeader1,
            this.uColHeader2,
            this.uColHeader3,
            this.uColHeader4,
            this.uColHeader5});
            this.uActiveSessionList.FullRowSelect = true;
            this.uActiveSessionList.Location = new System.Drawing.Point(7, 70);
            this.uActiveSessionList.MultiSelect = false;
            this.uActiveSessionList.Name = "uActiveSessionList";
            this.uActiveSessionList.Size = new System.Drawing.Size(446, 118);
            this.uActiveSessionList.TabIndex = 0;
            this.uActiveSessionList.UseCompatibleStateImageBehavior = false;
            this.uActiveSessionList.View = System.Windows.Forms.View.Details;
            // 
            // uColHeader1
            // 
            this.uColHeader1.Text = "ID";
            // 
            // uColHeader2
            // 
            this.uColHeader2.Text = "Admin";
            // 
            // uColHeader3
            // 
            this.uColHeader3.Text = "Owner";
            // 
            // uColHeader4
            // 
            this.uColHeader4.Text = "Mode";
            // 
            // uColHeader5
            // 
            this.uColHeader5.Text = "Protocol";
            // 
            // uAvailContextsGroup
            // 
            this.uAvailContextsGroup.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uAvailContextsGroup.Controls.Add(this.uRefreshContextsButton);
            this.uAvailContextsGroup.Controls.Add(this.uAvailContextsList);
            this.uAvailContextsGroup.Location = new System.Drawing.Point(7, 7);
            this.uAvailContextsGroup.Name = "uAvailContextsGroup";
            this.uAvailContextsGroup.Size = new System.Drawing.Size(459, 115);
            this.uAvailContextsGroup.TabIndex = 0;
            this.uAvailContextsGroup.TabStop = false;
            this.uAvailContextsGroup.Text = "Available Contexts";
            // 
            // uRefreshContextsButton
            // 
            this.uRefreshContextsButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.uRefreshContextsButton.Location = new System.Drawing.Point(7, 86);
            this.uRefreshContextsButton.Name = "uRefreshContextsButton";
            this.uRefreshContextsButton.Size = new System.Drawing.Size(75, 23);
            this.uRefreshContextsButton.TabIndex = 1;
            this.uRefreshContextsButton.Text = "Refresh";
            this.uRefreshContextsButton.UseVisualStyleBackColor = true;
            // 
            // uAvailContextsList
            // 
            this.uAvailContextsList.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uAvailContextsList.FullRowSelect = true;
            this.uAvailContextsList.Location = new System.Drawing.Point(7, 20);
            this.uAvailContextsList.MultiSelect = false;
            this.uAvailContextsList.Name = "uAvailContextsList";
            this.uAvailContextsList.Size = new System.Drawing.Size(446, 60);
            this.uAvailContextsList.TabIndex = 0;
            this.uAvailContextsList.UseCompatibleStateImageBehavior = false;
            this.uAvailContextsList.View = System.Windows.Forms.View.List;
            // 
            // uConsoleTab
            // 
            this.uConsoleTab.Controls.Add(this.uConsoleText);
            this.uConsoleTab.Controls.Add(this.uCommandCombo);
            this.uConsoleTab.Controls.Add(this.uCommandLabel);
            this.uConsoleTab.Controls.Add(this.uEnterButton);
            this.uConsoleTab.Controls.Add(this.uPrefixText);
            this.uConsoleTab.Controls.Add(this.uPrefixLabel);
            this.uConsoleTab.Location = new System.Drawing.Point(4, 22);
            this.uConsoleTab.Name = "uConsoleTab";
            this.uConsoleTab.Padding = new System.Windows.Forms.Padding(3);
            this.uConsoleTab.Size = new System.Drawing.Size(472, 604);
            this.uConsoleTab.TabIndex = 1;
            this.uConsoleTab.Text = "Console";
            this.uConsoleTab.UseVisualStyleBackColor = true;
            // 
            // uConsoleText
            // 
            this.uConsoleText.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uConsoleText.Location = new System.Drawing.Point(6, 62);
            this.uConsoleText.Multiline = true;
            this.uConsoleText.Name = "uConsoleText";
            this.uConsoleText.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.uConsoleText.Size = new System.Drawing.Size(460, 541);
            this.uConsoleText.TabIndex = 6;
            this.uConsoleText.WordWrap = false;
            // 
            // uCommandCombo
            // 
            this.uCommandCombo.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uCommandCombo.FormattingEnabled = true;
            this.uCommandCombo.Location = new System.Drawing.Point(71, 34);
            this.uCommandCombo.Name = "uCommandCombo";
            this.uCommandCombo.Size = new System.Drawing.Size(347, 21);
            this.uCommandCombo.TabIndex = 5;
            // 
            // uCommandLabel
            // 
            this.uCommandLabel.AutoSize = true;
            this.uCommandLabel.Location = new System.Drawing.Point(7, 37);
            this.uCommandLabel.Name = "uCommandLabel";
            this.uCommandLabel.Size = new System.Drawing.Size(57, 13);
            this.uCommandLabel.TabIndex = 4;
            this.uCommandLabel.Text = "Command:";
            // 
            // uEnterButton
            // 
            this.uEnterButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.uEnterButton.AutoSize = true;
            this.uEnterButton.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.uEnterButton.Location = new System.Drawing.Point(424, 32);
            this.uEnterButton.Name = "uEnterButton";
            this.uEnterButton.Size = new System.Drawing.Size(42, 23);
            this.uEnterButton.TabIndex = 3;
            this.uEnterButton.Text = "Enter";
            this.uEnterButton.UseVisualStyleBackColor = true;
            // 
            // uPrefixText
            // 
            this.uPrefixText.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uPrefixText.Location = new System.Drawing.Point(70, 7);
            this.uPrefixText.Name = "uPrefixText";
            this.uPrefixText.Size = new System.Drawing.Size(348, 20);
            this.uPrefixText.TabIndex = 1;
            // 
            // uPrefixLabel
            // 
            this.uPrefixLabel.AutoSize = true;
            this.uPrefixLabel.Location = new System.Drawing.Point(28, 10);
            this.uPrefixLabel.Name = "uPrefixLabel";
            this.uPrefixLabel.Size = new System.Drawing.Size(36, 13);
            this.uPrefixLabel.TabIndex = 0;
            this.uPrefixLabel.Text = "Prefix:";
            // 
            // uCabinetsTab
            // 
            this.uCabinetsTab.Controls.Add(this.uAgentsGroup);
            this.uCabinetsTab.Controls.Add(this.uCabinetsGroup);
            this.uCabinetsTab.Location = new System.Drawing.Point(4, 22);
            this.uCabinetsTab.Name = "uCabinetsTab";
            this.uCabinetsTab.Padding = new System.Windows.Forms.Padding(3);
            this.uCabinetsTab.Size = new System.Drawing.Size(472, 604);
            this.uCabinetsTab.TabIndex = 4;
            this.uCabinetsTab.Text = "Cabinets";
            this.uCabinetsTab.UseVisualStyleBackColor = true;
            // 
            // uAgentsGroup
            // 
            this.uAgentsGroup.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uAgentsGroup.Controls.Add(this.uNodePathLabel);
            this.uAgentsGroup.Controls.Add(this.uNodePathCombo);
            this.uAgentsGroup.Controls.Add(this.uCurrentCabinetText);
            this.uAgentsGroup.Controls.Add(this.uCurrentCabinetLabel);
            this.uAgentsGroup.Controls.Add(this.uNodesList);
            this.uAgentsGroup.Location = new System.Drawing.Point(7, 139);
            this.uAgentsGroup.Name = "uAgentsGroup";
            this.uAgentsGroup.Size = new System.Drawing.Size(459, 459);
            this.uAgentsGroup.TabIndex = 1;
            this.uAgentsGroup.TabStop = false;
            this.uAgentsGroup.Text = "Agents";
            // 
            // uNodePathLabel
            // 
            this.uNodePathLabel.AutoSize = true;
            this.uNodePathLabel.Location = new System.Drawing.Point(6, 50);
            this.uNodePathLabel.Name = "uNodePathLabel";
            this.uNodePathLabel.Size = new System.Drawing.Size(32, 13);
            this.uNodePathLabel.TabIndex = 4;
            this.uNodePathLabel.Text = "Path:";
            // 
            // uNodePathCombo
            // 
            this.uNodePathCombo.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.uNodePathCombo.FormattingEnabled = true;
            this.uNodePathCombo.Location = new System.Drawing.Point(97, 47);
            this.uNodePathCombo.Name = "uNodePathCombo";
            this.uNodePathCombo.Size = new System.Drawing.Size(155, 21);
            this.uNodePathCombo.TabIndex = 3;
            // 
            // uCurrentCabinetText
            // 
            this.uCurrentCabinetText.Enabled = false;
            this.uCurrentCabinetText.Location = new System.Drawing.Point(97, 20);
            this.uCurrentCabinetText.Name = "uCurrentCabinetText";
            this.uCurrentCabinetText.Size = new System.Drawing.Size(155, 20);
            this.uCurrentCabinetText.TabIndex = 2;
            // 
            // uCurrentCabinetLabel
            // 
            this.uCurrentCabinetLabel.AutoSize = true;
            this.uCurrentCabinetLabel.Location = new System.Drawing.Point(4, 23);
            this.uCurrentCabinetLabel.Name = "uCurrentCabinetLabel";
            this.uCurrentCabinetLabel.Size = new System.Drawing.Size(83, 13);
            this.uCurrentCabinetLabel.TabIndex = 1;
            this.uCurrentCabinetLabel.Text = "Current Cabinet:";
            // 
            // uNodesList
            // 
            this.uNodesList.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uNodesList.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.uColNameHdr,
            this.uColTypeHdr,
            this.uColSizeHdr,
            this.uColDateHdr,
            this.uColTimeHdr,
            this.uColVerHdr,
            this.uColIdHdr});
            this.uNodesList.Location = new System.Drawing.Point(6, 83);
            this.uNodesList.Name = "uNodesList";
            this.uNodesList.Size = new System.Drawing.Size(447, 370);
            this.uNodesList.TabIndex = 0;
            this.uNodesList.UseCompatibleStateImageBehavior = false;
            this.uNodesList.View = System.Windows.Forms.View.Details;
            // 
            // uColNameHdr
            // 
            this.uColNameHdr.Text = "Name/Value";
            // 
            // uColTypeHdr
            // 
            this.uColTypeHdr.Text = "Type";
            // 
            // uColSizeHdr
            // 
            this.uColSizeHdr.Text = "Size";
            // 
            // uColDateHdr
            // 
            this.uColDateHdr.Text = "Date";
            // 
            // uColTimeHdr
            // 
            this.uColTimeHdr.Text = "Time";
            // 
            // uColVerHdr
            // 
            this.uColVerHdr.Text = "Version";
            // 
            // uColIdHdr
            // 
            this.uColIdHdr.Text = "Id";
            // 
            // uCabinetsGroup
            // 
            this.uCabinetsGroup.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uCabinetsGroup.Controls.Add(this.uCabinetsList);
            this.uCabinetsGroup.Controls.Add(this.uRefreshCabinetsButton);
            this.uCabinetsGroup.Location = new System.Drawing.Point(7, 7);
            this.uCabinetsGroup.Name = "uCabinetsGroup";
            this.uCabinetsGroup.Size = new System.Drawing.Size(459, 126);
            this.uCabinetsGroup.TabIndex = 0;
            this.uCabinetsGroup.TabStop = false;
            this.uCabinetsGroup.Text = "Cabinets";
            // 
            // uCabinetsList
            // 
            this.uCabinetsList.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uCabinetsList.Location = new System.Drawing.Point(7, 20);
            this.uCabinetsList.MultiSelect = false;
            this.uCabinetsList.Name = "uCabinetsList";
            this.uCabinetsList.Size = new System.Drawing.Size(446, 69);
            this.uCabinetsList.TabIndex = 2;
            this.uCabinetsList.UseCompatibleStateImageBehavior = false;
            this.uCabinetsList.View = System.Windows.Forms.View.List;
            // 
            // uRefreshCabinetsButton
            // 
            this.uRefreshCabinetsButton.Location = new System.Drawing.Point(7, 95);
            this.uRefreshCabinetsButton.Name = "uRefreshCabinetsButton";
            this.uRefreshCabinetsButton.Size = new System.Drawing.Size(75, 23);
            this.uRefreshCabinetsButton.TabIndex = 1;
            this.uRefreshCabinetsButton.Text = "Refresh";
            this.uRefreshCabinetsButton.UseVisualStyleBackColor = true;
            // 
            // uLogsTab
            // 
            this.uLogsTab.Controls.Add(this.uLogText);
            this.uLogsTab.Location = new System.Drawing.Point(4, 22);
            this.uLogsTab.Name = "uLogsTab";
            this.uLogsTab.Padding = new System.Windows.Forms.Padding(3);
            this.uLogsTab.Size = new System.Drawing.Size(472, 604);
            this.uLogsTab.TabIndex = 2;
            this.uLogsTab.Text = "Logs";
            this.uLogsTab.UseVisualStyleBackColor = true;
            // 
            // uLogText
            // 
            this.uLogText.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uLogText.Location = new System.Drawing.Point(6, 6);
            this.uLogText.Multiline = true;
            this.uLogText.Name = "uLogText";
            this.uLogText.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.uLogText.Size = new System.Drawing.Size(460, 592);
            this.uLogText.TabIndex = 1;
            this.uLogText.WordWrap = false;
            // 
            // uUsernameText
            // 
            this.uUsernameText.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uUsernameText.DataBindings.Add(new System.Windows.Forms.Binding("Text", global::AppClientExcel.Properties.Settings.Default, "Username", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.uUsernameText.Location = new System.Drawing.Point(67, 19);
            this.uUsernameText.Name = "uUsernameText";
            this.uUsernameText.Size = new System.Drawing.Size(388, 20);
            this.uUsernameText.TabIndex = 13;
            this.uUsernameText.Text = global::AppClientExcel.Properties.Settings.Default.Username;
            // 
            // uHostText
            // 
            this.uHostText.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uHostText.DataBindings.Add(new System.Windows.Forms.Binding("Text", global::AppClientExcel.Properties.Settings.Default, "Host", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.uHostText.Location = new System.Drawing.Point(44, 19);
            this.uHostText.Name = "uHostText";
            this.uHostText.Size = new System.Drawing.Size(410, 20);
            this.uHostText.TabIndex = 8;
            this.uHostText.Text = global::AppClientExcel.Properties.Settings.Default.Host;
            // 
            // AppClientExcelControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.uAppTabs);
            this.Controls.Add(this.uStatusBar);
            this.Name = "AppClientExcelControl";
            this.Size = new System.Drawing.Size(493, 658);
            this.uStatusBar.ResumeLayout(false);
            this.uStatusBar.PerformLayout();
            this.uAppTabs.ResumeLayout(false);
            this.uConnectionTab.ResumeLayout(false);
            this.uAcctSettingsGroup.ResumeLayout(false);
            this.uAcctSettingsGroup.PerformLayout();
            this.uConnSettingsGroup.ResumeLayout(false);
            this.uConnSettingsGroup.PerformLayout();
            this.uSessionTab.ResumeLayout(false);
            this.uActiveSessionsGroup.ResumeLayout(false);
            this.uActiveSessionsGroup.PerformLayout();
            this.uAvailContextsGroup.ResumeLayout(false);
            this.uConsoleTab.ResumeLayout(false);
            this.uConsoleTab.PerformLayout();
            this.uCabinetsTab.ResumeLayout(false);
            this.uAgentsGroup.ResumeLayout(false);
            this.uAgentsGroup.PerformLayout();
            this.uCabinetsGroup.ResumeLayout(false);
            this.uLogsTab.ResumeLayout(false);
            this.uLogsTab.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.StatusStrip uStatusBar;
        private System.Windows.Forms.ToolStripStatusLabel uStatusMessage;
        private System.Windows.Forms.TabControl uAppTabs;
        private System.Windows.Forms.TabPage uConnectionTab;
        private System.Windows.Forms.TabPage uConsoleTab;
        private System.Windows.Forms.Button uLogoffButton;
        private System.Windows.Forms.TextBox uHostText;
        private System.Windows.Forms.Button uLogonButton;
        private System.Windows.Forms.Label uHostLabel;
        private System.Windows.Forms.Label uPasswordLabel;
        private System.Windows.Forms.Label uPortLabel;
        private System.Windows.Forms.TextBox uPasswordText;
        private System.Windows.Forms.TextBox uPortText;
        private System.Windows.Forms.TextBox uUsernameText;
        private System.Windows.Forms.Label uUsernameLabel;
        private System.Windows.Forms.TabPage uLogsTab;
        private System.Windows.Forms.TextBox uLogText;
        private System.Windows.Forms.GroupBox uAcctSettingsGroup;
        private System.Windows.Forms.GroupBox uConnSettingsGroup;
        private System.Windows.Forms.TabPage uSessionTab;
        private System.Windows.Forms.GroupBox uAvailContextsGroup;
        private System.Windows.Forms.ListView uAvailContextsList;
        private System.Windows.Forms.Button uRefreshContextsButton;
        private System.Windows.Forms.GroupBox uActiveSessionsGroup;
        private System.Windows.Forms.ListView uActiveSessionList;
        private System.Windows.Forms.ColumnHeader uColHeader1;
        private System.Windows.Forms.ColumnHeader uColHeader2;
        private System.Windows.Forms.ColumnHeader uColHeader3;
        private System.Windows.Forms.ColumnHeader uColHeader4;
        private System.Windows.Forms.ColumnHeader uColHeader5;
        private System.Windows.Forms.Button uRefreshSessionsButton;
        private System.Windows.Forms.TextBox uCurrentContextText;
        private System.Windows.Forms.Label uCurrentContextLabel;
        private System.Windows.Forms.Button uNewSessionButton;
        private System.Windows.Forms.Button uConnectSessionButton;
        private System.Windows.Forms.Label uCurrentSessionLabel;
        private System.Windows.Forms.TextBox uCurrentSessionText;
        private System.Windows.Forms.Label uCloseModeLabel;
        private System.Windows.Forms.ComboBox uCloseModeCombo;
        private System.Windows.Forms.ToolStripStatusLabel uConnectState;
        private System.Windows.Forms.TextBox uPrefixText;
        private System.Windows.Forms.Label uPrefixLabel;
        private System.Windows.Forms.ComboBox uCommandCombo;
        private System.Windows.Forms.Label uCommandLabel;
        private System.Windows.Forms.Button uEnterButton;
        private System.Windows.Forms.TextBox uConsoleText;
        private System.Windows.Forms.ToolStripStatusLabel uEngineState;
        private System.Windows.Forms.TabPage uCabinetsTab;
        private System.Windows.Forms.GroupBox uCabinetsGroup;
        private System.Windows.Forms.GroupBox uAgentsGroup;
        private System.Windows.Forms.ListView uNodesList;
        private System.Windows.Forms.Button uRefreshCabinetsButton;
        private System.Windows.Forms.ListView uCabinetsList;
        private System.Windows.Forms.Label uCurrentCabinetLabel;
        private System.Windows.Forms.TextBox uCurrentCabinetText;
        private System.Windows.Forms.Label uNodePathLabel;
        private System.Windows.Forms.ComboBox uNodePathCombo;
        private System.Windows.Forms.ColumnHeader uColNameHdr;
        private System.Windows.Forms.ColumnHeader uColTypeHdr;
        private System.Windows.Forms.ColumnHeader uColSizeHdr;
        private System.Windows.Forms.ColumnHeader uColDateHdr;
        private System.Windows.Forms.ColumnHeader uColTimeHdr;
        private System.Windows.Forms.ColumnHeader uColVerHdr;
        private System.Windows.Forms.ColumnHeader uColIdHdr;
    }
}
