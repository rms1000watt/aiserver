namespace AisExcel2003
{
    partial class GSMDemo
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
            {  components.Dispose();
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
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(GSMDemo));
            this.upStatusBar = new System.Windows.Forms.StatusStrip();
            this.upStripStatusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.upGsmToolTip = new System.Windows.Forms.ToolTip(this.components);
            this.upConnectButton = new System.Windows.Forms.Button();
            this.upGenerateDataButton = new System.Windows.Forms.Button();
            this.upLoadButton = new System.Windows.Forms.Button();
            this.upSplitButton = new System.Windows.Forms.Button();
            this.upRunButton = new System.Windows.Forms.Button();
            this.upSaveButton = new System.Windows.Forms.Button();
            this.upHelpButton = new System.Windows.Forms.Button();
            this.upCloseButton = new System.Windows.Forms.Button();
            this.upGsmFlowPanel = new System.Windows.Forms.FlowLayoutPanel();
            this.upConnectPanel = new System.Windows.Forms.Panel();
            this.upConnectHelpLabel = new System.Windows.Forms.Label();
            this.upConnectLabel = new System.Windows.Forms.Label();
            this.upGenerateDataPanel = new System.Windows.Forms.Panel();
            this.upGenerateHelpLabel = new System.Windows.Forms.Label();
            this.upGenerateLabel = new System.Windows.Forms.Label();
            this.upLoadPanel = new System.Windows.Forms.Panel();
            this.upLoadHelpLabel = new System.Windows.Forms.Label();
            this.upLoadLabel = new System.Windows.Forms.Label();
            this.upSplitPanel = new System.Windows.Forms.Panel();
            this.upSplitHelpLabel = new System.Windows.Forms.Label();
            this.upSplitLabel = new System.Windows.Forms.Label();
            this.upRunPanel = new System.Windows.Forms.Panel();
            this.upRunHelpLabel = new System.Windows.Forms.Label();
            this.upRunLabel = new System.Windows.Forms.Label();
            this.upSavePanel = new System.Windows.Forms.Panel();
            this.upSaveHelpLabel = new System.Windows.Forms.Label();
            this.upSaveLabel = new System.Windows.Forms.Label();
            this.upHelpPanel = new System.Windows.Forms.Panel();
            this.upHelpLabel = new System.Windows.Forms.Label();
            this.upHelpHelpLabel = new System.Windows.Forms.Label();
            this.upClosePanel = new System.Windows.Forms.Panel();
            this.upCloseLabel = new System.Windows.Forms.Label();
            this.upCloseHelpLabel = new System.Windows.Forms.Label();
            this.upWelcomPanel = new System.Windows.Forms.Panel();
            this.upWelcomeHelpLabel = new System.Windows.Forms.Label();
            this.upShowWelcomeText = new System.Windows.Forms.CheckBox();
            this.upAisPanel = new System.Windows.Forms.Panel();
            this.upAisHelpLabel = new System.Windows.Forms.Label();
            this.upShowAISMessages = new System.Windows.Forms.CheckBox();
            this.upStatusBar.SuspendLayout();
            this.upGsmFlowPanel.SuspendLayout();
            this.upConnectPanel.SuspendLayout();
            this.upGenerateDataPanel.SuspendLayout();
            this.upLoadPanel.SuspendLayout();
            this.upSplitPanel.SuspendLayout();
            this.upRunPanel.SuspendLayout();
            this.upSavePanel.SuspendLayout();
            this.upHelpPanel.SuspendLayout();
            this.upClosePanel.SuspendLayout();
            this.upWelcomPanel.SuspendLayout();
            this.upAisPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // upStatusBar
            // 
            this.upStatusBar.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.upStripStatusLabel});
            this.upStatusBar.Location = new System.Drawing.Point(0, 147);
            this.upStatusBar.Name = "upStatusBar";
            this.upStatusBar.Size = new System.Drawing.Size(331, 22);
            this.upStatusBar.TabIndex = 38;
            this.upStatusBar.Text = "statusStrip1";
            // 
            // upStripStatusLabel
            // 
            this.upStripStatusLabel.ImageAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.upStripStatusLabel.Name = "upStripStatusLabel";
            this.upStripStatusLabel.Size = new System.Drawing.Size(104, 17);
            this.upStripStatusLabel.Text = "Show current status";
            // 
            // upGsmToolTip
            // 
            this.upGsmToolTip.IsBalloon = true;
            this.upGsmToolTip.ToolTipTitle = "GSM Help";
            // 
            // upConnectButton
            // 
            this.upConnectButton.Location = new System.Drawing.Point(25, 0);
            this.upConnectButton.Name = "upConnectButton";
            this.upConnectButton.Size = new System.Drawing.Size(105, 23);
            this.upConnectButton.TabIndex = 1;
            this.upConnectButton.Text = "Connect to AIS ...";
            this.upGsmToolTip.SetToolTip(this.upConnectButton, "You must connect to an Analytic Information Server (AIS) \r\nrunning the GSM Demo s" +
                    "erver application to enable \r\nthe Generate Test button and the Run GSM  Test but" +
                    "ton.");
            this.upConnectButton.UseVisualStyleBackColor = true;
            this.upConnectButton.Click += new System.EventHandler(this.upConnectButton_Click);
            // 
            // upGenerateDataButton
            // 
            this.upGenerateDataButton.Location = new System.Drawing.Point(25, 0);
            this.upGenerateDataButton.Name = "upGenerateDataButton";
            this.upGenerateDataButton.Size = new System.Drawing.Size(105, 23);
            this.upGenerateDataButton.TabIndex = 2;
            this.upGenerateDataButton.Text = "Generate Test ...";
            this.upGsmToolTip.SetToolTip(this.upGenerateDataButton, resources.GetString("upGenerateDataButton.ToolTip"));
            this.upGenerateDataButton.UseVisualStyleBackColor = true;
            this.upGenerateDataButton.Click += new System.EventHandler(this.upGenerateDataButton_Click);
            // 
            // upLoadButton
            // 
            this.upLoadButton.Location = new System.Drawing.Point(25, 0);
            this.upLoadButton.Name = "upLoadButton";
            this.upLoadButton.Size = new System.Drawing.Size(105, 23);
            this.upLoadButton.TabIndex = 3;
            this.upLoadButton.Text = "Load Test ...";
            this.upGsmToolTip.SetToolTip(this.upLoadButton, "If you have previously saved a test set consisting \r\nof these three files on your" +
                    " local file system, you \r\ncan select Load Test button as an alternate way to \r\no" +
                    "btain a test set.");
            this.upLoadButton.UseVisualStyleBackColor = true;
            this.upLoadButton.Click += new System.EventHandler(this.upLoadButton_Click);
            // 
            // upSplitButton
            // 
            this.upSplitButton.Location = new System.Drawing.Point(25, 0);
            this.upSplitButton.Name = "upSplitButton";
            this.upSplitButton.Size = new System.Drawing.Size(105, 23);
            this.upSplitButton.TabIndex = 4;
            this.upSplitButton.Text = "Split ...";
            this.upGsmToolTip.SetToolTip(this.upSplitButton, "If you have previously saved a test set consisting of \r\nthese three files on your" +
                    " local file system, you can \r\nselect Load Test button as an alternate way to \r\no" +
                    "btain a test set.");
            this.upSplitButton.UseVisualStyleBackColor = true;
            this.upSplitButton.Click += new System.EventHandler(this.upSplitButton_Click);
            // 
            // upRunButton
            // 
            this.upRunButton.Location = new System.Drawing.Point(25, 0);
            this.upRunButton.Name = "upRunButton";
            this.upRunButton.Size = new System.Drawing.Size(105, 23);
            this.upRunButton.TabIndex = 5;
            this.upRunButton.Text = "Run GSM Test ...";
            this.upGsmToolTip.SetToolTip(this.upRunButton, "After the above worksheets have been filled and \r\nmodified as necessary, select R" +
                    "un GSM Test to \r\nconduct a regression.  This test may take a very \r\nlong time fo" +
                    "r large data sets.");
            this.upRunButton.UseVisualStyleBackColor = true;
            this.upRunButton.Click += new System.EventHandler(this.upRunButton_Click);
            // 
            // upSaveButton
            // 
            this.upSaveButton.Location = new System.Drawing.Point(25, 0);
            this.upSaveButton.Name = "upSaveButton";
            this.upSaveButton.Size = new System.Drawing.Size(105, 23);
            this.upSaveButton.TabIndex = 6;
            this.upSaveButton.Text = "Save Test";
            this.upGsmToolTip.SetToolTip(this.upSaveButton, resources.GetString("upSaveButton.ToolTip"));
            this.upSaveButton.UseVisualStyleBackColor = true;
            this.upSaveButton.Click += new System.EventHandler(this.upSaveButton_Click);
            // 
            // upHelpButton
            // 
            this.upHelpButton.Location = new System.Drawing.Point(25, 0);
            this.upHelpButton.Name = "upHelpButton";
            this.upHelpButton.Size = new System.Drawing.Size(105, 23);
            this.upHelpButton.TabIndex = 14;
            this.upHelpButton.Text = "Help";
            this.upGsmToolTip.SetToolTip(this.upHelpButton, resources.GetString("upHelpButton.ToolTip"));
            this.upHelpButton.UseVisualStyleBackColor = true;
            this.upHelpButton.Click += new System.EventHandler(this.upHelpButton_Click);
            // 
            // upCloseButton
            // 
            this.upCloseButton.Location = new System.Drawing.Point(25, 0);
            this.upCloseButton.Name = "upCloseButton";
            this.upCloseButton.Size = new System.Drawing.Size(105, 23);
            this.upCloseButton.TabIndex = 14;
            this.upCloseButton.Text = "Close";
            this.upGsmToolTip.SetToolTip(this.upCloseButton, "Select Close to dismiss this dialog.  Close does \r\nnot disconnect you from the se" +
                    "rver or terminate \r\nan ongoing test.  You can reopen this dialog from \r\nthe Smil" +
                    "ey Face icon in the custom toolbar.");
            this.upCloseButton.UseVisualStyleBackColor = true;
            this.upCloseButton.Click += new System.EventHandler(this.upCloseButton_Click);
            // 
            // upGsmFlowPanel
            // 
            this.upGsmFlowPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.upGsmFlowPanel.Controls.Add(this.upConnectPanel);
            this.upGsmFlowPanel.Controls.Add(this.upGenerateDataPanel);
            this.upGsmFlowPanel.Controls.Add(this.upLoadPanel);
            this.upGsmFlowPanel.Controls.Add(this.upSplitPanel);
            this.upGsmFlowPanel.Controls.Add(this.upRunPanel);
            this.upGsmFlowPanel.Controls.Add(this.upSavePanel);
            this.upGsmFlowPanel.Controls.Add(this.upHelpPanel);
            this.upGsmFlowPanel.Controls.Add(this.upClosePanel);
            this.upGsmFlowPanel.Controls.Add(this.upWelcomPanel);
            this.upGsmFlowPanel.Controls.Add(this.upAisPanel);
            this.upGsmFlowPanel.Location = new System.Drawing.Point(0, 0);
            this.upGsmFlowPanel.Name = "upGsmFlowPanel";
            this.upGsmFlowPanel.Size = new System.Drawing.Size(331, 152);
            this.upGsmFlowPanel.TabIndex = 41;
            // 
            // upConnectPanel
            // 
            this.upConnectPanel.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.upConnectPanel.Controls.Add(this.upConnectButton);
            this.upConnectPanel.Controls.Add(this.upConnectHelpLabel);
            this.upConnectPanel.Controls.Add(this.upConnectLabel);
            this.upConnectPanel.Location = new System.Drawing.Point(3, 3);
            this.upConnectPanel.Name = "upConnectPanel";
            this.upConnectPanel.Size = new System.Drawing.Size(155, 23);
            this.upConnectPanel.TabIndex = 17;
            // 
            // upConnectHelpLabel
            // 
            this.upConnectHelpLabel.BackColor = System.Drawing.SystemColors.Control;
            this.upConnectHelpLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.upConnectHelpLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 15F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upConnectHelpLabel.ForeColor = System.Drawing.Color.RoyalBlue;
            this.upConnectHelpLabel.Image = global::AisExcel2003.Properties.Resources.help1;
            this.upConnectHelpLabel.Location = new System.Drawing.Point(135, 0);
            this.upConnectHelpLabel.MinimumSize = new System.Drawing.Size(20, 20);
            this.upConnectHelpLabel.Name = "upConnectHelpLabel";
            this.upConnectHelpLabel.Size = new System.Drawing.Size(20, 23);
            this.upConnectHelpLabel.TabIndex = 39;
            this.upConnectHelpLabel.Text = " ";
            this.upConnectHelpLabel.Click += new System.EventHandler(this.upConnectHelpLabel_Click);
            // 
            // upConnectLabel
            // 
            this.upConnectLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.upConnectLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upConnectLabel.Location = new System.Drawing.Point(0, 0);
            this.upConnectLabel.Name = "upConnectLabel";
            this.upConnectLabel.Size = new System.Drawing.Size(24, 23);
            this.upConnectLabel.TabIndex = 46;
            this.upConnectLabel.Text = "1. ";
            this.upConnectLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // upGenerateDataPanel
            // 
            this.upGenerateDataPanel.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.upGenerateDataPanel.Controls.Add(this.upGenerateDataButton);
            this.upGenerateDataPanel.Controls.Add(this.upGenerateHelpLabel);
            this.upGenerateDataPanel.Controls.Add(this.upGenerateLabel);
            this.upGenerateDataPanel.Location = new System.Drawing.Point(164, 3);
            this.upGenerateDataPanel.Name = "upGenerateDataPanel";
            this.upGenerateDataPanel.Size = new System.Drawing.Size(155, 23);
            this.upGenerateDataPanel.TabIndex = 18;
            // 
            // upGenerateHelpLabel
            // 
            this.upGenerateHelpLabel.BackColor = System.Drawing.SystemColors.Control;
            this.upGenerateHelpLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.upGenerateHelpLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 15F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upGenerateHelpLabel.ForeColor = System.Drawing.Color.RoyalBlue;
            this.upGenerateHelpLabel.Image = global::AisExcel2003.Properties.Resources.help1;
            this.upGenerateHelpLabel.Location = new System.Drawing.Point(135, 0);
            this.upGenerateHelpLabel.MinimumSize = new System.Drawing.Size(20, 20);
            this.upGenerateHelpLabel.Name = "upGenerateHelpLabel";
            this.upGenerateHelpLabel.Size = new System.Drawing.Size(20, 23);
            this.upGenerateHelpLabel.TabIndex = 40;
            this.upGenerateHelpLabel.Click += new System.EventHandler(this.upGenerateHelpLabel_Click);
            // 
            // upGenerateLabel
            // 
            this.upGenerateLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.upGenerateLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upGenerateLabel.Location = new System.Drawing.Point(0, 0);
            this.upGenerateLabel.Name = "upGenerateLabel";
            this.upGenerateLabel.Size = new System.Drawing.Size(24, 23);
            this.upGenerateLabel.TabIndex = 47;
            this.upGenerateLabel.Text = "2a";
            this.upGenerateLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // upLoadPanel
            // 
            this.upLoadPanel.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.upLoadPanel.Controls.Add(this.upLoadButton);
            this.upLoadPanel.Controls.Add(this.upLoadHelpLabel);
            this.upLoadPanel.Controls.Add(this.upLoadLabel);
            this.upLoadPanel.Location = new System.Drawing.Point(3, 32);
            this.upLoadPanel.Name = "upLoadPanel";
            this.upLoadPanel.Size = new System.Drawing.Size(155, 23);
            this.upLoadPanel.TabIndex = 19;
            // 
            // upLoadHelpLabel
            // 
            this.upLoadHelpLabel.BackColor = System.Drawing.SystemColors.Control;
            this.upLoadHelpLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.upLoadHelpLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 15F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upLoadHelpLabel.ForeColor = System.Drawing.Color.Red;
            this.upLoadHelpLabel.Image = ((System.Drawing.Image)(resources.GetObject("upLoadHelpLabel.Image")));
            this.upLoadHelpLabel.Location = new System.Drawing.Point(135, 0);
            this.upLoadHelpLabel.MinimumSize = new System.Drawing.Size(20, 20);
            this.upLoadHelpLabel.Name = "upLoadHelpLabel";
            this.upLoadHelpLabel.Size = new System.Drawing.Size(20, 23);
            this.upLoadHelpLabel.TabIndex = 41;
            this.upLoadHelpLabel.Click += new System.EventHandler(this.upLoadHelpLabel_Click);
            // 
            // upLoadLabel
            // 
            this.upLoadLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.upLoadLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upLoadLabel.Location = new System.Drawing.Point(0, 0);
            this.upLoadLabel.Name = "upLoadLabel";
            this.upLoadLabel.Size = new System.Drawing.Size(24, 23);
            this.upLoadLabel.TabIndex = 48;
            this.upLoadLabel.Text = "2b";
            this.upLoadLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // upSplitPanel
            // 
            this.upSplitPanel.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.upSplitPanel.Controls.Add(this.upSplitButton);
            this.upSplitPanel.Controls.Add(this.upSplitHelpLabel);
            this.upSplitPanel.Controls.Add(this.upSplitLabel);
            this.upSplitPanel.Location = new System.Drawing.Point(164, 32);
            this.upSplitPanel.Name = "upSplitPanel";
            this.upSplitPanel.Size = new System.Drawing.Size(155, 23);
            this.upSplitPanel.TabIndex = 20;
            // 
            // upSplitHelpLabel
            // 
            this.upSplitHelpLabel.BackColor = System.Drawing.SystemColors.Control;
            this.upSplitHelpLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.upSplitHelpLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 15F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upSplitHelpLabel.ForeColor = System.Drawing.Color.Red;
            this.upSplitHelpLabel.Image = ((System.Drawing.Image)(resources.GetObject("upSplitHelpLabel.Image")));
            this.upSplitHelpLabel.Location = new System.Drawing.Point(135, 0);
            this.upSplitHelpLabel.MinimumSize = new System.Drawing.Size(20, 20);
            this.upSplitHelpLabel.Name = "upSplitHelpLabel";
            this.upSplitHelpLabel.Size = new System.Drawing.Size(20, 23);
            this.upSplitHelpLabel.TabIndex = 42;
            this.upSplitHelpLabel.Click += new System.EventHandler(this.upSplitHelpLabel_Click);
            // 
            // upSplitLabel
            // 
            this.upSplitLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.upSplitLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upSplitLabel.Location = new System.Drawing.Point(0, 0);
            this.upSplitLabel.Name = "upSplitLabel";
            this.upSplitLabel.Size = new System.Drawing.Size(24, 23);
            this.upSplitLabel.TabIndex = 49;
            this.upSplitLabel.Text = "2c";
            this.upSplitLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // upRunPanel
            // 
            this.upRunPanel.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.upRunPanel.Controls.Add(this.upRunButton);
            this.upRunPanel.Controls.Add(this.upRunHelpLabel);
            this.upRunPanel.Controls.Add(this.upRunLabel);
            this.upRunPanel.Location = new System.Drawing.Point(3, 61);
            this.upRunPanel.Name = "upRunPanel";
            this.upRunPanel.Size = new System.Drawing.Size(155, 23);
            this.upRunPanel.TabIndex = 21;
            // 
            // upRunHelpLabel
            // 
            this.upRunHelpLabel.BackColor = System.Drawing.SystemColors.Control;
            this.upRunHelpLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.upRunHelpLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 15F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upRunHelpLabel.ForeColor = System.Drawing.Color.Red;
            this.upRunHelpLabel.Image = ((System.Drawing.Image)(resources.GetObject("upRunHelpLabel.Image")));
            this.upRunHelpLabel.Location = new System.Drawing.Point(135, 0);
            this.upRunHelpLabel.MinimumSize = new System.Drawing.Size(20, 20);
            this.upRunHelpLabel.Name = "upRunHelpLabel";
            this.upRunHelpLabel.Size = new System.Drawing.Size(20, 23);
            this.upRunHelpLabel.TabIndex = 43;
            this.upRunHelpLabel.Click += new System.EventHandler(this.upRunHelpLabel_Click);
            // 
            // upRunLabel
            // 
            this.upRunLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.upRunLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upRunLabel.Location = new System.Drawing.Point(0, 0);
            this.upRunLabel.Name = "upRunLabel";
            this.upRunLabel.Size = new System.Drawing.Size(24, 23);
            this.upRunLabel.TabIndex = 50;
            this.upRunLabel.Text = "3.";
            this.upRunLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // upSavePanel
            // 
            this.upSavePanel.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.upSavePanel.Controls.Add(this.upSaveButton);
            this.upSavePanel.Controls.Add(this.upSaveHelpLabel);
            this.upSavePanel.Controls.Add(this.upSaveLabel);
            this.upSavePanel.Location = new System.Drawing.Point(164, 61);
            this.upSavePanel.Name = "upSavePanel";
            this.upSavePanel.Size = new System.Drawing.Size(155, 23);
            this.upSavePanel.TabIndex = 22;
            // 
            // upSaveHelpLabel
            // 
            this.upSaveHelpLabel.BackColor = System.Drawing.SystemColors.Control;
            this.upSaveHelpLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.upSaveHelpLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 15F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upSaveHelpLabel.ForeColor = System.Drawing.Color.Red;
            this.upSaveHelpLabel.Image = ((System.Drawing.Image)(resources.GetObject("upSaveHelpLabel.Image")));
            this.upSaveHelpLabel.Location = new System.Drawing.Point(135, 0);
            this.upSaveHelpLabel.MinimumSize = new System.Drawing.Size(20, 20);
            this.upSaveHelpLabel.Name = "upSaveHelpLabel";
            this.upSaveHelpLabel.Size = new System.Drawing.Size(20, 23);
            this.upSaveHelpLabel.TabIndex = 44;
            this.upSaveHelpLabel.Click += new System.EventHandler(this.upSaveHelpLabel_Click);
            // 
            // upSaveLabel
            // 
            this.upSaveLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.upSaveLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upSaveLabel.Location = new System.Drawing.Point(0, 0);
            this.upSaveLabel.Name = "upSaveLabel";
            this.upSaveLabel.Size = new System.Drawing.Size(24, 23);
            this.upSaveLabel.TabIndex = 51;
            this.upSaveLabel.Text = "4.";
            this.upSaveLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // upHelpPanel
            // 
            this.upHelpPanel.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.upHelpPanel.Controls.Add(this.upHelpButton);
            this.upHelpPanel.Controls.Add(this.upHelpLabel);
            this.upHelpPanel.Controls.Add(this.upHelpHelpLabel);
            this.upHelpPanel.Location = new System.Drawing.Point(3, 90);
            this.upHelpPanel.Name = "upHelpPanel";
            this.upHelpPanel.Size = new System.Drawing.Size(155, 23);
            this.upHelpPanel.TabIndex = 23;
            // 
            // upHelpLabel
            // 
            this.upHelpLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.upHelpLabel.Image = ((System.Drawing.Image)(resources.GetObject("upHelpLabel.Image")));
            this.upHelpLabel.Location = new System.Drawing.Point(135, 0);
            this.upHelpLabel.MinimumSize = new System.Drawing.Size(20, 20);
            this.upHelpLabel.Name = "upHelpLabel";
            this.upHelpLabel.Size = new System.Drawing.Size(20, 23);
            this.upHelpLabel.TabIndex = 13;
            this.upHelpLabel.Click += new System.EventHandler(this.upHelpLabel_Click);
            // 
            // upHelpHelpLabel
            // 
            this.upHelpHelpLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.upHelpHelpLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upHelpHelpLabel.Location = new System.Drawing.Point(0, 0);
            this.upHelpHelpLabel.Name = "upHelpHelpLabel";
            this.upHelpHelpLabel.Size = new System.Drawing.Size(24, 23);
            this.upHelpHelpLabel.TabIndex = 12;
            this.upHelpHelpLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // upClosePanel
            // 
            this.upClosePanel.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.upClosePanel.Controls.Add(this.upCloseButton);
            this.upClosePanel.Controls.Add(this.upCloseLabel);
            this.upClosePanel.Controls.Add(this.upCloseHelpLabel);
            this.upClosePanel.Location = new System.Drawing.Point(164, 90);
            this.upClosePanel.Name = "upClosePanel";
            this.upClosePanel.Size = new System.Drawing.Size(155, 23);
            this.upClosePanel.TabIndex = 24;
            // 
            // upCloseLabel
            // 
            this.upCloseLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.upCloseLabel.Image = ((System.Drawing.Image)(resources.GetObject("upCloseLabel.Image")));
            this.upCloseLabel.Location = new System.Drawing.Point(135, 0);
            this.upCloseLabel.MinimumSize = new System.Drawing.Size(20, 20);
            this.upCloseLabel.Name = "upCloseLabel";
            this.upCloseLabel.Size = new System.Drawing.Size(20, 23);
            this.upCloseLabel.TabIndex = 13;
            this.upCloseLabel.Click += new System.EventHandler(this.upCloseLabel_Click);
            // 
            // upCloseHelpLabel
            // 
            this.upCloseHelpLabel.Dock = System.Windows.Forms.DockStyle.Left;
            this.upCloseHelpLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.upCloseHelpLabel.Location = new System.Drawing.Point(0, 0);
            this.upCloseHelpLabel.Name = "upCloseHelpLabel";
            this.upCloseHelpLabel.Size = new System.Drawing.Size(24, 23);
            this.upCloseHelpLabel.TabIndex = 12;
            this.upCloseHelpLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // upWelcomPanel
            // 
            this.upWelcomPanel.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.upWelcomPanel.Controls.Add(this.upWelcomeHelpLabel);
            this.upWelcomPanel.Controls.Add(this.upShowWelcomeText);
            this.upWelcomPanel.Location = new System.Drawing.Point(3, 119);
            this.upWelcomPanel.Name = "upWelcomPanel";
            this.upWelcomPanel.Size = new System.Drawing.Size(155, 23);
            this.upWelcomPanel.TabIndex = 25;
            // 
            // upWelcomeHelpLabel
            // 
            this.upWelcomeHelpLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.upWelcomeHelpLabel.Image = ((System.Drawing.Image)(resources.GetObject("upWelcomeHelpLabel.Image")));
            this.upWelcomeHelpLabel.Location = new System.Drawing.Point(135, 0);
            this.upWelcomeHelpLabel.MinimumSize = new System.Drawing.Size(20, 20);
            this.upWelcomeHelpLabel.Name = "upWelcomeHelpLabel";
            this.upWelcomeHelpLabel.Size = new System.Drawing.Size(20, 23);
            this.upWelcomeHelpLabel.TabIndex = 13;
            this.upWelcomeHelpLabel.Click += new System.EventHandler(this.upWelcomeHelpLabel_Click);
            // 
            // upShowWelcomeText
            // 
            this.upShowWelcomeText.Checked = true;
            this.upShowWelcomeText.CheckState = System.Windows.Forms.CheckState.Checked;
            this.upShowWelcomeText.Location = new System.Drawing.Point(12, 0);
            this.upShowWelcomeText.Name = "upShowWelcomeText";
            this.upShowWelcomeText.Size = new System.Drawing.Size(130, 23);
            this.upShowWelcomeText.TabIndex = 18;
            this.upShowWelcomeText.Text = "Show Welcome Text";
            this.upShowWelcomeText.UseVisualStyleBackColor = true;
            this.upShowWelcomeText.CheckedChanged += new System.EventHandler(this.upShowWelcomeText_CheckedChanged);
            // 
            // upAisPanel
            // 
            this.upAisPanel.Anchor = System.Windows.Forms.AnchorStyles.None;
            this.upAisPanel.Controls.Add(this.upAisHelpLabel);
            this.upAisPanel.Controls.Add(this.upShowAISMessages);
            this.upAisPanel.Location = new System.Drawing.Point(164, 119);
            this.upAisPanel.Name = "upAisPanel";
            this.upAisPanel.Size = new System.Drawing.Size(155, 23);
            this.upAisPanel.TabIndex = 26;
            // 
            // upAisHelpLabel
            // 
            this.upAisHelpLabel.Dock = System.Windows.Forms.DockStyle.Right;
            this.upAisHelpLabel.Image = ((System.Drawing.Image)(resources.GetObject("upAisHelpLabel.Image")));
            this.upAisHelpLabel.Location = new System.Drawing.Point(135, 0);
            this.upAisHelpLabel.MinimumSize = new System.Drawing.Size(20, 20);
            this.upAisHelpLabel.Name = "upAisHelpLabel";
            this.upAisHelpLabel.Size = new System.Drawing.Size(20, 23);
            this.upAisHelpLabel.TabIndex = 16;
            this.upAisHelpLabel.Click += new System.EventHandler(this.upAisHelpLabel_Click);
            // 
            // upShowAISMessages
            // 
            this.upShowAISMessages.Checked = true;
            this.upShowAISMessages.CheckState = System.Windows.Forms.CheckState.Checked;
            this.upShowAISMessages.Location = new System.Drawing.Point(12, 0);
            this.upShowAISMessages.Name = "upShowAISMessages";
            this.upShowAISMessages.Size = new System.Drawing.Size(129, 23);
            this.upShowAISMessages.TabIndex = 17;
            this.upShowAISMessages.Text = "Show AIS Messages";
            this.upShowAISMessages.UseVisualStyleBackColor = true;
            this.upShowAISMessages.CheckedChanged += new System.EventHandler(this.upShowAISMessages_CheckedChanged);
            // 
            // GSMDemo
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.ClientSize = new System.Drawing.Size(331, 169);
            this.ControlBox = false;
            this.Controls.Add(this.upGsmFlowPanel);
            this.Controls.Add(this.upStatusBar);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "GSMDemo";
            this.Text = "GSM Demo";
            this.Load += new System.EventHandler(this.GSMDemo_Load);
            this.upStatusBar.ResumeLayout(false);
            this.upStatusBar.PerformLayout();
            this.upGsmFlowPanel.ResumeLayout(false);
            this.upConnectPanel.ResumeLayout(false);
            this.upGenerateDataPanel.ResumeLayout(false);
            this.upLoadPanel.ResumeLayout(false);
            this.upSplitPanel.ResumeLayout(false);
            this.upRunPanel.ResumeLayout(false);
            this.upSavePanel.ResumeLayout(false);
            this.upHelpPanel.ResumeLayout(false);
            this.upClosePanel.ResumeLayout(false);
            this.upWelcomPanel.ResumeLayout(false);
            this.upAisPanel.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.StatusStrip upStatusBar;
        private System.Windows.Forms.ToolStripStatusLabel upStripStatusLabel;
		private System.Windows.Forms.ToolTip upGsmToolTip;
        private System.Windows.Forms.FlowLayoutPanel upGsmFlowPanel;
        private System.Windows.Forms.Panel upConnectPanel;
        private System.Windows.Forms.Button upConnectButton;
        private System.Windows.Forms.Label upConnectHelpLabel;
        private System.Windows.Forms.Label upConnectLabel;
        private System.Windows.Forms.Panel upGenerateDataPanel;
        private System.Windows.Forms.Button upGenerateDataButton;
        private System.Windows.Forms.Label upGenerateHelpLabel;
        private System.Windows.Forms.Label upGenerateLabel;
        private System.Windows.Forms.Panel upLoadPanel;
        private System.Windows.Forms.Button upLoadButton;
        private System.Windows.Forms.Label upLoadHelpLabel;
        private System.Windows.Forms.Label upLoadLabel;
        private System.Windows.Forms.Panel upSplitPanel;
        private System.Windows.Forms.Button upSplitButton;
        private System.Windows.Forms.Label upSplitHelpLabel;
        private System.Windows.Forms.Label upSplitLabel;
        private System.Windows.Forms.Panel upRunPanel;
        private System.Windows.Forms.Button upRunButton;
        private System.Windows.Forms.Label upRunHelpLabel;
        private System.Windows.Forms.Label upRunLabel;
        private System.Windows.Forms.Panel upSavePanel;
        public System.Windows.Forms.Button upSaveButton;
        private System.Windows.Forms.Label upSaveHelpLabel;
        private System.Windows.Forms.Label upSaveLabel;
        private System.Windows.Forms.Panel upHelpPanel;
        private System.Windows.Forms.Button upHelpButton;
        private System.Windows.Forms.Label upHelpLabel;
        private System.Windows.Forms.Label upHelpHelpLabel;
        private System.Windows.Forms.Panel upClosePanel;
        private System.Windows.Forms.Button upCloseButton;
        private System.Windows.Forms.Label upCloseLabel;
        private System.Windows.Forms.Label upCloseHelpLabel;
        private System.Windows.Forms.Panel upWelcomPanel;
        private System.Windows.Forms.Label upWelcomeHelpLabel;
        private System.Windows.Forms.CheckBox upShowWelcomeText;
        private System.Windows.Forms.Panel upAisPanel;
        private System.Windows.Forms.Label upAisHelpLabel;
        private System.Windows.Forms.CheckBox upShowAISMessages;
    }
}