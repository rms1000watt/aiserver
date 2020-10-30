/**********************************************************************************
    Copyright (C) 2009 Investment Science Corp.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************/
namespace AisExcel2003
{
    partial class ConnectForm
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
		this.components = new System.ComponentModel.Container();
		System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ConnectForm));
		this.upServerIPText = new System.Windows.Forms.TextBox();
		this.upConnectButton = new System.Windows.Forms.Button();
		this.upCloseButton = new System.Windows.Forms.Button();
		this.label2 = new System.Windows.Forms.Label();
		this.upServerPortText = new System.Windows.Forms.TextBox();
		this.label1 = new System.Windows.Forms.Label();
		this.label3 = new System.Windows.Forms.Label();
		this.label4 = new System.Windows.Forms.Label();
		this.upUsernameText = new System.Windows.Forms.TextBox();
		this.upPasswordText = new System.Windows.Forms.TextBox();
		this.upLaunchButton = new System.Windows.Forms.Button();
		this.upConnectToolTip = new System.Windows.Forms.ToolTip(this.components);
		this.SuspendLayout();
		// 
		// upServerIPText
		// 
		this.upServerIPText.Location = new System.Drawing.Point(82, 21);
		this.upServerIPText.Name = "upServerIPText";
		this.upServerIPText.Size = new System.Drawing.Size(102, 20);
		this.upServerIPText.TabIndex = 0;
		// 
		// upConnectButton
		// 
		this.upConnectButton.Location = new System.Drawing.Point(140, 97);
		this.upConnectButton.Name = "upConnectButton";
		this.upConnectButton.Size = new System.Drawing.Size(88, 23);
		this.upConnectButton.TabIndex = 1;
		this.upConnectButton.Text = "Connect";
		this.upConnectToolTip.SetToolTip(this.upConnectButton, resources.GetString("upConnectButton.ToolTip"));
		this.upConnectButton.UseVisualStyleBackColor = true;
		this.upConnectButton.Click += new System.EventHandler(this.upConnectButton_Click);
		// 
		// upCloseButton
		// 
		this.upCloseButton.Location = new System.Drawing.Point(247, 97);
		this.upCloseButton.Name = "upCloseButton";
		this.upCloseButton.Size = new System.Drawing.Size(75, 23);
		this.upCloseButton.TabIndex = 2;
		this.upCloseButton.Text = "Close";
		this.upConnectToolTip.SetToolTip(this.upCloseButton, "Click on Close to close this dialog.  The connection to AIS\r\nwill not be closed.");
		this.upCloseButton.UseVisualStyleBackColor = true;
		this.upCloseButton.Click += new System.EventHandler(this.upCloseButton_Click);
		// 
		// label2
		// 
		this.label2.AutoSize = true;
		this.label2.Location = new System.Drawing.Point(194, 25);
		this.label2.Name = "label2";
		this.label2.Size = new System.Drawing.Size(60, 13);
		this.label2.TabIndex = 4;
		this.label2.Text = "Server Port";
		// 
		// upServerPortText
		// 
		this.upServerPortText.Location = new System.Drawing.Point(260, 22);
		this.upServerPortText.Name = "upServerPortText";
		this.upServerPortText.Size = new System.Drawing.Size(45, 20);
		this.upServerPortText.TabIndex = 7;
		// 
		// label1
		// 
		this.label1.AutoSize = true;
		this.label1.Location = new System.Drawing.Point(13, 24);
		this.label1.Name = "label1";
		this.label1.Size = new System.Drawing.Size(63, 13);
		this.label1.TabIndex = 8;
		this.label1.Text = "Server Host";
		// 
		// label3
		// 
		this.label3.AutoSize = true;
		this.label3.Location = new System.Drawing.Point(13, 63);
		this.label3.Name = "label3";
		this.label3.Size = new System.Drawing.Size(55, 13);
		this.label3.TabIndex = 13;
		this.label3.Text = "Username";
		// 
		// label4
		// 
		this.label4.AutoSize = true;
		this.label4.Location = new System.Drawing.Point(194, 67);
		this.label4.Name = "label4";
		this.label4.Size = new System.Drawing.Size(53, 13);
		this.label4.TabIndex = 11;
		this.label4.Text = "Password";
		// 
		// upUsernameText
		// 
		this.upUsernameText.Location = new System.Drawing.Point(82, 60);
		this.upUsernameText.Name = "upUsernameText";
		this.upUsernameText.Size = new System.Drawing.Size(102, 20);
		this.upUsernameText.TabIndex = 9;
		// 
		// upPasswordText
		// 
		this.upPasswordText.Location = new System.Drawing.Point(260, 60);
		this.upPasswordText.Name = "upPasswordText";
		this.upPasswordText.PasswordChar = '*';
		this.upPasswordText.Size = new System.Drawing.Size(95, 20);
		this.upPasswordText.TabIndex = 14;
		// 
		// upLaunchButton
		// 
		this.upLaunchButton.Location = new System.Drawing.Point(34, 97);
		this.upLaunchButton.Name = "upLaunchButton";
		this.upLaunchButton.Size = new System.Drawing.Size(88, 23);
		this.upLaunchButton.TabIndex = 15;
		this.upLaunchButton.Text = "Launch AIS";
		this.upConnectToolTip.SetToolTip(this.upLaunchButton, resources.GetString("upLaunchButton.ToolTip"));
		this.upLaunchButton.UseVisualStyleBackColor = true;
		this.upLaunchButton.Click += new System.EventHandler(this.upLaunchButton_Click);
		// 
		// ConnectForm
		// 
		this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
		this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
		this.ClientSize = new System.Drawing.Size(395, 143);
		this.Controls.Add(this.upLaunchButton);
		this.Controls.Add(this.upPasswordText);
		this.Controls.Add(this.label3);
		this.Controls.Add(this.label4);
		this.Controls.Add(this.upUsernameText);
		this.Controls.Add(this.label1);
		this.Controls.Add(this.upServerPortText);
		this.Controls.Add(this.label2);
		this.Controls.Add(this.upCloseButton);
		this.Controls.Add(this.upConnectButton);
		this.Controls.Add(this.upServerIPText);
		this.Name = "ConnectForm";
		this.Text = "Connect to AIS";
		this.ResumeLayout(false);
		this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox upServerIPText;
        private System.Windows.Forms.Button upConnectButton;
        private System.Windows.Forms.Button upCloseButton;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox upServerPortText;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox upUsernameText;
        private System.Windows.Forms.TextBox upPasswordText;
        private System.Windows.Forms.Button upLaunchButton;
		private System.Windows.Forms.ToolTip upConnectToolTip;
    }
}