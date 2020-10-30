namespace AisExcel2003
{
	partial class ConnectUserControl
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
			this.upServerLabel = new System.Windows.Forms.Label();
			this.label2 = new System.Windows.Forms.Label();
			this.upConnectButton = new System.Windows.Forms.Button();
			this.upServerTextBox = new System.Windows.Forms.TextBox();
			this.SuspendLayout();
			// 
			// upServerLabel
			// 
			this.upServerLabel.AutoSize = true;
			this.upServerLabel.Location = new System.Drawing.Point(18, 29);
			this.upServerLabel.Name = "upServerLabel";
			this.upServerLabel.Size = new System.Drawing.Size(54, 13);
			this.upServerLabel.TabIndex = 0;
			this.upServerLabel.Text = "Server IP:";
			// 
			// label2
			// 
			this.label2.AutoSize = true;
			this.label2.Location = new System.Drawing.Point(18, 66);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(35, 13);
			this.label2.TabIndex = 1;
			this.label2.Text = "label2";
			// 
			// upConnectButton
			// 
			this.upConnectButton.Location = new System.Drawing.Point(73, 116);
			this.upConnectButton.Name = "upConnectButton";
			this.upConnectButton.Size = new System.Drawing.Size(75, 23);
			this.upConnectButton.TabIndex = 2;
			this.upConnectButton.Text = "Connect";
			this.upConnectButton.UseVisualStyleBackColor = true;
			// 
			// upServerTextBox
			// 
			this.upServerTextBox.Location = new System.Drawing.Point(73, 26);
			this.upServerTextBox.Name = "upServerTextBox";
			this.upServerTextBox.Size = new System.Drawing.Size(121, 20);
			this.upServerTextBox.TabIndex = 3;
			// 
			// ConnectUserControl
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.Controls.Add(this.upServerTextBox);
			this.Controls.Add(this.upConnectButton);
			this.Controls.Add(this.label2);
			this.Controls.Add(this.upServerLabel);
			this.Name = "ConnectUserControl";
			this.Size = new System.Drawing.Size(328, 276);
			this.ResumeLayout(false);
			this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.Label upServerLabel;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.Button upConnectButton;
		private System.Windows.Forms.TextBox upServerTextBox;
	}
}
