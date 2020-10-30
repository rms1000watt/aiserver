namespace AisExcel2003
{
	partial class HelpForm
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
		this.upSubjectTextBox = new System.Windows.Forms.TextBox();
		this.upSubjectLabel = new System.Windows.Forms.Label();
		this.upMsgLabel = new System.Windows.Forms.Label();
		this.upMsgTextBox = new System.Windows.Forms.TextBox();
		this.upSendButton = new System.Windows.Forms.Button();
		this.upCancelButton = new System.Windows.Forms.Button();
		this.upHelpToolTip = new System.Windows.Forms.ToolTip(this.components);
		this.upFromTextBox = new System.Windows.Forms.TextBox();
		this.upFromLabel = new System.Windows.Forms.Label();
		this.SuspendLayout();
		// 
		// upSubjectTextBox
		// 
		this.upSubjectTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
					| System.Windows.Forms.AnchorStyles.Right)));
		this.upSubjectTextBox.Location = new System.Drawing.Point(7, 38);
		this.upSubjectTextBox.Name = "upSubjectTextBox";
		this.upSubjectTextBox.Size = new System.Drawing.Size(242, 20);
		this.upSubjectTextBox.TabIndex = 0;
		this.upSubjectTextBox.Text = "Enter topic, brief description";
		this.upHelpToolTip.SetToolTip(this.upSubjectTextBox, "Enter type of message and brief description, such as \r\nHow do I ..., \r\nSuggestion" +
				":..., \r\nBug:.., \r\nQuestion:...");
		// 
		// upSubjectLabel
		// 
		this.upSubjectLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
		this.upSubjectLabel.Location = new System.Drawing.Point(4, 20);
		this.upSubjectLabel.Name = "upSubjectLabel";
		this.upSubjectLabel.Size = new System.Drawing.Size(54, 13);
		this.upSubjectLabel.TabIndex = 1;
		this.upSubjectLabel.Text = "Subject:";
		// 
		// upMsgLabel
		// 
		this.upMsgLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
		this.upMsgLabel.Location = new System.Drawing.Point(4, 72);
		this.upMsgLabel.Name = "upMsgLabel";
		this.upMsgLabel.Size = new System.Drawing.Size(61, 13);
		this.upMsgLabel.TabIndex = 2;
		this.upMsgLabel.Text = "Message:";
		// 
		// upMsgTextBox
		// 
		this.upMsgTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
					| System.Windows.Forms.AnchorStyles.Left)
					| System.Windows.Forms.AnchorStyles.Right)));
		this.upMsgTextBox.Location = new System.Drawing.Point(2, 88);
		this.upMsgTextBox.Multiline = true;
		this.upMsgTextBox.Name = "upMsgTextBox";
		this.upMsgTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
		this.upMsgTextBox.Size = new System.Drawing.Size(247, 163);
		this.upMsgTextBox.TabIndex = 3;
		this.upMsgTextBox.Text = "Add detailed description with exact steps to reproduce the situation here...";
		this.upHelpToolTip.SetToolTip(this.upMsgTextBox, "Enter detailed description, including the exact sequence of steps \r\nto illustrate" +
				" the problem, suggestion, question, etc.  It is helpful to \r\nbe able to reproduc" +
				"e the environment.");
		// 
		// upSendButton
		// 
		this.upSendButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
		this.upSendButton.Location = new System.Drawing.Point(176, 317);
		this.upSendButton.Name = "upSendButton";
		this.upSendButton.Size = new System.Drawing.Size(75, 23);
		this.upSendButton.TabIndex = 4;
		this.upSendButton.Text = "Send";
		this.upHelpToolTip.SetToolTip(this.upSendButton, "Click on Send to send this email to GSM Administrator.\r\nWe will make every effort" +
				" to get back to you the next\r\nbusiness day.");
		this.upSendButton.UseVisualStyleBackColor = true;
		this.upSendButton.Click += new System.EventHandler(this.upSendButton_Click);
		// 
		// upCancelButton
		// 
		this.upCancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
		this.upCancelButton.Location = new System.Drawing.Point(74, 317);
		this.upCancelButton.Name = "upCancelButton";
		this.upCancelButton.Size = new System.Drawing.Size(75, 23);
		this.upCancelButton.TabIndex = 5;
		this.upCancelButton.Text = "Cancel";
		this.upHelpToolTip.SetToolTip(this.upCancelButton, "Click on Cancel to return to the GSM Demo form.");
		this.upCancelButton.UseVisualStyleBackColor = true;
		this.upCancelButton.Click += new System.EventHandler(this.upCancelButton_Click);
		// 
		// upFromTextBox
		// 
		this.upFromTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
					| System.Windows.Forms.AnchorStyles.Right)));
		this.upFromTextBox.Location = new System.Drawing.Point(7, 281);
		this.upFromTextBox.Name = "upFromTextBox";
		this.upFromTextBox.Size = new System.Drawing.Size(242, 20);
		this.upFromTextBox.TabIndex = 6;
		this.upFromTextBox.Text = "yourname@provider.com";
		// 
		// upFromLabel
		// 
		this.upFromLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
		this.upFromLabel.AutoSize = true;
		this.upFromLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
		this.upFromLabel.Location = new System.Drawing.Point(7, 262);
		this.upFromLabel.Name = "upFromLabel";
		this.upFromLabel.Size = new System.Drawing.Size(120, 13);
		this.upFromLabel.TabIndex = 7;
		this.upFromLabel.Text = "Your Email Address:";
		// 
		// HelpForm
		// 
		this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
		this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
		this.AutoSize = true;
		this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
		this.ClientSize = new System.Drawing.Size(263, 362);
		this.Controls.Add(this.upFromLabel);
		this.Controls.Add(this.upFromTextBox);
		this.Controls.Add(this.upCancelButton);
		this.Controls.Add(this.upSendButton);
		this.Controls.Add(this.upMsgTextBox);
		this.Controls.Add(this.upMsgLabel);
		this.Controls.Add(this.upSubjectLabel);
		this.Controls.Add(this.upSubjectTextBox);
		this.Name = "HelpForm";
		this.Text = "HelpForm";
		this.ResumeLayout(false);
		this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.TextBox upSubjectTextBox;
		private System.Windows.Forms.Label upSubjectLabel;
		private System.Windows.Forms.Label upMsgLabel;
		private System.Windows.Forms.TextBox upMsgTextBox;
		private System.Windows.Forms.Button upSendButton;
		private System.Windows.Forms.Button upCancelButton;
		private System.Windows.Forms.ToolTip upHelpToolTip;
		private System.Windows.Forms.TextBox upFromTextBox;
		private System.Windows.Forms.Label upFromLabel;
	}
}