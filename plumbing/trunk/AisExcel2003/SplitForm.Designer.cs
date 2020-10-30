namespace AisExcel2003
{
	partial class SplitForm
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
		System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SplitForm));
		this.upBandedCheckBox = new System.Windows.Forms.CheckBox();
		this.upCancelButton = new System.Windows.Forms.Button();
		this.upFileLabel = new System.Windows.Forms.Label();
		this.upFileTextBox = new System.Windows.Forms.TextBox();
		this.upSplitButton = new System.Windows.Forms.Button();
		this.upBrowseButton = new System.Windows.Forms.Button();
		this.upSplitLabel = new System.Windows.Forms.Label();
		this.upPctLabel = new System.Windows.Forms.Label();
		this.upPctUpDown = new System.Windows.Forms.NumericUpDown();
		((System.ComponentModel.ISupportInitialize)(this.upPctUpDown)).BeginInit();
		this.SuspendLayout();
		// 
		// upBandedCheckBox
		// 
		this.upBandedCheckBox.AutoSize = true;
		this.upBandedCheckBox.Location = new System.Drawing.Point(15, 251);
		this.upBandedCheckBox.Name = "upBandedCheckBox";
		this.upBandedCheckBox.Size = new System.Drawing.Size(99, 30);
		this.upBandedCheckBox.TabIndex = 0;
		this.upBandedCheckBox.Text = "Banded\r\n (every nth row)";
		this.upBandedCheckBox.UseVisualStyleBackColor = true;
		// 
		// upCancelButton
		// 
		this.upCancelButton.Location = new System.Drawing.Point(114, 301);
		this.upCancelButton.Name = "upCancelButton";
		this.upCancelButton.Size = new System.Drawing.Size(75, 23);
		this.upCancelButton.TabIndex = 1;
		this.upCancelButton.Text = "Cancel";
		this.upCancelButton.UseVisualStyleBackColor = true;
		this.upCancelButton.Click += new System.EventHandler(this.upCancelButton_Click);
		// 
		// upFileLabel
		// 
		this.upFileLabel.AutoSize = true;
		this.upFileLabel.Location = new System.Drawing.Point(12, 215);
		this.upFileLabel.Name = "upFileLabel";
		this.upFileLabel.Size = new System.Drawing.Size(49, 26);
		this.upFileLabel.TabIndex = 2;
		this.upFileLabel.Text = "Data File\r\nLocation";
		// 
		// upFileTextBox
		// 
		this.upFileTextBox.Location = new System.Drawing.Point(72, 218);
		this.upFileTextBox.Name = "upFileTextBox";
		this.upFileTextBox.Size = new System.Drawing.Size(212, 20);
		this.upFileTextBox.TabIndex = 3;
		// 
		// upSplitButton
		// 
		this.upSplitButton.Location = new System.Drawing.Point(229, 301);
		this.upSplitButton.Name = "upSplitButton";
		this.upSplitButton.Size = new System.Drawing.Size(75, 23);
		this.upSplitButton.TabIndex = 4;
		this.upSplitButton.Text = "Split";
		this.upSplitButton.UseVisualStyleBackColor = true;
		this.upSplitButton.Click += new System.EventHandler(this.upSplitButton_Click);
		// 
		// upBrowseButton
		// 
		this.upBrowseButton.Location = new System.Drawing.Point(290, 218);
		this.upBrowseButton.Name = "upBrowseButton";
		this.upBrowseButton.Size = new System.Drawing.Size(32, 23);
		this.upBrowseButton.TabIndex = 5;
		this.upBrowseButton.Text = "->";
		this.upBrowseButton.UseVisualStyleBackColor = true;
		this.upBrowseButton.Click += new System.EventHandler(this.upBrowseButton_Click);
		// 
		// upSplitLabel
		// 
		this.upSplitLabel.AutoSize = true;
		this.upSplitLabel.Location = new System.Drawing.Point(4, 4);
		this.upSplitLabel.Name = "upSplitLabel";
		this.upSplitLabel.Size = new System.Drawing.Size(327, 195);
		this.upSplitLabel.TabIndex = 6;
		this.upSplitLabel.Text = resources.GetString("upSplitLabel.Text");
		// 
		// upPctLabel
		// 
		this.upPctLabel.AutoSize = true;
		this.upPctLabel.Location = new System.Drawing.Point(226, 251);
		this.upPctLabel.Name = "upPctLabel";
		this.upPctLabel.Size = new System.Drawing.Size(45, 26);
		this.upPctLabel.TabIndex = 7;
		this.upPctLabel.Text = "Percent\r\nTraining";
		// 
		// upPctUpDown
		// 
		this.upPctUpDown.Location = new System.Drawing.Point(277, 257);
		this.upPctUpDown.Name = "upPctUpDown";
		this.upPctUpDown.Size = new System.Drawing.Size(45, 20);
		this.upPctUpDown.TabIndex = 8;
		this.upPctUpDown.Value = new decimal(new int[] {
            90,
            0,
            0,
            0});
		// 
		// SplitForm
		// 
		this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
		this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
		this.ClientSize = new System.Drawing.Size(363, 340);
		this.Controls.Add(this.upPctUpDown);
		this.Controls.Add(this.upPctLabel);
		this.Controls.Add(this.upSplitLabel);
		this.Controls.Add(this.upBrowseButton);
		this.Controls.Add(this.upSplitButton);
		this.Controls.Add(this.upFileTextBox);
		this.Controls.Add(this.upFileLabel);
		this.Controls.Add(this.upCancelButton);
		this.Controls.Add(this.upBandedCheckBox);
		this.Name = "SplitForm";
		this.Text = "Split Data File";
		((System.ComponentModel.ISupportInitialize)(this.upPctUpDown)).EndInit();
		this.ResumeLayout(false);
		this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.CheckBox upBandedCheckBox;
		private System.Windows.Forms.Button upCancelButton;
		private System.Windows.Forms.Label upFileLabel;
		private System.Windows.Forms.TextBox upFileTextBox;
		private System.Windows.Forms.Button upSplitButton;
		private System.Windows.Forms.Button upBrowseButton;
		private System.Windows.Forms.Label upSplitLabel;
		private System.Windows.Forms.Label upPctLabel;
		private System.Windows.Forms.NumericUpDown upPctUpDown;
	}
}