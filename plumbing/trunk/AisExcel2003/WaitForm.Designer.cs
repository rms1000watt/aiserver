namespace AisExcel2003
{
	partial class upWaitForm
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
			{	components.Dispose();
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
			this.upCancelButton = new System.Windows.Forms.Button();
			this.upCloseButton = new System.Windows.Forms.Button();
			this.upWaitLabel = new System.Windows.Forms.Label();
			this.SuspendLayout();
			// 
			// upCancelButton
			// 
			this.upCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.upCancelButton.Location = new System.Drawing.Point(168, 63);
			this.upCancelButton.Name = "upCancelButton";
			this.upCancelButton.Size = new System.Drawing.Size(75, 23);
			this.upCancelButton.TabIndex = 0;
			this.upCancelButton.Text = "Cancel";
			this.upCancelButton.UseVisualStyleBackColor = true;
			this.upCancelButton.Click += new System.EventHandler(this.upCancelButton_Click);
			// 
			// upCloseButton
			// 
			this.upCloseButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.upCloseButton.Location = new System.Drawing.Point(30, 63);
			this.upCloseButton.Name = "upCloseButton";
			this.upCloseButton.Size = new System.Drawing.Size(75, 23);
			this.upCloseButton.TabIndex = 1;
			this.upCloseButton.Text = "Close";
			this.upCloseButton.UseVisualStyleBackColor = true;
			this.upCloseButton.Click += new System.EventHandler(this.upCloseButton_Click);
			// 
			// upWaitLabel
			// 
			this.upWaitLabel.AutoSize = true;
			this.upWaitLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
			this.upWaitLabel.Location = new System.Drawing.Point(12, 9);
			this.upWaitLabel.Name = "upWaitLabel";
			this.upWaitLabel.Size = new System.Drawing.Size(277, 34);
			this.upWaitLabel.TabIndex = 2;
			this.upWaitLabel.Text = "Task is running.  Please wait...\r\nThis dialog will close when it is done.";
			// 
			// upWaitForm
			// 
			this.AcceptButton = this.upCloseButton;
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.CancelButton = this.upCancelButton;
			this.ClientSize = new System.Drawing.Size(292, 98);
			this.Controls.Add(this.upWaitLabel);
			this.Controls.Add(this.upCloseButton);
			this.Controls.Add(this.upCancelButton);
			this.Name = "upWaitForm";
			this.Text = "Task is Running";
			this.ResumeLayout(false);
			this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.Button upCancelButton;
		private System.Windows.Forms.Button upCloseButton;
		private System.Windows.Forms.Label upWaitLabel;
	}
}