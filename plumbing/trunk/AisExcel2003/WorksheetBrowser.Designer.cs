namespace AisExcel2003
{
    partial class WorksheetBrowser
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
            this.upImportButton = new System.Windows.Forms.Button();
            this.upCloseButton = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.upParameterButton = new System.Windows.Forms.Button();
            this.upParameterText = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            this.upImportButton.Location = new System.Drawing.Point(31, 87);
            this.upImportButton.Name = "upImportButton";
            this.upImportButton.Size = new System.Drawing.Size(75, 23);
            this.upImportButton.TabIndex = 0;
            this.upImportButton.Text = "Import";
            this.upImportButton.UseVisualStyleBackColor = true;
            this.upImportButton.Click += new System.EventHandler(this.upImportButton_Click);
            // 
            // upCloseButton
            // 
            this.upCloseButton.Location = new System.Drawing.Point(173, 87);
            this.upCloseButton.Name = "upCloseButton";
            this.upCloseButton.Size = new System.Drawing.Size(75, 23);
            this.upCloseButton.TabIndex = 1;
            this.upCloseButton.Text = "Close";
            this.upCloseButton.UseVisualStyleBackColor = true;
            this.upCloseButton.Click += new System.EventHandler(this.upCloseButton_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(9, 22);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(77, 13);
            this.label1.TabIndex = 2;
            this.label1.Text = "Parameter File:";
            // 
            // upParameterButton
            // 
            this.upParameterButton.Location = new System.Drawing.Point(192, 46);
            this.upParameterButton.Name = "upParameterButton";
            this.upParameterButton.Size = new System.Drawing.Size(75, 23);
            this.upParameterButton.TabIndex = 5;
            this.upParameterButton.Text = "Browse...";
            this.upParameterButton.UseVisualStyleBackColor = true;
            this.upParameterButton.Click += new System.EventHandler(this.upParameterButton_Click);
            // 
            // upParameterText
            // 
            this.upParameterText.Location = new System.Drawing.Point(12, 48);
            this.upParameterText.Name = "upParameterText";
            this.upParameterText.Size = new System.Drawing.Size(174, 20);
            this.upParameterText.TabIndex = 8;
            // 
            // WorksheetBrowser
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(279, 129);
            this.Controls.Add(this.upParameterText);
            this.Controls.Add(this.upParameterButton);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.upCloseButton);
            this.Controls.Add(this.upImportButton);
            this.Name = "WorksheetBrowser";
            this.Text = "Import Parameter File";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button upImportButton;
        private System.Windows.Forms.Button upCloseButton;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button upParameterButton;
        private System.Windows.Forms.TextBox upParameterText;
    }
}