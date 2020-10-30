namespace AppTestClient
{
    partial class AppTestCloseForm
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
            this.uCloseModeCombo = new System.Windows.Forms.ComboBox();
            this.uCloseModeLabel = new System.Windows.Forms.Label();
            this.uCancelButton = new System.Windows.Forms.Button();
            this.uOKButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // uCloseModeCombo
            // 
            this.uCloseModeCombo.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uCloseModeCombo.FormattingEnabled = true;
            this.uCloseModeCombo.Location = new System.Drawing.Point(85, 10);
            this.uCloseModeCombo.Name = "uCloseModeCombo";
            this.uCloseModeCombo.Size = new System.Drawing.Size(158, 21);
            this.uCloseModeCombo.TabIndex = 0;
            // 
            // uCloseModeLabel
            // 
            this.uCloseModeLabel.AutoSize = true;
            this.uCloseModeLabel.Location = new System.Drawing.Point(13, 13);
            this.uCloseModeLabel.Name = "uCloseModeLabel";
            this.uCloseModeLabel.Size = new System.Drawing.Size(66, 13);
            this.uCloseModeLabel.TabIndex = 1;
            this.uCloseModeLabel.Text = "Close Mode:";
            // 
            // uCancelButton
            // 
            this.uCancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.uCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.uCancelButton.Location = new System.Drawing.Point(168, 39);
            this.uCancelButton.Name = "uCancelButton";
            this.uCancelButton.Size = new System.Drawing.Size(75, 23);
            this.uCancelButton.TabIndex = 2;
            this.uCancelButton.Text = "&Cancel";
            this.uCancelButton.UseVisualStyleBackColor = true;
            // 
            // uOKButton
            // 
            this.uOKButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.uOKButton.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.uOKButton.Location = new System.Drawing.Point(87, 39);
            this.uOKButton.Name = "uOKButton";
            this.uOKButton.Size = new System.Drawing.Size(75, 23);
            this.uOKButton.TabIndex = 1;
            this.uOKButton.Text = "&OK";
            this.uOKButton.UseVisualStyleBackColor = true;
            // 
            // AppTestCloseForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(255, 74);
            this.Controls.Add(this.uOKButton);
            this.Controls.Add(this.uCancelButton);
            this.Controls.Add(this.uCloseModeLabel);
            this.Controls.Add(this.uCloseModeCombo);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Name = "AppTestCloseForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Close Mode";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ComboBox uCloseModeCombo;
        private System.Windows.Forms.Label uCloseModeLabel;
        private System.Windows.Forms.Button uCancelButton;
        private System.Windows.Forms.Button uOKButton;
    }
}