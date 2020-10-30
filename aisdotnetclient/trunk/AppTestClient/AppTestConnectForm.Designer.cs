namespace AppTestClient
{
    partial class AppTestConnectForm
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
            this.uCancelButton = new System.Windows.Forms.Button();
            this.uOKButton = new System.Windows.Forms.Button();
            this.uHostLabel = new System.Windows.Forms.Label();
            this.uPortLabel = new System.Windows.Forms.Label();
            this.uHostText = new System.Windows.Forms.TextBox();
            this.uPortText = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // uCancelButton
            // 
            this.uCancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.uCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.uCancelButton.Location = new System.Drawing.Point(203, 74);
            this.uCancelButton.Name = "uCancelButton";
            this.uCancelButton.Size = new System.Drawing.Size(75, 23);
            this.uCancelButton.TabIndex = 3;
            this.uCancelButton.Text = "&Cancel";
            this.uCancelButton.UseVisualStyleBackColor = true;
            // 
            // uOKButton
            // 
            this.uOKButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.uOKButton.Location = new System.Drawing.Point(122, 74);
            this.uOKButton.Name = "uOKButton";
            this.uOKButton.Size = new System.Drawing.Size(75, 23);
            this.uOKButton.TabIndex = 2;
            this.uOKButton.Text = "&OK";
            this.uOKButton.UseVisualStyleBackColor = true;
            // 
            // uHostLabel
            // 
            this.uHostLabel.AutoSize = true;
            this.uHostLabel.Location = new System.Drawing.Point(14, 13);
            this.uHostLabel.Name = "uHostLabel";
            this.uHostLabel.Size = new System.Drawing.Size(32, 13);
            this.uHostLabel.TabIndex = 2;
            this.uHostLabel.Text = "Host:";
            // 
            // uPortLabel
            // 
            this.uPortLabel.AutoSize = true;
            this.uPortLabel.Location = new System.Drawing.Point(14, 41);
            this.uPortLabel.Name = "uPortLabel";
            this.uPortLabel.Size = new System.Drawing.Size(29, 13);
            this.uPortLabel.TabIndex = 3;
            this.uPortLabel.Text = "Port:";
            // 
            // uHostText
            // 
            this.uHostText.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uHostText.Location = new System.Drawing.Point(52, 13);
            this.uHostText.Name = "uHostText";
            this.uHostText.Size = new System.Drawing.Size(226, 20);
            this.uHostText.TabIndex = 0;
            // 
            // uPortText
            // 
            this.uPortText.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uPortText.Location = new System.Drawing.Point(52, 41);
            this.uPortText.Name = "uPortText";
            this.uPortText.Size = new System.Drawing.Size(226, 20);
            this.uPortText.TabIndex = 1;
            // 
            // AppTestConnectForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(290, 109);
            this.Controls.Add(this.uPortText);
            this.Controls.Add(this.uHostText);
            this.Controls.Add(this.uPortLabel);
            this.Controls.Add(this.uHostLabel);
            this.Controls.Add(this.uOKButton);
            this.Controls.Add(this.uCancelButton);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "AppTestConnectForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Connect";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button uCancelButton;
        private System.Windows.Forms.Button uOKButton;
        private System.Windows.Forms.Label uHostLabel;
        private System.Windows.Forms.Label uPortLabel;
        private System.Windows.Forms.TextBox uHostText;
        private System.Windows.Forms.TextBox uPortText;
    }
}