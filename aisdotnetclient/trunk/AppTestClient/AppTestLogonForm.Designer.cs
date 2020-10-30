namespace AppTestClient
{
    partial class AppTestLogonForm
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
            this.uUsernameLabel = new System.Windows.Forms.Label();
            this.uPasswordLabel = new System.Windows.Forms.Label();
            this.uUsernameText = new System.Windows.Forms.TextBox();
            this.uPasswordText = new System.Windows.Forms.TextBox();
            this.uCancelButton = new System.Windows.Forms.Button();
            this.uOKButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // uUsernameLabel
            // 
            this.uUsernameLabel.AutoSize = true;
            this.uUsernameLabel.Location = new System.Drawing.Point(12, 13);
            this.uUsernameLabel.Name = "uUsernameLabel";
            this.uUsernameLabel.Size = new System.Drawing.Size(58, 13);
            this.uUsernameLabel.TabIndex = 0;
            this.uUsernameLabel.Text = "Username:";
            // 
            // uPasswordLabel
            // 
            this.uPasswordLabel.AutoSize = true;
            this.uPasswordLabel.Location = new System.Drawing.Point(12, 40);
            this.uPasswordLabel.Name = "uPasswordLabel";
            this.uPasswordLabel.Size = new System.Drawing.Size(56, 13);
            this.uPasswordLabel.TabIndex = 1;
            this.uPasswordLabel.Text = "Password:";
            // 
            // uUsernameText
            // 
            this.uUsernameText.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uUsernameText.Location = new System.Drawing.Point(78, 13);
            this.uUsernameText.Name = "uUsernameText";
            this.uUsernameText.Size = new System.Drawing.Size(197, 20);
            this.uUsernameText.TabIndex = 0;
            // 
            // uPasswordText
            // 
            this.uPasswordText.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uPasswordText.Location = new System.Drawing.Point(78, 40);
            this.uPasswordText.Name = "uPasswordText";
            this.uPasswordText.PasswordChar = '*';
            this.uPasswordText.Size = new System.Drawing.Size(197, 20);
            this.uPasswordText.TabIndex = 1;
            // 
            // uCancelButton
            // 
            this.uCancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.uCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.uCancelButton.Location = new System.Drawing.Point(200, 69);
            this.uCancelButton.Name = "uCancelButton";
            this.uCancelButton.Size = new System.Drawing.Size(75, 23);
            this.uCancelButton.TabIndex = 3;
            this.uCancelButton.Text = "&Cancel";
            this.uCancelButton.UseVisualStyleBackColor = true;
            // 
            // uOKButton
            // 
            this.uOKButton.Location = new System.Drawing.Point(119, 69);
            this.uOKButton.Name = "uOKButton";
            this.uOKButton.Size = new System.Drawing.Size(75, 23);
            this.uOKButton.TabIndex = 2;
            this.uOKButton.Text = "&OK";
            this.uOKButton.UseVisualStyleBackColor = true;
            // 
            // AppTestLogonForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(287, 104);
            this.Controls.Add(this.uOKButton);
            this.Controls.Add(this.uCancelButton);
            this.Controls.Add(this.uPasswordText);
            this.Controls.Add(this.uUsernameText);
            this.Controls.Add(this.uPasswordLabel);
            this.Controls.Add(this.uUsernameLabel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "AppTestLogonForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Logon";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label uUsernameLabel;
        private System.Windows.Forms.Label uPasswordLabel;
        private System.Windows.Forms.TextBox uUsernameText;
        private System.Windows.Forms.TextBox uPasswordText;
        private System.Windows.Forms.Button uCancelButton;
        private System.Windows.Forms.Button uOKButton;
    }
}