namespace AisExcel2003
{
    partial class CLIForm
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
            this.upCmdLabel = new System.Windows.Forms.Label();
            this.upSubmitButton = new System.Windows.Forms.Button();
            this.upAisMsgTextBox = new System.Windows.Forms.TextBox();
            this.upCommandTextBox = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // upCmdLabel
            // 
            this.upCmdLabel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.upCmdLabel.Location = new System.Drawing.Point(3, 196);
            this.upCmdLabel.Name = "upCmdLabel";
            this.upCmdLabel.Size = new System.Drawing.Size(68, 19);
            this.upCmdLabel.TabIndex = 43;
            this.upCmdLabel.Text = "Command:";
            this.upCmdLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // upSubmitButton
            // 
            this.upSubmitButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.upSubmitButton.Location = new System.Drawing.Point(253, 196);
            this.upSubmitButton.Name = "upSubmitButton";
            this.upSubmitButton.Size = new System.Drawing.Size(67, 23);
            this.upSubmitButton.TabIndex = 41;
            this.upSubmitButton.Text = "Submit";
            this.upSubmitButton.UseVisualStyleBackColor = true;
            this.upSubmitButton.Click += new System.EventHandler(this.upSubmitButton_Click);
            // 
            // upAisMsgTextBox
            // 
            this.upAisMsgTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.upAisMsgTextBox.Location = new System.Drawing.Point(3, 7);
            this.upAisMsgTextBox.Multiline = true;
            this.upAisMsgTextBox.Name = "upAisMsgTextBox";
            this.upAisMsgTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.upAisMsgTextBox.Size = new System.Drawing.Size(320, 176);
            this.upAisMsgTextBox.TabIndex = 40;
            this.upAisMsgTextBox.TabStop = false;
            this.upAisMsgTextBox.Text = "AIS Messages";
            // 
            // upCommandTextBox
            // 
            this.upCommandTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.upCommandTextBox.Location = new System.Drawing.Point(67, 195);
            this.upCommandTextBox.Name = "upCommandTextBox";
            this.upCommandTextBox.Size = new System.Drawing.Size(180, 20);
            this.upCommandTextBox.TabIndex = 42;
            // 
            // CLIForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(326, 226);
            this.Controls.Add(this.upCommandTextBox);
            this.Controls.Add(this.upCmdLabel);
            this.Controls.Add(this.upSubmitButton);
            this.Controls.Add(this.upAisMsgTextBox);
            this.Name = "CLIForm";
            this.Text = "AIS Messages";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label upCmdLabel;
        private System.Windows.Forms.Button upSubmitButton;
        private System.Windows.Forms.TextBox upAisMsgTextBox;
        private System.Windows.Forms.TextBox upCommandTextBox;

    }
}