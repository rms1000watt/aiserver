namespace AppTestClient
{
    partial class AppTestInputForm
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
            this.uPromptLabel = new System.Windows.Forms.Label();
            this.uInputText = new System.Windows.Forms.TextBox();
            this.uCancelButton = new System.Windows.Forms.Button();
            this.uOKButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // uPromptLabel
            // 
            this.uPromptLabel.AutoSize = true;
            this.uPromptLabel.Location = new System.Drawing.Point(12, 9);
            this.uPromptLabel.Name = "uPromptLabel";
            this.uPromptLabel.Size = new System.Drawing.Size(43, 13);
            this.uPromptLabel.TabIndex = 0;
            this.uPromptLabel.Text = "Prompt:";
            // 
            // uInputText
            // 
            this.uInputText.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.uInputText.Location = new System.Drawing.Point(120, 9);
            this.uInputText.Name = "uInputText";
            this.uInputText.Size = new System.Drawing.Size(250, 20);
            this.uInputText.TabIndex = 0;
            // 
            // uCancelButton
            // 
            this.uCancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.uCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.uCancelButton.Location = new System.Drawing.Point(295, 39);
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
            this.uOKButton.Location = new System.Drawing.Point(214, 39);
            this.uOKButton.Name = "uOKButton";
            this.uOKButton.Size = new System.Drawing.Size(75, 23);
            this.uOKButton.TabIndex = 1;
            this.uOKButton.Text = "&OK";
            this.uOKButton.UseVisualStyleBackColor = true;
            // 
            // AppTestInputForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(382, 74);
            this.Controls.Add(this.uPromptLabel);
            this.Controls.Add(this.uInputText);
            this.Controls.Add(this.uOKButton);
            this.Controls.Add(this.uCancelButton);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "AppTestInputForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "Input";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label uPromptLabel;
        private System.Windows.Forms.TextBox uInputText;
        private System.Windows.Forms.Button uCancelButton;
        private System.Windows.Forms.Button uOKButton;
    }
}