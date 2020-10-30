namespace AppTestClient
{
    partial class AppTestGenericInputForm
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
            this.uItemsPanel = new System.Windows.Forms.FlowLayoutPanel();
            this.SuspendLayout();
            // 
            // uCancelButton
            // 
            this.uCancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.uCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.uCancelButton.Location = new System.Drawing.Point(207, 163);
            this.uCancelButton.Name = "uCancelButton";
            this.uCancelButton.Size = new System.Drawing.Size(75, 23);
            this.uCancelButton.TabIndex = 1;
            this.uCancelButton.Text = "&Cancel";
            this.uCancelButton.UseVisualStyleBackColor = true;
            // 
            // uOKButton
            // 
            this.uOKButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.uOKButton.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.uOKButton.Location = new System.Drawing.Point(126, 163);
            this.uOKButton.Name = "uOKButton";
            this.uOKButton.Size = new System.Drawing.Size(75, 23);
            this.uOKButton.TabIndex = 0;
            this.uOKButton.Text = "&OK";
            this.uOKButton.UseVisualStyleBackColor = true;
            // 
            // uItemsPanel
            // 
            this.uItemsPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.uItemsPanel.AutoSize = true;
            this.uItemsPanel.FlowDirection = System.Windows.Forms.FlowDirection.TopDown;
            this.uItemsPanel.Location = new System.Drawing.Point(12, 12);
            this.uItemsPanel.Name = "uItemsPanel";
            this.uItemsPanel.Size = new System.Drawing.Size(270, 145);
            this.uItemsPanel.TabIndex = 2;
            this.uItemsPanel.WrapContents = false;
            // 
            // AppTestGenericInputForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(294, 198);
            this.Controls.Add(this.uItemsPanel);
            this.Controls.Add(this.uOKButton);
            this.Controls.Add(this.uCancelButton);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "AppTestGenericInputForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "AppTestGenericInputForm";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button uCancelButton;
        private System.Windows.Forms.Button uOKButton;
        private System.Windows.Forms.FlowLayoutPanel uItemsPanel;
    }
}