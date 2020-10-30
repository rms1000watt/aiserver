namespace AisExcel2003
{
    partial class WelcomeForm
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(WelcomeForm));
            this.upWelcomeTextBox = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // upWelcomeTextBox
            // 
            this.upWelcomeTextBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.upWelcomeTextBox.Location = new System.Drawing.Point(12, 12);
            this.upWelcomeTextBox.Multiline = true;
            this.upWelcomeTextBox.Name = "upWelcomeTextBox";
            this.upWelcomeTextBox.ReadOnly = true;
            this.upWelcomeTextBox.Size = new System.Drawing.Size(328, 238);
            this.upWelcomeTextBox.TabIndex = 2;
            this.upWelcomeTextBox.TabStop = false;
            this.upWelcomeTextBox.Text = resources.GetString("upWelcomeTextBox.Text");
            // 
            // WelcomeForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(352, 262);
            this.Controls.Add(this.upWelcomeTextBox);
            this.Name = "WelcomeForm";
            this.Text = "Welcome to the GSM Demo";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox upWelcomeTextBox;

    }
}