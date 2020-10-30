namespace AisExcel2003
{
    partial class RunTest
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
            this.upParameterList = new System.Windows.Forms.ListBox();
            this.label1 = new System.Windows.Forms.Label();
            this.upRunButton = new System.Windows.Forms.Button();
            this.upCancelButton = new System.Windows.Forms.Button();
            this.label2 = new System.Windows.Forms.Label();
            this.upTestName = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // upParameterList
            // 
            this.upParameterList.FormattingEnabled = true;
            this.upParameterList.Location = new System.Drawing.Point(12, 73);
            this.upParameterList.Name = "upParameterList";
            this.upParameterList.Size = new System.Drawing.Size(258, 225);
            this.upParameterList.TabIndex = 0;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 46);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(226, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "The test will be using the following parameters:";
            // 
            // upRunButton
            // 
            this.upRunButton.Location = new System.Drawing.Point(36, 311);
            this.upRunButton.Name = "upRunButton";
            this.upRunButton.Size = new System.Drawing.Size(75, 23);
            this.upRunButton.TabIndex = 2;
            this.upRunButton.Text = "Run";
            this.upRunButton.UseVisualStyleBackColor = true;
            this.upRunButton.Click += new System.EventHandler(this.upRunButton_Click);
            // 
            // upCancelButton
            // 
            this.upCancelButton.Location = new System.Drawing.Point(163, 311);
            this.upCancelButton.Name = "upCancelButton";
            this.upCancelButton.Size = new System.Drawing.Size(75, 23);
            this.upCancelButton.TabIndex = 3;
            this.upCancelButton.Text = "Cancel";
            this.upCancelButton.UseVisualStyleBackColor = true;
            this.upCancelButton.Click += new System.EventHandler(this.upCancelButton_Click);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(15, 13);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(59, 13);
            this.label2.TabIndex = 4;
            this.label2.Text = "Test Name";
            // 
            // upTestName
            // 
            this.upTestName.Location = new System.Drawing.Point(80, 10);
            this.upTestName.Name = "upTestName";
            this.upTestName.Size = new System.Drawing.Size(164, 20);
            this.upTestName.TabIndex = 5;
            // 
            // RunTest
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(283, 344);
            this.Controls.Add(this.upTestName);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.upCancelButton);
            this.Controls.Add(this.upRunButton);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.upParameterList);
            this.Name = "RunTest";
            this.Text = "Run Test";
            this.Load += new System.EventHandler(this.RunTest_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ListBox upParameterList;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button upRunButton;
        private System.Windows.Forms.Button upCancelButton;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox upTestName;
    }
}