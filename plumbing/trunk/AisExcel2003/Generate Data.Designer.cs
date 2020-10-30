namespace AisExcel2003
{
    partial class Generate_Data
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
            this.label1 = new System.Windows.Forms.Label();
            this.upClose = new System.Windows.Forms.Button();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.upTestCase = new System.Windows.Forms.ComboBox();
            this.upTestName = new System.Windows.Forms.TextBox();
            this.upRows = new System.Windows.Forms.TextBox();
            this.upColumns = new System.Windows.Forms.TextBox();
            this.upGenerateData = new System.Windows.Forms.Button();
            this.upNumGenerations = new System.Windows.Forms.TextBox();
            this.upAmtNoise = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.upModelName = new System.Windows.Forms.TextBox();
            this.upMaxTime = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.label8 = new System.Windows.Forms.Label();
            this.upNumChampions = new System.Windows.Forms.TextBox();
            this.upKernelID = new System.Windows.Forms.TextBox();
            this.label9 = new System.Windows.Forms.Label();
            this.label10 = new System.Windows.Forms.Label();
            this.upHaltScore = new System.Windows.Forms.TextBox();
            this.label11 = new System.Windows.Forms.Label();
            this.upAdvancedOptions = new System.Windows.Forms.CheckBox();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(16, 15);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(59, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Test Name";
            // 
            // upClose
            // 
            this.upClose.Location = new System.Drawing.Point(274, 46);
            this.upClose.Name = "upClose";
            this.upClose.Size = new System.Drawing.Size(86, 23);
            this.upClose.TabIndex = 19;
            this.upClose.Text = "Close";
            this.upClose.UseVisualStyleBackColor = true;
            this.upClose.Click += new System.EventHandler(this.upClose_Click);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(16, 46);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(55, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "Test Case";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(16, 79);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(86, 13);
            this.label3.TabIndex = 4;
            this.label3.Text = "Number of Rows";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(16, 109);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(99, 13);
            this.label4.TabIndex = 5;
            this.label4.Text = "Number of Columns";
            // 
            // upTestCase
            // 
            this.upTestCase.FormattingEnabled = true;
            this.upTestCase.Location = new System.Drawing.Point(135, 43);
            this.upTestCase.Name = "upTestCase";
            this.upTestCase.Size = new System.Drawing.Size(117, 21);
            this.upTestCase.TabIndex = 2;
            // 
            // upTestName
            // 
            this.upTestName.Location = new System.Drawing.Point(135, 12);
            this.upTestName.Name = "upTestName";
            this.upTestName.Size = new System.Drawing.Size(117, 20);
            this.upTestName.TabIndex = 1;
            // 
            // upRows
            // 
            this.upRows.Location = new System.Drawing.Point(135, 75);
            this.upRows.Name = "upRows";
            this.upRows.Size = new System.Drawing.Size(56, 20);
            this.upRows.TabIndex = 3;
            // 
            // upColumns
            // 
            this.upColumns.Location = new System.Drawing.Point(135, 106);
            this.upColumns.Name = "upColumns";
            this.upColumns.Size = new System.Drawing.Size(56, 20);
            this.upColumns.TabIndex = 4;
            // 
            // upGenerateData
            // 
            this.upGenerateData.Location = new System.Drawing.Point(274, 12);
            this.upGenerateData.Name = "upGenerateData";
            this.upGenerateData.Size = new System.Drawing.Size(86, 23);
            this.upGenerateData.TabIndex = 17;
            this.upGenerateData.Text = "Generate";
            this.upGenerateData.UseVisualStyleBackColor = true;
            this.upGenerateData.Click += new System.EventHandler(this.upGenerateData_Click);
            // 
            // upNumGenerations
            // 
            this.upNumGenerations.Location = new System.Drawing.Point(153, 264);
            this.upNumGenerations.Name = "upNumGenerations";
            this.upNumGenerations.Size = new System.Drawing.Size(56, 20);
            this.upNumGenerations.TabIndex = 9;
            // 
            // upAmtNoise
            // 
            this.upAmtNoise.Location = new System.Drawing.Point(153, 233);
            this.upAmtNoise.Name = "upAmtNoise";
            this.upAmtNoise.Size = new System.Drawing.Size(56, 20);
            this.upAmtNoise.TabIndex = 8;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(34, 267);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(116, 13);
            this.label5.TabIndex = 12;
            this.label5.Text = "Number of Generations";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(34, 237);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(67, 13);
            this.label6.TabIndex = 11;
            this.label6.Text = "Amt of Noise";
            // 
            // upModelName
            // 
            this.upModelName.Location = new System.Drawing.Point(153, 171);
            this.upModelName.Name = "upModelName";
            this.upModelName.Size = new System.Drawing.Size(99, 20);
            this.upModelName.TabIndex = 6;
            // 
            // upMaxTime
            // 
            this.upMaxTime.Location = new System.Drawing.Point(153, 296);
            this.upMaxTime.Name = "upMaxTime";
            this.upMaxTime.Size = new System.Drawing.Size(56, 20);
            this.upMaxTime.TabIndex = 10;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(34, 174);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(67, 13);
            this.label7.TabIndex = 16;
            this.label7.Text = "Model Name";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(34, 300);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(100, 13);
            this.label8.TabIndex = 15;
            this.label8.Text = "Max. Learning Time";
            // 
            // upNumChampions
            // 
            this.upNumChampions.Location = new System.Drawing.Point(153, 356);
            this.upNumChampions.Name = "upNumChampions";
            this.upNumChampions.Size = new System.Drawing.Size(56, 20);
            this.upNumChampions.TabIndex = 12;
            // 
            // upKernelID
            // 
            this.upKernelID.Location = new System.Drawing.Point(153, 201);
            this.upKernelID.Name = "upKernelID";
            this.upKernelID.Size = new System.Drawing.Size(99, 20);
            this.upKernelID.TabIndex = 7;
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(34, 359);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(111, 13);
            this.label9.TabIndex = 22;
            this.label9.Text = "Number of Champions";
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(34, 205);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(77, 13);
            this.label10.TabIndex = 21;
            this.label10.Text = "SVM Kernel ID";
            // 
            // upHaltScore
            // 
            this.upHaltScore.Location = new System.Drawing.Point(153, 326);
            this.upHaltScore.Name = "upHaltScore";
            this.upHaltScore.Size = new System.Drawing.Size(56, 20);
            this.upHaltScore.TabIndex = 11;
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(34, 329);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(112, 13);
            this.label11.TabIndex = 19;
            this.label11.Text = "Training Halting Score";
            // 
            // upAdvancedOptions
            // 
            this.upAdvancedOptions.AutoSize = true;
            this.upAdvancedOptions.Location = new System.Drawing.Point(19, 146);
            this.upAdvancedOptions.Name = "upAdvancedOptions";
            this.upAdvancedOptions.Size = new System.Drawing.Size(114, 17);
            this.upAdvancedOptions.TabIndex = 5;
            this.upAdvancedOptions.Text = "Advanced Options";
            this.upAdvancedOptions.UseVisualStyleBackColor = true;
            this.upAdvancedOptions.CheckedChanged += new System.EventHandler(this.upAdvancedOptions_CheckedChanged);
            // 
            // Generate_Data
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(378, 395);
            this.Controls.Add(this.upAdvancedOptions);
            this.Controls.Add(this.upNumChampions);
            this.Controls.Add(this.upKernelID);
            this.Controls.Add(this.label9);
            this.Controls.Add(this.label10);
            this.Controls.Add(this.upHaltScore);
            this.Controls.Add(this.label11);
            this.Controls.Add(this.upModelName);
            this.Controls.Add(this.upMaxTime);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.upNumGenerations);
            this.Controls.Add(this.upAmtNoise);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.upGenerateData);
            this.Controls.Add(this.upColumns);
            this.Controls.Add(this.upRows);
            this.Controls.Add(this.upTestName);
            this.Controls.Add(this.upTestCase);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.upClose);
            this.Controls.Add(this.label1);
            this.Name = "Generate_Data";
            this.Text = "Generate Data";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button upClose;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.ComboBox upTestCase;
        private System.Windows.Forms.TextBox upTestName;
        private System.Windows.Forms.TextBox upRows;
        private System.Windows.Forms.TextBox upColumns;
        private System.Windows.Forms.Button upGenerateData;
        private System.Windows.Forms.TextBox upNumGenerations;
        private System.Windows.Forms.TextBox upAmtNoise;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox upModelName;
        private System.Windows.Forms.TextBox upMaxTime;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox upNumChampions;
        private System.Windows.Forms.TextBox upKernelID;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.TextBox upHaltScore;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.CheckBox upAdvancedOptions;
    }
}