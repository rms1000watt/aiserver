using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace AppTestClient
{
    public partial class AppTestInputForm : Form
    {
        public AppTestInputForm()
        {
            InitializeComponent();
        }

        public string Prompt
        {
            set { uPromptLabel.Text = value; }
        }

        public string Value
        {
            get { return uInputText.Text; }
        }
    }
}