using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace AppTestClient
{
    public partial class AppTestLogonForm : Form
    {
        public AppTestLogonForm()
        {
            InitializeComponent();
            uOKButton.Click += new EventHandler(uOKButton_Click);
        }

        void uOKButton_Click(object sender, EventArgs e)
        {
            this.DialogResult = DialogResult.OK;
            this.Close();
        }

        public string Username
        {
            get { return uUsernameText.Text; }
        }

        public string Password
        {
            get { return uPasswordText.Text; }
        }


    }
}