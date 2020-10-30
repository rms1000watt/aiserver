using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using AppClient;

namespace AppTestClient
{
    public partial class AppTestCloseForm : Form
    {
        public AppTestCloseForm()
        {
            InitializeComponent();

            uCloseModeCombo.Items.AddRange(Enum.GetNames(typeof(ACloseMode)));
            uCloseModeCombo.Text = ACloseMode.Default.ToString();
        }

        public string CloseMode
        {
            get { return uCloseModeCombo.Text; }
        }
    }
}