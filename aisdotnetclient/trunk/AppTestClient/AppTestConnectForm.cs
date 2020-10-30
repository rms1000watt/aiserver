using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace AppTestClient
{
    public partial class AppTestConnectForm : Form
    {
        public AppTestConnectForm()
        {
            InitializeComponent();
            uOKButton.Click += new EventHandler(uOKButton_Click);

            _Port = 0;
        }

        void uOKButton_Click(object sender, EventArgs e)
        {
            // check inputs
            string aHost = uHostText.Text.Trim();
            string aPort = uPortText.Text.Trim();

            if (aHost.Length == 0 || aPort.Length == 0)
            {
                MessageBox.Show("Host and Port fields are required");
                return;
            }

            if (!ushort.TryParse(aPort, out _Port))
            {
                MessageBox.Show("Port field is invalid");
                return;
            }

            this.DialogResult = DialogResult.OK;
            this.Close();
        }

        public string Host
        {
            get { return uHostText.Text; }
        }

        public ushort Port
        {
            get { return _Port; }
        }

        private ushort _Port = 0;


    }
}