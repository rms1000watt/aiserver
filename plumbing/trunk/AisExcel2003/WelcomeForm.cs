using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace AisExcel2003
{
    public partial class WelcomeForm : Form
    {
        public WelcomeForm()
        {
            InitializeComponent();
        }

        /// <summary>
        /// Override the signal for WM_CLOSE(0x0010).  Hide the current window instead of closing it.
        /// </summary>
        /// <param name="m"></param>
        protected override void WndProc(ref System.Windows.Forms.Message m)
        {
            if (m.Msg != 0x0010)
            {
                base.WndProc(ref m);
            }
            else
            {   //Windows has send the WM_CLOSE message to your form.
                //Ignore this message will make the window stay open.
                //MessageBox.Show("Closing");
                Hide();
            }
        }
    }
}
