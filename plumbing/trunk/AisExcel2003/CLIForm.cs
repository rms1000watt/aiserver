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
    public partial class CLIForm : Form
    {
        #region Delegates
        // required for the cross-thread component update
        private delegate void setEnableSubmitButton(bool iEnabled);
        private delegate void setEnableCommandText(bool iEnabled);
        private delegate void AppendToTextBox(string iText);
        #endregion

        public CLIForm()
        {
            InitializeComponent();
        }

        public void enableCommandText(bool iEnabled)
        {
            if (upCommandTextBox.InvokeRequired)
            {
                upCommandTextBox.BeginInvoke(new setEnableCommandText(this.enableCommandText), iEnabled);
            }
            else
            {
                upCommandTextBox.Enabled = iEnabled;
            }
        }

        public void enableSubmitButton(bool iEnabled)
        {
            if (upSubmitButton.InvokeRequired)
            {
                upSubmitButton.BeginInvoke(new setEnableSubmitButton(this.enableSubmitButton), iEnabled);
            }
            else
            {
                upSubmitButton.Enabled = iEnabled;
            }
        }

        private void upSubmitButton_Click(object sender, EventArgs e)
        {
            submit(upCommandTextBox.Text);
        }

        private void submit(string iText)
        {
            // prepend _ais|eval|exp|
            string aAmpMsg = string.Format("_ais{0}eval{0}exp{0}{1}", "\x7F", iText);
            int aRet = Globals.ThisAddIn.cAppClient.Submit(Globals.ThisAddIn.cReceiver, ref aAmpMsg);
        }

        /// <summary>
        /// Display text in the edit box
        /// </summary>
        /// <param name="iDisplayText"></param>
        public void displayText(string iDisplayText)
        {
            if (upAisMsgTextBox.InvokeRequired)
            {   upAisMsgTextBox.BeginInvoke(new AppendToTextBox(this.displayText), iDisplayText);
            }
            else
            {   upAisMsgTextBox.AppendText(iDisplayText);
            }
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
