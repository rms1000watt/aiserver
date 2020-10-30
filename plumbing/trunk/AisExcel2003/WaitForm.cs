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
	public partial class upWaitForm : Form
	{
        #region Delegates
        // required for the cross-thread component update
        private delegate void setVisibleForm(bool iEnabled);
        #endregion

		public upWaitForm()
		{
			InitializeComponent();
		}
		private void upCloseButton_Click(object sender, EventArgs e)
		{
			Hide();
		}

		private void upCancelButton_Click(object sender, EventArgs e)
		{
			// Use SetEscape to stop currently running task.
            int aSessionId = Globals.ThisAddIn.cSessionId;		// SetEscape uses the current active session ID if aSessionId is 0.
            int aRet = Globals.ThisAddIn.cAppClient.SetEscape(Globals.ThisAddIn.cReceiver, aSessionId);
            Hide();
		}

        public void setVisible(bool iEnabled)
        {
            if (this.InvokeRequired)
            {
                this.BeginInvoke(new setVisibleForm(this.setVisible), iEnabled);
            }
            else
            {
                if (iEnabled)
                    this.Show();
                else
                    this.Hide();
            }
        }
	}
}
