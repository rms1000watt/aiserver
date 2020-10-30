/**********************************************************************************
	Copyright (C) 2009 Investment Science Corp.

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************/
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Excel = Microsoft.Office.Interop.Excel;
using Office = Microsoft.Office.Core;
using System.Reflection; 
using AppClient;

namespace AisExcel2003
{
	public partial class ConnectForm : Form
	{   public static AAsyncEventArgs cReceiver;
		public static AAppClient cAppClient;
        public static int cSessionId;
		private ushort _defaultPort = 8081;
		private string _defaultIPAddress = "localhost";
		private string _defaultUsername = "guest";

        #region Delegates
		// required for the cross-thread component update
		private delegate void setEnableServerIPText(bool iEnabled);
		private delegate void setEnableServerPortText(bool iEnabled);
		private delegate void setEnableUserNameText(bool iEnabled);
		private delegate void setEnablePasswordText(bool iEnabled);
		private delegate void setOnConnected(bool iConnected);
		private delegate void setOnWaitDone();
		#endregion

		public ConnectForm()
		{
			InitializeComponent();
			// Initialize GUI default values
			upServerIPText.Text = _defaultIPAddress;
			upServerPortText.Text = string.Format( "{0}", _defaultPort);
			upUsernameText.Text = _defaultUsername;
			upConnectButton.Text = "Connect";
		}

		private void upRemoteHostButton_CheckedChanged(object sender, EventArgs e)
		{
			upServerIPText.Enabled = true;
		}

		private void upLocalhostButton_CheckedChanged(object sender, EventArgs e)
		{
			upServerIPText.Enabled = false;
		}

		private void upCloseButton_Click(object sender, EventArgs e)
		{
			Hide();
		}

		private void upConnectButton_Click(object sender, EventArgs e)
		{	if (upConnectButton.Text == "Connect")
			{	string aHostIP = "";
				ushort aHostPort = 0;
				aHostIP = upServerIPText.Text;
				try
				{   aHostPort = ushort.Parse(upServerPortText.Text);
				}
				catch (Exception)
				{   MessageBox.Show("Invalid Port", "Error");
					return;
				}
                Globals.ThisAddIn.cAppClient.Host = aHostIP;
                Globals.ThisAddIn.cAppClient.Port = aHostPort;
                Globals.ThisAddIn.cAppClient.OpenConnection(Globals.ThisAddIn.cReceiver);
			}
			else
            {   Globals.ThisAddIn.cAppClient.CloseConnection(cReceiver, Globals.ThisAddIn.cSessionId, ACloseMode.Hard);
			}
		}

		public void setConnected(bool iConnected)
		{	if (upConnectButton.InvokeRequired)
			{	upConnectButton.BeginInvoke(new setOnConnected(this.setConnected), iConnected);
			}
			else if (iConnected)
			{		upConnectButton.Text = "Disconnect";
					Hide();
					Globals.ThisAddIn.cGsmDemoForm.enableButtons(true);
			}	
			else
			{		upConnectButton.Text = "Connect";
					Globals.ThisAddIn.cGsmDemoForm.enableButtons(false);
					Hide();
			}
		}

        public void enableServerIPText(bool iEnabled)
		{
			if (upServerIPText.InvokeRequired)
			{	upServerIPText.BeginInvoke(new setEnableServerIPText(this.enableServerIPText), iEnabled);
			}
			else
			{	upServerIPText.Enabled = iEnabled;
			}
		}

        public void enableServerPortText(bool iEnabled)
		{	if (upServerPortText.InvokeRequired)
			{	upServerPortText.BeginInvoke(new setEnableServerPortText(this.enableServerPortText), iEnabled);
			}
			else
			{	upServerPortText.Enabled = iEnabled;
			}
		}

        public void enableUserNameText(bool iEnabled)
		{	if (upUsernameText.InvokeRequired)
			{	upUsernameText.BeginInvoke(new setEnableUserNameText(this.enableUserNameText), iEnabled);
			}
			else
			{	upUsernameText.Enabled = iEnabled;
			}
		}

        public void enablePasswordText(bool iEnabled)
		{
			if (upPasswordText.InvokeRequired)
			{	upPasswordText.BeginInvoke(new setEnablePasswordText(this.enablePasswordText), iEnabled);
			}
			else
			{	upPasswordText.Enabled = iEnabled;
			}
		}

        private void upLaunchButton_Click(object sender, EventArgs e)
        {
            string aAISLocation = "";
            string aStartupLocation = "";
            OpenFileDialog aBrowseDialog = new OpenFileDialog();
            aBrowseDialog.Filter = "Exe Files (*.exe)|*.exe|All Files (*.*)|*.*";
            aBrowseDialog.Title = "Locate AIS Application";
            if (aBrowseDialog.ShowDialog() == DialogResult.Cancel)
                return;
            try
            {
                aAISLocation = aBrowseDialog.FileName;
            }
            catch (Exception iEx)
            {
                MessageBox.Show("Open Parameter file error " + iEx.ToString(), "File Error",
                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                return;
            }

            aBrowseDialog.Filter = "Exe Files (*.sl)|*.sl|All Files (*.*)|*.*";
            aBrowseDialog.Title = "Locate Startup File";
            if (aBrowseDialog.ShowDialog() == DialogResult.Cancel)
                return;
            try
            {
                aStartupLocation = aBrowseDialog.FileName;
            }
            catch (Exception iEx)
            {
                MessageBox.Show("Open Parameter file error " + iEx.ToString(), "File Error",
                MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
                return;
            }
            System.Diagnostics.Process.Start(aAISLocation, aStartupLocation);
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
            {
                //Windows has send the WM_CLOSE message to your form.
                //Ignore this message will make the window stay open.
                //MessageBox.Show("Closing");
                Hide();
            }
        }
	}
}
