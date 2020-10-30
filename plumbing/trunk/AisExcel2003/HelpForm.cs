using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Net.Mail;
using System.Text;
using System.Windows.Forms;
using AppClient;

namespace AisExcel2003
{
	public partial class HelpForm : Form
	{
		public HelpForm()
		{
		InitializeComponent();
		}

		private void upSendButton_Click(object sender, EventArgs e)
		{
			string aFrom = upFromTextBox.Text;
			string aMsg = upMsgTextBox.Text;
			string aSubject = upSubjectTextBox.Text;
			string aTo = "tedwilliams@alum.mit.edu";
			// Here is another alternative, but it also needs the local SMTP server
			try
			{	SmtpClient aClient = new SmtpClient("smtp.west.cox.net", 25);
				aClient.Send(aFrom, aTo, aSubject, aMsg);
			}
			catch (SmtpException iEx)
			{	MessageBox.Show(iEx.Message, "upSendButton: " + iEx.Message, MessageBoxButtons.OK, MessageBoxIcon.Error);
			}
			Close();
		}

		private void upCancelButton_Click(object sender, EventArgs e)
		{
			Close();
		}
	}
}
