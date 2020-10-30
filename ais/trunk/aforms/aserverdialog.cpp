/**********************************************************************************
    Copyright (C) 2008 Investment Science Corp.

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
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/webide/aserverdialog.cpp
											Add Server Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0056	 3/ 3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	----------------------------------------- IMPORTS ---------------------------------------------
#include "aserverdialog.h"
#include "aglobals.h"

//	------------------------------------- CLASS METHODS -------------------------------------------
/*!
\brief AServerDialog - constructor instantiates the GUI elements generated in ui_serverdialog.h
 
\par Args:
\param ipParent -> Parent widget that created this instance of AServerDialog
\param ipName -> Name assigned to this instance of the dialog.
\param iFlgs  Optional flags to set the mode of display.
\return void
 */
AServerDialog::AServerDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs)
    : QDialog(ipParent, iFlgs)
{
	cUi.setupUi(this);

	if (!ipName) setObjectName("AServerDialog");
	cUi.upServerLineEdit->setFocus();

	cUi.upInprocess->setCheckState(Qt::Unchecked);

	// signals and slots connections
	connect(cUi.upCancelButton, SIGNAL(clicked()), this, SLOT(reject()));
	connect(cUi.upOkButton, SIGNAL(clicked()), this, SLOT(accept()));
	connect(cUi.upInprocess, SIGNAL(stateChanged(int)), this, SLOT(onInprocessStateChanged(int)));
}

/*!
\brief getValues - Get current dialog settings

\param orServerName - Default local server name.
\param orIpAddress -> Server's IP address or DNS name (www.investbyagent.com).
\return Port - Server listens on this socket for incoming connections.
*/
ushort AServerDialog::getValues(QByteArray& orServerName, QByteArray& orIpAddress)
{
	orServerName = cUi.upServerLineEdit->text().simplified().toLatin1();
	orIpAddress = cUi.upAddressLineEdit->text().simplified().toLatin1();
	return (ushort)cUi.upPortSpinBox->value();
}

/*!
\brief ASaveAsAgentDialog - constructor instantiates the GUI elements generated in ui_saveasagentdialog.h
 
\par Args:
\param irServerName - Default local server name.
\param irHostDnsIp -> Server's IP address or DNS name (www.investbyagent.com).
\param iPort - Socket that the AIS server listens on for incoming connections.
\return void
 */
void AServerDialog::setValues(const QString& irServerName, const QString& irHostDnsIp, ushort iPort)
{
	cUi.upServerLineEdit->setText(irServerName);
	cUi.upAddressLineEdit->setText(irHostDnsIp);
	cUi.upPortSpinBox->setValue(iPort);
}

void AServerDialog::onInprocessStateChanged(int iState)
{
	if (iState == Qt::Checked)
	{
		cUi.upServerLineEdit->setText(AGLBLS_INPROCSVRNAME);
		cUi.upServerLineEdit->setDisabled(true);

		cUi.upAddressLineEdit->setText("local");
		cUi.upAddressLineEdit->setDisabled(true);

		cUi.upPortSpinBox->setValue(1);
		cUi.upPortSpinBox->setDisabled(true);
	}
	else
	{
		cUi.upServerLineEdit->setText("");
		cUi.upServerLineEdit->setDisabled(false);

		cUi.upAddressLineEdit->setText("");
		cUi.upAddressLineEdit->setDisabled(false);

		cUi.upPortSpinBox->setDisabled(false);
	}
}

// end
