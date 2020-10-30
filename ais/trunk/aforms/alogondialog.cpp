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
aisdev/webide/alogondialog.cpp
											Logon Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0056	 3/ 3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	----------------------------------------- IMPORTS ---------------------------------------------
#include "alogondialog.h"

//	------------------------------------- CLASS METHODS -------------------------------------------
// Constructs ALogonDialog object as a child of 'parent', with the 'name' and widget flags set to 'f'
// The dialog will be modeless, unless you set 'modal' to true.
ALogonDialog::ALogonDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs)
    : QDialog(ipParent, iFlgs)
{
	cUi.setupUi(this);
	setObjectName(ipName);
	setSizeGripEnabled(false);
	cUi.upUsrNameLineEdit->setFocus();

	// signals and slots connections
	connect(cUi.upCancelButton, SIGNAL(clicked()), this, SLOT(reject()));
	connect(cUi.upOkButton, SIGNAL(clicked()), this, SLOT(accept()));
}

/*!
\brief getValues - Get current dialog settings

\param orUsrName - Place to return the user logon name entered into the dialog
\param orPasswd - Place to return the password entered into the dialog
\return void
*/
void ALogonDialog::getValues(QString& orUsrName, QString& orPasswd)
{
	orUsrName = cUi.upUsrNameLineEdit->text().simplified();
	orPasswd = cUi.upPasswordLineEdit->text().simplified();
}

/*!
\brief setValues - Set the dialog entries to default values

\param irUsrName - The default user logon name
\param irPasswd - The default user password
\return void
*/
void ALogonDialog::setValues(const QString& irUsrName, const QString& irPasswd)
{
	cUi.upUsrNameLineEdit->setText(irUsrName);
	cUi.upPasswordLineEdit->setText(irPasswd);
}
