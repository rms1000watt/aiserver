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
#ifndef ASERVERDIALOG_H
#define ASERVERDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/webide/aserverdialog.h
														Logon Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0039	4/29/2004	tlw		Revise specification
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include "ui_aserverdialog.h"

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief AServerDialog - Dialog to allow user to set the local name, IP address and port of a server.

 */
class AServerDialog : public QDialog
{
    Q_OBJECT

public:
	AServerDialog(QWidget* ipParent = NULL, const char* ipName = NULL, Qt::WFlags iFlgs = 0);
	ushort	getValues(QByteArray& orServerName, QByteArray& orIpAddress);
	void	setValues(const QString& irServerName, const QString& irHostDnsIp, ushort iPort);

protected slots:
	void onInprocessStateChanged(int iState);

private:
	Ui::AServerDialogClass	cUi;
};

#endif // ASERVERDIALOG_H
