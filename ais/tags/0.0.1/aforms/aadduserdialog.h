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
#ifndef AADDUSERDIALOG_H
#define AADDUSERDIALOG_H

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/aadduserdialog.h
														Add User Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2001	01/27/2008	fchua	Initial version.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QDate>
#include "ui_aadduserdialog.h"

//  ------------------------------------------------- FORWARD DECLARATIONS ----------------------------------------------------
class QDialog;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
 * \brief Provides a dialog for adding and updating user accounts.
 */
class AAddUserDialog : public QDialog
{
	Q_OBJECT

public:
	AAddUserDialog(QWidget *parent = 0);
	~AAddUserDialog();

	void clearAll();

	const QString	getComment();
	const QDate		getEndDate();
	const QString	getPasssword();
	long			getSecurityLevel();
	const QString	getUsername();

	bool isPasswordSet();
	void setEditOnly(bool iFlag);

	void setComment(const QString& irComment);
	void setEndDate(const QDate& irEndDate);
	void setSecurityLevel(const long iSecurityLevel);
	void setUsername(const QString& irUsername);

private:
	Ui::AAddUserDialogClass cUi;

private slots:
	void on_upChgPasswordCheckBox_stateChanged(int iState);
	void on_upCancelButton_clicked();
	void on_upOkButton_clicked();
};

#endif // AADDUSERDIALOG_H
