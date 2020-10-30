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
#ifndef AREGISTERDIRDIALOG_H
#define AREGISTERDIRDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/aregisterdirdialog.h
															Register Directory Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0000	7/28/2008	rca		Initial Code
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include "ui_aregisterdirdialog.h"

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief ARegisterDirDialog - Register cabinets in a directory
 */
class ARegisterDirDialog : public QDialog
{
    Q_OBJECT
public:
    ARegisterDirDialog(QWidget* ipParent);
	QString getDatabaseDirectory();
	QString getSameSourceFlag();
	QString getSourceDirectory();
	void setDefault();

public slots:
	void onStateChange();
	void onRegister();
	void onCancel();

private:
	Ui::ARegisterDirClass	cUi;
};

#endif // AREGISTERDIRDIALOG_H
