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

#ifndef ATEXTEDITPREFDIALOG_H
#define ATEXTEDITPREFDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/atexteditprefdialog.h
												ATextEdit Preference Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2008	 4/01/2008	fchua	Initial Version

												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#include "ui_atexteditprefdialog.h"
#include "aprefdialog.h"

/*!
 * \brief The Edit Preferences dialog allows the user to modify some of the AIS editor parameters.
 *
 * This dialog allows the modification of the following values:
 * - Font
 * - Font Size
 * - Line Number
 */
class ATextEditPrefDialog : public APrefDialog
{
	Q_OBJECT

public:
	ATextEditPrefDialog(QWidget *ipParent = 0, const char *ipName = 0, Qt::WFlags iFlags = 0);
	~ATextEditPrefDialog();

	virtual void setParameters(AParameters *ipParams); // Set Parameters
	virtual void getParameters(AParameters *iopParams); // Get Parameters

private:
	Ui::ATextEditPrefDialogClass cUi;

private slots:
	void on_upCancel_clicked();
	void on_upOk_clicked();
};

#endif // ATEXTEDITPREFDIALOG_H
