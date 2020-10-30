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
#ifndef ACLOSEDIALOG_H
#define ACLOSEDIALOG_H

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/aclosedialog.h
															Close Dialog Page

CHANGE HISTORY
Version	Date		Who		Change
4.0003	 7/01/2008	fchua	[CR-120] Added function to describe default close mode.
3.1005	12/14/2007	fchua	Initial version.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------

#include <QtGui/QDialog>
#include "aglobals.h"
#include "ui_aclosedialog.h"

//	------------------------------------------------------- CLASSES -----------------------------------------------------------

/*!
 * \brief This dialog is used when closing an active session form.
 */
class ACloseDialog : public QDialog
{
	Q_OBJECT

public:
	ACloseDialog(QWidget *ipParent = 0, const QString &irName = "");
	~ACloseDialog();
	ACloseMode getCloseMode();
	void setDefaultCloseMode(ACloseMode iCloseMode);

private:
	Ui::ACloseDialogClass cUi;
	ACloseMode cCloseMode;

private slots:
	void on_uiHardRadio_toggled(bool);
	void on_uiFirmRadio_toggled(bool);
	void on_uiSoftRadio_toggled(bool);
	void on_uiDisconnectRadio_toggled(bool);
	void on_uiDefaultRadio_toggled(bool);
	void on_uiCancelButton_clicked();
	void on_uiOkButton_clicked();
};

#endif // ACLOSEDIALOG_H
