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
#ifndef AIDEPREFDIALOG_H
#define AIDEPREFDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/aideprefdialog.h
												AMainWindow Preference Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2009   5/08/2008	fchua	Added getCabinetParameters, setCabinetParameters, getDebugParameters, setDebugParameters
3.2008	 4/07/2008	fchua	Initial Version

												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#include <QtGui/QDialog>
#include "ui_aideprefdialog.h"
#include "aparameters.h"

/*!
 * \brief The IDE Preference dialog allows the user to modify some of the IDE settings.
 */
class AIdePrefDialog : public QDialog
{
	Q_OBJECT

public:
	AIdePrefDialog(QWidget *ipParent = 0, const char *ipName = 0, Qt::WFlags iFlags = 0);
	~AIdePrefDialog();

	void getCabinetParameters(AParameters *iopParams);
	void getConsoleParameters(AParameters *iopParams);
	void getDebugParameters(AParameters *ioParams);
	void getEditorParameters(AParameters *iopParams);
	void getLogParameters(AParameters *iopParams);
	
	void setCabinetParameters(AParameters *ipParams);
	void setConsoleParameters(AParameters *ipParams);
	void setDebugParameters(AParameters *ipParams);
	void setEditorParameters(AParameters *ipParams);
	void setLogParameters(AParameters *ipParams);

private:
	Ui::AIdePrefDialogClass cUi;

private slots:
	void on_upCancelButton_clicked();
	void on_upOkButton_clicked();
};

#endif // AIDEPREFDIALOG_H
