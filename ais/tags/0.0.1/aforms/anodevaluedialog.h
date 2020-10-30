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
#ifndef ANODEVALUEDIALOG_H
#define ANODEVALUEDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/anodevaluedialog.h
															Node Value Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0100	6/11/2006	tlw		Add Doxygen documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include "ui_anodevaluedialog.h"

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief ANodeValueDialog - A simple dialog to show the value of selected return types from selecting a memory element with a value.

*/
class ANodeValueDialog : public QDialog
{
    Q_OBJECT
public:
    ANodeValueDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFl = 0);
	void	setup(const QString& irData, const QString& irTitle);

private:
	Ui::ANodeValueDialogClass	cUi;
};

#endif // ANODEVALUEDIALOG_H
