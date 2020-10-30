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
#ifndef AABOUTDIALOG_H
#define AABOUTDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/aaboutdialog.h
															About Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		Remove unused destructor.
1.0100	4/20/2006	tlw		Add Doxygen documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include "ui_aaboutdialog.h"

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief AAboutDialog - Show a brief description of the AIS project.
 */
class AAboutDialog : public QDialog
{
    Q_OBJECT
public:
    AAboutDialog(QWidget* ipParent, const char* ipName, bool iModal = false, Qt::WFlags iFlgs = 0);

private:
	Ui::AAboutDialogClass	cUi;
};

#endif // AABOUTDIALOG_H
