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

#ifndef AHELPALPHADIALOG_H
#define AHELPALPHADIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/ahelpalphadialog.h
														Help Dialog

CHANGE HISTORY
Version	Date		Who		Change
2.0003	1/7/2007	tlw		Alt-F1. Add alphabetical list of edit commands
												--------------- ------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include "ui_ahelpalphadialog.h"

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief AHelpAlphaDialog - Shows a brief description of the keyboard and mouse edit commands

 */
class AHelpAlphaDialog : public QDialog
{
	Q_OBJECT
public:
    AHelpAlphaDialog(QWidget* ipParent, const char* ipName);
	void	closeDialog();

private:
	Ui::AHelpAlphaDialogClass	cUi;
};

#endif // AHELPALPHADIALOG_H
