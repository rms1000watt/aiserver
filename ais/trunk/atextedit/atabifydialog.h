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

#ifndef ATABIFYDIALOG_H
#define ATABIFYDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/atabifydialog.h
														Tabify Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS -----------------------------------------------------------
#include <QtGui/QDialog>
#include "ui_atabifydialog.h"
class ATextEdit;

//	------------------------------------------------ CLASS DEFINITIONS	-------------------------------------------------------
/*!
\brief ATabifyDialog - Modal dialog to allow the user to modify the tab setting

 */
class ATabifyDialog : public QDialog
{
    Q_OBJECT

public:
    ATabifyDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs=0);
	void	setEditor(ATextEdit* ipTextEdit);

private slots:
	void	onDetab();
	void	onTabify();

private:
	void	tabify(bool iToTabs);

	Ui::ATabifyDialogClass	cUi;
	ATextEdit*		cpTextEdit;		// Parent text editor
};

#endif // ATABIFYDIALOG_H
