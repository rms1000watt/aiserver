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

#ifndef AHELPFINDDIALOG_H
#define AHELPFINDDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/ahelpfinddialog.h
													Help Find Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/qdialog.h>		// QDialog
#include "ui_ahelpfinddialog.h"

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief AHelpApiDialog - Show a brief description of the search options for the Find dialog.

A brief description of the search patterns (regular expressions) is included.
 */
class AHelpFindDialog : public QDialog
{
	Q_OBJECT

public:
    AHelpFindDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlags = 0);

private:
	Ui::AHelpFindDialogClass	cUi;
};

#endif // AHELPFINDDIALOG_H
