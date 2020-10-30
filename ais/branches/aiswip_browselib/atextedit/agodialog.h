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

#ifndef AGODIALOG_H
#define AGODIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/agodialog.h
														Go Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2006	 3/10/2008	fchua	Simplified the class interface using signal/slots framework.
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/qdialog.h>
#include "ui_agodialog.h"
#include "aoperationsdialog.h"

//	------------------------------------------------ CLASS DEFINITIONS	-------------------------------------------------------
/*!
 * \brief The user may go directly to a specified line number using this dialog
 *
 * The Go dialog allows the user to enter a line number.  After pressing return or selecting the Go button, the cursor is moved in
 * the text to the specified line.
 */
class AGoDialog : public AOperationsDialog
{
    Q_OBJECT
public:
    AGoDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs=0);
	virtual void init();
	virtual void showOnTop();

signals:
	void goToLine(long iLineNum);

private slots:
	void onGoTo();

private:
	Ui::AGoDialogClass	cUi;
};

#endif // AGODIALOG_H
