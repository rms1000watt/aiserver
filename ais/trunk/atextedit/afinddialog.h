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

#ifndef AFINDDIALOG_H
#define AFINDDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/afinddialog.h
														Find Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2006	 3/10/2008	fchua	Simplified the class interface using signal/slots framework.
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include <QtGui/QCloseEvent>
#include "ui_afinddialog.h"
#include "aoperationsdialog.h"

class QSettings;
class ATextEdit;
//	------------------------------------------------ CLASS DEFINITIONS	-------------------------------------------------------
/*!
AFind dialog allows the user to enter a search pattern and configure the search parameters.

\par Notes:
Find provides:
-# Combo text window to enter a search pattern.
-# Search pattern may be text or a regular expression.
-# Search text may include \\n, \\t, \\b, \\r, etc to denote selected control characters.
-# Option to match whole words
-# Option to match case
-# Option to search forward or backward in text.
-# Buttons to find next match or quit.
*/
class AFindDialog : public AOperationsDialog
{
	Q_OBJECT

public:
    AFindDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlags=0);
	virtual void init(QSettings* ipSettings, ATextEdit* ipTextEdit);
	virtual void showOnTop();

signals:
	void	findPattern(const QString& irPattern, bool iAll, bool iMatchCase, bool iDown, bool iRegExp, bool iSelect, bool iMatchWord);

public slots:
	void	onFindPatternResult(bool iResult);

protected:
	void	closeEvent(QCloseEvent *ipEvent);

private slots:
	void	onFind();

private:
	void	saveSettings();

	QSettings*				cpSettings;
	ATextEdit*				cpTextEdit;
	Ui::AFindDialogClass	cUi;
};

#endif // AFINDDIALOG_H

