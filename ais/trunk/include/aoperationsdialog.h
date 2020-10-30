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

#ifndef AOPERATIONSDIALOG_H
#define AOPERATIONSDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/aoperationsdialog.h
														Operations Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2006	 3/11/2008	fchua	Initial version.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include <QtCore/QStringList>

class ATextEdit;
class QSettings;
class ASessionForm;
class AisEdit;

/*!
 * \brief This class serves as a generic interface for custom dialogs such as find, replace, go, function list.
 *
 * This purpose of this class is to minimize inter-project dependencies by providing a common interface for
 * initialization, display and even signal-slot mechanism.
 *
 * \sa
 * AFindDialog, AReplaceDialog, AFcnListDialog, AGoDialog, ATextEdit
 */
class AOperationsDialog: public QDialog
{
	Q_OBJECT

public:
	AOperationsDialog(QWidget* ipParent, const char* ipName = 0, Qt::WFlags iFlgs = 0);
	virtual ~AOperationsDialog();

	virtual void init();
	virtual void init(ATextEdit* ipTextEdit, QStringList& irList);
	virtual void init(QSettings* ipSettings, ATextEdit* ipTextEdit);
	virtual void showOnTop();

signals:
	// Consolidated signals for Find, Replace, and Go
	void findPattern(const QString& irPattern, bool iAll, bool iMatchCase, bool iDown, bool iRegExp, bool iSelect, bool iMatchWord);
	void replaceFindPattern(const QString& irPattern, bool iAll, bool iMatchCase, bool iDown, bool iRegExp, bool iSelect, bool iMatchWord);
	void replacePattern(const QString& irPattern, const QString& irText, bool iAllText, bool iMatchCase, bool iRegExp, bool iMatchWord);
	void replaceAllPattern(const QString& irPattern, const QString& irText, bool iAllText, bool iMatchCase, bool iRegExp, bool iSelect, bool iMatchWord);
	void goToLine(long iLineNum);

public slots:
	// Consolidated slots for Find and Replace
	void onFindPatternResult(bool iResult);
	void onReplaceFindPatternResult(bool iResult);
	void onReplacePatternResult(bool iResult);
};

#endif
