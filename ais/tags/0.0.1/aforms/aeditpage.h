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
#ifndef AEDITPAGE_H
#define AEDITPAGE_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/aeditpage.h
															Edit Page

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/9/2006	tlw		Cursor Stack. Add properties to save enabled status of cursor stack.
1.0107	9/20/2006	tlw		Add keyPressEvent to catch F1 key presses.
1.0106	9/14/2006	tlw		Add setExtentName method
1.0100	6/9/2006	tlw		Add Doxygen documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "ais.h"				// AReqType
#include "atextedit.h"
class AAppClient;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief AEditPage is a TextEdit page with a few extra methods required for a page added the AEditorTab set of edit pages.

AEditPage inherits from ATextEdit.  It also keeps track of its associated extent name and the page type (default, local file,
remote file and agent).
*/

class AEditPage : public ATextEdit
{
	Q_OBJECT
public:
	enum APageType	{ meDefault, meLocalFile, meRemoteFile, meAgent};
	AEditPage(QWidget* ipParent, APageType iPageType, const QString& irExtentName, const QString& irFileSpec, ALanguageType iLanguage);

	QString		extentName() { return cExtentName;};
	APageType	pageType() { return cPageType;};
	void		save() { editWrite(objectName());};
	void		setPageType(APageType iType) { cPageType = iType;};
	void		setExtentName(const QString& irExtentName) { cExtentName = irExtentName;};

	bool		mForward;		// Cursor stack forward enabled state.
	bool		mPrevious;		// Cursor stack previous enabled state.

protected:
	virtual	void keyPressEvent(QKeyEvent* ipEvent);

private:
	QString		cExtentName;
	APageType	cPageType;
};

#endif		// AEDITPAGE_H

