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
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/aeditpage.cpp
														Edit Page

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/9/2006	tlw		CursorStack. Initialize public properties that hold enabled cursor status.
1.0107	9/20/2006	tlw		keyPressEvent. Add keyPressEvent to catch F1 key presses.
1.0100	6/9/2006	tlw		Add Doxygen documentation
1.0070	10/3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QKeyEvent>
#include <QtGui/QMessageBox>
#include "appclient.h"
#include "aeditpage.h"
#include "asaveasagentdialog.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
\brief AEditPage - constructor instantiates an instance of ATextEdit with a few extra methods
\par Args:
\param ipParent -> Parent widget that created this instance of an edit page
\param iPageType - Type of page to be edited (local file, remote file, agent).
\param irExtentName - Name of the extent(cabinet) associated with this page
\param irFileSpec - Name of the file or agent to be edited
\param iLanguage - Index into table of languages (C, Lisp, JavaScript, Html, etc.)
\return void
 */
AEditPage::AEditPage(QWidget* ipParent, APageType iPageType, const QString& irExtentName, const QString& irFileSpec, ALanguageType iLanguage)
:	ATextEdit(ipParent, irFileSpec, iLanguage), cExtentName(irExtentName), cPageType(iPageType)
{
	// FileSpec. Save file specification for later save.
	setObjectName(irFileSpec);

	// Cursor stack. Start off with both disabled until first large move.
	mForward = mPrevious = false;
}

/*	---------------------------------------------------------------------------------------------------------------------------
keyPressEvent - Reimplement in order to catch Help key presses
Args:
	ipEvent	Ptr to the event emitted by main event loop
Returns:
	nothing
Notes:
 1.	F1 may be caught by ADebugPage, AEditPage or AMainWindow. First one to catch it wins.
	------------------------------------------------------------------------------------------------------------------------ */
void AEditPage::keyPressEvent(QKeyEvent* ipEvent)
{
	if (ipEvent->key()== Qt::Key_F1)
	{	ipEvent->accept();
		dialogHelp();
	}
}

// end
