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

#ifndef ATEXTCURSOR_H
#define ATEXTCURSOR_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/atextcursor.h
														Text Cursor

CHANGE HISTORY
Version	Date		Who		Change
2.0003	2/6/2007	tlw		decimalWidth. Add gutter on left side.
1.0118	12/12/2006	tlw		replace. Remove as it is no longer needed.
1.0116	11/27/2006	tlw		refresh. Expand tabs and adjust line wrapping.
1.0116	11/27/2006	tlw		insert,append. Pass in cursor to be adjusted if wrap.
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0110	10/10/2006	tlw		Mark. Only mark first added operation after a move.
1.0109	10/5/2006	tlw		dumpRow. Add dumpRow method. setCursor. Add IncludeNewline arg.
1.0108	9/30/2006	tlw		dumpUndo. Add new method.
1.0107	9/22/2006	tlw		Add mid to get a slice of text (for editWord).
1.0107	9/20/2006	tlw		Add third argument to setCursor for saving the cursor in the cursor stack.
1.0070	11/5/2005	tlw		Move ACursor to separate class
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QPoint>
#include "atextedit.h"
#include "atextdocument.h"
#include "aundo.h"
class QByteArray;
class QWidget;
class ATextEdit;
//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------


//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief ATextCursor - Implements cursor operations for the ATextEdit editor class

 */
class ATextCursor : public QObject
{
	Q_OBJECT
public:
	ATextCursor(QWidget* ipViewPort, AParameters* ipParameters);
	void		append(const QByteArray& irText, long iLgth, bool iMoveToEnd);
	void		clearUndo();
	long		count();
	QPoint		cursor(ATextEdit::ACursorType iType);
	long		deleteIt(long iLgth);
	long		deleteIt(const QPoint& irToCursor);
	QString		dumpRow(long iRow);
	QString		dumpUndo(long iPos);
	bool		find(const QString& irPattern, bool iAllText ,bool iMatchCase, bool iDown, bool iRegExp, bool iSelect
				, bool iMatchWord);
	QStringList	functionList();
	long		gutterWidth() { return cTextDocument.cGutterWidth; };
	bool		indent(long iCh, bool iIn);
	long		insert(ATextEdit::ACmdType iCmd, const QByteArray& irText, long iLgth);
	long		insert(char iCh);
	QString		mid(const QPoint& irBeg, const QPoint& irEnd);
	bool		move(ATextEdit::AMoveTo iMove, QPoint& iorCursor);
	void		paint(const QRect& irArea, const QPoint& irOrigin, bool iShow);
	void		rebalance();
	bool		redo(bool iToMark);
	bool		refresh();
	bool		replaceAll(const QString& irPattern, const QString& irReplace, bool iAllText, bool iMatchCase, bool iRegExp
				, bool iSelect, bool iMatchWord);
	long		selectedText(QByteArray& iorText, long iLgth);
	bool		selectMatchedText();
	bool		setCursor(const QPoint& irCursor, bool iExtendSelect, bool iIncludeNewline, bool iSaveCur);
	void		setViewSize(const QSize& irSize);
	bool		switchCursors();
	bool		tabify(long iExtraSpaces, bool iToTabs);
	long		text(long iRow, QByteArray& iorText, long iLgth);
	long		textLength();
	long		textRows();
	long		truncateTop(long iLeft);
	bool		undo(bool iToMark);

	friend class ATextEdit;		// Allow ATextEdit parent to access private variables

signals:
	void		rowCountChanged(long iCount);

private:
	long			decimalWidth(long iValue);
	long			deleteSelected(QByteArray& iorDeletedText, long iLgth);

	QPoint			cAnchor;				// Other end of matched or selected text
	QPoint			cCursor;				// Current X, Y position of cursor
	bool			cMark;					// True iff next undo operation should be marked due to a move.
	bool			cMatchedText;			// True iff text between matching brackets is highlighted
	AParameters*	cpParameters;			// -> All configurable parameters
	long			cRowCount;				// Number of rows in document.
	bool			cSelectedText;			// True iff text is selected between Anchor and Cursor
	ATextDocument	cTextDocument;			// Manages the rows of text
	AUndo			cUndo;					// Undo/redo list
	bool			cVertBarShow;			// True iff blinking cursor is visible
	QWidget*		cpViewPort;				// -> area of text that is being displayed
};
#endif		// ATEXTCURSOR_H
