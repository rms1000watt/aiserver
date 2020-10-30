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

#ifndef ATEXTDOCUMENT_H
#define ATEXTDOCUMENT_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/atextdocument.h
														Text Document

ATextDocument holds the color/text info for a TextEdit document.

NOTE:
Since ATextDocument has remote ownership of ATextRow referents, it requires special copy semantics (a destructor, a copy
constructor, assignment operator); however, since ATextDocument is never assigned, copied, or passed by value, the copy
constructor and assignment operator are omitted here.  See C++ FAQS chap 30 for more info.

CHANGE HISTORY
Version	Date		Who		Change
2.0003	2/6/2007	tlw		Add gutter on left side of view port.
1.0116	11/27/2006	tlw		refresh. Expand tabs if tabwidth changed.
1.0116	11/26/2006	tlw		insert, append. Add cursor position argument.
1.0110	10/13/2006	tlw		checkCurPos. Enable/disable cursor stack signal.
1.0109	10/5/2006	tlw		dumpRow. Add dumpRow method. setCursor. Add IncludeNewline arg.
1.0108	10/1/2006	tlw		Allow move to empty row on the end.
1.0107	 9/20/2006	tlw		Add a cursor stack.
1.0070	11/5/2005	tlw		Move ADoc to a separate class.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QList>
#include <QtCore/QStack>
#include "atextedit.h"
#include "atextrow.h"
class QPainter;
class QPoint;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
typedef QList<ATextRow*> ADocument;
typedef QList<QPoint> ACurStack;

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief ATextDocument - Insert, delete, search operations on a text document.

\par Notes:
-# Typically called by ATextCursor methods to perform any operation on the text.
 */
class ATextDocument : public QObject
{
	Q_OBJECT
public:
	ATextDocument(AParameters* ipParameters);
	~ATextDocument();
	bool		addCursor(const QPoint& irCursor);
	long		append(QPoint& iorOrigin, const QByteArray& irText, long iSrcLgth);
	long		at(long iX, long iY);
	long		deleteIt(const QPoint& irFrom, const QPoint& irTo, QByteArray& iorDeletedText, long iLgth);
	QString		dumpRow(long iRow);
	long		eol(long iY, bool iIncludeNewline = false);
	QPoint		eot();
	bool		find(const QString& irPattern, bool iWrap, bool iMatchCase, bool iDown, bool iRegExp, bool iMatchWord,
				QPoint& iorStart, QPoint& iorFinish);
	QStringList	functionList();
	long		indent(long iBegY, long iCh, long iEndY, bool iIn);
	long		insert(QPoint& iorOrigin, QByteArray& iorText, long iLgth);
	bool		matchParen(const QPoint& irPos, QPoint& orAnchor);
	bool		move(ATextEdit::AMoveTo iMove, QPoint& iorPos);
	void		paint(const QRect& irArea, const QPoint& irOrigin, QPainter& irPainter);
	void		rebalance(QPoint& iorOrigin, long iEndY);
	QPoint		scan(long iLength, QPoint& iorStart);
	QPoint		setCursor(const QPoint& irNewPos, bool iIncludeNewline);
	void		setSelected(const QPoint& irAnchor, const QPoint& irCursor, bool iMatchedText, bool iSelectedText);
	void		tabify(long iBegRow, long iCount, long iExtraSpaces, bool iToTabs);
	long		text(const QPoint& irBeg, const QPoint& irEnd, QByteArray& iorText, long iLgth);
	long		textLength();
	long		truncateTop(long iLeft);

	friend class ATextCursor;	// Allow ATextCursor access to cpParameters.
	friend class ATextRow;		// Allow ATextRow access to cpParameters and cGutterWidth.

signals:
	void		cursorStackChanged(bool iForward, bool iPrevious);

private:
	void		checkCurPos(bool iPush);
	long		decimalWidth(long iValue);
	bool		findRegExp(QString& irPattern, bool iWrap, bool iMatchCase, bool iDown,  bool iMatchWord, QPoint& iorStart
				, QPoint& iorFinish);
	bool		isLess(const QPoint& irLhs, const QPoint& irRhs);
	bool		refresh();
	void		setForeground(long iBegY, long iEndY);

	long		cCurPos;		// Current position in CurStack
	ACurStack	cCurStack;		// Stack of cursor positions
	long		cGutterWidth;	// Width of gutter on left side of viewport.
	long		cLastX;			// Largest offset in a sequence of vertical moves.
	AParameters* cpParameters;	// Configurable parameters
	long		cViewHeight;	// Number of displayable rows in viewport.
	long		cViewWidth;		// Number of displayable chars in a row.

	ADocument	cDocument;		// List of rows
};
#endif		// ATEXTDOCUMENT_H
// end
