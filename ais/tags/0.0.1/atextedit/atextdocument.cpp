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
aisdev/atextedit/atextdocument.cpp
														Text Document

ATextDocument holds the color/text info for a TextEdit document.

CHANGE HISTORY
Version	Date		Who		Change
3.2007	3/26/2008	fchua	find. Fixed bugs in handling partial matches.
2.0005	5/1/2007	tlw		rebalance. Only adjust cursor to next row on first time through.
2.0004	2/14/2007	tlw		move. Move up/down even if less than one page from top/bottom.
2.0003	2/6/2007	tlw		rebalance.  Deduct gutter width from line width.
1.0116	11/27/2006	tlw		refresh. Expand tabs and adjust line wrapping.
1.0116	11/26/2006	tlw		move. If at end of wrapped line, move past first char in next row. indent. Add // if aCh is /.
1.0116	11/26/2006	tlw		insert, append. Update cursor position which may change due to word wrap.
1.0113	11/9/2006	tlw		move. Allow move right to move to end-of-line.
1.0113	11/7/2006	tlw		Destructor. Note on referents.
1.0110	10/13/2006	tlw		CursorStack. Enable/disable cursor stack signal.
1.0109	10/5/2006	tlw		dumpRow. Add dumpRow method. setCursor. Add IncludeNewline arg.
1.0108	10/1/2006	tlw		Allow move to empty row on the end.
1.0107	9/20/2006	tlw		addCursor. Add cursor to the cursor stack. eol, eot. Return 0 if text document is empty. Add ePrevSow
1.0105	9/13/2006	tlw		Scan. do not scan past the last line.
1.0070	08/3/2006	mfk		ATextDocument. Do not add newline at the beginning of every text file loaded into the editor.
1.0070	11/5/2005	tlw		Move ADoc to a separate class.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QPainter>
#include "atextdocument.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief ATextDocument - Constructor initializes the ATextDocument properties

\param ipParameters - A structure containing the configurable parameters for this text document
\return void
 */
ATextDocument::ATextDocument(AParameters* ipParameters)
: 	cLastX(-1), cpParameters(ipParameters)
{
	// Init. Start with a single empty row
	cGutterWidth = cViewHeight = cViewWidth = 0;
	ATextRow* apRow = new ATextRow(this);
	cDocument.insert(0, apRow);

	// Cursor Stack. Initialize the cursor stack to beginning of text.
	cCurPos = 0;
	cCurStack.append(QPoint(0, 0));
}

/*!
\brief Destructor Free allocated resources

\par Notes:
-# ATextDocument has remote ownership of ATextRow referents in cDocument. 
-# Since instances of ATextDocument are never assigned, copied or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
ATextDocument::~ATextDocument()
{
	// Delete any rows that may be left lying around
	long aRowSz = cDocument.count();
	for (long aRow = 0; aRow < aRowSz; ++aRow)
		delete cDocument[aRow];

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~ATextDocument()");
#endif
}

/*!
\brief addCursor - Add a new cursor to the cursor stack

The last saved cursor position is at cCurPos in the cCurStack. irCursor is the new cursor position. A page is the number
of rows in one viewport. A large move is a move of irCursor that is at least a page from the last saved cursor position.
The saved cursor position at cCurPos is updated on all small moves.  On large moves, all the positions on the stack
after the current cursor positions are removed, cCurPos is incremented and the new cursor position is appended to the
stack.

After one or more large moves have been made, move(ePrevCur) moves the cursor to cCurStack[--cCurCursor] which moves
the cursor to the previously saved cursor position.  After one or more move(ePrevCur) moves have been made, move(eNextCur)
moves the cursor to cCurStack[++cCurCursor] which restores the old cursor position.

\param irCursor The new cursor position
\return void
\par Note
-# A whole bunch of small moves in the same direction do not add up to one large move.  
-# If the user engages in a lot of large moves , the cursor stack can grow very large.
 */
bool ATextDocument::addCursor(const QPoint& irCursor)
{
	bool aOk = false;
	long aNewY = irCursor.y(), aY = cCurStack[cCurPos].y();
	if (aNewY >= aY + cViewHeight || aNewY <= aY - cViewHeight)
	{	// Remove stack to right of current position
		long aLast = cCurStack.count();
		for (long aPos = ++cCurPos; aPos < aLast; ++aPos)
			cCurStack.removeLast();
		// Append. Tack the new position onto the end
		cCurStack.append(irCursor);

		// Enable. Update move controls only if moving off beginning or hit end for first time.
		if (cCurPos == 1 || cCurPos <= aLast)
			checkCurPos(true/*Push*/);
		aOk = true;
	}
	else
		cCurStack.replace(cCurPos, irCursor);
	return aOk;
}

/*!
append - append text to the document

append may append text to the end of the document or to the end of a row.
\param iorOrigin Text position of append.   If invalid, append to the last row.
\param irText	 Text to be appended
\param iSrcLgth Number of chars in irText
\return Number of rows in the text.
\par Notes:
-# The new cursor position is returned in iorOrigin.  The cursor position may change on wrap at the end of a line.
-# If the current row is newline-terminated row, the text is appended to a new row.
-# If irText contains embedded newlines multiple rows are added.
-# After the text is appended, rebalance is called to adjust the padding/length of this row and subsequent rows.
-# The last row of the document may be empty.
-# If the text appended to the end of the document is newline-terminated, an empty row is also appended.
-# Large text should be split into MaxRowLgth chunks before calling this routine.
 */
long ATextDocument::append(QPoint& iorOrigin, const QByteArray& irText, long iSrcLgth)
{
	// At End. If iY < 0, append to the end.
	long aY = iorOrigin.y();			// Index of first row being appended
	long aLastY = cDocument.count() - 1;	// Index of last row of text
	if (aY < 0)
		aY = aLastY;

	// Current Row. Current row never ends with a newline.
	bool aAtEnd = (aY == aLastY);// True iff appending to end of document
	ATextRow* apRow(cDocument[aY]);	// -> to row to have text appended
	if (apRow->mHasNewline)
	{	qDebug("ATextDocument::append(Y:%ld,Text:%s,SrcLgth:%ld),Newline terminated.", aY, irText.data(), iSrcLgth);
		apRow = new ATextRow(this);
		cDocument.insert(++aY, apRow); 
	}
	iorOrigin.setX(apRow->mRowLgth);
	iorOrigin.setY(aY);

	// Append. Add row for each line
	long aLgth;						// Length of line to be appended
	long aPos, aX = 0;				// Position of next newline, current offset into irText
	for (;;)
	{	// Append. Append next newline-terminated row to document at aY
		if ((aPos = irText.indexOf('\n', aX)) >= 0)
			aLgth = aPos + 1 - aX;
		else
			aLgth = iSrcLgth - aX;
		apRow->append(irText, aX, aLgth); 

		// Add row. If not at end, add empty row to hold next line.
		if ((aX += aLgth) >= iSrcLgth)
			break;
		else
		{	apRow = new ATextRow(this);
			cDocument.insert(++aY, apRow);
		}
	}
	// Add Row. If appending to eot and row is newline-terminated, add empty row to end of document.
	if (aAtEnd && apRow->mHasNewline)
	{	apRow = new ATextRow(this);
		cDocument.insert(++aY, apRow);
	}
	rebalance(iorOrigin, aY);
	return cDocument.count();
}

/*!
\brief at - fetch character at the specified position

\param iX The character position (counting from zero on the left).
\param iY The row position (counting from 0 at the top)
\return The character at the specified position in the text.
\par Notes:
-# The values of iX and iY must be within the current text range.
 */
long ATextDocument::at(long iX, long iY)
{
	ATextRow* apRow = cDocument[iY];
	const ushort* apCw = apRow->data();
	return *(apCw + iX) & 0xff;
}

/*
checkCurPos - Note when cursor stack hits either end.

Only emit a signal if a status has changed
Args:
	iPush - True iff advancing CurPos
Returns void
Notes:
  1. If advancing, status changes only if new position is moving off beginning or at end.
  2. If retreating, status changes only if new position is at 0 or moving off the end.
 */
void ATextDocument::checkCurPos(bool iPush)
{
	// Update. Note new status as noted above.
	bool aForward, aPrevious;			// New enable/disable status of the arrows.
	long aLast = cCurStack.count() -1;	// Position of last item in the cursor stack
	if ((iPush && (cCurPos == 1 || cCurPos == aLast)) || 
        (!iPush && (cCurPos == 0 || cCurPos == aLast - 1)))
	{	aForward = (cCurPos < aLast), aPrevious = (cCurPos > 0);
		emit cursorStackChanged(aForward, aPrevious);
	}
}

/*
decimalWidth - Calculate the number of digits in an integer

Args:
	iValue - number to be converted
Returns aWidth - Number of decimal places in iValue
Notes:
  1. Ignores negative numbers
 */
long ATextDocument::decimalWidth(long iValue)
{
	long aWidth = 0;	// Number of decimal places in iValue
	while (iValue > 0)
	{	iValue /= 10;
		++aWidth;
	}
	return aWidth;
}

/*!
\brief deleteIt - Delete an area of text starting at irFrom and extending up to irTo.

\param irFrom - Starting X,Y position.
\param irTo - Ending X, Y position.
\param iorDeletedText - The deleted characters are appended to iorDeletedText
\param iLgth - The initial number of bytes in iorDeletedText
\return  iLgth - The number of bytes in iorDeletedText on return
 */
long ATextDocument::deleteIt(const QPoint& irFrom, const QPoint& irTo, QByteArray& iorDeletedText, long iLgth)
{
	// Swap. Swap From and To if From < To
	long aBegX, aBegY, aEndX, aEndY;
	if (irFrom != irTo)
	{	if (isLess(irFrom, irTo))
		{	aBegX = irFrom.x();
			aBegY = irFrom.y();
			aEndX = irTo.x();
			aEndY = irTo.y();
		}
		else
		{	aBegX = irTo.x();
			aBegY = irTo.y();
			aEndX = irFrom.x();
			aEndY = irFrom.y();
		}
		// Delete.  Delete a section of a row or the entire row. When deleting a row, the remaining rows move up.
		long aX, aY = aBegY;
		long aLgth;
		ATextRow* apRow;
		while (aY <= aEndY)
		{	apRow = cDocument[aY];
			aLgth = apRow->mRowLgth;
			aX = (aY == aEndY) ? aEndX : aLgth;
			// Entire Row.  If delete spans a row that is not the last row, remove the row.
			if (aBegX == 0 && aX >= aLgth && aY < cDocument.count() - 1)
			{	iLgth = apRow->text(iorDeletedText, iLgth);
				cDocument.removeAt(aY);
				delete apRow;
				apRow = NULL;
				--aEndY;
			}
			else
			{	if (aX > aBegX)
					iLgth = apRow->deleteIt(aBegX, aX - aBegX, iorDeletedText, iLgth);
				++aY;
			}
			aBegX = 0;
		}
		// End. Move up if deleted beginning line.
		if (aBegY > 0 && aBegY > aEndY)
			--aBegY;
		QPoint aStart(0, aBegY);
		rebalance(aStart, aBegY);
	}
	return iLgth;
}

/*!
\brief dumpRow - Dump info for specified ATextRow

\return  (BegState,HasNewline,Lgth) text :Fg,Bg,Hex ...
\sa ATextRow
 */
QString ATextDocument::dumpRow(long iRow)
{
	ATextRow* apRow = cDocument[iRow];
	return apRow->dumpRow();
}

/*!
\brief eol - Fetch the number of characters in the specified row

\param iY - The index into the array of rows.
\param iIncludeNewline - Iff true, include the trailing newline, if any, in the count.
\return aEol - offset just past last char in row.  If iIncludeNewLine is false and row has newline, offset to newline.
\par Notes:
-# iY must be within the range of rows in the text
 */
long ATextDocument::eol(long iY, bool iIncludeNewline)
{
	long aEol = 0;
	if (cDocument.isEmpty())
		qDebug("ATextDocument::eol(Y:%ld, IncludeNewline:%d), Document is empty.", iY, iIncludeNewline);
	else if (iY < 0 || iY >= cDocument.count())
		qDebug("ATextDocument::eol(Y:%ld, IncludeNewline:%d), Line out of range.", iY, iIncludeNewline);
	else
	{	ATextRow* apRow = cDocument[iY];
		aEol = apRow->mRowLgth;
		if (!iIncludeNewline && aEol > 0 && apRow->mHasNewline)
			--aEol;
	}
	return aEol;
}

/*!
\brief eot - Fetch the column and row just past the last char in the text

\return aEol - Length of the row in chars.
\par Notes:
-# The document always contains at least one row (which may be empty).
-# The last row may be empty and it never contains a newline.
 */
QPoint ATextDocument::eot()
{
	long aY = cDocument.count() - 1;		// Last row
	QPoint aEot(0, 0);
	if (cDocument.isEmpty())
		qDebug("ATextDocument::eot(), Document is empty");
	else if (cDocument[aY]->mHasNewline)
		qDebug("ATextDocument::eot(), Last row at %ld has a newline", aY);
	else
	{	aEot.setX(eol(aY, false/*IncludeNewline*/));
		aEot.setY(aY);
	}
	return aEot;
}

/*!
\brief find - Search the text for a section that matches a pattern

\param irPattern - String or pattern to be matched
\param iWrap - Continue search if at top or bottom before hitting Finish.
\param iMatchCase - Case sensitive search iff iMatchCase is true.
\param iDown - If iDown, search from iorStart down/right; else, search from iorStart up/left.
\param iRegExp - Treat irPattern as a regular expression iff true.
\param iMatchWord - Matched text must start and end on a word boundry iff true.
\param iorStart - Starting row,col position measured from top-left corner (0,0).
\param iorFinish - Ending row, col position measured from top-left corner (0,0).
\par Notes:
-# Even if iRegExp is false, irPattern may include \\t, \\n, \\b, \\f, etc. which match the tab, newline, bell, and formfeed.
-# If iDown iorStart is above iorFinish and vice versa both on invocation and on return.
-# Because the text is broken into lines and because tabs are variable length, the search cannot take advantage of several speed up
methods such as the Boyer-Moore algorithm.
 */
bool ATextDocument::find(const QString& irPattern, bool iWrap, bool iMatchCase, bool iDown, bool iRegExp, bool iMatchWord,
	 QPoint& iorStart, QPoint& iorFinish)
{
	// RegExp. Deal with regular expressions separately
	QString aPattern(irPattern);
	if (iRegExp)
		return findRegExp(aPattern, iWrap, iMatchCase, iDown,  iMatchWord, iorStart, iorFinish);

	// Escape. Replace special characters with their values
	aPattern.replace(QString("\\n"), QString("\n"));
	aPattern.replace(QString("\\t"), QString("\t"));
	aPattern.replace(QString("\\b"), QString("\b"));
	aPattern.replace(QString("\\r"), QString("\r"));
	aPattern.replace(QString("\\f"), QString("\f"));

	// Convert. Convert the pattern from a QString to QByteArray.
	long aP = 0, aPtnLgth = aPattern.length();
	QByteArray aPatn(iMatchCase ? aPattern.toLatin1() : aPattern.toLower().toLatin1());

	// Search. Search each row. If row returns a tail that is not yet matched, include it in the next row.
	bool aFoundIt = false;
	long aX = iorStart.x(), aX1 = -1, aY = iorStart.y(), aY1 = -1, aEndY = cDocument.count() - 1;
	long aEndX = 0, aLastY = iorFinish.y();
	ATextRow* apRow;
	if (iDown)
	{	for (;;)
		{	// Match. Return starting and ending cursor positions if not past end of search area.
			apRow = cDocument[aY];
			if ((aEndX = apRow->find(aPatn, aP, iMatchCase, iDown, iMatchWord, aX)) >= 0)
			{	
				iorFinish = QPoint(aEndX, aY);

				// In case of partial match, use stored coordinates
				if (aP > 0)
				{
					aX = aX1;
					aY = aY1;
				}

				iorStart = QPoint(aX, aY);
				
				aFoundIt = true;
				break;
			}
			// Partial Match. Try matching the rest of the pattern with the beginning of the next row.
			else if (-aEndX < aPtnLgth)
			{
				if (aP == 0)
				{
					// First partial match
					// store the x and y coordinates
					aX1 = aX;
					aY1 = aY;
				}
				else if (aP >= -aEndX)
				{
					// New partial match is found (Remaining characters are greater than expected)
					// store the x and y coordinates
					aX1 = aX;
					aY1 = aY;
				}

				// Set number of matched characters
				aP = -aEndX;
			}
			else
			{
				// No match, reset temporary values
				aP = 0;
				aX1 = -1;
				aY1 = -1;
			}

			// Move to next row to continue matching
			aX = 0;
			++aY;
			if (iWrap)
			{	
				if (aY > aEndY)
				{	
					aY = 0;			// Wrap around to the top
					// reset partial matches
					aP = 0;
					aX1 = -1;
					aY1 = -1;
					iWrap = false;
				}
			}
			else if (aY > aEndY || (aY > aLastY && aP == 0)) // Make sure partial matches are continued
				break;
		}
	}
	else	// Up. Search up from Start to Finish. If a match, return bottom of match in Start and top of match in Finish.
	{
		apRow = cDocument[aY];
		for (;;)
		{
			// Match. Return starting and ending cursor positions if not past end of search area.
			if ((aEndX = apRow->find(aPatn, aP, iMatchCase, iDown, iMatchWord, aX)) >= 0)
			{	
				iorFinish = QPoint(aEndX, aY);

				// In case of partial match, use stored coordinates
				if (aP > 0)
				{
					aX = aX1;
					aY = aY1;
				}

				iorStart = QPoint(aX, aY);

				aFoundIt = true;
				break;
			}
			// Partial Match. Try matching the rest of the pattern with the beginning of the next row.
			else if (-aEndX < aPtnLgth)
			{
				if (aP == 0)
				{
					// First partial match
					// store the x and y coordinates
					aX1 = aX;
					aY1 = aY;
				}
				else if (aP >= -aEndX)
				{
					// New partial match is found (Remaining characters are greater than expected)
					// store the x and y coordinates
					aX1 = aX;
					aY1 = aY;
				}

				// Set number of matched characters
				aP = -aEndX;
			}
			else
			{
				// No match, reset temporary values
				aP = 0;
				aX1 = -1;
				aY1 = -1;
			}				

			// Wrap. Wrap around from top to the bottom if at end.
			--aY;
			if (iWrap)
			{	if (aY < 0)
				{	aY = aEndY;			// Wrap around to the bottom
					// reset partial matches
					aP = 0;
					aX1 = -1;
					aY1 = -1;
					iWrap = false;
				}
			}
			else if (aY < 0 || (aY < aLastY && aP == 0))
				break;
			
			apRow = cDocument[aY];
			aX = apRow->mRowLgth;
		}
	}
	return aFoundIt;
}

/*!
\brief findRegExp - Search the text for a section that matches a pattern

\param irPattern - String or pattern to be matched
\param iWrap - Contine search if at top or bottom before hitting Finish.
\param iMatchCase - Case sensitive search iff iMatchCase is true.
\param iDown - Search from from cursor down and to the right if iDown is true; else, search from cursor up and to the left.
\param iMatchWord - Matched text must start and end on a word boundry iff true.
\param iorStart - Starting row,col position measured from top-left corner (0,0).
\param iorFinish - Ending row, col position measured from top-left corner (0,0).
\par Notes:
-# Search is made one row at a time.
-# Uses the Qt regular expression rules and syntax.
 */
bool ATextDocument::findRegExp(QString& irPattern, bool iWrap, bool iMatchCase, bool iDown,  bool iMatchWord, QPoint& iorStart,
	 QPoint& iorFinish)
{
	// Words. Surround pattern with \b if matching on word boundaries
	bool aMatch = false;
	if (iMatchWord)
	{	irPattern.prepend("\\b");
		irPattern.append("\\b");
	}
	// RegExp. Initialize regular expression
	Qt::CaseSensitivity aCase = iMatchCase ? Qt::CaseSensitive : Qt::CaseInsensitive;
	QRegExp aRegExp(irPattern, aCase);

	// Down. Search text by row from iorStart to iorFinish
	long aBegY = iorStart.y(), aY = aBegY, aLastY = iorFinish.y(), aEndY = cDocument.count() - 1;
	long aBegX = iorStart.x(), aEndX, aLastX = iorFinish.x(), aMatched, aX;
	QByteArray aRow;
	ATextRow* apRow;
	if (iDown)
	{	for (;;)
		{	apRow = cDocument[aY];
			apRow->text(aRow, 0);
			if ((aX = aRegExp.indexIn(QString(aRow), aBegX)) >= 0)
			{	// Match. Return Start and Finish position.
				aMatched = aRegExp.matchedLength();		// Number of chars matched (tabs count as one char)
				aEndX = apRow->scan(aX, &aMatched);
				if (!(aY == aLastY && aEndX > aLastX))
				{	iorStart = QPoint(aX, aY);
					iorFinish = QPoint(aEndX, aY);
					aMatch = true;
					break;
				}
			}
			// Next. Move to next row.
			aBegX = 0;
			if (iWrap)
			{	if (++aY > aEndY)
				{	aY = 0;			// Wrap around to the top
					iWrap = false;
				}
			}
			else if (++aY > aLastY)
				break;
		}
	}
	else // Up. Search text from iorStart back to iorFinish
	{	apRow = cDocument[aY];
		for (;;)
		{	apRow->text(aRow, 0);
			if ((aX = aRegExp.lastIndexIn(QString(aRow), aBegX)) >= 0)
			{	// Match. Return Start (bottom most) and Finish (topmost) position.
				aEndX = aX + aRegExp.matchedLength();
				if (!(aY == aLastY && (aX < aLastX || aEndX > aBegX)))
				{	iorStart = QPoint(aX, aY);
					iorFinish = QPoint(aEndX, aY);
					aMatch = true;
					break;
				}
			}
			// Prev. Move back to previous row.
			if (iWrap)
			{	if (--aY < 0)
				{	aY = aEndY;			// Wrap around to the bottom
					iWrap = false;
				}
			}
			else if (--aY < aLastY)
				break;
			apRow = cDocument[aY];
			aBegX = apRow->mRowLgth;
		}
	}
	return aMatch;
}

/*!
\brief functionList - Return a list of pairs of strings noting the line number and function name of all the
function headers in the text.

\return A list of pairs of strings
\par Notes:
-# The line number is returned as a string of length 5 padded with leading spaces in order to facilitate string sorting.
-# A so-called function header is language dependent.  The detection of a header is pretty good, but it is not perfect.
 */
QStringList ATextDocument::functionList()
{
	long aCount = cDocument.count();
	QStringList aFcnList;
	QString aFcnName, aRowX;
	ATextRow* apRow;
	for (long aY = 0; aY < aCount; ++aY)
	{	apRow = cDocument[aY];
		if (apRow->functionList(aFcnName))
		{	aFcnList += aRowX.sprintf("%5ld", aY);
			aFcnList += aFcnName;
		}
	}
	return aFcnList;
}

/*!
\brief indent - Indent/outdent the selected lines by one char

\param iBegY - Starting row of search
\param iCh - The indent character (usually a tab)
\param iEndY - Final row of the search
\param iIn - Indent if true; else, outdent
\par Notes:
-# If outdenting and the leading char in a row is not iCh, the row is not modified.
-# iEndY must be greater than or equal to iBegY
 */
long ATextDocument::indent(long iBegY, long iCh, long iEndY, bool iIn)
{
	// Scan. Scan back until hit start of a line.
	while (iBegY > 0 && !cDocument[iBegY - 1]->mHasNewline)
		--iBegY;
	// Insert/delete delimiter at beginning of each line.
	bool aAtBol = true;				// At the start of a line
	ATextRow* apRow;
	for (long aY = iBegY; aY <= iEndY; ++aY)
	{	apRow = cDocument[aY];
		if (aAtBol)
		{	if (iIn)
			{	apRow->prepend(iCh);
				if (iCh == '/')
					apRow->prepend(iCh);
			}
			else
			{	apRow->deleteIt(iCh);
				if (iCh == '/')
					apRow->deleteIt(iCh);
			}
		}
		aAtBol = apRow->mHasNewline;
	}
	QPoint aStart(0, iBegY);
	rebalance(aStart, iEndY);
	return iBegY;
}

/*!
\brief insert - Insert text starting at irOrigin

\param iorOrigin - Row, column where insert begins
\param iorText - Text to be inserted. Note that the tail of the current row is appended to iorText
\param iLgth - Number of bytes to be inserted.
\return Number of rows in text document after the insertion is complete.
\par Notes:
 -# Chop off the tail of the existing row and append it to Text. Then append the extended row to existing row.
 -# The modified cursor position is returned in iorOrigin. It may change if a wrap occurs.
  */
long ATextDocument::insert(QPoint& iorOrigin, QByteArray& iorText, long iLgth)
{
	long aY = iorOrigin.y(), aX = iorOrigin.x();
	if (cDocument.isEmpty())
		qDebug("ATextDocument::insert(Text:%s), Document is empty.", iorText.data());

	if (!iorText.isEmpty())
	{	// Chop. Chop off the tail of the current row and append it to iorText
		ATextRow* apRow = cDocument[aY];
		if (aX < apRow->mRowLgth)
			iLgth = apRow->chop(aX, iorText, iLgth);

		// Append. Append the incoming text + the tail to row at aY.
		append(iorOrigin, iorText, iLgth);
	}
	return cDocument.count();
}

// isLess - returns true iff irLhs < irRhs
bool ATextDocument::isLess(const QPoint& irLhs, const QPoint& irRhs)
{
	long aLhsY = irLhs.y();
	long aRhsY = irRhs.y();
	return aLhsY < aRhsY || (aLhsY == aRhsY && irLhs.x() < irRhs.x());
}

/*!
\brief matchParen - Set the background color of the text to light grey between matching brackets

\param irPos - Starting row, column position of the scan for a match.
\param orAnchor - New anchor position at the head of the matched text.
\return aMatched - True iff a match was found.
 */
bool ATextDocument::matchParen(const QPoint& irPos, QPoint& orAnchor)
{
	long aCh, aCw, aChClose, aChOpen;
	long aEndY;				// Number of last row in document
	bool aFwd;				// True if scan forward; else false
	bool aMatched = false;	// True iff anchor is modified
	long aParenX = 0;		// Offset into array of bracket chars
	long aRefCount = 0;
	long aRet;
	long aX = irPos.x();
	long aY = irPos.y();
	if (cDocument.isEmpty())
	{	qDebug("ATextDocument::matchParen(), Document is empty");
		return aMatched;
	}
	ATextRow* apRow = cDocument[aY];
	ushort* apX = apRow->data() + aX;

	// Forward. Scan forward if aCh matches an opening bracket and not in a comment or string.
	aCh = (aCw = *apX) & ATextEdit::scMaskChar;
	if ((aParenX = ATextEdit::scBracket[aCh]) > 0 && (aCw & ATextEdit::scMaskPen) < ATextEdit::scPenComment)
	{	aFwd = true;
		aChOpen = ATextEdit::scpParenOpen[aParenX];
		aChClose = ATextEdit::scpParenClose[aParenX];
		aEndY = cDocument.count() - 1;
		for (;;)
		{	if ((aRet = apRow->matchParen(aChClose, aChOpen, aFwd, aRefCount, aX)) >= 0)
			{	orAnchor.setX(aRet);
				orAnchor.setY(aY);
				aMatched = true;
				break;
			}
			else if (aY < aEndY)
			{	aRefCount = -aRet;
				apRow = cDocument[++aY];
				aX = 0;
			}
			else
				break;
		}
	}
	else if (aX > 0)
	{	// Back. Scan backward if prev aCh matches a closing bracket and not in a comment string.
		aCh = (aCw = *(apX - 1)) & ATextEdit::scMaskChar;
		if ((aParenX = ATextEdit::scBracket[aCh]) < 0 && (aCw & ATextEdit::scMaskPen) < ATextEdit::scPenComment)
		{ 	aFwd = false;
			aChOpen = ATextEdit::scpParenOpen[-aParenX];
			aChClose = ATextEdit::scpParenClose[-aParenX];
			aEndY = cDocument.count() - 1;
			for (;;)
			{	if ((aRet = apRow->matchParen(aChClose, aChOpen, aFwd, aRefCount, aX)) >= 0)
				{	orAnchor.setX(aRet);
					orAnchor.setY(aY);
					aMatched = true;
					break;
				}
				else if (aY > 0)
				{	aRefCount = -aRet;
					apRow = cDocument[--aY];
					aX = apRow->mRowLgth;
				}
				else
					break;
			}
		}
	}
	return aMatched;
}

/*!
\brief move - Move iorPos to the position specifed by iMove.

\param iMove - Specifies distance to new position (usually a move relative to the starting position)
\param iorPos - On entry, the starting position of the move; on exit, the new text position.
\return aMoved - True iff the move succeeds.
 */
bool ATextDocument::move(ATextEdit::AMoveTo iMove, QPoint& iorPos)
{
	bool aMoved = false;			// True iff move succeeds.
	bool aVertMove = false;			// True iff move straight up or down
	long	aLast;						// Index into last item on cCurStack.
	long aTabWidth = cpParameters->mTabWidth; // Number of chars between tab stops.
	long aX = iorPos.x();			// Column index of char at new position
	long aY = iorPos.y();			// Row offset of char at new position
	long aEol = eol(aY);				// Offset to newline or past last char if no newline
	long aEoY = cDocument.count()- 1;// Row offset of last line
	long aEoX = eol(aEoY);			// Offset just past the last char on the last line
	if (cDocument.isEmpty())
	{	qDebug("ATextDocument::move(), Document is empty()");
		iorPos = QPoint(-1, -1);
		return aMoved;
	}
	switch (iMove)
	{
	case ATextEdit::eBol:			// Move to beginning-of-line.
		if (aX > 0)
		{	aX = 0;
			aMoved = true;
		}
		break;
	case ATextEdit::eBot:			// Move to beginning-of-text
		if (aX > 0 || aY > 0)
		{	aX = aY = 0;
			aMoved = true;
		}
		break;
	case ATextEdit::eEol:			// Move to newline or past last char on the line if wrapped.
		if (aX < aEol)
		{	aX = aEol;
			aMoved = true;
		}
		break;
	case ATextEdit::eEot:			// Move to newline or the null at end of the last row
		if (aX != aEoX || aY != aEoY)
		{	aX = aEoX;
			aY = aEoY;
			aMoved = true;
		}
		break;
	case ATextEdit::eNextBol:		// If at end of wrapped line, move to the beg of next line
		if (aX >= aEol && aY < aEoY)
		{	ATextRow* apRow = cDocument[aY];
			if (!apRow->mHasNewline)
			{	aX = 0, ++aY;
				aMoved = true;
			}
		}
		break;
	case ATextEdit::eNextBow:		// Move to the beginning of the next word.
		
		aX = iorPos.x(), aY = iorPos.y();
		for (; aY <= aEoY; ++aY)
		{	if ((aX = cDocument[aY]->scanBow(true/*Fwd*/, false/*SameWord*/, aX)) < 0)
				aX = 0;
			else
			{	aMoved = true;
				break;
			}
		}
		break;
	case ATextEdit::eNextChar:		// Move to the next character in the text.
		if (aX < aEol)
		{	while (aX < aEol && at(aX, aY) == ATextEdit::scLatin1Space)
				++aX;
			aX += (at(aX, aY) == '\t') ? aTabWidth - aX % aTabWidth : 1;
			if (aX > aEol)
				aX = aEol;
			aMoved = true;
		}
		// Eol. If at eol but not eot, move to beginning of next row.
		else if (aY < aEoY)
		{	aX = 0, ++aY;
			aMoved = true;
		}
		break;
	case ATextEdit::eNextCur:		// Move to next position in cursor stack
		// See addCursor for more details.
		aLast = cCurStack.count() - 1;
		if (cCurPos < aLast)
		{	QPoint aCursor = cCurStack[++cCurPos];
			aX = aCursor.x();
			aY = aCursor.y();
			aMoved = true;
			checkCurPos(true/*Push*/);
		}
		break;
	case ATextEdit::eNextEow:		// Move to end of next word
		for (; aY <= aEoY; ++aY)
		{	if ((aX = cDocument[aY]->scanEow(true/*Fwd*/, aX)) < 0)
				aX = 0;
			else
			{	aMoved = true;
				break;
			}
		}
		break;
	case ATextEdit::eNextLine:		// Move to same offset on the next line
		if (aY < aEoY)
		{	ATextRow* apRow = cDocument[++aY];
			if (aX < cLastX)
				aX = cLastX;
			else
				cLastX = aX;
			aX = apRow->scanBegTab(aX, false/*IncludeNewline*/);
			aMoved = true;
			aVertMove = true;
		}
		break;
	case ATextEdit::eNextHalfPage:	// Move down by half the height of the viewport
		if (aY + cViewHeight / 2 <= aEoY)
		{	ATextRow* apRow = cDocument[aY += cViewHeight / 2];
			if (aX < cLastX)
				aX = cLastX;
			else
				cLastX = aX;
			aX = apRow->scanBegTab(aX, false/*IncludeNewline*/);
			aMoved = true;
			aVertMove = true;
		}
		break;
	case ATextEdit::eNextPage:		// Move down by the height of the viewport
		if (aY < aEoY)
		{	if ((aY += cViewHeight) > aEoY) aY = aEoY;	// Don't move past last row.
			ATextRow* apRow = cDocument[aY];
			if (aX < cLastX)
				aX = cLastX;
			else
				cLastX = aX;
			aX = apRow->scanBegTab(aX, false/*IncludeNewline*/);
			aMoved = true;
			aVertMove = true;
		}
		break;
	case ATextEdit::eNextSow:		// Move forward to Start-Of-Word unless already at sow.
		{	aX = iorPos.x(), aY = iorPos.y();
			ATextRow* apRow = cDocument[aY];
			while (aY >= 0)
			{	if ((aX = apRow->scanBow(true/*Fwd*/, true/*SameWord*/, aX)) < 0)
				{	if (aY > 0)
					{	apRow = cDocument[--aY];
						aX = apRow->mRowLgth;
					}
					else
						break;
				}
				else
				{	aMoved = true;
					break;
				}
			}
		}
		break;

	case ATextEdit::ePrevBow:		// Move back to previous beg-of-word
	{	aX = iorPos.x(), aY = iorPos.y();
		ATextRow* apRow = cDocument[aY];
		while (aY >= 0)
		{	if ((aX = apRow->scanBow(false/*Fwd*/, false/*SameWord*/, aX)) < 0)
			{	if (aY > 0)
				{	apRow = cDocument[--aY];
					aX = apRow->mRowLgth;
				}
				else
					break;
			}
			else
			{	aMoved = true;
				break;
			}
		}
		break;
	}
	case ATextEdit::ePrevChar:		// Move back to previous position in the cusror stack.
		if (aX > 0)
		{	--aX;
			while (at(aX, aY) == ATextEdit::scLatin1Space)
				--aX;
			aMoved = true;
		}
		else if (aY > 0)
		{	aMoved = true;
			aX = eol(--aY);
		}
		break;
	case ATextEdit::ePrevCur:		// Move back to previous position in the cusror stack.
		// See addCursor for more details.
		if (cCurPos > 0)
		{	QPoint aCursor = cCurStack[--cCurPos];
			aX = aCursor.x();
			aY = aCursor.y();
			aMoved = true;
			checkCurPos(false/*Push*/);
		}
		break;
	case ATextEdit::ePrevHalfPage:	// Move up by half the rows in the display
		if (aY >= cViewHeight / 2)
		{	ATextRow* apRow = cDocument[aY -= cViewHeight / 2];
			if (aX < cLastX)
				aX = cLastX;
			else
				cLastX = aX;
			aX = apRow->scanBegTab(aX, false/*IncludeNewline*/);
			aMoved = true;
			aVertMove = true;
		}
		break;
	case ATextEdit::ePrevLine:		// Move up to previous row
		if (aY > 0)
		{	ATextRow* apRow = cDocument[--aY];
			if (aX < cLastX)
				aX = cLastX;
			else
				cLastX = aX;
			aX = apRow->scanBegTab(aX, false/*IncludeNewline*/);
			aMoved = true;
			aVertMove = true;
		}
		break;
	case ATextEdit::ePrevPage:		// Move up by the number of rows in the display
		if (aY > 0)
		{	aY -= (aY > cViewHeight) ? cViewHeight : aY;
			ATextRow* apRow = cDocument[aY];
			if (aX < cLastX)
				aX = cLastX;
			else
				cLastX = aX;
			aX = apRow->scanBegTab(aX, false/*IncludeNewline*/);
			aMoved = true;
			aVertMove = true;
		}
		break;
	case ATextEdit::ePrevSow:		// Move back to Start-Of-Word unless already at sow.
	{	aX = iorPos.x(), aY = iorPos.y();
		ATextRow* apRow = cDocument[aY];
		while (aY >= 0)
		{	if ((aX = apRow->scanBow(false/*Fwd*/, true/*SameWord*/, aX)) < 0)
			{	if (aY > 0)
				{	apRow = cDocument[--aY];
					aX = apRow->mRowLgth;
				}
				else
					break;
			}
			else
			{	aMoved = true;
				break;
			}
		}
		break;
	}
	case ATextEdit::ePrevTab:		// Move back to the previous tab stop on same line
		if (aX > 0 && aTabWidth > 0)
		{   --aX;
            aX -= (aX % aTabWidth);
			aMoved = true;
		}
		break;
	case ATextEdit::eNoMove:
	case ATextEdit::eSame:			// Return the current cursor position
		aMoved = true;
		break;
	default:
		break;
	}
	// Reset. Reset cLastX if no longer moving up/down
	if (!aVertMove)
		cLastX = -1;

	// Move. Return new position to caller
	if (aMoved)
	{	iorPos.setX(aX);
		iorPos.setY(aY);
	}
	return aMoved;
}

/*!
\brief paint - Called upon a paint event to update the display in the viewport.

\param irArea - Area in pixels relative to viewport coordinates.
\param irOrigin - Upper left corner of viewport relative to canvas.
\param irPainter - The painter for this document
\par Notes:
-# Since the vieport can move around in the text, the coordinates inside the viewport must be translated to
the coordinates of the underlying canvas.
 */
void ATextDocument::paint(const QRect& irArea, const QPoint& irOrigin, QPainter& irPainter)
{
	// Expand. Convert from pixels to row/colum. Expand rectangle to include whole characters.
	int aX, aY, aW, aH;
	irArea.getRect(&aX, &aY, &aW, &aH);
	int aChW = cpParameters->mCharWidth;
	int aLnH = cpParameters->mLineHeight;
	int aTop = aY / aLnH;
	int aBot = (aY + aH + aLnH - 1) / aLnH;
	int aLeftOffset = cpParameters->mLeftOffset;
	if (cpParameters->mLineNumbers)
		aLeftOffset += (cGutterWidth + 1) * aChW;
	aX -= aLeftOffset;
	int aLeft = (aX < 0 ? 0 : aX) / aChW;
	int aRight =(aX + aW + aChW - 1) / aChW;
	if (aLeft < aRight)
	{	// Origin. Set offset in pixels from top-left of viewport to top-left of display
		aX = aLeft * aChW;				// Round aX to left edge of char
		aY = aTop * aLnH;				// Round aY to top edge of row
		QPoint aOrigin(aX, aY);

		// Translate. Translate from viewport coordinates to coordinates of underlying canvas
		long aCount = cDocument.count();
		long aXvp = irOrigin.x();			// Viewport offset from left edge of text [chars]
		long aYvp = irOrigin.y();			// Viewport offset from top row of text [rows]
		aLeft += aXvp;
		aRight += aXvp;
		aTop += aYvp;
		if ((aBot += aYvp) > aCount)
			aBot = aCount;

		// Repaint.  Repaint aHeight rows starting at row aTop.  Paint aWidth characters starting from column aLeft 
		for (long aRow = aTop; aRow < aBot; ++aRow)
		{	cDocument[aRow]->paint(aLeft, aRight, aRow, aOrigin, irPainter);
			aOrigin.ry() += aLnH;
		}
	}
}

/*!
\brief refresh - Reset tabs if the tab width has changed

For each row that has a tab, expand the tabs in the row.
\return aTab true iff at least one row has changed width.
\par Notes:
-# Only needed for a change in the tab width.
-# If any rows have changed width, rebalance must be called to fix rows that are too long/short.
 */
bool ATextDocument::refresh()
{
	long aEndY = cDocument.count() - 1, aY; // index of last row in document, row index
	bool aTab = false;
	for (aY = 0; aY <= aEndY; ++aY)
		if (cDocument[aY]->refresh())
			aTab = true;
	return aTab;
}

/*!
\brief rebalance - scan modified rows to fix up the lengths and colors.

Join short rows that will fit on one row. Then, if a row is too long, add a new row with the tail. Move to the next row and continue.
\param iorOrigin - Starting cursor position from the top row.
\param iEndY - Final row to be rebalanced
\par Notes:
-# The scan continues until iEndY or until no more changes, whichever comes last. A change to just one row can ripple through the
rest of the document.
-# The adjusted starting cursor position is returned in iorOrigin
 */
void ATextDocument::rebalance(QPoint& iorOrigin, long iEndY)
{
	long aBegX = iorOrigin.x(), aBegY = iorOrigin.y();
	long aBow, aNextLgth;
	long aLgth, aY, aEndY = cDocument.count() - 1; // Row length, row index, index of last row in document
	ATextRow* apRow, *apNextRow;
	QByteArray aTail, aNext;		// Holds tail of current line, contents of next line.
	bool aWrap = cpParameters->mWrap;
	long aMaxWidth = (aWrap) ? cViewWidth : cpParameters->mMaxRowWidth;;
	if (cpParameters->mLineNumbers)
	{	aLgth = decimalWidth(aEndY + 1);
		if (cGutterWidth != aLgth)
		{	cGutterWidth = aLgth;
		}
		aMaxWidth -= cGutterWidth + 1;		// Allow a space between gutter and text.
	}
	if (iEndY < 0) iEndY = aEndY;
	for (aY = aBegY; aY <= aEndY; ++aY)
	{	// Too Short. If no newline, append the next row to this one.
		apRow = cDocument[aY];
		aLgth = apRow->mRowLgth;
		while (!apRow->mHasNewline && aLgth < aMaxWidth && aY < aEndY)
		{	apNextRow = cDocument[aY + 1];
			if (apNextRow->mRowLgth > 0)
			{	aNextLgth = apNextRow->chop(0/*StartX*/, aNext, 0/*Lgth*/);
				apRow->append(aNext, 0/*SrcX*/, aNextLgth);
			}
			--aEndY;
			cDocument.removeAt(aY + 1);
			delete apNextRow;
			aLgth = apRow->mRowLgth;		// Length after combining rows
		}
		// Too Wide. If the row is too wide or if row ends in a word, shorten the row.
		if (aLgth > aMaxWidth || (aWrap && aY < aEndY && aLgth == aMaxWidth && ATextEdit::scInWord[apRow->at(aLgth-1)&0xff] > 0))
		{	// If in a word, scan back from aMaxWidth to a bow.
			if (aWrap)
			{	if ((aBow = apRow->scanBow(false/*Fwd*/, false/*SameWord*/, aMaxWidth)) > 0)
					aLgth = aBow;
				else if (aLgth == aMaxWidth)
				{	if (aY > iEndY)
						break;
					else
						continue;
				}
				else
					aLgth = aMaxWidth;
			}
			else
				aLgth = aMaxWidth;

			// Cursor. Note if cursor moves to the next row.
			if (aBegX > aLgth)
				iorOrigin = QPoint(aBegX - aLgth, aY + 1);
			aBegX = 0;

			// Chop. Chop off the tail of this row. The tail is not empty.
			aTail.clear();
			aLgth = apRow->chop(aLgth/*StartX*/, aTail, 0/*Lgth*/);

			// Add. Add a new row containing the tail.
			if (aY == aEndY || aTail.at(aLgth-1) == '\n')
			{	apNextRow = new ATextRow(this);
				cDocument.insert(aY + 1, apNextRow);
				apNextRow->append(aTail, 0/*SrcX*/, aLgth);

				// Last Row. If last row is newline-terminated, add empty row to the end.
				if (aY == aEndY && apNextRow->mHasNewline)
				{	apRow = new ATextRow(this);
					cDocument.append(apRow);
				}
				aEndY = cDocument.count() - 1;
			}
			else	// Prepend. Prepend tail to next row. Tail is not newline-terminated.
			{	apNextRow = cDocument[aY + 1];
				aNextLgth = apNextRow->chop(0/*StartX*/, aNext, 0/*Lgth*/);
				apNextRow->append(aTail, 0/*SrcX*/, aLgth);
				if (!aNext.isEmpty())
					apNextRow->append(aNext, 0/*SrcX*/, aNextLgth);
			}
		}
		else if (aY > iEndY)
			break;
	}
	// Foreground. Fix colors in this row
	setForeground(aBegY, aY);
}

/*!
\brief scan - Determine the position of iLength characters from the iorStart position.

\param iLength - Number of chars to scan
\param iorStart - On entry the starting position; on exit the adjusted start position if in the middle of a tab.
\return aFinish - The new position
 */
QPoint ATextDocument::scan(long iLength, QPoint& iorStart)
{
	long aX = iorStart.x(), aEndY = cDocument.count() - 1, aBegY = iorStart.y(), aY = aBegY;
	long aEndX = aX;
	QPoint aFinish(aX, aBegY);
	ATextRow* apRow;
	long aRowLgth;
	if (iLength > 0 && aBegY <= aEndY)
	{	// Wrong foot. If at end of first row, start at beg of next row.
		apRow = cDocument[aY];
		aRowLgth = apRow->mRowLgth;
		if (aX >= aRowLgth)
		{	if (aBegY >= aEndY)
				return aFinish;
			aX = 0, ++aY;
			iorStart.setX(aX);
			iorStart.setY(aY);
		}
		// Scan Rows. Advance in each row until passed iLength chars or until at end-of-text.
		for (;;)
		{	aEndX = apRow->scan(aX, &iLength);
			// End. If at end-of-scan or on last row, quit
			if (iLength == 0 || aY == aEndY)
			{	// Newline. If past newline, move to beginning of next row
				if (aEndX >= aRowLgth && apRow->mHasNewline)
				{	// Eot. Back off if on last line.
					if (aY == aEndY)
						--aEndX;				// Should not happen
					else
						aEndX = 0, ++aY;
				}
				break;
			}
			// Eol. Continue scan at beginning of next row.
			aX = 0;
			apRow = cDocument[++aY];
			aRowLgth = apRow->mRowLgth;
		}
	}
	else if (iLength < 0 && aBegY >= 0)
	{	// Wrong foot. If at beg of first row, start at end of prev row.
		if (aX == 0)
		{	if (aBegY == 0)
				return aFinish;
			apRow = cDocument[--aY];
			aX = apRow->mRowLgth;
			iorStart.setX(aX);
			iorStart.setY(aY);
		}
		else
			apRow = cDocument[aY];
		// Scan Rows. Move back in each row until passed iLength chars or until at beg-of-text.
		for (;;)
		{	aEndX = apRow->scan(aX, &iLength);
			if (iLength == 0 || aY == 0)
				break;
			// Bol. Continue scan at end of previous row.
			apRow = cDocument[--aY];
			aX = apRow->mRowLgth;
		}
	}
	aFinish.setX(aEndX);
	aFinish.setY(aY);
	return aFinish;
}

/*!
\brief setCursor - Set the text cursor to the position specified by irNewPos

\param irNewPos - Row, column position of the text cursor.
\param iIncludeNewline - Iff true, allow cursor to move past the newline.
\return (aX aY) - New position of the text cursor.
\par Notes:
-# Limits the position to a valid character position within the text
 */
QPoint ATextDocument::setCursor(const QPoint& irNewPos, bool iIncludeNewline)
{
	// Check. Limit moves to within the text.
	long aCount = cDocument.count();
	long aX = irNewPos.x();
	long aY = irNewPos.y();
	if (aCount == 0)
	{	qDebug("ATextDocument::setCursor(X:%ld, Y:%ld), Empty document", aX, aY);
		aX = aY = 0;
	}
	else if (aY >= aCount)
	{	aY = aCount - 1;
		aX = eol(aY);
	}
	else
	{	long aEol = eol(aY, iIncludeNewline);
		if (aX > aEol)
			aX = aEol;

		// Tab. Move back to tab if inside a tab.
		ATextRow* apRow = cDocument[aY];
		aX = apRow->scanBegTab(aX, iIncludeNewline);
	}
	return QPoint(aX, aY);
}

/*!
\brief setForeground - update the foreground colors.

\param iBegY - First row to be scanned
\param iEndY - Last mandatory row to be scanned where iEndY >= iBegY.
\return void
\par Notes:
-# The foreground color depends upon the comments, quoted strings, keywords
-# The scan extends to the point in the text where the ending color of a row matches the starting color
of the next row.
 */
void ATextDocument::setForeground(long iBegY, long iEndY)
{
	long aEndY = cDocument.count() - 1;
	if (cDocument.isEmpty())
		qDebug("ATextDocument::setForeground(BegY:%ld, EndY:%ld), Document is empty.", iBegY, iEndY);
	else if (iBegY > iEndY || iBegY > aEndY)
		qDebug("ATextDocument::setForeground(BegY:%ld, EndY:%ld), Eot:%ld, BegY is out of range.", iBegY, iEndY, aEndY);
	else
	{	AInState aState = eStd;
		ATextRow* apRow;
		for (long aY = iBegY;  aY <= aEndY; ++aY)
		{	apRow = cDocument[aY];
			if (aY > iEndY && apRow->mBegState == aState)
				break;
			apRow->mBegState = aState;
			aState = apRow->setForeground(0, aState);
		}
	}
}

/*!
\brief setSelected - Set/clear the highlighted text from the anchor to the cursor

\param irAnchor - Starting row, column in the text to be selected
\param irCursor - Ending row, column in the text to be selected (may precede irAnchor).
\param iMatchedText - Iff true, set the background color to that for matched text.
\param iSelectedText - Iff true, set the background color to that for selected text.
\return void
\par Notes:
-# At most only one of iMatchedText and iSelected text can be true.
-# If neither are true, the background color is reset.
 */
void ATextDocument::setSelected(const QPoint& irAnchor, const QPoint& irCursor, bool iMatchedText, bool iSelectedText)
{
	long aBegX, aBegY, aEndX, aEndY, aX, aY;
	if (irAnchor != irCursor)
	{	if (isLess(irAnchor, irCursor))
		{	aBegX = irAnchor.x();
			aBegY = irAnchor.y();
			aEndX = irCursor.x();
			aEndY = irCursor.y();
		}
		else
		{	aBegX = irCursor.x();
			aBegY = irCursor.y();
			aEndX = irAnchor.x();
			aEndY = irAnchor.y();
		}
		long aCount = cDocument.count();
		if (aEndY >= aCount) aEndY = aCount - 1;
		for (aX = LONG_MAX, aY = aBegY; aY <= aEndY; ++aY)
		{	if (aY == aEndY) aX = aEndX;
			cDocument[aY]->setBackground(aBegX, aX, iMatchedText, iSelectedText);
			aBegX = 0;
		}
	}
}

/*
\brief tabify - Convert leading spaces/tabs in the selected rows into tabs or vice versa.

\param iBegRow -First row to be converted.
\param iCount - Number of rows to be converted (>0)
\param iExtraSpaces	-1, delete leftover spaces; 0, keep leftover spaces; +1 convert leftover spaces to tabs
\param iToTabs - If true, convert to spaces to tabs; else, convert tabs to spaces
\return void
\par Notes:
-# ExtraSpaces is ignored if converting tabs to spaces.
 */
void ATextDocument::tabify(long iBegRow, long iCount, long iExtraSpaces, bool iToTabs)
{
	long aEndY = iBegRow + iCount, aY;
	for (aY = iBegRow; aY < aEndY; ++aY)
		cDocument[aY]->tabify(iExtraSpaces, iToTabs);
}

/*!
\brief text - Fetch the text in one row or all of the text editor

\param irBeg - Starting row, column to be fetched
\param irEnd - Ending row, column just past last char to be fetched
\param iorText - The text in the row is appended to iorText
\param iLgth - The current number of bytes in iorText
\return aLgth - The number of bytes in iorText on return
 */
long ATextDocument::text(const QPoint& irBeg, const QPoint& irEnd, QByteArray& iorText, long iLgth)
{
	long aBegX, aBegY, aEndX, aEndY, aX, aY;
	if (irBeg != irEnd)
	{	if (isLess(irBeg, irEnd))
		{	aBegX = irBeg.x();
			aBegY = irBeg.y();
			aEndX = irEnd.x();
			aEndY = irEnd.y();
		}
		else
		{	aBegX = irEnd.x();
			aBegY = irEnd.y();
			aEndX = irBeg.x();
			aEndY = irBeg.y();
		}
		ATextRow* apRow;
		for (aY = aBegY; aY <= aEndY; ++aY)
		{	apRow = cDocument[aY];
			aX = (aY == aEndY) ? aEndX : apRow->mRowLgth;
			iLgth = apRow->mid(aBegX, aX - aBegX, iorText, iLgth);
			aBegX = 0;
		}
	}
	return iLgth;
}

/*!
\brief textLength - Fetch the number of chars in the text

\return The number of bytes in the text
\par Notes:
-# Tabs count as one byte.
 */
long ATextDocument::textLength()
{
	long aY = 0;
	long aLgth = 0;
	ATextRow* apRow;
	long aCount = cDocument.count();
	for (aY = 0; aY < aCount; ++aY)
	{	apRow = cDocument.at(aY);
		aLgth += apRow->mRowLgth;
	}
	return aLgth;
}

/*!
\brief truncateTop - delete all but the last iLeft rows from the text.

\param iLeft - Number of rows that are not deleted
 */
long ATextDocument::truncateTop(long iLeft)
{
	long aDeleted = cDocument.count() - iLeft;
	if (aDeleted > 0)
	{	for (long aRow = 0; aRow < aDeleted; ++aRow)
			delete cDocument.takeFirst();

		// Restore. Leave at least one empty row.
		if (iLeft == 0)
		{	ATextRow* apRow = new ATextRow(this);
			cDocument.insert(0, apRow);
		}
		QPoint aOrigin(0, 0);
		rebalance(aOrigin, 0);
	}
	return aDeleted;
}

// end
