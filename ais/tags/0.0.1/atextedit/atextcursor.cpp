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
aisdev/atextedit/atextcursor.cpp
														Text Cursor

ATextCursor mimics the behavior of a cursor in a screen editor.

CHANGE HISTORY
Version	Date		Who		Change
3.2007	3/26/2008	fchua	find. Fixed search highlight bug if a matching paren is highlighted.
3.2006	3/10/2008	fchua	Fixed indefinite loop bug in replaceAll.
2.0003	2/8/2007	tlw		paint.  Move left offset of cursor over if line numbers enabled.
1.0118	12/12/2006	tlw		replace. Remove as it is no longer needed.
1.0116	11/27/2006	tlw		refresh. Expand tabs and adjust line wrapping.
1.0116	11/27/2006	tlw		insert,append. Pass in cursor to be adjusted if wrap.
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0110	10/10/2006	tlw		Mark. Only mark first undo operation added after a move.
1.0109	10/5/2006	tlw		dumpRow. Add dumpRow method. setCursor. Add IncludeNewline arg.
1.0108	9/30/2006	tlw		dumpUndo. Add new method.
1.0107	9/22/2006	tlw		mid. Extract slice of text. move. Modify to use iorCursor as initial value.
1.0107	9/20/2006	tlw		Add third argument to setCursor for saving the cursor in the cursor stack.
1.0070	11/5/2005	tlw		Move ADoc to a separate class.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QRect>
#include <QtGui/QPainter>
#include "atextcursor.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief ATextCursor - Constructor initializes its class variables

\param ipViewPort -> The viewport is a rectangular widget that shows a section of the text.
\param ipParameters -> structure containing the configurable edit parameters.
\sa AParameters in aparameters.h
 */
ATextCursor::ATextCursor(QWidget* ipViewPort, AParameters* ipParameters)
  :	QObject(ipViewPort),cAnchor(0,0),cCursor(0, 0), cMark(true), cMatchedText(false), cpParameters(ipParameters), cRowCount(0),
	cSelectedText(false), cTextDocument(ipParameters), cpViewPort(ipViewPort)
{
}

/*!
\brief append - Append a string to the text.

\param irText - Text to be appended to the end of the current text.
\param iLgth - Length of irText.
\param iMoveToEnd - Iff true, move the cursor to the end of the appended text.
\return void
\par Notes:
-# Append limits the maximum number of lines to mMaxRows
-# Append does not delete selected.
-# Append does not save inserts on the undo stack. See also insert.
 */
void ATextCursor::append(const QByteArray& irText, long iLgth, bool iMoveToEnd)
{
	// Append. Append text to the document.
	QPoint aCursor(-1, -1);
	long aCount = cTextDocument.append(aCursor, irText, iLgth);
	
	// Truncate. Limit size to MaxRows
	// PENDING ATextCursor::append() Better to count lines to be added and then truncate lines in current text before appending irText.
	// also, truncate number of lines to be added if irText exceeds aMaxRows.
	long aMaxRows = cpParameters->mMaxRows;
	if (aCount > aMaxRows)
	{	if (aMaxRows > ATextEdit::scBfrInc)
			aMaxRows -= ATextEdit::scBfrInc;
		if (cTextDocument.truncateTop(aMaxRows) > 0)
		{	aCount = cTextDocument.cDocument.count();
			cMark = true;
		}
	}
	if (cRowCount != aCount)
	{	emit rowCountChanged(aCount);
		cRowCount = aCount;
	}
	// Move cursor to the end of the appended text
	if (iMoveToEnd)
		setCursor(cTextDocument.eot(), false/*ExtendSelect*/, false/*IncludeNewline*/, true/*SaveCur*/);
}

/*!
\brief clearUndo - Remove all entries from the undo stack

\return void
 */
void ATextCursor::clearUndo()
{
	cUndo.clear();
	cMark = true;
}

/*!
\brief count - Fetch the number of bytes in the document

\return Number of bytes in the document.
\par Notes:
-# By default, a new document has a single newline in it which cannot be deleted, so the count is rarely 0.
 */
long ATextCursor::count()
{
	return cTextDocument.cDocument.count();
}

/*!
\brief cursor - Returns the X, Y position of the selected cursor.

\return aCursor Selected cursor position(x,y)
\par Notes:
-# X is measured from the left in chars starting from 0.
-# Y is measured from the top in lines starting from 0.
*/
QPoint ATextCursor::cursor(ATextEdit::ACursorType iType)
{
	QPoint aCursor;
	switch (iType)
	{
	case ATextEdit::eAnchor:
		aCursor = cAnchor;
		break;
	case ATextEdit::eCursor:
		aCursor = cCursor;
		break;
	case ATextEdit::eEnd:
		aCursor = cTextDocument.eot();
		break;
	}
	return aCursor;
}

/*!
\brief deleteIt - Delete some text.

\param iCmd - eDelFwd or eDelBack. If eDelBack move back iLgth chars and then delete.
\param iLgth - Number of chars to be deleted. Negative if DelBack
\return Number of characters deleted (not including padding for tabs).
\par Notes:
-# If iLgth equals or exceeds the number of chars in the text, delete all but the final newline from the text.
 */
long ATextCursor::deleteIt(long iLgth)
{
	QPoint aCursor(cCursor);
	aCursor = cTextDocument.scan(iLgth, aCursor);
	return deleteIt(aCursor);
}

/*!
\brief deleteIt - Delete text from irToCursor up to the current cursor position.

\param iCmd - eDelFwd or eDelBack. Only used in selecting the type of undo.
\param irToCursor - Starting position of the cursor.
\return Number of characters deleted (not including padding for tabs).
\par Notes:
-# The deleted area includes the currently selected text which extends from the anchor to the current cursor (or vice versa).
-# The deleted area starts from the top-leftmost cursor: either the anchor, the current cursor, or irCursor.
-# If deleted area extends to the bottom-rightmost cursor: either the anchor, the current cursor, or irCursor.
 */
long ATextCursor::deleteIt(const QPoint& irToCursor)
{
	bool aBack;				// Direction of delete - forward or back
	bool aMark = cMark;		// Save current state prior to setCursor.
	long aLgth = 0;
	QByteArray aDeletedText;
	// Selected. Extend selected area to encompass the new cursor position
	if (cSelectedText)
	{	aBack = cTextDocument.isLess(cAnchor, cCursor);			// aALtC
		bool aALtN = cTextDocument.isLess(cAnchor, irToCursor);
		bool aCLtN = cTextDocument.isLess(cCursor, irToCursor);
		// If cursor is between anchor and new cursor, move it to new cursor
		if (aBack == aCLtN)
			cCursor = irToCursor;

		// If anchor is between cursor and new cursor, move it to new cursor.
		else if (aBack != aALtN)
			cAnchor = irToCursor;

		// Delete. Delete between adjusted anchor and adjusted cursor.  Moves cursor to begining of deleted text.
		aLgth = deleteSelected(aDeletedText, aLgth);
	}
	else
	{	// Matched. Erase matched text before deleting anything
		if (cMatchedText)
		{	cTextDocument.setSelected(cAnchor, cCursor, cMatchedText = false, false/*SelectedText*/);
			cAnchor = cCursor;
		}
		 // Delete. Delete between cursor and new cursor position 
		aLgth = cTextDocument.deleteIt(cCursor, irToCursor, aDeletedText, aLgth);

		// Move Back. Move cursor back to new position if delete backwards. Rematch parens in any case.
		aBack = cTextDocument.isLess(irToCursor, cCursor);
		QPoint aNewCur = aBack ? irToCursor : cCursor;
		setCursor(aNewCur, false/*ExtendSelect*/, false/*Newline*/, true/*SaveCur*/);
	}
	// Undo. Push the corresponding insert operation onto the undo stack. Increment undo stack index.
	ATextEdit::ACmdType aCmd = aBack ? ATextEdit::eInsFwd : ATextEdit::eInsBack;
	AUndoOp aUndoOp(aCmd, cCursor, aLgth, aMark, AUndoOp::eNot, aDeletedText);
	cMark = cUndo.add(aUndoOp);		// Clear cMark unless reversing an undo/redo.

	// Notify. If rows changed, let TextEdit know.
	long aCount = cTextDocument.cDocument.count();
	if (cRowCount != aCount)
	{	emit rowCountChanged(aCount);
		cRowCount = aCount;
	}
	return aLgth;
}

/*!
\brief deleteSelected - Delete text from the anchor to the cursor (or vice versa)

\param iorDeletedText - deleteSelected appends the deleted text to this byte array.
\param iLgth - The current length of iorDeletedText.
\par Notes:
-# Parens are rematched if the cursor is moved to the beginning or end of a pair of brackets.
-# Does not modify cMark (see deleteIt and insert where cMark is modified).
 */
long ATextCursor::deleteSelected(QByteArray& iorDeletedText, long iLgth)
{
	if (cSelectedText)
	{	// Delete.
		iLgth = cTextDocument.deleteIt(cAnchor, cCursor, iorDeletedText, iLgth);
		cSelectedText = false;

		// Move. Move cursor to beginning of the deleted text.
		if (cTextDocument.isLess(cAnchor, cCursor))
			cCursor = cAnchor;
		else
			cAnchor = cCursor;

		// Set cursor and rematch parens
		QPoint aNewCur = cCursor;
		setCursor(aNewCur, false/*ExtendSelect*/, false/*IncludeNewline*/, true/*SaveCur*/);
	}
	return iLgth;
}

/*!
\brief dumpRow - Dump info for specified ATextRow

\return  (BegState,HasNewline,Lgth) text :Fg,Bg,Hex ...
\sa ATextRow
 */
QString ATextCursor::dumpRow(long iRow)
{
	return cTextDocument.dumpRow(iRow);
}

/*!
\brief dumpUndo - Dump one operation or the entire undo stack

\param iPos  Position of operation relative to current position.
\return aDump (cmd, x, y, length, mark)<-(...)->(...)
\sa AUndo
\Notes:
-# If iPosition is large negative number, dump the entire stack.
 */
QString ATextCursor::dumpUndo(long iPos)
{
	return cUndo.dump(iPos);
}

/*!
\brief find - Search the text for a section that matches a pattern

\param irPattern - String or pattern to be matched
\param iAllText - Search all the text starting at the current cursor position (wrap at top or bottom).
\param iMatchCase - Case sensitive search iff iMatchCase is true.
\param iDown - Search from from cursor down and to the right if iDown is true; else, search from cursor up and to the left.
\param iRegExp - Treat irPattern as a regular expression iff true.
\param iSelect - Limit search to the currently selected text iff true.
\param iMatchWord - Matched text must start and end on a word boundry iff true.
\par Notes:
-# Even if iRegExp is false, irPattern may include \\t, \\n, \\b, \\f, etc. which match the tab, newline, bell, and formfeed.
-# Because the text is broken into lines and because tabs are variable length, the search cannot take advantage of several
speed up methods such as the Boyer-Moore algorithm.
 */
bool ATextCursor::find(const QString& irPattern, bool iAllText, bool iMatchCase, bool iDown, bool iRegExp, bool iSelect,
bool iMatchWord)
{
	bool aMatch = false;

	// Cursors. Set start and finish cursors
	QPoint aStart, aFinish;
	if (iSelect && cSelectedText)
	{	if (cTextDocument.isLess(cAnchor, cCursor) == iDown)
			aStart = cAnchor, aFinish = cCursor;
		else
			aStart = cCursor, aFinish = cAnchor;
	}
	else
	{	aStart = cCursor;
		aFinish = (iAllText) ? cCursor : (iDown) ? cTextDocument.eot() : QPoint(0,0);
	}
	if ((aMatch = cTextDocument.find(irPattern, iAllText, iMatchCase, iDown, iRegExp, iMatchWord, aStart, aFinish)))
	{	// Clear Selected. Clear existing selected before moving Anchor and Cursor.
		if (cSelectedText || cMatchedText)
			cTextDocument.setSelected(cAnchor, cCursor, cMatchedText = false/*MatchedText*/, cSelectedText = false);
		// Select. Select the matched text.
		cAnchor = aStart;
		setCursor(aFinish, true/*ExtendSelect*/, false/*IncludeNewline*/, true/*SaveCur*/);
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
QStringList ATextCursor::functionList()
{
	return cTextDocument.functionList();
}

/*!
\brief indent - Indent/outdent the selected lines by one char

\param iCh - The indent character (usually a tab)
\param iIn - Indent if true; else, outdent
\par Notes:
-# If outdenting and the leading char in a row is not iCh, the row is not modified.
 */
bool ATextCursor::indent(long iCh, bool iIn)
{
	bool aIndented = false;
	if (cSelectedText)
	{	// Clear. Clear selected text.
		bool aAnchorFirst = cTextDocument.isLess(cAnchor, cCursor);
		cTextDocument.setSelected(cAnchor, cCursor, cMatchedText = false, cSelectedText = false);

		// Indent. Indent/outdent first row of every line.
		long aBegY = aAnchorFirst ? cAnchor.y() : cCursor.y();
		long aEndY = aAnchorFirst ? cCursor.y() : cAnchor.y();
		aBegY = cTextDocument.indent(aBegY, iCh, aEndY, iIn);

		// Select. Set selected text from beginning to end of indented area.
		QPoint aCursor;
		if (aAnchorFirst)
		{	cAnchor.setX(0), cAnchor.setY(aBegY);
			aCursor.setX(cTextDocument.eol(aEndY)), aCursor.setY(aEndY);
		}
		else
		{	aCursor.setX(0), aCursor.setY(aBegY);
			cAnchor.setX(cTextDocument.eol(aEndY)), cAnchor.setY(aEndY);
		}
		setCursor(aCursor, true/*ExtendSelect*/, false/*IncludeNewline*/, true/*SaveCur*/);
		aIndented = true;
	}
	return aIndented;
}

/*!
\brief insert - Insert a string at the current cursor position.

\param iCmd - Either eInsFwd or eInsBack. If eInsFwd, the cursor is moved to the end of the inserted string.
\param irText - The text to be inserted.
\param iLgth - The number of chars in irText.
\return aCount - the number of rows in the document after the insert.
\par Notes:
-# irText may include newlines
 */
long ATextCursor::insert(ATextEdit::ACmdType iCmd, const QByteArray& irText, long iLgth)
{
	QByteArray aDeletedText;		// Selected text that was deleted
	bool aMark = cMark;				// Save current mark state while calling setCursor.
	long aDelLgth = 0;				// Length of deleted text, if any
	ATextEdit::ACmdType aUndoCmd;	// Command to undo the operation applied to the text
	// Matched. Clear matched text before inserting.
	if (cMatchedText)
	{	cTextDocument.setSelected(cAnchor, cCursor, cMatchedText = false, false/*SelectedText*/);
		cAnchor = cCursor;
	}
	// Selected. Clear selected text before inserting.
	else if (cSelectedText)
	{	// Selected Text. Delete selected text. deleteSelected sets cursor to beginning of deleted area.
		bool aALtC = cTextDocument.isLess(cAnchor, cCursor);
		if ((aDelLgth = deleteSelected(aDeletedText, aDelLgth)) > 0)
		{	// Undo. Save the corresponding insert operation.
			aUndoCmd = aALtC ? ATextEdit::eInsFwd : ATextEdit::eInsBack;
			AUndoOp aUndoOp(aUndoCmd, cCursor, aDelLgth, aMark, AUndoOp::eNot, aDeletedText);
			cUndo.add(aUndoOp);
			aMark = false;			// Do not mark subsequent insert after delete selected.
		}
	}
	// Insert. Insert text at the cursor. If InsFwd, scan to end of inserted area
	long aCount;
	if (!irText.isEmpty())
	{	QByteArray aText(irText);
		QPoint aCursor(cCursor);
		aCount = cTextDocument.insert(aCursor, aText, iLgth);

		// Cursor. Set cursor if insFwd. In any case, rematch parens.
		if (iCmd == ATextEdit::eInsFwd)
			aCursor = cTextDocument.scan(iLgth, aCursor);
		setCursor(aCursor, false/*ExtendSelect*/, false/*IncludeNewline*/, true/*SaveCur*/);
		
		// Undo. Push the corresponding delete operation onto the undo stack. Increment undo stack index.
		aUndoCmd = (iCmd == ATextEdit::eInsBack) ? ATextEdit::eDelFwd : ATextEdit::eDelBack;
		AUndoOp aUndoOp(aUndoCmd, cCursor, iLgth, aMark, AUndoOp::eNot, aText);
		cMark = cUndo.add(aUndoOp);		// Clear cMark unless reversing an undo/redo.
	}
	else
		aCount = cTextDocument.cDocument.count();

	// Notify. If rows changed, let TextEdit know.
	if (cRowCount != aCount)
	{	emit rowCountChanged(aCount);
		cRowCount = aCount;
	}
	return aCount;
}

/*!
insert - Insert a single character at the current cursor position.

\param iCh - The character to be inserted
\return The number of characters actually inserted
\par Notes:
-# This routine could be optimized to improve speed. For right now, it just calls the insert string method
 */
long ATextCursor::insert(char iCh)
{
	QByteArray aCh(1, iCh);
	return insert(ATextEdit::eInsFwd, aCh, 1);
}

QString ATextCursor::mid(const QPoint& irBeg, const QPoint& irEnd)
{
	QByteArray aText;
	cTextDocument.text(irBeg, irEnd, aText, 0);
	return QString(aText);
}

/*!
\brief move - Moves cursor based upon the move operation in iMove. Most moves are relative to the current position.

\param iCmd - eMoveFwd or eMoveBack
\param iMove - eNoMove, eBol, eBot, eEol, eEot, eNextBol, eNextBow, eNextChar, eNextCur, eNextEow, eNextHalfPage, eNextLine,
eNextPage, eNextSow, ePrevBow, ePrevChar, ePrevCur, ePrevHalfPage, ePrevLine, ePrevPage, ePrevSow, ePrevTab, eSame
\param orCursor - orCursor is set to the new cursor position.
\return aMoved - True iff move succeeds
\par Notes:
-# Sets orCursor to the new position if a move is possible.
-# Text, cCursor and cMark are not modified.
 */
bool ATextCursor::move(ATextEdit::AMoveTo iMove, QPoint& iorCursor)
{
	// Cursor. If an invalid cursor, use the current cursor position.
	if (iorCursor.y() < 0)
		iorCursor = cCursor;

	return cTextDocument.move(iMove, iorCursor);
}

/*!
\brief paint - Called on a paint event to update the display in the viewport.

\param irArea - Area in pixels relative to viewport coordinates.
\param irOrigin - Upper left corner of viewport relative to canvas.
\param iShow - Iff true, redraw the cursor.
\par Notes:
-# Since the viewport can move around in the text, the coordinates inside the viewport must be translated to
the coordinates of the underlying canvas.
-# The coordinates must be translated from pixels into chars and rows to locate the affected text.
 */
void ATextCursor::paint(const QRect& irArea, const QPoint& irOrigin, bool iShow)
{
	// Paint. Configure painter and paint text
	QPainter aPainter(cpViewPort);
	aPainter.setBackgroundMode(Qt::OpaqueMode);
	cTextDocument.paint(irArea, irOrigin, aPainter);

	// Cursor. Redraw Cursor
	if (iShow)
	{	// Translate. Translate from canvas coordinates to viewport coordinates.
		long aX = cCursor.x() - irOrigin.x();
		long aY = cCursor.y() - irOrigin.y();

		// Draw Cursor. Convert from chars to pixels.
		long aCharWidth = cpParameters->mCharWidth;
		long aLeftOffset = cpParameters->mLeftOffset;
		long aLineHeight = cpParameters->mLineHeight;
		if (cpParameters->mLineNumbers)
			aLeftOffset += (cTextDocument.cGutterWidth + 1) * aCharWidth;
		QPoint aBegCur(aX * aCharWidth + aLeftOffset + 1, aY * aLineHeight);
		QPoint aEndCur(aBegCur);
		aEndCur.ry() += cpParameters->mCursorHeight;		// ~12 pixels
		if (irArea.contains(aBegCur, false/*Inside*/))
		{	QPen aPen(ATextEdit::scPenColors[ATextEdit::scBgPlainText]);
			aPen.setWidth(cpParameters->mCursorWidth);		// ~2 pixels
			aPainter.setPen(aPen);			
			aPainter.drawLine(aBegCur, aEndCur);
		}
	}
	aPainter.end();
}

/*!
\brief rebalance - Some lines may be too long or too short. Scan every line and fix the padding for the tabs and
the point at which each row is wrapped or truncated.

\return void
\par Notes:
-#  We may need to reset the cursor after rebalance???
 */
void ATextCursor::rebalance()
{
	QPoint aOrigin(0, 0);
	cTextDocument.rebalance(aOrigin, -1);
	cMark = true;
}

/*!
\brief redo - Reapply the last undo operation

\param iToMark  If true, redo until next operation is marked; else, redo one operation.
\return aOk - True iff there is something to redo
\par Notes:
-# There is nothing to redo unless one or more undo operations have been performed since the last edit.
-# Redo does not change the operation, but subsequent insert/delete will reverse operation without changing the mark.
 */
bool ATextCursor::redo(bool iToMark)
{
	// Fetch. Fetch redo operations until an operation is marked.
	ATextEdit::ACmdType aCmdType;
	bool aOk = true;
	AUndoOp aOp;
	do
	{	aOp = cUndo.redo();
		if ((aCmdType = aOp.mCmd) != ATextEdit::eDelBack && aCmdType != ATextEdit::eDelFwd
		&& aCmdType != ATextEdit::eInsBack && aCmdType != ATextEdit::eInsFwd)
		{	qDebug("ATextCursor::redo, Bogus command, CmdType: %d", aCmdType);
			aOk = false;
			break;
		}
		// Redo. Redo the operation and reverse the operation on the undo stack. Increments undoStack index.
		if (cCursor != aOp.mCursor)
			setCursor(aOp.mCursor, false/*ExtendSelect*/, false/*IncludeNewline*/, true/*SaveCur*/);
		if (aCmdType == ATextEdit::eDelBack || aCmdType == ATextEdit::eDelFwd)
			deleteIt((aCmdType == ATextEdit::eDelBack) ? -aOp.mLength : aOp.mLength);
		else
			insert(aCmdType, aOp.mText, aOp.mLength);
	} while (iToMark && !cUndo.mark());
	return aOk;
}

/*!
\brief refresh - Reset tabs if the tab width has changed

For each row that has a tab, expand the tabs in the row.
\return aTab true iff at least one row has changed width.
\par Notes:
-# Only needed for a change in the tab width.
-# If any rows have changed width, rebalance must be called to fix rows that are too long/short.
 */
bool ATextCursor::refresh()
{
	return cTextDocument.refresh();
}

/*!
\brief replaceAll - Replace every section of text that matches irPattern with the text in irText.

\param irPattern - String or pattern to be matched.
\param irText - Text to replace the matched text.
\param iAllText - Search all the text from bottom to top (ignored if iSelect is true).
\param iMatchCase - Case sensitive search iff iMatchCase is true.
\param iRegExp - Treat irPattern as a regular expression iff true.
\param iSelect - Limit search to the currently selected text iff true.
\param iMatchWord - Matched text must begin and end on a word boundry iff true.
\return aMatch - True if at least one match.
\par Notes:
-# Even if iRegExp is false, irPattern may include \\t, \\n, \\b, \\f which match a tab, newline, bell, or formfeed character.
-# Search begins at the end of the search region and extends back up to the beg of the search region.  That way, the stopping
point is not shifting around as text is inserted/deleted.
-# replaceAll does not re-search the replacement text.  The search continues upward from the top of the replaced text to avoid
an endless loop.
 */
bool ATextCursor::replaceAll(const QString& irPattern, const QString& irText, bool iAllText, bool iMatchCase, bool iRegExp,
bool iSelect, bool iMatchWord)
{
	// Search Region. Start starts at end of region and moves upward. Stop is fixed at beg of region.
	bool aMatch = false;
	QPoint aTopMatch;		// Top end of matched text.
	QPoint aStart;			// Bottom starting position of a search region which is bottom starting position of a match.
	QPoint aStop;			// Top end of search region. aStop does not move as search/replace proceeds.
	if (iSelect)
	{	if (cSelectedText)
		{	if (cTextDocument.isLess(cAnchor, cCursor))
				aStart = cCursor, aStop = cAnchor;
			else
				aStart = cAnchor, aStop = cCursor;
		}
		else
			return false;
	}
	else
	{	aStart = cTextDocument.eot();
		aStop = (iAllText) ? QPoint(0, 0): cCursor;
		// make sure it doesn't loop endless
		iAllText = false;
	}
	// Find. If a match, select the matched text. On a match, find moves aStart to bottom of match, aTopMatch to top of match.
	while (cTextDocument.find(irPattern, iAllText, iMatchCase, false/*Down*/, iRegExp, iMatchWord, aStart, aTopMatch = aStop))
	{	// Clear Selected. Clear existing selected before moving Anchor or Cursor.
		if (cSelectedText)
			cTextDocument.setSelected(cAnchor, cCursor, false/*MatchedText*/, cSelectedText = false);

		// Select. Select matched text which may include a trailing newline. Move cursor up to top of matched region.
		cAnchor = aStart;
		setCursor(aTopMatch, true/*ExtendSelect*/, true/*IncludeNewline*/, false/*SaveCur*/);

		// Replace. Replace selected text with irText. Cursor is not moved.
		insert(ATextEdit::eInsBack, QByteArray(irText.toLatin1()), irText.length());

		// Search Region. Continue search upward from the top end of the matched text.
		aStart = aTopMatch;
		aMatch = true;
	}
	return aMatch;
}

/*!
\brief selectedText - Fetch the currently selected text.

\param iorText - selectedText appends the selected text to iorText
\param iLgth - Starting length of the existing iorText
\return aLgth - The length of the returned text
 */
long ATextCursor::selectedText(QByteArray& iorText, long iLgth)
{
	long aLgth = 0;
	if (cSelectedText)
		aLgth = cTextDocument.text(cAnchor, cCursor, iorText, iLgth);
	return aLgth;
}

/*!
\brief selectMatchedText - Change matched text into selected text.

\return aSwitched - True iff some text is currently matched.
 */
bool ATextCursor::selectMatchedText()
{
	bool aSwitched = false;
	// Switch. Select text if currently matched 
	if (cMatchedText)
	{	cMatchedText = false;
		cSelectedText = true;
		aSwitched = true;
	}
	 // Selected. Clear selected.
	else if (cSelectedText)
	{	cSelectedText = false;
		aSwitched = true;

		// Match. Match text if cursor and anchor are between brackets.
		if (cpParameters->mMatchParen && cTextDocument.matchParen(cCursor, cAnchor))
			cMatchedText = true;
	}
	// Select. Select from the anchor to the cursor.
	if (aSwitched)
		cTextDocument.setSelected(cAnchor, cCursor, cMatchedText, cSelectedText);
	return aSwitched;
}

/*!
\brief setCursor - move the current cursor position to irCursor

\param irCursor - New cursor position
\param iIncludeNewline - If true, allow cursor to move past newline; else, stop at the newline position.
\param iExtendSelect - Iff true, extend the currently selected area from the anchor to the new cursor position.
\return aRepaint - True iff the text needs to be repainted.
\par Notes
-# If an invalid cursor, use current cursor position.
-# If the cursor moves by a page or more, append this position to the cursor stack at the current position. 
 */
bool ATextCursor::setCursor(const QPoint& irCursor, bool iExtendSelect, bool iIncludeNewline, bool iSaveCur)
{
	// Clear. If matched or selected text previously highlighted, clear the highlighted section
	bool aRepaint = false;				// Repaint if text colors are modified.
	if (cSelectedText || cMatchedText)
	{	cTextDocument.setSelected(cAnchor, cCursor, false/*MatchedText*/, cSelectedText = false);
		if (cMatchedText)
		{	cAnchor = cCursor;
			cMatchedText = false;
		}
		aRepaint = true;
	}
	// Cursor. If an invalid cursor, use the current cursor position.
	QPoint aCursor(irCursor.y() < 0 ? cCursor : irCursor);

	// Cursor. Set cursor to new position. Limit move to a valid position in the text.
	cCursor = cTextDocument.setCursor(aCursor, iIncludeNewline);
	cMark = true;		// If cursor moved, mark next operation added to undo stack.

	// Stack. If the move is more than page, append this position to the cursor stack at the current position.
	if (iSaveCur)
		cTextDocument.addCursor(cCursor);

	// Selected text. Set selected text from new cursor to the anchor.
	if (iExtendSelect)
	{	cTextDocument.setSelected(cAnchor, cCursor, cMatchedText, cSelectedText = true);
		aRepaint = true;
	}
	// Matched text. Match text from new cursor to the new (matching) anchor.
	else if (cpParameters->mMatchParen && cTextDocument.matchParen(cCursor, cAnchor))
	{	cTextDocument.setSelected(cAnchor, cCursor, cMatchedText = true, cSelectedText);
		aRepaint = true;
	}
	else // Anchor. Move anchor to new position
		cAnchor = cCursor;
	return aRepaint;
}

/*!
\brief setViewSize - Set the size of the viewport

\param irSize - Sets the size and position of the viewport
\return void
 */
void ATextCursor::setViewSize(const QSize &irSize)
{
	cTextDocument.cViewWidth = irSize.width();
	cTextDocument.cViewHeight = irSize.height();
}

/*!
\brief switchCursors - Switch the anchor with the cursor

switchCursors does not modify highlighted or matched text
\return void
 */
bool ATextCursor::switchCursors()
{
	QPoint aAnchor = cAnchor;
	cAnchor = cCursor;
	return setCursor(aAnchor, cSelectedText, false/*includeNewline*/, true/*SaveCur*/);
}

/*!
\brief tabify - Convert leading spaces in the selected rows into tabs or vice versa.

\param iExtraSpaces - -1, delete leftover spaces; 0, keep leftover spaces; +1 convert leftover spaces to tabs
\param iToTabs - If true, convert to spaces to tabs; else, convert tabs to spaces
\return aIsSelected - True iff at least one row is currently selected
\par Notes:
-# ExtraSpaces is ignored if converting tabs to spaces.
-# Tabified area is selected even if text is not changed
-# Cursor is moved to end of tabified area.
 */
bool ATextCursor::tabify(long iExtraSpaces, bool iToTabs)
{
	bool aIsSelected = false;		// At least one row currently selected.
	if (cSelectedText)
	{	long aBegRow, aCount, aY = cCursor.y();
		if (cTextDocument.isLess(cAnchor, cCursor))
			aBegRow = cAnchor.y(), aCount = aY - aBegRow + 1;
		else
			aBegRow = aY, aCount = cAnchor.y() - aBegRow + 1;
		cTextDocument.tabify(aBegRow, aCount, iExtraSpaces, iToTabs);

		// End. Select text up to end of last indented row.
		cAnchor = QPoint(0, aBegRow);		// Select entire area.
		setCursor(QPoint(cTextDocument.eol(aY), aY), true/*Extend*/, false/*IncludeNewline*/, true/*SaveCur*/);
		aIsSelected = true;
	}
	return aIsSelected;
}

/*!
\brief text - Fetch the text in one row or all of the text editor

\param iRow - The row that contains the text
\param iorText - The text in the row is appended to iorText
\param iLgth - The current number of bytes in iorText
\return aLgth - The number of bytes in iorText on return
\par Notes:
-# If iRow is negative, all of the text is returned
 */
long ATextCursor::text(long iRow, QByteArray& iorText, long iLgth)
{
	QPoint aEot;
	if (iRow < 0)
	{	iRow = 0;
		aEot = cTextDocument.eot();
	}
	else
		aEot = QPoint(cTextDocument.cDocument[iRow]->mRowLgth, iRow);
	return cTextDocument.text(QPoint(0, iRow), aEot, iorText, iLgth);
}

/*!
\brief textLength - Fetch the number of chars in the text

\return The number of bytes in the text
\par Notes:
-# Tabs count as one byte.
 */
long ATextCursor::textLength()
{
	return cTextDocument.textLength();
}

/*!
\brief textRows - Fetch the number of rows in the text

\return The number of rows in the text.
\par Notes:
-# A newline-terminated line that wraps around spans two or more rows.
-# Every document has at least one (possibly empty) row.
 */
long ATextCursor::textRows()
{
	return cTextDocument.cDocument.count();
}

/*!
\brief truncateTop - delete all but the last iLeft rows from the text.

\param iLeft - Number of rows that are not deleted
\par Notes:
-# Clears the undo stack.
 */
long ATextCursor::truncateTop(long iLeft)
{
	long aDeleted = cTextDocument.truncateTop(iLeft);
	if (aDeleted > 0)
	{	long aX = cCursor.x();
		long aY = cCursor.y() - aDeleted;
		if (aY < 0)	aX = aY = 0;
		setCursor(QPoint(aX, aY), false/*ExtendSelect*/, false/*IncludeNewline*/, true/*SaveCur*/);
		clearUndo();
		cMark = true;
	}
	return aDeleted;
}

/*!
\brief undo - Reverse the last edit operation

\param iToMark  If true, undo up to including next marked operation; else, undo one operation.
\return aOk - True iff there is something to undo
\par Notes:
-# Undo operations reverse the last insert or delete.
-# Multiple undo operations are held in an undo stack.
 */
bool ATextCursor::undo(bool iToMark)
{
	bool aOk = true;
	// Fetch. Fetch undo operations until hit next undo operation.
	ATextEdit::ACmdType aCmdType;
	AUndoOp aOp;
	do
	{	aOp = cUndo.undo();
		if ((aCmdType = aOp.mCmd) != ATextEdit::eDelBack && aCmdType != ATextEdit::eDelFwd
		&& aCmdType != ATextEdit::eInsBack && aCmdType != ATextEdit::eInsFwd)
		{	qDebug("ATextCursor::undo, Bogus command, CmdType: %d", aCmdType);
			aOk = false;
			break;
		}
		// Undo. Undo the current operation.
		if (cCursor != aOp.mCursor)
			setCursor(aOp.mCursor, false/*ExtendSelect*/, false/*IncludeNewline*/, true/*SaveCur*/);
		if (aCmdType == ATextEdit::eDelBack || aCmdType == ATextEdit::eDelFwd)
			deleteIt((aCmdType == ATextEdit::eDelBack) ? -aOp.mLength : aOp.mLength);
		else
			insert(aCmdType, aOp.mText, aOp.mLength);
	} while (iToMark && !cUndo.mark());
	return aOk;
}
// end
