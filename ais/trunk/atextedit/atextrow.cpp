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
aisdev/atextedit/atextrow.cpp
														Text Row Specification

CHANGE HISTORY
Version	Date		Who		Change
3.2007	3/26/2008	fchua	find. Fixed bugs in handling partial matches.
3.2007	3/26/2008	fchua	find. Changed iP to output parameter iorP.
2.0004	 2/17/2007	tlw		Add quote characters to syntax highlighting.
2.0003	2/6/2007	tlw		Paint. Add a gutter with line numbers.
1.0116	11/27/2006	tlw		refresh. Add method to expand tabs if tabwidth changed
1.0116	11/27/2006	tlw		ATextRow. Remove unused irViewWidth. paint. Allow Latin1 codes above 0xac to be displayed.
1.0115	11/19/2006	tlw		append. Strip of CR if CRLF on end of a line.
1.0110	10/20/2006	tlw		find. Fix errors in find. deleteIt. Fix error when delete chars on last line
1.0109	10/ 5/2006	tlw		dumpRow. Added in scpStates, scpTxtBg, scpTxtFg
1.0107	9/22/2006	tlw		scanBow. Add iSameWord arg to avoid scan if already at beg-of-word.
1.0105	9/13/2006	tlw		deleteIt. Corrected returned RowLgth if row length did not change.
1.0070	11/5/2005	tlw		Move ADoc to a separate class.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <string.h>
#include <QtCore/QString>
#include <QtCore/QtDebug>
#include <QtCore/QByteArray>
#include <QtCore/QPoint>
#include <QtGui/QPainter>

#include "atextedit.h"		// For static defined constants
#include "atextdocument.h"	// For cpParameters, cGutterWidth
#include "atextrow.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief ATextRow - Constructor initializes the class properties.

\param ipParameters -> structure containing the configurable edit parameters.
\param irViewWidth - Reference to place that holds the current view width
\sa AParameters in aparameters.h
 */
ATextRow::ATextRow(ATextDocument *ipTextDocument)
: cpTextDocument(ipTextDocument)
{
	mHasNewline = false;
	mRowLgth = 0;						// Number of chars in cRow (may be less than cRow's size)
	mBegState = eStd;					// Initial starting state.
}

/*!
append - Add bytes to the end of a row

\param irText - Incoming text string.
\param iSrcX - Index into irText of the first char to be appended.
\param iSrcLgth - Number of chars in irText
\return aSize - number of chars appended
\par Notes:
-# Do not append to a newline-terminated row. The appended text may have, at most, one newline on the end.
-# Total length of current row + Text should be less than MaxRowWidth.
-# Tabs are expanded with spaces to next tab stop.
 */
long ATextRow::append(const QByteArray& irText, long iSrcX, long iSrcLgth)
{
	long aSize = 0;
	long aBfrInc = ATextEdit::scBfrInc;
	if (mHasNewline)
		qDebug("ATextRow:append(Text:%s,SrcX:%ld,SrcLgth:%ld), Append to newline", irText.data(), iSrcX,iSrcLgth);
	else if (iSrcLgth > 0)
	{	// Resize. Resize row buffer if it is not big enough to hold the text
		if ((aSize = cRow.size()) <= mRowLgth + iSrcLgth)
			cRow.resize((aSize = mRowLgth + iSrcLgth + aBfrInc) + 1);

		// Append. Tack Text onto the end of the existing row. Expand tabs
		long aTabWidth = cpTextDocument->cpParameters->mTabWidth;
		long aX = mRowLgth, aW;
		const char* apSrc = irText.data() + iSrcX, *apEol = apSrc + iSrcLgth;
		long aCh = '\0';
		ushort *apBfr = cRow.data(), *apDst = apBfr + mRowLgth, *apEnd = apBfr + aSize;
		while (apSrc < apEol)
		{	if ((aCh = (*apSrc++ & 0xff)) != ATextEdit::scLatin1Space)
			{	aW = (aCh == '\t') ? (aTabWidth - aX % aTabWidth) : 1;
				if (apDst + aW >= apEnd)
				{	// Resize. Resize if not enough room
					cRow.resize((aSize += aBfrInc) + 1);
					apBfr = cRow.data();
					apDst = apBfr + aX;
					apEnd = apBfr + aSize;
				}
				*apDst++ = aCh;
				aX += aW;
				// Expand tabs.
				if (aCh == '\t')
				{	for (; aW > 1; --aW)
						*apDst++ = ATextEdit::scLatin1Space;
				}
			}
		}
		// Newline. Note rows with a newline terminator
		if (aCh == '\n')
		{	mHasNewline = true;
			// Carriage Return. Strip off CRs.
			if (iSrcLgth > 1 && *(apDst -= 2) == '\r')
			{	*apDst = '\n';
				--aX;
			}
		}
		aSize = aX - mRowLgth;
		mRowLgth = aX;
	}
	return aSize;		// Width of the text appended.
}

/*!
\brief at - Fetch the character code at the specified offset into this row.

\param iX - Character offset into the row.
\return Character code at this offset
 */
ushort ATextRow::at(long iX)
{
	ushort aCh;
	if (mRowLgth == 0)
		aCh = 0;
	else
	{	if (iX >= mRowLgth)
			iX = mRowLgth - 1;
		aCh = *(cRow.data() + iX);
	}
	return aCh;
}

/*!
\brief chop - Remove the tail of the row and append it to iorText.

\param iX - Initial offset to the tail of this row.
\param iorText - Place to put the tail
\param iLgth - Initial length of iorText
\return Length of iorText on return
\par Note:
-# If iX is 0, the row will become an empty row.
 */
long ATextRow::chop(long iX, QByteArray& iorText, long iLgth)
{
	long aLgth = mRowLgth - iX, aW;
	if (iX < 0 || aLgth < 0 || iLgth < 0)
		qDebug("ATextRow:chop(X:%ld), Chop out of range", iX);
	else if (aLgth > 0)
	{	long aCh;
		long aTabWidth = cpTextDocument->cpParameters->mTabWidth;
		ushort *apSrc = cRow.data() + iX, *apEnd = apSrc + aLgth;
		// Tab. If in middle of tab, move back to the tab stop
		if ((*apSrc & 0xff) == ATextEdit::scLatin1Space)
		{	aW = iX % aTabWidth;
			iX -= aW;
			apSrc -= aW;
			aLgth += aW;
		}
		// Copy. Copy chars from tail into Text buffer.
		if (iorText.size() < iLgth + aLgth)
			iorText.resize(iLgth + aLgth);
		char* apBeg = iorText.data() + iLgth, *apDst = apBeg;
		while (apSrc < apEnd)
		{	if ((aCh = *apSrc++ & 0xff) != ATextEdit::scLatin1Space)
				*apDst++ = aCh;
		}
		*apDst = '\0';
		aLgth = apDst - apBeg;
		mRowLgth = iX;
		mHasNewline = false;
	}
	return iLgth + aLgth;
}

/*!
\brief data - Return pointer to the character data in this row.

\return -> to the first character of the text in this row.
 */
ushort* ATextRow::data()
{
	return cRow.data();
}

/*!
\brief deleteIt - Delete first char of row if it matches iCh.

\param iCh - Character to be deleted
\return aDelLgth -Number of chars deleted.
\par Note:
-# iCh is never a newline
 */
long ATextRow::deleteIt(long iCh)
{
	long aDelLgth = 0;
	ushort aCh;
	if (mRowLgth > 0 && (aCh = cRow[0] & 0xff) == iCh)
	{	long aTabWidth = cpTextDocument->cpParameters->mTabWidth, aW;
		long aW1 = aCh == '\t'? aTabWidth: 1;// Width of char being deleted
		long aX = 0;							// Offset to destination of move
		ushort aCw;							// Color-Character being moved
		ushort* apDst = cRow.data();		// -> destination of move
		ushort* apX = apDst + aW1;			// -> char being moved.
		ushort* apEnd = apDst + mRowLgth;	// -> just past end of chars being moved
		while (apX < apEnd)
		{	aCh = (aCw =*apX++) & 0xff;
			*apDst++ = aCw;
			// Expand tabs to next tab stop
			if (aCh == '\t')
			{	aX += (aW = aTabWidth - aX % aTabWidth);
				while (--aW > 0)
					*apDst++ = ATextEdit::scLatin1Space;
				while ((*apX & 0xff) == ATextEdit::scLatin1Space)
					++apX;
			}
			else
				++aX;
		}
		mRowLgth = aX;
		++aDelLgth;
	}
	return aDelLgth;
}

/*!
\brief deleteIt - Delete iLength chars from the row starting at offset iX in the row.

\param iX - Initial offset into the row.
\param iLength - Number of chars to be deleted.
\param iorDeletedText - The deleted text is appended to iorDeletedText
\param iDeletedLgth - Initial length of iorDeletedText
\return The final length of iorDeletedText.
\par Notes:
-# If delete starts in middle of tab, move offset to beg. of tab.
-# If delete ends in middle of a tab, extend delete to end of tab.
-# Do not leave an empty row in the list of rows.
 */
long ATextRow::deleteIt(long iX, long iLength, QByteArray& iorDeletedText, long iDeletedLgth)
{
	long aDelLgth = 0;
	if (iX < 0 || iLength <= 0)	
		qDebug("ATextRow:delete(X:%ld, Length:%ld), Delete out of range", iX, iLength);
	else
	{	long aCh;
		long aW;					// Distance to previous tab stop
		long aTabWidth = cpTextDocument->cpParameters->mTabWidth;
		ushort *apBfr = cRow.data(), *apBeg = apBfr + iX, *apEnd = apBeg + iLength, *apX;
		// Start. Move back to previous tab stop if starting in middle of tab.
		if ((*apBeg & 0xff) == ATextEdit::scLatin1Space)
		{	aW = iX % aTabWidth;
			apBeg -= aW, iX -= aW, iLength += aW;
		}
		// End. Extend delete to next tab stop if end is in middle of a tab.
		if ((*apEnd & 0xff) == ATextEdit::scLatin1Space)
		{	aW = aTabWidth - (iX + iLength) % aTabWidth;
			iLength += aW, apEnd += aW;
		}
		if (iX + iLength > mRowLgth)
		{	qDebug("ATextRow:delete(Offset:%ld, Length:%ld), Delete past end", iX, iLength);
			iLength = mRowLgth - iX;
			apEnd = apBeg + iLength;
		}
		// Resize. Expand text size if deleted chars won't fit in the space left.
		if (iorDeletedText.size() <= iDeletedLgth + iLength)
			iorDeletedText.resize(iDeletedLgth + iLength + 1);

		// Copy. Extract the deleted chars, omitting Latin1 spaces.
		char* apText = iorDeletedText.data() + iDeletedLgth, *apDst = apText;
		for (apX = apBeg; apX < apEnd; ++apX)
		{	if ((aCh = (*apX & 0xff)) != ATextEdit::scLatin1Space)
				*apDst++ = aCh;
		}
		aDelLgth = apDst - apText;
		*apDst = '\0';

		// Gap. Close up the gap being careful to adjust tabs.
		long aX = iX;				// Offset to destination of move
		ushort aCw;					// Color-Character being moved
		apX = apEnd;				// -> char being moved.
		apEnd = apBfr + mRowLgth;	// -> just past end of chars being moved
		while (apX < apEnd)
		{	aCh = (aCw =*apX++) & 0xff;
			*apBeg++ = aCw;
			// Expand tabs to next tab stop
			if (aCh == '\t')
			{	aX += (aW = aTabWidth - aX % aTabWidth);
				while (--aW > 0)
					*apBeg++ = ATextEdit::scLatin1Space;
				while ((*apX & 0xff) == ATextEdit::scLatin1Space)
					++apX;
			}
			else
				++aX;
		}
		mRowLgth = apBeg - apBfr;
		mHasNewline = (*(apBfr + mRowLgth - 1) & 0xff) == '\n';
	}
	return iDeletedLgth + aDelLgth;
}

/*!
\brief dumpRow - Return beg state, has newline, lgth, text for a row.

\return aMsg - Row information.
\par Notes:
-# Beg State identifiers: Comment1, Comment2, Std, Stg1, Stg2, InWord, Same
-# Background identifiers: Base,Highlighted,Matched
-# Foreground identifiers: Text,Highlighted,Keyword,Comment,String
 */
QString ATextRow::dumpRow()
{
	const char *apBg, *apPen;	// Background/foreground identifiers
	uchar aCh;
	ushort aCw, *apRow =  cRow.data(), *apEnd = apRow + mRowLgth;
	QString aMsg = QString("%1,%2,%3 \"").arg(ATextEdit::scpStates[mBegState]).arg(mHasNewline ? "N" : " ").arg(mRowLgth);
	QString aHex("\" :");
	while (apRow < apEnd)
	{	// Hex. Note background index, foreground index, character code
		aCw = *apRow++;
		aCh = aCw & 0xff;
		apPen = ATextEdit::scpTxtPen[(aCw & ATextEdit::scMaskPen) >> 8];
		apBg = ATextEdit::scpTxtBg[(aCw & ATextEdit::scMaskBg) >> 12];
		aHex += QString("%1%2%3 ").arg(apBg, apPen).arg(aCh, 2/*FieldWidth*/, 16/*Base*/);

		// Char. Translate newlines, tabs.
		switch (aCh = aCw & 0xff)
		{
		case '\0':		// Should not happen!
			aCh = '$';
			break;
		case '\n':
			aCh = ATextEdit::scCharNewline;
			break;
		case '\r':
			aCh = ATextEdit::scCarriageReturn;
			break;
		case '\t':
			aCh = ATextEdit::scCharTab;
			break;
		case 0xa0:
			aCh = '.';
			break;
		case 0x7f:
			aCh = ATextEdit::scCharDel;
			break;
		default:
			break;
		}
		aMsg += aCh;
	}
	return aMsg + aHex;
}

/*!
/brief find - Searches the text in the current row for a match to the pattern.

\param irPtn - Search pattern
\param iorP - Initial offset into the search pattern. On return, it maybe set to 0 iff the partial match was reset.
\param iMatchCase - True iff the search is case sensitive
\param iDown - True iff the search is from left-to-right
\param iMatchWord - True iff the matched text must start and end on a word boundry.
\param iorX - Starting offset into the text row. On return, it holds the beginning offset of the match.
\return aX - If a match, offset to end (beg if !iDown) of match. If partial match, minus the num. of chars in pattern that were
matched; else a large negative.
\par Note:
-# On a match and iDown, iorX is moved right to beg of matched text; else iorX is not changed.
-# On a match and !iDown, iorX is moved left to right end of matched text; else iorX is not changed.
 */
long ATextRow::find(const QByteArray& irPtn, long& iorP, bool iMatchCase, bool iDown, bool iMatchWord, long& iorX)
{
	uchar aCh = 0;							// Next char in the text
	long aPtnLgth = irPtn.length();		// Length of search pattern
	long aX = -aPtnLgth;					// If no match, return large negative.
	const char *apBegP, *apP, *apEndP;	// -> beg, middle, end of pattern
	ushort *apBfrX,*apBegX,*apX,*apEndX;// ->beg, middle, end of text.
	bool aInMatch = false;				// Part of the pattern is matched.

	// If iorX < 0, start at end of row.
	if (iorX < 0) iorX = mRowLgth;
	if (irPtn.isEmpty())	
		qDebug("ATextRow:find(Pattern:%s,Lgth:%ld,RowLgth:%ld), No search pattern", irPtn.data(), aPtnLgth, mRowLgth);
	else if (iDown)
	{	// In-word. Skip to eow if starting in a word
		apBfrX = cRow.data(), apBegX = apBfrX + iorX, apEndX = apBfrX + mRowLgth;
		if (iMatchWord && iorX > 0 && ATextEdit::scInWord[*(apBegX - 1) & 0xff])
		{	while (ATextEdit::scInWord[*apBegX & 0xff] && apBegX < apEndX)
				++apBegX;
		}
		// Padding. Skip over scLatin1Spaces
		while (apBegX < apEndX && (*apBegX & 0xff) == ATextEdit::scLatin1Space)
			++apBegX;

		// Search. apBegX -> beg of search area, apX-> end of search area. apBegP->beg of pattern, apP->into pattern
		apBegP = irPtn.data(), apP = apBegP + iorP, apEndP = apBegP + aPtnLgth;
		for (apX = apBegX; apP < apEndP && apX < apEndX;)
		{
			aCh = *apX & 0xff;
			// Case Insenstitive. Convert to lower case.
			if (!iMatchCase)
				aCh = ATextEdit::scLower[aCh];

			// Next match. If a match, check word boundaries.
			if (aCh == *apP)
			{	aInMatch = true;
				// Move to next character in search pattern.
				apP++;
				if (iMatchWord)
				{	// Beg Word Boundry. No match if first match and not at beg-of-line and prev char is a word-char.
					if (apX == apBegX && apBegX > apBfrX && ATextEdit::scInWord[*(apBegX - 1) & 0xff] )
						aInMatch = false;
					// End Word Boundry. No match if last match and not at end-of-line and next char is a word-char.
					else if (apP == apEndP && apX < apEndX && ATextEdit::scInWord[*(apX + 1) & 0xff])
						aInMatch = false;
				}
			}
			else
				aInMatch = false;

			// Match. Move to next character in search text
			if (aInMatch)
			{
				// Padding. Skip over scLatin1Spaces
				while (apX < apEndX && (*++apX & 0xff) == ATextEdit::scLatin1Space)
					;
			}
			else
			{
				// No match.
				apP = apBegP; // move to beginning of search pattern
				
				// if we are initially expecting a partial match, do not increment apBegX
				if (iorP > 0)
				{
					// No more partial matches at this point
					// Just move to the beginning of the search text
					iorP = 0;
				}
				else
				{
					// Move to next character in search text
					// Padding. Skip over scLatin1Spaces
					while (apBegX < apEndX && (*++apBegX & 0xff) == ATextEdit::scLatin1Space)
					;
				}

				apX = apBegX;
			}
		}
		// Match. Matched all or part of the pattern
		if (aInMatch)
		{	// Partial Match. Return negative of length of matched portion of the text.
			if (apP < apEndP && apX == apEndX)
				aX = -(apP - apBegP);
			else // Match. Return offset to end of matched text
				aX = apX - apBfrX;
			// Start. Set iorX to beg (left end) of match.
			iorX = apBegX - apBfrX;
		}
		// else, no match. Return large negative
	}
	else // Up. Search pattern from right to left starting at initial offset
	{	// In-word. Skip to bow if ending in a word
		apEndX = cRow.data(), apBegX = apEndX + iorX;
		if (iMatchWord && iorX < mRowLgth && ATextEdit::scInWord[*apBegX & 0xff])
		{	while (apBegX > apEndX && ATextEdit::scInWord[*(apBegX - 1) & 0xff])
				--apBegX;
		}
		// Search. apBegX -> right end of search area, apX-> search area. apBegP->right end of pattern, apP->into pattern
		apEndP = irPtn.data(), apBegP = apEndP + aPtnLgth, apP = apBegP - iorP;
		for (apX = apBegX; apP > apEndP && apX > apEndX;)
		{	// Padding. Back over scLatin1Spaces
			while (apX > apEndX && (aCh = *--apX & 0xff) == ATextEdit::scLatin1Space)
				;
			// Case Insenstitive. Convert to lower case.
			if (!iMatchCase)
				aCh = ATextEdit::scLower[aCh];

			// Next match. If a match, check word boundaries
			if (aCh == *--apP)
			{	aInMatch = true;
				if (iMatchWord)
				{	// End Word Boundry. No match if first match and not at end-of-line and next char is a word-char.
					if (apX == apBegX && aX < mRowLgth && ATextEdit::scInWord[*(apX + 1) & 0xff])
						aInMatch = false;
					// Beg Word Boundry. No match if last match and not at beg-of-line and prev char is a word-char.
					else if (apP == apEndP && apX > apEndX && ATextEdit::scInWord[*(apX - 1) & 0xff])
						aInMatch = false;
				}
			}
			else
				aInMatch = false;

			// No Match. Start over.
			if (!aInMatch)
			{	apP = apBegP; // move to beginning of search pattern
				if (iorP > 0)
					iorP = 0; // no more partial match at this point
				else
					apBegX--; // move to next character in search text
				apX = apBegX;
			}
		}
		if (aInMatch)
		{	// Partial Match. Return negative of length of matched portion of pattern.
			if (apP > apEndP && apX == apEndX)
				aX = apP - apBegP;
			else // Match. Return offset to left end of match.
				aX = apX - apEndX;
			// End. Set iorX to right end of match
			iorX = apBegX - apEndX;
		}
		// No match. Return large negative.
	}
	return aX;
}

/*!
\brief functionList - Return the function name if a function header found in this row.

\param orFcnName - Place to return the function name.
\return aMatch - True iff a match to the function header for this language
 */
bool ATextRow::functionList(QString& orFcnName)
{
	long aLgth = 0;
	bool aMatch = false;
	QString aFcnName;
	ALanguage& arLanguage = ATextEdit::scLanguages[cpTextDocument->cpParameters->mLanguage];
	QString aPattern(arLanguage.mFcnPattern);
	QRegExp aRegExp(aPattern, Qt::CaseSensitive);
	QByteArray aText;
	text(aText, aLgth);
	if (aRegExp.indexIn(QString(aText)) >= 0)
	{	orFcnName = aRegExp.cap(1);
		aMatch = true;
	}
	return aMatch;
}

/*!
\brief matchParen - Scan row for opening/closing bracket char until ref count goes to zero.

\param iChClose - Closing bracket character
\param iChOpen - Opening bracket character
\param iFwd - If true scan forward; else, scan backward
\param iRefCount - Number of matching brackets to go.
\param iX - Starting offset into row.
\return Offset in row where refCount goes to zero or -refCount if not zero.
 */
long ATextRow::matchParen(long iChClose, long iChOpen, bool iFwd, long iRefCount, long iX)
{
	long aCh, aCw, aX;
	ushort* apX = cRow.data() + iX;
	if (iFwd)
	{	for (aX = iX; aX < mRowLgth; ++aX)
		{	// Skip if in-comment or in-string
			if (((aCw = *apX++) & ATextEdit::scMaskPen) < ATextEdit::scPenComment)
			{	if ((aCh = aCw & ATextEdit::scMaskChar) == iChOpen)
					++iRefCount;
				else if (aCh == iChClose && --iRefCount == 0)
				{	++aX;
					break;
				}
			}
		}
	}
	else
	{	for (aX = iX; aX > 0; --aX)
		// Skip if in-comment or in-string
		if (((aCw = *--apX) & ATextEdit::scMaskPen) < ATextEdit::scPenComment)
		{	if ((aCh = aCw & ATextEdit::scMaskChar) == iChClose)
				++iRefCount;
			else if (aCh == iChOpen && --iRefCount == 0)
			{	--aX;
				break;
			}
		}
	}
	return (iRefCount == 0) ? aX : -iRefCount;
}

/*
\brief mid - Fetch a section of text.

\param iX - Starting offset into the row.
\param iSpan - Number of chars to scan
\param iorText - Section of the row is appended to iorText
\param iLgth - Initial length of iorText
\return aLgth - Length of iorText on return.
\par Notes:
-# If starting in middle of a tab, move back to previous tab stop.
-# iSpan includes padding for tabs, but aLgth only counts the chars included in iorText.
 */
long ATextRow::mid(long iX, long iSpan, QByteArray& iorText, long iLgth)
{
	long aCh;
	long aW;							// Width of a character
	long aLgth;						// Number of chars returned.
	long aTabWidth = cpTextDocument->cpParameters->mTabWidth;
	if (iorText.size() <= iLgth + iSpan)
		iorText.resize(iLgth + iSpan + 1);
	char* apText = iorText.data(), *apDst = apText + iLgth;

	// Align. If start in middle of a tab, move back to tab stop.
	ushort *apSrc = cRow.data() + iX, *apEnd = apSrc;
	if ((*apSrc & 0xff) == ATextEdit::scLatin1Space)
	{	aW = iX % aTabWidth;
		iX -= aW, iSpan += aW;
	}
	// Copy. Extract chars from row
	long aLeft = mRowLgth - iX;			// chars left in the row
	apEnd += (iSpan <= aLeft) ? iSpan : aLeft;
	while (apSrc < apEnd)
	{	if ((aCh = (*apSrc++ & 0xff)) != ATextEdit::scLatin1Space)
			*apDst++ = aCh;
	}
	aLgth = apDst - apText;
	*apDst = '\0';
	return aLgth;
}

/*!
\brief paint - Paint a rectangular portion of this row from left to right

Here's how this works.  The text document gets a paint event that specifies a rectangle in the viewport to be redrawn.
The x and the y offset from the upper left corner of the viewport is in irOrigin. The row is repainted from iLeft
until the right edge of the display area is reached or until the end-of-row is encountered.
\param iLeft - Offset [chars] of the left edge of text to be displayed.
\param iRight - Offset [chars] of the right edge or text to be displayed.
\param iTop - Row number of the top row.
\param irOrigin - X and Y offset [pixels] from left edge/top of viewport
\param irPainter - Painter for this viewport
\par Notes:
-# Every row knows the height of a row, the color associated with a character.
 */
void ATextRow::paint(long iLeft, long iRight, long iRow, QPoint& irOrigin, QPainter& irPainter)
{
	// Position. Set current position.
	ushort aCh;							// Current character
	long aX, aW;						// Column position, width of string
	AParameters* apParameters = cpTextDocument->cpParameters;
	long aCharWidth = apParameters->mCharWidth;
	long aRowHeight = apParameters->mLineHeight;
	long aBegX = irOrigin.x();			// X offset [pixels] from left edge of viewport to the first column of text.
	long aBegY = irOrigin.y();			// Y offset [pixels] from top edge of viewport to first row of text.
	long aAscent = apParameters->mLineOffset;// Offset of y from top of row in drawText [pixels]
	ushort aCw, *apCw = cRow.data(), *apEnd = apCw + mRowLgth;
	long aLeftOffset = apParameters->mLeftOffset; // Offset [pixels] of first char from left edge of viewport.
	QByteArray aText;					// Holds chars of the same color for display (add 1 for null)
	QBrush aTxtBrush;					// Background Brush for chars in Text buffer.
	aText.resize(iRight - iLeft);
	char* apTxtBfr = aText.data(), *apTxt = apTxtBfr;
	ushort aTxtColorIx = 0, aColorIx = 0;		// Color index of chars in Text, color index of next char.
	ushort aTxtBrushIx = 0, aTxtPenColorIx = 0;	// Brush/color index of chars in Text.

	// WhiteSpace.  Set the displayed char for special characters.
	bool aShowSpaces = apParameters->mShowWhiteSpace;
	uchar aCarriageReturn = ATextEdit::scCarriageReturn;
	uchar aCharDel = ATextEdit::scCharDel;
	uchar aCharNewline = (aShowSpaces) ? ATextEdit::scCharNewline : ' ';
	uchar aCharTab = (aShowSpaces) ? ATextEdit::scCharTab : ' ';

	// Gutter. Show the line number on the left.
	irPainter.setBackgroundMode(Qt::OpaqueMode);
	if (apParameters->mLineNumbers)
	{	irPainter.setBackground(ATextEdit::scBgBrushes[ATextEdit::scBgMatchText >> 12]);	// Grey background
		irPainter.setPen(ATextEdit::scPenColors[ATextEdit::scBgPlainText]);				// Black text
		QString aLineNum(QString("%1").arg(iRow + 1, cpTextDocument->cGutterWidth));	// Count from 1
		irPainter.drawText(aLeftOffset, aBegY + aAscent, aLineNum);
		aLeftOffset += (cpTextDocument->cGutterWidth + 1) * aCharWidth;		// Allow a space between gutter and text
	}
	// Paint. Paint the row
	for (aX = 0; apCw < apEnd && aX < iRight; ++aX)
	{	// Collect.  Collect text if at or past left edge of display area.
		aCw = *apCw++;
		if (aX >= iLeft)
		{	// Collect. Collect chars to be displayed.
			aColorIx = aCw & ATextEdit::scMaskDisplay;		// Mask = 0xf300 (omits string color)
			// Initialize. Set background brush and pen color after getting the first character.
			if (apTxt == apTxtBfr)
			{	aTxtColorIx = aColorIx;
				aTxtBrushIx = aColorIx & ATextEdit::scMaskBg;
				aTxtBrush = ATextEdit::scBgBrushes[aTxtBrushIx >> 12];
				irPainter.setBackground(aTxtBrush);
				aTxtPenColorIx = (aTxtBrushIx == ATextEdit::scBgSelText) ? ATextEdit::scPenSelText : aColorIx & ATextEdit::scMaskPen;
				irPainter.setPen(ATextEdit::scPenColors[aTxtPenColorIx >> 8]);
			}
			else if (aTxtColorIx != aColorIx)
			{	// Draw. Draw text in text buffer using the current text pen color
				*apTxt = '\0';
				aW = (apTxt - apTxtBfr) * aCharWidth;
				irPainter.fillRect(aBegX + aLeftOffset, aBegY, aW, aRowHeight, aTxtBrush);
				irPainter.drawText(aBegX + aLeftOffset, aBegY + aAscent, QString(aText));
				aBegX += aW;

				// Restart. Reset to beginning of text buffer, reset colors
				apTxt = apTxtBfr;
				aTxtColorIx = aColorIx;
				aTxtBrushIx = aColorIx & ATextEdit::scMaskBg;
				aTxtBrush = ATextEdit::scBgBrushes[aTxtBrushIx >> 12];
				irPainter.setBackground(aTxtBrush);
				aTxtPenColorIx = (aTxtBrushIx == ATextEdit::scBgSelText) ? ATextEdit::scPenSelText : aColorIx & ATextEdit::scMaskPen;
				irPainter.setPen(ATextEdit::scPenColors[aTxtPenColorIx >> 8]);
			}
			// Append. Append char to Text. Translate newlines, expand tabs. Use switch for performance
			switch (aCh = aCw & 0xff)
			{
			case '\0':		// Should not happen!
				break;
			case '\n':
				aCh = aCharNewline;
				break;
			case '\r':
				aCh = aCarriageReturn;
				break;
			case '\t':
				aCh = aCharTab;
				break;
			case 0xa0:
				aCh = ' ';
				break;
			case 0x7f:
				aCh = aCharDel;
				break;
			default:
				break;
			}
			*apTxt++ = aCh;
			*apTxt = '\0';
		}
	}
	// Flush. Display text left in aText.
	if (apTxt > apTxtBfr)
	{	aW = (apTxt - apTxtBfr) * aCharWidth;
		irPainter.fillRect(aBegX + aLeftOffset, aBegY, aW, aRowHeight, aTxtBrush);
		irPainter.drawText(aBegX + aLeftOffset, aBegY + aAscent, QString(aText));
	}
}

/*!
\brief prepend - Prepend iCh to row.

\param iCh - Character to be prepended.
\return void
\par Notes:
-# iCh is never a newline
 */
void ATextRow::prepend(long iCh)
{
	// Gap. Move whole row out of the way.
	long aGap = 4;		// Maximum gap needed to insert a char.
	if (cRow.size() < mRowLgth + aGap)
		cRow.resize(mRowLgth + aGap + ATextEdit::scBfrInc);
	ushort* apBfr = cRow.data();
	if (mRowLgth > 0)
		memmove(apBfr + aGap, apBfr, mRowLgth * 2);
	*(apBfr + --aGap) = iCh;

	// Insert. Insert char in front and then fill in gap
	long aCh;
	long aW;							// Width of a character.
	long aX = 0;						// Offset to destination of move
	ushort aCw;							// Color-Character being moved
	ushort* apDst = cRow.data();		// -> destination of move
	ushort* apX = apDst + aGap;			// -> char being moved.
	ushort* apEnd = apX + mRowLgth + 1;	// -> just past end of chars being moved
	long aTabWidth = cpTextDocument->cpParameters->mTabWidth;
	*apX = iCh;
	while (apX < apEnd)
	{	aCh = (aCw =*apX++) & 0xff;
		*apDst++ = aCw;
		// Expand tabs to next tab stop
		if (aCh == '\t')
		{	aX += (aW = aTabWidth - aX % aTabWidth);
			while (--aW > 0)
				*apDst++ = ATextEdit::scLatin1Space;
			while ((*apX & 0xff) == ATextEdit::scLatin1Space)
				++apX;
		}
		else
			++aX;
	}
	mRowLgth = apDst - apBfr;
}

/*!
\brief refresh - If line contains tabs, reset the tabs to the new length

\return aTab - true iff one or more tabs found
\par Notes:
-# It is best to only call this routine if the tab width has changed.
 */
bool ATextRow::refresh()
{
	bool aTab = false;
	ushort* apBfr = cRow.data();		// -> next char in the row
	ushort* apEnd = apBfr + mRowLgth;	// -> just past end of chars in row
	for (; apBfr < apEnd; ++apBfr)
	{	if ((*apBfr++ & 0xff) == '\t')
		{	aTab = true;
			break;
		}
	}
	// Extract chars from row and then append them back in with the correct spacing.
	if (aTab)
	{	QByteArray aText;
		long aLgth = chop(0/*X*/, aText, 0/*Lgth*/);
		append(aText, 0/*SrcX*/, aLgth);
	}
	return aTab;
}

/*!
\brief scan - Scan forward or backward in a row starting from offset iX

\param iX - Starting offset into the row
\param iopLength - On entry the distance [chars] to be scanned (see notes below)
\return aEndX - The X position of the char iopLength chars form iX
\par Notes:
-# A tab is counted as one character.  If starting in middle of a tab, move to the beginning of a tab.
-# If move is past end of row, returns chars to end of row.  If move is before beg of row, returns 0.
-# On exit iopLength is set to the number of characters left to be scanned (0 iff at end). 
 */
long ATextRow::scan(long iX, long* iopLength)
{
	long aEndX, aW;		// Final position in row, width of a character
	long aLength = *iopLength;
	if (iX <= 0 && aLength <= 0)
		aEndX = 0, aLength = 0;
	else if (iX >= mRowLgth && aLength >= 0)
		aEndX = mRowLgth;
	else if (aLength == 0)
		aEndX = iX;
	else
	{	// Tab Stop. If in middle of a tab, move back to tab stop.
		long aTabWidth = cpTextDocument->cpParameters->mTabWidth;
		ushort *apBfr = cRow.data(), *apX = apBfr + iX, *apEnd = apBfr + mRowLgth;
		if ((*apX & 0xff) == ATextEdit::scLatin1Space)
		{	aW = iX % aTabWidth;
			iX -= aW;
			apX -= aW;
		}
		// Scan Forward. Set EndX to last position in row if hit eol.
		if (aLength > 0)
		{	for (; aLength > 0 && apX < apEnd; --aLength)
			{	// Tab. If a tab, move to next tab stop.
				if ((*apX & 0xff) == '\t')
				{	aW = aTabWidth - iX % aTabWidth;
					iX += aW;
					apX += aW;
				}
				else // Next char.
				{	++iX;
					++apX;
				}
			}
			aEndX = iX;
		}
		else // Scan Back. Set EndX to if 0 if hit bol
		{	for (; aLength < 0 && apX > apBfr; --iX)
			{	if ((*--apX & 0xff) != ATextEdit::scLatin1Space)
					++aLength;
			}
			aEndX = iX;
		}
	}
	*iopLength = aLength;
	return aEndX;
}

// scanBegTab - if in a tab, move back to beg-of-tab, else return iX
// If at or past eol, move back to eol or eol-1 if past a newline and IncludeNewline is false
long ATextRow::scanBegTab(long iX, bool iIncludeNewline)
{
	ushort *apX = cRow.data() + iX;
	if (iX >= mRowLgth)
		iX = (mHasNewline && !iIncludeNewline) ? mRowLgth - 1 : mRowLgth;
	else
	{	while ((*apX & 0xff) == ATextEdit::scLatin1Space)
			--apX, --iX;
	}
	return iX;
}

/*!
\brief scanBow - scan forward/backward to beg-of-word on this row

\param iFwd - Scan forward if true; else, scan backward
\param iSameWord - If already at bow, do not move. 
\param iX - Beginning offset into the row
\return aX - Offset into row of the char at beg-of-word
\par Notes:
-# Returns -1 if no beg-of-word found.
-# If scan back to 0 and in-a-word, return 0.
-# If starting at bow, scan to next/prev bow unless iSameWord is true.
 */
long ATextRow::scanBow(bool iFwd, bool iSameWord, long iX)
{
	bool aInWord, aInNext;
	long aX = iX;			// Current offset
	ushort *apX = cRow.data() + iX;
	if (iFwd)
	{	// Start. If !iSameWord force one move; else, set aInWord based on previous char.
		aInWord = true;
		if (iSameWord)
			aInWord = (iX > 0) ? ATextEdit::scInWord[*(apX - 1) & 0xff] > 0 : false;

		// Scan. Move forward until transition from !in-a-word to in-a-word
		for (; aX < mRowLgth; ++aX)
		{	aInNext = (ATextEdit::scInWord[*apX++ & 0xff] > 0);
			if (!aInWord && aInNext)
				break;
			aInWord = aInNext;
		}
		if (aX == mRowLgth)
			aX = -1;
	}
	else
	{	// Start. If !iSameWord, force one move; else, set aInWord based upon current char.
		aInWord = false;
		if (iSameWord)
			aInWord = ATextEdit::scInWord[*apX & 0xff] > 0;

		// Scan. Move back until transition from in-a-word to !in-a-word
		for (; aX > 0; --aX)
		{	aInNext = (ATextEdit::scInWord[*--apX & 0xff] > 0);
			if (aInWord && !aInNext)
				break;
			aInWord = aInNext;
		}
		if (aX == 0 && !aInWord)
			aX = -1;
	}
	return aX;
}

/*!
\brief scanEow - scan forward/back to end-of-word on this row

\param iFwd - Scan forward if true; else, scan backward
\param iX - Beginning offset into the row
\return aX - Offset into row of the char at end-of-word
\par Notes:
-# If already at eow, move to the next eow.
-# At end of a row: if inWord return mRowLgth; else return -1.
Implies that a line break constitutes an end-of-word even if word continues on the next row.
-# Returns -1 if no end-of-word found
 */
long ATextRow::scanEow(bool iFwd, long iX)
{
	bool aInWord, aInNext;
	long aX = iX;			// Current offset
	ushort *apX = cRow.data() + aX;
	if (iFwd)
	{	// Scan forward until go from in-a-word to !in-a-word
		aInWord = false;
		for (; aX < mRowLgth; ++aX)
		{	aInNext = (ATextEdit::scInWord[*apX++ & 0xff] > 0);
			if (aInWord && !aInNext)
				break;
			aInWord = aInNext;
		}
		if (aX == mRowLgth && !aInWord)
			aX = -1;
	}
	else
	{	// First, scan back until go from !in-a-word to in-a-word
		aInWord = true;
		for (; aX > 0; --aX)
		{	aInNext = (ATextEdit::scInWord[*--apX & 0xff] > 0);
			if (!aInWord && aInNext)
				break;
			aInWord = aInNext;
		}
		if (aX == 0)
			aX = -1;
	}
	return aX;
}

/*!
\brief setBackground - Set/remove highlighted text from section of this row.

\param iBegX - Starting offset into this row.
\param iEndX - Just past last char to be modified.
\param iMatchedText - Iff true, use color for matched text (light gray).
\param iSelectedText - Iff true, use highlight color (medium blue)
\par Notes:
-# Only sets scBgSelText in each char. The scPenSelText is added to the char in paint
-# iMatchedText and iSelectedText are never both true.
-# If both iMatchedText and iSelectedText are false, the background is set back to normal text (white)
 */
void ATextRow::setBackground(long iBegX, long iEndX, bool iMatchedText, bool iSelectedText)
{
	long aBgColor = (iMatchedText) ? ATextEdit::scBgMatchText : (iSelectedText) ? ATextEdit::scBgSelText : 0;
	if (iEndX > mRowLgth) iEndX = mRowLgth;
	ushort *apBfr = cRow.data(), *apX = apBfr + iBegX, *apEnd = apBfr + iEndX;
	while (apX < apEnd)
    {
		*apX = aBgColor | (*apX & 0xfff);		// ~ATextEdit::scMaskBg
        apX++;
    }
}

/*!
\brief setForeground - Set the foreground color based upon the language and the current state.

\param iBegX - Starting offset into the row
\param iBegState - Initial state of the text (eComment1, eComment2, eStd, eString1, eString2, eInWord)
\return aLastState - The state just past the last character in the row.
\par Notes:
-# See atextstaticdefs.h for more info on syntax highlighting.
-# For better performance, only the delimiters used in the current languages are checked. If a new delimiter is added, this code
must be changed.
 */
AInState ATextRow::setForeground(long iBegX, AInState iBegState)
{
    Q_UNUSED(iBegX);
    Q_UNUSED(iBegState);

	QByteArray aWord;
	long aLanguage = cpTextDocument->cpParameters->mLanguage, aLgth;
	ALanguage& arLang = ATextEdit::scLanguages[aLanguage];
	ATextEdit::AChType aChType;
	ushort aCh, aLastCh = 0, aColorX;
	ushort *apBfr = cRow.data(), *apX = apBfr, *apEnd = apBfr + mRowLgth, *apWord;
	AInState aLastState = mBegState, aState = mBegState;
	for (; apX < apEnd; ++apX, aLastCh = aCh, aLastState = aState)
	{	if (aLanguage <= 0)
			aCh = *apX & ATextEdit::scMaskChar;
		else
		{	// Determine the AChType (eAlpha, eSep, eQuote1, eQuote2, eBeg1, eEnd1, eBeg2, eEnd2)
			aCh = *apX & ATextEdit::scMaskChar;
			aChType = ATextEdit::eSep;
			switch (aCh)
			{ // Quote1 chars.  Process quote
			case '"':
				if (aCh == arLang.mQuote1 && (aLastCh == '\0' || aLastCh != arLang.mEscape))
					aChType = ATextEdit::eQuote1;
				break;
			// Quote2 chars. Process apostrophe, braces
			case '\'':
				if (aCh == arLang.mQuote2 && (arLang.mEscape == '\0' || aLastCh != arLang.mEscape))
						aChType = ATextEdit::eQuote2;
				break;
			case '{':
				if (aCh == arLang.mQuote2)
					aChType = ATextEdit::eQuote2;		// No escaping this delimiter.
				break;
			case '}': // We should expand Quote2 to contain a beg char and an end char. For now, we cheat.
				if (arLang.mQuote2 == '{')
					aChType = ATextEdit::eQuote2;		// No escaping this delimiter.
				break;
			// Beg1 chars. Process ;, //
			case ';':
				if (aCh == arLang.mBeg1[0])
					aChType = ATextEdit::eBeg1;
				break;
			case '/':
				if(aLastCh != '\0' && aLastCh == arLang.mBeg1[0] && aCh == arLang.mBeg1[1])
					aChType = ATextEdit::eBeg1;
			// End2 chars. // Process */, ->
				if (aLastCh != '\0' && aLastCh == arLang.mEnd2[0] && aCh == arLang.mEnd2[1])
					aChType = ATextEdit::eEnd2;
				break;
			case '>':
				if (aLastCh != '\0' && aLastCh == arLang.mEnd2[0] && aCh == arLang.mEnd2[1])
					aChType = ATextEdit::eEnd2; 
				break;
			// Beg2 chars. Process /*, <!
			case '*':		// /*
				if (aLastCh != '\0' && aLastCh == arLang.mBeg2[0] && aCh == arLang.mBeg2[1])
					aChType = ATextEdit::eBeg2;
				break;
			case '!':
				if (aLastCh != '\0' && aLastCh == arLang.mBeg2[0] && aCh == arLang.mBeg2[1])
					aChType = ATextEdit::eBeg2;
				break;
			// End1 chars. \n
			case '\n':
				if (aCh == arLang.mEnd1[0] && (aLastCh == '\0' || aLastCh != arLang.mEscape))
					aChType = ATextEdit::eEnd1;
				break;
			// Alpha chars.
			default:
				if (ATextEdit::scInWord[aCh] > 0)
					aChType = ATextEdit::eAlpha;
				break;
			}
			// Determine next state (eComment1, eComment2, eStd, eString1, eString2, eInWord). Update current word.
			if ((aState = (AInState)ATextEdit::scNextState[aLastState][aChType]) == eInWord)
				aWord += aCh;

			// Set some transition colors and adjust some previous colors.
			aColorX = 0;						// Default color
			if (aLastState != aState)
			{	// Leaving a word
				if (aLastState == eInWord)
				{	// If a keyword, reach back and set keyword color
					if (arLang.mKeywords.contains(aWord))
					{	aLgth = aWord.length();
						// Reach back into row and set keyword color.
						apWord = apX;
						for (apX -= aLgth; apX < apWord; ++apX)
							*apX = ATextEdit::scPenKeyword | (*apX & ~ATextEdit::scMaskPen);
					}
					aWord = "";
				}
				// Entering a comment
				if (aState == eComment1 || aState == eComment2)
				{	if (aCh != ';' && apX > apBfr)
						*(apX - 1) = ATextEdit::scPenComment | (*(apX - 1) & ~ATextEdit::scMaskPen);
				}
				// Leaving a comment
				else if (aLastState == eComment1 || aLastState == eComment2)
					aColorX = ATextEdit::scPenComment;
				// Leaving a string
				else if (aState == eString1 || aState == eString2)
					aColorX = ATextEdit::scPenString;
			}
			// Set the current color index
			*apX &= ~ATextEdit::scMaskPen;
			if (aColorX == 0)
			{	if (aState == eComment1 || aState == eComment2)
					aColorX = ATextEdit::scPenComment;
				else if (aState == eString1 || aState == eString2)
					aColorX = ATextEdit::scPenString;
			}
			if (aColorX != 0)
			{	*apX |= aColorX;
				if ((aColorX & 0xf000) >= 0x3000)
					qDebug("ATextRow::setForeground, Color out of range, Color=%x", aColorX);
			}
		}
	}
	return aLastState;
}

/*!
\brief tabify - Convert leading spaces/tabs in the selected rows into tabs or vice versa.

\param iExtraSpaces - -1, delete leftover spaces; 0, keep leftover spaces; +1 convert leftover spaces to tabs
\param iToTabs - If true, convert to spaces to tabs; else, convert tabs to spaces
\return void
\par Notes:
-# If converting tabs to spaces and iExtraSpaces > 0, just keep leftover spaces.
 */
void ATextRow::tabify(long iExtraSpaces, bool iToTabs)
{
	bool aHasSpaces = false, aHasTabs = false;
	ushort *apBfr = cRow.data(), *apBeg, aCw, *apDst = apBfr, *apEnd = apBfr + mRowLgth, *apSrc;
	long aExtra, aTabs, aTabWidth = cpTextDocument->cpParameters->mTabWidth,  aWhiteSpace = 0, aW, aX;
	// Whitespace.  Determine the amount of leading whitespace. apBfr -> bol, apDst -> apBfr, apEnd -> eol
	ushort aColor = *apBfr & ATextEdit::scMaskColor;
	for (; apDst < apEnd && ((aCw = *apDst & ATextEdit::scMaskChar) == ' ' || aCw == ATextEdit::scLatin1Space || aCw == '\t'); ++apDst)
	{	if (aCw == ' ')
			aHasSpaces = true;
		else if (aCw == '\t')
			aHasTabs = true;
		++aWhiteSpace;
	}
	// To Spaces. Convert leading whitespace to all spaces.
	aTabs = aWhiteSpace / aTabWidth;
	aExtra = aWhiteSpace % aTabWidth;
	 if (!iToTabs)
	{	// Delete. If iExtraSpaces < 0, don't include leading extra spaces.
		apSrc = apBfr + aWhiteSpace;
		if (iExtraSpaces < 0 && aExtra > 0) aWhiteSpace = aTabs * aTabWidth;
		apEnd = apBfr + aWhiteSpace;
		for (aX = 0, apDst = apBfr; apDst < apEnd; ++apDst, ++aX)
			*apDst = aColor | ' ';
		// Shift. Shift line left, being careful to expand tabs to the next tab stop.
		if (apDst < apSrc)
		{	for (apEnd = apBfr + mRowLgth; apSrc < apEnd; ++apSrc)
			{	if ((aCw = *apSrc & ATextEdit::scMaskChar) != ATextEdit::scLatin1Space)
				{	*apDst++ = *apSrc;
					if (aCw == '\t')
						for (aW = aTabWidth - aX % aTabWidth; aW > 1; --aW, ++aX)
							*apDst++ = aColor | ATextEdit::scLatin1Space;
					++aX;
				}
			}
			mRowLgth = apDst - apBfr;
		}
	}
	// To Tabs. Convert leading whitespace to tabs.
	else if (aHasSpaces)
	{	// Extra Spaces. Adjust extra spaces according to iExtraSpaces.
		if (iExtraSpaces != 0 && aExtra > 0)
		{	aExtra = 0;
			if (iExtraSpaces > 0) ++aTabs;
		}
		// New Whitespace. Fill temp bfr with leading white space.
		ATextBfr aTmpBfr;
		aTmpBfr.resize(mRowLgth + aTabWidth);
		apDst = apBeg = aTmpBfr.data();
		for (; aTabs > 0; --aTabs)
		{	*apDst++ = aColor | '\t';
			for (aW = 1; aW < aTabWidth; ++aW)
				*apDst++ = aColor | ATextEdit::scLatin1Space;
		}
		for (; aExtra > 0; --aExtra)
			*apDst++ = aColor | ' ';
		aX = apDst - apBeg;

		// Append. Append the rest of this line, being careful to expand tabs to the next tab stop.
		for (apSrc = apBfr + aWhiteSpace; apSrc < apEnd; ++apSrc)
		{	if ((aCw = *apSrc & ATextEdit::scMaskChar) != ATextEdit::scLatin1Space)
			{	*apDst++ = *apSrc;
				if (aCw == '\t')
					for (aW = aTabWidth - aX % aTabWidth; aW > 1; --aW, ++aX)
						*apDst++ = aColor | ATextEdit::scLatin1Space;
				++aX;
			}
		}
		// Copy. Copy TmpBfr into current line.
		cRow = aTmpBfr;
		mRowLgth = apDst - apBeg;
	}
}

/*!
\brief text - Fetch the text from this row

\param iorText - The text from this row is appended to iorText
\param iLgth - The initial size of the iorText
\return Length of iorText after the row of text is appended
\par Notes:
-# iorText does not include padding for tabs.
-# Returned length may be less than iLgth if this row contains tabs.
 */
long ATextRow::text(QByteArray& iorText, long iLgth)
{
	return mid(0, mRowLgth, iorText, iLgth);
}

// end

