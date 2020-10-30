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

#ifndef ATEXTROW_H
#define ATEXTROW_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/atextrow.h
												Text Row

CHANGE HISTORY
Version	Date		Who		Change
3.2007	3/26/2008	fchua	find. Changed iP to output parameter iorP.
2.0003	2/5/2007	tlw		paint. Add top row number to arguments.
1.0116	11/27/2006	tlw		refresh. Add method to expand tabs if tabwidth changed
1.0116	11/27/2006	tlw		crViewWidth. Remove unused property.
1.0110	10/20/2006	tlw		find. Fix errors in find. deleteIt. Fix error when delete chars on last line
1.0109	10/8/2006	tlw		dumpRow. Added in scpStates, scpTxtBg, scpTxtFg
1.0108	9/29/2006	tlw		mid. Change signature.
1.0107	9/22/2006	tlw		scanBow. Add iSameWord argument
1.0070	11/5/2005	tlw		Move ARow to here.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QVarLengthArray>
// #include "aparameters.h"
class QByteArray;
class QPainter;
class ATextDocument;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
typedef QVarLengthArray<ushort, 64> ATextBfr;

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief ATextRow - Manages one row of a text document.

A text document is stored as a set of rows.  A line is stored as one or more rows with a newline at the end of the last row.
Every document has at least one newline-terminated row at the end.
\par Notes:
-# A row of characters consists of an array of ushort (16-bit unsigned) characters.
-# The least-significant 8 bits holds the
Latin1 character code(ISO 8859-1).  The most significant 4 bits holds an index into an array of background colors and the next
most significant 4 bits holds an index into an array of foreground colors.
-# Tabs are represented by the tab character 0x9 followed by zero or more Latin1 space characters that fill out the row to the
next tab stop.
 */
class ATextRow
{
public:
	ATextRow(ATextDocument* ipTextDocument);
	long		append(const QByteArray& irText, long iSrcX, long iSrcLgth);
	ushort		at(long iX);
	long		chop(long iX, QByteArray& iorText, long iLgth);
	ushort*		data();
	long		deleteIt(long iCh);
	long		deleteIt(long iX, long iLength, QByteArray& iorText, long iLgth);
	QString		dumpRow();
	long		find(const QByteArray& irPtn, long& iorP, bool iMatchCase, bool iDown, bool iMatchWord, long& iorX);
	bool		functionList(QString& orFcnName);
	long		matchParen(long iChClose, long iChOpen, bool iFwd, long iRefCount, long iX);
	long		mid(long iX, long iSpan, QByteArray& iorText, long iLgth);
	void		paint(long iLeft, long iRight, long iRow, QPoint& irOrigin, QPainter& irPainter);
	void		prepend(long iCh);
	bool		refresh();
	bool		rebalance();
	long		scan(long iX, long* iopLength);
	long		scanBegTab(long iX, bool iIncludeNewline);
	long		scanBow(bool iFwd, bool iSameWord, long iX);
	long		scanEow(bool iFwd, long iX);
	void		setBackground(long iBegX, long iEndX, bool iMatchedText, bool iSelectedText);
	AInState	setForeground(long iBegX, AInState iBegState = eSame);
	void		tabify(long iExtraSpaces, bool iToTabs);
	long		text(QByteArray& iorText, long iLgth);

	AInState	mBegState;		// Initial starting state.
	bool		mHasNewline;	// Row ends with a newline
	long		mRowLgth;		// Number of chars in cRow (may be less than cRow's size).

private:
	long		scanLines(long iNLines);

	ATextDocument*	cpTextDocument;	// Holds configurable parameters
	ATextBfr		cRow;			// Holds row of color/char elements in a dynamic array of unsigned 16-bit ints.
};
#endif		// ATEXTROW_H

