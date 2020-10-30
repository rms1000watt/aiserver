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
aisdev/atextedit/aundo.cpp
														Undo

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0110	10/10/2006	tlw		Mark. Only mark first added operation after a move.
1.0108	 9/29/2006	tlw		Add dump method.
1.0070	12/1/2005	tlw		Add a separate undo class
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "aundo.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
AUndoOp::AUndoOp()
: mCmd(ATextEdit::eNone), mCursor(QPoint(0,0)), mLength(0), mMark(true), mReverse(AUndoOp::eNot), mText(QByteArray())
{
}

AUndoOp::AUndoOp(const AUndoOp& irOp)
: mCmd(irOp.mCmd), mCursor(irOp.mCursor), mLength(irOp.mLength), mMark(irOp.mMark), mReverse(irOp.mReverse), mText(irOp.mText)
{
}

AUndoOp::AUndoOp(ATextEdit::ACmdType iCmd,const QPoint irCursor,long iLength,bool iMark,AReverseOp iReverse,const QByteArray irText)
: mCmd(iCmd), mCursor(irCursor), mLength(iLength), mMark(iMark), mReverse(iReverse), mText(irText)
{
}

AUndoOp& AUndoOp::operator=(const AUndoOp& irRhs)
{
	mCmd = irRhs.mCmd;
	mCursor = irRhs.mCursor;
	mLength = irRhs.mLength;
	mMark = irRhs.mMark;
	mReverse = irRhs.mReverse;
	mText = irRhs.mText;
	return *this;
}

/*!
\brief AUndo - Constructor sets the class properties
\return void
 */
AUndo::AUndo()
: cNullOp(ATextEdit::eNone, QPoint(-1, -1), 0, true, AUndoOp::eNot, QByteArray())
{
	cCurrent = 0;
}

AUndo::~AUndo()
{
	long aCount = cStack.count();
	cCurrent = 0;
	for (; cCurrent < aCount; --aCount)
		delete cStack.takeLast();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AUndo()");
#endif
}

/*!
\brief add - Append an operation to the stack that will undo/redo the insert or delete applied to the text

\param irOp - Undo operation to be added.
\return aMark - Mark for next operation.  Set next added operation to false. If cursor is moved before the next undo op is added,
the mark will be changed to true by setCursor.
\par Notes:
-# Mark is set to false.  Unless chain is broken by moving the cursor, every added operation is not marked.
-# The mark is not changed on reversed operations.
mark.
 */
bool AUndo::add(const AUndoOp& irOp)
{
	long aCount = cStack.count();
	AUndoOp* apOp;
	bool aMark = false;			// Next mark
	AUndoOp::AReverseOp aRevOp;

	// Lookup. Save if adding to end or if the current mReverse is set to eNot.
	if (cCurrent >= aCount)
	{	apOp = NULL;
		aRevOp = AUndoOp::eNot;
	}
	else
	{	apOp = cStack[cCurrent];
		aRevOp = apOp->mReverse;
	}
	// Save. Save the operation.
	if (aRevOp == AUndoOp::eNot)
	{	// Truncate. Remove any lingering redo operations.
		for (; cCurrent < aCount; --aCount)
			delete cStack.takeLast();
		// Push. Push this operation onto the stack. Increment current position.
		apOp = new AUndoOp(irOp);
		cStack += apOp;
		++cCurrent;
	}
	else
	{	// Reverse. Reverse undo/redo operation on the undo stack (but not the mark).
		apOp->mCmd = irOp.mCmd;
		apOp->mCursor = irOp.mCursor;
		apOp->mLength = irOp.mLength;
		apOp->mReverse = AUndoOp::eNot;
		apOp->mText = irOp.mText;

		// Post-redo.  Now that current operation is reversed, post increment current.
		if (aRevOp == AUndoOp::eRedo)
			++cCurrent;
	}
	return aMark;
}

/*!
\brief clear - Remove all pending operations from the stack.

\return void
 */
void AUndo::clear()
{
	cCurrent = 0;
	cStack.clear();
}

/*!
\brief dump - Dump one or more operations

\param iPos Offset of operation in undo stack relative to the current position
\return aDump (cmd, x, y, length, mark)<-(...)->(...)
\par Notes:
-# If iPos is large negative, dump the entire stack.
-# If the stack is empty, an empty string is returned
-# Cmd: None, DelFwd, DelBack, InsFwd, InsBack
-# Cursor: x, y
-# Length: length of insert or delete.
-# Mark: M if marked, m if not marked
 */
QString AUndo::dump(long iPos)
{
	long aCount = cStack.count();	// Number of items on queue (0 if empty)
	QString aDump;
	long aPos;
	if (aCount > 0)
	{	if (iPos < -aCount)
		{	// Dump entire undo stack.
			for (aPos = 0; aPos < aCount; ++aPos)
			{	if (aPos == cCurrent)
				{	if (aPos > 0)
						aDump += "<-";
					aDump += QString::number(cCurrent) + "->";
				}
				dumpOperation(aPos - cCurrent, aDump);
				if (aPos < aCount - 1)
					aDump += ',';
			}
			if (aPos == cCurrent)
				aDump += "<-" + QString::number(cCurrent);
		}
		else // Dump one row.
			dumpOperation(iPos, aDump);
	}
	return aDump;
}

/*
dumpOperation - Dump an operation on the stack
Args:
iPos - offset into stack relative to cCurrent
orText - Place to hold the Undo operation information
Returns: void
Note:
 1. Format: (cmd,x,y,length,mark)
 2. See dump for more details
 */
bool AUndo::dumpOperation(long iPos, QString& orText)
{
	bool aOk = true;
	long aPos = iPos + cCurrent;
	if (aPos >= 0 && aPos < cStack.count())
	{	AUndoOp* apOp = cStack[cCurrent + iPos];
		QPoint& arCur = apOp->mCursor;
		QString aCmd(ATextEdit::scpUndoTypes[apOp->mCmd]);
		orText += QString("(%1,%2,%3,%4,%5)").arg(aCmd).arg(arCur.x()).arg(arCur.y()).arg(apOp->mLength).arg(apOp->mMark?'M':'m');
	}
	else
		aOk = false;
	return aOk;
}

/*!
\brief mark - Return the mark of the next undo/redo operation.

\return aMark True if the current operation is marked or if at end of stack.
\par Notes:
-# Returns true if at either end of stack.
 */
bool AUndo::mark()
{
	return  (cCurrent >= 0 && cCurrent < cStack.count()) ? cStack[cCurrent]->mMark : true;
}

/*!
\brief nullUndoOp - Return a null operation.

A null operation has an eNone command and an invalid cursor position.
\return void
\par Notes:
-# A nullUndoOp is returned when there are no  undo/redo operations in the stack.
 * /
AUndoOp AUndo::nullUndoOp()
{
	return AUndoOp(ATextEdit::eNone, QPoint(-1, -1), 0, true, QByteArray());
} */

/*!
\brief redo - Returns the next redo operation to reapply the last undo operation.

\return void
\par Note:
-# If cCurrent points past the last item on the stack, there is nothing left to redo.
 */
AUndoOp AUndo::redo()
{
	// Nada. Nothing left to redo
	AUndoOp* apOp;
	long aCount = cStack.count();
	if (cCurrent >= aCount)
		apOp = &cNullOp;
	else // Current. Return current item. Post-increment current after add reverses the current operation.
	{	apOp = cStack[cCurrent];
		apOp->mReverse = AUndoOp::eRedo;
	}
	return *apOp;
}

/*!
\brief undo - Fetches the last undo operation to undo the previous insert or delete.

\return aOp - The last undo operation in the stack. 
\par Notes:
-# Returns a null operation if no undo operations are left in stack.
*/
AUndoOp AUndo::undo()
{
	// Nada. Nothing left to undo
	AUndoOp* apOp;
	if (cCurrent <= 0)
		apOp = &cNullOp;
	else // Decrement current and return item.
	{	apOp = cStack[--cCurrent];
		apOp->mReverse = AUndoOp::eUndo;
	}
	return *apOp;
}

// end
