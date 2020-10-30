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

#ifndef AUNDO_H
#define AUNDO_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/aundo.h
															Undo

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0110	10/10/2006	tlw		cStack. Rename. Add. return aReverse.
1.0108	 9/29/2006	tlw		Add dump to display contents of the queue
1.0070	12/1/2005	tlw		Move undo operations to this class.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QList>

#include "atextedit.h"			// ATextEdit statics
//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
class AUndoOp
{
public:
	enum	AReverseOp {eNot, eRedo, eUndo};

	AUndoOp();
	AUndoOp(const AUndoOp& irOp);
	AUndoOp(ATextEdit::ACmdType iCmd, const QPoint irCursor, long iLength, bool iMark, AReverseOp iReverse
	, const QByteArray irText = QByteArray());
	AUndoOp&			operator=(const AUndoOp& irRhs);

	ATextEdit::ACmdType	mCmd;		// eNone, eDelFwd, eDelBack, eInsFwd, eInsBack
	QPoint				mCursor;	// Starting cursor position
	long				mLength;	// Length of delete or length of saved text
	bool				mMark;		// True iff first operation of a transaction
	AReverseOp			mReverse;	// Reverse the current add operation.
	QByteArray			mText;		// Deleted text iff a delete operation
};

typedef QList<AUndoOp*> AUndoList;

/*!
\brief AUndo implements an undo queue that saves delete and insert operations.

A transaction is a sequence of undo operations marked by setting the Mark flag in the first AUndoOp in the transaction sequence.
\par Notes:
-# For an empty undo list, cCurrent is 0 and the queue is empty.
-# The cursor is always just after the last undo operation.  Redo operations extend from Current to the end of the queue.
-# If there are no redo operations in the queue, the cursor is just past the last undo (cCurrent = size of the queue). 
-# On an insert/delete command, the corresponding delete/insert operation is appended to the queue at the cursor.
The cursor is incremented. Any redo operations are removed from the queue unless it is a save operation as noted below.
-# On an undo, the cursor is decremented, the saved operation is returned to the caller. Queue is not modified. Then the undo
operation generates another delete/insert which converts the entry at the cursor to the opposite operation.
-# On a redo, the operation at the cursor is performed. The cursor and the queue are not modified.  Then the redo operation
generates another save which converts the entry at the cursor to the opposite operation. The cursor is incremented.
 */
class AUndo
{
public:
	AUndo();
	~AUndo();
	bool		add(const AUndoOp& irOp);
	void		clear();
	QString		dump(long iPos);
	bool		mark();
	AUndoOp		redo();
	AUndoOp		undo();

private:
	AUndoOp		cNullOp;
	bool		dumpOperation(long iPos, QString& orText);
	long		cCurrent;
	AUndoList	cStack;
};
#endif		// AUNDO_H

