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

#ifndef APARAMETERS_H
#define APARAMETERS_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/aparameters.h
													Text Edit Configurable Parameters
The set of configurable parameters for each instance of the editor.

CHANGE HISTORY
Version	Date		Who		Change
2.0003	2/5/2007	tlw		Add mLineNumbers.
1.0060	11/23/2005	tlw		Move all configurable parameters.
												---------------------------------

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
typedef struct
{
	long		mBottomMargin;		// Vertical margin at bottom of viewport [lines]
	long		mCharWidth;			// Width of a char 8[pixels] (based on font size)
	long		mCursorHeight;		// Height of blinking cursor [pixels]
	long		mCursorWidth;		// Width of blinking cursor [pixels]
	QString		mFont;				// Font family 
	long		mFontSize;			// Font size 10[points] (72 pts / inch)
	long		mLanguage;			// Index into scLanguages for current language
	bool		mLineNumbers;		// Show line numbers.
	long		mLeftOffset;		// Offset of first char in row from left edge of row in drawText (2 pixels).
	long		mLineHeight;		// Line height 16[pixels] (based on font size)
	long		mLineOffset;		// Line Offset 11[pixels] of y from top of row in drawText
	long		mMaxRowWidth;		// Maximum display width if no wrap [chars] (128 - 4096)
	long		mMaxRows;			// Maximum number of rows in edit buffer
	long		mMaxUndoDepth;		// Maximum depth of the undo stack
	bool		mMatchParen;		// Highlight text between matching pair of brackets
	long		mRightMargin;		// Horizontal margin on right of viewport [chars]
	bool		mShowWhiteSpace;	// Display tabs, newlines, carriage returns.
	long		mTabWidth;			// Default tab width [char]
	bool		mWrap;				// Enable word wrap
} AParameters;
#endif	// APARAMETERS_H
