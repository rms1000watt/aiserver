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

#ifndef ATEXTEDIT_H
#define ATEXTEDIT_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/atextedit.h
														Text Editor

CHANGE HISTORY
Version	Date		Who		Change
3.2008	 4/08/2008	fchua	Added createPrefDialog.
3.2008	 4/08/2008	fchua	Removed cSettings member variable.
3.2006	 3/10/2008	fchua	Added static functions for creating Find, Replace, Go and Function List dialogs.
3.2006	 3/10/2008	fchua	Removed association with Find, Replace, Go and Function List dialogs.
2.0004	 2/17/2007	tlw		Add quote characters to syntax highlighting.
2.0003	2/8/2007	tlw		dialogHelpAlpha. Alphabetical list of edit commands.
1.0118	12/12/2006	tlw		replace. Eliminate iSelect argument.
1.0116	11/27/2006	tlw		MoveTo. Add eNextBol if at end of wrapped line.
1.0110	10/13/2006	tlw		CursorStack. Enable/disable cursor stack signal.
1.0110	10/12/2006	tlw		ACmdType. Remove eUndo, eRedo. editClearUndo. Add to API.
1.0109	10/5/2006	tlw		dumpRow. Added in scpStates, scpTxtBg, scpTxtFg
1.0107	9/20/2006	tlw		editMove. Implement editMove. Add editWord. Add eNextSow, ePrevSow.
1.0105	9/13/2006	tlw		UpdateCursor. Add MoveToTop argument.
1.0060	5/ 3/2005	tlw		Combine initParameters, readIniFile and getScreenSize into one routine, init.
1.0057	3/18/2005	tlw		Update documentation
1.0036	3/22/2004	tlw		Encapsulate display position in cursors
1.0035	2/17/2004	tlw		Add mNLines to every row.
												--------------- ---------------
NOTES
 1. See atextstaticdefs.h for more info on language definitions.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <limits.h>
#include <QtGui/QAbstractScrollArea>
#include <QtCore/QSettings>
#include <QtCore/QMap>
class QTimer;

#include "aparameters.h"
class AHelpDialog;
class AHelpFindDialog;
class ATabifyDialog;
class ATabWidthDialog;
class ATestDialog;
class ATextCursor;
class AOperationsDialog;
class APrefDialog;

//	---------------------------------------------- CONFIGURABLE PARAMETERS ----------------------------------------------------
// Static defined constants are set in langdef.h
// Configurable parameters are declared in AParameters.

//	---------------------------------------------------- DEFINITIONS	-------------------------------------------------------
#define ATED_TESTFILENAME "testsuite.txt"	// Holds editor API commands

enum	AInState {eComment1, eComment2, eStd, eString1, eString2, eInWord, eSame};

// Syntax highlighting. See atextstaticdefs.h for more details.
typedef QMap<QString, long> ACharMap;
typedef QMap<QString, QString> AStringMap;
typedef struct
{	char		mBeg1[5];		// Beg-of-comment delimiter
	char		mBeg2[5];		// Alternate beg-of-comment delimiter
	char		mComment;		// Comment delimiter for commenting out code
	char		mEnd1[5];		// End-of-comment delimiter
	char		mEnd2[5];		// Alternate end-of-comment delimiter
	char		mEscape;		// Escape character used in quoted strings.
	QString		mFcnPattern;	// Function list regular expression
	ACharMap	mKeywords;		// List of keywords
	char		mQuote1;		// String delimiter
	char		mQuote2;		// Alternate string delimiter
} ALanguage;

typedef struct
{	// Lists are comma-delimited 
	const char* spExt;			// List of file extensions for this language
	const char* spQuote;		// List of one or two quote chars.
	const char* spEscape;		// Char for escaping quote chars in strings.
	const char* spComment;		// List of one or two pairs of opening/closing comment delimiters
	const char* spFcnPattern;	// Regular expression for identifying function definitions
	const char* spKeywords;		// List of keywords for this language.
} ALangDef;
enum		ALanguageType {eText, eHtml, eCpp, eJavaScript, eLisp};
typedef QHash<QString, ALanguageType> ALanguageMap;

// Dialog info
typedef struct
{	bool	mSortAscending;		// Sort names in ascending order
	long	mSortCol;			// Current sort column
} AFcnListParams;

//	------------------------------------------------ CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief ATextEdit - Implements a fast, flexible screen editor tailored for editing large files.

ATextEdit uses a very small character representation (16bits). A line is a set of one or more rows, with the last row terminated
by a newline. The maximum number of characters in a row is set by scDefaultMaxRowWidth if word-wrap is not enabled; else, the maximum
number of characters in a row is given by the position of the rightmost edge of the display (in chars).  Very long lines are
supported.

\par Features.
-# Edits large files gracefully.
-# Insert, move, delete, undo, redo
-# AisLisp, AisJavascript, AisCpp syntax highlighting
-# Matches parens, braces, brackets.
-# Supports word wrap, tabs, row attributes (e.g. line numbers)
-# Toolbars, menus, customization (tab width, font size)
-# Context menus including Lambda list.
-# Find/replace dialog.
-# Fixed-width fonts only. No font styles within a row (bold, underline...)
-# Config file to set font size, tab width, word wrap, window position & size, line attributes.
-# Mass indent/unindent, mass comment/uncomment

\par Rows.
A row is an expandable array of unsigned shorts.  The upper 4 bits contains an index into an array of background colors.  The
next 4 most-significant bits contains an index into an array of foreground colors.  The last 8 bits contains the Latin1 code
(ISO 8859-1)for the character.

One instance of ATextRow is allocated for every row of text in the document.  ATextRow holds the row buffer, a starting
state, and the buffer length.  This much state is a compromise between keeping track of even more state and recalculating
everything whenever it is needed.  For example, would it be worth the overhead to keep the row number for every row?

\par Document
A document is a list of ATextRows. Each row structure holds an array of 16-bit unsigned values. Tabs are padded with Latin1
space characters up to the next tab stop.  A line consists of one or more rows with the last row terminated by a newline.  It
is an error to have a newline anywhere else in any row of text.  Every document has at least one line.

Every instance of ATextEdit contains one instance of the ATextDocument class.  In addition to the document itself, ATextDocument
also holds an undo stack of insert and delete operations.  Each undo operation holds the starting position.  If it is an insert
the length of the insert is saved.  If it is a delete, the deleted text is saved.

\par Text vs. Display.
The text is saved in as a list of row structures that is a close approximation of the display.  Every row in the buffer
contains the same characters as those in the corresponding row of the display.  Most models keep the text representation
separate from the way it is rendered.  Here we choose to use a fixed-width font stored in a structure that mimics the display
strictly for performance.

Most characters take up one character width when displayed.  A few, such as a tab character, take up one or more character
spaces in the row buffer.  Newlines take up one space at the end, but are not normally displayed.  X refers to the beginning
offset of the character from the beginning of the row, starting from 0.

With word-wrap and without it, one line can take up more than one row if the line is longer than the maximum allowed width of
one line. Rows are numbered from zero starting at the beginning of text.  Y refers to the offset of a row from the first row
of the display.

\par Viewport:
Think of the display region as a huge rectangular canvas that stretches from the top-left edge of the text (X = Y = 0) to the
end of the last row of text (X = width of last row, Y = last row of text).  For more than a small piece of text, the entire canvas
cannot be viewed at once.  Rather, a small window, called the viewport, can be moved around on the canvas using the scroll bars
on the right and on the bottom of the viewport.  No matter where the viewport is positioned, the display coordinates X, Y
are all referenced from the top left corner of the canvas.  The underlying display routines take care of converting canvas-based
coordinates to the viewport coordinates.  Note that if word-wrap is enabled, all rows fit in the display, so horizontal
scrolling is not necessary.

\par Cursors:
A cursor holds the X-Y position in the edit buffer mRow, mCol. The position of the cursor is represented by a dark, blinking
vertical bar. A few strategic positions in the text are maintained:
- Cursor.  The cursor holds the position of the character just past the blinking vertical bar in the text.
This cursor notes the starting or ending position of inserts or deletes or the position after a move operation.
- Anchor. This cursor notes the other end of currently selected text or the other end of a matching pair of brackets. If no
text is currently selected (highlighted in blue) or if no pair of brackets is currently matched, the anchor is positioned at
the other end of any inserted text.  Otherwise, the anchor is at the same position as the cursor.
- Undo. An undo operation holds a beg cursor positon noting the starting position of an operation.

\par Edit Operations.
All edit operations are converted to a sequence of move, insert or delete operations.  For example, a character is inserted by
inserting a character into the text at the current cursor position and then the cursor is moved one position to the right.  A
Backspace is converted into a cursor move that moves the cursor back one character followed by a delete operation that deletes
the character at the cursor.  A Del key-press is converted into a delete operation that deletes the character at the cursor.

All edit operations that modify the text are converted into an insert operation or into a delete operation.
These routines save an "Undo Operation"  that reverses the operation.  That is, an insert operation creates a delete operation
the deletes the characters that were inserted and vice versa. If the undo operation is carried out, it reverses the undo operation
so that the user can "redo" the undo operation to restore the original edit.

Besides responding to key-press and mouse events, all edit operations can be programatically invoked by one of the public
functions that begin with "edit" such as editAppend, editClear, etc.

\par Selected Text.
Selected text is highlighted in the display as white text on a medium blue background, Text is selected by most move operations
when the shift-key is depressed. Also some mouse operations, such as click and drag or double-clicking on a word select a
portion of the text. Several edit operations, such as block delete, copy, cut, indent/unindent, comment/uncomment, find,
find/replace operate on selected text.  An insert operation while text is selected will replace the selected text with the
inserted text.

Delete operations first move the cursor to the new position and then delete the selected text.  For example, delete-to-eol
will extend (or contract) the selected area to the eol and then delete the selected text.  Delete-next-char and
delete-prev-char are exceptions to this rule.  They just delete the selected text without moving forward or backward.

Text may be selected by moving forwards or backwards through the text using any key that moves the cursor (such as the arrow
keys) with the shift key depressed.  When moving forward, the selected text starts at the anchor, cAnchor and extends to the
new cursor position, cCursor.  When moving backward, the selected text extends from cCursor to cAnchor.  Consecutive forward
moves move the cursor progressively forward or vice-versa.  Some commands, such as move word, are designed with this type of
selection in mind.  That is, the move word forward moves forward to the the next word and move word back always moves back to
the previous word so that the selected area may be extended word-by-word.

Most operations deselect the selected text. Indent/unindent, comment/uncomment, copy, and replace-in-a-section are exceptions
to this rule.  In every case the cursor is at one end of the selected text and the anchor is at the other.

\par Paren Matching.
Paren matching is a bit of a misnomer.  All items bracketed by (),[],{}, or <> are highlighted with a light grey background.
Paren matching occurs when moving the cursor to just before an opening bracket or just after a closing bracket.  The paren
matcher scans forward from the opening bracket to the matching closing bracket.  If no matching bracket is found, an alert
is issued.  The highlighted region extends from the cursor to the anchor, cAnchor.  Note that the paren matching is turned off
if text is selected.  See cursorSet() for more details.  The paren matcher is smart enough to ignore bracket characters that
are embedded in a comment or in a quoted string.  Moving the cursor to a new position causes the previously matched text, if
any, to be unhighlighted.

\par Colors.
Note that the background color is used to note plain text, selected text, or matched text.  The foreground color, called a
"pen" color, is used to note plain text, keywords, or commented text.  A color index is stored in the upper 8 bits of the
16-bit word allocated in the row buffer for each character in the text edit document.  The upper 4 bits hold an index into an
array of background colors.  The next 4 bits hold an index into an array of pen colors.  A pen color is allocated
for plain text, comments, keywords, etc.  This arrangement allows a single 16-bit number to hold all of the character-specific
information.

\par Syntax Coloring.
The syntax coloring parameters are set in atextstaticdefs.h.  Most of the heavy lifting is done in updateForeground() where a state
transition matrix guides the coloring process.  The state transition matrix is defined in langdef.h.  The next state is
dependent upon the current state and the type of character (bracket, quote char, comment delimiter, ...).  For example, an
alpha character while in a comment will not change the state, but an alpha character while in plain text will cause the state
to change to a in-a-word.

The pen color index of the char is set based upon the current state and previous state.  For example, if leaving the in-a-word
state and the previous word is a keyword, the pen color index for the previous word is set to scPenKeyword.


\par Edit Test.
The edit test command reads a test suite file and executes the commands found in the test suite. The user can single step thru
these tests or run them in batch mode.  Normally, the test suite is run in the C++ IDE debugger. If any errors are detected,
they will appear in the debug output window.  Moves are based upon all of the AMoveTo options such as eNextChar, ePrevWord,
etc.  The test suite exercises the edit routines (editAppend to editWrite).

\par End-of-Line Handling.
In reading text files, the end-of-line character used on various platforms is always translated into a newline in the text
document.  In writing a text document back to a text file, the newline is translated into the standard end-of-line character
for that platform.  These cases and the cases where a text file happens to have a non-standard termination for that platform
are noted below. The table show below uses the following mnemonics:
\verbatim
\r - Carriage return (0x0d)
\n - Line feed (0x0a)

Platform    End-of-Line Converted  End-of-Line on
            In File     To:        write back to file
--------    ----------  ---------  --------------
Windows     \r\n        \n         \r\n
            \n          \n         \r\n
            \r          (deleted)

Linux       \n          \n         \n
            \r\n		\n         \n
            \r          (deleted)
\endverbatim

 */
class ATextEdit : public QAbstractScrollArea
{
	Q_OBJECT
public:
	ATextEdit(QWidget* ipParent, const QString& irFileSpec = QString(), ALanguageType iLanguage = eText, long iMaxRows = LONG_MAX);
	~ATextEdit();

	enum		ACmdType {eNone, eDelFwd, eDelBack, eInsFwd, eInsBack, eMoveFwd, eMoveBack};
	enum		ACursorType {eAnchor, eCursor, eEnd};
	enum		WordWrap {NoWrap, WidgetWidth, FixedPixelWidth, FixedColumnWidth};
	enum		AMoveTo {eNoMove, eBol, eBot, eEol, eEot, eNextBol, eNextBow, eNextChar, eNextCur, eNextEow
				, eNextHalfPage, eNextLine, eNextPage, eNextSow, ePrevBow, ePrevChar, ePrevCur, ePrevHalfPage, ePrevLine
				, ePrevPage, ePrevSow, ePrevTab, eSame};
	enum		AChType {eAlpha, eSep, eQuote1, eQuote2,  eBeg1, eEnd1, eBeg2, eEnd2};

static void		beep();
static QString	getVersion();
static void		initializeStaticDefs(QWidget* ipParent, const QString& irTestFileName);

static AOperationsDialog* createFcnListDialog(QWidget* ipParent, const char* ipName);
static AOperationsDialog* createFindDialog(QWidget* ipParent, const char* ipName);
static AOperationsDialog* createGoDialog(QWidget* ipParent, const char* ipName);
static AOperationsDialog* createReplaceDialog(QWidget* ipParent, const char* ipName);
static APrefDialog*       createPrefDialog(QWidget* ipParent, const char* ipName);

	// Dialogs. Invoke selected dialogs
	void		dialogHelp();
	void		dialogHelpAlpha();
	void		dialogHelpApi();
	void		dialogHelpFind();
static QString	dialogOpen(QWidget* ipParent);
	QString		dialogSaveAs();

	// Edit. The editor api
	void		editAppend(const QString& irText, bool iMoveToEnd);
	bool		editAppendFile(const QString& irFileSpec);
	void		editClear();
	void		editClearUndo();
	void		editComment(bool iIn = true);
	void		editCopy(bool iAppend = false);
	void		editCut(bool iAppend = false);
	long		editDelete(ATextEdit::AMoveTo iMove);
	bool		editFind(const QString& irPattern, bool iAll, bool iMatchCase, bool iDown, bool iRegExp, bool iSelect, bool iMatchWord);
	QStringList	editFunctionList();
	void		editGoToLine(long iLineNum);
	void		editIndent(bool iIn = true);
	void		editInsert(const QString& irText, bool iForward);
	bool		editIsModified();
	bool		editIsSelected();
	bool		editMove(ATextEdit::AMoveTo iMove, bool iSelect);
	void		editPaste();
	void		editPrint();
	bool		editRead(const QString& irFileSpec);
	void		editRedo(bool iToMark);
	bool		editReplace(const QString& irPattern, const QString& irText, bool iAllText, bool iMatchCase, bool iRegExp
				, bool iMatchWord);
	bool		editReplaceAll(const QString& irPattern, const QString& irText, bool iAllText, bool iMatchCase, bool iRegExp
				, bool iSelect, bool iMatchWord);
	QByteArray	editSelectedText();
	bool		editSelectMatchedText();
	void		editSetModified(bool iModified);
	void		editSetParameter(const QString& irName, const QVariant& irValue);
	void		editSetReadOnly(bool iEnable);
	QString		editShow(const QString& irItem, long iRow);
	bool		editSwitch();
	bool		editTabify(long iExtraSpaces, bool iToTabs);
	QByteArray	editText();
	long		editTextLength();
	long		editTextRows();
	void		editUndo(bool iToMark);
	QString		editWord(bool iSelect);
	bool		editWrite(const QString& irFileSpec);

signals:
	void		cursorPositionChanged(long iX, long iY);
	void		cursorStackChanged(bool iForward, bool iPrevious);
	void		runCommand(const QByteArray& irCmd);
	void		statusAlert(const QString& irMsg);
	void		textModified(bool iModified);

protected:
	virtual	bool event(QEvent* ipEvent);
	virtual	void mouseDoubleClickEvent(QMouseEvent* ipMouseEvent);
	virtual	void mouseMoveEvent(QMouseEvent* ipMouseEvent);
	virtual	void mousePressEvent(QMouseEvent* ipMouseEvent);
	virtual	void focusInEvent(QFocusEvent* ipFocusEvent);
	virtual	void focusOutEvent(QFocusEvent* ipFocusEvent);
	virtual void paintEvent(QPaintEvent* ipEvent);
	virtual void resizeEvent(QResizeEvent* ipResizeEvent);
	virtual void wheelEvent(QWheelEvent* ipWheelEvent);

private slots:
	void		onBlinkCursor();
	void		onRowCountChanged(long iNewSize);
	void		onSliderMoved(int iValue);

private:
	void		emitRunCommand();
	void		fcnList(QStringList& orList);
	bool		findNext(ATextEdit::AMoveTo iMove, bool iForceWrap);
	void		initializeParameters(ALanguageType iLanguage);
	void		keyPress(long iKey, Qt::KeyboardModifiers iKeyState);
	void		tabifyDialog();
	void		tabWidthDialog();
	void		testDialog();
	long		textParenType(long iCh);
	long		truncateTop(long iLeft);
	void		updateCursor(bool iRepaint, bool iMoveToTop);

	QTimer*		cpBlinkTimer;		// Timer for blinking cursor
	QClipboard*	cpClipboard;		// Application's clipboard
	bool		cCursorShow;		// Toggle cursor show
	QPoint		cCurCursor;			// Current cursor position
	AFcnListParams cFcnListParams;	// Sort ascending, sort column
	QString		cFindPattern;
	bool		cFindCase;
	bool		cFindRegExp;
	bool		cFindSelect;
	bool		cFindWords;
	QScrollBar*	cpHorizScrollBar;	// Horizontal scroll bar
	long		cMouseMoves;		// Number of mouse events while dragging mouse
	AParameters	cParameters;		// Configurable parameters
	bool		cReadOnly;			// Allow append, but no other edits.
	bool		cStaleAlert;		// True iff alert message was displayed.
	ATextCursor*cpTextCursor;		// Holds text cursor.
	bool		cTextModified;		// True iff text has been modified.
	QScrollBar* cpVertScrollBar;	// Vertical scroll bar properties
	long		cViewPortHeight;	// Current height [rows] of display
	QWidget*	cpViewPort;			// -> Scrolling display area
	long		cViewPortWidth;		// Current width [chars] of display

public:
static QString	editOpen(QWidget* ipParent);

// Public static default values (see initLanguageDefs for details)
// Default Configuration Parameters:
static const long	scDefaultLeftOffset;	// Offset 2[pixels] of  row from left edge of viewport(drawText)
static const QString scDefaultFont;			// Font family
static const long	scDefaultFontSize;		// Default font size [points] (72 pts / inch)
static const long	scDefaultMaxRowWidth;	// Maximum display width if no wrap [chars] (128 - 4096)
static const long	scDefaultMaxUndoDepth;	// Maximum depth of the undo stack
static const long	scDefaultTabWidth;		// Default tab width 4[char]

// Defined constants:
// Note: integer types can be initialized here, but they are initialized in atextstaticdefs.h so that all initializations are in one place.
static const QString scApplication;		// Name of this application (ATextEdit)
static const long	scBfrInc;			// Minimum increment when resizing a buffer [ushort].
static const char	scCarriageReturn;	// Displayed carriage-return char
static const char	scCharDel;			// Displayed DEL (7f) char
static const char	scCharNewline;		// Displayed newline char
static const char	scCharTab;			// Displayed tab char
static const ushort	scLatin1Space;		// Latin1 space
static const long	scMaxMouseMoves;	// Mouse move events per update of display.
static const long	scMaxLinesRow;		// Maximum number of lines in one row.
static const QString scOrganization;	// Name of our organization (InvestByLambda.com)
static const char*	scpParenClose;		// Array of closing bracket chars.
static const char*	scpParenOpen;		// Array of open bracket chars.
static const char*	scpStates[];		// AInState names
static const char*	scpTxtBg[];			// Background identifiers Base,Matched,Highlighted
static const char*	scpTxtPen[];		// Foreground identifiers Text,Keyword,Comment,String
static const char*	scpUndoTypes[];		// Undo type names
static const long	scNTypes;			// Number of undo types.
static const char*	scpUntitled;		// New (anonymous) file name
static const char*	scpVersion;			// Current build label

// Dialogs:
static ATabifyDialog*	scpTabifyDialog;	// -> convert-spaces-tabs dialog
static ATabWidthDialog*	scpTabWidthDialog;	// -> set-tab-width dialog
static ATestDialog*		scpTestDialog;		// -> test suite dialog

// Color arrays. (indexed by color codes in upper bits of each char).
static QBrush		scBgBrushes[];
static QColor		scPenColors[];

// Language Syntax info
static char			scBracket[];
static ALanguageMap	scFileExts;
static bool			scInitDefs;
static uchar		scInWord[];
static ALangDef*	scpLangDefs[];
static ALanguage	scLanguages[];
static uchar		scLower[];
static uchar		scNextState[][8];
static QString		scNotInMap;
static QString		scNotInFile;

// Configuration parameters (defined constants). See atextstaticdefs.h for initialization.
// C++ FAQS (p.228) says that they can be initialized here, but they can't.
// Shifted color codes
static const ushort scBgPlainText;	// Index into scBgBrushes << 12 for plain text
static const ushort	scBgSelText;	// Index into scBgBrushes << 12 for selected text
static const ushort	scBgMatchText;	// Index into scBgBrushes << 12 for text between brackets.
static const ushort	scPenSelText;	// Index into scPenColors << 8 for selected text
static const ushort	scPenKeyword;	// Index into scPenColors << 8 for keywords
static const ushort	scPenComment;	// Index into scPenColors << 8 for comments
static const ushort	scPenString;	// Index into scPenColors << 8 for strings

static const ushort scPenMatchText;	// TEMPORARY UNTIL GET BACKGROUND COLORS SET.

// Color code masks:
static const ushort	scMaskBg;		// Background color-code mask
static const ushort	scMaskChar;		// Character code mask
static const ushort	scMaskColor;	// Color code mask
static const ushort	scMaskDisplay;	// Color bits that are used for display (omit string)
static const ushort	scMaskPen;		// Pen (foreground) color code mask
};

#endif		// ATEXTEDIT_H
