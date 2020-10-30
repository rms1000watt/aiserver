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
aisdev/atextedit/atextedit.cpp
												Text Editor

CHANGE HISTORY
Version	Date		Who		Change
3.2009   5/10/2008	fchua	Fixed scrollbar resize bug after font change.
3.2008	 4/08/2008	fchua	Fixed font change bugs in editSetParameter and initializeParameters.
3.2008	 4/08/2008	fchua	Removed support to store settings.
3.2006	 3/10/2008	fchua	Removed association with Find, Replace, Go and Function List dialogs.
2.0004	 2/17/2007	tlw		Add quote characters to syntax highlighting.
2.0003	2/7/2007	tlw		LineNumbers. Add a line numbers parameter.
2.0002	1/20/2007	tlw		dialogSaveAs. Start in the current file's directory.
1.0118	12/12/2006	tlw		replace. Eliminate iSelect argument.
1.0116	11/27/2006	tlw		resizeEvent. Reset cViewWidth. editSetParameter. Reset Horizontal scroll bar policy.
1.0115	11/16/2006	tlw		findNext. Emit alert if no more matches. keyPress. Add Ctrl-W toggle word wrap.
1.0110	10/13/2006	tlw		CursorStack. Enable/disable cursor stack signal.
1.0110	1012/2006	tlw		ACmdType. Remove eUndo, eRedo. editClearUndo. Add to API.
1.0109	10/5/2006	tlw		editShow. Add dump row option.
1.0108	9/28/2006	tlw		editApi. Repair the API.
1.0108	9/27/2006	tlw		editPrint. Implement editPrint using QTextDocument.
1.0107	9/22/2006	tlw		editWord. Implement to find nearby word. Add eNextSow and ePrevSow to edit moves.
1.0107	9/20/2006	tlw		editMove. Implement editMove.  Add third argument to setCursor.
1.0105	9/13/2006	tlw		editReplace. Do not set ipParent if creating an AReplaceDialog. UpdateCursor. Allow move to top.
1.0068	8/17/2005	tlw		editRead(). Translate all line terminations (CR, CR-LF, LF) to LF
1.0067	7/16/2005	tlw		Revamp cursorScanToX, drawSection to wrap correctly at word boundries. Add mouse wheel.
1.0060	5/3/2005	tlw		Combine initParameters, readIniFile and getScreenSize into init.
1.0057	3/18/2005	tlw		Update documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QtDebug>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QTimer>

#include <QtGui/QClipboard>
#include <QtGui/QFocusEvent>
#include <QtCore/QEvent>
#include <QtGui/QFileDialog>
#include <QtGui/QKeyEvent>
#include <QtGui/QMouseEvent>
#include <QtGui/QPainter>
#include <QtGui/QPrinter>
#include <QtGui/QProgressDialog>
#include <QtGui/QResizeEvent>
#include <QtGui/QScrollBar>
#include <QtGui/QWheelEvent>

#include "afcnlistdialog.h"
#include "afinddialog.h"
#include "agodialog.h"
#include "ahelpalphadialog.h"
#include "ahelpapidialog.h"
#include "ahelpdialog.h"
#include "ahelpfinddialog.h"
#include "areplacedialog.h"
#include "atabifydialog.h"
#include "atabwidthdialog.h"
#include "atestdialog.h"
#include "atextedit.h"
#include "atextcursor.h"
#include "atextstaticdefs.h"
#include "atexteditprefdialog.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief ATextEdit - Constructor initializes the class properties and sets the text's configuration parameters.

\param ipParent -> parent widget that created this instance of ATextEdit.
\param irFileSpec - The file path plus the file name.
\param iLanguage - The type of language (this may be changed later when a file is read).
\param iMaxRows - The maximum number of rows allowed in the text. The rows at the top are truncated if the text exceeds iMaxRows.
\returns void
\par Notes:
-# If iMaxRows is zero, the number of rows is only limited by available memory.
 */
ATextEdit::ATextEdit(QWidget* ipParent, const QString& irFileSpec, ALanguageType iLanguage, long iMaxRows)
	: QAbstractScrollArea(ipParent)
{
	// Initialize just once.
	if (!scInitDefs)
		initializeStaticDefs(this, ATED_TESTFILENAME);
	scInitDefs = true;

	// Configure system resources
	cpClipboard = QApplication::clipboard();

	// Initialize private variables.
	cpBlinkTimer = new QTimer(this);
	cCurCursor = QPoint(0, 0);
	cCursorShow = false;
	cFcnListParams.mSortAscending = true;
	cFcnListParams.mSortCol = 0;
	cFindCase = true;
	cFindRegExp = false;
	cFindSelect = false;
	cFindWords = false;
	cMouseMoves = 0;
	cReadOnly = false;
	cTextModified = false;				// Start off unmodified
	cpViewPort = viewport();
	initializeParameters(iLanguage);	// Set configurable parameters based on font

	// MaxRows. Set the maximum number of rows to the default setting provided by the caller.
	cParameters.mMaxRows = iMaxRows;	// Maximum number of rows in document

	// MatchParens. Override MatchParen setting based upon the file extension. Also set object name.
	cParameters.mMatchParen = (iLanguage != eText);
	if (!irFileSpec.isEmpty())
	{	QFileInfo aFileInfo(irFileSpec);
		QString aSuffix(aFileInfo.suffix());	// Extension after last dot.
		setObjectName(aFileInfo.fileName());	// Strip off path
		if (!aSuffix.isEmpty())
			cParameters.mMatchParen = (aSuffix == "sl" || aSuffix == "js" || aSuffix == "c" || aSuffix == "h" || aSuffix=="cpp");
	}
	// Configure text cursor.
	cpTextCursor = new ATextCursor(cpViewPort, &cParameters);
	connect(cpTextCursor, SIGNAL(rowCountChanged(long)), this, SLOT(onRowCountChanged(long)));
	connect(cpBlinkTimer, SIGNAL(timeout()), this, SLOT(onBlinkCursor()));
	connect(&cpTextCursor->cTextDocument, SIGNAL(cursorStackChanged(bool, bool)), this, SIGNAL(cursorStackChanged(bool, bool)));

	// Configure the viewport
	cpViewPort->setFocusProxy(this);
	setFocusPolicy(Qt::StrongFocus);
	cpViewPort->setMouseTracking(false/*enable*/);	// Disable events if mouse over widget (w/ no buttons pressed)
	cpViewPort->setCursor(Qt::IBeamCursor);

	// ScrollBars. Configure scroll bars. See also resizeEvent.
	long aCharWidth = cParameters.mCharWidth;
	long aLineHeight = cParameters.mLineHeight;
	setHorizontalScrollBarPolicy(cParameters.mWrap ? Qt::ScrollBarAlwaysOff : Qt::ScrollBarAlwaysOn);
	setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
	cpHorizScrollBar = horizontalScrollBar();
	cpVertScrollBar = verticalScrollBar();
	cpHorizScrollBar->setSingleStep(1);
	cpVertScrollBar->setSingleStep(1);
	// cpViewPort->width() returns a bogus value.  Maybe we can use a saved setting passed in by constructor.
	cViewPortWidth = cpViewPort->width() / aCharWidth;
	if (cViewPortWidth < 128)
		cViewPortWidth = 128;
	cViewPortHeight = (cpViewPort->height() + aLineHeight - 1) / aLineHeight;
	cpHorizScrollBar->setPageStep(cViewPortWidth);
	cpHorizScrollBar->setMaximum(cParameters.mMaxRowWidth - cViewPortWidth);
	cpVertScrollBar->setPageStep(cViewPortHeight);
	cpVertScrollBar->setMaximum(0);
	cpTextCursor->setViewSize(QSize(cViewPortWidth, cViewPortHeight));

	connect(cpHorizScrollBar, SIGNAL(sliderMoved(int)), this, SLOT(onSliderMoved(int)));
	connect(cpVertScrollBar, SIGNAL(sliderMoved(int)), this, SLOT(onSliderMoved(int)));
	setFocus();
}

/*!
\brief ~ATextEdit - Delete all allocated resources, save current settings

\par Notes:
-# ATextEdit has remote ownership of ATextCursor and QTimer referents. 
-# Since instances of ATextDocument are never assigned, copied or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
ATextEdit::~ATextEdit()
{
	// Deallocate. Delete all allocated resources
	cpBlinkTimer->stop();
	delete cpTextCursor;
	delete cpBlinkTimer;

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~ATextEdit()");
#endif
}

/*!
\brief beep - Send an audible alarm

\return void
\par Notes:
-# Set a breakpoint here to intercept all debug error messages.
	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::beep()
{
	qApp->beep();
}

/*!
 * \brief Create a Function/Lambda list dialog and return a pointer to it.
 *
 * \param[in] ipParent Parent widget of the created dialog.
 * \param[in] ipName Name of the dialog.
 */
AOperationsDialog* ATextEdit::createFcnListDialog(QWidget* ipParent, const char* ipName)
{
	return (new AFcnListDialog(ipParent, ipName, Qt::Dialog));
}

/*!
 * \brief Create a Find dialog and return a pointer to it.
 *
 * \param[in] ipParent Parent widget of the created dialog.
 * \param[in] ipName Name of the dialog.
 */
AOperationsDialog* ATextEdit::createFindDialog(QWidget* ipParent, const char* ipName)
{
	return (new AFindDialog(ipParent, ipName, Qt::Dialog));
}

/*!
 * \brief Create a Go dialog and return a pointer to it.
 *
 * \param[in] ipParent Parent widget of the created dialog.
 * \param[in] ipName Name of the dialog.
 */
AOperationsDialog* ATextEdit::createGoDialog(QWidget* ipParent, const char* ipName)
{
	return (new AGoDialog(ipParent, ipName, Qt::Dialog));
}

/*!
 * \brief Create a Preferences dialog and return a pointer to it.
 *
 * \param[in] ipParent Parent widget of the created dialog.
 * \param[in] ipName Name of the dialog.
 */
APrefDialog* ATextEdit::createPrefDialog(QWidget* ipParent, const char* ipName)
{
	return (new ATextEditPrefDialog(ipParent, ipName));
}

/*!
 * \brief Create a Replace dialog and return a pointer to it.
 *
 * \param[in] ipParent Parent widget of the created dialog.
 * \param[in] ipName Name of the dialog.
 */
AOperationsDialog* ATextEdit::createReplaceDialog(QWidget* ipParent, const char* ipName)
{
	return (new AReplaceDialog(ipParent, ipName, Qt::Dialog));
}

/*!
\brief dialogHelp - Launches general Help for edit key bindings

\return void
 */
void ATextEdit::dialogHelp()
{
	AHelpDialog aHelpDialog(this, "HelpDialog");
	long aRet = aHelpDialog.exec();
	if (aRet == QDialog::Accepted)
	{	// accept() called on dialog.
	}
}

/*!
\brief dialogHelpAlpha - Launches alphabetical Help for edit key bindings

\return void
 */
void ATextEdit::dialogHelpAlpha()
{
	AHelpAlphaDialog aHelpAlphaDialog(this, "HelpAlphaDialog");
	long aRet = aHelpAlphaDialog.exec();
	if (aRet == QDialog::Accepted)
	{	// accept() called on dialog.
	}
}

/*!
\brief dialogHelpApi - Launches the Help on the editor API

\return
 */
void ATextEdit::dialogHelpApi()
{
	AHelpApiDialog aHelpApiDialog(this, "HelpApiDialog");
	long aRet = aHelpApiDialog.exec();
	if (aRet == QDialog::Accepted)
	{	// accept() called on dialog.
	}
}

/*!
\brief dialogHelpFind - Launches  Help on find using regular expresssions

\return void
*/
void ATextEdit::dialogHelpFind()
{
	AHelpFindDialog aHelpFindDialog(this, "HelpFindDialog");
	long aRet = aHelpFindDialog.exec();
	if (aRet == QDialog::Accepted)
	{	// accept() called on dialog.
	}
}

/*!
\brief dialogOpen - Launch an file dialog to select a file on the local file system

\param ipParent -> Widget that called this open dialog
\return aFileSpec - The path + filename for the selected file
\par Notes:
-# If no file is selected, aFileSpec is empty
 */
QString ATextEdit::dialogOpen(QWidget* ipParent)
{
	QString aCaption("Open new or existing file");
	QString aDir(".");
	QString aFilters("All Files (*);;Text Files (*.txt *.log *.ini);;Source Files (*.c *.cpp *.h *.js *.sl)");
	QString aFileSpec = QFileDialog::getOpenFileName(ipParent, aCaption, aDir, aFilters);
	return aFileSpec;
}

/*!
\brief dialogSaveAs - Open a dialog to select a new file name and then save the text in this file

\return aFileSpec - The path + file name for the selected file
 */
QString ATextEdit::dialogSaveAs()
{
	QFileInfo aFileInfo(objectName());
	QString aFileName(aFileInfo.fileName());
	QString aCaption = QString("Choose a new file name to use for saving %1.").arg(aFileName);
	QString aDir(aFileInfo.path());
	QString aFilters("All Files (*);;Text Files (*.txt *.log *.ini);;Source Files (*.c *.cpp *.h *.js *.sl)");
	QString aFileSpec = QFileDialog::getSaveFileName(this, aCaption, aDir, aFilters);
	return aFileSpec;
}

/*!
\brief editAppend - Append input to the end of the last line of text.

\param irText - Text to be appended
\param iMoveToEnd - If true, move cursor to end of text after append.
\return void
\par Notes:
-# If the number of lines exceeds the maximum allowed, truncate top rows of the display.
-# Moves cursor to end of appended text
-# Append avoids the overhead of undo and paren matching.  Much faster than insert.
 */
void ATextEdit::editAppend(const QString& irText, bool iMoveToEnd)
{
	if (cReadOnly)
	{	emit statusAlert("Append: Text is read-only");
		cStaleAlert = true;
	}
	else
	{	cpTextCursor->append(irText.toAscii(), irText.length(), iMoveToEnd);
		updateCursor(true/*Repaint*/, false/*MoveToTop*/);
	}
}

/*!
\brief editAppendFile - Append contents of a file to end of text.

\param irFileSpec - Path+Name of file to be appended
\return aRet - True iff append succeeds
\par Notes:
-# Much faster than an insert.
-# If read-only, editAppend will fail. Need to return status from edit append.
 */
bool ATextEdit::editAppendFile(const QString& irFileSpec)
{
	bool aRet = false;
	QFile aF(irFileSpec);
	if (cReadOnly)
	{	emit statusAlert("Append File: Text is read-only");
		cStaleAlert = true;
	}
	else if (!aF.open(QIODevice::Text | QIODevice::ReadOnly))	// 16 | 1
	{	emit statusAlert(QString("editAppendFile, Unable to read " + irFileSpec));
		cStaleAlert = true;
	}
	else
	{	long aLgth;
		QByteArray aText;
		aText.resize(4100);
		char* apText = aText.data();

		while ((aLgth = aF.readLine(apText, 4096)) > 0)
		{	*(apText + aLgth) = '\0';
			editAppend(QString(aText), true/*MoveToEnd*/);
		}
		aRet = true;
	}
	return aRet;
}

/*!
\brief editClear - Clear the contents of a file.

\return void
 */
void ATextEdit::editClear()
{
	if (cReadOnly)
	{	emit statusAlert("Clear: Text is read-only");
		cStaleAlert = true;
	}
	else
	{	truncateTop(0);
		editSetModified(true/*Modified*/);
		updateCursor(true/*Repaint*/, false/*MoveToTop*/);
	}
}

/*!
\brief editClearUndo - Clear the undo stack

\return void
 */
void ATextEdit::editClearUndo()
{
	cpTextCursor->clearUndo();
}

/*!
\brief editComment - Prepend/remove the comment delimiter from beginning to every selected row.

\param iIn - If true, insert comment delimiter; else, remove comment delimiter.
\return aOk - true iff one or more rows modified
\par Notes:
-# Some portion of the text must be selected.
 */
void ATextEdit::editComment(bool iIn)
{
	long aCh = '*';
	long aLanguage = cParameters.mLanguage;
	if (cReadOnly)
	{	emit statusAlert("Comment: Text is read-only");
		cStaleAlert = true;
	}
	else // Indent. Indent if non null delimiter for this language 
	{	if (aLanguage > 0)
			aCh = scLanguages[aLanguage].mComment;
		if (aCh == '\0')
		{	emit statusAlert(QString("Comment: No comment delimiter for this language"));
			cStaleAlert = true;
		}
		else if (cpTextCursor->indent(aCh, iIn))
			updateCursor(true/*Repaint*/, false/*MoveToTop*/);
		else
		{	emit statusAlert(QString("Comment: No selected text to comment/uncomment"));
			cStaleAlert = true;
		}
	}
}

/*!
\brief editCopy - Copy the selected text to the clipboard.

\param iAppend - If true, append selected text to the contents of the clipboard
\return void
\par Notes:
-# Some text must be currently selected.
 */
void ATextEdit::editCopy(bool iAppend)
{
	if (cpTextCursor->cSelectedText)
	{	// Selected text.
		QByteArray aSelected;
		long aLgth = 0;
		if (iAppend)
		{	QString aClipboard(cpClipboard->text(QClipboard::Clipboard));
			aLgth = aClipboard.length();
			aSelected += aClipboard;
		}
		if ((aLgth = cpTextCursor->selectedText(aSelected, aLgth)) > 0)
			cpClipboard->setText(QString(aSelected), QClipboard::Clipboard);
	}
	else
	{	emit statusAlert(QString("copy, No selected text to copy"));
		cStaleAlert = true;
	}
}

/*!
\brief editCut - Move the selected text to the clipboard.

\param iAppend - If true, append selected text to the contents of the clipboard
\return void
\par Notes:
-# Some text must be currently selected.
 */
void ATextEdit::editCut(bool iAppend)
{
	if (cReadOnly)
	{	emit statusAlert("Cut: Text is read-only");
		cStaleAlert = true;
	}
	else if (cpTextCursor->cSelectedText)
	{	long aLgth = 0;
		QByteArray aSelected;
		if (iAppend)
		{	QString aClipboard(cpClipboard->text(QClipboard::Clipboard));
			aLgth = aClipboard.length();
			aSelected += aClipboard;
		}
		if ((aLgth = cpTextCursor->selectedText(aSelected, aLgth)) > 0)
			cpClipboard->setText(QString(aSelected), QClipboard::Clipboard);
		aLgth = cpTextCursor->deleteIt(0);
		onRowCountChanged(cpTextCursor->count());
		updateCursor(true/*Repaint*/, false/*MoveToTop*/);
	}
	else
	{	emit statusAlert(QString("cut, No selected text to cut"));
		cStaleAlert = true;
	}
}

/*!
\brief editDelete - Delete selected text plus iLgth chars from the text

\param iMove - Select area to be deleted
\return aLgth - Number of chars deleted not including pad characters.
\par Notes:
-# Some text must be currently selected.
 */
long ATextEdit::editDelete(AMoveTo iMove)
{
	long aLgth = 0;
	if (cReadOnly)
	{	emit statusAlert("Delete: Text is read-only");
		cStaleAlert = true;
	}
	else
	{	// Move. don't move if deleting just one char and selected text.
		if (editIsSelected() && (iMove == eNextChar || iMove == ePrevChar))
			iMove = eSame;

		// Delete. Set aCursor to end of move and then delete from current cursor to aCursor
		QPoint aCursor(-1, -1);			// Default to start at current cursor position.
		if (cpTextCursor->move(iMove, aCursor))
		{	aLgth = cpTextCursor->deleteIt(aCursor);
			updateCursor(true/*Repaint*/, false/*MoveToTop*/);
		}
	}
	return aLgth;
}

/*
\brief editFind - Search for a pattern specified by Pattern from current cursor to end.

\param irPattern - Pattern to be matched
\param iAll - Search from cursor back around to the cursor.
\param iMatchCase - Iff true, the search is case sensitive.
\param iDown - If true, search forward in the text
\param iRegExp - If true, treat irFind as a regular expression
\param iSelect - If true, just search selected text
\param iMatchWord - If true, pattern must start/end on a word boundry
\sa editFind for more info on find operation
\par Notes:
-# If pattern is matched, move cursor to end of matched text. Highlight matched text.
-# If iDown, search forward, else search backwards from end of search region.
-# If iMatchCase, search is case sensitive. (Foo does not match foo)
-# If iMatchWord, pattern must begin/end at word breaks. (foo does not match foobar)
-# If iRegExp, pattern is treated as a regular expression. (\d+foo matches 12foo)
-# If iAll, continue search from top/bottom of text back to the original cursor position.
-# iAll is ignored if iSelect is true.
 */
bool ATextEdit::editFind(const QString& irPattern,bool iAll,bool iMatchCase,bool iDown,bool iRegExp,bool iSelect,bool iMatchWord)
{
	bool aMatch;
	// Settings. Save settings for findNext.
	cFindPattern = irPattern;
	cFindCase = iMatchCase;
	cFindRegExp = iRegExp;
	cFindSelect = iSelect;
	cFindWords = iMatchWord;
	if ((aMatch = cpTextCursor->find(irPattern, iAll, iMatchCase, iDown, iRegExp, iSelect, iMatchWord)))
		updateCursor(true/*Repaint*/, false/*MoveToTop*/);
	// PENDING ATextEdit::find() - Only repaint if text colors modified.
	return aMatch;
}

/*!
\brief editFunctionList - Get list of line numbers and function names in the text.

\return void
 */
QStringList ATextEdit::editFunctionList()
{
	return cpTextCursor->functionList();
}

void ATextEdit::editGoToLine(long iLineNum)
{
	updateCursor(cpTextCursor->setCursor(QPoint(0, iLineNum), false/*Select*/, false/*Newline*/, true/*SaveCur*/), true/*MoveToTop*/);
}

/*!
\brief editIndent - Indent currently selected rows

\param iIn - If true, increase indent; else, decrease indent.
\return void
\par Notes:
-# Some text must be currently selected.
 */
void ATextEdit::editIndent(bool iIn)
{
	if (cReadOnly)
	{	emit statusAlert("Indent: Text is read-only");
		cStaleAlert = true;
	}
	else if (cpTextCursor->indent('\t', iIn))
		updateCursor(true/*Repaint*/, false/*MoveToTop*/);
	else
	{	emit statusAlert(QString("Indent: No selected text to indent"));
		cStaleAlert = true;
	}
}

/*!
\brief editInsert - Insert text at the current cursor position

\param irText - Text to be inserted
\param iForward - Iff true, move cursor to end of inserted text.
\return void
\par Notes:
-# Selected text is deleted.
 */
void ATextEdit::editInsert(const QString& irText, bool iForward)
{
	ACmdType aCmd = (iForward) ? eInsFwd : eInsBack;
	if (cReadOnly)
	{	emit statusAlert("Insert: Text is read-only");
		cStaleAlert = true;
	}
	else 
	{	cpTextCursor->insert(aCmd, irText.toLatin1(), irText.length());
		editSetModified(true/*Modified*/);
		updateCursor(true/*Repaint*/, false/*MoveToTop*/);
	}
}

/*!
\brief editIsModified - Determine if the text buffer has been modified since last save.

\return True iff text buffer has been modified
 */
bool ATextEdit::editIsModified()
{
	return cTextModified;
}

/*!
\brief editIsSelected - Returns true iff some test is currently selected

\return True iff some text is selected.
 */
bool ATextEdit::editIsSelected()
{
	return cpTextCursor->cSelectedText;
}

/*!
\brief editMove - Move the cursor forward/back to position specified by iMove.

\param iMove - Move operation (see atextedit.h AMoveTo enum for a list)
\param iSelect - If true, select intervening text.
\return aOk - Iff cursor position is changed
\par Notes:
-# Most move operations are relative to cursor position.
-# If text is currently selected an iSelect is true, move extends current selection.

ACmdType iCmd, ATextEdit::AMoveTo
 */
bool ATextEdit::editMove(AMoveTo iMove, bool iSelect)
{
	bool aOk = true, aSaveCur = (iMove == eNextCur || iMove == ePrevCur) ? false : true;
	ACmdType aCmd = eMoveFwd;
	QPoint aCursor(-1, -1);				// Default to use current cursor position.
	if (cpTextCursor->move(iMove, aCursor))
		updateCursor(cpTextCursor->setCursor(aCursor, iSelect, false/*IncludeNewline*/, aSaveCur), false/*MoveToTop*/);
	else
	{	emit statusAlert((aCmd == eMoveFwd) ? "At End" : "At Beginning");
		cStaleAlert = true;
		aOk = false;
	}
	return aOk;
}

/*!
\brief editPaste - Insert text from the clipboard into text at current cursor position.

\return void
\par Notes:
-# If clipboard is empty, no text is inserted.
-# The cursor is moved to the end of the inserted text
 */
void ATextEdit::editPaste()
{
	if (cReadOnly)
	{	emit statusAlert("Paste: Text is read-only");
		cStaleAlert = true;
	}
	else
	{	QString aClipText = cpClipboard->text(QClipboard::Clipboard);
		aClipText.remove('\r');
		if (!aClipText.isEmpty())
		{	long aLgth = aClipText.length();
			QByteArray aText;
			aText += aClipText;
			cpTextCursor->insert(eInsFwd, aText, aLgth);
			editSetModified(true/*Modified*/);
			updateCursor(true/*Repaint*/, false/*MoveToTop*/);
		}
		else
		{	emit statusAlert(QString("paste, No clipboard text to copy"));
			cStaleAlert = true;
		}
	}
}

/*!
\brief editPrint - Print the current text

\return void
*/
void ATextEdit::editPrint()
{
	QPrinter aPrinter;
	QByteArray aText;
	QTextDocument aTextDoc(this);
	aPrinter.setOrientation(QPrinter::Portrait);
	aPrinter.setPageSize(QPrinter::Letter);
	aPrinter.setResolution(720);		// DPI
	aPrinter.setFullPage(false);		// Printer cannot print too close to the edges
	aPrinter.setNumCopies(1);
	cpTextCursor->text(-1/*Row*/, aText, 0/*Lgth*/);
	aTextDoc.setPlainText(aText);
	aTextDoc.print(&aPrinter);
}

/*!
\brief editRead - Initialize editor with contents of text file

\param irFileSpec	File to be read
\return aRet - True iff able to read file.
\par Notes:
-# Clears the current contents of the the edit buffer.
 */
bool ATextEdit::editRead(const QString& irFileSpec)
{
	// Language.
	QString aExt = QFileInfo(irFileSpec).suffix().toLower();
	cParameters.mLanguage = scFileExts.value(aExt);				// Returns 0 if extension not in map

	// Read the text file.
	bool aRet = true;
	long aChunk, aLgth, aSize;		// read returns less than aSize chars if CR's are in the file.
	QFile aFile(irFileSpec);
	if ((aSize = aFile.size()) <= 0 || !aFile.open(QIODevice::Text | QIODevice::ReadOnly))	// 16 | 1
	{	emit statusAlert(QString("editRead, Unable to read " + irFileSpec));
		cStaleAlert = true;
		aRet = false;
	}
	else
	{	// Clear. Clear document.
		truncateTop(0);

		// Chunks. Determine number of megabytes in this file.
		long aLastCh = '\0';						// Last char in previous chunk
		long aChunkSz = (aSize <= 0x100000) ? aSize : 0x100000;
		long aLastChunk = aSize / aChunkSz;			// Index of last chunk in the file.
		QByteArray aText;
		aText.resize(aChunkSz + 1);
		char* apSrc = aText.data();

		// Large File. If the file is greater than 10 chunks, include a progress bar.
		if (aLastChunk >= 10)
		{	QProgressDialog aProgress("Reading file...", "Abort", 0, aLastChunk + 1, this);
			for (aChunk = 0; aChunk <= aLastChunk; ++aChunk)
			{	aProgress.setValue(aChunk);
				qApp->processEvents();
				if (aProgress.wasCanceled())
				{	aRet = false;
					break;
				}
				// Append. Add text into text document
				if ((aLgth = aFile.read(apSrc, aChunkSz)) > 0)
				{	if (aChunk == aLastChunk)
						aLastCh = *(apSrc + aLgth - 1);
					cpTextCursor->append(aText, aLgth, false/*SetCursor*/);
				}
				else
					break;
			}
			aProgress.setValue(aLastChunk + 1);
		}
		else // Small File. Just read the file.
		{	for (aChunk = 0; aChunk <= aLastChunk; ++aChunk)
			{	// Append. Add text into text document
				if ((aLgth = aFile.read(apSrc, aChunkSz)) > 0)
				{	if (aChunk == aLastChunk)
						aLastCh = *(apSrc + aLgth - 1);
					cpTextCursor->append(aText, aLgth, false/*SetCursor*/);
				}
				else
					break;
			}
		}
		aFile.close();
		// Modified. Reset on read.
		editSetModified(false/*Modified*/);
		cpViewPort->update();
	}
	return aRet;
}

/*!
\brief editRedo - Carry out the last operation at cUndoCur.

\param iToMark  If true, redo to next mark; else, redo one operation.
\return void
\par Notes:
-# Calls updateContents to update
 */
void ATextEdit::editRedo(bool iToMark)
{
	if (cReadOnly)
	{	emit statusAlert("Redo: Text is read-only");
		cStaleAlert = true;
	}
	else if (!cpTextCursor->redo(iToMark))
	{	statusAlert(QString("Redo: Nothing left to redo"));
		cStaleAlert = true;
	}
	else
		updateCursor(true/*Repaint*/, false/*MoveToTop*/);
}

/*
\brief editReplace - Search for a pattern specified by Pattern and then replace it with text.

\param irPattern - Pattern to be matched
\param irText - String to replace matched text
\param iAllText - Search from cursor back around to the cursor.
\param iMatchCase - Iff true, the search is case sensitive.
\param iRegExp - If true, treat Pattern as a regular expression
\param iMatchWord - If true, pattern must start/end on a word boundry
\sa editFind for more info on find operation
\par Notes:
-# If pattern is matched, move cursor to end of matched text. Highlight matched text, then insert from the anchor.
-# If iMatchCase, search is case sensitive. (Foo does not match foo)
-# If iMatchWord, pattern must begin/end at word breaks. (foo does not match foobar)
-# If iRegExp, pattern is treated as a regular expression. (\d+foo matches 12foo)
-# If iAll, continue search from top/bottom of text back to the original cursor position.
-# iAll is ignored if iSelect is true.
 */
bool ATextEdit::editReplace(const QString& irPattern, const QString& irText, bool iAllText, bool iMatchCase, bool iRegExp,
bool iMatchWord)
{
	bool aMatch = false;		// True iff a match if last find resulted in a match.
	bool aUpdate = false;		// True iff at least one find resulted in a match.
	if (cReadOnly)
	{	emit statusAlert("Replace: Text is read-only");
		cStaleAlert = true;
		aMatch = false;
	}
	else if (cpTextCursor->cSelectedText)
	{	aMatch = cpTextCursor->find(irPattern, false/*All*/, iMatchCase, true/*Down*/, iRegExp, true/*Select*/,iMatchWord);
		if (aMatch)
		{	cpTextCursor->insert(eInsFwd, irText.toLatin1(), irText.length());
			aMatch = cpTextCursor->find(irPattern, iAllText, iMatchCase, true/*Down*/, iRegExp, false/*Select*/, iMatchWord);
			aUpdate = true;
			editSetModified(true);
		}
	}
	else if ((aMatch = cpTextCursor->find(irPattern, iAllText, iMatchCase, true/*Down*/, iRegExp, false/*Select*/, iMatchWord)))
		aUpdate = true;
	if (aUpdate)
		updateCursor(true/*Repaint*/, false/*MoveToTop*/);
	return aMatch;
}

/*
\brief editReplaceAll - Replace every matched text with the replace string

\param irPattern - Pattern to be matched
\param irText - String to be substituted for the matched text (may be empty)
\param iAllText - Search from cursor back around to the cursor.
\param iMatchCase - Iff true, the search is case sensitive.
\param iRegExp - If true, treat irFind as a regular expression
\param iSelect	 - If true, just search selected text
\param iMatchWord - If true, pattern must start/end on a word boundry
\return aMatch - True iff at least one match with irPattern found
\sa editFind for more info on find operation
 */
bool ATextEdit::editReplaceAll(const QString& irPattern, const QString& irText, bool iAllText, bool iMatchCase, bool iRegExp,
				bool iSelect, bool iMatchWord)
{
	bool aMatch = false;
	if (cReadOnly)
	{	emit statusAlert(QString("Replace All: Text is read-only"));
		cStaleAlert = true;
	}
	else
	{	aMatch = cpTextCursor->replaceAll(irPattern, irText, iAllText, iMatchCase, iRegExp, iSelect, iMatchWord);
		if (aMatch)
		{
			updateCursor(true/*Repaint*/, false/*MoveToTop*/);
			editSetModified(true);
		}
	}
	return aMatch;
}

/*!
\brief editSelectedText - Fetch the currently selected (highlighted) text, if any

\return aText - The currently selected text
 */
QByteArray ATextEdit::editSelectedText()
{
	QByteArray aText;
	cpTextCursor->selectedText(aText, 0);
	return aText;
}

/*!
\brief editSelectMatchedText - Convert matched text into selected text

\return aIsMatched - True iff matched text found
 */
bool ATextEdit::editSelectMatchedText()
{
	bool aIsMatched = cpTextCursor->selectMatchedText();
	if (aIsMatched)
	{	cCursorShow = true;
		cpViewPort->update();
	}
	else
	{	emit statusAlert(QString("SelectMatched: No matched text to select/deselect"));
		cStaleAlert = true;
	}
	return aIsMatched;
}

/*!
\brief editSetModified - Set the modified flag to iModified

\param iModified - The new value of the flag
\return void
 */
void ATextEdit::editSetModified(bool iModified)
{
	if (iModified != cTextModified)
		emit textModified(cTextModified = iModified);
}

/*!
\brief editSetParameter - Change a settable parameter

\param ipName -> Name of parameter to be set
\param irValue - New parameter value
\return aFoundIt - True iff a valid parameter name
\par Parameter Names:
Default, Font, FontSize, LineNumbers, MatchParens, MaximumRows, MaximumRows, MaximumRowWidth, MaximumUndoDepth,
ShowWhiteSpace, TabWidth, WordWrap
\par Language Values:
Text, Html, Cpp, Javascript, Lisp
 */
void ATextEdit::editSetParameter(const QString& irName, const QVariant& irValue)
{
	bool aFixFont = false;			// Revise current font
	bool aRebalance = false;		// Modify tab padding, reset wrap points
	bool aResetCursor = false;		// Reset cursor in text if text moved. 
	bool aUpdateCursor = false;		// Update the visible cursor in viewport
	QString aName(irName.toLower());
	if (aName == "font")
	{	cParameters.mFont = irValue.toString();
		aFixFont = true;
	}
	else if (aName == "fontsize")
	{	cParameters.mFontSize = irValue.toInt();
		aFixFont = true;
		aRebalance = true;
	}
	else if (aName == "language")
	{	QString aLanguage(irValue.toString().toLower());
		long aIdx = 0;
		if (aLanguage == "text")
			aIdx = 0;
		else if (aLanguage == "html")
			aIdx = 1;
		else if (aLanguage == "cpp")
			aIdx = 2;
		else if (aLanguage == "javascript")
			aIdx = 3;
		else if (aLanguage == "lisp")
			aIdx = 4;
		cParameters.mLanguage = aIdx;
		aRebalance = true;
	}
	else if (aName == "linenumbers")
	{	cParameters.mLineNumbers = irValue.toBool();
		cpTextCursor->refresh();
		aRebalance = true;
	}
	else if (aName == "matchparens")
	{	cParameters.mMatchParen = irValue.toBool();
		aUpdateCursor = true;
	}
	else if (aName == "maximumrows")
	{	long aMaxRows = irValue.toInt();
		if (aMaxRows <= 0)
			aMaxRows = LONG_MAX;
		cParameters.mMaxRows = aMaxRows;
		if (cpTextCursor->count() > cParameters.mMaxRows)
		{	if (cReadOnly)
			{	emit statusAlert("SetParameter(MaximumRows): Text is read-only");
				cStaleAlert = true;
			}
			else
			{	cpTextCursor->truncateTop(cParameters.mMaxRows);
				aResetCursor = true;
			}
		}
	}
	else if (aName == "maximumrowwidth")
	{	cParameters.mMaxRowWidth = irValue.toInt();
		aRebalance = true;;
	}
	else if (aName == "maximumundodepth")
		cParameters.mMaxUndoDepth = irValue.toInt();
	else if (aName == "matchparens")
	{	cParameters.mMatchParen = irValue.toBool();
		QPoint aCursor(-1, -1);
		if (cpTextCursor->move(eSame, aCursor))
			cpTextCursor->setCursor(aCursor, false/*ExtendSelect*/, false/*IncludeNewline*/, true/*SaveCur*/);
		aUpdateCursor = true;
	}
	else if (aName == "showwhitespace")
	{	cParameters.mShowWhiteSpace = irValue.toBool();
		aUpdateCursor = true;
	}
	else if (aName == "tabwidth")
	{	long aTabWidth = irValue.toInt();
		if (aTabWidth != cParameters.mTabWidth)
		{	cParameters.mTabWidth = aTabWidth;
			cpTextCursor->refresh();
			aRebalance = true;
		}
	}
	else if (aName == "wordwrap")
	{	cParameters.mWrap = irValue.toBool();
		setHorizontalScrollBarPolicy(cParameters.mWrap ? Qt::ScrollBarAlwaysOff : Qt::ScrollBarAlwaysOn); 
		aRebalance = true;
	}
	else if (aName == "default")
	{	cParameters.mFont = scDefaultFont;
		cParameters.mFontSize = scDefaultFontSize;
		cParameters.mLineNumbers = false;
		cParameters.mMatchParen = true;		// Probably should depend upon the language.
		cParameters.mMaxRowWidth = scDefaultMaxRowWidth;
		cParameters.mMaxRows = LONG_MAX;
		cParameters.mMaxUndoDepth = scDefaultMaxUndoDepth;
		cParameters.mShowWhiteSpace = false;
		cParameters.mTabWidth = scDefaultTabWidth;
		cParameters.mWrap = false;
		aFixFont = true;
	}
	else
	{	statusAlert(QString("SetParameter. Unknown parameter: %1.").arg(irName));
		cStaleAlert = true;
	}
	if (aFixFont)
	{ 	QString& arFontFamily = cParameters.mFont;
		QFont aFont(arFontFamily, cParameters.mFontSize, QFont::Normal, false/*Italic*/);

		if (aFont.family() != arFontFamily)
		{	qDebug() << "Can't find " << arFontFamily << ". Font set to " << aFont.family();
			beep();
		}

		setFont(aFont);
		// set the font in our viewport widget
		cpViewPort->setFont(aFont);

		// recompute parameters for font display
		QFontMetrics aFm = cpViewPort->fontMetrics();
		cParameters.mLeftOffset = scDefaultLeftOffset;	//  ~2 pixels
		cParameters.mCharWidth = aFm.width('W');		//  ~8 pixels for Courier New 10 pts.
		cParameters.mLineHeight = aFm.height();			// ~16 pixels for Courier New 10 pts.
		cParameters.mLineOffset = aFm.ascent();			// ~11 pixels for Courier New 10 pts.
		cParameters.mCursorHeight = aFm.height();
		
		// adjust viewport settings, same computation as in the constructor
		cViewPortWidth = cpViewPort->width() / cParameters.mCharWidth;
		if (cViewPortWidth < 128)
			cViewPortWidth = 128;
		cViewPortHeight = (cpViewPort->height() + cParameters.mLineHeight - 1) / cParameters.mLineHeight;

		cpHorizScrollBar->setPageStep(cViewPortWidth);
		cpHorizScrollBar->setMaximum(cParameters.mMaxRowWidth - cViewPortWidth);
		cpVertScrollBar->setPageStep(cViewPortHeight);
		cpVertScrollBar->setMaximum(0);
		cpTextCursor->setViewSize(QSize(cViewPortWidth, cViewPortHeight));
		// update scrollbars
		onRowCountChanged(cpTextCursor->count());

		aRebalance = true;
	}
	if (aRebalance)
	{	cpTextCursor->rebalance();
		aResetCursor = true;
	}
	if (aResetCursor)
	{	cpTextCursor->setCursor(QPoint(-1, -1), false/*Select*/, false/*IncludeNewline*/, false/*SaveCur*/);
		aUpdateCursor = true;
	}
	if (aUpdateCursor)
		updateCursor(true/*Repaint*/, false/*MoveToTop*/);
}

/*!
\brief editSetReadOnly - Enable/disable most edit operations

In read-only mode: comment, cut, delete, indent, insert, paste, redo, replace, replaceAll, runCommand, tabify, undo
are not allowed.  Clear, append, and truncate are still allowed.
\param iEnable - Enable/disable flag
\return void
 */
void ATextEdit::editSetReadOnly(bool iEnable)
{
	cReadOnly = iEnable;
}

/*!
\brief editShow - Display item in debug window of IDE.

\param irItem - Item may be "cursors", "row", "text", or "undo".
\param iRow - Row to be shown
\return void
\par Notes:
-# If irItem is:
- cursor: Show anchor, cursor, eot values (iRow ignored)
- row: Display ATextRow information for 3 rows near iRow. If iRow < 0, just show one row.
- text: Display 3 rows of of text near iRow. If iRow < 0, show all text
- undo: Display undo operation at current position + iRow. if iRow large negative, show entire queue.
-# For undo operations:
- iRow	Operation
- 1		previous redo operation
- 0		redo operation
- -1	undo operation
- -2	previous undo operation
-# cUndoCur > 0:  cUndoCur undo operations pending
-# cUndoCur < LastItem:  LastItem - cUndoCur redo operations pending
 */
QString ATextEdit::editShow(const QString& irItem, long iRow)
{
	QString aMsg;
	if (irItem == "cursors")
	{	QPoint aAnchor, aCursor, aEot;
		aAnchor = cpTextCursor->cursor(eAnchor);
		aCursor = cpTextCursor->cursor(eCursor);
		aEot = cpTextCursor->cursor(eEnd);
		// Convert. Convert from internal coordinates to user coordinates
		aMsg = QString("Anchor x:%1 y:%2, Cursor x:%3 y:%4, End-of-Text x:%5 y:%6")
			.arg(aAnchor.x()+1).arg(aAnchor.y()+1).arg(aCursor.x()+1).arg(aCursor.y()+1).arg(aEot.x()+1).arg(aEot.y()+1);
	}
	else if (irItem == "row")
	{	long aEndY = cpTextCursor->cursor(eEnd).y();
		if (iRow < 0)
		{	iRow = (-iRow > aEndY) ? aEndY : -iRow;	
			aMsg = cpTextCursor->dumpRow(iRow);
		}
		else
		{	iRow = (iRow < 0) ? 0 : (iRow > aEndY) ? aEndY : iRow;	
			if (iRow > 0)
				aMsg = cpTextCursor->dumpRow(iRow - 1) + '\n';
			aMsg += cpTextCursor->dumpRow(iRow);
			if (iRow < aEndY)
				aMsg += '\n' + cpTextCursor->dumpRow(iRow + 1);
		}
	}
	else if (irItem == "text")
	{	if (iRow < 0)
			aMsg = editText();
		else
		{	long aEndY = cpTextCursor->cursor(eEnd).y(), aLgth = 0;
			QByteArray aText;
			char aNewline[] = "n\n";
			aNewline[0] = scCharNewline;
			if (iRow > aEndY) iRow = aEndY;
			if (iRow > 0)
			{	aLgth = cpTextCursor->text(iRow - 1, aText, aLgth);
				if (aText.at(aLgth-1) != '\n')
					aText += '\n', ++aLgth;
			}
			aLgth = cpTextCursor->text(iRow, aText, aLgth);
			if (iRow < aEndY)
			{	if (aText.at(aLgth-1) != '\n')
					aText += '\n', ++aLgth;
				aLgth = cpTextCursor->text(iRow + 1, aText, aLgth);
				if (aText.at(aLgth-1) == '\n')
					aText.chop(1);
			}
			aMsg = aText.replace('\n', aNewline).replace('\t', scCharTab);
		}
	}
	else if (irItem == "undo")
		aMsg = cpTextCursor->dumpUndo(iRow);
	return aMsg;
}

/*!
\brief editSwitch - Interchange the current anchor and cursor. Leave current highlighting, if any in place.

\return aSwitch - True iff cursor and anchor modified
 */
bool ATextEdit::editSwitch()
{
	bool aOk ;
	if ((aOk = cpTextCursor->switchCursors()))
		updateCursor(false/*Repaint*/, false/*MoveToTop*/);
	return aOk;
}

/*!
\brief editTabify - Convert leading spaces/tabs in the selected rows into tabs or vice versa.

\param iExtraSpaces - -1, delete leftover spaces; 0, keep leftover spaces; +1 convert leftover spaces to tabs
\param iToTabs - If true, convert to spaces to tabs; else, convert tabs to spaces
\return aOk - True iff at least one row is currently selected
\par Notes:
-# ExtraSpaces is ignored if converting tabs to spaces.
 */
bool ATextEdit::editTabify(long iExtraSpaces, bool iToTabs)
{
	bool aOk = false;
	if (cReadOnly)
	{	emit statusAlert("Tabify: Text is read-only");
		cStaleAlert = true;
	}
	else if (cpTextCursor->tabify(iExtraSpaces, iToTabs))
	{	cpTextCursor->clearUndo();
		editSetModified(aOk = true);		// Text was modified.
		updateCursor(true/*Repaint*/, false/*MoveToTop*/);
	}
	return aOk;
}

/*!
\brief editText - Return the entire edit buffer

\return aText - Entire document.
\par Notes:
-# If iRow < 0, return the entire text.
 */
QByteArray ATextEdit::editText()
{
	QByteArray aText;
	long aLgth = 0;
	cpTextCursor->text(-1, aText, aLgth);
	return aText;
}

/*!
\brief editTextLength - Returns the total number of characters in the edit buffer

\return Number of characters in the text
 */
long ATextEdit::editTextLength()
{
	return cpTextCursor->textLength();
}

/*!
\brief editTextRows - Returns the total number of rows in the edit buffer

\return aSize	Number of rows in text buffer
 */
long ATextEdit::editTextRows()
{
	return cpTextCursor->textRows();
}

/*!
\brief editUndo - Reverse the last insert or delete operation.

\param iToMark  If true, undo to next mark; else, undo one operation.
\return void
*/
void ATextEdit::editUndo(bool iToMark)
{
	if (cReadOnly)
	{	emit statusAlert("Undo: Text is read-only");
		cStaleAlert = true;
	}
	else if (cpTextCursor->undo(iToMark))
		updateCursor(true/*Repaint*/, false/*MoveToTop*/);
	else
	{	statusAlert(QString("Undo: Nothing left to undo"));
		cStaleAlert = true;
	}
}

/*!
editWord - Return any word near the current cursor position.

\param iSelectWord  Select (highlight) the word.
\return aWord - Word near cursor. Returns empty string if no word found.
\Note:
-# If iSelect is false, the cursor is not moved and the currently selected text is not changed.
-# If iSelect is true, the cursor is moved to the eow and the anchor is moved to the bow.
 */
QString ATextEdit::editWord(bool iSelect)
{
	// Next Char. Move forward to next char in case at bow so that move back will find the word on the right.
	QPoint aSow(-1, -1);
	QString aWord;
	// Bow. Move back to bow, or else move forward to next bow.
	if (cpTextCursor->move(ePrevSow, aSow) || cpTextCursor->move(eNextSow, aSow))
	{	// Set cursor moves the anchor to bow and unhighlights any selected text.
		if (iSelect)
			cpTextCursor->setCursor(aSow, false/*Extend*/, false/*IncludeNewline*/, true/*SaveCur*/);
		// Eow. Now select from bow to eow.
		QPoint aEow(aSow);
		if (cpTextCursor->move(eNextEow, aEow))
		{	if (iSelect)
				updateCursor(cpTextCursor->setCursor(aEow,true/*Extend*/,false/*Newline*/,true/*SaveCur*/),false/*MoveToTop*/);
			aWord = cpTextCursor->mid(aSow, aEow);
		}
	}
	return aWord;
}

/*!
editWrite - Write the entire edit buffer into the file specified by irFileSpec

\param irFileSpec - Path + full name of file to receive text
\return aFinished - True iff write succeeds
 */
bool ATextEdit::editWrite(const QString& irFileSpec)
{
	QFile aFile(irFileSpec);
	bool aRet = true;
	if (!aFile.open(QIODevice::Text | QIODevice::WriteOnly))	// 16 | 2
	{	emit statusAlert(("editWrite, Unable to open " + irFileSpec));
		cStaleAlert = true;
		aRet = false;
	}
	else
	{	// Write. Write each row from the document. A row may be empty.
		long aCount = cpTextCursor->count(), aLgth = 0, aRow;
		QByteArray aText;
		for (aRow = 0; aRow < aCount; ++aRow)
		{	aLgth = cpTextCursor->text(aRow, aText, aLgth);
			if (aLgth > 0)
			{	aFile.write(aText.data(), aLgth);
				aText.clear();
				aLgth = 0;
			}
		}
		aFile.close();
		editSetModified(false/*Modified*/);	// Changes have been saved
	}
	return aRet;
}
 
/*	---------------------------------------------------------------------------------------------------------------------------
event - Reimplement event in order to catch tab key presses
Args:
	ipEv	Ptr to the event emitted by main event loop
Returns:
	True if event processed by parent
Notes:
 1.	keyPressEvent does not receive Tab keypresses.  It is necessary to catch tab key press here.
	------------------------------------------------------------------------------------------------------------------------ */
bool ATextEdit::event(QEvent* ipEvent)
{
	bool aAccept = true;
	switch (ipEvent->type())
	{// Tab. Send tab where it should have gone in the first place.
	case QEvent::KeyPress:
	{	QKeyEvent* apKeyEvent = static_cast<QKeyEvent*>(ipEvent);
		long aKey = apKeyEvent->key();
		if (aKey != Qt::Key_Control && aKey != Qt::Key_Shift)
		{	// NOTE: key() only returns capital letters so use text to get the correct code (ugh!!)
			Qt::KeyboardModifiers aKeyState = apKeyEvent->modifiers();
			if (aKeyState == Qt::NoModifier && aKey >= 'A' && aKey <= 'Z')
				aKey = apKeyEvent->text().at(0).toAscii();
			keyPress(aKey, aKeyState);
		}
		break;
	}
	// Focus.
	case QEvent::FocusIn:
		focusInEvent(static_cast<QFocusEvent*>(ipEvent));
		break;
	case QEvent::FocusOut:
		focusOutEvent(static_cast<QFocusEvent*>(ipEvent));
		break;
	default:
		aAccept = QAbstractScrollArea::event(ipEvent);
		break;
	}
	return aAccept;
}

/*	---------------------------------------------------------------------------------------------------------------------------
emitRunCommand - Get the current line and emit a signal to run the command
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::emitRunCommand()
{
	QByteArray aCurLine;
	if (cReadOnly)
	{	emit statusAlert("Run Command: Text is read-only");
		cStaleAlert = true;
	}
	else
	{	cpTextCursor->text(cpTextCursor->cCursor.y(), aCurLine, 0);
		emit runCommand(aCurLine);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
findNext - Find the next match in the edit buffer. Search from cCursor forward.
Args:
	iMove		Search forward iff iMove is eEot
	iWrap	Search past top/bottom
Returns:
	aMatched	True iff a match is found
Notes:
	------------------------------------------------------------------------------------------------------------------------ */
bool ATextEdit::findNext(AMoveTo iMove, bool iWrap)
{
	bool aDown = (iMove == eEot), aMatch = false;
	if (!cFindPattern.isEmpty())
	{	aMatch = cpTextCursor->find(cFindPattern, iWrap, cFindCase, aDown, cFindRegExp, cFindSelect, cFindWords);
		if (aMatch)
			updateCursor(true/*Repaint*/, false/*MoveToTop*/);
		else
		{	emit statusAlert("No more matches");
			cStaleAlert = true;
		}
	}
	return aMatch;
}

/*	---------------------------------------------------------------------------------------------------------------------------
focusInEvent - Reimplement this method to start/stop cursor
Args:
	ipFocusEv	Passed on to parent widget
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::focusInEvent(QFocusEvent* ipFocusEvent)
{
    Q_UNUSED(ipFocusEvent);

	// Restart cursor timer
	if (!cpBlinkTimer->isActive())
		cpBlinkTimer->start(QApplication::cursorFlashTime() / 2);
}

/*	---------------------------------------------------------------------------------------------------------------------------
focusOutEvent - Reimplement this method to start/stop cursor
Args:
	ipFocusEv	Passed on to parent widget
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::focusOutEvent(QFocusEvent* ipFocusEvent)
{
    Q_UNUSED(ipFocusEvent);

	// Stop cursor timer
	cpBlinkTimer->stop();
}

/*!
\brief getVersion - Get current version number

\return aVer - String holding version number.
\par Notes:
-# Currently, the version number matches the AIS version number
 */
QString ATextEdit::getVersion()
{
	return QString();
}

/*!
\brief initializeStaticDefs - Initialize static language definitions used to perform syntax highlighting.

\param ipParent -> Parent widget that hosts this instance of ATextEdit.
\param irTestFileName - Name of test suite file used by the testDialog
\return void
\par Notes:
-# Specifically, set background colors and pen colors for each color index. Initialize language structures in scLanguages.
-# Initialize the edit dialogs, use one common dialog for all edit windows.
-# Must be called just once after the parameters have been set but prior to first use of this class.
-# See also comments in atextstaticdefs.h.
  */
void ATextEdit::initializeStaticDefs(QWidget* ipParent, const QString& irTestFileName)
{
    Q_UNUSED(ipParent);

	// Initialize QColor arrays from the color lists defined in atextstaticdefs.h
	QStringList aList = scBgColorNames.split(QChar(','), QString::KeepEmptyParts, Qt::CaseSensitive);
	long i, j, aLgth = aList.size();
	QColor aColor;
	for (i = 0; i < aLgth; ++i)
	{	aColor.setNamedColor(aList[i]);
		scBgBrushes[i].setStyle(Qt::SolidPattern);
		scBgBrushes[i].setColor(aColor);
	}
	aList = scPenColor.split(QChar(','), QString::KeepEmptyParts, Qt::CaseSensitive);
	aLgth = aList.size();
	for (i = 0; i < aLgth; ++i)
		scPenColors[i].setNamedColor(aList[i]);

	// Initialize language colorization for each language definition.
	ALangDef* apDef;
	for (i = 0; (apDef = scpLangDefs[i]) != NULL; ++i)
	{	// Init. Initialize the next language structure. See atextstaticdefs.h for more details.	
		ALanguage& arLang = scLanguages[i];
		arLang.mBeg1[0] = arLang.mBeg2[0] = '\0';		// Begin-comment delimiters
		arLang.mComment = '\0';							// Delimiter used for commenting out code
		arLang.mEnd1[0] = arLang.mEnd2[0] = '\n';		// End-comment delimiters
		arLang.mEscape = '\0';							// Escape character used in quoted strings.
		arLang.mFcnPattern.truncate(0);					// Reg exp to detect function declaration
		arLang.mKeywords.clear();						// Keyword list
		arLang.mQuote1 = '\0';							// String delimiter
		arLang.mQuote2 = '\0';							// Alternate string delimiter

		// Extensions. Set the language for each file extension in the list of file extensions for this language
		aList = QString(apDef->spExt).split(QChar(','), QString::KeepEmptyParts, Qt::CaseSensitive);
		aLgth = aList.size();
		for (j = 0; j < aLgth; ++j)
			scFileExts[aList[j]] = (ALanguageType)i;

		// Quotes. Set quote characters for each language.
		if ((arLang.mQuote1 = apDef->spQuote[0]) != '\0')
			arLang.mQuote2 = apDef->spQuote[1];

		// Escape Char. Set escape char, if any.
		arLang.mEscape = apDef->spEscape[0];

		// Comments. Set comment delimiters in scLanguages entry from language info from .h files
		aList = QString(apDef->spComment).split(QChar(','), QString::KeepEmptyParts, Qt::CaseSensitive);
		aLgth = aList.size();
		switch (aLgth)
		{	// fall thru each case.
		case 4:
			strncpy(arLang.mEnd2, aList[3].toAscii().data(), 4);
			arLang.mEnd2[4] = '\0';
		case 3:
			strncpy(arLang.mBeg2, aList[2].toAscii().data(), 4);
			arLang.mBeg2[4] = '\0';
		case 2:
			strncpy(arLang.mEnd1, aList[1].toAscii().data(), 4);
			arLang.mEnd1[4] = '\0';
		case 1:
			strncpy(arLang.mBeg1, aList[0].toAscii().data(),4);
			arLang.mBeg1[4] = '\0';
			arLang.mComment = arLang.mBeg1[0];		// Comment delimiter for commenting out selected code.. 
			break;
		}
		//	Functions. Initialize function reg-exp pattern used to recognize functions from .h file info.
		arLang.mFcnPattern = apDef->spFcnPattern;

		//	Keywords. Initialize keyword map form list of keywords in .h file
		aList = QString(apDef->spKeywords).split(QChar(','), QString::KeepEmptyParts, Qt::CaseSensitive);
		aLgth = aList.size();
		for (j = 0; j < aLgth; ++j)
			arLang.mKeywords[aList[j]] = 1;
	}

	// Initialize dialogs
	//scpGoDialog = new AGoDialog(NULL, "GoDialog");
	scpTabifyDialog = new ATabifyDialog(NULL, "TabifyDialog");
	scpTabWidthDialog = new ATabWidthDialog(NULL, "TabWidthDialog");
	scpTestDialog = new ATestDialog(irTestFileName, NULL, "TestDialog");
}

/*	---------------------------------------------------------------------------------------------------------------------------
initializeParameters - Initialize the font family and the font size
Args:
	iLanguage - Index into scLangDefs for syntax highlighting 
Returns:
	nothing
Notes:
 1.	mFont is a fixed pitch font such as: Courier, Courier New, Fixedsys, Lucida Console, Terminal
 2. mFontSize font size in points (72pts/inch). Standard font sizes are 8-12,14, 16, 18, 20
 	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::initializeParameters(ALanguageType iLanguage)
{
	cParameters.mFont = scDefaultFont;
	cParameters.mFontSize = scDefaultFontSize;
	cParameters.mLineNumbers = false;
	cParameters.mMaxRowWidth = scDefaultMaxRowWidth;
	cParameters.mMaxUndoDepth = scDefaultMaxUndoDepth;
	cParameters.mShowWhiteSpace = false;
	cParameters.mTabWidth = scDefaultTabWidth;
	cParameters.mWrap = false;

	QString& arFontFamily = cParameters.mFont;
	QFont aFont(arFontFamily, cParameters.mFontSize, QFont::Normal, false/*Italic*/);
	if (aFont.family() != arFontFamily)
	{	qDebug() << "Can't find " << arFontFamily << ". Font set to " << aFont.family();
		beep();
	}
	setFont(aFont);
	cpViewPort->setFont(aFont);
	QFontMetrics aFm = cpViewPort->fontMetrics();
	// Deduct one from line height to avoid gap between rows.
	cParameters.mLeftOffset = scDefaultLeftOffset;	//  ~2 pixels
	cParameters.mCharWidth = aFm.width('W');		//  ~8 pixels for Courier New 10pts
	cParameters.mLanguage = iLanguage;				//  0 for text
	cParameters.mLineHeight = aFm.height();			// ~16 pixels for Courier New 10pts
	cParameters.mLineOffset = aFm.ascent();			// ~11 pixels for Courier New 10pts
	cParameters.mCursorHeight = aFm.height();		// Height of blinking cursor ~12[pixels]

	cParameters.mCursorWidth = 2;					// Width of blinking cursor ~2[pixels]
	cParameters.mRightMargin = 4;					// Horizontal margin on right of viewport [chars]
	cParameters.mBottomMargin = 2;					// Vertical margin at bottom of viewport [lines]
}

/*	---------------------------------------------------------------------------------------------------------------------------
keyPress - Reimplement inherited version to catch all key presses including a Tab.
Args:
	ipKeyEv	->Information about the key press
Returns:
	nothing
Notes:
 1.	See event() for Tab key press events
 2.	Key codes are shown below.
 3.	State is Qt::ShiftButton, ControlButton, AltButton or MetaButton
	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::keyPress(long iKey, Qt::KeyboardModifiers iKeyState)
{
	// ATextEdit::keyPress(). Make sure all keystrokes are included in the help file.
	// Note key pressed and shift-key state
	char aAscii = '\0';
	bool aAltKey = (iKeyState == Qt::AltModifier);		// Alt key
	bool aCtrlKey = (iKeyState == Qt::ControlModifier);	// Control key
	bool aShiftKey = (iKeyState == Qt::ShiftModifier);	// Shift key
	bool aCtrlShiftKey = (iKeyState == (Qt::ShiftModifier | Qt::ControlModifier));	// Ctrl & Shift
	bool aExtendSelect = false;		// Set to true for shifted-move operations
	ACmdType aCmd = eNone;			// eNone, eDelFwd, eDelBack, eInsFwd, eInsBack, eMoveFwd, eMoveBack
	AMoveTo aMove = eNoMove;		// Move. For move and delete operations.

	if (aAltKey)
	{	switch (iKey)
		{
		case Qt::Key_F1:
			dialogHelpAlpha();
			break;
		default:
			break;
		}
	}
	else if (aCtrlShiftKey)
	{	switch (iKey)
		{
		case Qt::Key_Delete:
			cpTextCursor->clearUndo();
			break;
		case Qt::Key_Home:
			aCmd = eMoveBack;
			aMove = eBot;
			aExtendSelect = true;
			break;
		case Qt::Key_End:
			aCmd = eMoveFwd;
			aMove = eEot;
			aExtendSelect = true;
			break;
		case Qt::Key_Left:
			aCmd = eMoveBack;
			aMove = ePrevBow;
			aExtendSelect = true;
			break;
		case Qt::Key_Up:
			aCmd = eMoveBack;
			aMove = ePrevHalfPage;
			aExtendSelect = true;
			break;
		case Qt::Key_Right:
			aCmd = eMoveFwd;
			aMove = eNextBow;
			aExtendSelect = true;
			break;
		case Qt::Key_Down:
			aCmd = eMoveFwd;
			aMove = eNextHalfPage;
			aExtendSelect = true;
			break;
		case Qt::Key_PageUp:
			aCmd = eMoveBack;
			aMove = ePrevCur;
			aExtendSelect = true;
			break;
		case Qt::Key_PageDown:
			aCmd = eMoveFwd;
			aMove = eNextCur;
			aExtendSelect = true;
			break;
		case Qt::Key_F3:
			findNext(eBot, true/*Wrap*/);
			break;
		case Qt::Key_F5:
			testDialog();
			break;
		case Qt::Key_Backslash:		// CtrlShift-backslash
			aAscii = '\177';
			break;
		case Qt::Key_Semicolon:		// Ctrl-Colon
			editComment(false);
			break;
		case Qt::Key_A:				// Highlight from cursor to end.
			aCmd = eMoveFwd;
			aMove = eEot;
			aExtendSelect = true;
			break;
		case Qt::Key_B:
			aCmd = eMoveBack;
			aMove = eBol;
			aExtendSelect = true;
			break;
		case Qt::Key_C:
			// Append selected text to clipboard.
			editCopy(true/*append*/);
			break;
		case Qt::Key_E:
			aCmd = eMoveFwd;
			aMove = eEol;
			aExtendSelect = true;
			break;
		case Qt::Key_H:
			aCmd = eMoveBack;
			aMove = ePrevChar;
			aExtendSelect = true;
			break;
		case Qt::Key_I:
			aCmd = eMoveFwd;
			aMove = ePrevTab;
			aExtendSelect = true;
			break;
		case Qt::Key_J:
			aCmd = eMoveFwd;
			aMove = eNextLine;
			aExtendSelect = true;
			break;
		case Qt::Key_K:
			aCmd = eMoveBack;
			aMove = ePrevLine;
			aExtendSelect = true;
			break;
		case Qt::Key_L:
			aCmd = eMoveFwd;
			aMove = eNextChar;
			aExtendSelect = true;
			break;
		case Qt::Key_M:
			editSelectMatchedText();
			break;
		case Qt::Key_N:
			aCmd = eMoveFwd;
			aMove = eNextBow;
			aExtendSelect = true;
			break;
		case Qt::Key_P:
			aCmd = eMoveBack;
			aMove = ePrevBow;
			aExtendSelect = true;
			break;
		//case Qt::Key_T:
		//	tabWidthDialog();
		//	break;
		case Qt::Key_V:
			editPaste();
			break;
		case Qt::Key_X:
			editCut(true/*append*/);
			break;
		case Qt::Key_Y:
			editRedo(false/*ToMark*/);
			break;
		case Qt::Key_Z:
			editUndo(false/*ToMark*/);
			break;
		default:
			break;
		}
	}
	else if (aCtrlKey) 		//	Ctrl+ascii keys
	{	switch (iKey)
		{
		case Qt::Key_Backspace:
			aCmd = eDelBack;
			aMove = ePrevBow;
			break;
		case Qt::Key_Return:
		case Qt::Key_Enter:
			emitRunCommand();
			break;
		case Qt::Key_Delete:
			aCmd = eDelFwd;
			aMove = eNextBow;
			break;
		case Qt::Key_Insert:
			editSetParameter("MatchParens", QVariant(!cParameters.mMatchParen));
			emit statusAlert((cParameters.mMatchParen)? "Match Parens ON": "Match Parens OFF");
			cStaleAlert = true;
			break;
		case Qt::Key_Home:
			aCmd = eMoveBack;
			aMove = eBot;
			break;
		case Qt::Key_End:
			aCmd = eMoveFwd;
			aMove = eEot;
			break;
		case Qt::Key_Left:
			aCmd = eMoveBack;
			aMove = ePrevBow;
			break;
		case Qt::Key_Up:
			aCmd = eMoveBack;
			aMove = ePrevHalfPage;
			break;
		case Qt::Key_Right:
			aCmd = eMoveFwd;
			aMove = eNextBow;
			break;
		case Qt::Key_Down:
			aCmd = eMoveFwd;
			aMove = eNextHalfPage;
			break;
		case Qt::Key_PageUp:
			aCmd = eMoveBack;
			aMove = ePrevCur;
			break;
		case Qt::Key_PageDown:
			aCmd = eMoveFwd;
			aMove = eNextCur;
			break;
		case Qt::Key_F1:
			dialogHelpApi();
			break;
		case Qt::Key_F3:
			findNext(eEot, true/*Wrap*/);
			break;
		case Qt::Key_Period:	// Ctrl-GreaterThan
			aCmd = eDelFwd;
			aMove = eEol;
			break;
		case Qt::Key_Semicolon:	// Ctrl-Colon
			editComment(true/*Insert*/);
			break;
		case Qt::Key_0:
		case Qt::Key_9:
			editSetParameter("MatchParens", QVariant(!cParameters.mMatchParen));
			emit statusAlert((cParameters.mMatchParen)? "Match Parens ON": "Match Parens OFF");
			cStaleAlert = true;
			break;
		case Qt::Key_A:
			// First move to the beg-of-text, then move to end
			cpTextCursor->setCursor(QPoint(0, 0), false/*Select*/, false/*IncludeNewline*/, true/*SaveCur*/);
			aCmd = eMoveFwd;
			aMove = eEot;
			aExtendSelect = true;
			break;
		case Qt::Key_B:
			aCmd = eMoveBack;
			aMove = eBol;
			break;
		case Qt::Key_C:
			editCopy();
			break;
		case Qt::Key_D:
			aCmd = eDelFwd;
			aMove = eNextChar;
			break;
		case Qt::Key_E:
			aCmd = eMoveFwd;
			aMove = eEol;
			break;
		case Qt::Key_H:
			aCmd = eMoveBack;
			aMove = ePrevChar;
			break;
		case Qt::Key_I:
			aAscii = '\t';
			break;
		case Qt::Key_J:
			aCmd = eMoveFwd;
			aMove = eNextLine;
			break;
		case Qt::Key_K:
			aCmd = eMoveBack;
			aMove = ePrevLine;
			break;
		case Qt::Key_L:
			aCmd = eMoveFwd;
			aMove = eNextChar;
			break;
		case Qt::Key_M:
			editSelectMatchedText();
			break;
		case Qt::Key_N:
			aCmd = eMoveFwd;
			aMove = eNextBow;
			break;
		case Qt::Key_P:
			aCmd = eMoveBack;
			aMove = ePrevBow;
			break;
		case Qt::Key_T:
			tabifyDialog();
			break;
		case Qt::Key_U:
			aCmd = eDelBack;
			aMove = eBol;
			break;
		case Qt::Key_V:
			editPaste();
			break;
		case Qt::Key_X:
			editCut();
			break;
		case Qt::Key_Y:
			editRedo(true/*ToMark*/);
			break;
		case Qt::Key_Z:
			editUndo(true/*ToMark*/);
			break;
		case Qt::Key_6:
		case Qt::Key_AsciiCircum:
			editSwitch();
			break;
		default:
			break;
		}
	}
	else if (aShiftKey)
	{	switch (iKey)
		{
		case Qt::Key_Home:
			aCmd = eMoveBack;
			aMove = eBol;
			aExtendSelect = true;
			break;
		case Qt::Key_End:
			aCmd = eMoveFwd;
			aMove = eEol;
			aExtendSelect = true;
			break;
		case Qt::Key_Insert:
			editSetParameter("LineNumbers", QVariant(!cParameters.mLineNumbers));
			emit statusAlert((cParameters.mLineNumbers)? "Line Numbers ON": "Line Numbers OFF");
			cStaleAlert = true;
			break;
		case Qt::Key_Left:
			aCmd = eMoveBack;
			aMove = ePrevChar;
			aExtendSelect = true;
			break;
		case Qt::Key_Up:
			aCmd = eMoveBack;
			aMove = ePrevLine;
			aExtendSelect = true;
			break;
		case Qt::Key_Right:
			aCmd = eMoveFwd;
			aMove = eNextChar;
			aExtendSelect = true;
			break;
		case Qt::Key_Down:
			aCmd = eMoveFwd;
			aMove = eNextLine;
			aExtendSelect = true;
			break;
		case Qt::Key_PageUp:
			aCmd = eMoveBack;
			aMove = ePrevPage;
			aExtendSelect = true;
			break;
		case Qt::Key_PageDown:
			aCmd = eMoveFwd;
			aMove = eNextPage;
			aExtendSelect = true;
			break;
		case Qt::Key_F1:
			dialogHelpFind();
			break;
		case Qt::Key_F3:
			findNext(eBot, false/*Wrap*/);
			break;
		default:
			if (iKey < 256)
				aAscii = iKey;
			break;
		}
	}
	else	// No modifiers
	{	switch (iKey)
		{
		case Qt::Key_Tab:
			if (cpTextCursor->cSelectedText)
				editIndent(true/*In*/);
			else
				aAscii = '\t';
			break;
		case Qt::Key_Backtab:
			if (cpTextCursor->cSelectedText)
				editIndent(false/*In*/);
			else
			{	aCmd = eMoveBack;
				aMove = ePrevTab;
			}
			break;
		case Qt::Key_Backspace:
			aCmd = eDelBack;
			aMove = ePrevChar;
			break;
		case Qt::Key_Return:
		case Qt::Key_Enter:
			aAscii = '\n';
			break;
		case Qt::Key_Delete:
			aCmd = eDelFwd;
			aMove = eNextChar;
			break;
		case Qt::Key_Home:
			aCmd = eMoveBack;
			aMove = eBol;
			break;
		case Qt::Key_End:
			aCmd = eMoveFwd;
			aMove = eEol;
			break;
		case Qt::Key_Insert:
			editSetParameter("ShowWhiteSpace", QVariant(!cParameters.mShowWhiteSpace));
			emit statusAlert((cParameters.mShowWhiteSpace)? "Show Whitespace ON": "Show Whitespace OFF");
			cStaleAlert = true;
			break;
		case Qt::Key_Left:
			aCmd = eMoveBack;
			aMove = ePrevChar;
			break;
		case Qt::Key_Up:
			aCmd = eMoveBack;
			aMove = ePrevLine;
			break;
		case Qt::Key_Right:
			aCmd = eMoveFwd;
			aMove = eNextChar;
			break;
		case Qt::Key_Down:
			aCmd = eMoveFwd;
			aMove = eNextLine;
			break;
		case Qt::Key_PageUp:
			aCmd = eMoveBack;
			aMove = ePrevPage;
			break;
		case Qt::Key_PageDown:
			aCmd = eMoveFwd;
			aMove = eNextPage;
			break;
		case Qt::Key_F1:
			dialogHelp();
			break;
		case Qt::Key_F3:
			findNext(eEot, false/*Wrap*/);
			break;
		case 0x1c:					// Ctrl-backslash 
			aAscii = '\177';
			break;
		default:
			if (iKey < 256)
				aAscii = iKey;
			break;
		}
	}
	QPoint aCursor(-1, -1);
	QString aMsg;
	bool aRepaint = true;
	if (aAscii != '\0')
	{	if (cReadOnly)
			aMsg = "Insert: Text is read-only";
		else 
		{	cpTextCursor->insert(aAscii);
			if (aAscii != '\t')
				aRepaint = true;		// PENDING ATextEdit::keyPress() -  Just update previous char.
			updateCursor(aRepaint, false/*MoveToTop*/);
		}
	}
	else if (aCmd == eMoveBack || aCmd == eMoveFwd)
	{	if (cpTextCursor->move(aMove, aCursor))
			updateCursor(cpTextCursor->setCursor(aCursor, aExtendSelect, false/*Newline*/, true/*SaveCur*/), false/*MoveToTop*/);
		else
			aMsg = (aCmd == eMoveFwd) ? "At End" : "At Beginning";
	}
	else if (aCmd == eDelBack || aCmd == eDelFwd)
	{	if (cReadOnly)
			aMsg = "Delete: Text is read-only";
		else
		{	// Wrap. If at end of wrapped line,  move to the beginning of the next line
			if (aCmd == eDelFwd && cpTextCursor->move(eNextBol, aCursor))
				cpTextCursor->setCursor(aCursor, editIsSelected(), false/*Newline*/, false/*SaveCur*/);

			// Delete. Delete from the cursor to the destination specified by aMove.
			if (!editDelete(aMove))
				aMsg = (aCmd == eDelFwd) ? "At End" : "At Beginning";
		}
	}
	else
		return;	// Probably just a lone shift key or a lone control key

	if (aMsg.isEmpty())
	{	if (cStaleAlert)
		{	emit statusAlert(aMsg);
			cStaleAlert = false;
		}
		if (!cTextModified && !(aCmd == eMoveBack || aCmd == eMoveFwd))
			editSetModified(true/*Modified*/);
	}
	else
	{	emit statusAlert(aMsg);
		cStaleAlert = true;
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
mouseDoubleClickEvent - Extend current selected if shift key pressed, then select nearby word.
Args:
	ipMseEvent	-> mouse event which contains the current mouse position.
Returns:
	nothing
Calls:
Notes:
 1.	If just before an opening paren or just after a closing paren, select matching text. 
	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::mouseDoubleClickEvent(QMouseEvent* ipMouseEvent)
{
    Q_UNUSED(ipMouseEvent);

	if (editWord(true/*Select*/).isEmpty())
	{	emit statusAlert("No nearby word found");
		cStaleAlert = true;
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
mouseMoveEvent - Set the text cursor to the current mouse position
Args:
	ipMouseEvent	Ptr to mouse event which contains the current mouse position.
Returns:
	nothing
Notes:
 1.	Used to select text
	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::mouseMoveEvent(QMouseEvent* ipMouseEvent)
{
	if (++cMouseMoves % scMaxMouseMoves == 0)
	{	long aCharWidth = cParameters.mCharWidth;
        long aLeftOffset = cParameters.mLeftOffset;
		if (cParameters.mLineNumbers)
			aLeftOffset += (cpTextCursor->gutterWidth() + 1) * aCharWidth;
		//long aX = (ipMouseEvent->x() - aLeftOffset) / aCharWidth;
		long aX1 = ipMouseEvent->x() - aLeftOffset;
		long aX = (aX1 / aCharWidth) + (aX1 % aCharWidth) / (aCharWidth * 0.5);
		long aY = ipMouseEvent->y() / cParameters.mLineHeight;
		if ((aX += cpHorizScrollBar->sliderPosition()) < 0) aX = 0;
		if ((aY += cpVertScrollBar->sliderPosition()) < 0) aY = 0;
		updateCursor(cpTextCursor->setCursor(QPoint(aX,aY),true/*Select*/,false/*Newline*/,true/*SaveCur*/),false/*MoveToTop*/);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
mousePressEvent - Set the text cursor to the current mouse position
Args:
	ipMseEv	Ptr to mouse event which contains the current mouse position.
Returns:
	nothing
Notes:
 1.	cursorSet will reset selected, so save Type for mouseMove, mouseDblClick.
	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::mousePressEvent(QMouseEvent* ipMouseEvent)
{
	Qt::KeyboardModifiers aKeyState = ipMouseEvent->modifiers();
	bool aExtendSelect = (aKeyState & Qt::ShiftModifier) != 0;
	cMouseMoves = 0;
	if (ipMouseEvent->button() == Qt::LeftButton)
	{	// Translate. Convert from viewport coordinates to underlying canvas
		long aCharWidth = cParameters.mCharWidth;
		long aLeftOffset = cParameters.mLeftOffset;
		if (cParameters.mLineNumbers)
			aLeftOffset += (cpTextCursor->gutterWidth() + 1) * aCharWidth;
		//long aX = (ipMouseEvent->x() - aLeftOffset) / aCharWidth;
		long aX1 = ipMouseEvent->x() - aLeftOffset;
		long aX = (aX1 / aCharWidth) + (aX1 % aCharWidth) / (aCharWidth * 0.5);
		long aY = ipMouseEvent->y() / cParameters.mLineHeight;
		if ((aX += cpHorizScrollBar->sliderPosition()) < 0) aX = 0;
		if ((aY += cpVertScrollBar->sliderPosition()) < 0) aY = 0;
		updateCursor(cpTextCursor->setCursor(QPoint(aX, aY),aExtendSelect,false/*Newline*/,true/*SaveCur*/),false/*MoveToTop*/);
		ipMouseEvent->accept();
	}
	else
		ipMouseEvent->ignore();
}

/*	---------------------------------------------------------------------------------------------------------------------------
onBlinkCursor - Cause cursor to blink
Args:
	none
Returns:
	nothing
Notes:
 1.	Cursor refers to the text cursor which is a vertical bar (not the mouse cursor).
	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::onBlinkCursor()
{
	// Translate. Translate from canvas coordinates to viewport coordinates.
	// Note that aX is actual position, including line number offset if any.
	const QPoint& arCursor = cpTextCursor->cursor(eCursor);
	long aX = arCursor.x() - cpHorizScrollBar->sliderPosition();
	long aY = arCursor.y() - cpVertScrollBar->sliderPosition();

	// Offsets. Compute offsets
	long aCharWidth = cParameters.mCharWidth;
	long aCurHeight = cParameters.mCursorHeight;
	long aLeftOffset = cParameters.mLeftOffset;
	long aLineHeight = cParameters.mLineHeight;
	if (cParameters.mLineNumbers)
	 	aLeftOffset += (cpTextCursor->gutterWidth() + 1) * aCharWidth;

	// Update. Convert from chars to pixels and update the character at the cursor.
	// Extend box height by one in both directions to avoid leaving trail of dots behind.
	cCursorShow = !cCursorShow;
	cpViewPort->update(aX * aCharWidth + aLeftOffset, aY * aLineHeight-1, aCharWidth, aCurHeight+2);
}

void ATextEdit::onRowCountChanged(long iNewSize)
{
	// Vertical Size. Determine if scroll maximum should be expanded/shrunk. See also resizeEvent.
	long aMargin = cParameters.mBottomMargin * 2;		// Low water mark and bottom margin (~4)
	long aNewSize = iNewSize + aMargin;					// Expand size by the margin at the bottom
	long aPage = cpVertScrollBar->pageStep();			// DocSize = aMax + aPageStep
	long aDocSize = cpVertScrollBar->maximum() + aPage;	// Current Document size [rows]

	// Resize. Expand if NewSize above DocSize or shrink if NewSize below DocSize - LowWaterMark
	if (aNewSize > aDocSize || (aNewSize < aDocSize - aMargin && aNewSize >= aPage))
		cpVertScrollBar->setMaximum(aNewSize - aPage);
}

void ATextEdit::onSliderMoved(int iValue)
{
    Q_UNUSED(iValue);

	cpViewPort->update();
}

/*	---------------------------------------------------------------------------------------------------------------------------
paintEvent - Reimplement inherited version to catch repaint event
	ipEvent	->Area to be repainted
Returns:
	nothing
Note:
	Horizontal scrolling is a multiple of the char width.  Vertical scrolling is a multiple of the row height.
	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::paintEvent(QPaintEvent* ipPaintEvent)
{
	// Debug. Uncomment to debug...
	/*int aH, aW, aX, aY;
	ipPaintEvent->rect().getRect(&aX, &aY, &aW, &aH);
	qDebug("paintEvent, X=%ld, Y=%ld, W=%ld, H=%ld", aX, aY, aW, aH); */

	// Paint.  Note viewport position relative to underlying canvas.
	QPoint aOrigin(cpHorizScrollBar->value(), cpVertScrollBar->value());
	cpTextCursor->paint(ipPaintEvent->rect(), aOrigin, cCursorShow);
}

/*	---------------------------------------------------------------------------------------------------------------------------
resizeEvent - Called when viewport is resized
Args:
	ipRszEv	-> New viewport size information
Returns:
	nothing
Notes:
 1.	If word wrap is enabled, refresh display since the width of the viewport may have changed.
 2.	The scroll bar maximum is set so that DocSize = Max - Min + PageStep
 3. If word wrap is enabled, the horizontal scroll bar is not enabled (see editSetParameter).
 	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::resizeEvent(QResizeEvent* ipResizeEvent)
{
	bool aResized = false;
	const QSize& arSize = ipResizeEvent->size();
	long aCharWidth = cParameters.mCharWidth;
	long aLineHeight = cParameters.mLineHeight;
	long aViewHeight = (arSize.height() + aLineHeight - 1) / aLineHeight;
	long aViewWidth = arSize.width() / aCharWidth;	// Allow for scroll bar on the right.
	if (aViewWidth != cViewPortWidth)
	{	// Wrap. Initialize cViewPortWidth, horizontal scroll bar.
		cViewPortWidth = aViewWidth;
		cpHorizScrollBar->setPageStep(aViewWidth);
		cpHorizScrollBar->setMaximum(cParameters.mMaxRowWidth - aViewWidth);
		aResized = true;
	}
	if (aViewHeight != cViewPortHeight)
	{	// View Height. Set page step and maximum scroll height by calling onRowCountChanged.
		cViewPortHeight = aViewHeight;
		cpVertScrollBar->setPageStep(aViewHeight);		// Set page to number of rows in display
		onRowCountChanged(cpTextCursor->count());
		aResized = true;
	}
	if (aResized)
	{	cpTextCursor->setViewSize(QSize(aViewWidth, aViewHeight));
		if (cParameters.mWrap)
			cpTextCursor->rebalance();
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
tabifyDialog - Launches dialog to modify tab settings
Args:
	none
Returns:
	nothing
Notes:
 	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::tabifyDialog()
{
	if (cReadOnly)
	{	emit statusAlert("Tabify: Text is read-only");
		cStaleAlert = true;
	}
	else
	{	scpTabifyDialog->setEditor(this);
		if (scpTabifyDialog->exec() == QDialog::Accepted)
		{
		}
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
tabWidthDialog - Launches dialog to set the tab width.
Args:
	none
Returns:
	nothing
Notes:
 1.	Modify tabwidth.   X, Dy, Y can all change. So all the reference cursors may have lost sync.
 2.	Reset all the cursors from scratch.
 	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::tabWidthDialog()
{
	long aWidth;
	if (scpTabWidthDialog->exec() == QDialog::Accepted)
	{	if ((aWidth = scpTabWidthDialog->getTabWidth()) > 0 && aWidth != cParameters.mTabWidth)
			editSetParameter("TabWidth", QVariant((int)aWidth));
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
testDialog - Launch the test dialog for testing the editor using the built-in API
Args:
	none
Returns:
	nothing
Notes:
 	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::testDialog()
{
	scpTestDialog->setEditor(this);
	long aRet = scpTestDialog->exec();
	if (aRet == QDialog::Accepted)
	{	// accept() called on dialog.
		aRet = 0;
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
truncateTop - Truncate all but the last iLeft rows from the current text
Args:
	iLeft	 Number of rows remaining in the document
Returns:
	aDeleted Number of rows deleted.
Notes:
 	------------------------------------------------------------------------------------------------------------------------ */
long ATextEdit::truncateTop(long iLeft)
{
	return cpTextCursor->truncateTop(iLeft);
}

/*	---------------------------------------------------------------------------------------------------------------------------
updateCursor - Update visible cursor position
	iRepaint	Iff true, call update to repaint the screen
	iMoveToTop  Iff true, move viewport so that the cursor is at the top of the viewport.
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::updateCursor(bool iRepaint, bool iMoveToTop)
{
	// Viewport. Get cursor position. See also onRowCountChanged
	QPoint aCursor = cpTextCursor->cursor(eCursor);
	long aX = aCursor.x();
	long aY = aCursor.y();
	long aLeft = cpHorizScrollBar->sliderPosition();
	long aTop = cpVertScrollBar->sliderPosition();
	if (aCursor != cCurCursor)
	{	if (!iRepaint)
		{	// Translate. Translate old cursor from canvas coordinates to viewport coordinates.
			// Note that aX is the text cursor position, not including the line number offset, if any.
			long aCurX = cCurCursor.x() - aLeft;
			long aCurY = cCurCursor.y() - aTop;

			// Erase. Convert coordinates to pixels. Erase the old cursor.
			long aCharWidth = cParameters.mCharWidth;
			long aCurHeight = cParameters.mCursorHeight;
			long aCurWidth = cParameters.mCursorWidth;
			long aLeftOffset = cParameters.mLeftOffset;
			long aLineHeight = cParameters.mLineHeight;
			if (cParameters.mLineNumbers)
				aLeftOffset += (cpTextCursor->gutterWidth() + 1) * aCharWidth;
			// Extend box height to avoid leaving trail of dots behind.
			cpViewPort->update(aCurX * aCharWidth + aLeftOffset, aCurY * aLineHeight-1, aCurWidth, aCurHeight+2);
		}
		cCurCursor = aCursor;
		emit cursorPositionChanged(aX, aY);
	}
	// Visible. Make sure that the cursor is within the viewport. Alert others if changed
	long aCharWidth = cParameters.mCharWidth;
	long aLineHeight = cParameters.mLineHeight;
	long aWidth = (cpViewPort->width() + aCharWidth - 1) / aCharWidth;
	long aHeight = (cpVertScrollBar->height() + aLineHeight - 1) / aLineHeight - cParameters.mBottomMargin;
	if (aX < aLeft)
		cpHorizScrollBar->setSliderPosition((aLeft = aX));
	else if (aX > aLeft + aWidth)
		cpHorizScrollBar->setSliderPosition((aLeft = aX - aWidth  + cParameters.mRightMargin));
	if (aY < aTop || iMoveToTop)
		cpVertScrollBar->setSliderPosition(aTop = aY);
	else if (aY  > aTop + aHeight)		// Must be rounded to nearest multiple of line height.
		cpVertScrollBar->setSliderPosition(aTop = aY - aHeight);

	if (iRepaint)
	{	cCursorShow = true;
		cpViewPort->update();
	}
	else
	{	cCursorShow = false;
		onBlinkCursor();
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
wheelEvent - Reimplement to catch wheel events. Move viewport up/down by one screen width
Args:
	ipEv	-> wheelevent containing wheel changes and mouse position
Returns:
	nothing
Notes:
 1.	delta is positive if wheel rotated forward away from user, negative if the wheel was rotated backwards toward the user.
 	------------------------------------------------------------------------------------------------------------------------ */
void ATextEdit::wheelEvent(QWheelEvent* ipWheelEvent)
{
	long aClicks = ipWheelEvent->delta() / 120;	// Delta returns the number of wheel clicks * 120
	long aPage = cpVertScrollBar->pageStep();
	long aPos = cpVertScrollBar->sliderPosition();
	long aNewPos = aPos - aClicks * aPage / 2;
	long aMax = cpVertScrollBar->maximum();
	aNewPos = (aNewPos < 0) ? 0 : (aNewPos <= aMax) ? aNewPos : aMax;
	if (aNewPos != aPos)
	{	cpVertScrollBar->setSliderPosition(aNewPos);
		cpViewPort->update();
	}
}

/*	-------------------------------------- KEY COMMANDS -------------------------------------------

Ordered by key code:
Ctrl+Key_Colon		0x3a	Uncomment selected text
Ctrl+Key_Semicolon	0x3b	Comment selected text
Ctrl+Key_A			0x41	Move beg-of-line
Ctrl+Key_B			0x42	Move left 1 char
Ctrl+Key_C			0x43	Copy selected text to clipboard
Ctrl+Key_D			0x44	Delete right 1 char
Ctrl+Key_E			0x45	Move end-of-line
Ctrl+Key_F			0x46	Find dialog
Ctrl+Key_H			0x48	Delete left 1 char	
Ctrl+Key_I			0x49	Insert tab
Ctrl+Key_J			0x4a	Insert \n
Ctrl+Key_K			0x4b	Delete end-of-line
Ctrl+Key_N			0x4e	Move down 1 line
Ctrl+Key_P			0x50	Move up 1 line
Ctrl+Key_R			0x52	Replace dialog
Ctrl+Key_S			0x53	Select text between parens
Ctrl+Key_T			0x54	Set tabstop width
Ctrl+Key_U			0x55	Delete beg-of-line
Ctrl+Key_V			0x56	Paste clipboard into text
Ctrl+Key_X			0x58	Cut selected text, copy to clipboard
Ctrl+Key_Y			0x59	Redo last undo operation
Ctrl+Key_Z			0x5a	Undo last operation

Key_Tab				0x1001	Indent selected text
Key_Backtab			0x1002	Unindent selected text
Key_Backspace		0x1003	Delete left 1 char.
Ctrl+Key_Backspace	0x1003	Delete left 1 word.
Key_Return			0x1004	Insert \n
Key_Enter			0x1005	Insert \n
Key_Insert			0x1006	N/A
Key_Delete			0x1007	Delete right 1 char
Ctrl+Key_Delete		0x1007	Delete right 1 word
Key_Pause (break)	0x1008	N/A
Key_Print			0x1009	N/A
Key_SysReq			0x100a	N/A
Key_Home			0x1010	Move beg-of-line
Ctrl+Key_Home		0x1010	Move beg-of-text
Key_End				0x1011	Move end-of-line
Ctrl+Key_End		0x1011	Move end-of-text
Key_Left			0x1012	Move left 1 char
Ctrl+Key_Left		0x1012	Move left 1 word
Key_Up				0x1013	Move up one line
Ctrl+Key_Up			0x1013	Move up half page
Key_Right			0x1014	Move right one char
Ctrl+Key_Right		0x1014	Move right one word
Key_Down			0x1015	Move down one line
Ctrl+Key_Down		0x1015	Move down half page
Key_PageUp			0x1016	Move up 1 page
Ctrl+Key_PageUp		0x1016	Move back in list of saved positions
Key_PageDown		0x1017	Move down 1 page
Ctrl+Key_PageDown	0x1017	Move forward in list of saved positions

Shift+ArrowKey				Move + highlight

Key_F1				0x1030	Show help
Key_F2				0x1031	N/A
Key_F3				0x1032	Repeat find downward
Shift+Key_F3		0x1032	Repeat find upward
Ctrl+Key_F3			0x1032	Repeat find in all text
Key_F4				0x1033	N/A
Ctrl+Shift+Key_F5	0x1034	Launch test suite
Key_F6				0x1035	N/A
Key_F7				0x1036	N/A
Key_F8				0x1037	N/A
Key_F9				0x1038	N/A
Key_F10				0x1039	N/A
Key_F11				0x103a	N/A
Key_F12				0x103b	N/A
Key_Menu			0x1055	N/A
*/


/*	---------------------------------------- KEY CODES --------------------------------------------
Key codes:
Some have been edited out that are not likely to be used 
Qt::Key_Escape		0x1000 
Qt::Key_Tab			0x1001
Qt::Key_Backtab		0x1002
Qt::Key_Backspace	0x1003
Qt::Key_Return		0x1004
Qt::Key_Enter		0x1005
Qt::Key_Insert		0x1006
Qt::Key_Delete		0x1007
Qt::Key_Pause		0x1008	// Break
Qt::Key_Print		0x1009
Qt::Key_SysReq		0x100a
Qt::Key_Home		0x1010
Qt::Key_End			0x1011
Qt::Key_Left		0x1012
Qt::Key_Up			0x1013
Qt::Key_Right		0x1014
Qt::Key_Down		0x1015
Qt::Key_PageUp		0x1016
Qt::Key_PageDown	0x1017
Qt::Key_Shift		0x1020
Qt::Key_Control		0x1021
Qt::Key_Meta		0x1022
Qt::Key_Alt			0x1023
Qt::Key_CapsLock	0x1024
Qt::Key_NumLock		0x1025
Qt::Key_ScrollLock	0x1026
Qt::Key_F1			0x1030
Qt::Key_F2			0x1031
Qt::Key_F3			0x1032
Qt::Key_F4			0x1033
Qt::Key_F5			0x1034
Qt::Key_F6			0x1035
Qt::Key_F7			0x1036
Qt::Key_F8			0x1037
Qt::Key_F9			0x1038
Qt::Key_F10			0x1039
Qt::Key_F11			0x103a
Qt::Key_F12			0x103b
Qt::Key_Menu		0x1055


// Just the ascii codes from here on...
Qt::Key_Space		0x20
Qt::Key_Exclam		0x21
Qt::Key_QuoteDbl	0x22
Qt::Key_NumberSign	0x23
Qt::Key_Dollar		0x24
Qt::Key_Percent		0x25
Qt::Key_Ampersand	0x26
Qt::Key_Apostrophe	0x27
Qt::Key_ParenLeft	0x28
Qt::Key_ParenRight	0x29
Qt::Key_Asterisk	0x2a
Qt::Key_Plus		0x2b
Qt::Key_Comma		0x2c
Qt::Key_Minus		0x2d
Qt::Key_Period		0x2e
Qt::Key_Slash		0x2f
Qt::Key_0			0x30
Qt::Key_1			0x31
Qt::Key_2			0x32
Qt::Key_3			0x33
Qt::Key_4			0x34
Qt::Key_5			0x35
Qt::Key_6			0x36
Qt::Key_7			0x37
Qt::Key_8			0x38
Qt::Key_9			0x39
Qt::Key_Colon		0x3a
Qt::Key_Semicolon	0x3b
Qt::Key_Less		0x3c
Qt::Key_Equal		0x3d
Qt::Key_Greater		0x3e
Qt::Key_Question	0x3f
Qt::Key_At			0x40
Qt::Key_A			0x41
Qt::Key_B			0x42
Qt::Key_C			0x43
Qt::Key_D			0x44
Qt::Key_E			0x45
Qt::Key_F			0x46
Qt::Key_G			0x47
Qt::Key_H			0x48
Qt::Key_I			0x49
Qt::Key_J			0x4a
Qt::Key_K			0x4b
Qt::Key_L			0x4c
Qt::Key_M			0x4d
Qt::Key_N			0x4e
Qt::Key_O			0x4f
Qt::Key_P			0x50 
Qt::Key_Q			0x51
Qt::Key_R			0x52
Qt::Key_S			0x53
Qt::Key_T			0x54
Qt::Key_U			0x55
Qt::Key_V			0x56
Qt::Key_W			0x57
Qt::Key_X			0x58
Qt::Key_Y			0x59
Qt::Key_Z			0x5a
Qt::Key_BracketLeft	0x5b
Qt::Key_Backslash	0x5c
Qt::Key_BracketRight0x5d
Qt::Key_AsciiCircum	0x5e
Qt::Key_Underscore	0x5f
Qt::Key_QuoteLeft	0x60
// Skip lower case letter codes...
Qt::Key_BraceLeft	0x7b
Qt::Key_Bar			0x7c		// Vertical bar
Qt::Key_BraceRight 	0x7d
Qt::Key_AsciiTilde	0x7e
 */

