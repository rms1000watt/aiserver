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
aisdev/atextedit/atestdialog.h
														Self Test Dialog
The Test dialog is a modal pop-up dialog used to allow the user to run a sequence of self tests

CHANGE HISTORY
Version	Date		Who		Change
1.0118	12/12/2006	tlw		replace. Remove iSelect argument from this test.
1.0110	10/12/2006	tlw		Add clearUndo function.
1.0108	 9/29/2006	tlw		Add output pane, revamp runCmd, readTestSuite. Add getOption, getText.
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QtDebug>
#include <QtCore/QTimer>
#include <QtGui/QCloseEvent>

#include "atextedit.h"
#include "atestdialog.h"
#include "autilities.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief ATestDialog - Constructor initializes the Qt-Designer-generated GUI and initializes the class variables

\param irFileName - the name of the file holding the API tests.
\param ipParent -> the parent that created this instance of the class.
\param ipName -> the name assigned to this instance of the class
\param iFlgs - determines the behavior and layout of the dialog.
\return void
*/
ATestDialog::ATestDialog(const QString& irFileName, QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs)
  : QDialog(ipParent, iFlgs), cStepping(true), cTestFileName(irFileName)
{
	// Widgets. Initialize GUI.
	setObjectName(QString(ipName));
	setSizeGripEnabled(false);
	setWindowTitle(tr("Edit Test Suite"));
	cUi.setupUi(this);
	cUi.upCmdComboBox->setInsertPolicy(QComboBox::InsertAtTop);

	// Class Variables
	cCurTest = -1;
	cUi.upStatusLabel->setText("At beginning of tests. Press Next or Run to run tests");
	cStaleMsg = true;
	cpOutput = cUi.upOutputTextEdit;

	// Initialize maps.
	long aItm, aSize;
	QStringList aList;

	// Command names
	aList = QString(AEDTEST_CMDS).split(QChar(','), QString::KeepEmptyParts, Qt::CaseSensitive);
	for (aItm = 0, aSize = aList.size(); aItm < aSize; ++aItm)
		cCommands[aList.at(aItm)] = aItm;

	// MoveTo. Move-to names must be in same order as in AMoveTo enum.
	aList = QString(AEDTEST_MOVETO).split(QChar(','), QString::KeepEmptyParts, Qt::CaseSensitive);
	for (aItm = 0, aSize = aList.size(); aItm < aSize; ++aItm)
		cMoves[aList.at(aItm)] = (ATextEdit::AMoveTo)aItm;

	// Options.
	aList = QString(AEDTEST_TRUEARGS).split(QChar(','), QString::KeepEmptyParts, Qt::CaseSensitive);
	
	for (aItm = 0, aSize = aList.size(); aItm < aSize; ++aItm)
		cTrueOptions[aList.at(aItm)] = 1;

	aList = QString(AEDTEST_FALSEARGS).split(QChar(','), QString::KeepEmptyParts, Qt::CaseSensitive);
	for (aItm = 0, aSize = aList.size(); aItm < aSize; ++aItm)
		cFalseOptions[aList.at(aItm)] = 0;

	// Parameters. Names of configurable parameters
	aList = QString(AEDTEST_PARAMNAMES).split(QChar(','), QString::KeepEmptyParts, Qt::CaseSensitive);
	for (aItm = 0, aSize = aList.size(); aItm < aSize; ++aItm)
		cParamNames[aList.at(aItm)] = aItm;

	// Top section - note label
	cUi.upNoteLabel->setText(
		"Note: The last executed test is displayed in Last.\n"
		"The next test to be executed is displayed in Next.\n"
		"To modify and rerun a test, edit text in Last, select Go.\n"
		"To modify next test, edit text in Next, select Next.\n"
		"To skip a test, delete the text in Next, select Next or Run.\n\n"
		"Clear - Clear the output pane, text boxes and the command list.\n"
		"Run - Resume tests in test suite starting from operation shown in Next.\n"
		"Go - Run current test shown in Last.\n"
		"Next - Run next test shown in Next.\n"
		"Prev - Run previous test (not shown). \n"
		"Restart-Restart tests from beginning");

	// Connect up the buttons to slots.
	connect(cUi.upClearButton, SIGNAL(clicked()), this, SLOT(onClear()));
	connect(cUi.upCloseButton, SIGNAL(clicked()), this, SLOT(onClose()));
	connect(cUi.upGoButton, SIGNAL(clicked()), this, SLOT(onGo()));
	connect(cUi.upNextButton, SIGNAL(clicked()), this, SLOT(onNext()));
	connect(cUi.upPrevButton, SIGNAL(clicked()), this, SLOT(onPrev()));
	connect(cUi.upRestartButton, SIGNAL(clicked()), this, SLOT(onRestart()));
	connect(cUi.upRunButton, SIGNAL(clicked()), this, SLOT(onRun()));
}

//  Destroys the object and frees any allocated resources
ATestDialog::~ATestDialog()
{
#ifdef AIS_DEBUG
	qDebug("%s", "~~~~ATestDialog()");
#endif
	cCommands.clear();
	cMoves.clear();
	cTests.clear();
}

// Note currently used
void ATestDialog::closeEvent(QCloseEvent* cpEvent)
{
	cTests.clear();
	cpEvent->accept();
}

/*	---------------------------------------------------------------------------------------------------------------------------
getMoveTo - Extract the next move operation from the comma-delimited list of args.
Args:
	ipArg		-> next argument in a list comma-separated arguments
	orMoveTo	Place to put the next move operation.
Returns:
	aArgLgth	Length of extracted arg in chars including separator unless at terminator.
Note:
1. Scan terminates on a comma, closing paren, newline, null or ^A.
	------------------------------------------------------------------------------------------------------------------------ */
long ATestDialog::getMoveTo(const char* ipArg, ATextEdit::AMoveTo& orMoveTo)
{
	long aArgLgth;			// Returned length of next argument
	QString aArg;			// Place to put next argument

	// Transfer token to aArg
	if ((aArgLgth = AUtil::getArg(ipArg, ',', aArg, true/*Trim*/, true/*ToLower*/)) > 0 && cMoves.contains(aArg))
		orMoveTo = cMoves.value(aArg);
	else
	{	orMoveTo = ATextEdit::eNoMove;
		qDebug("ATestDialog::getMove, Unknown move command: %s", ipArg);
		cpTextEdit->beep();
	}
	return aArgLgth;
}

/*	---------------------------------------------------------------------------------------------------------------------------
getOption - Extract the next boolean option from the comma-delimited list of args.
Args:
	ipArg		-> next argument in a list comma-separated arguments
	orOption	Place to put the value of the next option iff an option is found
Returns:
	aArgLgth	Length of extracted arg in chars including separator unless at terminator.
Note:
 1. Does not modify orOption if no option found or if option is not recognized.
 1. Scan terminates on first comma, closing paren, newline, null or ^A.
 2. Leading and trailing whitespace is not included in the option name.
 3. The option name is converted to lower case before lookup.
	------------------------------------------------------------------------------------------------------------------------ */
long ATestDialog::getOption(const char* ipArg, bool& orOption)
{
	const char *apSrc = ipArg;	// -> into argument
	long aArgLgth;				// Returned length of next argument
	QString aArg;				// Place to put next argument

	// Transfer token to aArg
	if ((aArgLgth = AUtil::getArg(apSrc, ',', aArg, true/*Trim*/, true/*ToLower*/)) > 0)
	{	if (cFalseOptions.contains(aArg))
			orOption = false;
		else if (cTrueOptions.contains(aArg))
			orOption = true;
		else
		{	qDebug() << "ATestDialog::getOption, Unknown option: " << aArg;
			cpTextEdit->beep();
		}
	}
	return aArgLgth;
}

/*	---------------------------------------------------------------------------------------------------------------------------
getText - Extract the next quote-delimited string from a comma-delimited list of args.
Args:
	ipArg	-> next quoted string in the command
	orText	Place to put the extracted string
Returns:
	aArgLgth Length of the extracted string
Notes:
 1.	If no closing quote, scans to eol, comma, or closing paren.
 2.	orText is set to an empty string if no quoted string can be found.
	------------------------------------------------------------------------------------------------------------------------ */
long ATestDialog::getText(const char* ipArg, QString& orText)
{
	bool aGotQuote = false;
	const char *apSrc = ipArg;
	char aCh, aEndCh1 = ',', aEndCh2 = ')', aEndCh3 = '\n';
	QString aText;

	// Eat initial spaces, opening quote
	while (*apSrc == ' ' || *apSrc == '\t')
		++apSrc;
	if (*apSrc == '"')
	{	aGotQuote = true;
		++apSrc;
	}
	// EndCh. If opening quote, scan to closing quote; else scan to comma, closing paren, or newline 
	if (aGotQuote)
		aEndCh1 = aEndCh2 = aEndCh3 = '"';
	// Translate escaped characters, \", \r, \t, \\, \n. Transfer quoted-string to destination.
	for (;(aCh = *apSrc) > '\01' && aCh != aEndCh1 && aCh != aEndCh2 && aCh != aEndCh3; ++apSrc)
	{	if (aCh == '\\')
		{	if ((aCh = *++apSrc) > '\01')
			{	switch (aCh)
				{
				case '"':
					aText += '"';
					break;
				case '\\':
					aText += '\\';
					break;
				case 'n':
					aText += '\n';
					break;
				case 'r':
					aText += '\r';
					break;
				case 't':
					aText += '\t';
					break;
				default:
					aText += aCh;
				}
			}
			else
				break;
		}
		else
			aText += aCh;
	}
	// Eat terminating quote, spaces, next separator
	if (aGotQuote && *apSrc == '"')
		++apSrc;
	while (*apSrc == ' ' || *apSrc == '\t')
		++apSrc;
	if (*apSrc == ',' || *apSrc == ')' || *apSrc == '\n')
		++apSrc;
	orText = aText;
	return apSrc - ipArg;
}

/*	---------------------------------------------------------------------------------------------------------------------------
init - Initialize list of tests
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void ATestDialog::init()
{
	// Clear. Get a fresh start.
	cTests.clear();				// Moves current to beginning. Deletes lines from heap.
	cpOutput->clear();
	cUi.upCmdComboBox->clear();
	cUi.upNextLineEdit->clear();
	// Put lines from test file into list of tests
	if (readTestSuite(cTestFileName) <= 0)
	{	QString aMsg;
		aMsg.sprintf("Unable to read tests in %s", cTestFileName.toAscii().data());
		cUi.upStatusLabel->setText(aMsg);
	}
	// End. Tack one empty item on the end
	cTests.append("");
	cCurTest = -1;
	if (cTests.count() > 0)
	{	QLineEdit* apLine = cUi.upCmdComboBox->lineEdit();
		apLine->setText("<last completed test goes here>");
		apLine->selectAll();
		apLine->setFocus();
		cUi.upNextLineEdit->setText(cTests.at(0));
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
onClear - Clear output pane and drop-down list
Args: none
Returns: void
	------------------------------------------------------------------------------------------------------------------------ */
void ATestDialog::onClear()
{
	cpOutput->clear();
	cUi.upCmdComboBox->clear();
	cUi.upNextLineEdit->clear();
}

/*	---------------------------------------------------------------------------------------------------------------------------
onClose - Hide this dialog from view
Args:
	none
Returns:
	nothing
Notes:
 1.	Does not really close the dialog, just hides it.
	------------------------------------------------------------------------------------------------------------------------ */
void ATestDialog::onClose()
{
	cTests.clear();
	hide();
}

/*	---------------------------------------------------------------------------------------------------------------------------
onGo - Run the selected test
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void ATestDialog::onGo()
{
	QByteArray aCmd = cUi.upCmdComboBox->currentText().toAscii();
	if (!aCmd.isEmpty())
		runCmd(aCmd);
}

/*	---------------------------------------------------------------------------------------------------------------------------
onNext - Run the next test
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void ATestDialog::onNext()
{
	step(true/*Forward*/);
}

/*	---------------------------------------------------------------------------------------------------------------------------
onPrev - Run the previous test in the test sequence
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void ATestDialog::onPrev()
{
	step(false/*Forward*/);
}

/*	---------------------------------------------------------------------------------------------------------------------------
onRestart - Reload tests and move back to beginning of tests.
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void ATestDialog::onRestart()
{
	init();
	if (cStaleMsg)
		cUi.upStatusLabel->setText("");
	cUi.upStatusLabel->setText("At beginning of tests. Press Run or Next to run tests");
	cStaleMsg = true;
}

/*	---------------------------------------------------------------------------------------------------------------------------
onRun - Execute remaining tests
Args:
	none
Returns:
	nothing
Notes:
 1. cCurTest is index to last completed test or -1 if none.
 2. If at last non-empty test, the next test is empty.
	------------------------------------------------------------------------------------------------------------------------ */
void ATestDialog::onRun()
{
	cStepping = false;
	while (step())
		;
	cStepping = true;

	// Show last, next command.
	QString aTest;
	if (cCurTest >= 0 && !(aTest = cTests.at(cCurTest)).isEmpty())
	{	QComboBox* apCmd = cUi.upCmdComboBox;
		apCmd->insertItem(0/*Index*/, aTest);
		apCmd->lineEdit()->setText(aTest);
		if ((aTest = cTests.at(cCurTest + 1)).isEmpty())
			aTest = "<At End>";
		cUi.upNextLineEdit->setText(aTest);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
readTestSuite - Initialize list of tests from test suite
Args:
	irFileName	Name of file holding tests
Returns:
	aCount		Number of tests found in the test file
	------------------------------------------------------------------------------------------------------------------------ */
long ATestDialog::readTestSuite(const QString& irFileName)
{
	long aCount = 0;
	QFile aF(irFileName);
	if (!aF.open(QIODevice::Text | QIODevice::ReadOnly))	// 16|1
	{	QFileInfo aInfo(irFileName);
		QString aPath = aInfo.absoluteFilePath();
		qDebug() << "ATestDialog::readTestSuite(): Open fails. " << aF.errorString() << ". File: " << aPath;
		cpTextEdit->beep();
	}
	else
	{	QByteArray aLine;
		long aCh = 0;
        long aLgth = 0;
		char *apBfr, *apEnd, *apSrc;
		QString aFileName;
		while (!(aLine = aF.readLine()).isEmpty())	// Includes newlines
		{	// Eat leading white space
			aLgth = aLine.length();
			apBfr = apSrc = aLine.data();
			for (apEnd = apBfr + aLgth; apSrc < apEnd && ((aCh = *apSrc) == ' ' || aCh == '\t'); ++apSrc)
				;
			// Skip blank lines
			if (apSrc == apEnd || aCh == '\n')
				continue;
			// Process comments
			if (aCh == '#')
			{	if (aLgth > 5 && aLine.left(5) == "#stop")
					break;
				if (aLgth > 8 && aLine.left(8) == "#include")
				{	// Extract fileName
					apSrc = apBfr + 9;		// Just past #include
					if (AUtil::getArg(apSrc, '\n', aFileName, true/*Trim*/) > 0)
						readTestSuite(QString(aFileName));		// Recurse
					else
					{	qDebug("ATestDialog::readTestSuite(), Can't include: %s\n", apSrc);
						cpTextEdit->beep();
					}
				}
			}
			else	// Add command to list of tests.
			{	// Strip newlines
				apEnd = apBfr + aLgth - 1;
				if (*apEnd == '\n')
					*apEnd = '\0';
				cTests.append(apBfr);
			}
		}
		aF.close();
		aCount = cTests.count();
	}
	return aCount;
}


/*	---------------------------------------------------------------------------------------------------------------------------
runCmd - Parse the command line and submit it to the editor
Args:
	irTest	Command to be submitted
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
bool ATestDialog::runCmd(const QByteArray& irTest)
{
	// Parse the command line and submit it to editor
	bool aOk = true;					// OK to run next test
	long aArgLgth;
	QString aArg, aPattern, aText;		// Next argument in command.
	const char *apBfr = irTest.data();
	const char *apBeg = apBfr;

	// Put the command name in aArg
	if ((aArgLgth = AUtil::getArg(apBeg, '(', aArg, true/*Trim*/, true/*Lower*/)) <= 0)
	{	qDebug("ATestDialog::runCmd(), Empty test string, cmd=%s", apBfr);
		cpTextEdit->beep();
		aOk = false;
	}
	else if (!cCommands.contains(aArg))
	{	qDebug("ATestDialog::runCmd(), Unknown command, cmd=%s", apBfr);
		cpTextEdit->beep();
		aOk = false;
	}
	else		// Parse the command line. Invoke text edit command.
	{	bool aAll = false, aAppend = false, aDown = true, aEnable = false, aForward = true, aInsert = true, aMatchCase = false;
		bool aMatchWord = false, aRegExp = false, aSelect = false, aToMark = true, aToTabs = true, aRet;
		long aRow;			// Just for show and tabify.
		long aLgth;			// Hold text lengths
		ATextEdit::AMoveTo aMoveTo = ATextEdit::eSame;
		apBeg += aArgLgth;	// Move by command.
		switch(cCommands[aArg])
		{
		case 0:			// append("Text", MoveToEnd)
			if ((aArgLgth = getText(apBeg, aText)) > 0)
			{	getOption(apBeg += aArgLgth, aForward);
				cpTextEdit->editAppend(aText, aForward/*MoveToEnd*/);
			}
			break;
		case 1:			// appendfile(FileName)
			if (AUtil::getArg(apBeg, ')', aText, true/*Trim*/) > 0)
			{	cpTextEdit->editAppendFile(aText);
			}
			else
				cpOutput->append(QString("appendFile, No file name, Cmd: %1").arg(apBfr));
			break;
		case 2:			// clear()
			cpTextEdit->editClear();
			break;
		case 3:			// clearUndo()
			cpTextEdit->editClearUndo();
			break;
		case 4:			// comment(Insert)
			getOption(apBeg, aInsert);
			cpTextEdit->editComment(aInsert);
			break;
		case 5:			// copy(Append)
			getOption(apBeg, aAppend);
			cpTextEdit->editCopy(aAppend);
			break;
		case 6:			// cut(Append)
			getOption(apBeg, aAppend);
			cpTextEdit->editCut(aAppend);
			break;
		case 7:			// delete(MoveTo)
			getMoveTo(apBeg, aMoveTo);
			aLgth = cpTextEdit->editDelete(aMoveTo);
			cpOutput->append(QString("Delete: %1").arg(aLgth));
			break;
		case 8:			// find("Pattern",All,MatchCase,Down,RegExp,Select,MatchWord);
			if ((aArgLgth = getText(apBeg, aPattern)) > 0)
			{	if ((aArgLgth = getOption(apBeg += aArgLgth, aAll)) && (aArgLgth = getOption(apBeg += aArgLgth, aMatchCase)) &&
				(aArgLgth = getOption(apBeg += aArgLgth, aDown)) && (aArgLgth = getOption(apBeg += aArgLgth, aRegExp)) &&
				(aArgLgth = getOption(apBeg += aArgLgth, aSelect)))
					getOption(apBeg += aArgLgth, aMatchWord);
				aRet = cpTextEdit->editFind(aPattern, aAll, aMatchCase, aDown, aRegExp, aSelect, aMatchWord);
				aText = QString("Find: %1").arg(aRet ? "true" : "false");
				cpOutput->append(aText);
			}
			else
				cpOutput->append(QString("appendFile, No search pattern, Cmd: %1").arg(apBfr));
			break;
		case 9:			// functionList()
		{	QStringList aList = cpTextEdit->editFunctionList();
			cpOutput->append("FunctionList:");
			aArgLgth = aList.size();
			for (long aIx = 0; aIx < aArgLgth; aIx += 2)
				cpOutput->append(QString("%1, %2").arg(aList.at(aIx),aList.at(aIx+1)));
			break;
		}
		case 10:			// indent(Insert)
			getOption(apBeg, aInsert);
			cpTextEdit->editIndent(aInsert);
			break;
		case 11:			// insert("Text", Forward)
			if ((aArgLgth = getText(apBeg, aText)) > 0)
			{	getOption(apBeg += aArgLgth, aForward);
				cpTextEdit->editInsert(aText, aForward);
			}
			break;
		case 12:			// isModified()
			aRet = cpTextEdit->editIsModified();
			cpOutput->append(QString("IsModified: %1").arg(aRet ? "true" : "false"));
			break;
		case 13:			// isSelected()
			aRet = cpTextEdit->editIsSelected();
			cpOutput->append(QString("IsSelected: %1").arg(aRet ? "true" : "false"));
			break;
		case 14:		// move(MoveTo, Select)
			if ((aArgLgth = getMoveTo(apBeg, aMoveTo)) > 0)
			{	getOption(apBeg += aArgLgth, aSelect);
				aRet = cpTextEdit->editMove(aMoveTo, aSelect);
				cpOutput->append(QString("Move: %1").arg(aRet ? "true" : "false"));
			}
			else
				cpOutput->append(QString("move, Missing MoveTo argument, Cmd: %1").arg(apBfr));
			break;
		case 15:		// paste()
			cpTextEdit->editPaste();
			break;
		case 16:		// pause - a local command to stop continuous testing
			aOk = false;
			break;
		case 17:		// print()
			cpTextEdit->editPrint();
			break;
		case 18:		// read(FileName)
			if (AUtil::getArg(apBeg, ')', aText, true/*Trim*/) > 0)
			{	aRet = cpTextEdit->editRead(aText);
				cpOutput->append(QString("Read: %1").arg(aRet ? "true" : "false"));
			}
			break;
		case 19:		// redo(ToMark)
			getOption(apBeg, aToMark);		// Does not modify aToMark if no option found.
			cpTextEdit->editRedo(aToMark);
			break;
		case 20:		// 	replace("Pattern", "Text", AllText, MatchCase, RegExp, MatchWord)
			if ((aArgLgth = getText(apBeg, aPattern)) > 0)
			{	if ((aArgLgth = getText(apBeg += aArgLgth, aText)) && (aArgLgth = getOption(apBeg += aArgLgth, aAll)) &&
				(aArgLgth = getOption(apBeg += aArgLgth, aMatchCase)) && (aArgLgth = getOption(apBeg += aArgLgth, aRegExp)))
					getOption(apBeg += aArgLgth, aMatchWord);
				aRet = cpTextEdit->editReplace(aPattern, aText, aAll, aMatchCase, aRegExp, aMatchWord);
				cpOutput->append(QString("Replace: %1").arg(aRet ? "true" : "false"));
			}
			else
				cpOutput->append(QString("replace, Missing Pattern argument, Cmd: %1").arg(apBfr));
			break;
		case 21:		// replaceAll("Pattern", "Text", AllText, MatchCase, RegExp, Select, MatchWord)
			if ((aArgLgth = getText(apBeg, aPattern)) > 0)
			{	if ((aArgLgth = getText(apBeg += aArgLgth, aText)) && (aArgLgth = getOption(apBeg += aArgLgth, aAll)) &&
				(aArgLgth = getOption(apBeg += aArgLgth, aMatchCase)) && (aArgLgth = getOption(apBeg += aArgLgth, aRegExp)) &&
				(aArgLgth = getOption(apBeg += aArgLgth, aSelect)))
					getOption(apBeg += aArgLgth, aMatchWord);
				aRet = cpTextEdit->editReplaceAll(aPattern, aText, aAll, aMatchCase, aRegExp, aSelect, aMatchWord);
				cpOutput->append(QString("ReplaceAll: %1").arg(aRet ? "true" : "false"));
			}
			else
				cpOutput->append(QString("ReplaceAll, Missing Pattern argument, Cmd: %1").arg(apBfr));
			break;
		case 22:		// selectedText()
			aText = cpTextEdit->editSelectedText();
			cpOutput->append("Selected Text: " + aText);
			break;
		case 23:		// selectMatchedText()
			aRet = cpTextEdit->editSelectMatchedText();
			cpOutput->append(QString("SelectMatchedText: %1").arg(aRet ? "true" : "false"));
			break;
		case 24:		// setModified(Enable)
			getOption(apBeg, aEnable);
			cpTextEdit->editSetModified(aEnable);
			break;
		case 25:		// setParameter(ParamName, Value)
			if ((aArgLgth = AUtil::getArg(apBeg, ',', aArg, true/*Trim*/, true/*ToLower*/)) > 0)
			{	if (cParamNames.contains(aArg) && AUtil::getArg(apBeg += aArgLgth, ')', aText, true/*Trim*/) > 0)
					cpTextEdit->editSetParameter(aArg, QVariant(aText));
			}
			else
				cpOutput->append(QString("SetParameter, Unknown parameter, Cmd: %1").arg(apBfr));
			break;
		case 26:		// setReadOnly(Enable)
			getOption(apBeg, aEnable);			// Defaults to false.
			cpTextEdit->editSetReadOnly(aEnable);
			break;
		case 27:		// show(Item, Row)
			if ((aArgLgth = AUtil::getArg(apBeg, ',', aArg, true/*Trim*/, true/*ToLower*/)) > 0)
			{	if (AUtil::getArgValue(apBeg += aArgLgth, &aRow) <= 0)
					aRow = -LONG_MAX;		// Default to large negative row.
				if (aRow > 0) --aRow;		// Convert from user row numbers to internal row numbers
				if (aArg == "cursors" || aArg == "row" || aArg == "text" || aArg == "undo")
				{	aText = cpTextEdit->editShow(aArg, aRow);
					cpOutput->append(irTest + ": " + aText);
				}
				else
					cpOutput->append(QString("Show, Unknown item. Choose cursors,row,text,or undo, Cmd: %1").arg(apBfr));
			}
			else
				cpOutput->append(QString("Show, Missing Item argument, Cmd: %1").arg(apBfr));
			break;
		case 28:		// switch()
			cpTextEdit->editSwitch();
			break;
		case 29:		// tabify(ExtraSpaces, ToTabs)
			if ((aArgLgth = AUtil::getArgValue(apBeg, &aRow)) > 0)
			{	getOption(apBeg += aArgLgth, aToTabs);
				aRet = cpTextEdit->editTabify(aRow, aToTabs);
				cpOutput->append(QString("Tabify: %1").arg(aRet ? "true" : "false"));
			}
			else
				cpOutput->append(QString("Tabify, Missing ExtraSpaces argument, Cmd: %1").arg(apBfr));
			break;
		case 30:		// text()
			aText = cpTextEdit->editText();
			cpOutput->append("Text: " + aText);
			break;
		case 31:		// textLength()
			aLgth = cpTextEdit->editTextLength();
			cpOutput->append(QString("TextLength: %1").arg(QString::number(aLgth)));
			break;
		case 32:		// textRows()
			aLgth = cpTextEdit->editTextRows();
			cpOutput->append(QString("TextRows: %1").arg(QString::number(aLgth)));
			break;
		case 33:		// undo(ToMark)
			getOption(apBeg, aToMark);		// Defaults to true if no option specified.
			cpTextEdit->editUndo(aToMark);
			break;
		case 34:		// word(Select)
			getOption(apBeg, aSelect);
			aText = cpTextEdit->editWord(aSelect);
			cpOutput->append(QString("Word: %1").arg(aText));
			break;
		case 35:		// write(fileSpec)
			if (AUtil::getArg(apBeg, ')', aArg, true/*Trim*/) > 0)
			{	aRet = cpTextEdit->editWrite(aArg);
				cpOutput->append(QString("Write: %1").arg(aRet ? "true" : "false"));
			}
			else
				cpOutput->append(QString("Write, No file name, Cmd: %1").arg(apBfr));
			break;
		default:
			cpOutput->append(QString("RunCmd, Unknown edit operation, Cmd: %1").arg(apBfr));
			aOk = false;
			break;
		}
	}
	return aOk;
}

/*!
\brief setEditor - Initialize all the dialog

\param ipTextEdit - Save ptr to text for use later by editFind to search the text
\return void
\par Notes:
-# Called by text editor before showing this dialog.
-# setEditor also calls init to initialize the tests.
 */
void ATestDialog::setEditor(ATextEdit* ipTextEdit)
{
	cpTextEdit = ipTextEdit;
	init();
}

/*	---------------------------------------------------------------------------------------------------------------------------
step - Run the next/previous test in the test suite
Args:
	iForward	If true, run next test; else, run previous test
Returns:
	aOk			True iff test was found.
	------------------------------------------------------------------------------------------------------------------------ */
bool ATestDialog::step(bool iForward)
{
	// Clean. Clear status label.
	bool aOk = false;
	if (cStaleMsg)
	{	cUi.upStatusLabel->setText("");
		cStaleMsg = false;
	}
	// Forward. If moving forward and not at end, run next test.
	if (iForward)
	{	// Test. If stepping, use NextLineEdit; else, use next test in cTests.
		QByteArray aTest = cTests.at(cCurTest + 1);
		bool aAtEnd = aTest.isEmpty();
		if (cStepping)
		{	aTest = cUi.upNextLineEdit->text().toAscii();
			if (aTest.startsWith('<'))
				aTest.clear();
		}
		// Run test, if any.
		if (!aTest.isEmpty())
			aOk = runCmd(aTest);

		// Last. Display executed test in Cmd text box if stepping
		if (cStepping)
		{	QComboBox* apCmd = cUi.upCmdComboBox;
			apCmd->insertItem(0, aTest);
			apCmd->lineEdit()->setText(aTest);
		}
		// Next. Move on to the next test
		if (!aAtEnd)
		{	++cCurTest;
			// Display next test in Next text box.
			if (cStepping)
			{	if ((aTest = cTests.at(cCurTest+1)).isEmpty())
					aTest = "<At End>";
				cUi.upNextLineEdit->setText(aTest);
			}
		}
		else	// At end. Issue message.
		{	cStaleMsg = true;
			cUi.upStatusLabel->setText("At end of tests. Press Restart to repeat tests");
		}
	}
	else	// Backward. If moving back and not at beginning, run prev test.
	{	if (cCurTest > 0)
		{	QByteArray aTest(cTests.at(--cCurTest));
			aOk = runCmd(aTest);

			// Display executed test in Cmd text box and last test in Next text box.
			QComboBox* apCmd = cUi.upCmdComboBox;
			apCmd->removeItem(0);
			apCmd->lineEdit()->setText(aTest);
			apCmd->setCurrentIndex(0);
			cUi.upNextLineEdit->setText(cTests.at(cCurTest +1));
		}
		else	// At beginning. Issue message.
		{	cStaleMsg = true;
			cUi.upStatusLabel->setText("At beginning of tests. Press Next or Run to run tests");
		}
	}
	return aOk;
}

// end

