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

#ifndef ATESTDIALOG_H
#define ATESTDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/atestdialog.h
														Test Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0110	10/12/2006	tlw		Add clearUndo function
1.0108	 9/29/2006	tlw		Add output pane for messages. onClear to clear messages.
1.0108	 9/27/2006	tlw		MOVETO. Add nextsow, prevsow. sow same as bow except don't move if already at bow.
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include "ui_atestdialog.h"
#include "aglobals.h"			// AStringIntMap, AStringMap
#include "atextedit.h"			// ATextEdit, AMoveTo

//	----------------------------------------------- CONFIGURABLE PARAMETERS ---------------------------------------------------
// Command names - 12 + 12 + 12 = 36 commands
#define AEDTEST_CMDS "append,appendfile,clear,clearundo,comment,copy,cut,delete,find,functionlist,indent,insert,\
ismodified,isselected,move,paste,pause,print,read,redo,replace,replaceall,selectedtext,selectmatchedtext,\
setmodified,setparameter,setreadonly,show,switch,tabify,text,textlength,textrows,undo,word,write"

// MoveTo names
// These names must be in the same order as in the AMoveTo enum - 12 + 10
#define AEDTEST_MOVETO "nomove,bol,bot,eol,eot,nextbow,nextchar,nextcur,nexteow,nexthalf,nextline,nextpage,\
nextsow,prevbow,prevchar,prevcur,prevhalf,prevline,prevpage,prevsow,prevtab,same"

// Option Names - 14
#define AEDTEST_TRUEARGS "1,true,all,append,down,enable,forward,insert,matchcase,matchword,regexp,select,tomark,totabs"
#define AEDTEST_FALSEARGS "0,false,notall,noappend,up,disable,backward,remove,nomatchcase,nomatchword,noregexp,noselect,one,\
tospaces"

// Settable Parameter Names - 8 + 3
#define AEDTEST_PARAMNAMES "font,fontsize,language,matchparens,maximumrows,maximumrowwidth,maximumundodepth,showwhitespace,\
tabwidth,wordwrap,default"

//	------------------------------------------------- TYPE DECLARATIONS -------------------------------------------------------
typedef QHash<QString, ATextEdit::AMoveTo> AMoveMap;
typedef QList<QByteArray> ATests;
typedef QHash<QString, long> AStringIntHash;

//	------------------------------------------------ CLASS DECLARATIONS	-------------------------------------------------------
/*!
\brief ATestDialog - Modal dialog to allow the user to test the editor.

 */
class ATestDialog : public QDialog
{
	Q_OBJECT

public:
    ATestDialog(const QString& irFileName, QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs = 0);
    ~ATestDialog();
	void			setEditor(ATextEdit* ipTextEdit);

protected:
	void			closeEvent(QCloseEvent* cpEvent);

private slots:
	void			onClear();
	void			onClose();
	void			onGo();
	void			onNext();
	void			onPrev();
	void			onRestart();
	void			onRun();

private:
	void			init();
	long			getMoveTo(const char* ipArg, ATextEdit::AMoveTo& orMoveTo);
	long			getOption(const char* ipArg, bool& orOption);
	long			getText(const char* ipArg, QString& orText);
	long			readTestSuite(const QString& irFileName);
	bool			runCmd(const QByteArray& irTest);
	bool			step(bool iForward = true);

	AStringIntHash	cCommands;		// List of edit commands implemented in ATextEdit
	long			cCurTest;		// cTests index to last executed test or -1 if no tests executed.
	AStringIntHash	cFalseOptions;	// List of options that evaluate to false
	AMoveMap		cMoves;			// Key: MoveTo name, Value: MoveTo enum
	QTextEdit*		cpOutput;		// Output pane
	AStringIntHash	cParamNames;	// Map of configurable parameters
	bool			cStaleMsg;		// Stale status message
	bool			cStepping;		// Not running a sequence of tests
	QString			cTestFileName;	// Name of test suite containing the commands.
	ATests			cTests;			// List of byte array commands in test suite
	ATextEdit*		cpTextEdit;		// -> text edit used for this test
	AStringIntHash	cTrueOptions;	// List of options that evaluate to true

	Ui::ATestDialogClass	cUi;
};

#endif // ATESTDIALOG_H
