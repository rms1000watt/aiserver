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
aisdev/atextedit/ahelpapidialog.h
														Help API Dialog

CHANGE HISTORY
Version	Date		Who		Change
2.0003	1/8/2007	tlw		Parameters. Add enable line numbers to parameters.
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0110	10/12/2006	tlw		Add clearUndo function
1.0108	 9/27/2006	tlw		Add instructions on running API
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "ahelpapidialog.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief AHelpApiDialog - Constructor initializes the Qt-Designer-generated GUI and initializes the help text

\param ipParent -> the parent that created this instance of the class.
\param ipName -> the name assigned to this instance of the class
\param iFlags - determines the behavior and layout of the dialog.
\return void
*/
AHelpApiDialog::AHelpApiDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlags)
  :
	QDialog(ipParent, iFlags)
{
	// Widgets. Initialize GUI.
	if (ipName == NULL) ipName = "HelpApiDialog";
	setObjectName(QString(ipName));
	setSizeGripEnabled(true);
	cUi.setupUi(this);

	// Help text
	const char *apHelp = "AIS EDITOR API HELP:\n"
		"Open a file in the folder containing testsuite.txt and press F5 to invoke the test dialog.\n"
		"Edit testsuite.txt to modify commands submitted to the test dialog and press restart.\n"
		"\n"
		"FUNCTIONS:\n"
		"append(\"Text\", Forward)\n"
		"appendFile(FileName)\n"
		"clear\n"
		"clearUndo\n"
		"comment(Insert)\n"
		"copy(Append)\n"
		"cut(Append)\n"
		"delete(MoveTo)\n"
		"find(\"Pattern\",All,MatchCase,Down,RegExp,Select,MatchWord)\n"
		"functionList\n"
		"indent(Insert)\n"
		"insert(\"Text\", Forward)\n"
		"isModified\n"
		"isSelected\n"
		"move(MoveTo, Select)\n"
		"paste\n"
		"pause\n"
		"print\n"
		"read(\"FileName\")\n"
		"redo(ToMark)\n"
		"replace(\"Pattern\",\"Text\",All,MatchCase,RegExp,Select,MatchWord) \n"
		"replaceAll(\"Pattern\",\"Text\",All,MatchCase,RegExp,Select,MatchWord\n"
		"selectedText\n"
		"selectMatchedText\n"
		"setModified(Modified)\n"
		"setParameter(ParamName,Value)\n"
		"setReadOnly(Enable)\n"
		"show(Item, iRow)\n"
		"switch\n"
		"tabify(iExtraSpaces,ToTabs)\n"
		"text\n"
		"textLength\n"
		"textRows\n"
		"undo(ToMark)\n"
		"word(SelectWord)\n"
		"write(FileName)\n"
		"\n"
		"OPTIONS\n"
		" 1 / 0                      - May be used in place of any option name\n"
		"true/false                  - May be used in place of any option name\n"
		"All/NotAll (false)          - Replace all/one match or undo operation\n"
		"Append/NoAppend (false)     - Append/Dont-append text to clipboard\n"
		"Down/Up (true)              - Search Down/Up from cursor\n"
		"Enable/Disable (false)      - Enable/Disable setModified, setReadOnly\n"
		"Forward/Backward (true)     - Move or search forward/backward\n"
		"Insert/Remove (true)        - Insert/Remove comment/indent\n"
		"MatchCase/NoMatch (false)   - Case sensitive/insensitive search\n"
		"MatchWord/NoMatchWord(false)- Matched text on/not-on word boundaries\n"
		"RegExp/NoRegExp (false)     - Treat pattern as a reg-exp/plain text.\n"
		"Select/NoSelect (false)     - Select/dont select text on move\n"
		"Select/NoSelect (false)     - Search selected/all text\n"
		"ToMark/One (true)           - Undo or redo ToMark/One operation\n"
		"ToTabs/ToSpaces (true)      - Convert leading spaces to tabs or vice versa\n"
		"\n"
		"MOVETO OPERATIONS\n"
		"nomove - don't move\n"
		"bol - beginning-of-line\n"
		"bot - beginning-of-text\n"
		"eol - end-of-line\n"
		"eot - end-of-text\n"
		"nextbow - next beginning-of-word\n"
		"nextchar - next char\n"
		"nextcur - next cursor in cursor stack\n"
		"nexteow - next end-of-word\n"
		"nexthalf - next half-page\n"
		"nextline - next line\n"
		"nextpage - next page\n"
		"nextsow -  next bow if not at already at bow\n"
		"prevbow - previous beginning-of-word\n"
		"prevchar - previous char\n"
		"prevcur  - previous cursor in cursor stack\n"
		"prevhalf - previous half-page\n"
		"prevline - previous line\n"
		"prevpage - previous page\n"
		"prevsow - previous bow if not already at bow\n"
		"prevtab - previous tab stop\n"
		"same - no move\n"
		"\n"
		"SHOW ITEMS\n"
		"cursors- Show anchor, cursor, end cursor\n"
		"row    - Show row specified by Row\n"
		"text   - Show text near row specified by Row\n"
		"undo   - Show undo queue\n"
		"\n"
		"SET PARAMETERS\n"
		"* The units, default value, and range follow each parameter name\n"
		"Default - sets default values shown (Value is ignored)\n"
		"Font = [string]Courier New (FontFamilyNames)\n"
		"FontSize = [points]10, (8-12,14,16,18,20)\n"
		"Language = [string]Text (text,html,cpp,javascript,lisp)\n"
		"LineNumbers [#]0 (0, 1) \n"
		"MatchParens [#]1 (0,1)\n"
		"MaximumRows [#]maxInt, (1-maxInt)\n"
		"MaximumRowWidth [chars]1024, (4 - 2048)\n"
		"MaximumUndoDepth [#]1024, (0-4096)\n"
		"ShowWhiteSpace [#]0 (0,1)\n"
		"TabWidth [chars]4 (1-32)\n"
		"WordWrap [#]0 (0, 1) \n";
	// Set text
	cUi.upTextEdit->append(QString(apHelp));
	setWindowTitle(tr("AIS Edit API Help"));

}

// end

