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
aisdev/atextedit/ahelpdialog.h
														Help Dialog

CHANGE HISTORY
Version	Date		Who		Change
2.0004	2/14/2007	tlw		Shft-Insert. Add Shift-Insert to toggle line numbers. Revise Ctrl-Insert.
2.0003	2/6/2007	tlw		Ctrl-M. Make descriptions agree.
1.0015	11/17/2006	tlw		WordWrap.  Add toggle word wrap
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0110	10/12/2206	tlw		Modify ctrlshift-F5 for test dialog and ctrlshift-Del for clearUndo
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QLabel>
#include <QtGui/QTextEdit>

#include "ahelpdialog.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief AHelpDialog - Constructor initializes the Qt-Designer-generated GUI and initializes the help text

\param ipParent -> the parent that created this instance of the class.
\param ipName -> the name assigned to this instance of the class
\return void
*/
AHelpDialog::AHelpDialog(QWidget* ipParent, const char* ipName)
  :	QDialog(ipParent)
{
	// Widgets. Initialize GUI.
	if (ipName == NULL) ipName = "HelpDialog";
	setObjectName(QString(ipName));
	setSizeGripEnabled(true);
	cUi.setupUi(this);

	// Help text
	const char *apHelp = "AIS EDITOR HELP:\n"
		"* Command requires selected text\n"
		"** Command requires selected text or matched parens\n"
		"ANYMoveKey - any command that moves cursor\n"
		"ANYDelKey  - any command that deletes text\n"
		"ANYInsKey  - any key that inserts text\n"
		"\n"
		"MOVE:\n"
		"Shft+ANYMoveKey  Selects intervening text\n"
		"Shft+ANYMoveKey *Extends selected text to cursor\n"
		"\n"
		"Left         Move to prev char\n"
		"Ctrl-H       Move to prev char\n"
		"Right        Move to next char\n"
		"Ctrl-L       Move to next char\n"
		"Ctrl-Left    Move to prev beginning-of-word\n"
		"Ctrl-P       Move to prev beginning-of-word\n"
		"Ctrl-Right   Move to next beginning-of-word\n"
		"Ctrl-N       Move to next beginning-of-word\n"
		"BackTab      Move to prev tabstop\n"
		"Home         Move to beginning-of-line\n"
		"Ctrl-B       Move to beginning-of-line\n"
		"End          Move to end-of-line\n"
		"Ctrl-E       Move to end-of-line\n"
		"Up           Move to up one line\n"
		"Ctrl-K       Move to up one line\n"
		"Down         Move to down one line\n"
		"Ctrl-J       Move to down one line\n"
		"Ctrl-Up      Move up half-page\n"
		"Ctrl-Down    Move down half-page\n"
		"PgUp         Move up one page\n"
		"PgDown       Move down one page\n"
		"Ctrl-PgUp    Move to prev saved cursor position\n"
		"Ctrl-PgDown  Move to next saved cursor position\n"
		"Ctrl-Home    Move to beginning-of-text\n"
		"Ctrl-End     Move to end-of-text\n"
		"Ctrl-^     **Move cursor to other end of matched text\n"
		"MouseClick   Move cursor to Ibeam cursor\n"
		"\n"		
		"DELETE:\n"
		"ANYDelKey   *Delete selected text\n"
		"BackSpace    Delete prev char\n"
		"Delete       Delete next char\n"
		"Ctrl-D       Delete next char\n"
		"Ctrl-Del     Delete left word\n"
		"Ctrl-BS      Delete right word\n"
		"Ctrl-U       Delete to beginning-of-line\n"
		"Ctrl-.       Delete to end-of-line\n"
		"Ctrl-X      *Cut selected text\n"
		"\n"
		"INSERT:\n"
		"ANYInsKey   *Deletes selected before insert\n"
		"Tab          Insert tab\n"
		"Ctrl-I       Insert tab\n"
		"Enter        Insert newline\n"
		"Ctrl-V      *Paste selected text\n"
		"\n"
		"SELECTED TEXT:\n"
		"Ctrl-A       Select all text\n"
		"Ctrl-C      *Copy selected text to clipboard\n"
		"CtrlShft-C  *Append selected text to clipboard\n"
		"Ctrl-M       Select/unselect matched text\n"
		"Ctrl-X      *Move selected text to clipboard\n"
		"CtrlShft-X  *Delete and append sel. text to clipboard\n"
		"Shft-Click   Select text to Ibeam cursor\n"
		"Drag         Move cursor and select intervening text\n"
		"DblClick     If at paren, select between matching\n"
		"             parens, else select nearby word\n"
		"\n"
		"DIALOGS:\n"
		"RtClick      Context menu\n"
		"Ctrl-F       Find dialog\n"
		"Alt-L        Lambda List dialog\n"
		"Ctrl-G       Go-to-line dialog\n"
		"Ctrl-R       Replace dialog\n"
		"Ctrl-T       Tabify dialog\n"
		"CtrlShft-T   Set tab width dialog\n"
		"F1           This help dialog\n"
		"Alt-F1       Alphabetical help dialog\n"
		"Ctrl-F1      Command help dialog\n"
		"Shft-F1      Search help dialog\n"
		"F3           Repeat find down\n"
		"Shft-F3      Repeat find up\n"
		"Ctrl-F3      Search All Text\n"
		"CtrlShft-F5  Test dialog\n"
		"\n"
		"MATCHED TEXT:\n"
		"Ctrl-(       Toggle bracket matching\n"
		"Ctrl-)       Toggle bracket matching\n"
		"Ctrl-M       Select/unselect matched text\n"
		"\n"
		"UNDO:\n"
		"Ctrl-Y       Redo last set of undos\n"
		"CtrlShift-Y  Redo last undo\n"
		"Ctrl-Z       Undo last set of cmds\n"
		"CtrlShft-Z   Undo last cmd\n"
		"CtrlShft-Del Clear undo stack\n"
		"\n"
		"FILE:\n"
		"Alt-C        Close file\n"
		"Alt-N        Open new file\n"
		"Alt-O        Open existing file\n"
		"Ctrl-S       Save file\n"
		"CtrlShft-S   Save all files\n"
		"\n"
		"FORMAT & SWITCH:\n"
		"Ctrl-Tab     Switch documents\n"
		"Ctrl-;      *Comment selected text\n"
		"CtrlShft-;  *Uncomment selected text\n"
		"Ctrl-W	      Toggle word wrap\n"
		"Tab         *Indent selected text\n"
		"BackTab     *UnIndent selected text\n"
		"Insert       Toggle display whitespace\n"
		"Ctrl-Insert  Toggle bracket matching\n"
		"Shft-Insert  Toggle line numbers\n"
		"* Only if text is selected\n"
		"** Only if text is selected or matched";
	cUi.upTextEdit->append(QString(apHelp));
	setWindowTitle(tr("AIS Help"));
}

/*	---------------------------------------------------------------------------------------------------------------------------
closeDialog - Hide this dialog from view
Args:
	none
Returns:
	nothing
Notes:
 1.	Does not really close the dialog, just hides it.
	------------------------------------------------------------------------------------------------------------------------ */
void AHelpDialog::closeDialog()
{
	hide();
}
// end

