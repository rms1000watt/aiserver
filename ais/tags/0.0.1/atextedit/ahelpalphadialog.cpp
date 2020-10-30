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
aisdev/atextedit/ahelpalphadialog.h
														Help Dialog

CHANGE HISTORY
Version	Date		Who		Change
2.0004	2/14/2007	tlw		Shft-Insert. Add Shift-Insert to toggle line numbers. Revise Ctrl-Insert.
2.0003	2/6/2007	tlw		Alt-F1. Add alphabetical listing of edit commands
												--------------- ------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QLabel>
#include <QtGui/QTextEdit>

#include "ahelpalphadialog.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief AHelpAlphaDialog - Constructor initializes the Qt-Designer-generated GUI and initializes the help text

\param ipParent -> the parent that created this instance of the class.
\param ipName -> the name assigned to this instance of the class
\return void
*/
AHelpAlphaDialog::AHelpAlphaDialog(QWidget* ipParent, const char* ipName)
  :	QDialog(ipParent)
{
	// Widgets. Initialize GUI.
	if (ipName == NULL) ipName = "HelpAlphaDialog";
	setObjectName(QString(ipName));
	setSizeGripEnabled(true);
	cUi.setupUi(this);

	// Help text
	const char *apHelp = "AIS EDITOR ALPHABETICAL HELP:\n"
	"Ctrl-(      Toggle bracket matching\n"
	"Ctrl-)      Toggle bracket matching\n"
	"Ctrl-.      Delete to end-of-line\n"
	"Ctrl-;      Comment selected text\n"
	"CtrlShft-;  Uncomment selected text\n"
	"Ctrl-^      Move cursor to other end of matched text\n"
	"Ctrl-A      Select all text\n"
	"Ctrl-B      Move to beginning-of-line\n"
	"Ctrl-C      Copy selected text to clipboard\n"
	"CtrlShft-C  Append selected text to clipboard\n"
	"Alt-C       Close file\n"
	"Ctrl-D      Delete next char\n"
	"Ctrl-E      Move to end-of-line\n"
	"Ctrl-F      Find dialog\n"
	"Ctrl-G      Go-to-line dialog\n"
	"Ctrl-H      Move to prev char\n"
	"Ctrl-I      Insert tab\n"
	"Ctrl-J      Move to down one line\n"
	"Ctrl-K      Move to up one line\n"
	"Ctrl-L      Move to next char\n"
	"Alt-L       Lambda List dialog\n"
	"Ctrl-M      Select/unselect matched text\n"
	"Alt-N       Open new file\n"
	"Ctrl-N      Move to next beginning-of-word\n"
	"Alt-O       Open existing file\n"
	"Ctrl-P      Move to prev beginning-of-word\n"
	"Ctrl-R      Replace dialog\n"
	"Ctrl-S      Save file\n"
	"CtrlShft-S  Save all files\n"
	"Ctrl-T      Tabify dialog\n"
	"CtrlShft-T  Set tab width dialog\n"
	"Ctrl-U      Delete to beginning-of-line\n"
	"Ctrl-V      Paste selected text\n"
	"Ctrl-W      Toggle word wrap\n"
	"Ctrl-X      Move selected text to clipboard\n"
	"CtrlShft-X  Cut and append selected text\n"
	"Ctrl-Y      Redo last set of undos\n"
	"CtrlShft-Y  Redo last undo\n"
	"Ctrl-Z      Undo last set of cmds\n"
	"CtrlShft-Z  Undo last cmd\n"

	"BackSpace   Delete prev char\n"
	"Ctrl-Del    Delete left word\n"
	"Ctrl-BS     Delete right word\n"
	"BackTab     Move to prev tabstop\n"
	"BackTab     UnIndent selected text\n"
	"Delete      Delete next char\n"
	"CtrlSh-Del  Clear undo stack\n"
	"Down        Move to down one line\n"
	"Ctrl-Down   Move down half-page\n"
	"End         Move to end-of-line\n"
	"Ctrl-End    Move to end-of-text\n"
	"Enter       Insert newline\n"
	"F1          Edit help dialog\n"
	"Alt-F1      This help dialog\n"
	"Ctrl-F1     Command help dialog\n"
	"Shift-F1    Search help dialog\n"
	"F3          Repeat find down\n"
	"Ctrl-F3     Search All Text\n"
	"Shift-F3    Repeat find up\n"
	"CtrlShft-F5 Test dialog\n"
	"Home        Move to beginning-of-line\n"
	"Ctrl-Home   Move to beginning-of-text\n"
	"Insert      Toggle display whitespace\n"
	"Ctrl-Insert Toggle bracket matching\n"
	"Shft-Insert Toggle line numbers\n"
	"Left        Move to prev char\n"
	"Ctrl-Left   Move to prev beginning-of-word\n"
	"PgDown      Move down one page\n"
	"Ctrl-PgDown Move to next saved cursor position\n"
	"PgUp        Move up one page\n"
	"Ctrl-PgUp   Move to prev saved cursor position\n"
	"Right       Move to next char\n"
	"Ctrl-Right  Move to next beginning-of-word\n"
	"Tab         Insert tab\n"
	"Tab         Indent selected text\n"
	"Ctrl-Tab    Switch documents\n"
	"Up          Move to up one line\n"
	"Ctrl-Up     Move up half-page";
	cUi.upTextEdit->append(QString(apHelp));
	setWindowTitle(tr("AIS Alphabetical Help"));
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
void AHelpAlphaDialog::closeDialog()
{
	hide();
}
// end

