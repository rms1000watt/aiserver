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
aisdev/atextedit/ahelpfinddialog.h
													Help Find Dialog
 
CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QLabel>
#include <QtGui/QTextEdit>

#include "ahelpfinddialog.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief AHelpApiDialog - Constructor initializes the Qt-Designer-generated GUI and initializes the help text

\param ipParent -> the parent that created this instance of the class.
\param ipName -> the name assigned to this instance of the class
\param iFlags - determines the behavior and layout of the dialog.
\return void
*/
AHelpFindDialog::AHelpFindDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlags)
  :
	QDialog(ipParent, iFlags)
{
	// Widgets. Initialize GUI.
	if (ipName == NULL) ipName = "HelpFindDialog";
	setObjectName(QString(ipName));
	setSizeGripEnabled(true);
	cUi.setupUi(this);

	// Help text
	const char *apHelp = "AIS EDITOR FIND HELP:\n"
		"=> is read as \"matches\"\n"
		"whitespace is space,tab,newline\n"
		"alphanumeric is letter,digit,underscore\n"
		"\n"
		"ELEMENTS\n"
		"\\x => x  (x is \\.?*+|^$[](){}\n"
		"\\b => word break (not a char)\n"
		"\\d => one digit (0-9)\n"
		"\\D => one non-digit\n"
		"\\f => 0xc (form feed)\n"
		"\\n => 0xa (line feed)\n"
		"\\r => 0xd (carriage return)\n"
		"\\s => whitespace char\n"
		"\\S => non-whitespace char\n"
		"\\t => 0x9 (tab)\n"
		"\\v => 0xb (vertical tab)\n"
		"\\w => one alphanumeric char\n"
		"\\W => one non-alphanumeric char\n"
	   "\\0nnn  => char with ascii code 0nnn\n"
		"[abc]  => one char in list\n"
		"[^abc] => one char not in list\n"
		"[a-c]  => one char in range\n"
		"[^a-c] => one char not in range\n"
		". => any char\n"
		"\n"
		"REGULAR EXPRESSIONS\n"
		"E represents an element or any of:\n"
		"(E)    => an instance of E\n"
		"E?     => zero or one instance of E\n"
		"E+     => one or more instances of E\n"
		"E*     => zero or more instances of E\n"
		"E{n}   => n instances of E\n"
		"E{n,}  => n or more instances of E\n"
		"E{,m}  => zero to m instances of E\n"
		"E{n,m} => n to m instances of E\n"
		"E|E    => either instance of E\n"
		"^E     => row starts with E\n"
		"E$     => row ends with E\n"
		"E(?=E) => 1st E iff followed by 2nd E\n"
		"E(?!E) => 1st E iff not followed by 2nd E\n"
		"\n"
		"EXAMPLES\n"
	  "\\bXYZ\\b            => XYZ\n"
	"\\w+\\s*=\\s*\\w+      => ab = c or a7=c\n"
	   "^\\s*(#|$)          => # comment or blank line\n"
  "\\$\\(\\s*\\w*\\s*\\)    => $(QTDIR)\n"
	  "[/\\\\]              => slash or backslash\n"
	   "[{<([\\])}>}]       => any opening, closing bracket\n"
	"\\s*\\(def\\w{2,6}\\s+ => (defun or (deforphan\n";
	// Set text
	setWindowTitle(tr("Regular Expression Help"));
	cUi.upTextEdit->append(QString(apHelp));
}

// end
