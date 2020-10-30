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
aisdev/atextedit/atabifydialog.cpp
														Tabify Dialog
Converts leading spaces to tabs on each line.

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "atabifydialog.h"
#include "atextedit.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------

ATabifyDialog::ATabifyDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlags)
    : QDialog(ipParent, iFlags)
{
	// Widgets. Initialize GUI.
	setObjectName(QString(ipName));
	setSizeGripEnabled(false);
	setWindowTitle(tr("Convert Spaces to Tabs"));
	cUi.setupUi(this);

	// Set class variables
	cpTextEdit = NULL;

	// signals and slots connections
	connect(cUi.upTabifyButton, SIGNAL(clicked()), this, SLOT(onTabify()));
	connect(cUi.upDetabButton, SIGNAL(clicked()), this, SLOT(onDetab()));
	connect(cUi.upCloseButton, SIGNAL(clicked()), this, SLOT(reject()));
}

/*	---------------------------------------------------------------------------------------------------------------------------
onDetab - Replace tabs with spaces
Args:
	none
Returns:
	nothing
Notes:
	------------------------------------------------------------------------------------------------------------------------ */
void ATabifyDialog::onDetab()
{
	tabify(false/*ToTabs*/);
}
/*	---------------------------------------------------------------------------------------------------------------------------
onTabify - Replace spaces with tabs
Args:
	none
Returns:
	nothing
Notes:
	------------------------------------------------------------------------------------------------------------------------ */
void ATabifyDialog::onTabify()
{
	tabify(true/*ToTabs*/);
}

/*!
\brief setEditor - Initialize all the dialog settings saved from last use

\param ipTextEdit -> pointer to text for later use.
\return void
\par Notes:
-# Called by text editor before showing this dialog.
 */
void ATabifyDialog::setEditor(ATextEdit *ipTextEdit)
{
	cpTextEdit = ipTextEdit;
	cUi.upStatusLabel->clear();
}

/*	---------------------------------------------------------------------------------------------------------------------------
tabify - Convert tabs to spaces or vice versa
Args:
	iToTabs		If true, replace tabs with spaces or vice versa
Returns:
	nothing
Notes:
 1.	If converting 4 spaces to a tab, user can choose to keep, convert or discard fewer than 4 spaces.
	------------------------------------------------------------------------------------------------------------------------ */
void ATabifyDialog::tabify(bool iToTabs)
{
	// Decide what to do with extra spaces (keep, convert-to-tab, or toss.
	long aExtraSpaces = (cUi.upDeleteRadioButton->isChecked()) ? -1 : (cUi.upTabRadioButton->isChecked()) ? 1 : 0;
	if (!cpTextEdit->editTabify(aExtraSpaces, iToTabs))
		cUi.upStatusLabel->setText("No selected text. Close and highlight lines to be tabified (Ctrl-A selects all lines).");
}

// end
