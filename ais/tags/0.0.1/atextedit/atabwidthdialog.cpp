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
aisdev/atextedit/atabwidthdialog.h
														Tab Width Dialog
The TabWidth dialog is a modal pop-up dialog used to allow the user to set the tabstop.

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "atabwidthdialog.h"
#include "atextedit.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief ATabWidthDialog - Constructor initializes the Qt-Designer-generated GUI

\param ipParent -> the parent that created this instance of the class.
\param ipName -> the name assigned to this instance of the class
\param iFlgs - determines the behavior and layout of the dialog.
\return void
*/
ATabWidthDialog::ATabWidthDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs)
    : QDialog(ipParent, iFlgs)
{
	// Widgets. Initialize GUI.
	setObjectName(QString(ipName));
	setSizeGripEnabled(false);
	setWindowTitle(tr("Set Tab Stop Width"));
	cUi.setupUi(this);

	// Class Variables.
	cTabWidth = ATextEdit::scDefaultTabWidth;
	
	// signals and slots connections
	connect(cUi.upSetButton, SIGNAL(clicked()), this, SLOT(onSetTabWidth()));
	connect(cUi.upCancelButton, SIGNAL(clicked()), this, SLOT(reject()));
}

/*!
\brief getTabWidth - Return tab width to caller after dialog has been closed

\return void
 */
long ATabWidthDialog::getTabWidth()
{
	return cTabWidth;
}

/*	---------------------------------------------------------------------------------------------------------------------------
onSetTabWidth - Set the tab width set by user
Args:
	none
Returns:
	nothing
Notes:
 1.	Save current tab width setting and close the dialog
	------------------------------------------------------------------------------------------------------------------------ */
void ATabWidthDialog::onSetTabWidth()
{
	long aTabWidth = cUi.upTabWidthSpinBox->value();
	if (aTabWidth > 0 && aTabWidth <= 128)
	{	cTabWidth = aTabWidth;
		accept();
	}
	else
		reject();
	///close();
}
