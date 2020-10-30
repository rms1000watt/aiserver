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
aisdev/atextedit/afinddialog.h
													Search Dialog

 
CHANGE HISTORY
Version	Date		Who		Change
3.2006	 3/10/2008	fchua	Simplified the class interface using signal/slots framework.
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0107	 9/22/2006	tlw		getSettings. Use editWord to get a nearby word.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QSettings>
#include <QtGui/QLineEdit>
#include "afinddialog.h"
#include "atextedit.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
 * \brief AFindDialog is a modal pop-up dialog used to search for a string.
 *
 * Constructor initializes the Qt Designer-generated GUI.
 * \param[in] ipParent -> the parent that created this instance of the class.
 * \param[in] ipName -> the name assigned to this instance of the class
 * \param[in] iFlags determines the behavior and layout of the dialog.
 */
AFindDialog::AFindDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlags)
    : AOperationsDialog(ipParent, ipName, iFlags), cpSettings(0), cpTextEdit(0)
{
	// Widgets. Initialize GUI.
	cUi.setupUi(this);
	cUi.upFindCombo->setFocus();

	setObjectName(QString(ipName));
	setSizeGripEnabled(true);
	setWindowTitle(tr("Find Text"));

	// Buttons. Hook up buttons
	connect(cUi.upFindButton, SIGNAL(clicked()), this, SLOT(onFind()));
	connect(cUi.upCloseButton, SIGNAL(clicked()), this, SLOT(close()));
}

/*!
 * \brief Save the dialog settings before closing.
 */
void AFindDialog::closeEvent(QCloseEvent *ipEvent)
{
	saveSettings();
	ipEvent->accept();
}

/*!
 * \brief Initialize all the dialog settings saved from last use.
 *
 * \param[in] ipSettings -> Place to hold current settings
 * \param[in] ipTextEdit -> Currently active instance of ATextEdit
 *
 * \note
 * - Called by text editor before showing this dialog.
 */
void AFindDialog::init(QSettings* ipSettings, ATextEdit *ipTextEdit)
{
	// Save. Save pointers back into TextEdit
	cpTextEdit = ipTextEdit;
	cpSettings = ipSettings;
	cUi.upStatusLabel->setText("Press Return or Find to find next match, Close and use F3 to repeat");

	QString aPattern;

	// Pattern. Set pattern to nearby word
	if (cpTextEdit != NULL)
		aPattern = cpTextEdit->editWord(false/*Select*/);

	if (cpSettings != NULL)
	{
		// Settings. Get current settings.
		cpSettings->beginGroup(ATextEdit::scApplication);
		cUi.upAllRadio->setChecked(cpSettings->value("FindAll", QVariant(false)).toBool());
		cUi.upCaseCheckBox->setChecked(cpSettings->value("FindCase", QVariant(true)).toBool());
		cUi.upDownRadio->setChecked(cpSettings->value("FindDown", QVariant(true)).toBool());
		cUi.upRegExpCheckBox->setChecked(cpSettings->value("FindRegExp", QVariant(false)).toBool());
		cUi.upSelectRadio->setChecked(cpSettings->value("FindSelect", QVariant(false)).toBool());
		cUi.upToEndRadio->setChecked(cpSettings->value("FindToEnd", QVariant(true)).toBool());
		cUi.upUpRadio->setChecked(cpSettings->value("FindUp", QVariant(false)).toBool());
		cUi.upWordsCheckBox->setChecked(cpSettings->value("FindWords", QVariant(false)).toBool());
		if (aPattern.isEmpty())
			aPattern = cpSettings->value("FindText", QVariant("")).toString();
		cpSettings->endGroup();
	}
	else
	{
		cUi.upCaseCheckBox->setChecked(true);
		cUi.upAllRadio->setChecked(true);
		cUi.upDownRadio->setChecked(true);
	}

	// Set Pattern. Stow the pattern in the FindCombo's text box.
	QLineEdit* apLine = cUi.upFindCombo->lineEdit();
	apLine->setText(aPattern);
	apLine->selectAll();

	// Focus. Set initial focus to the Find combo.
	cUi.upFindCombo->setFocus();
}

/*!
 * \brief Emit a findPattern signal which contains the search parameters.
 */
void AFindDialog::onFind()
{
	QString aPattern = cUi.upFindCombo->currentText();
	if (aPattern.isEmpty())
	{	cUi.upStatusLabel->setText("Nothing to find. Enter search pattern in Find text box.");
		cUi.upFindCombo->setFocus();
		qApp->beep();
	}
	else
	{	// Start from just past the current cursor position and search.
		bool aAll = cUi.upAllRadio->isChecked();			// Continue search at end
		bool aCase = cUi.upCaseCheckBox->isChecked();		// Case sensitive flag
		bool aDown  = cUi.upDownRadio->isChecked();			// Search down from cursor
		bool aRegExp = cUi.upRegExpCheckBox->isChecked();	// Use-regular-expression flag
		bool aSelect  = cUi.upSelectRadio->isChecked();		// Search selected text
		bool aWords = cUi.upWordsCheckBox->isChecked();		// Match whole words

		emit findPattern(aPattern, aAll, aCase, aDown, aRegExp, aSelect, aWords);
	}
}

/*!
 * \brief Handles the result of the find operation.
 */
void AFindDialog::onFindPatternResult(bool iResult)
{
	if (iResult)
	{
		cUi.upStatusLabel->setText("Match. Press Find or press Close and use F3 to repeat search");
		cUi.upCloseButton->setFocus();
	}
	else
	{
		if (cUi.upAllRadio->isChecked())
		{
			cUi.upStatusLabel->setText("No match.");
			cUi.upFindCombo->setFocus();
		}
		else
		{
			cUi.upStatusLabel->setText("No match. Choose AllText to search entire doc.");
			cUi.upFindButton->setFocus();
		}
		qApp->beep();
	}
}

/*!
 * \brief Save all the changes to the dialog settings.
 *
 * \note
 * - Call after the dialog has been closed.
 */
void AFindDialog::saveSettings()
{
	if (cpSettings != NULL)
	{
		// Settings. Save current settings.
		cpSettings->beginGroup(ATextEdit::scApplication);
		cpSettings->setValue("FindAll", QVariant(cUi.upAllRadio->isChecked()));			// Search region set to entire document
		cpSettings->setValue("FindCase", QVariant(cUi.upCaseCheckBox->isChecked()));	// Conduct case-sensitive search
		cpSettings->setValue("FindDown", QVariant(cUi.upDownRadio->isChecked()));		// Search down from cursor
		cpSettings->setValue("FindRegExp", QVariant(cUi.upRegExpCheckBox->isChecked()));// Treat pattern as a regular exp
		cpSettings->setValue("FindSelect",QVariant(cUi.upSelectRadio->isChecked()));	// Search region set to selected text
		cpSettings->setValue("FindText",QVariant(cUi.upFindCombo->lineEdit()->text()));	// Search text

		cpSettings->setValue("FindToEnd", QVariant(cUi.upToEndRadio->isChecked()));		// Search region is from cursor to end
		cpSettings->setValue("FindUp", QVariant(cUi.upUpRadio->isChecked()));			// Search up from cursor
		cpSettings->setValue("FindWords", QVariant(cUi.upWordsCheckBox->isChecked()));	// Patterns don't begin or end in-a-word
		cpSettings->endGroup();
	}
}

void AFindDialog::showOnTop()
{
	show();
	raise();
	activateWindow();
}

// the end
