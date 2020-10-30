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
aisdev/atextedit/areplacedialog.h
														Replace Dialog
 Areplacedialog is a modal pop-up dialog used to replace an existing string with a new value.
 
CHANGE HISTORY
Version	Date		Who		Change
3.2006	 3/10/2008	fchua	Simplified the class interface using signal/slots framework.
1.0118	12/12/2006	tlw		onSelection. Disable replace button if the selection Region is selected.
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0108	 9/30/2006	tlw		Change prefix on calls to ATextEdit API.
1.0105	 9/12/2006	tlw		Find after a successful replace.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QSettings>
#include <QtGui/QLineEdit>

#include "areplacedialog.h"
#include "atextedit.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
 * \brief Replace dialog constructor.
 */
AReplaceDialog::AReplaceDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlags)
    : AOperationsDialog(ipParent, ipName, iFlags), cpSettings(NULL), cpTextEdit(NULL)
{
	cUi.setupUi(this);

	// Widgets. Initialize GUI.
	setObjectName(QString(ipName));
	setSizeGripEnabled(true);
	setWindowTitle(tr("Replace Matched Text"));
	
	// Buttons. Hook up buttons
	connect(cUi.upCloseButton, SIGNAL(clicked()), this, SLOT(close()), Qt::DirectConnection);
	connect(cUi.upFindButton, SIGNAL(clicked()), this, SLOT(onFind()), Qt::DirectConnection);
	connect(cUi.upReplaceButton, SIGNAL(clicked()), this, SLOT(onReplace()), Qt::DirectConnection);
	connect(cUi.upReplaceAllButton, SIGNAL(clicked()), this, SLOT(onReplaceAll()), Qt::DirectConnection);
	connect(cUi.upSelectionRadio, SIGNAL(toggled(bool)), this, SLOT(onSelection(bool)), Qt::DirectConnection);
}

/*!
 * \brief Saves the dialog settings before the dialog closes.
 */
void AReplaceDialog::closeEvent(QCloseEvent *ipEvent)
{
	saveSettings();
	ipEvent->accept();
}

/*!
 * \brief Emits a replaceFindPattern signal which contains the search parameters.
 *
 * \param[in] irPattern Pattern to be searched.
 */
void AReplaceDialog::find(const QString& irPattern)
{
	if (irPattern.isEmpty())
	{	cUi.upStatusLabel->setText("Nothing to find. Enter search pattern in Find text box.");
		cUi.upFindCombo->setFocus();
		qApp->beep();
	}
	else
	{	// Find. Start from just past the current cursor position and search.
		bool aAllText = cUi.upAllRadio->isChecked();		// Continue search at end
		bool aCase = cUi.upCaseCheckBox->isChecked();		// Case sensitive flag
		bool aDown = true;									// Search down from cursor
		bool aRegExp = cUi.upRegExpCheckBox->isChecked();	// Use-regular-expression flag
		bool aSelect  = cUi.upSelectionRadio->isChecked();	// Search selected text
		bool aWords = cUi.upWordsCheckBox->isChecked();		// Match whole words

		emit replaceFindPattern(irPattern, aAllText, aCase, aDown, aRegExp, aSelect, aWords);
	}
}

/*!
 * \brief Initialize all the dialog settings saved from last use.
 *
 * \param[in] ipSettings -> Place to hold the current settings.
 * \param[in] ipTextEdit -> Save ptr to text for use later by find to search the text.
 *
 * \note
 * - Called by text editor before showing this dialog.
 */
void AReplaceDialog::init(QSettings* ipSettings, ATextEdit *ipTextEdit)
{
	// Save. Save pointers back into TextEdit
	cpTextEdit = ipTextEdit;
	cpSettings = ipSettings;
	cUi.upStatusLabel->setText("Select Find to find next match. Select Replace to replace matched.");

	// Settings. Get current settings.
	bool aReplaceSelection, aReplaceToEnd;
	if (cpSettings != NULL)
	{
		cpSettings->beginGroup(ATextEdit::scApplication);
		cUi.upAllRadio->setChecked(cpSettings->value("ReplaceAll", QVariant(false)).toBool());
		cUi.upCaseCheckBox->setChecked(cpSettings->value("ReplaceCase", QVariant(true)).toBool());
		cUi.upRegExpCheckBox->setChecked(cpSettings->value("ReplaceRegExp", QVariant(false)).toBool());
		aReplaceSelection = cpSettings->value("ReplaceSelection",QVariant(false)).toBool();
		aReplaceToEnd = cpSettings->value("ReplaceToEnd", QVariant(true)).toBool();
		if (aReplaceSelection && cpTextEdit != NULL && !cpTextEdit->editIsSelected())
		{	aReplaceSelection = false;
			aReplaceToEnd = true;
		}
		cUi.upSelectionRadio->setChecked(aReplaceSelection);
		cUi.upToEndRadio->setChecked(aReplaceToEnd);
		cUi.upFindCombo->lineEdit()->setText(cpSettings->value("ReplaceFindText",QVariant("")).toString());
		cUi.upReplaceCombo->lineEdit()->setText(cpSettings->value("ReplaceText",QVariant("")).toString());
		cUi.upWordsCheckBox->setChecked(cpSettings->value("ReplaceWords", QVariant(false)).toBool());
		cpSettings->endGroup();
	}
	else
	{
		cUi.upCaseCheckBox->setChecked(true);
		cUi.upToEndRadio->setChecked(true);
	}

	// Focus. Set initial focus to the Find combo.
	cUi.upFindCombo->setFocus();
}

/*!
 * \brief Called when the Find button is activated.
 */
void AReplaceDialog::onFind()
{
	QString aPattern = cUi.upFindCombo->currentText();
	find(aPattern);
}

/*!
 * \brief Called when the Replace button is activated.
 *
 * \note
 * - Emits a replacePattern signal which contains the replace parameters.
 */
void AReplaceDialog::onReplace()
{
	const QString& arPattern = cUi.upFindCombo->lineEdit()->text();
	const QString& arText = cUi.upReplaceCombo->lineEdit()->text();
	if (arPattern.isEmpty())
		cUi.upStatusLabel->setText("No pattern to match in Find text box");
	else
	{	// Find.
		bool aAllText = cUi.upAllRadio->isChecked();		// Search entire text from Cursor back to Cursor.
		bool aMatchCase = cUi.upCaseCheckBox->isChecked();	// Case sensitive search
		bool aRegExp = cUi.upRegExpCheckBox->isChecked();	// Treat pattern as a regular expression.
		bool aMatchWord = cUi.upWordsCheckBox->isChecked();	// Start/end match on word boundaries.

		emit replacePattern(arPattern, arText, aAllText, aMatchCase, aRegExp, aMatchWord);
	}
}

/*!
 * \brief Called when the Replace All button is activated.
 *
 * \note
 * - Emits a replaceAllPattern signal which contains the replace parameters.
 */
void AReplaceDialog::onReplaceAll()
{
	const QString& arPattern = cUi.upFindCombo->lineEdit()->text();
	const QString& arReplace = cUi.upReplaceCombo->lineEdit()->text();
	if (arPattern.isEmpty())
		cUi.upStatusLabel->setText("No pattern to match in Find text box");
	else
	{	bool aAllText = cUi.upAllRadio->isChecked();		// Search entire text from Cursor back to Cursor.
		bool aCase = cUi.upCaseCheckBox->isChecked();		// Case sensitive search
		bool aRegExp = cUi.upRegExpCheckBox->isChecked();	// Treat pattern as a regular expression.
		bool aSelect = cUi.upSelectionRadio->isChecked();	// Search selected text.
		bool aWords = cUi.upWordsCheckBox->isChecked();		// Start/end match on word boundaries.

		emit replaceAllPattern(arPattern, arReplace, aAllText, aCase, aRegExp, aSelect, aWords);
	}
}

/*!
 * \brief Handles the result of the find operation.
 *
 * \param[in] iResult Result of search.
 */
void AReplaceDialog::onReplaceFindPatternResult(bool iResult)
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
 * \brief Handles the result of replace and replace all operation.
 *
 * \param[in] iResult Result of replace.
 */
void AReplaceDialog::onReplacePatternResult(bool iResult)
{
	if (iResult)
	{
		cUi.upStatusLabel->setText("Match. Press Find to go to next match or press Replace to replace next match with Replace.");
		cUi.upReplaceButton->setFocus();
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
 * \brief Enables/disables the replace button when the selection option is activated.
 */
void AReplaceDialog::onSelection(bool iIsChecked)
{
	cUi.upReplaceButton->setDisabled(iIsChecked);
}

/*!
 * \brief Save the dialog settings.
 *
 * \note
 * - Called after the dialog has been closed.
 */
void AReplaceDialog::saveSettings()
{
	if (cpSettings != NULL)
	{
		// Settings. Save current settings.
		cpSettings->beginGroup(ATextEdit::scApplication);
		cpSettings->setValue("ReplaceAll", QVariant(cUi.upAllRadio->isChecked()));				// Search region set to entire document
		cpSettings->setValue("ReplaceCase", QVariant(cUi.upCaseCheckBox->isChecked()));			// Conduct case-sensitive search
		cpSettings->setValue("ReplaceRegExp", QVariant(cUi.upRegExpCheckBox->isChecked()));		// Treat pattern as a regular exp
		cpSettings->setValue("ReplaceSelection",QVariant(cUi.upSelectionRadio->isChecked()));	// Search region set to selected text
		cpSettings->setValue("ReplaceFindText",QVariant(cUi.upFindCombo->lineEdit()->text()));	// Search pattern
		cpSettings->setValue("ReplaceText",QVariant(cUi.upReplaceCombo->lineEdit()->text()));	// Replacement text
		cpSettings->setValue("ReplaceToEnd", QVariant(cUi.upToEndRadio->isChecked()));			// Search region is from cursor to end
		cpSettings->setValue("ReplaceWords", QVariant(cUi.upWordsCheckBox->isChecked()));		// Patterns don't begin or end in-a-word
		cpSettings->endGroup();
	}
}

void AReplaceDialog::showOnTop()
{
	show();
	raise();
	activateWindow();
}
