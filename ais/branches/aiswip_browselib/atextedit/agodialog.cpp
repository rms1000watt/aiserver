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
aisdev/atextedit/agodialog.h
														Go Dialog
 
CHANGE HISTORY
Version	Date		Who		Change
3.2006	 3/10/2008	fchua	Simplified the class interface using signal/slots framework.
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QLineEdit>
#include <QtGui/QRegExpValidator>
#include "agodialog.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*
 * \brief Constructor initializes the Qt Designer-generated GUI.
 *
 * The Go dialog is a modal pop-up dialog used to allow the user to enter a line number.
 * \param[in] ipParent -> the parent that created this instance of the class.
 * \param[in] ipName -> the name assigned to this instance of the class
 * \param[in] iFlags - determines the behavior and layout of the dialog.
 */
AGoDialog::AGoDialog(QWidget* ipParent, const char* ipName,  Qt::WFlags iFlags)
    : AOperationsDialog(ipParent, ipName, iFlags)
{
	// Widgets. Initialize GUI.
	cUi.setupUi(this);

	setObjectName(QString(ipName));
	setSizeGripEnabled(false);
	setWindowTitle(tr("Go To Line"));

	// Connect. Hook up buttons.
	connect(cUi.upGoButton, SIGNAL(clicked()), this, SLOT(onGoTo()));
	connect(cUi.upCloseButton, SIGNAL(clicked()), this, SLOT(reject()));

	// Create number validator
	QRegExp aRx("^\\d+$");
	QRegExpValidator* aValidator = new QRegExpValidator(aRx, this);
	cUi.upLineEdit->setValidator(aValidator);
}

/*!
 * \brief Initializes the dialog.
 *
 * \note
 * -# Sets the status message.
 * -# Sets the focus.
 */
void AGoDialog::init()
{
	cUi.upStatusLabel->setText(tr("Enter a line number, press return."));
	cUi.upLineEdit->setFocus();
}

/*!
 * \brief Emits a signal when the go button is activated.
 */
void AGoDialog::onGoTo()
{
	const QString& arLineNum = cUi.upLineEdit->text();
	if (!arLineNum.isEmpty())
	{
		bool aOk;
		long aLine = arLineNum.toInt(&aOk);
		emit goToLine(--aLine);
		accept();
	}
	else
		cUi.upStatusLabel->setText("No number. Please enter a line number.");
}

void AGoDialog::showOnTop()
{
	show();
	raise();
	activateWindow();
}
