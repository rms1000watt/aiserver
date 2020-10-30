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
aisdev/aforms/aadduserdialog.cpp
														Add User Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2001	01/27/2008	fchua	Initial version.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "aadduserdialog.h"

//	------------------------------------------------------ METHODS ------------------------------------------------------------

/*!
 * \brief Constructor.
 */
AAddUserDialog::AAddUserDialog(QWidget *parent)
	: QDialog(parent)
{
	cUi.setupUi(this);
	cUi.upEndDate->setMinimumDate(QDate::currentDate());
	cUi.upEndDate->setDate(QDate::currentDate().addYears(1));

	cUi.upSecurityComboBox->addItem("0 - Disabled");
	cUi.upSecurityComboBox->addItem("1");
	cUi.upSecurityComboBox->addItem("2");
	cUi.upSecurityComboBox->addItem("3");
	cUi.upSecurityComboBox->addItem("4");
	cUi.upSecurityComboBox->addItem("5");
	cUi.upSecurityComboBox->addItem("6");
	cUi.upSecurityComboBox->addItem("7 - Super User");
	
	cUi.upUsernameErrorLabel->hide();
	cUi.upPasswordErrorLabel->hide();

	adjustSize();
}

/*!
 * \brief Destructor.
 */
AAddUserDialog::~AAddUserDialog()
{

}

/*!
 * \brief Resets the values of the form.
 */
void AAddUserDialog::clearAll()
{
	cUi.upCommentLineEdit->clear();
	cUi.upConfirmLineEdit->clear();
	cUi.upEndDate->setDate(QDate::currentDate().addYears(1));
	cUi.upPasswordLineEdit->clear();
	cUi.upSecurityComboBox->setCurrentIndex(0);
	cUi.upUsernameLineEdit->clear();

	cUi.upUsernameErrorLabel->clear();
	cUi.upUsernameErrorLabel->hide();
	cUi.upPasswordErrorLabel->clear();
	cUi.upPasswordErrorLabel->hide();
}

/*!
 * \brief Retrieves the value of comment.
 */
const QString AAddUserDialog::getComment()
{
	return cUi.upCommentLineEdit->text();
}

/*!
 * \brief Retrieves the value of end date.
 */
const QDate AAddUserDialog::getEndDate()
{
	return cUi.upEndDate->date();
}

/*!
 * \brief Retrieves the value of password.
 */
const QString AAddUserDialog::getPasssword()
{
	return cUi.upPasswordLineEdit->text();
}

/*!
 * \brief Retrieves the value of security level.
 */
long AAddUserDialog::getSecurityLevel()
{
	return cUi.upSecurityComboBox->currentIndex();
}

/*!
 * \brief Retrieves the value of username.
 */
const QString AAddUserDialog::getUsername()
{
	return cUi.upUsernameLineEdit->text();
}

/*!
 * \brief Returns true if the password checkbox is checked.
 */
bool AAddUserDialog::isPasswordSet()
{
	return (cUi.upChgPasswordCheckBox->checkState() == Qt::Checked);
}

/*!
 * \brief Sets the user dialog to edit user mode.
 *
 * \param[in] iFlag Edit only flag.
 */
void AAddUserDialog::setEditOnly(bool iFlag)
{
	cUi.upChgPasswordCheckBox->setChecked(!iFlag);
	cUi.upChgPasswordCheckBox->setVisible(iFlag);
	
	if (iFlag)
		setWindowTitle("Edit User Data");
	else
		setWindowTitle("Add New User");
}

/*!
 * \brief This function is called when the Cancel button is pressed.
 */
void AAddUserDialog::on_upCancelButton_clicked()
{
	adjustSize();
	reject();
}

/*!
 * \brief This function is called when the password checkbox value is changed.
 *
 * \param iState New state of the checkbox.
 */
void AAddUserDialog::on_upChgPasswordCheckBox_stateChanged(int iState)
{
	switch(iState)
	{
	case Qt::Checked:
		cUi.upPasswordLineEdit->setEnabled(true);
		cUi.upConfirmLineEdit->setEnabled(true);
		break;
	case Qt::Unchecked:
		cUi.upPasswordLineEdit->setEnabled(false);
		cUi.upConfirmLineEdit->setEnabled(false);
		break;
	default:
		break;
	}
}

/*!
 * \brief This function is called when the OK button is pressed.
 */
void AAddUserDialog::on_upOkButton_clicked()
{
	// Check values first before accepting
	bool aError = false;

	// clear the errors
	cUi.upUsernameErrorLabel->clear();
	cUi.upUsernameErrorLabel->hide();

	cUi.upPasswordErrorLabel->clear();
	cUi.upPasswordErrorLabel->hide();

	// Check username
	if (cUi.upUsernameLineEdit->text().isEmpty())
	{
		cUi.upUsernameErrorLabel->show();
		cUi.upUsernameErrorLabel->setText("<font color=red>Error: Username is empty.</font>");
		aError = true;
	}

	if (isPasswordSet())
	{
		if (cUi.upPasswordLineEdit->text().isEmpty() && cUi.upConfirmLineEdit->text().isEmpty())
		{
			cUi.upPasswordErrorLabel->show();
			cUi.upPasswordErrorLabel->setText("<font color=red>Error: Password is empty.</font>");
			aError = true;
		}
		else if (cUi.upPasswordLineEdit->text() != cUi.upConfirmLineEdit->text())
		{
			cUi.upPasswordErrorLabel->show();
			cUi.upPasswordErrorLabel->setText("<font color=red>Error: Passwords do not match.</font>");
			aError = true;
		}
	}

	if (!aError)
		accept();

	adjustSize();
}

/*!
 * \brief Sets the value of comment.
 *
 * \param[in] irComment Comment for the user.
 */
void AAddUserDialog::setComment(const QString& irComment)
{
	cUi.upCommentLineEdit->setText(irComment);
}

/*!
 * \brief Sets the value of end date.
 *
 * \param[in] irEndDate End date.
 */
void AAddUserDialog::setEndDate(const QDate& irEndDate)
{
	cUi.upEndDate->setDate(irEndDate);
}

/*!
 * \brief Sets the value of security level.
 *
 * \param[in] iSecurityLevel Security level.
 */
void AAddUserDialog::setSecurityLevel(const long iSecurityLevel)
{
	cUi.upSecurityComboBox->setCurrentIndex(iSecurityLevel);
}

/*!
 * \brief Sets the value of username.
 *
 * \param[in] irUsername Username.
 */
void AAddUserDialog::setUsername(const QString& irUsername)
{
	cUi.upUsernameLineEdit->setText(irUsername);
}
