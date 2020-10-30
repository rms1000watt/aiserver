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
aisdev/aised/atexteditprefdialog.cpp
												ATextEdit Preference Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2008	 4/01/2008	fchua	Initial Version

												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#include "atexteditprefdialog.h"

/*!
 * \brief Constructor.
 *
 * \param[in] ipParent Pointer to parent QWidget.
 * \param[in] iFlags QDialog display flags.
 */
ATextEditPrefDialog::ATextEditPrefDialog(QWidget *ipParent, const char *ipName, Qt::WFlags iFlags)
	: APrefDialog(ipParent, ipName, iFlags)
{
	cUi.setupUi(this);
}

/*!
 * \brief Destructor.
 */
ATextEditPrefDialog::~ATextEditPrefDialog()
{

}

/*!
 * \brief Called when OK button is clicked.
 */
void ATextEditPrefDialog::on_upOk_clicked()
{
	accept();
}

/*!
 * \brief Called when Cancel button is clicked.
 */
void ATextEditPrefDialog::on_upCancel_clicked()
{
	reject();
}

/*!
 * \brief Sets the values for each widget based from the given AParameters instance.
 *
 * \param[in] ipParams Pointer to an instance of AParameters.
 */
void ATextEditPrefDialog::setParameters(AParameters *ipParams)
{
	if (ipParams != 0)
	{
		QFont aFont(ipParams->mFont, ipParams->mFontSize, QFont::Normal, false);
		cUi.upFontComboBox->setCurrentFont(aFont);
		cUi.upFontSizeSpinBox->setValue(ipParams->mFontSize);
		cUi.upLineCheckBox->setChecked(ipParams->mLineNumbers);
		cUi.upWordWrapCheckBox->setChecked(ipParams->mWrap);
		cUi.upTabWidthSpinBox->setValue(ipParams->mTabWidth);
		cUi.upWhiteSpaceCheckBox->setChecked(ipParams->mShowWhiteSpace);
	}
}

/*!
 * \brief Copies the values of each widget to the given AParameter instance.
 *
 * \param[out] ioParams Pointer to an instance of AParameters.
 */
void ATextEditPrefDialog::getParameters(AParameters *iopParams)
{
	if (iopParams != 0)
	{
		iopParams->mFont = cUi.upFontComboBox->currentFont().family();
		iopParams->mFontSize = cUi.upFontSizeSpinBox->value();
		iopParams->mLineNumbers = cUi.upLineCheckBox->isChecked();
		iopParams->mWrap = cUi.upWordWrapCheckBox->isChecked();
		iopParams->mTabWidth = cUi.upTabWidthSpinBox->value();
		iopParams->mShowWhiteSpace = cUi.upWhiteSpaceCheckBox->isChecked();
	}
}
