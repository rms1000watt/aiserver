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
aisdev/aforms/aideprefdialog.cpp
												AMainWindow Preference Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2009   5/08/2008	fchua	Added getCabinetParameters, setCabinetParameters, getDebugParameters, setDebugParameters
3.2008	 4/07/2008	fchua	Initial Version

												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

#include "aideprefdialog.h"

/*!
 * \brief Constructor.
 *
 * \param[in] ipParent Pointer to parent widget.
 * \param[in] ipName Name of the dialog.
 * \param[in] iFlags Dialog display flags.
 */
AIdePrefDialog::AIdePrefDialog(QWidget *ipParent, const char *ipName, Qt::WFlags iFlags)
	: QDialog(ipParent, iFlags)
{
	cUi.setupUi(this);
	setObjectName(ipName);
}

/*!
 * \brief Destructor.
 */
AIdePrefDialog::~AIdePrefDialog()
{

}

/*!
 * \brief Store cabinet page settings in output parameter.
 *
 * \param[out] iopParams Pointer to AParameters instance.
 */
void AIdePrefDialog::getCabinetParameters(AParameters *iopParams)
{
	if (iopParams != NULL)
	{
		iopParams->mFont = cUi.upCabFont->currentFont().family();
		iopParams->mFontSize = cUi.upCabFontSize->value();
	}
}

/*!
 * \brief Store console settings in output parameter.
 *
 * \param[out] iopParams Pointer to AParameters instance.
 */
void AIdePrefDialog::getConsoleParameters(AParameters *iopParams)
{
	if (iopParams != NULL)
	{
		iopParams->mFont = cUi.upConsoleFont->currentFont().family();
		iopParams->mFontSize = cUi.upConsoleFontSize->value();
		iopParams->mTabWidth = cUi.upConsoleTabWidth->value();
		iopParams->mLineNumbers = cUi.upConsoleLineNumbers->isChecked();
		iopParams->mShowWhiteSpace = cUi.upConsoleWhiteSpaces->isChecked();
		iopParams->mWrap = cUi.upConsoleWordWrap->isChecked();		
	}
}

/*!
 * \brief Store debug page settings in output parameter.
 *
 * \param[out] iopParams Pointer to AParameters instance.
 */
void AIdePrefDialog::getDebugParameters(AParameters *iopParams)
{
	if (iopParams != NULL)
	{
		iopParams->mFont = cUi.upDebuggerFont->currentFont().family();
		iopParams->mFontSize = cUi.upDebuggerFontSize->value();
		iopParams->mTabWidth = cUi.upDebuggerTabWidth->value();
	}
}

/*!
 * \brief Store editor settings in output parameter.
 *
 * \param[out] iopParams Pointer to AParameters instance.
 */
void AIdePrefDialog::getEditorParameters(AParameters *iopParams)
{
	if (iopParams != NULL)
	{
		iopParams->mFont = cUi.upEditorFont->currentFont().family();
		iopParams->mFontSize = cUi.upEditorFontSize->value();
		iopParams->mTabWidth = cUi.upEditorTabWidth->value();
		iopParams->mLineNumbers = cUi.upEditorLineNumbers->isChecked();
		iopParams->mShowWhiteSpace = cUi.upEditorWhiteSpaces->isChecked();
		iopParams->mWrap = cUi.upEditorWordWrap->isChecked();		
	}	
}

/*!
 * \brief Store log settings in output parameter.
 *
 * \param[out] iopParams Pointer to AParameters instance.
 */
void AIdePrefDialog::getLogParameters(AParameters *iopParams)
{
	if (iopParams != NULL)
	{
		iopParams->mFont = cUi.upLogFont->currentFont().family();
		iopParams->mFontSize = cUi.upLogFontSize->value();
		iopParams->mLineNumbers = cUi.upLogLineNumbers->isChecked();
		iopParams->mWrap = cUi.upLogWordWrap->isChecked();		
	}	
}

/*!
 * \brief Update the cabinet settings' widget values.
 *
 * \param[in] ipParams Pointer to AParameters instance.
 */
void AIdePrefDialog::setCabinetParameters(AParameters *ipParams)
{
	if (ipParams != 0)
	{
		QFont aFont(ipParams->mFont, ipParams->mFontSize, QFont::Normal, false);
		cUi.upCabFont->setCurrentFont(aFont);
		cUi.upCabFontSize->setValue(ipParams->mFontSize);
	}
}

/*!
 * \brief Update the console settings' widget values.
 *
 * \param[in] ipParams Pointer to AParameters instance.
 */
void AIdePrefDialog::setConsoleParameters(AParameters *ipParams)
{
	if (ipParams != 0)
	{
		QFont aFont(ipParams->mFont, ipParams->mFontSize, QFont::Normal, false);
		cUi.upConsoleFont->setCurrentFont(aFont);
		cUi.upConsoleFontSize->setValue(ipParams->mFontSize);
		cUi.upConsoleTabWidth->setValue(ipParams->mTabWidth);
		cUi.upConsoleLineNumbers->setChecked(ipParams->mLineNumbers);
		cUi.upConsoleWhiteSpaces->setChecked(ipParams->mShowWhiteSpace);
		cUi.upConsoleWordWrap->setChecked(ipParams->mWrap);
	}
}

/*!
 * \brief Update the debug settings' widget values.
 *
 * \param[in] ipParams Pointer to AParameters instance.
 */
void AIdePrefDialog::setDebugParameters(AParameters *ipParams)
{
	if (ipParams != 0)
	{
		QFont aFont(ipParams->mFont, ipParams->mFontSize, QFont::Normal, false);
		cUi.upDebuggerFont->setCurrentFont(aFont);
		cUi.upDebuggerFontSize->setValue(ipParams->mFontSize);
		cUi.upDebuggerTabWidth->setValue(ipParams->mTabWidth);
	}
}

/*!
 * \brief Update the console settings' widget values.
 *
 * \param[in] ipParams Pointer to AParameters instance.
 */
void AIdePrefDialog::setEditorParameters(AParameters *ipParams)
{
	if (ipParams != 0)
	{
		QFont aFont(ipParams->mFont, ipParams->mFontSize, QFont::Normal, false);
		cUi.upEditorFont->setCurrentFont(aFont);
		cUi.upEditorFontSize->setValue(ipParams->mFontSize);
		cUi.upEditorTabWidth->setValue(ipParams->mTabWidth);
		cUi.upEditorLineNumbers->setChecked(ipParams->mLineNumbers);
		cUi.upEditorWhiteSpaces->setChecked(ipParams->mShowWhiteSpace);
		cUi.upEditorWordWrap->setChecked(ipParams->mWrap);
	}
}

/*!
 * \brief Update the log settings' widget values.
 *
 * \param[in] ipParams Pointer to AParameters instance.
 */
void AIdePrefDialog::setLogParameters(AParameters *ipParams)
{
	if (ipParams != 0)
	{
		QFont aFont(ipParams->mFont, ipParams->mFontSize, QFont::Normal, false);
		cUi.upLogFont->setCurrentFont(aFont);
		cUi.upLogFontSize->setValue(ipParams->mFontSize);
		cUi.upLogLineNumbers->setChecked(ipParams->mLineNumbers);
		cUi.upLogWordWrap->setChecked(ipParams->mWrap);
	}
}

/*!
 * \brief Called when the OK button is activated.
 */
void AIdePrefDialog::on_upOkButton_clicked()
{
	accept();
}

/*!
 * \brief Called when the Cancel button is activated.
 */
void AIdePrefDialog::on_upCancelButton_clicked()
{
	reject();
}
