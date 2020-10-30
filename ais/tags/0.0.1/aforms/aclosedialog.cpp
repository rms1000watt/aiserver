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
aisdev/aforms/aclosedialog.cpp
														Close Dialog Page

CHANGE HISTORY
Version	Date		Who		Change
4.0003	 7/01/2008	fchua	[CR-120] Added function to describe default close mode.
3.1005	12/14/2007	fchua	Initial version.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "aclosedialog.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/*!
 * \brief Constructor.
 */
ACloseDialog::ACloseDialog(QWidget *ipParent, const QString &irName)
	: QDialog(ipParent), cCloseMode(geDefault)
{
	cUi.setupUi(this);
	setWindowTitle("Close Session " + irName);
}

/*!
 * \brief Destructor.
 */
ACloseDialog::~ACloseDialog()
{

}

/*!
 * \brief Returns the close mode selected.
 */
ACloseMode ACloseDialog::getCloseMode()
{
	return cCloseMode;
}

/*!
 * \brief This function is invoked when the OK button is clicked.
 */
void ACloseDialog::on_uiOkButton_clicked()
{
	accept();
}

/*!
 * \brief This function is invoked when the Cancel button is clicked.
 */
void ACloseDialog::on_uiCancelButton_clicked()
{
	reject();
}

/*!
 * \brief This function is invoked when the Default Radio Button is selected.
 */
void ACloseDialog::on_uiDefaultRadio_toggled(bool)
{
	cCloseMode = geDefault;
}

/*!
 * \brief This function is invoked when the Disconnect Radio Button is selected.
 */
void ACloseDialog::on_uiDisconnectRadio_toggled(bool)
{
	cCloseMode = geDisconnect;
}

/*!
 * \brief This function is invoked when the Soft Radio Button is selected.
 */
void ACloseDialog::on_uiSoftRadio_toggled(bool)
{
	cCloseMode = geSoft;
}

/*!
 * \brief This function is invoked when the Firm Radio Button is selected.
 */
void ACloseDialog::on_uiFirmRadio_toggled(bool)
{
	cCloseMode = geFirm;
}

/*!
 * \brief This function is invoked when the Hard Radio Button is selected.
 */
void ACloseDialog::on_uiHardRadio_toggled(bool)
{
	cCloseMode = geHard;
}

/*!
 * \brief Sets the default close mode.
 */
void ACloseDialog::setDefaultCloseMode(ACloseMode iCloseMode)
{
	QString aDefault;
	switch(iCloseMode)
	{
		case geDisconnect:
			aDefault = "Disconnect";			
			break;
		case geSoft:
			aDefault = "Soft";
			break;
		case geFirm:
			aDefault = "Firm";
			break;
		case geHard:
			aDefault = "Hard";
			break;
		default:
			aDefault = "Not Set";
			break;
	}

	cUi.uiDefaultRadio->setText("Default (" + aDefault + ")");
}

// End
