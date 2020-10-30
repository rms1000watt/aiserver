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
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/aaboutdialog.cpp
											About AIS Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		Remove unused destructor
1.0070	10/27/2005	tlw		Add an about AIS dialog to AMainWindow
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "aaboutdialog.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
\brief AAboutDialog - Constructor instantiates the GUI elements generated in ui_aaboutdialog.h
 
 \par Args:
 \param ipParent -> Parent widget that created this instance of the dialog.
 \param ipName -> Name assigned to this instance of the dialog
 \param iModal true iff this dialog is modal
 \param iFlgs Optional flags to set the disposition of this dialog.
 \par Returns:
 nothing
 */
AAboutDialog::AAboutDialog(QWidget* ipParent, const char* ipName, bool iModal, Qt::WFlags iFlgs)
    : QDialog(ipParent, iFlgs)
{
    Q_UNUSED(iModal);

	cUi.setupUi(this);
	setObjectName(ipName);
	cUi.upAboutLabel->setAlignment(Qt::AlignLeft | Qt::AlignTop);
	QObject::connect(cUi.upOkButton, SIGNAL(clicked()), this, SLOT(accept()));
}
// end
