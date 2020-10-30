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
aisdev/aforms/aregisterdirdialog.cpp
											Register Directory Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0000	07/27/2008	rca		Register directory dialog
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "aregisterdirdialog.h"
#include <QtGui/QMessageBox>

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
\brief ARegisterDirDialog - Default Constructor
 
 \par Args:
 nothing
 \par Returns:
 nothing
 */
ARegisterDirDialog::ARegisterDirDialog(QWidget* ipParent)
    : QDialog(ipParent)
{
	cUi.setupUi(this);
	connect( cUi.cRegisterButton, SIGNAL(clicked()), this, SLOT(onRegister()));
	connect( cUi.cCancelButton, SIGNAL(clicked()), this, SLOT(onCancel()));
	connect( cUi.cSameSourceFlag, SIGNAL(stateChanged(int)), this, SLOT(onStateChange()));
	cUi.cSameSourceFlag->setCheckState( Qt::Checked );
}

/*!
\brief onRegister - Check for invalid directories and return a warning if there is any, otherwise call accept()

\par Args:
nothing
\par Returns:
nothing
*/
void ARegisterDirDialog::onRegister()
{
	bool aOk = true;
	QString aCaption = "Register Directory";
	QString aError = "";

	if( getSameSourceFlag() == "true" )
	{
		cUi.cSourceDirectory->setText( getDatabaseDirectory() );
	}
	if( getDatabaseDirectory() == "" )
	{
		aError = "Please specify a database directory";
		aOk = false;
	}
	else if( getSourceDirectory() == "" )
	{
		aError = "Please specify a source directory";
		aOk = false;
	}
	if( !aOk )
	{
		QMessageBox::warning(this, aCaption, aError, QMessageBox::Ok, QMessageBox::NoButton);
	}
	else
		accept();
}

/*!
\brief onCancel - Slot for Cancel Button

\par Args:
nothing
\par Returns:
nothing
*/
void ARegisterDirDialog::onCancel()
{
	reject();
}

/*!
\brief onStateChange - Enable/Disable the source directory depending on the checkbox value

\par Args:
nothing
\par Returns:
nothing
*/
void ARegisterDirDialog::onStateChange()
{
	if( getSameSourceFlag() == "true" )
	{
		cUi.label_2->setEnabled( false );
		cUi.cSourceDirectory->setText( "" );
		cUi.cSourceDirectory->setEnabled( false );
	}
	else
	{
		cUi.label_2->setEnabled( true );
		cUi.cSourceDirectory->setEnabled( true );
	}
}

/*!
\brief getDatabaseDirectory - Returns the database directory

\par Args:
nothing
\par Returns:
Database Directory
*/
QString ARegisterDirDialog::getDatabaseDirectory()
{
	return cUi.cDatabaseDirectory->text();
}

/*!
\brief getSourceDirectory - Returns the source directory

\par Args:
nothing
\par Returns:
Source Directory
*/
QString ARegisterDirDialog::getSourceDirectory()
{
	return cUi.cSourceDirectory->text();
}

/*!
\brief getSameSourceFlag - Returns the cSameSourceFlag value

\par Args:
nothing
\par Returns:
Auto Compile
*/
QString ARegisterDirDialog::getSameSourceFlag()
{
	if(cUi.cSameSourceFlag->checkState() == Qt::Checked)
	{
		return "true";
	}
	return "false";
}

/*!
\brief setDefault - Resets the UI to have the default settings

\par Args:
nothing
\par Returns:
Auto Compile
*/
void ARegisterDirDialog::setDefault()
{
	cUi.cDatabaseDirectory->setText( "" );
	cUi.cSourceDirectory->setText( "" );
	cUi.cSameSourceFlag->setCheckState( Qt::Checked );
	cUi.cSourceDirectory->setEnabled( false );
}
// end
