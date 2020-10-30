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
aisdev/aforms/acabsettingsdialog.cpp
											Cabinet Settings Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0000	07/27/2008	rca		Cabinet settings dialog
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "acabsettingsdialog.h"
#include <QtGui/QMessageBox>

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
\brief ACabSettingsDialog - Constructor instantiates the GUI elements generated in ui_acabsettingsdialog.h
 
 \par Args:
 \param ipAppClient -> Application client that forwards commands to the AIS server.
 \param ipParent -> Parent widget that created this instance of the dialog.
 \param ipName -> Name assigned to this instance of the dialog
 \param iModal true iff this dialog is modal
 \param iFlgs Optional flags to set the disposition of this dialog.
 \par Returns:
 nothing
 */
ACabSettingsDialog::ACabSettingsDialog(AAppClient*ipAppClient, QWidget* ipParent, const char* ipName, bool iModal, Qt::WFlags iFlgs)
    : QDialog(ipParent, iFlgs),cRemoteFileDialog(ipAppClient,"&path",this,"Cabinet File Name"), cpAppClient(ipAppClient)
{
    Q_UNUSED(iModal);

	cUi.setupUi(this);
	setObjectName(ipName);
	QStringList aSelectionList;
	aSelectionList << "none" << "ask" << "auto" << "notify";
	cUi.cImportSync->addItems( aSelectionList );
	cUi.cExportSync->addItems( aSelectionList );
	cUi.cStorageScope->addItem( (QString)"file" );
	cUi.cStorageScope->addItem( (QString)"folder" );
	connect( cUi.upButtonBox, SIGNAL( accepted() ), this, SLOT( onAccept() ));
	connect( cUi.upButtonBox, SIGNAL( rejected() ), this, SLOT( onReject() ));
	connect( cUi.cBrowseDatabase, SIGNAL( clicked() ), this, SLOT( onBrowseDatabase() ));
	connect( cUi.cBrowseSource, SIGNAL( clicked() ), this, SLOT( onBrowseSource() ));
}

/*!
\brief onAccept - Slot for OK Button

\par Args:
nothing
\par Returns:
nothing
*/
void ACabSettingsDialog::onAccept()
{
	bool aOk = true;
	QString aCaption = "Cabinet Settings";
	QString aError = "";
	QStringList aExtentList = cpAppClient->getCurrentExtentNames();
	if( getDatabaseName() == "" )
	{
		aError = "Please specify a database name";
		aOk = false;
	}
	else if( getDatabaseLocation() == "" )
	{
		aError = "Please specify a database location";
		aOk = false;
	}
	else if( getSourceLocation() == "" )
	{
		aError = "Please specify a source location";
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
\brief onReject - Slot for Cancel Button

\par Args:
nothing
\par Returns:
nothing
*/
void ACabSettingsDialog::onReject()
{
	reject();
}

/*!
\brief onBrowseDatabase - Browse a database file

\par Args:
nothing
\par Returns:
nothing
*/
void ACabSettingsDialog::onBrowseDatabase()
{
	QString aName = getDatabaseName();
	if( aName == "" )
	{
		QMessageBox::warning(this, "Browse Database", "Please specify a database name", QMessageBox::Ok, QMessageBox::NoButton);
	}
	else
	{
		cRemoteFileDialog.setFilters("AIS (*.db)");
		cRemoteFileDialog.setWindowTitle("New Cabinet - Select cabinet file name");
		cRemoteFileDialog.setFileName(aName + ".db");
		if (cRemoteFileDialog.exec() == QDialog::Accepted)
		{
			QString aFilePath = cRemoteFileDialog.filePath();
			cUi.cDatabaseLocation->setText( aFilePath );
		}
	}
}

/*!
\brief onBrowseSource - Browse a source file

\par Args:
nothing
\par Returns:
nothing
*/
void ACabSettingsDialog::onBrowseSource()
{
	QString aName = getDatabaseName();
	if( aName == "" )
	{
		QMessageBox::warning(this, "Browse Source Location", "Please specify a database name", QMessageBox::Ok, QMessageBox::NoButton);
	}
	else
	{
		cRemoteFileDialog.setFilters("AIS (*.sl)");
		cRemoteFileDialog.setWindowTitle("New Cabinet - Select source file name");
		cRemoteFileDialog.setFileName(aName + ".sl");
		if (cRemoteFileDialog.exec() == QDialog::Accepted)
		{
		QString aFilePath = cRemoteFileDialog.filePath();
		cUi.cSourceLocation->setText( aFilePath );
		}
	}
}

/*!
\brief getDatabaseName - Returns the database name

\par Args:
nothing
\par Returns:
Database Name
*/
QString ACabSettingsDialog::getDatabaseName()
{
	return cUi.cDatabaseName->text();
}

/*!
\brief getDatabaseLocation - Returns the database location

\par Args:
nothing
\par Returns:
Database Location
*/
QString ACabSettingsDialog::getDatabaseLocation()
{
	return cUi.cDatabaseLocation->text();
}

/*!
\brief getSourceLocation - Returns the source location

\par Args:
nothing
\par Returns:
Source Location
*/
QString ACabSettingsDialog::getSourceLocation()
{
	return cUi.cSourceLocation->text();
}

/*!
\brief getDependencies - Returns the lambda dependencies

\par Args:
nothing
\par Returns:
Lambda Dependencies
*/
QString ACabSettingsDialog::getDependencies()
{
	return cUi.cDependencies->text();
}

/*!
\brief getImportSync - Returns the Import Synchronization

\par Args:
nothing
\par Returns:
Import Synchronization
*/
QString ACabSettingsDialog::getImportSync()
{
	return cUi.cImportSync->currentText();
}

/*!
\brief getExportSync - Returns the Export Synchronization

\par Args:
nothing
\par Returns:
Export Synchronization
*/
QString ACabSettingsDialog::getExportSync()
{
	return cUi.cExportSync->currentText();
}

/*!
\brief getStorageScope - Returns the Storage Scope

\par Args:
nothing
\par Returns:
Storage Scope
*/
QString ACabSettingsDialog::getStorageScope()
{
	return cUi.cStorageScope->currentText();
}

/*!
\brief getAutoCompile - Returns the AutoCompile value

\par Args:
nothing
\par Returns:
Auto Compile
*/
QString ACabSettingsDialog::getAutoCompile()
{
	if(cUi.cAutoCompile->checkState() == Qt::Checked)
	{
		return "true";
	}
	return "false";
}

/*!
\brief getSearchSwitch - Returns the Search Switch value

\par Args:
nothing
\par Returns:
Search Switch
*/
QString ACabSettingsDialog::getSearchSwitch()
{
	if(cUi.cSearchSwitch->checkState() == Qt::Checked)
	{
		return "true";
	}
	return "false";
}

/*!
\brief setDefault - Redraws the UI to have the default settings

\par Args:
nothing
\par Returns:
Auto Compile
*/
void ACabSettingsDialog::setDefault()
{
	cUi.cDatabaseName->setText( "" );
	cUi.cDatabaseLocation->setText( "" );
	cUi.cSourceLocation->setText( "" );
	cUi.cDependencies->setText( "" );
	cUi.cImportSync->setCurrentIndex(0);
	cUi.cExportSync->setCurrentIndex(0);
	cUi.cStorageScope->setCurrentIndex(0);
	cUi.cAutoCompile->setCheckState( Qt::Checked );
	cUi.cSearchSwitch->setCheckState( Qt::Checked );
	cUi.cDatabaseName->setEnabled( true );
	cUi.cDatabaseLocation->setEnabled( true );
	cUi.cSourceLocation->setEnabled( true );
}

/*!
\brief setCabinetMetaData - Redraws the UI with the arguments as settings

\par Args:
nothing
\par Returns:
Auto Compile
*/
void ACabSettingsDialog::setData(const QString& iCabName, const QString& iDatabaseLocation, const QString& iSourceLocation,
	const QString& iStorageScope, const QString& iImportSync, const QString& iExportSync, const QString& iAutoCompile,
	const QString& iSearchSwitch, const QString& iDependencies)
{
	cUi.cDatabaseName->setText( iCabName );
	cUi.cDatabaseLocation->setText( iDatabaseLocation );
	cUi.cSourceLocation->setText( iSourceLocation );
	cUi.cDependencies->setText( iDependencies );
	cUi.cImportSync->setCurrentIndex(cUi.cImportSync->findText(iImportSync));
	cUi.cExportSync->setCurrentIndex(cUi.cExportSync->findText(iExportSync));
	cUi.cStorageScope->setCurrentIndex(cUi.cStorageScope->findText(iStorageScope));
	cUi.cAutoCompile->setCheckState( ( iAutoCompile == "true" ) ? Qt::Checked : Qt::Unchecked );
	cUi.cSearchSwitch->setCheckState( ( iSearchSwitch == "true" ) ? Qt::Checked : Qt::Unchecked );
	cUi.cDatabaseName->setEnabled( false );
	cUi.cDatabaseLocation->setEnabled( false );
	cUi.cSourceLocation->setEnabled( false );
}
// end
