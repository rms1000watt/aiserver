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
#ifndef ACABINETSETTINGS_H
#define ACABINETSETTINGS_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/acabinetsettings.h
															Cabinet Settings Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0000	7/28/2008	rca		Initial Code
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include "ui_acabsettingsdialog.h"
#include "aremotefiledialog.h"
#include "appclient.h"

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief ACabinetSettings - Select cabinet settings for synchronization and automatic compilation.
 */
class ACabSettingsDialog : public QDialog
{
    Q_OBJECT
public:
    ACabSettingsDialog(AAppClient* ipAppClient, QWidget* ipParent, const char* ipName, bool iModal = false, Qt::WFlags iFlgs = 0);
	QString getDatabaseName();
	QString getDatabaseLocation();
	QString getSourceLocation();
	QString getDependencies();
	QString getStorageScope();
	QString getImportSync();
	QString getExportSync();
	QString getAutoCompile();
	QString getSearchSwitch();
	void setDefault();
	void setData(const QString& iCabName, const QString& iDatabaseLocation, const QString& iSourceLocation,
		const QString& iImportSync, const QString& iExportSync, const QString& iStorageScope, const QString& iAutoCompile,
		const QString& iSearchSwitch, const QString& iDependencies);

public slots:
	void onAccept();
	void onReject();
	void onBrowseDatabase();
	void onBrowseSource();

private:
	Ui::ACabSettingsClass	cUi;
	ARemoteFileDialog		cRemoteFileDialog;
	AAppClient*				cpAppClient;
};

#endif // ACABINETSETTINGS_H
