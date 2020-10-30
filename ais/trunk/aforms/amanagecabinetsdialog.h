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
#ifndef AMANAGECABINETSDIALOG_H
#define AMANAGECABINETSDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/amanagecabinetsdialog.h
															Manage Cabinets Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0000	7/28/2008	rca		Initial Code
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include "ui_amanagecabinetsdialog.h"
#include "ais.h"
#include "appclient.h"

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief AManageCabinetsDialog - Manage cabinets
 */
class AManageCabinetsDialog : public QDialog, public AReturnRcvr
{
    Q_OBJECT
public:
    AManageCabinetsDialog(AAppClient* ipAppClient, QWidget* ipParent, const char* ipName, bool iModal = false, Qt::WFlags iFlgs = 0);
    ~AManageCabinetsDialog();
	void setCabinetList( const QStringList& irCabList );

    // AReturnRcvr Methods.
    virtual void returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue,const QString& irOut
                 , char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
                 = QString());
    virtual void setContextName(long iConnectId, const QString& irContextName);
    QString getStorageScope(const QString& irName);


public slots:
	void onClose();
	void onUpdate();

private:
	enum ACabinetTask {ecNone, ecUpdateStatus};
Ui::AManageCabinetsClass	cUi;
	AAppClient*				cpAppClient;
	QStringList				cCabinetList;
	QTreeWidgetItem*		cpUptodateListTree;
	QTreeWidgetItem*		cpImportListTree;
	QTreeWidgetItem*		cpExportListTree;
	QTreeWidgetItem*		cpMergeListTree;
	QTreeWidgetItem*		cpUnknownListTree;
	QIcon					cStatusUnknown;
	QIcon					cStatusUptodate;
	QIcon					cStatusImport;
	QIcon					cStatusExport;
	QIcon					cStatusMerge;
	QStringList				cForImportList;
	QStringList				cForExportList;
	QStringList				cForUpdateList;
	QStringList				cNameList;
	QStringList				cStorageScopeList;
	ACabinetTask			cTask;
};

#endif // AMANAGECABINETSDIALOG_H
