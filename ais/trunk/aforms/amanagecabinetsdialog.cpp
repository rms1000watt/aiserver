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
aisdev/aforms/amanagecabinetsdialog.cpp
											Manage Cabinets Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0000	07/27/2008	rca		Cabinet settings dialog
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "amanagecabinetsdialog.h"
#include <QtGui/QMessageBox>
#include <iostream>
using namespace std;

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
\brief AManageCabinetsDialog - Constructor instantiates the GUI elements generated in ui_aaboutdialog.h
 
 \par Args:
 \param ipParent -> Parent widget that created this instance of the dialog.
 \param ipName -> Name assigned to this instance of the dialog
 \param iModal true iff this dialog is modal
 \param iFlgs Optional flags to set the disposition of this dialog.
 \par Returns:
 nothing
 */
AManageCabinetsDialog::AManageCabinetsDialog(AAppClient*ipAppClient, QWidget* ipParent, const char* ipName, bool iModal, Qt::WFlags iFlgs)
    : QDialog(ipParent, iFlgs), cpAppClient(ipAppClient),
	cStatusUnknown(":images/statusunknown.png"), cStatusUptodate(":images/statusuptodate.png"),
    cStatusImport(":images/statusimport.png"), cStatusExport(":images/statusexport.png"),
    cStatusMerge(":images/statusmerge.png")
{
    Q_UNUSED(iModal);

	cUi.setupUi(this);
	setObjectName(ipName);
	connect( cUi.cUpdateButton, SIGNAL( clicked() ), this, SLOT( onUpdate() ));
	connect( cUi.cCloseButton, SIGNAL( clicked() ), this, SLOT( onClose() ));
	cpUptodateListTree = new QTreeWidgetItem(cUi.cCabinetTree);
	cpUptodateListTree->setText( 0, "Current" );
	cpUptodateListTree->setIcon( 0, cStatusUptodate );
	cpImportListTree = new QTreeWidgetItem(cUi.cCabinetTree);
	cpImportListTree->setText( 0, "Import" );
	cpImportListTree->setIcon( 0, cStatusImport );
	cpExportListTree = new QTreeWidgetItem(cUi.cCabinetTree);
	cpExportListTree->setText( 0, "Export" );
	cpExportListTree->setIcon( 0, cStatusExport );
	cpMergeListTree = new QTreeWidgetItem(cUi.cCabinetTree);
	cpMergeListTree->setText( 0, "Merge" );
	cpMergeListTree->setIcon( 0, cStatusMerge );
	cpUnknownListTree = new QTreeWidgetItem(cUi.cCabinetTree);
	cpUnknownListTree->setText( 0, "Unknown" );
	cpUnknownListTree->setIcon( 0, cStatusUnknown );
}

/*!
\brief Destructor

\par Args:
nothing
\par Returns:
nothing
*/
AManageCabinetsDialog::~AManageCabinetsDialog()
{
	delete cpUptodateListTree;
	delete cpImportListTree;
	delete cpExportListTree;
	delete cpMergeListTree;
	delete cpUnknownListTree;
}

/*!
\brief onUpdate - Update the selected cabinets(Import or Export selected cabinets)

\par Args:
nothing
\par Returns:
nothing
*/
void AManageCabinetsDialog::onUpdate()
{
	QTreeWidgetItem* aItem;
	QList<QTreeWidgetItem*> aRemoveList;
	QString aLabel = "";

	int aCount = cpImportListTree->childCount();
	for( int ctr = 0; ctr < aCount; ++ctr )
	{
		aItem = cpImportListTree->child( ctr );
		if( aItem->checkState(0) == Qt::Checked )
		{
			cForImportList.append( (aItem->text(0)) );
			aRemoveList.append( aItem );
		}
	}

	aCount = aRemoveList.size();
	for( int ctr = 0; ctr < aCount; ++ctr )
	{
		aItem = aRemoveList[ctr];
		cpImportListTree->removeChild( aItem );
		delete aItem;
		aItem = NULL;
	}
	aRemoveList.clear();

	aCount = cpExportListTree->childCount();
	for( int ctr = 0; ctr < aCount; ++ctr )
	{
		aItem = cpExportListTree->child( ctr );
		if( aItem->checkState(0) == Qt::Checked )
		{
			cForExportList.append( (aItem->text(0)) );
			aRemoveList.append( aItem );
		}
	}

	aCount = aRemoveList.size();
	for( int ctr = 0; ctr < aCount; ++ctr )
	{
		aItem = aRemoveList[ctr];
		cpExportListTree->removeChild( aItem );
		delete aItem;
		aItem = NULL;
	}
	aRemoveList.clear();

	cpImportListTree->setText( 1, aLabel.sprintf("- %d Items", cpImportListTree->childCount()));
	cpExportListTree->setText( 1, aLabel.sprintf("- %d Items", cpExportListTree->childCount()));

	if(cForImportList.size() > 0)
	{
		QString aName = cForImportList.takeFirst();
		QString aLocation = "..Lambda location..";
		QString aStorageScope = "";
		cTask = ecUpdateStatus;
		cForUpdateList.append(aName);
		setEnabled(false);
        if( getStorageScope(aName)  == "file" )
		    cpAppClient->importCabinetRemote( this, aName, aLocation );
        else
		    cpAppClient->importCabinetRemoteDir( this, aName, aLocation );
		return;
	}
	else
	if(cForExportList.size() > 0)
	{
		QString aName = cForExportList.takeFirst();
		QString aLocation = "..Lambda location..";
		QString aStorageScope = "";
		cTask = ecUpdateStatus;
		cForUpdateList.append(aName);
		setEnabled(false);
        if( getStorageScope(aName)  == "file" )
		    cpAppClient->exportCabinetRemote( this, aName, aLocation );
		else
		    cpAppClient->exportCabinetRemote( this, aName, aLocation );
		return;
	}
}

/*!
\brief setCabinetList - Set Cabinet information list

\par Args:
nothing
\par Returns:
nothing
*/
void AManageCabinetsDialog::setCabinetList(const QStringList& irCabList)
{
	QTreeWidgetItem *aItem = NULL;
	QString aLabel;
    QStringList aResultList;
    QString aAction = "";
    QString aMessage = "";
    QString aImportSync = "";
    QString aExportSync = "";
    QString aName = "";
    QString aStorageScope = "";
	cCabinetList = irCabList;

	for( long ctr = 0; ctr < irCabList.size(); ++ctr )
	{
    	aResultList = irCabList[ctr].split('\t');
    	aAction = aResultList[0];
    	aMessage = aResultList[1];
    	aImportSync = aResultList[2];
    	aExportSync = aResultList[3];
    	aName = aResultList[4];
    	aStorageScope = aResultList[5];
        cNameList.append( aName );
        cStorageScopeList.append( aStorageScope );

		if( aAction == "nothing" )
		{
			aItem = new QTreeWidgetItem(cpUptodateListTree);
		}
		else if( aAction == "import" )
		{
			aItem = new QTreeWidgetItem(cpImportListTree);
        	aItem->setFlags( Qt::ItemIsUserCheckable | Qt::ItemIsSelectable | Qt::ItemIsEnabled );
        	aItem->setCheckState(0, Qt::Unchecked);
		}
		else if( aAction == "export" )
		{
			aItem = new QTreeWidgetItem(cpExportListTree);
        	aItem->setFlags( Qt::ItemIsUserCheckable | Qt::ItemIsSelectable | Qt::ItemIsEnabled );
        	aItem->setCheckState(0, Qt::Unchecked);
		}
		else if( aAction == "merge" )
		{
			aItem = new QTreeWidgetItem(cpMergeListTree);
        	aItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );
		}
		if( aAction == "unknown" )
		{
			aItem = new QTreeWidgetItem(cpUnknownListTree);
        	aItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );
		}
		aItem->setText( 0, aName );
	}
	cpUptodateListTree->setText( 1, aLabel.sprintf("- %d Items", cpUptodateListTree->childCount()));
	cpImportListTree->setText( 1, aLabel.sprintf("- %d Items", cpImportListTree->childCount()));
	cpExportListTree->setText( 1, aLabel.sprintf("- %d Items", cpExportListTree->childCount()));
	cpMergeListTree->setText( 1, aLabel.sprintf("- %d Items", cpMergeListTree->childCount()));
	cpUnknownListTree->setText( 1, aLabel.sprintf("- %d Items", cpUnknownListTree->childCount()));
	cUi.cCabinetTree->expandAll();
	cUi.cCabinetTree->resizeColumnToContents( 0 );	
	cUi.cCabinetTree->resizeColumnToContents( 1 );	
}

/*!
\brief onClose - Slot for Closing the window

\par Args:
nothing
\par Returns:
nothing
*/
void AManageCabinetsDialog::onClose()
{
	QList<QTreeWidgetItem*> aItemList;

	aItemList = cpUptodateListTree->takeChildren();
	for( int ctr = 0; ctr < aItemList.size(); ++ctr )
	{
		delete aItemList[ctr];
	}

	aItemList = cpImportListTree->takeChildren();
	for( int ctr = 0; ctr < aItemList.size(); ++ctr )
	{
		delete aItemList[ctr];
	}

	aItemList = cpExportListTree->takeChildren();
	for( int ctr = 0; ctr < aItemList.size(); ++ctr )
	{
		delete aItemList[ctr];
	}

	aItemList = cpMergeListTree->takeChildren();
	for( int ctr = 0; ctr < aItemList.size(); ++ctr )
	{
		delete aItemList[ctr];
	}

	aItemList = cpUnknownListTree->takeChildren();
	for( int ctr = 0; ctr < aItemList.size(); ++ctr )
	{
		delete aItemList[ctr];
	}

	close();
}

/*!
 * \brief Receives payload returned from the server.
 *
 * \param[in] iConnectId A dummy placeholder (not used by AIS clients)
 * \param[in] iRqId Request ID assigned to requests from this client
 * \param[in] iStatus A positive error code (zero iff no error)
 * \param[in] iReqType Indicates the type of response being returned
 * \param[in] iRetValue Integer value returned by some types of responses, else zero
 * \param[in] irOut Message returned by this response
 * \param[in] ipData Binary buffer containing returned object closure (not used here)
 * \param[in] iDataSize Binary buffer size (not used here)
 * \param[in] irDisplay Console output returned by this response
 * \param[in] irError Error message if any
 * \param[in] iClientData Arbitrary string attached to the request and returned in the response
 *
 * \note
 * -# iRqId is zero for pushed output.
 * -# All of the expected types of returned output is listed in the method
 */
void AManageCabinetsDialog::returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(iRqId);
    Q_UNUSED(iStatus);
    Q_UNUSED(iRetValue);
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(irDisplay);
    Q_UNUSED(irError);
    Q_UNUSED(iClientData);

	switch (iReqType)
	{
	case geCheckCabinet:
	{
		QTreeWidgetItem *aItem = NULL;
		QStringList aResultList = irOut.split('\t');
		QString aLabel = "";
		QString aAction = "";
		QString aMessage = "";
		QString aImportSync = "";
		QString aExportSync = "";
		QString aName = "";
		QString aStorageScope = "";
		QModelIndex aIdx;
		setEnabled(true);
		// Normal Results for a browseLib.checkStatus call
		if( aResultList.size() == 6 )
		{
     		aAction = aResultList[0];
			aMessage = aResultList[1];
			aImportSync = aResultList[2];
			aExportSync = aResultList[3];
			aName = aResultList[4];
			aStorageScope = aResultList[5];
			if( aAction == "nothing" )
			{
				aItem = new QTreeWidgetItem(cpUptodateListTree);
				cpUptodateListTree->setText( 1, aLabel.sprintf("- %d Items", cpUptodateListTree->childCount()));
			}
			else if( aAction == "import" )
			{
				aItem = new QTreeWidgetItem(cpImportListTree);
        		aItem->setFlags( Qt::ItemIsUserCheckable | Qt::ItemIsSelectable | Qt::ItemIsEnabled );
        		aItem->setCheckState(0, Qt::Unchecked);
				cpImportListTree->setText( 1, aLabel.sprintf("- %d Items", cpImportListTree->childCount()));
			}
			else if( aAction == "export" )
			{
				aItem = new QTreeWidgetItem(cpExportListTree);
        		aItem->setFlags( Qt::ItemIsUserCheckable | Qt::ItemIsSelectable | Qt::ItemIsEnabled );
        		aItem->setCheckState(0, Qt::Unchecked);
				cpExportListTree->setText( 1, aLabel.sprintf("- %d Items", cpExportListTree->childCount()));
			}
			else if( aAction == "merge" )
			{
				aItem = new QTreeWidgetItem(cpMergeListTree);
        		aItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );
				cpMergeListTree->setText( 1, aLabel.sprintf("- %d Items", cpMergeListTree->childCount()));
			}
			if( aAction == "unknown" )
			{
				aItem = new QTreeWidgetItem(cpUnknownListTree);
        		aItem->setFlags( Qt::ItemIsSelectable | Qt::ItemIsEnabled );
				cpUnknownListTree->setText( 1, aLabel.sprintf("- %d Items", cpUnknownListTree->childCount()));
			}
			aItem->setText( 0, aName );
			if( cForUpdateList.size() > 0 )
			{
				setEnabled(false);
				cTask = ecUpdateStatus;
				cpAppClient->checkCabinet( this, cForUpdateList.takeFirst() );
			}
			else
			{
				cTask = ecNone;
			}
		}
	}
	break;
	case geExportCabinet:
	case geImportCabinet:
	setEnabled(true);
	if(cForImportList.size() > 0)
	{
		QString aName = cForImportList.takeFirst();
		QString aLocation = "..Lambda location..";
		cTask = ecUpdateStatus;
		cForUpdateList.append(aName);
		setEnabled(false);
        if( getStorageScope(aName)  == "file" )
			cpAppClient->importCabinetRemote( this, aName, aLocation );
		else
			cpAppClient->importCabinetRemoteDir( this, aName, aLocation );
		return;
	}
	if(cForExportList.size() > 0)
	{
		QString aName = cForExportList.takeFirst();
		QString aLocation = "..Lambda location..";
		cTask = ecUpdateStatus;
		cForUpdateList.append(aName);
		setEnabled(false);
        if( getStorageScope(aName)  == "file" )
		    cpAppClient->exportCabinetRemote( this, aName, aLocation );
		else
		    cpAppClient->exportCabinetRemoteDir( this, aName, aLocation );
		return;
	}
	if( cForUpdateList.size() > 0 )
	{
		setEnabled(false);
		cTask = ecUpdateStatus;
		cpAppClient->checkCabinet( this, cForUpdateList.takeFirst() );
		return;
	}
	break;
	default:
		setEnabled(true);
        cTask = ecNone;
		break;
	}
}

/*!
 * \brief Inherited from the AReturnRcvr abstract base class.
 *
 * \param[in] iConnectId - The ID of the connection for this context.
 * \param[in] irContextName - The new context name.
 */
void AManageCabinetsDialog::setContextName(long iConnectId, const QString& irContextName)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(irContextName);
}

/*!
 * \brief Inherited from the AReturnRcvr abstract base class.
 *
 * \param[in] iConnectId - The ID of the connection for this context.
 * \param[in] irContextName - The new context name.
 */
QString AManageCabinetsDialog::getStorageScope(const QString& irName)
{
	for( int n = 0; n < cNameList.size(); ++n )
	{
		if( irName == cNameList[n] )
			return cStorageScopeList[n];
	}
	return "";
}

// end
