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

							Server Tree Model

CHANGE HISTORY
Version	Date		Who		Change
4.0003	 6/30/2008	fchua	Updated Context information.
4.0002   6/26/2008	fchua	[CR-114] Added column headers for session info details and displayed them accordingly.
3.2002	 2/11/2008	fchua	Modified closeAllConnections() to include case where client was not logged on.
3.2002	 2/04/2008	fchua	Modified returnOutput() to support connection cleanup.
3.2002	 2/04/2008	fchua	Added closeAllConnections().
3.2002	 2/01/2008	fchua	Fixed arrangement of functions in file.
3.2001	 1/27/2008	fchua	Fixed memory leak during object deallocation.
3.1006	12/21/2007	fchua	getSession. Added NULL pointer checking.
3.1006	12/21/2007	fchua	addSession. Added session information in session structure.
3.1004	11/12/2007	fchua	Modified returnOutput() to support re-opening of server form.
3.1004	11/ 2/2007	fchua	Modified getServer() to retrieve the correct server column.
3.1004	11/ 2/2007	fchua	Modified updateServer() to display the port number correctly.
3.1003	10/23/2007	fchua	Modified returnOutput() to handle socket disconnection.

*/
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------

#include <QtGui/QIcon>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QDir>

#include <stdlib.h>

#include "autilities.h"
#include "aservertreemodel.h"

AServerTreeModel::AServerTreeModel(QString iModelName):QStandardItemModel(0,8), cIsClosing(false)
{
	setObjectName(iModelName);
	setHeaderData(0, Qt::Horizontal, QVariant("Server"));
	setHeaderData(1, Qt::Horizontal, QVariant("Port"));
	setHeaderData(2, Qt::Horizontal, QVariant("Admin"));
	setHeaderData(3, Qt::Horizontal, QVariant("Owner"));
	setHeaderData(4, Qt::Horizontal, QVariant("State"));
	setHeaderData(5, Qt::Horizontal, QVariant("Protocol"));
	setHeaderData(6, Qt::Horizontal, QVariant("Location"));
	setHeaderData(7, Qt::Horizontal, QVariant(""));

	setSortRole(Qt::DisplayRole);

	// The configuration file will be loaded from following locations:
	// 1. Install/Binary Path
	// 2. $HOME/ais/servercfg.txt

	QFileInfo aFileInfo;
	QString aAisHomeDirStr;
	QDir aAisHomeDir;

	cServerConfigurationPath = gAis.mGblParams["gblaisinstallpath"] + AGLBLS_SERVERCFGFILE;
	aFileInfo.setFile(cServerConfigurationPath);
	if (!aFileInfo.isFile())
	{
	    aAisHomeDirStr = QDir::homePath() + QDir::separator() + AGLBLS_AISUSERDIR;
	    aAisHomeDir.setPath(aAisHomeDirStr);
	    if (!aAisHomeDir.exists())
	        aAisHomeDir.mkpath(aAisHomeDirStr);

	    // create the configuration file in the user's home directory
	    cServerConfigurationPath = aAisHomeDirStr + AGLBLS_SERVERCFGFILE;
	}

	populateServerIcons();
	loadServerList();
}

AServerTreeModel::~AServerTreeModel()
{
	saveServerList();
	// delete the allocated resources for the server and session
	QList<QModelIndex> aServerIndexes = getServers();
	for (int aRow = aServerIndexes.size() - 1; aRow >= 0; aRow--)
		removeServer(aServerIndexes[aRow]);
}

bool AServerTreeModel::addContext(const char* irServerName, QString iContextName, QString iContextInfo)
{
	QModelIndex aServerIndex;
	if (isServerExist(irServerName,aServerIndex))
	{
		return addContext(aServerIndex,iContextName,iContextInfo);
	}

	return false;
}

bool AServerTreeModel::addContext(QModelIndex& ioServerIndex, QString iContextName, QString iContextInfo)
{
	QModelIndex aContextIndex;
	if (ioServerIndex.isValid() && !isContextExist(ioServerIndex, iContextName,aContextIndex))
	{
		QStandardItem* apContextNameItem = new QStandardItem();
		QStandardItem* apContextInfoItem = new QStandardItem();
		QList<QStandardItem*> aContextRow;
		QStandardItem* apServerNameItem = itemFromIndex(ioServerIndex);

		apContextNameItem->setText(iContextName);
		apContextNameItem->setIcon(cServerIcons[CONTEXTON]);
		apContextNameItem->setRowCount(1);
		apContextNameItem->setColumnCount(1);
		aContextRow.append(apContextNameItem);

		aContextRow.append(new QStandardItem()); // port
		aContextRow.append(new QStandardItem()); // admin
		aContextRow.append(new QStandardItem()); // owner
		
		if (!iContextInfo.isEmpty() && iContextInfo == QString::fromAscii("Reg"))
			apContextInfoItem->setText("Registered");
		else
			apContextInfoItem->setText(iContextInfo);

		aContextRow.append(apContextInfoItem);
		apServerNameItem->appendRow(aContextRow);

		return true;
	}
	return false;
}

bool AServerTreeModel::addServer(const char* irServerName, const char* irHostDnsIp, ushort iPort)
{
	return addServer(irServerName, irHostDnsIp, iPort, new AAppClient(irHostDnsIp, iPort, this, this, irServerName));
}

/*!
\brief addServer - Add new server to the model

\par Args:
\param irServerName	Local server name
\param irHostDnsIp		Host DNS name or IP address (octet format)
\param iPort			Host listens on this port
\return bool -> true if successfully added, false otherwise
*/

bool AServerTreeModel::addServer(const char* irServerName, const char* irHostDnsIp, ushort iPort, AAppClient* irAppClient)
{
	// If the name is not already in the server list, add a new entry to list and to list view.
	QModelIndex aServerIndex;
	if (irServerName != NULL && !isServerExist(irServerName,aServerIndex))
	{
		AServerSt* apServerSt = new AServerSt;

		apServerSt->mpAppClient = irAppClient;
		apServerSt->mDefaultContextName = QString::null;
		apServerSt->mHostDnsIp = irHostDnsIp;
		apServerSt->mNumServerForms = 0;
		apServerSt->mPasswd = QString::null;
		apServerSt->mPort = iPort;
		apServerSt->mServerName = irServerName;
		apServerSt->mSysContextName = AGLBLS_SYSTEMCONTEXTNAME;
		apServerSt->mUsrId = 0;
		apServerSt->mUsrName = QString::null;

		// we need to catch any independent events generated by the AppClient
		// and update the model if necessary
		apServerSt->mpAppClient->setDefaultRcvr(this);

		// Append to the display
		QStandardItem* apServerNameColumnItem = new QStandardItem();
		QStandardItem* apPortColumnItem = new QStandardItem();
		QList<QStandardItem*> aServerRow;

		apServerNameColumnItem->setData(qVariantFromValue((void*)apServerSt),Qt::UserRole);
		apServerNameColumnItem->setText(apServerSt->mServerName);
		apServerNameColumnItem->setIcon(cServerIcons[SERVERON]);

		// force the display of the expand icon by creating a dummy row and column
		apServerNameColumnItem->setRowCount(1);
		apServerNameColumnItem->setColumnCount(1);

		apPortColumnItem->setText(QString("%1").arg(apServerSt->mPort));
		aServerRow.append(apServerNameColumnItem); // 1st Column
		aServerRow.append(apPortColumnItem); // 2nd Column
		appendRow(aServerRow);

		return true;
	}	

	return false;
}

bool AServerTreeModel::addSession(const char* irServerName, QString iContextName, QString iSessionId, QString iUsername, QString iSessionInfo)
{
	QModelIndex aContextIndex;
	if (isContextExist(irServerName,iContextName,aContextIndex))
	{
		return addSession(aContextIndex,iSessionId,iUsername,iSessionInfo);
	}
	return false;
}

bool AServerTreeModel::addSession(QModelIndex& ioContextIndex, QString iSessionId, QString iUsername, QString iSessionInfo)
{
	QModelIndex aSessionIndex;
	if (ioContextIndex.isValid() && !isSessionExist(ioContextIndex,iSessionId,aSessionIndex))
	{
		QStandardItem* apSessionIdItem = new QStandardItem();
		QStandardItem* apSessionPortItem = new QStandardItem();
		QStandardItem* apSessionAdminItem = new QStandardItem();
		QStandardItem* apSessionOwnerItem = new QStandardItem();
		QStandardItem* apSessionStateItem = new QStandardItem();
		QStandardItem* apSessionProtoItem = new QStandardItem();
		QStandardItem* apSessionLocItem = new QStandardItem();

		QList<QStandardItem*> aSessionRow;
		QStandardItem* apServerColumnItem = itemFromIndex(ioContextIndex);
		ASessionSt* apSessionSt = new ASessionSt;

		apSessionSt->mSessionId = iSessionId;
		apSessionSt->mUserName = iUsername;
		apSessionSt->mSessionInfo = iSessionInfo;

		apSessionIdItem->setText("Session " + iSessionId);
		apSessionIdItem->setData(qVariantFromValue((void*)apSessionSt),Qt::UserRole);
		apSessionIdItem->setIcon(cServerIcons[SESSIONOFF]);

		QStringList aSessionInfoDetails = iSessionInfo.split("\t");

		apSessionPortItem->setText("");
		if (aSessionInfoDetails[0].toLower() == "admin")
			apSessionAdminItem->setText("Yes");
		else
			apSessionAdminItem->setText("No");

		apSessionOwnerItem->setText(aSessionInfoDetails[1]); // owner
		apSessionStateItem->setText(aSessionInfoDetails[2]); // state
		apSessionProtoItem->setText(aSessionInfoDetails[3]); // protocol
		apSessionLocItem->setText(aSessionInfoDetails[4]);   // location

		aSessionRow.append(apSessionIdItem);
		aSessionRow.append(apSessionPortItem);
		aSessionRow.append(apSessionAdminItem);
		aSessionRow.append(apSessionOwnerItem);
		aSessionRow.append(apSessionStateItem);
		aSessionRow.append(apSessionProtoItem);
		aSessionRow.append(apSessionLocItem);

		apServerColumnItem->appendRow(aSessionRow);
		return true;
	}
	return false;
}

bool AServerTreeModel::addSession(QModelIndex& ioServerIndex, QString iContextName, QString iSessionId, QString iUsername, QString iSessionInfo)
{
	QModelIndex aContextIndex;
	if (isContextExist(ioServerIndex,iContextName,aContextIndex))
	{
		return addSession(aContextIndex,iSessionId,iUsername,iSessionInfo);
	}
	return false;
}

bool AServerTreeModel::clearContexts(const char* irServerName)
{
	QModelIndex aServerIndex;
	if (isServerExist(irServerName, aServerIndex))
	{
		return clearContexts(aServerIndex);
	}
	return false;
}

bool AServerTreeModel::clearContexts(QModelIndex& iServerIndex)
{
	if (iServerIndex.isValid())
	{
		QModelIndex aCurrentContextIndex;
		int aRowCount = rowCount(iServerIndex);

		for (int aRow=0; aRow < aRowCount; aRow++)
		{
			aCurrentContextIndex = iServerIndex.child(0,0);
			removeContext(aCurrentContextIndex);
		}

		return true;
	}
	return false;
}

bool AServerTreeModel::clearSessions(const char* irServerName, QString iContextName)
{
	QModelIndex aContextIndex;
	if (isContextExist(irServerName, iContextName, aContextIndex))
	{
		return clearSessions(aContextIndex);
	}
	return false;
}

bool AServerTreeModel::clearSessions(QModelIndex& iContextIndex)
{
	if (iContextIndex.isValid())
	{
		QModelIndex aCurrentSessionIndex;
		int aRowCount = rowCount(iContextIndex);

		for (int aRow=0; aRow < aRowCount; aRow++)
		{
			aCurrentSessionIndex = iContextIndex.child(0,0);
			removeSession(aCurrentSessionIndex);
		}

		return true;
	}
	return false;
}

bool AServerTreeModel::clearSessions(QModelIndex& iServerIndex, QString iContextName)
{
	QModelIndex aContextIndex;
	if (isContextExist(iServerIndex, iContextName, aContextIndex))
	{
		return clearSessions(aContextIndex);
	}
	return false;
}

void AServerTreeModel::closeAllConnections()
{
	// Get list of servers
	QList<QModelIndex> aServers = getServers();
	long aServerSz = aServers.size();
	AServerSt *apServerSt = NULL;
	AAppClient *apAppClient = NULL;
	bool aEmit = true;

	cIsClosing = true;

	for (long aRow = 0; aRow < aServerSz; ++aRow)
	{
		if ((apServerSt = getServer(aServers[aRow])) != NULL)
		{
			if ((apAppClient = apServerSt->mpAppClient) != NULL)
			{
				// if client is connected
				if (apAppClient->isConnected())
				{
					// if client is logged on
					if (apAppClient->getUsrId() != 0)
					{
						apAppClient->onLogoff(this);
						aEmit = false;
					}
					else
					{
						// client is not logged on
						apAppClient->closeConnection(this, 0, geDefault);
						aEmit = false;
					}
				}
			}
		}
	}

	if (aEmit)
	{
		emit allConnectionsClosed();
	}
}

// call back functions implemented from AReturnRcvr
// this is an active model, any changes to server, context and session
// will be automatically be reflected in this model through these
// call back functions.
bool AServerTreeModel::connectionClosed(long iConnectId)
{
    Q_UNUSED(iConnectId);

	return false;
}

AAppClient* AServerTreeModel::getAppClient(const char* irServerName)
{
	QModelIndex aServerIndex;
	if ((irServerName != NULL) && isServerExist(irServerName,aServerIndex))
	{
		return getAppClient(aServerIndex);
	}
	return NULL;
}

AAppClient* AServerTreeModel::getAppClient(const QModelIndex& iServerIndex)
{
	if (iServerIndex.isValid() && !iServerIndex.parent().isValid()) // make sure this is a server index
	{
		QStandardItem* apServerNameColumn = itemFromIndex(iServerIndex);
		AServerSt* apServerSt = (AServerSt*)apServerNameColumn->data(Qt::UserRole).value<void*>();
		return apServerSt->mpAppClient;
	}
	return NULL;
}

QModelIndex AServerTreeModel::getChildIndex(QString iMatchString, QModelIndex iParentIndex, int iMatchColumn)
{
	if (iParentIndex.isValid())
	{
		QString aDisplayedData;
		QModelIndex aCurrentChildIndex;

		for (int aRow = 0; aRow < rowCount(iParentIndex); aRow++)
		{
			aCurrentChildIndex = iParentIndex.child(aRow,iMatchColumn);
			aDisplayedData = aCurrentChildIndex.data().toString();

			if (aDisplayedData == iMatchString)
			{
				return aCurrentChildIndex;
			}
		}
	}

	return QModelIndex(); // return an invalid index
}

AServerSt* AServerTreeModel::getServer(const char* irServerName)
{
	QModelIndex aServerIndex;
	if (isServerExist(irServerName,aServerIndex))
	{
		return getServer(aServerIndex);
	}
	return NULL;
}

AServerSt* AServerTreeModel::getServer(const QModelIndex& iServerIndex)
{
	if (iServerIndex.isValid())
	{
		// The AServerSt object is stored in the 1st column only
		QStandardItem* apServerNameColumn = item(iServerIndex.row());
		QVariant aUserRoleData = apServerNameColumn->data(Qt::UserRole);

		if (aUserRoleData.isValid())
		{
			return (AServerSt*) aUserRoleData.value<void*>();	
		}
	}
	return NULL;
}

QList<QModelIndex> AServerTreeModel::getServers()
{
	QStandardItem* apRoot = invisibleRootItem();
	QModelIndex apRootIndex = apRoot->index();
	QList<QModelIndex> aServerIndexes;
	
	for (int aRow = 0; aRow < rowCount(apRootIndex); aRow++)
	{
		aServerIndexes.append(apRoot->child(aRow,0)->index());
	}

	return aServerIndexes;
}

ASessionSt* AServerTreeModel::getSession(const char* irServerName, QString iContextName, QString iSessionId)
{
	QModelIndex aSessionIndex;
	if (isSessionExist(irServerName, iContextName, iSessionId, aSessionIndex))
	{
		return getSession(aSessionIndex);
	}
	return NULL;
}

ASessionSt* AServerTreeModel::getSession(const QModelIndex& iSessionIndex)
{
	QModelIndex aContextIndex = iSessionIndex.parent();
	QModelIndex aServerIndex = aContextIndex.parent();
	if (aServerIndex.isValid() && aContextIndex.isValid() && iSessionIndex.isValid())
	{
		// The ASessionSt object is stored in the 1st column only
		QStandardItem* apSessionNameColumn = itemFromIndex(aContextIndex)->child(iSessionIndex.row());
		if (apSessionNameColumn != NULL)
		{
			QVariant aUserRoleData = apSessionNameColumn->data(Qt::UserRole);

			if (aUserRoleData.isValid())
			{
				return (ASessionSt*) aUserRoleData.value<void*>();
			}
		}
	}
	return NULL;
}

bool AServerTreeModel::isContextExist(const char* irServerName, QString iContextName, QModelIndex& oIndex)
{
	QModelIndex aServerIndex;
	if (isServerExist(irServerName, aServerIndex))
	{
		return isContextExist(aServerIndex, iContextName, oIndex);
	}
	return false;
}

bool AServerTreeModel::isContextExist(const QModelIndex& iServerIndex, QString iContextName, QModelIndex& oIndex)
{
	if (iServerIndex.isValid())
	{
		oIndex = getChildIndex(iContextName,iServerIndex);
		
		if (oIndex.isValid())
		{
			return true;;
		}
	}
	return false;
}

bool AServerTreeModel::isServerExist(const char* irServerName, QModelIndex& oIndex)
{
	if (irServerName != NULL)
	{
		QList<QStandardItem*> aServers = findItems(irServerName);
		QStandardItem* apCurrentNameColumn;
		QString aCurrentServerName;
		for (int aIndex = 0; aIndex < aServers.size(); aIndex++)
		{
			apCurrentNameColumn = aServers[aIndex];
			aCurrentServerName = apCurrentNameColumn->text();
			// return the first server that matches the server name
			if (aCurrentServerName == irServerName)
			{
				oIndex = apCurrentNameColumn->index();
				return true;
			}
		}
	}
	return false;
}

bool AServerTreeModel::isSessionExist(const char* irServerName, QString iContextName, QString iSessionId, QModelIndex& oIndex)
{
	QModelIndex aContextIndex;
	if (isContextExist(irServerName,iContextName,aContextIndex))
	{
		return isSessionExist(aContextIndex,iSessionId,oIndex);
	}
	return false;
}

bool AServerTreeModel::isSessionExist(const QModelIndex& iContextIndex, QString iSessionId, QModelIndex& oIndex)
{
	QModelIndex iServerIndex = iContextIndex.parent();
	if (iServerIndex.isValid() && iContextIndex.isValid())
	{
		QString aDisplayedSessionId("Session %1");

		aDisplayedSessionId.arg(iSessionId);
		oIndex = getChildIndex(aDisplayedSessionId,iContextIndex);

		if (oIndex.isValid())
		{
			return true;
		}
	}
	return false;
}

bool AServerTreeModel::isSessionExist(const QModelIndex& iServerIndex, QString iContextName, QString iSessionId, QModelIndex& oIndex)
{
	QModelIndex aContextIndex;
	if (isContextExist(iServerIndex, iContextName, aContextIndex))
	{
		return isSessionExist(aContextIndex, iSessionId, oIndex);
	}
	return false;
}

void AServerTreeModel::loadServerList()
{
	QByteArray aLine;	// One record from servercfg file.
	QStringList aFields;	// List of fields
	QFile  aF(cServerConfigurationPath);
	long aLen;
	QByteArray aCurrentServerName, aCurrentHostDnsIp;
	short aCurrentPort;
	QModelIndex aServerIndex;

	if (!aF.open(QIODevice::Text | QIODevice::ReadOnly))
	{
		return;
	}

	aLine.resize(2050);

	while ((aLen = aF.readLine(aLine.data(), 2048)) >= 0)
	{	// Skip blank lines and comments
		if (AUtil::isComment(aLine))
			continue;
		if (aLen > 0)
		{	aFields = QString(aLine).split("\t", QString::KeepEmptyParts);
			aCurrentServerName = aFields.takeFirst().toLatin1();
			aCurrentHostDnsIp = aFields.takeFirst().toLatin1();
			aCurrentPort = aFields.front().toUShort();

			// If this name is not already in the list, add a new entry.
			if (!isServerExist(aCurrentServerName,aServerIndex))
			{	
				addServer(aCurrentServerName,aCurrentHostDnsIp,aCurrentPort,
					new AAppClient(aCurrentHostDnsIp,aCurrentPort,this,this,aCurrentServerName));
			}
		}
	}

	aF.close();
}

void AServerTreeModel::populateServerIcons()
{
	QString parentDir(":images/");

	// maps to the server icon enum
	cServerIcons.append(QIcon(parentDir+"connectserveroff.png"));  // SERVEROFF = 0
	cServerIcons.append(QIcon(parentDir+"connectserveron.png"));   // SERVERON
	cServerIcons.append(QIcon(parentDir+"connectcontextoff.png")); // CONTEXTOFF
	cServerIcons.append(QIcon(parentDir+"connectcontexton.png"));  // CONTEXTON
	cServerIcons.append(QIcon(parentDir+"connectsessionoff.png")); // SESSIONOFF
	cServerIcons.append(QIcon(parentDir+"connectsessionon.png"));   // SESSIONON
}

bool AServerTreeModel::removeContext(const char* irServerName, QString iContextName)
{
	QModelIndex aContextIndex;
	if (isContextExist(irServerName,iContextName,aContextIndex))
	{
		return removeContext(aContextIndex);
	}
	return false;
}

bool AServerTreeModel::removeContext(QModelIndex& iContextIndex)
{
	QModelIndex aServerIndex = iContextIndex.parent();
	if (iContextIndex.isValid() && aServerIndex.isValid())
	{
		int aSessionSize = rowCount(iContextIndex);
		QModelIndex aCurrentSessionIndex;

		for (int aRow = 0; aRow < aSessionSize; aRow++)
		{
			aCurrentSessionIndex = iContextIndex.child(0,0);
			removeSession(aCurrentSessionIndex);
		}	

		removeRow(iContextIndex.row(),aServerIndex);

		return true;
	}
	return false;
}

bool AServerTreeModel::removeContext(QModelIndex& iServerIndex, QString iContextName)
{
	QModelIndex aContextIndex;
	if (isContextExist(iServerIndex,iContextName,aContextIndex))
	{
		return removeContext(aContextIndex);
	}
	return false;
}

bool AServerTreeModel::removeServer(const char* irServerName)
{
	QModelIndex aServerIndex;

	if ((irServerName != NULL) && isServerExist(irServerName,aServerIndex))
	{
		removeServer(aServerIndex);
	}
	return false;
}

bool AServerTreeModel::removeServer(QModelIndex& iServerIndex)
{
	if (iServerIndex.isValid() && !iServerIndex.parent().isValid())
	{
		int aContextSize = rowCount(iServerIndex);
		QModelIndex aCurrentContextIndex;
		AServerSt* aServerSt = getServer(iServerIndex);
	
		for (int aRow = 0; aRow < aContextSize; aRow++)
		{
			// we remove the 1st child aContextSize times
			aCurrentContextIndex = iServerIndex.child(0,0);
			removeContext(aCurrentContextIndex);
		}

		delete aServerSt->mpAppClient;
		delete aServerSt;
		removeRow(iServerIndex.row());

		return true;
	}
	return false;
}

bool AServerTreeModel::removeSession(const char* irServerName, QString iContextName, QString iSessionId)
{
	QModelIndex aSessionIndex;
	if (isSessionExist(irServerName,iContextName,iSessionId,aSessionIndex))
	{
		return removeSession(aSessionIndex);
	}
	return false;
}

bool AServerTreeModel::removeSession(QModelIndex& iContextIndex, QString iSessionId)
{
	QModelIndex aSessionIndex;
	if (isSessionExist(iContextIndex,iSessionId,aSessionIndex))
	{
		return removeSession(aSessionIndex);
	}
	return false;
}

bool AServerTreeModel::removeSession(QModelIndex& iServerIndex, QString iContextName, QString iSessionId)
{
	QModelIndex aSessionIndex;
	if (isSessionExist(iServerIndex,iContextName,iSessionId,aSessionIndex))
	{
		return removeSession(aSessionIndex);
	}
	return false;
}

bool AServerTreeModel::removeSession(QModelIndex& iSessionIndex)
{
	QModelIndex aContextIndex = iSessionIndex.parent();
	QModelIndex aServerIndex = aContextIndex.parent();
	if (aServerIndex.isValid() && aContextIndex.isValid() && iSessionIndex.isValid())
	{
		ASessionSt* apSessionSt = getSession(iSessionIndex);
		QStandardItem* aSessionIdColumnItem = itemFromIndex(iSessionIndex);

		aSessionIdColumnItem->setData(QVariant(),Qt::UserRole); // set data to an invalid qvariant
		removeRow(iSessionIndex.row(),aContextIndex);

		if (apSessionSt != NULL)
		{
			delete apSessionSt;
		}

		return true;
	}
	return false;
}

void AServerTreeModel::returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(iRqId);
    Q_UNUSED(iRetValue);
    Q_UNUSED(irOut);
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(irDisplay);
    Q_UNUSED(irError);
    Q_UNUSED(iClientData);

	switch (iStatus)
	{
		case AERR_LOSTCONNECTION: // socket disconnection
			{
				// find the server with the matching connection id
				QList<QModelIndex> aServers = getServers();
				long aServerSz = aServers.size();
				AServerSt *apServerSt = 0;
				AAppClient *apAppClient = 0;

				for (long aRow = 0; aRow < aServerSz; ++aRow)
				{
					if ((apServerSt = getServer(aServers[aRow])) != NULL && 
						(apAppClient = apServerSt->mpAppClient) != NULL)
					{
						if ((apServerSt->mServerName == AGLBLS_INPROCSVRNAME && iConnectId == 0) || 
							(apAppClient->isConnected() && apAppClient->getConnectionId() == iConnectId))
						{
							// reset user id associated to server
							// the next time AConnectMgr checks this value, it will attempt to reconnect to the AIS server
							apServerSt->mUsrId = 0;
							apServerSt->mNumServerForms--;
							break;
						}
					}
				}

				// if closing, check if all connections have been closed
				if (cIsClosing)
				{
					long aNumConnected = 0;
					for (long aRow = 0; aRow < aServerSz; ++aRow)
					{
						if ((apServerSt = getServer(aServers[aRow])) != NULL && 
							(apAppClient = apServerSt->mpAppClient) != NULL)
						{
							if (apAppClient->isConnected())
							{
								aNumConnected++;
							}
						}
					}

					// this is the last connection
					// Note: Connected flag is set after this function is called
					if (aNumConnected <= 1)
					{
						//qDebug("emit allConnectionsClosed signal 1");
						emit allConnectionsClosed();
					}
				}

				break;
			}
		default:
			switch (iReqType)
			{
				case geCloseConnection: //qDebug("close connection received!");
					break;
				case geCloseContext: //qDebug("close context received!");
					break;
				case geCloseSession: //qDebug("close session received!");
					break;
				case geConnectSession: //qDebug("connect session received!");
					break;
				case geOpenConnection: //qDebug("open connection received!");
					break;
				case geOpenContext: //qDebug("open context received!");
					break;
				case geOpenSession: //qDebug("open session received!");
					break;
				case geRegisterContext: //qDebug("register context received!");
					break;
				case geLogoff:
				{
					// find the server with the matching connection id
					QList<QModelIndex> aServers = getServers();
					long aServerSz = aServers.size();
					AServerSt *apServerSt = 0;
					AAppClient *apAppClient = 0;

					for (long aRow = 0; aRow < aServerSz; ++aRow)
					{
						if ((apServerSt = getServer(aServers[aRow])) != NULL && 
							(apAppClient = apServerSt->mpAppClient) != NULL)
						{
							if ((apServerSt->mServerName == AGLBLS_INPROCSVRNAME && iConnectId == 0) || 
								(apAppClient->isConnected() && apAppClient->getConnectionId() == iConnectId))
							{
								apAppClient->closeConnection(this, 0, geDefault);
								break;
							}
						}
					}
					break;
				}
				default: //qDebug("unknown regtype received! - %d",iReqType);
						break;
			}
	}
}

void AServerTreeModel::saveServerList()
{
	QByteArray aLine; // One record from servercfg file.
	QFile aF(cServerConfigurationPath);
	QList<QModelIndex> aServerIndexes = getServers();

	if (!aF.open(QIODevice::Text | QIODevice::WriteOnly)) // 16|2
	{
		return;
	}

	// Compose a header.
	const char* apHdr = "# asvrcfg.txt\t Generated file. All edits to this file may be lost!\n#ServerName\tIP Address\tPort\n";
	AServerSt* apServerSt;

	aF.write(apHdr, strlen(apHdr));
	// Format each record and write out line to servercfg file.

	for (int aRow = 0; aRow < aServerIndexes.size(); aRow++)
	{	
		apServerSt = getServer(aServerIndexes[aRow]);
		if (apServerSt->mServerName != AGLBLS_INPROCSVRNAME)
		{
			aLine = apServerSt->mServerName + '\t';
			aLine += apServerSt->mHostDnsIp + '\t';
			aLine += QByteArray::number(apServerSt->mPort) + '\n';
			aF.write((const char*)aLine, aLine.length());
		}
	}

	aF.close();
}

bool AServerTreeModel::setContextIcon(QModelIndex& iContextIndex, int iIconType)
{
	if (iContextIndex.isValid())
	{
		QStandardItem* apContextNameColumn = itemFromIndex(iContextIndex);

		apContextNameColumn->setIcon(cServerIcons[iIconType]);
	}
	return false;
}

bool AServerTreeModel::setContextIconOff(const char* irServerName, QString iContextName)
{
	QModelIndex aContextIndex;
	if (isContextExist(irServerName,iContextName,aContextIndex))
	{
		return setContextIconOff(aContextIndex);
	}
	return false;
}

bool AServerTreeModel::setContextIconOff(QModelIndex& iContextIndex)
{
	if (iContextIndex.isValid())
	{
		return setContextIcon(iContextIndex,CONTEXTOFF);
	}
	return false;
}

bool AServerTreeModel::setContextIconOff(QModelIndex& iServerIndex, QString iContextName)
{
	QModelIndex aContextIndex;
	if (isContextExist(iServerIndex,iContextName,aContextIndex))
	{
		return setContextIconOff(aContextIndex);
	}
	return false;
}

bool AServerTreeModel::setContextIconOn(const char* irServerName, QString iContextName)
{
	QModelIndex aContextIndex;
	if (isContextExist(irServerName,iContextName,aContextIndex))
	{
		return setContextIconOn(aContextIndex);
	}
	return false;
}

bool AServerTreeModel::setContextIconOn(QModelIndex& iContextIndex)
{
	if (iContextIndex.isValid())
	{
		return setContextIcon(iContextIndex,CONTEXTON);
	}
	return false;
}

bool AServerTreeModel::setContextIconOn(QModelIndex& iServerIndex, QString iContextName)
{
	QModelIndex aContextIndex;
	if (isContextExist(iServerIndex,iContextName,aContextIndex))
	{
		return setContextIconOn(aContextIndex);
	}
	return false;
}

void AServerTreeModel::setContextName(long iConnectId, const QString& irContextName)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(irContextName);
}

bool AServerTreeModel::setServerIcon(QModelIndex& iServerIndex, int iIconType)
{
	if (iServerIndex.isValid())
	{
		QStandardItem* apServerNameColumn = itemFromIndex(iServerIndex);

		apServerNameColumn->setIcon(cServerIcons[iIconType]);
	}
	return false;
}

bool AServerTreeModel::setServerIconOff(const char* irServerName)
{
	QModelIndex aServerIndex;
	if (isServerExist(irServerName,aServerIndex))
	{
		setServerIconOff(aServerIndex);
		return true;
	}
	return false;
}

bool AServerTreeModel::setServerIconOff(QModelIndex& iServerIndex)
{
	if (iServerIndex.isValid())
	{
		return setServerIcon(iServerIndex,SERVEROFF);
	}
	return false;
}

bool AServerTreeModel::setServerIconOn(const char* irServerName)
{
	QModelIndex aServerIndex;
	if (isServerExist(irServerName,aServerIndex))
	{
		setServerIconOn(aServerIndex);
		return true;
	}
	return false;
}

bool AServerTreeModel::setServerIconOn(QModelIndex& iServerIndex)
{
	if (iServerIndex.isValid())
	{
		setServerIcon(iServerIndex,SERVERON);
		return true;
	}
	return false;
}

bool AServerTreeModel::setSessionIcon(QModelIndex& iSessionIndex, int iIconType)
{
	if (iSessionIndex.isValid())
	{
		QStandardItem* apSessionNameColumn = itemFromIndex(iSessionIndex);

		apSessionNameColumn->setIcon(cServerIcons[iIconType]);
	}
	return false;
}

bool AServerTreeModel::setSessionIconOff(const char* irServerName, QString iContextName, QString iSessionId)
{
	QModelIndex aSessionIndex;
	if (isSessionExist(irServerName,iContextName,iSessionId,aSessionIndex))
	{
		return setSessionIconOff(aSessionIndex);
	}	
	return false;
}

bool AServerTreeModel::setSessionIconOff(QModelIndex& iServerIndex, QString iContextName, QString iSessionId)
{
	QModelIndex aSessionIndex;
	if (isSessionExist(iServerIndex,iContextName,iSessionId,aSessionIndex))
	{
		return setSessionIconOff(aSessionIndex);
	}
	return false;
}

bool AServerTreeModel::setSessionIconOff(QModelIndex& iSessionIndex)
{
	if (iSessionIndex.isValid())
	{
		setSessionIcon(iSessionIndex,SESSIONOFF);
	}
	return false;
}

bool AServerTreeModel::setSessionIconOn(const char* irServerName, QString iContextName, QString iSessionId)
{
	QModelIndex aSessionIndex;
	if (isSessionExist(irServerName,iContextName,iSessionId,aSessionIndex))
	{
		return setSessionIconOn(aSessionIndex);
	}
	return false;
}

bool AServerTreeModel::setSessionIconOn(QModelIndex& iServerIndex, QString iContextName, QString iSessionId)
{
	QModelIndex aSessionIndex;
	if (isSessionExist(iServerIndex,iContextName,iSessionId,aSessionIndex))
	{
		return setSessionIconOn(aSessionIndex);
	}
	return false;
}

bool AServerTreeModel::setSessionIconOn(QModelIndex& iSessionIndex)
{
	if (iSessionIndex.isValid())
	{
		return setSessionIcon(iSessionIndex,SESSIONON);
	}
	return false;
}

int AServerTreeModel::type() const
{
	return QStandardItem::UserType+1;
}

bool AServerTreeModel::updateServer(const char* irServerName, QByteArray iNewServerName, QByteArray iHostName, ushort iPort)
{
	QModelIndex aServerIndex;
	if ((irServerName != NULL) && isServerExist(irServerName,aServerIndex))
	{
		return updateServer(aServerIndex, iNewServerName, iHostName, iPort);
	}
	return false;
}

bool AServerTreeModel::updateServer(QModelIndex& iServerIndex, QByteArray iNewServerName, QByteArray iHostName, ushort iPort)
{
	if (iServerIndex.isValid())
	{
		QStandardItem* ServerNameColumn = itemFromIndex(iServerIndex); // 1st column
		QStandardItem* PortColumn = itemFromIndex(iServerIndex.sibling(iServerIndex.row(),1)); // 2nd column
		AServerSt* apServerSt = getServer(iServerIndex);	

		apServerSt->mServerName = iNewServerName;
		apServerSt->mHostDnsIp = iHostName;
		apServerSt->mPort = iPort;

		// the view will be automatically informed through the item change signal
		ServerNameColumn->setText(apServerSt->mServerName);
		PortColumn->setText(QString::number(apServerSt->mPort));

		return true;
	}
	return false;
}
