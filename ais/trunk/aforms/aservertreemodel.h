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
#ifndef ASERVERTREEMODEL_H
#define ASERVERTREEMODEL_H

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

														Server Tree Model

CHANGE HISTORY
Version	Date		Who		Change
3.2002	 2/04/2008	fchua	Added closeAllConnections() and allConnectionsClosed().
3.1006	12/21/2007	fchua	Added mSessionInfo data member in ASessionSt.
												--------------- --------------
*/
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------

#include <QtGui/QStandardItemModel>
#include <QtCore/QMetaType>
#include "appclient.h"
#include "ais.h"

#define MODIFIED_STATUS Qt::UserRole+2

// Session Status
typedef struct
{	QString			mSessionId;			// Session Id for this session
	QString			mUserName;			// UserName used to log in to this session
	QString			mSessionInfo;		// Additional information for this session
} ASessionSt;
Q_DECLARE_METATYPE(ASessionSt);

// Server Status - One for each server displayed
typedef struct
{	AAppClient*		mpAppClient;		// Ptr to server's AIS interface module
	QString			mDefaultContextName;// Name of the default context
	QByteArray		mHostDnsIp;			// DNS Name or IP address (192.168.38.16)
	long			mNumServerForms;	// Reference count of open server forms
	QString			mPasswd;			// Password of last successful logon to this server
	ushort			mPort;				// Server port for TCP/IP socket
	QByteArray		mServerName;		// Local server name
	QString			mSysContextName;	// System context - usually _SystemContext
	long			mUsrId;				// User logged on to this server
	QString			mUsrName;			// Name of last successful logon to this server
} AServerSt;
Q_DECLARE_METATYPE(AServerSt);

enum SERVERICONS
{	SERVEROFF = 0,
	SERVERON,
	CONTEXTOFF,
	CONTEXTON,
	SESSIONOFF,
	SESSIONON	
};

class AServerTreeModel : public QStandardItemModel, public AReturnRcvr
{
	Q_OBJECT

public:
	virtual void returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
				 , char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
				 = QString());
	virtual void setContextName(long iConnectId, const QString& irContextName);
        virtual int type () const;

	AServerTreeModel(QString iModelName="Server Tree Model");
	~AServerTreeModel();
	bool	addServer(const char* irServerName, const char* irHostDnsIp, ushort iPort);
	bool	addServer(const char* irServerName, const char* irHostDnsIp, ushort iPort, AAppClient* irAppClient);
	bool	addContext(const char* irServerName, QString iContextName, QString iContextInfo);
	bool	addContext(QModelIndex& iServerIndex, QString iContextName, QString iContextInfo);
	bool	addSession(const char* irServerName, QString iContextName, QString iSessionId, QString iUsername, QString iSessionInfo);
	bool	addSession(QModelIndex& iServerIndex, QString iContextName, QString iSessionId, QString iUsername, QString iSessionInfo);
	bool	addSession(QModelIndex& iContextIndex,QString iSessionId, QString iUsername, QString iSessionInfo);

	bool	updateServer(const char* irServerName, QByteArray iNewServerName, QByteArray iHostname, ushort iPort);
	bool	updateServer(QModelIndex& iServerIndex, QByteArray irNewServerName, QByteArray iHostName, ushort iPort);

	bool	clearContexts(const char* irServerName);
	bool	clearContexts(QModelIndex& iServerIndex);
	bool	clearSessions(const char* irServerName, QString iContextName);
	bool	clearSessions(QModelIndex& iServerIndex, QString iContextName);
	bool	clearSessions(QModelIndex& iContextIndex);

	bool	removeServer(const char* irServerName);
	bool	removeServer(QModelIndex& iServerIndex);
	bool	removeContext(const char* irServerName, QString iContextName);
	bool	removeContext(QModelIndex& iServerIndex, QString iContextName);
	bool	removeContext(QModelIndex& iContextIndex);
	bool	removeSession(const char* irServerName, QString iContextName, QString iSessionId);
	bool	removeSession(QModelIndex& iServerIndex, QString iContextName, QString iSessionId);
	bool	removeSession(QModelIndex& iContextIndex, QString iSessionId);
	bool	removeSession(QModelIndex& iSessionIndex);

	bool	setServerIconOn(const char* irServerName);
	bool	setServerIconOn(QModelIndex& iServerIndex);
	bool	setServerIconOff(const char* irServerName);
	bool	setServerIconOff(QModelIndex& iServerIndex);
	bool	setContextIconOn(const char* irServerName, QString iContextName);
	bool	setContextIconOn(QModelIndex& iServerIndex, QString iContextName);
	bool	setContextIconOn(QModelIndex& iContextIndex);
	bool	setContextIconOff(const char* irServerName, QString iContextName);
	bool	setContextIconOff(QModelIndex& iServerIndex, QString iContextName);
	bool	setContextIconOff(QModelIndex& iContextIndex);
	bool	setSessionIconOn(const char* irServerName, QString iContextName, QString iSessionId);
	bool	setSessionIconOn(QModelIndex& iServerIndex, QString iContextName, QString iSessionId);
	bool	setSessionIconOn(QModelIndex& iSessionIndex);
	bool	setSessionIconOff(const char* irServerName, QString iContextName, QString iSessionId);
	bool	setSessionIconOff(QModelIndex& iServerIndex, QString iContextName, QString iSessionId);
	bool	setSessionIconOff(QModelIndex& iSessionIndex);
	bool	setServerIcon(QModelIndex& iServerIndex,int iIconType);
	bool	setContextIcon(QModelIndex& iContextIndex, int iIconType);
	bool	setSessionIcon(QModelIndex& iSessionIndex, int iIconType);

	AAppClient*	getAppClient(const char* irServerName);
	AAppClient*	getAppClient(const QModelIndex& iIndex);
	AServerSt*	getServer(const char* irServerName);
	AServerSt*	getServer(const QModelIndex& iServerIndex);
	ASessionSt*	getSession(const char* irServerName, QString iContextName, QString iSessionId);
	ASessionSt*	getSession(const QModelIndex& iSessionIndex);
	QList<QModelIndex>	getServers();

	bool	isServerExist(const char* irServerName, QModelIndex& oIndex);
	bool	isContextExist(const char* irServerName, QString iContextName, QModelIndex& oIndex);
	bool	isContextExist(const QModelIndex& iServerIndex, QString iContextName, QModelIndex& oIndex);
	bool	isSessionExist(const char* irServerName, QString iContextName, QString iSessionId, QModelIndex& oIndex);
	bool	isSessionExist(const QModelIndex& iServerIndex, QString iContextName, QString iSessionId, QModelIndex& oIndex);
	bool	isSessionExist(const QModelIndex& iContextIndex, QString iSessionId, QModelIndex& oIndex);

	void	refresh();
	void	loadServerList();
	void	saveServerList();
	void	setServerConfigurationPath(QString iServerConfigurationPath){cServerConfigurationPath = iServerConfigurationPath;}
	QString	getServerConfigurationPath() {return cServerConfigurationPath;}

	void	closeAllConnections();

signals:
	void	allConnectionsClosed();

private:
	QList<QIcon> cServerIcons;
	QString cServerConfigurationPath;
	bool	cIsClosing;

	QModelIndex getChildIndex(QString iMatchString, QModelIndex iParentIndex, int iMatchColumn=0);
	void	populateServerIcons();
};


#endif
