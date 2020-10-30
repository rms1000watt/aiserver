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
aisdev/aissvr/aissvr.cpp
														AIS SERVER

CHANGE HISTORY
Version	Date		Who		Change
4.002	 9/28/2008	tlw		CR128. Add subscribe to allow a connection subscribe to a session.
4.0003   7/01/2008	fchua	[CR-118] Added new field (In-Process) in GetSessions response.
3.2005	 2/18/2008	fchua	Added getConnectionStats, getLogonStats, getRequestStats, getSessionStats.
3.2002	 2/04/2008	fchua	Added logging feature for user logon/logoff events.
3.2002	 2/04/2008	fchua	Modified logoff. Updated message parameter.
3.2001	 1/27/2008	fchua	Added support for User Management functions.
3.1006	12/21/2007	fchua	onCloseConnection. Added cleanup in-case session was closed first.
3.1006	12/21/2007	fchua	closeSession. Added check to make sure connection was explicitly closed.
3.1005	12/16/2007	fchua	Modified submit. Updated geConnectSession case.
3.1005	12/16/2007	fchua	Modified returnOutput. Added check if connection is open.
3.1005	12/16/2007	fchua	Modified returnMsg. Added check if connection is open.
3.1005	12/16/2007	fchua	Modified onCloseConnection. Fixed handling of disconnection.
3.1005	12/16/2007	fchua	Modified connectSession. Fixed implementation.
3.1005	12/16/2007	fchua	Modified closeSession. Fixed cleanup implementation.
3.1004	11/13/2007	fchua	Modified returnOutput. Fixed formatting of console log (line breaks).
3.1004	11/13/2007	fchua	Modified enableConsoleLog. Fixed handling of reset and close console log.
3.1003	10/29/2007	fchua	Fixed logSysMsg documentation.
2.0001	12/29/2006	tmay	added geExecute
1.0120	12/19/2006	tlw		returnOutput, returnMsg. Add ipData and iDataSize arguments.
1.0118	12/12/2006	tlw		submit. Add serial binary stream argument.
1.0116	12/4/2006	tlw		onQuit. Add slot to handle shutdown of contexts.
1.0115	11/23/2006	tlw		returnOutput. Log all console output using LOGCONSOLEMSG. timerEvent. Update documentation.
1.0113	11/13/2006	tlw		~AAisSvr. Simplify destruction of cOpenPorts (does not contain heap objects).
1.0067	7/22/2005	tlw		onCloseConnection. Call setLogLvl to cancel logging subscriptions.
1.0062	5/8/2005	tlw		openConsoleLog. Add clear,record-oriented storage option.
1.0059	4/22/2005	tlw		Remove disconnectSession. Modify needsSession to list requests that need a session.
1.0057	3/18/2005	tlw		Add documentation
												---------------------------------
DOCUMENTATION
1.	See aissvrnotes.txt for project setup information.
2.	See include/aissvr.h for specifications.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <time.h>				// time
#include <QtCore/QtDebug>
#include <QtCore/QFileInfo>
#include <QtCore/QRegExp>
#include <QtCore/QTimer>

#include "ahttpsvr.h"
#include "aglobals.h"			// gAis
#include "aissvr.h"				// AAisSvr
#include "aisoutput.h"			// AISMGROUT_SIZE
#include "asessionmgrinterface.h"	// ASessionMgrInterface
#include "aismgr.h"				// AAisMgr
#include "appsvr.h"				// AAppSvr
#include "autilities.h"			// AUtil
#include "axmlsvr.h"			// AXmlSvr

//	---------------------------------------------------- ACONNECTION ----------------------------------------------------------
AConnection::AConnection(QObject* ipParent, const char* ipName)
	: QObject(ipParent)
{
	setObjectName(ipName);
	mConnectId = -1;			// Index into cOpenConnections, -1 if unused.
	mComment = QString::null;	// Comment returned from password file
	mpConsoleLog = NULL;		// Holds console logging state, if console log opened
	mContextName = "";			// Context for this connection.
	mDefaultCloseWait = 0;		// Wait-to-close period if close called with default CloseWait
	mEndDay = 0;				// User's expiration date (days since 1/1/2000)
	mIdleSecs = 0;				// Maximum time between requests on a connection
	mLastAccess = 0;			// Time (secs since 2K) of last access
	mMaxEntries = 0;			// Maximum number of entries
	mMode = geOpen;				// Connection active
	mProtocolId = -1;			// Type of protocol AISSVR_APP, AISSVR_HTTP, AISSVR_XML
	mpProtocolRcvr = NULL;		// -> Protocol server (null if disconnected)
	mSecLvl = 0;				// Security level of the current user
	mSessionId = -1;			// Current session for this user.
	mSysEndDay = 0;				// Usr's system-wide expiration date
	mSysSecLvl = 0;				// Usr's system-wide security level
	mpTimer = NULL;				// Timer to wait until close timeout period expires.
	mUsrId = -1;				// Index into user's account (in UsrMgr).
}

/*!
\brief Destructor Free allocated resources

\par Notes:
-# AConnection has remote ownership of AConsoleLogST, QTimer, and AReturnRcvr referents.
-# Since instances of AConnection are never assigned, copied or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
AConnection::~AConnection()
{
	if (mpConsoleLog != NULL)
		delete mpConsoleLog;
	// PENDING - Need way to capture original allocated address. Maybe use void*
	// if (mpProtocolRcvr != NULL)
	//	delete mpProtocolRcvr;
	// Parent of mpTimer is AConnection, no need for explicit deletion
	//if (mpTimer != NULL)
	//{	mpTimer->stop();		// Stop timer used to abort waiting for a connection to close.
	//	delete mpTimer;
	//}
#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AConnection()");
#endif
}

void AConnection::onTimeout()
{
	emit closeConnection(mConnectId);
}
//	--------------------------------------------------- PUBLIC METHODS --------------------------------------------------------
/*!
\brief AAisSvr - AIS Server is a repository for the common portion of the protocol servers.

Aissvr unwraps the incoming message to the server and then handles _ais requests or calls the appropriate aismgr
function for requests that are handled by the engine.
\param ipAisMgr -> The instance of the AIS manager for this server. Used for forwarding requests to the engine.
\param ipSessionMgr -> The instance of the ASessionManager for this server.
\param ipUsrMgr -> The instance of the AUsrMgr which validates system-wide logon requests.
 */
AAisSvr::AAisSvr(AAisMgr* ipAisMgr, ASessionManager* ipSessionMgr, AUsrMgr* ipUsrMgr, bool iServiceMode)
	: QObject(NULL), cpAisMgr(ipAisMgr), cpSessionMgr(ipSessionMgr), cMt(""), cpUsrMgr(ipUsrMgr)
{
	_trace = false;
	cServiceMode = iServiceMode;
	mRqId = 0;			// Unique ID every incoming message. Incremented on every incoming message.
	cpFileWatcher = new AFileWatcher( ipSessionMgr );

	// cOpenConnections. Create first null entry.
	AConnection* apConnection = new AConnection(this, "Null Connection");
	cOpenConnections.insert(0, apConnection);
	cOpenPorts.clear();
	cY2K = QDate(2000, 1, 1);
	
	// Start up an interval timer for flushing console output buffer. See also timerEvent()
	cFlushTimer.start(AISSVR_FLUSHMSEC, this);
}

/*!
\brief Destructor Free allocated resources

\par Notes:
-# AAisSvr has remote ownership of AConnection referents in cOpenConnections.
-# Since instances of AAisSvr are never assigned, copied, or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
AAisSvr::~AAisSvr()
{
	// cOpenConnections.  Free all instances of AConnection and QTimer
	long aConnection, aSz = cOpenConnections.size();
	for (aConnection = 0; aConnection < aSz; ++aConnection)
		delete cOpenConnections[aConnection];

	cFlushTimer.stop();
	cOpenPorts.clear();
	cOpenConnections.clear();
	delete cpFileWatcher;

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AAisSvr()");
#endif
}

/*!
 * \brief Add new user to the system.
 *
 * \param[in] iConnectId Connection Id of the client that issued the command.
 * \param[in] irAmpmsg Original AMP message containing the message body.
 */
long AAisSvr::addUser(long iConnectId, const QString& irAmpmsg)
{
	AConnection* apConnection = cOpenConnections[iConnectId];
	QString aError;
	long aRet = 0;

	// Check User Permission
	if (apConnection->mSecLvl < geSuper)
	{
		aError = "0,AAisSvr.addUser, No permission to add user";
		aRet = AERR_ACCESS;
	}
	else
	{
		QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
		long aTknSz = (long)aTkns.size();
		long aParamIdx = 0;
		QString aUsername;
		QString aPassword;
		ASecLvl aSecurityLevel = geNada;
		QDate aEndDate;
		QString aComment;
		QString aTemp;

		for (long aIdx = 2; aIdx < aTknSz; aIdx+=2)
		{
			aParamIdx = aIdx + 1;
			if (aParamIdx < aTknSz)
			{
				aTemp = aTkns[aIdx];
				if (aTemp == "username")
					aUsername = aTkns[aParamIdx];
				else if (aTemp == "password")
					aPassword = aTkns[aParamIdx];
				else if (aTemp == "securitylevel")
					aSecurityLevel = static_cast<ASecLvl>(aTkns[aParamIdx].toLong());
				else if (aTemp == "enddate")
					aEndDate = QDate::fromString(aTkns[aParamIdx],"MM/dd/yyyy");
				else if (aTemp == "comment")
					aComment = aTkns[aParamIdx];
				else
				{
					aError = QString("0,AAisSvr.addUser, Unknown Parameter %1").arg(aTemp);
					LOGSYSMSG(geWarn, aError);
					aError.clear();
				}
			}
		}
		if (aUsername.isEmpty() || aPassword.isEmpty())
		{
			aError = "0,AAisSvr.addUser, One of the required parameters is empty";
			aRet = AERR_FEWARGS;
		}
		else
			aRet = cpUsrMgr->addUser(aSecurityLevel, aUsername, aPassword, aEndDate, aComment);
	}
	if (!aError.isEmpty())
		LOGSYSMSG(geWarn, aError);
	return aRet;
}

/*	---------------------------------------------------------------------------------------------------------------------------
ccSubscribers - Carbon copy to subscribers on the list.
 Args:
	iConnectId		Index into cOpenConnections array
	irDisplay		Console output to be copied
	irSubscribers	ConnectIds of subscribers
Returns:
		nothing
PENDING	AAisSvr::ccSubscribers(). If subscriber is disconnected, log this output. Subscriber should be able to disconnect, reconnect
and get exactly the same console output as if subscriber were connected the entire time.
Note:
 1.	irSubscribers is a sorted list of subscribers. Ignore duplicates on the list.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisSvr::ccSubscribers(long iSessionId, const QString& irDisplay)
{
	long aSz, aIx;
	AMemberList aSubscribers = getSubscribers(iSessionId);
	aSz = aSubscribers.count();
	for (aIx = 0; aIx < aSz; ++aIx)
		returnMsg(aSubscribers[aIx], 0/*RqId*/, 0/*Status*/, geLogConsole, 0/*Value*/, irDisplay, NULL, 0, cMt, cMt);
}

/*!
\brief closeConnection - called from submit and from protocol servers on an unexpected close.

\param iConnectId - Index into cOpenConnections array for the connection that was closed.
\param iCloseMode - Close mode (geDefault, geOpen, geDisconnect, geSoft, geClear, geStop)
\param iWait - Close connection timeout (-2 default, -1 forever, 0 no wait, n wait n seconds)
\return void
\par Notes:
-# If iWait == -2, set iWait to the default timeout for this connection.
-# If iWait == -1, close connection, but do not close session
-# If iWait >= 0, wait for iWait seconds before closing connection and closing session
-# If any request is received before timeout, reset timer and do not close connection.
-# No result is returned.  It is up to the caller to return a result, especially if caller is not closing itself.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisSvr::closeConnection(long iConnectId, ACloseMode iCloseMode, long iWait)
{
	long aSz = cOpenConnections.size();
	AConnection* apConnection;
	if (iCloseMode != geOpen && iConnectId > 0 && iConnectId < aSz && (apConnection = cOpenConnections[iConnectId]) != NULL)
	{	// Default Close Wait. Set the default wait value
		if (iWait <= -2)
			iWait = apConnection->mDefaultCloseWait;
			
		// CloseMode. Note the close mode.
		if (iWait >= 0)
			apConnection->mMode = iCloseMode;

		// Timer. If close is postponed until later, set the timer to call doCloseConnection
		if (iWait > 0)
		{	QTimer* apTimer = apConnection->mpTimer;
			if (apTimer == NULL)
			{	apConnection->mpTimer = apTimer = new QTimer(apConnection);
				apTimer->setObjectName("CloseConnection");
				connect(apTimer, SIGNAL(timeout()), apConnection, SLOT(onTimeout()));
				connect(apConnection, SIGNAL(closeConnection(long)), this, SLOT(onCloseConnection(long)));
			}
			apTimer->setSingleShot(true /*singleShot*/);
			apTimer->start(iWait * 1000);
		}
		else if (iWait <= 0) // Close Connection.  Call onCloseConnection directly.
			onCloseConnection(iConnectId);
	}
}
/*	---------------------------------------------------------------------------------------------------------------------------
closeContext - Clean up after closing up the context.
Args:
	iContextId	Index into SMgr.cOpenContexts array for this context
Returns:
	nothing
PENDING AAisSvr::closeContext(). Call closePort to close any context-specific ports.
Notes:
 1.	AMP format:  _ais|closecontext|context|%s|closemode|%s
 2. Warning! Do not increment apIt after erasing the last entry. A crash is waiting.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisSvr::closeContext(long iContextId)
{
	if (iContextId > 0)
	{	iContextId <<= AISSVR_SHIFT;
		ASubscriptionMap& arSubscription = cSubscriptions[RULES];
		ASubscriptionMap::Iterator apIt = arSubscription.begin();		// STL-style iterator
		if (apIt != arSubscription.end())
		{	for (;;)
			{	if ((apIt.key() & ~AISSVR_MASK) == iContextId)
					arSubscription.erase(apIt);
				else if (apIt != arSubscription.end())
					++apIt;
				else
					break;
			}
		}
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
closePort - Close an opened port.
Args:
	iPort	Index into cOpenPorts array
Returns:
	nothing
Note:
 1.	Unsubscribe from all rules matching this protocol. Delete the port.
	Note that a client can have a global and zero or more context-specific ports, with the same protocol.
	------------------------------------------------------------------------------------------------------------------------ */
bool AAisSvr::closePort(ushort iPort)
{
	// For each client, unsubscribe from those rules that include this protocol.
	bool aClosed = true;
	if (iPort > 0 && cOpenPorts.contains(iPort))
	{	long aProtocolId = cOpenPorts[iPort].mProtocolId;
		ASubscriptionMap& arSubscription = cSubscriptions[RULES];
		ASubscriptionMap::Iterator apIt;		// STL-style iterator
		ASubscriptionMap::Iterator apEnd = arSubscription.end();
		while (apIt != apEnd)
		{	if ((apIt.key() & AISSVR_MASK) == aProtocolId)
				arSubscription.erase(apIt);
			// Warning! Do not increment apIt after erasing the last entry. A crash is waiting.
			else if (apIt != apEnd)
				++apIt;
		}
		delete cOpenPorts[iPort].mpProtocolSvr;
	}
	else // Nothing to close
	{	qDebug("AAisSvr.closePort(Port:%d), Port is not open.", iPort);
		aClosed = false;
	}
	return aClosed;
}

/*	---------------------------------------------------------------------------------------------------------------------------
closeSession - Mop up after a session is closed
Args:
	iSessionId	Index into SMgr.cSessions array for this session
Returns:
	aStatus			Currently returns 0
Notes:
 1.	AMP format:  _ais|closesession|sessionid|%d|closemode|%s
	------------------------------------------------------------------------------------------------------------------------ */
long AAisSvr::closeSession(long iSessionId)
{
	// Session List. Unsubscribe all the Subscribers to this session.
	removeSubscriptions(SESSIONS, iSessionId);

	// Exclusion List. Unsubscribe all Subscribers that excluded this session
	removeSubscriptions(EXCLUSIONS, iSessionId);

	// Close Session.  Mop up
	if (iSessionId > 0 && cSessionMap.contains(iSessionId))
	{
		// Do not delete session map entry if it is just disconnected
		if (!cpSessionMgr->isDisconnected(iSessionId))
		{
			long aConnectId = cSessionMap[iSessionId];
			AConnection* apConnection = cOpenConnections[aConnectId];
			cSessionMap.remove(iSessionId);

			// If connection is still associated with a session (!= 0)
			if (aConnectId > 0 && apConnection != NULL)
			{
				apConnection->mSessionId = 0;

				// If the connection was explicitly closed
				if (apConnection->mMode != geOpen)
				{
					// free the connection
					apConnection->mConnectId = 0;
					if (apConnection->mpConsoleLog != NULL)
					{
						delete apConnection->mpConsoleLog;
						apConnection->mpConsoleLog = NULL;
					}

					apConnection->mComment = QString::null;
					apConnection->mContextName = QString::null;
					apConnection->mDefaultCloseWait = 0;
					apConnection->mEndDay = 0;
					apConnection->mIdleSecs = 0;
					apConnection->mLastAccess = 0;
					apConnection->mMode = geDefault;
					apConnection->mProtocolId = -1;
					apConnection->mpProtocolRcvr = NULL;
					apConnection->mSecLvl = -1;
					apConnection->mSysEndDay = 0;
					apConnection->mSysSecLvl = -1;
					apConnection->mpTimer = NULL;
				}
			}
		}
	}
	return 0;
}

/*	---------------------------------------------------------------------------------------------------------------------------
connectSession - Reconnect to a disconnected session.
Args:
	iConnectId	Index into cOpenConnections array
	iRqId		Unique ID assigned to each message by each client
	irAmpmsg	_ais|connectsession|sessionid|%d
Returns:
	aStatus		Error code if connect fails; else, 0
Notes:
 1.	This request requires the session ID
	------------------------------------------------------------------------------------------------------------------------ */
long AAisSvr::connectSession(long iConnectId, long iRqId, const QString& irAmpmsg, long& orNewSessionId)
{
    Q_UNUSED(iRqId);

	QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
	long aStatus = 0;
#ifdef ACHECK_DEBUG
    long aSz = cOpenConnections.size(), aTknSz = aTkns.size();
#endif
	long aEndDay,  aIdleSecs = 0, aSessionId = aTkns[3].toLong(), aMinSecLvl = 0, aSecLvl;
	ACHECK(iConnectId > 0 && iConnectId < aSz && aTknSz == 4 && aSessionId > 0);

	// MinSecurityLevel. Check current user security level against MinSecLvl
	AConnection* apConnection = cOpenConnections[iConnectId];
	long aUsrId = apConnection->mUsrId;
	long aOldId = apConnection->mSessionId;
	long aContextId = cpSessionMgr->getContextId(aSessionId);
	const QString& arContextName = cpSessionMgr->getContextName(aSessionId);
	QString aError, aOut;
	cpSessionMgr->getUsrSettings(arContextName, aUsrId, aSecLvl, aEndDay);
	AStringMap* apContextParams = cpSessionMgr->getContextParams(aContextId);
	if (apContextParams != NULL)
	{	aMinSecLvl = (*apContextParams)["minsecuritylevel"].toLong();
		aIdleSecs = (*apContextParams)["usridlesecs"].toLong();
	}
	if (apContextParams == NULL)
		aStatus = AERR_UNKCONTEXTNAME;

	// Make sure we are not connecting to the same session
	else if (aOldId == aSessionId)
		aStatus = AERR_SESSIONOPEN;

	else if (aSecLvl < aMinSecLvl)
		aStatus = AERR_ACCESS;

	// Check expiration date
	else if (aEndDay <= today())
		aStatus = AERR_ACCTEXPIRED;

	// Reconnect to session with updated user settings
	else if ((aStatus = -cpSessionMgr->connectSession(aSessionId)) <= 0)
	{
		// Get reference to old connection object
		long aOldConnectId = cSessionMap[aSessionId];
		AConnection* apOldConnection = cOpenConnections[aOldConnectId];

		// Copy log information of old connection to new connection
		if (apOldConnection != NULL)
		{
			apConnection->mpConsoleLog = apOldConnection->mpConsoleLog;

			// Mark old connection as unused
			apOldConnection->mConnectId = 0;
			apOldConnection->mpConsoleLog = NULL;
			apOldConnection->mComment = QString::null;
			apOldConnection->mContextName = QString::null;
			apOldConnection->mDefaultCloseWait = 0;
			apOldConnection->mEndDay = 0;
			apOldConnection->mIdleSecs = 0;
			apOldConnection->mLastAccess = 0;
			apOldConnection->mMode = geDefault;
			apOldConnection->mProtocolId = -1;
			apOldConnection->mpProtocolRcvr = NULL;
			apOldConnection->mSecLvl = -1;
			apOldConnection->mSysEndDay = 0;
			apOldConnection->mSysSecLvl = -1;
			apOldConnection->mSessionId = -1;
			apOldConnection->mpTimer = NULL;
			apOldConnection->mUsrId = 0;
		}

		// Out with the old
		if (aOldId > 0 && cSessionMap.contains(aOldId))
		{
			// Remove association of old session with current connection
			// Make sure the current connection will not be closed after the old session is closed
			cSessionMap[aOldId] = 0;
			cpSessionMgr->closeSession(geFirm, 0, aOldId);
		}

		// In with the new
		// The session is still associated with the same context as before.
		apConnection->mEndDay = aEndDay;
		apConnection->mIdleSecs = aIdleSecs;
		apConnection->mSecLvl = aSecLvl;
		apConnection->mSessionId = aSessionId;
		cSessionMap[aSessionId] = iConnectId;
		aOut = "true";
		orNewSessionId = aSessionId;
	}
	return aStatus;
}

/*!
 * \brief Delete a user from the system.
 *
 * \param[in] iConnectId Connection Id of the client that issued the command.
 * \param[in] irAmpmsg Original AMP message containing the message body.
 */
long AAisSvr::deleteUser(long iConnectId, const QString& irAmpmsg)
{
	AConnection* apConnection = cOpenConnections[iConnectId];
	QString aError;
	long aRet = 0;

	// Check permission
	if (apConnection->mSecLvl < geSuper)
	{	aError = "0,AAisSvr.deleteUser, No permission to delete user";
		aRet = AERR_ACCESS;
	}
	else
	{	QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
		long aTknSz = (long)aTkns.size();
		long aParamIdx = 0;
		long aUserId = 0;

		for (long aIdx = 2; aIdx < aTknSz; aIdx+=2)
		{	aParamIdx = aIdx + 1;
			if (aParamIdx < aTknSz)
			{	if (aTkns[aIdx] == "userid")
					aUserId = aTkns[aParamIdx].toLong();
				else
				{	aError = QString("0,AAisSvr.deleteUser, Unknown Parameter %1").arg(aTkns[aIdx]);
					LOGSYSMSG(geWarn, aError);
					aError.clear();
				}
			}
		}

		if (aUserId == 0)
		{	aError = "0,AAisSvr.deleteUser, One of the required parameters is empty";
			aRet = AERR_FEWARGS;
		}
		else
			aRet = cpUsrMgr->deleteUser(aUserId);
	}

	if (!aError.isEmpty())
		LOGSYSMSG(geWarn, aError);

	return aRet;
}

/*	---------------------------------------------------------------------------------------------------------------------------
dumpArray - Display the integer values in an IntArray
Args:
	irList	Integer array to be dumped
Returns:
	nothing
Notes:
 1.	Calls qDebug to display results in debugger Output window
	------------------------------------------------------------------------------------------------------------------------ */
void AAisSvr::dumpArray(AMemberList& irList)
{
	QString aDump;
	long aId = 0, aSz = irList.count();
	for (;;)
	{	aDump += QString::number(irList[aId]);
		if (++aId == aSz)
			break;
		aDump += ",";
	}
	qDebug() << "Array: " << aDump;
}

/*!
\brief enableConsoleLog - Enable/suspend/close a console log

\param iSessionId - Index into SMgr.cSessions array for session generating console output
\param iEnable -2 Reset Log, -1 Close Log, =0 Suspend Log, 1 Enable Log, 2 Format Log
\return aStatus - Error code if close fails; else, 0
\par Notes:
-# The session ID is used to look up the connect ID for the session being logged.
-# If close, return any lingering output before shutting down the log.
 */
void AAisSvr::enableConsoleLog(long iSessionId, long iEnable)
{
	// Look up connect ID for this session. Enable/disable console output buffering
	long aConnectId;
	AConnection* apConnect;
	AConsoleLogST* apLog;
	if (cSessionMap.contains(iSessionId) && (aConnectId = cSessionMap[iSessionId])> 0 &&
	(apConnect = cOpenConnections[aConnectId]) != NULL && (apLog = apConnect->mpConsoleLog) != NULL)
	{	if (iEnable >= 0)
			apLog->mLogEnable = iEnable;
		else	// Close. If anyone waiting for more output, return any remaining output to this client.
		{	if (apLog->mLogConnectId > 0)
			{	long aRetValue;
				long aRqId = apLog->mLogRqId;
				long aLogId = apLog->mLogConnectId;
				QString aOut;
				apLog->mLogConnectId = apLog->mLogRqId = 0;
				if ((aRetValue = cpSessionMgr->getConsoleLog(iSessionId, true/*clear*/, aOut)) >= 0)
				{	//aOut.prepend("display\177");
					returnMsg(aLogId, aRqId, 0/*Status*/, geGetConsoleLog, aRetValue, aOut, NULL/*Data*/, 0, cMt/*Display*/
					, cMt/*Error*/);
				}
			}
			apConnect->mpConsoleLog = NULL;
			delete apLog;
		}
	}
}

bool AAisSvr::findSubscriber(long iList, long iSubscriberId, long iSubscriptionId, bool iDelete)
{
	bool aFoundIt = false;
	ASubscriptionMap& arMap = cSubscriptions[iList];	// Key: SubscriptionId, Values: SubscriberIds
	ASubscriptionMap::Iterator apIt, apEnd;				// STL-style iterators.
	apEnd = arMap.end();
	apIt = arMap.find(iSubscriptionId);
	for (; apIt != apEnd && apIt.key() == iSubscriptionId; ++apIt)
	{	if (apIt.value() == iSubscriberId)
		{	if (iDelete)
				arMap.erase(apIt);
			aFoundIt = true;
			break;
		}
	}
	return aFoundIt;
}

// getConnectIdBySocketId is in aissvr.h
// getCurUsrName is in aissvr.h
// getSecLvl(long iConnectId, irContextName) is in aissvr.h
// getContextParams(const QString&, long* ) is in aissvr.h

/*!
 * \brief Retrieve connection statistics.
 *
 * \param[in] iConnectId Connection Id of client.
 * \param[in] orOut Output parameter.
 *
 * \note
 * The output parameter contains newline-delimited records.
 * Each record contains tab-delimited fields of the following format:
 *     ConnectionType ConnectionCount
 */
long AAisSvr::getConnectionStats(long iConnectId, QString& orOut)
{
	AConnection* apConnection = cOpenConnections[iConnectId];
	long aRet = 0;
	int aAppCount = 0;
	int aXmlCount = 0;
	int aHttpCount = 0;

	orOut.clear();

	// security check
	if (apConnection->mSecLvl < geSuper)
	{	QString aError = "0,AAisSvr.getConnectionStats, No permission to get connection statistics";
		LOGSYSMSG(geWarn, aError);
		aRet = AERR_ACCESS;
	}
	else
	{	foreach(AConnection* apConnObj, cOpenConnections)
		{	if (apConnObj->mConnectId > 0 && apConnObj->mMode == geOpen)
			{	switch(apConnObj->mProtocolId)
				{
				case AISSVR_APP:
					aAppCount++;
					break;
				case AISSVR_HTTP:
					aHttpCount++;
					break;
				case AISSVR_XML:
					aXmlCount++;
					break;
				default:
					qDebug("Unknown Protocol");
					break;
				}
			}
		}
		orOut.sprintf("APP\t%d\nHTTP\t%d\nXML\t%d\n", aAppCount, aHttpCount, aXmlCount);
	}

	return aRet;
}

/*	--------------------------------------------------------------------------------------------------------------------------
getConsoleLog - Ask session being logged to return console output to current connection when available.
Args:
	iConnectId	ID of connection requesting console output
	iSessionId	ID of session generating console output
	iRqId		ID of request for console output
Returns:
	aStatus		Error code if close fails; else, 0
Notes:
 1. The session ID is used to look up the connect ID for session whose console output is being logged.
 2. Save iConnectId and iRqId in the console log state maintained for this connection.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisSvr::getConsoleLog(long iConnectId, long iSessionId, long iRqId)
{
	// ConnectId. Look up connect ID for the session being logged.
	long aCnId;				// ConnectId of session generating output
	AConnection* apCn;		// -> connection in slot aCnId
	AConsoleLogST* apLog;	// -> for the log of the session generating output
	if (cSessionMap.contains(iSessionId) && (aCnId = cSessionMap[iSessionId]) > 0 && (apCn = cOpenConnections[aCnId]) != NULL
	&& (apLog = apCn->mpConsoleLog) != NULL)
	{	apLog->mLogConnectId = iConnectId;
		apLog->mLogRqId = iRqId;
	}
}

/*!
\brief getFileSpec - Look up path to root directory for the associated context

\param iConnectId - Index into cOpenConnections array for this connection
\param iProtocolId - Index into list of protocols (see getProtocolId above)
\param irContextName - Name of default context if no connection established
\param iorFileSpec - Input the file name, output the path + name of file.
\return aStatus - Error code (>0) if lookup fails; else, 0
\par Notes:
-# Every connection has a context and a client associated with the connection
-# If connection established, use connect ID; else,use iProtocolId and irContextName
-# Returns error if authorization to protected file fails or if file does not exist.
 */
long AAisSvr::getFileSpec(long iConnectId, long iProtocolId, const QString& irContextName,
					QString& iorFileSpec)
{
	// Help. If request for help, use $InstallPath$/docs/onlinedocs
	long aStatus = 0;

	if (irContextName == "Help")
	{
	    // 1st location
	    QString aFilePath = gAis.mGblParams["gblaisinstallpath"] + AISSVR_HELPDIR + iorFileSpec;
	    QString aTempPath = aFilePath;
	    QFileInfo aFileInfo(aFilePath);

	    // 2nd location
	    if (!aFileInfo.exists())
	    {
	        aTempPath = AISSVR_DOCSDIR + iorFileSpec;
	        aFileInfo.setFile(aTempPath);
	        if (aFileInfo.exists())
	            iorFileSpec = aTempPath;
	        else
	            iorFileSpec = aFilePath; // Requested file is missing
	    }
	    else
	        iorFileSpec = aFilePath;
	}
	else
	{	// ContextName. If iConnnectId > 0, look up protocol ID and ContextName.
		QString aContextName, aDefault, aFileSpec, aSecure;
		AConnection* apConnection = NULL;

		if (iConnectId > 0 && (apConnection = cOpenConnections[iConnectId]) != NULL)
		{	aContextName = apConnection->mContextName;
			iProtocolId = apConnection->mProtocolId;
		}
		else
			aContextName = irContextName;

		long aContextId = -1;
		AStringMap* apParams = getContextParams(aContextName, &aContextId);
		if (apParams == NULL)
			aStatus = AERR_BADFILENAME;			// Actually, unknown context
		else
		{	switch (iProtocolId)
			{
			case AISSVR_APP:
			case AISSVR_INPROC:
				if ((*apParams).contains("appdir"))
					aFileSpec = (*apParams)["appdir"];
				if ((*apParams).contains("appsecuredir"))
					aSecure = (*apParams)["appsecuredir"];
				if ((*apParams).contains("appdefaultpage"))
					aDefault = (*apParams)["appdefaultpage"];
				break;
			case AISSVR_HTTP:
				if ((*apParams).contains("httpdir"))
					aFileSpec = (*apParams)["httpdir"];
				if ((*apParams).contains("httpsecuredir"))
					aSecure = (*apParams)["httpsecuredir"];
				if ((*apParams).contains("httpdefaultpage"))
					aDefault = (*apParams)["httpdefaultpage"];
				break;
			case AISSVR_XML:
				if ((*apParams).contains("xmldir"))
					aFileSpec = (*apParams)["xmldir"];
				if ((*apParams).contains("xmlsecuredir"))
					aSecure = (*apParams)["xmlsecuredir"];
				if ((*apParams).contains("xmldefaultpage"))
					aDefault = (*apParams)["xmldefaultpage"];
				break;
			}
		}
		// Check/fix the input file name.
		if (iorFileSpec.isEmpty() || iorFileSpec.endsWith("/"))
		{	if (aDefault.isEmpty())
				aStatus = AERR_BADFILENAME;
			else
				iorFileSpec += aDefault;
		}
		if (aStatus <= 0)
		{	// Set FileSpec. Make sure that the file exists. Return full path.
			AUtil::startStg('/', iorFileSpec);
			aFileSpec += iorFileSpec;
			QFileInfo aFInfo(aFileSpec);
			if (aFInfo.exists())
			{	// Check security level if file is in protected subtree. Do fastest tests first.
				if ((apConnection == NULL || apConnection->mSecLvl <= 1) &&	iorFileSpec.indexOf(aSecure, 0/*Index*/,
				Qt::CaseInsensitive) >= 0)
					aStatus = AERR_ACCESS;
				iorFileSpec = aFileSpec;
			}
			else
				aStatus = AERR_BADFILENAME;
		}
	}
	return aStatus;
}

/*!
 * \brief Retrieve logon statistics.
 *
 * \param[in] iConnectId Connection Id of client.
 * \param[in] orOut Output parameter.
 *
 * \note
 * The output parameter contains newline-delimited records.
 * Each record contains tab-delimited fields of the following format:
 *     Username LogonCount
 */
long AAisSvr::getLogonStats(long iConnectId, QString& orOut)
{
	AConnection* apConnection = cOpenConnections[iConnectId];
	long aRet = 0;

	// security check
	if (apConnection->mSecLvl < geSuper)
	{	QString aError = "0,AAisSvr.getConnectionStats, No permission to get connection statistics";
		LOGSYSMSG(geWarn, aError);
		aRet = AERR_ACCESS;
	}
	else
	{	aRet = cpUsrMgr->getLogonStats(orOut);
	}
	return aRet;
}

/*	---------------------------------------------------------------------------------------------------------------------------
getProtocolId - Return protocol Id given the protocol name
Args:
	iProtocolName	all,app,http,xml,disconnected,in-process,no-connection
Returns:
	aId		Index into gAis.mpProtocolNames array, or AISSVR_NOCONNECT if invalid name
Notes:
	------------------------------------------------------------------------------------------------------------------------ */
long	AAisSvr::getProtocolId(const QString& irProtocolName)
{
	long aId = AISSVR_NOCONNECT;		// Protocol Id
	if (!irProtocolName.isEmpty())
	{	QString aName = irProtocolName.toLower();
		for (aId = 0; aId < AISSVR_PROTOCOLS; ++aId)
		{	if (aName == gAis.mpProtocolNames[aId])
				break;
		}
	}
	return aId;
}

/*	---------------------------------------------------------------------------------------------------------------------------
getSubscribers - Fetch the subscribers from a specific list
Args:
	iSessionId		Key for the entry into the cSubscriptons[iList] map.
Returns:
	aSubscribers	List of subscribers (connect IDs) for this entry.
Notes:
Notes:
 1. Exclude those subscribers if this session ID is in their exclusion list
	------------------------------------------------------------------------------------------------------------------------ */
AMemberList AAisSvr::getSubscribers(long iList, long iSubscriptionId)
{
	AMemberList aSubscribers;			// List of subscriptions for this subscriber.
	ASubscriptionMap& arSubscription = cSubscriptions[iList];	// Multi-valued map. Key: SubscriptionId, Values: ConnectIds
	ASubscriptionMap::Iterator apIt = arSubscription.find(iSubscriptionId);
	ASubscriptionMap::Iterator apEnd = arSubscription.end();
	for (; apIt != apEnd && apIt.key() == iSubscriptionId; ++apIt)
		aSubscribers += apIt.value();
	return aSubscribers;
}

/*	---------------------------------------------------------------------------------------------------------------------------
getSubscribers - Fetch the subscribers who want a copy of the console output to this session
Args:
	iSessionId		Index into SMgr.cSessions array for session generating console output
	orSubscriberSt	Holds the connectIDs extracted from all applicable subscriber lists
Returns:
	nothing
Notes:
 1. Exclude those subscribers that have this session ID on their exclusion list
	------------------------------------------------------------------------------------------------------------------------ */
AMemberList AAisSvr::getSubscribers(long iSessionId)
{
	// Context. Look up Context ID and Protocol ID for this session.
	long aConnectId = 0, aProtocolId = AISSVR_NOCONNECT, aSubscriptionId = 0;
	long aContextId = cpSessionMgr->getContextId(iSessionId);
	AConnection* apConnect;
	if (cSessionMap.contains(iSessionId)&&(aConnectId=cSessionMap[iSessionId])>0&&(apConnect=cOpenConnections[aConnectId])!=NULL)
		aProtocolId = apConnect->mProtocolId;

	// Subscribers. Get the subscribers in the subscriber map for this session.
	AMemberList aSubscribers;
	aSubscribers = getSubscribers(SESSIONS, iSessionId);

	// Rules. Get the subscribers to any rule that includes this session.  A subscriber may appear in list more than once.
	if (aContextId > 0 && aProtocolId > 0)
	{	// All. Add subscribers that subscribe to all contexts, all protocols.
		aSubscribers += getSubscribers(RULES, 0);

		// This Protocol. Add subscribers to any context, this protocol
		aSubscribers += getSubscribers(RULES, aProtocolId);

		// This Context. Add subscribers to this context, any protocol
		aSubscribers += getSubscribers(RULES, aContextId << AISSVR_SHIFT);

		// This Context and Protocol. Add subscribers to this context and to this protocol
		aSubscriptionId = aContextId << AISSVR_SHIFT | aProtocolId;
		aSubscribers += getSubscribers(RULES, aSubscriptionId);
	}
	// Duplicates. Put subscribers into a map for easy lookup. Eliminate duplicates.
	QMap<long, long> aSubscriberMap;
	long aSubscriber, aSz = aSubscribers.count();
	for (aSubscriber = 0; aSubscriber < aSz; ++aSubscriber)
		aSubscriberMap[aSubscribers[aSubscriber]] = 1;

	// Exclusions. Exclude those subscribers that have this session ID in their exclusion list.
	aSubscribers = getSubscribers(EXCLUSIONS, iSessionId);
	aSz = aSubscribers.count();
	for (aSubscriber = 0; aSubscriber < aSz; ++aSubscriber)
		aSubscriberMap[aSubscribers[aSubscriber]] = 0;

	// Convert. Convert map back into a list.
	QMap<long, long>::Iterator apIt, apEnd;			// STL-style iterators
	apEnd = aSubscriberMap.end();
	aSubscribers.clear();
	for (apIt = aSubscriberMap.begin(); apIt != apEnd; ++apIt)
	{	if (apIt.value() > 0)
			aSubscribers += apIt.key();
	}
	return aSubscribers;
}

/*	---------------------------------------------------------------------------------------------------------------------------
getSubscriptions - Return list of current subscriptions for this connection.
Args:
	iConnectId		Value contained by one or more entries in the cSubcriptions map.
Returns:
	aSubscriptions	List of integer Subscription IDs for this connection
	------------------------------------------------------------------------------------------------------------------------ */
AMemberList AAisSvr::getSubscriptions(long iConnectId, long iList)
{
	AMemberList aSubscriptions;			// List of subscriptions for this subscriber.
	ASubscriptionMap& arSubscription = cSubscriptions[iList];	// Multi-valued map. Key: SubscriptionId, Values: ConnectIds
	QHashIterator<long, long> apJt(arSubscription);		// Java-style iterator
	while (apJt.findNext(iConnectId))
		aSubscriptions += apJt.key();
	return aSubscriptions;
}

/*	---------------------------------------------------------------------------------------------------------------------------
getSubscriptions - Return list of current subscriptions
Args:
	iConnectId		Index into cOpenConnections array for this connection
	irAmpmsg		_ais|getsubscriptions|context|%s
	orOut			Holds generated list
Returns:
	aSz				Number of subscriptions
Notes:
 1. Returns a newline-terminated set of records. Each tab-separated record contains:
	Session record: ContextName\tsessionID\tContextName\tAdmin\tUsrName\tProtocol\tSubscribed
	where:
		Admin    "admin" if ContextAdminSession; else, empty
		Protocol -, ais, http, xml, disconnected, in-process
		Subscribed * iff this connection subscribes to the session.
	------------------------------------------------------------------------------------------------------------------------ */
long AAisSvr::getSubscriptions(long iConnectId, const QString& irAmpmsg, QString& orOut)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(irAmpmsg);

	// not yet implemented
	qDebug() << "[AAisSvr::getSubscriptions] - not yet implemented";
	orOut = "";
	return 0;
}

/*!
 * \brief Retrieve request statistics.
 *
 * \param[in] iConnectId Connection Id of client.
 * \param[in] orOut Output parameter.
 *
 * \note
 * The output parameter contains newline-delimited records.
 * Each record contains tab-delimited fields of the following format:
 *     Username LogonCount
 */
long AAisSvr::getRequestStats(long iConnectId, QString& orOut)
{
	AConnection* apConnection = cOpenConnections[iConnectId];
	long aRet = 0;

	// security check
	if (apConnection->mSecLvl < geSuper)
	{	QString aError = "0,AAisSvr.getRequestStats, No permission to get request statistics";
		LOGSYSMSG(geWarn, aError);
		aRet = AERR_ACCESS;
	}
	else
	{	aRet = cpSessionMgr->getRequestStats(orOut);
	}
	return aRet;
}

/*	---------------------------------------------------------------------------------------------------------------------------
getSessions - Return list of sessions associated with a context
Args:
	iConnectId		Index into cOpenConnections array for this connection
	irAmpmsg		_ais|getsessions|context|%s
	orOut			Holds generated list
Returns:
	aSz				Number of sessions
Notes:
 1. Returns a newline-terminated set of records. Each tab-separated record contains:
	Session record: ContextName\tsessionID\tContextName\tAdmin\tUsrName\tProtocol\tSubscribed
	where:
		Admin    "admin" if ContextAdminSession; else, empty
		Protocol -, ais, http, xml, disconnected, in-process
		Subscribed * iff this connection subscribes to the session.
	------------------------------------------------------------------------------------------------------------------------ */
long AAisSvr::getSessions(long iConnectId, const QString& irAmpmsg, QString& orOut)
{
	// Get Context name, if any. Get this client's sessionId, if any.
	long aConnectId = 0, aContextId, aProtocolId, aSessionId, aSubscriptionId, aSz;
	QString aContextName(irAmpmsg.section('\177', 3, 3)), aEntry;
	AConnection *apSubscriber = cOpenConnections[iConnectId];
	ACHECK(iConnectId > 0 && apSubscriber != NULL);
	AAppSvr *apAppSvr = NULL;

	// Get list of sessions. Tack the protocol name onto each session record
	QStringList aSessionList;
	AConnection *apConnect = NULL;
	aSz = cpSessionMgr->getSessions(aContextName, aSessionList);
	QStringList::Iterator apIt;
	QStringList::Iterator apEnd = aSessionList.end();
	for (apIt = aSessionList.begin(); apIt != apEnd; ++apIt)
	{	// Protocol. Look up the protocol ID for this session. Append protocol name to session record.
		aEntry = *apIt;
		aSessionId = aEntry.section('\t', 1, 1).toInt();
		apConnect = NULL;
		if (aSessionId > 0 && cSessionMap.contains(aSessionId) && (aConnectId = cSessionMap[aSessionId]) > 0 &&
			(apConnect = cOpenConnections[aConnectId]) != NULL && apConnect->mMode == geOpen)
			aProtocolId = apConnect->mProtocolId;
		else if (cpSessionMgr->isDisconnected(aSessionId))
			aProtocolId = AISSVR_DIS;
		else
			aProtocolId = AISSVR_NOCONNECT;
		aEntry.sprintf("\t%s", gAis.mpProtocolNames[aProtocolId]);

		// In-Process. Check
		if (apConnect != NULL)
		{
			apAppSvr = dynamic_cast<AAppSvr*>(apConnect->mpProtocolRcvr);
			if ((apAppSvr != NULL) && (apAppSvr->isInProcess(aConnectId)))
				aEntry += "\tIn-Process";
			else
				aEntry += "\tRemote";
		}
		else
			aEntry += "\tIn-Process";

		// Append an asterisk iff the current connection is a subscriber to this session
		aContextId = cpSessionMgr->getContextId(aSessionId);
		aSubscriptionId = aContextId << AISSVR_SHIFT | aProtocolId;
		if (apSubscriber != NULL && isSubscribed(iConnectId, aSessionId, aSubscriptionId))
			aEntry += "\t*";
		*apIt += aEntry;
	}
	// Return response back to client
	aSz = aSessionList.size();
	orOut = (aSz > 0) ? aSessionList.join("\n") : cMt;
	return aSz;
}

/*!
 * \brief Retrieve session statistics.
 *
 * \param[in] iConnectId Connection Id of client.
 * \param[in] orOut Output parameter.
 *
 * \note
 * The output parameter contains newline-delimited records.
 * Each record contains tab-delimited fields of the following format:
 *     Username LogonCount
 */
long AAisSvr::getSessionStats(long iConnectId, QString& orOut)
{
	AConnection* apConnection = cOpenConnections[iConnectId];
	long aRet = 0;

	// security check
	if (apConnection->mSecLvl < geSuper)
	{	QString aError = "0,AAisSvr.getSessionStats, No permission to get session statistics";
		LOGSYSMSG(geWarn, aError);
		aRet = AERR_ACCESS;
	}
	else
	{	aRet = cpSessionMgr->getSessionStats(orOut);
	}
	return aRet;
}

/*!
 * \brief Retrieves a list of users in the system.
 *
 * \param[in] iConnectId Connection Id of the client that issued the command.
 * \param[out] orOut List of user active users in the system.
 */
long AAisSvr::getUsers(long iConnectId, QString& orOut)
{
	AConnection* apConnection = cOpenConnections[iConnectId];
	long aRet = 0;

	if (apConnection->mSecLvl < geSuper)
	{	QString aError = "0,AAisSvr.getUsers, No permission to get list of users";
		LOGSYSMSG(geWarn, aError);
		aRet = AERR_ACCESS;
	}
	else
		cpUsrMgr->getUsers(orOut);

	return aRet;
}

bool AAisSvr::isSubscriber(long iList, long iSubscriberId, long iSubscriptionId)
{
	bool aIsSubscriber = false;
	ASubscriptionMap& arSubscription = cSubscriptions[iList];	// Multi-valued map. Key: SubscriptionId, Values: ConnectIds
	ASubscriptionMap::Iterator apIt = arSubscription.find(iSubscriptionId);
	ASubscriptionMap::Iterator apEnd = arSubscription.end();
	for (; apIt != apEnd && apIt.key() == iSubscriptionId; ++apIt)
	{	if (apIt.value() == iSubscriberId)
		{	aIsSubscriber = true;
			break;
		}
	}
	return aIsSubscriber;
}

/*	---------------------------------------------------------------------------------------------------------------------------
isSubscribed - Note if a client has a subscription to this session/subscription
Args:
	iConnectId		Index into cOpenConnections array for this connection
	iSessionId		Index into SMgr.cSessions array for session generating console output
Returns:
	aIsSubscribed	true iff this session is included in Session list or subscription is included in the rules list

Notes:
 1. If a client subscribes to a subscription that matches iSubscriptionId, it is included.
	------------------------------------------------------------------------------------------------------------------------ */
bool AAisSvr::isSubscribed(long iConnectId, long iSessionId, long iSubscriptionId)
{
	// Excluded. No need to check further if this session is excluded for this connection.
	bool aIsSubscribed = false;
	if (!isSubscriber(EXCLUSIONS, iConnectId, iSessionId))
	{	// Session. Check session list.
		if (isSubscriber(SESSIONS, iConnectId, iSessionId))
			aIsSubscribed = true;
		else
		{	// Rules. Check context-protocol rules for a match
			long aContextId = iSubscriptionId & ~AISSVR_MASK;
			long aProtocolId = iSubscriptionId & AISSVR_MASK;
			// Any. Include if client subscribes to any context/protocol
			if (isSubscriber(RULES, iConnectId, 0))
					aIsSubscribed = true;
			// Any Context. Include if client subscribes to any context, this protocol.
			else if (isSubscriber(RULES, iConnectId, aProtocolId))
				aIsSubscribed = true;
			// Any Protocol. Include if client subscribes to any protocol, this context
			else if (isSubscriber(RULES, iConnectId, aContextId))
				aIsSubscribed = true;
			// Subscription. Include if client subscribes to this context/protocol
			else if (isSubscriber(RULES, iConnectId, iSubscriptionId))
				aIsSubscribed = true;
		}
	}
	return aIsSubscribed;
}

/*!
 * \brief Logoff connection from AIS server.
 *
 * \param[in] iConnectId Index into cOpenConnections array for this connection.
 * \param[in] irAmpmsg _ais|logoff|connectid|%d
 *
 * \return aUsrId - UsrId or error code (<0) if logoff fails.
 */
long AAisSvr::logoff(long iConnectId, const QString& irAmpmsg)
{
	long aConnectId = irAmpmsg.section('\177', 3, 3).toLong();
	long aUsrId = 0;
	QString aLogEntry;

	aLogEntry.append(QDateTime::currentDateTime().toString());
	aLogEntry.append(": ");

	if (iConnectId <= 0)
	{	aUsrId = -AERR_LOSTCONNECTION;
		aLogEntry.append(gAis.mpErrMsgs[AERR_LOSTCONNECTION]);
	}
	else 
	{	// Default to logging off the current connection.
		if (aConnectId == 0)
			aConnectId = iConnectId;

		// Get pointer to connection object
		AConnection* apConnection = cOpenConnections[aConnectId];

		if (apConnection == NULL || apConnection->mConnectId <= 0)
		{	// Connection already lost
			aUsrId = -AERR_LOSTCONNECTION;

			aLogEntry.append("Connect Id: ");
			aLogEntry.append(QString::number(aConnectId));
			aLogEntry.append(", ");
			aLogEntry.append(gAis.mpErrMsgs[AERR_LOSTCONNECTION]);
		}
		else
		{	// Get User Id
			aUsrId = apConnection->mUsrId;

			// Build the log message
			aLogEntry.append("User: ");
			aLogEntry.append(cpUsrMgr->getUsrName(aUsrId));
			aLogEntry.append(", Protocol: ");
			aLogEntry.append(gAis.mpProtocolNames[apConnection->mProtocolId]);
			aLogEntry.append(", Connect Id: ");
			aLogEntry.append(QString::number(aConnectId));
			aLogEntry.append(", User Id: ");
			aLogEntry.append(QString::number(aUsrId));
			aLogEntry.append(", ");

			if (aUsrId <= 0 || !cpUsrMgr->logoff(aUsrId))
			{	aUsrId = 0;		// Already logged off
				aLogEntry.append("Already logged off");
			}
			else
			{	apConnection->mEndDay = 0;	
				apConnection->mSecLvl = 0;
				apConnection->mUsrId = 0;
				aLogEntry.append("Logoff successful");
			}
		}
	}

	aLogEntry.append("\n");
	LOGUSRACCMSG(aLogEntry);

	return aUsrId;
}

/*!
\brief logon - Authenticate a user for this connection.  Open a new connection if one is not open.

\param ipProtocolRcvr-> returnOutput routine
\param iDefaultCloseWait - The default manner in which this session will be closed.
\param iProtocolId - Identifies callers protocol (see getProtocolId above)
\param irUsrName - Logon name submitted by user
\param irPasswd - Password submitted by user
\param irContextName - Context associated with this connection.
\param opConnectId -> Place to return connectId
\param orAisOut - Returns UserId
\return aUsrId - UsrId or error code (<0) if logon fails
\par Notes:
-# After a connection is established, the user may access unprotected resources and submit a logon request.  A user must be
logged on in order to submit other requests or access a protected file.  A logon is not allowed on HTTP connections until
a cookie is set on the client machine.
-# A logon provides access to a server.  No context is required; however, if a context is specified and no user name is
provided and the context allows autologon, then the user is logged on as a "guest" with an empty password.
-# When a user is logged on, an entry is made in the OpenConnections array that contains the userId, security level, and days
left until account expires, and a ptr to the associated protocol server for returning output.  When a session is opened,
the current sessionId is also noted in the structure.
-# The OpenConnections list is keyed by the connectId.  A map is maintained to fetch the connectId given the sessionId (used
for returned output).
-# Called directly by each protocol server, appsvr, httpsvr, xmlsvr
 */
long AAisSvr::logon(AReturnRcvr* ipProtocolRcvr, long iDefaultCloseWait, long iProtocolId, const QString& irUsrName,
		const QString& irPasswd, const QString& irContextName, long* opConnectId, QString& orAisOut)
{	
	// UsrName. If no usr name and if context allows autologon, logon as "guest".
	long aUsrId = 0;				// User's ID from password file or error code.
	QString aUsrName(irUsrName);	// User's logon name.
	QString aLogEntry;

	aLogEntry.append(QDateTime::currentDateTime().toString());
	aLogEntry.append(": ");

	if (aUsrName.isEmpty() && !irContextName.isEmpty())
	{	// Autologon. Check context to see if autologon is allowed for this protocol
		if (irUsrName.isEmpty())
		{	QString aAuto;			// Autologon parameter name / parameter value
			aAuto = iProtocolId == AISSVR_HTTP ? "httpautologon" : iProtocolId == AISSVR_XML ? "xmlautologon" : "appautologon";
	
			// ContextName. If contextName is bogus, aAutoLogon will be empty.
			if (cpSessionMgr->getContextParam(irContextName, aAuto) == "1")
				aUsrName = "guest";
			else
				aUsrId = -AERR_ACCESS;
		}
	}

	aLogEntry.append("User: ");
	aLogEntry.append(aUsrName);
	aLogEntry.append(", Protocol: ");
	aLogEntry.append(gAis.mpProtocolNames[iProtocolId]);
	aLogEntry.append(", ");

	// Logon. Authenticate the user.
	QString aComment;
	long aConnectId, aEndDay = 0, aSecLvl = 0;
	if (aUsrId >= 0)	
		aUsrId = cpUsrMgr->logon(aUsrName, irPasswd, &aSecLvl, &aEndDay, aComment);
		
	// OpenConnection. Update existing connection or open a new connection for this user.
	if (aUsrId > 0)
	{	if ((aConnectId = (opConnectId != NULL) ? *opConnectId : 0) <= 0)
			aConnectId = openConnection(aConnectId, irContextName);
		AConnection* apConnection = cOpenConnections[aConnectId];
		apConnection->mComment = aComment;
		apConnection->mContextName = irContextName;
		apConnection->mEndDay = apConnection->mSysEndDay = aEndDay;
		apConnection->mProtocolId = iProtocolId;
		apConnection->mpProtocolRcvr = ipProtocolRcvr;
		apConnection->mSecLvl = apConnection->mSysSecLvl = aSecLvl;
		apConnection->mUsrId = aUsrId;

		// Set Default Close Wait Period [secs]. If set to default, use the context's default close wait.
		if (iDefaultCloseWait <= -2)
		{	QString aCloseWait = cpSessionMgr->getContextParam(irContextName, "CloseWait");
		// PENDING AAisSvr::logon(). If ACloseWait is empty, don't set value???
			iDefaultCloseWait = aCloseWait.toLong();
		}
		apConnection->mDefaultCloseWait = iDefaultCloseWait;
		// System logons never time out
		if (aUsrId == USRMGR_SYSUSRID)
			apConnection->mIdleSecs = 0;

		if (!orAisOut.isNull())
			orAisOut = QString::number(aUsrId);
		if (opConnectId != NULL)
			*opConnectId = aConnectId;

		aLogEntry.append("Connect Id: ");
		aLogEntry.append(QString::number(aConnectId));
		aLogEntry.append(", User Id: ");
		aLogEntry.append(QString::number(aUsrId));
		aLogEntry.append(", Logon successful");
	}
	else
	{
		aLogEntry.append("Logon failed, ");
		aLogEntry.append(gAis.mpErrMsgs[-aUsrId]);
	}

	aLogEntry.append("\n");
	LOGUSRACCMSG(aLogEntry);

	// else logon fails. Return error code.
	return aUsrId;
}

/*	---------------------------------------------------------------------------------------------------------------------------
logSysMsg - Process incoming sysMsg submitted by client
Args:
	irAmpmsg	_ais|logsysmsg|level|%d|msg|%s
Returns:
	nothing
Notes:
 1. System Msg format: "class::fcn(arg0:value0...), reason, varname:value"
 2. If level is not specified, default to geWarn.
 3. If no message, default to an "unknown message" message
	------------------------------------------------------------------------------------------------------------------------ */
void AAisSvr::logSysMsg(const QString& irAmpmsg)
{
	AErrLvl aLvl = geWarn;
	QString aMsg;
	QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
	if (aTkns.size() >= 4)
	{	aLvl = (AErrLvl)aTkns[3].toLong();
		if (aTkns.size() >= 6)
			aMsg = aTkns[5];
	}
	else
		aMsg.sprintf("AAisSvr::logSysMsg(%s), unable to extract message", irAmpmsg.toLatin1().data());
	LOGSYSMSG(aLvl, aMsg);
}

/*	---------------------------------------------------------------------------------------------------------------------------
needsSession - Identifies those requests that require a sessionId for a currently open session.
Args:
	iReqType	ampmsg, built-ins...
Returns:
	aNeedsSession true iff request requries an open session	
Notes:
1.	All of the following requests make calls to Session Mgr that require a valid session ID.
2.	Most call submit to submit an aisLisp expression to the smtbase engine.
3.	In addition, a few other calls noted below also require the current session ID as an argument.
4.	Other calls, e.g. getSessionUser, supply a session ID as a part of the AMP call so session ID is optional.
	------------------------------------------------------------------------------------------------------------------------ */
bool AAisSvr::needsSession(AReqType iReqType)
{
	bool aNeedsSession;
	switch (iReqType)
	{
	case geAmpMsg:				// submit AmpReq 
	case geCloseCabinet:		// submit AmpReq (browseLib.dropExent cabinetName)
	case geCloseConnection:		// closeConnection submits onDisconnect call???
	case geCompileLambda:		// submit AmpReq (browseLib.compileSource)
	case geCompileCabinet:		// submit AmpReq (browseLib.compileAll true)
	case geDebug:				// submit AmpReq
	case geEraseNode:			// submit AmpReq (browseLib.eraseSource
	case geEval:				// submit AmpReq
	case geExecute:				// submit AmpReq
	case geExportCabinet:		// submit AmpReq (browseLib.exportSource fileName ..allLambdas..)
	case geExportNode:			// submit AmpReq (browseLib.exportSource fileName LambdaName)
	case geGetDirInfo:			// submit AmpReq
	case geGetExtentNames:		// submit AmpReq (browseLib.getExtentNames)
	case geGetExtentTypes:		// submit AmpReq (browseLib.case getTypes extentname)
	case geGetNextLevel:		// submit AmpReq (browseLib.getNextLevel)
	case geGetWorkspaceStatistics: // submit AmpReq (browseLib.inspect stats:)
	case geImportCabinet:		// submit AmpReq (browseLib.importSource fileName)
	case geNewCabinet:			// submit AmpReq (browseLib.addExtent cabName cabPath)
	case geOpenNode:			// submit AmpReq browseLib.checkout
	case geOpenCabinet:			// submit AmpReq (browseLib.addExtent cabName cabPath)
	case geRunScriptFile:		// submit AmpReq (runScript {%s})
	case geSaveNode:			// submit AmpReq
	case geSetBreakpoint:		// submit AmpReq (debug {%s})
	case geSetEngineFlags:		// setEngineFlags
	case geSetErrorTrace:		// errorTrace
	case geSetEscape:			// setEscape
	case geSetInstructionTrace:	// instructionTrace
	case geSetJit:				// jit
	case geSetSysCheck:			// sysCheck
	case geShowConsole:			// submit AmpReq (writeln (eval {%s}))
		aNeedsSession = true;
		break;
	default:
		aNeedsSession = false;
		break;
	}
	return aNeedsSession;
}

/*	---------------------------------------------------------------------------------------------------------------------------
newConnection - Process incoming sysMsg submitted by client
Args:
	ipProtocolRcvr	-> returnOutput routine
	iProtocolId		Identifies callers protocol (see getProtocolId above)
	iUsrId			Server-wide ID assigned to this user
Returns:
	aConnectId	Index into cOpenConnections for this new connection
Notes:
	1. Arguments are saved in new connection structure.
	------------------------------------------------------------------------------------------------------------------------ */
long AAisSvr::newConnection(AReturnRcvr* ipProtocolRcvr, long iProtocolId, long iUsrId)
{
	// Look for an empty slot in open connections list. First entry is reserved
	long aConnectId, aSz = cOpenConnections.size();
	for (aConnectId = 1; aConnectId < aSz; ++aConnectId)
		if (cOpenConnections[aConnectId]->mConnectId <= 0)
		{	cOpenConnections[aConnectId]->mConnectId = aConnectId;
			break;
		}
	if (aConnectId >= aSz)
	{	AConnection* apConnection = new AConnection(this, "AConnection");
		cOpenConnections.insert(aConnectId, apConnection);
	}
	// Fetch the idle time for this context
	long aIdleSecs = 0;

	// Initialize the new connection structure
	AConnection* apConnection = cOpenConnections[aConnectId];
	apConnection->mConnectId = aConnectId;			// Mark as in-use
	apConnection->mComment = QString::null;
	apConnection->mpConsoleLog = NULL;
	apConnection->mContextName = QString::null;
	apConnection->mDefaultCloseWait = 0;
	apConnection->mEndDay = apConnection->mSysEndDay = 0;
	apConnection->mIdleSecs = aIdleSecs;
	apConnection->mLastAccess = ATimeStamp::localTime();
	apConnection->mMode = geOpen;
	apConnection->mProtocolId = iProtocolId;
	apConnection->mpProtocolRcvr = ipProtocolRcvr;	// Mark as connected (if not NULL)
	apConnection->mSecLvl = apConnection->mSysSecLvl = 0;
	apConnection->mSessionId = 0;
	apConnection->mpTimer = NULL;
	apConnection->mUsrId = iUsrId;

	return aConnectId;
}

/*	---------------------------------------------------------------------------------------------------------------------------
onCloseConnection - Close connection in response to client request
Args:
	iConnectId	Index into cOpenConnections to indentify connection to be closed
Returns:
	nothing
Notes:
 1. onCloseConnection is not called until the close wait-period has expired.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisSvr::onCloseConnection(long iConnectId)
{
	ACloseMode aMode;
	AConnection* apConnection;
	long aSz = cOpenConnections.size();
	if (iConnectId > 0 && iConnectId < aSz && (apConnection = cOpenConnections[iConnectId]) != NULL)
	{	// Timer. Stop the timer
		QTimer* apTimer = apConnection->mpTimer;
		if (apTimer != NULL)
		{	apTimer->stop();
			delete apTimer;
			apConnection->mpTimer = NULL;
		}
		// Close Mode. Note the close mode
		if ((aMode = apConnection->mMode) == geOpen)
		{	qDebug("AAisSvr::onCloseConnection(ConnectId:%ld), Invalid close mode:%d", iConnectId, aMode);
			return;
		}
		// Subscriptions. Unsubscribe from all of the subscription lists with this sessionId
		long iList;				// Index to one of the cSubscriptions maps
		long aSessionId = apConnection->mSessionId;
		if (aSessionId > 0)
		{	for (iList = SESSIONS; iList < 3; ++iList)		// No need to do rules.
				removeSubscriptions(iList, aSessionId);
		}
		// Subscribers. Remove this subscriber from all of the cSubscriptions maps
		for (iList = 0; iList < 3; ++iList)		// No need to do rules.
			removeSubscribers(iList, iConnectId);

		// Session. If a session active, close it.
		long aReqId, aRetValue, aStatus = 0;
		if (aSessionId > 0)
		{	// OnDisconnect. Alert other sessions on this context that this client has left the building
			QString aAmpmsg("_ais\177closeconnection"), aOut(""), aDisplay;
			aReqId = cpAisMgr->submit(aSessionId, 0/*RqId*/, aAmpmsg, false/*IsAsync*/, &aRetValue, aOut, aDisplay, NULL/*Data*/);

			// Close session. _ais|closesession|sessionid|%d|closemode|%s
			aAmpmsg.sprintf("_ais\177closesession\177sessionid\177%ld\177closemode\177%s",aSessionId,gAis.mpCloseMode[aMode]);
			if ((aReqId = cpAisMgr->submit(aSessionId, 0/*RqId*/, aAmpmsg, false/*IsAsync*/, &aRetValue, aOut, aDisplay, NULL)) < 0 )
				aStatus = -aReqId;
		}
		// Logoff.
		if (apConnection->mUsrId > 0)
		{	cpUsrMgr->logoff(apConnection->mUsrId);
			apConnection->mUsrId = 0;
		}
		// LogMgr. Unsubscribe from any logging.
		gAis.mpLogMgr->setLogLvl(geLogAll, geFatal, iConnectId);

		// Notify Protocol Server. Give Protocol server a chance to close up shop.
		AReturnRcvr* apProtocolRcvr;
		if ((apProtocolRcvr = apConnection->mpProtocolRcvr) != NULL)
			apProtocolRcvr->connectionClosed(iConnectId);

		// Clear connect structure if it is no longer associated with any session
		if (aSessionId <= 0)
		{	apConnection->mConnectId = 0;
			if (apConnection->mpConsoleLog != NULL)
			{	delete apConnection->mpConsoleLog;
				apConnection->mpConsoleLog = NULL;
			}
			apConnection->mComment = QString::null;
			apConnection->mContextName = QString::null;
			apConnection->mDefaultCloseWait = 0;
			apConnection->mEndDay = 0;
			apConnection->mIdleSecs = 0;
			apConnection->mLastAccess = 0;
			apConnection->mMode = geDefault;
			apConnection->mProtocolId = -1;
			apConnection->mpProtocolRcvr = NULL;
			apConnection->mSecLvl = -1;
			apConnection->mSysEndDay = 0;
			apConnection->mSysSecLvl = -1;
			apConnection->mpTimer = NULL;			
		}
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
onFlush - Force display of buffered messages from engine.
Args:
	iSessionId	Index into SMgr.cSessions array for session generating console output
	iEnable		<0 Log closed, =0 Log suspended, >0 Log enabled
Returns:
	aStatus		Error code if close fails; else, 0
Notes:
 1. Display output from engine is buffered to avoid swamping server with events
 2. The buffers are dumped if result generated or if AisSvr's timer fires.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisSvr::onFlush()
{
	// Flush the session display buffers if return result is still pending.
	cpAisMgr->flushSessions();
}

/*	---------------------------------------------------------------------------------------------------------------------------
onQuit - Shutdown contexts before the GUI is shut down.
Args: none
Returns: void
Notes:
 1. Not currently used, but it is here in case the shutdown order needs to be revised in order to avoid a conflict.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisSvr::onQuit()
{
#ifdef AIS_DEBUG
	qDebug("AAisSvr::onQuit()");
#endif
	cpSessionMgr->quit();
}

/*!
\brief openConnection - open a new connection

\param iOldConnectId	Index of an existing connection to be used as a template for the new connection
\param irContextName	New context name for this connection
\return aConnectId	Index into new connection
\par Notes:
-# If OldConnectId <= 0, it is up to the caller to fill out the new connection settings, especially mDefaultCloseWait,
mProtocolId, mpProtocolRcvr, and mUsrId.
 */
long AAisSvr::openConnection(long iOldConnectId, const QString& irContextName)
{
	// Create a new connection. Look for an empty slot in open connections. First entry reserved.
	long aConnectId, aSz = cOpenConnections.size();
	for (aConnectId = 1; aConnectId < aSz; ++aConnectId)
		if (cOpenConnections[aConnectId]->mConnectId <= 0)
		{	cOpenConnections[aConnectId]->mConnectId = aConnectId;
			break;
		}
	if (aConnectId >= aSz)
	{	AConnection* apConnection = new AConnection(this, "AConnection");
		cOpenConnections.insert(aConnectId, apConnection);
	}
	AConnection* apConnection = cOpenConnections[aConnectId];
	apConnection->mConnectId = aConnectId;			// Mark as in-use
	apConnection->mComment = QString::null;
	apConnection->mpConsoleLog = NULL;
	apConnection->mContextName = irContextName;
	apConnection->mDefaultCloseWait = -2;			// Default => set it later on
	apConnection->mEndDay = apConnection->mSysEndDay = 0;
	apConnection->mIdleSecs = 0;
	apConnection->mLastAccess = ATimeStamp::localTime();
	apConnection->mMode = geOpen;
	apConnection->mProtocolId = 0;
	apConnection->mpProtocolRcvr = NULL;	// Marked as connected when not NULL
	apConnection->mSecLvl = apConnection->mSysSecLvl = 0;
	apConnection->mSessionId = 0;
	apConnection->mpTimer = NULL;
	apConnection->mUsrId = 0;

	// Existing Connection. Fill in selected parameters from existing template if it is in-use and connected.
	AConnection* apC;			// -> existing template.
	AReturnRcvr* apProtocolRcvr;
	if (iOldConnectId > 0 && (apC = cOpenConnections[iOldConnectId]) != NULL && (apProtocolRcvr = apC->mpProtocolRcvr) != NULL)
	{	apConnection->mComment = apC->mComment;
		apConnection->mDefaultCloseWait = apC->mDefaultCloseWait;
		apConnection->mEndDay = apC->mEndDay;
		apConnection->mIdleSecs = apC->mIdleSecs;
		apConnection->mProtocolId = apC ->mProtocolId;
		apConnection->mpProtocolRcvr = apC->mpProtocolRcvr;	// Mark as connected to same protocol svr
		apConnection->mSecLvl = apC->mSecLvl;
		apConnection->mSysEndDay = apC->mSysEndDay;
		apConnection->mSysSecLvl = apC->mSysSecLvl;
		apConnection->mUsrId = apC->mUsrId;		
	}
	return aConnectId;
}

/*!
\brief openConsoleLog - Enable logging and to set Redirect flag.

\param iRedirect		If true, do not forward copy of consoleOutput to client, if any
\param iSessionId		Index into SMgr.cSessions array for this session
\return void
\par Notes:
-# A console log must be opened so that a ring file is allocated for catching the output. Use enableConsoleLog to close it.
-# AMP: _ais|openconsolelog|clear|%s|redirect|%s|size|%d|sessionid|%d|startonnewline|%s
 */
void AAisSvr::openConsoleLog(bool iRedirect, long iSessionId)
{
	// Look up connect ID for this session. Enable/disable console output buffering
	long aConnectId;
	AConnection* apConnect;
	if (cSessionMap.contains(iSessionId) && (aConnectId = cSessionMap[iSessionId]) > 0
			&& (apConnect = cOpenConnections[aConnectId]) != NULL) 
	{	AConsoleLogST* apLog;
		if ((apLog = apConnect->mpConsoleLog) == NULL)
			apLog = apConnect->mpConsoleLog = new AConsoleLogST;
		apLog->mLogConnectId = apLog->mLogRqId = 0;
		apLog->mLogEnable = 1;
		apLog->mLogRedirect = iRedirect;
	}
}

/*!
\brief openPort - Mop up after a session is closed

\param iProtocol - APP, INPROC, HTTP, or XML
\param iPort - The port or socket that the server listens on for incoming requests.
\param irContextName - Context name of context-specific port; else, global default context name
\return aStatus			Error code if open fails; else, 0
\par Notes:
-# Fails if port already in use.
 */
long AAisSvr::openPort(long iProtocol, ushort iPort, const QString& irContextName)
{
	long aStatus = 0;
	// Check to see if port is in-use. An error if in-use by another protocol.
	if (cOpenPorts.contains(iPort))
			aStatus = AERR_PORTINUSE;
	else	// Create a new instance of this protocol server for this new port.
	{	AReturnRcvr* apProtocolRcvr = NULL;	// -> instance of server for this protocol
		switch (iProtocol)
		{
		case AISSVR_APP:
		case AISSVR_INPROC:
			apProtocolRcvr = (AReturnRcvr*)new AAppSvr(iPort, irContextName, this/*Parent*/);
			break;
		case AISSVR_HTTP:
			apProtocolRcvr = (AReturnRcvr*)new AHttpSvr(iPort, irContextName, this/*Parent*/);
			break;
		case AISSVR_XML:
			apProtocolRcvr = (AReturnRcvr*)new AXmlSvr(iPort, irContextName, this/*Parent*/);
			break;
		default:
			break;
		}
		// Add this port to the list of open ports
		APortSt& arPortSt = cOpenPorts[iPort];
		arPortSt.mProtocolId = iProtocol;
		arPortSt.mpProtocolSvr = apProtocolRcvr;
	}
	return aStatus;
}

/*	---------------------------------------------------------------------------------------------------------------------------
openSession - Update connection state upon a successful open by SessionMgr
Args:
	iConnectId	Index into cOpenConnections array
	irAmpmsg	 _ais|opensession|context|%s|userid|%d|closemode|%s
Returns:
	aStatus		SessionId or error code (<0) if open fails
Notes:
 1.	ContextName defaults to the current context name (set by logon).
 2.	UserId defaults to the current user ID (also set by logon).
 3.	Sets the default close mode in case the closing closemode is default.  The default close mode may be soft, firm, hard
 4. Sets the default wait time in case closing closemode is default. =-1 wait forever, =0 no wait, n wait n secs.
 5.	openContext calls SessionMgr directly to open the Context Admin Session
 6. If default close mode or close wait is unspecified , SMgr will set their values to the context-specific default values.
	------------------------------------------------------------------------------------------------------------------------ */
long AAisSvr::openSession(long iConnectId, const QString& irAmpmsg)
{
	// UsrId, ContextName. Set UsrId and ContextName from AMP msg or set defaults from current settings.
	QString aContextName, aTkn;
	AConnection* apConnection = cOpenConnections[iConnectId];
	QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
	long aTknSz = aTkns.size();
	long aEndDay = apConnection->mEndDay, aNewEndDay, aNewSecLvl, aSecLvl = apConnection->mSecLvl, aUsrId = 0;
	ACloseMode aDefMode = geDefault;		// Default to using context-specific default close mode
	ACHECK(iConnectId > 0 && aTknSz >= 2);
	if (aTknSz >= 4)
	{	aContextName = aTkns[3];
		if (aTknSz >= 6)
		{	aUsrId = aTkns[5].toLong();
			if (aTknSz >= 8 && gAis.mCloseModes.contains(aTkn = aTkns[7].toLower()))
				aDefMode = gAis.mCloseModes[aTkn];
		}
	}
	if (aUsrId <= 0)
		aUsrId = apConnection->mUsrId;
	if (aContextName.isEmpty())
		aContextName = apConnection->mContextName;

	long aSessionId = apConnection->mSessionId;
	if (aUsrId <= 0 || aSecLvl <= 0 || aContextName.isEmpty())
		aSessionId = -AERR_ACCESS;
	// SecLvl, EndDay. Get default settings for SecLvl and EndDay for this context
	else if (!cpSessionMgr->getUsrSettings(aContextName, aUsrId, aNewSecLvl, aNewEndDay))
		aSessionId = -AERR_UNKCONTEXTNAME;
	else if (aUsrId != USRMGR_SYSUSRID && aNewSecLvl <= 0)
		aSessionId = -AERR_ACCESS;
	// TODO:
	// make sure that users not in contextusers.ini are not allowed to
	// open a session
	// aNewSecLvl contains the security level of the user, -1 if the user is not in contextusers.ini
	// aNewEndDay contains the end day of the user as specified in contextusers.ini

//	if (aContextName.isEmpty())
//		aSessionId = -AERR_UNKCONTEXTNAME;
//	else if (!cpSessionMgr->getUsrSettings(aContextName, aUsrId, aNewSecLvl, aNewEndDay))
//		aSessionId = -AERR_UNKCONTEXTNAME;
//	else if (aUsrId <= 0 || aSecLvl <= 0 || aNewSecLvl <= 0)
//		aSessionId = -AERR_ACCESS;
	else
	{	if (aNewSecLvl >= 0)
			aSecLvl = aNewSecLvl;
		if (aNewEndDay >= 0)
			aEndDay = aNewEndDay;

		// Check expiration date
		if (aEndDay <= today())
			aSessionId = -AERR_ACCTEXPIRED;

		// Open or update session with new settings
		else if ((aSessionId = cpSessionMgr->openSession(aDefMode, aContextName, aUsrId, aSecLvl, aSessionId)) > 0)
		{	// Get context-specific parameters, update settings and session map.
			long aContextId, aIdleSecs = 0;
			AStringMap*	apContextParams = cpSessionMgr->getContextParams(aContextName, &aContextId);
			if (apContextParams != NULL)
				aIdleSecs = (*apContextParams)["usridlesecs"].toLong();

			apConnection->mEndDay = aEndDay;	
			apConnection->mIdleSecs = aIdleSecs;
			apConnection->mSecLvl = aSecLvl;
			apConnection->mSessionId = aSessionId;
			cSessionMap[aSessionId] = iConnectId;
		}
	}
	return aSessionId;
}

/*!
\brief returnMsg - Send back messages directly to the client

\param iConnectId	ID of client's connection
\param iRqId		Request ID (incrementing integer) established on submit of original request.
\param iStatus		Error code >0 if an error
\param iReqType	Request type (eval, ampMsg, or built-in command)
\param iRetValue	Integer returned from AIS
\param irAisOut	Returned message
\param irDisplay	Console output
\param irError		Error details
\return void
\par Notes:
-# Returns payload to protocol server for this connection
-# irDisplay is always empty except when called by returnOutput
 */
void AAisSvr::returnMsg(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irAisOut
, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError)
{
	AConnection* apConnection = NULL;
	AReturnRcvr* apProtocolRcvr = NULL;
	// OpenConnection entry.
	if (iConnectId <= 0 || iConnectId >= cOpenConnections.size() ||
		(apConnection = cOpenConnections[iConnectId]) == NULL || apConnection->mMode != geOpen)
	{	QString aMsg;
		long aCd = AERR_UNKCONNECTID;
		aMsg.sprintf("AAisSvr::returnMsg(ConnectId:%ld, RqId:%ld, ReqType:%s, AisOut:%.128s, Display:%.128s), %s", iConnectId,
		iRqId, REQNAME(iReqType), irAisOut.toLatin1().data(), irDisplay.toLatin1().data(), gAis.mpErrMsgs[aCd]);
		// We can't log this message because the logger would end up right back here.
		qDebug() << aMsg;
	}
	// ProtocolRcvr. Return payload to Svr for this protocol.
	else if ((apProtocolRcvr = apConnection->mpProtocolRcvr) == NULL)
	{	qDebug("0, AAisSvr::returnMsg(ConnectId:%ld,RqId:%ld,Status:%ld,ReqType:%s,RetValue:%ld,AisOut:%.128s),Disconnected session",
		iConnectId, iRqId, iStatus, REQNAME(iReqType), iRetValue, irAisOut.toLatin1().data());
	}
	else
	{	apProtocolRcvr->returnOutput(iConnectId,iRqId,iStatus,iReqType,iRetValue,irAisOut,ipData,iDataSize,irDisplay,irError);
		if (_trace)
		qDebug("-----------------------------\n%s##%s~~~~~~~~~~~~~~~~~~~\n",irDisplay.toLatin1().data(),irAisOut.toLatin1().data());
	}
}

/*!
\brief returnOutput - Update the task state and then process the returned information from the AAisMgr

\param iConnectId - The ID of the connection that submitted the original request.
\param iSessionId - The ID of the session, if any, that submitted the original request.
\param iRqId - An incrementing integer assigned by the protocol server to each incoming request.
\param iStatus - Zero or a positive error code if an error was generated by the server
\param iReqType - Enum describing the type of response or pushed data from the server.
\param iRetValue - Integer return value (defaults to zero).
\param irAisOut - Returned message (defaults to an empty string).
\param ipData - Returned binary buffer containing object closure
\param iDataSize - Size of binary buffer in bytes.
\param irDisplay - Return from writeln and display (defaults to an empty string).
\param irError - Returned error message (defaults to an empty string).
\param iIsAsync - True iff the request gets an immediate acknowledgement and then the result is returned at a later time.
\param iClientData - Optional string submitted with a request and returned verbatim with the response
\return void
*/
void AAisSvr::returnOutput(long iConnectId, long iSessionId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irAisOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, bool iIsAsync, const QString iClientData)
{
    Q_UNUSED(iClientData);

	long aConnectId = 0;
	bool aIgnore = false;		// If true, OK to discard output if no connection available.
	bool aRedirect = false;		// Return display to this connection unless specifically turned off
	if (iSessionId <= 0)
		aConnectId = iConnectId;
	else
	{	// ConnectId. Look up connect ID 
		AConnection* apConnect = NULL;
		AConsoleLogST* apLog = NULL;
		if (cSessionMap.contains(iSessionId)&&(aConnectId=cSessionMap[iSessionId])>0&&(apConnect=cOpenConnections[aConnectId])!=NULL)
			apLog = apConnect->mpConsoleLog;
		
		// ConsoleOut.	Add selected AisOut to be included in console output to subscribers. 
		// For now, do not include return from AmpMsg in console output.  If required later, add user-settable option
		//  to select which, if any, return results are included in console out.
		QString aConsoleOut(irDisplay);		// Stuff to return to subscribers
		if (iReqType == geExecute || iReqType == geEval || iReqType == geRunScriptFile || iReqType == geShowConsole)
		{	if (!aConsoleOut.isEmpty() && !irAisOut.isEmpty())
			{	aConsoleOut += "\n";
			}
			aConsoleOut += irAisOut;
		}
		// Subscribers. Return console output to interested subscribers
		if (!aConsoleOut.isEmpty())
		{	ccSubscribers(iSessionId, aConsoleOut);
			LOGCONSOLEMSG(aConsoleOut + "\n");

			/*	PENDING AAisSvr::returnOutput()
			1. If a subscriber has its output being logged, log the output copied to the subscriber
			2. Add irAisOut as a special record to the console log if one of the above special cases or if an async return.
			3. In HttpSvr, rip out stuff that adds <result>stuff</result> to console log output.
			*/
			// Console Log. Save console output if buffering is enabled. Alert anyone waiting for console output.
			// _aisopenconsolelogcleartrueredirecttruesessionid0size100startonnewlinetrue
			// _aisgetconsolelogcleartruesessionid0waittrue
			// _aisenableconsolelogenablesuspendsessionid0
			if (apLog != NULL)
			{	long aEnableLog = apLog->mLogEnable;
				aRedirect = (aEnableLog > 0 && apLog->mLogRedirect);
				if (aEnableLog > 0)
				{	if (!irDisplay.isEmpty())
						cpSessionMgr->saveConsoleLog(irDisplay, iRqId, iSessionId, 'D');
					if (!irAisOut.isEmpty())
						cpSessionMgr->saveConsoleLog(irAisOut, iRqId, iSessionId, 'R');
					
					// Alert. Alert anyone waiting for more output.
					if (apLog->mLogConnectId != 0)
					{	long aRetValue;		// Bytes returned
						if ((aRetValue = cpSessionMgr->getConsoleLog(iSessionId, true/*clear*/, aConsoleOut)) > 0)
						{	returnMsg(apLog->mLogConnectId, apLog->mLogRqId, 0/*Status*/, geGetConsoleLog, aRetValue
							, aConsoleOut, ipData, iDataSize, cMt, cMt);
							apLog->mLogConnectId = apLog->mLogRqId = 0;
						}
					}
				}
				// SysMsg. Note console output to Context's Admin Session
				else if (cpSessionMgr->isAdminSession(iSessionId))
				{	aConsoleOut.prepend("AdminSession: ");
					LOGSYSMSG(geInfo, aConsoleOut);
					aIgnore = true;			// Ok to ignore output to Context's Admin Session
				}
			}
		}

		// Make sure not to forward console output of disconnected connections
		if (apConnect != NULL && apConnect->mMode != geOpen)
		{	aConnectId = 0;
			aIgnore = true;
		}
	}
	// Post-process selected responses.
	switch (iReqType)
	{
	case geCloseConnection:
		aIgnore = true;
		break;

	case geCloseContext:
		if (iStatus == 0)
		{	if (iRetValue > 0)
				aConnectId = iRetValue;		// iRetValue holds connect ID of caller.
			if (iSessionId > 0)
				closeContext(iSessionId);	// iSessionId holds context ID of context being closed
			aIgnore = true;
		}
		break;

	// Send notice to session being closed. If session opened by another client, return result to
	// the calling client.
	case geCloseSession:
		// Make sure that close session was successful
		if (iStatus == 0)
		{	if (iRetValue > 0)
			{	aConnectId = iRetValue;
				iRetValue = iSessionId;	// Set iRetValue to new session id to be passed to the client
			}
			if (iSessionId > 0)
				closeSession(iSessionId); // Perform necessary final cleanup
		}
		aIgnore = true;
		break;

	/* No session ID required for the following requests.
	case geConnectSession:
	case geEnableConsoleLog:
	case geGetConsoleLog:
	case geGetContextId:
	case geGetContextParams:
	case geGetCurrentContexts:
	case geGetSessionId:
	case geGetSubscriptions:
	case geIsContextBusy:
	case geIsContextOpen:
	case geLogoff:
	case geLogSysMsg:
	case geNoop:
	case geOpenConsoleLog:
	case geOpenContext:
	case geOpenSession:
	case geRegisterContext:
	case geSetLogLvl:
	case geSetRules:
	case geSetSubscriptions: */
	default:
		break;
	}
	// Return output if connected. Include display if it is not currently redirected to console log.
	if (aConnectId > 0)
	{	if (!iIsAsync)
		{	const QString& arDisplay = (aRedirect) ? cMt : irDisplay;
			returnMsg(aConnectId, iRqId, iStatus, iReqType, iRetValue, irAisOut, ipData, iDataSize, arDisplay, irError);
		}
	}
	else if (cServiceMode)
	{
		qDebug() << irDisplay;
	}
	// Ok to drop selected requests generated by closing a connection	
	else if (!aIgnore)
	{	QString aMsg;
		long aCd = AERR_UNKCONNECTID;
		aMsg.sprintf("AAisSvr::returnOutput(SessionId:%ld, RqId:%ld, Status:%ld, ReqType:%s, AisOut:%.128s, Display:%.128s), %s",
		iSessionId,iRqId,iStatus,REQNAME(iReqType),irAisOut.toLatin1().data(),irDisplay.toLatin1().data(),gAis.mpErrMsgs[aCd]);
		qDebug() << aMsg;
	}
}

// Do not increment apIt after the last entry is deleted from a map. A crash is lurking.
bool AAisSvr::removeSubscriptions(long iList, long iSubscriptionId)
{
	bool aFoundOne = false;
	AMemberList aSubscribers;				// List of subscriptions for this subscriber.
	ASubscriptionMap::Iterator apIt;
	ASubscriptionMap& arSubscription = cSubscriptions[iList];	// Multi-valued map. Key: SubscriptionId, Values: ConnectIds
	while ((apIt = arSubscription.find(iSubscriptionId)) != arSubscription.end())
	{	arSubscription.erase(apIt);
		aFoundOne = true;
	}
	return aFoundOne;
}

bool AAisSvr::removeSubscriber(long iList, long iSubscriberId, long iSubscriptionId)
{
	bool aFoundIt = false;
	AMemberList aSubscribers;				// List of subscriptions for this subscriber.
	ASubscriptionMap& arSubscription = cSubscriptions[iList];	// Multi-valued map. Key: SubscriptionId, Values: ConnectIds
	ASubscriptionMap::Iterator apIt = arSubscription.find(iSubscriptionId);
	ASubscriptionMap::Iterator apEnd = arSubscription.end();
	while (apIt != apEnd && apIt.key() == iSubscriptionId)
	{	if (apIt.value() == iSubscriberId)
		{	arSubscription.erase(apIt);
			aFoundIt = true;
			break;
		}
		// Do not increment apIt after the last entry is deleted. A crash is lurking.
		if (apIt != apEnd)
			++apIt;
	}
	return aFoundIt;
}

bool AAisSvr::removeSubscribers(long iList, long iConnectId)
{
	bool aFoundOne = false;
	ASubscriptionMap& arSubscription = cSubscriptions[iList];	// Multi-valued map. Key: SubscriptionId, Values: ConnectIds
	QMutableHashIterator<long, long> apJt(arSubscription);		// Java-style iterator
	while (apJt.findNext(iConnectId))
	{	apJt.remove();
		aFoundOne = true;
	}
	return aFoundOne;
}

/*	---------------------------------------------------------------------------------------------------------------------------
setRules - Subscribe to or unsubscribe from one or more rules
Args:
	iConnectId	ID of client subscribing or unsubscribing 
	iSessionId  ID of client subscribing or unsubscribing
	irAmpmsg	_ais|setrules|rules|%s|mode|%d
Returns:
	Number of rules added
Notes:
 1. Rules holds one or more comma-delimited contextName.protocolName pairs.
 2. Each pair is separated by a period, e.g. TestAis.xml.
 3. If either the contextName is missing or is "all", all contexts are included in the rule.
 4. Similarly for the protocol name.
 5. If the mode element is 0 or missing, the rule is added; else, the rule is removed.
 6. If subscribing, the session of the client is excluded to avoid sending output to itself.
	------------------------------------------------------------------------------------------------------------------------ */
long AAisSvr::setRules(long iConnectId, long iSessionId, const QString& irAmpmsg)
{
	// Extract the rules from irAmpmsg.
	long aContextId = 0;
    long aProtocolId = 0;
    long aSubscriptionId = 0;
    long aSz = 0;
    long aIx = 0;
	QStringList aRules, aPair, aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
	long aTknSz = aTkns.size();
	bool aDelete = (aTknSz >= 6 && aTkns[5] != "0");
	ACHECK(iConnectId > 0 && aTknSz >= 4);
	QString aContext, aRuleList(aTkns[3]), aProtocol;

	// Rules. Convert rules to a Subscription ID. Update list of subscribers
	if (!aRuleList.isEmpty())
	{	// Extract rules from comma-delimited list of rules.
		aRules = aRuleList.split(QChar(','), QString::KeepEmptyParts);
		aSz = aRules.count();
		for (aIx = 0; aIx < aSz; ++aIx)
		{	// Convert each new rule to pair of names.
			aPair = aRules[aIx].split(QChar('.'), QString::KeepEmptyParts);
			aTknSz = aPair.size();
			if (aTknSz >= 2)
			{	aContext = aPair[0];
				aProtocol = aPair[1];
			}
			else
			{	aContext = "all";
				aProtocol = aPair[0];
			}
			if (aContext.isEmpty() || aContext == "all")
				aContextId = AISSVR_ALL;
			else
				aContextId = cpSessionMgr->getContextId(aContext);

			aProtocolId = getProtocolId(aProtocol);
			aSubscriptionId = aContextId << AISSVR_SHIFT | aProtocolId;
			if (aDelete)
			{	ASubscriptionMap& arSubscription = cSubscriptions[RULES];	// Multi-valued map. Key: SubscriptionId, Values: ConnectIds
				QMutableHashIterator<long, long> apJt(arSubscription);		// Java-style iterator
				if (apJt.findNext(iConnectId) && apJt.key() == aSubscriptionId)
					apJt.remove();
				ASubscriptionMap& arExSubscription = cSubscriptions[EXCLUSIONS];// Multi-valued map. Key: SessionId, Values: ConnectIds
				if (arExSubscription.contains(iSessionId))
					arExSubscription.remove(iSessionId);
			}
			else
			{	AMemberList aSubscribers;
				aSubscribers.append(iConnectId);
				setSubscription(RULES, aSubscribers, aSubscriptionId);
				setSubscription(EXCLUSIONS, aSubscribers, iSessionId);
			}
		}
	}
	return aSz;	// Return number of rules added
}

void AAisSvr::setSubscription(long iList, AMemberList iSubscribers, long iSubscriptionId)
{
	ASubscriptionMap& arSubscription = cSubscriptions[iList];	// Multi-valued map. Key: SubscriptionId, Values: ConnectIds
	long aSz = iSubscribers.count();
	for (long aSubscriber = 0; aSubscriber < aSz; ++aSubscriber)
		arSubscription.insert(iSubscriptionId, iSubscribers[aSubscriber]);
}

/*	---------------------------------------------------------------------------------------------------------------------------
// setSubscriptions - Client submits request to subscribe to new sessions
Args:
	iConnectId	ID of subscriber, subscribing to new sessions
	irAmpmsg	 _ais|setsubscriptions|new|%s|old|%s
Returns:
	Number of subscriptions added
Notes:
 1. New and Old each hold zero or more comma-delimited sessionIDs where old is the list
	provided by a previous call to getSubscriptions.  Note that current list may have changed
	since the old list was provided.
 2. The session list and exclusion list are updated as follows:
				New is in
	New   Old	Exclusion List   Action
	 Y     N       N             Add New to session list (New should not be in session list).
	 Y     N       Y             Remove New from exclusion list (New should be included in a rule).
				Old is in
	 New  Old  Session List
	  N     Y       Y           Remove from session list (Old should not be in exclusion list).
	  N     Y       N           Add to exclusion list (Old should be included in a rule).
	  N     N       -           No change
	  Y     Y       -           No change     
 3. If a session is removed that was included in the list due to a rule and not because it
	is in the session list, it is excluded rather than just removed.
	------------------------------------------------------------------------------------------------------------------------ */
long AAisSvr::setSubscriptions(long iConnectId, const QString& irAmpmsg)
{
	// SessionId. Extract the session IDs from irAmpmsg.
#ifdef ACHECK_DEBUG
	AConnection* apConnect = cOpenConnections[iConnectId];
#endif
	QStringList aNewList, aOldList, aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
	long aId, aSessionId, aTknSz = aTkns.size();
	ACHECK(iConnectId > 0 && aTknSz >= 4 && apConnect != NULL);
	QString aNew(aTkns[3]), aOld;
	if (aTknSz >= 6) aOld = aTkns[5];

	// Session Lists. Convert comma-delimited lists of strings into list of Session IDs
	if (!aNew.isEmpty())
		aNewList = aNew.split(QChar(','), QString::SkipEmptyParts);
	if (!aOld.isEmpty())
		aOldList = aOld.split(QChar(','), QString::SkipEmptyParts);	
	AMemberList aNewIds, aNyIds, aOldIds, aYnIds;
	// NewIds. Initialize NewIds to the new list (Y *)
	long aNewSz = aNewList.size();
	for (aId = 0; aId < aNewSz; ++aId)
	{	aSessionId = aNewList.at(aId).toInt();
		aNewIds += aSessionId;
	}
	// OldIds. Initialize OldIds to the old list (* Y) 
	long aOldSz = aOldList.size();
	for (aId = 0; aId < aOldSz; ++aId)
	{	aSessionId = aOldList.at(aId).toInt();
		aNewIds += aSessionId;
	}
	// YnIds. Set YnIds to new entries that are not in old (Y  N)
	aNewSz = aNewIds.count();
	for (aId = 0; aId < aNewSz; ++aId)
	{	aSessionId = aNewIds.at(aId);
		if (aOldIds.indexOf(aSessionId) < 0)
			aYnIds += aSessionId;
	}
	// NyIds. Set NyIds to the old entries that are not in new (N  Y)
	aOldSz = aOldIds.count();
	for (aId = 0; aId < aOldSz; ++aId)
	{	aSessionId = aOldIds.at(aId);
		if (aNewIds.indexOf(aSessionId) < 0)
			aNyIds += aSessionId;
	}
	// New Entries.
	if (!aYnIds.isEmpty())
	{	aNewSz = aYnIds.count();
		AMemberList aSubscribers;
		aSubscribers += iConnectId;
		for (aId = 0; aId < aNewSz; ++aId)
		{	aSessionId = aYnIds.at(aId);
			// Add New. Add those YnIds not in exclusion list (Y  N  N) to Session list
			// Remove New. Remove remaining new entries (Y   N  Y) from Exclusion list.
			if (!findSubscriber(EXCLUSIONS, iConnectId, aSessionId, true/*Delete*/))
				setSubscription(SESSIONS, aSubscribers, aSessionId);
		}
	}
	// Old Entries.
	if (!aNyIds.isEmpty())
	{	aOldSz = aNyIds.count();
		AMemberList aSubscribers;
		aSubscribers += iConnectId;
		for (aId = 0; aId < aOldSz; ++aId)
		{	aSessionId = aNyIds.at(aId);
			// Remove Old. Remove those aNyIds in exclusion list list (N  Y  Y) from Session list
			if (findSubscriber(EXCLUSIONS, iConnectId, aSessionId, false/*Delete*/))
				removeSubscriber(SESSIONS, iConnectId, aSessionId);
			else // Add Old. Add remaining old entries (N  Y   N) to Exclusion list.
				setSubscription(EXCLUSIONS, aSubscribers, aSessionId);
		}
	}
	// Return number of new sessions found.
	return aNewList.size();
}

/*	---------------------------------------------------------------------------------------------------------------------------
subscribe - Subscribe/unsubscribe. Copy output to the connection specified by iConnectId from the session specified by iConnectId
Args:
	iCurSessionId	ID of subscriber, subscribing to a session
	iSessionId		SessionId of the session that is being subscribed to.
	iRemove			Remove this subscriber from the subscription list for iSessionId
Returns:
	nothing
Notes:
PENDING - if iSessionID <= 0, Subscribe to all sessions.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisSvr::subscribe(long iCurSessionId, long iSessionId, bool iRemove)
{
	long aCnId;
	AConnection* apCn;

	if (cSessionMap.contains(iCurSessionId) && (aCnId = cSessionMap[iCurSessionId]) > 0 && (apCn = cOpenConnections[aCnId]) != NULL)
	{	if (iRemove)
			removeSubscriber(SESSIONS, aCnId, iSessionId);
		else
		{	ASubscriptionMap& arSubscription = cSubscriptions[SESSIONS];	// Multi-valued map. Key: SubscriptionId, Values: ConnectIds
			arSubscription.insert(iSessionId, aCnId);
		}
	}
}

/*!
\brief submit - Process an incoming request

\param irAmpmsg - The incoming AMP message to be processed.
\param iIsAsync - If true, return a response even before request is processed.
\param iConnectId - Index into cOpenConnections array
\param iRqId - ID assigned to the request by the calling client.
\param ipData -> to binary data portion of the request.
\return aReqId - Server ID assigned to this request or error code (<0) if submit fails
\par Notes:
-# If built-in command (e.g. LogSysMsg), it is handled locally
 */
long AAisSvr::submit(const QString& irAmpmsg, bool iIsAsync, long iConnectId, long iRqId, char* ipData)
{
	// Check inputs
	long aReqId = 0, aRetValue = 0, aSessionId = 0, aSz = cOpenConnections.size();
	AReqType aReqType = geUnknown;
	AConnection* apConnection = NULL;

	if ((iConnectId <= 0 || iConnectId >= aSz ||(apConnection = cOpenConnections[iConnectId]) == NULL ||
	apConnection->mConnectId <= 0 || irAmpmsg.isEmpty()))
		aReqId = -AERR_DISCONNECTED;
	else
	{	// Check to see if max idle period has been exceeded.	
		long aIdleSecs;
		if ((aIdleSecs = apConnection->mIdleSecs) > 0)
		{	long aLocalTime = ATimeStamp::localTime();		// Secs since 1/1/2000
			if (aLocalTime >= apConnection->mLastAccess + aIdleSecs)
			{	// Return userName in AisOut before this info is destroyed
				// PENDING AAisSvr::submit(). Save this elsewhere...
				// orAisOut = getCurUsrName(iConnectId);
				// Log off, close session, and return error.
				// logoff(iConnectId, geDefault);
				aReqId = -AERR_TIMEOUT;
			}
			else
				apConnection->mLastAccess = aLocalTime;
		}
		// Timer. If close-connection is pending, reset timer
		if (apConnection->mMode != geOpen && apConnection->mpTimer != NULL)
		{	apConnection->mpTimer->stop();
			apConnection->mMode = geOpen;
		}
		aSessionId = apConnection->mSessionId;
	}
	bool aSubmit = false;		// Do not submit to session manager, unless set below
	QString aError(cMt), aDisplay(cMt), aOut(cMt);
	if (aReqId >= 0)
	{	// Lookup request type from speech act. Check connection, then dispatch
		QString aTarget = irAmpmsg.section('\177', 0, 0);			// Target Lambda
		if (aTarget == "_ais")
		{	QString aAct = irAmpmsg.section('\177', 1, 1).toLower();	// Speech act
			if (aAct.isEmpty()) aAct = "noop";
			aReqType = REQTYPE(aAct);
			//	Dispatch to local commands:
			switch (aReqType)
			{
			// AddUser: _ais|adduser|userid|%d|username|%s|password|%s|securitylevel|%d|begindate|%s|enddate|%s|comment|%s
			case geAddUser:
			{
				if ((aReqId = -addUser(iConnectId, irAmpmsg)) == 0)
					aOut = "true";
				else
					aRetValue = -aReqId;
				break;
			}
			// CloseConnection: _ais|closeconnection|connectid|%d|closemode|%s|closewait|%d
			case geCloseConnection:
			{	QString aTkn;
				QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::SkipEmptyParts);
				long aTknSz = (long)aTkns.size();
				long aConnectId = 0;
				ACloseMode aMode = geDefault;
				long aWait = -2;		// Use default timeout period before closing.
				if (aTknSz >= 4)
				{	aConnectId = aTkns[3].toInt();
					if (aTknSz >= 6)
					{	if (gAis.mCloseModes.contains(aTkn = aTkns[5].toLower()))
							aMode = gAis.mCloseModes[aTkn];
						if (aTknSz >= 8)
							aWait = aTkns[7].toLong();
					}
				}
				// If the connect ID is zero, close the current connection.
				if (aConnectId <= 0)
					aConnectId = iConnectId;
				closeConnection(aConnectId, aMode, aWait);

				// Return. If this connection closed, no use returning, else return an immediate result to the caller.
				// If ReqId <- 1 and submit is false, no return.
				aReqId = (aConnectId == iConnectId) ? 1 : 0;
				break;
			}
			// CloseContext: _ais|closecontext|context|%s|closemode|%s
			// CloseSession: _ais|closesession|sessionid|%d|closemode|%s
			case geCloseContext:
			case geCloseSession:
				aSubmit = true;
				aRetValue = iConnectId; // Pass in connect ID to identify caller.
				break;
			// ConnectSession:	_ais|connectsession|sessionid|%d
			case geConnectSession:
				// connectSession returns status >= 0
				if ((aReqId = -connectSession(iConnectId, iRqId, irAmpmsg, aSessionId)) == 0)
				{
					aOut = "true";
					aRetValue = aSessionId;

					if ((aReqId = cpSessionMgr->getConsoleLog(aSessionId, true, aOut)) >= 0)
					{
						aReqId = 0;
						cpSessionMgr->enableConsoleLog(aSessionId,-1);
						if (aOut.isEmpty())
							aOut = "true";
					}
				}
				break;
			// DeleteUser: _ais|deleteuser|userid|%d
			case geDeleteUser:
			{	if ((aReqId = -deleteUser(iConnectId, irAmpmsg)) == 0)
					aOut = "true";
				else
					aRetValue = -aReqId;
				break;
			}
			//	EnableConsoleLog: _ais|enableconsolelog|enable|%s|sessionid|%d
			//	Enable: reset = -2, close = -1, suspend = 0, enable = 1, format = 2
			case geEnableConsoleLog:
			{	long aLogSessionId = 0;
				long aEnable = 1;	// Default to enable
				QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
				long aTknSz = aTkns.size();
				aLogSessionId = 0;		// Default to current session, if any
				if (aTknSz >= 4)
				{	QString aEn = aTkns[3].toLower();
					aEnable = (aEn == "-2" || aEn == "reset") ? -2 : (aEn == "-1" || aEn == "close") ? -1 : 
					(aEn == "0" || aEn == "suspend") ? 0 : (aEn == "2" || aEn == "format") ? 2 : 1;
					if (aTknSz >= 6)
						aLogSessionId = aTkns[5].toLong();
				}
				// Default to current session.
				if (aLogSessionId <= 0)
					aLogSessionId = aSessionId;
					
				if (aLogSessionId <= 0)
					aReqId = -AERR_SESSIONCLOSED;
				else	// Close (<0), suspend (=0) or enable (>0) the log
				{	// enableConsoleLog returns error code (<= 0)
					if ((aReqId = cpSessionMgr->enableConsoleLog(aLogSessionId, aEnable)) == 0)
						aOut = "true";
				}
				break;
			}
			// GetConnectionStats:	_ais|getconnectionstats
			case geGetConnectionStats:
			{
				if ((aReqId = -getConnectionStats(iConnectId, aOut)) != 0)
					aRetValue = -aReqId;
				break;
			}
			// GetConsoleLog:	_ais|getconsolelog|clear|%s|sessionid|%d|wait|%s
			case geGetConsoleLog:
			{	QString aTkn;
				QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
				long aTknSz = aTkns.size();
				bool aClear = true, aWait = false;		// Default to clear log, no wait
				bool aAbort = false;		// Abort the current outstanding request
				long aLogSessionId = 0;		// Default to current session, if any
				if (aTknSz >= 4)
				{	aTkn = aTkns[3].toLower();
					if (aTkn == "0" || aTkn == "false")
						aClear = false;
					else if (aTkn == "-1" || aTkn == "abort")
						aAbort = true;
					if (aTknSz >= 6)
					{	aLogSessionId = aTkns[5].toLong();
						if (aTknSz >= 8)
						{	aTkn = aTkns[7].toLower();
							aWait = !(aTkn == "0" || aTkn == "false");
						}
					}
				}
				// Default to current session.
				if (aLogSessionId <= 0)
					aLogSessionId = aSessionId;

				// Get Console Log. SMgr returns bytes read or error code (< 0)
				long aCnId;
				AConnection* apCn;
				AConsoleLogST* apLog;
				if (aLogSessionId <= 0 ||!cSessionMap.contains(aLogSessionId) || (aCnId=cSessionMap[aLogSessionId]) <= 0)
					aReqId = -AERR_SESSIONCLOSED;
				else if ((apCn = cOpenConnections[aCnId]) == NULL || (apLog=apCn->mpConsoleLog) == NULL || apLog->mLogEnable<=0)
					aReqId = -AERR_NOCONSOLELOG;
				else
				{	long aLogCnId = apLog->mLogConnectId;
					long aLogRqId = apLog->mLogRqId;
					if (aAbort)
					{	if (aLogCnId != 0)
						{	apLog->mLogConnectId = apLog->mLogRqId = 0;
							returnMsg(aLogCnId, aLogRqId, 0/*Status*/, geGetConsoleLog, aRetValue, aOut, NULL, 0, cMt, cMt);
							aOut = "true";
						}
						else
							aOut = "false";
					}
					else if ((aReqId = cpSessionMgr->getConsoleLog(aLogSessionId, aClear, aOut)) >= 0)
					{	aRetValue = aReqId;			// Number of bytes received
						aReqId = 0;

						// make sure no previous connection is waiting (aLogCnId)
						if (!(aLogCnId == 0 && aLogRqId == 0))
						{
							aReqId = -AERR_REQPENDING;
						}
						// Empty Log. If wait is set, wait for some output; else, return empty response.
						else if (aRetValue == 0 && aWait)
						{	getConsoleLog(iConnectId, aLogSessionId, iRqId);
							aReqId = 1;		// Avoid immediate return
						}
						// Wait over.  If this connection was waiting for output, the wait is over.
						else if (aLogCnId != 0 && aCnId == aLogCnId)
							apLog->mLogConnectId = apLog->mLogRqId = 0;
						// else, return nothing or the contents of the log
					}
				}
				break;
			}
			// GetLogonStats:	_ais|getlogonstats
			case geGetLogonStats:
			{	if ((aReqId = -getLogonStats(iConnectId, aOut)) != 0)
					aRetValue = -aReqId;
				break;
			}
			// GetRequestStats:	_ais|getrequeststats
			case geGetRequestStats:
			{	if ((aReqId = -getRequestStats(iConnectId, aOut)) != 0)
					aRetValue = -aReqId;
				break;
			}
			// GetSessionId:		_ais|getsessionid
			case geGetSessionId:
				aRetValue = apConnection->mSessionId;
				aOut = QString::number(aRetValue);
				break;
			// GetSessions:			_ais|getsessions|context|%s
			case geGetSessions:
				// getSessions returns number of sessions in aOut
				aRetValue = getSessions(iConnectId, irAmpmsg, aOut);
				break;
			// GetSessionStats:	_ais|getsessionstats
			case geGetSessionStats:
			{
				if ((aReqId = -getSessionStats(iConnectId, aOut)) != 0)
					aRetValue = -aReqId;
				break;
			}
			// GetSubscriptions:	_ais|getsubscriptions|context|%s
			case geGetSubscriptions:
				// getSubscriptions returns number of subscriptions in aOut
				aRetValue = getSubscriptions(iConnectId, irAmpmsg, aOut);
				break;
			// GetUsers: _ais|getusers
			case geGetUsers:
			{	if ((aReqId = -getUsers(iConnectId, aOut)) != 0)
					aRetValue = -aReqId;
				break;
			}
			// Logoff:			// _ais|logoff|connectid|%d
			case geLogoff:
				aRetValue = logoff(iConnectId, irAmpmsg);
				if (aRetValue < 0)
					aReqId = aRetValue;
				else
					aOut = QString::number(aRetValue);
				break;

			//	LogSysMsg:  _ais|logsysmsg|msg|%s|level|%d
			case geLogSysMsg:
				logSysMsg(irAmpmsg);
				aOut = "true";
				break;

			//	OpenConsoleLog: _ais|openconsolelog|clear|%s|redirect|%s|sessionid|%d|size|%d|startonnewline|%s
			case geOpenConsoleLog:
			{	bool aClear = true;				// Default to delete the existing log on open
				bool aRedirect = true;			// Default to redirect output if logging.
				long aSize = 0;					// Default to unlimited size
				bool aStartAtNewline = false;	// Default to start at first char of log.
				long aLogSessionId = 0;
				QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
				long aTknSz = aTkns.size();
				if (aTknSz >= 4)
				{	aClear = !(aTkns[3] == "0" || aTkns[3] == "false");
					if (aTknSz >= 6)
					{	aRedirect = !(aTkns[5] == "0" || aTkns[5] == "false");
						if (aTknSz >= 8)
						{	aLogSessionId = aTkns[7].toLong();
							if (aTknSz >= 10)
							{	aSize = aTkns[9].toInt();
								if (aTknSz >= 12)
									aStartAtNewline = !(aTkns[11] == "0" || aTkns[11] == "false");
							}
						}
					}
				}
				// LogSessionId. Default to current session
				if (aLogSessionId <= 0)
					aLogSessionId = aSessionId;
				if (aLogSessionId <= 0)
					aReqId = -AERR_SESSIONCLOSED;
				else if ((aReqId = cpSessionMgr->openConsoleLog(aClear, aRedirect, aLogSessionId, aSize, aStartAtNewline)) == 0)
					aOut = "true";
				break;
			}

			// OpenSession: _ais|opensession|context|%s|userid|%d|closemode|%s
			case geOpenSession:
				// openSession returns sessionId or error code if <0.
				if ((aReqId = openSession(iConnectId, irAmpmsg)) > 0)
				{	aSessionId = aRetValue = aReqId;
					aOut = QString::number(aSessionId);
					aReqId = 0;
				}				
				break;

			//  SetLogLvl:	_ais|setloglvl|logtype|%d|level|%d
			case geSetLogLvl:
				{	QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
				long aTknSz = aTkns.size();
				if (aTknSz >= 4)
				{	long aLogType = aTkns[3].toLong();
					AErrLvl aWarnLvl = geWarn;		// Default to warning level
					if (aTknSz >= 6)
						aWarnLvl = (AErrLvl)aTkns[5].toLong();
					gAis.mpLogMgr->setLogLvl((AReqType)aLogType, aWarnLvl, iConnectId);
					aOut = "true";
				}
				else
					aReqId = -AERR_FEWARGS;
				break;
			}

			// SetRules; _ais|setrules|rules|%s
			case geSetRules:
				if ((aRetValue = setRules(iConnectId, aSessionId, irAmpmsg)) < 0)
				{	aReqId = aRetValue;
					aRetValue = 0;
				}
				else
					aOut = QString::number(aRetValue);
				break;
	
			// SetSubscriptions: _ais|setsubscriptions|new|%s|old|%s
			case geSetSubscriptions:
				// setSubscriptions returns number of new subscriptions or error code if < 0
				if ((aRetValue = setSubscriptions(iConnectId, irAmpmsg)) < 0)
				{	aReqId = aRetValue;
					aRetValue = 0;
				}
				else
					aOut = QString::number(aRetValue);
				break;

			// UpdateUser: _ais|updateuser|userid|%d|username|%s|password|%s|securitylevel|%d|begindate|%s|enddate|%s|comment|%s
			case geUpdateUser:
			{
				if ((aReqId = -updateUser(iConnectId, irAmpmsg)) == 0)
					aOut = "true";
				else
					aRetValue = -aReqId;
				break;
			}
			default:
				aSubmit = true;		// Submit if built-in but not local
				break;
			}
		}
		else
		{	aReqType = geAmpMsg;
			aSubmit = true;			// Submit if not a built-in
		}
		if (aSubmit)
		{	// If no current session, open a session for commands that require a session
			if (aSessionId <= 0 && needsSession(aReqType))
			{	long aSecLvl = apConnection->mSecLvl;
				long aUsrId =  apConnection->mUsrId;
				QString& arContextName = apConnection->mContextName;
				ACloseMode aMode = geDefault;		// Set session default to context-specific default
				if ((aReqId = cpSessionMgr->openSession(aMode, arContextName, aUsrId, aSecLvl, 0/*SessionId*/)) >= 0)
				{	apConnection->mSessionId = aSessionId = aReqId;
					cSessionMap[aSessionId] = iConnectId;
				}
			}
			// Submit valid requests to the AisMgr
			if (aReqId >= 0)
			{	aReqId = cpAisMgr->submit(aSessionId, iRqId, irAmpmsg, iIsAsync, &aRetValue, aOut, aDisplay, ipData);
				ipData = NULL;		// Mark as in-use
			}
		}
	}
	// Clean up.
	if (ipData != NULL)
	{	free(ipData);
		qDebug("AAisSvr::submit(Ampmsg:%s,IsAsync:%d,ConnectId:%ld,RqId:%ld), Data dropped.", irAmpmsg.toAscii().data(), iIsAsync
		, iConnectId, iRqId);
	}
	// Return response to errors and to immediate requests.
	if (aReqId <= 0)
	{	QString aError(cMt);
		if (aReqId < 0)
		{	aError = gAis.mpErrMsgs[-aReqId];
			if (aOut.isEmpty())
				aOut = "false";
		}
		returnOutput(iConnectId, aSessionId, iRqId, -aReqId, aReqType, aRetValue, aOut, NULL, 0, aDisplay, aError, iIsAsync);
	}
	return aReqId;
}

/*	---------------------------------------------------------------------------------------------------------------------------
// timerEvent - Reimplement to catch cFlushTimer events
Args:
	ipTimerEvent	Not used
Returns:
	void
Notes:
 1. The flush timer occurs every AISSVR_FLUSHMSEC msec to push stale display output back to the client.  A long running
	request may not return for hours (even days).  This allows the client to receive periodic updates prior to receiving a
	result.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisSvr::timerEvent(QTimerEvent* ipTimerEvent)
{
    Q_UNUSED(ipTimerEvent);

    onFlush();
}

/*!
 * \brief Change a user in the system.
 *
 * \param[in] iConnectId Connection Id of the client that issued the command.
 * \param[in] irAmpmsg Original AMP message containing the message body.
 */
long AAisSvr::updateUser(long iConnectId, const QString& irAmpmsg)
{
	AConnection* apConnection = cOpenConnections[iConnectId];
	long aRet = 0;
	QString aError;

	if (apConnection->mSecLvl < geSuper)
	{
		aError = "0,AAisSvr.updateUser, No permission to update user";
		aRet = AERR_ACCESS;
	}
	else
	{
		QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
		long aTknSz = (long)aTkns.size();
		long aParamIdx = 0;
		long aUserId = 0;
		QString aUsername;
		QString aPassword;
		ASecLvl aSecurityLevel = geNada;
		QDate aEndDate;
		QString aComment;
		QString aTemp;

		for (long aIdx = 2; aIdx < aTknSz; aIdx+=2)
		{
			aParamIdx = aIdx + 1;
			if (aParamIdx < aTknSz)
			{
				aTemp = aTkns[aIdx];
				if (aTemp == "userid")
					aUserId = aTkns[aParamIdx].toLong();
				else if (aTemp == "username")
					aUsername = aTkns[aParamIdx];
				else if (aTemp == "password")
					aPassword = aTkns[aParamIdx];
				else if (aTemp == "securitylevel")
					aSecurityLevel = static_cast<ASecLvl>(aTkns[aParamIdx].toLong());
				else if (aTemp == "enddate")
					aEndDate = QDate::fromString(aTkns[aParamIdx],"MM/dd/yyyy");
				else if (aTemp == "comment")
					aComment = aTkns[aParamIdx];
				else
				{
					aError = QString("0,AAisSvr.updateUser, Unknown Parameter %s").arg(aTemp);
					LOGSYSMSG(geWarn, aError);
					aError.clear();
				}
			}
		}

		if (aUsername.isEmpty())
			aError = "0,AAisSvr.updateUser, One of the required parameters is empty";
		else
			aRet = cpUsrMgr->updateUser(aUserId, aUsername, aPassword, aSecurityLevel, aEndDate, aComment);
	}

	if (!aError.isEmpty())
		LOGSYSMSG(geWarn, aError);

	return aRet;
}
 
// end
