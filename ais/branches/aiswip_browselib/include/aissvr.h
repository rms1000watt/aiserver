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

#ifndef AISSVR_H
#define AISSVR_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/aissvr.h
													AIS Server

CHANGE HISTORY
Version	Date		Who		Change
4.002	 9/28/2008	tlw		CR128. Add subscribe to allow a connection subscribe to a session.
3.2005	 2/18/2008	fchua	Added getConnectionStats, getLogonStats, getRequestStats, getSessionStats. 
3.2001	 1/27/2008	fchua	Added support for User Management functions.
3.1005	12/16/2007	fchua	connectSession. Added orNewSessionId argument.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0118	12/12/2006	tlw		submit. Add serial binary stream argument.
1.0116	12/4/2006	tlw		onQuit. Add slot to handle shutdown of contexts.
1.0113	11/7/2006	tlw		APortSt. Add a port structure to cOpenPorts. Add destructors to free all allocated resources.
1.0057	3/18/2005	tlw		Update documentation
1.0044	7/6/2004	tlw		Redirect all responses thru returnOutput.
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------- IMPORTS -----------------------------------------------------------
#include <QtCore/QBasicTimer>
#include <QtCore/QMultiHash>

#include "aglobals.h"				// ACHECK
#include "alogmgr.h"				// ALogMgr
#include "ausrmgr.h"				// AUsrMgr
#include "../asessionmgr/asessionmgr.h"
#include "../afilewatcher/afilewatcher.h"
class AUsrMgr;
class AHttpSvr;

// ------------------------------------------------------ DEFINITIONS ---------------------------------------------------------
// cOpenConnections contains three maps of subscriptions:
#define RULES		0
#define EXCLUSIONS	1
#define SESSIONS	2

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
// Subscriptions.
// cSubscriptions - An array of 3 ASubscriptionMaps for RULES, EXCLUSIONS and SESSIONS.
// ASubscriptionMap - A multi-map. Key: SubscriptionId. Values one or more Subscribers (ConnectIds) .
// SubscriptionID - a RuleID (for RULE lists) or a SessionId (for EXCLUSION and SESSION lists).
// RuleID - ContextId << AISSVR_SHIFT | Protocol ID.  ContextId is shifted left 3 bits.
// For example, when a Session subscribes to a connection, a value is added to the map:
//		cSubscriptions[SESSIONS][aSessionId] = aConnectId

// The methods that add, fetch and remove elements from this map are:
//	getSubscribers, getSubscriptions, setSubscriptions, removeSubscribers, removeSubscriptions.
typedef QMultiHash<long, long> ASubscriptionMap;		// Key: SubscriptionId, Values: ConnectId
typedef QList<long> AMemberList;						// List of subscribers (connect IDs) or subscriptions.

// ConsoleLog. Console log state for a connection whose console output is being logged.
typedef struct
{	long			mLogConnectId;		// ID of connection waiting on more console output
	long			mLogEnable;			// <0 Log closed, =0 Log suspended, >0 Log enabled
	bool			mLogRedirect;		// false - send output to client, true - if enabled, just log output
	long			mLogRqId;			// ID of getConsoleLog request waiting on console output
} AConsoleLogST;

// Open Ports
typedef struct
{
	long			mProtocolId;		// Protocol ID (see globals.h)
	AReturnRcvr*	mpProtocolSvr;		// -> protocol receiver for this port
}	APortSt;
typedef	QHash<ushort, APortSt> APortsMap;	// Key= port, value=APortSt

// Connection. One Connection state is created for each connection to a client.
class AConnection : public QObject
{
	Q_OBJECT
public:
	AConnection(QObject* ipParent, const char* ipName);
	~AConnection();
	long				mConnectId;			// 0 if empty entry; else, index into cOpenConnections
	QString				mComment;			// Comment returned from password file
	AConsoleLogST*		mpConsoleLog;		// Current console log state if console logging opened
	QString				mContextName;		// Context name set by logon, openSession
	long				mDefaultCloseWait;	// Default wait-to-close timeout period [secs]
	long				mEndDay;			// User's context-specific expiration date (days since 1/1/2K)
	long				mIdleSecs;			// Maximum time [secs] between requests (0=>infinite)
	long				mLastAccess;		// Time (secs since 2K) of last access
	long				mMaxEntries;		// Maximum number of entries
	ACloseMode			mMode;				// Connection state: geDefault(closed), geOpen, geSoft, geFirm, geHard
	long				mProtocolId;		// Type of protocol AISSVR_APP, AISSVR_HTTP, AISSVR_XML
	AReturnRcvr*		mpProtocolRcvr;		// -> protocol server for this connection (NULL if disconnected)
	long				mSecLvl;			// User's context-specific security level
	long				mSessionId;			// Current session for this connection.
	long				mSysEndDay;			// User's system-wide expiration date (days since 1/1/2000)
	long				mSysSecLvl;			// User's system-wide security level
	// ASubscriptionMap	mSubscriptions[3];	// Lists of subscriptions for this client, Rules, Exclusions, Sessions
	QTimer*				mpTimer;			// Timer to wait for close connection timeout period
	long				mUsrId;				// Index into user's account (0 if structure is not in-use)

signals:
	void			closeConnection(long iConnectId);
private slots:
	void			onTimeout();
};
typedef QList<AConnection*> AConnections;

/*!
\brief AAisSvr holds the common elements that are used by all the protocol servers.

One instance of this class is created for each server.  The AAisSvr class provides a central interface to which each
protocol submits all requests.  AppSvr, AXmlSvr, AHttpSvr all use this interface for every incoming request for any client.
\par Requests.
All requests are converted from whatever format the protocol server supports into an AMP message.  An AMP message is a DEL
separated sequence of name value pairs starting with a target Lambda/speech act pair. Some built-in messages of the form
_ais| are handled in AisSvr with the results returned back via returnOutput. Other messages are handed on to AAisMgr for
submission to SessionMgr.
- All incoming requests, except logon, are routed thru submit.  The returned value from submit is the request ID for all
requests.  Http and XML servers do not return the request ID to the client.	Other (immediate) requests return directly via
returnOutput.  If the client is local (in-proc), the returnOutput of the client is called directly.
- Otherwise, the response is returned on a socket connection to the client.  If the request Id is returned at all, it is
returned in a header.
- If an immediate error is detected, an error status code is returned via returnMsg..
- It is up to the remote client to either wait for a response or not (depending upon the application).

\par Data Structures.
AisSvr is the keeper of the connection structure.  There is one connection structure for every remote or local connection that
has been established with this server.  A connection is associated with a specific user, context, and session.  The connection
is established when a protocol server calls openConnection (usually at logon).
\par Async Returns.
AisMgr passes asynchronous events from the engine back to Ais Server via calls to returnOutput.  AisSvr diverts selected
messages to the logging facility for broadcast to subscribers and dispatches the returned payload back to the appropriate
protocol server by calling the protocol server's returnOutput function.

Returned payloads are typically DEL delimited strings that are formatted by each protocol server into a format that is
prescribed by the protocol.  For example, AXmlSvr converts the return into an XML document to be returned to the client.
\par OpenSession.
- After the user logs on, the client may call openSession or it may be called automatically when the need arises.  The
session ID is included	by the engine in all returned output.
- Note that a user can log off or a connection can be lost while some session output is still pending.

\par ReturnOutput.
- Returned output can be from an immediate response, pushed output (e.g.,writeln), or a returned result.
- All returns have a connect Id associated with the message which identifies the destination of the output as noted above.
- The returned output is usually a string.  If several items are returned, the items are delimited by a DEL character.
Since the client knows the number of args returned for any given call, the last arg can contain DEL characters.  The
returned value is not an AMP message.
- Immediate and pushed output have a request ID of zero.  If the request ID is positive, information saved from the incoming
request (if any) is added to the response.
- The message is packaged for the specific protocol.  XML clients receive XML documents. HTTP clients receive HTTP formatted
messages, etc. 

\par Close Connection.
CloseMode. When the last connection is closed, the protocol server calls AisSvr's closeConnection with "CloseMode" and
"CloseWait" arguments.  CloseMode specifies one of the following:
- geSoft - Close session to new requests but allow pending requests and executing requests to complete.
- geFirm - Close session to new requests, clear all pending requests, but allow executing request, if any, to complete.
- geHard - Close session to new request, clear all pending requests including an executing request for this session.
- geDefault - Close using the default close mode for this session.

\par CloseWait.
CloseWait is the timeout period in secs before closing an connection.  During this period, any session
associated with this connection remains fully operational.  Console output from the session may be returned to subscribers or
it may be placed in a console log for later retrieval, but console output and any results returned are not returned directly
to the client because the connection has been lost.  If no sessions are open on this connection, CloseWait is ignored.  If an
HTTP client reconnects to AIS with the same cookie, the client is reconnected to this session.  If no new requests are received
before the end of the timeout period, the session is closed. CloseWait may be:
/verbatim
-2	Use default value (see discussion below)
-1	Wait forever.
 0	Close immediately (no wait)
>0  Wait for CloseWait secs and then close.
/endverbatim

\par DefaultCloseMode.
When a session is first established by openSession, a DefaultCloseMode is set in the ASession data structure
for this session. The DefaultCloseMode can be any value listed above for CloseMode except geDefault.  If closeSession is called
with CloseMode set to geDefault, CloseMode is set to the DefaultCloseMode value.  The DefaultCloseMode is set in the following
order:
-# Hard-Coded. DefaultCloseMode is set to AIS_CLOSEMODE and DefaultCloseWait is set to AIS_CLOSEWAIT.
-# Register Context. These parameters are set to the values of the context-specific parameters, "CloseMode" and "CloseWait".
-# OpenSession. DefaultCloseMode is set in open session call, "closemode".
Note that a session may be opened automatically. In this case the context-specific parameters are used.

\par DefaultCloseWait.
When a connection is first established by a logon, a DefaultCloseWait period is set in the AConnectionSt
structure.  The DefaultCloseWait value can be any integer value listed above for CloseWait, except -2, and it is set in the
same fashion as for DefaultCloseMode (except for the openSession step).  If a connection is closed with CloseWait set to -2,
the CloseWait is set to the DefaultCloseWait value (which is not -2).

SessionMode.  All open sessions have a mode assigned to them.  The mode is one the following:
\verbatim
 geDefault	- Closed
 geOpen     - Active
 geSoft     - In the process of being closed in "soft" mode
 geFirm     - In the process of being closed in "firm" mode
 geHard     - In the process of being closed in "hard" mode
\endverbatim

\par Errors.
- Errors are logged when they occur.  An error status is returned back to the caller.
- Error returns to XML clients are returned with the attribute type="error" in the root element tag.
- Error returns to HTTP clients are returned as plain text with Error + DEL prepended to the message. Java script may be
resident to intercept and process the message.
- Error returns to APP clients contain a header whose request ID is negative.
 */
class AAisSvr : public QObject
{
	Q_OBJECT
public:
	 AAisSvr(AAisMgr* ipAisMgr, ASessionManager* ipSessionMgr, AUsrMgr* ipUsrMgr);
	~AAisSvr();
	void		closeConnection(long iConnectId, ACloseMode iCloseMode, long iWait);
	void		enableConsoleLog(long iSessionId, long iEnable);
	QString		getContextName(long iSessionId, long iContextId);
	AStringMap*	getContextParams(const QString& irContextName, long* opContextId);
	long		getFileSpec(long iConnectId, long iProtocolId,const QString& irContextName, QString& iorFileSpec);
	long		getUsrId(long iConnectId);
	QString		getUsrName(long iUsrId);
	long		logon(AReturnRcvr* ipProtocolRcvr, long iDefaultCloseWait, long iProtocolId, const QString& irUsrName
				, const QString& irPasswd, const QString& irContextName, long* opConnectId, QString& orAisOut);
	long		openConnection(long iConnectId, const QString& irContextName);
	void		openConsoleLog(bool iRedirect, long iSessionId);
	long		openPort(long iProtocol, ushort iPort, const QString& irContextName);
	void		returnMsg(long iConnectId, long iRqId, long iStatus, AReqType iReqType,long iRetValue,const QString& irAisOut
				, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError);
	void		returnOutput(long iConnectId, long iSessionId, long iRqId, long iStatus, AReqType iReqType, long iRetValue
				, const QString& irAisOut, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError
				, bool iIsAsync, const QString iClientData = QString());
	long		submit(const QString& irAmpmsg, bool iIsAsync, long iConnectId, long iRqId, char* ipData);
	void		subscribe(long iCurSessionId, long iSessionId, bool iRemove);

	long		mRqId;	//!< Incrementing integer. A unique ID created in each protocol server and returned by AisMgr with result.

protected:
	virtual void timerEvent(QTimerEvent* ipTimerEvent);

public slots:
	void		onQuit();

private slots:
	void		onFlush();
	void		onCloseConnection(long iConnectId);

private:
	long		addUser(long iConnectId, const QString& irAmpmsg);
	void		ccSubscribers(long iSessionId, const QString& irDisplay);
	void		closeContext(long iContextId);
	bool		closePort(ushort iPort);
	long		closeSession(long iSessionId);
	long		connectSession(long iConnectId, long iRqId, const QString& irAmpmsg, long& orNewSessionId);
	long		deleteUser(long iConnectId, const QString& irAmpmsg);
	void		dumpArray(AMemberList& irAy);
	bool		findSubscriber(long iList, long iSubscriberId, long iSubscriptionId, bool iDelete);
	long		getConnectionStats(long iConnectId, QString& orOut);
	void		getConsoleLog(long iConnectId, long iSessionId, long iRqId);
	QString		getCurUsrName(long iConnectId);
	long		getLogonStats(long iConnectId, QString& orOut);
	long		getProtocolId(const QString& irProtocolName);
	long		getRequestStats(long iConnectId, QString& orOut);
	long		getSecLvl(long iConnectId);
	long		getSessions(long iConnectId, const QString& irAmpmsg, QString& orOut);
	long		getSessionStats(long iConnectId, QString& orOut);
	AMemberList	getSubscribers(long iList, long iSubscriptionId);
	AMemberList	getSubscribers(long iSessionId);
	AMemberList	getSubscriptions(long iConnectId, long iList);
	long		getSubscriptions(long iConnectId, const QString& irAmpmsg, QString& orOut);
	long		getUsers(long iConnectId, QString& orOut);
	bool		isSubscriber(long iList, long iSubscriberId, long iSubscriptionId);
	bool		isSubscribed(long iConnectId, long iSessionId, long iSubscriptionId);
	long		logoff(long iConnectId, const QString& irAmpmsg);
	void		logSysMsg(const QString& irAmpmsg);
	bool		needsSession(AReqType iReqType);
	long		newConnection(AReturnRcvr* ipProtocolRcvr, long iProtocolId, long iUsrId);
	long		openSession(long iConnectId, const QString& irAmpmsg);
	bool		removeSubscriptions(long iList, long iSubscriptionId);
	bool		removeSubscriber(long iList, long iSubscriberId, long iSubscriptionId);
	bool		removeSubscribers(long iList, long iConnectId);
	long		setRules(long iConnectId, long iSessionId, const QString& irAmpmsg);
	void		setSubscription(long iList, AMemberList iSubscribers, long iSubscriptionId);
	long		setSubscriptions(long iConnectId, const QString& irAmpmsg);
	long		today() { return cY2K.daysTo(QDate::currentDate());};
	long		updateUser(long iConnectId, const QString& irAmpmsg);

	bool				_trace;				// Temporary hack to echo returnOutput messages
	AAisMgr*			cpAisMgr;
	ASessionManager*	cpSessionMgr;		// -> session manager for those cases that go directly to the session manager.
	QBasicTimer			cFlushTimer;		// Timer for flushing display buffers (about 2 per sec)
	long				cIdleSecs;			// Maximum time [secs] w/ no activity before timeout
	QString				cMt;				// Empty (not null) placeholder
	AConnections		cOpenConnections;	// Array of open connections. Indexed by connect ID
	APortsMap			cOpenPorts;			// Key: port number, Value: APortSt
	ASubscriptionMap	cSubscriptions[3];	// Array of subscription maps, one each for RULES, EXCLUSIONS, SESSIONS
	ALongKeyMap			cSessionMap;		// Key: SessionId, Value: ConnectId
	AUsrMgr*			cpUsrMgr;			// Reference to the global user account facility
	QDate				cY2K;				// 1/1/2000
	AFileWatcher*		cpFileWatcher;		// File watcher class for the aissvr
};

/*!
\brief getContextName - Return the context name for for this Session or for this context ID.

\param iSessionId - The session ID of this session may be used to look up its associated context.
\param iContextId - The slot assigned to this context in cOpenContexts.
\return The context name.
 */
inline  QString AAisSvr::getContextName(long iSessionId, long iContextId)
{
	return cpSessionMgr->getContextName(iSessionId, iContextId);
}

/*!
\brief getContextParams - Get the context-specific parameters for the specified context

\param irContextName - The name of this context. Every context must have a unique name.
\param opContextId - The address of a place to return the index into the cOpenContexts array.
\return A map. Key is the parameter name (a string), Value is the parameter value (a string).
 */
inline AStringMap* AAisSvr::getContextParams(const QString& irContextName, long* opContextId)
{
	return cpSessionMgr->getContextParams(irContextName, opContextId);
}

inline QString AAisSvr::getCurUsrName(long iConnectId)
{
	QString aUsrName;
	AConnection* apConnection;
	if (iConnectId > 0 && iConnectId < cOpenConnections.size() &&
			(apConnection = cOpenConnections[iConnectId])->mpProtocolRcvr != NULL && apConnection->mUsrId > 0)
		aUsrName = cpUsrMgr->getUsrName(apConnection->mUsrId);

	return aUsrName;
}

inline long AAisSvr::getSecLvl(long iConnectId)
{
	long aSecLvl = -1;
	if (iConnectId > 0 && iConnectId < cOpenConnections.size())
		aSecLvl = cOpenConnections[iConnectId]->mSecLvl;
	return aSecLvl;
}

/*!
\brief getUsrId - Return the ID assigned to this user.

The userId is persistent integer assigned to each user on this server.

\param iConnectId - The index into cOpenConnections for this connection.
\return aUsrId
 */
inline long AAisSvr::getUsrId(long iConnectId)
{
	long aUsrId = 0;
	if (iConnectId > 0 && iConnectId < cOpenConnections.size())
		aUsrId = cOpenConnections[iConnectId]->mUsrId;
	return aUsrId;
}

/*!
\brief getUsrName - Return the logon name for this user.  The logon name is a persistent string assigned to each user
on this server.

\param iUsrId - A unique integer assigned to each user that may logon to this server.
\return The user's logon name.
 */
inline QString AAisSvr::getUsrName(long iUsrId)
{
	return cpUsrMgr->getUsrName(iUsrId);
}
#endif // AISSVR_H

