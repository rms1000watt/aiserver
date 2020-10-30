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

#ifndef AXMLSVR_H
#define AXMLSVR_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/axmlsvr.h
													AXmlSvr Specification					
CHANGE HISTORY
Version	 Date		Who		Change
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0112	10/27/2006	tlw		AXmlReqMap, AXmlSocketMap, AXmlConnectMap;. Convert to hash.
1.0104	 9/8/2006	tlw		C
1.0057	 3/18/2005	tlw		Update documentation.
												--------------- ---------------

	------------------------------------------------------------------------------------------------------------------------ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtNetwork/QTcpSocket>

#include "aglobals.h"			// ACHECK
#include "alogmgr.h"			// ALogMgr
#include "aissvr.h"				// AAisSvr

class QTcpServer;

//	-------------------------------------------------- DATA STRUCTURES --------------------------------------------------------
typedef struct
{	QString			mAct;				// Request speech act (request type if a built-in request)
	QString			mTarget;			// Request target Lambda ("_ais" if a built-in)
} AXmlReqSt;
typedef QHash<long, AXmlReqSt> AXmlReqMap;	// Key: msgId, Value: Request structure holding act and target

typedef struct
{	long			mBfrSz;				// Size of buffer in bytes
	AStringIntMap	mContextMap;		// Maps context name to connectId for this client
	long			mCurConnectId;		// Last ConnectId in-use
	long			mMsgLgth;			// Number of chars containing a message
	AXmlReqMap		mReqMap;			// Pending requests. Key: RqId, Value: Request structure holding act, target.
	QByteArray*		mpRcvBfr;			// -> allocated receive buffer for multi-packet reqs.
	QTcpSocket*		mpSocket;			// Socket used for TCP connection to client
	long			mRqId;				// Unique identifier for pending request (0 if none).
} AXmlSocketSt;

typedef QList<AXmlSocketSt*> AXmlSocketVec;		// Array of socket structures indexed by SocketId
typedef QHash<QTcpSocket*, long> AXmlSocketMap;	// Key: pSocket, value: SocketId
typedef QHash<long, long> AXmlConnectMap;		// Key: ConnectId, value: SocketId

//	------------------------------------------------ CLASS DEFINITIONS --------------------------------------------------------
/*!
\brief XmlSvr implements a general purpose sockets layer for transferring XML documents over the internet.

In particular, XmlSvr supports Macromedia's Flash clients.

\par Requests.
Input requests are XML documents with a null or ^A termination on the end. See onSubmit for more details.
Requests are of the form:
\verbatim
<amp act="mySpeechAct" context="myContext" mode="async" target="myTargetLambda">
    <myArg>...</myArg>
</amp>^A.
\endverbatim

\par Responses.
Returned messages are XML documents with a ^A termination on the end.   See returnOutput for more details
Responses are of the form:
\verbatim
<amp act="mySpeechAct" status="123" target="myTargetLambda" xtype="return">
  <result>...</result>
</amp>^A

\par Notes:
-# Requests are translated by submit from XML to an AMP message of the form:
"myTargetLambda|mSpeechAct|myArg|..."
-# The requested target and act are saved in the SocketSt mReqMap keyed by the RqId (server-wide unique ID).
-# xid is not currently handled. It could be added to AXmlReqSt, and returned with the act and target to returnOutput.
-# The characters &, ', ", <, > are replaced by their predefined entities in the content of the child entities.
-# target and act attributes are required if the type is msg or return
-# xtype may be:
\verbatim
	msg - indicates a request (the default)
	return - indicates a response to a request
\endverbatim

\par Data Structures.
-# The XML Server is responsible for maintaining a SocketId.  Initially, it is set to 0.  When the user first logs on,
openSocket() is called to acquire a slot in the cOpenSockets list.
-# The SocketId is the index into the cOpenSockets list.  Entries in the cOpenSockets list hold important information about
the current connection state. An entry in cSocketMap maps apSocket to the socketId.  There is one socketId per TCP/IP
connection to a client machine.  A socket can support more than one connection.  A call into AAisSvr opens a new
connection for each new connection.   The attributes of the root element includes a context name.  A separate connection
is established for each context.
-# A connection is established by a call into the AAisSvr when the user logs on.  The AAisSvr returns a connect ID from logon.
The connect ID for each context is saved in a contextMap.  There is one context map entry for each socket.
-# The apSocket is provided by the TCP connection on each remote request which may be used to look up the SocketId.  The
SocketId and current context name are used to look up the current ConnectId.  The ConnectId is passed with the request to
the AisSvr.
-# The zeroth slot is reserved for use as a null socket.

\par Sockets and TCP Connections.
There is one socket for every TCP/IP connection to a client machine.  There may be more than one connection on a socket
(if requests on one socket specify more than one context in their root tag).  To thwart bad guys, no slot is allocated in
cOpenSockets and no call is made into aisSvr to open a connection until the user logs on.   In the meantime, users can submit
requests using a null socketId and a null connectId.  No state is maintained and all requests must return immediately (no
requests into the engine are allowed).  All requests on a null connection have minimal privileges.

 */
class AXmlSvr : public QObject, public AReturnRcvr
{
	Q_OBJECT
public:
	AXmlSvr(ushort iPort, const QString& irContextName, QObject* ipParent=0);
	~AXmlSvr();
	virtual bool connectionClosed(long iConnectId);
	virtual void returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue
				 , const QString& irAisOut, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError
				 , const QString iClientData = QString::null);
	virtual void setContextName(long iConnectId, const QString& irContextName);

	long		submitInProc(AReturnRcvr* ipReturnRcvr, const QString& irAmpmsg, QString& orAisOut, long* opStatus);
signals:
	void		endConnectSgnl();
	void		newConnectSgnl();

private slots:
	void		onConnected();
	void		onDisconnected();
	void		onError(QAbstractSocket::SocketError iCode);
	void		onNewConnection();
	void		onSubmit();

private:
	void		closeSocket(QTcpSocket* ipSocket);
	long		returnFile(long iConnectId, const QString& irFileSpec, QTcpSocket* ipSocket);
	long		openSocket(long iConnectId, const QString& irContextName, QTcpSocket* ipSocket);
	void		submit(const QString& iXmlDoc, AXmlSocketSt* ipSockSt, QTcpSocket* ipSocket);

	AXmlConnectMap	cConnectionMap;		// Map from ConnectId to SocketId
	QString			cDefaultContextName;// Default context that created this server
	QString			cMt;				// Empty (not null) arg placeholder.
	AXmlSocketVec	cOpenSockets;		// List of active XmlSocketSt structures
	ushort			cPort;				// Server listens on this port. 0 if disabled
	AXmlSocketMap	cSocketMap;			// Key: pSocket, Value: SocketId
	QTcpServer*		cpXmlServer;		// Listens on the XML socket.
};

#endif //AXMLSVR_H
