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
aisdev/appsvr/appsvr.cpp
											App Server

Appsvr implements a general purpose sockets layer for transferring app server documents over
the internet.

CHANGE HISTORY
Version	 Date		Who		Change
4.0003   7/01/2008	fchua	[CR-118] Added function isInProcess to check if a connection is In-Process.
3.2002	 2/04/2008	fchua	openSocket. Added setting of InProcess client pointer.
3.1004	11/2/2007	fchua	Fixed parameter to LOGREQHDRMSG. Converted to QString first before appending '\n'.
3.1003	10/29/2007	fchua	Fixed recursion bug in logging of request header in retOutput.
3.1002	10/16/2007	fchua	onSubmit. Fixed bug in handling of partial response. Updated Doxygen documentation.
2.0001	12/29/2006	tlw		retFile. Add DataSize to output prefix. Remove null at end of binary data.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0118	12/12/2006	tlw		onSubmit, submit. Add serial binary stream argument.
1.0112	10/27/2006	tlw		cOpenSockets. Reclaim AAppSocketSt on destruct.
1.0108	 9/30/2006	tlw		getArg. Add arguments to trim and lower case.
1.0104	 9/8/2006	tlw		Null terminate received messages.
1.0056	 3/10/2005	tlw		Update documentation.
							---------------------------------
DOCUMENTATION
1.	See appsvrnotes.txt for project setup information.
2.	See ../include/appsvr.h for class specification.	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------- IMPORTS -----------------------------------------------------------
#include <QtCore/QtDebug>
#include <QtCore/QTextStream>
#include <QtNetwork/QTcpServer>

#include "appsvr.h"				// AAppSvr
#include "aisoutput.h"			// AISMGR_OUT_SIZE
#include "asessionmgrinterface.h"// ASessionMgrInterface
#include "aismgr.h"				// AAisMgr
#include "autilities.h"			// AUtil

//	------------------------------------------------- DEFINED CONSTANTS -------------------------------------------------------

//	-------------------------------------------------- MEMBER FUNCTIONS -------------------------------------------------------
/*!
 * \brief Constructor initializes QTcpServer and the openSockets list.
 *
 * \param[in] iPort AAppSvr listens on this socket for incoming requests.
 * \param[in] irContextName The default context to be used if request does not specify a context.
 * \param[in] ipParent The parent widget that created this instance of AAppSvr.
 */
AAppSvr::AAppSvr(ushort iPort, const QString& irContextName, QObject* ipParent)
	: QObject(ipParent), cDefaultContextName(irContextName), cMt(""), cPort(iPort)
{
	// AppServer. Set up server to listen on the App port.
	cpAppServer = new QTcpServer(this);
	if (!cpAppServer->listen(QHostAddress(QHostAddress::Any), iPort))
	{	// PENDING AAppSvr::AAppSvr(). Unable to start the server. Log meessage. Log the error.
		qDebug("AHttpSvr, Unable to start the server, %s", cpAppServer->errorString().toLatin1().data());
	}
	else
		connect(cpAppServer, SIGNAL(newConnection()), this, SLOT(onNewConnection()));

	// Create one "null" entry in openSockets list.
	AAppSocketSt* apSockSt = new AAppSocketSt();
	apSockSt->mContextMap.clear();		// Key: Context Name, Value: connectId
	apSockSt->mCurConnectId = 0;		// ConnectId assigned to this client by AisSvr
	apSockSt->mpDataBfr = NULL;			// -> binary data buffer holding serialized object
	apSockSt->mDataLgth = 0;			// Number of data bytes received so far.
	apSockSt->mDataSize = 0;			// Number of bytes of binary data expected.
	apSockSt->mpInProcClient = NULL;	// -> In-proc client iff in-process client
	apSockSt->mRcvBfr.truncate(0);		// Receive buffer for multi-packet requests
	apSockSt->mRcvLgth = 0;				// Number of chars in receive buffer
	apSockSt->mRcvSize = 0;				// Size of the receive buffer (number of expected bytes)
	apSockSt->mReqMap.clear();			// Saved incoming request Id from client.
	apSockSt->mpSocket = (QTcpSocket*)1;	// -> socket for remote connections
	cOpenSockets.insert(0, apSockSt);
	gAis.mpInProcSvr = this;
}

/*!
 * \brief Destructor Free allocated resources.
 *
 * \note
 * -# AAppSvr has remote ownership of AAppSocketSt referents.
 * -# Since instances of AAppSvr are never assigned copied, or passed by value, the copy constructor and assignment operator
 * are omitted here.  See C++ FAQS chap 30 for more info.
 */
AAppSvr::~AAppSvr()
{
	QTcpSocket* apSocket;
	AAppSocketSt* apSockSt;
	for (long aSocketId = 0; aSocketId < cOpenSockets.size(); ++aSocketId)
	{	if ((apSockSt = cOpenSockets[aSocketId]) != NULL)
		{	apSockSt->mReqMap.clear();
			if ((apSocket = apSockSt->mpSocket) > (QTcpSocket*)1)
			{	apSocket->abort();
				apSocket->deleteLater();
			}
			delete apSockSt;
		}
	}
	cConnectionMap.clear();
	cOpenSockets.clear();
	cSocketMap.clear();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AAppSvr()");
#endif
}

/*!
 * \brief Connection to a context was closed. Mop up.
 *
 * \param[in] iConnectId Index into cConnectionMap to identify client.
 * \return false
 *
 * \note
 * -# Called by AisSvr.closeConnection if this or some other protocol server closes connection.
 * -# Implements a pure virtual function inherited from the AReturnRcvr abstract base class.
 * -# When closing a remote connection, the remote client will be notified.
 * -# See onDisconnected which calls into AisSvr.closeConnection.
 */
bool AAppSvr::connectionClosed(long iConnectId)
{
	long aConnectId, aSocketId = 0;
	QTcpSocket* apSocket = NULL;
	AReturnRcvr* apInProcClient = NULL;
	AAppSocketSt* apSockSt = NULL;
	bool aFoundIt = false;
	if (iConnectId > 0 && cConnectionMap.contains(iConnectId) && (aSocketId = cConnectionMap.value(iConnectId)) > 0
	&& aSocketId < cOpenSockets.size() && (apSockSt = cOpenSockets[aSocketId]) != NULL)
	{	// Clear all entries for this socket from connection maps
		if ((apSocket = apSockSt->mpSocket) != NULL)
			cSocketMap.remove(apSocket);
		AStringIntMap& arMap = apSockSt->mContextMap;
		AStringIntMap::Iterator apIt;
		for (apIt = arMap.begin(); apIt != arMap.end(); ++apIt)
		{	aConnectId = *apIt;
			if (cConnectionMap.contains(aConnectId))
				cConnectionMap.remove(aConnectId);
		}
		// Mark entry in cOpenSockets as unused
		apInProcClient = apSockSt->mpInProcClient;
		apSockSt->mCurConnectId = 0;
		apSockSt->mContextMap.clear();
		apSockSt->mpInProcClient = NULL;
		apSockSt->mRcvBfr.truncate(0);
		apSockSt->mRcvLgth = apSockSt->mRcvSize = 0;
		apSockSt->mReqMap.clear();
		apSockSt->mpSocket = NULL;
		if (apSockSt->mpDataBfr != NULL)
			free(apSockSt->mpDataBfr);

		// Close remote socket TCP/IP connection
		if (apSocket != NULL)
		{	apSocket->close();
			// delete apSocket;
		}
		if (apInProcClient != NULL)
			apInProcClient->connectionClosed(iConnectId);
		aFoundIt = true;
	}
	return aFoundIt;
}

/*!
 * \brief Returns true if the connection is In-Process.
 */
bool AAppSvr::isInProcess(long iConnectId)
{
	int aSocketId = 0;
	AAppSocketSt *aSocketSt = NULL;
	bool aRet = false;

	// check if connection id exists
	if (cConnectionMap.contains(iConnectId))
	{
		// get the socket id
		aSocketId = cConnectionMap[iConnectId];
		if (aSocketId < cOpenSockets.size())
		{
			// get the socket data structure
			aSocketSt = cOpenSockets[aSocketId];
			if (aSocketSt != NULL)
			{
				// check if In-Process client is defined
				aRet = (aSocketSt->mpInProcClient != NULL);
			}
		}
	}
	return aRet;
}

/*!
 * \brief Catches connected signal after connectToHost is called.
 *
 * \note
 * Not much information is available at this point.
 */
void AAppSvr::onConnected()
{
}

/*!
 * \brief Close connection on signal from QSocket.
 *
 * \note
 * Maybe should squawk if connect ID is not valid.
 */
void AAppSvr::onConnectionClosed()
{
	// Close Connection. AisSvr will look up default close mode
	long aSocketId, aSz = cOpenSockets.size();
	QTcpSocket* apSocket = (QTcpSocket*)sender();
	ACHECK(apSocket != NULL);
	AAppSocketSt* apSockSt = NULL;
	if (cSocketMap.contains(apSocket) && (aSocketId = cSocketMap.value(apSocket)) > 0 && aSocketId < aSz
	&& (apSockSt = cOpenSockets[aSocketId]) != NULL && !apSockSt->mContextMap.isEmpty())
	{	gAis.mpAisSvr->closeConnection(apSockSt->mCurConnectId, geDefault, 0/*Wait*/);
		apSockSt->mReqMap.clear();
		if ((apSocket = apSockSt->mpSocket) > (QTcpSocket*)1)
		{	apSocket->abort();
			apSocket->deleteLater();
		}
		delete apSockSt;
	}
	else	// Just shut down the socket
		apSocket->close();
}

/*!
 * \brief Called on signal from QSocket when socket is unexpectedly closed.
 *
 * \todo Complete implementation.
 */
void AAppSvr::onDisconnected()
{
	// PENDING AAppSvr::onDisconnected(). Free socket structure if any. notify interested parties including subscribers.
}

/*!
 * \brief Called on signal from QSocket when socket error encountered.
 *
 * \param[in] iCode QSocket error code.
 */
void AAppSvr::onError(QAbstractSocket::SocketError iCode)
{
	if (iCode != QAbstractSocket::RemoteHostClosedError)
	{	QTcpSocket* apSocket = (QTcpSocket*)sender();
		qDebug() << "AAppSvr::onError(), Encountered TCP/IP error, " << apSocket->errorString() << " Code: " << iCode;
	}
}

/*!
 * \brief Establish a new TCP/IP socket connection.
 *
 * \note
 * Required virtual function by the parent class QServerSocket.
 */
void AAppSvr::onNewConnection()
{
	if (cPort <= 0) return;
	// Socket. Construct a QTcpSocket object when a new connection is established:
	QTcpSocket* apSocket = cpAppServer->nextPendingConnection();

	connect(apSocket, SIGNAL(connected()), this, SLOT(onConnected()));
	connect(apSocket, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
	connect(apSocket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(onError(QAbstractSocket::SocketError)));
	connect(apSocket, SIGNAL(readyRead()), this, SLOT(onSubmit()));
}

/*!
 * \brief Retrieve transmission arriving from TCP/IP connection.
 *
 * \note
 * -# Msg format: ReqSz|DataSz|RqId|!context|%s|targetLambda|speechAct|arg|%s...\01binary data...\0
 * Example: 0000000006|0000000000|12|ais\01
 * -# The prefix consists of ReqSz|DataSz|.
 * -# ReqSz and DataSz are a fixed width, zero filled, base 10, numeric strings.
 * -# The value of ReqSz is the length of the request (everything after the prefix up to, but not including, \01).
 * -# The value DataSz is the length of the binary data (everything after \01 up to, but not including the null on the end).
 * -# RqId is a numeric string.
 * -# !context|%s pair is optional where %s represents the context name. Not currently used.
 * -# BinaryData...\0 is an optional binary array followed by a null.  If it is missing DataSz is 0.
 * -# It is possible to receive a complete request, a partial request or multiple requests.
 * -# If a complete request has been fetched, it is submitted to the engine for consideration; otherwise partial requests are
 * saved for later processing.
 * -# A socket structure is allocated upon a successful logon. An entry is added to cSocket map to relate the pSocket to the
 * index (socket ID) into this array of structures and an entry is added to the cConnection map to relate the connect ID to
 * the socket ID.
 * -# When checking for a partial request the socket map is used to locate the socket structure which holds the partial request
 * buffer.
 * -# On returning a request, the connect ID is used to look up the socket ID in the connect map.
 */
void AAppSvr::onSubmit()
{
	// Socket. Initialize, get ptr to socket for this TCP/IP connection.
	QString aAmpmsg;				// Holds request.
	QTcpSocket* apSocket = (QTcpSocket*)sender();	// Ptr to socket for this connection
	ACHECK(apSocket != NULL && cPort > 0);

	// Initialize. Look up index to socket structure. Set current state.
	char* apDataBfr = NULL;			// -> binary data buffer holding serialized object
	long aDataLgth = 0;				// Number of data bytes received so far.
	long aDataSize = 0;				// Number of bytes of binary data expected.
	long aLgth;						// Length of next arg in the message including separator, if any.
	long aNeed;						// Number of bytes needed to fill a buffer.
	QByteArray aRcvBfr;				// Buffer to hold partial responses received from the client
	char* apRcvBfr = NULL;			// -> first byte in receive buffer.
	long aRcvLgth = 0;				// Number of bytes in mRcvBfr so far.
	long aRcvSize = 0;				// Size of receive buffer in bytes.
	long aRqId = 0;					// Request ID extracted from the prefix.
	QByteArray aSzBfr(11, '0');		// Place to put size fields
	char* apSzBfr=aSzBfr.data();	// -> first byte in size buffer.
	long aSocketId = 0;				// Index into the cOpenSockets array (0 if none)
	AAppSocketSt* apSockSt = NULL;	// Ptr to socket structure
	if (cSocketMap.contains(apSocket) && (aSocketId = cSocketMap.value(apSocket))>0 && (apSockSt = cOpenSockets[aSocketId])!=NULL)
	{	apDataBfr = apSockSt->mpDataBfr;
		aDataLgth = apSockSt->mDataLgth;
		aDataSize = apSockSt->mDataSize;
		aRcvLgth = apSockSt->mRcvLgth;
		aRcvSize = apSockSt->mRcvSize;
	}
	// Load. Fetch next transmission from socket.
	QByteArray& arRcvBfr = (apSockSt == NULL) ? aRcvBfr : apSockSt->mRcvBfr;
	apRcvBfr = arRcvBfr.data();
	while (apSocket->bytesAvailable() > 0)
	{	// Sizes.  Get the request size and binary buffer size from the prefix.
		if (aRcvSize == 0)
		{	// Resync.  When getting garbage, eat first couple chars if they are not digits.
			apSocket->peek(apSzBfr, 2);
			if (*apSzBfr < '0' || *apSzBfr > '9')
			{	aLgth = 1;
				if (*(apSzBfr + 1) < '0' || *(apSzBfr + 1) > '9')
					++aLgth;
				apSocket->read(apSzBfr, aLgth);
			}			
			
			if ((aLgth = apSocket->read(apSzBfr, ASIZE)) == ASIZE && *(apSzBfr + ASIZE - 1) == '\177'
			&& (aRcvSize = AUtil::toLong(apSzBfr, &aLgth)) > 0 && aLgth == ASIZE)
			{	if (apSocket->read(apSzBfr, ASIZE) == ASIZE && *(apSzBfr + ASIZE - 1) == '\177'
				&& (aDataSize = AUtil::toLong(apSzBfr, &aLgth)) >= 0 && aLgth == ASIZE)
					;
				else
				{	qDebug("AAppSvr::onSubmit(), Unable to extract binary data buffer size");
					aDataSize = 0;
				}
			}
			else
			{		qDebug("AAppSvr::onSubmit(), Unable to extract response message size");
					aRcvSize = 0;
					break;
			}

			// Resize. Make sure the response buffer is large enough.
			if (arRcvBfr.size() != (aRcvSize + 1))
			{	arRcvBfr.clear();
				arRcvBfr.resize(aRcvSize + 1);	// Allow for terminator on end
				apRcvBfr = arRcvBfr.data();
			}
		}
		// Response. Fetch more bytes if receive buffer not full.
		if ((aNeed = aRcvSize - aRcvLgth) > 0)
		{
			// Read. Read number of bytes needed to fill receive buffer.
			if ((aLgth = apSocket->read(apRcvBfr + aRcvLgth, aNeed + 1)) == aNeed + 1)
			{	// Terminator. Check terminator on end.
				aRcvLgth = aRcvSize;
				if (*(apRcvBfr + aRcvLgth) > '\01')
					qDebug("ASocket::onReadApp(), Encountered unterminated response in message");
				*(apRcvBfr + aRcvLgth) = '\0';		// Null terminate request.
			}
			else	// Quit with partial response in cRcvBfr.
			{	aRcvLgth += aLgth;
				break;
			}
		}
		// Binary Data. Fetch more bytes binary data buffer is not full.
		if ((aNeed = aDataSize - aDataLgth) > 0)
		{	// Allocate.  Allocate a binary data buffer big enough to hold expected data.
			if (apDataBfr == NULL)
				apDataBfr = (char*)malloc(aDataSize);

			// Read. Read the binary data available from the socket.
			if ((aLgth = apSocket->read(apDataBfr + aDataLgth, aNeed)) < aNeed)
			{	aDataLgth += aLgth;
				break;
			}
		}
		// ReqHdr. Uncomment to log this request in ReqHdrs
		LOGREQHDRMSG(QString(arRcvBfr) + '\n');
		// RqId.  Extract the RqId from the receive buffer.
		aRqId = AUtil::toLong(apRcvBfr, &aLgth);
		aAmpmsg = apRcvBfr + aLgth;

		// Got it. 	 Submit request: targetLambda|speechAct|...
		submit(aRqId, aAmpmsg, NULL/*pInProcClient*/, apSocket, apDataBfr);
		apDataBfr = NULL;
		aDataLgth = aDataSize = 0;
		aRcvLgth = aRcvSize = 0;
	}
	if (apSockSt != NULL)
	{	apSockSt->mpDataBfr = apDataBfr;
		apSockSt->mDataLgth = aDataLgth;
		apSockSt->mDataSize = aDataSize;
		apSockSt->mRcvLgth = aRcvLgth;
		apSockSt->mRcvSize = aRcvSize;
	}
}

/*!
 * \brief Establish a new socket structure in cOpenSockets array.
 *
 * \param[in] iConnectId Connection ID established by AppSvr as index into cOpenConnections.
 * \param[in] irContextName	Default context assigned to this connections.
 * \param[in] ipInProcClient -> in-process client making this connection (null if remote connection).
 * \param[in] ipSocket -> Socket for remote clients (null if in-process).
 *
 * \return aSocketId - Index into cOpenSockets.
 *
 * \note
 * -# A socket structure is allocated on a succesful logon.
 * -# The cConnectionMap and cSocketMaps are also updated here.
 */
long AAppSvr::openSocket(long iConnectId, const QString& irContextName, AReturnRcvr* ipInProcClient, QTcpSocket* ipSocket)
{
	// If a socket structure already exists, update the structure.
	QTcpSocket* apSocket = (ipSocket == NULL) ? (QTcpSocket*)ipInProcClient : ipSocket;
	ACHECK(iConnectId > 0 && !irContextName.isEmpty() && apSocket != NULL);
	// if (iConnectId <= 0 || irContextName.isEmpty() || apSocket == NULL)
	//	qDebug("AAppSvr::openSocket() fails");
	long aSocketId = 0, aSz = cOpenSockets.size();
	AAppSocketSt* apSockSt = NULL;
	
	if (cSocketMap.contains(apSocket) && (aSocketId = cSocketMap.value(apSocket)) > 0 && aSocketId < aSz &&
		(apSockSt = cOpenSockets[aSocketId]) != NULL)
	{	// Add this connection if it is not already in the mpContextMap.
		AStringIntMap& arMap = apSockSt->mContextMap;
		if (arMap.contains(irContextName))
		{	if (!cConnectionMap.contains(iConnectId))
				aSocketId = -AERR_CONNECTID;
		}
		else
		{	arMap[irContextName] = iConnectId;
			cConnectionMap[iConnectId] = aSocketId;
		}
		// Update current connection ID
		apSockSt->mCurConnectId = iConnectId;
		apSockSt->mpInProcClient = ipInProcClient;
	}
	else	// Add a new entry to cOpenSockets
	{	for (aSocketId = 1; aSocketId < aSz; ++aSocketId)
		{	apSockSt = cOpenSockets[aSocketId];
			if (apSockSt->mpSocket == NULL && apSockSt->mpInProcClient == NULL)
				break;
		}
		if (aSocketId >= aSz) 
		{	apSockSt = new AAppSocketSt;
			cOpenSockets.insert(aSocketId, apSockSt);
		}
		else
			apSockSt = cOpenSockets[aSocketId];

		// Fill in this new entry.
		apSockSt->mContextMap[irContextName] = iConnectId;
		apSockSt->mCurConnectId = iConnectId;
		apSockSt->mpDataBfr = NULL;
		apSockSt->mDataLgth = apSockSt->mDataSize = 0;
		apSockSt->mpInProcClient = ipInProcClient;
		apSockSt->mRcvBfr.truncate(0);
		apSockSt->mRcvLgth = apSockSt->mRcvSize = 0;
		apSockSt->mReqMap.clear();
		apSockSt->mRqId = 0;
		apSockSt->mpSocket = ipSocket;

		// Update the maps
		cConnectionMap[iConnectId] = aSocketId;
		cSocketMap[apSocket] = aSocketId;
	}
	return aSocketId;	
}

/*!
 * \brief Helper file to return messages to APP clients.
 *
 * \param[in] iConnectId Connection ID established by AppSvr as index into cOpenConnections.
 * \param[in] iRqId Request ID (client-generated) included in submitted request.
 * \param[in] iStatus Status code (>0 iff a problem encountered).
 * \param[in] iReqType Code indicating request type (eval, ampmsg, built-ins...).
 * \param[in] iRetValue Integer value (0 if this type of request does not return an integer value).
 * \param[in] irOut Returned message.
 * \param[in] ipData -> Returned binary string.
 * \param[in] irDisplay Console output, if any.
 * \param[in] irError Error details, if any.
 * \param[in] ipInProcClient -> in-process client making this connection (null if remote connection).
 * \param[in] ipSocket -> Socket for remote clients (null if in-process).
 *
 * \note
 * -# Request returned to in-proc clients via a callback to the client's return output routine. Called with same input args.$.
 * -# Request returned to remote clients via a return msg: MsgLgth|DataLgth|RqId|ReqType|Status|RetValue|Display|Error|Out\01.
 * -# Remote return does not include the superflous connect ID.
 * -# The prefix, MsgLgth|DataLgth|, consists of 2 fixed length, 0 filled integers each terminated by the DEL char.
 * -# MsgLgth is the number of bytes of the message transmitted not including the prefix or the '\01' on the end.
 * -# DataLgth is the number of bytes of binary data transmitted tacked on to the end of the message. 
 * -# Note that the originating target Lambda and speech act are not returned explicitly.  See testAisClient for examples.
 */
void AAppSvr::retOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
, char* ipData,long iDataSize,const QString& irDisplay,const QString& irError,AReturnRcvr* ipInProcClient,QTcpSocket* ipSocket)
{				
	// In-process client. Return message via a callback to the client.
	if (ipInProcClient != NULL)
		ipInProcClient->returnOutput(iConnectId, iRqId, iStatus, iReqType, iRetValue, irOut, ipData, 0, irDisplay, irError);
	// Remote Client.
	else
	{	if (ipSocket != NULL)
		{	QString aMsg = QString("%1\177%2\177%3\177%4\177%5\177%6\177%7").arg(iRqId).arg(REQNAME(iReqType)).arg(iStatus)
			.arg(iRetValue).arg(irDisplay, irError, irOut);
			// ReqHdr. Uncomment to log RESPONSES
			QString aHdr(aMsg + '\n');

			// make sure it will not log itself
			if (iReqType != geLogReqHdr)
			{
				LOGREQHDRMSG(aHdr);
			}

			long aSz = aMsg.length();
			aMsg.prepend(QString("%1\177%2\177").arg(aSz,ASIZE-1,10,QChar('0')).arg(iDataSize,ASIZE-1,10,QChar('0')));
			aMsg += '\01';
			ipSocket->write(aMsg.toLatin1());
			if (ipData != NULL && iDataSize > 0)
				ipSocket->write(ipData, iDataSize);
		}
		else
		{	qDebug("0,AppSvr::retOutput(ConnectId:%ld,RqId:%ld,Status:%ld,ReqType:%s,RetValue:%ld,Out:%.128s,Display:%.128s,"
			"Error:%.128s), No ptr to client.", iConnectId, iRqId, iStatus, REQNAME(iReqType), iRetValue, irOut.toLatin1().data(),
			irDisplay.toLatin1().data(), irError.toLatin1().data());
		}
		if (ipData != NULL)
			free(ipData);
	}
}

/*!
 * \brief Return a file on the server to client.
 *
 * \param[in] iConnectId Connection ID established by AppSvr as index into cOpenConnections.
 * \param[in] iRqId Request ID (incrementing integer) included in submitted request.
 * \param[in] irFileSpec Full path to requested file.
 * \param[in] ipInProcClient -> in-process client making this connection (null if remote connection).
 * \param[in] ipSocket -> Socket for remote clients (null if in-process).
 *
 * \return aStatus - Returns error code (>0) iff read file fails.
 *
 * \note
 * -# Returns large files in chunks to remote clients.
 * -# Returned Msg: Length|RqId|ReqType|Status|RetValue|Display|Error|Contents.
 * -# Warning: QByteArray methods may resize an allocated buffer. You were warned.
 */
long AAppSvr::returnFile(long iConnectId, long iRqId, const QString& irFileSpec, AReturnRcvr* ipInProcClient, QTcpSocket* ipSocket)
{
	// Check inputs. Open the file.
	long aRetValue = 0, aStatus = 0;
	ACHECK(!irFileSpec.isEmpty() && ipSocket != NULL);
	QFile aFile(irFileSpec);

	// Read file and send in 4K blocks to socket.
	if (!aFile.open(QIODevice::Unbuffered | QIODevice::ReadOnly))		// 64 | 1
		aStatus = AERR_FILEREAD;
	else
	{	long aSz = aFile.size();
		// Local. Return to in-process client
		QByteArray aBfr;
		char* apBfr;
		if (ipInProcClient != NULL)
		{	aBfr.resize(aSz + 1);
			apBfr = aBfr.data();
			if (aFile.read(apBfr, aSz) > 0)
			{	*(apBfr + aSz) = '\0';
				ipInProcClient->returnOutput(iConnectId, iRqId, aStatus, geRetFile, aRetValue, aBfr, NULL/*Data*/, 0/*Size*/
				, cMt/*Display*/, cMt/*Error*/);
			}
			else
				aStatus = AERR_FILEREAD;
		}	// Remote. Send over pipe
		else
		{	// Msg. Compose and send the return msg with a prefix attached.
			aBfr = QByteArray::number((int)iRqId) + "\177retfile\1770\1770\177\177\177";

			// ReqHdr. Uncomment to log RESPONSES
			QString aHdr(aBfr + '\n');
			LOGREQHDRMSG(aHdr);

			// Prefix. Prepend the prefix to the message
			long aMsgLgth = aBfr.length() + aSz;		// MsgLgth. Length of body + file (but not prefix or terminator).
			aBfr.prepend(QByteArray::number((int)aMsgLgth).rightJustified(10, '0') + "\1770000000000\177");
			ipSocket->write(aBfr.data(), aBfr.length());		// Don't send null on end yet.

			// Send. Send the file in 4K chunks.
			long aLgth;
			aBfr.clear();
			aBfr.resize(4098);
			apBfr = aBfr.data();
			while ((aLgth = (aSz >= 4096) ? 4096 : aSz) > 0)
			{	if (aFile.read(apBfr, aLgth) <= 0)
					break;
				else
				{	if ((aSz -= aLgth) == 0)
						*(apBfr + aLgth++) = '\0';		// Send null on end
					ipSocket->write(apBfr, aLgth);
				}
			}
		}
		aFile.close();
	}
	return aStatus;
}

/*!
 * \brief Update the task state and then process the returned information from the AIS server.
 *
 * \param[in] iConnectId Placeholder (used in other parts of AIS for the connectID).
 * \param[in] iRqId An incrementing integer assigned by appclient to each outgoing request to the server.
 * \param[in] iStatus Zero or a positive error code if an error was generated by the server.
 * \param[in] iReqType Enum describing the type of response or pushed data from the server.
 * \param[in] iRetValue Integer return value (defaults to zero).
 * \param[in] irOut Returned message (defaults to an empty string).
 * \param[in] ipData -> to binary data.
 * \param[in] iDataSize Size of binary data.
 * \param[in] irDisplay Return from writeln and display (defaults to an empty string).
 * \param[in] irError Returned error message (defaults to an empty string).
 * \param[in] iClientData Optional string submitted with a request and returned verbatim with the response.
 *
 * \note
 * -# If completing a request for a task, move to the next request in the task.
 * -# If last request of a task is completed, move to next task for this job.
 * -# Returns large files in chunks to remote clients.
 */
void AAppSvr::returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(iClientData);

	// Look up mpSocket in the cConnectionMap
	long aSocketId = 0, aSz = cOpenSockets.size(), aRqId = 0;
	AAppSocketSt* apSockSt = NULL;
	if (iConnectId > 0 && cConnectionMap.contains(iConnectId) && (aSocketId = cConnectionMap.value(iConnectId)) > 0
	&& aSocketId < aSz && (apSockSt = cOpenSockets[aSocketId]) != NULL)
	{	// Handle RetFile requests. _ais|retfile|file|%s
		QTcpSocket* apSocket = apSockSt->mpSocket;
		if (apSockSt->mReqMap.contains(iRqId))
		{	aRqId = apSockSt->mReqMap[iRqId];
			apSockSt->mReqMap.remove(iRqId);
		}
		if (iStatus <= 0 && iReqType == geRetFile)
		{	QString aOut(irOut);
			if ((iStatus = gAis.mpAisSvr->getFileSpec(iConnectId, AISSVR_APP, cDefaultContextName, aOut)) <= 0 && 
			(iStatus = returnFile(iConnectId, aRqId, aOut, apSockSt->mpInProcClient, apSocket)) <= 0)
				return;
		}
		retOutput(iConnectId, aRqId, iStatus, iReqType, iRetValue, irOut, ipData, iDataSize, irDisplay, irError
		, apSockSt->mpInProcClient, apSocket);
	}
	else
	{	qDebug("0,AppSvr::returnOutput(ConnectId: %ld,RqId: %ld,Status: %ld,ReqType: %s,Out:%.128s,Display:%123s, Error:%.128s),"
		"Uninitialized client.", iConnectId, iRqId, iStatus, REQNAME(iReqType), irOut.toLatin1().data(),irDisplay.toLatin1().data(),
		irError.toLatin1().data());
	}
}

/*!
 * \brief Called by AisSvr to associate a context name with connection when opening a session.
 *
 * \param[in] iConnectId Connection ID established by AppSvr as index into cOpenConnections.
 * \param[in] irContextName Name of context to be associated with this context.
 *
 * \note
 * -# Implements pure virtual function inherited from the AReturnRcvr abstract base class.
 * -# A session may be opened by an openSession request, an automatic open, or openContext.
 * -# If openSession request from client, context is set by AisSvr.openSession.
 * -# If openSession from engine, context is set by ???.
 * -# If openSession automatically, context is set by AisSvr.openSession.
 * -# If openSession on a SystemContext, no requests are submitted directly by clients.
 */
void AAppSvr::setContextName(long iConnectId, const QString& irContextName)
{
	if (cConnectionMap.contains(iConnectId))
	{	// Search for entry in map with a value equal to this connectId
		long aSocketId = cConnectionMap.value(iConnectId);
		AStringIntMap& arMap = cOpenSockets[aSocketId]->mContextMap;
		AStringIntMap::Iterator apIt;
		for (apIt = arMap.begin(); apIt != arMap.end(); ++apIt)
		{	if (*apIt == iConnectId)
			{	// Change the key for this entry to the new context name
				if (apIt.key() != irContextName)
				{	arMap.remove(apIt.key());
					arMap[irContextName] = iConnectId;
				}
				break;
			}
		}
	}
}

/*!
 * \brief Submit an incoming request to AisSvr.
 *
 * \param[in] iRqId Request ID (incrementing integer) included in submitted request.
 * \param[in] irAmpmsg Submitted request in AMP format.
 * \param[in] ipInProcClient -> in-process client making this connection (null if remote connection).
 * \param[in] ipSocket -> Socket for remote clients (null if in-process).
 * \param[in] ipData -> binary data array.
 *
 * \note
 * -# Ampmsg format: targetLambda|speechAct|arg|%s...
 * -# A RqId (server-wide incrementing integer) is assigned each request.  ReqHdr - Key:RqId, Value: RqId.
 * -# Returns large files in chunks to remote clients.
 * -# For now, set connect ID to mCurConnectId. If support is added for multiple contexts on one socket, pass in the context
 * name and use mContextMap to look up the connectId.
 */
void AAppSvr::submit(long iRqId, const QString& irAmpmsg, AReturnRcvr* ipInProcClient, QTcpSocket* ipSocket, char* ipData)
{
	// ConnectId. Look up connection ID
	long aConnectId = 0, aId = 0;		// Connect ID, Socket ID
	AAppSocketSt* apSockSt = NULL;
	QTcpSocket* apSocket = (ipSocket == NULL) ? (QTcpSocket*)ipInProcClient : ipSocket;
	if (cSocketMap.contains(apSocket) && (aId=cSocketMap.value(apSocket)) > 0 && aId > 0 && (apSockSt=cOpenSockets[aId])!=NULL)
		aConnectId = apSockSt->mCurConnectId;

	// Local. Process logon and local requests here.
	bool aIsAis = irAmpmsg.startsWith("_ais", Qt::CaseInsensitive);
	if (aIsAis || aConnectId <= 0)
	{	long aStatus;
		bool aIsLogon = false;
		QString aAct, aOut("");
		AReqType aReqType;
		if (aIsAis)
		{	// Built-ins. Process probe, retfile, logon requests
			aAct = irAmpmsg.section('\177', 1, 1).toLower();
			if (aAct.isEmpty() || aAct == "probe")
			{	aAct = "noop";
				aReqType = geNoop;
				aOut.sprintf("connectid\177%ld", aConnectId);
				retOutput(aConnectId, iRqId, 0/*Status*/, aReqType, 0/*Value*/, aOut, NULL, 0,cMt,cMt,ipInProcClient,ipSocket);
				if (ipData != NULL)
				{	free(ipData);
					qDebug("AAppSvr::submit(RqId:%ld, Ampmsg:%s), Data dropped.", iRqId, irAmpmsg.toAscii().data());
				}
				return;
			}
			// RetFile. Return a document.
			else if (aAct == "retfile")
			{	// _ais|retfile|file|%s
				QString aFileSpec = irAmpmsg.section('\177', 3, 3);
				aReqType = geRetFile;
				if ((aStatus = gAis.mpAisSvr->getFileSpec(aConnectId, AISSVR_APP, cDefaultContextName, aFileSpec)) > 0 ||
				(aStatus = returnFile(aConnectId, iRqId, aFileSpec, ipInProcClient, ipSocket)) > 0)
					retOutput(aConnectId, iRqId, aStatus, aReqType, 0/*RetValue*/, aFileSpec, NULL, 0, cMt, cMt,ipInProcClient
					, ipSocket);
				if (ipData != NULL)
				{	free(ipData);
					qDebug("AAppSvr::submit(RqId:%ld, Ampmsg:%s), Data dropped.", iRqId, irAmpmsg.toAscii().data());
				}
				return;
			}
			else if (aAct == "logon")
				aIsLogon = true;
		}
		// Logon. Bypass submit. Go directly to logon.
		if (aIsLogon || aConnectId <= 0)
		{	// _ais|logon|user|%s|passwd|%s|context|%s|wait|%d
			long aDefCloseWait = -2, aUsrId;
			QString aContextName(cDefaultContextName), aPasswd, aUsrName;
			if (aIsLogon)
			{	QStringList aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
				long aTknSz = aTkns.count();
				if (aTknSz >= 6)
				{	aUsrName = aTkns[3];
					aPasswd = aTkns[5];
					if (aTknSz >= 8)
					{	aContextName = aTkns[7];
						if (aTknSz >= 10)
							aDefCloseWait = aTkns[9].toLong();
					}
				}
			}
			// Authenticate user.
			aUsrId = gAis.mpAisSvr->logon(this, aDefCloseWait, AISSVR_APP, aUsrName, aPasswd, aContextName, &aConnectId, aOut);
			if (aUsrId > 0)
			{	// Open Socket. Open or update socket structure. If aUsrId >0, then aConnectId > 0. aId is socket ID.
				if ((aId = openSocket(aConnectId, aContextName, ipInProcClient, ipSocket)) < 0)
					aUsrId = aId;			// Return error code.
			}
			// Return. If logon or if logon fails, return response; else, submit request.
			if (aIsLogon || aUsrId < 0)
			{	if (aUsrId >= 0)
				{	aOut.prepend("userid\177");	// aOut holds usrid.
					aStatus = 0;
				}
				else
				{	aStatus = -aUsrId;
					aUsrId = 0;
				}
				aReqType = aIsAis ? REQTYPE(aAct) : geAmpMsg;
				retOutput(aConnectId, iRqId, aStatus, aReqType, aUsrId/*Value*/, aOut, NULL,0,cMt,cMt,ipInProcClient,ipSocket);
				if (ipData != NULL)
				{	free(ipData);
					qDebug("AAppSvr::submit(RqId:%ld, Ampmsg:%s), Data dropped.", iRqId, irAmpmsg.toAscii().data());
				}
				return;
			}
		}	// logon
	}
	// Submit. AisSvr.submit returns immediate and async responses via its returnOutput.
	long aRqId = ++gAis.mpAisSvr->mRqId;
	if (apSockSt != NULL)
		apSockSt->mReqMap[aRqId] = iRqId;
	gAis.mpAisSvr->submit(irAmpmsg, false/*IsAsync*/, aConnectId, aRqId, ipData);
}
// end


