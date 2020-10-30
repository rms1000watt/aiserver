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
aisdev/xmlsvr/axmlsvr.cpp
														Xml Server

CHANGE HISTORY
Version	 Date		Who		Change
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0118	12/12/2006	tlw		submit. Add null argument to call to AAisSvr::submit.
1.0112	10/27/2006	tlw		~AXmlSvr. Reclaim all AXmlSocketSt on destruct.
1.0104	 9/4/2006	tlw		Added more documentation of the XML format.
1.0065	 6/22/2005	tlw		returnOutput. Unwrap records returned by getConsoleLog. Convert _eval to built-in cmd.
1.0061	 5/5/2005	tlw		submit(). If target is eval, set target to _ais, act to eval.
1.0057	 3/18/2005	tlw		Update documentation.
												--------------- ---------------
DOCUMENTATION
1.	See XmlSvrNotes.txt for project setup information.
2.	See ../include/axmlsvr.h for class specification.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------- IMPORTS -----------------------------------------------------------
#include <QtCore/QFileInfo>
#include <QtNetwork/QTcpServer>

#include "axmlsvr.h"			// AXmlSvr
#include "autilities.h"			// AUtil

//	-------------------------------------------------- DEFINED CONSTANTS ------------------------------------------------------
#define AXMLSVR_MAXBFRSZ	1280000
#define AXMLSVR_BFRINC		2048

//	---------------------------------------------------- CLASS METHODS --------------------------------------------------------
/*!
\brief AXmlSvr constructor creates an instance of QTcpServer to listen on a server socket and initializes the cOpenSockets list.

\param iPort - The server listens for incoming XML requests and sends messages to the XML client on this socket
\param irContextName - The default context name that is used if no context is specified in the incoming request.
\param ipParent -> The parent that created this instance of AXmlSvr
\par Notes:
-# cDefaultContextName is set to system-wide default context or to the context of a context-specific port.
 */
AXmlSvr::AXmlSvr(ushort iPort, const QString& irContextName, QObject* ipParent)
	: QObject(ipParent), cDefaultContextName(irContextName), cMt(""), cPort(iPort)
{
	cpXmlServer = new QTcpServer(this);
	if (!cpXmlServer->listen(QHostAddress(QHostAddress::Any), iPort))
	{	// PENDING AXmlSvr(). Unable to start the server. Log message. Log the error.
		qDebug("AXmlSvr, Unable to start the server on port %d, %s",iPort,cpXmlServer->errorString().toLatin1().data());
	}
	else
		connect(cpXmlServer, SIGNAL(newConnection()), this, SLOT(onNewConnection()));

	// Create one "null" entry in openSockets list.
	setObjectName("XmlSvr");
	AXmlSocketSt* apSockSt = new AXmlSocketSt();
	apSockSt->mBfrSz = 0;
	apSockSt->mContextMap.clear();
	apSockSt->mCurConnectId = 0;
	apSockSt->mMsgLgth = -1;
	apSockSt->mReqMap.clear();
	apSockSt->mpRcvBfr = NULL;
	apSockSt->mpSocket = NULL;
	apSockSt->mRqId = -1;
	cOpenSockets.insert(0, apSockSt);
}


/*!
\brief Destructor Free allocated resources

\par Notes:
-# AXmlSvr has remote ownership of AXmlSocketSt referents in cOpenSockets. 
-# Since instances of AXmlSvr are never assigned, copied or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
AXmlSvr::~AXmlSvr()
{
	QTcpSocket* apSocket;
	AXmlSocketSt* apSockSt;
	for (long aSocketId = 0; aSocketId < cOpenSockets.size(); ++aSocketId)
	{	if ((apSockSt = cOpenSockets[aSocketId]) != NULL)
		apSockSt->mReqMap.clear();
			if ((apSocket = apSockSt->mpSocket) != NULL)
			{	apSocket->abort();
				if (apSockSt->mpRcvBfr != NULL)
					delete apSockSt->mpRcvBfr;
				apSocket->deleteLater();
			}
			delete apSockSt;
	}
	cSocketMap.clear();
	cOpenSockets.clear();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AXmlSvr()");
#endif
}

/*!
\brief connectionClosed - An connection to a context was closed. Mop up.

\param iConnectId - Index into cConnectionMap to identify connection to be closed
\return aFoundIt - True iff a valid connection could be located.
\par Notes:
-# Called by AisSvr.closeConnection if this or some other protocol server closes connection.
-# Inherited from the AReturnRcvr abstract base class.
-# See onDisconnected which calls into AisSvr.closeConnection.
*/
bool AXmlSvr::connectionClosed(long iConnectId)
{
	long aConnectId, aSocketId = 0;
	QTcpSocket* apSocket = NULL;
	AXmlSocketSt* apSockSt = NULL;
	bool aFoundIt = false;
	if (iConnectId>0&&cConnectionMap.contains(iConnectId)&&(aSocketId=cConnectionMap[iConnectId])>0
			&& aSocketId < cOpenSockets.size() && (apSockSt = cOpenSockets[aSocketId])!=NULL)
	{	// Clear all entries for this connection from connection maps
		if ((apSocket = apSockSt->mpSocket) != NULL)
			cSocketMap.remove(apSocket);
		AStringIntMap& arMap = apSockSt->mContextMap;
		AStringIntMap::Iterator apIt;
		for (apIt = arMap.begin(); apIt != arMap.end(); ++apIt)
		{	if (cConnectionMap.contains(aConnectId = *apIt))
				cConnectionMap.remove(aConnectId);
		}
		// Mark entry in cOpenSockets as unused
		apSockSt->mpSocket = NULL;
		apSockSt->mContextMap.clear();
		apSockSt->mReqMap.clear();
		if (apSockSt->mpRcvBfr != NULL)
		{	delete apSockSt->mpRcvBfr;
			apSockSt->mpRcvBfr = NULL;
		}
		delete apSockSt;
		// Close remote socket connection.
		if (apSocket != NULL)
		{	apSocket->close();
			apSocket->deleteLater();
		}
		aFoundIt = true;
	}
	return aFoundIt;
}

/*	---------------------------------------------------------------------------------------------------------------------------
onConnected - Called on a connected signal from QSocket after connectToHost succeeds.
Args:
	none
Return:	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AXmlSvr::onConnected()
{
}

/*	---------------------------------------------------------------------------------------------------------------------------
onDisconnected - Called on signal from QServerSocket when a connection is unexpectedly closed.
Args:	none
Returns:
	nothing
Notes:
 1.	Also called by retOutput if connection unexpectedly becomes unavailable.
	------------------------------------------------------------------------------------------------------------------------ */
void AXmlSvr::onDisconnected()
{	// Connected to the QServerSocket signal. Emitted when a connection is closed.
	QTcpSocket* apSocket = (QTcpSocket*)sender();
	ACHECK(apSocket != NULL);

	long aSocketId = 0, aConnectId = 0;
	long aSz = cOpenSockets.size();
	AXmlSocketSt* apSockSt = NULL;
	if (cSocketMap.contains(apSocket) && (aSocketId = cSocketMap.value(apSocket)) > 0 && aSocketId < aSz
			&& (apSockSt = cOpenSockets[aSocketId]) != NULL && !apSockSt->mContextMap.isEmpty())
	{	AStringIntMap& arMap = apSockSt->mContextMap;
		AStringIntMap::Iterator apIt = arMap.begin();

		// AisSvr will call connectionClosed to recover all allocated resources for this socket
		gAis.mpAisSvr->closeConnection(aConnectId = *apIt, geDefault, 0/*Wait*/);
	}
	// Debug. Uncomment to debug
	/* QString aErr;
	aErr.sprintf("0,AXmlSvr::onDisconnected(), Connection unexpectedly closed. ConnectId%ld\n", aConnectId);
	LOGSYSMSG(geInfo, aErr); */
}

/*	---------------------------------------------------------------------------------------------------------------------------
onError - Called on signal from QSocket when socket error encountered
Args:
	iCode	QSocket error code
Return:	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AXmlSvr::onError(QAbstractSocket::SocketError iCode)
{
	if (iCode != QAbstractSocket::RemoteHostClosedError)
	{	QTcpSocket* apSocket = (QTcpSocket*)sender();
		qDebug() << "AXmlSvr::onError(), Encountered TCP/IP error, " << apSocket->errorString() << " Code:" << iCode;
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
onNewConnection - Establish a new TCP/IP socket connection
Args:
Return:
	nothing
Note:
 1.	Required virtual function by the parent class QServerSocket
	------------------------------------------------------------------------------------------------------------------------ */
void AXmlSvr::onNewConnection()
{
	if (cPort <= 0) return;
	// Socket. Construct a QTcpSocket object when a new connection is established:
	QTcpSocket* apSocket = cpXmlServer->nextPendingConnection();

	connect(apSocket, SIGNAL(connected()), this, SLOT(onConnected()));
	connect(apSocket, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
	connect(apSocket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(onError(QAbstractSocket::SocketError)));
	connect(apSocket, SIGNAL(readyRead()), this, SLOT(onSubmit()));
}

/*	---------------------------------------------------------------------------------------------------------------------------
onSubmit - Retrieve transmission from TCP/IP connection.
Args:		none
Returns:	nothing
Notes:
 1.	It is possible to receive a complete request, a partial request or multiple requests. A request is terminated by ^A or
	a null.  For long messages, scanning for a terminator is God-awful slow.
 2.	If pSocket is in Socket map, look up apSockSt. Extract partial buffer from socket structure.
 2.	If a complete request has been fetched, submit incoming XML doc to submit.
 3.	The socket map is used to locate the socket structure containing a partial request, if any.
 4.	On returning a response, the connect map is used to map the connect ID to the socket ID.
 5. No cookie is required. A persistent connection is used to maintain a fixed pSocket.  Upon logon, an entry is added to the
	socket map to identify the client state across requests.
	------------------------------------------------------------------------------------------------------------------------ */
void AXmlSvr::onSubmit()
{
	// Prepend data from existing buffer. In order to thwart Bad Guys, no buffering until after logon.
	QTcpSocket* apSocket = (QTcpSocket*)sender();
	ACHECK(apSocket != NULL && cPort > 0);
	long aSocketId = cSocketMap.contains(apSocket) ? cSocketMap.value(apSocket) : -1;
	AXmlSocketSt* apSockSt = (aSocketId > 0) ? cOpenSockets[aSocketId] : NULL;
	ulong aBfrSz = AXMLSVR_BFRINC;		// Buffer size
	ulong aMsgLgth = 0, aSz;			// Message length
	QByteArray* apRcvBfr = NULL;
	if (apSockSt != NULL && (apRcvBfr = apSockSt->mpRcvBfr) != NULL)
	{	aBfrSz = apSockSt->mBfrSz;
		aMsgLgth = apSockSt->mMsgLgth;
		apSockSt->mpRcvBfr = NULL;
		apSockSt->mMsgLgth = 0;
	}
	if (apRcvBfr == NULL)
	{	aBfrSz = AXMLSVR_BFRINC;	// Buffer size (2K)
		aMsgLgth = 0;				// Message length
		apRcvBfr = new QByteArray;
		apRcvBfr->resize(aBfrSz);
	}
 	// Read incoming XML from the socket. This task is tricky in that a request may span multiple transmissions or a single
 	// transmission may contain multiple requests. Requests must be terminated by a null or ^A (01).
	uchar* apBeg = (uchar*)apRcvBfr->data();	// -> actual data
	uchar* apEnd;
	aSz = apSocket->bytesAvailable();
	if (aMsgLgth + aSz > aBfrSz)
	{	aBfrSz = aMsgLgth + aSz + AXMLSVR_BFRINC;
		if (aBfrSz > AXMLSVR_MAXBFRSZ)
		{	QString aErr;
			aErr.sprintf("0,AXmlSvr::onSubmit(), "
				"Message overflow. len=%ld rcvd=%.128s",aMsgLgth, apBeg);
			LOGSYSMSG(geCritical, aErr);
			delete apRcvBfr;
			onDisconnected();	// close all downstream resources
			return;
		}
		apRcvBfr->resize(aBfrSz);
		apBeg = (uchar *)apRcvBfr->data();
	}
	apEnd = apBeg + aMsgLgth;
	aMsgLgth += aSz;
	apSocket->read((char*)apEnd, aSz);
	apEnd += aSz - 1;

	// If we received a partial message. Wait for more data.
	if (*apEnd > '\01' && *apEnd != '\n')
	{	// Note: Don't buffer if no valid entry in openSockets list
		if (apSockSt != NULL)
		{	apSockSt->mBfrSz = aBfrSz;
			apSockSt->mMsgLgth = aMsgLgth;
			apSockSt->mpRcvBfr = apRcvBfr;
		}
		return;
	}
	// Else, process all received XML documents. It is possible to get multiple documents.
	uchar* apNext;			// -> end of next message
	*apEnd = '\0';
	if (aMsgLgth <= 3)
	{	QString aErr;
		aErr.sprintf("0,AXmlSvr::onSubmit(), Empty packet msg=%s", apBeg);
		LOGSYSMSG(geCritical, aErr);
		delete apRcvBfr;
		return;
	}
	do
	{	apNext = apBeg;
		// If the incoming messages are large, this is God-awful slow
		while (*apNext++ > '\01')
			;
		*(apNext - 1) = '\0';
		// Extract the next request and process it
		QString aXmlDoc((char*)apBeg);
		apBeg = apNext;

		// HTTP. If we get an HTTP request header, note that XML is required.
		QString aRsp = aXmlDoc.left(4);
		if (aRsp == "GET " || aRsp == "POST")
		{	aRsp.sprintf("HTTP/1.1 200 OK\r\n"
				"Host: AnalyticInformationServer-AIS/1.0\r\n"
				"Content-Type: text/html\r\n"
				"Content-Length: %d\r\n\r\n"
				"<HTML><HEAD><TITLE> Analytic Information Server - XML Socket Layer</TITLE></HEAD>"
				"<BODY><H3>Analytic Information Server </H3>Expecting an XML document. Received:<PRE>\r\n"
				"%s</PRE></BODY></HTML>", aXmlDoc.length(), aXmlDoc.toLatin1().data());
			apSocket->write(aRsp.toLatin1().data(), aRsp.length());
			delete apRcvBfr;
			return;
		}
		submit(aXmlDoc, apSockSt, apSocket);
	} while (apBeg < apEnd);
	delete apRcvBfr;
}

/*	---------------------------------------------------------------------------------------------------------------------------
openSocket - Add a new entry to the array of socket structures.
Args:
	iClientId		ClientId for the client associated with this TCP/IP connection
	irContextName	Used as key into map of AisSvr connections for this socket
	ipSocket		Incoming socket for this connection. Used to set socket map.
Returns:
	aSocketId		Index into cOpenSockets (>0)
Note:
 1.	Updates entry in cOopenSockets, cConnectionMap, cSocketMap
	------------------------------------------------------------------------------------------------------------------------ */
long AXmlSvr::openSocket(long iConnectId, const QString& irContextName, QTcpSocket* ipSocket)
{
	// Find an empty entry in the OpenSockets list
	long aSocketId;
	AXmlSocketSt* apSockSt = NULL;
	long aSz = cOpenSockets.size();
	for (aSocketId = 1; aSocketId < aSz; ++aSocketId)
	{	if (cOpenSockets[aSocketId]->mpSocket == NULL)
			break;
	}
	if (aSocketId >= aSz) 
	{	apSockSt = new AXmlSocketSt();
		cOpenSockets.insert(aSocketId, apSockSt);
	}
	else
		apSockSt = cOpenSockets[aSocketId];

	// Fill in this new entry.
	apSockSt->mBfrSz = apSockSt->mMsgLgth = -1;
	apSockSt->mCurConnectId = iConnectId;
	apSockSt->mContextMap[irContextName] = iConnectId;
	apSockSt->mReqMap.clear();
	apSockSt->mpRcvBfr = NULL;
	apSockSt->mpSocket = ipSocket;
	apSockSt->mRqId = 0;

	// Update the maps
	cConnectionMap[iConnectId] = aSocketId;
	cSocketMap[ipSocket] = aSocketId;
	return aSocketId;
}

/*	---------------------------------------------------------------------------------------------------------------------------
returnFile - Send contents of a file back to XML client.
Args:
	irFileSpec	File specification including absolute path and file name.
	irReqHdr	Original request header for use by buildResponseHdr. 
	ipSocket	Ptr to QSocket for this connection.
Returns:
	status		>0 Status code if unable to read file.
Notes:
 1.	Returns file to client in 4K chunks in order to facilitate TCP/IP chunking process.
	------------------------------------------------------------------------------------------------------------------------ */
long AXmlSvr::returnFile(long iConnectId, const QString& irFileSpec, QTcpSocket* ipSocket)
{
    Q_UNUSED(iConnectId);

	// Check inputs. Open the file.
	long aStatus = 0;
	ACHECK(!irFileSpec.isEmpty() && ipSocket != NULL);
	QFile aFile(irFileSpec);

	// Read file and send in 4K blocks to socket.
	if (!aFile.open(QIODevice::Unbuffered | QIODevice::ReadOnly))	// 64 | 1
		aStatus = AERR_FILEREAD;
	else
	{	QByteArray aBfr;
		aBfr.resize(4098);
		char *apBfr = aBfr.data();
		long aLgth;
		while ((aLgth = aFile.read(apBfr, 4096)) > 0) 
		{	if (aFile.atEnd())
			{	*(apBfr + aLgth) = '\0';	// Append null to end of last block
				ipSocket->write(apBfr, ++aLgth);
				break;
			}
			ipSocket->write(aBfr.data(), aLgth);
		}
		aFile.close();
	}
	return aStatus;
}

/*!
\brief returnOutput - Update the task state and then process the returned information from the AIS server.

\param iConnectId - ID of connection that is receiveing the payload
\param iRqId - an incrementing integer assigned by appclient to each outgoing request to the server.
\param iStatus - Zero or a positive error code if an error was generated by the server
\param iReqType - Enum describing the type of response or pushed data from the server.
\param iRetValue - Integer return value (defaults to zero).
\param irOut - Returned message (defaults to an empty string).
\param ipData - Binary buffer containing serialized object closure, if any.
\param iDataSize - Size of binary buffer in bytes.
\param irDisplay - Return from writeln and display (defaults to an empty string).
\param irError - Returned error message (defaults to an empty string).
\param iClientData - Optional string submitted with a request and returned verbatim with the response
\return void
\par Notes:
 -# If completing a request for a task, move to the next request in the task.
 -# If last request of a task is completed, move to next task for this job.
*/
void AXmlSvr::returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(iClientData);

	// Ignore selected pushed output
	QString aErr;
	switch (iReqType)
	{
	case geFcnDebug:
	case geFcnError:
	case geFcnFileOpen:
	case geFcnReadHtmlPage:
	case geFcnReturnResult:
	case geFcnRingBell:
		aErr.sprintf("0, AXmlSvr::returnOutput(ConnectId:%ld, RqId:%ld, Status:%ld, ReqType:%s, RetValue:%ld, Out:%.128s, "
		"Display:%.256s, Error %.128s), Ignoring pushed output.", iConnectId, iRqId, iStatus, REQNAME(iReqType),
		iRetValue, irOut.toLatin1().data(), irDisplay.toLatin1().data(), irError.toLatin1().data());
		LOGSYSMSG(geWarn, aErr);
	case geDisplay:						// Don't return console output
	case geFcnEngineState:			// For now, we do not return Engine State.
		return;
	case geFcnSendToClient:				// Allow sendToClient
	default:
		break;
	}
	// Dispatch message to client (in-proc or remote)
	long aSocketId;
	if (cPort > 0 && iConnectId > 0 && cConnectionMap.contains(iConnectId) && (aSocketId = cConnectionMap[iConnectId]) > 0
	&& aSocketId < cOpenSockets.size())
	{	// RetUrl.  If request is geRetUrl, return url tag.
		AXmlSocketSt* apSockSt = cOpenSockets[aSocketId];
		QTcpSocket* apSocket = apSockSt->mpSocket;
		QString aOut(irOut), aXml;
		if (iReqType == geRetUrl)		// Just return the url. Can't really redirect to XML
			aOut.prepend("url\177");

		// RetFile. If request is geRetFile, return contents of file specified by irOut.
		if (iStatus <= 0 && iReqType == geRetFile && (iStatus = gAis.mpAisSvr->getFileSpec(iConnectId, AISSVR_XML,
		cDefaultContextName, aOut)) <=  0 && (iStatus = returnFile(iConnectId, aOut, apSocket)) <= 0)
			return;
			
		// GetConsoleLog. If request is geGetConsoleLog, unwrap records supplied in irOut.
		if (iStatus <= 0 && iReqType == geGetConsoleLog)
		{	iReqType = geGetConsoleLog;
			// PENDING AXmlSvr::returnOutput(). Unwrap records returned by GetConsoleLog.
		}
		// Process  errors
		if (aOut[0] == '!')
		{	iStatus = AERR_GENERIC;		// 1
			aOut.prepend("error\177");
		}
		else if (iStatus > AERR_GENERIC)
			aOut.sprintf("error\177%s", gAis.mpErrMsgs[iStatus]);

		// XML. Return XML document to the client. Set attributes: act, status, target, xid, xtype
		// Currently, xid is not used.
		AStringMap aAttribs;
		if (apSockSt->mReqMap.contains(iRqId))
		{	AXmlReqSt& arReq = apSockSt->mReqMap[iRqId];
			aAttribs["act"] = arReq.mAct;
			aAttribs["target"] = arReq.mTarget;
		}
		else	// Must be pushed output. Set target and speech act
		{	aAttribs["act"] = REQNAME(iReqType);
			aAttribs["target"] = "_ais";
		}
		aAttribs["status"] = QString::number(iStatus);
		aAttribs["xtype"] = "return";
		if (aOut.indexOf('\177') <= 0)
			aOut.prepend("result\177");
		AUtil::ampmsgToXml(aOut, aAttribs, aXml);
		aXml += '\01';		// Terminate document with ^A
		apSocket->write(aXml.toLatin1().data(), aXml.length());
	}
	else		// Log no-connection error
	{	QString aErr;
		iStatus = AERR_CONNECTID;
		aErr.sprintf("AXmlSvr::returnOutput(ConnectId:%ld,RqId:%ld,Status:%ld,ReqType:%s,RetValue:%ld,Out:%.128s,Display:%.256s,"
		"Error:%.128s), %s", iConnectId, iRqId, iStatus,REQNAME(iReqType),iRetValue,irOut.toLatin1().data(),
		irDisplay.toLatin1().data(),irError.toLatin1().data(), gAis.mpErrMsgs[iStatus]);
		LOGSYSMSG(geWarn, aErr);
	}
}

/*!
\brief setContextName - Called by AisSvr to associate a context name with connection when opening a session

\param iConnectId  - Connection ID established by AppSvr as index into cOpenConnections
\param irContextName - Name of context to be associated with this context
\return void
\par Notes:
-# Implements pure virtual function inherited from the AReturnRcvr abstract base class.
-# A session may be opened by an openSession request, an automatic open, or openContext
-# If openSession request from client, context is set by AisSvr.openSession.
-# If openSession from engine, context is set by ???
-# If openSession automatically, context is set by AisSvr.openSession
-# If openSession on a SystemContext, no requests are submitted directly by clients
*/
void AXmlSvr::setContextName(long iConnectId, const QString& irContextName)
{
	if (cConnectionMap.contains(iConnectId))
	{	// Search for entry in map with a value equal to this connectId
		long aSocketId = cConnectionMap[iConnectId];
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
\brief submitInProc - Called by in-proc version of XML client.  Currently not implemented.

\param ipReturnRcvr -> Receiver that will process the returned message
\param irAmpmsg - AMP request
\param orAisOut - Place to return immediate responses, such as errors.
\param opStatus -> Place to return the status.
 */
long AXmlSvr::submitInProc(AReturnRcvr* ipReturnRcvr, const QString& irAmpmsg, QString& orAisOut, long* opStatus)
{
    Q_UNUSED(ipReturnRcvr);
    Q_UNUSED(irAmpmsg);
    Q_UNUSED(orAisOut);
    Q_UNUSED(opStatus);

	// Call a common submit routine shared by submitInProc and submit
	// return submit(ipClient, irAmpmsg, (QSocket*)ipClient, orAisOut, opStatus);
	return -1;
}

/*	---------------------------------------------------------------------------------------------------------------------------
submit - submit an incoming request to AisSvr
Args:
	irXmlDoc		Submitted request in AMP format
	ipSocket		-> Socket for remote clients (null if in-process)
Returns:
	nothing
Notes:
 1. Ampmsg format: targetLambda|speechAct|arg|%s...
 2.	Returns large files in chunks to remote clients.
 3. For now, set connect ID to mCurConnectId. If support is added for multiple contexts on one socket, pass in the context
	name and use mContextMap to look up the connectId.
	------------------------------------------------------------------------------------------------------------------------ */
void AXmlSvr::submit(const QString& irXmlDoc, AXmlSocketSt* ipSockSt, QTcpSocket* ipSocket)
{
	// AMP. Act, context, mode, target are extracted from root element attributes. Target, act are prepended to Ampmsg.
	long aConnectId = 0, aStatus = 0, aRetValue = 0;
	AReqType aReqType = geUnknown;
	QString aAct, aAmpmsg, aMode, aOut(cMt), aReqContext, aTarget;
	if (!AUtil::xmlToAmpmsg(irXmlDoc, aAmpmsg, aAct, aReqContext, aMode, aTarget))
	{	if (ipSockSt != NULL)
			aConnectId = ipSockSt->mCurConnectId;
		aStatus = AERR_PARSEAMP;
	}
	else
	{	// ReqType. Look up built-in request type. Set ReqType to geAmpMsg if not built-in.
		bool aIsAis = (aTarget == "_ais");
		aReqType = (aIsAis) ? REQTYPE(aAct) : geAmpMsg;

		//	ConnectId. Look up connect ID. Open new connection if a new Context name.
		if (ipSockSt != NULL)
		{	// Look up connectId for this context. If none, open a new connection.
			if (aReqContext.isEmpty())
				aConnectId = ipSockSt->mCurConnectId;
			else if (ipSockSt->mContextMap.contains(aReqContext))
			{	aConnectId = ipSockSt->mContextMap[aReqContext];
				ipSockSt->mCurConnectId = aConnectId;
			}
			else if (ipSockSt->mCurConnectId > 0)
			{	aConnectId = gAis.mpAisSvr->openConnection(ipSockSt->mCurConnectId, aReqContext);
				ipSockSt->mContextMap[aReqContext] = aConnectId;
				ipSockSt->mCurConnectId = aConnectId;
			}
		}
		// Local Requests.  Process local requests
		if (aIsAis)
		{	// GetFile. Return an XML document.
			if (aAct == "getfile")
			{	// _ais|getfile|file|%s
				QString aFileSpec = aAmpmsg.section('\177', 3, 3);
				if ((aStatus = gAis.mpAisSvr->getFileSpec(aConnectId, AISSVR_XML,cDefaultContextName,
					aFileSpec)) <= 0 && (aStatus = returnFile(aConnectId,aFileSpec,ipSocket)) <= 0)
					return;
			}
			else if (aAct.isEmpty() || aAct == "probe")
			{	aAct = "noop";
				aReqType = geNoop;
				aOut.sprintf("connectid\177%ld", aConnectId);
				aStatus = AERR_GENERIC;			// Return result directly
			}	
		}
		// Logon.  Ampmsg = _ais|logon|user|%s|passwd|%s|context|%s|wait|%d
		if (aStatus <= 0)
		{	bool aIsLogon = (aIsAis && aAct == "logon");
			// Extract user name, password from request
			if (aIsLogon || aConnectId <= 0)
			{	long aDefaultCloseWait = -2, aSocketId, aTknSz, aUsrId;
				QString aContextName, aPasswd, aUsrName;
				aContextName = (aReqContext.isEmpty() ? cDefaultContextName : aReqContext);
				if (aIsLogon)
				{	// Parse Ampmsg.
					QStringList aTkns = aAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
					if ((aTknSz = aTkns.size()) >= 6)
					{	aUsrName = aTkns[3];
						aPasswd = aTkns[5];
						if (aTknSz >= 8)
						{	aContextName = aTkns[7];
							if (aTknSz >= 10)
								aDefaultCloseWait = aTkns[9].toLong();
						}	
					}
				}
				// Submit logon request or try autologon as guest.
				aUsrId = gAis.mpAisSvr->logon(this,aDefaultCloseWait,AISSVR_XML,aUsrName,aPasswd,aContextName,&aConnectId,aOut);
				if (aUsrId > 0)
				{	// If this is the first logon, add this socket to the list of sockets.
					if (ipSockSt == NULL && (aSocketId = openSocket(aConnectId, aContextName, ipSocket)) > 0)
						ipSockSt = cOpenSockets[aSocketId];
					// else, PENDING AXmlSvr::submit(). Update mContextMap if context name changed or do it in openSocket...
					
					// If a logon request submitted, return result directly; else, submit request
					if (aIsLogon)
					{	aRetValue = aUsrId;		// aOut holds usrid.
						aOut.prepend("userid\177");
						aStatus = AERR_GENERIC;	// Return aOut directly
					}
				}
				else	// Logon fails. Return status.
					aStatus = -aUsrId;
			}
		}
	}
	// Submit. Submit request to engine (unless previous error or request already handled). If user is not logged on, the
	// connectId is zero (no current connection). An "immediate" response will call returnOutput before returning from submit.
	long aRqId = 0;
	if (ipSockSt != NULL)
	{	aRqId = ++gAis.mpAisSvr->mRqId;
		AXmlReqSt aReq;
		aReq.mAct = aAct;
		aReq.mTarget = aTarget;
		ipSockSt->mReqMap[aRqId] = aReq;
	}
	// Return errors or local results if AERR_GENERIC.
	if (aStatus > 0) 
		returnOutput(aConnectId, aRqId, aStatus, aReqType, aRetValue, aOut, NULL/*Data*/, 0, cMt/*Display*/, cMt/*Error*/);
	// If submit fails, response is returned to returnOutput in AisSvr. No need to do it here.
	else	
		aStatus = -gAis.mpAisSvr->submit(aAmpmsg, false/*IsAsync*/, aConnectId, aRqId, NULL/*Data*/);
}
// end
