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
aisdev/ahttpsvr/ahttpsvr.cpp
														Http Server

Http Server implements an HTTP server for AIS

CHANGE HISTORY
Version	Date		Who		Change
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments. Do not submit NCSA log.
1.0118	12/12/2006	tlw		submit. Add null argument to AAisSvr::submit.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0069	9/14/2005	tlw		Index partial requests by connection.
1.0065	6/24/2005	tlw		submit(). Use _ais with act set to eval.
1.0059	4/22/2005	tlw		Abort pending tasks that are pending for more than HTTPSVR_IDLESECS (1 hr.).
1.0059	4/22/2005	tlw		Remove loop thru a map if removing an entry from the map (causes an exception).
1.0056	3/ 3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												---------------------------------

REQUEST HEADERS
POST /amp.dll HTTP/1.1
Accept: image/gif, application/vnd.ms-powerpoint, ...
Accept-Language: en-us
Content-Type: multipart/form-data; boundary=---------------------------7d42af2730556
Accept-Encoding: gzip, deflate
User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)
Host: localhost
Content-Length: 349
Connection: Keep-Alive
Cache-Control: no-cache

-----------------------------7d42af2730556
Content-Disposition: form-data; name="_ais"

noop
-----------------------------7d42af2730556
Content-Disposition: form-data; name="MyFilePath"; filename="E:\...\wwwroot\somefile.txt"
Content-Type: text/plain

First Line
Last Line
-----------------------------7d42af2730556--
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------- IMPORTS -----------------------------------------------------------
#include <time.h>			// time
#include <QtCore/QtDebug>
#include <QtCore/QFileInfo>
#include <QtNetwork/QTcpServer>

#include "ahttpsvr.h"		// AHttpSvr
#include "autilities.h"		// isBlank

//	--------------------------------------------------- DEFINED CONSTANTS -----------------------------------------------------
//	Type of request - positive values reserved for error codes
#define  HTTP_AMPMSG	 0	// AMP message (from query string or from POST body)
#define  HTTP_FILEREQ	-1	// File request

// Method - positive values reserved for error codes
#define HTTP_NONE		 0	// Unknown request
#define HTTP_GET		 1	// HTTP GET request
#define HTTP_POST		 2	// HTTP POST request

//	-------------------------------------------------- MEMBER FUNCTIONS -------------------------------------------------------
/*!
\brief AHttpSvr - constructor instantiates a TCP socket to listen for incoming HTTP formatted requests from remote clients.

\par Args:
\param iPort - Port (socket) used to listen for incoming requests.
\param irContextName - Default context name if no context name specified in the request.
\param ipParent -> Parent widget that created this instance of an AHttpSvr
\return void
 */
AHttpSvr::AHttpSvr(ushort iPort, const QString& irContextName, QObject* ipParent)
	: QObject(ipParent), cCookieId(0), cDefaultContextName(irContextName), cMt(""), cPort(iPort)
{
	// HttpStatus. Initialize status descriptions.
	cHttpStatus[200] = "OK";
	cHttpStatus[303] = "See Other";				// New w/ HTTP/1.1
	cHttpStatus[400] = "Bad Request";
	cHttpStatus[403] = "Forbidden";
	cHttpStatus[404] = "Not Found";

	// HttpServer. Set up server to listen on the HTTP port.
	cpHttpServer = new QTcpServer(this);
	if (!cpHttpServer->listen(QHostAddress(QHostAddress::Any), iPort))
	{	// PENDING AHttpSvr() - Unable to start the server. Log message. Log the error.
		qDebug("AHttpSvr, Unable to start the server, %s", cpHttpServer->errorString().toLatin1().data());
	}
	else
		connect(cpHttpServer, SIGNAL(newConnection()), this, SLOT(onNewConnection()));

	// cOpenClients. Initialize the first (null) entry in the list of open clients.
	AHttpClientSt aClientSt;
	aClientSt.mContextMap.clear();
	aClientSt.mCookieId = 1;					// Mark as not empty
	aClientSt.mDefaultConnectId = 0;
	aClientSt.mDefaultContext = QString();
	aClientSt.mLogonForm = QString();
	aClientSt.mPartialMap.clear();
	aClientSt.mSavedReq = QString();
	aClientSt.mReqMap.clear();
	cOpenClients.append(aClientSt);

	// cReqTimer. Initialize request timer to go off every AHTTPSVR_REQSECS seconds.
	connect(&cReqTimer, SIGNAL(timeout()), this, SLOT(onReqTimeout()));
	cReqTimer.start(AHTTPSVR_REQSECS * 1000);
}

/*!
\brief Destructor Free allocated resources

close all clients.
\par Notes:
-# AHttpMgr has no remote ownership of any referents.
 */

AHttpSvr::~AHttpSvr()
{
	cCookieMap.clear();
	cConnectionMap.clear();
	cHttpStatus.clear();
	cSocketMap.clear();
	for (long aClientId = 0; aClientId < cOpenClients.size(); ++aClientId)
		closeClient(aClientId);
	cOpenClients.clear();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AHttpSvr()");
#endif
}

/*	---------------------------------------------------------------------------------------------------------------------------
buildResponseHdr - Build a response header for returning result to client
Args:
	irEnctype	Content-Type may be text/html, text/xml, text/plain, ...
	iHttpStatus 200 OK, 303 see other, 404 not found
	iBytesSent	Content-Length is number of bytes in body of response
	irUrl		Url if redirect using 303
	irReqHdr	Req hdr holds target, act, Set-Cookie
	orRspHdr	Place to put response header
Returns:
	nothing
Notes:
 1.	Location included iff redirect 303; else, Content-Length included.
 2. Set-Cookie is a session cookie included in first response to a client.
 3. Cache-Control iff irModified is true.
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpSvr::buildResponseHdr(const QString& irEnctype, long iHttpStatus, long iBytesSent, const QString& irUrl,
    const AStringMap& irReqHdr, QString& orRspHdr, const QDateTime& irModified)
{
    Q_UNUSED(irModified);

	// Date. Get current GMT.
	QString aTime = ATimeStamp::timeStamp(true);

	// LastModified. Set the last-modified time: Fri, 24 Dec 1999 06:21:42 GMT
	QString aLastModified;
	if (iHttpStatus != 404)
		aLastModified = "Cache-Control: no-cache\r\n";
	/* Do not set LastModified to last time if first file access. We need to set a cookie first to have a place to save timestamp
	if (irModified.isNull())
		aLastModified = "Cache-Control: no-cache";
	else
	{	// Convert to GMT
		QDateTime aModified(irModified.toUTC());

		// Format the Last-Modified time as per HTTP standards
		QString aDateTime = aModified.toString("ddd MMM d,yyyy hh:mm:ss");
		aLastModified.sprintf("Last-Modified: %s GMT", aDateTime.toLatin1().data());
	} */
	// Content-Type. Set content type
	QString aContentType;
	if (!irEnctype.isEmpty())
		aContentType.sprintf("Content-Type: %s\r\n", irEnctype.toLatin1().data());

	// Cookie. Set cookie if a pending set-cookie request
	QString aCookie("");
	if (irReqHdr.contains("Set-Cookie"))
		aCookie.sprintf("Set-Cookie: %s=%s\r\n", AHTTPSVR_COOKIENAME, irReqHdr["Set-Cookie"].toLatin1().data());

	// Location. Redirect to the specified url
	QString aLocation;
	if (iHttpStatus == 303 && !irUrl.isEmpty())
		aLocation.sprintf("Location: %s\r\n", irUrl.toLatin1().data());

	// Content-Length. Note the length of the returned body in bytes.
	QString aLength;
	if (iBytesSent > 0)
		aLength.sprintf("Content-Length: %ld\r\n", iBytesSent);

	// Header. Put these elements in the header
	orRspHdr.sprintf("HTTP/1.1 %ld %s\r\n"
		"Server: %s\r\n"
		"Date: %s\r\n"
		// "Connection: Keep-Alive\r\n" // Not needed for HTTP 1.1
		"%s"						// Last-Modified or cache-control
		"%s"						// Content-Type
		"%s"						// SetCookie, if any
		"%s"						// Location or transfer type
		"%s",						// Content-Length
		iHttpStatus, cHttpStatus[iHttpStatus], AHTTPSVR_NAME, aTime.toLatin1().data(),aLastModified.toLatin1().data(),
		aContentType.toLatin1().data(), aCookie.toLatin1().data(),aLocation.toLatin1().data(),aLength.toLatin1().data());

	/*  Sample response headers
	HTTP/1.0 200 OK
	Server: Microsoft-IIS/3.0
	Date: Wed, 27 Jan 1999 23:50:04 GMT
	Content-type: text/html
	Last-modified: Fri, 26 Dec 1999 06:21:42 GMT
	Set-Cookie: ASPSESSIONID=YEHPOEYLFPROWSIB; path=/

	HTTP/1.1 303 See Other
	Server: Microsoft-IIS/4.0
	Date: Thu, 28 Jan 1999 00:07:30 GMT
	Location: http://www.jbc.org
	Content-Type: text/html
	Set-Cookie: abaseid=GMJBLCEBEBOAHNHMKIGLPIJL
	Cache-control: no-cache
	 */
}

/*	---------------------------------------------------------------------------------------------------------------------------
closeClient - Recover abandoned client structure
Args:
	iClientId	Index into cOpenClients array
Returns:
	true iff entry found in cOpenClients
Note:
 1.	For now, outstanding requests are removed. May be better to return results to the client.
 2. cSocketMap: Key: ->socket, Value: client ID
	------------------------------------------------------------------------------------------------------------------------ */
bool AHttpSvr::closeClient(long iClientId)
{
	bool aFoundIt = false;
	AHttpClientSt* apClientSt;
	QTcpSocket* apSocket;
	if (iClientId > 0 && iClientId < cOpenClients.size() && (apClientSt = &cOpenClients[iClientId]) != NULL)
	{	// Maps. In a loop through the entries in a map, the iterator apIt is invalidated by arMap.remove(apIt). Any
		// subsequent operation, such as ++aItp may throw an exception. So, just get all the keys and iterate through them.
		AHttpSockMap& arMap = cSocketMap;
		ASocketList aKeys(arMap.keys());
		ASocketList::iterator apIt;
		for (apIt = aKeys.begin(); apIt != aKeys.end(); ++apIt)
		{	// Close. Close those sockets in the socket map that belong to this client.
			if ((apSocket = *apIt) != NULL && cSocketMap.value(apSocket) == iClientId)
			{	closeSocket(apSocket);				// Note. closeSocket removes the socket from cSocketMap.
				aFoundIt = true;
			}
		}
		// Update maps
		cCookieMap.remove(apClientSt->mCookieId);	// Key: cookie ID, Value: client ID
		if (!apClientSt->mContextMap.isEmpty())		// Key: context name, Value: connect ID
		{	AStringIntMap& arMap = apClientSt->mContextMap;
			AStringIntMap::Iterator apIt;
			for(apIt = arMap.begin(); apIt != arMap.end(); ++apIt)
				cConnectionMap.remove(*apIt);		// Key: connect ID, Value: client ID
			// Don't close connection to avoid endless loop.
		}
		// Remove entry from cOpenClients
		apClientSt->mContextMap.clear();
		apClientSt->mCookieId = 0;				// Mark as empty
		apClientSt->mDefaultConnectId = 0;
		apClientSt->mDefaultContext = QString();
		apClientSt->mLogonForm = QString();
		apClientSt->mPartialMap.clear();
		apClientSt->mReqMap.clear();
		apClientSt->mSavedReq = QString();
		aFoundIt = true;
	}
	return aFoundIt;
}

/*	---------------------------------------------------------------------------------------------------------------------------
closeSocket - Recover abandoned socket structure
Args:
	iSocketId	Index into cOpenSockets array
Returns:
	aNSockets	Number of sockets remaining open on this client
Note:
 1.	Just closes the socket.  See onDisconnected for closing the last socket.
 2.	Removes all pending request on this socket. No use returning a response if socket is being closed.
 3. If client is still around, the close generate a TCP/IP close event at the client. Then, the client can mop up.
 2. Removes this entry from the socket map.
	------------------------------------------------------------------------------------------------------------------------ */
bool AHttpSvr::closeSocket(QTcpSocket* ipSocket)
{
	long aClientId = 0, aRqId;
	if (ipSocket != NULL)
	{	if (cSocketMap.contains(ipSocket))
		{	// Requests. Remove pending requests for this socket.
			AHttpClientSt* apClientSt;
			if ((aClientId=cSocketMap.value(ipSocket))>0&&(apClientSt=&cOpenClients[aClientId])!=NULL&&!apClientSt->mReqMap.isEmpty())
			{	// Maps. In a loop through the entries in a map, the iterator apIt is invalidated by arMap.remove(apIt). Any
				// subsequent operation, such as ++aItp may throw an exception. So, just get all the keys and go through them.
				AHttpReqMap& arMap = apClientSt->mReqMap;	// Key: RqId, Value: AHttpReqSt
				AKeyList aKeys(arMap.keys());
				AKeyList::iterator apIt;
				for (apIt = aKeys.begin(); apIt != aKeys.end(); ++apIt)
				{	AHttpReqSt& arReq = arMap[aRqId = *apIt];
					if (arReq.mpSocket == ipSocket)
						arMap.remove(aRqId);
				}
			}
			cSocketMap.remove(ipSocket);
		}
		ipSocket->close();
		ipSocket->deleteLater();
	}
	return (aClientId > 0);
}

/*!
\brief connectionClosed - An connection to a context was closed. Mop up.

\param iConnectId - Index into cConnectionMap to identify client.
\return false
\par Notes:
-# Called by AisSvr.closeConnection if this or some other protocol server closes connection.
-# Inherited from the AReturnRcvr abstract base class.
-# See onDisconnected which calls into AisSvr.closeConnection.
*/
bool AHttpSvr::connectionClosed(long iConnectId)
{
	long aClientId = 0, aMapConnectId;
	AHttpClientSt* apClientSt = NULL;
	bool aFoundIt = false;
	if (iConnectId > 0 && cConnectionMap.contains(iConnectId) && (aClientId = cConnectionMap[iConnectId]) > 0 &&
	aClientId < cOpenClients.size() && (apClientSt = &cOpenClients[aClientId])!= NULL)
	{	// ContextMap. Remove this connection from the context map.
		AStringIntMap& arMap = apClientSt->mContextMap;
		AStringIntMap::Iterator apIt;
		for(apIt = arMap.begin(); apIt != arMap.end(); ++apIt)
			if ((aMapConnectId = *apIt) == iConnectId)
			{	arMap.remove(apIt.key());
				if (apClientSt->mDefaultConnectId == iConnectId)
				{	apClientSt->mDefaultConnectId = 0;
					apClientSt->mDefaultContext = QString::null;
				}
				break;
			}
		// ConnectionMap. Remove this entry from the connection map
		cConnectionMap.remove(iConnectId);

		// Last Connection. If closing last connection on this socket, close the client as well.
		if (arMap.isEmpty())
			closeClient(aClientId);
		aFoundIt = true;
	}
	return aFoundIt;
}

/*	---------------------------------------------------------------------------------------------------------------------------
getCookie - Extract cookie ID from encrypted cookie. Validate cookie.
Args:
	irCookie	Cookie string from request header: abaseid=3MUYX3G318T4HGHLLS44HDB9V7
	ipSocket	Ptr to QSocket to get client's IP address
Return:
	aCookieId
Notes:
 1.	If a trusted environment, set new config parameter (say, HttpTrustClient) to allocate a
	new socket structure request even if no cookie has been set (not currently implemented)
 2.	When a cookie is not returned, a second cookie may be set. Later, one or both may be returned
	in subsequent requests. If a cookie with a larger cookieId is detected, the newest cookie is used.
 3.	The following four numbers are stored in the cookie
	ClientIp   - Client's IP address
	Reserved   - Currently the Server Start Time (secs since 1/1/2000)
	CookieId   - Incrementing integer assigned to each cookie from this server
	CreateTime - Time (secs since 1/1/2000) cookie was created
 4.	Cookies can be unavailable, lost, outdated, or spoofed.  Cookie errors:
		COOKIEIP		- Cookie IP address does not match Client's IP (client changed)
		COOKIENAME		- Cookie format is not abaseid=<alphanum chars>
		STALECOOKIE		- Cookie CreateTime before StartTime or cookieId is out-of-range
		COOKIEMISMATCH	- Client ID from connection map and client ID from socket map do not match (see onSubmit)
		LOSTCOOKIE		- Cookie not provided by client after valid connection established (see onSubmit)
		NOCOOKIE		- No valid cookie returned in header (see onSubmit)
	------------------------------------------------------------------------------------------------------------------------ */
long AHttpSvr::getCookie(const QString& irCookie, QTcpSocket* ipSocket)
{
	// Cookie. Extract last abaseid setting from cookie string.
	long aCookieId = -AERR_COOKIENAME;	// Cookie ID extracted from irCookie.
	ulong aCreateTime = 0;				// Time cookie was created [secs since 1/1/2000]
	QRegExp aRxPattern("abaseid\\s*=\\s*(\\w{20,26})", Qt::CaseSensitive);
	ACookieSt aCookieSt;				// Values extracted from cookie
	long aIdx = (irCookie.length() > 36 ? -36 : 0); // max cookie length: 34chars + 2spaces
	// PENDING AHttpSvr::getCookie - If more than one abaseid cookie, delete the older ones.
	if (aRxPattern.indexIn(irCookie, aIdx) >= 0)
	{	QString aCookie(aRxPattern.cap(1));
		QHostAddress aClientAddr = ipSocket->peerAddress();
		AEncrypt::unpackCookie(aCookie, gAis.mKey4, aCookieSt);

		// Stale Cookie. Cookie is outdated or out-of-range
		aCookieId = aCookieSt.mCookieId;
		aCreateTime = aCookieSt.mCreateTime;
		if (aCookieId <= 0 || aCookieId > cCookieId || aCreateTime < gAis.mStartTime)
			aCookieId = -AERR_STALECOOKIE;

		// Client IP Address. Client's IP address does not match original IP saved in cookie
		else if (aCookieSt.mClientIp != aClientAddr.toIPv4Address())
			aCookieId = -AERR_COOKIEIP;
	}
	return aCookieId;
}
/*	---------------------------------------------------------------------------------------------------------------------------
getPair - Get name-value pair from header: <name>: <value>\r\n
Args:
	ipC			-> beginning of header line.
	orName		Place to put attribute name
	orValue		Place to put attribute value
Return:
	true iff name-value pair found
	------------------------------------------------------------------------------------------------------------------------ */
bool AHttpSvr::getPair(char* ipC, QString& orName, QString& orValue)
{
	bool aGotIt = false;	// Found a name and a value.
	char *apB, aCh, *apE;	// ->Bow, cur Char, ->Eow
	// Name.
	for (apE = ipC; (aCh=*apE) != ':' && aCh != ' ' && aCh != '\r' && aCh != '\n' && aCh != '\0';)
		++apE;
	if (apE > ipC)
	{	aCh = *apE, *apE = '\0';
		orName = ipC; *apE = aCh;

		// Value.
		for (apB = apE; *apB == ' '; ++apB)
			;
		if (*apB == ':') ++apB;
		while (*apB == ' ')
			++apB;
		for (apE = apB; (aCh=*apE) != '\r' && aCh != '\n' && aCh != '\0';)
			++apE;
		if (apE > apB)
		{	aCh = *apE, *apE = '\0';
			orValue = apB; *apE = aCh;
			aGotIt = true;
		}
	}
	return aGotIt;
}

/*	---------------------------------------------------------------------------------------------------------------------------
onDisconnected - Called on signal from QServerSocket when a socket is unexpectedly closed.
Args:	none
Return:	nothing
Notes:
 1.	Also called by retOutput if connection unexpectedly becomes unavailable.
 2. The client is also closed if this is the last socket on this client; otherwise, stale client entries would pile up.
 3. If the client is still active, a new entry will be created when a new connection is established.
 4. May as well close socket, when new connection opened by the client, the old socket is NOT reused.
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpSvr::onDisconnected()
{
	// Close socket.  Remove this socket from its associated client.
	QTcpSocket* apSocket = (QTcpSocket*)sender();
	ACHECK(apSocket != NULL);
	long aClientId  = cSocketMap.contains(apSocket) ?  cSocketMap.value(apSocket) : 0;
	// qDebug("AHttpSvr.onDisconnected(), Closing socket connection. pSocket:%p ClientId:%ld", apSocket, aClientId);
	closeSocket(apSocket);

	// Last Socket. Close all connections after a suitable timeout if this is the last socket.
	long aSz = cOpenClients.size();
	if (aClientId > 0 && aClientId < aSz)
	{	// SocketMap. Check socket map for other sockets connected to the same client
		bool aLastOne = true;			// true iff no more sockets open on this connection.
		AHttpSockMap::iterator apIt;
		for (apIt = cSocketMap.begin(); apIt != cSocketMap.end(); ++apIt)
		{	if (*apIt == aClientId)
			{	aLastOne = false;
				break;
			}
		}
		// Connections. Close all the connections for this client
		if (aLastOne)
		{	// Maps. In a loop through the entries in a map, the iterator apIt is invalidated by arMap.remove(apIt). Any
			// subsequent operation, such as ++aItp may throw an exception. So, just loop thru a list of the values.
			AStringIntMap& arMap = cOpenClients[aClientId].mContextMap;	// Key: context name, Value: connect ID
			AKeyList aList(arMap.values());		// List of connect IDs in the context map for this client
			AKeyList::iterator apIt;
			ACloseMode aCloseMode = geDefault;	// Use default close mode for this connection's session
			long aCloseWait = -2;				// Use default connection timeout for the connection
			for (apIt = aList.begin(); apIt != aList.end(); ++apIt)
				gAis.mpAisSvr->closeConnection(*apIt, aCloseMode, aCloseWait);// May result in arMap.remove()
		}
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
onConnected - Called on signal from QSocket when connection is made
Args:	none
Return: nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpSvr::onConnected()
{
	// Debug. Uncomment to debug
	// qDebug("onConnected(), Connection established");
}

/*	---------------------------------------------------------------------------------------------------------------------------
onError - Called on signal from QSocket when socket error encountered
Args:
	iCode	QSocket error code
Return:	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpSvr::onError(QAbstractSocket::SocketError iCode)
{
	if (iCode != QAbstractSocket::RemoteHostClosedError)
	{	QTcpSocket* apSocket = (QTcpSocket*)sender();
		qDebug() << "AHttpSvr::onError(), Encountered TCP/IP error, " << apSocket->errorString() << " Code: " << iCode;
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
void AHttpSvr::onNewConnection()
{
	if (cPort <= 0) return;
	// Socket. Construct a QTcpSocket object when a new connection is established:
	QTcpSocket* apSocket = cpHttpServer->nextPendingConnection();

	connect(apSocket, SIGNAL(connected()), this, SLOT(onConnected()));
	connect(apSocket, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
	connect(apSocket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(onError(QAbstractSocket::SocketError)));
	connect(apSocket, SIGNAL(readyRead()), this, SLOT(onSubmit()));
}

/*	---------------------------------------------------------------------------------------------------------------------------
onReqTimeout - Periodically update the idle time of every outstanding request
Args:
	none
Returns:
	nothing
Notes:
 1.	This routine is called every AHTTPSVR_REQSECS seconds.
 2.	Updates the idle time of every pending request.
 3. If the idle time exceeds AHTTPSVR_IDLETIME seconds, the task is aborted.
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpSvr::onReqTimeout()
{
	long aClientId, aSz = cOpenClients.size(), aRqId;
	AHttpClientSt* apClientSt;
	for (aClientId = 1; aClientId < aSz; ++aClientId)
	{	if ((apClientSt = &cOpenClients[aClientId]) != NULL && !apClientSt->mReqMap.isEmpty())
		{	long aHttpStatus = 200, aStatus = AERR_TIMEOUT;
			AHttpReqMap& arMap = apClientSt->mReqMap;
			QString aOut("");
			// Maps. In a loop through the entries in a map, the iterator apIt is invalidated by arMap.remove(apIt). Any
			// subsequent operation, such as ++aItp may throw an exception. So, just get all the keys and iterate through them.
			AKeyList aKeys(arMap.keys());
			AKeyList::iterator apIt;
			for (apIt = aKeys.begin(); apIt != aKeys.end(); ++apIt)
			{	AHttpReqSt& arReq = arMap[aRqId = *apIt];
				// Abort. Abort this request if idle time has been exceeded.
				if ((arReq.mIdleTime += AHTTPSVR_REQSECS) >= AHTTPSVR_IDLETIME)
				{	retOutput(aStatus, aHttpStatus, arReq.mReqHdr, aOut, arReq.mpSocket);
					arMap.remove(aRqId);
				}
			}
		}
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
onSubmit - Retrieve transmission from TCP/IP connection.
Args:		none
Returns:	nothing
Updates:
Be sure to reflect any changes made here in the ahttpsvr.h documentation.
Notes:
 1.	It is possible to receive a complete request, a partial request or multiple requests.
 2.	If a complete request has been fetched, submit it to the engine for consideration.
 3.	In the case of a POST, the header must be parsed to find the length of the body in order to determine if the message is
	complete; thus, it is not feasible to just scan the transmission for a terminator of some kind.
 4.	When checking for a partial request the socket map is used to locate the socket structure.
 5.	On returning a response, the connect map is used to find the client ID from the connect ID.
 6.	An entry is added to the socket map on the first request on a new connection. The initial client ID is set to 0.
 7. A client structure is allocated on the first receipt of a valid cookie. A client ID is set to the index into cOpenClients.
 8. A connection structure is allocated on the first logon. Connect ID is set to index into AisSvr.cOpenConnections.
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpSvr::onSubmit()
{
	// pSocket. Initialize, get ptr to socket for this TCP/IP connection.
	QString aErr;							// Holds error messages
	QTcpSocket* apSocket = (QTcpSocket*)sender();	// Ptr to socket for this connection
	ACHECK(apSocket != NULL && cPort > 0);

	// ClientId. Look up index to client structure. Needed to locate saved partial reqs, if any.
	long aClientId = -1;					// Client ID from socket map (-1 if no entry)
	long aCookieId = -AERR_NOCOOKIE;		// Cookie ID from the cookie in the header
	long aLgth = 0, aPostLgth = 0;			// Total chars in input buffer, post body length.
	AHttpClientSt* apClientSt = NULL;		// Ptr to client structure
	QByteArray* apPartial = NULL;			// -> partial request leftover from previous transmission.
	if (cSocketMap.contains(apSocket))
	{	if ((aClientId = cSocketMap.value(apSocket)) > 0 && (apClientSt = &cOpenClients[aClientId]) != NULL &&
		apClientSt->mPartialMap.contains(apSocket))
			apPartial = &apClientSt->mPartialMap[apSocket];
	}
	// Load. Fetch next transmission from socket.
	long aMethod = HTTP_NONE;			// HTTP_GET, HTTP_POST, or HTTP_NONE
	char aCh;							// Current character in input stream
	char *apBfr, *apC, *apEnd;			// ->Input, ->Current, ->Eot
	QByteArray aInput;					// Input buffer
	QString aName, aValue;				// Header name-value pair
	QString aReqLine;					// Request-line for error reporting
	long aAvail, aRcvd;					// Bytes expected, added to input buffer from readBlock
	while ((aAvail = apSocket->bytesAvailable()) > 0)
	{	// Preload. Preload the partial buffer, if any, into aInput.
		if (apPartial != NULL)
		{	aLgth = apPartial->length();
			aInput = *apPartial;		// deep copy
			apPartial->truncate(0);
			apClientSt->mPartialMap.remove(apSocket);
			apPartial = NULL;
		}
		// Append. Append bytes from socket to input buffer.
		aInput.resize(aLgth + aAvail + 1);
		apBfr = apC = aInput.data();	// Must come after copy from partial and after resize.
		apEnd = apBfr + aLgth;
		if ((aRcvd = apSocket->read(apEnd, aAvail)) <= 0)
		{	aErr.sprintf("AHttpSvr::onSubmit(), read Fails. Expected:%ld, Received:%ld. Input:|%.256s|", aAvail, aRcvd, apBfr);
			LOGSYSMSG(geWarn, aErr);
		}
		// Resync. Skip leading whitespace.
		aLgth += aRcvd;
		apEnd += aRcvd;					// Null-terminated transmission.
		*apEnd = '\0';
		for (; (aCh = *apC) == '\r' || aCh == '\n' || aCh == ' ' || aCh == '\t'; ++apC, --aLgth)
			;
		// Scan. Scan input stream for the end of next request.
		while (aLgth > 0)
		{	// Method. Determine method (GET or POST)
			if (*apC == 'G' && *(apC + 1) == 'E' && *(apC + 2) == 'T' && *(apC + 3) == ' ')
				aMethod = HTTP_GET;
			else if (*apC == 'P' && *(apC + 1) == 'O' && *(apC + 2) == 'S' && *(apC + 3) == 'T')
				aMethod = HTTP_POST;
			else	// Garbled message.  Soldier on.
			{	aErr.sprintf("AHttpSvr::onSubmit(), Garbled message Input:|%.256s|", apC);
				LOGSYSMSG(geWarn, aErr);
				return;	// PENDING AHttpSvr::onSubmit().  These loops lead to a crash in Release mode. May need to experiment on best
				// way to resync. Seems like aLgth > 0, but nothing in rcv buffer.
				for (; aLgth > 0; ++apC, --aLgth)
				{	if (*apC == 'G' && *(apC + 1) == 'E' && *(apC + 2) == 'T')
					{	aMethod = HTTP_GET;
						break;
					}
					else if (*apC == 'P' && *(apC+1) == 'O' && *(apC+2) == 'S' && *(apC+3) == 'T')
					{	aMethod = HTTP_POST;
						break;
					}
				}
				// Quit. Quit if unable to resync.
				if (aLgth <= 0)
				{	aLgth = 0;
					return;
				}
			}
			//	Request-Line. Capture request for reporting.
			while (apC < apEnd && *apC != '\r' && *apC != '\n')
					++apC;
			aCh = *apC, *apC = '\0', aReqLine = apBfr, *apC = aCh;
			if (*apC == '\r') ++apC;
			if (*apC == '\n') ++apC;

			// Header.  Scan through header lines. Quit on blank line
			aCookieId = -AERR_NOCOOKIE;
			while (apC < apEnd && *apC != '\r' && *apC != '\n')
			{	// Content-Length or Cookie. Process just these two headers.
				if (aMethod == HTTP_POST && *apC == 'C' && *(apC+1) == 'o' && *(apC+2) == 'n' && *(apC+3) == 't')
				{	getPair(apC, aName, aValue);
					if (aName == "Content-Length")
						aPostLgth = aValue.toUInt();
				}
				else if	(*apC == 'C' && *(apC+1) == 'o' && *(apC+2) == 'o' && *(apC+3) == 'k')
				{	getPair(apC, aName, aValue);
					if (aName == "Cookie" && (aCookieId = getCookie(aValue, apSocket)) > 0)
					{	// Valid Cookie. Establish apClientSt asap in case this req is a partial request.
						long aNewClientId = 1;
						if (cCookieMap.contains(aCookieId) && (aNewClientId = cCookieMap[aCookieId]) > 0)
						{	if (aClientId != aNewClientId)
							{	// Cookie Mismatch. Bogus socket map entry.
								// PENDING AHttpSvr::onSubmit(). Close all the sockets and connections associated with old client, aClientId.
								if (aClientId > 0)
									aCookieId = -AERR_COOKIEMISMATCH;
								cSocketMap[apSocket] = aClientId = aNewClientId;
								apClientSt = &cOpenClients[aClientId];
							}
							// else, no change to aClientId or apClientSt
						}
						else //	First cookie. Set client and set maps.
						{	if (aNewClientId <= 0)
								cCookieMap.remove(cCookieId);		// Bogus entry in cookie map.
							aClientId = openClient(aCookieId, apSocket);
							apClientSt = &cOpenClients[aClientId];
						}
					}
				}
				// Eol. Move apC to end of header line.
				while (apC < apEnd && *apC != '\r' && *apC != '\n')
					++apC;
				if (*apC == '\r') ++apC;
				if (*apC == '\n') ++apC;
			}
			// Move past blank line on end of header.
			if (*apC == '\r') ++apC;
			if (*apC == '\n') ++apC;

			// Quit. Quit if incomplete header
			if (apC > apEnd)
			{	apC = apBfr;
				break;
			}
			// Body. If POST, quit if incomplete body. Let's hope we get the rest later.
			if (aMethod == HTTP_POST && apC + aPostLgth > apEnd)
			{	apC = apBfr;
				break;
			}
			// Unpack. Unpack request header.  Extract AMP from query string/POST body.
			AStringMap aReqHdr;
			apC += aPostLgth;				// -> end of post
			aPostLgth = apC - apBfr;		// aPostLgth is now length of request
			unpackHdr(apBfr, aPostLgth, aMethod, aReqHdr, apSocket);

			// Cookie.  No cookie or invalid cookie.
			if (aCookieId < 0)
			{	// Set Cookie.  Set cookie if no cookie has been set
				if (aClientId <= 0)
					setCookie(aReqHdr, apSocket);

				// First Request. Add entry to socket map.
				if (aClientId < 0)
				{	cSocketMap[apSocket] = aClientId = 0;

					// No error if cookie is missing on first request.
					if (aCookieId == -AERR_NOCOOKIE)
						aCookieId = 0;
				}
				// Second Request. No valid cookie even though one was set on first request.
				else if (aClientId == 0)
				{	// Exceptions.  Some browsers do not return a cookie on selected requests. Avoid undue alarm.
					if (aCookieId == -AERR_NOCOOKIE && aReqHdr["file-path"] == "/favicon.ico")
						aCookieId = 0;
				}
				// Lost Cookie. Client ID > 0 but now cookie is corrupted or missing.
				else if (aCookieId == -AERR_NOCOOKIE)
					aCookieId = -AERR_LOSTCOOKIE;
			}
			// Error. Do not log since AIS could be swamped with invalid requests.
			if (aCookieId < 0 )
			{	// No cookie. Redirect request to nocookie.htm For now, we do not redirect user to nocookie page.
				if (aCookieId == -AERR_NOCOOKIE)
					aCookieId = -AERR_NOCOOKIE;		// Placeholder for now
				qDebug("AHttpSvr::onSubmit(), %s,  Req:%s", gAis.mpErrMsgs[-aCookieId], aReqLine.toLatin1().data());
			}
			// Submit. Submit completed requests
			submit(aReqHdr, apSocket, apClientSt, aClientId);

			// Update Length. Deduct this request from the current input.
			for (; (aCh = *apC) == '\r' || aCh == '\n' || aCh == ' ' || aCh == '\t';)
				++apC, ++aPostLgth;
			apBfr = apC;
			aLgth -= aPostLgth;
		}	// while aLgth > 0

		// Save. Save the partial request, if any
		if (aLgth > 0)
		{	if (apClientSt != NULL)
			{	QByteArray& arPartial = apClientSt->mPartialMap[apSocket];
				arPartial = apC;			// Deep copy
				// aLgth = arPartial.length();	// Reset Lgth to Lgth of partial request
			}
			else
			{	aErr.sprintf("AHttpSvr::onSubmit(), No buffer allocated. Lost partial transmission"
				" of %ld chars. Input:|%.256s|", aLgth, apC);
				LOGSYSMSG(geWarn, aErr);
				aLgth = 0;
			}
		}
		else aLgth = 0;
	}	// while aAvail > 0
}

/*	---------------------------------------------------------------------------------------------------------------------------
openClient - Add a new entry to the array of client structures.
Args:
	iCookieId		CookieId for the client associated with this TCP/IP connection
	ipSocket		Incoming socket for this connection. Used to set socket map.
Return:
	aClientId		>0 index into cOpenClients
Note:
 1.	CreateTime and cookie ID set by setCookie when cookie was created.
	------------------------------------------------------------------------------------------------------------------------ */
long AHttpSvr::openClient(ulong iCookieId, QTcpSocket* ipSocket)
{
	// Create a new socket entry. First slot is reserved
	long aClientId, aSz = cOpenClients.size();
	for (aClientId = 1; aClientId < aSz; ++aClientId)
	{	if (cOpenClients[aClientId].mCookieId <= 0)
			break;
	}
	if (aClientId >= aSz)		// Add a new entry to the list.
	{	AHttpClientSt aClientSt;
		cOpenClients.insert(aClientId, aClientSt);
	}
	AHttpClientSt& arClientSt = cOpenClients[aClientId];
	arClientSt.mContextMap.clear();
	arClientSt.mCookieId = iCookieId;
	arClientSt.mDefaultConnectId = 0;
	arClientSt.mDefaultContext = cDefaultContextName;
	arClientSt.mLogonForm.truncate(0);
	arClientSt.mPartialMap.clear();
	arClientSt.mReqMap.clear();
	arClientSt.mSavedReq.truncate(0);

	// Update the maps.
	cCookieMap[iCookieId] = aClientId;
	cSocketMap[ipSocket] = aClientId;
	return aClientId;
}
/*	---------------------------------------------------------------------------------------------------------------------------
returnFile - Send contents of a file back to browser.
Args:
	irFileSpec	File specification including absolute path and file name.
	irReqHdr	Original request header for use by buildResponseHdr.
	ipSocket	Ptr to QSocket for this connection.
Return:
	status		>0 Status code if unable to read file.
Notes:
 1.	Returns file to client in 4K chunks in order to facilitate TCP/IP chunking process.
	------------------------------------------------------------------------------------------------------------------------ */
long AHttpSvr::returnFile(const QString& irFileSpec, const AStringMap& irReqHdr, QTcpSocket* ipSocket)
{
	// Check inputs. Open the file.
	long aStatus = 0;
	ACHECK(!irFileSpec.isEmpty() && !irReqHdr.isEmpty() && ipSocket != NULL);
	QFile aFile(irFileSpec);
	QFileInfo aFileInfo(aFile.fileName());
	// Read file and send in 4K blocks to socket.
	if (!aFile.open(QIODevice::Unbuffered | QIODevice::ReadOnly))		// 32 | 1
		aStatus = AERR_FILEREAD;
	else
	{	QString aRspHdr, aContentType("text/html");
		if (aFileInfo.suffix() == "xml")
			aContentType = "text/xml";
		QByteArray aBfr;
		aBfr.resize(4096);
		long aLgth = aFile.size();
		QFileInfo aFileInfo(aFile);
		QDateTime aMod = aFileInfo.lastModified();
		buildResponseHdr(aContentType, 200/*HttpStatus*/,  aLgth, cMt, irReqHdr, aRspHdr, aMod);
		aRspHdr += QString("\r\n");
		ipSocket->write(aRspHdr.toLatin1().data(), aRspHdr.length());
		while (!aFile.atEnd())
		{	aLgth = aFile.read(aBfr.data(), 4096);
			ipSocket->write(aBfr.data(), aLgth);
		}
		// ReqHdr Log. Uncomment to display response headers.
		aRspHdr.replace('\r', "");
		LOGREQHDRMSG(aRspHdr);
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
\param irAisOut - Returned message (defaults to an empty string).
\param ipData - Binary buffer holding object closure, if any
\param iDataSize - Number of bytes in binary buffer.
\param irDisplay - Return from writeln and display (defaults to an empty string).
\param irError - Returned error message (defaults to an empty string).
\param iClientData - Optional string submitted with a request and returned verbatim with the response
\return void
\par Notes:
 -# If completing a request for a task, move to the next request in the task.
 -# If last request of a task is completed, move to next task for this job.
*/
void AHttpSvr::returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue,const QString& irAisOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(irError);
    Q_UNUSED(iClientData);

	// Selected Returns.  Provide special processing for selected returns
	long aHttpStatus = 200;
	QString aErr, aOut(irAisOut);
	if (iReqType == geRetUrl)
		aHttpStatus = 303;
	else if (iReqType == geRetFile)
		AUtil::readFile(irAisOut, aOut);

	// Unexpected Pushed Output. Report error.
	if (iRqId <= 0)
	{	switch (iReqType)
		{
		// Don't bother to log this expected pushed output:
		case geCloseSession:
			aHttpStatus = 200;
		case geCloseConnection:
		case geDisplay:
		case geFcnDebug:
		case geFcnError:
		case geFcnFileOpen:
		case geFcnReadHtmlPage:
		// case geFcnReturnResult:	// Not expected. Converted to an enctype in AisMgr
		case geFcnRingBell:
		case geFcnEngineState:
		case geFcnSendToClient:
			break;
		default:
			qDebug("AHttpSvr::returnOutput(ConnectId:%ld, RqId:%ld, Status:%ld, ReqType:%s, RetValue:%ld, Out:|%s|, Display:%s), "
			"Unexpected pushed output.", iConnectId, iRqId, iStatus, REQNAME(iReqType), iRetValue, irAisOut.toLatin1().data(),
			irDisplay.toLatin1().data());
			// LOGSYSMSG(geWarn, aErr);
			break;
		}
	}
	else
	{	// Client ID. Extract client ID and socket structure for this connection
		long aClientId;
		AHttpClientSt* apClientSt;
		if ((aClientId=cConnectionMap[iConnectId])<=0||(apClientSt=&cOpenClients[aClientId])==NULL||!apClientSt->mReqMap.contains(iRqId))
		{	// getConsoleLog. Ok to abort this request on a timeout
			if (iReqType != geGetConsoleLog)
			aErr.sprintf("AHttpSvr::returnOutput(ConnectId:%ld, RqId:%ld, Status:%ld, ReqType:%s, RetValue:%ld, Out:|%s|, "
			"Display:%s), Unable to locate client or matching request for this connection.", iConnectId, iRqId, iStatus,
			REQNAME(iReqType), iRetValue, irAisOut.toLatin1().data(), irDisplay.toLatin1().data());
			LOGSYSMSG(geWarn, aErr);
		}
		else // Return response. Return response, remove the client.
		{	AHttpReqSt& arReqSt = apClientSt->mReqMap[iRqId];
			AStringMap& arReqHdr = arReqSt.mReqHdr;
			if (arReqHdr["mode"] == "async")
				qDebug("AHttpSvr::returnOutput(ConnectId:%ld, RqId:%ld, Status:%ld, ReqType:%s, RetValue:%ld, Out:|%s|, Display:"
				"%s), Cancel response to an async request.", iConnectId, iRqId, iStatus, REQNAME(iReqType), iRetValue,
				irAisOut.toLatin1().data(), irDisplay.toLatin1().data());
			else
				retOutput(iStatus, aHttpStatus, arReqHdr, aOut, arReqSt.mpSocket);
			apClientSt->mReqMap.remove(iRqId);
		}
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
retOutput - Return response to remote client
Args:
	iStatus		Status code >0 iff responding with an error message
	iHttpStatus	Response header status code
	irReqHdr	Original request header. (Holds Act, AMP-Msg, Async, ContextName, Format, Set-Cookie).
	irOut		Body of response (DEL-delimited string of name-value pairs)
	ipSocket	Ptr to QSocket for this request.
Return:	nothing
Notes:
 1.	Returns an XML document iff request was an XML document.
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpSvr::retOutput(long iStatus, long iHttpStatus, AStringMap& irReqHdr, const QString& irOut, QTcpSocket* ipSocket)
{
	// Socket Closed. Make sure that this socket has not been closed.
	if (!cSocketMap.contains(ipSocket))
		return;

	// If an error, return the prescribed error message
	QString aOut;
	bool aIsXml = irReqHdr["format"] == "xml";
	if (iStatus > 0)
	{	if (iStatus == AERR_GENERIC)	// Use message provided
		{	iStatus = 0;
			aOut = irOut;
		}
		else
			aOut.sprintf("error\177%s", gAis.mpErrMsgs[iStatus]);
	}
	else
		aOut = irOut;

	// Convert XML to XML doc
	QString aContentType;
	if (aIsXml && iHttpStatus == 200)
	{	// If returning a singleton value, prepend the element name "result"
		if (aOut.indexOf(QChar('\177')) < 0)
			aOut.prepend("result\177");

		// Set root element attributes. No RqId provided by or returned to Http clients.
		AStringMap aAttribs;
		QString aXml, aTarget;
		aAttribs["act"] = irReqHdr["act"];
		aAttribs["status"] = QString::number(iStatus);
		aAttribs["target"] = irReqHdr["target"];
		aAttribs["xtype"] = "return";
		AUtil::ampmsgToXml(aOut, aAttribs, aXml);
		aContentType = "text/xml";
		aOut = aXml;
	}
	else if (iHttpStatus != 404)
		aContentType = "text/plain";
	else
		aContentType = "text/html";

	// Url. Set URL on redirect.
	QString aRspHdr, aUrl;
	if (iHttpStatus == 303)				// See Other (Moved Temporarily)
	{	aUrl = aOut;
		aOut.sprintf("<HTTP><HEAD><TITLE>Analytic Information Server %s </TITLE></HEAD><BODY>"
			"Analytic Information Server<BR>Request is being redirected to %s</BODY></HTML>",
			AGBL_AISVERSION, aUrl.toLatin1().data());
	}
	// Log the returned response
	if (aOut.isEmpty())
		aOut = "true";

	// Send header+response to socket
	long aBytesSent = aOut.length();
	buildResponseHdr(aContentType, iHttpStatus, aBytesSent, aUrl, irReqHdr, aRspHdr, QDateTime::currentDateTime());
	QString aHdr(aRspHdr);
	aRspHdr += "\r\n" + aOut;
	long aSz = aRspHdr.length();
	ACHECK(ipSocket != NULL);
	if (ipSocket->write(aRspHdr.toLatin1().data(), aSz) < 0)
	{	QString aErr;
		aErr.sprintf("AHttpSvr::retOutput, Write fails. Lost response: %.128s", aOut.toLatin1().data());
		 LOGSYSMSG(geWarn, aErr);
		 closeSocket(ipSocket);
	}
	// ReqHdr Log. Uncomment to log response headers.
	aHdr += aOut + "\n\n";
	aHdr.replace('\r', "");
	LOGREQHDRMSG(aHdr);
}
/*	---------------------------------------------------------------------------------------------------------------------------
sendFile - Client sends a file to AIS employing MIME type multipart/form-data.
Args:
	iorReqHdr	Holds AMP-Msg, Content-Disposition, Content-Type, ContextName
Return:	nothing
Notes:
 1.	Saves the transmitted file in root folder of parent context.
 2.	Content-Type is multipart/form-data and Amp-Msg contains body.
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpSvr::sendFile(AStringMap& iorReqHdr)
{
	// Extract boundary from the Content-Type boundary field
	long aPos;		// Index into the body
	QString aAmpmsg, aBndry, aBody(iorReqHdr["amp-msg"]), aFileName, aName, aValue;
	QString& arType = iorReqHdr["content-type"];
	QRegExp aBndryRx("boundary *= *([^\\r\\n]+)");
	if (aBndryRx.indexIn(arType) <= 0) return;
	aBndry = aBndryRx.cap(1);

	// Extract the AMP message, if any, from the POST message.
	aBndryRx.setPattern("--" + aBndry + "(--)?\\r*\\n");
	QRegExp aHdrRx("content-disposition: *form-data; *name=\"([^\"]+)\" *"
		"(?:; *filename=\"([^\\r\\\n]+)\")?\\r*\\n", Qt::CaseInsensitive);
	QRegExp aValueRx("(?:content[^\\r\\n]+\\r*\\n)*\\r*\\n([^\\r\\n]+)\\r*\\n", Qt::CaseInsensitive);
	while ((aPos = aBndryRx.indexIn(aBody)) >= 0)
	{	// Found a boundary, look for a filename field
		if (!aBndryRx.cap(1).isEmpty()) break;		// Hit terminating boundary. Shouldn't happen.
		aPos += aBndryRx.matchedLength();
		aBody = aBody.mid(aPos);					// Avoid rescanning over and over.

		// Found a boundary. Look for a header.  Extract header name.
		if ((aPos = aHdrRx.indexIn(aBody)) < 0) break;	// No header. Shouldn't happen.
		aName = aHdrRx.cap(1);
		aFileName = aHdrRx.cap(2);
		aPos += aHdrRx.matchedLength();
		aBody = aBody.mid(aPos);

		// Found a header. Look for filename field in the header.
		if (!aFileName.isEmpty())
			break;

		// Else, extract value of this field
		if ((aPos = aValueRx.indexIn(aBody)) >= 0)
		{	aValue = aValueRx.cap(1);
			aPos += aValueRx.matchedLength();
			aBody = aBody.mid(aPos);
		}
		if (!aName.isEmpty())
			aAmpmsg += aName + '\177' + aValue + '\177';
	}
	if ((aPos = aAmpmsg.length() - 1) > 0) aAmpmsg.truncate(aPos);// Strip trailing DEL
	iorReqHdr["amp-msg"] = aAmpmsg;

	// Extract the file from the body of the POST message
	// Note aFileName = source file path, aName = destination file name.
	if (!aFileName.isEmpty())
	{	// Strip miscellaneous fields after file header
		aValueRx.setPattern("(?:content[^\\r\\n]+\\r*\\n)*\\r*\\n");
		if ((aPos = aValueRx.indexIn(aBody)) >= 0)
		{	aPos += aValueRx.matchedLength();
			aBody = aBody.mid(aPos);
		}
		// Find the end of the file contents
		// pending - call into AisSvr to get the HttpDir.
		if ((aPos = aBndryRx.indexIn(aBody)) > 0)
		{	// Locate the wwwroot directory for the current context
			long aContextId = -1;
			QString aContext;
			aBody = aBody.left(aPos);
			QString& arCtxName = (!iorReqHdr.contains("contextname") || iorReqHdr["contextname"].isEmpty()) ?
				 gAis.mGblParams["gblaisdefaultcontextname"] : iorReqHdr["contextname"];
			AStringMap* apContextParams = gAis.mpAisSvr->getContextParams(arCtxName, &aContextId);
			if (apContextParams != NULL)
			{	QString& arHttpDir = (*apContextParams)["httpdir"];
				aName.prepend(arHttpDir + '/');

				// Write the body to the file. No need to translate CRLFs
				QFile aF(aName);
				if (aF.open(QIODevice::WriteOnly))		// 2
				{	aF.write(aBody.toLatin1().data(), aPos);
					aF.close();
				}
			}
		}
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
void AHttpSvr::setContextName(long iConnectId, const QString& irContextName)
{
	if (cConnectionMap.contains(iConnectId))
	{	// Search for entry in map with a value equal to this connectId
		long aClientId, aSz = cOpenClients.size();
		if ((aClientId = cConnectionMap[iConnectId]) > 0 && aClientId < aSz)
		{	AStringIntMap& arMap = cOpenClients[aClientId].mContextMap;
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
}
/*	---------------------------------------------------------------------------------------------------------------------------
setCookie - Set a cookie on first request from a new connection.
Args:
	orReqHdr		Holds AMP-Msg, Content-Disposition, Content-Type, ContextName
	ipSocket		Used to get client's IP address.
Return:
	true if cookie is set
Notes:
 1.	Add a "Set-Cookie" entry in the ReqHdr.  buildResponseHdr will include a set-cookie header in the next response to client.
	------------------------------------------------------------------------------------------------------------------------ */
bool AHttpSvr::setCookie(AStringMap& orReqHdr, QTcpSocket* ipSocket)
{
	// Build a cookie using the new SocketId and encryption key
	QString aCookie;
	ACookieSt aCookieSt;
	QHostAddress aClientAddr = ipSocket->peerAddress();
	aCookieSt.mClientIp = aClientAddr.toIPv4Address();
	aCookieSt.mReserved = (ulong)gAis.mStartTime;		// Optional parameter
	aCookieSt.mCookieId = ++cCookieId;
	aCookieSt.mCreateTime = (ulong)ATimeStamp::localTime();
	AEncrypt::packCookie(aCookieSt, gAis.mKey4, aCookie);
	// aCookie: "ZR3DPGS8PT02NWSZBCSJZ2VJ8";
	orReqHdr["Set-Cookie"] = aCookie;
	return true;
}
/*	---------------------------------------------------------------------------------------------------------------------------
setLogonForm - Generate logon form to be returned on redirect.
Args:
	irContextName	Context name returned in form
	iReason			Reason code for failure
	ipClientSt		-> structure to hold form
	irUser			User logon name
Return:
	nothing
Notes:
 1.	This is the only dynamic form currently implemented.
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpSvr::setLogonForm(const QString& irContextName, long iReason, AHttpClientSt* ipClientSt, const QString& irUser)
{
	QString aForm, aReason("");
	if (iReason > 0)
		aReason.sprintf("<B>%s</B>", gAis.mpErrMsgs[iReason]);

	aForm.sprintf("<HTML><HEAD><TITLE>Analytic Information Server Logon Page</TITLE></HEAD>"
		"<BODY><H2>Welcome to Analytic Information Server (AIS)</H2><BR>%s<P>"
		"Please enter your logon name and password.  You may enter an application name to switch "
		"to a new application. When finished, select the Submit button.</P>"
		"<FORM METHOD=\"POST\" ACTION=\"amp.dll\">"
		"<INPUT TYPE=\"hidden\" NAME=\"_ais\" VALUE=\"logon\">"
		"<TABLE><TR><TD ALIGN=\"right\">Logon name:</TD>"
		"<TD><INPUT TYPE=\"text\" NAME=\"user\" VALUE=\"%s\" SIZE=16 MAXLENGTH=32></TD></TR>"
		"<TR><TD ALIGN=\"right\">Password:</TD>"
		"<TD><INPUT TYPE=\"password\" NAME=\"passwd\" SIZE=16 MAXLENGTH=32></TD></TR>"
		"<TR><TD ALIGN=\"right\">Application:</TD>"
		"<TD><INPUT NAME=\"context\" VALUE=\"%s\" SIZE=16 MAXLENGTH=32></TD></TR>"
		"<TR><TD></TD><TD ALIGN=\"center\"><INPUT TYPE=\"submit\" VALUE=\"Submit\"></TD>"
		"</TR></TABLE></FORM></BODY></HTML>",aReason.toLatin1().data(),irUser.toLatin1().data(),irContextName.toLatin1().data());
	ipClientSt->mLogonForm = aForm;
}
/*	---------------------------------------------------------------------------------------------------------------------------
submit - Submit next request to AIS.
Args:
	irReqHdr	Header containing request headers and the following:
		act			Speech act extracted from an ampmsg
		amp-msg		AMP msg extracted from query string or body of POST.
		contextname	Context name from query string, if any
		file-path	File specification from Request-Uri
		format		amp, file, or xml
		mode		Set to "async" if immediate acknowledgement required
		request-uri	/path/file?query
		target		Target Lambda extracted from ampmsg
	ipSocket		-> QSocket for this connection
	ipClientSt		-> Client structure. Null if no valid cookie received.
	iClientId		Index into cOpenClients. Zero if no valid cookie received.
Return:	nothing
Updates:
Be sure to reflect all changes made here in the ahttpsvr.h documentation.
Notes:
 1. ContextName from query string, if any, is in irReqHdr. If none, submit uses the
	cDefaultContextName assigned to this protocol server during startup.
 2. A client structure is allocated upon the first receipt of a valid cookie. An entry is added to cSocket map to relate
	the pSocket to the socket ID for this structure (see onSubmit).
 3. Only unprotected page requests and probes are allowed without an allocated socket structure.
 4. Logon (or autologon) required to get a connection (ConnectId > 0)
 5. Only unprotected page request, probe, or logon allowed without a connection.
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpSvr::submit(AStringMap& irReqHdr, QTcpSocket* ipSocket, AHttpClientSt* ipClientSt, long iClientId)
{
	// ConnectId. Look up connect ID for this client and context.
	// Open new connection if a new context on a client that already has a default context (i.e., logged on)
	long aConnectId = 0;			// Index into cOpenConnections array or 0 if no connection
	long aHttpStatus = 200;			// Status returned in response header. Status for 200 is OK.
	QString aContextName(irReqHdr["contextname"]), aHdr, aOut;
	bool aIsMt = aContextName.isEmpty();
	if (ipClientSt != NULL)
	{	if (aIsMt)
		{	aConnectId = ipClientSt->mDefaultConnectId;
			aContextName = ipClientSt->mDefaultContext;
		}
		else if (ipClientSt->mContextMap.contains(aContextName))
			aConnectId = ipClientSt->mContextMap[aContextName];
		else if (ipClientSt->mDefaultConnectId > 0)
		{	aConnectId = gAis.mpAisSvr->openConnection(ipClientSt->mDefaultConnectId, aContextName);
			ipClientSt->mContextMap[aContextName] = aConnectId;
		}
	}
	// Context Name. Set context name as follows:
	//	1. !context in URL (from unpackHdr())
	//	2. default setting in socket structure, if any.
	//	3. default context name for this protocol server, set when Http port opened
	if (aContextName.isEmpty()) aContextName = cDefaultContextName;
	irReqHdr["contextname"] = aContextName;

	// Page Requests. Honor page requests asap.
	long aStatus = 0;
	QString aAmpmsg(irReqHdr["amp-msg"]), aFileSpec;
	QString aAct(irReqHdr["act"]), aTarget(irReqHdr["target"]);
	QString& arFormat = irReqHdr["format"];
	bool aIsFile = (arFormat == "file");
	if (aIsFile)
	{	aFileSpec = irReqHdr["file-path"];
		if (aFileSpec.indexOf("logon.htm", Qt::CaseInsensitive) >= 0)
		{	if (ipClientSt != NULL && !(aOut = ipClientSt->mLogonForm).isEmpty())
				aStatus = AERR_GENERIC;		// Return form as custom message
			else
				aStatus = AERR_DISCONNECTED;
		}
		else if ((aStatus = gAis.mpAisSvr->getFileSpec(aConnectId, AISSVR_HTTP, aContextName, aFileSpec)) > 0)
		{	// Protected File. If no socket, return logon page; else, try autologon
			if (aStatus == AERR_ACCESS)
				aStatus = 0;
		}
		else if ((aStatus = returnFile(aFileSpec, irReqHdr, ipSocket)) <= 0)
			return;
	}
	if (aStatus <= 0)
	{	// Probe. Return immediate response if no speech act.
		bool aIsAis = (aTarget == "_ais");
		bool aIsLogon = (aIsAis && aAct == "logon");
		if  (aIsAis && aAct.isEmpty())
		{	aOut = "result\177";
			aStatus = AERR_GENERIC;			// Return custom message.
		}
		// No Socket.  If no socket allocated, logon is not allowed
		else if (ipClientSt == NULL)
			aStatus = AERR_NOCOOKIE;

		// Logon. Ampmsg = _ais|logon|user|%s|passwd|%s|context|%s|wait|%d
		// We could logon without a cookie, but next transmission may not be on same socket.
		// If no cookie were returned, there would be no way to associate connection with this client,
		// so we would have lots of stale connections that would have to be purged from time to time.
		else if (aIsLogon || aConnectId <= 0)
		{	long aTknSz, aUsrId;
			long aDefaultCloseWait = AHTTPSVR_CLOSEWAIT; // Default value in case no default close wait not provided
			QString aPasswd, aSavReq, aUsrName;
			if (aIsLogon)
			{	QStringList aTkns = aAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
				if ((aTknSz = aTkns.size()) >= 6)
				{	aUsrName = aTkns[3];
					aPasswd = aTkns[5];
					if (aTknSz >= 8)
					{	irReqHdr["contextname"] = aContextName = aTkns[7];
						if (aTknSz >= 10)
							aDefaultCloseWait = aTkns[9].toLong();
					}
				}
			}
			// Authenticate user.
			aOut = "";					// Cannot be null.
			aUsrId = gAis.mpAisSvr->logon(this,aDefaultCloseWait,AISSVR_HTTP,aUsrName,aPasswd,aContextName,&aConnectId,aOut);
			if (aUsrId > 0)
			{	// Revise the default connection to the logon context.
				ACHECK(ipClientSt != NULL);
				ipClientSt->mDefaultConnectId = aConnectId;
				ipClientSt->mDefaultContext = aContextName;
				ipClientSt->mContextMap[aContextName] = aConnectId;
				cConnectionMap[aConnectId] = iClientId;

				// Save Request. Submit saved request, else return logon response.
				if (aIsLogon)
				{	if (ipClientSt->mSavedReq.isEmpty())
					{	aStatus = AERR_GENERIC;		// Return custom message
						aOut.sprintf("connectid\177%ld\177usrid\177%ld", aConnectId, aUsrId);
					}
					else
					{	aFileSpec = ipClientSt->mSavedReq;
						ipClientSt->mSavedReq.truncate(0);
						ipClientSt->mLogonForm.truncate(0);
						aIsFile = true;
					}
				}
			}
			// Redirect. Logon fails on page request. Redirect client to logon form.
			else if (aIsFile || !ipClientSt->mLogonForm.isEmpty())
			{	if (aIsFile)			// Save original file request.
					ipClientSt->mSavedReq = aFileSpec;
				setLogonForm(aContextName, -aUsrId, ipClientSt, aUsrName);
				aStatus = AERR_GENERIC;
				aHttpStatus = 303;
				aOut = "/logon.htm";
			}
			else // Logon Fails. Return error. pending: Count the retries.
				aStatus = -aUsrId;
		}	// logon
	}
	if (aStatus <= 0)
	{	// Protected File. Return protected file
		if (aIsFile)
			aStatus = returnFile(aFileSpec, irReqHdr, ipSocket);

		// Submit. Submit AMP message to AisSvr.
		else
		{	// Request.  Save request info for use by returnOutput. Submit request.
			long aReqId;
			AHttpReqSt aReqSt;
			ACHECK(iClientId > 0 && aConnectId > 0 && ipClientSt != NULL);
			long aRqId = ++gAis.mpAisSvr->mRqId;
			bool aIsAsync = (irReqHdr["mode"] == "async");
			aReqSt.mIdleTime = 0;
			aReqSt.mReqHdr = irReqHdr;
			aReqSt.mpSocket = ipSocket;
			ipClientSt->mReqMap[aRqId] = aReqSt;
			if ((aReqId = gAis.mpAisSvr->submit(aAmpmsg, aIsAsync, aConnectId, aRqId, NULL/*Data*/)) < 0)
				aStatus = -aReqId;

			// Async. Return immediate acknowledgement if an async request.
			else if (aIsAsync)
			{	aStatus = AERR_GENERIC;
				aOut.sprintf("result\177%ld", aRqId);
			}
		}
	}
	// Error. Return an error message, redirect to a another page or return immediate response.
	if (aStatus > 0)
	{	// No cookie. Cookie expected, but not available.
		if (aIsFile && aStatus == AERR_BADFILENAME)
		{	aStatus = AERR_GENERIC;
			aHttpStatus = 404;
			aOut = "<HTML><HEAD><TITLE>Analytic Information Server - Page Not Found</TITLE></HEAD>"
				"<BODY><H3>Analytic Information Server</H3>"
				"Unable to locate resource.<BR>HTTP 404 - File not found.</BODY></HTML>";
		}
		else if (aStatus == AERR_NOCOOKIE)
		{	aStatus = AERR_GENERIC;
			aHttpStatus = 303;
			aOut = "nocookie.htm";
		}
		// Return an error message
		retOutput(aStatus, aHttpStatus, irReqHdr, aOut, ipSocket);
	}
}
/*	---------------------------------------------------------------------------------------------------------------------------
unpackHdr - Unwrap header.
Args:
	ipBfr		-> Received request
	iLgth		Length [chars] of request including POST body, if any.
	iMethod		HTTP_GET or HTTP_POST
	orReqHdr	String map. Key: attribute name. Value: attribute.
	ipSocket	-> Socket for this connection. It may change between transmissions.
Return:		nothing
Note:
	 1.	See submit above for a list of items added to orReqHdr (in addition to header itself).
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpSvr::unpackHdr(char* ipBfr, long iLgth, long iMethod, AStringMap& orReqHdr, QTcpSocket* ipSocket)
{
    Q_UNUSED(ipSocket);

	// Header. Save the attributes from the header in aReqHdr.
	char *apBeg, aCh, *apEnd, *apEow, *apMid;	// Ptrs into input bfr, next char in bfr.
	QString aContextName, aErr, aHdr;	// Requested context, error description, next header

	// Request-Uri. Extract Request-Uri from the Request-Line. <method> <request-uri> HTTP/<version>
	// Code is designed for speed.
	// ipBfr->Request-Line, apBeg->request-uri, apMid->end of request-uri apEnd->end of header
	for (apBeg = ipBfr; *apBeg == ' '; ++apBeg)
		;
	while (*apBeg++ != ' ')
		;
	for (apMid = apBeg+1; *apBeg == '/' && *apMid == '/'; ++apBeg, ++apMid)
		;
	// Request-Uri. Scan to end of request-uri. Eat multiple trailing slashes.
	apEnd = ipBfr + iLgth;			// Move apEnd to null on end of request
	for (apMid = apBeg; apMid < apEnd; ++apMid)
		if (*apMid == ' ' && *(apMid+1) == 'H' && *(apMid+2) == 'T' && *(apMid+3) == 'T' &&
				*(apMid+4) == 'P' && *(apMid+5) == '/')
		break;
	while (apMid > apBeg && *(apMid-1) == '/' && *(apMid-2) == '/') --apMid;

	aCh = *apMid, *apMid = '\0';		// Null-terminate request-uri
	aHdr = apBeg, *apMid = aCh;			// Deep copy
	orReqHdr["request-uri"] = aHdr;

	// ContextName. Extract context name, if any, from request-uri. /!<context-name>/amp.dll
	if (*(apBeg+1) == '!')
	{	// apBeg->context-name, apEow->end of context-name
		for (apEow = apBeg + 2; apEow < apMid && *apEow != '/'; ++apEow)
			;
		aCh = *apEow, *apEow = '\0';	// Null-terminate context name.
		aContextName = apBeg + 2, *apEow = aCh;
		apBeg = apEow;		// apBeg->request-uri past context-name
		orReqHdr["contextname"] = aContextName;
	}
	// Query String. Extract query string, if any, from request-uri. /amp.dll?name=value...
	char *apAmp = NULL;		// -> Query string
	long aPostLgth = 0;		// Length of query string or POST body.
	if (iMethod == HTTP_GET)
	{	// apBeg->request-uri, apEow->query string or end of request-uri, apMid->end of request-uri
		// Scan for query string.
		for (apEow = apBeg + 1; apEow < apMid && *apEow != '?'; ++apEow)
			;
		if (*apEow == '?')
		{	apAmp = apEow + 1;
			aPostLgth = apMid - apAmp;
		}
		else if (*apBeg=='/' && *(apBeg+1)=='a' && *(apBeg+2)=='m' && *(apBeg+3)=='p' && *(apBeg+4)=='.')
			apAmp = apMid;
		else	// File Path. Extract file path from request-uri
		{	aCh = *apMid, *apMid = '\0';	// Null-terminate request-uri
			aHdr = apBeg, *apMid = aCh;		// Deep copy
			orReqHdr["act"] = "";
			orReqHdr["amp-msg"] = "";
			orReqHdr["file-path"] = aHdr;
			orReqHdr["format"] = "file";
			orReqHdr["mode"] = "";
			orReqHdr["target"] = "";
		}
	}
	// Move apBeg past Request-Line
	for (apBeg = apMid+1; apBeg < apEnd && *apBeg++ != '\n';)
		;

	// Request Header. Scan thru the header lines until end-of-transmission or blank line.
	bool aIsXml = false;
	QString aName;
	while (apBeg < apEnd && *apBeg != '\r' && *apBeg != '\n')
	{	// apBeg->Hdr, apEow->end of name.
		if (!((*apBeg == 'A' || *apBeg == 'a') && *(apBeg+1) == 'c' && *(apBeg+2) == 'c'))
		{	// Get next header name. <name>: <value>\r\n
			apEow = apBeg;
			while (apEow < apEnd && (aCh = *apEow) != ':' && aCh != ' ' && aCh != '\r' && aCh!='\n')
				++apEow;
			aCh = *apEow, *apEow = '\0';	// Null-terminate attribute name
			aName = apBeg, *apEow++ = aCh;
			// Get the header value, if any
			// apMid->value, apEow->end of value
			while (apEow < apEnd && *apEow == ' ')
				apEow++;
			apMid = apEow;
			while (apEow < apEnd && *apEow != '\r' && *apEow != '\n')
				++apEow;
			aCh = *apEow, *apEow = '\0';	// Null-terminate value
			aHdr = apMid, *apEow++ = aCh;
			orReqHdr[aName] = aHdr;

			// Get content-length and content-type.
			if (((aCh=*apBeg)=='C'||aCh=='c') && *(apBeg+1)=='o' && *(apBeg+2)=='n' && *(apBeg+3)=='t')
			{	if ((aErr = aName.toLower()) == "content-length")
					aPostLgth = aHdr.toUInt();
				if (aErr == "content-type" && aHdr.indexOf(QString("/xml")) >= 0)
					aIsXml = true;
			}
			apBeg = apEow;
		}
		// Move to next header line.
		while(apBeg < apEnd && *apBeg++ != '\n')
			;
	}
	// Resync. Skip by blank line at end of header
	apEow = apBeg;
	if (*apBeg == '\r') ++apBeg;
	if (*apBeg == '\n') ++apBeg;

	// Request Header. Capture entire header for logging, including post body.
	// ipBfr->beg of req, ipEnd->null on end of req.
	*apEow = '\0';				// Just capture the header.
	aHdr = ipBfr;
	aHdr.replace('\r', "");

	// Post Body. apAmp -> post body.
	if (iMethod == HTTP_POST && aPostLgth > 0)
	{	apAmp = apBeg;
		aName = apAmp;
		aHdr += aName;
	}
	aHdr += '\n';
	LOGREQHDRMSG(aHdr);

	// AMP. Convert query string/post body from xml or url-encoded string to AMP
	// apAmp->beg of ampmsg, apMid->end of ampmsg
	if (apAmp != NULL)
	{	// aIsXml.  Check for xml= prefix.
		apMid = apAmp + aPostLgth;
		if (*apAmp == 'x' && *(apAmp+1) == 'm' && *(apAmp+2) == 'l' && *(apAmp+3) == '=')
		{	apAmp += 4;
			aIsXml = true;
		}
		// 	XML. Convert XML to AMP msg.
		QString aAct, aAmpmsg, aMode(""), aTarget;
		if (aPostLgth <= 0)
			aAmpmsg = "_ais";		// probe
		else if (aIsXml)
		{	// Decode. If XML is Url-encoded, decode it.
			aCh = *apMid;
			*apMid = '\0', aName = apAmp, *apMid = aCh;
			if (*apAmp == '%')
				AUtil::urlDecode(aName);
			
			// AMP. xmlToAmpmsg prepends TargetLambda and SpeechAct attribs, if any, to Ampmsg
			if (!AUtil::xmlToAmpmsg(aName, aAmpmsg, aAct, aContextName, aMode, aTarget))
			{	// Unable to convert to AMP
				aErr.sprintf("0, AHttpSvr::unpackHdr(), Can't parse XML doc: %.256s", aName.toLatin1().data());
				LOGSYSMSG(geWarn, aErr);
				aIsXml = false;
			}
		}
		else  // Url-encoded. Decode query string or POST form.
		{	// Mode. Extract mode attribute. Query must be of the form:
			//		mode=%s&someTarget=someAct
			bool aIsEval = false;
			if (*apAmp == 'm' && *(apAmp+1) == 'o' && *(apAmp+2) == 'd' && *(apAmp+3) == 'e' &&	*(apAmp+4) == '=')
			{	apBeg = apAmp + 5;		// Move past mode=
				for(apAmp = apBeg; *apAmp != '&' && apAmp < apMid; ++apAmp)
					;
				if (apAmp > apBeg)
				{	aCh = *apAmp;
					*apAmp = '\0', aMode = apBeg, *apAmp = aCh;
					if (apAmp < apMid)
						++apAmp;		// Move past first &
				}
			}
			// Eval. If query starts with _eval=, just replace first = with DEL
			if (*apAmp == '_' && *(apAmp+1) == 'e' && *(apAmp+2) == 'v' && *(apAmp+3) == 'a' &&	*(apAmp+4) == 'l')
			{	apAmp += 6;				// Move past _eval=
				aIsEval = true;
				aTarget="_ais";
				aAct = "eval";
			}
			else
			{	// Target. Extract the target Lambda from the query.
				for (apEow = apAmp; *apEow != '=' && apEow < apMid; ++apEow)
					;
				aCh = *apEow;
				*apEow = '\0', aTarget = apAmp;

				// Act. Extract the speech act from the rest of the query.
				if (apEow >= apMid)
					*apEow = aCh;
				else
				{	*apEow = '\177';
					for (apBeg = ++apEow; *apBeg != '&' && apBeg < apMid; ++apBeg)
						;
					aCh = *apBeg;
					*apBeg = '\0', aAct = apEow;
					if (apBeg >= apMid)
						*apBeg = aCh;
					else
						*apBeg++ = '\177';
				}
				// Replace '=' and '&' with DEL
				for (; apBeg < apMid; ++apBeg)
					if (*apBeg == '=' || *apBeg == '&')
						*apBeg = '\177';
			}
			// Decode. Url-decode input.
			aCh = *apMid;
			*apMid = '\0', aAmpmsg = apAmp, *apMid = aCh;
			AUtil::urlDecode(aAmpmsg);
			if (aIsEval)
				aAmpmsg.prepend("_ais\177eval\177exp\177");
		}
		if (aTarget.isEmpty())
			orReqHdr["target"] = "_ais";
		else
		{	orReqHdr["target"] = aTarget;
			orReqHdr["act"] = aAct;
		}
		orReqHdr["amp-msg"] = aAmpmsg;
		orReqHdr["file-path"] = "amp.dll";
		orReqHdr["format"] = (aIsXml ? "xml" : "amp");
		orReqHdr["mode"] = aMode;
	}
}

/*	Request Header Entries
	Accept				; Section 14.1
	Accept-Charset		; Section 14.2
	Accept-Encoding		; Section 14.3
	Accept-Language		; Section 14.4
	Authorization		; Section 14.8
	Allow				; Section 14.7
	Content-Language	; Section 14.12
	Content-Length		; Section 14.13
	Content-Location	; Section 14.14
	Content-MD5			; Section 14.15
	Content-Range		; Section 14.16
	Content-Type		; Section 14.17
	Expect				; Section 14.20
	Expires				; Section 14.21
	From				; Section 14.22
	Host				; Section 14.23
	If-Match			; Section 14.24
	If-Modified-Since	; Section 14.25
	If-None-Match		; Section 14.26
	If-Range			; Section 14.27
	If-Unmodified-Since	; Section 14.28
	Last-Modified		; Section 14.29
	Max-Forwards		; Section 14.31
	Proxy-Authorization	; Section 14.34
	Range				; Section 14.35
	Referer				; Section 14.36
	TE					; Section 14.39
	User-Agent			; Section 14.43
 */

