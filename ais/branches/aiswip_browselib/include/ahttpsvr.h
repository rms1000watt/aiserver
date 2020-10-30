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

#ifndef AHTTPSVR_H
#define AHTTPSVR_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/ahttpsvr.h
										Http Server

Http Server implements an HTTP server for AIS

CHANGE HISTORY
Version	Date		Who		Change
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0069	9/14/2005	tlw		Add map of partial requests to AHttpClientReqSt.
1.0057	3/18/2005	tlw		Change request format. Allow multiple simultaneous connections.
					---------------------------------
	------------------------------------------------------------------------------------------------------------------------ */

//	------------------------------------------------------- IMPORTS -----------------------------------------------------------

// These includes are added here so that the meta-object compiler will include them in moc_ahttpsvr.cpp.
#include <QtCore/QDateTime>
#include <QtCore/QStringList>
#include <QtCore/QTimer>
#include <QtNetwork/QTcpSocket>

class AAisMgr;
class AUsrMgr;
#include "aglobals.h"		// gAis, ACHECK, SYSMSGs
#include "alogmgr.h"		// ALogMgr
#include "aissvr.h"			// AisSvr
class QTcpServer;
///class QTcpSocket;

//	------------------------------------------------------ DEFINITIONS --------------------------------------------------------
//	Maximum sizes
#define HTTPSVR_EXPIRE		(5 * 24 * 3600)	// Cookies expire after 5 days
#define HTTPSVR_MAXERR		 4				// Limit number of errors reported (conserve resources)

//	--------------------------------------------------- CLASS DECLARATIONS ----------------------------------------------------
// AHttpReqSt - One entry for each outstanding request. The request structure is initialized in submit with the original
// request header. The response is added in returnOutput if this request is not the first outstanding request (responses must
// be returned in order).
typedef struct
{	long		mIdleTime;				// Total time that this request has been pending.
	AStringMap	mReqHdr;				// Originating request header.
	QTcpSocket*	mpSocket;				// -> Initiating socket
} AHttpReqSt;
typedef QList<long> AKeyList;			// List of keys where the key type is long.

// AHttpSocketSt - One socket structure is allocated for every client. The structure is allocated by openSocket when a valid
// cookie is retrieved. One of these is kept in the Open sockets list for each active client.  Since a client may have multiple
// simultaneous connections, a map of partial requests for each TCP/IP connection is maintained.
typedef QMap<QTcpSocket*,QByteArray> APartialMap;// Key: QTcpSocket*, Value: Partial Req
typedef QMap<long, AHttpReqSt> AHttpReqMap;	// Key: RqId, Value: Request structure

typedef struct
{	AStringIntMap	mContextMap;		// Key: context name, Value: connect ID
	ulong			mCookieId;			// Unique ID assigned to every cookie. A sanity check.
	long			mDefaultConnectId;	// ConnectId if context name not included in request.
	QString			mDefaultContext;	// Default context name (set on logon)
	QString			mLogonForm;			// Saved logon form pending redirect.
	APartialMap		mPartialMap;		// Key connection, Value: Partial request received.
	AHttpReqMap		mReqMap;			// Key: RqId, Value: Pending request structure
	QString			mSavedReq;			// Saved request pending redirect.
} AHttpClientSt;
typedef QList<AHttpClientSt> AHttpClients;
typedef QMap<QTcpSocket*, long> AHttpSockMap;	// Key: -> current socket, Value: client ID
typedef QMap<long, long> AHttpCnctM;		// Key: connect ID, Value: client ID
typedef QList<QTcpSocket*> ASocketList;	// List of keys where the key type is ptr to QSocket.

/*!
\brief AHttpSvr -  Receives HTTP formatted requests over a TCP connection and responds to each request exactly once.

\par Requests.
Some overriding precautions dictate decisions made below.  Here are the guidelines:
- Public Request.  An "public" request is a request for an unprotected page or a probe, "_ais".
- Cookie. Only  public requests are allowed on a TCP socket without a cookie. A set-cookie header is returned in the
response header to the initial request.  A no-cookie error page is returned if a subsequent request is submitted on the
same socket without a valid cookie.  There are a few requests, such as favicon.ico, which are exceptions to this rule.
Minimal state is kept on a connection until a cookie is established.  Only an entry is added to the socket map (key is
a pointer to the QSocket object and the initial value is 0 until a client is established see Clients below).
- Clients.  A client is a machine returning a specific cookie.  Even if a machine has several browsers open to a server,
they all share the same session cookie.  On the return of the first cookie in the next request header, a client gets a
client ID and a client structure   A client may have one or more TCP connections (called sockets), open at the same time.
Sockets are not necessarily persistent. That is, the client may make each request on a new socket connection.  Any request besides
a public request requires a connection to an application. The socket map for this socket is updated with the clientId.
- Connections. Additional state is established upon a successful logon.  A single client may support one or more connections,
each associated with a separate instance of an application, called a context.  Each connection gets a connect ID and an
associated connection structure in the AisSvr class. See aissvr.h for details.
- Sessions.  Some requests require execution by the lisp engine within a context.  These requests require a session to be
opened.  A session may be opened automatically if one is not open upon the first request that requires a session.
- Summary
-# Cookie. A cookie is set in the response header of the response to the first public request from a new client.
-# Client ID. A client is established upon the receipt of a cookie in the request header of the next request.
-# Connect ID. A connection is established upon logon or upon the first non-public request with autologon enabled.
-# Session ID. A session is established upon opening a session or upon a request that requires accesse to the lisp engine.

\par Issues.
BadGuys. Suppose that a Bad Guy submits thousands of separate requests to a context that allows autologon. Then, the server
will be swamped and the main event loop will be clogged.  To thwart Bad Guys, we do the following: (NOT CURRENTLY IMPLEMENTED)
- If a request with no cookie is submitted, we set a busy-flag to true and process the request as noted above.  When the
response is returned, a one-shot timer is set for about 100 milliseconds.
- When the timer goes off, the busy-flag is set to false.
- Else, if a new request with no cookie (on another connection) is submitted while the busy-flag is true, a server-busy page
is returned.  This limits the server to serving about 10 new connections per second.

\par Stale Sockets.
Thousands of stale sockets may be created by illegitimate probes of the site that do not return a cookie. The
probe may be a single page request or a single query string. These sockets must be closed if they do not promptly make a second
request that includes a valid cookie and a subsequent logon.
We can utilize the same timer as described in the above paragraph to carry out the following:
- If the timer goes off before the client submits a request that contains a cookie all sockets without a valid cookie are
closed.

\par Request Idle Time.
If outstanding requests are pending for an extended time, some HTTP clients (e.g. IE) may timeout and
cause havoc.  To combat this problem, AIS periodically updates the idle time of every pending request.  If the total idle time
for a request exceeds AHTTPSVR_IDLETIME seconds, an error is returned.  If the request does complete at a later time, the
response is ignored.  To implement this feature the cReqTimer is set to timeout every AHTTPSVR_REQSECS.  The onReqTimeout
routine increases the idle time of every pending request by aHTTPSVR_REQSECS.  onReqTimeout aborts all requests with an idle
time greater than AHTTPSVR_IDLETIME seconds. (NOT CURRENTLY IMPLEMENTED)

\par Notes.
- A logon is system-wide.  On a successful logon, an entry is added in AisSvr's cOpenConnections.  The connect ID is the
index into the array of open connections.  The connection map is an content-associative array whose key is a connect ID
and whose value is the client ID for this client.   The connection map is used to relate a returned result with the
originating client.  The originating request is identified by the RqId returned with the request.
- If a request is made for a protected page prior to a logon (and autologon is not enabled), the client is redirected to a
logon form.  The original request is saved and resubmitted after the form's name-value pairs are returned.  If the request
is an AMP message, the request is denied.
- Upon receiving a remote request, the client ID is extracted from the socket map.  In addition, the context name, if any,
is extracted from the URL (http://www.byLambda3.com/!contextname/...).  If no context name is provided, the default context name
assigned to this protocol server is used.
- The client ID and the context name are used to look up the connectId in the cOpenClients list.
- The connect ID is used to look up the user, context, and session information in the AisSvr's cOpenConnections list.

\par Request Headers.
Every request is prefixed by a set of HTTP "headers".  The first header is called the request line.  Subsequent headers are
lines of the form: name: value\\r\\n.  setCookie and unpackHeader add a few entries to the header.  They are:
	set-cookie	Holds encrypted cookie for later use by buildResponseHdr.
	act			Speech act extracted from an ampmsg
	amp-msg		AMP msg extracted from query string or body of POST.
	contextname	Context name from query string, if any
	file-path	File specification from Request-Uri
	format		amp, file, or xml
	mode		Set to "async" if immediate acknowledgement required
	request-uri	/path/file?query
	target		Target Lambda extracted from ampmsg

A RqId (a server-wide incrementing integer) is assigned each incoming request.  An entry is added to the mReqMap keyed by
the msg ID.  The value of this entry is a request structure which includes the request header for the request.

\par Responses.
The body of the response may be an HTML web page or an XML document.  In either case, the return is prefaced with a response
header (see buildResponseHdr for details) followed by the body of the response.  A returned XML document has the following
format (see retOutput for more details):
\verbatim
	<amp act="ReqSpeechAct" status="0" target="ReqTargetLambda" xtype="return">body</amp>

where
 1.	ReqSpeechAct,ReqTargetLambda are replaced by the act and target values in the request header.
 2. Except if ReqTargetLambda is _eval, then target is set to _ais and act is set to eval.
 3. Special characters in the body text nodes are replaced by their predefined entities (see ampToXml in autilities.cpp).
 4. "0" is replace by the status code for this request (0 if no error).
 5. body may be some combination of the following:
		<result>some result...</result>
		<display>some console out...</display>
		<error>some error description...</error>
 \endverbatim
 */
class AHttpSvr : public QObject, public AReturnRcvr
{
	Q_OBJECT
public:
	AHttpSvr(ushort iPort, const QString& irContextName, QObject* ipParent=NULL);
	~AHttpSvr();
	virtual bool	connectionClosed(long iConnectId);
	virtual void	returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue
					, const QString& irAisOut, char* ipData, long iDataSize, const QString& irDisplay
					, const QString& irError, const QString iClientData);
	virtual void	setContextName(long iConnectId, const QString& irContextName);

private slots:
	void			onConnected();
	void			onDisconnected();
	void			onError(QAbstractSocket::SocketError iCode);
	void			onNewConnection();
	void			onReqTimeout();
	void			onSubmit();

private:
	void		ampmsgToHtml(QString& iorHtml, bool iUrl) throw();
	void		buildResponseHdr(const QString& irEnctype, long iHttpStatus, long iBytesSent, const QString& irUrl,
				const AStringMap& irReqHdr, QString& orRspHdr, const QDateTime& irModified);
	bool		closeClient(long iClientId);
	bool		closeSocket(QTcpSocket* ipSocket);
	long		getCookie(const QString& irCookie, QTcpSocket* ipSocket);
	bool		getPair(char* ipC, QString& orName, QString& orValue);
	long		openClient(ulong iCookieId, QTcpSocket* ipSocket);
	long		returnFile(const QString& irFileSpec, const AStringMap& irReqHdr, QTcpSocket* ipSocket);
	void		retOutput(long iStatus, long iHttpStatus, AStringMap& irReqHdr, const QString& irAisOut, QTcpSocket* ipSocket);
	void		sendFile(AStringMap& iorReqHdr);
	bool		setCookie(AStringMap& orReqHdr, QTcpSocket* ipSocket);
	void		setLogonForm(const QString& irContextName, long iReason, AHttpClientSt* ipClientSt, const QString& irUser);
	void		submit(AStringMap& irReqHdr, QTcpSocket* ipSocket, AHttpClientSt* ipClientSt, long iClientId);
	void		unpackHdr(char* ipBfr, long iLgth, long iMethod, AStringMap& orReqHdr, QTcpSocket* ipSocket);

	AHttpCnctM		cConnectionMap;		// Key: connect ID, Value: client ID
	long			cCookieId;			// Ever incrementing cookie ID assigned to cookie
	AHttpCnctM		cCookieMap;			// Key: cookie ID, Value: client ID
	QString			cDefaultContextName;// Protocol's default context name
	QString			cDefaultPage;		// Name of the default page (eg, index.htm)
	AIntCharMap		cHttpStatus;		// HTTP Status/description pairs
	QString			cMt;				// Empty string
	AHttpClients	cOpenClients;		// Currently active clients. Indexed by client ID
	ushort			cPort;				// Server listens on this port. 0 if disabled
	QTimer			cReqTimer;			// Used to update request idle time.
	AHttpSockMap	cSocketMap;			// Key: -> current socket, Value: client ID
	QTcpServer*		cpHttpServer;		// TCP Server listens on HTTP socket
};

#endif //AHTTPSVR_H


