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
aisdev/asessionmgr/ahttpclient.cpp

														HTTP Client

HTTP Client implements an HTTP client to interact with web servers, including:
login, fetch web pages, save files to disk.  Implements both GET and POST methods.
HTTP Client inherits Qt's QHttp class.

CHANGE HISTORY
Version	Date		Who		Change
1.0116	11/30/2006	tlw		destructor. Remove referents for this class.
1.0057	 3/18/2005	tlw		Update documentation.
												---------------------------------
DOCUMENTATION
 1.	See plumbing/httpclient project for a stand-alone implementation.
 2.	See ahttpclient.h for class specifications.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include	<QtCore/QFile>
#include	<QtCore/QTextStream>
#include	<QtCore/QTimer>
#include	<QtCore/QUrl>
#include	"aglobals.h"			// AHTTPCLIENT_SZ, etc.
#include	"ahttpclient.h"			// AHttpClient
#include	"asessionmgr.h"			// ASessionManager
#include	"aisoutput.h"				// AisMgrOutputEvent

//	--------------------------------------------------- PUBLIC METHODS --------------------------------------------------------
// AHttpClient constructor  (args in same order as in class declaration)
AHttpClient::AHttpClient(ASessionManager* ipSMgr, long iSessionId,  long iMsecToWait, QObject* ipParent, const char* ipName)
	:	QHttp(ipParent), cpSMgr(ipSMgr), cRcvd(0), cReqId(0), cSessionId(iSessionId)
{
    Q_UNUSED(ipName);

	init(iMsecToWait);
}

/*!
\brief Destructor Free allocated resources

\par Notes:
-# AHttpClient has remote ownership of a QTimer referent.
-# Since instances of AHttpClient are never assigned copied, or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
AHttpClient::~AHttpClient()
{
	if (cpTimer != NULL)
		cpTimer->stop();
	delete cpTimer;

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AHttpClient()");
#endif
}

/*	---------------------------------------------------------------------------------------------------------------------------
abortTask - Abort the request for a web page
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpClient::abortTask()
{
	cBody.truncate(0);
	cNewHost.truncate(0);
	cReqUri.truncate(0);
	cPendingTask = ceAborting;
	abort();
	// cReqId = closeConnection();
}

/*	---------------------------------------------------------------------------------------------------------------------------
cancelRequestHttp - Cancel the wait for the page
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpClient::cancelRequestHttp()
{
	cRcvBfr[0] = '\0';
	cpSMgr->notifyRequestHttp(cSessionId, cRcvBfr);
}

/*	---------------------------------------------------------------------------------------------------------------------------
dataSlot - Accept more data from the TCP connection to the server
Args:
	irHdr	Response header received from the server
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpClient::dataSlot(const QHttpResponseHeader& irHdr)
{
    Q_UNUSED(irHdr);

	cpTimer->stop();
	// Make room for new data
	long aRcvSize = cRcvBfr.size();
	long aLgth = bytesAvailable();
	if (cRcvd + aLgth >= aRcvSize)
	{	long aNeed = cRcvd + aLgth + 1 - aRcvSize;	// Make room for null
		aRcvSize += (aNeed < AHTTPCLIENT_SZ) ? AHTTPCLIENT_SZ : aNeed;
		cRcvBfr.resize(aRcvSize);
	}
	// Append received bytes to RcvBfr.
	char* apDst = cRcvBfr.data() + cRcvd;
	read(apDst, aLgth+1);
	cRcvd += aLgth;
	*(apDst + cRcvd) = '\0';
	cpTimer->start(cMsecToWait);
}

/*	---------------------------------------------------------------------------------------------------------------------------
init - Setup HttpClient
Args:
	iDefMsecToWait	Maximum time [msecs] to wait for an HTTP web page to be returned
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpClient::init(long iDefMsecToWait)
{
	// Initialize object
	cCookie = cFileName = cHost = cRcvBfr = "";
	cpResponseHdr = NULL;

	// Initialize timer used to wait for response from server.
	cpTimer = new QTimer(this);
	cpTimer->setSingleShot(true);
	cDefMsecToWait = (iDefMsecToWait > 0) ? iDefMsecToWait : AHTTPCLIENT_MSEC;

	// Connect network signals to webpage slots
	connect(cpTimer, SIGNAL(timeout()), this, SLOT(timerDoneSlot()));
	connect(this, SIGNAL(responseHeaderReceived(const QHttpResponseHeader&)), this, SLOT(responseHeaderReceivedSlot(const QHttpResponseHeader&)));
	connect(this, SIGNAL(readyRead(const QHttpResponseHeader&)), this, SLOT(dataSlot(const QHttpResponseHeader&)));
	//connect(this, SIGNAL(requestStarted(int)), this, SLOT(onRequestStarted(int)));
	connect(this, SIGNAL(requestFinished(int, bool)), this, SLOT(onRequestFinished(int, bool)));
	// Uncomment for debugging purposes
	// connect(this, SIGNAL(stateChanged(int)), this, SLOT(stateChangedSlot(int)));

	// Set the error messages
	cpErrMsgs[0] = "NoError - No error occurred";
	cpErrMsgs[1] = "UnknownError - An unknown error has occurred";
	cpErrMsgs[2] = "HostNotFound - The host name lookup failed";
	cpErrMsgs[3] = "ConnectionRefused - The server refused the connection";
	cpErrMsgs[4] = "UnexpectedClose - The server closed the connection unexpectedly";
	cpErrMsgs[5] = "InvalidResponseHeader - The server sent an invalid response header";
	cpErrMsgs[6] = "WrongContentLength - Mismatch between content-length and data received";
	cpErrMsgs[7] = "Aborted - The request was aborted with abort()";
	cpErrMsgs[8] = "AuthenticationRequiredError - The web server requires authentication to complete the request";
	cpErrMsgs[9] = "ProxyAuthenticationRequiredError - QHttp is using a proxy, and the proxy server requires authentication to establish a connection";

	// Set the state messages
	cpState[0] = "Unconnected - There is no connection to the host.";
	cpState[1] = "HostLookup - A host name lookup is in progress.";
	cpState[2] = "Connecting - An attempt to connect to the host is in progress.";
	cpState[3] = "Sending - The client is sending its request to the server.";
	cpState[4] = "Reading - The client's request has been sent and the client is reading the server's response.";
	cpState[5] = "Connected - The connection to the host is open, but not sending or waiting for a response.";
	cpState[6] = "Closing - The connection is closing down, but is not yet closed.";
}

/*	---------------------------------------------------------------------------------------------------------------------------
launchRequest - Initialize request header,
Args:
	no one
Returns:
	nothing

Notes:
 1.	By now connection is established, cHost has been set.
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpClient::launchRequest()
{
	QString aMethod((cBody.isEmpty()) ? "GET": "POST");
	QHttpRequestHeader aReqHdr(aMethod, cReqUri, 1, 1);
	aReqHdr.setValue("Accept-Language", "en-US");
	aReqHdr.setValue("Connection", "Keep-Alive");
	aReqHdr.setValue("Host", cHost);
	aReqHdr.setValue("User-Agent", "AnalyticInformationServer-AIS/1.0");
	if (!cBody.isEmpty())
	{
		aReqHdr.setContentType("application/x-www-form-urlencoded");
		aReqHdr.setContentLength(cBody.length());
	}
	cPendingTask = ceRequesting;
	if (!cCookie.isEmpty())
	 	aReqHdr.setValue("Cookie", cCookie);
	if (cBody.isEmpty())			// GET
		cReqId = request(aReqHdr);
	else							// POST
	{	QByteArray aBody = requestToUrl(cBody, '\177');
		cReqId = request(aReqHdr, aBody);
	}
	cpTimer->start(cMsecToWait);
}

/*	---------------------------------------------------------------------------------------------------------------------------
launchSetHost - Set the new web server host name
Args:
	no one
Returns:
	nothing
Notes:
 1.	By now the connection is closed and cNewHost has been set.
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpClient::launchSetHost()
{
	// If no new host available, quit.
	if (cNewHost.isEmpty())
	{	cRcvBfr = "Error\177AHttpClient:SetHost(), No new host available.";
		cpSMgr->notifyRequestHttp(cSessionId, cRcvBfr);
	}
	else	// Split cHost into host and port
	{	cHost = cNewHost;
		cNewHost.truncate(0);
		QUrl aUrl(cUrl);
		cPendingTask = ceSettingHost;
		bool aIsHttps = (aUrl.scheme().compare("https", Qt::CaseInsensitive) == 0);
		cReqId = setHost(aUrl.host(), 
			(aIsHttps) ? QHttp::ConnectionModeHttps : QHttp::ConnectionModeHttp,
			(aUrl.port() > 0) ? aUrl.port() : (aIsHttps) ? 443 : 80);
		cpTimer->start(cMsecToWait);
	}
}

void AHttpClient::launchSetUser()
{
	QUrl aUrl(cUrl);
	if (!aUrl.userName().isEmpty())
	{	
		cPendingTask = ceSettingUser;
		cReqId = setUser(aUrl.userName(), aUrl.password());
		cpTimer->start(cMsecToWait);
	}
	else
	{	launchRequest();
	}
}

void AHttpClient::onRequestStarted(int iReqId)
{
    Q_UNUSED(iReqId);
    //qDebug() << "AHttpClient::onRequestStarted: " << iReqId;
}

/*	---------------------------------------------------------------------------------------------------------------------------
onRequestFinished - Launch next task or return response to sender.
Args:
	iReqId	ID of this request
	iError	true iff error returned
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpClient::onRequestFinished(int iReqId, bool iError) throw()
{
    //qDebug() << "onRequestFinished: " << iReqId << ", " << iError;
	cpTimer->stop();
	QHttp::State aState = state();
	const char *apState = cpState[aState];
	if (cPendingTask == ceAborting)
	{	qDebug("AHttpClient::request aborted");
		cPendingTask = ceDone;
	}
	else if (iReqId == cReqId)		// Finished pending request
	{	if (iError)
		{	int aErrCode = error();
			// Send back error message to caller.
			cRcvBfr = "Error\177AHttpClient:requestFinished(), ";
			cRcvBfr += cpErrMsgs[aErrCode];
			cpSMgr->notifyRequestHttp(cSessionId, cRcvBfr);
			// Clean up the mess. Abort tasks.
			abortTask();
		}
		else if (cPendingTask == ceClosing)
		{	// If not unconnected, continue waiting for stateChanged()
			if (aState != QHttp::Unconnected)
			{	cPendingTask = ceDisconnecting;
				cpTimer->start(cMsecToWait);
			}
			else
			{	cPendingTask = ceDone;
				launchSetHost();
			}
		}
		else if (cPendingTask == ceSettingHost)
		{	cPendingTask = ceDone;
			if (!cReqUri.isEmpty())
				launchSetUser();
			else
			{	cRcvBfr = "Error\177AHttpClient:requestFinished(), Host set, but no request pending. State: ";
				cRcvBfr += apState;
				cpSMgr->notifyRequestHttp(cSessionId, cRcvBfr);
			}
		}
		else if (cPendingTask == ceSettingUser)
		{	cPendingTask = ceDone;
			if (!cReqUri.isEmpty())
				launchRequest();
			else
			{	cRcvBfr = "Error\177AHttpClient:requestFinished(), User set, but no request pending. State: ";
				cRcvBfr += apState;
				cpSMgr->notifyRequestHttp(cSessionId, cRcvBfr);
			}
		}
		else if (cPendingTask == ceRequesting)
		{	cPendingTask = ceDone;
			if (!cFileName.isEmpty())	// If filename provided, write results to file.
			{	QFile aF(cFileName);
				QTextStream aTs(&aF);
				if (!aF.open(QIODevice::Text | QIODevice::WriteOnly))		// 16 | 2
				{	cRcvBfr = "Error\177Unable to write to ";
					cRcvBfr += cFileName;
				}
				else
				{	aTs << cRcvBfr;
					cRcvBfr = "File\177";
					cRcvBfr += cFileName;
					aF.close();
				}
				cFileName = "";
			}
			// Finished HTTP request
			QString aResponseHdr;
			if (cpResponseHdr != NULL)
			{	aResponseHdr = cpResponseHdr->toString();
				if (cpResponseHdr->hasKey("set-cookie"))
					cCookie = cpResponseHdr->value("set-cookie");
				cpResponseHdr = NULL;
			}
			cpSMgr->notifyRequestHttp(cSessionId, cRcvBfr);
		}
	}
	else	// Unknown request. Abort pending tasks, return an error.
	{	cRcvBfr ="Error\177AHttpClient:requestFinished(), Unexpected request. ";
		if (iError)
		{	int aErrCode = error();
			cRcvBfr += cpErrMsgs[aErrCode];
		}
		cpSMgr->notifyRequestHttp(cSessionId, cRcvBfr);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
requestHttp - Request a web page from another user
Args:
	iUrl	URL of web page
	irBody	Body of POST request
	irFileName	Place to put the request
	iMsecToWait	Maximum time [msecs] to wait for web page
Returns:
	nothing
Notes:
 1.	All of this is somewhat involved in that QHttp's abort does not work as advertised.
 2.	Also QHttp does not support stacking up requests.  So if the sequence of tasks is closeConnection, setHost, send request,
	one must keep track of what needs to be done, wait for each task to finish and then check to see what remains to be done.
 3.	When a task is pending, the cReqId is positive.
		Pending task		Saved state
		closeConnection		none
		setHost				cNewHost holds new host name/IP
		request				cReqUri, cBody hold pending request
Examples:
	(postHttp "//192.168.38.14/amp.dll" "_ais=logon&usrname=tmay&logon=tim")
	(postHttp "//192.168.38.14/amp.dll" "_ais=noop&arg0=Hello")
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpClient::requestHttp(const QString& irUrl, const QString& irBody, const QString& irFileName, long iMsecToWait)
{
	// Initialize request state
	cUrl = irUrl;
	cBody = irBody;
	cFileName = irFileName;
	cMsecToWait = (iMsecToWait > 0) ? iMsecToWait : cDefMsecToWait;
	cNewHost.truncate(0);
	cRcvBfr[0] = '\0';
	cRcvd = 0;
	cReqId = 0;
	cpResponseHdr = NULL;

	// Extract Host and ReqUri, if any, from the url. For example.
	cReqId = -1;
	QUrl aUrl(irUrl);
	QString aNewHost = aUrl.host();
	cReqUri = aUrl.toEncoded(QUrl::RemoveScheme | QUrl::RemoveAuthority);
	if (cReqUri.isEmpty())
    {	cReqUri = "/";
	}

	// If a mismatch between url and current host, set cNewHost
	QHttp::State aState = state();
	if ((aNewHost.isEmpty() && cHost.isEmpty()) || (!aNewHost.isEmpty() && aNewHost != cHost))
	{	if (aNewHost.isEmpty())
			aNewHost = "localhost";
		cNewHost = aNewHost;
		cCookie.truncate(0);			// Reset cookie if changing hosts
		if (aState != QHttp::Unconnected)
		{	cPendingTask = ceClosing;
			cReqId = closeConnection();
			cpTimer->start(cMsecToWait);
		}
		else	// Skip closing, go right to setHost
			launchSetHost();
	}
	else if (aState == QHttp::Unconnected)
	{	cNewHost = cHost;
		launchSetHost();
	}
	else if (aState == QHttp::Connected)
		launchRequest();
	else
	{	cRcvBfr = "Error\177AHttpClient:requestFinished(), HttpClient busy. Try later. State: ";
		cRcvBfr += cpState[aState];
		cpSMgr->notifyRequestHttp(cSessionId, cRcvBfr);
		abortTask();
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
requestToUrl - Convert lines of the form name|value|... into url-Encoded string.
Args:
	irStg	Contains iSep-delimited string of name-value pairs
	iSep	Delimiting character
Returns:
	aOut	Coverted string
Notes:
 1.	Add this to AUtil class
 2.	iSep must be a character that does not appear in a name or a value, ever
	------------------------------------------------------------------------------------------------------------------------ */
QByteArray AHttpClient::requestToUrl(QString& irStg, char iSep)
{
	QByteArray aOut;
	QString aEntry;
	QStringList aLines(irStg.split(iSep, QString::KeepEmptyParts));
	QStringList::Iterator itp;
	for (itp = aLines.begin(); itp != aLines.end(); ++itp)
	{	QString& arName= *itp++;
		if (itp == aLines.end())
		{	aOut += arName + "&";
			break;
		}
        //const QString& arValue = urlEncode(*itp, true); // Use + for space
        const QString& arValue = QUrl::toPercentEncoding(*itp);
		aOut += arName + "=" + arValue + "&";
	}
	if (aOut.length() > 0)
		aOut.truncate(aOut.length() - 1);
	return aOut;
}

/*	---------------------------------------------------------------------------------------------------------------------------
responseHeaderReceivedSlot - Process received response header.
Args:
	irHdr	Received response header
Returns:
	nothing
Notes:
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpClient::responseHeaderReceivedSlot(const QHttpResponseHeader& irHdr)
{
	cpTimer->stop();
	cpResponseHdr = &irHdr;
	cpTimer->start(cMsecToWait);
}

/*	---------------------------------------------------------------------------------------------------------------------------
timerDoneSlot - Called to abort task that exceeded maximum wait time.
Args:
	none
Returns:
	nothing
Notes:
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpClient::timerDoneSlot()
{
	cpTimer->stop();
	cRcvBfr = "Error\177Timeout - unable to connect";
	cpSMgr->notifyRequestHttp(cSessionId, cRcvBfr);
	abortTask();
}

/*	---------------------------------------------------------------------------------------------------------------------------
urlEncode - URL-encode a string
Args:
	irStg	String to be encoded
	iUsePlus	If true convert space char into '+'; else, use %20
Returns:
	aOut	Encoded string
Notes:
 1.	Use the AUtil utility to do the encode
	------------------------------------------------------------------------------------------------------------------------ */
QString AHttpClient::urlEncode(QString& irStg, bool iUsePlus)
{
	long aLgth = irStg.length();	// Length of the input string
	if (aLgth <= 0) return "";

	// Put converted characters in aOut.
	QString aOut;					// Holds result + null
	for (long i = 0; i < aLgth; ++i)
	{	char aCh = irStg[(int)i].toAscii();
		char aHex;
		switch (aCh)
		{// Reserved characters (vf. HTML Def. Guide, p196)
		case ' ':
			if (iUsePlus)
			{	aOut += '+';
				break;
			}
		case ';':
		case '/':
		case '?':
		case ':':
		case '@':
		case '=':
		case '&':
		case '+':
		// Unsafe characters
		case '<':
		case '>':
		case '"':
		case '#':
		case '%':
		case '{':
		case '}':
		case '|':
		case '\\':
		case '^':
		case '~':
		case '[':
		case ']':
		case '`':	// backquote
			aOut += '%';
			aHex = aCh >> 4;
			aHex += (aHex <= 9) ? '0' : 'A' - 10;
			aOut += aHex;
			aHex = aCh & 0xF;
			aHex += (aHex <= 9) ? '0' : 'A' - 10;
			aOut += aHex;
			break;
		default:
			aOut += aCh;
			break;
		}
	}
	return aOut;
}

/*	---------------------------------------------------------------------------------------------------------------------------
stateChangedSlot - Note when web server has disconnected.
Args:
	iState	New HTTP request state
Returns:
	nothing
Notes:
 1.	OK to set a new host when disconnected from previous host.
 1.	After calling closeConnection we get:
		stateChanged(Closing...)
		stateChanged(Unconnected)
		requestFinished(4, false)
	------------------------------------------------------------------------------------------------------------------------ */
void AHttpClient::stateChangedSlot(int iState)
{
	qDebug() << "stateChangedSlot: " << iState;
	if (cPendingTask == ceDisconnecting && iState == QHttp::Unconnected)
	{	cpTimer->stop();
		qDebug("AHttpClient::stateChanged(), Finally disconnected");
		cPendingTask = ceDone;
		launchSetHost();
	}
}

// end
