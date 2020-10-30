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

#include "aglobals.h"
#include "atcpclient.h"

      
ATcpTaskId  ATcpClient::cTcpTaskId;

/*	---------------------------------------------------------------------------------------------------------------------------
\brief ATcpClient - initializes attributes and connects to<Action> signals to on<Action> slots. 
NOTES: 
Since it is impossible to pass the current task through a connected() signal to the onConnected() slot. 
This constructor connects to<Action>(void* ipTaskInfo) signals to on<Action>(void* ipTaskInfo). When the glue layer
calls a certain <Action> e.i. OpenConnection() the method will emit a to<Action>() signal and the on<Action>() slot
is activated. The ipTaskInfo parameter is setup by the <Action> and passed to the on<Action>() slot where the task complete
flag is being set to 'true'. This flag is being checked in the glue layer's event loop and breaks from it when the task is
complete to process the next <Action>.
	------------------------------------------------------------------------------------------------------------------------ */ 
ATcpClient::ATcpClient(long iProtocolId)
{   
    // initialize private attributes
    cCookie = "";	                  // Current cookie returned by server.
    cCurRequest = "";                 // Contains HTTP 1.1 header and body or for other uses
    cHostDnsIp = "";		          // Ais DNS name or IP address.
    cPort = 0;			              // AIS socket number
    cProtocolId = iProtocolId;        // Protocol (AISSVR_HTTP, ...) 
    cpTcpSocket = NULL;		          // Maintains TCP connection with server
    cXid = 0;			              // Client-generated request ID
    cTcpTaskId = 0;                   // Increments for every new task
    cTcpTasks.clear();                // Contains task ids as keys to the task information

    // connections
    connect(this,SIGNAL(toOpenConnection(ATcpTask*)),this,SLOT(onOpenConnection(ATcpTask*)),Qt::QueuedConnection);
    connect(this,SIGNAL(toCloseConnection(ATcpTask*)),this,SLOT(onCloseConnection(ATcpTask*)),Qt::QueuedConnection);
    connect(this,SIGNAL(toIsConnected(ATcpTask*)),this,SLOT(onIsConnected(ATcpTask*)),Qt::QueuedConnection);
    connect(this,SIGNAL(toSubmit(const QString &, const QString &, ATcpTask*)), this, SLOT(onSubmit(const QString &, 
                        const QString &, ATcpTask *)), Qt::QueuedConnection);
}

/*	---------------------------------------------------------------------------------------------------------------------------
\brief ~ATcpClient - clean up
	------------------------------------------------------------------------------------------------------------------------ */
ATcpClient::~ATcpClient(void)
{
    cTcpTasks.clear();
    if (cpTcpSocket != NULL)
        delete cpTcpSocket; 
}

/*	---------------------------------------------------------------------------------------------------------------------------
\brief closeConnection - Close the specified connection to AIS.
\return aTcpTaskId - Task ID (incrementing integer) established by submit
	------------------------------------------------------------------------------------------------------------------------ */
ATcpTaskId ATcpClient::closeConnection()
{
	ATcpTaskId aTcpTaskId = getNextTcpTaskId();
	cTcpMutex.lock();
    cTcpTasks[aTcpTaskId] = ATcpTask(aTcpTaskId);
	cTcpMutex.unlock();
    emit toCloseConnection(&cTcpTasks[aTcpTaskId]);
	return aTcpTaskId;
}

/*	---------------------------------------------------------------------------------------------------------------------------
\brief getNextTcpTaskId - retrieve the value of the next task id.
\return aTcpTaskId - Task ID (incrementing integer) established by submit
	------------------------------------------------------------------------------------------------------------------------ */
ATcpTaskId	ATcpClient::getNextTcpTaskId() 
{ 
    return ++cTcpTaskId;
}

/*!
\brief getReturnResult - Fetch the returned result after notification of completion of a task and clears requests from queue.
\param iTcpTaskId - The ID of the task that completed.
\param ipOk - Place to put return status.
\return aTcpTaskId - The info regarding this tcp task.
*/	
ATcpTask ATcpClient::getReturnResult(ATcpTaskId iTcpTaskId, bool *ipOk) 
{
	cTcpMutex.lock();
	*ipOk = false;
	ATcpTask aTcpTaskCopy;
	ATcpTasks::Iterator apIt = cTcpTasks.find(iTcpTaskId);
	if (apIt != cTcpTasks.end())
	{	
        if (apIt.value().mTcpTaskComplete)
		{	
            *ipOk = true;
			aTcpTaskCopy = apIt.value();
			cTcpTasks.remove(iTcpTaskId);
			cTcpMutex.unlock();
			return aTcpTaskCopy;
		}
		else
		{	cTcpMutex.unlock();
			return aTcpTaskCopy;
		}
	}
	cTcpMutex.unlock();
	return aTcpTaskCopy;
}

/*!
\brief isConnected - checks tcp client's connected state with the server
\return aTcpTaskId - unique incrementing integer assigned to this task
 */
ATcpTaskId ATcpClient::isConnected()
{
	ATcpTaskId aTcpTaskId = getNextTcpTaskId();
	cTcpMutex.lock();
    cTcpTasks[aTcpTaskId] = ATcpTask(aTcpTaskId);
	cTcpMutex.unlock();
    emit toIsConnected(&cTcpTasks[aTcpTaskId]);
    return aTcpTaskId;
}

/*	---------------------------------------------------------------------------------------------------------------------------
makeHttpRequestMsg - Construct request header
Args:
	irResPath	Of the form /default.html
    irBody      POST message if POST HTTP method otherwise empty.
	irHost		The DNS name or IP address of the server of the form localhost
Returns:
	nothing
Notes:
    Puts request header + body in cCurRequest
Example:
GET /default.htm HTTP/1.1
Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, * / *
Accept-Language: en
Connection: Keep-Alive
User-Agent: Analytic Information Server (AIS Client)
Accept-Charset: iso-8859-1,*,utf-8
Host: localhost
------------------------------------------------------------------------------------------------------------------------ */
void ATcpClient::makeHttpRequestMsg(const QString &irResPath, const QString& irBody, const QString& irHost)
{
    cCurRequest = (irBody.isEmpty()) ? "GET " : "POST ";
	cCurRequest += irResPath.isEmpty() ? "/" : irResPath; 
	cCurRequest += " HTTP/1.1\r\n"
			"Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, */*\r\n"
			"Accept-Language: en\r\n" 
			"Connection: Keep-Alive\r\n"
			"User-Agent: Analytic Information Server (AIS Client)\r\n"
			"Accept-Charset: iso-8859-1,*,utf-8\r\n";
	if (!irHost.isEmpty())
		cCurRequest += "Host: " + irHost + "\r\n" ;
    if (!cCookie.isEmpty())
		cCurRequest += "Cookie: " + cCookie + "\r\n";
    if (!irBody.isEmpty())
    {
        QString aBody = irBody;
        QString aPostBody = requestToUrl(aBody,'\177');
        cCurRequest.append(aPostBody);
    }
	cCurRequest += "\r\n";
}

/*!
\brief onIsConnected - slot for the TCP check Connection signal
\param ipTaskInfo - information of current tcp task
\return aXid - tcp task ID
*/
AXID ATcpClient::onIsConnected(ATcpTask* ipTaskInfo)
{
    Q_UNUSED(ipTaskInfo);

    long aXid = ++cXid;
    if(cpTcpSocket != NULL)
    {
        QString aOut = (cpTcpSocket->isConnected())? "true" : "false";
        returnMsg(cXid, aOut);
    }
    return aXid;
}

/*!
\brief onCloseConnection - slot for the TCP closeConnection signal
\param ipTaskInfo - information of current tcp task
\return aXid - tcp task ID
*/
AXID ATcpClient::onCloseConnection(ATcpTask* ipTaskInfo)
{
    Q_UNUSED(ipTaskInfo);

    long aXid = ++cXid;
    QString aOut = (cpTcpSocket->closeConnection())? "true" : "false";
    returnMsg(cXid, aOut);
    return aXid;
}
/*!
\brief onConnected - slot for the ASocket connected signal
*/
void ATcpClient::onConnected()
{
    returnMsg(cXid, "true");
}

/*	---------------------------------------------------------------------------------------------------------------------------
\brief onDisconnected - Catches disconnected signal from ASocket
\returns void
Notes:
 1. If the server terminates the connection or if the network fails, ASocket generates a disconnected signal which is caught
	here.
 2.	If closeConnection is called, it causes the server to terminate the connection, which generates the above sequence of
	events.
	------------------------------------------------------------------------------------------------------------------------ */
void ATcpClient::onDisconnected()
{
    returnMsg(cXid, "Tcp client disconnected from server.");
}

/*!
\brief onOpenConnection - Establish a new connection to an HTTP server
\param ipTaskInfo - holds the current task information
\return aXid - Unique incrementing integer assigned to this task
 */
AXID ATcpClient::onOpenConnection(ATcpTask* ipTaskInfo)
{
    Q_UNUSED(ipTaskInfo);

    long aXid = ++cXid;
    if (cpTcpSocket == NULL)
    {
        cpTcpSocket = new ASocket(this, cHostDnsIp, cProtocolId);
        connect(cpTcpSocket, SIGNAL(connected()), this, SLOT(onConnected()), Qt::DirectConnection);
        connect(cpTcpSocket, SIGNAL(response(QByteArray, QByteArray, char*)), this, SLOT(onResponse(QByteArray, QByteArray)),Qt::DirectConnection);
        connect(cpTcpSocket, SIGNAL(disconnected()), this, SLOT(onDisconnected()), Qt::DirectConnection);
        connect(cpTcpSocket, SIGNAL(socketError(long, QString)), this, SLOT(onSocketError()), Qt::DirectConnection);
    }   
    if (!cpTcpSocket->openConnection(cHostDnsIp, cPort))
    {
        returnMsg(cXid, "Tcp client failed to open connection.");
    }
    return aXid;
}

/*	---------------------------------------------------------------------------------------------------------------------------
onResponse - Gets response back from connection using HTTP.
\param iBody - Body of response returned by HTTP.
\param iCookie - Cookie value extracted from Set-Cookie: abaseid=GMJBLCEBEBOAHNHMKIGLPIJL
\param ipData - Not used.
\return void
	------------------------------------------------------------------------------------------------------------------------ */
void ATcpClient::onResponse(QByteArray iBody, QByteArray iCookie)
{
    QString asvrResponse;
    updateCookie(iCookie);
    asvrResponse = QString(iBody); 
    if (asvrResponse.isEmpty())
       returnMsg(cXid, "");
    else
       returnMsg(cXid, asvrResponse);
}

/*	---------------------------------------------------------------------------------------------------------------------------
\brief onSocketError - Catches the socketError signal from ASocket
\return void
	------------------------------------------------------------------------------------------------------------------------ */
void ATcpClient::onSocketError()
{
    returnMsg(cXid, "Tcp client socket error has occurred.");
}
/*	---------------------------------------------------------------------------------------------------------------------------
\brief onSubmit - sends a request to the respective server based on the provided cProtocolId 
\param irResPath - HTTP method
\param irBody - body of post message if POST HTTP method otherwise empty string
\param ipTaskInfo - application port in which tcp client will connect to
\returns aXid - returns the current task ID
     ------------------------------------------------------------------------------------------------------------------------ */
AXID ATcpClient::onSubmit(const QString& irResPath, const QString& irBody, ATcpTask* ipTaskInfo)
{
    Q_UNUSED(ipTaskInfo);

    long aXid = ++cXid;
    if (cpTcpSocket == NULL)
        return 0;
    if (!cpTcpSocket->isConnected())
        cpTcpSocket->openConnection(cHostDnsIp, cPort); //reconnect
    switch (cProtocolId)
    {
    case AISSVR_APP:
        break;
    case AISSVR_HTTP:
        {
        QString iPath = irResPath;
        iPath.prepend('/');
        makeHttpRequestMsg(iPath, irBody, cHostDnsIp);
	    cpTcpSocket->submit(cCurRequest, NULL, 0);
        }
        break;
    case AISSVR_XML:
        break;
    default: 
        break;
    }
	return aXid;
}

/*!
\brief openConnection - Establish a new connection by setting up a task and emitting a toOpenConnection
\return aTcpTaskId - Unique incrementing integer assigned to this tcp task
 */
ATcpTaskId ATcpClient::openConnection(const QByteArray& irServerAddress, ushort iPort)
{
	ATcpTaskId aTcpTaskId = getNextTcpTaskId();
	cTcpMutex.lock();
    cTcpTasks[aTcpTaskId] = ATcpTask(aTcpTaskId);
	cTcpMutex.unlock();
    cHostDnsIp = irServerAddress; 
    cPort = iPort;
    emit toOpenConnection(&cTcpTasks[aTcpTaskId]);
	return aTcpTaskId;
}

/*!
\brief returnMsg - primarily sets the task complete flag which exits the main eventloop
\param iXid - Request ID (incrementing integer) established by submit
\param irAisOut - Returned message
\return void
\par Notes:
 -# mAisOut may contains "true" or the HTML page when called from onResponse()
 */
void ATcpClient::returnMsg(AXID iXid, const QString& irAisOut) 
{
    cTcpMutex.lock();
	if (iXid > 0 && cTcpTasks.contains(iXid))
	{
        cTcpTasks[iXid].mTcpTaskComplete = true;
        cTcpTasks[iXid].mAisOut = irAisOut;
    }
    cTcpMutex.unlock();
}

/*!
\brief submit - sends a request to a server based on protocol (AISSVR_APP, _HTTP, _XML)
\param iResPath - resource path
        cTcpTasks[iXid].mAisOut = irAisOut;
\param iBody - body of post message if POST HTTP method otherwise empty string
\return ATcpTaskId - Unique incrementing integer assigned to this tcp task
 */
ATcpTaskId ATcpClient::submit(const QString& irResPath, const QString& irBody)
{
	ATcpTaskId aTcpTaskId = getNextTcpTaskId();
	cTcpMutex.lock();
    cTcpTasks[aTcpTaskId] = ATcpTask(aTcpTaskId);
	cTcpMutex.unlock();
    emit toSubmit(irResPath, irBody, &cTcpTasks[aTcpTaskId]);
	return aTcpTaskId;
}
/*!
\brief updateCookie - keep the previous cookie after an http read was successfully done.
*/
void ATcpClient::updateCookie(QByteArray iCookie)
{
    cCookie = iCookie;
}

/*	---------------------------------------------------------------------------------------------------------------------------
\brief requestToUrl - Convert lines of the form name|value|... into url-Encoded string.
\param irStg  - Contains iSep-delimited string of name-value pairs
\param iSep	  - Delimiting character
\returns aOut - Coverted string
	------------------------------------------------------------------------------------------------------------------------ */
QString ATcpClient::requestToUrl(QString& irStg, char iSep)
{
    QString aOut;
	QString aEntry;
	QStringList aLines(irStg.split(iSep, QString::KeepEmptyParts));
	QStringList::Iterator itp;
	for (itp = aLines.begin(); itp != aLines.end(); ++itp)
	{	QString& arName= *itp++;
		if (itp == aLines.end())
		{	aOut += arName + "&";
			break;
		}
		const QString& arValue = urlEncode(*itp, true);
		aOut += arName + "=" + arValue + "&";
	}
	if (aOut.length() > 0)
		aOut.truncate(aOut.length() - 1);
	return aOut;
}

/*	---------------------------------------------------------------------------------------------------------------------------
\brief urlEncode - URL-encode a string
\param irStg	 - String to be encoded
\param iUsePlus	 - If true convert space char into '+'; else, use %20
\returns aOut	 - Encoded string
	------------------------------------------------------------------------------------------------------------------------ */
QString ATcpClient::urlEncode(QString& irStg, bool iUsePlus)
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

//end
