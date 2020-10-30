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
aisdev/aismgr/awebpage.cpp
															Web Page

WebPage implements an HTTP client to allow the Lambdabase Server to fetch web pages.
 
CHANGE HISTORY
Version	Date		Who		Change
2.0004	4/13/2007	tlw		Emit debug output only if cTrace is set.
1.0113	11/7/2006	tlw		destructor. Review copy and assignment requirements.
1.0107	 9/20/2006	tlw		AWebPage inherits QHttp
1.0102	 9/1/2006	tlw		Improved diagnostic messages and added cTrace
1.0057	 3/18/2005	tlw		Add documentation
												--------------- ---------------

DOCUMENTATION
 1.	See htmlmgrnotes.txt for project setup information.
 2.	See webpage.h for class specifications.	
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include	<QtCore/QMap>
#include	<QtCore/QObject>
#include	<QtCore/QTimer>
#include	<QtCore/QUrl>

#include	"aisoutput.h"			// AISMGROUT_SIZE
#include	"asessionmgrinterface.h"	// ASessionMgrInterface
#include	"awebpage.h"		// AWebPage
#include	"aismgr.h"			// AAisMgr

//	--------------------------------------------------- PUBLIC METHODS --------------------------------------------------------

// AWebPage constructor  (args in same order as in class declaration)
AWebPage::AWebPage(AAisMgr* ipAisMgr, long iSessionId, long iMsecToWait)
	: QHttp(this), cpAisMgr(ipAisMgr), cGetId(-1), cReqAborted(false), cSessionId(iSessionId), cMsecToWait(iMsecToWait)
{
	// Debug. Set cTrace to true in order to generate debug output.
	cTrace = true;

	// Timer. Initialize a single-shot timer.
	cpTimer = new QTimer(this);
	cpTimer->setObjectName(QString("Http Single-shot timer"));
	cpTimer->setSingleShot(true);
	cpTimer->setInterval(cMsecToWait);
	QObject::connect(cpTimer, SIGNAL(timeout()), this, SLOT(cancelReadHtmlPage()), Qt::DirectConnection);

	// Connect. Connect network signals to webpage slots
	if (cTrace)
	{	QObject::connect(this, SIGNAL(requestStarted(int)), this, SLOT(onRequestStarted(int)), Qt::DirectConnection);
		QObject::connect(this, SIGNAL(stateChanged(int)), this, SLOT(onStateChanged(int)), Qt::DirectConnection);
		QObject::connect(this, SIGNAL(dataSendProgress(int, int)), this, SLOT(onDataSend(int, int)), Qt::DirectConnection);
		QObject::connect(this, SIGNAL(dataReadProgress(int, int)), this, SLOT(onDataRead(int, int)), Qt::DirectConnection);
		QObject::connect(this, SIGNAL(readyRead(const QHttpResponseHeader&)), this, SLOT(onReadyRead(const QHttpResponseHeader&)),
		Qt::DirectConnection);
		QObject::connect(this, SIGNAL(done(bool)), this, SLOT(onDone(bool)), Qt::DirectConnection);
	}
	QObject::connect(this,SIGNAL(responseHeaderReceived(const QHttpResponseHeader&)), this, 
	SLOT(onResponseHeaderReceived(const QHttpResponseHeader&)), Qt::DirectConnection);
	QObject::connect(this, SIGNAL(requestFinished(int, bool)), this, SLOT(onRequestFinished(int, bool)), Qt::DirectConnection);
}

/*!
\brief Destructor Free allocated resources

\par Notes:
-# AWebPage has remote ownership of a QTimer referent.
-# Since instances of AWebPage are never assigned copied, or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
AWebPage::~AWebPage()
{
	cpTimer->stop();		// Stop timer used to abort pending request if no response.
	delete cpTimer;

if (cTrace)
	qDebug("%s", "~~~~AWebPage()");
}

/*	---------------------------------------------------------------------------------------------------------------------------
cancelReadHtmlPage - Abort current read operation
Args:
	none
Returns:
	nothing
Notes:
 1.	Release current context from waiting for a page
	------------------------------------------------------------------------------------------------------------------------ */
void AWebPage::cancelReadHtmlPage()
{
	cReqAborted = true;
	cpTimer->stop();
	abort();
	QByteArray aPage(1, '\0');
	cpAisMgr->notifyHtmlPage(cSessionId, aPage);
}

/*	---------------------------------------------------------------------------------------------------------------------------
getPage - Launch request for a web page
Args:
	irUrl	URL of web page to be returned
Returns:
	nothing
Notes:
 1.	This request is aborted after cMsecToWait msecs.
	------------------------------------------------------------------------------------------------------------------------ */
void AWebPage::getPage(QString& irUrl)
{
	QUrl aUrl(irUrl);
	setHost(aUrl.host(), aUrl.port(80));
	cReqAborted = false;
	cpTimer->start();
	cGetId = get(aUrl.path()); // Make request, return immediately
}

/*	---------------------------------------------------------------------------------------------------------------------------
onDataSend - Note on send progress
Args:
	iDone	Bytes already sent
	iData	Total bytes to be sent
Returns:
	nothing
Notes:
 1. Only connected if cTrace is true.
	------------------------------------------------------------------------------------------------------------------------ */
void  AWebPage::onDataSend(int iDone, int iData)
{
	if (cTrace)
		qDebug("dataSend(Done:%d, Data:%d)", iDone, iData);
}

/*	---------------------------------------------------------------------------------------------------------------------------
onDone - Finished last request
Args:
	iError		True iff an error
Returns:
	nothing
Notes:
 1. Only connected if cTrace is true.
	------------------------------------------------------------------------------------------------------------------------ */
void AWebPage::onDone(bool iError)
{
	// Check. Check status.
	QByteArray aError(iError ? errorString().toLatin1() : "Ok");
	if (cTrace)
		qDebug("done(Error:%d), %s", iError, aError.data());
}

/*	---------------------------------------------------------------------------------------------------------------------------
onDataRead - 
Args:
	iDone	Bytes read
	iTotal  Total number of bytes expected
Returns:
	nothing
Notes:
 1. Only connected if cTrace is true.
	------------------------------------------------------------------------------------------------------------------------ */
void  AWebPage::onDataRead(int iDone, int iTotal)
{
	if (cTrace)
		qDebug("dataRead(Done:%d, Total:%d)", iDone, iTotal);
}

/*	---------------------------------------------------------------------------------------------------------------------------
onReadyRead - Note when data arrives
Args:
	iHdr	Response header received
Returns:
	nothing
Notes:
 1. Only connected if cTrace is true.
	------------------------------------------------------------------------------------------------------------------------ */
void  AWebPage::onReadyRead(const QHttpResponseHeader& iHdr)
{
    Q_UNUSED(iHdr);

	if (cTrace)
		qDebug("readyRead(Hdr:)");
}

/*	---------------------------------------------------------------------------------------------------------------------------
onRequestFinished - Page returned from web server
Args:
	orData		Place to put the incoming data
	ipNetOp		Network connection
Returns:
	nothing
Notes:
	------------------------------------------------------------------------------------------------------------------------ */
void AWebPage::onRequestFinished(int iId, bool iError)
{
	// Check. Check status.
	cpTimer->stop();
	QByteArray aErr(iError ? errorString().toLatin1() : "Ok");
	if (cTrace)
		qDebug("requestFinished(Id:%d, Error:%d), %s", iId, iError, aErr.data());

	if (!cReqAborted && iId == cGetId)
	{	// Read. Return response to waiting session.
		QByteArray aResponse;
		if (iError)
			aResponse = aErr;
		else if ((aResponse = readAll()).isEmpty())
			aResponse = "Page not found";
		else
			aResponse.replace('\r', "");
		cpAisMgr->notifyHtmlPage(cSessionId, aResponse);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
onRequestStarted - Request has been started
Args:
	iId	 Sequential number assigned to each Http request
Returns:
	nothing
Notes:
 1. Only connected if cTrace is true.
	------------------------------------------------------------------------------------------------------------------------ */
void  AWebPage::onRequestStarted(int iId)
{
	if (cTrace)
		qDebug("requestStarted(Id:%d)", iId);
}

void AWebPage::onResponseHeaderReceived(const QHttpResponseHeader& irHeader)
{
	cpTimer->stop();
	if (cTrace)
		qDebug("responseHeaderReceived(Hdr:)");

	// Fail.  Abort on a request failure. Else, wait for request finished signal
	if (irHeader.statusCode() != 200)
		cancelReadHtmlPage();
}

/*	---------------------------------------------------------------------------------------------------------------------------
onStateChanged - Next state entered by Http
Args:
	iState		QHttp::State (see below for list of values)
Returns:
	nothing
Notes:
 1. Only connected if cTrace is true.
	------------------------------------------------------------------------------------------------------------------------ */
void  AWebPage::onStateChanged(int iState)
{
	const char *apState;
	if (cTrace)
	{	switch (iState)
		{
		case 0:
			apState = "Unconnected";
			break;
		case 1:
			apState = "HostLookup";
			break;
		case 2:
			apState = "Connecting";
			break;
		case 3:
			apState = "Sending";
			break;
		case 4:
			apState = "Reading";
			break;
		case 5:
			apState = "Connected";
			break;
		case 6:
			apState = "Closing";
			break;
		default:
			apState = "Unknown";
			break;
		}
		qDebug("stateChanged(%s)", apState);
	}
}
// end
