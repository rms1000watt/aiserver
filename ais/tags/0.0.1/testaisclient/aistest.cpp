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

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/aaboutdialog.cpp
											About AIS Dialog

CHANGE HISTORY
Version	Date		Who		Change
2.0001	12/30/2006	tlw		onResponse, onResponseX0. Add ipData to the signatures
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0118	12/12/2006	tlw		onConnected, submit. Add serial binary stream argument to ASocket::submit.
1.0113	11/10/2006	tlw		submit. Allow standard IP address notation in host name.
1.0111	10/25/2006	tlw		Rename AAppClient methods
1.0105	 9/11/2006	tlw		Remove cpProtocols. Replace ATcpSocket with ASocket. Use appclient for APP protocol.
1.0070	10/27/2005	tlw		Add an about AIS dialog to AMainWindow
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QTextStream>
#include <QtCore/QTimer>
#include <QtGui/QMainWindow>
#include <QtGui/QMessageBox>
#include "appclient.h"
#include "asocket.h"
#include "aistest.h"
#include "autilities.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
\brief AAisTest - constructor initializes the GUI and reads the test suite

\param ipParent -> Parent that created this instance of AAisTest. Typically NULL.
\param irName - Name assigned to this instance of AAisTest
\param ipTestFileSpec -> Path + file name of the test suite.
\param iFlgs - Main window configuration flags
 */
AAisTest::AAisTest(QWidget* ipParent, const QString& irName, const char* ipTestFileSpec, Qt::WFlags iFlgs)
    : QMainWindow(ipParent, iFlgs )
{
	// Gui. Initialize widgets.
	cUi.setupUi(this);
	setObjectName(irName);
	setWindowTitle("Analytic Internet System Test Suite Version: " AGBL_AISVERSION);
	setWindowIcon(QIcon(":images/aistest.png"));
	cUi.upResultsTextEdit->append("Welcome to the AIS test suite!\n");

	// Properties. Initialize class properties.
	cpAppClient = NULL;
	cCurXid = 0;
	cCurIx = -1;
	cExpanded.truncate(0);
	cExpectedList.clear();
	cNextExpected = 0;
	cPass = false;
	cPending = 0;
	cRequest.clear();
	cResponse.clear();
	cRunAll = false;
	cShowResults = false;
	cTestFileSpec = ipTestFileSpec;
	cpTimer = new QTimer(this);
	QObject::connect(cpTimer, SIGNAL(timeout()), this, SLOT(onTimeout()));
	cpTimer->setInterval(cTimeout = AISTEST_TIMEOUT);		// [msec]
	cpTimer->setSingleShot(true);

	// Instructions.
	cUi.upInstructionsLabel->setText( tr( "Getting Started:\n"
	"	Edit tests in testsuite.txt\n"
	"	Start AIS and testAisClient\n"
	"	Select the NextTest button\n"
	"LastTest reruns the last test last request\n"
	"ShowResponse shows last response" ) );

    // Buttons. Hook up buttons.
    connect(cUi.upClearButton, SIGNAL(clicked()), cUi.upResultsTextEdit, SLOT(clear()));
    connect(cUi.upLastTestButton, SIGNAL(clicked()), this, SLOT(onLastTest()));
    connect(cUi.upNextTestButton, SIGNAL(clicked()), this, SLOT(onNextTest()));
    connect(cUi.upQuitButton, SIGNAL(clicked()), this, SLOT(close()));
    connect(cUi.upRestartButton, SIGNAL(clicked()), this, SLOT(onRestart()));
    connect(cUi.upShowResponseButton, SIGNAL(clicked()), this, SLOT(onShowResponse()));
    connect(cUi.upSubmitButton, SIGNAL(clicked()), this, SLOT(onSubmit()));

	// Clients. Connect up TCP socket signals.
	ASocketSt aSocketSt;			// Structure to hold connection state for every connection.
	aSocketSt.mPort = aSocketSt.mXid = 0;
	aSocketSt.mpSocket = NULL;		// APP protocol uses AppClient instead of ASocket.
	cSockets.append(aSocketSt);
	ASocket* apSocket = new ASocket(this, "HttpClient", AISSVR_HTTP);
	aSocketSt.mpSocket = apSocket;
	cSockets.append(aSocketSt);
	QObject::connect(apSocket, SIGNAL(connected()), this, SLOT(onConnected()), Qt::DirectConnection);
	QObject::connect(apSocket, SIGNAL(disconnected()), this, SLOT(onDisconnected()), Qt::DirectConnection);
	QObject::connect(apSocket, SIGNAL(response(QByteArray,QByteArray,char*)), this, SLOT(onResponse(QByteArray,QByteArray,char*))
	, Qt::DirectConnection);
	QObject::connect(apSocket,SIGNAL(socketError(long,QString)),this,SLOT(onSocketError(long,QString)),Qt::DirectConnection);
	apSocket = new ASocket(this, "HttpClient1", AISSVR_HTTP);
	aSocketSt.mpSocket = apSocket;
	cSockets.append(aSocketSt);
	QObject::connect(apSocket, SIGNAL(connected()), this, SLOT(onConnected()), Qt::DirectConnection);
	QObject::connect(apSocket, SIGNAL(disconnected()), this, SLOT(onDisconnected()), Qt::DirectConnection);
	QObject::connect(apSocket,SIGNAL(response(QByteArray,QByteArray,char*)),this,SLOT(onResponse(QByteArray,QByteArray,char*))
	,Qt::DirectConnection);
	QObject::connect(apSocket,SIGNAL(socketError(long,QString)),this,SLOT(onSocketError(long,QString)),Qt::DirectConnection);
	apSocket = new ASocket(this, "AppClient", AISSVR_XML);
	aSocketSt.mpSocket = apSocket;
	cSockets.append(aSocketSt);
	QObject::connect(apSocket, SIGNAL(connected()), this, SLOT(onConnected()), Qt::DirectConnection);
	QObject::connect(apSocket, SIGNAL(disconnected()), this, SLOT(onDisconnected()), Qt::DirectConnection);
	QObject::connect(apSocket, SIGNAL(response(QByteArray,QByteArray,char*)),this, SLOT(onResponseX0(QByteArray,QByteArray,char*)),
	Qt::DirectConnection);
	QObject::connect(apSocket,SIGNAL(socketError(long,QString)),this,SLOT(onSocketError(long,QString)),Qt::DirectConnection);

	// TestSuite. Initialize list of tests, variable definitions, and expected responses
	readFile(cTestFileSpec);
}

/*!
\brief Destructor Free allocated resources

\par Notes:
-# AAisTest has remote ownership of ASocket referents in ASocketSt (found in cSockets list), AAppClient, and QTimer.
-# Since instances of AAisTest are never assigned, copied or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
AAisTest::~AAisTest()
{
	// Delete the sockets in the current list.
	cCurIx = -1;
	cCurRequest.clear();
	long aSz = cSockets.size();
	for (long aIx = 0; aIx < aSz; ++aIx)
		delete cSockets.at(aIx).mpSocket;
	cSockets.clear();

	// Timer. Delete the timer
	if (cpTimer != NULL)
	{	cpTimer->stop();
		cpTimer->deleteLater();
	}
	// AAppClient. Delete the AppClient
	if (cpAppClient != NULL)
		cpAppClient->deleteLater();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AAisTest()");
#endif
}

/*	---------------------------------------------------------------------------------------------------------------------------
append - Append a message to the display
Args:
	irMsg		Message to be displayed
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::append(const QString& irMsg)
{
	QString aMsg;
	if (irMsg.length() > 256)
		aMsg = irMsg.left(256) + "...\n";
	else
		aMsg = irMsg + '\n';
	cUi.upResultsTextEdit->insertPlainText(aMsg);
	cUi.upResultsTextEdit->ensureCursorVisible();
}

/*	---------------------------------------------------------------------------------------------------------------------------
buildHttpRequest - Construct request header from TestSuite request
Args:
	irPath		Of the form /default.html
	irHost		The DNS name or IP address of the server of the form localhost
Returns:
	nothing
Notes:
 1.	Puts request header + body in cCurRequest
 2. If query string is XML, it must start with xml= or Content-Type must be text/xml (or both).
 3. Query is (1)url-encoded name-value pairs, (2)XML doc, (3)url-encoded XML doc.

 Example:
GET /default.htm HTTP/1.1
Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, * / *
Accept-Language: en
Connection: Keep-Alive
User-Agent: Analytic Information Server (AIS Client)
Accept-Charset: iso-8859-1,*,utf-8
Host: localhost
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::buildHttpRequest(const QString& irPath, const QString& irHost)
{
	// Get current structure for this connection.
	ASocketSt& arSocketSt = cSockets[cCurIx];
	cCurXid = ++arSocketSt.mXid;

	// Request Header.
	cCurRequest = "GET ";
	cCurRequest += irPath.isEmpty() ? "/" : irPath;
	cCurRequest += " HTTP/1.1\r\n"
			"Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, */*\r\n"
			"Accept-Language: en\r\n" 
			"Connection: Keep-Alive\r\n"
			"User-Agent: Analytic Information Server (AIS Client)\r\n"
			"Accept-Charset: iso-8859-1,*,utf-8\r\n";
	if (!irHost.isEmpty())
		cCurRequest += "Host: " + irHost + "\r\n" ;
	if (!arSocketSt.mCookie.isEmpty())
		cCurRequest += "Cookie: " + arSocketSt.mCookie + "\r\n";
	cCurRequest += "\r\n";
}

/*	---------------------------------------------------------------------------------------------------------------------------
buildPostRequest - Construct request header + body from TestSuite request
Args:
	irPath		Of the form /amp.dll
	irBody		Body of the POST request
Returns:
	nothing

Notes:
 1.	Puts request header + body in cCurRequest
 2. If body is XML, it must start with xml= or Content-Type must be text/xml (or both).
 3. Body is (1)url-encoded name-value pairs, (2)XML doc, (3)url-encoded XML doc.
 4. Content-Type is set to text/xml iff body starts with less-than (<).

 Example:
POST /amp.dll HTTP/1.1
Accept: image/gif, application/vnd.ms-powerpoint, ...
Accept-Language: en-us
Content-Type: multipart/form-data; boundary=---------------------------7d42af2730556
User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)
Host: localhost
Content-Length: 349
Connection: Keep-Alive
Cache-Control: no-cache

body...
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::buildPostRequest(const QString& irPath, const QString& irHost, const QString irBody)
{
	// Get current structure for this connection.
	ASocketSt& arSocketSt = cSockets[cCurIx];
	cCurXid = ++arSocketSt.mXid;

	// Request Header.
	if (!irBody.isEmpty())
	{	cCurRequest = "POST ";
		cCurRequest += irPath.isEmpty() ? "/amp.dll" : irPath;
		cCurRequest += " HTTP/1.1\r\n"
				"Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, image/png, */*\r\n"
				"Accept-Language: en\r\n"
				"Accept-Charset: iso-8859-1,*,utf-8\r\n";
				"Connection: Keep-Alive\r\n"
				"Host: " + irHost + "\r\n"
				"User-Agent: Analytic Information Server (AIS Client)\r\n";
		if (!arSocketSt.mCookie.isEmpty())
			cCurRequest += "Cookie: " + arSocketSt.mCookie + "\r\n";
		cCurRequest += (irBody[0] == '<') ? "Content-Type: text/xml\r\n" : "Content-Type: text/plain\r\n";
		cCurRequest += "Content-Length: " + QString::number(irBody.length()) + "\r\n\r\n";
		cCurRequest += irBody;
	}
	else
		append("buildPostRequest: No body found for request " + irPath);
}

/*	---------------------------------------------------------------------------------------------------------------------------
buildXmlRequest - Converts TestSuite request into XML
Args:
	irRequest	DEL-delimited name-value pairs target|act|request...
Returns:
	nothing
Notes:
 1.	Returns XML in cCurRequest
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::buildXmlRequest(const QString& irRequest)
{
	// Convert. Convert separators from vertical bar to DEL
	QString aRequest(irRequest);
	aRequest.replace('|', '\177');

	// Socket Structure. Get Socket Structure.
	ASocketSt& arSocketSt = cSockets[cCurIx];

	// Attributes. Set root element attributes.
	AStringMap aAttribs;
	aAttribs["act"] = aRequest.section('\177', 1, 1);
	aAttribs["target"] = aRequest.section('\177', 0, 0);
	aAttribs["xtype"] = "msg";
	aAttribs["xid"] = QString::number(cCurXid = ++arSocketSt.mXid);
	AUtil::ampmsgToXml(aRequest.section('\177', 2), aAttribs, cCurRequest);
}

/*	---------------------------------------------------------------------------------------------------------------------------
expand - Set variable definitions and expand variables in current cExpected
The regExp in the var definition is applied to the current response to determine the var value.  cExpanded is set to the
current cExpected with terms of the form $varName$ replaced by their value.  It is possible to extract a variable value from a
response and substitute the value of this variable to the cExpected applied to the same response.
Args:
	none
Returns:
	nothing
Note:
 1.	cNextExpected points to the current cExpectedList or to count() if at end.  It is advanced to the next element in the list.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::expand()
{
	// Expand current cExpected.
	long aBeg, aLgth;
	long aExpectedSz = cExpectedList.count();
	QRegExp aRx;
	QString aPattern, aValue, aVar;
	cExpanded.truncate(0);
	for (; cNextExpected < aExpectedSz; ++cNextExpected)
	{	// Set variable definitions given lines of the form &varName=RegExp
		QString &arNext = cExpectedList[cNextExpected];
		if (arNext.indexOf(QRegExp("^\\&\\w+\\s*=")) >= 0)
		{	// Extract varName and RegExp from the next entry in cExpectedList.
			aRx.setPattern("\\&(\\w+)\\s*=\\s*(.+)$");
			if (aRx.indexIn(arNext) >= 0)
			{	aVar = aRx.cap(1);
				aPattern = aRx.cap(2);
				if (!aPattern.isEmpty())
				{	// Apply aRegExp to current response. If a match, put value in aVar.
					aRx.setPattern(aPattern);
					if (aRx.indexIn(cResponse) >= 0 && !(aValue = aRx.cap(1)).isEmpty())
						cVariables[aVar] = aValue;
					else
					{	aVar = QString("Expand: %1 not found in %2").arg(aPattern, cResponse);
						append(aVar);
					}
				}
			}
			else
			{	aVar = "Expand: Invalid variable definition " + arNext;
				append(aVar);
			}
		}
		else	// Substitute variable value for $var$ entries in current cExpected
		{	aRx.setPattern("\\$(\\w+)\\$");
			cExpanded = arNext;
			while ((aBeg = aRx.indexIn(cExpanded)) >= 0)
			{	aLgth = aRx.matchedLength();
				if (cVariables.contains(aVar = aRx.cap(1)))
					cExpanded.replace(aBeg, aLgth, cVariables[aVar]);
				else
				{	aVar = QString("Expand: The var %1 in expected response %2 is not defined.").arg(aVar,cExpanded);
					append(aVar);
					break;
				}
			}
			++cNextExpected;
			break;
		}
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
nextTest -  Launch the next test or repeat last test.
Args:
	iRedoLast	If true, just repeat the last test
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::nextTest(bool iRedoLast)
{
	// If last test not completed and if user wishes to abort, print message and proceed.
	bool aAbort = false, aFail = false, aDoNext = true;	// Default to no fail and proceed with next test.
	if (cPending > 0)
	{	append("~~~~~~~~~~~~~~~~~~~~~~~~~~ ABORT: Cancel current test ~~~~~~~~~~~~~~~~~~~~~~~~~~");
		aAbort = true;
	}
	// Move on to the next test in cTests. Set cPending to expected number of responses.
	if (iRedoLast)
	{	if (cRequest.isEmpty())
		{	append("Nothing to repeat. Please run a test first.");
			aDoNext = false;		// No test to repeat.
		}
		else	// Restart from the top
			cExpanded.truncate(0);
	}
	// At end. Continue if repeat tests enabled or else stop.
	else if (cTests.isEmpty())
	{	cpTimer->stop();			// Ignore the last timeout
		if (cUi.upRepeatCheckBox->isChecked())
		{	append("----------------------------------- Restarting ----------------------------------");
			cNumExpected = cPending = 0;
			readFile(cTestFileSpec);
			cRunAll = cUi.upRunAllCheckBox->isChecked();
			nextTest(false/*RedoLast*/);
		}
		else
			append("At end. Press Restart to repeat tests.");
		aDoNext = false;
	}
	else
	{	// Get next request from cTests. Expand variables to their values.
		long aBeg, aLgth;
		QChar aFirst;		// First char in expected
		QString aVar;
		QRegExp aRx("\\$(\\w+)\\$");
		cRequest = cTests.front().mid(1);
		cTests.pop_front();
		
		while ((aBeg = aRx.indexIn(cRequest)) >= 0)
		{	aLgth = aRx.matchedLength();
			if (cVariables.contains(aVar = aRx.cap(1)))
				cRequest.replace(aBeg, aLgth, cVariables[aVar]);
			else
			{	aVar = QString("NextTest: The var %1 in request %2 is not defined. Revise test and try again.").arg(aVar, cRequest);
				append(aVar);
				aDoNext = false;
				aFail = true;
				break;
			}
		}
		// Get expected responses for this request from cTests.
		cExpectedList.clear();
		cExpanded.clear();
		cNumExpected = 0;			// Count the number of expected responses
		while (!cTests.isEmpty())
		{	QString& arNext = cTests.front();
			if ((aFirst = arNext[0]) == '#')
				break;
			if (aFirst != '&')
				++cNumExpected;
			cExpectedList += arNext;
			cTests.pop_front();
		}
		if (cNumExpected <= 0)
		{	append("No expected responses found for this test. Revise responses and try again.");
			aDoNext = false;
			aFail = true;
		}
	}
	// Display next test. Set cPass true. Launch next test
	if (aDoNext)
	{	bool aPause = false, aTimeout = false;
		cNextExpected = 0;
		cPending = cNumExpected;			// Resynch expected-response count
		cUi.upTestLineEdit->setText(cRequest);
		append(cRequest);

		// Handle built-in requests
		cPass = true;
		if ((aPause=cRequest.startsWith("pause"))||(aTimeout=cRequest.startsWith("timeout"))||cRequest.startsWith("connect"))
		{	cPending = 0;
			if (cNextExpected < cExpectedList.count())
				cExpanded = cExpectedList[cNextExpected];
			cUi.upExpectedResultLineEdit->setText(cExpanded);
			if (aPause)
				append("~~~~~~~~~~~~~~~~~~~~~~~ Paused: Press NextTest to continue ~~~~~~~~~~~~~~~~~~~~~~");
			else if (aTimeout)
			{	QRegExp aRx("timeout\\s*=\\s*(\\d+)");
				if (aRx.indexIn(cRequest) >= 0)
					cpTimer->setInterval(cTimeout = aRx.cap(1).toInt());
				append("----------------------------- Timeout period updated ----------------------------");
				if (cRunAll)
					nextTest(false/*RedoLast*/);
			}
		}
		else
		{	// Dispatch. Submit to selected client.
			submit(cRequest, aAbort);

			// Timer. Set timer to recover in case response is not forthcoming from AIS.
			if (cTimeout > 0)
				cpTimer->start();	// single-shot timer
		}
	}
	else if (aFail)
		append("************************ FAIL: Press NextTest to continue ***********************");
}

/*	---------------------------------------------------------------------------------------------------------------------------
onConnected - Catches ASocket signal from openConnection request.

After getting connected, submit the pending request.
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::onConnected()
{
	if (cCurIx >= 0 && !cCurRequest.isEmpty())
	{	ASocketSt& arSocketSt = cSockets[cCurIx];
		arSocketSt.mCookie.clear();
		append("Connected to " + arSocketSt.mHost);
		arSocketSt.mpSocket->submit(cCurRequest, NULL/*Data*/, 0/*DataLgth*/);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
onDisconnected - Catches ASocket signal from openConnection request.

After disconnecting from the current server, connect up to the new server.
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::onDisconnected()
{
	if (cCurIx >= 0 && !cCurRequest.isEmpty())
	{	ASocketSt& arSocketSt = cSockets[cCurIx];
		arSocketSt.mCookie.clear();
		append("Disconnected from host");
		arSocketSt.mpSocket->openConnection(arSocketSt.mHost, arSocketSt.mPort);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
onLastTest - Catches  signal from LastTestButton

Reruns the last test over.
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::onLastTest()
{
	cRunAll = false;		// Turn off continuous testing.
	cUi.upRunAllCheckBox->setChecked(false);
	nextTest(true/*RedoLast*/);
}

/*	---------------------------------------------------------------------------------------------------------------------------
onNextTest - Catches  signal from NextTestButton

Launches the next test.  If the RunAll check box is checked, all the remaining tests are run.
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::onNextTest()
{
	cRunAll = cUi.upRunAllCheckBox->isChecked();
	nextTest(false/*RedoLast*/);
}

/*	---------------------------------------------------------------------------------------------------------------------------
onResponse - Gets response back from connection using HTTP.
Args:
	iBody		Body of response returned by HTTP.
	iCookie		Cookie value extracted from Set-Cookie: abaseid=GMJBLCEBEBOAHNHMKIGLPIJL
	ipData		Not used.
Returns:
	nothing
Note:
 1.	One and only one response is returned from each request using HTTP.
 2. ASocket extracts the cookie and the length of the body from the header but does not return the header.
 3. If the server sets a cookie, buildHttpRequest will return this cookie value with each subsequent request.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::onResponse(QByteArray iBody, QByteArray iCookie, char* ipData)
{
    Q_UNUSED(ipData);

	// Cookie. Save cookie value, if any, in socket structure
	if (cCurIx >= 0 && !iCookie.isEmpty())
	{	ASocketSt& arSocketSt = cSockets[cCurIx];
		arSocketSt.mCookie = iCookie;
	}
	// Response. Return the response
	response(QString(iBody.replace('\177', '|')));
}

/*	---------------------------------------------------------------------------------------------------------------------------
onResponseX0 - Gets XML document returned from server.
Args:
	iXml	</amp  act="MyAct" status="0" target="MyTarget" context="MyContext" mode="async"><Arg>...</Arg></amp>
	iCookie	Cookie value (not used with this protocol)
	ipData	Not used.
Returns:
	nothing
Note:
 1.	aAmpmsg format: MyTarget|MyAct|Arg0|Value0|....
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::onResponseX0(QByteArray iXml, QByteArray iCookie, char* ipData)
{
    Q_UNUSED(iCookie);
    Q_UNUSED(ipData);

	// XML Document. Convert XML Doc into an AMP msg. Return AMP msg.
	QString aAct, aAmpmsg, aContext, aMode, aTarget;
	AUtil::xmlToAmpmsg(iXml, aAmpmsg, aAct, aContext, aMode,  aTarget);
	if (aAct.toLower() == "pushed")
		append("Pushed:" + aAmpmsg);
	else
	{	aAmpmsg = aAmpmsg.left(256).replace('\n', "\\n").replace('\177', '|').remove('\r');
		response(aAmpmsg);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
onRestart - Restart tests from the beginning.
Args:
	none
Returns:
	nothing
Note:
 1.	Clears the HttpClient's cookies.
 2.	Re-reads test suite in case that it changed.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::onRestart()
{
	// Restart tests from the beginning
	cNumExpected = cPending = 0;
	long aClientSz = cSockets.count();
	for (long aClient = 0; aClient < aClientSz; ++aClient)
	{	ASocketSt& arSocketSt = cSockets[aClient];
		arSocketSt.mCookie.clear();
		arSocketSt.mHost.clear();
		arSocketSt.mPort = 0;
		arSocketSt.mXid = 0;
		if (arSocketSt.mpSocket != NULL)
			arSocketSt.mpSocket->closeConnection();
	}
	cUi.upResultsTextEdit->clear();
	cTests.clear();
	readFile(cTestFileSpec);
	append("Reset tests to beginning. Press NextTest to continue..");
}

/*	---------------------------------------------------------------------------------------------------------------------------
onSocketError - Process error returned from the TCP connection
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::onSocketError(long iStatus, QString iError)
{
	QString aMsg(gAis.mpErrMsgs[iStatus]);
	response(aMsg + " Rcvd:" + iError);
}

/*	---------------------------------------------------------------------------------------------------------------------------
onShowResponse - Process showResponse request
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::onShowResponse()
{
	append(cResponse);
}

/*	---------------------------------------------------------------------------------------------------------------------------
onSubmit - Submit hand-crafted test
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::onSubmit()
{
	// Submit test from submitEditLine
	cRequest =  cUi.upTestLineEdit->text();
	cExpectedList.clear();
	cExpectedList += cUi.upExpectedResultLineEdit->text();
	cNumExpected = 1;
	cRunAll = false;		// Turn off continuous testing.
	cUi.upRunAllCheckBox->setChecked(false);
	nextTest(true/*RedoLast*/);
}

/*	---------------------------------------------------------------------------------------------------------------------------
onTimeout - timeout called iff a response is not received.
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::onTimeout()
{
	// Show timeout results
	append("~~~~~~~~~~~~~~~~~~~~~~~ Timeout: Press NextTest to continue ~~~~~~~~~~~~~~~~~~~~~");
	cResponse.clear();
}

/*	---------------------------------------------------------------------------------------------------------------------------
readFile - Transfer lines from test file to the string list cTests
Args:
	irTestFileName	Path and name of test file
Returns:
	nothing
Note:
 1.	A test consists of one request line followed by one or more expected response lines.
 2.	Each test must be separated by at least one comment line.
 3.	A comment consists of a blank line or line starting with #.
 4.	If #include, include the named file in the list of tests.
 5.	If #stop, ignore the remaining lines in the file.
 6. If #&VarName=value, record defined constant.
 7.	All other comments are ignored.
 8.	One or more variable definitions may precede each expected response.  A variable definition
	associates a VarName with a regular expression.  The variable's value is determined by
	applying the regular expression to the current response.
		&VarName=RegExp
 9.	When a response is received, this value is substituted for instances of $VarName$ in the
	request or in the current or subsequent expected responses.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::readFile(const QString& irFileSpec)
{
	// Open test suite file which contains the tests and the expected results
	long aCount = 0, aNLines = 0, aPos = 0;	// Number of non-comment lines since last comment.
	QString aHead, aLine, aMsg, aPattern, aTail;
	QFile aFile(irFileSpec);
	QFileInfo aFileInfo(aFile);
	if (!aFile.open(QIODevice::Text | QIODevice::ReadOnly))	// 16|1
	{	// Can't open test file.
		aMsg = QString("ATestSuite.readFile(): Can't open test file: %1!").arg(aFileInfo.absoluteFilePath());		
		append(aMsg);
	}
	// Read the file
	QTextStream aTs(&aFile);
	while (!aTs.atEnd())
	{	// Get next line.
		aLine = aTs.readLine();		// readLine excludes the newline terminator

		// Process comments. If #include, read named file. If #stop, quit reading lines.
		if (aLine.indexOf(QRegExp(QString("^\\s*(#|$)")), 0/*From*/) >= 0)
		{	//	Expect one test followed by at least one response
			if (aNLines > 0 && aNLines < 2)
			{	aMsg = QString("readFile: Missing response in %1 before the comment:\n%2").arg(aFileInfo.absoluteFilePath(), aLine.left(128));
				append(aMsg);
			}
			aNLines = 0;
			if (aLine.startsWith("#include"))
			{	QRegExp aRegExp("#include\\s+(.*)#*", Qt::CaseInsensitive);
				if (aRegExp.indexIn(aLine) < 0)
				{	aMsg = "ATestSuite.readFile(), Can't include: " + aLine;
					append(aMsg);
				}
				else	// Recurse
					readFile(aRegExp.cap(1));				
			}
			else if (aLine.startsWith("#stop"))
				break;
			else if (aLine.startsWith("#&") && (aPos = aLine.indexOf('=')) > 0)
			{	// Extract global variable definition from name=value
				aHead = aLine.mid(2, aPos-2);
				cVariables[aHead] = aLine.mid(aPos+1);
			}
		}
		// Save state variable definitions in cTests
		else if (aLine.indexOf(QRegExp("^\\&\\w+\\s*=")) >= 0)
			cTests.append(aLine);
		else	// Expand requests and expected responses
		{	if (++aNLines <= 1)
			{	// If a request contains [= or |]{n}stuff, replace stuff with n copies of stuff
				if ((aPos = aLine.indexOf(QRegExp("[=|]\\{"))) >= 0)
				{	aHead = aLine.left(++aPos);
					aLine = aLine.mid(aPos);
					if ((aPos = aLine.indexOf('|')) >= 0)
					{	aTail = aLine.mid(aPos);
						aLine = aLine.left(aPos);
					}
					QRegExp aRx("\\{(\\d+)\\}(.+)");
					if (aRx.indexIn(aLine) >= 0)
					{	aCount = aRx.cap(1).toInt();
						aPattern = aRx.cap(2);
						aLine = aPattern;
						for (aPos = 1; aPos < aCount; ++aPos)
							aLine += aPattern;
						aPattern.sprintf("-%d", aLine.length());
						aLine += aPattern;
					}
					aLine = aHead + aLine + aTail;
				}
				aLine.prepend("#");			// Mark the request line
			}
			// else, add expected responses to tests
			cTests.append(aLine);
		}
	}
	aFile.close();
}

/*	---------------------------------------------------------------------------------------------------------------------------
response - Process returned response from server.
Args:
	irResponse		Standardized response from server or connection message.
Returns:
	nothing
Note:
 1.	One or more responses may be returned from a single request.
 2. Compares response with cExpanded.  Set cPass to false if no match.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::response(const QString irResponse)
{
	// Clear. Clear the current values that are no longer needed
	cCurIx = -1;
	cCurRequest.clear();

	if (cPending == 0)	
	{	append(irResponse);
		append("***************************** Unexpected Response *******************************");
	}
	else if (cPending > 0)
	{	// Stop timer.
		cpTimer->stop();

		// Set the cResponse.  Set cExpanded by expanding current cExpected
		cResponse = irResponse;
		expand();
		cUi.upExpectedResultLineEdit->setText(cExpanded);
		if (cShowResults)
			append(cResponse);

		// Compare response with expected response
		if (cExpanded.isEmpty() || cResponse.indexOf(cExpanded) < 0)
		{	QString aMsg;
			aMsg = QString("Expected:%1:\nReceived:%2:").arg(cExpanded, cResponse.left(128));
			append(aMsg);
			cPass = false;
			append("************************ FAIL: Press NextTest to continue ***********************");
		}
		else
			append("------------------------------------- Pass --------------------------------------");

		if (--cPending > 0)
		{	// Restart the timer. Wait for next response or timeout.
			if (cTimeout > 0)
				cpTimer->start();
		}
		else if (cPass && cRunAll)// Ok to proceed to next test
			nextTest(false/*RedoLast*/);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
returnOutput - Implemented for pure abstract base class AReturnRcvr

returnOutput catches returned messages from appclient which is used just for A0 requests.
Args:
	iDummyId	Placeholder to fit returnOutput signature.
	iXid		Request ID (incrementing integer) established by submit
	iStatus		Error code >0 if an error
	iReqType	Request type (eval, ampMsg, or built-in command)
	iRetValue	Integer returned from AIS
	irOut		Returned message
	ipData		Binary buffer for object closures (not used here)
	iDataSize	Binary buffer size (not used here)
	irDisplay	Console output
	irError		Error details
	iClientData	Anonymous data submitted with the request
Returns:
	nothing
Notes:
 1. Repackages the returned result into: ReqType|Status|RetValue|Display|Error|Out
 2. Compare this with onResponseA0 which is the alternative way to catch returns from ASocket.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::returnOutput(long iConnectId, long iXid, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut,
    char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData)
{
    Q_UNUSED(iConnectId);
    Q_UNUSED(ipData);
    Q_UNUSED(iDataSize);
    Q_UNUSED(iClientData);

	QString aReqType, aRet;
	QString aOut(irOut);
	aOut.replace('\177', '|').replace('\n', "\\n");
	aRet = QString("%1|%2|%3|%4|%5|%6").arg(REQNAME(iReqType)).arg(iStatus).arg(iRetValue).arg(irDisplay, irError, aOut);
	aRet.replace('\n', "\\n").remove('\r');
	if (iXid == 0)
		append("Pushed:" + aRet);
	else if (iStatus == 0 && iXid != cCurXid)		// If an error, return the error.
		append(QString("XId mismatch. Expected:%1,Rcvd:%2,Msg:%3.").arg(cCurXid).arg(iXid).arg(aRet.left(128)));
	else // Response. Return a response of some sort.
	{	if (cCurIx >= 0 && iReqType == geCloseConnection)
		{	ASocketSt& arSocketSt = cSockets[cCurIx];
			append("Disconnected from host");
			if (arSocketSt.mPort != 0 && !arSocketSt.mHost.isEmpty())
			{	cpAppClient->setHost(arSocketSt.mHost, arSocketSt.mPort);
				cCurXid = cpAppClient->onOpenConnection(this);
			}
		}
		else if (iReqType == geOpenConnection)
		{	ASocketSt& arSocketSt = cSockets[cCurIx];
			append("Connected to " + arSocketSt.mHost);
			if (!cCurRequest.isEmpty())
				submitAppRequest(cCurRequest);
		}
		else
			response(aRet);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
submit -  Send the next request to the server via a TCP connection
Args:
	irRequest	Next test from the list.
	iNumSubmit	Number of submissions to be made to server
	iAbort		If true, submit a new request even if still responding to previous request.
Returns:
	void
Notes:
  1. Tests are of the form:
  		A0://localhost:8081/_ais|logon|user|guest|passwd|
		H0://localhost:80/default.htm
		H1://localhost:80/index.htm
		P1://localhost:81/amp.dll body
		X0://localhost:8080/_ais|probe
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::submit(const QString& irRequest, bool iAbort)
{
	/* Stop submission if last test is still pending....
	if (cPending)
		append("Pending request: " + cRequest);
	*/
	if (cPending || iAbort)
	{	// Reset. Reset current test state
		cExpanded.clear();
		cResponse.clear();
		
		// Protocol, Host, Port, Path, Body. Extract elements of the command
		bool aOk;
		long aProtocolType, aPos;
		ushort aNPort;
		QString aBody, aHost, aPath, aPort, aProtocol;
		QRegExp aRegExp("([AHPX]\\d)://([.\\w]+):(\\d+)/(.+)", Qt::CaseSensitive);
		if (aRegExp.indexIn(irRequest) >= 0)
		{	aProtocol = aRegExp.cap(1);
			aHost = aRegExp.cap(2);
			aPort = aRegExp.cap(3);
			aPath = aRegExp.cap(4);
			aNPort = aPort.toUShort(&aOk);
			if (!aOk || aNPort <= 0)
			{	aNPort = 80;
				append ("submit, Unable to determine port from " + irRequest);
			}
			// Request. Format request and put result in cCurRequest.
			aProtocolType = aProtocol.at(0).toAscii();
			switch (aProtocolType)
			{
			case 'A':	// A0 requests
			{	// Socket. Get reference to the current state for this server connection.
				cCurIx = AISTEST_APPSOCKET;
				ASocketSt& arSocketSt = cSockets[cCurIx];
				if (cpAppClient == NULL)
					cpAppClient = new AAppClient(aHost.toLatin1(), aNPort, this, this, "AisTestClient");

				if (!cpAppClient->isConnected())
				{	arSocketSt.mHost = aHost;
					arSocketSt.mPort = aNPort;
					cCurRequest = aPath;
					cCurXid = cpAppClient->onOpenConnection(this);
				}
				else if (aNPort != arSocketSt.mPort || aHost != arSocketSt.mHost)
				{	arSocketSt.mHost = aHost;
					arSocketSt.mPort = aNPort;
					cCurRequest = aPath;
					// ConnectId. If ConnectId is 0, the current connection is closed.
					cCurXid = cpAppClient->closeConnection(this, 0/*ConnectId*/, geHard);
				}
				else
					submitAppRequest(aPath);
				break;
			}
			case 'H':	// HO or H1 requests
				cCurIx = aProtocol.at(1) == '0' ? AISTEST_HTTPSOCKET : AISTEST_HTTPSOCKET1;
				aPath.prepend('/');
				buildHttpRequest(aPath, aHost);
				break;
			case 'P':	// P0 or P1 requests
				cCurIx = aProtocol.at(1) == '0' ? AISTEST_HTTPSOCKET : AISTEST_HTTPSOCKET1;
				aPath.prepend('/');
				if ((aPos = aPath.indexOf('\t')) > 0)
				{	aBody = aPath.mid(aPos + 1);
					aPath = aPath.left(aPos);
					buildPostRequest(aPath, aHost, aBody);
				}
				else
					append("submit: No tab delimiter found in " + irRequest);
				break;
			case 'X':	// X0 requests
				cCurIx = AISTEST_XMLSOCKET;
				buildXmlRequest(aPath);
				break;
			default:
				cCurIx = -1;
				append ("submit, Unknown protocol " + irRequest);
				break;
			}
			if (cCurIx > AISTEST_APPSOCKET)
			{	// Socket. Get reference to the current state for this server connection.
				ASocketSt& arSocketSt = cSockets[cCurIx]; 
				ASocket* apSocket = arSocketSt.mpSocket;

				// Connect. Establish a connection to the specified server.
				if (!apSocket->isConnected())
				{	arSocketSt.mHost = aHost;
					arSocketSt.mPort = aNPort;
					arSocketSt.mpSocket->openConnection(aHost, aNPort);
				}
				// Disconnect. If a different server, close the current connection.
				else if (aNPort != arSocketSt.mPort || aHost != arSocketSt.mHost)
				{	arSocketSt.mHost = aHost;
					arSocketSt.mPort = aNPort;
					arSocketSt.mpSocket->closeConnection();
				}
				else	//  Submit. Send this request to the server.
					apSocket->submit(cCurRequest, NULL/*Data*/, 0/*DataLgth*/);
			}
		}
		else
			append("submit, Unable to parse " + irRequest);
	}
}

/*	---------------------------------------------------------------------------------------------------------------------------
submitAppRequest - Submits next test to AAppClient

Unwrap the body of the request and route request to the appropriate AppClient method.
Args:
	irBody	Body of the request
Returns:
	nothing
Notes:
 1.	Request is of the form: A0://localhost:8081/_ais|logon|user|guest|passwd|
 2. Body of the request is everything past the last slash
 3. The result is returned via returnOutput callback function.
	------------------------------------------------------------------------------------------------------------------------ */
void AAisTest::submitAppRequest(const QString& irBody)
{
	QStringList aArgs = irBody.split('|', QString::KeepEmptyParts);
	long aCount = aArgs.count();
	QString aArg, aBody(irBody), aClientData;
	if (aCount <= 0)
		append("submitAppRequest: Too few args in " + aBody);
	else if (aArgs[0] != "_ais")
		append("submitAppRequest: Unrecognized target in " + aBody);
	else if (aCount == 1)			// Empty request
	{	QString aClientData;
		cCurXid = cpAppClient->submit(this, aBody, aClientData, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/);
	}
	else if (aArgs[1] == "setcurrentextent")	// _ais|setcurrentextent|extent|%s
	{	if (aCount < 4 || aArgs[2] != "extent")
			append("submitAppRequest: setCurrentExtent. Unrecognized argument name in " + aBody);
		else if (cpAppClient->setCurrentExtent(aArgs[3]/*Extent*/))
			response("setcurrentextent|" + aArgs[3]);
		else
			append("submitAppRequest: setCurrentExtent. Extent not found for current context. " + aBody);
	}
	else
	{	AReqType aReqType = REQTYPE(aArgs[1]);
	
		switch (aReqType)
		{
		case geCloseCabinet:			// _ais|closecabinet|cabname|%s
			if (aCount < 4 || aArgs[2] != "cabname")
				append("submitAppRequest: CloseCabinet. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->closeCabinet(this, aArgs[3]/*CabName*/);
			break;
		case geCloseConnection:			// _ais|closeconnection
			cCurXid = cpAppClient->closeConnection(this, 0/*ConnectId*/, geDefault);
			break;
		case geCloseContext:			// _ais|closecontext|context|%s|mode|%d
			if (aCount < 6 || aArgs[2] != "context" || aArgs[4] != "mode")
				append("submitAppRequest: closeContext. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->onCloseContext(this, aArgs[3]/*ContextName*/, (ACloseMode)aArgs[5].toInt()/*Mode*/);
			break;
		case geCloseSession:			// _ais|closesession|sessionid|%s|mode|%d
			if (aCount < 6 || aArgs[2] != "sessionid" || aArgs[4] != "mode")
				append("submitAppRequest: closeSession. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->onCloseSession(this, aArgs[3].toLong()/*SessionId*/, (ACloseMode)aArgs[5].toInt()/*Mode*/);
			break;
		case geCompileLambda:			// _ais|compilelambda|extent_Lambdas|%s
			if (aCount < 4 || aArgs[2] != "extent_Lambdas")
				append("submitAppRequest: compileLambda. Unrecognized argument name in " + aBody);
			else
			{	aArgs = aArgs[3].split('\t', QString::SkipEmptyParts);
				cCurXid = cpAppClient->compileLambda(this, aArgs/*ExtentLambdaList*/);
			}
			break;
		case geCompileCabinet:			// _ais|compilecabinet|extents|%s
			if (aCount < 4 || aArgs[2] != "extents")
				append("submitAppRequest: compileCabinet. Unrecognized argument name in " + aBody);
			else
			{	aArgs = aArgs[3].split('\t', QString::SkipEmptyParts);
				cCurXid = cpAppClient->compileCabinet(this, aArgs/*ExtentNames*/);
			}
			break;
		case geEnableConsoleLog:		// _ais|enableconsolelog|enable|%s
			if (aCount < 4 || aArgs[2] != "enable")
				append("submitAppRequest: enableConsoleLog. Unrecognized argument name in " + aBody);
			else
			{	aArg = aArgs[3];		// if format then 2
				long aEnable = aArg=="reset" ? -2 : aArg=="close" ? -1 : aArg=="suspend" ? 0 : aArg=="enable" ? 1 : 2;
				cCurXid = cpAppClient->enableConsoleLog(this, aEnable);
			}
			break;
		case geEraseNode:				// _ais|erasenode|nodes|%s
			if (aCount < 4 || aArgs[2] != "nodes")
				append("submitAppRequest: eraseNode. Unrecognized argument name in " + aBody);
			else
			{	aArgs = aArgs[3].split('\t', QString::SkipEmptyParts);
				cCurXid = cpAppClient->eraseNode(this, aArgs/*LambdaList*/);
			}
			break;
		case geEval:					// _ais|eval|exp|%s
			if (aCount < 4 || aArgs[2] != "exp")
				append("submitAppRequest: eval. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->onSubmit(this, aArgs[3]/*exp*/, NULL/*Data*/, 0/*DataSize*/);
			break;
		case geExecute:					// _ais|execute|exp|%s
			if (aCount < 4 || aArgs[2] != "exp")
				append("submitAppRequest: execute. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->onSubmit(this, aArgs[3]/*exp*/, NULL/*Data*/, 0/*DataSize*/);
			break;
		case geExportCabinet:			// _ais|exportcabinet|cabinet|%s|file|%s
			if (aCount < 6 || aArgs[2] != "cabinet" || aArgs[4] != "file")
				append("submitAppRequest: exportCabinet. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->exportCabinetRemote(this, aArgs[3]/*CabName*/, aArgs[5]/*CabPath*/);
			break;
		case geExportNode:				// _ais|exportnode|node|%s|file|%s|cabinet|%s
			if (aCount < 8 || aArgs[2] != "node" || aArgs[4] != "file" || aArgs[6] != "cabinet")
				append("submitAppRequest: exportNode. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->exportNodeRemote(this, aArgs[7]/*CabName*/, aArgs[3]/*NodeName*/, aArgs[5]/*FilePath*/);
			break;
		case geGetConsoleLog:			// _ais|getconsolelog|clear|true
			if (aCount < 4 || aArgs[2] != "clear")
				append("submitAppRequest: getConsoleLog. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->getConsoleLog(this, aArgs[3] == "false" ? false : true/*Clear*/);
			break;
		case geGetCurrentContexts:		//_ais|getcurrentcontexts
			cCurXid = cpAppClient->onGetCurrentContexts(this);
			break;
		case geGetDirInfo:				// _ais|getdirinfo|dir|%s
			if (aCount < 4 || aArgs[2] != "dir")
				append("submitAppRequest: getDirInfo. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->getDirInfo(this, aArgs[3]/*Dir*/);
			break;
		case geGetExeSession:			// __ais|getexesession
			cCurXid = cpAppClient->getExeSession(this);
			break;
		case geGetExtentNames:			// __ais|getextentnames
			cCurXid = cpAppClient->getExtentNames(this);
			break;
		case geGetExtentTypes:			// __ais|getextenttypes
			cCurXid = cpAppClient->getExtentTypes(this);
			break;
		case geGetNextLevel:			// _ais|getnextlevel|node|%s
			if (aCount < 4 || aArgs[2] != "node")
				append("submitAppRequest: getNextLevel. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->getNextLevel(this, aArgs[3]/*FullNodeName*/);
			break;	
		case geGetSessions:
		case geGetSubscriptions:			// _ais|getsubscriptions|context|%s
			if (aCount < 4 || aArgs[2] != "context")
				append("submitAppRequest: getSubscriptions. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->onGetSubscriptions(this, aArgs[3]/*ContextName*/);
			break;
		case geGetWorkspaceStatistics:	// __ais|getworkspacestatistics
			cCurXid = cpAppClient->getWorkspaceStatistics(this);
			break;
		case geImportCabinet:			// _ais|importcabinet|cabinet|%s|path|%s
			if (aCount < 6 || aArgs[2] != "cabinet" || aArgs[4] != "path")
				append("submitAppRequest: importCabinet. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->importCabinetRemote(this, aArgs[3]/*CabName*/, aArgs[5]/*FilePath*/);
			break;
		case geIsContextOpen:			// _ais|iscontextopen|context|%s
			if (aCount < 4 || aArgs[2] != "context")
				append("submitAppRequest: isContextOpen. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->onIsContextOpen(this, aArgs[3]/*ContextName*/);
			break;
		case geLogoff:					// _ais|logoff
			cCurXid = cpAppClient->onLogoff(this);
			break;
		case geLogon:					// _ais|logon|user|%s|passwd|%s
			if (aCount < 6 || aArgs[2] != "user" || aArgs[4] != "passwd")
				append("submitAppRequest: logon. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->onLogon(this, aArgs[3]/*UsrName*/, aArgs[5]/*Passwd*/);
			break;
		case geNewCabinet:			// _ais|newcabinet|cabinet|%s|path|%s
			if (aCount < 6 || aArgs[2] != "cabinet" || aArgs[4] != "path")
				append("submitAppRequest: newCabinet. Unrecognized argument name in " + aBody);
			//else
				//cCurXid = cpAppClient->newCabinet(this, aArgs[3]/*CabName*/, aArgs[5]/*CabPath*/);
			break;
		case geOpenCabinet:				// _ais|opencabinet|cabinet|%s|path|%s
			if (aCount < 6 || aArgs[2] != "cabinet" || aArgs[4] != "path")
				append("submitAppRequest: OpenCabinet. Unrecognized argument name in " + aBody);
			//else
				//cCurXid = cpAppClient->openCabinet(this, aArgs[3]/*CabName*/, aArgs[5]/*CabPath*/);
			break;
		case geOpenConsoleLog:	// _ais|openconsolelog|clear|%s|redirect|%d|size|%d
			if (aCount < 8 || aArgs[2] != "clear" || aArgs[4] != "redirect" || aArgs[6] != "size")
				append("submitAppRequest: openConsoleLog. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->openConsoleLog(this,aArgs[3]=="false"?false:true,aArgs[5]=="false"?false:true,aArgs[7].toLong(),true);
			break;
		case geOpenContext:				// _ais|opencontext|path|%s
			if (aCount < 4 || aArgs[2] != "path")
				append("submitAppRequest: openContext. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->onOpenContext(this, aArgs[3]/*Path*/, QString()/*ContextName*/);
			break;
		case geOpenNode:				// _ais|opennode|extent|%s|nodename|%s
			if (aCount < 6 || aArgs[2] != "extent" || aArgs[4] != "nodename")
				append("submitAppRequest: openNode. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->openNode(this, aArgs[3]/*ExtentName*/, aArgs[5]/*FullNodeName*/);
			break;
		case geOpenSession:				// _ais|opensession|context|TestAis
			if (aCount < 4 || aArgs[2] != "context")
				append("submitAppRequest: opensession. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->onOpenSession(this, aArgs[3]);
			break;
		case geRegisterContext:			// _ais|registercontext|path|%s
			if (aCount < 4 || aArgs[2] != "path")
				append("submitAppRequest: registerContext. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->registerContext(this, aArgs[3]/*StartupPath*/);
			break;
		case geRunScriptFile:			// _ais|runscriptfile|file|%s
			if (aCount < 4 || aArgs[2] != "file")
				append("submitAppRequest: runScriptFile. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->runRemoteScriptFile(this, aArgs[3]/*FilePath*/);
			break;
		case geSaveNode:		// _ais|savenode|extent|%s|node|%s|text|%s
			if (aCount < 8 || aArgs[2] != "extent" || aArgs[4] != "node" || aArgs[6] != "text")
				append("submitAppRequest: saveNode. Unrecognized argument name in " + aBody);
			else
			{	QByteArray aText(aArgs[7].toLatin1());
				cCurXid = cpAppClient->saveNode(this,aArgs[3]/*Extent*/, aArgs[5]/*Node*/, aText);
			}
			break;
		case geSetBreakpoint:			// _ais|setbreakpoint|Lambda|%s
			if (aCount < 4 || aArgs[2] != "Lambda")
				append("submitAppRequest: setBreakpoint. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->setBreakPoint(this, aArgs[3]/*LambdaName*/);
			break;
		case geSetEngineFlags:			// _ais|setengineflags|flags|%d
			if (aCount < 4 || aArgs[2] != "flags")
				append("submitAppRequest: setEngineFlags. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->setEngineFlags(this, aArgs[3].toUInt()/*Flags*/);
			break;
		case geSetErrorTrace:			// _ais|seterrortrace|onoff|%d
			if (aCount < 4 || aArgs[2] != "onoff")
				append("submitAppRequest: setErrorTrace. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->setErrorTrace(this, aArgs[3]=="0" ? false : true/*OnOff*/);
			break;
		case geSetEscape:				// _ais|setescape|sessionid|%d
			if (aCount < 4 || aArgs[2] != "sessionid")
				append("submitAppRequest: setEscape. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->setEscape(this, aArgs[3].toLong()/*SessionId*/);
			break;
		case geSetJit:					// _ais|setjit|onoff|%d
			if (aCount < 4 || aArgs[2] != "onoff")
				append("submitAppRequest: setJit. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->setJit(this, aArgs[3]=="0" ? false : true/*OnOff*/);
			break;
		case geSetInstructionTrace:		// _ais|setinstructiontrace|onoff|%d
			if (aCount < 4 || aArgs[2] != "onoff")
				append("submitAppRequest: setInstructionTrace. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->setInstructionTrace(this, aArgs[3]=="0" ? false : true/*OnOff*/);
			break;
		case geSetLogLvl:				// _ais|setloglvl|logtype|%d|level|%d
			if (aCount < 6 || aArgs[2] != "logtype" || aArgs[4] != "level")
				append("submitAppRequest: setLogLvl. Unrecognized argument name in " + aBody);
			else
				cCurXid = cpAppClient->setLogLvl(this, (AReqType)aArgs[3].toInt()/*LogType*/,(AErrLvl)aArgs[5].toInt()/*Warnlvl*/);
			break;
		case geSetRules:				// _ais|setrules|rules|%s|remove|%d
			if (aCount < 6 || aArgs[2] != "rules" || aArgs[4] != "remove")
				append("submitAppRequest: setRules. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->setRules(this, aArgs[3]/*Rules*/, aArgs[5] == 0 ? false : true/*Remove*/);
			break;
		case geSetSubscriptions:		// _ais|setsubscriptions|new|%s|old|%s
			if (aCount < 6 || aArgs[2] != "new" || aArgs[4] != "old")
				append("submitAppRequest: setSubscriptions. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->setSubscriptions(this, aArgs[3]/*NewSessions*/, aArgs[5]/*OldSessions*/);
			break;
		case geSetSysCheck:			// _ais|setsyscheck|onoff|%d
			if (aCount < 4 || aArgs[2] != "onoff")
				append("submitAppRequest: setSysCheck. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->setSysCheck(this, aArgs[3]=="0" ? false : true/*OnOff*/);
			break;
		case geShowConsole:			// _ais|showconsole|exp|%s
			if (aCount < 4 || aArgs[2] != "exp")
				append("submitAppRequest: showConsole. Unrecognized argument name in " + aBody); 
			else
				cCurXid = cpAppClient->showConsoleSelection(this, aArgs[3]/*text*/);
			break;
		case geGetContextId:			// _ais|getcontextid|context|%s, _ais|getcontextid|sessionid|%d
		case geGetContextParams:		// _ais|getcontextparams|contextid|%d, _ais|getcontextparams|context|%s
		case geGetSessionUser:			// __ais|getsessionuser
		case geIsContextBusy:			// _ais|iscontextbusy|context|%s
		case geLogSysMsg:				// _ais|logsysmsg|level|%d|msg|%s
		case geNoop:					// _ais|noop|arg0|%s...
		case geRetFile:					// _ais|retfile|file|%s
			aBody.replace('|', '\177');
			cCurXid = cpAppClient->submit(this, aBody, aClientData, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/);
			break;
		default:
			append("submitAppRequest: Unrecognized act in " + aBody);
			break;
		}
	}
}
// end
