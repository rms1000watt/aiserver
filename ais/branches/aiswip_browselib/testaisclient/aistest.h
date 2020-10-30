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

#ifndef AISTEST_H
#define AISTEST_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/testaisclient/atest.h
													AIS Test Suite

CHANGE HISTORY
Version	Date		Who		Change
2.0001	12/30/2006	tlw		onResponse. Add ipData to the arguments.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0105	 9/11/2006	tlw		Remove cpProtocols. Replace ATcpSocket with ASocket. Add AppClient for APP protocol
1.0070	 2/13/2005	tlw		Convert to QT4
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QFile>
#include "aglobals.h"			// AReturnRcvr
#include "ui_aistest.h"

class QTimer;
class AAppClient;
class ASocket;

//	----------------------------------------------------- DEFINITIONS ---------------------------------------------------------
#define AISTEST_FILESPEC	"testsuite.tst"	// Test suite file path + file name
#define AISTEST_TIMEOUT		 1000			// Max time to wait for response from AIS [msec.]

// Sockets. Index to connections to server.
#define AISTEST_APPSOCKET	0				// A0: requests
#define AISTEST_HTTPSOCKET	1				// HO: requests
#define AISTEST_HTTPSOCKET1	2				// H1: requests
#define AISTEST_XMLSOCKET	3				// X0: requests
#define AISTEST_SOCKETS		4				// Total number of clients

typedef struct
{	QString		mCookie;	// Current cookie returned by server.
	QString		mHost;		// DNS name or IP address of the server
	ushort		mPort;		// Port that this server is listening on.
	ASocket*	mpSocket;	// -> a TCP socket client.
	long		mXid;		// Incrementing integer to provide unique identifier for each request.
}ASocketSt;
typedef QList<ASocketSt> ASocketList;		// List of socket structures, one for each connection to a server.

// Symbols. Symbolic references in test suite.
typedef QMap<QString, QString> AStringMap;	// Symbol list. Key: variable name, Value: variable value.

//	------------------------------------------------ CLASS DEFINITIONS --------------------------------------------------------
/*!
\brief AAisTest - Implements the AIS test suite

\par Notes:
-# The test suite got started with just one client and grew slowly. Now, it has just one ASocket class which implements the
App, Http, and Xml protocol clients.  The test suite now has just one array of Protocol clients and requests are dispatched
with an ID to identify the protocol.
 */
class AAisTest : public QMainWindow, public AReturnRcvr
{
    Q_OBJECT
public:
    AAisTest(QWidget* ipParent, const QString& irName, const char* ipTestFileSpec, Qt::WFlags iFlgs = 0);
    ~AAisTest();
	// Inherited from AReturnRcvr:
	virtual bool	connectionClosed(long iDummyId)
    {
        iDummyId = iDummyId;
        return false;
    };
	virtual void	returnOutput(long iConnectId, long iXid, long iStatus,AReqType iReqType,long iRetValue,const QString& irOut
					, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
					= QString());
	virtual void	setContextName(long iConnectId, const QString& irContextName)
    {
        iConnectId = iConnectId;
        cContextName = irContextName;
    };

private slots:
	void		onConnected();
	void		onDisconnected();
	void		onLastTest();
	void		onNextTest();
	void		onResponse(QByteArray iBody, QByteArray iCookie, char* ipData);
	void		onResponseX0(QByteArray iXml, QByteArray iCookie, char* ipData);
	void		onRestart();
	void		onShowResponse();
	void		onSocketError(long iStatus, QString iError);
    void		onSubmit();
	void		onTimeout();

private:
	void		append(const QString& irMsg);
	void		buildHttpRequest(const QString& irPath, const QString& irHost);
	void		buildPostRequest(const QString& irPath, const QString& irHost, const QString irBody);
	void		buildXmlRequest(const QString& irRequest);
	void		expand();
	void		nextTest(bool iRedoLast);
	void		readFile(const QString& irFileSpec);
	void		response(const QString irResponse);
	void		submit(const QString& irRequest, bool iAbort);
	void		submitAppRequest(const QString& irBody);

	AAppClient*	cpAppClient;				// -> AppClient for use by APP protocol
	QString		cContextName;				// Current context name for use by APP protocol
	long		cCurXid;					// Current request identifier.
	long		cCurIx;						// Current index into cSockets
	QString		cCurRequest;				// Pending request as submitted for the specified protocol
	QString		cExpanded;					// Current expected response with variables expanded
	QStringList	cExpectedList;				// Holds expected responses and set variable commands
	long		cNextExpected;				// Index into texts to next expected response
	long		cNumExpected;				// Number of expected responses in Expected
	bool		cPass;						// True iff current test meets expectations
	long		cPending;					// Number of responses pending for this test
	QString		cRequest;					// Last request
	QString		cResponse;					// Last response
	bool		cRunAll;					// Do continuous testing without pause
	bool		cShowResults;				// Display request results.
	ASocketList	cSockets;					// Socket state for each server connection.
	QString		cTestFileSpec;				// TestSuite file path + file name.
	QStringList	cTests;						// List of tests to be performed
	long		cTimeout;					// Timer wait period in msec.
	QTimer*		cpTimer;					// Response timer. Times out if no response from server.
	AStringMap	cVariables;					// Key: var name, Value: var value.

	Ui::AisTestClass	cUi;
};

#endif // AISTEST_H

