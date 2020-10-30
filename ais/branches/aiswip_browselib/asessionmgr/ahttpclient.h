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

#ifndef AHTTPCLIENT_H
#define AHTTPCLIENT_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/asessionmgr/ahttpclient.h
													AHttpClient Specification

CHANGE HISTORY
Version	Date		Who		Change
1.0116	11/30/2006	tlw		destructor. Remove referents for this class.
1.0057	 3/18/2005	tlw		Update documentation.
												---------------------------------
NOTES
 1.	This simple minded class just fetches web pages.  It does not fetch embedded images, fetch embedded frames, execute
	scripts, render pages, etc.  Pages are kept in-memory.

USAGE
 1.	Define an instance of this class every time a page is to be fetched.
 2.	Typically a custom event alerts the web client with a page request. The custom event handler can't wait around for a page
	to come back with the page, so it creates an instance of the webpage which makes an asynchronous request for a page and
	returns immediately.
 3.	In a multi-tasking environment, several of these web clients could be getting pages at the same time.  Each one is
	responding to different events and pumping data into a different buffer.
 4.	The data signal is issued from the QUrlOperator class and received by the dataSlot function which stores the data into the
	supplied buffer as the page is delivered on the socket connection to the web server.
 5.	When the entire requested page is received,  back to an ampmgr notifier	method.
 6.	Typically, the notifier calls back into session manager that unlocks the context waiting on this page.

DOCUMENTATION
 1.	QtDocs->Home->Examples->Network Examples

EXAMPLE
See webtest project
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QByteArray>
#include <QtNetwork/QHttp>
class QTimer;
class ASessionManager;
//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------

//	------------------------------------------------- CLASS DEFINITION --------------------------------------------------------
class AHttpClient : public QHttp
{
    Q_OBJECT
public:
	AHttpClient(ASessionManager* ipSMgr, long iSessionId, long iMsecToWait=0, QObject* ipParent=NULL, const char* ipName=NULL);
	~AHttpClient();
	void	cancelRequestHttp();
	void	requestHttp(const QString& irUrl, const QString& irBody = tr(""), const QString& irFileName = tr(""), long iMsecToWait = 0);

private slots:
	void	timerDoneSlot();
	void	dataSlot(const QHttpResponseHeader& irHdr);
    void	onRequestFinished(int iReqId, bool iError) throw();
	void	responseHeaderReceivedSlot(const QHttpResponseHeader& irHdr);
	void	stateChangedSlot(int iState);

private:
	enum APending {ceDone, ceAborting, ceClosing, ceDisconnecting, ceRequesting, ceSettingHost};
	void		abortTask();
	void		init(long iDefMsecToWait);
	void		launchRequest();
	void		launchSetHost();
	QByteArray	requestToUrl(QString& irStg, char iSep);
	QString		urlEncode(QString& irStg, bool iUsePlus);

	ASessionManager* cpSMgr;	// Used to call back to Session Mgr when done.
	QString		cBody;			// Body of pending POST message
	QString		cCookie;		// Cookie if a cookie has been set
	const char*	cpErrMsgs[8];	// Error messages returned from QHttp
	long		cDefMsecToWait;	// Default timer wait period in msec.
	QString		cFileName;		// Filename iff write response to disk
	QString		cHost;			// Default host name (localhost)
	QString		cNewHost;		// New host name when switching hosts.
	long		cMsecToWait;	// Current timer wait period in msec.
	APending	cPendingTask;	// Type of pending task.
	QByteArray	cRcvBfr;		// Holds response from web server
	long		cRcvd;			// Number of characters received so far.
	long		cReqId;			// Id of pending GET or POST request
	QString		cReqUri;		// Pending request (/amp.dll?_ais=noop&arg0=Hello)
	long		cSessionId;		// Identify session in call back to SMgr when done
	const char*	cpState[7];		// Connection state descriptions
	QTimer*		cpTimer;		// Timeout if no response

	const QHttpResponseHeader* cpResponseHdr;	// ->Received response header. 
};

#endif //AHTTPCLIENT_H

