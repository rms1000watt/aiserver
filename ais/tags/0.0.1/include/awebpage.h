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

#ifndef AWEBPAGE_H
#define AWEBPAGE_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/awebpage.h
														Web Page Specification

WebPage implements an HTTP client to allow the Lambdabase Server to fetch web pages.
For now, WebPage uses Qt's QUrlOperator class.
Later, we may build a more general-purpose class using QSocket.
 
CHANGE HISTORY
Version	Date		Who		Change
1.0107	 9/19/2006	tlw		AWebPage inherits QHttp.
1.0057	 3/18/2005	tlw		Add documentation
												--------------- ---------------

NOTES
 1.	This simple minded class just fetches web pages.  It does not fetch embedded images,
	fetch embedded frames, execute scripts, render pages, etc.  Pages are kept in-memory.

USAGE
 1.	Define an instance of this class every time a page is to be fetched.
 2.	Typically a custom event alerts the web client with a page request. The custom event
	handler can't wait around for a page to come back with the page, so it creates an
	instance of the webpage which makes an asynchronous request for a page and returns
	immediately.
 3.	In a multi-tasking environment, several of these web clients could be getting pages
	at the same time.  Each one is responding to different events and pumping data into
	a different buffer.
 4.	The data signal is issued from the QUrlOperator class and received by the dataSlot
	function which stores the data into the supplied buffer as the page is delivered on
	the socket connection to the web server.
 5.	When the entire requested page is received,  back to an ampmgr notifier
	method.
 6.	Typically, the notifier calls back into session manager that unlocks the context
	waiting on this page.

DOCUMENTATION
 1.	htmlmgrnotes.txt
 2.	QtDocs->Home->Examples->Network Examples

EXAMPLE
See webtest project
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtNetwork/QHttp>
#include "aglobals.h"				// gAis
#include "alogmgr.h"

class QTimer;
class AAisMgr;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
#define AWEBPAGE_SZ   4096			// Buffer increment size

//	-------------------------------------------------- CLASS DEFINITION -------------------------------------------------------
class AWebPage : public QHttp
{
    Q_OBJECT
public:
    AWebPage(AAisMgr* ipAisMgr, long iSessionId, long iMsecToWait);
    ~AWebPage();
	void		getPage(QString& irUrl);

public slots:
	void		cancelReadHtmlPage();

private slots:
	void		onRequestStarted(int iId);
	void		onStateChanged(int iState);
	void		onDataSend(int iDone, int iData);
	void		onDataRead(int iDone, int iTotal);
	void		onReadyRead(const QHttpResponseHeader& iHdr);
	void		onDone(bool iError);

	void		onResponseHeaderReceived(const QHttpResponseHeader& irHeader);
    void		onRequestFinished(int iId, bool iError);

private:
	AAisMgr*	cpAisMgr;		// Call notifyHtml method with results
	long		cGetId;			// Id associated with this request.
	bool		cReqAborted;	// Set to true if request is aborted.
	long		cSessionId;		// Saved from initiating call.
	long		cMsecToWait;	// Maximum time to wait for a response.
	QTimer*		cpTimer;		// Timer used to abort request if no response.
	bool		cTrace;			// If true, generate debug output
};

#endif //AWEBPAGE_H

