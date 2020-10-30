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
aisdev/asessionmgr/asessionmgr.cpp
														Session Manager

CHANGE HISTORY
Version	Date		Who		Change
4.002	 9/28/2008	tlw		CR128. Add subscribe to allow a connection subscribe to a session.
5.0000   8/20/2008  fchua   MySQL Support.
4.0003	 7/02/2008	fchua	[CR-122] Fixed Start/Stop context functions.
4.0003	 7/01/2008	fchua	[CR-120] Fixed loading of the closemode context parameter.
3.2005	 2/18/2008	fchua	Added getRequestStats, getSessionStats.
3.1005	12/16/2007	fchua	Changed QList implementation of keeping sessions to QHash.
3.1005	12/16/2007	fchua	Added cleanupSession method.
3.1005	12/16/2007	fchua	Renamed mpRingFile to mpDisconnectedBuffer.
3.1004	11/13/2007	fchua	Modified enableConsoleLog. Fixed deadlock problem.
2.0001	12/29/2006	tmay	added geExecute
1.0120	12/19/2006	tlw		cbReturnResult. Add ipData and iDataSize arguments.
1.0118	12/12/2006	tlw		submit. Add serial binary stream argument and add mpData property to ARequest
1.0116	12/4/2006	tlw		quit. Add quit and fix error in destructor that caused contexts to not be closed.
1.0113	11/13/2006	tlw		Destructor. Do not delete mpContext. It is not a referent.
1.0112	10/27/2006	tlw		cOpenSockets. Reclaim AAppSocketSt on destruct. registerContext. Add workdir to read script file.
1.0110	10/20/2006	tlw		getMainThread. For use when moving AContextClient to main thread
1.0107	 9/20/2006	tlw		readHtmlPage. Unlock mutex if mReadHtmlOk.
1.0107	 9/18/2006	tlw		~ASessionManager. Revise destructor to make sure all threads are removed on exit.
1.0062   8/ 3/2006  mfk     setEscape. Fixed the bug wherein a STOP request from the console
1.0062   8/ 3/2006  mfk     Fixed bug where console display from HTML page initiated processes improperly double spaces.
1.0062	 5/ 8/2005	tlw		Add clear and record-oriented records to openConsoleLog. appendRecord to saveConsoleLog.
1.0061	 5/ 5/2005	tlw		registerContext. readPairs excludes globals.
1.0057	 3/18/2005	tlw		Update documentation.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <stdlib.h>
#include <malloc.h>
#include <QtCore/QEvent>			// QEvent
#include <QtCore/QFileInfo>
#include <QtCore/QTextStream>		// QTextStream
#include <QtCore/QDir>

#include "asessionmgr.h"
#include "acontextthread.h"
#include "aglobals.h"
#include "aissvr.h"					// AAisSvr
#include "alogmgr.h"				// ALogMgr
#include "ahttpclient.h"			// AHttpClient
#include "aisoutput.h"				// AisMgrOutputEvent
#include "anamedvalues.h"			// ANamedValues
#include "aringfile.h"				// ARingFile
#include "ascriptparams.h"			// AScriptParams
#include "ausrmgr.h"				// AUsrMgr
#include "autilities.h"				// AUtil
#include "asbglue.h"

#if __EXMYSQL
// Implementation Note: If MySQL is included (FSmtbase.h), we must also
// add the following include path to the properties for this source file:
// "C:\Program Files\MySQL\MySQL Server 5.1\include"
typedef unsigned int  SOCKET;
extern "C" { // includes for modules written in C
	#include "mysql.h"
	}
#endif

//	------------------------------------------------- MODULE DEFINITIONS ------------------------------------------------------
//	gpSessionManager is here to support the functions in asmfunctions.cpp. This value is initialized in the ASessionManager
//	constructor. Only one copy of SessionMgr per server!
ASessionManager *gpSessionManager;

//	------------------------------------------------- ASESSION FUNCTIONS ------------------------------------------------------
/*!
\brief Destructor Free allocated resources

\par Notes:
-# \par Notes:
-# ASession has remote ownership of AHttpClient, ARequest, and ARingFile referents.
-# Since instances of ASession are never assigned copied, or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
ASession::~ASession()
{
	if (mpHttpClient != NULL)
		delete mpHttpClient;

	if (mpRequest != NULL)
		delete mpRequest;

	if (mpDisconnectedBuffer != NULL)
		delete mpDisconnectedBuffer;

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~ASession()");
#endif
}

//	----------------------------------------------- ASESSIONMGR FUNCTIONS -----------------------------------------------------
/*!
\brief ASessionManager - Submits requests to the engine and processes results from the engine.

\return void
 */
ASessionManager::ASessionManager()
	: cpAisMgr(NULL), cpAisSvr(NULL)
{
	// Initialize the engine's global variables
	cSessionMgrMutex.lock();
	SBGlue_Init();

	// See the destructor for how contexts get removed!
	cNextRequestID = 0;
	cOpenContexts.clear();
	cpMainThread = thread();
	cMt = "";

	// Set first null session entry
	ASession* apSession = new ASession();
	apSession->mpContext = (AContextSt*)1;			// Mark as in-use
	apSession->mContextID = 0;
	apSession->mCurrentRequestID = 0;
	apSession->mDefaultCloseMode = gAis.mCloseModes[AIS_CLOSEMODE];
	apSession->mEngineFlags = 0;
	apSession->mpHttpClient = NULL;
	apSession->mMode = geOpen;
	apSession->mNoDebug = 1;
	apSession->mReadHtmlOk = false;
	apSession->mpRequest = NULL;
	apSession->mRequestHttpOk = false;
	apSession->mpDisconnectedBuffer = NULL;
	apSession->mSecurityLevel = 0;
	apSession->mSessionType = geAdminSession;
	apSession->mStale = false;
	apSession->mUsrID = 0;
	cSessions.insert(0, apSession);

	cEmbeddedMySQLEnabled = false;

	// Note that only one instance of SessionManager is allowed!
	gpSessionManager = this;

	cSessionMgrMutex.unlock();
}

// 1. Cannot sleep on a thread that has returned from its event loop.
// 2. Cannot call deleteLater while running on a thread that has returned from its event loop.
// 3. If a mutex is locked while sleeping, the thread will not start any other waiting tasks.
// 4. This destructor is called from the main event loop, but stopped context threads have returned from their event loops.

//Closing Down.
// 1. ~ASessionMgr sets ipContext->mAlive = -1 which causes
// 2. AContextThread::onProcessNextReqest drops out of loop  and calls quit which causes
// 3. AContextThread::run returns from exit and sets cQuit true which causes
// 4. ~ASessionMgr drops out of loop and calls removeContext which deletes the thread which causes
// 5. ~AContextThread which terminates the thread so that it is not left lying around after application is terminated.
ASessionManager::~ASessionManager()
{
	// cOpenContexts. Remove each AContextSt from cOpenContexts list.
	long aContext;
	long aMaxTries = 1000, aTry;			// 30720 MaxTries * 2 Msec = about 1 minute
	AContextSt* apContext;
	AContextThread* apThread;
	cSessionMgrMutex.lock();
	long aReq, aReqSz, aSz = cOpenContexts.size(); // Size will not change inside this loop.
	cSessionMgrMutex.unlock();
	for (aContext = 0; aContext < aSz; ++aContext)
    {
        cSessionMgrMutex.lock();
		if ((apContext = cOpenContexts[aContext]) != NULL)
        {
            apContext->mAlive = -1;		// (empty <0, registered =0, opened >0)
			if ((apThread = apContext->mpThread) != NULL)
            {
                // Wait for thread to finish current request, if any.
				for (aTry = 0; aTry < aMaxTries; ++aTry)
                {
                    cSessionMgrMutex.unlock();
					apThread->localSleep(2/*Msec*/);
					cSessionMgrMutex.lock();
					if (apThread->hasQuit())
						break;
				}

				if (aTry == aMaxTries)
                {
					apThread->exit(0);
                }
			}
			// AContextSt. Since it is a structure, it does not have its own destructor.
			if (apContext->mpCodeData != NULL)
				free(apContext->mpCodeData);
			if (apContext->mpRequest != NULL)
				delete apContext->mpRequest;
			aReqSz = apContext->mRequests.size();
			for (aReq = 0; aReq < aReqSz; ++aReq)
				delete apContext->mRequests[aReq];
			delete apContext;
		}
		cSessionMgrMutex.unlock();
	}

	// Close Sessions.
	// get 1st entry
	ASessionTbl::iterator aIter = cSessions.begin();
	while (aIter != cSessions.end())
	{
		delete aIter.value();
		// move to next entry
		aIter++;
	}

	cSessions.clear();
	cOpenContexts.clear();

    if (cEmbeddedMySQLEnabled)
        mysql_library_end();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~ASessionManager()");
#endif
}

// Not currently used, but it is called from AisSvr::onQuit so that some selective portions of the sessionMgr can be shutdown
// early to avoid a later conflict.  In particular, it may be necessary to shutdown the contexts before they send back any
// signals or events to the GUI which is in the process of being torn down.
void ASessionManager::quit()
{
#ifdef AIS_DEBUG
	qDebug("ASessionManager::quit()");
#endif
}

//	bfrDisplayOutput is called by SM_Display (on context thread). Locks SMgr. Puts display info into the display buffer for
//	this session. Unlock SMgr.
long ASessionManager::bfrDisplayOutput(long iSessionId, QString &irBfr)
{
	long aCode = 0;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId))
		aCode = ABAD_SESSIONID;
	else
	{	ASession* apSession = cSessions[iSessionId];
		if (apSession->mpContext == NULL)
			aCode = ASESSION_CLOSED;
		else
		{	QString& arBfr = apSession->mDsplyBfr;
			arBfr += irBfr;
			while (arBfr.length() > ASMGR_MAXBFRSIZE)
			{	cbDisplay(iSessionId, arBfr.left(ASMGR_MAXBFRSIZE));
				arBfr = arBfr.mid(ASMGR_MAXBFRSIZE);
				apSession->mStale = false;
			}
		}
	}
	cSessionMgrMutex.unlock();
	return aCode;
}

/*!
\brief cancel -Cancel a pending request

\param iSessionId - The session that submitted this request
\param iRequestID - The ID of the request to be cancelled
\return aCode - -1 if requestId is executing or if request is no longer in the queue
\par Notes:
-# Executing requests cannot be cancelled.  The user should call setEscape instead.
 */
AErrorCode ASessionManager::cancel(ASessionID iSessionId, ARequestID iRequestID)
{
	cSessionMgrMutex.lock();
	ARequestList& arRequests = cSessions[iSessionId]->mpContext->mRequests;
	ARequest* apReq;
	long aCode = -1;
    for (long aReq = 0; aReq < arRequests.size(); ++aReq)
	{	apReq = arRequests.at(aReq);
		if (apReq->mSessionID == iSessionId && (apReq->mRequestID == iRequestID) && !apReq->mExecuting)
		{	arRequests.removeAt(aReq);
			delete apReq;
			aCode = 0;
			break;
		}
	}
	cSessionMgrMutex.unlock();
	return	aCode;
}

/*!
\brief cancelAll cancel all requests on the specified session.

\param iSessionId - The ID of the session that submitted the requests to be cancelled
return 0 (no error)
 */
AErrorCode ASessionManager::cancelAll(ASessionID iSessionId)
{
	long aContextIndex;
	ARequest* apReq;
	cSessionMgrMutex.lock();
	ASession* apSession = cSessions[iSessionId];
	aContextIndex = apSession->mpContext->mContextIndex;
	ARequestList& arRequests = apSession->mpContext->mRequests;
    for (long aReq = 0; aReq < arRequests.size();)
	{	apReq = arRequests.at(aReq);
		if (apReq->mSessionID == iSessionId)
		{	if (!apReq->mExecuting)
			{	arRequests.removeAt(aReq);
				delete apReq;
			}
			else // Set the escape flag to cause execution in context to stop
			{	SBGlue_SetEngineFlags(aContextIndex,SBGLUE_ESCAPE, 1L);
				++aReq;
			}
		}
		else
			++aReq;
	}
	delete cSessions.take(iSessionId);
	cSessionMgrMutex.unlock();
	return 0;
}
// Callback functions - called on an engine thread. Pass payload back to originator
long ASessionManager::cbCancelReadHtmlPage(long iSessionId)
{
	// Post an event to AisMgr to cancel the pending ReadHtmlPage
	AisMgrOutputEvent* apEv = new AisMgrOutputEvent(iSessionId, -1/*ReqId*/, 0/*Status*/, cMt/*Enctype*/, 0/*RetValue*/,
	cMt/*AisOut*/, NULL/*Data*/, 0/*DataSize*/, cMt/*Display*/,cMt/*Error*/, AISMGROUT_CANCELREADHTMLPAGE);
	QCoreApplication::postEvent(cpAisMgr, apEv);
	return 0;
}

long ASessionManager::cbDebug(long iSessionId, long iReqID, QString& irBuf)
{
	AisMgrOutputEvent* apEv = new AisMgrOutputEvent(iSessionId, iReqID, 0/*Status*/, cMt/*Enctype*/, 0/*RetValue*/,
	irBuf/*AisOut*/, NULL/*Data*/, 0/*DataSize*/, cMt/*Display*/,cMt/*Error*/, AISMGROUT_DEBUG);
	QCoreApplication::postEvent(cpAisMgr, apEv);
	return 0;
}

long ASessionManager::cbDisplay(long iSessionId, QString iDisplay)
{
	AisMgrOutputEvent* apEv = new AisMgrOutputEvent(iSessionId, 0/*ReqID*/, 0/*Status*/, cMt/*Enctype*/, 0/*RetValue*/,
	 cMt/*AisOut*/, NULL/*Data*/, 0/*DataSize*/, iDisplay, cMt/*Error*/, AISMGROUT_DISPLAY);
	QCoreApplication::postEvent(cpAisMgr, apEv);
	return 0;
}

long ASessionManager::cbReadHtmlPage(long iSessionId, QString& irUrl, long iMsecToWait)
{
	// Post an event to AisMgr which will ask HtmlMgr to fetch the page
	AisMgrOutputEvent* apEv = new AisMgrOutputEvent(iSessionId, -1/*ReqId*/, 0/*Status*/, cMt/*Enctype*/, iMsecToWait,
	irUrl/*AisOut*/, NULL/*Data*/, 0/*DataSize*/, cMt/*Display*/, cMt/*Error*/, AISMGROUT_READHTMLPAGE);
	QCoreApplication::postEvent(cpAisMgr, apEv);
	return 0;
}

long ASessionManager::cbReturnResult(long iSessionId, long iReqID, const char* ipError,	const char* ipEnctype
, const char* ipText, char* ipData, long iDataSize, bool iForwarding)
{
	ASession* apSession;
	AisMgrOutputEvent *apEv;
	AContextSt *apContext;
	QString aText(ipText);
	QString aEnctype(ipEnctype);
	bool aRemote;
	long aCode = 0;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId))
		aCode = ABAD_SESSIONID;
	else
	{	apSession = cSessions[iSessionId];
		if ((apContext = apSession->mpContext) == NULL)
			aCode = ASESSION_CLOSED;
		else
		{	long aReq;
			long aMsgType = iForwarding ? AISMGROUT_RETURNRESULT_FORWARDING : AISMGROUT_RETURNRESULT;
			// Find request and check if it is a aRemote request
			ARequestList& arRequests = apContext->mRequests;
			ARequest* apReq = NULL;
			aRemote = false;
			long aSize = arRequests.size();
			for (aReq = 0; aReq < aSize; ++aReq)
			{	apReq = arRequests.at(aReq);
				if (apReq->mRequestID == iReqID)  // found request
				{	aRemote = apReq->mRemoteRequest;
					break;
				}
			}
			if (!aRemote && aReq < aSize)
			{	QString aDisplay(apSession->mDsplyBfr);
				QString aError(ipError);
				apEv = new AisMgrOutputEvent(iSessionId, iReqID, 0/*Status*/, aEnctype, 0/*RetValue*/, aText/*AisOut*/, ipData
				, iDataSize, aDisplay, aError, aMsgType);
				apSession->mStale = false;
				apSession->mDsplyBfr.truncate(0);
				QCoreApplication::postEvent(cpAisMgr, apEv);
				arRequests.removeAt(aReq);
				delete apReq;
			}
		}
	}
	cSessionMgrMutex.unlock();
	return aCode;
}
long ASessionManager::cbRingBell(long iSessionId, long iReqID)
{
	QString aOut("Ring A Ding Ding");
	AisMgrOutputEvent* apEv = new AisMgrOutputEvent(iSessionId, iReqID, 0/*Status*/, cMt/*Enctype*/, 0/*RetValue*/, aOut
	, NULL/*Data*/, 0/*DataSize*/, cMt/*Display*/,cMt/*Error*/, AISMGROUT_RINGBELL);
	QCoreApplication::postEvent(cpAisMgr, apEv);
	return 0;
}
long ASessionManager::cbSendEngineStateToSessions(long iSessionId, long iEngineState)
{
	QString aOut = QString::number(iEngineState);
	AisMgrOutputEvent* apEv = new AisMgrOutputEvent(iSessionId, -1/*ReqID*/, 0/*Status*/, cMt/*Enctype*/, iEngineState
	, aOut, NULL/*Data*/, 0/*DataSize*/, cMt/*Display*/, cMt/*Error*/, AISMGROUT_ENGINESTATE);
	QCoreApplication::postEvent(cpAisMgr, apEv);
	return 0;
}
long ASessionManager::cbSendToClient(long iSessionId, QString& irMsg)
{
	AisMgrOutputEvent* apEv = new AisMgrOutputEvent(iSessionId, -1/*ReqID*/, 0/*Status*/, cMt/*Enctype*/, 0/*RetValue*/
	, irMsg/*AisOut*/, NULL/*Data*/, 0/*DataSize*/, cMt/*Display*/,cMt/*Error*/, AISMGROUT_SENDTOCLIENT);
	QCoreApplication::postEvent(cpAisMgr, apEv);
	return 0;
}

//	FileOpen
long ASessionManager::cbServerFileOpenDialog(long iSessionId, long iReqID)
{
	QString aOut("Give me a filename? just type in _svrfile=<filename>");
	AisMgrOutputEvent* apEv = new AisMgrOutputEvent(iSessionId, iReqID, 0/*Status*/, cMt/*Enctype*/, 0/*RetValue*/, aOut
    , NULL/*Data*/, 0/*DataSize*/, cMt/*Display*/, cMt/*Error*/, AISMGROUT_FILEOPEN);
	QCoreApplication::postEvent(cpAisMgr, apEv);
	return 0;
}

/*!
 * \brief Performs final session clean up (geSoft, geFirm, geHard).
 *
 * \param iSessionId Session Id.
 */
long ASessionManager::cleanupSession(ASessionID iSessionId)
{
	long aCode = AOK;
	long aReqId = 0;
	long aStatus = 0;
	AisMgrOutputEvent* apEv = 0;
	ARequest* apReq = 0;
	ASession* apSession = 0;
	AContextSt* apContext = 0;
	ARequestList* apRequests = 0;
	long aConnectId = 0;
	QString aDisplay;
	QString aOut("true");
	QString aEnctype("plain/text");

	if (iSessionId <= 0 || !cSessions.contains(iSessionId))
	{
		// Invalid Session ID
		aCode = ABAD_SESSIONID;
	}
	else
	{
		apSession = cSessions[iSessionId];
		apContext = apSession->mpContext;
		apRequests = &apContext->mRequests;

		// Return response to closeSession request.
		// iSessionId is the ID of the session being closed (not necessarily the caller)
		if ((apReq = apSession->mpRequest) != NULL)
		{
			aDisplay = apSession->mDsplyBfr;
			aReqId = apReq->mRequestID;
			aConnectId = apReq->mNoDebug;		// ConnectId if caller is not closing itself.
			apEv = new AisMgrOutputEvent(iSessionId, aReqId, aStatus, aEnctype,	aConnectId/*RetValue*/, aOut, NULL/*Data*/
			, 0/*DataSize*/, aDisplay, cMt/*Error*/, AISMGROUT_CLOSESESSION);
			QCoreApplication::postEvent(cpAisMgr, apEv);
			delete apSession->mpRequest;
			apSession->mpRequest = NULL;
		}

		// Remove session object from session table and deallocate
		delete cSessions.take(iSessionId);

		// Close Context. If closing a context, return response if no more requests pending.
		// Even if more sessions are closed, no more responses to reqs are pending, so OK to respond.
		if ((apReq = apContext->mpRequest) != NULL && apRequests->isEmpty())
		{
			// return context to registered state
			apContext->mAlive = 0;
			apContext->mMode = geOpen;

			// terminate our thread
			apContext->mpThread->exit(0);
			apContext->mpThread = NULL;

			aReqId = apReq->mRequestID;
			aConnectId = apReq->mNoDebug;		// Holds connect ID
			apEv = new AisMgrOutputEvent(apReq->mSessionID, aReqId, aStatus, aEnctype, aConnectId/*RetValue*/, aOut, NULL/*Data*/
			, 0/*DataSize*/, aDisplay, cMt/*Error*/, AISMGROUT_CLOSESESSION);
			QCoreApplication::postEvent(cpAisMgr, apEv);
			delete apContext->mpRequest;
			apContext->mpRequest = NULL;
		}
	}
	return aCode;
}

long ASessionManager::clearHtmlPage(long iSessionId)
{
	cSessionMgrMutex.lock();
	long aCode = AOK;
	if (iSessionId <= 0 || !cSessions.contains(iSessionId))
		aCode = ABAD_SESSIONID;
	else
	{	ASession* apSession = cSessions[iSessionId];
		if (apSession->mpContext == NULL)
			aCode = ABAD_SESSIONID;
		else
			apSession->mPageContent.truncate(0);
	}
	cSessionMgrMutex.unlock();
	return aCode;
}

long	ASessionManager::clearRequestHttp(long iSessionId)
{
	long aCode = AOK;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId))
		aCode = ABAD_SESSIONID;
	else
	{	ASession* apSession = cSessions[iSessionId];
		if (apSession->mpContext == NULL)
			aCode = ABAD_SESSIONID;
		else
			apSession->mPageContent.truncate(0);
	}
	cSessionMgrMutex.unlock();
	return aCode;
}

/*!
 * \brief Clear pending requests for this session in reverse order and close session.
 *
 * \param[in] iSessionId Session Id.
 *
 * \note Must be executed within a mutex lock.
 * Returns error response for each cancelled request (in reverse order) and
 * returns result to caller
 */
void ASessionManager::clearRequests(long iSessionId)
{
	long aReqId, aStatus = 0;
	AisMgrOutputEvent* apEv;
	ARequest* apReq;
	ASession* apSession = cSessions[iSessionId];
	AContextSt* apContext = apSession->mpContext;
	ARequestList& arRequests = apContext->mRequests;
	QString aEnctype("plain/text");
	for (long aReq = 0; aReq < arRequests.size();)
	{	apReq = arRequests.at(aReq);
		if (iSessionId == apReq->mSessionID)
		{	aReqId = apReq->mRequestID;
			QString aAisOut(apReq->mCmdString);
			apEv = new AisMgrOutputEvent(iSessionId, aReqId, aStatus, aEnctype, 0/*RetValue*/, aAisOut, NULL/*Data*/
			, 0/*DataSize*/, cMt/*Display*/, cMt/*/Error*/, AISMGROUT_RETURNRESULT);
			QCoreApplication::postEvent(cpAisMgr, apEv);
			arRequests.removeAt(aReq);
			delete apReq;
		}
		else
			++aReq;
	}
}

/*!
\brief closeConnection - Called when a connection is unexpectedly closed.

The session is not necessarily closed.  The client may disconnect, yet keep the session alive.
\param iSessionId - The Id of the session to be closed
\return 0, an error code (<0), or the pending requestId(>0) if a request is put on the queue.
\par Notes:
-# If the OnDisconnect parameter specifies an AisLisp Lambda, this Lambda is called just before the session is closed.
An application may use this Lambda to implement a cleanup operation.
The call to the onDisconnect Lambda includes the sessionId.
 */
ARequestID ASessionManager::closeConnection(ASessionID iSessionId)
{
	long aReqId = 0;
	AContextSt* apContext;
	ASession* apSession;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) ||
		(apSession = cSessions[iSessionId])	== NULL || (apContext = apSession->mpContext) == NULL)
	{	aReqId = ABAD_SESSIONID;
		cSessionMgrMutex.unlock();
	}
	else	// OnDisconnect. Call the onDisconnect function to notify parent context
	{	QString aOnDisconnectLambda;
		AStringMap& arContextParams = apContext->mContextParams;
		long aAdminSessionId = apContext->mAdminSessionId;
		if (aAdminSessionId>0&&arContextParams.contains("ondisconnect")&&!(aOnDisconnectLambda=arContextParams["ondisconnect"]).isEmpty())
		{	// Put a request to call OnDisconnect Lambda on the context admin session request queue.
			// Note that this connection is severely restricted in that no further communication with the client is possible.
			cSessionMgrMutex.unlock();
			QByteArray aCmdStg(aOnDisconnectLambda.toLatin1().data());
			aCmdStg += ' ';
			aCmdStg += QByteArray::number((int)iSessionId);
			aReqId = submit(aAdminSessionId, SBGLUE_CMDSTRING, SBGLUE_NO_DEBUG, aCmdStg, QDateTime::currentDateTime(), NULL/*Data*/);
		}
		else
			cSessionMgrMutex.unlock();
	}
	return aReqId;
}

/*!
\brief closeContext -  Close all sessions open on this context and shut down the context

The context is closed only after all the outstanding requests have completed or cleared from the request queue.
\param iConnectId - The ID for the calling connection.
\param  irContextName - The name of the context to be closed.
\param iCloseMode - the mode of dealing with pending requests (soft, firm, or hard).
\par Notes:
-# closeContext is special in that it returns a connect ID rather than a session ID.
-# Returns immediately if an error; else, returns via cbReturnResult
-# CloseModes:
\verbatim
soft - No new requests, allow pending reqs to finish
firm - No new requests, clear pending reqs from input queue
hard - No new reqeusts, clear pending reqs, stop executing req. (if any)
\endverbatim
 */
ARequestID ASessionManager::closeContext(long iConnectId, const QString& irContextName, ACloseMode iCloseMode)
{
	// Close Mode. Lookup default close mode
	long aContextId = -1, aReqId = AUNKNOWN_CONTEXTNAME;
	AContextSt* apContext;

	cSessionMgrMutex.lock();
	if (cContextMap.contains(irContextName) && (aContextId = cContextMap.value(irContextName)) > 0 &&
		aContextId < cOpenContexts.size() && (apContext = cOpenContexts[aContextId])!=NULL)
	{	// Set Default CloseMode. Set mode if not already set
		if (iCloseMode <= geDefault)
			iCloseMode = apContext->mDefaultCloseMode;

		// Freeze context. Stop all further submissions. Flush any lingering output
		apContext->mMode = iCloseMode;
		cSessionMgrMutex.unlock();
		flushSessions(true/*Now*/);

		// Pending Request.  Note pending request. Save ContextId in NoDebug.
		if (iConnectId > 0)
		{	aReqId = ++cNextRequestID;
			apContext->mpRequest = new ARequest(aContextId, aReqId, SBGLUE_MSGBLOCK/*EvalType*/, iConnectId/*NoDebug*/,
			"_ais|closeContext", NULL/*Data*/, QDateTime::currentDateTime());
		}
		else
		{	apContext->mpRequest = NULL;
			aReqId = 0;			// Immediate return
		}
		// Close Sessions.  Close sessions open on this context in reverse order.
		ASession* apSession;
		cSessionMgrMutex.lock();

		// Get list of keys (Session Id) in session table
		// Note: Sessions are not stored sequentially so there is no way to determine which was created first (at least for now)
		QList<ASessionID> aSessIdLst = cSessions.keys();
		QList<ASessionID>::iterator aSessIdLstIter = aSessIdLst.begin();
		while (aSessIdLstIter != aSessIdLst.end())
		{
			if (*aSessIdLstIter != 0)
			{
				// Retrieve session object
				apSession = cSessions[*aSessIdLstIter];
				if (apSession->mContextID == aContextId)
				{
					cSessionMgrMutex.unlock();
					closeSession(iCloseMode, 0 /*ConnectId*/, *aSessIdLstIter);
					cSessionMgrMutex.lock();
				}
			}
			// Move to next Session Id in list
			aSessIdLstIter++;
		}
	}
	else if (aContextId == 0)
		aReqId = -AERR_SYSTEMCONTEXT;

	cSessionMgrMutex.unlock();
	return aReqId;
}

/*!
\brief closeSession.  Called by the host application to close a user's session.

\param iCloseMode - Default, Soft, Firm, Hard
\param iConnectId - Connection ID of the caller.
\param iSessionId - Session ID of the session to be closed
\return Error code (<0) if an error, RequestID (>0) if a request posted.
\par Notes:
-# Returns immediately if an error; else, returns async response via an event.
-# CloseModes:
\verbatim
geDefault - Set iMode to DefaultCloseMode
geSoft    - No new requests, allow pending requests to finish
geFirm    - No new requests, clear pending requests, allow executing request to finish
geHard    - No new requests, clear pending requests, stop executing request
\endverbatim
 */
ARequestID ASessionManager::closeSession(ACloseMode iCloseMode, long iConnectId, ASessionID iSessionId)
{
	long aReqId = AOK;
	bool aPendingRequests = false;
	long aStatus = 0;
	ASession* apSession = 0;
	AContextSt* apContext = 0;
	AisMgrOutputEvent* apEv = 0;
	QString aOut("true");
	QString aEnctype("plain/text");
	cSessionMgrMutex.lock();

	if (iSessionId <= 0 || !cSessions.contains(iSessionId))
		aReqId = ABAD_SESSIONID;
	else if ((apSession = cSessions[iSessionId]) != NULL && (apContext = apSession->mpContext) != NULL)
	{
		aReqId = ++cNextRequestID;

		// Session Mode. Set session mode to CloseMode or DefaultCloseMode if CloseMode is not specified by caller.
		apSession->mMode = iCloseMode = (iCloseMode == geDefault) ? apSession->mDefaultCloseMode : iCloseMode;

		// if close mode is geDisconnect or geDefault
		if (apSession->mMode < geOpen)
		{
			// do not clear session requests, but send response to closesession
			apEv = new AisMgrOutputEvent(iSessionId, aReqId, aStatus, aEnctype,	iConnectId, aOut, NULL,
				0, apSession->mDsplyBfr, cMt, AISMGROUT_CLOSESESSION);
			QCoreApplication::postEvent(cpAisMgr, apEv);
		}
		else
		{
			// Request. Create a request structure for this request.
			apSession->mpRequest = new ARequest(iSessionId, aReqId, SBGLUE_MSGBLOCK/*EvalType*/, iConnectId/*NoDebug*/,
				"_ais|closeSession", NULL/*Data*/, QDateTime::currentDateTime());

			// Executing Session.  If current request is for this session is executing, mark session and continue.
			if (iCloseMode == geHard && iSessionId == apContext->mExeSessionID)
				SBGlue_SetEngineFlags(apContext->mContextIndex, SBGLUE_ESCAPE, 1L);

			aPendingRequests = isPending(iSessionId);

			// Pending Requests. If pending requests for this session, remove them from the list.
			if (iCloseMode >= geFirm && aPendingRequests)
				clearRequests(iSessionId);

			// If no session is executing and no pending requests for the session, clean up the session
			if (iCloseMode >= geSoft && !aPendingRequests && (apContext->mExeSessionID != iSessionId))
				cleanupSession(iSessionId);
		}
	}
	cSessionMgrMutex.unlock();
	return aReqId;
}

/*!
\brief connectSession - Connect to a disconnected session

\param iSessionId - Session ID of the session to be reconnected
\return aCode - 0 if no error, Error code (<0) if an error
*/
AErrorCode ASessionManager::connectSession(ASessionID iSessionId)
{
	AErrorCode aCode = AOK;
	ASession* apSession = 0;
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || (apSession = cSessions[iSessionId]) == NULL)
		aCode = ABAD_SESSIONID;
	else if (apSession->mMode >= geOpen)
		aCode = AERR_CONNECTED;			// Closing, or already connected
	else
		apSession->mMode = geOpen;		// Active and connected
	return aCode;
}

bool ASessionManager::event(QEvent* ipEv)
{
	// Fetch payload  returned in event
    if (ipEv->type() != (QEvent::User + AISMGROUT_EVENT)) return false;
	AisMgrOutputEvent* apEv = (AisMgrOutputEvent*)ipEv;
	long aFcnType = apEv->mFcnType;		// AISMGROUT_REQUESTHTTP, ...
	QString& arEnctype = apEv->mEnctype;		// Holds FileName
	QString& arAisOut = apEv->mAisOut;			// Holds Url
	QString& arDisplay = apEv->mDisplay;		// Holds Body
	long aMsecToWait = apEv->mRetValue;
	long aSessionId = apEv->mSessionID;

	// Check the sessionId
	cSessionMgrMutex.lock();
	if (aSessionId <= 0 || !cSessions.contains(aSessionId) || cSessions[aSessionId]->mpContext == NULL)
	{	cSessionMgrMutex.unlock();
		return true;
	}
	ASession* apSession = cSessions[aSessionId];
	AHttpClient* apHttpClient = apSession->mpHttpClient;
	cSessionMgrMutex.unlock();

	switch (aFcnType)
	{
	case AISMGROUT_REQUESTHTTP:
		if (apHttpClient != NULL)
			//						  Url,     Body,     FileName   MsecToWait
			apHttpClient->requestHttp(arAisOut,arDisplay,arEnctype,aMsecToWait);
		break;
	default:
		break;
	}
	return true;
}

/*-----------------------------------------------------------------------------------------------------------------------------
debug
It is called by SBGlue_DebugDialog through SM_Debug and handles the return of debugging information to the client. It is also
responsible for handling incoming debug commands and client requests that can be processed while the contexts thread is waiting
in this routine for debug commands. This takes some explaining so bear with me.

This routine is called by the engine context when a debug breakpoint is encountered. This routine then passes the request for a
debug command to the client by making the callback in sessionManager cbDebug.  Like all other SM callbacks, the call to cbDebug
should return quickly. The debug response from the client should arrive on the context request queue just like any other request.

Here is where things get interesting. Normally, the request queue is processed FIFO. The queue can contain requests from
multiple sessions so requests from different sessions are intermixed in the queue. When a request's execution causes a call
into debug the rules governing the order of execution of requests in the request queue change. In fact, the normal request queue
dispatch method processNextRequest is not involved. Instead, debug must implement the following rules for managing the request
queue:
1. Only requests from the session that submitted the request that spawned the debug can be processed. These session requests
	will be processed in FIFO order.

2. Requests that are not debug commands may be executed on the context's thread and then return to this function. This allows
	some engine activity to proceed while the debugger is in control of the context's thread.

NOTES:
1. A session request that throws a context into debug mode essentially blocks all other sessions from executing requests until
	the debugger is allowed to proceed. Other sessions are free to continue to submit requests and these will be placed on the
	request queue for later processing.

2. When non-debugger requests are processed, by debug, it is important to remember that callbacks into the Session Manager from
	those requests can be made. The callbacks call getRequestID() to find out what currently executing request they are
	returning results for. This means that care must be taken to manage the sessions[mSessionID].currentRequestID value in
	this routine.

3. TM! What happens when a non-debugger request causes another debug breakpoint to fire? This could get really interesting.

4. TM! The whole format and debug command scheme needs to be revisited. It is barely efficient enough when running in-process.
	It may be almost unusable when running remote. Suggestions for change include:
- smartbase engine change so that vars return settings have a state
- smartbase engine change so that vars are retuned with code (one roundtrip instead	of two)
- smartbase engine change so that vars and code can be returned as difs from first debug or Lambda change
- sessionmanager change to return debug information formatted in html or DEL delimited based on host preference
- sessionmanager change to apply diffed var info to create full code and var textor send it on to client based on host preference

5. TM! I'm implementing a bit of the suggested debug command changes in this function. Some of this work should be done down in
	the engine. Later, after we throw byLambda.dll away, we can reimplement as necessary.

Arguments:
iSessionId		The session making the debug callback
debugData		A rubout delimited string containing the debugging information
debugDataLen	Length of the debug data
cmdBuffer		A buffer into which the client's debugger command is placed for return
cmdBufferLen	The maximum length of the cmdBuffer. This is set by SB_DebugDialog.
				Trying to pass a longer command back to SB_DebugDialog is an error.
---------------------------------------------------------------------------------------------------------------------------- */
long ASessionManager::debug(long iSessionId, char *ipDebugData, long iDebugDataLen, char *ipCmdBuffer, long iCmdBufferLen)
{
	bool		aDebugCmdRcvd = false;
	ARequestID	aDebugReqID;
	long		aDebugReqNoDebug;
	long		aRequestID;
	ARequest*	apReq = NULL;
	ARequestType aReqType;
	QStringList aMsgBlock;
	long			aRetval;
	char*		apStr;
	AContextSt*	apContext;
	ARequestList* apRequests;
	QString		aVarState("v");	// State of the variables return. Start with full var trace

	// Debug output is tricky. The following logic is applied to reduce roundtrips to the host application by the client. Two
	// kinds of output arrive in the debugData buffer: code and info When code arrives we want to save the debug code and then
	// return to the engine with a "v" debug command to get the variables information. The engine will then callback into this
	// routine again and we can assemble the orginal code information with the variables information and then make a callback
	// into the host application with the combined code and variable information.  If the application is a remote IDE this
	// eliminates a round trip. Even if the application is in-process it saves some time. In addition, this method reduces the
	// complexity of the client talking to the host application.
	// debugData buffer layout.  debugData is a del separated buffer containing the following fields:
	// titleString\del
	// promptString\del
	// selectedLine\del
	// numCodeLines\del
	// numInfoLines\del
	// codelines..	zero or more del separated lines
	// infolines..	zero or more del separated lines

	// Step One - determine the type of return data in the debugData buffer It could be simpler, however the engine interface
	// was developed for a  commmand line environment. So we have to read it like we were a  human and determine what got
	// returned. The fourth DEL-separated line contains the number of code lines.
	long retType = 0; // default to code being returned
	long i=0;	// char position in char buffer
	long lc = 0; // line count
	while (i < iDebugDataLen && lc < 3)
	{	if (ipDebugData[i++] == '\177') lc++; // the del ('\177') separates lines
	}
	if (lc == 3)
	{	// Found beginning of 4th line. Info returned
		if (ipDebugData[i] == '0' && ipDebugData[i+1] == '\177') retType = 1;
	}
	else
	{ //Engine Error! too few lines returned
		//TM! what to do here?
		throw; //puke
	}
	// Step Two - if code was not returned then we just continue on through the routine otherwise we have to save the debug
	// information and go and get variables info to package with our return to the host application.
	if (retType == 0)
	{	// save code data and return from this routine to get vars info
		cSessionMgrMutex.lock();
		apContext = cSessions[iSessionId]->mpContext;
		// Add one for null terminator
		apContext->mpCodeData = (char *)malloc(iDebugDataLen + 1);
		memmove(apContext->mpCodeData, ipDebugData, iDebugDataLen + 1);
		apContext->mCodeDataLen = iDebugDataLen + 1;
		// Set the cmdbuffer to the v command
		ipCmdBuffer[0] = 'v';
		ipCmdBuffer[1] = 0;
		cSessionMgrMutex.unlock();
		return 0; // return to engine so it can process the "v" command
	}
	/*else
	{	 cSessionMgrMutex.lock();
		apContext = cSessions[iSessionId]->mpContext;
		if (apContext != NULL)
			apContext->mpCodeData = NULL;
		cSessionMgrMutex.unlock();
	} */
	// Step Three - determine if we need to package code and info or just info for delivery to the host application.
	cSessionMgrMutex.lock();
	apContext = cSessions[iSessionId]->mpContext;
	aDebugReqID = cSessions[iSessionId]->mCurrentRequestID; // save request id
	aDebugReqNoDebug = cSessions[iSessionId]->mNoDebug;		// save engine execution nodebug flag
	if (apContext->mpCodeData != NULL)
	{	// We have code to package with info so copy it to local
		char * apCodeData = (char *)malloc(apContext->mCodeDataLen);
		if (apCodeData == NULL)
		{	cSessionMgrMutex.unlock();
			throw; //TM! need better error handler here
		}
		memmove(apCodeData, apContext->mpCodeData, apContext->mCodeDataLen);
		free(apContext->mpCodeData);
		apContext->mpCodeData = NULL;
		long aCodeDataLen = apContext->mCodeDataLen;
		apContext->mCodeDataLen = 0;
		cSessionMgrMutex.unlock();

		// Build a new string that combines codeData and the new info content in debugData.Find the header lines in the codeData.
		long aCodeDataDelPos[5]; // postions of the first four del separators in codeData (ie 5 lines)
		long lc = 0; // linecount
		long c;
		for (c = 0; c < aCodeDataLen && lc < 5; ++c)
		{	if (apCodeData[c] == '\177') aCodeDataDelPos[lc++] = c;
		}
		if (lc < 5)
			throw; // Engine error! too few lines returned

		// Find the header lines in the debugData (info)
		long aInfoDataDelPos[5];	// positions of the first four lines in debugData buffer (info)
		lc = 0;
		c = 0;
		for (c=0; c < iDebugDataLen && lc < 5; ++c)
		{	if (ipDebugData[c] == '\177') aInfoDataDelPos[lc++] = c;
		}
		if (lc < 5)
			throw; // Engine error! too few lines returned

		// We can now efficiently construct the combined string. Use title, prompt, selected line and codeLen fields from
		// codeData. Use infoLen field from debugData
		long aCodeBuf_HeaderLen = aCodeDataDelPos[3]; // does not include trailing del separator
		long aCodeBuf_DataStart = aCodeDataDelPos[4]; // includes leading del separator
		long aCodeBuf_DataLen   = aCodeDataLen - aCodeDataDelPos[4] - 1; // inlcudes leading del, does not include null terminator
		long aInfoBuf_NumInfoLinesStart = aInfoDataDelPos[3]; // includes leading del separator
		long aInfoBuf_NumInfoLinesLen = aInfoDataDelPos[4] - aInfoBuf_NumInfoLinesStart; // inlcudes leading del separator
		long aInfoBuf_DataStart = aInfoDataDelPos[4]; // includes leading del separator
		long aInfoBuf_DataLen   = iDebugDataLen - aInfoDataDelPos[4]; // includes leading del, does not include null terminator
		// Note: debugDataLen, returned from the engine, does not include the buffer position holding the null terminator. It is
		// actually the length of the string less the null.
		long aCodeAndInfoLen =  aCodeBuf_HeaderLen			// Header fields from codeData
							+ aInfoBuf_NumInfoLinesLen	// The field containing the number of info lines
							+ aCodeBuf_DataLen			// The code lines
							+ aInfoBuf_DataLen			// The info lines
							+ 1;						// The null terminator
		char* apCodeAndInfo = (char *)malloc(aCodeAndInfoLen);
		long aPos = 0;
		memmove(&apCodeAndInfo[aPos],apCodeData,aCodeBuf_HeaderLen); // Header fields from code data
		aPos += aCodeBuf_HeaderLen;
		memmove(&apCodeAndInfo[aPos],&ipDebugData[aInfoBuf_NumInfoLinesStart],aInfoBuf_NumInfoLinesLen); // num info lines field
		aPos += aInfoBuf_NumInfoLinesLen;
		memmove(&apCodeAndInfo[aPos],&apCodeData[aCodeBuf_DataStart],aCodeBuf_DataLen); // code lines
		aPos += aCodeBuf_DataLen;
		memmove(&apCodeAndInfo[aPos],&ipDebugData[aInfoBuf_DataStart],aInfoBuf_DataLen); // info lines
		aPos += aInfoBuf_DataLen;
		apCodeAndInfo[aPos] = '\0';

		// Make a callback to send the debug data to the client session
		// Note that the call to pDebug will return quickly.
		// maybe use QStrings in the above code??
		QString aCodeInfo(apCodeAndInfo);
		cbDebug(iSessionId,aDebugReqID, aCodeInfo);
		free(apCodeAndInfo);
		apCodeAndInfo = NULL;
		aCodeAndInfoLen = 0;
	}	// end if
	else
	{	cSessionMgrMutex.unlock();
		// Make a callback to send the debug data to the client session
		// Note that the call to pDebug will return quickly.
		// maybe use QStrings??
		QString aDebugData(ipDebugData);
		cbDebug(iSessionId, aDebugReqID, aDebugData);
	}
	// Now loop through the request queue looking for requests for the session that is in debug mode. All other session requests
	// are ignored. This process is very similar to processNextRequest. processNextRequest is of course blocked while the
	// context thread is in this method until debug ultimately returns with a debug command.
	while (!aDebugCmdRcvd)
	{	cSessionMgrMutex.lock();
		apContext = cSessions[iSessionId]->mpContext;
		apRequests = &apContext->mRequests;

		// Iterate through the request queue looking for a new session request. While in debug we ignore requests from other
		// sessions on the context.
		for (long aReq = 0; aReq < apRequests->size(); ++aReq)
		{	apReq = apRequests->at(aReq);
			if (apReq->mSessionID == iSessionId && !apReq->mExecuting)
				break;			// Request match found
			else
				apReq = NULL;
		}
		if (apReq != NULL)
		{	apReq->mExecuting = true;
			aRequestID = apReq->mRequestID;
			aReqType = apReq->mEvalType;
			apStr = apReq->mCmdString.data();

			// Determine type of command in request
			switch (aReqType)
			{
			case SBGLUE_DEBUGCMD:
				aDebugCmdRcvd = true;
				//buffer should contain the debug command
                // TODO: What if buffer is not enough?
				strncpy(ipCmdBuffer, &apStr[0], iCmdBufferLen - 1);
                ipCmdBuffer[iCmdBufferLen - 1] = '\0';
				break;
			case SBGLUE_FILEOPENRESPONSE:
				/*TM! an error. Report or Log? */
				break;
			default:
				// Process a non-debug command request.
				//TM! what happens if the debug happened in a slave context? Is the eval below a good idea? Should it go to the
				// master or slave context?
				cSessions[iSessionId]->mCurrentRequestID = aRequestID;
				cSessions[iSessionId]->mNoDebug = 1;	// Causes gTP->DebugSuspended to be set true.
														// Debug and Instruction trace will be ignored.
				long aUserID = cSessions[iSessionId]->mUsrID;
				long aSecLvl = cSessions[iSessionId]->mSecurityLevel;
				apContext->mExeSessionID = iSessionId;		// Note executing session ID
				cSessionMgrMutex.unlock();
				aRetval = SBGlue_Eval(apContext->mpCP,iSessionId,aRequestID,aUserID,aSecLvl,aReqType,1,0,apContext->mJitMode,apStr,NULL);
				cSessionMgrMutex.lock();
				cSessions[iSessionId]->mCurrentRequestID = aDebugReqID; // restore saved request id
				cSessions[iSessionId]->mNoDebug = aDebugReqNoDebug;		// restore saved NoDebug flag
				break;
			}
			// Remove processed request from queue
			apContext = cSessions[iSessionId]->mpContext;
			apRequests = &apContext->mRequests;
			for (long aReq = 0; aReq < apRequests->size();)
			{	apReq = apRequests->at(aReq);
				if (apReq->mSessionID == iSessionId && apReq->mRequestID == aRequestID)
				{	apRequests->removeAt(aReq);
					delete apReq;
				}
				else
					++aReq;
			}
		}	// end if
		cSessionMgrMutex.unlock();
		if (!aDebugCmdRcvd)
			apContext->mpThread->localSleep(1L/*Msec*/);
	}	// end while
	return 0;
}

/*!
\brief enableConsoleLog - Enable the log that captures all the console output to a session

\param iSessionId - Session ID of the session console output to be captured
\param iEnable -2 Reset Log, -1 Close Log, =0 Suspend Log, 1 Enable Log, 2 Format Log
\return aCode - 0 if no error, Error code (<0) if an error
 */
AErrorCode ASessionManager::enableConsoleLog(ASessionID iSessionId, long iEnable)
{
	ASession* apSession;
	ARingFile* apRingFile;
	long aCode = AOK;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || (apSession = cSessions[iSessionId]) == NULL || apSession->mpContext == NULL)
		aCode = ABAD_SESSIONID;
	else if ((apRingFile = apSession->mpDisconnectedBuffer) == NULL)
		aCode = -AERR_NOCONSOLELOG;			// Nothing to enable
	else if (iEnable >= 2)					// Flush the buffer to a file.
		apRingFile->flush();
	else
	{	// Call back first to allow client one last shot at the output before shutting down the log.
		cSessionMgrMutex.unlock();
		cpAisSvr->enableConsoleLog(iSessionId, iEnable);
		cSessionMgrMutex.lock();
		if (iEnable < 0)
		{	delete apRingFile;
			apSession->mpDisconnectedBuffer = NULL;
		}
	}
	cSessionMgrMutex.unlock();
	return aCode;
}

/*!
\brief errorTrace - Enable error trace to stop execution if an error occurs.

\param iSessionId - Session ID of the session to be traced
\param iOnoff - Iff true, turn the error trace on.
\return aCode - 0 if no error, Error code (<0) if an error
 */
AErrorCode ASessionManager::errorTrace(ASessionID iSessionId, bool iOnoff)
{
	long ContextIndex;
	long aCode = AOK;
	ASession* apSession;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || (apSession = cSessions[iSessionId]) == NULL || apSession->mpContext == NULL)
		aCode = ABAD_SESSIONID;
	else
	{	ContextIndex = apSession->mpContext->mContextIndex;
		if (apSession->mpContext->mExeSessionID == iSessionId)
				SBGlue_SetEngineFlags(ContextIndex,SBGLUE_ERROR_TRACE,iOnoff?1L:0);
	}
	cSessionMgrMutex.unlock();
	return aCode;
}

/* ASessionManager::flags is called by the SM_Flags function on
the context's thread. mEngineFlags contains the session's requested
engine state.*/
unsigned ASessionManager::flags(long iSessionId, long currentState)
{
	cSessionMgrMutex.lock();
	ASession* apSession = cSessions[iSessionId];
	if (apSession->mNoDebug) { // Ignore session's requested debuging states
		cSessionMgrMutex.unlock();
		return currentState;
	}

	unsigned aFlags = apSession->mEngineFlags;
	apSession->mEngineFlags |= SBGLUE_ESCAPE;
	apSession->mEngineFlags &= ~SBGLUE_ESCAPE; // Clear the local escape flag - it remains in aFlags
	cSessionMgrMutex.unlock();
	return aFlags;
}

/*!
\brief flushSessions - Send the buffered console output (writelns, display) to the clients

\param iNow - if true force error trace even if the output has not been waiting for a full period.
\return aCode - 0
\par Notes:
-# AisSvr timer calls this routine periodically to write out stale display buffers
 */
long ASessionManager::flushSessions(bool iNow)
{
	//long aSessionId;
	cSessionMgrMutex.lock();

	ASessionTbl::iterator aIter = cSessions.begin();
	while (aIter != cSessions.end())
	{
		if (aIter.key() != 0)
		{
			ASession* apSession = aIter.value();
			if (apSession->mpContext != NULL && !apSession->mDsplyBfr.isEmpty())
			{	if (iNow || apSession->mStale)
				{	apSession->mStale = false;
					QString& arBfr = apSession->mDsplyBfr;
					while (arBfr.length() >= ASMGR_MAXBFRSIZE)
					{	cbDisplay(aIter.key(), arBfr.left(ASMGR_MAXBFRSIZE));
						arBfr = arBfr.mid(ASMGR_MAXBFRSIZE);
					}
					if (!arBfr.isEmpty())
					{	cbDisplay(aIter.key(), arBfr);
						arBfr.truncate(0);
					}
				}
				else
					apSession->mStale = true;
			}
		}
		aIter++;
	}

	cSessionMgrMutex.unlock();
	return 0;
}
/*!
\brief getConsoleLog - Retrieve currently buffered output, if any.

\param iSessionId - Retrieve the buffered output for this session
\param iClear - If true, clear the buffer
\param orOut - Place to put the console output.
\return aCode - 0 if no error, Error code (<0) if an error
\par Notes:
-# AMP: _ais|getconsolelog|clear|%s|sessionid|wait|%d
*/
AErrorCode ASessionManager::getConsoleLog(long iSessionId, bool iClear, QString& orOut)
{
	// Extract contextId from the context
	long aCode = AOK;
	QByteArray aOut;
	ASession* apSession;
	ARingFile* apRingFile;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) ||
		(apSession = cSessions[iSessionId]) == NULL || apSession->mpContext == NULL ||
		(apRingFile = apSession->mpDisconnectedBuffer) == NULL)
		aCode = -AERR_NOCONSOLELOG;
	else if ((aCode = apRingFile->read(aOut, iClear)) >= 0)
		orOut = aOut;
	cSessionMgrMutex.unlock();
	return aCode;
}

/*!
\brief getContextId - Get index into cOpenContexts vector given the context name

\param irContextName - Name of the context
\return aCode - ContextId (>0) or error code (<0) if an error
*/
long ASessionManager::getContextId(const QString& irContextName)
{
	long aContextId;
	cSessionMgrMutex.lock();
	if (cContextMap.contains(irContextName))
	{	aContextId = cContextMap.value(irContextName);
		if (aContextId < 0 || aContextId >= cOpenContexts.size())
			aContextId = -AERR_CONTEXTID;
	}
	else
		aContextId = AUNKNOWN_CONTEXTNAME;
	cSessionMgrMutex.unlock();
	return aContextId;
}

/*!
\brief getContextId - Get index into cOpenContexts vector given the session ID

\param iSessionId - A session running in the context.
\return aCode - ContextId (>0) or error code (<0) if an error
*/
long ASessionManager::getContextId(long iSessionId)
{
	long aContextId;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId))
		aContextId = ABAD_SESSIONID;
	else
		aContextId = cSessions[iSessionId]->mContextID;
	cSessionMgrMutex.unlock();
	return aContextId;
}

/*!
\brief getContextName - Get the context name for a session.

\param iSessionId - Session ID for a session running in this context.
\param iContextId - Context ID for this context
\return aCtx Context name or empty string if Context not found
\par Notes:
-# If the session Id is 0, routine uses the context's ID to locate the Context name.
*/
QString ASessionManager::getContextName(long iSessionId, long iContextId)
{
	QString aCtx;
	cSessionMgrMutex.lock();
	if (iSessionId > 0 && cSessions.contains(iSessionId))
	{	ASession* apSession = cSessions[iSessionId];
		if (apSession->mpContext != NULL)
			aCtx = apSession->mpContext->mContextName;
	}
	else if (iContextId > 0)
		aCtx = cOpenContexts[iContextId]->mContextName;
	cSessionMgrMutex.unlock();
	return aCtx;
}

/*!
\brief getContextParam - Get the value of a context parameter

\param irContextName - Name of the context
\param irParameter - Name of the parameter
\return aValue - Parameter value or empty string if parameter not found
*/
QString ASessionManager::getContextParam(const QString& irContextName, const QString& irParameter)
{
	QString aValue;
	cSessionMgrMutex.lock();
	if (!irContextName.isEmpty() && cContextMap.contains(irContextName))
	{	long aContextId = cContextMap.value(irContextName);
		AStringMap& arParams = cOpenContexts[aContextId]->mContextParams;
		if (arParams.contains(irParameter))
			aValue = arParams[irParameter];
	}
	cSessionMgrMutex.unlock();
	return aValue;
}

/*!
\brief getContextParams - Get all of the context parameters for a context

\param irContextName - Name of the context
\param opContextId -> Place to put the context Id
\return apContextParams -> A string map of the context parameters
*/
AStringMap* ASessionManager::getContextParams(const QString& irContextName, long* opContextId)
{
	AStringMap* apContextParams = NULL;
	long aContextId = -1;

	cSessionMgrMutex.lock();
	if (cContextMap.contains(irContextName))
	{	aContextId = cContextMap.value(irContextName);
		apContextParams = &cOpenContexts[aContextId]->mContextParams;
	}
	cSessionMgrMutex.unlock();
	if (opContextId != NULL)
		*opContextId = aContextId;
	return apContextParams;
}

/*!
\brief getContextParams - Get all of the context parameters for a context

\param iContextId - Index into the cOpenContexts list
\return apContextParams -> A string map of the context parameters
*/
AStringMap* ASessionManager::getContextParams(long iContextId)
{
	AStringMap* apContextParams = NULL;

	cSessionMgrMutex.lock();
	if (iContextId >= 0 && iContextId < cOpenContexts.count())
		apContextParams = &cOpenContexts[iContextId]->mContextParams;
	cSessionMgrMutex.unlock();
	return apContextParams;
}

/*!
\brief getContextPtr - Get pointer to beginning of memory allocated for context

\param irContextName - Name of the context
\return aContextPtr (NULL if context is not found)
*/
void* ASessionManager::getContextPtr(const QString& irContextName)
{
	long aContextId;
	void* aContextPtr = NULL;
	cSessionMgrMutex.lock();
	if (cContextMap.contains(irContextName))
	{	aContextId = cContextMap.value(irContextName);
		if (aContextId < 0 || aContextId >= cOpenContexts.size())
			aContextId = -AERR_CONTEXTID;
	}
	else
		aContextId = AUNKNOWN_CONTEXTNAME;

	if (aContextId != AUNKNOWN_CONTEXTNAME)
	{	AContextSt* apContext = cOpenContexts[aContextId];
		aContextPtr = apContext->mpCP;
	}
	cSessionMgrMutex.unlock();
	return aContextPtr;
}

/*!
\brief getCurrentContexts - Get a list of contexts that have been registered (0) or opened (>0).

\return  newline-delimited list of context names.
*/
QString ASessionManager::getCurrentContexts()
{
	long aAlive, i;
	QString aEntry;
	QStringList aList;
	cSessionMgrMutex.lock();
    for (i = 0; i < cOpenContexts.size(); ++i)
	{	AContextSt* apContext = cOpenContexts[i];
		if ((aAlive = apContext->mAlive) >= 0)		// (empty <0, registered =0, opened >0)
		{	aEntry = apContext->mContextName + (aAlive > 0 ? "" : "\tReg");
			aList += aEntry;
		}
	}
	cSessionMgrMutex.unlock();
	return aList.join("\n");
}

/*!
\brief getExeSession - Get the session ID of the currently executing session on this context

\param iSessionId - Session ID for a session running in this context.
\param irContextName - Context name
\return aSessionId - Session ID of currently executing session or 0 if none.
\par Notes:
-# If the context name is empty, look up the context using the iSessionId
*/
long ASessionManager::getExeSession(long iSessionId, const QString& irContextName)
{
	// If no context name is provided, use the session's context.
	long aContextId = 0, aSessionId;
	cSessionMgrMutex.lock();
	if (irContextName.isEmpty() && iSessionId > 0 && cSessions.contains(iSessionId))
	{	ASession* apSession = cSessions[iSessionId];
		aContextId = apSession->mContextID;
	}
	else if (cContextMap.contains(irContextName))
		aContextId = cContextMap.value(irContextName);
	else
		aSessionId = AUNKNOWN_CONTEXTNAME;

	// Look up the ExeSession in cOpenContexts using the context ID
	aSessionId = (aContextId > 0) ? cOpenContexts[aContextId]->mExeSessionID : -AERR_CONTEXTID;
	cSessionMgrMutex.unlock();
	return aSessionId;
}

/*!
\brief getMainThread - get the main thread for use by ASBGlue_ContextClient.

\return apMainThread -> main thread
\par Notes:
-# The ContextClient is moved from the context thread to the main thread where event processing is active.
-# Returns NULL if invalid iContextId.
*/
QThread* ASessionManager::getMainThread()
{
	return cpMainThread;
}


/*!
\brief getRequestID - get a unique requestID without making a request

\return aRequestID - a unique RequestID
\par Notes:
-#  This allows the host to maintain a unique index on its brower response queue using requestIDs.
-# ASessionManager does not keep track of the requestID's returned from this call.
*/
ARequestID	ASessionManager::getRequestID()
{
	cSessionMgrMutex.lock();
	ARequestID aRequestID = ++cNextRequestID;
	cSessionMgrMutex.unlock();
	return aRequestID;
}

/*!
\brief getSessionUser - Get the user ID associated with a specified session.

\param iSessionId - The session ID of the user's session.
\return aRetVal - The user ID (>0) or an error code (<0) if an error.
*/
long ASessionManager::getSessionUser(ASessionID iSessionId)
{
	long aRetval;
	cSessionMgrMutex.lock();
	if (iSessionId > 0 && cSessions.contains(iSessionId) && cSessions[iSessionId]->mpContext != NULL)
		aRetval = cSessions[iSessionId]->mUsrID;
	else
		aRetval = ASESSION_NOT_ACTIVE;
	cSessionMgrMutex.unlock();
	return aRetval;
}

/*!
\brief getStartupScriptName - Get the name of the startup script for the specified context

\param irContextName - The name of the context holding the startup script
\return aScriptName - The startup script path + name or empty if the context could not be found.
*/
QByteArray ASessionManager::getStartupScriptName(const QString& irContextName)
{
	QByteArray aScriptName;
	cSessionMgrMutex.lock();
	if (cContextMap.contains(irContextName))
	{	long aContextId = cContextMap.value(irContextName);
		aScriptName = cOpenContexts[aContextId]->mStartupScriptName.toLatin1();
	}
	cSessionMgrMutex.unlock();
	return aScriptName;
}

long ASessionManager::getRequestStats(QString& orRequestStats)
{
	QMap<long,long> aStats;

	orRequestStats.clear();

	cSessionMgrMutex.lock();
	foreach(AContextSt* apContext, cOpenContexts)
	{
		if (apContext->mAlive > 0)
		{
			foreach(ARequest* apRequest, apContext->mRequests)
			{
				if (!aStats.contains(apRequest->mSessionID))
					aStats.insert(apRequest->mSessionID, 1);
				else
					aStats[apRequest->mSessionID]++;
			}
		}
	}

	foreach(ASessionID aSessionId, cSessions.keys())
	{
		if (aSessionId > 0)
		{
			orRequestStats.append(QString::number(aSessionId));
			orRequestStats.append("\t");

			if (aStats.contains(aSessionId))
				orRequestStats.append(QString::number(aStats[aSessionId]));
			else
				orRequestStats.append("0");

			orRequestStats.append("\n");
		}
	}

	cSessionMgrMutex.unlock();

	return 0;
}

/*!
\brief getSessions - Gets list of current sessions.

\param irInContextName - The name of the parent context
\param orSessionList - Place to put the session IDs
\return aSize - The number of sessions in the list
\par Notes:
-# If irInContextName is empty, get all sessions.
-# Session record:  ContextName\\tSessionId\\tAdmin\\tUsrName\\tCloseMode
*/
long ASessionManager::getSessions(const QString& irInContextName, QStringList& orSessionList)
{
	QString aRecord;
	ASession* apSession;
	const char * apAdmin;
	cSessionMgrMutex.lock();
	long aId = 0;
	long aInContextId = (irInContextName.isEmpty()) ? -1 : cContextMap.value(irInContextName);

	// Return record for all open sessions for this context
	long aAdminId, aContextId;
	AContextSt* apContext;

	QList<ASessionID> aSessIdLst = cSessions.keys();
	QList<ASessionID>::iterator aSessIdIter = aSessIdLst.begin();
	while (aSessIdIter != aSessIdLst.end())
	{
		aId = *aSessIdIter;
		if (aId != 0)
		{
			apSession = cSessions[aId];
			if (apSession != NULL && apSession->mpContext != NULL)
			{
				aContextId = apSession->mContextID;
				apContext = cOpenContexts[aContextId];
				if (apContext != NULL && (aInContextId == -1 || aContextId == aInContextId))
				{	aAdminId = apContext->mAdminSessionId;
					apAdmin = (aId == aAdminId) ? "admin" : "        ";
					QString& arContextName = apContext->mContextName;
					aRecord.sprintf("%s\t%ld\t%s\t%s\t%s",arContextName.toLatin1().data(),aId,apAdmin,apSession->mUsrName.toLatin1().data(),
						gAis.mpCloseMode[apSession->mMode]);
					orSessionList += aRecord;
				}
			}
		}
		aSessIdIter++;
	}
	cSessionMgrMutex.unlock();
	return orSessionList.size();
}

long ASessionManager::getSessionStats(QString& orSessionStats)
{
	QMap<QString,long> aStats;

	orSessionStats.clear();
	cSessionMgrMutex.lock();
	foreach(ASession* apSession, cSessions)
	{
		if (!apSession->mUsrName.isEmpty())
		{
			if (!aStats.contains(apSession->mUsrName))
				aStats.insert(apSession->mUsrName, 1);
			else
				aStats[apSession->mUsrName]++;
		}
	}
	cSessionMgrMutex.unlock();

	foreach(QString aUsername, aStats.keys())
	{
		orSessionStats.append(aUsername);
		orSessionStats.append("\t");
		orSessionStats.append(QString::number(aStats[aUsername]));
		orSessionStats.append("\n");
	}

	return 0;
}

/*!
\brief getSystemSessionId - Get the session ID for the System Context

\param iContextId - The ID of the context
\return aSystemSessionId - The ID of the System Session
\par Notes:
-# Every context has one system session used for housekeeping chores.
-# The System Session ID of the default context is 1
*/
long ASessionManager::getSystemSessionId(long iContextId)
{
	long systemSessionId = -1;
	cSessionMgrMutex.lock();
	// Find context using iterator
	long N = cOpenContexts.count();
	for(long n=1; n < N; n++) // Skip over index 0 as it never contains a valid entry
		if(cOpenContexts[n]->mContextIndex == iContextId)
		{	systemSessionId = cOpenContexts[n]->mAdminSessionId;
			break;
		}
	cSessionMgrMutex.unlock();
	return systemSessionId;
}

/*!
\brief getUsrSettings - Fetch context-specific updates, if any, to this users' settings

\param irContextName - The context name for this context
\param iUsrId - The user's ID
\param orSecLvl - Place to return the security level
\param orEndDay - Place to return the expiration date of this user's logon
\return aOk - True if user is found.
\par Notes:
-# If User has entry, update settings using this entry (a setting of -1 implies no change).
-# Else, if a Default entry, update settings using this entry (again, -1 implies no change).
-# Else, return -1 (no change) for each setting.
 */
bool ASessionManager::getUsrSettings(const QString& irContextName, long iUsrId, long& orSecLvl, long& orEndDay)
{
	// Look up UsrMap
	bool aOk = false;
	long aContextId, aEndDay = -1, aSecLvl = -1;
	cSessionMgrMutex.lock();
	if (cContextMap.contains(irContextName))
	{	aContextId = cContextMap.value(irContextName);
		if (aContextId >= 0 && aContextId < cOpenContexts.size())
		{	AUsrMap& arUsrMap = cOpenContexts[aContextId]->mUsrMap;
			aOk = true;
			if (arUsrMap.contains(iUsrId))
			{	aEndDay = arUsrMap.value(iUsrId).mEndDay;
				aSecLvl = arUsrMap.value(iUsrId).mSecLvl;
			}
			else if (arUsrMap.contains(DEFAULTUSRID))
			{	aEndDay = arUsrMap[DEFAULTUSRID].mEndDay;
				aSecLvl = arUsrMap[DEFAULTUSRID].mSecLvl;
			}
		}
	}
	cSessionMgrMutex.unlock();
	orSecLvl = aSecLvl;
	orEndDay = aEndDay;
	return aOk;
}

/*!
\brief instructionTrace - Enable instruction trace to stop execution at beginning of this Lambda.

\param iSessionId - Session ID of the session to be traced
\param iOnoff - Turn instruction trace on/off
\return aCode - 0 if no error, Error code (<0) if an error
 */
long ASessionManager::instructionTrace(long iSessionId, bool iOnoff)
{
	long ContextIndex;
	long aCode = AOK;
	ASession* apSession = NULL;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || (apSession = cSessions[iSessionId]) == NULL ||
		apSession->mpContext == NULL)
		aCode = ABAD_SESSIONID;
	else
	{	ContextIndex = apSession->mpContext->mContextIndex;
		if (apSession->mpContext->mExeSessionID == iSessionId)
			SBGlue_SetEngineFlags(ContextIndex,SBGLUE_INSTRUCTION_TRACE,iOnoff?1L:0);
	}
	cSessionMgrMutex.unlock();
	return aCode;
}

/*!
\brief isAdminSession - Return ture if session Id is an admin session

\param iSessionId - Session ID of the session to be checked
\return aIsAdminSession - true iff the session is an admin session
 */
bool ASessionManager::isAdminSession(ASessionID iSessionId)
{
	bool aIsAdminSession = false;
	ASession* apSession;
	AContextSt* apContext;
	cSessionMgrMutex.lock();
	if (iSessionId > 0 && cSessions.contains(iSessionId) && (apSession = cSessions[iSessionId]) != NULL &&
		(apContext = apSession->mpContext) != NULL)
	{	if (iSessionId == apContext->mAdminSessionId)
			aIsAdminSession = true;
	}
	cSessionMgrMutex.unlock();
	return aIsAdminSession;
}

/*!
\brief isContextAlive checks to see if a specified context is accepting new requests

\param ipContext -> Context structure for the specified context
\return aRetVal - true iff the context is alive.
 */
bool ASessionManager::isContextAlive(AContextSt* ipContext)
{
	bool aRetval;
	cSessionMgrMutex.lock();
	aRetval = (ipContext->mAlive > 0);	// (empty <0, registered =0, opened >0)
	cSessionMgrMutex.unlock();
	return aRetval;
}

/*!
\brief isContextBusy returns 0 or executing sessionId if context is busy.

\param iSessionId - Session ID executing in this context
\param irContextName - Name of the context
\return aExeSessionId - 0 if idle, else the session ID of the executing session.
\par Notes:
-# If irContextName is empty, isContextBusy uses iSessionId to look up the context.
 */
long ASessionManager::isContextBusy(long iSessionId, const QString& irContextName)
{
	long aExeSessionId = ABAD_SESSIONID;
	// If no context name, look in cSessions
	if (irContextName.isEmpty())
	{	cSessionMgrMutex.lock();
		if (iSessionId > 0 && cSessions.contains(iSessionId))
		{	AContextSt* apContext = cSessions[iSessionId]->mpContext;
			if (apContext == NULL)
				aExeSessionId = AUNKNOWN_CONTEXTNAME;
			else
				aExeSessionId = apContext->mExeSessionID;
			cSessionMgrMutex.unlock();
		}
		else
			cSessionMgrMutex.unlock();
	}
	else	// Look up index into the Open Contexts array
	{	cSessionMgrMutex.lock();
		if (cContextMap.contains(irContextName))
		{	long aContextId = cContextMap.value(irContextName);
			if (aContextId >= 0 && aContextId < cOpenContexts.size())
				aExeSessionId = cOpenContexts[aContextId]->mExeSessionID;
			else
				aExeSessionId = -AERR_CONTEXTID;
		}
		else
			aExeSessionId = AUNKNOWN_CONTEXTNAME;
		cSessionMgrMutex.unlock();
	}
	return aExeSessionId;
}

/*!
\brief isContextOpen checks if a context is open.

\param irContextName - Name of the context
\return aRet - true iff the context is open
 */
bool ASessionManager::isContextOpen(const QString& irContextName)
{
	bool aRet;
	cSessionMgrMutex.lock();
	aRet = cContextMap.contains(irContextName);
	cSessionMgrMutex.unlock();
	return aRet;
}

/*!
\brief isContextOpen checks if a context is open.

\param iContextId - Index into the cOpenContexts list
\return aRet - true iff the context is open
 */
bool ASessionManager::isContextOpen(long iContextId)
{
	bool aRet = false;
	cSessionMgrMutex.lock();
	if (iContextId > 0 && iContextId < cOpenContexts.size())
		aRet = cOpenContexts[iContextId]->mAlive > 0;
	cSessionMgrMutex.unlock();
	return aRet;
}

/*!
\brief isDisconnected - Checks to see if a session is disconnected

\param iSessionId - ID of the session to be checked.
\return aIsDisconnected - true iff the session is disconnected
 */
bool ASessionManager::isDisconnected(ASessionID iSessionId)
{
	bool aIsDisconnected = false;
	ASession* apSession;
	if (iSessionId > 0 && cSessions.contains(iSessionId) && (apSession = cSessions[iSessionId]) != NULL &&
		apSession->mpContext != NULL)
		aIsDisconnected = (apSession->mMode < geOpen);
	return aIsDisconnected;
}

/*!
 * \brief Returns true if the Embedded MySQL feature is enabled.
 */
bool ASessionManager::isEmbeddedMySQLEnabled()
{
	return cEmbeddedMySQLEnabled;
}

/*!
\brief isPending - Return true iff one or more requests pending for this session.

\param iSessionId - ID of the session to be checked.
\return aPending - true iff a request is pending
 */
bool ASessionManager::isPending(long iSessionId)
{
	bool aPending = false;
	ASession* apSession = cSessions[iSessionId];
	ARequestList& arRequests = apSession->mpContext->mRequests;
	ARequest* apReq;
	for (long aReq = 0; aReq < arRequests.size(); ++aReq)
	{	apReq = arRequests.at(aReq);
		if (apReq->mSessionID == iSessionId)
		{	aPending = true;
			break;
		}
	}
	return aPending;
}

/*!
\brief jit - Turn on/off the just-in-time compiler setting

\param iSessionId - ID of a session.
\param iOnoff - Iff true, turn the jit on.
\return aCode - 0 if no error, else error code (<0)
 */
long ASessionManager::jit(ASessionID iSessionId, bool iOnoff)
{
	long aCode = AOK;
	ASession* apSession = NULL;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || (apSession = cSessions[iSessionId]) == NULL ||
		apSession->mpContext == NULL)
		aCode = ABAD_SESSIONID;
	else
	{	long aContextIndex = apSession->mpContext->mContextIndex;
		if (apSession->mpContext->mExeSessionID == iSessionId)
			SBGlue_SetEngineFlags(aContextIndex, SBGLUE_JITON, iOnoff? 1L : 0);
	}
	cSessionMgrMutex.unlock();
	return aCode;
}

/*!
\brief notifyHtmlPage - Return an HTML page to a waiting session

\param iSessionId - ID of a session waiting
\param irPageBfr - The HTML page contents
\return aCode - 0 if no error, else error code (<0)
 */
long ASessionManager::notifyHtmlPage(long iSessionId, const QByteArray& irPageBfr)
{
	long aCode = AOK;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || cSessions[iSessionId]->mpContext == NULL)
		aCode = ABAD_SESSIONID;
	else
	{	ASession* apSession = cSessions[iSessionId];
		// Copy results from returned buffer into mPageContent
		apSession->mPageContent = irPageBfr;
		apSession->mReadHtmlOk = true;
	}
	cSessionMgrMutex.unlock();
	return aCode;
}

/*!
\brief notifyRequestHttp - Return an HTML page to a waiting session

\param iSessionId - ID of a session waiting
\param irPageBfr - The HTML page contents
\return aCode - 0 if no error, else error code (<0)
 */
long ASessionManager::notifyRequestHttp(long iSessionId, QByteArray& irPageBfr)
{
	long aCode = AOK;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || cSessions[iSessionId]->mpContext == NULL)
		aCode = ABAD_SESSIONID;
	else
	{	ASession* apSession = cSessions[iSessionId];
		// Copy results from returned buffer into mPageContent
		apSession->mPageContent = irPageBfr;	// Deep copy
		apSession->mRequestHttpOk = true;
	}
	cSessionMgrMutex.unlock();
	return aCode;
}

/*!
\brief openConsoleLog - Start catching console output for the specified session.

\param iClear - The default setting of the clear flag
\param iRedirect - The default setting of the redirect flag
\param iSessionId - The Id of the session
\param iSize - Maximum log size
\param iStartAtNewline - Skip over a partial line at the top of a truncated log
\return aCode - 0 if no error, else error code (<0)
 */
AErrorCode ASessionManager::openConsoleLog(bool iClear, bool iRedirect, long iSessionId, long iSize, bool iStartAtNewline)
{
    Q_UNUSED(iStartAtNewline);

	ASession* apSession;
	ARingFile* apRingFile;
	long aCode = AOK;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || (apSession = cSessions[iSessionId]) == NULL || apSession->mpContext == NULL)
		aCode = ABAD_SESSIONID;
	else
	{	// If currently logging, remove existing log
		if ((apRingFile = apSession->mpDisconnectedBuffer) != NULL)
		{	delete apRingFile;
			apSession->mpDisconnectedBuffer = NULL;
		}
		// Start up a new buffer with iSize limit
		QString aFilePath = gAis.mGblParams["gblaislogpath"] + QString("session_%1.log").arg(iSessionId);
		if (iSize < 0) iSize = gAis.mGblParams["gblaismaxbuffersize"].toLong();
		apSession->mpDisconnectedBuffer = new ARingFile(iClear, false/*Persistent*/, true/*Record*/, iSize, aFilePath);
		cpAisSvr->openConsoleLog(iRedirect, iSessionId);
	}
	cSessionMgrMutex.unlock();
	return aCode;
}

/*!
\brief openContext - Open a context and then boot the context into operation.

\param irStartupPath - The path + file name of the startup script.
\param iorContextName - The context name if the context is not already registered
\param orMsgs - Place to put error and progress messages
\return aCode - 0 if no error, else error code (<0)
 */
long ASessionManager::openContext(const QString& irStartupPath, QString& iorContextName, QString& orMsgs)
{
	// Redo registration in case something has changed or if no previous registration.
	long aCode = 0, aSessionId;
	QString aMsgs(""), aErr;
	if (!irStartupPath.isEmpty())
		aCode = registerContext(irStartupPath, iorContextName, aMsgs, true/*isOpen*/);
	else if (iorContextName.isEmpty())
		aCode = AUNKNOWN_CONTEXTNAME;

	if (aCode >= 0)
	{	// Get a pointer to new or existing context entry
		cSessionMgrMutex.lock();
		if (!cContextMap.contains(iorContextName))
		{	cSessionMgrMutex.unlock();
			aCode = AUNKNOWN_CONTEXTNAME;
		}
		else
		{	long aAlive, aContextId = cContextMap.value(iorContextName);
			AContextSt* apContext = cOpenContexts[aContextId];
			// Can't open the system context.
			if (aContextId == 0)
			{	cSessionMgrMutex.unlock();
				aCode = -AERR_SYSTEMCONTEXT;
			}
			else if (aContextId < 0 || apContext == NULL)
			{	cSessionMgrMutex.unlock();
				aCode = -AERR_CONTEXTOPEN;
			}
			// Do not open context if it is already open
			else if ((aAlive = apContext->mAlive) != 0)	// (empty <0, registered =0, opened >0)
			{	cSessionMgrMutex.unlock();
				aCode = (aAlive > 0 ? -AERR_CONTEXTISOPEN : -AERR_NOTREGISTERED);
			}
			else
			{	// Launch a new protocol server for any new ports.
				ushort aPorts[4] = {0,0,0,0};
				AStringMap& arParams = apContext->mContextParams;
				AStringMap& arGblParams = gAis.mGblParams;
				for (long i = 1; i <= AISSVR_XML; ++i)
				{	const char *apPortName = gAis.mpPortNames[i];
					ushort aPort = arParams.contains(apPortName) ? arParams[apPortName].toUShort() : 0;
					if (aPort > 0 && (aCode = -cpAisSvr->openPort(i, aPort, iorContextName)) >= 0)
					{	aPorts[i] = aPort;
						aErr.sprintf("Local %s listening on port %d for context %s\n", gAis.mpSvrNames[i], aPort,
						iorContextName.toLatin1().data());
						aMsgs += aErr;
					}
					else // Set port number to global port if no context-specific port has been opened.
					{	QString aPortName("gbl");
						aPortName += apPortName;
						aPort = arGblParams.contains(aPortName) ? arGblParams[aPortName].toUShort() : 0;
						if (aPort > 0)
							aPorts[i] = aPort;
					}
				}
				// Compose script to set input args when executing an AMP request
				QByteArray aRunScript;
				aRunScript = "(setq _path {";
				aRunScript += arParams["WorkDir"];
				aRunScript += "}) (setq _ais #{appDir: {";
				aRunScript += arParams["appdir"];
				aRunScript += "} appPort: {";
				aRunScript += QByteArray::number(aPorts[AISSVR_APP]);
				aRunScript += "} contextName: {";
				aRunScript += iorContextName;
				aRunScript += "} clientViewUrl: {";
				aRunScript += arParams["clientviewurl"];
				aRunScript += "} httpDir: {";
				aRunScript += arParams["httpdir"];
				aRunScript += "} httpPort: {";
				aRunScript += QByteArray::number(aPorts[AISSVR_HTTP]);
				aRunScript += "} installPath: {";
				aRunScript += arGblParams["gblaisinstallpath"];
				aRunScript += "} applicationPath: {";
				aRunScript += arGblParams["gblaisapplicationpath"];
				aRunScript += "} machineName: {";
				aRunScript += arGblParams["gblaismachinename"];
				aRunScript += "} hostAddress: {";
				aRunScript += arGblParams["gblaishostaddress"];
				aRunScript += "} xmlDir: {";
				aRunScript += arParams["xmldir"];
				aRunScript += "} xmlPort: {";
				aRunScript += QByteArray::number(aPorts[AISSVR_XML]);
				aRunScript += "}})";

				// Start up this context.
				apContext = cOpenContexts[aContextId];
				apContext->mAlive = 1;		// (empty <0, registered =0, opened >0)
				apContext->mMemorySize = arParams["memory"].toLongLong() * 1048576;
				apContext->mObjHdrSize = arParams["memoryobjectheaders"].toLong() * 1048576;
				if (apContext->mObjHdrSize >= apContext->mMemorySize || apContext->mObjHdrSize <= apContext->mMemorySize / 50)
					apContext->mObjHdrSize = 0;		// Defaults to 20% of total allocated memory.
				apContext->mRunScript = aRunScript;

				// Initialize and load the context into memory
				long aStackSize = SBGlue_CalculateStackSpace(apContext->mMemorySize);
				AContextThread *apThread = new AContextThread(apContext, this, aStackSize);
				apContext->mpThread = apThread;

#if 0
				if (gAis.mpFirstBlock == NIL)
				{
					// This code DOES clear initial memory requested by ASessionManager::initMemoryMgr.
					// Note: Asking for initial memory and then freeing during openConext results
					//       in too little memory being available in 32bit execution mode. Please
					//       do not use this initialization algorithm without contacting Michael Korns.
					for(long i = 0; i < MAXCONTEXTALLOCATIONBLOCKS; ++i)
					{	if (cContextMemory[i] != NULL)
						{	free(cContextMemory[i]);
							cContextMemory[i] = NULL;
						}
					}
				}
#endif
				// The thread's run method calls SBGlue_OpenContext in the engine glue layer.
				cSessionMgrMutex.unlock();
				apThread->start();

				// Create the "system session" on the new context. The system session is used internally as a way for the AIS
				// server to send messages into the context. This system session is not available to AIS clients. Note that a
				// userID of 0 is used for this session.  No logon is performed.
				ACloseMode aMode = geDefault;	// Set session's default close mode to context-specific value
				aSessionId = openSession(aMode, iorContextName, USRMGR_SYSUSRID,USRMGR_SYSSECLVL, 0, geAdminSession);
				apContext->mAdminSessionId = aCode = aSessionId;
			}
		}
	}
	if (orMsgs != QString::null)
		orMsgs = aMsgs;
	return aCode;
}

/*!
\brief openSession Reconnect to iSessionId or open a new session on a context.

 Each context may have one or more open sessions.  An admin session is created when each context is created. Requests are
made by session. Incoming requests are queued in FIFO order and processed one at a time in the context. Sessions are specific
to a single user identified by a userID argument passed to the open Session method. An iUserID of 0 is reserved for the
system session. No remote client can have a iUserID of 0.
\param iDefMode - Default closing mode in case session is closed with no mode specified
\param irContextName - The parent context for this session
\param iUserID - The user opening this session
\param iSecurityLevel - The security level for this user on this context.
\param iSessionId - The session Id if reconnecting
\param iSessionType - Remote or local
\return aSessionId - SessionId (>0) if no error, else error code (<0)
 */
ASessionID ASessionManager::openSession(ACloseMode iDefMode, const QString& irContextName, long iUserID, long iSecurityLevel,
long iSessionId, ASessionType iSessionType)
{
	long aSessionId = AUNKNOWN_CONTEXTNAME;
	// Get ptr to the context for this session
	cSessionMgrMutex.lock();
	if (cContextMap.contains(irContextName))
	{	// Get a pointer to the existing context structure
		long aContextId = cContextMap.value(irContextName);
		AContextSt* apContext = cOpenContexts[aContextId];
		if (apContext == NULL || apContext->mAlive <= 0)	// (empty <0, registered =0, opened >0)
			aSessionId = ASESSION_NOT_ACTIVE;
		else
		{
			// Find an unused session slot
			if (iSessionId > 0 && cSessions.contains(iSessionId))
			{
				aSessionId = iSessionId;
			}
			else
			{
				for (aSessionId = 1; aSessionId < cSessions.size(); ++aSessionId)
				{
					// if session does not exist or is not associated with any context
					if (!cSessions.contains(aSessionId) || cSessions[aSessionId]->mpContext == NULL)
					{
						break;
					}
				}
			}

			// Add a new slot not yet present
			if (!cSessions.contains(aSessionId))
			{
				ASession* apSession = new ASession();
				apSession->mpHttpClient = NULL;
				cSessions.insert(aSessionId, apSession);
			}

			// Initialize session entry
			ASession* apSession = cSessions[aSessionId];
			apSession->mpContext = apContext;
			apSession->mContextID = aContextId;
			apSession->mCurrentRequestID = 0;
			apSession->mDefaultCloseMode = (iDefMode == geDefault) ? apContext->mDefaultCloseMode : iDefMode;
			apSession->mEngineFlags = 0;
			apSession->mpHttpClient = NULL;
			apSession->mMode = geOpen;			// Active and connected
			apSession->mNoDebug = 1;
			apSession->mReadHtmlOk = false;
			apSession->mpRequest = NULL;
			apSession->mRequestHttpOk = false;
			apSession->mpDisconnectedBuffer = NULL;
			apSession->mSecurityLevel = iSecurityLevel;
			apSession->mSessionType = iSessionType;
			apSession->mStale = false;
			apSession->mUsrID = iUserID;
			apSession->mUsrName = cpAisSvr->getUsrName(iUserID);
		}
	}
	cSessionMgrMutex.unlock();
	return aSessionId;
}

//	processNextRequest
//	This method dispatches requests off of the context's request queue into the engine. Note that requests may also be
//	dispatched by the debug member function. processNextRequest is executed ONLY on the context's thread.
void ASessionManager::processNextRequest(AContextSt* ipContext)
{
	ARequestID	aRequestID;
	cSessionMgrMutex.lock();
	ARequestList& arRequests = ipContext->mRequests;
	if (arRequests.count() > 0)
	{	// Process the next new request off of the context's request queue.  Note that it is possible for executing requests
		// to remain on the queue and we need to ignore these. These executing requests are there because we returned from
		// executing them in the engine with the suspendRequest function call. suspendRequest is part of the distributed
		// processing capabilities of the system. See the suspendRequest function and farm Lambda for more details.
		// ProcessNextRequest also needs to ignore requests that have a valid WhenToRun value unless that date/time has been
		// reached.
		ARequest* apReq;
		long aReq, aSize = arRequests.size();
		for (aReq = 0; aReq < aSize; ++aReq)
		{	apReq = arRequests.at(aReq);
			// Check to make sure we have a valid request.
			if (apReq->mRequestID <= 0 || apReq->mEvalType < 0 || apReq->mEvalType > 8 ||
			apReq->mSessionID <= 0 || apReq->mSessionID > 1000 || apReq->mCmdString.isEmpty() || !apReq->mWhenToRun.isValid())
			{	qDebug("ASessionManager::processNextRequest. Invalid request.");
				arRequests.clear();
				break;
			}
			else if (!apReq->mExecuting	&& (apReq->mWhenToRun.isNull() || apReq->mWhenToRun < QDateTime::currentDateTime()))
				goto PROCESS_REQUEST;
		}
		goto NO_MORE;

	PROCESS_REQUEST:
		apReq->mExecuting = true;
		aRequestID = apReq->mRequestID;
		long aRequestType = apReq->mEvalType;
		char* apStr = apReq->mCmdString.data();
		char* apData = apReq->mpData;
		long aSessionId = apReq->mSessionID;
		long aNoDebug = apReq->mNoDebug;

		switch(apReq->mEvalType)
		{
		case SBGLUE_DEBUGCMD: // These commands are handled in the member function debug
		case SBGLUE_FILEOPENRESPONSE:
			/*TM! an error. Log or error out?*/
			// Delete request and Log?
			arRequests.removeAt(aReq);
			delete apReq;
			break;
		default:
		{	ASession* apSession = cSessions[aSessionId];
			apSession->mCurrentRequestID = aRequestID;
			apSession->mNoDebug = aNoDebug;
			long aUsrID = apSession->mUsrID;
			long aSecLvl = apSession->mSecurityLevel;
			ipContext->mExeSessionID = aSessionId;		// Note executing session
			cSessionMgrMutex.unlock();
			// Pass apData to SBGlue_Eval who is responsible for deleting the buffer on completion.
			long aRetval = SBGlue_Eval(ipContext->mpCP, aSessionId, aRequestID, aUsrID, aSecLvl, aRequestType, aNoDebug, 1
			, ipContext->mJitMode, apStr, apData);
            Q_UNUSED(aRetval);
			apData = NULL;

			// Reset context information and remove request from queue (Unless a request was marked as remoteRequest)
			cSessionMgrMutex.lock();
			ipContext->mExeSessionID = 0;
			for (long aReq = 0; aReq < arRequests.size(); ++aReq)
			{	apReq = arRequests.at(aReq);
				if (apReq->mSessionID == aSessionId && apReq->mRequestID == aRequestID && !apReq->mRemoteRequest)
				{	arRequests.removeAt(aReq);
					delete apReq;
					break;
				}
			}
			// Bury the zombies
			if (apSession->mMode >= geSoft)
			{
				if (!isPending(aSessionId))
				{
					cleanupSession(aSessionId);
				}
				// No need to call clearRequests() since it was already called when closeSession() is invoked.
			}
			break;
		}
		}	// end switch
			// Clean up.
		if (apData != NULL)
		{	free(apData);
			qDebug("ASessionMgr::processNextRequest(SessionId:%ld, RequestID:%ld, EvalType:%ld, NoDebug:%ld, CmdStr:%s) , Data dropped.",
                aSessionId, aRequestID, aRequestType, aNoDebug, apStr);
		}
	}	// end if
	NO_MORE:
	cSessionMgrMutex.unlock();
	//TM! how do we want to report errors? retval might contain one.
}

long ASessionManager::readHtmlPage(long iSessionId, char* ipUrl, const char** opPage, long iMsecToWait)
{
	long aCode = AOK;
	bool aCanceled = false;
	iMsecToWait = (iMsecToWait == 0) ? 20000 : iMsecToWait;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || cSessions[iSessionId]->mpContext == NULL)
	{	cSessionMgrMutex.unlock();
		aCode = ABAD_SESSIONID;
		*opPage = NULL;
	}
	else
	{	ASession* apSession = cSessions[iSessionId];
		apSession->mReadHtmlOk = false;
		cSessionMgrMutex.unlock();

		//	Call the current host to go get the page
		QString aUrl(ipUrl);
		cbReadHtmlPage(iSessionId, aUrl, iMsecToWait);

		// Wait for notification from notifyHtmlPage
		for (;;)
		{	cSessionMgrMutex.lock();
			apSession = cSessions[iSessionId];
			if (apSession->mReadHtmlOk)
			{	cSessionMgrMutex.unlock();
				break;
			}
			unsigned aEscape = apSession->mEngineFlags & SBGLUE_ESCAPE;
			AContextSt* apContext = apSession->mpContext;
			cSessionMgrMutex.unlock();
			if (!aCanceled && aEscape != 0)
			{	aCanceled = true; // only call cancelReadHtmlPage once!
				cbCancelReadHtmlPage(iSessionId);
			}
			apContext->mpThread->localSleep(1L/*Msec*/);
		}
		*opPage = (const char *)(cSessions[iSessionId]->mPageContent);
	}
	return	aCode;
}

/*!
\brief registerContext creates a new context in the list of contexts managed by the Session Manager.

The context is not loaded into memory or run (see loadContext below).  Submit adds requests to the context's request queue.  The run
 method in AContextThread removes requests from this queue. See parameters.txt for a description of the context-specific
 parameters.  Establish a new context, but do not load it yet. Return Context Name.  If orContextName is set to the System
 Context, just use defaults.  System context does not have an context-specific ini file or a startup script associated with it.
\param irStartupPath - Path + file name of startup file
\param orContextName - Place to return the name of the context for the context
\param orMsgs - Place to return any generated messages
\param iIsOpen - If true, it is OK to reregister a context that is already open.
\return aSessionId - SessionId (>0) if no error, else error code (<0)
 */
long ASessionManager::registerContext(const QString& irStartupPath, QString& orContextName, QString& orMsgs, bool iIsOpen)
{
	// Extract ini file spec and work dir from irStartupPath. The path is converted into an absolute path.
	long aCode = 0;
	QString aMsg, aMsgs, aWorkDir, aStartupFile;
	QFileInfo aCurFile("");
	QFileInfo aFi(irStartupPath);
	// FileInfo values if startup path ../aisdev/astartup.sl and CWD is E:/IBA/aisdev/foo
	// QDir aFi.absoluteDir				E:/IBA/aisdev
	// QString aFi.absoluteFilePath()	E:/IBA/aisdev/astartup.sl
	// QString aFi.absolutePath()		E:/IBA/aisdev
	// QDir aFi.Dir()					../aisdev
	// QString aFi.fileName()			startup.sl
	// QString aFi.filePath()			../aisdev/astartup.sl
	// QString aFi.path()				E:/IBA/aisdev

	if (aFi.isFile())
	{	aWorkDir = aFi.absolutePath();	// Strip off file name.
		aStartupFile = aFi.fileName();	// e.g. astartup.sl
	}
	else if (aFi.isDir())
	{	aWorkDir = aFi.absoluteFilePath();
		aStartupFile = "";
		if (orContextName.isEmpty())
			aMsg = QString("No startup file for unnamed context in %1.\n").arg(aWorkDir);
		else
		aMsg = QString("No startup file for context: %1 in %2.\n").arg(orContextName).arg(aWorkDir);
		aMsgs += aMsg;
	}
	else	// Can't locate startup directory, give up.
	{	aMsg = QString("registerContext(), No startup file or working dir for %1 in path %2. Unable to register.\n")
		.arg(orContextName).arg(irStartupPath);
		if (orMsgs != QString::null)
			orMsgs += aMsg;
		return ACONTEXT_OPEN_FAILED;
	}
	// Initialize context-specific parameters to server-wide context default values.
	AStringMap aCtxParams(gAis.mCtxParams);	// Context-dependent configuration parameters
	AUtil::terminateStg('/', aWorkDir);

	// Read the context config. parameters from the context.ini file in the startup work dir.
	// Skip this step if orContextName is set to the system context name
	bool aIsSystemContext = (orContextName == AGLBLS_SYSTEMCONTEXTNAME);
	if (orContextName.isEmpty() || !aIsSystemContext)
	{	QString aAppIniFile = aWorkDir +  AGLBLS_APPINIFILENAME;
		QFile aIniFile(aAppIniFile);
		if (!aIniFile.exists())
		{	aMsg.sprintf("No config file %s. Using defaults.\n", aAppIniFile.toLatin1().data());
			aMsgs += aMsg;
		}
		else
		{	ANamedValues aNamedValues(aAppIniFile);	// true=> expand values
			aMsg.truncate(0);
			if (!aNamedValues.readPairs(true/*Expand*/, false/*DoGlobals*/, aCtxParams, gAis.mGblParams, aMsg))
				aMsgs += "Unexpected definitions: " + aMsg + '\n';
		}
		// Extract overrides from script file
		QString aScript;
		if (!aStartupFile.isEmpty())
		{	AUtil::readFile(aWorkDir + aStartupFile, aScript);
			if (!aScript.isEmpty())
				AScriptParams::parse(aScript, aCtxParams, false/*DoGlobals*/);
			else
				aStartupFile = "";
		}
		// After the final set of overrides, extract the context name
		orContextName = aCtxParams["contextname"];
		if (aStartupFile.isEmpty())
		{	aMsg.sprintf("registerContext(), No startup file found for context %s.\n", orContextName.toLatin1().data());
			aMsgs += aMsg;
		}
	}
	else if (aIsSystemContext)
		aCtxParams["contextname"] = orContextName;

	// WorkDir. Set workdir
	aCtxParams["WorkDir"] = aWorkDir;

	// Look for an empty slot in OpenContexts
	AContextSt* apContext = NULL;
	long aContextId;
	cSessionMgrMutex.lock();
	if (cContextMap.contains(orContextName))
	{	aContextId = cContextMap.value(orContextName);
		// Don't reregister if already open
		if (cOpenContexts[aContextId]->mAlive > 0)
		{	cSessionMgrMutex.unlock();
			return 0;
		}
		if (!iIsOpen)		// OK for openContext to reregister
		{	aMsg.sprintf("ASessionMgr::registerContext(), %s is being reregistered.\n", orContextName.toLatin1().data());
			aMsgs += aMsg;
		}
	}
	else
	{	long aSz = cOpenContexts.size();
		for (aContextId = 0; aContextId < aSz; ++aContextId)
		{	if (cOpenContexts[aContextId]->mAlive < 0)	// (empty <0, registered =0, opened >0)
				break;
		}
		if (aContextId >= aSz)		// Else, add an entry to Open Contexts list
		{	apContext = new AContextSt;
			cOpenContexts.insert(aContextId, apContext);
		}
		cContextMap[orContextName] = aContextId;
		if (aCtxParams.contains("httpdir"))
			aCtxParams["httpdir"].prepend(aWorkDir);
		if (aCtxParams.contains("xmldir"))
			aCtxParams["xmldir"].prepend(aWorkDir);
		if (aCtxParams.contains("appdir"))
			aCtxParams["appdir"].prepend(aWorkDir);
		aMsg.sprintf("Registered %s at %s\n", orContextName.toLatin1().data(), aWorkDir.toLatin1().data());
		aMsgs += aMsg;
	}
	// Save/update context state
	apContext = cOpenContexts[aContextId];
	apContext->mAdminSessionId = -1;
	apContext->mAlive = 0;		// (empty <0, registered =0, opened >0)
	apContext->mpCP = NULL;
	apContext->mContextName = orContextName;
	apContext->mpCodeData = NULL;
	apContext->mContextParams = aCtxParams;
	apContext->mDefaultCloseMode = gAis.mCloseModes[aCtxParams["closemode"]];
	aMsg = aCtxParams["defaultjitmode"];
	apContext->mJitMode = (aMsg == "off" || aMsg == "0") ? 0 : SBGLUE_JITON;
	apContext->mMemorySize = 0;
	apContext->mObjHdrSize = 0;
	apContext->mMode = geOpen;			// Active and registered
	apContext->mpRequest = NULL;
	apContext->mRunScript = "";
	apContext->mStartupScriptName = aStartupFile;
	apContext->mpThread = NULL;
	cSessionMgrMutex.unlock();

	// Read the security settings from the context users file in the startup work directory. Skip this step if orContextName is
	// set to the system context name.
	if (!orContextName.isEmpty() && !aIsSystemContext)
	{	QString aAppUsersFile = aWorkDir +  AGLBLS_APPUSERSFILENAME;
		if (!updateUsrMap(apContext, aAppUsersFile))
		{	aMsg.sprintf("No user update file %s.\n", aAppUsersFile.toLatin1().data());
			aMsgs += aMsg;
		}
	}
	if (orMsgs != QString::null)
		orMsgs += aMsgs;
	return aCode;
}


/*!
\brief removeRemoteRequest - Remove a remote request from the request queue

Used Only by AContextClient. See AContextClient for details.
\param iSessionID - The session Id that made the request
\param iRequestID - The ID of the request to be removed
\return aFoundIt - True iff the request is found
 */
bool ASessionManager::removeRemoteRequest(ASessionID iSessionId, ARequestID iRequestId)
{
	bool aFoundIt = false;
	ASession* apSession = NULL;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || (apSession=cSessions[iSessionId]) == NULL || apSession->mpContext == NULL)
	{	cSessionMgrMutex.unlock();
		return false;
	}
	AContextSt *apContext = cSessions[iSessionId]->mpContext;
	ARequestList& arRequests = apContext->mRequests;
	ARequest* apReq;
	for (long aReq = 0; aReq < arRequests.size(); ++aReq)
	{	apReq = arRequests.at(aReq);
		if (apReq->mRequestID == iRequestId && apReq->mRemoteRequest)  // found request to remove
		{	arRequests.removeAt(aReq);
			delete apReq;
			aFoundIt = true;
			break;
		}
	}
	cSessionMgrMutex.unlock();
	return aFoundIt;
}

// Makes an HTTP request
long ASessionManager::requestHttp(long iSessionId,char* ipUrl,char* ipBody,char* ipFileName,const char **opPage,long iMsecToWait)
{
	long aCode = AOK;
	bool aCanceled = false;

	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || cSessions[iSessionId]->mpContext == NULL)
	{	aCode = ABAD_SESSIONID;
		*opPage = NULL;
	}
	else
	{	ASession* apSession = cSessions[iSessionId];
		apSession->mRequestHttpOk = false;
		AHttpClient* apHttpClient = apSession->mpHttpClient;
		if (apHttpClient == NULL)
		{	apHttpClient = new AHttpClient(this, iSessionId, 0, NULL, "ASessionManager");
			apSession->mpHttpClient = apHttpClient;
		}
		cSessionMgrMutex.unlock();

		//	Call the HttpClient on main event thread to go get the page
		QString aUrl(ipUrl);
		QString aFileName(ipFileName);
		QString aBody(ipBody);
		AisMgrOutputEvent* apEv = new AisMgrOutputEvent(iSessionId, 0/*ReqID*/, 0/*Status*/, aFileName/*Enctype*/, iMsecToWait,
		aUrl/*AisOut*/, NULL/*Data*/, 0/*DataSize*/, aBody/*Display*/, cMt/*Error*/, AISMGROUT_REQUESTHTTP);
		QCoreApplication::postEvent(this, apEv);

		// Wait for notification from notifyRequestHttp
		for (;;)
		{	cSessionMgrMutex.lock();
			apSession = cSessions[iSessionId];
			if (apSession->mRequestHttpOk)
				break;

			unsigned aEscape = apSession->mEngineFlags & SBGLUE_ESCAPE;
			AContextSt* apContext = apSession->mpContext;
			cSessionMgrMutex.unlock();
			if (!aCanceled && aEscape != 0)
			{	aCanceled = true; // cancelRequestHttp just once
				apHttpClient->cancelRequestHttp();
			}
			apContext->mpThread->localSleep(1L/*Msec*/);
		}
		cSessionMgrMutex.lock();
		*opPage = (const char *)(cSessions[iSessionId]->mPageContent);
	}
	cSessionMgrMutex.unlock();
	return	aCode;
}

/*!
\brief saveConsoleLog - save console output in console log

\param irDisplay - Body of record to be saved.
\param iRqId - ID of the request (server-wide unique incrementing integer)
\param iSessionId - ID of session generating this output
\param iType - Type of record (R for result, D for display).
\return aSaved - true iff save succeeds
\par Notes:
-# See ARingFile::appendRecord for information on composing records for storage in a ring file.
-# See AUtil::formatConsoleLog for information on formatting records for return in XML documents.
-# The client can filter these records or take action if a pending result is returned.
 */
bool ASessionManager::saveConsoleLog(const QString& irDisplay, ARequestID iRqId, ASessionID iSessionId, char iType)
{
	bool aSaved = false;
	ARingFile* apRingFile;
	if (!irDisplay.isEmpty())
	{	cSessionMgrMutex.lock();
		ASession* apSession = cSessions[iSessionId];
		ACHECK(iSessionId > 0 && cSessions.contains(iSessionId) && apSession != NULL && apSession->mpContext != NULL);
		if ((apRingFile = apSession->mpDisconnectedBuffer) != NULL)
			aSaved = apRingFile->appendRecord(irDisplay, iRqId, iSessionId, iType);
		cSessionMgrMutex.unlock();
	}
	return aSaved;
}

/*!
\brief serverFileOpenDialog is called by the SM_ServerFileOpenDialog() function.

This method is responsible for returning a filename to the SM_ServerFileOpenDialog() function by requesting the host to
provide the filename. This method operates in a similar fashion to ASessionManager::debug().  This routine passes the request
for a filename to the host by making the callback cbServerFileOpenDialog(). Like all other SM callbacks, the call to
cbServerFileOpenDialog() should return quickly. The _svrfile request from the client should arrive on the request queue just
like any other request.
Here is where things get interesting. Normally, the request queue is processed FIFO. The queue can contain requests from
multiple sessions so requests from different sessions are intermixed in the queue. When a request's execution causes a call
into serverFileOpenDialog the rules governing the order of execution of requests in the request queue change.  In fact, the
normal request queue dispatch method processNextRequest is not involved. Instead, serverFileOpenDialog must implement the
following rules for managing the request queue:
\param iSessionId - The ID of the session that submitted this request.
\param iRequestID - The ID of this request.
\param opFileNameBuffer - Place to put the selected filename
\param iBufLen - The length of the opFileNameBuffer
\par Notes:
-# Only _svrfile requests from the session that submitted the request that spawned the serverFileOpen can be processed.
-# A session request that makes a serverFileOpenDialog request essentially blocks all other sessions from executing requests
until the request is allowed to proceed. Other sessions are free to continue to submit requestsand these will be placed on
the request queue for later processing.
 */
long ASessionManager::serverFileOpenDialog(ASessionID iSessionId, ARequestID iRequestID, char *opFileNameBuffer, long iBufLen)
{
	bool		aFileNameReceived = false;
	ARequest*	apReq = NULL;
	long		aSize = 0;
	char*		apStr = NULL;
	AContextSt*	apContext = NULL;
	ARequestList* apRequests = NULL;

	// Make a callback to send the debug data to the client session. Note that the call to cbServerFileOpenDialog will return
	// quickly.
	cbServerFileOpenDialog(iSessionId, iRequestID);

	// Now loop through the request queue looking for requests for the session that has issued the serverFileOpenDialog call.
	// All other session requests are ignored.  This process is very  similar to processNextRequest. processNextRequest is
	// of course blocked while the context thread is in this method until this call ultimately returns a file name.
	while (!aFileNameReceived)
	{	cSessionMgrMutex.lock();
		apContext = cSessions[iSessionId]->mpContext;
		apRequests = &apContext->mRequests;

		// Iterate through the request queue looking for a new session request. While in debug we ignore requests from other
		// sessions on the context.
		aSize = apRequests->size();
		for (long aReq = 0; aReq < aSize; ++aReq)
		{	apReq = apRequests->at(aReq);
			if (apReq->mSessionID == iSessionId && apReq->mEvalType == SBGLUE_FILEOPENRESPONSE)
				break; // out of for with a request match found
			else
				apReq = NULL;
		}
		if (apReq != NULL)
		{	apStr = apReq->mCmdString.data();
			aFileNameReceived = true;
			//buffer should contain something like _svrfile=<filename>
            // TODO: What if buffer is not enough?
			strncpy(opFileNameBuffer, &apStr[9], iBufLen - 1);
            opFileNameBuffer[iBufLen - 1] = '\0';

			// Remove processed request from queue
			ARequest* apTargetReq;
			aSize = apRequests->size();
			for (long aReq = 0; aReq < aSize; ++aReq)
			{	apTargetReq = apRequests->at(aReq);
				if (apTargetReq == apReq)
				{	apRequests->removeAt(aReq);
					delete apReq;
					break;
				}
			}
		}	// end if
		cSessionMgrMutex.unlock();
		if (!aFileNameReceived) apContext->mpThread->wait(1000);
	}	// end while
	return 0;
}

/*!
\brief setAisMgr - Set the pointers to the AisMgr and AisSvr

\param ipAisMgr -> The AisMgr for this server
\param ipAisSvr -> The AisSvr for this server
\return 0
\par Notes:
-# This method must be called before any other SessionManager method (except the constructor)
 */
long ASessionManager::setAisMgr(QObject* ipAisMgr, AAisSvr* ipAisSvr)
{
	cpAisMgr = ipAisMgr;
	cpAisSvr = ipAisSvr;
	return 0;
}

/*!
\brief setEngineFlags Set the collection of flags that controls the execution of Lambdas for the session.

\param iSessionId - Session ID for this set of flags.
\param iFlags
\return aOk - always 0
\par Flags:
- SBGLUE_ESCAPE - Request for engine to stop current processing
- SBGLUE_ERROR_TRACE - Request engine enter error trace mode
- SBGLUE_INSTRUCTION_TRACE - Request engine enter instruction trace mode
- SBGLUE_JITON - Request engine enter Jit mode
- SBGLUE_SYSCHECKON - Request engine enter system check mode
\par Notes:
-# The error or instruction trace flags cause the engine to ignorethe jit on flag. This is not an error.
 */
AErrorCode ASessionManager::setEngineFlags(long iSessionId, unsigned iFlags)
{
    Q_UNUSED(iSessionId);
    Q_UNUSED(iFlags);

	// Note currently implemented
	return AOK;
}

/*!
\brief setEscape - Ask the engine to stop processing the current request.

\param iSessionId - Session ID for this request
\return aOk - always 0
 */
AErrorCode ASessionManager::setEscape(long iSessionId)
{
	long aCode = AOK;
	ASession* apSession = NULL;
	AContextSt* apContext;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || (apSession = cSessions[iSessionId]) == NULL ||
		(apContext = apSession->mpContext) == NULL || iSessionId == apContext->mAdminSessionId)
		aCode = ABAD_SESSIONID;
	else if (iSessionId == apContext->mExeSessionID)
	{	//TM! Note: we may want to see what it would take to clear the current request if it is not executing. This way the
		// client would not have to wait for the engine to clear the request. This may be important in a multi-session
		// circumstance.  Note that the escape bit is now set in two places. session->mEngineFlags and
		// gContextFlags[ContextIndex]. When the escape is handled, both of these bits need to be cleared!
		SBGlue_SetEngineFlags(apContext->mContextIndex, SBGLUE_ESCAPE, 1L);
	}
	// Ensure that a STOP request from the console will halt the current processing.  This may not be appropriate in
	// the multi-context case???
	else if (iSessionId >= 0)
		SBGlue_SetEngineFlags(apContext->mContextIndex, SBGLUE_ESCAPE, 1L);
	cSessionMgrMutex.unlock();
	return aCode;
}

/*!
\brief submit - Put a request on the context's request queue.

\param iSessionId		- context session
\param iEvalType		- SBGLUE_CMDSTRING, SBGLUE_DIRINFO, SBGLUE_...
\param iNoDebug		- 1=process command without respecting session's debug flags in the session's mEngineFlags variable
\param irCmdStr		- command string
\param iDateTime	- Time that this request was submitted
\param ipData       - binary buffer
\return aReqId - The unique request ID assigned to this request
 */
ARequestID ASessionManager::submit(long iSessionId, long iEvalType, long iNoDebug, const QByteArray& irCmdStr, QDateTime iDateTime
, char* ipData)
{
	// Check the session
	ARequestID aReqId = 0;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId))
		aReqId = ABAD_SESSIONID;
	else
	{	ASession* apSession = cSessions[iSessionId];
		AContextSt* apContext = apSession->mpContext;

		if (apContext == NULL || apSession->mMode != geOpen || apContext->mMode != geOpen)
			aReqId = ASESSION_CLOSED;
		else if (apContext->mAlive <= 0)		// (empty <0, registered =0, opened >0)
			aReqId = ACONTEXT_NOT_ACTIVE;
		else		// Determine request type from target Lambda
		{
			switch (iEvalType)
			{
			case SBGLUE_CMDSTRING:
			case SBGLUE_DIRINFO:
				if (apSession->mSecurityLevel < 1)
					aReqId = ASECURITY_VIOLATION;		// -35
				break;
			}
			// Add new request to request queue
			if (aReqId == 0)
			{	aReqId = ++cNextRequestID;
				// If this is an event we change the sessionID to the context admin session ID
				if (iEvalType == SBGLUE_EVENTCMD || iEvalType == SBGLUE_EVENTMSG)
					iSessionId = apSession->mpContext->mAdminSessionId;
				ARequest* apReq = new ARequest(iSessionId, aReqId, iEvalType, iNoDebug, irCmdStr, ipData, iDateTime);
				apSession->mpContext->mRequests.append(apReq);
				ipData = NULL;
			}
		}
	}
	cSessionMgrMutex.unlock();
	if (ipData != NULL)
	{	free(ipData);
		qDebug("ASessionMgr::submit(SessionId:%ld, EvalType:%ld, NoDebug:%ld, CmdStr:%s), Data dropped.",
            iSessionId, iEvalType, iNoDebug, irCmdStr.data());
	}
	return aReqId;
}
AErrorCode ASessionManager::subscribe(long iCurSessionID, long iSessionID, bool iRemove)
{
	ASession* apSession;
	long aCode = AOK;

	cSessionMgrMutex.lock();
	if (iSessionID <= 0 || !cSessions.contains(iSessionID) || (apSession = cSessions[iSessionID]) == NULL || apSession->mpContext == NULL)
		aCode = ABAD_SESSIONID;
	else if (iCurSessionID <= 0 || !cSessions.contains(iCurSessionID) || (apSession = cSessions[iCurSessionID]) == NULL || apSession->mpContext == NULL)
		aCode = ABAD_SESSIONID;
	else
	{	// Forward request to AisSvr to subscribe to this session.
		cpAisSvr->subscribe(iCurSessionID, iSessionID, iRemove);
	}
	cSessionMgrMutex.unlock();
	return aCode;
}

/*!
\brief sysCheck - Turn on/off the sysCheck flag

\param iSessionId - Session to be checked
\param iOnoff - If true set the flag, else reset it
\return aCode - 0 if no error, else the error code (<0)
\par Notes:
-# SysCheck causes the engine to run extensive checks on the system integrity during execution.
 */
long ASessionManager::sysCheck(ASessionID iSessionId, bool iOnoff)
{
	long aCode = AOK;
	ASession* apSession = NULL;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || (apSession = cSessions[iSessionId]) == NULL ||
	apSession->mpContext == NULL)
		aCode = ABAD_SESSIONID;
	else
	{	long ContextIndex = apSession->mpContext->mContextIndex;
		if (apSession->mpContext->mExeSessionID == iSessionId)
			SBGlue_SetEngineFlags(ContextIndex, SBGLUE_SYSCHECKON, iOnoff ? 1L : 0);
	}
	cSessionMgrMutex.unlock();
	return AOK;
}

// Allocate a minimum system context during system startup.  
// Note: This routine is only run once at startup.
bool ASessionManager::initMemoryMgr(NUM iMinimumMemory)
{
#if 1
    Q_UNUSED(iMinimumMemory);

	// This code does NOT ask for initial memory.
	// Note: Asking for initial memory and then freeing during openConext resulted
	//       in too little memory being available in 32bit execution mode. Please
	//       do not alter this initialization algorithm without contacting Michael Korns.
	long i = 0;

	// Clear cContextMemory allocations array
	cSessionMgrMutex.lock();
	for (i = 0; i < MAXCONTEXTALLOCATIONBLOCKS; cContextMemory[i++] = NULL)
		;

	cSessionMgrMutex.unlock();
	return(TRUE);
#else 
	// This code DOES ask for initial memory.
	// Note: Asking for initial memory and then freeing during openConext results
	//       in too little memory being available in 32bit execution mode. Please
	//       do not use this initialization algorithm without contacting Michael Korns.
	long i;
	char* apBlock;
	NUM aBlockSize = 1048576;
	NUM aNeeded = (iMinimumMemory + aBlockSize - 1) / aBlockSize; // Round up to nearest megabyte.
	NUM aRequested = aNeeded;
	NUM aAcquired = 0;

	// Clear cContextMemory allocations array
	cSessionMgrMutex.lock();
	for (i = 0; i < MAXCONTEXTALLOCATIONBLOCKS; cContextMemory[i++] = NULL)
		;

	// Ask for the number of blocks that are needed.  If that fails, ask for one less block until malloc succeeds.
	for (i = 0; aNeeded > 0 && i < MAXCONTEXTALLOCATIONBLOCKS; )
	{	apBlock = (char *)malloc(aRequested * aBlockSize);
		if (apBlock != NULL)
		{	cContextMemory[i++] = apBlock;
			aAcquired += aRequested;
			aNeeded -= aRequested;
			aRequested = aNeeded;
		}
		else if (--aRequested == 0)
			break;			// Fail if can't even get one block
	}

	cSessionMgrMutex.unlock();
	return (aNeeded == 0);
#endif
}

bool ASessionManager::initEmbeddedMySQL(QString &orAisOut)
{
#if __EXMYSQL
    const char *apServerArgs[] = {"none",
                                  "--datadir=./data",
                                  "--language=./english",
                                  "--basedir=./"
                                  };
    int aArgCount = sizeof(apServerArgs)/sizeof(char*);
    QString aWorkDir;
    QString aInstallDir;
    QString aLangDir;
    QDir aDataDir;
    QFileInfo aFileInfo;
	QString aDone;
	QString aDataDirStr(gAis.mGblParams["gblmysqldatadir"]);
    QString aInstallDirStr;
    QString aLanguageStr;
    bool aSkip = false;

    aInstallDir = gAis.mGblParams["gblaisinstallpath"];
    aWorkDir = gAis.mGblParams["gblaisapplicationpath"];

    // if data directory is absolute
    if (QDir::isAbsolutePath(aDataDirStr))
        aDataDir.setPath(aDataDirStr);
    else
    // if data directory is relative, append it to the working directory path
        aDataDir.setPath(aWorkDir + aDataDirStr);

    orAisOut += "MySQL data directory set to: " + aDataDir.absolutePath() + ".\n";

	// Create the directory if it doesn't exist
	if (!aDataDir.exists())
    {
        if (!aDataDir.mkpath(aDataDir.absolutePath()))
        {
            orAisOut += "Unable to create MySQL data directory: " + aDataDir.absolutePath() + ".\n";
            aSkip = true;
        }
    }
	
    // Possible locations of mysqlmsgs/errmsg.sys
    // 1. Install Path
    // 2. AGLBLS_SYSSHAREDIR (/usr/share/ais)
	    
    aLangDir = aInstallDir + AGLBLS_MYSQLLANGDIR + AGLBLS_MYSQLLANGFILE;
    aFileInfo.setFile(aLangDir);
    // 1st Location
    if (!aFileInfo.isFile())
	{
        orAisOut += "Unable to find MySQL language file: " + aFileInfo.absoluteFilePath() + ".\n";

#ifdef Q_OS_LINUX
	    aLangDir = QString(AGLBLS_SYSSHAREDIR) + AGLBLS_MYSQLLANGDIR + AGLBLS_MYSQLLANGFILE;
        aFileInfo.setFile(aLangDir);
	
        // 2nd Location
        if (!aFileInfo.isFile())
        {
	        orAisOut += "Unable to find MySQL language file: " + aFileInfo.absoluteFilePath() + ", initialization would fail.\n";
            aSkip = true;
        }
        else
        {
            orAisOut += "MySQL language set to: " + aFileInfo.absolutePath() + ".\n";
        }
#else
        aSkip = true;
#endif
	}
    else
    {
        orAisOut += "MySQL language set to: " + aFileInfo.absolutePath() + ".\n";
    }

	aDataDirStr = "--datadir=" + aDataDir.absolutePath();
    aLanguageStr = "--language=" + aFileInfo.absolutePath();
    aInstallDirStr = "--basedir=" + aInstallDir;

#if _MSVC
    apServerArgs[1] = _strdup(qPrintable(aDataDirStr));
    apServerArgs[2] = _strdup(qPrintable(aLanguageStr));
    apServerArgs[3] = _strdup(qPrintable(aInstallDirStr));
#else
    apServerArgs[1] = strdup(qPrintable(aDataDirStr));
    apServerArgs[2] = strdup(qPrintable(aLanguageStr));
    apServerArgs[3] = strdup(qPrintable(aInstallDirStr));
#endif

	// Implementation Note: If MySQL is included (FSmtbase.h), we must also
	// add the following include path to the properties for this source file:
	// "$MYSQLDIR\include"
    if (aSkip || (mysql_library_init(aArgCount, const_cast<char**>(apServerArgs), NULL) != 0))
	{
        orAisOut += "Embedded MySQL library initialization failed. The SQL feature will be disabled.\n";
        orAisOut += "This can be caused by several reasons:\n";
        orAisOut += "\t1. Missing or no read/write permission to data directory\n";
        orAisOut += "\t2. Missing or no read/write permission to language file\n";
        orAisOut += "\t3. Version of errmsg.sys does not match version of MySQL library\n";
        orAisOut += QString("MySQL Error: %1.\n").arg(mysql_error(0));
	}
    else
        cEmbeddedMySQLEnabled = true;

	free((void*)apServerArgs[3]);
	free((void*)apServerArgs[2]);
	free((void*)apServerArgs[1]);

#endif

	return true;
}

// Used Only by AContextClient. See AContextClient for details.
bool ASessionManager::markRequestAsRemote(ASessionID iSessionId, ARequestID iRequestId)
{	// Mark a request as remote so that it is not removed in processNext request. An instance of AContextClient
	// is responsible for deleting the request.
	bool aFoundIt = false;
	ASession* apSession = NULL;
	cSessionMgrMutex.lock();
	if (iSessionId <= 0 || !cSessions.contains(iSessionId) || (apSession = cSessions[iSessionId]) == NULL ||
	apSession->mpContext == NULL)
	{	cSessionMgrMutex.unlock();
		return false;
	}

	AContextSt *apContext = cSessions[iSessionId]->mpContext;
	ARequestList& arRequests = apContext->mRequests;
	ARequest* apReq;
	long aSize = arRequests.size();
	for (long aReq = 0; aReq < aSize; ++aReq)
	{	apReq = arRequests.at(aReq);
		if (apReq->mRequestID == iRequestId)  // found request to mark
		{	apReq->mRemoteRequest = true;
			aFoundIt = true;
			break;
		}
	}
	cSessionMgrMutex.unlock();
	return aFoundIt;
}

// updateUsrMap - Initialize Context's UsrMap from the AppUsersFile
// File record - UsrId:SecLvl:DateEnd:UserName
bool ASessionManager::updateUsrMap(AContextSt* ipContext, const QString& arAppUsersFile)
{
	// Initialize user map
	bool aOk = false;
	ipContext->mUsrMap.clear();		// Map of security updates for selected users.

	// Read the Users update file in the context's startup (workDir) folder
	QFile aF(arAppUsersFile);
	QString aMsg;
	if (aF.open(QIODevice::Text | QIODevice::ReadOnly))		// 16|1
	{	long  aSz;
		long aEndDay, aSecLvl, aUsrId;
		QDate aEndDate;
		QDate aY2K(2000, 1, 1);
		QString aLine, aTkn0, aTkn1, aTkn2;
		QStringList aTkns;
		aOk = true;
		QTextStream aInput(&aF);
		while (!aInput.atEnd())
		{	// Skip blank lines and comments
			aLine = aInput.readLine(1024);
			if (AUtil::isComment(aLine))
				continue;
			// Extract first 3 fields
			aTkns = aLine.split(':', QString::KeepEmptyParts);
			aSz = aTkns.size();
			if (aSz >= 3)
			{	aTkn0 = aTkns[0], aTkn1 = aTkns[1], aTkn2 = aTkns[2];
				aEndDay = aSecLvl = aUsrId = -1;
				if ((aUsrId = aTkn0.toLong()) < 1)
					qDebug("ASessionMgr::updateUsrMap(), Invalid UserID:%ld", aUsrId);
				else if (!aTkn1.isEmpty() && ((aSecLvl = aTkn1.toLong()) < 0 || aSecLvl > 7))
				{	qDebug("ASessionMgr::updateUsrMap(), UserID: %ld, Invalid SecLvl:%ld", aUsrId, aSecLvl);
					aSecLvl = 0;
				}
				else if (!aTkn2.isEmpty() && (!AUtil::stringToDate(aTkn2, aEndDate) || (aEndDay = aY2K.daysTo(aEndDate)) <= 0))
				{	qDebug("ASessionMgr::updateUsrMap(), UserID: %ld, Invalid expiration date:%s", aUsrId, aTkn2.toLatin1().data());
					aEndDay = 0;
				}
				if (aUsrId > 0)
				{	AUserSt aUsr;
					aUsr.mEndDay = aEndDay;
					aUsr.mSecLvl = aSecLvl;
					ipContext->mUsrMap[aUsrId] = aUsr;
				}
			}
			else
				qDebug("ASessionMgr::updateUsrMap(), Invalid entry:%s", aLine.toLatin1().data());
		}
		aF.close();
	}
	return aOk;
}
// end


