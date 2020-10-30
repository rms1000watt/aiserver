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
aisdev/aismgr/aismgr.cpp
											AAisMgr

CHANGE HISTORY
Version	Date		Who		Change
3.1007	 1/04/2008	fchua	Modified submit. Moved location of ACHECK in geCloseSession case.
3.1005	12/16/2007	fchua	Modified submit. Fixed geGetSessionUser case.
3.1005	12/16/2007	fchua	Modified submit. Supported buffering of console output after closing of session.
2.0004	4/13/2007	tlw		notifyHtmlPage. Do not delete the existing AWebPage as it causes a system crash.
2.0001	12/29/2006	tmay	added geExecute
1.0120	12/25/2006	tlw		returnOutput. Add iDataSize argument
1.0120	12/19/2006	tlw		Event. Add mpData argument.
1.0118	12/12/2006	tlw		onSubmit, submit. Add serial binary stream argument.
1.0113	11/7/2006	tlw		destructor. Free allocated resources in cWebPages.
1.0112	10/27/2006	tlw		notifyHtmlPage.  Remove entry from cWebPages.
1.0059	4/22/2005	tlw		Remove disconnectSession
1.0057	3/18/2005	tlw		Add documentation
												--------------- ---------------

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------- IMPORTS -----------------------------------------------------------
#include <time.h>				// time
#include <QtCore/QEvent>		// QEvent
#include <QtCore/QThread>		// QThread
#include <QtCore/QRegExp>		// QRegExp
#include <QtCore/QUrl>

#include "aglobals.h"			// ACHECK, 
#include "alogmgr.h"			// ALogMgr
#include "aissvr.h"				// AAisSvr
#include "aisoutput.h"			// AISMGROUT_SIZE
#include "aismgr.h"				// AAisMgr
#include "ausrmgr.h"			// AUsrMgr
#include "awebpage.h"			// AWebPage

#include "asessionmgrinterface.h"	// ASessionMgrInterface
#include "autilities.h"			// AUtil
#include "asbglue.h"

//	-------------------------------------------------- DEFINED CONSTANTS ------------------------------------------------------

//	--------------------------------------------------- PUBLIC METHODS --------------------------------------------------------
/*!
\brief AAisMgr - Provides an interface between the protocol servers and the Session Manager.

AIS Manager interfaces AAisSvr on one side with the Session Manager on the other side. It formats requests being sent to the
Session Manager and it processes returns from the Session Manager and then returns the returned information back to AisSvr.
It also keeps track of each request submitted in a queue and then combines the information saved in the queue with the returned
result from the engine.
 */
AAisMgr::AAisMgr(ASessionManager* ipSessionMgr)
	: cPendingReqs(0), cPendingReqsLast(0), cpSessionMgr(ipSessionMgr)
{
	// Initialize & check inputs
	cpAisSvr = NULL;
	cMt = "";

	// Initialize map of special enctypes
	cEnctypes["url"]  = geRetUrl;			// Return type 1
	cEnctypes["file"] = geRetFile;			// Return type 2
	cEnctypes["text"] = geRetText;			// Return type 3
	cEnctypes["application/x-ampmsg"] = geRetQuery;

	// Initialize special function types for Ais call-back functions
	cFcnTypes[AISMGROUT_DEBUG] = geFcnDebug;
	cFcnTypes[AISMGROUT_FILEOPEN] = geFcnFileOpen;
	cFcnTypes[AISMGROUT_READHTMLPAGE] = geFcnReadHtmlPage;
	cFcnTypes[AISMGROUT_RETURNRESULT] = geFcnReturnResult;
	cFcnTypes[AISMGROUT_RINGBELL] = geFcnRingBell;
	cFcnTypes[AISMGROUT_SENDTOCLIENT] = geFcnSendToClient;
	cFcnTypes[AISMGROUT_SENDSTATUSTOCLIENT] = geFcnSendStatusToClient;
	cFcnTypes[AISMGROUT_ENGINESTATE] = geFcnEngineState;
	cFcnTypes[AISMGROUT_RETURNRESULT_FORWARDING] = geFcnReturnResult;
	cFcnTypes[AISMGROUT_DISPLAY] = geDisplay;
}

/*!
\brief destructor. Free all allocated resources.

Make sure that all entries in cWebPages have been deallocated.
\par Notes:
-# AConnectMgr has remote ownership of ASessionSt and AServerSt referents.
-# Since instances of AConnectMgr are never assigned, copied, or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
AAisMgr::~AAisMgr()
{
	// remove association with session manager
	cpSessionMgr->setAisMgr(NULL, NULL);

	AWebPages::const_iterator aIt;
	for (aIt = cWebPages.constBegin(); aIt != cWebPages.constEnd(); ++aIt)
	{	delete aIt.value();
	}
	cWebPages.clear();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AAisMgr()");
#endif
}

/*!
\brief event - Return result to client from event generated by engine
\param ipEv - Event payload containing information returned from Session Manager
\return void
\par Notes:
-# Requests to the engine that generate asychronous output make a callback into one of the callback functions declared in
sessionmgr.h.  These functions post an AisMgrOutputEvent. These events are caught here (and in SessionMgr).  A callback is
made to AAisSvr's returnOutput which, in turn, calls the returnOutput of the controlling protocol server (appsvr, xmlsvr,
httpsvr,...).  The protocol servers return this payload back to the appropriate client.
-# If a request is associated with the returned output (the return is not "pushed" output), the request queue has additional
information that is included in the callback to the server.
-# The payload in ipEv includes a "function type" which is a numeric code for each return type. The function types are:
 \verbatim
	AMPMSG
	Type	Return
	Code	Type	Enctype		Text		Url
	-----	-----	-------		----		---	
	0		Amp		"amp"		ampmsg		-
	1		Url		"url"		  -			url
	2		File	"file"		filename	 -
	3		Text	enctype		mimetext	 -
\endverbatim
 */
bool AAisMgr::event(QEvent* ipEv)
{
    if (ipEv->type() != QEvent::User + AISMGROUT_EVENT) return false; 
	QString aAmpmsg;
	long aContextID, aRqId = 0;
	bool aForwarding = false, aIsAsync = false, aRetOut = false;
	AReqType aReqType = geUnknown;
	AisMgrOutputEvent* apEv = (AisMgrOutputEvent*)ipEv;
	QString aAisOut = apEv->mAisOut;
	long aFcnType = apEv->mFcnType;
	char* apData = apEv->mpData;
	long aDataSize = apEv->mDataSize;
	QString& arDisplay = apEv->mDisplay;
	QString& arEnctype = apEv->mEnctype;
	QString aError = apEv->mError;
	long aReqID = apEv->mRequestID;
	long aStatus = apEv->mStatus;
	long aSessionID = apEv->mSessionID;
	long aRetValue = apEv->mRetValue;

	if (aSessionID <= 0)
	{	qDebug("AAisMgr::event(SessionID:%ld, ReqID:%ld, Status:%ld, Enctype:%s, RetValue:%ld,AisOut:%.128s, Error:%.128s)"
		", Invalid SessionId", aSessionID, aReqID, aStatus, arEnctype.toLatin1().data(), aRetValue,aAisOut.toLatin1().data()
		,aError.toLatin1().data());
		aStatus = AERR_SESSIONID;
		aError = gAis.mpErrMsgs[aStatus];
		aRetOut = true;
	}
	else
	{	switch(aFcnType)
		{
		case AISMGROUT_DISPLAY:
			aReqType = cFcnTypes[aFcnType];			// geDisplay
			aRetOut = true;
			break;
		// Close a session or a context.  SessionID is the ID of the session or context being closed. RetValue holds connect
		// ID if caller closing another session.
		case AISMGROUT_CLOSESESSION:
			if (cPendingRequests.contains(aReqID))
			{	APendingReq& arPendReq = cPendingRequests[aReqID];
				aReqType = arPendReq.mReqType;
				aAmpmsg = arPendReq.mAmpmsg;
				aIsAsync = arPendReq.mIsAsync;
				aRqId = arPendReq.mRqId;
				if (--cPendingReqs < 0)
					cPendingReqs = 0;			// Should always be >= 0
				cPendingRequests.remove(aReqID);
			}
			else		// Forced closeSession (probably due to closeContext)
				aReqType = geCloseSession;
			aRetOut = true;
			break;
		case AISMGROUT_RETURNRESULT_FORWARDING:
			// See AContextClient::returnMsg
			aForwarding = true;
		case AISMGROUT_RETURNRESULT:
			// Check the request queue
			aContextID = cpSessionMgr->getContextId(aSessionID);
			// PENDING. AisMgr::event(). Getting back superflous events that have nowhere to go when closing a session.
			if (cPendingRequests.contains(aReqID) && aContextID >= 0)
			{	APendingReq& arPendReq = cPendingRequests[aReqID];
				aReqType = arPendReq.mReqType;
				aAmpmsg = arPendReq.mAmpmsg;
				aIsAsync = arPendReq.mIsAsync;
				aRqId = arPendReq.mRqId;
				LOGAMPMSG(aContextID, aSessionID, aReqID, aAmpmsg, aAisOut);
				if (--cPendingReqs < 0)
					cPendingReqs = 0;			// Should always be >= 0
				cPendingRequests.remove(aReqID);

				if (!aForwarding)
				{	// Convert enctype to geRetUrl,geRetFile,geRetText,geRetQuery
					if  (aReqType == geAmpMsg && cEnctypes.contains(arEnctype) && (aReqType=cEnctypes[arEnctype]) == geRetQuery)
					{	// Convert query string to AMP msg format
						aAisOut.replace(QRegExp("[=&]"), DELSTG);
						AUtil::urlDecode(aAisOut);
						aReqType = geAmpMsg;
					}
					// Replace DEL delimiters with newlines
					else if (aReqType == geGetDirInfo)
						aAisOut.replace('\177', '\n');
				}
			}
			else	// No request pending, return error
			{	aStatus = (aContextID < 0) ? -aContextID : AERR_BADREQUEST;
				const char *apErr = gAis.mpErrMsgs[aStatus];
				aError = apErr;
				qDebug("AAisMgr::event(SessionID:%ld,ReqID:%ld,Status:%ld,Enctype:%s,RetValue:%ld,AisOut:%.128s), %s", aSessionID
				, aReqID, aStatus, arEnctype.toLatin1().data(), aRetValue, aAisOut.toLatin1().data(), apErr);
			}
			aRetOut = true;
			break;

		// Return aAisOut generated by these commands. arDisplay is not used.
		case AISMGROUT_FILEOPEN:
		case AISMGROUT_RINGBELL:
		case AISMGROUT_DEBUG:
		case AISMGROUT_SENDTOCLIENT:
		case AISMGROUT_SENDSTATUSTOCLIENT:
			aReqType = cFcnTypes[aFcnType];
			aRetOut = true;
			break;

		case AISMGROUT_ENGINESTATE:
			// TM - we will need to broadcast to all sessions connected to this session's context next. Just get messaging back
			// to single session working first. Returns state in aRetValue.
			aReqType = cFcnTypes[aFcnType];
			aRetOut = true;
			break;

		// None of the following cases return async output from here.
		case AISMGROUT_CANCELREADHTMLPAGE:
			if (cWebPages.contains(aSessionID))
			{	AWebPage* apWebPage = cWebPages.value(aSessionID);
				apWebPage->cancelReadHtmlPage();		// cancelReadHtmlPage calls notify
			}
			break;

		// readHtmlPage is a special case. Instead of returning this output event to the client, send it to the html manager to
		// fetch a web page.  Html manager will notify session manager when the page arrives.
		case AISMGROUT_READHTMLPAGE:
			// Note that aRetValue is the msecToWait specification
			if (!cWebPages.contains(aSessionID))
			{	cWebPages.insert(aSessionID, new AWebPage(this, aSessionID, aRetValue));
			}
			cWebPages[aSessionID]->getPage(aAisOut);
			break;
		default:
			qDebug("AAisMgr::event(), unrecognized return type");
			break;
		}
	}
	if (aRetOut)
		cpAisSvr->returnOutput(0, aSessionID, aRqId, aStatus, aReqType, aRetValue, aAisOut, apData, aDataSize, arDisplay,aError
		, aIsAsync);
	return true;
}

/*!
\brief flushSessions - Called periodically to push out old console output even if the display buffers are not full.

\return void
\par Notes:
-# Calls ASessionManager::flushSessions which returns the contents of the session display buffers to AisSvr.returnOutput
-# If the pending requests has changed to/from zero, update "engine busy" status
 */
void AAisMgr::flushSessions()
{	
	cpSessionMgr->flushSessions(false/*Now*/);
	if ((cPendingReqs > 0) != (cPendingReqsLast > 0))
	{	const char* apMsg = (cPendingReqs > 0) ? "_icon=busy\177status=on" : "_icon=busy\177status=off";
		LOGSTATUSMSG(apMsg);
		cPendingReqsLast = cPendingReqs;
	}
}

/*!
\brief notifyHtmlPage - Return notification that a requested HTML Page has been retrieved
\param iSessionId - Session waiting for a page
\param irPageBfr - Page returned from the web
\return void
\par Notes:
-# Calls ASessionManager::notifyHtmlPage which returns the page to the waiting session.
-# An HTTP page request is synchronous. That is, the context thread is suspended until the page is returned.
 */
void AAisMgr::notifyHtmlPage(long iSessionId, const QByteArray& irPageBfr)
{
	cpSessionMgr->notifyHtmlPage(iSessionId, irPageBfr);
	if (cWebPages.contains(iSessionId))
	{	// delete cWebPages.value(iSessionId);	// this causes problems because the parent was already deleted.
		cWebPages.remove(iSessionId);
	}
}

/*!
\brief submit - Format and submit a AMP request to the Session Manger

\param iSessionId	Identifies session generating request
\param iRqId		Unique identifier (per Server) for every incoming message.
\param irAmpmsg	DEL-delimited string of name-value pairs starting with target Lambda and speech acg
\param iIsAsync	Anonymous flag passed back to caller.
\param opRetValue	Place to put the integer value for immediate returns only.
\param orAisOut	Place to return result for immediate returns only.
\param orDisplay	Place to return display for immediate returns only
\return aReqId - (0 immediate, >0 pending, <0 error code) 
\par Notes:
-# Calls ASessionManager::submit
-# Errors are logged locally, but a negative request ID is returned to caller, to allow callers to clean up.
-# Note that callers may get an error notification via a call to returnOutput if error generated further downstream.
-# Request is pending iff ReqId > 0.
-# Every submission results in exactly one of the following:
- Submit to SMgr.  Result returned via a user event. Request added to cPendingRequests
- Direct return. Result returned back to caller. No pending req generated.
- Error. Error returned back to caller. No pending req generated.
 */
long AAisMgr::submit(long iSessionId, long iRqId, const QString& irAmpmsg, bool iIsAsync, long* opRetValue, QString& orAisOut
, QString& orDisplay, char* ipData)
{
	// irAmpmsg is either:  target|act|..., _ais|act|...,  _ais|eval|exp|(debug...)(...)
	AReqType aReqType = geAmpMsg;	// target|act|...
	QByteArray aAmpReq;				// Extended request that is passed on to session mgr.
	QString aTkn;
	QStringList aTkns;
	long aReqId = 1,aSessionId = 0;	// ReqId: > 0 ReqPending, 0 return result, <0 return error
	long aTknSz = 1;
	if (irAmpmsg[0] == '_')
	{	if (irAmpmsg.startsWith("_ais"))// _ais|act|...
		{	// Expect a target Lambda and a speech act at a minimum.
			aTkns = irAmpmsg.split(QChar('\177'), QString::KeepEmptyParts);
			aTknSz = aTkns.size();

			// Extract request type from the speech act. Returns geUnknown if not found
			aReqType = (aTknSz < 2) ? geNoop : REQTYPE(aTkns[1].toLower());
		}
		else
			aReqId = -AERR_BADAMP;
	}
	// Preprocess and dispatch to session manager based upon request type
	QString aAisOut(""), aDisplay("");
	const QString& arArg3 = (aTknSz >= 4) ? aTkns[3] : "";
	const QByteArray& arByteArray3 = arArg3.toLatin1();
	long aEvalType = SBGLUE_CMDSTRING;	// Default request type for session manager
	long aNoDebug = SBGLUE_NO_DEBUG;	// Default flag for debug processing. ie: ignore user specified debug flags	except for specific req types.
	long aRetValue = 0;					// Holds returned integer values (immediate only).
	if (aReqId < 0) goto lRetError;
	switch(aReqType)
	{
	case geAmpMsg:						// target|act|...
		aAmpReq = irAmpmsg.toLatin1();
		aEvalType = SBGLUE_MSGBLOCK;
		aNoDebug = SBGLUE_WITH_DEBUG;	// This may be dangerous - What happens if we debug an XML session?  Leave it in for
		break;							// now as we have to be able to STOP a session running an amp Lambda.

	case geCheckCabinet:				// _ais|checkcabinet|cabname|%s
		ACHECK(aTknSz == 4);
		aAmpReq = "(browseLib.checkStatus {" + arByteArray3 + "})";
		break;

	case geCloseCabinet:				// _ais|closecabinet|cabname|%s
		ACHECK(aTknSz == 4);
		aAmpReq = "(browseLib.dropExtent {" + arByteArray3 + "})";
		break;

	case geCloseConnection:				// _ais|closeconnection|connectid|%d|closemode|%s|closewait|%d
		ACHECK(aTknSz == 2);
		aReqId = cpSessionMgr->closeConnection(iSessionId);
		break;

	case geCloseContext:				// _ais|closecontext|context|%s|closemode|%s
	{	ACHECK(aTknSz == 6 && !arArg3.isEmpty() && opRetValue != NULL);
		ACloseMode aCloseMode = geDefault;
		if (aTknSz >= 6 && gAis.mCloseModes.contains(aTkn = aTkns[5].toLower()))
			aCloseMode = gAis.mCloseModes[aTkn];

		// In this special case, connect ID of caller is passed in via RetValue.
		long aConnectId = (opRetValue != NULL) ? *opRetValue : 0;
		aReqId = cpSessionMgr->closeContext(aConnectId, arArg3, aCloseMode);
		break;
	}
	case geCloseSession:		// _ais|closesession|sessionid|%d|closemode|%s
	{	ACloseMode aCloseMode = geDefault;
		aSessionId = iSessionId;
		if (aTknSz >= 4)
		{	aSessionId = arArg3.toLong();
			if (aTknSz >= 6 && gAis.mCloseModes.contains(aTkn = aTkns[5].toLower()))
				aCloseMode = gAis.mCloseModes[aTkn];
		}
		ACHECK(aSessionId > 0 && aCloseMode != geOpen);
		cpSessionMgr->flushSessions(true/*Now*/);
		// In this special case, connect ID of caller is passed in via RetValue iff closing some other client's connection.
		long aConnectId = (aSessionId != iSessionId && opRetValue != NULL) ? *opRetValue : 0;
		aReqId = cpSessionMgr->closeSession(aCloseMode, aConnectId, aSessionId);
		
		// if call to closeSession is successful
		if (aReqId > 0)
		{
			if (aCloseMode < geOpen)
				// Create console log to store display after disconnection
				cpSessionMgr->openConsoleLog(false, false, aSessionId, 0, false);
			else
				// Close/terminate any existing console log
				cpSessionMgr->enableConsoleLog(aSessionId,-1);
		}
		break;
	}
	case geCompileAll:					// _ais|compileall
	{	ACHECK(aTknSz == 2);
		aAmpReq = "(browseLib.compileAllAutoCompileCabinets)";
	}
	break;
	case geCompileLambda:					// _ais|compilelambda|extent_Lambdas|%s
	{	ACHECK(aTknSz == 4);
		// Extract extent-Lambda pairs
		QStringList aPair = arArg3.split(QChar('\t'), QString::KeepEmptyParts);
		QString aLastFocus = "";
		QByteArray aItem;
		long aPairSz = aPair.size();
		aAmpReq = "((lambda()vars:(saveFocus)(setq saveFocus (browseLib.getFocus))";

		// Add in each pair to compile
		for (long i=0; i < aPairSz; ++i)
		{	// Add code to focus on correct extent
			if (aPair[i] != aLastFocus)
			{	aItem = " (browseLib.setFocus |";
				aItem += aPair[i].toLatin1();
				aItem += "|:) ";
				aAmpReq += aItem;
				aLastFocus = aPair[i];
			}
			aItem = "(browseLib.compileSource |";
			aItem += (++i < aPairSz) ? aPair[i].toLatin1() : "noLambda";
			aItem += "|:) ";
			aAmpReq += aItem;
		}
		aAmpReq += " (browseLib.setFocus saveFocus) true))";
		break;
	}
	case geCompileCabinet:			// _ais|compilecabinet|extents|%s
	{	ACHECK(aTknSz == 4);
		// Extract extent names
		QStringList aExtents = arArg3.split(QChar('\t'), QString::KeepEmptyParts);
		QByteArray aItem;
		long aExtentSz = aExtents.size();
		aAmpReq = "((lambda()vars:(saveFocus)(setq saveFocus (browseLib.getFocus))";
		// Loop thru extents to compile each one
		for (long i = 0; i < aExtentSz; ++i)
		{
			aItem = "(browseLib.compileCabinet |";
			aItem +=  aExtents[i].toLatin1();
			aItem += "|:)";
			aAmpReq += aItem;
		}
		aAmpReq += " (browseLib.setFocus saveFocus) true))";
		break;
	}
	case geDebug:					// _ais|debug|exp|%s
		ACHECK(aTknSz >= 4);
		aAmpReq = irAmpmsg.section('\177', 3).toLatin1();	// Everything after exp
		aEvalType = SBGLUE_DEBUGCMD;
		break;

	case geEraseNode:				// _ais|erasenode|extent|%s|nodes|%s
	{	ACHECK(aTknSz == 6);
		// Extract node names
		QStringList aNodes(aTkns[5].split(QChar('\t'), QString::SkipEmptyParts));
		QByteArray aItem;
		long aNodesSz = aNodes.size();
		aAmpReq = "((lambda()vars:(saveFocus)(setq saveFocus (browseLib.getFocus))(browseLib.setFocus |";
		aAmpReq += arByteArray3 + "|:)";

		// Add in name of each node to delete
		for (long i=0; i < aNodesSz; ++i)
		{	aItem = " (browseLib.eraseSource |" + aNodes[i].toLatin1() + "|:) ";
			aAmpReq += aItem;
		}
		aAmpReq += "(browseLib.setFocus saveFocus) true))";
		break;
	}
	case geEval:					// _ais|eval|exp|(debug ...)(...)
		// Note: A Lisp expression will be subject to the settings of the session instructionTrace, errorTrace and systemCheck
		// flags unless we are already interrupted in debug mode (debugging a currently executing request).
		aAmpReq = irAmpmsg.section('\177', 3).toLatin1();
		// Noop. Bypass normal request processing
		if (aAmpReq.startsWith("(noop"))
		{	aReqId = 0;
			aAisOut = "true";
		}
		else
			aNoDebug = SBGLUE_WITH_DEBUG;
		break;

	case geExecute:					// _ais|execute|exp|(debug ...)(...)
		// Note: A Lisp expression will be subject to the settings of the session instructionTrace, errorTrace and systemCheck
		// flags unless we are already interrupted in debug mode (debugging a currently executing request).
		aAmpReq = irAmpmsg.section('\177', 3).toLatin1();
		// Noop. Bypass normal request processing
		if (aAmpReq.startsWith("(noop"))
		{	aReqId = 0;
			aAisOut = "true";
		}
		else
			aNoDebug = SBGLUE_WITH_DEBUG;
		aEvalType = SBGLUE_CMDSTRING_BINTRANSFER;
		break;

	case geExportCabinet:			// _ais|exportcabinet|cabinet|%s|file|%s|setlocation|%s
                                    // _ais|exportcabinet|cabinet|%s|folder|%s|setlocation|%s
		ACHECK(aTknSz == 8);
		aAmpReq = "((lambda()vars:(aExtentInfo saveFocus ret)(setq saveFocus (browseLib.getFocus))(browseLib.setFocus |" + arByteArray3 + "|:)";
        if( aTkns[4] == "file" )
        {
		    aAmpReq += "(setq ret (browseLib.exportSource {" + aTkns[5].toLatin1() + "} {..all Lambdas..} " + aTkns[7].toLatin1() + "))";
        }
        else
        {
		    aAmpReq += "(setq ret (browseLib.exportDirectory {" + aTkns[5].toLatin1() + "} {..all Lambdas..} " + aTkns[7].toLatin1() + "))";
        }
        aAmpReq += "(browseLib.setFocus saveFocus)(return ret)))";
		break;

	case geExportNode:				// _ais|exportnode|node|%s|file|%s|cabinet|%s
		ACHECK(aTknSz == 8);
		aAmpReq = "((lambda()vars:(saveFocus ret)(setq saveFocus (browseLib.getFocus))(browseLib.setFocus |";
		aAmpReq += aTkns[7].toLatin1() + "|:)(setq ret (browseLib.exportSource |" + aTkns[5].toLatin1() + "|: {";
		aAmpReq += arByteArray3 + "}))(browseLib.setFocus saveFocus)(return ret)))";
		break;

	case geGetContextId:			// _ais|getcontextid|context|%s
		ACHECK(aTknSz == 4);		// _ais|getcontextid|sessionid|%d
		if (aTkns[2] == "context")
			aReqId = cpSessionMgr->getContextId(arArg3);
		else
			aReqId = cpSessionMgr->getContextId(arArg3.toLong());
		if (aReqId > 0)
		{	aRetValue = aReqId;
			aAisOut = QString::number(aRetValue);
			aReqId = 0;
		}
		break;
	
	case geGetContextParams:		// _ais|getcontextparams|context|%s
	{	ACHECK(aTknSz == 4);		// _ais|getcontextparams|contextid|%d
		AStringMap* apParams;
		if (aTkns[2] == "contextid")
			apParams = cpSessionMgr->getContextParams(arArg3.toLong());
		else
			apParams = cpSessionMgr->getContextParams(arArg3, NULL);
		if (apParams == NULL)
			aReqId = -AERR_UNKCONTEXTNAME;
		// Returns a DEL-delimited list of pairs.
		else
		{	AUtil::stringMapToString(*apParams, aAisOut);
			aRetValue = apParams->count();
			aReqId = 0;	// Direct return
		}
		break;
	}
	case geGetCurrentContexts:		// _ais|getcurrentcontexts
	{	// Return tab-delimited list of contexts
		aAisOut = cpSessionMgr->getCurrentContexts();
		aReqId = 0;		// Direct return
		break;
	}
	case geGetDirInfo:				// _ais|getdirinfo|dir|%s
		ACHECK(aTknSz == 4);
		aAmpReq = arByteArray3;
		aEvalType = SBGLUE_DIRINFO;
		break;

	case geGetExeSession:			// _ais|getexesession|context|%s
		ACHECK(aTknSz >= 2);
		if ((aReqId = cpSessionMgr->getExeSession(iSessionId, arArg3)) >= 0)
		{	aRetValue = aReqId;		// Holds sessionId
			aAisOut = QString::number(aRetValue);
			aReqId = 0;	// Direct return
		}
		break;

	case geGetExtentNames:			// _ais|getextentnames
		aAmpReq = "(browseLib.delimitedString (browseLib.getExtentNames) #\\tab #\\tab)";
		break;

	case geGetExtentStatus:			// _ais|getextentstatus
		aAmpReq = "(browseLib.getExtentStatus)";
		break;

	case geGetExtentTypes:			// _ais|getextenttypes|extent|%s
		ACHECK(aTknSz == 4);
		aAmpReq = "(browseLib.getTypes |" + arByteArray3 + "|:)";
		break;
	
	case geGetMetaData:			// _ais|getmetadata|extent|%s
		aAmpReq = "(browseLib.getMetaData {" + arByteArray3 + "})";
		break;

	case geGetNextLevel:			// _ais|getnextlevel|extent|%s|node|%s|options|%s
		ACHECK(aTknSz == 6 || aTknSz == 8);
		if (aTknSz >= 6)
		{	aAmpReq = "(browseLib.getNextLevel |" + arByteArray3 + "|: |" + aTkns[5].toLatin1() + "|: 0 99999 ";
			if (aTknSz == 8)
				aAmpReq += aTkns[7].toLatin1();
			aAmpReq += ")";
		}
		break;

	case geGetSessionUser:			// _ais|getsessionuser|sessionid|%d
		if ((aSessionId = (aTknSz >= 4)? arArg3.toLong() : iSessionId) <= 0)
			aReqId = -AERR_SESSIONID;
		else if ((aReqId = cpSessionMgr->getSessionUser(aSessionId)) > 0)
		{	aRetValue = aReqId;		// Holds UsrId
			aAisOut = QString::number(aRetValue);
			aReqId = 0;	// Direct return
		}
		break;

	case geGetWorkspaceStatistics:
		aAmpReq =  "(browseLib.delimitedString (browseLib.inspect stats:) { = } #\\newline)";
		break;

	case geImportCabinet:			// _ais|importcabinet|cabinet|%s|file|%s|setlocation|%s
	                                // _ais|importcabinet|cabinet|%s|folder|%s|setlocation|%s
		ACHECK(aTknSz == 8);	
		aAmpReq = "((lambda()vars:(saveFocus ret savepath)(setq savepath _path)(setq saveFocus (browseLib.getFocus))"
		"(browseLib.setFocus |" + arByteArray3 + "|:)";
        if( aTkns[4] == "file" )
        {
            aAmpReq += "(setq ret (browseLib.importSource {" + aTkns[5].toLatin1() + "} "+ aTkns[7].toLatin1() +"))";
        }
        else
        {
            aAmpReq += "(setq ret (browseLib.importDirectory {" + aTkns[5].toLatin1() + "} "+ aTkns[7].toLatin1() +"))";
        }
        aAmpReq += "(browseLib.setFocus saveFocus)(setq _path savepath) (return ret)))";
		break;

	case geIsContextBusy:			// _ais|iscontextbusy|context|%s
		ACHECK(aTknSz >= 2);
		if ((aReqId = cpSessionMgr->isContextBusy(iSessionId, arArg3)) >= 0)
		{	aRetValue = aReqId;		// Returns sessionId or 0 if not busy
			aAisOut = QString::number(aRetValue);
			aReqId = 0;	// Direct return
		}
		break;

	case geIsContextOpen:			// _ais|iscontextopen|context|%s
		ACHECK(aTknSz == 4);
		if (cpSessionMgr->isContextOpen(arArg3))
			aRetValue = 1, aAisOut = "true";
		else
			aRetValue = 0, aAisOut = "false";
		aReqId = 0;		// Direct return
		break;

	case geNewCabinet:				// _ais|newcabinet|cabinet|%s|path|%s|location|%s|storage|%s|importsync|%s|exportsync|%s|autocompile|%s|searchsw|%s|dependencies|%s
		ACHECK(aTknSz == 20);
		aAmpReq = "((lambda()vars:(savepath)(if ((browseLib.fileExists {" + aTkns[5].toLatin1()
		+ "}))(return existing:))(setq savepath _path)(setq _path {})(browseLib.addExtent {" + arByteArray3 + "} {" + aTkns[5].toLatin1() + "} {" + aTkns[7].toLatin1()
		+ "} {" + aTkns[9].toLatin1() + "} {" + aTkns[11].toLatin1() + "} {" + aTkns[13].toLatin1() + "} {" + aTkns[15].toLatin1()
		+ "} {" + aTkns[17].toLatin1() + "} {" + aTkns[19].toLatin1()
		+ "})(setq _path savepath)true))";
		break;

	case geNoop:					// _ais|noop|args...
		// Return  session id and the args sent
		for (long i = 2; i < aTknSz; ++i)
		{	aAisOut += aTkns[i++];
			if (i < aTknSz)
				aAisOut += '=' + aTkns[i];
			aAisOut += ',';
		}
		aAisOut += "sessionid=" + QString::number(iSessionId);
		aRetValue = iSessionId;
		aReqId = 0;		// Direct return
		break;

	case geOpenCabinet:				// _ais|opencabinet|cabinet|%s|path|%s|location|%s|storage|%s|importsync|%s|exportsync|%s|autocompile|%s|searchsw|%s|dependencies|%s
		ACHECK(aTknSz == 20);
		if (aTknSz < 20)
			aReqId = -AERR_FEWARGS;
		else
		{
			aAmpReq = "((lambda()vars:(savepath)(if (not (browseLib.fileExists {" + aTkns[5].toLatin1()
			+ "}))(return notfound:))(setq savepath _path)(setq _path {})(browseLib.addExtent {" + arByteArray3 + "} {" + aTkns[5].toLatin1() + "} {" + aTkns[7].toLatin1()
			+ "} {" + aTkns[9].toLatin1() + "} {" + aTkns[11].toLatin1() + "} {" + aTkns[13].toLatin1() + "} {" + aTkns[15].toLatin1()
			+ "} {" + aTkns[17].toLatin1() + "} {" + aTkns[19].toLatin1()
			+ "})(setq _path savepath)true))";
		}
		break;

	case geOpenContext:			// _ais|opencontext|context|%s|path|%s
		// Extract contextname and path arguments
		ACHECK(aTknSz >= 4);
		aAisOut = "false";
		if (aTknSz < 4)
			aReqId = -AERR_FEWARGS;
		else
		{	QString aContextName, aStartupPath;
			if (aTkns[2] == "context")
			{	aContextName = arArg3;
				if (aTknSz >= 6 && aTkns[4] == "path")
					aStartupPath = aTkns[5];
			}
			else
				aStartupPath = arArg3;
				
			// openContext returns error code. Display holds port status msgs.
			// openContext returns: error code (<0) or contextId (>0)
			if ((aReqId = cpSessionMgr->openContext(aStartupPath, aContextName, aDisplay)) >= 0)
			{	aAisOut = aContextName;
				aReqId = 0;	// Direct return
			}
		}
		break;

	case geOpenNode:			// _ais|opennode|extent|%s|node|%s
		ACHECK(aTknSz == 6);	
		aAmpReq = "(browseLib.checkout |" + arByteArray3 + "|: |" + aTkns[5].toLatin1() + "|:)";
		break;

	case geRegisterContext:		// _ais|registercontext|path|%s
		ACHECK(aTknSz == 4);
		// Return ContextName in aAisOut, messages in aDisplay. ReqId is 0.
		aReqId = cpSessionMgr->registerContext(aTkns[3], aAisOut, aDisplay);
		break;
	
	case geRegisterDirectory:				// _ais|registerdirectory|databasedir|%s|sourcedir|%s
		ACHECK(aTknSz == 6);
		if( aTkns[5] != "" )
			aAmpReq = "(browseLib.registerDirectory {" + arByteArray3 + "} {" + aTkns[5].toLatin1() + "})";
		else
			aAmpReq = "(browseLib.registerDirectory {" + arByteArray3 + "})";
		break;

	case geRunScriptFile:		// _ais|runscriptfile|file|%s|prefix|%s		
	{	ACHECK(aTknSz >= 4);
		aNoDebug = SBGLUE_WITH_DEBUG;

		// Script File Name.
		// Convert placeholder to the startup script file name
		QByteArray aStartupScriptName(arArg3.toLatin1());
		if (arArg3.isEmpty() || arArg3 == ASMGR_STARTUPSCRIPT)
		{	const QString& arContextName = cpSessionMgr->getContextName(iSessionId, 0/*ContextId*/);
			aStartupScriptName = cpSessionMgr->getStartupScriptName(arContextName);
		}
		// Prepend a prefix Lisp expression, if any, to the runScript command. (Use to set debug mode)
		if (aTknSz > 5)
			aAmpReq = aTkns[5].toLatin1();
        aAmpReq += "(runScript {";
		aAmpReq += aStartupScriptName;
		aAmpReq += "})";
		break;
	}
	case geSaveNode:			// _ais|savenode|extent|%s|node|%s|text|%s
	{	ACHECK(aTknSz >= 8);
		QByteArray aExp(irAmpmsg.section('\177', 7).toLatin1());	// Everything after text
		aAmpReq = arByteArray3 + '\177' + aTkns[5].toLatin1() + '\177' + aExp;
		aEvalType = SBGLUE_CHECKIN;
		break;
	}
	case geSetBreakpoint:		// _ais|setbreakpoint|Lambda|%s
		ACHECK(aTknSz == 4);
		aAmpReq = "(debug {" + arByteArray3 + "})";
		break;

	case geSetEngineFlags:		// _ais|setengineflags|flags|%d
	{	ACHECK(aTknSz == 4);
		unsigned aFlags = arArg3.toInt();
		cpSessionMgr->setEngineFlags(iSessionId, aFlags);
		aAisOut = "true";
		aRetValue = aFlags;
		aReqId = 0;		// Direct return
		break;
	}
	case geSetErrorTrace:		// _ais|seterrortrace|onoff|%d
	{	ACHECK(aTknSz == 4);
		bool aOnoff = (aRetValue = arArg3.toInt());
		aReqId = cpSessionMgr->errorTrace(iSessionId, aOnoff);
		aAisOut = (aReqId >= 0) ? "true" : "false";
		break;
	}
	case geSetEscape:			// _ais|setescape|sessionid|%d
		ACHECK(aTknSz == 4);
		if ((aRetValue = arArg3.toLong()) > 0)	// Session to be stopped
			aReqId = cpSessionMgr->setEscape(aRetValue);
		else
			aReqId = 0;
		aAisOut = (aReqId >= 0) ? "" : "false";
		break;
	
	case geSetInstructionTrace:	// _ais|setinstructiontrace|onoff|%d
	{	ACHECK(aTknSz == 4);
		bool aOnoff = (aRetValue = arArg3.toLong());
		aReqId = cpSessionMgr->instructionTrace(iSessionId, aOnoff);	// Direct return
		aAisOut = (aReqId >= 0) ? "true" : "false";
		break;
	}
	case geSetJit:				// _ais|setjit|onoff|%d
	{	ACHECK(aTknSz == 4);
		bool aOnoff = (aRetValue = arArg3.toInt());
		aReqId = cpSessionMgr->jit(iSessionId, aOnoff);	// Direct return
		aAisOut = (aReqId >= 0) ? "true" : "false";
		break;
	}
	case geSetSysCheck:			// _ais|setsyscheck|onoff|%d
	{	ACHECK(aTknSz == 4);
		bool aOnoff = (aRetValue = arArg3.toInt());
		aReqId = cpSessionMgr->sysCheck(iSessionId, aOnoff);	// Direct return
		aAisOut = (aReqId >= 0) ? "true" : "false";
		break;
	}
	case geShowConsole:			// _ais|showconsole|exp|%s
		ACHECK(aTknSz == 4);
		aAmpReq = "(writeln (eval {" + arByteArray3 + "}))";
		break;

	case geUpdateMetadata:				// _ais|updatemetadata|cabinet|%s|path|%s|location|%s|storage|%s|importsync|%s|exportsync|%s|autocompile|%s|searchsw|%s|dependencies|%s
		ACHECK(aTknSz == 20);	
		aAmpReq = "(browseLib.updateMetaData {" + arByteArray3 + "} {" + aTkns[5].toLatin1() + "} {" + aTkns[7].toLatin1()
		+ "} {" + aTkns[9].toLatin1() + "} {" + aTkns[11].toLatin1() + "} {" + aTkns[13].toLatin1() + "} {" + aTkns[15].toLatin1()
		+ "} {" + aTkns[17].toLatin1() + "} {" + aTkns[19].toLatin1()
		+ "})";
		break;

	// These req types never appear as inputs to AisMgr. Only used for output processing.
	// case geFcnDebug:
	// case geFcnEngineState:
	// case geFcnError:
	// case geFcnFileOpen:
	// case geFcnReadHtmlPage:
	// case geFcnReturnResult:
	// case geFcnRingBell:
	// case geFcnSendToClient:
	// case geFcnSendStatusToClient:
	// case geUnknown
	// case geRetFile:
	// case geRetQuery:
	// case geRetText:
	// case geRetUrl:
	default:
		aReqId = -AERR_UNKACT;
		break;
	}
	// Record request if aReqId > 0.  Submit this request iff AmpReq is not empty.
	if (aReqId > 0)
	{	// Submit AMP Request to engine. If an error returned here, no pending request is generated.
		if (!aAmpReq.isEmpty())
		{	aReqId = cpSessionMgr->submit(iSessionId, aEvalType, aNoDebug, aAmpReq, QDateTime::currentDateTime(), ipData);
			ipData = NULL;
		}
		if (aReqId > 0)
		{	++cPendingReqs;
			APendingReq aPendReq;
			aPendReq.mAmpmsg = irAmpmsg.left(256);
			aPendReq.mIsAsync = iIsAsync;
			aPendReq.mReqType = aReqType;
			aPendReq.mRqId = iRqId;
			cPendingRequests[aReqId] = aPendReq;
		}
	}
	lRetError:
	// Clean up.
	if (ipData != NULL)
	{	free(ipData);
		qDebug("AAisMgr::submit(SessionId:%ld, RqId:%ld, Ampmsg:%s, IsAsync:%d), Data dropped.", iSessionId, iRqId
		, irAmpmsg.toAscii().data(), iIsAsync);
	}

	// Return immediate results and errors back to caller who knows where to return msgs.
	if (aReqId <= 0)
	{	// Format error messages
		if (aReqId < 0)
		{	QString aErr;
			aRetValue = 0;
			const char *apErrMsg = gAis.mpErrMsgs[-aReqId];
			aErr.sprintf("AAisMgr::submit(SessionId:%ld, Ampmsg:%s), %s", iSessionId, irAmpmsg.toLatin1().data(), apErrMsg);
			LOGSYSMSG(geWarn, aErr);
			aAisOut = "false";
		}
		if (opRetValue != NULL)
			*opRetValue = aRetValue;
		if (orAisOut != QString::null)
			orAisOut = aAisOut;
		if (orDisplay != QString::null)
			orDisplay = aDisplay;
	}
	return aReqId;				// 0 if return result, >0 if pending req, <0 if error.
}

// setAisSvr is in aismgr.h
