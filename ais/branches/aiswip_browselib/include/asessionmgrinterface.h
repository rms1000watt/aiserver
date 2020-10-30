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

#ifndef ASESSIONMGRINTERFACE_H
#define ASESSIONMGRINTERFACE_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/asessionmgr/asessionmgrinterface.h
														Session Manager

Sessionmgrinterface.h constitutes the external interface of the session manager
for host applications like ampmgr and the ide.

CHANGE HISTORY
Version	Date		Who		Change
4.002	 9/28/2008	tlw		CR128. Add subscribe to allow a connection subscribe to a session.
3.2005	 2/18/2008	fchua	Added getRequestStats, getSessionStats.
1.0118	12/12/2006	tlw		submit. Add serial binary stream argument.
1.0107	 9/19/2006	tlw		Add i prefix to selected arguments.
1.0057	 3/18/2005	tlw		Update documentation.
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QDateTime>
#include "aglobals.h"
extern "C" { // includes for modules written in C
	#include "fsmtbase.h" // SmartBase engine declarations
	}

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
class AAisMgr;
class AAisSvr;
class AUsrMgr;
#define AINITIALSESSIONS			64
#define SMDISPLAY					1
#define SMRETURNRESULT				2
#define	SMDEBUG						3
#define SMMAXSECLEVEL				7

// Error Codes See also aerrdefs.h
#define AOK	0
#define AUNKNOWN_CONTEXTNAME		-28		//-200
#define ACONTEXTNAME_ALREADY_OPEN	-29		// -201
#define ACONTEXT_OPEN_FAILED		-30		// -202
#define ASESSION_NOT_ACTIVE			-31		// -203
#define ABAD_SESSIONID				-32		// -204
#define ASESSION_REQUESTS_PENDING	-33		// -205
#define ASESSION_CLOSED				-34		// -206
#define	ASECURITY_VIOLATION			-35		// -207
#define ASESSION_OPEN				-36		// -208
#define ACONTEXT_NOT_ACTIVE			-37		// -209
#define ATIMEOUT					-64		// AERR_TIMEOUT

typedef	long		ASessionID;
typedef	long		ARequestID;
typedef long		AContextID;
typedef long		AErrorCode;
typedef long		ARequestType;
typedef QMap<QString, QString> AStringMap;

enum ASessionType {
	 geUserSession		// User sessions have connections through Ais servers to remote clients
	,geAdminSession		// Each context has a single admin session that ais uses to execute Lambdas in context. Remote clients
						// may not use the admin session.
};

//	------------------------------------------------------ CLASSES ------------------------------------------------------------
class ASessionMgrInterface
{
public:
	// Use cancel to cancel a request submitted to the context. A currently executing request cannot be cancelled.
	virtual AErrorCode cancel(ASessionID iSessionId, ARequestID iRequestId) = 0;

	// Use cancelAll to cancel all requests currently in the sessions request queue.
	virtual AErrorCode cancelAll(ASessionID iSessionId) = 0;

	// closeConnection - called by a protocol server when a connection is unexpectedly closed.
	// If the OnDisconnect parameter specifies an AisLisp Lambda, it is called.
	virtual ARequestID closeConnection(ASessionID iSessionId) = 0;

	// Use closeContext to unconditionally close the context. Pending requests will be allowed to continue or terminated
	// depending upon the close mode.  A response returned to the caller when the last request is removed.
	virtual ARequestID closeContext(ASessionID iSessionId, const QString& irContextName, ACloseMode iCloseMode) = 0;

	// Use closeSession to close a session if there are not pending requests for the session.
	virtual ARequestID closeSession(ACloseMode iCloseMode, long iConnectId, ASessionID iSessionId) = 0;

	// Use connectSession to reconnect to a disconnected session. Returns buffered output.
	virtual AErrorCode connectSession(ASessionID iSessionId) = 0;
	virtual AErrorCode enableConsoleLog(ASessionID iSessionId, long iEnable) = 0;

	virtual AErrorCode errorTrace(ASessionID iSessionId, bool iOnoff) = 0;
	virtual AErrorCode jit(ASessionID iSessionId, bool iOnoff) = 0;
	virtual AErrorCode setEngineFlags(ASessionID iSessionId, unsigned iFlags) = 0;
	virtual AErrorCode sysCheck(ASessionID iSessionId, bool iOnoff) = 0;
	virtual AErrorCode flushSessions(bool iNow) = 0;

	// Get the logged console output for this context.
	virtual AErrorCode getConsoleLog(ASessionID iSessionId, bool iClear, QString& orOut) = 0;

	// Get the index into open contexts vector given the context name or error code (<0)
	virtual long getContextId(const QString& irContextName) = 0;
	virtual long getContextId(ASessionID iSessionId) = 0;


	// Get the context name for a session. Returns null string if context not found.
	virtual QString getContextName(ASessionID iSessionId, long iContextId = 0) = 0;

	// get a copy of the context parameter(s) given the name or index into the array
	virtual QString getContextParam(const QString& irContextName, const QString& irParameter) = 0;
	virtual AStringMap* getContextParams(const QString& irContextName, long* opContextId=NULL) = 0;
	virtual AStringMap* getContextParams(long iContextId) = 0;

	// getCurrentContexts() returns a list of currently open contexts
	virtual QString getCurrentContexts() = 0;

	// GetExeSession returns the session ID of the currently executing session for a context or -1 if none.
	virtual long getExeSession(ASessionID iSessionId, const QString& irContextName) = 0;

	// GetMainThread returns a pointer to the main thread for use by ASBGlue_ContextClient when creating a new client.
	virtual QThread* getMainThread() = 0;

	// Use getRequestID to get a unique requestID from the engine that is not actually associated with a request. This is
	// useful when the host application wants to maintain a table of outstanding requests and needs to insert placeholders
	// in this table for responsed from the context that are not directly associated with a submitted request. i.e. console
	// and debug callbacks.
	virtual ARequestID getRequestID() = 0;

	// i think this function is misplaced - pete
	// GetSubscriptions returns newline-terminated session records. Each record contains the following tab-separated fields:
	//		SessionId ContextName UserName Admin ProtocolName
	//virtual long getSubscriptions(const QString& irContextName, QStringList& orSessionList) = 0;

	// getRequestStats returns newline-terminated request statistics records. Each record contains the following tab-separated fields:
	//		SessionId PendingRequestCount
	virtual long getRequestStats(QString& orRequestStats) = 0;

	// GetSessions returns newline-terminated session records. Each record contains the following tab-separated fields:
	//		SessionId ContextName UserName Admin ProtocolName
	virtual long getSessions(const QString& irContextName, QStringList& orSessionList) = 0;

	// getSessionStats returns newline-terminated session statistics records. Each record contains the following tab-separated fields:
	//		Username SessionCount
	virtual long getSessionStats(QString& orSessionStats) = 0;

	// getSessionUser just returns the userID associated with a session id
	virtual long getSessionUser(ASessionID iSessionId) = 0;

	// getStartupScriptName returns a the startup script file name set by openContext
	virtual QByteArray getStartupScriptName(const QString& irContextName) = 0;

	// getUserSettings - fetch context-specific user SecLvl and EndDay for this user.
	virtual bool getUsrSettings(const QString& irContextName, long iUsrId, long& orSecLvl, long& orEndDay) = 0;

	virtual AErrorCode instructionTrace(long iSessiondD, bool iOnoff) = 0;

	// isAdminSession - Returns true if iSessionId is the Admin Session for this context
	virtual bool isAdminSession(ASessionID iSessionId) = 0;

	// isContextBusy returns executing sessionId or 0 (<0 if error). Defaults to current context
	virtual long isContextBusy(long iSessionId, const QString& irContextName) = 0;

	// isContextOpen checks if a contest of the specified name is already open
	virtual bool isContextOpen(const QString& irContextName) = 0;
	virtual bool isContextOpen(long iContextId) = 0;
	virtual bool isDisconnected(ASessionID iSessionId) = 0;

	// Start/stop console output logging

	// Notify waiting thread that page requested by readHtmlPage is cancelled or available.
	virtual AErrorCode notifyHtmlPage(long iSessionId, const QByteArray& irPageBfr) = 0;

	// Create new console output log for this session.
	virtual AErrorCode openConsoleLog(bool iClear, bool iRedirect, ASessionID iSessionId, long iSize, bool iStartAtNewline)=0;

	// Establish a smartbase execution environment, but do not load it yet. Returns context name
	virtual ASessionID openContext(const QString& irStartupPath, QString& orContextName, QString& orAisOut) = 0;

	// Each context may have one or more sessions. Usually, a session is associated with a specific user. Context's don't
	// have any intrinsic knowledge of a user so a userID is passed in that they may query and use in applications. Note
	// that the security level of the user is also passed in on this call. The host application is responsible for setting
	// both the userID and security levels for each session. Note that a unique sessionId number is returned from newHost and
	// the host application needs to keep track of this for subsequent calls to session manager.
	virtual ASessionID openSession(ACloseMode iDefMode, const QString& irContextName,long iUserID, long iSecurityLevel,
	long iSessionId, ASessionType iSessionType = geUserSession)=0;

	// Establish a smartbase execution environment, but do not load it yet. Returns context name
	virtual AErrorCode registerContext(const QString& irStartupPath,QString& orContextName,QString&orAisOut,bool isOpen=false)=0;

	// Save output going to a session in a ring file. Called by AisSvr::returnOutput
	virtual bool saveConsoleLog(const QString& irDisplay, long iRqId, long iSessionId, char iType) = 0;

	// Use setEscape to ask the engine to stop processing any currently executing request.
	//TM! this is goofy. I should not have made this specific to a session or if I really want this I have to figure out the
	// semantics. Maybe we should have a setResume(ASessionID) as well and then this might make more sense. We might also want
	// to use setEscape to halt the engine regardless of what session is executing.
	virtual AErrorCode setEscape(ASessionID iSessionId) = 0;

	// Set the pointer to the calling module for call back support
	virtual AErrorCode setAisMgr(QObject* ipAisMgr, AAisSvr* ipAisSvr) = 0;

	// Subscribe to all the output being sent to iSessionID
	virtual AErrorCode subscribe(long iCurSessionID, long iSessionID, bool iRemove) = 0;

	// Use submit to pass a request through to the context. A requestString must have one of the following forms:
	//	 "_context\del<lispstring>"  where <lispstring> is any valid lisp expression
	//	 "_debug\del<debugcmd"		where <debugcmd> is any valid debug expression
	//	 AmpCommand					See AMP documentation
	// Note that the \del above is character \177. Responses from the submit request will return async to the host application
	// through the callback member functions in session manager.
	virtual ARequestID submit(ASessionID,long iEvalType,long iNoDebug,const QByteArray& irCmdStg,QDateTime iDateTime,char* ipData)=0;
	virtual bool initMemoryMgr(NUM iMinimumMemory) = 0;

	// Initializes the Embedded MySQL facility
	virtual bool initEmbeddedMySQL(QString &orAisOut) = 0;
};
#endif // ASESSIONMGRINTERFACE_H
