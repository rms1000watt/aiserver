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

#ifndef ASESSIONMGR_H
#define ASESSIONMGR_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/asessionmgr/asessionmgr.h
														Session Manager

CHANGE HISTORY
Version	Date		Who		Change
5.2000	2/27/2013	mfk		Add support for large grain multitasking.
4.002	 9/28/2008	tlw		CR128. Add subscribe to allow a connection subscribe to a session.
3.2005	 2/18/2008	fchua	Added getRequestStats, getSessionStats.
3.1005	12/16/2007	fchua	Changed QList implementation of keeping sessions to QHash.
3.1005	12/16/2007	fchua	Added cleanupSession method.
3.1005	12/16/2007	fchua	Renamed mpRingFile to mpDisconnectedBuffer.
2.0001	1/5/2006	tlw		cpData. Remove superflous private member.
1.0120	12/19/2006	tlw		cbReturnResult. Add ipData and iDataSize arguments.
1.0118	12/12/2006	tlw		submit. Add serial binary stream argument. ARequest. Add mpData property.
1.0116	12/4/2006	tlw		quit. Add quit to allow early shutdown of contexts.
1.0112	10/27/2006	tlw		ASessionManager. Convert cContextMap, mUsrMap to QHash. Move cSessionMgrMutex to private.
1.0110	10/20/2006	tlw		getMainThread. For use when moving AContextClient to main thread
1.0107	 9/19/2006	tlw		Add i prefix to selected arguments.
1.0057	 3/18/2005	tlw		Update documentation.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QMap>
#include <QtCore/QMutex>
#include <QtCore/QObject>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QThread>
#include <QtCore/QDateTime>
#include "asessionmgrinterface.h"
extern "C" { // includes for modules written in C
	#include "fsmtbase.h" // SmartBase engine declarations
	}

// Forward declarations
class AAisSvr;
class AContextThread;
class AHttpClient;
class ARingFile;
class ASessionManager;
class ASession;
class AUsrMgr;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
#define MAXCONTEXTALLOCATIONBLOCKS	20
#define DEFAULTUSRID				1
#define CLOSEMODES					6
#define CONTEXTTYPECODE				101010101

//	------------------------------------------------------ CLASSES ------------------------------------------------------------
// Classes local to this file:
class ARequest
{
public:
	ARequest(long iSessionID,long iRequestID,long iEvalType,long iNoDebug,QByteArray iCmdString,char* ipData,QDateTime iWhenToRun)
	: mCmdString(iCmdString), mpData(ipData), mExecuting(false), mRequestID(iRequestID), mEvalType(iEvalType),mNoDebug(iNoDebug)
	, mSessionID(iSessionID), mWhenToRun(iWhenToRun)
	{ mRemoteRequest = false; }
	QByteArray	mCmdString;
	char*		mpData;
	bool		mExecuting;
	ARequestID	mRequestID;
	ARequestType mEvalType;	// 0=cmdString, 1=messageBlock, 3=debug, 4=fileOpenSvrSide, 5=EventMessage
	long		mNoDebug;	// 1=execute request ignoring user specified debug flags
	long		mSessionID;
	QDateTime	mWhenToRun;	// Wait until specified time to run request.
	bool		mRemoteRequest;
};

typedef QList<ARequest*> ARequestList;

// AUserSt - Structure containing user settings for one user.  These structures held for each context in
// the UserMap
typedef struct
{	long			mSecLvl;		// Updated security level for this user. (-1 if blank)
	long			mEndDay;		// Updated days from 1/1/2000 until account expires
} AUserSt;
typedef QHash<long, AUserSt> AUsrMap;// Key UsrId, Value: User's security settings

// This class is referenced only in this file and in acontextthread.cpp so it need not be
// in a separate header file
typedef struct
{	long			mType;			// Context type code = CONTEXTTYPECODE.
	long			mAdminSessionId;// Admin session for this context
	long			mAlive;			// Context state (empty <0, registered =0, opened >0)
	void*			mpCP;			// pointer to engine context structure
	long			mContextIndex;	// A numeric index used in sbglue for mutex lock management.
									// See the SBGlue_SetEngineState and SBGlue_Escape functions.
	QString			mContextName;
	AStringMap		mContextParams;	// Dirs, ports, scripts, urls for this context
	char*			mpCodeData;
	long			mCodeDataLen;
	ACloseMode		mDefaultCloseMode; // Default CloseMode.  Values are: geSoft, geFirm, or geHard
	long			mJitMode;		// Default jit mode set from DefaultJitMode context-specific confi parameter.
	long			mExeSessionID;	// SessionID of currently executing session (0 if none)
	NUM				mMemorySize;	// Memory size in bytes.
	long			mObjHdrSize;	// Memory size of object header heap in bytes.
	ACloseMode		mMode;			// Current close state geOpen,  geSoft, geFirm, geHard, geSoftWait,...
	ARequest*		mpRequest;		// Pending context operation
	ARequestList	mRequests;
	QByteArray		mRunScript;		// Script to pass args to speech act
	QString			mStartupScriptName;	// Pending startup script waiting for first access.
	AContextThread*	mpThread;		// Main thread for context
	AUsrMap			mUsrMap;		// Key: UsrId, Value: User's new security settings
} AContextSt;
typedef QList<AContextSt*> AContextVec; // List of ptrs to AContextSt indexed by contextId
typedef	QHash<QString, long> AContextMap;	// Key= ContextName, value = ContextId

// This class is referenced only in this file so it need not be
// in a separate header file.
class ASession
{
public:
	~ASession();
	AContextSt*		mpContext;			// Each session is associated with a single context
	long			mContextID;			// Index into the AContextVec array for this entry
	long			mCurrentRequestID;	// ID of the currently executing request
	QString			mDsplyBfr;			// Buffer display output (writelns)
	QString			mReturnString;		// The return value from the last evaluation (converted to a String)
	bool			mReturnAvailableSW;	// True iff the mReturnString has been set with a return value
	ACloseMode		mDefaultCloseMode;	// Default CloseMode for this session, geSoft, geFirm, geHard.
	unsigned		mEngineFlags;		// SBGLUE_ESCAPE,SBGLUE_ERROR_TRACE,SBGLUE_INSTRUCTION_TRACE,SBGLUE_SYSCHECK,SBGLUE_JIT
	bool			mEngineEscapeSW;	// Large grain multitasking escape switch
	AHttpClient*	mpHttpClient;		// -> instance of HttpClient.
	ACloseMode		mMode;				// Current session state: geOpen, geSoft, geFirm, geHard
	long			mNoDebug;			// Indicates user's engine state flags should be ignored
	QByteArray		mPageContent;		// html page buffer
	bool			mReadHtmlOk;		// html page is now available
	ARequest*		mpRequest;			// Pending session operation
	bool			mRequestHttpOk;		// html page from requestHttp now available
	ARingFile*		mpDisconnectedBuffer; // Buffer to hold disconnected output
	long			mSecurityLevel;		// Security access level of user. 1=sysadmin
	ASessionType	mSessionType;		// Type of session. See enum.
	bool			mStale;				// True iff timer has gone off since DsplyBfr filled.
	long			mUsrID;				// One user per session
	QString			mUsrName;			// Login name of user
};
typedef QHash<ASessionID,ASession*> ASessionTbl;

/*!
\brief ASessionManager - Queue up requests submitted to the engine, manage contexts, process results from the engine

 */
class ASessionManager : public ASessionMgrInterface, QObject
{
public:
	ASessionManager();
	virtual ~ASessionManager();
	virtual bool			event(QEvent* ipEv);

	// The following member functions are what the host interface may call. See
	// sessionmgrinterface.h for a detailed explanation of each call.
	virtual AErrorCode		cancel(ASessionID iSessionId,ARequestID iReqId);
	virtual AErrorCode		cancelAll(ASessionID iSessionId);
	virtual	ARequestID		closeConnection(ASessionID iSessionId);
	virtual ARequestID		closeContext(long iConnectId, const QString& irContextName, ACloseMode iCloseMode = geDefault);
	virtual ARequestID		closeSession(ACloseMode iCloseMode, long iConnectId, ASessionID iSessionId);
	virtual AErrorCode		connectSession(ASessionID iSessionId);
	virtual AErrorCode		enableConsoleLog(ASessionID iSessionId, long iEnable);
	virtual AErrorCode		errorTrace(ASessionID iSessionId, bool iOnoff);
	virtual AErrorCode		flushSessions(bool iNow);
	virtual AErrorCode		getConsoleLog(long iSessionId, bool iClear, QString& orOut);
	virtual long			getContextId(const QString& irContextName);
	virtual long			getContextId(long iSessionId);
	virtual QString			getContextName(long iSessionId, long iContextId = 0);
	virtual QString			getContextParam(const QString& irContextName, const QString& irParameter);
	virtual AStringMap*		getContextParams(const QString& irContextName, long* opContextId = NULL);
	virtual AStringMap*		getContextParams(long iContextId);
	virtual void*			getContextPtr(const QString& irContextName);
	virtual AContextSt*		getContextSt(const QString& irContextName);
	virtual QString			getCurrentContexts();
	virtual long			getExeSession(long iSessionId, const QString& irContextName);
	virtual QThread*		getMainThread();
	virtual ARequestID		getRequestID();
	virtual long			getRequestStats(QString& orRequestStats);
	virtual long			getSessions(const QString& irContextName, QStringList& orSessionList);
	virtual long			getSessionStats(QString& orSessionStats);
	virtual long			getSessionUser(ASessionID iSessionId);
	virtual QByteArray		getStartupScriptName(const QString& irContextName);
	//virtual long			getSubscriptions(const QString& irContextName, QStringList& orSessionList);
	virtual bool			getUsrSettings(const QString& irContextName, long iUsrId, long& orSecLvl,
								 long& orEndDay);
	virtual bool			initMemoryMgr(NUM iMinimumMemory);
	virtual bool			initEmbeddedMySQL(QString &orAisOut);
	virtual bool			isAdminSession(ASessionID iSessionId);
	virtual bool			isDisconnected(ASessionID iSessionId);
	virtual long			isContextBusy(long iSessionId, const QString& irContextName);
	virtual bool			isContextOpen(const QString& irContextName);
	virtual bool			isContextOpen(long iContextId);
	virtual AErrorCode		instructionTrace(long iSessionID, bool iOnoff);
	virtual AErrorCode		jit(ASessionID iSessionId, bool iOnOff);
	// Used Only by ContextClient. See ContextClient for details.
	virtual bool			markRequestAsRemote(ASessionID iSessionId, ARequestID iReqId);
	virtual AErrorCode		notifyHtmlPage(long iSessionId, const QByteArray& irPageBfr);
	virtual AErrorCode		openConsoleLog(bool iClear, bool iRedirect, long iSessionId, long iSize, bool iStartAtNewline);
	virtual ASessionID		openContext(const QString& irStartupPath, QString& orContextName, QString& orAisOut);
	virtual ASessionID		openSession(ACloseMode iDefMode, const QString& irContextName, long iUserID, long iSecurityLevel
							, long iSessionId, ASessionType iSessionType = geUserSession);
	virtual AErrorCode		registerContext(const QString& irStartupPath, QString& orContextName, QString& orAisOut
							, bool isOpen=false);
	// Used Only by ContextClient. See ContextClient for details.
	virtual bool			removeRemoteRequest(ASessionID iSessionId, ARequestID iReqId);
	virtual bool			saveConsoleLog(const QString& irDisplay, ARequestID iRqId, ASessionID iSessionId, char iType);
	virtual AErrorCode		setEngineFlags(long iSessionId, unsigned iFlags);
	virtual AErrorCode		setEscape(long iSessionId);
	virtual AErrorCode		setAisMgr(QObject* ipAisMgr, AAisSvr* ipAisSvr);
	virtual ARequestID		submit(long iSessionID, long iEvalType, long iNoDebug, const QByteArray& irCmdStg, QDateTime iDateTime
							, char* ipData);
	virtual AErrorCode		subscribe(long iCurSessionID, long iSessionID, bool iRemove);
	virtual AErrorCode		sysCheck(ASessionID iSessionId, bool iOnOff);

	// Utility functions
	QObject* getAisMgrPtr() { return(cpAisMgr);};	// Return a ponter to the AisMgr instance
	long getSystemSessionId(long contextId); // Return the sessionid of the system session for specified context


	// Callback functions - may be called on an engine thread.
	long		cbCancelReadHtmlPage(long iSessionId);
	long		cbCancelRequestHttp(long iSessionId);
	long		cbDebug(long iSessionId, long iReqID, QString& irBuf);
	long		cbDisplay(long iSessionId, QString iDisplay);
	long		cbReadHtmlPage(long iSessionId, QString& irUrl, long iMsecToWait);
	long		cbRequestHttp(long iSessionId, QString& irUrl, QString& irFileName, QString& irBody, long iMsecToWait);
	long		cbReturnResult(long iSessionId, long iReqID, const char* ipError, const char* ipEnctype
				, const char* ipText, char *ipData, long iDataSize, bool iForwarding = false);
	long		cbRingBell(long iSessionId, long iReqID);
	long		cbSendToClient(long iSessionId, QString& irMsg);
	long		cbSendStatusUpdateToClient(long iSessionId, QString& irMsg);
	long		cbServerFileOpenDialog(long iSessionId, long iReqID);
	long		cbSendEngineStateToSessions(long iSessionId, long iEngineState);
	void		quit();

	// Aismgr should never call the following functions. These functions are only visible because
	// the asbglue functions may call them.
	ASession*	getSession(long iSessionId);
	QString		getResult(long iSessionId);
	QString		setResult(long iSessionId, QString &irBfr);
	QString		getDisplay(long iSessionId);
	long		bfrDisplayOutput(long iSessionId, QString &irBfr);
	long		clearHtmlPage(long iSessionId);
	long		clearRequestHttp(long iSessionId);
	void		clearRequests(long iSessionId);
	long		debug(long,	char*, long,	char*, long);
	bool		setSessionEscapeFlag(long iSessionID, bool flag);
	unsigned	flags(long iSessionID, long iCurrentState);
	bool		isContextAlive(AContextSt*);
	bool		isEmbeddedMySQLEnabled();
	bool		isPending(long iSessionId);
	long		notifyRequestHttp(long iSessionId, QByteArray& irPageBfr);
	void		processNextRequest(AContextSt*);
	long		readHtmlPage(long iSessionId,char* ipUrl,const char** ipPage,long timeToLive);
	long		requestHttp(long iSessionId, char* ipUrl, char* ipBody, char* ipFileName
				, const char** ipPage, long timeToLive);
	long		serverFileOpenDialog(ASessionID, ARequestID, char* opFileNameBuffer, long iBufLen);
	bool		updateUsrMap(AContextSt* ipContext, const QString& arAppUsersFile);

private:
	QObject*			cpAisMgr;		// Used by callback functions
	AAisSvr*			cpAisSvr;		// Used to open ports, return stale display output.
	AContextMap			cContextMap;	// Key= ContextName, value = ContextId
	QThread*			cpMainThread;	// Main thread used by getMainThread
	QString				cMt;			// Empty string arg placeholder
	AContextVec			cOpenContexts;	// QPtrVector holding AContextSt
	QMutex				cSessionMgrMutex;
	ASessionTbl			cSessions;		// ASessionTbl holding ASession.
	ARequestID			cNextRequestID;
	QMap<QString,long>	cBuiltInCmds;	// key= cmd string, value = request type
	char *				cContextMemory[MAXCONTEXTALLOCATIONBLOCKS];	// memory allocated for contexts with malloc
	bool				cEmbeddedMySQLEnabled;

	long		cleanupSession(ASessionID iSessionId);
};

#endif // ASESSIONMGR_H

