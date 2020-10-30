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

#ifndef ACONTEXTCLIENT_H
#define ACONTEXTCLIENT_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/acontextclient.h
														Context Client

CHANGE HISTORY
Version	Date		Who		Change
2.0001	12/29/2006	tmay	added support for execute
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0119	12/18/2006	tlw		createBinaryBuffer, deleteBinaryBuffer. Add support routines for inter-context binary transfer.
1.0118	12/7/2006	tlw		read, write. Add methods for inter-context binary transfer
1.0112	11/2/2006	tlw		Make cContextClientMutex private.
1.0111	10/26/2006	tlw		Add queued signals to submit requests on main thread
1.0056	 3/18/2005	tlw		Update documentation.
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "asessionmgr.h"
#include "appclient.h"

// geWait - Wait for remote execution to complete. The polling loop is on the local context thread. 
// geNoReturn - No return to client. Return a requestID to local context.
// geReturnRemote -Return a requestID to local context.
// Remote client will receive return result and other output from remote context.
enum AEvalReturnType {geWait, geNoReturn, geReturnRemote};

// geNoNotify - Perform no notification
// geFullNotify	- Create an event message to inform calling context of completion of request and return full result
// geNotify	- Create an event message to inform calling context of completion of request
// Return result will not be included.
enum AEvalNotifyType {geNoNotify, geFullNotify, geNotify};

enum ATaskType {geSingleRequestTask,geLogonAndOpenSessionTask};
typedef long ATaskId;

class AClientTask
{
public:
	AClientTask(){} 

	AClientTask(ATaskId iTaskId, ASessionID iReferenceSessionID, AXID iReferenceXid, ATaskType iTaskType,
	AEvalReturnType iEvalReturnType, AEvalNotifyType iEvalNotifyType) 
	:	mTaskId(iTaskId), mReferenceXid(iReferenceXid), mReferenceSessionID(iReferenceSessionID), mEvalReturnType(iEvalReturnType), 
        mEvalNotifyType(iEvalNotifyType), mTaskType(iTaskType), mError("") 
        { mStep = 0; mComplete = false;}

	ATaskId			mTaskId;			
	AXID			mHeadXid;			// Xid of request. HeadXid is set on the processing of the return result! Not on submission.
	AXID			mReferenceXid;		// RequestID active in calling context
	ASessionID		mReferenceSessionID;// Session active in calling context
	AEvalReturnType	mEvalReturnType;	// See enum def.
	AEvalNotifyType	mEvalNotifyType;	// see enum def
	ATaskType		mTaskType;			// See enum def.
	char *			mpData;				// binary return result
	long			mDataSize;			// length of binary return result
	long			mStep;				// Current step in task process starting at 0
	long			mNumValue;
	QString			mAisOut;
	QString			mDisplay;
	QString			mError;
	bool			mComplete;
	long			mUserId;
	long			mSecurityLevel;
};

typedef QHash<long, AClientTask> AClientTasks;

class ATaskIDFactory
{
public:
	ATaskIDFactory() {cTaskId = 0;}
	ATaskId		getNextTaskId()
	{	cTaskMutex.lock();
		ATaskId aNewTaskId = ++cTaskId;
		cTaskMutex.unlock();
		return aNewTaskId;
	}
private:
	ATaskId		cTaskId;
	QMutex		cTaskMutex;
};

/*!
\brief ContexClient - Implements a client connection to a context from the server itself

Allows a session to talk to another instance of a context on the same server or on another server.
\par Usage.
A typical scenario:
-# Create an instance of AContextClient specifying the server to connect to
-# Perform a logon to connect to server and get a userId and secLevel
-# Perform an openSession to start a session in the destination context.
-# Call eval to send requests to be processed in the destination context.
-# Call logoff to disconnect from the session
-# Destroy the AContextClient instance to sever the connection with the server
 */
class AContextClient : public AAppClient
{
	Q_OBJECT
public:
	AContextClient(const QByteArray& ServerAddress, ushort Port, long CallingContextId);
	~AContextClient();

	void		clearDeadTasks() {};	// Need to figure out how this will work
	ATaskId		closeContext(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irContextName
				, AEvalReturnType iEvalReturnType = geWait, AEvalNotifyType iEvalNotifyType = geNoNotify);
	ATaskId		closeSession(ASessionID iReferenceSessionID, AXID iReferenceXid, AEvalReturnType iEvalReturnType = geWait
				, AEvalNotifyType iEvalNotifyType = geNoNotify);
	char*		createBinaryBuffer(long iSize)
				{	if (cpData != NULL && cDataOwned)
						free(cpData);
					cDataOwned = true;
					cDataSize = iSize;
					cpData = (char*)malloc(iSize + 1);
					*(cpData + iSize) = '\0';		// Null terminate.
					return cpData;
				};
	void		deleteBinaryBuffer(){if (cpData != NULL && cDataOwned){ free(cpData); cpData = NULL, cDataSize = 0
				, cDataOwned = false;}};
	ATaskId		eval(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irExp
				, AEvalReturnType iEvalReturnType = geWait, AEvalNotifyType iEvalNotifyType = geNoNotify);
	ATaskId		execute(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irExp, char* ipData, long iDataSize, AEvalReturnType iEvalReturnType
				= geWait, AEvalNotifyType iEvalNotifyType = geNoNotify);
	long		getClientRequestQueueLength() {return cClientRequests.count();}
	QString		getContextName() {return cContextName;}
	ATaskId		getCurrentContexts(ASessionID iReferenceSessionID, AXID iReferenceXid, AEvalReturnType iEvalReturnType = geWait
				, AEvalNotifyType iEvalNotifyType = geNoNotify);
	AClientTask getReturnResult(ATaskId iTaskId, bool *ipOk);
	QString		getServerAddress() {return cServerAddress;}
	ASessionID	getSessionID() {return cSessionId;}
	ATaskId		getSessions(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irContextName
				, AEvalReturnType iEvalReturnType = geWait, AEvalNotifyType iEvalNotifyType = geNoNotify);
	ATaskId		getSubscriptions(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irContextName
				, AEvalReturnType iEvalReturnType = geWait, AEvalNotifyType iEvalNotifyType = geNoNotify);
	ATaskId		isContextOpen(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irContextName
				, AEvalReturnType iEvalReturnType = geWait, AEvalNotifyType iEvalNotifyType = geNoNotify);
	ATaskId		logon(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irUsrName, const QString& irPasswd
				, AEvalReturnType iEvalReturnType = geWait, AEvalNotifyType iEvalNotifyType = geNoNotify);
	ATaskId		logoff(ASessionID iReferenceSessionID, AXID iReferenceXid, AEvalReturnType iEvalReturnType = geWait
				, AEvalNotifyType iEvalNotifyType = geNoNotify);
	ATaskId		openConnection(ASessionID iReferenceSessionID, AXID iReferenceXid, AEvalReturnType iEvalReturnType = geWait
				, AEvalNotifyType iEvalNotifyType = geNoNotify);
	ATaskId		openContext(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irStartupPath, const QString&
				irContextName, AEvalReturnType iEvalReturnType = geWait, AEvalNotifyType iEvalNotifyType = geNoNotify);
	ATaskId		openSession(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irContextName
				, AEvalReturnType iEvalReturnType = geWait, AEvalNotifyType iEvalNotifyType = geNoNotify);
	void		postMessage(AClientTask *ipTask, AXID iXid, AReqType iReqType, long iNumValue, QString irAisOut, char* ipData
				, long iDataSize, QString irDisplay, QString irError, QString iClientData);

signals:
	void		toCloseContext(AReturnRcvr* ipRcvr, const QString& irContextName, long iMode, void* ipCustomData);
	void		toCloseSession(AReturnRcvr* ipRcvr, long iSessionId, long iMode, void* ipCustomData);
	void		toGetCurrentContexts(AReturnRcvr* ipRcvr, void* ipCustomData);
	void		toGetSessions(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData);
	void		toGetSubscriptions(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData);
	void		toIsContextOpen(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData);
	void		toLogoff(AReturnRcvr* ipRcvr, void* ipCustomData);
	void		toLogon(AReturnRcvr* ipRcvr, const QString& irUsrName, const QString& irPasswd, void* ipCustomData);
	void		toOpenConnection(AReturnRcvr* ipRcvr, void* ipCustomData);
	void		toOpenContext(AReturnRcvr* ipRcvr, const QString& irStartupPath,const QString& irContextName,void* ipCustomData);
	void		toOpenSession(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData);
	void		toSubmit(AReturnRcvr* ipRcvr, const QString& irExp, char* ipData, long iDataSize, void* ipCustomData);
	void		toSubmitBin(AReturnRcvr* ipRcvr, const QString& irExp, char* ipData, long iDataSize, void* ipCustomData);

protected:
	virtual void returnMsg(AXID iXid, long iStatus, AReqType iReqType, long iRetValue, const QString& irAisOut, char* ipData
				 , long iDataSize, const QString& irDisplay,const QString& irError,const QString  iClientData
				 , bool aClearRequesQueueItem = true);

private:
	ATaskId		getNextTaskId() { return AContextClient::taskIDFactory.getNextTaskId();}
	static ATaskIDFactory	taskIDFactory;

	QObject*	cpAisMgr;			// Used to post return result messages to remote clients
	long		cCallingContextId;	// ContextId of the calling context
	AClientTasks cClientTasks;		// List of tasks 
	QString		cContextName;		// Name of the context being called
	QMutex		cContextClientMutex;// Mutex to synchronize access to shared variables.
	char*		cpData;				// -> Binary buffer for object closure transfer between contexts
	bool		cDataOwned;			// True iff cpData owned by the current process.
	long		cDataSize;			// Size of binary buffer in bytes.
	long		cSecurityLevel;		// Security level established by logon.
	QString		cServerAddress;		// Url or IP address of remote server.
	long		cSessionId;			// SessionId on the context being called.
	long		cSystemSessionId;	// Each context has a system session. We need to know what that is.
	long		cUserId;
};

#endif		// ACONTEXTCLIENT_H
