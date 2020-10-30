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
aisdev/asessionmgr/acontextclient.cpp
													Context Client

CHANGE HISTORY
Version	Date		Who		Change
2.0001	1/23/2007	tmay	modified postMessage to pass along binary buffer on gpSessionManager->submit
							modified postMessage to free buffer when it is not passed on (see aFreeBufferData logic).
							Note: postMessage: geFullNotify & geWait result in a copy of the binary buffer being made
							as we are sending the information in two directions. Make a note of this in the docs.
							Removed the write member function.
2.0001	12/29/2006	tmay	added support for execute
2.0001	12/28/2006	tlw		linux. Cast QByteArray::number args to int to avoid compiler error.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0119	12/18/2006	tlw		createBinaryBuffer, deleteBinaryBuffer. Add routines for inter-context binary transfer
1.0118	12/7/2006	tlw		read, write. Add methods for inter-context binary transfer.
1.0112	10/27/2006	tlw		~AContextClient. Cancel all outstanding requests. Make mutex private
1.0110	10/20/2006	tlw		Move context client to new thread. Add queued signals to submit requests to AppClient.
1.0063	 5/25/2005	tlw		returnMsg(). Avoid null ptr reference if Xid > 0 but no longer valid.
1.0056	 3/18/2005	tlw		Update documentation.
												---------------------------------
DOCUMENTATION
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QTextStream>
#include "asessionmgr.h"
#include "acontextthread.h"
#include "asbglue.h"
#include <malloc.h>
#include "acontextclient.h"
#include "aisoutput.h"
extern ASessionManager *gpSessionManager;

//	------------------------------------------------------ METHODS ------------------------------------------------------------
ATaskIDFactory AContextClient::taskIDFactory;

/*!
\brief AContextClient - constructor creates an instance of AContextClient to connect to another instance of a context.

\param ServerAddress - The IP or DNS address of the server.
\param iPort - The socket that the server listens on for incoming requests.
\param CallingContextId - The Id of the calling context.
 */
AContextClient::AContextClient (const QByteArray& irServerAddress, ushort iPort, long iCallingContextId)
: AAppClient(irServerAddress,iPort,NULL,NULL,"Context Server"),cCallingContextId(iCallingContextId),cServerAddress(irServerAddress)
{
	cpAisMgr = gpSessionManager->getAisMgrPtr();
	cContextName = "";
	cDataOwned = false;
	cSessionId = 0;
	cpData = NULL;
	cSecurityLevel = 0;
	cSystemSessionId = gpSessionManager->getSystemSessionId(iCallingContextId);
	cUserId = 0;

	// Connections
	connect(this,SIGNAL(toCloseContext(AReturnRcvr*, const QString&, long, void*)),this,SLOT(onCloseContext(AReturnRcvr*
	, const QString&, long, void*)), Qt::QueuedConnection);
	connect(this,SIGNAL(toCloseSession(AReturnRcvr*, long, long, void*)),this,SLOT(onCloseSession(AReturnRcvr*, long, long, void*))
	, Qt::QueuedConnection);
	connect(this,SIGNAL(toGetCurrentContexts(AReturnRcvr*,void*)),this,SLOT(onGetCurrentContexts(AReturnRcvr*,void*)),Qt::QueuedConnection);
	connect(this,SIGNAL(toGetSubscriptions(AReturnRcvr*, const QString&, void*)),this,SLOT(onGetSubscriptions(AReturnRcvr*
	, const QString&, void*)), Qt::QueuedConnection);
	connect(this,SIGNAL(toGetSessions(AReturnRcvr*, const QString&, void*)),this,SLOT(onGetSessions(AReturnRcvr*
	, const QString&, void*)), Qt::QueuedConnection);
	connect(this,SIGNAL(toIsContextOpen(AReturnRcvr*, const QString&, void*)),this,SLOT(onIsContextOpen(AReturnRcvr*
	, const QString&, void*)), Qt::QueuedConnection);
	connect(this,SIGNAL(toLogoff(AReturnRcvr*, void*)),this,SLOT(onLogoff(AReturnRcvr*, void*)), Qt::QueuedConnection);
	connect(this,SIGNAL(toLogon(AReturnRcvr*, const QString&, const QString&, void*)),this,SLOT(onLogon(AReturnRcvr*
	, const QString&, const QString&, void*)), Qt::QueuedConnection);
	connect(this,SIGNAL(toOpenConnection(AReturnRcvr*,void*)),this,SLOT(onOpenConnection(AReturnRcvr*,void*)),Qt::QueuedConnection);
	connect(this,SIGNAL(toOpenContext(AReturnRcvr*, const QString&, const QString&, void*)), this, SLOT(onOpenContext(AReturnRcvr*
	, const QString&, const QString&, void*)), Qt::QueuedConnection);
	connect(this,SIGNAL(toOpenSession(AReturnRcvr*, const QString&, void*)),this,SLOT(onOpenSession(AReturnRcvr*, const QString&
	, void*)), Qt::QueuedConnection);
	connect(this,SIGNAL(toSubmit(AReturnRcvr*, const QString&, char*, long, void*)), this, SLOT(onSubmit(AReturnRcvr*
	, const QString&, char*, long, void*)), Qt::QueuedConnection);
	connect(this,SIGNAL(toSubmitBin(AReturnRcvr*, const QString&, char*, long, void*)), this, SLOT(onSubmitBin(AReturnRcvr*
	, const QString&, char*, long, void*)), Qt::QueuedConnection);
}

AContextClient::~AContextClient()
{	
	// Clean up any outstanding remote requests
	cContextClientMutex.lock();
	long aTask, aSz = cClientTasks.size();
    //ATaskId aTaskId;
	AEvalReturnType aEvalReturnType;;
    //AEvalNotifyType aEvalNotifyType;
	AXID aReferenceXid;
	ASessionID aReferenceSessionID;
	for (aTask = 0; aTask < aSz; ++aTask)
	{	AClientTask& arTask = cClientTasks[aTask];
        //aTaskId = arTask.mTaskId;
		aEvalReturnType = arTask.mEvalReturnType;
        //aEvalNotifyType = arTask.mEvalNotifyType;
		aReferenceXid = arTask.mReferenceXid;
		aReferenceSessionID = arTask.mReferenceSessionID;

		// Remove and return remote requests from request queue.
		if (aEvalReturnType == geReturnRemote) 
		{	QString aErr("!Error - Context Client Terminated!");
			gpSessionManager->removeRemoteRequest(aReferenceSessionID, aReferenceXid);
			// returnOutput(aReferenceSessionID, aReferenceXid, 0, AISMGROUT_RETURNRESULT_FORWARDING, 0, tr(""), aErr, tr(""), 0);
		}
	}
	cContextClientMutex.unlock();	
	cClientTasks.clear();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AContextClient()");
#endif
}

/*!
\brief closeContext - Shut down a running context.

\param iReferenceSessionID - The session active in the context making the contextClient.eval call.
\param iReferenceXid -  Request ID when the call to contextClient.eval was made from a context.
\param irContextName - Context to be referenced.
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task
 */
ATaskId AContextClient::closeContext(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irContextName,
AEvalReturnType iEvalReturnType, AEvalNotifyType iEvalNotifyType) 
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId, iReferenceSessionID, iReferenceXid, geSingleRequestTask,
	iEvalReturnType, iEvalNotifyType);
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID, iReferenceXid);
	emit toCloseContext(NULL, irContextName ,geDefault, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief closeSession - Close a session on the remote server

\param iReferenceSessionID - The session active in the context making the contextClient.eval call.
\param iReferenceXid -  Request ID when the call to contextClient.eval was made from a context.
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task
 */
ATaskId AContextClient::closeSession(ASessionID iReferenceSessionID, AXID iReferenceXid, AEvalReturnType iEvalReturnType,
AEvalNotifyType iEvalNotifyType)
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId,iReferenceSessionID,iReferenceXid,geSingleRequestTask,iEvalReturnType,
	iEvalNotifyType);
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID,iReferenceXid);
	emit toCloseSession(NULL, cSessionId, geDefault, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief eval - Evaluate an Lambda in the remote context.

Evaluate a Lambda in the remote context. The return result can be returned in a variety of ways depending on the EvalType passed.
To get here, some Lambda has called (myContextClient.eval) Lambda in a context. This calling context is executing a request on the
behalf of a remote client. Thus, this calling context has a sessionID and requestID. These are the referenceSessionID and 
referenceRequestID values passed to this function.
\param iReferenceSessionID - The session active in the calling context.
\param iReferenceXid -  Request ID in the calling context.
\param irExp - The AMP message to be evaluated
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task

\par Notes:
-# Each call to eval creates a task object. The task object maintain the state of the request submited to the remote context. A
taskID is returned to the calling context. When return results are received and the various messaging options are executed the
information in the task object is used to make sure the results go to the right places. For instance, the EvalType requested
is stored in the task object.
-# The task object also carries a taskType. This enum is usually geSingleRequestType which means that a single request to the
remote context will be performed to complete the actions called for in the AContextClient api. See the logonAndOpenSession
function for an example of a task requiring two requests on the remote context to be performed.
*/
ATaskId AContextClient::eval(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irExp, AEvalReturnType iEvalReturnType,
AEvalNotifyType iEvalNotifyType)
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId,iReferenceSessionID,iReferenceXid,geSingleRequestTask,
	iEvalReturnType, iEvalNotifyType);
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID,iReferenceXid);
	emit toSubmit(NULL, irExp, NULL, 0, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief getCurrentContexts - Get a list of all the contexts available on the remote server

\param iReferenceSessionID - The session active in the context making the contextClient.eval call.
\param iReferenceXid -  Request ID when the call to contextClient.eval was made from a context.
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task
 */
ATaskId AContextClient::getCurrentContexts(ASessionID iReferenceSessionID, AXID iReferenceXid, AEvalReturnType iEvalReturnType,
AEvalNotifyType iEvalNotifyType) 
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId, iReferenceSessionID, iReferenceXid, geSingleRequestTask,
	iEvalReturnType, iEvalNotifyType);
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID, iReferenceXid);
	emit toGetCurrentContexts(NULL, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief getReturnResult - Fetch the returned result after notification of completion of a task

Clear request from queue.
\param iTaskId - The ID of the task that completed
\param ipOk -> Place to put return status.
\return taskCopy - The info regarding this task.
*/
AClientTask AContextClient::getReturnResult(ATaskId iTaskId, bool *ipOk) 
{
	cContextClientMutex.lock();
	*ipOk = false;
	AClientTask aTaskCopy;
	AClientTasks::Iterator apIt = cClientTasks.find(iTaskId);
	if (apIt != cClientTasks.end())
	{	if (apIt.value().mComplete)
		{	*ipOk = true;
			aTaskCopy = apIt.value();
			cClientTasks.remove(iTaskId);
			cContextClientMutex.unlock();
			return aTaskCopy;
		}
		else
		{	cContextClientMutex.unlock();
			return aTaskCopy;
		}
	}
	cContextClientMutex.unlock();
	return aTaskCopy;
}

/*!
\brief getSubscriptions - Get the current subscriptions for this context

\param iReferenceSessionID - The session active in the context making the contextClient.eval call.
\param iReferenceXid -  Request ID when the call to contextClient.eval was made from a context.
\param irContextName - Context to be referenced.
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task
 */
ATaskId AContextClient::getSubscriptions(ASessionID iReferenceSessionID, AXID iReferenceXid,
const QString& irContextName, AEvalReturnType iEvalReturnType, AEvalNotifyType iEvalNotifyType)
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId,iReferenceSessionID,iReferenceXid,geSingleRequestTask,
	iEvalReturnType, iEvalNotifyType);
	cContextName = irContextName;
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID,iReferenceXid);
	emit toGetSubscriptions(NULL, irContextName, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief getSessions - Get the current sessions for this context

\param iReferenceSessionID - The session active in the context making the contextClient.eval call.
\param iReferenceXid -  Request ID when the call to contextClient.eval was made from a context.
\param irContextName - Context to be referenced.
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task
 */
ATaskId AContextClient::getSessions(ASessionID iReferenceSessionID, AXID iReferenceXid,
const QString& irContextName, AEvalReturnType iEvalReturnType, AEvalNotifyType iEvalNotifyType)
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId,iReferenceSessionID,iReferenceXid,geSingleRequestTask,
	iEvalReturnType, iEvalNotifyType);
	cContextName = irContextName;
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID,iReferenceXid);
	emit toGetSessions(NULL, irContextName, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief isContextOpen - Determine if a context has already been opened on the remote server

\param iReferenceSessionID - The session active in the context making the contextClient.eval call.
\param iReferenceXid -  Request ID when the call to contextClient.eval was made from a context.
\param irContextName - Context to be referenced.
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task
 */
ATaskId AContextClient::isContextOpen(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irContextName,
AEvalReturnType iEvalReturnType, AEvalNotifyType iEvalNotifyType)
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId,iReferenceSessionID,iReferenceXid,geSingleRequestTask,iEvalReturnType,iEvalNotifyType);
	cContextName = irContextName;
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID,iReferenceXid);
	emit toIsContextOpen(NULL, irContextName, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief logoff - Logoff and close the current session.

\param iReferenceSessionID - The session active in the context making the contextClient.eval call.
\param iReferenceXid -  Request ID when the call to contextClient.eval was made from a context.
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task
 */
ATaskId AContextClient::logoff(ASessionID iReferenceSessionID, AXID iReferenceXid, AEvalReturnType iEvalReturnType,
AEvalNotifyType iEvalNotifyType) 
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId,iReferenceSessionID,iReferenceXid,geSingleRequestTask,iEvalReturnType,iEvalNotifyType);
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID,iReferenceXid);
	emit toLogoff(NULL, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief logon - Logon to the remote context.

\param iReferenceSessionID - The session active in the context making the contextClient.eval call.
\param iReferenceXid -  Request ID when the call to contextClient.eval was made from a context.
\param irUsrName - User's logon name
\param irPasswd - Password for this user
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task
 */
ATaskId AContextClient::logon(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irUsrName,
const QString& irPasswd, AEvalReturnType iEvalReturnType, AEvalNotifyType iEvalNotifyType) 
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId,iReferenceSessionID,iReferenceXid,geSingleRequestTask,
	iEvalReturnType,iEvalNotifyType);
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID,iReferenceXid);
	emit toLogon(NULL, irUsrName, irPasswd, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief openConnection - Establish a new connection to another instance of a context on a local or remote server

\param iReferenceSessionID - The session active in the context making the contextClient.eval call.
\param iReferenceXid -  Request ID when the call to contextClient.eval was made from a context.
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task
 */
ATaskId AContextClient::openConnection(ASessionID iReferenceSessionID, AXID iReferenceXid, AEvalReturnType iEvalReturnType
, AEvalNotifyType iEvalNotifyType)
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId, iReferenceSessionID, iReferenceXid, geSingleRequestTask, iEvalReturnType
	, iEvalNotifyType);
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID, iReferenceXid);

	emit toOpenConnection(NULL, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief openContext - Start up a new context on the remote server

\param iReferenceSessionID - The session active in the context making the contextClient.eval call.
\param iReferenceXid -  Request ID when the call to contextClient.eval was made from a context.
\param irStartupPath - Path + name of startup script
\param irContextName - Name of context to be opened.
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task
 */
ATaskId AContextClient::openContext(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irStartupPath,
const QString& irContextName, AEvalReturnType iEvalReturnType, AEvalNotifyType iEvalNotifyType) 
{
	ATaskId aTaskId = getNextTaskId();
	QString aContextName(irContextName);
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId,iReferenceSessionID,iReferenceXid,geSingleRequestTask,
	iEvalReturnType,iEvalNotifyType);
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID,iReferenceXid);
	emit toOpenContext(NULL, irStartupPath, aContextName, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief openSession - Open a new session on the remote server

\param iReferenceSessionID - The session active in the context making the contextClient.eval call.
\param iReferenceXid -  Request ID when the call to contextClient.eval was made from a context.
\param irContextName - Context to be referenced.
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task
 */
ATaskId AContextClient::openSession(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irContextName,
AEvalReturnType iEvalReturnType, AEvalNotifyType iEvalNotifyType)
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId, iReferenceSessionID, iReferenceXid, geSingleRequestTask
	, iEvalReturnType, iEvalNotifyType);
	cContextName = irContextName;
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID,iReferenceXid);
	emit toOpenSession(NULL, irContextName, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief postMessage - Send notification message to a remote client with updated status about a task

\param ipTask -> Pending task
\param iXid - The for the request executed on the remote context
\param iReqType - The requesType as returned from the remote context
\param iNumValue - The integer value returned from the task
\param irAisOut - The returned message from the task
\param ipData - binary buffer
\param iDatasize - length of binary buffer
\param irDisplay - The strings from writeln, display, etc
\param irError - Error messages, if any
\param iClientData - Not used
\return void
 */
void AContextClient::postMessage(AClientTask *ipTask, AXID iXid, AReqType iReqType, long iNumValue, QString irAisOut,
    char* ipData, long iDataSize, QString irDisplay, QString irError, QString iClientData)
{
    Q_UNUSED(iXid);
    Q_UNUSED(iReqType);
    Q_UNUSED(iClientData);

	AisMgrOutputEvent* apEv;
	QByteArray aAmpMsg;
	bool aFreeBufferData = true;

	if (ipTask == NULL)
	{ // no task found - assume pushed output not associated with an executing request, ignore for now
	  // Later we may want to utilize this possible communication option for messaging from the remote context
		return;
	}
	// Process Notify Options
	// qDebug("PostMessage: ReferenceSession=%ld ReferenceXid=%ld ReturnType=%d NotifyType=%d ",ipTask->mReferenceSessionID,
	// ipTask->mReferenceXid, ipTask->mEvalReturnType, ipTask->mEvalNotifyType);
	switch (ipTask->mEvalNotifyType)
	{
	case geNoNotify:		// No Notify action necessary
		break;
	case geFullNotify:	// Create notification amp message for context that made the (myContextClient.eval ..) call. This
						// notification will carry the full return response received from the remote context.
						// Create full notification message		
						// Note that contextClient allows multiple Lambdas to establish a "listener" relationship
						// so that they can be notified when a notify comes into the context.
		aAmpMsg = "contextClient\177notify\177TaskID\177" + QByteArray::number((int)ipTask->mTaskId);
		aAmpMsg += "\177ReferenceXid\177" + QByteArray::number((int)ipTask->mReferenceXid);
		aAmpMsg += "\177ReferenceSessionID\177" + QByteArray::number((int)ipTask->mReferenceSessionID);
		aAmpMsg += "\177AisOut\177" + irAisOut + "\177Display\177" + irDisplay;
		aAmpMsg += "\177NumValue\177" + QByteArray::number((int)iNumValue) + "\177Error\177" + irError;

		// Note that we submit on the system session and use an eval type of 5 - EventMessage.  We don't grab the returned
		// Xid because we won't be getting any return response from an EventMessage submission.
		gpSessionManager->submit(cSystemSessionId, SBGLUE_EVENTMSG, true, aAmpMsg, QDateTime::currentDateTime(), ipData);
		aFreeBufferData = false;
		break;
		// Create notification amp message for context that made the (myContextClient.eval ..) call. This brief notification
		// carries just enough information to identify the task that has been completed.
	case geNotify:
		aAmpMsg = "contextClient\177notify\177TaskID\177" + QByteArray::number((int)ipTask->mTaskId);
		aAmpMsg += "\177ReferenceXid\177" + QByteArray::number((int)ipTask->mReferenceXid);
		aAmpMsg += "\177ReferenceSessionID\177" + QByteArray::number((int)ipTask->mReferenceSessionID);
	
		// Note that we submit on the system session and use an eval type of SBGLUE_EVENTMSG. We don't grab the returned Xid
		// because we won't be getting any return response from an EventMessage submission.
		gpSessionManager->submit(cSystemSessionId, SBGLUE_EVENTMSG, true, aAmpMsg, QDateTime::currentDateTime(), NULL/*Data*/);
		break;
	}//End notify options switch

	// Process Return Result Options
	switch (ipTask->mEvalReturnType)
	{
	case geWait: // Store return result in task object so getReturnResult can find it.
		ipTask->mAisOut = irAisOut;		// Text data if any
		if (ipData != NULL)
			{
			if (geFullNotify == ipTask->mEvalNotifyType)
				{ //Make a copy of the binary buffer as we already passed the current buffer on in a notify message
				ipTask->mpData = (char *) malloc(iDataSize);
				memcpy(ipTask->mpData,ipData,iDataSize);
				}
			else
				ipTask->mpData = ipData;		// Binary data if any
			}
		else
			ipTask->mpData = NULL;
		ipTask->mDataSize = iDataSize;
		ipTask->mDisplay = irDisplay;
		ipTask->mNumValue = iNumValue;
		ipTask->mError = irError;
		if (!irDisplay.isEmpty())
		{	QString aDisplay(irDisplay);
			QString aEmpty("");
			apEv = new AisMgrOutputEvent(ipTask->mReferenceSessionID, ipTask->mReferenceXid, 0, aEmpty, 0/*RetValue*/
			, aEmpty, ipData, iDataSize, aDisplay, aEmpty, AISMGROUT_DISPLAY);
			QCoreApplication::postEvent(cpAisMgr,apEv);
		}
		aFreeBufferData = false;
		break;

	case geNoReturn:	// No return result action necessary
		cClientTasks.remove(ipTask->mTaskId);
		gpSessionManager->removeRemoteRequest(ipTask->mReferenceSessionID,ipTask->mReferenceXid);
		break;

	// Post an AisMgrOutputEvent to return result back out to remote client that made the original call. See the farm Lambda
	// for an example.
	case geReturnRemote:
	{	//gpSessionManager->flushSessions();
		QString aEmpty("");
		apEv = new AisMgrOutputEvent(ipTask->mReferenceSessionID, ipTask->mReferenceXid, 0 /*Status*/, aEmpty /*Enctype*/
		, iNumValue, irAisOut, NULL/*Data*/, 0/*DataSize*/, irDisplay, irError, AISMGROUT_RETURNRESULT_FORWARDING);
		QCoreApplication::postEvent(cpAisMgr,apEv);
		cClientTasks.remove(ipTask->mTaskId);
		break;
	}
	}//end return result options switch

	if(aFreeBufferData && ipData != NULL)
		free(ipData);

}


/*!
\brief execute - Fetch a serialized object from a remote context.


execute evaluates an expression in the remote context that results in the return of a serialized object. Note that the serialized object
is a byte vector that can be very large.  The return result can be returned in a variety of ways depending on the EvalType
passed.  In this case, a synchronous call is usually made (Wait, noNotify).  In this case the return result is returned back to the
local context which is suspended until the response is returned.
To get here, some Lambda has called (myContextClient.execute) in a local context. The calling context has a sessionID and requestID.
These are the referenceSessionID and referenceRequestID values passed to this function.
\param iReferenceSessionID - The session active in the calling context.
\param iReferenceXid -  Request ID in the calling context.
\param irExp - The AMP message to be evaluated
\param irData - Serialized object to be operated on by the above expression.
\param iEvalReturnType - geWait , geNoReturn, geReturnRemote
\param iEvalNotifyType - geNoNotify, geFullNotify, geNotify	
\return aTaskId - Unique incrementing integer assigned to this task

\par Notes:
-# Each call to execute creates a task object. The task object maintains the state of the request submitted to the remote context.
A taskID is returned to the calling context. When return results are received and the various messaging options are executed the
information in the task object is used to make sure the results go to the right places. For instance, the EvalType requested
is stored in the task object.
-# The task object also carries a taskType. This enum is usually geSingleRequestType which means that a single request to the
remote context will be performed to complete the actions called for in the AContextClient api. See the logonAndOpenSession
function for an example of a task requiring two requests on the remote context to be performed.
*/
ATaskId AContextClient::execute(ASessionID iReferenceSessionID, AXID iReferenceXid, const QString& irExp, char* ipData, long iDataSize
, AEvalReturnType iEvalReturnType, AEvalNotifyType iEvalNotifyType)
{
	ATaskId aTaskId = getNextTaskId();
	cContextClientMutex.lock();
	cClientTasks[aTaskId] = AClientTask(aTaskId,iReferenceSessionID,iReferenceXid,geSingleRequestTask,
	iEvalReturnType, iEvalNotifyType);
	cContextClientMutex.unlock();
	if (iEvalReturnType == geReturnRemote)
		gpSessionManager->markRequestAsRemote(iReferenceSessionID,iReferenceXid);
	emit toSubmitBin(NULL, irExp, ipData, iDataSize, &cClientTasks[aTaskId]);
	return aTaskId;
}

/*!
\brief returnMsg - Helper file that carries out the final steps of returnOutput

\param iXid - Request ID (incrementing integer) established by submit
\param iStatus - Error code >0 if an error
\param iReqType - Request type (eval, ampMsg, or built-in command)
\param iNumValue - Integer returned from AIS
\param irAisOut - Returned message
\param ipData - binary buffer
\param iDataSize - length of ipData
\param irDisplay - Console output
\param irError - Error details
\param iClientData - Anonymous data submitted with the request
\param iClearRequestQueueItem If true pending request is removed from the queue
\return void
\par Notes:
 -# Returns payload to calling client in an event
 */
void AContextClient::returnMsg(AXID iXid, long iStatus, AReqType iReqType, long iNumValue, const QString& irAisOut
, char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString  iClientData
, bool iClearRequestQueueItem)
{
	bool aTrace = false;
	cContextClientMutex.lock();
	AClientTask *apTask;
	AClientReqSt *apReqSt;
	QString aAisOut = irAisOut;
	QString aDisplay = irDisplay;
	QString aError = irError;

	if (aTrace)
        qDebug("returnMsg: remXid=%ld iStatus=%ld iReqType=%d iNumValue=%ld iAisOut=%s irDisplay=%s irError=%s",
            iXid, iStatus, iReqType, iNumValue, irAisOut.toLatin1().data(), irDisplay.toLatin1().data(), irError.toLatin1().data());
	ATaskType taskType;
	if (iXid > 0 && cClientRequests.contains(iXid))
	{	// get pointer to clientRequest
		apReqSt = &cClientRequests[iXid];
		// get pointer to AClientTask 
		apTask = (AClientTask *)apReqSt->mpCustomData;
	}
	else
		apTask = NULL;

	if (apTask == NULL)
	{ // no task found - assume pushed output not associated with an executing request so just ignore this output
		cContextClientMutex.unlock();
		return;
	}
	else	// task found so get task information required to process based on task type
		taskType = apTask->mTaskType;
	if (aTrace)
		qDebug("returnMsgTaskInfo: taskType=%d aTaskId=%ld refSessionID=%ld refXid=%ld", apTask->mTaskType, apTask->mTaskId
		, apTask->mReferenceSessionID, apTask->mReferenceXid);

	switch(taskType) 
	{
	case geSingleRequestTask:
		switch(iReqType)
		{// Handle pushed output to throw away for ContextClient
		case geDisplay:
		case geFcnEngineState:
		case geLogAll:
		case geLogConsole:
			// throw message away.
			break;
		case geFcnError:
			// TM! this needs to be dealt with.
			break;
		case geOpenContext:
			if (iNumValue >= 0)
			{	aAisOut = tr("true"); // indicate openContext succeeded
				aDisplay = tr("");
 			} else
			{	aAisOut = tr("false"); // indicate openContext failed
				aDisplay = tr("");
			}
			apTask->mComplete = true;
			postMessage(apTask, iXid, iReqType, iNumValue, aAisOut, ipData, iDataSize, aDisplay, aError, iClientData);
			break;
		case geOpenSession:
			cSessionId = iNumValue;
			apTask->mComplete = true;
			postMessage(apTask, iXid, iReqType, iNumValue, aAisOut, ipData, iDataSize, aDisplay, aError, iClientData);
			break;
		case geLogon:
			// Save UserId and SecurityLevel in contextClient instance
			if (iNumValue > 0)
			{	cUserId = iNumValue;
				QStringList aTkns = irAisOut.split('\177', QString::KeepEmptyParts);
				cSecurityLevel = aTkns[0].toLong();
				iNumValue = 0;
				aAisOut = tr("true"); // indicate logon succeeded
				aDisplay = tr("");
 			}
			else
			{	cUserId = -1; // Set to invalid user id
				cSecurityLevel = 0;
				aAisOut = tr("false"); // indicate logon failed
				aDisplay = tr("");
			}
			apTask->mComplete = true;
			postMessage(apTask, iXid, iReqType, iNumValue, aAisOut, ipData, iDataSize, aDisplay, aError, iClientData);
			break;
		case geLogoff:
			// Clear UserId and SecurityLevel in contextClient instance
			cUserId = -1;
			cSecurityLevel = 0;
			aAisOut = (iStatus <= 0) ? "true" : "false";
			apTask->mComplete = true;
			postMessage(apTask, iXid, iReqType, iNumValue, aAisOut, ipData, iDataSize, aDisplay, aError, iClientData);
			break;
		default:
			apTask->mComplete = true;
			postMessage(apTask, iXid, iReqType, iNumValue, aAisOut, ipData, iDataSize, aDisplay, aError, iClientData);
			break;
		}
		break;
	default: // bad task type found
		// Throw away message		
		break;
	}
	if (iClearRequestQueueItem)
		cClientRequests.remove(iXid);
	cContextClientMutex.unlock();
}

// end
