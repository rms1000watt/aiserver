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

#ifndef AISMGR_H
#define AISMGR_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/aismgr.h

										AIS	Manager

One instance of this class is created for each AIS server.  The AAisMgr class provides a central interface to which each
protocol submits all requests via AisSvr.  All requests that are not intercepted earlier (such as logon) end up here.
 
CHANGE HISTORY
Version	Date		Who		Change
1.0118	12/12/2006	tlw		submit. Add serial binary stream argument.
1.0113	11/7/2006	tlw		destructor. Free allocated resources.
1.0057	 3/18/2005	tlw		Add documentation
												--------------- ---------------

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QHash>
#include "awebpage.h"
#include "aisoutput.h"			// AISMGR_OUT_SIZE

class AAisSvr;
class AWebPage;
class ASessionManager;

//	------------------------------------------------------ TYPEDEFS -----------------------------------------------------------
typedef	struct
{	QString		mAmpmsg;
	bool		mIsAsync;
	AReqType	mReqType;
	long		mRqId;
} APendingReq;

typedef QMap<long, APendingReq> APendReqMap;	// Key=RequestId, value= request info
typedef QMap<QString, AReqType> AEnctypeM;		// key: encoding type, value: Ret request type
typedef QHash<long, AWebPage*> AWebPages;		// key=sessionid, value=AWebPage instance

//	------------------------------------------------- CLASS DECLARATIONS -------------------------------------------------------
/*!
\brief AAisMgr - Format requests to be submitted to the Session Manager. Process returns from the engine.

\par Requests.
All requests are in the form of an AMP message.  An AMP message is a DEL delimited set of name-value pairs.  The first pair is
a targetLambda|speechAct.  AisMgr is responsible for handling those AMP messages whose targetLambda is _ais and whose speechAct
is a request type such as closeCabinet or openNode, etc.  AisMgr either converts a request into a call to a specific SessionMgr
function or it converts the request into AisLisp code that is submitted to the engine via SessionMgr's submit function.

\par Notes.
- This class must remain general enough to service all Qt applications that want to talk to AIS engine.  It may be better to
provide multiple interfaces that subdivide the interface into specific parts, such as local requests, system administration,
GUI, etc.
- This class inherits ASessionManagerInterface which is the public interface to the session manager component of the AIS.
- When the engine finishes a task, it sends an event to AisMgr's user event.  Event then calls AisSvr's returnOutput
which dispatches the response to all the interested protocol Servers.

\par Return Value.
An integer value may be returned in RetValue passed back as part of the message payload as follows:
\verbatim
	ReqType				RetValue		AisOut
	-------				--------		------
	TCPIPERROR			State			msg. State:%s, Reason:%s
	GetContextId		ContextId		ContextId
	GetContextParams	ParamCount		Parameters
	GetExeSession		SessionId		SessionId
	GetSessionUser		UsrId			UsrId
	IsContextBusy		SessionId		SessionId
	IsContextOpen		0-1				false-true
	Logoff				UsrId			UsrId
	Logon				UsrId			secLvl|endday|comment
	Noop				SessionId		sessionid=%d
	OpenSession			SessionId		SessionId
	ReadHtmlPage		MsecToWait		Url
	SendEngineState		EngineState		EngineState
\endverbatim
 */
class AAisMgr : public QObject
{
public:
	// Opens connection using specified protocol for local in process
	AAisMgr(ASessionManager* ipSessionMgr);
	~AAisMgr();
	virtual bool	event(QEvent* ipEv);
	void			flushSessions();
	void			notifyHtmlPage(long iSessionID, const QByteArray& irPageBfr);
	long			submit(long iSessionId, long iRqId, const QString& irAmpmsg, bool iIsAsync, long* opRetValue
					, QString& orAisOut, QString& orDisplay, char* ipData);
	void			setAisSvr(AAisSvr* ipAisSvr);

private:
	AAisSvr*		cpAisSvr;			// For returning async output
	AEnctypeM		cEnctypes;			// key - encoding type, value - Ret request type
	QString			cMt;				// Empty string
	long			cPendingReqs;		// Reference count of requests started/finished
	long			cPendingReqsLast;	// Reference count as of last reported state
	AReqType		cFcnTypes[AISMGROUT_SIZE];
	APendReqMap		cPendingRequests;
	AWebPages		cWebPages;			// Keep track of web pages for sessions using a web page
	ASessionManager*cpSessionMgr;
};

/*!
\brief setAisSvr - Set cpAisSvr

\param ipAisSvr - The instance of AAisSvr handling returns from AAisMgr.
\return void
 */
inline void AAisMgr::setAisSvr(AAisSvr* ipAisSvr)
{
	cpAisSvr = ipAisSvr;
}
#endif // AISMGR_H
