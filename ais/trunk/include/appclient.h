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

#ifndef APPCLIENT_H
#define APPCLIENT_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/appclient.h
														APP Client

CHANGE HISTORY
Version	Date		Who		Change
4.002    9/28/2008  tlw     CR128. Add subscribe to allow a connection subscribe to a session.
3.2005	 2/12/2008	fchua	Added getLogonStats, getConnectionStats, getSessionStats, getRequestStats.
3.2003	 2/12/2008	fchua	Added cOpenConnXid member to AAppClient.
3.2003	 2/12/2008	fchua	Removed unused variables cCurXid and cCurReqType.
3.2003	 2/12/2008	fchua	Changed AClientReqM to QMap from QHash.
3.2003	 2/12/2008	fchua	Added mReqType member in AClientReqSt.
3.2001	 1/27/2008	fchua	Added support for User Management functions.
3.1004	11/2/2007	fchua	Added returnMsg() signal.
3.1003	10/23/2007	fchua	Added cConnectionId property and getConnectionId() method.
3.1002	10/21/2007	fchua	Changed cpServerName to cServerName.
2.0001	1/3/2006	tlw		Remove cClientBrowser, cClientHelpUrl.
2.0001	12/29/2006	tmay	added onSubmitBin.
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0118	12/12/2006	tlw		onSubmit,submit. Add serial binary stream argument.
1.0114	11/15/2006	tlw		dumpNodes. Show list of current nodes in mNodes.
1.0111	10/23/2006	tlw		onOpenConnection.  Convert public methods used by contextClient to slots.
1.0104	 9/8/2006	tlw		Revise to use ASocket that now uses signals/slot interface.
1.0057	 3/18/2005	tlw		Revise doSocketRead to allow multiple and partial responses.
											---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QDateTime>
#include <QtCore/QHash>
#include <QtCore/QMutex>
#include <QtCore/QStringList>

#include "aglobals.h"					// AReturnRcvr, AReqType, ACloseMode
class ASocket;

//	----------------------------------------------------- DEFINITIONS ---------------------------------------------------------

//	------------------------------------------------------ TYPEDEFS -----------------------------------------------------------
// Methods returning AXID submit one request to the server.  Xid is a unique identifier for each request to the server. The
// response to the request, returned at a later time via returnMsg, contains the same Xid.  Requests not returning AXID return
// an immediate result from locally stored connection state. In these cases, no results are returned via returnMsg (its one or
// the other, never both). Methods returning ASXID submit requests to AIS that require an open session.
typedef long ASXID;		// Submits request to AIS (needs sessionID)
typedef long AXID;		// Submits request to AIS

/*!
 * \brief AIS Client request information.
 */
typedef struct
{	QString		mAmpMsg;		/*!< Amp message */
	QString		mClientData;	/*!< Any stuff client wishes to retain with request */
	long		mClientValue;	/*!< Client integer value */
	void*		mpCustomData;	/*!< Data not managed by appClient */
	AReturnRcvr* mpRcvr;		/*!< Client form that submitted request */
	AReqType	mReqType;		/*!< Request type */
} AClientReqSt;

/*!
 * \brief AIS Client request map.
 *
 * \note This was changed from QHash since the order of the keys will necessary
 */
typedef QMap<long, AClientReqSt> AClientReqM;	// key - AXID, value - request info

/*!
 * \brief AIS Client request list.
 */
typedef QList<AClientReqSt> AClientReqList;

/*!
 * \brief This class holds one line of the results of a getNextLevel() call.
 * \see ANodeVector and AExtentInfo.mNodes.
 */
class ANodeInfo
{
public:
	/*!
	 * \brief Default constructor.
	 */
	ANodeInfo() {}

	/*!
	 * \brief Constructor. Initializes the object.
	 *
	 * \param[in] irType Type.
	 * \param[in] irValue Value.
	 * \param[in] irSize Size.
	 * \param[in] irDate Date.
	 * \param[in] irTime Time.
	 * \param[in] irVersion Version.
	 * \param[in] irSymbol Symbol.
	 * \param[in] irUniqueKey Unique Key.
	 */
    ANodeInfo(const QString& irType, const QString& irValue, 
		const QString& irSize, const QString& irDate,
		const QString& irTime, const QString& irVersion,
		const QString& irSymbol, const QString& irUniqueKey) :
		mType(irType), mValue(irValue), mSize(irSize),
		mDate(irDate), mTime(irTime), mVersion(irVersion),
		mSymbol(irSymbol), mUniqueKey(irUniqueKey)
	{ }

    QString mType;			/*!< type of nodes TBD */
	QString	mValue;			/*!< value/name of node */
	QString	mSize;			/*!< size, string for now - change to long later */
	QString mDate;			/*!< date */ 
	QString mTime;			/*!< time */
	QString mVersion;		/*!< version, string for now - change later? */
	QString mSymbol;		/*!< symbolic name of node */
	QString mUniqueKey;		/*!< reference based unique key for use when duplicate symbols exist */
};

/*!
 * \brief Node information list.
 */
typedef QList<ANodeInfo> ANodeVector; // Key is Node Name or "string value"

/*!
 * \brief This class holds the list of supported actions for an extent type.
 */
class AExtentTypeInfo
{
public:
	/*!
	 * \brief Default constructor.
	 */
	AExtentTypeInfo() {}

	/*!
	 * \brief Constructor. Initializes the object.
	 * \param[in] irTypeName Name of type.
	 * \param[in] irActionCodes Comma-delimited list of actions.
	 */
	AExtentTypeInfo(const QString& irTypeName, const QString& irActionCodes) : mTypeName(irTypeName), mActionCodes(irActionCodes){}

	QString mTypeName;		/*!< name of type supported on server */
	QString	mActionCodes;	/*!< comma delimited list of supported actions */
};

/*!
 * \brief Extent type information map.
 */
typedef QHash<QString, AExtentTypeInfo> AExtentTypeMap; // Key: extent name, value: extent type info

/*!
 * \brief Extent options map.
 */
typedef QHash<QString, QString> AExtentTypeOptions;		// Key: extent name, value: option

/*!
 * \brief This class holds one line of the results of a call to getExtents call except the node extent name is the key stored in
 * the QMap that holds the AExtentInfo instance. See AExtentMap and AAppClient.cExtents below.
 */
class AExtentInfo
{
public:
	/*!
	 * \brief Default constructor.
	 */
	AExtentInfo() {}

	/*!
	 * \brief Constructor. Initializes the object.
	 *
	 * \param[in] irNodePath Node path.
	 */
    AExtentInfo(const QString& irNodePath) : mNodePath(irNodePath){ }

    QString			mNodePath;			/*!< current nodes path */
	ANodeVector		mNodes;				/*!< Current nodes */
	AExtentTypeMap	mServerTypes;		/*!< dictionary of extent types and allowed actions */
	QString			mOptions;			/*!< options for getNextLevel - see special handling of .Memory. cabinet */
};

/*!
 * \brief Extent information map.
 */
typedef QHash<QString, AExtentInfo> AExtentMap; // Key: extent name, value: extent info

//	------------------------------------------------------ CLASSES ------------------------------------------------------------
/*!
 * \brief The AAppClient class provides a central interface for an APP Client to talk to the AIS server.
 *
 * One instance of this class is created for each connection to the AIS server made by an APP client.
 *
 * \par General Interface Description
 * The AAppClient class provides a number of member functions that can be grouped into these categories:
 * - Security and Connectivity Functions
 * - Context Functions
 * - Session Functions
 * - Cabinet Functions
 * - Utility Functions
 *
 * All member functions return immediately. Many start a communciation process with the server that results in a return result
 * coming back from the server at some future time. These member functions return a non-zero requestId. The functions that return
 * something other than a requestId are generally performing a client-side only process, setting or reading a client-side property.
 *
 * The AAppClient handles these return results and returns a result back to the client application . In almost every case, the
 * client application blocks further calls to AAppClient until these results are delivered.  This simplifies error handling and
 * limits hogging by any one client.
 *
 * \par Engine Flags
 * The AAppClient object contain a property named cEngineFlags and member functions that request changes to cEngineFlags.
 * cEngineFlags contains the state of the engine for the session that has been set through the AAppClient interface.
 *
 * Some AAppClient member functions produce requests to the server that are not subject to the session's requested engine state.
 * For instance, if the session is in debugging mode, because a currently executing request has dropped into error trace or
 * instruction trace mode, some requests submitted to the engine will temporarily turn off the instruction trace flag. This allows
 * communication with the engine, (for example checking out a Lambda's source while debugging the Lambda), to proceed normally. The
 * AAppClient member functions that submit requests in this manner are called privileged members.
 *
 * For non-privileged member functions, the state of the cEngineFlags is stored in the request submission so that it can be
 * restored when the request is completed. Remember that a request is considered complete when the return result from the server
 * is handled by AAppClient.  The general rule is that we restore the state of the session's cEngineFlags variable to whatever it
 * was at the start of the last non-priviledged request.
 *
 * When notifications of a change to the engines state is received by AAppClient it is returned back to the APP client so that
 * the client can update its GUI.
 *
 * \par Notes
 * -# This class must remain general enough to service all the requests that are required by an AIS IDE and a server monitor.
 * It might be better to provide multiple interfaces that subdivide the interface into specific parts, such as local requests,
 * system administration, GUI, etc.
 * -# This class must inherit from QObject and run on the Main event-loop thread in order to use signals and slots.
 *
 * \par Ordering
 * -# openConnection - Must come first. Cannot openConnection if one already open.
 * -# logon - Logon must come before requests can be submitted to AIS.  Subsequent logons are allowed.  If a session is open,
 * the current EndDay and SecLvl do not change.
 * -# openSession - Open session or reconnect to disconnected session requires a previous logon.  If the context is different
 * than previous context, the EndDay and SecLvl for the user is updated.  If the new context has a security file, it is
 * searched for entries with the current usrID. If the file does not exist of if the usrId is not found, the EndDay and
 * SecLvl are reset to the SysSecLvl and SysEndDate.  Open or reconnect are not allowed if a session is already open.
 *
 * \par Sessions
 * All requests that require access to the engine require an open session.  That includes everything except page requests and a
 * few administrative requests such as openConnection, logon, openSession.
 */
class AAppClient : public QObject, public AReturnRcvr
{
	Q_OBJECT
public:
	// Opens connection using specified protocol for local in-process
	AAppClient(const QByteArray& irHostDnsIp, ushort iPort, QObject* ipParent, AReturnRcvr* ipRcvr, const char *ipName);
	virtual ~AAppClient();
	virtual bool	connectionClosed(long iDummyId);
	virtual void	returnOutput(long iConnectId, long iXid, long iStatus,AReqType iReqType,long iRetValue,const QString& irOut
					, char *ipData, long iDataSize, const QString& irDisplay, const QString& irError
					, const QString iClientData = QString());

	/*!
	 * \brief Set the context name associated with this client.
	 *
	 * \param[in] iConnectId Not used.
	 * \param[in] irContextName Name of context.
	 */
	virtual void	setContextName(long iConnectId, const QString& irContextName)
    {
        iConnectId = iConnectId;
        cContextName = irContextName;
    };

	// Connectivity Functions
	AXID		closeConnection(AReturnRcvr* ipRcvr, long iConnectId, ACloseMode iMode, void* ipCustomData = NULL);
	QString		getServerName();
	bool		isConnected();
	void		setDefaultRcvr(AReturnRcvr* ipRcvr);
	void		setHost(const QString& irHost, ushort iPort);
	AXID		setLogLvl(AReturnRcvr* ipRcvr, AReqType iLogType, AErrLvl iWarnLvl, void* ipCustomData = NULL);
	AXID		setRules(AReturnRcvr* ipRcvr, const QString& irRules, bool iRemove = false, void* ipCustomData = NULL);
	AXID		setSubscriptions(AReturnRcvr* ipRcvr, const QString& irNewSessions,const QString& irOldSessions,void* ipCustomData=NULL);
	ASXID		submit(AReturnRcvr* ipRcvr, const QString& irAmpmsg, const QString& iorClientData, long iClientValue
				, char* ipData, long iDataSize, void* ipCustomData = NULL);
	AXID		submitError(AReturnRcvr* ipRcvr, long iStatus, AReqType iReqType, const QString& irAmpmsg, const QString&
				iorClientData, long iClientValue, void* ipCustomData);
	long		getConnectionId();

	// Context Functions
	bool		getContextParam(const QString& irKey, QString& orValue);
	AXID		getContextParams(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData = NULL);

	/*!
	 * \brief Get the context name associated with this client.
	 *
	 * \return cContextName - Name of context.
	 */
	QString		getCurrentContextName() {return cContextName;};
	AXID		registerContext(AReturnRcvr* ipRcvr, const QString& irStartupPath, void* ipCustomData=NULL);

	// Session Functions
	AXID		connectSession(AReturnRcvr* ipRcvr, long iSessionId, void* ipCustomData = NULL);
	ASXID		debug(AReturnRcvr* ipRcvr, const QString& irExp, void* ipCustomData = NULL);
	ASXID		enableConsoleLog(AReturnRcvr* ipRcvr, long iEnable, void* ipCustomData=NULL);
	ASXID		getConsoleLog(AReturnRcvr* ipRcvr, bool iClear=true, void* ipCustomData=NULL);

	/*!
	 * \brief Get the Session Id.
	 *
	 * \return cSessionId - Session Id.
	 */
	long		getSessionId() { return cSessionId;};
	bool		getErrorTrace();
	ASXID		getExeSession(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	bool		getInstructionTrace();
	bool		getJit();
	bool		getSysCheck();

	/*!
	 * \brief Get the User Id.
	 *
	 * \return cUsrId - User Id.
	 */
	long		getUsrId() { return cUsrId;};
	ASXID		getWorkspaceStatistics(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	ASXID		openConsoleLog(AReturnRcvr* ipRcvr, bool iClear,bool iRedirect, long iSize,bool iStartAtNewline, void* ipCustomData=NULL);
	AXID		runRemoteScriptFile(AReturnRcvr* ipRcvr, const QString& irFilePath,const QString& irPrefix=QString(),void* ipCustomData = NULL);
	AXID		setBreakPoint(AReturnRcvr* ipRcvr, QString& irLambdaName,void* ipCustomData =NULL);
	AXID		setEngineFlags(AReturnRcvr* ipRcvr, unsigned iFlags, void* ipCustomData = NULL);
	AXID		setErrorTrace(AReturnRcvr* ipRcvr, bool iOnoff, void* ipCustomData = NULL);
	AXID		setEscape(AReturnRcvr* ipRcvr, long iSessionId, void* ipCustomData = NULL);
	AXID		setInstructionTrace(AReturnRcvr* ipRcvr, bool iOnoff, void* ipCustomData = NULL);
	AXID		setJit(AReturnRcvr* ipRcvr, bool iOnOff, void* ipCustomData = NULL);
	AXID		setSysCheck(AReturnRcvr* ipRcvr, bool iOnOff, void* ipCustomData = NULL);
	ASXID		showConsoleSelection(AReturnRcvr* ipRcvr, QString& irText, void* ipCustomData = NULL);

	// Cabinet Functions
	ASXID		checkCabinet(AReturnRcvr* ipRcvr, const QString& irCabName, void* ipCustomData = NULL);
	ASXID		closeCabinet(AReturnRcvr* ipRcvr, const QString& irCabName, void* ipCustomData = NULL);
	ASXID		compileAllCabinets(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	ASXID		compileLambda(AReturnRcvr* ipRcvr, QStringList& irExtentLambdaList, void* ipCustomData = NULL);
	ASXID		compileCabinet(AReturnRcvr* ipRcvr, QStringList& irExtentNames, void* ipCustomData = NULL);
	ASXID		eraseNode(AReturnRcvr* ipRcvr, QStringList& irNodeList, void* ipCustomData=NULL);
	ASXID		exportCabinetRemote(AReturnRcvr* ipRcvr, QString& irCabName, QString& irCabPath, bool iSetLocation = false, void* ipCustomData = NULL);
	ASXID		exportCabinetRemoteDir(AReturnRcvr* ipRcvr, QString& irCabName, QString& irCabPath, bool iSetLocation = false, void* ipCustomData = NULL);
	ASXID		exportNodeRemote(AReturnRcvr* ipRcvr, QString& irCabName,QString& irNodeName,QString& irFilePath,void* ipCustomData = NULL);
	QStringList getCurrentNodeNames();
	ASXID		registerDirectory(AReturnRcvr* ipRcvr, const QString& irDatabaseDirectory, const QString& irSourceDirectory, void* ipCustomData = NULL);
	ASXID		updateMetadata(AReturnRcvr* ipRcvr, QString& irCabName, QString& irCabPath, QString& irCabLocation, QString& irStorage, QString& irImportSync, QString& irExportSync,
					QString& irAutoCompile, QString& irSearchSwitch, QString& irDependencies, void* ipCustomData = NULL);

	/*!
	 * \brief Get the name of the current extent.
	 *
	 * \return cCurExtentName - Name of current extent.
	 */
	QString		getCurrentExtentName() {return cCurExtentName;};
	QStringList	getCurrentExtentNames();

	/*!
	 * \brief Get the list of current repository names.
	 *
	 * \return cRepositoryNames - List of repository names.
	 */
	QStringList	getCurrentRepositoryNames() {return cRepositoryNames;};
	ASXID		getDirInfo(AReturnRcvr* ipRcvr, QString& irDir, void* ipCustomData = NULL);
	ASXID		getExtentNames(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	ASXID		getExtentStatus(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	ASXID		getExtentTypes(AReturnRcvr* ipRcvr, bool irForce = true, void* ipCustomData=NULL);
	QString		getExtentTypeOptions(QString& irExtentType);
	QString		getFullNodeName(const QString& irExtentName, long iIndex);
	QString		getFullNodeName(long iIndex); // use current context
	ASXID		getPreviousLevel(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	AXID 		getMetaData(AReturnRcvr* ipRcvr, const QString& irCabName, void* ipCustomData = NULL);
	ASXID		getNextLevel(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	ASXID		getNextLevel(AReturnRcvr* ipRcvr, const QString& irFullNodeName, void* ipCustomData = NULL);
	ANodeInfo	getNode(long iNodeIndex);
	QStringList	getNodeActions(long iNodeIndex);
	QString		getNodeType(QString& irExtentName, QString& irFullNodeName);
	QString		getNodeType(long iNodeIndex);
	QStringList	getNodePathTree();
	QString		getNodeSymbolName(long iNodeIndex);
	long		getNumNodes();
	long		getNumNodes(QString& irExtentName);
	ASXID		getRootLevel(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	QStringList	getServerTypeActions(const QString& irType, const QString& irExtentName);
	ASXID		importCabinetRemote(AReturnRcvr* ipRcvr, QString& irCabName, QString& irFilePath, bool iSetLocation = false, void* ipCustomData = NULL);
	ASXID		importCabinetRemoteDir(AReturnRcvr* ipRcvr, QString& irCabName, QString& irFilePath, bool iSetLocation = false, void* ipCustomData = NULL);
	ASXID		newCabinet(AReturnRcvr* ipRcvr, QString& irCabName, QString& irCabPath, QString& irCabLocation, QString& irStorage, QString& irImportSync, QString& irExportSync,
					QString& irAutoCompile, QString& irSearchSwitch, QString& irDependencies, void* ipCustomData = NULL);
	ASXID		openNode(AReturnRcvr* ipRcvr, const QString& irExtentName, const QString& irFullNodeName, void* ipCustomData = NULL);
	ASXID		openCabinet(AReturnRcvr* ipRcvr, QString& irCabName, QString& irCabPath, QString& irCabLocation, QString& irStorage, QString& irImportSync, QString& irExportSync,
					QString& irAutoCompile, QString& irSearchSwitch, QString& irDependencies, void* ipCustomData = NULL);
	ASXID		refreshLevel(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	ASXID		saveNode(AReturnRcvr* ipRcvr,const QString& irExtentName, const QString& irNodeName, QByteArray& irText,void* ipCustomData=NULL);
	bool		setCurrentExtent(const QString& irExtentName);
	void		setExtentTypeOptions(const QString& irExtentType, const QString& irOptions);
	long		today();
	// Client Request Queue management functions
	AClientReqList getReqQueue();			// return a copy of client requests sorted by xid
	AClientReqSt getReqQueueItem(AXID iXid);// return copy of a specific item from request queue

	// User Management Functions
	ASXID		addUser(AReturnRcvr* ipRcvr, const QString& irUsername, const QString& irPassword,
						long iSecurityLevel, const QDate& irEndDate, const QString& irComment);

	ASXID		deleteUser(AReturnRcvr* ipRcvr, long iUserId);

	ASXID		getUsers(AReturnRcvr* ipRcvr);

	ASXID		updateUser(AReturnRcvr* ipRcvr, long iUserId, const QString& irUsername, const QString& irPassword,
						long iSecurityLevel, const QDate& irEndDate, const QString& irComment);

	// System Monitor Functions
	ASXID		getConnectionStats(AReturnRcvr* ipRcvr);
	ASXID		getLogonStats(AReturnRcvr* ipRcvr);
	ASXID		getSessionStats(AReturnRcvr* ipRcvr);
	ASXID		getRequestStats(AReturnRcvr* ipRcvr);

	// Why is this public?
	QMutex		mMutex; /*!< Mutex object */

public slots:
	AXID		onCloseContext(AReturnRcvr* ipRcvr, const QString& irContextName, long iMode, void* ipCustomData = NULL);
	AXID		onCloseSession(AReturnRcvr* ipRcvr, long iSessionId, long iMode, void* ipCustomData = NULL);
	AXID		onGetCurrentContexts(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	AXID		onGetSessions(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData = NULL);
	AXID		onGetSubscriptions(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData = NULL);
	AXID		onIsContextOpen(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData = NULL);
	AXID		onLogoff(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	AXID		onLogon(AReturnRcvr* ipRcvr, const QString& irUsrName, const QString& irPasswd, void* ipCustomData = NULL);
	AXID		onOpenConnection(AReturnRcvr* ipRcvr, void* ipCustomData = NULL);
	AXID		onOpenContext(AReturnRcvr* ipRcvr, const QString& irStartupPath, const QString& irContextName
				, void* ipCustomData = NULL);
	AXID		onOpenSession(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData = NULL);
	ASXID		onSubmit(AReturnRcvr* ipRcvr, const QString& irExp, char* ipData, long iDataSize, void* ipCustomData = NULL);
	ASXID		onSubmitBin(AReturnRcvr* ipRcvr, const QString& irExp, char* ipData, long iDataSize, void* ipCustomData = NULL);

signals:
	void		returnStuff(AReqType, const QString&); 

protected:
	AClientReqM	 cClientRequests;  /*!< Table of client request objects */
	virtual void returnMsg(long iXid, long iStatus, AReqType iReqType, long iRetValue, const QString& irAisOut, char* ipData
				 , long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData, bool iClearRequestQueueItem);

private slots:
	void		onConnected();
	void		onDisconnected();
	void		onResponse(QByteArray iResponse, QByteArray iCookie, char* ipData);
	void		onSocketError(long iStatus, QString iError);

private:
	void		dumpNodes();
	bool		errorReturned(const QString& irRetMsg);

	// Client Parameters
	QString		cMt;				// An empty (not null) string. Arg placeholder
	QDate		cY2K;				// 1/1/2000

	// Connection state
	QString		cContextName;		// Current context name for this client
	AStringMap	cContextParams;		// Context-specific parameters for this context.
	AReturnRcvr* cpDefaultRcvr;		// Default client form.
	unsigned	cEngineFlags;		// Current engine flags
	QByteArray	cHostDnsIp;			// Ais DNS name or IP address.
	AAppSvr*	cpInProcSvr;		// In-process AAppSvr (null if none).
	long		cInstructionTrace;	// Instruction trace reference count
	bool		cIsConnected;		// Connection to AIS established
	ushort		cPort;				// AIS socket number
	QString		cServerName;		// AIS local name for this connection.
	long		cSessionId;			// The session ID or 0 if no session open
	ASocket*	cpSocket;			// Maintains TCP connection with server
	AReqType	cTask;				// Current task that is underway
	long		cUsrId;				// User ID returned by logon.
	long		cXid;				// Client-generated request ID.
	long		cConnectionId;		// Connection ID
	long		cOpenConnXid;		// Request ID for OpenConnection

	// Memory tab state
	QStringList	cBackHistory;

	// Cabinet state
	QString		cCurExtentName;
	AExtentMap	cExtents;				// Dictionary of ACabinet structures

	// DatamineLambda state
	long		cColCount;
	QStringList	cColNames;
	QString		cCursor;
	QStringList	cDatabaseNames;
	QString		cDatabaseName;
	long		cRecordCount;
	QStringList	cRepositoryNames;
	AExtentTypeOptions cExtentTypeOptions;
};

/*!
 * \brief Returns the most recent Connection Id stored.
 *
 * \sa onConnected().
 */
inline long AAppClient::getConnectionId()
{
	return cConnectionId;
}

/*!
 * \brief Get the value of a named context-specific parameter.
 *
 * \param[in] irKey The name of the context-specific parameter.
 * \param[in] orValue A place to put the returned value.
 * \return aRet - true iff the parameter name was found.
 * \note
 * -# Must call getContextParams before calling this routine.
 */
inline bool AAppClient::getContextParam(const QString& irKey, QString& orValue)
{
	bool aRet = false;
	if (cContextParams.contains(irKey))
	{	orValue = cContextParams[irKey];
		aRet = true;
	}
	return aRet;
}

/*!
 * \brief Get the names of the cabinets that are available for this context.
 *
 * \return aNames - List of extent names
 * \note
 * -# Must call getExtentNames before calling this routine.
 */
inline QStringList AAppClient::getCurrentExtentNames()
{	
	QStringList aNames;
	AExtentMap::ConstIterator aItp;
	for (aItp = cExtents.begin(); aItp != cExtents.end(); ++aItp)
		aNames += aItp.key();
	return aNames;
}

/*!
 * \brief Get the names of the Lambdas/values that are available for the current extent.
 *
 * \return aNames - List of node names
 * \note
 * -# Must call getNodeNames before calling this routine.
 */
inline QStringList AAppClient::getCurrentNodeNames()
{
	long aSize;
	QStringList aNames;
	if (cExtents.contains(cCurExtentName))
	{	const ANodeVector& arNodes = cExtents.value(cCurExtentName).mNodes;
		aSize = arNodes.size();
		for (long aNode = 0; aNode < aSize; ++aNode )
			aNames += arNodes[aNode].mValue;
	}
	return aNames;
}

/*!
 * \brief Get the capabilities of this extent.
 *
 * \param[in] irExtentType For now, just the extent name.
 * \return Options in the form of "#(1 1 1)" for .Memory extent; otherwise empty.
 * \see setExtentTypeOptions
 * \note
 * -# Must call setExtentOptions before calling this routine.
 */
inline QString AAppClient::getExtentTypeOptions(QString& irExtentType)
{	// Currently we use the extent name as the key to the extent type options. This is bogus but works for now. 
	QString aOptions;
	if (cExtentTypeOptions.contains(irExtentType))
		aOptions = cExtentTypeOptions.value(irExtentType);
	return aOptions;
}

/*!
 * \brief Get the name of the variable/Lambda/file for the specified node.
 *
 * \param[in] iIndex Index into list of nodes for the current extent.
 * \return aNodeName - The full name including the path + file name for files.
 * \note
 * -# aNodeName may be empty if the mSymbol element in mNodes is empty for this index.
 */
inline QString AAppClient::getFullNodeName(long iIndex)
{
	return getFullNodeName(cCurExtentName, iIndex);
}

/*!
 * \brief Get the name of the variable/Lambda/file for the specified extent/node.
 *
 * \param[in] irExtentName Name of extent that is the parent of this node.
 * \param[in] iIndex Index into list of nodes for the named extent.
 * \return aNodeName - The full name including the path + file name if node is a file.
 * \note
 * -# aNodeName may be empty if the mSymbol element in mNodes is empty for this index.
 */
inline QString AAppClient::getFullNodeName(const QString& irExtentName, long iIndex)
{
	QString aNodeName;
	if (cExtents.contains(irExtentName))
	{	const AExtentInfo& arInfo = cExtents.value(irExtentName);
		if (iIndex < arInfo.mNodes.size())
		{	aNodeName = arInfo.mNodes[iIndex].mSymbol;
			if (!aNodeName.isEmpty() && !arInfo.mNodePath.isEmpty())
				aNodeName = arInfo.mNodePath + "/" + aNodeName;
		}
	}
	return aNodeName;
}

// Note that these defines are primarily defined in sbglue.h
// Change them here if you change them in sbglue.h
#define SBGLUE_ERROR_TRACE			0x01	//1st bit 
#define SBGLUE_INSTRUCTION_TRACE	0x02	//2nd bit
#define SBGLUE_SYSCHECKON			0x04	//3rd bit
#define SBGLUE_JITON				0x10	//4th bit

/*!
 * \brief Get the current instruction trace flag setting.
 *
 * \return The current instruction trace setting.
 */
inline bool AAppClient::getInstructionTrace()
{
	return(cEngineFlags & SBGLUE_INSTRUCTION_TRACE);
}

/*!
 * \brief Get the current error trace flag setting.
 *
 * \return The current error trace setting.
 */
inline bool AAppClient::getErrorTrace()
{
	return(cEngineFlags & SBGLUE_ERROR_TRACE);
}

/*!
 * \brief Get the current JIT flag setting.
 *
 * \return The current JIT setting.
 */
inline bool AAppClient::getJit()
{
	return(cEngineFlags & SBGLUE_JITON);
}

/*!
 * \brief Get the name of the variable/Lambda/file for the specified extent/node.
 *
 * \param[in] iNodeIndex Index into list of nodes for the named extent.
 * \return aSymbolName - The node name (not including any path that might be set).
 * \see getFullNodeName
 * \note
 * -# aSymbolName may be empty if the mSymbol element in mNodes is empty for this index.
 */
inline QString AAppClient::getNodeSymbolName(long iNodeIndex)
{
	QString aSymbolName;
	if (cExtents.contains(cCurExtentName))
	{	const AExtentInfo& arInfo = cExtents.value(cCurExtentName);
		const ANodeVector aNodes = arInfo.mNodes;
		if (iNodeIndex < aNodes.size())
			aSymbolName = aNodes[iNodeIndex].mSymbol;
	}
	return aSymbolName;
}

/*!
 * \brief Get the type of node for the specified node.
 *
 * \param[in] iIndex Index into list of nodes for the current extent.
 * \return aType - "Lambda" or variable type (Function, Macro, Number, Pcode, Quoted Symbol, Special Form ...).
 * \see getFullNodeName
 * \note
 * -# aSymbolName may be empty if the mType element in mNodes is empty for this index.
 */
inline QString AAppClient::getNodeType(long iIndex)
{	
	QString aType;
	if (cExtents.contains(cCurExtentName))
	{	const AExtentInfo& arInfo = cExtents.value(cCurExtentName);
		const ANodeVector aNodes = arInfo.mNodes;
		if (iIndex < aNodes.size())
			aType = aNodes[iIndex].mType;
	}
	return aType;
}

/*!
 * \brief Get the number of nodes for the specified extent.
 *
 * \param[in] irExtentName The extent to be examined.
 * \return aNodes - the number of nodes for this extent.
 * \note
 * -# aNodes may be zero if the mNodes element in mNodes is empty for this index.
 */
inline long AAppClient::getNumNodes(QString& irExtentName) 
{
	long aNodes = 0;
	if (cExtents.contains(irExtentName))
	{	const AExtentInfo& arInfo = cExtents.value(irExtentName);
		const ANodeVector& arNode = arInfo.mNodes;
		aNodes = arNode.count();
	}
	return aNodes;
}

/*!
 * \brief Get the number of nodes for the current extent.
 *
 * \return aNodes - the number of nodes for the current extent.
 * \note
 * -# aNodes may be zero if the mNodes element in mNodes is empty for this index.
 */
inline long AAppClient::getNumNodes() 
{
	return getNumNodes(cCurExtentName);
}

/*!
 * \brief Return a copy of the request queue.
 *
 * \return The values in cClientRequests.
 */
inline AClientReqList AAppClient::getReqQueue()
{	
	return cClientRequests.values();
}

/*!
 * \brief Return a copy of a specific item from request queue.
 *
 * \param[in] iXid The index long cClientRequests.
 * \return the value in cClientRequests for the specified index.
 */
inline AClientReqSt AAppClient::getReqQueueItem(AXID iXid)
{
    return cClientRequests[iXid];
}

/*!
 * \brief Get the current server name value.
 *
 * \return The server name.
 */
inline QString AAppClient::getServerName()
{
	return cServerName;
}

/*!
 * \brief Get the current SysCheck flag setting.
 *
 * \return The current SysCheck setting.
 */
inline bool AAppClient::getSysCheck()
{
	return(cEngineFlags & SBGLUE_SYSCHECKON);
}

/*!
 * \brief Get the current cIsConnected flag setting.
 *
 * \return The current cIsConnected setting.
 */
inline bool AAppClient::isConnected()
{
	return cIsConnected;
}

/*!
 * \brief Set the current extent.
 *
 * \return true iff name is found in cExtents.
 * \note
 * -# irExtentName must be one of the extents in cExtents for this context.
 */
inline bool AAppClient::setCurrentExtent(const QString& irExtentName)
{
	bool aFoundIt = false;
	if (cExtents.contains(irExtentName))
	{	cCurExtentName = irExtentName;
		aFoundIt = true;
	}
	return aFoundIt;
}

/*!
 * \brief Set the extent options.
 *
 * \param[in] irExtentType Extent type.
 * \param[in] irOptions List of options.
 * \note
 * ExtentType(".Memory"), aOptions("#(1 1 1)") Note that this is a bit bogus. We don't really  have "extent types" so we are using
 * the "name" of "special" extents as the key for saving extent type options. These options are currently used only for 
 * passing optional arguments to the ais|getnextlevel command. Note that we only have this special processing for the
 * .Memory cabinet right now and that this special handling sets the ShowLockedVariables and ShowSystemVariables options.
 *
 * \par
 * QString aExtentType(".Memory"), aOptions("#1 ");
 * aOptions += cpShowLockedVariablesAct->isChecked() ? "1 " : "0 ";
 * aOptions += cpShowSystemVariablesAct->isChecked() ? "1)" : "0)";
 *
 * \par
 * cExtentTypeOptions holds getNextLevel options for each extent type. Currently we use the extent name for the extent type
 * and only have the .Memory extent to worry about. Later on we could easily extend the system to have browseLib return
 * an "extent type" for each extent and customize getNextLevel options for each extent type. see setGetNextLevelOptions.
 */
inline void AAppClient::setExtentTypeOptions(const QString& irExtentType,const QString& irOptions)
{
	// Currently we use the extent name as the key for this data. This is bogus but works for now.
	cExtentTypeOptions.insert(irExtentType, irOptions);
}

/*!
 * \brief Set the host parameters.
 *
 * \note
 * -# It is important to call closeConnection prior to changing to a new host.
 * -# It is also important to call openConnection after changing to a new host.
 * In fact, it would be better to pass these params in openConnection instead of passing them in the constructor.
 */
inline void AAppClient::setHost(const QString& irHost, ushort iPort)
{
	cHostDnsIp = irHost.toLatin1();			// Ais DNS name or IP address.
	cPort = iPort;
}

/*!
 * \brief Days elapsed since 1/1/2000.
 *
 * \return Number of days since 1/1/2000.
 */
inline long AAppClient::today()
{
	return cY2K.daysTo(QDate::currentDate());
}

#endif // APPCLIENT_H
