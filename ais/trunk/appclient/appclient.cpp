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
aisdev/appclient/appclient.cpp

													AIS Application Client

The Application Client provides the interface for local or remote applications to the Analytic Information Server (AIS).  It
formats each request into a DEL-delimited string and then sends this string to the appsvr.  For remote clients, the call is
transmitted over a TCP socket connection.  A local instance of the appclient makes a function call to the appsvr.
Appclient provides the entire interface between an application window and AIS.  An instance of appclient provides a separate
interface for each Session Form or other GUI.  Use aisclient as simplified interface for mainQtForm, serverForm, etc.

CHANGE HISTORY
Version	Date		Who		Change
4.002    9/28/2008  tlw     CR128. Add subscribe to allow a connection subscribe to a session.
3.2005	 2/12/2008	fchua	Added getLogonStats, getConnectionStats, getSessionStats, getRequestStats.
3.2005	 2/12/2008	fchua	Fixed handling of out-of-sequence responses.
3.2003	 2/08/2008	fchua	onOpenConnection. Updated method of checking for pending requests.
3.2002	 2/04/2008	fchua	Updated implementation of onLogoff.
3.2002	 2/04/2008	fchua	Added support for new log type geLogUserAccess.
3.2001	 1/27/2008	fchua	Added support for User Management functions.
3.1006	12/21/2007	fchua	submit. Removed check of session id in geCloseSession case.
3.1005	12/16/2007	fchua	Modified returnOutput. Added updating of stored session id after connect session.
3.1004	11/2/2007	fchua	Modified returnMsg(). Added emit signal for log requests.
3.1003	10/23/2007	fchua	Added retrieval of connection id in onConnected(). Used connection id in returnMsg(). Added getConnectionId().
3.1002  10/21/2007	fchua	Changed cpServerName to cServerName. Removed deleteLater() in connectionClosed(). Updated onSocketError().
3.1001	10/9/2007	fchua	Removed extra delete in ~AAppClient(). cpSocket deallocation is already handled by QT.
2.0002	1/25/2007	tlw		connectionClosed. Do not return result if cpDefaultRcvr is NULL.
2.0001	1/3/2007	tlw		Remove cClientBrowser cClientHelpUrl, geLogNcsa
2.0001	12/29/2006	tmay	added onSubmitBin
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0118	12/12/2006	tlw		onsubmit,submit. Add serial binary stream argument.
1.0117	12/6/2006	tlw		returnMsg.  Generate error if null receiver.
1.0114	11/15/2006	tlw		dumpNodes. Show list of current nodes in mNodes.
1.0112	11/2/2006	tlw		onCloseSession.  Convert close mode to a string.
1.0111	10/23/2006	tlw		onOpenConnection.  Convert public methods used by contextClient to slots.
1.0108	 9/30/2006	tlw		getArg. Add arguments to trim and lower case.
1.0107	 9/19/2006	tlw		Return immediate errors from submit. Allow request if request is pending while debugging.
1.0104	 9/8/2006	tlw		Revise to use ASocket that uses signals/slot interface.
1.0056	 3/10/2005	tlw		Update documentation.
												---------------------------------

DOCUMENTATION
1.	See appclientnotes.txt for project setup information.
2.	See include/appclient.h for specifications.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QtDebug>
#include "aglobals.h"
#include "appclient.h"			// AAppClient
#include "appsvr.h"				// AAppSvr
#include "asocket.h"			// ASocket
#include "autilities.h"			// AUtil

//	--------------------------------------------------- PUBLIC METHODS --------------------------------------------------------
/*!
 * \brief Constructor, initialize member variables.
 *
 * \note
 * Create one AAppClient per AIS Server.
 */
AAppClient::AAppClient(const QByteArray& irHostDnsIp, ushort iPort, QObject* ipParent, AReturnRcvr* ipRcvr, const char* ipServerName)
	:	QObject(ipParent), cMt(""), cHostDnsIp(irHostDnsIp), cConnectionId(0), cOpenConnXid(0)
{
	// Client parameters
	setObjectName(ipServerName);
	cY2K = QDate(2000, 1, 1);
	
	// Connection State
	cClientRequests.clear();
	cContextParams.clear();
	cpDefaultRcvr = ipRcvr;
	cEngineFlags = 0;
	cpInProcSvr = NULL;
	if (cHostDnsIp.isEmpty())
	{	cHostDnsIp = AGLBLS_INPROCSVRNAME;
		iPort = 0;
	}
	cInstructionTrace = 0;
	cIsConnected = false;
	cPort = iPort;
	cServerName = ipServerName;
	cSessionId = 0;
	cpSocket = NULL;
	cTask = geUnknown;
	cUsrId = 0;
	cXid = 0;

	// Memory Tab State
	cBackHistory.clear();

	// Cabinet state
	cCurExtentName.truncate(0);
	cExtents.clear();
	cRepositoryNames.clear();
}

/*!
 * \brief Destructor, free allocated resources.
 *
 * \note
 * -# AAppClient has remote ownership of an ASocket referent.
 * -# Since instances of AAppClient are never assigned, copied, or passed by value, the copy constructor and assignment operator
 * are omitted here.  See C++ FAQS chap 30 for more info.
 */
AAppClient::~AAppClient()
{
	cSessionId = 0;
	cExtents.clear();
	cExtentTypeOptions.clear();
	cClientRequests.clear();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AAppClient()");
#endif
}

/*!
 * \brief Sends a geAddUser AMP request to the AIS Server.
 *
 * \param[in] ipRcvr Return receiver object pointer.
 * \param[in] irUsername Username.
 * \param[in] irPassword Password.
 * \param[in] iSecurityLevel Security Level.
 * \param[in] irEndDate End date.
 * \param[in] irComment Comment.
 *
 * \return Request Id.
 */
ASXID AAppClient::addUser(AReturnRcvr* ipRcvr, const QString& irUsername, const QString& irPassword,
						long iSecurityLevel, const QDate& irEndDate, const QString& irComment)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177addUser\177username\177%s\177password\177%s\177securitylevel\177%ld"
		"\177enddate\177%s\177comment\177%s", irUsername.toLatin1().constData(), 
		irPassword.toLatin1().constData(), iSecurityLevel,
		irEndDate.toString("MM/dd/yyyy").toLatin1().constData(),
		irComment.toLatin1().constData());
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/);
}

/*!
 * \brief Submit request to register all the cabinets in a directory
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irDatabaseDir Database directory
 * \param[in] irSourceDir Source directory
 * \param[in] ipCustomData An anonymous pointer returned to caller with request.
 *
 * \return aXid - Request Id (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format: _ais|registerdirectory|cabname|%s
 */
AXID AAppClient::registerDirectory(AReturnRcvr* ipRcvr, const QString& irDatabaseDir, const QString& irSourceDir, void* ipCustomData)
{
    long aXid;
    QString aAmpmsg;
    aAmpmsg.sprintf("_ais\177registerdirectory\177databasedir\177%s\177sourcedir\177%s", irDatabaseDir.toLatin1().data(),irSourceDir.toLatin1().data());
    if (!irDatabaseDir.isEmpty())
        aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
    else
        aXid = submitError(ipRcvr, AERR_BADCABINET, geRegisterDirectory, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
    return aXid;
}

/*!
 * \brief Submit request to check cabinet's import/export status.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irCabName Name of cabinet to be checked.
 * \param[in] ipCustomData An anonymous pointer returned to caller with request.
 *
 * \return aXid - Request Id (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format: _ais|checkcabinet|cabname|%s
 */
AXID AAppClient::checkCabinet(AReturnRcvr* ipRcvr, const QString& irCabName, void* ipCustomData)
{
    long aXid;
    QString aAmpmsg;
    aAmpmsg.sprintf("_ais\177checkcabinet\177cabname\177%s", irCabName.toLatin1().data());
    if (!irCabName.isEmpty())
        aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
    else
        aXid = submitError(ipRcvr, AERR_BADCABINET, geCheckCabinet, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
    return aXid;
}

/*!
 * \brief Submit request to disconnect from the cabinet.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irCabName Name of cabinet to be closed.
 * \param[in] ipCustomData An anonymous pointer returned to caller with request.
 *
 * \return aXid - Request Id (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format: _ais|closecabinet|cabname|%s
 */
AXID AAppClient::closeCabinet(AReturnRcvr* ipRcvr, const QString& irCabName, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177closecabinet\177cabname\177%s", irCabName.toLatin1().data());
	if (!irCabName.isEmpty())
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = submitError(ipRcvr, AERR_BADCABINET, geCloseCabinet, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Close the specified connection to AIS.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iConnectId ID of connection to be closed.
 * \param[in] iMode Close mode (geDefault, geDisconnect, geOpen, geSoft, geFirm, geHard).
 * \param[in] ipCustomData An anonymous pointer returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# Closes current connection if connect ID is 0 or connect ID is current connection.
 * -# AMP format: _ais|closeconnection|connectid|%d|closemode|%s|closewait|%d
 */
AXID AAppClient::closeConnection(AReturnRcvr* ipRcvr, long iConnectId, ACloseMode iMode, void* ipCustomData)
{
    Q_UNUSED(ipCustomData);

	// Submit request for common processing.
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177closeconnection\177connectid\177%ld\177closemode\177%s",iConnectId,gAis.mpCloseMode[iMode]);
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/);
}

/*!
 * \brief Compile an Lambda.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irExtentLambdaList	Linked list of pairs of extent_name, Lambda-names to be compiled.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|compilelambda|extent_Lambdas|%s
 */
AXID AAppClient::compileLambda(AReturnRcvr* ipRcvr, QStringList& irExtentLambdaList, void* ipCustomData)
{
	long aXid;
	// ExtentLambdaList contains pairs of extent-name,Lambda-name entries
	QString aAmpmsg("_ais\177compilelambda\177extent_Lambdas\177");
    if (irExtentLambdaList.count() > 0)
	{	aAmpmsg += irExtentLambdaList.join("\t");
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
	{	aAmpmsg += "noExtents";
		aXid = submitError(ipRcvr, AERR_NOEXTENT, geCompileLambda, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	}
	return aXid;
}


/*!
 * \brief Submit request to AIS to compile all cabinets
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|compileall|extents|%s
 */
AXID AAppClient::compileAllCabinets(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg("_ais\177compileall");
	aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	return aXid;
}


/*!
 * \brief Submit request to AIS to compile all Lambdas in a cabinet.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irExtentNames	Linked list of pairs of cabinets to be compiled.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|compilecabinet|extents|%s
 */
AXID AAppClient::compileCabinet(AReturnRcvr* ipRcvr, QStringList& irExtentNames, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg("_ais\177compilecabinet\177extents\177");
	if (irExtentNames.count() > 0)
	{	aAmpmsg += irExtentNames.join("\t");
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
	{	aAmpmsg += "noExtents";
		aXid = submitError(ipRcvr, AERR_NOEXTENT, geCompileCabinet, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	}
	return aXid;
}

/*!
 * \brief Free allocated resources for this connection.
 *
 * \param[in] iDummyId Not used but required by the AReturnRcvr class.
 *
 * \return true
 *
 * \note
 * -# Implements a pure virtual function inherited from AReturnRcvr.
 * -# Posts an event back to Application client to allow application to close up shop.
 * -# The server name is returned in the ClientData slot so that the client will know which server was closed.
 */
bool AAppClient::connectionClosed(long iDummyId)
{
    Q_UNUSED(iDummyId);

	// Shut down the connection just before sending one last response to client.
	if (cIsConnected && cpDefaultRcvr != NULL)
	{	long aStatus = AERR_LOSTCONNECTION;
		QString aOut("false"), aErr(gAis.mpErrMsgs[aStatus]);
		submitError(cpDefaultRcvr, aStatus, geCloseConnection, "_ais\177connectionClosed", cServerName.toLatin1().data()/*ClientData*/, 0, NULL);
	}
	if (!cClientRequests.isEmpty())
		cClientRequests.clear();
	if (!cContextParams.isEmpty())
		cContextParams.clear();

	cpDefaultRcvr = NULL;
	cExtents.clear();
	if (!cExtentTypeOptions.isEmpty())
		cExtentTypeOptions.clear();
	cEngineFlags = 0;
	cInstructionTrace = 0;
	cIsConnected = false;
	cSessionId = 0;
	cTask = geUnknown;
	cXid = 0;

	// deleteLater(); causes crash to calls to object after this function is called.
	return true;
}

/*!
 * \brief Reconnect to a disconnected session.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iSessionId Reconnect to this disconnected session.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# This request requires a session to be opened.
 * -# AMP format:  _ais|connectsession|sessionid|%d
 */
ASXID AAppClient::connectSession(AReturnRcvr* ipRcvr, long iSessionId, void* ipCustomData)
{
	long aXid = 0, aStatus = 0;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177connectsession\177sessionid\177%ld", iSessionId);
	if (!cIsConnected)
		aStatus = AERR_DISCONNECTED;
	else if (cUsrId <= 0)
		aStatus = AERR_NOLOGON;
	else if (iSessionId <= 0)
		aStatus = AERR_SESSIONID;
	else
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	if (aStatus > 0)
		aXid = submitError(ipRcvr, aStatus, geConnectSession, aAmpmsg, cMt/*ClientData*/,0/*ClientValue*/,ipCustomData);
	return aXid;
}

/*!
 * \brief Submit a debug command during a debugging session.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irExp Debug command.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|debug|exp|%s
 */
AXID AAppClient::debug(AReturnRcvr* ipRcvr, const QString& irExp, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177debug\177exp\177%s", irExp.toLatin1().data());
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Sends a geDeleteUser AMP request to the AIS Server.
 *
 * \param[in] ipRcvr Return receiver object pointer.
 * \param[in] iUserId User Id.
 *
 * \return Request Id.
 */
ASXID AAppClient::deleteUser(AReturnRcvr* ipRcvr, long iUserId)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177deleteUser\177userid\177%ld", iUserId);
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/);
}

/*!
 * \brief Dump all the nodes to qDebug for the current extent.
 */
void AAppClient::dumpNodes()
{
	if (cExtents.contains(cCurExtentName))
	{	ANodeVector aNodes = cExtents[cCurExtentName].mNodes;
		long aSz = aNodes.size();
		ANodeInfo aNode;
		for (long aIx = 0; aIx < aSz; ++aIx)
		{	const ANodeInfo& arNode = aNodes[aIx];
			qDebug() << arNode.mType << ',' << arNode.mValue << ',' << arNode.mSize << ',' << arNode.mDate << ','
			<< arNode.mTime << ',' << arNode.mVersion << ',' << arNode.mSymbol << ',' << arNode.mUniqueKey;
		}
	}
}

/*!
 * \brief Close, suspend, enable the console log.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iEnable -2 to reset, -1 to close, =0 to suspend, 1 to enable, 2 to format.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# This connection must have an open session that has an open console output log.
 * -# AMP format:  _ais|enableconsolelog|enable|%s|sessionid|%d
 * -# Reset formats and then closes.  Format puts top of message at top of file.
 */
ASXID AAppClient::enableConsoleLog(AReturnRcvr* ipRcvr, long iEnable, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177enableconsolelog\177enable\177%ld\177sessionid\1770", iEnable);
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Remove an existing Lambda from a cabinet.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irLambdaList Linked list of Lambdas to be erased.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|erasenode|extent|%s|nodes|%s
 * -# Notes is a tab-delimited list of Lambda names.
 */
AXID AAppClient::eraseNode(AReturnRcvr* ipRcvr, QStringList& irLambdaList, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg(QString("_ais\177erasenode\177extent\177%1\177nodes\177").arg(cCurExtentName));
	if (!cCurExtentName.isEmpty() && irLambdaList.size() > 0)
	{	aAmpmsg += irLambdaList.join("\t");
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
	{	aAmpmsg += "noExtents";
		aXid = submitError(ipRcvr, AERR_NOEXTENT, geEraseNode, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	}
	return aXid;
}

/*!
 * \brief Submit request to AIS to compile all Lambdas in a cabinet.
 *
 * \param[in] irRet Error message.
 *
 * \return true iff message format is !msg!.
 *
 * \note
 * -# Errors from the engine have the !msg! format.
 */
bool AAppClient::errorReturned(const QString& irRet)
{
	return !irRet.isEmpty()&& irRet.startsWith("!") && irRet.endsWith("!");
}


/*!
 *
 * \brief Save a cabinet as a folder on the server.
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irCabName	Name of cabinet to be saved.
 * \param[in] irCabPath Path to folder on the server.
 * \param[in] irSetLocation Flag to set the folder as cabinet's source location.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|exportcabinet|cabinet|%s|folder|%s
 */
AXID AAppClient::exportCabinetRemoteDir(AReturnRcvr* ipRcvr, QString& irCabName, QString& irCabPath, bool iSetLocation, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	QString aSetLocation = "false";
	if(iSetLocation == true)
		aSetLocation = "true";

	aAmpmsg.sprintf("_ais\177exportcabinet\177cabinet\177%s\177folder\177%s\177setlocation\177%s", irCabName.toLatin1().data(), irCabPath.toLatin1().data(), aSetLocation.toLatin1().data());
	if (!irCabPath.isEmpty() && !irCabName.isEmpty())	
		aXid = submit(ipRcvr, aAmpmsg, irCabName, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = submitError(ipRcvr, AERR_BADCABINET, geExportCabinet, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 *
 * \brief Save a cabinet as a .sl file on the server.
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irCabName	Name of cabinet to be saved.
 * \param[in] irCabPath Path/name to .sl file on the server.
 * \param[in] irSetLocation Flag to set the .sl file as cabinet's source location.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|exportcabinet|cabinet|%s|file|%s
 */
AXID AAppClient::exportCabinetRemote(AReturnRcvr* ipRcvr, QString& irCabName, QString& irCabPath, bool iSetLocation, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	QString aSetLocation = "false";
	if(iSetLocation == true)
		aSetLocation = "true";

	aAmpmsg.sprintf("_ais\177exportcabinet\177cabinet\177%s\177file\177%s\177setlocation\177%s", irCabName.toLatin1().data(), irCabPath.toLatin1().data(), aSetLocation.toLatin1().data());
	if (!irCabPath.isEmpty() && !irCabName.isEmpty())	
		aXid = submit(ipRcvr, aAmpmsg, irCabName, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = submitError(ipRcvr, AERR_BADCABINET, geExportCabinet, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Save an Lambda as a .sl file on the server.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irCabName Name of cabinet holding the Lambda.
 * \param[in] irNodeName Name of Lambda to be saved.
 * \param[in] irFilePath Path/name to .sl file on the server.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|exportnode|node|%s|file|%s|cabinet|%s
 */
AXID AAppClient::exportNodeRemote(AReturnRcvr* ipRcvr, QString& irCabName, QString& irNodeName, QString& irFilePath, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177exportnode\177node\177%s\177file\177%s\177cabinet\177%s", irNodeName.toLatin1().data(),
	irFilePath.toLatin1().data(), irCabName.toLatin1().data());
	if (!irCabName.isEmpty() && !irFilePath.isEmpty() && !irNodeName.isEmpty())	
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = submitError(ipRcvr, AERR_BADCABINET, geExportNode, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Get connection statistics.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 */
ASXID AAppClient::getConnectionStats(AReturnRcvr* ipRcvr)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getconnectionstats");
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, 0/*CustomData*/);
}

/*!
 * \brief Get the currently buffered console output.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iClear Truncate log after fetching current contents.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getconsolelog|clear|%s|sessionid|%d|wait|%d
 * -# The Lisp expression may contain DEL characters.
 * -# Mostly used by HTTP sessions that ask for console output and disconnected sessions that do not have a client to receive 
 * console output until reconnect.
 * -# This connection must have an open session that has buffered output waiting.
 * -# Buffering must be enabled due to a protocol configuration parameter or due to a call to openConsoleLog.
 */
AXID AAppClient::getConsoleLog(AReturnRcvr* ipRcvr, bool iClear, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getconsolelog\177clear\177%s\177sessionid\1770", (iClear ? "true" : "false"));
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Returns Del-delimited list of named parameters.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irContextName	Name of target context.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getcontextparams|context|%s
 */
AXID AAppClient::getContextParams(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData)
{
	long aXid = 0, aStatus = 0;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getcontextparams\177context\177%s", irContextName.toLatin1().data());
	if (irContextName.isEmpty())
		aStatus = AERR_UNKCONTEXTNAME;
	else if (!cIsConnected)
		aStatus = AERR_DISCONNECTED;
	else if (cUsrId <= 0)
		aStatus = AERR_NOLOGON;
	else
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	if (aStatus > 0)
		aXid = submitError(ipRcvr, aStatus, geGetContextParams, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

// See appclient.h for:
// getCurrentContextName, getContextParam,
// getCurrentExtentName, getCurrentExtentNames, getCurrentRepositoryNames
// getSessionId

/*!
 * \brief Get directory information from AIS.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irDir Name of directory, &root for root dir, or &app for _path dir.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:	_ais|getdirinfo|dir|%s
 * -# Returns, via returnOutput, the specified directory information as a set of strings.
 * -# Each string is a DEL-delimited string of the form type|name|size|date|time
 * -# Type is one of the following letters: D=drive, R=rootdir, C=currentdir, D=directory, F=file
 */
AXID AAppClient::getDirInfo(AReturnRcvr* ipRcvr, QString& irDir, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getdirinfo\177dir\177%s", irDir.toLatin1().data());
	if (!irDir.isEmpty())
		aXid = submit(ipRcvr,  aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = submitError(ipRcvr,  AERR_NOINPUT, geGetDirInfo, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Determine session ID of currently executing session for the current context.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getexesession|context|%s
 * -# The null session ID is returned if no session is currently executing.
 */
AXID AAppClient::getExeSession(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getexesession");
	return submit(ipRcvr,  aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Fetch a list of cabinet names from the current context.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getextentnames
 */
AXID AAppClient::getExtentNames(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	return submit(ipRcvr,  "_ais\177getextentnames", cMt/*ClientData*/, 0/*ClientValue*/,NULL/*Data*/, 0/*Size*/,ipCustomData);
}

/*!
 * \brief Fetch a list of cabinet names together with their cabinet import/export status from the current context.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getextentstatus
 */
AXID AAppClient::getExtentStatus(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	return submit(ipRcvr,  "_ais\177getextentstatus", cMt/*ClientData*/, 0/*ClientValue*/,NULL/*Data*/, 0/*Size*/,ipCustomData);
}

/*!
 * \brief Fetch a list of extent types for the current extent.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irForce if true, refresh the current list of server types.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getextenttypes|extent|%s
 */
AXID AAppClient::getExtentTypes(AReturnRcvr* ipRcvr, bool irForce, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getextenttypes\177extent\177%s", cCurExtentName.toLatin1().data());
	if (cExtents.contains(cCurExtentName) && (cExtents.value(cCurExtentName).mServerTypes.count() == 0 || irForce))
		aXid = submit(ipRcvr,  aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = submitError(ipRcvr,  AERR_NOEXTENT, geGetExtentTypes, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Retrieve the cabinet's meta data information
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irCabName Cabinet name to retrieve the meta data information from.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getmetadata|extent|%s
 */
AXID AAppClient::getMetaData(AReturnRcvr* ipRcvr, const QString& irCabName, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getmetadata\177extent\177%s", irCabName.toLatin1().data());
	return submit(ipRcvr,  aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/,NULL/*Data*/, 0/*Size*/,ipCustomData);
}

/*!
 * \brief Get nodes for the level above the current node.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getnextlevel|extent|%s|nodePath|%s|options|%s
 */
AXID AAppClient::getNextLevel(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	if (!cCurExtentName.isEmpty() && cExtents.contains(cCurExtentName))
	{	QString aOptions = getExtentTypeOptions(cCurExtentName);
		aAmpmsg.sprintf("_ais\177getnextlevel\177extent\177%s\177nodePath\177%s\177options\177%s",cCurExtentName.toLatin1().data(),
		cExtents.value(cCurExtentName).mNodePath.toLatin1().data(), getExtentTypeOptions(cCurExtentName).toLatin1().data());
		aXid = submit(ipRcvr,  aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
	{	aAmpmsg.sprintf("_ais\177getnextlevel\177extent\177%s\177nodePath\177none\177options\177none", cCurExtentName.toLatin1().data());
		aXid = submitError(ipRcvr,  AERR_NOEXTENT, geGetNextLevel, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	}
	return aXid;
}

/*!
 * \brief Get logon statistics.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 */
ASXID AAppClient::getLogonStats(AReturnRcvr* ipRcvr)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getlogonstats");
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, 0/*CustomData*/);
}


/*!
 * \brief Get nodes for the level below the current node.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irFullNodeName Full node path from root node.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getnextlevel|extent|%s|nodePath|%s|options|%s
 */
AXID AAppClient::getNextLevel(AReturnRcvr* ipRcvr, const QString& irFullNodeName, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getnextlevel\177extent\177%s\177nodePath\177%s\177options\177",cCurExtentName.toLatin1().data(),
	irFullNodeName.toLatin1().data());
	if (!cCurExtentName.isEmpty() && cExtents.contains(cCurExtentName))
	{	QString aOptions = getExtentTypeOptions(cCurExtentName);
		aAmpmsg += aOptions;
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
		aXid = submitError(ipRcvr,  AERR_NOEXTENT, geGetNextLevel, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Get the selected node from array of nodes.
 *
 * \param[in] iNodeIndex Index into node array.
 *
 * \return aNodeInfo - Node structure for this node.
 *
 * \note
 * -# A local call that returns results directly.
 */
ANodeInfo AAppClient::getNode(long iNodeIndex)
{
	ANodeInfo aNodeInfo;
	if (cExtents.contains(cCurExtentName))
	{	const AExtentInfo& arExtentInfo = cExtents.value(cCurExtentName);
		const ANodeVector& arNodes = arExtentInfo.mNodes;
		if (iNodeIndex >= 0 && iNodeIndex < arNodes.count())
		{	aNodeInfo = arNodes[iNodeIndex];
			// qDebug() << "AppClient::getNode, Value:" << aNodeInfo.mValue << " Symbol:" << aNodeInfo.mSymbol << " Key:" <<
			// aNodeInfo.mUniqueKey << " Type:" << aNodeInfo.mType << " Size:" << aNodeInfo.mDate << " Time:" << aNodeInfo.mTime <<
			// " Version:" << aNodeInfo.mVersion;
		}
	}
	return aNodeInfo;
}

/*!
 * \brief Get the server actions for this node.
 *
 * \param[in] iNodeIndex Index into node array.
 *
 * \return List of strings containing actions.
 *
 * \note
 * -# A local call that returns results directly.
 */
QStringList AAppClient::getNodeActions(long iNodeIndex)
{
	QStringList aActions;
	if (cExtents.contains(cCurExtentName))
	{	const ANodeVector& arNodes = cExtents.value(cCurExtentName).mNodes;
		if (iNodeIndex >= 0 && iNodeIndex < arNodes.size())
		{	QString aType = arNodes[iNodeIndex].mType;
			aActions = getServerTypeActions(aType, cCurExtentName);
		}
	}
	return aActions;
}

/*!
 * \brief Get node path.
 *
 * \return List of strings containing path.
 *
 * \note
 * -# local call that returns results directly.
 */
QStringList AAppClient::getNodePathTree()
{
	QStringList aNodePathTree;
	if (cExtents.contains(cCurExtentName))
	{	const AExtentInfo& arInfo = cExtents.value(cCurExtentName);
		QString temp = arInfo.mNodePath;
		QStringList aNodePaths = arInfo.mNodePath.split('/', QString::KeepEmptyParts);
		long aNumNodes = aNodePaths.count();
		aNodePathTree += "/";
		for (long i = 0; i < aNumNodes; ++i)
		{	QString aPath;
			for (long k=0; k <= i; ++k)
			{	aPath += "/";
				aPath += aNodePaths[k];
			}
			aNodePathTree += aPath;
		}
	}
	return aNodePathTree;
}

/*!
 * \brief Get the node type for the node given the extent and node name.
 *
 * \param[in] irExtentName Name of cabinet.
 * \param[in] irFullNodeName Full path to node.
 *
 * \return Node Type.
 *
 * \note
 * -# A local call that returns results directly.
 * -# Returns a null string if cCurExtentName not set or if node name not found.
 * -# Slow but useful in some cases. Use this call with care - usually for getting the type for a single node.
 * -# Having this call makes it/ easier to code some things in the GUI when node index is not handy.
 */
QString AAppClient::getNodeType(QString& irExtentName, QString& irFullNodeName)
{
	QString aType;
	if (cExtents.contains(cCurExtentName))
	{	ANodeVector aNodeVector = cExtents.value(cCurExtentName).mNodes;
		long iNumNodes = aNodeVector.count();
		for (long i=0; i < iNumNodes; ++i)
		{	if (getFullNodeName(irExtentName,i) == irFullNodeName)
			{	aType = aNodeVector[i].mType;
				break;
			}
		}
	}
	return aType;
}

/*!
 * \brief Get nodes for the level above the current node.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getnextlevel|extent|%s|nodePath|%s|options|%s
 */
AXID AAppClient::getPreviousLevel(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	if (!cCurExtentName.isEmpty() && cExtents.contains(cCurExtentName))
	{	const AExtentInfo& arInfo = cExtents.value(cCurExtentName);
		QStringList aNodes = arInfo.mNodePath.split('/', QString::KeepEmptyParts);
		if (aNodes.count() == 0)
			aXid = -AERR_ATBEG;		// already at top level
		else
		{	aNodes.pop_back(); // remove last item
			QString aNodePath = aNodes.join("/");
			QString aOptions = getExtentTypeOptions(cCurExtentName);
			aAmpmsg.sprintf("_ais\177getnextlevel\177extent\177%s\177nodePath\177%s\177options\177%s",cCurExtentName.toLatin1().data(),
			aNodePath.toLatin1().data(), aOptions.toLatin1().data());
			aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
		}
	}
	else
		aXid = -AERR_NOEXTENT;

	if (aXid < 0)
	{	aAmpmsg.sprintf("_ais\177getnextlevel\177extent\177%s\177nodePath\177none\177options\177none", cCurExtentName.toLatin1().data());
		aXid = submitError(ipRcvr, -aXid, geGetNextLevel, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	}	
	return aXid;
}

/*!
 * \brief Get request statistics.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 */
ASXID AAppClient::getRequestStats(AReturnRcvr* ipRcvr)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getrequeststats");
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, 0/*CustomData*/);
}

/*!
 * \brief Get nodes at the top level.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getnextlevel|extent|%s|nodePath|%s|options|%s
 */
AXID AAppClient::getRootLevel(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	long aXid = 0;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getnextlevel\177extent\177%s\177nodePath\177\177options\177", cCurExtentName.toLatin1().data());
	if (!cCurExtentName.isEmpty() && cExtents.contains(cCurExtentName))
	{	QString aOptions = getExtentTypeOptions(cCurExtentName);
		aAmpmsg += aOptions;
		aXid = submit(ipRcvr,  aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
		aXid = submitError(ipRcvr,  AERR_NOEXTENT, geGetNextLevel, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Get list of actions for a given extent and type.
 *
 * \param[in] irType Type of actions.
 * \param[in] irExtentName Name of extent.
 *
 * \return List of strings containing actions.
 *
 * \note
 * -# A local call that returns results directly.
 */
QStringList AAppClient::getServerTypeActions(const QString& irType, const QString& irExtentName) 
{
	QStringList aServerActionList;
	if (cExtents.contains(irExtentName))
	{	QString aDefault(".default.");
		const AExtentInfo& arExtentInfo = cExtents.value(irExtentName);
		const AExtentTypeMap& arServerTypes = arExtentInfo.mServerTypes;
        AExtentTypeInfo aServerTypeInfo;
		const AExtentTypeInfo *apServerTypeInfo = &aServerTypeInfo;

        if (arServerTypes.contains(irType))
            aServerTypeInfo = arServerTypes.value(irType);
        else if (arServerTypes.contains(".default."))
            aServerTypeInfo = arServerTypes.value(".default.");
        else
            apServerTypeInfo = NULL;

		if (apServerTypeInfo != NULL)
		{	QString aServerActions = apServerTypeInfo->mActionCodes;
			aServerActionList = aServerActions.split(',', QString::KeepEmptyParts);
		}
	}
	return aServerActionList;
}

/*!
 * \brief Get session statistics.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 */
ASXID AAppClient::getSessionStats(AReturnRcvr* ipRcvr)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getsessionstats");
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, 0/*CustomData*/);
}

/*!
 * \brief Sends a geGetUsers AMP request to the AIS Server.
 *
 * \param[in] ipRcvr Return receiver object pointer.
 *
 * \return Request Id.
 */
ASXID AAppClient::getUsers(AReturnRcvr* ipRcvr)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177getUsers");
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/);
}

// getSessionId is in appclient.h

/*!
 * \brief Retrieve workspace statistics from the server.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getworkspacestatistics
 */
AXID AAppClient::getWorkspaceStatistics(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	QString aAmpmsg("_ais\177getworkspacestatistics");
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Load a cabinet from a .sl file on the server.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irCabName Cabinet to be loaded.
 * \param[in] irFilePath Path to .sl file containing cabinet definitions.
 * \param[in] irSetLocation Flag to set the .sl file as cabinet's source location.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|importcabinet|cabinet|%s|file|%s
 * -# The existing cabinet contents will be lost.
 */
AXID AAppClient::importCabinetRemote(AReturnRcvr* ipRcvr, QString& irCabName, QString& irFilePath, bool iSetLocation, void* ipCustomData)
{
	long aStatus = 0, aXid = 0;
	QString aAmpmsg;
	QString aSetLocation = "false";
	if( iSetLocation == true )
		aSetLocation = "true";
		
	if (!irCabName.isEmpty() && !irFilePath.isEmpty())
	{	aAmpmsg.sprintf("_ais\177importcabinet\177cabinet\177%s\177file\177%s\177setlocation\177%s", irCabName.toLatin1().data(),
		irFilePath.toLatin1().data(), aSetLocation.toLatin1().data());
		aXid = submit(ipRcvr,  aAmpmsg, irCabName, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
		aStatus = AERR_BADCABINET;
	
	if (aStatus > 0)
	{	aAmpmsg.sprintf("_ais\177importcabinet\177cabinet\177%s\177file\177%s",irCabName.toLatin1().data(),irFilePath.toLatin1().data());
		aXid = submitError(ipRcvr,  aStatus, geImportCabinet, aAmpmsg, irCabName/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	}
	return aXid;
}

/*!
 * \brief Load a cabinet from a folder on the server.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irCabName Cabinet to be loaded.
 * \param[in] irFilePath Path to folder containing cabinet definitions.
 * \param[in] irSetLocation Flag to set the folder as cabinet's source location.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|importcabinet|cabinet|%s|folder|%s
 * -# The existing cabinet contents will be lost.
 */
AXID AAppClient::importCabinetRemoteDir(AReturnRcvr* ipRcvr, QString& irCabName, QString& irFilePath, bool iSetLocation, void* ipCustomData)
{
	long aStatus = 0, aXid = 0;
	QString aAmpmsg;
	QString aSetLocation = "false";
	if( iSetLocation == true )
		aSetLocation = "true";
	if (!irCabName.isEmpty() && !irFilePath.isEmpty() && cExtents.contains(irCabName))
	{	aAmpmsg.sprintf("_ais\177importcabinet\177cabinet\177%s\177folder\177%s\177setlocation\177%s", irCabName.toLatin1().data(),
		irFilePath.toLatin1().data(), aSetLocation.toLatin1().data());
		aXid = submit(ipRcvr,  aAmpmsg, irCabName, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
		aStatus = AERR_BADCABINET;
	
	if (aStatus > 0)
	{	aAmpmsg.sprintf("_ais\177importcabinet\177cabinet\177%s\177folder\177%s",irCabName.toLatin1().data(),irFilePath.toLatin1().data());
		aXid = submitError(ipRcvr,  aStatus, geImportCabinet, aAmpmsg, irCabName/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	}
	return aXid;
}
// logoff - use close connection.

/*!
 * \brief Call server to create a cabinet.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irCabName	New cabinet's name.
 * \param[in] irCabPath	Path to place to put the cabinet.
 * \param[in] irCabLocation	Path to the Lisp Source.
 * \param[in] irStorage	Storage Type either File or Folder
 * \param[in] irImportSync Import Syncronization flag
 * \param[in] irExportSync Export Syncronization flag
 * \param[in] irAutoCompile Auto Compilation flag
 * \param[in] irSearchSwitch Search Switch flag
 * \param[in] irDependencies List of dependent cabinets each separated by comma
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|newcabinet|cabinet|%s|path|%s|location|%s|storage|%s|importsync|%s|exportsync|%s|autocompile|%s|force|%s

 */
AXID AAppClient::newCabinet(AReturnRcvr* ipRcvr, QString& irCabName, QString& irCabPath, QString& irCabLocation, QString& irStorage,
	QString& irImportSync, QString& irExportSync, QString& irAutoCompile, QString& irSearchSwitch, QString& irDependencies, void* ipCustomData)
{
	long aStatus = 0, aXid = 0;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177newcabinet\177cabinet\177%s\177path\177%s\177location\177%s\177storage\177%s\177importsync\177%s\177exportsync\177%s\177autocompile\177%s\177searchsw\177%s\177dependencies\177%s",
		irCabName.toLatin1().data(), irCabPath.toLatin1().data(), irCabLocation.toLatin1().data(), irStorage.toLatin1().data(), irImportSync.toLatin1().data(), irExportSync.toLatin1().data(),
		irAutoCompile.toLatin1().data(), irSearchSwitch.toLatin1().data(), irDependencies.toLatin1().data());
	if (!irCabPath.isEmpty() && !irCabName.isEmpty())
	{	if (cExtents.contains(irCabName))
			aStatus = AERR_CABINETOPEN; // cabinet already exists!
		else
			aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
		aStatus = AERR_BADCABINET;

	if (aStatus > 0)
		aXid = submitError(ipRcvr, aStatus, geNewCabinet, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Close the named context on AIS.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irContextName	Reference to context name.
 * \param[in] iMode ACloseMode (geDefault, geOpen, geDisconnect, geSoft, geClear, geStop).
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|closecontext|context|%s|closemode|%s
 * -# The iMode argument is long because queued signals do not recognize the ACloseMode data type.
 */
AXID AAppClient::onCloseContext(AReturnRcvr* ipRcvr, const QString& irContextName, long iMode, void* ipCustomData)
{
	long aXid = 0, aStatus = 0;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177closecontext\177context\177%s\177closemode\177%s",irContextName.toLatin1().data(),gAis.mpCloseMode[iMode]);
	
	if (irContextName.isEmpty())
		aStatus = AERR_UNKCONTEXTNAME;
	else if (!cIsConnected)
		aStatus = AERR_DISCONNECTED;
	else if (cUsrId <= 0)
		aStatus = AERR_NOLOGON;
	else
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);	
	if (aStatus > 0)
		aXid = submitError(ipRcvr, aStatus, geCloseContext, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Close an open session.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iSessionId ID of session to be closed.
 * \param[in] iMode ACloseMode (geDefault, geOpen, geDisconnect, geSoft, geClear, geStop).
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|closesession|sessionid|%d|closemode|%s
 * -# ACloseMode is not an allowed argument type for queued connections so ACloseMode is converted to long.
 */
AXID AAppClient::onCloseSession(AReturnRcvr* ipRcvr, long iSessionId, long iMode, void* ipCustomData)
{
	//	If session ID is null, close the current session
	if (iSessionId <= 0)
		iSessionId = cSessionId;

	long aXid = 0, aStatus = 0;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177closesession\177sessionid\177%ld\177closemode\177%s", iSessionId, gAis.mpCloseMode[iMode]);
	if (!cIsConnected)
		aStatus = AERR_DISCONNECTED;
	else if (cUsrId <= 0)
		aStatus = AERR_NOLOGON;
	else if (iSessionId <= 0)
		aStatus = AERR_SESSIONID;
	else
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	if (aStatus > 0)
		aXid = submitError(ipRcvr, aStatus, geCloseSession, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Catches connected signal from ASocket.
 *
 * \note
 * -# An openConnection call generates a connectToHost in ASocket. On connection, the connected signal is sent back here.
 * -# If the connection fails, a socketError signal is caught by onSocketError shown below.
 */
void AAppClient::onConnected()
{
	QString aOut("true");
	
	cConnectionId = cpSocket->socketDescriptor();
	returnOutput(0/*Dummy*/, cOpenConnXid, 0/*Status*/, geOpenConnection, 0/*RetValue*/, aOut, NULL, 0, ""/*Display*/, ""/*Error*/);
}

/*!
 * \brief Catches disconnected signal from ASocket.
 *
 * \note
 * -# If the server terminates the connection or if the network fails, ASocket generates a disconnected signal which is caught
 * here.
 * -# If closeConnection is called, it causes the server to terminate the connection, which generates the above sequence of
 * events.
 */
void AAppClient::onDisconnected()
{
	connectionClosed(0/*DummyId*/);
}

/*!
 * \brief Returns tab-delimited list of contexts via returnOutput.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getcurrentcontexts
 */
AXID AAppClient::onGetCurrentContexts(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	QString aAmpmsg("_ais\177getcurrentcontexts");
	return submit(ipRcvr,  aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Get current list of subscriptions for the named context.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irContextName	Name of target context.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getsubscriptions|context|%s
 * -# returnOutput returns newline-terminated records, each record holds one of the following the tab-delimited fields:
 *   - SessionID\\tContextName\\tAdmin\\tUserName\\tProtocolName
 *   - ContextName\\tReg
 *   - ProtocolName
 * -# The subscriptions of the cur. connection are prefixed with an asterisk.  Unsubscribed entries are prefixed with a space.
 */
AXID AAppClient::onGetSubscriptions(AReturnRcvr* ipRcvr, const QString& irContextName,void* ipCustomData)
{
	QString aAmpmsg("_ais\177getsubscriptions");
	if (!irContextName.isEmpty())
		aAmpmsg += "\177context\177" + irContextName;
	return submit(ipRcvr,  aAmpmsg,  cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Get current list of sessions for the named context.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irContextName Name of target context.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getsessions|context|%s
 * -# returnOutput returns newline-terminated records, each record holds one of the following the tab-delimited fields:
 *   - SessionID\\tContextName\\tAdmin\\tUserName\\tProtocolName
 *   - ContextName\\tReg
 *   - ProtocolName
 * -# The subscriptions of the cur. connection are prefixed with an asterisk.  Unsubscribed entries are prefixed with a space.
 */
AXID AAppClient::onGetSessions(AReturnRcvr* ipRcvr, const QString& irContextName,void* ipCustomData)
{
	QString aAmpmsg("_ais\177getsessions");
	if (!irContextName.isEmpty())
		aAmpmsg += "\177context\177" + irContextName;
	return submit(ipRcvr,  aAmpmsg,  cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Determine open status of a context.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irContextName	The name of the target context.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|iscontextopen|context|%s
 * -# The Lisp expression may contain DEL characters
 */
AXID AAppClient::onIsContextOpen(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg("_ais\177iscontextopen\177context\177" + irContextName);
	if (!irContextName.isEmpty())
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = submitError(ipRcvr, AERR_UNKCONTEXTNAME, geIsContextOpen, aAmpmsg, cMt/*Data*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Same as closeConnection
 *
 * \return Request ID for this request.
 */
AXID AAppClient::onLogoff(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177logoff");
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/ , 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Set new user name and password.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irUsrName	User's logon name (jdoe).
 * \param[in] irPasswd Password (not encrypted).
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|logon|user|%s|passwd|%s|context|%s|wait|%d
 * -# If already logged on, logon just changes the current logon parameters.
 */
AXID AAppClient::onLogon(AReturnRcvr* ipRcvr, const QString& irUsrName, const QString& irPasswd, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177logon\177user\177%s\177passwd\177%s",irUsrName.toLatin1().data(), irPasswd.toLatin1().data());
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/ , 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Establish an APP client connection to server.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] ipCustomData An anonymous ptr returned to caller with response.
 *
 * \return aXid - Request ID (incrementing integer > 0).
 *
 * \note
 * -# AMP format:  _ais|openconnection
 */
AXID AAppClient::onOpenConnection(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	// Save args for use in returnOutput. openConnection posts an event and returns immediately.
	long aStatus, aXid = 0;
	QString aAmpmsg("_ais\177openconnection");
	long aPendingReq = 0;

	mMutex.lock();
	aPendingReq = cClientRequests.size();
	mMutex.unlock();

	if (cIsConnected)
		aStatus = 0;				// Return quietly
	else if (cSessionId > 0)
		aStatus = AERR_SESSIONOPEN;
	else if (cHostDnsIp == AGLBLS_INPROCSVRNAME)
	{	cpInProcSvr = gAis.mpInProcSvr;
		if (cPort > 0 || cpInProcSvr == NULL)
			aStatus = AERR_UNKHOST;
		else
		{	cIsConnected = true;
			aStatus = 0;
		}
	}
	else if (aPendingReq > 0)
		aStatus = AERR_REQPENDING;
	else 
	{	AClientReqSt aReqSt;
		aReqSt.mAmpMsg = aAmpmsg;
		aReqSt.mClientData = cMt;
		aReqSt.mClientValue = 0;
		aReqSt.mpCustomData = ipCustomData;
		aReqSt.mpRcvr = ipRcvr;
		aReqSt.mReqType = geOpenConnection;
		mMutex.lock();
		cClientRequests[aXid = ++cXid] = aReqSt;
		mMutex.unlock();
		cOpenConnXid = aXid;
		if (cpSocket == NULL)
		{	cpSocket = new ASocket(this, cServerName.toLatin1().data(), AISSVR_APP);
			QObject::connect(cpSocket, SIGNAL(connected()), this, SLOT(onConnected()), Qt::DirectConnection);
			QObject::connect(cpSocket, SIGNAL(disconnected()), this, SLOT(onDisconnected()), Qt::DirectConnection);
			QObject::connect(cpSocket, SIGNAL(response(QByteArray, QByteArray, char*)), this
			, SLOT(onResponse(QByteArray, QByteArray, char*)),Qt::DirectConnection);
			QObject::connect(cpSocket, SIGNAL(socketError(long, QString)), this, SLOT(onSocketError(long,QString))
			, Qt::DirectConnection);
		}
		if (cpSocket->openConnection(cHostDnsIp, cPort))
			aStatus = -1;
		else
			aStatus = AERR_REQPENDING;
	}
	if (aStatus >= 0)
		aXid = submitError(ipRcvr, aStatus, geOpenConnection, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;		
}

/*!
 * \brief Start up an application on the server.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irStartupPath Path/filename of startup script (may be empty).
 * \param[in] irContextName	Name of context to be opened.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|opencontext|context|%s|path|%s
 * -# The path is not necessary if the context is already registered.
 * -# The context name is not necessary if the path specifies an existing startup file. The context name is set during
 * registration.
 */
AXID AAppClient::onOpenContext(AReturnRcvr* ipRcvr,const QString& irStartupPath,const QString& irContextName,void* ipCustomData)
{
	long aXid = -AERR_UNKCONTEXTNAME;
	QString aAmpmsg("_ais\177opencontext");
	if (!irContextName.isEmpty())
	{	aXid = 0;
		aAmpmsg += "\177context\177" + irContextName;
	}
	if (!irStartupPath.isEmpty())
	{	aXid = 0;
		aAmpmsg += "\177path\177" +irStartupPath;
	}
	if (aXid == 0)
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = submitError(ipRcvr, -aXid, geOpenContext, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Open a session on the server to process Lisp expressions or AMP messages.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irContextName	Session is opened on this context.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|opensession|context|%s|userid|%d|closemode|%s
 * -# A successful logon must precede this call.
 */
AXID AAppClient::onOpenSession(AReturnRcvr* ipRcvr, const QString& irContextName, void* ipCustomData)
{
	long aXid = 0, aStatus = 0;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177opensession\177context\177%s\177userid\177%ld", irContextName.toLatin1().data(), cUsrId);
	if (irContextName.isEmpty())
		aStatus = AERR_UNKCONTEXTNAME;
	else if (!cIsConnected)
		aStatus = AERR_DISCONNECTED;
	else if (cUsrId <= 0)
		aStatus = AERR_NOLOGON;
	else if (cSessionId > 0)
		aStatus = 0;			// AERR_SESSIONOPEN;
	else
	{	aStatus = -1;			// Skip error return
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	if (aStatus >= 0)
		aXid = submitError(ipRcvr, aStatus, geOpenSession, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Catches response signal from ASocket.
 *
 * \param[in] iResponse Message of the form RqId|ReqType|Status|RetValue|Display|Error|Out where RqId,Status,RetValue are integers.
 * \param[in] iCookie Returns cookie setting, if any (not used by APP protocol).
 * \param[in] ipData Binary data buffer.
 *
 * \note
 * -# ASocket collects transmissions from the server into a complete message and then generates a response signal.
 * -# If multiple messages are included in a single transmission, a single signal is generated for each complete message.
 */
void AAppClient::onResponse(QByteArray iResponse, QByteArray iCookie, char* ipData)
{
    Q_UNUSED(iCookie);

	long aRetValue, aRqId, aStatus;
	const char* apBeg = iResponse.data();
	AReqType aReqType;
	QString aOut, aDisplay, aErr;

	// RqId. Extract returned RqId
	apBeg += AUtil::getArgValue(apBeg,&aRqId);		// ->beg of msg after RqId + DEL

	// Request type.
	apBeg += AUtil::getArg(apBeg, '\177', aOut, true/*Trim*/, true/*ToLower*/); // ->beg of msg after ReqType + DEL
	aReqType = REQTYPE(aOut);

	// Status.
	apBeg += AUtil::getArgValue(apBeg, &aStatus);		// ->beg of msg after Status + DEL		
	
	// RetValue.
	apBeg += AUtil::getArgValue(apBeg, &aRetValue);		// ->beg of msg after RetValue + DEL			

	// Display.
	apBeg += AUtil::getRecord(apBeg, '\177', aDisplay);	// ->beg of msg after Display + DEL
		
	// Error.
	apBeg += AUtil::getRecord(apBeg, '\177', aErr);		// ->beg of msg after Error + DEL		

	// Out.
	aOut = apBeg;									// Return entire rest of the message


	returnOutput(0, aRqId, aStatus, aReqType, aRetValue, aOut, ipData, 0/*DataSize*/, aDisplay, aErr);
}

/*!
 * \brief Catches the socketError signal from ASocket.
 *
 * \param[in] iStatus The error code for the cause of the error.
 * \param[in] iError The message generating an error or an explanation of the network failure.
 *
 * \note
 * -# See aerrmsgs.h in autilities for a description of the error codes.
 */
void AAppClient::onSocketError(long iStatus, QString iError)
{
	// socket error occurred
	AReqType aCurReqType = geConnectionError;
	long aCurXid = 0;

	if (cClientRequests.size() > 0)
	{
		// get the first request id and type
		aCurXid = cClientRequests.keys().first();
		aCurReqType = cClientRequests[aCurXid].mReqType;
	}

	returnOutput(0, aCurXid, iStatus, aCurReqType, 0/*Value*/, ""/*Out*/, NULL/*Data*/, 0/*DataSize*/, ""/*Display*/, iError);
}

/*!
 * \brief Call server to evaluate an AisLisp expression.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irExp A AisLisp expression.
 * \param[in] ipData Pointer to binary data stream.
 * \param[in] iDataSize Length of binary data.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|eval|exp|%s
 * -# The Lisp expression may contain DEL characters
 */
AXID AAppClient::onSubmit(AReturnRcvr* ipRcvr, const QString& irExp, char *ipData, long iDataSize, void* ipCustomData)
{
	QString aAmpmsg("_ais\177eval\177exp\177" + irExp);
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipData, iDataSize, ipCustomData);
}

/*!
 * \brief Call server to evaluate an AisLisp expression and return a binary result.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irExp A AisLisp expression.
 * \param[in] ipData Pointer to binary data stream.
 * \param[in] iDataSize Length of binary data.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|execute|exp|%s
 * -# The Lisp expression may contain DEL characters
 */
AXID AAppClient::onSubmitBin(AReturnRcvr* ipRcvr, const QString& irExp, char *ipData, long iDataSize, void* ipCustomData)
{
	QString aAmpmsg("_ais\177execute\177exp\177" + irExp);
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipData, iDataSize, ipCustomData);
}

/*!
 * \brief Make contents of an existing cabinet visible.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irCabName Name of existing cabinet.
 * \param[in] irCabPath Path/file name of file holding cabinet.
 * \param[in] irCabLocation	Path to the Lisp Source.
 * \param[in] irStorage	Storage Type either File or Folder
 * \param[in] irImportSync Import Syncronization flag
 * \param[in] irExportSync Export Syncronization flag
 * \param[in] irAutoCompile Auto Compilation flag
 * \param[in] irSearchSwitch Search Switch flag
 * \param[in] irDependencies List of dependent cabinets each separated by comma
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|opencabinet|cabinet|%s|path|%s
 */
AXID AAppClient::openCabinet(AReturnRcvr* ipRcvr, QString& irCabName, QString& irCabPath, QString& irCabLocation, QString& irStorage,
	QString& irImportSync, QString& irExportSync, QString& irAutoCompile, QString& irSearchSwitch, QString& irDependencies, void* ipCustomData)
{
	long aStatus = 0, aXid = 0;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177opencabinet\177cabinet\177%s\177path\177%s\177location\177%s\177storage\177%s\177importsync\177%s\177exportsync\177%s\177autocompile\177%s\177searchsw\177%s\177dependencies\177%s",
		irCabName.toLatin1().data(), irCabPath.toLatin1().data(), irCabLocation.toLatin1().data(), irStorage.toLatin1().data(), irImportSync.toLatin1().data(), irExportSync.toLatin1().data(),
		irAutoCompile.toLatin1().data(), irSearchSwitch.toLatin1().data(), irDependencies.toLatin1().data());
	if (!irCabPath.isEmpty() && !irCabName.isEmpty())
	{	if (cExtents.contains(irCabName))
			aStatus = AERR_CABINETOPEN; // cabinet is already open!
		else	
			aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
		aStatus = AERR_BADCABINET;

	if (aStatus > 0)
		aXid = submitError(ipRcvr, aStatus, geOpenCabinet, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Create a new console output log for the current session.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iClear Iff true, clear the existing log prior to first read.
 * \param[in] iRedirect If true, do not also send console output to existing client, if any.
 * \param[in] iSize Maximum number of bytes to be stored (0 for unlimited).
 * \param[in] iStartAtNewline
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \todo Complete parameter description.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|openconsolelog|clear|%s|redirect|%s|sessionid|%d|size|%d|startatnewline|%s
 * -# iSize <0 use default, 0 unlimited log size, >0 max bytes to be logged
 */
ASXID AAppClient::openConsoleLog(AReturnRcvr* ipRcvr, bool iClear,bool iRedirect,long iSize,bool iStartAtNewline,void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177openconsolelog\177clear\177%s\177redirect\177%s\177sessionid\1770\177size\177%ld\177"
	"startatnewline\177%s", iClear ? "true" : "false", iRedirect ? "true" : "false", iSize,iStartAtNewline ? "true" : "false");
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Make an existing Lambda or variable visible.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irExtentName Name of existing extent containing Lambda/variable.
 * \param[in] irFullNodeName Path to node from the root.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|opennode|extent|%s|node|%s
 */
AXID AAppClient::openNode(AReturnRcvr* ipRcvr, const QString& irExtentName,const QString& irFullNodeName,void* ipCustomData)
{
	long aXid = 0, aStatus = 0;
	QString aAmpmsg, aClientData(irFullNodeName);
	aAmpmsg = QString("_ais\177opennode\177extent\177%1\177node\177%2").arg(irExtentName, irFullNodeName);
	if (!irExtentName.isEmpty() && !irFullNodeName.isEmpty())
	{	if (cExtents.contains(irExtentName))
			aXid = submit(ipRcvr, aAmpmsg, aClientData, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
		else
			aStatus = AERR_NOEXTENT;
	}
	else
		aStatus = AERR_BADNODE;

	if (aStatus > 0)
		aXid = submitError(ipRcvr, aStatus, geOpenNode, aAmpmsg, aClientData, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Update nodes for the current extent and the current node path.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|getnextlevel|extent|%s|nodePath|%s|options|%s
 */
AXID AAppClient::refreshLevel(AReturnRcvr* ipRcvr, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	if (!cCurExtentName.isEmpty() && cExtents.contains(cCurExtentName))
	{	QString aNodePath = cExtents.value(cCurExtentName).mNodePath;
		QString aOptions = getExtentTypeOptions(cCurExtentName);
		aAmpmsg.sprintf("_ais\177getnextlevel\177extent\177%s\177nodePath\177%s\177options\177%s",cCurExtentName.toLatin1().data(),
		aNodePath.toLatin1().data(), aOptions.toLatin1().data());
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
	{	aAmpmsg.sprintf("_ais\177getnextlevel\177extent\177%s\177nodePath\177empty\177options\177none", cCurExtentName.toLatin1().data());
		aXid = submitError(ipRcvr, AERR_NOEXTENT, geGetNextLevel, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	}
	return aXid;
}

/*!
 * \brief Register a context.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irStartupPath Path/file to startup script on server.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|registercontext|path|%s
 * -# Registration initializes the context state from the configuration files and startup script.
 * -# Initialization is complete up to allocation of memory and the startup of a thread.
 * -# The context name is extracted from the configuration steps.
 */
AXID AAppClient::registerContext(AReturnRcvr* ipRcvr, const QString& irStartupPath, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177registercontext\177path\177%s", irStartupPath.toLatin1().data());
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Helper file that carries out the final steps of returnOutput.
 *
 * \param[in] iXid Request ID (incrementing integer) established by submit.
 * \param[in] iStatus Error code >0 if an error.
 * \param[in] iReqType Request type (eval, ampMsg, or built-in command).
 * \param[in] iRetValue	Integer returned from AIS.
 * \param[in] irAisOut Returned message.
 * \param[in] ipData Binary Buffer.
 * \param[in] iDataSize Length of ipData.
 * \param[in] irDisplay	Console output.
 * \param[in] irError Error details.
 * \param[in] iClientData Anonymous data submitted with the request.
 * \param[in] iClearRequestQueueItem If true pending request is removed from the queue.
 *
 * \note
 * -# Returns payload to calling client in an event
 */
void AAppClient::returnMsg(long iXid, long iStatus, AReqType iReqType, long iRetValue, const QString& irAisOut,
						   char *ipData, long iDataSize, const QString& irDisplay, const QString& irError, 
						   const QString iClientData, bool iClearRequestQueueItem)
{
    Q_UNUSED(iDataSize);
    Q_UNUSED(iClientData);

	AReturnRcvr* apRcvr = cpDefaultRcvr;
	QString aOut(irAisOut);
	QString aClientData;
	long aStatus = 0;

	if (iXid > 0)
	{	//	Determine form to catch return.
		mMutex.lock();
		if (cClientRequests.contains(iXid))
		{	AClientReqSt& arReq = cClientRequests[iXid];
			aClientData = arReq.mClientData;
			apRcvr = arReq.mpRcvr;
			// note: previous location of mMutex.lock()
			if (apRcvr == NULL)
			{	aStatus = AERR_NOCLIENT;
				apRcvr = cpDefaultRcvr;
			}
			if (iClearRequestQueueItem)
				cClientRequests.remove(iXid);
			mMutex.unlock();
		}
		else	// No outstanding request. Return error to default form
		{	mMutex.unlock();
			aStatus = AERR_BADREQUEST;
		}
	}
	else if (iStatus > 0)	// Process errors with an unknown xid, such as a timeout.
	{	// Return an error to all pending requests
		AClientReqM::ConstIterator itp;
		long aXid = 0;
		mMutex.lock();
		for (itp = cClientRequests.begin(); itp != cClientRequests.end(); ++itp)
		{	aXid = itp.key();
			const AClientReqSt& arReq = *itp;
			aClientData = arReq.mClientData;
			apRcvr = arReq.mpRcvr;
			mMutex.unlock();
			if (apRcvr == NULL)
				cpDefaultRcvr->returnOutput(getConnectionId(), aXid, AERR_NOCLIENT, iReqType, iRetValue, aOut, ipData, 0, irDisplay, irError
				, aClientData);
			else
				apRcvr->returnOutput(getConnectionId(), aXid, iStatus, iReqType, iRetValue, aOut, ipData, 0, irDisplay, irError, aClientData);
			mMutex.lock();
		}
		cClientRequests.clear();
		mMutex.unlock();

		// Quit unless error not returned even once. Else, send to default form
		if (aXid > 0)
			return;
	}

	// Else, must be pushed output. Return to default form
	if (aStatus > 0)
	{	aOut.sprintf("AAppClient:returnMsg(Xid:%ld,Status:%ld,ReqType:%s,RetValue:%ld,Out:%.128s,Display:%.128s,Error:%.128s"
		",ClientData:%.128s) %s\n", iXid, iStatus, REQNAME(iReqType), iRetValue, irAisOut.toLatin1().data(),
		irDisplay.toLatin1().data(), irError.toLatin1().data(), aClientData.toLatin1().data(), gAis.mpErrMsgs[aStatus]);
		qDebug() << aOut;
		apRcvr = cpDefaultRcvr;
		aOut = "false";
	}
	if (apRcvr == NULL)
	{	aOut.sprintf("AAppClient:returnMsg(Xid:%ld,Status:%ld,ReqType:%s,RetValue:%ld,Out:%.128s,Display:%.128s,Error:%.128s"
		",ClientData:%.128s) Null receiver\n", iXid, iStatus, REQNAME(iReqType), iRetValue, irAisOut.toLatin1().data(),
		irDisplay.toLatin1().data(), irError.toLatin1().data(), aClientData.toLatin1().data());
		qDebug() << aOut;
	}
	else // Return. Return output to client form or widget.
	{
		switch(iReqType)
		{
			case geLogAll:
			case geLogAmp:
			case geLogConsole:
			case geLogReqHdr:
			case geLogSysMsg:
			case geLogUserAccess:
				emit returnStuff(iReqType, aOut);
				break;
			default:
				if (apRcvr != NULL)
					apRcvr->returnOutput(getConnectionId(), iXid, iStatus, iReqType, iRetValue, aOut, ipData, 0, irDisplay, irError, aClientData);
		}
	}
}

/*!
 * \brief Catches returned messages and request results from server.
 *
 * \param[in] iDummyId Placeholder to fit returnOutput signature.
 * \param[in] iXid Request ID (incrementing integer) established by submit.
 * \param[in] iStatus Error code >0 if an error.
 * \param[in] iReqType Request type (eval, ampMsg, or built-in command).
 * \param[in] iRetValue Integer returned from AIS.
 * \param[in] irOut	Returned message.
 * \param[in] ipData Binary Buffer data.
 * \param[in] iDataSize	Length of ipData buffer.
 * \param[in] irDisplay	Console output.
 * \param[in] irError Error details.
 * \param[in] iClientData Anonymous data submitted with the request.
 *
 * \note
 * -# Returns payload to calling client in an event.
 * -# This method may be reimplemented in classes that inherit AAppClient.
 */
void AAppClient::returnOutput(long iDummyId, long iXid, long iStatus, AReqType iReqType, long iRetValue, 
							  const QString& irOut, char* ipData, long iDataSize, const QString& irDisplay,
							  const QString& irError, const QString iClientData)
{
    Q_UNUSED(iDummyId);
    Q_UNUSED(iClientData);

	QString aClientData, aOut(irOut);
	long aSessionId = cSessionId;
	bool aClearRequestQueueItem = true;			// Output event has request queue entry
	bool aOk;
	long aExpXid = 0;

	if (iXid > 0)				// Response to a request.
	{
		mMutex.lock();
		if (cClientRequests.size() == 0)
		{
			// we are not expecting any responses
			qDebug("AAppClient::returnOutput(), Unexpected response Xid = %ld", iXid);
		}
		else
		{
			// get the oldest request expected
			aExpXid = cClientRequests.keys().first();
			// geCloseConnection is an exception, see connectionClosed() and submitError()
			if (aExpXid != iXid && iReqType != geCloseConnection)
				qDebug("AAppClient::returnOutput(), Expected Xid = %ld, Received Xid = %ld", aExpXid, iXid);

			// Note: In cases where a response will take a while to arrive,
			// Issuing another request with an immediate response will flag this error
		}
		mMutex.unlock();
	}
	if (iStatus > 0)	
		goto lErrorRet;

	switch(iReqType)
	{	// Forward these returns directly to initiating form.
	case geGetContextParams:	// call to SMgr.getContextParams
		AUtil::stringToStringMap(irOut, cContextParams);
	case geAddUser:
	case geAmpMsg:				// call to SMgr.submit
	case geCheckCabinet:		// call to browseLib.checkStatus
	case geCloseCabinet:		// call to (browseLib.dropExtent cabName cabPath)
	case geCloseConnection:		// call to AisSvr.closeConnection
	case geCloseContext:		// call SMgr.closeContext
	case geCloseSession:		// call SMgr.closeSession
	case geCompileLambda:		// call to browseLib.compileSource
	case geCompileCabinet:		// call to (browseLib.compileAll true)
	case geConnectSession:		// call to SMgr.connectSession
	case geDebug:				// call to SMgr.submit(ipRcvr, SBGLUE_DEBUGCMD)
	case geDeleteUser:
	case geEnableConsoleLog:	// call to SMgr.closeConsoleLog
	case geEraseNode:			// call to browseLib.eraseSource
	case geEval:				// call to SMgr.submit
	case geExecute:				// call to SMgr.submit
	case geExportNode:			// call to browseLib.exportSource 
	case geExportCabinet:		// call to browseLib.exportSource
	// case geFcnError:			// expect iStatus > 0
	case geGetConnectionStats:
	case geGetConsoleLog:		// call to SMgr.getConsoleLog
	case geGetContextId:		// call to SMgr.getContextId
	case geGetCurrentContexts:	// call to SMgr.getCurrentContexts
	case geGetDirInfo:			// call to SMgr.submit(ipRcvr, SBGLUE_DIRINFO)
	case geGetExeSession:		// call to SMgr.getExeSession
	case geGetLogonStats:
	case geGetRequestStats:
	case geGetSessionId:		// call to AisClient.submit
	case geGetSessions:			// call to SMgr.getSessions
	case geGetSessionStats:
	case geGetSessionUser:		// call to SMgr.getSessionUser
	case geGetSubscriptions:	// call to SMgr.getSubscriptions
	case geGetUsers:
	case geImportCabinet:		// call to browseLib.importSource
	case geIsContextBusy:		// call to SMgr.isContextBusy
	case geIsContextOpen:		// call to SMgr.isContextOpen
	case geLogSysMsg:			// call to AisSvr.logSysmsg
	// case geNewCabinet:			// call to (browseLib.addExtent cabName cabPath)
	case geNoop:				// call to AisMgr.submit
	case geOpenNode:			// call to browseLib.checkout
	case geOpenCabinet:			// call to (browseLib.addExtent cabName cabPath)
	// case geOpenConnection:		// call to AisClient.openConnection (see below)
	case geOpenConsoleLog:		// call to SMgr.openConsoleLog
	case geOpenContext:			// call to SMgr.openContext
	//case geOpenSession:		// call to AisSvr.openSession (see below)
	case geRegisterDirectory:	// call to AppSvr.registerDirectory
	case geRetFile:				// call to AppSvr.returnOutput
	case geRetQuery:			// return from SMgr.event for "application/x-ampmsg" enctype
	case geRetText:				// return from SMgr.event for "text" enctype
	case geRetUrl:				// return from SMgr.event for "url" enctype
	case geRunScriptFile:		// call to SMgr.submit(ipRcvr, "(runscript %s)")
	case geSaveNode:			// call to browseLib.checkin
	case geSetBreakpoint:		// call to SMgr.setBreakpoint
	case geSetEngineFlags:		// call to SMgr.setEngineFlags
	case geSetErrorTrace:		// call to SMgr.errorTrace
	case geSetEscape:			// call to SMgr.setEscape
	case geSetInstructionTrace:	// call to SMgr.setInstructionTrace
	case geSetJit:				// call to SMgr.setJit
	case geSetLogLvl:			// call to ALogMgr.setLogLvl
	case geSetRules:			// call to AisSvr.setRules
	case geSetSubscriptions:	// call to AisSvr.setSubscriptions
	case geSetSysCheck:			// call to SMgr.setSysCheck
	case geShowConsole:			// call to SMgr.submit(ipRcvr, "(writeln (eval {%s}))")
	case geUpdateUser:
		break;
	// Pushed output does not have a pending request
	case geFcnEngineState:		// pushed output from engine
		cEngineFlags = iRetValue;
	case geDisplay:
	case geLogAll:
	case geLogAmp:
	case geLogConsole:
	case geLogReqHdr:
	case geLogUserAccess:
	case geLogStatus:			// return from AisMgr.flushSessions
		aClearRequestQueueItem = false;
		break;
	
	case geFcnDebug:
	{	// First, make the debugger active
		returnMsg(iXid, iStatus, geProcSetDebuggerActive, 0/*RetValue*/,cMt, NULL, 0, cMt, cMt, cMt, aClearRequestQueueItem);

		// Convert string into list
		QStringList aDebugInfo = aOut.split('\177', QString::KeepEmptyParts);
		if (aDebugInfo.count() < 5)
		{	qDebug("AAppClient:returnOutput(), FcnDebug - Missing header");
			break;
		}
		// First element contains the title
		returnMsg(iXid, iStatus, geLogStatus, 0/*RetValue*/, aDebugInfo.first(), NULL, 0, cMt, cMt,cMt,aClearRequestQueueItem);
		aDebugInfo.pop_front();

		if( aDebugInfo.first().startsWith("=") )
		// Display the result in the response line edit
			returnMsg(iXid,iStatus,geProcDebugLineEdit,0/*RetValue*/,aDebugInfo[4],NULL,0,cMt,cMt,cMt,aClearRequestQueueItem);
		else
		// Second element contains the prompt
			returnMsg(iXid,iStatus,geProcDebugLineEdit,0/*RetValue*/,aDebugInfo.first(),NULL,0,cMt,cMt,cMt,aClearRequestQueueItem);
		aDebugInfo.pop_front();

		// Third element contains the selected line in the code block
		long aSelectedLine = aDebugInfo.first().toLong(&aOk);
		aDebugInfo.pop_front();
		
		// Fourth element contains the number of code lines
		long aNumCodeLines = aDebugInfo.first().toLong(&aOk);
		aDebugInfo.pop_front();

		// Fifth element contains the number of info lines
		long aNumInfoLines = aDebugInfo.first().toLong(&aOk);
		aDebugInfo.pop_front();
		
		// Get the next aNumCodeLines codelines
		if (aNumCodeLines > 0)
		{	aOut.truncate(0);
			if (aNumCodeLines > aDebugInfo.count())
			{	aOut = "AAppClient:returnOutput(), FcnDebug - Missing code lines\n";
				qDebug() << aOut;
				aNumCodeLines = aDebugInfo.count();
			}
			for (;;)
			{	aOut += aDebugInfo.first();
				aDebugInfo.pop_front();
				if (--aNumCodeLines > 0)
					aOut += '\177';
				else
					break;
			}
			returnMsg(iXid,iStatus,geProcCodeLines,aSelectedLine,aOut,NULL,0,cMt,cMt,cMt/*ClientData*/,aClearRequestQueueItem);
		}
		// Extract aNumInfoLines from aDebugInfo. Extract tab-delimited lines out of each extracted line. Note workaround:
		// Some info lines may contain tab separators. These lines need to be split into individual lines. This is a hack to
		// get around some limitations of the original engine interface. Work at the engine level would get rid of this
		// problem.  It is caused by the way the descent through variables in the info window is processed. See the
		// ADebugPage::onVarListDoubleClicked for more info.
		if (aNumInfoLines > 0)
		{	long aNum, aNumLines = 0;
			QStringList aLines;
			aOut.truncate(0);
			if (aNumInfoLines > aDebugInfo.count())
			{	aOut = "AAppClient:returnOutput(), FcnDebug - Missing info lines\n";
				qDebug() << aOut;
				aNumInfoLines = aDebugInfo.count();
			}
			for (long i = 0; i < aNumInfoLines;)
			{	aLines = aDebugInfo.first().split('\t', QString::KeepEmptyParts);
				aDebugInfo.pop_front();
				aNum = aLines.count();
				bool aLastLine = (++i >= aNumInfoLines);
				for (long j = 0; j < aNum;)
				{	aOut += aLines.first();
					aLines.pop_front();
					++aNumLines;
					if (++j < aNum || !aLastLine )
						aOut += '\177';
				}
			}
			returnMsg(iXid, iStatus, geProcInfoLines, aNumLines, aOut, NULL/*Data*/, 0/*DataSize*/, cMt/*Display*/,cMt/*Error*/
			, cMt/*ClientData*/, aClearRequestQueueItem);
		}
		// Don't return any more output from here on.
		return;
	}
	case geGetExtentNames:		// call to browseLib.getExtentNames
	{	// Convert the tab delimited return from the call to a string list
		QStringList aExtentNames = aOut.split('\t', QString::SkipEmptyParts);
		// browseLib.getExtentNames returns #void if no extents exist so the following code treats this as an empty list.
		if ((aExtentNames.isEmpty()) || aExtentNames.indexOf("#void") != -1)
		{	cExtents.clear();
			cCurExtentName.truncate(0);
		} 
		else // Reset the internal state variable in case the list has changed
		{	long aIx, aSize;		// Index into extent name-list, number of items in list
			cExtents.clear();
			aSize = aExtentNames.size();
			for (aIx = 0; aIx < aSize; ++aIx)
				cExtents[aExtentNames[aIx]] = AExtentInfo("");
		}
		break;
	}
	case geGetExtentTypes:		// call to browseLib.getTypes
		if (!errorReturned(aOut) && cExtents.contains(cCurExtentName))
		{	QStringList aTypes = aOut.split('\n', QString::SkipEmptyParts);
			long aSize = aTypes.count();

			// Types. Each line contains tab-separated list of Type\tActions where Actions is a comma-separated list of actions.
			AExtentInfo& arExtentInfo = cExtents[cCurExtentName];
			QString aTypeName;
			for (long aX = 0; aX < aSize; ++aX)
			{	QStringList aField = aTypes[aX].split('\t', QString::KeepEmptyParts);
				if (aField.count() > 1)
				{	aTypeName = aField[0];
					// qDebug() << "type=" << aTypeName << " Actions=" << aField[1];
					arExtentInfo.mServerTypes[aTypeName] = AExtentTypeInfo(aTypeName, aField[1]);
				}
			}
		}
		break;

	case geGetNextLevel:	// call to browseLib.getNextLevel
	{	long aX, aFields, aSize;
		if (!cExtents.contains(cCurExtentName))
			break;
		AExtentInfo& arExtentInfo = cExtents[cCurExtentName];
		QStringList aLines = aOut.split('\n', QString::KeepEmptyParts);
		if ((aSize = aLines.count()) < 2)
			break;
		// Lines. aLines[0] contains the following fields which we do not currently use:
		// aField[0] == starting number
		// aField[1] == number of lines transferred
		// aField[2] == total lines available on server
		arExtentInfo.mNodes.clear();
		QStringList aField;
		for (aX = 1; aX < aSize; ++aX)
		{	aField = aLines[aX].split('\t', QString::KeepEmptyParts);
			aFields = aField.count();
			if (aField[0] == "Current")
				arExtentInfo.mNodePath = aField[1];
			else if (aFields >= 8)
			{	// Fields:			type	  name/value  size       date       time       version    symbol     key
				ANodeInfo aNodeInfo(aField[0], aField[1], aField[2], aField[3], aField[4], aField[5], aField[6], aField[7]);
				arExtentInfo.mNodes.append(aNodeInfo);
			}
		}
		// dumpNodes();
		// aOut.truncate(0);
		break;
	}
	case geGetWorkspaceStatistics:	// call to browseLib.inspect stats:
		aOut.prepend("Workspace Statistics\n");
		break;

	case geLogoff:
		// Disable connection.	
		if (iStatus <= 0)
		{	mMutex.lock();
			cUsrId = 0;
			mMutex.unlock();
		}
		break;

	case geLogon:
		// RetValue holds UsrId
		if (iStatus <= 0 && iRetValue > 0)
		{	mMutex.lock();
			cUsrId = iRetValue;
			mMutex.unlock();
		}
		// Logon fails, disable connection
		else
		{	mMutex.lock();
			cUsrId = 0;
			mMutex.unlock();
		}
		break;
	case geNewCabinet:
		// cExtents[aExtentNames[aIx]] = AExtentInfo("")
		break;
	case geOpenConnection:
		if (iStatus > 0)
			aOut = "Unable to connect to server";
		else
		{	mMutex.lock();
			cIsConnected = true;
			mMutex.unlock();
		}
		break;

	case geOpenSession:
		mMutex.lock();
		aSessionId = cSessionId = iRetValue;
		mMutex.unlock();
		break;

	case geRegisterContext:		// call to SMgr::registerContext
		if (!aOut.isEmpty())
		{	// aOut returns ContextName|Registration messages.
			QString aContextName, aRegistrationMsgs;
			QStringList aTkns = aOut.split('\177', QString::KeepEmptyParts);
			long aTknSz = aTkns.size();
			if (aTknSz >= 1)
			{	aContextName = aTkns[0];
				if (aTknSz >= 2)
					aRegistrationMsgs = aTkns[1];
			}
		}
		break;

	case geFcnSendToClient:
	case geFcnSendStatusToClient:
		// Sending any messages to the client
		break;
	case geGetExtentStatus: // Pass back the results of (browseLib.getExtentStatus)
	{
        // RAJAH - add description here
		QString aName;
		QString aAction;
		QStringList aExtentStatus;
		QStringList aExtentStatusList = irOut.split('\n', QString::SkipEmptyParts);
		long aCount = aExtentStatusList.size();
		cExtents.clear();
		if( aCount <= 0 )
		{
			cCurExtentName.truncate(0);
		}
		else
		{
			cExtents.clear();
			for (long aExtent = 0; aExtent < aCount; ++aExtent)
			{
				aExtentStatus = aExtentStatusList[aExtent].split('\t', QString::SkipEmptyParts);
	
				if( aExtentStatus.size() == 6 )
				{
					aAction = aExtentStatus[0];
					aName = aExtentStatus[4];
				}
				cExtents[aName] = AExtentInfo("");
			}
		}
	}
		break;
	case geUpdateMetadata: // Pass back the results of (browseLib.updateMetaData)
	case geGetMetaData: // Pass back the results of (browseLib.getMetaData)
		break;

	// The following request types are not currently expected to return a message
	//case geFcnFileOpen:
	//case geFcnReadHtmlPage:
	//case geFcnRingBell:
	//case geFcnSendToClient:
	// case geFcnSendStatusToClient:
	default: // AppClient got a response to an unexpected return type
		qDebug("AAppClient:returnOutput(Xid:%ld,Status:%ld, ReqType:%s,RetValue:%ld,Out:%.128s,Display:%.128s,Error:%.128s) %s\n"
		, iXid, iStatus, REQNAME(iReqType), iRetValue, aOut.toLatin1().data(), irDisplay.toLatin1().data()
		, irError.toLatin1().data(), gAis.mpErrMsgs[AERR_BADREQUEST]);
		break;
	} // end switch on iReqType
lErrorRet:
	returnMsg(iXid, iStatus, iReqType, iRetValue, aOut, ipData,iDataSize,irDisplay,irError,aClientData,aClearRequestQueueItem);

	// Post process selected events after last request has been captured.
	if (iStatus <= 0)
	{	switch (iReqType)
		{
			case geCloseSession:		
				// RetValue holds session being closed
				if (cSessionId == iRetValue)
					cSessionId = 0;
				break;
			case geConnectSession:
				// Assign new session id, rename session form
				cSessionId = iRetValue;
				break;
			default:
				break;
		}
	}
}

/*!
 * \brief Run a script remotely.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irFilePath Path/name of startup script on server.
 * \param[in] irPrefix Prefix to be prepended to the runscript expression that runs the startup script.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|runscriptfile|file|%s|prefix|%s
 */
AXID AAppClient::runRemoteScriptFile(AReturnRcvr* ipRcvr, const QString& irFilePath, const QString& irPrefix, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177runscriptfile\177file\177%s", irFilePath.toLatin1().data());
	if (!irPrefix.isEmpty())
		aAmpmsg += "\177prefix\177" + irPrefix;
	if (!irFilePath.isEmpty())
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = submitError(ipRcvr, AERR_BADFILENAME, geRunScriptFile, aAmpmsg, cMt/*Data*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Submit request to save a node.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irExtentName Name of target extent.
 * \param[in] irNodeName Node to be save.
 * \param[in] irText
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \todo Complete parameter description.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|savenode|extent|%s|node|text|%s
 * -# A special _checkin target Lambda implements this operation. _checkin avoids having to have an AMP Lambda for this basic
 * operation. An AisLisp expression is not guaranteed to have a proper form, thereby resulting in partial data loss.
 */
AXID AAppClient::saveNode(AReturnRcvr* ipRcvr, const QString& irExtentName,const QString& irNodeName, QByteArray& irText,void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177savenode\177extent\177%s\177node\177%s\177text\177%s", irExtentName.toLatin1().data(),
	irNodeName.toLatin1().data(), irText.data());
	if (!irExtentName.isEmpty() && !irNodeName.isEmpty() && !irText.isEmpty())
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = submitError(ipRcvr, AERR_BADNODE, geSaveNode, aAmpmsg,cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Set a breakpoint that suspends execution of the target Lambda.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irLambdaName Lambda to be suspended.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|setbreakpoint|Lambda|%s
 * -# The Lisp expression may contain DEL characters.
 */
AXID AAppClient::setBreakPoint(AReturnRcvr* ipRcvr, QString& irLambdaName, void* ipCustomData)
{
	AXID aXid;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177setbreakpoint\177Lambda\177%s", irLambdaName.toLatin1().data());
	if (!irLambdaName.isEmpty())	
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = submitError(ipRcvr, AERR_BADLambda, geSetBreakpoint, aAmpmsg,cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Set the default client page to receive pushed output.
 *
 * Set a new client to receive pushed output (returned Xid is zero) returned by the engine.
 *
 * \param ipRcvr New client that inherits AReturnRcvr.
 */
void AAppClient::setDefaultRcvr(AReturnRcvr* ipRcvr)
{
	cpDefaultRcvr = ipRcvr;
}

/*!
 * \brief Set one or more engine flags to bits in iFlags.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iFlags New setting.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|setengineflags|flags|%d
 */
AXID AAppClient::setEngineFlags(AReturnRcvr* ipRcvr, unsigned iFlags, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177setengineflags\177flags\177%d", iFlags);
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Cause the program to stop on an error.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iOnoff Enable/disable error trace.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|seterrortrace|onoff|%d
 * -# The Lisp expression may contain DEL characters
 */
AXID AAppClient::setErrorTrace(AReturnRcvr* ipRcvr, bool iOnoff, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177seterrortrace\177onoff\177%d", iOnoff);
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Set flag to stop execution of currently executing request of the target session.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iSessionId ID of session.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|setescape|sessionid|%d
 */
AXID AAppClient::setEscape(AReturnRcvr* ipRcvr, long iSessionId, void* ipCustomData)
{
	AXID aXid;
	QString aAmpmsg;
	if (iSessionId == 0)
		iSessionId = cSessionId;
	aAmpmsg.sprintf("_ais\177setescape\177sessionid\177%ld", iSessionId);
	if (iSessionId > 0)
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else
		aXid = -AERR_SESSIONID;
	return aXid;
}

/*!
 * \brief Enable debugging on the current program.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iOnoff Enable/disable instruction trace.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|setinstructiontrace|onoff|%d
 * -# The Lisp expression may contain DEL characters
 */
AXID AAppClient::setInstructionTrace(AReturnRcvr* ipRcvr, bool iOnoff, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177setinstructiontrace\177onoff\177%d", iOnoff ? 1 : 0);
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Enable compilation to native code.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iOnOff Enable/disable just-in-time compilation.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|setjit|onoff|%d
 */
AXID AAppClient::setJit(AReturnRcvr* ipRcvr, bool iOnOff, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177setjit\177onoff\177%d", iOnOff ? 1 : 0);
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Determine type and quantity of messages to be returned by the server to this client.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iLogType The target log (System, Console, ReqHdrs, etc.)
 * \param[in] iWarnLvl The minimum level to be returned. The higher the level, the fewer the messages.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|setloglvl|logtype|%d|level|%d
 * -# The log type, warning level, and sessionid set the type and quantity of messages returned. The subscriber does not need
 * to open a session (however, a logon and connection are required).
 * -# This function is unusual in that the client does not usually need to disable anything while waiting for the server 
 * response to this request.
 */
AXID AAppClient::setLogLvl(AReturnRcvr* ipRcvr, AReqType iLogType, AErrLvl iWarnLvl, void* ipCustomData)
{
	QString aAmpmsg = QString("_ais\177setloglvl\177logtype\177%1\177level\177%2").arg(iLogType).arg(iWarnLvl);
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Set the rules for displaying nodes.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irRules Rules to be modified.
 * \param[in] iRemove
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \todo Complete parameter description.
 * 
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|setrules|rules|%s|mode|%d
 */
AXID AAppClient::setRules(AReturnRcvr* ipRcvr, const QString& irRules, bool iRemove, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177setrules\177rules\177%s", irRules.toLatin1().data());
	if (iRemove)
		aAmpmsg += "\177mode\1771";
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Allow a client to receive a copy of the console output from a session.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irNewSessions List of new sessions to be added to subscriptions.
 * \param[in] irOldSessions	List of existing subscriptions.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|setsubscriptions|new|%s|old|%s
 */
AXID AAppClient::setSubscriptions(AReturnRcvr* ipRcvr,const QString& irNewSessions,const QString& irOldSessions,void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177setsubscriptions\177new\177%s\177old\177%s",irNewSessions.toLatin1().data(),irOldSessions.toLatin1().data());
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Enable/disable self-checking.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iOnOff Enable/disable self checking.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|setsyscheck|onoff|%d
 */
AXID AAppClient::setSysCheck(AReturnRcvr* ipRcvr, bool iOnOff, void* ipCustomData)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177setsyscheck\177onoff\177%d", iOnOff);
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
}

/*!
 * \brief Display results of executing an expression.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irText A AisLisp expression.
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|showconsole|exp|%s
 * -# If the result of an expression is a variable, its value is displayed in text format.
 */
AXID AAppClient::showConsoleSelection(AReturnRcvr* ipRcvr, QString& irText, void* ipCustomData)
{
	long aXid;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177showconsole\177exp\177%s", irText.toLatin1().data());
	if (!irText.isEmpty())
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	else	
		aXid = submitError(ipRcvr, AERR_NOINPUT, geShowConsole, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}

/*!
 * \brief Submit a request to the server.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irAmpmsg AMP message (target|act, _ais|cmd, ...).
 * \param[in] irClientData
 * \param[in] iClientValue
 * \param[in] ipData
 * \param[in] iDataSize
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \todo Complete parameter description.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# Returns the client-generated request ID, Xid.  A request is generated and a positive Xid is always returned.
 * -# If not an error or  local request, the request is forwarded to AisClient for submission to AIS.
 * -# ClientData, ClientValue, and CustomData are saved and returned with the result.
 */
AXID AAppClient::submit(AReturnRcvr* ipRcvr, const QString& irAmpmsg, const QString& irClientData, long iClientValue
, char* ipData, long iDataSize, void* ipCustomData)
{
	long aRetValue = 0, aStatus = 0;
	QString aOut, aSpeechAct;
	AReqType aReqType = geAmpMsg;			// target|act|...
	if (!cIsConnected)
	{	aStatus = AERR_DISCONNECTED;
		goto lError;
	}
	else if (irAmpmsg.isEmpty())
		aReqType = geUnknown;
	else if (irAmpmsg.startsWith("_ais"))// _ais|cmd|...
	{	aSpeechAct = irAmpmsg.section('\177', 1, 1).toLower();
		if (aSpeechAct.isEmpty()) aSpeechAct = "noop";
		aReqType = REQTYPE(aSpeechAct);
	}
	else
		aReqType = geAmpMsg;

	switch (aReqType)
	{	// Submit the following directly to app server
	case geAmpMsg:				// target|act|...
	case geAddUser:				// _ais|adduser|userid|%d|...
	case geCheckCabinet:		// _ais|checkcabinet|cabname|%s
	case geCloseCabinet:		// _ais|closecabinet|cabname|%s
	case geCloseContext:		// _ais|closecontext|context|%s|closemode|%s
	case geCloseConnection:		// _ais|closeconnection|connectid|%d|closemode|%s|closewait|%d
	case geCloseSession:		// _ais|closesession|sessionid|%d|closemode|%s

	// Note: extents, extent_Lambdas and nodes are tab-delimited lists
	case geCompileLambda:		// _ais|compilelambda|extent_Lambdas|%s
	case geCompileCabinet:		// _ais|compilecabinet|extents|%s
	case geConnectSession:		// _ais|connectsession|sessionid|%d
	case geDebug:				// _ais|debug|exp|%s"
	case geDeleteUser:			// _ais|deleteuser|userid|%d
	case geEnableConsoleLog:	// _ais|enableconsolelog|enable|%s|sessionid|%d
	case geEraseNode:			// _ais|erasenode|extent|%s|nodes|%s
	case geEval:				// _ais|eval|exp|(debug ...)(...)
	case geExecute:				// _ais|execute|exp|(debug ...)(...)
	case geExportNode:			// _ais|exportnode|node|%s|file|%s|cabinet|%s
	case geExportCabinet:		// _ais|exportcabinet|cabinet|%s|file|%s
	case geGetNextLevel:		// _ais|getnextlevel|extent|%s|nodeName|%s
	case geGetConnectionStats:	// _ais|getconnectionstats
	case geGetConsoleLog:		// _ais|getconsolelog|clear|%s|sessionid|%d|wait|%d
	case geGetContextId:		// _ais|getcontextid|context|%s
								// _ais|getcontextid|sessionid|%d
	case geGetContextParams:	// _ais|getcontextparams|context|%s
	case geGetCurrentContexts:	// _ais|getcurrentcontexts
	case geGetDirInfo:			// _ais|getdirinfo|dir|%s
	case geGetExeSession:		// _ais|getexesession|context|%s
	case geGetExtentNames:		// _ais|getextentnames
	case geGetExtentStatus:		// _ais|getextentstatus
	case geGetExtentTypes:		// _ais|getextenttypes|extent|%s
	case geGetLogonStats:		// _ais|getlogonstats
	case geGetMetaData:			// _ais|getmetadata|context|%s
	case geGetRequestStats:		// _ais|getrequeststats
	case geGetSessions:			// _ais|getsessions
	case geGetSessionStats:		// _ais|getsessionstats
	case geGetSessionUser:		// _ais|getsessionuser|sessionid|%d (not currently used)
	case geGetSubscriptions:	// _ais|getsubscriptions|context|%s
	case geGetUsers:			// _ais|getUsers
	case geGetWorkspaceStatistics: // _ais|getworkspacestatistics
	case geImportCabinet:		// _ais|importcabinet|cabinet|%s|file|%s
	case geIsContextBusy:		// _ais|iscontextbusy|context|%s
	case geIsContextOpen:		// _ais|iscontextopen|context|%s
	case geLogSysMsg:			// _ais|logsysmsg|msg|%s|level|%d
	case geNewCabinet:			// _ais|newcabinet|cabinet|%s|path|%s
	case geLogon:				// _ais|logon|user|%s|passwd|%s|context|%s|wait|%d
	case geLogoff:				// _ais|logoff|connectid|%d
	case geNoop:				// _ais|noop
	case geOpenCabinet:			// _ais|opencabinet|cabinet|%s|path|%s
	case geOpenNode:			// _ais|opennode|extent|%s|node|%s
	case geOpenConsoleLog:		// _ais|openconsolelog|clear|%s|redirect|%s|sessionid|%d|size|%d|startatnewline|%s
	case geOpenContext:			// _ais|opencontext|path|%s
	case geOpenSession:			// _ais|opensession|context|%s|userid|%d|closemode|%s
	case geRegisterContext:		// _ais|registercontext|path|%s
	case geRegisterDirectory:	// _ais|registerdirectory|databasedir|%s|sourcedir|%s
	case geRetFile:				// _ais|retfile|file|%s
	case geRunScriptFile:		// _ais|runscriptfile|file|%s|prefix|%s
	case geSaveNode:			// _ais|savenode|extent|%s|node|%s|text|%s
	case geSetBreakpoint:		// _ais|setbreakpoint|Lambda|%s
	case geSetEngineFlags:		// _ais|setEngineFlags|flags|%d
	case geSetErrorTrace:		// _ais|seterrortrace|onoff|%d
	case geSetEscape:			// _ais|setescape|sessionid|%d
	case geSetInstructionTrace:	// _ais|setinstructiontrace|onoff|%d
	case geSetJit:				// _ais|setjit|onoff|%d
	case geSetLogLvl:			// _ais|setloglvl|logtype|%d|level|%d
	case geSetRules:			// _ais|setrules|rules|%s|mode|%d
	case geSetSubscriptions:	// _ais|setsubscriptions|new|%s|old|%s
	case geSetSysCheck:			// _ais|setsyscheck|onoff|%d
	case geShowConsole:			// _ais|showconsole|exp|%s
	case geUpdateMetadata:		// _ais|updatemetadata|cabinet|%s|path|%s|location|%s|storage|%s|importsync|%s|exportsync|%s|autocompile|%s
	case geUpdateUser:			// _ais|updateuser|userid|%d|...
		break;

	// Process local requests. These "local" commands are usually executed by calling specific public methods provided by
	// AAppClient.  However, it is possible for debugging purposes to call them from the console using the form
	// "_ais|speechact".  In this case, the request ends up here.
	default:
		aStatus = -1;			// Note a local request
		aReqType = geAmpMsg;
		if (aSpeechAct == "getcurrentnodenames")
		{	QStringList aNodeNames = getCurrentNodeNames();
			aOut = aNodeNames.join("\t");
		}
		else if (aSpeechAct == "getcurrentextentname")
			aOut = cCurExtentName;
		else if (aSpeechAct == "getcurrentextentnames")
		{	QStringList aExtentNames = cExtents.keys();
				aOut = aExtentNames.join("\t");
		}
		else if (aSpeechAct == "getcurrentrepositorynames")
			aOut = cRepositoryNames.join("\t");
		else if (aSpeechAct == "getcurrentsessionid")
			aRetValue = cSessionId;
		else
			aOut = gAis.mpErrMsgs[AERR_UNKACT];
	}
lError:		// Error Return.
	long aXid = 0;
	if (aStatus > 0)
		submitError(ipRcvr, aStatus, aReqType, irAmpmsg, irClientData, 0/*ClientValue*/, ipCustomData);
	else
	{	// Privileged means immune from instruction trace. Note: all results are returned back via returnOutput. Make a note of
		// this request for use by returnOutput. Since an in-proc return can occur before return from submit, this must be done
		// before call to submit.  Since SetEscape and Debug intervene while a request is outstanding, we do not inject a new request.
		if (aReqType != geSetEscape)
		{	AClientReqSt aReqSt;
			aReqSt.mAmpMsg = irAmpmsg;
			aReqSt.mClientData = irClientData;
			aReqSt.mClientValue = iClientValue;
			aReqSt.mpCustomData = ipCustomData;
			aReqSt.mpRcvr = ipRcvr;
			aReqSt.mReqType = aReqType;
			mMutex.lock();
			cClientRequests[aXid = ++cXid] = aReqSt;
			mMutex.unlock();
		}
		// Local Request. Direct return
		if (aStatus < 0)
			returnOutput(0/*DummyId*/, aXid, 0/*Status*/, aReqType, aRetValue, aOut, NULL, 0, cMt/*Display*/, cMt/*Error*/);
		else
		{	if (cpInProcSvr != NULL)
				cpInProcSvr->submit(aXid, irAmpmsg, this, NULL/*pSocket*/, ipData);
			else
			{	QString aRequest(QString::number(aXid) + '\177');
				cpSocket->submit(aRequest + irAmpmsg, ipData, iDataSize);
			}
			ipData = NULL;
		}
	}
	if (ipData != NULL)
	{	free(ipData);
		qDebug("AAppClient::submit(Ampmsg:%s, ClientData:%s, ClientValue:%ld), Data dropped.", irAmpmsg.toAscii().data()
		, irClientData.toAscii().data(), iClientValue);
	}
	return aXid;
}

/*!
 * \brief Direct return of an error to client.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] iStatus
 * \param[in] iReqType
 * \param[in] irAmpmsg
 * \param[in] irClientData
 * \param[in] iClientValue
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \todo Complete parameter description.
 *
 * \return aXid - Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# Creates a request and then calls returnOutut directly.
 */
AXID AAppClient::submitError(AReturnRcvr* ipRcvr, long iStatus, AReqType iReqType, const QString& irAmpmsg
, const QString& irClientData, long iClientValue, void* ipCustomData)
{
	// PENDING. Note that the Xid is returned by returnOutput before the Xid is returned to the caller from this call.
	// It may be better to just return an error or immediate response back to the caller directly from submit.
	long aXid;
	QString aOut(iStatus == 0 ? "true" : "");
	AClientReqSt aReqSt;
	aReqSt.mAmpMsg = irAmpmsg;
	aReqSt.mClientData = irClientData;
	aReqSt.mClientValue = iClientValue;
	aReqSt.mpCustomData = ipCustomData;
	aReqSt.mpRcvr = ipRcvr;
	aReqSt.mReqType = iReqType;
	mMutex.lock();
	cClientRequests[aXid = ++cXid] = aReqSt;
	mMutex.unlock();

	returnOutput(0/*DummyId*/, aXid, iStatus, iReqType, 0/*RetValue*/, aOut, NULL, 0, cMt/*Display*/,gAis.mpErrMsgs[iStatus]);
	return aXid;
}

/*!
 * \brief Call server to update a cabinet's meta data information.
 *
 * \param[in] ipRcvr Pointer to object that will receive the return.
 * \param[in] irCabName	New cabinet's name.
 * \param[in] irCabPath	Path to place to put the cabinet.
 * \param[in] irCabLocation	Path to the Lisp Source.
 * \param[in] irStorage	Storage Type either File or Folder
 * \param[in] irImportSync Import Syncronization flag
 * \param[in] irExportSync Export Syncronization flag
 * \param[in] irAutoCompile Auto Compilation flag
 * \param[in] irSearchSwitch Search Switch flag
 * \param[in] irDependencies List of dependent cabinets each separated by comma
 * \param[in] ipCustomData An anonymous ptr returned to caller with request.
 *
 * \return aXid	- Request ID (incrementing integer) established by submit.
 *
 * \note
 * -# AMP format:  _ais|updatemetadata|cabinet|%s|path|%s|location|%s|storage|%s|importsync|%s|exportsync|%s|autocompile|%s

 */
ASXID AAppClient::updateMetadata(AReturnRcvr* ipRcvr, QString& irCabName, QString& irCabPath, QString& irCabLocation, QString& irStorage,
	QString& irImportSync, QString& irExportSync, QString& irAutoCompile, QString& irSearchSwitch, QString& irDependencies, void* ipCustomData)
{
	long aStatus = 0, aXid = 0;
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177updatemetadata\177cabinet\177%s\177path\177%s\177location\177%s\177storage\177%s\177importsync\177%s\177exportsync\177%s\177autocompile\177%s\177searchsw\177%s\177dependencies\177%s",
		irCabName.toLatin1().data(), irCabPath.toLatin1().data(), irCabLocation.toLatin1().data(), irStorage.toLatin1().data(), irImportSync.toLatin1().data(), irExportSync.toLatin1().data(),
		irAutoCompile.toLatin1().data(), irSearchSwitch.toLatin1().data(), irDependencies.toLatin1().data());
	if(!irCabPath.isEmpty() && !irCabName.isEmpty() && !irCabLocation.isEmpty() && !irStorage.isEmpty() && !irImportSync.isEmpty() && !irExportSync.isEmpty() && !irAutoCompile.isEmpty() && !irSearchSwitch.isEmpty())
	{
		aXid = submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/, ipCustomData);
	}
	else
		aStatus = AERR_BADCABINET;

	if (aStatus > 0)
		aXid = submitError(ipRcvr, aStatus, geUpdateMetadata, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, ipCustomData);
	return aXid;
}


/*!
 * \brief Sends a geUpdateUser AMP request to the AIS Server.
 *
 * \param[in] ipRcvr Return receiver object pointer.
 * \param[in] iUserId User Id of user to be updated.
 * \param[in] irUsername Username.
 * \param[in] irPassword Password.
 * \param[in] iSecurityLevel Security Level.
 * \param[in] irEndDate End date.
 * \param[in] irComment Comment.
 *
 * \return Request Id.
 */
ASXID AAppClient::updateUser(AReturnRcvr* ipRcvr, long iUserId, const QString& irUsername, const QString& irPassword,
						long iSecurityLevel, const QDate& irEndDate, const QString& irComment)
{
	QString aAmpmsg;
	aAmpmsg.sprintf("_ais\177updateUser\177userid\177%ld\177username\177%s\177password\177%s\177securitylevel\177%ld"
		"\177enddate\177%s\177comment\177%s", iUserId, irUsername.toLatin1().constData(),
		irPassword.toLatin1().constData(), iSecurityLevel,
		irEndDate.toString("MM/dd/yyyy").toLatin1().constData(), irComment.toLatin1().constData());
	return submit(ipRcvr, aAmpmsg, cMt/*ClientData*/, 0/*ClientValue*/, NULL/*Data*/, 0/*DataSize*/);
}

// end
