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

#ifndef AIS_H
#define AIS_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/ais.h
													AIS Common Elements

Ais.h contains the common elements that are used by aismgr.h, appclient.h and appsvr.h.

CHANGE HISTORY
Version	Date		Who		Change
3.2005	 2/14/2008	fchua	Added new types of request (geConnectionStats, geLogonStats, geSessionStats, geRequestStats).
3.2002	 2/04/2008	fchua	Added new type of request geLogUserAccess.
3.2001	 1/28/2008	fchua	Added new types of request (geAddUser, geDeleteUser, geGetUsers, geUpdateUser).
3.1002	10/21/2007	fchua	Added geConnectionError.
2.0001	12/29/2006	tmay	added geExecute
1.0120	12/19/2006	tlw		returnOutput. Add ipData and iDataSize arguments.
1.0113	11/13/2006	tlw		AReqTypeMap. Modify to use QHash
1.0059	4/22/2005	tlw		Remove disconnectSession
1.0057	3/18/2005	tlw		Add documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
#include <QtCore/QHash>
#include <QtCore/QString>

//  ********************************* ADD ANY CHANGES MADE HERE TO MREQNAMES IN AGLOBALS.H **********************************
enum AReqType
{	geUnknown,				// unknown request
	geAmpMsg,				// evaluate AMP message
	geCheckCabinet,			// call to (browseLib.checkStatus cabName)
	geCloseCabinet,			// call to (browseLib.dropExent cabName)
	geCompileAll,			// call to browseLib.compileAllAutoCompileCabinets
	geCompileLambda,		// call to browseLib.compileSource
	geCompileCabinet,		// call to (browseLib.compileAll true)
	geDebug,				// Debugger command
	geEraseNode,			// call to browseLib.eraseSource
	geEval,					// evaluate SmartLisp expression
	geExecute,				// evaluate SmartLisp expression and return result binary closure record
	geExportNode,			// call to (browseLib.exportSource fileName LambdaName)
	geExportCabinet,		// call to (browseLib.exportSource fileName ..allLambdas..)
	geGetDirInfo,
	geGetExtentNames,
	geGetExtentStatus,      // call to (browseLib.getExtentStatus)
	geGetExtentTypes,		// call to (browseLib.getTypes extentname)
	geGetMetaData,			// call to retrieve Meta Data information (browseLib.getMetaData)
	geGetNextLevel,
	geGetWorkspaceStatistics,
	geImportCabinet,		// call to (browseLib.importSource fileName)
	geLogoff,				// built-in ais request
	geLogon,				// built-in ais request
	geNewCabinet,			// call to (browseLib.addExtent cabName cabPath)
	geOpenNode,				// call to browseLib.checkout
	geOpenCabinet,			// call to (browseLib.addExtent cabName cabPath)
	geRegisterDirectory,	// call to (browseLib.registerDirectory databaseDir ...)
	geRunScriptFile,
	geSaveNode,				// call to browseLib.checkin
	geSetBreakpoint,
	geSetLogLvl,			// Diverted to log mgr
	geShowConsole,
	geUpdateMetadata,
	// Immediate returns from AisMgr
	geCloseConnection,
	geCloseContext,
	geCloseSession,
	geConnectSession,
	geDisplay,				// Requested console output
	geEnableConsoleLog,
	geGetConsoleLog,
	geGetContextId,
	geGetContextParams,
	geGetCurrentContexts,
	geGetExeSession,
	geGetSessions,
	geGetSessionId,
	geGetSessionUser,		// Requires session ID
	geGetSubscriptions,
	geIsContextBusy,
	geIsContextOpen,
	geNoop,
	geOpenConnection,
	geOpenConsoleLog,		// Requires session ID
	geOpenContext,
	geOpenSession,
	geRegisterContext,
	geSetEngineFlags,		// Requires session ID
	geSetErrorTrace,		// Requires session ID
	geSetEscape,
	geSetInstructionTrace,	// Requires session ID
	geSetJit,				// Requires session ID
	geSetRules,
	geSetSubscriptions,
	geSetSysCheck,			// Requires session ID
	// Additional output function types. Never a request, only used in output dispatch.
	geFcnDebug,
	geFcnError,
	geFcnFileOpen,
	geFcnReadHtmlPage,
	geFcnReturnResult,
	geFcnRingBell,
	geFcnEngineState,
	geFcnSendToClient,
	geFcnSendStatusToClient,
	// Additional log types. Never a request, only used in output dispatch.
	geLogAll,
	geLogAmp,
	geLogConsole,
	geLogReqHdr,
	geLogSysMsg,		// Can be a request from appClient
	geLogUserAccess,
	geLogStatus,

	// Outputs for various returned enctypes
	geRetFile,
	geRetQuery,
	geRetText,
	geRetUrl,
	// Local events posted in client. Never requested or returned from AIS.
	geProcCodeLines,
	geProcDebugLineEdit,
	geProcInfoLines,
	geProcSetDebuggerActive,
	geConnectionError,

	// User Management Request Types
	geAddUser,
	geUpdateUser,
	geDeleteUser,
	geGetUsers,

	// System Resource Monitor Request Types
	geGetConnectionStats,
	geGetLogonStats,
	geGetSessionStats,
	geGetRequestStats,

	geReqTypeSz						// Always last, never used.
};
#define AREQTYPES geReqTypeSz		// Number of request types

typedef QHash<QString, AReqType> AReqTypeMap;	// key=request name, value=request type

// AReturnRcvr is inherited by the protocol servers, AHttpSvr, AAppSvr, AXmlSvr and also by AppClient.  These classes implement
// returnOutput. AisSvr gets a pointer to the protocol servers during initialization.  When AisSvr::returnOutput is called, it
// routes the call to the appropriate servers' returnOutput.  AppClient is unique.  AppClient not only implements returnOutput,
// it raises an event to return results back to a parent form. Note that the parent form may start out to be the ConnectMgr but
// may be switched to the AServerForm if one is opened for the server.  AppClient normally raises an event to return an event
// to the parent form.  However, the return may be overridden with a revised returnMsg that also calls back to the parent via
// a returnOutput call.  See contextClient for an example.  
class AReturnRcvr
{
public:
	virtual void returnOutput(long iConnectId, long iRqId, long iStatus, AReqType iReqType, long iRetValue, const QString& irOut
				 , char* ipData, long iDataSize, const QString& irDisplay, const QString& irError, const QString iClientData
				 = QString()) = 0;

	virtual bool connectionClosed(long iConnectId) { Q_UNUSED(iConnectId); return true; };
	
	virtual void setContextName(long iConnectId, const QString& irContextName) { Q_UNUSED(iConnectId); Q_UNUSED(irContextName); };

	virtual ~AReturnRcvr() {};
};

#endif // AIS_H
