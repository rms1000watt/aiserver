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
aisdev/include/areqtypes.h
														Request-Type Definitions

CHANGE HISTORY
Version	Date		Who		Change
3.2005	 2/14/2008	fchua	Added new types of request (geConnectionStats, geLogonStats, geSessionStats, geRequestStats).
3.2002	 2/04/2008	fchua	Added new type of request (geLogUserAccess).
3.2001	 1/28/2008	fchua	Added new types of request (geAddUser, geDeleteUser, geGetUsers, geUpdateUser).
3.1003	10/9/2007	fchua   Renamed "fcnsendenginestate" to "fcnenginestate" mReqTypes.
3.1003	1/8/2007	tlw		mReqTypes. Convert keys explicitly to a QString.
2.0001	12/29/2006	tmay	added geExecute
1.0105	 9/11/2006	tlw		Change name of geRetFile to retfile.
1.0064	 6/3/2005	tlw		Move request types to aglobals.h for both client and server access.
												---------------------------------
NOTES
The request type name for each request type is held in mpReqNames array.  The request type for each request-type name is
held in the mReqTypes map.  Include this file just ONCE in aglobals.h to initialize these request type conversions.  The
request types used here are defined in ais.h.  The two files must remain in sync.  Add an entry in both places or
delete an entry in both places. 
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------

// Initialize array of built-in request names indexed by the request-type
	mpRequestNames[geUnknown] = "unknown";
	mpRequestNames[geAmpMsg] = "ampmsg";
	mpRequestNames[geCheckCabinet] = "checkcabinet";
	mpRequestNames[geCloseCabinet] = "closecabinet";
	mpRequestNames[geCompileAll] = "compileall";
	mpRequestNames[geCompileLambda] = "compilelambda";
	mpRequestNames[geCompileCabinet] = "compilecabinet";
	mpRequestNames[geDebug] = "debug";
	mpRequestNames[geDisplay] = "display";
	mpRequestNames[geEraseNode] = "erasenode";
	mpRequestNames[geEval] = "eval";
	mpRequestNames[geExecute] = "execute";
	mpRequestNames[geExportNode] = "exportnode";
	mpRequestNames[geExportCabinet] = "exportcabinet";
	mpRequestNames[geGetDirInfo] = "getdirinfo";
	mpRequestNames[geGetExtentNames] = "getextentnames";
	mpRequestNames[geGetExtentStatus] = "getextentstatus";
	mpRequestNames[geGetExtentTypes] = "getextenttypes";
	mpRequestNames[geGetMetaData] = "getmetadata";
	mpRequestNames[geGetNextLevel] = "getnextlevel";
	mpRequestNames[geGetWorkspaceStatistics] = "getworkspacestatistics";
	mpRequestNames[geImportCabinet] = "importcabinet"; 
	mpRequestNames[geLogoff] = "logoff"; 
	mpRequestNames[geLogon] = "logon"; 
	mpRequestNames[geNewCabinet] = "newcabinet"; 
	mpRequestNames[geOpenNode] = "opennode"; 
	mpRequestNames[geOpenCabinet] = "opencabinet"; 
	mpRequestNames[geRunScriptFile] = "runscriptfile"; 
	mpRequestNames[geSaveNode] = "savenode"; 
	mpRequestNames[geSetBreakpoint] = "setbreakpoint"; 
	mpRequestNames[geSetLogLvl] = "setloglvl"; 
	mpRequestNames[geShowConsole] = "showconsole"; 
	mpRequestNames[geCloseConnection] = "closeconnection"; 
	mpRequestNames[geCloseContext] = "closecontext"; 
	mpRequestNames[geCloseSession] = "closesession"; 
	mpRequestNames[geConnectSession] = "connectsession"; 
	mpRequestNames[geEnableConsoleLog] = "enableconsolelog"; 
	mpRequestNames[geGetConsoleLog] = "getconsolelog"; 
	mpRequestNames[geGetContextId] = "getcontextid"; 
	mpRequestNames[geGetContextParams] = "getcontextparams"; 
	mpRequestNames[geGetCurrentContexts] = "getcurrentcontexts"; 
	mpRequestNames[geGetExeSession] = "getexesession"; 
	mpRequestNames[geGetSessions] = "getsessions";
	mpRequestNames[geGetSessionId] = "getsessionid"; 
	mpRequestNames[geGetSessionUser] = "getsessionuser"; 
	mpRequestNames[geGetSubscriptions] = "getsubscriptions"; 
	mpRequestNames[geIsContextBusy] = "iscontextbusy"; 
	mpRequestNames[geIsContextOpen] = "iscontextopen"; 
	mpRequestNames[geNoop] = "noop"; 
	mpRequestNames[geOpenConnection] = "openconnection"; 
	mpRequestNames[geOpenConsoleLog] = "openconsolelog"; 
	mpRequestNames[geOpenContext] = "opencontext"; 
	mpRequestNames[geOpenSession] = "opensession"; 
	mpRequestNames[geRegisterContext] = "registercontext"; 
	mpRequestNames[geRegisterDirectory] = "registerdirectory"; 
	mpRequestNames[geSetEngineFlags] = "setengineflags"; 
	mpRequestNames[geSetErrorTrace] = "seterrortrace"; 
	mpRequestNames[geSetEscape] = "setescape"; 
	mpRequestNames[geSetInstructionTrace] = "setinstructiontrace"; 
	mpRequestNames[geSetJit] = "setjit"; 
	mpRequestNames[geSetRules] = "setrules"; 
	mpRequestNames[geSetSubscriptions] = "setsubscriptions"; 
	mpRequestNames[geSetSysCheck] = "setsyscheck"; 
	mpRequestNames[geFcnDebug] = "fcndebug"; 
	mpRequestNames[geFcnError] = "fcnerror"; 
	mpRequestNames[geFcnFileOpen] = "fcnfileopen"; 
	mpRequestNames[geFcnReadHtmlPage] = "fcnreadhtmlpage"; 
	mpRequestNames[geFcnReturnResult] = "fcnreturnresult"; 
	mpRequestNames[geFcnRingBell] = "fcnringbell"; 
	mpRequestNames[geFcnEngineState] = "fcnenginestate"; 
	mpRequestNames[geFcnSendToClient] = "fcnsendtoclient"; 
	mpRequestNames[geLogAll] = "logall"; 
	mpRequestNames[geLogAmp] = "logamp"; 
	mpRequestNames[geLogConsole] = "logconsole"; 
	mpRequestNames[geLogReqHdr] = "logreqhdr"; 
	mpRequestNames[geLogSysMsg] = "logsysmsg"; 
	mpRequestNames[geLogUserAccess] = "loguseraccess";
	mpRequestNames[geLogStatus] = "logstatus"; 
	mpRequestNames[geRetFile] = "retfile"; 
	mpRequestNames[geRetQuery] = "retquery"; 
	mpRequestNames[geRetText] = "rettext"; 
	mpRequestNames[geRetUrl] = "returl"; 
	mpRequestNames[geProcCodeLines] = "proccodelines"; 
	mpRequestNames[geProcDebugLineEdit] = "procdebuglineedit"; 
	mpRequestNames[geProcInfoLines] = "procinfolines"; 
	mpRequestNames[geProcSetDebuggerActive] = "procsetdebuggeractive";
	mpRequestNames[geAddUser] = "adduser";
	mpRequestNames[geDeleteUser] = "deleteuser";
	mpRequestNames[geGetUsers] = "getusers";
	mpRequestNames[geUpdateUser] = "updateuser";
	mpRequestNames[geGetConnectionStats] = "getconnectionstats";
	mpRequestNames[geGetLogonStats] = "getlogonstats";
	mpRequestNames[geGetSessionStats] = "getsessionstats";
	mpRequestNames[geGetRequestStats] = "getrequeststats";
	mpRequestNames[geUpdateMetadata] = "updatemetadata";

// Initialize a map of built-in request types. Returns ReqType given built-in speech act.
	mReqTypes[QString("unknown")] = geUnknown;
	mReqTypes[QString("ampmsg")] = geAmpMsg;
	mReqTypes[QString("checkcabinet")] = geCheckCabinet;
	mReqTypes[QString("closecabinet")] = geCloseCabinet;
	mReqTypes[QString("closeconnection")] = geCloseConnection;
	mReqTypes[QString("compileall")] = geCompileAll;
	mReqTypes[QString("compilelambda")] = geCompileLambda;
	mReqTypes[QString("compilecabinet")] = geCompileCabinet;
	mReqTypes[QString("debug")] = geDebug;
	mReqTypes[QString("erasenode")] = geEraseNode; 
	mReqTypes[QString("eval")] = geEval;
	mReqTypes[QString("execute")] = geExecute;
	mReqTypes[QString("exportnode")] = 	geExportNode;
	mReqTypes[QString("exportcabinet")] = geExportCabinet;
	mReqTypes[QString("getmetadata")] = geGetMetaData;
	mReqTypes[QString("getnextlevel")] = geGetNextLevel;
	mReqTypes[QString("getdirinfo")] = geGetDirInfo;
	mReqTypes[QString("getextentnames")] = geGetExtentNames;
	mReqTypes[QString("getextentstatus")] = geGetExtentStatus;
	mReqTypes[QString("getextenttypes")] = geGetExtentTypes;
	mReqTypes[QString("getworkspacestatistics")] = geGetWorkspaceStatistics;
	mReqTypes[QString("importcabinet")] = geImportCabinet;
	mReqTypes[QString("newcabinet")] = geNewCabinet;
	mReqTypes[QString("opennode")] = geOpenNode;
	mReqTypes[QString("opencabinet")] = geOpenCabinet;
	mReqTypes[QString("runscriptfile")] = geRunScriptFile;
	mReqTypes[QString("savenode")] = geSaveNode;
	mReqTypes[QString("setbreakpoint")] = geSetBreakpoint;
	mReqTypes[QString("showconsole")] = geShowConsole;
	mReqTypes[QString("closecontext")] = geCloseContext;
	mReqTypes[QString("closesession")] = geCloseSession;
	mReqTypes[QString("connectsession")] = geConnectSession;
	mReqTypes[QString("display")] = geDisplay;
	mReqTypes[QString("enableconsolelog")] = geEnableConsoleLog;
	mReqTypes[QString("setengineflags")] = geSetEngineFlags;
	mReqTypes[QString("getconsolelog")] = geGetConsoleLog;
	mReqTypes[QString("getcontextid")] = geGetContextId;
	mReqTypes[QString("getcontextparams")] = geGetContextParams;
	mReqTypes[QString("getcurrentcontexts")] = geGetCurrentContexts;
	mReqTypes[QString("getexesession")] = geGetExeSession;
	mReqTypes[QString("retfile")] = geRetFile;
	mReqTypes[QString("getsessions")] = geGetSessions;
	mReqTypes[QString("getsessionid")] = geGetSessionId;
	mReqTypes[QString("getsessionuser")] = geGetSessionUser;
	mReqTypes[QString("getsubscriptions")] = geGetSubscriptions;
	mReqTypes[QString("iscontextbusy")] = geIsContextBusy;
	mReqTypes[QString("iscontextopen")] = geIsContextOpen;
	mReqTypes[QString("logoff")] = geLogoff;
	mReqTypes[QString("logon")] = geLogon;
	mReqTypes[QString("noop")] = geNoop;
	mReqTypes[QString("openconnection")] = geOpenConnection;
	mReqTypes[QString("openconsolelog")] = geOpenConsoleLog;
	mReqTypes[QString("opencontext")] = geOpenContext;
	mReqTypes[QString("opensession")] = geOpenSession;
	mReqTypes[QString("registercontext")] = geRegisterContext;
	mReqTypes[QString("registerdirectory")] = geRegisterDirectory;
	mReqTypes[QString("retquery")] = geRetQuery;
	mReqTypes[QString("rettext")] = geRetText;
	mReqTypes[QString("returl")] = geRetUrl; 
	mReqTypes[QString("seterrortrace")] = geSetErrorTrace;
	mReqTypes[QString("setescape")] = geSetEscape;
	mReqTypes[QString("setinstructiontrace")] = geSetInstructionTrace;
	mReqTypes[QString("setjit")] = geSetJit;
	mReqTypes[QString("setrules")] = geSetRules;
	mReqTypes[QString("setsubscriptions")] = geSetSubscriptions;
	mReqTypes[QString("setsyscheck")] = geSetSysCheck;
	mReqTypes[QString("setloglvl")] = geSetLogLvl;
	mReqTypes[QString("fcndebug")] = geFcnDebug;
	mReqTypes[QString("fcnerror")] = geFcnError;
	mReqTypes[QString("fcnfileopen")] = geFcnFileOpen;
	mReqTypes[QString("fcnreadhtmlpage")] = geFcnReadHtmlPage;
	mReqTypes[QString("fcnreturnresult")] = geFcnReturnResult;
	mReqTypes[QString("fcnringbell")] = geFcnRingBell;
	mReqTypes[QString("fcnsendtoclient")] = geFcnSendToClient;
	mReqTypes[QString("fcnenginestate")] = geFcnEngineState;
	mReqTypes[QString("logall")] = geLogAll;
	mReqTypes[QString("logamp")] = geLogAmp;
	mReqTypes[QString("logconsole")] = geLogConsole;
	mReqTypes[QString("logreqhdr")] = geLogReqHdr;
	mReqTypes[QString("logsysmsg")] = geLogSysMsg;
	mReqTypes[QString("loguseraccess")] = geLogUserAccess;
	mReqTypes[QString("logstatus")] = geLogStatus;
	mReqTypes[QString("proccodelines")] = geProcCodeLines;
	mReqTypes[QString("procdebuglineedit")] = geProcDebugLineEdit;
	mReqTypes[QString("procinfolines")] = geProcInfoLines;
	mReqTypes[QString("procsetdebuggeractive")] = geProcSetDebuggerActive;
	mReqTypes[QString("adduser")] = geAddUser;
	mReqTypes[QString("deleteuser")] = geDeleteUser;
	mReqTypes[QString("getusers")] = geGetUsers;
	mReqTypes[QString("updateuser")] = geUpdateUser;
	mReqTypes[QString("getconnectionstats")] = geGetConnectionStats;
	mReqTypes[QString("getlogonstats")] = geGetLogonStats;
	mReqTypes[QString("getsessionstats")] = geGetSessionStats;
	mReqTypes[QString("getrequeststats")] = geGetRequestStats;
	mReqTypes[QString("updatemetadata")] = geUpdateMetadata;

// areqtypes.h
