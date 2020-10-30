package com.ecwise.ais;

public enum ARequestType {
	Unknown,			// unknown request
	AmpMsg,				// evaluate AMP message
	CheckCabinet,		// call to (browseLib.checkStatus cabName)
	CloseCabinet,		// call to (browseLib.dropExent cabName)
	CompileAll,			// call to browseLib.compileAllAutoCompileCabinets
	CompileLambda,		// call to browseLib.compileSource
	CompileCabinet,		// call to (browseLib.compileAll true)
	Debug,				// Debugger command
	EraseNode,			// call to browseLib.eraseSource
	Eval,				// evaluate SmartLisp expression
	Execute,			// evaluate SmartLisp expression and return result binary closure record
	ExportNode,			// call to (browseLib.exportSource fileName LambdaName)
	ExportCabinet,		// call to (browseLib.exportSource fileName ..allLambdas..)
	GetDirInfo,
	GetExtentNames,
	GetExtentStatus,    // call to (browseLib.getExtentStatus)
	GetExtentTypes,		// call to (browseLib.getTypes extentname)
	GetMetaData,		// call to retrieve Meta Data information (browseLib.getMetaData)
	GetNextLevel,
	GetWorkspaceStatistics,
	ImportCabinet,		// call to (browseLib.importSource fileName)
	Logoff,				// built-in ais request
	Logon,				// built-in ais request
	NewCabinet,			// call to (browseLib.addExtent cabName cabPath)
	OpenNode,			// call to browseLib.checkout
	OpenCabinet,		// call to (browseLib.addExtent cabName cabPath)
	RegisterDirectory,	// call to (browseLib.registerDirectory databaseDir ...)
	RunScriptFile,
	SaveNode,			// call to browseLib.checkin
	SetBreakpoint,
	SetLogLvl,			// Diverted to log mgr
	ShowConsole,
	UpdateMetaData,
	// Immediate returns from AisMgr
	CloseConnection,
	CloseContext,
	CloseSession,
	ConnectSession,
	Display,			// Requested console output
	EnableConsoleLog,
	GetConsoleLog,
	GetContextId,
	GetContextParams,
	GetCurrentContexts,
	GetExeSession,
	GetSessions,
	GetSessionId,
	GetSessionUser,		// Requires session ID
	GetSubscriptions,
	IsContextBusy,
	IsContextOpen,
	Noop,
	OpenConnection,
	OpenConsoleLog,		// Requires session ID
	OpenContext,
	OpenSession,
	RegisterContext,
	SetEngineFlags,		// Requires session ID
	SetErrorTrace,		// Requires session ID
	SetEscape,
	SetInstructionTrace,	// Requires session ID
	SetJit,				// Requires session ID
	SetRules,
	SetSubscriptions,
	SetSysCheck,			// Requires session ID
	// Additional output function types. Never a request, only used in output dispatch.
	FcnDebug,
	FcnError,
	FcnFileOpen,
	FcnReadHtmlPage,
	FcnReturnResult,
	FcnRingBell,
	FcnEngineState,
	FcnSendToClient,
	FcnSendStatusToClient,
	// Additional log types. Never a request, only used in output dispatch.
	LogAll,
	LogAmp,
	LogConsole,
	LogReqHdr,
	LogSysMsg,		// Can be a request from appClient
	LogUserAccess,
	LogStatus,

	// Outputs for various returned enctypes
	RetFile,
	RetQuery,
	RetText,
	RetUrl,
	// Local events posted in client. Never requested or returned from AIS.
	ProcCodeLines,
	ProcDebugLineEdit,
	ProcInfoLines,
	ProcSetDebuggerActive,
	ConnectionError,

	// User Management Request Types
	AddUser,
	UpdateUser,
	DeleteUser,
	GetUsers,

	// System Resource Monitor Request Types
	GetConnectionStats,
	GetLogonStats,
	GetSessionStats,
	GetRequestStats,

	ReqTypeSz						// Always last, never used.
}
