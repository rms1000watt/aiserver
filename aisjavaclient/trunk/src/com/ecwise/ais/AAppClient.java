package com.ecwise.ais;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Hashtable;

/**
 * Java implementation of the C++ AAppClient class. This class provides
 * an interface for a client to talk to the AIS server.
 * Most of the method names and signatures match their C++ counterparts.
 * 
 * Calls that sends a request to the server returns immediately with
 * a request id. The result from a request is returned through a callback
 * interface (AReturnReceiver).
 * @author Franklin Chua
 *
 */
public class AAppClient implements ASocketEventHandler {

	/**
	 * Constructor.
	 * @param iHost
	 * @param iPort
	 * @param iReceiver
	 * @param iName
	 */
	public AAppClient(String iHost, int iPort, 
			AReturnReceiver iReceiver, String iName) {
		
		host = iHost;
		port = iPort;
		defaultReceiver = iReceiver;
		name = iName;
		
		xId = 0;
		requests = new Hashtable<Integer, AClientRequest>();
		extents = new Hashtable<String, AExtentInfo>();
		extentTypeOptions = new Hashtable<String, String>();
		contextParameters = new Hashtable<String, String>();
		
		currentExtent = "";
	}
	
	/**
	 * Returns true if error trace is on.
	 * @return
	 */
	public boolean isErrorTrace() {
		
		return ((engineFlags & ENGINE_ERROR_TRACE) != 0);
	}
	
	/**
	 * Returns true if instruction trace is on.
	 * @return
	 */
	public boolean isInstructionTrace() {
		
		return ((engineFlags & ENGINE_INSTRUCTION_TRACE) != 0);
	}
	
	/**
	 * Returns true if JIT is on.
	 * @return
	 */
	public boolean isJit() {
		
		return ((engineFlags & ENGINE_JITON) != 0);
	}
	
	/**
	 * Returns true if system check is on.
	 * @return
	 */
	public boolean isSysCheck() {
		
		return ((engineFlags & ENGINE_SYSCHECKON) != 0);
	}
	
	/**
	 * Returns the server name.
	 * @return
	 */
	public String getServerName() {
		
		return name;
	}
	
	/**
	 * Returns the user id of the logged in user.
	 * @return
	 */
	public int getUserId() {
		
		return userId;
	}
	
	/**
	 * Returns true if client is connected.
	 * @return
	 */
	public boolean isConnected() {
		
		return isConnected;
	}
	
	/**
	 * Sets the remote host.
	 * @param iHost
	 */
	public void setHost(String iHost) {
		
		host = iHost;
	}
	
	/**
	 * Sets the remote port.
	 * @param iPort
	 */
	public void setPort(int iPort) {
		
		port = iPort;
	}
	
	/**
	 * Sets the log event handler.
	 * @param iLogReceiver
	 */
	public void setLogEventHandler(ALogReceiver iLogReceiver) {
		
		logReceiver = iLogReceiver;
	}
	
	/**
	 * Sets the default event handler.
	 * @param iReturnReceiver
	 */
	public void setDefaultEventHandler(AReturnReceiver iDefaultReceiver) {
		
		defaultReceiver = iDefaultReceiver;
	}
	
	/*
	 * Connection routines.
	 */
	
	/**
	 * Opens a connection to AIS.
	 * @param iReceiver
	 * @return
	 */
	public int openConnection(AReturnReceiver iReceiver) {
		
		int aXid = 0;
		int aStatus = 0;
		int aPendingReq = 0;
		String aAmpMsg = AAppMessage.OpenConnection;
		
		aPendingReq = requests.size();
		
		if (isConnected) {
			aStatus = 0;
		} else if (sessionId > 0) {
			aStatus = AErrorCode.SessionOpen.ordinal();
		} else if (aPendingReq > 0) {
			aStatus = AErrorCode.ReqPending.ordinal();
		} else {
			openConnXid = addRequest(aAmpMsg, "", 0, null, iReceiver, ARequestType.OpenConnection);
			
			if (appSocket == null) {
				appSocket = new AAppSocketNoAsync();
				appSocket.setSocketEventHandler(this);
			}
			
			if (appSocket.openConnection(host, port)) {
				aStatus = -1;
			} else {
				aStatus = AErrorCode.ReqPending.ordinal();
			}
		}
		
		if (aStatus >= 0) {
			aXid = submitError(iReceiver, 
					aStatus, 
					ARequestType.OpenConnection,
					null,
					null,
					0,
					null);
		}
		
		return aXid;
	}
	
	/**
	 * Closes the specified connection or the associated connection.
	 * @param iReceiver
	 * @param iConnectionId
	 * @param iCloseMode
	 * @return
	 */
	public int closeConnection(AReturnReceiver iReceiver, int iConnectionId, ACloseMode iCloseMode) {
		
		String aAmpMsg = String.format(AAppMessage.CloseConnection, 
				iConnectionId, globals.getCloseModeFromInt(iCloseMode));
		closeConnXid = submit(iReceiver, aAmpMsg);

		return closeConnXid;
	}
	
	/*
	 * Session routines.
	 */
	
	/**
	 * Opens a session in the given context.
	 * @param iReceiver
	 * @param iContextName
	 * @param iCustomData
	 * @return
	 */
	public int openSession(AReturnReceiver iReceiver, String iContextName) {
		
		int aXid = 0;
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.OpenSession, iContextName, userId);
		
		if (iContextName.isEmpty()) {
			aStatus = AErrorCode.UnkContextName.ordinal();
		} else if (userId <= 0) {
			aStatus = AErrorCode.NoLogon.ordinal();
		} else if (sessionId < 0) {
			aStatus = 0; // AErrorCode.SessionOpen
		} else {
			aStatus = -1;
		}
		
		if (aStatus >= 0) {
			aXid = submitError(iReceiver, aStatus, ARequestType.OpenSession, aAmpMsg, null, 0, null);
		} else {
			aXid = submit(iReceiver, aAmpMsg, null, 0, null, null);
		}
		
		return aXid;
	}	
	
	/**
	 * Connects to a disconnected session.
	 * @param iReceiver
	 * @param iSessionId
	 * @return
	 */
	public int connectSesion(AReturnReceiver iReceiver, int iSessionId) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.ConnectSession, iSessionId);
		
		if (userId <= 0) {
			aStatus = AErrorCode.NoLogon.ordinal();
		} else if (sessionId <= 0) {
			aStatus = AErrorCode.SessionId.ordinal();
		}

		return submitCheck(iReceiver, aStatus, ARequestType.ConnectSession, aAmpMsg);
	}
	
	/**
	 * Closes an active session.
	 * @param iReceiver
	 * @param iSessionId
	 * @param iCloseMode
	 * @return
	 */
	public int closeSession(AReturnReceiver iReceiver, 
			int iSessionId, ACloseMode iCloseMode) {
		
		int aStatus = 0;
				
		if (iSessionId <= 0) {
			iSessionId = sessionId;
		}
		
		String aAmpMsg = String.format(AAppMessage.CloseSession, iSessionId, 
				globals.getCloseModeFromInt(iCloseMode));
		
		if (userId <= 0) {
			aStatus = AErrorCode.NoLogon.ordinal();
		} else if (iSessionId <= 0) {
			aStatus = AErrorCode.SessionId.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.CloseSession, aAmpMsg);
	}
	
	/**
	 * Returns the session id.
	 * @return
	 */
	public int getSessionId() {
		
		return sessionId;
	}
	
	/**
	 * Sends a request to get a list of sessions for the specified or associated context.
	 * @param iReceiver
	 * @param iContextName
	 * @return
	 */
	public int getSessions(AReturnReceiver iReceiver, String iContextName) {
		
		String aAmpMsg = String.format(AAppMessage.GetSessions);
		
		if (iContextName != null && !iContextName.isEmpty()) {
			aAmpMsg += String.format(AAppMessage.GetSessionsContext, iContextName);
		}

		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to get the active session for the current context.
	 * @param iReceiver
	 * @return
	 */
	public int getExeSession(AReturnReceiver iReceiver) {
		
		String aAmpMsg = String.format(AAppMessage.GetExeSession);
		
		return submit(iReceiver, aAmpMsg);
	}	
	
	/*
	 * Authentication routines.
	 */
	
	/**
	 * Sends a logon request.
	 * @param iReceiver
	 * @param iUsername
	 * @param iPassword
	 * @return
	 */
	public int logon(AReturnReceiver iReceiver, String iUsername, String iPassword) {
		
		return submit(iReceiver, String.format(AAppMessage.Logon, iUsername, iPassword));
	}
	
	/**
	 * Sends a logoff request.
	 * @param iReceiver
	 * @return
	 */
	public int logoff(AReturnReceiver iReceiver) {
		
		return submit(iReceiver, AAppMessage.Logoff);
	}
	
	/*
	 * Execute routine.
	 */
	
	/**
	 * Sends a request to evaluate a lisp expression.
	 * @param iReceiver
	 * @param iExp
	 * @param iData
	 * @return
	 */
	public int eval(AReturnReceiver iReceiver, String iExp, byte[] iData) {
		
		String aAmpMsg = String.format(AAppMessage.Eval, iExp);
		
		return submit(iReceiver, aAmpMsg, "", 0, iData, null);
	}
	
	/**
	 * Sends a request to evaluate a lisp expression.
	 * @param iReceiver
	 * @param iExp
	 * @return
	 */
	public int eval(AReturnReceiver iReceiver, String iExp) {
		
		return eval(iReceiver, iExp, null);
	}
	
	/**
	 * Sends a request to evaluate a lisp expression and return a binary result.
	 * @param iReceiver
	 * @param iExp
	 * @param iData
	 * @return
	 */
	public int execute(AReturnReceiver iReceiver, String iExp, byte[] iData) {
		
		String aAmpMsg = String.format(AAppMessage.Execute, iExp);
		
		return submit(iReceiver, aAmpMsg, "", 0, iData, null);
	}
	
	/**
	 * Sends a request to evaluate a lisp expression and display the result.
	 * @param iReceiver
	 * @param iExp
	 * @return
	 * @note AMP format: _ais|showconsole|exp|%s
	 */
	public int showConsole(AReturnReceiver iReceiver, String iExp) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.ShowConsole, iExp);
		
		if (iExp == null || iExp.isEmpty()) {
			aStatus = AErrorCode.NoInput.ordinal();
		}

		return submitCheck(iReceiver, aStatus, ARequestType.ShowConsole, aAmpMsg);
	}
	
	/**
	 * Sends a request to execute a remote script.
	 * @param iReceiver
	 * @param iFilePath
	 * @param iPrefix
	 * @return
	 * @note AMP format: _ais|runscriptfile|file|%s|prefix|%s
	 */
	public int runRemoteScriptFile(AReturnReceiver iReceiver, String iFilePath, String iPrefix) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.RunScriptFile, iFilePath);

		if (iPrefix != null && !iPrefix.isEmpty()) {
			aAmpMsg += String.format(AAppMessage.RunScriptFilePrefix, iPrefix);
		}
		
		if (iFilePath == null || iFilePath.isEmpty()) {
			aStatus = AErrorCode.BadFilename.ordinal();
		}

		return submitCheck(iReceiver, aStatus, ARequestType.RunScriptFile, aAmpMsg);
	}
	
	/*
	 * Context-related operations.
	 */
	
	/**
	 * Returns the value of a specified context parameter.
	 * @param iName
	 * @return
	 */
	public String getContextParameters(String iName) {
		
		return (contextParameters != null) ? contextParameters.get(iName) : null;
	}
	
	/**
	 * Returns the context parameters Hashtable.
	 * @return
	 */
	public Hashtable<String, String> getContextParameters() {
		
		return contextParameters;
	}
	
	/**
	 * Sends a request to get a list of active contexts.
	 * @param iReceiver
	 * @return
	 * @note AMP format: _ais|getcurrentcontexts
	 */
	public int getCurrentContexts(AReturnReceiver iReceiver) {
		
		String aAmpMsg = String.format(AAppMessage.GetCurrentContexts);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to retrieve context parameters.
	 * The parameters will be extracted and can be accessed using getContextParam.
	 * @param iReceiver
	 * @param iContext
	 * @return
	 * @note AMP format: _ais|getcontextparams|context|%s
	 */
	public int getContextParams(AReturnReceiver iReceiver, String iContext) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.GetContextParams, iContext);
		
		if (iContext == null || iContext.isEmpty()) {
			aStatus = AErrorCode.UnkContextName.ordinal();
		} else if (userId <= 0) {
			aStatus = AErrorCode.NoLogon.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.GetContextParams, aAmpMsg);
	}
	
	/**
	 * Sends a request to determine if a context is active.
	 * @param iReceiver
	 * @param iContext
	 * @return
	 * @note AMP format: _ais|iscontextopen|context|%s
	 */
	public int isContextOpen(AReturnReceiver iReceiver, String iContext) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.IsContextOpen, iContext);
		
		if (iContext == null || iContext.isEmpty()) {
			aStatus = AErrorCode.UnkContextName.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.IsContextOpen, aAmpMsg);
	}
	
	/**
	 * Sends a request to determine if a context is busy.
	 * @param iReceiver
	 * @param iContext
	 * @return
	 * @note AMP format: _ais|iscontextbusy|context|%s
	 */
	public int isContextBusy(AReturnReceiver iReceiver, String iContext) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.IsContextBusy, iContext);
		
		if (iContext == null || iContext.isEmpty()) {
			aStatus = AErrorCode.UnkContextName.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.IsContextBusy, aAmpMsg);
	}
	
	/**
	 * Sends a request to start a context.
	 * @param iReceiver
	 * @param iStartupPath This is optional if context is already registered.
	 * @param iContext This is optional if path points to an existing startup file.
	 * @return
	 * @note AMP format: _ais|opencontext|context|%s|path|%s
	 */
	public int openContext(AReturnReceiver iReceiver, 
			String iStartupPath, String iContext) {
		
		int aStatus = AErrorCode.UnkContextName.ordinal();
		String aAmpMsg = String.format(AAppMessage.OpenContext);
		
		if (iContext != null && !iContext.isEmpty()) {
			aStatus = 0;
			aAmpMsg += String.format(AAppMessage.OpenContextName, iContext);
		}
		
		if (iStartupPath != null && !iStartupPath.isEmpty()) {
			aStatus = 0;
			aAmpMsg += String.format(AAppMessage.OpenContextPath, iStartupPath);
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.OpenContext, aAmpMsg);
	}
	
	/**
	 * Sends a request to register a context.
	 * @param iReceiver
	 * @param iStartupPath Path to startup file.
	 * @return
	 * @note AMP format: _ais|registercontext|path|%s
	 */
	public int registerContext(AReturnReceiver iReceiver, String iStartupPath) {
		
		String aAmpMsg = String.format(AAppMessage.RegisterContext, iStartupPath);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to stop a context.
	 * @param iReceiver
	 * @param iContext
	 * @param iCloseMode
	 * @return
	 * @note AMP format: _ais|closecontext|context|%s|closemode|%s
	 */
	public int closeContext(AReturnReceiver iReceiver, String iContext, ACloseMode iCloseMode) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.CloseContext,
				iContext, globals.getCloseModeFromInt(iCloseMode));
		
		if (iContext == null || iContext.isEmpty()) {
			aStatus = AErrorCode.UnkContextName.ordinal();
		} else if (userId <= 0) {
			aStatus = AErrorCode.NoLogon.ordinal();
		}
	
		return submitCheck(iReceiver, aStatus, ARequestType.CloseContext, aAmpMsg);
	}
	
	/**
	 * Sends a request to get the list of subscribers on the specified context.
	 * @param iReceiver
	 * @param iContext
	 * @return
	 * @note AMP format: _ais|getsubscriptions|context|%s
	 */
	public int getSubscriptions(AReturnReceiver iReceiver, String iContext) {
		
		String aAmpMsg = String.format(AAppMessage.GetSubscriptions);
		
		if (iContext != null && !iContext.isEmpty()) {
			aAmpMsg += String.format(AAppMessage.GetSubscriptionsContext, iContext);
		}
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to receive console output of a session.
	 * @param iReceiver
	 * @param iNewSessions
	 * @param iOldSessions
	 * @return
	 * @note AMP format: _ais|setsubscriptions|new|%s|old|%s
	 */
	public int setSubscriptions(AReturnReceiver iReceiver, 
			String iNewSessions, String iOldSessions) {
		
		String aAmpMsg = String.format(AAppMessage.SetSubscriptions, iNewSessions, iOldSessions);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to get the workspace statistics.
	 * @param iReceiver
	 * @return
	 */
	public int getWorkspaceStatistics(AReturnReceiver iReceiver) {
		
		String aAmpMsg = String.format(AAppMessage.GetWorkspaceStatistics);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/*
	 * Debug routines.
	 */
	
	/*
	 * Cabinet routines.
	 */
	
	/**
	 * Sends a request to create a cabinet.
	 * @param iReceiver
	 * @param iCabinet
	 * @param iPath
	 * @param iLocation
	 * @param iStorage
	 * @param iImportSync
	 * @param iExportSync
	 * @param iAutoCompile
	 * @param iSearchSwitch
	 * @param iDependencies
	 * @return
	 */
	public int newCabinet(AReturnReceiver iReceiver, String iCabinet, String iPath,
			String iLocation, String iStorage, String iImportSync, String iExportSync,
			String iAutoCompile, String iSearchSwitch, String iDependencies) {
	
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.NewCabinet, iCabinet, iPath,
				iLocation, iStorage, iImportSync, iExportSync, iAutoCompile,
				iSearchSwitch, iDependencies);
		
		if (!iCabinet.isEmpty() && !iPath.isEmpty()) {
			if (extents.containsKey(iCabinet)) {
				aStatus = AErrorCode.CabinetOpen.ordinal();
			}
		} else {
			aStatus = AErrorCode.BadCabinet.ordinal();
		}

		return submitCheck(iReceiver, aStatus, ARequestType.NewCabinet, aAmpMsg);
	}
	
	/**
	 * Sends a request to open an existing cabinet.
	 * @param iReceiver
	 * @param iCabinet
	 * @param iPath
	 * @param iLocation
	 * @param iStorage
	 * @param iImportSync
	 * @param iExportSync
	 * @param iAutoCompile
	 * @param iSearchSwitch
	 * @param iDependencies
	 * @return
	 */
	public int openCabinet(AReturnReceiver iReceiver, String iCabinet, String iPath,
			String iLocation, String iStorage, String iImportSync, String iExportSync,
			String iAutoCompile, String iSearchSwitch, String iDependencies) {

		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.OpenCabinet, iCabinet, iPath,
				iLocation, iStorage, iImportSync, iExportSync, iAutoCompile,
				iSearchSwitch, iDependencies);
		
		if (!iCabinet.isEmpty() && !iPath.isEmpty()) {
			if (extents.containsKey(iCabinet)) {
				aStatus = AErrorCode.CabinetOpen.ordinal();
			}
		} else {
			aStatus = AErrorCode.BadCabinet.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.OpenCabinet, aAmpMsg);
	}
	
	/**
	 * Sends a request to close a cabinet.
	 * @param iReceiver
	 * @param iCabinet
	 * @return
	 * @note AMP format: _ais|closecabinet|cabname|%s
	 */
	public int closeCabinet(AReturnReceiver iReceiver, String iCabinet) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.CloseCabinet, iCabinet);
		
		if (iCabinet == null || iCabinet.isEmpty()) {
			aStatus = AErrorCode.BadCabinet.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.CloseCabinet, aAmpMsg);
	}
	
	/**
	 * Sends a request to check the cabinet's sync state.
	 * @param iReceiver
	 * @param iCabinet
	 * @return
	 */
	public int checkCabinet(AReturnReceiver iReceiver, String iCabinet) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.CheckCabinet, iCabinet);
		
		if (iCabinet.isEmpty()) {
			aStatus = AErrorCode.BadCabinet.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.CheckCabinet, aAmpMsg);
	}
	
	/**
	 * Sends a request to get information on a directory.
	 * @param iReceiver
	 * @param iDir
	 * @return
	 * @note AMP format: _ais|getdirinfo|dir|%s
	 * Each string is a DEL-delimited string of the form type|name|size|date|time
	 * Type is one of the following letters: D=drive, R=rootdir, C=currentdir, D=directory, F=file
	 */
	public int getDirInfo(AReturnReceiver iReceiver, String iDir) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.GetDirInfo, iDir);
		
		if (iDir == null || iDir.isEmpty()) {
			aStatus = AErrorCode.NoInput.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.GetDirInfo, aAmpMsg);
	}
	
	/**
	 * Sends a request to register all cabinets in a directory.
	 * @param iReceiver
	 * @param iDatabaseDir
	 * @param iSourceDir
	 * @return
	 */
	public int registerDirectory(AReturnReceiver iReceiver, String iDatabaseDir, String iSourceDir) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.RegisterDirectory, iDatabaseDir, iSourceDir);
		
		if (iDatabaseDir == null || iDatabaseDir.isEmpty()) {
			aStatus = AErrorCode.BadCabinet.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.RegisterDirectory, aAmpMsg);
	}
	
	/**
	 * Sends a request to save a cabinet as a file on the server.
	 * @param iReceiver
	 * @param iCabinet
	 * @param iPath
	 * @param iSetLocation
	 * @return
	 */
	public int exportCabinet(AReturnReceiver iReceiver, String iCabinet, String iPath, boolean iSetLocation) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.ExportCabinet, iCabinet, "file", iPath, iSetLocation);
		
		if (iCabinet == null || iCabinet.isEmpty() || iPath == null || iPath.isEmpty()) {
			aStatus = AErrorCode.BadCabinet.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.ExportCabinet, aAmpMsg);
	}
	
	/**
	 * Sends a request to save a cabinet as a folder on the server.
	 * @param iReceiver
	 * @param iCabinet
	 * @param iPath
	 * @param iSetLocation
	 * @return
	 * @note AMP format: _ais|exportcabinet|cabinet|%s|folder|%s
	 */
	public int exportCabinetDir(AReturnReceiver iReceiver, String iCabinet, String iPath, boolean iSetLocation) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.ExportCabinet, iCabinet, "folder", iPath, iSetLocation);
		
		if (iCabinet == null || iCabinet.isEmpty() || iPath == null || iPath.isEmpty()) {
			aStatus = AErrorCode.BadCabinet.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.ExportCabinet, aAmpMsg);
	}
	
	/**
	 * Sends a request to load a cabinet from a file (.sl) on the server.
	 * @param iReceiver
	 * @param iCabinet
	 * @param iPath
	 * @param iSetLocation
	 * @return
	 * @note AMP format: _ais|importcabinet|cabinet|%s|file|%s
	 */
	public int importCabinet(AReturnReceiver iReceiver, String iCabinet, String iPath, boolean iSetLocation) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.ImportCabinet, iCabinet, "file", iPath, iSetLocation);
		
		if (iCabinet == null || iCabinet.isEmpty() || iPath == null || iPath.isEmpty()) {
			aStatus = AErrorCode.BadCabinet.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.ImportCabinet, aAmpMsg);
	}
	
	/**
	 * Sends a request to load a cabinet from a folder on the server.
	 * @param iReceiver
	 * @param iCabinet
	 * @param iPath
	 * @param iSetLocation
	 * @return
	 * @note AMP format: _ais|importcabinet|cabinet|%s|folder|%s
	 */
	public int importCabinetDir(AReturnReceiver iReceiver, String iCabinet, String iPath, boolean iSetLocation) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.ImportCabinet, iCabinet, "folder", iPath, iSetLocation);
		
		if (iCabinet == null || iCabinet.isEmpty() || iPath == null || iPath.isEmpty()) {
			aStatus = AErrorCode.BadCabinet.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.ImportCabinet, aAmpMsg);
	}
	
	/**
	 * Sends a request to compile a list of cabinets
	 * @param iReceiver
	 * @param iExtentNames
	 * @return
	 * @note AMP format: _ais|compilecabinet|extents|%s
	 */
	public int compileCabinet(AReturnReceiver iReceiver, String[] iExtentNames) {

		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.CompileCabinet);
		
		if (iExtentNames == null || iExtentNames.length == 0) {
			aStatus = AErrorCode.NoExtent.ordinal();
			aAmpMsg += "noExtents";
		} else {
			aAmpMsg += AUtilities.joinStringArray(iExtentNames, "\t");
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.CompileCabinet, aAmpMsg);
	}
	
	/**
	 * Sends a request to compile a list of lambdas
	 * @param iReceiver
	 * @param iLambdas
	 * @return
	 * @note AMP format: _ais|compilelambda|extent_Lambdas|%s
	 */
	public int compileLambda(AReturnReceiver iReceiver, String[] iLambdas) {
		
		String aAmpMsg = String.format(AAppMessage.CompileLambda);
		int aStatus = 0;
		
		if (iLambdas != null && iLambdas.length > 0) {
			aAmpMsg += AUtilities.joinStringArray(iLambdas, "\t");
		} else {
			aAmpMsg += "noExtents";
			aStatus = AErrorCode.NoExtent.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.CompileLambda, aAmpMsg);
	}
	
	/**
	 * Sends a request to compile all cabinets.
	 * @param iReceiver
	 * @return
	 * @note AMP format: _ais|compileall
	 */
	public int compileAllCabinets(AReturnReceiver iReceiver) {
		
		return submit(iReceiver, AAppMessage.CompileAll);
	}
	
	/**
	 * Sends a request to retrieve a cabinet's meta data.
	 * @param iReceiver
	 * @param iCabinet
	 * @return
	 * @note AMP format: _ais|getmetadata|extent|%s
	 */
	public int getMetaData(AReturnReceiver iReceiver, String iCabinet) {
		
		String aAmpMsg = String.format(AAppMessage.GetMetaData, iCabinet);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to update a cabinet's meta data.
	 * @param iReceiver
	 * @param iCabinet
	 * @param iPath
	 * @param iLocation
	 * @param iStorage
	 * @param iImportSync
	 * @param iExportSync
	 * @param iAutoCompile
	 * @param iSearchSwitch
	 * @param iDependencies
	 * @return
	 * @note AMP format: _ais|updatemetadata|cabinet|%s|path|%s|location|%s|storage|%s|importsync|%s|exportsync|%s|autocompile|%s
	 */
	public int setMetaData(AReturnReceiver iReceiver, String iCabinet,
			String iPath, String iLocation, String iStorage,
			String iImportSync, String iExportSync, String iAutoCompile,
			String iSearchSwitch, String iDependencies) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.UpdateMetaData, iCabinet,
				iPath, iLocation, iStorage, iImportSync, iExportSync,
				iAutoCompile, iSearchSwitch, iDependencies);
		
		if (iCabinet == null || iCabinet.isEmpty() ||
				iPath == null || iPath.isEmpty() ||
				iLocation == null || iLocation.isEmpty() ||
				iStorage == null || iStorage.isEmpty() ||
				iImportSync == null || iImportSync.isEmpty() ||
				iExportSync == null || iExportSync.isEmpty() ||
				iAutoCompile == null || iAutoCompile.isEmpty() ||
				iSearchSwitch == null || iSearchSwitch.isEmpty() ||
				iDependencies == null || iDependencies.isEmpty()) {
			aStatus = AErrorCode.BadCabinet.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.UpdateMetaData, aAmpMsg);
	}
	
	/**
	 * Returns the names of the lambdas/variables in the current extent.
	 * @return
	 */
	public String[] getCurrentNodeNames() {
		
		String[] aNodeNames = null;
		// get the current extent
		AExtentInfo aExtentInfo = extents.get(currentExtent);
		
		if (aExtentInfo != null) {
			// get the nodes of the current extent
			ArrayList<ANodeInfo> aNodes = aExtentInfo.getNodes();
			int aSize = aNodes.size();
			aNodeNames = new String[aSize];
			for (int i = 0; i < aSize; i++) {
				aNodeNames[i] = aNodes.get(i).getValue();
			}
		}
		
		return aNodeNames;
	}
	
	/**
	 * Get the name of the variable/lambda/file for the specified extent/node.
	 * @param iExtent
	 * @param iIndex
	 * @return
	 */
	public String getFullNodeName(String iExtent, int iIndex) {
		
		String aNodeName = null;
		AExtentInfo aExtentInfo = extents.get(iExtent);

		if (aExtentInfo != null && (iIndex < aExtentInfo.getNodes().size())) {
			aNodeName = aExtentInfo.getNodes().get(iIndex).getSymbol();
			if (!aNodeName.isEmpty() && !aExtentInfo.getNodePath().isEmpty()) {
				aNodeName = aExtentInfo.getNodePath() + "/" + aNodeName;
			}
		}
		
		return aNodeName;
	}
	
	/**
	 * Get the name of the variable/lambda/file for the specified node.
	 * @param iIndex
	 * @return
	 */
	public String getFullNodeName(int iIndex) {
		
		return getFullNodeName(currentExtent, iIndex);
	}
	
	/**
	 * Get the node information of the specified node of the current extent.
	 */
	public ANodeInfo getNodeInfo(int iNodeIndex) {
		
		ANodeInfo aNodeInfo = null;
		AExtentInfo aExtentInfo = extents.get(currentExtent);
		
		if (aExtentInfo != null) {
			if (iNodeIndex >= 0 && iNodeIndex < aExtentInfo.getNodes().size()) {
				aNodeInfo = aExtentInfo.getNodes().get(iNodeIndex);
			}
		}
		
		return aNodeInfo;
	}
	
	/**
	 * Get the list of actions for the specified node of the current extent.
	 * @param iNodeIndex
	 * @return
	 */
	public String[] getNodeActions(int iNodeIndex) {
		
		String[] aActions = null;
		ANodeInfo aNodeInfo = getNodeInfo(iNodeIndex);

		if (aNodeInfo != null) {
			aActions = getServerTypeActions(aNodeInfo.getType(), currentExtent);
		}
		
		return aActions;
	}
	
	/**
	 * Get the name of the lambda/variable/file for the specified node of the current extent.
	 * @param iNodeIndex
	 * @return
	 */
	public String getNodeSymbol(int iNodeIndex) {
		
		String aSymbol = null;
		ANodeInfo aNodeInfo = getNodeInfo(iNodeIndex);

		if (aNodeInfo != null) {
			aSymbol = aNodeInfo.getSymbol();
		}
		
		return aSymbol;
	}
	
	/**
	 * Returns the node paths of the current extent.
	 * @return
	 */
	public String[] getNodePathTree() {
		
		AExtentInfo aExtentInfo = extents.get(currentExtent);
		String[] aNodePathTree = null;

		if (aExtentInfo != null) {
			String[] aNodePaths = aExtentInfo.getNodePath().split("/");
			int aSize = (aNodePaths.length == 1 && aNodePaths[0].isEmpty()) ? 0 : aNodePaths.length;

			aNodePathTree = new String[aSize + 1];
			aNodePathTree[0] = "/";
			
			for (int i = 0; i < aSize; i++) {
				StringBuilder aPath = new StringBuilder();
				
				for (int j = 0; j <= i; j++) {
					aPath.append("/" + aNodePaths[j]);
				}
				
				aNodePathTree[i + 1] = aPath.toString();
			}
		}
		
		return aNodePathTree;
	}
	
	/**
	 * Sends a request to get nodes at the top level.
	 * @param iReceiver
	 * @return
	 */
	public int getRootLevel(AReturnReceiver iReceiver) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.GetNextLevel, 
				currentExtent, getExtentTypeOptions(currentExtent), "");
		
		if (currentExtent == null || currentExtent.isEmpty() ||
				!extents.containsKey(currentExtent)) {
			aStatus = AErrorCode.NoExtent.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.GetNextLevel, aAmpMsg);
	}
	
	/**
	 * Sends a request to get the nodes at the level below the current node.
	 * @param iReceiver
	 * @return
	 */
	public int getNextLevel(AReturnReceiver iReceiver) {
		
		int aStatus = 0;
		AExtentInfo aExtentInfo = extents.get(currentExtent);
		String aNodePath = (aExtentInfo != null) ? aExtentInfo.getNodePath() : null;
		String aAmpMsg = String.format(AAppMessage.GetNextLevel, 
				currentExtent, aNodePath, getExtentTypeOptions(currentExtent));
		
		if (currentExtent == null || currentExtent.isEmpty() ||
				!extents.containsKey(currentExtent)) {
			aStatus = AErrorCode.NoExtent.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.GetNextLevel, aAmpMsg);
	}
	
	/**
	 * Sends a request to get the nodes at the level below the specified node.
	 * @param iReceiver
	 * @return
	 */
	public int getNextLevel(AReturnReceiver iReceiver, String iFullNodeName) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.GetNextLevel, 
				currentExtent, iFullNodeName, getExtentTypeOptions(currentExtent));
		
		if (currentExtent == null || currentExtent.isEmpty() ||
				!extents.containsKey(currentExtent)) {
			aStatus = AErrorCode.NoExtent.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.GetNextLevel, aAmpMsg);
	}
	
	/**
	 * Sends a request to updates the nodes at the current extent and node path.
	 * @param iReceiver
	 * @return
	 */
	public int refreshLevel(AReturnReceiver iReceiver) {
		return getNextLevel(iReceiver);
	}
	
	/**
	 * Sends a request to get the nodes at the level above the specified node.
	 * @param iReceiver
	 * @return
	 */
	public int getPreviousLevel(AReturnReceiver iReceiver) {
		
		int aStatus = 0;
		AExtentInfo aExtentInfo = extents.get(currentExtent);
		String aNodePath = "";
		String aOptions = "";
		
		if (aExtentInfo != null) {
			String[] aNodes = aExtentInfo.getNodePath().split("/");
			if (aNodes.length == 0) {
				aStatus = AErrorCode.AtBeg.ordinal();
			} else {
				aNodePath = AUtilities.joinStringArray(aNodes, "/", aNodes.length - 1);
				aOptions = getExtentTypeOptions(currentExtent);
			}			
		} else {
			aStatus = AErrorCode.NoExtent.ordinal();
		}
		
		String aAmpMsg = String.format(AAppMessage.GetNextLevel, 
				currentExtent, aNodePath, aOptions);
		
		return submitCheck(iReceiver, aStatus, ARequestType.GetNextLevel, aAmpMsg);		
	}
	
	/**
	 * Sends a request to open a lambda/variable.
	 * @param iReceiver
	 * @param iExtent
	 * @param iFullNode
	 * @return
	 * @note AMP format: _ais|opennode|extent|%s|node|%s
	 */
	public int openNode(AReturnReceiver iReceiver, String iExtent, String iFullNode) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.OpenNode, iExtent, iFullNode);
		
		if (iExtent == null || iFullNode == null || 
				iExtent.isEmpty() || iFullNode.isEmpty()) {
			aStatus = AErrorCode.BadNode.ordinal();
		} else if (!extents.containsKey(iExtent)) {
			aStatus = AErrorCode.NoExtent.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.OpenNode, aAmpMsg);
	}
	
	/**
	 * Sends a request to remove existing lambdas from the selected cabinet.
	 * @param iReceiver
	 * @param iLambdas
	 * @return
	 * @note AMP format: _ais|erasenode|extent|%s|nodes|%s
	 */
	public int eraseNode(AReturnReceiver iReceiver, String[] iLambdas) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.EraseNode, currentExtent);
		
		if (!currentExtent.isEmpty() && iLambdas != null && iLambdas.length > 0) {
			aAmpMsg += AUtilities.joinStringArray(iLambdas, "\t");
		} else {
			aStatus = AErrorCode.NoExtent.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.EraseNode, aAmpMsg);
	}
	
	/**
	 * Sends a request to save a node.
	 * @param iReceiver
	 * @param iExtent
	 * @param iNode
	 * @param iText
	 * @return
	 * @note AMP format: _ais|savenode|extent|%s|node|text|%s
	 */
	public int saveNode(AReturnReceiver iReceiver, String iExtent, String iNode, String iText) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.SaveNode, iExtent, iNode, iText);
		
		if (iExtent == null || iExtent.isEmpty() || 
				iNode == null || iNode.isEmpty() ||
				iText == null || iText.isEmpty()) {
			aStatus = AErrorCode.BadNode.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.SaveNode, aAmpMsg);
	}
	
	/**
	 * Sends a request to save a lambda to a file (.sl) on the server.
	 * @param iReceiver
	 * @param iCabinet
	 * @param iNode
	 * @param iPath
	 * @return
	 * @note AMP format: _ais|exportnode|node|%s|file|%s|cabinet|%s
	 */
	public int exportNode(AReturnReceiver iReceiver, String iCabinet, String iNode, String iPath) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.ExportNode, iCabinet, iNode, iPath);
		
		if (iCabinet == null || iCabinet.isEmpty() ||
				iNode == null || iNode.isEmpty() ||
				iPath == null || iPath.isEmpty()) {
			aStatus = AErrorCode.BadCabinet.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.ExportNode, aAmpMsg);
	}
	
	/**
	 * Gets the list of actions for a given extent and type.
	 * @param iType
	 * @param iExtent
	 * @return
	 */
	public String[] getServerTypeActions(String iType, String iExtent) {
		
		String[] aServerActions = null;
		AExtentInfo aExtentInfo = extents.get(iExtent);
		
		if (aExtentInfo != null) {
			Hashtable<String, AExtentTypeInfo> aServerTypes = aExtentInfo.getExtentTypes();
			AExtentTypeInfo aExtentTypeInfo = null;
			if (aServerTypes.containsKey(iType)) {
				aExtentTypeInfo = aServerTypes.get(iType);
			} else if (aServerTypes.containsKey(".default.")) {
				aExtentTypeInfo = aServerTypes.get(".default.");
			}
			
			if (aExtentTypeInfo != null) {
				aServerActions = aExtentTypeInfo.getActionCodes().split(",");
			}
		}
		
		return aServerActions;
	}
	
	/**
	 * Sends a request to retrieve the list of cabinets and their import/export status.
	 * @param iReceiver
	 * @return
	 */
	public int getExtentStatus(AReturnReceiver iReceiver) {
		
		String aAmpMsg = String.format(AAppMessage.GetExtentStatus);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to retrieve a list of extent types for the current extent.
	 * @param iReceiver
	 * @param iForce
	 * @return
	 * @note AMP format: _ais|getextenttypes|extent|%s
	 */
	public int getExtentTypes(AReturnReceiver iReceiver, boolean iForce) {
		
		int aStatus = 0;
		String aAmpMsg = String.format(AAppMessage.GetExtentTypes, currentExtent);
		
		if (extents.containsKey(currentExtent) && 
				(!extents.get(currentExtent).getExtentTypes().isEmpty() || iForce)) {
			// do nothing
		} else {
			aStatus = AErrorCode.NoExtent.ordinal();
		}
		
		return submitCheck(iReceiver, aStatus, ARequestType.GetExtentTypes, aAmpMsg);
	}
	
	/**
	 * Returns the capabilities of the specified extent.
	 * @param iExtent
	 * @return
	 */
	public String getExtentTypeOptions(String iExtent) {
		
		return extentTypeOptions.containsKey(iExtent) ? extentTypeOptions.get(iExtent) : "";
	}
	
	/**
	 * Returns the extent information.
	 * @param iExtent
	 * @return
	 */
	public AExtentInfo getExtentInfo(String iExtent) {
		
		return extents.get(iExtent);
	}
	
	/**
	 * Sends a request to get the list of cabinet names from the current context.
	 * @param iReceiver
	 * @return
	 * @note AMP format: _ais|getextentnames
	 */
	public int getExtentNames(AReturnReceiver iReceiver) {
		
		return submit(iReceiver, AAppMessage.GetExtentNames);
	}
	
	/**
	 * Returns the current extent.
	 * @return
	 */
	public String getCurrentExtent() {
		
		return currentExtent;
	}
	
	/**
	 * Sets the current extent.
	 * @param iExtent
	 * @return
	 */
	public boolean setCurrentExtent(String iExtent) {
		
		boolean aFound = false;
		
		if (extents.containsKey(iExtent)) {
			currentExtent = iExtent;
			aFound = true;
		}
		
		return aFound;
	}
	
	/**
	 * Returns the current extents.
	 * @return
	 */
	public String[] getCurrentExtents() {
		
		String[] aExtents = null;
		
		if (!extents.isEmpty()) {
			aExtents = extents.keySet().toArray(new String[0]);
		}
		
		return aExtents;
	}
	
	/**
	 * Sets the rules for displaying nodes.
	 * @param iReceiver
	 * @param iRules
	 * @param iRemove
	 * @return
	 * @note AMP format: _ais|setrules|rules|%s|mode|%d
	 */
	public int setRules(AReturnReceiver iReceiver, String iRules, boolean iRemove) {
		
		String aAmpMsg = String.format(AAppMessage.SetRules, iRules, (iRemove) ? 1 : 0);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/*
	 * Engine routines.
	 */
	
	/*
	 * Logging routines.
	 */
	
	/**
	 * Sends a request to set the amount of logging to be forwarded to the client.
	 * @param iReceiver
	 * @param iReqType
	 * @param iWarnLevel
	 * @return
	 * @note AMP format: _ais|setloglvl|logtype|%d|level|%d
	 */
	public int setLogLevel(AReturnReceiver iReceiver, ARequestType iReqType,
			AErrorLevel iWarnLevel) {
		
		String aAmpMsg = String.format(AAppMessage.SetLogLevel, iReqType.ordinal(), iWarnLevel.ordinal());
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to set the console log options.
	 * @param iReceiver
	 * @param iEnable (-2 reset, -1 close, 0 suspend, 1 enable, 2 format)
	 * @return
	 * @note AMP format: _ais|enableconsolelog|enable|%s|sessionid|%d
	 */
	public int enableConsoleLog(AReturnReceiver iReceiver, int iEnable) {
		
		String aAmpMsg = String.format(AAppMessage.EnableConsoleLog, iEnable, 0);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to retrieve the buffered console output log.
	 * @param iReceiver
	 * @param iClear
	 * @return
	 * @note AMP format: _ais|getconsolelog|clear|%s|sessionid|%d
	 */
	public int getConsoleLog(AReturnReceiver iReceiver, boolean iClear) {
		
		String aAmpMsg = String.format(AAppMessage.GetConsoleLog, (iClear) ? 1 : 0, 0);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to create a new console output log for the session.
	 * @param iReceiver
	 * @param iClear
	 * @param iRedirect
	 * @param iSize
	 * @param iStartAtNewLine
	 * @return
	 * @note AMP format: _ais|openconsolelog|clear|%s|redirect|%s|sessionid|%d|size|%d|startatnewline|%s
	 */
	public int openConsoleLog(AReturnReceiver iReceiver, boolean iClear, 
			boolean iRedirect, int iSize, boolean iStartAtNewLine) {
		
		String aAmpMsg = String.format(AAppMessage.OpenConsoleLog, 
				iClear, iRedirect, iSize, iStartAtNewLine);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/*
	 * Monitoring routines.
	 */
	
	/**
	 * Sends a request to get the connection statistics.
	 * @param iReceiver
	 * @return
	 */
	public int getConnectionStats(AReturnReceiver iReceiver) {
		
		return submit(iReceiver, AAppMessage.GetConnectionStats);
	}
	
	/**
	 * Sends a request to get the logon statistics.
	 * @param iReceiver
	 * @return
	 */
	public int getLogonStats(AReturnReceiver iReceiver) {
		
		return submit(iReceiver, AAppMessage.GetLogonStats);
	}
	
	/**
	 * Sends a request to get the session statistics.
	 * @param iReceiver
	 * @return
	 */
	public int getSessionStats(AReturnReceiver iReceiver) {
		
		return submit(iReceiver, AAppMessage.GetSessionStats);
	}
	
	/**
	 * Sends a request to get the request statistics.
	 * @param iReceiver
	 * @return
	 */
	public int getRequestStats(AReturnReceiver iReceiver) {
		
		return submit(iReceiver, AAppMessage.GetRequestStats);
	}
	
	/**
	 * Sends a request to stop the execution of currently executing request of the target session.
	 * @param iReeceiver
	 * @param iSessionId
	 * @return
	 * @note AMP format: _ais|setescape|sessionid|%d
	 */
	public int setEscape(AReturnReceiver iReceiver, int iSessionId) {
		
		String aAmpMsg = String.format(AAppMessage.SetEscape, iSessionId);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to stop the execution of currently executing request of the target session.
	 * @param iReeceiver
	 * @param iSessionId
	 * @return
	 * @note AMP format: _ais|setescape|sessionid|%d
	 */
	public int setEscape(AReturnReceiver iReceiver) {
		
		String aAmpMsg = String.format(AAppMessage.SetEscape, sessionId);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to set one or more engine flags.
	 * @param iReceiver
	 * @param iFlags
	 * @return
	 * @note AMP format: _ais|setengineflags|flags|%d
	 */
	public int setEngineFlags(AReturnReceiver iReceiver, int iFlags) {
		
		String aAmpMsg = String.format(AAppMessage.SetEngineFlags, iFlags);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to set the error trace flag.
	 * @param iReceiver
	 * @param iFlag
	 * @return
	 * @note AMP format: _ais|seterrortrace|onoff|%d
	 */
	public int setErrorTrace(AReturnReceiver iReceiver, boolean iFlag) {
		
		String aAmpMsg = String.format(AAppMessage.SetErrorTrace, iFlag ? 1 : 0);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to set the instruction trace flag.
	 * @param iReceiver
	 * @param iFlag
	 * @return
	 * @note AMP format: _ais|setinstructiontrace|onoff|%d
	 */
	public int setInstructionTrace(AReturnReceiver iReceiver, boolean iFlag) {
		
		String aAmpMsg = String.format(AAppMessage.SetInstructionTrace, iFlag ? 1 : 0);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to set the JIT (compile to native code) flag.
	 * @param iReceiver
	 * @param iFlag
	 * @return
	 * @note AMP format: _ais|setjit|onoff|%d
	 */
	public int setJit(AReturnReceiver iReceiver, boolean iFlag) {
		
		String aAmpMsg = String.format(AAppMessage.SetJit, iFlag ? 1 : 0);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	/**
	 * Sends a request to set the self-check flag.
	 * @param iReceiver
	 * @param iFlag
	 * @return
	 * @note AMP format: _ais|setsyscheck|onoff|%d
	 */
	public int setSysCheck(AReturnReceiver iReceiver, boolean iFlag) {
		
		String aAmpMsg = String.format(AAppMessage.SetSysCheck, iFlag ? 1 : 0);
		
		return submit(iReceiver, aAmpMsg);
	}
	
	@Override
	public void onConnected() {

		returnOutput(0, openConnXid, 0, ARequestType.OpenConnection,
				0, "true", null, "", "");
	}

	@Override
	public void onDisconnected() {

		onConnectionClosed();
	}

	@Override
	public void onResponse(byte[] iResponse, byte[] iCookie, byte[] iData) {

		int aReturnValue;
		int aRequestId;
		int aStatus;
		int aOffset = 0;
		ARequestType aRequestType;
		String aOut;
		String aDisplay;
		String aError;
		String aTemp;
		int aLength = 0;
		
		// Request Id
		aLength = getFieldLength(iResponse, aOffset);
		aRequestId = Integer.parseInt(new String(iResponse, aOffset, aLength));
		aOffset += (aLength + 1);
		
		// Request type
		aLength = getFieldLength(iResponse, aOffset);
		aTemp = (new String(iResponse, aOffset, aLength)).toLowerCase();
		aRequestType = globals.getRequestTypes().get(aTemp);
		aOffset += (aLength + 1);
		
		// Status
		aLength = getFieldLength(iResponse, aOffset);
		aStatus = Integer.parseInt(new String(iResponse, aOffset, aLength));
		aOffset += (aLength + 1);
		
		// Return value
		aLength = getFieldLength(iResponse, aOffset);
		aReturnValue = Integer.parseInt(new String(iResponse, aOffset, aLength));
		aOffset += (aLength + 1);
		
		// Display
		aLength = getFieldLength(iResponse, aOffset);
		aDisplay = new String(iResponse, aOffset, aLength);
		aOffset += (aLength + 1);
		
		// Error
		aLength = getFieldLength(iResponse, aOffset);
		aError = new String(iResponse, aOffset, aLength);
		aOffset += (aLength + 1);
		
		// Out
		aLength = getLastFieldLength(iResponse, aOffset);
		aOut = new String(iResponse, aOffset, aLength);
		
		returnOutput(0, aRequestId, aStatus, aRequestType, aReturnValue, aOut, iData, aDisplay, aError);
	}

	@Override
	public void onSocketError(int iStatus, String iError) {
	
		ARequestType aReqType = ARequestType.ConnectionError;
		int aXid = 0;
		
		if (!requests.isEmpty()) {
			aXid = requests.keys().nextElement();
			aReqType = requests.get(aXid).getRequestType();
		}
		
		returnOutput(0, aXid, AErrorCode.TcpIpError.ordinal(), 
				aReqType, 0, "", null, "", iError);
	}
	
	private int submitCheck(AReturnReceiver iReceiver, int iStatus, ARequestType iRequestType,
			String iAmpMsg) {
		
		if (iStatus > 0) {
			return submitError(iReceiver, iStatus, iRequestType, iAmpMsg);
		}
		
		return submit(iReceiver, iAmpMsg);
	}
	
	private int submit(AReturnReceiver iReceiver, String iAmpMsg) {
		
		return submit(iReceiver, iAmpMsg, "", 0, null, null);
	}
	
	private int submit(AReturnReceiver iReceiver, String iAmpMsg,
			String iClientData, int iClientValue,
			byte[] iData, Object iCustomData) {
		
		int aRetValue = 0;
		int aStatus = 0;
		String aOut = "";
		String aSpeechAct = "";
		int aXid = 0;
		ARequestType aRequestType = ARequestType.Unknown;
		
		if (!isConnected) {
			aStatus = AErrorCode.Disconnected.ordinal();
		} else {
			if (iAmpMsg.startsWith("_ais")) {
				int aBeg = iAmpMsg.indexOf(AAppSocketNoAsync.MSG_DELIM) + 1;
				int aEnd = iAmpMsg.indexOf(AAppSocketNoAsync.MSG_DELIM, aBeg);
				
				if (aEnd < 0) {
					aEnd = iAmpMsg.length();
				}
					
				aSpeechAct = iAmpMsg.substring(aBeg, aEnd);
				
				if (aSpeechAct.isEmpty()) {
					aSpeechAct = "noop";
				}
				
				aRequestType = globals.getRequestTypes().get(aSpeechAct);
				if (aRequestType == null) {
					aRequestType = ARequestType.Unknown;
				}
			} else {
				aRequestType = ARequestType.AmpMsg;
			}
				
			switch (aRequestType) {
			case AmpMsg:
            case AddUser:
            case CloseCabinet:
            case CloseContext:
            case CloseConnection:
            case CloseSession:
            case CompileLambda:
            case CompileCabinet:
            case ConnectSession:
            case Debug:
            case DeleteUser:
            case EnableConsoleLog:
            case EraseNode:
            case Eval:
            case Execute:
            case ExportNode:
            case ExportCabinet:
            case GetNextLevel:
            case GetConnectionStats:
            case GetConsoleLog:
            case GetContextId:
            case GetContextParams:
            case GetCurrentContexts:
            case GetDirInfo:
            case GetExeSession:
            case GetExtentNames:
            case GetExtentStatus:
            case GetExtentTypes:
            case GetLogonStats:
            case GetRequestStats:
            case GetSessions:
            case GetSessionStats:
            case GetSessionUser:
            case GetSubscriptions:
            case GetUsers:
            case GetWorkspaceStatistics:
            case ImportCabinet:
            case IsContextBusy:
            case IsContextOpen:
            case LogSysMsg:
            case Logoff:
            case Logon:
            case NewCabinet:
            case Noop:
            case OpenCabinet:
            case OpenNode:
            case OpenConsoleLog:
            case OpenContext:
            case OpenSession:
            case RegisterContext:
            case RetFile:
            case RunScriptFile:
            case SaveNode:
            case SetBreakpoint:
            case SetEngineFlags:
            case SetErrorTrace:
            case SetEscape:
            case SetInstructionTrace:
            case SetJit:
            case SetLogLvl:
            case SetRules:
            case SetSubscriptions:
            case SetSysCheck:
            case ShowConsole:
            case UpdateUser:
                break;

            default:
                // process as local request
                aStatus = -1;
                aRequestType = ARequestType.AmpMsg;

                if (aSpeechAct.matches("getcurrentnodenames")) {
                	aOut = AUtilities.joinStringArray(getCurrentNodeNames(), "\t");
                } else if (aSpeechAct.matches("getcurrentextentname")) {
                    aOut = currentExtent;
                } else if (aSpeechAct.matches("getcurrentextentnames")) {
                	aOut = AUtilities.joinStringArray(getCurrentExtents(), "\t");
                } else if (aSpeechAct == "getcurrentsessionid") {
                    aRetValue = sessionId;
                } else {
                    aOut = globals.getErrorMessages().get(AErrorCode.UnkAct);
                }
                break;
			}
		}
		
		if (aStatus > 0) {
			aXid = submitError(iReceiver, aStatus, aRequestType, iAmpMsg, iClientData, 0, iCustomData);
		} else {
			if (aRequestType != ARequestType.SetEscape) {
				aXid = addRequest(iAmpMsg, iClientData, iClientValue, iCustomData, iReceiver, aRequestType);
			}
			
			if (aStatus < 0) {
				returnOutput(0, aXid, 0, aRequestType, aRetValue, aOut, null, "", "");
			} else {
				String aRequest = aXid + "\177";
				appSocket.submit(aRequest + iAmpMsg, iData);
			}
		}

		return aXid;
	}
	
	private int submitError(AReturnReceiver iReceiver, int iStatus, 
			ARequestType iReqType, String iAmpMsg) {

		return submitError(iReceiver, iStatus, iReqType, iAmpMsg, "", 0, null);
	}
	
	private int submitError(AReturnReceiver iReceiver, int iStatus,
			ARequestType iReqType, String iAmpMsg,
			String iClientData, int iClientValue, Object iCustomData) {

		int aXid = 0;
		String aOut = (iStatus == 0) ? "true" : "";
		aXid = addRequest(iAmpMsg, iClientData, iClientValue, iCustomData, iReceiver, iReqType);
		AErrorCode aErrorCode = AErrorCode.values()[iStatus];	
		returnOutput(0, aXid, iStatus, iReqType, 0, aOut, null, "", 
				globals.getErrorMessages().get(aErrorCode));
		
		return aXid;
	}
	
	private void returnMsg(int iXid, int iStatus, ARequestType iReqType, int iRetValue,
			String iOut, byte[] iData, String iDisplay, String iError,
			String iClientData, boolean iClearQueueItem) {
	
		String aOut = iOut;
		String aClientData = null;
		AReturnReceiver aReceiver = defaultReceiver;
		int aStatus = 0;

		if (iXid > 0) {
			AClientRequest request = requests.get(iXid);
			if (request != null) {
				aClientData = request.getClientData();
				aReceiver = request.getReceiver();
				if (aReceiver == null) {
					aStatus = AErrorCode.NoClient.ordinal();
					aReceiver = defaultReceiver;
				}
				if (iClearQueueItem) {
					requests.remove(iXid);
				}
			} else {
				aStatus = AErrorCode.BadRequest.ordinal();
			}
		} else if (iStatus > 0) {
			// process errors with an unknown id, such as a timeout
			// return an error to all pending requests
			int aXid = 0;
			Enumeration<Integer> enumerator = requests.keys();
			while (enumerator.hasMoreElements()) {
				aXid = enumerator.nextElement();
				AClientRequest request = requests.remove(aXid);
				aClientData = request.getClientData();
				aReceiver = request.getReceiver();
				if (aReceiver == null) {
					aStatus = AErrorCode.NoClient.ordinal();
					aReceiver = defaultReceiver;
				} else {
					aStatus = iStatus;
				}
				aReceiver.returnOutput(getConnectionId(), aXid, aStatus,
						iReqType, iRetValue, iOut, iData, iDisplay, iError, aClientData);
			}
			
			// quit unless error not returned even once.
			if (aXid > 0) {
				return;
			}
		}
		
		if (aStatus > 0) {
			// return to default receiver
			aReceiver = defaultReceiver;
			aOut = "false";
		}
		
		if (aReceiver == null) {
			System.err.println("Null receiver");
		} else {
			
			// return output to client
			switch (iReqType) {
			case LogAll:
			case LogAmp:
			case LogConsole:
			case LogReqHdr:
			case LogSysMsg:
			case LogUserAccess:
				if (logReceiver != null) {
					logReceiver.returnLog(iReqType, aOut);
				}
				break;
			default:
				aReceiver.returnOutput(getConnectionId(), iXid, iStatus,
						iReqType, iRetValue, aOut, iData, iDisplay, iError, aClientData);
				break;
			}
		}
	}	
	
	private void returnOutput(int iConnectId, int iXid,
			int iStatus, ARequestType iReqType,
			int iRetValue, String iOut, byte[] iData,
			String iDisplay, String iError) {

		String aOut = iOut;
		boolean aClearQueueItem = true;
	
		if (iXid > 0) {
			if (!requests.containsKey(iXid) && iReqType != ARequestType.CloseConnection) {
				System.out.println("AAppClient::returnOutput(), Unexpected response Xid = " + iXid);
			}
		}
		
		if (iStatus > 0) {
			
		} else {
			switch (iReqType) {
			case GetContextParams:
				contextParameters = AUtilities.stringToStringDictionary(iOut);
				break;
			case AddUser:
			case AmpMsg:				// call to SMgr.submit
			case CheckCabinet:		// call to browseLib.checkStatus
			case CloseCabinet:		// call to (browseLib.dropExtent cabName cabPath)
			case CloseConnection:		// call to AisSvr.closeConnection
			case CloseContext:		// call SMgr.closeContext
			case CloseSession:		// call SMgr.closeSession
			case CompileLambda:		// call to browseLib.compileSource
			case CompileCabinet:		// call to (browseLib.compileAll true)
			case ConnectSession:		// call to SMgr.connectSession
			case Debug:				// call to SMgr.submit(ipRcvr, SBGLUE_DEBUGCMD)
			case DeleteUser:
			case EnableConsoleLog:	// call to SMgr.closeConsoleLog
			case EraseNode:			// call to browseLib.eraseSource
			case Eval:				// call to SMgr.submit
			case Execute:				// call to SMgr.submit
			case ExportNode:			// call to browseLib.exportSource 
			case ExportCabinet:		// call to browseLib.exportSource
			// case FcnError:			// expect iStatus > 0
			case FcnSendToClient:
			case FcnSendStatusToClient:
			case GetConnectionStats:
			case GetConsoleLog:		// call to SMgr.getConsoleLog
			case GetContextId:		// call to SMgr.getContextId
			case GetCurrentContexts:	// call to SMgr.getCurrentContexts
			case GetDirInfo:			// call to SMgr.submit(ipRcvr, SBGLUE_DIRINFO)
			case GetExeSession:		// call to SMgr.getExeSession
			case GetLogonStats:
			case GetMetaData:
			case GetRequestStats:
			case GetSessionId:		// call to AisClient.submit
			case GetSessions:			// call to SMgr.getSessions
			case GetSessionStats:
			case GetSessionUser:		// call to SMgr.getSessionUser
			case GetSubscriptions:	// call to SMgr.getSubscriptions
			case GetUsers:
			case ImportCabinet:		// call to browseLib.importSource
			case IsContextBusy:		// call to SMgr.isContextBusy
			case IsContextOpen:		// call to SMgr.isContextOpen
			case LogSysMsg:			// call to AisSvr.logSysmsg
			case NewCabinet:			// call to (browseLib.addExtent cabName cabPath)
			case Noop:				// call to AisMgr.submit
			case OpenNode:			// call to browseLib.checkout
			case OpenCabinet:			// call to (browseLib.addExtent cabName cabPath)
			// case OpenConnection:		// call to AisClient.openConnection (see below)
			case OpenConsoleLog:		// call to SMgr.openConsoleLog
			case OpenContext:			// call to SMgr.openContext
			//case OpenSession:		// call to AisSvr.openSession (see below)
			case RegisterDirectory:	// call to AppSvr.registerDirectory
			case RetFile:				// call to AppSvr.returnOutput
			case RetQuery:			// return from SMgr.event for "application/x-ampmsg" enctype
			case RetText:				// return from SMgr.event for "text" enctype
			case RetUrl:				// return from SMgr.event for "url" enctype
			case RunScriptFile:		// call to SMgr.submit(ipRcvr, "(runscript %s)")
			case SaveNode:			// call to browseLib.checkin
			case SetBreakpoint:		// call to SMgr.setBreakpoint
			case SetEngineFlags:		// call to SMgr.setEngineFlags
			case SetErrorTrace:		// call to SMgr.errorTrace
			case SetEscape:			// call to SMgr.setEscape
			case SetInstructionTrace:	// call to SMgr.setInstructionTrace
			case SetJit:				// call to SMgr.setJit
			case SetLogLvl:			// call to ALogMgr.setLogLvl
			case SetRules:			// call to AisSvr.setRules
			case SetSubscriptions:	// call to AisSvr.setSubscriptions
			case SetSysCheck:			// call to SMgr.setSysCheck
			case ShowConsole:			// call to SMgr.submit(ipRcvr, "(writeln (eval {%s}))")
			case UpdateMetaData:
			case UpdateUser:
				break;
			// Pushed output does not have a pending request
			case FcnEngineState:		// pushed output from engine
				engineFlags = iRetValue;
			case Display:
			case LogAll:
			case LogAmp:
			case LogConsole:
			case LogReqHdr:
			case LogUserAccess:
			case LogStatus:			// return from AisMgr.flushSessions
				aClearQueueItem = false;
				break;
			case FcnDebug:
				processDebug(iXid, iStatus, aOut, aClearQueueItem);
				return;
			case GetExtentNames:
				processGetExtentNames(aOut);
				break;
			case GetExtentStatus:
				processGetExtentStatus(aOut);
				break;
			case GetExtentTypes:
				processGetExtentTypes(aOut);
				break;
			case GetNextLevel:
				processGetNextLevel(aOut);
				break;
			case GetWorkspaceStatistics:
				aOut = "Workspace Statistics\n" + aOut;
				break;
			case Logoff:
				processLogoff(iStatus);
				break;
			case Logon:
				processLogon(iStatus, iRetValue);
				break;
			case OpenConnection:
				aOut = processOpenConnection(iStatus);
				break;
			case OpenSession:
				iRetValue = processOpenSession(iRetValue);
				break;
			case RegisterContext:
				processRegisterContext(aOut);
				break;
			default:
				System.out.println("AAppClient::returnOutput(), Unexpected response type");
				break;
			}
		}

		returnMsg(iXid, iStatus, iReqType, iRetValue, aOut, iData,
				iDisplay, iError, null, aClearQueueItem);

		if (iStatus <= 0) {
			switch (iReqType) {
			case CloseSession:
				if (sessionId == iRetValue) {
					sessionId = 0;
				}
				break;
			case ConnectSession:
				sessionId = iRetValue; 
				break;
			default:
				break;
			}
		}
	}
	
	private void processGetExtentStatus(String iOut) {

		String[] aExtentStatusList = iOut.split("\n");
		
		int aCount = aExtentStatusList.length;
		extents.clear();
		if(aCount <= 0)
		{
			currentExtent = "";
		}
		else
		{
			extents.clear();
			for (String aExtentStatusItem : aExtentStatusList) {
				if (!aExtentStatusItem.isEmpty()) {
					String[] aExtentStatus = aExtentStatusItem.split("\t");
					
					if (aExtentStatus.length == 6) {
						//String aAction = aExtentStatus[0];
						String aName = aExtentStatus[4];
						extents.put(aName, new AExtentInfo());
					}					
				}
			}
		}	
	}

	private void processRegisterContext(String iOut) {
		
		String[] aPair = iOut.split("" + AAppSocketNoAsync.MSG_DELIM);
		String aContext = aPair[0];
		String aMessages = aPair[1];
		System.out.println("aContext = " + aContext + ", aMessages = " + aMessages);
	}

	private int processOpenSession(int iRetValue) {

		if (iRetValue > 0) {
			sessionId = iRetValue;
		} else {
			iRetValue = sessionId;
		}
		return iRetValue;
	}

	private String processOpenConnection(int iStatus) {

		String aRet = "";
		if (iStatus > 0) {
			aRet = "Unable to connect to server";
		} else {
			isConnected = true;
		}
		return aRet;
	}

	private void processLogon(int iStatus, int iRetValue) {

		if (iStatus < 1 && iRetValue > 0) {
			userId = iRetValue;
		} else {
			userId = 0;
		}
	}

	private void processLogoff(int iStatus) {

		if (iStatus < 1) {
			userId = 0;
		}
	}

	private void processGetExtentTypes(String iOut) {

		if (!AUtilities.isError(iOut) && extents.containsKey(currentExtent)) {
			String[] aTypes = iOut.split("\n");
			AExtentInfo aExtentInfo = extents.get(currentExtent);
			
			for (int i = 0; i < aTypes.length; i++) {
				String[] aFields = aTypes[i].split("\t");
				if (aFields.length > 1) {
					aExtentInfo.getExtentTypes().put(aFields[0], 
							new AExtentTypeInfo(aFields[0], aFields[1]));
				}
			}
		}
	}

	private void processGetNextLevel(String iOut) {
		
		AExtentInfo aExtentInfo = extents.get(currentExtent);
		if (aExtentInfo != null) {
			
			String[] aLines = iOut.split("\n");
			String[] aFields = null;
			
			if (aLines.length > 1) {
				// Lines. aLines[0] contains the following fields which we do not currently use:
				// aField[0] == starting number
				// aField[1] == number of lines transferred
				// aField[2] == total lines available on server
				
				aExtentInfo.getNodes().clear();
				for (int x = 1; x < aLines.length; x++) {
					aFields = aLines[x].split("\t");
					if (aFields[0].matches("Current")) {
						aExtentInfo.setNodePath(aFields[1]);
					} else if (aFields.length > 7) {
						aExtentInfo.getNodes().add(new ANodeInfo(aFields));
					}
				}				
			}
		}
	}

	private void processGetExtentNames(String iOut) {
		
		String[] aExtentNames = iOut.split("\t");
		
		if (aExtentNames == null || Arrays.asList(aExtentNames).contains("#void")) {
			extents.clear();
			currentExtent = "";
		} else {
			extents.clear();
			for (int i = 0; i < aExtentNames.length; i++) {
				extents.put(aExtentNames[i], new AExtentInfo());
			}
		}
	}
	
	private void processDebug(int iXid, int iStatus, String iOut, boolean iClearQueueItem) {
		// TODO Auto-generated method stub
	}

	private int getConnectionId() {
		return 0;
	}
	
	private synchronized int addRequest(String iAmpMsg, String iClientData,
			int iClientValue, Object iCustomData,
			AReturnReceiver iReceiver, ARequestType iReqType) {

		int aXid = ++xId;
		requests.put(aXid, new AClientRequest(iAmpMsg, iClientData,
				iClientValue, iCustomData, iReceiver, iReqType));
		
		return aXid;
	}
	
	private void onConnectionClosed() {
		
		if (isConnected && defaultReceiver != null) {
			if (closeConnXid != 0) {
				returnMsg(closeConnXid, 0, ARequestType.CloseConnection, 
						0, "true", null, "", "", "", true);
			} else {
				String aAmpMsg = String.format(AAppMessage.ConnectionClosed);
				submitError(defaultReceiver, AErrorCode.LostConnection.ordinal(),
						ARequestType.CloseConnection, aAmpMsg, name, 0, null);
			}
		}
		
		requests.clear();
		contextParameters.clear();
		extents.clear();
		extentTypeOptions.clear();
		
		isConnected = false;
		sessionId = 0;
		xId = 0;
		openConnXid = 0;
		closeConnXid = 0;
	}
	
	private int getFieldLength(byte[] iBuffer, int iOffset) {
		
		return getFieldLength(iBuffer, iOffset, AAppSocketNoAsync.MSG_DELIM);
	}
	
	private int getLastFieldLength(byte[] iBuffer, int iOffset) {
		
		return getFieldLength(iBuffer, iOffset, '\0');
	}
	
	private int getFieldLength(byte[] iBuffer, int iOffset, char iDelim) {
		
		int aLength = 0;
		
		while (iOffset < iBuffer.length && iBuffer[iOffset] != iDelim) {
			aLength++;
			iOffset++;
		}
		
		return aLength;
	}
	
	private String name;
	private String host;
	private int port;
	private AReturnReceiver defaultReceiver;
	private ALogReceiver logReceiver;
	
	private AAppSocketNoAsync appSocket;
	private Hashtable<Integer, AClientRequest> requests;
	private int sessionId;
	private boolean isConnected = false;
	private int userId = 0;
	private String currentExtent;
	
	private int xId = 0;
	private int engineFlags = 0;
	private int openConnXid = 0;
	private int closeConnXid = 0;

	private Hashtable<String, AExtentInfo> extents;
	private Hashtable<String, String> extentTypeOptions;
	private Hashtable<String, String> contextParameters;
	private AGlobals globals = AGlobals.getInstance();
	
	private static int ENGINE_ERROR_TRACE = 0x01;
	private static int ENGINE_INSTRUCTION_TRACE = 0x02;
	private static int ENGINE_SYSCHECKON = 0x04;
	private static int ENGINE_JITON = 0x10;
}
