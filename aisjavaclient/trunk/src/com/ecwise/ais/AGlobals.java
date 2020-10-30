package com.ecwise.ais;

import java.util.Hashtable;

public class AGlobals {
	
	private static AGlobals singleton = null;
	private Hashtable<AErrorCode, String> errorMessages;
	
	private Hashtable<ARequestType, String> requestNames;
	private Hashtable<String, ARequestType> requestTypes;
	
	private Hashtable<ACloseMode, String> closeModes;
	
	public static AGlobals getInstance() {
		if (singleton == null) {
			singleton = new AGlobals();
		}
		return singleton;
	}
	
	public AGlobals() {
		initErrorMessages();
		initRequestTypes();
		initCloseModes();
	}
	
	public Hashtable<AErrorCode, String> getErrorMessages() {
		return errorMessages;
	}
	
	public Hashtable<ARequestType, String> getRequestNames() {
		return requestNames;
	}
	
	public Hashtable<String, ARequestType> getRequestTypes() {
		return requestTypes;
	}
	
	public String getCloseModeFromInt(ACloseMode iCloseMode) {
		return closeModes.get(iCloseMode);
	}
	
	private void initCloseModes() {

		ACloseMode[] aCloseModes = ACloseMode.values();
		String aKey;
		
		closeModes = new Hashtable<ACloseMode, String>();
		
		for (int i = 0; i < aCloseModes.length; i++) {
			aKey = aCloseModes[i].name().toLowerCase();
			closeModes.put(aCloseModes[i], aKey);
		}
	}
	
	private void initErrorMessages() {
		errorMessages = new Hashtable<AErrorCode, String>();
		errorMessages.put(AErrorCode.Empty, "");
        errorMessages.put(AErrorCode.Generic, "Unspecified error");
        errorMessages.put(AErrorCode.EvalFail, "!evalFailure!");
        errorMessages.put(AErrorCode.FileRead, "!errReadIO!");
        errorMessages.put(AErrorCode.FileWrite, "!errWriteIO!");
        errorMessages.put(AErrorCode.OutMemory, "!outOfMemory!");
        errorMessages.put(AErrorCode.FrameOvflw, "!gcFrameOverflw!");
        errorMessages.put(AErrorCode.DamagedMem, "!damageMemoryHD!");
        errorMessages.put(AErrorCode.StackOvflw, "!stackOverflw!");
        errorMessages.put(AErrorCode.UsrEsc, "!userEscape!");
        errorMessages.put(AErrorCode.Pcode, "!invalidPcode!");
        errorMessages.put(AErrorCode.DamagedObject, "!damagedObject!");
        errorMessages.put(AErrorCode.RecursionLimit, "!recursionLimit!");
        errorMessages.put(AErrorCode.FrameRelease, "!gcFrameRelease!");
        errorMessages.put(AErrorCode.StackRelease, "!stackRelease!");
        errorMessages.put(AErrorCode.RecursionRel, "!recursionRelease!");
        errorMessages.put(AErrorCode.UsrQuit, "!userQuit!");
        errorMessages.put(AErrorCode.FileVersion, "!diskFileVersion!");
        errorMessages.put(AErrorCode.EngineBusy, "!engineBusy!");
        errorMessages.put(AErrorCode.Repository, "!repositoryGC!");
        errorMessages.put(AErrorCode.Unexpected, "!unexpectedError!");
        errorMessages.put(AErrorCode.NotImplemented, "!notImplemented!");
        errorMessages.put(AErrorCode.System, "!systemError!");
        errorMessages.put(AErrorCode.Access, "!access denied!");
        errorMessages.put(AErrorCode.CheckIn, "!bad checkin argument!");
        errorMessages.put(AErrorCode.Filter, "!bad filter argument!");
        errorMessages.put(AErrorCode.Score, "!bad score argument!");
        errorMessages.put(AErrorCode.SendToClient, "!sendToClientFailure!");
        errorMessages.put(AErrorCode.UnkContextName, "Unknown or missing context name");
        errorMessages.put(AErrorCode.ContextIsOpen, "Context already open");
        errorMessages.put(AErrorCode.ContextOpen, "Open context fails");
        errorMessages.put(AErrorCode.InactiveSession, "Session not active");
        errorMessages.put(AErrorCode.SessionId, "Invalid session ID");
        errorMessages.put(AErrorCode.ReqPending, "Pending request has not completed");
        errorMessages.put(AErrorCode.SessionClosed, "Session is closed. Request denied.");
        errorMessages.put(AErrorCode.Security, "Access denined");
        errorMessages.put(AErrorCode.SessionOpen, "Session is already open");
        errorMessages.put(AErrorCode.InactiveContext, "Context not active");
        errorMessages.put(AErrorCode.LoggedOn, "User already logged on");
        errorMessages.put(AErrorCode.UnkUsrName, "Unknown or missing user name");
        errorMessages.put(AErrorCode.BadUsrId, "Invalid user ID");
        errorMessages.put(AErrorCode.UsrMismatch, "Usr Name mismatch");
        errorMessages.put(AErrorCode.AcctDisabled, "Account disabled");
        errorMessages.put(AErrorCode.BadPasswd, "Invalid user name or password");
        errorMessages.put(AErrorCode.AcctExpired, "Account has expired");
        errorMessages.put(AErrorCode.NoLogon, "No one logged on");
        errorMessages.put(AErrorCode.ParseAmp, "Unable to parse AMP input");
        errorMessages.put(AErrorCode.FewArgs, "Too few input args");
        errorMessages.put(AErrorCode.UnkAct, "Unsupported request");
        errorMessages.put(AErrorCode.Disconnected, "Not currently connected");
        errorMessages.put(AErrorCode.EmptyArg, "Empty input arg");
        errorMessages.put(AErrorCode.NoExtent, "No extent selected");
        errorMessages.put(AErrorCode.NoContext, "No context established for this connection");
        errorMessages.put(AErrorCode.NoPair, "Expected a name-value pair following speech act");
        errorMessages.put(AErrorCode.OutOfRange, "Error code out of range");
        errorMessages.put(AErrorCode.UnkConnectId, "Unknown connection");
        errorMessages.put(AErrorCode.UnkMethod, "Unknown HTTP method");
        errorMessages.put(AErrorCode.NoCookie, "No cookie returned by client");
        errorMessages.put(AErrorCode.NoArg, "Missing required argument");
        errorMessages.put(AErrorCode.BfrOvflw, "Input buffer overflow");
        errorMessages.put(AErrorCode.BadHttp, "Garbled HTTP request");
        errorMessages.put(AErrorCode.BadXml, "Garbled XML request");
        errorMessages.put(AErrorCode.BadHttpReq, "Incomplete HTTP request");
        errorMessages.put(AErrorCode.CookieName, "Unknown cookie name (expected abaseid)");
        errorMessages.put(AErrorCode.Timeout, "Wait period expired, operation aborted");
        errorMessages.put(AErrorCode.IpMismatch, "Client IP address does not match original");
        errorMessages.put(AErrorCode.ConnectId, "Invalid connection ID");
        errorMessages.put(AErrorCode.LostConnection, "Connection closed. Unable to return response");
        errorMessages.put(AErrorCode.NoServer, "No protocol server for this connection");
        errorMessages.put(AErrorCode.NoClient, "Unable to locate module that initiated this request");
        errorMessages.put(AErrorCode.ContextId, "Invalid context ID");
        errorMessages.put(AErrorCode.LostCookie, "Once valid cookie missing or invalid");
        errorMessages.put(AErrorCode.BadAmp, "Ill-formed AMP msg");
        errorMessages.put(AErrorCode.BadRequest, "Unexpected request returned");
        errorMessages.put(AErrorCode.Connected, "Already connected");
        errorMessages.put(AErrorCode.UnkHost, "Unknown DNS name, IP address or port");
        errorMessages.put(AErrorCode.NotRegistered, "Context is not registered");
        errorMessages.put(AErrorCode.BadCabinet, "Invalid or missing cabinet specification");
        errorMessages.put(AErrorCode.BadNode, "Invalid or missing node specification");
        errorMessages.put(AErrorCode.BadAgent, "Invalid or missing agent name");
        errorMessages.put(AErrorCode.NoInput, "Missing or invalid input");
        errorMessages.put(AErrorCode.TcpIpError, "TCP/IP Socket error");
        errorMessages.put(AErrorCode.AtBeg, "Already at beginning");
        errorMessages.put(AErrorCode.AtEnd, "Already at end");
        errorMessages.put(AErrorCode.CabinetOpen, "Cabinet is already open");
        errorMessages.put(AErrorCode.BadFilename, "Invalid or missing file specification");
        errorMessages.put(AErrorCode.InProcess, "Operation not supported on in-process client");
        errorMessages.put(AErrorCode.ServerOpen, "Server connection already established");
        errorMessages.put(AErrorCode.BadContext, "Request not supported on this context");
        errorMessages.put(AErrorCode.NoConsoleLog, "No console output buffer enabled");
        errorMessages.put(AErrorCode.BfrTruncate, "Truncate buffer fails");
        errorMessages.put(AErrorCode.BfrSeek, "Move position in buffer fails");
        errorMessages.put(AErrorCode.BadArg, "Argument value is out-of-range");
        errorMessages.put(AErrorCode.PortInUse, "Port is in-use by another protocol");
        errorMessages.put(AErrorCode.CookieIp, "Client's IP address and cookie's IP do not match");
        errorMessages.put(AErrorCode.StaleCookie, "Cookie key is outdated or corrupted");
        errorMessages.put(AErrorCode.CookieMismatch, "Cookie key does not match key for this socket");
        errorMessages.put(AErrorCode.SystemContext, "Unsupported operation on _SystemContext");
        errorMessages.put(AErrorCode.UnkProtocol, "Unexpected protocol on this port");
        errorMessages.put(AErrorCode.Unterminated, "Unterminated message");
        errorMessages.put(AErrorCode.UsrNameExists, "Username already exists");
        errorMessages.put(AErrorCode.MaxUsrReached, "Maximum no. of users reached");
        errorMessages.put(AErrorCode.BadSecLvl, "Invalid security level");
        errorMessages.put(AErrorCode.PwdOpenFail, "Failed in opening of password file");
        errorMessages.put(AErrorCode.PwdWriteFail, "Failed in writing to password file");
	}
	
	private void initRequestTypes() {
		ARequestType[] aRequestTypes = ARequestType.values();
		String aKey;
		
		requestNames = new Hashtable<ARequestType, String>();
		requestTypes = new Hashtable<String, ARequestType>();
		
		for (int i = 0; i < aRequestTypes.length; i++) {
			aKey = aRequestTypes[i].name().toLowerCase();
			requestNames.put(aRequestTypes[i], aKey);
			requestTypes.put(aKey, aRequestTypes[i]);
		}
	}
}
