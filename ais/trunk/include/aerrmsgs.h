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
aisdev/include/aerrmsgs.h
														Error Definitions

CHANGE HISTORY
Version	Date		Who		Description
3.2001	 1/27/2008	fchua	Added AERR_USRNAMEEXISTS, AERR_MAXUSRREACHED, AERR_BADSECLVL, AERR_PWDOPENFAIL, AERR_PWDWRITEFAIL.
1.0104	 9/8/2006	tlw		Add UNKPROTOCOL, UNTERMINATED
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
NOTES
The error messages are listed below.  Include this file just once in the place where the global
error messages are initialized, such as in AGlobals::init().  The defined constants used here
are defined in aerrdefs.h.  The two files must remain in sync.  Add an entry in both places or
delete an entry in both places. 
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------

// AERR Messages
	mpErrMsgs[AERR_EMPTY]			= "";
	mpErrMsgs[AERR_GENERIC]			= "Unspecified error";
	mpErrMsgs[AERR_EVALFAIL]		= "!evalFailure!";
	mpErrMsgs[AERR_FILEREAD]		= "!errReadIO!";
	mpErrMsgs[AERR_FILEWRITE]		= "!errWriteIO!";
	mpErrMsgs[AERR_OUTMEMORY]		= "!outOfMemory!";
	mpErrMsgs[AERR_FRAMEOVFLW]		= "!gcFrameOverflw!";
	mpErrMsgs[AERR_DAMAGEDMEM]		= "!damagedMemoryHD!";
	mpErrMsgs[AERR_STACKOVFLW]		= "!stackOverflw!";
	mpErrMsgs[AERR_USRESC]			= "!userEscape!";
	mpErrMsgs[AERR_PCODE]			= "!invalidPcode!";
	mpErrMsgs[AERR_DAMAGEDOBJECT]	= "!damagedObject!";
	mpErrMsgs[AERR_RECURSIONLIMIT]	= "!recursionLimit!";
	mpErrMsgs[AERR_FRAMERELEASE]	= "!gcFrameRelease!";
	mpErrMsgs[AERR_STACKRELEASE]	= "!stackRelease!";
	mpErrMsgs[AERR_RECURSIONREL]	= "!recursionRelease!";
	mpErrMsgs[AERR_USRQUIT]			= "!userQuit!";
	mpErrMsgs[AERR_FILEVERSION]		= "!diskFileVersion!";
	mpErrMsgs[AERR_ENGINEBUSY]		= "!engineBusy!";
	mpErrMsgs[AERR_REPOSITORY]		= "!repositoryGC!";
	mpErrMsgs[AERR_UNEXPECTED]		= "!unexpectedError!";
	mpErrMsgs[AERR_NOTIMPLEMENTED]	= "!notImplemented!";
	mpErrMsgs[AERR_SYSTEM]			= "!systemError!";
	mpErrMsgs[AERR_ACCESS]			= "!access denied!";
	mpErrMsgs[AERR_CHECKIN]			= "!bad checkin argument!";
	mpErrMsgs[AERR_FILTER]			= "!bad filter argument!";
	mpErrMsgs[AERR_SCORE]			= "!bad score argument!";
	mpErrMsgs[AERR_SENDTOCLIENT]	= "!sendToClientFailure!";
	mpErrMsgs[AERR_UNKCONTEXTNAME]	= "Unknown or missing context name";
	mpErrMsgs[AERR_CONTEXTISOPEN]	= "Context already open";
	mpErrMsgs[AERR_CONTEXTOPEN]		= "Open context fails";
	mpErrMsgs[AERR_INACTIVESESSION]	= "Session not active";
	mpErrMsgs[AERR_SESSIONID]		= "Invalid session ID";
	mpErrMsgs[AERR_REQPENDING]		= "Pending request has not completed";
	mpErrMsgs[AERR_SESSIONCLOSED]	= "Session is closed. Request denied.";
	mpErrMsgs[AERR_SECURITY]		= "Access denied";
	mpErrMsgs[AERR_SESSIONOPEN]		= "Session is already open";
	mpErrMsgs[AERR_INACTIVECONTEXT]	= "Context not active";
	mpErrMsgs[AERR_LOGGEDON]		= "User already logged on";
	mpErrMsgs[AERR_UNKUSRNAME]		= "Unknown or missing user name";
	mpErrMsgs[AERR_BADUSRID]		= "Invalid user ID";
	mpErrMsgs[AERR_USRMISMATCH]		= "Usr Name mismatch";
	mpErrMsgs[AERR_ACCTDISABLED]	= "Account disabled";
	mpErrMsgs[AERR_BADPASSWD]		= "Invalid user name or password";
	mpErrMsgs[AERR_ACCTEXPIRED]		= "Account has expired";
	mpErrMsgs[AERR_NOLOGON]			= "No one logged on";
	mpErrMsgs[AERR_PARSEAMP]		= "Unable to parse AMP input";
	mpErrMsgs[AERR_FEWARGS]			= "Too few input args";
	mpErrMsgs[AERR_UNKACT]			= "Unsupported request";
	mpErrMsgs[AERR_DISCONNECTED]	= "Not currently connected";
	mpErrMsgs[AERR_EMPTYARG]		= "Empty input arg";
	mpErrMsgs[AERR_NOEXTENT]		= "No extent selected";
	mpErrMsgs[AERR_NOCONTEXT]		= "No Context established for this connection.";
	mpErrMsgs[AERR_NOPAIR]			= "Expected a name-value pair following speech act";
	mpErrMsgs[AERR_OUTOFRANGE]		= "Error code out of range";
	mpErrMsgs[AERR_UNKCONNECTID]	= "Unknown connection";
	mpErrMsgs[AERR_UNKMETHOD]		= "Unknown HTTP method";
	mpErrMsgs[AERR_NOCOOKIE]		= "No cookie returned by client";
	mpErrMsgs[AERR_NOARG]			= "Missing required argument";
	mpErrMsgs[AERR_BFROVFLW]		= "Input buffer overflow";
	mpErrMsgs[AERR_BADHTTP]			= "Garbled HTTP request";
	mpErrMsgs[AERR_BADXML]			= "Garbled XML request";
	mpErrMsgs[AERR_BADHTTPREQ]		= "Incomplete HTTP request";
	mpErrMsgs[AERR_COOKIENAME]		= "Unknown cookie name (expected abaseid)";
	mpErrMsgs[AERR_TIMEOUT]			= "Wait period expired, operation aborted";
	mpErrMsgs[AERR_IPMISMATCH]		= "Client IP address does not match original";
	mpErrMsgs[AERR_CONNECTID]		= "Invalid connection ID";
	mpErrMsgs[AERR_LOSTCONNECTION]	= "Connection closed. Unable to return response";
	mpErrMsgs[AERR_NOSERVER]		= "No protocol server for this connection";
	mpErrMsgs[AERR_NOCLIENT]		= "Unable to locate module that initiated this request";
	mpErrMsgs[AERR_CONTEXTID]		= "Invalid context ID";
	mpErrMsgs[AERR_LOSTCOOKIE]		= "Once valid cookie missing or invalid";
	mpErrMsgs[AERR_BADAMP]			= "Ill-formed AMP msg";
	mpErrMsgs[AERR_BADREQUEST]		= "Unexpected request returned";
	mpErrMsgs[AERR_CONNECTED]		= "Already connected";
	mpErrMsgs[AERR_UNKHOST]			= "Unknown DNS name, IP address or port";
	mpErrMsgs[AERR_NOTREGISTERED]	= "Context is not registered";
	mpErrMsgs[AERR_BADCABINET]		= "Invalid or missing cabinet specification";
	mpErrMsgs[AERR_BADNODE]			= "Invalid or missing node specification";
	mpErrMsgs[AERR_BADLambda]		= "Invalid or missing Lambda name";
	mpErrMsgs[AERR_NOINPUT]			= "Missing or invalid input";
	mpErrMsgs[AERR_TCPIPERROR]		= "TCP/IP Socket error";
	mpErrMsgs[AERR_ATBEG]			= "Already at beginning";
	mpErrMsgs[AERR_ATEND]			= "Already at end";
	mpErrMsgs[AERR_CABINETOPEN]		= "Cabinet is already open";
	mpErrMsgs[AERR_BADFILENAME]		= "Invalid or missing file specification";
	mpErrMsgs[AERR_INPROCESS]		= "Operation not supported on in-process client";
	mpErrMsgs[AERR_SERVEROPEN]		= "Server connection already established";
	mpErrMsgs[AERR_BADCONTEXT]		= "Request not supported on this context";
	mpErrMsgs[AERR_NOCONSOLELOG]	= "No console output buffer enabled";
	mpErrMsgs[AERR_BFRTRUNCATE]		= "Truncate buffer fails";
	mpErrMsgs[AERR_BFRSEEK]			= "Move position in buffer fails";
	mpErrMsgs[AERR_BADARG]			= "Argument value is out-of-range";
	mpErrMsgs[AERR_PORTINUSE]		= "Port is in-use by another protocol";
	mpErrMsgs[AERR_COOKIEIP]		= "Client's IP address and cookie's IP do not match";
	mpErrMsgs[AERR_STALECOOKIE]		= "Cookie key is outdated or corrupted";
	mpErrMsgs[AERR_COOKIEMISMATCH]	= "Cookie key does not match key for this socket";
	mpErrMsgs[AERR_SYSTEMCONTEXT]	= "Unsupported operation on _SystemContext";
	mpErrMsgs[AERR_UNKPROTOCOL]		= "Unexpected protocol on this port";
	mpErrMsgs[AERR_UNTERMINATED]	= "Unterminated message";
	mpErrMsgs[AERR_USRNAMEEXISTS]	= "Username already exists";
	mpErrMsgs[AERR_MAXUSRREACHED]	= "Maximum no. of users reached";
	mpErrMsgs[AERR_BADSECLVL]		= "Invalid security level";
	mpErrMsgs[AERR_PWDOPENFAIL]		= "Failed in opening of password file";
	mpErrMsgs[AERR_PWDWRITEFAIL]	= "Failed in writing to password file";

// aerrmsgs.h

