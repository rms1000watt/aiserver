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

#ifndef AERRDEFS_H
#define AERRDEFS_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/aerrdefs.h
														Error Definitions

CHANGE HISTORY
Version	Date		Who		Change
3.2001	 1/27/2008	fchua	Added AERR_USRNAMEEXISTS, AERR_MAXUSRREACHED, AERR_BADSECLVL, AERR_PWDOPENFAIL, AERR_PWDWRITEFAIL.
1.0104	 9/8/2006	tlw		Add AERR_UNKPROTOCOL, UNTERMINATED
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
NOTES
All error messages sent to clients are defined here. The error strings are defined in
errmsgs.h These files must be kept in sync.  Add definitions to both files and delete
definitions from both files.  The numeric values should be kept in sequence.  The values
of the definitions must be unique.

Try to keep messages alphabetic by the defined constant.  The error definitions from
SbGlue and SessionManager are listed first here.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	----------------------------------------------------- DEFINITIONS ---------------------------------------------------------
// AERR Definitions
#define AERR_EMPTY			  0		// ""
#define AERR_GENERIC		  1		// Unspecified error
#define AERR_EVALFAIL		  2		// !evalFailure!	- SBGLUE_EVAL_FAILURE (-100)
#define AERR_FILEREAD		  3		// !errReadIO!		- SBGLUE_ERR_FILE_READ
#define AERR_FILEWRITE		  4		// !errWriteIO!		- SBGLUE_ERR_FILE_WRITE
#define AERR_OUTMEMORY		  5		// !outOfMemory!	- SBGLUE_ERR_OUT_OF_MEMORY
#define AERR_FRAMEOVFLW		  6		// !gcFrameOverflw!	- SBGLUE_ERR_FRAME_ERROR
#define AERR_DAMAGEDMEM		  7		// !damagedMemoryHD!- SBGLUE_ERR_INVALID
#define AERR_STACKOVFLW		  8		// !stackOverflw!	- SBGLUE_ERR_STACK
#define AERR_USRESC			  9		// !userEscape!		- SBGLUE_ERR_ESCAPE
#define AERR_PCODE			 10		// !invalidPcode!	- SBGLUE_ERR_PCODE
#define AERR_DAMAGEDOBJECT	 11		// !damagedObject!	- SBGLUE_ERR_BAD_DATATYPE
#define AERR_RECURSIONLIMIT	 12		// !recursionLimit!	- SBGLUE_ERR_RECURSION
#define AERR_FRAMERELEASE	 13		// !gcFrameRelease!	- SBGLUE_ERR_FRAME_RELEASE
#define AERR_STACKRELEASE	 14		// !stackRelease!"	- SBGLUE_ERR_STACK_RELEASE
#define AERR_RECURSIONREL	 15		// !recursionRelease!- SBGLUE_ERR_RECURSION_RELEASE
#define AERR_USRQUIT		 16		// !userQuit!		- SBGLUE_ERR_QUIT
#define AERR_FILEVERSION	 17		// !diskFileVersion!- SBGLUE_ERR_WRONG_VERSION
#define AERR_ENGINEBUSY		 18		// !engineBusy!		- SBGLUE_ERR_ENGINE_BUSY
#define AERR_REPOSITORY		 19		// !repositoryGC!	- SBGLUE_ERR_REPOSITORY_GC
#define AERR_UNEXPECTED		 20		// !unexpectedError!- SGBLUE_UNEXPECTED_ERROR
#define AERR_NOTIMPLEMENTED	 21		// !notImplemented!	- SBGLUE_ERR_NOT_IMPLEMENTED
#define AERR_SYSTEM			 22		// !systemError!	- SBGLUE_ERR_SYSTEM_ERROR
#define AERR_ACCESS			 23		// !access denied!	- SBGLUE_ERR_EVAL_SECURITY
#define AERR_CHECKIN		 24		// !bad checkin argument!- SBGLUE_ERR_EVAL_BAD_CHECKIN
#define AERR_FILTER			 25		// !bad filter argument!- SBGLUE_ERR_EVAL_BAD_FILTER
#define AERR_SCORE			 26		// !bad score argument!	- SBGLUE_ERR_EVAL_BAD_SCORE
#define AERR_SENDTOCLIENT	 27		// !sendToClientFailure!- SBGLUE_SENDTOCLIENT_FAILURE(-125)
#define AERR_UNKCONTEXTNAME	 28		// Unknown or missing context name	- AUNKNOWN_CONTEXTNAME (-200)
#define AERR_CONTEXTISOPEN	 29		// Context already open - ACONTEXTNAME_ALREADY_OPEN
#define AERR_CONTEXTOPEN	 30		// Context open fails	- ACONTEXT_OPEN_FAILED
#define AERR_INACTIVESESSION 31		// Session not active	- ASESSION_NOT_ACTIVE
#define AERR_SESSIONID		 32		// Invalid session ID	- ABAD_SESSIONID
#define AERR_REQPENDING		 33		// Pending request has not completed - ASESSION_REQUESTS_PENDING
#define AERR_SESSIONCLOSED	 34		// Session is closed. Request denied.	- ASESSION_CLOSED
#define	AERR_SECURITY		 35		// Access denied		- ASECURITY_VIOLATION
#define AERR_SESSIONOPEN	 36		// Session is already open
#define AERR_INACTIVECONTEXT 37		// Context not active	- ACONTEXT_NOT_ACTIVE (-209)

#define AERR_LOGGEDON		 38		// User already logged on
#define AERR_UNKUSRNAME		 39		// Unknown or missing user name
#define AERR_BADUSRID		 40		// Invalid user ID
#define AERR_USRMISMATCH	 41		// Usr Name mismatch
#define AERR_ACCTDISABLED	 42		// Account disabled
#define AERR_BADPASSWD		 43		// Invalid password
#define AERR_ACCTEXPIRED	 44		// Account has expired
#define AERR_NOLOGON		 45		// No one logged on
#define AERR_PARSEAMP		 46		// Unable to parse AMP input
#define AERR_FEWARGS		 47		// Too few input args
#define AERR_UNKACT			 48		// Unsupported request
#define AERR_DISCONNECTED	 49		// Not currently connected
#define AERR_EMPTYARG		 50		// Empty input arg
#define AERR_NOEXTENT		 51		// No extent selected
#define AERR_NOCONTEXT		 52		// No context established for this connection.
#define AERR_NOPAIR			 53		// Expected a name-value pair following speech act.
#define AERR_OUTOFRANGE		 54		// Error code out of range
#define AERR_UNKCONNECTID	 55		// Unknown connection
#define AERR_UNKMETHOD		 56		// Unknown HTTP method 
#define AERR_NOCOOKIE		 57		// No cookie returned by client
#define AERR_NOARG			 58		// Missing required argument
#define AERR_BFROVFLW		 59		// Input buffer overflow
#define AERR_BADHTTP		 60		// Garbled HTTP request
#define AERR_BADXML			 61		// Garbled XML request
#define AERR_BADHTTPREQ		 62		// Incomplete HTTP request
#define AERR_COOKIENAME		 63		// Unknown cookie format (abaseid=XXXXX)
#define AERR_TIMEOUT		 64		// Wait period expired. Operation aborted.
#define AERR_IPMISMATCH		 65		// Client IP address does not match original
#define AERR_CONNECTID		 66		// Invalid connect ID
#define AERR_LOSTCONNECTION	 67		// Connection closed. Unable to return response.
#define AERR_NOSERVER		 68		// No protocol server for this connection
#define AERR_NOCLIENT		 69		// Unable to locate module that initiated this request
#define AERR_CONTEXTID		 70		// Invalid context ID
#define AERR_LOSTCOOKIE		 71		// Once valid cookie missing or invalid
#define AERR_BADAMP			 72		// Ill-formed AMP msg
#define AERR_BADREQUEST		 73		// Unexpected request
#define AERR_CONNECTED		 74		// Already connected
#define AERR_UNKHOST		 75		// Unknown DNS name, IP address or port
#define AERR_NOTREGISTERED	 76		// Context is not registered
#define AERR_BADCABINET		 77		// Invalid or missing cabinet specification
#define AERR_BADNODE		 78		// Invalid or missing node specification
#define AERR_BADLambda		 79		// Invalid or missing Lambda name
#define AERR_NOINPUT		 80		// Missing or invalid input
#define AERR_TCPIPERROR		 81		// TCP/IP Socket error
#define AERR_ATBEG			 82		// Already at beginning
#define AERR_ATEND			 83		// Already at end
#define AERR_CABINETOPEN	 84		// Cabinet is already open
#define AERR_BADFILENAME	 85		// Invalid or missing file specification
#define AERR_INPROCESS		 86		// Operation not supported on in-process client
#define AERR_SERVEROPEN		 87		// Server connection already established
#define AERR_BADCONTEXT		 88		// Request not supported on this context 
#define AERR_NOCONSOLELOG	 89		// No console output buffer enabled
#define AERR_BFRTRUNCATE	 90		// Truncate buffer fails
#define AERR_BFRSEEK		 91		// Move position in buffer fails
#define AERR_BADARG			 92		// Argument value is out-of-range
#define AERR_PORTINUSE		 93		// Port is in-use by another protocol
#define AERR_COOKIEIP		 94		// Client's IP address and cookie's IP do not match.
#define AERR_STALECOOKIE	 95		// Cookie key is outdated or corrupted.
#define AERR_COOKIEMISMATCH	 96		// Cookie key does not match key for this socket
#define AERR_SYSTEMCONTEXT	 97		// Unsupported operation on _SystemContext
#define AERR_UNKPROTOCOL	 98		// Unexpected protocol on this port
#define AERR_UNTERMINATED	 99		// Unterminated message
#define AERR_USRNAMEEXISTS	100		// Username already exists
#define AERR_MAXUSRREACHED	101		// Maximum no. of users reached
#define AERR_BADSECLVL		102		// Invalid security level
#define AERR_PWDOPENFAIL	103		// Failed in opening of password file
#define AERR_PWDWRITEFAIL	104		// Failed in writing to password file

#define AERR_MSGS			105		// One more than last msg. BE SURE TO UPDATE IF COUNT CHANGES

#endif // AERRDEFS_H

