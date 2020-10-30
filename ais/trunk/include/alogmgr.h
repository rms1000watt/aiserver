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

#ifndef ALOGMGR_H
#define ALOGMGR_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/alogmgr.h
														Log Manager

CHANGE HISTORY
Version	Date		Who		Change
3.2002	 2/04/2008	fchua	Added LOGUSRACCMSG.
2.0001	1/6/2007	tlw		Ncsa. Remove ncsa logs.
1.0115	11/23/2006	tlw		LogConsole. Add LOGCONSOLEMSG.
1.0113	11/7/2006	tlw		~ALogInfo. Add a constructor and destructor.
1.0060	4/27/2005	tlw		Remove format string from most methods.
1.0057	3/ 3/2005	tlw		Add documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "ais.h"				// AReqType
#include "aringfile.h"			// ARingFile
class AAisSvr;
class AAisMgr;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
// Error processing
#ifdef ACHECK_DEBUG
#define ACHECK(x) ((x)?(void)0:gAis.mpLogMgr->addSysMsg3("CHECK, %s line %d, %.256s fails.", __FILE__, __LINE__, #x))
#else
#define ACHECK(x)
#endif
#define LOGAMPMSG(contextid,sessionid,reqid,ampmsg,outmsg) gAis.mpLogMgr->addAmpMsg(contextid,sessionid,reqid,(ampmsg),(outmsg))
#define LOGCONSOLEMSG(msg) gAis.mpLogMgr->sendMsg(LOGMGR_CONSOLE, geInfo, (msg))
#define LOGFILESTATUS(status) gAis.mpLogMgr->mpFileStatus[(status) <= 8 ? (status) : 9 ]
#define LOGREQHDRMSG(msg) gAis.mpLogMgr->sendMsg(LOGMGR_REQHDR, geWarn, (msg))
#define LOGUSRACCMSG(msg) gAis.mpLogMgr->sendMsg(LOGMGR_ACCESS, geWarn, (msg))
#define LOGSTATUSMSG(msg) gAis.mpLogMgr->sendMsg(LOGMGR_STATUS, geInfo, (msg))
// sysmsg format is "errcode, class::fcn(arg0:value0...), reason, varname:value
#define LOGSYSMSG(lvl, msg) gAis.mpLogMgr->addSysMsg((lvl), (msg))
#define LOGTYPENAMES(type) gAis.mpLogMgr->mpLogTypeNames[(type)]
#define LOGWARNLVLNAMES(warnlvl) gAis.mpLogMgr->mpWarnLvlNames[(warnlvl)]

//	---------------------------------------------------- DEFINED TYPES --------------------------------------------------------
// Client info for every client receiving log messages
typedef struct
{	long		mConnectId;
	AErrLvl		mErrLvl;
} ALogClientSt;
typedef QList<ALogClientSt*> AClientList;

// Information for each log type (Amp, Console, ReqHdr, ...)
// See ALOGMGR in aglobals.h for a list of log types.
class ALogInfo
{
public:
	ALogInfo();
	~ALogInfo();

	AClientList	mClientList;	// List of clients subscribing to this log
	AErrLvl		mMinLevel;		// Minimum level added to log file (geNone, geInfo, geWarn, geSoftware, geCritical, geFatal)
	AReqType	mReqType;		// Request type (enum) for this log type
	ARingFile*	mpRingFile;		// Ring file for this log
};

//	-------------------------------------------------- CLASS DECLARATION ------------------------------------------------------
/*!
\brief ALogMgr provides a flexible way to record messages regarding the status of a task.

Applications have some high-level objects that are designated as "controllers".  Controllers establish policy that may
be used by all other objects in their implementation.  The constructor for an instance of ALogMgr is called by the
controller which establishes the device to be used to receive messages for the application.  The device can be modified
as the application proceeds.  For example, messages may initially go to a console, but then go to a log file after a log
file is established.

\par Multitasking.
- Each instance of this class must use a separate log file.
- For now, all writers to a log must be on the same thread.

\par Warning Levels.
Warning levels are specified by the values of the global enum AErrLvl:
- geNone - An invalid code (0).
- geInfo - A milestone in the process.  Reserved for a normal, expected event.
- geWarn - A dubious situation that does not prevent the module from meeting its mission.
- geSoftware - An unexpected input or result.  Probably a bug that should be fixed.
- geCritical - A serious error that puts the mission in jeopardy.  The application continues under dubious circumstances.
- geFatal - An unrecoverable error that prevents the module from its mission.  The application terminates.	
A warning level threshold may be set to filter the amount of logging. Only warnings above the threshold are logged.
	
\par Example Message.
Info,01/Jan/1997:12:57:45 -0600,0,AClass::someMethod,some description.

Each entry in the log is one line with warning level, time stamp, message.  Note that the message passed to sendMsg should
be a comma-delimited set of fields containing:
- System error/status code; else 0.
- Class and function name.
- Significant arguments and description.
The warning levels descriptions are:
- None, Info, Warning, Software, Critical, Fatal

\par Log File Fields.
-# remotehost - IP address or DNS name of remote host
-# rfc931 - The remote login name of the user.
	Usually just a dash.
-# authuser - The authenticated username (usually a dash)
-# [date localtime offset] - [dd/Mon/yyyy:hr:min:sec -hrs]
Offset of local time from Universal Coordinated Time (UTC).
For example, PDT is 8 hours behind (minus) UTC.
For this discussion Greenwich Mean Time (GMT) is same UTC.
Local time is the time that the server started responding to request.
-# "request" - The request uri as it came from the client.
-# status - HTTP response code returned to the client.
-# bytes - The number of bytes returned to client.
-# user-Lambda - The description of the web client issuing the request.
-# referer - The URL of the web page link that generated the request, if any.
 The misspelling of referrer is codified in the standard, nothing I can do about it.

\par Notes:
-# The CLF includes the first 7 fields. The ECLF includes an additional 10 optional fields.
-# Eight of the extended CLF fields are not included here.  See documentation listed below for more details.
-# Fields are separated by a space.
-# Empty fields are denoted by a dash (-).

\par Usage.
- Create instance of NamedValues class to get configuration parameters
- Use these parameters to create instance of message log,
- Call sendMsg to log warning or progress messages.
- Or create instance of ... in conjunction with a web server that utilizes addNcsaEntry to log web server activity.
*/
class ALogMgr
{
public:
	ALogMgr(AStringMap& irParams);
	void		addAmpMsg(long iContextId, long iSessionId, long iReqId, const QString& irAmpmsg, const QString& irAmpOut);
	void		addSysMsg3(char* ipFmt, char* ipArg1, long iArg2, char* ipArg3);
	void		addSysMsg(AErrLvl iWarnLvl, QString& irMsg);
	void		formatSysMsg(AErrLvl iWarnLvl, QString& iorMsg);
	void		sendMsg(long iLogType, AErrLvl iWarnLvl, const QString& irMsg, long iContextId = 0, long iSessionId = 0);
	void		setAisSvr(AAisSvr* ipAisSvr);
	void		setLogLvl(AReqType iLogType, AErrLvl iWarnLvl, long iConnectId);

	AReqTypeMap	mReqTypes;				    // key=request name, value=request type
	static const char* mpFileStatus[];	    // A list of error messages returned by file I/O
	static const char* mpLogTypeNames[];	// A list of logType names
	static const char* mpWarnLvlNames[];	// A list of the warning level names

private:
	AAisSvr*	cpAisSvr;				// Ptr to AisSvr that uses these logs
	QString		cMt;					// Empty (not null) placeholder
	ALogInfo	cLogs[LOGMGR_SIZE];		// Info for each log type
};

inline void	ALogMgr::setAisSvr(AAisSvr* ipAisSvr) { cpAisSvr = ipAisSvr;}

#endif // ALOGMGR_H
