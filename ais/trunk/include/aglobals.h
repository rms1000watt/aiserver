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

#ifndef AGLOBALS_H
#define AGLOBALS_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/aglobals.h
													Global Definitions

CHANGE HISTORY
Version	Date		Who		Change
3.2002	 2/04/2008	fchua	Added new log type LOGMGR_ACCESS.
3.1005	12/16/2007	fchua	Added geDisconnect.
2.0001	1/6/2007	tlw		Remove LOGMGR_NCSA defines.
1.0115	11/20/2006	tlw		Add LOGMGR_CONSOLE to log console output
1.0113	11/13/2006	tlw		REQTYPE. Modify to use QHash
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
NOTES
In a multi-threaded environment, globals can be a difficult problem to manage. In fact,globals should be avoided.
Nevertheless, system-wide resources that are commonly accessed by a wide variety of classes can be best handled by a select
few globals. The alternative is to pass the variables as arguments or  "register" them with every single class.  It is better
to use a global for system-wide access to an important tool, such as error reporting, than to saddle developers with an
unwieldy approach that they will not use.

For ease in management and troubleshooting, the globals are all encapsulated into a single class.  This class may be modified
to suit each application or, better yet, a class may be derived from this class that adjusts the globals for a particular
application.

The initial cut of this class defines a set of warning and error reporting tools that are used in nearly every class of the
application.

PROPERTIES
char*		mpErrLevels[6];
Purpose:  Initialized by AGlobals constructor to a descriptive string for each error level in the enum AErrLvl.  Used by
message logs.

MULTITASKING
Globals that may be accessed by more than one thread must be protected by a mutex.

DOCUMENTATION
 1.	VisualStudio/ProgrammingConventions.doc - Error processing conventions
 2.	C++ FAQS, chapter 9, Error Handling Strategies
 3. include/??? - more detail on message logs.

USAGE
 1.	Create instance of NamedValues class to get configuration parameters
 2.	Use these parameters to create instance of message log class, AMsgLog.
 3.	Call fmtMsg to log warning or progress messages.


ERROR LEVLES
geFatal - Unrecoverable error - throw an exception. Terminates application.

geCritical - Unrecoverable error - log an error exception. Application proceeds anyway.  Mostly used in destructors or where
an exception is not advisable.

geSoftware - Recoverable error w/ bogus input. The promise made by a class is conditioned on usable input. If the input is
bogus, the method does nothing and generates a software error. Used by ACHECK macro.

geWarn - Suspicious input. Same as above except that input does not preclude completion of the prescribed mission.  An error
is logged and the method limps forward.

geInfo - Not an error. Used for debugging, progress messages:
  enum AErrLvl {geNone, geInfo, geWarn, geSoftware, geCritical, geFatal};
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ PRAGMAS ------------------------------------------------------------
#ifdef _Win
#pragma warning(disable : 4290)		// C++ Exception Specification is ignored by Microsoft
#endif

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QMap>
#include <QtCore/QStringList>
#include <QtCore/QVector>
#include <QtGui/QApplication>
#include "aglobaldefs.h"			// AGBL_
#include "ais.h"					// AReqType, AReqTypeMap, AReturnRcvr
class AAisSvr;
class AAppSvr;
class ALogMgr;

//	-------------------------------------------------- GLOBAL TYPEDEFS --------------------------------------------------------
enum AErrLvl {geNone, geInfo, geWarn, geSoftware, geCritical, geFatal};
enum ASecLvl {geNada, geGuest, geLvl2, geLvl3, geLvl4, geLvl5, geLvl6, geSuper};
// See aissvr.h for a description of close mode (order is significant)
enum ACloseMode {geDefault, geDisconnect, geOpen, geSoft, geFirm, geHard, geEnd};

typedef QMap<QString, ACloseMode> ACloseMap;
typedef QMap<long, const char*> AIntCharMap;
typedef QMap<long, long> ALongKeyMap;
typedef QMap<long, QString> AIntStringMap;
typedef QMap<QString, QString> AStringMap;
typedef QMap<QString, long> AStringIntMap;
typedef quint32 AWord;
typedef QVector<AWord> AWordArray;

//	---------------------------------------------------- GLOBAL DATA ----------------------------------------------------------
#include <QtCore/QDebug>
#include "aencrypt.h"
#include "aerrdefs.h"
#include "atimestamp.h"

//	Globals
class AGlobals;
extern AGlobals gAis;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
#define REQNAME(req) gAis.mpRequestNames[(req)]
#define REQTYPE(act) (gAis.mReqTypes.contains(act) ? gAis.mReqTypes.value(act) : geUnknown)

// ************************************************ Global Default Parameters *************************************************
// AIS
#define AIS_HOSTADDRESS	        "127.0.0.1"     // IP address of this server
#define AIS_MACHINENAME         "localhost"     // URL path to this server
#define AIS_MAXBUFFERSIZE       "5000"          // Size [bytes] of buffer to hold disconnected output
#define AIS_MINCONTEXTPOOL      "16"            // Default min memory (16 MB)
#define AIS_FIRSTCONTEXTPOOL    1300000*1024    // Default first context memory block
#define AIS_GBLMEMORYPOOL       1               // Default AIS global memory (2 MB)

// AppClient
 // Location of browser on client machine
#define APPCLIENT_BROWSER		"C:/PROGRA~1/INTERN~1/IEXPLORE.EXE"
#define APPCLIENT_HELPURL		"file:///C:/ais/docs/onlinedocs/index.html"

// LogMgr
#define LOGMGR_ACCESSFILENAME   "access.log"
#define LOGMGR_ACCESSMINLEVEL   "1"
#define LOGMGR_AMPFILENAME      "amp.log"
#define LOGMGR_AMPMINLEVEL      "1"			// Minimum err level to be set
#define LOGMGR_FILEDIR          "logs"		// Folder for log files
#define LOGMGR_CONSOLEFILENAME  "console.log"
#define LOGMGR_CONSOLEMINLEVEL  "1"
#define LOGMGR_LOGSIZE          "10000"
#define LOGMGR_REQHDRFILENAME   "reqhdr.log"
#define LOGMGR_REQHDRMINLEVEL   "1"
#define LOGMGR_SYSMSGFILENAME   "sysmsg.log"
#define LOGMGR_SYSMSGMINLEVEL   "1"

// System
#ifdef _DEBUG						// Defined when compiling debug version
#define ACHECK_DEBUG	1			// Comment out to remove ACHECK code from debug compile
#endif

// *********************************************** Context-specific Definitions ***********************************************
// AIS - Default values if parameter not set in an ini file or in startup.sl
#define AIS_CLIENTVIEWURL               ""              // Let user select if URL not available.
#define AIS_CLOSEMODE                   "soft"          // Default close mode (disconnect,soft,clear,stop)
#define AIS_CLOSEWAIT                   "0"             // Wait period in secs before starting the close of a session
#define AIS_MINSECURITYLEVEL            "7"             // Min sec lvl required to connect to disconnect session

// AppSvr
#define AAPPSVR_AUTOLOGON               "0"             // Bypass logon process if not 0
#define AAPPSVR_DIR                     "wwwroot"       // Dir name at top of subtree for App Files
#define AAPPSVR_PORT                    "0"
#define AAPPSVR_SECUREDIR               "secure"        // Subtree to hold protected App files

// HttpClient
#define AHTTPCLIENT_SZ			4096		// Buffer size increment
#define AHTTPCLIENT_MSEC		2000		// Time to wait for response from server

// HttpSvr
#define AHTTPSVR_AUTOLOGON              "0"             // Bypass logon process if not 0
#define AHTTPSVR_CLOSEWAIT              480             // Default close wait period for HTTP clients [secs]
#define AHTTPSVR_DEFAULT                "index.htm"     // Default name of web page to be served
#define AHTTPSVR_DIR                    "wwwroot"       // Dir name at top of subtree for HTTP files
#define AHTTPSVR_IDLETIME               3360            // Max allowed idle time for outstanding requests [secs]
#define AHTTPSVR_PORT                   "0"
#define AHTTPSVR_DIR                    "wwwroot"       // Dir name at top of subtree for HTTP files
#define AHTTPSVR_REQSECS                600             // Update interval for updating the idle time of all outstanding requests
#define AHTTPSVR_SECUREDIR              "secure"        // Subtree to hold protected pages
#define AHTTPSVR_UPLOADDIR              "upload"        // Subtree to hold files client uploaded to server

// SessionMgr
#define ASMGR_STARTUPSCRIPT             "_startupscript"// Runs startupscript
#define ASMGR_MAXBFRSIZE                4048            // Maximum chars allowed in mDsplyBfr. FireFox may fail if >4096.

// UsrMgr
#define AUSRMGR_FILEDIR                 "usr"           // Dir name for password files and logon forms.
#define AUSRMGR_IDLESECS                "0"             // Max secs between requests on an active connection
#define AUSRMGR_MAXLOGON                "4"             // Maximum number of logon tries

// XmlSvr
#define AXMLSVR_AUTOLOGON               "0"             // Bypass logon process if not 0
#define AXMLSVR_PORT                    "0"
#define AXMLSVR_DIR                     "wwwroot"       // Dir name at top of subtree for XML files
#define AXMLSVR_SECUREDIR               "secure"

// ********************************** The following definitions are not user-settable *****************************************
// Globals
#define AGLBLS_AISDIR                   "AISDIR"                // Environment variable to specify install path
#define AGLBLS_APPINIFILENAME           "context.ini"           // Name of context-specific config. parameter file
#define AGLBLS_APPUSERSFILENAME         "contextusers.ini"      // Name of file with users' security settings
#define AGLBLS_BFRINC                   16                      // Minimum increment in many structure sizes
#define AGLBLS_INSTALLFILENAME          "aisinstall.ini"        // Version-specific global parameter settings
#define AGLBLS_INIFILENAME              "ais.ini"               // User-defined global parameter settings
#define AGLBLS_INPROCSVRNAME            "_InProcess"            // In-process server name
#define AGLBLS_MYSQLDATADIR             "mysqldata"             // Default MySQL database files folder
#define AGLBLS_MYSQLLANGDIR             "mysqlmsgs/"            // Default MySQL error messages folder
#define AGLBLS_MYSQLLANGFILE            "errmsg.sys"            // Default MySQL error messages file
#define AGLBLS_REDIRECTFILENAME         "redirect.ini"          // Redirect ini file
#define AGLBLS_RIDEINSTALLFILENAME      "rideinstall.ini"       // Ride ini file in install (ride.exe) directory
#define AGLBLS_RIDEINIFILENAME          "ride.ini"              // Ride ini file in context's startup directory
#define AGLBLS_STARTUPFILENAME          "astartup.sl"           // Default startup script filename
#define AGLBLS_SYSCONFDIR               "/etc/ais/"             // Default location of installed configurations
#define AGLBLS_SYSSHAREDIR              "/usr/share/ais/"       // Default location of installed read-only files
#define AGLBLS_SYSTEMCONTEXTNAME        "_SystemContext"        // Name of server-wide context for housekeeping
#define AGLBLS_AISUSERDIR               "ais/"                  // Name of AIS directory under $HOME directory
#define AGLBLS_SERVERCFGFILE            "serverscfg.txt"        // Name of the persistent list of servers available.

// AisSvr
#define AISSVR_FLUSHMSEC                500                     // Interval [msec] to flush stale console buffers
#define AISSVR_HELPDIR                  "docs/onlinedocs"       // Path to online help files.
#define AISSVR_DOCSDIR                  "/usr/share/doc/ais/onlinedocs" // Path to intalled online help files.

// Supported protocol types
const long AISSVR_ALL		= 0;		// Subscribe to all protocols
const long AISSVR_APP		= 1;		// App (AIS IDE) protocol
const long AISSVR_HTTP		= 2;		// HTTP protocol
const long AISSVR_XML		= 3;		// XML (Macromedia Flash) protocol
const long AISSVR_DIS		= 4;		// Disconnected
const long AISSVR_INPROC	= 5;		// In-process
const long AISSVR_NOCONNECT	= 6;		// Not connected (Admin Sessions)
const long AISSVR_PROTOCOLS	= 7;		// Total number of protocols
const long AISSVR_SHIFT		= 3;		// Number of bits reserved for protocol. Increase if total > 8
const long AISSVR_MASK		= 7;		// 2**AISSVR_SHIFT - 1

// LogMgr Log Suffix, Log types
#define LOGMGR_EXTENSION        ".log"
#define LOGMGR_NONE             0
#define LOGMGR_AMP              1
#define LOGMGR_CONSOLE          2
#define LOGMGR_REQHDR           3
#define LOGMGR_SYSMSG           4
#define LOGMGR_ACCESS           5
#define LOGMGR_STATUS           6
#define LOGMGR_ALL              7
#define LOGMGR_SIZE             8               // Set to number of logs

// UsrMgr
#define USRMGR_GUESTNAME		"guest"
#define USRMGR_GUESTSECLVL		geGuest
#define USRMGR_GUESTUSRID		2
#define USRMGR_NOBODYNAME		"nobody"
#define USRMGR_NOBODYSECLVL		geNada
#define USRMGR_NOBODYUSRID		1
#define USRMGR_NULLNAME			"null"
#define USRMGR_NULLSECLVL		geNada
#define USRMGR_NULLUSRID		0
#define USRMGR_PASSWDFILE		"passwd.txt"
#define USRMGR_SYSNAME			"system"
#define USRMGR_SYSSECLVL		geSuper
#define USRMGR_SYSUSRID			3

// HttpSvr
#define AHTTPSVR_COOKIENAME		"abaseid"
#define AHTTPSVR_AMPDLL			"/amp.dll"
#define AHTTPSVR_NAME			"InvestByLambda-AIS/1.0"

// XmlSvr
#define AXMLSVR_HOSTNAME		"xml.client.com"
#define AXMLSVR_METHOD			"XML"
#define AXLMSVR_REFERER			"."
#define AXMLSVR_USERAGENT		"flash-player"

#define DEL             '\177'
#define DELSTG          "\177"
#define EOR             '\266'          // Paragraph mark ()

//	------------------------------------------------- CLASS DECLARATION -------------------------------------------------------
class AGlobals
{
public:
	AGlobals();
	~AGlobals();
	void beep()throw();
	void init(ALogMgr* ipLogMgr) throw();
	void showList(const QStringList& irList);

	AAisSvr*       mpAisSvr;                       // Ais server
	ACloseMap      mCloseModes;                    // Key: close mode name, Value: ACloseMode
	const char*    mpCloseMode[geEnd];             // Close mode names
	AAppSvr*       mpInProcSvr;                    // In-proc server for AppClient
	bool           mIsService;                     // True iff running as a service.
	const char*    mpErrLevels[6];                 // Error level strings
	const char*    mpErrMsgs[AERR_MSGS];           // Error messages returned to clients
	const char*    mpPortNames[4];                 // Port name used in context-specific ini file
	const char*    mpProtocolNames[7];             // Protocol names used in context-specific ini file
	const char*    mpRequestNames[AREQTYPES];      // Built-in speech acts indexed by Request Type.
	AReqTypeMap    mReqTypes;                      // Key: Built-in speech act name, Value: AReqType
	const char*    mpSvrNames[4];                  // Protocol server names
	ulong          mStartTime;                     // Time server last started [secs since 1/1/2000]
	AStringMap     mGblParams;                     // Global configuration parameters
	AStringMap     mCtxParams;                     // Server-wide, default context params
	AWordArray     mKey4;                          // A 4 word encryption key used for test purposes
	ALogMgr*       mpLogMgr;                       // Handles all messaging
	char*          mpFirstBlock;                   // Pointer to first application context memory block.
};

inline AGlobals::AGlobals()
: mKey4(4)
{
	// AisSvr:
	mpAisSvr = NULL;

	// CloseModes:
	mCloseModes["default"] = geDefault;
	mCloseModes["open"] = geOpen;
	mCloseModes["disconnect"] = geDisconnect;
	mCloseModes["soft"] = geSoft;
	mCloseModes["firm"] = geFirm;
	mCloseModes["hard"] = geHard;
	mpCloseMode[geDefault] = "default";
	mpCloseMode[geOpen] = "open";
	mpCloseMode[geDisconnect] = "disconnect";
	mpCloseMode[geSoft] = "soft";
	mpCloseMode[geFirm] = "firm";
	mpCloseMode[geHard] = "hard";

	// InProcSvr, IsService
	mpInProcSvr = NULL;
	mIsService = false;

	// ErrLevels and ErrMsgs
	mpErrLevels[geNone]    = "None";
	mpErrLevels[geInfo]    = "Info";
	mpErrLevels[geWarn]    = "Warn";
	mpErrLevels[geSoftware]= "Soft";
	mpErrLevels[geCritical]= "Critical";
	mpErrLevels[geFatal]   = "Fatal";
	#include "aerrmsgs.h"		// Initialize mpErrMsgs

	// PortNames:
	mpPortNames[AISSVR_ALL] = "allports";
	mpPortNames[AISSVR_APP] = "appport";
	mpPortNames[AISSVR_HTTP]= "httpport";
	mpPortNames[AISSVR_XML] = "xmlport";

	// ProtocolNames:
	mpProtocolNames[AISSVR_ALL] = "all";
	mpProtocolNames[AISSVR_APP] = "app";
	mpProtocolNames[AISSVR_HTTP] = "http";
	mpProtocolNames[AISSVR_XML] = "xml";
	mpProtocolNames[AISSVR_DIS] = "disconnected";
	mpProtocolNames[AISSVR_INPROC] = "in-process";
	mpProtocolNames[AISSVR_NOCONNECT] = "no-connection";

	// RequestNames and ReqTypes:
	#include "areqtypes.h"		// Initialize request type maps

	// SvrNames:
	mpSvrNames[AISSVR_ALL] = "NoSvr";
	mpSvrNames[AISSVR_APP] = "AppSvr";
	mpSvrNames[AISSVR_HTTP] = "HttpSvr";
	mpSvrNames[AISSVR_XML] = "XmlSvr";

	// StartTime. Set server start time [secs since 1/1/2000]
	mStartTime = ATimeStamp::localTime();

	// Key4. Must be a fixed key for duration of an AIS instance
	AEncrypt::newKey(4, mKey4);

	// LogMgr
	mpLogMgr = NULL;
}
inline AGlobals::~AGlobals()
{
	mCloseModes.clear();			// Key: close mode name, Value: ACloseMode
	mReqTypes.clear();				// Key: Built-in speech act name, Value: AReqType
	mGblParams.clear();				// Global configuration parameters
	mCtxParams.clear();				// Server-wide, default context params
	mKey4.clear();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AGlobals()");
#endif
}

// beep - Set a breakpoint here to catch errors.
inline void AGlobals::beep() throw()
{
#ifdef _DEBUG
	qApp->beep();
#else
	qDebug("Beep");
#endif
}
inline void AGlobals::init(ALogMgr* ipLogMgr) throw()
{
	mpLogMgr = ipLogMgr;
}

inline void AGlobals::showList(const QStringList& irList)
{
	qDebug() << irList.join(",");
}

#endif // AGLOBALS_H
