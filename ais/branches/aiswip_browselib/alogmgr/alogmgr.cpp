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
ALogMgr.cpp
														Message Logs

ALogMgr records web server activity in a log file.

CHANGE HISTORY
Version	Date		Who		Change
3.2002	 2/04/2008	fchua	Added new log type User Access.
3.1004	11/2/2007	fchua	Modified setLogLvl(). Corrected insertion procedure.
2.0001	1/5/2007	tlw		ncsaLog. Remove ncsa log.
1.0120	12/25/2006	tlw		sendMsg. Add returnOutput, add pData and ataSize arguments
1.0115	11/23/2006	tlw		ALogMgr. Add LogConsole to the list of logs.
1.0115	11/19/2006	tlw		ALogMgr. Make ring file's persistent to maintain current record of activity.
1.0113	11/7/2006	tlw		~ALogInfo. Reclaim all ALogInfo allocated resources on destruct.
1.0112	10/27/2006	tlw		~ALogMgr. Reclaim all cLogs on destruct.
1.0107	9/19/2006	tlw		Postpone delete of ring file in destructor
1.0065	7/22/2005	tlw		setLogLvl. Reset all log types if iLogType is LOGMGR_NONE (0).
1.0060	4/27/2005	tlw		Convert to using ring file.
1.0059	4/22/2005	tlw		Remove disconnectSession
1.0057	3/ 3/2005	tlw		Add documentation
												---------------------------------

	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <stdarg.h>				// va_arg
#include <QtCore/QtDebug>
#include <QtCore/QDir>
// #include <QtCore/QFile>
#include <QtCore/QObject>
#include <QtCore/QStringList>

#include "aglobals.h"			// ACHECK, 
#include "alogmgr.h"			// ALogMgr
#include "ausrmgr.h"			// AUsrMgr
#include "aissvr.h"				// AAisSvr
#include "aisoutput.h"			// AISMGROUT_SIZE
#include "asessionmgrinterface.h"	// ASessionMgrInterface
#include "aismgr.h"				// AAisMgr
#include "autilities.h"			// AUtil

//	------------------------------------------------- DATA DEFINITIONS --------------------------------------------------------
const char* ALogMgr::mpFileStatus[] = {
		"No device error",				//  IO_Ok				0
		"Unable to read from device",	//  IO_ReadError		1
		"Unable to write to device",	//  IO_WriteError		2
		"Fatal unrecoverable error",	//	IO_FatalError		3
		"Unsupported device operation",	//	IO_ResourceError	4
		"Can't open device",			//	IO_OpenError		5
		"Operation aborted",			//	IO_AbortError		6
		"Operation timed out",			//	IO_TimeOutError		7
		"Unspecified error on close",	//	IO_UnspecifiedError	8
		"Status out-of-range",			//	Unexpected error	9
	};

// See also LOGMGR definitions in aglobals.h
const char* ALogMgr::mpLogTypeNames[] =
		{"None","Amp","ReqHdr","SysMsg","UsrAccess","StatusBar", "All"};

const char* ALogMgr::mpWarnLvlNames[] =
        {"None", "Info", "Warn", "Software", "Critical", "Fatal"};

//	------------------------------------------------ ALOGINFO FUNCTIONS -------------------------------------------------------
ALogInfo::ALogInfo()
{
	mClientList.clear();
	mMinLevel = geNone;			// geNone, geInfo, geWarn, geSoftware, geCritical, geFatal
	mReqType = geUnknown;
}

/*!
\brief Destructor Free allocated resources

\par Notes:
-# ALogInfo has remote ownership of ALogClientSt in mClientList and ARingFile referents.
-# Since instances of ALogInfo are never assigned, copied or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
ALogInfo::~ALogInfo()
{
	long aClient, aSz = mClientList.size();
	for (aClient = 0; aClient < aSz; ++aClient)
		delete mClientList[aClient];

	if (mpRingFile != NULL)
		delete mpRingFile;

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~ALogInfo()");
#endif
}

//	------------------------------------------------ ALOGMGR FUNCTIONS --------------------------------------------------------
/*!
\brief ALogMgr - Constructor initializes all the log types.

\param irGblParams - A map of the system's global parameters.
\return void
 */
ALogMgr::ALogMgr(AStringMap& irGblParams)
{
	// Set configuration parameters
	cpAisSvr = NULL;				// Set later by setAisSvr()

	// LogDir. Create path from InstallDir forward if LogFileDir does not exist.
	QString& arLogDir = irGblParams["gbllogfiledir"];
	QFileInfo aFileInfo;
	QDir aDir;

	// If path is relative, prepend application directory
	aFileInfo.setFile(arLogDir);
	if (aFileInfo.isRelative())
		arLogDir = irGblParams["gblaisapplicationpath"] + arLogDir;

	AUtil::terminateStg(QDir::separator().toAscii(), arLogDir);

	// Create log directory, we currently have no way of telling the user
	// if the log directory was not created.
	aDir.setPath(arLogDir);
	if (!aDir.exists())
		aDir.mkpath(arLogDir);

	cMt = "";

	// Save the log path in the global parameters
	irGblParams["gblaislogpath"] = arLogDir;

	// Initialize each log type in cLogTypes
	QString aFile;			// File name for this log file.
	long aLogSize = irGblParams["gbllogsize"].toLong();
	for (long aLogType = 0; aLogType < LOGMGR_SIZE; ++aLogType)
	{	ALogInfo& arLog = cLogs[aLogType];
		arLog.mpRingFile = NULL;
		aFile.clear();
		switch (aLogType)
		{
		case LOGMGR_NONE:		// 0
			arLog.mMinLevel = geFatal;	// geNone, geInfo, geWarn, geSoftware, geCritical, geFatal
			arLog.mReqType = geUnknown;
			break;
		case LOGMGR_AMP:		// 1
			aFile = irGblParams["gbllogampfilename"]; 
			arLog.mMinLevel = (AErrLvl)irGblParams["gbllogampminlevel"].toInt();
			arLog.mReqType = geLogAmp;
			break;
		case LOGMGR_CONSOLE:	// 2
			aFile = irGblParams["gbllogconsolefilename"];
			arLog.mMinLevel = (AErrLvl)irGblParams["gbllogconsoleminlevel"].toInt();
			arLog.mReqType = geUnknown;		// Not used for this log type.
			break;
		case LOGMGR_REQHDR:		// 3
			aFile = irGblParams["gbllogreqhdrfilename"];
			arLog.mMinLevel = (AErrLvl)irGblParams["gbllogreqhdrminlevel"].toInt();
			arLog.mReqType = geLogReqHdr;
			break;
		case LOGMGR_SYSMSG:		// 4
			aFile = irGblParams["gbllogsysmsgfilename"];
			arLog.mMinLevel = (AErrLvl)irGblParams["gbllogsysmsgminlevel"].toInt();
			arLog.mReqType = geLogSysMsg;
			break;
		case LOGMGR_STATUS:		// 5
			arLog.mMinLevel = geInfo;
			arLog.mReqType = geLogStatus;
			break;
		case LOGMGR_ACCESS:		// 6
			aFile = irGblParams["gbllogaccessfilename"];
			arLog.mMinLevel = (AErrLvl)irGblParams["gbllogaccessminlevel"].toInt();
			arLog.mReqType = geLogUserAccess;
			break;
		default:
			arLog.mMinLevel = geFatal;
			arLog.mReqType = geUnknown;
			break;
		}

		if (!aFile.isEmpty())
		{	if (!aFile.endsWith(LOGMGR_EXTENSION,Qt::CaseInsensitive))
				aFile += LOGMGR_EXTENSION;

			arLog.mpRingFile = new ARingFile(true/*Clear*/,true/*Persistent*/,false/*Record*/,aLogSize,arLogDir + aFile);
			// Ring File. Test the ring file functionality
			if (aLogType == LOGMGR_SYSMSG)
				arLog.mpRingFile->selfTest();
		}
	}
}

/*!
\brief addAmpMsg - AMP request and response to be forwarded to subscribers and written to the log file

\param iContextId - ID of the context generating this message
\param iSessionId - ID of the session generating this message
\param iReqId - Request ID for the request.
\param irAmpmsg - AmpMsg generating request
\param irAmpOut - Response to this message
\return void
\par Notes:
-# Prepend context name and session id to each entry.
 */
void ALogMgr::addAmpMsg(long iContextId, long iSessionId, long iReqId, const QString& irAmpmsg, const QString& irAmpOut)
{
	QString aMsg;				// contextname|sessionid|reqid|AMPMSG:%s AISOUT:%s
	const QString& arContextName = (iContextId > 0) ? cpAisSvr->getContextName(0,iContextId) : "";
	QString aOut(irAmpOut.left(128));
	aOut.replace('\n', "\266");
	aMsg.sprintf("%s|%3ld|%4ld|AMPMSG:%.128s AISOUT:%s\n",
        arContextName.toLatin1().data(), iSessionId, iReqId, irAmpmsg.toLatin1().data(), aOut.toLatin1().data());
	sendMsg(LOGMGR_AMP, geWarn, aMsg, iSessionId, iContextId);
}

/*!
\brief addSysMsg3 - Special version of addSysMsg that expects 3 arguments.

\param ipFmt - Format string containing three %s entries.
\param ipArg1 - String associated with the first %s
\param iArg2 - String associated with the second %s
\param ipArg3 - String associated with the third %s
\return void
\par Notes:
-# Tailored for the ACHECK macro (see globals.h)
 */
void ALogMgr::addSysMsg3(char* ipFmt, char* ipArg1, long iArg2, char* ipArg3)
{
	QString aMsg;
	aMsg.sprintf(ipFmt, ipArg1, iArg2, ipArg3);
	addSysMsg(geSoftware, aMsg);
}
 
/*!
\brief addSysMsg - Prepend warnlvl, timestamp. Send msg to qualified clients and write to log.

\param iWarnLvl - geNone, geInfo, geWarn, geSoftware, geCritical, geFatal
\param irMsg - System message to be logged
\return void
\par Notes:
-# Add breakpoint at qDebug to stop on system errors.
 */
void ALogMgr::addSysMsg(AErrLvl iWarnLvl, QString& irMsg)
{
	// Note message in debugger as well
	if (irMsg.isEmpty()) return;
	qDebug() << irMsg;
	if (iWarnLvl > geInfo)
		qDebug("ALogMgr::addSysMsg(), %s", irMsg.toAscii().data());

	// Prepend warning level and time.
	formatSysMsg(iWarnLvl, irMsg);	// Appends a newline.

	// Send entry to subscribers and to log file
	sendMsg(LOGMGR_SYSMSG, iWarnLvl, irMsg);
}

/*!
\brief formatSysMsg - Prepend warning level and timestamp to system message

\param iWarnLvl - geNone, geInfo, geWarn, geSoftware, geCritical, geFatal
\param iorMsg - System message
\return void
\par Notes:
-# The formatted message is returned in iorMsg (modified in place)
 */
void ALogMgr::formatSysMsg(AErrLvl iWarnLvl, QString& iorMsg)
{
	long aWarnLvl = iWarnLvl;	// Convert to long
	QString aMsg;
	QString aTime = ATimeStamp::timeStamp();
	const char *apLvl = gAis.mpErrLevels[aWarnLvl];
	aMsg.sprintf("%s, %.32s, %s", apLvl, aTime.toLatin1().data(),iorMsg.toLatin1().data());
	AUtil::terminateStg('\n', aMsg);
	iorMsg = aMsg;
}

/*!
\brief sendMsg - Send formatted message to qualified subscribers and add entry to log file

\param iLogType - "DisplayAll","Amp","ReqHdr","SysMsg","StatusBar"
\param iWarnLvl - geNone, geInfo, geWarn, geSoftware, geCritical, geFatal
\param irMsg - Formatted message
\param iContextId - ID of associated context
\param iSessionId - ID of associated session generating this message
\return void
 */
void ALogMgr::sendMsg(long iLogType, AErrLvl iWarnLvl, const QString& irMsg, long iContextId, long iSessionId)
{
    Q_UNUSED(iContextId);
    Q_UNUSED(iSessionId);

	// Check inputs. Do NOT use ACHECK
	if (iLogType <= 0 || iLogType >= LOGMGR_SIZE)
	{	qDebug("ALogMgr::sendMsg(LogType:%s, WarnLvl:%s, Msg:%s), Inputs out of range.",
		LOGTYPENAMES(iLogType), LOGWARNLVLNAMES(iWarnLvl), irMsg.toLatin1().data());
	}
	else
	{	ALogInfo& arLog = cLogs[iLogType];
		// Subscribers. Send to those clients if iWarnLvl is at or above the minimum Err level for this client
		if (iLogType != LOGMGR_CONSOLE)
		{	AReqType aReqType = arLog.mReqType;
			AClientList& arClients = arLog.mClientList;
			if (arClients.count() > 0)
			{	ALogClientSt* apClient;
				long aSize = arClients.size();
				for (long aClient = 0; aClient < aSize; ++aClient)
				{	apClient = arClients.at(aClient);
					// PENDING ALogMgr::sendMsg(). Limit messages if client wants to subscribe to just 1 SessionId or ContextId
					if (iWarnLvl >= apClient->mErrLvl)
						cpAisSvr->returnMsg(apClient->mConnectId, 0/*RqId*/, 0/*Status*/, aReqType, 0/*RetValue*/, irMsg
						, NULL/*Data*/, 0/*DataSize*/, cMt, cMt);
				}
			}
		}
		// Log File. Also, append msg to log file if iWarnLvl at or above log file minimum. Don't log status messages.
		if (iWarnLvl >= arLog.mMinLevel && arLog.mpRingFile != NULL)
			arLog.mpRingFile->append(irMsg.toLatin1().data(), irMsg.length());
	}
}

/*!
\brief setLogLvl - Maintain a list of subscribers sorted by error level, in ascending order

\param iLogType - geLogAll,geLogAmp,geLogConsole,geLogReqHdr,geLogSysMsg,geLogStatus
\param iWarnLvl - geNone, geInfo, geWarn, geSoftware, geCritical, geFatal
\param iConnectId - Connect ID of subscriber.
\return void
\par Notes:
-# First deletes an existing entry and then reinserts at new warning level.
-# Set all log types if iLogType is LOGMGR_NONE
 */
void ALogMgr::setLogLvl(AReqType iLogType, AErrLvl iWarnLvl, long iConnectId)
{
	if (iLogType < geLogAll || iLogType > geLogStatus || iConnectId <= 0)
	{	qDebug("ALogMgr::setLogLvl(LogType:%d, WarnLvl:%s, ConnectId:%ld), Inputs out of range.", iLogType
		, LOGWARNLVLNAMES(iWarnLvl), iConnectId);
	}
	else if (iLogType == geLogAll)	// Recursively reset all logs to iWarnLvl.
	{	for (long aLogType = iLogType + 1; aLogType <= geLogStatus; ++aLogType)
			setLogLvl((AReqType)aLogType, iWarnLvl, iConnectId);
	}
	else	// Remove existing entry, if any, in list of clients for this log type
	{	ALogInfo& arLog = cLogs[iLogType - geLogAll];
		AClientList& arClients = arLog.mClientList;
		ALogClientSt* apClient;
		long aSize = arClients.size();
		for (long aClient = 0; aClient < aSize; ++aClient)
		{	apClient = arClients.at(aClient);
			if (apClient->mConnectId == iConnectId)
			{	delete apClient;
				arClients.removeAt(aClient);
				break;
			}
		}
		// Insert new entry in client list ordered by warning level
		if (iWarnLvl > geNone && iWarnLvl < geFatal)
		{	apClient = new ALogClientSt;
			apClient->mConnectId = iConnectId;
			apClient->mErrLvl = iWarnLvl;
			if (arClients.isEmpty())
				arClients.append(apClient);
			else
			{	ALogClientSt* apLog;
				long aClient = 0;
				aSize = arClients.size();
				for (aClient = 0; aClient < aSize; ++aClient)
				{	apLog = arClients.at(aClient);
					if (apLog->mErrLvl >= iWarnLvl)
						break;
				}
				arClients.insert(aClient, apClient);		// Append if aClient == count()
			}
		}
	}
}
// end
