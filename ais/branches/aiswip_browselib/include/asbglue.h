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

/*
SBGlue.h

CHANGE HISTORY
Version	Date		Who		Change
4.002	 9/28/2008	tlw		CR128. Add subscribe to allow a connection subscribe to a session.
1.0120	12/19/2006	tlw		Eval. Add ipData argument.
1.0105	12/9/2006	tm		Added SBGLUE_ERR_BADBUFFERSIZE
1.0112	11/3/2006	tlw		List SBGlue functions by category
1.0110	10/12/2006	tlw		Add WaitTimer
1.0057	 3/18/2005	tlw		Update documentation
*/

/*  Make sure that we skip this include file if it has already been seen.   */
#ifndef _H_SBGLUE
#define _H_SBGLUE

#define SBGLUE_URL_BUFFER_SIZE 4096

#define SBGLUE_RETURN_CHUNKSIZE	4096

#define SBGLUE_MAX_CONTEXTS	128

#define SBGLUE_SUCCESS				0
#define SBGLUE_EVAL_SUCCESS			0
#define SBGLUE_EVAL_SUSPEND			1
#define SBGLUE_EVAL_FAILURE			-100
#define SBGLUE_ERR_FILE_READ		-101
#define SBGLUE_ERR_FILE_WRITE		-102
#define SBGLUE_ERR_OUT_OF_MEMORY	-103
#define SBGLUE_ERR_FRAME_ERROR		-104
#define SBGLUE_ERR_INVALID			-105
#define SBGLUE_ERR_STACK			-106
#define SBGLUE_ERR_ESCAPE			-107
#define SBGLUE_ERR_PCODE			-108
#define SBGLUE_ERR_BAD_DATATYPE		-109
#define SBGLUE_ERR_RECURSION		-110
#define SBGLUE_ERR_FRAME_RELEASE	-111
#define SBGLUE_ERR_STACK_RELEASE	-112
#define SBGLUE_ERR_RECURSION_RELEASE -113
#define SBGLUE_ERR_QUIT				-114
#define SBGLUE_ERR_WRONG_VERSION	-115
#define SBGLUE_ERR_ENGINE_BUSY		-116
#define SBGLUE_ERR_REPOSITORY_GC	-117
#define SGBLUE_UNEXPECTED_ERROR		-118
#define SBGLUE_ERR_NOT_IMPLEMENTED  -119
#define SBGLUE_ERR_SYSTEM_ERROR		-120
#define SBGLUE_ERR_EVAL_SECURITY	-121
#define SBGLUE_ERR_EVAL_BAD_CHECKIN -122
#define SBGLUE_ERR_EVAL_BAD_FILTER	-123
#define SBGLUE_ERR_EVAL_BAD_SCORE	-124
#define SBGLUE_SENDTOCLIENT_FAILURE -125
#define SBGLUE_ERR_BADBUFFERSIZE	-126

// iEvalTypes
#define SBGLUE_CMDSTRING			0		// simple cmd expression request
#define SBGLUE_MSGBLOCK				1		// amp message request
#define SBGLUE_DEBUGCMD				2
#define SBGLUE_CHECKIN				3
#define SBGLUE_DIRINFO				4
#define SBGLUE_FILEOPENRESPONSE		5
#define SBGLUE_EVENTMSG				6		// amp message event 
#define SBGLUE_EVENTCMD				7		// simple cmd expression event
#define SBGLUE_CMDSTRING_BINTRANSFER 8		// execute returning binary result

#define SBGLUE_NO_DEBUG				1
#define SBGLUE_WITH_DEBUG			0

/* AIS/SMTBASE control flags */
// Note that some of these defines are also defined in
// appclient.cpp. Change then there if you change them here!
#define SBGLUE_ERROR_TRACE			0x01	//1st bit 
#define SBGLUE_INSTRUCTION_TRACE	0x02	//2nd bit
#define SBGLUE_SYSCHECKON			0x04	//3rd bit
#define SBGLUE_JITON				0x10	//4th bit
#define SBGLUE_ESCAPE				0x100	//7th bit
#define SBGLUE_MODIFIED				0x200	//8th bit

/*  Host system external symbol definition (How API symbols are exported). */
#ifndef PUBLIC
#ifdef _MSVC
#define PUBLIC              extern _declspec(dllexport)
#else
#define PUBLIC              extern
#endif
#endif

// External functions for use by SessionManager:
long SBGlue_CalculateStackSpace(long iMemorySize);
long SBGlue_CloseContext(void* ipContext);
long SBGlue_Eval(void* ipContext, long iSessionID, long iRequestID, long iUserID, long iLevel, long iRequestType, long iNoDebug
	, long iSetJmp, long iJitMode, char* ipCmdString, char* ipData);
long SBGlue_Init();
long SBGlue_OpenContext(const char* ipContextName, const char* ipScript, void** ipContext, NUM iMemorySize, long iObjHdrSize
	, QThread* ipSessionMgrContextThread);
long SBGlue_SetEngineFlags(long iContextIndex,long iFlag, long iFlagValue);
long SBGlue_CountOfOpenContexts();

/* SBGlue functions registered with the SmartBase Engine:
SBGlue_AXml
SBGlue_Browse
SBGlue_BuildLambdaMessage
SBGlue_ContextClient
SBGlue_cpOSFile
SBGlue_DebugDialog
SBGlue_DecodeURL
SBGlue_Dir
SBGlue_DisplayWorkbenchWindow
SBGlue_EnableConsoleLog
SBGlue_EncodeURL
SBGlue_EvalAMP
SBGlue_EvalsInSyncLocalContext
SBGlue_ExistsOSFile
SBGlue_FileInfo
SBGlue_GetConsoleLog
SBGlue_GetContextPtr
SBGlue_GetFileStatus
SBGlue_GetHttp
SBGlue_GetMachineName
SBGlue_GetOSDir
SBGlue_GetTickCount
SBGlue_HostCreate
SBGlue_HostDestroy
SBGlue_HostGetProperty
SBGlue_HostInvoke
SBGlue_HostPutProperty
SBGlue_HostSend
SBGlue_Input
SBGlue_LoadBrowseLib
SBGlue_LoadLib
SBGlue_LoadUtilityLambdas
SBGlue_MkOSDir
SBGlue_MsgBox
SBGlue_NextUserID
SBGlue_Notify
SBGlue_OpenConsoleLog
SBGlue_PostHttp
SBGlue_ReadHtmlPage
SBGlue_ReloadLib
SBGlue_RingBell
SBGlue_Select
SBGlue_SendToClient
SBGlue_ServerDoomed
SBGlue_Sleep
SBGlue_SMOpenContext
SBGlue_Submit
SBGlue_Subscribe
SBGlue_System
SBGlue_Throw
SBGlue_OpenLog
SBGlue_CloseLog
SBGlue_StartLogging
SBGlue_StopLogging
 */

/* Private methods for local use only.
NUM			createContext(const char* ipContextName, long iContextIndex, const char* ipScript, LpXCONTEXT* ipCP, long iMaxThreads,
			NUM iMaxMemory, long iObjHdrSize, POINTER ipSessionMgrContextThread);
char*		decodeUrl(char *ipBfr);
char*		encodeUrl(char* ipBfr, long iBfrSz);
const char*	errorMsg(NUM code);
long		errorNum(NUM code);
QByteArray	getDirInfo(LpXCONTEXT gCP, LpTHREAD gTP, char* ipCmdString);
bool		getEvalNotifyType(long notifyType, AEvalNotifyType *type);
bool		getEvalReturnType(long returnType, AEvalReturnType *type);
long		hexToInt(char iCh);
 */
#endif
