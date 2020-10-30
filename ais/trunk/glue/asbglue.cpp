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
aisdev/glue/asbglue.cpp
															Glue
This source file contains the functions necessary to marry the SmartBase engine with
the SessionManager. 

CHANGE HISTORY
Version	Date		Who		Change
5.0012  2/28/2013	mfk	    Added contextOpen, contextClose, contextSubmit, and other context mgmt funcions.
5.0010	6/2/2009	mfk	    Fixed close context to free memory with Microsoft virtual memory functions.
4.0003	9/28/2008	tlw	    CR128 Add subscribe to allow a connection to subscribe to a session.
4.0003	07/14/2008	fchua	Fix for CR-127.
2.0002	02/27/2006	jmo		added SBGlue_TcpClient.
2.0001	12/20/2006	tmay	added support for execute and binary transfer in contextClient
1.0120	12/19/2006	tlw		Eval. Add ipData argument to calls to cbReturnResult.
1.0119	12/18/2006	tlw		createBinaryBuffer, deleteBinaryBuffer. Add routines to SBGlue_ContextClient
1.0118	12/9/2006	tm		Added SBGLUE_ERR_BADBUFFERSIZE
1.0117	12/7/2006	tlw		SBGlue_ContextClient. Add read/write for inter-context binary transfer
1.0115	11/20/2006	tlw		SBGlue_Eval. Fix error in parse of prefix.
1.0114	11/13/2006	tlw		ALibs. Revise and document.  Replace ordinals with exported function names.
1.0112	10/27/2006	tlw		~ALibs. Reclaim all mLib entries on destruct.
1.0111	10/26/2006	tlw		SBGlue_Submit. Set default date/time to current time.
1.0110	10/20/2006	tlw		ContextClient. Move AContextClient to main thread.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ---------------
NOTES
SBGlue is the "glue" between the two components of the system. It provides the following:

- Call interface to the API of the engine where conversions from generic C types to TVALs are performed. The idea is to isolate
 all engine type conversion in this file. No Engine types should appear in the SessionManager and the SessionManager should not
 include the fsmtbase.h file.

- Callback interface for SmartBase engine callbacks. These functions convert data types etc. and then make calls on the
 SessionManager as necessary. If the function can be fully implemented in this file we do so. The callbacks are functions so
 that they can be easily registered with the engine. It would be difficult to register class methods with the engine because of
 C++'s approach to pointers to class members. 

- Utility functions. Functions to be used by other functions in this file and by the Session Manager. These functions must only
 use generic C types and can not pass or return structures. If you find this too hard to live with then write a separate
 utility function for the SessionManager and SBGlue. Make no Smartbase calls in a utility function that is to be called by the
 SessionManager.

- Local functions. Functions to be used only by functions in this file that were called by the engine. 

Note that only those functions called by the SessionManager should appear in SBGlue.h. The other functions are meant to be
local to this file or dynamcially registered with the engine.

NOTES:

Callbacks:
This file provides callback method entry points for the smartbase engine. Some of these functions then make calls to the
Session Manager. All core functions that must be registered with the engine are found in this file or the sysglue.c file. When
we can handle all of the required fucntionality in this file we do it. Many callback functions require that we call a companion
function in the SessionManager. There are also functions that are here only to allow depreciated, obsolete or irrelevent calls,
that might exist in Lambdas, to be trapped for error processing or benign return.


Qt:
This file can contain only those Qt utility classes that do not rely on the the GUI thread.
These include any classes that inherit:
QWidget
QTimer
QSocketNotifier
QNetwork

Qt Library Requirements
Note that this module must be linked with a version of the Qt library compiled with 4 byte structure alignment.

Portability:
This module is currently windows specific in that it includes windows.h and uses a few window calls. These are pretty
rudimentary calls and should be easily portable to another system. The windows calls currently used include:
	Sleep()

***************** Quick Cheat Notes ****************
QString -> TVAL
QString sQString("test");
*tstr = FSmartbase_CnvFromText(gCP,gTP, (const char*)apStr);

TVAL -> QString
char *strptr;
long  apStrlen;
*aResult = FSmartbase_Convert(gCP,gTP,TYSTRING,*somevalue);
FSmartbase_StringPtr(gCP,gTP,aResult,&strptr,&apStrlen);
QString apStr(strptr);
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#ifdef _LINUX
#include "sys/time.h"
#include <sys/mman.h>
#include <stdlib.h>
#include <unistd.h>
#endif
#ifdef _MSVC
#ifdef _M64
#include <windows.h>
#endif
#endif
#include <assert.h>
#include <time.h>
#include <stdlib.h>
#include <QtCore/QString>
#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtCore/QVariant>
#include <QtCore/QLibrary>
#include <QtCore/QMutex>
#include <QtXml/QtXml>
#include <QtCore/QThread>
// #include <lm.h> was here for the getMachineName function - remove later unless we have to revive getMachineName
#include "axml.h"

#include "../afilewatcher/afilewatcher.h"
#include "asessionmgr.h"
#include "acontextthread.h"
#include "acontextclient.h"
#include "atcpclient.h"

#include "../smtbase/tstring.h"
#include "../smtbase/tsymbol.h"

// Note: Macro to check if log time should be written to file.
//       Only write the log time if it is the new line flag is TRUE.
#define _WriteLogTime \
if(gCP->logTime == TRUE && gCP->logNewLineFlag == TRUE)\
{\
SBGlue_WriteTime(gCP,gTP);\
}


// Note: there are macro naming conflicts between Qt and FSmartbase.h. qt derived class headers must come first! Be careful
// about the macros you use in qt derived classes compiled in this file!  Known conflicts include: isNull(). 

#include "asbglue.h"		// Functions callable by SessionManager in SBGlue
#include "asysglue.h"		// SBGlue_Open calls SysGlue_Open

extern "C" { // includes for modules written in C
	#include "fsmtbase.h" // SmartBase engine declarations
	#include "../smtbase/tstruct.h"
	}

enum AClientResultType {geBooleanContextClientResultType,geNumberContextClientResultType,geStringContextClientResultType,
	geStructureContextClientResultType}; 

// Global variables
extern ASessionManager *gpSessionManager;
extern AFileWatcher *gpFileWatcher;

QMutex gContextMutex[SBGLUE_MAX_CONTEXTS];
struct AContextFlags {
int mInUse;
int mEscape;
int mInstructionTrace;
int mErrorTrace;
int mSystemCheck;
int mJit;
int mModified;
};

REAL gStartClock;
AContextFlags gContextFlags[SBGLUE_MAX_CONTEXTS];

// Local file class definitions - no need to make them external!
typedef QHash<QString, QLibrary*> ALibMap;
typedef const char* (*AGetLibNamePrototype)();

/*!
\brief ALIBS Load libraries dynamically

\par Shared Libraries
ALIBS dynamically loads a shared library on any supported platform.  The shared library must export two functions, __getLibName
and __registerLibFunctions.  See the xml C++ project for an example of a shared library meeting these requirements.  To test
a DLL (on a Windows platform) to make sure that it meets this requirement, use the command:
     dumpbin /EXPORTS myLib.dll
 For example, if "xml" is substituted for myLib, the dumpbin command yields:

\verbatim
C:/ais>dumpbin /EXPORTS xml.dll
Microsoft (R) COFF/PE Dumper Version 7.10.3077
Copyright (C) Microsoft Corporation.  All rights reserved.
Dump of file xml.dll
File Type: DLL
  Section contains the following exports for xml.dll
    00000000 characteristics
    4559CD35 time date stamp Tue Nov 14 06:05:41 2006
        0.00 version
           1 ordinal base
           2 number of functions
           2 number of names
    ordinal hint RVA      name
          1    0 0000101E __getLibName
          2    1 00001023 __registerLibFunctions
 \endverbatim

 \par DLL Location
 On a Windows platform, ALIBS begins its search for a DLL in the current directory and then searches the directories specified
 in the PATH environment variable for the DLL.  See the MSDN discussion "DLL Hell" for more details on the selection of a DLL.
One good place to put DLLs is in C:/ais/bin which is normally included in the PATH.

\par DLL Naming
Only the base name of the shared library is required.  The appropriate extension (.dll, .so, etc.) are automatically added to
the name based upon the current platform.

 */
class ALibs
{
public:
	ALibs(){};
	~ALibs();
	QLibrary*	addLib(QString iLibName);
	bool		delLib(QString iLibName);
	bool		loadLib(LpXCONTEXT gCP, LpTHREAD gTP, QString iLibName);
	bool		reloadLib(LpXCONTEXT gCP, LpTHREAD gTP, QString iLibName);

	ALibMap		mLibs;

private:
	QMutex		cMutex;
};

/*!
\brief Destructor Free allocated resources

\par Notes:
-# ALibs has remote ownership of QLibrary referents.
-# Since instances of ALibs are never assigned, copied or passed by value, the copy constructor and assignment operator
are omitted here.  See C++ FAQS chap 30 for more info.
 */
ALibs::~ALibs()
{
	ALibMap::const_iterator aIt, aEnd = mLibs.constEnd();
	for (aIt = mLibs.constBegin(); aIt != aEnd; ++aIt)
		delete aIt.value();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~ALibs()");
#endif
}

/*!
\brief addLib - Add a shared library to the mLibs map.

addLib depends upon the ability to extract and access the __getLibName function to retrieve the name and version number
of the shared library.  This name is used as a key in the mLibs map.
\param iLibName The path and base name of the shared library.  The platform-dependent extension is added automatically.
If no path is provided, the current directory and then the directories specified by the path environment variable are
searched.  The first match is used.
\returns apLib -> a new QLibrary for this shared library.
 */
QLibrary* ALibs::addLib(QString iLibName)
{
	cMutex.lock();
	QLibrary* apLib = new QLibrary(iLibName);
	// get apLib name and version to use as key
	AGetLibNamePrototype apGetLibName = (AGetLibNamePrototype)apLib->resolve("__getLibName");
	if (apGetLibName == NULL)
	{	QString aErr(apLib->errorString());
		qDebug("ALibs::addLib(%s) fails: %s", iLibName.toAscii().data(), aErr.toAscii().data());
		delete apLib;
		apLib = NULL;
	}
	else
	{	QString aLibName((*apGetLibName)());
		// See if apLib has already been loaded
		if (mLibs.contains(aLibName))
		{	delete apLib;		// Delete the new object because a library is already in mLibs
			apLib = mLibs.value(aLibName);
		}
		else	// Proceed with insert as library is not yet in mLibs
			mLibs.insert(aLibName, apLib);
	}
	cMutex.unlock();
	return apLib;
}

/*!
\brief loadLib - Fetches and runs an exported function named __registerLibFunctions.

The shared library must export a function named __registerLibFunctions that returns a pointer to a function that accepts
four arguments.  This function is called by loadLib to register one or more function with the SmartBase engine.  See
the C++ xml project for an example.
\param gCP  Pointer the context structure for this context.
\param gTp  Pointer to the thread variables for this context
\param iLibName The path and base name of the shared library.  The platform-dependent extension is added automatically.
If no path is provided, the current directory and then the directories specified by the path environment variable are
searched.  The first match is used.
\return aOk - true iff ALibs is able to resolve the __registerLibFunctions function for this shared library.
 */
bool ALibs::loadLib(LpXCONTEXT gCP, LpTHREAD gTP, QString iLibName)
{
	bool aOk = false;
	QLibrary *apLib = addLib(iLibName);
	LpFUNC apRegisterLibFunctions;
	if (apLib != NULL && (apRegisterLibFunctions = (LpFUNC)apLib->resolve("__registerLibFunctions")) != NULL)
	{	(*apRegisterLibFunctions)(gCP,gTP,0,NULL);
		aOk = true;
	}
	return aOk;
}

/*!
\brief reloadLib - Deletes the existing shared library, if any, and then reloads the shared library.

If a shared library has been modified, it is possible to update this library using reloadLib.  See loadLib for more detail
on the loading of a library.  See delLib for more information on removing a shared library.
\param gCP  Pointer the context structure for this context.
\param gTp  Pointer to the thread variables for this context
\param iLibName The path and base name of the shared library.  The platform-dependent extension is added automatically.
If no path is provided, the current directory and then the directories specified by the path environment variable are
searched.  The first match is used.
\return aOk - true iff ALibs is able to resolve the __registerLibFunctions function for this shared library.
 */
bool ALibs::reloadLib(LpXCONTEXT gCP, LpTHREAD gTP, QString iLibName)
{
	delLib(iLibName);
	return(loadLib(gCP, gTP, iLibName));
}

/*!
\brief delLib - Remove a library that was previously loaded.

addLib depends upon the ability to extract and access the __getLibName function to retrieve the name and version number
of the shared library.  This name is used as a key in the mLibs map.  If __getLibName is resolved and if the key is found
in mLibs, the library is removed.
\param iLibName The path and base name of the shared library.  The platform-dependent extension is added automatically.
If no path is provided, the current directory and then the directories specified by the path environment variable are
searched.  The first match is used.
\return aOk - True iff the shared library was, in fact, previously loaded.
 */
bool ALibs::delLib(QString iLibName)
{
	bool aOk = false;
	QLibrary aLib(iLibName);
	cMutex.lock();
	// Get shared library name and version to use as key into the mLibs map.
	AGetLibNamePrototype apGetLibName = (AGetLibNamePrototype)aLib.resolve("__getLibName");
	QString aLibName((*apGetLibName)());
	// If this library has been loaded, remove it from the map and reclaim the allocated storage for this instance of QLibrary.
	if (mLibs.contains(aLibName))
	{	delete mLibs.take(aLibName);
		aOk = true;
	}
	cMutex.unlock();
	return aOk;
}
// file scope globals holding callback pointers. See SBGlue_Init().
//ApSMDisplay gcpSMDisplay;
//ApSMReturnResult gcpSMReturnResult;
//ApSMError	gcpSMError;
//ApSMServerFileOpenDialog	gcpSMServerFileOpenDialog;
//ApSMRingBell gcpSMRingBell;
//ApSMDebug gcpSMDebug;
//ApSMFlags gcpSMFlags;
//ApSMReadHtmlPage gcpSMReadHtmlPage;
//ApSMClearHtmlPage gcpSMClearHtmlPage;
//ApSMcbOpenContext gcpSMcbOpenContext;
//ApSMRequestHttp gcpSMRequestHttp;
//ApSMClearRequestHttp gcpSMClearRequestHttp;
//ApSMSendToClient gcpSMSendToClient;
//ApSMEnableConsoleLog gcpSMEnableConsoleLog;
//ApSMGetConsoleLog gcpSMGetConsoleLog;
//ApSMOpenConsoleLog gcpSMOpenConsoleLog;
//ApSMSendEngineStateToSessions gcpSMSendEngineStateToSessions;


// File scope global holding dynamic library objects. Note
// that shared libraries and dlls are loaded once on startup
// and each libraries functions are registered when each
// context is created.

ALibs DynamicLibs; // create object to hold list of dynamic libraries loaded by applications


/******************************************************************************************************************************
Forward Declarations
The following declarations are necessary for LOCAL functions called only inside this file. DO NOT ADD LOCAL FUNCTIONS TO
SBGlue.h  as they usually have references to types defined in fsmtbase.h.
******************************************************************/
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



/********************************************* PUBLIC FUNCTIONS ***************************************************************
The following functions represent the external interface of the SBGlue file. These functions are primarily called by the
SessionManager.  These functions can only use standard C types as the fsmtbase.h file cannot be included in the calling
program (e.g. SessionManager).
******************************************************************************************************************************/
/*-----------------------------------------------------------------
SBGlue_CalculateStackSpace is called by the host application to 
determine how much 'C' stack space must be allocated for the 
thread the context is running on.
------------------------------------------------------------------*/
long SBGlue_CalculateStackSpace(long iMemorySize)
{
	return(FSmartbase_CalculateStackSpace(iMemorySize));
}

/*-----------------------------------------------------------------
SBGlue_FlushLispCodeInContext

Description:	Flush the Lisp code in the specified context and
				release all resources.

Note:			Only the Lisp code and Lisp resources are freed.
				The middleware QT context and session objects must
				still be closed in C++/QT.

Args:	ipContext	The pointer to the Lisp Context block (gCP)
Return:	retCode		The sbglue return code

----------------------------------------------------------------*/
long SBGlue_FlushLispCodeInContext(void* ipContext)
{
	LpXCONTEXT	gCP;
	LpTHREAD	gTP;
	NUM			aCatchCode;
	char		aErrorFileName[1024];
	char*		apStrptr;
	NUM			aStrlen;
	POINTER		apAllocBlocks[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
	NUM			aAllocBlockSize[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];

	if ((gCP = (LpXCONTEXT)ipContext) == NULL)
			return(SBGLUE_SUCCESS);
	gTP = (LpTHREAD)&gCP->ThreadBlocks[0]; // Only using a single thread block at this time
	StartFrame
	DeclareTVAL(aResult);
	EndFrame

	// Generate a full path to an error file in case we need to write an error out
	*aResult = FSmartbase_Ref(gCP,gTP,1, TGVALUE("_path") );
	FSmartbase_StringPtr(gCP,gTP,aResult,&apStrptr,&aStrlen);
	if (aStrlen < 1000) {
		strcpy(aErrorFileName,apStrptr);
		strcpy(aErrorFileName + aStrlen,"ShutDownError.err");
	}
	else strcpy(aErrorFileName,"ShutDownError.err"); // just use a relative name

	// Setup the setjmp since smartbase uses longjump to throw an error.
	_FSmartbase_ECatch(gCP,/*out*/aCatchCode, LBLSystemThrowError);

	// Close all files opened by context
	FSmartbase_Evals(gCP,gTP,(LpCHAR)"(clear)",FALSE);

	// Force garbage collection for the context.
 	FSmartbase_MarkAndSweep(gCP,gTP);

	// Close MySQL connections
	FSmartbase_Evals(gCP,gTP,(LpCHAR)"(sqlend)",FALSE);

	// Reopen the slot the context occupies in gContextFlags
	for(long i=0; i<60; ++i)
	{	if (gContextMutex[gCP->ContextIndex].tryLock())
			goto gotLock;
		AContextThread* sessionMgrContextThread = (AContextThread*) gCP->SessionMgrContextThread;
		sessionMgrContextThread->localSleep(1L/*Msec*/);
	}
	// If we fell through then we failed to get a lock on the mutex so fail!
	goto LBLSystemThrowError;

gotLock:
	gContextFlags[gCP->ContextIndex].mInUse = -1; // Open slots are identified by a negative one value
	gContextMutex[gCP->ContextIndex].unlock();

	// Free the memory for the context
	// Copy the apAllocBlocks for the context so we can free them
	for(long i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i)
	{
		apAllocBlocks[i] = gCP->ContextBlocks[i];
		aAllocBlockSize[i] = gCP->ContextBlockSize[i];
	}

	for(long i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i)
	{
		if (apAllocBlocks[i] != NULL)
		{
#if defined(_MSVC) && defined(_M64)
			VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_DECOMMIT);
			VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_RELEASE);
#else
			free(apAllocBlocks[i]);
#endif
		}
		// FCC: gCP points to the first block. After deallocation, gCP will be garbage
		//gCP->ContextBlocks[i] = NULL;
		//gCP->ContextBlockSize[i] = 0;
	}

	return(SBGLUE_SUCCESS);

LBLSystemThrowError:
	// Write error to an errorFile so it can be reported during next launch of context
	// We can not report this error in the normal logs by calling back into AIS
	// as AIS may be shutting down when SBGlue_FlushLispCodeInContext was called.

	///// Deferred until we get the new OS file layer in place
	///// It is difficult to use asysglue functions here because the context
	///// is partially toasted.

	// Free the memory for the context
	// Copy the apAllocBlocks for the context so we can free them
	for(long i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i)
		apAllocBlocks[i] = gCP->ContextBlocks[i];

	for(long i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i)
		if (apAllocBlocks[i] != NULL)
		{
#if defined(_MSVC) && defined(_M64)
			VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_DECOMMIT);
			VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_RELEASE);
#else
			free(apAllocBlocks[i]);
#endif
		}
	return SBGLUE_EVAL_FAILURE;
}

/*-----------------------------------------------------------------
SBGlue_Eval
This function executes a string in the engine and then returns the 
string aResult of the execution back to the calling program
by calling SM_ReturnResult. 
Arguments
	ipCP			- pointer to context structure
	iSessionID	- session making request
	iUserID		- user making request
	iLevel		- aSecurity level of user making request
	iEvalType- determines the type of evaluation and return processing performed
	iNoDebug	- 1=run without user specified debug flags 0=run with user specified debug flags
	iSetJmp		- 1=set catch environment
	ipCmdString	- command string to process. Form is determined by iEvalType.
	ipData		- binary data buffer containing object closure passed in from another context.
Returns
	suspendState - 0 for normal exit
				   1 for suspended request. See suspendRequest function.
------------------------------------------------------------------*/
long SBGlue_Eval(void *ipCP, long iSessionID, long aRequestID, long iUserID, long iLevel,long iEvalType, long iNoDebug,long iSetJmp, long iJitMode, char* ipCmdString, char* ipData)
{
	ASession*	apSession;
	char*		aAmpCmd = NULL;
	LpXCONTEXT	gCP;
	LpTHREAD	gTP;
	NUM 		aCatchCode;
	NUM			aBufLen;
    char*       apBuf = NULL;
	NUM			aLen;
	NUM			aBufPos;
	NUM			aIx;
	NUM			aE;
	NUM			aTokenStart;
	char*		apName;
	NUM			aNameLen;
	char*		apData = NULL;
	NUM			aDataSize = 0;
	char*		apError;
	NUM			aErrorLen;
	char*		apURL;
	NUM			aUrlLen;
	char*       apENCTYPE;
	NUM			aEnctypeLen;
	char*		apMimeText;
	NUM			aMimeTextLen = 0;
	QByteArray	aRetStr;
	int			aCmdStringLen;
	char		aErrorStringBuffer[2048];
	int			aSecurity = 0;
	int			aCallingConvention = 0;
	int			aCurrentState;
	int			aScanlen = 0;
	NUM			aReturntype;

#define SBRESULTBUFSIZE 4096

	// Note that the StartFrame macro uses gCP and gTP
	gCP = (LpXCONTEXT)ipCP;
	gTP = (LpTHREAD)&gCP->ThreadBlocks[0]; // Only using a single thread block at this time
	StartFrame
	DeclareTVAL(aCmdStringToVector);
	DeclareTVAL(aCmdObjectToStructure);
	DeclareTVAL(aCmdIsLambda);
	DeclareTVAL(aCmdNew);
	DeclareTVAL(aCmdAppend);
	DeclareTVAL(aCmdMember);
	DeclareTVAL(aCmdAppendWriteln);
	DeclareTVAL(aCmdResize);
	DeclareTVAL(aCmdCheckin);
	DeclareTVAL(aCmdBrowseLambda);
	DeclareTVAL(aSymCheckin);
	DeclareTVAL(aCmdDataMineLambda);
	DeclareTVAL(aCmdLoadObject);
	DeclareTVAL(aSymCompileLambda);
	DeclareTVAL(aSymStructure);
	DeclareTVAL(aSymVector);
	DeclareTVAL(aSymByte);
	DeclareTVAL(aSymPv);
	DeclareTVAL(aResult);        
	DeclareTVAL(aResult2);        
	DeclareTVAL(aRuboutChr);
	DeclareTVAL(aTabChr);
	DeclareTVAL(aMsgBlockString);
	DeclareTVAL(aMsgBlockVector);
	DeclareTVAL(aMsgBlockStructure);
	DeclareTVAL(aBlockAnnotations);
	DeclareTVAL(aLambdaName);
	DeclareTVAL(aName);
	DeclareTVAL(aValue);
	DeclareTVAL(aExtent);
	DeclareTVAL(aLambdaText);
	DeclareTVAL(aPv);
	DeclareTVAL(aSymColCount);
	DeclareTVAL(aSymRecordCount);
	DeclareTVAL(aSymGetColumnHeaders);
	DeclareTVAL(aSymRun);
	DeclareTVAL(aLambda);
	DeclareTVAL(aIndex);
	DeclareTVAL(aErrorvalue);
	DeclareTVAL(aEncvalue);
	DeclareTVAL(aSpeechAct);
	DeclareTVAL(aFacesStruct);
	DeclareTVAL(aSymIn);
	DeclareTVAL(aSymAmp);
	DeclareTVAL(aSymPublic);
	DeclareTVAL(aSymSecurity);
	DeclareTVAL(apBinBuf);
	DeclareTVAL(aBinBufSize);
	DeclareTVAL(aCmdSizeof);
	DeclareTVAL(aCmdSaveObject);
	DeclareTVAL(aCmdRemove);
	DeclareTVAL(aCmdDelete);
	DeclareTVAL(aBinTransfer);
	EndFrame

	gTP->SessionID = iSessionID; // Set sessionID so that all callbacks from the engine have
								// access to it. Each callback into SessionManager from SBGlue
								// passes sessionID so that SessionManager can match up the 
								// incoming callback with the correct context and session.
	gTP->RequestID = aRequestID;

	// Get session object pointer
	apSession = gpSessionManager->getSession(iSessionID);
	if (apSession == NULL)
		FrameExit(SBGLUE_EVAL_FAILURE);

	// Reset the session evaluation result before evaluation.
	apSession->mReturnString = "...please wait...";
	apSession->mReturnAvailableSW = FALSE;
	apSession->mDsplyBfr = "";

	// We use the setjmp setup since SmartBase uses Throw (longjmp)
	// (this uses the one and only catch environment)
	// Note that we check to make sure we are not in a debug state where we are processing
	// a new request nested inside a previous call to SBGlue_Eval. For example, we could have
	// called SBGlue_Eval and then had the engine call SBGlue_DebugDialog which might then 
	// (depending on the client side implementation SBGlue_DebugDialog calls) make another
	// SBGlue_Eval call. Without this check the second call would reset the throw environment 
	// and if the original SBGlue_Eval processing was interrupted by an escape condition the 
	// longjmp would end up with an obsolete stack environment.
	if (iSetJmp == 1)
		_FSmartbase_ECatch(gCP,/*out*/aCatchCode, LBLSystemThrowError);

	/* Initialize TVALS used in following calls to the engine */
	*aCmdStringToVector = TGVALUE("stringToVector");
	*aCmdObjectToStructure = TGVALUE("objectToStructure");
	*aCmdIsLambda = TGVALUE("isLambda");
	*aCmdAppend = TGVALUE("append");
	*aCmdNew = TGVALUE("new");
	*aCmdMember = TGVALUE("member");
	*aCmdBrowseLambda= TGVALUE("browseLib");
	*aCmdDataMineLambda = TGVALUE("dataMineLib");
	*aCmdAppendWriteln = TGVALUE("appendWriteln");
	*aCmdResize = TGVALUE("resize");
	*aCmdSaveObject = TGVALUE("saveObject");
	*aCmdSizeof = TGVALUE("sizeof");
	*aCmdLoadObject = TGVALUE("loadObject");
	*aSymCheckin = TSYMBOL("checkin");
	*aSymCompileLambda = TSYMBOL("compileLambda");
	*aSymPv = TSYMBOL("Pv");
	*aSymStructure = TSYMBOL("Structure");
	*aSymVector = TSYMBOL("Vector");
	*aSymByte = TSYMBOL("byte");
	*aSymRecordCount = TSYMBOL("aRecordCount");
	*aSymColCount = TSYMBOL("aColCount");
	*aSymGetColumnHeaders = TSYMBOL("getColumnHeaders");
	*aSymRun = TSYMBOL("run");
	*aRuboutChr = TCHAR(127);
	*aTabChr = TCHAR(9);
	*aSymIn = TSYMBOL("In");
	*aSymAmp = TSYMBOL("Amp");
	*aSymPublic = TSYMBOL("public");
	*aSymSecurity = TSYMBOL("aSecurity");
	aCmdStringLen = (int)strlen(ipCmdString);
	*aCmdRemove = TGVALUE("remove");
	*aCmdDelete = TGVALUE("delete");
	
	// Set up engine execution environment to match session's preferences and type of execution
	gTP->DebugSuspended = (iNoDebug == 1) // Ignore debug and instruction trace engine flags when true
		? TRUE 
		: FALSE; 

	// NOTE: if DebugSuspended is true then all debug engine flags will be ignored. 
	if (gTP->DebugSuspended == FALSE) {					// The jit mode is normally SBGLUE_JITON (0x10).
		aCurrentState = FSmartbase_SetEngineFlags(gCP,gTP, iJitMode);
		// gpSessionManager->cbSendEngineStateToSessions(gTP->SessionID,aCurrentState);
		gTP->EngineStateChanged = FALSE;
	} 
	switch(iEvalType)
	{
	case SBGLUE_EVENTCMD:	// provide no return aResult
	case SBGLUE_CMDSTRING:	// provide return aResult
	case SBGLUE_CMDSTRING_BINTRANSFER: // provide return aResult and binary data, see execute.
									 // also handle incomming binary data, see execute
		if (iLevel < SYSGLUESECLEVEL)
		{	gpSessionManager->cbReturnResult(gTP->SessionID, gTP->RequestID, errorMsg(errorNum(SBGLUE_ERR_EVAL_SECURITY))
			, "text/plain", NULL, NULL/*Data*/, 0/*DataSize*/);
			FrameExit(SBGLUE_ERR_EVAL_SECURITY);
		}
		// Check if command string contains %_%. This token indicates an amp message is included in the string.
		// Long ago, amp messages started with %. Then, a prefix was added, so now (debug ...)%_%(whatever...)
		aScanlen = aCmdStringLen - 3;
		aAmpCmd = 0;
		for (aIx=0; aIx < aScanlen; ++aIx)
		{	if (ipCmdString[aIx] == '%' && ipCmdString[aIx+1] == '_' && ipCmdString[aIx+2] == '%')
			{	ipCmdString[aIx] = 0; // null terminate the prefix portion of the string
				aAmpCmd = ipCmdString + aIx + 3; // ipCmdString follows prefix
				break;
			}
		}
		// Debug. If a debug prefix, run it separately.
		if (ipCmdString[0] == '(' && ipCmdString[1] == 'd' && ipCmdString[2] == 'e' && ipCmdString[3] == 'b' && ipCmdString[4] == 'u'
		&& ipCmdString[5] == 'g')
		{	for (aLen = 1, aIx = 6; aIx < aCmdStringLen && aLen > 0; ++aIx)
			{	if ((aE = ipCmdString[aIx]) == '(')
					++aLen;
				else if (aE == ')')
					--aLen;
			}
			if (aLen == 0)
			{	// Evaluate the prefix Lambda separately.
				aE = ipCmdString[aIx];
				ipCmdString[aIx] = '\0';
				*aResult = FSmartbase_Evals(gCP, gTP, ipCmdString, FALSE);
				if (aResult->Tag == TYERROR)
					break;
				ipCmdString[aIx] = aE;
			}
			else
				aIx = 0;

			// Evaluate everything following the prefix.
			if (aIx < aCmdStringLen)
			{
				// Save the session evaluation result after evaluation.
				*aResult = FSmartbase_Evals(gCP, gTP, &ipCmdString[aIx], FALSE);
				if (aResult->Tag != TYERROR)
				{
					*aResult = FSmartbase_Convert(gCP,gTP,TYSTRING,*aResult);
					FSmartbase_StringPtr(gCP,gTP,aResult,&apBuf,&aBufLen);
				} 
				else
				{
					apBuf = (char *)*aResult->u.Error->itsCString;
				}
				apSession->mReturnString = apBuf;
				apSession->mReturnAvailableSW = TRUE;
			}
		}
		else
		{	
			if (iEvalType == SBGLUE_CMDSTRING_BINTRANSFER && ipData != NULL)
			{ // Handle incomming binary data transfers
				//Instantiate the incomming binary data and assign it to an automatic variable.
				//If the data is associated with an execute command we will later assign this
				//data to the _buffer_ global variable.
				//If the data is associated with an executeAmp command, then this data is the
				//amp message structure and will be used in the amp message handling later on.
				*aBinTransfer = FSmartbase_Eval(gCP,gTP,*aCmdLoadObject,1,TPOINTER(ipData));

				//Check for error from LoadObject here

				// Delete data buffer
				free(ipData);
				ipData = NULL;
				
				//Set the result as the value of the _buffer_ global variable
				if (aAmpCmd == 0)
					*aBinTransfer = FSmartbase_SetSymbolValue(gCP,gTP,(LpCHAR)"_buffer_",*aBinTransfer);

			}

			// Evaluate the ipCmdString portion of the message in the context
			if (strlen(ipCmdString) > 0)
				*aResult = FSmartbase_Evals(gCP,gTP,&ipCmdString[0],FALSE);
			else
				*aResult = TBOOL(true);

			// Save the session evaluation result after evaluation.
			if (aResult->Tag != TYERROR)
			{
				*aResult = FSmartbase_Convert(gCP,gTP,TYSTRING,*aResult);
				FSmartbase_StringPtr(gCP,gTP,aResult,&apBuf,&aBufLen);
			} 
			else
			{
				apBuf = (char *)*aResult->u.Error->itsCString;
			}
			apSession->mReturnString = apBuf;
			apSession->mReturnAvailableSW = TRUE;


			// Set _buffer_ to #void - don't leave a reference to the transfered data lying around!
			if (aAmpCmd == 0 && iEvalType == SBGLUE_CMDSTRING_BINTRANSFER && ipData != NULL)
			{	*aResult2 = FSmartbase_SetSymbolValue(gCP,gTP,(LpCHAR)"_buffer_",gCP->Tval_VOID);
			}
		}
		if (aResult->Tag == TYERROR)
			break;

		// Jump to the amp processing for the amp portion of the message if any
		if (aAmpCmd != 0)
		{	ipCmdString = aAmpCmd;
			goto MESSAGE_BLOCK_ENTRY; // now process the amp message
		}
		if (iEvalType == SBGLUE_EVENTCMD) // Skip sending return aResult if this is an event
			FrameExit(SBGLUE_EVAL_SUCCESS);

		if (iEvalType == SBGLUE_CMDSTRING_BINTRANSFER)
		{ // Handle outgoing binary data transfers
			if (aResult->Tag == TYVOID)
			{	apBuf = const_cast<char*>("#void");
				gpSessionManager->cbReturnResult(gTP->SessionID,gTP->RequestID, NULL, "text/plain", apBuf, NULL/*Data*/,0/*Size*/);
				FrameExit(SBGLUE_EVAL_SUCCESS);
			}
			else
			{	*aBinBufSize = FSmartbase_Eval(gCP,gTP,*aCmdSizeof,1,*aResult);
				apBinBuf->Tag = TYPOINTER;
				apBinBuf->u.Pointer = (POINTER)malloc((size_t)asNumIndex(aBinBufSize));
				if (apBinBuf->u.Pointer == NULL)
				{ //Need some error handling here
					gpSessionManager->cbReturnResult(gTP->SessionID,gTP->RequestID, "!Error:execute: could not allocated binary transfer buffer!", "text/plain", apBuf, NULL/*Data*/,0/*Size*/);
					FrameExit(SBGLUE_EVAL_SUCCESS);
				}
				else
				{	// put object closure record into binary buffer
					*aResult = FSmartbase_Eval(gCP,gTP,*aCmdSaveObject,3,*aResult,*aBinBufSize,*apBinBuf);
					gpSessionManager->cbReturnResult(gTP->SessionID,gTP->RequestID, NULL, "text/plain", "", apBinBuf->u.Pointer,aBinBufSize->u.Int);
					FrameExit(SBGLUE_EVAL_SUCCESS);
				}
			}
		}
		else
		{	// Convert eval return value to a string representation
			// This utilizes the engines heap!
			// The rendering of a data type as a string depends upon the context, so code is
			// inserted on an ad hoc basis to select the conversion since FSmartbase_Convert is
			// not context-aware.
			if (aResult->Tag == TYVOID)
			{	apBuf = const_cast<char*>("#void");
				gpSessionManager->cbReturnResult(gTP->SessionID,gTP->RequestID, NULL, "text/plain", apBuf, NULL/*Data*/,0/*Size*/);
				FrameExit(SBGLUE_EVAL_SUCCESS);
			}
			else
			{	*aResult = FSmartbase_Convert(gCP,gTP,TYSTRING,*aResult);
				if (aResult->Tag != TYERROR)
				{
					// Save the session evaluation result after evaluation.
					*aResult = FSmartbase_Convert(gCP,gTP,TYSTRING,*aResult);
					FSmartbase_StringPtr(gCP,gTP,aResult,&apBuf,&aBufLen);
					apSession->mReturnString = apBuf;
					apSession->mReturnAvailableSW = TRUE;
					// Make callbacks to return string return values. Note that this
					// should aResult in a COPY of the string value so later when 
					// we exit this routine we can let the engine garbage collect the 
					// source TVAL.
					gpSessionManager->cbReturnResult(gTP->SessionID,gTP->RequestID, NULL, "text/plain", apBuf, NULL,0/*DataSize*/);
					FrameExit(SBGLUE_EVAL_SUCCESS);
				}
			}
		}

		break;

		case SBGLUE_DIRINFO: 
			// Make sure the message has a _context target Lambda
			aCmdStringLen = (int)strlen(ipCmdString);

			if (iLevel < SYSGLUESECLEVEL)
			{	gpSessionManager->cbReturnResult(gTP->SessionID, gTP->RequestID, errorMsg(errorNum(SBGLUE_ERR_EVAL_SECURITY))
				, "text/plain", NULL, NULL/*Data*/, 0/*DataSize*/);
				FrameExit(SBGLUE_ERR_EVAL_SECURITY);
			}
			aRetStr = getDirInfo(gCP, gTP, ipCmdString);
			apBuf = aRetStr.data();
			aBufLen = aRetStr.length();
			gpSessionManager->cbReturnResult(gTP->SessionID,gTP->RequestID, NULL, "text/plain", apBuf, NULL/*Data*/,0/*Size*/);
			FrameExit(SBGLUE_EVAL_SUCCESS);

			// Create a aResult set message
			if (aResult->Tag != TYERROR)
			{	// Convert eval return value to a string representation
				// This utilizes the engines heap!
				*aResult = FSmartbase_Convert(gCP,gTP,TYSTRING,*aResult);
				if (aResult->Tag != TYERROR)
				{	FSmartbase_StringPtr(gCP,gTP,aResult,&apBuf,&aBufLen);
					// Make callbacks to return string return values. Note that this
					// should aResult in a COPY of the string value so later when 
					// we exit this routine we can let the engine garbage collect the 
					// source TVAL.
					gpSessionManager->cbReturnResult(iSessionID,aRequestID, NULL, "text/plain", apBuf, NULL/*Data*/,0/*Size*/);
					FrameExit(SBGLUE_EVAL_SUCCESS);
				}
			}
			break;

		case SBGLUE_CHECKIN:
			if (iLevel < SYSGLUESECLEVEL)
			{	gpSessionManager->cbReturnResult(iSessionID, aRequestID, errorMsg(errorNum(SBGLUE_ERR_EVAL_SECURITY))
				,"text/plain", NULL, NULL/*Data*/, 0/*DataSize*/);
				FrameExit(SBGLUE_ERR_EVAL_SECURITY);
			}
			// Extract Extent Name
			for (aIx=0; aIx < aCmdStringLen && ipCmdString[aIx] != '\177'; aIx++)
				; // find next del
			if (aIx == aCmdStringLen)
			{	gpSessionManager->cbReturnResult(iSessionID, aRequestID, errorMsg(errorNum(SBGLUE_ERR_EVAL_BAD_CHECKIN))
				,"text/plain", NULL, NULL/*Data*/, 0/*DataSize*/);
				FrameExit(SBGLUE_ERR_EVAL_BAD_CHECKIN);
			}
			ipCmdString[aIx++] = '\0'; // replace del with null and advance
			*aExtent = TSTRING(&ipCmdString[0]);

			// Extract Lambda Name
			aTokenStart = aIx; // save starting char pos of Lambda name
			for(; aIx < aCmdStringLen && ipCmdString[aIx] != 127; aIx++)
				; // find next del
			if (aIx == aCmdStringLen)
			{	gpSessionManager->cbReturnResult(iSessionID, aRequestID, errorMsg(errorNum(SBGLUE_ERR_EVAL_BAD_CHECKIN))
				, "text/plain", NULL, NULL, 0/*DataSize*/);
				FrameExit(SBGLUE_ERR_EVAL_BAD_CHECKIN);
			}
			ipCmdString[aIx++] = '\0'; // replace del with null and advance
			*aLambdaName = TSTRING(&ipCmdString[aTokenStart]);
			if (aIx == aCmdStringLen)
			{	gpSessionManager->cbReturnResult(gTP->SessionID, gTP->RequestID, errorMsg(errorNum(SBGLUE_ERR_EVAL_BAD_CHECKIN))
				, "text/plain", NULL, NULL/*Data*/, 0/*DataSize*/);
				FrameExit(SBGLUE_ERR_EVAL_BAD_CHECKIN);
			}
			// Get the browseLambda.checkin Lambda reference
			*aLambdaText = TSTRING(&ipCmdString[aIx]);
			*aPv = FSmartbase_Ref(gCP,gTP,2,*aCmdBrowseLambda,*aSymPv);
			*aCmdCheckin = FSmartbase_Ref(gCP,gTP,2,*aPv,*aSymCheckin);

			// Call the browseLambda.checkin Lambda
			*aResult = FSmartbase_Eval(gCP,gTP,*aCmdCheckin,3,
					*aExtent,*aLambdaName,*aLambdaText);

			// Create a aResult set message
			if (aResult->Tag != TYERROR)
			{	// Convert eval return value to a string representation
				// This utilizes the engines heap!
				*aResult = FSmartbase_Convert(gCP,gTP,TYSTRING,*aResult);
				if (aResult->Tag != TYERROR)
				{	FSmartbase_StringPtr(gCP,gTP,aResult,&apBuf,&aBufLen);
					// Make callbacks to return string return values. Note that this
					// should aResult in a COPY of the string value so later when 
					// we exit this routine we can let the engine garbage collect the 
					// source TVAL.
					gpSessionManager->cbReturnResult(gTP->SessionID, gTP->RequestID, NULL, "text/plain", apBuf, NULL, 0);
				}
			}
			break;

		case SBGLUE_MSGBLOCK: // Amp Message
		case SBGLUE_EVENTMSG: // Amp message but no return aResult generated
MESSAGE_BLOCK_ENTRY:
			// Process a message block and execute the specified
			// target Lambda. The message block is a sequence of
			// del delimited name value pairs. The first name
			// value pair contains the target Lambda and speech
			// act.
			// If we have received a executeAmp messasge the amp messaage is
			// already  available as a structure in aBinTransfer set earlier.


			//----------- Parse the message block ------------
			*aMsgBlockString = TSTRING(ipCmdString);

			*aMsgBlockVector = FSmartbase_Eval(gCP,gTP,*aCmdStringToVector,4,*aMsgBlockString,*aRuboutChr,TBOOL(true),TBOOL(true));
			// Build message block vector 
			aLen = FSmartbase_VectorLen(gCP,gTP,*aMsgBlockVector);
			for (aIx=0; aIx < aLen; aIx+=2)
			{	*aName = FSmartbase_Ref(gCP,gTP,2,*aMsgBlockVector,TINT(aIx));
				FSmartbase_StringPtr(gCP,gTP,aName,&apBuf,&aBufLen);
				if (aIx == 0) // Check if target Lambda and speech act are valid
				{	// (setq aResult (isLambda aLambdaName))
					*aLambdaName = FSmartbase_Ref(gCP,gTP,2,*aMsgBlockVector,TINT(aIx));
					FSmartbase_StringPtr(gCP,gTP,aLambdaName,&apBuf,&aBufLen);
					*aLambda= TGVALUE(apBuf);
					// return error if not an Lambda
					if (aLambda->Tag != TYLAMBDA)
					{	*aResult = TERROR((LpCHAR)"!Target Lambda does not exist!");
						break;
					}
					// All Lambdas have a faces, In:, structure
					*aFacesStruct = FSmartbase_Ref(gCP,gTP,2,*aLambda,*aSymIn);

					// Convert name portion to a symbol for message structure
					*aResult = FSmartbase_Set(gCP,gTP,3,*aMsgBlockVector,TINT(aIx),TSYMBOL(apBuf));
					if (aResult->Tag == TYERROR)
					{	*aResult = TERROR((LpCHAR)"!AMP structure name could not be converted to symbol");
						break;
					}
					// Get aSecurity level of target Lambda. Note that the aSecurity level of the
					// target Lambda is the default aSecurity level of all speech acts.
					aSecurity = 1; // Default to lowest aSecurity level
					// (setq aResult (member aFacesStruct aSymSecurity:))
					*aResult = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,*aSymSecurity,*aFacesStruct);
					if (aResult->Tag == TYNUM)
					{	// (setq aResult aFacesStruct[aResult 1])
						*aResult = FSmartbase_Ref(gCP,gTP,3,*aFacesStruct,*aResult,TINT(1));
						if (aResult->Tag == TYNUM)
							aSecurity = asInt(aResult);
					}
					// Check if execution of target Lambda is requested. ie. speech act = *
					*aSpeechAct = FSmartbase_Ref(gCP, gTP, 2, *aMsgBlockVector, TINT(aIx + 1));
					FSmartbase_StringPtr(gCP,gTP,aSpeechAct,&apBuf,&aBufLen);
					if (apBuf[0] != '*')
					{	*aPv = FSmartbase_Ref(gCP, gTP, 2, *aLambda, *aSymPv);
						*aResult = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,*aSpeechAct,*aPv);
						if (aResult->Tag != TYNUM)
						{	*aResult = TERROR((LpCHAR*)"!Requested Lambda does not exist!");
							break;
						}
						*aLambda = FSmartbase_Ref(gCP,gTP,3,*aPv,*aResult,TINT(1));
						if (aLambda->Tag != TYLAMBDA)
						{	*aResult = TERROR((LpCHAR)"!Requested Lambda does not exist!");
							break;
						}
						// Get the faces structure for the Lambda
						*aFacesStruct = FSmartbase_Ref(gCP,gTP,2,*aLambda,*aSymIn);

						// Get aSecurity level of child Lambda
						// (setq aResult (member aFacesStruct aSymSecurity:))
						*aResult = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,*aSymSecurity,*aFacesStruct);
						if (aResult->Tag == TYNUM)
						{	// (setq aResult aFacesStruct[aResult 1])
							*aResult = FSmartbase_Ref(gCP,gTP,3,*aFacesStruct,*aResult,TINT(1));
							if (aResult->Tag == TYNUM)
								aSecurity = asInt(aResult);
						}
					}
					// check that requested Lambda can be called and determine the type
					// of argument passing to do.
					aCallingConvention = 0; // default is to not allow calls
					// (setq aResult (member public: aFacesStruct))
					*aResult = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,*aSymPublic,*aFacesStruct);
					if (aResult->Tag == TYNUM)
					{	// (setq aResult aFacesStruct[aResult 1])
						*aResult = FSmartbase_Ref(gCP,gTP,3,*aFacesStruct,*aResult,TINT(1));
						if (aResult->Tag == TYBOLE)
						{	if (asBool(aResult) == 0)
							{	*aResult = TERROR((LpCHAR)"!Requested Lambda is not a public Lambda!");
								break;
							}
							aCallingConvention = 1; // normal argument passing
						}
						else if ((aResult->Tag == TYSYMBOL || aResult->Tag == TYQUOTEDSYMBOL))
						{	if (aResult->u.Symbol == aSymAmp->u.Symbol)
								aCallingConvention = 2; // AMP message passed as argument	
							else
							{ // unknown public type 
								*aResult = TERROR((LpCHAR)"!Requested Lambda has unknown public type!");
								break;
							}
						}
					}
					if (aCallingConvention == 0)
					{	*aResult = TERROR((LpCHAR)"!Requested Lambda is not public!");
						break;
					}
				}
				else
				{	// Convert name portion to a symbol
					*aResult = FSmartbase_Set(gCP,gTP,3,*aMsgBlockVector,TINT(aIx),TSYMBOL(apBuf));
					if (aResult->Tag == TYERROR)
					{	*aResult = TERROR((LpCHAR)"!AMP structure name could not be converted to symbol");
						break;
					}
				}
			}//aIx
			if (aResult->Tag == TYERROR)
				break; // out of SBGLUE_MSGBLOCK case

			if (aSecurity > iLevel)
			{	*aResult = TERROR((LpCHAR*)"!Security violation!");
				break;
			}

			// Process AMP call
			if (aCallingConvention == 2)
			{	// Convert message block vector to structure
 				if (iEvalType == SBGLUE_CMDSTRING_BINTRANSFER && aAmpCmd != 0)
					*aMsgBlockStructure = *aBinTransfer;	
				else
					*aMsgBlockStructure = FSmartbase_Eval(gCP,gTP,*aCmdObjectToStructure,1,*aMsgBlockVector);

				// Build tempoary structure of middleware annotations for appending to message block structure
				*aBlockAnnotations = FSmartbase_Eval(gCP,gTP,*aCmdNew,9,TSYMBOL("Structure")
						,TSYMBOL("_sessionid"),TINT(iSessionID)
						,TSYMBOL("_userid"),TINT(iUserID)
						,TSYMBOL("_level"),TINT(iLevel)
						,TSYMBOL("_sessionid"),TINT(iSessionID));

				// Append annotations structure to message block structure
				*aMsgBlockStructure = FSmartbase_Eval(gCP,gTP,*aCmdAppend,2,*aMsgBlockStructure,*aBlockAnnotations);

				//-------- Execute the Lambda passing the message block as an argument ---------
				// (setq aResult (aLambdaName aMsgBlockStructure))
				*aResult = FSmartbase_Eval(gCP,gTP,*aLambda,1,*aMsgBlockStructure);
				if (aResult->Tag == TYERROR)
					break;
				if (iEvalType == SBGLUE_EVENTMSG)
					FrameExit(SBGLUE_EVAL_SUCCESS);	// goto SKIP_RETURN_RESULT;

				// Make sure the aResult is a Structure. It is an error for anything other than
				// a structure to be returned from an AMP call.
				if (aResult->Tag != TYSTRUCTURE)
				{	*aResult = TERROR((LpCHAR)"!AMP call did not return a Structure!");
					break;
				}


				if (iEvalType == SBGLUE_CMDSTRING_BINTRANSFER)
				{	// Process the result structure. Return types are limited when you use
					// executeAmp so this process is somewhat abbreviated compared to the
					// variety of processing we would do if we were not processing an
					// executeAmp message.

					// Remove _* elements from the returned structure
					aLen = FSmartbase_VectorLen(gCP,gTP,*aResult);
					for (aIx = aLen - 1; aIx > -1; --aIx)
					{	*aName = FSmartbase_Ref(gCP,gTP,3,*aResult,TINT(aIx),TINT(0));
						*aName = FSmartbase_Convert(gCP,gTP,TYSTRING,*aName);
						FSmartbase_StringPtr(gCP,gTP,aName,&apName,&aNameLen);

						// Check element for special processing
						if (apName[0] == '_') // delete element
							*aResult = FSmartbase_Eval(gCP,gTP,*aCmdDelete,2,*aResult,TINT(aIx));
					}

					if (aResult->Tag != TYERROR)
					{
						gpSessionManager->cbReturnResult(iSessionID, aRequestID, "", "amp", "", apData, aDataSize);
						FrameExit(SBGLUE_EVAL_SUCCESS);	// SKIP_RETURN_RESULT
					}
					else
						FrameExit(SBGLUE_EVAL_FAILURE);

				}


				//------------ Convert the return TVAL into a results string ----------
				aLen = FSmartbase_VectorLen(gCP,gTP,*aResult); // Works on structures
				aBufPos = 0;
				// Determine return type
				*aIndex = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,TSYMBOL("_returntype"),*aResult);
				if (aIndex->Tag == TYNUM)
				{	// found a return type element, now find out what AMP Lambda wants
					// Get the value of the returntype structure element
					*aValue = FSmartbase_Ref(gCP, gTP, 3, *aResult, *aIndex, TINT(1));
					if (aValue->Tag != TYNUM)
					{	*aResult = TERROR((LpCHAR)"!AMP invalid returntype value!");
						break;
					}
					aReturntype = asInt(aValue);
				}
				else
					aReturntype = 0; //AMP use default return type processing
				int elementCount = 0;
				apError = NULL;
				apURL = NULL;
				apMimeText = NULL;
				apENCTYPE = NULL;

				// Find and process _error element if any
				*aIndex = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,TSYMBOL("_error"),*aResult);
				if (aIndex->Tag == TYNUM)
				{	// Get the value of the _error element
					*aErrorvalue = FSmartbase_Ref(gCP, gTP, 3, *aResult, asInt(aIndex), TINT(1));
					*aErrorvalue = FSmartbase_Convert(gCP,gTP,TYSTRING,*aErrorvalue);
					if (aErrorvalue->Tag == TYERROR)
					{	*aResult = TERROR((LpCHAR*)"!AMP error specified but error value could not be converted to string!");
						break;
					}
					FSmartbase_StringPtr(gCP,gTP,aErrorvalue,&apError,&aErrorLen);
				}

				// Process the rest of the aResult structure
				switch (aReturntype)
				{
				case 0:	// AMP default processing. Make enctype=amp and place del separated values into the mimetext field.
					*aMsgBlockString = FSmartbase_CnvFromText(gCP,gTP,(char*)""); // create empty string to append to
					// Get _BinaryPtr and _BinarySize to be returned with the result.
					*aIndex = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,TSYMBOL("_buffer_"),*aResult);
					if (aIndex->Tag == TYPOINTER)
						apData = (char*)aIndex->u.Pointer;
					*aIndex = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,TSYMBOL("_bufferSize_"),*aResult);
					if (aIndex->Tag == TYNUM)
						aDataSize = aIndex->u.Int;

					for (aIx=0; aIx < aLen; aIx++)
					{	// (setq name (ref aResult aIx 0))
						*aName = FSmartbase_Ref(gCP,gTP,3,*aResult,TINT(aIx),TINT(0));
						*aName = FSmartbase_Convert(gCP,gTP,TYSTRING,*aName);
						FSmartbase_StringPtr(gCP,gTP,aName,&apName,&aNameLen);

						// Check element for special processing
						if (apName[0] == '_') // skip element
							continue;
						else
						{	// Add structure element to del separated string
							*aValue = FSmartbase_Ref(gCP,gTP,3,*aResult,TINT(aIx),TINT(1));
							if (aValue->Tag != TYSTRING)
								*aValue = FSmartbase_Convert(gCP,gTP,TYSTRING,*aValue);
							if (aValue->Tag == TYERROR)
							{	*aResult = TERROR((LpCHAR*)"!AMP return value could not be converted to string!");
								break;
							}
							if (elementCount > 0) 
								*aMsgBlockString = FSmartbase_Eval(gCP,gTP,*aCmdAppend,5,*aMsgBlockString
								,*aRuboutChr,*aName, *aRuboutChr,*aValue);
							else
								*aMsgBlockString = FSmartbase_Eval(gCP,gTP,*aCmdAppend,4,*aMsgBlockString
								,*aName,*aRuboutChr, *aValue);
							elementCount++;
						}
					}
					// assign del separated aResult message to mimetext and set enctype
					FSmartbase_StringPtr(gCP, gTP, aMsgBlockString, &apMimeText, &aMimeTextLen); 
					apENCTYPE = const_cast<char*>("amp");										
					break;
				case 1: // URL processing. Return only the URL element. ENCTYPE and MIMETEXT will be empty.
					*aIndex = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,TSYMBOL("_url"),*aResult);
					if (aIndex->Tag == TYNUM)
					{	// Get the value of the _url element
						*aValue = FSmartbase_Ref(gCP,gTP,3,*aResult,*aIndex,TINT(1));
						*aValue = FSmartbase_Convert(gCP,gTP,TYSTRING,*aValue);
						if (aValue->Tag == TYERROR)
						{	*aResult = TERROR((LpCHAR*)"!AMP url return specified but _url value could not be converted to string!");
							break;
						}
					}
					else
					{	*aResult = TERROR((LpCHAR*)"!AMP url return specified but _url element not found!");
						break;
					}
					FSmartbase_StringPtr(gCP, gTP, aValue, &apURL, &aUrlLen);
					apENCTYPE = const_cast<char*>("url");					
					break;

				case 2: // FILE processing. Return only the URL element. ENCTYPE will be FILE and MIMETEXT will contain file url.
					*aIndex = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,TSYMBOL("_file"),*aResult);
					if (aIndex->Tag == TYNUM)
					{	// Get the value of the _mimetext element
						*aValue = FSmartbase_Ref(gCP,gTP,3,*aResult,*aIndex,TINT(1));
						*aValue = FSmartbase_Convert(gCP, gTP, TYSTRING, *aValue);
						if (aValue->Tag == TYERROR)
						{	*aResult = TERROR((LpCHAR*)"!AMP file return specified but _file value could not be converted to string!");
							break;
						}
					}
					else
					{	*aResult = TERROR((LpCHAR*)"!AMP file return specified but no _file element found!");
						break;
					}
					FSmartbase_StringPtr(gCP, gTP, aValue, &apMimeText, &aMimeTextLen);
					apENCTYPE = const_cast<char*>("file");					
					break;

				case 3: // TEXT processing. Return the ENCTYPE and MIMETEXT elements.
					*aIndex = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,TSYMBOL("_mimetext"),*aResult);
					if (aIndex->Tag == TYNUM)
					{	// Get the value of the _mimetext element
						*aValue = FSmartbase_Ref(gCP, gTP, 3, *aResult, *aIndex, TINT(1));
						*aValue = FSmartbase_Convert(gCP, gTP, TYSTRING,*aValue);
						if (aValue->Tag == TYERROR)
						{	*aResult = TERROR((LpCHAR*)"!AMP text return specified but _mimetext value could not be converted to string!");
							break;
						}
					}
					else
					{	*aResult = TERROR((LpCHAR*)"!AMP text returns specified but _mimetext element not found!");
						break;
					}
					*aIndex = FSmartbase_Eval(gCP,gTP,*aCmdMember,2,TSYMBOL("_enctype"),*aResult);
					if (aIndex->Tag == TYNUM)
					{	// Get the value of the _enctype element
						*aEncvalue = FSmartbase_Ref(gCP, gTP, 3, *aResult, *aIndex, TINT(1));
						*aEncvalue = FSmartbase_Convert(gCP,gTP,TYSTRING,*aEncvalue);
						if (aEncvalue->Tag == TYERROR)
						{	*aResult = TERROR((LpCHAR*)"!AMP text return specified but _enctype value could not be"
							"converted to string!");
							break;
						}
					}
					else
					{	*aResult = TERROR((LpCHAR*)"!AMP text return specified but _enctype element not found!");
						break;
					}
					FSmartbase_StringPtr(gCP, gTP, aValue, &apMimeText, &aMimeTextLen);
					FSmartbase_StringPtr(gCP, gTP, aEncvalue, &apENCTYPE, &aEnctypeLen);
					break;

				default:
						*aResult = TERROR((LpCHAR*)"!AMP invalid return type specified");
						break;
				}
			}// end of AMP processing
			// Regular Argument passing convention
			else if (aCallingConvention == 1)
			{	*aResult = TERROR((LpCHAR*)"!Standard function interface not implemented yet!");
				break;
			}
			//------- Return byte vector, as a string, to host -------------
			// Make callbacks to return string return values. Note that this
			// should aResult in a COPY of the string value so later when 
			// we exit this routine we can let the engine garbage collect the 
			// source TVAL.
			if (aResult->Tag != TYERROR)
			{	char *apText = (apURL != NULL && *apURL != '\0') ? apURL : apMimeText;
				gpSessionManager->cbReturnResult(iSessionID, aRequestID, apError, apENCTYPE, apText, apData, aDataSize);
				FrameExit(SBGLUE_EVAL_SUCCESS);	// SKIP_RETURN_RESULT
			}
			break; // out of case SBGLUE_MSGBLOCK
		}
	if (aResult->Tag == TYERROR)
	{	
		if (apData != NULL)
		{	//Delete binary transfer buffer if any
			free(apData);
			aDataSize = 0;
		}
		FSmartbase_StringPtr(gCP,gTP,aResult,&apBuf,&aBufLen);
		if ((apBuf[1] == 'f') && (apBuf[2] == 'i') && (apBuf[3] == 'l') && (apBuf[4] == 'e'))
		{	SysGlue_lastError(gCP, gTP, aErrorStringBuffer);
			gpSessionManager->cbReturnResult(iSessionID, aRequestID, aErrorStringBuffer, "text/plain", apBuf, NULL/*Data*/, 0/*Size*/);
		}
		else 
			gpSessionManager->cbReturnResult(iSessionID, aRequestID, apBuf, "text/plain", apBuf, NULL/*Data*/, 0/*DataSize*/);
		FrameExit(SBGLUE_EVAL_FAILURE);
	}
	if (apData != NULL)
	{	//Delete binary transfer buffer if any
		free(apData);
		aDataSize = 0;
	}
	// Should never get here so return a SBGLUE error
	FrameExit(SGBLUE_UNEXPECTED_ERROR);

	// If we got here we have an engine error OR a request suspension
	LBLSystemThrowError:
	if (apData != NULL)
	{	//Delete binary transfer buffer if any
		free(apData);
		aDataSize = 0;
	}

	// Here is where we handle real errors
	if ((aCatchCode == SBGLUE_ERR_FILE_READ) || (aCatchCode == SBGLUE_ERR_FILE_WRITE))
	{	SysGlue_lastError(gCP,gTP,aErrorStringBuffer);
		gpSessionManager->cbReturnResult(iSessionID,aRequestID, aErrorStringBuffer, "text/plain", apBuf, NULL, 0/*DataSize*/);
	}
	else
		gpSessionManager->cbReturnResult(iSessionID,aRequestID, errorMsg(errorNum(aCatchCode)), "text/plain", apBuf, NULL, 0);
	return(errorNum(aCatchCode));
} // end SBGlue_Eval

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
long SBGlue_Init()
{
#if defined(_LINUX) && defined(_M32)
	// Initialize the time when application started.
	// The value will be subtracted from the current time when getTickCount is called.
	struct timeval aTv;
	time_t aSeconds;
	suseconds_t aMseconds;
	gettimeofday(&aTv, NULL);
	aSeconds = aTv.tv_sec;
	aMseconds = aTv.tv_usec;
	gStartClock = (double)(aSeconds / 1.0) + (double)(aMseconds / (double)1000000.0);
#endif
	/* Initilize context flags */
	for (long i = 0; i < SBGLUE_MAX_CONTEXTS; ++i)
	{	gContextFlags[i].mInUse = -1;
		gContextFlags[i].mEscape = -1;
		gContextFlags[i].mInstructionTrace = -1;
		gContextFlags[i].mErrorTrace = -1;
		gContextFlags[i].mSystemCheck = -1;
		gContextFlags[i].mJit = -1;
		gContextFlags[i].mModified = 0;
	}
	return(SysGlue_IOInit()); // Initialize global values for file access management
}

/*-----------------------------------------------------------------
SBGlue_OpenContext
ipContextName		Textual name of context
ipScript			Script to run on setup
ipContext			pointer to pointer to memory for context. 
iMemorySize			memorysize
objHdrSize			Size of Object Header pool
sessionMgrContextThread	Pointer to the aContextThread in session manager for this context

Note: if the ipContext is Null then the memory will be allocated
in farther down in the call chain (see create_context) otherwise
the memory has been previously allocated and is being passed to 
this routine.
------------------------------------------------------------------*/
long SBGlue_OpenContext(const char * ipContextName,const char* ipScript,/*out*/ void ** ipContext, NUM iMemorySize, long objHdrSize,
	QThread*sessionMgrContextThread)
{
	long aContextIndex, aResult;

	// Find open slot in gContextFlags array
	aContextIndex = -1;
	for(long i = 0; i < SBGLUE_MAX_CONTEXTS; ++i)
	{	gContextMutex[i].lock(); // Block until access granted
		// open context slots are identified by a value of -1
		if (gContextFlags[i].mInUse == -1)
		{	aContextIndex = i;
			gContextFlags[i].mInUse = 1;
		}
		gContextMutex[i].unlock();
		if (aContextIndex != -1) // are we done?
			break;
	}
	if (aContextIndex == -1) return -1; // Could not open context!
	aResult = createContext(ipContextName, aContextIndex, ipScript, (LpXCONTEXT *)ipContext, 1, (NUM)iMemorySize, objHdrSize
	, (POINTER)sessionMgrContextThread);
	if (aResult < 0)
		return aResult;
	return(aContextIndex);
}

/* 
SBGlue_SetEngineFlags changes the state of the requested engine flags.
The context checks these values on a frequent basis and changes it state 
when appropriate. 

Note that these flags are reset to -1 (meaning don't change anything) after the
engine examines them. Note as well that changing any of these flags sets the 
Modified flag to 1.
*/
long SBGlue_SetEngineFlags(long iContextIndex,long iFlag, long iFlagValue)
{
	gContextMutex[iContextIndex].lock(); // Block until access granted.
	switch(iFlag)
	{
	case SBGLUE_ESCAPE:
		gContextFlags[iContextIndex].mEscape = iFlagValue;
		gContextFlags[iContextIndex].mModified = 1;
		break;
	case SBGLUE_INSTRUCTION_TRACE:
		gContextFlags[iContextIndex].mInstructionTrace = iFlagValue;
		gContextFlags[iContextIndex].mModified = 1;
		break;
	case SBGLUE_ERROR_TRACE:
		gContextFlags[iContextIndex].mErrorTrace = iFlagValue;
		gContextFlags[iContextIndex].mModified = 1;
		break;
	case SBGLUE_SYSCHECKON:
		gContextFlags[iContextIndex].mSystemCheck = iFlagValue;
		gContextFlags[iContextIndex].mModified = 1;
		break;
	case SBGLUE_JITON:
		gContextFlags[iContextIndex].mJit = iFlagValue;
		gContextFlags[iContextIndex].mModified = 1;
		break;
	}
	gContextMutex[iContextIndex].unlock();
//	if (!ReleaseMutex(gContextMutex[iContextIndex]))
//		return(-1);
	return(0);
}

// ****************************************** END OF PUBLIC FUNCTIONS *********************************************************


//	***************************************  FUNCTIONS REGISTERED WITH THE ENGINE *********************************************
// Forward declaration of the functions registered with the engine
TVAL	SBGlue_AXml(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_Browse(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_BuildLambdaMessage(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_SetResult(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextClient(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_CpOSFile(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_DebugDialog(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_DecodeURL(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_Dir(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
NUM		SBGlue_Display(LpXCONTEXT gCP,LpTHREAD gTP, char* ipString, NUM  iNewline);
TVAL	SBGlue_DisplayWorkbenchWindow(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_EnableConsoleLog(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_EncodeURL(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
NUM		SBGlue_Escape(LpXCONTEXT gCP, LpTHREAD gTP);
TVAL	SBGlue_EvalAMP(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_EvalsInSyncLocalContext(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ExistsOSFile(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_FileInfo(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_GetConsoleLog(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_GetContextPtr(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_GetFileStatus(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_GetHttp(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_GetOSDir(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_GetTickCount(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_HostCreate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_HostDestroy(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_HostGetProperty(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_HostInvoke(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_HostPutProperty(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_HostSend(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_Input(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_LoadBrowseLambda(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_LoadLib(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_LoadUtilityLambdas(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_MkOSDir(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_MsgBox(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_NextUserID(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_Notify(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
NUM		SBGlue_Open(LpXCONTEXT gCP, LpTHREAD gTP, char* ipName, NUM iMode, NUM iType);
TVAL	SBGlue_OpenConsoleLog(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_PostHttp(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ReadHtmlPage(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ReloadLib(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_RingBell (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_Select(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_SendToClient(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ServerDoomed(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_SmOpenContext(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_Sleep(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_Submit(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_Subscribe(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_System(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_TcpClient(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_Throw(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[]);
TVAL	SBGlue_OpenLog(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_StartLogging(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_StopLogging(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_CloseLog(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
void	SBGlue_WriteTime(LpXCONTEXT gCP,LpTHREAD gTP);
NUM		SBGlue_UpdateState(LpXCONTEXT gCP, LpTHREAD gTP);
TVAL	SBGlue_Md5Hash(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_AddToFileWatcher(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_RemoveFromFileWatcher(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_SendFileChangedEvent(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_SendStatusUpdateEvent(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextOpen(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextSubmit(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextBusy(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextExists(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextStop(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextGetResult(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextGetDisplay(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextClose(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextMyself(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextSubscribe(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
TVAL	SBGlue_ContextList(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);

/*-----------------------------------------------------------------
SBGlue_AXml
Arguments:
file
------------------------------------------------------------------*/
TVAL SBGlue_AXml(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	AXml*		apXmlObjectPtr;
	long			aFunctionHandle;
	char*		apStr;
	NUM			aStrlen;
	bool		aRes;
	bool		aFBool;
	StartFrame
	EndFrame
 
	if (argc < 2)
		FrameExit(TERROR((LpCHAR)"SBGlue_AXml, arglist")); // make sure we have the minimum number of arguments
	apXmlObjectPtr = (AXml*)asFunction(&argv[0]); // Get pointer to directory object if any
	aFunctionHandle = asInt(&argv[1]); // get aFunctionHandle for use in following dispatch table
	switch (aFunctionHandle)
	{
	case 0: //_CleanUp - free resource allocated in the glue layer
		if (apXmlObjectPtr != NULL)
		{	delete apXmlObjectPtr;
			FrameExit(TFUNCTION(0));
		}
		break;
	case 1: //new - Allocate C++ AXml object
		if (argc != 3) FrameExit(TERROR((LpCHAR)"!Error: AXml:new, arglist!"));
		apXmlObjectPtr = new AXml(gCP,gTP, argv[2]);
		FrameExit(TFUNCTION(apXmlObjectPtr));
		break;

	case 2: //setHandlerLambda
		if (apXmlObjectPtr == 0) FrameExit(TERROR((LpCHAR)"!AXml:setHanderLambda, no AXml object!"));
		if (argc != 3) FrameExit(TERROR((LpCHAR)"!aXmo:setHanderLambda, arglist!"));
		aRes = apXmlObjectPtr->setHandlerLambda(argv[2]);
		FrameExit(TBOOL(aRes));
		break;

	case 3: //parseFile
		if (apXmlObjectPtr == 0) FrameExit(TERROR((LpCHAR)"AXml:parseFile, no AXml object"));
		if (argc != 3) FrameExit(TERROR((LpCHAR)"AXml:parseFile, arglist"));
		FSmartbase_StringPtr(gCP, gTP, &argv[2], &apStr, &aStrlen); // filename
		aRes = apXmlObjectPtr->parseFile(QString(apStr));
		FrameExit(TBOOL(aRes));
		break;

	case 4: //parseBuffer
		if (apXmlObjectPtr == 0) FrameExit(TERROR((LpCHAR)"AXml:parseBuffer, no AXml object"));
		if (argc != 3) FrameExit(TERROR((LpCHAR)"AXml:parseBuffer, arglist"));
		FSmartbase_StringPtr(gCP, gTP, &argv[2], &apStr, &aStrlen); // filename
		aRes = apXmlObjectPtr->parseBuffer(apStr);
		FrameExit(TBOOL(aRes));
		break;

	case 5: //setFeature
		if (apXmlObjectPtr == 0) FrameExit(TERROR((LpCHAR)"AXml:setFeature, no AXml object"));
		if (argc != 4) FrameExit(TERROR((LpCHAR)"AXml:setFeature, arglist"));
		FSmartbase_StringPtr(gCP, gTP, &argv[2], &apStr, &aStrlen); // filename
		aFBool = asBool(&argv[3]);
		apXmlObjectPtr->setFeature(QString(apStr), aFBool);
		FrameExit(TBOOL(true));
		break;

	case 6: //clearFeatures
		if (apXmlObjectPtr == 0) FrameExit(TERROR((LpCHAR)"AXml:clearFeatures, no aXml object"));
		if (argc != 2) FrameExit(TERROR((LpCHAR)"AXml:clearFeatures, arglist"));
		apXmlObjectPtr->clearFeatures();
		FrameExit(TBOOL(true));
		break;

		} // End of Switch
	FrameExit(TERROR("!function not supported by AXml yet!"));
}

/***************************************************************** 
Obsolete functions that should raise an error.
The following functions may be called by the engine but should have 
no implementation in this glue layer. Raise an error in the host 
application if one of these functions is called. We register each 
of these individually to ensure we can report which fucntion was 
called. These functions call the SM_Error().

TVAL SBGlue_Browse(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_Input(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_Select(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_HostCreate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_HostSend(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_HostInvoke(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_HostDestroy(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_HostGetProperty(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_HostPutProperty(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_Notify(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_MsgBox(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_NextUserID(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_EvalAMP(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_ServerDoomed(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
TVAL SBGlue_DisplayWorkbenchWindow(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
******************************************************************/
/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_Browse(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	return(TERROR("!browse not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
TM! do we really want this?
------------------------------------------------------------------*/
TVAL SBGlue_BuildLambdaMessage(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_BuildLambdaMessage
	return(TERROR( "buildLambdaMessage: not implemented yet")); 
}

/*-----------------------------------------------------------------
SBGlue_CreateBuffer
The SBGlue_CreateBuffer function allocates a buffer of the specified
size and returns a pointer to that buffer.
------------------------------------------------------------------*/
TVAL SBGlue_CreateBuffer(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{ 
	POINTER		apBlock;
	StartFrame
	DeclareTVAL(aSize);
	DeclareTVAL(aRet);
	EndFrame

	if (argc != 1)
	{	*aRet = TERROR((LpCHAR)"CreateBuffer requires one argument");
		FrameExit(*aRet);
	}

	if (asTag(&argv[0]) != TYNUM)
	{	*aSize = FSmartbase_Convert(gCP, gTP, TYNUM, argv[0]);
		ExitOnError(*aSize);
	}
	else
		aSize->u.Int = argv[0].u.Int;

	//Malloc the buffer
	apBlock = (POINTER)malloc(aSize->u.Int);
	if (apBlock == NULL)
	{	*aRet = TERROR((LpCHAR)"CreateBuffer failed to get requested memory");
		FrameExit(*aRet);
	}

	aRet->Tag = TYPOINTER;
	aRet->u.Pointer = apBlock;
	FrameExit(*aRet);
}



/*-----------------------------------------------------------------
SBGlue_DeleteBuffer
The SBGlue_DeleteBuffer function deallocates a buffer.
------------------------------------------------------------------*/
TVAL SBGlue_DeleteBuffer(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{ 
	StartFrame
	DeclareTVAL(aRet);
	EndFrame

	if (argc != 1)
	{	*aRet = TERROR((LpCHAR)"DeleteBuffer requires one argument");
		FrameExit(*aRet);
	}

	if (asTag(&argv[0]) != TYPOINTER)
	{	*aRet = TERROR((LpCHAR)"DeleteBuffer requires pointer argument");
		FrameExit(*aRet);
	}

	if (argv[0].u.Pointer == NULL)
	{	*aRet = TERROR((LpCHAR)"DeleteBuffer passed null pointer argument");
		FrameExit(*aRet);
	}

	//free the buffer
	free(argv[0].u.Pointer);
	FrameExit(gCP->Tval_TRUE);
}

/*-----------------------------------------------------------------
SBGlue_TcpClient
------------------------------------------------------------------*/
/* This function implements an interface to the TcpClient object.
arguments
argv[0]		- pointer to ATcpClient object
argv[1]		- function handle
argv[2]		- vector of arguments to member function
Notes:
*/ 
TVAL SBGlue_TcpClient(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	long			aFunctionHandle;
	long			aRequestID = 0;
	char*			apStr;
	NUM				aStrlen;
    char*			apStr2;
	NUM				aStrlen2;
	long			aArgsLen;
	bool			aOk;
	QThread*		apMainThread;
	bool			aTrace = false;
    ATcpTask		aTcpTaskCopy;
    ATcpClient*     apATcpClient;
    QByteArray      aAisOut;
    long            aCount = 0;

	StartFrame
	DeclareTVAL(aArgs);
	DeclareTVAL(aArg1);
	DeclareTVAL(aArg2);
    DeclareTVAL(aResult);
	EndFrame

	if (argc < 2) 
		FrameExit(TERROR((LpCHAR)"SBGlue_Dir, arglist")); // make sure we have the minimum number of arguments

    apATcpClient = (ATcpClient *)asFunction(&argv[0]); // Get pointer to ATcpClient object if any    
	aFunctionHandle = asInt(&argv[1]); // get aFunctionHandle for use in following dispatch table

	// Note that function arguments are passed in a single vector which is the forth argument of the SBGlue_ContextClient function.
	aArgsLen = 0; // Default to no arguments
	if (argc == 3)
	{	// Get Lambda arguments from arguments vector.
		*aArgs = argv[2];
		if (aArgs->Tag != TYVECTOR)
			FrameExit(TERROR((LpCHAR)"SBGlue_TcpClient: bad arguments vector"));
		aArgsLen = FSmartbase_VectorLen(gCP,gTP,*aArgs);
		if (aArgsLen > 0)
		{	*aArg1 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(0));
			if (aArgsLen > 1)
			{	*aArg2 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(1)); }
		}
	}
	if (apATcpClient == 0 && aFunctionHandle != 10) 
				FrameExit(TERROR((LpCHAR)"SBGlue_TcpClient:new, no Tcp Client object"));
	switch(aFunctionHandle)
	{	
	//_CleanUp
	case 0:		//_CleanUp - free resource allocated in the glue layer
        apATcpClient->deleteLater();
        FrameExit(TFUNCTION(0));
		break;

	// new
	case 10: //new - Allocate C++ ATcpClient object
        if (aArgsLen > 0)
            FrameExit(TERROR((LpCHAR)"SBGlue_TcpClient:new, arglist"));
        apATcpClient = new ATcpClient(AISSVR_HTTP);
		apMainThread = gpSessionManager->getMainThread();
        apATcpClient->moveToThread(apMainThread);     
        FrameExit(TFUNCTION(apATcpClient));
		break;

	// openConnection
	case 20:	//(_AISTcpClient objPtr 20 #)
		if (aArgsLen != 2 || (aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT) || aArg2->Tag != TYNUM)
			FrameExit(TERROR((LpCHAR)"SBGlue_TcpClient:openTcpConnection, host address and port number required."));
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen);
        aRequestID = apATcpClient->openConnection(apStr, asInt(aArg2));
		if (aTrace) qDebug("openTcpConnection task=%ld", aRequestID);
		break;

	// isConnected
	case 30:	//(_AISContextClient objPtr 30 #)
        if (aArgsLen > 0)
            FrameExit(TERROR((LpCHAR)"SBGlue_TcpClient:IsConnected, no arguments required"));
		aRequestID = apATcpClient->isConnected();
		if (aTrace) qDebug("isConnected task=%ld", aRequestID);
		break;

	// submit
	case 40:	//(_AISTcpClient objPtr 40 #)
		if ((aArgsLen != 2)|| (aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT)
			||	(aArg2->Tag != TYSTRING && aArg2->Tag != TYTEXT))
            FrameExit(TERROR((LpCHAR)"SBGlue_TcpClient:submit, URL and Http method required"));	
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen);   // Resource path
        FSmartbase_StringPtr(gCP,gTP,aArg2,&apStr2,&aStrlen2); // Message
		aRequestID = apATcpClient->submit(QString(apStr), QString(apStr2));
		if (aTrace) qDebug("submit task=%ld", aRequestID);
		break;

	// closeConnection
	case 50:	//(_AISTcpClient objptr 50 #)
		if (aArgsLen > 0)
            FrameExit(TERROR((LpCHAR)"SBGlue_TcpClient:closeConnection, no arguments required"));
		aRequestID = apATcpClient->closeConnection();
		if (aTrace) qDebug("closeConnection task=%ld", aRequestID);
		break;
	} // End of Switch

	for (;;)
	{	
        aTcpTaskCopy = apATcpClient->getReturnResult(aRequestID, &aOk);
		if (aOk) break;
		if (++aCount > 60 && ((aFunctionHandle == 20) || (aFunctionHandle == 40)))
		{	FrameExit(TINT(ATIMEOUT));		// Wait for 30 seconds maximum if connecting to host.
			break;
		}
		AContextThread* apSessionMgrContextThread = (AContextThread*)gCP->SessionMgrContextThread;
		apSessionMgrContextThread->localSleep(500L); 
	}
   
    *aResult = FSmartbase_Eval(gCP,gTP,TGVALUE("new"), 3, TSYMBOL("Structure"), 
                                     TSYMBOL("AisOut"),TSTRING(aTcpTaskCopy.mAisOut.toAscii().data()));
	FrameExit(*aResult); 
}

/*-----------------------------------------------------------------
SBGlue_ContextClient
------------------------------------------------------------------*/
/* This function implements an interface to the AContextClient object.
arguments
argv[0]		- pointer to aContextClient object
argv[1]		- function handle
argv[2]		- vector of arguments to member function
Notes:
*/ 
TVAL SBGlue_ContextClient(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	AContextClient*	apClient;		// -> ContextClient
	long				aFunctionHandle;
	long			aRequestID = 0;
	char*			apStr;
	char*			apStr2;
	NUM				aStrlen;
	NUM				aStrlen2;
	long			aArgsLen;
	bool			aOk;
	QThread*		apMainThread;
	QString			aTmpStr;
	QString			aTmpStr1;
	QStringList		aTmpStringList;
	bool			aTrace = false;
	AEvalReturnType	aEvalReturnType = geWait;
	AEvalNotifyType	aEvalNotifyType = geNoNotify;
	AClientResultType aResultType;
	AClientTask		aTaskCopy;

	StartFrame
	DeclareTVAL(aArgs);
	DeclareTVAL(aArg1);
	DeclareTVAL(aArg2);
	DeclareTVAL(aArg3);
	DeclareTVAL(aArg4);
	DeclareTVAL(aArg5);
	DeclareTVAL(aArg6);
	DeclareTVAL(aArg7);
	DeclareTVAL(aArg8);
	DeclareTVAL(aResult);
	EndFrame
	if (argc < 2) 
		FrameExit(TERROR((LpCHAR)"SBGlue_Dir, arglist")); // make sure we have the minimum number of arguments
	apClient = (AContextClient *)asFunction(&argv[0]); // Get pointer to AContextClient object if any
	aFunctionHandle = asInt(&argv[1]); // get aFunctionHandle for use in following dispatch table

	// Note that function arguments are passed in a single vector which is the forth argument of the SBGlue_ContextClient function.
	aArgsLen = 0; // Default to no arguments
	if (argc == 3)
	{	// Get Lambda arguments from arguments vector.
		*aArgs = argv[2];
		if (aArgs->Tag != TYVECTOR)
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient: bad arguments vector"));
		aArgsLen = FSmartbase_VectorLen(gCP,gTP,*aArgs);
		if (aArgsLen > 0)
		{	*aArg1 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(0));
			if (aArgsLen > 1)
			{	*aArg2 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(1));
				if (aArgsLen > 2)
				{	*aArg3 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(2));
					if (aArgsLen > 3)
					{	*aArg4 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(3));
						if (aArgsLen > 4)
						{	*aArg5 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(4));
							if (aArgsLen > 5)
							{	*aArg6 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(5));
								if (aArgsLen > 6)
								{	*aArg7 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(6));
									if (aArgsLen > 7)
										*aArg8 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(7));
								}
							}
						}
					}
				}
			}
		}
	}
	if (apClient == 0 && aFunctionHandle != 10) 
				FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:logon, no ContextClient object"));
	aResultType = geStringContextClientResultType; // default aResult return type

	switch(aFunctionHandle)
	{	//_CleanUp
	case 0:		//_CleanUp - free resource allocated in the glue layer
		apClient->deleteLater();
		FrameExit(TFUNCTION(0));
		break;
	// new
	// Arguments
	//	ServerAddress
	//	ServerPort
	// Returns
	//	handle to new object
	case 10:		//new - Allocate C++ ContextClient object
		if (aArgsLen != 2 || (aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT) || aArg2->Tag != TYNUM)
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:new, arglist"));
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen); // server path argument
		apClient = new AContextClient(apStr, asInt(aArg2), gCP->ContextIndex);
		apMainThread = gpSessionManager->getMainThread();
		apClient->moveToThread(apMainThread);
		FrameExit(TFUNCTION(apClient));
		break;
	// openConnection
	// Returns
	// 0 - sucessful connection
	case 20:	//(_AISContextClient objPtr 20 #(returnType notifyType))
		if (aArgsLen > 2 || (aArgsLen > 0 && (aArg1->Tag != TYNUM || !getEvalReturnType(asInt(aArg1), &aEvalReturnType)))
		|| (aArgsLen > 1 && (aArg2->Tag != TYNUM || !getEvalNotifyType(asInt(aArg2), &aEvalNotifyType))))
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:openConnection, arglist"));		

		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********", gCP->ContextName, gTP->SessionID, apClient->getServerAddress()
			.toLatin1().data(), apClient->getContextName().toLatin1().data(), apClient->getSessionID());
			qDebug("openConnection: refContext=%s refSessionID=%ld refRequestID=%ld evalReturnType=%d evalNotifyType=%d",
			gCP->ContextName,gTP->SessionID,gTP->RequestID,aEvalReturnType,aEvalNotifyType);
		}
		aRequestID = apClient->openConnection(gTP->SessionID, gTP->RequestID, aEvalReturnType, aEvalNotifyType);
		if (aTrace)
			qDebug("openConnection task=%ld", aRequestID);
		// For now, limit wait in case connection is not made. Later, modify ASocket to timeout.
		if (aEvalReturnType == geWait)
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;

	// logon
	// Arguments
	//	userName
	//	password
	//	aEvalReturnType
	//	aEvalNotifyType
	// Returns
	//	aRequestID
	case 30:	//(_AISContextClient objptr 30 #(userName password returnType notifyType))
			//ContextClient.logon(QString& irUsrName, QString& irPasswd, AEvalReturnType iEvalReturnType, AEvalNotifyType iEvalNotifytType)
			// Returns RequestId
		if ((aArgsLen < 2 || aArgsLen > 4) || (aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT)
			||	(aArg2->Tag != TYSTRING && aArg2->Tag != TYTEXT)
			||	(aArgsLen > 2 && (aArg3->Tag != TYNUM || !getEvalReturnType(asInt(aArg3), &aEvalReturnType)))
			||	(aArgsLen > 3 && (aArg4->Tag != TYNUM || !getEvalNotifyType(asInt(aArg4), &aEvalNotifyType))))
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:logon, arglist"));		

		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen); // userName 
		FSmartbase_StringPtr(gCP,gTP,aArg2,&apStr2,&aStrlen2); // password
		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********",gCP->ContextName,gTP->SessionID,apClient->getServerAddress().toLatin1().data()
			, apClient->getContextName().toLatin1().data(),apClient->getSessionID());

			qDebug("logon: refContext=%s refSessionID=%ld refRequestID=%ld userName=%s password=%s evalReturnType=%d evalNotifyType=%d"
			, gCP->ContextName, gTP->SessionID, gTP->RequestID, apStr, apStr2, aEvalReturnType, aEvalNotifyType);
		}
		aRequestID = apClient->logon(gTP->SessionID, gTP->RequestID,QString(apStr), QString(apStr2), aEvalReturnType, aEvalNotifyType);
		if (aTrace) qDebug("logon task=%ld", aRequestID);
		if (aEvalReturnType == geWait) 
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;

	// openContext
	case 40:	//(_AISContextClient objptr 40 #(startupPath contextName returnType notifyType))
		// ContextClient.openContext(QString& irStartupPath, QString& irContextName, 
		// AEvalReturnType iEvalReturnType = geWait, AEvalNotifyType iEvalNotifyType = geNoNotify)
		if ((aArgsLen < 2)
			||	(aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT)
			||	(aArg2->Tag != TYSTRING && aArg2->Tag != TYTEXT)
			||	(aArgsLen > 2 && (aArg3->Tag != TYNUM || !getEvalReturnType(asInt(aArg3), &aEvalReturnType)))
			||	(aArgsLen > 3 && (aArg4->Tag != TYNUM || !getEvalNotifyType(asInt(aArg4), &aEvalNotifyType))))
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:openContext, arglist"));		

		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen); // startupPath
		FSmartbase_StringPtr(gCP,gTP,aArg2,&apStr2,&aStrlen2); // contextName
		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********",gCP->ContextName,gTP->SessionID,apClient->getServerAddress().toLatin1().data(),
			apClient->getContextName().toLatin1().data(),apClient->getSessionID());
			qDebug("openContext: refContext=%s refSessionID=%ld refRequestID=%ld startupPath=%s contextName=%s evalReturnType=%d"
			" evalNotifyType=%d",gCP->ContextName,gTP->SessionID,gTP->RequestID, apStr, apStr2, aEvalReturnType,aEvalNotifyType);
		}
		aRequestID = apClient->openContext(gTP->SessionID, gTP->RequestID,QString(apStr), QString(apStr2), aEvalReturnType, aEvalNotifyType);
		if (aTrace) qDebug("openContext task=%ld", aRequestID);
		if (aEvalReturnType == geWait) 
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;

	// openSession
	case 50:	//(_AISContextClient objPtr 50 #(contextName returnType notifyType))
		if (aArgsLen < 1 
			||	(aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT)
			|| (aArgsLen > 1 && (aArg2->Tag != TYNUM || !getEvalReturnType(asInt(aArg2), &aEvalReturnType)))
			|| (aArgsLen > 2 && (aArg3->Tag != TYNUM || !getEvalNotifyType(asInt(aArg3), &aEvalNotifyType))))
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:openSession, arglist"));		

		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen); // contextName
		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********",gCP->ContextName, gTP->SessionID,apClient
			->getServerAddress().toLatin1().data(), apClient->getContextName().toLatin1().data(), apClient->getSessionID());
			qDebug("openSession: refContext=%s refSessionID=%ld refRequestID=%ld contextName=%s evalReturnType=%d"
			" evalNotifyType=%d", gCP->ContextName, gTP->SessionID, gTP->RequestID, apStr, aEvalReturnType, aEvalNotifyType);
		}
		aRequestID = apClient->openSession(gTP->SessionID, gTP->RequestID,QString(apStr),aEvalReturnType, aEvalNotifyType);
		if (aTrace)
			qDebug("openSession task=%ld", aRequestID);
		if (aEvalReturnType == geWait)
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;

	// eval
	case 60:	//(_AISContextClient objptr 60 #(command returnType notifyType))
		//ContextClient.eval(long aCallingSession, const QString& irExp, AEvalReturnType iEvalReturnType, AEvalNotifyType iEvalNofityType)
		if ((aArgsLen < 1 || aArgsLen > 3)
			||	(aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT)
			||	(aArgsLen > 1 && aArg2->Tag != TYNUM) || !getEvalReturnType(asInt(aArg2), &aEvalReturnType)
			||	(aArgsLen > 2 && aArg3->Tag != TYNUM) || !getEvalNotifyType(asInt(aArg3), &aEvalNotifyType))
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:eval, arglist"));

		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen); // command
		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********", gCP->ContextName, gTP->SessionID,apClient->getServerAddress()
			.toLatin1().data(), apClient->getContextName().toLatin1().data(), apClient->getSessionID());
			qDebug("eval: refContext=%s refSessionID=%ld refRequestID=%ld cmd=%s evalReturnType=%d evalNotifyType=%d"
			, gCP->ContextName, gTP->SessionID, gTP->RequestID, apStr, aEvalReturnType, aEvalNotifyType);
		}
		aRequestID = apClient->eval(gTP->SessionID, gTP->RequestID, QString(apStr), aEvalReturnType, aEvalNotifyType);
		if (aTrace) qDebug("eval task=%ld", aRequestID);
		if (aEvalReturnType == geWait) 
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;

	// execute
	case 65:	//(_AISContextClient objptr 65 #(expression bufptr bufsize returnType notifyType))
		//ContextClient.execute(long aCallingSession, const QString& irExp, char* ipData, long iDatasize, AEvalReturnType iEvalReturnType
		// , AEvalNotifyType iEvalNofityType)

		//Handle case where buffer was passed
		if ((aArgsLen > 1) && (aArg2->Tag == TYPOINTER))
			{
			if ((aArgsLen < 3 || aArgsLen > 5)
				||	(aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT)
				||  (aArg3->Tag != TYNUM)
				||	(aArgsLen > 3 && aArg4->Tag != TYNUM) || !getEvalReturnType(asInt(aArg4), &aEvalReturnType)
				||	(aArgsLen > 4 && aArg5->Tag != TYNUM) || !getEvalNotifyType(asInt(aArg5), &aEvalNotifyType))
				FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:eval, arglist"));
			
			FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen); // expression

			aRequestID = apClient->execute(gTP->SessionID, gTP->RequestID, QString(apStr), aArg2->u.Pointer, aArg3->u.Int, aEvalReturnType, aEvalNotifyType);
			}
		else // Handle case where no buffer was passed
			{
			if ((aArgsLen < 1 || aArgsLen > 3)
				||	(aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT)
				||	(aArgsLen > 1 && aArg2->Tag != TYNUM) || !getEvalReturnType(asInt(aArg2), &aEvalReturnType)
				||	(aArgsLen > 2 && aArg3->Tag != TYNUM) || !getEvalNotifyType(asInt(aArg3), &aEvalNotifyType))
				FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:eval, arglist"));

			FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen); // expression
			aRequestID = apClient->execute(gTP->SessionID, gTP->RequestID, QString(apStr), NULL, 0, aEvalReturnType, aEvalNotifyType);
			}

		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********", gCP->ContextName, gTP->SessionID, apClient->getServerAddress()
			.toLatin1().data(), apClient->getContextName().toLatin1().data(), apClient->getSessionID());
			qDebug("read: refContext=%s refSessionID=%ld refRequestID=%ld exp=%s evalReturnType=%d evalNotifyType=%d"
			, gCP->ContextName, gTP->SessionID, gTP->RequestID, apStr, aEvalReturnType, aEvalNotifyType);
		}

		if (aTrace) qDebug("execute task=%ld", aRequestID);
		if (aEvalReturnType == geWait) 
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;

	// logoff
	case 70:	//(_AISContextClient objptr 70 #(returnType notifyType))
		//ContextClient.logoff(AEvalReturnType iEvalReturnType))
		if ((aArgsLen > 0 && (aArg1->Tag != TYNUM || !getEvalReturnType(asInt(aArg1), &aEvalReturnType)))
			||	(aArgsLen > 1 && (aArg2->Tag != TYNUM || !getEvalNotifyType(asInt(aArg2), &aEvalNotifyType))))
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:logoff, arglist"));		
		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********",gCP->ContextName, gTP->SessionID, apClient->getServerAddress()
			.toLatin1().data(), apClient->getContextName().toLatin1().data(), apClient->getSessionID());
			qDebug("logoff: refContext=%s refSessionID=%ld refRequestID=%ld evalReturnType=%d evalNotifyType=%d"
			,gCP->ContextName, gTP->SessionID, gTP->RequestID, aEvalReturnType, aEvalNotifyType);
		}
		aRequestID = apClient->logoff(gTP->SessionID, gTP->RequestID, aEvalReturnType, aEvalNotifyType);
		if (aTrace) qDebug("logoff task=%ld", aRequestID);
		if (aEvalReturnType == geWait) 
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;
	// closeContext
	case 80:	//(_AISContextClient objptr 80 #(contextName returnType notifyType))
		// ContextClient.closeContext(QString& irContextName, 
		// AEvalReturnType iEvalReturnType = geWait, AEvalNotifyType iEvalNotifyType = geNoNotify)
		if ((aArgsLen < 1)
			||	(aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT)
			||	(aArgsLen > 1 && (aArg2->Tag != TYNUM || !getEvalReturnType(asInt(aArg2), &aEvalReturnType)))
			||	(aArgsLen > 2 && (aArg3->Tag != TYNUM || !getEvalNotifyType(asInt(aArg3), &aEvalNotifyType))))
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:closeContext, arglist"));		
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen); // startupPath
		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********",gCP->ContextName,gTP->SessionID,apClient->getServerAddress()
			.toLatin1().data(), apClient->getContextName().toLatin1().data(),apClient->getSessionID());
			qDebug("closeContext: refContext=%s refSessionID=%ld refRequestID=%ld contextName=%s aEvalReturnType=%d aEvalNotifyType=%d"
			, gCP->ContextName, gTP->SessionID, gTP->RequestID, apStr, aEvalReturnType, aEvalNotifyType);
		}
		aRequestID = apClient->closeContext(gTP->SessionID, gTP->RequestID,QString(apStr), aEvalReturnType, aEvalNotifyType);
		if (aTrace) qDebug("closeContext task=%ld", aRequestID);
		if (aEvalReturnType == geWait) 
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;
	// closeSession
	case 90:	//(_AISContextClient objptr 90 #(returnType notifyType))
		//ContextClient.closeSession(AEvalReturnType iEvalReturnType))
		if (	(aArgsLen > 0 && (aArg1->Tag != TYNUM || !getEvalReturnType(asInt(aArg1), &aEvalReturnType)))
			||	(aArgsLen > 1 && (aArg2->Tag != TYNUM || !getEvalNotifyType(asInt(aArg2), &aEvalNotifyType))))
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:logoff, arglist"));		

		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********",gCP->ContextName, gTP->SessionID, apClient->getServerAddress()
			.toLatin1().data(), apClient->getContextName().toLatin1().data(),apClient->getSessionID());
			qDebug("closeSession: refContext=%s refSessionID=%ld refRequestID=%ld evalReturnType=%d evalNotifyType=%d"
			,gCP->ContextName, gTP->SessionID, gTP->RequestID, aEvalReturnType, aEvalNotifyType);
		}
		aRequestID = apClient->closeSession(gTP->SessionID, gTP->RequestID, aEvalReturnType, aEvalNotifyType);
		if (aTrace) qDebug("closeSession task=%ld", aRequestID);
		if (aEvalReturnType == geWait)
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;

	// isContextOpen
	case 100:	//(_AISContextClient objPtr 100 #(contextName returnType notifyType))
		if (aArgsLen < 1 
			||	(aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT)
			|| (aArgsLen > 1 && (aArg2->Tag != TYNUM || !getEvalReturnType(asInt(aArg2), &aEvalReturnType)))
			|| (aArgsLen > 2 && (aArg3->Tag != TYNUM || !getEvalNotifyType(asInt(aArg3), &aEvalNotifyType))))
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:openSession, arglist"));		

		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen); // contextName
		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********", gCP->ContextName, gTP->SessionID,apClient->getServerAddress()
			.toLatin1().data(), apClient->getContextName().toLatin1().data(), apClient->getSessionID());
			qDebug("isContextOpen: refContext=%s refSessionID=%ld refRequestID=%ld contextName=%s evalReturnType=%d evalNotifyType=%d"
			, gCP->ContextName, gTP->SessionID, gTP->RequestID, apStr, aEvalReturnType, aEvalNotifyType);
		}
		aRequestID = apClient->isContextOpen(gTP->SessionID, gTP->RequestID,QString(apStr),aEvalReturnType, aEvalNotifyType);
		if (aTrace) qDebug("isContextOpen task=%ld", aRequestID);
		if (aEvalReturnType == geWait)
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;

	// getCurrentContexts
	// Returns
	// list of current contexts
	case 110:	//(_AISContextClient objPtr 110 #(returnType notifyType))
		if (aArgsLen > 2 
			|| (aArgsLen > 0 && (aArg1->Tag != TYNUM || !getEvalReturnType(asInt(aArg1), &aEvalReturnType)))
			|| (aArgsLen > 1 && (aArg2->Tag != TYNUM || !getEvalNotifyType(asInt(aArg2), &aEvalNotifyType))))
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:getCurrentContexts, arglist"));		

		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********", gCP->ContextName, gTP->SessionID, apClient->
			getServerAddress().toLatin1().data(), apClient->getContextName().toLatin1().data(), apClient->getSessionID());
			qDebug("getCurrentContexts: refContext=%s refSessionID=%ld refRequestID=%ld evalReturnType=%d evalNotifyType=%d"
			, gCP->ContextName, gTP->SessionID, gTP->RequestID, aEvalReturnType, aEvalNotifyType);
		}
		aRequestID = apClient->getCurrentContexts(gTP->SessionID, gTP->RequestID,aEvalReturnType, aEvalNotifyType);
		if (aTrace) qDebug("getCurrentContexts task=%ld", aRequestID);
		if (aEvalReturnType == geWait) 
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;

	// getSubscriptions
	// contextName can be an empty string to get all sessions on all contexts
	case 120:	//(_AISContextClient objPtr 120 #(contextName returnType notifyType))
		if (aArgsLen < 1 
			||	(aArg1->Tag != TYSTRING && aArg1->Tag != TYTEXT)
			|| (aArgsLen > 1 && (aArg2->Tag != TYNUM || !getEvalReturnType(asInt(aArg2), &aEvalReturnType)))
			|| (aArgsLen > 2 && (aArg3->Tag != TYNUM || !getEvalNotifyType(asInt(aArg3), &aEvalNotifyType))))
			FrameExit(TERROR((LpCHAR)"SBGlue_ContextClient:openSession, arglist"));		
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&aStrlen); // contextName
		if (aTrace)
		{	qDebug("******** %s %ld --> %s %s %ld ********", gCP->ContextName, gTP->SessionID, apClient->getServerAddress()
			.toLatin1().data(), apClient->getContextName().toLatin1().data(),apClient->getSessionID());
			qDebug("getSubscriptions: refContext=%s refSessionID=%ld refRequestID=%ld contextName=%s evalReturnType=%d evalNotifyType=%d"
			, gCP->ContextName, gTP->SessionID, gTP->RequestID, apStr, aEvalReturnType, aEvalNotifyType);
		}
		aRequestID = apClient->getSubscriptions(gTP->SessionID, gTP->RequestID, QString(apStr), aEvalReturnType, aEvalNotifyType);
		if (aTrace) qDebug("getSubscriptions task=%ld", aRequestID);
		if (aEvalReturnType == geWait)
			goto WAITFORRETURN;
		FrameExit(TINT(aRequestID));
		break;

	} // End of Switch
	FrameExit(TERROR("!function not supported by SBGlue_ContextClient yet!"));

WAITFORRETURN: // Come here if we are doing a synchronous call
	long aCount = 0;
	for (;;)
	{	aTaskCopy = apClient->getReturnResult(aRequestID, &aOk);
		if (aOk) break;
		if (++aCount > 60 && aFunctionHandle == 20)
		{	FrameExit(TINT(ATIMEOUT));		// Wait for 30 seconds maximum if connecting to host.
			break;
		}
		AContextThread* apSessionMgrContextThread = (AContextThread*)gCP->SessionMgrContextThread;
		apSessionMgrContextThread->localSleep(500L/*Msec*/);
	}
	// Construct a structure holding the return aResult from call

	*aResult = FSmartbase_Eval(gCP,gTP,TGVALUE("new"), 9, 
			TSYMBOL("Structure"), 
			TSYMBOL("BinaryBuffer"),
			TPOINTER((POINTER)aTaskCopy.mpData),
			TSYMBOL("AisOut"),
			TSTRING(aTaskCopy.mAisOut.toLatin1().data()), 
			TSYMBOL("Error"), 
			TSTRING(aTaskCopy.mError.toLatin1().data()), 
			TSYMBOL("NumValue"),
			TINT(aTaskCopy.mNumValue)
			);

	FrameExit(*aResult);
}

/*-----------------------------------------------------------------
SBGlue_DebugDialog
   Arguments:
   argv[0] titleString		String
   argv[1] promptString		String
   argv[2] listItems		Vector of Strings
   argv[3] selectItem		Integer
   argv[4] killSwitch		Optional, default = true.

This routine transforms the message it receives to the following
del separated string:
titleString\del
promptString\del
selectItem\del
numberOfCodeLines\del
numberOfInfoLines\del
delSeparatedCodeLines
delSeparatedInfoLines

  Notes:	
	1. was Functions_LispDebugDialog
	2. killSwitch is ignored by the session manager.
	3. Eventually, we may want to change the engine to return info in this format
	4. Currently, the engine only returns either code or info. Later
		I want it to return both at the same time. The Session Manager contains
		logic to collect a couple of calls together to create the package
		of code and info for transmission to the host application.
------------------------------------------------------------------*/
TVAL SBGlue_DebugDialog(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	NUM		aN;
	POINTER	apStrptr = NULL;
	NUM		aStrlen = 0;
	long	aCmdbuflen = 4096; // includes null terminator
	char	aCmdbuf[4096];
	long	aRetcode;

	// Protected variables
	StartFrame
	DeclareTVAL(aRet);        
	DeclareTVAL(aDebugLog);
	DeclareTVAL(aTvResult);
	DeclareTVAL(aEvalCmd);
	DeclareTVAL(aAppendCmd);
	DeclareTVAL(aRuboutChr);
	DeclareTVAL(aNumCodeLines);
	DeclareTVAL(aNumInfoLines);
	EndFrame

	/*  Make sure we have the proper argument count. */
	if (argc != 5 && argc != 4)
	{	*aRet = TERROR((LpCHAR)"!debug:aArgs!");
		FrameExit(*aRet);
	}	
	*aEvalCmd = TGVALUE("eval");
	*aAppendCmd = TGVALUE("append");
	*aRuboutChr = TCHAR(127);

	// Convert the debugDialog arguments into Strings and assign them
	// to the the Debug log.
	aDebugLog->Tag = TYTEXT;
	aDebugLog->u.Text[0] = 0;

	// Determine if we are sending code or info
	*aTvResult = FSmartbase_Ref(gCP, gTP, 2, argv[2], TINT(0));
	FSmartbase_StringPtr(gCP, gTP, aTvResult, &apStrptr, &aStrlen);
	long aRetType = 0; // default to info data
	if (isdigit(*apStrptr) && isdigit(apStrptr[1]) && isdigit(apStrptr[2]) && isdigit(apStrptr[3]) && isdigit(apStrptr[4]) && isdigit(apStrptr[5]) 
		&& (apStrptr[6]) == ':')
		aRetType = 1; // code data found

	// Determine number of list items
	aN = FSmartbase_VectorLen(gCP, gTP, argv[2]);

	// Create line count strings
	if (aRetType == 0)
	{	// info data being returned
		*aNumCodeLines = TINT(0);
		*aNumInfoLines = TINT(aN);
	}
	else
	{	*aNumCodeLines = TINT(aN);
		*aNumInfoLines = TINT(0);
	}
	// Concatenate arguments into a rubout separated string
	// as follows:
	// titleString	 #\rubout
	// promptString #\rubout
	// selectItem	 #\rubout
	// numCodeLines #\rubout
	// numInfoLines #\rubout
	// listItems	 (each item separated by a #\rubout character)
	*aDebugLog = FSmartbase_Eval(gCP, gTP, *aAppendCmd, 10, *aDebugLog, argv[0], *aRuboutChr, argv[1],
		*aRuboutChr, argv[3], *aRuboutChr, *aNumCodeLines, *aRuboutChr, *aNumInfoLines);

	// Concatenate each list item as a rubout separated string
	for (NUM i = 0; i < aN; ++i)
	{	*aTvResult = FSmartbase_Ref(gCP, gTP, 2, argv[2], TINT(i));
		*aDebugLog = FSmartbase_Eval(gCP, gTP,*aAppendCmd, 3, *aDebugLog,*aRuboutChr,*aTvResult);
	}
	// FSmartbase_StringPtr returns a pointer to the string and its length
	FSmartbase_StringPtr(gCP, gTP, aDebugLog, &apStrptr, &aStrlen);

	// Call the SessionManager function to get the next debug command.
	// The returned command is placed in the locally allocated cmdbuf.
	aRetcode = gpSessionManager->debug(gTP->SessionID, apStrptr, aStrlen, aCmdbuf, aCmdbuflen);

	//	SBGlue_Escape(gCP,gTP); // make sure engine flags get restored properly
	if (aRetcode < 0) 
		*aRet = TERROR((LpCHAR)"!debug:error calling session manager!");
	else 
		*aRet = TSTRING(aCmdbuf);
	FrameExit(*aRet);
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_DecodeURL(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	// was Functions_DecodeURL  TM! I think this function should be destructive.
	// Function pointers
	TVAL	aCopyFN = TGVALUE("copy"); 
	TVAL	aStringFN = TGVALUE("string"); 

	// Protected variables 
	StartFrame
	DeclareTVAL(aTvWorkBuffer);	// Copy of argv[0] string is decoded so function is non-destructive
	EndFrame

	// Make sure we have 1 argument being passed. 
	if (argc != 1) FrameExit(TERROR((LpCHAR)"decodeURL: arglist"));

	// Make sure we have a String, Text, or a ByteVector.
	// Get a pointer to the HTML String. 
	switch (argv[0].Tag)
    {
    case TYTEXT:
    case TYSTRING:
    case TYBYTEVECTOR:
        break;
    default:
        FrameExit(TERROR((LpCHAR)"decodeURL: argtype"));
        break;
    }
	// Copy argument to return variable
	*aTvWorkBuffer = FSmartbase_Eval(gCP, gTP, aCopyFN, 1, argv[0]);
	ExitOnError(*aTvWorkBuffer);	// Check for errors

	// Make sure we have a string
	*aTvWorkBuffer = FSmartbase_Eval(gCP, gTP, aStringFN, 1, *aTvWorkBuffer);
	ExitOnError(*aTvWorkBuffer);	// Check for errors

	// Decode string
	decodeUrl((char*)FSmartbase_ObjectPtr(gCP, gTP, aTvWorkBuffer));

	// Resize TVAL
	*aTvWorkBuffer = FSmartbase_CnvFromText(gCP, gTP,(char*)FSmartbase_ObjectPtr(gCP, gTP, aTvWorkBuffer));	
	ExitOnError(*aTvWorkBuffer);	// Check for errors

	// Return aResult and exit
	FrameExit(*aTvWorkBuffer);
}

/*-----------------------------------------------------------------
SBGlue_Dir
------------------------------------------------------------------*/
/* This function implements an interface to the QtDir object for
the Lisp Lambda dir. See the lisp Lambda dir documentation and the
QtDir documentation for additional information about the individual
member functions of the QtDir object surfaced by this interface.

Notes:
*/ 
TVAL SBGlue_Dir(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	QDir*			apDirObjectPtr;
	long			aFunctionHandle;
	char*			apStr;
	char*			apStr2;
	NUM				apStrlen;
	NUM				apStrlen2;
	long			apArgsLen;
	long			aTmpInt;
	long			aN;
	long				aNx;
	QString			aTmpStr;
	QString			aTmpStr1;
	QDir::Filters	aTmpFilterSpec;
	QDir::SortFlags	aTmpSortSpec;
	QStringList		aTmpStringList;
	QFileInfoList	aTmpFileInfoList;
	QFileInfo		aTmpFileInfo;
	bool			aTmpBool;
	QDir			aTmpDir;

	StartFrame
	DeclareTVAL(aArgs);
	DeclareTVAL(aArg1);
	DeclareTVAL(aArg2);
	DeclareTVAL(aArg3);
	DeclareTVAL(aArg4);
	DeclareTVAL(aResult);
	DeclareTVAL(aTmpVector);
	EndFrame
 
	if (argc < 2) 
		FrameExit(TERROR((LpCHAR)"SBGlue_Dir, arglist")); // make sure we have the minimum number of arguments
	apDirObjectPtr = (QDir *)asFunction(&argv[0]); // Get pointer to directory object if any
	aFunctionHandle = asInt(&argv[1]); // get apFunctionHandle for use in following dispatch table

	// Note that function arguments are passed in a single vector which is the third argument of the SBGlue_Dir function.
	apArgsLen = 0; // Default to no arguments
	if (argc == 3)
	{	// Get Lambda arguments from arguments vector.
		*aArgs = argv[2];
		if (aArgs->Tag != TYVECTOR)
			FrameExit(TERROR((LpCHAR)"SBGlue_Dir: bad arguments vector"));
		apArgsLen = FSmartbase_VectorLen(gCP,gTP,*aArgs);
		if (apArgsLen > 0) *aArg1 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(0));
		if (apArgsLen > 1) *aArg2 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(1));
		if (apArgsLen > 2) *aArg3 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(2));
		if (apArgsLen > 3) *aArg4 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(3));
	}
	switch(aFunctionHandle)
	{case 0: //_CleanUp - free resource allocated in the glue layer
		if (apDirObjectPtr != 0)
		{	delete apDirObjectPtr;
			FrameExit(TFUNCTION(0));
		}
		break;
	case 1: //abssoluteFilePath
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:absoluteFilePath, no dir object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:absoluteFilePath, arglist"));
		FSmartbase_StringPtr(gCP,gTP,aArg1, &apStr, &apStrlen); // fileName argument
		aTmpStr = apDirObjectPtr->absoluteFilePath(QString(apStr));
		*aResult = FSmartbase_CnvFromText(gCP, gTP, (char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 2: //absolutePath
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:absolutePath, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:absolutePath, arglist"));
		aTmpStr = apDirObjectPtr->absolutePath();
		*aResult = FSmartbase_CnvFromText(gCP, gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 3: //canonicalPath
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:canonicalPath, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:canonicalPath, arglist"));
		aTmpStr = apDirObjectPtr->canonicalPath();
		*aResult = FSmartbase_CnvFromText(gCP, gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 4: //cd
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:cd, no dir object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:cd, arglist"));
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&apStrlen); // dirName argument
		aTmpStr = apDirObjectPtr->cd(QString(apStr));
		*aResult = FSmartbase_CnvFromText(gCP, gTP, (char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 5: //cdUp
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:cdUp, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:cdUp, arglist"));
		aTmpBool = apDirObjectPtr->cdUp();
		*aResult = TBOOL(aTmpBool);
		FrameExit(*aResult);
		break;
	case 6: //cleanDirPath
		FrameExit(TERROR((LpCHAR)"SBGlue_Dir:cleanDirPath, obsolete"));
		break;
	case 7: //currentDirPath
		FrameExit(TERROR((LpCHAR)"SBGlue_Dir:currentDirPath, obsolete"));
		break;
	case 8: //convertToAbsolute
		FrameExit(TERROR((LpCHAR)"SBGlue_Dir:convertToAbs, obsolete"));
		break;
	case 9: //convertSeparators
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:convertSeparators, arglist"));
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&apStrlen); // path argument
		aTmpStr = QDir::convertSeparators(QString(apStr));
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 10: //count
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:count, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:count, arglist"));
		aTmpInt = apDirObjectPtr->count();
		*aResult = TINT(aTmpInt);
		FrameExit(*aResult);
		break;
	case 11: //current
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:current, arglist"));
		aTmpDir = QDir::current();
		apDirObjectPtr = new QDir(aTmpDir.path());
		FrameExit(TFUNCTION(apDirObjectPtr));
		break;
	case 12: //dirName
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:dirName, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:dirName, arglist"));
		aTmpStr = apDirObjectPtr->dirName();
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 13: //drives
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:drives, arglist"));
		aTmpFileInfoList = QDir::drives();
		aN = aTmpFileInfoList.count();
		*aTmpVector = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),2,TSYMBOL("Vector"),TINT(aN));
		// Create a vector containing copies of the QFileInfo objects owned by the QDir object
		for (aNx = 0; aNx < aN; ++aNx)
		{	aTmpFileInfo = aTmpFileInfoList[aNx];
			*aTmpVector = FSmartbase_Set(gCP,gTP,3,*aTmpVector,TINT(aNx),TFUNCTION(new QFileInfo(aTmpFileInfo)));
			++aNx;
		}
		FrameExit(*aTmpVector);
		break;
	case 14: //entryInfoList
		if (apArgsLen != 3) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:entryInfoList, arglist"));
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:entryInfoList, no dir object"));
		aTmpFilterSpec = (aArg2->Tag == TYVOID) ? apDirObjectPtr->filter() : (QDir::Filters)asInt(aArg2);
		aTmpSortSpec = (aArg3->Tag == TYVOID) ? apDirObjectPtr->sorting() : (QDir::SortFlags)asInt(aArg3);
		FSmartbase_StringPtr(gCP, gTP, aArg1, &apStr, &apStrlen);	// nameFilter argument
		aTmpFileInfoList =  (apStrlen == 0) 
			? apDirObjectPtr->entryInfoList(aTmpFilterSpec, aTmpSortSpec) 
			: apDirObjectPtr->entryInfoList(QStringList(QString(apStr)), aTmpFilterSpec, aTmpSortSpec);

		aN = aTmpFileInfoList.count();
		*aTmpVector = FSmartbase_Eval(gCP, gTP, TGVALUE("new"), 2, TSYMBOL("Vector"), TINT(aN));
		// Create a vector containing copies of the QFileInfo objects owned by the QDir object
		for (aNx = 0; aNx < aN; ++aNx)
		{	aTmpFileInfo = aTmpFileInfoList[aNx];
			*aTmpVector = FSmartbase_Set(gCP,gTP,3,*aTmpVector,TINT(aNx), TFUNCTION(new QFileInfo(aTmpFileInfo)));
		}
		FrameExit(*aTmpVector);
		break;
	case 15: //entryList
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:entryList, no dir object"));
		if (apArgsLen != 3) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:entryList, arglist"));
		aTmpFilterSpec = (aArg2->Tag == TYVOID) ? apDirObjectPtr->filter() : (QDir::Filters)asInt(aArg2);
		aTmpSortSpec = (aArg3->Tag == TYVOID) ? apDirObjectPtr->sorting() : (QDir::SortFlags)asInt(aArg3);
		FSmartbase_StringPtr(gCP, gTP, aArg1, &apStr, &apStrlen); // nameFilter argument
		aTmpStringList =  (apStrlen == 0) 
			? apDirObjectPtr->entryList(aTmpFilterSpec, aTmpSortSpec) 
			: apDirObjectPtr->entryList(QStringList(QString(apStr)), aTmpFilterSpec, aTmpSortSpec);

		// Construct return vector from aTmpStringList returned from QDir entryList method
		aN = aTmpStringList.count();
		*aTmpVector = FSmartbase_Eval(gCP, gTP, TGVALUE("new"), 3, TSYMBOL("Vector"), TSYMBOL("object"), TINT(aN));
		for (aNx=0; aNx < aN; ++aNx)
			FSmartbase_Set(gCP, gTP, 3, *aTmpVector, TINT(aNx), TSTRING(aTmpStringList[aNx].toLatin1().data()));
		FrameExit(*aTmpVector);
		break;
	case 16: //exists
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:exists, no dir object"));
		if (apArgsLen > 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:exists, arglist"));
		if (apArgsLen == 1)
		{	FSmartbase_StringPtr(gCP, gTP, aArg1, &apStr, &apStrlen); // fileName argument
			aTmpBool = apDirObjectPtr->exists(QString(apStr));
		}
		else
			aTmpBool = apDirObjectPtr->exists();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 17: //filePath
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:filePath, no dir object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:filePath, arglist"));
		FSmartbase_StringPtr(gCP, gTP, aArg1, &apStr, &apStrlen); // fileName argument
		aTmpStr = apDirObjectPtr->filePath(QString(apStr));
		*aResult = FSmartbase_CnvFromText(gCP, gTP, (char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 18: //filter
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:filter, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:filter, arglist"));
		aTmpStr = apDirObjectPtr->filter();
		*aResult = FSmartbase_CnvFromText(gCP, gTP, (char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 19: //fileInfoList
		if (apArgsLen != 3) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:fileInfoList, arglist"));
		FrameExit(TERROR((LpCHAR)"SBGlue_Dir:fileInfoList, not implemented yet"));
		break;
	case 20: //home
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:home, arglist"));
		aTmpDir = QDir::home();
		apDirObjectPtr = new QDir(aTmpDir.path());
		FrameExit(TFUNCTION(apDirObjectPtr));
		break;
	case 21: //homePah
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:homePath, arglist"));
		aTmpStr = QDir::homePath();
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 22: //isReadable
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:isReadable, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:isReadable, arglist"));
		aTmpBool = apDirObjectPtr->isReadable();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 23: //isRelative
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:isRelative, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:isRelative, arglist"));
		aTmpBool = apDirObjectPtr->isRelative();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 24: //isRelativePath
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:isRelativePath, arglist"));
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&apStrlen); // fileName argument
		aTmpBool = QDir::isRelativePath(apStr);
		FrameExit(TBOOL(aTmpBool));
		break;
	case 25: //isRoot
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:isRoot, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:isRoot, arglist"));
		aTmpBool = apDirObjectPtr->isRoot();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 26: //match
		if (apArgsLen != 2) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:match, arglist"));
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&apStrlen); // filter argument
		FSmartbase_StringPtr(gCP,gTP,aArg2,&apStr2,&apStrlen2); // fileName argument
		aTmpBool = QDir::match(QString(apStr),QString(apStr2));
		FrameExit(TBOOL(aTmpBool));
		break;
	case 27: //matchAllDirs
		FrameExit(TERROR((LpCHAR)"SBGlue_Dir:matchAllDirs, obsolete"));
		break;
	case 28: //mkDir
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:mkdir, no dir object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:mkdir, arglist"));
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&apStrlen); // pathName argument
		aTmpBool = apDirObjectPtr->mkdir(QString(apStr));
		FrameExit(TBOOL(aTmpBool));
		break;
	case 29: //nameFilters
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:nameFilters, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:nameFilters, arglist"));
		aTmpStr = apDirObjectPtr->nameFilters().join(",");
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 30: //new - Allocate C++ QtDir object
		if (apArgsLen == 0) apDirObjectPtr = new QDir;
		if (apArgsLen > 0) FSmartbase_StringPtr(gCP, gTP, aArg1, &apStr, &apStrlen); // pathName argument
		if (apArgsLen > 1) FSmartbase_StringPtr(gCP, gTP, aArg2, &apStr2, &apStrlen2); // nameFilter argument
		switch(apArgsLen)
		{
		case 1: // pathName
			apDirObjectPtr = new QDir(QString(apStr));
			break;
		case 2: // pathName nameFilter
			apDirObjectPtr = new QDir(QString(apStr), QString(apStr2));
			break;
		case 3: // pathName nameFilter sortFlags
			aTmpSortSpec = (QDir::SortFlags)asInt(aArg2);
			apDirObjectPtr = new QDir(QString(apStr), QString(apStr2), aTmpSortSpec);
			break;
		case 4: // pathName nameFilter sortFlags filterSpec
			aTmpSortSpec = (QDir::SortFlags)asInt(aArg2);
			aTmpFilterSpec = (QDir::Filters)asInt(aArg3);
			apDirObjectPtr = new QDir(QString(apStr), QString(apStr2), aTmpSortSpec, aTmpFilterSpec);
			break;
		}
		FrameExit(TFUNCTION(apDirObjectPtr));
		break;
	case 31: //path
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:path, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:path, arglist"));
		aTmpStr = apDirObjectPtr->path();
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 32: //refresh
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:refresh, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:refresh, arglist"));
		apDirObjectPtr->refresh();
		FrameExit(TBOOL(true));
		break;
	case 33: //remove
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:remove, no dir object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:remove, arglist"));
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&apStrlen); // fileName argument
		aTmpBool = apDirObjectPtr->remove(QString(apStr));
		FrameExit(TBOOL(aTmpBool));
		break;
	case 34: //rename
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:rename, no dir object"));
		if (apArgsLen != 2) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:rename, arglist"));
		FSmartbase_StringPtr(gCP, gTP, aArg1, &apStr, &apStrlen); // oldName argument
		FSmartbase_StringPtr(gCP, gTP, aArg2, &apStr2, &apStrlen2); // newName argument
		aTmpBool = apDirObjectPtr->rename(QString(apStr), QString(apStr2));
		FrameExit(TBOOL(aTmpBool));
		break;
	case 35: //rmdir
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:rmdir, no dir object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:rmdir arglist"));
		FSmartbase_StringPtr(gCP, gTP, aArg1, &apStr, &apStrlen); // dirName argument
		aTmpBool = apDirObjectPtr->rmdir(QString(apStr));
		FrameExit(TBOOL(aTmpBool));
		break;
	case 36: //root
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:root, arglist"));
		aTmpDir = QDir::root();
		apDirObjectPtr = new QDir(aTmpDir.path());
		FrameExit(TFUNCTION(apDirObjectPtr));
		break;
	case 37: //rootDirPath
		FrameExit(TERROR((LpCHAR)"SBGlue_Dir:rootDirPath, obsolete"));
		break;
	case 38: //separator
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:separator, arglist"));
		aTmpStr = QDir::separator();
		*aResult = FSmartbase_CnvFromText(gCP, gTP, (char *)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 39: //setCurrent
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:setCurrent arglist"));
		FSmartbase_StringPtr(gCP, gTP, aArg1, &apStr, &apStrlen); // path argument
		aTmpBool = QDir::setCurrent(QString(apStr));
		FrameExit(TBOOL(aTmpBool));
		break;
	case 40: //setFilter
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:setFilter, no dir object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:setFilter, arglist"));
		aTmpFilterSpec = (QDir::Filters)asInt(aArg1);
		apDirObjectPtr->setFilter(aTmpFilterSpec);
		FrameExit(TBOOL(true));
		break;
	case 41: //setMatchAllDirs
		FrameExit(TERROR((LpCHAR)"SBGlue_Dir:setMatchAllDirs, obsolete"));
		break;
	case 42: //setNameFilters
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:setNameFilters, no dir object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:setNameFilters, arglist"));
		FSmartbase_StringPtr(gCP, gTP, aArg1, &apStr, &apStrlen); // filter argument
		apDirObjectPtr->setNameFilters(QStringList(QString(apStr)));
		FrameExit(TBOOL(true));
		break;
	case 43: //setPath
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:setPath, no dir object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:setPath, arglist"));
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&apStrlen); // path argument
		apDirObjectPtr->setPath(QString(apStr));
		FrameExit(TBOOL(true));
		break;
	case 44: //setSorting
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:setSorting, no dir object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:setSorting, arglist"));
		aTmpSortSpec = (QDir::SortFlags)asInt(aArg1);
		apDirObjectPtr->setSorting(aTmpSortSpec);
		FrameExit(TBOOL(true));
		break;
	case 45: //sorting
		if (apDirObjectPtr == 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:sorting, no dir object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_Dir:sorting, arglist"));
		aTmpStr = apDirObjectPtr->sorting();
		*aResult = FSmartbase_CnvFromText(gCP, gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	} // End of Switch
	FrameExit(TERROR("!function not supported by _QtDir yet!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
NUM  SBGlue_Display(LpXCONTEXT gCP,LpTHREAD gTP, char* ipString, NUM  iNewline)
{
	QString aDsplyBfr(ipString);
	QString afinalBfr(ipString);

	// Set current result String to current display string
	gpSessionManager->setResult(gTP->SessionID, aDsplyBfr);

	// If new line is requested for display buffer, then add it
	if (iNewline > 0)
	{
		afinalBfr = aDsplyBfr + '\n';
	}

	////////////// For File Logging //////////////
	if(gCP->logMode == TRUE)
	{
		_WriteLogTime
		SysGlue_write(gCP, gTP, gCP->logFileID, aDsplyBfr.length(), (char*)afinalBfr.toAscii().constData());
		if (iNewline > 0)
		{
			gCP->logNewLineFlag = TRUE;
		}
	}
	//////////////////////////////////////////////

	// Add current display + newline to display buffer
	gpSessionManager->bfrDisplayOutput(gTP->SessionID, afinalBfr);

	return 0;
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_DisplayWorkbenchWindow(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	return(TERROR("!displayWorkbenchWindow not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
enableConsoleLog(long iSessionId, long iEnable)
	iEnable <0 to close, =0 to suspend, >0 to enable
------------------------------------------------------------------*/
TVAL SBGlue_EnableConsoleLog(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	NUM			aEnable = -1;		// Default to closing log.

	// Note that the StartFrame macro uses gCP and gTP
	StartFrame
	DeclareTVAL(aResult);
	EndFrame

	//  Expect one integer argument containing enable.
	if (argc != 1 || argv[0].Tag != TYNUM)
		*aResult = TERROR((LpCHAR)"!enableConsoleLog:Expect one integer argument!");
	else		
	{	// Call the session manager to get the console output. Convert return to TYSTRING
		aEnable = argv[0].u.Int;
		if (gpSessionManager->enableConsoleLog(gTP->SessionID, aEnable) < 0)
			*aResult = TERROR((LpCHAR)"!enableConsoleLog: Console log is not available!");
		else
			*aResult = gCP->Tval_TRUE;
	}
	FrameExit(*aResult);
}

TVAL SBGlue_CpOSFile(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	NUM aRet;
	LpCHAR aSource;
	LpCHAR aTarget;

	StartFrame
	EndFrame;

	if (argc != 2) FrameExit(TERROR((LpCHAR)"cpOSFile: need two arguments"));
	if ((argv[0].Tag != TYSTRING) && (argv[0].Tag != TYTEXT))
		FrameExit(TERROR("cpOSFile: first argument is not a filename"));
	if ((argv[1].Tag != TYSTRING) && (argv[1].Tag != TYTEXT))
		FrameExit(TERROR("cpOSFile: second argument is not a filename"));

	aSource = (argv[0].Tag == TYSTRING) ? CharArray(argv[0]) : asText(&argv[0]);
	aTarget = (argv[1].Tag == TYSTRING) ? CharArray(argv[1]) : asText(&argv[1]);

	aRet = 	SysGlue_copy(gCP,gTP,aSource,aTarget);
	FrameExit(TINT(aRet));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_EncodeURL(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	// was Functions_EncodeURL
	// TM! This routine needs to be cleaned up. It should act on the string on the smartlisp
	// heap directly and grow the smartlisp string object in chunks as necessary. I also
	// think this functions should be destructive and not create a copy.
	char	aURLEncodeBuffer[SBGLUE_URL_BUFFER_SIZE];

	// Function pointers
	TVAL	aCopyFN = TGVALUE("copy"); 
	TVAL	aStringFN = TGVALUE("string"); 

	// Protected variables 
	StartFrame
	DeclareTVAL(aTvWorkBuffer);	// Copy of argv[0] string is decoded so function is non-destructive
	EndFrame

	// Make sure we have 1 argument being passed. 
	if (argc != 1) FrameExit(TERROR((LpCHAR)"encodeURL: arglist"));

	// Make sure we have a String, Text, or a ByteVector.
	// Get a pointer to the HTML String. 
	switch (argv[0].Tag)
    {
    case TYTEXT:
    case TYSTRING:
    case TYBYTEVECTOR:
        break;
    default:
        FrameExit(TERROR((LpCHAR)"encodeURL: argtype"));
        break;
    }
	// Copy argument to return variable
	*aTvWorkBuffer = FSmartbase_Eval(gCP, gTP, aCopyFN, 1, argv[0]);
	ExitOnError(*aTvWorkBuffer);	// Check for errors

	// Make sure we have a string
	*aTvWorkBuffer = FSmartbase_Eval(gCP, gTP, aStringFN, 1, *aTvWorkBuffer);
	ExitOnError(*aTvWorkBuffer);	// Check for errors

	// Make sure we don't have buffer overflow. 
	if (strlen((char*) FSmartbase_ObjectPtr(gCP, gTP, aTvWorkBuffer)) >= SBGLUE_URL_BUFFER_SIZE)
		FrameExit(TERROR((LpCHAR)"encodeURL: 4K URL buffer overflow"));

	// Prepare URL decoding buffer
	strcpy(aURLEncodeBuffer, (char*) FSmartbase_ObjectPtr(gCP,gTP,aTvWorkBuffer));

	// Decode string and resize TVAL aResult
	*aTvWorkBuffer = FSmartbase_CnvFromText(gCP, gTP, encodeUrl(aURLEncodeBuffer, SBGLUE_URL_BUFFER_SIZE));
	ExitOnError(*aTvWorkBuffer);	// Check for errors

	// Return aResult and exit
	FrameExit(*aTvWorkBuffer);
}

/*-----------------------------------------------------------------------------------------------------------------------------
SBGlue_Escape
This function is called frequently by the engine to check if the user wants to quit processing the currently exeucting Lambda or
otherwise modify the processing state of the engine.
		==================================
This function calls into session manager to get the session's requested engine state.This information is returned in an
integer as the following bit flags.  These flags are defined in asbglue.h and in FSmartbase.h. They include:
SBGLUE_ERROR_TRACE			_FSmartbase_DEBUGON		- request for trace on error
SBGLUE_INSTRUCTION_TRACE	_FSmartbase_TRACEON		- request for instruction tracing
SBGLUE_SYSCHECKON			_FSmartbase_SYSCHECKON	- request for system checking
SBGLUE_JITON				_FSmartbase_JITON		- request for JIT processing
SBGLUE_ESCAPE				N/A						- request to stop processing

Note:Take care to keep these bit flag definitions in sync.

The function then compares the session's requested state with the current engine state and if they differ it takes appropriate
actions to change the engine state and notify session manager of the engine state after the requested changes have been made.

The escape condition is handled a bit differently that other engine state change requests. The function simply returns a value
to the caller indicating whether an escape request was found and it is the caller's responsiblity to stop processing
gracefully.

return(NUM)     The escape code. A value of:
                    (0)     indicates no escape requested. 
                    (1)     indicates escape requested.

------------------------------------------------------------------*/
#define ENGINEFLAGSMASK SBGLUE_INSTRUCTION_TRACE | SBGLUE_ERROR_TRACE | SBGLUE_SYSCHECKON
NUM SBGlue_Escape(LpXCONTEXT gCP, LpTHREAD gTP)
{
	long aResult = 0;
	long aContextIndex;
	long aCurrentState;
	long aCurrentFlag;
	NUM aFlag;

	if (gTP->SessionID == -1) 
		return(0); // Engine not yet fully initialized

	// Multitasking session escape request testing
	// MFK:	 The legacy escape testing tries to stop the context
	//       In multitasking, we need to stop only the specified session
	//		 We will need some design review here before proceeding
	if (gTP->SessionID >= 3) // Perform only for multitasking sessions
		if (gpSessionManager->setSessionEscapeFlag(gTP->SessionID,FALSE) == TRUE)
			return(1);		// Multitasking session escape requested

	aContextIndex = gCP->ContextIndex;
	for(long i=0; i<60; ++i)
	{	if (gContextMutex[aContextIndex].tryLock())
			goto lGotMutex;
			AContextThread *apSessionMgrContextThread = (AContextThread*)gCP->SessionMgrContextThread;
			apSessionMgrContextThread->localSleep(1L); // sleep 1 msec on the current thread
	}
	// If we fell through then the gContextMutex is locked up and we should die.
	_FSmartbase_Throw(SYSGLUE_MUTEXLOCKRELEASE);  // this should never happen so don't be gracefull

lGotMutex: // OK we got the mutext lock!
	if (gContextFlags[aContextIndex].mModified == 0) { // No session state change requests so return
		gContextMutex[aContextIndex].unlock();
		if (gTP->EngineStateChanged == TRUE) {
			aCurrentState = FSmartbase_SetEngineFlags(gCP, gTP, -1);
			gpSessionManager->cbSendEngineStateToSessions(gTP->SessionID, aCurrentState);
			gTP->EngineStateChanged = FALSE;
		}
		return(0);
	}
	gContextFlags[aContextIndex].mModified = 0; // Clear the modified flag

	/* Process session's state change requests */
	// Escape request.
	aFlag = gContextFlags[aContextIndex].mEscape;
	if (aFlag == 1)
	{	aResult = 1; // Returning 1 causes the engine to process the escape
		gContextFlags[aContextIndex].mEscape = -1;
		if (gTP->DebugSuspended == TRUE)
		{	// Dont process escapes during debugsuspend - just ignore them
            gContextMutex[aContextIndex].unlock();
			return(0);
		}
		goto lExit;
	}
	// System Check status change
	aFlag = gContextFlags[aContextIndex].mSystemCheck;
	if (aFlag != -1)
	{	gContextFlags[aContextIndex].mSystemCheck = -1; // Reset 
		aCurrentFlag = FSmartbase_SystemCheck(gCP,gTP,-1);
		if (aCurrentFlag != aFlag)
			FSmartbase_SystemCheck(gCP, gTP, aFlag);
	}
	// Dont process Instruction Trace or Debug flags while in DebugSuspended state
	if (gTP->DebugSuspended == TRUE) {
		gContextMutex[aContextIndex].unlock();
		return(0); 
	}
	// Instruction Trace status change
	aFlag = gContextFlags[aContextIndex].mInstructionTrace;
	if (aFlag != -1) {
		gContextFlags[aContextIndex].mInstructionTrace = -1; // Reset 
		aCurrentFlag = FSmartbase_InstructionTrace(gCP,gTP,-1);
		if (aCurrentFlag != aFlag) 
			FSmartbase_InstructionTrace(gCP, gTP, aFlag);
	}
	// Error Trace status change
	aFlag = gContextFlags[aContextIndex].mErrorTrace;
	if (aFlag != -1)
	{	gContextFlags[aContextIndex].mErrorTrace = -1; // Reset 
		aCurrentFlag = FSmartbase_ErrorTrace(gCP, gTP, -1);
		if (aCurrentFlag != aFlag)
			FSmartbase_ErrorTrace(gCP, gTP, aFlag);
	}
	// Jit status change
	aFlag = gContextFlags[aContextIndex].mJit;
	if (aFlag != -1)
	{	gContextFlags[aContextIndex].mJit = -1; // Reset
		aCurrentFlag = FSmartbase_Jit(gCP,gTP,-1);
		if (aCurrentFlag != aFlag)
			FSmartbase_Jit(gCP, gTP, aFlag);
	}
lExit:
	gContextMutex[aContextIndex].unlock();
	return aResult;
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_EvalAMP(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_EvalAMP
	return(TERROR("!evalAMP not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
   SBGlue_EvalsInSyncLocalContext

   The Functions_EvalsInSyncLocalContext function creates a new temporary context,
   evaluates the specified string, returns the aResult as a String, and closes the
   temporary context just opened.
   
   The caller passes a context workspace size, and the maximum number of threads for the
   new context. NOTE THAT THE MAXIMUM NUMBER OF THREADS IS ONE AT THIS TIME.
   Arguments:
    aWorkspaceSize		(in megabytes)
    aEvalString			(string to be evaluated synchronously in new context)

   Return:
    resultString		(results are always returned as a String)
------------------------------------------------------------------*/
TVAL SBGlue_EvalsInSyncLocalContext(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	// was Functions_EvalsInSyncLocalContext
	LpXCONTEXT	aLgCp = NULL;
	LpTHREAD	aLgTP;
	NUM			aWorkspaceSize;
	LpCHAR		aEvalString;
	NUM			aMaxBufSize = _FSmartbase_MAXBUFFERLEN;
	NUM			aRet;
	NUM 		aCatchCode;

	TVAL		aLoadObjectFN = TGVALUE("loadObject");
	TVAL		aSaveObjectFN = TGVALUE("saveObject");

	// Protected variables
	StartFrame
	DeclareTVAL(aResult);        
	DeclareTVAL(aEc);        
	EndFrame

	/*  Make sure we have the proper argument count. */
	if (argc != 2 || !isNumIndex(&argv[0]) || (argv[1].Tag != TYSTRING && argv[1].Tag != TYTEXT && argv[1].Tag != TYBYTEVECTOR))
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

	/* Compute the workspace memory size in bytes */
	aWorkspaceSize = (NUM)asNumIndex(&argv[0]) * 1000000;
	aEvalString = gCP->FSmartbase_ObjectPtr((POINTER)gCP,gTP,&argv[1]);
	aRet = createContext("...EvalsInSyncLocalContext...",gCP->ContextIndex, NULL, &aLgCp, 1, aWorkspaceSize, (NUM)0,gCP->SessionMgrContextThread);
	if (aRet != 0)
		*aResult = TERROR("!Context Initialization Error!");
	else
	{	// Evaluate a command in the temporary context. This execution is on the same thread as the calling context
		aLgTP = (LpTHREAD)&aLgCp->ThreadBlocks[0];

		// Copying the SessionID from the parent context allows us to return
		// the SessionID in callbacks. This makes it easy to return output from
		// a child context to the parent contexts session.
		aLgTP->SessionID = gTP->SessionID;

		// TRY block in case there is a C++ (or memory or ...) exception
		// But we also need the setjmp setup since SmartBase uses Throw (longjmp)
		// (this uses the one and only catch environment)
		_FSmartbase_ECatch(aLgCp,aCatchCode, LBLSystemThrowError);

		// Evaluate the user script in the temporary context. 
		*aEc = FSmartbase_Evals(aLgCp,aLgTP,aEvalString,FALSE);
		if (aEc->Tag == TYERROR)
		{	strncpy(gTP->TempBuffer,aLgCp->FSmartbase_ObjectPtr((POINTER)aLgCp, aLgTP, aEc), aMaxBufSize);
			*aResult = TERROR(gTP->TempBuffer);
		}
		else if (aEc->Tag != TYERROR)
		{	switch (aEc->Tag)
			{
			case TYVOID:
			case TYBOLE:
			case TYCHAR:
			case TYCOMPARE:
			case TYNUM:
			case TYREAL:
			case TYDATE:
			case TYMONEY:
			case TYSHORT:
			case TYTYPE:
			case TYTEXT:
				*aResult = *aEc;
				break;

			case TYSTRING:
				*aResult = FSmartbase_CnvFromText(gCP, gTP, aLgCp->FSmartbase_ObjectPtr((POINTER)aLgCp, aLgTP, aEc));
				break;

			case TYSYMBOL:
				*aResult = TSYMBOL(aLgCp->FSmartbase_ObjectPtr((POINTER)aLgCp, aLgTP, aEc));
				break;

			case TYQUOTEDSYMBOL:
				*aResult = TQSYMBOL(aLgCp->FSmartbase_ObjectPtr((POINTER)aLgCp, aLgTP, aEc));
				break;

			case TYBYTEVECTOR:
				*aResult = FSmartbase_CnvFromText(gCP, gTP, aLgCp->FSmartbase_ObjectPtr((POINTER)aLgCp, aLgTP, aEc));
				break;

			case TYBITVECTOR:
			case TYINTVECTOR:
			case TYNUMVECTOR:
			case TYFLTVECTOR:
			case TYCPX:
			case TYCPXVECTOR:
			case TYNUMMATRIX:
			case TYVECTOR:
			case TYOBJVECTOR:
			case TYSTRUCTURE:
			case TYPAIR:
			case TYQUOTEDPAIR:
			case TYLAMBDA:
			case TYMACRO:
			case TYDICTIONARY:
			case TYDIRECTORY:
			case TYMATRIX:
				*aEc = aLgCp->FSmartbase_Evalv((POINTER)aLgCp, aLgTP, aSaveObjectFN, 1, aEc);
				*aResult = gCP->FSmartbase_Evalv((POINTER)gCP, gTP, aLoadObjectFN, 1, aEc);
				break;

			default:
				*aResult = TERROR("!Invalid data type for return from temporary context!");
				break;
			}
		}
		else
		{	LBLSystemThrowError:
			// Force garbage collection for the temporary context.
 			FSmartbase_MarkAndSweep(aLgCp,aLgTP);

			// Free the memory for the temporary context and shut it down. 
			free(aLgCp);
			*aResult = TERROR(errorMsg(errorNum(aCatchCode)));
		}
	}
	FrameExit(*aResult);
}

TVAL SBGlue_ExistsOSFile(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	NUM		aRet;
	char*	apBuf;
	NUM		aBufLen;

	StartFrame
	EndFrame;

	if (argc != 1) FrameExit(TERROR((LpCHAR)"existsOSFile: need one argument"));
	if ((argv[0].Tag != TYSTRING) && (argv[0].Tag != TYTEXT))
		FrameExit(TERROR("existsOSFile: first argument is not a filename"));
	FSmartbase_StringPtr(gCP, gTP, &argv[0], &apBuf, &aBufLen);
    QFileInfo aFile(apBuf);
    if (aFile.exists() ) 
		aRet = 1;
	else
		aRet = 0;
	FrameExit(TINT(aRet));
}

/*-----------------------------------------------------------------
SBGlue_FileInfo
------------------------------------------------------------------*/
/* This function implements an interface to the QtfileInfo object for
the Lisp Lambda dir. See the lisp Lambda dir documentation and the
QFileInfo documentation for additional information about the individual
member functions of the QFileInfo object surfaced by this interface.
*/ 
TVAL SBGlue_FileInfo(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	QFileInfo*	apFileInfo;
	long		aFunctionHandle;
	char*		apStr;
	NUM			apStrlen;
	long		apArgsLen;
	long		aTmpInt;
	QString		aTmpStr;
	QString		aTmpStr1; 
	QDateTime 	aTmpDateTime;
	QDate		aTmpDate;
	QTime		aTmpTime;
	QStringList aTmpStringList;
	bool		aTmpBool;
	QDir		aTmpDir;
	QDir*		apTmpDir;
	QFileInfo*	apTmpFileInfo;

	StartFrame
	DeclareTVAL(aArgs);
	DeclareTVAL(aArg1);
	DeclareTVAL(aArg2);
	DeclareTVAL(aArg3);
	DeclareTVAL(aArg4);
	DeclareTVAL(aResult);
	EndFrame

	if (argc < 2)
		FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo, arglist")); // make sure we have the minimum number of arguments
	apFileInfo = (QFileInfo *)asFunction(&argv[0]); // Get pointer to directory object if any
	aFunctionHandle = asInt(&argv[1]); // get apFunctionHandle for use in following dispatch table

	// Note that function arguments are passed in a single vector which is the third argument of the SBGlue_FileInfo function.
	apArgsLen = 0; // Default to no arguments
	if (argc == 3)
	{	// Get Lambda arguments from arguments vector.
		*aArgs = argv[2];
		if (aArgs->Tag != TYVECTOR)
			FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo: bad arguments vector"));
		apArgsLen = FSmartbase_VectorLen(gCP,gTP,*aArgs);
		if (apArgsLen > 0) *aArg1 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(0));
		if (apArgsLen > 1) *aArg2 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(1));
		if (apArgsLen > 2) *aArg3 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(2));
		if (apArgsLen > 3) *aArg4 = FSmartbase_Ref(gCP,gTP,2,*aArgs,TINT(3));
	}
	else if (argc == 5)
	{	// only valid for new call
		*aArg1 = argv[2];
		*aArg2 = argv[3];
		*aArg3 = argv[4];
	}
	else if (argc != 2)
		FrameExit(TERROR("!Error: SBGlue_FileInfo - bad number of arguments"));
	switch(aFunctionHandle)
	{
	case 0: //_CleanUp - free resource allocated in the glue layer
		if (apFileInfo != 0)
		{	delete apFileInfo;
			FrameExit(TFUNCTION(0));
		}
		break;
	case 1: //absoluteFilePath
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:absoluteFilePath, no fileInfo object"));
		if (apArgsLen != 0 ) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:absoluteFilePath, arglist"));
		aTmpStr = apFileInfo->absoluteFilePath();
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 2: //baseName
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:baseName, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:baseName, arglist"));
		apFileInfo->baseName();
		*aResult = FSmartbase_CnvFromText(gCP, gTP, (char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 3: //cachingPath
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:caching, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:caching, arglist"));
		aTmpBool = apFileInfo->caching();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 4: //makeAbsolute
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:makeAbsolute, no fileInfo object"));
		if (apArgsLen != 0 ) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:makeAbsolute, arglist"));
		apFileInfo->makeAbsolute();
		FrameExit(TBOOL(true));
		break;
	case 5: //created
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:created, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:created, arglist"));
		aTmpDateTime = apFileInfo->created();
		aTmpDate = aTmpDateTime.date();
		aTmpTime = aTmpDateTime.time();
		*aResult = FSmartbase_Eval(gCP,gTP,TGVALUE("date"),6,
				TINT(aTmpDate.year()),
				TINT(aTmpDate.month()),
				TINT(aTmpDate.day()),
				TINT(aTmpTime.hour()),
				TINT(aTmpTime.minute()),
				TINT(aTmpTime.second()));
		FrameExit(*aResult);
		break;
	case 6: //path
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:path, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:path, arglist"));
		aTmpDir = apFileInfo->path();
		apTmpDir = new QDir(aTmpDir.absolutePath());
		FrameExit(TFUNCTION(apTmpDir));
		break;
	case 7: //absolutePath
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:absolutePath, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:absolutePath, arglist"));
		aTmpStr = apFileInfo->absolutePath();
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 8: //exists
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:exists, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:exists, arglist"));
		aTmpBool = apFileInfo->exists();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 9: //suffix
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:suffix, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:extension, arglist"));
		aTmpStr = apFileInfo->suffix();
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 10: // fileName
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:fileName, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:fileName, arglist"));
		aTmpStr = apFileInfo->fileName();
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 11: // filePath
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:filePath, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:filePath, arglist"));
		aTmpStr = apFileInfo->filePath();
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 12: //group
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:group, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:group, arglist"));
		aTmpStr = apFileInfo->group();
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 13: //groupId
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:groupId, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:groupId, arglist"));
		aTmpInt = apFileInfo->groupId();
		FrameExit(TINT(aTmpInt));
		break;
	case 14: //isDir
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isDir, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isDir, arglist"));
		aTmpBool = apFileInfo->isDir();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 15: //isExecutable
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isExecutable, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isExecutable, arglist"));
		aTmpBool = apFileInfo->isExecutable();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 16: //isFile
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isFile, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isFile, arglist"));
		aTmpBool = apFileInfo->isFile();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 17: //isHidden
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isHidden, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isHidden, arglist"));
		aTmpBool = apFileInfo->isHidden();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 18: //isReadable
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isReadable, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isReadable, arglist"));
		aTmpBool = apFileInfo->isReadable();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 19: //isRelative
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isRelative, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isRelative, arglist"));
		aTmpBool = apFileInfo->isDir();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 20: //isSymLink
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isSymLink, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isSymLink, arglist"));
		aTmpBool = apFileInfo->isSymLink();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 21: //isWritable
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isWritable, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:isWritable, arglist"));
		aTmpBool = apFileInfo->isWritable();
		FrameExit(TBOOL(aTmpBool));
		break;
	case 22: //lastModified
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:lastModified, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:lastModified, arglist"));
		aTmpDateTime = apFileInfo->lastModified();
		aTmpDate = aTmpDateTime.date();
		aTmpTime = aTmpDateTime.time();
		*aResult = FSmartbase_Eval(gCP,gTP,TGVALUE("date"), 6, TINT(aTmpDate.year()), TINT(aTmpDate.month()),
			TINT(aTmpDate.day()), TINT(aTmpTime.hour()), TINT(aTmpTime.minute()), TINT(aTmpTime.second()));
		FrameExit(*aResult);
		break;
	case 23: //lastRead
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:lastRead, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:lastRead, arglist"));
		aTmpDateTime = apFileInfo->lastRead();
		aTmpDate = aTmpDateTime.date();
		aTmpTime = aTmpDateTime.time();
		*aResult = FSmartbase_Eval(gCP,gTP,TGVALUE("date"),6, TINT(aTmpDate.year()), TINT(aTmpDate.month()),
			TINT(aTmpDate.day()), TINT(aTmpTime.hour()), TINT(aTmpTime.minute()), TINT(aTmpTime.second()));
		FrameExit(*aResult);
		break;
	case 24: // new
		// The caller is expected to pass one of the following argument signatures
		if (asInt(aArg1) == 0 && asInt(aArg2) == 0 && asInt(aArg3) == 0)
		{	// (_AISQtFileInfo #void aFunctionHandle #void #void #void) 
			apFileInfo = new QFileInfo();
		}
		else if (asInt(aArg1) != 0 && asInt(aArg2) == 0 && asInt(aArg3) == 0)
		{	// (_AISQtFileInfo #void aFunctionHandle fileName #void #void)
			FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&apStrlen); // FileName argument
			apFileInfo = new QFileInfo(QString(apStr));
		}
		else if (asInt(aArg1) != 0 && asInt(aArg2) != 0 && asInt(aArg3) == 0)
		{	// (_AISQtFileInfo #void aFunctionHandle fileName dirInstance #void)
			FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&apStrlen); // FileName argument
			apTmpDir = (QDir *)asFunction(aArg2);
			apFileInfo = new QFileInfo(*apTmpDir, QString(apStr));
		}
		else if (asInt(aArg1) == 0 && asInt(aArg2) == 0 && asInt(aArg3) != 0)
		{	// (_AISQtFileInfo #void aFunctionHandle #void #void fileInfoInstance) 
			apTmpFileInfo = (QFileInfo *)asFunction(aArg3);
			apFileInfo = new QFileInfo(*apTmpFileInfo);
		}
		else	// Error
			FrameExit(TERROR("SBGlue_FileInfo:new, arglist"));			

		FrameExit(TFUNCTION(apFileInfo));
		break;
	case 25: //owner
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:owner, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:owner, arglist"));
		aTmpStr = apFileInfo->owner();
		*aResult = FSmartbase_CnvFromText(gCP,gTP,(char*)aTmpStr.toLatin1().data());
		FrameExit(*aResult);
		break;
	case 26: //ownerId
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:ownerId, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:ownerId, arglist"));
		aTmpInt = apFileInfo->ownerId();
		FrameExit(TINT(aTmpInt));
		break;
	case 27: //permission
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:refresh, no fileInfo object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:refresh, arglist"));
		aTmpBool = apFileInfo->permission((QFile::Permissions)asInt(aArg1));
		FrameExit(TBOOL(aTmpBool));			
		break;
	case 28: //readLink
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:readLink, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:readLink, arglist"));
		aTmpStr = apFileInfo->readLink();
		apStr = (char *) aTmpStr.toLatin1().data();
		*aResult = FSmartbase_CnvFromText(gCP,gTP,apStr);
		FrameExit(*aResult);
		break;
	case 29: //refresh
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:refresh, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:refresh, arglist"));
		apFileInfo->refresh();
		FrameExit(TBOOL(true));			
		break;
	case 30: //setCaching
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:setCaching, no fileInfo object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:setFile, arglist"));
		apFileInfo->setCaching(asBool(aArg1));
		FrameExit(TBOOL(true));
		break;
	case 31: //setFile
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:setFile, no fileInfo object"));
		if (apArgsLen != 1) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:setFile, arglist"));
		FSmartbase_StringPtr(gCP,gTP,aArg1,&apStr,&apStrlen); // FileName argument
		apFileInfo->setFile(QString(apStr));
		FrameExit(TBOOL(true));
		break;
	case 32: //size
		if (apFileInfo == 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:refresh, no fileInfo object"));
		if (apArgsLen != 0) FrameExit(TERROR((LpCHAR)"SBGlue_FileInfo:refresh, arglist"));
		aTmpInt = apFileInfo->size();
		FrameExit(TINT(aTmpInt));
		break;
	} // End of Switch
	FrameExit(TERROR("!function not supported by _AISFileInfo yet!"));
}

/*-----------------------------------------------------------------
getConsoleLog(long iSessionId, bool iClear, QString& orOut)
------------------------------------------------------------------*/
TVAL SBGlue_GetConsoleLog(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	const char*	aStrptr = NULL;
	bool		aClear = true;		// Default to clear buffer after read
	QString		aMsgStr;

	// Note that the StartFrame macro uses gCP and gTP
	StartFrame
	DeclareTVAL(aResult);
	EndFrame

	//  May have zero, or one argument. The first arg is the clear-buffer-on-return flag
	if (argc > 0 && argv[0].Tag == TYBOLE)
		aClear = argv[0].u.Bool;

	// Call the session manager to get the console output. Convert return to TYSTRING
	// Return output as a aResult, not as display (some protocols can't return pushed output).
	if (gpSessionManager->getConsoleLog(gTP->SessionID, aClear, aMsgStr) < 0)
		*aResult = TERROR((LpCHAR)"!getConsoleLog: No console buffer available!");
	else
	{	if ((aStrptr = aMsgStr.toLatin1().data()) == NULL) aStrptr = "";
		*aResult = FSmartbase_CnvFromText(gCP, gTP, (char*)aStrptr);
	}
	FrameExit(*aResult);
}

TVAL SBGlue_GetContextPtr(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	char*		apName = NULL;
	NUM			aNameLen = 0;
	void*		apContextPtr;

	// Note that the StartFrame macro uses gCP and gTP
	StartFrame
	DeclareTVAL(aResult);
	EndFrame

	//  Make sure we have the proper argument count.
	if (argc != 1 )
	{	*aResult = TERROR((LpCHAR)"!getContextPtr:Incorrect number of arguments!");
		FrameExit(*aResult);
	}	
	if (argv[0].Tag != TYSTRING && argv[0].Tag != TYTEXT)
	{	*aResult = TERROR((LpCHAR)"!getContextPtr:First arg must be name of context!");
		FrameExit(*aResult);
	}
	FSmartbase_StringPtr(gCP, gTP, &argv[0], &apName, &aNameLen);
	QString aContextName(apName);
	apContextPtr = gpSessionManager->getContextPtr(aContextName);
	*aResult = TINT(apContextPtr);
	FrameExit(*aResult);
}


/*-----------------------------------------------------------------
Dead Code
SBGlue_GetDirInfo
Arguments:
dir -- name of directory to get info for or:
	- "&root"	return root directory list (or drives) for system
	- "&path"	return dir info for the context's _path directory

Returns, directory information for the speciried directory as a 
string list. Each string in the list is a del separated record 
having the following fields:
type	entry type: R=drive or root folder, C=current directory D=directory, F=file
name
size
datetime
------------------------------------------------------------------
TVAL SBGlue_GetDirInfo(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	char*	apBuf;
	long	aBufLen;

	StartFrame
	DeclareTVAL(aResult);
	EndFrame
	if (argc != 1) 
		FrameExit(TERROR((LpCHAR)"arglist")); // make sure there is a dir specification
	if (argv[0].Tag != TYSTRING && argv[0].Tag != TYTEXT) 
		FrameExit(TERROR((LpCHAR)"arglist"));
	FSmartbase_StringPtr(gCP, gTP, &argv[0], &apBuf, &aBufLen);	
	QByteArray aRet = getDirInfo(gCP, gTP, apBuf);
	*aResult = FSmartbase_CnvFromText(gCP, gTP, aRet.data());
	FrameExit(*aResult);
} */

/* ----------------------------------------------------------------------------------
   SBGlue_GetFileStatus
   TM! need to make callback into SMfunctions and use a QT QFileInfo object to
   get this information.
---------------------------------------------------------------------------------- */
TVAL SBGlue_GetFileStatus(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	return(TERROR("getFileStatus: not implemented yet")); 
}

/*-----------------------------------------------------------------
SBGlue_GetHttp
------------------------------------------------------------------*/
TVAL SBGlue_GetHttp(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	POINTER		apUrlstr = NULL;
	NUM			aUrllen = 0;
	POINTER		apFilename = NULL;
	NUM			aFilenamelen = 0;
	const char*	apPage;
	long		aRetcode;
	long		aMsecToLive = 0;

	// Protected variables
	StartFrame
	DeclareTVAL(aResult);        
	EndFrame

	//  Expect aArgs: ipUrl, ipFilename, iTimeToLive
	if (argc < 1)
	{	*aResult = TERROR((LpCHAR)"!getHttp:aArgs!");
		FrameExit(*aResult);
	}
	// Get pointer to url argument, filename
	FSmartbase_StringPtr(gCP, gTP, &argv[0], &apUrlstr, &aUrllen);
	if (argc >= 2)
	{	FSmartbase_StringPtr(gCP, gTP, &argv[1], &apFilename, &aFilenamelen);
		if (argc >= 3)
			aMsecToLive = (long)asNumIndex(&argv[2]);
	}
	// Call the SessionManager function to process the HTTP request
	aRetcode = gpSessionManager->requestHttp(gTP->SessionID, apUrlstr, NULL, apFilename, &apPage,aMsecToLive);
	if (aRetcode < 0) 
		*aResult = TERROR((LpCHAR)"!getHttp:error!");
	else
	{	*aResult = TSTRING(apPage);
		// release the memory used in session manager to buffer the page
		gpSessionManager->clearRequestHttp(gTP->SessionID);
	}
	FrameExit(*aResult);
}

TVAL SBGlue_GetOSDir(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	char			*apBuf;
	NUM				aBufLen;
	StartFrame
	DeclareTVAL(aResult);
	DeclareTVAL(aRow);
	EndFrame
	if (argc != 1) 
		FrameExit(TERROR((LpCHAR)"arglist")); // make sure there is a dir specification
	if (argv[0].Tag != TYSTRING && argv[0].Tag != TYTEXT) 
		FrameExit(TERROR((LpCHAR)"arglist"));
	*aResult = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),2,TSYMBOL("Vector"),TSYMBOL("object"));
	FSmartbase_StringPtr(gCP,gTP,&argv[0],&apBuf,&aBufLen);	
	QString path(apBuf);
	QDir d(path);
	if (!d.exists()) FrameExit(*aResult);
	d.setSorting(QDir::DirsFirst | QDir::Name);

	// QFileInfoList& arInfoList = d.entryInfoList();
	const QFileInfoList& arInfoList = d.entryInfoList();
    QFileInfo aFi;						// One FileInfo entry in the list
	long aIx, aSz = arInfoList.size();	// Index into list, size of list.
    for (aIx = 0; aIx < aSz; ++aIx)
	{	*aRow = FSmartbase_Eval(gCP, gTP, TGVALUE("new"), 2, TSYMBOL("Vector"), TINT(4));
		aFi = arInfoList[aIx];
		if (aFi.isDir())
		{	FSmartbase_Set(gCP,gTP,3,*aRow,TINT(0),TSTRING("D"));
			FSmartbase_Set(gCP,gTP,3,*aRow,TINT(1),TSTRING(aFi.fileName().toLatin1().data()));
		}
		else if (aFi.isFile())
		{	FSmartbase_Set(gCP, gTP, 3, *aRow, TINT(0), TSTRING("F"));
			FSmartbase_Set(gCP, gTP, 3, *aRow, TINT(1), TSTRING(aFi.fileName().toLatin1().data()));
			FSmartbase_Set(gCP, gTP, 3, *aRow, TINT(2), TINT(aFi.size()));
			FSmartbase_Set(gCP, gTP, 3, *aRow, TINT(1), TSTRING(aFi.lastModified().toString("ddd MMMM d yy h:m:s ap").toLatin1().data()));
		}
		FSmartbase_Set(gCP, gTP, 3, *aResult, TINT(aIx), *aRow);
	}
	FrameExit(*aResult);
}

// The clock() function calls behave differently between Windows and Linux. We will use gettimeofday to determine the elapsed time instead for linux in both 32 and 64 bit.
#if defined(_LINUX)
/*-----------------------------------------------------------------
	SBGlue_GetTickCount
	Return the amount of time, in seconds, since the system was booted.
------------------------------------------------------------------*/
TVAL SBGlue_GetTickCount(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	// was Functions_LispGetTickCount 
	REAL	aOldTime;
	REAL	aNewTime;
	struct timeval aTv;
	time_t aSeconds;
	suseconds_t aMseconds;

	StartFrame
	DeclareTVAL(aRet);
	EndFrame

	if (argc != 1) FrameExit(TERROR((LpCHAR)"arglist"));
	//  Convert old time argument to real.
	if (asTag(&argv[0]) != TYREAL)
	{	*aRet = FSmartbase_Convert(gCP, gTP, TYREAL, argv[0]);
		ExitOnError(*aRet);
		aOldTime = aRet->u.Real;
	}
	else
		aOldTime = argv[0].u.Real;

	gettimeofday(&aTv, NULL);
	aSeconds = aTv.tv_sec;
	aMseconds = aTv.tv_usec;
	aNewTime = (double)(aSeconds / 1.0) + (double)(aMseconds / (double)1000000.0);
	// Subtract the time when the program started(gStartClock initialized in SBGlue_Init)
	aNewTime = aNewTime - gStartClock;

	aRet->u.Real = aNewTime - aOldTime;
	aRet->Tag = TYREAL;
	FrameExit(*aRet);
}
#else
/*-----------------------------------------------------------------
	SBGlue_GetTickCount
	Return the amount of time, in seconds, since the system was booted.
------------------------------------------------------------------*/
TVAL SBGlue_GetTickCount(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	// was Functions_LispGetTickCount 
	REAL	aOldTime;
	REAL	aNewTime;

	StartFrame
	DeclareTVAL(aRet);
	EndFrame

	if (argc != 1) FrameExit(TERROR((LpCHAR)"arglist"));

	//  Convert old time argument to real.
	if (asTag(&argv[0]) != TYREAL)
	{	*aRet = FSmartbase_Convert(gCP, gTP, TYREAL, argv[0]);
		ExitOnError(*aRet);
		aOldTime = aRet->u.Real;
	}
	else
		aOldTime = argv[0].u.Real;

	/*  Subtract old time argument from new time. */
	aNewTime = clock();
	aRet->Tag = TYREAL;
	aRet->u.Real = (aNewTime / (REAL) CLOCKS_PER_SEC) - aOldTime ; // in time.h
	FrameExit(*aRet);
}
#endif

/*-----------------------------------------------------------------
	SBGlue_WriteTime
	Write the time to logfile.
	This call will always set gCP->logNewLineFlag to FALSE
------------------------------------------------------------------*/
void SBGlue_WriteTime(LpXCONTEXT gCP, LpTHREAD gTP)
{
	time_t	aNewTime;
	NUM		aCurrentTime;
	char	aTimeStr[12];
	NUM		aTimeSec = 0;
	NUM		aTimeMin = 0;
	NUM		aTimeHr = 0;

	// Return if we have an invalid log file and if logging mode/time is disabled
	if( gCP->logFileID <= 0 || gCP->logMode == FALSE)
		return;

	time(&aNewTime);
	aCurrentTime = (aNewTime - gCP->logStartTime);
	aTimeHr = aCurrentTime / 3600;
	aCurrentTime = aCurrentTime % 3600;
	aTimeMin = aCurrentTime / 60;
	aTimeSec = aCurrentTime % 60;

#if defined(_M64) && defined(_MSVC)
    sprintf(aTimeStr, "%04I64d:%02I64d:%02I64d ", aTimeHr, aTimeMin, aTimeSec);
#else
	sprintf(aTimeStr, "%04ld:%02ld:%02ld ", aTimeHr, aTimeMin, aTimeSec);
#endif

	SysGlue_write(gCP, gTP, gCP->logFileID, strlen(aTimeStr), aTimeStr);

	gCP->logNewLineFlag = FALSE;
}

/*-----------------------------------------------------------------
	SBGlue_OpenLog
	Opens a logfile, returns TRUE if successful.
	Thea following symbols are optional arguments:
	append: or clear:
	timing: or notiming:

	The note to be written on the log file should always be the last argument.
------------------------------------------------------------------*/
TVAL SBGlue_OpenLog(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	StartFrame
	DeclareTVAL(aRet);
	EndFrame
	NUM aFileID = -1;
	*aRet = gCP->TObject_TRUE;

	// Setting the default values
	NUM aOpenMode = 1;  // Set to clear: as default which should be "mode" value for fileOpen
	gCP->logTime = TRUE;

	if (argc == 0 || argc > 4)
	{
		FrameExit(TERROR((LpCHAR)"arglist"));
	}

	if (argv[0].Tag != TYSTRING && argv[0].Tag != TYTEXT)
	{
		// Argument is not valid, return arglist
		FrameExit(TERROR((LpCHAR)"arglist"));
	}

	if (argc == 2 )
	{
		if( ((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL)) && // Check if 2nd TVAL is a symbol
		((strcmp(SymbolArray(argv[1]),"append") != 0) && (strcmp(SymbolArray(argv[1]),"clear") != 0) &&
		 (strcmp(SymbolArray(argv[1]),"timing") != 0) && (strcmp(SymbolArray(argv[1]),"notiming") != 0)
		))
		{
			// Second Argument is not a symbol, check if it is a note.
			if(argv[1].Tag != TYSTRING && argv[1].Tag != TYTEXT)
			{
				// Argument is not valid, return arglist
				FrameExit(TERROR((LpCHAR)"arglist"));
			}
		}

		
		if((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL))
		{
			if(strcmp(SymbolArray(argv[1]),"append") == 0)
			{
				aOpenMode = 0;
			}

			if(strcmp(SymbolArray(argv[1]),"clear") == 0)
			{
				aOpenMode = 1;
			}

			if(strcmp(SymbolArray(argv[1]),"notiming") == 0)
			{
				gCP->logTime = FALSE;
			}
		}

	}
	else
	if (argc == 3 )
	{
		if( ((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL)) && // Check if 2nd TVAL is a symbol
		((strcmp(SymbolArray(argv[1]),"append") != 0) && (strcmp(SymbolArray(argv[1]),"clear") != 0) &&
		 (strcmp(SymbolArray(argv[1]),"timing") != 0) && (strcmp(SymbolArray(argv[1]),"notiming") != 0)
		))
		{
			// Argument is not valid, return arglist
			FrameExit(TERROR((LpCHAR)"arglist"));
		}
		if( ((argv[2].Tag == TYSYMBOL) || (argv[2].Tag == TYQUOTEDSYMBOL)) && // Check if 3rd TVAL is a symbol
		((strcmp(SymbolArray(argv[2]),"append") != 0) && (strcmp(SymbolArray(argv[2]),"clear") != 0) &&
		 (strcmp(SymbolArray(argv[2]),"timing") != 0) && (strcmp(SymbolArray(argv[2]),"notiming") != 0)
		))
		{
			if(argv[2].Tag != TYSTRING && argv[2].Tag != TYTEXT) // Final Check if argument is a note that should be written on the log file.
			{
				// Argument is not valid, return arglist
				FrameExit(TERROR((LpCHAR)"arglist"));
			}
		}

		if(strcmp(SymbolArray(argv[1]),"append") == 0)
		{
			aOpenMode = 0;
		}

		if(strcmp(SymbolArray(argv[1]),"clear") == 0)
		{
			aOpenMode = 1;
		}

		if(strcmp(SymbolArray(argv[1]),"notiming") == 0)
		{
			gCP->logTime = FALSE;
		}
		else
		if(strcmp(SymbolArray(argv[1]),"timing") == 0)
		{
			gCP->logTime = TRUE;
		}

		if(argv[2].Tag == TYSYMBOL || argv[2].Tag == TYQUOTEDSYMBOL) 
		{
			if(strcmp(SymbolArray(argv[2]),"notiming") == 0)
			{
				gCP->logTime = FALSE;
			}
			else
			if(strcmp(SymbolArray(argv[2]),"timing") == 0)
			{
				gCP->logTime = FALSE;
			}
			else
			if(strcmp(SymbolArray(argv[2]),"append") == 0)
			{
				aOpenMode = 0;
			}
			else
			if(strcmp(SymbolArray(argv[2]),"clear") == 0)
			{
				aOpenMode = 1;
			}
		}
	}
	else
	if (argc == 4 )
	{
		if( ((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL)) && // Check if 2nd TVAL is a symbol
		((strcmp(SymbolArray(argv[1]),"append") != 0) && (strcmp(SymbolArray(argv[1]),"clear") != 0) &&
		 (strcmp(SymbolArray(argv[1]),"timing") != 0) && (strcmp(SymbolArray(argv[1]),"notiming") != 0)
		))
		{
			FrameExit(TERROR((LpCHAR)"arglist"));
		}
		if( ((argv[2].Tag == TYSYMBOL) || (argv[2].Tag == TYQUOTEDSYMBOL)) && // Check if 2nd TVAL is a symbol
		((strcmp(SymbolArray(argv[2]),"append") != 0) && (strcmp(SymbolArray(argv[2]),"clear") != 0) &&
		 (strcmp(SymbolArray(argv[2]),"timing") != 0) && (strcmp(SymbolArray(argv[2]),"notiming") != 0)
		))
		{
			FrameExit(TERROR((LpCHAR)"arglist"));
		}
		if( ((argv[3].Tag == TYSYMBOL) || (argv[3].Tag == TYQUOTEDSYMBOL)) && // Check if 3rd TVAL is a symbol
		((strcmp(SymbolArray(argv[3]),"append") != 0) && (strcmp(SymbolArray(argv[3]),"clear") != 0) &&
		 (strcmp(SymbolArray(argv[3]),"timing") != 0) && (strcmp(SymbolArray(argv[3]),"notiming") != 0)
		))
		{
			if(argv[3].Tag != TYSTRING && argv[3].Tag != TYTEXT) // Final Check if argument is a note that should be written on the log file.
			{
				FrameExit(TERROR((LpCHAR)"arglist"));
			}
		}

		// Check for Mode if append: or clear:
		if(strcmp(SymbolArray(argv[1]),"append") == 0)
		{
			aOpenMode = 0;
		}
		else 
		if(strcmp(SymbolArray(argv[1]),"clear") == 0)
		{
			aOpenMode = 1;
		}
		else
		if(strcmp(SymbolArray(argv[2]),"append") == 0)
		{
			aOpenMode = 0;
		}
		else
		if(strcmp(SymbolArray(argv[2]),"clear") == 0)
		{
			aOpenMode = 1;
		}

		if((strcmp(SymbolArray(argv[1]),"notiming") == 0) || (strcmp(SymbolArray(argv[2]),"notiming") == 0))
		{
			gCP->logTime = FALSE;
		}

		if((argv[3].Tag == TYSYMBOL || argv[3].Tag == TYQUOTEDSYMBOL) && (strcmp(SymbolArray(argv[3]),"append") == 0))
		{
			aOpenMode = 0;
		}
		else
		if((argv[3].Tag == TYSYMBOL || argv[3].Tag == TYQUOTEDSYMBOL) && (strcmp(SymbolArray(argv[3]),"clear") == 0))
		{
			aOpenMode = 1;
		}

		if((argv[3].Tag == TYSYMBOL || argv[3].Tag == TYQUOTEDSYMBOL) && (strcmp(SymbolArray(argv[3]),"notiming") == 0))
		{
			gCP->logTime = FALSE;
		}
	}

	if(gCP->logFileID != -1)
	{
		// There is an open log, close it first
		SysGlue_close( gCP, gTP, gCP->logFileID, 1);
	}

	if(argv[0].Tag == TYSTRING)
	{
		aFileID = SysGlue_open( gCP, gTP, CharArray(argv[0]), aOpenMode, 0); // Last Argument is 0 for text file.
	}
	else
	if(argv[0].Tag == TYTEXT)
	{
		aFileID = SysGlue_open( gCP, gTP, argv[0].u.Text, aOpenMode, 0); // Last Argument is 0 for text file.
	}

	if(aFileID <= 0)
	{
		FrameExit(TERROR((LpCHAR)"SysGlue_OpenLog Failed."));
	}


	gCP->logFileID = aFileID;
	gCP->logMode = TRUE;
	gCP->logNewLineFlag = TRUE;
	time(&gCP->logStartTime);

	// Seek to end-of-file
	SysGlue_seek(gCP, gTP, gCP->logFileID, 0, 2);

	// Check if the last argument(aside from the log's filename) is a note to be written to log.
	if( argc > 1 && argv[argc-1].Tag == TYSTRING)
	{
		_WriteLogTime
		SysGlue_write(gCP, gTP, gCP->logFileID, strlen(CharArray(argv[argc-1])), CharArray(argv[argc-1]));
		SysGlue_write(gCP, gTP, gCP->logFileID, 1, "\n");
	}
	else
	if( argc > 1 && argv[argc-1].Tag == TYTEXT)
	{
		_WriteLogTime
		SysGlue_write(gCP, gTP, gCP->logFileID, strlen(argv[argc-1].u.Text), argv[argc-1].u.Text);
		SysGlue_write(gCP, gTP, gCP->logFileID, 1, "\n");
	}

	// Set gCP->logNewLineFlag to TRUE always after this call
	// so that the next display would display the timestamp
	// if gCP->logTime is set to TRUE.
	gCP->logNewLineFlag = TRUE;

	FrameExit(*aRet);
}

/*-----------------------------------------------------------------
	SBGlue_StartLogging
	Description here
------------------------------------------------------------------*/
TVAL SBGlue_StartLogging(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	StartFrame
	DeclareTVAL(aRet);
	EndFrame
	*aRet = gCP->TObject_TRUE;

	if (argc > 3)
	{
		FrameExit(TERROR((LpCHAR)"arglist"));
	}

	// If no log was openned before, do nothing.
	if( gCP->logFileID == -1 )
	{
		FrameExit(gCP->TObject_FALSE);
	}

	if (argc == 1 )
	{
		if( ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL)) && // Check if 1st TVAL is a symbol
		((strcmp(SymbolArray(argv[0]),"resettime") != 0) && (strcmp(SymbolArray(argv[0]),"notiming") != 0) &&
		 (strcmp(SymbolArray(argv[0]),"timing") != 0)
		))
		{
			if(argv[0].Tag != TYSTRING && argv[0].Tag != TYTEXT) // Final Check if argument is a note that should be written on the log file.
			{
				FrameExit(TERROR((LpCHAR)"arglist"));
			}
		}

		gCP->logTime = TRUE;
		if((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
		{
			if(strcmp(SymbolArray(argv[0]),"notiming") == 0)
			{
				gCP->logTime = FALSE;
			}

			if(strcmp(SymbolArray(argv[0]),"resettime") == 0)
			{
				time(&gCP->logStartTime);
			}
		}
	}
	else
	if (argc == 2 )
	{
		if( ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL)) && // Check if 1st TVAL is a symbol
		((strcmp(SymbolArray(argv[0]),"resettime") != 0) && (strcmp(SymbolArray(argv[0]),"notiming") != 0) &&
		 (strcmp(SymbolArray(argv[0]),"timing") != 0)
		))
		{
			FrameExit(TERROR((LpCHAR)"arglist"));
		}

		if( ((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL)) && // Check if 2nd TVAL is a symbol
		((strcmp(SymbolArray(argv[1]),"resettime") != 0) && (strcmp(SymbolArray(argv[1]),"notiming") != 0) &&
		 (strcmp(SymbolArray(argv[1]),"timing") != 0)
		))
		{
			if(argv[1].Tag != TYSTRING && argv[1].Tag != TYTEXT) // Final Check if argument is a note that should be written on the log file.
			{
				FrameExit(TERROR((LpCHAR)"arglist"));
			}
		}

		gCP->logTime = TRUE;
		if((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
		{
			if(strcmp(SymbolArray(argv[0]),"notiming") == 0)
			{
				gCP->logTime = FALSE;
			}
			else
			if(strcmp(SymbolArray(argv[0]),"resettime") == 0)
			{
				time(&gCP->logStartTime);
			}
		}

		if((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL))
		{
			if(strcmp(SymbolArray(argv[1]),"notiming") == 0)
			{
				gCP->logTime = FALSE;
			}
			else
			if(strcmp(SymbolArray(argv[1]),"resettime") == 0)
			{
				time(&gCP->logStartTime);
			}
		}
	}
	else
	if(argc == 3)
	{
		if( ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL)) && // Check if 1st TVAL is a symbol
		((strcmp(SymbolArray(argv[0]),"resettime") != 0) && (strcmp(SymbolArray(argv[0]),"notiming") != 0) &&
		 (strcmp(SymbolArray(argv[0]),"timing") != 0)
		))
		{
			FrameExit(TERROR((LpCHAR)"arglist"));
		}

		if( ((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL)) && // Check if 2nd TVAL is a symbol
		((strcmp(SymbolArray(argv[1]),"resettime") != 0) && (strcmp(SymbolArray(argv[1]),"notiming") != 0) &&
		 (strcmp(SymbolArray(argv[1]),"timing") != 0)
		))
		{
			if(argv[1].Tag != TYSTRING && argv[1].Tag != TYTEXT) // Final Check if argument is a note that should be written on the log file.
			{
				FrameExit(TERROR((LpCHAR)"arglist"));
			}
		}

		if( ((argv[2].Tag == TYSYMBOL) || (argv[2].Tag == TYQUOTEDSYMBOL)) && // Check if 3rd TVAL is a symbol
		((strcmp(SymbolArray(argv[2]),"resettime") != 0) && (strcmp(SymbolArray(argv[2]),"notiming") != 0) &&
		 (strcmp(SymbolArray(argv[2]),"timing") != 0)
		))
		{
			if(argv[2].Tag != TYSTRING && argv[2].Tag != TYTEXT) // Final Check if argument is a note that should be written on the log file.
			{
				FrameExit(TERROR((LpCHAR)"arglist"));
			}
		}

		gCP->logTime = TRUE;
		if((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
		{
			if(strcmp(SymbolArray(argv[0]),"notiming") == 0)
			{
				gCP->logTime = FALSE;
			}

			if(strcmp(SymbolArray(argv[0]),"resettime") == 0)
			{
				time(&gCP->logStartTime);
			}
		}

		if((argv[1].Tag == TYSYMBOL) || (argv[1].Tag == TYQUOTEDSYMBOL))
		{
			if(strcmp(SymbolArray(argv[1]),"notiming") == 0)
			{
				gCP->logTime = FALSE;
			}

			if(strcmp(SymbolArray(argv[1]),"resettime") == 0)
			{
				time(&gCP->logStartTime);
			}
		}

		if((argv[2].Tag == TYSYMBOL) || (argv[2].Tag == TYQUOTEDSYMBOL))
		{
			if(strcmp(SymbolArray(argv[2]),"notiming") == 0)
			{
				gCP->logTime = FALSE;
			}

			if(strcmp(SymbolArray(argv[2]),"resettime") == 0)
			{
				time(&gCP->logStartTime);
			}
		}
	}

	gCP->logMode = TRUE;
	gCP->logNewLineFlag = TRUE;

	// Check if the last argument is a note to be written to log.
	if(argc > 0 && argv[argc-1].Tag == TYSTRING)
	{
		_WriteLogTime
		SysGlue_write(gCP, gTP, gCP->logFileID, strlen(CharArray(argv[argc-1])), CharArray(argv[argc-1]));
		SysGlue_write(gCP, gTP, gCP->logFileID, 1, "\n");
	}
	else
	if(argc > 0 && argv[argc-1].Tag == TYTEXT)
	{
		_WriteLogTime
		SysGlue_write(gCP, gTP, gCP->logFileID, strlen(argv[argc-1].u.Text), argv[argc-1].u.Text);
		SysGlue_write(gCP, gTP, gCP->logFileID, 1, "\n");
	}

	// Set gCP->logNewLineFlag to TRUE always after this call
	// so that the next display would display the timestamp
	// if gCP->logTime is set to TRUE.
	gCP->logNewLineFlag = TRUE;

	FrameExit(*aRet);
}

/*-----------------------------------------------------------------
	SBGlue_StopLogging
	Description here
------------------------------------------------------------------*/
TVAL SBGlue_StopLogging(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	StartFrame
	EndFrame

	if (argc > 1)
	{
		FrameExit(TERROR((LpCHAR)"arglist"));
	}
	else
	if (argc == 1 && (argv[0].Tag != TYSTRING && argv[0].Tag != TYTEXT))
	{
		FrameExit(TERROR((LpCHAR)"arglist"));
	}

	// If no log was openned before, do nothing.
	if( gCP->logFileID == -1 )
	{
		FrameExit(gCP->TObject_FALSE);
	}

	if(argc == 1)
	{
		// Check if the last argument is a note to be written to log.
		if(argv[argc-1].Tag == TYSTRING)
		{
			_WriteLogTime
			SysGlue_write(gCP, gTP, gCP->logFileID, strlen(CharArray(argv[argc-1])), CharArray(argv[argc-1]));
			SysGlue_write(gCP, gTP, gCP->logFileID, 1, "\n");
		}
		else
		if(argv[argc-1].Tag == TYTEXT)
		{
			_WriteLogTime
			SysGlue_write(gCP, gTP, gCP->logFileID, strlen(argv[argc-1].u.Text), argv[argc-1].u.Text);
			SysGlue_write(gCP, gTP, gCP->logFileID, 1, "\n");
		}
	}

	gCP->logMode = FALSE;
	gCP->logNewLineFlag = TRUE;

	FrameExit(gCP->TObject_TRUE);
}

/*-----------------------------------------------------------------
	SBGlue_CloseLog
	Description here
------------------------------------------------------------------*/
TVAL SBGlue_CloseLog(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	StartFrame
	DeclareTVAL(aRet);
	EndFrame
	*aRet = gCP->TObject_TRUE;

	if (argc > 1)
	{
		FrameExit(TERROR((LpCHAR)"arglist"));
	}

	if(argc == 1 && (argv[0].Tag != TYSTRING && argv[0].Tag != TYTEXT)) // Final Check if argument is a note that should be written on the log file.
	{
		FrameExit(TERROR((LpCHAR)"arglist"));
	}

	// If no log was openned before, do nothing.
	if( gCP->logFileID == -1)
	{
		FrameExit(gCP->TObject_FALSE);
	}

	if(gCP->logMode == TRUE)
	{
		// Set gCP->logNewLineFlag to TRUE so that the note will be written on the next line.
		gCP->logNewLineFlag = TRUE;

		// Check if the last argument is a note to be written to log.
		if(argc == 1 && argv[argc-1].Tag == TYSTRING)
		{
			_WriteLogTime
			SysGlue_write(gCP, gTP, gCP->logFileID, strlen(CharArray(argv[argc-1])), CharArray(argv[argc-1]));
			SysGlue_write(gCP, gTP, gCP->logFileID, 1, "\n");
		}
		else
		if(argc == 1 && argv[argc-1].Tag == TYTEXT)
		{
			_WriteLogTime
			SysGlue_write(gCP, gTP, gCP->logFileID, strlen(argv[argc-1].u.Text), argv[argc-1].u.Text);
			SysGlue_write(gCP, gTP, gCP->logFileID, 1, "\n");
		}
	}

	// Close the log file
	SysGlue_close(gCP, gTP, gCP->logFileID, 1);

	// Load back default values
	gCP->logFileID = -1;
	gCP->logTime = TRUE;
	gCP->logMode = FALSE;

	FrameExit(*aRet);
}

NUM SBGlue_GetFileID(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(gTP);
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	return gCP->logFileID;
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_HostCreate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_LispCreate
	return(TERROR("!hostCreate not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_HostDestroy(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_LispDestroy
	return(TERROR("!hostDestroy not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_HostGetProperty(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_LispGetProperty
	return(TERROR("!hostGetProperty not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_HostInvoke(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_LispInvoke
	return(TERROR("!hostInvoke not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_HostPutProperty(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_LispPutProperty
	return(TERROR("!hostPutProperty not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_HostSend(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_LispSend
	return(TERROR("!hostSend not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_Input(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_LispInput
	return(TERROR("!input not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_LoadBrowseLambda(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(gTP);
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_LoadBrowseLambda
	return(gCP->Tval_TRUE);
}

/*-----------------------------------------------------------------
SBGlue_LoadLib
Arguments:
	libpath	-- path to library to load. 
Returns:
	aResult
------------------------------------------------------------------*/
TVAL SBGlue_LoadLib(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	char			*apBuf;
	NUM				aBufLen;
	StartFrame
	DeclareTVAL(aResult);
	EndFrame

	if (argc != 1) 
		FrameExit(TERROR((LpCHAR)"arglist")); // make sure there is a path specification
	if (argv[0].Tag != TYSTRING && argv[0].Tag != TYTEXT) 
		FrameExit(TERROR((LpCHAR)"arglist"));
	FSmartbase_StringPtr(gCP,gTP,&argv[0],&apBuf,&aBufLen);	
	QString libpath(apBuf);
	*aResult = TBOOL(DynamicLibs.loadLib(gCP,gTP,libpath));
	FrameExit(*aResult);
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_LoadUtilityLambdas(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(gTP);
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_LoadUtilityLambdas
	return(gCP->Tval_TRUE);
}

TVAL SBGlue_MkOSDir(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	char*	apBuf;
	NUM		aBufLen;
	bool	aRet;

	StartFrame
	DeclareTVAL(aResult);
	EndFrame

	if (argc != 1) 
		FrameExit(TERROR((LpCHAR)"arglist")); // make sure there is a dir specification
	if (argv[0].Tag != TYSTRING && argv[0].Tag != TYTEXT) 
		FrameExit(TERROR((LpCHAR)"arglist"));
	FSmartbase_StringPtr(gCP, gTP, &argv[0], &apBuf, &aBufLen);	
	QString aCmdString(apBuf);
    QDir aDir(aCmdString ); 
    if (!aDir.exists() )	
		aRet = aDir.mkdir(aCmdString);
	else
		aRet = true;

	*aResult = TBOOL(aRet);
	FrameExit(*aResult);
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_MsgBox(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	// was Functions_LispMsgBox
	char *apStr;
	NUM	 apStrlen;
	if (argc > 0)
	{	FSmartbase_StringPtr(gCP,gTP,&argv[0],&apStr,&apStrlen);
		return(TERROR(apStr));
	}
	return(TERROR("!msgBox not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_NextUserID(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_NextUserID
	return(TERROR("!nextUserID not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_Notify(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Functions_LispNotify
	return(TERROR("!notify not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
NUM	SBGlue_Open(LpXCONTEXT gCP, LpTHREAD gTP, char* ipName, NUM iMode, NUM iType)
{
	char aFileNameBuffer[1024];
	NUM aResult;

	/* If the name is not null, call SysGlue_Open() */
	if (*ipName != '\0')
		aResult = SysGlue_open(gCP, gTP, ipName, iMode, iType);
	else 
	{	// Get file name from user by calling SessionManager file open dialog
		aResult = gpSessionManager->serverFileOpenDialog(gTP->SessionID, gTP->RequestID, aFileNameBuffer, 1024);
		if (aResult == 0 && aFileNameBuffer[0] != 0) 
			aResult = SysGlue_open(gCP, gTP, aFileNameBuffer, iMode, iType);
	}
	return aResult;
}

/*-----------------------------------------------------------------
openConsoleLog(long iSessionId, long iMaxSize, bool iRedirect)
------------------------------------------------------------------*/
TVAL SBGlue_OpenConsoleLog(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{

	bool	aClear = true;			// Delete the existing log if any.
	bool	aRedirect = true;		// Redirect console output to log only
	NUM		aSize = -1;				// Default to use default size.
	bool	aStartAtNewline = false;// Start at first char of log

	// Note that the StartFrame macro uses gCP and gTP
	StartFrame
	DeclareTVAL(aResult);
	EndFrame

	//  Expect one integer argument containing the maximum size of buffer and one optional arg.
	if (argc < 1 || argv[0].Tag != TYNUM)
		*aResult = TERROR((LpCHAR)"!openConsoleLog:Expect one integer argument!");
	else	// Call the session manager to get the console output. Return error message or true
	{	if (argc > 1 && argv[1].Tag == TYBOLE)
			aRedirect = argv[1].u.Bool;
		aSize = argv[0].u.Int;
		if (gpSessionManager->openConsoleLog(aClear, aRedirect, gTP->SessionID, aSize, aStartAtNewline) < 0) 
			*aResult = TERROR((LpCHAR)"!openConsoleLog: Invalid sessionID!");
		else 
			*aResult = gCP->Tval_TRUE;
	}
	FrameExit(*aResult);
}

/*-----------------------------------------------------------------
SBGlue_PostHttp
------------------------------------------------------------------*/
TVAL SBGlue_PostHttp(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	POINTER	apUrlstr = NULL;
	NUM		aUrlLgth = 0;
	POINTER	apBody = NULL;
	NUM		aBodyLgth = 0;
	POINTER	apName = NULL;
	NUM		aNameLgth = 0;
	POINTER	apValue = NULL;
	NUM		aValueLgth = 0;
	POINTER	apFilename = NULL;
	NUM		aFilenameLgth = 0;

const char*	apPage;
	long	aRetcode;
	long	aMsecToLive = 0;
	QByteArray aBody, aPair;

	// Protected variables
	StartFrame
	DeclareTVAL(aName);
	DeclareTVAL(aNameStr);
	DeclareTVAL(aResult);
	DeclareTVAL(aValueStr);
	DeclareTVAL(aValue);
	EndFrame

	//  Expect aArgs: ipUrl, iBody, ipFilename, iTimeToLive
	if (argc < 2 )
	{	*aResult = TERROR((LpCHAR)"!postHttp:need 2+ aArgs!");
		FrameExit(*aResult);
	}
	// Get pointer to url argument, post body, filename
	FSmartbase_StringPtr(gCP, gTP, &argv[0], &apUrlstr, &aUrlLgth);

	// Convert the structure into a DEL-delimited string
	if (argv[1].Tag == TYSTRUCTURE)
	{	*aResult = argv[1];
		long aLgth = FSmartbase_VectorLen(gCP, gTP, *aResult);

		for (long i = 0; i < aLgth;)
		{	// (setq name (ref aResult i 0))
			*aName = FSmartbase_Ref(gCP, gTP, 3, *aResult, TINT(i), TINT(0));
			*aNameStr = FSmartbase_Convert(gCP, gTP, TYSTRING, *aName);
			FSmartbase_StringPtr(gCP, gTP, aNameStr, &apName, &aNameLgth);
			// (setq value (ref aResult i 1))
			*aValue = FSmartbase_Ref(gCP,gTP,3,*aResult,TINT(i),TINT(1));
			if (aValue->Tag == TYSTRING || aValue->Tag == TYTEXT) 
				*aValueStr = FSmartbase_Convert(gCP, gTP,TYSTRING, *aValue);
			else *aValueStr = FSmartbase_Eval(gCP, gTP, TGVALUE("string"), 2, *aValue, TBOOL(true));

			if (aValueStr->Tag == TYERROR)
			{	*aResult = TERROR((LpCHAR)
					"!postHttp: Structure cannot be converted to string!");
				FrameExit(*aResult);			
			}
			FSmartbase_StringPtr(gCP, gTP, aValueStr, &apValue, &aValueLgth);
			aPair = apName;
			aPair += '\177';
			aPair += apValue;
			aBody += aPair;
			if (++i < aLgth)
				aBody += '\177';
		}
		apBody = aBody.data();
	}
	else	// Get ptr to string
		FSmartbase_StringPtr(gCP, gTP, &argv[1], &apBody, &aBodyLgth);

	if (argc >= 3)
	{	FSmartbase_StringPtr(gCP, gTP, &argv[2], &apFilename, &aFilenameLgth);
		if (argc >= 4)
			aMsecToLive = (long)asNumIndex(&argv[3]);
	}
	// Call the SessionManager function to process the HTTP request
	aRetcode = gpSessionManager->requestHttp(gTP->SessionID, apUrlstr, apBody, apFilename, &apPage,	aMsecToLive);
	if (aRetcode < 0) 
		*aResult = TERROR((LpCHAR)"!postHttp:error!");
	else
	{	*aResult = TSTRING(apPage);
		// release the memory used in session manager to buffer the page
		gpSessionManager->clearRequestHttp(gTP->SessionID);
	}
	FrameExit(*aResult);
}

/*-----------------------------------------------------------------
SBGlue_ReadHtmlPage
------------------------------------------------------------------*/
TVAL SBGlue_ReadHtmlPage(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	POINTER	urlstr = NULL;
	NUM		urllen = 0;
	const char	*pageContent;
	NUM		pageSize;
	long	retcode;
	long	timeToLive = 0;

	// Protected variables
	StartFrame
	DeclareTVAL(aResult);
	EndFrame

	/*  Make sure we have the proper argument count. */
	if (argc < 1 ) {
		*aResult = TERROR((LpCHAR)"!readHtmlPage:aArgs!");
		FrameExit(*aResult);
		}	
	if (argc == 2) 
		timeToLive = (long)asNumIndex(&argv[1]);

	// Get pointer to url argument
	FSmartbase_StringPtr(gCP,gTP,&argv[0],&urlstr,&urllen);

	// Add the http prefix (if it is missing).
	// Note1: Internet html files, with missing http prefixes,
	//        will not read properly.
	//        as local html files will read without any prefix.
	// Note2: The file prefix does not need to be added,
	//        as local html files will read without any prefix.
	if ((urllen <= (_FSmartbase_MAXBUFFERLEN-10)) && (strncmp(urlstr,"www.",4) == 0))
		{
		strcpy(gTP->TempBuffer,"http://");
		strcat(gTP->TempBuffer,urlstr);
		urlstr = gTP->TempBuffer;
		urllen = (NUM)strlen(urlstr);
		}
	
	// Call the SessionManager function to get the html page
	retcode = gpSessionManager->readHtmlPage(gTP->SessionID,urlstr,&pageContent,timeToLive);
	if (retcode < 0) {
		// If we have an explicit error code, then return an error.
		ReadError:
		*aResult = TERROR((LpCHAR)"!readHtmlPage:error!");
	} else 
	if ((urllen >= 5) && (strcmp(&urlstr[urllen-5],".html") == 0)) {
		// If we have a trailing dot html, then return the results whatever they are.
		PageRead:
		*aResult = TSTRING(pageContent);
		gpSessionManager->clearHtmlPage(gTP->SessionID); // release the memory used in session manager to buffer the page
	} else 
	if ((urllen >= 4) && (strcmp(&urlstr[urllen-4],".htm") == 0)) {
		// If we have a trailing dot htm, then return the results whatever they are.
		goto PageRead;
	} else 
	if ((urllen >= 4) && (strcmp(&urlstr[urllen-4],".xml") == 0)) {
		// If we have a trailing dot xml, then return the results whatever they are.
		goto PageRead;
	} else 
	if ((strcmp(pageContent,"Page not found") != 0) && (strcmp(pageContent,"") != 0)) {
		// If we retrieved a page, then return the results whatever they are.
		goto PageRead;
	} else 
	if ((urllen > 1) && (strcmp(&urlstr[urllen-1],"/") == 0)) {
		// If there is an ending slash, then return the results whatever they are.
		// If we did not find the page, and there is an ending slash, then append an ending slash index.html.
		// Note: The ending slash index.html is sometimes needed by some hosts to find the folder index page.
		strcpy(gTP->TempBuffer,urlstr);
		strcat(gTP->TempBuffer,"index.html");
		urlstr = gTP->TempBuffer;
		urllen = (NUM)strlen(urlstr);

		// Call the SessionManager function to get the html page
		retcode = gpSessionManager->readHtmlPage(gTP->SessionID,urlstr,&pageContent,timeToLive);
	    if (retcode < 0) goto ReadError;
		*aResult = TSTRING(pageContent);
		gpSessionManager->clearHtmlPage(gTP->SessionID); // release the memory used in session manager to buffer the page
	} else 
	if ((urllen > 11) && (strcmp(&urlstr[urllen-11],"/index.html") == 0)) {
		// If there is an ending slash index, then return the results whatever they are.
		goto PageRead;
	} else 
	if ((urllen < 11) || (strcmp(&urlstr[urllen-11],"/index.html") != 0)) {
		// If we did not find the page, and there is no ending slash, then append an ending slash index.html.
		// Note: The ending slash index.html is sometimes needed by some hosts to find the folder index page.
		strcpy(gTP->TempBuffer,urlstr);
		strcat(gTP->TempBuffer,"/index.html");
		urlstr = gTP->TempBuffer;
		urllen = (NUM)strlen(urlstr);

		// Call the SessionManager function to get the html page
		retcode = gpSessionManager->readHtmlPage(gTP->SessionID,urlstr,&pageContent,timeToLive);
	    if (retcode < 0) goto ReadError;
		*aResult = TSTRING(pageContent);
		gpSessionManager->clearHtmlPage(gTP->SessionID); // release the memory used in session manager to buffer the page
	}
	// If we are unable to retrieve the page, then return an error.
    FSmartbase_StringPtr(gCP,gTP,aResult,(LpPOINTER)&pageContent,(LpNUM)&pageSize);
	if ((strcmp(pageContent,"Page not found") == 0) || (strcmp(pageContent,"") == 0)) {	
		*aResult = TERROR((LpCHAR)"!readHtmlPage: page not found!");
	}
	FrameExit(*aResult);
}

/*-----------------------------------------------------------------
SBGlue_ReloadLib
Arguments:
	libpath	-- path to library to load. 
Returns:
	aResult
------------------------------------------------------------------*/
TVAL SBGlue_ReloadLib(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	char			*apBuf;
	NUM				aBufLen;

	StartFrame
	DeclareTVAL(aResult);
	EndFrame

	if (argc != 1) 
		FrameExit(TERROR((LpCHAR)"arglist")); // make sure there is a path specification
	if (argv[0].Tag != TYSTRING && argv[0].Tag != TYTEXT) 
		FrameExit(TERROR((LpCHAR)"arglist"));
	FSmartbase_StringPtr(gCP, gTP, &argv[0], &apBuf, &aBufLen);	
	QString aLibpath(apBuf);
	*aResult = TBOOL(DynamicLibs.reloadLib(gCP, gTP, aLibpath));
	FrameExit(*aResult);
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_RingBell (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);
   
	return(TINT(gpSessionManager->cbRingBell(gTP->SessionID, gTP->RequestID)));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_test (LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);

	return(FSmartbase_SetSymbolValue(gCP,gTP,const_cast<LpCHAR>("timsvar"),argv[0]));
}


/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_Select(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	// was Fucntions_LispSelect
	return(TERROR("!select not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_SendToClient(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	NUM			aLen;
	char		*apName = NULL;
	char		*apValue = NULL;
	NUM			aIx;
	NUM			aNameLen = 0;
	NUM			aValueLen = 0;
	NUM			aRet;
	NUM			aSessionID;
	QString		aMsgStr;

	// Note that the StartFrame macro uses gCP and gTP
	StartFrame
	DeclareTVAL(aResult);
	DeclareTVAL(aValueStr);
	DeclareTVAL(aValue);
	DeclareTVAL(aName);
	DeclareTVAL(aNameStr);
	EndFrame

	/*  Make sure we have the proper argument count. */
	if (argc != 2 )
	{	*aResult = TERROR((LpCHAR)"!sendToClient:Incorrect number of arguments!");
		FrameExit(*aResult);
	}	
	if (argv[0].Tag != TYNUM)
	{	*aResult = TERROR((LpCHAR)"!sendToClient:First arg not a number!");
		FrameExit(*aResult);
	}
	if (argv[1].Tag != TYSTRUCTURE)
	{	*aResult = TERROR((LpCHAR)"!sendToClient:Second arg not Structure!");
		FrameExit(*aResult);
	}
	// Get the sessionID passed as the first argument
	aSessionID = argv[0].u.Int;

	//------------ Convert the return TVAL into a results string ----------
	// Iterate through the TVAL converting each Structure element into a
	// del separated string.

	*aResult = argv[1];
	aLen = FSmartbase_VectorLen(gCP,gTP,*aResult); // Works on structures

	for (aIx =0; aIx < aLen;)
	{	// (setq name (ref aResult i 0))
		*aName = FSmartbase_Ref(gCP, gTP, 3, *aResult, TINT(aIx), TINT(0));
		*aNameStr = FSmartbase_Convert(gCP, gTP, TYSTRING, *aName);
		FSmartbase_StringPtr(gCP, gTP , aNameStr,&apName, &aNameLen);

		*aValue = FSmartbase_Ref(gCP, gTP, 3, *aResult, TINT(aIx), TINT(1));
		*aValueStr = FSmartbase_Convert(gCP, gTP, TYSTRING, *aValue);

		if (aValueStr->Tag == TYERROR)
		{	*aResult = TERROR((LpCHAR)"!sendToClient: Structure value cannot be converted to string!");
			FrameExit(*aResult);			
		}
		FSmartbase_StringPtr(gCP, gTP, aValueStr, &apValue, &aValueLen);
		aMsgStr += apName;
		aMsgStr += '\177';
		aMsgStr += apValue;
		aIx++;
		if (aIx < aLen)
			aMsgStr += '\177';
	}
	aRet = gpSessionManager->cbSendToClient(aSessionID, aMsgStr);
	if (aRet != 0) 
		*aResult = TERROR((LpCHAR)"!sendToClient: Bad sessionID, client session probably dead!");
	else 
		*aResult = TINT(0);
	FrameExit(*aResult);
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
TVAL SBGlue_ServerDoomed(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	return(TERROR("!serverDoomed not supported by SBGlue!"));
}

/*-----------------------------------------------------------------
SBGlue_SmOpenContext
Call SessionManager to open a context
------------------------------------------------------------------*/
TVAL SBGlue_SmOpenContext(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	POINTER	apPathstr = NULL;
	NUM		aPathLen = 0;
	long		aRetcode;

	// Protected variables
	StartFrame
	DeclareTVAL(aResult);        
    EndFrame

	//  Expect aArgs: ipPath
	if (argc < 1)
	{	*aResult = TERROR((LpCHAR)"!OpenContext:aArgs!");
		FrameExit(*aResult);
	}
	// Get pointer to path argument
	FSmartbase_StringPtr(gCP, gTP, &argv[0], &apPathstr, &aPathLen);
	QString aAisOut, aContextName, aPath(apPathstr);
	aRetcode = gpSessionManager->openContext(aPath, aContextName, aAisOut);
	if (aRetcode < 0) 
		*aResult = TERROR((LpCHAR)"!openContext:error!");
	else
		*aResult = aContextName.isEmpty() 
			? TSTRING(aAisOut.toLatin1().data())
			: TSTRING(aContextName.append(aAisOut).append('\n').toLatin1().data());
	FrameExit(*aResult);
}

/*-----------------------------------------------------------------
SBGlue_Sleep
The SBGlue_Sleep function calls a sleep on the currently executing 
thread.
------------------------------------------------------------------*/
TVAL SBGlue_Sleep(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{ 
	// was AnsShell_Sleep
	REAL	aTimeInterval;		// Time interval in msec.

	StartFrame
	DeclareTVAL(aInterval);
	DeclareTVAL(aRet);
	EndFrame

	if (argc != 1)
	{	*aRet = TERROR((LpCHAR)"arglist");
		FrameExit(*aRet);
	}
	// Convert interval time argument to real.
	if (asTag(&argv[0]) != TYREAL)
	{	*aInterval = FSmartbase_Convert(gCP, gTP, TYREAL, argv[0]);
		ExitOnError(*aInterval);
		aTimeInterval = aInterval->u.Real;
	}
	else
		aTimeInterval = argv[0].u.Real;

	// Sleep on this context's thread in msec.
	AContextThread* sessionMgrContextThread = (AContextThread*)gCP->SessionMgrContextThread;
	sessionMgrContextThread->localSleep((LONG)(aTimeInterval/*Msec*/));
	FrameExit(gCP->Tval_TRUE);
}

/*-----------------------------------------------------------------
SBGlue_submit
Submit an event to your own request queue.
NOTE: Events do not return results!
------------------------------------------------------------------*/
TVAL SBGlue_Submit(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	QDateTime	aDateTime;
	char*		apBuf;
	NUM			aBufLen;
	long		aRequestID;
	long		aMonth;
	long		aDay;
	long		aYear;
	long		aHour;
	long		aMinute;
	long		aSecond;

	StartFrame
	DeclareTVAL(aResult);
	EndFrame

	if (argc < 1 || argc > 2) 
		FrameExit(TERROR((LpCHAR)"arglist"));
	if (argv[0].Tag != TYSTRING && argv[0].Tag != TYTEXT) 
		FrameExit(TERROR((LpCHAR)"1st arg must be a string"));
	if (argc == 1)
			aDateTime = QDateTime::currentDateTime();
	else
	{	if (argv[1].Tag != TYDATE) 
			FrameExit(TERROR((LpCHAR)"2nd arg must be a date"));

		*aResult = FSmartbase_Eval(gCP, gTP, TGVALUE("year"), 1, argv[1]);
		aYear = asInt(aResult);

		*aResult = FSmartbase_Eval(gCP, gTP, TGVALUE("month"), 1, argv[1]);
		aMonth = asInt(aResult);

		*aResult = FSmartbase_Eval(gCP, gTP, TGVALUE("day"), 1, argv[1]);
		aDay = asInt(aResult);

		*aResult = FSmartbase_Eval(gCP, gTP, TGVALUE("hour"), 1, argv[1]);
		aHour = asInt(aResult);

		*aResult = FSmartbase_Eval(gCP, gTP, TGVALUE("minute"), 1, argv[1]);
		aMinute = asInt(aResult);

		*aResult = FSmartbase_Eval(gCP, gTP, TGVALUE("second"), 1, argv[1]);
		aSecond = asInt(aResult);

		aDateTime = QDateTime(QDate(aYear, aMonth, aDay) ,QTime(aHour, aMinute, aSecond));
	}
	FSmartbase_StringPtr(gCP, gTP, &argv[0], &apBuf, &aBufLen);	
	aRequestID = gpSessionManager->submit(gTP->SessionID, SBGLUE_EVENTCMD, 1 , apBuf, aDateTime, NULL/*Data*/);
	FrameExit(TINT(aRequestID));
}

/*-----------------------------------------------------------------
SBGlue_subscribe
Subscribe to another session.
NOTE: Events do not return results!
------------------------------------------------------------------*/
TVAL SBGlue_Subscribe(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	long	aErrCode;			// Error code returned from the session manager.
	bool	aRemove = false;	// Remove aCurSessionId from subscriber list for aSessionID
	long	aSessionID;			// The session whose output is being captured.

	StartFrame
	EndFrame

	if (argc < 1 || argc > 2) 
		FrameExit(TERROR((LpCHAR)"Subscribe: arglist"));
	if (argv[0].Tag != TYNUM)
		FrameExit(TERROR((LpCHAR)"Subscribe: First arg must be an integer."));
	if (argc == 2)
	{	if (argv[1].Tag != TYBOLE)
			FrameExit(TERROR((LpCHAR)"Subscribe: Second arg must be a boolean."));
		aRemove = argv[1].u.Bool;
	}
	aSessionID = argv[0].u.Long;
	aErrCode = gpSessionManager->subscribe(gTP->SessionID, aSessionID, aRemove);
	FrameExit(TINT(aErrCode));
}

/*-----------------------------------------------------------------
SBGlue_System
Sends the string argument to the system command processor for execution.
Note: was Functions_LispSystem
------------------------------------------------------------------*/
TVAL SBGlue_System(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{ 
	char*       apCptr;
	TVAL        aCode;

	//  Make sure we have a String, Text, or a ByteVector.
	if (argc != 1) goto lBad;

	// Get a pointer to the command String and execute it.
	if (argv[0].Tag == TYTEXT)
		apCptr = argv[0].u.Text;
	else if (argv[0].Tag == TYCHAR)
	{	argv[0].u.Text[1] = 0;
		apCptr = argv[0].u.Text;
	}
	else if (argv[0].Tag == TYSTRING)
		apCptr = FSmartbase_VectorPtr(gCP,gTP,argv[0]);
	else
		goto lBad;
	aCode.Tag = TYNUM;
	aCode.u.Int = system(apCptr);
	return aCode;
lBad:
	return(TERROR( "system: Expecting Text, String, or Byte Vector argument")); 
}

/*-----------------------------------------------------------------
SBGlue_Throw
Arguments -- 
	none
-------------------------------------------------------------------*/
TVAL SBGlue_Throw(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
    // Unused Variables
    Q_UNUSED(argc);
    Q_UNUSED(argv);

	StartFrame
	DeclareTVAL(aResult);
	EndFrame
	*aResult = FSmartbase_Throw(gCP, gTP, FSMARTBASE_ERR_ESCAPE);
	FrameExit(*aResult);
}

/*-----------------------------------------------------------------------------------------------------------------------------
SBGlue_UpdateState is registered in the context funcs structure as the call to _Host_UpdateState. This function is called each
time the engine state is changed by debug_debug.
-----------------------------------------------------------------------------------------------------------------------------*/
NUM SBGlue_UpdateState(LpXCONTEXT gCP, LpTHREAD gTP)
{
	long aCurrentState;
	if (gTP->SessionID == -1) 
		return 0; // Engine not yet fully initialized
	if (gTP->EngineStateChanged)
	{	aCurrentState = FSmartbase_SetEngineFlags(gCP, gTP, -1);
		gpSessionManager->cbSendEngineStateToSessions(gTP->SessionID, aCurrentState);
		gTP->EngineStateChanged = false;
	}
	return 0;
}
//	************************************************* END REGISTERED FUNCTIONS ************************************************

//	***************************************************** PRIVATE FUNCTIONS ***************************************************
/*-----------------------------------------------------------------
Name:			createContext

Description:	Initializes the context memory block so that Lisp code can execute in the context.
				The C-Functions are registered to the Smartbase Engine. The context stck and main memory
				buffers are allocated from the system memory pool, and the context thread structures are
				initialzied so that the context is ready for Lisp execution.

Note:	(MFK)	This function is misnamed. A better name might be "initializeContext"
------------------------------------------------------------------*/
NUM createContext(const char* ipContextName, long iContextIndex, const char* ipScript, LpXCONTEXT* ipCP,long iMaxThreads,
					NUM iMaxMemory, long iObjHdrSize, POINTER ipSessionMgrContextThread)
{
    // Unused Variables
    ipSessionMgrContextThread = ipSessionMgrContextThread;
    iMaxThreads = iMaxThreads;

	LpXCONTEXT		gCP;
	LpTHREAD		gTP;
	NUM 			aCatchCode;
	NUM				aRequiredHostStackSpace;
	NUM				aMaxRecursions;
	NUM				aMinBlockSize;
	POINTER			apAllocBlocks[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
	NUM				aAllocBlockSize[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
	struct			FSmartbase_HostCallBackFunctions    aFuncs;

	if (strlen(ipContextName) > 62)
		return -1;
	aFuncs._Host_Display = SBGlue_Display;
	aFuncs._Host_Escape	= SBGlue_Escape;
	aFuncs._Host_UpdateState = SBGlue_UpdateState;
	aFuncs._Host_OpenF	= SBGlue_Open;
	aFuncs._Host_ReadF	= SysGlue_read;
	aFuncs._Host_WriteF	= SysGlue_write;
	aFuncs._Host_SeekF	= SysGlue_seek;
	aFuncs._Host_ResizeF	= SysGlue_resize;
	aFuncs._Host_CloseF	= SysGlue_close;
	strcpy(aFuncs.ContextName, ipContextName);
	if (iMaxMemory <= 0) 
		return -1;

	// Calculate and assign memory allocation parameters
	aFuncs._Host_memorySize = iMaxMemory;
	aFuncs._Host_memoryObjHdrSize = iObjHdrSize;
	aFuncs._Host_MaxThreadCount = 1;
	aRequiredHostStackSpace = FSmartbase_CalculateStackSpace(aFuncs._Host_memorySize);
	aMaxRecursions = (aRequiredHostStackSpace/_FSmartbase_C_Temporaries);
	aFuncs._Host_GarbageStackWords = (aFuncs._Host_memorySize/2000000)*aMaxRecursions;
	aFuncs._Host_GarbageStackWords = min(20*aMaxRecursions,aFuncs._Host_GarbageStackWords);
	aFuncs._Host_OperationsStackWords = (aFuncs._Host_memorySize/2000000)*aMaxRecursions;
	aFuncs._Host_OperationsStackWords = min(20*aMaxRecursions,aFuncs._Host_OperationsStackWords);
	for(long i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; aFuncs._Host_memBlockPtr[i++] = NULL) ;
	for(long i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; apAllocBlocks[i++] = NULL) ;
	for(long i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; aAllocBlockSize[i++] = 0) ;

	// Calculate the minimum size of the first block
	aMinBlockSize = (XCONTEXTBLOCKLENGTH
				     + (aFuncs._Host_MaxThreadCount * THREADBLOCKLENGTH) 
				     + ((aFuncs._Host_GarbageStackWords + 100) * sizeof(OBJ**))
				     + ((aFuncs._Host_OperationsStackWords  + 100) * sizeof(TVAL)));

	// Check to make sure the maxMemory requested is big enough
	if (aMinBlockSize >= iMaxMemory)
		return -1; 

	// Make sure the minimum block size is not too small
	// Too small is an empirical measure.
	if (aMinBlockSize < FSMARTBASE_MINBLOCKSIZE) 
		aMinBlockSize = FSMARTBASE_MINBLOCKSIZE;

	// Allocate the Heap for the context if it has not already been allocated
	if (*ipCP == NULL) 
	{	NUM aTotalAcquired = 0;
		NUM aRequest = aFuncs._Host_memorySize;			// Amount of memory requested
		NUM aBlockSize;								// Usable amount of allocated block (<= aRequest)
		POINTER apBlock;
		NUM i = 0, j, lowSlot;
#if defined(_LINUX)
        long aPageSize = sysconf(_SC_PAGESIZE);		// Virtual memory page size (must be a power of 2)
#else
		long aPageSize = 0;
#endif
		while ((aRequest >= aMinBlockSize) && (aTotalAcquired < aFuncs._Host_memorySize) && (i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS)) 
		{	// aBlockSize = (aRequest > MAXINT) ? MAXINT : aRequest;	// Limit each block to 2GB or less
			aBlockSize = aRequest;
#if defined(_MSVC) && defined(_M64)
			if  (gAis.mpFirstBlock != NIL)
				{
				if (aBlockSize < AIS_FIRSTCONTEXTPOOL)
					{
					apBlock = (POINTER)realloc(gAis.mpFirstBlock, aBlockSize);
					gAis.mpFirstBlock = NIL;
					}
				else
					{
					apBlock = gAis.mpFirstBlock;
					aBlockSize = AIS_FIRSTCONTEXTPOOL;
					gAis.mpFirstBlock = NIL;
					}
				}
			else
				{
				apBlock = (POINTER)VirtualAlloc(NULL, aBlockSize, MEM_RESERVE, PAGE_EXECUTE_READWRITE);
				VirtualAlloc(apBlock, aBlockSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
				}
#else
			if  (gAis.mpFirstBlock != NIL)
				{
				if (aBlockSize < AIS_FIRSTCONTEXTPOOL)
					{
					apBlock = (POINTER)realloc(gAis.mpFirstBlock,aBlockSize);
					gAis.mpFirstBlock = NIL;
					}
				else
					{
					apBlock = gAis.mpFirstBlock;
					aBlockSize = AIS_FIRSTCONTEXTPOOL;
					gAis.mpFirstBlock = NIL;
					}
				}
			else
				{
#if defined(_GCC) && defined(_LINUX)
					posix_memalign((void**)&apBlock, aPageSize, aBlockSize); // Allocates memory on page boundary
#else
					apBlock = (POINTER)malloc(aBlockSize);
#endif
				}
#endif



#ifdef _LINUX
			if (apBlock != NULL)
			{
				if (mprotect(apBlock, aRequest, PROT_EXEC|PROT_WRITE|PROT_READ) < 0)
					apBlock = NULL;
			}
#endif
			if (apBlock != NULL)
			{	
#ifdef _MSVC
#ifdef _M64
				// Call VirtualLock, return -1 if there is an error during the call.
				if( VirtualLock(apBlock, aBlockSize) )
				{
					return -1;
				}
#endif
#endif
				aTotalAcquired += aBlockSize;
				apAllocBlocks[i] = apBlock;
				aAllocBlockSize[i++] = aBlockSize;
				if (aFuncs._Host_memorySize > aTotalAcquired)
					aRequest = aFuncs._Host_memorySize - aTotalAcquired; // Try to get remainder as another single block
			}
			else
				aRequest = aBlockSize - FSMARTBASE_BLOCKDECREMENT; // Lets try getting a smaller block
		}
		// If we did not get enough memory then we return a failure state
		if (aTotalAcquired < aFuncs._Host_memorySize) 
		{
			//CleanUpUnwantedMallocs:
			*ipCP = NULL;
			// Clean up all unwanted memory allocations if we failed to open the context properly
			for (i = 0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i) 
			{	
				if (aFuncs._Host_memBlockPtr[i] != NULL)
				{	
#if defined(_MSVC) && defined(_M64)
					VirtualFree(aFuncs._Host_memBlockPtr[i], aFuncs._Host_memBlockSize[i], MEM_DECOMMIT);
					VirtualFree(aFuncs._Host_memBlockPtr[i], aFuncs._Host_memBlockSize[i], MEM_RELEASE);
#else
					free(aFuncs._Host_memBlockPtr[i]);
#endif
					aFuncs._Host_memBlockPtr[i] = NULL;
					aFuncs._Host_memBlockSize[i] = 0;
				}
				else
				{	aFuncs._Host_memBlockPtr[i] = NULL;
					aFuncs._Host_memBlockSize[i] = 0;
				}

				if (apAllocBlocks[i] != NULL)
				{	
#if defined(_MSVC) && defined(_M64)
					VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_DECOMMIT);
					VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_RELEASE);
#else
					free(apAllocBlocks[i]);
#endif
					apAllocBlocks[i] = NULL;
					aAllocBlockSize[i] = 0;
				}
				else
				{	
					apAllocBlocks[i] = NULL;
					aAllocBlockSize[i] = 0;
				}
			}
			return -1;
		}

		// Copy allocated blocks in ascending order into _Host_memBlockPtr array
		for (i = 0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i) // For each slot in _Host_memBlockPtr
		{	// Find lowest pointer in apAllocBlocks for assignment into _Host_memBlockPtr
			lowSlot = 0;
			for (j = 1; j < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++j)
				if ((apAllocBlocks[lowSlot] == NULL) || (apAllocBlocks[j] != NULL && (apAllocBlocks[j] < apAllocBlocks[lowSlot])))
					 lowSlot = j;
			if (apAllocBlocks[lowSlot] != NULL)
			{	aFuncs._Host_memBlockPtr[i] = apAllocBlocks[lowSlot];
				aFuncs._Host_memBlockSize[i] = aAllocBlockSize[lowSlot];
				memset(aFuncs._Host_memBlockPtr[i], 0, aFuncs._Host_memBlockSize[i]);
				apAllocBlocks[lowSlot] = NULL;
				aAllocBlockSize[lowSlot] = 0;
			}
			else
			{	aFuncs._Host_memBlockPtr[i] = NULL;
				aFuncs._Host_memBlockSize[i] = 0;
			}
		}

		*ipCP = (LpXCONTEXT)aFuncs._Host_memBlockPtr[0];
		aFuncs._Host_memorySize = aFuncs._Host_memBlockSize[0];
		memset(aFuncs._Host_memBlockPtr[0], 0, aFuncs._Host_memBlockSize[0]);
} else
	{	
		aFuncs._Host_memBlockPtr[0] = (POINTER)*ipCP;
		aFuncs._Host_memBlockSize[0] = aFuncs._Host_memorySize;
		memset(aFuncs._Host_memBlockPtr[0], 0, aFuncs._Host_memorySize);
	}

	// This will make sure that the Context knows if the Embedded MySQL
	// has failed to initialize.
	aFuncs._EmbeddedMySQLEnabled = (BOLE)gpSessionManager->isEmbeddedMySQLEnabled();

	// Initialize the SmartBase engine.
	gCP = FSmartbase_Init(aFuncs); // Note that gCP == aFuncs._Host_memBlockPtr[0]!!!
	gTP = (LpTHREAD)&gCP->ThreadBlocks[0];
	gTP->SessionID = -1; // The init is never associated with a session
	gCP->ContextIndex = iContextIndex; // See sbglue.cpp gContextMutexs for usage
	gCP->SessionMgrContextThread = NULL;

	// Register utility procedures.
	StartFrame
	TVAL		aEc;
	EndFrame
	// TRY block in case there is a C++ (or memory or ...) exception
	// But we also need the setjmp setup since SmartBase uses Throw (longjmp)
	// (this uses the one and only catch environment)
	_FSmartbase_ECatch(gCP,/*out*/aCatchCode, LBLSystemThrowError);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"contextBusy",(LpFUNC)&SBGlue_ContextBusy);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"contextClose",(LpFUNC)&SBGlue_ContextClose);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"contextExists",(LpFUNC)&SBGlue_ContextExists);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"contextOpen",(LpFUNC)&SBGlue_ContextOpen);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"contextSubmit",(LpFUNC)&SBGlue_ContextSubmit);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"contextStop",(LpFUNC)&SBGlue_ContextStop);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"contextGetResult",(LpFUNC)&SBGlue_ContextGetResult);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"contextGetDisplay",(LpFUNC)&SBGlue_ContextGetDisplay);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"contextMyself",(LpFUNC)&SBGlue_ContextMyself);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"contextSubscribe",(LpFUNC)&SBGlue_ContextSubscribe);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"contextList",(LpFUNC)&SBGlue_ContextList);

	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"ringBell",(LpFUNC)&SBGlue_RingBell);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"debugDialog",(LpFUNC)&SBGlue_DebugDialog);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"loadBrowseLambda",(LpFUNC)&SBGlue_LoadBrowseLambda);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"loadUtilityLambdas",(LpFUNC)&SBGlue_LoadUtilityLambdas);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"getTickCount",(LpFUNC)&SBGlue_GetTickCount);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"sleep",(LpFUNC)&SBGlue_Sleep); 
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"system",(LpFUNC)&SBGlue_System);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"fileGetStatus",(LpFUNC)&SBGlue_GetFileStatus);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"buildLambdaMessage",(LpFUNC)&SBGlue_BuildLambdaMessage);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"decodeURL",(LpFUNC)&SBGlue_DecodeURL);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"encodeURL",(LpFUNC)&SBGlue_EncodeURL);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"evalsInSyncLocalContext",(LpFUNC)&SBGlue_EvalsInSyncLocalContext);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"browse",(LpFUNC)&SBGlue_Browse);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"input",(LpFUNC)&SBGlue_Input);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"select",(LpFUNC)&SBGlue_Select);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"hostCreate",(LpFUNC)&SBGlue_HostCreate);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"hostSend",(LpFUNC)&SBGlue_HostSend);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"hostInvoke",(LpFUNC)&SBGlue_HostInvoke);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"hostDestroy",(LpFUNC)&SBGlue_HostDestroy);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"hostGetProperty",(LpFUNC)&SBGlue_HostGetProperty);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"hostPutProperty",(LpFUNC)&SBGlue_HostPutProperty);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"notify",(LpFUNC)&SBGlue_Notify);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"msgbox",(LpFUNC)&SBGlue_MsgBox);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"nextUserId",(LpFUNC)&SBGlue_NextUserID);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"evalAMP",(LpFUNC)&SBGlue_EvalAMP);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"serverDoomed",(LpFUNC)&SBGlue_ServerDoomed);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"displayWorkbenchWindow",(LpFUNC)&SBGlue_DisplayWorkbenchWindow);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"openContext",(LpFUNC)&SBGlue_SmOpenContext);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"readHtmlPage",(LpFUNC)&SBGlue_ReadHtmlPage);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"getHttp",(LpFUNC)&SBGlue_GetHttp);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"postHttp",(LpFUNC)&SBGlue_PostHttp);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"sendToClient",(LpFUNC)&SBGlue_SendToClient);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"enableConsoleLog",(LpFUNC)&SBGlue_EnableConsoleLog);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"getConsoleLog",(LpFUNC)&SBGlue_GetConsoleLog);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"openConsoleLog",(LpFUNC)&SBGlue_OpenConsoleLog);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"throw",(LpFUNC)&SBGlue_Throw);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"loadLib",(LpFUNC)&SBGlue_LoadLib);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"reloadLib",(LpFUNC)&SBGlue_ReloadLib);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"getOSDir",(LpFUNC)&SBGlue_GetOSDir);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"mkOSDir",(LpFUNC)&SBGlue_MkOSDir);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"cpOSFile",(LpFUNC)&SBGlue_CpOSFile);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"existsOSFile",(LpFUNC)&SBGlue_ExistsOSFile);
//	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"getMachineName",(LpFUNC)&SBGlue_GetMachineName);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"_AISQDir",(LpFUNC)&SBGlue_Dir);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"_AISQFileInfo",(LpFUNC)&SBGlue_FileInfo);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"_AISContextClient",(LpFUNC)&SBGlue_ContextClient);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"_AISTcpClient",(LpFUNC)&SBGlue_TcpClient);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"submit",(LpFUNC)&SBGlue_Submit);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"subscribe",(LpFUNC)&SBGlue_Subscribe);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"getContextPtr",(LpFUNC)&SBGlue_GetContextPtr);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"_aXml",(LpFUNC)&SBGlue_AXml);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"createBuffer",(LpFUNC)&SBGlue_CreateBuffer);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"deleteBuffer",(LpFUNC)&SBGlue_DeleteBuffer);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"_test",(LpFUNC)&SBGlue_test); // temporary testing stub 
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"openLog",(LpFUNC)&SBGlue_OpenLog);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"startLog",(LpFUNC)&SBGlue_StartLogging);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"stopLog",(LpFUNC)&SBGlue_StopLogging);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"closeLog",(LpFUNC)&SBGlue_CloseLog);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"md5hash",(LpFUNC)&SBGlue_Md5Hash);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"addFileWatcher",(LpFUNC)&SBGlue_AddToFileWatcher);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"removeFileWatcher",(LpFUNC)&SBGlue_RemoveFromFileWatcher);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"sendFileChangedEvent",(LpFUNC)&SBGlue_SendFileChangedEvent);
	aEc = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"sendStatusUpdateEvent",(LpFUNC)&SBGlue_SendStatusUpdateEvent);
	
	//  Lock all globals defined herein.
	FSmartbase_Evals(gCP, gTP, (LpCHAR)"(lock _globals)", false);

	//	Set the globals _path and _ais
	if (ipScript != NULL && *ipScript != '\0')
		FSmartbase_Evals(gCP, gTP, (char*)ipScript, false);
	FrameExit(0);
	
LBLSystemThrowError:
	// Give back any memory we allocated as context is assumed to be toast on an error during creation
	for(long i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i)
		if (apAllocBlocks[i] != NULL) 
				free(apAllocBlocks[i]);

	FrameExit(errorNum(aCatchCode));
}

/*	-----------------------------------------------------------------------------------
	decodeUrl - Decode a url string
	Warning: String in ipBfr is modified, but it does not get longer.

	pBfr		-> to string to be decoded
	RETURNS:	ipBfr 
	-------------------------------------------------------------------------------- */
char* decodeUrl(char* ipBfr)
{
	char	aCh;				// Current char
	char	*apBfr, *apT = ipBfr;
	long	aValue;

	if(*ipBfr != '\0')
	{
		for (apBfr = ipBfr; *apBfr != '\0'; ++apBfr)
		{	// Replace + with a space
			if ((aCh = *apBfr) == '+')
				aCh = ' ';
			else if (aCh == '%')
			{	// Convert next 2 Hex chars to equivalent ASCII code
				if ((aValue = hexToInt(*++apBfr)) == -1)
				{	ipBfr[0] = '\0';
					return ipBfr;				// Invalid url string or a null ipBfr
				}
				aCh = (char)(aValue << 4);
				if ((aValue = hexToInt(*++apBfr)) == -1)
				{	ipBfr[0] = '\0';
					return ipBfr;				// Invalid (non-hex) char in string
				}
				aCh |= (char)aValue;
			}
			*apT++ = aCh;
		}
		*apT = '\0';
	}
	return ipBfr;
}

/*	-----------------------------------------------------------------------------------
	encodeUrl - Encode a url string
	Warning: input string is modified and must be long enough to allow extra chars to
	be added.

	ipBfr		-> to string to be encoded
	RETURNS:	ipBfr
	-------------------------------------------------------------------------------- */
char* encodeUrl(char* ipBfr, long iBfrSz)
{
	char	aCh, aHex;	// Next input char from pBfr, hex value of Ch
	char*	apBfr;		// -> input string
	long	aLgth;		// Length of the rest of string in the buffer
	long	aBfrLen;	// Length of the string in the buffer

	if (ipBfr == NULL || --iBfrSz <= 0)
		return ipBfr;
	aBfrLen = (long)strlen(ipBfr);

	for (apBfr = ipBfr, aLgth = aBfrLen+1; *apBfr != '\0' && aBfrLen < iBfrSz; ++apBfr, --aLgth)
	{	aCh = *apBfr;
		switch (aCh)
		{
		case ' ':
		// Reserved characters (vf. HTML Def. Guide, p196)
		case ';':
		case '/':
		case '?':
		case ':':
		case '@':
		case '=':
		case '&':
		// Unsafe characters
		case '<':
		case '>':
		case '"':
		case '#':
		case '%':
		case '{':
		case '}':
		case '|':
		case '\\':
		case '^':
		case '~':
		case '[':
		case ']':
		case '`':	// backquote
			if ((aBfrLen += 2) >= iBfrSz)
				goto lExit;
			memmove(apBfr + 2, apBfr, aLgth);	// Make room for 2 more chars in stg
			*apBfr++ = '%';					// One could also use sprintf but it is slower
			aHex = aCh >> 4;
			aHex += (aHex <= 9) ? '0' : 'A' - 10;
			*apBfr++ = aHex;
			aHex = aCh & 0xF;
			aHex += (aHex <= 9) ? '0' : 'A' - 10;
			*apBfr = aHex;
			break;
		default:
			break;
		}
	}
lExit:
	return ipBfr;
}

bool getEvalReturnType(long returnType, AEvalReturnType *type)
{
	bool aOk = true;
	switch(returnType)
	{
	case 0:
		*type = geWait;	
		break;
	case 1:
		*type = geNoReturn;	
		break;
	case 2:
		*type = geReturnRemote;
		break;				
	default:
		*type = geWait;	
		aOk = false;
		break;
	}	
	return(aOk);
}
/*-----------------------------------------------------------------
------------------------------------------------------------------*/
long errorNum(NUM iCode)
{
	switch (iCode)
    {
// case errFileRead:						return(SBGLUE_ERR_FILE_READ);
// case errFileWrite:						return(SBGLUE_ERR_FILE_WRITE);
    case FSMARTBASE_ERR_OUT_OF_MEMORY:		return(SBGLUE_ERR_OUT_OF_MEMORY);
    case FSMARTBASE_ERR_FRAME_ERROR:		return(SBGLUE_ERR_FRAME_ERROR);
    case FSMARTBASE_ERR_INVALID:			return(SBGLUE_ERR_INVALID);
    case FSMARTBASE_ERR_STACK:				return(SBGLUE_ERR_STACK);
    case FSMARTBASE_ERR_ESCAPE:				return(SBGLUE_ERR_ESCAPE);
    case FSMARTBASE_ERR_PCODE:				return(SBGLUE_ERR_PCODE);
	case FSMARTBASE_ERR_BAD_DATATYPE:		return(SBGLUE_ERR_BAD_DATATYPE);
    case FSMARTBASE_ERR_RECURSION:			return(SBGLUE_ERR_RECURSION);
    case FSMARTBASE_ERR_FRAME_RELEASE:		return(SBGLUE_ERR_FRAME_RELEASE);
	case FSMARTBASE_ERR_STACK_RELEASE:		return(SBGLUE_ERR_STACK_RELEASE);
    case FSMARTBASE_ERR_RECURSION_RELEASE:	return(SBGLUE_ERR_RECURSION_RELEASE);
	case FSMARTBASE_ERR_QUIT:				return(SBGLUE_ERR_QUIT);
    case FSMARTBASE_ERR_WRONG_VERSION:		return(SBGLUE_ERR_WRONG_VERSION);
    case FSMARTBASE_ERR_ENGINE_BUSY:		return(SBGLUE_ERR_ENGINE_BUSY);
    case FSMARTBASE_ERR_REPOSITORY_GC:		return(SBGLUE_ERR_REPOSITORY_GC);
	case FSMARTBASE_ERR_BADBUFFERSIZE:		return(SBGLUE_ERR_BADBUFFERSIZE);
    default:
		return iCode;
    }
}

/*-----------------------------------------------------------------
------------------------------------------------------------------*/
const char* errorMsg(NUM iCode)
{
	const char *apMsg = NULL;
	switch (errorNum(iCode))
    {
//    case errFileRead:					apMsg = "!errReadIO!"; break;
//    case errFileWrite:				apMsg = "!errWriteIO!"; break;
	case SBGLUE_EVAL_FAILURE:			apMsg = "!evalFailure!";break;
	case SBGLUE_ERR_FILE_READ:			apMsg = "!errReadIO!";break;
	case SBGLUE_ERR_FILE_WRITE:			apMsg = "!errWriteIO!";break;
    case SBGLUE_ERR_OUT_OF_MEMORY:		apMsg = "!outOfMemory!"; break;
    case SBGLUE_ERR_FRAME_ERROR:		apMsg = "!gcFrameOverflw!"; break;
    case SBGLUE_ERR_INVALID:			apMsg = "!damagedMemoryHD!"; break;
    case SBGLUE_ERR_STACK:				apMsg = "!stackOverflw!"; break;
    case SBGLUE_ERR_ESCAPE:				apMsg = "!userEscape!"; break;
    case SBGLUE_ERR_PCODE:				apMsg = "!invalidPcode!"; break;
	case SBGLUE_ERR_BAD_DATATYPE:		apMsg = "!damagedObject!"; break;
    case SBGLUE_ERR_RECURSION:			apMsg = "!recursnLimit!"; break;
    case SBGLUE_ERR_FRAME_RELEASE:		apMsg = "!gcFrameRelease!"; break;
    case SBGLUE_ERR_STACK_RELEASE:		apMsg = "!stackRelease!"; break;
    case SBGLUE_ERR_RECURSION_RELEASE:	apMsg = "!recursnRelease!"; break;
	case SBGLUE_ERR_QUIT:				apMsg = "!userQuit!"; break;
    case SBGLUE_ERR_WRONG_VERSION:		apMsg = "!diskFileVersion!"; break;
    case SBGLUE_ERR_ENGINE_BUSY:		apMsg = "!engineBusy!"; break;
    case SBGLUE_ERR_REPOSITORY_GC:		apMsg = "!repoGC!"; break;
	case SGBLUE_UNEXPECTED_ERROR:		apMsg = "!unexpectedError!";break;
	case SBGLUE_ERR_NOT_IMPLEMENTED:	apMsg = "!notImplemented!"; break;
	case SBGLUE_ERR_SYSTEM_ERROR:		apMsg = "!systemError!"; break;
	case SBGLUE_ERR_EVAL_SECURITY:		apMsg = "!aSecurity!"; break;
	case SBGLUE_ERR_EVAL_BAD_CHECKIN:	apMsg = "!bad checkin argument!"; break;
	case SBGLUE_ERR_EVAL_BAD_FILTER:	apMsg = "!bad filter argument!"; break;
	case SBGLUE_ERR_EVAL_BAD_SCORE:		apMsg = "!bad score argument!"; break;
	case SBGLUE_SENDTOCLIENT_FAILURE:	apMsg = "!sendToClientFailure!";break;
	case SBGLUE_ERR_BADBUFFERSIZE:		apMsg = "!bad buffer size!";break;
    default:
		apMsg = "!Unknown Engine Error!";
		break;
    }
	return apMsg;
}

// The following functions are Helper functions that are not registered by the engine and not exposed in SBGlue.h. These
// functions are called only by other functions in this file.
/*	---------------------------------------------------------------------------------------------------------------------------
getDirInfo() returns a string with \rubout separated records. Each record has the following tab separated fields:
type		entry types: R=drive or root folder, C=current directory D=directory, F=file	
size		empty for types other than F
datetime	empty for types other than F
	------------------------------------------------------------------------------------------------------------------------ */
QByteArray getDirInfo(LpXCONTEXT gCP,LpTHREAD gTP, char* ipCmdString)
{
	char*			apBuf;
	NUM				aBufLen;
	QDateTime		aDateTime;
	QDir			aDir;
	QByteArray		aDirList;
	long				aIx;
	long				aSize;
	StartFrame
	DeclareTVAL(aResult);
	EndFrame

	// Collect Drives if Windows OS or root dir if not Windows
	QFileInfoList aDrives = QDir::drives(); // Returns "/" if not Windows OS
    QFileInfo aFi;
	aSize = aDrives.size();
    for (aIx = 0; aIx < aSize; ++aIx)
	{	aFi = aDrives[aIx];
		if (aIx > 0)
			aDirList += '\177';
		aDirList += "R\t";
		aDirList += aFi.absolutePath().toLatin1();
		aDirList += "\t\t";
	}
	// Look for &root or &path argument
	if (strcmp(ipCmdString, "&root") == 0)
		aDir = QDir::root();
	else if (strcmp(ipCmdString, "&path") == 0)
	{	*aResult = FSmartbase_Evals(gCP, gTP, const_cast<LpCHAR>("_path"), false);
		if (aResult->Tag != TYERROR)
		{	FSmartbase_StringPtr(gCP, gTP, aResult, &apBuf, &aBufLen);
			aDir.setPath((aBufLen == 0) ? "." : apBuf);
		}
	}
	else 
		aDir.setPath(ipCmdString);
	aDirList += "\177C\t";
	aDirList += aDir.canonicalPath().toLatin1();
	aDirList += "\t\t";
	if (aDir.isReadable())
	{	aDir.setSorting(QDir::DirsFirst | QDir::Name);
		QFileInfoList aDirItems = aDir.entryInfoList();
		aSize = aDirItems.count();
		for (aIx = 0; aIx < aSize; ++aIx)
		{	aFi = aDirItems[aIx];
			if (aFi.isDir())
			{	aDirList += "\177D\t";
				aDirList += aFi.fileName().toLatin1();
				aDirList += "\t\t";
			}
			else if (aFi.isFile())
			{	aDirList += "\177F\t";
				aDirList += aFi.fileName().toLatin1();
				aDirList += '\t';
				aDirList += QByteArray::number(aFi.size());
				aDirList += '\t';
				aDirList += aFi.lastModified().toString("ddd MMMM d yy h:m:s ap").toLatin1();
			}
		}
	}
	FrameExit(aDirList);
}

bool getEvalNotifyType(long notifyType, AEvalNotifyType *type)
{
	bool aOk = true;
	switch(notifyType)
	{
	case 0:
		*type = geNoNotify;		// Perform no notification
	break;
	case 1:
		*type = geFullNotify;	// Create an Amp Message to inform calling context of sucessful completion of request and full return aResult
	break;
	case 2:
		*type = geNotify;		// Create an Amp Message to inform calling context of sucessful completion of request
	break;					// Return aResult will not be included.
	default:
		*type = geNoNotify;	// this should be ignored if caller checks the aOk flag
	aOk = false;
	break;
}	
	return(aOk);
}


/* -----------------------------------------------------------------------------------
 hexToInt - Convert hex char to its binary value
 
 iCh   ASCII char between 0-9, A-F
 RETURNS: binary value or -1 if not a hex char
 -------------------------------------------------------------------------------- */
long hexToInt(char iCh)
{
	long aValue = -1;

	if (islower(iCh)) iCh = _toupper(iCh);
	if (iCh >= '0' && iCh <= '9')
		aValue = iCh - '0';
	else if (iCh >= 'A' && iCh <= 'Z')
		aValue = iCh + 10 - 'A';
	return aValue;
}

/*-----------------------------------------------------------------
Dead Code
qvarToTval creates a TVAL containing the closest matching type
found in the QVariant argument.

TM: For the time being I am only converting the simplest types. I
simply throw away the content of a complex type. Later we may want
to get more aggressive with this as we find we need it when using
the Qt apLib from the engine.
------------------------------------------------------------------*/
// QVariant qtvalToQvar(LpXCONTEXT gCP,LpTHREAD gTP,TVAL iTvInput)
// {
// 	QVariant aResult; // Create empty variant
// 
// 	//	Use the SmartBase TVAL type tag to guide the conversion.
// 	switch (iTvInput.Tag)
// 	{
// 	case TYVOID:
// 		break;
// 		
// 	case TYBOLE:
// 		aResult = (iTvInput.u.Bool == TRUE) ? TRUE : FALSE;
// 		break;
// 		
// 	case TYSHORT:
// 		aResult = iTvInput.u.Short;
// 		break;
// 		
// 	case TYNUM:
// 		aResult = iTvInput.u.Int;
// 		break;
// 		
// 	case TYMONEY:
// 	case TYDATE:
// 	case TYREAL:
// 		aResult = iTvInput.u.Real;
// 		break;
// 		
// 	case TYTEXT:
// 		aResult = iTvInput.u.Text;
// 		break;
// 		READ gTP,TVAL iTvInput)
// {
// 	QVariant aResult; // Create empty variant
// 
// 	//	Use the SmartBase TVAL type tag to guide the conversion.
// 	switch (iTvInput.Tag)
// 	{
// 	case TYVOID:
// 		break;
// 		
// 	case TYBOLE:
// 		aResult = (iTvInput.u.Bool == TRUE) ? TRUE : FALSE;
// 		break;
// 		
// 	case TYSHORT:
// 		aResult = iTvInput.u.Short;
// 		break;
// 		
// 	case TYNUM:
// 		aResult = iTvInput.u.Int;
// 		break;
// 		
// 	case TYMONEY:
// 	case TYDATE:
// 	case TYREAL:
// 		aResult = iTvInput.u.Real;
// 		break;
// 		
// 	case TYTEXT:
// 		aResult = iTvInput.u.Text;
// 		break;
// 		
// 	case TYSTRING:
// 	case TYSYMBOL:
// 	case TYERROR:
// 		//	Convert the input into an OLE Variant string.
// 		aResult = (char *)FSmartbase_ObjectPtr(gCP,gTP,&iTvInput);
// 		break;
// 		
// 	case TYVECTOR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Vector %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYPAIR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Pair %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYDICTIONARY:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Dictionary %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYDIRECTORY:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Directory %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 	{
// 	case TYVOID:
// 		break;
// 		
// 	case TYBOLE:
// 		aResult = (iTvInput.u.Bool == TRUE) ? TRUE : FALSE;
// 		break;
// 		
// 	case TYSHORT:
// 		aResult = iTvInput.u.Short;
// 		break;
// 		
// 	case TYNUM:
// 		aResult = iTvInput.u.Int;
// 		break;
// 		
// 	case TYMONEY:
// 	case TYDATE:
// 	case TYREAL:
// 		aResult = iTvInput.u.Real;
// 		break;
// 		
// 	case TYTEXT:
// 		break;
// 		
// 	case TYTEXT:
// 		aResult = iTvInput.u.Text;
// 		break;
// 		
// 	case TYSTRING:
// 	case TYSYMBOL:
// 	case TYERROR:
// 		//	Convert the input into an OLE Variant string.
// 		aResult = (char *)FSmartbase_ObjectPtr(gCP,gTP,&iTvInput);
// 		break;
// 		
// 	case TYVECTOR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Vector %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYPAIR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Pair %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYDICTIONARY:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Dictionary %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYDIRECTORY:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Directory %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 	{
// 	case TYVOID:
// 		break;
// 		
// 	case TYBOLE:
// 		aResult = (iTvInput.u.Bool == TRUE) ? TRUE : FALSE;
// 		break;
// 		
// 	case TYSHORT:
// 		aResult = iTvInput.u.Short;
// 		break;
// 		
// 	case TYNUM:
// 		aResult = iTvInput.u.Int;
// 		break;
// 		
// 	case TYMONEY:
// 	case TYDATE:
// 	case TYREAL:
// 		aResult = iTvInput.u.Real;
// 		break;
// 		
// 	case TYTEXT:
// 		aResult = iTvInput.u.Text;
// 		break;
// 		
// 	case TYSTRING:
// 	case TYSYMBOL:
// 	case TYERROR:
// 		//	Convert the input into an OLE Variant string.
// 		aResult = (char *)FSmartbase_ObjectPtr(gCP,gTP,&iTvInput);
// 		break;
// 		
// 	case TYVECTOR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Vector %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYPAIR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Pair %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYDICTIONARY:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Dictionary %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		aResult = iTvInput.u.Text;
// 		break;
// 		
// 	case TYSTRING:
// 	case TYSYMBOL:
// 	case TYERROR:
// 		//	Convert the input into an OLE Variant string.
// 		aResult = (char *)FSmartbase_ObjectPtr(gCP,gTP,&iTvInput);
// 		break;
// 		
// 	case TYVECTOR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Vector %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYPAIR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Pair %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYDICTIONARY:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Dictionary %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		break;
// 		
// 	case TYTEXT:
// 		aResult = iTvInput.u.Text;
// 		break;
// 		
// 	case TYSTRING:
// 	case TYSYMBOL:
// 	case TYERROR:
// 		//	Convert the input into an OLE Variant string.
// 		aResult = (char *)FSmartbase_ObjectPtr(gCP,gTP,&iTvInput);
// 		break;
// 		
// 	case TYVECTOR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Vector %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYPAIR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Pair %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYDICTIONARY:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Dictionary %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYDIRECTORY:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Directory %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 	{
// 	case TYVOID:
// 		break;
// 		
// 	case TYBOLE:
// 		aResult = (iTvInput.u.Bool == TRUE) ? TRUE : FALSE;
// 		break;
// 		
// 	case TYSHORT:
// 		aResult = iTvInput.u.Short;
// 		break;
// 		
// 	case TYNUM:
// 		aResult = iTvInput.u.Int;
// 		break;
// 		
// 	case TYMONEY:
// 	case TYDATE:
// 	case TYREAL:
// 		aResult = iTvInput.u.Real;
// 		break;
// 		
// 	case TYTEXT:
// 		aResult = iTvInput.u.Text;
// 		break;
// 		
// 	case TYSTRING:
// 	case TYSYMBOL:
// 	case TYERROR:
// 		//	Convert the input into an OLE Variant string.
// 		aResult = (char *)FSmartbase_ObjectPtr(gCP,gTP,&iTvInput);
// 		break;
// 		
// 	case TYVECTOR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Vector %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYPAIR:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Pair %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYDICTIONARY:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Dictionary %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYDIRECTORY:
// 		//ObjID = FSmartbase_GetObjectID(gCP,gTP,iTvInput);
// 		//sprintf(cnvBuf,"#<Directory %ld>",ObjID);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	case TYSTRUCTURE:
// 		//FSmartbase_CnvToText(gCP,gTP,cnvBuf,sizeof(cnvBuf),iTvInput);
// 		//strTmp = cnvBuf;
// 		//theResult = strTmp.c_str();
// 		break;
// 
// 	}
// 		
// 	return(aResult);
// }
// endstrTmp = cnvBuf;

/*--------------------------------------------------------------------------------------- */
#if 0

SBGlue_Md5Hash

The  md5hash  Procedure returns the a cryptographic hash from the data using md5 algorithm.

#endif

TVAL SBGlue_Md5Hash(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
QByteArray aHashResult;
StartFrame
DeclareTVAL(aRet);
EndFrame

/*  Make sure we have the proper number of arguments. */

if (argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

if((asTag(&argv[0]) != TYSTRING) && (asTag(&argv[0]) != TYTEXT))
{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

if(asTag(&argv[0]) == TYSTRING)
{
    aHashResult = QCryptographicHash::hash( (QByteArray)CharArray(argv[0]), QCryptographicHash::Md5 );
}

if(asTag(&argv[0]) == TYTEXT)
{
    aHashResult = QCryptographicHash::hash( (QByteArray)(argv[0].u.Text), QCryptographicHash::Md5 );
}

*aRet = FSmartbase_CnvFromText(gCP,gTP, aHashResult.data());
//aRet->Tag = TYSTRING;

FrameExit(*aRet);
}

/*--------------------------------------------------------------------------------------- */
#if 0

SBGlue_AddToFileWatcher

The  AddToFileWatcher  Procedure adds a file to the AFileWatcher class

#endif

TVAL SBGlue_AddToFileWatcher(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
QString aFileName;
long aSessionID;
StartFrame
EndFrame

/*  Make sure we have the proper number of arguments. */

if (argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

if((asTag(&argv[0]) != TYSTRING) && (asTag(&argv[0]) != TYTEXT))
{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

if(asTag(&argv[0]) == TYSTRING)
{
	aSessionID = gTP->SessionID;
    aFileName = CharArray(argv[0]);
    gpFileWatcher->addPath(aSessionID, aFileName);
}

if(asTag(&argv[0]) == TYTEXT)
{
	aSessionID = gTP->SessionID;
    aFileName = argv[0].u.Text;
    gpFileWatcher->addPath(aSessionID, aFileName);
}

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0

SBGlue_RemoveFromFileWatcher

The  RemoveFromFileWatcher  Procedure removes a file to the AFileWatcher class

#endif

TVAL SBGlue_RemoveFromFileWatcher(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
QString aFileName;
long aSessionID;
StartFrame
EndFrame

/*  Make sure we have the proper number of arguments. */

if (argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

if((asTag(&argv[0]) != TYSTRING) && (asTag(&argv[0]) != TYTEXT))
{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

if(asTag(&argv[0]) == TYSTRING)
{
    aFileName = CharArray(argv[0]);
	aSessionID = gTP->SessionID;
    gpFileWatcher->removePath(aSessionID, aFileName);
}

if(asTag(&argv[0]) == TYTEXT)
{
    aFileName = argv[0].u.Text;
	aSessionID = gTP->SessionID;
    gpFileWatcher->removePath(aSessionID, aFileName);
}

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0

SBGlue_SendFileChangedEvent

The  SendFileChangedEvent sends a signal to all associated sessions of the context that triggered a fileChangedEvent

#endif

TVAL SBGlue_SendFileChangedEvent(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
QString aFileName;
QString aCabinetName;
long aSessionID;
StartFrame
EndFrame

/*  Make sure we have the proper number of arguments. */

if (argc != 2)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

if( (asTag(&argv[0]) != TYSTRING) && (asTag(&argv[0]) != TYTEXT) &&
	(asTag(&argv[1]) != TYSTRING) && (asTag(&argv[1]) != TYTEXT))
{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

if(asTag(&argv[0]) == TYSTRING)
{
    aFileName = CharArray(argv[0]);
}

if(asTag(&argv[0]) == TYTEXT)
{
    aFileName = argv[0].u.Text;
}

if(asTag(&argv[1]) == TYSTRING)
{
    aCabinetName = CharArray(argv[1]);
}

if(asTag(&argv[1]) == TYTEXT)
{
    aCabinetName = argv[1].u.Text;
}

aSessionID = gTP->SessionID;
gpFileWatcher->sendFileChangedEvent(aSessionID, aFileName, aCabinetName);

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0

SBGlue_SendStatusUpdateEvent

The SBGlue_SendStatusUpdateEvent sends a status update to all associated sessions of the context

#endif

TVAL SBGlue_SendStatusUpdateEvent(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
QString aMessage;
NUM aSessionID;

StartFrame
EndFrame

/*  Make sure we have the proper number of arguments. */

if (argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

if ((asTag(&argv[0]) != TYSTRING) && 
	(asTag(&argv[0]) != TYTEXT))
{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

if (asTag(&argv[0]) == TYSTRING)
{
    aMessage = CharArray(argv[0]);
}

if (asTag(&argv[0]) == TYTEXT)
{
    aMessage = argv[0].u.Text;
}

aSessionID = gTP->SessionID;
gpSessionManager->cbSendStatusUpdateToClient(aSessionID, aMessage);

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0

SBGlue_CountOfOpenContexts

The CountOfOpenContexts returns the number of open contexts

#endif

long SBGlue_CountOfOpenContexts()
{
	long aContextsInUse = 0;	// Context Index

	for(long i = 0; i < SBGLUE_MAX_CONTEXTS; ++i)
	{	
		gContextMutex[i].lock();	// Block until access granted
		// open context slots are identified by a value of -1
		if (gContextFlags[i].mInUse != -1) ++aContextsInUse;
		gContextMutex[i].unlock();
	}

	return(aContextsInUse);
}

/*-----------------------------------------------------------------
SBGlue_ContextOpen 
Summary:	Open a context, a session, and run the specified startup script.
            Returns an error OR the Lisp context Structure containing the context name, session ID, and startup scriptFileName.
			The contextStructure is used communicate with and manage the new context.    

Args:		scriptFileName		The startup script file name
			subscribeSW			TRUE if the calling context subscribes to the new context, FALSE if no subscription. 
Return:		contextStructure	The Lisp Structure containing the context name, session ID, and startup scriptFileName

Note:		Subscribing to the new context causes all writeln displays to pass through to the creating context's console
------------------------------------------------------------------*/
TVAL SBGlue_ContextOpen(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	POINTER		scriptFileNameStr = NULL;
	NUM			scriptFileNameLen = 0;
	QString		aAisOut; 
	QString		aContextName;
	QString		scriptFileName;
	QString		runScriptCommand("(runScript {");

	ACloseMode	aDefMode = geDefault;
	ASessionID	aSessionId = 0;
	bool		twoSessionsSW = FALSE;
	bool		subscribeSW = FALSE;


	// Protected variables
	StartFrame
	DeclareTVAL(aResult);        
	DeclareTVAL(contextName);        
	DeclareTVAL(contextStructure);        
	DeclareTVAL(tvalRequestId);
	DeclareTVAL(scriptFileName2);
	DeclareTVAL(sessionId);
	DeclareTVAL(ec);
	DeclareTVALArray(parms,3);
    EndFrame

	//  Expect aArgs: ipPath
	if (argc != 2)
	{	
		*aResult = TERROR((LpCHAR)"!contextOpen:aArgs!");
		FrameExit(*aResult);
	}
	// Get pointer to startup script file name argument
	FSmartbase_StringPtr(gCP, gTP, &argv[0], &scriptFileNameStr, &scriptFileNameLen);
	scriptFileName = scriptFileNameStr;
	
	// Get subscribeSW argument
	if ((argv[1].Tag == TYBOLE) && (argv[1].u.Bool == TRUE))
		subscribeSW = TRUE;
	else
		subscribeSW = FALSE;
	
	// Open a new context using the startup script embedded meta commands.
	aSessionId = gpSessionManager->openContext(scriptFileName, aContextName, aAisOut);
		if (aSessionId < 0)
		{
			*aResult = TERROR((LpCHAR)"!contextOpen:context open failed!");
			FrameExit(*aResult);
		}

	// Open a second session if that is the new protocol
    if (twoSessionsSW)
	{
	// Open a new session on the newly opened context.
	if ((aSessionId = gpSessionManager->openSession(aDefMode, aContextName, USRMGR_SYSUSRID/*aUsrId*/, USRMGR_SYSSECLVL/*aSecLvl*/, 0/*SessionId*/)) <= 0)
	{
		// Handle clean up and error return if session open fails.
		gpSessionManager->closeContext(-1, aContextName, geHard);
		*aResult = TERROR((LpCHAR)"!contextOpen:session open failed!");
		FrameExit(*aResult);
	}
	}

	//qDebug("aAisOut = %s", aAisOut.toLatin1().data());
	//qDebug("aContextName = %s", aContextName.toLatin1().data());

	// Return the new context/session structure.
	*contextName = aContextName.isEmpty() ? TSTRING(aAisOut.toLatin1().data()) : TSTRING(aContextName.toLatin1().data());

	*scriptFileName2 = TSTRING(scriptFileNameStr);
	*sessionId = TINT(aSessionId);

	//qDebug("Calling FSmartbase_Evals...");
	// Create, set, and return the new context/session structure.
	*contextStructure = FSmartbase_Evals(gCP, gTP,
		(LpCHAR)"(new Structure: Type: LocalContext: ContextName: #void SessionID: #void ScriptFileName: #void RequestID: #void)",FALSE);
	ExitOnError(*contextStructure);

	parms[0] = *contextStructure;
	parms[1] = TSYMBOL("ContextName");
	parms[2] = *contextName;
	//qDebug("Calling FSmartbase_Set...");
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);

	parms[1] = TSYMBOL("ScriptFileName");
	parms[2] = *scriptFileName2;
	//qDebug("Calling FSmartbase_Set...");
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);

	parms[1] = TSYMBOL("SessionID");
	parms[2] = *sessionId;
	//qDebug("Calling FSmartbase_Set...");
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);

	// Make the current session a subscriber to the new asynchronous session.
	if (subscribeSW == TRUE) 
		gpSessionManager->subscribe(gTP->SessionID, aSessionId, FALSE /* Add Subscriber */);

	// Submit the startup script for asynchronous evaluation in the newly opened context.
	parms[0] = *contextStructure;
	parms[1] = TSTRING(runScriptCommand.append(scriptFileName).append("})").toLatin1().data());

	//qDebug("Calling SBGlue_ContextSubmit...");
	*tvalRequestId = SBGlue_ContextSubmit(gCP, gTP, 2, &parms[0]);
	ExitOnError(*tvalRequestId);
	
	parms[1] = TSYMBOL("RequestID");
	parms[2] = *tvalRequestId;

	//qDebug("Calling FSmartbase_Set...");
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);

	// Return the newly created context structure with the startup script evaluation request ID
	FrameExit(*contextStructure);
}

/*-----------------------------------------------------------------
SBGlue_ContextSubmit 
Summary:	Submit a Lisp command to a context and session for asynchronous evaluation.
            Returns an error OR the request ID for late retrieval of the asynchronous evaluation results.

Args:		contextStructure	The Lisp context Structure returned from a previous contextOpen function call
     		lispCommand			The Lisp expression to be evaluated asynchronously in the specified context
Return:		requestId			The request ID for obtaining the results of the submission

Note:		Added to experiment with Lisp remote context/session management

Questions:	Do we need a connection manager between the context and the session?
			How do we capture the console writelns (is this what the consoleLog is for?)?
------------------------------------------------------------------*/
TVAL SBGlue_ContextSubmit(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	QDateTime	aDateTime;
	char*		lispCommand;
	NUM			lispCommandLen;
	ASessionID	aSessionId = 0;
	ARequestID	aRequestID;

	StartFrame
	DeclareTVAL(tvalSessionId);
	DeclareTVALArray(parms,2);
	EndFrame

	if (argc < 1 || argc > 2)
		FrameExit(TERROR((LpCHAR)"!contextSubmit:arglist!"));
	if (argv[0].Tag != TYSTRUCTURE)
		FrameExit(TERROR((LpCHAR)"!contextSubmit:1st arg must be a context Structure!"));
	if (argv[1].Tag != TYSTRING && argv[1].Tag != TYTEXT)
		FrameExit(TERROR((LpCHAR)"!contextSubmit:2nd arg must be a Lisp expression string!"));
	
	//	Use the current date & time as the submission date and time
	aDateTime = QDateTime::currentDateTime();

	//	Get a pointer to the Lisp expression to be evaluated (submitted)
	FSmartbase_StringPtr(gCP, gTP, &argv[1], &lispCommand, &lispCommandLen);

	//	Get the session ID from the specified context structure
	parms[0] = argv[0];
	parms[1] = TSYMBOL("SessionID");

	*tvalSessionId = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
	ExitOnError(*tvalSessionId);
	if (tvalSessionId->Tag != TYNUM)
		FrameExit(TERROR((LpCHAR)"!contextSubmit:invalid session ID in context Structure!"));
	aSessionId = tvalSessionId->u.Int;

	// Submit the request to evaluate the Lisp command asynchronously in the specified context
	aRequestID = gpSessionManager->submit(aSessionId, SBGLUE_EVENTCMD, 1 , lispCommand, aDateTime, NULL/*Data*/);

	// Return the request ID which can be used to retrieve the results of the asynchronous evaluation
	FrameExit(TINT(aRequestID));
}

/*-----------------------------------------------------------------
SBGlue_ContextBusy 
Summary:	Query wither or not the specified context is busy.

Args:		contextStructure	The Lisp context Structure returned from a previous contextOpen function call
Return:		returnCode			The return code will be true if the context is busy
------------------------------------------------------------------*/
TVAL SBGlue_ContextBusy(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	POINTER		apPathstr = NULL;
	NUM			aPathLen = 0;
	long		aRetcode;
	QString		aAisOut;
	QString		aContextName;
	ASessionID	aSessionId = 0;

	// Protected variables
	StartFrame
	DeclareTVAL(aResult);
	DeclareTVAL(tvalSessionId);
	DeclareTVAL(tvalContextName);
	DeclareTVALArray(parms,2);
    EndFrame

	//  Expect the first argument to be a context Structure
	if ((argc != 1) || (argv[0].Tag != TYSTRUCTURE))
	{	
		*aResult = TERROR((LpCHAR)"!contextExists:1st argument must be a context Structure!");
		FrameExit(*aResult);
	}
	parms[0] = argv[0];

	parms[1] = TSYMBOL("ContextName");
	// Get the context name from the context structure
	*tvalContextName = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
	ExitOnError(*tvalContextName);
	if ((tvalContextName->Tag != TYTEXT) && (tvalContextName->Tag != TYSTRING) && (tvalContextName->Tag != TYSYMBOL))
		FrameExit(TERROR((LpCHAR)"!contextExists:invalid context name in context Structure!"));
	FSmartbase_StringPtr(gCP, gTP, tvalContextName, &apPathstr, &aPathLen);
	aContextName = apPathstr;

	// Get the session ID from the context structure
	
	parms[1] = TSYMBOL("SessionID");
	*tvalSessionId = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
	ExitOnError(*tvalSessionId);
	if (tvalSessionId->Tag != TYNUM)
		FrameExit(TERROR((LpCHAR)"!contextExists:invalid session ID in context Structure!"));
	aSessionId = tvalSessionId->u.Int;

	// Close the session and release all resources
	aRetcode = gpSessionManager->isContextBusy(aSessionId, aContextName);
	if (aRetcode != 0)
		FrameExit(gCP->TObject_TRUE)
	else
		FrameExit(gCP->TObject_FALSE);
}

/*-----------------------------------------------------------------
SBGlue_ContextExists
Summary:	Query if the specified context exists.

Args:		contextName				The Lisp context name
Return:		contextStructure		The return code will be false if the context does NOT exist, and a contest Structure if the context does exist.
------------------------------------------------------------------*/
TVAL SBGlue_ContextExists(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	POINTER		apPathstr = NULL;
	NUM			aPathLen = 0;
	long		aRetcode;
	QString		aAisOut;
	QString		aContextName;
	ASessionID	aSessionId = 0;
	ASession*	apSession = NULL;
	AContextSt*	apContext = NULL;
	long		aRequestId = 0;

	// Protected variables
	StartFrame
	DeclareTVAL(aResult);
	DeclareTVAL(contextName);
	DeclareTVAL(scriptFileName);
	DeclareTVAL(contextStructure);
	DeclareTVAL(ec);
	DeclareTVALArray(parms, 3);
    EndFrame

	//  Expect the first argument to be a context Structure
	if (argc != 1)
	{	
		*aResult = TERROR((LpCHAR)"!contextExists:1st argument must be a context Structure or name!");
		FrameExit(*aResult);
	}
	else 
	if ((argv[0].Tag == TYTEXT) || (argv[0].Tag == TYSTRING) || (argv[0].Tag != TYSYMBOL))
	{
		*contextName = argv[0];
	}
	else
	{	
		*aResult = TERROR((LpCHAR)"!contextExists:1st argument must be a context Structure or name!");
		FrameExit(*aResult);
	}


	// Get the context name from the Lisp tval
	if ((contextName->Tag != TYTEXT) && (contextName->Tag != TYSTRING) && (contextName->Tag != TYSYMBOL))
		FrameExit(TERROR((LpCHAR)"!contextExists:invalid context name in context Structure!"));
	FSmartbase_StringPtr(gCP, gTP, contextName, &apPathstr, &aPathLen);
	aContextName = apPathstr;

	// Check to see if the context exists.
	aRetcode = gpSessionManager->isContextOpen(aContextName);
	if (aRetcode != TRUE)
		FrameExit(gCP->TObject_FALSE)
	else
	{
		// Get the context structure from the context name.
        apContext = gpSessionManager->getContextSt(aContextName);
		if (apContext <= 0)
		{
			*aResult = TERROR((LpCHAR)"!contextExists:invalid context name!");
			FrameExit(*aResult);
		}

		// Get the session id for the context.
		aSessionId = ((apContext->mAdminSessionId > 0) ? apContext->mAdminSessionId : apContext->mExeSessionID);
		if (aSessionId <= 0)
		{
			*aResult = TERROR((LpCHAR)"!contextExists:invalid context session ID!");
			FrameExit(*aResult);
		}
		apSession = gpSessionManager->getSession(aSessionId);
		if (apSession <= 0)
		{
			*aResult = TERROR((LpCHAR)"!contextExists:invalid context session ID!");
			FrameExit(*aResult);
		}

		// Convert relevaent names and IDs into Lisp TVALS.
		*contextName = TSTRING(apContext->mContextName.toLatin1().data());
		*scriptFileName = TSTRING(apContext->mStartupScriptName.toLatin1().data());

		// Create, set, and return the new context/session structure.
		*contextStructure = FSmartbase_Evals(gCP, gTP, (LpCHAR)"(new Structure: Type: LocalContext: ContextName: #void SessionID: #void ScriptFileName: #void RequestID: #void)",FALSE);
		ExitOnError(*contextStructure);

		parms[0] = *contextStructure;
	
		parms[1] = TSYMBOL("ContextName");
		parms[2] = *contextName;
		*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
		ExitOnError(*ec);

		parms[1] = TSYMBOL("ScriptFileName");
		parms[2] = *scriptFileName;
		*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
		ExitOnError(*ec);

		parms[1] = TSYMBOL("SessionID");
		parms[2] = TINT(aSessionId);
		*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
		ExitOnError(*ec);
	
		parms[1] = TSYMBOL("RequestID");
		parms[2] = TINT(aRequestId);
		*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
		ExitOnError(*ec);

		// Return the newly created context structure with the startup script evaluation request ID
		FrameExit(*contextStructure);
	}
}

/*-----------------------------------------------------------------
SBGlue_ContextStop
Summary:	Tell the specified context to stop all execution.

Args:		contextStructure		The Lisp context Structure returned from a previous contextOpen function call OR
								      the context name
Return:		returnCode				The return code will be true if the context exists
------------------------------------------------------------------*/
TVAL SBGlue_ContextStop(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	QString		aContextName;
	ASessionID	aSessionId = 0;

	// Protected variables
	StartFrame
	DeclareTVAL(aResult);
	DeclareTVAL(tvalSessionId);
	DeclareTVAL(tvalContextName);
	DeclareTVALArray(parms,2);
    EndFrame

	//  Expect the first argument to be a context Structure
	if (argc != 1)
	{	
		*aResult = TERROR((LpCHAR)"!contextStop:1st argument must be a context Structure!");
		FrameExit(*aResult);
	}
	else 
	if (argv[0].Tag == TYSTRUCTURE)
	{
		// Get the session ID from the context structure
		parms[0] = argv[0];

		parms[1] = TSYMBOL("ContextName");
		*tvalContextName = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
		ExitOnError(*tvalContextName);
		parms[1] = TSYMBOL("SessionID");		
		*tvalSessionId = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
		ExitOnError(*tvalSessionId);
		if (tvalSessionId->Tag != TYNUM)
			FrameExit(TERROR((LpCHAR)"!contextStop:invalid session ID in context Structure!"));
		aSessionId = tvalSessionId->u.Int;
		gpSessionManager->getContextId(aSessionId);
	}
	else 
	{	
		*aResult = TERROR((LpCHAR)"!contextStop:1st argument must be a context Structure!");
		FrameExit(*aResult);
	}

	// Set the session escape flag
	gpSessionManager->setSessionEscapeFlag(aSessionId, true);
	FrameExit(gCP->TObject_TRUE);

}

/*-----------------------------------------------------------------
SBGlue_ContextGetResult
Summary:	Return the current value resulting from the last evaluation
			in the specified context.

			Large grain multitasking is accomplished via the following general steps.
				A local context is opened with the contextOpen function.
				A Lisp expression is submitted for asynchronous evaluation via the contextSubmit function
				The asynchronously executing context is tested with the contextBusy function
				When the context is no longer busy, the return value is retrieved via the contextGetResult function

Notes:		Retrieving the result resets the result to "".
			In safe mode, large grain multitasking values can only be returned as String or Text values.
			If the return value is very large, then a file name can be returned.
			In unsafe mode, pointers into the local context can be returned and binary data can be copied into/outof 
			 the local context.
		    However, the contextGetResult is a safe mode function and only returns values as a String or Text values.
				
Example:	;; A simple example showing creating a local asynchronous context, 
			;;  running an asynchronous task, 
			;;  and closing the local asynchronous context 
			(setq context (contextOpen "AContextStartup.sl" true))
			(contextSubmit {(runMyAsynchTask)})
			(while (contextBusy context) do (sleep 1000.0))
			(setq result (contextGetResult context))
			(contextClose context)


Args:		contextStructure	The Lisp context Structure returned from a previous contextOpen function
Return:		returnValue			The return value resulting from the last evaluation in the specified context.
								 Always returned as a String or Text value. If the context is busy or if
								 there is no value available, then "" is returned.
------------------------------------------------------------------*/
TVAL SBGlue_ContextGetResult(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	QString		aResult;
	QString		aContextName;
	ASessionID	aSessionId = 0;

	// Protected variables
	StartFrame
	DeclareTVAL(tvalResult);
	DeclareTVAL(tvalSessionId);
	DeclareTVAL(tvalContextName);
	DeclareTVALArray(parms,2);
    EndFrame

	//  Expect the first argument to be a context Structure
	if (argc != 1)
	{	
		*tvalResult = TERROR((LpCHAR)"!contextGetResult:1st argument must be a context Structure!");
		FrameExit(*tvalResult);
	}
	else 
	if (argv[0].Tag == TYSTRUCTURE)
	{
		// Get the context name from the context structure
		parms[0] = argv[0]; // context struture
		parms[1] = TSYMBOL("ContextName");
		*tvalContextName = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
		ExitOnError(*tvalContextName);
		parms[1] = TSYMBOL("SessionID");
		*tvalSessionId = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
		ExitOnError(*tvalContextName);
		if (tvalSessionId->Tag != TYNUM)
		{
			*tvalResult = TERROR((LpCHAR)"!contextGetResult:Invalid session ID in context Structure!");
			FrameExit(*tvalResult);
		}
		aSessionId = tvalSessionId->u.Int;
	}
	else
	{	
		*tvalResult = TERROR((LpCHAR)"!contextGetResult:1st argument must be a context Structure!");
		FrameExit(*tvalResult);
	}


	// Return the latest evaluation result as a String or Text value.
	aResult = gpSessionManager->getResult(aSessionId);
	*tvalResult = TSTRING(aResult.toLatin1().data());
	FrameExit(*tvalResult);
}

/*-----------------------------------------------------------------
SBGlue_ContextGetDisplay
Summary:	Return the current display buffer from the specified local asynchronous context.
			
			During evaluation of the local asynchronous context, the display buffer is filled 
			with the writeln function (among others). The display buffer can be passed onto the
			parent context's console OR the display buffer can be used as a progress report on
			the local asynchronously executing context while it is busy evaluating.

			Large grain multitasking progress reporting is accomplished via the following general steps.
				A local context is opened with the contextOpen function (non-subscribe).
				A Lisp expression is submitted for asynchronous evaluation via the contextSubmit function
				The asynchronously executing context is tested with the contextBusy function
				When the context is no longer busy, the return value is retrieved via the contextGetReturnValue function

Notes:		The display buffer is truncated to only the last 4000 bytes of display value.
			Retrieving the current display buffer resets the display buffer to ""
				
Example:	;; A simple example showing creating a local asynchronous context, 
			;;  running an asynchronous task,
			;;  checking the asynchronous task for progress reports
			;;  and closing the local asynchronous context 
			(setq context (contextOpen "AContextStartup.sl" false))
			(contextSubmit {(runMyAsynchTask)})
			(while (contextBusy context) do 
				(sleep 50.0) 
				(setq progress (contextGetDisplay context))
				(writeln "Current status of MyAsynchTask is:" progress)
				)
			(setq result (contextGetResult context))
			(contextClose context)


Args:		contextStructure	The Lisp context Structure returned from a previous contextOpen function
Return:		returnValue			The return value resulting from the last evaluation in the specified context.
								 Always returned as a String or Text value. If the context is busy or if
								 there is no value available, then "" is returned.
------------------------------------------------------------------*/
TVAL SBGlue_ContextGetDisplay(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	QString		aResult;
	QString		aContextName;
	ASessionID	aSessionId = 0;

	// Protected variables
	StartFrame
	DeclareTVAL(tvalResult);
	DeclareTVAL(tvalSessionId);
	DeclareTVAL(tvalContextName);
	DeclareTVALArray(parms,2);
    EndFrame

	//  Expect the first argument to be a context Structure
	if (argc != 1)
	{	
		*tvalResult = TERROR((LpCHAR)"!contextGetDisplay:1st argument must be a context Structure!");
		FrameExit(*tvalResult);
	}
	else 
	if (argv[0].Tag == TYSTRUCTURE)
	{
		// Get the context name from the context structure
		parms[0] = argv[0]; // context structure
		parms[1] = TSYMBOL("ContextName");
		*tvalContextName = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
		ExitOnError(*tvalContextName);
		parms[1] = TSYMBOL("SessionID");
		*tvalSessionId = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
		ExitOnError(*tvalContextName);
		if (tvalSessionId->Tag != TYNUM)
		{
			*tvalResult = TERROR((LpCHAR)"!contextGetDisplay:Invalid session ID in context Structure!");
			FrameExit(*tvalResult);
		}
		aSessionId = tvalSessionId->u.Int;
	}
	else
	{	
		*tvalResult = TERROR((LpCHAR)"!contextGetDisplay:1st argument must be a context Structure!");
		FrameExit(*tvalResult);
	}


	// Reset and reset the session display buffer during or after evaluation.
	aResult = gpSessionManager->getDisplay(aSessionId);
	*tvalResult = TSTRING(aResult.toLatin1().data());
	FrameExit(*tvalResult);
}
/*-----------------------------------------------------------------
SBGlue_ContextClose 
Summary:	Close a context and session then release all memory and system resources.

Args:		contextStructure	The Lisp context Structure returned from a previous contextOpen function call
Return:		returnCode			The return code will be 0 if the close was successful

Note: (MFK)	Measured by Windows-8 Task Manager, executing contextClose appears to have
            a memory leak of between 2 to 5 meg which accumulates with each contextOpen
			and contextClose pair executed. As of this note, it does NOT appear to be
			related to the Smartbase engine (whose memory leak has been fixed), nor does
			it appear to be related to MySQL (whose mysql_end memory leak has been fixed).
------------------------------------------------------------------*/
TVAL SBGlue_ContextClose(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	POINTER		apPathstr = NULL;
	NUM			aPathLen = 0;
	long		aRetcode;
	QString		aAisOut;
	QString		aContextName;
	LpXCONTEXT	ipContext = NULL;
	ASessionID	aSessionId = 0;
	AContextSt*	apContext;

	// Protected variables
	StartFrame
	DeclareTVAL(aResult);
	DeclareTVAL(tvalSessionId);
	DeclareTVAL(tvalContextName);
	DeclareTVAL(ec);
	DeclareTVALArray(parms,2);
    EndFrame

	//  Expect the first argument to be a context Structure
	if ((argc != 1) || (argv[0].Tag != TYSTRUCTURE))
	{	
		*aResult = TERROR((LpCHAR)"!contextClose:1st argument must be a context Structure!");
		FrameExit(*aResult);
	}

	// Get the context name from the context structure
	parms[0] = argv[0]; // context structure
	parms[1] = TSYMBOL("ContextName");
	*tvalContextName = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
	ExitOnError(*tvalContextName);
	if ((tvalContextName->Tag != TYTEXT) && (tvalContextName->Tag != TYSTRING) && (tvalContextName->Tag != TYSYMBOL))
		FrameExit(TERROR((LpCHAR)"!contextClose:invalid context name in context Structure!"));
	FSmartbase_StringPtr(gCP, gTP, tvalContextName, &apPathstr, &aPathLen);
	aContextName = apPathstr;

	// Get the session ID from the context structure
	parms[1] = TSYMBOL("SessionID");
	*tvalSessionId = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
	ExitOnError(*tvalSessionId);
	if (tvalSessionId->Tag != TYNUM)
		FrameExit(TERROR((LpCHAR)"!contextClose:invalid session ID in context Structure!"));
	aSessionId = tvalSessionId->u.Int;

	// Check for context busy
	aRetcode = gpSessionManager->isContextBusy(aSessionId, aContextName);
	if (aRetcode != 0)
		FrameExit(TERROR((LpCHAR)"!contextClose:cannot close a busy context!"));

	// FCC: This is not necessary. This will eventually be called in AContextThread::run()
	// Flush the Lisp code in the context and release all resources
	// MFK: Does not seem to get rid of Lisp context memory allocations completely??
	//ipContext = (LpXCONTEXT)gpSessionManager->getContextPtr(aContextName);
	//if (ipContext != NULL) 
	//{
	//	aRetcode = SBGlue_FlushLispCodeInContext((void*)ipContext);
	//	if (aRetcode < 0)
	//	{
	//		*aResult = TERROR((LpCHAR)"!contextClose:error flushing Lisp resources in context!");
	//		FrameExit(*aResult);
	//	}
	//	// Must set mpCP to NULL so system will not crash next time SBGlue_FlushLispCodeInContext is executed.
	//	apContext = gpSessionManager->getContextSt(aContextName);
	//	apContext->mpCP = NULL;
	//}

	// Cause the current session to unsubscribe from the session to be closed.
	gpSessionManager->subscribe(gTP->SessionID, aSessionId, TRUE /* Unsubscribe */);

	//// Close the session and release all resources
	//aRetcode = gpSessionManager->closeSession(geHard /*ACloseMode*/, -1 /*iConnectId*/, aSessionId);
	//if (aRetcode < 0)
	//{
	//	*aResult = TERROR((LpCHAR)"!contextClose:error closing session!");
	//	FrameExit(*aResult);
	//}

	// FCC: Closing the context will also close all associated sessions
	// This function will also wait until the thread terminates
	// Close the context and release all resources
	aRetcode = gpSessionManager->closeContext(-1 /*iConnectId*/, aContextName, geDefault /*ACloseMode*/);
	if (aRetcode < 0)
	{
		*aResult = TERROR((LpCHAR)"!contextClose:error closing context!");
		FrameExit(*aResult);
	}

	// Reset the context Structure to reflect the close
	parms[0] = argv[0];
	parms[2] = gCP->TObject_VOID;

	parms[1] = TSYMBOL("ContextName");
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);
	parms[1] = TSYMBOL("ScriptFileName");
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);
	parms[1] = TSYMBOL("SessionID");
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);
	parms[1] = TSYMBOL("RequestID");
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);

	FrameExit(gCP->TObject_TRUE);
}

/*-----------------------------------------------------------------
SBGlue_ContextMyself
Summary:	 The contextMyself function returns a context Structure for the current context. 
             This context structure can then be used with all of the other appropriate AIS multi-tasking functions.   

Args:		none
Return:		contextStructure	The Lisp Structure containing the context name, session ID, and startup scriptFileName

------------------------------------------------------------------*/
TVAL SBGlue_ContextMyself(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	ASessionID	aSessionId = 0;
	ASession*	apSession = NULL;
	AContextSt*	apContext = NULL;
	long		aRequestId = 0;


	// Protected variables
	StartFrame
	DeclareTVAL(aResult);        
	DeclareTVAL(contextName);        
	DeclareTVAL(scriptFileName);        
	DeclareTVAL(contextStructure);        
	DeclareTVAL(ec);
	DeclareTVALArray(parms, 3);
    EndFrame

	//  Expect aArgs: ipPath
	if (argc != 0)
	{	
		*aResult = TERROR((LpCHAR)"!contextMyself:aArgs!");
		FrameExit(*aResult);
	}

	// Get the session id for the current session.
	aSessionId = gTP->SessionID;
	apSession = gpSessionManager->getSession(aSessionId);
	if (apSession <= 0)
	{
		*aResult = TERROR((LpCHAR)"!contextMyself:invalid current session ID!");
		FrameExit(*aResult);
	}

	// Get the context pointer for the current session.
	apContext = apSession->mpContext;
	if (apContext <= 0)
	{
		*aResult = TERROR((LpCHAR)"!contextMyself:invalid current context ptr!");
		FrameExit(*aResult);
	}

	// Convert relevaent names and IDs into Lisp TVALS.
	*contextName = TSTRING(apContext->mContextName.toLatin1().data());
	*scriptFileName = TSTRING(apContext->mStartupScriptName.toLatin1().data());

	// Create, set, and return the new context/session structure.
	*contextStructure = FSmartbase_Evals(gCP, gTP, (LpCHAR)"(new Structure: Type: LocalContext: ContextName: #void SessionID: #void ScriptFileName: #void RequestID: #void)",FALSE);
	ExitOnError(*contextStructure);

	parms[0] = *contextStructure;
	
	parms[1] = TSYMBOL("ContextName");
	parms[2] = *contextName;
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);

	parms[1] = TSYMBOL("ScriptFileName");
	parms[2] = *scriptFileName;
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);

	parms[1] = TSYMBOL("SessionID");
	parms[2] = TINT(aSessionId);
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);
	
	parms[1] = TSYMBOL("RequestID");
	parms[2] = TINT(aRequestId);
	*ec = FSmartbase_Setv(gCP, gTP, 3, &parms[0]);
	ExitOnError(*ec);

	// Return the newly created context structure with the startup script evaluation request ID
	FrameExit(*contextStructure);
}

/*-----------------------------------------------------------------
SBGlue_ContextSubscribe
Summary:	The contextSubscribe function subscribes current context to the specified asynchronous context. 
            This context structure can then be used with all of the other appropriate AIS multi-tasking functions.   

Args:		contextStructure	The context structure returned from a previous contextOpen function.
     		subscribeSW			The subscribe switch (true = subscribe, false = unsubscribe).
Return:		errCode				The error code for the subscribe (0 if no errors).

------------------------------------------------------------------*/
TVAL SBGlue_ContextSubscribe(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
	long	aErrCode;			// Error code returned from the session manager.
	bool	aRemove = false;	// Remove aCurSessionId from subscriber list for aSessionID
	long	aSessionID;			// The session whose output is being captured.

	StartFrame
	DeclareTVAL(sessionID);
	DeclareTVALArray(parms, 2);
	EndFrame

	if (argc < 1 || argc > 2) 
		FrameExit(TERROR((LpCHAR)"contextSubscribe: arglist"));
	if (argv[0].Tag != TYSTRUCTURE)
		FrameExit(TERROR((LpCHAR)"contextSubscribe: First arg must be a context structure."));
	if (argc == 2)
	{	
		if (argv[1].Tag != TYBOLE)
			FrameExit(TERROR((LpCHAR)"contextSubscribe: Second arg must be a boolean."));
		if (argv[1].u.Bool == TRUE) aRemove = false; else aRemove = true;
	}

	parms[0] = argv[0];
	parms[1] = TSYMBOL("SessionID");

	*sessionID = FSmartbase_Refv(gCP, gTP, 2, &parms[0]);
	ExitOnError(*sessionID);

	if (sessionID->Tag != TYNUM)
		FrameExit(TERROR((LpCHAR)"contextSubscribe: Context Session ID invalid."));
	aSessionID = sessionID->u.Int;
	aErrCode = gpSessionManager->subscribe(gTP->SessionID, aSessionID, aRemove);
	FrameExit(TINT(aErrCode));
}

/*-----------------------------------------------------------------
SBGlue_ContextList
Summary:	The SBGlue_ContextList function returns a Vector of currently open context names. 

Args:		none
Return:		nameVector		The Lisp Vector containing the names of currently open contexts

------------------------------------------------------------------*/
TVAL SBGlue_ContextList(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
	QString		aContextNames;
	TVAL        stringToVector = TGVALUE("stringToVector");

	// Protected variables
	StartFrame
	DeclareTVAL(aResult);        
	DeclareTVAL(contextNames);        
	DeclareTVAL(namesVector);        
    EndFrame

	//  Expect aArgs: ipPath
	if (argc != 0)
	{	
		*aResult = TERROR((LpCHAR)"!contextList:aArgs!");
		FrameExit(*aResult);
	}

	// Get the list of names of the currently opened contexts.
	aContextNames = gpSessionManager->getCurrentContexts();
	*contextNames = TSTRING(aContextNames.toLatin1().data());
	ExitOnError(*contextNames);

	// Convert String to Vector of Names.
	*namesVector = FSmartbase_Eval(gCP,gTP,stringToVector,2,*contextNames,TSTRING("\n"));
    ExitOnError(*namesVector);

	FrameExit(*namesVector);
}
