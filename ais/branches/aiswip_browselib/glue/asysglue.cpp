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

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/glue/asysglue.cpp
									Glue
This source file contains the file IO functions which provide the lowest level system i/o interface
for the SmartBase engine.

CHANGE HISTORY
Version	Date		Who		Change
3.1000	11/26/2007	tim		Modified to use REAL as necessary to support large file support in 32 bit version of AIS
3.1000	11/25/2007	tim		Made Windows locking optional (_WINLOCK) - Qt sometimes does not return
							a file handle so calling locking() will fail unexpectedly!
3.0000	11/24/2007	tim		Removed _MAC support - the old MAC OS is dead. The new MAC OS is a
							unix derivative which we have not targeted for support.
3.0000	11/24/2007	tim		Fixed a bunch of bugs introduced by work done by rmf.
3.0000	11/24/2007	tim		Merged Ted's case-insensitive open for non-windows systems.
1.0102	8/07/2006	rmf		Use QFile for file I/O operations
1.0057	3/18/2005	tlw		Update documentation
1.0000	1/17/2002	tim		SysGlue derived from AnsShell
												--------------- ---------------
NOTES
The external functions in this module are registered as callbacks in the SmartBase engine.
None of these functions perform dialog with the user.

QFile Notes
QFile provides us with large file support on both 32 and 64 bit platforms. Note that files > 2G
are not supported on FAT32 -- you must use NTFS.
QFile seems to have a problem returning Window File Handles. The problem seems related to the number
of times a file is opened and closed. The only time we need Window File Handles is to use the 
locking() function. For the time being, I have disabled the file locking functionality as we do 
not acutally use it in production. TM
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	---------------------------------------- IMPORTS ---------------------------------------------


#ifdef _WIN
//#include <windows.h>
#include <sys/locking.h>
#include <io.h>
//#else
//#ifdef _MAC
//#include <console.h>  //_MAC no longer supported!
//#endif
#endif

#include <QtCore/QDir>
#include <QtCore/QMutex>
#include <QtCore/QFile>
#include "asessionmgr.h"
#include "acontextthread.h"

#include "asysglue.h"
// RCA
#include <iostream>
using namespace std;

//!\brief Defines the sleep time for a thread.
#define MAXIOSLEEPSBEFOREERROR 1000

/*
SysGlue GLOBAL VARIABLES

Define all sysglue global variables within this program. Note that these global variables
should NOT be accessed by any other program because of the mutex locking strategies employed
by the functions in this program that use them. DO NOT PLACE THESE VARIABLES IN THE sysglue.h FILE.
*/

//!\brief Mutex to synchronize thread access to all globals.
QMutex			gGlobalMutex;
//!\brief SysGlue_Open function mutex lock.
QMutex			gFileOpenMutex;
//!\brief Sleep time in usecs.
NUM             gSleepTime = 1;			// Sleep time in msecs
BOLE            gConsole = FALSE;
//!\brief Maximum number of files that can be opened.
NUM             gMaxFiles = FOPEN_MAX;

/*!
\brief Array of pointers to list of opened files.
\par Note:
The entries in the array are most often contained in a variable called fileID. 
This is often referred to as the file slot in the comments throughout the program.
File slots are indexed from 1. This means the array index 0 is never used.
*/
QFile			*gOpenFiles[FOPEN_MAX+1];
/*!
\brief Lock status of opened file.
\par Note:
The entries in the array are most often contained in a variable called fileID. 
This is often referred to as the file slot in the comments throughout the program.
File slots are indexed from 1. This means the array index 0 is never used.
*/
BOLE            gOpenLocks[FOPEN_MAX+1];
/*!
\brief Type of opened file.
\par Note:
The entries in the array are most often contained in a variable called fileID. 
This is often referred to as the file slot in the comments throughout the program.
File slots are indexed from 1. This means the array index 0 is never used.
*/
NUM             gOpenTypes[FOPEN_MAX+1];
/*!
\brief For database files. Keeps track of number of database users.
\par Note:
The entries in the array are most often contained in a variable called fileID. 
This is often referred to as the file slot in the comments throughout the program.
File slots are indexed from 1. This means the array index 0 is never used.
*/
NUM             gOpenUsers[FOPEN_MAX+1];
/*!
\brief For database files. Keeps track of the thread that the database uses.
\par Note:
The entries in the array are most often contained in a variable called fileID. 
This is often referred to as the file slot in the comments throughout the program.
File slots are indexed from 1. This means the array index 0 is never used.
*/
NUM				gOpenContexts[FOPEN_MAX+1];
NUM				gOpenMode[FOPEN_MAX+1];
//!\brief Mutex to synchronize access to data in a file slot.
QMutex			gFilesMutex[FOPEN_MAX+1];
/*!
\brief Holds description of the last file error encountered.
\par Note:
This is across all contexts so it is possible to get the wrong error reported to a
context if multiple errors are occuring in different contexts.
*/
CHAR			gLastError[FILE_ERROR_STRING_SIZE]; 

/*!
\brief WaitForMutex() attempts to grab a mutex lock.

\param gCP
\param myMutex
\param iUSecs A long integer specifying the length of time to try locking the mutex.
\return 
-# true if mutex lock is successful.
-# false if mutex lock is unsuccessful.
*/
bool WaitForMutex(LpXCONTEXT gCP, QMutex* myMutex, long iMsecs)
{	
	bool aGotLock = false;
	while (!(aGotLock = myMutex->tryLock()) && iMsecs > 0)
	{	AContextThread* sessionMgrContextThread = (AContextThread*)gCP->SessionMgrContextThread;	// Use thread sleep
		sessionMgrContextThread->localSleep(1L/*Msec*/);
		--iMsecs;
	}
	return aGotLock;
}

/*!
\brief SysGlue_lastError() simply copies the last error message to gLastError.

\param gCP
\param gTP
\param opDataPtr A long pointer to the error message to be copied.
\return 
	-# 4-byte 0 if successful.
	-# 4-byte Negative integer code if a problem occurs. 
\par Note:
This routine is not very smart yet. No mutex lock or attention is paid to the current context. 
*/
NUM SysGlue_lastError(LpXCONTEXT gCP,LpTHREAD gTP, char* opDataPtr)
{
    // Unused Variables
    gCP = gCP;
    gTP = gTP;

	strcpy(opDataPtr, gLastError);
	return 0;
}

/*!
\brief SysGlue_showFileSlot() displays the file slot information.

\param gCP
\param gTP
\param iFileID A 4-byte file identifier.
\param opDataPtr A long pointer where the file slot information will be stored.
\return 
	-# 4-byte 0 if successful.
	-# 4-byte Negative integer code if a problem occurs. 
*/
NUM SysGlue_showFileSlot(LpXCONTEXT gCP, LpTHREAD gTP, NUM iFileID, char* opDataPtr)
{
    // Unused Variables
    gTP = gTP;

	if ((iFileID < 1) || (iFileID > gMaxFiles)) 
	{	strcpy(opDataPtr,"FileID of range");
		return SYSGLUE_BADFILEID;
	}	
	if (!WaitForMutex(gCP, &gFilesMutex[iFileID], 6000))
	{	strcpy(opDataPtr,"Could not grab mutext lock");
		return SYSGLUE_MUTEXLOCKSET;
	}

	/* Check if file status is closed.*/
	if (gOpenFiles[iFileID] == NULL)
	{	gFilesMutex[iFileID].unlock();
		strcpy(opDataPtr,"File slot not used.");
		return SYSGLUE_FILE_NOT_OPEN;
	}

	/*  copy file slot report line into output buffer*/
	sprintf(opDataPtr, "FileID=%ld File=%s Locked=%d Type=%ld Users=%ld Mode=%ld",
        iFileID, qPrintable(gOpenFiles[iFileID]->fileName()),
        gOpenLocks[iFileID], gOpenTypes[iFileID], gOpenUsers[iFileID], gOpenMode[iFileID]);
	gFilesMutex[iFileID].unlock();
	//if (!ReleaseMutex(gFilesMutex[iFileID])) {
	//	strcpy(opDataPtr,"Could not release mutext lock");
	//	_FSmartbase_Throw(SYSGLUE_MUTEXLOCKRELEASE);
	//	}
	return 0;
}

/*!
\brief  SysGlue_IOInit() initializes the SmartBase global variables.

\return 0 
*/
NUM SysGlue_IOInit(void)
{
	NUM         aSlot;

	//gGlobalMutex = CreateMutex( 
	//		NULL,                       // no security attributes
	//		FALSE,                      // initially not owned
	//		(LPCTSTR)"");	

	//gFileOpenMutex = CreateMutex( 
	//		NULL,                       // no security attributes
	//		FALSE,                      // initially not owned
	//		(LPCTSTR)"");	

	/*  Reset the open file tables before proceeding. */
	/*  Note:   These tables are used to help in locking */
	/*          database files as well as in exit clean up. */

	gLastError[0] = '\0';

	for (aSlot = 1; aSlot <= gMaxFiles; ++aSlot) 
	{	if (gOpenFiles[aSlot] != NULL)
			delete gOpenFiles[aSlot];		
		gOpenFiles[aSlot] = NULL;
		gOpenContexts[aSlot] = 0;
		gOpenLocks[aSlot] = FALSE;
		gOpenTypes[aSlot] = 0;
		gOpenUsers[aSlot] = 0;
		//	gFilesMutex[aSlot] = CreateMutex( 
		//						NULL,  // no security attributes
		//						FALSE, // initially not owned
		//						(LPCTSTR)"");
	}
	return 0;
}

/*!
\brief  Sysglue_write() is the host support for the file write callback function.

\param  gCP
\param  gTP
\param  iFileID		A 4-byte file identifier.
\param  iLength		A REAL length specifying the number of bytes to write.
\param  ipDataPtr	A long pointer to the data to be written.
\return 
	-# 4-byte 0 if successful.
	-# 4-byte Negative integer code if a problem occurs. 
*/
NUM SysGlue_write(LpXCONTEXT gCP, LpTHREAD gTP, NUM iFileID, NUM iLength, const char* ipDataPtr)
{
    // Unused Variables
    gTP = gTP;

	NUM			aCode;
	//NUM			aErrorCode = SYSGLUE_FILEWRITE;

	if ((iFileID < 1) || (iFileID > gMaxFiles)) return SYSGLUE_BADFILEID;
	if (iLength < 0) return SYSGLUE_BAD_WRITE_LEN;
	if (!WaitForMutex(gCP, &gFilesMutex[iFileID], 1000))
		return SYSGLUE_MUTEXLOCKSET;

	/* Check if file status is closed. This is an application error not a system error. */
	if (gOpenFiles[iFileID] == NULL)
	{	gFilesMutex[iFileID].unlock();
		return SYSGLUE_FILE_NOT_OPEN;
	}

	gFilesMutex[iFileID].unlock();

	/* Write to the file. */
	aCode = gOpenFiles[iFileID]->write( ipDataPtr, iLength );
	if (aCode <= 0) 
	{	sprintf(gLastError, "fwrite returned code %ld", aCode);
		return -1;
		//FSmartbase_Throw(gCP,gTP,errorCode); //TM -- R removed this. Put back in?
	}
	return 0;
}

/*!
\brief  Sysglue_read() is the host support for the file read callback function.

\param  gCP
\param  gTP
\param  iFileID		A 4-byte file identifier.
\param  iLength		A NUM specifying the number of bytes to read.
\param  opDataPtr	A long pointer to the data to be read.
\return 
	-# 4-byte 0 if successful.
	-# 4-byte Negative integer code if a problem occurs. 
\par Note:
Host may redirect to a different error handler at its own discretion.
*/

NUM SysGlue_read(LpXCONTEXT gCP, LpTHREAD gTP, NUM iFileID, NUM iLength, char* opDataPtr)
{   
    // Unused Variables
    gTP = gTP;

	NUM				aCode;
	//NUM				aErrorCode = SYSGLUE_FILEREAD;

	if ((iFileID < 1) || (iFileID > gMaxFiles)) return SYSGLUE_BADFILEID;
	if (!WaitForMutex(gCP, &gFilesMutex[iFileID], 6000))
		return SYSGLUE_MUTEXLOCKSET;

	/* Check if file status is closed. This is an application error not a system error. */
	if (gOpenFiles[iFileID] == NULL)
	{	gFilesMutex[iFileID].unlock();
		return SYSGLUE_FILE_NOT_OPEN;
	}
	gFilesMutex[iFileID].unlock();

	/* Read from the file. */
	aCode = gOpenFiles[iFileID]->read(opDataPtr, iLength);

	//QByteArray temp;
	//temp = gOpenFiles[iFileID]->

	if (aCode <= 0) 
	{	sprintf(gLastError, "fread returned code %ld", aCode);
		return -1;
		//FSmartbase_Throw(gCP,gTP,errorCode); //TM -- R removed this. Put back in?
	}
	return 0;
}

/*!
\brief  Sysglue_seek() is the OS support for the file seek callback function.

\param	gCP
\param	gTP
\param  iFileID		A 4-byte file identifier.
\param  iOffset		A REAL specifying the number of bytes to seek.
\param  iCode		A 4-byte integer code specifying the place to seek from.
\return 
	-# REAL Resulting file position if successful.
	-# REAL Negative numeric integer code if a problem occurs. 
*/

REAL SysGlue_seek(LpXCONTEXT gCP, LpTHREAD gTP, NUM iFileID, REAL iOffset, NUM iCode)
{   
    // Unused Variables
    gTP = gTP;

	REAL			aNewOffset;

	if ((iFileID < 1) || (iFileID > gMaxFiles)) return SYSGLUE_BADFILEID;
	if (!WaitForMutex(gCP, &gFilesMutex[iFileID], 1000))
		return SYSGLUE_MUTEXLOCKSET;

	/* Check if file status is closed. This is an application error not a system error. */
	if (gOpenFiles[iFileID] == NULL)
	{	gFilesMutex[iFileID].unlock();
		return SYSGLUE_FILE_NOT_OPEN;
	}
	gFilesMutex[iFileID].unlock();

	/* Seek in the file from the location specified by code */
	switch (iCode)
    {	
		//TM - note that all the ifdefs for Linux are not included in this code. My
		//assumption is that Qt handles the differences for us.

		/* Seek from current location */
		case 0:
			aNewOffset = gOpenFiles[iFileID]->pos();
			gOpenFiles[iFileID]->seek((NUM)(iOffset + aNewOffset));
			aNewOffset = gOpenFiles[iFileID]->pos();
			return aNewOffset;
			break;

		/* Seek from start of file */
		case 1:
			gOpenFiles[iFileID]->seek((NUM)iOffset);
			aNewOffset = gOpenFiles[iFileID]->pos();
			return aNewOffset;
			break;

		/* Seek from end of file */
		case 2:
			gOpenFiles[iFileID]->seek((NUM)(gOpenFiles[iFileID]->size() + iOffset));
			aNewOffset = gOpenFiles[iFileID]->pos();
			return aNewOffset;
			break;

		/*  Invalid seek code. */
		default:			
			return SYSGLUE_BAD_SEEK_LOCATION;
			break;
	}
	return -1;
}

/*!
\brief  Sysglue_resize() is the host support for the file resize callback function.

\param	gCP
\param	gTP
\param  iFileID		A 4-byte file identifier.
\param  ioNewsize	A REAL specifying the new size of the file.
\return 
	-# REAL Resulting new end of file position, if successful.
	-# REAL Negative integer code if a problem occurs. 
\par Note:
Host may redirect to a different error handler at its own discretion.
*/

REAL SysGlue_resize(LpXCONTEXT gCP, LpTHREAD gTP, NUM iFileID, REAL ioNewsize)
{
    // Unused Variables
    gTP = gTP;

	REAL     aEofOffset;
//	NUM     aCnt;
//	char    aDummy = 0;
//	NUM     aCode;

	if ((iFileID < 1) || (iFileID > gMaxFiles)) return SYSGLUE_BADFILEID;
	if (ioNewsize < 0) return SYSGLUE_BAD_FILE_SIZE;
	if (!WaitForMutex(gCP, &gFilesMutex[iFileID], 6000))
		return SYSGLUE_MUTEXLOCKSET;

	/* Check if file status is closed. This is an application error not a system error. */
	if (gOpenFiles[iFileID] == NULL)
	{	gFilesMutex[iFileID].unlock();
		return SYSGLUE_FILE_NOT_OPEN;
	}
	gFilesMutex[iFileID].unlock();
	gOpenFiles[iFileID]->seek(gOpenFiles[iFileID]->size());
	aEofOffset = gOpenFiles[iFileID]->pos();

	//Use QFile resize
	if (!gOpenFiles[iFileID]->resize((NUM)ioNewsize))
		{	sprintf(gLastError, "fwrite in resize failed");
			return -1;
		}

//	#ifdef _WIN
//	/* Windows NT supports releasing file space. */
//	//TM -- I'm surprised the Qt object does not provide a method. Check
//	//if it does and just got missed by R.
//	chsize(gOpenFiles[iFileID]->handle(), ioNewsize);
//	#else
//	/* ANSI does not support releasing file space, so we */
//	/* must at least support grabbing extra file space. */
//	//TM -- somebody removed the following code? Why? 
//	if (ioNewsize > aEofOffset) 
//	{	for (aCnt = 0; aCnt < (size_t)(ioNewsize - aEofOffset); ++aCnt) 
//		{	aCode = gOpenFiles[iFileID]->write(&aDummy, 1);
//			if (aCode < 0) 
//			{	sprintf(gLastError, "fwrite in resize returned code %ld", aCode);
//				return -1;
//			}
//      }
//    }
//	#endif
	return ioNewsize;
}



/*!
\brief  Sysglue_close1() is the host support for the individual file close function. Only called by SysGlue_close().

\param	gCP
\param	gTP
\param  iFileID		A 4-byte file identifier.
\param  iOpcode		A value of zero (0) requests that the file be erased after close. Otherwise, 
					the file is closed and placed permanently on disk.
\return 
	-# 4-byte Positive integer code if successful.
	-# 4-byte Negative integer code if a problem occurs. 
\par Note:
This routine is not going to be thread safe if mutliple threads are allowed in a context. It
will have to be reworked at that time. It is fine as long as there is a single thread per
context.
*/

NUM SysGlue_close1(LpXCONTEXT gCP, LpTHREAD gTP, NUM iFileID, NUM iOpcode)
{
    // Unused Variables
    gCP = gCP;
    gTP = gTP;

#ifdef _WINLOCK
	NUM aFileNum;
#endif

	/* Decrement the user count and if it drops to zero we can close the database file. */
	--gOpenUsers[iFileID];
	if ((gOpenLocks[iFileID]) && (gOpenUsers[iFileID] <= 0)) 
	{   
		#ifdef _WINLOCK
		/*  Unlock the file, to provide for concurrent users. */
		aFileNum = gOpenFiles[iFileID]->handle(); //TM -- Check if Qt object has method.
			locking(aFileNum, _LK_UNLCK, 1);
		gOpenFiles[iFileID]->reset();
		gOpenLocks[iFileID] = FALSE;
		#endif
	}

	if (gOpenUsers[iFileID] <= 0) 
	{	gOpenFiles[iFileID]->close();
		/* With this close option, we must remove the file. */
		if (iOpcode == 0)
			gOpenFiles[iFileID]->remove();

		delete gOpenFiles[iFileID];
		gOpenFiles[iFileID] = NULL;
		gOpenTypes[iFileID] = 0;
		gOpenLocks[iFileID] = FALSE;
		gOpenUsers[iFileID] = 0;
		gOpenContexts[iFileID] = 0;
	}
	return 0;
}

/*!
\brief  Sysglue_close() is the host support for the file close callback function.

\param	gCP
\param	gTP
\param  iFileID		A 4-byte file identifier. Pass 0 to close all files in the specified context.
\param  iOpcode		A value of zero (0) requests that the file be erased after close. Otherwise, 
					the file is closed and placed permanently on disk.
\return 
	-# 4-byte Positive integer code if successful.
	-# 4-byte Negative integer code if a problem occurs. 
\par Note:
Host may redirect to a different error handler at its own discretion.
\par
This routine is not going to be thread safe if mutliple threads are allowed in a context. It
will have to be reworked at that time. It is fine as long as there is a single thread per
context.
*/

NUM SysGlue_close(LpXCONTEXT gCP, LpTHREAD gTP, NUM iFileID, NUM iOpcode)
{   
	NUM             aSlot;
	NUM				aResult;

	/* Close all open files without erasing */
	if (iFileID == -100)  //TM -- added by R?
	{	SysGlue_IOInit();
		return 0;
	}
	else if ((iFileID < 0) || (iFileID > gMaxFiles)) 
		return SYSGLUE_BADFILEID; //TM -- added by R?
	/* Close only the file specified */
	else if (iFileID > 0)
	{	if (!WaitForMutex(gCP, &gFilesMutex[iFileID], 6000))
			return SYSGLUE_MUTEXLOCKSET;

		/* Check if file status is closed. This is an application error not a system error. */
		if (gOpenFiles[iFileID] == NULL) 
		{	gFilesMutex[iFileID].unlock();
			return SYSGLUE_FILE_NOT_OPEN;
		}
		aResult = SysGlue_close1(gCP, gTP, iFileID, iOpcode);
		gFilesMutex[iFileID].unlock();
		return aResult;
	}
	/* Close all the currently open files for the current context? */
	else if (iFileID == 0) 
		{	/*  Close all of the open files for the current context. */  
			/*  Try and acquire individual mutex slot locks on each file opened in context */
			for (aSlot = 1; aSlot <= gMaxFiles; aSlot++) 
			{	if (gOpenContexts[aSlot] == (NUM)gCP) 
				{	if (!WaitForMutex(gCP, &gFilesMutex[aSlot], 6000))	/* try and acquire a mutex lock on the slot */
						return SYSGLUE_MUTEXLOCKSET;
				}
			}

			/* Close each file belonging to context */
			int aFailure = 0;
			for (aSlot = 1; aSlot <= gMaxFiles; aSlot++) 
			{	if (gOpenContexts[aSlot] != (NUM)gCP) continue;
				if (gOpenFiles[iFileID] != NULL) 
				{	aResult = SysGlue_close1(gCP, gTP, iFileID, iOpcode);	/* Close this file. */
					gFilesMutex[aSlot].unlock();
					if (aResult != 0) aFailure = 1; // Delay failure message until we have closed all files and cleared mutex locks
				}
			}
			if (aFailure != 0) return SYSGLUE_BAD_CONTEXT_WIDE_FILE_CLOSE;
		}	

	return 0;
}

/*!
\brief  Sysglue_open() is the host support for the file open callback function.

\param	gCP
\param	gTP
\param  ipPathname	Path and filename of the file to be opened.
\param  iMode		The mode switch. 
-# (0) Requests that only an existing file be opened. 
-# (1) Indicates that a new file should be created, or an existing file overwritten. 
-# (2) Indicates that an existing database file be opened for read only transactions,
      shared with other users (valid for database files only).
-# (3) Requests that an inserted repository file be opened. 
\param  iType		The file type switch which determines the type of host data file which is opened.
-# (0) requests a Smartscript text file.
-# (1) requests a Spreadsheet binary file
-# (2) requests a Smarttable binary file.
-# (3) requests a Workspace binary file.
-# (4) requests an Object binary file.
-# (5) requests an Object Database file.
\return The non-zero integer file identifier or zero to indicate an  error condition.
\par Note:
Note that the filename has a maximum length which is system dependent, if that length is too short 
to provide the full specification required then it is the callers responsibility to setup the 
drive and directory so that a name which meets the system requirements will be sufficient. 
I.E. this function defaults to the "current" directory.
*/

NUM SysGlue_open(LpXCONTEXT gCP, LpTHREAD gTP, char* ipPathname, NUM iMode, NUM iType)
{
    // Unused Variables
    gTP = gTP;

	BOLE            aLockSW = FALSE;
	BOLE            aResizeSW = FALSE;
	NUM				aGotOne;
	NUM             aSlot;
	NUM				aSlot2;
	NUM				aRt;
	BOLE			aIsOpen;
	QIODevice::OpenMode             aMode[2];
#ifdef _WINLOCK
	NUM             aFileNum;
	NUM             aError;
#endif
	NUM				aSleepCounter;
	int				aNumModes = 0;
	int				aTryMode;
	BOLE			aStatus;
	char			aName[FILENAME_MAX + 1]; //TM -- added back in by me.

	/* Check the arguments to make sure they are correct */
	if (strlen(ipPathname) >= FILENAME_MAX || iMode < 0 || iMode > 3 || iType < 0 || iType > 5)
		return 0;

	//TM -- added the path mangling back in. 
	int i;
	for (i=0; i<FILENAME_MAX && ipPathname[i] != '\0'; ++i)
	{
#ifdef _WIN
		if (ipPathname[i] == '/') 
			aName[i] = '\\';
		else
			aName[i] = ipPathname[i];
#else
		if (ipPathname[i] == '\\')
			aName[i]  = '/';
		else
			aName[i] = ipPathname[i];
#endif
	}
	aName[i] = '\0';


	aIsOpen = false;

	// Set the aMode[] by file type and mode. The aMode array contains
	// one or more elements that specify a possible way to open the file.
	// Usually there are only two possible modes with the objective of giving 
	// the user some level of access if full access can not be granted.
	// This nested switch also makes it easier to spot bad modes etc. The old code
	// in ansShell did not catch bad modes for the various file types.

	//TM -- this code relies on Qt for handling the differences between Windows and Linux.

	aNumModes = 0;
	switch (iType) 
	{	case 0: // Text file. TM-- note that we treat text files as binary files. We do not allow
				// Qt or the OS to do eol conversion for us.
			switch (iMode)
			{	case 0: // Open an existing file
					aMode[aNumModes++] = QIODevice::ReadWrite;		// open existing text file for read/write
					aMode[aNumModes++] = QIODevice::ReadOnly;		// open existing text file for read only
					aResizeSW = false;
					break;
				case 1: // Open a new file
					aMode[aNumModes++] = QIODevice::ReadWrite|QIODevice::Truncate;	// open new text file for read/write
					aMode[aNumModes++] = QIODevice::WriteOnly|QIODevice::Truncate;	// open new text file for write only
					aResizeSW = true;
					break;
				default:
					return 0;
			}
			break;
		case 1: // Spreadsheet binary file
		case 2: // Smarttable binary file
		case 3: // Workspace binary file
		case 4: // Object binary file
			switch (iMode) 
			{	case 0:  // Open an existing file
					aMode[aNumModes++] = QIODevice::ReadWrite;	// open existing binary file for read/write				
					aMode[aNumModes++] = QIODevice::ReadOnly;		// open existing binary file for read only
					aResizeSW = false;
					break;
				case 1:  // Open a new file
					aMode[aNumModes++] = QIODevice::ReadWrite|QIODevice::Truncate; // open new binary file for for read/write
					aMode[aNumModes++] = QIODevice::WriteOnly|QIODevice::Truncate; // open new binary file for write only
					aResizeSW = true;
					break;
				default:
					return 0;
			} // end of mode switch
			break;
		case 5: // Object Database file
			switch (iMode) 
			{	case 0:  // Open an existing database file
					aMode[aNumModes++] = QIODevice::ReadWrite;	// open existing binary file for read/write
					aResizeSW = false;
					break;
				case 1: // open an existing or new database file
					aMode[aNumModes++] = QIODevice::ReadWrite;	// open existing binary file for read/write
					aMode[aNumModes++] = QIODevice::ReadWrite|QIODevice::Truncate; // open new binary file for for read/write
					// SPECIAL MODE CASE -- must check on open to see how to set aResizeSW!!!
					break;
				case 2: // open an existing database for retrieval only
					aMode[aNumModes++] = QIODevice::ReadOnly;	// open existing binary file for read only
					aResizeSW = false;
					break;
				case 3: // Open an inserted database files
					aMode[aNumModes++] = QIODevice::ReadWrite;	// open existing binary file for read/write
					aResizeSW = false;
					break;
				default:
					return 0;
			} // end of mode switch
			break;
		default:
			return 0;
	} // end of file type switch

// Make open case-insensitive by converting name to the case used by the file system.
#ifndef _WIN
QString aNameIn(aName);
QString aNameOut((aName[0] == '/') ? "/" : "");
QDir aDir(aNameOut);
QStringList aDirList(aNameIn.split('/', QString::SkipEmptyParts));
QStringList aFileList;
int aCount = aDirList.count();
QByteArray aPathOut;
const char* apData;
for (int i = 0; i < aCount; )
{	if (aDir.exists())
	{	aFileList = aDir.entryList(QStringList(aDirList.value(i)), QDir::Dirs | QDir::Files, QDir::IgnoreCase);
		if (aFileList.count() > 0)
		{	aNameOut += aFileList.value(0);
			aDir.setPath(aNameOut);
			if (++i < aCount)
				aNameOut += '/';
			else
			{	aPathOut = aNameOut.toAscii();
				apData = aPathOut.constData();
				for (int j = 0; j < aPathOut.count(); ++j)
					aName[j] = *apData++;
			}
		}
		else
			break;
	}
	else
		break;
}
#endif // _WIN



	switch (iType) 
	{	case 0: // Text file
		case 1: // Spreadsheet binary file
		case 2: // Smarttable binary file
		case 3: // Workspace binary file
		case 4: // Object binary file
			/* find the next open file slot and lock the slot mutex so we can use the slot */
			aGotOne = 0;
			for (aRt = 0; (aRt < 2) && (aGotOne == 0); aRt++) // retries
			{ 	for (aSlot2 = 1; (aSlot2 <= gMaxFiles) ; ++aSlot2)		// see if filename is already in use
				{	
#ifdef _WIN
					if ((gOpenFiles[aSlot2] != NULL) && (_stricmp(gOpenFiles[aSlot2]->fileName().toAscii(), aName) == 0)) 
#else
					if ((gOpenFiles[aSlot2] != NULL) && (strcmp(gOpenFiles[aSlot2]->fileName().toAscii(), aName) == 0)) 
#endif
					{	sprintf(gLastError, "fopen failed - file %s is already in use by a context.", aName);
						return 0; /* Application error! No open slots */
					}				
				}
				for (aSlot = 1; (aSlot <= gMaxFiles) && (aGotOne == 0); aSlot++)	/* file slots */
				{ 	if (gOpenFiles[aSlot] == NULL)							/* see if slot is used */
					{ 	if (gFilesMutex[aSlot].tryLock())
						{	for (aSlot2 = 1; (aSlot2 <= gMaxFiles) ; ++aSlot2)		/* Make sure filename is not already in use */
							{	
#ifdef _WIN
								if ((gOpenFiles[aSlot2] != NULL) && (_stricmp(gOpenFiles[aSlot2]->fileName().toAscii(), aName) == 0)) 
#else
								if ((gOpenFiles[aSlot2] != NULL) && (strcmp(gOpenFiles[aSlot2]->fileName().toAscii(), aName) == 0)) 
#endif
								{	sprintf(gLastError, "fopen failed - file %s is already in use by a context.", aName);
									gFilesMutex[aSlot].unlock(); // TM Fixed error - this used to read gFilesMutex[aSlot2].unlock();
									return 0; /* Application error! Filename already in use */
								}
							}

							if (gOpenFiles[aSlot] == NULL) /* check if the slot is still unmodified */
							{ 	gOpenFiles[aSlot] = new QFile; /* just a place holder to keep other contexts from trying to lock this slot */
								aGotOne = 1; /* flag to exit aRt and aSlot loop */
								break;
							}
							else /* release the slot lock if someone else grabbed it before we did */
								gFilesMutex[aSlot].unlock();
						}
					}//end if (gOpenFiles[aSlot] == NULL)
				} /* aSlot */
			} /* aRt */

			if (aGotOne == 0) 
			{	sprintf(gLastError, "fopen failed with no open file slots. %d slots filled.", FOPEN_MAX+1 );
				return 0; /* Application error! No open slots */
			}

			/***************************************************/
			/* if we got here we have an lock on the file slot */
			/***************************************************/

			/*  Open the file, return if there is an error. */
			aTryMode = 0; // try aMode[0] first

			while (aTryMode < aNumModes) 
			{	aStatus = gOpenFiles[aSlot]->exists(aName); //TM -- This looks wrong. check if slot is initialized.
				if ((!aStatus) && (iMode == 0))	
					break;		/* Mode 0 specifies that only existing files should be opened. Break here if the file does not exist.  */
				gOpenFiles[aSlot]->setFileName(aName);		
				aIsOpen = gOpenFiles[aSlot]->open(aMode[aTryMode++]);

				if (aIsOpen)
					break; // got a good one!
			}

			if (!aIsOpen) // Could not open file so fail gracefully if possible
			{ 	delete gOpenFiles[aSlot];
				gOpenFiles[aSlot] = NULL;
				gFilesMutex[aSlot].unlock();
				return 0;
			}
 
			/*  Add this file to the list of open files. */
			gOpenLocks[aSlot] = aLockSW;
			gOpenTypes[aSlot] = iType;
			gOpenUsers[aSlot] = 1;
			gOpenContexts[aSlot] = (NUM)gCP;
			gFilesMutex[aSlot].unlock();

			return aSlot;

			break;
		case 5:  /* open database file */
			/* Look to see if this database file is already open.	*/
			/* Note: If so, we piggy back on this file if the open  */
			/* call came from the same context that has it open.	*/
			/* We Never open the database file twice!				*/

			/* NOTE: This whole open database strategy has a problem with the file locking
			that is performed by calling the windows locking() function. This locking function
			only works on processes. This means that the current implementation will allow
			contexts running in different instances of the engine to access the same repository
			but it will not allow two contexts running in the same instance to access the
			same repository.
			*/

			/* NOTE: The following routine has a problem. There is no check to make sure the
			fileOpen mode parameter passed is compatible with the fileOpen mode parameter passed
			when the database was first opened. This may have to be resolved.
			*/

			DatabaseOpenrt:
			for (aSlot = 1; aSlot <= gMaxFiles; aSlot++) 
			{	if ((gOpenFiles[aSlot] !=NULL ) &&  /* Open file */
					(gOpenTypes[aSlot] == 5) &&  /* Database file */
#ifdef _WIN
					(_stricmp(gOpenFiles[aSlot]->fileName().toAscii(), aName) == 0))
#else
					(strcmp(gOpenFiles[aSlot]->fileName().toAscii(), aName) == 0))
#endif
				{										
					/*  Never open database files from two different contexts.  */
					/*  Note: If a database file is open in one context, it is  */
					/*        unavailable to all other contexts. A database can */
					/*        only be open in one thread at a time.            */
					if (gOpenContexts[aSlot] != (NUM)gCP) 
					{	/* If the database file is open in another context then */
						/* aRt for a bit to see if we can get access */
						aSleepCounter = MAXIOSLEEPSBEFOREERROR;
						while ((gOpenFiles[aSlot] != NULL) && 
								(gOpenTypes[aSlot] == 5) && 
#ifdef _WIN
								(_stricmp(gOpenFiles[aSlot]->fileName().toAscii(), aName) == 0))
#else
								(strcmp(gOpenFiles[aSlot]->fileName().toAscii(), aName) == 0))
#endif
						{	--aSleepCounter;
							if (aSleepCounter <= 0) return 0;
							AContextThread* sessionMgrContextThread = (AContextThread*)gCP->SessionMgrContextThread;
							sessionMgrContextThread->localSleep(gSleepTime);	// Sleep 1000 usecs
							//Sleep(gSleepTime);
						} 
						goto DatabaseOpenrt;
					}
					else 
					{	/* since only one thread is allowed per context we do not */
						/* need to acquire a mutex to increment the open count */
						if (gFilesMutex[aSlot].tryLock()) 
						{	// Make sure no one closed the file while we were not looking!
							if ((gOpenFiles[aSlot] != NULL ) &&
								(gOpenTypes[aSlot] == 5) &&
#ifdef _WIN
								(_stricmp(gOpenFiles[aSlot]->fileName().toAscii(), aName) == 0))
#else
								(strcmp(gOpenFiles[aSlot]->fileName().toAscii(), aName) == 0))
#endif

							{	++gOpenUsers[aSlot];
								gFilesMutex[aSlot].unlock();
								return aSlot; /* We are riding piggy back, ye ha! */
							}
							else
								goto DatabaseOpenrt;
						}
						else
							goto DatabaseOpenrt;
					}
				}
			}

			/* If we got here we need to open the database */

			/* Look to see if there is an empty file slot available. */
			/* Note: If so, add this file to the list of open files. */
			/* We use the gFileOpenMutex to ensure that only one context */
			/* at a time can run through this section of code. */
			if (!WaitForMutex(gCP, &gFileOpenMutex, 1000))
				return 0;

			/* Check again that no other context has gotten in and opened the database file */
			/* This time we do not sleep between checks as this would block all contexts from */
			/* doing database file opens - a bad idea mate! */

			for (aSlot = 1; aSlot <= gMaxFiles; aSlot++) 
			{	if ((gOpenFiles[aSlot] !=NULL ) &&  /* Open file */
					(gOpenTypes[aSlot] == 5) &&  /* Database file */
#ifdef _WIN
					(_stricmp(gOpenFiles[aSlot]->fileName().toAscii(), aName) == 0))
#else
					(strcmp(gOpenFiles[aSlot]->fileName().toAscii(), aName) == 0))
#endif
				{										//TM! possible bug here with name case 
														// treatment between different OS'
				    if (gOpenContexts[aSlot] != (NUM)gCP) 
					{	gFileOpenMutex.unlock();
						return 0; /* Some other context got in and opened the db */
					}
					/* If we got here somthing is seriously wrong. It would mean that the current */
					/* context was able to open the database. This would mean there is an error */
					/* somewhere above this point in this routine. */
					gFileOpenMutex.unlock();
					return 0; /* This should never happen. TM! maybe this should be a throw? */
				}
			}

			/* find the next open file slot and lock the slot mutex so we can use the slot */
			aGotOne = 0;
			for (aRt = 0; (aRt < 2) && (aGotOne == 0); aRt++) 
			{	for (aSlot = 1; (aSlot <= gMaxFiles) && (aGotOne == 0); aSlot++) 
				{	if (gOpenFiles[aSlot] == NULL) /* try to lock this slot */
					{ 	if (WaitForMutex(gCP, &gFilesMutex[aSlot], 6000)) 
						{	if (gOpenFiles[aSlot] == NULL) /* check if the slot is still unmodified */
							{ 	gOpenFiles[aSlot] = new QFile; /* place holder until we get a real filehandle */ 
								aGotOne = 1; /* flag to exit aRt and aSlot loop */
								break;
							}
							else /* release the slot lock if someone else grabbed it before we did */
								gFilesMutex[aSlot].unlock();
						}
					}
				} /* aSlot */
			} /* aRt */

			if (aGotOne == 0) 
			{	gFileOpenMutex.unlock();
				sprintf(gLastError, "fopen failed with no open file slots. %d slots filled.", FOPEN_MAX+1 );
				return 0; /* Application error! No open slots */
			}

			/* Make sure we set the mode switches correctly. */
			if (iMode == 0) 
			{   
				/* File should exist in order to open it successfully in this mode. */
				aStatus = gOpenFiles[aSlot]->exists(aName);
				if (aStatus)
				{	/*  Open an existing database file for update and access. */			
					gOpenFiles[aSlot]->setFileName(aName);
					aIsOpen = gOpenFiles[aSlot]->open(QIODevice::ReadWrite);
				}
				if (!aIsOpen) 
				{	delete gOpenFiles[aSlot];
					gOpenFiles[aSlot] = NULL;
					gFileOpenMutex.unlock();
					gFilesMutex[aSlot].unlock();
					sprintf(gLastError, "fopen failed with invalid filename. %s.700", aName);
					return 0; /* Application error! No file of that name */
				}

				/*  Lock the database files, to provide for concurrent users. */
				#ifdef _WINLOCK
				aFileNum = gOpenFiles[aSlot]->handle();
				if (aFileNum >= 0) { //TM - debug
					aError = locking(aFileNum, _LK_NBLCK, 1);
					aSleepCounter = MAXIOSLEEPSBEFOREERROR;
					while (aError != 0) 
					{	--aSleepCounter;
						if (aSleepCounter <= 0) return 0;
						AContextThread* sessionMgrContextThread = (AContextThread*)gCP->SessionMgrContextThread;
						sessionMgrContextThread->localSleep(gSleepTime);	// Sleep 1000 usecs
						//Sleep(gSleepTime);
						aError = locking(aFileNum, _LK_NBLCK, 1);
					}
					aLockSW = TRUE;
				}
				#endif
			}
			else        
			if (iMode == 1) 
			{   /*  Create and open a database file for new. */
				//gOpenFiles[aSlot] = new QFile;
				gOpenFiles[aSlot]->setFileName(aName);
				aIsOpen = gOpenFiles[aSlot]->open(QIODevice::ReadWrite);
				if (!aIsOpen) 
				{   aIsOpen = gOpenFiles[aSlot]->open(QIODevice::ReadWrite|QIODevice::Truncate);
					if (!aIsOpen) 
					{	delete gOpenFiles[aSlot];
						gOpenFiles[aSlot] = NULL;
						gFileOpenMutex.unlock();
						gFilesMutex[aSlot].unlock();
						sprintf(gLastError, "fopen for new file failed for filename. %s.700", aName);
						return 0;  
					}
				}
				else
					aResizeSW = TRUE;            

				/*  Check for concurrent users and reopen the database file. */
				/*  Lock the database files, to provide for concurrent users. */

				#ifdef _WINLOCK
				aFileNum = gOpenFiles[aSlot]->handle();
				aError = locking(aFileNum, _LK_NBLCK, 1);
				aSleepCounter = MAXIOSLEEPSBEFOREERROR;
				while (aError != 0) 
				{	--aSleepCounter;
					if (aSleepCounter <= 0) return 0;
					AContextThread* sessionMgrContextThread = (AContextThread*)gCP->SessionMgrContextThread;
					sessionMgrContextThread->localSleep(gSleepTime);	// Sleep 1000 usecs
					//Sleep(gSleepTime);
					aError = locking(aFileNum, _LK_NBLCK, 1);
				}
				aLockSW = TRUE;
				#endif
			}
			else
			if (iMode == 2) 
			{   /*  Open an existing file for retrieval only (no update allowed). */
				//gOpenFiles[aSlot] = new QFile;
				gOpenFiles[aSlot]->setFileName(aName);
				aIsOpen = gOpenFiles[aSlot]->open(QIODevice::ReadOnly);
				if (!aIsOpen) 
				{	delete gOpenFiles[aSlot];
					gOpenFiles[aSlot] = NULL;
					gFileOpenMutex.unlock();
					gFilesMutex[aSlot].unlock();
					sprintf(gLastError, "fopen for existing file failed for filename. %s.700", aName);
					return 0;  /*TM! this should be a system error. Why is there no code for this? */
				}

				/*  Lock the database files, to provide for concurrent users. */
				#ifdef _WINLOCK
				aFileNum = gOpenFiles[aSlot]->handle();
				aError = locking(aFileNum, _LK_NBLCK, 1);
				aSleepCounter = MAXIOSLEEPSBEFOREERROR;
				while (aError != 0)
				{	--aSleepCounter;
					if (aSleepCounter <= 0) return 0;
					AContextThread* sessionMgrContextThread = (AContextThread*)gCP->SessionMgrContextThread;
					sessionMgrContextThread->localSleep(gSleepTime);	// sleep 1000 usecs.
					//Sleep(gSleepTime);
					aError = locking(aFileNum, _LK_NBLCK, 1);
				}
				aLockSW = TRUE;
				#endif
			}
			else
			if (iMode == 3) 
			{   
				/* File should exist in order to open it successfully in this mode. */
				aStatus = gOpenFiles[aSlot]->exists(aName);
				if (aStatus)
				{	/*  Open an inserted database file for update and access. */
					//gOpenFiles[aSlot] = new QFile;
					gOpenFiles[aSlot]->setFileName(aName);
					aIsOpen = gOpenFiles[aSlot]->open(QIODevice::ReadWrite);
				}
				if (!aIsOpen) 
				{	delete gOpenFiles[aSlot];
					gOpenFiles[aSlot] = NULL;
					gFileOpenMutex.unlock();
					gFilesMutex[aSlot].unlock();
					sprintf(gLastError, "fopen failed for inserted database with filename. %s.700", aName);
					return 0;  /*TM! this should be a system error. Why is there no code for this? */
				}

				/*  Lock the database files, to provide for concurrent users. */
				/*  Note: Locking strategies must be redesigned for inserted repositories. */
				#ifdef _WINLOCK
				aFileNum = gOpenFiles[aSlot]->handle();
				aError = locking(aFileNum, _LK_NBLCK, 1);
				aSleepCounter = MAXIOSLEEPSBEFOREERROR;
				while (aError != 0)
				{	--aSleepCounter;
					if (aSleepCounter <= 0) return 0;
					AContextThread* sessionMgrContextThread = (AContextThread*)gCP->SessionMgrContextThread;
					sessionMgrContextThread->localSleep(gSleepTime);	// sleep 1000 usecs.
					//Sleep(gSleepTime);
					aError = locking(aFileNum, _LK_NBLCK, 1);
				}
				aLockSW = TRUE;
				#endif
			}

			/*  Add this file to the list of open files. */
			gOpenLocks[aSlot] = aLockSW;
			gOpenTypes[aSlot] = iType;
			gOpenUsers[aSlot] = 1;
			gOpenContexts[aSlot] = (NUM)gCP;
 
			/*  Reset database size to zero. */
			if (aResizeSW) 
			{	
				//Use QFile resize
				if (!gOpenFiles[aSlot]->resize(0))
						sprintf(gLastError, "resize in database open failed");

//				#ifdef _WIN
//					/* Windows NT supports releasing file space. */
//					chsize(gOpenFiles[aSlot]->handle(), 0);
//				#else
//					/* ANSI does not support releasing file space so this is a no op */
//				#endif
			}

			gFileOpenMutex.unlock();
			gFilesMutex[aSlot].unlock();
			return aSlot;
			break;

		default:
			return 0;
	}
    return 0;
}

/*!
\brief  Sysglue_copy() is the host support for the file copy function.

\param	gCP
\param	gTP
\param  ipSourceNameIn	Filename of the source file to be copied.
\param  ipTargetNameIn	Filename of the destination file.
\return 
	-# 4-byte 0 if successful.
	-# 4-byte Negative integer code if a problem occurs.
*/

NUM SysGlue_copy(LpXCONTEXT gCP, LpTHREAD gTP, char* ipSourceNameIn, char* ipTargetNameIn) 
{
	NUM aSourceFileID;
	NUM aTargetFileID;
	bool aMore;
	int	aBytesRead;
	int	aBufLen = 8 * 1024;
	char aBuffer[8 * 1024 + 1];

	aSourceFileID = SysGlue_open(gCP, gTP, ipSourceNameIn, 0, 0);
	if (aSourceFileID == 0) return SYSGLUE_FILEOPEN_ERROR;

	aTargetFileID = SysGlue_open(gCP, gTP, ipTargetNameIn, 1, 0);
	if (aTargetFileID == 0) 
	{	SysGlue_close(gCP, gTP, aSourceFileID, 1);
		return SYSGLUE_FILEOPEN_ERROR;
	}

	aMore = true;
	while (aMore) 
	{	aBytesRead = gOpenFiles[aSourceFileID]->read(aBuffer, aBufLen);
		aMore = (aBytesRead == aBufLen);
		if (aBytesRead > 0) 
			gOpenFiles[aTargetFileID]->write(aBuffer, aBytesRead);
	}
	SysGlue_close(gCP, gTP, aSourceFileID, 1);
	SysGlue_close(gCP, gTP, aTargetFileID, 1);
	return 0;
}





