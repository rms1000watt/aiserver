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
aisdev/glue/asysglue.h
									Glue
This source file contains the functions which manage low level system
IO for the SmartBase engine. 

CHANGE HISTORY
Version	Date		Who		Change
2.0000	11/25/2007	tim		Modified API to allow large file support
1.0102	9/04/2006	rmf		Update embedded documentation
1.0000	1/17/2002	tim		SysGlue derived from AnsShell
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

/*  Make sure that we skip this include file if it has already been seen.   */
/*  Also, make sure that all common variables are declared "extern" if      */
/*  the common keyword has not explicitly been declared.                    */

#ifndef _H_SysGlue
#define _H_SysGlue

/*  Make sure that we import the SmartBase API symbols correctly if we are  */
/*  building from a DLL version of SmartBase in Windows or a shared library */
/*  version of SmartBase in Unix.                                           */

extern "C" { // includes for modules written in C
	#include "fsmtbase.h" // SmartBase engine declarations
	}

#include <ctype.h>
#include <time.h>

//!\brief Returned if the user does not have the required security level to make the request.
#define		SYSGLUESECLEVEL				1
#define     SYSGLUE_CANTLAUNCH       20000
//!\brief Returned if an error occurs when reading from a file.
#define     SYSGLUE_FILEREAD         20001
//!\brief Returned if an error occurs when writing to a file.
#define     SYSGLUE_FILEWRITE        20002
#define     SYSGLUE_TOOMANYFILES     20003
#define     SYSGLUE_CANTLOCKFILE     20004
#define     SYSGLUE_CANTUNLOCKFILE   20005
/*!
\brief Returned if the file ID specified is invalid.
\par Note:
Valid file ID values range from 1 until gMaxFiles.
*/
#define		SYSGLUE_BADFILEID		 20006

//!\brief Returned if the attempt to grab mutex lock is unsuccessful.
#define		SYSGLUE_MUTEXLOCKSET	 -101
//!\brief Returned if the attempt to release mutex lock is unsuccessful.
#define		SYSGLUE_MUTEXLOCKRELEASE -102
//!\brief Returned if the length to be written to the file is less than 0.
#define		SYSGLUE_BAD_WRITE_LEN	 -103
//!\brief Returned if an operation is attempted on a file that is closed.
#define		SYSGLUE_FILE_NOT_OPEN	 -104
//!\brief Returned if a file belonging to a context was not closed successfully.
#define		SYSGLUE_BAD_CONTEXT_WIDE_FILE_CLOSE	-105
//!\brief Returned if the file location to seek from is not valid.
#define		SYSGLUE_BAD_SEEK_LOCATION -106
//!\brief Returned if the new file size specified during resize() is less than 0.
#define		SYSGLUE_BAD_FILE_SIZE	 -107
//!\brief Returned if either the source file or destination file during copy() was not opened successfully.
#define		SYSGLUE_FILEOPEN_ERROR	 -108

//!\brief Defines the string size for the last error message.
#define		FILE_ERROR_STRING_SIZE	1024


/* Functions not registered as callbacks to the engine */
extern NUM SysGlue_IOInit (void);

/* Callback functions registered with the engine in the Funcs structure. All registered
   functions preserve gCP and gTP.  */
extern NUM  SysGlue_open			(LpXCONTEXT gCP,LpTHREAD gTP,char* name, NUM mode, NUM type);
extern NUM  SysGlue_read			(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, NUM len, char* dataPtr);
extern NUM  SysGlue_write			(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, NUM len, const char* dataPtr);
extern REAL  SysGlue_seek			(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, REAL offset, NUM code);
extern REAL  SysGlue_resize			(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, REAL newsize);
extern NUM  SysGlue_close			(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, NUM opcode);

/* Functions for use in asbglue.cpp. Not registed with engine directly. */
extern NUM SysGlue_lastError		(LpXCONTEXT gCP,LpTHREAD gTP, char* dataPtr);
extern NUM SysGlue_showFileSlot		(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, char* dataPtr);
extern NUM SysGlue_copy				(LpXCONTEXT gCP,LpTHREAD gTP,char *sourceName, char *targetName);

#endif
