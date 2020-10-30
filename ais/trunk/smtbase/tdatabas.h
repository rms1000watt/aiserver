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

#if 0
TDatabase.h

Declaration of the Database CLASS which manages the General Object Repository. 
The General Object Repository supports the association of persistent objects
of an arbitrary nature with a key value of an arbitrary nature..


PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TDatabase
#define _H_TDatabase

#include "tsymbol.h"

/*  Declare all macros for this Class. */
#define Database(tval)	((TDatabase*)(tval).u.Object)
#define asDatabase(a)	((TDatabase*)asObject(a))
#define COMFORTABLEBTREENODESIZE	1000

/*  Function declarations */
extern void			TDatabase_Init					(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL         TDatabase_Length				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TDatabase*	TDatabase_New					(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL			TDatabase_SaveBlock				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theBlock,REAL overFlow,LpNUM blockSize);
extern TDatabase*	TDatabase_UniqueObjectRepository(LpXCONTEXT gCP,LpTHREAD gTP,TDatabase* objRepo);

/*  Method to function conversions */

extern TVAL			TDatabase_AbortTransaction		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL			TDatabase_IsTransaction			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL			TDatabase_BeginTransaction		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL			TDatabase_Clear					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL			TDatabase_CommitTransaction		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL			TDatabase_CheckPointTransaction	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern void         TDatabase_ComputeSize			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern TObject*     TDatabase_Copy					(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void         TDatabase_Doomed				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL         TDatabase_GetIV1				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL         TDatabase_GetIV2				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2);
extern TVAL         TDatabase_GetIV3				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL index3);
extern TVAL         TDatabase_Inspect				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL			TDatabase_isImmediate			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL			TDatabase_Load					(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern TVAL			TDatabase_LoadRepository		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL         TDatabase_MakeObjectRepository	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern void         TDatabase_Mark					(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL			TDatabase_Rename				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern HMemory      TDatabase_Save					(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory anHMemory);
extern TVAL			TDatabase_SaveImmediate			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL			TDatabase_SaveRepository		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL         TDatabase_SetIV1				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);
extern TVAL         TDatabase_SetIV2				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1, TVAL index2,TVAL newValue);
extern TVAL         TDatabase_SetIV3				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1, TVAL index2, TVAL index3,TVAL newValue);
extern TVAL			TDatabase_SizeOf				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);	
extern TVAL         TDatabase_Delete				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);

extern TVAL         TDatabase_IndexedDiskErase		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TDirectory* nodeIndex);
extern TVAL         TDatabase_IndexedDiskLoad		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL         TDatabase_IndexedDiskSave		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);
extern TVAL         TDatabase_PositionIndex			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL         TDatabase_RefIndex				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL         TDatabase_RefIndexKey			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL         TDatabase_SetInIndexOnly		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1, TVAL newValue);


/*  The disk structure formats */

typedef struct
    {
    NUM             itsBufferCount;         /*  Count of buffered items in ObjectRepository. */
    TVAL            itsCodeKey;             /*  Numeric key for encryption, true for compression. */
	TVAL			itsBaseFilePos;			/*  Starting position in the database file. */
    CHAR            itsFileName[256];       /*  File name of the Database archive disk file. */
   } TDatabaseOnDisk;
#define TDatabaseOnDiskPtr(h,n)  ((TDatabaseOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TDatabaseOnDisk   ((NUM)sizeof(TDatabaseOnDisk))

#endif

