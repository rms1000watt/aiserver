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
TDirectory.h 

Implementation of the Directory class which stores a variable length set of 
(key,value) pairs stored as a linear array. It is one of the main mechanisms 
for variable bonding in SmartBase.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif
 
#ifndef _H_TDirectory
#define _H_TDirectory

#include "tsymbol.h"
#include "tstruct.h"

/*  Class definitions */

#define PBindArray(tval)   ((LpPBIND)*((TDirectory*)((tval).u.Object))->itsDirectoryArray)
#define Directory(tval)    ((TDirectory*)((tval).u.Object))
#define asDirectory(a)     ((TDirectory*)((LpTVAL)(a))->u.Obj)

/*  Class macro definitions */
#define _TDirectory_KeyIndex   0
#define _TDirectory_ValueIndex 1
    
typedef struct
    {
    TVAL                Key;        
    TVAL	            Value; 
    }PBINDONDISK;

typedef struct
    {
    NUM                 itsMaxItemIndex;        
    TVAL		        itsCdr; 
    PBINDONDISK         itsItemArray[1]; 
    }TDirectoryOnDisk;


#define TDirectoryOnDiskPtr(h,n)   ((TDirectoryOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TDirectoryOnDisk    ((NUM)&((TDirectoryOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TDirectoryOnDisk. */

#define TDirectoryOnDiskData(h,n)  (PBINDONDISK*)(_HMemoryPtr(h)+n+SIZEOF_TDirectoryOnDisk)


#ifdef _SMARTBASE
/*  Function declarations */

extern TVAL         TDirectory_AddNewValue		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newKey,TVAL newValue);
extern TVAL         TDirectory_BSearchKey		(LpXCONTEXT gCP,LpTHREAD gTP,TDirectory* theSpace, TVAL theKey, COMPARE* compareCode);
extern TVAL         TDirectory_Delete			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index);
extern NUM          TDirectory_GetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL         TDirectory_IDirectory		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[]);
extern void         TDirectory_Init				(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL         TDirectory_Insert			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index, TVAL newObject, TVAL newValue);
extern TVAL         TDirectory_MakeUniqueKey	(LpXCONTEXT gCP,LpTHREAD gTP,TDirectory* theSpace, TVAL theKey, TVAL keyData);
extern TDirectory*  TDirectory_New				(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL         TDirectory_Print			(LpXCONTEXT gCP,LpTHREAD gTP,TDirectory* theEnv, LpNUM size, LpCHAR buf);
extern TVAL         TDirectory_SetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats);

extern void         TDirectory_ComputeSize		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize);
extern TObject*     TDirectory_Copy				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void         TDirectory_Doomed			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL         TDirectory_Load				(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern TVAL         TDirectory_MakeNew			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern void         TDirectory_Mark				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL         TDirectory_GetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1);
extern TVAL         TDirectory_GetIV2			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL index2);
extern TVAL         TDirectory_SetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue);
extern TVAL         TDirectory_SetIV2			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL index2, TVAL newValue);
extern TVAL         TDirectory_Map				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc);
extern TVAL         TDirectory_Mapc				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc);
extern HMemory      TDirectory_Save				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory anHMemory);
#endif
#endif

