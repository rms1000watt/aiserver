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
TByteVector.h

Implementation of the Byte vector class which stores a variable number of
bytes in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TByteVector
#define _H_TByteVector

#include "tobject.h"

#define ByteArray(tval)     ((LpCHAR)*((TByteVector*)((tval).u.Object))->itsByteArray)
#define ByteVector(tval)    ((TByteVector*)((tval).u.Object))
#define asByteVector(atval) ((TByteVector*)(((TVAL*)atval)->u.Object))


/*  The disk structure format */

typedef struct
    {
    NUM                 itsMaxItemIndex;
    TVAL                itsCdr;         
    CHAR                itsItemArray[1];    
    }TByteVectorOnDisk;
#define TByteVectorOnDiskPtr(h,n)   ((TByteVectorOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TByteVectorOnDisk    ((NUM)&((TByteVectorOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TByteVectorOnDisk. */

#define TByteVectorOnDiskData(h,n)  (CHAR*)(_HMemoryPtr(h)+n+SIZEOF_TByteVectorOnDisk)


#ifdef _SMARTBASE
/*  Declare the new type variables for this Class.   */
/*  Make sure basic types are registered in the proper order. */
/*  Note:   For speed, the FVMSCRIPT files use hard coded types (like TYNUM) */
/*          in switch statements. These hard coded basic type codes must be */
/*          coordinated with the actual dynamically registered types codes. */

/*  Function declarations */
extern TVAL			TByteVector_AddNewValue	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL         TByteVector_Delete		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index);
extern TVAL         TByteVector_GetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern NUM          TByteVector_GetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void         TByteVector_IByteVector	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr);
extern void         TByteVector_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL         TByteVector_Insert		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue);
extern TByteVector* TByteVector_MakeUnique	(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString, NUM firstChar, NUM theLen);
extern TByteVector* TByteVector_New			(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL         TByteVector_SetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL         TByteVector_SetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats);

/*  Methods converted to functions  */

extern TVAL         TByteVector_Print		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf);
extern void         TByteVector_ComputeSize	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern TObject*     TByteVector_Copy		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void         TByteVector_Doomed		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL         TByteVector_Load		(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern void         TByteVector_Mark		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern HMemory      TByteVector_Save		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory aHMemory);
extern TVAL         TByteVector_GetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL         TByteVector_SetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);
extern TVAL         TByteVector_MakeNew		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
#endif

#endif

