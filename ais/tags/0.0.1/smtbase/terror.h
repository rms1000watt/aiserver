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
TError.h

Interface for the error CLASS which stores a K&R C string in a variable length
object.

Note:   Duplicate TError objects containing the same K&R C string are eliminated
        by the TError constructors. Therefore, TError object may be compared for
        equality on the basis of the object handles alone.
        
PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TError
#define _H_TError

#include "tobject.h"

/*  Misc defines */

#define ErrorArray(tval)    ((LpCHAR)*((TError*)((tval).u.Object))->itsCString)
#define Error(tval)         ((TError*)((tval).u.Object))
//#define asError(atval)      ((TError*)((LpTVAL)(atval))->u.Obj)

/*  Disk Format */

typedef struct
    {
    NUM             itsMaxItemIndex;
    char            itsStringData[1];
    }TErrorOnDisk;

#define TErrorOnDiskPtr(h,n)   ((TErrorOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TErrorOnDisk    ((NUM)&((TErrorOnDisk*)0)->itsStringData)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TErrorOnDiskPtr. */

#define TErrorOnDiskData(h,n)  (char*)(_HMemoryPtr(h)+n+SIZEOF_TErrorOnDisk)
    

#ifdef _SMARTBASE
/*  Declare the new type variables for this Class.   */
/*  Make sure basic types are registered in the proper order. */
/*  Note:   For speed, the FVMSCRIPT files use hard coded types (like TYNUM) */
/*          in switch statements. These hard coded basic type codes must be */
/*          coordinated with the actual dynamically registered types codes. */
extern  TYPE        TySTRING;

/*  Function declarations */
extern  NUM         TError_GetMaxIndex			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TVAL        TError_GetNumericValue		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  LpCHAR      TError_GetStringPtr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void        TError_Init					(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TError*     TError_MakeUnique			(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString);
extern  TError*     TError_New					(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TError_SetMaxIndex			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats);
extern  TVAL		TError_sprintf				(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR dest,LpCHAR fmt, ...);
extern  TError*     TError_SubString_MakeUnique	(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString, NUM firstChar, NUM theLen);

/*  Method to function converions */

extern  void        TError_ComputeSize			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize);
extern  TObject*    TError_Copy					(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void        TError_Doomed				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TVAL        TError_GetIV1				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern  TVAL        TError_SetIV1				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);
extern  TVAL        TError_Load					(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  TVAL		TError_MakeError			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL        TError_Map					(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc);
extern  TVAL        TError_Mapc					(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc);
extern  TVAL        TError_Print				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf);
extern  HMemory     TError_Save					(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory aHMemory);
extern  TVAL        TError_StringAnyCmp			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL        TError_StringAnyCnv			(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL        TError_TvalAnyCnv			(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget,TVAL oldValue);
#endif

#endif

