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
TLongVector.h

Implementation of the Long vector CLASS which stores a variable number of
Long integers in a linear array. The manifest typing system together with C++ methods 
control and size of the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TLongVector
#define _H_TLongVector

#include "tobject.h"

#define LongArray(tval)		((LpNUM32)*((TLongVector*)((tval).u.Object))->itsLongArray)
#define LongVector(tval)	((TLongVector*)((tval).u.Object))
#define asLongVector(atval) ((TLongVector*)(((TVAL*)atval)->u.Object))

/*  The disk structure format */

typedef struct
    {
    NUM                 itsMaxItemIndex;
    TVAL                itsCdr;         
    NUM32               itsItemArray[1];    
    }TLongVectorOnDisk;
#define TLongVectorOnDiskPtr(h,n)   ((TLongVectorOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TLongVectorOnDisk    ((NUM)&((TLongVectorOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the			*/
/*  variable portion of the data and casts it to an appropriate type.	*/
/*  The arguments will be the same as for TLongVectorOnDisk.			*/

#define TLongVectorOnDiskData(h,n)   (NUM*)(_HMemoryPtr(h)+n+SIZEOF_TLongVectorOnDisk)

#ifdef _SMARTBASE
    
/*  Declare the new type variables for this Class.   */
/*  Make sure basic types are registered in the proper order. */
/*  Note:   For speed, the FVMSCRIPT files use hard coded types (like TYNUM) */
/*          in switch statements. These hard coded basic type codes must be */
/*          coordinated with the actual dynamically registered types codes. */

/*  Function declarations */
extern  TVAL        TLongVector_AddNewValue	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern  TVAL        TLongVector_Delete		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index);
extern  TVAL        TLongVector_GetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  NUM         TLongVector_GetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void        TLongVector_IIntVector	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM argc,TVAL argv[],TVAL newCdr);
extern  void        TLongVector_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TLongVector_Insert		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index,TVAL newValue);
extern  TLongVector* TLongVector_New		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TLongVector_SetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue);
extern  TVAL        TLongVector_SetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats);
extern  TVAL        TLongVector_Print		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);

/*  Methods converted to functions  */

extern   void       TLongVector_ComputeSize	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern  TObject*    TLongVector_Copy		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void        TLongVector_Doomed		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TVAL        TLongVector_GetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1);
extern  TVAL        TLongVector_Load		(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  TVAL        TLongVector_MakeNew		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  void        TLongVector_Mark		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  HMemory     TLongVector_Save		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory anHMemory);
extern  TVAL        TLongVector_SetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue);
#endif
#endif
