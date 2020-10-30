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
TIntVector.h

Implementation of the Integer vector CLASS which stores a variable number of
integers in a linear array. The manifest typing system together with C++ methods 
control and size of the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TIntVector
#define _H_TIntVector

#include "tobject.h"

#define IntArray(tval)      ((LpNUM)*((TIntVector*)((tval).u.Object))->itsIntArray)
#define IntVector(tval)     ((TIntVector*)((tval).u.Object))
#define asIntVector(atval)  ((TIntVector*)(((TVAL*)atval)->u.Object))

/*  The disk structure format */

typedef struct
    {
    NUM                 itsMaxItemIndex;
    TVAL                itsCdr;         
    NUM                 itsItemArray[1];    
    }TIntVectorOnDisk;
#define TIntVectorOnDiskPtr(h,n)    ((TIntVectorOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TIntVectorOnDisk     ((NUM)&((TIntVectorOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TIntVectorOnDisk. */

#define TIntVectorOnDiskData(h,n)   (NUM*)(_HMemoryPtr(h)+n+SIZEOF_TIntVectorOnDisk)

#ifdef _SMARTBASE
    
/*  Declare the new type variables for this Class.   */
/*  Make sure basic types are registered in the proper order. */
/*  Note:   For speed, the FVMSCRIPT files use hard coded types (like TYNUM) */
/*          in switch statements. These hard coded basic type codes must be */
/*          coordinated with the actual dynamically registered types codes. */

/*  Function declarations */
extern  TVAL        TIntVector_AddNewValue	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern  TVAL        TIntVector_Delete		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index);
extern  TVAL        TIntVector_GetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  NUM         TIntVector_GetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void        TIntVector_IIntVector	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM argc,TVAL argv[],TVAL newCdr);
extern  void        TIntVector_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TIntVector_Insert		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index,TVAL newValue);
extern  TIntVector* TIntVector_New			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TIntVector_SetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue);
extern  TVAL        TIntVector_SetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats);
extern  TVAL        TIntVector_Print		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);

/*  Methods converted to functions  */

extern   void       TIntVector_ComputeSize	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern  TObject*    TIntVector_Copy			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void        TIntVector_Doomed		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TVAL        TIntVector_GetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1);
extern  TVAL        TIntVector_Load			(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  TVAL        TIntVector_MakeNew		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  void        TIntVector_Mark			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  HMemory     TIntVector_Save			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory anHMemory);
extern  TVAL        TIntVector_SetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue);
#endif
#endif
