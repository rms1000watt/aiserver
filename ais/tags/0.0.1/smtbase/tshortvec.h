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
TShtVector.h

Implementation of the Short vector CLASS which stores a variable number of
short integers in a linear array. The manifest typing system together with C++ methods 
control and size of the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TShtVector
#define _H_TShtVector

#include "tobject.h"

#define ShortArray(tval)    ((LpSHORT)*((TShtVector*)((tval).u.Object))->itsShortArray)
#define ShtVector(tval)     ((TShtVector*)((tval).u.Object))
#define asShtVector(atval)  ((TShtVector*)(((TVAL*)atval)->u.Object))

/*  The disk structure format */

typedef struct
    {
    NUM                 itsMaxItemIndex;
    TVAL                itsCdr;         
    SHORT               itsItemArray[1];    
    }TShtVectorOnDisk;
#define TShtVectorOnDiskPtr(h,n)    ((TShtVectorOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TShtVectorOnDisk     ((NUM)&((TShtVectorOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TShtVectorOnDisk. */

#define TShtVectorOnDiskData(h,n)   (NUM*)(_HMemoryPtr(h)+n+SIZEOF_TShtVectorOnDisk)

#ifdef _SMARTBASE
    
/*  Declare the new type variables for this Class.   */
/*  Make sure basic types are registered in the proper order. */
/*  Note:   For speed, the FVMSCRIPT files use hard coded types (like TYNUM) */
/*          in switch statements. These hard coded basic type codes must be */
/*          coordinated with the actual dynamically registered types codes. */

/*  Function declarations */
extern  TVAL        TShtVector_AddNewValue	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern  TVAL        TShtVector_Delete		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index);
extern  TVAL        TShtVector_GetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  NUM         TShtVector_GetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void        TShtVector_IIntVector	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM argc,TVAL argv[],TVAL newCdr);
extern  void        TShtVector_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TShtVector_Insert		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index,TVAL newValue);
extern  TShtVector* TShtVector_New			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TShtVector_SetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue);
extern  TVAL        TShtVector_SetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats);
extern  TVAL        TShtVector_Print		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);

/*  Methods converted to functions  */

extern   void       TShtVector_ComputeSize	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern  TObject*    TShtVector_Copy			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void        TShtVector_Doomed		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TVAL        TShtVector_GetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1);
extern  TVAL        TShtVector_Load			(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  TVAL        TShtVector_MakeNew		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  void        TShtVector_Mark			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  HMemory     TShtVector_Save			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory anHMemory);
extern  TVAL        TShtVector_SetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue);
#endif
#endif
