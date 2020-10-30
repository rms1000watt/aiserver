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
TBitVector.h

Implementation of the Bit vector CLASS which stores a variable number of
bits in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TBitVector
#define _H_TBitVector

#include "tobject.h"

#define BitArray(tval)      ((LpCHAR)*((TBitVector*)((tval).u.Object))->itsBitArray)
#define BitVector(tval)     ((TBitVector*)((tval).u.Object))
#define asBitVector(atval)  ((TBitVector*)(((TVAL*)atval)->u.Object))
    
/*  The disk structure format */

typedef struct
    {
    NUM                 itsMaxItemIndex;
    TVAL                itsCdr;         
    CHAR                itsItemArray[1];    
    }TBitVectorOnDisk;
#define TBitVectorOnDiskPtr(h,n)    ((TBitVectorOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TBitVectorOnDisk     ((NUM)&((TBitVectorOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TBitVectorOnDisk. */

#define TBitVectorOnDiskData(h,n)   (CHAR*)(_HMemoryPtr(h)+n+SIZEOF_TBitVectorOnDisk)


#ifdef _SMARTBASE
/*  Function declarations */
extern TVAL			TBitVector_AddNewValue		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL			TBitVector_Delete			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index);
extern void			TBitVector_Functions_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL			TBitVector_GetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern NUM			TBitVector_GetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void			TBitVector_IBitVector		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr);
extern void			TBitVector_Init				(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL			TBitVector_Insert			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue);
extern TBitVector*	TBitVector_New				(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL			TBitVector_SetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL			TBitVector_SetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats);
extern TVAL         TBitVector_Print			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);

/*  Methods converted to functions  */

extern void     TBitVector_ComputeSize			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern TObject* TBitVector_Copy					(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void     TBitVector_Doomed				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL		TBitVector_Load					(LpXCONTEXT gCP,LpTHREAD gTP,HMemory anHMemory, NUM theFileID, NUM bResolve);
extern TVAL     TBitVector_FindBlock			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL     TBitVector_FreeBlock			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL     TBitVector_GetIV1				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL     TBitVector_MakeNew				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern void     TBitVector_Mark					(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL     TBitVector_SetIV1				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);
extern HMemory  TBitVector_Save					(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory anHMemory);
extern TVAL     TBitVector_SetBlock				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL     TBitVector_BitToNumberVector	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL     TBitVector_BitToIntegerVector	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#endif

#endif

