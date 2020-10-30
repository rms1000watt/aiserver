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
TNumVector.h

Implementation of the Number vector CLASS which stores a variable number of
Numbers in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TNumVector
#define _H_TNumVector

#include "tobject.h"

#define RealArray(tval)     ((LpREAL)*((TNumVector*)((tval).u.Object))->itsRealArray)
#define NumVector(tval)     ((TNumVector*)((tval).u.Object))
#define asNumVector(atval)  ((TNumVector*)(((TVAL*)atval)->u.Object))


/*  The disk structure format */

typedef struct
    {
    NUM                 itsMaxItemIndex;
    TVAL                itsCdr;         
    REAL                itsItemArray[1];    
    }TNumVectorOnDisk;
#define TNumVectorOnDiskPtr(h,n)    ((TNumVectorOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TNumVectorOnDisk     ((NUM)&((TNumVectorOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TNumVectorOnDiskPtr. */

#define TNumVectorOnDiskData(h,n)   (REAL*)(_HMemoryPtr(h)+n+SIZEOF_TNumVectorOnDisk)

#ifdef _SMARTBASE
/*  Function declarations */
extern TVAL			TNumVector_AddNewValue	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL			TNumVector_BSearch		(LpXCONTEXT gCP,LpTHREAD gTP,TNumVector* theSpace, REAL aReal, COMPARE* compareCode);
extern TVAL			TNumVector_Delete		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index);
extern TVAL			TNumVector_GetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern NUM			TNumVector_GetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void			TNumVector_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL			TNumVector_Insert		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue);
extern void			TNumVector_INumVector	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr);
extern TVAL			TNumVector_MakeUnique	(LpXCONTEXT gCP,LpTHREAD gTP,TNumVector* theSpace, REAL aReal);
extern TNumVector*	TNumVector_New			(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL			TNumVector_SetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL			TNumVector_SetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats);
extern TVAL         TNumVector_Print		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);

/*  Method to function conversions */

extern void			TNumVector_ComputeSize	(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern TObject*		TNumVector_Copy			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern void			TNumVector_Doomed		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern TVAL			TNumVector_GetIV1		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL			TNumVector_Load			(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern TVAL			TNumVector_MakeNew		(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc,TVAL argv[]);
extern void			TNumVector_Mark			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern TVAL			TNumVector_SetIV1		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);
extern HMemory		TNumVector_Save			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,HMemory aHMemory);
#endif

#endif

