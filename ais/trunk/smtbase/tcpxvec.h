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
TCpxvec.h

Implementation of the Cpx vector class which stores an array of complex numbers.
Each element in the array is a pair of doubles, one for the real part and one for
the imaginary part.

PARENT:             TObject 

AUTHORS:            T. L. Williams

#endif
 
#ifndef _H_TCpxVector
#define _H_TCpxVector

#include "tobject.h"

#define CpxArray(tval)     ((LpREAL)*((TCpxVector*)((tval).u.Object))->itsCpxArray)
#define CpxVector(tval)    ((TCpxVector*)((tval).u.Object))
#define asCpxVector(atval) ((TCpxVector*)(((TVAL*)atval)->u.Object))

					//  The disk structure format
typedef struct
{	NUM		itsMaxItemIndex;
	TVAL	itsCdr;         
	REAL	itsItemArray[1];    
}TCpxVectorOnDisk;

#define SIZEOF_TCpxVectorOnDisk     ((NUM)&((TCpxVectorOnDisk*)0)->itsItemArray)

#ifdef _SMARTBASE
//  Function declarations
extern void     TCpxVector_Init(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL     TCpxVector_AddNewValue(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL     TCpxVector_Compare(LpXCONTEXT gCP, LpTHREAD gTP,TVAL left, TVAL right);
extern void     TCpxVector_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern TObject*	TCpxVector_Copy(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern TVAL		TCpxVector_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL tval,TVAL index);
extern void     TCpxVector_Doomed(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern TVAL		TCpxVector_GetCdr(LpXCONTEXT gCP, LpTHREAD gTP, TVAL tval);
extern TVAL     TCpxVector_GetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern NUM      TCpxVector_GetMaxIndex(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval);
extern TVAL     TCpxVector_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue);
extern TVAL     TCpxVector_Load(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern TVAL     TCpxVector_MakeNew(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc,TVAL argv[]);
extern void     TCpxVector_Mark(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval);
extern TCpxVector* TCpxVector_New(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL     TCpxVector_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);
extern HMemory  TCpxVector_Save(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, HMemory aHMemory);
extern TVAL		TCpxVector_SetCdr(LpXCONTEXT gCP, LpTHREAD gTP, TVAL tval, TVAL newCdr);
extern TVAL     TCpxVector_SetIV1(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue);
extern TVAL     TCpxVector_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP, TVAL selfTval, NUM newRepeats);

#endif	//	_SMARTBASE
#endif	//	_H_TCpxVector 

