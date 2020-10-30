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
TVector.h

Interface for the vector CLASS which stores a variable number of items of any
type in a linear vector. The manifest typing system is used to control and size the
data items and the array itself.

PARENT:             indirect 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TVector
#define _H_TVector

#include    "tobject.h"
#include    "tobjvec.h"

/*  Class declaration */

#define TvalArray(tval)     ((LpTVAL)*((TVector*)((tval).u.Object))->itsTvalArray)
#define Vector(tval)        ((TVector*)((tval).u.Object))
#define asVector(a)         ((TVector*)((LpTVAL)(a))->u.Obj)
    

typedef struct
    {
    NUM                 itsMaxItemIndex;
    NUM                 itsAttributes;
    TVAL	            itsCdr; 
    TVAL                itsItemArray[1]; 
    }TVectorOnDisk;
#define TVectorOnDiskPtr(h,n)   ((TVectorOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TVectorOnDisk    ((NUM)&((TVectorOnDisk*)0)->itsCdrAndItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TVectorOnDiskPtr. */

#define TVectorOnDiskData(h,n)  (TVAL*)(_HMemoryPtr(h)+n+SIZEOF_TVectorOnDisk)

#ifdef _SMARTBASE
/*  Function declarations */
extern  TVAL        TVector_AddNewValue		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL newValue);
extern  TVAL        TVector_BSearch			(LpXCONTEXT gCP,LpTHREAD gTP,TVector* theSpace, NUM start, NUM len, TVAL newData, COMPARE* compareCode);
extern  TVAL        TVector_Delete			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index);
extern  TVAL        TVector_GetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  NUM         TVector_GetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  void        TVector_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TVector_Insert			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index,TVAL newValue);
extern  TVAL        TVector_IVector			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval,NUM argc,TVAL argv[],TVAL newCdr);
extern  TVAL        TVector_MakeUnique		(LpXCONTEXT gCP,LpTHREAD gTP,TVector* theSpace, NUM start, NUM len, TVAL newData);
extern  TVector*    TVector_New				(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TVector_Print			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);
extern  TVAL        TVector_SetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, NUM newRepeats);
extern  TVAL        TVector_SetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL newValue);

/*  Method to function conversions */

extern  void        TVector_ComputeSize		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval,NUM* aSize);
extern  TObject*    TVector_Copy			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  void        TVector_Doomed			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  TVAL        TVector_GetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1);
extern  TVAL        TVector_Load			(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  void        TVector_Mark			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  TVAL        TVector_Map				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL proc);
extern  TVAL        TVector_Mapc			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL proc);
extern  HMemory     TVector_Save			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, HMemory aHMemory);
extern  TVAL        TVector_SetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1,TVAL newValue);
/*  The disk structure format */
#endif
#endif
