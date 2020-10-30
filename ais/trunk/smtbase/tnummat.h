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
Tnummat.h

Interface for the number matrix CLASS which stores a variable number of items of number
type in a rank dimensioned matrix. The manifest typing system is used to control 
and size the data items and the matrix itself.

PARENT:             indirect 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TNumMatrix
#define _H_TNumMatrix

#include    "tobject.h"

/*  Class declaration */

#define RealMatrix(tval)	    ((LpREAL)*((TNumMatrix*)((tval).u.Object))->itsRealMatrix)
#define NumMatrix(tval)         ((TNumMatrix*)((tval).u.Object))
#define asNumMatrix(atval)      ((TNumMatrix*)(((TVAL*)atval)->u.Object))
#define NumMatrixFromRow(tval)  ((TNumMatrix*)_TObject_MainObjectList(tval.u.Index[0]))

typedef struct
    {
    NUM                 itsMaxItemIndex;
    TVAL                itsCdr;         
	NUM                 itsRank;
	NUM                 itsDimensions[_MAXDIMENSIONS];
    REAL                itsItemArray[1];    
    }TNumMatrixOnDisk;
#define TNumMatrixOnDiskPtr(h,n)   ((TNumMatrixOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TNumMatrixOnDisk    ((NUM)&((TNumMatrixOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TNumMatrixOnDiskPtr. */

#define TNumMatrixOnDiskData(h,n)  (TVAL*)(_HMemoryPtr(h)+n+SIZEOF_TNumMatrixOnDisk)

typedef struct
    {
    NUM                 itsNumMatrixObj;
    NUM                 itsRowIdx;       
	NUM                 itsFldIdx;
    REAL                itsItemArray[1];    
    }TNumMatrixRowOnDisk;
#define TNumMatrixRowOnDiskPtr(h,n)     ((TNumMatrixRowOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TNumMatrixRowOnDisk      ((NUM)&((TNumMatrixRowOnDisk*)0)->itsItemArray)

#ifdef _SMARTBASE
/*  Function declarations */
extern  TVAL        TNumMatrix_AddNewValue		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL newValue);
extern  TVAL        TNumMatrix_Delete			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index);
extern  TVAL        TNumMatrix_GetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  NUM         TNumMatrix_GetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  void        TNumMatrix_Init				(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TNumMatrix_Insert			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index,TVAL newValue);
extern  TVAL        TNumMatrix_IMatrix			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval,NUM argc,REAL argv[],TVAL newCdr);
extern  TNumMatrix* TNumMatrix_New				(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TNumMatrix_Print			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);
extern  TVAL        TNumMatrix_SetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, NUM newRepeats);
extern  TVAL        TNumMatrix_SetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL newValue);

/*  Method to function conversions */

extern  void        TNumMatrix_ComputeSize		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval,NUM* aSize);
extern  TObject*    TNumMatrix_Copy				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  void        TNumMatrix_Doomed			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  TVAL        TNumMatrix_GetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1);
extern  TVAL        TNumMatrix_GetIV2			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1, TVAL index2);
extern  TVAL        TNumMatrix_GetIV3			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1, TVAL index2, TVAL index3);
extern  TVAL        TNumMatrix_Load				(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  void        TNumMatrix_Mark				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  TVAL        TNumMatrix_Map				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL proc);
extern  TVAL        TNumMatrix_Mapc				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL proc);
extern  HMemory     TNumMatrix_Save				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, HMemory aHMemory);
extern  TVAL        TNumMatrix_SetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1,TVAL newValue);
extern  TVAL        TNumMatrix_SetIV2			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1, TVAL index2,TVAL newValue);
extern  TVAL        TNumMatrix_SetIV3			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1, TVAL index2, TVAL index3,TVAL newValue);
extern  TVAL        TNumMatrix_Resize			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

extern  void        TNumMatrixRow_ComputeSize   (LpXCONTEXT gCP, LpTHREAD gTP, TVAL aTval, NUM* aSize);
extern  TVAL        TNumMatrixRow_GetIV1        (LpXCONTEXT gCP, LpTHREAD gTP, TVAL aTval, TVAL index1);
extern  TVAL        TNumMatrixRow_SetIV1        (LpXCONTEXT gCP, LpTHREAD gTP, TVAL aTval, TVAL index1, TVAL newValue);
extern  TVAL        TNumMatrixRow_Print         (LpXCONTEXT gCP, LpTHREAD gTP, TVAL aTval, LpNUM size, LpCHAR buf);

#endif
#endif
