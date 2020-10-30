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
TMatrix.h

Interface for the matrix CLASS which stores a variable number of items of any
type in a rank dimensioned matrix. The manifest typing system is used to control 
and size the data items and the matrix itself.

PARENT:             indirect 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TMatrix
#define _H_TMatrix

#include    "tobject.h"

/*  Class declaration */

#define TvalMatrix(tval)    ((LpTVAL)*((TMatrix*)((tval).u.Object))->itsTvalMatrix)
#define Matrix(tval)        ((TMatrix*)((tval).u.Object))
#define asMatrix(a)         ((TMatrix*)((LpTVAL)(a))->u.Obj)
#define MatrixFromRow(tval) ((TMatrix*)_TObject_MainObjectList(tval.u.Index[0]))


typedef struct
    {
    NUM                 itsMaxItemIndex;
	NUM                 itsRank;
	NUM                 itsDimensions[_MAXDIMENSIONS];
    TVAL	            itsCdr; 
    TVAL                itsItemArray[1];
    }TMatrixOnDisk;
#define TMatrixOnDiskPtr(h,n)   ((TMatrixOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TMatrixOnDisk    ((NUM)&((TMatrixOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TMatrixOnDiskPtr. */

#define TMatrixOnDiskData(h,n)  (TVAL*)(_HMemoryPtr(h)+n+SIZEOF_TMatrixOnDisk)

typedef struct
    {
    NUM                 itsMatrixObj;
	NUM                 itsRowIdx;
	NUM                 itsFldIdx;
	TVAL                itsItemArray[1];
    }TMatrixRowOnDisk;
#define TMatrixRowOnDiskPtr(h,n)    ((TMatrixRowOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TMatrixRowOnDisk     ((NUM)&((TMatrixRowOnDisk*)0)->itsItemArray)

#ifdef _SMARTBASE
/*  Function declarations */
extern  TVAL        TMatrix_AddNewValue		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL newValue);
extern  TVAL        TMatrix_Delete			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index);
extern  TVAL        TMatrix_GetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  NUM         TMatrix_GetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  void        TMatrix_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TMatrix_Insert			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index,TVAL newValue);
extern  TVAL        TMatrix_IMatrix			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval,NUM argc,TVAL argv[],TVAL newCdr);
extern  TMatrix*    TMatrix_New				(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TMatrix_Print			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);
extern  TVAL        TMatrix_SetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, NUM newRepeats);
extern  TVAL        TMatrix_SetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL newValue);

/*  Method to function conversions */

extern  void        TMatrix_ComputeSize		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval,NUM* aSize);
extern  TObject*    TMatrix_Copy			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  void        TMatrix_Doomed			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  TVAL        TMatrix_GetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1);
extern  TVAL        TMatrix_GetIV2			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1, TVAL index2);
extern  TVAL        TMatrix_GetIV3			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1, TVAL index2, TVAL index3);
extern  TVAL        TMatrix_Load			(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  void        TMatrix_Mark			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  TVAL        TMatrix_Map				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL proc);
extern  TVAL        TMatrix_Mapc			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL proc);
extern  HMemory     TMatrix_Save			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, HMemory aHMemory);
extern  TVAL        TMatrix_SetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1,TVAL newValue);
extern  TVAL        TMatrix_SetIV2			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1, TVAL index2,TVAL newValue);
extern  TVAL        TMatrix_SetIV3			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, TVAL index1, TVAL index2, TVAL index3,TVAL newValue);
extern  TVAL        TMatrix_Resize			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL        TMatrix_Rank			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

extern  void        TMatrixRow_ComputeSize  (LpXCONTEXT gCP, LpTHREAD gTP, TVAL aTval, NUM* aSize);
extern  TVAL        TMatrixRow_GetIV1       (LpXCONTEXT gCP, LpTHREAD gTP, TVAL aTval, TVAL index1);
extern  TVAL        TMatrixRow_Print        (LpXCONTEXT gCP, LpTHREAD gTP, TVAL aTval, LpNUM size, LpCHAR buf);
extern  TVAL        TMatrixRow_SetIV1       (LpXCONTEXT gCP, LpTHREAD gTP, TVAL aTval, TVAL index1, TVAL newValue);

#endif
#endif
