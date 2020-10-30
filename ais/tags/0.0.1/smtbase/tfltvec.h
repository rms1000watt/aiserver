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
TFltVector.h

Implementation of the Float vector CLASS which stores a variable number of
Floats in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TFltVector
#define _H_TFltVector

#include "tobject.h"
 
#define FloatArray(tval)    ((LpFLOAT)*((TFltVector*)((tval).u.Object))->itsFloatArray)
#define FltVector(tval)     ((TFltVector*)((tval).u.Object))
#define asFltVector(atval)  ((TFltVector*)(((TVAL*)atval)->u.Object))


/*  The disk structure format */

typedef struct
    {
    NUM                 itsMaxItemIndex;
    TVAL                itsCdr;         
    FLOAT               itsItemArray[1];    
    }TFltVectorOnDisk;
#define TFltVectorOnDiskPtr(h,n)    ((TFltVectorOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TFltVectorOnDisk     ((NUM)&((TFltVectorOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TFltVectorOnDiskPtr. */

#define TFltVectorOnDiskData(h,n)   (REAL*)(_HMemoryPtr(h)+n+SIZEOF_TFltVectorOnDisk)

#ifdef _SMARTBASE

/*  Declare the new type variables for this Class.   */
/*  Make sure basic types are registered in the proper order. */
/*  Note:   For speed, the FVMSCRIPT files use hard coded types (like TYNUM) */
/*          in switch statements. These hard coded basic type codes must be */
/*          coordinated with the actual dynamically registered types codes. */

/*  Function declarations */
extern TVAL			TFltVector_AddNewValue	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL			TFltVector_BSearch		(LpXCONTEXT gCP,LpTHREAD gTP,TFltVector* theSpace, REAL aReal, COMPARE* compareCode);
extern TVAL			TFltVector_Delete		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index);
extern TVAL			TFltVector_GetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern NUM			TFltVector_GetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void			TFltVector_IFltVector	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr);
extern void			TFltVector_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL			TFltVector_Insert		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue);
extern TVAL			TFltVector_MakeUnique	(LpXCONTEXT gCP,LpTHREAD gTP,TFltVector* theSpace, REAL aReal);
extern TFltVector*	TFltVector_New			(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL			TFltVector_SetCdr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL			TFltVector_SetMaxIndex	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats);
extern TVAL         TFltVector_Print		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);

/*  Method to function conversions */

extern void			TFltVector_ComputeSize	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern TObject*		TFltVector_Copy			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void			TFltVector_Doomed		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL			TFltVector_MakeNew		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern void			TFltVector_Mark			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL			TFltVector_Load			(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern TVAL			TFltVector_GetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL			TFltVector_SetIV1		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);
extern HMemory		TFltVector_Save			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory aHMemory);
#endif
#endif
