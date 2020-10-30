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
TObjVector.h

Interface for the ObjVector CLASS which stores a variable number of items of any TObject 
descendent in a linear array. The manifest typing system together with C++ methods 
controls and size the data items and the array itself.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 

#ifndef _H_TObjVector
#define _H_TObjVector
#include "tobject.h"

#define ObjArray(tval)      ((struct TObject**)*((TObjVector*)((tval).u.Object))->itsObjectArray)
#define ObjVector(tval)     ((TObjVector*)((tval).u.Object))
#define asObjVector(atval)  ((TObjVector*)(((TVAL*)atval)->u.Object))
    
/*  The disk structure format */

typedef struct
    {
    NUM                 itsMaxItemIndex;
    TVAL                itsCdr;         
    NUM                 itsItemArray[1];    
    }TObjVectorOnDisk;
#define TObjVectorOnDiskPtr(h,n)    ((TObjVectorOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TObjVectorOnDisk     ((NUM)&((TObjVectorOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TObjVectorOnDisk. */

#define TObjVectorOnDiskData(h,n)   (NUM*)(_HMemoryPtr(h)+n+SIZEOF_TObjVectorOnDisk)


#ifdef _SMARTBASE
/*  Function declarations */

extern TVAL			TObjVector_AddNewValue		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL			TObjVector_BSearch			(LpXCONTEXT gCP,LpTHREAD gTP,TObjVector* theSpace, TObject* anObject, COMPARE* compareCode);
extern TVAL			TObjVector_Delete			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index);
extern TVAL			TObjVector_GetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern NUM			TObjVector_GetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void			TObjVector_IArray			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr);
extern void			TObjVector_Init				(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL			TObjVector_Insert			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue);
extern void			TObjVector_LoadFcn			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory aHMemory,NUM theFileID);
extern TVAL			TObjVector_MakeUnique		(LpXCONTEXT gCP,LpTHREAD gTP,TObjVector* theSpace, TObject* anObject);
extern TObjVector*	TObjVector_New				(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL			TObjVector_SetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL			TObjVector_SetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats);
extern TVAL         TObjVector_Print			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval, LpNUM size, LpCHAR buf);

/*  Method to function conversions */

extern void			TObjVector_ComputeSize		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern TObject*		TObjVector_Copy				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern void			TObjVector_Doomed			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern TVAL			TObjVector_GetIV1			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL			TObjVector_Load				(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern TVAL			TObjVector_MakeNew			(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc,TVAL argv[]);
extern void			TObjVector_Mark				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern HMemory		TObjVector_Save				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,HMemory anHMemory);
extern TVAL			TObjVector_SetIV1			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);


#ifdef _C_TOBJVECTOR
#include "tstring.h"
#include "twkspace.h"
#endif
#endif
#endif
