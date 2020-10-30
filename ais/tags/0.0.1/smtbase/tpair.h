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
TPair.h

This declaration contains the storage layout for the SmartLisp_ Pair 
Object. A Pair object holds a pair of atoms. Present in all Lisp dialects, 
it is the fundamental data structure for list processing in SmartLisp_. 

Note:   This CLASS contains the support for the C++ extensions such as: automated
        mark and sweep garbage collection, manifest types, and oodbms save and load.


PARENT:             TObject

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TPair
#define _H_TPair

#include "tobject.h"

/*  CLASS definitions */

/*  The disk structure format */

typedef struct
    {
    TVAL                itsCar;
    TVAL                itsCdr;
    }TPairOnDisk;
#define TPairOnDiskPtr(h,n) ((TPairOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TPairOnDisk  ((NUM)sizeof(TPairOnDisk))

#ifdef _SMARTBASE 
/*  CLASS type definitions */

#define asPair(a)       ((TPair*)((LpTVAL)(a))->u.Obj)
#define isPair(a)       (asTag((LpTVAL)(a)) == TYPAIR)
#define isObjPair(o)    (_VALIDOBJ(o) && ((o)->itsObjectType == TYPAIR))
#define CarOfPair(tv)   (((TPair*)(tv).u.Obj)->itsCar)
#define CdrOfPair(tv)   (((TPair*)(tv).u.Obj)->itsCdr)
    
/*  CLASS macro definitions */
    
/*  Declare all global variables for this CLASS. */

extern  BOLE        TPair_Initialized;

/*  Function declarations */
extern  TVAL        TPair_GetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TVAL        TPair_GetIndexedPair	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index);
extern  void        TPair_Init				(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void        TPair_IPair				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newCar,TVAL newCdr);
extern  TPair*      TPair_New				(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TPair_Print				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf);
extern  TVAL        TPair_PrintCont			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf);
extern  TVAL        TPair_SetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue);
/*  Method to function conversions. */

extern  void        TPair_ComputeSize		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize);
extern  TObject*    TPair_Copy				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void        TPair_Doomed			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TVAL        TPair_GetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1);
extern  TVAL        TPair_Load				(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  TVAL        TPair_Map				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc);
extern  TVAL        TPair_Mapc				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc);
extern  TVAL        TPair_PrintEllipses		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern  TVAL        TPair_PrintQuoted		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern  void        TPair_Mark				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  HMemory     TPair_Save				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory anHMemory);
extern  TVAL        TPair_SetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue);

#ifdef _C_TPAIR
#include "tlambda.h"
#include "fproc.h"
#include "fconio.h"
#include "fobject.h"
#endif
#endif
#endif
