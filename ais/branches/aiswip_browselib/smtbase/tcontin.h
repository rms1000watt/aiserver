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
TContinuation.h

This declaration contains the storage layout for the SmartLisp Continuation
Object. A Continuation object is used to restart execution from the point it
was created. Adopted from the Scheme dialect, it is one of the main mechanisms 
for resumed execution in SmartLisp. 

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TContinuation
#define _H_TContinuation

#include "tobject.h"

/*  CLASS macro definitions */

#define asContinuation(a)           ((TContinuation*)((LpTVAL)(a))->u.Object)

/*  The disk structure format */

typedef struct
    {
    BOLE                inScope;
    TVAL                itsResult;
    }TContinuationOnDisk;
#define TContinuationOnDiskPtr(h,n) ((TContinuationOnDisk*)(_HMemoryPtr(h)+n))


#ifdef _SMARTBASE
/*  Function declarations */
extern  TVAL            TContinuation_Evaluate		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc, TVAL argv[]);
extern  void            TContinuation_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TContinuation*	TContinuation_New			(LpXCONTEXT gCP,LpTHREAD gTP);


extern  void            TContinuation_ComputeSize	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern  TVAL            TContinuation_Load			(LpXCONTEXT gCP,LpTHREAD gTP,HMemory anHMemory, NUM theFileID, NUM bResolve);
extern  void            TContinuation_Mark			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  HMemory			TContinuation_Save			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory anHMemory);
extern  TObject*        TContinuation_Copy			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);

#endif
#endif

