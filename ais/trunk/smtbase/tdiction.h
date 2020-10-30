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
TDictionary.h 

Interface for the Dictionary CLASS which demonstrates the protocols required for the
OODBMS CLASS hierarchy. TDictionary is a descendent of TObject.

Note:   This CLASS contains the support for the C++ extensions such as: automated
        mark and sweep garbage collection, manifest types, and oodbms save and load.


PARENT:             TObject

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TDictionary
#define _H_TDictionary

#include "tsymbol.h"
#include "tstruct.h"

/*  Class definitions */

#define BondArray(tval)     ((LpBIND)*((TDictionary*)((tval).u.Object))->itsDictionaryArray)
#define Dictionary(tval)    ((TDictionary*)((tval).u.Object))
#define asDictionary(a)     ((TDictionary*)((LpTVAL)(a))->u.Obj)


/*  The disk structure format */

typedef struct
    {
    NUM                 itsMaxItemIndex;        
    TVAL	            itsCdr; 
    BINDONDISK	        itsItemArray[0]; 
    }TDictionaryOnDisk;


#define TDictionaryOnDiskPtr(h,n)   ((TDictionaryOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TDictionaryOnDisk    ((NUM)&((TDictionaryOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TDictionaryOnDisk. */

#define TDictionaryOnDiskData(h,n)  (BINDONDISK*)(_HMemoryPtr(h)+n+SIZEOF_TDictionaryOnDisk)


#ifdef _SMARTBASE
/*  Class macro definitions */
#define _TDictionary_KeyIndex   0
#define _TDictionary_ValueIndex 1
    
/*  Function declarations */
extern TVAL         TDictionary_AddNewValue		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newKey,TVAL newValue);
extern TVAL			TDictionary_BSearchKey		(LpXCONTEXT gCP,LpTHREAD gTP,TDictionary* theSpace, TObject* anObject, COMPARE* compareCode);
extern TVAL         TDictionary_Delete			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index);
extern NUM          TDictionary_GetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL			TDictionary_IDictionary		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[]);
extern void			TDictionary_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL         TDictionary_Insert			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index, TVAL newObject, TVAL newValue);
extern TVAL			TDictionary_MakeUniqueKey	(LpXCONTEXT gCP,LpTHREAD gTP,TDictionary* theSpace, TObject* theObject, TVAL keyData);
extern TDictionary* TDictionary_New				(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL         TDictionary_Print			(LpXCONTEXT gCP,LpTHREAD gTP,TDictionary* theEnv, LpNUM size, LpCHAR buf);
extern TVAL         TDictionary_SetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats);

extern void         TDictionary_ComputeSize		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize);
extern void         TDictionary_Doomed			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void         TDictionary_Mark			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL			TDictionary_Load			(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern HMemory      TDictionary_Save			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory anHMemory);
extern TObject*     TDictionary_Copy			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL         TDictionary_GetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1);
extern TVAL         TDictionary_GetIV2			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL index2);
extern TVAL         TDictionary_SetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue);
extern TVAL         TDictionary_SetIV2			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL index2,TVAL newValue);
extern TVAL         TDictionary_Map				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc);
extern TVAL         TDictionary_Mapc			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc);

#ifdef  _C_TDICTIONARY
#include "tlambda.h"
#include "fproc.h"
#include "fconio.h"
#include "futil2.h"
#include "fobject.h"
#endif

#endif
#endif
