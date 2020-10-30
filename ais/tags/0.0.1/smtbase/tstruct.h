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
TStructure.h 

Interface for the Structure CLASS which demonstrates the protocols required for the
OODBMS CLASS hierarchy. TStructure is a descendent of TObject.

Note:   This CLASS contains the support for the C++ extensions such as: automated
        mark and sweep garbage collection, manifest types, and oodbms save and load.


PARENT:             TObject

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TStructure
#define _H_TStructure

#include "tsymbol.h"

/*  Class definitions */

#define BindArray(tval)     ((LpBIND)*((TStructure*)((tval).u.Object))->itsDictionaryArray)
#define asBindArray(obj)    ((LpBIND)*((TStructure*)(obj))->itsDictionaryArray)
#define Structure(tval)		((TStructure*)((tval).u.Object))
#define asStructure(a)		((TStructure*)((LpTVAL)(a))->u.Obj)

/*  Class type definitions */
typedef struct 
    {
     TVAL       Value;
     NUM        Key;
    } BINDONDISK;
    
typedef struct 
    {
     TStructure*  Structure;
     NUM            indexOf;
    } BINDING, *LpBINDING;

#define asBindingStructure(a)   (((LpBINDING)&((LpTVAL)(a))->u)->Structure)
#define asBindingIndex(a)       (((LpBINDING)&((LpTVAL)(a))->u)->indexOf)
#define asBinding(a)            (((LpBINDING)&((LpTVAL)(a))->u))

/*  Class macro definitions */
#define _TStructure_KeyIndex  0
#define _TStructure_ValueIndex    1
    

/*  The disk structure format */
typedef struct
    {
    NUM                 itsMaxItemIndex;        
    NUM                 itsMethods;
    TVAL                itsCdr; 
    BINDONDISK          itsItemArray[1]; 
    }TStructureOnDisk;


#define TStructureOnDiskPtr(h,n)  ((TStructureOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TStructureOnDisk   ((NUM)&((TStructureOnDisk*)0)->itsItemArray)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TStructureOnDisk. */

#define TStructureOnDiskData(h,n) (BINDONDISK*)(_HMemoryPtr(h)+n+SIZEOF_TStructureOnDisk)


#ifdef _SMARTBASE
/*  Function declarations */
extern TVAL         TStructure_AddNewValue		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newKey,TVAL newValue);
extern TVAL			TStructure_BSearchKey		(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* theSpace, TObject* anObject, COMPARE* compareCode);
extern TVAL			TStructure_BSearchValue		(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* theSpace, TVAL keyData, COMPARE* compareCode);
extern TVAL         TStructure_Delete			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index);
extern TVAL         TStructure_GetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern NUM          TStructure_GetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void			TStructure_Init				(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL         TStructure_Insert			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index, TVAL newObject, TVAL newValue);
extern TVAL         TStructure_isBound			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1);
extern TVAL         TStructure_IStructure		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[],TVAL newCdr);
extern TVAL			TStructure_MakeUniqueKey	(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* theSpace, TObject* theObject, TVAL keyData);
extern TVAL			TStructure_MakeUniqueValue	(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* theSpace, TObject* theObject, TVAL keyData);
extern TStructure*	TStructure_New				(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL         TStructure_Print			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf);
extern TVAL			TStructure_PrintStructure	(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* theEnv, LpNUM size, LpCHAR buf, BOLE PrintingBinding, NUM PrintIndex );
extern TVAL			TStructure_SearchKey		(LpXCONTEXT gCP,LpTHREAD gTP,TStructure* theSpace, TObject* anObject, COMPARE* compareCode);
extern TVAL         TStructure_SetCdr			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL newValue);
extern TVAL         TStructure_SetMaxIndex		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats);

extern void         TStructure_ComputeSize		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize);
extern TObject*     TStructure_Copy				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void         TStructure_Doomed			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL         TStructure_GetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1);
extern TVAL         TStructure_GetIV2			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL index2);
extern void         TStructure_Mark				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL         TStructure_Map				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc);
extern TVAL         TStructure_Mapc				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc);
extern TVAL			TStructure_Load				(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern HMemory      TStructure_Save				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory anHMemory);
extern TVAL         TStructure_SetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue);
extern TVAL         TStructure_SetIV2			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL index2,TVAL newValue);

#ifdef  _C_TSTRUCTURE
#include "tlambda.h"
#include "fproc.h"
#include "fconio.h"
#include "futil2.h"
#include "fobject.h"
#endif
#endif
#endif

