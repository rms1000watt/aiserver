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
TBrick.h

Implementation of the Brick class which stores a variable number of
fields in a linear array. The manifest typing system together with C++ methods 
control and size the the array.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TBRICK
#define _H_TBRICK

#include "tobject.h"

#define FieldArray(tval)    ((LpCHAR)*((tval).u.Brick->itsFieldArray))
#define asFieldArray(obj)   ((LpCHAR)*((TBrick*)(obj))->itsFieldArray)
#define Brick(tval)			((tval).u.Brick)
#define asBrick(atval)		(((TVAL*)atval)->u.Brick)
#define BrickFromRow(tval) ((TBrick*)_TObject_MainObjectList(tval.u.Index[0]))
#define BrickFromFld(tval) ((TBrick*)_TObject_MainObjectList(tval.u.Index[0]))


/*  The Brick disk structure format */

typedef struct
    {
    TVAL                itsCdr;         
    TVAL                itsFieldList;
    NUM                 itsMaxItemIndex;
    NUM                 itsFieldCount;
    NUM                 itsRowCount;
    NUM                 itsRowByteCount;
    CHAR                itsItemArray[1];    
    }TBrickOnDisk;
#define TBrickOnDiskPtr(h,n)		((TBrickOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TBrickOnDisk        ((NUM)&((TBrickOnDisk*)0)->itsItemArray)

/*  The BrickRow disk structure format */

typedef struct
    {
    NUM		            itsBrickObj;        
    NUM                 itsRowIndex;
    }TBrickRowOnDisk;
#define TBrickRowOnDiskPtr(h,n)	((TBrickRowOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TBrickRowOnDisk     ((NUM)sizeof(TBrickRowOnDisk))

/*  The BrickField disk structure format */

typedef struct
    {
    NUM	                itsBrickObj;         
    NUM                 itsRowIndex;
    NUM                 itsFieldIndex;
    }TBrickFieldOnDisk;
#define TBrickFieldOnDiskPtr(h,n)	((TBrickFieldOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TBrickFieldOnDisk   ((NUM)sizeof(TBrickFieldOnDisk))

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TBrickOnDisk. */

#define TBrickOnDiskData(h,n)  (CHAR*)(_HMemoryPtr(h)+n+SIZEOF_TBrickOnDisk)


#ifdef _SMARTBASE
/*  Declare the new type variables for this Class.   */
/*  Make sure basic types are registered in the proper order. */
/*  Note:   For speed, the FVMSCRIPT files use hard coded types (like TYNUM) */
/*          in switch statements. These hard coded basic type codes must be */
/*          coordinated with the actual dynamically registered types codes. */

/*  Function declarations */
extern TVAL         TBrick_GetCdr				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern NUM          TBrick_GetMaxIndex			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL         TBrick_Init				(LpXCONTEXT gCP,LpTHREAD gTP);
extern TBrick*		TBrick_New					(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL         TBrick_SetCdr				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue);
extern TVAL         TBrick_SetMaxIndex			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats);

/*  Methods converted to functions  */

extern void         TBrick_ComputeSize			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern TObject*     TBrick_Copy				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern void         TBrick_Doomed				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern TVAL         TBrick_Load				(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern void         TBrick_Mark				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern HMemory      TBrick_Save				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,HMemory aHMemory);
extern TVAL         TBrick_GetIV1				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL         TBrick_GetIV2				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2);
extern TVAL         TBrick_GetIV3				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL index3);
extern TVAL         TBrick_SetIV1				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);
extern TVAL         TBrick_SetIV2				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL newValue);
extern TVAL         TBrick_SetIV3				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL index3,TVAL newValue);
extern TVAL         TBrick_MakeNew				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL         TBrick_DeleteRows			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL         TBrick_InsertRows			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

extern TVAL			TBrick_Print				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size,LpCHAR buf);
extern TVAL			TBrick_PrintBrick			(LpXCONTEXT gCP,LpTHREAD gTP,TBrick* theEnv,LpNUM size,LpCHAR buf,BOLE PrintingBinding,NUM PrintIndex);

/* ******************* */
/* TBrickRowT Methods */ 
/* ******************* */
extern void         TBrickRowT_ComputeSize		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern TVAL         TBrickRowT_GetIV1          (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL         TBrickRowT_GetIV2          (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2);
extern TVAL         TBrickRowT_Print           (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size,LpCHAR buf);
extern TVAL         TBrickRowT_PrintBrickRow  (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size,LpCHAR buf,BOLE PrintingBinding,NUM PrintIndex);
extern TVAL         TBrickRowT_SetIV1          (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);
extern TVAL         TBrickRowT_SetIV2          (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL newValue);

/* ********************* */
/* TBrickFieldT Methods */ 
/* ********************* */
extern void             TBrickFieldT_ComputeSize       (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern TVAL             TBrickFieldT_GetIV1            (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern TVAL             TBrickFieldT_Print             (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size,LpCHAR buf);
extern TVAL             TBrickFieldT_PrintBrickField  (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size,LpCHAR buf,BOLE PrintingBinding,NUM PrintIndex);
extern TVAL             TBrickFieldT_SetIV1            (LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);

/*  Public functions  */
extern TVAL             TBrick_SymbolToTypeCode    (LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL             TBrick_TypeCode            (LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#endif

#endif

