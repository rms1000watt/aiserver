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
TSymbol.h

Interface for the string CLASS which stores a K&R C string in a variable length
object.

The global symbol/value table is stored piecemeal in each TSymbol object.
Note:   This tradeoff requires a little longer to do a complete table traversal,
        but yields immediate symbol to value lookup because the global value for
        a symbol (if any) is stored with the symbol. Remember that TSymbol objects
        never have duplicates!! 

Note:   Duplicate TSymbol objects containing the same K&R C string are eliminated
        by the TSymbol constructors. Therefore, TSymbol objects may be compared for
        equality on the basis of the object handles alone.
        
PARENT:             TString 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TSymbol
#define _H_TSymbol

#include "tobject.h"
#include "tstring.h"

#define objSymbolArray(obj) ((LpCHAR)*((TSymbol*)(obj))->itsCString)
#define SymbolArray(tval)   ((LpCHAR)*((TSymbol*)((tval).u.Object))->itsCString)
#define Symbol(tval)        ((TSymbol*)((tval).u.Object))
#define asSymbol(atval)     ((TSymbol*)((LpTVAL)(atval))->u.Obj)
    
/*  Disk Format */

typedef struct
    {
    unsigned char       itsGlobalLock : 1;
    unsigned char       itNeedsBars   : 1;
    TVAL                itsGlobalValue;
	NUM					itsVMEmulator;
    NUM                 itsUserTypeParent;
    NUM                 itsUserTypeFields;
    NUM                 itsUserTypeMethods;
    }TSymbolOnDisk;
#define TSymbolOnDiskPtr(h,n)   ((TSymbolOnDisk*)(_HMemoryPtr(h)+n))

/*  Note:	To increase the speed of Symbol lookup, we use a symbol table vector */
/*			which is sorted alphabetically on the text contents of each symbol. */

/*  Function declarations */
extern  TVAL     TSymbol_AddSortedValue			(LpXCONTEXT gCP,LpTHREAD gTP,struct TObjVector* theSpace, TSymbol* aTSymbol );
extern  TVAL     TSymbol_BSearch				(LpXCONTEXT gCP,LpTHREAD gTP,struct TObjVector* alphaSortedVector, LpCHAR aString, COMPARE* compareCode);
extern  NUM	     TSymbol_FindInSymbolTable		(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString, COMPARE* compareCode);
extern  void     TSymbol_CheckSymbolTable		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL	 TSymbol_GetGlobalValue			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  long	 TSymbol_GetInheritedComputedSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TVAL     TSymbol_GetIVGlobal			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL index);
extern  void     TSymbol_Init					(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TSymbol* TSymbol_MakeUnique				(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString);
extern  TSymbol* TSymbol_New					(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL	 TSymbol_SetGlobalValue			(LpXCONTEXT gCP,LpTHREAD gTP,TSymbol* self,TVAL newValue);
extern  TVAL	 TSymbol_SetIV1					(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);
extern  TVAL     TSymbol_SetIVGlobal			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL index, TVAL newValue);
extern  TSymbol* TSymbol_SubString_MakeUnique	(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString, NUM firstChar, NUM theLen);

/*  Method to function conversions */

extern  void	 TSymbol_ComputeSize			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,NUM* aSize);
extern  void	 TSymbol_Doomed					(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern  TVAL     TSymbol_GlobalAnyCmp			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL     TSymbol_GlobalAnyCnv			(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL     TSymbol_Load					(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  TVAL     TSymbol_MapcGlobal				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc);
extern  TVAL     TSymbol_MapGlobal				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc);
extern  void	 TSymbol_Mark					(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern  TVAL     TSymbol_PrintGlobal			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern  TVAL     TSymbol_PrintQuoted			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern  HMemory	 TSymbol_Save					(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,HMemory aHMemory);
extern  TVAL     TSymbol_SymbolAnyCnv			(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL     TSymbol_SymbolAnyCmp			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);

    
#endif

