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
TString.h

Interface for the string CLASS which stores a K&R C string in a variable length
object.

Note:   Duplicate TString objects containing the same K&R C string are eliminated
        by the TString constructors. Therefore, TString object may be compared for
        equality on the basis of the object handles alone.
        
PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TString
#define _H_TString

#include "tobject.h"

/*  Misc defines */

#define CharArray(tval)     ((LpCHAR)*((TString*)((tval).u.Object))->itsCString)
#define String(tval)        ((TString*)((tval).u.Object))
#define asString(atval)     ((TString*)((LpTVAL)(atval))->u.Obj)

/*  Disk Format */

typedef struct
    {
    NUM             itsMaxItemIndex;
    char            itsStringData[1];
    }TStringOnDisk;
#define TStringOnDiskPtr(h,n)   ((TStringOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TStringOnDisk    ((NUM)&((TStringOnDisk*)0)->itsStringData)

typedef struct
    {
    NUM     itsObject;
    NUM     itsOffset;
    NUM     itsLength;
    }TStringSubstringTOnDisk;
#define TStringSubstringTOnDiskPtr(h,n)     ((TStringSubstringTOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TStringSubstringTOnDisk      sizeof(TStringSubstringTOnDisk)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */
/*  The arguments will be the same as for TStringOnDiskPtr. */

#define TStringOnDiskData(h,n)  (char*)(_HMemoryPtr(h)+n+SIZEOF_TStringOnDisk)

#ifdef _SMARTBASE
/*  Function declarations */
extern  NUM         TString_GetMaxIndex			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TVAL        TString_GetNumericValue		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  LpCHAR      TString_GetStringPtr		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  void        TString_Init				(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TString*    TString_MakeUnique			(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString);
extern  TString*    TString_New					(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL        TString_SetMaxIndex			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM newRepeats);
extern  TString*    TString_SubString_MakeUnique(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR aString, NUM firstChar, NUM theLen);


/*  Method to function converions */

extern  void        TString_ComputeSize			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, NUM* aSize);
extern  TObject*    TString_Copy				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern  void        TString_Doomed				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
extern  TVAL        TString_GetIV1				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1);
extern  TVAL        TString_Load				(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
extern  TVAL        TString_Map					(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL proc);
extern  TVAL        TString_Mapc				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, TVAL proc);
extern  TVAL        TString_Print				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, LpNUM size, LpCHAR buf);
extern  HMemory     TString_Save				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, HMemory aHMemory);
extern  TVAL        TString_SetIV1				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue);
extern  TVAL        TString_StringAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL        TString_StringAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL        TString_TvalAnyCnv			(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget,TVAL oldValue);

extern  int                 substrlcmp                      (const char *str, const char *substr, size_t substrlen);
extern  int                 substrrcmp                      (const char *substr, size_t substrlen,const char *str);
extern  int                 substr2cmp                      (const char *substr1, const char *substr2, size_t len1, size_t len2);

extern  TVAL                TString_substrlcmp              (LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR pString, LpCHAR pSubstr, NUM Length);
extern  TVAL                TString_substrrcmp              (LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR pSubstr, NUM Length, LpCHAR pString);
extern  TVAL                TString_substr2cmp              (LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR pSubstr1, LpCHAR pSubstr2, NUM Length1, NUM Length2);

extern  void                TStringSubstringT_ComputeSize       (LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, NUM* aSize);
extern  TVAL                TStringSubstringT_GetIV1            (LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, TVAL index1);
extern  LpCHAR              TStringSubstringT_GetStringPtr      (LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval);
extern  TVAL                TStringSubstringT_Mapc              (LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, TVAL proc);
extern  TVAL                TStringSubstringT_Print             (LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, LpNUM size, LpCHAR buf);
extern  TVAL                TStringSubstringT_SubstringAnyCmp   (LpXCONTEXT gCP, LpTHREAD gTP, TVAL leftVal, TVAL rightVal);

#endif
    
#endif

