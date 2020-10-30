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
TObject.h

Interface for the typed object CLASS which is at the root of the typing hierarchy. 
TObject is the CLASS which supports runtime typing and automatic garbage collection.

Note:   This CLASS contains the support for the C++ extensions such as: automated
        mark and sweep garbage collection, manifest types, and oodbms save and load.


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_TObject
#define _H_TObject

#include "fsmtbase.h"
#include "fmemory.h"
#include "fobject.h"


#if 0
OBJECT FILE RECORD SIGNATURE

The definition of the object file record signature.

#endif

#define _OBJECTRECORDTYPE   0       /* Signature of Object file records */ 
#define _TVALRECORDTYPE     1		/* Signature of TVAL file records */
#define _INDEXRECORDTYPE    2       /* Signature of Index file records */

/*  Class macro declarations */

#if 0
FRAME REGISTRATION MACROS

The TObject CLASS supports a frame for registration to temporary OBJ and TVAL variables
which need to be marked during the garbage collection process. These macros are used
to register any temoporary OBJ or TVAL variables.

#endif

/*--------------------------------------------------------------------------------------- */
#if 0
_TObject_LoadTval

This is a MACRO version of TObject_LoadTval which may be used for speed.

#endif

#define _TObject_LoadTval(tag, pTval)                               \
if (_TObject_TypeFlag(tag) == _TObject_TfTOBJECT)                   \
    asObject(pTval) = (TObject*)TObject_CheckRegistration(gCP,gTP,(NUM)asObj(pTval));

#if 0
FRAMING MACROS

The TObject CLASS supports a frame for registration to temporary OBJ and TVAL variables
which need to be marked during the garbage collection process. This macro is used
to exit any function or method which has registered temoporary OBJ or TVAL variables.
Whenever the frame is reset we automatically turn framing back on. This prevents
errors which might occur when framing is turned off for performance and then not turned
back on.

#endif

#define FTVAL   NUM

#define _TObject_FramedObject(index)    ((TObject*)(*gTP->ObjStack[index]))

#define _TObject_RecordFrame\
    {NUM __tvalf__ = gTP->TvalStackIdx;\
     NUM __objsf__ = gTP->ObjStackIdx;\
     NUM __recur__ = gTP->RecursionCount;

#if 1
#define _TObject_CheckFrame\
     gTP->TvalStackIdx = __tvalf__;\
     gTP->ObjStackIdx = __objsf__;\
     gTP->RecursionCount = __recur__;\
    }
#else
#define _TObject_CheckFrame\
     if (__tvalf__ != gTP->TvalStackIdx)\
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_STACK_RELEASE);\
     else\
     if (__objsf__ != gTP->ObjStackIdx)\
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_FRAME_RELEASE);\
     else\
     if (__recur__ != gTP->RecursionCount)\
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_RECURSION_RELEASE);\
    }
#endif

#define _TObject_FrameTval(var) __tval__++

#define _TObject_FrameTobj(var)\
    (gTP->ObjStack[__obj__++] = (OBJ *)(var))
    

#if 0
STATUS TEST MACROS

The TObject CLASS supports status test macros. These macros are contained 
herein to maximize portability.

#endif

#define _TObject_IsError(sts)           (asTag(&sts) == TYERROR)
#define _TObject_ErrorChk(sts)          if (asTag(&sts) == TYERROR) return(sts)
#define _TObject_ErrorJmp(sts,lbl)      if (asTag(&sts) == TYERROR) goto lbl

#if 0
TVAL TEST MACRO

The TObject CLASS supports a TVAL validity testing macro. This macro 
returns TRUE if the TVAL supports a brief test for validity. This  
macro is used to check system internal states for correctness.

#endif

#define _VALIDTVAL(t)																	\
	((((t).Tag >= TYVOID) && ((t).Tag <= TYMAXVALIDTYPE)) &&                            \
     ((_TObject_TypeFlag((t).Tag) != _TObject_TfTOBJECT)  ||                            \
      ((((LpCHAR)(t).u.Pointer >= (LpCHAR)gCP->FMemory_ParentPtr) &&                         \
        ((LpCHAR)(t).u.Pointer < (LpCHAR)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated))) &&  \
       (((t).Tag == (t).u.Object->itsObjectType) ||										\
		(((t).Tag == TYSPECIALFORM) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||	\
		(((t).Tag == TYQUOTEDSYMBOL) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||	\
		(((t).Tag == TYLABELSYMBOL) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||	\
		(((t).Tag == TYCPROCEDURE) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||		\
		(((t).Tag == TYVMEVALUATOR) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||	\
		(((t).Tag == TYCMACRO) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||			\
		(((t).Tag == TYMACRO) && ((t).u.Object->itsObjectType == TYLAMBDA)) ||			\
		(((t).Tag == TYQUOTEDPAIR) && ((t).u.Object->itsObjectType == TYPAIR))))))

#if 0
OBJECT TEST MACRO

The TObject CLASS supports an OBJECT validity testing macro. This macro 
returns TRUE if the OBJECT supports a brief test for validity. This  
macro is used to check system internal states for correctness.

#endif

#define _VALIDOBJ(o)																\
	(((LpCHAR)(o) >= (LpCHAR)gCP->FMemory_ParentPtr) &&								\
	 ((LpCHAR)(o) < (LpCHAR)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated)) &&	\
	 (((TObject*)(o))->itsObjectType > TYVOID) &&									\
	 (((TObject*)(o))->itsObjectType <= TYMAXVALIDTYPE) &&							\
	 (((TObject*)(o))->itsObjectIndex >= 0) &&										\
	 (((TObject*)(o))->itsObjectIndex < gCP->TObject_MaxObjectCount) &&				\
	 (_TObject_TypeFlag(((TObject*)(o))->itsObjectType) == _TObject_TfTOBJECT))

#if 0
TVAL CHECK MACRO

The TObject CLASS supports a TVAL validity testing macro. This macro 
returns TRUE if the TVAL supports a brief test for validity. This  
macro is used to check system internal states for correctness.

#endif

#define _CHECKTVAL(t)																	\
	((((t).Tag >= TYVOID) && ((t).Tag <= TYMAXVALIDTYPE)) &&                            \
     ((_TObject_TypeFlag((t).Tag) != _TObject_TfTOBJECT)  ||                            \
      ((((LpCHAR)(t).u.Pointer >= (LpCHAR)gCP->FMemory_ParentPtr) &&                         \
        ((LpCHAR)(t).u.Pointer < (LpCHAR)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated))) &&  \
       (((t).Tag == (t).u.Object->itsObjectType) ||										\
		(((t).Tag == TYSPECIALFORM) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||	\
		(((t).Tag == TYQUOTEDSYMBOL) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||	\
		(((t).Tag == TYLABELSYMBOL) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||	\
		(((t).Tag == TYCPROCEDURE) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||		\
		(((t).Tag == TYVMEVALUATOR) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||	\
		(((t).Tag == TYCMACRO) && ((t).u.Object->itsObjectType == TYSYMBOL)) ||			\
		(((t).Tag == TYMACRO) && ((t).u.Object->itsObjectType == TYLAMBDA)) ||			\
		(((t).Tag == TYQUOTEDPAIR) && ((t).u.Object->itsObjectType == TYPAIR))))))

#if 0
OBJECT CHECK MACRO

The TObject CLASS supports an OBJECT validity checking macro. This macro 
returns TRUE if the OBJECT supports a brief test for validity. This  
macro is used to check system internal states for correctness.

#endif

#define _CHECKOBJ(o)																\
	(((LpCHAR)(o) == NULL) ||														\
	 (((LpCHAR)(o) >= (LpCHAR)gCP->FMemory_ParentPtr) &&								\
	  ((LpCHAR)(o) < (LpCHAR)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated)) &&	\
	  (((TObject*)(o))->itsObjectType > TYVOID) &&									\
	  (((TObject*)(o))->itsObjectType <= TYMAXVALIDTYPE) &&							\
	  (((TObject*)(o))->itsObjectIndex >= 0) &&										\
	  (((TObject*)(o))->itsObjectIndex < gCP->TObject_MaxObjectCount) &&			\
	  (_TObject_TypeFlag(((TObject*)(o))->itsObjectType) == _TObject_TfTOBJECT)))

#if 0
FLAG TEST MACROS

The TObject CLASS supports flag test macros. These macros are contained 
herein to maximize portability.

#endif

#define _TObject_MainObjectList(ix)     atHMObj(gCP->TObject_MainObjectList,ix)
#define _TObject_ObjectByIndex(ix)      atHMObject(gCP->TObject_MainObjectList,ix)
#define _TObject_ObjectFlag(ix)         atHMChar(gCP->TObject_ObjectFlag,ix)
#define _TObject_IsMarked(oo)           (atHMChar(gCP->TObject_ObjectFlag,((TObject*)(oo))->itsObjectIndex) & _TObject_OfMARK)
#define _TObject_MarkFlag(oo)           atHMChar(gCP->TObject_ObjectFlag,((TObject*)(oo))->itsObjectIndex)
#define _TObject_TypeName(tt)           atHMName(gCP->TObject_TypeName,(tt))
#define _TObject_TypeFlag(tt)           atHMChar(gCP->TObject_TypeFlag,(tt))
#define _TObject_TypeSize(tt)           atHMShort(gCP->TObject_TypeSize,(tt))
#define _TObject_TypeNew(tt)            atHMLpfnew(gCP->TObject_TypeNew,(tt))
#define _TObject_TypeMark(tt)           atHMLpfmark(gCP->TObject_TypeMark,(tt))
#define _TObject_TypeGlobalMark(tt)     atHMLpfgmark(gCP->TObject_TypeGlobalMark,(tt))
#define _TObject_TypeConvert(tt)        atHMLpfconvert(gCP->TObject_TypeConvert,(tt))
#define _TObject_TypeCompare(tt)        atHMLpf2tvals(gCP->TObject_TypeCompare,(tt))
#define _TObject_TypeSetIV1(tt)         atHMLpf3tvals(gCP->TObject_TypeSetIV1,(tt))
#define _TObject_TypeSetIV2(tt)         atHMLpf4tvals(gCP->TObject_TypeSetIV2,(tt))
#define _TObject_TypeSetIV3(tt)         atHMLpf5tvals(gCP->TObject_TypeSetIV3,(tt))
#define _TObject_TypeGetIV1(tt)         atHMLpf2tvals(gCP->TObject_TypeGetIV1,(tt))
#define _TObject_TypeGetIV2(tt)         atHMLpf3tvals(gCP->TObject_TypeGetIV2,(tt))
#define _TObject_TypeGetIV3(tt)         atHMLpf4tvals(gCP->TObject_TypeGetIV3,(tt))
#define _TObject_TypeMap(tt)            atHMLpf2tvals(gCP->TObject_TypeMap,(tt))
#define _TObject_TypeMapc(tt)           atHMLpf2tvals(gCP->TObject_TypeMapc,(tt))
#define _TObject_TypePrint(tt)          atHMLpfprint(gCP->TObject_TypePrint,(tt))
#define _TObject_TypeLoad(tt)           atHMLpfnload(gCP->TObject_TypeLoad,(tt))
#define _TObject_TypeSave(tt)           atHMLpfnsave(gCP->TObject_TypeSave,(tt))
#define _TObject_TypeComputeSize(tt)    atHMLpfcomputesize(gCP->TObject_TypeComputeSize,(tt))
#define _TObject_TypeDoom(tt)           atHMLpfdoom(gCP->TObject_TypeDoom,(tt)) 
#define _TObject_TypeCopy(tt)           atHMLpfcopy(gCP->TObject_TypeCopy,(tt)) 

#if 0
DEFSTRUCT MANAGEMENT MACROS

The defstruct functionality supports some management macros. These macros are contained 
herein to maximize portability.

#endif

#define _TObject_TypeMethods(tt)            atHMTval(gCP->TObject_TypeMethods,(tt))
#define _TObject_TypeFields(tt)             atHMTval(gCP->TObject_TypeFields,(tt))
#define _TObject_TypeParent(tt)             atHMType(gCP->TObject_TypeParent,(tt))

#if 0
CHARACTER TEST MACROS

FSmartbase supports its own CHAR test macros. These macros are contained 
herein to maximize portability. These CHAR test macros are dependent upon 
the TObject_Ctype array in the CLASS global area.

NOTE: The CHAR data type must be defined as unsigned in order for these 
      tests to operate correctly on data > 127.

#endif

#define CHRALPHA        1               /* FLAG: CHAR IS ALPHA          */
#define CHRDIGIT        2               /* FLAG: CHAR IS A DEC DIGIT    */
#define CHRHEX          4               /* FLAG: CHAR IS A HEX DIGIT    */
#define CHROCTAL        8               /* FLAG: CHAR IS AN OCTAL DIGIT */
#define CHRASCII        16              /* FLAG: CHAR IS AN ASCII CHAR  */
#define CHRCNTRL        32              /* FLAG: CHAR IS A CNTRL CHAR   */
#define CHRPUNCT        64              /* FLAG: CHAR IS PUNCTUATION    */
#define CHRSPACE        128             /* FLAG: CHAR IS WHITE SPACE    */
#define CHRUPPER        256             /* FLAG: CHAR IS UPPER CASE     */
#define CHRLOWER        512             /* FLAG: CHAR IS LOWER CASE     */
#define CHRSYMBOL       1024            /* FLAG: CHAR IS A SYMBOL CHAR  */
#define CHROPER         2048            /* FLAG: CHAR IS AN OPERATOR    */

#define ISALNUM(c)      (gCP->TObject_Ctype[(c)] & (CHRALPHA|CHRDIGIT))
#define ISALPHA(c)      (gCP->TObject_Ctype[(c)] & CHRALPHA)
#define ISASCII(c)      (gCP->TObject_Ctype[(c)] & CHRASCII)
#define ISCNTRL(c)      (gCP->TObject_Ctype[(c)] & CHRCNTRL)
#define ISSYM(c)        ((gCP->TObject_Ctype[(c)] & (CHRALPHA|CHRDIGIT)) || (c) == '_')
#define ISCSYMF(c)      ((gCP->TObject_Ctype[((c))] & CHRALPHA) || (c) == '_')
#define ISDIGIT(c)      (gCP->TObject_Ctype[((c))] & CHRDIGIT)
#define ISGRAPH(c)      ((c) >= '!' && (c) <= '~')
#define ISODIGIT(c)     (gCP->TObject_Ctype[(c)] & CHROCTAL)
#define ISOPER(c)       (gCP->TObject_Ctype[(c)] & CHROPER)
#define ISPRINT(c)      (((c) >= 32) && ((c) < 127))
#define ISPUNCT(c)      (gCP->TObject_Ctype[(c)] & CHRPUNCT)
#define ISSPACE(c)      (gCP->TObject_Ctype[(c)] & CHRSPACE)
#define ISSYMBOL(c)     (gCP->TObject_Ctype[(c)] & CHRSYMBOL)
#define ISXDIGIT(c)     (gCP->TObject_Ctype[(c)] & CHRHEX)
#define ISXUPPER(c)     (gCP->TObject_Ctype[(c)] & CHRUPPER)
#define ISXLOWER(c)     (gCP->TObject_Ctype[(c)] & CHRLOWER)
#define TOASCII(c)      ((c) & 0x7F)



#ifdef _SMARTBASE

#if 0
CLASS DECLARATIONS

The TObject CLASS is defined by the following C++ declarations.

#endif
    
/*  Function declarations */

extern  TVAL      TObject_atob				(LpXCONTEXT gCP,LpTHREAD gTP,LpBOLE b,LpCHAR sp);
extern  TVAL      TObject_ator				(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL rs,LpCHAR sp);
extern  TObject*  TObject_CheckRegistration	(LpXCONTEXT gCP,LpTHREAD gTP,NUM aFileID);
extern  TVAL      TObject_CnvFromText		(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR inputText);
extern  TVAL      TObject_CnvToText			(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pTarget, NUM maxLen, TVAL source);
extern  TVAL      TObject_Compare			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL      TObject_Convert			(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  void      TObject_Destruct			(LpXCONTEXT gCP,LpTHREAD gTP,TObject* self);
extern  TVAL      TObject_Error				(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR theMsg);
extern  void      TObject_FrameError		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TYPE      TObject_GetClassType		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  TVAL      TObject_GetIV1			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL srcTval, TVAL index1);
extern  TVAL      TObject_GetIV2			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL srcTval, TVAL index1,TVAL index2);
extern  TVAL      TObject_GetIV3			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL srcTval, TVAL index1,TVAL index2,TVAL index3);
extern  void      TObject_Init				(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL      TObject_Invalid			(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TObject*  TObject_LoadObject		(LpXCONTEXT gCP,LpTHREAD gTP,NUM theFile, NUM aFileID, NUM bResolve );
extern  TVAL      TObject_LoadTval			(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  void      TObject_MarkAndSweep		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void      TObject_MarkObj			(LpXCONTEXT gCP,LpTHREAD gTP,TObject* anObject);
extern  void      TObject_NewCompare		(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget,LpF2TVALS aCmpFunction);
extern  void      TObject_NewConvert		(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget,LpFCONVERT aCnvFunction);
extern  void      TObject_NewSetIndexedValue(LpXCONTEXT gCP,LpTHREAD gTP,TYPE tTarget,LpF3TVALS aSetIndexedValueFunction);
extern  void      TObject_OperatorDelete	(LpXCONTEXT gCP,LpTHREAD gTP,TObject* self);
extern  void*     TObject_OperatorNew		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL      TObject_Print				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern  TVAL      TObject_PrintNever		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern  TVAL      TObject_RegisterLoad		(LpXCONTEXT gCP,LpTHREAD gTP,NUM aFileID, TObject*  theObject);
extern  NUM       TObject_RegisterObject	(LpXCONTEXT gCP,LpTHREAD gTP,TObject* anObject);
extern  TVAL      TObject_RegisterTval		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
extern  NUM       TObject_sprintReal		(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR buf,REAL aReal);
extern  TVAL      TObject_strcmp			(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pTarget,LpCHAR pSource);
extern  TVAL      TObject_strncmp			(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pTarget,LpCHAR pSource,NUM Length);


extern  TVAL      TObject_BoolAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL      TObject_CharAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL      TObject_CharPrint			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern  void      TObject_ComputeSize		(LpXCONTEXT gCP, LpTHREAD gTP,long* aSize);
extern  TVAL      TObject_DateAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL      TObject_ErrorAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL      TObject_GetIVText			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL srcTval, TVAL index1);
extern  TVAL      TObject_GlobalMark		(LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL      TObject_GlobalMarkNever	(LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL      TObject_IntAnyCmp			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL      TObject_LoadFile			(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL      TObject_LoadNever			(LpXCONTEXT gCP, LpTHREAD gTP,HMemory anHMemory, NUM theFileID, NUM bResolve);
extern  TVAL      TObject_Map				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc);
extern  TVAL      TObject_Mapc				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc);
extern  TVAL      TObject_MapcObject		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc);
extern  TVAL      TObject_MapcText			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc);
extern  TVAL      TObject_MapDefault		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc);
extern  TVAL      TObject_MapObject			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc);
extern  TVAL      TObject_MapText			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc);
extern  void      TObject_MarkNever			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL aTval);
extern  void      TObject_MarkTval			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL aTval);
extern  TVAL      TObject_MoneyAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TObject*  TObject_New				(LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL      TObject_NewNever			(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL      TObject_NilFunction		(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL      TObject_PrintObj			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern  TVAL      TObject_PrintDefault		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern  TVAL      TObject_RealAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL      TObject_FloatAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL      TObject_SaveFile			(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]) ;
extern  TVAL      TObject_SetIV1			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval, TVAL index1,TVAL newValue);
extern  TVAL      TObject_SetIV2			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval, TVAL index1,TVAL index2,TVAL newValue);
extern  TVAL      TObject_SetIV3			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval, TVAL index1,TVAL index2,TVAL index3,TVAL newValue);
extern  TVAL      TObject_SetIVText			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval, TVAL index1,TVAL newValue);
extern  TVAL      TObject_ShortAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL      TObject_TextAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL      TObject_TextAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL      TObject_CnvDateToText		(LpXCONTEXT gCP, LpTHREAD gTP,LpCHAR buf, TVAL dateValue);
extern  TVAL      TObject_TextPrint			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern  TVAL      TObject_TvalAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);
extern  TVAL      TObject_VoidAnyCmp		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL leftVal, TVAL rightVal);

/*  Conversion functions. */



/*  Standard Unix library extension functions. */

#if defined(_GCC) && defined(_WIN)
extern  NUM       strnlen(LpCHAR aString,NUM aMaxLength);
#endif

#include "tvector.h"
#include "fpred2.h"
#if defined(_C_TOBJECT)
#include "tstring.h"
#include "tcontin.h"
#include "fproc.h"
#include "tbitvec.h"
#include "tintvec.h"
#include "tpcodvec.h"
#include "tbytevec.h"
#include "tnumvec.h"
#include "tfltvec.h"
#include "tobjvec.h"
#include "tpair.h"
#include "fconio.h"
#include "fmacro.h"
#include "tlambda.h"
#include "tstruct.h"
#include "futil2.h"
#include "fobject.h"
#endif

#endif
#endif

