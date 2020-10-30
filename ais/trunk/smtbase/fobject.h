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
FObject.h

PARENT:             None 


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FControl
#define _H_FControl

#include "tobject.h"

/*  Macro definitions */

/*  Function declarations */
#ifdef _SMARTBASE


extern  TVAL        FObject_BoolAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL        FObject_CharAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  void        FObject_ComputeSize		(LpXCONTEXT gCP, LpTHREAD gTP,TObject* anObject, NUM* aSize);
extern  void        FObject_ComputeSizeNever(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, NUM *aSize);
extern  TVAL        FObject_Copy			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self);
extern  TObject*	FObject_CopyNever		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self);
extern  TVAL        FObject_CompareNever	(LpXCONTEXT gCP, LpTHREAD gTP,TVAL left,TVAL right);
extern  TVAL        FObject_DateAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  void        FObject_Doomed			(LpXCONTEXT gCP, LpTHREAD gTP,TObject* anObject);
extern  void        FObject_DoomNever		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self);
extern  TVAL        FObject_ErrorAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL        FObject_GetIV1Bits		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL srcTval,TVAL index1);
extern  TVAL        FObject_GetIV1Never		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL srcTval,TVAL index1);
extern  TVAL        FObject_GetIV2Never		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL srcTval,TVAL index1,TVAL index2);
extern  TVAL        FObject_GetIV3Never		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL srcTval,TVAL index1,TVAL index2,TVAL index3);
extern  TVAL        FObject_IntAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL        FObject_UIntAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL        FObject_Map				(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc);
extern  TVAL        FObject_Mapc			(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, TVAL proc);
extern  TVAL        FObject_MoneyAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL        FObject_ObjAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL        FObject_RealAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  void        FObject_Save			(LpXCONTEXT gCP, LpTHREAD gTP,TObject* anObject, HMemory aHMemory);
extern  HMemory     FObject_SaveNever		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, HMemory aHMemory);
extern  TVAL        FObject_SetIV1Bits		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval, TVAL index1,TVAL newValue);
extern  TVAL        FObject_SetIV1Never		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval, TVAL index1,TVAL newValue);
extern  TVAL        FObject_SetIV2Never		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval, TVAL index1,TVAL index2,TVAL newValue);
extern  TVAL        FObject_SetIV3Never		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL dstTval, TVAL index1,TVAL index2,TVAL index3,TVAL newValue);
extern  TVAL        FObject_ShortAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL        FObject_TvalAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);
extern  TVAL        FObject_VoidAnyCnv		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);


extern  TVAL        FObject_AddNewValue		(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, TVAL newValue);
extern  TVAL        FObject_atoui           (LpXCONTEXT gCP, LpTHREAD gTP, LpUNUM n, LpCHAR sp);
extern  TVAL        FObject_atoi			(LpXCONTEXT gCP, LpTHREAD gTP, LpNUM n, LpCHAR sp);
extern  void        FObject_Destruct		(LpXCONTEXT gCP, LpTHREAD gTP, TObject* self);
extern  TVAL        FObject_GetCdr			(LpXCONTEXT gCP, LpTHREAD gTP, TObject* anObject);
extern  TVAL        FObject_GetMaxIndex		(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval);
extern  LpCHAR      FObject_GetStringPtr	(LpXCONTEXT gCP, LpTHREAD gTP, TObject* anObject);
extern  BOLE        FObject_Perm			(LpXCONTEXT gCP, LpTHREAD gTP, TObject* self, BOLE fPerm);
extern  TVAL        FObject_Print			(LpXCONTEXT gCP, LpTHREAD gTP, TVAL self, LpNUM size, LpCHAR buf);
extern  TVAL        FObject_recfrac			(LpXCONTEXT gCP, LpTHREAD gTP, LpREAL dp,LpNUM count,LpCHAR sp);
extern  TVAL        FObject_recint			(LpXCONTEXT gCP, LpTHREAD gTP, LpNUM intResult, LpREAL realResult, LpNUM count, LpCHAR sp);
extern  TVAL        FObject_SetCdr			(LpXCONTEXT gCP, LpTHREAD gTP, TObject* anObject, TVAL  newCdr);
extern  TVAL        FObject_SetMaxIndex		(LpXCONTEXT gCP, LpTHREAD gTP, TObject* anObject, NUM newRepeats);


#endif

#ifdef _C_FOBJECT
#include "fproc.h"
#include "tpair.h"
#include "fmacro.h"
#include "fconio.h"
#include "tnumvec.h"
#include "tfltvec.h"
#include "tbitvec.h"
#include "tbytevec.h"
#include "tcontin.h"
#include "tstruct.h"
#include "tintvec.h"
#include "tobjvec.h"
#include "twkspace.h"
#include "tpcodvec.h"
#include "tlambda.h"
#include "tsymbol.h"
#include "tvector.h"
#include "twkspace.h"

#endif
#endif

