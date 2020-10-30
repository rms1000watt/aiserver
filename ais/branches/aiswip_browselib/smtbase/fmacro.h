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
FMacro.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FMacro
#define _H_FMacro
#include "tsymbol.h"
#include "tpair.h"

/*  Macro definitions */

#define _FMacro_PairsToArray(p, a, max)     \
{UNUM cn;                                    \
for(a[0] = p, cn = 1; cn < max ; cn++)      \
    if ((a[cn-1] != NIL) && (isPair(&a[cn-1]->itsCdr))) \
        a[cn] = asPair(&a[cn-1]->itsCdr);   \
     else                                   \
        a[cn] = (TPair*)NIL;                \
}

/*  Function declarations */

extern  TVAL    FMacro_Defmacro		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FMacro_Defmethod	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FMacro_Defstruct	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FMacro_Defun		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FMacro_Defvm		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FMacro_DivEquals	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FMacro_MinusMinus	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FMacro_MinusEquals	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FMacro_PlusPlus		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FMacro_PlusEquals	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FMacro_TimesEquals	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

extern  TVAL    FMacro_Init			(LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL    FMacro_NewCMacro	(LpXCONTEXT gCP, LpTHREAD gTP, TSymbol **symName, LpCHAR funcName, LpFUNC lpFunc);
extern  TVAL    FMacro_GlobalMark	(LpXCONTEXT gCP, LpTHREAD gTP);


#ifdef _C_FMACRO
#include "futil2.h"
#include "tvector.h"
#include "tobjvec.h"
#include "tstruct.h"
#include "tlambda.h"
#include "fproc.h"
#include "fcompile.h"
#include "flisp.h"
#include "fconio.h"

/*--------------------------------------------------------------------------------------- */
#if 0
C preprocessor MACROS for FMacro.c

#endif

#define _FMacro_GetFreeNode(list)           \
if (list->itsCar.Tag != TYVOID)				\
    {                                       \
    list->itsCdr = gCP->Tval_VOID;         \
    asObject(&list->itsCdr) = (TObject*)TPair_New(gCP,gTP); \
    asTag(&list->itsCdr) = TYPAIR;          \
    list = (TPair*)asObject(&list->itsCdr); \
    }

#define _FMacro_AddAtom(list, atom)     \
_FMacro_GetFreeNode(list)               \
list->itsCar = atom;                

#define _FMacro_NewNode(curPair)        \
    {                                   \
    TVAL    tmp;                        \
    tmp = gCP->Tval_VOID;              \
    asObject(&tmp) = (TObject*)TPair_New(gCP,gTP);  \
    asTag(&tmp) = TYPAIR;               \
    _FMacro_AddAtom(curPair, tmp);      \
    curPair = asPair(&curPair->itsCar); \
    }


#endif

#ifdef _C_FMACRO
#include "fobject.h"
#endif

#endif

