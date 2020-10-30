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
FSpecialForms1.h

PARENT:             None 


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FSpecialForms1
#define _H_FSpecialForms1

#include    "tpair.h"
#include    "tvector.h"

/*  Macro definitions */

/*  Function declarations */

extern  TVAL    FSpecialForms1_AllocTemp(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState,NUM preferredType);
extern  TVAL    FSpecialForms1_CodeMove	(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TVAL SFResult, TVAL finalResult);
extern  TVAL    FSpecialForms1_defun    (LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms1_define   (LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms1_GetResult(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState);
extern  TVAL    FSpecialForms1_lambda   (LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms1_let      (LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* curPair);

#endif

#ifdef  _C_FSPECIALFORMS1
#include    "futil2.h"
#include    "tlambda.h"
#include    "fvmcode.h"
#include    "fvmscpt.h"
#include    "fmacro.h"
#include    "flisp.h"
#include    "fcompile.h"
#include    "tstruct.h"
#include    "tobjvec.h"
#endif

