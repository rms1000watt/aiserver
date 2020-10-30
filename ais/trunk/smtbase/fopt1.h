/**********************************************************************************
    Copyright (C) 2013 Analytic Research Foundation.

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
FOptimize1.h

PARENT:             None 


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FOptimize1
#define _H_FOptimize1

#include    "tsymbol.h"
#include    "tpair.h"
#include    "tvector.h"

/*  Global symbol declarations */


/*  Macro definitions */

/*  Function declarations */

extern  TVAL    FOptimize1_add          (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
extern  TVAL    FOptimize1_arguments    (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
extern  TVAL    FOptimize1_atLeastOneOp (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
extern  TVAL    FOptimize1_Calls        (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol);
extern  TVAL    FOptimize1_div          (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
extern  TVAL    FOptimize1_IndirectCall (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FOptimize1_Init         (LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL    FOptimize1_mul          (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
extern  TVAL    FOptimize1_oneOpAdd     (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM addThis );
extern  TVAL    FOptimize1_ref          (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
extern  TVAL    FOptimize1_set          (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
extern  TVAL    FOptimize1_sub          (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FOptimize1_subi         (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FOptimize1_subu         (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FOptimize1_twoOp        (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
extern  TVAL    FOptimize1_zeroOp       (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
extern  TVAL    FOptimize1_StrongRef    (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
extern  TVAL    FOptimize1_StrongSet    (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
#endif

#ifdef  _C_FOPTIMIZE1
#include    "tvector.h"
#include    "tobjvec.h"
#include    "fcompile.h"
#include    "fsforms1.h"
#include    "fdebug.h"
#include    "fopt2.h"
#include    "flisp.h"
#endif

