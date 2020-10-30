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
FSpecialForms2.h

PARENT:             None 


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FSpecialForms2
#define _H_FSpecialForms2

#include    "tobject.h"
#include    "tpair.h"
#include    "tvector.h"

/*  Macro definitions */

/*  Function declarations */

extern  TVAL    FSpecialForms2_argfetch (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms2_begin    (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms2_case     (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms2_cond     (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms2_if       (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms2_loop     (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms2_onError  (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms2_send     (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms2_super    (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms2_while    (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);

#endif

