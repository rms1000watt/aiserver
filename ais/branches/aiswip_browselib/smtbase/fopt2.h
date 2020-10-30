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
FOptimize2.h

PARENT:             None 


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FOptimize2
#define _H_FOptimize2

#include    "tsymbol.h"
#include    "tpair.h"
#include    "tvector.h"

/*  Global symbol declarations */

/*  Macro definitions */

/*  Function declarations */

extern  TVAL    FOptimize2_AddGoto      (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* aSymbol, NUM location);
extern  TVAL    FOptimize2_AddLabel     (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* aSymbol, NUM  location);
extern  TVAL    FOptimize2_andJmp       (LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair, NUM notsw, NUM cmpsw, 
														 TSymbol* passLbl,TSymbol* branchLbl);
extern  TVAL    FOptimize2_andRet       (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FOptimize2_boolJmp      (LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair, NUM notsw, NUM cmpsw, 
														 TSymbol* passLbl,TSymbol* branchLbl, NUM instruction);
extern  TVAL    FOptimize2_boolRet      (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction);
extern  TVAL    FOptimize2_codeBranch   (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* alternateLbl, NUM genJump);
extern  TVAL    FOptimize2_codePass     (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* defaultLbl, NUM genReturn);
extern  TVAL    FOptimize2_codeResult   (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* passLbl, TSymbol* branchLbl);
extern  TVAL    FOptimize2_Init         (LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL    FOptimize2_LabelSymbol  (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState);
extern  TVAL    FOptimize2_notJmp       (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM notsw, NUM cmpsw, TSymbol* passLbl, 
														 TSymbol* branchLbl);
extern  TVAL    FOptimize2_notRet       (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FOptimize2_orJmp        (LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TPair* curPair, NUM notsw, NUM cmpsw, 
														 TSymbol* passLbl,TSymbol* branchLbl);
extern  TVAL    FOptimize2_orRet        (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FOptimize2_RecognizeHead(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM notsw, NUM cmpsw, TSymbol* passLbl, TSymbol* branchLbl);

extern  TVAL    FOptimize2_Assembler    (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol);
extern  TVAL    FOptimize2_VmMemoryTarget (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol, LpTVAL modifier, LpTVAL target);
extern  TVAL    FOptimize2_VmGotoLabel  (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol, LpTVAL modifier, LpTVAL target);
extern  TVAL    FOptimize2_VmImmediate  (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol, LpTVAL modifier, LpTVAL target);
extern  TVAL    FOptimize2_VmMemoryArg  (LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol, LpTVAL modifier, LpTVAL target);
extern  TVAL    FOptimize2_VmRegisterArg(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol, LpTVAL modifier, LpTVAL target);
#endif

#ifdef  _C_FOPTIMIZE2
#include    "tstruct.h"
#include    "tvector.h"
#include    "tpair.h"
#include    "tsymbol.h"
#include    "tobjvec.h"
#include    "fcompile.h"
#include    "fsforms1.h"
#include    "flisp.h"
#endif

