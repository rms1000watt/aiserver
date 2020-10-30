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
FSpecialForms3.h

PARENT:             None 


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FSpecialForms3
#define _H_FSpecialForms3

#include    "tobject.h"
#include    "tpair.h"
#include    "tvector.h"


/*  Macro definitions */

/*  Function declarations */
extern  TVAL    FSpecialForms3_addGotoRef	(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval,TVAL newKey,TVAL newValue);
extern  TVAL    FSpecialForms3_Defstruct	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_Defclass 	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_Defmacro		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_Defmethod	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_Defvm    	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_DivEquals	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_goto			(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_gotoCC		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM opcode);
extern  TVAL    FSpecialForms3_label		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_MinusEquals	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_MinusMinus	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_PlusPlus		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_PlusEquals	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_return		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_setf			(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_setq			(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_setv			(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_TimesEquals	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_Defchild 	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_Deforphan 	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_Defriend 	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
extern  TVAL    FSpecialForms3_Defclone 	(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);

extern  TVAL    FSpecialForms3_Macro		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TVAL macro);
extern  TVAL    FSpecialForms3_RefMacro		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair);
#endif


