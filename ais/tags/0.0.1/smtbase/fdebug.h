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
FCompile.h

PARENT:             None 


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FDebug
#define _H_FDebug

#include    "tsymbol.h"
#include    "tlambda.h"

/*  Function declarations */

extern  TVAL    FDebug_browsableProcs		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FDebug_CreateDebug			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FDebug_Debug				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FDebug_GetGlobalBind		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FDebug_GetLambdaBind			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FDebug_GetGlobalValue		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FDebug_GetSymbolTable		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FDebug_SourceDisassemble	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FDebug_VMDisassemble		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

extern  TVAL    FDebug_Init					(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FDebug_ClearBreakpoints		(LpXCONTEXT gCP,LpTHREAD gTP, TLambda* self,  NUM instruction);
extern  TVAL    FDebug_AMToString			(LpXCONTEXT gCP,LpTHREAD gTP, NUM mod,  LpCHAR result);
extern  TVAL    FDebug_AMToLocString		(LpXCONTEXT gCP,LpTHREAD gTP, NUM mod,  LpCHAR result);
extern  TVAL    FDebug_INSToString			(LpXCONTEXT gCP,LpTHREAD gTP, NUM mod,  LpCHAR result);
extern  TVAL    FDebug_DumpAdd				(LpXCONTEXT gCP,LpTHREAD gTP, NUM format, LpTVAL input, LpCHAR addition);
extern  TVAL    FDebug_VMDisassembleInstruction(LpXCONTEXT gCP,LpTHREAD gTP, TLambda* self, LpNUM vecIndex, LpTVAL dumpTval, NUM toplineDisplay);
extern  TVAL    FDebug_FormatText			(LpXCONTEXT gCP,LpTHREAD gTP, CHAR* theText);
extern  TVAL    FDebug_TStringCat			(LpXCONTEXT gCP,LpTHREAD gTP, LpTVAL input, LpCHAR buf);

#endif

#ifdef  _C_FDEBUG
#include    "tlambda.h"
#include    "tstruct.h"
#include    "tobjvec.h"
#include    "tpcodvec.h"
#include    "twkspace.h"
#include    "flisp.h"
#include    "futil2.h"
#include    "fcompile.h"
#include    "fconio.h"
#include    "fsmtbase.h"
#include    "fproc.h"
#endif

