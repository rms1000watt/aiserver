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
FProcedure.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FProcedure
#define _H_FProcedure

#ifdef _C_FPROCEDURE
#include "tobject.h"
#endif

#include "fsmtbase.h"
#include "tsymbol.h"

/*  Declare the new type variables for this Class.   */

/*  GLOBAL OBJECT DECLARATIONS */

/*  Macro definitions */

/*  Function declarations */

extern TVAL     FProcedure_LoadCProcedure	(LpXCONTEXT gCP,LpTHREAD gTP,HMChar aHandle, NUM theFileID, NUM bResolve);
extern TVAL     FProcedure_LoadCMacro		(LpXCONTEXT gCP,LpTHREAD gTP,HMChar aHandle, NUM theFileID, NUM bResolve);
extern TVAL     FProcedure_LoadMacro		(LpXCONTEXT gCP,LpTHREAD gTP,HMChar aHandle, NUM theFileID, NUM bResolve);
extern TVAL     FProcedure_Make				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL     FProcedure_MakeMacro		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL     FProcedure_PrintcProcedure	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern TVAL     FProcedure_PrintcMacro		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern TVAL     FProcedure_Procedurep		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL     FProcedure_PrintSpecialForm	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
extern TVAL     FProcedure_Send				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

extern TVAL     FProcedure_cnvDate			(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pSource,LpNUM iChar);
extern TVAL     FProcedure_cnvTime			(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR pSource,LpNUM iChar);
extern void     FProcedure_FailureReset		(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL     FProcedure_NewCProcedure	(LpXCONTEXT gCP,LpTHREAD gTP,TSymbol **symName,LpCHAR funcName,LpFUNC lpFunc);
extern TVAL     FProcedure_NewSpecialForm	(LpXCONTEXT gCP,LpTHREAD gTP,TSymbol **symName, LpCHAR funcName, LpFUNC lpFunc);
extern TVAL     FProcedure_recDate			(LpXCONTEXT gCP,LpTHREAD gTP,HMChar hSource,LpNUM iChar,LpTVAL lpTval);
extern TVAL     FProcedure_recFrac			(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL realResult, LpNUM count, LpCHAR stringInput);
extern TVAL     FProcedure_recInt			(LpXCONTEXT gCP,LpTHREAD gTP,LpREAL realResult, LpNUM count, LpCHAR stringInput);
extern TVAL     FProcedure_recName			(LpXCONTEXT gCP,LpTHREAD gTP,HMChar hSource,LpNUM iChar,TSymbol **symName,BOLE arithSW);
extern TVAL     FProcedure_recNumber		(LpXCONTEXT gCP,LpTHREAD gTP,HMChar hSource,LpNUM iChar,LpREAL aNumber,LpTVAL result);
extern TVAL     FProcedure_recString		(LpXCONTEXT gCP,LpTHREAD gTP,HMChar hSource,LpNUM iChar,LpTVAL lpTval);

#ifdef _C_FPROCEDURE
#include "tcontin.h"
#include "tpair.h"
#include "tvector.h"
#include "tstruct.h"
#include "tlambda.h"
#include "fproc.h"
#include "fmath1.h"
#include "fmath2.h"
#include "futil1.h"
#include "futil2.h"
#include "fconvert.h"
#include "fconio.h"
#include "fpropty.h"
#include "fpred.h"
#include "fvmscpt.h"
#include "fcompile.h"
#endif

#endif

