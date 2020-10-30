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
FVmScript2.h

Implementation of the Virtual Machine Procedure engine utilities.

This source file contains the main utility functions for the VmScript 
Procedure object. A Procedure object contains a compiled VmScript formula. 
Adopted from Scheme and other dialects of Lisp, it is one of the main 
mechanisms for storing Lisp functions or scripts in SmartLisp.

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FVmScript2
#define _H_FVmScript2

#include    "tsymbol.h"
#include    "tlambda.h"
#include    "tpcodvec.h"
#include    "tstruct.h"

/*  Function declarations */
extern  void FVmScript2_DebugManager	(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* self,NUM argc,TVAL argv[],TPcodeVector* Pc,TStructure* Pv,TStructure* Sv,LpTVAL Rb,LpREAL Vs,NUM VsTopOfStack,LpTVAL Fb,LpNUM Ip,BOLE nestedDbgSwt,TVAL DebugErr);
extern  TVAL FVmScript2_UpdateBindings	(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* self,NUM argc,TVAL argv[],LpTVAL Fb);

#ifdef _C_FVMSCRIPT2
#include    "tstruct.h"
#include    "tlambda.h"
#include    "tbitvec.h"
#include    "tintvec.h"
#include    "tpcodvec.h"
#include    "tbytevec.h"
#include    "tnumvec.h"
#include    "tobjvec.h"
#include    "fmath1.h"
#include    "fdebug.h"
#include    "fvmscpt.h"
#include    "fproc.h"
#include    "futil1.h"
#include    "futil2.h"
#include    "fconvert.h"
#include    "fpropty.h"
#include    "fpred.h"
#include    "fmake.h"
#include    "flisp.h"
#include    "fcompile.h"
#endif
#endif


