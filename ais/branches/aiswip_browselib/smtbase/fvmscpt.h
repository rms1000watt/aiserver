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
FVmScript.h

Implementation of the Virtual Machine Procedure engine.

This source file contains the main evaluation functions for the VmScript 
Procedure object. A Procedure object contains a compiled VmScript formula. 
Adopted from Scheme and other dialects of Lisp, it is one of the main 
mechanisms for storing Lisp functions or scripts in SmartLisp.

PARENT:             None 

AUTHORS:            Michael F. Korns

CHANGE HISTORY
Version     Date        Who     Change
1.0001      8/03/2006   raj     Added JITNAME for _AMD64
												--------------- ---------------
#endif
 
#ifndef _H_FVmScript
#define _H_FVmScript
#endif

#include "tobject.h"
#include "tlambda.h"
     

#define MAXVECTORCNT  4

/*  Function declarations */

extern TVAL FVmScript_Init(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL FVmScript_Eval(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* proc, NUM argc, TVAL argv[]);   
extern TVAL FVmScript_Emulator(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* proc, NUM argc, TVAL argv[]);   


