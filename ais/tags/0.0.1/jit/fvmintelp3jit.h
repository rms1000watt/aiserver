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
FVmIntelP3Jit.h

Implementation of the Virtual Machine Procedure Intel P3 JIT.

This source file contains the main evaluation functions for the Intel P3 
Just-In-Time-Compiler (JIT). This JIT is designed to work hand-in-glove
with the FVmScript_Eval DRM virtual machine emulator.

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FVmIntelP3Jit
#define _H_FVmIntelP3Jit

#include "tlambda.h"
    
/*  Function declarations */
extern TVAL FVmIntelP3Jit_Eval(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* proc, NUM argc, TVAL argv[]);   
extern void FVmIntelP3Jit_RegisterCnt(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM vecIndex,TVAL Rp[],NUM Ir[],NUM maxIr,NUM Nr[],NUM maxNr);

#endif

