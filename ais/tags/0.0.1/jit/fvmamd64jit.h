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

/*  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AisDev/smtbase/fvmamd64jit.h

CHANGE HISTORY
Version     Date        Who     Change
1.0101      8/01/2006   raj     Initial Changes to AMD 64 JIT
												--------------- ---------------

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

/*!  \file fvmamd64jit.h
\brief This is the AMD 64 JIT declaration file.

 */
 
#ifndef _H_FVmAmd64Jit
#define _H_FVmAmd64Jit

#include "tobject.h"
#include "tlambda.h"
    
/*  Function declarations */
extern TVAL FVmAmd64Jit_Eval(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* proc, NUM argc, TVAL argv[]);   
extern void FVmAmd64Jit_RegisterCnt(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM vecIndex,TVAL Rp[],NUM Ir[],NUM maxIr,NUM Nr[],NUM maxNr);

/* TLW Remarks on changes made to accommodate the Linux compiler.
The Linux compiler objects to *((LpNUM)Np)++ so this code was changed to *(LpNUM)Np = ..., Np += 4;
Actually, there is a better solution.  Write 3 or more assembler macros named loadByte(lhs), loadWord(lhs), loadDWord(lhs) that
optimize *Np++ = lhs  for 1, 2, or 4 bytes.  Then combine multiple 1 byte loads into a single operation that does it in a single
4 byte operation.  The time and space required to load 4 bytes at once is about the same as for 1 byte, so one macro will run 4
times faster than 4 1-byte C instructions (or even faster since the assembler can be optimized over what C can do). */
#endif	// _H_FVmAmd64Jit

