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
FVmCode.h

Implementation of the Virtual Machine Pcode Creation Procedures.

This source file contains the main pseudo code generation and test functions 
for the DRM Virtual Machine. An Lambda object contains a compiled VmScript formula. 
Adopted from Scheme and other dialects of Lisp, it is one of the main 
mechanisms for storing Lisp functions or scripts in AIS.

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FVmCode
#define _H_FVmCode

#include "tobject.h"
    
/*  Function declarations */
extern  TVAL FVmCode_Init					(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL FVmCode_ArgLength				(LpXCONTEXT gCP,LpTHREAD gTP, NUM modifier);

/*  Test function declarations */
extern  TVAL FVmCode_GeneratePcode			(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState,TVAL theOpcode,TVAL operand1,TVAL operand2,TVAL operand3);
extern  TVAL FVmCode_CMemoryInteger			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);    
extern  TVAL FVmCode_CPersistInteger		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);    
extern  TVAL FVmCode_CGenericInteger		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);    
extern  TVAL FVmCode_CMemoryReal			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern  TVAL FVmCode_CPersistReal			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern  TVAL FVmCode_CGenericReal			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern  TVAL FVmCode_Testsc					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern  TVAL FVmCode_Testcc					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern  TVAL FVmCode_TestLoop				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#ifdef _C_FVmCode
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tstruct.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "futil1.h"
#include    "futil2.h"
#include    "fconvert.h"
#include    "tbitvec.h"
#include    "tintvec.h"
#include    "tpcodvec.h"
#include    "tbytevec.h"
#include    "tnumvec.h"
#include    "tobjvec.h"
#include    "fvmscpt.h"
#include    "fmath1.h"
#endif
#endif

