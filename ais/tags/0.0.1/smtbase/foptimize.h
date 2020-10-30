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
FOptimize.h

This include file contains external variable declarations, macro definitions and function prototypes
for the FOptimize.c source file.

PARENT:             None 


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FOptimize
#define _H_FOptimize

#include    "tvector.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tintvec.h"
#include    "tlambda.h"
#include    "tpcodvec.h"

/*  FUNCTION DECLARATIONS */

extern  TVAL    FOptimize_OptimizePcode		(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, NUM argc,TVAL argv[]);

#endif

