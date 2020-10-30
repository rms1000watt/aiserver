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
FDefine.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FDefine
#define _H_FDefine

#include "tobject.h"

/*  Macro definitions */

/*  Function declarations */

extern  TVAL FDefine_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL FDefine_Set		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FDefine_Defstruct	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FDefine_SetKey		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL FDefine_SetValue	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

#endif

#ifdef _C_FDEFINE
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tvector.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "futil1.h"
#include    "futil2.h"
#include    "fconvert.h"
#include    "fpred.h"
#include    "fcontrol.h"
#include	"fmacro.h"
#endif

