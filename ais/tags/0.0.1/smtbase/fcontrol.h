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
FControl.h

PARENT:             None 


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FControl
#define _H_FControl

#include "tobject.h"

/*  Macro definitions */

extern TVAL FControl_Init(LpXCONTEXT gCP,LpTHREAD gTP);

/*  Function declarations */

extern TVAL FControl_Mapc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);   
extern TVAL FControl_Map (LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);    

#endif

#ifdef _C_FCONTROL
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
#endif

