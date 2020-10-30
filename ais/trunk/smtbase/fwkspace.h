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
FWkspace.h

Declaration of the procedure library which services the Workspace class.

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FWorkspace
#define _H_FWorkspace

#ifdef _C_FWORKSPACE
#include "tobject.h"
#endif

#include "fsmtbase.h"

/*  Declare the new type variables for this Class.   */

/*  GLOBAL OBJECT DECLARATIONS */

/*  Macro definitions */

/*  Function declarations */
extern  TVAL    FWorkspace_Init					(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FWorkspace_Clear				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
#ifdef _C_FWORKSPACE
#include "fproc.h"
#include "tlambda.h"
#include "tsymbol.h"
#include "twkspace.h"
#include "tdatabas.h"
#include "tneural.h"
#endif
#endif

