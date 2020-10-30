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
FUtil1.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FUtil1
#define _H_FUtil1

#include    "tobject.h"
#include    "tstring.h"
#include    "tsymbol.h"
#include    "flisp.h"

/*  Function declarations */

extern TVAL FUtil1_Init			(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL FUtil1_CmpConstants	(LpXCONTEXT gCP,LpTHREAD gTP, TVAL left, TVAL right);

extern TVAL FUtil1_Apply		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);    
extern TVAL FUtil1_Balance		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FUtil1_Cons			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil1_Delete		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil1_Delq			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil1_ErrorTrap	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);    
extern TVAL FUtil1_Eval			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil1_Gc			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil1_ProcedureEnv	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil1_TheEnv		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   

#endif

