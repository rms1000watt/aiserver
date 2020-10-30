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
FUtil2.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FProperty
#define _H_FProperty

#include "tobject.h"

/*  Function declarations */
extern TVAL FProperty_Init			(LpXCONTEXT gCP,LpTHREAD gTP);

extern TVAL FProperty_Proprecord	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);    
extern TVAL FProperty_Proplist		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);  
extern TVAL FProperty_Putprop		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FProperty_Getprop		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FProperty_Remprop		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FProperty_AddMethod		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FProperty_Methodsof		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FProperty_Fieldsof		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);  
extern TVAL FProperty_Factory		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   

#ifdef _C_FPROPERTY
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tstruct.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "futil1.h"
#include    "futil2.h"
#include    "fconvert.h"
#endif
#endif

