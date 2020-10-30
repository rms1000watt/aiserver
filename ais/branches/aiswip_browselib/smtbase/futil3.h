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
Futil3.h

PARENT:             None

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FUtil3
#define _H_FUtil3

#include    "tobject.h"
#include    "tstring.h"
#include    "tsymbol.h"

/*  Function declarations */

extern TVAL FUtil3_Init					(LpXCONTEXT gCP,LpTHREAD gTP);


extern TVAL FUtil3_Assoc			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);    
extern TVAL FUtil3_Assq				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil3_ClearConstraints	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil3_ConstraintEval	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil3_Lock				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FUtil3_Member			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil3_Inside			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil3_Memq				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil3_RefValues		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil3_RefAttributes	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil3_SetAttributes	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil3_Setvar			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FUtil3_VectorDelete		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil3_VectorInsert		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil3_Version			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);  

#endif

