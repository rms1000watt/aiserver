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
 
#ifndef _H_FUtil2
#define _H_FUtil2

#include    "tobject.h"
#include    "tstring.h"
#include    "tsymbol.h"

/*  Function declarations */
extern TVAL FUtil2_AddVariable			(LpXCONTEXT gCP,LpTHREAD gTP, TVAL envTval, TSymbol *symName, TVAL value);
extern TVAL FUtil2_BoundVariable		(LpXCONTEXT gCP,LpTHREAD gTP, TVAL anEnvTval, TSymbol *symName);
extern TVAL FUtil2_CongruentProcs		(LpXCONTEXT gCP,LpTHREAD gTP, OBJ target,OBJ source,BOLE advancedMethods);
extern TVAL FUtil2_DefVariable			(LpXCONTEXT gCP,LpTHREAD gTP, TVAL envTval, TSymbol* symbol, TVAL value);
extern TVAL FUtil2_GetVariable			(LpXCONTEXT gCP,LpTHREAD gTP, TVAL envTval,TSymbol *symName);
extern TVAL FUtil2_Init					(LpXCONTEXT gCP,LpTHREAD gTP);
extern TVAL FUtil2_SetVariable			(LpXCONTEXT gCP,LpTHREAD gTP, TVAL envTval,TVAL symTval, TVAL newTval);
extern TVAL FUtil2_SymbolToNativeType	(LpXCONTEXT gCP,LpTHREAD gTP, LpTYPE type, TSymbol *name);

extern TVAL FUtil2_QuickAppend          (LpXCONTEXT gCP,LpTHREAD gTP,LpTVAL first, LpTVAL second);     
extern TVAL FUtil2_Append				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil2_Boundp				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil2_Callcc				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil2_Copy					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil2_Dimension			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FUtil2_Inspect				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);  
extern TVAL FUtil2_Invoke				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);       
extern TVAL FUtil2_Length				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FUtil2_Offset				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil2_Pointer				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil2_Nth					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FUtil2_Quit					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]); 
extern TVAL FUtil2_Ref					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FUtil2_Reset				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);    
extern TVAL FUtil2_Reverse				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);  
extern TVAL FUtil2_Setnth				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil2_Vectorfill			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FUtil2_RefKey				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FUtil2_RefValue				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

#endif

