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
FMake.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FMake
#define _H_FMake

#include "tobject.h"

/*  Macro definitions */

TVAL FMake_Init(LpXCONTEXT gCP, LpTHREAD gTP);

/*  Function declarations */
extern TVAL FMake_BinaryInsert	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_BinarySearch	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_Dictionary	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_List			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_Quote			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_Object		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_Pair			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_QuotedList	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_QuotedSymbol	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);      
extern TVAL FMake_Resize		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_Structure		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_String		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_Substring     (LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_SubstringT    (LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_Symbol		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);        
extern TVAL FMake_UniqueInsert	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_Vector		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern TVAL FMake_Matrix		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
#endif

