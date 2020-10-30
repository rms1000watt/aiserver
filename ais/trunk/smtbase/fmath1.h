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
FMath1.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FMath1
#define _H_FMath1

#include    "tobject.h"

/*  Macro definitions */

#define rand01      ((REAL)abs(rand())/32768.0)

/*  Function declarations */

extern TVAL FMath1_Init						(LpXCONTEXT gCP, LpTHREAD gTP);

extern TVAL FMath1_Divide					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_Minus					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);     
extern TVAL FMath1_Multiply					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_Plus						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath1_Abs						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);       
extern TVAL FMath1_Exp						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);       
extern TVAL FMath1_Log						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);       
extern TVAL FMath1_Log10					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);     
extern TVAL FMath1_Sqrt						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath1_Ceiling					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);       
extern TVAL FMath1_Floor					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);     
extern TVAL FMath1_Expt						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath1_Add1						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath1_Sub1						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath1_Gcd						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_Lcm						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_Even						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_Odd						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_Exact					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_Inexact					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_Modulo					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_Fact						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath1_HashString				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath1_LogBase					(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);   
extern TVAL FMath1_Log2						(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_MakeGramMatrix			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_MakeGaussianMatrix		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_MatrixGaussianEliminate	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_MatrixGaussianSubstitute	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_CIntegerVector			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_CNumberVector			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath1_CStringVector			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#endif

