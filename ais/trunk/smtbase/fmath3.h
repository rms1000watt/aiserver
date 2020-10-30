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
FMath3.h

PARENT:             None 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FMath3
#define _H_FMath3

#include    "tobject.h"

/*  Macro definitions */

#define _FMath3_CONSTANT_PI 3.14159265358979323846264338327950
#define _FMath3_CONSTANT_E  2.7182818284590452353602875
#define _FMath3_RAD_FACTOR  (_FMath3_CONSTANT_PI / 180.0)
#define _FMath3_DEG_FACTOR  (180.0 / _FMath3_CONSTANT_PI)


/*  Function declarations */
extern TVAL FMath3_Init(LpXCONTEXT gCP, LpTHREAD gTP);
extern REAL FMath3_argumentInternal(REAL a,REAL b);      
extern REAL FMath3_sinInternal(REAL x);      
extern REAL FMath3_cosInternal(REAL x);      
extern TVAL FMath3_Acos(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath3_Asin(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath3_Atan(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath3_Real(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_Imaginary(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_Argument(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath3_Conjugate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);      
extern TVAL FMath3_Cos(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);       
extern TVAL FMath3_Sin(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);       
extern TVAL FMath3_Tan(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);       
extern TVAL FMath3_Tanh(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);  
extern TVAL FMath3_Cosh(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);  
extern TVAL FMath3_Sinh(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);  
extern TVAL FMath3_Tan2(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);  
extern TVAL FMath3_Deg(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);       
extern TVAL FMath3_Rad(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);       
extern TVAL FMath3_Pi(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);        
extern TVAL FMath3_Sign(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_Fraction(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_SvmRegression(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorSquareInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorCubeInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorQuartInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorQuintInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorSineInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorCosineInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorTanInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorTanhInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorLogInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorExpInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorSigmoidInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorBinaryInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorBipolarInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
extern TVAL FMath3_VectorDistance(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);    

#endif

