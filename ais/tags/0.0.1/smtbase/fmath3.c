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

#define _C_FMATH3
#define _SMARTBASE

#if 0
FMath3.c

This source file contains some of the cProcedures which implement the math functions
supported by the SmartLisp interpreter.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include    "fmath3.h"
#include    "fmath1.h"
#include	"math.h"
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tvector.h"
#include    "tnumvec.h"
#include    "tintvec.h"
#include    "tnummat.h"
#include    "tobjvec.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "fconvert.h"
#include    "ffloat.h"
#include    "futil2.h"
#include	"tcpx.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Init

Initialize the math portion of the SmartLisp function library.  

#endif

TVAL FMath3_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareTVAL(ec);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,alsoSymbol);
EndFrame
 
if(gCP->FMath3_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FMath3_Initialized = TRUE;

// Register the SmartLisp cProcedures contained in this package
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"acos",(LpFUNC)&FMath3_Acos);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"asin",(LpFUNC)&FMath3_Asin);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"atan",(LpFUNC)&FMath3_Atan);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"real",(LpFUNC)&FMath3_Real);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"imaginary",(LpFUNC)&FMath3_Imaginary);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"argument",(LpFUNC)&FMath3_Argument);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"conjugate",(LpFUNC)&FMath3_Conjugate);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cos",(LpFUNC)&FMath3_Cos);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"sin",(LpFUNC)&FMath3_Sin);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"tan",(LpFUNC)&FMath3_Tan);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"tanh",(LpFUNC)&FMath3_Tanh);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cosh",(LpFUNC)&FMath3_Cosh);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"sinh",(LpFUNC)&FMath3_Sinh);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"tan2",(LpFUNC)&FMath3_Tan2);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"deg",(LpFUNC)&FMath3_Deg);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"rad",(LpFUNC)&FMath3_Rad);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"pi",(LpFUNC)&FMath3_Pi);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"sign",(LpFUNC)&FMath3_Sign);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fraction",(LpFUNC)&FMath3_Fraction);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"svmRegression",(LpFUNC)&FMath3_SvmRegression);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorInnerProduct",(LpFUNC)&FMath3_VectorInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorDistance",(LpFUNC)&FMath3_VectorDistance);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorSquareInnerProduct",(LpFUNC)&FMath3_VectorSquareInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorCubeInnerProduct",(LpFUNC)&FMath3_VectorCubeInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorQuartInnerProduct",(LpFUNC)&FMath3_VectorQuartInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorQuintInnerProduct",(LpFUNC)&FMath3_VectorQuintInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorSineInnerProduct",(LpFUNC)&FMath3_VectorSineInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorCosineInnerProduct",(LpFUNC)&FMath3_VectorCosineInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorTanInnerProduct",(LpFUNC)&FMath3_VectorTanInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorTanhInnerProduct",(LpFUNC)&FMath3_VectorTanhInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorLogInnerProduct",(LpFUNC)&FMath3_VectorLogInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorExpInnerProduct",(LpFUNC)&FMath3_VectorExpInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorSigmoidInnerProduct",(LpFUNC)&FMath3_VectorSigmoidInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorBinaryInnerProduct",(LpFUNC)&FMath3_VectorBinaryInnerProduct);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"vectorBipolarInnerProduct",(LpFUNC)&FMath3_VectorBipolarInnerProduct);
ExitOnError(*ec);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_argumentInternal

Return the argument of a complex number. 

#endif

REAL FMath3_argumentInternal(REAL a, REAL b)      
{
REAL              y;

/* Correct for any rounding errors at the limits. */

if (a == 0.0) 
	{
	if (b == 0.0)
		{
		y = 0.0;
		}
	if (b > 0.0)
		{
		y = (_FMath3_CONSTANT_PI/2);
		}
	if (b < 0.0)
		{
		y = (-_FMath3_CONSTANT_PI/2);
		}
	}
else
if (b == 0.0) 
	{
	if (a == 0.0)
		{
		y = 0.0;
		}
	if (a < 0.0)
		{
		y = _FMath3_CONSTANT_PI;
		}
	if (a > 0.0)
		{
		y = 0.0;
		}
	}
else
	{
	/* Return the angle of the complex argument expressed in radians. */
	if (a > 0.0) y = atan(b/a);
	else
	y = atan(b/a) + _FMath3_CONSTANT_PI;

	}

return(y);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_sinInternal

Return the sine of a real argument. 

Note:   This routine attempts to correct for any rounding errors in the C runtime library. 
  
#endif

REAL FMath3_sinInternal(REAL x)      
{
REAL              y;
REAL              z;

/* Call the C runtime library for sine and cosine. */

y = sin(x);
z = abs(cos(x));
 
/* Correct for any rounding errors at the limits. */

if (z == 1.0) y = 0.0;
if ((z == 0.0) && (y < 0.0)) y = -1.0;
if ((z == 0.0) && (y > 0.0)) y = 1.0;

return(y);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_cosInternal

Return the cosine of a real argument. 

Note:   This routine attempts to correct for any rounding errors in the C runtime library. 
  
#endif

REAL FMath3_cosInternal(REAL x)      
{
REAL              y;
REAL              z;

/* Call the C runtime library for sine and cosine. */

y = cos(x);
z = abs(sin(x));
 
/* Correct for any rounding errors at the limits. */

if (z == 1.0) y = 0.0;
else
if ((z == 0.0) && (y < 0.0)) y = -1.0;
else
if ((z == 0.0) && (y > 0.0)) y = 1.0;

return(y);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Acos

Return the inverse cosine of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error.

-i*log(ix+sqrt(1-(x*x)))  
  
#endif

TVAL FMath3_Acos(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
REAL              x;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareTVAL(cx);
DeclareTVAL(cxSq);
DeclareTVAL(one);
DeclareTVAL(i);
DeclareTVAL(mi);
DeclareTVALArray(prmv,3);

EndFrame
 
asTag(ret) = TYREAL;
asReal(ret) = 0.0;
if (argc != 1) {FrameReset;return gCP->TObject_ERROR_INVALID_ARGLIST;}

if (asTag(&argv[0]) == TYCPX)
    {
	/* -i*log(x+i*sqrt(1-(x*x)))  */
    i->Tag = TYCPX;
    i->u.Complex = TCpx_New(gCP,gTP);
    i->u.Complex->itsReal = 0;
    i->u.Complex->itsImag = 1;
    
    mi->Tag = TYCPX;
    mi->u.Complex = TCpx_New(gCP,gTP);
    mi->u.Complex->itsReal = 0;
    mi->u.Complex->itsImag = -1;
    
    one->Tag = TYCPX;
    one->u.Complex = TCpx_New(gCP,gTP);
    one->u.Complex->itsReal = 1;
    one->u.Complex->itsImag = 0;
    
	prmv[1] = prmv[0] = *cx = argv[0];
	*cxSq = FMath1_Multiply(gCP,gTP,2,&prmv[0]);
	ExitOnError(*cxSq);
	
	prmv[0] = *one;
	prmv[1] = *cxSq;
	*tmp = FMath1_Minus(gCP,gTP,2,&prmv[0]);
	ExitOnError(*tmp);
	*tmp = FMath1_Sqrt(gCP,gTP,1,tmp);
	ExitOnError(*tmp);
	
	prmv[0] = *i;
	prmv[1] = *tmp;
	*tmp = FMath1_Multiply(gCP,gTP,2,&prmv[0]);
	ExitOnError(*tmp);

	prmv[0] = *cx;
	prmv[1] = *tmp;
	*tmp = FMath1_Plus(gCP,gTP,2,&prmv[0]);
	ExitOnError(*tmp);
	*tmp = FMath1_Log(gCP,gTP,1,tmp);
	ExitOnError(*tmp);
	
	prmv[0] = *mi;
	prmv[1] = *tmp;
	*ret = FMath1_Multiply(gCP,gTP,2,&prmv[0]);
    FrameExit(*ret);
    }
else
if (asTag(&argv[0]) != TYREAL)
    {
    *tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    ExitOnError( *tmp);
    x = asNumIndex(tmp);
    asReal(ret) = acos(x);
    FrameExit(*ret);
    }
else
    {
    asReal(ret) = acos(asNumIndex(&argv[0]));
    FrameExit(*ret);
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Asin

Return the inverse sine of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Asin(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
REAL              x;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareTVAL(cx);
DeclareTVAL(cxSq);
DeclareTVAL(one);
DeclareTVAL(i);
DeclareTVAL(mi);
DeclareTVAL(ix);
DeclareTVALArray(prmv,3);

EndFrame
 
asTag(ret) = TYREAL;
asReal(ret) = 0.0;
if (argc != 1) {FrameReset;return gCP->TObject_ERROR_INVALID_ARGLIST;}

if (asTag(&argv[0]) == TYCPX)
    {
	/* -i*log(ix+sqrt(1-(x*x))) */
    i->Tag = TYCPX;
    i->u.Complex = TCpx_New(gCP,gTP);
    i->u.Complex->itsReal = 0;
    i->u.Complex->itsImag = 1;
    
    mi->Tag = TYCPX;
    mi->u.Complex = TCpx_New(gCP,gTP);
    mi->u.Complex->itsReal = 0;
    mi->u.Complex->itsImag = -1;
    
    one->Tag = TYCPX;
    one->u.Complex = TCpx_New(gCP,gTP);
    one->u.Complex->itsReal = 1;
    one->u.Complex->itsImag = 0;
    
	prmv[1] = prmv[0] = *cx = argv[0];
	*cxSq = FMath1_Multiply(gCP,gTP,2,&prmv[0]);
	ExitOnError(*cxSq);
	
	prmv[0] = *i;
	prmv[1] = *cx;
	*ix = FMath1_Multiply(gCP,gTP,2,&prmv[0]);
	ExitOnError(*ix);
	
	prmv[0] = *one;
	prmv[1] = *cxSq;
	*tmp = FMath1_Minus(gCP,gTP,2,&prmv[0]);
	ExitOnError(*tmp);
	*tmp = FMath1_Sqrt(gCP,gTP,1,tmp);
	ExitOnError(*tmp);
	
	prmv[0] = *ix;
	prmv[1] = *tmp;
	*tmp = FMath1_Plus(gCP,gTP,2,&prmv[0]);
	ExitOnError(*tmp);
	*tmp = FMath1_Log(gCP,gTP,1,tmp);
	ExitOnError(*tmp);
	
	prmv[0] = *mi;
	prmv[1] = *tmp;
	*ret = FMath1_Multiply(gCP,gTP,2,&prmv[0]);
    FrameExit(*ret);
    }
else
if (asTag(&argv[0]) != TYREAL)
    {
    *tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    ExitOnError( *tmp);
    x = asNumIndex(tmp);
    asReal(ret) = asin(x);
    FrameExit(*ret);
    }
else
    {
    asReal(ret) = asin(asNumIndex(&argv[0]));
    FrameExit(*ret);
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Atan

Return the arctangent of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Atan(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
REAL              x;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareTVAL(cx);
DeclareTVAL(ipx);
DeclareTVAL(imx);
DeclareTVAL(two);
DeclareTVAL(i);
DeclareTVAL(ihalf);
DeclareTVALArray(prmv,3);

EndFrame
 
asTag(ret) = TYREAL;
asReal(ret) = 0.0;
if (argc != 1) {FrameReset;return gCP->TObject_ERROR_INVALID_ARGLIST;}

if (asTag(&argv[0]) == TYCPX)
    {
	/* (i/2)*log((i+x)/(i-x))  */
    i->Tag = TYCPX;
    i->u.Complex = TCpx_New(gCP,gTP);
    i->u.Complex->itsReal = 0;
    i->u.Complex->itsImag = 1;
    
    two->Tag = TYCPX;
    two->u.Complex = TCpx_New(gCP,gTP);
    two->u.Complex->itsReal = 2;
    two->u.Complex->itsImag = 0;
    
	prmv[0] = *i;
	prmv[1] = *two;
	*ihalf = FMath1_Divide(gCP,gTP,2,&prmv[0]);
	ExitOnError(*ihalf);
	
	prmv[0] = *i;
	prmv[1] = *cx = argv[0];
	*ipx = FMath1_Plus(gCP,gTP,2,&prmv[0]);
	ExitOnError(*ipx);
	*imx = FMath1_Minus(gCP,gTP,2,&prmv[0]);
	ExitOnError(*imx);

	prmv[0] = *ipx;
	prmv[1] = *imx;
	*tmp = FMath1_Divide(gCP,gTP,2,&prmv[0]);
	ExitOnError(*tmp);
	*tmp = FMath1_Log(gCP,gTP,1,tmp);
	ExitOnError(*tmp);
	
	prmv[0] = *ihalf;
	prmv[1] = *tmp;
	*ret = FMath1_Multiply(gCP,gTP,2,&prmv[0]);
    FrameExit(*ret);
    }
else
if (asTag(&argv[0]) != TYREAL)
    {
    *tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    ExitOnError( *tmp);
    x = asReal(tmp);
    asReal(ret) = atan(x);
    FrameExit(*ret);
    }
else
    {
    asReal(ret) = atan(asNumIndex(&argv[0]));
    FrameExit(*ret);
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Real

Return the real component of a complex argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Real(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
/* Initialize output and check for errors in input arguments. */
if ((argc != 1) || (argv[0].Tag != TYCPX)) {FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);}

ret->Tag = TYREAL;
ret->u.Real = argv[0].u.Complex->itsReal;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Imaginary

Return the imaginary component of a complex argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Imaginary(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
/* Initialize output and check for errors in input arguments. */
if ((argc != 1) || (argv[0].Tag != TYCPX)) {FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);}

ret->Tag = TYREAL;
ret->u.Real = argv[0].u.Complex->itsImag;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Argument

Return the arctangent of the imaginary/real ratio of a complex argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Argument(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TCpx,self);
EndFrame
 
/* Initialize output and check for errors in input arguments. */
if ((argc != 1) || (argv[0].Tag != TYCPX)) {FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);}

self = argv[0].u.Complex;
ret->Tag = TYREAL;
ret->u.Real = FMath3_argumentInternal(self->itsReal, self->itsImag);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Conjugate

Return the conjugate of a complex argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Conjugate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
/* Initialize output and check for errors in input arguments. */
ret->Tag = TYREAL;
ret->u.Real = 0.0;
if ((argc != 1) || (argv[0].Tag != TYCPX)) {FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);}
*ret = FUtil2_Copy(gCP,gTP,1,&argv[0]);

/* Return the conjugate of the complex argument expressed in radians. */
ret->u.Complex->itsImag = -ret->u.Complex->itsImag;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Tan2

FMath3_Tan2 computes the value of the arc tangent of (y/x).
It uses the signs of both arguments to determine the quadrant 
of the return value.

Note:   Exactly two arguments are expected, anything else is an error. 
  
#endif

TVAL FMath3_Tan2(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareTVALArray(prmv,3);

EndFrame
 
asTag(ret) = TYREAL;
asReal(ret) = 0.0;
if (argc != 2) {FrameReset;return gCP->TObject_ERROR_INVALID_ARGLIST;}

prmv[0] = argv[0];
prmv[1] = argv[1];
*tmp = FMath1_Divide(gCP,gTP,2,&prmv[0]);
ExitOnError(*tmp);

*ret = FMath3_Tan(gCP,gTP,1,tmp);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Tanh

Return the hyperbolic tangent of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Tanh(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
REAL			x;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);
DeclareTVALArray(prmv,3);

EndFrame
 
ret->Tag = TYREAL;
ret->u.Real = 0.0;
if (argc != 1) {FrameReset;return gCP->TObject_ERROR_INVALID_ARGLIST;}

if (argv[0].Tag == TYCPX)
{	prmv[0] = FMath3_Sinh(gCP,gTP,1,&argv[0]);
	ExitOnError(prmv[0]);
	prmv[1] = FMath3_Cosh(gCP,gTP,1,&argv[0]);
	ExitOnError(prmv[0]);
	*ret = FMath1_Divide(gCP,gTP,2,&prmv[0]);
	FrameExit(*ret);
}

if (argv[0].Tag != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError( *tmp);
	x = tmp->u.Real;
}
else
	x = argv[0].u.Real;

ret->u.Real = tanh(x);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Cosh

Return the hyperbolic cosine of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Cosh(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
REAL			x;
REAL			a;
REAL			b;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);
EndFrame
 
ret->Tag = TYREAL;
ret->u.Real = 0.0;
if (argc != 1) {FrameReset;return gCP->TObject_ERROR_INVALID_ARGLIST;}

if (argv[0].Tag == TYCPX)
{	a = argv[0].u.Complex->itsReal;
	b = argv[0].u.Complex->itsImag;
	ret->Tag = TYCPX;
	ret->u.Complex = cp = TCpx_New(gCP, gTP);
	ret->u.Complex->itsReal = cosh(a)*FMath3_cosInternal(b);
	ret->u.Complex->itsImag = sinh(a)*FMath3_sinInternal(b);
	FrameExit(*ret);
}

if (argv[0].Tag != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError( *tmp);
	x = tmp->u.Real;
}
else
	x = argv[0].u.Real;

ret->u.Real = cosh(x);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Sinh

Return the hyperbolic sine of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Sinh(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
REAL			x;
REAL			a;
REAL			b;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);
EndFrame
 
ret->Tag = TYREAL;
ret->u.Real = 0.0;
if (argc != 1) {FrameReset;return gCP->TObject_ERROR_INVALID_ARGLIST;}

if (argv[0].Tag == TYCPX)
{	a = argv[0].u.Complex->itsReal;
	b = argv[0].u.Complex->itsImag;
	ret->Tag = TYCPX;
	ret->u.Complex = cp = TCpx_New(gCP, gTP);
	ret->u.Complex->itsReal = sinh(a)*FMath3_cosInternal(b);
	ret->u.Complex->itsImag = cosh(a)*FMath3_sinInternal(b);
	FrameExit(*ret);
}

if (argv[0].Tag != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError( *tmp);
	x = tmp->u.Real;
}
else
	x = argv[0].u.Real;

ret->u.Real = sinh(x);
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Cos

Return the cosine of an angle in radians. 
Note:   Exactly one argument is expected, anything else is an error. 
		For complex numbers:
			cos (a + ib) = (cos(a)*cosh(b)) - (sin(a)*sinh(b))i
#endif

TVAL FMath3_Cos(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL			x;
REAL			a;
REAL			b;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);
EndFrame
 
ret->Tag = TYREAL;
ret->u.Real = 0.0;
if (argc != 1) {FrameReset;return gCP->TObject_ERROR_INVALID_ARGLIST;}

if (argv[0].Tag == TYCPX)
{	a = argv[0].u.Complex->itsReal;
	b = argv[0].u.Complex->itsImag;
	ret->Tag = TYCPX;
	ret->u.Complex = cp = TCpx_New(gCP, gTP);
	ret->u.Complex->itsReal = FMath3_cosInternal(a)*cosh(b);
	ret->u.Complex->itsImag = -(FMath3_sinInternal(a)*sinh(b));
	FrameExit(*ret);
}

if (argv[0].Tag != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError( *tmp);
	x = tmp->u.Real;
}
else
	x = argv[0].u.Real;

ret->u.Real = FMath3_cosInternal(x);
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Sin
Return the sine of an angle in radians.

Note:   Exactly one argument is expected, anything else is an error.
		For complex numbers:
			sin (x + iy) = (sin x cosh iy + icos x sinh iy)
#endif

TVAL FMath3_Sin(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL			x;
REAL			a;
REAL			b;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);
EndFrame
 
ret->Tag = TYREAL;
ret->u.Real = 0.0;
if (argc != 1) {FrameReset;return gCP->TObject_ERROR_INVALID_ARGLIST;}

if (argv[0].Tag == TYCPX)
{	a = argv[0].u.Complex->itsReal;
	b = argv[0].u.Complex->itsImag;
	ret->Tag = TYCPX;
	ret->u.Complex = cp = TCpx_New(gCP, gTP);
	ret->u.Complex->itsReal = FMath3_sinInternal(a)*cosh(b);
	ret->u.Complex->itsImag = FMath3_cosInternal(a)*sinh(b);
	FrameExit(*ret);
}

if (argv[0].Tag != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError( *tmp);
	x = tmp->u.Real;
}
else
	x = argv[0].u.Real;

ret->u.Real = FMath3_sinInternal(x);
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Tan

Return the tangent of an angle in radians  radians = pi * degrees / 180

Note:   Exactly one argument is expected, anything else is an error. 
		For complex numbers:
			tan(z) = sin(z)/cos(z)
#endif
TVAL FMath3_Tan(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL			x;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);
DeclareTVALArray(prmv,3);

EndFrame
 
ret->Tag = TYREAL;
ret->u.Real = 0.0;
if (argc != 1) {FrameReset;return gCP->TObject_ERROR_INVALID_ARGLIST;}

if (argv[0].Tag == TYCPX)
{	prmv[0] = FMath3_Sin(gCP,gTP,1,&argv[0]);
	ExitOnError(prmv[0]);
	prmv[1] = FMath3_Cos(gCP,gTP,1,&argv[0]);
	ExitOnError(prmv[0]);
	*ret = FMath1_Divide(gCP,gTP,2,&prmv[0]);
	FrameExit(*ret);
}

if (argv[0].Tag != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError( *tmp);
	x = tmp->u.Real;
}
else
	x = argv[0].u.Real;

ret->u.Real = sin(x)/cos(x);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Deg

Takes an existing number and converts it into degrees by the
conversion = x (radian) * (180 (degree)/PI())
Assumes the argument argv[0] is in radians.

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Deg(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL                x;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame
 
asTag(ret) = TYREAL;
asReal(ret) = 0.0;
if (argc != 1) 
    {
    FrameExit (gCP->TObject_ERROR_INVALID_ARGLIST);
    }

if (asTag(&argv[0]) != TYREAL)
    {
    *tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    ExitOnError( *tmp);
    x = asNumIndex(tmp);
    }
else
    {
    x = asNumIndex(&argv[0]);
    }
    
asReal(ret) = (x * _FMath3_DEG_FACTOR);
asTag(ret) = TYREAL;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Rad

Takes an existing number and converts it into radian by the
conversion = x degree * (PI radians / 180 degrees))
Assumes the argument argv[0] is in degrees.

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Rad(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL                x;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame
 
asTag(ret) = TYREAL;
asReal(ret) = 0.0;
if (argc != 1) 
    {
    FrameExit (gCP->TObject_ERROR_INVALID_ARGLIST);
    }

if (asTag(&argv[0]) != TYREAL)
    {
    *tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    ExitOnError( *tmp);
    x = asNumIndex(tmp);
    }
else
    {
    x = asNumIndex(ret);
    }
    
asReal(ret) = (x * _FMath3_RAD_FACTOR);
asTag(ret) = TYREAL;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Pi

Returns the mathematic constant value of PI.

Note:   Exactly no arguments are expected, anything else is an error. 
  
#endif

TVAL FMath3_Pi(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
argv = argv; // NOOP to hide unused parameter warning message
if (argc != 0) 
    {
    FrameExit (gCP->TObject_ERROR_INVALID_ARGLIST);
    }

asReal(ret) = _FMath3_CONSTANT_PI;
asTag(ret) = TYREAL;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Sign

Returns the sign of the specified argument
.
Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Sign(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
EndFrame

if (argc != 1) 
	{
	*ret = TERROR("!sign: Expecting one argument!");
	FrameExit(*ret);
	}


/*  RETRIEVE AND VERIFY THE SPECIFIED ARGUMENTS FROM ARGV */

ret->Tag = TYREAL;
switch(argv[0].Tag)
    {
    case TYCHAR:
        if (argv[0].u.Char == 0)
            ret->u.Real = 0;
        else
            ret->u.Real = 1;
        break;
        
    case TYNUM:
        if (argv[0].u.Int == 0)
            ret->u.Real = 0;
        else
        if (argv[0].u.Int < 0)
            ret->u.Real = -1;
        else
            ret->u.Real = 1;
        break;
        
    case TYCPX:
        if (argv[0].u.Complex->itsReal == 0)
            ret->u.Real = 0;
        else
        if (argv[0].u.Complex->itsReal < 0)
            ret->u.Real = -1;
        else
            ret->u.Real = 1;
        break;
        
    case TYDATE:
    case TYMONEY:
    case TYREAL:
        if (argv[0].u.Real == 0)
            ret->u.Real = 0;
        else
        if (argv[0].u.Real < 0)
            ret->u.Real = -1;
        else
            ret->u.Real = 1;
        break;
        
    default:
		*ret = TERROR("!sign: Argument must be numeric!");
        break;
    }
    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Fraction

Returns the fraction part of the specified argument
.
Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Fraction(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
REAL        x;
StartFrame
DeclareTVAL(ret);
EndFrame

if (argc != 1) 
	{
	*ret = TERROR("!fraction: Expecting one argument!");
	FrameExit(*ret);
	}

/*  RETRIEVE AND VERIFY THE SPECIFIED ARGUMENTS FROM ARGV */

ret->Tag = TYREAL;
switch(argv[0].Tag)
    {
    case TYCHAR:
    case TYNUM:
        ret->u.Real = 0;
        break;
        
    case TYDATE:
    case TYMONEY:
    case TYREAL:
        ret->u.Real = modf(argv[0].u.Real,&x);
        break;
        
    case TYCPX:
        ret->u.Real = modf(argv[0].u.Complex->itsReal,&x);
        break;
        
    default:
		*ret = TERROR("!fraction: Expecting a numeric argument!");
        break;
    }
    
FrameExit(*ret);
}

#if 0
/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Atanh

Return the hyperbolic arc-tangent of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Atanh(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
REAL                x;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

asTag(ret) = TYREAL;
asReal(ret) = 0.0;
if (argc != 1) 
	{
	*ret = TERROR("!atanh: Expecting one argument!");
	FrameExit(*ret);
	}

if (asTag(&argv[0]) != TYREAL)
    {
    *tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    ExitOnError(*tmp);
    x = asNumIndex(tmp);
    }
else
    {
    x = asNumIndex(ret);
    }
    
if (x == 1.0) 
    {
    FrameExit(gCP->TObject_ERROR_DIVIDE_BY_ZERO);
    }

asReal(ret) = log((1.0 + x) / (1.0 - x)) * 0.5;
asTag(ret) = TYREAL;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Asinh

Return the hyperbolic arc-sine of a numeric argument. 
Assumes the argument argv[0] is in radians.

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Asinh(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
REAL                x;
REAL                u;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame
 
asTag(ret) = TYREAL;
asReal(ret) = 0.0;
if (argc != 1) 
	{
	*ret = TERROR("!asinh: Expecting one argument!");
	FrameExit(*ret);
	}

if (asTag(&argv[0]) != TYREAL)
    {
    *tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    ExitOnError( *tmp);
    x = asNumIndex(tmp);
    }
else
    {
    x = asNumIndex(ret);
    }
    
u = sqrt((x * x) + 1.0);
asReal(ret) = log(x + u);
asTag(ret) = TYREAL;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_Acosh

Return the hyperbolic arc-cos of a numeric argument. 
Assumes the argument argv[0] is in radians.

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath3_Acosh(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
REAL                x;
REAL                u;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame
 
asTag(ret) = TYREAL;
asReal(ret) = 0.0;
if (argc != 1) 
	{
	*ret = TERROR("!acosh: Expecting one argument!");
	FrameExit(*ret);
	}

if (asTag(&argv[0]) != TYREAL)
    {
    *tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    ExitOnError( *tmp);
    x = asNumIndex(tmp);
    }
else
    {
    x = asNumIndex(ret);
    }
    
u = sqrt((x * x) - 1.0);
asReal(ret) = log(x + u);
asTag(ret) = TYREAL;

FrameExit(*ret);
}
#endif

/*--------------------------------------------------------------------------------------- */
/* Start of Support Vector Machine Training, Output, and Kernel Functions				  */
/*--------------------------------------------------------------------------------------- */


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorInnerProduct

Returns the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The inner product of the two input vectors. 

Example:	(svmRegression X Y vectorInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".

#endif

TVAL FMath3_VectorInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the inner product of the two vectors. */
result.u.Real = innerProduct;
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorDistance

Returns the distance between the two specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: D		     The distance between the two input vectors. 

References:	[1] Christianini, "Support Vector Machines".

#endif

TVAL FMath3_VectorDistance(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                difference;
REAL				df;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorDistance: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorDistance: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorDistance: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the componentwise difference of the two vectors. */

difference = 0;
for (n = 0; n < N; ++n) 
	{
	df = (XPtr[n]-YPtr[n]);
	difference += (df*df);
	}

/* Return the distance of the two vectors. */
result.u.Real = sqrt(difference);
result.Tag = TYREAL;
return(result);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorSquareInnerProduct

Returns the square of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The square of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorSquareInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".

Notes: The formula used is,  (1 + innerProduct)*(1 + innerProduct)
	   which finds all 2nd order polynomial patterns of up to power 2 in length.
#endif

TVAL FMath3_VectorSquareInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorSquareInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorSquareInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorSquareInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the square of the inner product of the two vectors. */
result.u.Real = (1 + innerProduct) * (1 + innerProduct);
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorCubeInnerProduct

Returns the cube of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The cube of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorCubeInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".

Notes: The formula used is,  (1 + innerProduct)*(1 + innerProduct)*(1 + innerProduct)
	   which finds all 3rd order polynomial patterns of up to power 3 in length.
#endif

TVAL FMath3_VectorCubeInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorCubeInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorCubeInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorCubeInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the cube of the inner product of the two vectors. */
result.u.Real = (1 + innerProduct) * (1 + innerProduct) * (1 + innerProduct);
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorQuartInnerProduct

Returns the quart of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The quart of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorQuartInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".

Notes: The formula used is,  (1 + innerProduct)*(1 + innerProduct)*(1 + innerProduct)*(1 + innerProduct)
	   which finds all 4th order polynomial patterns of up to power 4 in length.
#endif

TVAL FMath3_VectorQuartInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorQuartInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorQuartInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorQuartInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the quart of the inner product of the two vectors. */
result.u.Real = (1 + innerProduct) * (1 + innerProduct) * (1 + innerProduct) * (1 + innerProduct);
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorQuintInnerProduct

Returns the quint of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The quint of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorQuintInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".

Notes: The formula used is,  (1 + innerProduct)*(1 + innerProduct)*(1 + innerProduct)*(1 + innerProduct)*(1 + innerProduct)
	   which finds all 5th order polynomial patterns of up to power 5 in length.
#endif

TVAL FMath3_VectorQuintInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorQuintInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorQuintInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorQuintInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the quint of the inner product of the two vectors. */
result.u.Real = (1 + innerProduct) * (1 + innerProduct) * (1 + innerProduct) * (1 + innerProduct) * (1 + innerProduct);
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorSineInnerProduct

Returns the sine of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The sine of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorSineInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".

Notes: The formula used is,  sin(innerProduct)
	   which produces a periodic output in the range of [-pi,+pi].
#endif

TVAL FMath3_VectorSineInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorSineInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorSineInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorSineInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the sine of the inner product of the two vectors. */
result.u.Real = FMath3_sinInternal(innerProduct);
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorCosineInnerProduct

Returns the cosine of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The cosine of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorCosineInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".

Notes: The formula used is,  cos(innerProduct)
	   which produces a periodic output in the range of [-pi,+pi].
#endif

TVAL FMath3_VectorCosineInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorCosineInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorCosineInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorCosineInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the cosine of the inner product of the two vectors. */
result.u.Real = FMath3_cosInternal(innerProduct);
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorTanInnerProduct

Returns the tangent of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The tangent of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorTanInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".

Notes: The formula used is,  tan(innerProduct)
	   which produces a periodic output in the range of [-pi,+pi].
#endif

TVAL FMath3_VectorTanInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorTanInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorTanInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorTanInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the tangent of the inner product of the two vectors. */
result.u.Real = sin(innerProduct)/cos(innerProduct);
result.Tag = TYREAL;
return(result);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorTanhInnerProduct

Returns the hyperbolic tangent of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The hyperbolic tangent of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorTanhInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".

Notes: The formula used is,  tanh(innerProduct)
	   which produces a symmetric output in the range of [-1,+1].
#endif

TVAL FMath3_VectorTanhInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorTanhInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorTanhInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorTanhInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the hyperbolic tangent of the inner product of the two vectors. */
result.u.Real = tanh(innerProduct);
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorLogInnerProduct

Returns the log of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The log of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorLogInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".

Notes: The formula used is,  log(abs(innerProduct)+1)
	   which produces a hyperbolic output in the range of [+inf,0,+inf].
#endif

TVAL FMath3_VectorLogInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorLogInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorLogInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorLogInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the log of the inner product of the two vectors. */
result.u.Real = log(abs(innerProduct)+1);
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorExpInnerProduct

Returns the exponential of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The exponential of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorExpInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".


Notes: The formula used is,  (2.0 / (1.0 + exp(-innerProduct))) - 1
	   which produces a symmetric output in the range of [-1,+1].
#endif

TVAL FMath3_VectorExpInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorExpInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorExpInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorExpInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the exponential of the inner product of the two vectors. */
result.u.Real = (2.0 / (1.0 + exp(-innerProduct))) - 1;
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorSigmoidInnerProduct

Returns the exponential sigmoid of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The exponential sigmoid of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorSigmoidInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".


Notes: The formula used is,  (1.0 / (1.0 + exp(-innerProduct)))
	   which produces a sigmoid output in the range of [0,1].
#endif

TVAL FMath3_VectorSigmoidInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorExpInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorSigmoidInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorSigmoidInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the exponential sigmoid of the inner product of the two vectors. */
result.u.Real = (1.0 / (1.0 + exp(-innerProduct)));
result.Tag = TYREAL;
return(result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorBinaryInnerProduct

Returns the exponential binary of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The exponential of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorBinaryInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".


Notes: The formula used is,  ((2.0 / (1.0 + exp(-innerProduct))) - 1) > 0 ? 1 : 0;
	   which produces a binary output  of [0 or 1].
#endif

TVAL FMath3_VectorBinaryInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorBinaryInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorBinaryInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorBinaryInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the exponential of the inner product of the two vectors. */
result.u.Real = ((2.0 / (1.0 + exp(-innerProduct))) - 1) > 0 ? 1 : 0;
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_VectorBipolarInnerProduct

Returns the exponential bipolar of the inner product the specified two number vector inputs.

Args:    x:          A number vector of length N.
		 y:   		 A number vector of length N.
 
Returns: R		     The exponential of the inner product of the two input vectors. 

Example:	(svmRegression X Y vectorBipolarInnerProduct ETollerance maxErr maxGen printSW)


References:	[1] Christianini, "Support Vector Machines".


Notes: The formula used is,  ((2.0 / (1.0 + exp(-innerProduct))) - 1) > -1 ? 1 : 0;
	   which produces a bipolar output of [-1 or +1].
#endif

TVAL FMath3_VectorBipolarInnerProduct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                N;
NUM	                n;
REAL                innerProduct;
LpREAL				YPtr;
LpREAL				XPtr;
TVAL				result;
 
/* Validate and setup the user supplied arguments. */
if (argc != 2) 
	{
	result = TERROR("!vectorExpInnerProduct: Expecting two arguments!");
	return(result);
	}

if ((argv[0].Tag != TYNUMVECTOR) ||
	((N = NumVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = RealArray(argv[0])) == NULL))
    {
	result = TERROR("!vectorBipolarInnerProduct: Expecting first argument to be a number vector of length N!");
	return(result);
    }

if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	result = TERROR("!vectorBipolarInnerProduct: Expecting second argument to be a number vector of length N!");
	return(result);
    }

/* Compute the inner product of the two vectors. */

innerProduct = 0;
for (n = 0; n < N; ++n) 
	{
	innerProduct += (XPtr[n]*YPtr[n]);
	}

/* Return the exponential of the inner product of the two vectors. */
result.u.Real = ((2.0 / (1.0 + exp(-innerProduct))) - 1) > -1 ? 1 : 0;
result.Tag = TYREAL;
return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3__computeError

Return the regression error for the currently proposed changes in the dual coefficients.

Args:    kernel:		The kernel to use during training.
		 N:				The number of training points in X.
		 worstN2:		The training point with the worst percentage error
		 ETollerance:	The support vector machine error tollerance.
		 WPtr:			Pointer to the real array of weights to be trained.
		 XPtr:			Pointer to the X object array of number vectors (input examples).
		 YPtr:			Pointer to the real array of actual target values.
		 EyPtr:			Pointer to the real array of current svm target estimates.
		 PyPtr:			Pointer to the real array of temporary delta estimate errors (as a percent of target values).
		 KcPtr:			Pointer to the Kc real number array of chached kernel values kernel(x,X[Nn]).
 
Returns: Error:			The current SVM regression error (as a percent of target values). 

References:	[1] Christianini, "Support Vector Machines".

Note1: The regression error is computed as a percent of the target.
Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.

#endif

static      TVAL FMath3__computeError(LpXCONTEXT gCP,
									  LpTHREAD gTP,
									  TVAL kernel,
									  LpREAL Error,
									  NUM N,
									  LpNUM worstN2,
									  REAL ETollerance,
									  LpREAL WPtr,
									  LpOBJ  XPtr,     
									  LpREAL YPtr,
									  LpREAL EyPtr,
									  LpREAL PyPtr,     
									  LpREAL KcPtr)     
{
NUM	                m;
NUM	                n;
REAL				pct;
TVAL				XNn;
TVAL				Xm;
REAL				ey;
StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,20);

EndFrame
 
KcPtr = KcPtr; // NOOP to hide unused parameter warning message
/* Compute Return the regression error for the currently trained SVM weights. */
/* Note1: The regression error is computed as a percent of the target. */
/* Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error. */
*Error = 0;
*worstN2 = 0;
for (n = 0; n < N; ++n) 
	{
	/* Always respond to the escape key. */
	if (++gTP->FVmscript_escapeCheck >= _FSMARTBASE_ESCAPECHECK)
		{
		gTP->FVmscript_escapeCheck = 0;
		if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);
		}

	/* Retrieve the Nn number vector from the array of input examples. */
	XNn.u.NumVector = (TNumVector*)XPtr[n];
	XNn.Tag = XNn.u.NumVector->itsObjectType;
	prmv[0] = XNn;

	/* Compute the N number vector of cached kernel values for kernel(X[Nn],X[m]). */
	ey = 0.0;
	for (m = 0; m < N; ++m) 
		{
		/* Retrieve the nth number vector from the array of input examples. */
		Xm.u.NumVector = (TNumVector*)XPtr[m];
		Xm.Tag = Xm.u.NumVector->itsObjectType;
		prmv[1] = Xm;

		if (WPtr[m] != 0.0)
			{
			/* Compute the kernel function and add the weighted result. */
			/* Note: We only compute the kernel functions where the */
			/*       weight is not equal to zero. */ 
			switch (kernel.Tag)
				{
				case TYCPROCEDURE:
					*ret = (*(asSymbol(&kernel)->itsCProcedure))(gCP,gTP,2,&prmv[0]);
					ExitOnError(*ret);
					break;

				case TYCFUNCTION:
					*ret = (*asFunction(&kernel))(gCP,gTP,2,&prmv[0]);     
					ExitOnError(*ret);
					break;

				case TYLAMBDA:
					*ret = _VmEvaluate(asProcedure(&kernel),2,&prmv[0]);
					ExitOnError(*ret);
					break;

				default:
					*ret = TERROR("!svmRegression: Missing or invalid kernel function!");
					ExitOnError(*ret);
					break;
				}

			/* Cache the result value of the kernel function evaluation. */
			switch (ret->Tag)
				{
				case TYREAL:
					ey += (WPtr[m]*ret->u.Real);
					break;

				case TYNUM:
					ey += (WPtr[m]*ret->u.Int);
					break;

				default:
					*ret = TERROR("!svmRegression: kernel function must return a numeric value!");
					ExitOnError(*ret);
					break;
				}
			}
	} /* End M loop */
	
	/* Computation of error as a percent of target value */
	/* breaks down when the target value is zero. In this */
	/* special case, we use the actual error as if it were */
	/* a percent of target value. */
	EyPtr[n] = ey;
	if (YPtr[n] != 0)
		PyPtr[n] = pct = abs((YPtr[n]-EyPtr[n])/YPtr[n]);
	else
		PyPtr[n] = pct = abs(YPtr[n]-EyPtr[n]);
	if (pct > *worstN2) *worstN2 = n;
	*Error += max(pct-ETollerance,0)/N;
	}

/* Return the delta error as a percent of target values. */
*ret = TREAL(*Error);
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3__initFromGaussian

Initialize the svm dual regression model, before training, with a guassian estimate of the weights.

Args:    kernel:		The kernel to use during training.
		 N:				The number of training points in X.
		 worstN2:		The training point with the worst percentage error
		 maxSVSize:		The maximum number of support vectors selected for Gaussian initialization
		 ETollerance:	The support vector machine error tollerance.
		 WPtr:			Pointer to the real array of weights to be trained.
		 XPtr:			Pointer to the X object array of number vectors (input examples).
		 YPtr:			Pointer to the real array of actual target values.
		 EyPtr:			Pointer to the real array of current svm target estimates.
		 PyPtr:			Pointer to the real array of temporary delta estimate errors (as a percent of target values).
		 KcPtr:			Pointer to the Kc real number array of chached kernel values kernel(x,X[Nn]).
 
Returns: Error:			The initial SVM regression error (as a percent of target values). 

References:	[1] Christianini, "Support Vector Machines".

Note1: The regression error is computed as a percent of the target.
Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.

#endif

static  TVAL FMath3__initFromGaussian(LpXCONTEXT gCP,
									  LpTHREAD gTP,
									  TVAL kernel,
									  LpREAL Error,
									  NUM N,
									  LpNUM worstN2,
									  NUM maxSVSize,
									  REAL ETollerance,
									  LpREAL WPtr,
									  LpOBJ  XPtr,     
									  LpREAL YPtr,
									  LpREAL EyPtr,
									  LpREAL PyPtr,     
									  LpREAL KcPtr)     
{
NUM	                m;
NUM	                n;
NUM	                k;				/* Absolute index into the Graussian matrix used during initialization. */
REAL				r;				/* Random seed number used during Gaussian initialization. */
NUM					SN; 			/* Number of seed support vectors to use in Gaussian initialization. */
LpNUM				SVPtr; 			/* Pointer to the Vector of support vector indices to use in Gaussian initialization. */
LpREAL				CSPtr; 			/* Pointer to the Gaussian regression coefficients for the support vectors. */
LpREAL				RMPtr; 			/* Point to the regression matrix regressing the support vectors against all of the training points in X. */
TVAL				XNn;
TVAL				Xm;
REAL				ey;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(RM);					/* The regression matrix regressing the support vectors against all of the training points in X. */
DeclareTVAL(CS);					/* The Gaussian regression coefficients for the support vectors. */
DeclareTVAL(SV);					/* The Vector of support vector indices to use in Gaussian initialization. */
DeclareTVAL(randomFN);				/* The random number function to use in Gaussian initialization. */
DeclareTVALArray(prmv,10);

EndFrame
 

/* Determine the initial number of support vectors. */
/* Note: The initial number of support vectors is */
/*       limited to the user specified maximum */
/*       so that the support vector machine will */
/*       not overfit by learning all available training */
/*       data exactly. Choosing an initial support vector */
/*       set smaller than available training data forces */
/*       the support vector machine to perform in-sample */
/*       learning with out-of-sample testing. */
SN = (N <= maxSVSize) ? N : maxSVSize;

/* Initialize the empty vector of support vector indices. */
SV->u.IntVector = TIntVector_New(gCP,gTP);
SV->Tag = TYINTVECTOR;
TIntVector_SetMaxIndex(gCP,gTP,*SV,SN);
SVPtr = IntArray(*SV);

/* Select the initial support vectors, at random, from the entire set of training points. */
*randomFN = TGVALUE("random");
m = 0;
for (n = 0; n < N; ++n)
	{
	r = FSmartbase_Eval(gCP,gTP,*randomFN,1,TINT((N - n))).u.Real;
    if ((r < (SN - m)) && (m < SN)) SVPtr[m++] = n;
	}

/* Construct the support vectors regression matrix.					*/
/* Note: We attempt to regress a linear model of the support vectors*/
/*       against all of the training points in X. The Regression	*/
/*       Matrix is is of the following form:						*/
/*          kernel(X[0],X[SV[0]]) ... kernel(X[0],X[SV[SN]]) Y[0]	*/
/*          kernel(X[1],X[SV[0]]) ... kernel(X[1],X[SV[SN]]) Y[1]	*/
/*                  ...           ...              ...				*/
/*          kernel(X[N],X[SV[0]]) ... kernel(X[N],X[SV[SN]]) Y[N]	*/
RM->u.NumMatrix = TNumMatrix_New(gCP,gTP);
RM->Tag = TYNUMMATRIX;
TNumMatrix_SetMaxIndex(gCP, gTP, *RM, N*(SN+1));
RM->u.NumMatrix->itsRank = 2;
RM->u.NumMatrix->itsDimensions[2] = 1;		/* no 3rd dim	*/
RM->u.NumMatrix->itsDimensions[1] = (SN+1);	/* colCount		*/
RM->u.NumMatrix->itsDimensions[0] = N;		/* rowCount		*/
RMPtr = RealMatrix(*RM);

k = 0;
for (n = 0;n < N; ++n)
	{
	/* Retrieve the Nn number vector from the array of input examples. */
	if ((XPtr[n] == NIL) || (((TNumVector*)XPtr[n])->itsObjectType != TYNUMVECTOR))
		{
		MissingNumVector:
		*ret = TERROR("!svmRegression: missing number vector in X vector array!");
		ExitOnError(*ret);
		}
	XNn.u.NumVector = (TNumVector*)XPtr[n];
	XNn.Tag = XNn.u.NumVector->itsObjectType;
	prmv[0] = XNn;

	for (m = 0; m < SN; ++m)
		{
		/* Compute the N number vector of cached kernel values for kernel(X[Nn],X[SV[m]]). */
		ey = 0.0;

		/* Retrieve the nth number vector from the array of input examples. */
		if ((XPtr[SVPtr[m]] == NIL) || (((TNumVector*)XPtr[SVPtr[m]])->itsObjectType != TYNUMVECTOR)) goto MissingNumVector; 
		Xm.u.NumVector = (TNumVector*)XPtr[SVPtr[m]];
		Xm.Tag = Xm.u.NumVector->itsObjectType;
		prmv[1] = Xm;

		/* Compute the kernel function for this element of the regression matrix. */
		/* Note: We only compute the kernel functions where the */
		/*       weight is not equal to zero. */ 
		switch (kernel.Tag)
			{
			case TYCPROCEDURE:
				*ret = (*(asSymbol(&kernel)->itsCProcedure))(gCP,gTP,2,&prmv[0]);
				ExitOnError(*ret);
				break;

			case TYCFUNCTION:
				*ret = (*asFunction(&kernel))(gCP,gTP,2,&prmv[0]);     
				ExitOnError(*ret);
				break;

			case TYLAMBDA:
				*ret = _VmEvaluate(asProcedure(&kernel),2,&prmv[0]);
				ExitOnError(*ret);
				break;

			default:
				*ret = TERROR("!svmRegression: Missing or invalid kernel function!");
				ExitOnError(*ret);
				break;
			}

		/* Cache the result value of the kernel function evaluation. */
		switch (ret->Tag)
			{
			case TYREAL:
				RMPtr[k++] = ret->u.Real;
				break;

			case TYNUM:
				RMPtr[k++] = ret->u.Int;
				break;

			default:
				*ret = TERROR("!svmRegression: kernel function must return a numeric value!");
				ExitOnError(*ret);
				break;
			}
		} /* End m loop */
	RMPtr[k++] = YPtr[n];
	WPtr[n] = 0.0;
	} /* End n loop */

/* Solve for the Gaussian coefficients of the support vectors. */
/* Note: We perform a Gaussian linear regression on the Regression */
/*       Matrix, returning an SN Vector of coefficients, which we */
/*       set as the weights, in the SVM  model, of each of the */
/*       support vectors respectively. All other non-support-vector */
/*       weights, in the initial SVM model, are set to zero. */
*RM = FMath1_MakeGaussianMatrix(gCP,gTP,1,RM);
ExitOnError(*RM);
*RM = FMath1_MatrixGaussianEliminate(gCP,gTP,1,RM);
ExitOnError(*RM);
*CS = FMath1_MatrixGaussianSubstitute(gCP,gTP,1,RM);
ExitOnError(*CS);
CSPtr = RealArray(*CS);
for (m = 0; m < SN; ++m)
	{
	WPtr[SVPtr[m]] = CSPtr[m];
	}

/* Return the error as a percent of target values. */
*ret = FMath3__computeError(gCP,gTP,kernel,Error,N,worstN2,ETollerance,WPtr,XPtr,YPtr,EyPtr,PyPtr,KcPtr);
ExitOnError(*ret);
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3__computeKnVector

Return the regression error for the currently proposed changes in the dual coefficients.

Args:    kernel:		The kernel to use during training.
		 N:				The number of training points in X.
		 Nc:			The chosen training point in X.
		 XPtr:			Pointer to the X object array of number vectors (input examples).
		 KcPtr:			Pointer to the Kc real number array of chached kernel values kernel(x,X[Nc]).
 
Returns: result			True, or An error if one occurs. 

References:	[1] Christianini, "Support Vector Machines".

Note1: The regression error is computed as a percent of the target.
Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.
#endif

static   TVAL FMath3__computeKnVector(LpXCONTEXT gCP,
									  LpTHREAD gTP,
									  TVAL kernel,
									  NUM N,
									  NUM Nc,
									  LpOBJ XPtr,     
									  LpREAL KcPtr)     
{
NUM	                n;
TVAL				XNc;
TVAL				Xn;
StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,20);

EndFrame

/* Always respond to the escape key. */
if (++gTP->FVmscript_escapeCheck >= _FSMARTBASE_ESCAPECHECK)
	{
	gTP->FVmscript_escapeCheck = 0;
	if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);
	}

/* Retrieve the Nc number vector from the array of input examples. */
XNc.u.NumVector = (TNumVector*)XPtr[Nc];
XNc.Tag = XNc.u.NumVector->itsObjectType;
prmv[0] = XNc;

/* Compute the N number vector of cached kernel values for kernel(X[Nc],X[n]). */
for (n = 0; n < N; ++n) 
	{
	/* Retrieve the nth number vector from the array of input examples. */
	Xn.u.NumVector = (TNumVector*)XPtr[n];
	Xn.Tag = Xn.u.NumVector->itsObjectType;
	prmv[1] = Xn;

	/* Evaluate the kernel function and receive the result. */
	switch (kernel.Tag)
		{
		case TYCPROCEDURE:
			*ret = (*(asSymbol(&kernel)->itsCProcedure))(gCP,gTP,2,&prmv[0]);
			ExitOnError(*ret);
			break;

		case TYCFUNCTION:
			*ret = (*asFunction(&kernel))(gCP,gTP,2,&prmv[0]);     
			ExitOnError(*ret);
			break;

		case TYLAMBDA:
			*ret = _VmEvaluate(asProcedure(&kernel),2,&prmv[0]);
			ExitOnError(*ret);
			break;

		default:
			*ret = TERROR("!svmRegression: Missing or invalid kernel function!");
			ExitOnError(*ret);
			break;
		}

	/* Cache the result value of the kernel function evaluation. */
	switch (ret->Tag)
		{
		case TYREAL:
			KcPtr[n] = ret->u.Real;
			break;

		case TYNUM:
			KcPtr[n] = ret->u.Int;
			break;

		default:
			*ret = TERROR("!svmRegression: kernel function must return a numeric value!");
			ExitOnError(*ret);
			break;
		}
	}
 

/* Return successful completion. */
FrameExit(gCP->Tval_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3__computeDeltaError

Return the regression error for the currently proposed changes in the dual coefficients.

Args:    w1:			The new guess for the weight at W[N1].
		 w2:			The new guess for the weight at W[N2].
		 N1:			The first training point in X.
		 N2:			The second training point in X.
		 worstN2:			The training point with the worst percentage error
		 N:				The number of training points in X.
		 ETollerance:	The support vector machine error tollerance.
		 kernel:		Pointer to the real array of weight coefficients.
		 WPtr:			Pointer to the real array of weight coefficients.
		 K1Ptr:			Pointer to the real array of cached kernel values, kernel(x,X[N1]).
		 K2Ptr:			Pointer to the real array of cached kernel values, kernel(x,X[N2]).
		 YPtr:			Pointer to the real array of actual target values.
		 EyPtr:			Pointer to the real array of current svm target estimates.
		 DyPtr:			Pointer to the real array of temporary delta target estimates.
		 PyPtr:			Pointer to the real array of temporary delta estimate errors (as a percent of target values).
 
Returns: err:		 The experimental delta regression error (as a percent of target). 

References:	[1] Christianini, "Support Vector Machines".

Note1: The regression error is computed as a percent of the target.
Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.

#endif

static REAL FMath3__computeDeltaError(LpXCONTEXT gCP,
									  LpTHREAD gTP,
									  REAL w1,
									  REAL w2,
									  NUM N1,
									  NUM N2,
									  LpNUM worstN2,
									  NUM N,
									  REAL ETollerance,
									  LpREAL WPtr,
									  LpREAL K1Ptr,
									  LpREAL K2Ptr,
									  LpREAL YPtr,
									  LpREAL EyPtr,
									  LpREAL DyPtr,
									  LpREAL PyPtr)     
{
NUM	                n;
REAL				dy;
REAL				ey;
REAL				pct;
REAL                error;
 
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
/* Compute the delta regression error for the currently proposed changes in the weight coefficients. */
/* Note1: Since we initialized the weight vector to all zeros, */
/*        we already know that the starting model estimate for */
/*        each target point is zero. */
/* Note2: This is an important time savings in that we never */
/*        have to compute the full model target estimates completely. */
/*        From this point on the model target estimates are */
/*        kep constantly updated with incremental computation */
/* Note3: By computing the proposed changes to the model, from the new */
/*		  two point weight guesses, in a seperate delta estimate area, */
/*		  we can easily throw these changes away if they do not lead */
/*		  to an overall improvement in the total model error. */
error = 0;
*worstN2 = 0;
for (n = 0; n < N; ++n) 
	{
	/* We only need to compute the incremental effects */
	/* of changes in the two weights for points N1 and N2. */
	dy = ((w1-WPtr[N1])*K1Ptr[n]);
	dy += ((w2-WPtr[N2])*K2Ptr[n]);
	DyPtr[n] = ey = EyPtr[n] + dy;
	/* Computation of error as a percent of target value */
	/* breaks down when the target value is zero. In this */
	/* special case, we use the actual error as if it were */
	/* a percent of target value. */
	if (YPtr[n] != 0)
		PyPtr[n] = pct = abs((YPtr[n]-DyPtr[n])/YPtr[n]);
	else
		PyPtr[n] = pct = abs(YPtr[n]-DyPtr[n]);
	if (pct > *worstN2) *worstN2 = n;
	error += max(pct-ETollerance,0)/N;
	}

/* Return the delta error as a percent of target values. */
return(error);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FMath3__takeStep

Incrementally train the SVM model on the two chosen training points.

Args:    w1:			The new guess for the weight at W[N1].
		 w2:			The new guess for the weight at W[N2].
		 N1:			The first training point in X.
		 N2:			The second training point in X.
		 worstN2:		The training point with the worst percentage error
		 N:				The number of training points in X.
		 printSW:		The verbose mode switch used for scientific testing.
		 ETollerance:	The support vector machine error tollerance.
		 maxErr:		The maximum error which will halt training.
		 Error:			The current SVM error as a percent of target value.
		 Generations:	The current count of generations attempted
		 kernel:		The kernel argument for use during SVM training
		 WPtr:			Pointer to the real array of weight coefficients.
		 K1Ptr:			Pointer to the real array of cached kernel values, kernel(x,X[N1]).
		 K2Ptr:			Pointer to the real array of cached kernel values, kernel(x,X[N2]).
		 XPtr:			Pointer to the X object array of number vectors (input examples).
		 YPtr:			Pointer to the real array of actual target values.
		 EyPtr:			Pointer to the real array of current svm target estimates.
		 DyPtr:			Pointer to the real array of temporary delta target estimates.
		 PyPtr:			Pointer to the real array of temporary delta estimate errors (as a percent of target values).


Returns: result			True, or An error if one occurs. 

References:	[1] Christianini, "Support Vector Machines".

Note1: The regression error is computed as a percent of the target.
Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.

#endif

static TVAL          FMath3__takeStep(LpXCONTEXT gCP,
									  LpTHREAD gTP,
									  NUM N1,
									  NUM N2,
									  LpNUM worstN2,
									  NUM N,
									  BOLE printSW,
									  REAL ETollerance,
									  REAL maxErr,
									  LpREAL Error,
									  LpREAL Generations,
									  TVAL kernel,
									  LpREAL WPtr,
									  LpREAL K1Ptr,
									  LpREAL K2Ptr,
									  LpOBJ XPtr,
									  LpREAL YPtr,
									  LpREAL EyPtr,
									  LpREAL DyPtr,
									  LpREAL PyPtr)     
{
NUM	                g;						/* A temporary index to be used during training */
NUM	                m;						/* A temporary index to be used during training */
REAL                w1 = 0;					/* The new guestimate weight for point N1 */
REAL                w2 = 0;					/* The new guestimate weight for point N2 */
REAL				err;					/* The delta error to be used during two point training */
REAL				bestError = 1.0;		/* The best saved SVM error as a percent of target value */
REAL				bestW1 = 0;				/* The best saved new weight guestimate for chosen point N1 */
REAL				bestW2 = 0;				/* The best saved new weight guestimate for chosen point N2 */
REAL				bestM = 0;				/* The best saved new weight multiplier */
REAL				multiplier = 0;			/* The current multiplier used in selecting new weight guestimates */
REAL				D1 = 0;					/* The temporary new weight delta for chosen point N1 */
REAL				D2 = 0;					/* The temporary new weight delta for chosen point N2 */
REAL				E1 = 0;					/* The temporary new weight delta error for chosen point N1 */
REAL				E2 = 0;					/* The temporary new weight delta error for chosen point N2 */
REAL				k = 0;					/* The local derrivative for chosen points N1 and N2 */
NUM					retryCount = 0;			/* The current number of attempts for chosen points N1 and N2 */
NUM					maxRetries = 4;			/* The current number of maximum retry attempts allowed */
CHAR				buf[256];				/* Temporary display buffer for verbose mode progress reports. */
REAL				high;					/* High search marker for binary optimization search. */
REAL				low;					/* Low search marker for binary optimization search. */
REAL				mid;					/* Mid search marker for binary optimization search. */
REAL				highError;				/* High search marker error during binary optimization search. */
REAL				lowError;				/* Low search marker error during binary optimization search. */
REAL				midError;				/* Mid search marker error during binary optimization search. */
REAL				oldPyN1;				/* Old percent error at point N1. */
REAL				oldPyN2;				/* Old percent error at point N1. */
StartFrame
DeclareTVAL(result);
EndFrame

/* Compute and cache the kernel values for the chosen point N1, */
/* and then compute the local SVM model derrivative for the two */
/* chosen points N1 and N2. Also increment the generations count. */
/* Note1: We only have to perform these expensive computations */
/*		  once when we entry this function. */
/* Note2: The kernel values for the chosen point N2 have already */
/*		  been computed and cached by the caller. We do everything */
/*		  possible to perform these expensive computations parsimoniously. */
*Generations += (1.0/(REAL)N);
*result = FMath3__computeKnVector(gCP,gTP,kernel,N,N1,XPtr,K1Ptr);
ExitOnError(*result);     
k = K1Ptr[N1] + K2Ptr[N2] - (2*K1Ptr[N2]);

/* Make a new attempt to improve the SVM model error. */
/* Note: We use a modification of the SMO regression algorithm which */
/*	     attempts in incrementally improve the SVM model by making trial */
/*	     alterations in the weights at the two chosen points N1 and N2. */
Retry:

/* Compute the SVM model errors and the optimal deltas at the chosen points. */
oldPyN1 = PyPtr[N1];
oldPyN2 = PyPtr[N2];
E1 = EyPtr[N1] - YPtr[N1];
E2 = EyPtr[N2] - YPtr[N2];
if (abs(k) == 0.0)
	{
	D1 = (E2 - E1);
	D2 = (E1 - E2);
	}
else
	{
	D1 = (E2 - E1)/k;
	D2 = (E1 - E2)/k;
	}

/* Perform several iterative attempts at improving the SVM model error. */
/* Note1: This heuristic search works well because, at this point, */
/*        we do not have to recompute any of the kernels. Therefore */
/*        experimental iteration here is cheaper than any other technique. */
/* Note2: Since we originally initialized the weight vector to all zeros, */
/*        we already know that the starting model estimate for each target */
/*        point is zero. */
/* Note3: This is an important time savings in that we never */
/*        have to compute the full model target estimates completely. */
/*        Throughout the whole algorithm, the model target estimates are */
/*        kept constantly updated with incremental computation only. */
/* Note4: By computing the proposed changes to the model, from the new */
/*		  two point weight guesses, in a seperate delta estimate area, */
/*		  we can easily throw these changes away if they do not lead */
/*		  to an overall improvement in the total model error. */
bestError = *Error;
high = 2.0;
mid = 0.0;
low = -2.0;

/* Attempt to improve the SVM model regression error at the high point. */
/* Note: By computing the proposed changes to the model, from the new */
/*		 two point weight guesses, in a seperate delta estimate area, */
/*		 we can easily throw these changes away if they do not lead */
/*		 to an overall improvement in the total model error. */
multiplier = high;
w2 = WPtr[N2] + (multiplier * D2);
w1 = WPtr[N1] + (multiplier * D1);
err = FMath3__computeDeltaError(gCP,gTP,w1,w2,N1,N2,worstN2,N,ETollerance,WPtr,K1Ptr,K2Ptr,YPtr,EyPtr,DyPtr,PyPtr);
highError = err;
if (err < bestError)
	{
	bestError = err;
	bestM = multiplier;
	bestW1 = w1;
	bestW2 = w2;
	}

/* Attempt to improve the SVM model regression error at the low point. */
/* Note: By computing the proposed changes to the model, from the new */
/*		 two point weight guesses, in a seperate delta estimate area, */
/*		 we can easily throw these changes away if they do not lead */
/*		 to an overall improvement in the total model error. */
multiplier = low;
w2 = WPtr[N2] + (multiplier * D2);
w1 = WPtr[N1] + (multiplier * D1);
err = FMath3__computeDeltaError(gCP,gTP,w1,w2,N1,N2,worstN2,N,ETollerance,WPtr,K1Ptr,K2Ptr,YPtr,EyPtr,DyPtr,PyPtr);
lowError = err;
if (err < bestError)
	{
	bestError = err;
	bestM = multiplier;
	bestW1 = w1;
	bestW2 = w2;
	}

/* Attempt to improve the SVM model regression error at the mid point. */
/* Note: Since the initial mid point is the result of no change at all, */
/*		 we can use the current SVM model Error as the result of the */
/*		 mid point improvement attempt. */
midError = *Error;

/* Perform a binary search of iterative attempts at improving the SVM model error. */
/* Note1: This heuristic search works well because changes the SVM model produce */
/*        monotonically increasing or decreasing error rates. Therefore experimental */
/*        iteration via binary search is cheaper than any other technique. */
/* Note2: Since we originally initialized the weight vector to all zeros, */
/*        we already know that the starting model estimate for each target */
/*        point is zero. */
/* Note3: This is an important time savings in that we never */
/*        have to compute the full model target estimates completely. */
/*        Throughout the whole algorithm, the model target estimates are */
/*        kept constantly updated with incremental computation only. */
/* Note4: By computing the proposed changes to the model, from the new */
/*		  two point weight guesses, in a seperate delta estimate area, */
/*		  we can easily throw these changes away if they do not lead */
/*		  to an overall improvement in the total model error. */
for (m = 0; m < 8; ++m)
	{
	/* Now we narrow the search based on the observed Error results. */
	/* Note: This heuristic search works well because changes the SVM model */
	/*       produce monotonically increasing or decreasing error rates. */
	/*       Therefore experimental iteration via binary search is possible. */
	if (lowError < midError)
		{
		high = mid;
		highError = midError;
		mid = low + ((high - low)/2);
		}
	else
	    /* (highError < midError) */
		{
		low = mid;
		lowError = midError;
		mid = low + ((high - low)/2);
		}

	/* Attempt to improve the SVM model regression error at the mid point. */
	/* Note: By computing the proposed changes to the model, from the new */
	/*		 two point weight guesses, in a seperate delta estimate area, */
	/*		 we can easily throw these changes away if they do not lead */
	/*		 to an overall improvement in the total model error. */
	multiplier = mid;
	w2 = WPtr[N2] + (multiplier * D2);
	w1 = WPtr[N1] + (multiplier * D1);
	err = FMath3__computeDeltaError(gCP,gTP,w1,w2,N1,N2,worstN2,N,ETollerance,WPtr,K1Ptr,K2Ptr,YPtr,EyPtr,DyPtr,PyPtr);
	midError = err;
	if (err < bestError)
		{
		bestError = err;
		bestM = multiplier;
		bestW1 = w1;
		bestW2 = w2;
		}

	/* Always respond to the escape key. */
	if (++gTP->FVmscript_escapeCheck >= _FSMARTBASE_ESCAPECHECK)
		{
		gTP->FVmscript_escapeCheck = 0;
		if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);
		}
	}

/* Don't update these two weights unless there the binary search yields an improvement. */
if (bestError < *Error)
	{
	w1 = bestW1;
	w2 = bestW2;
	/* Compute new delta regression error for the two points. */
	/* Note1: Make the changes permanent by copying the temporary */
	/*		  delta error estimate area into the current error */
	/*		  estimate area: Ey = Dy. This also leaves Py with the */
	/*		  correct percentage errors. */
	err = FMath3__computeDeltaError(gCP,gTP,w1,w2,N1,N2,worstN2,N,ETollerance,WPtr,K1Ptr,K2Ptr,YPtr,EyPtr,DyPtr,PyPtr);     
	if (printSW == TRUE)
		{
		/* If we are in verbose mode, we will display a progress report on the console. */
		g = *Generations;
		sprintf(buf,"svmRegression: Generations = ["INTFORMAT"], L=[%.4G], N1=["INTFORMAT"], N2=["INTFORMAT"], N1Err=[%.13G,%.13G], N2Err=[%.13G,%.13G], OldErr=[%.13G], Error=[%.13G]",g,bestM,N1,N2,oldPyN1,PyPtr[N1],oldPyN2,PyPtr[N2],err,*Error);  
		(*gCP->_Host_Display)((POINTER)gCP,gTP,(char*)buf, 1);
		}
	_FMemory_memcpy(EyPtr,DyPtr,N*sizeof(REAL));
	*Error = err;
	WPtr[N1] = w1;
	WPtr[N2] = w2;

	/* Have we reached an acceptable error level? */
	if (*Error <= maxErr) FrameExit(gCP->Tval_TRUE);

    /* Perform several repeated retries at improving the error. */
    /* Note: This heuristic search works well because, even at this point, */
    /*       we do not have to recompute any of the kernels. Therefore */
    /*       experimental iteration here is cheaper than any other technique. */
    if (++retryCount < maxRetries) goto Retry;

	/* Return a successful completion. */
	/* Note: We get here if we have been able to improve the error. */
	FrameExit(gCP->Tval_TRUE);
	}


/* Return unsuccessful completion. */
/* Note: We get here if we have been unable to improve the error. */
FrameExit(gCP->Tval_FALSE);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FMath3__examineExample

Incrementally train the SVM model on the specified example training point.

Args:    w1:			The new guess for the weight at W[N1].
		 w2:			The new guess for the weight at W[N2].
		 N1:			The first training point in X.
		 N2:			The second training point in X.
		 worstN2:		The training point with the worst percentage error
		 N:				The number of training points in X.
		 maxGen:		The maximum generation count which will halt training.
		 printSW:		The verbose mode switch used for scientific testing.
		 ETollerance:	The support vector machine error tollerance.
		 maxErr:		The maximum error which will halt training.
		 Error:			The current SVM error as a percent of target value.
		 Generations:	The current count of generations attempted
		 kernel:		The kernel argument for use during SVM training
		 WPtr:			Pointer to the real array of weight coefficients.
		 K1Ptr:			Pointer to the real array of cached kernel values, kernel(x,X[N1]).
		 K2Ptr:			Pointer to the real array of cached kernel values, kernel(x,X[N2]).
		 XPtr:			Pointer to the X object array of number vectors (input examples).
		 YPtr:			Pointer to the real array of actual target values.
		 EyPtr:			Pointer to the real array of current svm target estimates.
		 DyPtr:			Pointer to the real array of temporary delta target estimates.
		 PyPtr:			Pointer to the real array of temporary delta estimate errors (as a percent of target values).


Returns: result			True, or An error if one occurs. 

References:	[1] Christianini, "Support Vector Machines".

Note1: The regression error is computed as a percent of the target.
Note2: Any regression error within the tollerance limit, ETollerance, is treated as NO error.

#endif

static TVAL    FMath3__examineExample(LpXCONTEXT gCP,
									  LpTHREAD gTP,
									  NUM N1,
									  NUM N2,
									  LpNUM worstN2,
									  NUM N,
									  NUM maxGen,
									  BOLE printSW,
									  REAL ETollerance,
									  REAL maxErr,
									  LpREAL Error,
									  LpREAL Generations,
									  TVAL kernel,
									  LpREAL WPtr,
									  LpREAL K1Ptr,
									  LpREAL K2Ptr,
									  LpOBJ XPtr,
									  LpREAL YPtr,
									  LpREAL EyPtr,
									  LpREAL DyPtr,
									  LpREAL PyPtr)     
{
NUM	                m;				/* A temporary index to be used during training */
NUM	                n;				/* A temporary index to be used during training */
StartFrame
DeclareTVAL(result);
EndFrame

/* Compute and cache the kernel values for the chosen point N2. */
/* Note1: We only have to perform these expensive computations */
/*		  once when we enter this function. */
*result = FMath3__computeKnVector(gCP,gTP,kernel,N,N2,XPtr,K2Ptr);
ExitOnError(*result); 

/* Loop through each training example beginning at a random point. */
m = FSmartbase_Eval(gCP,gTP,TGVALUE("random"),1,TINT(.99999)).u.Real * N;
for (n = 0; n < N; ++n)
	{
	/* Always respond to the escape key. */
	if (++gTP->FVmscript_escapeCheck >= _FSMARTBASE_ESCAPECHECK)
		{
		gTP->FVmscript_escapeCheck = 0;
		if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);
		}

	/* Select the N1 training point at random. */
	N1 = (n+m) >= N ? (n+m)-N : (n+m);
	/* Attempt improvement only on those training points with an unacceptable error. */
	if (((PyPtr[N1]-ETollerance) > maxErr) && (N1 != N2))
		{
		*result = FMath3__takeStep(gCP,gTP,N1,N2,worstN2,N,printSW,ETollerance,maxErr,Error,Generations,kernel,WPtr,K1Ptr,K2Ptr,XPtr,YPtr,EyPtr,DyPtr,PyPtr);
		ExitOnError(*result);
		/* Were we able to improve the error? */
		if ((result->Tag == TYBOLE) && (result->u.Bool == TRUE))
			{
			/* Return a successful completion. */
			/* Note: We get here if we have been able to improve the error. */
			FrameExit(gCP->Tval_TRUE);
			}
		/* Have we exhausted the maximum number of generations allowed? */
		if (*Generations >= maxGen)
			{
			/* Return a successful completion. */
			/* Note: We get here if we have been able to improve the error. */
			FrameExit(gCP->Tval_FALSE);
			}
		}
	}


/* Return unsuccessful completion. */
/* Note: We get here if we have been unable to improve the error. */
FrameExit(gCP->Tval_FALSE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath3_SvmRegression

Returns the Number Vector containing the trained weights for the support vector machine
dual model regression on the specified inputs.

Args:    x:          The N by M vector array representing the original observations
                     in the form of:    x x ... x
                                        x x ... x
                                            ... 
                                        x x ... x
         y   		 The N vector of dependent variables.
         kernel	     The kernel function to be used for support vector machine training,
					 as in the following examples:
							vectorInnerProduct	  
							vectorSquareInnerProduct	  
							vectorCubeInnerProduct	  
							vectorSineInnerProduct	  
							vectorCosineInnerProduct	  
							vectorTanInnerProduct	  
							vectorLogInnerProduct	  
							vectorExpInnerProduct
						Note: the kernel function may be a CFunction, CProcedure, or an Lambda.
         ETollerance The regression error tollerance as a percent of Y.
         maxErr  	 The maximum error before halting training as a percent of Y.
         maxGen  	 The maximum generation count before halting training.
         maxSVSize	 The maximum number of support vectors to select for Gaussian initialization.
		 printSW	 The verbose mode switch for scientific testing.
 
Returns: S		     A Structure containing the following elements:
						Error:			The average absolute error, outside the specified error tollerance, 
										expressed as a percent of target values.		
						Weights:		The number Vector containing the trained weights for the svm 
										dual model regression.
						Generations:	The number of generations required for training.
						Ey:				The number Vector containing the SVM model estimates for the 
										target training examples.
						Py:				The number Vector containing the SVM model estimates errors
										as a percentage of each target training example.

Returns: W		     The number Vector containing the trained weights for the svm dual model regression,
					 also the cdr of the returned weight vector contains the average absolute error, outside
					 the specified error tollerance, expressed as a percent of target values. 

Example:	(svmRegression X Y kernelID ETollerance maxErr maxGen printSW)


Note:   Uses a modified version of the SMO regression algorithm in Christianini, page 162.
		Once trained, the dual model produces guestimates for the dependent variables Y,
		using the trained weight vector, W, the new input vector, x, the kernel function,
		kernel, and the original input examples used for training, X, via the following 
		formula: f(x) == y == SUM(n:0toN{W[n]*kernel(x,X[n])}.
 
References:	[1] Christianini, "Support Vector Machines".


#endif

TVAL FMath3_SvmRegression(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM	                n;				/* A temporary index to be used during training */
BOLE				examineAll;		/* True iff we are to examine all training points */
NUM					numChanged;		/* A count of the number of improvments actually made in this examination pass */
NUM	                N;				/* The number of SVM examples to be used during training */
NUM	                N1 = 0;			/* The first example point chosen for two point training */
NUM	                N2 = 0;			/* The second example point chosen for two point training */
NUM	                worstN2;		/* The training point with the worst percentage error */
REAL                ETollerance;	/* The error tollerance to be used during error computation */
REAL				maxErr;			/* The maximum error which will halt training */
REAL				Error = 1.0;	/* The current SVM error as a percent of target value */
REAL				Generations;	/* The current count of generations attempted */
NUM					maxGen;			/* The maximum generation count which will halt training */
NUM					maxSVSize;		/* The maximum number of support vectors to select for Gaussian initialization */
BOLE				printSW;		/* The verbose mode switch used for scientific testing */
LpREAL				YPtr;			/* Pointer to the Y real number array (target examples) */
LpREAL				WPtr;			/* Pointer to the W real number array of weights to be trained */
LpREAL				K1Ptr;			/* Pointer to the K1 real number array of chached kernel values kernel(x,X[N1]) */
LpREAL				K2Ptr;			/* Pointer to the K2 real number array of chached kernel values kernel(x,X[N2]) */
LpREAL				EyPtr;			/* Pointer to the Ey real number array of current target estimates */
LpREAL				DyPtr;			/* Pointer to the Dy real number array of delta target estimates */
LpREAL				PyPtr;			/* Pointer to the Py real number array of delta target estimate errors as a percent of target values */
LpOBJ				XPtr;			/* Pointer to the X object array of number vectors (input examples) */
TVAL				kernel;			/* The kernel argument for use during SVM training */
StartFrame
DeclareTVAL(W);						/* Vector of SVM weights to be trained */
DeclareTVAL(K1);					/* Vector of chached kernel values kernel(x,X[N1]) for use during training */
DeclareTVAL(K2);					/* Vector of chached kernel values kernel(x,X[N2]) for use during training */
DeclareTVAL(Ey);					/* Vector of SVM current target estimates for use during training */
DeclareTVAL(Dy);					/* Vector of SVM delta target estimates for use during training */
DeclareTVAL(Py);					/* Vector of SVM delta target estimates errors as a percent of target value */
DeclareTVAL(result);
DeclareTVAL(symNew);
DeclareTVAL(symStructure);
DeclareTVAL(symError);
DeclareTVAL(symWeights);
DeclareTVAL(symGenerations);
DeclareTVAL(symEy);
DeclareTVAL(symPy);
EndFrame
 
/* Check for the correct number of arguments. */
if (argc != 8) 
	{
	*result = TERROR("!svmRegression: Expecting eight arguments!");
	FrameExit(*result);
	}

/* Validate and point to the X argument array of number vectors (input examples). */
if ((argv[0].Tag != TYOBJVECTOR) ||
	((N = ObjVector(argv[0])->itsMaxItemIndex) <= 0) ||
	((XPtr = (OBJ*)ObjArray(argv[0])) == NULL))
    {
	*result = TERROR("!svmRegression: Expecting first argument to be an object vector array of length N!");
	FrameExit(*result);
    }

/* Validate and point to the Y argument array of real numbers (target examples). */
if ((argv[1].Tag != TYNUMVECTOR) ||
	(N != NumVector(argv[1])->itsMaxItemIndex) ||
	((YPtr = RealArray(argv[1])) == NULL))
    {
	*result = TERROR("!svmRegression: Expecting second argument to be a number vector of length N!");
	FrameExit(*result);
    }

/* Validate and save the kernel argument for use during SVM training. */
kernel = argv[2];

/* Validate and point to the ETollerance argument (the error tollerance during training). */
if ((argv[3].Tag != TYREAL) ||
	((ETollerance = argv[3].u.Real) < 0) ||
	(ETollerance > 1.0))
    {
	*result = TERROR("!svmRegression: Expecting fourth argument to be a percent error tollerance fraction!");
	FrameExit(*result);
    }
Error = 1.0 - ETollerance;

/* Validate and point to the maxErr argument (the average error as a percent of target which halts training). */
if ((argv[4].Tag != TYREAL) ||
	((maxErr = argv[4].u.Real) < 0) ||
	(maxErr > 1.0))
    {
	*result = TERROR("!svmRegression: Expecting fifth argument to be a percent maximum error fraction!");
	FrameExit(*result);
    }

/* Validate and point to the maxGen argument (which controls the maximum number of generations during training). */
if ((isNumIndex(&argv[5]) != TRUE) ||
	((maxGen = asNumIndex(&argv[5])) < 0))
    {
	*result = TERROR("!svmRegression: Expecting sixth argument to be an integer maximum generations!");
	FrameExit(*result);
    }

/* Validate and point to the maxSVSize argument (which controls the maximum number of generations during training). */
if ((isNumIndex(&argv[6]) != TRUE) ||
	((maxSVSize = asNumIndex(&argv[6])) < 0))
    {
	*result = TERROR("!svmRegression: Expecting seventh argument to be an integer maximum support vectors for Gaussian initialization!");
	FrameExit(*result);
    }

/* Validate and point to the printSW argument (which controls verbose mode during scientific testing). */
if (argv[7].Tag != TYBOLE)
    {
	*result = TERROR("!svmRegression: Expecting eighth argument to be a boolean verbose mode switch!");
	FrameExit(*result);
    }
printSW = argv[7].u.Bool;

/* Initialize the empty weights vector to all zeros. */
W->u.NumVector = TNumVector_New(gCP,gTP);
W->Tag = TYNUMVECTOR;
TNumVector_SetMaxIndex(gCP,gTP,*W,N);
WPtr = RealArray(*W);

/* Initialize the empty K1 kernel vector to all zeros. */
K1->u.NumVector = TNumVector_New(gCP,gTP);
K1->Tag = TYNUMVECTOR;
TNumVector_SetMaxIndex(gCP,gTP,*K1,N);
K1Ptr = RealArray(*K1);

/* Initialize the empty K2 kernel vector to all zeros. */
K2->u.NumVector = TNumVector_New(gCP,gTP);
K2->Tag = TYNUMVECTOR;
TNumVector_SetMaxIndex(gCP,gTP,*K2,N);
K2Ptr = RealArray(*K2);

/* Initialize the empty Ey estimate vector to all zeros. */
/* Note1: Since we initialized the weight vector to all zeros, */
/*        we already know that the starting model estimate for */
/*        each target point is zero. */
/* Note2: This is an important time savings in that we never */
/*        have to compute the full model target estimates completely. */
/*        From this point on the model target estimates are */
/*        kep constantly updated with incremental computation */
Ey->u.NumVector = TNumVector_New(gCP,gTP);
Ey->Tag = TYNUMVECTOR;
TNumVector_SetMaxIndex(gCP,gTP,*Ey,N);
EyPtr = RealArray(*Ey);

/* Initialize the empty Dy delta estimate vector to all zeros. */
Dy->u.NumVector = TNumVector_New(gCP,gTP);
Dy->Tag = TYNUMVECTOR;
TNumVector_SetMaxIndex(gCP,gTP,*Dy,N);
DyPtr = RealArray(*Dy);

/* Initialize the empty Py delta percent error vector to all ones. */
/* Note1: Since we initialized the weight vector to all zeros, */
/*        we already know that the starting model estimate for */
/*        each target point is zero. Therefore the model estimate errors */
/*		  start out at 100% (1.0) of target values. */
Py->u.NumVector = TNumVector_New(gCP,gTP);
Py->Tag = TYNUMVECTOR;
TNumVector_SetMaxIndex(gCP,gTP,*Py,N);
PyPtr = RealArray(*Py);
for (n = 0; n < N; ++n)
	{
	PyPtr[n] = 1.0;
	}

/* Select the initial support vectors using a Gaussian heuristic. */
/* Note: Perform Guassian initialization only if requested. */
if (maxSVSize > 0)
	{
	*result = FMath3__initFromGaussian(gCP,gTP,kernel,&Error,N,&worstN2,maxSVSize,ETollerance,WPtr,XPtr,YPtr,EyPtr,PyPtr,K1Ptr);
	ExitOnError(*result);
	}

/* Train the SVM model until the generation count exceeds the maximum allowed limit. */
/* Note: We also quit if we read the error stop limit. */
Generations = 1.0;
examineAll = TRUE;
while ((Generations < maxGen) && (Error > maxErr))
	{
	Generations += (1.0/(REAL)N);
	numChanged = 0;

	/* Loop through each training example looking for W[n] == 0. */
	/* Note: Improve only the training points with no current contribution to the estimate. */
	for (n = 0; n < N; ++n)
		{
		/* Improve only training points where W[n] == 0. */
		if (WPtr[n] == 0.0)
			{
			/* Compute and cache the kernel values for the chosen point N2. */
			/* Note1: We only have to perform these expensive computations */
			/*		  once when we enter this function. */
			N2 = n;
			*result = FMath3__computeKnVector(gCP,gTP,kernel,N,N2,XPtr,K2Ptr);
			ExitOnError(*result); 

			/* Select the second training point at random. */
			N1 = N2; while (N1 != N2) N1 = FSmartbase_Eval(gCP,gTP,TGVALUE("random"),1,TINT(.99999)).u.Real * N;

			/* Improve only training points with no contribution to the SVM model. */
			*result = FMath3__takeStep(gCP,gTP,N1,N2,&worstN2,N,printSW,ETollerance,maxErr,&Error,&Generations,kernel,WPtr,K1Ptr,K2Ptr,XPtr,YPtr,EyPtr,DyPtr,PyPtr);
			ExitOnError(*result);
			/* Should we terminate training at this point? */
			if ((Generations >= maxGen) || (Error <= maxErr)) goto HaltTraining;
			}
		}

	/* Try to improve the training point with the worst error. */
	N2 = worstN2;
	*result = FMath3__examineExample(gCP,gTP,N1,N2,&worstN2,N,maxGen,printSW,ETollerance,maxErr,&Error,&Generations,kernel,WPtr,K1Ptr,K2Ptr,XPtr,YPtr,EyPtr,DyPtr,PyPtr);
	ExitOnError(*result);
	numChanged += result->u.Bool;
	/* Should we terminate training at this point? */
	if ((Generations >= maxGen) || (Error <= maxErr)) goto HaltTraining;

    /* Loop through each training example looking to improve the error. */
    /* Note: We improve only the training point with unacceptable errors, */
    /*       or we improve any training point depending on the setting */
    /*       of the examineAll switch. */
	for (n = 0; n < N; ++n)
		{
		/* Always respond to the escape key. */
		if (++gTP->FVmscript_escapeCheck >= _FSMARTBASE_ESCAPECHECK)
			{
			gTP->FVmscript_escapeCheck = 0;
			if ((*gCP->_Host_Escape)((POINTER)gCP,gTP))
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);
			}
		else
		if (examineAll == TRUE)
			{
			/* Try to improve all training points. */
			N2 = n;
			*result = FMath3__examineExample(gCP,gTP,N1,N2,&worstN2,N,maxGen,printSW,ETollerance,maxErr,&Error,&Generations,kernel,WPtr,K1Ptr,K2Ptr,XPtr,YPtr,EyPtr,DyPtr,PyPtr);
			ExitOnError(*result);
			numChanged += result->u.Bool;
			/* Should we terminate training at this point? */
			if ((Generations >= maxGen) || (Error <= maxErr)) goto HaltTraining;
			}
		else
			{
			/* Try to improve only any training points with an unacceptable error. */
			if ((PyPtr[n]-ETollerance) > maxErr)
				{
				N2 = n;
				*result = FMath3__examineExample(gCP,gTP,N1,N2,&worstN2,N,maxGen,printSW,ETollerance,maxErr,&Error,&Generations,kernel,WPtr,K1Ptr,K2Ptr,XPtr,YPtr,EyPtr,DyPtr,PyPtr);
				ExitOnError(*result);
				numChanged += result->u.Bool;
				/* Should we terminate training at this point? */
				if ((Generations >= maxGen) || (Error <= maxErr)) goto HaltTraining;
				}
			}
		
		/* Update the training termination conditions. */
		if (examineAll == TRUE)
			{
			examineAll = FALSE;
			}
		else
			{
			if (numChanged == 0) examineAll = TRUE;
			}
		}
	}


/* Return the Structure containing the trained weight vector, the average percent error, etc. */
HaltTraining:

*symNew = TGVALUE("new");
*symStructure = TSYMBOL("Structure");
*symError = TSYMBOL("Error");
*symWeights = TSYMBOL("Weights");
*symGenerations = TSYMBOL("Generations");
*symEy = TSYMBOL("Ey");
*symPy = TSYMBOL("Py");
*result = FSmartbase_Eval(gCP,gTP,*symNew,11,*symStructure,
														 *symError,TREAL(Error),
														 *symWeights,*W,
														 *symGenerations,TREAL(Generations),
														 *symEy,*Ey,
														 *symPy,*Py);
FrameExit(*result);
}
