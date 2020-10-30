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

#define _C_FMATH1
#define _SMARTBASE

#if 0
FMath1.c

This source file contains some of the cProcedures which implement the math functions
supported by the SmartLisp interpreter.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include    "fmath1.h"
#include    "fmath2.h"
#include    "fmath3.h"
#include	"math.h"
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tvector.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "fconvert.h"
#include    "ffloat.h"
#include	"fdatefnc.h"
#include	"futil2.h"
#include	"tcpx.h"
#include	"tnummat.h"
#include	"tnumvec.h"
#include	"tbytevec.h"
#include	"tvector.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Init

Initialize the math portion of the SmartLisp function library.  

#endif

TVAL FMath1_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareTVAL(ec);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,alsoSymbol);
EndFrame
 
if(gCP->FMath1_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FMath1_Initialized = 1;

/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"nadd",(LpFUNC)&FMath1_Plus);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ndiv",(LpFUNC)&FMath1_Divide);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"nmod",(LpFUNC)&FMath1_Modulo);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"divr",(LpFUNC)&FMath1_Modulo);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"nmul",(LpFUNC)&FMath1_Multiply);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"nsub",(LpFUNC)&FMath1_Minus);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"/",(LpFUNC)&FMath1_Divide);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"-",(LpFUNC)&FMath1_Minus);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"*",(LpFUNC)&FMath1_Multiply);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"+",(LpFUNC)&FMath1_Plus);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"abs",(LpFUNC)&FMath1_Abs);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"exp",(LpFUNC)&FMath1_Exp);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"log",(LpFUNC)&FMath1_Log);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"log10",(LpFUNC)&FMath1_Log10);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"sqrt",(LpFUNC)&FMath1_Sqrt);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"fact",(LpFUNC)&FMath1_Fact);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ceiling",(LpFUNC)&FMath1_Ceiling);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"floor",(LpFUNC)&FMath1_Floor);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"truncate"), aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"expt",(LpFUNC)&FMath1_Expt);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isEven",(LpFUNC)&FMath1_Even);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isOdd",(LpFUNC)&FMath1_Odd);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isExact",(LpFUNC)&FMath1_Exact);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isInexact",(LpFUNC)&FMath1_Inexact);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"gcd",(LpFUNC)&FMath1_Gcd);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"lcm",(LpFUNC)&FMath1_Lcm);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"mod",(LpFUNC)&FMath1_Modulo);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"modulo"),    aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"remainder"), aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"add1",(LpFUNC)&FMath1_Add1);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"1+"),  aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"sub1",(LpFUNC)&FMath1_Sub1);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"-1+"),  aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"logbase",(LpFUNC)&FMath1_LogBase);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"log2",(LpFUNC)&FMath1_Log2);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"hashString",(LpFUNC)&FMath1_HashString);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeGramMatrix",(LpFUNC)&FMath1_MakeGramMatrix);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"makeGaussianMatrix",(LpFUNC)&FMath1_MakeGaussianMatrix);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"matrixGaussianEliminate",(LpFUNC)&FMath1_MatrixGaussianEliminate);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"matrixGaussianSubstitute",(LpFUNC)&FMath1_MatrixGaussianSubstitute);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cIntegerVector",(LpFUNC)&FMath1_CIntegerVector);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cNumberVector",(LpFUNC)&FMath1_CNumberVector);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cStringVector",(LpFUNC)&FMath1_CStringVector);
ExitOnError(*ec);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Divide

Return the quotient of an arbitrary number of numeric arguments. 

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, return its inverse. 
  
#endif

TVAL FMath1_Divide(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                     index;
REAL                    x;
REAL                    sr;
REAL                    si;
REAL                    ar;
REAL                    ai;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);
EndFrame

asTag(ret) = TYREAL;
asReal(ret) = 1.0;
index = -1;

if (argc == 0) {FrameExit(gCP->TObject_ERROR_INVALID);}

if (argc > 1)
	{	
	++index;
	if (argv[index].Tag == TYCPX)
		{	
		ret->Tag = TYCPX;
		ret->u.Complex = cp = TCpx_New(gCP, gTP);
		cp->itsReal = argv[index].u.Complex->itsReal;
		cp->itsImag = argv[index].u.Complex->itsImag;
		}
	else
    if (argv[index].Tag != TYREAL)
		{	
		*ret = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[index]);
		ExitOnError( *ret);
		}
	else
		asReal(ret) = asNumIndex(&argv[index]);
	}

/* At this point, ret is either Real or Complex */
while (++index < argc)
	{
	if (ret->Tag == TYCPX || argv[index].Tag == TYCPX)
		{	
		if (ret->Tag != TYCPX)	
			{	
			*ret = FConvert_ToComplex(gCP, gTP, *ret);
			ExitOnError(*ret);
			}
		if (argv[index].Tag != TYCPX)	
			{	
			*tmp = FConvert_ToComplex(gCP, gTP, argv[index]);
			ExitOnError(*tmp);
			}
		else
			*tmp = argv[index];

		sr = ret->u.Complex->itsReal;
		si = ret->u.Complex->itsImag;
		ar = tmp->u.Complex->itsReal;
		ai = tmp->u.Complex->itsImag;

		if ((x = (ar * ar) + (ai * ai)) == (REAL)0.0)
			{FrameReset;return gCP->TObject_ERROR_DIVIDE_BY_ZERO;}
		ret->u.Complex->itsReal = ((sr * ar) + (si * ai)) / x;
		ret->u.Complex->itsImag = ((si * ar) - (sr * ai)) / x;
		}
	else
	if (argv[index].Tag != TYREAL)
		{	
		*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[index]);
		ExitOnError(*tmp);
		if (tmp->u.Real == 0.0)
			{FrameReset;return gCP->TObject_ERROR_DIVIDE_BY_ZERO;}
		ret->u.Real /= tmp->u.Real;
		}
	else
		{	
		if (argv[index].u.Real == 0.0)
			{FrameReset;return gCP->TObject_ERROR_DIVIDE_BY_ZERO;}
		ret->u.Real /= argv[index].u.Real;
		}
	}

/* Check the type to see if it can be coerced to an integer */
if (ret->Tag == TYREAL)
	{	
	index = ret->u.Real;
	if (index == ret->u.Real)
		{	
		ret->u.Int = index;
		ret->Tag = TYNUM;
		}
	}

FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Minus

Return the difference of an arbitrary number of numeric arguments. 

Note:   If no arguments are specified, return an error. 
        If only one argument is specified, return its negation. 
  
#endif
TVAL FMath1_Minus(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
NUM                 index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);
EndFrame
 
asTag(ret) = TYREAL;
asReal(ret) = 0.0;
index = -1;

if (argc == 0) {FrameReset;return gCP->TObject_ERROR_INVALID_ARGLIST;}

// If more than one argument, we subtract all the others from it.
if (argc > 1)
{	++index;
	if (argv[index].Tag == TYCPX)
	{	ret->Tag = TYCPX;
		ret->u.Complex = cp = TCpx_New(gCP, gTP);
		cp->itsReal = argv[index].u.Complex->itsReal;
		cp->itsImag = argv[index].u.Complex->itsImag;
	}
	else
	if (argv[index].Tag != TYREAL)
	{	*ret = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[index]);
		ExitOnError(*ret);
	}
	else
		*ret = argv[index];
}
// At this point, ret holds a Real or a Complex
while (++index < argc)
{	
	if (ret->Tag == TYCPX || argv[index].Tag == TYCPX)
	{	if (ret->Tag != TYCPX)	
		{	*ret = FConvert_ToComplex(gCP, gTP, *ret);
			ExitOnError(*ret);
		}
		if (argv[index].Tag != TYCPX)	
		{	*tmp = FConvert_ToComplex(gCP, gTP, argv[index]);
			ExitOnError(*tmp);
		}
		else
			*tmp = argv[index];

		cp = ret->u.Complex;
		cp->itsReal -= tmp->u.Complex->itsReal;
		cp->itsImag -= tmp->u.Complex->itsImag;
	}
	else
	if (argv[index].Tag != TYREAL)
	{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[index]);
		ExitOnError(*tmp);
		asReal(ret) -= asNumIndex(tmp);
	}
	else
		asReal(ret) -= asNumIndex(&argv[index]);
}
// Check the return value to see if it can be coerced to an integer
if (ret->Tag == TYREAL)
{	index = ret->u.Real;
	if (index == ret->u.Real)
	{	ret->u.Int = index;
		ret->Tag = TYNUM;
	}
}
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Multiply

Return the product of an arbitrary number of numeric arguments. 

Note:   If no arguments are specified, return a one. 
  
#endif
TVAL FMath1_Multiply(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                     index;
REAL                    ar;
REAL                    ai;
REAL                    sr;
REAL                    si;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx,cp);
EndFrame
 
asTag(ret) = TYREAL;
asReal(ret) = 1.0;
index = -1;

/* At this point, ret holds a Real	*/
while (++index < argc)
	{	
	if (ret->Tag == TYCPX || argv[index].Tag == TYCPX)
		{	
		if (ret->Tag != TYCPX)	
			{	
			*ret = FConvert_ToComplex(gCP, gTP, *ret);
			ExitOnError(*ret);
			}
		if (argv[index].Tag != TYCPX)	
			{	
			*tmp = FConvert_ToComplex(gCP, gTP, argv[index]);
			ExitOnError(*tmp);
			}
		else
			*tmp = argv[index];

		sr = ret->u.Complex->itsReal;
		si = ret->u.Complex->itsImag;
		ar = tmp->u.Complex->itsReal;
		ai = tmp->u.Complex->itsImag;

		ret->u.Complex->itsReal = (sr * ar) - (si * ai);
		ret->u.Complex->itsImag = (sr * ai) + (si * ar);
		}
	else
	if (argv[index].Tag != TYREAL)
		{	
		*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[index]);
		ExitOnError(*tmp);
		asReal(ret) *= asNumIndex(tmp);
		}
	else
		asReal(ret) *= asNumIndex(&argv[index]);
	}

/* Check the return value to see if it can be coerced to an integer */
if (ret->Tag == TYREAL)
	{	
	index = ret->u.Real;
	if (index == ret->u.Real)
		{	
		ret->u.Int = index;
		ret->Tag = TYNUM;
		}
	}

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Plus

Return the sum of an arbitrary number of numeric arguments. 

Note:   If no arguments are specified, return a zero. 
  
#endif
TVAL FMath1_Plus(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
NUM                     index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame
 
ret->Tag = TYREAL;
ret->u.Real = 0;
index = -1;

// At this point ret holds a Real.
while (++index < argc)
{	
	if (ret->Tag == TYCPX || argv[index].Tag == TYCPX)
	{	if (ret->Tag != TYCPX)	
		{	*ret = FConvert_ToComplex(gCP, gTP, *ret);
			ExitOnError(*ret);
		}
		if (argv[index].Tag != TYCPX)	
		{	*tmp = FConvert_ToComplex(gCP, gTP, argv[index]);
			ExitOnError(*tmp);
		}
		else
			*tmp = argv[index];
		ret->u.Complex->itsReal += tmp->u.Complex->itsReal;
		ret->u.Complex->itsImag += tmp->u.Complex->itsImag;
	}
	else
	if (argv[index].Tag != TYREAL)
	{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[index]);
		ExitOnError(*tmp);
		asReal(ret) += asNumIndex(tmp);
	}
	else
		asReal(ret) += asNumIndex(&argv[index]);

}
// Check the return value to see if it can be coerced to an integer
if (ret->Tag == TYREAL)
{	index = ret->u.Real;
	if (index == ret->u.Real)
	{	ret->u.Int = index;
		ret->Tag = TYNUM;
	}
}
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Abs

Return the absolute value of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif
TVAL FMath1_Abs(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL                    x;
StartFrame
DeclareOBJ(TCpx, cp);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame
 
if (argc != 1) 
	{
	*ret = TERROR("!abs: Expecting a single numeric argument!" );
	FrameExit(*ret);
	}

ret->Tag = TYREAL;
ret->u.Real = 0.0;

switch (argv[0].Tag)
    {
    case TYMONEY:
    case TYREAL:
    case TYDATE:
        ret->u.Real = fabs(argv[0].u.Real);
        break;
        
    case TYNUM:
        ret->u.Real = abs(argv[0].u.Int);
        break;

	case TYCPX:
		cp = argv[0].u.Complex;
		x = (cp->itsReal * cp->itsReal) + (cp->itsImag * cp->itsImag);
		ret->u.Real = sqrt(x);
		break;
        
    default:
        *tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL,argv[0]);
        ExitOnError(*tmp);
        x = tmp->u.Real;
        ret->u.Real = fabs(x);
        break;
    }
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Fact

Takes an number x and gives a factorial of it; fact(x) = x * (x-1) * (x - 2) ... * 1.
Assumes the argument argv[0] is a number (currently it can`t handle very large values 
because of overflows. On exit the factorial value is returned.  x should be a whole number.

Note:   Exactly one argument is expected, anything else is an error. 
#endif
TVAL FMath1_Fact(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
NUM        n;
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(argr);
EndFrame

if (argc != 1)
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST) 

argr->Tag = TYREAL;
argr->u.Real = 1.0;

// Retrieve and verify the input argument
*tmp = FObject_IntAnyCnv(gCP, gTP, TYNUM, argv[0]);
ExitOnError(*tmp);
n = tmp->u.Int;

if(n < 0 || n > 170)
	FrameExit(gCP->TObject_ERROR_INVALID)

if (n == 0) 
    asReal(argr) = 0.0;
else 
{	while (n > 1) 
		argr->u.Real *= n--;
}
    
FrameExit(*argr);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Log2

Returns Log of the specified argument base 2.
  
#endif
TVAL FMath1_Log2(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,2);

EndFrame

if (argc != 1)
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)

prmv[0] = argv[0];
prmv[1].Tag = TYREAL;
prmv[1].u.Real = 2.0;
*ret = FMath1_LogBase(gCP,gTP,2,&prmv[0]);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_LogBase
Returns Log of the specified argument base B. If b is omitted then base 10.
Base is an integer.
#endif
TVAL FMath1_LogBase(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL        x;			// Input arg 
REAL		base;		// Input base
REAL		logBase;	// Log of base

StartFrame
DeclareTVAL(argr);
DeclareTVAL(tmp);
DeclareTVAL(cx);
DeclareTVAL(cbase);
DeclareOBJ(TCpx, cp);
DeclareTVALArray(prmv,3);

EndFrame

// Check inputs
if (argc == 1) 
	FrameExit(FMath1_Log10(gCP,gTP,argc,argv))
if(argc != 2)
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST)
*argr = gCP->Tval_VOID;

// Convert base to real.
*tmp = FObject_RealAnyCnv(gCP, gTP, TYREAL, argv[1]);
ExitOnError(*tmp);
base = tmp->u.Real;
if (base <= 1.0)
	FrameExit(gCP->TObject_ERROR_INVALID)

// Process Complex input arguments
if ((argv[0].Tag == TYCPX) || (argv[1].Tag == TYCPX))
{
	if (argv[0].Tag == TYCPX)
		{
		*cx = argv[0];		
		}
	else
		{
		cx->Tag = TYCPX;
		cx->u.Complex = cp = TCpx_New(gCP, gTP);
		cx->u.Complex->itsReal = asNumIndex(&argv[0]);		
		cx->u.Complex->itsImag = 0;		
		}
		
	if (argv[1].Tag == TYCPX)
		{
		*cbase = argv[1];		
		}
	else
		{
		cbase->Tag = TYCPX;
		cbase->u.Complex = cp = TCpx_New(gCP, gTP);
		cbase->u.Complex->itsReal = asNumIndex(&argv[1]);		
		cbase->u.Complex->itsImag = 0;		
		}

	*cx = FMath1_Log(gCP,gTP,1,cx);
	ExitOnError(*cx);
	*cbase = FMath1_Log(gCP,gTP,1,cbase);
	ExitOnError(*cbase);
	prmv[0] = *cx;
	prmv[1] = *cbase;
	*argr = FMath1_Divide(gCP,gTP,2,&prmv[0]);
	FrameExit(*argr);
}

// Process numeric input
*tmp = FObject_RealAnyCnv(gCP, gTP, TYREAL, argv[0]);
ExitOnError(*tmp);
x = tmp->u.Real;
if (x <= 0.0)
	FrameExit(gCP->TObject_ERROR_INVALID)

logBase = log(base);
argr->Tag = TYREAL;
argr->u.Real = log(x) / logBase;
FrameExit(*argr)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Exp

Return the natural exponential of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath1_Exp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL         x; 
REAL		ex;

StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareTVALArray(prmv,3);

EndFrame
 
if (argc != 1)
	{
	*ret = TERROR("!exp: Expecting a single numeric argument!" );
	FrameExit(*ret);
	}

asTag(ret) = TYREAL;
asReal(ret) = 0.0;

if (asTag(&argv[0]) == TYREAL)
    {	x = argv[0].u.Real;
		ex = exp(x);
		ret->u.Real = ex;
    }
else 
if (asTag(&argv[0]) == TYCPX)
	{
	prmv[0].Tag = TYREAL;
	prmv[0].u.Real = _FMath3_CONSTANT_E;
	prmv[1] = argv[0];
	*ret = FMath1_Expt(gCP,gTP,2,&prmv[0]);
	}
else
    {
    *tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    ExitOnError( *tmp);
    x = asNumIndex(tmp);        
    asReal(ret) = exp(x);
	}

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Log

Return the natural logarithm of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error.  
#endif
TVAL FMath1_Log(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL           x;
REAL		   P;
REAL		   D;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);
EndFrame
 
if (argc != 1) 
{	*ret = TERROR("!log: Expecting a single numeric argument!");
	FrameExit(*ret)
}
ret->Tag = TYREAL;
ret->u.Real = 0.0;

// Process Complex input arguments
if (argv[0].Tag == TYCPX)
{	*tmp = FMath3_Argument(gCP,gTP,1,&argv[0]);
	ExitOnError(*tmp);
	P = asNumIndex(tmp);
	*tmp = FMath1_Abs(gCP,gTP,1,&argv[0]);
	ExitOnError(*tmp);
	D = asNumIndex(tmp);
	ret->Tag = TYCPX;
	ret->u.Complex = cp = TCpx_New(gCP, gTP);
	ret->u.Complex->itsReal = log(D);
	ret->u.Complex->itsImag = P;
	FrameExit(*ret);
}
else
if (asTag(&argv[0]) != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError( *tmp);
	x = asNumIndex(tmp);
}
else
	x = argv[0].u.Real;

ret->u.Real = log(x);
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Log10

Return the decimal logarithm of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath1_Log10(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
REAL           x;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);
DeclareTVALArray(prmv,3);

EndFrame

if (argc != 1)
{	*ret = TERROR("!log10: Expecting a single numeric argument!" );
	FrameExit(*ret)
}
asTag(ret) = TYREAL;
asReal(ret) = 0.0;

// Process Complex input arguments
if (argv[0].Tag == TYCPX)
{   prmv[0] = argv[0];
	prmv[1] = TREAL(10);
	*ret = FMath1_LogBase(gCP,gTP,2,&prmv[0]);
	FrameExit(*ret)
}
else
if (argv[0].Tag != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError(*tmp);
	x = tmp->u.Real;
}
else
    x = argv[0].u.Real;

asReal(ret) = log10(x);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Sqrt

Return the square root of a numeric argument. 

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif
TVAL FMath1_Sqrt(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
REAL			x;
REAL			D;
REAL			P;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);

EndFrame
 
if (argc != 1) 
{	*ret = TERROR("!sqrt: Expecting a single numeric argument!" );
	FrameExit(*ret);
}
ret->Tag = TYREAL;
ret->u.Real = 0.0;

if (argv[0].Tag == TYCPX)
{	P = FMath3_argumentInternal(argv[0].u.Complex->itsReal,argv[0].u.Complex->itsImag);
	D = sqrt((argv[0].u.Complex->itsReal*argv[0].u.Complex->itsReal)+(argv[0].u.Complex->itsImag*argv[0].u.Complex->itsImag));
	D = sqrt(D);
	ret->Tag = TYCPX;
	ret->u.Complex = cp = TCpx_New(gCP, gTP);
	ret->u.Complex->itsReal = FMath3_cosInternal(P/2)*D;
	ret->u.Complex->itsImag = FMath3_sinInternal(P/2)*D;
	if (ret->u.Complex->itsImag == 0.0) ret->u.Complex->itsReal = abs(ret->u.Complex->itsReal);
	FrameExit(*ret);
}
else
if (argv[0].Tag != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError( *tmp);
	x = tmp->u.Real;
}
else 
    x = argv[0].u.Real;

if (x == 0.0)
	{	
	ret->u.Real = 0.0;
	}
else
if (x < 0.0)
	{	
	ret->u.Complex = cp = TCpx_New(gCP, gTP);
	ret->Tag = TYCPX;
	cp->itsReal = 0.0;
	cp->itsImag = sqrt(abs(x));
	}
else
	{
	ret->u.Real = sqrt(x);
	}

FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Ceiling

Return a number which is rounded up to the next highest integer value.

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath1_Ceiling(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL                    x;
StartFrame
DeclareTVAL(ret);
EndFrame
 
if (argc != 1)
	{
	*ret = TERROR("!ceiling: Expecting a single numeric argument!" );
	FrameExit(*ret);
	}

ret->Tag = TYREAL;
ret->u.Real = 0.0;

switch (argv[0].Tag)
	{
	case TYREAL:
	case TYMONEY:
	case TYDATE:
		x = argv[0].u.Real;
		break;

	case TYNUM:
		x = argv[0].u.Int;
		break;

	case TYCPX:
		x = argv[0].u.Complex->itsReal;
		break;

	default:
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		break;
	}

ret->u.Real = ceil(x);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Floor

Return a number which is rounded down to the next lowest integer value.

Note:   Exactly one argument is expected, anything else is an error. 
  
#endif

TVAL FMath1_Floor(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
REAL                    x;
StartFrame
DeclareTVAL(ret);
EndFrame

if (argc != 1)
	{
	*ret = TERROR("!floor: Expecting a single numeric argumentt!" );
	FrameExit(*ret);
	}

 
ret->Tag = TYREAL;
ret->u.Real = 0.0;

switch (argv[0].Tag)
	{
	case TYREAL:
	case TYMONEY:
	case TYDATE:
		x = argv[0].u.Real;
		break;

	case TYCPX:
		x = argv[0].u.Complex->itsReal;
		break;

	case TYNUM:
		x = argv[0].u.Int;
		break;

	default:
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		break;
	}

ret->u.Real = floor(x);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Expt

Note:   Exactly two arguments are expected, anything else is an error.
(expt base power)
Returns: Base to the Power exponent.		
#endif

TVAL FMath1_Expt(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
REAL			a1;
REAL			b1;
REAL			a2;
REAL			b2;
REAL			D;
REAL			P;
REAL			trig;
REAL			base;
REAL			power;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);

EndFrame
 
if (argc != 2)
{	*ret = TERROR("!(expt base power): Need 2 arguments!" );
	FrameExit(*ret)
}
ret->Tag = TYREAL;
ret->u.Real = 0.0;

// Process Complex input arguments
if ((argv[0].Tag == TYCPX) || (argv[1].Tag == TYCPX))
{
	if (argv[0].Tag == TYCPX)
		{
		a1 = argv[0].u.Complex->itsReal;		
		b1 = argv[0].u.Complex->itsImag;		
		}
	else
		{
		a1 = asNumIndex(&argv[0]);		
		b1 = 0;		
		}
		
	if (argv[1].Tag == TYCPX)
		{
		a2 = argv[1].u.Complex->itsReal;		
		b2 = argv[1].u.Complex->itsImag;		
		}
	else
		{
		a2 = asNumIndex(&argv[1]);		
		b2 = 0;		
		}		

	P = FMath3_argumentInternal(a1,b1);
	D = sqrt((a1*a1)+(b1*b1));
	base = pow(D,a2)*exp((-b2)*P);
	trig = (b2*log(D))+(a2*P);
	ret->Tag = TYCPX;
	ret->u.Complex = cp = TCpx_New(gCP, gTP);
	ret->u.Complex->itsReal = base*FMath3_cosInternal(trig);
	ret->u.Complex->itsImag = base*FMath3_sinInternal(trig);
	FrameExit(*ret);
}

// Convert base, power to real and then call pow.
if (argv[0].Tag != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
    ExitOnError(*tmp);
    base = tmp->u.Real;
}
else
	base = argv[0].u.Real;

if (argv[1].Tag != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[1]);
	ExitOnError(*tmp);
    power = tmp->u.Real;
}
else
	power = argv[1].u.Real;

ret->u.Real = pow(base, power);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Sub1

Decrement the numeric argument by one.
Note:   Exactly one argument must be specified, otherwise an error 
        condition results. 
  
#endif
TVAL FMath1_Sub1(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
NUM			index;

StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame
 
if (argc != 1)
{	*ret = TERROR("!sub1: Expecting a single argument!" );
	FrameExit(*ret)
}
asTag(ret) = TYREAL;
asReal(ret) = 0.0;

if (asTag(&argv[0]) != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError( *tmp);
	ret->u.Real = tmp->u.Real;
}
else
	ret->u.Real = argv[0].u.Real;

--(ret->u.Real); 

// Check the type to see if it can be coerced to an integer.
index = ret->u.Real;
if (index == ret->u.Real)
{	ret->u.Int = index;
	ret->Tag = TYNUM;
}
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Gcd

The gcd  cProcedure returns the greatest common divisor of a pair of integers. 

Several examples follow.

    (gcd  206  40)                  =>  2
    (gcd  27   33)                  =>  3
    
Note:   This function is based upon Euclid`s algorithm, which states that
        if r = a % b, then GCD(a,b) == GCD(b,r).

#endif

TVAL FMath1_Gcd(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
        
{
REAL                    r;
REAL                    a;
REAL                    b;
REAL                    x;
REAL                    y;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

// There must be exactly two arguments.

if (argc != 2) 
{	*ret = TERROR("!gcd: Expecting 2 arguments!" );
	FrameExit(*ret)
}
*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
ExitOnError( *tmp);
x = tmp->u.Real;
y = abs(x);
a = floor(y);

*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[1]);
ExitOnError( *tmp);
x = tmp->u.Real;

y = abs(x);
b = floor(y);

/*  Perform Euclid`s algorithm iteratively. */
while (b != 0)
{	x = a / b;
	y = floor(x);
	r = a - (b * y);
	a = b;
	b = r;
}
ret->u.Real = a;
ret->Tag = TYREAL;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Lcm

The lcm  cProcedure returns the least common multiple of a pair of integers. 

Several examples follow.

    (lcm  32  -36)                  =>  288
    (lcm  4   5)                    =>  20
    
Note:   This function is dependent upon the FMath1_Gcd result. If GCD(a,b) == g,
        then LCM(a,b) == ((a / g) * b).

#endif

TVAL FMath1_Lcm(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
        
{
REAL                    g;
REAL                    a;
REAL                    b;
REAL                    x;
REAL                    y;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  There must be exactly two arguments. */

if (argc != 2) 
{	*ret = TERROR("!lcd: Expecting 2 arguments!" );
	FrameExit(*ret)
}

*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
ExitOnError( *tmp);
x = tmp->u.Real;
y = abs(x);
a = floor(y);

*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[1]);
ExitOnError( *tmp);
x = tmp->u.Real;
y = abs(x);
b = floor(y);

//  Compute the GCD of the two numbers.
*ret = FMath1_Gcd(gCP,gTP,argc, argv);
ExitOnError( *tmp);

x = ret->u.Real;
g = (a / x) * b;

asReal(ret) = g;
ret->Tag = TYREAL;
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Even

The FMath1_Even function returns true if the argument is an even integer 
with no fraction. 

Note:   Exactly one argument is expected, anything else is an error.

#endif
TVAL FMath1_Even(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])    
{
NUM			n;
REAL		rx;
REAL		ry;
REAL		rw;
StartFrame
DeclareTVAL(ret);
EndFrame

//  There must be one argument and it must be numeric.
if (argc != 1) 
{	*ret = TERROR("!even: Expecting a single argument!" );
	FrameExit(*ret)
}
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

// If the argument is an integer, it must only be even.

if (argv[0].Tag == TYNUM)
{	n = asInt(&argv[0]);
	if (n & 1)
		ret->u.Bool = FALSE;
	else
		ret->u.Bool = TRUE;
	FrameExit(*ret)
}
//  If the argument is real, we must examine it.

if (asTag(&argv[0]) == TYREAL)
{	rx = asNumIndex(&argv[0]);
	ry = rx / 2;
	rw = floor(ry);
	if (rx == (rw * 2))
		ret->u.Bool = TRUE;
	else
		ret->u.Bool = FALSE;
	FrameExit(*ret)
}
//  If the argument is complex, we must examine it.

if (asTag(&argv[0]) == TYCPX)
{	rx = argv[0].u.Complex->itsReal;
	ry = rx / 2;
	rw = floor(ry);
	if (rx == (rw * 2))
		ret->u.Bool = TRUE;
	else
		ret->u.Bool = FALSE;
	FrameExit(*ret)
}
ret->u.Bool = FALSE;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Odd

The FMath1_Odd function returns true if the argument is an odd integer 
with no fraction. 

Note:   Exactly one argument is expected, anything else is an error.
#endif
TVAL FMath1_Odd(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                     n;
REAL                    rx;
REAL                    ry;
REAL                    rw;
StartFrame
DeclareTVAL(ret);
EndFrame

//  There must be one argument and it must be numeric.
if (argc != 1) 
{	*ret = TERROR("!odd: Expecting a single argument!" );
	FrameExit(*ret)
}
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

//  If the argument is an integer, it must only be odd.
if (asTag(&argv[0]) == TYNUM)
{	n = asInt(&argv[0]);
	if (n & 1)
		ret->u.Bool = TRUE;
	else
		ret->u.Bool = FALSE;
	FrameExit(*ret)
}

//  If the argument is real, we must examine it.
if (argv[0].Tag == TYREAL)
{	rx = asNumIndex(&argv[0]);
	ry = rx / 2;
	rw = floor(ry);
	if (rx == ((rw*2) + 1))
		ret->u.Bool = TRUE;
	else
		ret->u.Bool = FALSE;
	FrameExit(*ret)
}

//  If the argument is complex, we must examine it.
if (argv[0].Tag == TYCPX)
{	rx = argv[0].u.Complex->itsReal;
	ry = rx / 2;
	rw = floor(ry);
	if (rx == ((rw*2) + 1))
		ret->u.Bool = TRUE;
	else
		ret->u.Bool = FALSE;
	FrameExit(*ret)
}

ret->u.Bool = FALSE;
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Exact

The FMath1_Exact function returns true if the argument is an integer 
with no fraction. 

Note:   Exactly one argument is expected, anything else is an error.
#endif
TVAL FMath1_Exact(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
REAL                    rx;
REAL                    ry;
StartFrame
DeclareTVAL(ret);
EndFrame

//  There must be one argument and it must be numeric.
if (argc != 1)
{	*ret = TERROR("!isExact: Expecting a single argument!" );
	FrameExit(*ret)
}
ret->Tag = TYBOLE;
ret->u.Bool = FALSE;

// If the argument is an integer, it is true by definition.
if (argv[0].Tag == TYNUM)
{	ret->u.Bool = TRUE;
	{FrameReset;return *ret;}
}
//  If the argument is real, we must examine it.
if (asTag(&argv[0]) == TYREAL)
{	rx = argv[0].u.Real;
	ry = floor(rx);
	if (rx == ry)
		ret->u.Bool = TRUE;
	else
		ret->u.Bool = FALSE;
	{FrameReset;return *ret;}
}

//  If the argument is complex, we must examine it.
if (asTag(&argv[0]) == TYCPX)
{	rx = argv[0].u.Complex->itsReal;
	ry = floor(rx);
	if (rx == ry)
		ret->u.Bool = TRUE;
	else
		ret->u.Bool = FALSE;
	{FrameReset;return *ret;}
}


ret->u.Bool = FALSE;
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Inexact

The FMath1_Inexact function returns false if the argument is an integer 
with no fraction. 

Note:   Exactly one argument is expected, anything else is an error.
#endif
TVAL FMath1_Inexact(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
REAL		rx;
REAL		ry;
StartFrame
DeclareTVAL(ret);
EndFrame

//  There must be one argument and it must be numeric.
ret->Tag = TYBOLE;
ret->u.Bool = FALSE;
if (argc != 1)
{	*ret = TERROR("!isInexact: Expecting a single argument!" );
	FrameExit(*ret)
}
//  If the argument is an integer, it is false by definition.
if (argv[0].Tag == TYNUM)
{	ret->u.Bool = FALSE;
    FrameExit(*ret)
}
//  If the argument is real, we must examine it.
if (argv[0].Tag == TYREAL)
{	rx = argv[0].u.Real;
	ry = floor(rx);
	ret->u.Bool = (rx == ry) ? FALSE : TRUE;
	FrameExit(*ret)
}
//  If the argument is complex, we must examine it.
if (argv[0].Tag == TYCPX)
{	rx = argv[0].u.Complex->itsReal;
	ry = floor(rx);
	ret->u.Bool = (rx == ry) ? FALSE : TRUE;
	FrameExit(*ret)
}

ret->u.Bool = FALSE;
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Modulo

The mod cProcedure divides two numbers and returns the remainder. 

Examples:
    (mod  5  6)                         =>  5
    (mod  (+ 1  5)  (+ 3  2))           =>  1

#endif
TVAL FMath1_Modulo(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL		x;
REAL		y;
REAL		Fraction;
REAL		Integer;
NUM			index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

if (argc != 2)
{	*ret = TERROR("!mod: Expecting 2 arguments !" );
	FrameExit(*ret)
}
ret->Tag = TYREAL;
ret->u.Real = 0.0;


//  Convert first argument to a real.
if (argv[0].Tag == TYREAL)
	x = argv[0].u.Real;
else if (argv[0].Tag == TYNUM)
	x = (REAL)argv[0].u.Int;
else
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError( *tmp);
	x = tmp->u.Real;
}  
//  Convert second argument to a real.
if (argv[1].Tag == TYREAL)
	y = argv[1].u.Real;
else if (argv[1].Tag == TYNUM)
    y = (REAL)argv[1].u.Int;
else
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[1]);
	ExitOnError(*tmp);
	y = tmp->u.Real;
}
if (y == 0.0)
{	*ret = TERROR("!mod: Divisor cannot equal zero!" );
	FrameExit(*ret)
}  
//  Compute the x modulo y.
Fraction = modf(x / y, &Integer);
ret->u.Real = fabs(y) * Fraction;

// Check the type to see if it can be coerced to an integer
index = ret->u.Real;
if (index == ret->u.Real)
{	ret->u.Int = index;
	ret->Tag = TYNUM;
}
FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_Add1

Increment the numeric argument by one. 

Note:   Exactly one argument must be specified, otherwise an error 
        condition results. 
  
#endif
TVAL FMath1_Add1(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
NUM		index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

if (argc != 1) 
{	*ret = TERROR("!add1: Expecting a single argument!" );
	FrameExit(*ret)
}
ret->Tag = TYREAL;
ret->u.Real = 0.0;

if (argv[0].Tag != TYREAL)
{	*tmp = FObject_RealAnyCnv(gCP,gTP,TYREAL, argv[0]);
	ExitOnError(*tmp);
	ret->u.Real = tmp->u.Real;
}
else
	ret->u.Real = argv[0].u.Real;
++ret->u.Real; 

// Check the type to see if it can be coerced to an integer.
index = ret->u.Real;
if (index == ret->u.Real)
{	ret->u.Int = index;
	ret->Tag = TYNUM;
}
FrameExit(*ret)
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_HashString

Convert the string argument into an integer hash value. 

Note:   Exactly one argument must be specified, otherwise an error 
        condition results. 
  
#endif
TVAL FMath1_HashString(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
NUM			n;
NUM			N;
NUM			hash;
LpCHAR		strptr;
StartFrame
DeclareTVAL(ret);
EndFrame

if (argc != 1)
	{
	*ret = TERROR("!hashString: Expecting a single argument!" );
	FrameExit(*ret);
	}
else
if (argv[0].Tag == TYSTRING)
	{
	strptr = CharArray(argv[0]);
    N = strlen(strptr);
	}
else
if (argv[0].Tag == TYSYMBOL)
	{
	strptr = SymbolArray(argv[0]);
    N = strlen(strptr);
	}
else
if (argv[0].Tag == TYTEXT)
	{
	strptr = argv[0].u.Text;
    N = strlen(strptr);
	}
else
if (argv[0].Tag == TYSTRINGSUBSTR)
    {
    strptr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    if (strptr == NULL)
        FrameExit(gCP->TObject_ERROR_INVALID);
    N = SubLen(argv[0]);
    }
else
	{
	*ret = TERROR("!hashString: Expecting a string argument!" );
	FrameExit(*ret);
	}

hash = -1;
for (n = 0; n < N; ++n)
	{
	hash += (1000003*(NUM)strptr[n]*(NUM)strptr[n]);
	}

FrameExit(TINT(hash));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_MakeGramMatrix

summary:  Returns the N by N matrix containing the dot products of the
          row vectors from the input matrix, known as the Gramm matrix.

Parms:    X:         The N by M matrix representing the original independent
		             variable observations (the dependent variable is omitted)
                     in the form of:    x x ... x
                                        x x ... x
                                            ... 
                                        x x ... x
		  Y:         (Optional)An N number vector containing the dependent variable.
Return:   G:         The N by N matrix containing the dot products of the row 
                     vectors of the original observation matrix X,
                     where: G[r,c] = vectorDotProduct(rowX[r],rowX[c]);

Note:     See Cristianini, "Support Vector Machines", page 169.

#endif
TVAL FMath1_MakeGramMatrix(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
NUM				yRowCount = 0;
NUM				rowCount;
NUM				r1;
NUM				r2;
NUM				yColCount = 0;
NUM				colCount;
NUM				c;
NUM				gramRowSize;
NUM				selfRowSize;
REAL			product;
REAL			dotProduct;
LpREAL			selfArray;
LpREAL			yArray = NULL;
LpREAL			gramArray;
LpREAL			r1LP;
LpREAL			r2LP;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareOBJ(TNumVector,yVector);
DeclareOBJ(TNumMatrix,gramMatrix);
DeclareTVAL(ec);
DeclareTVAL(ret);
EndFrame

/* Validate the argument and initialize the return value. */
if ((argc == 2) && (argv[1].Tag == TYNUMVECTOR)) 
	{
	yVector = argv[1].u.NumVector;
	yArray = RealArray(argv[1]);
	yRowCount = yVector->itsMaxItemIndex;
	yColCount = 1;
	--argc;
	}
if ((argc != 1) || (argv[0].Tag != TYNUMMATRIX)) 
	{
	BadArg:
	*ec = TERROR("!makeGramMatrix: expecting a rank two numeric matrix argument!" );
	FrameExit(*ec)
	}
self = argv[0].u.NumMatrix;
if (self->itsRank != 2)  goto BadArg;
rowCount = self->itsDimensions[0];
colCount = self->itsDimensions[1];
if ((yRowCount != 0) && (yRowCount != rowCount))  goto BadArg;
selfRowSize = colCount;
gramRowSize = (rowCount+yColCount);

/* Initialize the Gram Matrix (the return value). */
/* Note: The Gram matrix is always square, symmetric */
/*       and positive definite (has positive Eigenvalues). */
gramMatrix = TNumMatrix_New(gCP,gTP);
*ret = TOBJ(gramMatrix);
TNumMatrix_SetMaxIndex(gCP, gTP, *ret, rowCount*(rowCount+yColCount));
gramMatrix->itsRank = 2;
gramMatrix->itsDimensions[2] = 1;
gramMatrix->itsDimensions[1] = (rowCount+yColCount);
gramMatrix->itsDimensions[0] = rowCount;
selfArray = RealMatrix(argv[0]);
gramArray = RealMatrix(*ret);


/* Fill the Gram matrix with the dot product of the original row vectors. */
/* Note: Since the gram matrix is always symmetric, we only calculate the */
/*       dot products for the upper triangular portion and replicate the */
/*       values for the lower triangular portion of the result matrix. */
for (r1 = 0; r1 < rowCount; ++r1) 
	{
	r1LP = &selfArray[r1*selfRowSize];
	for (r2 = r1; r2 < rowCount; ++r2) 
		{
		r2LP = &selfArray[r2*selfRowSize];
		dotProduct = 0.0;
		for (c = 0; c < colCount; ++c) 
			{
			product = r1LP[c]*r2LP[c];
			dotProduct += REALCHECK(product);
			}
		gramArray[(r1*gramRowSize)+r2] = dotProduct;
		gramArray[(r2*gramRowSize)+r1] = dotProduct;
		}
	if (yRowCount > 0) gramArray[(r1*gramRowSize)+r2] = yArray[r1];
	}


FrameExit(*ret)
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_MakeGaussianMatrix

summary:  Returns the M by M+1 system of linear equations representing
          the coefficient derivative equations for the minimizing the
		  least squares error.

Parms:    XY:        The N by M+1 matrix representing the original independent
		             variable observations with the dependent variable in 
					 the last column in the form of:    
										x x x x ... y
                                        x x x x ... y
                                            ... 
                                        x x x x ... y
Return:   G:         The M by M+1 matrix containing the dot products of the column 
                     vectors of the original observation matrix XY,
                     where: G[r,c] = vectorDotProduct(colX[r],colX[c]);

Note1:    See Sedgewick[2] chap 38.
Note2:	  The Gaussian matrix contains one row for each independent variable and
		  an extra column for the dependent variable. Each cell in the Gaussian
		  matrix is the statistical cross correlation of the specified independent 
		  variable (row index) with the specified correlation variable (col index).
		  The final column of the Gaussian matrix is the statistical cross
		  correlation of the dependent variable with each of the independent 
		  variables.
#endif

TVAL FMath1_MakeGaussianMatrix(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
NUM				colCount;
NUM				c1;
NUM				c2;
NUM				rowCount;
NUM				r;
NUM				gausRowSize;
NUM				selfRowSize;
REAL			product;
REAL			dotProduct;
LpREAL			selfArray;
LpREAL			gausArray;
LpREAL			rLP;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareOBJ(TNumMatrix,gausMatrix);
DeclareTVAL(ec);
DeclareTVAL(ret);
EndFrame

/* Validate the argument and initialize the return value. */
if ((argc != 1) || (argv[0].Tag != TYNUMMATRIX)) 
	{
	BadArg:
	*ec = TERROR("!makeGaussianMatrix: expecting a rank two numeric matrix argument!" );
	FrameExit(*ec)
	}
self = argv[0].u.NumMatrix;
if (self->itsRank != 2) goto BadArg;
rowCount = self->itsDimensions[0];
colCount = self->itsDimensions[1];
selfRowSize = colCount;
gausRowSize = colCount;

/* Initialize the Gaussian Matrix (the return value). */
gausMatrix = TNumMatrix_New(gCP,gTP);
*ret = TOBJ(gausMatrix);
TNumMatrix_SetMaxIndex(gCP, gTP, *ret, (colCount-1)*colCount);
gausMatrix->itsRank = 2;
gausMatrix->itsDimensions[2] = 1;
gausMatrix->itsDimensions[1] = colCount;
gausMatrix->itsDimensions[0] = colCount-1;
selfArray = RealMatrix(argv[0]);
gausArray = RealMatrix(*ret);


/* Fill the Gaussian matrix with the dot product of the original col vectors. */
for (c1 = 0; c1 < colCount-1; ++c1) 
	{
	for (c2 = 0; c2 < colCount; ++c2) 
		{
		dotProduct = 0.0;
		rLP= &selfArray[0];
		for (r = 0; r < rowCount; ++r) 
			{
			/* product = selfArray[(r*selfRowSize)+c1]*selfArray[(r*selfRowSize)+c2];*/
			product = rLP[c1]*rLP[c2];
			dotProduct += REALCHECK(product);
			rLP += selfRowSize;
			}
		gausArray[(c1*gausRowSize)+c2] = dotProduct;
		}
	}


FrameExit(*ret)
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_MatrixGaussianEliminate

summary:  Triangulates the M by M+1 coefficient matrix representing 
		  a system of M simultaneous linear equations in M variables.

Parms:    XY:        The M by M+1 matrix representing the original independent
		             variable observations with the dependent variable in the
					 last column in the form of:    
										x x x x ... y
                                        x x x x ... y
                                            ... 
                                        x x x x ... y
		  destroySW  (Optional) True iff the input matrix is to be destroyed 
					 by performing the Gaussian elimination operation in the
					 matrix supplied as input.
Return:   T:         The M by M+1 matrix after triangulation. The triangulated
					 matrix is now ready for Gaussian substitution.

Note1:    See Sedgewick[2] chap 37, uses row swapping.
Note2:	  This function implements an approximate form of Gaussian 
          elimination by fudging around singular conditions where
          dividing by zero would be a problem.
#endif

TVAL FMath1_MatrixGaussianEliminate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
NUM				colCount;
NUM				c1;
NUM				rowCount;
NUM				r1;
NUM				r2;
NUM				gausRowSize;
NUM				selfRowSize;
NUM				maxRow;
REAL			coefficient1;
REAL			coefficient2;
REAL			product;
LpREAL			selfArray;
LpREAL			gausArray;
LpREAL			maxRowLP;
LpREAL			r1LP;
LpREAL			r2LP;
BOLE			destroySW = FALSE;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareOBJ(TNumMatrix,gausMatrix);
DeclareTVAL(ec);
DeclareTVAL(ret);
EndFrame

/* Validate the arguments and initialize the return value. */
if ((argc < 1) || (argv[0].Tag != TYNUMMATRIX)) 
	{
	BadArg:
	*ec = TERROR("!matrixGaussianEliminate: expecting an M by M+1 numeric matrix argument!" );
	FrameExit(*ec)
	}
/* Check for the destroy switch argument. */
if ((argc == 2) && (argv[1].Tag == TYBOLE) && (argv[1].u.Bool == TRUE)) 
	{
	destroySW = TRUE;
	}
self = argv[0].u.NumMatrix;
if (self->itsRank != 2) goto BadArg;
rowCount = self->itsDimensions[0];
colCount = self->itsDimensions[1];
selfRowSize = colCount;
gausRowSize = colCount;
if (rowCount != (colCount-1)) goto BadArg;


if (destroySW == TRUE)
	{
	/* Initialize the triangulated Gaussian Matrix (the return value). */
	/* Note: This operation WILL destroy the input matrix. */
	gausMatrix = self;
	*ret = TOBJ(gausMatrix);
	selfArray = RealMatrix(argv[0]);
	gausArray = RealMatrix(*ret);
	}
else
	{
	/* Initialize the triangulated Gaussian Matrix (the return value). */
	/* Note: This operation will NOT destroy the input matrix. */
	gausMatrix = TNumMatrix_New(gCP,gTP);
	*ret = TOBJ(gausMatrix);
	TNumMatrix_SetMaxIndex(gCP, gTP, *ret, rowCount*colCount);
	gausMatrix->itsRank = 2;
	gausMatrix->itsDimensions[2] = 1;
	gausMatrix->itsDimensions[1] = colCount;
	gausMatrix->itsDimensions[0] = rowCount;
	selfArray = RealMatrix(argv[0]);
	gausArray = RealMatrix(*ret);

	/* Copy the contents of the input matrix to the result matrix. */
	/* Note: This function is non-destructive. */
	for (r1 = 0; r1 < self->itsMaxItemIndex; ++r1) 
		{
		gausArray[r1] = selfArray[r1];
		}
	}

/* Perform Gaussian elimination to form the triangulated matrix. */
/* Note: The result matrix is an upper triangulated matrix. */
for (r1 = 0; r1 < rowCount-1; ++r1) 
	{
	/* Determine the row with the largest coefficient. */
	/* Note: This is in preparation for later row swapping */
	/*       which helps to reduce round off errors which */
	/*       are a serious source of arithmetic errors otherwise. */
	maxRow = r1;
	for (r2 = r1+1; r2 < rowCount; ++r2) 
		{
		coefficient1 = abs(gausArray[(r2*gausRowSize)+r1]);
		coefficient2 = abs(gausArray[(maxRow*gausRowSize)+r1]);
		if (coefficient1 > coefficient2) maxRow = r2;
		}
	/* Perform row swapping which helps to reduce round off errors */
	/* which are a serious source of arithmetic errors otherwise. */
	/* Note: The contents of the row, with the maximum coefficient, */
	/*       are swapped with the contents of the current row. */
	maxRowLP = &gausArray[(maxRow*gausRowSize)];
	r1LP = &gausArray[(r1*gausRowSize)];
	for (c1 = 0; c1 < colCount; ++c1) 
		{
		coefficient1 = r1LP[c1];
		r1LP[c1] = maxRowLP[c1];
		maxRowLP[c1] = coefficient1;
		}
	/* Perform Gaussian elimination to triangulate the current row. */
	for (r2 = r1+1; r2 < rowCount; ++r2) 
		{
		r2LP = &gausArray[(r2*gausRowSize)];
		for (c1 = colCount-1; c1 >= r1; --c1) 
			{
			/* Fudge around singular condition iff we would be dividing by zero. */
			/* Note: This occurs whenever the matrix is singular and a slight fudge */
			/*       introduces very little error and allows approximate elimination. */
			if (r1LP[r1] == 0.0)
				{
				if (r2LP[r1] == 0.0)
					{
					product = r2LP[c1] - r1LP[c1];
					r2LP[c1] = REALCHECK(product);
					}
				else
					{
					product = r2LP[c1] - (r1LP[c1] * ((1+r2LP[r1]) / (1+r1LP[r1])));
					r2LP[c1] = REALCHECK(product);
					}
				}
			else	
				{
				product = r2LP[c1] - (r1LP[c1] * (r2LP[r1] / r1LP[r1]));
				r2LP[c1] = REALCHECK(product);
				}
			}
		}
	}


FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_MatrixGaussianSubstitute

summary:  Returns the M coefficient number vector from a triangulated array 
		  representing the solution of a triangulated system of M 
		  simultaneous linear equations in M variables.
		  
Parms:    TXY:       The M by M+1 matrix representing the original independent
		             variable observations with the dependent variable in 
					 the last column all having been triangulated via the
					 Gaussian elimination in the form of:    
										x x x x ... x y
                                        0 x x x ... x y
                                        0 0 x x ... x y
                                              ... 
                                        0 0 0 0 ... x y
Return:   C:         The M coefficient number vector representing the
					 solution to the original system of M simultaneous
					 equations in M unknowns.

Note:     See Sedgewick[2] chap 37, assumes input matrix is triangulated.
#endif

TVAL FMath1_MatrixGaussianSubstitute(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
NUM				colCount;
NUM				c1;
NUM				rowCount;
NUM				r1;
NUM				selfRowSize;
REAL			coefficient1;
REAL			coefficient2;
REAL			product;
LpREAL			selfArray;
LpREAL			coefArray;
LpREAL			r1LP;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareOBJ(TNumVector,coefVector);
DeclareTVAL(ec);
DeclareTVAL(ret);
EndFrame

/* Validate the argument and initialize the return value. */
if ((argc != 1) || (argv[0].Tag != TYNUMMATRIX)) 
	{
	BadArg:
	*ec = TERROR("!matrixGaussianSubstitute: expecting an M by M+1 triangulated numeric matrix argument!" );
	FrameExit(*ec)
	}
self = argv[0].u.NumMatrix;
if (self->itsRank != 2) goto BadArg;
rowCount = self->itsDimensions[0];
colCount = self->itsDimensions[1];
selfRowSize = colCount;
if (rowCount != (colCount-1)) goto BadArg;

/* Initialize the coefficient number Vector (the return value). */
coefVector = TNumVector_New(gCP,gTP);
*ret = TOBJ(coefVector);
TNumVector_SetMaxIndex(gCP, gTP, *ret, colCount-1);
selfArray = RealMatrix(argv[0]);
coefArray = RealArray(*ret);

/* Perform Gaussian substitution from the last row up to the */
/* top row to find the M coefficients of the solution. */
for (r1 = rowCount-1; r1 >= 0; --r1) 
	{
	r1LP = &selfArray[(r1*selfRowSize)];
	/* Compute the product of the previous coefficients before substituting. */
	product = 0.0;
	for (c1 = r1+1; c1 < rowCount; ++c1) 
		{
		coefficient1 = product + r1LP[c1] * coefArray[c1];
		product = REALCHECK(coefficient1);
		}
	/* Fudge around singular condition iff we would be dividing by zero. */
	/* Note: This occurs whenever the matrix is singular and a slight fudge */
	/*       introduces very little error and allows approximate elimination. */
	if (r1LP[r1] == 0.0)
		{
		if (r1LP[rowCount] == 0.0)
			{
			coefArray[r1] = 0.0;
			}
		else
			{
			coefficient2 = ((r1LP[rowCount] - product)+1) / (1+r1LP[r1]);
			coefArray[r1] = REALCHECK(coefficient2);
			}
		}
	else	
		{
		coefficient2 = (r1LP[rowCount] - product) / r1LP[r1];
		coefArray[r1] = REALCHECK(coefficient2);
		}
	}


FrameExit(*ret)
}


/**************************************************************************************** */
/*******************************C Function Speed/Timing Tests**************************** */
/**************************************************************************************** */


/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_CIntegerVector

Timing utility to test the speed of a simple loop transfering data between three 
Integer Vectors passed as arguments.

ARGS:		argv[0]	Argument Integer Vector.
			argv[1]	Source Integer Vector.
			argv[2]	Destination Integer Vector.

RETURN:		argv[2] Returns the destination vector 

EXAMPLE:	(cIntegerVector (new Vector: Integer: 10000 1 -2 3 -4 5 -3 10 -20 3 8) 
			                (new Vector: Integer: 10000 1 2 3 4 5 6 7 8 9 10) 
							(new Vector: Integer: 10000 1 2 3 4 5 6 7 8 9 10))

NOTES:		[1]This timing function always destructively returns the destination vector with
			    the alterations which have been made.
			[2]The calling Lisp Lambda should create large vectors in order to have the timings 
			   be as representative as possible.

PARENT:             None. 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

TVAL FMath1_CIntegerVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM				m;
NUM				M1;
NUM				M2;
NUM				n;
NUM				N;
NUM				x;
NUM				A;
LpNUM			pvA;
LpNUM			pv1;
LpNUM			pv2;
StartFrame
DeclareOBJ(TIntVector,vA);
DeclareOBJ(TIntVector,v1);
DeclareOBJ(TIntVector,v2);
EndFrame

/*
**  Get the number of times to repeat this timing test.
*/
if (argc != 3) 
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

if (argv[0].Tag == TYINTVECTOR)
	{
    vA = argv[0].u.IntVector;
	pvA = ((LpNUM)*(vA->itsIntArray));
	N = vA->itsMaxItemIndex;
	}
else
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

if (argv[1].Tag == TYINTVECTOR)
	{
    v1 = argv[1].u.IntVector;
	pv1 = ((LpNUM)*(v1->itsIntArray));
	M1 = v1->itsMaxItemIndex;
	}
else
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

if (argv[2].Tag == TYINTVECTOR)
	{
    v2 = argv[2].u.IntVector;
	pv2 = ((LpNUM)*(v2->itsIntArray));
	M2 = v2->itsMaxItemIndex;
	if (M1 != M2) 
		{
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
	}
else
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}


/* Move data from the source vector to the destination vector. */
for (n = 0; n < N; ++n)
	{
	A = pvA[n];
	for (m = 0; m < M1; ++m)
		{
		x = pv1[m];
		x = x + A;
		x = x * x;
		x = x / A;
		pv2[m] = x;
		}
	}
 
FrameExit(argv[2]);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_CNumberVector

Timing utility to test the speed of a simple loop transfering data between three 
Number Vectors passed as arguments.

ARGS:		argv[0]	Argument Number Vector.
			argv[1]	Source Number Vector.
			argv[2]	Destination Number Vector.

RETURN:		argv[2] Returns the destination vector 

EXAMPLE:	(cNumberVector  (new Vector: Number: 10000 1.1 -2.2 3.7 -4.6 5.3 -3.8 10.3 -20.2 3.2 8.1) 
			                (new Vector: Number: 10000 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3) 
							(new Vector: Number: 10000 1.1 2.4 3.8 4.3 5.6 6.9 7.1 8.7 9.1 10.3))

NOTES:		[1]This timing function always destructively returns the destination vector with
			    the alterations which have been made.
			[2]The calling Lisp Lambda should create large vectors in order to have the timings 
			   be as representative as possible.

PARENT:             None. 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

TVAL FMath1_CNumberVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM				m;
NUM				M1;
NUM				M2;
NUM				n;
NUM				N;
REAL			x;
REAL			A;
LpREAL			pvA;
LpREAL			pv1;
LpREAL			pv2;
StartFrame
DeclareOBJ(TNumVector,vA);
DeclareOBJ(TNumVector,v1);
DeclareOBJ(TNumVector,v2);
EndFrame

/*
**  Get the number of times to repeat this timing test.
*/
if (argc != 3) 
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

if (argv[0].Tag == TYNUMVECTOR)
	{
    vA = argv[0].u.NumVector;
	pvA = ((LpREAL)*(vA->itsRealArray));
	N = vA->itsMaxItemIndex;
	}
else
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

if (argv[1].Tag == TYNUMVECTOR)
	{
    v1 = argv[1].u.NumVector;
	pv1 = ((LpREAL)*(v1->itsRealArray));
	M1 = v1->itsMaxItemIndex;
	}
else
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

if (argv[2].Tag == TYNUMVECTOR)
	{
    v2 = argv[2].u.NumVector;
	pv2 = ((LpREAL)*(v2->itsRealArray));
	M2 = v2->itsMaxItemIndex;
	if (M1 != M2) 
		{
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
	}
else
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}


/* Move data from the source vector to the destination vector. */
for (n = 0; n < N; ++n)
	{
	A = pvA[n];
	for (m = 0; m < M1; ++m)
		{
		x = pv1[m];
		x = x + A;
		x = x * x;
		x = x / A;
		pv2[m] = x;
		}
	}
 
FrameExit(argv[2]);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath1_CStringVector

Timing utility to test the speed of a simple loop transfering result codes from String 
comparison between two Vectors to a third Integer Vector. 

ARGS:		argv[0]	Argument Vector.
			argv[1]	Source Vector.
			argv[2]	Destination Integer Vector.

RETURN:		argv[2] Returns the destination vector 

EXAMPLE:	(cStringVector  (new Vector: 10000 "Hello" "Test" "xyz") 
			                (new Vector: 10000 "Hello1" "Test1" "xyz1") 
							(new Vector: Integer: 10000 0 1 0 ))

NOTES:		[1]This timing function always destructively returns the destination vector with
			    the alterations which have been made.
			[2]The calling Lisp Lambda should create large vectors in order to have the timings 
			   be as representative as possible.

PARENT:             None. 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

TVAL FMath1_CStringVector(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM				M1;
NUM				M2;
NUM				n;
NUM				N;
NUM				result;
LpCHAR			lp;
LpCHAR			rp;
LpTVAL			pv1;
LpTVAL			pv2;
LpNUM			pv3;
StartFrame
DeclareTVAL(left);
DeclareTVAL(right);
DeclareOBJ(TVector,v1);
DeclareOBJ(TVector,v2);
DeclareOBJ(TIntVector,v3);
EndFrame

/*
**  Get the number of times to repeat this timing test.
*/
if (argc != 3) 
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

if (argv[0].Tag == TYVECTOR)
	{
    v1 = argv[0].u.Vector;
	pv1 = ((LpTVAL)*(v1->itsTvalArray));
	N = v1->itsMaxItemIndex;
	}
else
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

if (argv[1].Tag == TYVECTOR)
	{
    v2 = argv[1].u.Vector;
	pv2 = ((LpTVAL)*(v2->itsTvalArray));
	M1 = v2->itsMaxItemIndex;
	}
else
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

if (argv[2].Tag == TYINTVECTOR)
	{
    v3 = argv[2].u.IntVector;
	pv3 = ((LpNUM)*(v3->itsIntArray));
	M2 = v3->itsMaxItemIndex;
	if ((M1 != M2) || (N != M1))
		{
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
	}
else
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}


/* Compare Strings from the source vector and argument            */
/* vectors, placing the result codes in the destination vector.   */
for (n = 0; n < N; ++n)
	{
	*left = pv1[n];
	*right = pv2[n];

	/* Point to the left string */
	if (left->Tag == TYTEXT) lp = left->u.Text;
	else
	if (left->Tag == TYSTRING) lp = CharArray(*left);
	else
    if (left->Tag == TYSTRINGSUBSTR) lp = TStringSubstringT_GetStringPtr(gCP, gTP, *left);
    else
	if (left->Tag == TYSYMBOL) lp = SymbolArray(*left);
	else
	if (left->Tag == TYQUOTEDSYMBOL) lp = SymbolArray(*left);
	else
	if (left->Tag == TYBYTEVECTOR) lp = ByteArray(*left);
	else FrameExit(TERROR("!CStringVector: invalid left string object!"));

	/* Point to the right string */
	if (right->Tag == TYTEXT) rp = right->u.Text;
	else
	if (right->Tag == TYSTRING) rp = CharArray(*right);
	else
    if (right->Tag == TYSTRINGSUBSTR) rp = TStringSubstringT_GetStringPtr(gCP, gTP, *right);
    else
	if (right->Tag == TYSYMBOL) rp = SymbolArray(*right);
	else
	if (right->Tag == TYQUOTEDSYMBOL) rp = SymbolArray(*right);
	else
	if (right->Tag == TYBYTEVECTOR) rp = ByteArray(*right);
	else FrameExit(TERROR("!CStringVector: invalid right string object!"));

    /* Comparison with substring are done differently */
    if (left->Tag == TYSTRINGSUBSTR)
        {
        if (right->Tag == TYSTRINGSUBSTR)
            result = substr2cmp(lp, rp, asSubLen(left), asSubLen(right));
        else
            result = substrrcmp(lp, asSubLen(left), rp);
        }
    else
    if (right->Tag == TYSTRINGSUBSTR)
        {
        if (left->Tag == TYSTRINGSUBSTR)
            result = substr2cmp(lp, rp, asSubLen(left), asSubLen(right));
        else
            result = substrlcmp(lp, rp, asSubLen(right));
        }
    else
	    result = strcmp(lp,rp);

	pv3[n] = result;
	}
 
FrameExit(argv[2]);
}
