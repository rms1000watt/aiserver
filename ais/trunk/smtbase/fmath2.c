/**********************************************************************************
    Copyright (C) 2013 Analytic Research Foundation.

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

#define _C_FMATH2
#define _SMARTBASE

#if 0
FMath2.c

This source file contains some of the cProcedures which implement the math functions
supported by the SmartLisp interpreter.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include    "fmath2.h"
#include    "fmath3.h"
#include    "fmath1.h"
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tdirect.h"
#include    "tdiction.h"
#include    "tvector.h"
#include    "tbitvec.h"
#include    "tbytevec.h"
#include    "tnumvec.h"
#include    "tfltvec.h"
#include    "tintvec.h"
#include    "tobjvec.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "fconvert.h"
#include    "flisp.h"
#include    "ffloat.h"
#include    "tmatrix.h"
#include    "tnummat.h"
#include    "fpred.h"
#include    "fpred2.h"
#include    "futil1.h"
#include	"tcpx.h"
#include	"tcpxvec.h"
#include	"tshortvec.h"


/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Init

Initialize the math portion of the SmartLisp function library.  

#endif

TVAL FMath2_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareTVAL(ec);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,savSymbol);
EndFrame
 
if(gCP->FMath2_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FMath2_Initialized = TRUE;

/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"addi",(LpFUNC)&FMath2_Addi);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"addu",(LpFUNC)&FMath2_Addu);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bitwiseShiftLeft",(LpFUNC)&FMath2_bitSHL);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bitwiseShiftRight",(LpFUNC)&FMath2_bitSHR);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"iadd",(LpFUNC)&FMath2_Addi);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"uadd",(LpFUNC)&FMath2_Addu);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"idiv",(LpFUNC)&FMath2_Divi);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"udiv",(LpFUNC)&FMath2_Divu);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"imod",(LpFUNC)&FMath2_Modi);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"umod",(LpFUNC)&FMath2_Modu);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"imul",(LpFUNC)&FMath2_Muli);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"umul",(LpFUNC)&FMath2_Mulu);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"isub",(LpFUNC)&FMath2_Subi);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"usub",(LpFUNC)&FMath2_Subu);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cadd",(LpFUNC)&FMath2_Addc);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"cdiv",(LpFUNC)&FMath2_Divc);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"cmod",(LpFUNC)&FMath2_Modc);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"cmul",(LpFUNC)&FMath2_Mulc);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"csub",(LpFUNC)&FMath2_Subc);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"divi",(LpFUNC)&FMath2_Divi);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&savSymbol,(LpCHAR)"divu",(LpFUNC)&FMath2_Divu);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"quotient"), savSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"modi",(LpFUNC)&FMath2_Modi);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"divri",(LpFUNC)&FMath2_Modi);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"modu",(LpFUNC)&FMath2_Modu);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"divru",(LpFUNC)&FMath2_Modu);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"muli",(LpFUNC)&FMath2_Muli);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"mulu",(LpFUNC)&FMath2_Mulu);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"subi",(LpFUNC)&FMath2_Subi);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"subu",(LpFUNC)&FMath2_Subu);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"randomize",(LpFUNC)&FMath2_Randomize);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"random",(LpFUNC)&FMath2_Random);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"srandom",(LpFUNC)&FMath2_Random);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"round",(LpFUNC)&FMath2_Round);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"max",(LpFUNC)&FMath2_Max);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"min",(LpFUNC)&FMath2_Min);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"avg",(LpFUNC)&FMath2_AVG);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"product",(LpFUNC)&FMath2_PRODUCT);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"sum",(LpFUNC)&FMath2_SUM);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"count",(LpFUNC)&FMath2_COUNT);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"sort",(LpFUNC)&FMath2_Sort);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bitwiseOr",(LpFUNC)&FMath2_Or);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bitwiseAnd",(LpFUNC)&FMath2_And);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bitwiseXor",(LpFUNC)&FMath2_Xor);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bitwiseNor",(LpFUNC)&FMath2_Nor);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bitwiseNand",(LpFUNC)&FMath2_Nand);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bitwiseNot",(LpFUNC)&FMath2_BitwiseNot);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bitwiseNxor",(LpFUNC)&FMath2_Nxor);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"binaryNor",(LpFUNC)&FMath2_Norb);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"binaryNand",(LpFUNC)&FMath2_Nandb);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"binaryNot",(LpFUNC)&FMath2_BinaryNot);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"binaryNxor",(LpFUNC)&FMath2_Nxorb);
ExitOnError(*ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"reduceOr",(LpFUNC)&FMath2_ROr);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"reduceAnd",(LpFUNC)&FMath2_RAnd);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"reduceXor",(LpFUNC)&FMath2_RXor);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"reduceNor",(LpFUNC)&FMath2_RNor);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"reduceNand",(LpFUNC)&FMath2_RNand);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"reduceNxor",(LpFUNC)&FMath2_RNxor);
ExitOnError(*ec);
FrameExit(gCP->TObject_OK);
}
/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Addu

The addu  Procedure returns the sum of its argument as an Integer quantity.  

For example

    (addu  12.3  5  6)  =>  23
    (addu  -4  -6  5)   =>  5
    (addu  42.6)    =>  42

#endif

TVAL FMath2_Addu(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
UNUM                aUInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

ret->Tag = TYUNUM;
ret->u.Int = 1;
index = -1;

if (argc == 0) 
	{
	*ret = TERROR("!addu: Expecting a minimum of one argument!" );
	FrameExit(*ret);
	}

++index;
if (argv[index].Tag != TYUNUM)
    {
    *tmp = FObject_IntAnyCnv(gCP,gTP,TYUNUM, argv[index]);
    ExitOnError( *tmp);
    *ret = *tmp;
    }
else
    {
    *ret = argv[index];
    }

while (++index < argc)
    {
    if (argv[index].Tag != TYUNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYUNUM, argv[index]);
        ExitOnError( *tmp);
        aUInt = tmp->u.UInt;
        ret->u.UInt += aUInt;
        }
    else
        {
        ret->u.UInt += argv[index].u.UInt;
        }
    } 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Addi

The addi  Procedure returns the sum of its argument as an Integer quantity.  

For example

    (addi  12.3  5  6)  =>  23
    (addi  -4  -6  5)   =>  -5
    (addi  42.6)    =>  42

#endif

TVAL FMath2_Addi(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
NUM                 anInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

asTag(ret) = TYNUM;
asReal(ret) = 1;
index = -1;

if (argc == 0) 
	{
	*ret = TERROR("!addi: Expecting a minimum of one argument!" );
	FrameExit(*ret);
	}

++index;
if (asTag(&argv[index]) != TYNUM)
    {
    *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
    ExitOnError( *tmp);
    *ret = *tmp;
    }
else
    {
    *ret = argv[index];
    }

while (++index < argc)
    {
    if (asTag(&argv[index]) != TYNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
        ExitOnError( *tmp);
        anInt = asInt(tmp);
        asInt(ret) += anInt;
        }
    else
        {
        asInt(ret) += asInt(&argv[index]);
        }
    } 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_bitSHL

The bitwiseShiftLeft Procedure bitwise left shifts the first number by the 
number of bits specified in the second number and returns the result as an Integer.

For example
    (bitwiseShiftLeft  4  2)      =>  16
    (bitwiseShiftLeft  4  2.2)    =>  16
    (bitwiseShiftLeft  3.3  1)    =>  6

#endif

TVAL FMath2_bitSHL(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
NUM                 anInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame


/*  There must be exactly two arguments. */
if (argc != 2) 
	{
	*ret = TERROR("!bitwiseShiftLeft: Expecting 2 arguments!" );
	FrameExit(*ret);
	}

asTag(ret) = TYNUM;
asReal(ret) = 1;
index = -1;

++index;
if (asTag(&argv[index]) != TYNUM)
    {
    *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
    ExitOnError( *tmp);
    *ret = *tmp;
    }
else
    {
    *ret = argv[index];
    }

while (++index < argc)
    {
    if (asTag(&argv[index]) != TYNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
        ExitOnError( *tmp);
        anInt = asInt(tmp);
        asInt(ret)  = asInt(ret) << anInt;
        }
    else
        {
        asInt(ret)  = asInt(ret) << asInt(&argv[index]);
        }
    } 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_bitSHR

The bitwiseShiftRight Procedure bitwise right shifts the first number by the 
number of bits specified in the second number and returns the result as an Integer.
    
For example
    (bitwiseShiftRight  4  2)     =>  1
    (bitwiseShiftRight  4  2.2)   =>  1
    (bitwiseShiftRight  6.3  1)   =>  3

#endif

TVAL FMath2_bitSHR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
NUM                 anInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  There must be exactly two arguments. */
if (argc != 2) 
	{
	*ret = TERROR("!bitwiseShiftRight: Expecting 2 arguments!" );
	FrameExit(*ret);
	}


asTag(ret) = TYNUM;
asReal(ret) = 1;
index = -1;


++index;
if (asTag(&argv[index]) != TYNUM)
    {
    *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
    ExitOnError( *tmp);
    *ret = *tmp;
    }
else
    {
    *ret = argv[index];
    }

while (++index < argc)
    {
    if (asTag(&argv[index]) != TYNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
        ExitOnError( *tmp);
        anInt = asInt(tmp);

        asInt(ret)  = asInt(ret) >> anInt;
        }
    else
        {
        asInt(ret)  = asInt(ret) >> asInt(&argv[index]);
        }
    } 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Divu

The divu Procedure returns the quotient of its arguments as an Unsigned Integer quantity. 

For example
    (divu  12  3  4.4)  =>  1
    (divu  8.4  -4)     =>  2
    (divi  20  4.4)     =>  5

#endif

TVAL FMath2_Divu(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
NUM                 aUInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

ret->Tag = TYUNUM;
ret->u.UInt = 1;
index = -1;

if (argc == 0) 
	{
	*ret = TERROR("!divu: Expecting at least one argument!" );
	FrameExit(*ret);
	}


if (argc > 1)
    {
    ++index;
    if (argv[index].Tag != TYUNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYUNUM, argv[index]);
        ExitOnError( *tmp);
        *ret = *tmp;
        }
    else
        {
        *ret = argv[index];
        }
    }

while (++index < argc)
    {
    if (argv[index].Tag != TYUNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYUNUM, argv[index]);
        ExitOnError( *tmp);
        aUInt = tmp->u.UInt;
        if (aUInt == 0) 
			{
			*ret = TERROR("!divu: Divisor cannot equal zero!" );
			FrameExit(*ret);
			}

        ret->u.UInt /= aUInt;
        }
    else
        {
        if (argv[index].u.UInt == 0)
			{
			*ret = TERROR("!divu: Divisor cannot equal zero!" );
			FrameExit(*ret);
			}

        ret->u.UInt /= argv[index].u.UInt;
        }
    } 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Divi

The divi Procedure returns the quotient of its arguments as an Integer quantity. 

For example
    (divi  12  3  4.4)  =>  1
    (divi  8.4  -4)     =>  -2
    (divi  20  4.4)     =>  5

#endif

TVAL FMath2_Divi(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
NUM                 anInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

asTag(ret) = TYNUM;
asReal(ret) = 1;
index = -1;

if (argc == 0) 
	{
	*ret = TERROR("!divi: Expecting at least one argument!" );
	FrameExit(*ret);
	}


if (argc > 1)
    {
    ++index;
    if (asTag(&argv[index]) != TYNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
        ExitOnError( *tmp);
        *ret = *tmp;
        }
    else
        {
        *ret = argv[index];
        }
    }

while (++index < argc)
    {
    if (asTag(&argv[index]) != TYNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
        ExitOnError( *tmp);
        anInt = asInt(tmp);
        if (anInt == 0) 
			{
			*ret = TERROR("!divi: Divisor cannot equal zero!" );
			FrameExit(*ret);
			}

        asInt(ret) /= anInt;
        }
    else
        {
        if (asInt(&argv[index]) == 0)
			{
			*ret = TERROR("!divi: Divisor cannot equal zero!" );
			FrameExit(*ret);
			}

        asInt(ret) /= asInt(&argv[index]);
        }
    } 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Modi

The modi Procedure divides two numbers and returns the remainder as an Integer.  

For example
    (modi  12  5.3) =>  2
    (modi  8.4  -4) =>  0
    (modi  21  4.4) =>  1

#endif

TVAL FMath2_Modi(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
NUM                 anInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  There must be exactly two arguments. */
if (argc < 2)
	{
	*ret = TERROR("!modi: Expecting 2 arguments!" );
	FrameExit(*ret);
	}


asTag(ret) = TYNUM;
asReal(ret) = 1;
index = -1;


++index;
if (asTag(&argv[index]) != TYNUM)
    {
    *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
    ExitOnError( *tmp);
    *ret = *tmp;
    }
else
    {
    *ret = argv[index];
    }

while (++index < argc)
    {
    if (asTag(&argv[index]) != TYNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
        ExitOnError( *tmp);
        anInt = asInt(tmp);
        if (anInt == 0) 
			{
			*ret = TERROR("!modi: Divisor cannot equal zero!" );
			FrameExit(*ret);
			}
        asInt(ret) %= anInt;
        }
    else
        {
        if (asInt(&argv[index]) == 0)
			{
			*ret = TERROR("!modi: Divisor cannot equal zero!" );
			FrameExit(*ret);
			}
        asInt(ret) %= asInt(&argv[index]);
        }
    } 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Modu

The modi Procedure divides two numbers and returns the remainder as an Unsigned Integer.  

For example
    (modu  12  5.3) =>  2
    (modu  8.4  -4) =>  0
    (modu  21  4.4) =>  1

#endif

TVAL FMath2_Modu(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
UNUM                aUInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  There must be exactly two arguments. */
if (argc < 2)
	{
	*ret = TERROR("!modu: Expecting 2 arguments!" );
	FrameExit(*ret);
	}


ret->Tag = TYUNUM;
ret->u.UInt = 1;
index = -1;


++index;
if (argv[index].Tag != TYUNUM)
    {
    *tmp = FObject_IntAnyCnv(gCP,gTP,TYUNUM, argv[index]);
    ExitOnError( *tmp);
    *ret = *tmp;
    }
else
    {
    *ret = argv[index];
    }

while (++index < argc)
    {
    if (argv[index].Tag != TYUNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYUNUM, argv[index]);
        ExitOnError( *tmp);
        aUInt = tmp->u.UInt;
        if (aUInt == 0) 
			{
			*ret = TERROR("!modu: Divisor cannot equal zero!" );
			FrameExit(*ret);
			}
        ret->u.UInt %= aUInt;
        }
    else
        {
        if (argv[index].u.UInt == 0)
			{
			*ret = TERROR("!modu: Divisor cannot equal zero!" );
			FrameExit(*ret);
			}
        ret->u.UInt %= argv[index].u.UInt;
        }
    } 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Mulu

The muli Procedure returns the product of its arguments as an Unsigned Integer. 

For example
    (mulu  2  3.6  4)   =>  24
    (mulu  2  -4.1) =>  8
    (mulu  4  5)    =>  20

#endif

TVAL FMath2_Mulu(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
NUM                 aUInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

ret->Tag = TYUNUM;
ret->u.UInt = 1;
index = -1;

if (argc == 0) 
	{
	*ret = TERROR("!mulu: Expecting at least one argument!" );
	FrameExit(*ret);
	}

++index;
if (argv[index].Tag != TYUNUM)
    {
    *tmp = FObject_IntAnyCnv(gCP,gTP,TYUNUM, argv[index]);
    ExitOnError( *tmp);
    *ret = *tmp;
    }
else
    {
    *ret = argv[index];
    }

while (++index < argc)
    {
    if (argv[index].Tag != TYUNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
        ExitOnError( *tmp);
        aUInt = tmp->u.UInt;
        ret->u.UInt *= aUInt;
        }
    else
        {
        ret->u.UInt *= argv[index].u.UInt;
        }
    } 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Muli

The muli Procedure returns the product of its arguments as an Integer. 

For example
    (muli  2  3.6  4)   =>  24
    (muli  2  -4.1) =>  -8
    (muli  4  5)    =>  20

#endif

TVAL FMath2_Muli(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
NUM                 anInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

asTag(ret) = TYNUM;
asReal(ret) = 1;
index = -1;

if (argc == 0) 
	{
	*ret = TERROR("!muli: Expecting at least one argument!" );
	FrameExit(*ret);
	}

++index;
if (asTag(&argv[index]) != TYNUM)
    {
    *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
    ExitOnError( *tmp);
    *ret = *tmp;
    }
else
    {
    *ret = argv[index];
    }

while (++index < argc)
    {
    if (asTag(&argv[index]) != TYNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
        ExitOnError( *tmp);
        anInt = asInt(tmp);
        asInt(ret) *= anInt;
        }
    else
        {
        asInt(ret) *= asInt(&argv[index]);
        }
    } 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Subu

The subi Procedure returns the difference of its arguments as an Unsigned Integer quantity.

For example
    (subu 12  3.3  4)   =>  5
    (subu  2  -4)   =>  6
    (subu  42.4  6) =>  36

#endif

TVAL FMath2_Subu(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
NUM                 aUInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

ret->Tag = TYUNUM;
ret->u.UInt = 1;
index = -1;

if (argc == 0) 
	{
	*ret = TERROR("!subu: Expecting at least one argument!" );
	FrameExit(*ret);
	}

++index;
if (argv[index].Tag != TYUNUM)
    {
    *tmp = FObject_IntAnyCnv(gCP,gTP,TYUNUM,argv[index]);
    ExitOnError( *tmp);
    *ret = *tmp;
    }
else
    {
    *ret = argv[index];
    }

while (++index < argc)
    {
    if (argv[index].Tag != TYUNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYUNUM, argv[index]);
        ExitOnError( *tmp);
        aUInt = tmp->u.UInt;
        ret->u.UInt -= aUInt;
        }
    else
        {
        ret->u.UInt -= argv[index].u.UInt;
        }
    } 
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Subi

The subi Procedure returns the difference of its arguments as an Integer quantity.

For example
    (subi 12  3.3  4)   =>  5
    (subi  2  -4)   =>  6
    (subi  42.4  6) =>  -36

#endif

TVAL FMath2_Subi(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                 index;
NUM                 anInt;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

asTag(ret) = TYNUM;
asReal(ret) = 1;
index = -1;

if (argc == 0) 
	{
	*ret = TERROR("!subi: Expecting at least one argument!" );
	FrameExit(*ret);
	}

++index;
if (asTag(&argv[index]) != TYNUM)
    {
    *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
    ExitOnError( *tmp);
    *ret = *tmp;
    }
else
    {
    *ret = argv[index];
    }

while (++index < argc)
    {
    if (asTag(&argv[index]) != TYNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[index]);
        ExitOnError( *tmp);
        anInt = asInt(tmp);
        asInt(ret) -= anInt;
        }
    else
        {
        asInt(ret) -= asInt(&argv[index]);
        }
    } 
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Randomize

The randomize  cProcedure changes the seed for the random number 
generator. The value of the seed is always returned. 

An example follows.
    
    (randomize  345643)         =>  345643

Note:	Based on algorithm 266 by Pike and Hill (Modified by Hansson).
		Communications of the ACM, Vol 8, No. 10, October 1965.

#endif

TVAL FMath2_Randomize(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
EndFrame	

argc = argc; // NOOP to hide unused parameter warning message

rand();

/* Always return the dummy argument */

FrameExit(argv[0]);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Random

The random function returns a random number between
zero and the specified numeric argument. 

An example follows.
    
    (random  2)         =>  1.56928564

Note:	The standard C library rand function is used.

#endif

TVAL FMath2_Random(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL		randMax;
REAL		upperLimit;
REAL		rn;
StartFrame
DeclareTVAL(randomNumber);
DeclareTVAL(ret);
EndFrame

/*  There must be one and only one numeric argument. */

if (argc != 1) 
	{
	*ret = TERROR("!random: Expecting one argument!" );
	FrameExit(*ret);
	}

switch (argv[0].Tag)
	{
	case TYREAL:
	case TYMONEY:
	case TYDATE:
		upperLimit = argv[0].u.Real;
		break;

	case TYNUM:
		upperLimit = argv[0].u.Int;
		break;

	default:
		*ret = TERROR("!random: Expecting argument to be a number!" );
		FrameExit(*ret);
		break;
	}

/*  Generate the random number. */

randMax = RAND_MAX;
rn = rand();

/* Always return the generated random number */

randomNumber->Tag = TYREAL;
randomNumber->u.Real = rn * (1.0 / randMax);
randomNumber->u.Real *= upperLimit;
FrameExit(*randomNumber);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Round

The round cProcedure rounds the number to the closest integer. 
Several examples follow.

    (round  4.2)                    =>  4
    (round  4.50)                   =>  4
    (round  4.75)                   =>  5
    (round  -4.2)                   =>  -4

#endif

TVAL FMath2_Round(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                     sign;
REAL                    places;
REAL                    tens;
REAL                    x;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

 
/*  There must be one and only one numeric argument. */

if (argc != 1) 
	{
	*ret = TERROR("!round: Expecting one argument!" );
	FrameExit(*ret);
	}

asTag(ret) = TYREAL;
asReal(ret) = 0.0;


if (asTag(&argv[0]) != TYREAL)
    {
    *tmp = FObject_RealAnyCnv(gCP,gTP,TYNUM, argv[0]);
    ExitOnError( *tmp);
    x = asNumIndex(tmp);
    }
else
    {
    x = asNumIndex(&argv[0]);
    }
    
sign  = (x >= 0.0 ? 1 : -1);
x *= sign;

if(argc == 2)
    {
    if (asTag(&argv[1]) != TYNUM)
        {
        *tmp = FObject_IntAnyCnv(gCP,gTP,TYNUM, argv[1]);
        ExitOnError( *tmp);
        places = asInt(tmp);
        }
    else
        {
        places = asInt(&argv[1]);
        }
    }
else
    places = 0;
    
tens = pow(10.0, (REAL)places);
    
    
modf(((x * tens) + 0.5),&x);
x = (sign * x) / tens;

asReal(ret) = x;
FrameExit( *ret); 
}

/*--------------------------------------------------------------------------------------- */
#if 0

Note:   The remaining functions in this file support the implementation of Lisp map and 
        mapc functionality. Each of these functions utilizes a static callback function
        and may store results in one or more static variables as side effects of execution.
        
#endif

/* Static variables are used to store the result of mapc as a side effect. */
/* A callback function is used for all map and mapc evaluations. */

/*--------------------------------------------------------------------------------------- */
/*--------------------------------------------------------------------------------------- */

static TVAL MathSUMCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#if 0
FMath2_SUM

Return the sum of the arguments. The arguments my be vectors, ranges,
cells, or numeric values.

        (sum A1:C5 20 X 40)

Note:   The FMath2_SUM function expects at least one argument. In addition the FMath2_SUM 
function utilizes a static Callback Function and two static variables which
are used to store side effects.

#endif

TVAL FMath2_SUM(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM			index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TCpx, cp);
EndFrame

//  Initialization of totals and call back function.
gTP->FMath2_realResult = 0.0;
gTP->FMath2_imagResult = 0.0;
gTP->FMath2_intTotal = 0;
gTP->FMath2_boolInitial = TRUE;				// Flag to indicate complex sum
asTag(&gTP->FMath2_CallBackTval) = TYCFUNCTION;
asPointer(&gTP->FMath2_CallBackTval) = (POINTER)&MathSUMCallback;

//  Sum each argument.
for(index=0; index < argc; index++)
{	*tmp = MathSUMCallback(gCP,gTP,1,&argv[index]);
	if (tmp->Tag == TYERROR) FrameExit(*tmp);
}
//  Return the result total
if (!gTP->FMath2_boolInitial)
{	ret->Tag = TYCPX;
	ret->u.Complex = cp = TCpx_New(gCP, gTP);
	cp->itsReal = gTP->FMath2_realResult;
	cp->itsImag = gTP->FMath2_imagResult; 
}
else
{	ret->Tag = TYREAL;
	ret->u.Real = gTP->FMath2_realResult;
}
FrameExit(*ret);
}

/*  The SUM call back function. */
/*  *************************** */
static TVAL MathSUMCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{

argc = argc; // NOOP to hide unused parameter warning message
switch(argv[0].Tag)
	{
	case TYDATE:
	case TYMONEY:
	case TYREAL:
		gTP->FMath2_intTotal++;
		gTP->FMath2_realResult += argv[0].u.Real;
	break;

	case TYNUM:
	case TYCHAR:
		gTP->FMath2_intTotal++;
		gTP->FMath2_realResult += (REAL)argv[0].u.Int;
	break;
	case TYCPX:
		gTP->FMath2_intTotal++;
		gTP->FMath2_boolInitial = FALSE;
		if (argv[0].u.Complex != NIL)
		{	gTP->FMath2_realResult += argv[0].u.Complex->itsReal;
			gTP->FMath2_imagResult += argv[0].u.Complex->itsImag;
		}
	break;
	case TYVOID:
		gTP->FMath2_intTotal++;
	break;

	default:
		if (_TObject_TypeFlag(asTag(&argv[0])) == _TObject_TfTOBJECT)
			{
			/* Use mapc to extract elements of a repeating object */
			return(TObject_Mapc(gCP, gTP, argv[0], gTP->FMath2_CallBackTval));
			}
		break;
    }
	return(argv[0]);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_AVG

Return the average of the arguments. The arguments may be vectors, ranges,
cells, or numeric values.

        (avg A1:C5 20 X 40)

Note:   The FMath2_AVG function expects at least one argument. In addition the FMath2_AVG 
        function utilizes a static Callback Function and two static variables which
        are used to store side effects.

#endif

TVAL FMath2_AVG(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TCpx, cp);
EndFrame

// Calling FMath2_SUM generates both a SUM and the number of arguments evaluated.
FMath2_SUM(gCP,gTP,argc, argv);

if (!gTP->FMath2_boolInitial)
{	ret->Tag = TYCPX;
	ret->u.Complex = cp = TCpx_New(gCP, gTP);
	if (gTP->FMath2_intTotal > 0)
	{	cp->itsReal = gTP->FMath2_realResult / gTP->FMath2_intTotal;
	}	cp->itsImag = gTP->FMath2_imagResult / gTP->FMath2_intTotal;
}
else
{	ret->Tag = TYREAL;
	ret->u.Real = (gTP->FMath2_intTotal <= 0) ? 0 :
		gTP->FMath2_realResult / gTP->FMath2_intTotal;
}
FrameExit(*ret)

}

/*--------------------------------------------------------------------------------------- */

static TVAL MathPRODUCTCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#if 0
FMath2_PRODUCT

Return the product of the arguments. The arguments my be vectors, ranges,
cells, or numeric values.

        (product A1:C5 20 X 40)

Note:   The FMath2_PRODUCT function expects at least one argument. In addition the 
        FMath2_PRODUCT function utilizes a static Callback Function and two static 
        variables which are used to store side effects.

#endif

TVAL FMath2_PRODUCT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 index;
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareOBJ(TCpx, cp);
EndFrame

gTP->FMath2_realResult = 1.0;
gTP->FMath2_imagResult = 0.0;
gTP->FMath2_intTotal = 0;
gTP->FMath2_boolInitial = TRUE;
asTag(&gTP->FMath2_CallBackTval) = TYCFUNCTION;
asPointer(&gTP->FMath2_CallBackTval) = (POINTER)&MathPRODUCTCallback;

for(index=0; index < argc; index++)
{	*tmp = MathPRODUCTCallback(gCP,gTP,1, &argv[index]);
	ExitOnError(*tmp);
}

if (!gTP->FMath2_boolInitial)
{	ret->Tag = TYCPX;
	ret->u.Complex = cp = TCpx_New(gCP, gTP);
	if (gTP->FMath2_intTotal > 0)
	{	cp->itsReal = gTP->FMath2_realResult;
	}	cp->itsImag = gTP->FMath2_imagResult;
}
else
{	ret->Tag = TYREAL;
	ret->u.Real = (gTP->FMath2_intTotal <= 0) ? 0.0 : gTP->FMath2_realResult;
}
FrameExit( *ret);
}


static TVAL MathPRODUCTCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
REAL		ra, ia, rr, ir;	// Real and imaginary parts of argv[0] and result

argc = argc; // NOOP to hide unused parameter warning message
switch(argv[0].Tag)
	{
	case TYDATE:
	case TYMONEY:
	case TYREAL:
		gTP->FMath2_intTotal++;
		gTP->FMath2_realResult *= argv[0].u.Real;
		if (gTP->FMath2_boolInitial == FALSE)
			gTP->FMath2_imagResult *= argv[0].u.Real;
		break;

	case TYNUM:
	case TYCHAR:
		gTP->FMath2_intTotal++;
		gTP->FMath2_realResult *= (REAL)argv[0].u.Int;
		if (gTP->FMath2_boolInitial == FALSE)
			gTP->FMath2_imagResult *= (REAL)argv[0].u.Int;

		break;
	case TYCPX:
		gTP->FMath2_intTotal++;
		gTP->FMath2_boolInitial = FALSE;
		if (argv[0].u.Complex != NIL)
		{	ra = argv[0].u.Complex->itsReal;
			ia = argv[0].u.Complex->itsImag;
			rr = gTP->FMath2_realResult;
			ir = gTP->FMath2_imagResult;
			gTP->FMath2_realResult = ra * rr - ia * ir;
			gTP->FMath2_imagResult = rr * ia + ra * ir;
		}
		break;
	case TYVOID:
		gTP->FMath2_intTotal++;
		break;

	default:
		if (_TObject_TypeFlag(asTag(&argv[0])) == _TObject_TfTOBJECT)
		{	// Use mapc to multiply the native elements of a repeating object
			return(TObject_Mapc(gCP, gTP, argv[0], gTP->FMath2_CallBackTval));
		}
		break;
    }
return(argv[0]);
}

/*--------------------------------------------------------------------------------------- */

static TVAL MathCOUNTCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#if 0
FMath2_COUNT

Return the count of the non-void arguments. The arguments may be vectors, ranges,
cells, or numeric values.

        (count A1:C5 20 X 40)

Note:   The FMath2_COUNT function expects at least one argument. In addition the 
        FMath2_COUNT function utilizes a static Callback Function and two static 
        variables which are used to store side effects.

#endif

TVAL FMath2_COUNT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM		index;
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(ret);
EndFrame

gTP->FMath2_intTotal = 0;
gTP->FMath2_boolInitial = TRUE;
asTag(&gTP->FMath2_CallBackTval) = TYCFUNCTION;
asPointer(&gTP->FMath2_CallBackTval) = (POINTER)&MathCOUNTCallback;


for(index=0; index < argc; index++)
{	*tmp = MathCOUNTCallback(gCP,gTP,1, &argv[index]);
	ExitOnError( *tmp);
}   
ret->Tag = TYNUM;
ret->u.Int = gTP->FMath2_intTotal;
FrameExit(*ret);
}

static TVAL MathCOUNTCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareOBJ(TPair,pp);
EndFrame

argc = argc; // NOOP to hide unused parameter warning message
//  Is the target object a List?
if (isLexMorphPair(&argv[0]))
{	++gTP->FMath2_intTotal;
	pp = (TPair*)asObject(&argv[0]);
	while (isLexMorphPair(&pp->itsCdr) && (asObj(&pp->itsCdr) != NIL))
	{	++gTP->FMath2_intTotal;
		pp = (TPair*)asObject(&pp->itsCdr);
	}
}
else if (argv[0].Tag == TYCPX)
	gTP->FMath2_intTotal++;
else if (isNumIndex(&argv[0]) || argv[0].Tag == TYVOID || asTag(&argv[0]) == TYCHAR || 
	argv[0].Tag == TYTEXT || argv[0].Tag == TYSTRINGSUBSTR || argv[0].Tag == TYSTRING || argv[0].Tag == TYBOLE  ||
	argv[0].Tag == TYSYMBOL || argv[0].Tag == TYQUOTEDSYMBOL )
{	// Count one for REAL, NUM, CHAR, TEXT, STRING, BOOLEAN, SYMBOL, AND QUOTEDSYMBOL
	gTP->FMath2_intTotal++;
}
else if (_TObject_TypeFlag(asTag(&argv[0])) == _TObject_TfTOBJECT)
{	// Use mapc to count the number of native elements in a repeating object
	FrameExit(TObject_Mapc(gCP, gTP, argv[0], gTP->FMath2_CallBackTval));
}
FrameExit(argv[0]);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FMath2_Max

Return the maximum of the arguments. The arguments may be of any data type.

#endif

TVAL FMath2_Max(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 index;
TVAL				compareFN = TFUNCTION(&FPredicate2_Compare);
TVAL				applyFN = TFUNCTION(&FUtil1_Apply);
TVAL				maxFN = TFUNCTION(&FMath2_Max);
StartFrame
DeclareTVAL(ec);
DeclareTVAL(tmp);
DeclareTVAL(ret);
EndFrame

/* Set the initial value of the maximum item. */

*ret = gCP->TObject_VOID;

/* Find the value of the maximum item. */

for (index = 0; index < argc; ++index)
	{
	switch (argv[index].Tag)
		{
		case TYVECTOR:
		case TYNUMVECTOR:
		case TYINTVECTOR:
		case TYLONGVECTOR:
		case TYSHORTVECTOR:
		case TYFLTVECTOR:
		case TYMATRIX:
		case TYNUMMATRIX:
		case TYSTRUCTURE:
		case TYDIRECTORY:
		case TYDICTIONARY:
		case TYCPXVECTOR:
			*tmp = FSmartbase_Eval(gCP,gTP,applyFN,2,maxFN,argv[index]);
			ExitOnError(*tmp);
			*ec = FSmartbase_Eval(gCP,gTP,compareFN,2,*ret,*tmp);
			ExitOnError(*ec);
			if (ec->u.Int < 0) *ret = *tmp;
			break;

		default:
			*ec = FSmartbase_Eval(gCP,gTP,compareFN,2,*ret,argv[index]);
			ExitOnError(*ec);
			if (ec->u.Int < 0) *ret = argv[index];
			break;
		}
	}

FrameExit(*ret);
}
/*--------------------------------------------------------------------------------------- */

// static TVAL MathMinCallback(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#if 0
FMath2_Min

Return the minimum of the arguments. The arguments may be of any data type.

#endif

TVAL FMath2_Min(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 index;
TVAL				compareFN = TFUNCTION(&FPredicate2_Compare);
TVAL				applyFN = TFUNCTION(&FUtil1_Apply);
TVAL				minFN = TFUNCTION(&FMath2_Min);
StartFrame
DeclareTVAL(ec);
DeclareTVAL(tmp);
DeclareTVAL(ret);
EndFrame

/* Set the initial value of the minimum item. */

if (argc == 0) 
	{
	FrameExit(gCP->Tval_VOID);
	}
else
	{
	switch (argv[0].Tag)
		{
		case TYVECTOR:
		case TYNUMVECTOR:
		case TYINTVECTOR:
		case TYSHORTVECTOR:
		case TYLONGVECTOR:
		case TYFLTVECTOR:
		case TYMATRIX:
		case TYNUMMATRIX:
		case TYSTRUCTURE:
		case TYDIRECTORY:
		case TYDICTIONARY:
		case TYCPXVECTOR:
			*ret = FSmartbase_Eval(gCP,gTP,applyFN,2,minFN,argv[0]);
			ExitOnError(*ret);
			break;

		default:
			*ret = argv[0];
			break;
		}
	}

/* Find the value of the minimum item. */

for (index = 1; index < argc; ++index)
	{
	switch (argv[index].Tag)
		{
		case TYVECTOR:
		case TYNUMVECTOR:
		case TYINTVECTOR:
		case TYSHORTVECTOR:
		case TYLONGVECTOR:
		case TYFLTVECTOR:
		case TYMATRIX:
		case TYNUMMATRIX:
		case TYSTRUCTURE:
		case TYDIRECTORY:
		case TYDICTIONARY:
		case TYCPXVECTOR:
			*tmp = FSmartbase_Eval(gCP,gTP,applyFN,2,minFN,argv[index]);
			ExitOnError(*tmp);
			*ec = FSmartbase_Eval(gCP,gTP,compareFN,2,*ret,*tmp);
			ExitOnError(*ec);
			if (ec->u.Int > 0) *ret = *tmp;
			break;

		default:
			*ec = FSmartbase_Eval(gCP,gTP,compareFN,2,*ret,argv[index]);
			ExitOnError(*ec);
			if (ec->u.Int > 0) *ret = argv[index];
			break;
		}
	}

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_And

The bitwiseAnd  Procedure numerically bitwise ands each expression from left to right, 
returning the value of the result as an Integer. 

For example
    (bitwiseAnd  (=  12  12)  (>  2  1))   =>  1
    (bitwiseAnd  (=  24  23)  (<  1  2))   =>  0
    (bitwiseAnd  1  2  4  8)               =>  0
    (bitwiseAnd  1  3  5  7)               =>  1

#endif

TVAL FMath2_And(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
NUM                     theBits;
NUM                     tmpBits;
NUM                     indexOf;
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYNUM;
asInt(ret) = 1;
if (argc == 0) FrameExit(*ret);
theBits = -1;

/*  Return the result of bitwise anding each argument. */

indexOf = 0;
while (indexOf < argc)
    {
    if  isNumIndex(&argv[indexOf])
        {
        tmpBits = asNumIndex(&argv[indexOf]);
		indexOf++;
        theBits &= tmpBits;
        }
    else
    if (asTag(&argv[indexOf]) == TYUNUM)
        {
        theBits &= argv[indexOf++].u.UInt;
        }
    else
    if (asTag(&argv[indexOf]) == TYNUM)
        {
        theBits &= argv[indexOf++].u.Int;
        }
    else
    if (asTag(&argv[indexOf]) == TYCHAR)
        {
        theBits &= asChar(&argv[indexOf++]);
        }
    else
    if (asTag(&argv[indexOf]) == TYBOLE)
        {
        if (asBool(&argv[indexOf++]) == TRUE)
            theBits &= 1;
        else
			{
			++indexOf;
            theBits &= FALSE;
			}
        }
    else
        {
		++indexOf;
        theBits &= FALSE;
        }
    }
    
asInt(ret) = theBits;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Nand

The bitwiseNand  Procedure numerically bitwise not ands each expression from left to right, 
returning the value of the result as an Integer. 

#endif

TVAL FMath2_Nand(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret = FMath2_And(gCP,gTP,argc,argv);
ExitOnError(*ret);
ret->u.Int = ret->u.Int ^ -1;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_BitwiseNot

The bitwiseNot  Procedure numerically bitwise not ands the single expression to itself, 
returning the value of the result as an Integer. 

#endif

TVAL FMath2_BitwiseNot(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
TVAL		prmv[2];
StartFrame
DeclareTVAL(ret);
EndFrame
 
argc = argc; // NOOP to hide unused parameter warning message
prmv[0] = prmv[1] = argv[0];
*ret = FMath2_And(gCP,gTP,2,prmv);
ExitOnError(*ret);
ret->u.Int = ret->u.Int ^ -1;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Nandb

The binaryNand  Procedure numerically bitwise not ands each expression from left to right, 
returning the value of the result as a binary Integer. 

#endif

TVAL FMath2_Nandb(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret		 = FMath2_And(gCP,gTP,argc,argv);
ExitOnError(*ret);
ret->u.Int = !ret->u.Int;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_BinaryNot

The binaryNot  Procedure numerically bitwise not ands the single argument to itself, 
returning the value of the result as a binary Integer. 

#endif

TVAL FMath2_BinaryNot(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
TVAL		prmv[2];
StartFrame
DeclareTVAL(ret);
EndFrame
 
argc = argc; // NOOP to hide warning
prmv[0] = prmv[1] = argv[0];
*ret = FMath2_And(gCP,gTP,2,prmv);
ExitOnError(*ret);
ret->u.Int = !ret->u.Int;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Nor

The bitwiseNor  Procedure numerically bitwise not ors each expression from left to right, 
returning the value of the result as an Integer. 

#endif

TVAL FMath2_Nor(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret = FMath2_Or(gCP,gTP,argc,argv);
ExitOnError(*ret);
ret->u.Int = ret->u.Int ^ -1;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Norb

The binaryNor  Procedure numerically bitwise not ors each expression from left to right, 
returning the value of the result as a binary Integer. 

#endif

TVAL FMath2_Norb(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret = FMath2_Or(gCP,gTP,argc,argv);
ExitOnError(*ret);
ret->u.Int = !ret->u.Int;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Nxor

The bitwiseNxor  Procedure numerically bitwise not xors each expression from left to right, 
returning the value of the result as an Integer. 

#endif

TVAL FMath2_Nxor(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret = FMath2_Xor(gCP,gTP,argc,argv);
ExitOnError(*ret);
ret->u.Int = ret->u.Int ^ -1;

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Nxorb

The binaryNxor  Procedure numerically bitwise not xors each expression from left to right, 
returning the value of the result as a binary Integer. 

#endif

TVAL FMath2_Nxorb(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret = FMath2_Xor(gCP,gTP,argc,argv);
ExitOnError(*ret);
ret->u.Int = !ret->u.Int;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Or

The bitwiseOr  Procedure numerically bitwise ors each expression from left to right, 
returning the value of the result as an Integer. 

For example
    (bitwiseOr  (=  12  12)  (>  2  1))    =>  1
    (bitwiseOr  (=  24  23)  (<  1  2))    =>  1
    (bitwiseOr  1  2  4  8)                =>  15

#endif

TVAL FMath2_Or(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
NUM                     theBits;
NUM                     tmpBits;
NUM                     indexOf;
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYNUM;
asInt(ret) = 1;
if (argc == 0) FrameExit(*ret);
theBits = 0;

/*  Return the result of bitwise oring each argument. */

indexOf = 0;
while (indexOf < argc)
    {
    if isNumIndex(&argv[indexOf])
        {
        tmpBits = asNumIndex(&argv[indexOf]);
		indexOf++;
        theBits |= tmpBits;
        }
    else
    if (asTag(&argv[indexOf]) == TYUNUM)
        {
        theBits |= argv[indexOf++].u.UInt;
        }
    else
    if (asTag(&argv[indexOf]) == TYNUM)
        {
        theBits |= asInt(&argv[indexOf++]);
        }
    else
    if (asTag(&argv[indexOf]) == TYCHAR)
        {
        theBits |= asChar(&argv[indexOf++]);
        }
    else
    if (asTag(&argv[indexOf]) == TYBOLE)
        {
        if (asBool(&argv[indexOf++]) == TRUE)
            theBits |= 1;
        else
			{
			++indexOf;
            theBits |= FALSE;
			}
        }
    else
        {
		++indexOf;
        theBits |= FALSE;
        }
    }
    
asInt(ret) = theBits;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Xor

The bitwiseXor  Procedure numerically bitwise xors each expression from left to right, 
returning the value of the result as an Integer. 

For example
    (bitwiseXor  (=  12  12)  (>  2  1))   =>  0
    (bitwiseXor  (=  24  23)  (<  1  2))   =>  1
    (bitwiseXor  1  2  4  8)               =>  15

#endif

TVAL FMath2_Xor(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
NUM                     theBits;
NUM                     tmpBits;
NUM                     indexOf;
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYNUM;
asInt(ret) = 0;
if (argc == 0) 
	FrameExit(*ret);
theBits = 0;

/*  Return the result of bitwise xoring each argument. */

indexOf = 0;
while (indexOf < argc)
    {
    if isNumIndex(&argv[indexOf])
        {
        tmpBits = asNumIndex(&argv[indexOf]);
		indexOf++;
        theBits ^= tmpBits;
        }
    else
    if (asTag(&argv[indexOf]) == TYNUM)
        {
        theBits ^= asInt(&argv[indexOf++]);
        }
    else
    if (asTag(&argv[indexOf]) == TYCHAR)
        {
        theBits ^= asChar(&argv[indexOf++]);
        }
    else
    if (asTag(&argv[indexOf]) == TYBOLE)
        {
        if (asBool(&argv[indexOf++]) == TRUE)
            theBits ^= 1;
        else
            theBits ^= FALSE;
        }
    else
        {
        theBits ^= FALSE;
        }
    }
    
asInt(ret) = theBits;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_RAnd

The reduceAnd  Procedure numerically bitwise ands (for the specified bit width) each 
expression from left to right, returning the value of the result reduced to a binary Integer.
The last argument is always the bit width. If only one argument is specified, the width is one. 

#endif

TVAL FMath2_RAnd(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
NUM                     theBits;
NUM                     tmpBits;
NUM                     indexOf;
NUM         width = 1;
NUM         imask;
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYNUM;
asInt(ret) = 1;

if (argc == 0) 
	FrameExit(*ret);

theBits = -1;

/*  Determine the width argument. */

if (argc > 1)
   {
   --argc;
   width = asNumIndex(&argv[argc]);
   if ((width < 0) || (width > 31))
      width = 1;
   imask = gCP->FMath2_mask[width];
   }
else
   {
   imask = gCP->FMath2_mask[1];
   }
   

indexOf = 0;
/*  Return the result of bitwise anding each argument. */

indexOf = 0;
while (indexOf < argc)
    {
    if  ((asTag(&argv[indexOf]) == TYDATE)||(asTag(&argv[indexOf]) == TYMONEY)||(asTag(&argv[indexOf]) == TYREAL))
        {
        tmpBits = asNumIndex(&argv[indexOf++]);
        theBits &= tmpBits;
        }
    else
    if (asTag(&argv[indexOf]) == TYNUM)
        {
        theBits &= asInt(&argv[indexOf++]);
        }
    else
    if (asTag(&argv[indexOf]) == TYCHAR)
        {
        theBits &= asChar(&argv[indexOf++]);
        }
    else
    if (asTag(&argv[indexOf]) == TYBOLE)
        {
        if (asBool(&argv[indexOf++]) == TRUE)
            theBits &= 1;
        else
            theBits &= FALSE;
        }
    else
        {
        ++indexOf;
        theBits &= FALSE;
        }
    }
    
asInt(ret) = ((theBits & imask) == imask) ? 1 : 0;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_RNand

The reduceNand  Procedure numerically bitwise not ands each expression from left to right, 
returning the value of the result as a binary Integer. 

#endif

TVAL FMath2_RNand(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret = FMath2_RAnd(gCP,gTP,argc,argv);
ret->u.Int = !ret->u.Int;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_RNor

The reduceNor  Procedure numerically bitwise not ors each expression from left to right, 
returning the value of the result as an Integer. 

#endif

TVAL FMath2_RNor(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret = FMath2_ROr(gCP,gTP,argc,argv);
ret->u.Int = !ret->u.Int;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_ROr

The reduceOr  Procedure numerically bitwise ors (for the specified bit width) each 
expression from left to right, returning the value of the result reduced to a binary Integer.
The last argument is always the bit width. If only one argument is specified, the width is one. 

#endif

TVAL FMath2_ROr(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
NUM                     theBits;
NUM                     tmpBits;
NUM                     indexOf;
NUM						width = 1;
NUM						imask;
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYNUM;
asInt(ret) = 1;
if (argc == 0) 
	FrameExit(*ret);
theBits = 0;

/*  Determine the width argument. */

if (argc > 1)
   {
   --argc;
   width = asNumIndex(&argv[argc]);
   if ((width < 0) || (width > 31))
      width = 1;
   imask =gCP->FMath2_mask[width];
   }
else
   {
   imask =gCP->FMath2_mask[1];
   }
   

indexOf = 0;
/*  Return the result of bitwise oring each argument. */

indexOf = 0;
while (indexOf < argc)
    {
    if  ((asTag(&argv[indexOf]) == TYDATE)||(asTag(&argv[indexOf]) == TYMONEY)||(asTag(&argv[indexOf]) == TYREAL))
        {
        tmpBits = asNumIndex(&argv[indexOf++]);
        theBits |= tmpBits;
        }
    else
    if (asTag(&argv[indexOf]) == TYNUM)
        {
        theBits |= asInt(&argv[indexOf++]);
        }
    else
    if (asTag(&argv[indexOf]) == TYCHAR)
        {
        theBits |= asChar(&argv[indexOf++]);
        }
    else
    if (asTag(&argv[indexOf]) == TYBOLE)
        {
        if (asBool(&argv[indexOf++]) == TRUE)
            theBits |= 1;
        else
            theBits |= FALSE;
        }
    else
        {
        ++indexOf;
        theBits |= FALSE;
        }
    }
    
asInt(ret) = ((theBits & imask) == 0) ? 0 : 1;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_RXor

The reduceXor  Procedure numerically bitwise xors (for the specified bit width) each 
expression from left to right, returning the value of the result reduced to a binary Integer.
The last argument is always the bit width. If only one argument is specified, the width is one. 

#endif

TVAL FMath2_RXor(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
NUM                     theBits;
NUM                     tmpBits;
NUM                     indexOf;
NUM						width = 1;
NUM						imask;
NUM						count = 0;
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYNUM;
asInt(ret) = 1;
if (argc == 0) FrameExit(*ret);
theBits = 0;

/*  Determine the width argument. */

if (argc > 1)
   {
   --argc;
   width = asNumIndex(&argv[argc]);
   if ((width < 0) || (width > 31))
      width = 1;
   imask =gCP->FMath2_mask[width];
   }
else
   {
   imask =gCP->FMath2_mask[1];
   }
   

indexOf = 0;
/*  Return the result of bitwise xoring each argument. */

indexOf = 0;
while (indexOf < argc)
    {
    if  ((asTag(&argv[indexOf]) == TYDATE)||(asTag(&argv[indexOf]) == TYMONEY)||(asTag(&argv[indexOf]) == TYREAL))
        {
        tmpBits = asNumIndex(&argv[indexOf++]);
        theBits ^= tmpBits;
        }
    else
    if (asTag(&argv[indexOf]) == TYNUM)
        {
        theBits ^= asInt(&argv[indexOf++]);
        }
    else
    if (asTag(&argv[indexOf]) == TYCHAR)
        {
        theBits ^= asChar(&argv[indexOf++]);
        }
    else
    if (asTag(&argv[indexOf]) == TYBOLE)
        {
        if (asBool(&argv[indexOf++]) == TRUE)
            theBits ^= 1;
        else
            theBits ^= FALSE;
        }
    else
        {
        ++indexOf;
        theBits ^= FALSE;
        }
    }
    
/* Count the number of bits in the answer. */

theBits &= imask;
imask = 1;
while (width-- > 0)
   {
   if (theBits & imask) count = !count;
   imask <<= 1;
   }
asInt(ret) = count;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_RNxor

The reduceNxor  Procedure numerically bitwise not xors each expression from left to right, 
returning the value of the result as a binary Integer. 

#endif

TVAL FMath2_RNxor(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
*ret = FMath2_RXor(gCP,gTP,argc,argv);
ret->u.Int = !ret->u.Int;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Addc

The addc Procedure returns the sum of its argument as a Character quantity.

WARNING: This strongly typed function performs no error checking.  

#endif

TVAL FMath2_Addc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL		result;

gTP = gTP; // NOOP to hide unused parameter warning message
if (argc != 2) return(gCP->TObject_ERROR_INVALID_ARGLIST);

result.u.Char = argv[0].u.Char + argv[1].u.Char;
result.Tag = TYCHAR;

return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Subc

The subc Procedure returns the difference of its argument as a Character quantity.

WARNING: This strongly typed function performs no error checking.  

#endif

TVAL FMath2_Subc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL		result;

gTP = gTP; // NOOP to hide unused parameter warning message
if (argc != 2) return(gCP->TObject_ERROR_INVALID_ARGLIST);

result.u.Char = argv[0].u.Char - argv[1].u.Char;
result.Tag = TYCHAR;
JumpOnOverflow(OverFlowError);

return(result);

#ifdef JumpOverflowSet
OverFlowError:
return(gCP->FVmScript_ERROR_OVERFLOW);
#endif
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Divc

The divc Procedure returns the quotient of its argument as a Character quantity.

WARNING: This strongly typed function performs no error checking.  

#endif

TVAL FMath2_Divc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL		result;

gTP = gTP; // NOOP to hide unused parameter warning message
if (argc != 2) return(gCP->TObject_ERROR_INVALID_ARGLIST);

if (argv[1].u.Char == 0) goto IllegalDivide;
result.u.Char = argv[0].u.Char / argv[1].u.Char;
result.Tag = TYCHAR;

return(result);

IllegalDivide:
return(gCP->FVmScript_ERROR_DIVIDE_BY_ZERO);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Modc

The modc Procedure returns the remainder of its argument as a Boolean quantity.

WARNING: This strongly typed function performs no error checking.  

#endif

TVAL FMath2_Modc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL		result;

gTP = gTP; // NOOP to hide unused parameter warning message
if (argc != 2) return(gCP->TObject_ERROR_INVALID_ARGLIST);

if (argv[1].u.Char == 0) goto IllegalDivide;
result.u.Char = argv[0].u.Char % argv[1].u.Char;
result.Tag = TYCHAR;

return(result);

IllegalDivide:
return(gCP->FVmScript_ERROR_DIVIDE_BY_ZERO);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMath2_Mulc

The mulc Procedure returns the product of its argument as a Character quantity.

WARNING: This strongly typed function performs no error checking.  

#endif

TVAL FMath2_Mulc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
TVAL		result;

gTP = gTP; // NOOP to hide unused parameter warning message
if (argc != 2) return(gCP->TObject_ERROR_INVALID_ARGLIST);

result.u.Char = argv[0].u.Char * argv[1].u.Char;
result.Tag = TYCHAR;

return(result);
}

// RCA

/*--------------------------------------------------------------------------------------- */
#if 0

FMath2_Sort

The sort cProcedure returns a sorted copy of the specified List, Vector, or
Structure argument {key} in the order specified by the 
two argument predicate {pred}. 

Several examples follow.

    (define  Y  ''(3  2  1  4))             =>  (3  2  1  4)
    (sort  Y  <)                            =>  (1  2  3  4)
    (sort  Y  >)                            =>  (4  3  2  1)
    (sort  #(1 2 3 4)  >)                   =>  #(4  3  2  1)
    (sort  #{A: 1  B: 2  C: 3}  >)          =>  #{C: 3  B: 2  A: 1}

Note:	An optional third argument (if it is set to true) leaves the key
		unsorted, but returns an integer vector containing the sorted order
		of the key. For example:

		(sort #("B" "A" "C") < true)  ==>  #(1 0 2)

#endif

TVAL FMath2_Sort(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
BIND                envHold;
PBIND               envPHold;

StartFrame
DeclareTVAL(ret);
EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;

/*  Initialization. */

*ret = gCP->Tval_VOID;

switch( asTag(&argv[0]) )
{
	case TYVECTOR:
		FrameExit(FMath2_Sort_TYVECTOR(gCP,gTP,argc,argv));
    break;
    case TYOBJVECTOR:
		FrameExit(FMath2_Sort_TYOBJVECTOR(gCP,gTP,argc,argv));
	break;
	case TYMATRIX:
		FrameExit(FMath2_Sort_TYMATRIX(gCP,gTP,argc,argv));
	break;
	case TYNUMMATRIX:
		FrameExit(FMath2_Sort_TYNUMMATRIX(gCP,gTP,argc,argv));
	break;
	case TYBITVECTOR:
		FrameExit(FMath2_Sort_TYBITVECTOR(gCP,gTP,argc,argv));
	break;
	case TYBYTEVECTOR:
		FrameExit(FMath2_Sort_TYBYTEVECTOR(gCP,gTP,argc,argv));
	break;
	case TYINTVECTOR:
		FrameExit(FMath2_Sort_TYINTVECTOR(gCP,gTP,argc,argv));
	break;
	case TYLONGVECTOR:
		FrameExit(FMath2_Sort_TYLONGVECTOR(gCP,gTP,argc,argv));
	break;
	case TYSHORTVECTOR:
		FrameExit(FMath2_Sort_TYSHORTVECTOR(gCP,gTP,argc,argv));
	break;
	case TYNUMVECTOR:
		FrameExit(FMath2_Sort_TYNUMVECTOR(gCP,gTP,argc,argv));
	break;
	case TYFLTVECTOR:
		FrameExit(FMath2_Sort_TYFLTVECTOR(gCP,gTP,argc,argv));
	break;
	case TYPAIR:
		FrameExit(FMath2_Sort_TYPAIR(gCP,gTP,argc,argv));
	break;
	case TYSTRUCTURE:
		FrameExit(FMath2_Sort_TYSTRUCTURE(gCP,gTP,argc,argv));
	break;
	case TYDICTIONARY:
		FrameExit(FMath2_Sort_TYDICTIONARY(gCP,gTP,argc,argv));
	break;
	case TYDIRECTORY:
		FrameExit(FMath2_Sort_TYDIRECTORY(gCP,gTP,argc,argv));
	break;
	case TYCPXVECTOR:
		FrameExit(FMath2_Sort_TYCPXVECTOR(gCP,gTP,argc,argv));
	break;
}

	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST) 
}


		

TVAL FMath2_Sort_TYVECTOR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
BIND                envHold;
PBIND               envPHold;
NUM                 size;
NUM                 indexOf;
NUM                 numInserted;
NUM                 binaryHigh;
NUM                 binaryMiddle = 0;
NUM                 binaryLow;
BOLE                binarySearch;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TVector,vp1);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;

/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

/*  Sort a Vector argument. */
/*  Note:   We copy the vector, and perform a binary insertion sort, */
/*          inserting into the new object to reduce array move time. */
/*          (see Knuth Vol 3, pp 83) */

/*  Make a copy of the vector, and insert the first item gratis. */
/*  Note: If we have an empty vector, return the nil object. */

vp1 = (TVector*)asObject(&argv[0]);
size = vp1->itsMaxItemIndex;
if (size == 0) FrameExit(*ret);
numInserted = 1;

/*  If we are returning a sorted integer vector, instead of the */
/*  key, then we must create the initial unsorted integer vector here. */

if (integerVectorOption)
{
intVecObj = TIntVector_New(gCP,gTP);
integerVector->Tag = TYINTVECTOR;
integerVector->u.Object = (TObject*)intVecObj;
*ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
ExitOnError(*ec);
/* Fill the new integer vector with ascending indices. */
for (integerIndex = 0; integerIndex < size; integerIndex++)
	{
	atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
	}
}

/*  For each source vector element, perform a binary search on the */
/*  already sorted and inserted portion of the result vector. Grow */
/*  the inserted section of the result vector by one, and insert  */
/*  the source vector element in the proper sort order. */

while (numInserted < size)
{

/*  Perform a binary search. */
binarySearch = TRUE;
binaryLow = 0;
binaryHigh = numInserted - 1;
if (integerVectorOption)
	{
	integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
	prmv[0] = atHMTval(vp1->itsTvalArray,integerIndex);
	}
else
	{
	prmv[0] = atHMTval(vp1->itsTvalArray,numInserted);
	}
while (binarySearch)
	{
	binaryMiddle = (binaryLow + binaryHigh) / 2;

	/*  Apply the predicate->  */
    
	if (integerVectorOption)
		{
		tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
		prmv[1] = atHMTval(vp1->itsTvalArray,tempIndex);
		*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
		ExitOnError(*answer);
		}
	else
		{
		prmv[1] = atHMTval(vp1->itsTvalArray,binaryMiddle);
		*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
		ExitOnError(*answer);
		}
    
	/*  Adjust the binary search indices based on the *predicates answer-> */
    
	if ((asTag(answer) == TYBOLE) && (answer->u.Bool == TRUE))
		{
		binaryHigh = binaryMiddle - 1;
		}
	else
		{
		binaryLow = binaryMiddle + 1;
		}
        
	if (binaryHigh < binaryLow)
		{
		binaryMiddle = binaryLow;
		binarySearch = FALSE;
		}
	}

indexOf = numInserted;

/*  Grow the sorted portion of the result vector, and insert */
/*  the source element in its proper sort order. */

if (integerVectorOption)
	{
	if (binaryMiddle < numInserted)
		{
		memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
				(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
				sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
		}
	asInt(ndx2) = binaryMiddle;
	atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
	}
else
	{
	if (binaryMiddle < numInserted)
		{
		memmove((char*)&atHMTval(vp1->itsTvalArray,binaryMiddle+1),						/*target*/
				(char*)&atHMTval(vp1->itsTvalArray,binaryMiddle),						/*source*/
				sizeof(atHMTval(vp1->itsTvalArray,0)) * (numInserted - binaryMiddle));	/*amount*/
		}
	atHMTval(vp1->itsTvalArray,binaryMiddle) = prmv[0];
	}

++numInserted;
}

/*  At this point, the result is sorted so we return it. */

if (integerVectorOption)
	*ret = *integerVector;
else
	*ret = argv[0];
FrameExit(*ret);
}


TVAL FMath2_Sort_TYMATRIX(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
BIND                envHold;
PBIND               envPHold;
NUM                 size;
NUM                 indexOf;
NUM                 numInserted;
NUM                 binaryHigh;
NUM                 binaryMiddle = 0;
NUM                 binaryLow;
BOLE                binarySearch;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex;

StartFrame
DeclareOBJ(TMatrix,mp1);
DeclareOBJ(TIntVector,intVecObj);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;

/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

/*  Sort a Matrix argument. */
/*  Note:   We copy the matrix, and perform a binary insertion sort, */
/*          inserting into the new object to reduce array move time. */
/*          (see Knuth Vol 3, pp 83) */

/*  Make a copy of the matrix, and insert the first item gratis. */
/*  Note: If we have an empty vector, return the nil object. */
    
mp1 = (TMatrix*)asObject(&argv[0]);
size = mp1->itsMaxItemIndex;
if (size == 0) FrameExit(*ret);
numInserted = 1;

/*  If we are returning a sorted integer vector, instead of the */
/*  key, then we must create the initial unsorted integer vector here. */

if (integerVectorOption)
	{
	intVecObj = TIntVector_New(gCP,gTP);
	integerVector->Tag = TYINTVECTOR;
	integerVector->u.Object = (TObject*)intVecObj;
	*ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
	ExitOnError(*ec);
	/* Fill the new integer vector with ascending indices. */
	for (integerIndex = 0; integerIndex < size; integerIndex++)
		{
		atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
		}
	}
    
/*  For each source vector element, perform a binary search on the */
/*  already sorted and inserted portion of the result vector. Grow */
/*  the inserted section of the result vector by one, and insert  */
/*  the source vector element in the proper sort order. */

while (numInserted < size)
    {
    
    /*  Perform a binary search. */
    binarySearch = TRUE;
    binaryLow = 0;
    binaryHigh = numInserted - 1;
	if (integerVectorOption)
		{
		integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
		prmv[0] = atHMTval(mp1->itsTvalMatrix,integerIndex);
		}
	else
		{
		prmv[0] = atHMTval(mp1->itsTvalMatrix,numInserted);
		}
    while (binarySearch)
        {
        binaryMiddle = (binaryLow + binaryHigh) / 2;

        /*  Apply the predicate->  */
        
		if (integerVectorOption)
			{
			tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
			prmv[1] = atHMTval(mp1->itsTvalMatrix,tempIndex);
			*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
			ExitOnError(*answer);
			}
		else
			{
			prmv[1] = atHMTval(mp1->itsTvalMatrix,binaryMiddle);
			*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
			ExitOnError(*answer);
			}
        
        /*  Adjust the binary search indices based on the *predicates answer-> */
        
        if ((asTag(answer) == TYBOLE) && (answer->u.Bool == TRUE))
            {
            binaryHigh = binaryMiddle - 1;
            }
        else
            {
            binaryLow = binaryMiddle + 1;
            }
            
        if (binaryHigh < binaryLow)
            {
            binaryMiddle = binaryLow;
            binarySearch = FALSE;
            }
        }
    
    indexOf = numInserted;

	/*  Grow the sorted portion of the result vector, and insert */
    /*  the source element in its proper sort order. */
    
	if (integerVectorOption)
		{
		if (binaryMiddle < numInserted)
			{
			memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
					(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
					sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
			}
		asInt(ndx2) = binaryMiddle;
		atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
		}
	else
		{
		if (binaryMiddle < numInserted)
			{
			memmove((char*)&atHMTval(mp1->itsTvalMatrix,binaryMiddle+1),					/*target*/
					(char*)&atHMTval(mp1->itsTvalMatrix,binaryMiddle),						/*source*/
					sizeof(atHMTval(mp1->itsTvalMatrix,0)) * (numInserted - binaryMiddle));	/*amount*/
			}
		atHMTval(mp1->itsTvalMatrix,binaryMiddle) = prmv[0];
		}

    ++numInserted;
    }

    /*  At this point, the result is sorted so we return it. */
    
	if (integerVectorOption)
		*ret = *integerVector;
	else
		*ret = argv[0];
    FrameExit(*ret);
}


TVAL FMath2_Sort_TYNUMMATRIX(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 indexOf = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex = 0;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TNumMatrix,np1);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

        /*  Sort a NumMatrix argument. */
        /*  Note:   We copy the matrix, and perform a binary insertion sort, */
        /*          inserting into the new object to reduce array move time. */
        /*          (see Knuth Vol 3, pp 83) */

        /*  Make a copy of the matrix, and insert the first item gratis. */
        /*  Note: If we have an empty vector, return the nil object. */
            
        np1 = (TNumMatrix*)asObject(&argv[0]);
        size = np1->itsMaxItemIndex;
        if (size == 0) FrameExit(*ret);
        numInserted = 1;
        
        /*  If we are returning a sorted integer vector, instead of the */
        /*  key, then we must create the initial unsorted integer vector here. */
		
		if (integerVectorOption)
			{
			intVecObj = TIntVector_New(gCP,gTP);
			integerVector->Tag = TYINTVECTOR;
			integerVector->u.Object = (TObject*)intVecObj;
			*ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
			ExitOnError(*ec);
			/* Fill the new integer vector with ascending indices. */
			for (integerIndex = 0; integerIndex < size; integerIndex++)
				{
				atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
				}
			}
            
        /*  For each source vector element, perform a binary search on the */
        /*  already sorted and inserted portion of the result vector. Grow */
        /*  the inserted section of the result vector by one, and insert  */
        /*  the source vector element in the proper sort order. */
        
        while (numInserted < size)
            {
            
            /*  Perform a binary search. */
            binarySearch = TRUE;
            binaryLow = 0;
            binaryHigh = numInserted - 1;
			if (integerVectorOption)
				{
				integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
				prmv[0] = TREAL(atHMReal(np1->itsRealMatrix,integerIndex));
				}
			else
				{
				prmv[0] = TREAL(atHMReal(np1->itsRealMatrix,numInserted));
				}
            while (binarySearch)
                {
                binaryMiddle = (binaryLow + binaryHigh) / 2;
    
                /*  Apply the predicate->  */
                
				if (integerVectorOption)
					{
					tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
					prmv[1] = TREAL(atHMReal(np1->itsRealMatrix,tempIndex));
					*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					ExitOnError(*answer);
					}
				else
					{
					prmv[1] = TREAL(atHMReal(np1->itsRealMatrix,binaryMiddle));
					*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					ExitOnError(*answer);
					}
                
                /*  Adjust the binary search indices based on the *predicates answer-> */
                
                if ((asTag(answer) == TYBOLE) && (answer->u.Bool == TRUE))
                    {
                    binaryHigh = binaryMiddle - 1;
                    }
                else
                    {
                    binaryLow = binaryMiddle + 1;
                    }
                    
                if (binaryHigh < binaryLow)
                    {
                    binaryMiddle = binaryLow;
                    binarySearch = FALSE;
                    }
                }
            
            indexOf = numInserted;

			/*  Grow the sorted portion of the result vector, and insert */
            /*  the source element in its proper sort order. */
            
			if (integerVectorOption)
				{
				if (binaryMiddle < numInserted)
					{
					memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
							(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
							sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
					}
				asInt(ndx2) = binaryMiddle;
				atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
				}
			else
				{
				if (binaryMiddle < numInserted)
					{
					memmove((char*)&atHMReal(np1->itsRealMatrix,binaryMiddle+1),					/*target*/
							(char*)&atHMReal(np1->itsRealMatrix,binaryMiddle),						/*source*/
							sizeof(atHMReal(np1->itsRealMatrix,0)) * (numInserted - binaryMiddle));	/*amount*/
					}
				atHMReal(np1->itsRealMatrix,binaryMiddle) = asNumIndex(&prmv[0]);
				}

            ++numInserted;
            }
        
            /*  At this point, the result is sorted so we return it. */
            
			if (integerVectorOption)
				*ret = *integerVector;
			else
				*ret = argv[0];
            FrameExit(*ret);
}



TVAL FMath2_Sort_TYBITVECTOR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 indexOf = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex = 0;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TBitVector,vBitp);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

        vBitp = (TBitVector*)asObject(&argv[0]);
        size = vBitp->itsMaxItemIndex;
        if (size == 0) FrameExit(*ret);
        numInserted = 1;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
		
        /*  If we are returning a sorted integer vector, instead of the */
        /*  key, then we must create the initial unsorted integer vector here. */
        
        if (integerVectorOption)
			{
			intVecObj = TIntVector_New(gCP,gTP);
			integerVector->Tag = TYINTVECTOR;
			integerVector->u.Object = (TObject*)intVecObj;
			*ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
			ExitOnError(*ec);
			/* Fill the new integer vector with ascending indices. */
			for (integerIndex = 0; integerIndex < size; integerIndex++)
				{
				atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
				}
			}

        while (numInserted < size)
            {
            
            /*  Perform a binary search. */
            binarySearch = TRUE;
            binaryLow = 0;
            binaryHigh = numInserted - 1;
            asInt(ndx1) = numInserted;

 			if (integerVectorOption)
				{
				integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
				prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], TINT(integerIndex));
				}
            else
                {
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
                }

            while (binarySearch)
                {
                binaryMiddle = (binaryLow + binaryHigh) / 2;
    
                /*  Apply the predicate->  */
                
				if (integerVectorOption)
					{
					tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
					prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], TINT(tempIndex));
                    *answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					ExitOnError(*answer);
					}
                else
                    {
                    asInt(ndx2) = binaryMiddle;
                    prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2);
                    *answer = FSmartbase_Evalv(gCP,gTP,*predicate, (NUM)2,&prmv[0]);
                    ExitOnError(*answer);
                    }
                
                /*  Adjust the binary search indices based on the *predicates answer-> */
                
                if ((asTag(answer) == TYBOLE) && (asBool(answer) == TRUE))
                    {
                    binaryHigh = binaryMiddle - 1;
                    }
                else
                    {
                    binaryLow = binaryMiddle + 1;
                    }
                    
                if (binaryHigh < binaryLow)
                    {
                    binaryMiddle = binaryLow;
                    binarySearch = FALSE;
                    }
                }
            
            /*  Grow the sorted portion of the result vector, and insert */
            /*  the source element in its proper sort order. */
            indexOf = numInserted;
   			if (integerVectorOption)
				{
				if (binaryMiddle < numInserted)
					{
					memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
							(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
							sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
					}
				asInt(ndx2) = binaryMiddle;
				atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
				}
             else
                {
                if (binaryMiddle < numInserted)
                    {
                    asInt(ndx2) = binaryMiddle;
                    TBitVector_Insert(gCP,gTP,argv[0],*ndx2,prmv[0]);
                    }
                }
            ++numInserted;
            }
        
            /*  At this point, the result is sorted so we return it. */
			if (integerVectorOption)
				*ret = *integerVector;
            else
                *ret = argv[0];
            FrameExit(*ret);
}


TVAL FMath2_Sort_TYBYTEVECTOR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 indexOf = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex = 0;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TByteVector,vBytep);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];
      
        vBytep = (TByteVector*)asObject(&argv[0]);
        size = vBytep->itsMaxItemIndex;
        if (size == 0) FrameExit(*ret);
        numInserted = 1;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        
        /*  If we are returning a sorted integer vector, instead of the */
        /*  key, then we must create the initial unsorted integer vector here. */
        
        if (integerVectorOption)
			{
			intVecObj = TIntVector_New(gCP,gTP);
			integerVector->Tag = TYINTVECTOR;
			integerVector->u.Object = (TObject*)intVecObj;
			*ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
			ExitOnError(*ec);
			/* Fill the new integer vector with ascending indices. */
			for (integerIndex = 0; integerIndex < size; integerIndex++)
				{
				atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
				}
			}

        while (numInserted < size)
            {
            
            /*  Perform a binary search. */
            binarySearch = TRUE;
            binaryLow = 0;
            binaryHigh = numInserted - 1;
            asInt(ndx1) = numInserted;
            if (integerVectorOption)
				{
				integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], TINT(integerIndex));
				}
            else
                {
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
                }

            while (binarySearch)
                {
                binaryMiddle = (binaryLow + binaryHigh) / 2;
    
                /*  Apply the predicate->  */
                
				if (integerVectorOption)
					{
					tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
					prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], TINT(tempIndex));
					*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					ExitOnError(*answer);
					}
                else
                    {
                    asInt(ndx2) = binaryMiddle;
                    prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2);
                    *answer = FSmartbase_Evalv(gCP,gTP,*predicate, (NUM)2,&prmv[0]);
                    ExitOnError(*answer);
                    }
                
                /*  Adjust the binary search indices based on the *predicates answer-> */
                
                if ((asTag(answer) == TYBOLE) && (asBool(answer) == TRUE))
                    {
                    binaryHigh = binaryMiddle - 1;
                    }
                else
                    {
                    binaryLow = binaryMiddle + 1;
                    }
                    
                if (binaryHigh < binaryLow)
                    {
                    binaryMiddle = binaryLow;
                    binarySearch = FALSE;
                    }
                }
            
            /*  Grow the sorted portion of the result vector, and insert */
            /*  the source element in its proper sort order. */
            
            indexOf = numInserted;
   			if (integerVectorOption)
				{
				if (binaryMiddle < numInserted)
					{
					memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
							(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
							sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
					}
				asInt(ndx2) = binaryMiddle;
				atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
				}
            else
                {
                if (binaryMiddle < numInserted)
                    {
                    memmove((char*)&atHMChar(vBytep->itsByteArray,binaryMiddle+1),                      /*target*/
                            (char*)&atHMChar(vBytep->itsByteArray,binaryMiddle),                        /*source*/
                            sizeof(atHMChar(vBytep->itsByteArray,0)) * (numInserted - binaryMiddle));   /*amount*/
                    }
            
                asInt(ndx2) = binaryMiddle;
                (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2, prmv[0]);
                }
            ++numInserted;
            }
        
            /*  At this point, the result is sorted so we return it. */
            
			if (integerVectorOption)
				*ret = *integerVector;
            else
                *ret = argv[0];
            
            FrameExit(*ret);
}


TVAL FMath2_Sort_TYINTVECTOR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 indexOf = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex = 0;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TIntVector,vIntp);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];
           
        vIntp = (TIntVector*)asObject(&argv[0]);
        size = vIntp->itsMaxItemIndex;
        if (size == 0) FrameExit(*ret);
        numInserted = 1;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;

        /*  If we are returning a sorted integer vector, instead of the */
        /*  key, then we must create the initial unsorted integer vector here. */
		
		if (integerVectorOption)
			{
			intVecObj = TIntVector_New(gCP,gTP);
			integerVector->Tag = TYINTVECTOR;
			integerVector->u.Object = (TObject*)intVecObj;
			*ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
			ExitOnError(*ec);
			/* Fill the new integer vector with ascending indices. */
			for (integerIndex = 0; integerIndex < size; integerIndex++)
				{
				atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
				}
			}

     while (numInserted < size)
            {
            
            /*  Perform a binary search. */
            binarySearch = TRUE;
            binaryLow = 0;
            binaryHigh = numInserted - 1;
            asInt(ndx1) = numInserted;
            if (integerVectorOption)
				{
				integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], TINT(integerIndex));
				}
            else
                {
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], *ndx1);
                }
            while (binarySearch)
                {
                binaryMiddle = (binaryLow + binaryHigh) / 2;
    
                /*  Apply the predicate->  */
                
				if (integerVectorOption)
					{
					tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
					prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], TINT(tempIndex));
					*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					ExitOnError(*answer);
					}
                else
                    {
                    asInt(ndx2) = binaryMiddle;
                    prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], *ndx2);
                    *answer = FSmartbase_Evalv(gCP,gTP,*predicate, (NUM)2,&prmv[0]);
                    ExitOnError(*answer);
                    }

                /*  Adjust the binary search indices based on the *predicates answer-> */
                
                if ((asTag(answer) == TYBOLE) && (asBool(answer) == TRUE))
                    {
                    binaryHigh = binaryMiddle - 1;
                    }
                else
                    {
                    binaryLow = binaryMiddle + 1;
                    }
                    
                if (binaryHigh < binaryLow)
                    {
                    binaryMiddle = binaryLow;
                    binarySearch = FALSE;
                    }
                }
            
            /*  Grow the sorted portion of the result vector, and insert */
            /*  the source element in its proper sort order. */
            indexOf = numInserted;
    		if (integerVectorOption)
				{
				if (binaryMiddle < numInserted)
					{
					memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
							(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
							sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
					}
				asInt(ndx2) = binaryMiddle;
				atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
				}
            else
                {
                if (binaryMiddle < numInserted)
                    {
                    memmove((char*)&atHMInt(vIntp->itsIntArray,binaryMiddle+1),						/*target*/
                            (char*)&atHMInt(vIntp->itsIntArray,binaryMiddle),                       /*source*/
                            sizeof(atHMInt(vIntp->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
                    }
            
                asInt(ndx2) = binaryMiddle;
                (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2, prmv[0]);
                }

            ++numInserted;
            }
        
            /*  At this point, the result is sorted so we return it. */
			if (integerVectorOption)
				*ret = *integerVector;
            else
                *ret = argv[0];
            FrameExit(*ret);
}


TVAL FMath2_Sort_TYLONGVECTOR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 indexOf = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex = 0;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TLongVector,vLngp);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];
        
        vLngp = (TLongVector*)asObject(&argv[0]);
        size = vLngp->itsMaxItemIndex;
        if (size == 0) FrameExit(*ret);
        numInserted = 1;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;

        /*  If we are returning a sorted integer vector, instead of the */
        /*  key, then we must create the initial unsorted integer vector here. */
		
		if (integerVectorOption)
			{
			intVecObj = TIntVector_New(gCP,gTP);
			integerVector->Tag = TYINTVECTOR;
			integerVector->u.Object = (TObject*)intVecObj;
			*ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
			ExitOnError(*ec);
			/* Fill the new integer vector with ascending indices. */
			for (integerIndex = 0; integerIndex < size; integerIndex++)
				{
				atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
				}
			}

     while (numInserted < size)
            {
            
            /*  Perform a binary search. */
            binarySearch = TRUE;
            binaryLow = 0;
            binaryHigh = numInserted - 1;
            asInt(ndx1) = numInserted;
            if (integerVectorOption)
				{
				integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], TINT(integerIndex));
				}
            else
                {
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], *ndx1);
                }
            while (binarySearch)
                {
                binaryMiddle = (binaryLow + binaryHigh) / 2;
    
                /*  Apply the predicate->  */
                
				if (integerVectorOption)
					{
					tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
					prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], TINT(tempIndex));
					*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					ExitOnError(*answer);
					}
                else
                    {
                    asInt(ndx2) = binaryMiddle;
                    prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], *ndx2);
                    *answer = FSmartbase_Evalv(gCP,gTP,*predicate, (NUM)2,&prmv[0]);
                    ExitOnError(*answer);
                    }

                /*  Adjust the binary search indices based on the *predicates answer-> */
                
                if ((asTag(answer) == TYBOLE) && (asBool(answer) == TRUE))
                    {
                    binaryHigh = binaryMiddle - 1;
                    }
                else
                    {
                    binaryLow = binaryMiddle + 1;
                    }
                    
                if (binaryHigh < binaryLow)
                    {
                    binaryMiddle = binaryLow;
                    binarySearch = FALSE;
                    }
                }
            
            /*  Grow the sorted portion of the result vector, and insert */
            /*  the source element in its proper sort order. */
            indexOf = numInserted;
    		if (integerVectorOption)
				{
				if (binaryMiddle < numInserted)
					{
					memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
							(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
							sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
					}
				asInt(ndx2) = binaryMiddle;
				atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
				}
            else
                {
                if (binaryMiddle < numInserted)
                    {
                    memmove((char*)&atHMLong(vLngp->itsLongArray,binaryMiddle+1),						/*target*/
                            (char*)&atHMLong(vLngp->itsLongArray,binaryMiddle),							/*source*/
                            sizeof(atHMShort(vLngp->itsLongArray,0)) * (numInserted - binaryMiddle));	/*amount*/
                    }
            
                asInt(ndx2) = binaryMiddle;
                (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2, prmv[0]);
                }

            ++numInserted;
            }
        
            /*  At this point, the result is sorted so we return it. */
			if (integerVectorOption)
				*ret = *integerVector;
            else
                *ret = argv[0];
            FrameExit(*ret);
}


TVAL FMath2_Sort_TYSHORTVECTOR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 indexOf = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex = 0;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TShtVector,vShtp);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

		vShtp = (TShtVector*)asObject(&argv[0]);
        size = vShtp->itsMaxItemIndex;
        if (size == 0) FrameExit(*ret);
        numInserted = 1;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;

        /*  If we are returning a sorted integer vector, instead of the */
        /*  key, then we must create the initial unsorted integer vector here. */
		
		if (integerVectorOption)
			{
			intVecObj = TIntVector_New(gCP,gTP);
			integerVector->Tag = TYINTVECTOR;
			integerVector->u.Object = (TObject*)intVecObj;
			*ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
			ExitOnError(*ec);
			/* Fill the new integer vector with ascending indices. */
			for (integerIndex = 0; integerIndex < size; integerIndex++)
				{
				atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
				}
			}

     while (numInserted < size)
            {
            
            /*  Perform a binary search. */
            binarySearch = TRUE;
            binaryLow = 0;
            binaryHigh = numInserted - 1;
            asInt(ndx1) = numInserted;
            if (integerVectorOption)
				{
				integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], TINT(integerIndex));
				}
            else
                {
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], *ndx1);
                }
            while (binarySearch)
                {
                binaryMiddle = (binaryLow + binaryHigh) / 2;
    
                /*  Apply the predicate->  */
                
				if (integerVectorOption)
					{
					tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
					prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], TINT(tempIndex));
					*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					ExitOnError(*answer);
					}
                else
                    {
                    asInt(ndx2) = binaryMiddle;
                    prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], *ndx2);
                    *answer = FSmartbase_Evalv(gCP,gTP,*predicate, (NUM)2,&prmv[0]);
                    ExitOnError(*answer);
                    }

                /*  Adjust the binary search indices based on the *predicates answer-> */
                
                if ((asTag(answer) == TYBOLE) && (asBool(answer) == TRUE))
                    {
                    binaryHigh = binaryMiddle - 1;
                    }
                else
                    {
                    binaryLow = binaryMiddle + 1;
                    }
                    
                if (binaryHigh < binaryLow)
                    {
                    binaryMiddle = binaryLow;
                    binarySearch = FALSE;
                    }
                }
            
            /*  Grow the sorted portion of the result vector, and insert */
            /*  the source element in its proper sort order. */
            indexOf = numInserted;
    		if (integerVectorOption)
				{
				if (binaryMiddle < numInserted)
					{
					memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
							(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
							sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
					}
				asInt(ndx2) = binaryMiddle;
				atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
				}
            else
                {
                if (binaryMiddle < numInserted)
                    {
                    memmove((char*)&atHMShort(vShtp->itsShortArray,binaryMiddle+1),						/*target*/
                            (char*)&atHMShort(vShtp->itsShortArray,binaryMiddle),						/*source*/
                            sizeof(atHMShort(vShtp->itsShortArray,0)) * (numInserted - binaryMiddle));	/*amount*/
                    }
            
                asInt(ndx2) = binaryMiddle;
                (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2, prmv[0]);
                }

            ++numInserted;
            }
        
            /*  At this point, the result is sorted so we return it. */
			if (integerVectorOption)
				*ret = *integerVector;
            else
                *ret = argv[0];
            FrameExit(*ret);
}

TVAL FMath2_Sort_TYNUMVECTOR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 indexOf = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex = 0;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TNumVector,vNump);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

		vNump = (TNumVector*)asObject(&argv[0]);
        size = vNump->itsMaxItemIndex;
        if (size == 0) FrameExit(*ret);
        numInserted = 1;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
        /*  If we are returning a sorted integer vector, instead of the */
        /*  key, then we must create the initial unsorted integer vector here. */
		
		if (integerVectorOption)
			{
			intVecObj = TIntVector_New(gCP,gTP);
			integerVector->Tag = TYINTVECTOR;
			integerVector->u.Object = (TObject*)intVecObj;
			*ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
			ExitOnError(*ec);
			/* Fill the new integer vector with ascending indices. */
			for (integerIndex = 0; integerIndex < size; integerIndex++)
				{
				atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
				}
			}
        while (numInserted < size)
            {
            
            /*  Perform a binary search. */
            binarySearch = TRUE;
            binaryLow = 0;
            binaryHigh = numInserted - 1;
            asInt(ndx1) = numInserted;
            if (integerVectorOption)
				{
				integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], TINT(integerIndex));
				}
            else
                {
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], *ndx1);
                }
            while (binarySearch)
                {
                binaryMiddle = (binaryLow + binaryHigh) / 2;
    
                /*  Apply the predicate->  */
                
				if (integerVectorOption)
					{
					tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
					prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP,argv[0], TINT(tempIndex));
					*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					ExitOnError(*answer);
					}
                else
                    {
                    asInt(ndx2) = binaryMiddle;
                    prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2);
                    *answer = FSmartbase_Evalv(gCP,gTP,*predicate, (NUM)2,&prmv[0]);
                    ExitOnError(*answer);
                    }
                
                /*  Adjust the binary search indices based on the *predicates answer-> */
                
                if ((asTag(answer) == TYBOLE) && (asBool(answer) == TRUE))
                    {
                    binaryHigh = binaryMiddle - 1;
                    }
                else
                    {
                    binaryLow = binaryMiddle + 1;
                    }
                    
                if (binaryHigh < binaryLow)
                    {
                    binaryMiddle = binaryLow;
                    binarySearch = FALSE;
                    }
                }
            
            /*  Grow the sorted portion of the result vector, and insert */
            /*  the source element in its proper sort order. */
            
            indexOf = numInserted;
     		if (integerVectorOption)
				{
				if (binaryMiddle < numInserted)
					{
					memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
							(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
							sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
					}
				asInt(ndx2) = binaryMiddle;
				atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
				}
            else
                {
               if (binaryMiddle < numInserted)
                    {
                    memmove((char*)&atHMReal(vNump->itsRealArray,binaryMiddle+1),                       /*target*/
                            (char*)&atHMReal(vNump->itsRealArray,binaryMiddle),                     /*source*/
                            sizeof(atHMReal(vNump->itsRealArray,0)) * (numInserted - binaryMiddle));    /*amount*/
                    }
            
                asInt(ndx2) = binaryMiddle;
                (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2, prmv[0]);
                }
            ++numInserted;
            }
        
            /*  At this point, the result is sorted so we return it. */
            
			if (integerVectorOption)
				*ret = *integerVector;
            else
                *ret = argv[0];

            FrameExit(*ret);
}


        
TVAL FMath2_Sort_TYFLTVECTOR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 indexOf = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex = 0;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TFltVector,vFltp);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];
        
        vFltp = (TFltVector*)asObject(&argv[0]);
        size = vFltp->itsMaxItemIndex;
        if (size == 0) FrameExit(*ret);
        numInserted = 1;
        asTag(ndx1) = TYNUM;
        asTag(ndx2) = TYNUM;
 
        /*  If we are returning a sorted integer vector, instead of the */
        /*  key, then we must create the initial unsorted integer vector here. */
		
		if (integerVectorOption)
			{
			intVecObj = TIntVector_New(gCP,gTP);
			integerVector->Tag = TYINTVECTOR;
			integerVector->u.Object = (TObject*)intVecObj;
			*ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
			ExitOnError(*ec);
			/* Fill the new integer vector with ascending indices. */
			for (integerIndex = 0; integerIndex < size; integerIndex++)
				{
				atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
				}
			} 

       while (numInserted < size)
            {
            
            /*  Perform a binary search. */
            binarySearch = TRUE;
            binaryLow = 0;
            binaryHigh = numInserted - 1;
            asInt(ndx1) = numInserted;
            if (integerVectorOption)
				{
				integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], TINT(integerIndex));
				}
            else
                {
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
                }
            while (binarySearch)
                {
                binaryMiddle = (binaryLow + binaryHigh) / 2;
    
                /*  Apply the predicate->  */
                                
				if (integerVectorOption)
					{
					tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
					prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], TINT(tempIndex));
					*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					ExitOnError(*answer);
					}
                else
                    {
                    asInt(ndx2) = binaryMiddle;
                    prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2);
                    *answer = FSmartbase_Evalv(gCP,gTP,*predicate, (NUM)2,&prmv[0]);
                    ExitOnError(*answer);
                    }
                
                /*  Adjust the binary search indices based on the *predicates answer-> */
                
                if ((asTag(answer) == TYBOLE) && (asBool(answer) == TRUE))
                    {
                    binaryHigh = binaryMiddle - 1;
                    }
                else
                    {
                    binaryLow = binaryMiddle + 1;
                    }
                    
                if (binaryHigh < binaryLow)
                    {
                    binaryMiddle = binaryLow;
                    binarySearch = FALSE;
                    }
                }
            
            /*  Grow the sorted portion of the result vector, and insert */
            /*  the source element in its proper sort order. */
            
            indexOf = numInserted;
       		if (integerVectorOption)
				{
				if (binaryMiddle < numInserted)
					{
					memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
							(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
							sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
					}
				asInt(ndx2) = binaryMiddle;
				atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
				}
            else
                {
                if (binaryMiddle < numInserted)
                    {
                    memmove((char*)&atHMFloat(vFltp->itsFloatArray,binaryMiddle+1),                     /*target*/
                            (char*)&atHMFloat(vFltp->itsFloatArray,binaryMiddle),                       /*source*/
                            sizeof(atHMFloat(vFltp->itsFloatArray,0)) * (numInserted - binaryMiddle));  /*amount*/
                    }
            
                asInt(ndx2) = binaryMiddle;
                (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2, prmv[0]);
                }
            ++numInserted;
            }
        
            /*  At this point, the result is sorted so we return it. */
            
			if (integerVectorOption)
				*ret = *integerVector;
            else
                *ret = argv[0];

            FrameExit(*ret);
}

        
TVAL FMath2_Sort_TYPAIR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;

StartFrame
DeclareTVAL(ret);
DeclareTVAL(input);
DeclareTVAL(predicate);
DeclareTVAL(ec);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

			/*  Sort a List argument. */
            /*  Note:   We convert the List to a vector, and then, */
            /*          we copy the vector, and perform a binary insertion sort, */
            /*          inserting into the new object to reduce array move time. */
            /*          Finally, we convert the sorted Vector back into a List. */
            /*          (see Knuth Vol 3, pp 83) */
            
            prmv[0] = FConvert_ObjToVector(gCP,gTP,(NUM)1,&argv[0]);
            ExitOnError(*ec);
            prmv[1] = *predicate;
            if (integerVectorOption)
                {
                prmv[2].Tag = TYBOLE;
                prmv[2].u.Bool = TRUE;
                *input = FMath2_Sort(gCP,gTP,(NUM)3,&prmv[0]);
                FrameExit(*input);
                }
            else
                {
                *input = FMath2_Sort(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError(*input);
                prmv[0] = *input;
                *ret = FConvert_ObjToList(gCP,gTP,(NUM)1,&prmv[0]);
                FrameExit(*ret);
                }
}

TVAL FMath2_Sort_TYSTRUCTURE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 indexOf = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex = 0;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TStructure,ep1);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

            /*  Sort a Structure argument. */
            /*  Note:   We copy the Structure, and perform a binary insertion sort, */
            /*          inserting into the new object to reduce array move time. */
            /*          (see Knuth Vol 3, pp 83) */
            /*  Make a copy of the Structure, and insert the first item gratis. */
            /*  Note: If we have an empty Structure, return the nil object. */
            
            ep1 = (TStructure*)asObject(&argv[0]);
            size = ep1->itsMaxItemIndex;
            if (size == 0) FrameExit(*ret);
            numInserted = 1;
            /*  If we are returning a sorted integer vector, instead of the */
            /*  key, then we must create the initial unsorted integer vector here. */
		    
		    if (integerVectorOption)
			    {
			    intVecObj = TIntVector_New(gCP,gTP);
			    integerVector->Tag = TYINTVECTOR;
			    integerVector->u.Object = (TObject*)intVecObj;
			    *ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
			    ExitOnError(*ec);
			    /* Fill the new integer vector with ascending indices. */
			    for (integerIndex = 0; integerIndex < size; integerIndex++)
				    {
				    atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
				    }
			    }
            
            /*  For each source Structure element, perform a binary search on the */
            /*  already sorted and inserted portion of the result Structure. Grow */
            /*  the inserted section of the result Structure by one, and insert  */
            /*  the source Structure element in the proper sort order. */
            
            while (numInserted < size)
                {
                
                /*  Perform a binary search. */
                
                binarySearch = TRUE;
                binaryLow = 0;
                binaryHigh = numInserted - 1;
                if (integerVectorOption)
				    {
				    integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
					if (byValueOption)
						prmv[0] = atHMBind(ep1->itsDictionaryArray,integerIndex).Value;
					else
						{
						asObject(&prmv[0]) = atHMBind(ep1->itsDictionaryArray,integerIndex).Key;
						asTag(&prmv[0]) = (char)asObject(&prmv[0])->itsObjectType;
						}
				    }
                else
                    {
                    envHold = atHMBind(ep1->itsDictionaryArray,numInserted);
					if (byValueOption)
						prmv[0] = atHMBind(ep1->itsDictionaryArray,numInserted).Value;
					else
						{
						asObject(&prmv[0]) = atHMBind(ep1->itsDictionaryArray,numInserted).Key;
						asTag(&prmv[0]) = (char)asObject(&prmv[0])->itsObjectType;
						}
                    }
                while (binarySearch)
                    {
                    binaryMiddle = (binaryLow + binaryHigh) / 2;
                    
                    /*  Apply the predicate->  */
                    
 				    if (integerVectorOption)
					    {
					    tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
						if (byValueOption)
							prmv[1] = atHMBind(ep1->itsDictionaryArray,tempIndex).Value;
						else
							{
							asObject(&prmv[1]) = atHMBind(ep1->itsDictionaryArray,tempIndex).Key;
							asTag(&prmv[1]) = (char)asObject(&prmv[1])->itsObjectType;
							}
					    *answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					    ExitOnError(*answer);
					    }
                    else
                        {
  						if (byValueOption)
							prmv[1] = atHMBind(ep1->itsDictionaryArray,binaryMiddle).Value;
						else
							{
							asObject(&prmv[1]) = atHMBind(ep1->itsDictionaryArray,binaryMiddle).Key;
							asTag(&prmv[1]) = (char)asObject(&prmv[1])->itsObjectType;
							}
                        *answer = FSmartbase_Evalv(gCP,gTP,*predicate, (NUM)2,&prmv[0]);
                        ExitOnError(*answer);
                        }
                    
                    /*  Adjust the binary search indices based on the *predicates answer-> */
                    
                    if ((asTag(answer) == TYBOLE) && (asBool(answer) == TRUE))
                        {
                        binaryHigh = binaryMiddle - 1;
                        }
                    else
                        {
                        binaryLow = binaryMiddle + 1;
                        }
                        
                    if (binaryHigh < binaryLow)
                        {
                        binaryMiddle = binaryLow;
                        binarySearch = FALSE;
                        }
                    }
                
                
                /*  Grow the sorted portion of the result vector, and insert */
                /*  the source element in its proper sort order. */
            
                indexOf = numInserted;
      		    if (integerVectorOption)
				    {
				    if (binaryMiddle < numInserted)
					    {
					    memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
							    (char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
							    sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
					    }
				    asInt(ndx2) = binaryMiddle;
				    atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
				    }
                else
                    {
                    if (binaryMiddle < numInserted)
                        {
                        memmove((char*)&atHMBind(ep1->itsDictionaryArray,binaryMiddle+1),						/*target*/
                                (char*)&atHMBind(ep1->itsDictionaryArray,binaryMiddle),							/*source*/
                                sizeof(atHMBind(ep1->itsDictionaryArray,0)) * (numInserted - binaryMiddle));    /*amount*/
                        }
                    atHMBind(ep1->itsDictionaryArray,binaryMiddle) = envHold;
                    }
                ++numInserted;
                }
            
            /*  At this point, the result is sorted so we return it. */
            
			if (integerVectorOption)
				*ret = *integerVector;
            else
                *ret = argv[0];
            FrameExit(*ret);
}


TVAL FMath2_Sort_TYDICTIONARY(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 indexOf = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex = 0;
NUM					n = 0;
TVAL				left;
TVAL				right;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TDictionary,dc1);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

			/*  Sort a Dictionary argument. */
            /*  Note:   We copy the Dictionary, and perform a binary insertion sort, */
            /*          inserting into the new object to reduce array move time. */
            /*          (see Knuth Vol 3, pp 83) */
            /*  Make a copy of the Dictionary, and insert the first item gratis. */
            /*  Note: If we have an empty Dictionary, return the nil object. */
            
            dc1 = (TDictionary*)asObject(&argv[0]);
            size = dc1->itsMaxItemIndex;
            if (size == 0) FrameExit(*ret);
            numInserted = 1;
            /*  If we are returning a sorted integer vector, instead of the */
            /*  key, then we must create the initial unsorted integer vector here. */
		    
		    if (integerVectorOption)
			    {
			    intVecObj = TIntVector_New(gCP,gTP);
			    integerVector->Tag = TYINTVECTOR;
			    integerVector->u.Object = (TObject*)intVecObj;
			    *ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
			    ExitOnError(*ec);
			    /* Fill the new integer vector with ascending indices. */
			    for (integerIndex = 0; integerIndex < size; integerIndex++)
				    {
				    atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
				    }
			    }
            
            /*  For each source Dictionary element, perform a binary search on the */
            /*  already sorted and inserted portion of the result Dictionary. Grow */
            /*  the inserted section of the result Dictionary by one, and insert  */
            /*  the source Dictionary element in the proper sort order. */
            
            while (numInserted < size)
                {
                
                /*  Perform a binary search. */
                
                binarySearch = TRUE;
                binaryLow = 0;
                binaryHigh = numInserted - 1;
                if (integerVectorOption)
				    {
				    integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
					if (byValueOption)
						prmv[0] = atHMBind(dc1->itsDictionaryArray,integerIndex).Value;
					else
						{
						asObject(&prmv[0]) = atHMBind(dc1->itsDictionaryArray,integerIndex).Key;
						asTag(&prmv[0]) = (char)asObject(&prmv[0])->itsObjectType;
						}
				    }
                else
                    {
                    envHold = atHMBind(dc1->itsDictionaryArray,numInserted);
					if (byValueOption)
						prmv[0] = atHMBind(dc1->itsDictionaryArray,numInserted).Value;
					else
						{
						asObject(&prmv[0]) = atHMBind(dc1->itsDictionaryArray,numInserted).Key;
						asTag(&prmv[0]) = (char)asObject(&prmv[0])->itsObjectType;
						}
                    }
                while (binarySearch)
                    {
                    binaryMiddle = (binaryLow + binaryHigh) / 2;
                    
                    /*  Apply the predicate->  */
                    
 				    if (integerVectorOption)
					    {
					    tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
						if (byValueOption)
							prmv[1] = atHMBind(dc1->itsDictionaryArray,tempIndex).Value;
						else
							{
							asObject(&prmv[1]) = atHMBind(dc1->itsDictionaryArray,tempIndex).Key;
							asTag(&prmv[1]) = (char)asObject(&prmv[1])->itsObjectType;
							}
					    *answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					    ExitOnError(*answer);
					    }
                    else
                        {
  						if (byValueOption)
							prmv[1] = atHMBind(dc1->itsDictionaryArray,binaryMiddle).Value;
						else
							{
							asObject(&prmv[1]) = atHMBind(dc1->itsDictionaryArray,binaryMiddle).Key;
							asTag(&prmv[1]) = (char)asObject(&prmv[1])->itsObjectType;
							}
                        *answer = FSmartbase_Evalv(gCP,gTP,*predicate, (NUM)2,&prmv[0]);
                        ExitOnError(*answer);
                        }
                    
                    /*  Adjust the binary search indices based on the *predicates answer-> */
                    
                    if ((asTag(answer) == TYBOLE) && (asBool(answer) == TRUE))
                        {
                        binaryHigh = binaryMiddle - 1;
                        }
                    else
                        {
                        binaryLow = binaryMiddle + 1;
                        }
                        
                    if (binaryHigh < binaryLow)
                        {
                        binaryMiddle = binaryLow;
                        binarySearch = FALSE;
                        }
                    }
                
                
                /*  Grow the sorted portion of the result vector, and insert */
                /*  the source element in its proper sort order. */
            
                indexOf = numInserted;
      		    if (integerVectorOption)
				    {
				    if (binaryMiddle < numInserted)
					    {
					    memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
							    (char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
							    sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
					    }
				    asInt(ndx2) = binaryMiddle;
				    atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
				    }
                else
                    {
                    if (binaryMiddle < numInserted)
                        {
                        memmove((char*)&atHMBind(dc1->itsDictionaryArray,binaryMiddle+1),						/*target*/
                                (char*)&atHMBind(dc1->itsDictionaryArray,binaryMiddle),							/*source*/
                                sizeof(atHMBind(dc1->itsDictionaryArray,0)) * (numInserted - binaryMiddle));    /*amount*/
                        }
                    atHMBind(dc1->itsDictionaryArray,binaryMiddle) = envHold;
                    }
                ++numInserted;
                }
            
            /*  At this point, the result is sorted so we return it. */
            
			if (integerVectorOption)
				*ret = *integerVector;
            else
				{
                *ret = argv[0];
				/* Perform self check to make sure Dictionary is in ascending sort order. */
				if ((ret->u.Dictionary->itsMaxItemIndex > 0) &&
					(ret->u.Dictionary->itsImmediatePtr == NULL) &&
					((ret->u.Dictionary->itsDictionaryArray == NULL) || 
					 (((LpMHANDLEINFO)ret->u.Dictionary->itsDictionaryArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				/* Make sure the keys are in ascending sorted order. */
				for (n = 0; n < ret->u.Dictionary->itsMaxItemIndex - 1; ++n)
					{
					left = TOBJ(((LpBIND)*ret->u.Dictionary->itsDictionaryArray)[n].Key);
					right = TOBJ(((LpBIND)*ret->u.Dictionary->itsDictionaryArray)[n+1].Key);
					if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) >= 0)
						{
						prmv[0] = *ret;
						prmv[1].u.Pointer = (POINTER)&FPredicate_LT;
						prmv[1].Tag = TYCFUNCTION;
						FMath2_Sort(gCP,gTP,2,&prmv[0]);
						*ec = TERROR("!sort: attempt to sort Dictionary out of ascending key order!");
						FrameExit(*ec);
						}
					}
				}
            FrameExit(*ret);
}

 
TVAL FMath2_Sort_TYDIRECTORY(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 indexOf = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex = 0;
NUM					n = 0;
TVAL				left;
TVAL				right;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TDirectory,dr1);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

			/*  Sort a Directory argument. */
            /*  Note:   We copy the Directory, and perform a binary insertion sort, */
            /*          inserting into the new object to reduce array move time. */
            /*          (see Knuth Vol 3, pp 83) */
            /*  Make a copy of the Directory, and insert the first item gratis. */
            /*  Note: If we have an empty Directory, return the nil object. */
            
            dr1 = (TDirectory*)asObject(&argv[0]);
            size = dr1->itsMaxItemIndex;
            if (size == 0) FrameExit(*ret);
            numInserted = 1;
            /*  If we are returning a sorted integer vector, instead of the */
            /*  key, then we must create the initial unsorted integer vector here. */
		    
		    if (integerVectorOption)
			    {
			    intVecObj = TIntVector_New(gCP,gTP);
			    integerVector->Tag = TYINTVECTOR;
			    integerVector->u.Object = (TObject*)intVecObj;
			    *ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
			    ExitOnError(*ec);
			    /* Fill the new integer vector with ascending indices. */
			    for (integerIndex = 0; integerIndex < size; integerIndex++)
				    {
				    atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
				    }
			    }
            
            /*  For each source Directory element, perform a binary search on the */
            /*  already sorted and inserted portion of the result Directory. Grow */
            /*  the inserted section of the result Directory by one, and insert  */
            /*  the source Directory element in the proper sort order. */
            
            while (numInserted < size)
                {
                
                /*  Perform a binary search. */
                
                binarySearch = TRUE;
                binaryLow = 0;
                binaryHigh = numInserted - 1;
                if (integerVectorOption)
				    {
				    integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
					if (byValueOption)
						prmv[0] = atHMPBind(dr1->itsDirectoryArray,integerIndex).Value;
					else
						prmv[0] = atHMPBind(dr1->itsDirectoryArray,integerIndex).Key;
				    }
                else
                    {
                    envPHold = atHMPBind(dr1->itsDirectoryArray,numInserted);
					if (byValueOption)
						prmv[0] = atHMPBind(dr1->itsDirectoryArray,numInserted).Value;
					else
						prmv[0] = atHMPBind(dr1->itsDirectoryArray,numInserted).Key;
                    }
                while (binarySearch)
                    {
                    binaryMiddle = (binaryLow + binaryHigh) / 2;
                    
                    /*  Apply the predicate->  */
                    
 				    if (integerVectorOption)
					    {
					    tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
						if (byValueOption)
							prmv[1] = atHMPBind(dr1->itsDirectoryArray,tempIndex).Value;
						else
							prmv[1] = atHMPBind(dr1->itsDirectoryArray,tempIndex).Key;
					    *answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
					    ExitOnError(*answer);
					    }
                    else
                        {
  						if (byValueOption)
							prmv[1] = atHMPBind(dr1->itsDirectoryArray,binaryMiddle).Value;
						else
							prmv[1] = atHMPBind(dr1->itsDirectoryArray,binaryMiddle).Key;
                        *answer = FSmartbase_Evalv(gCP,gTP,*predicate, (NUM)2,&prmv[0]);
                        ExitOnError(*answer);
                        }
                    
                    /*  Adjust the binary search indices based on the *predicates answer-> */
                    
                    if ((asTag(answer) == TYBOLE) && (asBool(answer) == TRUE))
                        {
                        binaryHigh = binaryMiddle - 1;
                        }
                    else
                        {
                        binaryLow = binaryMiddle + 1;
                        }
                        
                    if (binaryHigh < binaryLow)
                        {
                        binaryMiddle = binaryLow;
                        binarySearch = FALSE;
                        }
                    }
                
                
                /*  Grow the sorted portion of the result vector, and insert */
                /*  the source element in its proper sort order. */
            
                indexOf = numInserted;
      		    if (integerVectorOption)
				    {
				    if (binaryMiddle < numInserted)
					    {
					    memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
							    (char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
							    sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
					    }
				    asInt(ndx2) = binaryMiddle;
				    atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
				    }
                else
                    {
                    if (binaryMiddle < numInserted)
                        {
                        memmove((char*)&atHMPBind(dr1->itsDirectoryArray,binaryMiddle+1),						/*target*/
                                (char*)&atHMPBind(dr1->itsDirectoryArray,binaryMiddle),							/*source*/
                                sizeof(atHMPBind(dr1->itsDirectoryArray,0)) * (numInserted - binaryMiddle));    /*amount*/
                        }
                    atHMPBind(dr1->itsDirectoryArray,binaryMiddle) = envPHold;
                    }
                ++numInserted;
                }
            
            /*  At this point, the result is sorted so we return it. */
            
			if (integerVectorOption)
				*ret = *integerVector;
            else
				{
                *ret = argv[0];

				if ((ret->u.Directory->itsMaxItemIndex > 0) &&
					(ret->u.Directory->itsImmediatePtr == NIL) &&
					((ret->u.Directory->itsDirectoryArray == NIL) || 
					 (((LpMHANDLEINFO)ret->u.Directory->itsDirectoryArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				/* Make sure the keys are in ascending sorted order. */
				for (n = 0; n < ret->u.Directory->itsMaxItemIndex - 1; ++n)
					{
					left = ((LpPBIND)*ret->u.Directory->itsDirectoryArray)[n].Key;
					right = ((LpPBIND)*ret->u.Directory->itsDirectoryArray)[n+1].Key;
					if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) >= 0)
						{
						prmv[0] = *ret;
						prmv[1].u.Pointer = (POINTER)&FPredicate_LT;
						prmv[1].Tag = TYCFUNCTION;
						FMath2_Sort(gCP,gTP,2,&prmv[0]);
						*ec = TERROR("!sort: attempt to sort Directory out of ascending key order!");
						FrameExit(*ec);
						}
					}
				}
			FrameExit(*ret);
}


TVAL FMath2_Sort_TYCPXVECTOR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BIND                envHold;
PBIND               envPHold;
NUM                 size = 0;
NUM                 numInserted = 0;
NUM                 binaryHigh = 0;
NUM                 binaryMiddle = 0;
NUM                 binaryLow = 0;
BOLE                binarySearch = 0;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
REAL				*rp = NULL;				// -> pair of reals in complex array

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TCpxVector,vCpxp);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;


/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

	// Setup
	vCpxp = argv[0].u.CpxVector;
	size = vCpxp->itsMaxItemIndex;
	ndx1->Tag = ndx2->Tag = TYNUM;
	if (size == 0) FrameExit(*ret);
	numInserted = 1;

	//  Create an unsorted integer vector if returning an integer vector	
	if (integerVectorOption)
	{	integerVector->u.IntVector = intVecObj = TIntVector_New(gCP,gTP);
		integerVector->Tag = TYINTVECTOR;
		*ec = TIntVector_SetMaxIndex(gCP, gTP, *integerVector, size);
		ExitOnError(*ec);
		// Fill the new integer vector with ascending indices
		for (integerIndex = 0; integerIndex < size; ++integerIndex)
			atHMInt(intVecObj->itsIntArray, integerIndex) = integerIndex;
	}
	while (numInserted < size)
	{	//  Perform a binary search
 		binarySearch = TRUE;
		binaryLow = 0;
		binaryHigh = numInserted - 1;
		if (integerVectorOption)
			ndx1->u.Int = integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
		else
			ndx1->u.Int = numInserted;

		prmv[0] = TCpxVector_GetIV1(gCP, gTP, argv[0], *ndx1);

		while (binarySearch)
		{	binaryMiddle = (binaryLow + binaryHigh) / 2;
			ndx2->u.Int = (integerVectorOption) ?
				atHMInt(intVecObj->itsIntArray, binaryMiddle) : binaryMiddle;
			prmv[1] = TCpxVector_GetIV1(gCP, gTP, argv[0], *ndx2);
			*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
			ExitOnError(*answer);

			//  Adjust the binary search indices based on the *predicates *answer
			if (answer->Tag == TYBOLE && answer->u.Bool == TRUE)
				binaryHigh = binaryMiddle - 1;
			else
				binaryLow = binaryMiddle + 1;
            
			if (binaryHigh < binaryLow)
			{	binaryMiddle = binaryLow;
				binarySearch = FALSE;
			}
		}
		//  Grow the sorted portion of the result vector, and insert next element in order    
		if (integerVectorOption)
		{	if (binaryMiddle < numInserted)
			{	memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),
						(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),
						sizeof(NUM) * (numInserted - binaryMiddle));
			}
			atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
		}
		else
		{	if (binaryMiddle < numInserted)
			{	rp = (LpREAL)*vCpxp->itsCpxArray + 2 * binaryMiddle;
				memmove((char*)(rp + 2), (char*)rp, 2*sizeof(REAL)*(numInserted - binaryMiddle));
			}
			ndx2->u.Int = binaryMiddle;
			TCpxVector_SetIV1(gCP, gTP, argv[0], *ndx2, prmv[0]);
		}
		++numInserted;
	}
	//  At this point, the result is sorted
	if (integerVectorOption)
		*ret = *integerVector;
	else
		*ret = argv[0];
	FrameExit(*ret);
}


TVAL FMath2_Sort_TYOBJVECTOR(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])     
{
BIND                envHold;
PBIND               envPHold;
NUM                 size;
NUM                 indexOf;
NUM                 numInserted;
NUM                 binaryHigh;
NUM                 binaryMiddle = 0;
NUM                 binaryLow;
BOLE                binarySearch;
BOLE				integerVectorOption = FALSE;
BOLE				byValueOption = FALSE;
NUM 				integerIndex = 0;
NUM 				tempIndex;

StartFrame
DeclareOBJ(TIntVector,intVecObj);
DeclareOBJ(TObjVector,vObjp);
DeclareTVAL(integerVector);
DeclareTVAL(ret);
DeclareTVAL(predicate);
DeclareTVAL(answer);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVAL(ndx2);
DeclareTVALArray(prmv,3);

EndFrame
envHold.Key = NULL;
envHold.Value = gCP->Tval_VOID;
envPHold.Key = gCP->Tval_VOID;
envPHold.Value = gCP->Tval_VOID;

/*  Initialization. */

*ret = gCP->Tval_VOID;

/* Are we returning a sorted integer vector ? */

if ((argc == 3) && 
	(argv[2].Tag == TYBOLE) &&
	(argv[2].u.Bool == TRUE))
	{
    argc = 2;
    integerVectorOption = TRUE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 3) && 
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = FALSE;
	}

/* Are we returning a sorted integer vector by value ? */

if ((argc == 4) && 
	(argv[3].Tag == TYBOLE)			&&
	(argv[3].u.Bool == TRUE)		&&
	(argv[2].Tag == TYSYMBOL)		&&
    (strcmp(SymbolArray(argv[2]),"byValue") == 0))
	{
    argc = 2;
    byValueOption = TRUE;
    integerVectorOption = TRUE;
	}

/*  Argument validity checking. */

if ((argc != 2) || 
    (isNullTval(&argv[0])) || 
    ((asTag(&argv[1]) != TYCPROCEDURE) && (argv[1].Tag != TYLAMBDA))) 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
*predicate = argv[1];

vObjp = (TObjVector*)asObject(&argv[0]);
size = vObjp->itsMaxItemIndex;
if (size == 0) FrameExit(*ret);
numInserted = 1;
asTag(ndx1) = TYNUM;
asTag(ndx2) = TYNUM;
/*  If we are returning a sorted integer vector, instead of the */
/*  key, then we must create the initial unsorted integer vector here. */

if (integerVectorOption)
	{
	intVecObj = TIntVector_New(gCP,gTP);
	integerVector->Tag = TYINTVECTOR;
	integerVector->u.Object = (TObject*)intVecObj;
	*ec = TIntVector_SetMaxIndex(gCP,gTP,*integerVector,size);
	ExitOnError(*ec);
	/* Fill the new integer vector with ascending indices. */
	for (integerIndex = 0; integerIndex < size; integerIndex++)
		{
		atHMInt(intVecObj->itsIntArray,integerIndex) = integerIndex;
		}
	}
while (numInserted < size)
    {
    
    /*  Perform a binary search. */
    binarySearch = TRUE;
    binaryLow = 0;
    binaryHigh = numInserted - 1;
    asInt(ndx1) = numInserted;
    if (integerVectorOption)
		{
		integerIndex = atHMInt(intVecObj->itsIntArray,numInserted);
        prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], TINT(integerIndex));
		}
    else
        {
        prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
        }
    while (binarySearch)
        {
        binaryMiddle = (binaryLow + binaryHigh) / 2;

        /*  Apply the predicate->  */
        
		if (integerVectorOption)
			{
			tempIndex = atHMInt(intVecObj->itsIntArray,binaryMiddle);
			prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], TINT(tempIndex));
			*answer = FSmartbase_Evalv(gCP,gTP,*predicate,(NUM)2,&prmv[0]);
			ExitOnError(*answer);
			}
        else
            {
            asInt(ndx2) = binaryMiddle;
            prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2);
            *answer = FSmartbase_Evalv(gCP,gTP,*predicate, (NUM)2,&prmv[0]);
            ExitOnError(*answer);
            }
        /*  Adjust the binary search indices based on the *predicates answer-> */
        
        if ((asTag(answer) == TYBOLE) && (asBool(answer) == TRUE))
            {
            binaryHigh = binaryMiddle - 1;
            }
        else
            {
            binaryLow = binaryMiddle + 1;
            }
            
        if (binaryHigh < binaryLow)
            {
            binaryMiddle = binaryLow;
            binarySearch = FALSE;
            }
        }
    
    /*  Grow the sorted portion of the result vector, and insert */
    /*  the source element in its proper sort order. */
    
    indexOf = numInserted;
	if (integerVectorOption)
		{
		if (binaryMiddle < numInserted)
			{
			memmove((char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle+1),						/*target*/
					(char*)&atHMInt(intVecObj->itsIntArray,binaryMiddle),                       /*source*/
					sizeof(atHMInt(intVecObj->itsIntArray,0)) * (numInserted - binaryMiddle));	/*amount*/
			}
		asInt(ndx2) = binaryMiddle;
		atHMInt(intVecObj->itsIntArray,binaryMiddle) = integerIndex;
		}
    else
        {
        if (binaryMiddle < numInserted)
            {
            memmove((char*)&atHMObject(vObjp->itsObjectArray,binaryMiddle+1),                       /*target*/
                    (char*)&atHMObject(vObjp->itsObjectArray,binaryMiddle),                     /*source*/
                    sizeof(atHMObject(vObjp->itsObjectArray,0)) * (numInserted - binaryMiddle));    /*amount*/
            }
    
        asInt(ndx2) = binaryMiddle;
        (*_TObject_TypeSetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx2, prmv[0]);
        }
    ++numInserted;
    }

    /*  At this point, the result is sorted so we return it. */
    
	if (integerVectorOption)
		*ret = *integerVector;
    else
        *ret = argv[0];

    FrameExit(*ret);
}
