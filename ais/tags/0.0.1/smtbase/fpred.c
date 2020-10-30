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

#define _C_FPREDICATE
#define _SMARTBASE

#if 0
FPredicate.c

Implementation of utility functions which are used to return the boolean
result of a particular test or expression.

PARENT:             None.

AUTHORS:            Michael F. Korns

MODIFICATIONS:

#endif

#include    "fpred.h"
#include    "fpred2.h"
#include    "futil3.h"
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tpair.h"
#include    "tstruct.h"
#include    "tdiction.h"
#include    "tdirect.h"
#include    "tvector.h"
#include    "tbitvec.h"
#include    "tbytevec.h"
#include    "tnumvec.h"
#include    "tfltvec.h"
#include    "tintvec.h"
#include    "tobjvec.h"
#include    "tpcodvec.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "futil1.h"
#include    "futil2.h"
#include    "fconvert.h"
#include    "fmacro.h"
#include    "tcontin.h"
#include    "ffloat.h"
#include    "tmatrix.h"
#include    "tnummat.h"
#include	"tcpxvec.h"
#include	"tshortvec.h"
#include	"tlongvec.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_Init

Initialize a portion of the the SmartLisp function library.

#endif

TVAL FPredicate_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,alsoSymbol);
DeclareTVAL(ec);
EndFrame

if(gCP->FPred_Initialized)
    FrameExit(gCP->TObject_OK);

gCP->FPred_Initialized = 1;

/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ccompareLT",(LpFUNC)&FPredicate_LT);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ccompareLE",(LpFUNC)&FPredicate_LE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ccompareEQ",(LpFUNC)&FPredicate_EQ);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ccompareNE",(LpFUNC)&FPredicate_NE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ccompareGE",(LpFUNC)&FPredicate_GE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ccompareGT",(LpFUNC)&FPredicate_GT);
ExitOnError( *ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bcompareLT",(LpFUNC)&FPredicate_LT);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bcompareLE",(LpFUNC)&FPredicate_LE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bcompareEQ",(LpFUNC)&FPredicate_EQ);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bcompareNE",(LpFUNC)&FPredicate_NE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bcompareGE",(LpFUNC)&FPredicate_GE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"bcompareGT",(LpFUNC)&FPredicate_GT);
ExitOnError( *ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"icompareLT",(LpFUNC)&FPredicate_LT);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"icompareLE",(LpFUNC)&FPredicate_LE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"icompareEQ",(LpFUNC)&FPredicate_EQ);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"icompareNE",(LpFUNC)&FPredicate_NE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"icompareGE",(LpFUNC)&FPredicate_GE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"icompareGT",(LpFUNC)&FPredicate_GT);
ExitOnError( *ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ncompareLT",(LpFUNC)&FPredicate_LT);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ncompareLE",(LpFUNC)&FPredicate_LE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ncompareEQ",(LpFUNC)&FPredicate_EQ);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ncompareNE",(LpFUNC)&FPredicate_NE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ncompareGE",(LpFUNC)&FPredicate_GE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"ncompareGT",(LpFUNC)&FPredicate_GT);
ExitOnError( *ec);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"<",(LpFUNC)&FPredicate_LT);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"charLT"),    aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringLT"),  aSymbol->itsGlobalValue);


*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"<=",(LpFUNC)&FPredicate_LE);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"charLE"),   aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringLE"), aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"=",(LpFUNC)&FPredicate_EQ);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"isEq"),       aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"isEqv"),      aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"charEQ"),    aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringEQ"),  aSymbol->itsGlobalValue);



*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"<>",(LpFUNC)&FPredicate_NE);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"charNE"),   aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringNE"), aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)">=",(LpFUNC)&FPredicate_GE);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"charGE"),   aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringGE"), aSymbol->itsGlobalValue);


*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)">",(LpFUNC)&FPredicate_GT);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"charGT"),    aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"stringGT"),  aSymbol->itsGlobalValue);


*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isSymbol",(LpFUNC)&FPredicate_Symbolp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isVector",(LpFUNC)&FPredicate_Vectorp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isMatrix",(LpFUNC)&FPredicate_Matrixp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isString",(LpFUNC)&FPredicate_Stringp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isText",(LpFUNC)&FPredicate_Textp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isObject",(LpFUNC)&FPredicate_Object);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"atom",(LpFUNC)&FPredicate_Atom);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"isAtom"), aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isBoolean",(LpFUNC)&FPredicate_Boolean);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isChar",(LpFUNC)&FPredicate_Charp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isCharAlphabetic",(LpFUNC)&FPredicate_CharAlphabeticp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isCharLowercase",(LpFUNC)&FPredicate_CharLowerCasep);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isCharNumeric",(LpFUNC)&FPredicate_CharNumericp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isCharWhitespace",(LpFUNC)&FPredicate_CharWhitespacep);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isCharUppercase",(LpFUNC)&FPredicate_CharUpperCasep);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isCharAlphanumeric",(LpFUNC)&FPredicate_CharAlphanumericp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isCharName",(LpFUNC)&FPredicate_CharNamep);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isBitVector",(LpFUNC)&FPredicate_BitVectorp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isByteVector",(LpFUNC)&FPredicate_ByteVectorp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isFloatVector",(LpFUNC)&FPredicate_FltVectorp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isIntegerVector",(LpFUNC)&FPredicate_IntVectorp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isNumberVector",(LpFUNC)&FPredicate_NumVectorp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isObjectVector",(LpFUNC)&FPredicate_ObjVectorp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isPcodeVector",(LpFUNC)&FPredicate_PcodeVectorp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isComplexVector",(LpFUNC)&FPredicate_CpxVectorp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isNaN",(LpFUNC)&FPredicate_NaN);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"isNotNaN",(LpFUNC)&FPredicate_NotNaN);
ExitOnError( *ec);


FrameExit( gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_LE

Return true if argv[0] <= argv[1], otherwise return false.

    (<= a b)

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate_LE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  Initialization and argument validation. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 2) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

*tmp = FPredicate2_FullCompare(gCP,gTP,argv[0], argv[1]);
if (!isERROR(tmp) && isCompareLE(tmp))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_EQ

Return true if argv[0] == argv[1], otherwise return false.

    (= a b)

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate_EQ(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  Initialization and argument validation. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 2) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

*tmp = FPredicate2_FullCompare(gCP,gTP,argv[0],argv[1]);
if (!isERROR(tmp) && isCompareEQ(tmp))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_NE

Return true if argv[0] != argv[1], otherwise return false.

    (<> a b)

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate_NE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  Initialization and argument validation. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 2) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

*tmp = FPredicate2_FullCompare(gCP,gTP,argv[0],argv[1]);
if (!isERROR(tmp) && isCompareNE(tmp))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_GE

Return true if argv[0] >= argv[1], otherwise return false.

    (>= a b)

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate_GE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  Initialization and argument validation. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 2) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

*tmp = FPredicate2_FullCompare(gCP,gTP,argv[0],argv[1]);
if (!isERROR(tmp) && isCompareGE(tmp))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_GT

Return true if argv[0] > argv[1], otherwise return false.

    (> a b)

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate_GT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  Initialization and argument validation. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 2) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

*tmp = FPredicate2_FullCompare(gCP,gTP,argv[0],argv[1]);
if (!isERROR(tmp) && isCompareGT(tmp))
    asBool(ret) = TRUE;

FrameExit( *ret);
}
/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_Symbolp

The FPredicate_Symbolp function returns true if the argument is a Symbol object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_Symbolp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

ret->Tag = TYBOLE;
ret->u.Bool = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if ((argv[0].Tag == TYSYMBOL) || (argv[0].Tag == TYQUOTEDSYMBOL))
    ret->u.Bool = TRUE;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_Vectorp

The FPredicate_Vectorp function returns true if the argument is a Vector object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_Vectorp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

switch(argv[0].Tag)
    {
    case TYVECTOR:
    case TYBITVECTOR:
    case TYBYTEVECTOR:
    case TYINTVECTOR:
    case TYSHORTVECTOR:
    case TYLONGVECTOR:
    case TYNUMVECTOR:
    case TYFLTVECTOR:
    case TYOBJVECTOR:
	case TYPCODEVECTOR:
	case TYCPXVECTOR:
        asBool(ret) = TRUE;
	}


FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_Matrixp

The FPredicate_Matrixp function returns true if the argument is a Matrix object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_Matrixp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

switch(argv[0].Tag)
    {
    case TYMATRIX:
    case TYNUMMATRIX:
        asBool(ret) = TRUE;
	break;
	}


FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_BitVectorp

The FPredicate_BitVectorp function returns true if the argument is a BitVector object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_BitVectorp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (!isNullTval(&argv[0]) && (asTag(&argv[0]) == TYBITVECTOR))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_ByteVectorp

The FPredicate_ByteVectorp function returns true if the argument is a ByteVector object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_ByteVectorp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (!isNullTval(&argv[0]) && (asTag(&argv[0]) == TYBYTEVECTOR))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_CpxVectorp

The FPredicate_CpxVectorp function returns true if the argument is a Cpx Vector.

Note:   Exactly one argument is expected, anything else is an error.
#endif
TVAL FPredicate_CpxVectorp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (!isNullTval(&argv[0]) && (asTag(&argv[0]) == TYCPXVECTOR))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_IntVectorp

The FPredicate_IntVectorp function returns true if the argument is a IntVector object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_IntVectorp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (!isNullTval(&argv[0]) && (asTag(&argv[0]) == TYINTVECTOR))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_NumVectorp

The FPredicate_NumVectorp function returns true if the argument is a NumVector object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_NumVectorp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (!isNullTval(&argv[0]) && (asTag(&argv[0]) == TYNUMVECTOR))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_FltVectorp

The FPredicate_FltVectorp function returns true if the argument is a FltVector object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_FltVectorp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (!isNullTval(&argv[0]) && (asTag(&argv[0]) == TYFLTVECTOR))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_ObjVectorp

The FPredicate_ObjVectorp function returns true if the argument is a ObjVector object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_ObjVectorp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (!isNullTval(&argv[0]) && (asTag(&argv[0]) == TYOBJVECTOR))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_PcodeVectorp

The FPredicate_PcodeVectorp function returns true if the argument is a PcodeVector object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_PcodeVectorp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (!isNullTval(&argv[0]) && (asTag(&argv[0]) == TYPCODEVECTOR))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_Stringp

The isString cProcedure returns true if the argument {obj} is a String;
otherwise, false is returned.

Several examples follow.

    (define  Y  "My love")          =>  "My love"
    (isString  Y)                    =>  true
    (isString  "")                   =>  true
    (isString  22.3)                 =>  false

#endif

TVAL FPredicate_Stringp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYSTRING || asTag(&argv[0]) == TYTEXT || asTag(&argv[0]) == TYSTRINGSUBSTR)
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_Textp

The isText cProcedure returns true if the argument {obj} is a Text;
otherwise, false is returned.

Several examples follow.

    (define  Y  "Hello")		   =>  "Hello"
    (isText  Y)                    =>  true
    (isText  "")                   =>  true
    (isText  22.3)                 =>  false

#endif

TVAL FPredicate_Textp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYTEXT || asTag(&argv[0]) == TYSTRINGSUBSTR)
    asBool(ret) = TRUE;

FrameExit( *ret);
}
/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_Object

The FPredicate_Object function returns true if the argument is an object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_Object(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (_TObject_TypeFlag(asTag(&argv[0])) == _TObject_TfTOBJECT)
    asBool(ret) = TRUE;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_Atom

The FPredicate_Atom function returns true if the argument is not a Pair object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_Atom(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (((asTag(&argv[0]) != TYPAIR) && (asTag(&argv[0]) != TYQUOTEDPAIR)) || (asObj(&argv[0]) == NIL))
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_Boolean

The FPredicate_Boolean function returns true if the argument is a Boolean object.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_Boolean(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
if (asTag(&argv[0]) == TYBOLE)
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_Charp

The FPredicate_Charp function returns true if the argument is a Character.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_Charp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret)  = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

if (asTag(&argv[0]) == TYCHAR)
    {
    asBool(ret) = TRUE;
    }

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_CharAlphabeticp

The FPredicate_CharAlphabeticp function returns true if the argument is an
alphabetic Character.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_CharAlphabeticp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM			tIndex;
NUM			tLength;
LpCHAR		tPtr;
StartFrame
DeclareTVAL(ret);
EndFrame

*ret = gCP->Tval_FALSE;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Mange each of the character or string types */

switch (argv[0].Tag)
	{
    case TYTEXT:
		tPtr = &argv[0].u.Text[0];
		tLength = strlen(tPtr);

	RepeatingTest:
		for (tIndex = 0; tIndex < tLength; ++tIndex)
			{
			if (!ISALPHA((NUM)tPtr[tIndex]))
				{
				goto WrongTypeOfChar;
				}
			}
			ret->u.Bool = TRUE;
		break;

    case TYSTRINGSUBSTR:
        tPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        if (tPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        tLength = SubLen(argv[0]);
        goto RepeatingTest;
        break;

	case TYSTRING:
	case TYSYMBOL:
	case TYBYTEVECTOR:
		tPtr = FSmartbase_VectorPtr(gCP,gTP,argv[0]);
		tLength = FSmartbase_StringLen(gCP,gTP,&argv[0]);
		goto RepeatingTest;
		break;

	case TYCHAR:
		if (ISALPHA(argv[0].u.Char))
			{
			ret->u.Bool = TRUE;
			}
		else
			{
			ret->u.Bool = FALSE;
			}
		break;

	default:
	WrongTypeOfChar:
		ret->u.Bool = FALSE;
		break;
	}

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_CharLowerCasep

The FPredicate_CharLowerCasep function returns true if the argument is an
alphabetic lower case Character.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_CharLowerCasep(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM			tIndex;
NUM			tLength;
LpCHAR		tPtr;
StartFrame
DeclareTVAL(ret);
EndFrame

ret->Tag = TYBOLE;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Mange each of the character or string types */

switch (argv[0].Tag)
	{
    case TYTEXT:
		tPtr = &argv[0].u.Text[0];
		tLength = strlen(tPtr);

	RepeatingTest:
		for (tIndex = 0; tIndex < tLength; ++tIndex)
			{
			if (!ISXLOWER((NUM)tPtr[tIndex]))
				{
				goto WrongTypeOfChar;
				}
			}
			ret->u.Bool = TRUE;
		break;

    case TYSTRINGSUBSTR:
        tPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        if (tPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        tLength = SubLen(argv[0]);
        goto RepeatingTest;
        break;

	case TYSTRING:
	case TYSYMBOL:
	case TYBYTEVECTOR:
		tPtr = FSmartbase_VectorPtr(gCP,gTP,argv[0]);
		tLength = FSmartbase_StringLen(gCP,gTP,&argv[0]);
		goto RepeatingTest;
		break;

	case TYCHAR:
		if (ISXLOWER(argv[0].u.Char))
			ret->u.Bool = TRUE;
		else
			ret->u.Bool = FALSE;
		break;

	default:
	WrongTypeOfChar:
		ret->u.Bool = FALSE;
		break;
	}

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_CharUpperCasep

The FPredicate_CharUpperCasep function returns true if the argument is an
alphabetic upper case Character.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_CharUpperCasep(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM			tIndex;
NUM			tLength;
LpCHAR		tPtr;
StartFrame
DeclareTVAL(ret);
EndFrame

ret->Tag = TYBOLE;
ret->u.Bool = FALSE;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Mange each of the character or string types */

switch (argv[0].Tag)
	{
	case TYTEXT:
		tPtr = &argv[0].u.Text[0];
		tLength = strlen(tPtr);

	RepeatingTest:
		for (tIndex = 0; tIndex < tLength; ++tIndex)
			{
			if (!ISXUPPER((NUM)tPtr[tIndex]))
				{
				goto WrongTypeOfChar;
				}
			}
			ret->u.Bool = TRUE;
		break;

    case TYSTRINGSUBSTR:
        tPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        if (tPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        tLength = SubLen(argv[0]);
        goto RepeatingTest;
        break;

	case TYSTRING:
	case TYSYMBOL:
	case TYBYTEVECTOR:
		tPtr = FSmartbase_VectorPtr(gCP,gTP,argv[0]);
		tLength = FSmartbase_StringLen(gCP,gTP,&argv[0]);
		goto RepeatingTest;
		break;

	case TYCHAR:
		if (ISXUPPER(argv[0].u.Char))
			ret->u.Bool = TRUE;
		break;

	default:
	WrongTypeOfChar:
		ret->u.Bool = FALSE;
		break;
	}

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_CharNumericp

The FPredicate_CharNumericp function returns true if the argument is a
digit Character.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_CharNumericp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM			tIndex;
NUM			tLength;
LpCHAR		tPtr;
StartFrame
DeclareTVAL(ret);
EndFrame

ret->Tag = TYBOLE;
ret->u.Bool = FALSE;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Mange each of the character or string types */

switch (argv[0].Tag)
	{
    case TYTEXT:
		tPtr = &argv[0].u.Text[0];
		tLength = strlen(tPtr);

	RepeatingTest:
		for (tIndex = 0; tIndex < tLength; ++tIndex)
			{
			if (!ISDIGIT((NUM)tPtr[tIndex]))
				{
				goto WrongTypeOfChar;
				}
			}
			ret->u.Bool = TRUE;
		break;

    case TYSTRINGSUBSTR:
        tPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        if (tPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        tLength = SubLen(argv[0]);
        goto RepeatingTest;
        break;

	case TYSTRING:
	case TYSYMBOL:
	case TYBYTEVECTOR:
		tPtr = FSmartbase_VectorPtr(gCP,gTP,argv[0]);
		tLength = FSmartbase_StringLen(gCP,gTP,&argv[0]);
		goto RepeatingTest;
		break;

	case TYCHAR:
		if (ISDIGIT(argv[0].u.Char))
			{
			ret->u.Bool = TRUE;
			}
		break;

	default:
	WrongTypeOfChar:
		ret->u.Bool = FALSE;
		break;
	}

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_CharWhitespacep

The FPredicate_CharWhitespacep function returns true if the argument is a
whitespace Character.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_CharWhitespacep(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM			tIndex;
NUM			tLength;
LpCHAR		tPtr;
StartFrame
DeclareTVAL(ret);
EndFrame

ret->Tag = TYBOLE;
ret->u.Bool = FALSE;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Mange each of the character or string types */

switch (argv[0].Tag)
	{
	case TYTEXT:
		tPtr = &argv[0].u.Text[0];
		tLength = strlen(tPtr);

	RepeatingTest:
		for (tIndex = 0; tIndex < tLength; ++tIndex)
			{
			if (!ISSPACE((NUM)tPtr[tIndex]))
				{
				goto WrongTypeOfChar;
				}
			}
		ret->u.Bool = TRUE;
		break;

    case TYSTRINGSUBSTR:
        tPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        if (tPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        tLength = SubLen(argv[0]);
        goto RepeatingTest;
        break;

	case TYSTRING:
	case TYSYMBOL:
	case TYBYTEVECTOR:
		tPtr = FSmartbase_VectorPtr(gCP,gTP,argv[0]);
		tLength = FSmartbase_StringLen(gCP,gTP,&argv[0]);
		goto RepeatingTest;
		break;

	case TYCHAR:
		if (ISSPACE(argv[0].u.Char))
			{
			ret->u.Bool = TRUE;
			}
		break;

	default:
	WrongTypeOfChar:
		ret->u.Bool = FALSE;
		break;
	}

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_CharAlphanumericp

The FPredicate_CharAlphanumericp function returns true if the argument is a
whitespace Character.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_CharAlphanumericp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM			tIndex;
NUM			tLength;
LpCHAR		tPtr;
StartFrame
DeclareTVAL(ret);
EndFrame

ret->Tag = TYBOLE;
ret->u.Bool = FALSE;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Mange each of the character or string types */

switch (argv[0].Tag)
	{
	case TYTEXT:
		tPtr = &argv[0].u.Text[0];
		tLength = strlen(tPtr);

	RepeatingTest:
		for (tIndex = 0; tIndex < tLength; ++tIndex)
			{
			if (!ISALNUM((NUM)tPtr[tIndex]))
				{
				goto WrongTypeOfChar;
				}
			}
		ret->u.Bool = TRUE;
		break;

    case TYSTRINGSUBSTR:
        tPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        if (tPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        tLength = SubLen(argv[0]);
        goto RepeatingTest;
        break;

	case TYSTRING:
	case TYSYMBOL:
	case TYBYTEVECTOR:
		tPtr = FSmartbase_VectorPtr(gCP,gTP,argv[0]);
		tLength = FSmartbase_StringLen(gCP,gTP,&argv[0]);
		goto RepeatingTest;
		break;

	case TYCHAR:
		if (ISALNUM(argv[0].u.Char))
			{
			ret->u.Bool = TRUE;
			}
		break;

	default:
	WrongTypeOfChar:
		ret->u.Bool = FALSE;
		break;
	}

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_CharNamep

The FPredicate_CharNamep function returns true if the argument is a
whitespace Character.

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_CharNamep(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM			tIndex;
NUM			tLength;
LpCHAR		tPtr;
StartFrame
DeclareTVAL(ret);
EndFrame

ret->Tag = TYBOLE;
ret->u.Bool = FALSE;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Mange each of the character or string types */

switch (argv[0].Tag)
	{
	case TYTEXT:
		tPtr = &argv[0].u.Text[0];
		tLength = strlen(tPtr);

	RepeatingTest:
		for (tIndex = 0; tIndex < tLength; ++tIndex)
			{
			if (!ISSYM((NUM)tPtr[tIndex]))
				{
				goto WrongTypeOfChar;
				}
			}
		ret->u.Bool = TRUE;
		break;

    case TYSTRINGSUBSTR:
        tPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        if (tPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        tLength = SubLen(argv[0]);
        goto RepeatingTest;
        break;

	case TYSTRING:
	case TYSYMBOL:
	case TYBYTEVECTOR:
		tPtr = FSmartbase_VectorPtr(gCP,gTP,argv[0]);
		tLength = FSmartbase_StringLen(gCP,gTP,&argv[0]);
		goto RepeatingTest;
		break;

	case TYCHAR:
		if (ISSYM(argv[0].u.Char))
			{
			ret->u.Bool = TRUE;
			}
		break;

	default:
	WrongTypeOfChar:
		ret->u.Bool = FALSE;
		break;
	}

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_LT

Return true if argv[0] < argv[1], otherwise return false.

    (< a b)

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate_LT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  Initialization and argument validation. */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 2) FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);

*tmp = FPredicate2_FullCompare(gCP,gTP,argv[0],argv[1]);
if (!isERROR(tmp) && isCompareLT(tmp))
    asBool(ret) = TRUE;

FrameExit( *ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_NaN

Return true if argv[0] is #void, nan, snan, or negative zero.

    (isNaN a)

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_NaN(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
INT32 tHiBits;
INT32 tLoBits;
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

switch (argv[0].Tag)
{
	case TYVOID:
		asBool(ret) = TRUE;
		break;

	case TYDATE:
	case TYREAL:
	case TYMONEY:
		/* the size of the double data type is 8 bytes.
		 * the 1st 4 bytes represent the low bits.
		 * the 2nd 4 bytes represent the high bits.
		 * [0] = low 4-byte
		 * [1] = high 4-byte
		 *
		 * we can use 64-bit integer here but we want
		 * to make sure this will also work in 32-bit
		 * systems
		 */

		tLoBits = ((LpINT32)&argv[0].u.Real)[0];
		tHiBits = ((LpINT32)&argv[0].u.Real)[1];

		/* ingore the sign */
		tHiBits = tHiBits & 0x7FFFFFFF;

		/* QNaN - 0x7FF8000000000000 - 0x7FFFFFFFFFFFFFFF
		 * SNaN - 0x7FF0000000000001 - 0x7FF7FFFFFFFFFFFF
		 * Inf  - 0x7FF0000000000000 (Not a NaN)
		 */
		if (((tHiBits & NAN_64_HI) == NAN_64_HI) ||
			(((tHiBits & INF_64_HI) == INF_64_HI) && !((tHiBits == INF_64_HI) && (tLoBits == INF_64_LO))))
			asBool(ret) = TRUE;
		break;

	default:

		break;
}

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate_NotNaN

Return true if argv[0] is not #void, nan, snan, or negative zero.

    (isNotNaN a)

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate_NotNaN(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

*ret = FPredicate_NaN(gCP, gTP, argc, argv);
ExitOnError(*ret);

asBool(ret) = !(asBool(ret));

FrameExit(*ret);
}
