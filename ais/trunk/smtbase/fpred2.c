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

#define _SMARTBASE
#if 0
FPredicate2.c

Implementation of utility functions which are used to return the boolean
result of a particular test or expression.

PARENT:             None. 

AUTHORS:            Michael F. Korns

MODIFICATIONS:

#endif

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
#include    "tdatabas.h"
#include    "fdebug.h"
#include    "tmatrix.h"
#include    "tnummat.h"
#include	"tcpx.h"
#include	"tcpxvec.h"
#include	"tshortvec.h"
#include	"tlongvec.h"
#include	"tbrick.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Init

Initialize a portion of the the SmartLisp function library.  

#endif

TVAL FPredicate2_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,alsoSymbol);
DeclareTVAL(ec);
EndFrame
 
if (gCP->FPred2_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FPred2_Initialized = TRUE;

/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isNegative",(LpFUNC)&FPredicate2_Negativep);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isPositive",(LpFUNC)&FPredicate2_Positivep);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isZero",(LpFUNC)&FPredicate2_Zerop);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isDate",(LpFUNC)&FPredicate2_Datep);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isMoney",(LpFUNC)&FPredicate2_Moneyp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isInteger",(LpFUNC)&FPredicate2_Integerp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isNumber",(LpFUNC)&FPredicate2_Numberp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isNull",(LpFUNC)&FPredicate2_Nullp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isPair",(LpFUNC)&FPredicate2_Pairp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isFunction",(LpFUNC)&FPredicate2_Procedurep);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isMacro",(LpFUNC)&FPredicate2_Macrop);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isIdentical",(LpFUNC)&FPredicate2_Identicalp);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"=="), aSymbol->itsGlobalValue);

*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"compare",(LpFUNC)&FPredicate2_Compare);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"compareLT",(LpFUNC)&FPredicate2_CompareLT);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"compareLE",(LpFUNC)&FPredicate2_CompareLE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isEqual",(LpFUNC)&FPredicate2_Equal);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"equal"), aSymbol->itsGlobalValue);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"compareEQ"), aSymbol->itsGlobalValue);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"compareNE",(LpFUNC)&FPredicate2_CompareNE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"compareGE",(LpFUNC)&FPredicate2_CompareGE);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"compareGT",(LpFUNC)&FPredicate2_CompareGT);
ExitOnError( *ec);

*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isType",(LpFUNC)&FPredicate2_Predicate);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"type",(LpFUNC)&FPredicate2_Type);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isStructure",(LpFUNC)&FPredicate2_Structure);
ExitOnError( *ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"isStructure"), aSymbol->itsGlobalValue);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isDictionary",(LpFUNC)&FPredicate2_Dictionary);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isDirectory",(LpFUNC)&FPredicate2_Directory);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isCongruent",(LpFUNC)&FPredicate2_Congruentp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isError",(LpFUNC)&FPredicate2_Errorp);
ExitOnError( *ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isMember",(LpFUNC)&FPredicate2_Memeqv);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isInside",(LpFUNC)&FPredicate2_IsInside);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"isMemEqv"), aSymbol->itsGlobalValue);
*ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isComplex",(LpFUNC)&FPredicate2_Complexp);
ExitOnError( *ec);
FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Negativep

The FPredicate2_Negativep function returns true if the argument is negative. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Negativep(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYBOLE;
if (argc != 1) 
	{
	*ret = TERROR("!isNegative: Expecting a single argument!");
	FrameExit(*ret);
	}

/*  Convert argument to a real. */

if ((asTag(&argv[0]) == TYREAL)  || 
    (asTag(&argv[0]) == TYDATE)  ||
    (asTag(&argv[0]) == TYMONEY))
    asBool(ret) = FFloat_NEGCHECK(gCP, gTP, argv[0].u.Real);
else
if (asTag(&argv[0]) == TYUNUM)
    asBool(ret) = (argv[0].u.UInt < 0);
else
if (asTag(&argv[0]) == TYNUM)
    asBool(ret) = (argv[0].u.Int < 0);
else
    asBool(ret) = FALSE;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Positivep

The FPredicate2_Positivep function returns true if the argument is positive. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Positivep(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;

if (argc != 1)
	{
	*ret = TERROR("!isPositive: Expecting a single argument!");
	FrameExit(*ret);
	}

/*  Convert argument to a real. */

if ((asTag(&argv[0]) == TYREAL)  || 
    (asTag(&argv[0]) == TYDATE)  ||
    (asTag(&argv[0]) == TYMONEY))
    asBool(ret) = FFloat_POSNUMCHECK(gCP,gTP,argv[0].u.Real);
else
if (asTag(&argv[0]) == TYNUM)
    asBool(ret) = (argv[0].u.Int > 0);
else
if (asTag(&argv[0]) == TYUNUM)
    asBool(ret) = (argv[0].u.UInt > 0);
else
    asBool(ret) = FALSE;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Zerop

The FPredicate2_Zerop function returns true if the argument is zero. 

Note:   Exactly one argument is expected, anything else is an error.
#endif
TVAL FPredicate2_Zerop(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TCpx, cp);
EndFrame
 
asTag(ret) = TYBOLE;
if (argc != 1) 
	{
	*ret = TERROR("!isZero: Expecting a single argument!");
	FrameExit(*ret);
	}
//  Convert argument to a real
if ((asTag(&argv[0]) == TYREAL)  || 
    (asTag(&argv[0]) == TYDATE)  ||
    (asTag(&argv[0]) == TYMONEY))
    asBool(ret) = FFloat_ZEROCHECK(gCP, gTP, argv[0].u.Real);
else if (asTag(&argv[0]) == TYUNUM)
    asBool(ret) = (argv[0].u.UInt == 0);
else if (asTag(&argv[0]) == TYNUM)
    asBool(ret) = (argv[0].u.Int == 0);
else if (asTag(&argv[0]) == TYCPX)
{	cp = argv[0].u.Complex;
	ret->u.Bool = FFloat_ZEROCHECK(gCP, gTP, cp->itsReal) == TRUE &&
						FFloat_ZEROCHECK(gCP, gTP, cp->itsImag) == TRUE;
}
else
    asBool(ret) = FALSE;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Datep

The FPredicate2_Datep function returns true if the argument is a Date object. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Datep(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
if (argc != 1)
	{
	*ret = TERROR("!isDate: Expecting a single argument!");
	FrameExit(*ret);
	}

/*  Convert argument to a real. */

if (asTag(&argv[0]) == TYDATE)
    asBool(ret) = FFloat_NUMCHECK(gCP, gTP, argv[0].u.Real);
else
    asBool(ret) = FALSE;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Moneyp

The FPredicate2_Moneyp function returns true if the argument is a Money object. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Moneyp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
if (argc != 1)
	{
	*ret = TERROR("!isMoney: Expecting a single argument!");
	FrameExit(*ret);
	}


/*  Convert argument to a real. */

if (asTag(&argv[0]) == TYMONEY)
    asBool(ret) = FFloat_NUMCHECK(gCP, gTP, argv[0].u.Real);
else
    asBool(ret) = FALSE;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Numberp

The FPredicate2_Numberp function returns true if the argument is a Numeric object. 

PORTATION NOTE: The code to determine whether a real quantity is valid,
                is machine dependent. During portation this code must be
                checked for accuracy on the new host machine.

#endif

TVAL FPredicate2_Numberp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])      
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
if (argc != 1) 
	{
	*ret = TERROR("!isNumber: Expecting a single argument!");
	FrameExit(*ret);
	}

if ((asTag(&argv[0]) == TYREAL)  || 
    (asTag(&argv[0]) == TYDATE)  ||
    (asTag(&argv[0]) == TYMONEY))
    asBool(ret) = FFloat_NUMCHECK(gCP, gTP, argv[0].u.Real);
else
if (asTag(&argv[0]) == TYUNUM)
    asBool(ret) = TRUE;
else
if (asTag(&argv[0]) == TYNUM)
    asBool(ret) = TRUE;
else
    asBool(ret) = FALSE;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Integerp

The FPredicate2_Integerp function returns true if the argument is an Integer object. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Integerp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])     
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

if (argc != 1) 
	{
	*ret = TERROR("!isInteger: Expecting a single argument!");
	FrameExit(*ret);
	}

if (asTag(&argv[0]) == TYNUM)
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Nullp

The FPredicate2_Nullp function returns true if the argument is the empty list object. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Nullp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

if (argc != 1) 
	{
	*ret = TERROR("!isNull: Expecting a single argument!");
	FrameExit(*ret);
	}

if (isNullTval(&argv[0])) 
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Pairp

The FPredicate2_Pairp function returns true if the argument is a Pair object. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Pairp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

if (argc != 1)
	{
	*ret = TERROR("!isPair: Expecting a single argument!");
	FrameExit(*ret);
	}

if ((asTag(&argv[0]) == TYPAIR)  && (asObj(&argv[0]) != NIL)) 
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Errorp

The FPredicate2_Errorp function returns true if the argument is an Error Tval. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Errorp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1)
	{
	*ret = TERROR("!isError: Expecting a single argument!");
	FrameExit(*ret);
	}

if ((asTag(&argv[0]) == TYERROR)  && (asObj(&argv[0]) != NIL)) 
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Procedurep

The FPredicate2_Procedurep function returns true if the argument is a Procedure object. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Procedurep(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

if (argc != 1)
	{
	*ret = TERROR("!isFunction: Expecting a single argument!");
	FrameExit(*ret);
	}

if ((asTag(&argv[0]) == TYLAMBDA)     || 
    (asTag(&argv[0]) == TYCPROCEDURE)    || 
    (asTag(&argv[0]) == TYCONTINUATION)) 
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_Macrop

The FPredicate2_Macrop function returns true if the argument is a Macro or cMacro object. 

Note:   Exactly one argument is expected, anything else is an error.

#endif
TVAL FPredicate2_Macrop(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])       
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1)
	{
	*ret = TERROR("!isMacro: Expecting a single argument!");
	FrameExit(*ret);
	}


if ((asTag(&argv[0]) == TYMACRO) || 
	(asTag(&argv[0]) == TYSPECIALFORM) ||
	(asTag(&argv[0]) == TYMACRO)) 
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_Predicate

The FPredicate2_Predicate cProcedure is a predicate which returns true if the argument 
{obj} is a structure (see defineStructure) of type {name}; otherwise, 
the predicate returns false. The isType cProcedure can also be used to 
check the types of  natives SmartLisp objects.

Some examples follow.
    (defineStructure  employee  name  job  salary)         =>  employee
    (define  X  (make_employee  `salary  2201.34))          
                            =>  #{`name  ()  `job  ()  `salary  2201.34}
    (defineStructure  manager  include: employee  perks)   =>  manager
    (define  Y  (make_manager)  `perks  "parking")          
                =>  #{`name  ()  `job  ()  `salary  ()  `perks  "parking"}
    (isType  `employee  X)                                 =>  true
    (isType  `employee  Y)                                 =>  true
    (isType  `manager  X)                                  =>  false
    (isType  `manager  Y)                                  =>  true
    (isType  `String  "Hello")                             =>  true
    (isType  `Number  22.34)                               =>  true
    (isType  `Vector  #(1 2 3))                            =>  true

Note:   In SmartLisp FPredicate2_Predicate is bound to the name: isType.
  
#endif

TVAL FPredicate2_Predicate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])    
{
TYPE                    type;
StartFrame
DeclareOBJ(TSymbol,StructureType);
DeclareOBJ(TSymbol,name);
DeclareOBJ(TStructure,sp);
DeclareTVAL(ec);
DeclareTVAL(ret);
EndFrame
 
/*  Make sure there are two arguments and the first is a valid symbol! */

asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

if ((argc != 2) || (argv[0].Tag != TYSYMBOL))
	{
	*ret = TERROR("!isType:Expecting two arguments. The first must be a type Symbol!");
	FrameExit(*ret);
	}

name = Symbol(argv[0]);

/*  Check the type for all native types. */

*ec = FUtil2_SymbolToNativeType(gCP,gTP,&type,name);
if ((ec->Tag == TYBOLE) && (ec->u.Bool == FALSE)) goto Bad;

/*  Check the type for Strings. */

if (type == TYSTRING)
    {
    if ((argv[1].Tag != TYSTRING) && (argv[1].Tag != TYTEXT) && (argv[1].Tag != TYSTRINGSUBSTR)) goto Bad;
    ret->u.Bool = TRUE;
    FrameExit(*ret);
    }

/*  Check the type for Strings. */

if (type == TYREAL)
    {
    if ((asTag(&argv[1]) != TYNUM)    &&
        (asTag(&argv[1]) != TYREAL)) goto Bad;
    ret->u.Bool = TRUE;
    FrameExit(*ret);
    }
 

/*  Check the type of any other SmartLisp objects. */

if (type == argv[1].Tag)
    {
    ret->u.Bool = TRUE;
    FrameExit(*ret);
    }
 
Bad:
asBool(ret) = FALSE;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_Type

The FPredicate2_Type Procedure returns the type symbol of the argument.

Some examples follow.

    (defineStructure  employee  name  job  salary)         =>  employee
    (define  X  (make_employee  `salary  2201.34))          
                            =>  #{`name  ()  `job  ()  `salary  2201.34}
    (defineStructure  manager  include: employee  perks)   =>  manager
    (define  Y  (make_manager)  `perks  "parking")          
                =>  #{`name  ()  `job  ()  `salary  ()  `perks  "parking"}
    (type  X)                           =>  employee
    (type  Y)                           =>  manager
    (type  "Hello")                     =>  String
    (type  22.34)                       =>  Number
    (type  #(1 2 3))                    =>  Vector

#endif

TVAL FPredicate2_Type(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]) 
{
TYPE            type;
StartFrame
DeclareOBJ(TSymbol,name);
DeclareOBJ(TStructure,sp);
DeclareTVAL(ret);
EndFrame
 
/*  Make sure there is one argument! */

if (argc != 1)
	{
	*ret = TERROR("!type: Expecting a single argument!");
	FrameExit(*ret);
	}

/*  Save the type of the argument. */

type = (argv[0].Tag == TYTYPE) ? argv[0].u.Type : argv[0].Tag;

/*  Convert the type for all native types. */

name = TSymbol_MakeUnique(gCP,gTP,_TObject_TypeName(type));
ret->u.Object = (TObject*)name;
ret->Tag = TYSYMBOL;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_Structure

The FPredicate2_Structure function returns true if the argument is an 
Structure object. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Structure(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])  
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;
if (argc != 1)
	{
	*ret = TERROR("!isStructure: Expecting a single argument!");
	FrameExit(*ret);
	}

if (asTag(&argv[0]) == TYSTRUCTURE) 
    asBool(ret) = TRUE;

FrameExit( *ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FPredicate2_Complexp

Returns true iff the argument is Complex. 
Note:   Exactly one argument is expected, anything else is an error.
#endif
TVAL FPredicate2_Complexp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])  
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
ret->Tag = TYBOLE;
ret->u.Bool = FALSE;
if (argc != 1)
{	*ret = TERROR("!isComplex: Expecting a single argument!");
	FrameExit(*ret)
}
if (argv[0].Tag == TYCPX) 
    ret->u.Bool = TRUE;

FrameExit(*ret)
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_Dictionary

The FPredicate2_Dictionary function returns true if the argument is a 
Dictionary object. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Dictionary(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])   
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

if (argc != 1)
	{
	*ret = TERROR("!isDictionary: Expecting a single argument!");
	FrameExit(*ret);
	}

if (asTag(&argv[0]) == TYDICTIONARY) 
    asBool(ret) = TRUE;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_Directory

The FPredicate2_Directory function returns true if the argument is a 
Directory object. 

Note:   Exactly one argument is expected, anything else is an error.

#endif

TVAL FPredicate2_Directory(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])   
{
StartFrame
DeclareTVAL(ret);
EndFrame
 
asTag(ret) = TYBOLE;
asBool(ret) = FALSE;

if (argc != 1)
	{
	*ret = TERROR("!isDirectory: Expecting a single argument!");
	FrameExit(*ret);
	}

if (asTag(&argv[0]) == TYDIRECTORY) 
    asBool(ret) = TRUE;

FrameExit(*ret);
}
/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_Congruentp

Check two SmartLisp Procedure objects to see if they are the same. The 
third argument is true if we wish to allow constant folding, expression 
reduction, etc.; otherwise, only isomorphic Procedure objects will be 
considered congruent. The isCongruent cPredicate returns true
if the two Procedure objects are the same, and false otherwise.

        (isCongruent `foo `foo)  ==> true

Note:   The FPredicate2_Congruentp function expects at least two arguments.

#endif

TVAL FPredicate2_Congruentp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])   
{
BOLE                advancedMethods;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(global1);
DeclareTVAL(global2);
EndFrame

asTag(ret)  = TYBOLE;                       
asBool(ret) = FALSE;
advancedMethods = FALSE;                        
if ((argc < 2) || (argc > 3)) return (gCP->TObject_ERROR_INVALID_ARGLIST);
if (argc == 3)
    {
    if (asTag(&argv[2]) != TYBOLE)
        {
        FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    advancedMethods = asBool(&argv[2]);
    }
    
if ((asTag(&argv[0]) == TYSYMBOL) && (asTag(&argv[1]) == TYSYMBOL)) 
    {
    *global1 = asSymbol(&argv[0])->itsGlobalValue;
    *global2 = asSymbol(&argv[1])->itsGlobalValue;
    if (asTag(global1) != asTag(global2)) 
        {
        FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    else
        {
        if ((asTag(global1) != TYLAMBDA) && (asTag(global1) != TYMACRO)) 
            {
            FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
            }
        else
            {
            if(asObject(global1) == asObject(global2))
                {
                FrameExit( gCP->TObject_TRUE);
                }
            else
                {
                FrameExit(gCP->TObject_FALSE);
                }
            }
        }
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    }
else
    {
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
    }
*ret =  FUtil2_CongruentProcs(gCP,gTP,asObj(&argv[0]),asObj(&argv[1]),advancedMethods);
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_FullCompare

Compares any two SmartLisp objects to determine if they are equivalent.
Native objects are equivalent if they are equal after conversion. Compound
objects are equivalent if their components are equivalent.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate2_FullCompare(LpXCONTEXT gCP, LpTHREAD gTP, TVAL left,TVAL right)  
{
TVAL			argv[2];
TVAL			compareCode;

argv[0] = left;
argv[1] = right;
compareCode = FPredicate2_Compare(gCP,gTP,2,&argv[0]);
if (compareCode.Tag == TYNUM)
    {
    compareCode.Tag = TYCOMPARE;
    compareCode.u.Compare = (short)compareCode.u.Int;
    }
return(compareCode);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_DeepCompare

Compares any two SmartLisp objects to determine if they are equivalent.
Native objects are equivalent if they are equal after conversion. Compound
objects are equivalent if their components are equivalent.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate2_DeepCompare(LpXCONTEXT gCP,LpTHREAD gTP,TVAL left,TVAL right)  
{
TVAL			argv[2];

argv[0] = left;
argv[1] = right;
return(FPredicate2_Compare(gCP,gTP,2,&argv[0]));
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_QuickCompare

Compares any two SmartLisp objects to determine if they are equivalent.
Native objects are equivalent if they are equal after conversion. Compound
objects are equivalent if their components are equivalent.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

NUM FPredicate2_QuickCompare(LpXCONTEXT gCP,LpTHREAD gTP,LpTVAL left,LpTVAL right)  
{
TVAL			argv[2];
TVAL			compareCode;

argv[0] = *left;
argv[1] = *right;
compareCode = FPredicate2_Compare(gCP,gTP,2,&argv[0]);
return(compareCode.u.Compare);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_Compare

Compares any two SmartLisp objects to determine if they are equivalent.
Native objects are equivalent if they are equal after conversion. Compound
objects are equivalent if their components are equivalent.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate2_Compare(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])  
{
NUM                 indexOf;
NUM                 sizeOf;
LpF2TVALS			aFunction;
REAL				rtemp;
NUM					itemp;
NUM					MSize;
NUM					LSize;
NUM					RSize;
NUM					LIndex;
NUM					RIndex;
NUM					index1;
NUM					index2;
LpCHAR				LPtr;
LpCHAR				RPtr;
LpBIND				fldPtr;
NUM					fldIndexOf;
NUM					fldSizeOf;
NUM					repIndexOf;
NUM					repSizeOf;
TVAL				rLOW;
TVAL				rHIGH;
TVAL				rEQUAL;
LpBIND				bindPtr;
LpCHAR				fieldArrayPtr;
StartFrame
DeclareOBJ(TBrick,rp);
DeclareOBJ(TVector,vAp);
DeclareOBJ(TVector,vBp);
DeclareOBJ(TMatrix,mAp);
DeclareOBJ(TMatrix,mBp);
DeclareOBJ(TNumMatrix,nAp);
DeclareOBJ(TNumMatrix,nBp);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TBitVector,vBitt);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TByteVector,vBytet);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TNumVector,vNumt);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TFltVector,vFltt);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TIntVector,vIntt);
DeclareOBJ(TShtVector,vShtp);
DeclareOBJ(TShtVector,vShtt);
DeclareOBJ(TLongVector,vLngp);
DeclareOBJ(TLongVector,vLngt);
DeclareOBJ(TPcodeVector,vPcdp);
DeclareOBJ(TPcodeVector,vPcdt);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TObjVector,vObjt);
DeclareOBJ(TStructure,eAp);
DeclareOBJ(TStructure,eBp);
DeclareOBJ(TDictionary,dAp);
DeclareOBJ(TDictionary,dBp);
DeclareOBJ(TDirectory,xAp);
DeclareOBJ(TDirectory,xBp);
DeclareOBJ(TPair,pAp);
DeclareOBJ(TPair,pBp);
DeclareOBJ(TCpx, cAp);
DeclareOBJ(TCpx, cBp);
DeclareOBJ(TCpxVector,vCpxp);
DeclareOBJ(TCpxVector,vCpxt);
DeclareOBJ(TBrick,bAp);
DeclareOBJ(TBrick,bBp);
DeclareTVAL(ret);
DeclareTVAL(result);
DeclareTVAL(ec);
DeclareTVAL(ndx1);
DeclareTVALArray(prmv,2);
EndFrame

/*  Initialization */

if (argc != 2) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
rLOW.Tag = TYNUM;
rLOW.u.Int = -1;
rHIGH.Tag = TYNUM;
rHIGH.u.Int = 1;
rEQUAL.Tag = TYNUM;
rEQUAL.u.Int = 0;
*ret = rEQUAL;


/*  Return an ERROR if we have recursed down too far. */
/*  Note: A deep compare will recurse infinitely deep */
/*        when applied to a circular object closure. */
/*        We solve this halting problem by stopping if */
/*        we have exceeded some arbitrary limit, and */
/*        since we are undecided, we return an ERROR. */

if ((gTP->RecursionCount > (gTP->MaxRecursions - 200)) || 
	(__tval__ > (gTP->MaxTvalStackSize - 200)) || 
	(__obj__ > (gTP->MaxObjStackSize - 200)))
	{
	*ec = TERROR("!compare: too many recursions!");
	FrameExit(*ec);
	}    

/*  Perform a deep comparison on both objects. */

switch(argv[0].Tag)
    {
    case TYVOID:
        if (argv[0].Tag < argv[1].Tag)
			{
            FrameExit(rLOW);
			}
        else
        if (argv[0].Tag > argv[1].Tag)
			{
            FrameExit(rHIGH);
			}
        else
			{
            FrameExit(rEQUAL);
			}
        break;
    
    case TYBYTEVECTOR:
        LPtr = (char *)ByteArray(argv[0]);
        LSize = ByteVector(argv[0])->itsMaxItemIndex;

        RightCompare:

        switch (argv[1].Tag)
            {
            case TYBYTEVECTOR:
				if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
                RPtr = (char *)ByteArray(argv[1]);
                RSize = ByteVector(argv[1])->itsMaxItemIndex;
                goto MemoryCompare;
                break;
                
            case TYTEXT:
                RPtr = (char *)&argv[1].u.Text[0];
                RSize = strlen(RPtr);
                goto MemoryCompare;
                break;
   
            case TYSTRINGSUBSTR:
                RPtr = (char*)TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
                RSize = SubLen(argv[1]);
                goto MemoryCompare;
                break;
            
            case TYCHAR:    
                argv[1].u.Text[1] = 0;
                RPtr = (char *)&argv[1].u.Text[0];
                RSize = 1;            
                goto MemoryCompare;
                break;

            case TYERROR:
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
            case TYSTRING:
            case TYSPECIALFORM:
                RPtr = (char *)CharArray(argv[1]);
                RSize = strlen(RPtr);

                MemoryCompare:
                
                MSize = (LSize < RSize) ? LSize : RSize;
                itemp = memcmp(LPtr,RPtr,MSize);
                FrameExit(itemp ? (itemp > 0 ? rHIGH : rLOW) : (LSize == RSize ? rEQUAL : (LSize > RSize ? rHIGH : rLOW)));
                break;
                
            default:
                if (argv[0].Tag < argv[1].Tag)
					{
                    FrameExit(rLOW);
					}
                else
					{
                    FrameExit(rHIGH);
					}
                break;
            }
        break;
        
    case TYTEXT:
        LPtr = (char *)&argv[0].u.Text[0];
        LSize = strlen(LPtr);
        goto RightCompare;
        break;
       
    case TYSTRINGSUBSTR:
        LPtr = (char*)TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        LSize = SubLen(argv[0]);
        goto RightCompare;
        break;

    case TYSYMBOL:
	case TYQUOTEDSYMBOL:
    case TYSTRING:
    case TYSPECIALFORM:
    case TYERROR:
        LPtr = (char *)CharArray(argv[0]);
        LSize = strlen(LPtr);
        goto RightCompare;
        break;
 
	case TYCPROCEDURE:
	case TYCMACRO:
		if ((argv[1].Tag != TYCPROCEDURE) && (argv[1].Tag != TYCMACRO)) goto CompareDefault;
        LPtr = (char *)CharArray(argv[0]);
        LSize = strlen(LPtr);
        RPtr = (char *)CharArray(argv[1]);
        RSize = strlen(RPtr);
        goto MemoryCompare;
        break;

	case TYLAMBDA:
	case TYMACRO:
		if ((argv[1].Tag != TYLAMBDA) && (argv[1].Tag != TYMACRO)) goto CompareDefault;
		if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
		*ret = FPredicate2_DeepCompare(gCP, gTP, TOBJ(argv[0].u.Lambda->ConstantVariables),TOBJ(argv[1].u.Lambda->ConstantVariables));
		if ((ret->Tag != TYNUM) || (ret->u.Int != EQUAL)) FrameExit(*ret);
		*ret = FPredicate2_DeepCompare(gCP, gTP, TOBJ(argv[0].u.Lambda->PcodeVector),TOBJ(argv[1].u.Lambda->PcodeVector));
		if ((ret->Tag != TYNUM) || (ret->u.Int != EQUAL)) FrameExit(*ret);
		*ret = FPredicate2_DeepCompare(gCP, gTP, TOBJ(argv[0].u.Lambda->ArgumentVariables),TOBJ(argv[1].u.Lambda->ArgumentVariables));
		if ((ret->Tag != TYNUM) || (ret->u.Int != EQUAL)) FrameExit(*ret);
		*ret = FPredicate2_DeepCompare(gCP, gTP, TOBJ(argv[0].u.Lambda->TemporaryVariables),TOBJ(argv[1].u.Lambda->TemporaryVariables));
		if ((ret->Tag != TYNUM) || (ret->u.Int != EQUAL)) FrameExit(*ret);
		*ret = FPredicate2_DeepCompare(gCP, gTP, TOBJ(argv[0].u.Lambda->RegisterVariables),TOBJ(argv[1].u.Lambda->RegisterVariables));
		if ((ret->Tag != TYNUM) || (ret->u.Int != EQUAL)) FrameExit(*ret);
		*ret = FPredicate2_DeepCompare(gCP, gTP, TOBJ(argv[0].u.Lambda->ClassVariables),TOBJ(argv[1].u.Lambda->ClassVariables));
		if ((ret->Tag != TYNUM) || (ret->u.Int != EQUAL)) FrameExit(*ret);
		*ret = FPredicate2_DeepCompare(gCP, gTP, TOBJ(argv[0].u.Lambda->Interfaces),TOBJ(argv[1].u.Lambda->Interfaces));
		if ((ret->Tag != TYNUM) || (ret->u.Int != EQUAL)) FrameExit(*ret);
		*ret = FPredicate2_DeepCompare(gCP, gTP, TOBJ(argv[0].u.Lambda->VirtualMachine),TOBJ(argv[1].u.Lambda->VirtualMachine));
		if ((ret->Tag != TYNUM) || (ret->u.Int != EQUAL)) FrameExit(*ret);
		*ret = FPredicate2_DeepCompare(gCP, gTP, TOBJ(argv[0].u.Lambda->PersistantVariables),TOBJ(argv[1].u.Lambda->PersistantVariables));
		if ((ret->Tag == TYERROR) && (strcmp(CharArray(*ret),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
		if ((ret->Tag != TYNUM) || (ret->u.Int != EQUAL)) FrameExit(*ret);
        FrameExit(*ret);
        break;

	case TYVMEVALUATOR:
		if (argv[1].Tag != TYVMEVALUATOR) goto CompareDefault;
        LPtr = (char *)CharArray(argv[0]);
        LSize = strlen(LPtr);
        RPtr = (char *)CharArray(argv[1]);
        RSize = strlen(RPtr);
        goto MemoryCompare;
        break;
 
	case TYOBJREPOSITORY:
		if (argv[1].Tag != TYOBJREPOSITORY) goto CompareDefault;
		prmv[0] = TOBJ(argv[0].u.Repository->itsFileName);
		prmv[1] = TOBJ(argv[1].u.Repository->itsFileName);
		*ret = FPredicate2_Compare(gCP,gTP,2,&prmv[0]);
		if ((ret->Tag == TYERROR) && (strcmp(CharArray(*ret),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
		ExitOnError(*ret);
		if (ret->u.Int != EQUAL) FrameExit(*ret);
		*ret = FPredicate2_DeepCompare(gCP, gTP, TINT(argv[0].u.Repository->itsBaseFilePos),TINT(argv[1].u.Repository->itsBaseFilePos));
		if ((ret->Tag == TYERROR) && (strcmp(CharArray(*ret),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
		FrameExit(*ret);
        break;

    case TYUNUM:
        switch (argv[1].Tag)
            {
			case TYUNUM:
				FrameExit((argv[0].u.UInt != argv[1].u.UInt) ? ((argv[0].u.UInt > argv[1].u.UInt) ? rHIGH : rLOW) : rEQUAL);
				break;

            case TYNUM:
                FrameExit((argv[0].u.UInt != argv[1].u.Int) ? ((argv[0].u.UInt > argv[1].u.Int) ? rHIGH : rLOW) : rEQUAL);
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
				FrameExit((argv[0].u.UInt != argv[1].u.Real) ? ((argv[0].u.UInt > argv[1].u.Real) ? rHIGH : rLOW) : rEQUAL);
                break;
                
            case TYCHAR:
                FrameExit((argv[0].u.UInt != argv[1].u.Char) ? ((argv[0].u.UInt > argv[1].u.Char) ? rHIGH : rLOW) : rEQUAL);
                break;

			case TYCPX:
				if ((cAp = argv[1].u.Complex) != NULL)
				{	if ((rtemp = (REAL)argv[0].u.UInt - cAp->itsReal) == 0.0)
						rtemp = -cAp->itsImag;
					FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
				}
				// Fall thru if cAp is NIL
            default:
                if (argv[0].Tag < argv[1].Tag)
					{
                    FrameExit(rLOW);
					}
                else
					{
                    FrameExit(rHIGH);
					}
                break;
            }
        break;

    case TYNUM:
        switch (argv[1].Tag)
            {
            case TYNUM:
                rtemp = argv[0].u.Int - argv[1].u.Int;
                FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                rtemp = (REAL)argv[0].u.Int - argv[1].u.Real;
                FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
                break;
                
            case TYCHAR:
                rtemp = (REAL)argv[0].u.Int - argv[1].u.Char;
                FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
                break;
			case TYCPX:
				if ((cAp = argv[1].u.Complex) != NULL)
				{	if ((rtemp = (REAL)argv[0].u.Int - cAp->itsReal) == 0.0)
						rtemp = -cAp->itsImag; 
					FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
				}
				// Fall thru if cAp is NIL
            default:
                if (argv[0].Tag < argv[1].Tag)
					{
                    FrameExit(rLOW);
					}
                else
					{
                    FrameExit(rHIGH);
					}
                break;
            }
        break;
        
    case TYDATE:
    case TYMONEY:
    case TYREAL:
        switch (argv[1].Tag)
            {
            case TYNUM:
                rtemp = argv[0].u.Real - argv[1].u.Int;
                FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                rtemp = argv[0].u.Real - argv[1].u.Real;
                FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
                break;
                
            case TYCHAR:
                rtemp = argv[0].u.Real - argv[1].u.Char;
                FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
                break;
			case TYCPX:
				if ((cAp = argv[1].u.Complex) != NULL)
				{	if ((rtemp = argv[0].u.Real - cAp->itsReal) == 0.0)
						rtemp = -cAp->itsImag; 
					FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
				}
				// Fall thru if cAp is NIL
            default:
                if (argv[0].Tag < argv[1].Tag)
					{
                    FrameExit(rLOW);
					}
                else
					{
                    FrameExit(rHIGH);
					}
                break;
            }
        break;
        
    case TYCHAR:
        switch (argv[1].Tag)
            {
            case TYNUM:
                rtemp = argv[0].u.Char - argv[1].u.Int;
                FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                rtemp = argv[0].u.Char - argv[1].u.Real;
                FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
                break;
                
            case TYCHAR:
                rtemp = argv[0].u.Char - argv[1].u.Char;
                FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
                break;
                
            case TYTEXT:
            case TYSTRINGSUBSTR:
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
            case TYSTRING:
            case TYSPECIALFORM:
                argv[0].u.Text[1] = 0;
                LPtr = (char *)&argv[0].u.Text[0];
                LSize = 1;
                goto RightCompare;
                break;

			case TYCPX:
				if ((cAp = argv[1].u.Complex) != NULL)
				{	if ((rtemp = (REAL)argv[0].u.Char - cAp->itsReal) == 0.0)
						rtemp = -cAp->itsImag; 
					FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
				}
				// Fall thru if cAp is NIL
            default:
                if (argv[0].Tag < argv[1].Tag)
					{
                    FrameExit(rLOW);
					}
                else
					{
                    FrameExit(rHIGH);
					}
                break;
            }
        break;

	case TYCPX:
		if ((cAp = argv[0].u.Complex) == NULL)
			FrameExit(rLOW) 
		switch (argv[1].Tag)
		{
        case TYNUM:
			if ((rtemp = cAp->itsReal - (REAL)argv[1].u.Char) == 0.0)
				rtemp = cAp->itsImag; 
			FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
            break;
            
        case TYDATE:
        case TYMONEY:
        case TYREAL:
			if ((rtemp = cAp->itsReal - argv[1].u.Real) == 0.0)
				rtemp = cAp->itsImag; 
			FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
            break;
            
        case TYCHAR:
			if ((rtemp = cAp->itsReal - (REAL)argv[1].u.Char) == 0.0)
				rtemp = cAp->itsImag; 
			FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
            break;
 
		case TYCPX:
			if ((cAp = argv[0].u.Complex) == NULL)
				rtemp = -1.0;
			else if ((cBp = argv[1].u.Complex) == NULL)
				rtemp = 1.0;
			else
			{	rtemp = cAp->itsReal - cBp->itsReal;
				if (rtemp == 0.0)
					rtemp = cAp->itsImag - cBp->itsImag;
			}
			FrameExit((rtemp != 0.0) ? (rtemp > 0.0) ? rHIGH : rLOW : rEQUAL)
			break;

		default:
			FrameExit((argv[0].Tag < argv[1].Tag) ? rLOW : rHIGH)
			break;
		}
		break;

    case TYBOLE:
        switch (argv[1].Tag)
            {
            case TYBOLE:
                rtemp = argv[0].u.Bool - argv[1].u.Bool;
                FrameExit(rtemp ? (rtemp > 0 ? rHIGH : rLOW) : rEQUAL);
                break;
                
            default:
                if (argv[0].Tag < argv[1].Tag)
					{
                    FrameExit(rLOW);
					}
                else
					{
                    FrameExit(rHIGH);
					}
                break;
            }
        break;

   case TYBRICKFIELD:
        rp = (TBrick*)_TObject_MainObjectList(ObjIdx(argv[0]));
		bindPtr = BindArray(rp->itsFieldList);
		index1 = RowIdx(argv[0]);
		index2 = FldIdx(argv[0]);
		fieldArrayPtr = (char*)*rp->itsFieldArray;
		fieldArrayPtr += (index1*rp->itsRowByteCount);
		LPtr = (POINTER)(fieldArrayPtr+bindPtr[index2].Value.Offset);		    
		switch (bindPtr->Value.DeclaredType)
			{
			case TYBOLE:
				LSize = ((NUM)bindPtr->Value.Modifier)*sizeof(BOLE);
				break;

			case TYREAL:
			case TYDATE:
			case TYMONEY:
				LSize = ((NUM)bindPtr->Value.Modifier)*sizeof(REAL);
				break;

			case TYUNUM:
			case TYNUM:
			case TYCHARPOINTER:
			case TYFLOATPOINTER:
			case TYREALPOINTER:
			case TYJUMPPOINTER:
			case TYINTPOINTER:
			case TYSHORTPOINTER:
			case TYLONGPOINTER:
			case TYWORDPOINTER:
				LSize = ((NUM)bindPtr->Value.Modifier)*sizeof(NUM);
				break;

			case TYFLOAT:
				LSize = ((NUM)bindPtr->Value.Modifier)*sizeof(FLOAT);
				break;

			case TYOBJ:
				LSize = ((NUM)bindPtr->Value.Modifier)*sizeof(OBJ);
				break;

			case TYSHORT:
				LSize = ((NUM)bindPtr->Value.Modifier)*sizeof(SHORT);
				break;

			case TYLONG:
				LSize = ((NUM)bindPtr->Value.Modifier)*sizeof(LONG);
				break;

			case TYTVAL:
				LSize = ((NUM)bindPtr->Value.Modifier)*sizeof(TVAL);
				break;
			}
		switch (argv[1].Tag)
            {
			case TYBRICKFIELD:
				rp = (TBrick*)_TObject_MainObjectList(ObjIdx(argv[1]));
				bindPtr = BindArray(rp->itsFieldList);
				index1 = RowIdx(argv[1]);
				index2 = FldIdx(argv[1]);
				fieldArrayPtr = (char*)*rp->itsFieldArray;
				fieldArrayPtr += (index1*rp->itsRowByteCount);
				RPtr = (POINTER)(fieldArrayPtr+bindPtr[index2].Value.Offset);		    
				switch (bindPtr->Value.DeclaredType)
					{
					case TYBOLE:
						RSize = ((NUM)bindPtr->Value.Modifier)*sizeof(BOLE);
						break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						RSize = ((NUM)bindPtr->Value.Modifier)*sizeof(REAL);
						break;

					case TYUNUM:
					case TYNUM:
					case TYCHARPOINTER:
					case TYFLOATPOINTER:
					case TYREALPOINTER:
					case TYJUMPPOINTER:
					case TYINTPOINTER:
					case TYSHORTPOINTER:
					case TYLONGPOINTER:
					case TYWORDPOINTER:
						RSize = ((NUM)bindPtr->Value.Modifier)*sizeof(NUM);
						break;

					case TYFLOAT:
						RSize = ((NUM)bindPtr->Value.Modifier)*sizeof(FLOAT);
						break;

					case TYOBJ:
						RSize = ((NUM)bindPtr->Value.Modifier)*sizeof(OBJ);
						break;

					case TYSHORT:
						RSize = ((NUM)bindPtr->Value.Modifier)*sizeof(SHORT);
						break;

					case TYLONG:
						RSize = ((NUM)bindPtr->Value.Modifier)*sizeof(LONG);
						break;

					case TYTVAL:
						RSize = ((NUM)bindPtr->Value.Modifier)*sizeof(TVAL);
						break;
					}
                goto MemoryCompare;
				break; 

           default:
                if (argv[0].Tag < argv[1].Tag)
					{
                    FrameExit(rLOW);
					}
                else
					{
                    FrameExit(rHIGH);
					}
                break;
            }
        break;

   case TYBRICKROW:
	    rp = (TBrick*)_TObject_MainObjectList(ObjIdx(argv[0]));
 	    LPtr = (char *)((char*)*rp->itsFieldArray)+((RowIdx(argv[0]))*rp->itsRowByteCount);		    
        LSize = rp->itsRowByteCount;

        switch (argv[1].Tag)
            {
			case TYBRICKROW:
				rp = (TBrick*)_TObject_MainObjectList(ObjIdx(argv[1]));
				RPtr = (char *)((char*)*rp->itsFieldArray)+((RowIdx(argv[1]))*rp->itsRowByteCount);
				RSize = rp->itsRowByteCount;		    
                goto MemoryCompare;
				break; 

           default:
                if (argv[0].Tag < argv[1].Tag)
					{
                    FrameExit(rLOW);
					}
                else
					{
                    FrameExit(rHIGH);
					}
                break;
            }
        break;

    case TYBRICK:
        switch (argv[1].Tag)
            {
            case TYBRICK:
				if ((asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
					{
        			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
        			bAp = (TBrick*)(asObject(&argv[0]));
        			bBp = (TBrick*)(asObject(&argv[1]));

        			// Compare field list
        			prmv[0] = bAp->itsFieldList;
        			prmv[1] = bBp->itsFieldList;
        			*result = FPredicate2_Compare(gCP,gTP,2,&prmv[0]);
        			if ((asTag(result) == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
        			ExitOnError(*result);
        			if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);

        			// Get no. of rows
        			sizeOf = min(bAp->itsRowCount, bBp->itsRowCount);
					indexOf = 0;
					// Get no. of columns
					fldSizeOf = bAp->itsFieldCount;

					while (indexOf < sizeOf)
						{
						fldIndexOf = 0;
						LPtr = asFieldArray(bAp) + (indexOf * bAp->itsRowByteCount);
						RPtr = asFieldArray(bBp) + (indexOf * bBp->itsRowByteCount);

						while (fldIndexOf < fldSizeOf)
							{
							// Get field information
							fldPtr = &BindArray(bAp->itsFieldList)[fldIndexOf];
							// Get no. of repeats
							repSizeOf = fldPtr->Value.Modifier;

							// Special case for Character fields
							if (fldPtr->Value.DeclaredType == TYCHAR)
								{
								if (repSizeOf > 1)
									{
			            			asTag(&prmv[0]) = TYSTRING;
			            			prmv[0].u.String = TString_SubString_MakeUnique(gCP,gTP,(LpCHAR)(LPtr+fldPtr->Value.Offset),0,fldPtr->Value.Modifier);
			            			asTag(&prmv[1]) = TYSTRING;
			            			prmv[1].u.String = TString_SubString_MakeUnique(gCP,gTP,(LpCHAR)(RPtr+fldPtr->Value.Offset),0,fldPtr->Value.Modifier);
									}
								else
									{
			            			asTag(&prmv[0]) = TYCHAR;
			            			asChar(&prmv[0]) = ((LpCHAR)(LPtr + fldPtr->Value.Offset))[0];
			            			asTag(&prmv[1]) = TYCHAR;
			            			asChar(&prmv[1]) = ((LpCHAR)(RPtr + fldPtr->Value.Offset))[0];
									}

								*result = FPredicate2_Compare(gCP,gTP,2,&prmv[0]);
		            			if ((asTag(result) == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
		            			ExitOnError(*result);
		            			if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
								}
							else
								{
			        			repIndexOf = 0;
								while (repIndexOf < repSizeOf)
									{
									switch(fldPtr->Value.DeclaredType)
										{
										case TYBOLE:
											asTag(&prmv[0]) = TYBOLE;
											asBool(&prmv[0]) = ((LpCHAR)(LPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[1]) = TYBOLE;
											asBool(&prmv[1]) = ((LpCHAR)(RPtr + fldPtr->Value.Offset))[repIndexOf];
											break;

										case TYDATE:
											asTag(&prmv[0]) = TYDATE;
											asReal(&prmv[0]) = ((LpREAL)(LPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[1]) = TYDATE;
											asReal(&prmv[1]) = ((LpREAL)(RPtr + fldPtr->Value.Offset))[repIndexOf];
											break;

										case TYUNUM:
											asTag(&prmv[0]) = TYUNUM;
											asUInt(&prmv[0]) = ((LpUNUM)(LPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[1]) = TYUNUM;
											asUInt(&prmv[1]) = ((LpUNUM)(RPtr + fldPtr->Value.Offset))[repIndexOf];
											break;

										case TYNUM:
										case TYCHARPOINTER:
										case TYFLOATPOINTER:
										case TYREALPOINTER:
										case TYJUMPPOINTER:
										case TYINTPOINTER:
										case TYSHORTPOINTER:
										case TYLONGPOINTER:
										case TYWORDPOINTER:
											asTag(&prmv[0]) = TYNUM;
											asInt(&prmv[0]) = ((LpNUM)(LPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[1]) = TYNUM;
											asInt(&prmv[1]) = ((LpNUM)(RPtr + fldPtr->Value.Offset))[repIndexOf];
											break;

										case TYFLOAT:
											asTag(&prmv[0]) = TYREAL;
											asReal(&prmv[0]) = ((LpFLOAT)(LPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[1]) = TYREAL;
											asReal(&prmv[1]) = ((LpFLOAT)(RPtr + fldPtr->Value.Offset))[repIndexOf];
											break;

										case TYMONEY:
											asTag(&prmv[0]) = TYMONEY;
											asReal(&prmv[0]) = ((LpREAL)(LPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[1]) = TYMONEY;
											asReal(&prmv[1]) = ((LpREAL)(RPtr + fldPtr->Value.Offset))[repIndexOf];
											break;

										case TYREAL:
											asTag(&prmv[0]) = TYREAL;
											asReal(&prmv[0]) = ((LpREAL)(LPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[1]) = TYREAL;
											asReal(&prmv[1]) = ((LpREAL)(RPtr + fldPtr->Value.Offset))[repIndexOf];
											break;

										case TYOBJ:
											asObject(&prmv[0]) = ((TObject**)(LPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[0]) = (asObject(&prmv[0]) == NULL) ? TYVOID : asObject(&prmv[0])->itsObjectType;
											asObject(&prmv[1]) = ((TObject**)(RPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[1]) = (asObject(&prmv[1]) == NULL) ? TYVOID : asObject(&prmv[1])->itsObjectType;
											break;

										case TYSHORT:
											asTag(&prmv[0]) = TYNUM;
											asInt(&prmv[0]) = ((LpSHORT)(LPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[1]) = TYNUM;
											asInt(&prmv[1]) = ((LpSHORT)(RPtr + fldPtr->Value.Offset))[repIndexOf];
											break;

										case TYLONG:
											asTag(&prmv[0]) = TYNUM;
											asInt(&prmv[0]) = ((LpNUM32)(LPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[1]) = TYNUM;
											asInt(&prmv[1]) = ((LpNUM32)(RPtr + fldPtr->Value.Offset))[repIndexOf];
											break;

										case TYTVAL:
											asTag(&prmv[0]) = TYTVAL;
											prmv[0] = ((LpTVAL)(LPtr + fldPtr->Value.Offset))[repIndexOf];
											asTag(&prmv[1]) = TYTVAL;
											prmv[1] = ((LpTVAL)(RPtr + fldPtr->Value.Offset))[repIndexOf];
											break;

										default:
											FrameExit(TERROR("!Invalid Brick field!"));
										}

									*result = FPredicate2_Compare(gCP,gTP,2,&prmv[0]);
									if ((asTag(result) == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
			            			ExitOnError(*result);
			            			if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);

									// Move to next repeat
									++repIndexOf;
									} // compare repeats

								}

							// Move to next column
							++fldIndexOf;
							} // compare columns

						// Move to next row
						++indexOf;
						} // compare rows

					if (bAp->itsMaxItemIndex < bBp->itsMaxItemIndex) FrameExit(rLOW);
					if (bAp->itsMaxItemIndex > bBp->itsMaxItemIndex) FrameExit(rHIGH);

					asInt(ret) = 0;
					FrameExit(*ret);
					} // end if
				else
					{
					FrameExit(TERROR("!Invalid Brick word reference!"));
					}
		        break;
                
            default:
                if (argv[0].Tag < argv[1].Tag)
					{
                    FrameExit(rLOW);
					}
                else
					{
                    FrameExit(rHIGH);
					}
                break;
            }
        break;

    case TYVECTOR:
        /*  If the objects are both Vectors and each repeating value, the */
        /*  the Cdr, and the attributes are equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYVECTOR) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            vAp = (TVector*)(asObject(&argv[0]));
            vBp = (TVector*)(asObject(&argv[1]));
			if ((vAp->itsAttributes != NULL) && (vBp->itsAttributes != NULL))
				{
				prmv[0] = TOBJ(vAp->itsAttributes);
				prmv[1] = TOBJ(vBp->itsAttributes);
				*result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
				if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
				ExitOnError( *result);
				if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
				}
            sizeOf = (vAp->itsMaxItemIndex < vBp->itsMaxItemIndex) ? vAp->itsMaxItemIndex : vBp->itsMaxItemIndex;
            indexOf = 0;
            while (indexOf < sizeOf)
                {
                prmv[0] = atHMTval(vAp->itsTvalArray,indexOf);
                prmv[1] = atHMTval(vBp->itsTvalArray,indexOf);
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
				if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
                ExitOnError( *result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                ++indexOf;
                }
            if (vAp->itsMaxItemIndex < vBp->itsMaxItemIndex) FrameExit(rLOW);
            if (vAp->itsMaxItemIndex > vBp->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = vAp->itsCdr;
            prmv[1] = vBp->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError(*result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;
    
    case TYMATRIX:
        /*  If the objects are both Matrices and each repeating value, the */
        /*  the Cdr, and the attributes are equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYMATRIX) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            mAp = (TMatrix*)(asObject(&argv[0]));
            mBp = (TMatrix*)(asObject(&argv[1]));
            sizeOf = (mAp->itsMaxItemIndex < mBp->itsMaxItemIndex) ? mAp->itsMaxItemIndex : mBp->itsMaxItemIndex;
            indexOf = 0;
            while (indexOf < sizeOf)
                {
                prmv[0] = atHMTval(mAp->itsTvalMatrix,indexOf);
                prmv[1] = atHMTval(mBp->itsTvalMatrix,indexOf);
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
				if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
                ExitOnError(*result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                ++indexOf;
                }
            if (mAp->itsMaxItemIndex < mBp->itsMaxItemIndex) FrameExit(rLOW);
            if (mAp->itsMaxItemIndex > mBp->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = mAp->itsCdr;
            prmv[1] = mBp->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError(*result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit(*ret);
            }
    break;

    case TYMATRIXROW:
        /*  If the objects are both MatrixRows and each repeating value are equal, then return a value of true. */
        
        if (asTag(&argv[1]) == TYMATRIXROW)
            {
            if ((ObjIdx(argv[0]) == ObjIdx(argv[1])) && 
                (RowIdx(argv[0]) == RowIdx(argv[1])) &&
                (FldIdx(argv[0]) == FldIdx(argv[1])))
                FrameExit(rEQUAL);

			mAp = MatrixFromRow(argv[0]);
			mBp = MatrixFromRow(argv[1]);

            /* Get Starting Index and Size of 1st MatrixRow */
			if (FldIdx(argv[0]) >= 0)
                {
                if (mAp->itsRank == 3)
                    {
                    LSize  = mAp->itsDimensions[2];
                    LIndex = RowIdx(argv[0]) * mAp->itsDimensions[1] * mAp->itsDimensions[2] +
                             FldIdx(argv[0]) * mAp->itsDimensions[2];
                    }
                else
                    FrameExit(TERROR("!Invalid Matrix rank!"));
                }
            else
            if (RowIdx(argv[0]) >= 0)
                {
                if (mAp->itsRank == 2)
                    {
                    LSize  = mAp->itsDimensions[1];
                    LIndex = RowIdx(argv[0]) * mAp->itsDimensions[1];
                    }
                else
                if (mAp->itsRank == 3)
                    {
                    LSize  = mAp->itsDimensions[1] * mAp->itsDimensions[2];
                    LIndex = RowIdx(argv[0]) * mAp->itsDimensions[1] * mAp->itsDimensions[2];
                    }
                else
                    FrameExit(TERROR("!Invalid Matrix rank!"));
                }
            else
                FrameExit(TERROR("!Invalid MatrixRow instance!"));

            /* Get Starting Index and Size of 2nd MatrixRow */                
			if (FldIdx(argv[1]) >= 0)
                {
                if (mBp->itsRank == 3)
                    {
                    RSize = mBp->itsDimensions[2];
                    RIndex = RowIdx(argv[1]) * mBp->itsDimensions[1] * mBp->itsDimensions[2] +
                             FldIdx(argv[1]) * mBp->itsDimensions[2];
                    }
                else
                    FrameExit(TERROR("!Invalid Matrix rank!"));
                }
            else
            if (RowIdx(argv[1]) >= 0)
                {
                if (mBp->itsRank == 2)
                    {
                    RSize  = mBp->itsDimensions[1];
                    RIndex = RowIdx(argv[1]) * mBp->itsDimensions[1];
                    }
                else
                if (mBp->itsRank == 3)
                    {
                    RSize  = mBp->itsDimensions[1] * mBp->itsDimensions[2];
                    RIndex = RowIdx(argv[1]) * mBp->itsDimensions[1] * mBp->itsDimensions[2];
                    }
                else
                    FrameExit(TERROR("!Invalid Matrix rank!"));
                }
            else
                FrameExit(TERROR("!Invalid MatrixRow instance!"));

            sizeOf = min(LSize, RSize);
            indexOf = 0;
            while (indexOf < sizeOf)
                {
                prmv[0] = atHMTval(mAp->itsTvalMatrix,LIndex + indexOf);
                prmv[1] = atHMTval(mBp->itsTvalMatrix,RIndex + indexOf);
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
				if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
                ExitOnError(*result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                ++indexOf;
                }
            if (LSize < RSize) FrameExit(rLOW);
            if (LSize > RSize) FrameExit(rHIGH);

            asInt(ret) = 0;
            FrameExit(*ret);
            }
    break;
			
    case TYNUMMATRIX:
        /*  If the objects are both NumMatrices and each repeating value, the */
        /*  the Cdr, and the attributes are equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYNUMMATRIX) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            nAp = (TNumMatrix*)(asObject(&argv[0]));
            nBp = (TNumMatrix*)(asObject(&argv[1]));
            sizeOf = (nAp->itsMaxItemIndex < nBp->itsMaxItemIndex) ? nAp->itsMaxItemIndex : nBp->itsMaxItemIndex;
            indexOf = 0;
            while (indexOf < sizeOf)
                {
                prmv[0] = TREAL(atHMReal(nAp->itsRealMatrix,indexOf));
                prmv[1] = TREAL(atHMReal(nBp->itsRealMatrix,indexOf));
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError( *result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                ++indexOf;
                }
            if (nAp->itsMaxItemIndex < nBp->itsMaxItemIndex) FrameExit(rLOW);
            if (nAp->itsMaxItemIndex > nBp->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = nAp->itsCdr;
            prmv[1] = nBp->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;

    case TYNUMMATRIXROW:
        /*  If the objects are both NumMatrixRows and each repeating value are equal, then return a value of true. */
        
        if (asTag(&argv[1]) == TYNUMMATRIXROW)
            {
            if ((ObjIdx(argv[0]) == ObjIdx(argv[1])) && 
                (RowIdx(argv[0]) == RowIdx(argv[1])) &&
                (FldIdx(argv[0]) == FldIdx(argv[1])))
                FrameExit(rEQUAL);

			nAp = NumMatrixFromRow(argv[0]);
			nBp = NumMatrixFromRow(argv[1]);

            /* Get Starting Index and Size of 1st MatrixRow */
			if (FldIdx(argv[0]) >= 0)
                {
                if (nAp->itsRank == 3)
                    {
                    LSize  = nAp->itsDimensions[2];
                    LIndex = RowIdx(argv[0]) * nAp->itsDimensions[1] * nAp->itsDimensions[2] +
                             FldIdx(argv[0]) * nAp->itsDimensions[2];
                    }
                else
                    FrameExit(TERROR("!Invalid NumMatrix rank!"));
                }
            else
            if (RowIdx(argv[0]) >= 0)
                {
                if (nAp->itsRank == 2)
                    {
                    LSize  = nAp->itsDimensions[1];
                    LIndex = RowIdx(argv[0]) * nAp->itsDimensions[1];
                    }
                else
                if (nAp->itsRank == 3)
                    {
                    LSize  = nAp->itsDimensions[1] * nAp->itsDimensions[2];
                    LIndex = RowIdx(argv[0]) * nAp->itsDimensions[1] * nAp->itsDimensions[2];
                    }
                else
                    FrameExit(TERROR("!Invalid NumMatrix rank!"));
                }
            else
                FrameExit(TERROR("!Invalid NumMatrixRow instance!"));

            /* Get Starting Index and Size of 2nd MatrixRow */                
			if (FldIdx(argv[1]) >= 0)
                {
                if (nBp->itsRank == 3)
                    {
                    RSize = nBp->itsDimensions[2];
                    RIndex = RowIdx(argv[1]) * nBp->itsDimensions[1] * nBp->itsDimensions[2] +
                             FldIdx(argv[1]) * nBp->itsDimensions[2];
                    }
                else
                    FrameExit(TERROR("!Invalid NumMatrix rank!"));
                }
            else
            if (RowIdx(argv[1]) >= 0)
                {
                if (nBp->itsRank == 2)
                    {
                    RSize  = nBp->itsDimensions[1];
                    RIndex = RowIdx(argv[1]) * nBp->itsDimensions[1];
                    }
                else
                if (nBp->itsRank == 3)
                    {
                    RSize  = nBp->itsDimensions[1] * nBp->itsDimensions[2];
                    RIndex = RowIdx(argv[1]) * nBp->itsDimensions[1] * nBp->itsDimensions[2];
                    }
                else
                    FrameExit(TERROR("!Invalid NumMatrix rank!"));
                }
            else
                FrameExit(TERROR("!Invalid NumMatrixRow instance!"));

            sizeOf = min(LSize, RSize);
            indexOf = 0;
            while (indexOf < sizeOf)
                {
                prmv[0] = TREAL(atHMReal(nAp->itsRealMatrix,LIndex + indexOf));
                prmv[1] = TREAL(atHMReal(nBp->itsRealMatrix,RIndex + indexOf));
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
                ExitOnError( *result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                ++indexOf;
                }
            if (LSize < RSize) FrameExit(rLOW);
            if (LSize > RSize) FrameExit(rHIGH);

            asInt(ret) = 0;
            FrameExit(*ret);
            }
    break;

    case TYBITVECTOR:
        /*  If the objects are both BitVectors and each repeating value and the */
        /*  the Cdr is equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYBITVECTOR) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            vBitp = (TBitVector*)(asObject(&argv[0]));
            vBitt = (TBitVector*)(asObject(&argv[1]));
            sizeOf = (vBitp->itsMaxItemIndex < vBitt->itsMaxItemIndex) ? vBitp->itsMaxItemIndex : vBitt->itsMaxItemIndex;
            indexOf = 0;
            asTag(ndx1) = TYNUM;
            LPtr = (char *)CharArray(argv[0]);
            RPtr = (char *)CharArray(argv[1]);
            itemp = memcmp(LPtr,RPtr,(sizeOf+7)/8);
			if (itemp < 0) FrameExit(rLOW);
			if (itemp > 0) FrameExit(rHIGH);
            if (vBitp->itsMaxItemIndex < vBitt->itsMaxItemIndex) FrameExit(rLOW);
            if (vBitp->itsMaxItemIndex > vBitt->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = vBitp->itsCdr;
            prmv[1] = vBitt->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;

	case TYCPXVECTOR:
		//  Equal if both objects are complex vectors, each repeating value, cdr are equal.        
		if (argv[1].Tag == TYCPXVECTOR && argv[0].u.CpxVector != NULL && argv[1].u.CpxVector != NULL)
			{	
			if (argv[0].u.CpxVector == argv[1].u.CpxVector) FrameExit(rEQUAL)
			vCpxp = argv[0].u.CpxVector;
			vCpxt = argv[1].u.CpxVector;
            sizeOf = (vCpxp->itsMaxItemIndex < vCpxt->itsMaxItemIndex) ? (vCpxp->itsMaxItemIndex * 2) : (vCpxt->itsMaxItemIndex * 2);
			LPtr = (LpCHAR)RealArray(argv[0]);
			RPtr = (LpCHAR)RealArray(argv[1]);
			for (indexOf = 0; indexOf < sizeOf; ++indexOf)
				{	
				if (((LpREAL)LPtr)[indexOf] < ((LpREAL)RPtr)[indexOf]) FrameExit(rLOW)
				if (((LpREAL)LPtr)[indexOf] > ((LpREAL)RPtr)[indexOf]) FrameExit(rHIGH)
				}
			if (vCpxp->itsMaxItemIndex < vCpxt->itsMaxItemIndex) FrameExit(rLOW);
			if (vCpxp->itsMaxItemIndex > vCpxt->itsMaxItemIndex) FrameExit(rHIGH);
			prmv[0] = vCpxp->itsCdr;
			prmv[1] = vCpxt->itsCdr;
			*result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
			ExitOnError( *result);
			if (result->Tag != TYNUM || result->u.Int != 0) FrameExit(*result)
			ret->u.Int = 0;
			FrameExit(*ret)
			}
		break;
    
    case TYNUMVECTOR:
        /*  If the objects are both NumVectors and each repeating value and the */
        /*  the Cdr is equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYNUMVECTOR) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            vNump = (TNumVector*)(asObject(&argv[0]));
            vNumt = (TNumVector*)(asObject(&argv[1]));
            sizeOf = (vNump->itsMaxItemIndex < vNumt->itsMaxItemIndex) ? vNump->itsMaxItemIndex : vNumt->itsMaxItemIndex;
            indexOf = 0;
			LPtr = (LpCHAR)RealArray(argv[0]);
			RPtr = (LpCHAR)RealArray(argv[1]);
			for (indexOf = 0; indexOf < sizeOf; ++indexOf)
				{	
				if (((LpREAL)LPtr)[indexOf] < ((LpREAL)RPtr)[indexOf]) FrameExit(rLOW)
				if (((LpREAL)LPtr)[indexOf] > ((LpREAL)RPtr)[indexOf]) FrameExit(rHIGH)
				}
            if (vNump->itsMaxItemIndex < vNumt->itsMaxItemIndex) FrameExit(rLOW);
            if (vNump->itsMaxItemIndex > vNumt->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = vNump->itsCdr;
            prmv[1] = vNumt->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;
    
    case TYFLTVECTOR:
        /*  If the objects are both FltVectors and each repeating value and the */
        /*  the Cdr is equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYFLTVECTOR) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            vFltp = (TFltVector*)(asObject(&argv[0]));
            vFltt = (TFltVector*)(asObject(&argv[1]));
            sizeOf = (vFltp->itsMaxItemIndex < vFltt->itsMaxItemIndex) ? vFltp->itsMaxItemIndex : vFltt->itsMaxItemIndex;
            indexOf = 0;
			LPtr = (LpCHAR)FloatArray(argv[0]);
			RPtr = (LpCHAR)FloatArray(argv[1]);
			for (indexOf = 0; indexOf < sizeOf; ++indexOf)
				{	
				if (((LpFLOAT)LPtr)[indexOf] < ((LpFLOAT)RPtr)[indexOf]) FrameExit(rLOW)
				if (((LpFLOAT)LPtr)[indexOf] > ((LpFLOAT)RPtr)[indexOf]) FrameExit(rHIGH)
				}
            if (vFltp->itsMaxItemIndex < vFltt->itsMaxItemIndex) FrameExit(rLOW);
            if (vFltp->itsMaxItemIndex > vFltt->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = vFltp->itsCdr;
            prmv[1] = vFltt->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;
    
    case TYINTVECTOR:
        /*  If the objects are both IntVectors and each repeating value and the */
        /*  the Cdr is equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYINTVECTOR) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            vIntp = (TIntVector*)(asObject(&argv[0]));
            vIntt = (TIntVector*)(asObject(&argv[1]));
            sizeOf = (vIntp->itsMaxItemIndex < vIntt->itsMaxItemIndex) ? vIntp->itsMaxItemIndex : vIntt->itsMaxItemIndex;
            indexOf = 0;
			LPtr = (LpCHAR)IntArray(argv[0]);
			RPtr = (LpCHAR)IntArray(argv[1]);
			for (indexOf = 0; indexOf < sizeOf; ++indexOf)
				{	
				if (((LpNUM)LPtr)[indexOf] < ((LpNUM)RPtr)[indexOf]) FrameExit(rLOW)
				if (((LpNUM)LPtr)[indexOf] > ((LpNUM)RPtr)[indexOf]) FrameExit(rHIGH)
				}
            if (vIntp->itsMaxItemIndex < vIntt->itsMaxItemIndex) FrameExit(rLOW);
            if (vIntp->itsMaxItemIndex > vIntt->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = vIntp->itsCdr;
            prmv[1] = vIntt->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;
    
    case TYSHORTVECTOR:
        /*  If the objects are both ShortVectors and each repeating value and the */
        /*  the Cdr is equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYSHORTVECTOR) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            vShtp = (TShtVector*)(asObject(&argv[0]));
            vShtt = (TShtVector*)(asObject(&argv[1]));
            sizeOf = (vShtp->itsMaxItemIndex < vShtt->itsMaxItemIndex) ? vShtp->itsMaxItemIndex : vShtt->itsMaxItemIndex;
            indexOf = 0;
			LPtr = (LpCHAR)ShortArray(argv[0]);
			RPtr = (LpCHAR)ShortArray(argv[1]);
			for (indexOf = 0; indexOf < sizeOf; ++indexOf)
				{	
				if (((LpSHORT)LPtr)[indexOf] < ((LpSHORT)RPtr)[indexOf]) FrameExit(rLOW)
				if (((LpSHORT)LPtr)[indexOf] > ((LpSHORT)RPtr)[indexOf]) FrameExit(rHIGH)
				}
            if (vShtp->itsMaxItemIndex < vShtt->itsMaxItemIndex) FrameExit(rLOW);
            if (vShtp->itsMaxItemIndex > vShtt->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = vShtp->itsCdr;
            prmv[1] = vShtt->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;
    
    case TYLONGVECTOR:
        /*  If the objects are both LongVectors and each repeating		*/
        /*  value and the the Cdr is equal, then return a value of true.*/
        
        if ((asTag(&argv[1]) == TYLONGVECTOR) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            vLngp = (TLongVector*)(asObject(&argv[0]));
            vLngt = (TLongVector*)(asObject(&argv[1]));
            sizeOf = (vLngp->itsMaxItemIndex < vLngt->itsMaxItemIndex) ? vLngp->itsMaxItemIndex : vLngt->itsMaxItemIndex;
            indexOf = 0;
			LPtr = (LpCHAR)LongArray(argv[0]);
			RPtr = (LpCHAR)LongArray(argv[1]);
			for (indexOf = 0; indexOf < sizeOf; ++indexOf)
				{	
				if (((LpNUM32)LPtr)[indexOf] < ((LpNUM32)RPtr)[indexOf]) FrameExit(rLOW)
				if (((LpNUM32)LPtr)[indexOf] > ((LpNUM32)RPtr)[indexOf]) FrameExit(rHIGH)
				}
            if (vLngp->itsMaxItemIndex < vLngt->itsMaxItemIndex) FrameExit(rLOW);
            if (vLngp->itsMaxItemIndex > vLngt->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = vLngp->itsCdr;
            prmv[1] = vLngt->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;
    
    case TYPCODEVECTOR:
        /*  If the objects are both PcodeVectors and each repeating */
        /*  value is equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYPCODEVECTOR) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            vPcdp = (TPcodeVector*)(asObject(&argv[0]));
            vPcdt = (TPcodeVector*)(asObject(&argv[1]));
            sizeOf = (vPcdp->itsMaxItemIndex < vPcdt->itsMaxItemIndex) ? vPcdp->itsMaxItemIndex : vPcdt->itsMaxItemIndex;
            indexOf = 0;
			LPtr = (LpCHAR)IntArray(argv[0]);
			RPtr = (LpCHAR)IntArray(argv[1]);
			for (indexOf = 0; indexOf < sizeOf; ++indexOf)
				{	
				if (((LpNUM)LPtr)[indexOf] < ((LpNUM)RPtr)[indexOf]) FrameExit(rLOW)
				if (((LpNUM)LPtr)[indexOf] > ((LpNUM)RPtr)[indexOf]) FrameExit(rHIGH)
				}
            if (vPcdp->itsMaxItemIndex < vPcdt->itsMaxItemIndex) FrameExit(rLOW);
            if (vPcdp->itsMaxItemIndex > vPcdt->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = vPcdp->itsCdr;
            prmv[1] = vPcdt->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;
    
    case TYOBJVECTOR:
        /*  If the objects are both ObjVectors and each repeating value and the */
        /*  the Cdr is equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYOBJVECTOR) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            vObjp = (TObjVector*)(asObject(&argv[0]));
            vObjt = (TObjVector*)(asObject(&argv[1]));
            sizeOf = (vObjp->itsMaxItemIndex < vObjt->itsMaxItemIndex) ? vObjp->itsMaxItemIndex : vObjt->itsMaxItemIndex;
            indexOf = 0;
            asTag(ndx1) = TYNUM;
            while (indexOf < sizeOf)
                {
                asInt(ndx1) = indexOf;
                prmv[0] = (*_TObject_TypeGetIV1(asTag(&argv[0])))(gCP,gTP, argv[0], *ndx1);
                prmv[1] = (*_TObject_TypeGetIV1(asTag(&argv[1])))(gCP,gTP, argv[1], *ndx1);
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
				if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
                ExitOnError( *result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                ++indexOf;
                }
            if (vObjp->itsMaxItemIndex < vObjt->itsMaxItemIndex) FrameExit(rLOW);
            if (vObjp->itsMaxItemIndex > vObjt->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = vObjp->itsCdr;
            prmv[1] = vObjt->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;
    
    case TYSTRUCTURE:
        /*  If the objects are both Structures and each binding key and value and the */
        /*  the Cdr is equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYSTRUCTURE) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            eAp = (TStructure*)(asObject(&argv[0]));
            eBp = (TStructure*)(asObject(&argv[1]));
            sizeOf = (eAp->itsMaxItemIndex < eBp->itsMaxItemIndex) ? eAp->itsMaxItemIndex : eBp->itsMaxItemIndex;
            indexOf = 0;
            while (indexOf < sizeOf)
                {
                prmv[0] = TOBJ(atHMBind(eAp->itsDictionaryArray,indexOf).Key);
                prmv[1] = TOBJ(atHMBind(eBp->itsDictionaryArray,indexOf).Key);
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
				if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
                ExitOnError( *result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                prmv[0] = atHMBind(eAp->itsDictionaryArray,indexOf).Value;
                prmv[1] = atHMBind(eBp->itsDictionaryArray,indexOf).Value;
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
				if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
                ExitOnError( *result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                ++indexOf;
                }
            if (eAp->itsMaxItemIndex < eBp->itsMaxItemIndex) FrameExit(rLOW);
            if (eAp->itsMaxItemIndex > eBp->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = eAp->itsCdr;
            prmv[1] = eBp->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;
    
    case TYDICTIONARY:
        /*  If the objects are both Dictionarys and each binding key and value, */
        /*  then return a value of true. */
        
        if ((asTag(&argv[1]) == TYDICTIONARY) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
 			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            dAp = (TDictionary*)(asObject(&argv[0]));
            dBp = (TDictionary*)(asObject(&argv[1]));
            sizeOf = (dAp->itsMaxItemIndex < dBp->itsMaxItemIndex) ? dAp->itsMaxItemIndex : dBp->itsMaxItemIndex;
            indexOf = 0;
            while (indexOf < sizeOf)
                {
                prmv[0] = TOBJ(atHMBind(dAp->itsDictionaryArray,indexOf).Key);
                prmv[1] = TOBJ(atHMBind(dBp->itsDictionaryArray,indexOf).Key);
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
				if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
                ExitOnError( *result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                prmv[0] = atHMBind(dAp->itsDictionaryArray,indexOf).Value;
                prmv[1] = atHMBind(dBp->itsDictionaryArray,indexOf).Value;
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
				if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
                ExitOnError( *result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                ++indexOf;
                }
            if (dAp->itsMaxItemIndex < dBp->itsMaxItemIndex) FrameExit(rLOW);
            if (dAp->itsMaxItemIndex > dBp->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = dAp->itsCdr;
            prmv[1] = dBp->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit(*ret);
            }
    break;
    
    case TYDIRECTORY:
        /*  If the objects are both Directorys and each binding key and value, */
        /*  are equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYDIRECTORY) && (asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            xAp = (TDirectory*)(asObject(&argv[0]));
            xBp = (TDirectory*)(asObject(&argv[1]));
            sizeOf = (xAp->itsMaxItemIndex < xBp->itsMaxItemIndex) ? xAp->itsMaxItemIndex : xBp->itsMaxItemIndex;
            indexOf = 0;
            while (indexOf < sizeOf)
                {
                prmv[0] = atHMPBind(xAp->itsDirectoryArray,indexOf).Key;
                prmv[1] = atHMPBind(xBp->itsDirectoryArray,indexOf).Key;
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
				if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
                ExitOnError( *result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                prmv[0] = atHMPBind(xAp->itsDirectoryArray,indexOf).Value;
                prmv[1] = atHMPBind(xBp->itsDirectoryArray,indexOf).Value;
                *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
				if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
                ExitOnError( *result);
                if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
                ++indexOf;
                }
            if (xAp->itsMaxItemIndex < xBp->itsMaxItemIndex) FrameExit(rLOW);
            if (xAp->itsMaxItemIndex > xBp->itsMaxItemIndex) FrameExit(rHIGH);
            prmv[0] = xAp->itsCdr;
            prmv[1] = xBp->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit(*ret);
            }
    break;
    
    case TYPAIR:
	case TYQUOTEDPAIR:

        /*  If the objects are both Pairs and each both Car and Cdr values */
        /*  are equal, then return a value of true. */
        
        if ((asTag(&argv[1]) == TYPAIR ||asTag(&argv[1]) == TYQUOTEDPAIR) && 
			(asObj(&argv[0]) != NIL) && (asObj(&argv[1]) != NIL))
            {
			if (argv[0].u.Object == argv[1].u.Object) FrameExit(rEQUAL);
            pAp = (TPair*)(asObject(&argv[0]));
            pBp = (TPair*)(asObject(&argv[1]));
            prmv[0] = pAp->itsCar;
            prmv[1] = pBp->itsCar;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            prmv[0] = pAp->itsCdr;
            prmv[1] = pBp->itsCdr;
            *result = FPredicate2_Compare(gCP,gTP,(NUM)2,&prmv[0]);
			if ((result->Tag == TYERROR) && (strcmp(CharArray(*result),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
            ExitOnError( *result);
            if ((asTag(result) != TYNUM) || (asInt(result) != 0)) FrameExit(*result);
            asInt(ret) = 0;
            FrameExit( *ret);
            }
    break;
    

    default:
		/* If the left type is not in the above list, then we either */
		/* */
		goto CompareDefault;
		break;   
	}

CompareDefault:
/* Is the left type lower than the right type ?*/
if (argv[0].Tag < argv[1].Tag)
    {
    FrameExit(rLOW);
    }

/* Is the left type higher than the right type ?*/
if (argv[0].Tag > argv[1].Tag)
    {
    FrameExit(rHIGH);
    }

/* Since both left and right types are equal, */
/* we have an undefined condition if we get here? */

if ((_TObject_TypeFlag(argv[0].Tag) == _TObject_TfTOBJECT) &&
	(_TObject_TypeFlag(argv[1].Tag) == _TObject_TfTOBJECT) &&
	(argv[0].u.Obj == argv[1].u.Obj))
    {
    FrameExit(rEQUAL);
    }

/* Compare both left and right values according */
/* to their registered comparison functions. */
aFunction = (LpF2TVALS)_TObject_TypeCompare(argv[0].Tag);
*ret = ((*aFunction)(gCP, gTP, argv[0], argv[1]));
if ((ret->Tag == TYERROR) && (strcmp(CharArray(*ret),"!compare: too many recursions!") == 0)) FrameExit(rEQUAL);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_FullIdentical

Compares any two SmartLisp objects to determine if they are identical.

#endif

TVAL FPredicate2_FullIdentical(LpXCONTEXT gCP,LpTHREAD gTP,TVAL left,TVAL right)  
{
TVAL			argv[2];
TVAL			compareCode;

argv[0] = left;
argv[1] = right;
compareCode = FPredicate2_Identicalp(gCP,gTP,2,&argv[0]);
return(compareCode);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_Identicalp

Compares any two SmartLisp objects to determine if they are identical.
Native objects are identical if they are equal.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate2_Identicalp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])  
{
REAL				rtemp;
NUM					itemp;
NUM					MSize;
NUM					LSize;
NUM					RSize;
NUM					LenCmp;
LpCHAR				LPtr;
LpCHAR				RPtr;
StartFrame
DeclareOBJ(TVector,vAp);
DeclareOBJ(TVector,vBp);
DeclareOBJ(TBitVector,vBitp);
DeclareOBJ(TBitVector,vBitt);
DeclareOBJ(TByteVector,vBytep);
DeclareOBJ(TByteVector,vBytet);
DeclareOBJ(TNumVector,vNump);
DeclareOBJ(TNumVector,vNumt);
DeclareOBJ(TFltVector,vFltp);
DeclareOBJ(TFltVector,vFltt);
DeclareOBJ(TIntVector,vIntp);
DeclareOBJ(TIntVector,vIntt);
DeclareOBJ(TObjVector,vObjp);
DeclareOBJ(TObjVector,vObjt);
DeclareOBJ(TStructure,eAp);
DeclareOBJ(TStructure,eBp);
DeclareOBJ(TDictionary,dAp);
DeclareOBJ(TDictionary,dBp);
DeclareOBJ(TDirectory,xAp);
DeclareOBJ(TDirectory,xBp);
DeclareOBJ(TPair,pAp);
DeclareOBJ(TPair,pBp);
DeclareOBJ(TCpx, cAp);
DeclareOBJ(TCpx, cBp);
EndFrame

/*  Initialization */

if (argc != 2) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);


/*  Perform a surface comparison on both objects. */

switch(argv[0].Tag)
    {
    case TYVOID:
        if (argv[0].Tag != argv[1].Tag)
			{
            FrameExit(gCP->Tval_FALSE);
			}
        else
			{
            FrameExit(gCP->Tval_TRUE);
			}
        break;
    
    case TYSTRING:
    case TYERROR:
        LPtr = (char *)CharArray(argv[0]);
        LSize = strlen(LPtr);

        RightCompare:

        switch (argv[1].Tag)
            {
            case TYTEXT:
                RPtr = (char *)&argv[1].u.Text[0];
                RSize = strlen(RPtr);
                goto MemoryCompare;
                break;

            case TYSTRINGSUBSTR:
                RPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[1]);
                if (RPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
                RSize = SubLen(argv[1]);
                goto MemoryCompare;
                break;

            case TYERROR:
            case TYSTRING:
                RPtr = (char *)CharArray(argv[1]);
                RSize = strlen(RPtr);

                MemoryCompare:
                
				LenCmp = LSize - RSize;
                MSize = (LSize < RSize) ? LSize : RSize;
                itemp = memcmp(LPtr,RPtr,MSize);
                FrameExit((itemp != 0 ? gCP->Tval_FALSE : (LenCmp == 0 ? gCP->Tval_TRUE : gCP->Tval_FALSE)));
                break;
                
           case TYCHAR:
                argv[1].u.Text[1] = 0;
                RPtr = (char *)&argv[1].u.Text[0];
                RSize = 1;
                goto MemoryCompare;
                break;

            default:
                goto CompareDefault;
                break;
            }
        break;
        
    case TYTEXT:
        LPtr = (char *)&argv[0].u.Text[0];
        LSize = strlen(LPtr);
        goto RightCompare;
        break;
      
    case TYSTRINGSUBSTR:
        LPtr = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
        if (LPtr == NULL) FrameExit(gCP->TObject_ERROR_INVALID);
        LSize = SubLen(argv[0]);
        goto RightCompare;
        break;

    case TYUNUM:
        switch (argv[1].Tag)
            {
            case TYUNUM:
                rtemp = argv[0].u.UInt - argv[1].u.UInt;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                rtemp = (REAL)argv[0].u.UInt - argv[1].u.Real;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            case TYCHAR:
                rtemp = (REAL)argv[0].u.UInt - argv[1].u.Char;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            default:
                FrameExit(gCP->Tval_FALSE);
                break;
            }
        break;
        
    case TYNUM:
        switch (argv[1].Tag)
            {
            case TYNUM:
                rtemp = argv[0].u.Int - argv[1].u.Int;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                rtemp = (REAL)argv[0].u.Int - argv[1].u.Real;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            case TYCHAR:
                rtemp = (REAL)argv[0].u.Int - argv[1].u.Char;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            default:
                FrameExit(gCP->Tval_FALSE);
                break;
            }
        break;
        
    case TYDATE:
    case TYMONEY:
    case TYREAL:
        switch (argv[1].Tag)
            {
            case TYNUM:
                rtemp = argv[0].u.Real - argv[1].u.Int;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                rtemp = argv[0].u.Real - argv[1].u.Real;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            case TYCHAR:
                rtemp = argv[0].u.Real - argv[1].u.Char;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            default:
                FrameExit(gCP->Tval_FALSE);
                break;
            }
        break;
        
    case TYCHAR:
        switch (argv[1].Tag)
            {
            case TYNUM:
                rtemp = argv[0].u.Char - argv[1].u.Int;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                rtemp = argv[0].u.Char - argv[1].u.Real;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            case TYCHAR:
                rtemp = argv[0].u.Char - argv[1].u.Char;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
            
            default:
                FrameExit(gCP->Tval_FALSE);
                break;
            }
        break;
        
    case TYBOLE:
        switch (argv[1].Tag)
            {
            case TYBOLE:
                rtemp = argv[0].u.Bool - argv[1].u.Bool;
                FrameExit(rtemp ? gCP->Tval_FALSE : gCP->Tval_TRUE);
                break;
                
            default:
                FrameExit(gCP->Tval_FALSE);
                break;
            }
        break;
	case TYCPX:
		switch (argv[1].Tag)
			{
			case TYCPX:
                cAp = argv[0].u.Complex;
				cBp = argv[1].u.Complex;
				itemp = cAp->itsReal == cBp->itsReal && cAp->itsImag == cBp->itsImag;
                FrameExit(itemp ? gCP->Tval_TRUE : gCP->Tval_FALSE);
                break;
                
            default:
                FrameExit(gCP->Tval_FALSE);
                break;
            }
        break;
    default:
		/* If the left type is not in the above list, then we either */
		/* */
		goto CompareDefault;
		break;   
	}

CompareDefault:
/* Is the left type lower than the right type ?*/
if (argv[0].Tag != argv[1].Tag)
    {
    FrameExit(gCP->Tval_FALSE);
    }

/* Since both left and right types are equal, */
/* we have an undefined condition if we get here? */

if ((_TObject_TypeFlag(argv[0].Tag) == _TObject_TfTOBJECT) &&
	(_TObject_TypeFlag(argv[1].Tag) == _TObject_TfTOBJECT) &&
	(argv[0].u.Obj == argv[1].u.Obj))
    {
    FrameExit(gCP->Tval_TRUE);
    }

FrameExit(gCP->Tval_FALSE);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_Memeqv

The FPredicate2_Memeqv function determines whether or not the specified object {key} 
is a member of the {target}. The target may be a list, the Structure, 
 or a Vector.

In comparing the {key} with the elements of {target}, the FPredicate2_Memeqv function 
uses isEqv.

If {key} is found, true is returned, else false is returned

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate2_Memeqv(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ec);
EndFrame
 
/*  Initialization */

asTag(ec) = TYBOLE;

/* Structures, Dictionaries, and Directories have an optional 3rd argument */
if (argc == 3)
	{
	if (argv[1].Tag == TYSTRUCTURE  ||
		argv[1].Tag == TYDICTIONARY   ||
		argv[1].Tag == TYDIRECTORY )
		{
		if (argv[2].Tag != TYNUM)
			{
			FrameExit(*ec);
			}
		}
	else
		{
		FrameExit(*ec);
		}
	}
else
	{
	if (argc != 2)
		{
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
	}

*ec =  FUtil3_Member(gCP,gTP,argc, argv);
if (ec->Tag == TYBOLE )
	{
	asBool(ec) = FALSE;
    }
else
if (ec->Tag == TYNUM)
	{ 
	asTag(ec) = TYBOLE;
	asBool(ec) = TRUE;
	}

FrameExit(*ec);
	

}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_IsInside

The FPredicate2_IsInside function determines whether or not the specified object {key} 
is a member of the {target}. The target may be a list, the Structure, 
 or a Vector.

In comparing the {key} with the elements of {target}, the FPredicate2_IsInside function 
uses isEqv.

If {key} is found, true is returned, else false is returned

Note:   Exactly two arguments are expected, anything else is an error.
		The isInside function uses the isIdentical predicate.

#endif

TVAL FPredicate2_IsInside(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])      
{
StartFrame
DeclareTVAL(ec);
EndFrame
 
/*  Initialization */

asTag(ec) = TYBOLE;

/* Structures, Dictionaries, and Directories have an optional 3rd argument */
if (argc == 3)
	{
	if (argv[1].Tag == TYSTRUCTURE  ||
		argv[1].Tag == TYDICTIONARY   ||
		argv[1].Tag == TYDIRECTORY )
		{
		if (argv[2].Tag != TYNUM)
			{
			FrameExit(*ec);
			}
		}
	else
		{
		FrameExit(*ec);
		}
	}
else
	{
	if (argc != 2)
		{
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		}
	}

*ec =  FUtil3_Inside(gCP,gTP,argc, argv);
if (ec->Tag == TYBOLE )
	{
	asBool(ec) = FALSE;
    }
else
if (ec->Tag == TYNUM)
	{ 
	asTag(ec) = TYBOLE;
	asBool(ec) = TRUE;
	}

FrameExit(*ec);
	

}
/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_CompareLT

Compares any two SmartLisp objects to determine if they are less than.
Native objects are equivalent if they are equal after conversion. Compound
objects are equivalent if their components are equivalent.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate2_CompareLT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])    
{
TVAL        ret;

ret = FPredicate2_Compare(gCP,gTP,argc,argv);
if (ret.Tag != TYNUM)
    return(ret);
else
if (ret.u.Int < 0)
    return(gCP->Tval_TRUE);
else
    return(gCP->Tval_FALSE);

}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_CompareLE

Compares any two SmartLisp objects to determine if they are less than or equivalent.
Native objects are equivalent if they are equal after conversion. Compound
objects are equivalent if their components are equivalent.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate2_CompareLE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])    
{
StartFrame
DeclareTVAL(ret);
EndFrame

*ret = FPredicate2_Compare(gCP,gTP,argc,argv);

if (ret->Tag != TYNUM)
	{
    FrameExit(*ret);
	}
else
if (ret->u.Int <= 0)
	{
    FrameExit(gCP->Tval_TRUE);
	}
else
    FrameExit(gCP->Tval_FALSE);

}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_Equal

Compares any two SmartLisp objects to determine if they are equivalent.
Native objects are equivalent if they are equal after conversion. Compound
objects are equivalent if their components are equivalent.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate2_Equal(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])    
{
StartFrame
DeclareTVAL(ret);
EndFrame

*ret = FPredicate2_Compare(gCP,gTP,argc,argv);
if (ret->Tag != TYNUM)
	{
    FrameExit(*ret);
	}
else
if (ret->u.Int == 0)
	{
    FrameExit(gCP->Tval_TRUE);
	}
else
    FrameExit(gCP->Tval_FALSE);

}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_CompareNE

Compares any two SmartLisp objects to determine if they are not equivalent.
Native objects are equivalent if they are equal after conversion. Compound
objects are equivalent if their components are equivalent.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate2_CompareNE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])    
{
StartFrame
DeclareTVAL(ret);
EndFrame

*ret = FPredicate2_Compare(gCP,gTP,argc,argv);

if (ret->Tag != TYNUM)
	{
    FrameExit(*ret);
	}
else
if (ret->u.Int != 0)
	{
    FrameExit(gCP->Tval_TRUE);
	}
else
    FrameExit(gCP->Tval_FALSE);

}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_CompareGE

Compares any two SmartLisp objects to determine if they are greater than or equivalent.
Native objects are equivalent if they are equal after conversion. Compound
objects are equivalent if their components are equivalent.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate2_CompareGE(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])    
{
StartFrame
DeclareTVAL(ret);
EndFrame

*ret = FPredicate2_Compare(gCP,gTP,argc,argv);
if (ret->Tag != TYNUM)
	{
    FrameExit(*ret);
	}
else
if (ret->u.Int >= 0)
	{
    FrameExit(gCP->Tval_TRUE);
	}
else
    FrameExit(gCP->Tval_FALSE);

}

/*--------------------------------------------------------------------------------------- */
#if 0

FPredicate2_CompareGT

Compares any two SmartLisp objects to determine if they are greater than.
Native objects are equivalent if they are equal after conversion. Compound
objects are equivalent if their components are equivalent.

Note:   Exactly two arguments are expected, anything else is an error.

#endif

TVAL FPredicate2_CompareGT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])    
{
StartFrame
DeclareTVAL(ret);
EndFrame

*ret = FPredicate2_Compare(gCP,gTP,argc,argv);

if (ret->Tag != TYNUM)
	{
    FrameExit(*ret);
	}
else
if (ret->u.Int > 0)
	{
    FrameExit(gCP->Tval_TRUE);
	}
else
    FrameExit(gCP->Tval_FALSE);

}
