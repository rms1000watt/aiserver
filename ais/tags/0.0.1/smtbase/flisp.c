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
FLisp.c

This file contains some of the procedures required to support the SmartLisp "lisp" and "morph"
commands. These two procedures make up the preprocessing phase of the compiler.

PARENT:              

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "flisp.h"
#include "fsmtbase.h"
#include "tpair.h"
#include "futil3.h"
#include "tvector.h"
#include "tstruct.h"
#include "tlambda.h"
#include "tobjvec.h"
#include "tbytevec.h"
#include "fproc.h"
#include "fmath1.h"
#include "fmacro.h"
#include "fconvert.h"
#include "futil2.h"
#include "fmake.h"
#include "fdebug.h"
#include "fsforms3.h"
#include "fcompile.h"
#include "tdiction.h"
#include "tdirect.h"
#include "tbrick.h"
#include "tcpx.h"
#include "terror.h"

/*--------------------------------------------------------------------------------------- */
#if 0

FLisp_Init

Initialize a part of the Lisp parser portion of the SmartLisp function library.  

#endif

TVAL FLisp_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TSymbol,alsoSymbol);
DeclareTVAL(ec);
EndFrame
 
if (gCP->FLisp_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FLisp_Initialized = 1;

/* Register the SmartLisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"lisp",(LpFUNC)&FLisp_Lisp);
ExitOnError(*ec);
TSymbol_SetGlobalValue(gCP,gTP,TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_parser"), aSymbol->itsGlobalValue);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"morph",(LpFUNC)&FLisp_Morph);
ExitOnError(*ec);
 
FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FLisp_Morph

The FLisp_Morph function attempts a morphological transformation on its input.
The first argument is the list to be morphologically transformed (mandatory). 
The second argument is the user supplied rule Lambda (optional). 
The third argument is the user supplied failure result (optional)(default false). 

#endif

TVAL FLisp_Morph(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])        
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(altRules);
DeclareTVAL(altFailure);
EndFrame
 
/*  Make sure there are no more than three arguments. */

*ret = gCP->Tval_VOID;
if ((argc > 3) )    
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  The only morphable input is a list. */
/*  All other inputs (such as singletons) */
/*  must be returned as is. */

if (asTag(&argv[0]) != TYPAIR)
    {   
    FrameExit(argv[0]);
    }

/*  The user may supply the morph rules Lambda. */

if (argc >= 2)  
    *altRules = argv[1];
else
    *altRules = gCP->Tval_VOID;
    
/*  The user may supply the morph rules failure result. */

if (argc == 3)  
    *altFailure = argv[2];
else
    *altFailure = gCP->Tval_FALSE;
    
*ret = FLisp_MorphList(gCP, gTP,argv[0],*altRules,*altFailure);
    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
HCompileMe

The FLisp_HCompileMe method does preprocessing for compilation of SmartLisp source
and calls the appropriate compiler.

#endif

TVAL FLisp_HCompileMe(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory hEncode)
{
StartFrame
DeclareTVAL(lex);
DeclareTVAL(ret);
EndFrame

selfTval = selfTval; // NOOP to hide unused parameter warning message
if(strlen((char*)*hEncode))
    {
    /* Lex and morph the input buffer */
	/* Initialize the first argument as Void object */
    *lex = FLisp_Parser(gCP, gTP, 0, &gCP->Tval_VOID, (HMChar)hEncode, NIL);
    ExitOnError( *lex);

	*ret = FCompile_Compile(gCP,gTP,1, lex);
    FrameExit( *ret);
    }
            
FrameExit(gCP->TObject_VOID);
}
/*--------------------------------------------------------------------------------------- */
/*

FLisp_Lisp

(gCP,gTP,lisp  inputString)
(lisp  inputString  Lambda)
(lisp  inputString  extended:)
(lisp  inputString  Lambda  extended:)

The lisp Function performs normal SmartBase lexical analysis on the 
inputString argument. The input argument must be a string containing 
a valid SmartLisp simple or compound expression. The output of the lisp 
function is a SmartBase generic parse tree containing Lisp lexical and 
symantic tokens. Normal SmartLisp strings are passed to lisp, whose output
is passed to morph, whose output is passed to compile. The end result is a 
SmartBase Lambda ready to perform its assigned tasks.

This three step process allows the use of alternate lexical analyzers, and 
alternate macro preprocessors with the SmartBase optimizing compiler.

If the optional  Lambda  argument is passed, the input source string is 
converted into a Token Vector and stored in the Sc field of the Lambda 
(see the decompile Function). This allows the original source to be stored 
with the Lambda after compilation. For instance, this technique is used 
inside the Spreadsheet tool during compilation of Spreadsheet formulas, 
which often need to be decompiled and displayed in the Spreadsheets edit 
box.
 
If the optional  extended:  argument is passed, the normal Lisp lexical 
analysis is altered as follows:

    The lexical symbol   is no longer a lexical quote operator, and 
        becomes a stand alone operator symbol.

    The lexical symbols    ,  [  ]  {  }  ;  .  become singleton 
        (ungrouped) stand alone operator symbols.

    The lexical symbols  !  @  #  $  %  ^  &  *  -  =  +  |  \  :  <  >  ?  /  
        become clustered (grouped) operator symbols.

    The lexical symbols  (  )  still create sub lists as in normal Lisp.

    The lexical constants for Money, Integer, Number, String, and Error 
        are still recognized as in normal Lisp.

    The lexical Lisp names are now terminated when a grouped or ungrouped 
        operator symbol is encountered.

    These changes, to normal Lisp lexical analysis, allow Lambdas and end 
        users to use the lisp function as the lexical analyzer for a 
        variety of languages which would share a lexical similarity to 
        SmartLisp even though their symantics were quite different.

Arguments
	
inputString	    A string containing a valid SmartLisp simple or compound 
                    expression.

Lambda	        An Lambda object whose Sc field will contain the tokenized 
                    input string after lexical analysis

extended:	    Converts the lisp function into a more general lexical 
                    analyzer for other languages.
	
For example:

	(define foo (makeLambda))	=>	#<Lambda 492>
	(lisp  "(+ 1 2)")			=>	((+ 1 2))
	(lisp  "1")			        =>	(1)
	(lisp  "'(++ x)" foo)		=>	((++ x))
	(decompile  foo)		    =>	"(++ x)"
	(lisp  "x.y[1]")			=>	((ref  (ref x 'y)  1))
	(lisp  "x.y.z")			    =>	((ref  (ref x 'y)  'z))
	(lisp  "x[y[z[0]]]")		=>	((ref  x (ref y  (ref  z  0))))
	(lisp  "x[0][1]")			=>	((ref  (ref  x  0) 1))
	(lisp  "x[0  1]")			=>	((ref  x  0  1))

Note:	Normal SmartBase strings are passed to lisp, whose output is 
            passed to morph, whose output is passed to compile. This 
            allows the use of alternate lexical analyzers, and alternate 
            macro preprocessors with the SmartBase optimizing compiler. 
            For instance, use of the optional  extended:  argument 
            causes significant alterations in the normal Lisp lexical 
            analysis process as follows:

	(lisp  "1 + 2")			    =>	(1  +  2)
	(lisp  "1+2")			    =>	(1+2)
	(lisp  "1+2" extended:)     =>	(1 + 2)
	(lisp  "x.y[1]")			=>	((ref  (ref x 'y)  1))
	(lisp  "x.y[1]" extended:)  =>	(x  .  y  [  1  ])
	(lisp  "x=>y")			    =>	(x=>y)
	(lisp  "x=>y" extended:)	=>	(x  =>  y)

Note:	Normal SmartBase strings are passed to lisp, whose output is 
            passed to morph, whose output is passed to compile. This 
            allows the use of alternate lexical analyzers, and alternate 
            macro preprocessors with the SmartBase optimizing compiler. 
            For instance, use of the optional  arithmetic:  argument 
            causes normal Lisp names to be separated by the arithmetic 
            operators as follows:

	(lisp  "1 + 2")					=>	(1  +  2)
	(lisp  "1+2")					=>	(1+2)
	(lisp  "1+2" arithmetic:)		=>	(1 + 2)
	(lisp  "x.y[1]")				=>	((ref  (ref x 'y)  1))
	(lisp  "x.y[1]+n" arithmetic:)	=>	((ref  (ref x 'y)  1) + n)
	(lisp  "x=>y")					=>	(x=>y)
	(lisp  "x=>y" arithmetic:)		=>	(x  =>  y)

Note:	Strong typing should be added to the Lisp language with Av, Rv, Tv, Pv, and Cv declarations 
		as follows:

		vars:(type:name (type:name constant))

*/

TVAL FLisp_Lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])       
{
HMChar              hmChar;
LpCHAR              theSource;
NUM                 lexargc;
NUM                 nLen;
NUM	                extendedLex = NIL;
StartFrame
DeclareOBJ(TLambda,proc);
DeclareOBJ(TString,aTString);
DeclareOBJ(TByteVector,aBVector);
DeclareTVAL(ret);
DeclareTVALArray(prmv,3);
EndFrame
 

/*      Argument validation  and setup */

if(argc >= 1 && argc <= 3 && !isNullTval(&argv[0]))
    {
    proc = (gTP->TLambda_CurrentProcedure ? gTP->TLambda_CurrentProcedure : gTP->TLambda_TheProc);
    lexargc = 0;

    if ((argc == 2) && (argv[1].Tag == TYLAMBDA))
        {
        prmv[0] = argv[1];
        lexargc = 1;
        }
    else
    if ((argc == 2) && 
        (argv[1].Tag == TYSYMBOL) &&
        (strcmp(SymbolArray(argv[1]),"extended") == 0))
        {
        extendedLex = 1;
        }
    else
    if ((argc == 2) && 
        (argv[1].Tag == TYSYMBOL) &&
        (strcmp(SymbolArray(argv[1]),"arithmetic") == 0))
        {
        extendedLex = 2;
        }
    else
    if ((argc == 3) && 
        (argv[1].Tag == TYLAMBDA) && 
        (argv[2].Tag == TYSYMBOL) &&
        (strcmp(SymbolArray(argv[2]),"extended") == 0))
        {
        extendedLex = 1;
        prmv[0] = argv[1];
        lexargc = 1;
        }
    else
    if ((argc == 3) && 
        (argv[1].Tag == TYLAMBDA) && 
        (argv[2].Tag == TYSYMBOL) &&
        (strcmp(SymbolArray(argv[2]),"arithmetic") == 0))
        {
        extendedLex = 2;
        prmv[0] = argv[1];
        lexargc = 1;
        }
    else
    if (argc != 1)
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    

if (asTag(&argv[0]) == TYTEXT)
    {
    theSource = &asText(&argv[0])[0];
    nLen = strlen((char*)theSource);
    hmChar = (HMChar)FMemory_New(gCP, gTP, nLen + 1,TRUE);
    if (hmChar != NULL)
        {
        strcpy((char *)&atHMChar(hmChar, 0), (char *)theSource);
		gTP->FCompile_DebugSourceVector = NIL;
	    gTP->FCompile_DebugListLines = NIL;
        *ret = FLisp_Parser(gCP, gTP, lexargc, &prmv[0], hmChar, extendedLex);
        FMemory_Free(gCP, gTP, (HMemory)hmChar);
        }
    else 
        *ret = gCP->TObject_ERROR_OUT_OF_MEMORY;
    }
else
if (asTag(&argv[0]) == TYSTRINGSUBSTR)
    {
    theSource = TStringSubstringT_GetStringPtr(gCP, gTP, argv[0]);
    nLen = SubLen(argv[0]);
    hmChar = (HMChar)FMemory_New(gCP, gTP, nLen + 1,TRUE);
    if (hmChar != NULL)
        {
        strncpy((char *)&atHMChar(hmChar, 0), (char *)theSource, nLen);
		gTP->FCompile_DebugSourceVector = NIL;
	    gTP->FCompile_DebugListLines = NIL;
        *ret = FLisp_Parser(gCP, gTP, lexargc, &prmv[0], hmChar, extendedLex);
        FMemory_Free(gCP, gTP, (HMemory)hmChar);
        }
    else 
        *ret = gCP->TObject_ERROR_OUT_OF_MEMORY;
    }
else
if (asTag(&argv[0]) == TYSTRING)
    {
    aTString = (TString*)asObject(&argv[0]);
    *ret = FLisp_Parser(gCP, gTP, lexargc, &prmv[0], aTString->itsCString, extendedLex);
    }
else
if (asTag(&argv[0]) == TYBYTEVECTOR )
    {
    aBVector = (TByteVector*)asObject(&argv[0]);
    *ret = FLisp_Parser(gCP, gTP, lexargc, &prmv[0], aBVector->itsByteArray, extendedLex);
    }
else
    goto Invalid;
    
FrameExit(*ret);

Invalid:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}


/*--------------------------------------------------------------------------------------- */
#if 0
CompileMe

The FLisp_CompileMe method copies the passed string into a handle and calls the HCompileMe
method to do the actual encoding.

#endif

TVAL FLisp_CompileMe(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpCHAR theSource)
{
HMemory             theData;
NUM                 nLen;
StartFrame
DeclareTVAL(ret);
EndFrame

nLen = strlen((char*)theSource);
theData = FMemory_New(gCP, gTP, nLen + 1,TRUE);
strcpy((char*)*theData, (const char*)theSource);
*ret =  FLisp_HCompileMe(gCP, gTP, selfTval, theData);
FMemory_Free(gCP, gTP, theData);

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FLisp_Parser

The FLisp_Parser procedure is the SmartBase parser for SmartLisp and generates a proper list
from an input Handle containing the text representation of a SmartLisp program.

Lisp takes a count, a pointer to a tval vector, and a handle to source data as arguments. The
tval vector may be empty or contain a second optional argument which must be a procedure object.
If the procedure object is present then as a side effect lisp will fill
in the `Sc TVAL vector in the procedure object which allows for redisplay of the original source,
and will be used for name tracking and positional formula adjustment.

#endif

TVAL FLisp_Parser(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[], HMChar hSrc, NUM extendedLex)
{
NUM                 savNdx;
NUM                 vNdx;
NUM                 hNdx;
NUM                 hLen;
NUM                 cvrNdx;
NUM                 quoteNdx;
NUM                 quoteCnt;
NUM                 stackNdx;
NUM                 anInt;
NUM                 tmpInt;

NUM                 brkNdx;    /* Brick index store */
NUM                 fldNdx;    /* Brick field index store */
NUM                 fldLen;    /* Brick field length */
BOLE                bSetValue; /* Brick set value flag */

NUM                 popQuoteCount;
NUM                 sourceLineNumber = 0;
BOLE                bAddAtom;
BOLE                bInsertBrk;
REAL                aReal;
CHAR                cvrChar[_FLisp_CVRSize];
CHAR                tmpChar;
CHAR                curChar;
NUM					stringConstMaxLen = _FSmartbase_MAXBUFFERLEN;
LpCHAR              stringBuf = gTP->TempBuffer;
LpCHAR              stringPtr;
TVAL				aNumber;
NUM					len;
NUM					dimensions[_MAXDIMENSIONS];
NUM					rank;
NUM					i;
NUM					n;
BOLE				returnLambda = FALSE;
StartFrame
DeclareTVAL(dbgParseTreeSYM);
DeclareTVAL(dbgListLinesSYM);
DeclareTVAL(dbgSourceLinesSYM);
DeclareOBJ(TStructure,dbgListLines);
DeclareOBJ(TVector,dbgSourceLines);
DeclareOBJ(TStructure,tmpEnv);
DeclareOBJ(TLambda,theProc);
DeclareOBJ(TPair,popPair);
DeclareOBJ(TPair,curPair);
DeclareOBJ(TPair,basePair);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,firstPairInSourceLine);
DeclareOBJ(TSymbol,aTSymbol);
DeclareOBJ(TSymbol,aTString);
DeclareOBJ(TObjVector,stack);
DeclareOBJ(TCpx,complex);
DeclareTVAL(argTval);
DeclareTVAL(srcVecTval);
DeclareTVAL(tmp);
DeclareTVAL(tmp2);
DeclareTVAL(tmp3);
DeclareTVAL(ret);
DeclareTVAL(sav);
DeclareTVAL(err);
DeclareOBJArray(TPair,pairs,3);
EndFrame

/*   Recognize the #name# pre-compiler directive */
stringPtr = &atHMChar(hSrc,0);
if ((&atHMChar(hSrc,0))[0] == '#')
	{	
	hNdx = 1;
	*ret = FProcedure_recName(gCP,gTP,hSrc,&hNdx,&aTSymbol,FALSE);
	if ((asBool(ret) == TRUE) && (stringPtr[hNdx] == '#'))
		{
		*ret = TSymbol_GetGlobalValue(gCP,gTP,TOBJ(aTSymbol));
		*tmp = TSTRING(stringPtr);
		FrameExit( FSmartbase_Eval(gCP,gTP,*ret,1,*tmp) );
		} 
	}

/*  Initialize all of the control variables for this procedure. */

*err					= gCP->TObject_ERROR_INVALID;
curPair					= basePair = TPair_New(gCP,gTP);
curPair->itsCar.Tag		= TYLEX;
stackNdx				= 0;
cvrNdx					= 0;
savNdx					= 0;
quoteNdx				= 0;
quoteCnt				= 0;
hNdx					= 0;
vNdx					= 0;
hLen					= strlen((char*)&atHMChar(hSrc,0));
tmpChar					= 0;
bInsertBrk				= FALSE;
*tmp					= gCP->Tval_VOID;
stack					= TObjVector_New(gCP,gTP);
FObject_SetMaxIndex(gCP, gTP,(TObject*)stack, _FLisp_StackAlloc);

/*  We initialize the stack of characters which we use to track compound variable references. */

for (anInt = 0; anInt < _FLisp_CVRSize; ++anInt) cvrChar[anInt] = 0;
    

if ((argc == 1) && (argv[0].Tag == TYLAMBDA))
    {
    /*  If the second argument is a procedure object */
    /*  we  will attach a vector to receive the      */
    /*  unmodified source for this input.            */
    
    theProc = (TLambda*)asObject(&argv[0]);
    dbgSourceLines = TVector_New(gCP,gTP);
    srcVecTval->Tag = dbgSourceLines->itsObjectType;
    srcVecTval->u.Vector = dbgSourceLines;
    theProc->DebuggerSource = (TObject*)dbgSourceLines;
    }

/* Are we generating debugging information for this Lambda? */
if (gTP->FCompile_GenerateDebugInfo == TRUE)
    {
   /* Do we need to create an internal Lambda object? */
    if (theProc == NIL)
		{
		theProc = TLambda_New(gCP,gTP);
		}
	*dbgSourceLinesSYM	= TOBJ(gCP->FCompile_dbgSourceLinesSYM);
	*dbgListLinesSYM	= TOBJ(gCP->FCompile_dbgListLinesSYM);
	*dbgParseTreeSYM	= TOBJ(gCP->FCompile_dbgParseTreeSYM);

    /*  Create the dbgSourceLines for this object */
    
    if (dbgSourceLines == NIL) dbgSourceLines = TVector_New(gCP,gTP);

    /*  Create the In for this object and attach a binding for the debug vector */
    
    if (theProc->Interfaces == NIL)
        {
        tmpEnv = TStructure_New(gCP,gTP);
        theProc->Interfaces = tmpEnv;
        }
    else
        tmpEnv = theProc->Interfaces;
        
    srcVecTval->Tag = dbgSourceLines->itsObjectType;
    srcVecTval->u.Vector = dbgSourceLines;
    argTval->u.Structure = tmpEnv;
    argTval->Tag = TYSTRUCTURE;
    TStructure_AddNewValue(gCP,gTP,*argTval,*dbgSourceLinesSYM,*srcVecTval);
        
    dbgListLines = TStructure_New(gCP,gTP);
    sav->u.Structure = dbgListLines;
    sav->Tag = TYSTRUCTURE;
    argTval->u.Structure = tmpEnv;
    argTval->Tag = TYSTRUCTURE;
    TStructure_AddNewValue(gCP,gTP,*argTval,*dbgListLinesSYM,*sav);
    returnLambda = TRUE;
    }

/*  Set flag so that we will gen a new break when a new line is encountered */

bInsertBrk = TRUE;

/*  If this is extended lexical analysis mode, then go there. */

if (extendedLex == 1) goto ExtendedLexMode;


/*  Trap each source character, using it as a switch to guide the lexical analysis. */

while( hNdx < hLen  )
    {
    /*  Loop through the data */
    
    /*  We make the default assumption that we will recognize an atom which we will add to the */
    /*  parse tree which we are building. */
    
    bAddAtom = TRUE;
    
    if (cvrChar[cvrNdx] == '.' && atHMChar(hSrc,hNdx) != '|' &&
        atHMChar(hSrc,hNdx) != '.' && atHMChar(hSrc,hNdx) != '[' && 
        atHMChar(hSrc,hNdx) != '#' && !ISSYMBOL((NUM)atHMChar(hSrc,hNdx)))
        {
        /*  We have satisfied the tests which indicate that we are terminating a compound variable  */
        /*  reference. We will therefore pop the stack, and reset the cvrChar for this level.		*/
        
        _FLisp_Pop(stack, stackNdx, curPair);
        cvrChar[cvrNdx] = 0;
        }
	else
    if (cvrChar[cvrNdx] == '@' && atHMChar(hSrc,hNdx) != '|' &&
        atHMChar(hSrc,hNdx) != '.' && atHMChar(hSrc,hNdx) != '[' && 
        atHMChar(hSrc,hNdx) != '#' && !ISSYMBOL((NUM)atHMChar(hSrc,hNdx)))
        {
        /*  We have satisfied the tests which indicate that we are terminating a compound macro   */
        /*  reference. We will therefore pop the stack, and reset the cvrChar for this level.     */
        
        _FLisp_Pop(stack, stackNdx, curPair);
        cvrChar[cvrNdx] = 0;
        }
       
    switch ((curChar = atHMChar(hSrc,hNdx++)))
        {
        /*  Use each source character as a switch to guide the lexical analysis. */
        
        case '[':
            /*  Keep track of compound variable references by incrementing the cvrNdx counter */
            /*  and allocating and pushing into a new branch in the parse tree. */
            
            ++cvrNdx;
            *sav = curPair->itsCar;
            asTag(&curPair->itsCar) = TYLEX;
            curPair->itsCdr = gCP->Tval_VOID;
                    
            _FLisp_Push(stack, stackNdx, 0, curPair);
            
            /* Save debugging information (if requested) */
            if ((dbgListLines != NIL) && (firstPairInSourceLine == NIL))
				{
				firstPairInSourceLine = curPair;
				*ret = FSmartbase_Set(gCP,gTP,3,TOBJ(dbgListLines),TOBJ(firstPairInSourceLine),TINT(sourceLineNumber));
				ExitOnError(*ret);
				}
            
            /*  Compound variable references are translated to calls to ref. */
            /*  For example x[1] translates to (ref x 1) */
            /*  Link a ( ref ..) call at the car of the new subexpression list. */

            _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->FMacro_refSym, 0);
            _FLisp_AddAtom(stackNdx, curPair, *tmp);
            
            /*  Add the previous sub-expression */
            
            _FLisp_AddAtom(stackNdx, curPair, *sav);
            
            cvrChar[cvrNdx] = '[';
            
            /*  We have already added the atom for the newly created node so we unset the flag */
            /*  to add this atom. */
            
            bAddAtom = FALSE;
        break;
        
        case '(':   
            /*  Every time that we have an explicit sub-expression we generate a new branch in our */
            /*  parse tree and push our context to the newly created node. */
            
            /*  Save end of current expression and push */
            _FLisp_Push(stack, stackNdx,  quoteNdx, curPair);
            
            /* Save debugging information (if requested) */
            if ((dbgListLines != NIL) && (firstPairInSourceLine == NIL))
				{
				firstPairInSourceLine = curPair;
				*ret = FSmartbase_Set(gCP,gTP,3,TOBJ(dbgListLines),TOBJ(firstPairInSourceLine),TINT(sourceLineNumber));
				ExitOnError(*ret);
				}
            
            /*  We reset control variables for this new node. */
            
            quoteNdx = 0;
            
            /*  if this is a quoted or nested quoted list, convert to a list constant */
            
            if (quoteCnt)
                {
                /*  We have overloaded the meaning of TYPCODE to allow us to insert atoms with */
                /*  values which will have special for use when we have completed processing the */
                /*  rest of the expression. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeList;
                }
            else
                {
                
                bAddAtom = FALSE;
                }
        break;
        
        case ']':
            /*  We are terminating a compound variable reference. We pop back out of the subtree */
            /*  which we created for the ref call associated with this compound variable reference. */
            
            _FLisp_Pop(stack, stackNdx, curPair);
            
            /*  Unset the char flag which tells us whether a node was created in the generation */
            /*  of a compound variable reference. */
            
            cvrChar[cvrNdx] = 0;

            --cvrNdx;
            
            /*  No other action is associated with exiting a compound variable reference. */
            
            bAddAtom = FALSE;
        break;      
                                                         
        case ')':
            /*  We are finishing the processing of a nested expression. */
            
            tmpPair = curPair;
            
        case '}':
            /*  We are finishing the processing of a Structure constant (or a nested expression). */
            
            /*  We may not add any data for this operation. */
            bAddAtom = FALSE;
            
            /*  We figure the quote count associated with this node only by getting the count */
            /*  before the pop, then popping, getting the count after the pop and calculating */
            /*  the difference between the two, thus if we have nested quoted expressions, we */
            /*  will assign the correct number of quotes to each one.  */
            
            popQuoteCount = quoteCnt;
            _FLisp_Pop(stack, stackNdx, curPair);
            popQuoteCount -= quoteCnt;
            
            /*  Get information required for post-processing this node, certain cases require */
            /*  further analysis. */
            
            popPair = curPair;
            *tmp = FUtil2_Length(gCP,gTP,1, &popPair->itsCar);
            aReal = asInt(tmp);
            
            /*  Set up an array of pointers into the list for easy indexed access */
            
            _FMacro_PairsToArray(asPair(&popPair->itsCar), pairs, sizeof(pairs)/sizeof(pairs[0]));
            
            /*  We check for some particular syntactic cases. */
            
            if(aReal == 3 && asTag(&pairs[1]->itsCar) == TYSYMBOL && 
            asSymbol(&pairs[1]->itsCar) == gCP->TLambda_dotcdr)
                {
                /*  When we parse a list  containing a cdr specification, we initially */
                /*  insert the TLambda_dotcdr symbol, then we we are done processing that */
                /*  expression, we check to see if was have a list or a simple pair. If it looks */
                /*  like a pair (because it is length three and the middle element is the */
                /*  TLambda_dotcdr symbol) then we flatten out the second element in the list */
                /*  and set the cdr of the first element to be the car of the third element. */
                
                tmpPair = TPair_New(gCP,gTP);
                tmpPair->itsCar = pairs[0]->itsCar;
                tmpPair->itsCdr = pairs[2]->itsCar;
                
                asTag(tmp) = TYPAIR;
                asObject(tmp) = (TObject*)tmpPair;
                popPair->itsCar = *tmp;
                }
            else
            if(asTag(&pairs[0]->itsCar) == TYSYMBOL)
                {
                if(asObject(&pairs[0]->itsCar) == (TObject*)gCP->FMacro_quoteSym )
                    {
                    /*  This is the case where we translate from cases like: */
                    /*      (quote (A  (+  1  2)  (x  y))) */
                    /*  to: */
                    /*      '(A (+ 1 2) (x y)) */
                    
                    /*  What we do is to flatten out the unneeded level in the parse tree */
                    /*  and increment the quote count for the revealed level. */
                    
                    curPair->itsCar = tmpPair->itsCar;
                    curPair->itsCdr = tmpPair->itsCdr;
                    popPair = curPair;
                    *tmp = FUtil2_Length(gCP,gTP,1, &popPair->itsCar);
                    aReal = asInt(tmp);
                    if (asTag(&popPair->itsCar) == TYPAIR)
                        {
                         _FMacro_PairsToArray(asPair(&popPair->itsCar), pairs, sizeof(pairs)/sizeof(pairs[0]));
                        }
                    }           
                }
            else
            if((isNullTval(&pairs[0]->itsCar) || ((asTag(&pairs[0]->itsCar) == TYPCODE) && (asShort(&pairs[0]->itsCar) == _FLisp_MakeList))) && isNullTval(&pairs[0]->itsCdr))
                {
                /*  We tag this node as available for reuse. */
                
                asTag(&popPair->itsCar) = TYLEX;
                popPair->itsCdr = gCP->Tval_VOID;
                if(popQuoteCount)
                    {
                    /*  When we parse a quoted empty list we generate a quoted empty list, but */
                    /*  we translate that into a quoted TLambda_nil symbol because this */
                    /*  greatly simplifies later processing. */

                    asTag(tmp) = TYQUOTEDSYMBOL;
                    asObject(tmp) = (TObject*)gCP->TLambda_nil;
                    asQuoteCnt(tmp) = popQuoteCount;
                    }
                else
                    {
                    asTag(tmp) = TYSYMBOL;
                    asObject(tmp) = (TObject*)gCP->TLambda_nil;
                    }
                /*  We will add the constructed atom at the bottom of this loop */
                
                bAddAtom = TRUE;
                }
            else
            if(asTag(&pairs[0]->itsCar) == TYPCODE)
                {
                /*  We handle all of the special cases in which we have overloaded a TYPCODE */
                /*  to generate constant objects. */
                
                switch(asShort(&pairs[0]->itsCar))
                    {
                    case    _FLisp_MakeStructure:
                        /*  We have simply saved the argument list which we will send */
                        /*  to FConvert_ObjToStructure to create a constant object. */
                        
						if (pairs[1] != NIL)
							{
							asTag(tmp2) = TYPAIR;
							asObject(tmp2) = (TObject*)pairs[1];
							*tmp = FConvert_ObjToStructure(gCP,gTP,1,tmp2);
							}
						else
							{
							tmp->u.Structure = TStructure_New(gCP,gTP);
							tmp->Tag = TYSTRUCTURE;
							}
                        if(asTag(tmp) == TYSTRUCTURE)
                            {
                            /*  The conversion was successful */
                            
                            popPair->itsCar = *tmp;
                            }
                        else
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToStructure */
      						*err = FLisp_Error(gCP,gTP,"Lisp: Invalid structure constant", &atHMChar(hSrc,0), hNdx, hLen); 
                            bAddAtom = TRUE;
							goto BadCleanUp;
                           }
                    break;
                    
                    case    _FLisp_MakeTemplate:
                        /*  We have simply saved the argument list which we will send   */
                        /*  to FConvert_ObjToDeclStructure to create a constant object. */
                        
						if (pairs[1] != NIL)
							{
							tmp->u.Pair = pairs[1];
							tmp->Tag = TYPAIR;
							//*tmp = FConvert_ObjToStructure(gCP,gTP,1, tmp);
							*tmp = FConvert_ObjToDeclStructure(gCP,gTP,1, tmp);
							}
						else
							{
							tmp->u.Structure = TStructure_New(gCP,gTP);
							tmp->Tag = TYSTRUCTURE;
							}
                        if(asTag(tmp) == TYSTRUCTURE)
                            {
                            /*  The conversion was successful */
                            
                            popPair->itsCar = *tmp;
                            }
                        else
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToStructure */
      						*err = FLisp_Error(gCP,gTP,"Lisp: Invalid decl structure constant", &atHMChar(hSrc,0), hNdx, hLen); 
                            bAddAtom = TRUE;
							goto BadCleanUp;
                           }
                    break;
                    
                    case    _FLisp_MakeDirectory:
                        /*  We have simply saved the argument list which we will send */
                        /*  to FConvert_ObjToDirectory to create a constant object. */
                        
						if (pairs[1] != NIL)
							{
							asTag(tmp) = TYPAIR;
							asObject(tmp) = (TObject*)pairs[1];
							*tmp = FConvert_ObjToDirectory(gCP,gTP,1, tmp);
							}
						else
							{
							tmp->u.Directory = TDirectory_New(gCP,gTP);
							tmp->Tag = TYDIRECTORY;
							}
                        if(asTag(tmp) == TYDIRECTORY)
                            {
                            /*  The conversion was successful */
                            
                            popPair->itsCar = *tmp;
                            }
                        else
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToDirectory */                       
      						*err = FLisp_Error(gCP,gTP,"Lisp: Invalid directory constant", &atHMChar(hSrc,0), hNdx, hLen); 
                            bAddAtom = TRUE;
 							goto BadCleanUp;
                           }
                    break;
                    
                    case    _FLisp_MakeDictionary:
                        /*  We have simply saved the argument list which we will send */
                        /*  to FConvert_ObjToDirectory to create a constant object. */
                        
						if (pairs[1] != NIL)
							{
							asTag(tmp) = TYPAIR;
							asObject(tmp) = (TObject*)pairs[1];
							*tmp = FConvert_ObjToDictionary(gCP,gTP,1, tmp);
							}
						else
							{
							tmp->u.Dictionary = TDictionary_New(gCP,gTP);
							tmp->Tag = TYDICTIONARY;
							}
                        if(asTag(tmp) == TYDICTIONARY)
                            {
                            /*  The conversion was successful */
                            
                            popPair->itsCar = *tmp;
                            }
                        else
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToDictionary */
        					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid dictionary constant", &atHMChar(hSrc,0), hNdx, hLen); 
                            bAddAtom = TRUE;
 							goto BadCleanUp;
                           }
                    break;
                    
                    case    _FLisp_MakeList:
                        /*  We have simply saved the argument list which we will send */
                        /*  to FConvert_ObjToList to create a constant object. */
                        
						if (pairs[1] != NIL)
							{
							asTag(tmp) = TYPAIR;
							asObject(tmp) = (TObject*)pairs[1];
							*tmp = FConvert_ObjToList(gCP,gTP,1, tmp);
							}
						else
							{
        					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid list constant", &atHMChar(hSrc,0), hNdx, hLen); 
                            bAddAtom = TRUE;
 							goto BadCleanUp;
							}
                        if(asTag(tmp) == TYPAIR)
                            {
                            /*  The conversion was successful */
                            
                            if(popQuoteCount)
                                {
                                asTag(tmp) = TYQUOTEDPAIR;
                                asQuoteCnt(tmp) = popQuoteCount;
                                }
                            else
                                asTag(tmp) = TYPAIR;
                            popPair->itsCar = *tmp;
                            }
                        else
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToList */                       
        					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid list constant", &atHMChar(hSrc,0), hNdx, hLen); 
                            bAddAtom = TRUE;
 							goto BadCleanUp;
                           }
                    break;
                    
                    case    _FLisp_MakeVector:
                        /*  We have simply saved the argument list which we will send */
                        /*  to FConvert_ObjToVector to create a constant object. */
						if (pairs[1] != NIL)
							{
							asTag(tmp) = TYPAIR;
							asObject(tmp) = (TObject*)pairs[1];
							*tmp = FConvert_ObjToVector(gCP,gTP,1, tmp);
							}
						else
							{
							tmp->u.Vector = TVector_New(gCP,gTP);
							tmp->Tag = TYVECTOR;
							}
                        if(asTag(tmp) == TYVECTOR)
                            {
                            /*  The conversion was successful */
                            popPair->itsCar = *tmp;
                            }
                        else
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToVector */
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Vector constant", &atHMChar(hSrc,0), hNdx, hLen); 
                            bAddAtom = TRUE;
  							goto BadCleanUp;
                           }
                    break;
                    
                    case    _FLisp_MakeTypeVector:
                        /*  We have simply saved the argument list which we will send */
                        /*  to FConvert_ObjToVector to create an argument object, */
                        /*  then to FMake_Vector to create a Vector object, */
						if (pairs[1] != NIL)
							{
							asTag(tmp) = TYPAIR;
							asObject(tmp) = (TObject*)pairs[1];
							*sav = FConvert_ObjToVector(gCP,gTP,1, tmp);
						
							}
						else
							{
							sav->u.Vector = TVector_New(gCP,gTP);
							sav->Tag = TYVECTOR;
							}
                        if (asTag(sav) != TYVECTOR)
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToVector */
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Vector constant", &atHMChar(hSrc,0), hNdx, hLen); 
                            bAddAtom = TRUE;
  							goto BadCleanUp;
                           }
						len = FSmartbase_ObjectLen(gCP,gTP,sav);
						*tmp = FMake_Vector(gCP,gTP,len,TvalArray(*sav));
                        if (tmp->Tag == TYERROR)
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToVector */
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Matrix constant", &atHMChar(hSrc,0), hNdx, hLen); 
                            bAddAtom = TRUE;
  							goto BadCleanUp;
							}
						else
							{ // If the object vector has a cdr, attach the cdr to result vector.
							if (sav->u.Vector->itsCdr.Tag != TYVOID)
								FObject_SetCdr(gCP, gTP, asObject(tmp),sav->u.Vector->itsCdr);
                            popPair->itsCar = *tmp;
                            }
					break;
                    
                    case    _FLisp_MakeMatrix:
                        /*  We have simply saved the argument list which we will send */
                        /*  to FConvert_ObjToVector to create an argument object, */
                        /*  then to FMake_Matrix to create a Matrix object, */
                        
						if (pairs[1] != NIL)
							{
							asTag(tmp) = TYPAIR;
							asObject(tmp) = (TObject*)pairs[1];
							*tmp = FConvert_ObjToVector(gCP,gTP,1, tmp);
							}
						else
							{
							tmp->u.Vector = TVector_New(gCP,gTP);
							tmp->Tag = TYVECTOR;
							}
                        if (asTag(tmp) != TYVECTOR)
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToVector */
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Matrix constant", &atHMChar(hSrc,0), hNdx, hLen); 
                            bAddAtom = TRUE;
  							goto BadCleanUp;
                           }
						len = FSmartbase_ObjectLen(gCP,gTP,tmp);
						*tmp = FMake_Matrix(gCP,gTP,len,TvalArray(*tmp));
                        if ((tmp->Tag != TYMATRIX) && (tmp->Tag != TYNUMMATRIX))
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToVector */
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Matrix constant", &atHMChar(hSrc,0), hNdx, hLen); 
                            bAddAtom = TRUE;
  							goto BadCleanUp;
                           }
						else
							{
                            /*  The conversion was successful */
                            
                            popPair->itsCar = *tmp;
                            }
					break;

                    case    _FLisp_MakeBrick:
                        /*  We have simply saved the argument list which we will send to */
                        /*  FConvert_ObjToVector to create a field list vector, and then */
                        /*  TBrick_MakeNew to create a Brick template constant object.  */

						if (pairs[1] != NIL)
							{
							asTag(tmp) = TYPAIR;
							asObject(tmp) = (TObject*)pairs[1];
							*tmp = FConvert_ObjToVector(gCP,gTP,1, tmp);
							}
						else
							{
							tmp->u.Vector = TVector_New(gCP,gTP);
							tmp->Tag = TYVECTOR;
							}
                        if (asTag(tmp) != TYVECTOR)
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToVector */
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
                            bAddAtom = TRUE;
  							goto BadCleanUp;
                           }
						len = FSmartbase_ObjectLen(gCP,gTP,tmp);
						*tmp = TBrick_MakeNew(gCP,gTP,len,TvalArray(*tmp));
                        if (tmp->Tag != TYBRICK)
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToVector */
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
                            bAddAtom = TRUE;
  							goto BadCleanUp;
                           }
						else
							{
                            /*  The conversion was successful */

                            popPair->itsCar = *tmp;
                            }
					break;

                    case    _FLisp_MakeBrickInit:
                        /*  We have simply saved the argument list which we will send to */
                        /*  FConvert_ObjToVector to create a field list vector, and then */
                        /*  TBrick_MakeNew to create a Brick template constant object.  */
                        
						if (pairs[1] != NIL)
							{
							asTag(tmp) = TYPAIR;
							asObject(tmp) = (TObject*)pairs[1];
							*tmp = FConvert_ObjToVector(gCP,gTP,1, tmp);
							}
						else
							{
							tmp->u.Vector = TVector_New(gCP,gTP);
							tmp->Tag = TYVECTOR;
							}
                        if (asTag(tmp) != TYVECTOR)
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToVector */
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick constant", &atHMChar(hSrc,0), hNdx, hLen);
                            bAddAtom = TRUE;
  							goto BadCleanUp;
                           }

                        /* Check for '|' after the ')' */
                        if ((&atHMChar(hSrc,0))[hNdx] != '|')
							{
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick constant", &atHMChar(hSrc,0), hNdx, hLen);
                            bAddAtom = TRUE;
  							goto BadCleanUp;
							}
                        hNdx++;

						len = FSmartbase_ObjectLen(gCP,gTP,tmp);
						*tmp = TBrick_MakeNew(gCP,gTP,len,TvalArray(*tmp));
                        if (tmp->Tag != TYBRICK)
                            {
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick constant", &atHMChar(hSrc,0), hNdx, hLen);
                            bAddAtom = TRUE;
  							goto BadCleanUp;
                           }
						else
							{
							/* save the created Brick object */
							popPair->itsCar = *tmp;

							/* skip whitespace characters */
							while (((&atHMChar(hSrc,0))[hNdx] <= ' ') && ((&atHMChar(hSrc,0))[hNdx] > 0)) ++hNdx;
							if ((&atHMChar(hSrc,0))[hNdx] == '[')
								{
								/* #(brk(...)| [index](field:value ...)) */

								hNdx++;
								/* get the index */
								*ret = FProcedure_recNumber(gCP,gTP,hSrc,&hNdx,&aReal,&aNumber);
								if (ret->u.Bool != TRUE)
									{
									*err = FLisp_Error(gCP,gTP,"Lisp: Integer index expected", &atHMChar(hSrc,0), hNdx, hLen);
									bAddAtom = TRUE;
									goto BadCleanUp;
									}

								if ((&atHMChar(hSrc,0))[hNdx] != ']')
									{
									*err = FLisp_Error(gCP,gTP,"Lisp: ']' expected", &atHMChar(hSrc,0), hNdx, hLen);
									bAddAtom = TRUE;
									goto BadCleanUp;
									}
								hNdx++;

								if ((&atHMChar(hSrc,0))[hNdx] != '(')
									{
									*err = FLisp_Error(gCP,gTP,"Lisp: '(' expected", &atHMChar(hSrc,0), hNdx, hLen);
									bAddAtom = TRUE;
									goto BadCleanUp;
									}
								hNdx++;

								/* add the index to the stack */
								*tmp = TINT(aReal);
								_FLisp_AddAtom(stackNdx, curPair, *tmp);

								/* start another branch */
				                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
				                quoteNdx = 0;

				                asTag(tmp) = TYPCODE;
				                asShort(tmp) = _FLisp_MakeBrickList;
								_FLisp_AddAtom(stackNdx, curPair, *tmp);

								bAddAtom = FALSE;
								}
							else
								{
								/* #(brk(...)| field:value field[index]:value) */
								}
							/* continue parsing for Brick values */
							/* use the no. of rows to determine the maximum no. of iterations */
							/* look for '[' and ']' to determine the index */
							/* look for '(' and ')' to determine the value list */
							/* the value list begins with a Symbol, optionally followed by another index */
							/* then finally a ':', followed by the value */
                            /*  The conversion was successful */
                            }
					break;

                    case    _FLisp_MakeBrickList:
                        /*  We have simply saved the argument list which we will send to */
                        /*  FConvert_ObjToVector to create a field list vector, and then */
                        /*  TBrick_MakeNew to create a Brick template constant object.  */

						if (pairs[1] != NIL)
							{
							asTag(tmp) = TYPAIR;
							asObject(tmp) = (TObject*)pairs[1];
							*tmp = FConvert_ObjToVector(gCP,gTP,1, tmp);
							}
						else
							{
							tmp->u.Vector = TVector_New(gCP,gTP);
							tmp->Tag = TYVECTOR;
							}
                        if (asTag(tmp) != TYVECTOR)
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToVector */
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick values", &atHMChar(hSrc,0), hNdx, hLen);
                            bAddAtom = TRUE;
  							goto BadCleanUp;
                           }
						len = FSmartbase_ObjectLen(gCP,gTP,tmp);

						/* save the Brick value list */
						popPair->itsCar = *tmp;

						/* skip whitespace characters */
						while (((&atHMChar(hSrc,0))[hNdx] <= ' ') && ((&atHMChar(hSrc,0))[hNdx] > 0)) ++hNdx;
						if ((&atHMChar(hSrc,0))[hNdx] == '[')
							{
							hNdx++;
							/* get the index */
							*ret = FProcedure_recNumber(gCP,gTP,hSrc,&hNdx,&aReal,&aNumber);
							if (ret->u.Bool != TRUE)
								{
								*err = FLisp_Error(gCP,gTP,"Lisp: Integer index expected", &atHMChar(hSrc,0), hNdx, hLen);
								bAddAtom = TRUE;
								goto BadCleanUp;
								}

							if ((&atHMChar(hSrc,0))[hNdx] != ']')
								{
								*err = FLisp_Error(gCP,gTP,"Lisp: ']' expected", &atHMChar(hSrc,0), hNdx, hLen);
								bAddAtom = TRUE;
								goto BadCleanUp;
								}
							hNdx++;

							if ((&atHMChar(hSrc,0))[hNdx] != '(')
								{
								*err = FLisp_Error(gCP,gTP,"Lisp: '(' expected", &atHMChar(hSrc,0), hNdx, hLen);
								bAddAtom = TRUE;
								goto BadCleanUp;
								}
							hNdx++;

							/* add the index to the stack */
							*tmp = TINT(aReal);
							_FLisp_AddAtom(stackNdx, curPair, *tmp);

							/* start another branch */
			                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
			                quoteNdx = 0;

			                asTag(tmp) = TYPCODE;
			                asShort(tmp) = _FLisp_MakeBrickList;
							_FLisp_AddAtom(stackNdx, curPair, *tmp);
							}
					break;

                    case    _FLisp_MakeBrickFinal:
                        /*  We have simply saved the argument list which we will send to */
                        /*  FConvert_ObjToVector to create a field list vector, and then */
                        /*  TBrick_MakeNew to create a Brick template constant object.  */

						if (pairs[1] != NIL)
							{
							asTag(tmp) = TYPAIR;
							asObject(tmp) = (TObject*)pairs[1];
							*tmp = FConvert_ObjToVector(gCP,gTP,1, tmp);
							}
						else
							{
							tmp->u.Vector = TVector_New(gCP,gTP);
							tmp->Tag = TYVECTOR;
							}
                        if (asTag(tmp) != TYVECTOR)
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToVector */
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
                            bAddAtom = TRUE;
  							goto BadCleanUp;
                            }
						len = FSmartbase_ObjectLen(gCP,gTP,tmp);
						/* item:0 should be a Brick */
						/* if item:1 is an Integer, we have more than 1 rows */
						/* if item:1 is a Symbol or QuotedSymbol, we have only one row */
						/* if item:1 is a Pair, we have only one row */

						if (len > 2)
							{
							*ret = TvalArray(*tmp)[1];  /* next parameter */
							*tmp3 = TvalArray(*tmp)[0]; /* Brick object */
							if (ret->Tag == TYNUM)
								{
								/* [index](...) [index](...) */
								/* we're dealing with a multi-row Brick */
								for (tmpInt = 1; tmpInt < len; tmpInt++)
									{
									/* get index value */
									*ret = TvalArray(*tmp)[tmpInt];

									if (ret->Tag == TYNUM)
										{
										/* this is a Brick row index */
										brkNdx = asInt(ret);
										}
									else
										{
										/* not an Integer */
										*err = FLisp_Error(gCP,gTP,"Lisp: Brick index expected", &atHMChar(hSrc,0), hNdx, hLen);
										bAddAtom = TRUE;
										goto BadCleanUp;
										}
									tmpInt++;

									/* get Brick values */
									*ret = TvalArray(*tmp)[tmpInt];
									if (ret->Tag == TYVECTOR)
										{
										/* this is a Brick row value list */
										fldLen = asVector(ret)->itsMaxItemIndex;
										if ((fldLen % 2) != 0)
											{
											*err = FLisp_Error(gCP,gTP,"Lisp: Incomplete Brick template", &atHMChar(hSrc,0), hNdx, hLen);
											bAddAtom = TRUE;
											goto BadCleanUp;
											}

										for (fldNdx = 0; fldNdx < fldLen; fldNdx+=2)
											{
											/* process each Brick value pair */
											*tmp2 = TvalArray(*ret)[fldNdx];
											if (asTag(tmp2) == TYSYMBOL || asTag(tmp2) == TYQUOTEDSYMBOL)
												{
												/* TBrick_SetIV3(gCP, gTP, Brick, FieldIndex, RepeatIndex, RowIndex, NewValue) */
												*err = TBrick_SetIV3(gCP, gTP, *tmp3, *tmp2,
															TINT(0), TINT(brkNdx), TvalArray(*ret)[fldNdx+1]);
												if (err->Tag == TYERROR)
													{
													*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
													bAddAtom = TRUE;
													goto BadCleanUp;
													}
												} /* if (asTag(tmp2) == TYSYMBOL || ...) */
											else
											if (asTag(tmp2) == TYPAIR)
												{
												/* (ref Field Index)
												 * (Symbol Symbol Integer) */
												*tmp2 = FConvert_ObjToVector(gCP, gTP, 1, tmp2);
												if (tmp2->Tag != TYVECTOR)
													{
													*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
													bAddAtom = TRUE;
													goto BadCleanUp;
													}

												if (strcmp(SymbolArray(TvalArray(*tmp2)[0]),"ref") != 0)
													{
													*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
													bAddAtom = TRUE;
													goto BadCleanUp;
													}
												/* TBrick_SetIV3(gCP, gTP, Brick, FieldIndex, RepeatIndex, RowIndex, NewValue) */
												*err = TBrick_SetIV3(gCP, gTP, *tmp3,
															TvalArray(*tmp2)[1], TvalArray(*tmp2)[2], TINT(brkNdx), TvalArray(*ret)[fldNdx+1]);
												if (err->Tag == TYERROR)
													{
													*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
													bAddAtom = TRUE;
													goto BadCleanUp;
													}
												} /* if (asTag(tmp2) == TYPAIR) */
											else
												{
												*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
												bAddAtom = TRUE;
												goto BadCleanUp;
												}
											} /* for (fldNdx = 0; ...) */
										} /* if (ret->Tag == TYVECTOR) */
									else
										{
										/* not a Vector */
										*err = FLisp_Error(gCP,gTP,"Lisp: Brick value list expected", &atHMChar(hSrc,0), hNdx, hLen);
										bAddAtom = TRUE;
										goto BadCleanUp;
										}
									} /* for (tmpInt = 1; ...) */
								} /* if (ret->Tag == TYNUM) */
							else
							if ((ret->Tag == TYSYMBOL) || (ret->Tag == TYQUOTEDSYMBOL) || (ret->Tag == TYPAIR))
								{
								/* we're dealing with a single row Brick */
								/* #(brk(...)| Field:Value Field:Value ...) */
								if ((len % 2) == 0)
									{
									*err = FLisp_Error(gCP,gTP,"Lisp: Incomplete Brick template", &atHMChar(hSrc,0), hNdx, hLen);
									bAddAtom = TRUE;
									goto BadCleanUp;
									}

								for (tmpInt = 1; tmpInt < len; tmpInt+=2)
									{
									*tmp2 = TvalArray(*tmp)[tmpInt];
									if (asTag(tmp2) == TYSYMBOL || asTag(tmp2) == TYQUOTEDSYMBOL)
										{
										/* TBrick_SetIV3(gCP, gTP, Brick, FieldIndex, RepeatIndex, RowIndex, NewValue) */
										*err = TBrick_SetIV3(gCP, gTP, *tmp3, *tmp2,
													TINT(0), TINT(0), TvalArray(*tmp)[tmpInt+1]);
										if (err->Tag == TYERROR)
											{
											*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
											bAddAtom = TRUE;
											goto BadCleanUp;
											}
										}
									else
									if (asTag(tmp2) == TYPAIR)
										{
										/* (ref Field Index)
										 * (Symbol Symbol Integer) */
										*tmp2 = FConvert_ObjToVector(gCP, gTP, 1, tmp2);
										if (tmp2->Tag != TYVECTOR)
											{
											*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
											bAddAtom = TRUE;
											goto BadCleanUp;
											}

										if (strcmp(SymbolArray(TvalArray(*tmp2)[0]),"ref") != 0)
											{
											*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
											bAddAtom = TRUE;
											goto BadCleanUp;
											}

										/* TBrick_SetIV3(gCP, gTP, Brick, FieldIndex, RepeatIndex, RowIndex, NewValue) */
										*err = TBrick_SetIV3(gCP, gTP, *tmp3,
													TvalArray(*tmp2)[1], TvalArray(*tmp2)[2], TINT(0), TvalArray(*tmp)[tmpInt+1]);
										if (err->Tag == TYERROR)
											{
											*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
											bAddAtom = TRUE;
											goto BadCleanUp;
											}
										}
									else
										{
										*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
										bAddAtom = TRUE;
										goto BadCleanUp;
										}
									} /* for (tmpInt = 1; ... ) */
								}
							else /* if ((ret->Tag == TYSYMBOL) || ... ) */
								{
								/* throw an error: unexpected type */
								*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
								bAddAtom = TRUE;
								goto BadCleanUp;
								}
							}

						*tmp = TvalArray(*tmp)[0];
                        if (tmp->Tag != TYBRICK)
                            {
                            /*  There was a syntax error in the data for FConvert_ObjToVector */
         					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Brick template", &atHMChar(hSrc,0), hNdx, hLen);
                            bAddAtom = TRUE;
  							goto BadCleanUp;
                            }
						else
							{
                            /*  The conversion was successful */
                            popPair->itsCar = *tmp;
                            }
					break;

					case    _FLisp_MakeComplex:
						//  Send the argument list to FConvert_ToComplex to create complex
						asTag(tmp) = TYPAIR;
						asObject(tmp) = (TObject*)pairs[1];
						len = 2;
						*tmp = FConvert_ObjToComplex(gCP, gTP, &len, *tmp);
						if(asTag(tmp) == TYCPX)
						{	// The conversion was successful
							popPair->itsCar = *tmp;
						}
						else
						{	//  There was a syntax error in the data for FConvert_ToComplex
							*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Complex constant", &atHMChar(hSrc,0), hNdx, hLen); 
							bAddAtom = TRUE;
							goto BadCleanUp;
						}
						break;

                    default:
                        /*  We do not recognize this syntax, so we insert an error TVAL directly */
                        /*  in the parse tree at the point of error. */
         				*err = FLisp_Error(gCP,gTP,"Lisp: Invalid constant", &atHMChar(hSrc,0), hNdx, hLen); 
                        bAddAtom = TRUE;
   						goto BadCleanUp;
                  break;
                    }
                }
        break;
                
        case '#':
            /*  We attempt to recognize the syntax for all objects starting with '#' */
            
            if (
            ((&atHMChar(hSrc,0))[hNdx]== 'L')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'O')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'C')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 'K')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == 'G')    &&
            ((&atHMChar(hSrc,0))[hNdx+5] == 'L')    &&
            ((&atHMChar(hSrc,0))[hNdx+6] == 'O')    &&
            ((&atHMChar(hSrc,0))[hNdx+7] == 'B')    &&
            ((&atHMChar(hSrc,0))[hNdx+8] == 'A')    &&
            ((&atHMChar(hSrc,0))[hNdx+9] == 'L')    &&
            ((&atHMChar(hSrc,0))[hNdx+10] == 'S')   &&
            ((&atHMChar(hSrc,0))[hNdx+11] == '#')
            ) 
                {
                asTag(tmp) = TYGLOBALS;
                FUtil3_Lock(gCP,gTP,1,tmp);
                bAddAtom = FALSE;
                hNdx += 12;
                }
            else
            if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'm')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'a')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 't')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == '|') 
            ) 
                {
                /*  We recognize a matrix constant: #(mat| 1 2 3) */
                /*  Insert a TYPCODE to cause creation of a matrix constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 5;
                
                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;

                /*  Add a makeMatrix call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeMatrix;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a "~" at the car of the subexpression list for Matrix length. */
                
				tmp->Tag = TYTEXT;
				tmp->u.Text[0] = '~';
				tmp->u.Text[1] = 0;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				bAddAtom = FALSE;
				}
            else
            if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'm')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'a')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 't')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == '[') 
            ) 
                {
                /*  We recognize a matrix constant: #(mat[x y z]| 1 2 3) */
                /*  Insert a TYPCODE to cause creation of a matrix constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 5;

				/* Recognize the Matrix dimension constants */

				for (rank = 0; rank < _MAXDIMENSIONS; ++rank)
					{
					/* Ignore whitespace characters. */

					while (((&atHMChar(hSrc,0))[hNdx] <= ' ') && ((&atHMChar(hSrc,0))[hNdx] > 0)) ++hNdx;

					/* Recognize dimension constants. */

					*ret = FProcedure_recNumber(gCP,gTP,hSrc,&hNdx,&aReal,&aNumber);
					if (ret->u.Bool == TRUE)
						{
                        dimensions[rank] = aReal;
						}
					else
						goto MatrixDim;
					}
                
				MatrixDim:
				if (((&atHMChar(hSrc,0))[hNdx] != ']') &&
					((&atHMChar(hSrc,0))[hNdx+1] != '|'))
					{
                    /*  There was a syntax error in the data for FConvert_ObjToVector */
         			*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Matrix constant", &atHMChar(hSrc,0), hNdx, hLen); 
                    bAddAtom = TRUE;
  					goto BadCleanUp;
					}
				hNdx += 2;

                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;
            
                /*  Add a makeMatrix call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeMatrix;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				/* Push the Matrix rank and dimension constants. */

				*tmp = TINT(rank);
				_FLisp_AddAtom(stackNdx, curPair, *tmp);
				for (i = 0; i < rank; ++i)
					{
					*tmp = TINT(dimensions[i]);
					_FLisp_AddAtom(stackNdx, curPair, *tmp);
					}

				bAddAtom = FALSE;
				}
            else
            if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'n')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'u')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 'm')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == 'm')    &&
            ((&atHMChar(hSrc,0))[hNdx+5] == 'a')    &&
            ((&atHMChar(hSrc,0))[hNdx+6] == 't')    &&
            ((&atHMChar(hSrc,0))[hNdx+7] == '|') 
            ) 
                {
                /*  We recognize a number matrix constant: #(nummat| 1 2 3) */
                /*  Insert a TYPCODE to cause creation of a matrix constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 8;
                
                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;
            
                /*  Add a makeMatrix call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeMatrix;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a number: at the car of the subexpression list for Matrix length. */
                
				*tmp = TSYMBOL("number");;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);


                /*  Add a "~" at the car of the subexpression list for Matrix length. */
                
				tmp->Tag = TYTEXT;
				tmp->u.Text[0] = '~';
				tmp->u.Text[1] = 0;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				bAddAtom = FALSE;
				}
            else
            if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'n')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'u')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 'm')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == 'm')    &&
            ((&atHMChar(hSrc,0))[hNdx+5] == 'a')    &&
            ((&atHMChar(hSrc,0))[hNdx+6] == 't')    &&
            ((&atHMChar(hSrc,0))[hNdx+7] == '[') 
            ) 
                {
                /*  We recognize a number matrix constant: #(nummat[x y z]| 1 2 3) */
                /*  Insert a TYPCODE to cause creation of a matrix constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 8;

				/* Recognize the Matrix dimension constants */

				for (rank = 0; rank < _MAXDIMENSIONS; ++rank)
					{
					/* Ignore whitespace characters. */

					while (((&atHMChar(hSrc,0))[hNdx] <= ' ') && ((&atHMChar(hSrc,0))[hNdx] > 0)) ++hNdx;

					/* Recognize dimension constants. */

					*ret = FProcedure_recNumber(gCP,gTP,hSrc,&hNdx,&aReal,&aNumber);
					if (ret->u.Bool == TRUE)
						{
                        dimensions[rank] = aReal;
						}
					else
						goto NumMatrixDim;
					}
                
				NumMatrixDim:
				if (((&atHMChar(hSrc,0))[hNdx] != ']') &&
					((&atHMChar(hSrc,0))[hNdx+1] != '|'))
					{
                    /*  There was a syntax error in the data for FConvert_ObjToVector */
         			*err = FLisp_Error(gCP,gTP,"Lisp: Invalid Matrix constant", &atHMChar(hSrc,0), hNdx, hLen); 
                    bAddAtom = TRUE;
  					goto BadCleanUp;
					}
				hNdx += 2;

                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;
            
                /*  Add a makeMatrix call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeMatrix;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a number: at the car of the subexpression list for Matrix length. */
                
				*tmp = TSYMBOL("number");;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				/* Push the Matrix rank and dimension constants. */

				*tmp = TINT(rank);
				_FLisp_AddAtom(stackNdx, curPair, *tmp);
				for (i = 0; i < rank; ++i)
					{
					*tmp = TINT(dimensions[i]);
					_FLisp_AddAtom(stackNdx, curPair, *tmp);
					}

				bAddAtom = FALSE;
				}
            else
            if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'b')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'i')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 't')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == '|') 
            ) 
                {
                /*  We recognize a bit vector constant: #(bit| 1 0 1) */
                /*  Insert a TYPCODE to cause creation of a matrix constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 5;
                
                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;
            
                /*  Add a makeVector call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeTypeVector;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a bit: at the car of the subexpression list for Matrix length. */
                
				*tmp = TSYMBOL("bit");;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a "~" at the car of the subexpression list for Matrix length. */
                
				tmp->Tag = TYTEXT;
				tmp->u.Text[0] = '~';
				tmp->u.Text[1] = 0;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				bAddAtom = FALSE;
				}
            else
            if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'i')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'n')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 't')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == '|') 
            ) 
                {
                /*  We recognize a integer vector constant: #(int| 1 0 1) */
                /*  Insert a TYPCODE to cause creation of a matrix constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 5;
                
                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;
            
                /*  Add a makeVector call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeTypeVector;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a bit: at the car of the subexpression list for Matrix length. */
                
				*tmp = TSYMBOL("integer");;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a "~" at the car of the subexpression list for Matrix length. */
                
				tmp->Tag = TYTEXT;
				tmp->u.Text[0] = '~';
				tmp->u.Text[1] = 0;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				bAddAtom = FALSE;
				}
            else
            if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'n')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'u')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 'm')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == '|') 
            ) 
                {
                /*  We recognize a number vector constant: #(num| 1 0 1) */
                /*  Insert a TYPCODE to cause creation of a matrix constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 5;
                
                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;
            
                /*  Add a makeVector call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeTypeVector;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a bit: at the car of the subexpression list for Matrix length. */
                
				*tmp = TSYMBOL("number");;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a "~" at the car of the subexpression list for Matrix length. */
                
				tmp->Tag = TYTEXT;
				tmp->u.Text[0] = '~';
				tmp->u.Text[1] = 0;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				bAddAtom = FALSE;
				}
            else
            if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'b')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'r')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 'k')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == '(')
            )
                {
                /*  We recognize a brick template vector constant: #(brk(Cnt Field:Type:Repeats ...)|) */
                /*  Insert a TYPCODE to cause creation of a brick when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 5;
                
                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;

                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeBrickFinal;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;

                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeBrickInit;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

 				bAddAtom = FALSE;
				}
            else
			if (
			((&atHMChar(hSrc,0))[hNdx]== '(')       &&
			((&atHMChar(hSrc,0))[hNdx+1] == 'b')    &&
			((&atHMChar(hSrc,0))[hNdx+2] == 'r')    &&
			((&atHMChar(hSrc,0))[hNdx+3] == 'k')    &&
			((&atHMChar(hSrc,0))[hNdx+4] == '|')
			)
				{
				/*  We recognize a brick template vector constant: #(brk| Field:Type:Repeats ...) */
				/*  Insert a TYPCODE to cause creation of a brick when we are  */
				/*  popping out of this node. */

				/*  Step to the next character */

				hNdx += 5;

				/*  Save end of current expression and push. Init quoteNdx for the new expression. */

				_FLisp_Push(stack, stackNdx, quoteNdx, curPair);
				quoteNdx = 0;

				/*  Add a makeVector call at the car of the subexpression list. */

				asTag(tmp) = TYPCODE;
				asShort(tmp) = _FLisp_MakeBrick;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				bAddAtom = FALSE;
				}
			else
            if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'o')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'b')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 'j')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == '|') 
            ) 
                {
                /*  We recognize a object vector constant: #(obj| a b c) */
                /*  Insert a TYPCODE to cause creation of a matrix constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 5;
                
                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;
            
                /*  Add a makeVector call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeTypeVector;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a bit: at the car of the subexpression list for Matrix length. */
                
				*tmp = TSYMBOL("object");;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a "~" at the car of the subexpression list for Matrix length. */
                
				tmp->Tag = TYTEXT;
				tmp->u.Text[0] = '~';
				tmp->u.Text[1] = 0;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				bAddAtom = FALSE;
				}
            else
            if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'f')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'l')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 'o')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == 'a')    &&
            ((&atHMChar(hSrc,0))[hNdx+5] == 't')    &&
            ((&atHMChar(hSrc,0))[hNdx+6] == '|') 
            ) 
                {
                /*  We recognize a float vector constant: #(float| 1.2 0.4 1.6) */
                /*  Insert a TYPCODE to cause creation of a matrix constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 7;
                
                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;
            
                /*  Add a makeVector call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeTypeVector;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a bit: at the car of the subexpression list for Matrix length. */
                
				*tmp = TSYMBOL("float");;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a "~" at the car of the subexpression list for Matrix length. */
                
				tmp->Tag = TYTEXT;
				tmp->u.Text[0] = '~';
				tmp->u.Text[1] = 0;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				bAddAtom = FALSE;
				}
            else
            if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 's')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'h')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 'o')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == 'r')    &&
            ((&atHMChar(hSrc,0))[hNdx+5] == 't')    &&
            ((&atHMChar(hSrc,0))[hNdx+6] == '|') 
            ) 
                {
                /*  We recognize a short vector constant: #(short| 1 0 1) */
                /*  Insert a TYPCODE to cause creation of a short constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 7;
                
                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;
            
                /*  Add a makeVector call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeTypeVector;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a short: at the car of the subexpression list for Matrix length. */
                
				*tmp = TSYMBOL("short");;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a "~" at the car of the subexpression list for Vector length. */
                
				tmp->Tag = TYTEXT;
				tmp->u.Text[0] = '~';
				tmp->u.Text[1] = 0;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				bAddAtom = FALSE;
				}
			else if (
            ((&atHMChar(hSrc,0))[hNdx]== '(')       &&
            ((&atHMChar(hSrc,0))[hNdx+1] == 'l')    &&
            ((&atHMChar(hSrc,0))[hNdx+2] == 'o')    &&
            ((&atHMChar(hSrc,0))[hNdx+3] == 'n')    &&
            ((&atHMChar(hSrc,0))[hNdx+4] == 'g')    &&
            ((&atHMChar(hSrc,0))[hNdx+5] == '|') 
            ) 
                {
                /*  We recognize a short vector constant: #(long| 1 0 1) */
                /*  Insert a TYPCODE to cause creation of a long constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx += 6;
                
                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;
            
                /*  Add a makeVector call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeTypeVector;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a short: at the car of the subexpression list for Matrix length. */
                
				*tmp = TSYMBOL("long");;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                /*  Add a "~" at the car of the subexpression list for Vector length. */
                
				tmp->Tag = TYTEXT;
				tmp->u.Text[0] = '~';
				tmp->u.Text[1] = 0;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				bAddAtom = FALSE;
				}
			else if (
			((&atHMChar(hSrc,0))[hNdx]== '(')       &&
			((&atHMChar(hSrc,0))[hNdx+1] == 'c')    &&
			((&atHMChar(hSrc,0))[hNdx+2] == 'p')    &&
			((&atHMChar(hSrc,0))[hNdx+3] == 'x')    &&
			((&atHMChar(hSrc,0))[hNdx+4] == '|')) 
			{	//  Complex vector constant: #(cpx| 1 1.0 2.0)
				hNdx += 5;

				//  Save end of current expression and push.
				_FLisp_Push(stack, stackNdx, quoteNdx, curPair);
				quoteNdx = 0;
            
				//  Insert a TYPCODE to cause creation of a vector constant.                
				asTag(tmp) = TYPCODE;
				asShort(tmp) = _FLisp_MakeTypeVector;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                //  Add Complex: at the car of the subexpression list
				*tmp = TSYMBOL("Complex");
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

                //  Add a "~" at the car of the subexpression list for the length.
				tmp->Tag = TYTEXT;
				tmp->u.Text[0] = '~';
				tmp->u.Text[1] = 0;
				_FLisp_AddAtom(stackNdx, curPair, *tmp);

				bAddAtom = FALSE;
			}
            else
            if ((&atHMChar(hSrc,0))[hNdx] == '(') 
                {
                /*  We recognize a vector constant: #( 1 2 3) */
                /*  Insert a TYPCODE to cause creation of a vector constant when we are  */
                /*  popping out of this node. */

                /*  Step to the next character */
                
                hNdx++;
                
                /*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);
                quoteNdx = 0;
            
                /*  Add a makeVector call at the car of the subexpression list. */
                
                asTag(tmp) = TYPCODE;
                asShort(tmp) = _FLisp_MakeVector;
                }
            else
			if (strncmp((char*)&atHMChar(hSrc,hNdx),"c", 1) == 0)
			{	/* Complex literal of the form #c1.0-2.0i */
                hNdx++;
				/* Check for the imaginary number: #ci */
				if (strncmp((char*)&atHMChar(hSrc,hNdx),"i", 1) == 0)
					{
					hNdx++;
					*ret = TREAL(0.0);
					*tmp = TREAL(1.0);
					goto ComplexConstant;
					}
				else               
				/* Check for the imaginary number: #c-i */
				if (strncmp((char*)&atHMChar(hSrc,hNdx),"-i", 2) == 0)
					{
					hNdx+=2;
					*ret = TREAL(0.0);
					*tmp = TREAL(-1.0);
					goto ComplexConstant;
					}
				else               
				/* Check for the imaginary number: #c+i */
				if (strncmp((char*)&atHMChar(hSrc,hNdx),"+i", 2) == 0)
					{
					hNdx+=2;
					*ret = TREAL(0.0);
					*tmp = TREAL(1.0);
					goto ComplexConstant;
					}

				/* Check for the real component. */
				*ret = FProcedure_recNumber(gCP,gTP,hSrc,&hNdx,&aReal,&aNumber);
				if ((ret->Tag != TYBOLE) || (ret->u.Bool !=TRUE)) goto Bad;
				ret->Tag = TYREAL;
				ret->u.Real = aReal;

				/* Check for the imaginary indicator: #cNN.NNi */
				if (strncmp((char*)&atHMChar(hSrc,hNdx),"i", 1) == 0)
					{
					hNdx++;
					*tmp = *ret;
					*ret = TREAL(0);
					goto ComplexConstant;
					}
				else
				/* Check for the imaginary sign: #cNN.NN-MM.MMi */
				if (strncmp((char*)&atHMChar(hSrc,hNdx),"-", 1) == 0)
					{
					hNdx++;
					*tmp = TREAL(-1.0);
					}
				else
				/* Check for the imaginary sign: #cNN.NN-MM.MMi */
				if (strncmp((char*)&atHMChar(hSrc,hNdx),"+", 1) == 0)
					{
					hNdx++;
					*tmp = TREAL(1.0);
					}
				else
					{
					*tmp = TREAL(0.0);
					goto ComplexConstant;
					}


				/* Check for the imaginary component. */
				*err = FProcedure_recNumber(gCP,gTP,hSrc,&hNdx,&aReal,&aNumber);
				if ((err->Tag != TYBOLE) || (err->u.Bool !=TRUE)) goto Bad;
				tmp->Tag = TYREAL;
				tmp->u.Real = aReal * tmp->u.Real;

				/* Check for the imaginary indicator: #cNN.NNi */
				if (strncmp((char*)&atHMChar(hSrc,hNdx),"i", 1) == 0)
					{
					hNdx++;
					}
				else
					goto Bad;					

				ComplexConstant:

				complex = (TCpx*)TCpx_New(gCP,gTP);
				complex->itsReal = ret->u.Real;
				complex->itsImag = tmp->u.Real;
				*ret = TOBJ(complex);
				_FLisp_AddAtom(stackNdx, curPair, *ret);
				bAddAtom = FALSE;
			}
			else
            if ((&atHMChar(hSrc,0))[hNdx] == '<') 
                {
                /*  We will try to recognize an object index reference of the form : #<Name: nn> or #<Name: name> */
                /*  Step to the next character */                
                hNdx++;
                                
				/* Check for a Type name followed by a space. */
				*ret = FProcedure_recName(gCP,gTP,hSrc,&hNdx,&aTSymbol,FALSE);
				if ((asBool(ret) != TRUE))
					goto Bad; 
					
				/* Skip any white space. */

				while (((&atHMChar(hSrc,0))[hNdx] <= ' ' ) &&
					   ((&atHMChar(hSrc,0))[hNdx] >= 0 ))
					{
					hNdx++;
					}

				savNdx = hNdx;
				
				/* Check for a object identifier followed by a >. */
				*ret = FProcedure_recNumber(gCP,gTP,hSrc,&hNdx,&aReal,&aNumber);
				if (asBool(ret) == TRUE)
					{
					/*  Is this symbol quoted? */
					if ((&atHMChar(hSrc,0))[hNdx] == '>' )
						hNdx++;
                    else
						goto Bad;

					/*  Save end of current expression and push. Init quoteNdx for the new expression. */
					/*  The new expression with contains a call to "inspect" */
					_FLisp_Push(stack, stackNdx, quoteNdx, curPair);
					quoteNdx = 0;
            
					/* Save debugging information (if requested) */
					if ((dbgListLines != NIL) && (firstPairInSourceLine == NIL))
						{
						firstPairInSourceLine = curPair;
						*ret = FSmartbase_Set(gCP,gTP,3,TOBJ(dbgListLines),TOBJ(firstPairInSourceLine),TINT(sourceLineNumber));
						ExitOnError(*ret);
						}
            
					/*  Add a call to inspect, quoting as required. */
        
					*tmp = TSYMBOL("inspect");
					asTag(tmp) = quoteNdx ? TYQUOTEDSYMBOL : TYSYMBOL;
					asQuoteCnt(tmp) = quoteNdx;
					_FLisp_AddAtom(stackNdx, curPair, *tmp);
                                        
					/*  Add a type symbol argument, quoting as required. */
        
					asTag(tmp) = quoteNdx + 1 ? TYQUOTEDSYMBOL : TYSYMBOL;
					asQuoteCnt(tmp) = quoteNdx + 1;
					asObject(tmp) = (struct TObject *)aTSymbol;
					_FLisp_AddAtom(stackNdx, curPair, *tmp);

					/*  Add an integer argument for the object identifier. */
        
					asTag(tmp) = TYNUM;
					asInt(tmp) = aReal;
					_FLisp_AddAtom(stackNdx, curPair, *tmp);


					/* We are terminating an object identifier reference. */
					
					_FLisp_Pop(stack, stackNdx, curPair);
					cvrChar[cvrNdx] = 0;
        
					bAddAtom = FALSE;
					}
				else
				/* Check for a global variable name followed by a >. */
				if (FProcedure_recName(gCP,gTP,hSrc,&savNdx,&aTSymbol,FALSE).u.Bool == TRUE)
					{
					char	buf[200];

					/*  Is this symbol quoted? */
					hNdx = savNdx;
					strncpy(buf,(LpCHAR)*(aTSymbol)->itsCString,198);
					n = strlen(buf);
					if (buf[n-1] == '>' )
						{
						buf[n-1] = 0;
						aTSymbol = TSymbol_MakeUnique(gCP,gTP,buf);
						}
                    else
						goto Bad;

					/*  Add a global variable argument for the object identifier. */
        
					asTag(tmp) = TYSYMBOL;
					//asSymbol(tmp) = aTSymbol; // gcc barfs on this
					tmp->u.Symbol = aTSymbol; //This works but I'm not sure if it is correct
					//(TSymbol*)(((LpTVAL)(&gTP->TvalStack[tmp]))->u.Obj) = aTSymbol; //This is how the macros expand
					//TSymbol is a structure. Obj is a long. aTSymbol is declared DeclareOBJ(TSymbol,aTSymbol)
					// DeclareOBJ(TSymbol,aTSymbol) expands to TSymbol* aTSymbol = (TSymbol *)(gTP->ObjStack[__obj__++] = (OBJ *)(&aTSymbol))
					// 
					_FLisp_AddAtom(stackNdx, curPair, *tmp);

					bAddAtom = FALSE;
					}
				else
					goto Bad; 
				break;
			}
			else 
            if ((&atHMChar(hSrc,0))[hNdx] == '{') 
                {
				if (strncmp((char*)&atHMChar(hSrc,hNdx + 1),"decl|",5) == 0) 
					{
					/*  We recognize an decl Structure constant: #{decl| a b Number:c (Integer:d 2)} */
					/*  Insert a TYPCODE to cause creation of a decl Structure constant */
                
					/*  Step to the next character */

					hNdx+=6;
                
					/*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
					_FLisp_Push(stack, stackNdx, quoteNdx, curPair);
					quoteNdx = 0;
            
					/*  Queue a makeTemplate call at the car of the subexpression list. */
                
					asTag(tmp) = TYPCODE;
					asShort(tmp) = _FLisp_MakeTemplate;
					}
				else
				if (strncmp((char*)&atHMChar(hSrc,hNdx + 1),"dir||",5) == 0) 
					{
					/*  We recognize an Directory constant: #{dir|| 1 a 2 b} */
					/*  Insert a TYPCODE to cause creation of a Directory constant */
                
					/*  Step to the next character */

					hNdx+=6;
                
					/*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
					_FLisp_Push(stack, stackNdx, quoteNdx, curPair);
					quoteNdx = 0;
            
					/*  Queue a makeDirectory call at the car of the subexpression list. */
                
					asTag(tmp) = TYPCODE;
					asShort(tmp) = _FLisp_MakeDirectory;
					_FLisp_AddAtom(stackNdx, curPair, *tmp);

					/*  Add an ellipses at the head of the subexpression argument list. */
					/*  Note: This indicates to make the Directory 'unsorted as is'. */
                
					tmp->Tag = TYELLIPSES;
					tmp->u.Int = NIL;
					}
				else
				if (strncmp((char*)&atHMChar(hSrc,hNdx + 1),"dir|",4) == 0) 
				/* if ((&atHMChar(hSrc,0))[hNdx + 1] == '!') */ 
					{
					/*  We recognize an Directory constant: #{dir| 1 a 2 b} */
					/*  Insert a TYPCODE to cause creation of a Directory constant */
                
					/*  Step to the next character */

					hNdx+=5;
                
					/*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
					_FLisp_Push(stack, stackNdx, quoteNdx, curPair);
					quoteNdx = 0;
            
					/*  Queue a makeDirectory call at the car of the subexpression list. */
                
					asTag(tmp) = TYPCODE;
					asShort(tmp) = _FLisp_MakeDirectory;
					}
				else
				if ((&atHMChar(hSrc,0))[hNdx + 1] == '!')
					{
					/*  We recognize an Directory constant: #{! 1 a 2 b} */
					/*  Insert a TYPCODE to cause creation of a Directory constant */
                
					/*  Step to the next character */

					hNdx+=2;
                
					/*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
					_FLisp_Push(stack, stackNdx, quoteNdx, curPair);
					quoteNdx = 0;
            
					/*  Queue a makeDirectory call at the car of the subexpression list. */
                
					asTag(tmp) = TYPCODE;
					asShort(tmp) = _FLisp_MakeDirectory;
					}
				else
				if (strncmp((char*)&atHMChar(hSrc,hNdx + 1),"dic||",5) == 0) 
					{
					/*  We recognize an Dictionary constant: #{dic|| a 1 b 2} */
					/*  Insert a TYPCODE to cause creation of a Directory constant */
                
					/*  Step to the next character */

					hNdx+=6;
                
					/*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
					_FLisp_Push(stack, stackNdx, quoteNdx, curPair);
					quoteNdx = 0;

					/*  Queue a makeDirectory call at the car of the subexpression list. */
                
					asTag(tmp) = TYPCODE;
					asShort(tmp) = _FLisp_MakeDictionary;
					_FLisp_AddAtom(stackNdx, curPair, *tmp);

					/*  Add an ellipses at the head of the subexpression argument list. */
					/*  Note: This indicates to make the Dictionary 'unsorted as is'. */
                
					tmp->Tag = TYELLIPSES;
					tmp->u.Int = NIL;
					}
				else
				if (strncmp((char*)&atHMChar(hSrc,hNdx + 1),"dic|",4) == 0) 
					{
					/*  We recognize an Dictionary constant: #{dic| a 1 b 2} */
					/*  Insert a TYPCODE to cause creation of a Directory constant */
                
					/*  Step to the next character */

					hNdx+=5;
                
					/*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
					_FLisp_Push(stack, stackNdx, quoteNdx, curPair);
					quoteNdx = 0;

					/*  Queue a makeDirectory call at the car of the subexpression list. */
                
					asTag(tmp) = TYPCODE;
					asShort(tmp) = _FLisp_MakeDictionary;
					}
				else
					{
					/*  We recognize a Structure constant: #{ 'a 1 'b 2} */
					/*  Insert a TYPCODE to cause creation of a Structure constant */
                
					/*  Step to the next character */

					hNdx++;
                
					/*  Save end of current expression and push. Init quoteNdx for the new expression. */
                
					_FLisp_Push(stack, stackNdx, quoteNdx, curPair);
					quoteNdx = 0;

					/*  Queue a makeStructure call at the car of the subexpression list. */
                
					asTag(tmp) = TYPCODE;
					asShort(tmp) = _FLisp_MakeStructure;
					}
                }
            else
            if (((&atHMChar(hSrc,0))[hNdx] == 't') && (!ISSYMBOL((NUM)((&atHMChar(hSrc,0))[hNdx+1])))) 
                {
                /*  We recognize the constant for TRUE: #t */
                
                asBool(tmp) = TRUE;
                asTag(tmp) = TYBOLE;
                ++hNdx;
                }
            else
            if (((&atHMChar(hSrc,0))[hNdx] == 'f') && (!ISSYMBOL((NUM)((&atHMChar(hSrc,0))[hNdx+1])))) 
                {
                /*  We recognize the constant for FALSE: #f */
                
                asBool(tmp) = FALSE;
                asTag(tmp) = TYBOLE;
                ++hNdx;
                }
            else
            if (((&atHMChar(hSrc,0))[hNdx]== 'v')       &&
                ((&atHMChar(hSrc,0))[hNdx+1] == 'o')    &&
                ((&atHMChar(hSrc,0))[hNdx+2] == 'i')    &&
                ((&atHMChar(hSrc,0))[hNdx+3] == 'd')) 
                {
                /*  We recognize the constant for void: #void */
                
                *tmp = gCP->Tval_VOID;
                hNdx += 4;
                }
            else
            if ((&atHMChar(hSrc,0))[hNdx] == '\\') 
                {
                /*  Manage special sequences for aliased char codes which begin with '\\'   */
                
                 ++hNdx;
                
                if (
                ((&atHMChar(hSrc,0))[hNdx] == 'b')  &&
                ((&atHMChar(hSrc,0))[hNdx+1] == 'a')    &&
                ((&atHMChar(hSrc,0))[hNdx+2] == 'c')    &&
                ((&atHMChar(hSrc,0))[hNdx+3] == 'k')    &&
                ((&atHMChar(hSrc,0))[hNdx+4] == 's')    &&
                ((&atHMChar(hSrc,0))[hNdx+5] == 'p')    &&
                ((&atHMChar(hSrc,0))[hNdx+6] == 'a')    &&
                ((&atHMChar(hSrc,0))[hNdx+7] == 'c')    &&
                ((&atHMChar(hSrc,0))[hNdx+8] == 'e')    
                ) 
                    {
                    /*  We recognize the constant for backspace: #\backspace */
                    
                    asTag(tmp) = TYCHAR;
                    asChar(tmp) = 0x08;
                    hNdx += 9;
                    }
                else
                if (
                ((&atHMChar(hSrc,0))[hNdx] == 'e')  &&
                ((&atHMChar(hSrc,0))[hNdx+1] == 's')    &&
                ((&atHMChar(hSrc,0))[hNdx+2] == 'c')    &&
                ((&atHMChar(hSrc,0))[hNdx+3] == 'a')    &&
                ((&atHMChar(hSrc,0))[hNdx+4] == 'p')    &&
                ((&atHMChar(hSrc,0))[hNdx+5] == 'e')    
                ) 
                    {
                    /*  We recognize the constant for escape: #\escape */
                    
                    asTag(tmp) = TYCHAR;
                    asChar(tmp) = 0x1b;
                    hNdx += 6;
                    }
                else
                if (
                ((&atHMChar(hSrc,0))[hNdx] == 'n')  &&
                ((&atHMChar(hSrc,0))[hNdx+1] == 'e')    &&
                ((&atHMChar(hSrc,0))[hNdx+2] == 'w')    &&
                ((&atHMChar(hSrc,0))[hNdx+3] == 'l')    &&
                ((&atHMChar(hSrc,0))[hNdx+4] == 'i')    &&
                ((&atHMChar(hSrc,0))[hNdx+5] == 'n')    &&
                ((&atHMChar(hSrc,0))[hNdx+6] == 'e')    
                ) 
                    {
                    /*  We recognize the constant for newline: #\newline */
                    
                    asTag(tmp) = TYCHAR;
                    asChar(tmp) = 0x0a;
                    hNdx += 7;
                    }
                else
                if (
                ((&atHMChar(hSrc,0))[hNdx] == 'p')  &&
                ((&atHMChar(hSrc,0))[hNdx+1] == 'a')    &&
                ((&atHMChar(hSrc,0))[hNdx+2] == 'g')    &&
                ((&atHMChar(hSrc,0))[hNdx+3] == 'e')    
                ) 
                    {
                    /*  We recognize the constant for page: #\page */
                    
                    asTag(tmp) = TYCHAR;
                    asChar(tmp) = 0x0c;
                    hNdx += 4;
                    }
                else
                if (
                ((&atHMChar(hSrc,0))[hNdx] == 'r')  &&
                ((&atHMChar(hSrc,0))[hNdx+1] == 'e')    &&
                ((&atHMChar(hSrc,0))[hNdx+2] == 't')    &&
                ((&atHMChar(hSrc,0))[hNdx+3] == 'u')    &&
                ((&atHMChar(hSrc,0))[hNdx+4] == 'r')    &&
                ((&atHMChar(hSrc,0))[hNdx+5] == 'n')    
                ) 
                    {
                    /*  We recognize the constant for return : #\return */
                    
                    asTag(tmp) = TYCHAR;
                    asChar(tmp) = 0x0d;
                    hNdx += 6;
                    }
                else
                if (
                ((&atHMChar(hSrc,0))[hNdx]== 'r')   &&
                ((&atHMChar(hSrc,0))[hNdx+1] == 'u')    &&
                ((&atHMChar(hSrc,0))[hNdx+2] == 'b')    &&
                ((&atHMChar(hSrc,0))[hNdx+3] == 'o')    &&
                ((&atHMChar(hSrc,0))[hNdx+4] == 'u')    &&
                ((&atHMChar(hSrc,0))[hNdx+5] == 't')    
                ) 
                    {
                    /*  We recognize the constant for rubout : #\rubout */
                    
                    asTag(tmp) = TYCHAR;
                    asChar(tmp) = 0x7f;
                    hNdx += 6;
                    }
                else
                if (
                ((&atHMChar(hSrc,0))[hNdx]== 's')   &&
                ((&atHMChar(hSrc,0))[hNdx+1] == 'p')    &&
                ((&atHMChar(hSrc,0))[hNdx+2] == 'a')    &&
                ((&atHMChar(hSrc,0))[hNdx+3] == 'c')    &&
                ((&atHMChar(hSrc,0))[hNdx+4] == 'e')
                ) 
                    {
                    /*  We recognize the constant for space : #\space */
                    
                    asTag(tmp) = TYCHAR;
                    asChar(tmp) = 0x20;
                    hNdx += 5;
                    }
                else
                if (
                ((&atHMChar(hSrc,0))[hNdx]== 't')   &&
                ((&atHMChar(hSrc,0))[hNdx+1] == 'a')    &&
                ((&atHMChar(hSrc,0))[hNdx+2] == 'b')    
                ) 
                    {
                    /*  We recognize the constant for tab : #\tab */
                    
                    asTag(tmp) = TYCHAR;
                    asChar(tmp) = 0x09;
                    hNdx += 3;
                    }
                else
                    {
                    /*  We recognize the constant for an arbitrary character constant: #\c */
                    
                    asTag(tmp) = TYCHAR;
                    asChar(tmp) = (&atHMChar(hSrc,0))[hNdx];
                    ++hNdx;
                    }
                }
            else
                {
                /*  We attempt to see if this is a date constant: #Feb,23,45BC */
                
                /*  Since FProcedure_recDate will increment hNdx, we will save before calling */
                /*  to allow us to recover if this is not a cell or range (although it is  */
                /*  expected to be.) */
                
                savNdx = --hNdx;
                *tmp = FProcedure_recDate(gCP,gTP,hSrc,&hNdx,ret);
                if ( asTag(tmp) == TYDATE)
                    break;
                    
        
                Unknown:
                    /*  We have encountered a syntax starting with a char which we do not recognize. */
                    
                    ++hNdx;
                    goto Bad;
                }
        break;
        
        case ';':
            /*  AIS Lisp supports comment constants.  A comment is any series of AIS Lisp  */
            /*  characters or symbols, preceded by a semicolon (;), and terminating at the end  */
            /*  of the line on which the comment appears.  */
            
            /*  We always ignore comments, but save them for the source vector. */
            
            while ((tmpChar = (&atHMChar(hSrc,0))[hNdx]) != 0)
                {
                if (ISLINEBREAK(tmpChar))
                    {
                    bInsertBrk = TRUE;
                    break;
                    }
                hNdx++;
                }
            
            /*  There is no data to add associated with a comment. */
            
            bAddAtom = FALSE;
        break;
        
        case ':':
            if ((&atHMChar(hSrc,0))[hNdx]== '=')
                {
                /* if we are in a quoted symbol, copy the string */
                if (quoteCnt > 0)
                    {
                    *tmp = TSYMBOL(":=");
                    hNdx++;
                    bAddAtom = TRUE;
                   }
                else
                    /*  Otherwise, drop it */
                    {              
                    hNdx++;
                    bAddAtom = FALSE;
                    }
                }
            else
                {
            	/* In order to support the Brick field value form: Field[Index]:Value */
            	if ((hNdx > 1) && (&atHMChar(hSrc,0))[hNdx-2]== ']')
					{
            		/* we're just dropping it since it's not significant in the processing */
            		/* it's only useful for reading */
            		bAddAtom = FALSE;
					}
            	else
					{
            		/*  An unrecognized syntax is an error. */
            		goto Unknown;
					}
                }
        break;
                                 
        case '!':
            /*  AIS Lisp supports error message constants. The exclamation character '!' is  */
            /*  used as a way of indicating error constants.  AIS Lisp error messages are  */
            /*  case-sensitive, may include blanks but not special characters  */
            /*  and must be delimited between two exclamation characters */

            
            for(anInt = 0;  ((&atHMChar(hSrc,0))[hNdx] != 0); /*hNdx < hLen*/)
                {
                /*  We parse forward through the source, looking for the end of error constant  */
                
                if (anInt >= stringConstMaxLen)
                    {
                    /*  We are going to limit the error constant length to stringConstMaxLen.  */                  
                    goto StringTooLong;       
                    }
                else    
                if((&atHMChar(hSrc,0))[hNdx] == '!' )
                    {
                    /*  We found the end of the error constant. */
                    break;
                    }
                else    
                    {
                    /*  Stuff the character into the error constant buffer. */                  
                    stringBuf[anInt++] = (&atHMChar(hSrc,0))[hNdx++];
                    }
                }
             
            /*  We will null terminate the string constant. */
            
            stringBuf[anInt]   = 0;
            
            if ((&atHMChar(hSrc,0))[hNdx] == '!')
                ++hNdx;
                            
            /*  Create an error object */            
            *tmp = FSmartbase_Error(gCP, gTP, stringBuf);
        break;

        
        case '\'':
			if  ((&atHMChar(hSrc,0))[hNdx] == '{')
				/*  We have encountered something which is a quoted string indicator. */
				/*  At this point we will simply treat the entire remaining source as  */
				/*  a string constant. There is no length limit to a quoted string constant. */
				/*  It is critical that we trap this as a special case because this usage */
				/*  allows the Master Client to send a source command which captures an arbitrary  */
				/*  string constant, as long as the entire trailing portion of the source */
				/*  command is treated as a string constant. */
				{
                hNdx++;
				stringPtr = &atHMChar(hSrc,hNdx);

				/*  Now we will use TObject_CnvFromText to either create a TYTEXT or a TYSTRING  */
				/*  depending on the length of the trailing text. The string constant is pushed */
				/*  onto the stack automatically at the end of this switch statement. */
            
				*tmp = TObject_CnvFromText(gCP,gTP,stringPtr);
				hNdx += strlen(stringPtr);
				}
			else
				{
				/*  We have encountered something which is quoted with the single quote shorthand. */
				/*  At this point we will simply count the number of quotes and keep track of it  */
				/*  for later use when we find out what was being quoted. */
				/*  It is critical that we trap this as a special case because this usage is unique */
				/*  to LISP and the symbol recognition routine will try to treat quoted symbols  */
				/*  just like long symbols. (See the '|' case .) */
            
				anInt = 1;
				while ((&atHMChar(hSrc,0))[hNdx] == '\'')
					{
					hNdx++;
					anInt++;
					}
            
				quoteNdx += anInt;
                
				/*  There is no data to add associated with shorthand quote recognition. */
            
				bAddAtom = FALSE;
				}
        break;
        
        case '{':
            /*  AIS Lisp supports string constants. The double left brace '{' is used as  */
            /*  a way of indicating string constants.  AIS Lisp string constants are  */
            /*  case-sensitive, may include blanks, special characters, and balanced imbedded  */
            /*  right brace characters. */
            
            for(tmpInt = 1, anInt = 0;  ((&atHMChar(hSrc,0))[hNdx] != 0); /*hNdx < hLen*/)
                {
                /*  We parse forward through the source, looking for the end of string and taking */
                /*  care to balance any left brace characters with right brace characters. */
                
                if (anInt >= stringConstMaxLen)
                    {
                    /*  The string constant is too long.  */
                    
                    goto StringTooLong;       
                    }
                else    
                if ((&atHMChar(hSrc,0))[hNdx] == '{' )
                    {
                    /*  The left brace character '{' must be balanced with a right brace.  */
                    
                    tmpInt++;
                    stringBuf[anInt++] = (&atHMChar(hSrc,0))[hNdx++];       
                    }
                else    
                if (((&atHMChar(hSrc,0))[hNdx] == '}') && (!(--tmpInt)))
                    {
                    /*  String constants are terminated with a right brace '}'. */
                    
                    break;
                    }
                else    
                    {
                    /*  We add a non-escaped character to the string constant. */
                    
                    stringBuf[anInt++] = (&atHMChar(hSrc,0))[hNdx++];
                    }
                }
             
            /*  We will null terminate the string constant. */
            
            stringBuf[anInt]   = 0;
            
            if ((&atHMChar(hSrc,0))[hNdx] == '}')
                ++hNdx;
                            
            /*  Now we will use TObject_CnvFromText to either create a TYTEXT or a TYSTRING  */
            /*  depending on the length of the recognized text. */
            
            *tmp = TObject_CnvFromText(gCP, gTP, stringBuf);
        break;

        case '\\':
        
        case '`':
            /*  We have encountered a syntax starting with a char which we do not recognize, or */
            /*  expect at this time. So we format the data as an error constant and insert it */
            /*  in the parse tree. */
 			stringPtr = &atHMChar(hSrc,hNdx-1);
			*tmp = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!%c!",stringPtr);  
			break;
        
        /*  Manage string constant */
        
        case '"':
            /*  AIS Lisp supports string constants. The double quote character '"' is used as  */
            /*  a way of indicating string constants.  AIS Lisp string constants are  */
            /*  case-sensitive, may include blanks, special characters, but not imbedded  */
            /*  double quote characters. */
            
            for(anInt = 0;  ((&atHMChar(hSrc,0))[hNdx] != 0); /*hNdx < hLen*/)
                {
                /*  We parse forward through the source, looking for the end of string and taking */
                /*  care to process any "escaped" characters. */
                
                if (anInt >= stringConstMaxLen)
                    {
                    /*  The string constant is too long.  */
                    
                    goto StringTooLong;       
                    }
                else    
                if(((&atHMChar(hSrc,0))[hNdx] == '\\' ) && ((&atHMChar(hSrc,0))[hNdx+1] != 0))
                    {
                    /*  The backslash character '\' is used as a way of accepting any character  */
                    /*  as alphabetic within a string constant and in other special situations. */
                    
                    hNdx++;
                    stringBuf[anInt++] = (&atHMChar(hSrc,0))[hNdx++];       
                    }
                else    
                if((&atHMChar(hSrc,0))[hNdx] == '"' )
                    {
                    /*  String constants are terminated with a matching double quote '"'. */
                    
                    break;
                    }
                else    
                    {
                    /*  We add a non-escaped character to the string constant. */
                    
                    stringBuf[anInt++] = (&atHMChar(hSrc,0))[hNdx++];
                    }
                }
             
            /*  We will null terminate the string constant. */
            
            stringBuf[anInt]   = 0;
            
            if ((&atHMChar(hSrc,0))[hNdx] == '"')
                ++hNdx;
                            
            /*  Now we will use TObject_CnvFromText to either create a TYTEXT or a TYSTRING  */
            /*  depending on the length of the recognized text. */
            
            *tmp = TObject_CnvFromText(gCP, gTP, stringBuf);
        break;

        case '|':
            /*  AIS Lisp supports long symbol constants with imbedded blanks and special */
            /*  characters. The bar character '|' is used as a way of indicating long symbol  */
            /*  constants.  AIS Lisp long symbol constants are case-sensitive, may include  */
            /*  blanks, special characters, but not imbedded bar characters. */
            
            goto RecognizeSymbol;
        break;

        /*  Manage period special character */
        
        case '.':           
            if ((ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx - 2]) || 
                 ((&atHMChar(hSrc,0))[hNdx - 2] == '|') || 
                 ((&atHMChar(hSrc,0))[hNdx - 2] == ']') || 
                 ((&atHMChar(hSrc,0))[hNdx - 2] == '"'))  && 
                _FLisp_CVRType(asTag(&curPair->itsCar)) && hNdx > 1)
                {
                /*  AIS Lisp supports any number of symbols, each separated by the period '.'  */
                /*  character, in a compound variable reference.   */
                
                /*  We check to see what preceeded this '.' char. We check the text stream to */
                /*  see if it looks like it was a symbol or string, and then we check the parse */
                /*  tree to see if it was a data type that could be used in a compound variable  */
                /*  reference ( by checking the type of the car of the previous atom via */
                /*  the _FLisp_CVRType macro.) */
                
                /*  Example x.y is parsed as: (x  */
                /*  when we reach this code then was save the  */
                /*  x into a tval and create a new level like : ((ref */
                /*  then we add the x back so we end up with : ((ref x */
                /*  We will check at the next recognition whether we have a reasonable expression. */
                /*  I.E. we will expect either a quoted symbol or a numeric index as the next */
                /*  atom, because they would be reasonable values as indice arguments in a ref call. */
                                
                /*  Save the car of the current sub-expression and then mark the current node */
                /*  as available for the next production. */
                    
                *sav = curPair->itsCar;
                asTag(&curPair->itsCar) = TYLEX;
                curPair->itsCdr = gCP->Tval_VOID;
                    
                /*  Push down into the tree and create a new level which starts with a "ref" */
                /*  call. */
                
                /*  Insert a ( ref ..) call at the car of the new subexpression list: ((ref */

                _FLisp_Push(stack, stackNdx, 0, curPair);

				/* Save debugging information (if requested) */
				if ((dbgListLines != NIL) && (firstPairInSourceLine == NIL))
					{
					firstPairInSourceLine = curPair;
					*ret = FSmartbase_Set(gCP,gTP,3,TOBJ(dbgListLines),TOBJ(firstPairInSourceLine),TINT(sourceLineNumber));
					ExitOnError(*ret);
					}

                _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->FMacro_refSym, 0);
                _FLisp_AddAtom(stackNdx, curPair, *tmp);
                
                /*  Add back the previously saved sub-expression:   ((ref x */
                
                _FLisp_AddAtom(stackNdx, curPair, *sav);

                /*quoteNdx = (cvrChar[cvrNdx] == 0); */
                quoteNdx = 1;
                cvrChar[cvrNdx] = '.';
                bAddAtom = FALSE;
                }
            else
            if((&atHMChar(hSrc,0))[hNdx] == '.' && (&atHMChar(hSrc,0))[hNdx+1] == '.')
                {
                /*  We recognize an ellipses '...' which is used to signal an indefinite argument */
                /*  list. We do not do any semantic checking to see if this is a reasonable place */
                /*  for such an atom, i.e. with the arglist to a lambda or defun. */
                
                asTag(tmp) = TYELLIPSES;
                hNdx += 2;
                }
            else
            if (!ISDIGIT((NUM)(&atHMChar(hSrc,0))[hNdx]))
                {
                /*  We have encountered a free standing '.' period symbol which we interpret as */
                /*  indicating that we are constructing and object which has a cdr and the cdr */
                /*  is to follow. We do not do semantic checking at this time but rather insert */
                /*  a symbol (TLambda_dotcdr) to indicate during later processing that a cdr */
                /*  is to follow. */
                
                if(stackNdx)
                    {
                    /*  Insert the dotcdr symbol. */
                    
                    asTag(tmp) = TYSYMBOL;
                    asObject(tmp) = (TObject*)gCP->TLambda_dotcdr;
                    }
                else
                    {
                    /*  It would be a syntax error if the dotcdr was the first thing we */
                    /*  encountered. */
      			*err = FLisp_Error(gCP,gTP,"Lisp: Invalid CDR Value", &atHMChar(hSrc,0), hNdx, hLen);
                goto BadCleanUp;
                    }
                    
                }
            else
                {
                /*  We will attempt to recognize an numeric constants of the form ".123" */
                    
                goto RecognizeNumbers;
                }
        break;

        case '@':           
            if ((ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx - 2]) || 
                 ((&atHMChar(hSrc,0))[hNdx - 2] == '|') || 
                 ((&atHMChar(hSrc,0))[hNdx - 2] == ']') || 
                 ((&atHMChar(hSrc,0))[hNdx - 2] == '"'))  && 
                _FLisp_CVRType(asTag(&curPair->itsCar)) && hNdx > 1)
                {
                /*  AIS Lisp supports the '@' special symbol as a method for invoking a macro member. */
                
                /*  We check to see what preceeded this '.' char. We check the text stream to */
                /*  see if it looks like it was a symbol or string, and then we check the parse */
                /*  tree to see if it was a data type that could be used in a compound variable  */
                /*  reference ( by checking the type of the car of the previous atom via */
                /*  the _FLisp_CVRType macro.) */
                
                /*  Example x.y is parsed as: (x  */
                /*  when we reach this code then was save the  */
                /*  x into a tval and create a new level like : ((ref */
                /*  then we add the x back so we end up with : ((ref x */
                /*  We will check at the next recognition whether we have a reasonable expression. */
                /*  I.E. we will expect either a quoted symbol or a numeric index as the next */
                /*  atom, because they would be reasonable values as indice arguments in a ref call. */
                                
                /*  Save the car of the current sub-expression and then mark the current node */
                /*  as available for the next production. */
                    
                *sav = curPair->itsCar;
                asTag(&curPair->itsCar) = TYLEX;
                curPair->itsCdr = gCP->Tval_VOID;
                    
                /*  Push down into the tree and create a new level which starts with a "ref" */
                /*  call. */
                
                /*  Insert a ( refmacro ..) call at the car of the new subexpression list: ((ref */

                _FLisp_Push(stack, stackNdx, 0, curPair);

				/* Save debugging information (if requested) */
				if ((dbgListLines != NIL) && (firstPairInSourceLine == NIL))
					{
					firstPairInSourceLine = curPair;
					*ret = FSmartbase_Set(gCP,gTP,3,TOBJ(dbgListLines),TOBJ(firstPairInSourceLine),TINT(sourceLineNumber));
					ExitOnError(*ret);
					}

                _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->FCompile_refmacroSym, 0);
                _FLisp_AddAtom(stackNdx, curPair, *tmp);
                
                /*  Add back the previously saved sub-expression:   ((refmacro x */
                
                _FLisp_AddAtom(stackNdx, curPair, *sav);

                quoteNdx = 1;
                cvrChar[cvrNdx] = '@';
                bAddAtom = FALSE;
                }
            else
                {
                /*  We have encountered a free standing '.' period symbol which we interpret as */
                /*  indicating that we are constructing and object which has a cdr and the cdr */
                /*  is to follow. We do not do semantic checking at this time but rather insert */
                /*  a symbol (TLambda_dotcdr) to indicate during later processing that a cdr */
                /*  is to follow. */
 
				*err = FLisp_Error(gCP,gTP,"Lisp: Invalid @ macro reference", &atHMChar(hSrc,0), hNdx, hLen);
                goto BadCleanUp;
                }
        break;

        /* Manage the ^ special character */

        case '^':
            /*  AIS Lisp supports the automatic specification of global variables */
            /*  in contexts where there are conflicts with local variables. AIS Lisp  */
            /*  supports global variable references by name for all currently active  */
            /*  global variables.  The special character ^ is followed immediately by a string  */
            /*  or symbol representing a global variable name.  After the reference, */
            /*  the value for the specified global variable is returned. The ^ special  */
            /*  character is a lexical form which causes the getGlobalValue function */
            /*  to be invoked.  For example: */
            /*      ^foo   is equivalent to    (getGlobalValue |foo|:) */
            
            savNdx = hNdx - 1;
            
            if((&atHMChar(hSrc,0))[hNdx] == '"')
                {
                /*  ^"SS1" translates to (getGlobalValue  SS1:) */
                
                hNdx++;
                *ret = FProcedure_recString(gCP,gTP,hSrc,&hNdx,tmp);
                if(asTag(ret) == TYSTRING || asTag(ret) == TYTEXT || asTag(ret) == TYSTRINGSUBSTR)
                    {
                    *ret = TSymbol_SymbolAnyCnv(gCP,gTP,TYSTRING,*tmp);
                    aTSymbol = (TSymbol*)asObject(ret);
                    }
                }
            else
                {
                /*  ^SS1 translates to (getGlobalValue  SS1:) */

                *ret = FProcedure_recName(gCP,gTP,hSrc,&hNdx,&aTSymbol,FALSE);
                if(asBool(ret) == TRUE)
                    {
                    /*  In order to use common code below for both the symbol and the string */
                    /*  syntax for this construction we will always build a TSymbol to hold the */
                    /*  intermediate value. */
                    
                    asTag(ret) = TYSYMBOL;
                    asObject(ret) = (TObject*)aTSymbol;
                    }
 				else
                /*  ^ translates to |^| */
					{
					goto RecognizeSymbol;
					}
               }

                /* Replace ^Name with (getGlobalValue Name:) */
                bAddAtom = FALSE;
                            
                /*  Save end of current expression, push, and insert a call to ref. */
            
                _FLisp_Push(stack, stackNdx, quoteNdx, curPair);

				/* Save debugging information (if requested) */
				if ((dbgListLines != NIL) && (firstPairInSourceLine == NIL))
					{
					firstPairInSourceLine = curPair;
					*ret = FSmartbase_Set(gCP,gTP,3,TOBJ(dbgListLines),TOBJ(firstPairInSourceLine),TINT(sourceLineNumber));
					ExitOnError(*ret);
					}

                /*  Reset the quotendx for this level. */
            
                quoteNdx = 0;
                        
                _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->FDebug_getGlobalValue, 0 );
                _FLisp_AddAtom(stackNdx, curPair, *tmp);
            
                asTag(tmp) = TYQUOTEDSYMBOL;
                asObject(tmp) = (TObject*)aTSymbol;
                asQuoteCnt(tmp) = 1;
                _FLisp_AddAtom(stackNdx, curPair, *tmp);
            
                /* pop out of the (ref expression */
            
                _FLisp_Pop(stack, stackNdx, curPair);
            break;

        /*  Manage numeric constants and special macros */
        
        case '-':
			if (extendedLex == 2) goto RecognizeSymbol;
            if(((&atHMChar(hSrc,0))[hNdx] == '1' && (&atHMChar(hSrc,0))[hNdx+1] == '+') ||
            (&atHMChar(hSrc,0))[hNdx] == '-')
                {
                /*  We recognize the 1- symbol */
                
                goto RecognizeSymbol;
                }
            else
                goto RecognizeNumbers;
        break;
        
        case '1':
		    if (extendedLex == 2) goto RecognizeNumbers;
            if((&atHMChar(hSrc,0))[hNdx] == '+')
                {
                /*  We recognize the 1+ symbol */
                
                goto RecognizeSymbol;
                }
            else
                goto RecognizeNumbers;
        break;
        
        case '$':
            savNdx = hNdx - 1;
            *ret = FProcedure_recNumber(gCP,gTP,hSrc,&hNdx,&aReal,&aNumber);
            if (asBool(ret) != TRUE) 
                {
                hNdx = savNdx + 1;
                goto RecognizeSymbol;
                }
            tmp->Tag = TYMONEY;
            tmp->u.Real = aReal;
        break;
        
        case '0':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            RecognizeNumbers:
            anInt = hNdx - 1;
            *ret = FProcedure_recNumber(gCP,gTP,hSrc,&anInt,&aReal,&aNumber);
            if (asBool(ret) == TRUE)
                {
                if (ISSYMBOL((NUM)(&atHMChar(hSrc,0))[anInt]))
                    {
                    if (extendedLex != 2) goto RecognizeSymbol;
                    }
                hNdx = anInt;
                *tmp = aNumber;

				/*  Is this symbol quoted? */
				if ((&atHMChar(hSrc,0))[hNdx] == ':' )
					{
					/*  Setup normal label: notation by setting the quote counter. */
            
					hNdx++;
					quoteNdx++;
            
					/* Create an atom from the number and add to parse tree */
        
					_FLisp_AddAtom(stackNdx, curPair, *tmp);
					quoteNdx = 0;
					bAddAtom = FALSE;
					}
                }

			else			
                goto RecognizeSymbol; 
        break;

        
        /*  Manage special constants starting with 'a'. */
        
        case 'a':
            if (((&atHMChar(hSrc,0))[hNdx]!= 'r')       ||
                ((&atHMChar(hSrc,0))[hNdx+1] != 'g')    ||
                ((&atHMChar(hSrc,0))[hNdx+2] != 's')    ||
                ((&atHMChar(hSrc,0))[hNdx+3] != ':')    ||
                (ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+4]))) 
                goto RecognizeSymbol;
            _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->TLambda_args, quoteNdx+1);
            hNdx += 4;
        break;
                                                
        /*  Manage special constants starting with 'b'. */
        
        case 'b':
            if ((quoteNdx != 0) || (quoteCnt != 0)) goto RecognizeSymbol;
            if (((&atHMChar(hSrc,0))[hNdx]!= 'e')       ||
                ((&atHMChar(hSrc,0))[hNdx+1] != 'g')    ||
                ((&atHMChar(hSrc,0))[hNdx+2] != 'i')    ||
                ((&atHMChar(hSrc,0))[hNdx+3] != 'n')    ||
                (ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+4]))) 
                goto RecognizeSymbol;
            _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->FCompile_beginSym, quoteNdx);
            hNdx += 4;
        break;

        case 'c':
            if (((&atHMChar(hSrc,0))[hNdx]!= 'v')       ||
                ((&atHMChar(hSrc,0))[hNdx+1] != 'a')    ||
                ((&atHMChar(hSrc,0))[hNdx+2] != 'r')    ||
                ((&atHMChar(hSrc,0))[hNdx+3] != 's')    ||
                ((&atHMChar(hSrc,0))[hNdx+4] != ':')    ||
                (ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+4]))) 
                goto RecognizeSymbol;
                
            _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->TLambda_cvars, quoteNdx+1);
            hNdx += 5;
        break;
                                 
        case 'd':
            if (((&atHMChar(hSrc,0))[hNdx+1] != 'o')    ||
                ((&atHMChar(hSrc,0))[hNdx+2] != 'o')    ||
                ((&atHMChar(hSrc,0))[hNdx+3] != 'm')    ||
                ((&atHMChar(hSrc,0))[hNdx+4] != 'e')    ||
                ((&atHMChar(hSrc,0))[hNdx+5] != 'd')    ||
                ((&atHMChar(hSrc,0))[hNdx+6] != ':')    ||
                (ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+6]))) 
                goto RecognizeSymbol;
                
            _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->TLambda_Doomed, quoteNdx+1);
            hNdx += 7;
        break;
                                         
        /*  Manage special constants starting with 'e'. */
        
        case 'e':
            if ((quoteNdx != 0) || (quoteCnt != 0)) goto RecognizeSymbol;
            if (((&atHMChar(hSrc,0))[hNdx] == 'n')  &&
                ((&atHMChar(hSrc,0))[hNdx+1] == 'd')    &&
                (! ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+2]))) 
                {
                _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->TLambda_end, quoteNdx);
                hNdx += 2;
                }
            else
            if (((&atHMChar(hSrc,0))[hNdx] == 'l')  &&
                ((&atHMChar(hSrc,0))[hNdx+1] == 's')    &&
                ((&atHMChar(hSrc,0))[hNdx+2] == 'e')    &&
                (! ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+3]))) 
                {
                _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->TLambda_else, quoteNdx);
                hNdx += 3;
                }
            else
                goto RecognizeSymbol;
        break;
                                         
        /*  Manage special constants starting with 'f'. */
        
        case 'f':
            if (((&atHMChar(hSrc,0))[hNdx]!= 'a')       ||
                ((&atHMChar(hSrc,0))[hNdx+1] != 'l')    ||
                ((&atHMChar(hSrc,0))[hNdx+2] != 's')    ||
                ((&atHMChar(hSrc,0))[hNdx+3] != 'e')    ||
                (ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+4]))) 
                goto RecognizeSymbol;
            asTag(tmp) = TYBOLE;
            asBool(tmp) = FALSE;
            hNdx += 4;
        break;
                                 
        case 'p':
            if (((&atHMChar(hSrc,0))[hNdx]!= 'v')       ||
                ((&atHMChar(hSrc,0))[hNdx+1] != 'a')    ||
                ((&atHMChar(hSrc,0))[hNdx+2] != 'r')    ||
                ((&atHMChar(hSrc,0))[hNdx+3] != 's')    ||
                ((&atHMChar(hSrc,0))[hNdx+4] != ':')    ||
                (ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+4]))) 
                goto RecognizeSymbol;
                
            _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->TLambda_pvars, quoteNdx+1);
            hNdx += 5;
        break;
                                 
        
        case 'r':
            if (((&atHMChar(hSrc,0))[hNdx]!= 'v')       ||
                ((&atHMChar(hSrc,0))[hNdx+1] != 'a')    ||
                ((&atHMChar(hSrc,0))[hNdx+2] != 'r')    ||
                ((&atHMChar(hSrc,0))[hNdx+3] != 's')    ||
                ((&atHMChar(hSrc,0))[hNdx+4] != ':')    ||
                (ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+4]))) 
                goto RecognizeSymbol;
                
            _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->TLambda_rvars, quoteNdx+1);
            hNdx += 5;
        break;
                                 
        
        case 's':
            if (((&atHMChar(hSrc,0))[hNdx]!= 'v')       ||
                ((&atHMChar(hSrc,0))[hNdx+1] != 'a')    ||
                ((&atHMChar(hSrc,0))[hNdx+2] != 'r')    ||
                ((&atHMChar(hSrc,0))[hNdx+3] != 's')    ||
                ((&atHMChar(hSrc,0))[hNdx+4] != ':')    ||
                (ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+4]))) 
                goto RecognizeSymbol;
                
            _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->TLambda_svars, quoteNdx+1);
            hNdx += 5;
        break;
                                 
        
        /*  Manage special constants starting with 't'. */
        
        case 't':
            if (((&atHMChar(hSrc,0))[hNdx]!= 'r')       ||
                ((&atHMChar(hSrc,0))[hNdx+1] != 'u')    ||
                ((&atHMChar(hSrc,0))[hNdx+2] != 'e')    ||
                (ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+3]))) 
                goto RecognizeSymbol;
            asTag(tmp) = TYBOLE;
            asBool(tmp) = TRUE;
            hNdx += 3;
        break;
                                 
        case 'v':
            if (((&atHMChar(hSrc,0))[hNdx]!= 'a')       ||
                ((&atHMChar(hSrc,0))[hNdx+1] != 'r')    ||
                ((&atHMChar(hSrc,0))[hNdx+2] != 's')    ||
                ((&atHMChar(hSrc,0))[hNdx+3] != ':')    ||
                (ISSYMBOL((NUM)(&atHMChar(hSrc,0))[hNdx+4]))) 
                goto RecognizeSymbol;
            _FLisp_MakeSymbolTval(tmp, (TObject*)gCP->TLambda_vars, quoteNdx+1);
            hNdx += 4;
        break;

        /*  Manage white space in source string */
        case 0:
        case 10:
        case 13:
            /*  Make sure we save any trailing source before the next line break. */
            /*  Note: This is assuming we are saving source. */
            bInsertBrk = TRUE;
            if ((theProc != NIL) && (vNdx < hNdx) && (gTP->FCompile_GenerateDebugInfo == TRUE))
                {
				if (dbgSourceLines != NIL)
					{
					stringPtr = &atHMChar(hSrc,0);
					FLisp_SaveSrcText(gCP, gTP, dbgSourceLines, vNdx, hNdx, hSrc);
					sourceLineNumber = dbgSourceLines->itsMaxItemIndex;
					firstPairInSourceLine = NIL; 
					tmpChar = (&atHMChar(hSrc,0))[hNdx];
					if (curChar == 0) {vNdx = hNdx; goto CleanUp;}
					else
					if ((curChar == RETURNCHR) && ((&atHMChar(hSrc,0))[hNdx]== 0)) {++hNdx; vNdx = hNdx; goto CleanUp;}
					else
					if ((curChar == LINEBREAK) && ((&atHMChar(hSrc,0))[hNdx]== 0)) {++hNdx; vNdx = hNdx; goto CleanUp;}
					else
					if ((curChar == RETURNCHR) && ((&atHMChar(hSrc,0))[hNdx]== LINEBREAK)) {++hNdx; vNdx = hNdx;}
					else
					if ((curChar == LINEBREAK) && ((&atHMChar(hSrc,0))[hNdx]== RETURNCHR)) {++hNdx; vNdx = hNdx;}
					else vNdx = hNdx;
					}
				else
					vNdx = hNdx;
                }
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 6:
        case 7:
        case 8:
        case 9:
        case 11:
        case 12:
        case 14:
        case 15:
        case 16:
        case 17:
        case 18:
        case 19:
        case 20:
        case 21:
        case 22:
        case 23:
        case 24:
        case 25:
        case 26:
        case 27:
        case 28:
        case 29:
        case 30:
        case 31:
        case 32:
        case ',':
            bAddAtom = FALSE;
        break;
        
        
        default:

        RecognizeSymbol:
        
		/* In arithmetic mode all operator symbols are grouped together. */
		if ((extendedLex == 2) && (strchr("!@#$%^&*-=+|\\:<>?/",curChar) != NULL))
			{
		    /*  Recognize all of the grouped operators. */
            stringBuf[0] = curChar;
            anInt = 1;
            stringBuf[anInt] = 0;
            while ((tmpChar = (&atHMChar(hSrc,0))[hNdx]) &&
                   (strchr("!@#$%^&*-=+|\\:<>?/",tmpChar) != NULL))
                {
                if (anInt > 1020) goto NameTooLong;
                stringBuf[anInt++] = tmpChar;
                stringBuf[anInt] = 0;
                hNdx++;
                }
            
            /* Create a symbol from the grouped operators. */
            *tmp = TSYMBOL(stringBuf);
            _FLisp_AddAtom(stackNdx, curPair, *tmp);
			bAddAtom = FALSE;
			break;
			}
        else
		/* In standard lisp mode, operator symbols are mixed with all other symbols. */
			{
			--hNdx;
			*ret = FProcedure_recName(gCP,gTP,hSrc,&hNdx,&aTSymbol, (BOLE)(extendedLex == 2));
			if ((asBool(ret) == TRUE))
				{
				/*  Is this symbol a goto label declaration? */
				if (((&atHMChar(hSrc,0))[hNdx] == ':' ) &&
					((&atHMChar(hSrc,0))[hNdx + 1] == ':' ))
					{
					/*  Setup a goto label:: by prefixing quoted symbol with label keyword. */
                
					hNdx += 2;
					quoteNdx++;
					_FLisp_MakeSymbolTval(tmp, (TObject*)gCP->TLambda_label, quoteNdx);
					asTag(tmp) = TYLABELSYMBOL;
					_FLisp_AddAtom(stackNdx, curPair, *tmp);
					}
				else

				/*  Is this symbol quoted? */
				if ((&atHMChar(hSrc,0))[hNdx] == ':' )
					{
					/*  Setup normal label: notation by setting the quote counter. */
                
					hNdx++;
					quoteNdx++;
					}
                                
				/*  Add a symbol, quoting as required. */
            
				_FLisp_MakeSymbolTval(tmp, (TObject*)aTSymbol, quoteNdx);
				quoteNdx = 0;

				if (bAddAtom)
					{
					/*  Add the symbol on the parse tree (if requested). */
					_FLisp_AddAtom(stackNdx, curPair, *tmp);
					bAddAtom = FALSE;
					}
                                
				if(aTSymbol == gCP->FMacro_quoteSym)
					{
					/*  Do special processing for the "quote" typographic symbol. */
                
					if(stackNdx)
						{
						/*  We must bump the quote cnt on the next guy or we will */
						/*  not be able to correctly process nested info */
                    
						quoteNdx = 1;
						}
					else
						{
						/*  error ... quote detected outside of parenthesis   */
      					*err = FLisp_Error(gCP,gTP,"Lisp: Invalid quoted symbol", &atHMChar(hSrc,0), hNdx, hLen);
						goto BadCleanUp;
						}
					}
				else
					quoteNdx = 0;
                
				if (cvrChar[cvrNdx] == '.')
					{
					/*  We are terminating a compound variable reference. */
                
					_FLisp_Pop(stack, stackNdx, curPair);
                
					cvrChar[cvrNdx] = 0;
					}
                
				if (cvrChar[cvrNdx] == '@')
					{
					/*  We are terminating a compound macro reference. */
                
					_FLisp_Pop(stack, stackNdx, curPair);
                
					cvrChar[cvrNdx] = 0;
					}
                
				bAddAtom = FALSE;
			} /* end RecognizeName if */
			else
				{
				Bad:
      			*err = FLisp_Error(gCP,gTP,"Lisp: unrecognized input, expecting name or symbol", &atHMChar(hSrc,0), hNdx, hLen);
				goto BadCleanUp;
				}
			} /* end RecognizeSymbol else */
			break;
			
		}  /* end while */
        
    if(bAddAtom)
        {
        /*  Do chaining of atoms and pairs on the current list */
        _FLisp_AddAtom(stackNdx, curPair, *tmp);
        
        }
    }

/*  Termination */

CleanUp:

if (stackNdx == 0)
    {
    /*  sweep through the parse tree and insert required calls to begin */
    
    curPair = basePair;

    do {
        if(asTag(&curPair->itsCar) == TYLEX)
            curPair->itsCar = gCP->Tval_VOID;
        if(isPair(&curPair->itsCdr))
            curPair = asPair(&curPair->itsCdr);
        else
            break;
        }while(asTag(&curPair->itsCdr) == TYPAIR);
        
    if ((theProc != NIL) && (gTP->FCompile_GenerateDebugInfo == TRUE))
        {
        /*  Save any remaining text to the src vector */
        
        if ((dbgSourceLines != NIL) && (vNdx < hNdx))
			{
			FLisp_SaveSrcText(gCP, gTP, dbgSourceLines, vNdx, hNdx, hSrc);
			} 
        }
        
    /* Set up the universal parse tree result */
    ret->u.Pair = basePair;
    ret->Tag = TYPAIR;
    
    /* Are we returning an Lambda with debugging information? */
	gTP->FCompile_DebugSourceVector = NIL;
	gTP->FCompile_DebugListLines = NIL;
    if (returnLambda == TRUE)
		{
		/*  Create the In for this object and attach a binding for the debug vector */
	    
		if (theProc->Interfaces == NIL)
			{
			theProc->Interfaces = tmpEnv = TStructure_New(gCP,gTP);
			}
		else
			tmpEnv = theProc->Interfaces;
	        
		argTval->u.Structure = tmpEnv;
		argTval->Tag = TYSTRUCTURE;
		TStructure_AddNewValue(gCP,gTP,*argTval,*dbgParseTreeSYM,*ret);

		ret->u.Lambda = theProc;
		ret->Tag = TYLAMBDA;
		}
    
    FrameExit(*ret);
    }
else
    {
    MatchParen:
  	*err = FLisp_Error(gCP,gTP,"Lisp: Mismatched parenthesis", &atHMChar(hSrc,0), hNdx, hLen);
    goto BadCleanUp;

    StringTooLong:
 	*err = FLisp_Error(gCP,gTP,"Lisp: String Too Long", &atHMChar(hSrc,0), hNdx, hLen);
   goto BadCleanUp;

    NameTooLong:
	*err = FLisp_Error(gCP,gTP,"Lisp: Name Too Long", &atHMChar(hSrc,0), hNdx, hLen);
	goto BadCleanUp;
    }

BadCleanUp:

if (theProc != NIL)
    {
    /*  If the caller has passed in a procedure object to be associated */
    /*  with this lex, then we will attach a vector to this object. */
    
    /*  The decompile Procedure uses the Token Vector stored in the Sc  */
    /*  field of the Procedure argument (proc) to return the original formula  */
    /*  source string for the Procedure. AIS Lisp Procedures stored as cell  */
    /*  formulas often need to be redisplayed. For instance, any request for  */
    /*  the formula associated with a cell must return the original formula  */
    /*  source as entered by the user.  A conventional Lisp compiler,  */
    /*  like Common Lisp, does not have this requirement and discards  */
    /*  the original source after compilation.  */

    /*  Save error message text to the src vector, and update indices */
    
    TVector_AddNewValue(gCP,gTP,*srcVecTval, *err);

    /*  Save trailing text to the src vector, and update indices */
    
    hNdx = strlen((char*)*hSrc);
    FLisp_SaveSrcText(gCP, gTP, dbgSourceLines, vNdx, hNdx, hSrc); 
	sourceLineNumber = dbgSourceLines->itsMaxItemIndex;       
	firstPairInSourceLine = NIL;      
    vNdx = hNdx;
    }

_FLisp_AddAtom(stackNdx, curPair, *err);
if (isNullTval(&basePair->itsCdr) )
    {
    *ret = basePair->itsCar;
    }
else
    {
    asTag(ret) = TYPAIR;
    asObject(ret) = (TObject*)basePair;
    }

FrameExit(*err);

/* Extended lexical Analysis loop. */

ExtendedLexMode:

/*  Trap each source character, using it as a switch to guide the lexical analysis. */

bAddAtom = FALSE;

while( hNdx < hLen  )
    {
    /*  Loop through the data */
    
    /*  We make the default assumption that we will recognize an atom which we will add to the */
    /*  parse tree which we are building. */
    
    switch ((curChar = atHMChar(hSrc,hNdx++)))
        {
        /*  Use each source character as a switch to guide the lexical analysis. */
        
        case 0:
            /*  Manage end of null-terminated source string. */
            
            goto CleanUp;
        break;
                
        case 10:
        case 13:
            /*  Manage end of line characters. */
            tmpChar = (&atHMChar(hSrc,0))[hNdx];
            if ((curChar == 13) && (tmpChar == 10))
                {
                hNdx++;
                }
            
            /*  Make sure we save any trailing source before the next line break. */
            /*  Note: This is assuming we are saving source. */
            bInsertBrk = TRUE;
            if ((vNdx < hNdx) && (dbgSourceLines != NIL))
                {
                FLisp_SaveSrcText(gCP, gTP, dbgSourceLines, vNdx, hNdx, hSrc);
                vNdx = hNdx;
                }

            /*  Always push a #\newline character on the stack. */
            tmp->Tag = TYCHAR;
            tmp->u.Char = 10;
            _FLisp_AddAtom(stackNdx, curPair, *tmp);
        break;
                
        
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 6:
        case 7:
        case 8:
        case 9:
        case 11:
        case 12:
        case 14:
        case 15:
        case 16:
        case 17:
        case 18:
        case 19:
        case 20:
        case 21:
        case 22:
        case 23:
        case 24:
        case 25:
        case 26:
        case 27:
        case 28:
        case 29:
        case 30:
        case 31:
        case 32:
            /*  Ignore all of the whitespace characters. */
        break;

        
        case '[':
        case ']':
        case '(':
        case ')':
        case '{':
        case '}':
        case '.':
        case ',':
        case ';':
        case '\'':
            /*  Recognize all singleton ungrouped operators. */
            stringBuf[0] = curChar;
            stringBuf[1] = 0;
            
            /* Create a symbol from the singleton operator. */
            *tmp = TSYMBOL(stringBuf);
            _FLisp_AddAtom(stackNdx, curPair, *tmp);
        break;
        
                
        case '$':
            /*  Recognize money constant or maybe a grouped operator. */
            savNdx = hNdx - 1;
            *ret = FProcedure_recNumber(gCP,gTP,hSrc,&hNdx,&aReal,&aNumber);
            if (asBool(ret) == TRUE) 
                {
                tmp->Tag = TYMONEY;
                tmp->u.Real = aReal;
                _FLisp_AddAtom(stackNdx, curPair, *tmp);
                break;
                }

            /* If not a money constant, fall through to operator. */
            hNdx = savNdx + 1;
            goto RecognizeOperator;
            break;
        
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            /* Recognize numeric constants both Integers and Numbers. */
            anInt = hNdx - 1;
            *ret = FProcedure_recNumber(gCP,gTP,hSrc,&anInt,&aReal,&aNumber);
            if (asBool(ret) == TRUE)
                {
                hNdx = anInt;
                *tmp = aNumber;

                _FLisp_AddAtom(stackNdx, curPair, *tmp);
                break;
                }
            else
                goto Unknown; 
        break;

        case '+':
        case '-':
        case '!':
        case '#':
        case '%':
        case '^':
        case '&':
        case '*':
        case '=':
        case '|':
        case '\\':
        case ':':
        case '<':
        case '>':
        case '?':
        case '/':
            /*  Recognize all of the grouped operators. */
            RecognizeOperator:
            stringBuf[0] = curChar;
            anInt = 1;
            stringBuf[anInt] = 0;
            while ((tmpChar = (&atHMChar(hSrc,0))[hNdx]) &&
                   (strchr("!@#$%^&*-=+|\\:<>?/",tmpChar) != NULL))
                {
                if (anInt > 1020) goto NameTooLong;
                stringBuf[anInt++] = tmpChar;
                stringBuf[anInt] = 0;
                hNdx++;
                }
            
            /* Create a symbol from the grouped operators. */
            *tmp = TSYMBOL(stringBuf);
            _FLisp_AddAtom(stackNdx, curPair, *tmp);
        break;
        
                                       
        case '\"':
            /*  AIS Lisp supports string constants. The double quote character '"' is used as  */
            /*  a way of indicating string constants.  AIS Lisp string constants are  */
            /*  case-sensitive, may include blanks, special characters, but not imbedded  */
            /*  double quote characters. */
            
            for(anInt = 0;  ((&atHMChar(hSrc,0))[hNdx] != 0); /*hNdx < hLen*/)
                {
                /*  We parse forward through the source, looking for the end of string and taking */
                /*  care to process any "escaped" characters. */
                
                if (anInt >= 1020)
                    {
                    /*  The string constant is too long.  */
                    
                    goto StringTooLong;       
                    }
                else    
                if(((&atHMChar(hSrc,0))[hNdx] == '\\' ) && ((&atHMChar(hSrc,0))[hNdx+1] != 0))
                    {
                    /*  The backslash character '\' is used as a way of accepting any character  */
                    /*  as alphabetic within a string constant and in other special situations. */
                    
                    hNdx++;
                    stringBuf[anInt++] = (&atHMChar(hSrc,0))[hNdx++];       
                    }
                else    
                if((&atHMChar(hSrc,0))[hNdx] == '"' )
                    {
                    /*  String constants are terminated with a matching double quote '"'. */
                    
                    break;
                    }
                else    
                    {
                    /*  We add a non-escaped character to the string constant. */
                    
                    stringBuf[anInt++] = (&atHMChar(hSrc,0))[hNdx++];
                    }
                }
             
            /*  We will null terminate the string constant. */
            
            stringBuf[anInt]   = 0;
            
            if ((&atHMChar(hSrc,0))[hNdx] == '"')
                ++hNdx;
                            
            /*  Now we will use TObject_CnvFromText to either create a TYTEXT or a TYSTRING  */
            /*  depending on the length of the recognized text. */
            
            *tmp = TSTRING(stringBuf);
            _FLisp_AddAtom(stackNdx, curPair, *tmp);
        break;

        
        case '_':
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
        case 'g':
        case 'h':
        case 'i':
        case 'j':
        case 'k':
        case 'l':
        case 'm':
        case 'n':
        case 'o':
        case 'p':
        case 'q':
        case 'r':
        case 's':
        case 't':
        case 'u':
        case 'v':
        case 'w':
        case 'x':
        case 'y':
        case 'z':
        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
        case 'G':
        case 'H':
        case 'I':
        case 'J':
        case 'K':
        case 'L':
        case 'M':
        case 'N':
        case 'O':
        case 'P':
        case 'Q':
        case 'R':
        case 'S':
        case 'T':
        case 'U':
        case 'V':
        case 'W':
        case 'X':
        case 'Y':
        case 'Z':
            /*  Recognize all names as symbols. */
            stringBuf[0] = curChar;
            anInt = 1;
            stringBuf[anInt] = 0;
            while ((tmpChar = (&atHMChar(hSrc,0))[hNdx]) &&
                   ((tmpChar == '_') ||
                   ((tmpChar >= 'a') && (tmpChar <= 'z')) ||
                   ((tmpChar >= 'A') && (tmpChar <= 'Z')) ||
                   ((tmpChar >= '0') && (tmpChar <= '9'))))
                {
                if (anInt > 1020) goto NameTooLong;
                stringBuf[anInt++] = tmpChar;
                stringBuf[anInt] = 0;
                hNdx++;
                }
            
            /* Create a symbol from the grouped operators. */
            *tmp = TSYMBOL(stringBuf);
            _FLisp_AddAtom(stackNdx, curPair, *tmp);
        break;
        
        default:
            /*  All other unknown cases are returned as singletons. */
            stringBuf[0] = curChar;
            stringBuf[1] = 0;
            
            /* Create a symbol from the singleton operator. */
            *tmp = TSYMBOL(stringBuf);
            _FLisp_AddAtom(stackNdx, curPair, *tmp);
        break;
                                     
        } /* end switch */
        
    } /* end while */

goto CleanUp;
}

/*--------------------------------------------------------------------------------------- */
#if 0
FLisp_SaveSrcText

Add the indicated text as TYTEXT tvals to the src vector so that it will be available
for later redisplay.

#endif

TVAL    FLisp_SaveSrcText(LpXCONTEXT gCP,LpTHREAD gTP,TVector* srcVector, NUM start, NUM end, HMChar text)
{
NUM                 div;
NUM                 mod;
NUM					maxTextLine = 2000;
StartFrame
DeclareTVAL(srcVecTval);
DeclareTVAL(tmp);
DeclareTVAL(ret);
EndFrame

div = (end - start) / maxTextLine;
mod = (end - start) % maxTextLine;

*ret = gCP->TObject_TRUE;
srcVecTval->Tag = srcVector->itsObjectType;
srcVecTval->u.Vector = srcVector;

while (div > 0)
    {
    _FMemory_memcpy(gTP->TempBuffer, ((CHAR*)*text)+start, maxTextLine);
    gTP->TempBuffer[maxTextLine] = LINEBREAK;
    gTP->TempBuffer[maxTextLine+1] = 0;
    *tmp = TSTRING(gTP->TempBuffer);
    start += maxTextLine;
    div--;
    *ret = TVector_AddNewValue(gCP,gTP,*srcVecTval, *tmp);
    }
if (mod)
    {
    _FMemory_memcpy(gTP->TempBuffer, ((CHAR*)*text)+start, mod);
    gTP->TempBuffer[mod-1] = LINEBREAK;
    gTP->TempBuffer[mod] = 0;
    *tmp = TSTRING(gTP->TempBuffer);
    *ret = TVector_AddNewValue(gCP,gTP,*srcVecTval, *tmp);
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FLisp_MorphList

Traverse the parse tree, traveling to the end of each branch. As we restore the context
when reaching the end of each sub-branch we do TYMACRO and TYCMACRO replacement as 
required.

#endif

TVAL    FLisp_MorphList(LpXCONTEXT gCP,LpTHREAD gTP,TVAL theInput,TVAL altRule,TVAL altFailure)
{
NUM                 stackNdx;
NUM                 argc;
BOLE                encloseSW = FALSE;
NUM                 SizeOfFormals;
BOLE                definiteArgs;
NUM					argMax = 21;	// This must match EXACTLY the dimensions for the argv WORD array declared below!
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TObjVector,stack);
DeclareOBJ(TPair,basePair);
DeclareOBJ(TPair,curPair);
DeclareOBJ(TPair,carPair);
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(theMacro);
DeclareTVAL(theArguments);
DeclareTVAL(ret);
DeclareTVALArray(argv,21);			// These dimensions must match EXACTLY the argMax size declared above!
EndFrame

/*  Anything other than a Pair is returned as is. */
if (!isPair(&theInput))
	{
    FrameExit(theInput);
	}

basePair = asPair(&theInput);

/*  A single item List containing an atom, returns the atom as is. */
/*  ie:   '(1)  ==>  1 */
if ((basePair->itsCdr.Tag == TYVOID) && ((basePair->itsCar.Tag != TYMACRO)))
    {
    if (!isPair(&basePair->itsCar))
		{	
        FrameExit(basePair->itsCar);
		}
    }
    
/*  We convert a single level List into a List of Lists. */
/*  ie:   '(1 2 3 4)  ==>  '((1 2 3 4)) */
if (!isPair(&basePair->itsCar))
    {
    basePair = TPair_New(gCP,gTP);
    basePair->itsCar = theInput;
    basePair->itsCdr = gCP->Tval_VOID;
    encloseSW = TRUE;
    }

/*  Initialize and set up the return value. We always return */
/*  the original input List. While its component pairs may or */
/*  may not have been morphed, the top List will never be */
/*  altered (remember we always create a List of Lists). */
curPair = basePair;
asTag(ret) = TYPAIR;
asObject(ret) = (TObject*)curPair;
stackNdx = 0;
stack = TObjVector_New(gCP,gTP);
FObject_SetMaxIndex(gCP,gTP,(TObject*)stack,_FLisp_StackAlloc);

/*  Loop forever performing macro substitution on each sub list. */
while (TRUE)
    {
VisitNode:
	if (!isObjPair(curPair))
		{
        /*  We assume that curPair is a Pair. */
        /*  Note:   If curPair is not a Pair, then something */
        /*          serious has gone wrong. Perhaps the original */
        /*          input was not a proper List, or perhaps some */
        /*          other serious error has occurred. Under no */
        /*          circumstances should we continue List processing. */
		*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!macro preprocessor: Unable to morph input: %a!",&curPair);
		goto BadCleanUp;
		}
	else
    if (isPair(&curPair->itsCar))
        {
        /*  Move down */
        /*  Note:   When the head of the sub list is a list, */
        /*          we push the current context. Later we will */
        /*          return to this context on the way back up */
        /*          the list. We always process macros on the */
        /*          way up the list, because we want to process */
        /*          the bottom most sub lists first. */
        _FLisp_SaveContext(stack, stackNdx, curPair);
        }
    else
    if (isPair(&curPair->itsCdr))
        {
        /*  Move across */
        /*  Note:   When the tail of the sub list is a list, */
        /*          we make it the new top of the list and */
        /*          begin to process it. */
        if (isERROR(&curPair->itsCar))
            {
            *ret = curPair->itsCar;
            goto BadCleanUp;
            }
        else
            curPair = asPair(&curPair->itsCdr);
        }
    else
        {
        /*  Move up */
        /*  Note:   We are now at the bottom of this sub list, */
        /*          so it is time to do macro processing. The */
        /*          last context saved (see move down) is the */
        /*          first sub list on which we will attempt */
        /*          macro processing. We always macro process on */
        /*          the bottom most sub lists first. */

RestoreContext:     
        _FLisp_RestoreContext(stack, stackNdx, curPair);
		if (!isObjPair(curPair))
			{
			/*  We assume that curPair is a Pair. */
			/*  Note:   If curPair is not a Pair, then something */
			/*          serious has gone wrong. Perhaps the original */
			/*          input was not a proper List, or perhaps some */
			/*          other serious error has occurred. Under no */
			/*          circumstances should we continue List processing. */
			*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!macro preprocessor: Unable to morph input: %a!",&curPair);
			goto BadCleanUp;
			}
		else
		if (!isPair(&curPair->itsCar))
			{
			/*  We assume that curPair is a Pair. */
			/*  Note:   If curPair is not a Pair, then something */
			/*          serious has gone wrong. Perhaps the original */
			/*          input was not a proper List, or perhaps some */
			/*          other serious error has occurred. Under no */
			/*          circumstances should we continue List processing. */
			*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"!macro preprocessor: Unable to morph input: %a!",&curPair);
			goto BadCleanUp;
			}
		else
			{
			carPair = asPair(&curPair->itsCar);
			}
        
        /* We perform macro processing if the the altRule is not void. */
        /* Note:	Remember that the morph function may be used as */
        /*          a rule based production system by specifying an */
        /*          alternate substitution function. */
        if (altRule.Tag != TYVOID)
            {
            argv[0] = curPair->itsCar;
            
            *ret = FSmartbase_Evalv(gCP,gTP,altRule,1,&argv[0]);
			if (FPredicate2_QuickCompare(gCP, gTP, ret,&altFailure) == 0)
                {
                goto SkipIt;
                }
            else
            if (isERROR(ret))
                {
                goto BadCleanUp;
                }
            else
                {
                /*  Connect the morphed expression and skip this node. */
                
                curPair->itsCar = *ret;
                goto SkipIt;
                }
            }
        else
		/* We perform macro processing if the head of the sub list is a macro Lambda. */
        if (carPair->itsCar.Tag == TYMACRO)
            {
			*theMacro = carPair->itsCar;
			goto InvokeMacroLambda;
			}
		else
        if ((carPair->itsCar.Tag == TYCMACRO) && isObjPair(carPair) && isPair(&carPair->itsCdr))
            {
            asTag(&argv[0]) = TYPAIR;
            asObject(&argv[0]) = (TObject*)asPair(&carPair->itsCdr);
            
            *ret = (*((LpFUNC)carPair->itsCar.u.Pointer))(gCP, gTP, (NUM)1, &argv[0]);
            if (isERROR(ret))
                {
				*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"macro preprocessor got this error: %s", ((LpCHAR)*ret->u.Symbol->itsCString));
                goto BadCleanUp;
                }
            else
                {
                /*  Connect the morphed expression and re-evaluate the same node. */
                curPair->itsCar = *ret;
                goto VisitNode;
                }
            }
		else
        /* We perform macro processing if the head of the sub list */
        /* is symbol globally assigned to a macro function. */
        if (isLexMorphSymbol(&carPair->itsCar))
            {
            aSymbol = asSymbol(&carPair->itsCar);
                                
            /* We perform macro processing if the head of the sub list is a lisp macro. */
            if (aSymbol->itsGlobalValue.Tag == TYMACRO)
                {
                if(stackNdx)
                    {
                    /*  If the owner is a "define" TYCMACRO then do not */
                    /*  evaluate the TYMACRO. This will allow it to be redefined. */
                    tmpPair =  (TPair*)atHMObject(stack->itsObjectArray,stackNdx-1);
                    if(isObjPair(tmpPair) && isPair(&tmpPair->itsCar))
                        {
                        tmpPair = asPair(&tmpPair->itsCar);
                        if( isLexMorphSymbol(&tmpPair->itsCar) && asSymbol(&tmpPair->itsCar) == gCP->FCompile_defineSym)
                            goto SkipIt;
                        }
                    }
                

				/* Retrieve the macro Lambda so we can invoke it. */
                *theMacro = aSymbol->itsGlobalValue;
 
				/* Determine the number for formal arguments to the Macro. */
				InvokeMacroLambda:
				*theArguments = TOBJ(asProcedure(theMacro)->ArgumentVariables);
                if (theArguments->Tag != TYSTRUCTURE)
                    {
                    SizeOfFormals = 0;
                    definiteArgs = TRUE;
                    }
                else
                    {
                    SizeOfFormals = Structure(*theArguments)->itsMaxItemIndex;
                    definiteArgs = (Structure(*theArguments)->itsCdr.Tag == TYVOID);
                    }
    

                /*  Set up the argument list for the TYMACRO call. */
                /*  Note:   The formal arguments are passed individually. */
                /*          The remainder are passed as a single list. */
                tmpPair = carPair;
                argc = 0;
                if (isPair(&tmpPair->itsCdr))
                    {
                    tmpPair = asPair(&tmpPair->itsCdr);
                    if (!isNullTval(&tmpPair->itsCar))
                        {                        
                        /*  Perform error checking for argument overflow... */
                        /*  Only pass the formula arguments individually. */
                        
    					if (SizeOfFormals > 0)
							{
							do 
								{
								argv[argc++] = tmpPair->itsCar;
								if (isPair(&tmpPair->itsCdr))
									{
									tmpPair = asPair(&tmpPair->itsCdr);
									}
								else
									{
									tmpPair = NIL;
									break;
									}
								} while ((argc < argMax) && (argc < SizeOfFormals));
							}

                        /*  Were there too many formal arguments? */
                        if (argc >= argMax)
                            {							
                            *ret = TERROR("!morph: Too Many Arguments!");
							goto BadCleanUp;
							}

                        /*  Pass any remaining arguments as a list. */
                        if ((argc == SizeOfFormals) && (isObjPair(tmpPair)))
                            {
                            argv[argc].u.Object = (TObject*)tmpPair; 
                            argv[argc++].Tag = tmpPair->itsObjectType; 
                            }
                        }
                    }
                
				/* Evaluate the Macro replacing the old sub List with the morphed sub List. */
                *ret = _VmEvaluate(theMacro->u.Lambda, argc, &argv[0]);
                if(isERROR(ret))
                    {
					*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"macro preprocessor got this error: %s", ((LpCHAR)*ret->u.Symbol->itsCString));
                    goto BadCleanUp;
                    }
                else
                    {
                    /*  Connect the morphed expression and re-evaluate the same node. */
                    curPair->itsCar = *ret;
                    goto VisitNode;
                    }
                }
            else
            /* We perform macro processing if the head of the sub list is a C macro. */
            if ((asTag(&aSymbol->itsGlobalValue) == TYCMACRO) && isObjPair(carPair) && isPair(&carPair->itsCdr))
                {
                asTag(&argv[0]) = TYPAIR;
                asObject(&argv[0]) = (TObject*)asPair(&carPair->itsCdr);
                
                *ret = (*(aSymbol->itsCProcedure))(gCP, gTP, (NUM)1, &argv[0]);
                if (isERROR(ret))
                    {
					*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,"macro preprocessor got this error: %s", ((LpCHAR)*ret->u.Symbol->itsCString));
                    goto BadCleanUp;
                    }
                else
                    {
                    /*  Connect the morphed expression and re-evaluate the same node. */
                    curPair->itsCar = *ret;
                    goto VisitNode;
                    }
                }
            }
            
        /* We move on to the next sub list. */
        SkipIt:         
        if (isPair(&curPair->itsCdr))
            {
            /*  Move across */
            curPair = asPair(&curPair->itsCdr);
            }
        else
        if (stackNdx == 0)
            break;
        else
            goto RestoreContext;
        }
    }

/*  Make sure we return the original morphed input. */

if (encloseSW == TRUE)
    {
    *ret = basePair->itsCar;
    }
else
    {
    asTag(ret) = TYPAIR;
    asObject(ret) = (TObject*)basePair;
    }
    
FrameExit(*ret);

BadCleanUp:
MatchParen:				/* Label generated by _FLisp_Pop macro -- do not generate error msg. */
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FLisp_Error

Return a parsing error in an Lisp source string. The error is returned
in a standard format.

We want the error to appear as in the following example:

	[line1]: !(defun foo(x)
	[line2]:	 vars:(x
					    ^ error
	[line3]:     (+ x 1)
	[line4]:     true)!

#endif
TVAL FLisp_Error(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR errorMsg,LpCHAR inputStr, NUM curPos, NUM inputLen)
{
#define	BUFLEN		2048
#define	LBUFLEN		200

LpCHAR				INP;
NUM					i;				
NUM					j;
NUM					N;				
NUM					line1;				
NUM					line2;
NUM					line3;				
NUM					line4;				
NUM					endline1;				
NUM					endline2;				
NUM					endline3;				
NUM					endline4;				
NUM					eof;				
NUM					tabs;				
NUM					nontabs;				
NUM					pos;				
NUM					lineno;
CHAR                errorBuf[BUFLEN];
CHAR				line1buf[LBUFLEN];
CHAR				line2buf[LBUFLEN];
CHAR				line3buf[LBUFLEN];
CHAR				line4buf[LBUFLEN];
CHAR				errMgbuf[LBUFLEN];

StartFrame
DeclareTVAL(ec);
EndFrame

/*  Initialize the starting variables. */
INP = inputStr;


eof = strlen(inputStr);
eof = inputLen < eof ? inputLen : eof;
pos = curPos < eof ? curPos : eof;
pos = pos < 0 ? 0 : pos;
tabs = 0;
nontabs = 0;
lineno = 0;

/* Find the start position of line containing the error. This will be called line2 */

if (pos >= eof) 
	{
	/* Skip over the previous EOL (if any) */
	while ((pos >= 0) && ((INP[pos] == 0) || (INP[pos] == 10) || (INP[pos] == 13))) --pos;
	// pos >= eof ? eof : pos;
	pos = (pos < 0) ? 0 : pos;
	}

i = pos;
/* Skip until the previous EOL (if any) */
while ((i >= 0) && (INP[i] != 10) && (INP[i] != 13)) --i;
line2 = i + 1;
line2 = line2 >= eof ? eof : line2;
line2 = line2 <= 0 ? 0 : line2;


/* Find the start of line before the error. This will be called line1. */
/* Skip over the previous EOL (if any) */
while ((i >= 0) && ((INP[i] == 10) || (INP[i] == 13))) --i;
endline1 = i < 0 ? 0 : i;
/* Skip until the previous EOL (if any) */
while ((i >= 0) && (INP[i] != 10) && (INP[i] != 13)) --i;
line1 = i + 1;
line1 = line1 >= endline1 ? 0 : line1;


/* Find start of first line after the error line. This will be called line3 */

i = pos;
/* Skip until the next EOL (if any) */
while ((i < eof) && (INP[i] != 10) && (INP[i] != 13)) ++i;
endline2 = i > eof ? eof : i;
/* Skip over the next EOL (if any) */
while ((i < eof) && ((INP[i] == 10) || (INP[i] == 13))) ++i;
line3 = i > eof ? eof : i;
line3 = line3 <= 0 ? 0 : line3;


/* Find the start of the second line after the error line. This will be called line4  */

i = line3;
/* Skip until the next EOL (if any) */
while ((i < eof) && (INP[i] != 10) && (INP[i] != 13)) ++i;
endline3 = i > eof ? eof : i;
/* Skip over the next EOL (if any) */
while ((i < eof) && ((INP[i] == 10) || (INP[i] == 13))) ++i;
line4 = i > eof ? eof : i;
line4 = line4 <= 0 ? 0 : line4;

/* Find the end of the second line after the error line. This will be called the end of line4  */

i = line4;
/* Skip until the next EOL (if any) */
while ((i < eof) && (INP[i] != 10) && (INP[i] != 13)) ++i;
endline4 = i > eof ? eof : i;


/* Fill each of the four temporary line buffers  */

N = (endline1 - line1);
N = (N <= 0) ? 0 : N;
N = (N >= eof) ? eof - 1 : N;
N = (N >= (NUM)sizeof(line1buf)) ? (NUM)sizeof(line1buf) -1 : N;
strncpy(line1buf,&INP[line1], N);
line1buf[N] = 0;

N = (endline2 - line2);
N = (N <= 0) ? 0 : N;
N = (N >= eof) ? eof - 1 : N;
N = (N >= (NUM)sizeof(line2buf)) ? (NUM)sizeof(line2buf) -1 : N;
strncpy(line2buf,&INP[line2], N);
line2buf[N] = 0;

N = (endline3 - line3);
N = (N <= 0) ? 0 : N;
N = (N >= eof) ? eof - 1 : N;
N = (N >= (NUM)sizeof(line3buf)) ? (NUM)sizeof(line3buf) -1 : N;
strncpy(line3buf,&INP[line3], N);
line3buf[N] = 0;

N = (endline4 - line4);
N = (N <= 0) ? 0 : N;
N = (N >= eof) ? eof - 1 : N;
N = (N >= (NUM)sizeof(line4buf)) ? (NUM)sizeof(line4buf) -1 : N;
strncpy(line4buf,&INP[line4], N);
line4buf[N] = 0;

N = strlen(errorMsg);
N = (N <= 0) ? 0 : N;
N = (N >= eof) ? eof - 1 : N;
N = (N >= (NUM)sizeof(errMgbuf)) ? (NUM)sizeof(errMgbuf) -1 : N;
strncpy(errMgbuf,errorMsg, N);
errMgbuf[N] = 0;


/* Count number of tabs and non-tabs up to error in error line. */

j = line2;
while (j < pos)
	{
	if (INP[j] == 9)
		++tabs;
	else
		++nontabs;
    j++;
	}

/* Count number of lines up to the error line. */

j = 0;
while (j < pos)
	{
	if (INP[j] == 13) ++lineno;
    j++;
	}
++lineno;


/*  Create a detailed error message. */
/*  Notes: Use the truncated versions of the lines surrounding the error */
sprintf(errorBuf,"!%s%s"INTFORMAT"\n%s\n%s\n",errMgbuf," at line ", lineno ,line1buf,line2buf);

/*  If the error line is too long, we mark the first of the line */
if ((pos - line2) < (NUM)sizeof(line2buf))
	{
	j = strlen(errorBuf);
	i = 0;
	while (i < tabs) errorBuf[j + i++] = 9;

	/* Compute the character position relative to the beginning of the line of the error */
	j += i; 
	i = 0;
	while (i <= nontabs) errorBuf[j + i++] = ' ';
	errorBuf[i + j] = 0;
	}

/* Create a second line containing an arrow to the location of the error */
strcat(errorBuf," ^error^\n");
	
/* Concatenate the source line immediately following the error line */
strcat(errorBuf,line3buf);
strcat(errorBuf,"!");
 
/*  Return the error message. */

*ec = TERROR(errorBuf);
FrameExit(*ec);
}

