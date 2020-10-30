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

#define _C_FSPECIALFORMS2
#define _SMARTBASE
#if 0
FSpecialForms2.c

This file contains some of the procedures required to support the SmartLisp compiler 
special form processing.

NOTE:   All examples of generated code are for illustrative purposes only, the actual code
        which would be generated for a given expression may be very different depending on the
        level of optimization in effect at compilation time. They are meant as an aid in 
        understanding, and not as explicit examples of the code that the compiler will actually
        produce at any given time.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include    "fsforms2.h"
#include    "fsforms3.h"
#include    "tstruct.h"
#include    "tlambda.h"
#include    "tobjvec.h"
#include    "fmacro.h"
#include    "fsforms1.h"
#include    "flisp.h"
#include    "fcompile.h"
#include    "foptimize.h"
#include    "fopt2.h"
#include	"terror.h"


/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms2_if

This procedure recognizes all S-expressions (which begin with if) at the top level. 
This procedure generates the correct set of linear virtual machine instructions for returning 
appropriate values, and generates the proper jump instructions. 

Note: 
     0    1     [2]  3 [4]  [5]
    (if  test  then r1 else r2 )
    
(setq vms(vmc "(setq foo (if (< x 10) `ak `nak))"))

 0000: jump    aminteg amvoid  amvoid  2               
 0002: jmpge   amgvoff aminteg aminteg  x                10               11              
 0006: move    amobjec amgvoff amvoid   ak               foo             
 0009: jump    aminteg amvoid  amvoid   14              
 0011: move    amobjec amgvoff amvoid   nak              foo             
 0014: return  amgvoff amvoid  amvoid   foo             

#endif

TVAL    FSpecialForms2_if(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
NUM                 ndx;
BOLE				thenSW = FALSE;
_FCompile_StartFrame
DeclareOBJ(TPcodeVector,Pc);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TSymbol,passLbl);
DeclareOBJ(TSymbol,branchLbl);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareOBJArray(TPair,pairs,12);
_FCompile_EndFrame


/* Abort if rest of expression is NULL */
if (curPair == NULL)
	{
	*ret = TERROR("!if: Missing arguments!");
	_FCompile_FrameExit(*ret);
	}

/*  Setup a pcode object for rapid access. */

Pc = _Pc(compilerState);

/*  We set up an array of pointers into the input list for easy indexed access */

_FMacro_PairsToArray(curPair, pairs, sizeof(pairs)/sizeof(pairs[0]));

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  Generate label symbols for then and else. */

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto BadCleanUp;
passLbl = (TSymbol*)asObject(ret);

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto BadCleanUp;
branchLbl = (TSymbol*)asObject(ret);

/*  VOID result so that test code will alloc its own temps */

_Result(compilerState) = gCP->Tval_VOID;

/*  Call appropriate procedure to generate jump code for (if ...) */

if (pairs[1] == NULL)
	{
	*ret = TERROR("!if: Missing 'test' clause!");
	goto BadCleanUp;
	}

tmpPair = TPair_New(gCP,gTP);
tmpPair->itsCar = pairs[1]->itsCar;

*ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, tmpPair, TRUE, FALSE, passLbl, branchLbl);
if (ret->Tag == TYERROR) goto BadCleanUp;

/*  Setup the _LabelsP to insure that passLbl resolves properly */

*ret = FOptimize2_AddLabel(gCP, gTP, compilerState, passLbl, Pc->itsCurItemIndex);
if (ret->Tag == TYERROR) goto BadCleanUp;


/*  Generate code for the "then" case */
ndx = 2;
/* If the argument list is empty, generate an error and exit */
if (pairs[ndx] == NULL)
	{
	*ret = TERROR("!if: Missing 'then' clause!");
	goto BadCleanUp;
	}

/*  skip "then" keyword if present  */
ndx += (thenSW = ((pairs[ndx]->itsCar.Tag == TYSYMBOL) && (pairs[ndx]->itsCar.u.Symbol == gCP->TLambda_then)));

if (pairs[ndx] == NULL)
	{
	*ret = TERROR("!if: Missing 'then' clause!");
	goto BadCleanUp;
	}

passLbl->itsGlobalValue = pairs[ndx]->itsCar;
ndx++;

/*  skip "else" keyword if present  */

if (pairs[ndx] != NULL)
    ndx += (asTag(&pairs[ndx]->itsCar) == TYSYMBOL && asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_else);

if ((pairs[ndx] == NULL) && (final->Tag == TYVOID) && (thenSW == TRUE))
    {
    /*  If the "then" keyword is present, and no else clause is present,     */
    /*  we generate a simple test and branch without returning the false     */
    /*  return value specified by the standard Lisp (if cc then) expression. */
    /*  This generates more efficient code where the false value is ignored. */
    
    branchLbl->itsGlobalValue = passLbl->itsGlobalValue;

	_Result(compilerState) = *final;
	*ret = FOptimize2_codePass(gCP, gTP, compilerState, branchLbl, FALSE);
	_FCompile_FrameChk(*ret);
	if (ret->Tag == TYERROR) goto BadCleanUp;

	*ret = FOptimize2_AddLabel(gCP, gTP, compilerState, branchLbl, Pc->itsCurItemIndex);
	if (ret->Tag == TYERROR) goto BadCleanUp;
    }
else
if (pairs[ndx] == NULL)
    {
    /*  Setup false as the default else response. */
    
    branchLbl->itsGlobalValue = gCP->TObject_FALSE;
	_Result(compilerState) = *final;
	*ret = FOptimize2_codeResult(gCP, gTP, compilerState, passLbl, branchLbl);
	if (ret->Tag == TYERROR) goto BadCleanUp;
    }
else
    {
    /* Generate the code for r2 (the "else" expression) */
    
    branchLbl->itsGlobalValue = pairs[ndx]->itsCar;
	_Result(compilerState) = *final;
	*ret = FOptimize2_codeResult(gCP, gTP, compilerState, passLbl, branchLbl);
	if (ret->Tag == TYERROR) goto BadCleanUp;
    }

if(!isNullTval(final))
    {
    /*  If an explicit final result was requested, we may have to generate code for it */
    
    *ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
	if (ret->Tag == TYERROR) goto BadCleanUp;
    }


if (passLbl != NULL) passLbl->itsGlobalValue = gCP->Tval_VOID;
if (branchLbl != NULL) branchLbl->itsGlobalValue = gCP->Tval_VOID;

_FCompile_FrameExit(*ret);

BadCleanUp:

if (passLbl != NULL) passLbl->itsGlobalValue = gCP->Tval_VOID;
if (branchLbl != NULL) branchLbl->itsGlobalValue = gCP->Tval_VOID;

if(asTag(ret) != TYERROR)
    {
    *ret = TERROR("!if!");
    }
_FCompile_FrameExit(*ret);
}
 
/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms2_while

This procedure recognizes all S-expressions (which begin with while) at the top level. 
This procedure generates the correct set of linear virtual machine instructions for returning 
appropriate values, and generates the proper jump instructions. 

Note: 
     0      1     [2]  3 
    (while  test  do  e1  ) else...
    
    which we treat as
    (if test then (e1 if test then e1)) else ...
    
    (setq vms(vmc "(while (< x 10) (++ x))"))
    
(setq vms(vmc "(setq foo (while (< x 10) (++ x)))"))

 0000: jump    aminteg amvoid  amvoid   2               
 0002: jmpge   amgvoff aminteg aminteg  x                10               24              
 0006: add     amcvoff amgvoff amfboff  1                x                __T1            
 0017: move    amfboff amgvoff amvoid   __T1             x               
 0020: jmplt   amgvoff aminteg aminteg  x                10               6               
 0024: move    amcvoff amgvoff amvoid   false            foo             
 0034: return  amgvoff amvoid  amvoid   foo             
    
#endif

TVAL    FSpecialForms2_while(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
NUM                 ndx;
NUM                 uncondPatch;
_FCompile_StartFrame
DeclareOBJ(TPcodeVector,Pc);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,beginPair);
DeclareOBJ(TSymbol,passLbl);
DeclareOBJ(TSymbol,branchLbl);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareOBJArray(TPair,pairs,8);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame

/* Abort if rest of expression is NULL */
if (curPair == NULL)
	{
	*ret = TERROR("!while: Missing arguments!");
	_FCompile_FrameExit(*ret);
	}

/*  Setup a pcode object for rapid access. */

Pc = _Pc(compilerState);

/*  We set up an array of pointers into the input list for easy indexed access */

_FMacro_PairsToArray(curPair, pairs, sizeof(pairs)/sizeof(pairs[0]));

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  Generate code to unconditionally jump over "do" code to generate branchLbl. */
/*  Save the location for the target of the VMJUMP just generated so that we may  */
/*  backpatch it later. */
    
_FCompile_MakeOBJ(prmv[0], Pc);
_FCompile_MakeINT(prmv[1], VMJUMP);
_FCompile_MakeINT(prmv[2], AMINTEGER);
_FCompile_MakeINT(prmv[3], AMVOID);
_FCompile_MakeINT(prmv[4], AMVOID);
_FCompile_MakeIMM(prmv[5], -1);
*ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,6, &prmv[0]);
_FCompile_FrameChk(*ret);
uncondPatch = Pc->itsCurItemIndex - 1;

/*  Generate label symbols for then and else. */

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto BadCleanUp;
passLbl = (TSymbol*)asObject(ret);

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto BadCleanUp;
branchLbl = (TSymbol*)asObject(ret);

/*  Call appropriate procedure to generate jump code for (while ...) */

/*  Generate code for the sucessful "then" case */

/*  skip "do" keyword if present  */

ndx = 2;
if (pairs[ndx] == NULL)
	{
	*ret = TERROR("!while: Missing 'do' clause!");
	goto BadCleanUp;
	}

ndx += (asTag(&pairs[ndx]->itsCar) == TYSYMBOL && asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_do);

/*  Call appropriate procedures to generate jumps and code for results */
/*  Note: If we don't have a singleton, we need to generate a (begin ...) */

if (pairs[ndx] == NULL) 
	{
	*ret = TERROR("!while: Missing 'do' clause!");
	goto BadCleanUp;
	}

if (asTag(&pairs[ndx]->itsCdr) == TYVOID)
    passLbl->itsGlobalValue = pairs[ndx]->itsCar;
else
    {
    beginPair = TPair_New(gCP,gTP);
    beginPair->itsCar.u.Object = (TObject*)gCP->FCompile_beginSym;
    beginPair->itsCar.Tag = TYSYMBOL;
    beginPair->itsCdr.u.Object = (TObject*)pairs[ndx];
    beginPair->itsCdr.Tag = TYPAIR;
    passLbl->itsGlobalValue.u.Object = (TObject*)beginPair;
    passLbl->itsGlobalValue.Tag = TYPAIR;
    }

branchLbl->itsGlobalValue = gCP->TObject_FALSE;

/*  We generate the code for the "do" expression. */
/*  Note:   VOID result so that test code will alloc its own temps */

_Result(compilerState) = gCP->Tval_VOID;
*ret = FOptimize2_codePass(gCP, gTP, compilerState, passLbl, FALSE);
if (ret->Tag == TYERROR) goto BadCleanUp;

/* We may now backpatch the location for unconditional jump  generated above */
    
atHMInt(Pc->itsInstructionArray,uncondPatch) = Pc->itsCurItemIndex;

/*  We create a copy of the test expression and send it to the recognizer. */
/*  Note:   VOID result so that test code will alloc its own temps */

if (pairs[1] == NULL)
	{
	*ret = TERROR("!if: Missing 'then' clause!");
	goto BadCleanUp;
	}

tmpPair = TPair_New(gCP, gTP);
tmpPair->itsCar = pairs[1]->itsCar;
_Result(compilerState) = gCP->Tval_VOID;
*ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, tmpPair, FALSE, FALSE, branchLbl, passLbl);
if (ret->Tag == TYERROR) goto BadCleanUp;

_Result(compilerState) = *final;
*ret = FOptimize2_codeBranch(gCP, gTP, compilerState, branchLbl, FALSE);
if (ret->Tag == TYERROR) goto BadCleanUp;

if(!isNullTval(final))
    {
    /*  If an explicit final result was requested, we may have to generate code for it */
    
    *ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
    }

if (passLbl != NULL) passLbl->itsGlobalValue = gCP->Tval_VOID;
if (branchLbl != NULL) branchLbl->itsGlobalValue = gCP->Tval_VOID;

_FCompile_FrameExit(*ret);

BadCleanUp:
if(asTag(ret) != TYERROR)
    {
    *ret = TERROR("!while!");
    }

if (passLbl != NULL) passLbl->itsGlobalValue = gCP->Tval_VOID;
if (branchLbl != NULL) branchLbl->itsGlobalValue = gCP->Tval_VOID;

_FCompile_FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms2_cond

This procedure recognizes all S-expressions (which begin with cond) at the top level. 
This procedure generates the correct set of linear virtual machine instructions for returning 
appropriate values, and generates the proper jump instructions. 

The cond  special form selects one of a series of clauses to evaluate based upon the 
value of their guard expressions.  Each clause is a list (guard  exp...) with a guard 
expression followed by a series of result expressions.  Each guard is an expression or 
the keyword else.

From left to right each guard is evaluated until the result is true or until a the keyword 
else is encountered.  If no guard is true or else, the cond special form returns the value false.
If a clause is selected, the expressions in the clause are evaluated left to right and 
the value of the last expression is returned.  If only the guard is present, true is 
returned.  

For example

    (cond   ((>  3  2)  `GREATER)
        ((<  3  2)  `LESS-THAN))    =>  GREATER
    (cond   ((>  3  3)  `GREATER)
        ((<  3  1)  `LESS-THAN))
        (else  `EQUAL)) =>  EQUAL
        
    (cond ((guard) exp...)... [(else ...)])
#endif

TVAL    FSpecialForms2_cond(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
NUM                 ndx;
_FCompile_StartFrame
DeclareOBJ(TPcodeVector,Pc);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,movPair);
DeclareOBJ(TSymbol,passLbl);
DeclareOBJ(TSymbol,branchLbl);
DeclareOBJ(TSymbol,doneLbl);
DeclareTVAL(cdr);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame


/* Abort if rest of expression is NULL */
if (curPair == NULL)
	{
	*ret = TERROR("!cond: Missing arguments!");
	_FCompile_FrameExit(*ret);
	}

/*  Setup a pcode object for rapid access. */

Pc = _Pc(compilerState);

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  Generate label symbol for done. */

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
_FCompile_FrameChk(*ret);
doneLbl = (TSymbol*)asObject(ret);

/*  Allocate a pair for use during code generation. */

tmpPair = TPair_New(gCP, gTP);
            
/*  Advance the curPair to point to the first argument to this special form. */

for (ndx = 0; (curPair = _FCompile_CdrPair(curPair)) != NULL; ndx++)
    {
    /*  We process each subexpression which must start which a pair until we have processed */
    /*  one which contains an else at which point we skip any further code generation for */
    /*  any further clauses. */
    if (curPair == NULL)
        goto BadCleanUp;
   
    if (isPair(&curPair->itsCar) && ((movPair = asPair(&curPair->itsCar))) != NULL)
        {
        /* Test for valid cond syntax */
        
        if(asTag(&movPair->itsCar) == TYSYMBOL && 
        asSymbol((&movPair->itsCar)) == gCP->TLambda_else)
            break;
        else
        if(!isNullTval(&movPair->itsCar) )
            {
            /*  Generate label symbols for pass and branch. */

            *ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
			if (ret->Tag == TYERROR) goto BadCleanUp;
            branchLbl = (TSymbol*)asObject(ret);
            
            *ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
			if (ret->Tag == TYERROR) goto BadCleanUp;
            passLbl = (TSymbol*)asObject(ret);
            
            /*  Generate code for the default "then" case */
                        
            /*  Call appropriate procedure to generate jump code for the test */
            
            tmpPair->itsCar = movPair->itsCar;
            
            /*  Force test code to alloc its own temps. */

            _Result(compilerState) = gCP->Tval_VOID;
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, tmpPair, TRUE, FALSE, passLbl, branchLbl);
			if (ret->Tag == TYERROR) goto BadCleanUp;
            
            /*  Setup the _LabelsP to insure that passLbl resolves properly */
            
            *ret = FOptimize2_AddLabel(gCP, gTP, compilerState, passLbl, Pc->itsCurItemIndex);
			if (ret->Tag == TYERROR) goto BadCleanUp;
            
            /*  Call appropriate procedure to gen  code for the "then" case (test succeeded). */
            
            if ((tmpPair = _FCompile_CdrPair(movPair)) != NULL)
                {
                if (tmpPair == NULL)
					{
					*ret = TERROR("!cond: guard exp must be folowed by result exp!");
					goto BadCleanUp;
					}

                /*  Allow for multiple expressions following the guard clause... */
                
                while(isPair(&tmpPair->itsCdr) )
                    {
                    /*  We process one at a time, all but the last expression. */
                    
                    _Result(compilerState) = *final;
                    *cdr = tmpPair->itsCdr;
                    tmpPair->itsCdr = gCP->Tval_VOID;
                    *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
					if (ret->Tag == TYERROR) goto BadCleanUp;
                    tmpPair = asPair(cdr);
                    }

                passLbl->itsGlobalValue = tmpPair->itsCar;
                
                /*  Force the result of compilation.. */
                
                _Result(compilerState) = *final;
                *ret = FOptimize2_codePass(gCP, gTP, compilerState, passLbl, FALSE);
				if (ret->Tag == TYERROR) goto BadCleanUp;
                
                /*  Generate code to unconditionally jump over the other cases */
                
                _FCompile_MakeOBJ(prmv[0], Pc);
                _FCompile_MakeINT(prmv[1], VMJUMP);
                _FCompile_MakeINT(prmv[2], AMINTEGER);
                _FCompile_MakeINT(prmv[3], AMVOID);
                _FCompile_MakeINT(prmv[4], AMVOID);
                _FCompile_MakeIMM(prmv[5], -1);
                *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,6, &prmv[0]);
				if (ret->Tag == TYERROR) goto BadCleanUp;
                
                /*  We update the _GotosP(compilerState) vector to contain the location of every  */
                /* jump generated in this procedure. */
                
                *ret = FOptimize2_AddGoto(gCP,gTP,compilerState, doneLbl, Pc->itsCurItemIndex - 1);
				if (ret->Tag == TYERROR) goto BadCleanUp;
                
                /*  Setup the _LabelsP to insure that branchLbl resolves properly */
                
                *ret = FOptimize2_AddLabel(gCP, gTP, compilerState, branchLbl, Pc->itsCurItemIndex);
				if (ret->Tag == TYERROR) goto BadCleanUp;
                }
            else
                {
				*ret = TERROR("!cond: guard exp must be folowed by result exp!!");
				if (ret->Tag == TYERROR) goto BadCleanUp;
				}

			if (passLbl != NULL) passLbl->itsGlobalValue = gCP->Tval_VOID;
			if (branchLbl != NULL) branchLbl->itsGlobalValue = gCP->Tval_VOID;
            }
        else
            break;
        }
    }
        
/*  We have processed all of the ((guard) exp ) portions of this cond. */

if (movPair == NULL) 
	goto BadCleanUp;


if(asTag(&movPair->itsCar) == TYSYMBOL && asSymbol((&movPair->itsCar)) == gCP->TLambda_else)
    {
    /*  Test to see if we are at the else case */
    /*  Setup false as the default response. */
    
    if ((movPair = _FCompile_CdrPair(movPair)) != NULL)
		{
        tmpPair = movPair;
		}
    else
		{
		*ret = TERROR("!cond: Missing 'else' clause!");
		goto BadCleanUp;
		}
    }
else
    {
    /*  Setup false as the default response. */
    
    tmpPair->itsCar = gCP->TObject_FALSE;
    }
    	
/*  Allow for multiple expressions following the guard clause... */

while(tmpPair != NULL)
    {
    /*  We process one at a time, all but the last expression. */
    
    _Result(compilerState) = *final;
    *cdr = tmpPair->itsCdr;
    tmpPair->itsCdr = gCP->Tval_VOID;
    *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
	if (ret->Tag == TYERROR) goto BadCleanUp;
	if (cdr->Tag == TYPAIR)
		tmpPair = cdr->u.Pair;
	else
		tmpPair = NULL;
    }

/*  Setup the _LabelsP to insure that doneLbl resolves properly */

*ret = FOptimize2_AddLabel(gCP, gTP, compilerState, doneLbl, Pc->itsCurItemIndex);
if (ret->Tag == TYERROR) goto BadCleanUp;

if(!isNullTval(final))
    {
    /*  If an explicit final result was requested, we may have to generate code for it */
    
    *ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
    }
else
    *ret = _LastResult(compilerState);


if (doneLbl != NULL) doneLbl->itsGlobalValue = gCP->Tval_VOID;

_FCompile_FrameExit(*ret);

BadCleanUp:

if (doneLbl != NULL) doneLbl->itsGlobalValue = gCP->Tval_VOID;

if(asTag(ret) != TYERROR)
    {
    *ret = TERROR("!cond!");
    }
_FCompile_FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms2_begin

Returns the location of the result.

The  begin  special form evaluates a sequence of expressions, and returns the value of the 
last expression.  If no expressions are specified, the empty list () is returned. 

For example

    (begin  (+  1  2)  (+  3  4))
    
might generate the instructions:

 0000: add     aminteg aminteg amfboff  2                1                __T0            
 0004: add     aminteg aminteg amfboff  4                3                __T0            
 0008: return  amfboff amvoid  amvoid   __T0            

From this example we can see that the begin special form evaluates each expression in turn, and
places the result in the same location, the temporary binding used for return values from 
special forms.

#endif

TVAL    FSpecialForms2_begin(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,endPair);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVAL(empty);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame


/* Abort if rest of expression is NULL */
if (curPair == NULL)
	{
	*ret = TERROR("!begin: Missing arguments!");
	_FCompile_FrameExit(*ret);
	}

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));

/*  We advance the curPair to point to the next argument after the "begin" keyword. */

if ((asTag(&curPair->itsCar) == TYSYMBOL) &&
    (asTag(&(aSymbol = asSymbol(&curPair->itsCar))->itsGlobalValue) == TYSPECIALFORM) &&
    ((aSymbol = asSymbol(&aSymbol->itsGlobalValue)) == gCP->FCompile_beginSym))
    curPair = _FCompile_CdrPair(curPair);

/*  We compile all the statements following the "begin" keyword. */

if (curPair != NULL)
    {
    /*  There are values in the begin argument list. Create a temp pair to send to  */
    /*  FCompile_Recognize. */
    
    tmpPair = TPair_New(gCP, gTP);
    endPair = TPair_New(gCP, gTP);
    
    while (curPair != NULL)
        {
        /*  We process the arguments inside the begin block. */
        
		if (curPair->itsCar.Tag == TYLABELSYMBOL)
			{
			/*  Handle the special case where the argument is a Label. This situation.	*/
			/*  requires that FSpecialForms3_label to generate code for the label		*/

 			tmpPair->itsCar = curPair->itsCar;
			tmpPair->itsCdr = curPair->itsCdr;

			_Result(compilerState) = *final;
			*ret = FSpecialForms3_label(gCP, gTP, compilerState, curPair);
			_FCompile_FrameChk(*ret);
			curPair = _FCompile_CdrPair(curPair);  /* skip pass the real label argument */
			tmpPair->itsCdr = gCP->Tval_VOID;

			/*  Get to the next argument in the "begin" arglist if any. */			
			curPair = _FCompile_CdrPair(curPair);
			}
			
		else
		if ((curPair->itsCar.Tag == TYQUOTEDSYMBOL) && 
			(curPair->itsCdr.Tag == TYPAIR) && 
			((curPair->itsCar.u.Symbol == gCP->TLambda_args) ||
			 (curPair->itsCar.u.Symbol == gCP->TLambda_rvars) ||
			 (curPair->itsCar.u.Symbol == gCP->TLambda_faces) ||
			 (curPair->itsCar.u.Symbol == gCP->TLambda_vars) ||
			 (curPair->itsCar.u.Symbol == gCP->TLambda_svars) ||
			 (curPair->itsCar.u.Symbol == gCP->TLambda_pvars) ||
			 (curPair->itsCar.u.Symbol == gCP->TLambda_cvars)))
			{
			/*  Note: If we see an attempt to declare variables,	*/
			/*        then we process two arguments at a time.		*/
			/*  The following argument pairs are processed together	*/
			/*  args:() regs:() vars:() svars:() pvars:() cvars:()  */

			tmpPair->itsCar = curPair->itsCar;
			tmpPair->itsCdr.Tag = TYPAIR;
			tmpPair->itsCdr.u.Pair = endPair;
			endPair->itsCar = curPair->itsCdr.u.Pair->itsCar;
			endPair->itsCdr = gCP->Tval_VOID;

			_Result(compilerState) = *final;
			*ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
			_FCompile_FrameChk(*ret);

			/*  Get to the next argument in the "begin" arglist if any. */			
			curPair = _FCompile_CdrPair(curPair);
			curPair = _FCompile_CdrPair(curPair);
			}

		else
			{
			/*  We process any other arguments as separate subexpressions.		*/
			/*  Call FCompile_Recognize to generate code for the subexpression. */
        
			tmpPair->itsCar = curPair->itsCar;
			tmpPair->itsCdr = gCP->Tval_VOID;

			_Result(compilerState) = *final;
			*ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
			_FCompile_FrameChk(*ret);

			/*  Get to the next argument in the "begin" arglist if any. */			
			curPair = _FCompile_CdrPair(curPair);
			}
    
        }
   }
else
    {
    /*  There are no arguments, so we generate code to return void. */
    
/*  Allocate for the result of the selector expression */

    *empty = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
    _FCompile_FrameChk(*empty);
    
    _FCompile_MakeINT(prmv[1], VMMOVE);
    _FCompile_MakeINT(prmv[2], AMWORD);
    _FCompile_MakeCON(prmv[5], gCP->TObject_VOID);
    _FCompile_MakeINT(prmv[3], asModifier(empty));
    _FCompile_MakeSYM(prmv[6], *empty);
    _FCompile_MakeINT(prmv[4], AMVOID);
    
    *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,7,&prmv[0]);
    _FCompile_FrameChk(*ret);
    }
    
if(!isNullTval(final))
    {
    /*  If an explicit final result was requested, we may have to generate code for it */
    
    *ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
    }
else
     *ret = _LastResult(compilerState);

_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms2_case

This procedure recognizes all S-expressions (which begin with case) at the top level. 
This procedure generates the correct set of linear virtual machine instructions for returning 
appropriate values, and generates the proper jump instructions. 

The case  special form selects one of a series of clauses to evaluate based upon the 
value of a controlling selector expression.  Each clause is a list (match  exp...) with 
a match expression followed by a series of result expressions.  Each match expression is 
an atom, a list of atoms, or the keyword else.

First the selector is evaluated, and this value is compared with the match expression 
from each clause until the comparison returns true or until a match with the keyword 
else is encountered.  The selector is compared to each match using isMemEqv if match is a 
list (a Pair) and isEqv if match is not a pair.  If no selector matches, the case special
form returns the value false.  If a clause is selected, the expressions in the clause are 
evaluated left to right and the value of the last expression is returned.

For example

    (case  (*  2  3)  
        ((2  3  5  7)  `PRIME) 
         ((1  4  6  8  9)  `COMPOSITE)) =>  `COMPOSITE
    (case  (+  4  -2)
        (1  `(DO-THIS))
        (2  `(DO-THAT))
        (else  `(DO-THE-OTHER)) =>  (DO-THAT)
    (case  6
        (1  (/ 6 5))
        (2  (sin 1.3))
        (else  (+ 1 2) (* 4 5)) =>  20

#endif

TVAL    FSpecialForms2_case(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
NUM                 ndx;
_FCompile_StartFrame
DeclareOBJ(TSymbol,testFunc);
DeclareOBJ(TPcodeVector,Pc);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,clausePair);
DeclareOBJ(TPair,beginPair);
DeclareOBJ(TSymbol,passLbl);
DeclareOBJ(TSymbol,branchLbl);
DeclareOBJ(TSymbol,doneLbl);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVAL(selector);
DeclareTVAL(test);
DeclareTVAL(success);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame

/* Abort if rest of expression is NULL */
if (curPair == NULL)
	{
	*ret = TERROR("!case: Missing arguments!");
	_FCompile_FrameExit(*ret);
	}

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  Setup a pcode object for rapid access. */

Pc = _Pc(compilerState);

/*  Generate label symbol for done. */

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto Last;
doneLbl = (TSymbol*)asObject(ret);

/*  Allocate a pair for use during code generation. */

tmpPair = TPair_New(gCP,gTP);
            
/*  Step over the "case" keyword */

curPair = _FCompile_CdrPair(curPair);
if (curPair == NULL)
	{
	*ret = TERROR("!case: Missing arguments!");
	goto Last;
	}

/*  Generate code to evaluate the selector expression. */
/*  Allocate a temporary for saving the result of the */
/*  selector expression evaluation. After this section, */
/*  the *selector contains the temp label where */
/*  the generated code has placed the selector result. */

*selector = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
if (ret->Tag == TYERROR) goto Last;

/*  Save selector as the expression return value. */

_Result(compilerState) = *selector;

/*  Generate code the evaluate the selector. */
if (asTag(&curPair->itsCar) == TYVOID)
    {
	*ret = TERROR("!case: Missing selector!");
	goto Last;
    }
tmpPair->itsCar = curPair->itsCar;

*ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
if (ret->Tag == TYERROR) goto Last;

/*  Advance the curPair to point to the first argument to this special form. */

for (ndx = 0; (curPair = _FCompile_CdrPair(curPair)) != NULL; ndx++)
    {
    if (curPair == NULL)
        {
		*ret = TERROR("!case: Missing match clause!");
		goto Last;
        }
    /*  We process each subexpression until we have processed */
    /*  one which contains an else at which point we skip any further code generation for */
    /*  any further clauses. */
    
    if (isPair(&curPair->itsCar) && (clausePair = asPair(&curPair->itsCar)))
        {
        /* Test for valid case syntax */
        /* Stop if we come to an else guard symbol. */

        if ((asTag(&clausePair->itsCar) == TYSYMBOL && asSymbol((&clausePair->itsCar)) == gCP->TLambda_else))
            {
            break;
            }
        else
        /* Generate code for each non-else guard clause. */

        if (!isNullTval(&clausePair->itsCar))
            {
            /*  Save the value to compare */
            
            *test = clausePair->itsCar;
                
            if (asTag(&clausePair->itsCar) == TYPAIR )
                {
                /*  Select test function for lists */
                
                testFunc = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"isMemEqv");

                /*  We quote the list in the call to memeqv so that  the recognizer does not try */
                /*  to generate code to evaluate it. */
                
                asTag(test) = TYQUOTEDPAIR;
                asQuoteCnt(test) = 1;
                }
            else
                {
                /*  Select test function for atoms */
                
                testFunc = gCP->FCompile_eqSym;
                }
                
            /*  Save the result to return on success */
            
            if ((clausePair = _FCompile_CdrPair(clausePair)) != NULL)
                {
                /*  Note: If we don't have a singleton, we need to generate a (begin ...) */
                
                if (asTag(&clausePair->itsCdr) == TYVOID)
                    *success = clausePair->itsCar;
                else
                    {
                    beginPair = TPair_New(gCP,gTP);
                    beginPair->itsCar.u.Object = (TObject*)gCP->FCompile_beginSym;
                    beginPair->itsCar.Tag = TYSYMBOL;
                    beginPair->itsCdr.u.Object = (TObject*)clausePair;
                    beginPair->itsCdr.Tag = TYPAIR;
                    success->u.Object = (TObject*)beginPair;
                    success->Tag = TYPAIR;
                    }
                }
            else
				{
				*ret = TERROR("!case: Invalid case clause!");
				goto Last;
				}
                
            /*  Generate code for call to either isEqv or memeqv. */
            
            clausePair= TPair_New(gCP,gTP);
            asTag(&tmpPair->itsCar) = TYPAIR;
            asObject(&tmpPair->itsCar) = (TObject*)clausePair;


            /*  We will construct an expression to send to the recognizer and allow */
            /*  the compiler to generate code for us which will return its result in a */
            /*  location which we will be able to check. We construct an expression like: */
            /*      (isEqv selector atom)   */
            /*  or */
            /*      (memeqv selector list)   */
            /*  where selector is a compiler temp which was loaded via the code generated */
            /*  at the start of this compilation     */
            
            _FCompile_MakeOBJ(clausePair->itsCar, testFunc);
            asObject(&clausePair->itsCdr) = (TObject*)TPair_New(gCP,gTP);
            asTag(&clausePair->itsCdr) = TYPAIR;
            clausePair = _FCompile_CdrPair(clausePair);
            clausePair->itsCar = *selector;
            asObject(&clausePair->itsCdr) = (TObject*)TPair_New(gCP,gTP);
            asTag(&clausePair->itsCdr) = TYPAIR;
            clausePair = _FCompile_CdrPair(clausePair);
            clausePair->itsCar = *test;

            /*  Generate label symbols for pass and branch. */
            
            *ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
            if (ret->Tag == TYERROR) goto Last;
            branchLbl = (TSymbol*)asObject(ret);
            
            *ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
            if (ret->Tag == TYERROR) goto Last;
            passLbl = (TSymbol*)asObject(ret);
            
            /*  Force test code to alloc its own temps. */

            _Result(compilerState) = gCP->Tval_VOID;
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, tmpPair, TRUE, FALSE, passLbl, branchLbl);
            if (ret->Tag == TYERROR) goto Last;
            
            /*  Setup the _LabelsP to insure that passLbl resolves properly */
            
            *ret = FOptimize2_AddLabel(gCP, gTP, compilerState, passLbl, Pc->itsCurItemIndex);
            if (ret->Tag == TYERROR) goto Last;
            
            /*  If we find a matched key case (test succeeded). */
            /*  Call appropriate procedure to gen  code for the clause. */
            passLbl->itsGlobalValue = *success;
 
           
            _Result(compilerState) = *final;
            *ret = FOptimize2_codePass(gCP, gTP, compilerState, passLbl, FALSE);
            if (ret->Tag == TYERROR) goto Last;

            /*  Generate code to unconditionally jump over the other cases */
            
            _FCompile_MakeOBJ(prmv[0], Pc);
            _FCompile_MakeINT(prmv[1], VMJUMP);
            _FCompile_MakeINT(prmv[2], AMINTEGER);
            _FCompile_MakeINT(prmv[3], AMVOID);
            _FCompile_MakeINT(prmv[4], AMVOID);
            _FCompile_MakeIMM(prmv[5], -1);
            *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,6, &prmv[0]);
            if (ret->Tag == TYERROR) goto Last;
            
            /*  We update the _GotosP(compilerState) vector to contain the location of every  */
            /* jump generated in this procedure. */
            
            *ret = FOptimize2_AddGoto(gCP,gTP,compilerState, doneLbl, Pc->itsCurItemIndex - 1);
            if (ret->Tag == TYERROR) goto Last;
            
            /*  Setup the _LabelsP to insure that branchLbl resolves properly */
            
            *ret = FOptimize2_AddLabel(gCP, gTP, compilerState, branchLbl, Pc->itsCurItemIndex);
            if (ret->Tag == TYERROR) goto Last;

			if (passLbl != NULL) passLbl->itsGlobalValue = gCP->Tval_VOID;
			if (branchLbl != NULL) branchLbl->itsGlobalValue = gCP->Tval_VOID;
            }
        else
			{
			*ret = TERROR("!case: Invalid case clause!");
			if (ret->Tag == TYERROR) goto Last;
			}
        }
    }
        
/*  We have processed all of the ((guard) exp ) portions of this cond. */
/*  Do we have an (else exp) style clause of this cond? */

if (clausePair != NULL && 
	asTag(&clausePair->itsCar) == TYSYMBOL && 
	asSymbol((&clausePair->itsCar)) == gCP->TLambda_else)
    {
    /*  Test to see if we are at the else case */
    /*  Setup false as the default response. */
    
    if ((clausePair = _FCompile_CdrPair(clausePair)) != NULL)
        tmpPair->itsCar = clausePair->itsCar;
    else
		{
		*ret = TERROR("!case: Invalid else clause!");
		goto Last;
		}
    }
else
    {
    /*  If no else guard, setup false as the default response. */
    
    tmpPair->itsCar = gCP->TObject_FALSE;
    }
    
_Result(compilerState) = *final;
*ret = FCompile_Singleton(gCP, gTP, compilerState, tmpPair);
if (ret->Tag == TYERROR) goto Last; 

/*  Setup the _LabelsP to insure that doneLbl resolves properly */

*ret = FOptimize2_AddLabel(gCP, gTP, compilerState, doneLbl, Pc->itsCurItemIndex);
if (ret->Tag == TYERROR) goto Last;

if(!isNullTval(final))
    {
    /*  If an explicit final result was requested, we may have to generate code for it */
    
    *ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
    }
else
    *ret = _LastResult(compilerState);


Last:
if (doneLbl != NULL) doneLbl->itsGlobalValue = gCP->Tval_VOID;

_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms2_loop

This procedure recognizes all S-expressions (which begin with loop) at the top level. 
This procedure generates the correct set of linear virtual machine instructions for returning 
appropriate values, and generates the proper jump instructions. 

(loop  for  var  from init   to final   by step   do  stmt...)

Do the processing required for handling this special form.
The loop special form performs fast numeric iteration.  The specified variable {var} 
is the index of the iteration. The {init} expression forms the initial value for var at 
the start of the iteration.  The {step} expression forms the value which is added to the
var at the start of the next iteration.

The {final} expression is compared to the var at the start of each iteration.  If the 
var is less than or equal to final, each {stmt} is evaluated in order. Next, the {step} 
value is added to the var.  Then {final} is compared to var again.

If the {step} is positive, iteration stops when the var is greater than {final}.  
If the {step} is negative, iteration stops when the var is less than {final}.  
If the {step} is omitted, the step value is assumed to be 1, and iteration stops when 
the var is greater than {final}.  The loop special form returns the value of the var.
  
This example displays the numbers one thru ten and returns eleven as its result.

    (loop for  n  from 1  to 10  by 1  do  (display    n))  =>  11
    Console Window  =>  1  2  3  4  5  6  7  8  9  10

which might generate the following code:

 0000: jump    aminteg amvoid  amvoid  2               
 0002: move    aminteg amgvoff amvoid   1                n               
 0005: move    aminteg amfboff amvoid   10               __T1            
 0008: jmpgt   amgvoff amfboff aminteg  n                __T1             26              
 0012: push    amgvoff amvoid  amvoid   n               
 0014: call    aminteg amgvoff amfboff  1                display          __T0            
 0018: addi    aminteg amgvoff amgvoff  1                n                n               
 0022: jmple   amgvoff amfboff aminteg  n                __T1             12              
 0026: return  amgvoff amvoid  amvoid   n               


Note: 
     0      1     2   3     4    5   6      [7]  [8]    9   10
    (loop  for  var  from init   to max   by  step   do  stmt...)
    
#endif

TVAL    FSpecialForms2_loop(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
NUM                 ndx;
NUM                 step;
_FCompile_StartFrame
DeclareOBJ(TPcodeVector,Pc);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,testPair);
DeclareOBJ(TPair,beginPair);
DeclareOBJ(TSymbol,passLbl);
DeclareOBJ(TSymbol,branchLbl);
DeclareTVAL(ret);
DeclareTVAL(var);
DeclareTVAL(final);
DeclareTVAL(max);
DeclareTVAL(stepValue);
DeclareTVALArray(prmv,8);
DeclareOBJArray(TPair,pairs,11);
_FCompile_EndFrame

/* Abort if rest of expression is NULL */
if (curPair == NULL)
	{
	*ret = TERROR("!loop: Invalid case clause!");
	_FCompile_FrameExit(*ret);
	}

/*  Setup a pcode object for rapid access. */

Pc = _Pc(compilerState);

/*  We set up an array of pointers into the input list for easy indexed access */

_FMacro_PairsToArray(curPair, pairs, sizeof(pairs)/sizeof(pairs[0]));

if( (pairs[5] == NULL) ||
    (asTag(&pairs[1]->itsCar) != TYSYMBOL || asSymbol(&pairs[1]->itsCar) != gCP->TLambda_for) ||
    (asTag(&pairs[3]->itsCar) != TYSYMBOL || asSymbol(&pairs[3]->itsCar) != gCP->TLambda_from)||
    (asTag(&pairs[5]->itsCar) != TYSYMBOL || 
     (asSymbol(&pairs[5]->itsCar) != gCP->TLambda_to && asSymbol(&pairs[5]->itsCar) != gCP->TLambda_until))  ||
    asTag(&pairs[2]->itsCar) != TYSYMBOL)
	{
    /*  Syntax checking failed. */
	*ret = TERROR("!loop: Missing mandatory keywords: for, from, until!");
	_FCompile_FrameExit(*ret);
	}

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  Allocate a temporary pair for use in code generation. */

tmpPair = TPair_New(gCP,gTP);

/*  get a pointer to the var */
if(pairs[2] == NULL || asTag(&pairs[2]->itsCar) != TYSYMBOL)
    {
	*ret = TERROR("!loop: Invalid Init Clause!");
	_FCompile_FrameExit(*ret);
    }

*var = *ret = FCompile_LookUp(gCP, gTP, compilerState, asSymbol(&pairs[2]->itsCar));
if(asTag(ret) == TYVOID)
    {
	*ret = TERROR("!loop: Invalid Init Clause!");
	_FCompile_FrameExit(*ret);
    }

/*  create a copy of the "init" expression and send it to the recognizer */
if (asTag(&pairs[4]->itsCar) == TYVOID)
    {
	*ret = TERROR("!loop: Invalid Init Clause!");
	_FCompile_FrameExit(*ret);
    }


tmpPair->itsCar = pairs[4]->itsCar;

    
/*  force compilation of init to place result in var */

_Result(compilerState) = *var;
*ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
_FCompile_FrameChk(*ret);

/*  Save the "max" expression. */

if (pairs[6] == NULL) 
    {
	*ret = TERROR("!loop: Invalid stop Clause!");
	_FCompile_FrameExit(*ret);
    }

*max = pairs[6]->itsCar;
    
ndx = 7;
if ((pairs[ndx] != NULL) && 
    (asTag(&pairs[ndx]->itsCar) == TYSYMBOL) && 
    (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_by))
    {
    /*  The step value has been explicitly specified, we must check to insure that it is */
    /*  a constant integer value. */
    
    if (pairs[8] == NULL)
		{
		*ret = TERROR("!loop:  Missing Step Clause!");
		_FCompile_FrameExit(*ret);
		}

    tmpPair->itsCar = pairs[8]->itsCar;
    *stepValue = tmpPair->itsCar;
    
    if(asTag(stepValue) != TYNUM)
        {
        /*  In this implementation step must be a simple numeric constant. */		
		*ret = TERROR("!loop: Invalid step clause: not an integer constant!");
		_FCompile_FrameExit(*ret);
        }
    else
        step = asInt(stepValue);
    
    /*  we skip step value as constant */

    ndx += 2;
    }
else
    {
    /*  Define initial default value for step. */
    
    step = 1;
    _FCompile_MakeINT(*stepValue, step);
    }
    
/*  skip "do" keyword if present  */

if (pairs[ndx] != NULL)
    ndx += (asTag(&pairs[ndx]->itsCar) == TYSYMBOL && asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_do);

/*  skip #void expression if no expression is present  */

if (pairs[ndx] == NULL)
    pairs[ndx] = TPair_New(gCP,gTP);

/*  Generate label symbols for then and else. */

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto Last;
passLbl = (TSymbol*)asObject(ret);

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto Last;
branchLbl = (TSymbol*)asObject(ret);

/*  Call appropriate procedure to generate jump code for (loop ...) */

/*  Call appropriate procedures to generate jumps and code for results */
/*  Note: If we don't have a singleton, we need to generate a (begin ...) */

if (asTag(&pairs[ndx]->itsCdr) == TYVOID)
    passLbl->itsGlobalValue = pairs[ndx]->itsCar;
else
    {
    beginPair = TPair_New(gCP,gTP);
    beginPair->itsCar.u.Object = (TObject*)gCP->FCompile_beginSym;
    beginPair->itsCar.Tag = TYSYMBOL;
    beginPair->itsCdr.u.Object = (TObject*)pairs[ndx];
    beginPair->itsCdr.Tag = TYPAIR;
    passLbl->itsGlobalValue.u.Object = (TObject*)beginPair;
    passLbl->itsGlobalValue.Tag = TYPAIR;
    }

if (branchLbl != NULL) branchLbl->itsGlobalValue = *var;

/*  Since we are assuming that step is a constant numeric we generate code according */
/*  to its sign value. */
/*  Generate code for the looptest */

if(step == 0)
    {
    /*  In this implementation step must be a simple numeric constant. */		
	*ret = TERROR("!loop: Invalid step clause, cannot have 0 step value!");
	goto Last;
    }
else
    {
    /*  Construct list for the test expression */
    
    testPair = tmpPair;
    
    asObject(&tmpPair->itsCar) = (TObject*)TPair_New(gCP,gTP);
    asTag(&tmpPair->itsCar) = TYPAIR;
    tmpPair = asPair(&tmpPair->itsCar);
    
    if (asSymbol(&pairs[5]->itsCar) == gCP->TLambda_to)
        tmpPair->itsCar.u.Object = (TObject*)(( step < 0) ? gCP->FCompile_igeSym : gCP->FCompile_ileSym);
    else
        tmpPair->itsCar.u.Object = (TObject*)(( step < 0) ? gCP->FCompile_igtSym : gCP->FCompile_iltSym);
    tmpPair->itsCar.Tag = tmpPair->itsCar.u.Object->itsObjectType;
    asObject(&tmpPair->itsCdr) = (TObject*)TPair_New(gCP,gTP);
    asTag(&tmpPair->itsCdr) = TYPAIR;
    tmpPair = asPair(&tmpPair->itsCdr);
    
    tmpPair->itsCar = *var;
    asObject(&tmpPair->itsCdr) = (TObject*)TPair_New(gCP,gTP);
    asTag(&tmpPair->itsCdr) = TYPAIR;
    asPair(&tmpPair->itsCdr)->itsCar = *max;
    }

/*  Send a copy of the test expression to the recognizer. */

/*  VOID result so that test code will alloc its own temps */

_Result(compilerState) = gCP->Tval_VOID;
*ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, testPair, TRUE, FALSE, passLbl, branchLbl);
if (ret->Tag == TYERROR) goto Last;

/*  VOID result so that then code will alloc its own temps */

_Result(compilerState) = gCP->Tval_VOID;
*ret = FOptimize2_codePass(gCP, gTP, compilerState, passLbl, FALSE);
if (ret->Tag == TYERROR) goto Last;

/* generate the code to increment our loop counter */

_FCompile_MakeOBJ(prmv[0], Pc);
_FCompile_MakeINT(prmv[1], VMIADD);
_FCompile_MakeINT(prmv[2], AMWORD);
_FCompile_MakeCON(prmv[5], TINT(step));
_FCompile_MakeINT(prmv[3], asModifier(var));
_FCompile_MakeSYM(prmv[6], *var);
_FCompile_MakeINT(prmv[4], asModifier(var));
_FCompile_MakeSYM(prmv[7], *var);

*ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,8, &prmv[0]);
if (ret->Tag == TYERROR) goto Last;

/*  Send a copy of the test expression to the recognizer. */
/*  First we must reverse the  */

/*  VOID result so that test code will alloc its own temps */

_Result(compilerState) = gCP->Tval_VOID;

*ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, testPair, FALSE, FALSE, branchLbl, passLbl);
if (ret->Tag == TYERROR) goto Last;

/*  Force the result of compilation.. */

_Result(compilerState) = gCP->Tval_VOID;
*ret = FOptimize2_codeBranch(gCP, gTP, compilerState, branchLbl, FALSE);
if (ret->Tag == TYERROR) goto Last;

if(!isNullTval(final))
    {
    /*  If an explicit final result was requested, we may have to generate code for it */
    
    *ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
    }

Last:
if (passLbl != NULL) passLbl->itsGlobalValue = gCP->Tval_VOID;
if (branchLbl != NULL) branchLbl->itsGlobalValue = gCP->Tval_VOID;

_FCompile_FrameExit(*ret);
}

/*  We experiment with turning framing off for selected procedures to try to improve performance. */

/*#include "disableframing.h" */

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms2_send

The  send  special form supports symbolic messaging with any number of arguments. The message 
argument must be an element of type Symbol, and there must be at least one argument. 

The action taken by the symbolic message is dependent upon the type of the first argument.  
The message symbol is looked up in the Methods Structure attached to the type (see the defmethod 
Macro).  If the message symbol is found, the associated Procedure is invoked against the whole 
argument list.  If the message symbol is not found, an error condition results.

 For example
 
    (defmethod  Integer:  square(n)  (*  n  n)) =>  #{`square #<TLambda 707>}
    (send  square:  0)  =>  0
    (send  square:  4)  =>  16

vmsend  argcount    message target

Note: 
     0      1       2...
    (send  message:  arguments ... )
    
Consider the expression:

        (send  square:  4)

which might generate the code:

 0000: push    aminteg amvoid  amvoid   4               
 0002: send    aminteg amobjec amfboff  1                square           __T0            
 0006: return  amfboff amvoid  amvoid   __T0            

From this example we can see that when the send special form is processed, first the arguments are
pushed and then a send instruction is generated.

#endif

TVAL    FSpecialForms2_send(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
NUM					vmsend = VMSEND;
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(final);
DeclareTVAL(sym);
DeclareTVAL(ret);
DeclareTVALArray(prmv,9);
_FCompile_EndFrame

/* Abort if rest of expression is NULL */
if (curPair == NULL)
    {
    /*  In this implementation step must be a simple numeric constant. */		
	*ret = TERROR("!send: Missing Arguments!");
	_FCompile_FrameExit(*ret);
    }

/*  Move past the send: special form to the first arg or expression */

curPair = _FCompile_CdrPair(curPair);

/* Abort if rest of the subexpression is NIL */
if (curPair == NULL)
    {
    /*  In this implementation step must be a simple numeric constant. */		
	*ret = TERROR("!compile: send has missing Arguments!");
	_FCompile_FrameExit(*ret);
    }

/*  Handle an indirect message argument (if necessary). */

if (asTag(&curPair->itsCar) == TYPAIR)
    {
    tmpPair = asPair(&curPair->itsCar);

    /*  Make sure the indirect message send is correct. */

    if (asTag(&tmpPair->itsCar) != TYSYMBOL)
        {
        /*  This is an error ... */
 		goto SendMsgMissing;
        }

    /*  Save the final location, if any was requested */

    *final = _Result(compilerState);
    _Result(compilerState) = gCP->Tval_VOID;

    /*  Generate code to dereference the function to call, and save the result. */

    /*  Allocate/reserve the result location for the message symbol */

    *sym = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
    _FCompile_FrameChk(*sym);

    /*  force compilation of init to place result in sym */

    /*  Allocate a temporary pair for use in code generation. */

    tmpPair = TPair_New(gCP,gTP);
    tmpPair->itsCar = curPair->itsCar;
    if (asTag(&tmpPair->itsCar) == TYVOID)
        {
        /*  This is an error ... */
 		*ret = TERROR("!send: Missing target object!");
		_FCompile_FrameExit(*ret);
        }

    _Result(compilerState) = *sym;
    *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
    _FCompile_FrameChk(*ret);
    }
else
/*  Handle the message argument if it is a variable name symbol. */

if (asTag(&curPair->itsCar) == TYSYMBOL)
    {
    /*  We will setup a message send call. */
    
    *sym = curPair->itsCar;

    /*  Save the final location, if any was requested */
    
    *final = _Result(compilerState);
    _Result(compilerState) = gCP->Tval_VOID;
        
    /*  Move to the first arg or expression */

    curPair = _FCompile_CdrPair(curPair);
    if (curPair != NULL)
        {
        /*  Push takes care of generating code for VMPUSH instructions to push an arbitrary  */
        /*  number of arguments, and returns the number of arguments for which VMPUSH instructions */
        /*  were generated. */
        
        *ret = FCompile_Push(gCP, gTP, compilerState, curPair, &vmsend);
        _FCompile_FrameChk(*ret);
        }
    else
        {
        /*  There are no arguments to this function, then it is a bad message send! */       
  		*ret = TERROR("!send: Missing Arguments for the message!");
		_FCompile_FrameExit(*ret);
        }
        
    /*  After any subexpressions have been processed generate code for a function call */
    
    _FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
    _FCompile_MakeINT(prmv[1], VMSEND);
    _FCompile_MakeINT(prmv[2], AMINTEGER);
    _FCompile_MakeIMM(prmv[5], asInt(ret));

	*ret = FCompile_LookUp(gCP, gTP, compilerState, sym->u.Symbol);
    _FCompile_FrameChk(*ret);
    _FCompile_MakeINT(prmv[3], asModifier(ret));
    _FCompile_MakeINT(prmv[6], asOffset(ret));
    
    if(isNullTval(final))
        {
        /*  No return location was specified. During code generation for a function call we */
        /*  may always safely reuse the first temporary which was available when we entered */
        /*  this procedure, it will always be available even if it was used in code generation */
        /*  for the arguments which were pushed for this call. We make this so by resetting  */
        /*  _TempN(compilerState) to __Base__, which is a hidden control variable managed by the */
        /*  _FCompile_* framing macros. */
        
        _FCompile_ResetBase;
        *final = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
        }

    _LastResult(compilerState) = *final;
    _FCompile_MakeINT(prmv[4], asModifier(final));
    _FCompile_MakeSYM(prmv[7], *final);
    
    /*  Finally append the new instruction to the procedure object. */
    
    *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    /*  Set the return value for this function. */
    
    _FCompile_MakeTVL(*ret, _LastResult(compilerState));
    _FCompile_FrameExit(*ret);
    }
else
/*  Handle the message argument if it is a quoted symbol. */

if (asTag(&curPair->itsCar) == TYQUOTEDSYMBOL)
    {
    /*  We will setup a message send call. */
    
    *sym = curPair->itsCar;

    /*  Save the final location, if any was requested */
    
    *final = _Result(compilerState);
    _Result(compilerState) = gCP->Tval_VOID;
        
    /*  Move to the first arg or expression */

    curPair = _FCompile_CdrPair(curPair);
    if (curPair != NULL)
        {
        /*  Push takes care of generating code for VMPUSH instructions to push an arbitrary  */
        /*  number of arguments, and returns the number of arguments for which VMPUSH instructions */
        /*  were generated. */
        
        *ret = FCompile_Push(gCP, gTP, compilerState, curPair, &vmsend);
        _FCompile_FrameChk(*ret);
        }
    else
        {
        /*  There are no arguments to this function, then it is a bad message send! */       
  		*ret = TERROR("!send: Missing Arguments for the message!");
		_FCompile_FrameExit(*ret);
        }
        
    /*  After any subexpressions have been processed generate code for a function call */
    
    _FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
    _FCompile_MakeINT(prmv[1], VMSEND);
    _FCompile_MakeINT(prmv[2], AMINTEGER);
    _FCompile_MakeIMM(prmv[5], asInt(ret));
    _FCompile_MakeINT(prmv[3], AMWORD);
    _FCompile_MakeCON(prmv[6], *sym);
    
    if(isNullTval(final))
        {
        /*  No return location was specified. During code generation for a function call we */
        /*  may always safely reuse the first temporary which was available when we entered */
        /*  this procedure, it will always be available even if it was used in code generation */
        /*  for the arguments which were pushed for this call. We make this so by resetting  */
        /*  _TempN(compilerState) to __Base__, which is a hidden control variable managed by the */
        /*  _FCompile_* framing macros. */
        
        _FCompile_ResetBase;
        *final = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
        }

    _LastResult(compilerState) = *final;
    _FCompile_MakeINT(prmv[4], asModifier(final));
    _FCompile_MakeSYM(prmv[7], *final);
    
    /*  Finally append the new instruction to the procedure object. */
    
    *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    /*  Set the return value for this function. */
    
    _FCompile_MakeTVL(*ret, _LastResult(compilerState));
    _FCompile_FrameExit(*ret);
    }
else
    {
	SendMsgMissing:
	*ret = TERROR("!compile: send missing  message name argument!");
	_FCompile_FrameExit(*ret);
    }

/*  Move to the message receiver arg or expression */

curPair = _FCompile_CdrPair(curPair);

/*  Save the final location, if any was requested */
    
*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;
        
if (curPair != NULL)
    {
    /*  Push takes care of generating code for VMPUSH instructions to push an arbitrary  */
    /*  number of arguments, and returns the number of arguments for which VMPUSH instructions */
    /*  were generated. */
        
    *ret = FCompile_Push(gCP, gTP, compilerState, curPair, &vmsend);
    _FCompile_FrameChk(*ret);
    }
else
    {
    /*  There are no arguments to this function */
        
    asTag(ret) = TYNUM;
    asInt(ret) = 0;
    }
        
/*  After any subexpressions have been processed generate code for a function call */
    
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], VMSEND);
_FCompile_MakeINT(prmv[2], AMINTEGER);
_FCompile_MakeIMM(prmv[5], asInt(ret));
_FCompile_MakeINT(prmv[3], asModifier(sym));
_FCompile_MakeSYM(prmv[6], *sym);
  
if(isNullTval(final))
    {
    /*  No return location was specified. During code generation for a function call we */
    /*  may always safely reuse the first temporary which was available when we entered */
    /*  this procedure, it will always be available even if it was used in code generation */
    /*  for the arguments which were pushed for this call. We make this so by resetting  */
    /*  _TempN(compilerState) to __Base__, which is a hidden control variable managed by the */
    /*  _FCompile_* framing macros. */
        
    _FCompile_ResetBase;
    *final = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
    }

_LastResult(compilerState) = *final;
_FCompile_MakeINT(prmv[4], asModifier(final));
_FCompile_MakeSYM(prmv[7], *final);
    
/*  Finally append the new instruction to the procedure object. */
    
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
_FCompile_FrameChk(*ret);
    
/*  Set the return value for this function. */
    
_FCompile_MakeTVL(*ret, _LastResult(compilerState));

_FCompile_FrameExit(*ret);

if(asTag(ret) != TYERROR)
    {
    *ret = TERROR("!compile: send has invalid arguments!");
    }
_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms2_super

The  super  special form supports symbolic messaging with any number of arguments. The message 
argument must be an element of type Symbol, and there must be at least one argument. 

The action taken by the symbolic message is dependent upon the type of the first argument.  
The message symbol is looked up in the Methods Structure attached to the types Parent 
(see the defmethodMacro).  If the message symbol is found, the associated Procedure is invoked 
against the whole argument list.  If the message symbol is not found, an error condition results.

 For example
 
    (defineStructure employee:  name:  job:  salary:)  

    (defmethod  employee:  talk(me) "I am an employee.")

    (send talk: (make_employee)) ==> "I am an employee."

    (defineStructure manager: include: employee: department:)  

    (defmethod  manager:  talk(me)  "I am a manager.")

    (send talk: (make_manager)) ==> "I am a manager."

    (super talk: (make_manager)) ==> "I am an employee."

vmsend  argcount  ''message target

Note: 
     0      1           2...
    (send  ''message  arguments ... )
    
Consider the expression:

        (send  ''square  4)

which might generate the code:

 0000: push    aminteg amvoid  amvoid   4               
 0002: send    aminteg amobjec amfboff  1                square:          __T0            
 0006: return  amfboff amvoid  amvoid   __T0            

From this example we can see that when the send special form is processed, first the arguments are
pushed and then a send instruction is generated.

#endif

TVAL    FSpecialForms2_super(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
NUM					vmsend = VMSEND;
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(final);
DeclareTVAL(sym);
DeclareTVAL(ret);
DeclareTVALArray(prmv,9);
_FCompile_EndFrame

/*  Move past the send: special form to the first arg or expression */

curPair = _FCompile_CdrPair(curPair);

/* Abort if rest of expression is NULL */
if (curPair == NULL)
	{
	*ret = TERROR("!super: Missing Arguments!");
	_FCompile_FrameExit(*ret);
	}

/*  Do not handle an indirect message argument. */

if (asTag(&curPair->itsCar) == TYPAIR)
    {
	*ret = TERROR("!super: Invalid indirect message!");
	_FCompile_FrameExit(*ret);
    }
else
/*  Handle the message argument if it is a quoted symbol. */

if (asTag(&curPair->itsCar) == TYQUOTEDSYMBOL)
    {
    /*  We will setup a message send call. */
    
    *sym = curPair->itsCar;

    /*  Make sure we double quote the message so vmsend will */
    /*  know to use the method in the super methods Dictionary. */

    asQuoteCnt(sym) = 2;
    
    /*  Save the final location, if any was requested */
    
    *final = _Result(compilerState);
    _Result(compilerState) = gCP->Tval_VOID;
        
    /*  Move to the first arg or expression */

    curPair = _FCompile_CdrPair(curPair);
    if (curPair != NULL)
        {
        /*  Push takes care of generating code for VMPUSH instructions to push an arbitrary  */
        /*  number of arguments, and returns the number of arguments for which VMPUSH instructions */
        /*  were generated. */
        
        *ret = FCompile_Push(gCP, gTP, compilerState, curPair, &vmsend);
        _FCompile_FrameChk(*ret);
        }
    else
        {
        /*  There are no arguments to this function, then it is a bad message send! */      
		*ret = TERROR("!super: Missing Arguments to the function!");
		_FCompile_FrameExit(*ret);
        }
        
    /*  After any subexpressions have been processed generate code for a function call */
    
    _FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
    _FCompile_MakeINT(prmv[1], VMSEND);
    _FCompile_MakeINT(prmv[2], AMINTEGER);
    _FCompile_MakeIMM(prmv[5], asInt(ret));
    _FCompile_MakeINT(prmv[3], AMWORD);
    _FCompile_MakeCON(prmv[6], *sym);
    
    if(isNullTval(final))
        {
        /*  No return location was specified. During code generation for a function call we */
        /*  may always safely reuse the first temporary which was available when we entered */
        /*  this procedure, it will always be available even if it was used in code generation */
        /*  for the arguments which were pushed for this call. We make this so by resetting  */
        /*  _TempN(compilerState) to __Base__, which is a hidden control variable managed by the */
        /*  _FCompile_* framing macros. */
        
        _FCompile_ResetBase;
        *final = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
        }

    _LastResult(compilerState) = *final;
    _FCompile_MakeINT(prmv[4], asModifier(final));
    _FCompile_MakeSYM(prmv[7], *final);
    
    /*  Finally append the new instruction to the procedure object. */
    
    *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    /*  Set the return value for this function. */
    
    _FCompile_MakeTVL(*ret, _LastResult(compilerState));
    _FCompile_FrameExit(*ret);
    }
else
    {
	*ret = TERROR("!super: Missing Arguments to the function!");
	_FCompile_FrameExit(*ret);
    }

/*  Move to the message receiver arg or expression */

curPair = _FCompile_CdrPair(curPair);

/*  Save the final location, if any was requested */
    
*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;
        
if (curPair != NULL)
    {
    /*  Push takes care of generating code for VMPUSH instructions to push an arbitrary  */
    /*  number of arguments, and returns the number of arguments for which VMPUSH instructions */
    /*  were generated. */
        
    *ret = FCompile_Push(gCP, gTP, compilerState, curPair, &vmsend);
    _FCompile_FrameChk(*ret);
    }
else
    {
    /*  There are no arguments to this function */
        
    asTag(ret) = TYNUM;
    asInt(ret) = 0;
    }
        
/*  After any subexpressions have been processed generate code for a function call */
    
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], VMSEND);
_FCompile_MakeINT(prmv[2], AMINTEGER);
_FCompile_MakeIMM(prmv[5], asInt(ret));
_FCompile_MakeINT(prmv[3], asModifier(sym));
_FCompile_MakeSYM(prmv[6], *sym);
  
if(isNullTval(final))
    {
    /*  No return location was specified. During code generation for a function call we */
    /*  may always safely reuse the first temporary which was available when we entered */
    /*  this procedure, it will always be available even if it was used in code generation */
    /*  for the arguments which were pushed for this call. We make this so by resetting  */
    /*  _TempN(compilerState) to __Base__, which is a hidden control variable managed by the */
    /*  _FCompile_* framing macros. */
        
    _FCompile_ResetBase;
    *final = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
    }

_LastResult(compilerState) = *final;
_FCompile_MakeINT(prmv[4], asModifier(final));
_FCompile_MakeSYM(prmv[7], *final);
    
/*  Finally append the new instruction to the procedure object. */
    
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
_FCompile_FrameChk(*ret);
    
/*  Set the return value for this function. */
    
_FCompile_MakeTVL(*ret, _LastResult(compilerState));

_FCompile_FrameExit(*ret);

if(asTag(ret) != TYERROR)
    {
    *ret = TERROR("!super!");
    }
_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms2_argfetch

The argFetch Special Form returns the nth argument passed to the current Procedure.  
The argFetch special form is used to access arguments in Procedures which have been sent an 
indefinite number of arguments. (That is we do not know how many arguments will be passed until 
run time.)

When a Procedure is defined, if an indefinite list of argument names is specified, at calling 
time the procedure`s arguments are bound to the specified definite arguments respectively.  
Passing too few arguments results in an error message. If too many arguments are passed, the 
remaining arguments can only be accessed via the argCount and argFetch procedures.  
(see the lambda special form).

#endif

TVAL    FSpecialForms2_argfetch(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame

/*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  We advance the curPair to point to the next argument after the "argFetch" keyword. */

curPair = _FCompile_CdrPair(curPair);

if(curPair != NULL)
    {
    /*  There is a value in the "argFetch" argument list. */
    
    /*  Generate code to return the indexed Av value using VMARGFETCH */
    
    if(isNullTval(final))
        {
        /*  Allocate/reserve the result location */

        *final = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
        _FCompile_FrameChk(*final);
        }

    /*  Call FCompile_SetupArg to load the modifier and value TVALs for this item. It will  */
    /*  also generate code for nested sub-expressions. */
    
    *ret = FCompile_SetupArg(gCP, gTP, compilerState, curPair->itsCar, &prmv[2], &prmv[5] );
    ExitOnError(*ret);

    _FCompile_MakeINT(prmv[1], VMARGFETCH);
    _FCompile_MakeINT(prmv[3], asModifier(final));
    _FCompile_MakeSYM(prmv[6], *final);
    _FCompile_MakeINT(prmv[4], AMVOID);
        
    /*  Append the instruction to the current pcode vector */
        
    *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState,7, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    /*  See if there are any more arguments */
    
    curPair = _FCompile_CdrPair(curPair);
    if(curPair != NULL)
        {
        /*  Only one argument is allowed to the argFetch special form. */       
		*ret = TERROR("!argFetch: Too Many Arguments!");
		_FCompile_FrameExit(*ret);
        }
        
    *ret = _LastResult(compilerState) = *final;
    }

_FCompile_FrameExit(*ret);

if(asTag(ret) != TYERROR)
    {
	*ret = TERROR("!argFetch: Missing Index Argument!");
	_FCompile_FrameExit(*ret);
    }
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms2_onError

The onError Special Form sets the current Lambdas error handling function. Any error
result, received by the current Lambda, will be converted to a text argument and passed
to the error handling Lambda specified in the onError special form. 

Note: The onError special form has a scope of the current procedure only.

#endif

TVAL    FSpecialForms2_onError(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame

/*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  We advance the curPair to point to the next argument after the "argFetch" keyword. */

curPair = _FCompile_CdrPair(curPair);

if(curPair != NULL)
    {
    /*  There is a value in the "onError" argument list. */
    
    /*  Generate code to return the indexed Av value using VMONERROR */
    
    if(isNullTval(final))
        {
        /*  Allocate/reserve the result location */

        *final = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
        _FCompile_FrameChk(*final);
        }

    /*  Call FCompile_SetupArg to load the modifier and value TVALs for this item. It will  */
    /*  also generate code for nested sub-expressions. */
    
    *ret = FCompile_SetupArg(gCP, gTP, compilerState, curPair->itsCar, &prmv[2], &prmv[5] );
    ExitOnError(*ret);

    _FCompile_MakeINT(prmv[1], VMONERROR);
    _FCompile_MakeINT(prmv[3], asModifier(final));
    _FCompile_MakeSYM(prmv[6], *final);
    _FCompile_MakeINT(prmv[4], AMVOID);
        
    /*  Append the instruction to the current pcode vector */
        
    *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    /*  See if there are any more arguments */
    
    curPair = _FCompile_CdrPair(curPair);
    if(curPair != NULL)
        {
        /*  Only one argument is allowed to the onError special form. */      
 		*ret = TERROR("!onError: Too Many Arguments!");
		_FCompile_FrameExit(*ret);
        }
        
    *ret = _LastResult(compilerState) = *final;
    }

_FCompile_FrameExit(*ret);

if(asTag(ret) != TYERROR)
    {
 	*ret = TERROR("!onError: Missing Lambda Argument!");
    }
_FCompile_FrameExit(*ret);
}
