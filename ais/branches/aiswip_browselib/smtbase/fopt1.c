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

#define _C_FOPTIMIZE1
#define _SMARTBASE

#if 0 
FOptimize1.c

This file contains some of the procedures required to support the SmartLisp compiler 
local optimization processing. Included here are the procedures which generate virtual
machine instructions as a replacement for function calls. We include support for the basic
arithmetic instructions in the virtual machine as well as support for simple boolean expressions.

NOTE:   All examples of generated code are for illustrative purposes only, the actual code
        which would be generated for a given expression may be very different depending on the
        level of optimization in effect at compilation time. They are meant as an aid in 
        understanding, and not as explicit examples of the code that the compiler will actually
        produce at any given time.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fopt1.h"
#include "fsforms3.h"
#include "tpair.h"
#include "terror.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_Init

For the six booleans shown below we generate conditional jump instructions as indicated to 
return TRUE or FALSE from the boolean expression.

    =   via VMJMPNE
    >   via VMJMPLE
    <   via VMJMPGE
    >=  via VMJMPLT
    <=  via VMJMPGT
    <>  via VMJMPEQ

	bcompareLE   via vmnatJmpLEInteger
	bcompareLT   via vmnatJmpLTInteger
	bcompareEQ   via vmnatJmpEQInteger
	bcompareNE   via vmnatJmpNEInteger
	bcompareGE   via vmnatJmpGEInteger
	bcompareGT   via vmnatJmpGTInteger

	ccompareLE   via vmnatJmpLEInteger
	ccompareLT   via vmnatJmpLTInteger
	ccompareEQ   via vmnatJmpEQInteger
	ccompareNE   via vmnatJmpNEInteger
	ccompareGE   via vmnatJmpGEInteger
	ccompareGT   via vmnatJmpGTInteger

	icompareLE   via vmnatJmpLEInteger
	icompareLT   via vmnatJmpLTInteger
	icompareEQ   via vmnatJmpEQInteger
	icompareNE   via vmnatJmpNEInteger
	icompareGE   via vmnatJmpGEInteger
	icompareGT   via vmnatJmpGTInteger

	ncompareLE   via vmnatJmpLENumber
	ncompareLT   via vmnatJmpLTNumber
	ncompareEQ   via vmnatJmpEQNumber
	ncompareNE   via vmnatJmpNENumber
	ncompareGE   via vmnatJmpGENumber
	ncompareGT   via vmnatJmpGTNumber

The special forms and instructions in the following list are generated whenever a reasonable
substitution for a function call can be made.

    VMARGCOUNT
    VMSELF
    VMADD
    VMADDI
	VMADDN
	VMDIVN
	VMMULN
	VMSUBN
	VMCADD
	VMCDIV
	VMCMUL
	VMCSUB
    VMDIV
    VMDIVI
    VMDIVR
    VMDIVRI
	VMIADD
	VMIDIV
	VMIDIVR
	VMIMUL
	VMISUB
    VMMUL
    VMMULI
	VMNADD
	VMNDIV
	VMNDIVR
	VMNMUL
	VMNSUB
    VMIAND
    VMIANDB
    VMIOR
    VMIORB
    VMIXOR
    VMIXORB
    VMSHL
    VMSHR
    VMSUB
    VMSUBI
    VMXOR

	VMREFTEXT
	VMREFSTRING
	VMSETSTRING
	VMREFSYMBOL
	VMREFCAR
	VMSETCAR
	VMREFCDR
	VMSETCDR
	VMREFVECTOR
	VMSETVECTOR
	VMREFSTRVALUE
	VMSETSTRVALUE
	VMREFSTRKEY
	VMSETSTRKEY
	VMREFDICVALUE
	VMSETDICVALUE
	VMREFDICKEY
	VMSETDICKEY
	VMREFDIRVALUE
	VMSETDIRVALUE
	VMREFDIRKEY
	VMSETDIRKEY
	VMREFBITVECTOR
	VMSETBITVECTOR
	VMREFBYTVECTOR
	VMSETBYTVECTOR
	VMREFPCDVECTOR
	VMSETPCDVECTOR
	VMREFOBJVECTOR
	VMSETOBJVECTOR
	VMREFSHTVECTOR
	VMSETSHTVECTOR
	VMREFINTVECTOR
	VMSETINTVECTOR
	VMREFNUMVECTOR
	VMSETNUMVECTOR
	VMREFFLTVECTOR
	VMSETFLTVECTOR
	VMREFMATRIX
	VMSETMATRIX
	VMREFNUMMATRIX
	VMSETNUMMATRIX
	VMREFNUMVECTOR


#endif

TVAL FOptimize1_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
EndFrame
 
/*  We maintain a static flag to track whether this package has been initialized. */

if(gCP->FOpt1_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FOpt1_Initialized = 1;
    
/*  Initialize the second phase optimizations. */

FOptimize2_Init(gCP,gTP);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_set

This is the procedure which is used to generate vminstructions directly for VMSET
where it is possible.

For example:
    (set target index source)

might generate:

 0000: vmset   aminteg amfboff amfboff  index            source           target            
 0008: return  amfboff amvoid  amvoid   target           

For example:
    (set target source)

might generate:

 0000: vmset   amvoid  amfboff amfboff  source           target           
 0008: return  amfboff amvoid  amvoid   source            

From this example we can see how a series of VMSET instructions will be generated which 
cascade their return result.
For example:

    (setq x (set target source))

might generate:

 0000: vmset   aminteg amfboff amfboff  index            source           target           
 0008: vmmove  amfboff amfboff amvoid   target           x            
 0016: return  amfboff amvoid  amvoid   x            

#endif

TVAL FOptimize1_set(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
TYPE                tag;
NUM                 ndx;
NUM                 total;
NUM                 mod;
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVAL(intermediate);
DeclareTVAL(car);
DeclareTVAL(swapMod);
DeclareTVAL(swapVal);
DeclareTVALArray(terms,3);
DeclareTVALArray(prmv,8);


_FCompile_EndFrame

/*  Save the final location, if any was requested */

*final = _Result(compilerState);

/*  Void _Result(compilerState) to guard against side effects. */

_Result(compilerState) = gCP->Tval_VOID;

/*  Initialize control indices for code generation. */

ndx = 2;
total = 0;

/*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));

do 
    {
    /*  Loop through all of the arguments in the argument list (maximum of 2). */
    
    *car = curPair->itsCar;
    switch((tag = asTag(car)))
        {
        /*  Process the next argument */
        
        case TYPAIR:
            /*  Let the sub-expression allocate any needed temporary variables. */
            /*  Note1: This way they will be given optimized preferred types.   */
            /*  Note2: Setup a temp pair and recognize the sub-expression.      */
            if ((terms[0].Modifier == AMREGISTER) && ((terms[0].DeclaredType == TYCHARPOINTER) || (terms[0].DeclaredType == TYINTPOINTER) || (terms[0].DeclaredType == TYSHORTPOINTER) || (terms[0].DeclaredType == TYLONGPOINTER)))
				{
				*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);
				_FCompile_FrameChk(*ret);
				}
			else
            if ((terms[0].DeclaredType == TYINTVECTOR) || (terms[0].DeclaredType == TYSHORTVECTOR) || (terms[0].DeclaredType == TYLONGVECTOR) || (terms[0].DeclaredType == TYBYTEVECTOR) || (terms[0].DeclaredType == TYSTRING))
				{
				*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);
				_FCompile_FrameChk(*ret);
				}
			else
            if ((terms[0].Modifier == AMREGISTER) && ((terms[0].DeclaredType == TYFLOATPOINTER) || (terms[0].DeclaredType == TYREALPOINTER)))
				{
				if (ndx == 3)
					{				
					*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);
					_FCompile_FrameChk(*ret);
					}
				else
					{
					*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);
					_FCompile_FrameChk(*ret);
					}
				}
			else
            if ((terms[0].DeclaredType == TYFLTVECTOR) || (terms[0].DeclaredType == TYNUMMATRIX) || (terms[0].DeclaredType == TYNUMVECTOR))
				{
				if (ndx == 3)
					{				
					*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);
					_FCompile_FrameChk(*ret);
					}
				else
					{
					*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);
					_FCompile_FrameChk(*ret);
					}
				}
			else
				{
				*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
				_FCompile_FrameChk(*ret);
				}

            /*  Setup a temp pair and send the sub-expression to the recognizer. */
            
            tmpPair = TPair_New(gCP,gTP);
            FObject_Perm(gCP,gTP,(TObject*)tmpPair, TRUE);

            tmpPair->itsCar = *car;
            _Result(compilerState) = *ret;
            *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
            FObject_Perm(gCP,gTP,(TObject*)tmpPair, FALSE);
            _FCompile_FrameChk(*ret);
            
            /*  Format the result as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], asModifier(ret));
            _FCompile_MakeSYM(prmv[ndx+3], *ret);
            terms[ndx-2] = *ret;
        break;
        
        case TYSYMBOL:
            /*  This is a symbol reference, search Av Tv Pv Gv to resolve it.  */
            
            aSymbol = asSymbol(car);
            *ret = FCompile_LookUp(gCP, gTP, compilerState, aSymbol);
                
            /*  Format the result as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], asModifier(ret));
            _FCompile_MakeSYM(prmv[ndx+3], *ret);
            terms[ndx-2] = *ret;
        break;
        
        case TYNUM:
            /*  Format the TYNUM as an argument for FOptimize_OptimizePcode */

            _FCompile_MakeINT(prmv[ndx], AMWORD);
            _FCompile_MakeCON(prmv[ndx+3], *car);
            terms[ndx-2] = *car;
			break;
        
        default:
			if (ndx == 2)
				{
				/* This set must be implemented as a call to the built-in set function. */
				_FCompile_FrameExit(gCP->Tval_FALSE);
				}

            /*  Setup the default modifier */
            
            mod = AMWORD;
                
            if(tag == TYQUOTEDPAIR)
                {
                /*  Decrement the quote count on any quoted thing. */
                
                if(--asQuoteCnt(car) == 0)
                    asTag(car) = TYPAIR;
                }
            else
            if(tag == TYQUOTEDSYMBOL)
                {
                if (ndx == 2)
                    {
                    /*  Treat any quoted target as a global symbol. */
                    asQuoteCnt(car) = 0;
                    asModifier(car) = AMGVOFFSET;
                    tag = asTag(car) = TYSYMBOL;
                    mod = AMGVOFFSET;
                    }
                else
                    {
                    /*  Decrement the quote count on any quoted thing. */
                    if(--asQuoteCnt(car) == 0)
                        tag = asTag(car) = TYSYMBOL;
                    }
                }

            /*  Format the result of the default processing  as an argument for FOptimize_OptimizePcode */

            _FCompile_MakeINT(prmv[ndx], mod);
            _FCompile_MakeCON(prmv[ndx+3], *car);
			terms[ndx-2] = *car;
        break;
        }
    
    /*  Increment the FOptimize_OptimizePcode argument index. */
    
    ndx++;
    } while ((curPair = _FCompile_CdrPair(curPair)) != NULL);
    
/*  Generate the vmset instruction for the arguments. */

if(ndx == 5)
    {
    /*  If ndx == 5 then we have loaded three arguments for this instruction */
    /*  and it is time to generate code. */
    
    /*  Reset the first available temp to that at the start of this function. */
    
    _FCompile_ResetBase;
    
    /* Swap arguments before calling append as vmset requires: index source target. */
    
    *swapMod = prmv[3];
    *swapVal = prmv[6];
    prmv[3] = prmv[4];
    prmv[6] = prmv[7];
    prmv[4] = prmv[2];
    prmv[7] = prmv[5];
    prmv[2] = *swapMod;
    prmv[5] = *swapVal;

    /*  Complete the target assignment for the vmset instruction. */
    
    _FCompile_MakeINT(prmv[1], instruction);

    *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState, 8, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    *intermediate = terms[0];
    }
else
	{
	/* This set must be implemented as a call to the built-in set function. */
	_FCompile_FrameExit(gCP->Tval_FALSE);
	}
    
/*  Do we need to move the result into a final target ? */

if (final->Tag != TYVOID)
    {
    /*  The final result is a temp so we can gen code directly to that location. */
    
    _FCompile_MakeINT(prmv[1], VMMOVE);
    _FCompile_MakeINT(prmv[2], asModifier(intermediate));
    _FCompile_MakeINT(prmv[3], asModifier(final));
    _FCompile_MakeINT(prmv[4], AMVOID);
    _FCompile_MakeSYM(prmv[5], *intermediate);
    _FCompile_MakeSYM(prmv[6], *final);
    *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    *intermediate = *final;
    }

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

_LastResult(compilerState) = *intermediate;

_FCompile_FrameExit(_LastResult(compilerState));

if(asTag(ret) != TYERROR)
    {
    *ret = TERROR("!SmartLisp Compiler: set!");
    }
_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_add

This is the procedure which is used to generate vminstructions directly for VMADD and VMADDI
where it is possible.

If zero arguments are specified generate 0, else generate the sum of the arguments.

For example:
    (+ 1 2 3)

might generate:

 0000: add     aminteg aminteg amfboff  2                1                __T0            
 0004: add     aminteg amfboff amfboff  3                __T0             __T0            
 0008: return  amfboff amvoid  amvoid   __T0            

From this example we can see how a series of VMADD instructions will be generated which 
cascade their result via a temporary variable.

#endif

TVAL FOptimize1_add(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
NUM                 length;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,aPair);
EndFrame

if (curPair != NIL)
    {
    /*  We have at least one argument to process. */
    
    for( length = 1, tmpPair = curPair; (tmpPair = _FCompile_CdrPair(tmpPair)); length++) {}
    
    if(length == 1)
        {
        /*  One argument is handled as a special case. */
        
        tmpPair = TPair_New(gCP,gTP);
        tmpPair->itsCar = curPair->itsCar;
        
		switch (instruction)
			{
			case VMADDI:
			case VMIADD:
			case VMISUB:
				/*  For the vmaddi case we gen code for addi arg 0  */
            
				aPair = asPair(&tmpPair->itsCdr);
				aPair = TPair_New(gCP,gTP);
				asObject(&tmpPair->itsCdr) = (TObject*)aPair;
				asTag(&tmpPair->itsCdr) = TYPAIR;
				asTag(&asPair(&tmpPair->itsCdr)->itsCar) = TYNUM;
				asInt(&asPair(&tmpPair->itsCdr)->itsCar) = 0;
				_LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, tmpPair, instruction);
				ExitOnError(*ret);
				break;

			case VMNADD:
			case VMNSUB:
			case VMADDN:
			case VMSUBN:
				/*  For the vmnadd case we gen code for nadd arg 0  */
            
				aPair = asPair(&tmpPair->itsCdr);
				aPair = TPair_New(gCP,gTP);
				asObject(&tmpPair->itsCdr) = (TObject*)aPair;
				asTag(&tmpPair->itsCdr) = TYPAIR;
				asTag(&asPair(&tmpPair->itsCdr)->itsCar) = TYREAL;
				asReal(&asPair(&tmpPair->itsCdr)->itsCar) = 0;
				_LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, tmpPair, instruction);
				ExitOnError(*ret);
				break;

			case VMCADD:
			case VMCSUB:
				/*  For the vmcadd case we gen code for cadd arg 0  */
            
				aPair = asPair(&tmpPair->itsCdr);
				aPair = TPair_New(gCP,gTP);
				asObject(&tmpPair->itsCdr) = (TObject*)aPair;
				asTag(&tmpPair->itsCdr) = TYPAIR;
				asPair(&tmpPair->itsCdr)->itsCar.Tag = TYCHAR;
				asPair(&tmpPair->itsCdr)->itsCar.u.Text[0] = 0;
				asPair(&tmpPair->itsCdr)->itsCar.u.Text[1] = 0;
				_LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, tmpPair, instruction);
				ExitOnError(*ret);
				break;

			case VMIMUL:
			case VMIDIV:
			case VMIDIVR:
				/*  For the vmimul case we gen code for imul arg 1  */
            
				aPair = asPair(&tmpPair->itsCdr);
				aPair = TPair_New(gCP,gTP);
				asObject(&tmpPair->itsCdr) = (TObject*)aPair;
				asTag(&tmpPair->itsCdr) = TYPAIR;
				asTag(&asPair(&tmpPair->itsCdr)->itsCar) = TYNUM;
				asInt(&asPair(&tmpPair->itsCdr)->itsCar) = 1	;
				_LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, tmpPair, instruction);
				ExitOnError(*ret);
				break;

			case VMNMUL:
			case VMNDIV:
			case VMNDIVR:
			case VMMULN:
			case VMDIVN:
				/*  For the vmnmul case we gen code for nmul arg 1  */
            
				aPair = asPair(&tmpPair->itsCdr);
				aPair = TPair_New(gCP,gTP);
				asObject(&tmpPair->itsCdr) = (TObject*)aPair;
				asTag(&tmpPair->itsCdr) = TYPAIR;
				asTag(&asPair(&tmpPair->itsCdr)->itsCar) = TYREAL;
				asReal(&asPair(&tmpPair->itsCdr)->itsCar) = 1	;
				_LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, tmpPair, instruction);
				ExitOnError(*ret);
				break;

			case VMCMUL:
			case VMCDIV:
				/*  For the vmcmul case we gen code for cmul arg true  */
            
				aPair = asPair(&tmpPair->itsCdr);
				aPair = TPair_New(gCP,gTP);
				asObject(&tmpPair->itsCdr) = (TObject*)aPair;
				asTag(&tmpPair->itsCdr) = TYPAIR;
				asPair(&tmpPair->itsCdr)->itsCar.Tag = TYCHAR;
				asPair(&tmpPair->itsCdr)->itsCar.u.Text[0] = 1;
				asPair(&tmpPair->itsCdr)->itsCar.u.Text[1] = 0;
				_LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, tmpPair, instruction);
				ExitOnError(*ret);
				break;

			default:
				/*  If we have one argument then we generate code to move that arg into the */
				/*  return location.  */
				/*  For example */
				/*      (+ 1) */
				/*  might generate: */
				/*       0000: move    aminteg amfboff amvoid  1                __T0             */
				/*       0003: return  amfboff amvoid  amvoid   __T0             */
            
				_LastResult(compilerState) = *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
				break;

			} /* end switch */
        }
    else
        {
        /*  We have more than one argument, we will utilize the FOptimize1_arguments which knows */
        /*  how to generate code for vm instructions which must cascade their results for an */
        /*  arbitrary number of arguments. */
        
        _LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, curPair, instruction);
        ExitOnError(*ret);
        }
        
    }
else
    {
    /*  For zero arguments we generate 0 by creating a tmppair with that for a car an  */
    /*  then sending it to FCompile_Recognize. */
    
    tmpPair = TPair_New(gCP,gTP);
    asTag(&tmpPair->itsCar) = TYNUM;
    asInt(&tmpPair->itsCar) = 0;
    _LastResult(compilerState) = *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
    } 

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_div
This is the procedure which is used to generate vminstructions directly for VMDIV and VMDIVI
where it is possible.

If one argument is specified generate its reciprocal, more than one argument, divide.

For example:

    (/ 100 2 5)

might generate:

 0000: div     aminteg aminteg amfboff  2                100              __T0            
 0004: div     aminteg amfboff amfboff  5                __T0             __T0            
 0008: return  amfboff amvoid  amvoid   __T0            

From this example we can see how a series of VMDIV instructions will be generated which 
cascade their result via a temporary variable.

#endif

TVAL FOptimize1_div(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
NUM                 length;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,tmpPair);
EndFrame

if (curPair != NIL)
    {
    /*  We have at least one argument to process. */
    
    for (length = 1, tmpPair = curPair; (tmpPair = _FCompile_CdrPair(tmpPair)); length++) {}
    
    if(length == 1)
        {
        /*  For the special case of one argument we generate the arguments reciprocal. */
        /*  return location.  */
        /*  For example */
        /*      (/ 100) */
        /*  might generate: */
        /*      0000: div     aminteg aminteg amfboff 100              1                __T0             */
        /*      0004: return  amfboff amvoid  amvoid   __T0             */
        
        tmpPair = TPair_New(gCP,gTP);
        asInt(&tmpPair->itsCar) = 1;
        asTag(&tmpPair->itsCar) = TYNUM;
        asObject(&tmpPair->itsCdr) = (TObject*)TPair_New(gCP,gTP);
        asTag(&tmpPair->itsCdr) = TYPAIR;
        asPair(&tmpPair->itsCdr)->itsCar = curPair->itsCar;
        }
    else
        tmpPair = curPair;

    /*  We will utilize the FOptimize1_arguments which knows how to generate code for vm  */
    /*  instructions which must cascade their results for an arbitrary number of arguments. */
    
    _LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, tmpPair, instruction);
    ExitOnError(*ret);
    }
else
    {
    /*  For zero arguments we generate 0 by creating a tmppair with that for a car an  */
    /*  then sending it to FCompile_Recognize. */
    
    tmpPair = TPair_New(gCP,gTP);
    asTag(&tmpPair->itsCar) = TYNUM;
    asInt(&tmpPair->itsCar) = 0;
    _LastResult(compilerState) = *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
    } 

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_oneOpAdd

This is the procedure which is used to generate vminstructions directly for the sub1 and add1
cProcedures.

One argument is required.

#endif

TVAL FOptimize1_oneOpAdd(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM addThis)
{
NUM                 length;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,tmpPair);
EndFrame

if (curPair != NIL)
    {
    /*  We have at least one argument to process. */
    
    for (length = 1, tmpPair = curPair; (tmpPair = _FCompile_CdrPair(tmpPair)); length++) {}
    
    if(length == 1)
        {
        /*  We convert (1+ n) to be (+ 1 n) */
        
        tmpPair = TPair_New(gCP,gTP);
        asInt(&tmpPair->itsCar) = addThis;
        asTag(&tmpPair->itsCar) = TYNUM;
        asObject(&tmpPair->itsCdr) = (TObject*)TPair_New(gCP,gTP);
        asTag(&tmpPair->itsCdr) = TYPAIR;
        asPair(&tmpPair->itsCdr)->itsCar = curPair->itsCar;
        }
    else
        {
        goto BadCleanUp;
        }

    /*  We will utilize the FOptimize1_arguments which knows how to generate code for vm  */
    /*  instructions which must cascade their results for an arbitrary number of arguments. */
    
    _LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, tmpPair, VMADD);
    ExitOnError(*ret);
    }
else
    {
    goto BadCleanUp;
    } 

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

FrameExit(*ret);

BadCleanUp:
if(asTag(ret) != TYERROR)
    {
    *ret = TERROR("!SmartLisp Compiler: illegal sub1 or add1 operation!");
    }
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_mul

This is the procedure which is used to generate vminstructions directly for VMMUL and VMMULI
where it is possible.

If zero arguments are specified generate 1, else generate the product.

For example:
    (+ 1 2 3)

might generate:

 0000: mul     aminteg aminteg amfboff  2                1                __T0            
 0004: mul     aminteg amfboff amfboff  3                __T0             __T0            
 0008: return  amfboff amvoid  amvoid   __T0            

From this example we can see how a series of VMMUL instructions will be generated which 
cascade their result via a temporary variable.

#endif

TVAL FOptimize1_mul(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
NUM                 length;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,tmpPair);
EndFrame

if (curPair != NIL)
    {
    /*  We have at least one argument to process. */
    
    for (length = 1, tmpPair = curPair; (tmpPair = _FCompile_CdrPair(tmpPair)); length++) {}
    
    if(length == 1)
        {
        /*  If we have one argument then we generate code to move that arg into the */
        /*  return location.  */
        /*  For example */
        /*      (+ 1) */
        /*  might generate: */
        /*       0000: move    aminteg amfboff amvoid  1                __T0             */
        /*       0003: return  amfboff amvoid  amvoid   __T0             */
        
        tmpPair = TPair_New(gCP,gTP);
        tmpPair->itsCar = curPair->itsCar;
        _LastResult(compilerState) = *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
        }
    else
        {
        /*  We have more than one argument, we will utilize the FOptimize1_arguments which knows */
        /*  how to generate code for vm instructions which must cascade their results for an */
        /*  arbitrary number of arguments. */

        _LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, curPair, instruction);
        ExitOnError(*ret);
        }
    }
else
    {
    /*  For zero arguments we generate 1 by creating a tmppair with that for a car an  */
    /*  then sending it to FCompile_Recognize. */
    
    tmpPair = TPair_New(gCP,gTP);
    asTag(&tmpPair->itsCar) = TYNUM;
    asInt(&tmpPair->itsCar) = 1;
    _LastResult(compilerState) = *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
    } 

/*  Return the _LastResult(compilerState) which contains the location where the last result */

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_sub

This is the procedure which is used to generate vminstructions directly for VMSUB
where it is possible.

If one argument is specified generate its negative, else generate the difference.

For example:
    (- 1 2 3)

might generate:

 0000: sub     aminteg aminteg amfboff 2                1                __T0            
 0004: sub     aminteg amfboff amfboff  3                __T0             __T0            
 0008: return  amfboff amvoid  amvoid   __T0            

From this example we can see how a series of VMSUB instructions will be generated which 
cascade their result via a temporary variable.

#endif

TVAL FOptimize1_sub(LpXCONTEXT gCP, LpTHREAD gTP,TVector* compilerState, TPair* curPair)
{
NUM                 length;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,tmpPair);
EndFrame

if (curPair != NIL)
    {
    /*  We have at least one argument to process. */

    for (length = 1, tmpPair = curPair; (tmpPair = _FCompile_CdrPair(tmpPair)); length++) {}
    
    if(length == 1)
        {
        /*  One argument is handled as a special case. We generate its negative by creating a code */
        /*  sequence which multiplies the argument by -1. */
        
        tmpPair = TPair_New(gCP,gTP);
        asInt(&tmpPair->itsCar) = -1;
        asTag(&tmpPair->itsCar) = TYNUM;
        asObject(&tmpPair->itsCdr) = (TObject*)TPair_New(gCP,gTP);
        asTag(&tmpPair->itsCdr) = TYPAIR;
        asPair(&tmpPair->itsCdr)->itsCar = curPair->itsCar;
        _LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, tmpPair, VMMUL);
        ExitOnError(*ret);
        }
    else
        {
        /*  We have more than one argument, we will utilize the FOptimize1_arguments which knows */
        /*  how to generate code for vm instructions which must cascade their results for an */
        /*  arbitrary number of arguments. */

        _LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, curPair, VMSUB);
        ExitOnError(*ret);
        }
        
    }
else
    {
    /*  For zero arguments we generate 0 by creating a tmppair with that for a car an  */
    /*  then sending it to FCompile_Recognize. */
    
    tmpPair = TPair_New(gCP,gTP);
    asTag(&tmpPair->itsCar) = TYNUM;
    asInt(&tmpPair->itsCar) = 0;
    _LastResult(compilerState) = *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
    } 

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_subi

This is the procedure which is used to generate vminstructions directly for VMSUBI
where it is possible.

If one argument is specified generate its negative, else generate the difference.

For example:
    (subi 1 2 3)

might generate:

 0000: subi    aminteg aminteg amfboff  2                1                __T0            
 0004: subi    aminteg amfboff amfboff  3                __T0             __T0            
 0008: return  amfboff amvoid  amvoid   __T0            

From this example we can see how a series of VMSUBI instructions will be generated which 
cascade their result via a temporary variable.

#endif

TVAL FOptimize1_subi(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
NUM                 length;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,tmpPair);
EndFrame

if (curPair != NIL)
    {
    /*  We have at least one argument to process. */

    for (length = 1, tmpPair = curPair; (tmpPair = _FCompile_CdrPair(tmpPair)); length++) {}
    
    if(length == 1)
        {
        /*  One argument is handled as a special case. We generate its negative by creating a code */
        /*  sequence which multiplies the argument by -1. */
        
        tmpPair= TPair_New(gCP,gTP);
        asInt(&tmpPair->itsCar) = -1;
        asTag(&tmpPair->itsCar) = TYNUM;
        asObject(&tmpPair->itsCdr) = (TObject*)TPair_New(gCP,gTP);
        asTag(&tmpPair->itsCdr) = TYPAIR;
        asPair(&tmpPair->itsCdr)->itsCar = curPair->itsCar;
        _LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, tmpPair, VMMULI);
        ExitOnError(*ret);
        }
    else
        {
        /*  We have more than one argument, we will utilize the FOptimize1_arguments which knows */
        /*  how to generate code for vm instructions which must cascade their results for an */
        /*  arbitrary number of arguments. */

        _LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, curPair, VMSUBI);
        ExitOnError(*ret);
        }
        
    }
else
    {
    /*  For zero arguments we generate 0 by creating a tmppair with that for a car an  */
    /*  then sending it to FCompile_Recognize. */
    
    tmpPair = TPair_New(gCP,gTP);
    asTag(&tmpPair->itsCar) = TYNUM;
    asInt(&tmpPair->itsCar) = 0;
    _LastResult(compilerState) = *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
    } 

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_twoOp

Handle general case of optimizing instructions which allow two and only two arguments.
These instructions include:  VMDIVR VMSHL and VMSHR.

#endif

TVAL FOptimize1_twoOp(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
NUM                 length;
CHAR				tmpBuffer[100];
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,tmpPair);

EndFrame

if (curPair != NULL)
    {
    /*  We have at least one argument to process. */
    
    for (length = 1, tmpPair = curPair; (tmpPair = _FCompile_CdrPair(tmpPair)); length++) {}
    
    if(length == 2)
        {
        /*  We have exactly two arguments, we will utilize the FOptimize1_arguments which knows */
        /*  how to generate code for vm instructions which must cascade their results for an */
        /*  arbitrary number of arguments. */

        _LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, curPair, instruction);
        ExitOnError(*ret);
        }
    else
        {
        /*  We do not have not two arguments for this procedure, this is an error. */
        
        goto BadCleanUp;
        }
        
    }
else
    {
    /*  We do not have not two arguments for this procedure, this is an error. */
    
    goto BadCleanUp;
    }

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

FrameExit(*ret);

BadCleanUp:

/* Construct an Error message */
strcpy(tmpBuffer, "!Smartlisp:");
FDebug_INSToString(gCP, gTP, instruction,  &tmpBuffer[strlen(tmpBuffer)]);   /* convert the instruction opcode to a string */
strcat(tmpBuffer, (const char*)" has too many arguments!");

*ret = TERROR(tmpBuffer);
FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_zeroOp

Handle general case of optimizing instructions which require no arguments.
These instructions include: VMSELF and VMARGCOUNT.

#endif

TVAL FOptimize1_zeroOp(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
CHAR				tmpBuffer[100];
_FCompile_StartFrame
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVALArray(prmv,8);

_FCompile_EndFrame

/*  Void _Result(compilerState) to guard against side effects. */
*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

if (curPair != NIL)
    {
    /*  We have arguments for this procedure, this is an error. */

    goto BadCleanUp;
    }
else
    {
    if(asTag(final) == TYVOID)
        {
        /*  Insure that a result location has been specified for this operation. */
        
        *final = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
        }
            
    /*  Generate the code sequence for an instruction with zero arguments. */
            
    _FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
    _FCompile_MakeINT(prmv[1], instruction);
    _FCompile_MakeINT(prmv[2], asModifier(final));
    _FCompile_MakeSYM(prmv[5], *final);
    _FCompile_MakeINT(prmv[3], AMVOID);
    _FCompile_MakeINT(prmv[4], AMVOID);
    *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 6, &prmv[0]);
    _FCompile_FrameChk(*ret);
    _LastResult(compilerState) = *ret = *final;
    }

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

_FCompile_FrameExit(*ret);

BadCleanUp:


/* Construct an Error message */
strcpy(tmpBuffer, "!Smartlisp:");
FDebug_INSToString(gCP, gTP, instruction,  &tmpBuffer[strlen(tmpBuffer)]);   /* convert the instruction opcode to a string */
strcat(tmpBuffer, (const char*)" has too many arguments!");
*ret = TERROR(tmpBuffer);
_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_atLeastOneOp

This is the procedure which is used to generate vminstructions directly for 
VMXOR, VMAND, VMOR, VMIXOR, VMIAND, VMIOR, VMIXORB, VMIANDB, VMIORB where it 
is possible.

No args is an error, a single arg returns the thing itself, else generate code for the VM.

For example:
    (bitwiseXor 2 4 8 )

might generate:

 0000: xor     aminteg aminteg amfboff  4                2                __T0            
 0004: xor     aminteg amfboff amfboff  8                __T0             __T0            
 0008: return  amfboff amvoid  amvoid   __T0            

From this example we can see how a series of VMXOR instructions will be generated which 
cascade their result via a temporary variable.

#endif

TVAL FOptimize1_atLeastOneOp(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
NUM                 length;
CHAR				tmpBuffer[100];
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TPair,tmpPair);
EndFrame

if (curPair != NIL)
    {
    /*  We have at least one argument to process. */
    
    for (length = 1, tmpPair = curPair; (tmpPair = _FCompile_CdrPair(tmpPair)); length++) {}
    
    if(length == 1)
        {
        /*  For one argument we generate whatever that arg returns */
        
        tmpPair = TPair_New(gCP,gTP);
        tmpPair->itsCar = curPair->itsCar;
        _LastResult(compilerState) = *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
        }
    else
        {
        /*  We have more than one argument, we will utilize the FOptimize1_arguments which knows */
        /*  how to generate code for vm instructions which must cascade their results for an */
        /*  arbitrary number of arguments. */
        
        _LastResult(compilerState) = *ret = FOptimize1_arguments(gCP, gTP, compilerState, curPair, instruction);
        ExitOnError(*ret);
        }
    }
else
    {
    /*  We have no arguments for this procedure, this is an error. */

    goto BadCleanUp;
    }
    
/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

FrameExit(*ret);

BadCleanUp:

/* Construct an Error message */
strcpy(tmpBuffer, "!Smartlisp:");
FDebug_INSToString(gCP, gTP, instruction, &tmpBuffer[strlen(tmpBuffer)]);   /* convert the instruction opcode to a string */
strcat(tmpBuffer, (const char*)" is missing arguments!");

*ret = TERROR(tmpBuffer);
FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_Calls

This is the dispatch routine for optimizing function calls. Returns TObject_TRUE if this sub-list
was optimized, and TObject_FALSE if it should be handled by the normal compiler call processing.

When we enter this routine we are given the symbol for the call and the curPair is positioned at
the first element of the list after the call symbol.

For example:

    (+  1 2 3 4)
        ^ The curpair would be positioned here at entry.

#endif

TVAL FOptimize1_Calls(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol)
{
TVAL					cs;
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,setqPair);
DeclareOBJ(TPair,symPair);
DeclareTVAL(ret);
EndFrame

/*  Extract the global value from the symbol. This allows the processing of aliases. */

cs.u.Symbol = callSymbol;
cs.Tag = TYSYMBOL;
aSymbol = (TSymbol*)asObject(&callSymbol->itsGlobalValue);

/*  Check for each optmizable symbol in turn. */

if( (aSymbol == gCP->FCompile_refSym) &&
    (curPair != NIL) &&
    ((curPair->itsCdr.Tag != TYPAIR) ||
     (asPair(&curPair->itsCdr)->itsCdr.Tag != TYPAIR)))
    {
    *ret = FOptimize1_ref(gCP, gTP, compilerState, curPair, VMREF);
    }
else
if( (aSymbol == gCP->FCompile_setSym) &&
    (curPair != NIL) &&
    ((curPair->itsCdr.Tag == TYPAIR) &&
     (asPair(&curPair->itsCdr)->itsCdr.Tag == TYPAIR) &&
     (asPair(&asPair(&curPair->itsCdr)->itsCdr)->itsCdr.Tag != TYPAIR)))
    {
    *ret = FOptimize1_set(gCP, gTP, compilerState, curPair, VMSET);
    }
else
if( aSymbol == gCP->FCompile_addSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMADD);
    }
else
if( aSymbol == gCP->FCompile_divSym)
    {
    *ret = FOptimize1_div(gCP, gTP, compilerState, curPair, VMDIV);
    }
else
if( aSymbol == gCP->FCompile_iaddSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMIADD);
    }
else
if( aSymbol == gCP->FCompile_idivSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMIDIV);
    }
else
if( aSymbol == gCP->FCompile_imodSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMIDIVR);
    }
else
if( aSymbol == gCP->FCompile_imulSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMIMUL);
    }
else
if( aSymbol == gCP->FCompile_isubSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMISUB);
    }
else
if( aSymbol == gCP->FCompile_naddSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMNADD);
    }
else
if( aSymbol == gCP->FCompile_ndivSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMNDIV);
    }
else
if( aSymbol == gCP->FCompile_nmodSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMNDIVR);
    }
else
if( aSymbol == gCP->FCompile_nmulSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMNMUL);
    }
else
if( aSymbol == gCP->FCompile_nsubSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMNSUB);
    }
else
if( aSymbol == gCP->FCompile_mulSym)
    {
    *ret = FOptimize1_mul(gCP, gTP, compilerState, curPair, VMMUL);
    }
else
if( aSymbol == gCP->FCompile_subSym)
    {
    *ret = FOptimize1_sub(gCP, gTP, compilerState, curPair);
    }
else
if( aSymbol == gCP->FCompile_ieqSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpEQInteger);
    }
else
if( aSymbol == gCP->FCompile_igtSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpGTInteger);
    }
else
if( aSymbol == gCP->FCompile_iltSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpLTInteger);
    }
else
if( aSymbol == gCP->FCompile_igeSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpGEInteger);
    }
else
if( aSymbol == gCP->FCompile_ileSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpLEInteger);
    }
else
if( aSymbol == gCP->FCompile_ineSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpNEInteger);
    }
else
if( aSymbol == gCP->FCompile_neqSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpEQNumber);
    }
else
if( aSymbol == gCP->FCompile_ngtSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpGTNumber);
    }
else
if( aSymbol == gCP->FCompile_nltSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpLTNumber);
    }
else
if( aSymbol == gCP->FCompile_ngeSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpGENumber);
    }
else
if( aSymbol == gCP->FCompile_nleSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpLENumber);
    }
else
if( aSymbol == gCP->FCompile_nneSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpNENumber);
    }
else
if( aSymbol == gCP->FCompile_eqSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, VMJMPEQ);
    }
else
if( aSymbol == gCP->FCompile_gtSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, VMJMPGT);
    }
else
if( aSymbol == gCP->FCompile_ltSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, VMJMPLT);
    }
else
if( aSymbol == gCP->FCompile_geSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, VMJMPGE);
    }
else
if( aSymbol == gCP->FCompile_leSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, VMJMPLE);
    }
else
if( aSymbol == gCP->FCompile_neSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, VMJMPNE);
    }
else
if( aSymbol == gCP->FCompile_addiSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMADDI);
    }
else
if( aSymbol == gCP->FCompile_diviSym)
    {
    *ret = FOptimize1_div(gCP, gTP, compilerState, curPair, VMDIVI);
    }
else
if( aSymbol == gCP->FCompile_muliSym)
    {
    *ret = FOptimize1_mul(gCP, gTP, compilerState, curPair, VMMULI);
    }
else
if( aSymbol == gCP->FCompile_subiSym)
    {
    *ret = FOptimize1_subi(gCP, gTP, compilerState, curPair);
    }
else
if( aSymbol == gCP->FCompile_divrSym)
    {
    *ret = FOptimize1_twoOp(gCP, gTP, compilerState, curPair, VMDIVR);
    }
else
if( aSymbol == gCP->FCompile_divriSym)
    {
    *ret = FOptimize1_twoOp(gCP, gTP, compilerState, curPair, VMDIVRI);
    }
else
if( aSymbol == gCP->FCompile_shlSym)
    {
    *ret = FOptimize1_twoOp(gCP, gTP, compilerState, curPair, VMSHL);
    }
else
if( aSymbol == gCP->FCompile_shrSym)
    {
    *ret = FOptimize1_twoOp(gCP, gTP, compilerState, curPair, VMSHR);
    }
else
if( aSymbol == gCP->FCompile_xorSym)
    {
    *ret = FOptimize1_atLeastOneOp(gCP, gTP, compilerState, curPair, VMXOR);
    }
else
if( aSymbol == gCP->FCompile_borSym)
    {
    *ret = FOptimize1_atLeastOneOp(gCP, gTP, compilerState, curPair, VMOR);
    }
else
if( aSymbol == gCP->FCompile_bandSym)
    {
    *ret = FOptimize1_atLeastOneOp(gCP, gTP, compilerState, curPair, VMAND);
    }
else
if( aSymbol == gCP->FCompile_selfSym)
    {
    *ret = FOptimize1_zeroOp(gCP, gTP, compilerState, curPair, VMSELF);
    }
else
if( aSymbol == gCP->FCompile_argcountSym)
    {
    *ret = FOptimize1_zeroOp(gCP, gTP, compilerState, curPair, VMARGCOUNT);
    }
else
if( aSymbol == gCP->FCompile_add1Sym)
    {
    *ret = FOptimize1_oneOpAdd(gCP, gTP, compilerState, curPair, 1);
    }
else
if( aSymbol == gCP->FCompile_sub1Sym)
    {
    *ret = FOptimize1_oneOpAdd(gCP, gTP, compilerState, curPair, -1);
    }
else
if( aSymbol == gCP->FCompile_addnSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMADDN);
    }
else
if( aSymbol == gCP->FCompile_divnSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMDIVN);
    }
else
if( aSymbol == gCP->FCompile_mulnSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMMULN);
    }
else
if( aSymbol == gCP->FCompile_subnSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMSUBN);
    }
else
if( aSymbol == gCP->FCompile_beqSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpEQInteger);
    }
else
if( aSymbol == gCP->FCompile_bgtSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpGTInteger);
    }
else
if( aSymbol == gCP->FCompile_bltSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpLTInteger);
    }
else
if( aSymbol == gCP->FCompile_bgeSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpGEInteger);
    }
else
if( aSymbol == gCP->FCompile_bleSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpLEInteger);
    }
else
if( aSymbol == gCP->FCompile_bneSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpNEInteger);
    }
else
if( aSymbol == gCP->FCompile_caddSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMCADD);
    }
else
if( aSymbol == gCP->FCompile_cdivSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMCDIV);
    }
else
if( aSymbol == gCP->FCompile_cmulSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMCMUL);
    }
else
if( aSymbol == gCP->FCompile_csubSym)
    {
    *ret = FOptimize1_add(gCP, gTP, compilerState, curPair, VMCSUB);
    }
else
if( aSymbol == gCP->FCompile_ceqSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpEQInteger);
    }
else
if( aSymbol == gCP->FCompile_cgtSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpGTInteger);
    }
else
if( aSymbol == gCP->FCompile_cltSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpLTInteger);
    }
else
if( aSymbol == gCP->FCompile_cgeSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpGEInteger);
    }
else
if( aSymbol == gCP->FCompile_cleSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpLEInteger);
    }
else
if( aSymbol == gCP->FCompile_cneSym)
    {
    *ret = FOptimize2_boolRet(gCP, gTP, compilerState, curPair, vmnatJmpNEInteger);
    }
else
if( aSymbol == gCP->FCompile_refTextSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFTEXT);
    }
else
if( aSymbol == gCP->FCompile_refStringSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFSTRING);
    }
else
if( aSymbol == gCP->FCompile_setStringSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETSTRING);
    }
else
if( aSymbol == gCP->FCompile_refSymbolSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFSYMBOL);
    }
else
if( aSymbol == gCP->FCompile_refVectorSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFVECTOR);
    }
else
if( aSymbol == gCP->FCompile_setVectorSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETVECTOR);
    }
else
if( aSymbol == gCP->FCompile_refStrValueSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFSTRVALUE);
    }
else
if( aSymbol == gCP->FCompile_setStrValueSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETSTRVALUE);
    }
else
if( aSymbol == gCP->FCompile_refStrKeySym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFSTRKEY);
    }
else
if( aSymbol == gCP->FCompile_setStrKeySym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETSTRKEY);
    }
else
if( aSymbol == gCP->FCompile_refDicValueSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFDICVALUE);
    }
else
if( aSymbol == gCP->FCompile_setDicValueSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETDICVALUE);
    }
else
if( aSymbol == gCP->FCompile_refDicKeySym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFDICKEY);
    }
else
if( aSymbol == gCP->FCompile_setDicKeySym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETDICKEY);
    }
else
if( aSymbol == gCP->FCompile_refDirValueSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFDIRVALUE);
    }
else
if( aSymbol == gCP->FCompile_setDirValueSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETDIRVALUE);
    }
else
if( aSymbol == gCP->FCompile_refDirKeySym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFDIRKEY);
    }
else
if( aSymbol == gCP->FCompile_setDirKeySym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETDIRKEY);
    }
else
if( aSymbol == gCP->FCompile_refBitVectorSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFBITVECTOR);
    }
else
if( aSymbol == gCP->FCompile_setBitVectorSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETBITVECTOR);
    }
else
if( aSymbol == gCP->FCompile_refBytVectorSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFBYTVECTOR);
    }
else
if( aSymbol == gCP->FCompile_setBytVectorSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETBYTVECTOR);
    }
else
if( aSymbol == gCP->FCompile_refPcdVectorSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFPCDVECTOR);
    }
else
if( aSymbol == gCP->FCompile_setPcdVectorSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETPCDVECTOR);
    }
else
if( aSymbol == gCP->FCompile_refObjVectorSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFOBJVECTOR);
    }
else
if( aSymbol == gCP->FCompile_setObjVectorSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETOBJVECTOR);
    }
else
if( aSymbol == gCP->FCompile_refIntVectorSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFINTVECTOR);
    }
else
if( aSymbol == gCP->FCompile_setIntVectorSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETINTVECTOR);
    }
else
if( aSymbol == gCP->FCompile_refNumVectorSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFNUMVECTOR);
    }
else
if( aSymbol == gCP->FCompile_setNumVectorSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETNUMVECTOR);
    }
else
if( aSymbol == gCP->FCompile_refFltVectorSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFFLTVECTOR);
    }
else
if( aSymbol == gCP->FCompile_setFltVectorSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETFLTVECTOR);
    }
else
if( aSymbol == gCP->FCompile_refMatrixSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFMATRIX);
    }
else
if( aSymbol == gCP->FCompile_setMatrixSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETMATRIX);
    }
else
if( aSymbol == gCP->FCompile_refNumMatrixSym)
    {
    *ret = FOptimize1_StrongRef(gCP, gTP, compilerState, curPair, VMREFNUMMATRIX);
    }
else
if( aSymbol == gCP->FCompile_setNumMatrixSym)
    {
    *ret = FOptimize1_StrongSet(gCP, gTP, compilerState, curPair, VMSETNUMMATRIX);
    }
else
if( (callSymbol->itsMaxItemIndex > 2) && (SymbolArray(cs)[0] == 'v') && (SymbolArray(cs)[1] == 'm'))
    {
    *ret = FOptimize2_Assembler(gCP, gTP, compilerState, curPair, callSymbol);
    }
else
    {
    *ret = gCP->TObject_FALSE;
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_arguments

This is the procedure that knows how to cascade results for an arbitrary number of arguments
for a given instruction, it is used for 

     VMADD VMADDI VMDIVR VMSHL VMSHR VMMUL VMMULI VMSUB and VMSUBI 
     VMADDN VMDIVN VMMULN VMSUBN 
     VMCADD VMCDIV VMCMUL VMCSUB 
     VMIADD VMIDIVR VMIDIV VMIMUL VMISUB 
     VMNADD VMNDIVR VMNDIV VMNMUL VMNSUB 

See the procedures above for samples of possible code generation.

#endif

TVAL    FOptimize1_arguments(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
TYPE                tag;
TYPE                declaredType = TYTVAL;
NUM                 ndx;
NUM                 total;
NUM                 mod;
CHAR				tmpBuffer[100];
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVAL(intermediate);
DeclareTVAL(car);
DeclareTVAL(swapMod);
DeclareTVAL(swapVal);
DeclareTVALArray(prmv,8);

_FCompile_EndFrame

/*  Save the final location, if any was requested */

*final = _Result(compilerState);

/*  Void _Result(compilerState) to guard against side effects. */

_Result(compilerState) = gCP->Tval_VOID;

/*  Initialize control indices for code generation. */

ndx = 2;
total = 0;

/*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));

do 
    {
    /*  Loop through all of the arguments in the argument list. */
    
    *car = curPair->itsCar;
    switch((tag = asTag(car)))
        {
        /*  Process the next argument */
        
        case TYPAIR:
            /*  Let the sub-expression allocate any needed temporary variables. */
            /*  Note1: This way they will be given optimized preferred types.   */
            /*  Note2: Setup a temp pair and recognize the sub-expression.      */
            if ((final->Modifier == AMREGISTER) && (final->DeclaredType == TYNUM))
				{
				*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);
				_FCompile_FrameChk(*ret);
				}
			else
            if ((final->Modifier == AMREGISTER) && (final->DeclaredType == TYREAL))
				{
				*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);
				_FCompile_FrameChk(*ret);
				}
			else
            if (final->DeclaredType == TYNUM)
				{
				*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);
				_FCompile_FrameChk(*ret);
				}
			else
            if (final->DeclaredType == TYREAL)
				{
				*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);
				_FCompile_FrameChk(*ret);
				}
			else
				{
				*ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
				_FCompile_FrameChk(*ret);
				}

            /*  Setup a temp pair and send the sub-expression to the recognizer. */
            
            tmpPair = TPair_New(gCP,gTP);
            FObject_Perm(gCP,gTP,(TObject*)tmpPair, TRUE);

            tmpPair->itsCar = *car;
            _Result(compilerState) = *ret;
            *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
            FObject_Perm(gCP,gTP,(TObject*)tmpPair, FALSE);
            _FCompile_FrameChk(*ret);
            
            /*  Format the result as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], asModifier(ret));
            _FCompile_MakeSYM(prmv[ndx+3], *ret);
			declaredType = ret->DeclaredType;
        break;
        
        case TYSYMBOL:
            /*  This is a symbol reference, search Av Tv Pv Gv to resolve it.  */
            
            aSymbol = asSymbol(car);
            *ret = FCompile_LookUp(gCP, gTP, compilerState, aSymbol);
                
            /*  Format the result as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], asModifier(ret));
            _FCompile_MakeSYM(prmv[ndx+3], *ret);
			declaredType = ret->DeclaredType;
        break;
        
        case TYNUM:
            /*  Format the TYNUM as an argument for FOptimize_OptimizePcode */
            
		if (declaredType == TYREAL)
			{
			car->Tag = TYREAL;
			car->u.Real = car->u.Int;
            _FCompile_MakeINT(prmv[ndx], AMWORD);
            _FCompile_MakeCON(prmv[ndx+3], *car);
			}
		else
			{
            _FCompile_MakeINT(prmv[ndx], AMWORD);
            _FCompile_MakeCON(prmv[ndx+3], *car);
			}
        break;
        
        default:
            /*  Setup the default modifier */
            
            mod = AMWORD;
                
            if(tag == TYQUOTEDPAIR)
                {
                /*  Decrement the quote count on any quoted thing. */
                
                if(--asQuoteCnt(car) == 0)
                    asTag(car) = TYPAIR;
                }
            else
            if(tag == TYQUOTEDSYMBOL)
                {
                /*  Decrement the quote count on any quoted thing. */
                
                if(--asQuoteCnt(car) == 0)
                    asTag(car) = TYSYMBOL;
                }

            /*  Format the result of the default processing  as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], mod);
            _FCompile_MakeCON(prmv[ndx+3], *car);
        break;
        }
    
    /*  Increment the FOptimize_OptimizePcode argument index. */
    
    ndx++;
    
    if(ndx == 4)
        {
        /*  If ndx == 4 then we have loaded two arguments for this instruction and it is time */
        /*  to generate code. */
        
        /*  Setup the return location for this instruction. */
        
        /*  Reset the first available temp to that at the start of this function. */
        
        _FCompile_ResetBase;
        
        /* Swap arguments b4 calling append as VM uses reverse ordering. */
        
        *swapMod = prmv[2];
        *swapVal = prmv[5];
        prmv[2] = prmv[3];
        prmv[5] = prmv[6];
        prmv[3] = *swapMod;
        prmv[6] = *swapVal;
        _FCompile_MakeINT(prmv[1], instruction);
        
        if ((final->Tag != TYVOID) && (curPair->itsCdr.Tag == TYVOID))
            {
            /*  The final result is a temp so we can gen code directly to that location. */
            
            *intermediate = *final;
            }
        else
            {
            /*  Utilize the first available temp to cascade the result. */
            
            *intermediate = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
            _FCompile_FrameChk(*intermediate);
            }
        
        /*  Format the argument for FOptimize_OptimizePcode */
            
        _FCompile_MakeINT(prmv[ndx], asModifier(intermediate));
        _FCompile_MakeSYM(prmv[ndx+3], *intermediate);
        
        *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
        _FCompile_FrameChk(*ret);
        
        /*  Now, if there are more args then generate code to place the previous result as  */
        /*  the first argument to the next instruction to be generated. */
        
        if(isPair(&curPair->itsCdr) )
            {
            /*  Cascade the result. */
            
            prmv[2] = prmv[ndx];
            prmv[5] = prmv[ndx+3];
            
            /*  Set the FOptimize_OptimizePcode argument index appropriately. */
            
            ndx = 3;
            }
        }
    } while ((curPair = _FCompile_CdrPair(curPair)) != NULL);

_LastResult(compilerState) = *intermediate;

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

_FCompile_FrameExit(_LastResult(compilerState));


/* Construct an Error message */
strcpy(tmpBuffer, "!Smartlisp Compiler:");
FDebug_INSToString(gCP, gTP, instruction,  &tmpBuffer[strlen(tmpBuffer)]);   /* convert the instruction opcode to a string */
strcat(tmpBuffer, (const char*)"!");

*ret = TERROR(tmpBuffer);
_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_StrongSet

This is the procedure which is used to generate vminstructions directly for VMSET
where it is possible.

For example:
    (setVector target index source)

might generate:

 0000: vmsetvector   aminteg amfboff amfboff  index            source           target            
 0008: return  amfboff amvoid  amvoid   target           

For example:
    (setCdr target source)

might generate:

 0000: vmsetcdr  amvoid  amfboff amfboff  source           target           
 0008: return	 amfboff amvoid  amvoid   source            

From this example we can see how a series of VMSET instructions will be generated which 
cascade their return result.
For example:

    (setq x (setCdr target source))

might generate:

 0000: vmsetcdr   aminteg amfboff amfboff  index            source           target           
 0008: vmmove     amfboff amfboff amvoid   target           x            
 0016: return     amfboff amvoid  amvoid   x            

#endif

TVAL FOptimize1_StrongSet(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
TYPE                tag;
NUM                 ndx;
NUM                 total;
NUM                 mod;
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVAL(intermediate);
DeclareTVAL(car);
DeclareTVAL(swapMod);
DeclareTVAL(swapVal);
DeclareTVALArray(terms,3);
DeclareTVALArray(prmv,8);


_FCompile_EndFrame

/*  Save the final location, if any was requested */

*final = _Result(compilerState);

/*  Void _Result(compilerState) to guard against side effects. */

_Result(compilerState) = gCP->Tval_VOID;

/*  Initialize control indices for code generation. */

ndx = 2;
total = 0;

/*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));

do 
    {
    /*  Loop through all of the arguments in the argument list (maximum of 2). */
    
    *car = curPair->itsCar;
    switch((tag = asTag(car)))
        {
        /*  Process the next argument */
        
        case TYPAIR:
            /*  Allocate a new temp var as needed for each sub-expression. */
            
            *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
            _FCompile_FrameChk(*ret);

            /*  Setup a temp pair and send the sub-expression to the recognizer. */
            
            tmpPair = TPair_New(gCP,gTP);
            FObject_Perm(gCP,gTP,(TObject*)tmpPair, TRUE);

            tmpPair->itsCar = *car;
            _Result(compilerState) = *ret;
            *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
            FObject_Perm(gCP,gTP,(TObject*)tmpPair, FALSE);
            _FCompile_FrameChk(*ret);
            
            /*  Format the result as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], asModifier(ret));
            _FCompile_MakeSYM(prmv[ndx+3], *ret);
            terms[ndx-2] = *ret;
        break;
        
        case TYSYMBOL:
            /*  This is a symbol reference, search Av Tv Pv Gv to resolve it.  */
            
            aSymbol = asSymbol(car);
            *ret = FCompile_LookUp(gCP, gTP, compilerState, aSymbol);
                
            /*  Format the result as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], asModifier(ret));
            _FCompile_MakeSYM(prmv[ndx+3], *ret);
            terms[ndx-2] = *ret;
        break;
        
        case TYNUM:
            /*  Format the TYNUM as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], AMWORD);
            _FCompile_MakeCON(prmv[ndx+3], *car);
            terms[ndx-2] = *car;
        break;
        
        default:
			if (ndx == 2)
				{
				/* This set must be implemented as a call to the built-in set function. */
				_FCompile_FrameExit(gCP->Tval_FALSE);
				}

            /*  Setup the default modifier */
            
            mod = AMWORD;
                
            if(tag == TYQUOTEDPAIR)
                {
                /*  Decrement the quote count on any quoted thing. */
                
                if(--asQuoteCnt(car) == 0)
                    asTag(car) = TYPAIR;
                }
            else
            if(tag == TYQUOTEDSYMBOL)
                {
                if (ndx == 2)
                    {
                    /*  Treat any quoted target as a global symbol. */
                    asQuoteCnt(car) = 0;
                    asModifier(car) = AMGVOFFSET;
                    tag = asTag(car) = TYSYMBOL;
                    mod = AMGVOFFSET;
                    }
                else
                    {
                    /*  Decrement the quote count on any quoted thing. */
                    if(--asQuoteCnt(car) == 0)
                        tag = asTag(car) = TYSYMBOL;
                    }
                }

            /*  Format the result of the default processing  as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], mod);
            _FCompile_MakeCON(prmv[ndx+3], *car);
            terms[ndx-2] = *car;
        break;
        }
    
    /*  Increment the FOptimize_OptimizePcode argument index. */
    
    ndx++;
    } while ((curPair = _FCompile_CdrPair(curPair)) != NULL);
    
/*  Generate the vmset instruction for the arguments. */

if(ndx == 5)
    {
    /*  If ndx == 5 then we have loaded three arguments for this instruction */
    /*  and it is time to generate code. */
    
    /*  Reset the first available temp to that at the start of this function. */
    
    _FCompile_ResetBase;
    
    /* Swap arguments before calling append as vmset requires: index source target. */
    
    *swapMod = prmv[3];
    *swapVal = prmv[6];
    prmv[3] = prmv[4];
    prmv[6] = prmv[7];
    prmv[4] = prmv[2];
    prmv[7] = prmv[5];
    prmv[2] = *swapMod;
    prmv[5] = *swapVal;

    /*  Complete the target assignment for the vmset instruction. */
    
    _FCompile_MakeINT(prmv[1], instruction);

    *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState, 8, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    *intermediate = terms[0];
    }
else
	{
	/* This set must be implemented as a call to the built-in set function. */
	_FCompile_FrameExit(gCP->Tval_FALSE);
	}
    
/*  Do we need to move the result into a final target ? */

if (final->Tag != TYVOID)
    {
    /*  The final result is a temp so we can gen code directly to that location. */
    
    _FCompile_MakeINT(prmv[1], VMMOVE);
    _FCompile_MakeINT(prmv[2], asModifier(intermediate));
    _FCompile_MakeINT(prmv[3], asModifier(final));
    _FCompile_MakeINT(prmv[4], AMVOID);
    _FCompile_MakeSYM(prmv[5], *intermediate);
    _FCompile_MakeSYM(prmv[6], *final);
    *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    *intermediate = *final;
    }

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

_LastResult(compilerState) = *intermediate;

_FCompile_FrameExit(_LastResult(compilerState));

if(asTag(ret) != TYERROR)
    {
    *ret = TERROR("!SmartLisp Compiler: setXXX!");
    }
_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_StrongRef

This is the procedure which is used to generate vminstructions directly for VMREF
where it is possible.

For example:
    (setq y (refCdr x))

might generate:

 0000: vmrefcdr  amfboff amfboff amvoid		x			y	            

For example:
    (setq n (refNumVector v i))

might generate:

 0000: vmrefnumvector   amfboff  amfboff amfboff	v			i		n            

#endif

TVAL FOptimize1_StrongRef(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
TYPE                tag;
NUM                 ndx;
NUM                 total;
NUM                 targetArg;
NUM                 targetMod;
NUM                 mod;
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVAL(intermediate);
DeclareTVAL(car);
DeclareTVAL(swapMod);
DeclareTVAL(swapVal);
DeclareTVALArray(prmv,8);

_FCompile_EndFrame

/*  Save the final location, if any was requested */

*final = _Result(compilerState);

/*  Void _Result(compilerState) to guard against side effects. */

_Result(compilerState) = gCP->Tval_VOID;

/*  Initialize control indices for code generation. */

ndx = 2;
total = 0;

/*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));

do 
    {
    /*  Loop through all of the arguments in the argument list (maximum of 2). */
    
    *car = curPair->itsCar;
    switch((tag = asTag(car)))
        {
        /*  Process the next argument */
        
        case TYPAIR:
            /*  Allocate a new temp var as needed for eaxh sub-expression. */
            
            *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
            _FCompile_FrameChk(*ret);

            /*  Setup a temp pair and send the sub-expression to the recognizer. */
            
            tmpPair = TPair_New(gCP,gTP);
            FObject_Perm(gCP,gTP,(TObject*)tmpPair, TRUE);

            tmpPair->itsCar = *car;
            _Result(compilerState) = *ret;
            *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
            FObject_Perm(gCP,gTP,(TObject*)tmpPair, FALSE);
            _FCompile_FrameChk(*ret);
            
            /*  Format the result as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], asModifier(ret));
            _FCompile_MakeSYM(prmv[ndx+3], *ret);
        break;
        
        case TYSYMBOL:
            /*  This is a symbol reference, search Av Tv Pv Gv to resolve it.  */
            
            aSymbol = asSymbol(car);
            *ret = FCompile_LookUp(gCP, gTP, compilerState, aSymbol);
                
            /*  Format the result as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], asModifier(ret));
            _FCompile_MakeSYM(prmv[ndx+3], *ret);
        break;
        
        case TYNUM:
            /*  Format the TYNUM as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], AMWORD);
            _FCompile_MakeCON(prmv[ndx+3], *car);
        break;
        
        default:
            /*  Setup the default modifier */
            
            mod = AMWORD;
                
            if(tag == TYQUOTEDPAIR)
                {
                /*  Decrement the quote count on any quoted thing. */
                
                if(--asQuoteCnt(car) == 0)
                    asTag(car) = TYPAIR;
                }
            else
            if(tag == TYQUOTEDSYMBOL)
                {
                /*  Decrement the quote count on any quoted thing. */
                
                if(--asQuoteCnt(car) == 0)
                    asTag(car) = TYSYMBOL;
                }

            /*  Format the result of the default processing  as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], mod);
            _FCompile_MakeCON(prmv[ndx+3], *car);
        break;
        }
    
    /*  Increment the FOptimize_OptimizePcode argument index. */
    
    ndx++;
    } while ((curPair = _FCompile_CdrPair(curPair)) != NULL);
    
/*  Generate the vmref instruction for the arguments. */

if(ndx == 4)
    {
    /*  If ndx == 4 then we have loaded two arguments for this instruction and it is time */
    /*  to generate code. */
    
    /*  Setup the return location for this instruction. */
    
    /*  Reset the first available temp to that at the start of this function. */
    
    _FCompile_ResetBase;
    
    /* Swap arguments before calling append as vmref places the index first. */
    
    *swapMod = prmv[2];
    *swapVal = prmv[5];
    prmv[2] = prmv[3];
    prmv[5] = prmv[6];
    prmv[3] = *swapMod;
    prmv[6] = *swapVal;
    targetMod = 4;
    targetArg = 7;
    }
else
if(ndx == 3)
    {
    /*  If ndx == 4 then we have loaded two arguments for this instruction and it is time */
    /*  to generate code. */
    
    /*  Setup the return location for this instruction. */
    
    /*  Reset the first available temp to that at the start of this function. */
    
    _FCompile_ResetBase;
    
    /* Promote the source argument, before calling append, as vmref leaves the index void. */
    
    prmv[3] = prmv[2];
    _FCompile_MakeINT(prmv[2], AMVOID);
    targetMod = 4;
    targetArg = 6;
    }
else
    goto BadCleanUp;
    
/*  Complete the target assignment for the vmref instruction. */

_FCompile_MakeINT(prmv[1], instruction);

if (final->Tag != TYVOID)
    {
    /*  The final result is a temp so we can gen code directly to that location. */
    
    *intermediate = *final;
    }
else
    {
    /*  Utilize the first available temp to cascade the result. */
    
    *intermediate = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
    _FCompile_FrameChk(*intermediate);
    }

/*  Format the argument for FOptimize_OptimizePcode */
    
_FCompile_MakeINT(prmv[targetMod], asModifier(intermediate));
_FCompile_MakeSYM(prmv[targetArg], *intermediate);

*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
_FCompile_FrameChk(*ret); 

_LastResult(compilerState) = *intermediate;

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

_FCompile_FrameExit(_LastResult(compilerState));

BadCleanUp:
if(asTag(ret) != TYERROR)
    {
    *ret = TERROR("!SmartLisp Compiler: refXXX!");
    }
_FCompile_FrameExit(*ret);
}





/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_ref

This is the procedure which is used to generate vminstructions directly for VMREF
where it is possible.

For example:
    (ref i 2)

might generate:

 0000: vmref   aminteg amfboff amfboff  2                i                __T0            
 0008: return  amfboff amvoid  amvoid   __T0            

For example:
    (ref i)

might generate:

 0000: vmref   amvoid  amfboff amfboff  i                __T0            
 0008: return  amfboff amvoid  amvoid   __T0            

From this example we can see how a series of VMREF instructions will be generated which 
cascade their result via a temporary variable.

#endif

TVAL FOptimize1_ref(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
TYPE                tag;
NUM                 ndx;
NUM                 total;
NUM                 targetArg;
NUM                 targetMod;
NUM                 mod;
NUM					prefType[8];
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVAL(intermediate);
DeclareTVAL(car);
DeclareTVAL(swapMod);
DeclareTVAL(swapVal);
DeclareTVALArray(prmv,8);

_FCompile_EndFrame

/*  Save the final location, if any was requested */

*final = _Result(compilerState);

/*  Void _Result(compilerState) to guard against side effects. */

_Result(compilerState) = gCP->Tval_VOID;

/*  Initialize control indices for code generation. */

ndx = 2;
total = 0;

/*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));

do 
    {
    /*  Loop through all of the arguments in the argument list (maximum of 2). */
    
    *car = curPair->itsCar;
    switch((tag = asTag(car)))
        {
        /*  Process the next argument */
        
        case TYPAIR:
            /*  Allocate a new temp var as needed for eaxh sub-expression. */
            
            *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
            _FCompile_FrameChk(*ret);

            /*  Setup a temp pair and send the sub-expression to the recognizer. */
            
            tmpPair = TPair_New(gCP,gTP);
            FObject_Perm(gCP,gTP,(TObject*)tmpPair, TRUE);

            tmpPair->itsCar = *car;
            _Result(compilerState) = *ret;
            *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
            FObject_Perm(gCP,gTP,(TObject*)tmpPair, FALSE);
            _FCompile_FrameChk(*ret);
            
            /*  Format the result as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], asModifier(ret));
            _FCompile_MakeSYM(prmv[ndx+3], *ret);
            prefType[ndx-2] = prmv[ndx+3].DeclaredType;
        break;
        
        case TYSYMBOL:
            /*  This is a symbol reference, search Av Tv Pv Gv to resolve it.  */
            
            aSymbol = asSymbol(car);
            *ret = FCompile_LookUp(gCP, gTP, compilerState, aSymbol);
                
            /*  Format the result as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], asModifier(ret));
            _FCompile_MakeSYM(prmv[ndx+3], *ret);
            prefType[ndx-2] = prmv[ndx+3].DeclaredType;
        break;
        
        case TYNUM:
            /*  Format the TYNUM as an argument for FOptimize_OptimizePcode */

            _FCompile_MakeINT(prmv[ndx], AMWORD);
            _FCompile_MakeCON(prmv[ndx+3], *car);
            prefType[ndx-2] = TYNUM;
        break;
        
        default:
            /*  Setup the default modifier */
            
            mod = AMWORD;
                
            if(tag == TYQUOTEDPAIR)
                {
                /*  Decrement the quote count on any quoted thing. */
                
                if(--asQuoteCnt(car) == 0)
                    asTag(car) = TYPAIR;
                }
            else
            if(tag == TYQUOTEDSYMBOL)
                {
                /*  Decrement the quote count on any quoted thing. */
                
                if(--asQuoteCnt(car) == 0)
                    asTag(car) = TYSYMBOL;
                }

            /*  Format the result of the default processing  as an argument for FOptimize_OptimizePcode */
            
            _FCompile_MakeINT(prmv[ndx], mod);
            _FCompile_MakeCON(prmv[ndx+3], *car);
            prefType[ndx-2] = prmv[ndx+3].Tag;
		break;
        }
    
    /*  Increment the FOptimize_OptimizePcode argument index. */
    
    ndx++;
    } while ((curPair = _FCompile_CdrPair(curPair)) != NULL);
    
/*  Generate the vmref instruction for the arguments. */

if(ndx == 4)
    {
    /*  If ndx == 4 then we have loaded two arguments for this instruction   */
	/*    (ref source index) and it is time to generate code.                */
    
    /*  Reset the first available temp to that at the start of this function.*/
    
    _FCompile_ResetBase;
    
    /* Swap arguments before calling append as vmref places the index first, */
    /* and expects to see (vmref index source).                              */
    
    *swapMod = prmv[2];
    *swapVal = prmv[5];
    prmv[2] = prmv[3];
    prmv[5] = prmv[6];
    prmv[3] = *swapMod;
    prmv[6] = *swapVal;
    targetMod = 4;
    targetArg = 7;
    }
else
if(ndx == 3)
    {
    /*  If ndx == 3 then we have loaded one argument for this instruction    */
	/*    (ref source) and this is the same as returning the source argument.*/
    
    /*  Reset the first available temp to that at the start of this function.*/
    
    _FCompile_ResetBase;
    
    /*  Simply return the source argument, vmref is overkill.*/
    
	_LastResult(compilerState) = prmv[5];
	_FCompile_FrameExit(_LastResult(compilerState));
    }
else
    goto BadCleanUp;
    
/*  Complete the target assignment for the ref instruction. */

_FCompile_MakeINT(prmv[1], instruction);

if (final->Tag != TYVOID)
    {
    *intermediate = *final;
    }
else
    {
    /*  Utilize the first available temporary variable to receive the result.         */
    /*  Note: we "registerize" the temp to optimize code generation (where possible). */
    if ((ndx >= 4) && (prefType[1] == TYNUM) && ((prefType[0] == TYCHARPOINTER) || (prefType[0] == TYINTPOINTER) || (prefType[0] == TYSHORTPOINTER) || (prefType[0] == TYLONGPOINTER)))
		{
		/*  Allocate an Integer register temporary variable to receive the result */
		*intermediate = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);
		_FCompile_FrameChk(*intermediate);
		}
	else
    if ((ndx >= 4) && (prefType[1] == TYNUM) && ((prefType[0] == TYBYTEVECTOR) || (prefType[0] == TYSTRING) || (prefType[0] == TYSYMBOL) || (prefType[0] == TYTEXT) || (prefType[0] == TYINTVECTOR) || (prefType[0] == TYSHORTVECTOR) || (prefType[0] == TYLONGVECTOR)))
		{
		/*  Allocate an Integer register temporary variable to receive the result */
		*intermediate = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);
		_FCompile_FrameChk(*intermediate);
		}
	else
    if ((ndx >= 4) && (prefType[1] == TYNUM) && ((prefType[0] == TYFLOATPOINTER) || (prefType[0] == TYREALPOINTER)))
		{
		/*  Allocate a Number register temporary variable to receive the result */
		*intermediate = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);
		_FCompile_FrameChk(*intermediate);
		}
	else
    if ((ndx >= 4) && (prefType[1] == TYNUM) && ((prefType[0] == TYFLTVECTOR) || (prefType[0] == TYNUMMATRIX) || (prefType[0] == TYNUMVECTOR)))
		{
		/*  Allocate a Number register temporary variable to receive the result */
		*intermediate = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);
		_FCompile_FrameChk(*intermediate);
		}
	else
		{
		/*  Allocate a Word memory temporary variable to receive the result */
		*intermediate = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
		_FCompile_FrameChk(*intermediate);
		}
    
    /*  Format the result as an argument for FOptimize_OptimizePcode */
    
    _FCompile_MakeINT(prmv[ndx], asModifier(ret));
    _FCompile_MakeSYM(prmv[ndx+3], *ret);
    prefType[ndx-2] = prmv[ndx+3].DeclaredType;
    }

/*  Format the argument for FOptimize_OptimizePcode */
    
_FCompile_MakeINT(prmv[targetMod], asModifier(intermediate));
_FCompile_MakeSYM(prmv[targetArg], *intermediate);

*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
_FCompile_FrameChk(*ret); 

_LastResult(compilerState) = *intermediate;

/*  Return the _LastResult(compilerState) which contains the location where the last result */
/*  was generated. */

_FCompile_FrameExit(_LastResult(compilerState));

BadCleanUp:
if(asTag(ret) != TYERROR)
    {
    *ret = TERROR("!SmartLisp Compiler: ref!");
    }
_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize1_IndirectCall

We  do special processing here to identify cases like ((ref array index ) ...)
which should compile into function calls. We will need to assume that the array
contains executable objects and generate code to call them.

We need to handle the general case of a multiple indexed ref call and eventually provide
support for directly generating indexed instructions (to avoid ref calls ) wherever possible.

We also handle member macro reference cases like ((refmacro name macro) ...)

#endif

TVAL FOptimize1_IndirectCall(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
NUM                 n;
NUM					vmcall = VMCALL;
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(macro);
DeclareTVAL(final);
DeclareTVAL(sym);
DeclareTVAL(ret);
DeclareTVAL(args);
DeclareTVALArray(prmv,20);

_FCompile_EndFrame

if (curPair->itsCar.Tag == TYPAIR)
    {
    tmpPair = curPair->itsCar.u.Pair;
    if (tmpPair->itsCar.Tag != TYSYMBOL) goto BadCleanUp;
    if (tmpPair->itsCar.u.Symbol == gCP->FCompile_refmacroSym)
        {
        /* ---------------------------------------------------------------- */
        /*  This is member macro reference like ((refmacro name macro) ...) */
        /* ---------------------------------------------------------------- */

        /*  Retrieve the member macro from the reference form (refmacro name macro) */
        /*  Note: The member macro must be available at compile time.               */
		*macro = FSpecialForms3_RefMacro(gCP, gTP, compilerState, tmpPair);
		_FCompile_FrameChk(*macro);
			
        /*  Invoke the retrieved macro on the argument list. */
		tmpPair = TPair_New(gCP, gTP);
		tmpPair->itsCar = *macro;
		tmpPair->itsCdr = curPair->itsCdr;
		*args = curPair->itsCdr;
		for (n=0;n < 20;++n)
			{
			if (args->Tag != TYPAIR) goto Finished;			
			prmv[n] = args->u.Pair->itsCar;
			*args = args->u.Pair->itsCdr;
			}
		FrameExit(TERROR("!refmacro: too many macro arguments!"))

		Finished:

		/* Evaluate the Macro replacing the old sub List with the morphed sub List. */
        *ret = _VmEvaluate(macro->u.Lambda, n, &prmv[0]);
		if (ret->Tag == TYERROR) {FrameExit(*ret);}

		/* Use the recognize function to compile the new result form.  */
		/* Note: This will compile the new form returned by the macro. */

		tmpPair = TPair_New(gCP, gTP);
		tmpPair->itsCar = *ret;
		tmpPair->itsCdr = gCP->Tval_VOID;
		*ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
		_FCompile_FrameChk(*ret);
                
		_FCompile_MakeTVL(*ret, _LastResult(compilerState));
		_FCompile_FrameExit(*ret);
        }
    }
else
    {
    /*  This is an error ... */
    
    goto BadCleanUp;
    }

/*  Save the final location, if any was requested */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  Generate code to dereference the function to call, and save the result. */

/*  Allocate/reserve the result location for the function symbol */

*sym = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
_FCompile_FrameChk(*sym);

/*  force compilation of init to place result in sym */

/*  Allocate a temporary pair for use in code generation. */

tmpPair = TPair_New(gCP,gTP);
tmpPair->itsCar = curPair->itsCar;

_Result(compilerState) = *sym;
*ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
_FCompile_FrameChk(*ret);

/*  Move to the first arg or expression */

curPair = _FCompile_CdrPair(curPair);
    
if(curPair)
    {
    /*  Push takes care of generating code for VMPUSH instructions to push an arbitrary  */
    /*  number of arguments, and returns the number of arguments for which VMPUSH instructions */
    /*  were generated. */
    
    *ret = FCompile_Push(gCP,gTP,compilerState,curPair, &vmcall);
    _FCompile_FrameChk(*ret);
    }
else
    {
    /*  There are no arguments to this function */
    
    ret->Tag = TYNUM;
    ret->u.Int = 0;
    ret->Modifier = AMINTEGER;
    ret->Offset = 0;
    }
    
        
/* After any subexpressions have been processed generate code for a function call */
/* Note: For all single argument function calls, FCompile_Push will return the    */
/*       single argument in *ret and change the instruction, in vmcall, to  */
/*       vmcallarg. The following code will pass these changes on to the compiler */
/*       optimizer as either a vmpush-vmcall sequence or as a vmcallarg pcode.    */
    

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], vmcall);
_FCompile_MakeINT(prmv[2], ret->Modifier);
prmv[5] = *ret;
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

BadCleanUp:

*ret = TERROR("!SmartLisp Compiler: Invalid indirect call!");
_FCompile_FrameExit(*ret);
}
