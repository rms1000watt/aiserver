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

#define _C_FSPECIALFORMS1
#define _SMARTBASE
#if 0
FSpecialForms1.c

This file contains some of the procedures required to support the SmartLisp compiler 
special form processing. In addition to procedures for processing specific special forms
this file also contains support routines used for special form processing in general.

NOTE:   All examples of generated code are for illustrative purposes only, the actual code
        which would be generated for a given expression may be very different depending on the
        level of optimization in effect at compilation time. They are meant as an aid in 
        understanding, and not as explicit examples of the code that the compiler will actually
        produce at any given time.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fsforms1.h"
#include "fcompile.h"
#include "foptimize.h"
#include "tpair.h"
#include "futil1.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms1_lambda

This procedure provides support for code generation for the lambda special form.
This is a two phase (modal) process in which FCompile_lambdaSwitch is set to either zero or
one to indicate which phase is occuring. In the first phase (FCompile_lambdaSwitch == 0) we
prepare to perform a recusive call to the compiler to generate a new lambda object. This consists
of setting up a location for the code to be generated and setting up and invoking a recusive call
to FCompile_Compile, with the 2nd (optional) argument in the argv initialized to the current 
procedure object. This will insure that code will be appended to the current object. Finally we
generate code to move the result to the requested return location.

Note: The flattened layout of the input list (curPair) is as shown:

    0   1       2    3          4     5          6
(lambda (args) vars: (vars...) pvars: (pvars...) (exp))

#endif

TVAL    FSpecialForms1_lambda(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* curPair)
{
NUM                 ndx;
NUM                 saveNest;
NUM					maxPairs = 9;
_FCompile_StartFrame
DeclareOBJ(TStructure,tmpEnv);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(ret);
DeclareTVAL(lambdaConstant);
DeclareTVAL(final);
DeclareTVALArray(prmv,8);
DeclareOBJArray(TPair,pairs,20);
DeclareOBJArray(TPair,inits,2);
_FCompile_EndFrame


/* Check if the input argument is legal */
if (curPair == NULL)
	{
	*ret = TERROR("!lambda: Missing arguments!");
	_FCompile_FrameExit(*ret);
	}

if (gTP->FCompile_lambdaSwitch == 0)
    {
    /*  Phase zero (0) */
    
    /*  Set up and perform a recursive compilation of this Lambda object */
    
    *final = _Result(compilerState);
    
    /*  Set FCompile_lambdaSwitch for the next phase of processing, and setup parameters */
    /*  for a recusive call to the compiler to generate a new lambda object. */
    
    gTP->FCompile_lambdaSwitch = 1;
    _FCompile_MakeOBJ(prmv[1], curPair);
    _FCompile_MakeOBJ(prmv[2], _CurProcP(compilerState));
    gTP->FCompile_LambdaNest++;
    *ret = *lambdaConstant = FCompile_CompilePrivate(gCP,gTP,2, &prmv[1]);
    gTP->FCompile_LambdaNest--;
    _FCompile_FrameChk(*ret);

    /*  Complete setup for the return value for the lambda expression */
    
    _FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
    _FCompile_MakeINT(prmv[1], VMMOVE);
    _FCompile_MakeINT(prmv[2], AMWORD);
    _FCompile_MakeCON(prmv[5], *ret);
    
    if (isNullTval(final))
        {
        /*  Allocate a temp for the return */
    
        *final = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
        }
        
    _FCompile_MakeINT(prmv[3], asModifier(final));
    _FCompile_MakeSYM(prmv[6], *final);
                
    _FCompile_MakeINT(prmv[4], AMVOID);
    _FCompile_MakeINT(prmv[7], AMVOID);

    /*  Finally append the compiled lambda expression onto its owners pcode stream. */
    
    *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,7,&prmv[0]);
    _FCompile_FrameChk(*ret);

    /*  Set the correct return value */
    
    *ret = _LastResult(compilerState) = *final;
    _Result(compilerState) = gCP->Tval_VOID;
    }
else
    {
    
    /*  Reset the FCompile_lambdaSwitch to insure that nested lambda objects will compile properly. */
    
    gTP->FCompile_lambdaSwitch = 0;

    /*  Phase one (1). In this phase we preform code generation for the body of the lambda */
    /*  expression. */
    
    /*  We flatten the input list so that we may directly index the elements. */
    
    _FMacro_PairsToArray(curPair, pairs, sizeof(pairs)/sizeof(pairs[0]));

    saveNest = _NestLvlN(compilerState);
    _NestLvlN(compilerState) = 1;
   
    if ((pairs[1] != NULL) && 
        (asTag(&pairs[1]->itsCar) == TYPAIR))
        {
        /*  Process (arg...) via the FCompile_args procedure */
        tmpPair = asPair(&pairs[1]->itsCar);
        *ret = FCompile_args(gCP, gTP, compilerState, tmpPair);
        _FCompile_FrameChk(*ret);
        }
    else
    if ((pairs[1] != NULL) &&
        ((pairs[1]->itsCar.Tag != TYVOID) && 
         ((asTag(&pairs[1]->itsCar) != TYSYMBOL) || (asSymbol(&pairs[1]->itsCar) != gCP->TLambda_nil))))
        {
        /*  The  form  (arg...) is mandatory.  */
		*ret = TERROR("!lambda: The  form  (arg...) is mandatory!");
		_FCompile_FrameExit(*ret);
        }
	else
    if (pairs[1] == NULL)
        {
        /*  The  form  (lambda) must be converted into (lambda() #void).  */
		pairs[1] = TPair_New(gCP,gTP);
		pairs[1]->itsCar.u.Symbol = gCP->TLambda_nil;
		pairs[1]->itsCar.Tag = TYSYMBOL;
		ndx = 1;
		pairs[2] = TPair_New(gCP,gTP);
		pairs[2]->itsCar = gCP->Tval_VOID;
        }

/*  Because some parameters in a Lambda declaration are optional we utilize an index to  */
/*  keep track of which indexed pair is next in the processing sequence. */
        
ndx = 2;
    
          
    /*  Compile for faces:() before pvars:() or vars: */
TryToRecognizeFacesPvarsVars:
    
    if ((ndx < maxPairs) &&
		(pairs[ndx] != NULL) && 
        (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL) && 
        (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_faces))
        {
        if ((pairs[ndx+1] != NULL) && (asTag(&pairs[ndx+1]->itsCar) == TYPAIR))
            {
            *ret = FCompile_faces(gCP, gTP, compilerState, asPair(&pairs[ndx+1]->itsCar));
            ExitOnError(*ret);
            
            ndx += 2;
			goto TryToRecognizeFacesPvarsVars;
            }
        else
			{
			*ret = TERROR("!lambda: faces: must be followed by a variable list (...)!");
			_FCompile_FrameExit(*ret);
			}
        }

	if ((ndx < maxPairs) &&
		(pairs[ndx] != NULL) && 
        (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL))
        {
        if (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_vars)
            {
            /*  Process the optional vars: () via FCompile_vars */
            
            if ((pairs[ndx+1] != NULL) && (asTag(&pairs[ndx+1]->itsCar) == TYPAIR))
                {
                *ret = FCompile_vars(gCP, gTP, compilerState, asPair(&pairs[ndx+1]->itsCar));
                ExitOnError(*ret);
                
                /*  increment the control index to step over vars: () */
                
                ndx += 2;
				goto TryToRecognizeFacesPvarsVars;
                }
            else
                {
                /*  vars: must be followed by a variable list (...) */
   				*ret = TERROR("!lambda: vars: must be followed by a variable list (...)!");
				_FCompile_FrameExit(*ret);
                }
            }
        }
            
    if ((ndx < maxPairs) &&
		(pairs[ndx] != NULL) && 
        (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL))
        {
        if (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_pvars)
            {
            /*  Process the optional pvars: () via FCompile_pvars */
            
            if ((pairs[ndx+1] != NULL) && (asTag(&pairs[ndx+1]->itsCar) == TYPAIR))
                {
                *ret = FCompile_pvars(gCP, gTP, compilerState, asPair(&pairs[ndx+1]->itsCar));
                ExitOnError(*ret);
                
                /*  increment the control index to step over vars: () */
                
                ndx += 2;
				goto TryToRecognizeFacesPvarsVars;
                }
            else
                {
                /*  pvars: must be followed by a variable list (...) */
    			*ret = TERROR("!lambda: pvars: must be followed by a variable list (...)!");
				_FCompile_FrameExit(*ret);
                }
            }
        }
    
    if ((ndx < maxPairs) &&
		(pairs[ndx] != NULL) && 
        (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL))
        {
        if (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_svars)
            {
            /*  Process the optional svars: () via FCompile_svars */
            
            if ((pairs[ndx+1] != NULL) && (asTag(&pairs[ndx+1]->itsCar) == TYPAIR))
                {
                *ret = FCompile_svars(gCP, gTP, compilerState, asPair(&pairs[ndx+1]->itsCar));
                ExitOnError(*ret);
                
                /*  increment the control index to step over svars: () */
                
                ndx += 2;
				goto TryToRecognizeFacesPvarsVars;
                }
            else
                {
                /*  svars: must be followed by a type name (typeName) */
    			*ret = TERROR("!lambda: svars: must be followed by a type name (typeName)!");
				_FCompile_FrameExit(*ret);
                }
            }
        }
    
    if ((ndx < maxPairs) &&
		(pairs[ndx] != NULL) && 
        (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL))
        {
        if (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_cvars)
            {
            /*  Process the optional vars: () via FCompile_vars */
            
            if ((pairs[ndx+1] != NULL) && (asTag(&pairs[ndx+1]->itsCar) == TYPAIR))
                {
                *ret = FCompile_cvars(gCP, gTP, compilerState, asPair(&pairs[ndx+1]->itsCar));
                ExitOnError(*ret);
                
                /*  increment the control index to step over vars: () */
                
                ndx += 2;
				goto TryToRecognizeFacesPvarsVars;
                }
            else
                {
                /*  pvars: must be followed by a variable list (...) */
    			*ret = TERROR("!lambda: cvars: must be followed by a variable list (...)!");
				_FCompile_FrameExit(*ret);
                }
            }
        }

    if ((ndx < maxPairs) &&
		(pairs[ndx] != NULL) && 
        (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL))
        {
		if(asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_Doomed)
			{
			_Dm(compilerState) = TRUE;
			}
        }

    
    /*  reset curPair to point to the expression portion of this lambda form. */
    
    curPair = pairs[ndx];
    if (curPair == NULL)
        {
        /*  pvars: must be followed by a variable list (...) */
    	*ret = TERROR("!lambda: pvars: Missing expresion!");
		_FCompile_FrameExit(*ret);
        }
    
    /*  Re-Initialize the base for the special form variable store in order to insure that when */
    /*  we perform our _FCompile_FrameExit we do not free up the return value which we use */
    /*  for this lambda processing. */
    
    __Base__ = _TempN(compilerState) = _Tv(compilerState)->itsMaxItemIndex;

    /*  Call the recognizer for the expression portion of this lambda */
    
    *ret = FCompile_Recognize(gCP, gTP, compilerState, curPair);
    _NestLvlN(compilerState) = saveNest;
    }

/*  We return the result of the current processing phase. In phase zero this is the return location */
/*  generated during processing. In phase one it is the result of procesing the expression portion */
/*  of this special form. */

_FCompile_FrameExit(*ret);
}
    
/*--------------------------------------------------------------------------------------- */
/*
FSpecialForms1_defun

This procedure provides support for code generation for the defun special form. The defun
special form is a flavor of define in which a new lambda object will be generated. Please see the 
FSpecialForms1_lambda procedure for a description of the nature and control of this processing.

(define	name(arg...)  vars:  (var...)  pvars:  (var...)  exp...)
                 "             "             "
                 "             "             "
                 V             V             V
(define			(name  arg...)  vars:  (var...)  pvars:  (var...)  exp...)

The defun special form creates and initializes a variable containing a lambda object (Lambda).  
The Lambda may receive zero or more arguments, and its body may have zero or more expressions. 
The defun special form always returns the value to which name is initialized 
(Note: this value will always be an Lambda object).  
The defun special form is equivalent to the following AIS Lisp expression.

(define  name  (lambda  (arg...)  vars: (var...)  pvars: (var...)  exp...))

If a list of argument names is specified,  (arg...)  , at calling time the procedure`s 
arguments are bound to the specified arguments respectively.  Passing an incorrect number of 
arguments results in an error message. (see the lambda special form).
If an indefinite list of argument names is specified  (name   arg...) , at calling time the 
procedure`s arguments are bound to the specified definite arguments respectively.  Passing too 
few arguments results in an error message.  If too many arguments are passed, the remaining 
arguments can only be accessed via the argCount and argFetch procedures.  (see the lambda 
special form).

The keyword vars:  (var...)  declares the temporary variables of the procedure.  
The variables are specified as a list  vars:  (X  Y...).   If any one of the variables is 
specified as a list,  vars:  ((X  0)  Y...),  the listed variable is initialized to the value 
specified.

The keyword pvars:  (var...)  declares the persistent variables of the procedure.  
The variables are specified as a list  pvars:  (W  Q...).   If any one of the variables is 
specified as a list,  pvars:  ((X  0)  Y...),  the listed variable is initialized to the value 
specified.

The body of the procedure is a sequence of AIS Lisp expressions {exp}.  At calling time, each of 
the expressions is evaluated, in the extended environment, from left to right.  
The procedure returns the value of the final expression in the sequence.

*/

TVAL    FSpecialForms1_defun(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(result);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;
*tmp = FMacro_Defun(gCP,gTP,1,&argPair->itsCdr);
*result = FSpecialForms1_define(gCP,gTP,compilerState,tmp->u.Pair);
FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
    
/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms1_define

This procedure provides support for code generation for the define special form. There are many
flavors of define, in two of them new lambda objects will be generated. Please see the 
FSpecialForms1_lambda procedure for a description of the nature and control of this processing.

(define  name)
(define  name  exp)
(define			(name  arg...)  vars:  (var...)  pvars:  (var...)  exp...)
(define  macro: (name  arg...)  vars:  (var...)  pvars:  (var...)  exp...)
(define  vm: (name  arg...)  vars:  (var...)  pvars:  (var...)  exp...)

The define  special form creates and initializes a variable.  There are four forms of define. 
The first form, shown above, simply declares a new variable, but does not initialize it.  
In this form, define always returns true.

The second form not only declares a new variable but also initializes it to the value returned 
by the expression.  In this form, define always returns the value to which name is initialized 
(Note: this is always the value of the expression).

The third form, shown above, allows the user to declare a new variable and initialize it to a 
procedure. The procedure may receive zero or more arguments, and its body may have zero or
more expressions. In this form, define always returns the value to which name is initialized 
(Note: this value will always be a procedure object).  
The third form is equivalent to the following SmartLisp expression.

define  name  (lambda  (arg...)  vars: (var...)  pvars: (var...)  exp...))

If a list of argument names is specified  (name   arg...)  , at calling time the procedure`s 
arguments are bound to the specified arguments respectively.  Passing an incorrect number of 
arguments results in an error message. (see the lambda special form).
If an indefinite list of argument names is specified  (name   arg...) , at calling time the 
procedure`s arguments are bound to the specified definite arguments respectively.  Passing too 
few arguments results in an error message.  If too many arguments are passed, the remaining 
arguments can only be accessed via the argCount and argFetch procedures.  (see the lambda 
special form).

The keyword vars:  (var...)  declares the temporary variables of the procedure.  
The variables are specified as a list  vars:  (X  Y...).   If any one of the variables is 
specified as a list,  vars:  ((X  0)  Y...),  the listed variable is initialized to the value 
specified.

The keyword pvars:  (var...)  declares the persistent variables of the procedure.  
The variables are specified as a list  pvars:  (W  Q...).   If any one of the variables is 
specified as a list,  pvars:  ((X  0)  Y...),  the listed variable is initialized to the value 
specified.

The body of the procedure is a sequence of SmartLisp expressions {exp}.  At calling time, each of 
the expressions is evaluated, in the extended environment, from left to right.  
The procedure returns the value of the final expression in the sequence.

The fourth form, shown last, allows the user to declare a new variable and initialize it to a 
macro. The macro must receive zero or more arguments, and its body may have zero or more 
expressions. In this form, define always returns the value to which name is initialized 
(Note: this value will always be a macro object). 

A SmartLisp macro is a Procedure object which is used exclusively by the compiler in the macro
substitution phase. A macro Procedure is normally unavailable outside of the compilation process. 
During the compiler`s substitution phase, each macro is invoked with its unevaluated arguments. 
The value returned by the Macro is substituted for the original macro reference in the source.
Note:   The scope of defined variables depends upon the current environment at the time the define 
is invoked.  A variable defined at the top level will have global scope.  A variable defined 
within a procedure will extend the pvars: Structure associated with the procedure and will 
have scope limited to the procedure.

#endif

TVAL    FSpecialForms1_define(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* curPair)
{
TYPE                theType;
NUM                 ndx;
NUM                 saveNest;
NUM					maxPairs = 9;
_FCompile_StartFrame
DeclareOBJ(TStructure,tmpEnv);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(ret);
DeclareTVAL(sym);
DeclareTVAL(makeVMLambda);
DeclareTVAL(final);
DeclareTVAL(newLambda);
DeclareTVALArray(prmv,8);
DeclareOBJArray(TPair,pairs,20);
DeclareOBJArray(TPair,inits,2);
DeclareOBJArray(TPair,temps,20);
_FCompile_EndFrame

/* Abort if rest of expression is NULL */
if (curPair == NULL)
    {
    /*  pvars: must be followed by a variable list (...) */
    *ret = TERROR("!define: Missing arguments!");
	_FCompile_FrameExit(*ret);
    }

/*  Remember where the final result should go, if anywhere. */

*final = _Result(compilerState);
    
/*  We set up an array of pointers into the input list for easy indexed access */

_FMacro_PairsToArray(curPair, pairs, sizeof(pairs)/sizeof(pairs[0]));

/*  Start initialization of the parameter list to FOptimize_OptimizePcode */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], VMMOVE);

if ((pairs[1] != NULL) && (asTag(&pairs[1]->itsCar) == TYSYMBOL))
    {
    /*  The following forms are recognized in this section: */
    /*  (define  name) */
    /*  (define  name  exp) */
    
    /*  Resolve this symbol (name) in the appropriate Structure, creating a binding as needed.   */
    /*  Note:   The scope of defined variables depends upon the current environment at the time  */
    /*  the define is invoked.  A variable defined at the top level will have global scope.      */
    /*  A variable defined within a procedure will extend the pvars: Structure associated with   */
    /*  the procedure and will have scope limited to that procedure and its direct relatives.    */
    
    *ret = FCompile_Resolve(gCP, gTP, compilerState, asSymbol(&pairs[1]->itsCar), gTP->FCompile_LambdaNest ? AMPVOFFSET : AMGVOFFSET);
    _FCompile_FrameChk(*ret);
    *sym = *ret;
        
    if (pairs[2] == NULL)
        {
        /*  Handle processing for the case shown: */
        /*      0    1 */
        /*  (define  name) */
        
        /* The result of a define is an assignment. In a globally scoped define, we generate  */
        /* inline instructions to evaluate the result into the proper global variable. In a   */
        /* nested scoped define, no inline instructions are generated; instead, the result is */
        /* evaluated at compile time and bound into the persistent variables Structure of     */ 
		/* the parent Lambda during the compilation. */
        
        if (gTP->FCompile_LambdaNest == 0)
			{
			/* A global scope define must generate an inline move instruction */
			/* to bind void to the specified global symbol at run time. Also, */
			/* since globally scopped defines take effect immediately, the    */
			/* specified global symbol is set to void immediately.            */

			_FCompile_MakeINT(prmv[2], AMWORD);
			_FCompile_MakeCON(prmv[5], gCP->TObject_VOID);
			_FCompile_MakeINT(prmv[3], asModifier(sym));
			_FCompile_MakeSYM(prmv[6], *sym);
			_FCompile_MakeINT(prmv[4], AMVOID);
			*ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,7,&prmv[0]);
			_FCompile_FrameChk(*ret);
			sym->u.Symbol->itsGlobalValue = gCP->Tval_VOID;
			}
		else
            {
			/* Bind the result into the persistent variables Structure of the parent Lambda. */

            TStructure_AddNewValue(gCP,gTP, TOBJ(_Pv(compilerState)), *sym, gCP->Tval_VOID);
			}
        
        /*  set up the lastresult location */
        
        *ret = _LastResult(compilerState) = *sym;
        }
    else
    if (pairs[3] == NULL)
        {
        /*  Handle processing for the case shown: */
        /*      0    1      2 */
        /*  (define  name  exp) */
        
        /*  Set up the desired return location */
        
        _Result(compilerState) = *sym;

        /* The result of a define is a compile-time assignment. In a globally scoped define, */
		/* we generate an inline move instruction to evaluate the result into the specified	 */
		/* global variable; AND, we also assign the specified global variable immediately.	 */ 
		/* In a nested scoped define, no inline instructions are generated; instead, the	 */ 
		/* result is evaluated at compile time and bound into the persistent variables		 */ 
		/* Structure of the parent Lambda during the compilation.                             */
        
        if (gTP->FCompile_LambdaNest == 0)
			{
			ret->u.Pair = TPair_New(gCP,gTP);
			ret->Tag = TYPAIR;
			ret->u.Pair->itsCar.u.Symbol = gCP->FCompile_setqSym;
			ret->u.Pair->itsCar.Tag = TYSYMBOL;
			ret->u.Pair->itsCdr = pairs[0]->itsCdr;
			FUtil1_Eval(gCP,gTP,1,ret);

			/* Generate inline move instruction to assign the specified global at run time.  */
			*ret = FCompile_Recognize(gCP, gTP, compilerState, pairs[2]);
			_FCompile_FrameChk(*ret);
			}
		else
            {
			*ret = pairs[2]->itsCar;
			if ((pairs[2]->itsCar.Tag == TYPAIR) && (pairs[2]->itsCdr.Tag == TYVOID))
				{
				/*  Set up and perform a recursive compilation of the expression to be */
				/*  assigned, so the assignment is made immediately at compile time.   */
        
				_FCompile_MakeOBJ(prmv[1], pairs[2]);
				_FCompile_MakeOBJ(prmv[2], _CurProcP(compilerState));
        
				gTP->FCompile_LambdaNest++;
				*ret = FCompile_CompilePrivate(gCP,gTP,2, &prmv[1]);
				gTP->FCompile_LambdaNest--;
				_FCompile_FrameChk(*ret);
				*ret = FSmartbase_Evalv(gCP,gTP,*ret,0, &prmv[1]);
				_FCompile_FrameChk(*ret);
				}
			else
			if ((pairs[2]->itsCar.QuoteCnt == 0) && (pairs[2]->itsCdr.Tag == TYVOID))
				{
				/*  Set up and perform a recursive compilation of the singleton to be assigned. */
   				/*  Recursively process the define  */
        
				prmv[1] = pairs[2]->itsCar;
				_FCompile_MakeOBJ(prmv[2], _CurProcP(compilerState));
        
				gTP->FCompile_LambdaNest++;
				*ret = FCompile_CompilePrivate(gCP,gTP,2, &prmv[1]);
				gTP->FCompile_LambdaNest--;
				_FCompile_FrameChk(*ret);
				*ret = FSmartbase_Evalv(gCP,gTP,*ret,0, &prmv[1]);
				_FCompile_FrameChk(*ret);
				}
			else
			if (pairs[2]->itsCdr.Tag != TYVOID)
				{
				/*  If the element following the define symbol is an unquoted symbol then we MUST be */
				/*  processing either (define  name) or (define  name  exp), anything else is an error. */
				*ret = TERROR("!define: Invalid expression to be assigned!");
				_FCompile_FrameExit(*ret);
				}

			/* Bind the result into the persistent variables Structure of the parent Lambda. */

            TStructure_AddNewValue(gCP,gTP, TOBJ(_Pv(compilerState)), *sym, *ret);
        
			/*  set up the lastresult location */
        
			_LastResult(compilerState) = *sym;
            }        
		}
    else
        {
        /*  If the element following the define symbol is an unquoted symbol then we MUST be */
        /*  processing either (define  name) or (define  name  exp), anything else is an error. */
		*ret = TERROR("!define: Invalid name of symbol or expresion to be defined!");
		_FCompile_FrameExit(*ret);
        }
    }
else
if ((pairs[1] != NULL) && 
    (asTag(&pairs[1]->itsCar) == TYQUOTEDSYMBOL) && 
    (asSymbol(&pairs[1]->itsCar) == gCP->FMacro_macro))
    {
    /*  Macro processing and procedure (lambda) processing are identical except that the object */
    /*  which is created is tagged differently so that it will be properly recognized during the */
    /*  macro preprocessing phase of the compiler. */
    
    /*      0    1      2               3       4           5       6       7 */
    /*  (define  macro: (name  arg...)  vars:  (var...)  pvars:  (var...)  exp...) */

    theType = TYMACRO;
    goto DefineProcedure;
    
    }
else
if ((pairs[1] != NULL) && 
    (asTag(&pairs[1]->itsCar) == TYQUOTEDSYMBOL) && 
    (asSymbol(&pairs[1]->itsCar) == gCP->FMacro_vm))
    {
    /*  Lambda VM processing and procedure (lambda) processing are identical except that the object */
    /*  which is created is tagged differently so that it will be properly recognized during the */
    /*  macro preprocessing phase of the compiler. */
    
    /*      0    1      2               3       4           5       6       7 */
    /*  (define  vm: (name  arg...)  vars:  (var...)  pvars:  (var...)  exp...) */

    theType = TYVMEVALUATOR;
    goto DefineProcedure;
    
    }
else
if ((pairs[1] != NULL) && 
    (asTag(&pairs[1]->itsCar) == TYPAIR) && 
    (pairs[2] != NULL))
    {
    /*  Here we handle the creation of new named lambda objects. Please see FSpecialForms1_lambda */
    /*  for a description of how this process occurs. The only additional step which occurs for */
    /*  define processing in this case is that the created object will be bound to the named symbol */
    /*  which is specified in the define statement. */
    
    /*      0    1               2      3           4       5       6 */
    /*  (define  (name  arg...)  vars:  (var...)  pvars:  (var...)  exp...) */
    /*      0   1       2    3          4     5          6 */
    /*  (lambda (args) vars: (vars...) pvars: (pvars...) (exp)) */
    /*  (define  name  (lambda  (arg...)  vars: (var...)  pvars: (var...)  exp...)) */

    theType = TYLAMBDA;
    
    DefineProcedure:
    
    /*  We utilize an index to insure that we parse the */
	/*  expression depending on whether it is a         */
    /*  (define  macro: (name  arg...) or a             */
    /*  (define  vm: (name  arg...) or simply a         */
    /*  (define  (name  arg...) ...                     */
    
	ndx = (theType == TYLAMBDA) ? 1 : 2;
    
    if (gTP->FCompile_lambdaSwitch == 0)
        {
        /*  Set up and perform a recursive compilation of this Lambda object */
        
        gTP->FCompile_lambdaSwitch = 1;
        if (pairs[ndx] == NULL) 
			{
			*ret = TERROR("!define: Missing name of the lambda object!");
			_FCompile_FrameExit(*ret);
			}
        _FMacro_PairsToArray(asPair(&pairs[ndx]->itsCar), inits, sizeof(inits)/sizeof(inits[0]));
      
		if (asTag(&inits[0]->itsCar) != TYSYMBOL)
			{
			*ret = TERROR("!define: Expecting name of the lambda object to be a symbol!");
			_FCompile_FrameExit(*ret);
			}
        else
            aSymbol = asSymbol(&inits[0]->itsCar);
        
        *ret = FCompile_Resolve(gCP, gTP, compilerState, aSymbol, gTP->FCompile_LambdaNest ? AMPVOFFSET : AMGVOFFSET);
        _FCompile_FrameChk(*ret);
        
        *sym = *ret;

        /*  Recursively process the define  */
        
        _FCompile_MakeOBJ(prmv[1], curPair);
        _FCompile_MakeOBJ(prmv[2], _CurProcP(compilerState));
        
        gTP->FCompile_LambdaNest++;
        *ret = *newLambda = FCompile_CompilePrivate(gCP,gTP,2, &prmv[1]);
        gTP->FCompile_LambdaNest--;
        _FCompile_FrameChk(*ret);
        
        /* The result of a defun is a new Lambda.                                         */
        /* In a nested scoped defun, no vmref is generated, but the Lambda result is      */
        /* bound into the persistent variables Structure of the parent Lambda immediately.*/
        
        if (gTP->FCompile_LambdaNest != NIL)
            {
			if (theType == TYVMEVALUATOR)
				{
				*ret = TERROR("!define: embedded vm Lambdas not supported!");
				_FCompile_FrameExit(*ret);
				}
			newLambda->Tag = theType;
            TStructure_AddNewValue(gCP,gTP, TOBJ(_Pv(compilerState)), *sym, *newLambda);
			/*  set up the lastresult location */
			_LastResult(compilerState) = *sym;
           }
		else
			{
			/* The result of a defun is a new Lambda.                                         */
			/* In a globally scoped defun, we generate a vmmove instruction to set the Lambda */
			/* result into the proper global variable immediately.                           */
			sym->u.Symbol->itsGlobalValue = *newLambda;
				
			if (theType == TYVMEVALUATOR) 
				{
				ret->Tag = TYLAMBDA;

				/* Push the makeVMLambda arguments on the stack */
				_FCompile_MakeINT(prmv[1], VMPUSH);
				_FCompile_MakeINT(prmv[2], AMWORD);
				_FCompile_MakeCON(prmv[5], *ret);
				_FCompile_MakeINT(prmv[3], AMWORD);
				_FCompile_MakeCON(prmv[6], *sym);
				_FCompile_MakeINT(prmv[4], AMVOID);
				_FCompile_MakeINT(prmv[7], AMVOID);
				*ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,7,&prmv[0]);
				_FCompile_FrameChk(*ret);


				/* Call the makeVMLambda function */
				*makeVMLambda = TSYMBOL("makeVMLambda");
				makeVMLambda->Modifier = AMGVOFFSET;
				makeVMLambda->Offset = 0;
				sym->Modifier = AMGVOFFSET;
				sym->Offset = 0;
				_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
				_FCompile_MakeINT(prmv[1], VMCALL);
				_FCompile_MakeINT(prmv[2], AMINTEGER);
				_FCompile_MakeIMM(prmv[5], 2);
				_FCompile_MakeINT(prmv[3], AMGVOFFSET);
				_FCompile_MakeSYM(prmv[6], *makeVMLambda);
				_FCompile_MakeINT(prmv[4], AMGVOFFSET);
				_FCompile_MakeSYM(prmv[7], *sym);
				*ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,8,&prmv[0]);
				_FCompile_FrameChk(*ret);

				/*  set up the lastresult location */
				_LastResult(compilerState) = *sym;
				}
			else 
				{
				ret->Tag = theType;
				_FCompile_MakeINT(prmv[1], VMMOVE);
				_FCompile_MakeINT(prmv[2], AMWORD);
				_FCompile_MakeCON(prmv[5], *ret);
				_FCompile_MakeINT(prmv[3], asModifier(sym));
				_FCompile_MakeSYM(prmv[6], *sym);
				_FCompile_MakeINT(prmv[4], AMVOID);
				_FCompile_MakeINT(prmv[7], AMVOID);
				*ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,7,&prmv[0]);
				_FCompile_FrameChk(*ret);

				/*  set up the lastresult location */
				_LastResult(compilerState) = *sym;
				}
			}
        }
    else
        {
        
        /*  Reset the FCompile_lambdaSwitch to insure that nested lambda objects will compile properly. */
        
        gTP->FCompile_lambdaSwitch = 0;

        saveNest = _NestLvlN(compilerState);
        _NestLvlN(compilerState) = 1;
        
        /*  Process args vars pvars .... */
        
        if (pairs[ndx] == NULL)
			{
			*ret = TERROR("!define: Missing name of the lambda object!");
			_FCompile_FrameExit(*ret);
			}

        tmpPair = asPair(&pairs[ndx]->itsCar);
        tmpPair = _FCompile_CdrPair(tmpPair);
        
        if (tmpPair != NULL)
            {
            /*  Process args */
            
            *ret = FCompile_args(gCP, gTP, compilerState, tmpPair);
            _FCompile_FrameChk(*ret);
            }
            
        ndx++;
        
        
        /*  Compile for faces:() before pvars:() or vars: */
 TryToRecognizeFacesPvarsVars:
		
        if ((ndx < maxPairs) &&
			(pairs[ndx] != NULL) && 
            (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL) && 
            (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_faces))
            {
            if ((pairs[ndx+1] != NULL) && (asTag(&pairs[ndx+1]->itsCar) == TYPAIR))
                {
                *ret = FCompile_faces(gCP, gTP, compilerState, asPair(&pairs[ndx+1]->itsCar));
                ExitOnError(*ret);
                
                ndx += 2;
				goto TryToRecognizeFacesPvarsVars;
                }
            else
				{
				*ret = TERROR("!define: faces: must be followed by a variable list (...)!");
				_FCompile_FrameExit(*ret);
				}
            }

		/*  Compile for vars:() */
        
        if ((ndx < maxPairs) &&
			(pairs[ndx] != NULL) && 
            (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL) && 
            (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_vars))
            {
            if ((pairs[ndx+1] != NULL) && (asTag(&pairs[ndx+1]->itsCar) == TYPAIR))
                {
                *ret = FCompile_vars(gCP, gTP, compilerState, asPair(&pairs[ndx+1]->itsCar));
                ExitOnError(*ret);
                
                ndx += 2;
				goto TryToRecognizeFacesPvarsVars;
                }
            else
				{
				*ret = TERROR("!define: vars: must be followed by a variable list (...)!");
				_FCompile_FrameExit(*ret);
				}
            }

        /*  Compile for pvars:() */
        
        if ((ndx < maxPairs) &&
			(pairs[ndx] != NULL) && 
            (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL) && 
            (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_pvars))
            {
            if ((pairs[ndx+1] != NULL) && (asTag(&pairs[ndx+1]->itsCar) == TYPAIR))
                {
                *ret = FCompile_pvars(gCP, gTP, compilerState, asPair(&pairs[ndx+1]->itsCar));
                ExitOnError(*ret);
				ndx += 2;
 				goto TryToRecognizeFacesPvarsVars;
               }
            else
				{
				*ret = TERROR("!define: pvars must be followed by a variable list (...)!");
				_FCompile_FrameExit(*ret);
				}
            }
            
        /*  Compile for svars:() */
        
        if ((ndx < maxPairs) &&
			(pairs[ndx] != NULL) && 
            (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL) && 
            (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_svars))
            {
            if ((pairs[ndx+1] != NULL) && (asTag(&pairs[ndx+1]->itsCar) == TYPAIR))
                {
                *ret = FCompile_svars(gCP, gTP, compilerState, asPair(&pairs[ndx+1]->itsCar));
                ExitOnError(*ret);
				ndx += 2;
 				goto TryToRecognizeFacesPvarsVars;
               }
            else
				{
				*ret = TERROR("!define: svars must be followed by a type name (typeName)!");
				_FCompile_FrameExit(*ret);
				}
            }
            
        if ((ndx < maxPairs) &&
			(pairs[ndx] != NULL) && 
            (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL) && 
            (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_cvars))
            {
            if ((pairs[ndx+1] != NULL) && (asTag(&pairs[ndx+1]->itsCar) == TYPAIR))
                {
                *ret = FCompile_cvars(gCP, gTP, compilerState, asPair(&pairs[ndx+1]->itsCar));
                ExitOnError(*ret);
				ndx += 2;
 				goto TryToRecognizeFacesPvarsVars;
               }
            else
				{
				*ret = TERROR("!define: cvars must be followed by a variable list (...)!");
				_FCompile_FrameExit(*ret);
				}
            }
            
        if ((ndx < maxPairs) &&
			(pairs[ndx] != NULL) && 
            (asTag(&pairs[ndx]->itsCar) == TYQUOTEDSYMBOL) && 
            (asSymbol(&pairs[ndx]->itsCar) == gCP->TLambda_Doomed))
            {
            _Dm(compilerState) = TRUE;
		}
            
        curPair = pairs[ndx];
        if (curPair == NULL)
			{
			*ret = TERROR("!define: Missing expression!");
			_FCompile_FrameExit(*ret);
			}
            
        /*  Re-Initialize the base for the special form variable store */
        
        __Base__ = _TempN(compilerState) = _Tv(compilerState)->itsMaxItemIndex;
    
        /*  Call recognizer on the remainder */
        *ret = FCompile_Recognize(gCP, gTP, compilerState, curPair);
        _NestLvlN(compilerState) = saveNest;
        ExitOnError(*ret);
		}
    }
    
if ((gTP->FCompile_lambdaSwitch == 0) && !isNullTval(final))
    {
    /*  If an explicit final result was requested, we may have to generate code for it.   */
    /*  Note: We do NOT generate code for PV or CV variables into TV temporary variables. */
    if (!((_LastResult(compilerState).Tag == TYSYMBOL) &&
	      ((_LastResult(compilerState).Modifier == AMPVOFFSET) || (_LastResult(compilerState).Modifier == AMCVOFFSET)) &&
	      (final->Tag == TYSYMBOL) &&
		  (strncmp(SymbolArray(*final),"__T",3) == 0) &&
	      (final->Modifier == AMTVOFFSET))
		  )
		{
		*ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
		}
    }
else
	{
    if (!isNullTval(final))
		{
		*ret = _Result(compilerState) = *final;
		}
	}
    
_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms1_let

The let Special Form extends the current environment and evaluates the expressions {exp...} 
in the extended environment.  The let special form evaluates the initial expressions {init}.  
The current environment is then extended to include the specified variables {var}.  The 
initial expressions {init} are then bound to the proper variables {var}. The expressions {exp} 
are evaluated from left to right, and the value of the final expression is returned. 

Note:   (let  ((var  init)...  )  exp...)


#endif

TVAL    FSpecialForms1_let(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,initPair);
DeclareOBJ(TStructure,tmpEnv);
DeclareTVAL(final);
DeclareTVAL(ret);
DeclareTVAL(sym);
DeclareOBJArray(TPair,pairs,3);
DeclareOBJArray(TPair,inits,2);
_FCompile_EndFrame

/* Abort if rest of expression is NULL */
if (curPair == NULL)
    goto BadCleanUp;


/*  We set up an array of pointers into the input list for easy indexed access */

_FMacro_PairsToArray(curPair, pairs, sizeof(pairs)/sizeof(pairs[0]));

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

if ((pairs[1] != NULL) &&
    ((pairs[1]->itsCar.Tag == TYPAIR) ||
     ((pairs[1]->itsCar.Tag == TYSYMBOL) && (strcmp(SymbolArray(pairs[1]->itsCar),"()") == 0))))
    {
    /*    0   1                   2      */
    /*  (let  ((var  init)...  )  exp...) */

    if ((pairs[1] != NULL) &&
        ((pairs[1]->itsCar.Tag == TYSYMBOL) && (strcmp(SymbolArray(pairs[1]->itsCar),"()") == 0)))
        {
        initPair = TPair_New(gCP,gTP);
        goto EvalBody;
        }

    /*  Process inits .... */
            
    tmpPair = pairs[1];
    if (tmpPair && asTag(&tmpPair->itsCar) == TYPAIR)
        tmpPair = asPair(&tmpPair->itsCar);
    if (tmpPair)
        {
        /*  We have some ((var  init)...  ) to process */
        
        initPair = TPair_New(gCP,gTP);
        do
            {
            if (tmpPair == NULL)
                {
                goto BadCleanUp;
                }
    
            if (asTag(&tmpPair->itsCar) == TYPAIR)
                {
                /*  The correct format for the init portion of a let is always (var init) */
                /*  FCompile_Resolve to allocate vars as needed. */
                
                _FMacro_PairsToArray(asPair(&tmpPair->itsCar), inits, sizeof(inits)/sizeof(inits[0]));
                if (asTag(&inits[0]->itsCar) != TYSYMBOL)
                    goto BadCleanUp;
                *ret = FCompile_Resolve(gCP, gTP, compilerState, asSymbol(&inits[0]->itsCar), gTP->FCompile_LambdaNest ? AMPVOFFSET : AMGVOFFSET);
                _FCompile_FrameChk(*ret);
                
                if (asTag(ret) == TYVOID)
                    goto BadCleanUp;
                else
                    {
                    /*  If FCompile_Resolve succeeds the it will return an overloaded TVAL with */
                    /*  the allocated var. */
                    
                    *sym = *ret;
                    }
                
                if ((inits[0] != NULL) && (inits[1] != NULL) && asTag(&inits[0]->itsCar) == TYSYMBOL)
                    {
                    /*  In order to do the init, we simply setup the return location for */
                    /*  code generated by the recognizer and then call it to perform the */
                    /*  code generation for the init. */
                    
                    initPair->itsCar = inits[1]->itsCar;
                    if (asTag(&initPair->itsCar) == TYVOID)
                        {
                        goto BadCleanUp;
                        }                  
                    _Result(compilerState) = *sym;
                    *ret = FCompile_Recognize(gCP, gTP, compilerState, initPair);
                    _FCompile_FrameChk(*ret);
                    }
                else
                    {
                    /*  We have an (var init) which does not follow the correct format. */
                    
                    goto BadCleanUp;
                    }
                }
            else
                {
                /*  The format for inits in a let is ((var init) (var init)...) so if the first */
                /*  element in the sublist is not a list then we have an error. */
                
                goto BadCleanUp;
                }
            } while ((tmpPair = _FCompile_CdrPair(tmpPair)) != NULL);
            
        EvalBody:
        tmpPair = pairs[2];
        while (tmpPair && asTag(&tmpPair->itsCar) != TYVOID)
            {
            /*  If there is any expression code for this let then we will */
            /*  send the init expression to the recognizer for compilation. */
            
            initPair->itsCar = tmpPair->itsCar;
            
            *ret = FCompile_Recognize(gCP, gTP, compilerState, initPair);
            _FCompile_FrameChk(*ret);
            tmpPair = _FCompile_CdrPair(tmpPair);   
            }
            
        /*  Now if we need to we generate the code to return the result in the correct location. */
        
        if (!isNullTval(final))
            {
            /*  If an explicit final result was requested, we may have to generate code for it */
            
            *ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
            }
        }
    else
        {
        /*  syntax error...  */
        *ret = gCP->Tval_VOID;
        goto BadCleanUp;
        }
    }
else
    {
    /*  syntax error...  */
    *ret = gCP->Tval_VOID;
    goto BadCleanUp;
    }

/*  We return the overloaded TVAL containing the location for the last instruction generated. */

_FCompile_FrameExit(*ret);

BadCleanUp:
*ret = TERROR("!SmartLisp Compiler: cannot parse let!");
_FCompile_FrameExit(*ret);

}

/*  We experiment with turning framing off for selected procedures to try to improve performance. */

/*#include "disableframing.h" */

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms1_GetResult

This procedure sets up an overloaded TVAL with the information defining the position in the
Tv to utilize in generating code for a result or return value from a special form. At this time
this is a fixed location.

We reserve the first available temp after the group used for function calls to hold results
for special forms. We can reuse this slot in nested special forms and thus mitigate the need
to create unwanted temporary variables. 

#endif

TVAL    FSpecialForms1_GetResult(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState)
{
StartFrame
DeclareOBJ(TStructure,tmpEnv);
DeclareTVAL(ret);
EndFrame

/*  Set up and return an overloaded TVAL with the offset and modifier for the special forms result */
/*  location in the Tv Structure. */

if(_SFReturnN(compilerState) == -1 || _SFReturnN(compilerState) > _TempN(compilerState))
    {
    /*  We need to (re)allocate a new value for _SFReturnN */
    
    *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
    ExitOnError(*ret);
    _SFReturnN(compilerState) = asOffset(ret);
    }
else
    {
    /*  We have already allocated _SFReturnN. We will reuse the previously allocated value */
    
    tmpEnv = _Tv(compilerState);
    asOffset(ret) = _SFReturnN(compilerState);
    asModifier(ret) = AMTVOFFSET;
    asObject(ret) = atHMBind(tmpEnv->itsDictionaryArray,_SFReturnN(compilerState)).Key;
    asTag(ret) = TYSYMBOL;

    if(_SFReturnN(compilerState) == _TempN(compilerState))
        {
        /*  We are utilizing a previously allocated _SFReturnN */
        /*  Increment the control counter which provides an index for the next available temporary. */
        
        _TempN(compilerState)++;
        }
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms1_AllocTemp

Arbitrary temporaries required for special form processing are allocated after the vars: for
a given lambda object. These are managed as a stack, new ones are allocated as needed, and then
freed up for reuse when they become available again. Availability is governed by a simple counter
which acts as an index into the Tv.

#endif

TVAL    FSpecialForms1_AllocTemp(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState,NUM preferredType)
{
NUM					n;
char                buf[MAXTVALTEXTLEN+10];
TVAL				voidPref;
TVAL				intPref;
TVAL				realPref;
StartFrame
DeclareOBJ(TStructure,tmpEnv);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
EndFrame

/*  Set up the TVALs with the offset, preferred types, and modifier for the the temporary */

voidPref = gCP->TObject_VOID; voidPref.DeclaredType = TYTVAL;
intPref.Tag = TYNUM; intPref.u.Int = 0; intPref.DeclaredType = TYNUM;
realPref.Tag = TYREAL; realPref.u.Real = 0.0; realPref.DeclaredType = TYREAL;

/*  Set up and return an overloaded TVAL with the offset and modifier for the a temporary */
/*  location in the Tv Structure. */

if(_TempN(compilerState) == _SFReturnN(compilerState))
    {
    /*  We have previously allocated the next available temp for _SFReturnN, since we would */
    /*  like to reuse this temp for another purpose we will reinit _SFReturnN to -1 so that */
    /*  the next time FSpecialForms1_GetResult is called a new value will be allocated. */
    
    _SFReturnN(compilerState) = -1;
    }

/* Locate the temporary variable in the current Lambda. */

tmpEnv = _Tv(compilerState);
sprintf((char*)buf, "__T%ld", (LONG)_TempN(compilerState));
aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)buf);
for (n = 0; n < tmpEnv->itsMaxItemIndex; ++n)
    {
    if (atHMBind(tmpEnv->itsDictionaryArray,n).Key == (TObject*)aSymbol) goto TemporaryExists;
    }


/*  Allocate the temporary variables, for this _TempN level, in the current Lambda. */

/*  We must allocate a new Integer temporary register. Generate a name for this */
/*  register based on the binding index which will be utilized for its storage. */

sprintf((char*)buf, "__R%ld", (LONG)_TempN(compilerState));
aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)buf);
FObject_Perm(gCP,gTP,(TObject*) aSymbol,FALSE);

/*  Add to the new temporary register variable to the Lambda register variables, */
/*  then initialize the new temporary variable to a preferred type of Integer.  */

*ret = FCompile_ResolveTemp(gCP, gTP, compilerState, aSymbol, AMREGISTER, intPref);
ExitOnError(*ret);
_FCompile_MakeOBJ(*ret, aSymbol);

/*  We must allocate a new Number temporary register. Generate a name for this  */
/*  register based on the binding index which will be utilized for its storage. */

sprintf((char*)buf, "__RN%ld", (LONG)_TempN(compilerState));
aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)buf);
FObject_Perm(gCP,gTP,(TObject*) aSymbol,FALSE);

/*  Add to the new temporary register variable to the Lambda register variables, */
/*  then initialize the new temporary variable to a preferred type of Number.   */

*ret = FCompile_ResolveTemp(gCP, gTP, compilerState, aSymbol, AMREGISTER, realPref);
ExitOnError(*ret);
_FCompile_MakeOBJ(*ret, aSymbol);

/*  We must allocate a new Word temporary variable. Generate a name for this    */
/*  variable based on the binding index which will be utilized for its storage. */

sprintf((char*)buf, "__T%ld", (LONG)_TempN(compilerState));
aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)buf);
FObject_Perm(gCP,gTP,(TObject*) aSymbol,FALSE);

/*  Add to the new temporary Word variable to the Lambda register variables, */
/*  then initialize the new temporary variable to a preferred type of Word. */

*ret = FCompile_ResolveTemp(gCP, gTP, compilerState, aSymbol, AMTVOFFSET, voidPref);
ExitOnError(*ret);
_FCompile_MakeOBJ(*ret, aSymbol);

TemporaryExists:

/* We now resolve the temporary variable and return it to the caller */
if (preferredType == TYNUM)
	{
    /*  Locate the proper temporary Integer register variable.  */
    sprintf((char*)buf, "__R%ld", (LONG)_TempN(compilerState));
    aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)buf);
    *ret = FCompile_Resolve(gCP, gTP, compilerState, aSymbol, AMREGISTER);
    ExitOnError(*ret);
	}
else   
if (preferredType == TYREAL)
	{
    /*  Locate the proper temporary Number register variable.  */
    sprintf((char*)buf, "__RN%ld", (LONG)_TempN(compilerState));
    aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)buf);
    *ret = FCompile_Resolve(gCP, gTP, compilerState, aSymbol, AMREGISTER);
    ExitOnError(*ret);
	}
else   
	{
    /*  Locate the proper temporary Word memory variable.  */
    sprintf((char*)buf, "__T%ld", (LONG)_TempN(compilerState));
    aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)buf);
    *ret = FCompile_Resolve(gCP, gTP, compilerState, aSymbol, AMTVOFFSET);
    ExitOnError(*ret);
	}

/*  Increment the control counter which provides an index for the next available temporary. */

_TempN(compilerState)++;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms1_CodeMove

The process of generating the code for the correct return value for a special form is similar
for most special forms. This procedure provides a function which performs code generation at exit 
from a special form to do a move to return the required return value as needed.

This function sets LastResult.

#endif

TVAL    FSpecialForms1_CodeMove(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TVAL source, TVAL target)
{
_FCompile_StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame


if( asModifier(&target) != asModifier(&source) || 
    asOffset(&target) != asOffset(&source)  ||
    asObject(&target) != asObject(&source))
    {
    /*  If we need to generate code to set the target result, then we will do so. */
    /*  We usually only need to do so when _Result(compilerState) was not void at entry to a special form. */
    
    /*  Start encoding of the parmameter array for FOptimize_OptimizePcode */
    
    _FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
    _FCompile_MakeINT(prmv[1], VMMOVE);
    _FCompile_MakeINT(prmv[2], asModifier(&source));
    _FCompile_MakeSYM(prmv[5], source);
    _FCompile_MakeINT(prmv[3], asModifier(&target));
    _FCompile_MakeSYM(prmv[6], target);
    _FCompile_MakeINT(prmv[4], AMVOID);
    *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,7,&prmv[0]);
    _FCompile_FrameChk(*ret);
    }
    
/*  Set _LastResult(compilerState) so that the previous form or function knows where the source */
/*  result was generated. */

_LastResult(compilerState) = target;
    
_FCompile_FrameExit(target);
}
