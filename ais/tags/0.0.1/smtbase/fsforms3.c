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

#define _C_FSPECIALFORMS3
#define _SMARTBASE
#if 0
FSpecialForms3.c

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

#include    "fsforms3.h"
#include    "fcompile.h"
#include    "foptimize.h"
#include    "flisp.h"
#include    "fmacro.h"
#include    "fopt1.h"
#include    "fopt2.h"
#include    "fsforms1.h"
#include    "futil1.h"
#include    "futil2.h"
#include    "tlambda.h"
#include    "terror.h"
#include    "tobjvec.h"
#include    "tpair.h"
#include    "tstruct.h"

/*--------------------------------------------------------------------------------------- */
/*
FSpecialForms3_Defmacro

This procedure provides support for code generation for the defmacro special form. The defmacro
special form is a flavor of define in which a new lambda object will be generated. Please see the 
FSpecialForms1_lambda procedure for a description of the nature and control of this processing.

(defmacro	name(arg...)  vars:  (var...)  pvars:  (var...)  exp...)
                 "             "             "
                 "             "             "
                 V             V             V
(define	macro:	(name  arg...)  vars:  (var...)  pvars:  (var...)  exp...)

*/

TVAL    FSpecialForms3_Defmacro(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(parentName);
DeclareTVAL(parentLambda);
DeclareTVAL(faces);
DeclareTVAL(parentPv);
DeclareTVAL(parentCv);
DeclareTVAL(childName);
DeclareTVAL(childBody);
DeclareTVAL(childLambda);
DeclareTVAL(macros);
DeclareTVAL(tmp);
DeclareTVAL(head);
DeclareTVAL(tail);
DeclareTVAL(result);
DeclareOBJArray(TPair,pairs,20);
EndFrame

/* Is this a global macro declaration? */
if ((argPair->itsCdr.Tag == TYPAIR) &&
	(argPair->itsCdr.u.Pair->itsCar.Tag == TYSYMBOL) &&
	(argPair->itsCdr.u.Pair->itsCdr.Tag == TYPAIR) &&
	((argPair->itsCdr.u.Pair->itsCdr.u.Pair->itsCar.Tag == TYPAIR) ||
	 ((argPair->itsCdr.u.Pair->itsCdr.u.Pair->itsCar.Tag == TYSYMBOL) && 
	  (argPair->itsCdr.u.Pair->itsCdr.u.Pair->itsCar.u.Symbol == gCP->TLambda_nil)
	  )
	 )
	)
	{
	*tmp = FMacro_Defmacro(gCP,gTP,1,&argPair->itsCdr);
	ExitOnError(*tmp);
	*result = FSpecialForms1_define(gCP,gTP,compilerState,tmp->u.Pair);
	FrameExit(*result);
	}

/* Is this a child macro declaration? */

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;

/*  Initialize */

_FMacro_PairsToArray(argPair, pairs, 4);
if (pairs[3] == NULL)  goto BadCleanUp;
*parentName = pairs[1]->itsCar;
*childName = pairs[2]->itsCar;
childBody->u.Pair = pairs[3]; childBody->Tag = TYPAIR;

/* Create head of list as follows: (morph '(defun childName childBody)) */

*head = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("defun"),*childName);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("setLastCdr"),2,*head,*childBody);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("apply"),2,TGVALUE("makeQuotedList"),*head);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("morph"),*head);
ExitOnError(*head);

if ((childName->Tag == TYSYMBOL) || (childName->Tag == TYQUOTEDSYMBOL))
	{
	childName->Tag = TYSYMBOL;
	childName->QuoteCnt = 0;
	}
else
	{
	FrameExit(TERROR("!defmacro: macro name must be a symbol!"));
	}

/* Locate the parent Lambda. */
 
if ((parentName->Tag == TYSYMBOL) || (parentName->Tag == TYQUOTEDSYMBOL))
	{
	parentName->Tag = TYSYMBOL;
	parentName->QuoteCnt = 0;
	}
*parentLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*parentName);
ExitOnError(*parentLambda);
if ((parentLambda->Tag != TYLAMBDA) && (parentLambda->Tag != TYMACRO)) goto BadCleanUp;

/* Locate the parent Lambda Pv and Cv structures. */

*parentCv = TOBJ(parentLambda->u.Lambda->ConstantVariables);
*parentPv = TOBJ(parentLambda->u.Lambda->PersistantVariables);
if (parentPv->Tag != TYSTRUCTURE)
	{
	parentPv->u.Structure = TStructure_New(gCP,gTP);
	parentPv->Tag = TYSTRUCTURE;
	parentLambda->u.Lambda->PersistantVariables = parentPv->u.Structure;
	}

/* Create tail of list as follows: (makeLambda Pv: (ref parentName Pv:) Cv: (ref parentName Cv:)) */
/* Note: the tail of list is the portion inheriting the parental properties                      */

if (gTP->FCompile_GenerateDebugInfo == TRUE)
	{
	/* We are generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),7,TSYMBOL("makeLambda"),TQSYMBOL("Pv"),*parentPv,TQSYMBOL("Cv"),*parentCv,TQSYMBOL("In"),gTP->FCompile_DebugInterfaces);
	ExitOnError(*tail);
	}
else
	{
	/* We are NOT generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),5,TSYMBOL("makeLambda"),TQSYMBOL("Pv"),*parentPv,TQSYMBOL("Cv"),*parentCv);
	ExitOnError(*tail);
	}
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),4,TSYMBOL("compile"),*head,*tail,gCP->Tval_TRUE);
ExitOnError(*tail);
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("eval"),*tail);
ExitOnError(*tail);
*childLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*tail);
ExitOnError(*childLambda);
if ((childLambda->Tag != TYLAMBDA) && (childLambda->Tag != TYMACRO)) goto BadCleanUp;
childLambda->Tag = TYMACRO;

/* Set the child Lambda into the parent Lambda immediately as follows: */

*result = TStructure_SetIV1(gCP,gTP,*parentPv,*childName,*childLambda);
ExitOnError(*result);

/* Add the child name to the parent Lambda macros list (if necessary). */
 
if ((parentLambda->Tag != TYVOID) && (parentLambda->u.Lambda->Interfaces != NIL))
	{
	*faces = TOBJ(parentLambda->u.Lambda->Interfaces);
	*tmp = TSYMBOL("macros");
	*macros = FSmartbase_Eval(gCP,gTP,TGVALUE("ref"),2,*faces,*tmp);
	ExitOnError(*macros);
	if (macros->Tag == TYSTRUCTURE)
		{
		*tmp = TStructure_SetIV1(gCP,gTP,*macros,*childName,*childLambda);
		ExitOnError(*tmp);
		}
	}

/* ************************************************************************ */
/* Generate the code which returns the childLambda as the result value.		*/
/* ************************************************************************ */

/* Generate (set parentPv childName childLambda)	*/
*childName = FSmartbase_Eval(gCP,gTP,TGVALUE("makeQuotedSymbol"),1,*childName);
ExitOnError(*childName);
*parentPv = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentName,TQSYMBOL("Pv"));
ExitOnError(*parentPv);
*result = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentPv,*childName);
ExitOnError(*result);
*result = FCompile_Call(gCP,gTP,compilerState,result->u.Pair);
ExitOnError(*result);

FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
    
/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_Defmethod

This procedure provides support for code generation for the defmethod special form. 

    (defmethod parent: msg(arg...) vars: (var...) pvars: (var...) exp ...)


#endif

TVAL    FSpecialForms3_Defmethod(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* argPair)
{
TYPE            type;
StartFrame
DeclareOBJ(TSymbol,typSymbol);
DeclareOBJ(TSymbol,msgSymbol);
DeclareTVAL(native);
DeclareTVAL(parentName);
DeclareTVAL(parentLambda);
DeclareTVAL(faces);
DeclareTVAL(parentPv);
DeclareTVAL(parentCv);
DeclareTVAL(parentSv);
DeclareTVAL(childName);
DeclareTVAL(childBody);
DeclareTVAL(childLambda);
DeclareTVAL(methods);
DeclareTVAL(tmp);
DeclareTVAL(head);
DeclareTVAL(tail);
DeclareTVAL(result);
DeclareOBJArray(TPair,pairs,20);
EndFrame

/* Is this a global method declaration? */
if ((argPair->itsCdr.Tag == TYPAIR) &&
	(argPair->itsCdr.u.Pair->itsCar.Tag == TYSYMBOL) &&
	(argPair->itsCdr.u.Pair->itsCdr.Tag == TYPAIR) &&
	(argPair->itsCdr.u.Pair->itsCdr.u.Pair->itsCar.Tag == TYPAIR)
	)
	{
	*result = TERROR("!defmethod: a method must have a parent Lambda which is a class!");
	FrameExit(*result);
	}

/* Is this a child method declaration? */

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;

/*  Initialize */

_FMacro_PairsToArray(argPair, pairs, 4);
if (pairs[3] == NULL)  goto BadCleanUp;
*parentName = pairs[1]->itsCar;
*childName = pairs[2]->itsCar;
childBody->u.Pair = pairs[3]; childBody->Tag = TYPAIR;

/* Create head of list as follows: (morph '(defun childName childBody)) */

*head = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("defun"),*childName);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("setLastCdr"),2,*head,*childBody);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("apply"),2,TGVALUE("makeQuotedList"),*head);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("morph"),*head);
ExitOnError(*head);

if ((childName->Tag == TYSYMBOL) || (childName->Tag == TYQUOTEDSYMBOL))
	{
	childName->Tag = TYSYMBOL;
	childName->QuoteCnt = 0;
	}
else
	{
	FrameExit(TERROR("!defmethod: method name must be a symbol!"));
	}

/* Locate the parent Lambda. */
 
if ((parentName->Tag == TYSYMBOL) || (parentName->Tag == TYQUOTEDSYMBOL))
	{
	parentName->Tag = TYSYMBOL;
	parentName->QuoteCnt = 0;

	/* If the parent name is the symbol of a native type or a user defined structure type, */
	/* then we want to add this new method to the methods list of the global type symbol.  */

	typSymbol = parentName->u.Symbol;
	msgSymbol = childName->u.Symbol;
	*native = FUtil2_SymbolToNativeType(gCP,gTP,&type,typSymbol);
	if (((native->Tag == TYBOLE) && (native->u.Bool == TRUE)) || (typSymbol->itsUserTypeFields != NULL))
		{
		/* Now we call addMethod to extend the methods list of the specified global type.  */
		*tmp = FMacro_Defmethod(gCP,gTP,1,&argPair->itsCdr);
		*result = FCompile_Call(gCP,gTP,compilerState,tmp->u.Pair);
		FrameExit(*result);
		}
	}
*parentLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*parentName);
ExitOnError(*parentLambda);
if ((parentLambda->Tag != TYLAMBDA) && (parentLambda->Tag != TYMACRO)) goto BadCleanUp;

/* Locate the parent Lambda Pv and Cv structures. */

*parentCv = TOBJ(parentLambda->u.Lambda->ConstantVariables);
*parentPv = TOBJ(parentLambda->u.Lambda->PersistantVariables);
*parentSv = TOBJ(parentLambda->u.Lambda->ClassVariables);
if (parentPv->Tag != TYSTRUCTURE)
	{
	parentPv->u.Structure = TStructure_New(gCP,gTP);
	parentPv->Tag = TYSTRUCTURE;
	parentLambda->u.Lambda->PersistantVariables = parentPv->u.Structure;
	}

/* Create tail of list as follows: (makeLambda Pv: (ref parentName Pv:) Cv: (ref parentName Cv:)) */
/* Note: the tail of list is the portion inheriting the parental properties                      */

if (gTP->FCompile_GenerateDebugInfo == TRUE)
	{
	/* We are generating debugging information during this compile. */
	tail->u.Lambda = TLambda_New(gCP,gTP);
	tail->Tag = TYLAMBDA;
	tail->u.Lambda->PersistantVariables = (parentPv->Tag != TYSTRUCTURE) ? NIL : parentPv->u.Structure;
	tail->u.Lambda->ConstantVariables = (parentCv->Tag != TYSTRUCTURE) ? NIL : parentCv->u.Structure;
	tail->u.Lambda->ClassVariables = (parentSv->Tag != TYSTRUCTURE) ? NIL : parentSv->u.Structure;
	tail->u.Lambda->Interfaces = (gTP->FCompile_DebugInterfaces.Tag != TYSTRUCTURE) ? NIL : gTP->FCompile_DebugInterfaces.u.Structure;
	}
else
	{
	/* We are NOT generating debugging information during this compile. */
	tail->u.Lambda = TLambda_New(gCP,gTP);
	tail->Tag = TYLAMBDA;
	tail->u.Lambda->PersistantVariables = (parentPv->Tag != TYSTRUCTURE) ? NIL : parentPv->u.Structure;
	tail->u.Lambda->ConstantVariables = (parentCv->Tag != TYSTRUCTURE) ? NIL : parentCv->u.Structure;
	tail->u.Lambda->ClassVariables = (parentSv->Tag != TYSTRUCTURE) ? NIL : parentSv->u.Structure;
	}
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),4,TSYMBOL("compile"),*head,*tail,gCP->Tval_TRUE);
ExitOnError(*tail);
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("eval"),*tail);
ExitOnError(*tail);
*childLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*tail);
ExitOnError(*childLambda);

/* Set the child Lambda into the parent Lambda immediately as follows: */

*result = TStructure_SetIV1(gCP,gTP,*parentPv,*childName,*childLambda);
ExitOnError(*result);

/* Add the child name to the parent Lambda macros list (if necessary). */
 
if ((parentLambda->Tag != TYSTRUCTURE) && (parentLambda->u.Lambda->Interfaces != NIL))
	{
	*faces = TOBJ(parentLambda->u.Lambda->Interfaces);
	*tmp = TSYMBOL("methods");
	*methods = FSmartbase_Eval(gCP,gTP,TGVALUE("ref"),2,*faces,*tmp);
	ExitOnError(*methods);
	if (methods->Tag == TYSTRUCTURE)
		{
		*tmp = TStructure_SetIV1(gCP,gTP,*methods,*childName,*childLambda);
		ExitOnError(*tmp);
		}
	}

/* ************************************************************************ */
/* Generate the code which returns the childLambda as the result value.		*/
/* ************************************************************************ */

/* Generate (set parentPv childName childLambda)	*/
*childName = FSmartbase_Eval(gCP,gTP,TGVALUE("makeQuotedSymbol"),1,*childName);
ExitOnError(*childName);
*parentPv = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentName,TQSYMBOL("Pv"));
ExitOnError(*parentPv);
*result = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentPv,*childName);
ExitOnError(*result);
*result = FCompile_Call(gCP,gTP,compilerState,result->u.Pair);
ExitOnError(*result);

FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
        
/*--------------------------------------------------------------------------------------- */
/*
FSpecialForms3_Defchild

This procedure provides support for code generation for the defchild special form. 

    (defchild parent: name(arg...) vars: (var...) pvars: (var...) exp ...)
                 "             "             "
                 "             "             "
                 V             V             V
    (defun parent(...) 
       (defun name(arg...) vars: (var...) pvars: (var...) exp ...) 
       ...parent continues hereafter...)

This function was originally encoded in Lisp as follows:

(defchild parentName childName (...)
  vars:(result tail)
  (setq result (apply makeQuotedList (setLastCdr (list defun: childName) (argFetch 2))))
  (setq result (list morph: result))
  (if (isSymbol parentName)
      (setq tail (list 'makeLambda ''Pv (list 'ref (symbol parentName) ''Pv) ''Cv (list 'ref (symbol parentName) ''Cv))))
  (if (isPair parentName)
      (setq tail (list 'makeLambda ''Pv (list 'ref parentName ''Pv) ''Cv (list 'ref parentName ''Cv))))
  (setq result (list 'ref (list compile: result tail true) (makeQuotedSymbol childName)))
  result)

*/

TVAL    FSpecialForms3_Defchild(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(parentName);
DeclareTVAL(parentPv);
DeclareTVAL(parentCv);
DeclareTVAL(parentSv);
DeclareTVAL(parentLambda);
DeclareTVAL(childName);
DeclareTVAL(childBody);
DeclareTVAL(childLambda);
DeclareTVAL(head);
DeclareTVAL(tail);
DeclareTVAL(result);
DeclareOBJArray(TPair,pairs,20);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;

/*  Initialize */

_FMacro_PairsToArray(argPair, pairs, 4);
if (pairs[3] == NULL)  goto BadCleanUp;
*parentName = pairs[1]->itsCar;
*childName = pairs[2]->itsCar;
childBody->u.Pair = pairs[3]; childBody->Tag = TYPAIR;

/* Create head of list as follows: (morph '(defun childName childBody)) */

*head = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("defun"),*childName);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("setLastCdr"),2,*head,*childBody);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("apply"),2,TGVALUE("makeQuotedList"),*head);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("morph"),*head);
ExitOnError(*head);

if ((childName->Tag == TYSYMBOL) || (childName->Tag == TYQUOTEDSYMBOL))
	{
	childName->Tag = TYSYMBOL;
	childName->QuoteCnt = 0;
	}
else
	{
	FrameExit(TERROR("!defchild: child name must be a symbol!"));
	}

/* Locate the parent Lambda. */
 
if ((parentName->Tag == TYSYMBOL) || (parentName->Tag == TYQUOTEDSYMBOL))
	{
	parentName->Tag = TYSYMBOL;
	parentName->QuoteCnt = 0;
	}
*parentLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*parentName);
ExitOnError(*parentLambda);
if ((parentLambda->Tag != TYLAMBDA) && (parentLambda->Tag != TYMACRO)) goto BadCleanUp;

/* Locate the parent Lambda Pv and Cv structures. */

*parentCv = TOBJ(parentLambda->u.Lambda->ConstantVariables);
*parentPv = TOBJ(parentLambda->u.Lambda->PersistantVariables);
*parentSv = TOBJ(parentLambda->u.Lambda->ClassVariables);
if (parentPv->Tag != TYSTRUCTURE)
	{
	parentPv->u.Structure = TStructure_New(gCP,gTP);
	parentPv->Tag = TYSTRUCTURE;
	parentLambda->u.Lambda->PersistantVariables = parentPv->u.Structure;
	}

/* Create tail of list as follows: (makeLambda Pv: (ref parentName Pv:) Cv: (ref parentName Cv:)) */
/* Note: the tail of list is the portion inheriting the parental properties                      */

if (gTP->FCompile_GenerateDebugInfo == TRUE)
	{
	/* We are generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),9,TSYMBOL("makeLambda"),TQSYMBOL("Pv"),*parentPv,TQSYMBOL("Cv"),*parentCv,TQSYMBOL("Sv"),*parentSv,TQSYMBOL("In"),gTP->FCompile_DebugInterfaces);
	ExitOnError(*tail);
	}
else
	{
	/* We are NOT generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),7,TSYMBOL("makeLambda"),TQSYMBOL("Pv"),*parentPv,TQSYMBOL("Cv"),*parentCv,TQSYMBOL("Sv"),*parentSv);
	ExitOnError(*tail);
	}
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),4,TSYMBOL("compile"),*head,*tail,gCP->Tval_TRUE);
ExitOnError(*tail);
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("eval"),*tail);
ExitOnError(*tail);
*childLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*tail);
ExitOnError(*childLambda);

/* Set the child Lambda into the parent Lambda immediately as follows: */

*result = TStructure_SetIV1(gCP,gTP,*parentPv,*childName,*childLambda);
ExitOnError(*result);

/* ************************************************************************ */
/* Generate the code which returns the childLambda as the result value.		*/
/* ************************************************************************ */

/* Generate (set parentPv childName childLambda)	*/
*childName = FSmartbase_Eval(gCP,gTP,TGVALUE("makeQuotedSymbol"),1,*childName);
ExitOnError(*childName);
*parentPv = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentName,TQSYMBOL("Pv"));
ExitOnError(*parentPv);
*result = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentPv,*childName);
ExitOnError(*result);
*result = FCompile_Call(gCP,gTP,compilerState,result->u.Pair);
ExitOnError(*result);

FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}


/*--------------------------------------------------------------------------------------- */
/*
FSpecialForms3_Defclone

This procedure provides support for code generation for the defcopy special form. 

    (defclone parent: name(arg...) vars: (var...) pvars: (var...) exp ...)
                 "             "             "
                 "             "             "
                 V             V             V
    (defun parent(...) 
       (defun name(arg...) vars: (var...) pvars: (var...) exp ...) 
       ...parent continues hereafter...)

This function would be encoded in Lisp as follows:

(define name (eval (compile '((lambda() ...user cose...)) (copy parent))))

*/

TVAL    FSpecialForms3_Defclone(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(parentName);
DeclareTVAL(parentPv);
DeclareTVAL(parentCv);
DeclareTVAL(parentIn);
DeclareTVAL(parentSv);
DeclareTVAL(parentLambda);
DeclareTVAL(childName);
DeclareTVAL(childBody);
DeclareTVAL(childLambda);
DeclareTVAL(head);
DeclareTVAL(tail);
DeclareTVAL(result);
DeclareOBJArray(TPair,pairs,20);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;

/*  Initialize */

_FMacro_PairsToArray(argPair, pairs, 4);
if (pairs[3] == NULL)  goto BadCleanUp;
*parentName = pairs[1]->itsCar;
*childName = pairs[2]->itsCar;
childBody->u.Pair = pairs[3]; childBody->Tag = TYPAIR;

/* Create head of list as follows: (morph '(defun childName childBody)) */

*head = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("defun"),*childName);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("setLastCdr"),2,*head,*childBody);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("apply"),2,TGVALUE("makeQuotedList"),*head);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("morph"),*head);
ExitOnError(*head);

if ((childName->Tag == TYSYMBOL) || (childName->Tag == TYQUOTEDSYMBOL))
	{
	childName->Tag = TYSYMBOL;
	childName->QuoteCnt = 0;
	}
else
	{
	FrameExit(TERROR("!defclone: child name must be a symbol!"));
	}

/* Locate the parent Lambda. */
 
if ((parentName->Tag == TYSYMBOL) || (parentName->Tag == TYQUOTEDSYMBOL))
	{
	parentName->Tag = TYSYMBOL;
	parentName->QuoteCnt = 0;
	}
*parentLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*parentName);
ExitOnError(*parentLambda);
if ((parentLambda->Tag != TYLAMBDA) && (parentLambda->Tag != TYMACRO)) goto BadCleanUp;

/* Make a copy of the parent Lambda. */
 
*parentLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("copy"),1,*parentLambda);
if ((parentLambda->Tag != TYLAMBDA) && (parentLambda->Tag != TYMACRO)) goto BadCleanUp;

/* Locate the parent Lambda Pv and Cv structures. */

*parentCv = TOBJ(parentLambda->u.Lambda->ConstantVariables);
*parentPv = TOBJ(parentLambda->u.Lambda->PersistantVariables);
*parentSv = TOBJ(parentLambda->u.Lambda->ClassVariables);
*parentIn = TOBJ(parentLambda->u.Lambda->Interfaces);

/* Create tail of list as follows: (makeLambda Pv: (ref parentName Pv:) Cv: (ref parentName Cv:)) */
/* Note: the tail of list is the portion inheriting the parental properties                      */

if (gTP->FCompile_GenerateDebugInfo == TRUE)
	{
	/* We are generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),9,TSYMBOL("makeLambda"),TQSYMBOL("Pv"),*parentPv,TQSYMBOL("Cv"),*parentCv,TQSYMBOL("Sv"),*parentSv,TQSYMBOL("In"),gTP->FCompile_DebugInterfaces);
	ExitOnError(*tail);
	}
else
	{
	/* We are NOT generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),7,TSYMBOL("makeLambda"),TQSYMBOL("Pv"),*parentPv,TQSYMBOL("Cv"),*parentCv,TQSYMBOL("Sv"),*parentSv);
	ExitOnError(*tail);
	}
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),4,TSYMBOL("compile"),*head,*tail,gCP->Tval_TRUE);
ExitOnError(*tail);
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("eval"),*tail);
ExitOnError(*tail);
*childLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*tail);
ExitOnError(*childLambda);

/* Set the child Lambda into the global child Lambda name immediately as follows: */

childName->u.Symbol->itsGlobalValue = *childLambda;

/* ************************************************************************ */
/* Generate the code which returns the childLambda as the result value.		*/
/* ************************************************************************ */

/* Generate (set parentPv childName childLambda)	*/
*childName = FSmartbase_Eval(gCP,gTP,TGVALUE("makeQuotedSymbol"),1,*childName);
ExitOnError(*childName);
*result = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("set"),*childName,*childLambda);
ExitOnError(*result);
*result = FCompile_Call(gCP,gTP,compilerState,result->u.Pair);
ExitOnError(*result);

FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}


/*--------------------------------------------------------------------------------------- */
/*
FSpecialForms3_Deforphan

This procedure provides support for code generation for the defmethod special form. 

    (deforphan parent: name(arg...) vars: (var...) pvars: (var...) exp ...)

This function was originally encoded in Lisp as follows:

(defmacro deforphan(parentName childName ...)
  vars:(result tail)
  (setq tail (setLastCdr (list lambda:) (argFetch 2)))
  (if (isSymbol parentName)
      (setq result (list set: (list ref: (symbol parentName) ''Pv) (makeQuotedSymbol childName) tail)))
  (if (isPair parentName)
      (setq result (list set: (list ref: parentName ''Pv) (makeQuotedSymbol childName) tail)))
  (setq result (list 'ref result (makeQuotedSymbol childName)))
  result)

*/

TVAL    FSpecialForms3_Deforphan(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(parentName);
DeclareTVAL(parentPv);
DeclareTVAL(parentCv);
DeclareTVAL(parentLambda);
DeclareTVAL(childName);
DeclareTVAL(childBody);
DeclareTVAL(childLambda);
DeclareTVAL(head);
DeclareTVAL(tail);
DeclareTVAL(result);
DeclareOBJArray(TPair,pairs,20);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;

/*  Initialize */

_FMacro_PairsToArray(argPair, pairs, 4);
if (pairs[3] == NULL)  goto BadCleanUp;
*parentName = pairs[1]->itsCar;
*childName = pairs[2]->itsCar;
childBody->u.Pair = pairs[3]; childBody->Tag = TYPAIR;

/* Create head of list as follows: (morph '(defun childName childBody)) */

*head = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("defun"),*childName);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("setLastCdr"),2,*head,*childBody);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("apply"),2,TGVALUE("makeQuotedList"),*head);
ExitOnError(*head);
*head = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("morph"),*head);
ExitOnError(*head);

if ((childName->Tag == TYSYMBOL) || (childName->Tag == TYQUOTEDSYMBOL))
	{
	childName->Tag = TYSYMBOL;
	childName->QuoteCnt = 0;
	}
else
	{
	FrameExit(TERROR("!defchild: child name must be a symbol!"));
	}

/* Locate the parent Lambda. */
 
if ((parentName->Tag == TYSYMBOL) || (parentName->Tag == TYQUOTEDSYMBOL))
	{
	parentName->Tag = TYSYMBOL;
	parentName->QuoteCnt = 0;
	}
*parentLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*parentName);
ExitOnError(*parentLambda);
if ((parentLambda->Tag != TYLAMBDA) && (parentLambda->Tag != TYMACRO)) goto BadCleanUp;

/* Locate the parent Lambda Pv and Cv structures. */

*parentCv = TOBJ(parentLambda->u.Lambda->ConstantVariables);
*parentPv = TOBJ(parentLambda->u.Lambda->PersistantVariables);
if (parentPv->Tag != TYSTRUCTURE)
	{
	parentPv->u.Structure = TStructure_New(gCP,gTP);
	parentPv->Tag = TYSTRUCTURE;
	parentLambda->u.Lambda->PersistantVariables = parentPv->u.Structure;
	}

/* Create tail of list as follows: (makeLambda Pv: (ref parentName Pv:) Cv: (ref parentName Cv:)) */
/* Note: the tail of list is the portion inheriting the parental properties                      */

if (gTP->FCompile_GenerateDebugInfo == TRUE)
	{
	/* We are generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),7,TSYMBOL("makeLambda"),TQSYMBOL("Pv"),gCP->TObject_VOID,TQSYMBOL("Cv"),gCP->TObject_VOID,TQSYMBOL("In"),gTP->FCompile_DebugInterfaces);
	ExitOnError(*tail);
	}
else
	{
	/* We are NOT generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),5,TSYMBOL("makeLambda"),TQSYMBOL("Pv"),gCP->TObject_VOID,TQSYMBOL("Cv"),gCP->TObject_VOID);
	ExitOnError(*tail);
	}
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),4,TSYMBOL("compile"),*head,*tail,gCP->Tval_TRUE);
ExitOnError(*tail);
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("eval"),*tail);
ExitOnError(*tail);
*childLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*tail);
ExitOnError(*childLambda);

/* Set the child Lambda into the parent Lambda immediately as follows: */

*result = TStructure_SetIV1(gCP,gTP,*parentPv,*childName,*childLambda);
ExitOnError(*result);

/* ************************************************************************ */
/* Generate the code which returns the childLambda as the result value.		*/
/* ************************************************************************ */

/* Generate (set parentPv childName childLambda)	*/
*childName = FSmartbase_Eval(gCP,gTP,TGVALUE("makeQuotedSymbol"),1,*childName);
ExitOnError(*childName);
*parentPv = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentName,TQSYMBOL("Pv"));
ExitOnError(*parentPv);
*result = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentPv,*childName);
ExitOnError(*result);
*result = FCompile_Call(gCP,gTP,compilerState,result->u.Pair);
ExitOnError(*result);

FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
    

/*--------------------------------------------------------------------------------------- */
/*
FSpecialForms3_Defriend

This procedure provides support for code generation for the defmethod special form. 

    (defriend parent: name(arg...) vars: (var...) pvars: (var...) exp ...)

This function was originally encoded in Lisp as follows:

(defmacro defriend(parentName childName ...)
  vars:(result tail friend parentPv childBody)
  (setq childBody (argFetch 2))
  (setq childName (makeQuotedSymbol childName))
  (if (isSymbol parentName) (setq parentName (symbol parentName)))
  (setq parentPv (list 'ref parentName ''Pv))
  (setq friend (apply makeQuotedList (setLastCdr (list lambda:) childBody)))
  (setq friend (list morph: friend))
  (setq tail (list 'makeLambda ''Cv parentPv)))
  (setq tail (list eval: (list compile: friend tail true)))
  (setq result (list set: parentPv childName tail)))
  (setq result (list 'ref result childName))
 result)

*/

TVAL    FSpecialForms3_Defriend(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(parentName);
DeclareTVAL(parentPv);
DeclareTVAL(parentSv);
DeclareTVAL(parentLambda);
DeclareTVAL(friendName);
DeclareTVAL(friendBody);
DeclareTVAL(friendLambda);
DeclareTVAL(friend);
DeclareTVAL(tmp);
DeclareTVAL(tail);
DeclareTVAL(result);
DeclareOBJArray(TPair,pairs,20);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;

/*  Initialize */

_FMacro_PairsToArray(argPair, pairs, 4);
if (pairs[3] == NULL)  goto BadCleanUp;
*parentName = pairs[1]->itsCar;
*friendName = pairs[2]->itsCar;
friendBody->u.Pair = pairs[3]; friendBody->Tag = TYPAIR;

/* Force all quoted symbol parent names to be just symbol names */
 
if ((parentName->Tag == TYSYMBOL) || (parentName->Tag == TYQUOTEDSYMBOL))
	{
	parentName->Tag = TYSYMBOL;
	parentName->QuoteCnt = 0;
	}

/* Force all symbol child names to be quoted symbol child names */
 
if ((friendName->Tag == TYSYMBOL) || (friendName->Tag == TYQUOTEDSYMBOL))
	{
	friendName->Tag = TYQUOTEDSYMBOL;
	friendName->QuoteCnt = 1;
	}
else
	{
	FrameExit(TERROR("!defriend: friend name must be a symbol!"));
	}

/* Locate the parent Pv element. */

*parentLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*parentName);
ExitOnError(*parentLambda);
if (parentLambda->Tag != TYLAMBDA) goto BadCleanUp;
*parentSv = TOBJ(parentLambda->u.Lambda->ClassVariables);
*parentPv = TOBJ(parentLambda->u.Lambda->PersistantVariables);
if (parentPv->Tag != TYSTRUCTURE)
	{
	parentPv->u.Structure = TStructure_New(gCP,gTP);
	parentPv->Tag = TYSTRUCTURE;
	parentLambda->u.Lambda->PersistantVariables = parentPv->u.Structure;
	}

/* Create the friend syntax element as follows:									*/
/* (setq friend (apply makeQuotedList (setLastCdr (list lambda:) friendBody)))  */
/* (setq friend (list morph: friend))											*/

*tmp = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),1,TSYMBOL("lambda"));
ExitOnError(*tmp);
*friend = FSmartbase_Eval(gCP,gTP,TGVALUE("setLastCdr"),2,*tmp,*friendBody);
ExitOnError(*friend);
*friend = FSmartbase_Eval(gCP,gTP,TGVALUE("apply"),2,TGVALUE("makeQuotedList"),*friend);
ExitOnError(*friend);
*friend = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("morph"),*friend);
ExitOnError(*friend);

/* Create the tail element as follows:							*/
/* (setq tail (list 'makeLambda ''Cv parentPv)))					*/
/* (setq tail (list eval: (list compile: friend tail true)))	*/

if (gTP->FCompile_GenerateDebugInfo == TRUE)
	{
	/* We are generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),7,TSYMBOL("makeLambda"),TQSYMBOL("Cv"),*parentPv,TQSYMBOL("Sv"),*parentSv,TQSYMBOL("In"),gTP->FCompile_DebugInterfaces);
	ExitOnError(*tail);
	}
else
	{
	/* We are NOT generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),5,TSYMBOL("makeLambda"),TQSYMBOL("Cv"),*parentPv,TQSYMBOL("Sv"),*parentSv);
	ExitOnError(*tail);
	}
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),4,TSYMBOL("compile"),*friend,*tail,gCP->Tval_TRUE);
ExitOnError(*tail);
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("eval"),*tail);
ExitOnError(*tail);
*friendLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*tail);
ExitOnError(*friendLambda);

/* Set the friend Lambda into the parent Lambda immediately as follows: */

*result = TStructure_SetIV1(gCP,gTP,*parentPv,*friendName,*friendLambda);
ExitOnError(*result);

/* ******************************************************************** */
/* Generate the code which returns the friendLambda as the result value.	*/
/* ******************************************************************** */

/* Generate (set parentPv childName childLambda)	*/
*friendName = FSmartbase_Eval(gCP,gTP,TGVALUE("makeQuotedSymbol"),1,*friendName);
ExitOnError(*friendName);
*parentPv = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentName,TQSYMBOL("Pv"));
ExitOnError(*parentPv);
*result = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentPv,*friendName);
ExitOnError(*result);
*result = FCompile_Call(gCP,gTP,compilerState,result->u.Pair);
ExitOnError(*result);

FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
    
/*--------------------------------------------------------------------------------------- */
/*
FSpecialForms3_Macro

This special form allows macro expansions to generate in-line code during compilation.
Assume the following macro definition:

(defmacro plus(x y) (repalceMacro x y '(+ %1 %2)))

 Invoking the plus macro will call this special form and generate code as follows:

(plus (/ x 23) (* w x)) ==> (+ ( x 23) (*w x))

*/

TVAL    FSpecialForms3_Macro(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* argPair, TVAL macro)
{
StartFrame
DeclareTVAL(tmp);
DeclareOBJ(TPair,headPair);
DeclareTVAL(result);
EndFrame

macro = macro; // NOOP to hide unused parameter warning message
/* Use the morph function to invoke the macro special form.  */
/* Note: This will produce a new form returned by the macro. */

result->Tag = TYPAIR;
result->u.Pair = argPair;
gTP->FCompile_OptimizeSW = TRUE;
*tmp = FLisp_MorphList(gCP, gTP,*result,gCP->Tval_VOID,gCP->Tval_FALSE);
gTP->FCompile_OptimizeSW = TRUE;
if (tmp->Tag == TYERROR) {FrameExit(*tmp);}

/* Use the recognize function to compile the new result form.  */
/* Note: This will compile the new form returned by the macro. */

headPair = TPair_New(gCP, gTP);
headPair->itsCar = *tmp;
headPair->itsCdr = gCP->Tval_VOID;
*result = FCompile_Recognize(gCP, gTP, compilerState, headPair);
FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_RefMacro
FMacro_RefMacro

The refmacro CMacro allows the invocation of compound macro references in Lisp.

((refmacro name macro) ...argc...)

If the macro is a simple globally-named macro, the refmacro CMacro will have the same
effect as invoking the macro name alone. 

If the macro is a member Lambda requiring a compound macro name reference, then 
the refmacro CMacro is required to invoke the macro.

#endif

TVAL    FSpecialForms3_RefMacro(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(tmp);
DeclareOBJ(TPair,headPair);
DeclareTVAL(result);
EndFrame

compilerState = compilerState; // NOOP to hide unused parameter warning message
headPair = TPair_New(gCP, gTP);
headPair->itsCar.u.Symbol = gCP->FCompile_refSym;
headPair->itsCar.Tag = TYSYMBOL;
headPair->itsCdr = argPair->itsCdr;
tmp->u.Pair = headPair;
tmp->Tag = TYPAIR;

*result = FCompile_Compile(gCP,gTP,1,&*tmp);
ExitOnError(*result);
*result = _VmEvaluate(asProcedure(&*result),(NUM)0,&*result);
FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */
/*
FSpecialForms3_Defstruct

This procedure provides support for code generation for the defmethod special form. 

    (defstruct type: msg(arg...) vars: (var...) pvars: (var...) exp ...)
                 "             "             "
                 "             "             "
                 V             V             V
    (defineStructure type: msg: (lambda (arg...) vars: (var...) pvars: (var...) exp ...))

*/

TVAL    FSpecialForms3_Defstruct(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(result);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;
*tmp = FMacro_Defstruct(gCP,gTP,1,&argPair->itsCdr);
*result = FCompile_Call(gCP,gTP,compilerState,tmp->u.Pair);
FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
/*
FSpecialForms3_Defclass

This procedure provides support for code generation for the defmethod special form. 

	(defclass library: parent: name(arg...) vars: (var...) pvars: (var...) exp ...)

	(defclass parent: name(arg...) vars: (var...) pvars: (var...) exp ...)

	(defclass name(arg...)  vars:  (var...)  pvars:  (var...)  exp...)
                 "             "             "
                 "             "             "
                 V             V             V
	(define	class:	(name  arg...)  vars:  (var...)  pvars:  (var...)  exp...)


This function was originally encoded in Lisp as follows:

(defmacro defclass(parentName childName ...)
  vars:(result tail friend parentPv childBody)
  (setq childBody (argFetch 2))
  (setq childName (makeQuotedSymbol childName))
  (if (isSymbol parentName) (setq parentName (symbol parentName)))
  (setq parentPv (list 'ref parentName ''Pv))
  (setq friend (apply makeQuotedList (setLastCdr (list lambda:) childBody)))
  (setq friend (list morph: friend))
  (setq tail (list 'makeLambda ''Cv parentPv)))
  (setq tail (list eval: (list compile: friend tail true)))
  (setq result (list set: parentPv childName tail)))
  (setq result (list 'ref result childName))
 result)

*/

TVAL    FSpecialForms3_Defclass(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* argPair)
{
BOLE	parentIsClassSW = TRUE;
StartFrame
DeclareTVAL(parentName);
DeclareTVAL(parentLambda);
DeclareTVAL(parentPv);
DeclareTVAL(parentSv);
DeclareTVAL(parentIn);
DeclareTVAL(className);
DeclareTVAL(classLambda);
DeclareTVAL(classBody);
DeclareTVAL(faces);
DeclareTVAL(methods);
DeclareTVAL(macros);
DeclareTVAL(tmp);
DeclareTVAL(tail);
DeclareTVAL(friend);
DeclareTVAL(result);
DeclareOBJArray(TPair,pairs,20);
EndFrame

/* Is this a global or "root" class declaration? */

if ((argPair->itsCdr.Tag == TYPAIR) &&
	(argPair->itsCdr.u.Pair->itsCar.Tag == TYSYMBOL) &&
	(argPair->itsCdr.u.Pair->itsCdr.Tag == TYPAIR) &&
	((argPair->itsCdr.u.Pair->itsCdr.u.Pair->itsCar.Tag == TYPAIR) || 
	 ((argPair->itsCdr.u.Pair->itsCdr.u.Pair->itsCar.Tag == TYSYMBOL) && 
	  (strcmp(SymbolArray(argPair->itsCdr.u.Pair->itsCdr.u.Pair->itsCar),"()") == 0)))
	)
	{
	if (gTP->FCompile_LambdaNest != NIL)
		{
		*result = TERROR("!defclass: embedded class Lambdas not supported!");
		FrameExit(*result);
		}

	*className = argPair->itsCdr.u.Pair->itsCar;
	*tmp = FMacro_Defun(gCP,gTP,1,&argPair->itsCdr);
	*result = FSpecialForms1_define(gCP,gTP,compilerState,tmp->u.Pair);
	ExitOnError(*result);
	*classLambda = className->u.Symbol->itsGlobalValue;
	if (classLambda->Tag == TYLAMBDA)
		{
		if (classLambda->u.Lambda->Interfaces == NIL) 
			{
			classLambda->u.Lambda->Interfaces = TStructure_New(gCP,gTP);
			}
		*faces = TOBJ(classLambda->u.Lambda->Interfaces);
		_In(compilerState) = classLambda->u.Lambda->Interfaces;
		*tmp = TSYMBOL("class");
		*tmp = TStructure_SetIV1(gCP,gTP,*faces,*tmp,*className);
		*tmp = TSYMBOL("parent");
		*tmp = TStructure_SetIV1(gCP,gTP,*faces,*tmp,gCP->Tval_FALSE);
		methods->u.Structure = TStructure_New(gCP,gTP);
		methods->Tag = TYSTRUCTURE;
		*tmp = TSYMBOL("methods");
		*tmp = TStructure_SetIV1(gCP,gTP,*faces,*tmp,*methods);
		macros->u.Structure = TStructure_New(gCP,gTP);
		macros->Tag = TYSTRUCTURE;
		*tmp = TSYMBOL("macros");
		*tmp = TStructure_SetIV1(gCP,gTP,*faces,*tmp,*macros);
		}
	FrameExit(*result);
	}

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;

/*  Initialize */

_FMacro_PairsToArray(argPair, pairs, 4);
if (pairs[3] == NULL)  goto BadCleanUp;
*parentName = pairs[1]->itsCar;
*className = pairs[2]->itsCar;
classBody->u.Pair = pairs[3]; classBody->Tag = TYPAIR;

/* Force all quoted symbol parent names to be just symbol names */
 
if ((parentName->Tag == TYSYMBOL) || (parentName->Tag == TYQUOTEDSYMBOL))
	{
	parentName->Tag = TYSYMBOL;
	parentName->QuoteCnt = 0;
	}

/* Force all symbol class names to be quoted symbol class names */
 
if ((className->Tag == TYSYMBOL) || (className->Tag == TYQUOTEDSYMBOL))
	{
	className->Tag = TYQUOTEDSYMBOL;
	className->QuoteCnt = 1;
	}
else
	{
	FrameExit(TERROR("!defclass: class name must be a symbol!"));
	}

/* Locate the parent Lambda  */
*parentLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*parentName);
ExitOnError(*parentLambda);
if (parentLambda->Tag != TYLAMBDA)
	{
	*result = TERROR("!defclass: parent must be an Lambda!");
	FrameExit(*result);
	}

/* Locate the parent Pv and Sv elements.  */

*parentSv = FSmartbase_Eval(gCP,gTP,TGVALUE("copy"),1,TOBJ(parentLambda->u.Lambda->ClassVariables));
ExitOnError(*parentSv);
*parentPv = TOBJ(parentLambda->u.Lambda->PersistantVariables);
if (parentPv->Tag != TYSTRUCTURE)
	{
	parentPv->u.Structure = TStructure_New(gCP,gTP);
	parentPv->Tag = TYSTRUCTURE;
	parentLambda->u.Lambda->PersistantVariables = parentPv->u.Structure;
	}

/* Create the friend element as follows:										*/
/* (setq friend (apply makeQuotedList (setLastCdr (list lambda:) classBody)))   */
/* (setq friend (list morph: friend))											*/

*tmp = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),1,TSYMBOL("lambda"));
ExitOnError(*tmp);
*friend = FSmartbase_Eval(gCP,gTP,TGVALUE("setLastCdr"),2,*tmp,*classBody);
ExitOnError(*friend);
*friend = FSmartbase_Eval(gCP,gTP,TGVALUE("apply"),2,TGVALUE("makeQuotedList"),*friend);
ExitOnError(*friend);
*friend = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("morph"),*friend);
ExitOnError(*friend);

/* Create the tail element as follows:							*/
/* (setq tail (list 'makeLambda ''Cv parentPv)))					*/

if (gTP->FCompile_GenerateDebugInfo == TRUE)
	{
	/* We are generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),7,TSYMBOL("makeLambda"),TQSYMBOL("Cv"),*parentPv,TQSYMBOL("Sv"),*parentSv,TQSYMBOL("In"),gTP->FCompile_DebugInterfaces);
	//*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),5,TSYMBOL("makeLambda"),TQSYMBOL("Cv"),*parentPv,TQSYMBOL("In"),gTP->FCompile_DebugInterfaces);
	ExitOnError(*tail);
	}
else
	{
	/* We are NOT generating debugging information during this compile. */
	*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),5,TSYMBOL("makeLambda"),TQSYMBOL("Cv"),*parentPv,TQSYMBOL("Sv"),*parentSv);
	//*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("makeLambda"),TQSYMBOL("Cv"),*parentPv);
	ExitOnError(*tail);
	}

/* Retrieve the parent class and create the child class	as follows:  */
/* (setq childClass (list eval: (list compile: friend tail true)))	 */

*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),4,TSYMBOL("compile"),*friend,*tail,gCP->Tval_TRUE);
ExitOnError(*tail);
*tail = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),2,TSYMBOL("eval"),*tail);
ExitOnError(*tail);
*classLambda = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*tail);
ExitOnError(*classLambda);


/* Set the child class into the parent class immediately as follows: */

*result = TStructure_SetIV1(gCP,gTP,*parentPv,*className,*classLambda);
ExitOnError(*result);

/* The child class inherits its methods and macros from the parent class. */

if ((parentLambda->Tag == TYLAMBDA) && (classLambda->Tag == TYLAMBDA))
	{
	*parentIn = TOBJ(parentLambda->u.Lambda->Interfaces);
	if (parentIn->Tag != TYSTRUCTURE) 
		{
		parentIsClassSW = FALSE;
		}
	if (classLambda->u.Lambda->Interfaces == NIL) 
		{
		classLambda->u.Lambda->Interfaces = TStructure_New(gCP,gTP);
		}
	*faces = TOBJ(classLambda->u.Lambda->Interfaces);
	_In(compilerState) = classLambda->u.Lambda->Interfaces;
	className->Tag = TYSYMBOL;
	className->QuoteCnt = 0;
	*tmp = TStructure_SetIV1(gCP,gTP,*faces,TSYMBOL("class"),*className);
	ExitOnError(*tmp);
	if (parentIsClassSW == TRUE)
		{
		*methods = TStructure_GetIV1(gCP,gTP,*parentIn,TSYMBOL("methods"));
		ExitOnError(*methods);
		if (methods->Tag != TYSTRUCTURE) 
			{
			parentIsClassSW = FALSE;
			}
		else
			{
			*methods = FSmartbase_Eval(gCP,gTP,TGVALUE("copy"),1,*methods);
			ExitOnError(*methods);
			}
		}
	if (parentIsClassSW == TRUE)
		{
		*macros = TStructure_GetIV1(gCP,gTP,*parentIn,TSYMBOL("macros"));
		ExitOnError(*macros);
		if (macros->Tag != TYSTRUCTURE) 
			{
			parentIsClassSW = FALSE;
			}
		else
			{
			*macros = FSmartbase_Eval(gCP,gTP,TGVALUE("copy"),1,*macros);
			ExitOnError(*macros);
			}
		}
	if (parentIsClassSW != TRUE)
		{
		methods->u.Structure = TStructure_New(gCP,gTP);
		methods->Tag = TYSTRUCTURE;
		macros->u.Structure = TStructure_New(gCP,gTP);
		macros->Tag = TYSTRUCTURE;
		}
	if (parentIsClassSW == TRUE)
		{
		*tmp = TStructure_SetIV1(gCP,gTP,*faces,TSYMBOL("parent"),*parentLambda);
		ExitOnError(*tmp);
		}
	else
		{
		*tmp = TStructure_SetIV1(gCP,gTP,*faces,TSYMBOL("parent"),gCP->Tval_FALSE);
		ExitOnError(*tmp);
		}
	*tmp = TStructure_SetIV1(gCP,gTP,*faces,TSYMBOL("methods"),*methods);
	ExitOnError(*tmp);
	*tmp = TStructure_SetIV1(gCP,gTP,*faces,TSYMBOL("macros"),*macros);
	ExitOnError(*tmp);
	}

/* ************************************************************************ */
/* Generate the code which returns the classLambda as the result value.		*/
/* ************************************************************************ */

/* Generate (set parentPv className childLambda)	*/
*className = FSmartbase_Eval(gCP,gTP,TGVALUE("makeQuotedSymbol"),1,*className);
ExitOnError(*className);
*parentPv = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentName,TQSYMBOL("Pv"));
ExitOnError(*parentPv);
*result = FSmartbase_Eval(gCP,gTP,TGVALUE("list"),3,TSYMBOL("ref"),*parentPv,*className);
ExitOnError(*result);
*result = FCompile_Call(gCP,gTP,compilerState,result->u.Pair);
ExitOnError(*result);

FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
    
/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_gotoCC

The gotoEQ  Special Form allows SmartLisp Procedures to conditionally branch to labels 
within a Procedure (see the gotoEQ and other special forms).  

For example
    
(defun  foo (x)
    (gotoEQ x 2 TWO:)
    ONE::
    (writeln " ONE " X)
    goto EXIT:
    TWO::
    (writeln " TWO" )
    EXIT::
    (writeln "foo EXIT" )
)

During the first pass of the compiler this procedure generates conditional VMJMPCC instructions
when the goto special form is encountered. It also has responsibility for keeping the 
_GotosP(compilerState) vector up to date with the locations of all of these instructions. 
Please see FCompile_ResolveGotos for a complete description of how this information is utilized
to resolve gotos in a SmartLisp program.

#endif

TVAL    FSpecialForms3_gotoCC(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM opcode)
{
_FCompile_StartFrame 
DeclareTVAL(loc);
DeclareTVAL(ret);
DeclareTVAL(sym);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame

/*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));

/*  Advance the curPair to point to the source of this gotoEQ special form. */
/*  Recognize the source of this return special form and setup for append pcode. */

if (curPair->itsCdr.Tag != TYPAIR)
    {
 	*ret = TERROR("!gotoCC: Missing or invalid expression for condition!");
	_FCompile_FrameExit(*ret);
    }
curPair = _FCompile_CdrPair(curPair);
*ret = FCompile_SetupArg(gCP, gTP, compilerState, curPair->itsCar, &prmv[2], &prmv[5]);
_FCompile_FrameChk(*ret);

/*  Advance the curPair to point to the argument of this gotoEQ special form. */
/*  Recognize the argument of this return special form and setup for append pcode. */

if (curPair->itsCdr.Tag != TYPAIR) 
	{
 	*ret = TERROR("!gotoCC: Missing or invalid expression for condition!");
	_FCompile_FrameExit(*ret);
	}

curPair = _FCompile_CdrPair(curPair);
*ret = FCompile_SetupArg(gCP, gTP, compilerState, curPair->itsCar, &prmv[3], &prmv[6]);
_FCompile_FrameChk(*ret);

/*  Advance the curPair to point to the label of this gotoEQ */

curPair = _FCompile_CdrPair(curPair);

if (curPair != NULL && asTag(&curPair->itsCar) == TYQUOTEDSYMBOL)
    {
    /*  Complete formatting of this conditional jump instruction, leaving the jump target */
    /*  initialized to -1, it will be patched up when we resolve gotos at the end of this compiler */
    /*  pass. */
    
    _FCompile_MakeINT(prmv[1], opcode);
    _FCompile_MakeINT(prmv[4], AMINTEGER);
    _FCompile_MakeIMM(prmv[7], -1);
    *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState,8, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    /*  We update the _GotosP(compilerState) vector to contain the location of every unconditional */
    /*  jump generated as a result of a goto in this procedure. */
    
    loc->Tag = TYNUM;
    loc->u.Int = _PcP(compilerState)->itsCurItemIndex - 1;
    sym->Tag = TYSYMBOL;
    sym->u.Symbol = curPair->itsCar.u.Symbol;

    *ret = TStructure_AddNewValue(gCP,gTP,_Gotos(compilerState), *sym, *loc);

    _FCompile_FrameChk(*ret);
    
    }
else
    {
    /*  We encountered something other than a quoted symbol for a label and that is not valid. */   
  	*ret = TERROR("!gotoCC: Missing or invalid goto Label!");
	_FCompile_FrameExit(*ret);
    }

_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_return

The  return  Special Form allows SmartLisp Procedures to return values from a Procedure 
(see the return special form).  

For example
    
(defun  foo(x) (if (= x 2) (return 3)) x)
    

#endif

TVAL    FSpecialForms3_return(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame 
DeclareTVAL(ret);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame

/*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode, */
/*  and setup the parameter array for the VMRETURN virtual machine instruction. */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], VMRETURN);

/*  Advance the curPair to point to the target of this return special form. */
/*  Recognize the target of this return special form and setup for append pcode. */

if (curPair->itsCdr.Tag != TYPAIR)
    {
    /*  We encountered something other than a quoted symbol for a label and that is not valid. */   
  	*ret = TERROR("!return: Missing Argument!");
	_FCompile_FrameExit(*ret);
    }
curPair = _FCompile_CdrPair(curPair);
*ret = FCompile_SetupArg(gCP, gTP, compilerState, curPair->itsCar, &prmv[2], &prmv[5]);
_FCompile_FrameChk(*ret);

/*  Generate the code to set the return value. */

_FCompile_MakeINT(prmv[3], AMVOID);      
_FCompile_MakeINT(prmv[4], AMVOID);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState,6, &prmv[0]);
_FCompile_FrameChk(*ret);

_FCompile_FrameExit(*ret);


}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_goto

The goto  Special Form allows SmartLisp Procedures to branch to labels within a Procedure 
(see the lambda special form).  

For example
    
(defun  FOO1 (X)
    goto TWO: 
    ONE:
    (writeln " ONE " X)
    goto EXIT:
    TWO:
    (writeln " TWO" )
    goto    ONE:
    EXIT:
    (writeln "FOO1 EXIT" )
)

which might generate the code :

 0000: jump    aminteg amvoid  amvoid   14              
 0002: push    amcvoff amsboff amvoid    ONE             X               
 0008: call    aminteg amgvoff amfboff  2                writeln          __T0            
 0012: jump    aminteg amvoid  amvoid   25              
 0014: push    amcvoff amvoid  amvoid   TWO            
 0019: call    aminteg amgvoff amfboff  1                writeln          __T0            
 0023: jump    aminteg amvoid  amvoid   2               
 0025: push    amcvoff amvoid  amvoid   FOO1 EXIT       
 0030: call    aminteg amgvoff amfboff  1                writeln          __T0            
 0034: return  amfboff amvoid  amvoid   __T0            

During the first pass of the compiler this procedure generates unconditional VMJUMP instructions
when the goto special form is encountered. It also has responsibility for keeping the 
_GotosP(compilerState) vector up to date with the locations of all of these instructions. 
Please see FCompile_ResolveGotos for a complete description of how this information is utilized
to resolve gotos in a SmartLisp program.

#endif

TVAL    FSpecialForms3_goto(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame 
DeclareTVAL(loc);
DeclareTVAL(ret);
DeclareTVAL(sym);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame

/*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));

/*  Advance the curPair to point to the target of this goto */

if (curPair->itsCdr.Tag != TYPAIR)
	{
 	*ret = TERROR("!goto: Missing Label Argument!");
	_FCompile_FrameExit(*ret);
	}

curPair = _FCompile_CdrPair(curPair);

if (curPair && asTag(&curPair->itsCar) == TYQUOTEDSYMBOL)
    {
    /*  Complete formatting of this unconditional jump instruction, leaving the jump target */
    /*  initialized to -1, it will be patched up when we resolve gotos at the end of this compiler */
    /*  pass. */
    
    _FCompile_MakeINT(prmv[1], VMJUMP);
    _FCompile_MakeINT(prmv[2], AMINTEGER);
    _FCompile_MakeIMM(prmv[5], -1);
    _FCompile_MakeINT(prmv[3], AMVOID);      
    _FCompile_MakeINT(prmv[4], AMVOID);
    *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState,6, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    /*  We update the _GotosP(compilerState) vector to contain the location of every unconditional */
    /*  jump generated as a result of a goto in this procedure. */
    
    loc->Tag = TYNUM;
    loc->u.Int = _PcP(compilerState)->itsCurItemIndex - 1;
    sym->Tag = TYSYMBOL;
    sym->u.Symbol = curPair->itsCar.u.Symbol;

    *ret = FSpecialForms3_addGotoRef(gCP, gTP, _Gotos(compilerState), *sym, *loc);

    _FCompile_FrameChk(*ret);
    
    }
else
if (curPair != NULL && asTag(&curPair->itsCar) == TYSYMBOL)
    {
    /*  Complete formatting of this unconditional register jump instruction. The jump target */
    /*  must be a register variable of type JumpPointer. */

    _FCompile_MakeINT(prmv[1], vmregJump);
    _FCompile_MakeINT(prmv[3], AMVOID);      
    _FCompile_MakeINT(prmv[4], AMVOID);

    *ret = FOptimize2_VmRegisterArg(gCP,gTP,compilerState,curPair,gCP->TLambda_goto,&prmv[2],&prmv[5]);
    if (ret->Tag == TYERROR) goto BadArgument;
    
	*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState,6, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    }
else
    /*  We encountered something other than a quoted symbol for a label and that is not valid. */
	{
	BadArgument:
 	*ret = TERROR("!goto: Invalid Label Argument!");
	_FCompile_FrameExit(*ret);
	}

_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_label

When we encounter a label in a SmartLisp program we do not generate any code but simply 
add the location of the label in the _LabelsP(compilerState) vector. 

Please see FCompile_ResolveGotos for a complete description of how this information is utilized
to resolve gotos in a SmartLisp program.

We maintain the _LabelsP structure as a structure which is sorted on unique symbol keys, in
this way we provide support for a search mechanism which will speed up the resolution of symbol
label lookup.

#endif

TVAL    FSpecialForms3_label(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
NUM                 oldMax;
_FCompile_StartFrame
DeclareTVAL(loc);
DeclareTVAL(ret);
DeclareOBJ(TStructure,tmpEnv);
_FCompile_EndFrame

/*  Advance the curPair to point to the target of this label. During the lex phase of the compiler */
/*  we insert a TYLABELSYMBOL TVAL before any TYQUOTEDSYMBOL's which appear to be program labels. */
/*  Thus we will step over this element to get to the TYQUOTEDSYMBOL label itself. */

curPair = _FCompile_CdrPair(curPair);

if(curPair != NULL && asTag(&curPair->itsCar) == TYQUOTEDSYMBOL)
    {
    /*  This looks like a valid label. We will add it to the _LabelsP(compilerState) vector. */
    
    loc->Tag = TYNUM;
    loc->u.Int = _PcP(compilerState)->itsCurItemIndex;
    tmpEnv = _LabelsP(compilerState);
    oldMax = tmpEnv->itsMaxItemIndex;
    *ret = TStructure_MakeUniqueKey(gCP,gTP,tmpEnv,asObject(&curPair->itsCar),*loc);
    _FCompile_FrameChk(*ret);
    
    if(tmpEnv->itsMaxItemIndex == oldMax)
        {
        /*  We have encountered two labels at different locations for the same symbol */
        /*  and this is an error! */
 		*ret = TERROR("!label: Duplicate label encountered!");
		_FCompile_FrameExit(*ret);
        }
    }
else
    {
    /*  We have encountered a syntax error. */
 	*ret = TERROR("!label: Illegal Label Syntax!");
	_FCompile_FrameExit(*ret);
    }

_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_PlusPlus

(++ x) ==> (setq x (+ x 1))

The  ++  Special Form increments the value of the variable {var}.  The ++ special form is
the equivalent of issuing a setq special form as shown above. 
    
#endif

TVAL    FSpecialForms3_PlusPlus(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(result);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;
*tmp = FMacro_PlusPlus(gCP,gTP,1,&argPair->itsCdr);
*result = FSpecialForms3_setq(gCP,gTP,compilerState,tmp->u.Pair);
FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_MinusMinus

(-- x) ==> (setq x (- x 1))

The  --  Special Form decrements the value of the variable {var}.  The -- special form is
the equivalent of issuing a setq special form as shown above. 
    
#endif

TVAL    FSpecialForms3_MinusMinus(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(result);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;
*tmp = FMacro_MinusMinus(gCP,gTP,1,&argPair->itsCdr);
*result = FSpecialForms3_setq(gCP,gTP,compilerState,tmp->u.Pair);
FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_TimesEquals

(*= x y) ==> (setq x (* x y))

The  *=  Special Form multiplies the value of the first variable by the second variable.  
The *= special form is the equivalent of issuing a setq special form as shown above. 
    
#endif

TVAL    FSpecialForms3_TimesEquals(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(result);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;
*tmp = FMacro_TimesEquals(gCP,gTP,1,&argPair->itsCdr);
*result = FSpecialForms3_setq(gCP,gTP,compilerState,tmp->u.Pair);
FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_PlusEquals

(+= x y) ==> (setq x (+ x y))

The  +=  Special Form adds the value of the first variable by the second variable.  
The += special form is the equivalent of issuing a setq special form as shown above. 
    
#endif

TVAL    FSpecialForms3_PlusEquals(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(result);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;
*tmp = FMacro_PlusEquals(gCP,gTP,1,&argPair->itsCdr);
*result = FSpecialForms3_setq(gCP,gTP,compilerState,tmp->u.Pair);
FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_MinusEquals

(-= x y) ==> (setq x (- x y))

The  -=  Special Form subtracts the value of the first variable by the second variable.  
The -= special form is the equivalent of issuing a setq special form as shown above. 
    
#endif

TVAL    FSpecialForms3_MinusEquals(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(result);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;
*tmp = FMacro_MinusEquals(gCP,gTP,1,&argPair->itsCdr);
*result = FSpecialForms3_setq(gCP,gTP,compilerState,tmp->u.Pair);
FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_DivEquals

(-= x y) ==> (setq x (- x y))

The  /=  Special Form divides the value of the first variable by the second variable.  
The /= special form is the equivalent of issuing a setq special form as shown above. 
    
#endif

TVAL    FSpecialForms3_DivEquals(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(result);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;
*tmp = FMacro_DivEquals(gCP,gTP,1,&argPair->itsCdr);
*result = FSpecialForms3_setq(gCP,gTP,compilerState,tmp->u.Pair);
FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
/*
FSpecialForms3_Defvm

This procedure provides support for code generation for the defvm special form. The defvm
special form is a flavor of define in which a new lambda object will be generated. Please see the 
FSpecialForms1_lambda procedure for a description of the nature and control of this processing.

(defvm   	name(arg...)  vars:  (var...)  pvars:  (var...)  exp...)
                 "             "             "
                 "             "             "
                 V             V             V
(define	vm:	(name  arg...)  vars:  (var...)  pvars:  (var...)  exp...)

*/

TVAL    FSpecialForms3_Defvm(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState, TPair* argPair)
{
StartFrame
DeclareTVAL(tmp);
DeclareTVAL(result);
EndFrame

if (argPair->itsCdr.Tag != TYPAIR) goto BadCleanUp;
*tmp = FMacro_Defvm(gCP,gTP,1,&argPair->itsCdr);
*result = FSpecialForms1_define(gCP,gTP,compilerState,tmp->u.Pair);
FrameExit(*result);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}
    
/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_setq

(setq var exp)

The  setq  Special Form changes the value of the variable {var}.  If the {var} is a symbol, 
the setq special form sets {var} to the value of the expression {exp}, and returns the value. 
If the {var} is a spreadsheet cell or rowcol ref, the setq special form sets {var} to the value 
of the expression {exp}, and returns the value.  If the {var} is a list, the setq special form 
uses the {list} as an indexed reference into a list, vector, structure, string, buffer, 
or spreadsheet cell array.  The indexed variable is set to the value of the expression {exp}, 
and the altered list, vector, etc. is returned.  An optional keyword :=  may precede the {exp} 
for readability.  

For example

    (setq  $A$4  :=  (*  3  4))    =>  12
    (setq  A4  :=  (*  3  4))  =>  12
    (define  Y  (+  3  4))  =>  7
    Y   =>  7
    (setq  Y  (*  3  4))    =>  12
    Y   =>  12
    (setq  Y  :=  (*  3  4))    =>  12
    Y   =>  12
    (setq  (ref  #(1  2  3)  2)  (*  3  4)) =>  #(1  2  12)
    (setq  (ref  `(1  2  3)  2)  :=  (*  3  4)) =>  (1  2  12)
    (setq  (ref  "Hello"  0)  #\h)  =>  "hello"
    (setq  (ref  #{A: 1  B: 2  C: 3}  `A)  (*  3  4))   =>  #{A: 12  B: 2  C: 3}
    (setq  (ref  #{A: 1  B: 2}  0  1)  (*  3  4))   =>  #{A: 12  B: 2}
    (setq  x.salary  (*  3  4)) =>  12
    
#endif

TVAL    FSpecialForms3_setq(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,movPair);
DeclareOBJ(TPair,savPair);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVAL(sym);
DeclareTVAL(tmp);
DeclareOBJArray(TPair,pairs,8);
DeclareTVALArray(prmv,11);
_FCompile_EndFrame

/* Abort if rest of expression is NULL */
if (curPair == NULL)
    {
 	*ret = TERROR("!setq: Missing Arguments!");
	_FCompile_FrameExit(*ret);
    }
/*  Set up an array of pointers into the input list for easy indexed access */

_FMacro_PairsToArray(curPair, pairs, sizeof(pairs)/sizeof(pairs[0]));

/*  Initialization */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], VMMOVE);

/*  Remember where the final result should go, if anywhere. */

*final = _Result(compilerState);

if (pairs[1] == NULL)
    {
 	*ret = TERROR("!setq: Missing variable argument!");
	_FCompile_FrameExit(*ret);
    }

if(asTag(&pairs[1]->itsCar) == TYSYMBOL)
    {
    /*  The following forms are recognized in this section: */
    /*  (setq  name [i1 i2 i3] exp) */
    
    /*  Lookup this symbol in the appropriate Structure. Note that all symbols will have been */
    /*  bound globally during the lex phase of the compile so FCompile_LookUp will not fail. */
    
    aSymbol = asSymbol(&pairs[1]->itsCar);
    *ret = FCompile_LookUp(gCP, gTP, compilerState, aSymbol);
    *sym = *ret;
                
    if(pairs[3] == NULL)
        {
        /*  This is a simple non-indexed symbol assignment. */
        /*      0    1  2 */
        /*  (setq  name exp) */
        /*  Generate the  code for this initialization */
                
        /*  Set the result to the symbol of interest */
        
        _Result(compilerState) = *sym;
        if (pairs[2] == NULL)
			{
 			*ret = TERROR("!setq: Missing expression argument!");
			_FCompile_FrameExit(*ret);
			}
        
        *ret = FCompile_Recognize(gCP, gTP, compilerState, pairs[2]);
        _FCompile_FrameChk(*ret);
        _LastResult(compilerState) = *ret;

		if ((_Result(compilerState).Modifier != _LastResult(compilerState).Modifier) || (_Result(compilerState).Offset != _LastResult(compilerState).Offset)) 
			{
			/*  If we did not assign the target variable, we may have to generate code for it */
    
			*ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *sym);
			*ret = _LastResult(compilerState) = *sym;
			}

        _Result(compilerState) = *final;

		if ((_Result(compilerState).Tag != TYVOID) &&
		    ((_Result(compilerState).Modifier == AMREGISTER) || (sym->Modifier != AMREGISTER)) 
			)
			{
			/*  If an explicit final result was requested, we may have to generate code for it */
    
			*ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
			*ret = _LastResult(compilerState) = *final;
			}

		_FCompile_FrameExit(*ret);
        }
    else
    if(pairs[4] == NULL)
        {
        /*  This is an indexed symbol assignment with two or three indices. */
        /*      0    1    2   3  */
        /*  (setq  name   i1  exp) */
        /*  This will translate to (set name i1 exp) */
                
        /*  Set the result to the symbol of interest */
        
        *ret = FOptimize1_set(gCP, gTP, compilerState, pairs[1], VMSET);
        _FCompile_FrameChk(*ret);
        _LastResult(compilerState) = *ret;
        }
    else
    if (pairs[5] == NULL || pairs[6] == NULL)
        {
        /*  This is an indexed symbol assignment with two or three indices. */
        /*      0    1      2   3 */
        /*  (setq  name     i1  exp) */
        /*      0    1      2   3   4 */
        /*  (setq  name     i1  i2  exp) */
        /*      0    1      2   3   4   5 */
        /*  (setq  name     i1  i2  i3  exp) */
        /*  This will translate to (set name i1 i2 exp) */
        
        /*  Generate the idx1 code for this initialization */
        savPair = TPair_New(gCP,gTP);
        tmpPair = TPair_New(gCP,gTP);
        savPair->itsCar = gCP->Tval_VOID;
        tmpPair->itsCar = gCP->Tval_VOID;
        savPair->itsCdr = gCP->Tval_VOID;
        tmpPair->itsCdr = gCP->Tval_VOID;
        asTag(&savPair->itsCar) = TYPAIR;
        asObject(&savPair->itsCar) = (TObject*)tmpPair;
        asObject(&tmpPair->itsCar) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"set");
        asTag(&tmpPair->itsCar) = TYSYMBOL;
        asObject(&tmpPair->itsCdr) = (TObject*)pairs[1];
        asTag(&tmpPair->itsCdr) = TYPAIR;
        
        /*  VOID up the return location */

        _Result(compilerState) = gCP->Tval_VOID;
        *ret = FCompile_Recognize(gCP, gTP, compilerState, savPair);
        _FCompile_FrameChk(*ret);
                    
        _LastResult(compilerState) = *ret;
        }
    else
		{
 		*ret = TERROR("!setq: Received too many arguments!");
		_FCompile_FrameExit(*ret);
		}
    
    /*  Setup to return the location of the result */

    _FCompile_MakeTVL(*ret, _LastResult(compilerState));
    }
else
if(asTag(&pairs[1]->itsCar) == TYPAIR )
    {
    /*  The following forms are recognized in this section: */
    /*      0    1      2   3   4 */
    /*  (setq   ()      i1  i2  exp) */
    /*      0    1      2   3   4   5 */
    /*  (setq   ()      i1  i2  i3  exp) */
    /*      0   1        2 */
    /*  (setq (ref ...)  exp) */
    
    tmpPair = asPair(&pairs[1]->itsCar);
    
    /*  Do some syntax checking to insure that this looks like a valid syntax for a ref call. */
    
    if(asTag(&tmpPair->itsCar) == TYSYMBOL && asSymbol(&tmpPair->itsCar) == gCP->FMacro_refSym 
    && !isNullTval(&tmpPair->itsCdr) && !isNullTval(&pairs[1]->itsCdr))
        {
        
        /*  (setq (ref sym    ...)  exp ) ==> (setq sym ... exp) */
        /*  (setq (ref ()    ...)  exp  ) ==> (setq ... ... exp) */
        
        /*  Initially we will handle all cases of embedded ref calls by  */
        /*  eliminating the first ref and replacing it with a call to set followed */
        /*  by the rest of the expressions properly tacked together, and then sending this */
        /*  to the recognizer. */
                
        /*  For example (setq (ref (ref a 'b) 'c ) 100 ) ==> (set (ref a 'b) 'c 100) */
        
        /*  For example (setq (ref a 'b ) 100 ) ==> (set a 'b 100) */
        /*  Note that this last one could be handled like the indexed case above. */
        /*  i.e. (setq (ref a 'b ) 100 ) ==> (setq a 'b 100) */
        /*  Also note that (setq (ref #B2) 100 ) ==> (setq #B2 100) */
        

        /*  Fixup the start of the  sublist and attach the correct cdr. */
        
        savPair = movPair = TPair_New(gCP,gTP);
        asObject(&movPair->itsCar) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"set");
        asTag(&movPair->itsCar) = TYSYMBOL;
        
        while ((tmpPair = _FCompile_CdrPair(tmpPair)) != NULL)
            {
            /*  Copy the rest of the argument list into a new list. */
            
            asObject(&movPair->itsCdr) = (TObject*)TPair_New(gCP,gTP);
            asTag(&movPair->itsCdr) = TYPAIR;
            movPair = _FCompile_CdrPair(movPair);
            movPair->itsCar = tmpPair->itsCar;
            }
            
        asObject(&movPair->itsCdr) = (TObject*)pairs[2];
        asTag(&movPair->itsCdr) = TYPAIR;

        /*  We will send it to ourselves. */
                
        /*  VOID up the return location */

        _Result(compilerState) = gCP->Tval_VOID;

        /*  Then we send it. */
        
        asObject(&*tmp) = (TObject*)savPair;
        asTag(&*tmp) = savPair->itsObjectType;
        /*FConio_Writeln(gCP,gTP,1, &*tmp); */
        
        *ret = FCompile_RecognizeHead(gCP, gTP, compilerState, savPair);
        _FCompile_FrameChk(*ret);

        _LastResult(compilerState) = *ret;
        }
    else
		{
 		*ret = TERROR("!setq: Invalid variable argument: check if reference is valid!");
		_FCompile_FrameExit(*ret);
		}
    }
else
    {
 	*ret = TERROR("!setq: Unrecognizable syntax!");
	_FCompile_FrameExit(*ret);
    }
    
if (!isNullTval(final))
    {
    /*  If an explicit final result was requested, we may have to generate code for it */
    
    *ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
    }

_FCompile_FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_setf

(setf %var exp)

The  setf  Special Form changes the constraint formula of the constrained variable {%var}.

For example

    (setf  %x  {(sqrt %y)})
    
#endif

TVAL    FSpecialForms3_setf(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,movPair);
DeclareOBJ(TPair,savPair);
DeclareOBJ(TPair,repPair);
DeclareOBJ(TPair,keyPair);
DeclareOBJ(TPair,newPair);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareTVAL(setfRepo);
DeclareTVAL(setfRepoKey);
DeclareOBJArray(TPair,pairs,8);
DeclareTVALArray(prmv,11);
_FCompile_EndFrame

/* Abort if rest of expression is NULL */
if (curPair == NULL)
    {
 	*ret = TERROR("!setf: Missing Arguments!");
	_FCompile_FrameExit(*ret);
    }

/*  Set up an array of pointers into the input list for easy indexed access */

_FMacro_PairsToArray(curPair, pairs, sizeof(pairs)/sizeof(pairs[0]));

/*  Initialization */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], VMMOVE);

/*  Remember where the final result should go, if anywhere. */

*final = _Result(compilerState);

if (pairs[1] == NULL)
    {
 	*ret = TERROR("!setf: Missing Arguments!");
	_FCompile_FrameExit(*ret);
    }

if(asTag(&pairs[1]->itsCar) == TYPAIR )
    {
    /*  The following forms are recognized in this section: */
    /*  (setf (ref ...)  exp) */
    
	/*  Point into the target list of (ref cell) or */
	/*	(ref Repository keyValue) for easy indexed access */
    tmpPair = asPair(&pairs[1]->itsCar);

	
	/*  Check for the special case of (setf (ref Repository keyValue) exp) */
	/*	and make it (saveImmediate Repository keyValue exp). */

    if ((asTag(&tmpPair->itsCar) == TYSYMBOL) && 
        (asSymbol(&tmpPair->itsCar) == gCP->FMacro_refSym) &&
        (!isNullTval(&tmpPair->itsCdr)) &&
        (!isNullTval(&pairs[1]->itsCdr))&& 
        (tmpPair->itsCdr.Tag == TYPAIR) &&
		(tmpPair->itsCdr.u.Pair->itsCdr.Tag == TYPAIR) &&
        (tmpPair->itsCdr.u.Pair->itsCdr.u.Pair->itsCdr.Tag == TYVOID))
        {
        /*  We handle (setf (ref Repository keyValue) exp) as a special case: */
        /*   0             1          2        3    */
        /*  (saveImmediate Repository keyValue exp) */
        *setfRepo = tmpPair->itsCdr.u.Pair->itsCar;
        *setfRepoKey = tmpPair->itsCdr.u.Pair->itsCdr.u.Pair->itsCar;
        
        /*  Generate the code for the expression: */
		/*	((saveImmediate Repository keyValue exp)) */

        savPair = TPair_New(gCP,gTP);
        tmpPair = TPair_New(gCP,gTP);
        repPair = TPair_New(gCP,gTP);
        keyPair = TPair_New(gCP,gTP);
        savPair->itsCar.Tag = TYPAIR;
        savPair->itsCar.u.Object = (TObject*)tmpPair;
        
        tmpPair->itsCar = gCP->Tval_VOID;
		tmpPair->itsCar.u.Object = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"saveImmediate");
        tmpPair->itsCar.Tag = TYSYMBOL;
        tmpPair->itsCdr.u.Object = (TObject*)repPair;
        tmpPair->itsCdr.Tag = TYPAIR;

        repPair->itsCar = *setfRepo;
        repPair->itsCdr.u.Object = (TObject*)keyPair;
        repPair->itsCdr.Tag = TYPAIR;

        keyPair->itsCar = *setfRepoKey;
        keyPair->itsCdr.u.Object = (TObject*)newPair;
        keyPair->itsCdr.Tag = TYPAIR;

        if (pairs[2] == NULL)
			{
 			*ret = TERROR("!setf: Invalid variable argument: check if repository reference is valid!");
			_FCompile_FrameExit(*ret);
			}

        keyPair->itsCdr.u.Object = (TObject*)pairs[2];
        keyPair->itsCdr.Tag = TYPAIR;
        
        _Result(compilerState) = gCP->Tval_VOID;
        *ret = FCompile_Recognize(gCP, gTP, compilerState, savPair);
        _FCompile_FrameChk(*ret);
                    
        _LastResult(compilerState) = *ret;
        }
    else            
        {
 		*ret = TERROR("!setf: Invalid variable argument: check if repository reference is valid!");
		_FCompile_FrameExit(*ret);
        }
    }
else
    {
  	*ret = TERROR("!setf: Unrecognizable syntax!");
	_FCompile_FrameExit(*ret);
    }
    
if(!isNullTval(final))
    {
    /*  If an explicit final result was requested, we may have to generate code for it */
    
    *ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
    }

_FCompile_FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_setv

(setv %var exp)

The  setv  Special Form erases the constraint formula of the constrained variable {%var}, 
and sets the value.

For example

    (setv  %x  22)
    
#endif

TVAL    FSpecialForms3_setv(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(ret);
DeclareTVAL(final);
DeclareOBJArray(TPair,pairs,8);
DeclareTVALArray(prmv,11);
_FCompile_EndFrame

/* Abort if rest of expression is NULL */
if (curPair == NULL)
    {
  	*ret = TERROR("!setv: Missing Arguments!");
	_FCompile_FrameExit(*ret);
    }

/*  Set up an array of pointers into the input list for easy indexed access */

_FMacro_PairsToArray(curPair, pairs, sizeof(pairs)/sizeof(pairs[0]));

if (pairs[0] == NULL)
    {
  	*ret = TERROR("!setv: Missing Arguments!");
	_FCompile_FrameExit(*ret);
    }

/*  Initialization */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], VMMOVE);

/*  Remember where the final result should go, if anywhere. */

*final = _Result(compilerState);

if(asTag(&pairs[1]->itsCar) == TYPAIR )
    {
    /*  The following forms are recognized in this section: */
    /*  (setv (ref ...)  exp) */
    
    tmpPair = asPair(&pairs[1]->itsCar);
    }
else
    {
  	*ret = TERROR("!setv: Unrecognizable syntax!");
	_FCompile_FrameExit(*ret);
    }
    
if(!isNullTval(final))
    {
    /*  If an explicit final result was requested, we may have to generate code for it */
    
    *ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
    }

_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSpecialForms3_addGotoRef


The FSpecialForms3_addGotoRef function installs a goto reference in the _GotosP structure.
    
#endif

TVAL FSpecialForms3_addGotoRef(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval,TVAL newKey, TVAL newValue)
{
StartFrame
DeclareOBJ(TStructure,self);
EndFrame

self = (TStructure*)asObject(&selfTval);

/*  Convert text and string keys to symbolic keys. */
/*  Note:   Symbolic keys guarantee uniqueness. */

if ((newKey.Tag == TYTEXT) || (newKey.Tag == TYSTRING) || (newKey.Tag == TYSTRINGSUBSTR))
    {
    newKey = TSymbol_SymbolAnyCnv(gCP,gTP,TYSYMBOL,newKey);
    }

if (_TObject_TypeFlag(asTag(&newKey)) != _TObject_TfTOBJECT)
    FrameExit(gCP->TObject_ERROR_INVALID);

 
/* add a new Binding */
TStructure_SetMaxIndex(gCP,gTP,selfTval, self->itsMaxItemIndex + 1);
atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Key = asObject(&newKey);
atHMBind(self->itsDictionaryArray,self->itsMaxItemIndex-1).Value = newValue;

    
FrameExit(selfTval);
}
