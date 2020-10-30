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

#define _C_FMACRO
#define _SMARTBASE

#if 0
FMacro.c

Support for the TyMACROS.

This source file contains the support functions for the LISP MACRO preprocessor. Each 
function in this file is responsible for altering the list passed to it and returning 
a new list which represents an equivilent expression using procedures and/or special forms
which are available in this implementation of LISP.

PARENT:              

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fmacro.h"
#include "futil1.h"


/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_Init

Initialize the SmartLisp Lexical Analyser function library.  

#endif

TVAL FMacro_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ec);
EndFrame

if(!gCP->FMacro_Initialized)
    {

FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYLEX,
					(LpCHAR)"!LEX!",
					_TObject_TfNATIVE,
					NIL,
					(LpFNEW)&TObject_NewNever,
					&TObject_MarkNever,
					&FMacro_GlobalMark,
					&FObject_VoidAnyCnv,
					&TObject_VoidAnyCmp,
					&FObject_SetIV1Never,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&FObject_GetIV1Never,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapDefault,
					&TObject_Mapc,
					&TObject_PrintDefault,
					&TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
    
FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYLABELSYMBOL,
					(LpCHAR)"LabelSymbol",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TObject_NewNever,
					&TSymbol_Mark,
					&TObject_GlobalMarkNever,
					&TSymbol_SymbolAnyCnv,
					&TSymbol_SymbolAnyCmp,
					&FObject_SetIV1Never,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&FObject_GetIV1Never,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapDefault, 
					&TObject_Mapc, 
					&TSymbol_PrintQuoted,
					&TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);



    /*  Register new global C Functions */
    
    *ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"memstat",(LpFUNC)&FMemory_SystemDiagnostic);
    ExitOnError(*ec);
    *ec = FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"systemCheck",(LpFUNC)&FMemory_SystemDiagnostic);
    ExitOnError(*ec);


    /*  Register the keyword symbols and assign globals */
    
    gCP->FMacro_boolean      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"boolean");
    FObject_Perm(gCP,gTP,(TObject*) gCP->FMacro_boolean,TRUE);   
    gCP->FMacro_macro        = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"macro");
    FObject_Perm(gCP,gTP,(TObject*) gCP->FMacro_macro,TRUE);
    gCP->FMacro_vm           = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"vm");
    FObject_Perm(gCP,gTP,(TObject*) gCP->FMacro_vm,TRUE);
    gCP->FMacro_refSym       = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ref");
    FObject_Perm(gCP,gTP,(TObject*) gCP->FMacro_refSym,TRUE);
    gCP->FMacro_makevecSym   = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"makeVector");
    FObject_Perm(gCP,gTP,(TObject*) gCP->FMacro_makevecSym,TRUE);
    gCP->FMacro_makeenvSym   = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"makeStructure");
    FObject_Perm(gCP,gTP,(TObject*) gCP->FMacro_makeenvSym,TRUE);
    gCP->FMacro_quoteSym     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"quote");
    FObject_Perm(gCP,gTP,(TObject*) gCP->FMacro_quoteSym,TRUE);
    

    gCP->FMacro_Initialized = TRUE;
    }

FrameExit(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_Defstruct

    (defstruct name: include: parent: fieldnames... )
    (defineStructure name: include: parent: `fieldnames... )

#endif

TVAL FMacro_Defstruct(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
TVAL            retTval;
StartFrame
DeclareOBJ(TPair,newList);
DeclareOBJ(TPair,oldList);
DeclareOBJ(TPair,newPair);
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareOBJArray(TPair,pairs,4);
EndFrame

/*  Make sure there is only one argument and it must be a list. */

if (argc != 1 || argv[0].Tag != TYPAIR)
    goto BadCleanUp;


/*  We must replace the input list with a new list which has */
/*  defineStructure at its head and all symbols quoted. The */
/*  input list is as follows: */
/*  ( 0     1       2       3... */
/*  (name: include: parent: fieldnames... ) */
/*  The output list must be as follows: */
/*  (defineStructure name: include: parent: fieldnames: ... ) */
/*  Note:   We start by placing defineStructure at the head */
/*          of the new list. */

newList = newPair = TPair_New(gCP,gTP);
*tmp = TOBJ(TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"defineStructure"));
_FMacro_AddAtom(newList, *tmp);

/*  Now traverse the car's in the input list and add them to the */
/*  new list. Quote any symbols. */

oldList = asPair(&argv[0]);
/* The input list must have at least one argument */
if (oldList == NULL)
    goto BadCleanUp;
do  {
    /*  Append all old arguments to the new list. */
    
    if (oldList->itsCar.Tag != TYVOID)
        {
        _FMacro_AddAtom(newList, oldList->itsCar);
        }
        
    /*  Quote any unquoted symbols. */
    
    if (newList->itsCar.Tag == TYSYMBOL)
        {
        newList->itsCar.Tag = TYQUOTEDSYMBOL;
        asQuoteCnt(&newList->itsCar) = 1;
        }
        
    /*  Stop only when we run out of old arguments. */
    
    if (oldList->itsCdr.Tag == TYPAIR)
        oldList = asPair(&oldList->itsCdr);
    else
        break;
    }
while (TRUE);

/*  Define the new class immediately so that any methods in this */
/*  source file can know about the new class without waiting.    */

retTval = TOBJ(newPair);
*ret = FUtil1_Eval(gCP,gTP,1,&retTval);

/*  Return the first Pair pair in the new list. */

retTval = TOBJ(newPair);
FrameExit(retTval);

BadCleanUp:
FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_GlobalMark

The marking function for global objects associated with FLisp, and FCompile.

#endif

TVAL FMacro_GlobalMark(LpXCONTEXT gCP, LpTHREAD gTP)
{
gTP = gTP; // NOOP to hide unused parameter warning message
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_Defvm

This macro allows the declaration of Lambdas as virtual machines for other Lambdas.

The defvm macro is similar, in structure, to the defmacro macro; however, the defvm
macro creates an Lambda which is assigned to the specified global name symbol as
an Lambda Virtual Machine. 

(gCP,gTP,defun 0    1       [2      3 ]     [4      5 ]         6...)
(defvm name(arg...) vars: (vars...) pvars: (pvars...) exp...)

#endif

TVAL FMacro_Defvm(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])    
{
NUM             length;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,savPair);
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareOBJArray(TPair,pairs,3);
EndFrame

if(argc != 1 || asTag(&argv[0]) != TYPAIR )
    goto BadCleanUp;
    
*tmp = FUtil2_Length(gCP,gTP,1, argv);
length = asInt(tmp);

if(length < 3 )
    goto BadCleanUp;

/*  Initialize */

_FMacro_PairsToArray(asPair(&argv[0]), pairs, 3);

/*  Initialize */

curPair = TPair_New(gCP,gTP);
asTag(ret) = TYPAIR;    
asObject(ret) = (TObject*)curPair;
    
/* ( name(arg...) ... */
/* (define vm:[1] ( name arg...)... */

*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)gCP->FCompile_defineSym;
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);

*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)gCP->FMacro_vm;
asTag(tmp) = TYQUOTEDSYMBOL;    
asQuoteCnt(tmp) = 1;    
_FMacro_AddAtom(curPair, *tmp);

if(isPair(&pairs[1]->itsCar))
    {
    /*  insert a new pair... */
    
    savPair = asPair(&pairs[1]->itsCar);
    tmpPair = (TPair*)TPair_Copy(gCP,gTP,pairs[1]->itsCar);
    asTag(&savPair->itsCdr) = TYPAIR;
    asObject(&savPair->itsCdr) = (TObject*)tmpPair;
    
    savPair->itsCar = pairs[0]->itsCar;
    
    asTag(&curPair->itsCdr) = TYPAIR;
    asObject(&curPair->itsCdr) = (TObject*)pairs[1];
    }
else
    goto BadCleanUp;


FrameExit(*ret);

BadCleanUp:
FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_Defun

(gCP,gTP,defun 0    1       [2      3 ]     [4      5 ]         6...)
(defun name(arg...) vars: (vars...) pvars: (pvars...) exp...)
(define  (name  arg...)  vars:  (var...)  pvars:  (var...)  exp...)
(define  (0  1...)  2  (3...)  4  (5...)  6...)


(defun timingtest (proc count) vars: (starttime endtime) 
        (set starttime (getTickCount 0))
        (proc count)
        (set endtime (getTickCount starttime))
        (writeln "elapsed time is " endtime "seconds")
)

(define (timingtest proc count) vars: (starttime endtime) exp... )
#endif

TVAL FMacro_Defun(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])    
{
TVAL            retTval;
NUM             expNdx;
NUM             varsNdx;
NUM             pvarsNdx;
NUM             length;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,savPair);
DeclareTVAL(tmp);
DeclareOBJArray(TPair,pairs,8);
EndFrame

if(argc != 1 || asTag(&argv[0]) != TYPAIR )
    goto BadCleanUp;
    
*tmp = FUtil2_Length(gCP,gTP,1, argv);
length = asInt(tmp);

if(length < 3 )
    goto BadCleanUp;

/*  Initialize */

varsNdx = pvarsNdx = expNdx = 0;
_FMacro_PairsToArray(asPair(&argv[0]), pairs, 3);
curPair = savPair = TPair_New(gCP,gTP);

/*  (defun   0  (1)    2     (3)    4      (5)   6) */
/*  (defun name(arg) vars:(vars) pvars:(pvars) exp) */
/*  (define  (name  arg)  vars:  (var)  pvars:  (var)  exp) */
/*  (define  (0       1)    2     (3)     4      (5)     6) */

asObject(tmp) = (TObject*)gCP->FCompile_defineSym;
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);

if(asTag(&pairs[1]->itsCar) == TYPAIR)
    {
    tmpPair = TPair_New(gCP,gTP);
    tmpPair->itsCar = asPair(&pairs[1]->itsCar)->itsCar;
    tmpPair->itsCdr = asPair(&pairs[1]->itsCar)->itsCdr;
    asTag(&asPair(&pairs[1]->itsCar)->itsCdr) = TYPAIR;
    asObject(&asPair(&pairs[1]->itsCar)->itsCdr) = (TObject*)tmpPair;
    asPair(&pairs[1]->itsCar)->itsCar = pairs[0]->itsCar;
    
    asTag(tmp) = TYPAIR;    
    asObject(tmp) = (TObject*)pairs[1];
    curPair->itsCdr = *tmp;
    }
else
if((asTag(&pairs[1]->itsCar) == TYSYMBOL) && (asSymbol(&pairs[1]->itsCar) == gCP->TLambda_nil))
    {
    
    tmpPair = TPair_New(gCP,gTP);
    tmpPair->itsCar = pairs[0]->itsCar;
    asTag(&pairs[0]->itsCar) = TYPAIR;
    asObject(&pairs[0]->itsCar) = (TObject*)tmpPair;
    
    pairs[0]->itsCdr = pairs[1]->itsCdr;
    asTag(tmp) = TYPAIR;
    asObject(tmp) = (TObject*)pairs[0];
    curPair->itsCdr = *tmp;
    }
else
    {
    goto BadCleanUp;
    }

asTag(&retTval) = TYPAIR;   
asObject(&retTval) = (TObject*)savPair;

FrameExit(retTval);

BadCleanUp:
FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_Defmacro

            0   1        2      4       5       6       7
(defmacro name  (arg...) vars: (var...) pvars: (var...) exp...)
(define macro: (name arg...) vars: (var...) pvars: (var...) exp...)

#endif

TVAL FMacro_Defmacro(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]) 
{
NUM             length;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,savPair);
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareOBJArray(TPair,pairs,3);
EndFrame

if ((argc != 1) || (argv[0].Tag != TYPAIR))
    goto BadCleanUp;
    
*tmp = FUtil2_Length(gCP,gTP,1,argv);
length = tmp->u.Int;

if (length < 3 )
    goto BadCleanUp;

/*  Initialize */

_FMacro_PairsToArray(argv[0].u.Pair, pairs, 3);

/*  Initialize */

curPair = TPair_New(gCP,gTP);
ret->Tag = TYPAIR;    
ret->u.Pair = curPair; 

/* Create '(define) list constant off (curPair) */

*tmp = gCP->Tval_VOID;    
tmp->u.Symbol = gCP->FCompile_defineSym;
tmp->Tag = TYSYMBOL;    
curPair->itsCar = *tmp;

/* Create '(define macro:) list constant off (curPair) */

curPair->itsCdr.u.Pair = TPair_New(gCP,gTP);
curPair->itsCdr.Tag = TYPAIR;
curPair = curPair->itsCdr.u.Pair;

*tmp = gCP->Tval_VOID;    
tmp->u.Symbol = gCP->FMacro_macro;
tmp->Tag = TYQUOTEDSYMBOL;    
tmp->QuoteCnt = 1;    
curPair->itsCar = *tmp;

if (pairs[1]->itsCar.u.Pair->itsCar.Tag == TYVOID)
	{
	/*  (defmacro name() ...)
	    insert a new pair ==> (define macro: (name) ...) */
	
	savPair = TPair_New(gCP,gTP);
	savPair->itsCar = pairs[0]->itsCar;
	pairs[1]->itsCar.Tag = TYPAIR;
	pairs[1]->itsCar.u.Pair = savPair;

	curPair->itsCdr.Tag = TYPAIR;
	curPair->itsCdr.u.Pair = pairs[1];
	}
else
if (pairs[1]->itsCar.Tag == TYPAIR)
    {
	/*  (defmacro name(arg...) ...)
        insert a new pair ==> (define macro: (name arg...) ...) */
    
	savPair = pairs[1]->itsCar.u.Pair;
	tmpPair = (TPair*)TPair_Copy(gCP,gTP,pairs[1]->itsCar);
	savPair->itsCdr.Tag = TYPAIR;
	savPair->itsCdr.u.Pair = tmpPair;
	
	savPair->itsCar = pairs[0]->itsCar;
	
	curPair->itsCdr.Tag = TYPAIR;
	curPair->itsCdr.u.Pair = pairs[1];
    }
else
    goto BadCleanUp;


FrameExit(*ret);

BadCleanUp:
FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_Defmethod

    (gCP,gTP,defmethod type: msg(arg) vars: (var) pvars: (var) exp )
    
    (defmethod type: msg(arg...) vars: (var...) pvars: (var...) exp ...)
    (addMethod type: msg: (lambda (arg...) vars: (var...) pvars: (var...) exp ...))
    
#endif

TVAL FMacro_Defmethod(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
TVAL            retTval;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareOBJ(TPair,savPair);
DeclareTVAL(tmp);
DeclareOBJArray(TPair,pairs,4);
EndFrame

if(argc != 1 || asTag(&argv[0]) != TYPAIR )
    goto BadCleanUp;

/*  Initialize */
_FMacro_PairsToArray(asPair(&argv[0]), pairs, 4);
curPair = savPair = TPair_New(gCP,gTP);

/* Abort if the rest of expression is NULL */
if (pairs[0] == NULL)
    goto BadCleanUp;

    
/*  ( type: msg(arg...) ... */
/*  ( 0     1   2       ... */
/*  (addMethod type: msg: (lambda (arg...) ... */
/*  (addMethod 0 '1 (lambda 2 ... */


*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"addMethod");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);

_FMacro_AddAtom(curPair, pairs[0]->itsCar);

asTag(tmp) = TYQUOTEDSYMBOL;
asQuoteCnt(tmp) = 1;
if (pairs[1] == NULL || 
    (asTag(&pairs[1]->itsCar) != TYSYMBOL &&
    asTag(&pairs[1]->itsCar) != TYQUOTEDSYMBOL))
    {
    goto BadCleanUp;
    }
asObject(tmp) = (TObject*)asSymbol(&pairs[1]->itsCar);
_FMacro_AddAtom(curPair, *tmp);

_FMacro_NewNode(curPair);

*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"lambda");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);

asTag(tmp) = TYPAIR;    
asObject(tmp) = (TObject*)pairs[2];
curPair->itsCdr = *tmp;

asTag(&retTval) = TYPAIR;   
asObject(&retTval) = (TObject*)savPair;

FrameExit(retTval);

BadCleanUp:
FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_TimesEquals

(*= dst src) <==> (setq dst (* dst src))

#endif

TVAL FMacro_TimesEquals(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])  
{
REAL            length;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareTVAL(tmp);
DeclareTVAL(firstCar);
DeclareOBJArray(TPair,pairs,4);
EndFrame

if(argc != 1 || asTag(&argv[0]) != TYPAIR )
    goto BadCleanUp;

*tmp = FUtil2_Length(gCP,gTP,1, argv);
length = asInt(tmp);
if(length != 2)
    goto BadCleanUp;

/*  Initialize */

curPair                     = asPair(&argv[0]);
*firstCar             = curPair->itsCar;
_FMacro_PairsToArray(curPair, pairs, 2);
curPair->itsCar				= gCP->Tval_VOID;
curPair->itsCdr				= gCP->Tval_VOID;
    
/* ( dst src) <==> (setq dst (* dst src)) */
/* (firstCar [1]) <==> (setq firstCar (* firstCar [1])) */

*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setq");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
_FMacro_NewNode(curPair);
*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"*");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
_FMacro_AddAtom(curPair, pairs[1]->itsCar);

FrameExit(argv[0]);

BadCleanUp:
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_PlusPlus

(++ dst) <==> (setq dst (+ dst 1))

    and also
    
( 1+ dst) <==> (setq dst (+ dst 1))

#endif

TVAL FMacro_PlusPlus(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]) 
{
REAL            length;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareTVAL(tmp);
DeclareTVAL(firstCar);
EndFrame

if(argc != 1 || asTag(&argv[0]) != TYPAIR )
    goto BadCleanUp;

*tmp = FUtil2_Length(gCP,gTP,1, argv);
length = asInt(tmp);
if(length != 1)
    goto BadCleanUp;

/*  Initialize */

curPair                     = asPair(&argv[0]);
*firstCar             = curPair->itsCar;
curPair->itsCar				= gCP->Tval_VOID;
curPair->itsCdr				= gCP->Tval_VOID;
    
/* (++ dst ) <==> (setq dst (+ dst 1)) */
/* ( firstcar ) <==> (setq firstcar (+ firstcar 1)) */

*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setq");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
_FMacro_NewNode(curPair);
*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"+");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
tmp->Tag = TYNUM;    
tmp->u.Int = 1;
_FMacro_AddAtom(curPair, *tmp);

FrameExit(argv[0]);

BadCleanUp:
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_PlusEquals

(+= dst src) <==> (setq dst (+ dst src))

#endif

TVAL FMacro_PlusEquals(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])   
{
REAL            length;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareTVAL(tmp);
DeclareTVAL(firstCar);
DeclareOBJArray(TPair,pairs,4);
EndFrame

if(argc != 1 || asTag(&argv[0]) != TYPAIR )
    goto BadCleanUp;

*tmp = FUtil2_Length(gCP,gTP,1, argv);
length = asInt(tmp);
if(length != 2)
    goto BadCleanUp;

/*  Initialize */

curPair                     = asPair(&argv[0]);
*firstCar             = curPair->itsCar;
_FMacro_PairsToArray(curPair, pairs, 2);
curPair->itsCar				= gCP->Tval_VOID;
curPair->itsCdr				= gCP->Tval_VOID;

    
/* ( dst src) <==> (setq dst (* dst src)) */
/* (firstCar [1]) <==> (setq firstCar (+ firstCar [1])) */

*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setq");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
_FMacro_NewNode(curPair);
*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"+");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
_FMacro_AddAtom(curPair, pairs[1]->itsCar);

FrameExit(argv[0]);

BadCleanUp:
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_MinusMinus

(gCP,gTP, -- dst ) <==> (setq dst (- dst 1))

    and also
    
(-- dst ) <==> (setq dst (- dst 1))

#endif

TVAL FMacro_MinusMinus(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])   
{
REAL            length;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareTVAL(tmp);
DeclareTVAL(firstCar);
EndFrame

if(argc != 1 || asTag(&argv[0]) != TYPAIR )
    goto BadCleanUp;

*tmp = FUtil2_Length(gCP,gTP,1, argv);
length = asInt(tmp);
if(length != 1)
    goto BadCleanUp;

/*  Initialize */

curPair                     = asPair(&argv[0]);
*firstCar             = curPair->itsCar;
curPair->itsCar				= gCP->Tval_VOID;
curPair->itsCdr				= gCP->Tval_VOID;
    
/* (-- dst ) <==> (setq dst (- dst 1)) */
/* ( firstcar ) <==> (setq firstcar (- firstcar 1)) */

*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setq");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
_FMacro_NewNode(curPair);
*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"-");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
tmp->Tag = TYNUM;    
tmp->u.Int = 1;
_FMacro_AddAtom(curPair, *tmp);

FrameExit(argv[0]);

BadCleanUp:
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_MinusEquals

(-= dst src) <==> (setq dst (- dst src))

#endif

TVAL FMacro_MinusEquals(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])  
{
REAL            length;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareTVAL(tmp);
DeclareTVAL(firstCar);
DeclareOBJArray(TPair,pairs,4);
EndFrame

if(argc != 1 || asTag(&argv[0]) != TYPAIR )
    goto BadCleanUp;

*tmp = FUtil2_Length(gCP,gTP,1, argv);
length = asInt(tmp);
if(length != 2)
    goto BadCleanUp;

/*  Initialize */

curPair                     = asPair(&argv[0]);
*firstCar             = curPair->itsCar;
_FMacro_PairsToArray(curPair, pairs, 2);
curPair->itsCar				= gCP->Tval_VOID;
curPair->itsCdr				= gCP->Tval_VOID;

    
/* ( dst src) <==> (setq dst (* dst src)) */
/* (firstCar [1]) <==> (setq firstCar (- firstCar [1])) */

*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setq");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
_FMacro_NewNode(curPair);
*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"-");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
_FMacro_AddAtom(curPair, pairs[1]->itsCar);

FrameExit(argv[0]);

BadCleanUp:
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_DivEquals

(/= dst src) <==> (setq dst (/ dst src))

#endif

TVAL FMacro_DivEquals(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])    
{
REAL            length;
StartFrame
DeclareOBJ(TPair,curPair);
DeclareTVAL(tmp);
DeclareTVAL(firstCar);
DeclareOBJArray(TPair,pairs,4);
EndFrame

if(argc != 1 || asTag(&argv[0]) != TYPAIR )
    goto BadCleanUp;

*tmp = FUtil2_Length(gCP,gTP,1, argv);
length = asInt(tmp);
if(length != 2)
    goto BadCleanUp;

/*  Initialize */

curPair                     = asPair(&argv[0]);
*firstCar             = curPair->itsCar;
_FMacro_PairsToArray(curPair, pairs, 2);
curPair->itsCar				= gCP->Tval_VOID;
curPair->itsCdr				= gCP->Tval_VOID;
    
/* ( dst src) <==> (setq dst (* dst src)) */
/* (firstCar [1]) <==> (setq firstCar (/ firstCar [1])) */

*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setq");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
_FMacro_NewNode(curPair);
*tmp = gCP->Tval_VOID;    
asObject(tmp) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"/");
asTag(tmp) = TYSYMBOL;  
_FMacro_AddAtom(curPair, *tmp);
_FMacro_AddAtom(curPair, *firstCar);
_FMacro_AddAtom(curPair, pairs[1]->itsCar);

FrameExit(argv[0]);

BadCleanUp:
    FrameExit( gCP->TObject_ERROR_INVALID_ARGLIST);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FMacro_NewCMacro

The FMacro_NewCMacro function converts the specified text into a symbol. The symbol`s
value in the symbol global value member is given the value of a TYCMACRO.

#endif

TVAL FMacro_NewCMacro(LpXCONTEXT gCP, LpTHREAD gTP, TSymbol **symName, LpCHAR funcName, LpFUNC lpFunc)
{
StartFrame
DeclareTVAL(tmpTval);
EndFrame

(*symName) = TSymbol_MakeUnique(gCP,gTP,funcName);
(*symName)->itsCProcedure =  lpFunc;

asTag(tmpTval) = TYCMACRO;
asObject(tmpTval) = (TObject*)*symName;

/*  Calling SetGlobalValue with a non-void value will automatically PERM the symbol */

TSymbol_SetGlobalValue(gCP,gTP,*symName,*tmpTval);

FrameExit(gCP->TObject_TRUE);
}


