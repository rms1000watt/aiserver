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

#define _C_FOPTIMIZE2
#define _SMARTBASE

#if 0 
FOptimize2.c

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fopt1.h"
#include "fopt2.h"
#include "fsforms1.h"
#include "fsforms2.h"
#include "fmacro.h"
#include "futil1.h"
#include "fdebug.h"


/*  Declare the symbols which will be optimized */


/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_Init

#endif

TVAL FOptimize2_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
EndFrame
 
if(gCP->FOpt2_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FOpt2_Initialized = TRUE;
    
/* Register the SmartLisp cProcedures contained in this package */

/* Allocate symbols which we will use during optimization. */


FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_LabelSymbol

This procedure generates a new unique temporary label symbol. Given the current compile state
we construct a temporary label name based on the next available index in the _LabelsP Structure.

#endif

TVAL FOptimize2_LabelSymbol(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState)
{
NUM                 oldMax;
char                buf[32];
StartFrame
DeclareOBJ(TStructure,tmpEnv);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
DeclareTVAL(loc);
DeclareTVAL(sym);
EndFrame

tmpEnv = _LabelsP(compilerState);

/*  We must allocate a new temporary. Generate a name for this */
/*  variable based on the binding index which will be utilized for its storage. */

sprintf((char*)buf, ":_compiler_:%ld", ++gTP->FOpt2_labelCounter);
if (gTP->FOpt2_labelCounter >= 999999) gTP->FOpt2_labelCounter = 100000;

asObject(sym) = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)buf);
asTag(sym) = TYSYMBOL;
sym->u.Symbol->itsGlobalValue = gCP->Tval_VOID;
sym->u.Symbol->itsGlobalLock = 0;

asTag(loc) = TYNUM;
asInt(loc) = -1;

oldMax = tmpEnv->itsMaxItemIndex;
*ret = TStructure_MakeUniqueKey(gCP,gTP,tmpEnv,asObject(sym),*loc);
ExitOnError(*ret);

if(tmpEnv->itsMaxItemIndex == oldMax)
    {
    /*  We have encountered two labels at different locations for the same symbol */
    /*  and this is an error! */
   
    goto BadCleanUp;
    }

/*  If successful we return a TVAL which contains the symbol for the new label. */

FrameExit(*sym);

BadCleanUp:

*ret = TERROR("!SmartLisp Compiler: Duplicate label encountered!");
FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_AddLabel

This procedure creates an entry in the _LabelsP Structure so that when we call 
FCompile_ResolveGotos at the end of compilation all gotos will resolve properly.

#endif

TVAL FOptimize2_AddLabel(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* aSymbol, NUM  location)
{
StartFrame
DeclareOBJ(TStructure,tmpEnv);
DeclareTVAL(ret);
DeclareTVAL(sym);
DeclareTVAL(loc);
EndFrame

tmpEnv = _LabelsP(compilerState);

asTag(loc) = TYNUM;
asInt(loc) = location;
asTag(sym) = TYSYMBOL;
asObject(sym) = (TObject*)aSymbol;

*ret = TStructure_AddNewValue(gCP,gTP,_Labels(compilerState),*sym, *loc);

ExitOnError(*ret);
    
FrameExit(*ret);


}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_AddGoto

This procedure creates an entry in the _GotosP Structure so that when we call 
FCompile_ResolveGotos at the end of compilation all gotos will resolve properly.

We need a special function to do this because normally we do not allow identical keys
in a structure, but we must allow identical keys for the _GotosP Structure.

#endif

TVAL FOptimize2_AddGoto(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* aSymbol, NUM location)
{
StartFrame
DeclareOBJ(TStructure,tmpEnv);
DeclareTVAL(ret);
EndFrame

tmpEnv = _GotosP(compilerState);

/* add a new Binding */

FObject_SetMaxIndex(gCP,gTP,(TObject*)tmpEnv, tmpEnv->itsMaxItemIndex + 1);
atHMBind(tmpEnv->itsDictionaryArray,tmpEnv->itsMaxItemIndex-1).Key = (TObject*)aSymbol;
atHMBind(tmpEnv->itsDictionaryArray,tmpEnv->itsMaxItemIndex-1).Value.u.Int = location;
atHMBind(tmpEnv->itsDictionaryArray,tmpEnv->itsMaxItemIndex-1).Value.Tag = TYNUM;
    
FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_codeResult

Generate the code to return true or false values.

#endif

TVAL    FOptimize2_codeResult(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* passLbl, TSymbol* branchLbl)
{
_FCompile_StartFrame
DeclareTVAL(ret);
DeclareTVAL(final);
_FCompile_EndFrame

*final = _Result(compilerState);
*ret = FOptimize2_codePass(gCP, gTP, compilerState, passLbl, TRUE);
_FCompile_FrameChk(*ret);

_Result(compilerState) = *final;
*ret = FOptimize2_codeBranch(gCP, gTP, compilerState, branchLbl, TRUE);
_FCompile_FrameChk(*ret);

_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_codePass

Generate the code to return the parse tree stored as the global value of the pass
label symbol.

#endif

TVAL    FOptimize2_codePass(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* passLbl, NUM genReturn)
{
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPcodeVector,Pc);
DeclareTVAL(ret);

_FCompile_EndFrame

genReturn = genReturn; // NOOP to hide unused parameter warning message
/*  Setup a pcode object for rapid access. */

Pc = _Pc(compilerState);

/*  Setup the _LabelsP to insure that passLbl resolves properly */

*ret = FOptimize2_AddLabel(gCP, gTP,  compilerState, passLbl, Pc->itsCurItemIndex);
_FCompile_FrameChk(*ret);

/*  Generate code to return default value. */
/*  Allocate a temporary pair for generation of code for results */

tmpPair = TPair_New(gCP,gTP);

tmpPair->itsCar = passLbl->itsGlobalValue;
_LastResult(compilerState) = *ret = FCompile_Singleton(gCP, gTP, compilerState, tmpPair);
_FCompile_FrameChk(*ret); 

if (_TObject_TypeFlag(asTag(&passLbl->itsGlobalValue)) == _TObject_TfTOBJECT)
    {
    /*  We always void the global values after use to insure against crashes in MarkAndSweep */
    /*  after the thing pointed to has been collected. */
    
    if (passLbl != NULL) passLbl->itsGlobalValue = gCP->Tval_VOID;
    }

_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_codeBranch

Generate the code to return the parse tree stored as the global value of the branch
label symbol.

#endif

TVAL    FOptimize2_codeBranch(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* branchLbl, NUM genJump)
{
NUM                 uncondPatch = 0;
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPcodeVector,Pc);
DeclareTVAL(ret);
DeclareTVALArray(prmv,8);

_FCompile_EndFrame

/*  Setup a pcode object for rapid access. */

Pc = _Pc(compilerState);

/*  Generate code to return default value. */
/*  Allocate a temporary pair for generation of code for results */

tmpPair = TPair_New(gCP,gTP);

if (genJump != 0)
    {
    /*  Generate code to unconditionally jump over code to generate branchLbl */
    
    _FCompile_MakeOBJ(prmv[0], Pc);
    _FCompile_MakeINT(prmv[1], VMJUMP);
    _FCompile_MakeINT(prmv[2], AMINTEGER);
    _FCompile_MakeINT(prmv[3], AMVOID);
    _FCompile_MakeINT(prmv[4], AMVOID);
    _FCompile_MakeIMM(prmv[5], -1);
    *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,6,&prmv[0]);
    _FCompile_FrameChk(*ret);
    
    /*  Save the location for the target of the VMJUMP just generated so that we may  */
    /*  backpatch it later.  */
    
    uncondPatch = Pc->itsCurItemIndex - 1;
    }

/*  Setup the _LabelsP to insure that branchLbl resolves properly */

*ret = FOptimize2_AddLabel(gCP, gTP, compilerState, branchLbl, Pc->itsCurItemIndex);
_FCompile_FrameChk(*ret);

/*  Generate code to return alternate. */

tmpPair->itsCar = branchLbl->itsGlobalValue;
_LastResult(compilerState) = *ret = FCompile_Singleton(gCP, gTP, compilerState, tmpPair);
_FCompile_FrameChk(*ret);

if (genJump)
    {
    /* We may now backpatch the location for unconditional jump  generated above */
    
    atHMInt(Pc->itsInstructionArray,uncondPatch) = Pc->itsCurItemIndex;
    }

if (_TObject_TypeFlag(asTag(&branchLbl->itsGlobalValue)) == _TObject_TfTOBJECT)
    {
    /*  We always void the global values after use to insure against crashes in MarkAndSweep */
    /*  after the thing pointed to has been collected. */
    
    if (branchLbl != NULL) branchLbl->itsGlobalValue = gCP->Tval_VOID;
    }

_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_codeJump

Generate the code to return the parse tree stored as the global value of the branch
label symbol. If not false, the genJump argument contains the displacement of the 
backward jump around the generated code.

#endif

TVAL    FOptimize2_codeJump(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* branchLbl, NUM genJump)
{
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPcodeVector,Pc);
DeclareTVALArray(prmv,8);
DeclareTVAL(ret);

_FCompile_EndFrame

/*  Setup a pcode object for rapid access, and save the displacement */
/*  displacement of the jump back*/

Pc = _Pc(compilerState);

/*  Generate code to return default value. */
/*  Allocate a temporary pair for generation of code for results */

tmpPair = TPair_New(gCP,gTP);

/*  Setup the _LabelsP to insure that branchLbl resolves properly */

*ret = FOptimize2_AddLabel(gCP, gTP, compilerState, branchLbl, Pc->itsCurItemIndex);
_FCompile_FrameChk(*ret);

/*  Generate code to return alternate. */

tmpPair->itsCar = branchLbl->itsGlobalValue;
_LastResult(compilerState) = *ret = FCompile_Singleton(gCP, gTP, compilerState, tmpPair);
_FCompile_FrameChk(*ret);

if (genJump)
    {
    /*  Generate code to unconditionally jump over the generated code. */
    
    _FCompile_MakeOBJ(prmv[0], Pc);
    _FCompile_MakeINT(prmv[1], VMJUMP);
    _FCompile_MakeINT(prmv[2], AMINTEGER);
    _FCompile_MakeINT(prmv[3], AMVOID);
    _FCompile_MakeINT(prmv[4], AMVOID);
    _FCompile_MakeIMM(prmv[5], genJump);
    *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,6,&prmv[0]);
    _FCompile_FrameChk(*ret);
    }

/*  We always void the global values after use to insure against crashes in MarkAndSweep */
/*  after the thing pointed to has been collected. */
    
if (_TObject_TypeFlag(asTag(&branchLbl->itsGlobalValue)) == _TObject_TfTOBJECT)
    {
    if (branchLbl != NULL) branchLbl->itsGlobalValue = gCP->Tval_VOID;
    }

_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_orRet

This procedure recognizes all S-expressions (which begin with or) at the top level. 
This procedure generates the correct set of linear virtual machine instructions for returning 
true or false values, and calls the comp-or procedure to generate the proper jump instructions. 

o   Generate the following set of instructions:

        (comp-or true false s-exp _tmplbl1 _tmplbl2)
        _tmplbl1:
        vmreturn true
        _tmplbl2:
        vmreturn false
#endif

TVAL    FOptimize2_orRet(LpXCONTEXT gCP, LpTHREAD gTP,TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TSymbol,passLbl);
DeclareOBJ(TSymbol,branchLbl);
DeclareTVAL(ret);
DeclareTVAL(final);
_FCompile_EndFrame

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  Generate label symbols for true and false. */

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto Last;

passLbl = (TSymbol*)asObject(ret);
passLbl->itsGlobalValue = gCP->TObject_TRUE;

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto Last;

branchLbl = (TSymbol*)asObject(ret);
branchLbl->itsGlobalValue = gCP->TObject_FALSE;

/*  Call appropriate procedure to generate jump code for (or ...) */

*ret = FOptimize2_orJmp(gCP, gTP, compilerState, curPair, TRUE, FALSE, passLbl, branchLbl);
if (ret->Tag == TYERROR) goto Last;

/*  Call appropriate procedure to generate return values */

*ret = FOptimize2_codeResult(gCP, gTP, compilerState, passLbl, branchLbl);
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

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_andRet

This procedure recognizes all S-expressions (which begin with and) at the top level. 
This procedure generates the correct set of linear virtual machine instructions for returning 
true or false values, and calls the comp-and procedure to generate the proper jump instructions. 

o   Generate the following set of instructions:

        (comp-and true false s-exp _tmplbl1 _tmplbl2)
        _tmplbl1:
        vmreturn true
        _tmplbl2:
        vmreturn false
#endif

TVAL    FOptimize2_andRet(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame
DeclareOBJ(TSymbol,passLbl);
DeclareOBJ(TSymbol,branchLbl);
DeclareTVAL(ret);
DeclareTVAL(final);
_FCompile_EndFrame

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  Generate label symbols for true and false. */

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
_FCompile_FrameChk(*ret);

passLbl = (TSymbol*)asObject(ret);
passLbl->itsGlobalValue = gCP->TObject_TRUE;

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto Last;

branchLbl = (TSymbol*)asObject(ret);
branchLbl->itsGlobalValue = gCP->TObject_FALSE;

/*  Call appropriate procedure to generate jump code for booleans */

*ret = FOptimize2_andJmp(gCP, gTP, compilerState, curPair, TRUE, FALSE, passLbl, branchLbl);
if (ret->Tag == TYERROR) goto Last;

/*  Call appropriate procedure to generate return values */

*ret = FOptimize2_codeResult(gCP, gTP, compilerState, passLbl, branchLbl);
if (ret->Tag == TYERROR) goto Last;

if(!isNullTval(final))
    {
    /*  If an explicit final result was requested, we may have to generate code for it */
    
    *ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *final);
    }

Last:
if (passLbl != NULL) passLbl->itsGlobalValue = gCP->Tval_VOID;
if (passLbl != NULL) branchLbl->itsGlobalValue = gCP->Tval_VOID;

_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_orJmp

This procedure generates the correct set of linear virtual machine instructions for an 
S-expression which begins with or.

(comp-or notsw s-exp pass-thru-label branch-label)

The general operation of comp-or is as follows:

o   If necessary, generate intermediate code for the left and right subexpressions.

o   If notsw is true and cmpsw is true, generate the following set of instructions: 

        (comp-rel true true left _tmplbl1 branch-label)
        _tmplbl1:
        (comp-rel true true right pass-thru-label branch-label)
        
o   If notsw is true and cmpsw is false, generate the following set of instructions: 

        (comp-rel true true left _tmplbl1 pass-thru-label)
        _tmplbl1:
        (comp-rel true false right pass-thru-label branch-label)
        
o   If notsw is false and cmpsw is true, generate the following set of instructions: 

        (comp-rel true true left _tmplbl1 pass-thru-label)
        _tmplbl1:
        (comp-rel true false right pass-thru-label branch-label)
        
o   If notsw is false and cmpsw is false, generate the following set of instructions: 

        (comp-rel true true left _tmplbl1 branch-label)
        _tmplbl1:
        (comp-rel true true right pass-thru-label branch-label)
        
The or Special Form evaluates each expression from left to right, returning the value true at 
the first expression that is true.  Any remaining expressions are not evaluated. If no expression 
returns true, the value false is returned.  If no expressions are specified, the value false 
is returned.        

#endif

TVAL    FOptimize2_orJmp(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM notsw, NUM cmpsw, TSymbol* passLbl, 
                                TSymbol* branchLbl)
{
_FCompile_StartFrame
DeclareOBJ(TSymbol,tmpLbl);
DeclareOBJ(TPcodeVector,Pc);
DeclareTVAL(ret);
_FCompile_EndFrame

/*  (or ...)    : */
/*  The or Special Form evaluates each expression from left to right, returning the value true  */
/*  at the first expression that is true.  Any remaining expressions are not evaluated. If no  */
/*  expression returns true, the value false is returned.  If no expressions are specified,  */
/*  the value false is returned.  */

/*  Setup a pcode object for rapid access. */

Pc = _Pc(compilerState);

curPair = _FCompile_CdrPair(curPair);
if(curPair == NULL)
    {
    /*  Code for the special case of (or) no arguments, this is equivilent to (or false) */
    
    curPair = TPair_New(gCP,gTP);
    curPair->itsCar.Tag = TYBOLE;
    curPair->itsCar.u.Bool = FALSE;
    }
    
do 
    {
    /*  Generate label symbols for tmpLbl */
    
    *ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
    _FCompile_FrameChk(*ret);
    tmpLbl = (TSymbol*)asObject(ret);
    tmpLbl->itsGlobalValue.Tag = TYNUM;
    tmpLbl->itsGlobalValue.u.Int = 2;

    if(notsw == TRUE && cmpsw == TRUE)
        {
        if(!isNullTval(&curPair->itsCdr))
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, TRUE, tmpLbl, branchLbl);
        else
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, TRUE, passLbl, branchLbl);
        }
    else
    if(notsw == TRUE && cmpsw == FALSE)
        {
        if(!isNullTval(&curPair->itsCdr))
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, TRUE, tmpLbl, passLbl);
        else
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, FALSE, passLbl, branchLbl);
        }
    else
    if(notsw == FALSE && cmpsw == TRUE)
        {
        if(!isNullTval(&curPair->itsCdr))
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, TRUE, tmpLbl, passLbl);
        else
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, FALSE, passLbl, branchLbl);
        }
    else
    if(notsw == FALSE && cmpsw == FALSE)
        {
        if(!isNullTval(&curPair->itsCdr))
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, TRUE, tmpLbl, branchLbl);
        else
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, TRUE, passLbl, branchLbl);
        }
	if (ret->Tag == TYERROR) goto Last;
    
    /*  Setup the _LabelsP to insure that tmpLbl resolves properly */
    
    *ret = FOptimize2_AddLabel(gCP, gTP, compilerState, tmpLbl, Pc->itsCurItemIndex);
	if (ret->Tag == TYERROR) goto Last;

	if (tmpLbl != NULL) tmpLbl->itsGlobalValue = gCP->Tval_VOID;

    } while((curPair = _FCompile_CdrPair(curPair)) != NULL);

/*  Return the location for the target of the code just generated. */
Last:
if (tmpLbl != NULL) tmpLbl->itsGlobalValue = gCP->Tval_VOID;
_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_andJmp

This procedure generates the correct set of linear virtual machine instructions for an 
S-expression which begins with and.

(comp-and notsw s-exp pass-thru-label branch-label)

The general operation of comp-and is as follows:

o   If necessary, generate intermediate code for the left and right subexpressions.

o   If notsw is true and cmpsw is true, generate the following set of instructions:

        (comp-rel true false left _tmplbl1 pass-thru-label)
        _tmplbl1:
        (comp-rel true true right pass-thru-label branch-label)
        
o   If notsw is true and cmpsw is false, generate the following set of instructions:

        (comp-rel true false left _tmplbl1 branch-label)
        _tmplbl1:
        (comp-rel true false right pass-thru-label branch-label)
        
o   If notsw is false and cmpsw is true, generate the following set of instructions:

        (comp-rel true false left _tmplbl1 branch-label)
        _tmplbl1:
        (comp-rel true false right pass-thru-label branch-label)
        
o   If notsw is false and cmpsw is false, generate the following set of instructions:

        (comp-rel true false left _tmplbl1 pass-thru-label)
        _tmplbl1:
        (comp-rel true true right pass-thru-label branch-label)
        
The and special form evaluates each expression from left to right, returning false as soon as one 
expression fails to return true.  If all expressions return true, the value true is returned. 
If no expressions are specified, the value true is returned. 

#endif

TVAL    FOptimize2_andJmp(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM notsw, NUM cmpsw, TSymbol* passLbl, 
                                TSymbol* branchLbl)
{
_FCompile_StartFrame
DeclareOBJ(TSymbol,tmpLbl);
DeclareOBJ(TPcodeVector,Pc);
DeclareTVAL(ret);
_FCompile_EndFrame

/*  Setup a pcode object for rapid access. */

Pc = _Pc(compilerState);
    
curPair = _FCompile_CdrPair(curPair);
if (curPair == NULL)
    {
    /*  Code for the special case of (and) no arguments, this is equivilent to (and true) */
    
    curPair = TPair_New(gCP,gTP);
    curPair->itsCar.Tag = TYBOLE;
    curPair->itsCar.u.Bool = TRUE;
    }
    
do 
    {
    /*  Generate label symbols for tmpLbl */
    
    *ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
    _FCompile_FrameChk(*ret);
    tmpLbl = (TSymbol*)asObject(ret);
    tmpLbl->itsGlobalValue.Tag = TYNUM;
    tmpLbl->itsGlobalValue.u.Int = 2;

    if(notsw == TRUE && cmpsw == TRUE)
        {
        if(!isNullTval(&curPair->itsCdr))
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, FALSE, tmpLbl, passLbl);
        else
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, TRUE, passLbl, branchLbl);
        }
    else
    if(notsw == TRUE && cmpsw == FALSE)
        {
        if(!isNullTval(&curPair->itsCdr))
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, FALSE, tmpLbl, branchLbl);
        else
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, FALSE, passLbl, branchLbl);
        }
    else
    if(notsw == FALSE && cmpsw == TRUE)
        {
        if(!isNullTval(&curPair->itsCdr))
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, FALSE, tmpLbl, branchLbl);
        else
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, FALSE, passLbl, branchLbl);
        }
    else
    if(notsw == FALSE && cmpsw == FALSE)
        {
        if(!isNullTval(&curPair->itsCdr))
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, FALSE, branchLbl, passLbl);
        else
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, TRUE, passLbl, branchLbl);
        }           
	if (ret->Tag == TYERROR) goto Last;
    
    /*  Setup the _LabelsP to insure that tmpLbl resolves properly */
    
    *ret = FOptimize2_AddLabel(gCP, gTP, compilerState, tmpLbl, Pc->itsCurItemIndex);
	if (ret->Tag == TYERROR) goto Last;

	if (tmpLbl != NULL) tmpLbl->itsGlobalValue = gCP->Tval_VOID;

    } while((curPair = _FCompile_CdrPair(curPair)) != NULL);

/*  Return the location for the target of the code just generated. */

Last:
if (tmpLbl != NULL) tmpLbl->itsGlobalValue = gCP->Tval_VOID;
_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_boolRet

This procedure recognizes all S-expressions (which begin with < <= = <> > >=) at the top level. 
This procedure generates the correct set of linear virtual machine instructions for returning 
true or false values, and calls the comp-cond procedure to generate the proper jump instructions. 

o   Generate the following set of instructions:

        (comp-cond true false s-exp _tmplbl1 _tmplbl2)
        _tmplbl1:
        vmreturn true
        _tmplbl2:
        vmreturn false
        
#endif

TVAL    FOptimize2_boolRet(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM instruction)
{
_FCompile_StartFrame
DeclareOBJ(TSymbol,passLbl);
DeclareOBJ(TSymbol,branchLbl);
DeclareTVAL(final);
DeclareTVAL(ret);
_FCompile_EndFrame

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  Generate label symbols for true and false. */

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
_FCompile_FrameChk(*ret);

passLbl = (TSymbol*)asObject(ret);
passLbl->itsGlobalValue = gCP->TObject_TRUE;

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto Last;

branchLbl = (TSymbol*)asObject(ret);
branchLbl->itsGlobalValue = gCP->TObject_FALSE;

/*  Call appropriate procedure to generate jump code for booleans */

*ret = FOptimize2_boolJmp(gCP, gTP, compilerState, curPair, TRUE, FALSE, passLbl, branchLbl, instruction);
if (ret->Tag == TYERROR) goto Last;

/*  Call appropriate procedure to generate return values */

*ret = FOptimize2_codeResult(gCP, gTP, compilerState, passLbl, branchLbl);
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

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_boolJmp

This procedure generates the correct set of linear virtual machine instructions for an 
S-expression which begins with < <= = <> > >=.

The general operation of comp-cond is as follows:

o   Check that the remaining S-expression is composed of a left and a right subexpression only.

o   If necessary, generate intermediate code for the left and the right subexpressions.

o   If notsw is true and cmpsw is true, generate the following set of instructions:
 
        (vmjmpcc left right branch-label)
        
o   If notsw is true and cmpsw is false, generate the following set of instructions: 

        (vmjmp!cc left right branch-label)
        
o   If notsw is false and cmpsw is true, generate the following set of instructions: 

        (vmjmp!cc left right branch-label)
        
o   If notsw is false and cmpsw is false, generate the following set of instructions: 

        (vmjmpcc left right branch-label)
    
NOTE:   We utilize the ordering of opcodes for conditional jump instructions to calculate the
        correct instruction based on notsw, if this ordering changes then the calculation will 
        fail.
        
#endif

TVAL    FOptimize2_boolJmp(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM notsw, NUM cmpsw, TSymbol* passLbl, 
                                TSymbol* branchLbl, NUM instruction)
{
NUM                 ndx;
CHAR				tmpBuffer[100];
NUM					baseCode = VMJMPEQ;
_FCompile_StartFrame
DeclareOBJ(TPcodeVector,Pc);
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(left);
DeclareTVAL(right);
DeclareTVAL(ret);
DeclareTVALArray(prmv,8);

_FCompile_EndFrame

passLbl = passLbl; // NOOP to hide unused parameter warning message
/*  We do some elementary syntax checking */

if(curPair != NULL)
    {
    /*  Check to insure that we have a valid number of arguments */
    
    for( ndx = 1, tmpPair = curPair; (tmpPair = _FCompile_CdrPair(tmpPair)) != NULL; ndx++) {}
    
    if(ndx == 2)
        {
        /*  Setup a pcode object for rapid access. */
        
        Pc = _Pc(compilerState);
        
        /*  Start setup of the parameter array for the calls to FOptimize_OptimizePcode */
        
        _FCompile_MakeOBJ(prmv[0], Pc);

        /*  We compute the base compare instruction. */

		if ((instruction >= VMJMPEQ) && (instruction <= VMJMPLE)) baseCode = VMJMPEQ;
		if ((instruction >= vmnatJmpEQInteger) && (instruction <= vmnatJmpLEInteger)) baseCode = vmnatJmpEQInteger;
		if ((instruction >= vmnatJmpEQNumber) && (instruction <= vmnatJmpLENumber)) baseCode = vmnatJmpEQNumber;
        
        /*  We utilize the ordering of opcodes for conditional jump instructions to calculate  */
        /*  the correct instruction based on notsw, if this ordering changes then the  */
        /*  calculation will fail. */
        
        if (notsw == TRUE && cmpsw == TRUE) 
        {}   /* Why does this do nothing ? */
        else
        if(notsw == TRUE && cmpsw == FALSE)
            {
            instruction = baseCode + ((instruction - baseCode) + 3) % 6;
            }
        else
        if(notsw == FALSE && cmpsw == TRUE)
            {
            instruction = baseCode + ((instruction - baseCode) + 3) % 6;
            }
        else
        if (notsw == FALSE && cmpsw == FALSE)
        {}   /* Why does this do nothing ? */
        
        _FCompile_MakeINT(prmv[1], instruction);
                
        /*  Allocate a new temp for the next argument */
        
        _Result(compilerState) = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
  
        *ret = FCompile_SetupArg(gCP, gTP, compilerState, curPair->itsCar, &prmv[2], &prmv[5]);
        _FCompile_FrameChk(*ret);

        *left = *ret;
                                        
        /*  We advance the curPair to point to the next argument. */
        
        curPair = _FCompile_CdrPair(curPair);
        
		if ((curPair->itsCar.Tag == TYNUM) && (prmv[5].DeclaredType == TYREAL))
			{
			curPair->itsCar.Tag = TYREAL;
			curPair->itsCar.u.Real = curPair->itsCar.u.Int;
			}
        
        /*  Allocate a new temp for the next argument */
        
        _Result(compilerState) = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
  
        *ret = FCompile_SetupArg(gCP, gTP, compilerState, curPair->itsCar, &prmv[3], &prmv[6]);
        _FCompile_FrameChk(*ret);
        
        *right = *ret;
        
        /*  Init the target of this jump to an illegal value */
                                        
        _FCompile_MakeINT(prmv[4], AMINTEGER);
        _FCompile_MakeIMM(prmv[7], -1);
        
        /*  Append the instruction to the current pcode vector */
        
        *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,8,&prmv[0]);
        _FCompile_FrameChk(*ret);
        
        /*  We update the _GotosP(compilerState) vector to contain the location of every  */
        /* jump generated in this procedure. */
        
        *ret = FOptimize2_AddGoto(gCP,gTP,compilerState, branchLbl, Pc->itsCurItemIndex - 1);
        _FCompile_FrameChk(*ret);
        }
    else
        {
        /*  We did not have one argument, and that is illegal. */
		/* Construct an Error message */
		strcpy(tmpBuffer, "Lisp:");
		FDebug_INSToString(gCP, gTP, instruction,  &tmpBuffer[strlen(tmpBuffer)]);   /* convert the instruction opcode to a string */
		strcat(tmpBuffer, (const char*)" Expression is missing operand!!");
		*ret = TERROR("!SmartLisp Compiler: Duplicate label encountered!");
		_FCompile_FrameExit(*ret);
        }
    }
else
    {
	/*  Syntax did not look good. */  
	strcpy(tmpBuffer, "!Lisp:");
	FDebug_INSToString(gCP, gTP, instruction, &tmpBuffer[strlen(tmpBuffer)]);   /* convert the instruction opcode to a string */
	strcat(tmpBuffer, (const char*)" Expression is missing operand!");
	*ret = TERROR(tmpBuffer);
	_FCompile_FrameExit(*ret);
    } 

/*  Return the location for the target of the code just generated. */

_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_notRet

The not special form returns the boolean complement of the argument. For all arguments, 
not returns true if the argument {arg} is false; otherwise, not returns false. 

The general operation of special-not is as follows:

o   Generate the following set of instructions:

        (comp-not true false s-exp _tmplbl1 _tmplbl2)
        _tmplbl1:
        vmreturn true
        _tmplbl2:
        vmreturn false

For example

    (not  false)    =>  true
    (not  (= 2 2))  =>  false
    (not  (+ 2 2))  =>  false

#endif

TVAL    FOptimize2_notRet(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
_FCompile_StartFrame
DeclareOBJ(TSymbol,passLbl);
DeclareOBJ(TSymbol,branchLbl);
DeclareTVAL(ret);
DeclareTVAL(final);
_FCompile_EndFrame

/*  Remember where to place the final result if one has been specified. */

*final = _Result(compilerState);
_Result(compilerState) = gCP->Tval_VOID;

/*  Generate label symbols for true and false. */

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto Last;

passLbl = (TSymbol*)asObject(ret);
passLbl->itsGlobalValue = gCP->TObject_TRUE;

*ret = FOptimize2_LabelSymbol(gCP, gTP, compilerState);
if (ret->Tag == TYERROR) goto Last;

branchLbl = (TSymbol*)asObject(ret);
branchLbl->itsGlobalValue = gCP->TObject_FALSE;

/*  Call appropriate procedure to generate jump code for booleans */

*ret = FOptimize2_notJmp(gCP, gTP, compilerState, curPair, TRUE, FALSE, passLbl, branchLbl);
if (ret->Tag == TYERROR) goto Last;

/*  Call appropriate procedure to generate return values */

*ret = FOptimize2_codeResult(gCP, gTP, compilerState, passLbl, branchLbl);
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

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_notJmp

This procedure generates the correct set of linear virtual machine instructions for an 
S-expression which begins with not.

(comp-not notsw s-exp pass-thru-label branch-label)

The general operation of comp-not is as follows:

o   Check that the remaining S-expression is composed of a single subexpression only.

o   If necessary, generate intermediate code for the single subexpression.

o   If notsw is true and cmpsw is true, generate the following set of instructions: 

        (comp-rel false true single pass-thru-label branch-label)
        
o   If notsw is true and cmpsw is false, generate the following set of instructions: 

        (comp-rel false false single pass-thru-label branch-label)
        
o   If notsw is false and cmpsw is true, generate the following set of instructions: 

        (comp-rel true true single pass-thru-label branch-label)
        
o   If notsw is false and cmpsw is false, generate the following set of instructions: 

        (comp-rel true false single pass-thru-label branch-label)
    
NOTE:   We utilize the ordering of opcodes for conditional jump instructions to calculate the
        correct instruction based on notsw, if this ordering changes then the calculation will 
        fail.
        
#endif

TVAL    FOptimize2_notJmp(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, NUM notsw, NUM cmpsw, TSymbol* passLbl, 
                                TSymbol* branchLbl)
{
NUM                 ndx;
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(ret);
_FCompile_EndFrame

/*  We do some elementary syntax checking */

if(curPair != NULL)
    {
    /*  Check to insure that we have a valid number of arguments */
    
    for( ndx = 0, tmpPair = curPair; (tmpPair = _FCompile_CdrPair(tmpPair)) != NULL; ndx++) {}
    
    if(ndx == 1)
        {
        /*  We advance the curPair to point to the first and only argument. */
        
        curPair = _FCompile_CdrPair(curPair);
        
        /*  Since this procedure processes "not" we reverse the */
        /*  meaning of true and false. */

        _NotSwN(compilerState) = !_NotSwN(compilerState);

        if(notsw == TRUE && cmpsw == TRUE)
            {
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, FALSE, TRUE, passLbl, branchLbl);
            }
        else
        if(notsw == TRUE && cmpsw == FALSE)
            {
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, FALSE, FALSE, passLbl, branchLbl);
            }
        else
        if(notsw == FALSE && cmpsw == TRUE)
            {
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, TRUE, passLbl, branchLbl);
            }
        else
        if(notsw == FALSE && cmpsw == FALSE)
            {
            *ret = FOptimize2_RecognizeHead(gCP, gTP, compilerState, curPair, TRUE, FALSE, passLbl, branchLbl);
            }                   

        /*  Since this procedure processes "not" we restore the */
        /*  meaning of true and false. */

        /*_NotSwN(compilerState) = !_NotSwN(compilerState);*/
        _FCompile_FrameChk(*ret);

        }
    else
        {
        /*  We did not have one argument, and that is illegal. */
        *ret = TERROR("!SmartLisp: 'not' expression missing operand!");
		_FCompile_FrameExit(*ret);
        }
    }
else
    {
    /*  Syntax did not look good. */
     *ret = TERROR("!SmartLisp: 'not' expression missing operand!");
	_FCompile_FrameExit(*ret);
    } 

/*  Return the location for the target of the code just generated. */

_FCompile_FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_RecognizeHead

(comp-rel notsw s-exp true-label false-label)

This procedure dispatches to the correct compilation procedure (FOptimize2_boolJmp, FOptimize2_or, 
FCompile_andSym) depending upon the head of the S-expression. In the case that we do not have one 
of the above we generate a branch similar to comp-cond based on the value passed or generated. 

The general operation of comp-rel is as follows:

o   If the S-expression has a head of not, then generate the following set of instructions: 

        (comp-not notsw cmpsw s-exp pass-thru-label branch-label)
        
o   If the S-expression has a head of and, then generate the following set of instructions: 

        (comp-and notsw cmpsw s-exp pass-thru-label branch-label)
        
o   If the S-expression has a head of or, then generate the following set of instructions: 

        (comp-or notsw cmpsw s-exp pass-thru-label branch-label)
        
o   Otherwise generate the following set of instructions: 

        (comp-cond notsw cmpsw s-exp pass-thru-label branch-label)
#endif

TVAL    FOptimize2_RecognizeHead(LpXCONTEXT gCP, LpTHREAD gTP,TVector* compilerState, TPair* curPair, NUM notsw, NUM cmpsw, TSymbol* passLbl, TSymbol* branchLbl)
{
TYPE                tag;
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,callPair);
DeclareOBJ(TPair,equPair);
DeclareOBJ(TPair,truePair);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,aPair);
DeclareOBJ(TPcodeVector,Pc);
DeclareTVAL(ret);
EndFrame

if ((tag = asTag(&curPair->itsCar)) == TYPAIR)
    {
	/*  We check to see if this is a symbol which requires special processing. */

	tmpPair = asPair(&curPair->itsCar);

	if (!tmpPair || (tag = asTag(&tmpPair->itsCar)) == TYPAIR)
		{
		/*  If the S-expression is an indirect call, then reconstruct the S-expression to be  */
		/*  (= true S-expression). */

		equPair = TPair_New(gCP,gTP);
		truePair = TPair_New(gCP,gTP);
		callPair = TPair_New(gCP,gTP);
		equPair->itsCar.u.Symbol = gCP->FCompile_ceqSym;
		equPair->itsCar.Tag = TYSYMBOL;
		equPair->itsCdr.u.Pair = truePair;
		equPair->itsCdr.Tag = TYPAIR;
		truePair->itsCar = gCP->TObject_TRUE;
		truePair->itsCdr.u.Pair = callPair;
		truePair->itsCdr.Tag = TYPAIR;
        callPair->itsCar.u.Pair = tmpPair;
		callPair->itsCar.Tag = TYPAIR;
		tmpPair = equPair;
		goto SymbolAtHead;
        }
    else
	if (!tmpPair || (tag = asTag(&tmpPair->itsCar)) != TYSYMBOL)
		{
		/*  If the S-expression is a constant, then reconstruct the S-expression to be  */
		/*  (= true constant). */

		callPair = tmpPair;
		tmpPair = TPair_New(gCP,gTP);
		tmpPair->itsCar = gCP->TObject_TRUE;
		aPair = TPair_New(gCP,gTP);
		asObject(&tmpPair->itsCdr) = (TObject*)aPair;
		asTag(&tmpPair->itsCdr) = TYPAIR;
		asPair(&tmpPair->itsCdr)->itsCar = callPair->itsCar;

		*ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, 
										 VMJMPEQ);
        }
    else
        {
		SymbolAtHead:
        /*  The only symbols which are acceptable at the start of a list are those */
        /*  for special forms or function calls, anything else is an error. */
        
        /*  Get the global value for the symbol object from the car of the cdr pair. */
        
        aSymbol = asSymbol(&tmpPair->itsCar);
        aSymbol = (TSymbol*)asObject(&aSymbol->itsGlobalValue);
        
        if( aSymbol == gCP->FCompile_ceqSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpEQInteger);
            }
        else
        if( aSymbol == gCP->FCompile_cgtSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpGTInteger);
            }
        else
        if( aSymbol == gCP->FCompile_cltSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpLTInteger);
            }
        else
        if( aSymbol == gCP->FCompile_cgeSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpGEInteger);
            }
        else
        if( aSymbol == gCP->FCompile_cleSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpLEInteger);
            }
        else
        if( aSymbol == gCP->FCompile_cneSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpNEInteger);
            }
        else
        if( aSymbol == gCP->FCompile_beqSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpEQInteger);
            }
        else
        if( aSymbol == gCP->FCompile_bgtSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpGTInteger);
            }
        else
        if( aSymbol == gCP->FCompile_bltSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpLTInteger);
            }
        else
        if( aSymbol == gCP->FCompile_bgeSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpGEInteger);
            }
        else
        if( aSymbol == gCP->FCompile_bleSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpLEInteger);
            }
        else
        if( aSymbol == gCP->FCompile_bneSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpNEInteger);
            }
        else
        if( aSymbol == gCP->FCompile_ieqSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpEQInteger);
            }
        else
        if( aSymbol == gCP->FCompile_igtSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpGTInteger);
            }
        else
        if( aSymbol == gCP->FCompile_iltSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpLTInteger);
            }
        else
        if( aSymbol == gCP->FCompile_igeSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpGEInteger);
            }
        else
        if( aSymbol == gCP->FCompile_ileSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpLEInteger);
            }
        else
        if( aSymbol == gCP->FCompile_ineSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpNEInteger);
            }
        else
        if( aSymbol == gCP->FCompile_neqSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpEQNumber);
            }
        else
        if( aSymbol == gCP->FCompile_ngtSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpGTNumber);
            }
        else
        if( aSymbol == gCP->FCompile_nltSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpLTNumber);
            }
        else
        if( aSymbol == gCP->FCompile_ngeSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpGENumber);
            }
        else
        if( aSymbol == gCP->FCompile_nleSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpLENumber);
            }
        else
        if( aSymbol == gCP->FCompile_nneSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, vmnatJmpNENumber);
            }
        else
        if( aSymbol == gCP->FCompile_eqSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, VMJMPEQ);
            }
        else
        if( aSymbol == gCP->FCompile_gtSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, VMJMPGT);
            }
        else
        if( aSymbol == gCP->FCompile_ltSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, VMJMPLT);
            }
        else
        if( aSymbol == gCP->FCompile_geSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, VMJMPGE);
            }
        else
        if( aSymbol == gCP->FCompile_leSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, VMJMPLE);
            }
        else
        if( aSymbol == gCP->FCompile_neSym)
            {
            tmpPair = _FCompile_CdrPair(tmpPair);
            *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, VMJMPNE);
            }
        else
        if( aSymbol == gCP->FCompile_notSym)
            {
            *ret = FOptimize2_notJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl);        
            }
        else
        if( aSymbol == gCP->FCompile_andSym)
            {
            *ret = FOptimize2_andJmp(gCP, gTP, compilerState, tmpPair, notsw, cmpsw, passLbl, branchLbl);        
            }
        else
        if( aSymbol == gCP->FCompile_orSym)
            {
            *ret = FOptimize2_orJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl);
            }
        else
            {
            /*  This is not a special form which requires special processing for optimization. */
            
            goto CompileNonlogical;
            }
        }
    }
else
    {
    CompileNonlogical:
    
    /*  If the S-expression is a singleton, then reconstruct the S-expression to be  */
    /*  (= true singleton). For instance the S-expression 1 would be reconstructed as (= true 1). */
    
    tmpPair = TPair_New(gCP,gTP);
    tmpPair->itsCar = gCP->TObject_TRUE;
    aPair = asPair(&tmpPair->itsCdr);
    aPair = TPair_New(gCP,gTP);
    asObject(&tmpPair->itsCdr) = (TObject*)aPair;
    asTag(&tmpPair->itsCdr) = TYPAIR;
    asPair(&tmpPair->itsCdr)->itsCar = curPair->itsCar;

    *ret = FOptimize2_boolJmp(gCP, gTP, compilerState, tmpPair, notsw,  cmpsw, passLbl, branchLbl, 
                                     VMJMPEQ);
    }
    
/*  The return value from specialform or function call processing is the same as  */
/*  _LastResult(compilerState) (see FCompile_SetupProc()). */

FrameExit(*ret);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_VmMemoryTarget

This is the procedure that knows how recognize process target arguments for virtual
machine memory based instructions.

#endif

TVAL    FOptimize2_VmMemoryTarget(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol, LpTVAL modifier, LpTVAL target)
{
CHAR			tmpBuffer[100];
_FCompile_StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
DeclareTVAL(tmp);
_FCompile_EndFrame

/*  Only symbols are accepted as targets of a virtual machine memory based instruction. */

if (curPair->itsCar.Tag == TYSYMBOL)
	{
	/*  This is a symbol reference, search Av Tv Pv Gv to resolve it.  */

	aSymbol = asSymbol(&curPair->itsCar);
	*ret = FCompile_LookUp(gCP, gTP, compilerState, aSymbol);
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYPAIR) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Number") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);

    /*  Is the the register name in the Rv structure of the Lambda? */
    *tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCar.u.Pair->itsCdr.u.Pair);
    _FCompile_FrameChk(*tmp);
    

	if ((_LastResult(compilerState).Modifier != AMREGISTER) || (_LastResult(compilerState).DeclaredType != TYREAL))
		{
		/*  If an explicit final result was requested, we may have to generate code for it */

		*ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *ret);
		}
	else
		{
		*ret = _LastResult(compilerState);
		}
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYVOID) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Number") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYPAIR) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Integer") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);

    /*  Is the the register name in the Rv structure of the Lambda? */
    *tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCar.u.Pair->itsCdr.u.Pair);
    _FCompile_FrameChk(*tmp);

	if ((_LastResult(compilerState).Modifier != AMREGISTER) || (_LastResult(compilerState).DeclaredType != TYNUM))
		{
		/*  If an explicit final result was requested, we may have to generate code for it */

		*ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *ret);
		}
	else
		{
		*ret = _LastResult(compilerState);
		}
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYVOID) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Integer") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYPAIR) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Word") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);

    /*  Is the the register name in the Rv structure of the Lambda? */
    *tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCar.u.Pair->itsCdr.u.Pair);
    _FCompile_FrameChk(*tmp);

	if (_LastResult(compilerState).Modifier == AMREGISTER)
		{
		/*  If an explicit final result was requested, we may have to generate code for it */

		*ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *ret);
		}
	else
		{
		*ret = _LastResult(compilerState);
		}
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYVOID) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Word") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYPAIR) && 
	(strncmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"vm",2) == 0))
	{
    /*  Is the the register name in the Rv structure of the Lambda? */
	*tmp = FOptimize2_Assembler(gCP, gTP, compilerState, curPair->itsCar.u.Pair->itsCdr.u.Pair, curPair->itsCar.u.Pair->itsCar.u.Symbol);
    //*tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCar.u.Pair);
    _FCompile_FrameChk(*tmp);

	if (_LastResult(compilerState).Modifier != AMREGISTER)
		{
		*ret = _LastResult(compilerState);
		 goto BadCleanUp;
		}
	else
		{
		*ret = _LastResult(compilerState);
		}
	}
else
	goto BadCleanUp;

/*  Format the result as an argument for FOptimize_OptimizePcode */

_LastResult(compilerState) = *ret;
_FCompile_MakeINT(*modifier,asModifier(ret));
_FCompile_MakeSYM(*target,*ret);

_FCompile_FrameExit(*ret);

BadCleanUp:
/* Construct an Error message */
strcpy(tmpBuffer, "!compile (");
strcat(tmpBuffer, (const char*)SymbolArray(TOBJ(callSymbol)));
strcat(tmpBuffer, (const char*)"): invalid target argument!");

*ret = TERROR(tmpBuffer);
_FCompile_FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_VmGotoLabel

This is the procedure that knows how recognize process goto label arguments for virtual
machine memory and register based instructions.

#endif

TVAL    FOptimize2_VmGotoLabel(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol, LpTVAL modifier, LpTVAL target)
{
CHAR			tmpBuffer[100];
_FCompile_StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
_FCompile_EndFrame

/*  Only symbols are accepted as goto labels of a virtual machine memory or register based instruction. */

if ((curPair->itsCar.Tag != TYSYMBOL) && (curPair->itsCar.Tag != TYQUOTEDSYMBOL)) goto BadCleanUp;

/*  This is a symbol reference, search Av Tv Pv Gv to resolve it.  */

aSymbol = asSymbol(&curPair->itsCar);
*ret = TOBJ(aSymbol);

    
/*  Format the result as an argument for FOptimize_OptimizePcode */

_LastResult(compilerState) = *ret;
_FCompile_MakeINT(*modifier,AMINTEGER);
_FCompile_MakeIMM(*target,-1);

_FCompile_FrameExit(*ret);

BadCleanUp:
/* Construct an Error message */
strcpy(tmpBuffer, "!compile (");
strcat(tmpBuffer, (const char*)SymbolArray(TOBJ(callSymbol)));
strcat(tmpBuffer, (const char*)"): invalid goto label argument!");

*ret = TERROR(tmpBuffer);
_FCompile_FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_VmImmediate

This is the procedure that knows how recognize process an immediate integer argument for virtual
machine memory and register based instructions.

#endif

TVAL    FOptimize2_VmImmediate(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol, LpTVAL modifier, LpTVAL target)
{
NUM				argument;
CHAR			tmpBuffer[100];
_FCompile_StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
_FCompile_EndFrame

/*  Special immediate bitshift symbols are accepted as immediate arguments of a virtual machine memory or register based instruction. */

if (curPair->itsCar.Tag == TYQUOTEDSYMBOL)
	{
	ret->Tag = TYNUM;
	if (strcmp(SymbolArray(curPair->itsCar),"abs") == 0) ret->u.Int = 0;
	else if (strcmp(SymbolArray(curPair->itsCar),"add") == 0) ret->u.Int = 0;
	else if (strcmp(SymbolArray(curPair->itsCar),"Lambda") == 0) ret->u.Int = TYLAMBDA;
	else if (strcmp(SymbolArray(curPair->itsCar),"argument") == 0) ret->u.Int = 0;
	else if (strcmp(SymbolArray(curPair->itsCar),"Boolean") == 0) ret->u.Int = TYBOLE;
	else if (strcmp(SymbolArray(curPair->itsCar),"BitVector") == 0) ret->u.Int = TYBITVECTOR;
	else if (strcmp(SymbolArray(curPair->itsCar),"Brick") == 0) ret->u.Int = TYBRICK;
	else if (strcmp(SymbolArray(curPair->itsCar),"ByteVector") == 0) ret->u.Int = TYBYTEVECTOR;
	else if (strcmp(SymbolArray(curPair->itsCar),"Character") == 0) ret->u.Int = TYCHAR;
	else if (strcmp(SymbolArray(curPair->itsCar),"CharPointer") == 0) ret->u.Int = 0;
	else if (strcmp(SymbolArray(curPair->itsCar),"cos") == 0) ret->u.Int = 1;
	else if (strcmp(SymbolArray(curPair->itsCar),"Date") == 0) ret->u.Int = TYDATE;
	else if (strcmp(SymbolArray(curPair->itsCar),"dbl") == 0) ret->u.Int = 2;
	else if (strcmp(SymbolArray(curPair->itsCar),"dec") == 0) ret->u.Int = 3;
	else if (strcmp(SymbolArray(curPair->itsCar),"Dictionary") == 0) ret->u.Int = TYDICTIONARY;
	else if (strcmp(SymbolArray(curPair->itsCar),"Directory") == 0) ret->u.Int = TYDIRECTORY;
	else if (strcmp(SymbolArray(curPair->itsCar),"dis") == 0) ret->u.Int = 0;
	else if (strcmp(SymbolArray(curPair->itsCar),"div") == 0) ret->u.Int = 1;
	else if (strcmp(SymbolArray(curPair->itsCar),"dot") == 0) ret->u.Int = 1;
	else if (strcmp(SymbolArray(curPair->itsCar),"drop") == 0) ret->u.Int = 3;
	else if (strcmp(SymbolArray(curPair->itsCar),"dup") == 0) ret->u.Int = 3;
	else if (strcmp(SymbolArray(curPair->itsCar),"eq") == 0) ret->u.Int = 2;
	else if (strcmp(SymbolArray(curPair->itsCar),"Float") == 0) ret->u.Int = TYFLOAT;
	else if (strcmp(SymbolArray(curPair->itsCar),"FloatPointer") == 0) ret->u.Int = BITSIZEOFFLOAT;
	else if (strcmp(SymbolArray(curPair->itsCar),"Function") == 0) ret->u.Int = TYCPROCEDURE;
	else if (strcmp(SymbolArray(curPair->itsCar),"ge") == 0) ret->u.Int = 4;
	else if (strcmp(SymbolArray(curPair->itsCar),"gt") == 0) ret->u.Int = 5;
	else if (strcmp(SymbolArray(curPair->itsCar),"inc") == 0) ret->u.Int = 4;
	else if (strcmp(SymbolArray(curPair->itsCar),"IntPointer") == 0) ret->u.Int = BITSIZEOFNUM;
	else if (strcmp(SymbolArray(curPair->itsCar),"Integer") == 0) ret->u.Int = TYNUM;
	else if (strcmp(SymbolArray(curPair->itsCar),"IntVector") == 0) ret->u.Int = TYINTVECTOR;
	else if (strcmp(SymbolArray(curPair->itsCar),"JumpPointer") == 0) ret->u.Int = BITSIZEOFNUM;
	else if (strcmp(SymbolArray(curPair->itsCar),"LongPointer") == 0) ret->u.Int = BITSIZEOFNUM32;
	else if (strcmp(SymbolArray(curPair->itsCar),"LongVector") == 0) ret->u.Int = TYLONGVECTOR;
	else if (strcmp(SymbolArray(curPair->itsCar),"le") == 0) ret->u.Int = 1;
	else if (strcmp(SymbolArray(curPair->itsCar),"lt") == 0) ret->u.Int = 0;
	else if (strcmp(SymbolArray(curPair->itsCar),"Matrix") == 0) ret->u.Int = TYMATRIX;
	else if (strcmp(SymbolArray(curPair->itsCar),"Money") == 0) ret->u.Int = TYMONEY;
	else if (strcmp(SymbolArray(curPair->itsCar),"mone") == 0) ret->u.Int = 4;
	else if (strcmp(SymbolArray(curPair->itsCar),"mov") == 0) ret->u.Int = 2;
	else if (strcmp(SymbolArray(curPair->itsCar),"mul") == 0) ret->u.Int = 3;
	else if (strcmp(SymbolArray(curPair->itsCar),"ne") == 0) ret->u.Int = 3;
	else if (strcmp(SymbolArray(curPair->itsCar),"Number") == 0) ret->u.Int = TYREAL;
	else if (strcmp(SymbolArray(curPair->itsCar),"NumMatrix") == 0) ret->u.Int = TYNUMMATRIX;
	else if (strcmp(SymbolArray(curPair->itsCar),"NumPointer") == 0) ret->u.Int = BITSIZEOFREAL;
	else if (strcmp(SymbolArray(curPair->itsCar),"NumVector") == 0) ret->u.Int = TYNUMVECTOR;
	else if (strcmp(SymbolArray(curPair->itsCar),"ObjPointer") == 0) ret->u.Int = BITSIZEOFNUM;
	else if (strcmp(SymbolArray(curPair->itsCar),"ObjVector") == 0) ret->u.Int = TYOBJVECTOR;
	else if (strcmp(SymbolArray(curPair->itsCar),"one") == 0) ret->u.Int = 5;
	else if (strcmp(SymbolArray(curPair->itsCar),"Pair") == 0) ret->u.Int = TYPAIR;
	else if (strcmp(SymbolArray(curPair->itsCar),"QuotedPair") == 0) ret->u.Int = TYQUOTEDPAIR;
	else if (strcmp(SymbolArray(curPair->itsCar),"QuotedSymbol") == 0) ret->u.Int = TYQUOTEDSYMBOL;
	else if (strcmp(SymbolArray(curPair->itsCar),"Short") == 0) ret->u.Int = TYSHORT;
	else if (strcmp(SymbolArray(curPair->itsCar),"ShortPointer") == 0) ret->u.Int = BITSIZEOFSHORT;
	else if (strcmp(SymbolArray(curPair->itsCar),"ShortVector") == 0) ret->u.Int = TYSHORTVECTOR;
	else if (strcmp(SymbolArray(curPair->itsCar),"sin") == 0) ret->u.Int = 5;
	else if (strcmp(SymbolArray(curPair->itsCar),"source") == 0) ret->u.Int = 1;
	else if (strcmp(SymbolArray(curPair->itsCar),"sqr") == 0) ret->u.Int = 6;
	else if (strcmp(SymbolArray(curPair->itsCar),"sqrt") == 0) ret->u.Int = 7;
	else if (strcmp(SymbolArray(curPair->itsCar),"start") == 0) ret->u.Int = 0;
	else if (strcmp(SymbolArray(curPair->itsCar),"startNoLoad") == 0) ret->u.Int = 2;
	else if (strcmp(SymbolArray(curPair->itsCar),"stop") == 0) ret->u.Int = 1;
	else if (strcmp(SymbolArray(curPair->itsCar),"stopNoSave") == 0) ret->u.Int = 3;
	else if (strcmp(SymbolArray(curPair->itsCar),"String") == 0) ret->u.Int = TYSTRING;
	else if (strcmp(SymbolArray(curPair->itsCar),"Structure") == 0) ret->u.Int = TYSTRUCTURE;
	else if (strcmp(SymbolArray(curPair->itsCar),"sub") == 0) ret->u.Int = 4;
	else if (strcmp(SymbolArray(curPair->itsCar),"sum") == 0) ret->u.Int = 2;
	else if (strcmp(SymbolArray(curPair->itsCar),"swp") == 0) ret->u.Int = 5;
	else if (strcmp(SymbolArray(curPair->itsCar),"Symbol") == 0) ret->u.Int = TYSYMBOL;
	else if (strcmp(SymbolArray(curPair->itsCar),"tan") == 0) ret->u.Int = 8;
	else if (strcmp(SymbolArray(curPair->itsCar),"target") == 0) ret->u.Int = 2;
	else if (strcmp(SymbolArray(curPair->itsCar),"Text") == 0) ret->u.Int = TYTEXT;
	else if (strcmp(SymbolArray(curPair->itsCar),"true") == 0) ret->u.Int = 6;
	else if (strcmp(SymbolArray(curPair->itsCar),"Vector") == 0) ret->u.Int = TYVECTOR;
	else if (strcmp(SymbolArray(curPair->itsCar),"Word") == 0) ret->u.Int = TYTVAL;
	else if (strcmp(SymbolArray(curPair->itsCar),"WordPointer") == 0) ret->u.Int = BITSIZEOFTVAL;
	else if (strcmp(SymbolArray(curPair->itsCar),"zero") == 0) ret->u.Int = 6;

	else goto BadCleanUp;
	
	argument = ret->u.Int;
	goto Last;
	}

/*  Only integers are accepted as immediate arguments of a virtual machine memory or register based instruction. */

if (curPair->itsCar.Tag != TYNUM) goto BadCleanUp;

/*  This is a symbol reference, search Av Tv Pv Gv to resolve it.  */

argument = curPair->itsCar.u.Int;
*ret = TINT(argument);

    
/*  Format the result as an argument for FOptimize_OptimizePcode */
Last:
_LastResult(compilerState) = *ret;
_FCompile_MakeINT(*modifier,AMINTEGER);
_FCompile_MakeIMM(*target,argument);

_FCompile_FrameExit(*ret);

BadCleanUp:
/* Construct an Error message */
strcpy(tmpBuffer, "!compile (");
strcat(tmpBuffer, (const char*)SymbolArray(TOBJ(callSymbol)));
strcat(tmpBuffer, (const char*)"): invalid immediate integer argument!");

*ret = TERROR(tmpBuffer);
_FCompile_FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_VmMemoryArg

This is the procedure that knows how recognize process standard arguments for virtual
machine memory based instructions.

#endif

TVAL    FOptimize2_VmMemoryArg(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol, LpTVAL modifier, LpTVAL target)
{
NUM 			envIndex;
NUM 			cVarsLen;
CHAR			tmpBuffer[100];
_FCompile_StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TStructure,cVars);
DeclareTVAL(final);
DeclareTVAL(ret);
DeclareTVAL(tmp);
_FCompile_EndFrame

/*  Symbols are accepted as arguments of virtual machine memory based instruction. */

*final = _Result(compilerState);
if (curPair->itsCar.Tag == TYSYMBOL)
	{
	aSymbol = asSymbol(&curPair->itsCar);
	*ret = FCompile_LookUp(gCP, gTP, compilerState, aSymbol);
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYPAIR) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Number") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);

    /*  Is the the register name in the Rv structure of the Lambda? */
    *tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCar.u.Pair->itsCdr.u.Pair);
    _FCompile_FrameChk(*tmp);
    

	if ((_LastResult(compilerState).Modifier != AMREGISTER) || (_LastResult(compilerState).DeclaredType != TYREAL))
		{
		/*  If an explicit final result was requested, we may have to generate code for it */

		*ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *ret);
		}
	else
		{
		*ret = _LastResult(compilerState);
		}
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYVOID) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Number") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYPAIR) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Integer") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);

    /*  Is the the register name in the Rv structure of the Lambda? */
    *tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCar.u.Pair->itsCdr.u.Pair);
    _FCompile_FrameChk(*tmp);

	if ((_LastResult(compilerState).Modifier != AMREGISTER) || (_LastResult(compilerState).DeclaredType != TYNUM))
		{
		/*  If an explicit final result was requested, we may have to generate code for it */

		*ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *ret);
		}
	else
		{
		*ret = _LastResult(compilerState);
		}
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYVOID) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Integer") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYPAIR) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Word") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);

    /*  Is the the register name in the Rv structure of the Lambda? */
    *tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCar.u.Pair->itsCdr.u.Pair);
    _FCompile_FrameChk(*tmp);

	if (_LastResult(compilerState).Modifier == AMREGISTER)
		{
		/*  If an explicit final result was requested, we may have to generate code for it */

		*ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *ret);
		}
	else
		{
		*ret = _LastResult(compilerState);
		}
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYVOID) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Word") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYPAIR) && 
	(strncmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"vm",2) == 0))
	{
    /*  Is the the register name in the Rv structure of the Lambda? */
	*tmp = FOptimize2_Assembler(gCP, gTP, compilerState, curPair->itsCar.u.Pair->itsCdr.u.Pair, curPair->itsCar.u.Pair->itsCar.u.Symbol);
    //*tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCar.u.Pair);
    _FCompile_FrameChk(*tmp);

	if (_LastResult(compilerState).Modifier != AMREGISTER)
		{
		*ret = _LastResult(compilerState);
		 goto BadCleanUp;
		}
	else
		{
		*ret = _LastResult(compilerState);
		}
	}
else
	{
    /*  Anything else is treated as a constant.         */
    /*  Note: If not already in the constant structure, */
    /*        Place the constant into the Cv Structure. */
    if (_Cv(compilerState) == NIL)
        {
        _Cv(compilerState) = TStructure_New(gCP,gTP);
        }

    /*  Try to find a duplicate constant in the Cv Structure. */
    
    cVars = _Cv(compilerState);
    cVarsLen = cVars->itsMaxItemIndex;
    for (envIndex = 0; envIndex < cVarsLen; ++envIndex)
        {
        if (strncmp((char*)*((TSymbol*)atHMBind(cVars->itsDictionaryArray,envIndex).Key)->itsCString,"__C",3) == 0)
            {
            *tmp = FUtil1_CmpConstants(gCP, gTP, atHMBind(cVars->itsDictionaryArray,envIndex).Value, curPair->itsCar);
            if (isCompareEQ(tmp))
                goto BindConstant;
            }
        }
    
    /*  Do we need to add a new constant to the Cv Structure ? */
    BindConstant:
    if (envIndex == cVarsLen)
        {
        TStructure_SetMaxIndex(gCP, gTP, TOBJ(_Cv(compilerState)), cVarsLen + 1);
        sprintf((char*)tmpBuffer, "__C%ld", (LONG)(gTP->FCompile_tmpCount = ++gTP->FCompile_tmpCount & 0xFFFF));
        atHMBind(cVars->itsDictionaryArray,cVarsLen).Key = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)tmpBuffer);
        atHMBind(cVars->itsDictionaryArray,cVarsLen).Value = curPair->itsCar;
        }
	ret->u.Symbol = (TSymbol*)atHMBind(cVars->itsDictionaryArray,envIndex).Key;
	ret->Tag = atHMBind(cVars->itsDictionaryArray,envIndex).Key->itsObjectType;
	asOffset(ret) = envIndex;
	asModifier(ret) = AMCVOFFSET;
	}
    
/*  Format the result as an argument for FOptimize_OptimizePcode */

_LastResult(compilerState) = *ret;
_FCompile_MakeINT(*modifier,asModifier(ret));
_FCompile_MakeSYM(*target,*ret);

_Result(compilerState) = *final;
_FCompile_FrameExit(*ret);

BadCleanUp:
/* Construct an Error message */
strcpy(tmpBuffer, "!compile (");
strcat(tmpBuffer, (const char*)SymbolArray(TOBJ(callSymbol)));
strcat(tmpBuffer, (const char*)"): invalid argument list!");

_Result(compilerState) = *final;
*ret = TERROR(tmpBuffer);
_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_VmRegisterArg

This is the procedure that knows how recognize process register arguments for virtual
machine register based instructions.

#endif

TVAL    FOptimize2_VmRegisterArg(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol, LpTVAL modifier, LpTVAL target)
{
NUM 			envIndex;
NUM 			rVarsLen;
CHAR			tmpBuffer[100];
_FCompile_StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TStructure,rVars);
DeclareTVAL(final);
DeclareTVAL(ret);
DeclareTVAL(tmp);
_FCompile_EndFrame

/*  Only symbols are accepted as register arguments for virtual machine register based instruction. */

*final = _Result(compilerState);
if (curPair->itsCar.Tag == TYSYMBOL)
	{
    /*  Is the the register name in the Rv structure of the Lambda? */
	aSymbol = asSymbol(&curPair->itsCar);
    if (_Rv(compilerState) != NIL)
        {
		rVars = _Rv(compilerState);
		rVarsLen = rVars->itsMaxItemIndex;
		for (envIndex = 0; envIndex < rVarsLen; ++envIndex)
			{
			if (aSymbol == ((TSymbol*)atHMBind(rVars->itsDictionaryArray,envIndex).Key))
				{
				goto BindRegister;
				}
			}
        }
	goto BadCleanUp;

    /*  We bind the register argument to its location in the register structure. */
    BindRegister:
	ret->u.Symbol = (TSymbol*)atHMBind(rVars->itsDictionaryArray,envIndex).Key;
	ret->Tag = atHMBind(rVars->itsDictionaryArray,envIndex).Key->itsObjectType;
	asOffset(ret) = envIndex;
	asModifier(ret) = AMREGISTER;
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYPAIR) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Number") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);

    /*  Is the the register name in the Rv structure of the Lambda? */
    *tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCar.u.Pair->itsCdr.u.Pair);
    _FCompile_FrameChk(*tmp);
    

	if ((_LastResult(compilerState).Modifier != AMREGISTER) || (_LastResult(compilerState).DeclaredType != TYREAL))
		{
		/*  If an explicit final result was requested, we may have to generate code for it */

		*ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *ret);
		}
	else
		{
		*ret = _LastResult(compilerState);
		}
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYVOID) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Number") == 0))
	{
	/* Allocate a register variable */
	_LastResult(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYPAIR) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Integer") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);

    /*  Is the the register name in the Rv structure of the Lambda? */
    *tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCar.u.Pair->itsCdr.u.Pair);
    _FCompile_FrameChk(*tmp);

	if ((_LastResult(compilerState).Modifier != AMREGISTER) || (_LastResult(compilerState).DeclaredType != TYNUM))
		{
		/*  If an explicit final result was requested, we may have to generate code for it */

		*ret = FSpecialForms1_CodeMove(gCP, gTP, compilerState, _LastResult(compilerState), *ret);
		}
	else
		{
		*ret = _LastResult(compilerState);
		}
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYVOID) && 
	(strcmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"Integer") == 0))
	{
	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);
	}
else
if ((curPair->itsCar.Tag == TYPAIR) && 
	(curPair->itsCar.u.Pair->itsCar.Tag == TYSYMBOL) && 
	(curPair->itsCar.u.Pair->itsCdr.Tag == TYPAIR) && 
	(strncmp(SymbolArray(curPair->itsCar.u.Pair->itsCar),"vm",2) == 0))
	{
    /*  Is the the register name in the Rv structure of the Lambda? */
	*tmp = FOptimize2_Assembler(gCP, gTP, compilerState, curPair->itsCar.u.Pair->itsCdr.u.Pair, curPair->itsCar.u.Pair->itsCar.u.Symbol);
    //*tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCar.u.Pair);
    _FCompile_FrameChk(*tmp);

	if (_LastResult(compilerState).Modifier != AMREGISTER)
		{
		*ret = _LastResult(compilerState);
		 goto BadCleanUp;
		}
	else
		{
		*ret = _LastResult(compilerState);
		}
	}
else
	goto BadCleanUp; 
    
/*  Format the result as an argument for FOptimize_OptimizePcode */

_LastResult(compilerState) = *ret;
_FCompile_MakeINT(*modifier,asModifier(ret));
_FCompile_MakeSYM(*target,*ret);

_Result(compilerState) = *final;
_FCompile_FrameExit(*ret);

BadCleanUp:
/* Construct an Error message */
strcpy(tmpBuffer, "!compile (");
strcat(tmpBuffer, (const char*)SymbolArray(TOBJ(callSymbol)));
strcat(tmpBuffer, (const char*)"): invalid register name in argument list!");

*ret = TERROR(tmpBuffer);
_Result(compilerState) = *final;
_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize2_Assembler

This is the dispatch routine for optimizing VM assembler instructions. Returns TObject_TRUE 
if this sub-list was a VM assembler instruction, and TObject_FALSE if it should be handled 
by the normal compiler call processing.

When we enter this routine we are given the symbol for the call and the curPair is positioned at
the first element of the list after the call symbol.

For example:

    (vmregAddInteger r1 r2)
					 ^ The curpair would be positioned here at entry.

#endif

TVAL FOptimize2_Assembler(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TSymbol* callSymbol)
{
TVAL		cs;
LpCHAR		csPtr;
NUM			csLen;
UNUM		instruction;
CHAR		tmpBuffer[100];
StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPcodeVector,Pc);
DeclareTVAL(ret);
DeclareTVAL(label);
DeclareOBJArray(TPair,pairs,20);
DeclareTVALArray(prmv,8);
EndFrame

/*  Initialize the call symbol and the call symbol data pointer. */

Pc = _Pc(compilerState);
cs.u.Symbol = callSymbol;
cs.Tag = TYSYMBOL;
csPtr = &SymbolArray(cs)[0];
csLen = callSymbol->itsMaxItemIndex;
_FCompile_MakeINT(prmv[2], AMVOID);
_FCompile_MakeINT(prmv[3], AMVOID);
_FCompile_MakeINT(prmv[4], AMVOID);
    
/*  We flatten the input argument list so that we may directly index each argument. */
    
_FMacro_PairsToArray(curPair, pairs, sizeof(pairs)/sizeof(pairs[0]));

/*  The call symbol must begin with "vm" if this is an Assembler instruction. */

if ((csLen <= 2) || (csPtr[0] != 'v') || (csPtr[1] != 'm')) goto NotAssembler;

/*  Use the remainder of the call symbol to determine if this is an Assembler instruction. */

switch (csPtr[2])
	{
	case 'a':
		if (strcmp(csPtr,"vmadd") == 0) {instruction = VMADD; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmaddi") == 0) {instruction = VMADDI;  goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmaddn") == 0) {instruction = VMADDN;  goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmand") == 0) {instruction = VMAND;  goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmapply") == 0) {instruction = VMAPPLY; goto MemoryImmTwoArgs;}
		else
		if (strcmp(csPtr,"vmargcount") == 0) {instruction = VMARGCOUNT;  goto MemoryOneArg;}
		else
		if (strcmp(csPtr,"vmargfetch") == 0) {instruction = VMARGFETCH;  goto MemoryTwoArgs;}

		goto NotAssembler;
		break;

	case 'c':
		if (strcmp(csPtr,"vmcadd") == 0) {instruction = VMCADD; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmcall") == 0) {instruction = VMCALL; goto MemoryImmTwoArgs;}
		else
		if (strcmp(csPtr,"vmcallarg") == 0) {instruction = VMCALLARG; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmcdiv") == 0) {instruction = VMCDIV; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmcmul") == 0) {instruction = VMCMUL; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmcsub") == 0) {instruction = VMCSUB; goto MemoryThreeArgs;}
		else

		goto NotAssembler;
		break;

	case 'd':
		if (strcmp(csPtr,"vmdebugger") == 0) {instruction = VMDEBUGGER; goto MemoryZeroArgs;}
		else
		if (strcmp(csPtr,"vmdiv") == 0) {instruction = VMDIV; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmdivi") == 0) {instruction = VMDIVI; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmdivn") == 0) {instruction = VMDIVN; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmdivr") == 0) {instruction = VMDIVR; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmdivri") == 0) {instruction = VMDIVRI; goto MemoryThreeArgs;}
		else

		goto NotAssembler;
		break;

	case 'i':
		if (strcmp(csPtr,"vmiadd") == 0) {instruction = VMIADD; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmidiv") == 0) {instruction = VMIDIV; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmidivr") == 0) {instruction = VMIDIVR; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmimul") == 0) {instruction = VMIMUL; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmisub") == 0) {instruction = VMISUB; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmiand") == 0) {instruction = VMIAND; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmiandb") == 0) {instruction = VMIANDB; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmior") == 0) {instruction = VMIOR; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmiorb") == 0) {instruction = VMIORB; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmixor") == 0) {instruction = VMIXOR; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmixorb") == 0) {instruction = VMIXORB; goto MemoryThreeArgs;}
		else

		goto NotAssembler;
		break;

	case 'j':
		if (strcmp(csPtr,"vmjmpeq") == 0) {instruction = VMJMPEQ; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmjmplt") == 0) {instruction = VMJMPLT; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmjmpgt") == 0) {instruction = VMJMPGT; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmjmpne") == 0) {instruction = VMJMPNE; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmjmpge") == 0) {instruction = VMJMPGE; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmjmple") == 0) {instruction = VMJMPLE; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmjump") == 0) {instruction = VMJUMP; goto MemoryJump;}
		else

		goto NotAssembler;
		break;

	case 'm':
		if (strcmp(csPtr,"vmmove") == 0) {instruction = VMMOVE; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmmovei") == 0) {instruction = VMMOVEI; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmmoven") == 0) {instruction = VMMOVEN; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmmul") == 0) {instruction = VMMUL; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmmuli") == 0) {instruction = VMMULI; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmmuln") == 0) {instruction = VMMULN; goto MemoryThreeArgs;}
		else

		goto NotAssembler;
		break;

	case 'n':
		if (strcmp(csPtr,"vmnadd") == 0) {instruction = VMNADD; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmndiv") == 0) {instruction = VMNDIV; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmndivr") == 0) {instruction = VMNDIVR; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnmul") == 0) {instruction = VMNMUL; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnsub") == 0) {instruction = VMNSUB; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatJmpEQNumber") == 0) {instruction = vmnatJmpEQNumber; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnatJmpLTNumber") == 0) {instruction = vmnatJmpLTNumber; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnatJmpGTNumber") == 0) {instruction = vmnatJmpGTNumber; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnatJmpNENumber") == 0) {instruction = vmnatJmpNENumber; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnatJmpGENumber") == 0) {instruction = vmnatJmpGENumber; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnatJmpLENumber") == 0) {instruction = vmnatJmpLENumber; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnatAddInteger") == 0) {instruction = vmnatAddInteger; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatAndInteger") == 0) {instruction = vmnatAndInteger; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatAddNumber") == 0) {instruction = vmnatAddNumber; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatDivInteger") == 0) {instruction = vmnatDivInteger; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatDivNumber") == 0) {instruction = vmnatDivNumber; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatDivrInteger") == 0) {instruction = vmnatDivrInteger; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatDivrNumber") == 0) {instruction = vmnatDivrNumber; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatMulInteger") == 0) {instruction = vmnatMulInteger; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatMulNumber") == 0) {instruction = vmnatMulNumber; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatOrInteger") == 0) {instruction = vmnatOrInteger; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatShlInteger") == 0) {instruction = vmnatShlInteger; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatShrInteger") == 0) {instruction = vmnatShrInteger; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatSubInteger") == 0) {instruction = vmnatSubInteger; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatXorInteger") == 0) {instruction = vmnatXorInteger; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatSubNumber") == 0) {instruction = vmnatSubNumber; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmnatLoadCharacter") == 0) {instruction = vmnatLoadCharacter; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatLoadFloat") == 0) {instruction = vmnatLoadFloat; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatLoadInteger") == 0) {instruction = vmnatLoadInteger; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatLoadLong") == 0) {instruction = vmnatLoadLong; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatLoadNumber") == 0) {instruction = vmnatLoadNumber; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatLoadObject") == 0) {instruction = vmnatLoadObject; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatLoadShort") == 0) {instruction = vmnatLoadShort; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatSaveCharacter") == 0) {instruction = vmnatSaveCharacter; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatSaveFloat") == 0) {instruction = vmnatSaveFloat; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatSaveInteger") == 0) {instruction = vmnatSaveInteger; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatSaveLong") == 0) {instruction = vmnatSaveLong; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatSaveNumber") == 0) {instruction = vmnatSaveNumber; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatSaveObject") == 0) {instruction = vmnatSaveObject; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatSaveShort") == 0) {instruction = vmnatSaveShort; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmnatJmpEQInteger") == 0) {instruction = vmnatJmpEQInteger; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnatJmpLTInteger") == 0) {instruction = vmnatJmpLTInteger; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnatJmpGTInteger") == 0) {instruction = vmnatJmpGTInteger; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnatJmpNEInteger") == 0) {instruction = vmnatJmpNEInteger; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnatJmpGEInteger") == 0) {instruction = vmnatJmpGEInteger; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnatJmpLEInteger") == 0) {instruction = vmnatJmpLEInteger; goto MemoryJumpCC;}
		else
		if (strcmp(csPtr,"vmnop") == 0) 
			{
			/* This instruction is a compiler directive only. */
			/* Note: no VM code is generated.                 */
				
			FrameExit(gCP->Tval_TRUE);
			}

		goto NotAssembler;
		break;

	case 'o':
		if (strcmp(csPtr,"vmonerror") == 0) {instruction = VMONERROR; goto MemoryTwoArgs;}
		else
		if (strcmp(csPtr,"vmor") == 0) {instruction = VMOR; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmopt") == 0) 
			{
			/* This instruction is a compiler directive only. */
			/* Note: no VM code is generated.                 */
			if ((pairs[0]->itsCar.Tag == TYQUOTEDSYMBOL) && (strcmp(SymbolArray(pairs[0]->itsCar),"start") == 0)) gTP->FCompile_OptimizeSW = TRUE;
			else
			if ((pairs[0]->itsCar.Tag == TYSYMBOL) && (strcmp(SymbolArray(pairs[0]->itsCar),"start") == 0)) gTP->FCompile_OptimizeSW = TRUE;
			else
			if ((pairs[0]->itsCar.Tag == TYNUM) && (pairs[0]->itsCar.u.Int == 1)) gTP->FCompile_OptimizeSW = TRUE;
			else
			if ((pairs[0]->itsCar.Tag == TYQUOTEDSYMBOL) && (strcmp(SymbolArray(pairs[0]->itsCar),"stop") == 0)) gTP->FCompile_OptimizeSW = FALSE;
			else
			if ((pairs[0]->itsCar.Tag == TYSYMBOL) && (strcmp(SymbolArray(pairs[0]->itsCar),"stop") == 0)) gTP->FCompile_OptimizeSW = FALSE;
			else
			if ((pairs[0]->itsCar.Tag == TYNUM) && (pairs[0]->itsCar.u.Int == 0)) gTP->FCompile_OptimizeSW = FALSE;
			else
				goto BadCleanUp;
				
			FrameExit(gCP->Tval_TRUE);
			}
		else

		goto NotAssembler;
		break;

	case 'p':
		if (strcmp(csPtr,"vmpop") == 0) 
			{
			instruction = VMPOP;
			if (pairs[1] == NULL) goto MemoryOneArg;
			else
			if (pairs[2] == NULL) goto MemoryTwoArgs;
			else
			goto MemoryThreeArgs;
			}
		else
		if (strcmp(csPtr,"vmpush") == 0) 
			{
			instruction = VMPUSH;
			if (pairs[1] == NULL) goto MemoryOneArg;
			else
			if (pairs[2] == NULL) goto MemoryTwoArgs;
			else
			goto MemoryThreeArgs;
			}
		else

		goto NotAssembler;
		break;

	case 'r':
		if (strcmp(csPtr,"vmref") == 0) 
			{
			if (pairs[2] == NULL) 
				{
				instruction = VMMOVE;
				goto MemoryTwoArgs;
				}
			else
				{
				instruction = VMREF;
				goto MemoryThreeArgs;
				}
			}
		else
		if (strncmp(csPtr,"vmref",5) == 0) 
			{
			if (strcmp(csPtr,"vmrefbitvector") == 0) {instruction = VMREFBITVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefbytevector") == 0) {instruction = VMREFBYTVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefdicvalue") == 0) {instruction = VMREFDICVALUE; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefdickey") == 0) {instruction = VMREFDICKEY; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefdirvalue") == 0) {instruction = VMREFDIRVALUE; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefdirkey") == 0) {instruction = VMREFDIRKEY; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmreffltvector") == 0) {instruction = VMREFFLTVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefnumvector") == 0) {instruction = VMREFNUMVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefintvector") == 0) {instruction = VMREFINTVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefmatrix") == 0) {instruction = VMREFMATRIX; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefnummatrix") == 0) {instruction = VMREFNUMMATRIX; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefobjvector") == 0) {instruction = VMREFOBJVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefpcdvector") == 0) {instruction = VMREFPCDVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefstring") == 0) {instruction = VMREFSTRING; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefstrkey") == 0) {instruction = VMREFSTRKEY; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefsymbol") == 0) {instruction = VMREFSYMBOL; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefstrvalue") == 0) {instruction = VMREFSTRVALUE; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmreftext") == 0) {instruction = VMREFTEXT; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmrefvector") == 0) {instruction = VMREFVECTOR; goto MemoryThreeArgs;}
			else

			goto NotAssembler;
			}
		else
		if (strncmp(csPtr,"vmreg",5) == 0) 
			{
			switch (csPtr[5])
				{
				case 'A':
					if (strcmp(csPtr,"vmregAbsNumber") == 0) {instruction = vmregAbsNumber; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregAddImmediate") == 0) {instruction = vmregAddImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregAddInteger") == 0) {instruction = vmregAddInteger; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregAddNumber") == 0) {instruction = vmregAddNumber; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregAddPointer") == 0) {instruction = vmregAddPointer; goto Register_ShlRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregAndImmediate") == 0) {instruction = vmregAndImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregAndInteger") == 0) {instruction = vmregAndInteger; goto Register_RegRegArgs;}
					else

					goto NotAssembler;
					break;

				case 'C':
					if (strcmp(csPtr,"vmregCosNumber") == 0) {instruction = vmregCosNumber; goto Register_RegRegArgs;}
					else

					goto NotAssembler;
					break;

				case 'D':
					if (strcmp(csPtr,"vmregDivImmediate") == 0) {instruction = vmregDivImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregDivInteger") == 0) {instruction = vmregDivInteger; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregDivNumber") == 0) {instruction = vmregDivNumber; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregDivrImmediate") == 0) {instruction = vmregDivrImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregDivrInteger") == 0) {instruction = vmregDivrInteger; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregDivrNumber") == 0) {instruction = vmregDivrNumber; goto Register_RegRegArgs;}
					else

					goto NotAssembler;
					break;

				case 'I':
					if (strcmp(csPtr,"vmregIncPointer") == 0) {instruction = vmregIncPointer; goto Register_ShlImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregInteger") == 0) {instruction = vmregInteger; goto Register_RegRegArgs;}
					else

					goto NotAssembler;
					break;

				case 'J':
					if (strcmp(csPtr,"vmregJmpLTImmediate") == 0) {instruction = vmregJmpLTImmediate; goto RegisterImm_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpLEImmediate") == 0) {instruction = vmregJmpLEImmediate; goto RegisterImm_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpEQImmediate") == 0) {instruction = vmregJmpEQImmediate; goto RegisterImm_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpNEImmediate") == 0) {instruction = vmregJmpNEImmediate; goto RegisterImm_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpGEImmediate") == 0) {instruction = vmregJmpGEImmediate; goto RegisterImm_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpGTImmediate") == 0) {instruction = vmregJmpGTImmediate; goto RegisterImm_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpLTInteger") == 0) {instruction = vmregJmpLTInteger; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpLEInteger") == 0) {instruction = vmregJmpLEInteger; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpEQInteger") == 0) {instruction = vmregJmpEQInteger; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpNEInteger") == 0) {instruction = vmregJmpNEInteger; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpGEInteger") == 0) {instruction = vmregJmpGEInteger; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpGTInteger") == 0) {instruction = vmregJmpGTInteger; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpLTNumber") == 0) {instruction = vmregJmpLTNumber; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpLENumber") == 0) {instruction = vmregJmpLENumber; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpEQNumber") == 0) {instruction = vmregJmpEQNumber; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpNENumber") == 0) {instruction = vmregJmpNENumber; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpGENumber") == 0) {instruction = vmregJmpGENumber; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJmpGTNumber") == 0) {instruction = vmregJmpGTNumber; goto Register_JumpCC;}
					else
					if (strcmp(csPtr,"vmregJump") == 0) {instruction = vmregJump; goto Register_RegArg;}
					else

					goto NotAssembler;
					break;

				case 'L':
					if (strcmp(csPtr,"vmregLoadAddress") == 0) {instruction = vmregLoadAddress; goto Register_MemRegArgs;}
					else
					if (strcmp(csPtr,"vmregLoadInteger") == 0) {instruction = vmregLoadInteger; goto Register_MemRegArgs;}
					else
					if (strcmp(csPtr,"vmregLoadTail") == 0) {instruction = vmregLoadTail; goto Register_MemRegArgs;}
					else
					if (strcmp(csPtr,"vmregLoadDeclType") == 0) {instruction = vmregLoadDeclType; goto Register_MemRegArgs;}
					else
					if (strcmp(csPtr,"vmregLoadType") == 0) {instruction = vmregLoadType; goto Register_MemRegArgs;}
					else
					if (strcmp(csPtr,"vmregLoadJmpPointer") == 0) {instruction = vmregLoadJmpPointer; goto Register_LblRegArgs;}
					else
					if (strcmp(csPtr,"vmregLoadNumber") == 0) {instruction = vmregLoadNumber; goto Register_MemRegArgs;}
					else
					if (strcmp(csPtr,"vmregLogNumber") == 0) {instruction = vmregLogNumber; goto Register_RegRegRegArgs;}
					else

					goto NotAssembler;
					break;

				case 'M':
					if (strcmp(csPtr,"vmregMoveImmediate") == 0) {instruction = vmregMoveImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregMoveInteger") == 0) {instruction = vmregMoveInteger; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregMoveNumber") == 0) {instruction = vmregMoveNumber; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregMulImmediate") == 0) {instruction = vmregMulImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregMulInteger") == 0) {instruction = vmregMulInteger; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregMulNumber") == 0) {instruction = vmregMulNumber; goto Register_RegRegArgs;}
					else

					goto NotAssembler;
					break;

				case 'N':
					if (strcmp(csPtr,"vmregNumber") == 0) {instruction = vmregNumber; goto Register_RegRegArgs;}
					else

					goto NotAssembler;
					break;

				case 'O':
					if (strcmp(csPtr,"vmregObjLength") == 0) {instruction = vmregObjLength; goto Register_MemRegArgs;}
					else
					if (strcmp(csPtr,"vmregObjPointer") == 0) {instruction = vmregObjPointer; goto Register_MemRegArgs;}
					else
					if (strcmp(csPtr,"vmregOrImmediate") == 0) {instruction = vmregOrImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregOrInteger") == 0) {instruction = vmregOrInteger; goto Register_RegRegArgs;}
					else

					goto NotAssembler;
					break;

				case 'P':
					if (strcmp(csPtr,"vmregPwrNumber") == 0) {instruction = vmregPwrNumber; goto Register_RegRegRegArgs;}
					else

					goto NotAssembler;
					break;


				case 'R':
					if (strcmp(csPtr,"vmregRefCharacter") == 0) {instruction = vmregRefCharacter; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefFloat") == 0) {instruction = vmregRefFloat; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefInteger") == 0) {instruction = vmregRefInteger; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefNumber") == 0) {instruction = vmregRefNumber; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefShort") == 0) {instruction = vmregRefShort; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefLong") == 0) {instruction = vmregRefLong; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefWord") == 0) {instruction = vmregRefWord; goto Register_RegMemArgs;}
					else
					if (strcmp(csPtr,"vmregRefXCharacter") == 0) {instruction = vmregRefXCharacter; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefXFloat") == 0) {instruction = vmregRefXFloat; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefXLong") == 0) {instruction = vmregRefXLong; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefXInteger") == 0) {instruction = vmregRefXInteger; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefXNumber") == 0) {instruction = vmregRefXNumber; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefXShort") == 0) {instruction = vmregRefXShort; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregRefXWord") == 0) {instruction = vmregRefXWord; goto Register_RegRegMemArgs;}
					else
					if (strcmp(csPtr,"vmregRunInHardware") == 0) {instruction = vmregRunInHardware; goto Register_ImmArg;}
					else

					goto NotAssembler;
					break;

				case 'S':
					if (strcmp(csPtr,"vmregSinNumber") == 0) {instruction = vmregSinNumber; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSqrtNumber") == 0) {instruction = vmregSqrtNumber; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregStringCompare") == 0) {instruction = vmregStringCompare; goto Register_MemMemRegArgs;}
					else
					if (strcmp(csPtr,"vmregStringiCompare") == 0) {instruction = vmregStringiCompare; goto Register_MemMemRegArgs;}
					else
					if (strcmp(csPtr,"vmregSaveInteger") == 0) {instruction = vmregSaveInteger; goto Register_RegMemArgs;}
					else
					if (strcmp(csPtr,"vmregSaveTail") == 0) {instruction = vmregSaveTail; goto Register_RegMemArgs;}
					else
					if (strcmp(csPtr,"vmregSaveTailImmediate") == 0) {instruction = vmregSaveTailImmediate; goto Register_ImmMemArgs;}
					else
					if (strcmp(csPtr,"vmregSaveDeclType") == 0) {instruction = vmregSaveDeclType; goto Register_RegMemArgs;}
					else
					if (strcmp(csPtr,"vmregSaveDeclTypeImmediate") == 0) {instruction = vmregSaveDeclTypeImmediate; goto Register_ImmMemArgs;}
					else
					if (strcmp(csPtr,"vmregSaveNumber") == 0) {instruction = vmregSaveNumber; goto Register_RegMemArgs;}
					else
					if (strcmp(csPtr,"vmregSetXCharImmediate") == 0) {instruction = vmregSetXCharImmediate; goto Register_ImmRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetXIntImmediate") == 0) {instruction = vmregSetXIntImmediate; goto Register_ImmRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetXLongImmediate") == 0) {instruction = vmregSetXLongImmediate; goto Register_ImmRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetXShortImmediate") == 0) {instruction = vmregSetXShortImmediate; goto Register_ImmRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetCharImmediate") == 0) {instruction = vmregSetCharImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetIntImmediate") == 0) {instruction = vmregSetIntImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetLongImmediate") == 0) {instruction = vmregSetLongImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetShortImmediate") == 0) {instruction = vmregSetShortImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetCharacter") == 0) {instruction = vmregSetCharacter; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetFloat") == 0) {instruction = vmregSetFloat; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetInteger") == 0) {instruction = vmregSetInteger; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetNumber") == 0) {instruction = vmregSetNumber; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetShort") == 0) {instruction = vmregSetShort; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetLong") == 0) {instruction = vmregSetLong; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetWord") == 0) {instruction = vmregSetWord; goto Register_MemRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetXCharacter") == 0) {instruction = vmregSetXCharacter; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetXFloat") == 0) {instruction = vmregSetXFloat; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetXInteger") == 0) {instruction = vmregSetXInteger; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetXLong") == 0) {instruction = vmregSetXLong; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetXNumber") == 0) {instruction = vmregSetXNumber; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetXShort") == 0) {instruction = vmregSetXShort; goto Register_RegRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSetXWord") == 0) {instruction = vmregSetXWord; goto Register_MemRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregShlImmediate") == 0) {instruction = vmregShlImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregShlInteger") == 0) {instruction = vmregShlInteger; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregShrImmediate") == 0) {instruction = vmregShrImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregShrInteger") == 0) {instruction = vmregShrInteger; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSubImmediate") == 0) {instruction = vmregSubImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregSubInteger") == 0) {instruction = vmregSubInteger; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSubNumber") == 0) {instruction = vmregSubNumber; goto Register_RegRegArgs;}
					else
					if (strcmp(csPtr,"vmregSubPointer") == 0) {instruction = vmregSubPointer; goto Register_ShlRegRegArgs;}
					else
					if (strcmp(csPtr,"vmregTanNumber") == 0) {instruction = vmregTanNumber; goto Register_RegRegArgs;}
					else

					goto NotAssembler;
					break;

				case 'T':
					if (strcmp(csPtr,"vmregTanNumber") == 0) {instruction = vmregTanNumber; goto Register_RegRegArgs;}
					else

					goto NotAssembler;
					break;

				case 'X':
					if (strcmp(csPtr,"vmregXorImmediate") == 0) {instruction = vmregXorImmediate; goto Register_ImmRegArgs;}
					else
					if (strcmp(csPtr,"vmregXorInteger") == 0) {instruction = vmregXorInteger; goto Register_RegRegArgs;}
					else

					goto NotAssembler;
					break;

				}

			goto NotAssembler;
			}
		else
		if (strcmp(csPtr,"vmreturn") == 0) {instruction = VMRETURN; goto MemoryOneArg;}
		else

		goto NotAssembler;
		break;

	case 's':
		if (strcmp(csPtr,"vmself") == 0) {instruction = VMSELF; goto MemoryOneArg;}
		else
		if (strcmp(csPtr,"vmsend") == 0) {instruction = VMSEND; goto MemoryImmTwoArgs;}
		else
		if (strcmp(csPtr,"vmset") == 0) {instruction = VMSET; goto MemoryThreeArgs;}
		else
		if (strncmp(csPtr,"vmset",5) == 0) 
			{
			if (strcmp(csPtr,"vmsetbitvector") == 0) {instruction = VMSETBITVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetbytevector") == 0) {instruction = VMSETBYTVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetdicvalue") == 0) {instruction = VMSETDICVALUE; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetdickey") == 0) {instruction = VMSETDICKEY; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetdirvalue") == 0) {instruction = VMSETDIRVALUE; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetdirkey") == 0) {instruction = VMSETDIRKEY; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetfltvector") == 0) {instruction = VMSETFLTVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetnumvector") == 0) {instruction = VMSETNUMVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetintvector") == 0) {instruction = VMSETINTVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetmatrix") == 0) {instruction = VMSETMATRIX; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetnummatrix") == 0) {instruction = VMSETNUMMATRIX; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetobjvector") == 0) {instruction = VMSETOBJVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetpcdvector") == 0) {instruction = VMSETPCDVECTOR; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetstring") == 0) {instruction = VMSETSTRING; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetstrkey") == 0) {instruction = VMSETSTRKEY; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetstrvalue") == 0) {instruction = VMSETSTRVALUE; goto MemoryThreeArgs;}
			else
			if (strcmp(csPtr,"vmsetvector") == 0) {instruction = VMSETVECTOR; goto MemoryThreeArgs;}
			else

			goto NotAssembler;
			}
		else
		if (strcmp(csPtr,"vmshl") == 0) {instruction = VMSHL; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmshr") == 0) {instruction = VMSHR; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmsub") == 0) {instruction = VMSUB; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmsubi") == 0) {instruction = VMSUBI; goto MemoryThreeArgs;}
		else
		if (strcmp(csPtr,"vmsubn") == 0) {instruction = VMSUBN; goto MemoryThreeArgs;}
		else

		goto NotAssembler;
		break;

	case 't':
		if (strcmp(csPtr,"vmtestescape") == 0) {instruction = VMTESTESCAPE; goto MemoryZeroArgs;}
		else

		goto NotAssembler;
		break;

	case 'v':
		if (strcmp(csPtr,"vmvecBinary") == 0) {instruction = vmvecBinary; goto Register_ImmArg;}
		else
		if (strcmp(csPtr,"vmvecInitialize") == 0) {instruction = vmvecInitialize; goto Register_ImmRegArgs;}
		else
		if (strcmp(csPtr,"vmvecLoop") == 0) {instruction = vmvecLoop; goto Register_NoArgs;}
		else
		if (strcmp(csPtr,"vmvecNumScalar") == 0) {instruction = vmvecNumScalar; goto Register_ImmRegRegArgs;}
		else
		if (strcmp(csPtr,"vmvecNumVector") == 0) {instruction = vmvecNumVector; goto Register_ImmRegArgs;}
		else
		if (strcmp(csPtr,"vmvecPop") == 0) {instruction = vmvecPop; goto Register_ImmImmArgs;}
		else
		if (strcmp(csPtr,"vmvecPopNumber") == 0) {instruction = vmvecPopNumber; goto Register_RegArg;}
		else
		if (strcmp(csPtr,"vmvecPush") == 0) {instruction = vmvecPush; goto Register_ImmImmArgs;}
		else
		if (strcmp(csPtr,"vmvecPushNumber") == 0) {instruction = vmvecPushNumber; goto Register_RegArg;}
		else
		if (strcmp(csPtr,"vmvecSetIncrements") == 0) {instruction = vmvecSetIncrements; goto Register_RegRegRegArgs;}
		else
		if (strcmp(csPtr,"vmvecSetPointers") == 0) {instruction = vmvecSetPointers; goto Register_RegRegRegArgs;}
		else
		if (strcmp(csPtr,"vmvecSwapCC") == 0) {instruction = vmvecSwapCC; goto Register_ImmArg;}
		else
		if (strcmp(csPtr,"vmvecUnary") == 0) {instruction = vmvecUnary; goto Register_ImmArg;}
		else

		goto NotAssembler;
		break;

	case 'x':
		if (strcmp(csPtr,"vmxor") == 0) {instruction = VMXOR; goto MemoryThreeArgs;}
		else

		goto NotAssembler;
		break;

	default:
		goto NotAssembler;
		break;
	} /* end of csPtr[2] switch statement */

/*  Implement Memory based VM instructions with zero arguments. */
MemoryZeroArgs:
if (pairs[0] != NULL) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 5, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = gCP->Tval_VOID;
FrameExit(*ret);

/*  Implement Memory based VM instructions with one argument. */
MemoryOneArg:
if ((pairs[0] == NULL) || (pairs[1] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmMemoryTarget(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 6, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[5];
FrameExit(*ret);

/*  Implement Memory based VM instructions with two arguments. */
MemoryTwoArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL) || (pairs[2] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmMemoryArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
_TempN(compilerState)++;
*ret = FOptimize2_VmMemoryTarget(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[6];
_TempN(compilerState)--;
FrameExit(*ret);

/*  Implement Memory based VM instructions with three arguments. */
MemoryThreeArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL)|| (pairs[2] == NULL) || (pairs[3] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmMemoryArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmMemoryArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmMemoryTarget(gCP, gTP, compilerState, pairs[2], callSymbol, &prmv[4], &prmv[7]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[7];
_TempN(compilerState)--; 
_TempN(compilerState)--; 
FrameExit(*ret);

/*  Implement Memory based VM conditional jump instructions with three arguments. */
MemoryJumpCC:
if ((pairs[0] == NULL) || (pairs[1] == NULL)|| (pairs[2] == NULL) || (pairs[3] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmMemoryArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmMemoryArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*label = FOptimize2_VmGotoLabel(gCP, gTP, compilerState, pairs[2], callSymbol, &prmv[4], &prmv[7]);
ExitOnError(*label);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
ExitOnError(*ret);
*ret = FOptimize2_AddGoto(gCP,gTP,compilerState, label->u.Symbol, Pc->itsCurItemIndex - 1);
_TempN(compilerState)--; 
FrameExit(gCP->Tval_TRUE);

/*  Implement Memory based VM unconditional jump instruction with one argument. */
MemoryJump:
if ((pairs[0] == NULL) || (pairs[1] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*label = FOptimize2_VmGotoLabel(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*label);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 6, &prmv[0]);
ExitOnError(*ret);
*ret = FOptimize2_AddGoto(gCP,gTP,compilerState, label->u.Symbol, Pc->itsCurItemIndex - 1);
FrameExit(gCP->Tval_TRUE);

/*  Implement Memory based VM instructions with an immediate and two arguments. */
MemoryImmTwoArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL)|| (pairs[2] == NULL) || (pairs[3] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmImmediate(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
*ret = FOptimize2_VmMemoryArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmMemoryTarget(gCP, gTP, compilerState, pairs[2], callSymbol, &prmv[4], &prmv[7]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[7];
_TempN(compilerState)--; 
FrameExit(*ret);

/*  Implement Register based VM instructions with no arguments. */
Register_NoArgs:
if (pairs[0] != NULL) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
_FCompile_MakeINT(prmv[2], AMVOID);
_FCompile_MakeINT(prmv[3], AMVOID);
_FCompile_MakeINT(prmv[4], AMVOID);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 5, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[5];
FrameExit(*ret);

/*  Implement Register based VM instructions with one register argument. */
Register_RegArg:
if ((pairs[0] == NULL) || (pairs[1] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 6, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[5];
FrameExit(*ret);

/*  Implement Register based VM instructions with an immediate and one register argument. */
Register_ImmRegArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL)|| (pairs[2] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmImmediate(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[6];
FrameExit(*ret);

/*  Implement Register based VM instructions with a shift immediate an immediate and one register argument. */
Register_ShlImmRegArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL)|| (pairs[2] == NULL) || (pairs[3] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmImmediate(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
*ret = FOptimize2_VmImmediate(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[2], callSymbol, &prmv[4], &prmv[7]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[7];
FrameExit(*ret);

/*  Implement Register based VM instructions with a shift immediate and two register arguments. */
Register_ShlRegRegArgs:
Register_ImmRegRegArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL) || (pairs[2] == NULL) || (pairs[3] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmImmediate(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[2], callSymbol, &prmv[4], &prmv[7]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[7];
_TempN(compilerState)--; 
FrameExit(*ret);

/*  Implement Register based VM instructions with two register arguments. */
Register_RegRegArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL)|| (pairs[2] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[6];
_TempN(compilerState)--; 
FrameExit(*ret);

/*  Implement Register based VM instructions with three register arguments. */
Register_RegRegRegArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL) || (pairs[2] == NULL) || (pairs[3] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[2], callSymbol, &prmv[4], &prmv[7]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[7];
_TempN(compilerState)--; 
_TempN(compilerState)--; 
FrameExit(*ret);

/*  Implement Register based VM instructions with two register arguments and one memory argument. */
Register_RegRegMemArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL) || (pairs[2] == NULL) || (pairs[3] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmMemoryTarget(gCP, gTP, compilerState, pairs[2], callSymbol, &prmv[4], &prmv[7]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[7];
_TempN(compilerState)--; 
_TempN(compilerState)--; 
FrameExit(*ret);

/*  Implement Register based VM instructions with one memory and two register arguments. */
Register_MemRegRegArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL) || (pairs[2] == NULL) || (pairs[3] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmMemoryArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[2], callSymbol, &prmv[4], &prmv[7]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[7];
_TempN(compilerState)--; 
_TempN(compilerState)--; 
FrameExit(*ret);

/*  Implement Register based VM conditional jump instructions with three arguments. */
Register_JumpCC:
if ((pairs[0] == NULL) || (pairs[1] == NULL)|| (pairs[2] == NULL) || (pairs[3] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*label = FOptimize2_VmGotoLabel(gCP, gTP, compilerState, pairs[2], callSymbol, &prmv[4], &prmv[7]);
ExitOnError(*label);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
ExitOnError(*ret);
*ret = FOptimize2_AddGoto(gCP,gTP,compilerState, label->u.Symbol, Pc->itsCurItemIndex - 1);
_TempN(compilerState)--; 
FrameExit(gCP->Tval_TRUE);

/*  Implement Register based VM conditional jump instructions with three arguments. */
RegisterImm_JumpCC:
if ((pairs[0] == NULL) || (pairs[1] == NULL)|| (pairs[2] == NULL) || (pairs[3] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
*ret = FOptimize2_VmImmediate(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*label = FOptimize2_VmGotoLabel(gCP, gTP, compilerState, pairs[2], callSymbol, &prmv[4], &prmv[7]);
ExitOnError(*label);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
ExitOnError(*ret);
*ret = FOptimize2_AddGoto(gCP,gTP,compilerState, label->u.Symbol, Pc->itsCurItemIndex - 1);
FrameExit(gCP->Tval_TRUE);

/*  Implement Register based VM instructions with one label argument and one register argument. */
Register_LblRegArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL)|| (pairs[2] != NULL) || (pairs[3] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*label = FOptimize2_VmGotoLabel(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*label);
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
ExitOnError(*ret);
*ret = FOptimize2_AddGoto(gCP,gTP,compilerState, label->u.Symbol, Pc->itsCurItemIndex - 1);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[6];
FrameExit(gCP->Tval_TRUE);

/*  Implement Register based VM instructions with one memory argument and one register argument. */
Register_MemRegArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL) || (pairs[2] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmMemoryArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[6];
_TempN(compilerState)--; 
FrameExit(*ret);

/*  Implement Register based VM instructions with one register argument and one memory argument. */
Register_RegMemArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL) || (pairs[2] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmMemoryTarget(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[6];
_TempN(compilerState)--; 
FrameExit(*ret);

/*  Implement Register based VM instructions with one immediate argument and one memory argument. */
Register_ImmMemArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL) || (pairs[2] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmImmediate(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
*ret = FOptimize2_VmMemoryTarget(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[6];
FrameExit(*ret);


/*  Implement Register based VM instructions with two memory arguments and one register argument. */
Register_MemMemRegArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL) || (pairs[2] == NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmMemoryArg(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmMemoryArg(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
_TempN(compilerState)++; 
*ret = FOptimize2_VmRegisterArg(gCP, gTP, compilerState, pairs[2], callSymbol, &prmv[4], &prmv[7]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[7];
_TempN(compilerState)--; 
FrameExit(*ret);

/*  Implement Register based VM instructions with two immediate arguments. */
Register_ImmImmArgs:
if ((pairs[0] == NULL) || (pairs[1] == NULL) || (pairs[2] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmImmediate(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
*ret = FOptimize2_VmImmediate(gCP, gTP, compilerState, pairs[1], callSymbol, &prmv[3], &prmv[6]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[6];
FrameExit(*ret);

/*  Implement Register based VM instructions with one immediate argument. */
Register_ImmArg:
if ((pairs[0] == NULL) || (pairs[1] != NULL)) goto BadCleanUp;
_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], instruction);
*ret = FOptimize2_VmImmediate(gCP, gTP, compilerState, pairs[0], callSymbol, &prmv[2], &prmv[5]);
ExitOnError(*ret);
*ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 7, &prmv[0]);
ExitOnError(*ret);
*ret = _LastResult(compilerState) = prmv[5];
FrameExit(*ret);

/*  This is NOT an optimizable assembler instruction. */
NotAssembler:
FrameExit(gCP->Tval_FALSE);


BadCleanUp:
/* Construct an Error message */
strcpy(tmpBuffer, "!compile (");
strcat(tmpBuffer, (const char*)SymbolArray(cs));
strcat(tmpBuffer, (const char*)"): invalid argument list!");

*ret = TERROR(tmpBuffer);
FrameExit(*ret);

/* Construct an Error message */
strcpy(tmpBuffer, "!compile (");
strcat(tmpBuffer, (const char*)SymbolArray(cs));
strcat(tmpBuffer, (const char*)"): not yet implemented!");

*ret = TERROR(tmpBuffer);
FrameExit(*ret);
}

