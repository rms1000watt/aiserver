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

#define _C_FCOMPILE
#define _SMARTBASE
#if 0 
FCompile.c

This file contains some of the procedures required to support the Lisp "compile" command.
The data structures used to control the basic compilation process are allocated and managed here.
Except for the handling of Special Forms the procedures in this file provide all support for
the generation of non-optimized VMSCRIPT procedure objects.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fcompile.h"
#include "foptimize.h"
#include "tpair.h"
#include "tdiction.h"
#include "tdirect.h"
#include "futil1.h"
#include "futil2.h"
#include "terror.h"
#include "fmake.h"
#include "tobject.h"
#include "tobjvec.h"
#include "fvmcode.h"
#include "fvmscpt.h"
#include "fvmscpt2.h"
#include "fmacro.h"
#include "fsforms1.h"
#include "fsforms2.h"
#include "fsforms3.h"
#include "fconio.h"
#include "fopt1.h"
#include "fopt2.h"
#include "fdebug.h"
#include "flisp.h"
#include "fproc.h"

NUM counter = 0;

/*  The compilation of nested lambda objects is a two phase process, we use FCompile_lambdaSwitch */
/*  to keep track of which phase is occuring for the lambda object currently under compilation. */
/*  For a description of lambda objects see the Lisp Ref. Guide under lambda and define. */
 
/*  FCompile_LambdaNest is a counter which is incremented everytime we start the compilation */
/*  of a nested lambda object and decremented every time we complete that process. */

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_Init

In order to initialize the virtual machine compiler, we register the cProcedures contained
in this file and also allocate and initialize any data structures used by the compiler.

#endif

TVAL FCompile_Init(LpXCONTEXT gCP, LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
EndFrame
 
/*  We utilize the  static FCompile_Initialized as a flag which is set the first time that */
/*  this procedure is invoked to insure that initialization will occur only one time. */

if(gCP->FCompile_Initialized) 
    {
    FrameExit(gCP->TObject_OK);
    }
else
    gCP->FCompile_Initialized = 1;

/* Allocate symbols which we will use during optimization. */

/*  Allocate symbols for the 18  booleans */
gTP->FCompile_GenerateDebugInfo = FALSE;
gTP->FCompile_OptimizeSW = TRUE;
gCP->FCompile_ieqSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"icompareEQ");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_ieqSym,TRUE);
gCP->FCompile_igtSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"icompareGT");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_igtSym,TRUE);
gCP->FCompile_iltSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"icompareLT");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_iltSym,TRUE);
gCP->FCompile_igeSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"icompareGE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_igeSym,TRUE);
gCP->FCompile_ileSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"icompareLE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_ileSym,TRUE);
gCP->FCompile_ineSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"icompareNE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_ineSym,TRUE);
gCP->FCompile_neqSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ncompareEQ");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_neqSym,TRUE);
gCP->FCompile_ngtSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ncompareGT");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_ngtSym,TRUE);
gCP->FCompile_nltSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ncompareLT");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_nltSym,TRUE);
gCP->FCompile_ngeSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ncompareGE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_ngeSym,TRUE);
gCP->FCompile_nleSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ncompareLE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_nleSym,TRUE);
gCP->FCompile_nneSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ncompareNE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_nneSym,TRUE);
gCP->FCompile_eqSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"=");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_eqSym,TRUE);
gCP->FCompile_gtSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)">");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_gtSym,TRUE);
gCP->FCompile_ltSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"<");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_ltSym,TRUE);
gCP->FCompile_geSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)">=");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_geSym,TRUE);
gCP->FCompile_leSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"<=");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_leSym,TRUE);
gCP->FCompile_neSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"<>");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_neSym,TRUE);

/*  Allocate symbols for the Lisp debugger information trees. */

gCP->FCompile_dbgSourceLinesSYM = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"dbgSourceLines");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_dbgSourceLinesSYM,TRUE);
gCP->FCompile_dbgSourceLinesInstrSYM = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"dbgSourceLinesInstr");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_dbgSourceLinesInstrSYM,TRUE);
gCP->FCompile_dbgSourceLinesSYM = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"dbgSourceLines");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_dbgSourceLinesSYM,TRUE);
gCP->FCompile_dbgParseTreeSYM = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"dbgParseTree");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_dbgParseTreeSYM,TRUE);
gCP->FCompile_dbgListLinesSYM = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"dbgListLines");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_dbgListLinesSYM,TRUE);


/*  Allocate symbols for the Lisp functions which translate readily into VM instructions. */

gCP->FCompile_refSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ref");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refSym,TRUE);
gCP->FCompile_setSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"set");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setSym,TRUE);
gCP->FCompile_addSym    = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"+");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_addSym,TRUE);
gCP->FCompile_addiSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"addi");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_addiSym,TRUE);
gCP->FCompile_iaddSym  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"iadd");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_iaddSym,TRUE);
gCP->FCompile_idivSym  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"idiv");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_idivSym,TRUE);
gCP->FCompile_imodSym  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"imod");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_imodSym,TRUE);
gCP->FCompile_imulSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"imul");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_imulSym,TRUE);
gCP->FCompile_isubSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"isub");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_isubSym,TRUE);
gCP->FCompile_naddSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"nadd");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_naddSym,TRUE);
gCP->FCompile_ndivSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ndiv");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_ndivSym,TRUE);
gCP->FCompile_nmodSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"nmod");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_nmodSym,TRUE);
gCP->FCompile_nmulSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"nmul");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_nmulSym,TRUE);
gCP->FCompile_nsubSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"nsub");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_nsubSym,TRUE);
gCP->FCompile_divSym  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"/");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_divSym,TRUE);
gCP->FCompile_diviSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"divi");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_diviSym,TRUE);
gCP->FCompile_divrSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"mod");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_divrSym,TRUE);
gCP->FCompile_divriSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"modi");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_divriSym,TRUE);
gCP->FCompile_mulSym   = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"*");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_mulSym,TRUE);
gCP->FCompile_muliSym  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"muli");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_muliSym,TRUE);
gCP->FCompile_shlSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bitwiseShiftLeft");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_shlSym,TRUE);
gCP->FCompile_shrSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bitwiseShiftRight");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_shrSym,TRUE);
gCP->FCompile_subSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"-");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_subSym,TRUE);
gCP->FCompile_subiSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"subi");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_subiSym,TRUE);
gCP->FCompile_xorSym  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bitwiseXor");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_xorSym,TRUE);
gCP->FCompile_nandSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bitwiseNand");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_nandSym,TRUE);
gCP->FCompile_norSym  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bitwiseNor");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_norSym,TRUE);
gCP->FCompile_nxorSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bitwiseNxor");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_nxorSym,TRUE);
gCP->FCompile_nandbSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"binaryNand");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_nandSym,TRUE);
gCP->FCompile_norbSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"binaryNor");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_norSym,TRUE);
gCP->FCompile_nxorbSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"binaryNxor");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_nxorSym,TRUE);
gCP->FCompile_add1Sym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"add1");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_add1Sym,TRUE);
gCP->FCompile_sub1Sym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"sub1");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_sub1Sym,TRUE);
gCP->FCompile_bandSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bitwiseAnd");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_bandSym,TRUE);
gCP->FCompile_borSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bitwiseOr");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_borSym,TRUE);

*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoLE,(LpCHAR)"gotoLE");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoLT,(LpCHAR)"gotoLT");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoEQ,(LpCHAR)"gotoEQ");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoNE,(LpCHAR)"gotoNE");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoGT,(LpCHAR)"gotoGT");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoGE,(LpCHAR)"gotoGE");
ExitOnError(*ret);

*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoLEb,(LpCHAR)"gotoLEb");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoLTb,(LpCHAR)"gotoLTb");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoEQb,(LpCHAR)"gotoEQb");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoNEb,(LpCHAR)"gotoNEb");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoGTb,(LpCHAR)"gotoGTb");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoGEb,(LpCHAR)"gotoGEb");
ExitOnError(*ret);

*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoLEc,(LpCHAR)"gotoLEc");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoLTc,(LpCHAR)"gotoLTc");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoEQc,(LpCHAR)"gotoEQc");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoNEc,(LpCHAR)"gotoNEc");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoGTc,(LpCHAR)"gotoGTc");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoGEc,(LpCHAR)"gotoGEc");
ExitOnError(*ret);

*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoLEi,(LpCHAR)"gotoLEi");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoLTi,(LpCHAR)"gotoLTi");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoEQi,(LpCHAR)"gotoEQi");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoNEi,(LpCHAR)"gotoNEi");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoGTi,(LpCHAR)"gotoGTi");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoGEi,(LpCHAR)"gotoGEi");
ExitOnError(*ret);

*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoLEn,(LpCHAR)"gotoLEn");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoLTn,(LpCHAR)"gotoLTn");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoEQn,(LpCHAR)"gotoEQn");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoNEn,(LpCHAR)"gotoNEn");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoGTn,(LpCHAR)"gotoGTn");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_gotoGEn,(LpCHAR)"gotoGEn");
ExitOnError(*ret);

*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_return,(LpCHAR)"return");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->TLambda_goto,(LpCHAR)"goto");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_ifSym,(LpCHAR)"if");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_selfSym,(LpCHAR)"myself");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_notSym,(LpCHAR)"not");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_andSym,(LpCHAR)"and");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_orSym,(LpCHAR)"or");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_argcountSym,(LpCHAR)"argCount");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_defineSym,(LpCHAR)"define");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_defunSym,(LpCHAR)"defun");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_defmacroSym,(LpCHAR)"defmacro");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_defchildSym,(LpCHAR)"defchild");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_defChildLambdaSym,(LpCHAR)"defChildLambda");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_deforphanSym,(LpCHAR)"deforphan");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_defriendSym,(LpCHAR)"defriend");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_defclassSym,(LpCHAR)"defclass");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_defcloneSym,(LpCHAR)"defclone");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_defstructSym,(LpCHAR)"defstruct");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_defvmSym,(LpCHAR)"defvm");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_defmethodSym,(LpCHAR)"defmethod");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_lambdaSym,(LpCHAR)"lambda");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_whileSym,(LpCHAR)"while");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_loopSym,(LpCHAR)"loop");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_beginSym,(LpCHAR)"begin");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_sendSym,(LpCHAR)"send");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_superSym,(LpCHAR)"super");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_argfetchSym,(LpCHAR)"argFetch");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_onerrorSym,(LpCHAR)"onError");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_letSym,(LpCHAR)"let");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_caseSym,(LpCHAR)"case");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_condSym,(LpCHAR)"cond");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_setfSym,(LpCHAR)"setf");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_setvSym,(LpCHAR)"setv");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_plusPlusSym,(LpCHAR)"++");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_minusMinusSym,(LpCHAR)"--");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_timesEqualsSym,(LpCHAR)"*=");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_plusEqualsSym,(LpCHAR)"+=");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_minusEqualsSym,(LpCHAR)"-=");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_divEqualsSym,(LpCHAR)"/=");
ExitOnError(*ret);
*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_refmacroSym,(LpCHAR)"refmacro");
ExitOnError(*ret);

*ret = FCompile_NewSpecialForm(gCP, gTP, &gCP->FCompile_setqSym,(LpCHAR)"setq");
ExitOnError(*ret);
aSymbol     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setq");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol,gCP->FCompile_setqSym->itsGlobalValue);

gCP->FCompile_addnSym  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"addn");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_addnSym,TRUE);
gCP->FCompile_divnSym  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"divn");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_divnSym,TRUE);
gCP->FCompile_mulnSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"muln");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_mulnSym,TRUE);
gCP->FCompile_subnSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"subn");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_subnSym,TRUE);
gCP->FCompile_beqSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bcompareEQ");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_beqSym,TRUE);
gCP->FCompile_bgtSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bcompareGT");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_bgtSym,TRUE);
gCP->FCompile_bltSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bcompareLT");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_bltSym,TRUE);
gCP->FCompile_bgeSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bcompareGE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_bgeSym,TRUE);
gCP->FCompile_bleSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bcompareLE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_bleSym,TRUE);
gCP->FCompile_bneSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bcompareNE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_bneSym,TRUE);

gCP->FCompile_caddSym  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"cadd");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_caddSym,TRUE);
gCP->FCompile_cdivSym  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"cdiv");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_cdivSym,TRUE);
gCP->FCompile_cmulSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"cmul");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_cmulSym,TRUE);
gCP->FCompile_csubSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"csub");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_csubSym,TRUE);
gCP->FCompile_ceqSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ccompareEQ");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_ceqSym,TRUE);
gCP->FCompile_cgtSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ccompareGT");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_cgtSym,TRUE);
gCP->FCompile_cltSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ccompareLT");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_cltSym,TRUE);
gCP->FCompile_cgeSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ccompareGE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_cgeSym,TRUE);
gCP->FCompile_cleSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ccompareLE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_cleSym,TRUE);
gCP->FCompile_cneSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ccompareNE");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_cneSym,TRUE);
gCP->FCompile_refTextSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refText");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refTextSym,TRUE);
gCP->FCompile_refStringSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refString");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refStringSym,TRUE);
gCP->FCompile_setStringSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setString");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setStringSym,TRUE);
gCP->FCompile_refSymbolSym      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refSymbol");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refSymbolSym,TRUE);
gCP->FCompile_refVectorSym		= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refVectorSym,TRUE);
gCP->FCompile_setVectorSym		= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setVectorSym,TRUE);
gCP->FCompile_refStrValueSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refStrValue");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refStrValueSym,TRUE);
gCP->FCompile_setStrValueSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setStrValue");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setStrValueSym,TRUE);
gCP->FCompile_refStrKeySym		= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refStrKey");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refStrKeySym,TRUE);
gCP->FCompile_setStrKeySym		= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setStrKey");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setStrKeySym,TRUE);
gCP->FCompile_refDicValueSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refDicValue");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refDicValueSym,TRUE);
gCP->FCompile_setDicValueSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setDicValue");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setDicValueSym,TRUE);
gCP->FCompile_refDicKeySym		= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refDicKey");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refDicKeySym,TRUE);
gCP->FCompile_setDicKeySym		= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setDicKey");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setDicKeySym,TRUE);
gCP->FCompile_refDirValueSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refDirValue");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refDirValueSym,TRUE);
gCP->FCompile_setDirValueSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setDirValue");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setDirValueSym,TRUE);
gCP->FCompile_refDirKeySym		= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refDirKey");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refDirKeySym,TRUE);
gCP->FCompile_setDirKeySym		= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setDirKey");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setDirKeySym,TRUE);
gCP->FCompile_refBitVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refBitVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refBitVectorSym,TRUE);
gCP->FCompile_setBitVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setBitVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setBitVectorSym,TRUE);
gCP->FCompile_refBytVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refBytVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refBytVectorSym,TRUE);
gCP->FCompile_setBytVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setBytVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setBytVectorSym,TRUE);
gCP->FCompile_refPcdVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refPcdVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refPcdVectorSym,TRUE);
gCP->FCompile_setPcdVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setPcdVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setPcdVectorSym,TRUE);
gCP->FCompile_refObjVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refObjVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refObjVectorSym,TRUE);
gCP->FCompile_setObjVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setObjVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setObjVectorSym,TRUE);
gCP->FCompile_refIntVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refIntVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refIntVectorSym,TRUE);
gCP->FCompile_setIntVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setIntVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setIntVectorSym,TRUE);
gCP->FCompile_refNumVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refNumVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refNumVectorSym,TRUE);
gCP->FCompile_setNumVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setNumVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setNumVectorSym,TRUE);
gCP->FCompile_refFltVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refFltVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refFltVectorSym,TRUE);
gCP->FCompile_setFltVectorSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setFltVector");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setFltVectorSym,TRUE);
gCP->FCompile_refMatrixSym		= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refMatrix");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refMatrixSym,TRUE);
gCP->FCompile_setMatrixSym		= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setMatrix");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setMatrixSym,TRUE);
gCP->FCompile_refNumMatrixSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"refNumMatrix");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_refNumMatrixSym,TRUE);
gCP->FCompile_setNumMatrixSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"setNumMatrix");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_setNumMatrixSym,TRUE);
gCP->FCompile_CharPointerSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"CharPointer");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_CharPointerSym,TRUE);
gCP->FCompile_FloatPointerSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"FloatPointer");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_FloatPointerSym,TRUE);
gCP->FCompile_IntegerSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Integer");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_IntegerSym,TRUE);
gCP->FCompile_IntPointerSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"IntPointer");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_IntPointerSym,TRUE);
gCP->FCompile_JumpPointerSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"JumpPointer");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_JumpPointerSym,TRUE);
gCP->FCompile_NumberSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Number");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_NumberSym,TRUE);
gCP->FCompile_NumPointerSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"NumPointer");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_NumPointerSym,TRUE);
gCP->FCompile_ShortPointerSym	= TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ShortPointer");
FObject_Perm(gCP,gTP,(TObject*)gCP->FCompile_ShortPointerSym,TRUE);



/* Register the new function for CompilerLib FCompile___Send */
*ret = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"__send",(LpFUNC)&FCompile___Send);
ExitOnError(*ret);

/*  Initialize the code for optimzations which may occur during the first phase of the compiler. */

FOptimize1_Init(gCP, gTP);
    
/* Register the Lisp cProcedures contained in this package */

*ret = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"compile",(LpFUNC)&FCompile_Compile);
ExitOnError(*ret);

FrameExit(gCP->TObject_OK);
}

/*  The compilation of nested lambda objects is a two phase process, we use FCompile_lambdaSwitch */
/*  to keep track of which phase is occuring for the lambda object currently under compilation. */
/*  For a description of lambda objects see the Lisp Ref. Guide under lambda and define. */
 
/*  FCompile_LambdaNest is a counter which is incremented everytime we start the compilation */
/*  of a nested lambda object and decremented every time we complete that process. */


/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_Compile

This is the procedure which serves as an entry point for the "compile" procedure defined in the
Lisp Ref. Guide. It will input a list (i.e. the output of lex and morph) representing a 
parse tree, and optionally an existing procedure object to attach the resultant pcode and source 
vectors to. See the ref guide for an explanation of the interaction between lex morph and compile.

We maintain necessary state information for each invocation of the compiler in a TVector. To 
see a description of what is stored in this vector and how it is initialized please see the 
FCompile_SetupProc() procedure.

The FCompile_Compile Procedure compiles the Lisp form (normally output from morph) into a 
Lisp Virtual Machine Procedure object. The source argument must be a Lisp form. The 
resulting Procedure object is returned. 

        (compile `(lambda (n) (+ n 10)))    

Note:   The same result can be obtained by compiling a string:

        (compile "(lambda (n) (+ n 10))")   

Normal Lisp strings are passed to lex, whose output is passed to morph, 
whose output is passed to compile. This allows the use of alternate lexical 
analyzers, and alternate macro preprocessors with the Lisp optimizing 
compiler. If a string is passed to compile, compile automatically passes the
string through first lex, then morph, before compiling.

#endif

TVAL    FCompile_Compile(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM			n;
NUM         oldLambdaNest;
NUM         oldlambdaSwitch;
StartFrame
DeclareOBJ(TStructure,oldDbgListLines);
DeclareOBJ(TVector,oldDbgSourceVector);
DeclareTVAL(oldDbgInterfaces);
DeclareTVAL(oldDbgCurrentSourceLines);
DeclareTVAL(oldDbgCurrentSourceLineIndex);
DeclareTVAL(dbgSourceLinesSYM);
DeclareTVAL(dbgParseTreeSYM);
DeclareTVAL(dbgListLinesSYM);
DeclareTVAL(ret);
DeclareTVALArray(prmv,3);
EndFrame

/* Save previous debugging information holder objects */
oldDbgSourceVector = gTP->FCompile_DebugSourceVector;
oldDbgListLines = gTP->FCompile_DebugListLines;
*oldDbgInterfaces = gTP->FCompile_DebugInterfaces;
*oldDbgCurrentSourceLines = gTP->FCompile_DebugCurrentSourceLine;
*oldDbgCurrentSourceLineIndex = gTP->FCompile_DebugCurrentSourceLineIndex;
gTP->FCompile_DebugSourceVector = NIL;
gTP->FCompile_DebugListLines = NIL;
gTP->FCompile_DebugInterfaces = gCP->Tval_VOID;
gTP->FCompile_DebugCurrentSourceLine = gCP->Tval_VOID;
gTP->FCompile_DebugCurrentSourceLineIndex = gCP->Tval_VOID;

/* Is this an single Lambda as an argument. */

if ((argc == 1) && (argv[0].Tag == TYLAMBDA))
	{
	argc = 1;
	*dbgParseTreeSYM	= TOBJ(gCP->FCompile_dbgParseTreeSYM);
	*ret = FSmartbase_Ref(gCP,gTP,2,TOBJ(argv[0].u.Lambda->Interfaces),*dbgParseTreeSYM);
	if (ret->Tag != TYPAIR) {*ret = gCP->FVmScript_ERROR_ILLEGAL_VALUE; goto Last;}
	if (ret->Tag == TYERROR) goto Last;
	prmv[0] = *ret;
	if (gTP->FCompile_GenerateDebugInfo == TRUE)
		{
		/* We are generating debugging information during this compile. */
		gTP->FCompile_DebugInterfaces = TOBJ(argv[0].u.Lambda->Interfaces);
		*dbgSourceLinesSYM = TOBJ(gCP->FCompile_dbgSourceLinesSYM);
		*ret = FSmartbase_Ref(gCP,gTP,2,TOBJ(argv[0].u.Lambda->Interfaces),*dbgSourceLinesSYM);
		if (ret->Tag != TYVECTOR)  {*ret = gCP->FVmScript_ERROR_ILLEGAL_VALUE; goto Last;}
		gTP->FCompile_DebugSourceVector = ret->u.Vector;
		*dbgListLinesSYM	= TOBJ(gCP->FCompile_dbgListLinesSYM);
		*ret = FSmartbase_Ref(gCP,gTP,2,TOBJ(argv[0].u.Lambda->Interfaces),*dbgListLinesSYM);
		if (ret->Tag != TYSTRUCTURE)  {*ret = gCP->FVmScript_ERROR_ILLEGAL_VALUE; goto Last;}
		gTP->FCompile_DebugListLines = ret->u.Structure;
		}
	}
else
/* Is this a parse tree and an Lambda as an argument. */

if ((argc >= 2) && (argv[0].Tag == TYPAIR) && (argv[1].Tag == TYLAMBDA))
	{
	if (gTP->FCompile_GenerateDebugInfo == TRUE)
		{
		/* We are generating debugging information during this compile. */
		gTP->FCompile_DebugInterfaces = TOBJ(argv[1].u.Lambda->Interfaces);
		*dbgSourceLinesSYM = TOBJ(gCP->FCompile_dbgSourceLinesSYM);
		*ret = FSmartbase_Ref(gCP,gTP,2,TOBJ(argv[1].u.Lambda->Interfaces),*dbgSourceLinesSYM);
		if (ret->Tag != TYVECTOR)  {*ret = gCP->FVmScript_ERROR_ILLEGAL_VALUE; goto Last;}
		gTP->FCompile_DebugSourceVector = ret->u.Vector;
		*dbgListLinesSYM	= TOBJ(gCP->FCompile_dbgListLinesSYM);
		*ret = FSmartbase_Ref(gCP,gTP,2,TOBJ(argv[1].u.Lambda->Interfaces),*dbgListLinesSYM);
		if (ret->Tag != TYSTRUCTURE)  {*ret = gCP->FVmScript_ERROR_ILLEGAL_VALUE; goto Last;}
		gTP->FCompile_DebugListLines = ret->u.Structure;
		}

	for (n = 0; n < argc; ++n)
		{
		prmv[n] = argv[n];
	    }
	}
else
if ((argc >= 1) && (argv[0].Tag == TYPAIR))
	{
	for (n = 0; n < argc; ++n)
		{
		prmv[n] = argv[n];
	    }
	}
else
	{
	*ret = TERROR("!compile: invalid arguments!");
	FrameExit(*ret);
	}

/* Expand all macros before compilation. */
gTP->FCompile_OptimizeSW = TRUE;
*ret = FLisp_MorphList(gCP, gTP,prmv[0],gCP->Tval_VOID,gCP->Tval_FALSE);
gTP->FCompile_OptimizeSW = TRUE;
if (ret->Tag == TYERROR) goto Last;
prmv[0] = *ret;

/* Save the old compiler state. */

oldLambdaNest = gTP->FCompile_LambdaNest;
oldlambdaSwitch = gTP->FCompile_lambdaSwitch;
gTP->FCompile_LambdaNest = 0;
gTP->FCompile_lambdaSwitch = 0;

/*  Call the private compiler function. */

gTP->FCompile_OptimizeSW = TRUE;
*ret = FCompile_CompilePrivate(gCP,gTP,argc,&prmv[0]);
gTP->FCompile_OptimizeSW = TRUE;

/* Restore the old compiler state. */
gTP->FCompile_LambdaNest = oldLambdaNest;
gTP->FCompile_lambdaSwitch = oldlambdaSwitch;

Last:
/* Restore previous debugging information holder objects */

gTP->FCompile_DebugSourceVector = oldDbgSourceVector;
gTP->FCompile_DebugListLines = oldDbgListLines;    
gTP->FCompile_DebugInterfaces = *oldDbgInterfaces;    
gTP->FCompile_DebugCurrentSourceLine = *oldDbgInterfaces;
gTP->FCompile_DebugCurrentSourceLineIndex = *oldDbgInterfaces;
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_CompilePrivate

This is the private procedure which serves as an entry point for the "compile" procedure defined in the
Lisp Ref. Guide. It will input a list (i.e. the output of lex and morph) representing a 
parse tree, and optionally an existing procedure object to attach the resultant pcode and source 
vectors to. See the ref guide for an explanation of the interaction between lex morph and compile.

We maintain necessary state information for each invocation of the compiler in a TVector. To 
see a description of what is stored in this vector and how it is initialized please see the 
FCompile_SetupProc() procedure.

The FCompile_Compile Procedure compiles the Lisp form (normally output from morph) into a 
Lisp Virtual Machine Procedure object. The source argument must be a Lisp form. The 
resulting Procedure object is returned. 

        (compile `(lambda (n) (+ n 10)))    

Note:   The same result can be obtained by compiling a string:

        (compile "(lambda (n) (+ n 10))")   

Normal Lisp strings are passed to lex, whose output is passed to morph, 
whose output is passed to compile. This allows the use of alternate lexical 
analyzers, and alternate macro preprocessors with the Lisp optimizing 
compiler. If a string is passed to compile, compile automatically passes the
string through first lex, then morph, before compiling.

#endif

TVAL    FCompile_CompilePrivate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
StartFrame
DeclareOBJ(TStructure,tmpEnv);
DeclareOBJ(TPair,curPair);
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TVector,compilerState);
DeclareTVAL(tmp);
DeclareTVAL(ret);
DeclareTVAL(dbgSourceLinesInstr);
DeclareTVAL(pcodeInstr);
DeclareTVALArray(prmv,8);
EndFrame

counter++;

gTP->FCompile_OptimizeSW = TRUE;

/*  We will accept either one, two, or three arguments. */
/*  The first (mandatory) argument can be anything output from morph. */
/*  The second (optional) argument must be an existing Lambda object */
/*  which will serve as the target of the compilation. */
/*  The third (optional) argument must be true if the compilation is */
/*  always to inherit the persistent variables of the second argument. */

/*  Exit with an error if that is what was input. */

if (isERROR(&argv[0]))
    {
    FrameExit(argv[0]);
    }

    
/*  If there is a third argument then set the nested lambda switch */
/*  so that the compilation will always inherit the persistent */
/*  variables of the Lambda object supplied as the second argument. */
    
if (argc == 3)
    {
    argc = 2;

    if ((argv[2].Tag == TYBOLE) && (argv[2].u.Bool == TRUE))
        {
        /* Make sure we always inherit the persistent variables */
        /* of the Lambda object supplied in the second argument. */
        gTP->FCompile_LambdaNest = 2;
        gTP->FCompile_lambdaSwitch = 0;
        }
    else
        {
        /* Make sure we never inherit the persistent variables */
        /* of the Lambda object supplied in the second argument. */
        gTP->FCompile_LambdaNest = 0;
        gTP->FCompile_lambdaSwitch = 0;
        }
    }
    
/*  Initialize the state information for this invocation. */

compilerState   = TVector_New(gCP,gTP);

asObject(tmp) = (TObject*)compilerState;
asTag(tmp) = compilerState->itsObjectType;
TVector_SetMaxIndex(gCP,gTP,*tmp, _FCompile_SizeOfStateVector);

/*  The compiler expects input in the form of a list. */

curPair = TPair_New(gCP,gTP);
curPair->itsCar = argv[0];

/*  We check the FCompile_LambdaNest counter to determine if we are processing a nested */
/*  lambda object. Nested lambda objects are created via recursive calls to the compiler */
/*  from the procedures which generate code for the lambda and define special forms. */
    
if (argc == 2)
    {
    /*  The second optional argument has already been validated as a procedure object. It may */
    /*  have been passed in at the initial invocation of the compiler, or it may have been  */
    /*  generated as the result of generating code for a nested lambda object. */
    
    if (gTP->FCompile_LambdaNest)
        {
        /*  If the FCompile_LambdaNest counter is greater than zero then we know that we have */
        /*  been called as a result of processing a lambda or define special form. In this case */
        /*  the procedure object which is passed in is the object for the parent of this object. */
        /*  Because of the way in which variables are scoped in Lisp (see the Lisp  */
        /*  Ref. Guide), we need to create a new procedure object which inherits its permanent  */
        /*  variable Structure from its parent. */
        
        _CurProc(compilerState).u.Lambda = TLambda_New(gCP,gTP);
        if (_CurProc(compilerState).u.Object == NIL) goto BadCleanUp;
        _CurProc(compilerState).Tag = TYLAMBDA;
        
        /*  Single nested lambda objects will receive new Pv structures. */
        /*  See the Lisp Ref. Guide to see rules for variable scoping in Lisp. */
        if (gTP->FCompile_LambdaNest == 1)
            {
            _Pv(compilerState) = NIL;
            _Cv(compilerState) = NIL;
            _Sv(compilerState) = NIL;
            _Sc(compilerState) = ((TLambda*)asObject(&argv[1]))->DebuggerSource;
            }
        else
        /*  Doubly nested lambda objects will inherit the Pv structure */
        /*  (permanent variables) from the caller. See the Lisp */
        /*  Ref. Guide to see rules for variable scoping in Lisp. */
            {
            _Sv(compilerState) = ((TLambda*)asObject(&argv[1]))->ClassVariables;
            _Pv(compilerState) = ((TLambda*)asObject(&argv[1]))->PersistantVariables;
            _Cv(compilerState) = ((TLambda*)asObject(&argv[1]))->ConstantVariables;
            _Sc(compilerState) = ((TLambda*)asObject(&argv[1]))->DebuggerSource;
            }
        }
    else
        {
        /*  This is not a nested lambda object but we have been provided with a procedure object */
        /*  as the target of the compilation. This is the case when compiling spreadsheet formulas. */
        
        _CurProc(compilerState) = argv[1];
        
        /*  Debug information is passed via Pv and Sc */
        
        _Sv(compilerState) = _CurProcP(compilerState)->ClassVariables;
        _Pv(compilerState) = _CurProcP(compilerState)->PersistantVariables;
        _Cv(compilerState) = _CurProcP(compilerState)->ConstantVariables;
        _Sc(compilerState) = _CurProcP(compilerState)->DebuggerSource;

        /* Make sure we always inherit the persistent variables */
        /* of the Lambda object supplied in the second argument. */
        gTP->FCompile_LambdaNest = 2;
        gTP->FCompile_lambdaSwitch = 0;
        }

    /*  Call FCompile_SetupProc to allocate and initialize objects in */
    /*  the current compiler state vector. */
    
    FCompile_SetupProc(gCP, gTP, compilerState);
    }
else
    {
    /* We have not been passed a target procedure object and so we must create a new one. */
    
    _CurProc(compilerState).u.Lambda = TLambda_New(gCP,gTP);
    if (_CurProc(compilerState).u.Object == NIL) goto BadCleanUp;
    _CurProc(compilerState).Tag = TYLAMBDA;
    
    /*  This is not a nested call to the compiler and we must reinitialize the variables which */
    /*  are used to track the state and depth of lambda object compilation. */
    
    gTP->FCompile_lambdaSwitch = 0;
    gTP->FCompile_LambdaNest = 0;
    
    /*  Call FCompile_SetupProc to allocate and initialize objects in the current compiler */
    /*  state vector. */

    FCompile_SetupProc(gCP, gTP, compilerState);
    }

/*  If we are generating debugger information, add a new Interfaces Structure to the current procedure object. */
if (gTP->FCompile_GenerateDebugInfo == TRUE)
	{
	/* Add a new Interfaces Structure to this Lambda.  */
	if (_In(compilerState) == NIL)
		{
		_In(compilerState) = TStructure_New(gCP,gTP);
		TStructure_AddNewValue(gCP,gTP,TOBJ(_In(compilerState)),TOBJ(gCP->FCompile_dbgSourceLinesSYM),TOBJ(gTP->FCompile_DebugSourceVector));
		}
	/* Get source lines ==> pcode instruction displacement vector.  */
	*dbgSourceLinesInstr = FSmartbase_Ref(gCP,gTP,2,TOBJ(_In(compilerState)),TOBJ(gCP->FCompile_dbgSourceLinesInstrSYM));
	ExitOnError(*dbgSourceLinesInstr);
	if (dbgSourceLinesInstr->Tag != TYDIRECTORY)
		{
		dbgSourceLinesInstr->u.Directory = TDirectory_New(gCP,gTP);
		dbgSourceLinesInstr->Tag = TYDIRECTORY;
		TStructure_AddNewValue(gCP,gTP,TOBJ(_In(compilerState)),TOBJ(gCP->FCompile_dbgSourceLinesInstrSYM),*dbgSourceLinesInstr);
		}
	if (_Pc(compilerState) == NIL)
		{
		_Pc(compilerState) = TPcodeVector_New(gCP,gTP);
		}
	/* Associate index of the current source line with the current pcode instruction displacement.  */
	if (gTP->FCompile_DebugCurrentSourceLineIndex.Tag == TYNUM)
		{
		pcodeInstr->u.Int = _Pc(compilerState)->itsMaxItemIndex;
		pcodeInstr->Tag = TYNUM;
		*ret = FSmartbase_Set(gCP,gTP,3,*dbgSourceLinesInstr,gTP->FCompile_DebugCurrentSourceLineIndex,*pcodeInstr);
		ExitOnError(*ret);
		}
	}

/*  Invoke the main recognizer to compile the parse tree into the current procedure object */
/*  This invocation of the recognizer is the entry point for the first phase of the compiler */
/*  which will also include the simple optimizations allowed by replacing some function calls */
/*  with virtual machine instructions, i.e. add, subtract, divide, multiply etc. */

*ret = FCompile_Recognize(gCP, gTP, compilerState, curPair);
if (_TObject_IsError(*ret)) goto BadCleanUp;

/*  During the compilation process we create and maintain lists of all gotos and labels in the */
/*  procedure being compiled. We must cross reference these lists in order to resolve all of the */
/*  goto statements in the program. */

*ret = FCompile_ResolveGotos(gCP, gTP,compilerState);
if (_TObject_IsError(*ret)) goto BadCleanUp;

if ((_LastResult(compilerState).Tag == TYVOID) || ((_LastResult(compilerState).Modifier == AMGVOFFSET) && !(_VALIDOBJ(_LastResult(compilerState).u.Object))))
	{
	/* Force a return of a void as the final opcode. */
	_FCompile_MakeSYM(prmv[5], gCP->Tval_VOID);
	prmv[5].Modifier = AMVOID;
	*ret = FCompile_WritePcode(gCP, gTP, compilerState, TINT(VMRETURN), prmv[5], gCP->Tval_VOID, gCP->Tval_VOID);
	if (_TObject_IsError(*ret)) goto BadCleanUp;
	}
else
	{
	/* Force the return of the final result of this Lambda object. */
	_FCompile_MakeSYM(prmv[5], _LastResult(compilerState));
	*ret = FCompile_WritePcode(gCP, gTP, compilerState, TINT(VMRETURN), prmv[5], gCP->Tval_VOID, gCP->Tval_VOID);
	if (_TObject_IsError(*ret)) goto BadCleanUp;
	}

/* Truncate the pcode vector to that portion actually used. */

TPcodeVector_SetMaxIndex(gCP, gTP, TOBJ(_Pc(compilerState)), _PcP(compilerState)->itsCurItemIndex);

_FCompile_MakeTVL(*ret, _CurProc(compilerState));

/*  Void unused Structure objects to free unused space */

if(_DbgP(compilerState))
    {
    /*  Discard the sorted patch vector for debug. */
    
    _FCompile_MakeINT(*tmp, 0);
    TStructure_Delete(gCP,gTP,TOBJ(_Pv(compilerState)),*tmp);
    }

/* Make sure the basic machine registers are allocated. */

FCompile_SetupRegs(gCP,gTP,_CurProc(compilerState).u.Lambda);

/* ....................................................... */
/* Check for any compiler limits which have been exceeded. */
/* ....................................................... */

tmpEnv = _Av(compilerState);
if ((tmpEnv != NIL) && (tmpEnv->itsMaxItemIndex == 0))
    _Av(compilerState) = NIL;
if ((_Av(compilerState) != NIL) && (tmpEnv->itsMaxItemIndex > 20))
	{
	*ret = TERROR("!compile: an Lambda cannot have more than 20 arguments!");
    goto BadCleanUp;
	}
tmpEnv = _Tv(compilerState);
if ((tmpEnv != NIL) && (tmpEnv->itsMaxItemIndex == 0))
    _Tv(compilerState) = NIL;
if ((_Tv(compilerState) != NIL) && (tmpEnv->itsMaxItemIndex > 3200))
	{
	*ret = TERROR("!compile: an Lambda cannot have more than 3200 temporary variables!");
    goto BadCleanUp;
	}
tmpEnv = _Sv(compilerState);
if ((tmpEnv != NIL) && (tmpEnv->itsMaxItemIndex == 0))
    _Sv(compilerState) = NIL;
if ((_Sv(compilerState) != NIL) && (tmpEnv->itsMaxItemIndex > 3200))
	{
	*ret = TERROR("!compile: an Lambda cannot have more than 3200 svars variables!");
    goto BadCleanUp;
	}
tmpEnv = _Pv(compilerState);
if ((tmpEnv != NIL) && (tmpEnv->itsMaxItemIndex == 0))
    _Pv(compilerState) = NIL;
if ((_Pv(compilerState) != NIL) && (tmpEnv->itsMaxItemIndex > 3200))
	{
	*ret = TERROR("!compile: an Lambda cannot have more than 32000 pvars variables!");
    goto BadCleanUp;
	}
tmpEnv = _Cv(compilerState);
if ((tmpEnv != NIL) && (tmpEnv->itsMaxItemIndex == 0))
    _Cv(compilerState) = NIL;
if ((_Cv(compilerState) != NIL) && (tmpEnv->itsMaxItemIndex > 3200))
	{
	*ret = TERROR("!compile: an Lambda cannot have more than 32000 cvars variables!");
    goto BadCleanUp;
	}
tmpEnv = _Rv(compilerState);
if ((_Rv(compilerState) != NIL) && (tmpEnv->itsMaxItemIndex == 0))
    _Rv(compilerState) = NIL;
if ((_Rv(compilerState) != NIL) && (tmpEnv->itsMaxItemIndex > MAXREGISTERCNT))
	{
	*ret = TERROR("!compile: an Lambda cannot have more than 50 register variables!");
    goto BadCleanUp;
	}
    
/*  We return a the successfully compiled procedure object via the return TVAL. */

FrameExit(*ret);

BadCleanUp:
/*  Return an error diagnostic and truncate pcode vector  */
/*  so that no invalid code is ever evaluated. */

TPcodeVector_SetMaxIndex(gCP, gTP, TOBJ(_Pc(compilerState)), 0);
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_Recognize

This is the main recognizer for the compiler. It is called in recursive descent each time 
a Pair is encountered in the parse tree. It may also be called during the processing of 
special forms or from other functions in the compiler.

#endif

TVAL    FCompile_Recognize(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
TYPE                tag;
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(ret);
EndFrame


do 
    {
    switch((tag = asTag(&curPair->itsCar)))
        {
		case TYPAIR:

            _NestLvlN(compilerState) += 1;
            *ret = FCompile_RecognizeHead(gCP, gTP, compilerState, asPair(&curPair->itsCar));
            _NestLvlN(compilerState) -= 1;
            if(isERROR(ret)) 
                {
                /*  If an error occured while processing a subexpression then we exit with an */
                /*  error. */
                
                curPair = NIL;
                goto BadCleanUp;
                }
                
            /*  If no error occured then we will move to the next pair in the parse tree */
            /*  and continue processing in this main FCompile_Recognize loop. */
        break;
        
        case TYSYMBOL:
            /*  Get the symbol object from the car of the current pair. */
            
            aSymbol = asSymbol(&curPair->itsCar);
            
            /*  The tag of the global value of a symbol tells us whether this symbol is associated */
            /*  with a special form or a procedure, or what. */

            tag = asTag(&aSymbol->itsGlobalValue);
            
            if(tag == TYSPECIALFORM)
                {
                /*  Even when symbols are aliased they will still have the same global value, */
                /*  and the global value contains the symbol associated with a Lisp */
                /*  procedure or special form. These connections are initially made in the  */
                /*  functions FProcedure_NewCProcedure() and FCompile_NewSpecialForm(). */
                
                /*  We check the global value for a match with a recognized special form */
                /*  and perform processing associated with that special form. */

                aSymbol = (TSymbol*)asObject(&aSymbol->itsGlobalValue);
                
                if(aSymbol == gCP->TLambda_goto)
                    {

                    *ret = FSpecialForms3_goto(gCP, gTP, compilerState, curPair);
                    
                    /*  Check to see if an error occured, or continue */
                    
                    if(isERROR(ret)) 
                        goto BadCleanUp;
                    else
                        {
                        /*  Move to the cdr pair and continue processing in the recognizer loop. */
                        
                        curPair = _FCompile_CdrPair(curPair);
                        }
                    }
                else
                    {
                    /*  There is only one special form symbol which is legal at a location other */
                    /*  than the head of a list (cases in FCompile_RecognizeHead ) and that is  */
                    /*  the goto special form, which is processed above. */
					*ret = TERROR("!Lisp: Encountered a Special Form outside head of list!");
					goto BadCleanUp;

                    }
                }
            else
                {
                /*  Process a single symbol */
                
                goto Singleton;
                }
        break;
        
        default:
            /*  Here we handle all of the remaining special cases which include Labels (i.e goto */
            /*  targets), quoted symbols, vars, pvars, and args statements, as well as the general */
            /*  case processing for constants of all other data types (object or native.) */
            
            if(tag == TYLABELSYMBOL)
                {
                aSymbol = asSymbol(&curPair->itsCar);
                if(aSymbol == gCP->TLambda_label)
                    {
                    *ret = FSpecialForms3_label(gCP, gTP, compilerState, curPair);
                    if(isERROR(ret)) 
                        goto BadCleanUp;
                    
                    /*  continue processing in the recognizer loop */
                    
                    curPair = _FCompile_CdrPair(curPair);
                    }
                else
                    {
                    /*  Since the tag for the car is TYLABELSYMBOL then the symbol MUST be  */
                    /*  TLambda_label else we have an error. */
                    
					*ret = TERROR("!Lisp: Encountered an extra or illegal label here!");
 					goto BadCleanUp;
                   }
                }
            else
            if(tag == TYQUOTEDSYMBOL)
                {
                aSymbol = asSymbol(&curPair->itsCar);
                if(aSymbol == gCP->TLambda_args)
                    {
                    tmpPair = _FCompile_CdrPair(curPair);
                    if(asTag(&tmpPair->itsCar) == TYPAIR)
                        {
                        /*  process args: ()  */
                        
                        *ret = FCompile_args(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                        if(isERROR(ret)) goto BadCleanUp;
                        
                        /*  continue processing in the recognizer loop */

						curPair = tmpPair;
                        }
                    else
                        {
                        /*  An args: statement MUST be followed by a list of arguments i.e. */
                        /*      args: ( a b c) */
                        /*  It is not legal to have the args: quoted symbol standing alone. */
                        
   						*ret = TERROR("!args: Missing argument list!");
						goto BadCleanUp;

						}
                    }
                else
				if(aSymbol == gCP->TLambda_faces)
                    {
                    tmpPair = _FCompile_CdrPair(curPair);
                    /*  A faces: statement MUST be followed by a list of arguments i.e. */
                    /*      vars: ( a b c) */
                    if (tmpPair == NIL)
                        {                        
   						*ret = TERROR("!faces: Missing variable list!");
 						goto BadCleanUp;
                       }
                    if(asTag(&tmpPair->itsCar) == TYPAIR)
                        {
                        /*  process faces: () */
                        
                        *ret = FCompile_faces(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                        if(isERROR(ret)) goto BadCleanUp;
                        
                        /*  continue processing in the recognizer loop */

						curPair = tmpPair;
                        }
                    else
                        {
                        /*  It is not legal to have the faces: quoted symbol standing alone. */
                        
   						*ret = TERROR("!faces: Missing variable list!");
						goto BadCleanUp;
                        }
                    }
                else
				if(aSymbol == gCP->TLambda_rvars)
                    {
                    tmpPair = _FCompile_CdrPair(curPair);
                    /*  An regs: statement MUST be followed by a list of registers i.e. */
                    /*      regs: ( a b c) */
                    if (tmpPair == NIL)
                        {                        
   						*ret = TERROR("!regs: Missing register list!");
 						goto BadCleanUp;
                       }
                    if(asTag(&tmpPair->itsCar) == TYPAIR)
                        {
                        /*  process regs: () */
                        
                        *ret = FCompile_rvars(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                        if(isERROR(ret)) goto BadCleanUp;
                        
                        /*  continue processing in the recognizer loop */

						curPair = tmpPair;
                        }
                    else
                        {
                        /*  It is not legal to have the regs: quoted symbol standing alone. */
                        
   						*ret = TERROR("!regs: Missing register list!");
						goto BadCleanUp;
                        }
                    }
                else
				if(aSymbol == gCP->TLambda_vars)
                    {
                    tmpPair = _FCompile_CdrPair(curPair);
                    /*  A vars: statement MUST be followed by a list of arguments i.e. */
                    /*      vars: ( a b c) */
                    if (tmpPair == NIL)
                        {                        
   						*ret = TERROR("!vars: Missing variable list!");
 						goto BadCleanUp;
                       }
                    if(asTag(&tmpPair->itsCar) == TYPAIR)
                        {
                        /*  process vars: () */
                        
                        *ret = FCompile_vars(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                        if(isERROR(ret)) goto BadCleanUp;
                        
                        /*  continue processing in the recognizer loop */

						curPair = tmpPair;
                        }
                    else
                        {
                        /*  It is not legal to have the vars: quoted symbol standing alone. */
                        
   						*ret = TERROR("!vars: Missing variable list!");
						goto BadCleanUp;
                        }
                    }
                else
                if(aSymbol == gCP->TLambda_pvars)
                    {
                    tmpPair = _FCompile_CdrPair(curPair);
                        /*  A pvars: statement MUST be followed by a list of arguments i.e. */
                        /*  pvars: ( a b c) */
                   if (tmpPair == NIL)
                        {                        
   						*ret = TERROR("!pvars: Missing permanent variable list!");
						goto BadCleanUp;
                        }
                   if(asTag(&tmpPair->itsCar) == TYPAIR)
                        {
                        /*  process pvars: () */
                        
                        *ret = FCompile_pvars(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                        if(isERROR(ret)) goto BadCleanUp;
                        
                        /*  continue processing in the recognizer loop */

						curPair = tmpPair;
                        }
                    else
                        {
                        /*  It is not legal to have the pvars: quoted symbol standing alone. */
                        
   						*ret = TERROR("!pvars: Missing permanent variable list!");
						goto BadCleanUp;
                        }
                    }
                else
                if(aSymbol == gCP->TLambda_svars)
                    {
                    tmpPair = _FCompile_CdrPair(curPair);
                        /*  A svars: statement MUST be followed by a type name i.e. */
                        /*  svars: ( typeName ) */
                   if (tmpPair == NIL)
                        {                        
   						*ret = TERROR("!svars: Missing permanent variable list!");
						goto BadCleanUp;
                        }
                   if(asTag(&tmpPair->itsCar) == TYPAIR)
                        {
                        /*  process svars: ()  or  svars:typeName*/
                        
                        *ret = FCompile_svars(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                        if(isERROR(ret)) goto BadCleanUp;
                        
                        /*  continue processing in the recognizer loop */

						curPair = tmpPair;
                        }
                    else
                        {
                        /*  It is not legal to have the pvars: quoted symbol standing alone. */
                        
   						*ret = TERROR("!pvars: Missing permanent variable list!");
						goto BadCleanUp;
                        }
                    }
                else
                if(aSymbol == gCP->TLambda_cvars)
                    {
                    tmpPair = _FCompile_CdrPair(curPair);
                        /*  A cvars: statement MUST be followed by a list of arguments i.e. */
                        /*  cvars: ( a b c) */
                   if (tmpPair == NIL)
                        {                        
   						*ret = TERROR("!cvars: Missing permanent variable list!");
						goto BadCleanUp;
                        }
                   if(asTag(&tmpPair->itsCar) == TYPAIR)
                        {
                        /*  process cvars: () */
                        
                        *ret = FCompile_cvars(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                        if(isERROR(ret)) goto BadCleanUp;
                        
                        /*  continue processing in the recognizer loop */

						curPair = tmpPair;
                        }
                    else
                        {
                        /*  It is not legal to have the pvars: quoted symbol standing alone. */
                        
   						*ret = TERROR("!cvars: Missing permanent variable list!");
						goto BadCleanUp;
                        }
                    }
                else
                if(aSymbol == gCP->TLambda_Doomed)
                    {
					_Dm(compilerState) = TRUE;
					}
                else
                    goto Singleton;                 
                }
            else
                {
                /*  Singleton processing is triggered when we compile a Lisp expression */
                /*  such as : */
                /*              ... */
                
                Singleton:
                
                if (curPair == NIL) goto BadCleanUp;
                asTag(ret) = curPair->itsObjectType;
                asObject(ret) = (TObject*)curPair;
                tmpPair = (TPair*)TPair_Copy(gCP, gTP, *ret);

                tmpPair->itsCdr = gCP->TObject_VOID;
                *ret = FCompile_Singleton(gCP, gTP, compilerState, tmpPair);
                if(isERROR(ret)) goto BadCleanUp;
                }
        break;
        }
    } while((curPair = _FCompile_CdrPair(curPair)) != NULL);

FrameExit(*ret);

BadCleanUp:

/* If no error message have been previously assigned, use a generic one */
if(asTag(ret) != TYERROR)
    {
   	*ret = TERROR("!Lisp: Cannot parse expression!");
    }
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_RecognizeHead

This is the list head recognizer for the compiler. It is called in recursive descent each time 
a Pair is encountered in the parse tree. It may also be called from within special forms 
processing when we are replacing a special form with a constructed function call.

Consider the following Lisp expression:

(setq foo 1)

This expression will be passed to FCompile_Recognize who will identify the pair whose
car is the symbol setq, and will pass control to FCompile_RecognizeHead with the curpair
pointing to that pair. FCompile_RecognizeHead will switch on the car of the pair which it
is passed, in this case a TYSYMBOL for the special form setq. The setq special form has the
responsibility for insuring that any subexpressions it contains are processed via calls back
to the recognizer, thus once setq has returned control to FCompile_RecognizeHead the entire
list which was passed in will have been compiled and control will return to the caller, in this
case FCompile_Recognize.

#endif

TVAL    FCompile_RecognizeHead(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
TYPE                tag;
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(head);
DeclareTVAL(sourceLine);
DeclareTVAL(sourceLineIndex);
DeclareTVAL(dbgSourceLinesInstr);
DeclareTVAL(pcodeInstr);
DeclareTVAL(ret);
EndFrame

do 
    {
	if ((gTP->FCompile_GenerateDebugInfo == TRUE) && (curPair != NIL))
		{
		/* Try to locate the head of this list in the DebugListLines Directory of the Lambda */
		*head = TOBJ(curPair);
		*sourceLineIndex = FSmartbase_Ref(gCP,gTP,2,TOBJ(gTP->FCompile_DebugListLines),*head);
		ExitOnError(*sourceLineIndex);
		if (sourceLineIndex->Tag == TYNUM)
			{
			/* Get index of the current source line and associate it with the head of this list.  */
			gTP->FCompile_DebugCurrentSourceLineIndex = *sourceLineIndex;
			*sourceLine = FSmartbase_Ref(gCP,gTP,2,TOBJ(gTP->FCompile_DebugSourceVector),*sourceLineIndex);
			ExitOnError(*sourceLine);
			gTP->FCompile_DebugCurrentSourceLine = *sourceLine;
			if (_In(compilerState) == NIL)
				{
				_In(compilerState) = TStructure_New(gCP,gTP);
				}
			/* Get source lines ==> pcode instruction displacement vector.  */
			*dbgSourceLinesInstr = FSmartbase_Ref(gCP,gTP,2,TOBJ(_In(compilerState)),TOBJ(gCP->FCompile_dbgSourceLinesInstrSYM));
			ExitOnError(*dbgSourceLinesInstr);
			if (dbgSourceLinesInstr->Tag != TYDIRECTORY)
				{
				dbgSourceLinesInstr->u.Directory = TDirectory_New(gCP,gTP);
				dbgSourceLinesInstr->Tag = TYDIRECTORY;
				TStructure_AddNewValue(gCP,gTP,TOBJ(_In(compilerState)),TOBJ(gCP->FCompile_dbgSourceLinesInstrSYM),*dbgSourceLinesInstr);
				}
			if (_Pc(compilerState) == NIL)
				{
				_Pc(compilerState) = TPcodeVector_New(gCP,gTP);
				}
			/* Associate index of the current source line with the current pcode instruction displacement.  */
			pcodeInstr->u.Int = _Pc(compilerState)->itsCurItemIndex;
			pcodeInstr->Tag = TYNUM;
			*ret = FSmartbase_Set(gCP,gTP,3,*dbgSourceLinesInstr,*sourceLineIndex,*pcodeInstr);
			ExitOnError(*ret);
			}
		}
		
    switch((tag = curPair->itsCar.Tag))
        {
        case TYPAIR:
            /*  We call the main recognizer to process this subexpression. */
            /*  It is possible to have Lisp expressions which are lists of lists */
            /*  i.e. ( (+ 1 1) (+ 2 2) ) and this case is needed to process situations such as */
            /*  this. */
            /*  We also do special processing here to identify cases like ((ref array index ) ...) */
            /*  which should compile into function calls. We will need to assume that the array */
            /*  contains executable objects and generate code to call them. */
            
            tmpPair = curPair->itsCar.u.Pair;		
            if ((tmpPair->itsCar.Tag == TYSYMBOL) && (_NestLvlN(compilerState) > 1))
                {
                /*  We assume that this symbol will resolve to a function call of some sort */
                /*  at run time. */
                     
                *ret = FOptimize1_IndirectCall(gCP, gTP, compilerState, curPair);
                if(isERROR(ret)) 
                    goto BadCleanUp;
                
                /*  We return control to the caller after compiling code for a function call. */
                /*  The FOptimize1_IndirectCall code and its descendents has the responsibility to */
                /*  call the compiler recursively to process nested subexpressions. */
                
                curPair = NIL;
                }
            else
                {
                *ret = FCompile_Recognize(gCP, gTP, compilerState, curPair);
                curPair = NIL;
                if(isERROR(ret)) 
                    {
                    goto BadCleanUp;
                    }
                }
        break;
        
        case TYSYMBOL:
            /*  The only symbols which are acceptable at the start of a list are those	*/
            /*  for special forms or function calls, anything else is an error.			*/
            
            /*  Get the symbol object from the car of the current pair. */
            
            aSymbol = curPair->itsCar.u.Symbol;
            
            /*  The tag of the global value of a symbol tells us whether this symbol is associated */
            /*  with a special form or a procedure. */
            
            tag = aSymbol->itsGlobalValue.Tag;

            if ((tag == TYMACRO) || (tag == TYCMACRO))
                {
                *ret = FSpecialForms3_Macro(gCP, gTP, compilerState, curPair, aSymbol->itsGlobalValue);

				/*  Check to see if an error occured */
                if (isERROR(ret)) goto BadCleanUp;
                        
                /*  For all macro special forms in this block we will return control to caller after      */
                /*  they have completed their processing. Each macro special form in this block has       */
                /*  the responsibility to recursively call the compiler to process nested subexpressions. */
                
                curPair = NIL;
				}
            else
			if (tag == TYSPECIALFORM)
                {
                /*  Even when symbols are aliased they will still have the same global value, */
                /*  and the global value contains the symbol associated with a Lisp */
                /*  procedure or special form. These connections are initially made in the  */
                /*  functions FProcedure_NewCProcedure() and FCompile_NewSpecialForm(). */
                
                /*  We check the global value for a match with a recognized special form */
                /*  and perform processing associated with that special form. */
                
                /*  (PTC: NOTE: once we finish the implementation of sorted TStructures (by sym) */
                /*  it will probably be possible to do a binary lookup of function by symbol */
                /*  which will speed up this part of the processing.) */
                
                aSymbol = (TSymbol*)asObject(&aSymbol->itsGlobalValue);
                
                if(aSymbol == gCP->TLambda_goto)
                    {
                    /*  Goto is handled as a special case among special forms. It is the only */
                    /*  special form for whom processing may continue in the main while loop */
                    /*  of FCompile_RecognizeHead after the special form processing has finished. */
                    
                    *ret = FSpecialForms3_goto(gCP, gTP, compilerState, curPair);
                    
                    /*  Check to see if an error occured, or continue */
                    ExitOnError(*ret)
                    /*  continue processing in the recognizehead loop */
                    
                    curPair = _FCompile_CdrPair(curPair);
                    curPair = _FCompile_CdrPair(curPair);
                    }
                else
                    {
                    /*  Handle all other special forms. */
                    
                    if(aSymbol == gCP->FCompile_refmacroSym)
                        {
                        *ret = FSpecialForms3_RefMacro(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoLE)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, VMJMPLE);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoLT)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, VMJMPLT);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoEQ)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, VMJMPEQ);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoNE)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, VMJMPNE);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoGT)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, VMJMPGT);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoGE)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, VMJMPGE);
                        }
                    else
					if(aSymbol == gCP->FCompile_gotoLEb)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpLEInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoLTb)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpLTInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoEQb)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpEQInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoNEb)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpNEInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoGTb)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpGTInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoGEb)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpGEInteger);
                        }
                    else
					if(aSymbol == gCP->FCompile_gotoLEc)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpLEInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoLTc)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpLTInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoEQc)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpEQInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoNEc)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpNEInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoGTc)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpGTInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoGEc)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpGEInteger);
                        }
                    else
					if(aSymbol == gCP->FCompile_gotoLEi)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpLEInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoLTi)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpLTInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoEQi)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpEQInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoNEi)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpNEInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoGTi)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpGTInteger);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoGEi)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpGEInteger);
                        }
                    else
					if(aSymbol == gCP->FCompile_gotoLEn)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpLENumber);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoLTn)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpLTNumber);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoEQn)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpEQNumber);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoNEn)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpNENumber);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoGTn)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpGTNumber);
                        }
                    else
                    if(aSymbol == gCP->FCompile_gotoGEn)
                        {
                        *ret = FSpecialForms3_gotoCC(gCP, gTP, compilerState, curPair, vmnatJmpGENumber);
                        }
                    else
					if(aSymbol == gCP->FCompile_return)
                        {
                        *ret = FSpecialForms3_return(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_defineSym)
                        {
                        *ret = FSpecialForms1_define(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_minusMinusSym)
                        {
                        *ret = FSpecialForms3_MinusMinus(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_plusPlusSym)
                        {
                        *ret = FSpecialForms3_PlusPlus(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_timesEqualsSym)
                        {
                        *ret = FSpecialForms3_TimesEquals(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_minusEqualsSym)
                        {
                        *ret = FSpecialForms3_MinusEquals(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_divEqualsSym)
                        {
                        *ret = FSpecialForms3_DivEquals(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_plusEqualsSym)
                        {
                        *ret = FSpecialForms3_PlusEquals(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_defunSym)
                        {
                        *ret = FSpecialForms1_defun(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_defmacroSym)
                        {
                        *ret = FSpecialForms3_Defmacro(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_defchildSym)
                        {
                        *ret = FSpecialForms3_Defchild(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_defChildLambdaSym)
                        {
                        *ret = FSpecialForms3_Defchild(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_deforphanSym)
                        {
                        *ret = FSpecialForms3_Deforphan(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_defriendSym)
                        {
                        *ret = FSpecialForms3_Defriend(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_defclassSym)
                        {
                        *ret = FSpecialForms3_Defclass(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_defcloneSym)
                        {
                        *ret = FSpecialForms3_Defclone(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_defstructSym)
                        {
                        *ret = FSpecialForms3_Defstruct(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_defvmSym)
                        {
                        *ret = FSpecialForms3_Defvm(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_defmethodSym)
                        {
                        *ret = FSpecialForms3_Defmethod(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_lambdaSym)
                        {
                        *ret = FSpecialForms1_lambda(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_ifSym)
                        {
                        *ret = FSpecialForms2_if(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_whileSym)
                        {
                        *ret = FSpecialForms2_while(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_loopSym)
                        {
                        *ret = FSpecialForms2_loop(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_beginSym)
                        {
                        *ret = FSpecialForms2_begin(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_andSym)
                        {
                        *ret = FOptimize2_andRet(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_notSym)
                        {
                        *ret = FOptimize2_notRet(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_orSym)
                        {
                        *ret = FOptimize2_orRet(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_sendSym)
                        {
                        *ret = FSpecialForms2_send(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_superSym)
                        {
                        *ret = FSpecialForms2_super(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_argcountSym)
                        {
                        curPair = _FCompile_CdrPair(curPair);
                        *ret = FOptimize1_Calls(gCP, gTP, compilerState, curPair, aSymbol);
                        }
                    else
                    if(aSymbol == gCP->FCompile_selfSym)
                        {
                        curPair = _FCompile_CdrPair(curPair);
                        *ret = FOptimize1_Calls(gCP, gTP, compilerState, curPair, aSymbol);
                        }
                    else
                    if(aSymbol == gCP->FCompile_argfetchSym)
                        {
                        *ret = FSpecialForms2_argfetch(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_onerrorSym)
                        {
                        *ret = FSpecialForms2_onError(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_letSym)
                        {
                        *ret = FSpecialForms1_let(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_caseSym)
                        {
                        *ret = FSpecialForms2_case(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_condSym)
                        {
                        *ret = FSpecialForms2_cond(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_setqSym)
                        {
                        *ret = FSpecialForms3_setq(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_setfSym)
                        {
                        *ret = FSpecialForms3_setf(gCP, gTP, compilerState, curPair);
                        }
                    else
                    if(aSymbol == gCP->FCompile_setvSym)
                        {
                        *ret = FSpecialForms3_setv(gCP, gTP, compilerState, curPair);
                        }
                    else
                        {
						*ret = TERROR("!Lisp Compiler: Illegal special form syntax!"); 
						goto BadCleanUp;
                        }
                        
                    /*  Check to see if an error occured */
                    
                    if(isERROR(ret)) 
                        goto BadCleanUp;
                        
                    /*  For all special forms in this block we will return control to caller */
                    /*  after they have completed their processing. Each special form in this block  */
                    /*  has the responsibility to recursively call the compiler to process nested  */
                    /*  subexpressions. */
                    
                    curPair = NIL;
                    }
                }
            else
                {
                
                /*  We assume that this symbol will resolve to a function call of some sort */
                /*  at run time. */
                     
                *ret = FCompile_Call(gCP, gTP, compilerState, curPair);
                if(isERROR(ret)) 
                    goto BadCleanUp;
                
                /*  We return control to the caller after compiling code for a function call. */
                /*  The FCompile_Call code and its descendents has the responsibility to  call the  */
                /*  compiler recursively to process nested subexpressions. */
                
                curPair = NIL;
                }
        break;
        
        default:
            /*  We do not accept anything as the head of a list except another list */
            /*  or a symbol for a special form or function call, or an args, svars, vars, regs or pvars. */
            
			if(tag == TYQUOTEDSYMBOL)
                {
                aSymbol = asSymbol(&curPair->itsCar);
                tmpPair = _FCompile_CdrPair(curPair);
                if ((aSymbol == gCP->TLambda_args)  || 
					(aSymbol == gCP->TLambda_faces) || 
					(aSymbol == gCP->TLambda_rvars) || 
					(aSymbol == gCP->TLambda_vars)  || 
					(aSymbol == gCP->TLambda_pvars) || 
					(aSymbol == gCP->TLambda_svars) || 
					(aSymbol == gCP->TLambda_Doomed) || 
					(aSymbol == gCP->TLambda_cvars))
                    {
                    if ((aSymbol == gCP->TLambda_args) && (tmpPair && asTag(&tmpPair->itsCar) == TYPAIR))
                        *ret = FCompile_args(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                    else
                    if ((aSymbol == gCP->TLambda_faces) && (tmpPair && asTag(&tmpPair->itsCar) == TYPAIR))
                        *ret = FCompile_faces(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                    else
                    if ((aSymbol == gCP->TLambda_rvars) && (tmpPair && asTag(&tmpPair->itsCar) == TYPAIR))
                        *ret = FCompile_rvars(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                    else
                    if ((aSymbol == gCP->TLambda_vars) && (tmpPair && asTag(&tmpPair->itsCar) == TYPAIR))
                        *ret = FCompile_vars(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                    else
                    if ((aSymbol == gCP->TLambda_pvars) && (tmpPair && asTag(&tmpPair->itsCar) == TYPAIR))
                        *ret = FCompile_pvars(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                    else
                    if ((aSymbol == gCP->TLambda_svars) && (tmpPair && asTag(&tmpPair->itsCar) == TYPAIR))
                        *ret = FCompile_svars(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
                    else
                    if ((aSymbol == gCP->TLambda_cvars) && (tmpPair && asTag(&tmpPair->itsCar) == TYPAIR))
                        *ret = FCompile_cvars(gCP, gTP, compilerState, asPair(&tmpPair->itsCar));
					else
					if (aSymbol == gCP->TLambda_Doomed)
						_Dm(compilerState) = TRUE;
                    else
                        {
   						*ret = TERROR("!Lisp: Function missing vars, pvars, cvars, regs, or args!");
                        goto BadCleanUp;
                        }

                    if(isERROR(ret)) 
                        goto BadCleanUp;
                    
                    /*  continue processing in the recognizer loop */
                    
                    curPair = _FCompile_CdrPair(tmpPair);
                    }
                else
                    {
                    /*  A quoted symbol at the start of a list indicates a message send. */
                    /*  We assume that this message symbol will resolve to a function call */
                    /*  at run time. */
                    
                    *ret = FCompile_Call(gCP, gTP,compilerState, curPair);
                    if(isERROR(ret)) 
                        goto BadCleanUp;
                
                    /*  We return control to the caller after compiling code for a function call. */
                    /*  The FCompile_Call code and its descendents has the responsibility to  call the  */
                    /*  compiler recursively to process nested subexpressions. */
                
                    curPair = NIL;
                    }
                }
            else
                {
				*ret = TERROR("!Lisp: Missing or Unrecognized Function call!");
                goto BadCleanUp;
                }
        break;
        
        }
    } while (curPair != NULL);
    
/*  The return value from specialform or function call processing is the same as  */
/*  _LastResult(compilerState) (see FCompile_SetupProc()). */

FrameExit(*ret);

BadCleanUp:

if(asTag(ret) != TYERROR)
    {
	/* If no specific error is set, create a generic error message */
	*ret = FSmartbase_Error(gCP, gTP,"!Lisp: Bad Expression!");
    }
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_SetupRegs

Set up the specified Lambda with a basic register variables structure. The basic register
variables are the Gv, Sv, Av, Tv, Pv, Cv, and Rv registers. All should be set to type CharPointer. 

Note:   These registers are used to support the basic addressing modes:

		AMGVOFFSET
		AMSVOFFSET
		AMAVOFFSET
		AMTVOFFSET
		AMPVOFFSET
		AMCVOFFSET
		AMREGISTER

#endif

TVAL    FCompile_SetupRegs(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* Lambda)
{
StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,22);
EndFrame

/* We only set up the register structure if it is empty. */

if (Lambda->RegisterVariables != NIL) FrameExit(TOBJ(Lambda->RegisterVariables));

/* Create the basic register variables structure. */

prmv[0] = TSYMBOL("Gv");
prmv[1].u.Int = 0;
prmv[1].DeclaredType = TYCHARPOINTER;
prmv[1].Tag = TYNUM;

prmv[2] = TSYMBOL("Sv");
prmv[3].u.Int = 0;
prmv[3].DeclaredType = TYCHARPOINTER;
prmv[3].Tag = TYNUM;

prmv[4] = TSYMBOL("Av");
prmv[5].u.Int = 0;
prmv[5].DeclaredType = TYCHARPOINTER;
prmv[5].Tag = TYNUM;

prmv[6] = TSYMBOL("Tv");
prmv[7].u.Int = 0;
prmv[7].DeclaredType = TYCHARPOINTER;
prmv[7].Tag = TYNUM;

prmv[8] = TSYMBOL("Pv");
prmv[9].u.Int = 0;
prmv[9].DeclaredType = TYCHARPOINTER;
prmv[9].Tag = TYNUM;

prmv[10] = TSYMBOL("Cv");
prmv[11].u.Int = 0;
prmv[11].DeclaredType = TYCHARPOINTER;
prmv[11].Tag = TYNUM;

prmv[12] = TSYMBOL("Rv");
prmv[13].u.Int = 0;
prmv[13].DeclaredType = TYCHARPOINTER;
prmv[13].Tag = TYNUM;

prmv[14] = TSYMBOL("__Ri");
prmv[15].u.Int = 0;
prmv[15].DeclaredType = TYNUM;
prmv[15].Tag = TYNUM;

prmv[16] = TSYMBOL("__Rp");
prmv[17].u.Int = 0;
prmv[17].DeclaredType = TYNUM;
prmv[17].Tag = TYNUM;

prmv[18] = TSYMBOL("__RNx");
prmv[19].u.Real = 0.0;
prmv[19].DeclaredType = TYREAL;
prmv[19].Tag = TYREAL;

/* ********************************************************************** */
/* If this deisplacement changes must also change FOptimize_OptimizePcode! */
/* ********************************************************************** */
prmv[20] = TSYMBOL("lnE");
prmv[21].u.Real = 2.718281828459;
prmv[21].DeclaredType = TYREAL;
prmv[21].Tag = TYREAL;

*ret = FMake_Structure(gCP,gTP,22,&prmv[0]);
ExitOnError(*ret);
Lambda->RegisterVariables = ret->u.Structure;

/* Return the Lambda Register Variables as updated. */

FrameExit(TOBJ(Lambda->RegisterVariables));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_SetupVars

Set up the specified Lambda with a basic temporary variables structure. The basic temporary
variables includes the __Tx variable. 

#endif

TVAL    FCompile_SetupVars(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* Lambda)
{
StartFrame
DeclareTVAL(ret);
DeclareTVALArray(prmv,18);
EndFrame

/* We only set up the temporary structure if it is empty. */

if (Lambda->TemporaryVariables != NIL) FrameExit(TOBJ(Lambda->TemporaryVariables));

/* Create the basic temporary variables structure. */

prmv[0] = TSYMBOL("__Tx");
prmv[1].u.Int = 0;
prmv[1].DeclaredType = TYTVAL;
prmv[1].Tag = TYVOID;

*ret = FMake_Structure(gCP,gTP,2,&prmv[0]);
ExitOnError(*ret);
Lambda->TemporaryVariables = ret->u.Structure;

/* Return the Lambda Temporary Variables as updated. */

FrameExit(TOBJ(Lambda->TemporaryVariables));
}



/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_SetupProc

Setup a new procedure object and a compilation state. Return proc if successful. 

The data objects used to manage the compiler state are stored in a TVector an accessed via
Macros  as indicated in FCompile.h and as shown here:

_Temp(compilerState)

    A TVAL containing three integer indices utilized to track temporary variable usage. 
    Temporary variables are managed as a simple stack with the exception of the variable
    which is allocated as the return for special forms. As we descend through nested expressions
    during a compilation we will reuse this return var for all special form code generation, this
    helps us to minimize temp var creation.
    
    See below for descriptions of how each index is used:
    
    _TempN(compilerState)

        An short integer index used to track which temporary variable is 
        available for use in code generation for function calls. Three temporaries are 
        allocated for each lambda object before its vars are allocated.
    
    _TempBaseN(state)
        
        An short integer index used to store the base of temporary variable allocation in the Tv.
        This will occur after the allocation of any vars.
                
    _SFReturnN(compilerState)
    
        An short integer index used to track which temporary variable is available for 
        use in code generation for returns from special forms.
                
    _NestLvlN(compilerState)
    
        An short integer index used to track the parenthetical nesting level during compilation.
        If we know that we are at level 0 then we may generate VMRETURN instructions directly
        and avoid moving data into temporary result locations.
                
    _NotSwN(compilerState)
    
        An short integer value used to maintain the value of the not switch, which gets toggled each
        time we encoutner a (not special form.
                
_CurProc(compilerState)

    A TVAL containting  the current procedure object (the compilation target).
    which will contain among other things the following:
                
    _Av(compilerState)  :   A tval for the current Argument variables Structure.
    _Tv(compilerState)  :   A tval for the current Temporary variables Structure.
    _Pv(compilerState)  :   A tval for the current Permanent variables Structure.
    _Pc(compilerState)  :   A tval for the current PCode vector.
    _Sc(compilerState)  :   A tval for the current source tracking vector.

_Result(compilerState)

    A TVAL containing the symbol for the result of an expression. This is used internally
    by the various compiler procedures to keep track of the requested result of the compilation
    of an expression. In cases where side effects could occur by using the requested location, or
    if no location is specified, the compiler temporary variable allocation scheme would be 
    used to determine the result of an expression
    
    Consider the Lisp expression:
    
        (+ 1 1)
    
    which would compile to:
    
        0000: addi    aminteg aminteg amfboff 1                1                __T0            
        0004: return  amfboff amvoid  amvoid   __T0            

    In this example no target has been specified as the result of the expression and
    so the compiler uses the first available temporary to return the result.
    
    In contrast consider the Lisp expression:
    
    (setq foo 1)
    
    which would compile to:
    
         0000: move    aminteg amgvoff amvoid  1                foo             
         0003: return  amgvoff amvoid  amvoid   foo             
         
    Before the compiler called the setq special form it would set _Result(compilerState) to contain
    foo, then the setq code will examine the parse tree for the expression and determine whether
    it could place the result directly into foo or whether it should use the compilers allocation
    scheme for intermediate results before moving the final result into foo.

    In either case once code has been generated by a compiler procedure it must set 
    _LastResult(compilerState) (see below) to indicate the location for the result of the 
    instruction just compiled.
    
_LastResult(compilerState)

    A TVAL containing the symbol for the result of the last instruction for which code
    was generated.
                
_Labels(compilerState)

    A TVAL containing a Structure of label-pcode offset bindings. (See FCompile_ResolveGotos.)

_Gotos(compilerState)

    A TVAL containing a Structure of goto-pcode offset bindings.  (See FCompile_ResolveGotos.)


_Jumps(compilerState)

    A TVAL containing a sorted TIntVector of all jump instructions in this lambda object.

#endif

TVAL    FCompile_SetupProc(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState)
{
StartFrame
DeclareOBJ(TStructure,tmpEnv);
DeclareOBJ(TPcodeVector,tmpPcode);
DeclareOBJ(TVector,tmpVec);
DeclareOBJ(TIntVector,tmpIntVec);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(proc);
EndFrame

/*  Allocate a Structure for Self  constant variables (Sv). */

if (_Sv(compilerState)==NIL)
    {
    /*  If this procedure object has not already inherited its Sv Structure */
    /*  then we must allocate one here. (See FCompile_Compile for more information.) */
    
    _Sv(compilerState) = tmpEnv = TStructure_New(gCP,gTP);
    _CurProcP(compilerState)->ClassVariables = _Sv(compilerState);
    }

/*  Allocate a Structure for Persistant  constant variables (Cv). */

if (_Cv(compilerState)==NIL)
    {
    /*  If this procedure object has not already inherited its Cv Structure */
    /*  then we must allocate one here. (See FCompile_Compile for more information.) */
    
    _Cv(compilerState) = tmpEnv = TStructure_New(gCP,gTP);
    _CurProcP(compilerState)->ConstantVariables = _Cv(compilerState);
    }

/*  Allocate a Structure for Persistant variables (Pv). */

if (_Pv(compilerState)==NIL)
    {
    /*  If this procedure object has not already inherited its Pv Structure */
    /*  then we must allocate one here. (See FCompile_Compile for more information.) */
    
    _Pv(compilerState) = tmpEnv = TStructure_New(gCP,gTP);
    _CurProcP(compilerState)->PersistantVariables = _Pv(compilerState);
    _Dbg(compilerState).u.Object = (TObject*)NIL;
    _Dbg(compilerState) = gCP->Tval_VOID;
    }
else
    {
    if ((_PvP(compilerState)->itsDictionaryArray != NIL) &&
        (atHMBind(_PvP(compilerState)->itsDictionaryArray,0).Key == (TObject*)gCP->FDebug_dbgVec))
        {
        _Dbg(compilerState).u.Object = (TObject*)(atHMBind(_PvP(compilerState)->itsDictionaryArray,0).Value.u.Object);
        _Dbg(compilerState).Tag = TYVECTOR;
        }
    }
    
/*  Allocate a Structure for tracking gotos and initialize the current compiler state */
/*  to use it. */

tmpEnv = TStructure_New(gCP,gTP);
_FCompile_MakeOBJ(_Gotos(compilerState), tmpEnv);

/*  Allocate a Structure for tracking labels and initialize the current compiler state */
/*  to use it. */

tmpEnv = TStructure_New(gCP,gTP);
_FCompile_MakeOBJ(_Labels(compilerState), tmpEnv);

/*  Allocate Pcode Vector and initialize the current compiler state to use it. */

_Pc(compilerState) = tmpPcode = TPcodeVector_New(gCP,gTP);

/*  Allocate  Vector for tracking jump instructions and init the current compiler state to use it. */

tmpIntVec = TIntVector_New(gCP,gTP);
_FCompile_MakeOBJ(_Jumps(compilerState), tmpIntVec);

/*  Allocate a Structure for Argument variables (Av). */

_Av(compilerState) = tmpEnv = TStructure_New(gCP,gTP);

/*  Initialize control indices for temporary variable management. */

_TempBaseN(compilerState) = _TempN(compilerState) = _NestLvlN(compilerState) = 0;
_NotSwN(compilerState) = 1;
_SFReturnN(compilerState) = -1;

/*  Return the correctly initialized procedure object. */

FrameExit(*proc);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_ResolveTemp

The logic for FCompile_ResolveTemp is similar to that for FCompile_LookUp with the exception that
FCompile_Resolve will create the specified key in the search structures if it does not yet exist. 
It is called in special form processing for the define special form or for the let special form. 
See the Lisp Ref. Guide for a description of how lambda nesting affects variable binding in 
these two cases.

#endif

TVAL    FCompile_ResolveTemp(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* theSymbol, NUM envMod, TVAL prefValue)
{
NUM                 len;
NUM                 ndx;
LpCHAR				sp;
StartFrame
DeclareOBJ(TStructure,tmpEnv);
DeclareTVAL(ret);
DeclareTVAL(key);
DeclareTVAL(envTval);
EndFrame


/* ************************************************************** */
/* Start recognition of explicit global variable name references */
/* ************************************************************** */

if (strncmp(objSymbolArray(theSymbol),"Gv:",3) == 0)
	{
	/* Return the variable name as a global reference. */
	sp = &objSymbolArray(theSymbol)[3];
	*ret = TSYMBOL(sp);
	ret->Modifier = AMGVOFFSET; 
	ret->Offset = 0; 
	ret->DeclaredType = TYVOID;

	FrameExit(*ret);
	}
	
/* ************************************************************** */
/* Start recognition of register offset addressing variable names */
/* ************************************************************** */

if (strcmp(objSymbolArray(theSymbol),"regoffset:") == 0)
	{
	*ret = FCompile_LookUp(gCP,gTP,compilerState,theSymbol);
	FrameExit(*ret);
	}

/* ************************************************************** */
/* Stop recognition of register offset addressing variable names  */
/* ************************************************************** */

StartSearch:
switch(envMod)
    {
    case AMCVOFFSET: *envTval = TOBJ(_Cv(compilerState));goto SearchAndInsert;
    case AMPVOFFSET: *envTval = TOBJ(_Pv(compilerState));goto SearchAndInsert;
    case AMTVOFFSET: *envTval = TOBJ(_Tv(compilerState));goto SearchAndInsert;
    case AMREGISTER: *envTval = TOBJ(_Rv(compilerState));goto SearchAndInsert;
    case AMSVOFFSET: *envTval = TOBJ(_Sv(compilerState));goto SearchAndInsert;
    case AMAVOFFSET: *envTval = TOBJ(_Av(compilerState));

		/*  Construct a key for the given item and if it does not exist then create it. */

        SearchAndInsert:
        tmpEnv = asStructure(envTval);
        len = tmpEnv->itsMaxItemIndex;
        for(ndx = 0; ndx < len; ndx++)
            {
            if (atHMBind(tmpEnv->itsDictionaryArray,ndx).Key == (TObject*)theSymbol)
                {
                ret->Offset = ndx;
                ret->Modifier = envMod;
                ret->u.Symbol = theSymbol;
				ret->DeclaredType = atHMBind(tmpEnv->itsDictionaryArray,ndx).Value.DeclaredType;
                ret->Tag = TYSYMBOL;
                FrameExit(*ret);
                }
            }
        key->u.Symbol = theSymbol;
        key->Tag = TYSYMBOL;
        
		if ((envMod == AMREGISTER) && (_Rv(compilerState) != NIL) && (_Rv(compilerState)->itsMaxItemIndex >= (MAXREGISTERCNT - 1)) && (strncmp(SymbolArray(*key),"__R",3) == 0))
			{
			envMod = AMTVOFFSET;
			goto StartSearch;
			}
        
		TStructure_SetIV1(gCP,gTP,*envTval, *key, prefValue);

		if ((_Rv(compilerState) != NIL) && (_Rv(compilerState)->itsMaxItemIndex > MAXREGISTERCNT))
			{
			FrameExit(TERROR("!compile: an Lambda cannot have more than 50 register variables!"));
			}
        
        ret->Offset = len;
        ret->Modifier = envMod;
		ret->DeclaredType = TYVOID;
        ret->u.Symbol = theSymbol;
        ret->Tag = TYSYMBOL;
    break;
    
    case AMGVOFFSET:
        ret->Offset = 0;
        ret->Modifier = AMGVOFFSET;
		ret->DeclaredType = TYVOID;
        ret->u.Symbol = theSymbol;
        ret->Tag = TYSYMBOL;
    break;
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_Resolve

The logic for FCompile_Resolve is similar to that for FCompile_LookUp with the exception that
FCompile_Resolve will create the specified key in the search structures if it does not yet exist. 
It is called in special form processing for the define special form or for the let special form. 
See the Lisp Ref. Guide for a description of how lambda nesting affects variable binding in 
these two cases.

#endif

TVAL    FCompile_Resolve(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* theSymbol, NUM envMod)
{
NUM                 len;
NUM                 ndx;
LpCHAR				sp;
StartFrame
DeclareOBJ(TStructure,tmpEnv);
DeclareTVAL(ret);
DeclareTVAL(key);
DeclareTVAL(envTval);
EndFrame


/* ************************************************************** */
/* Start recognition of explicit global variable name references */
/* ************************************************************** */

if (strncmp(objSymbolArray(theSymbol),"Gv:",3) == 0)
	{
	/* Return the variable name as a global reference. */
	sp = &objSymbolArray(theSymbol)[3];
	*ret = TSYMBOL(sp);
	ret->Modifier = AMGVOFFSET; 
	ret->Offset = 0; 
	ret->DeclaredType = TYVOID;

	FrameExit(*ret);
	}
	
/* ************************************************************** */
/* Start recognition of register offset addressing variable names */
/* ************************************************************** */

if (strcmp(objSymbolArray(theSymbol),"regoffset:") == 0)
	{
	*ret = FCompile_LookUp(gCP,gTP,compilerState,theSymbol);
	FrameExit(*ret);
	}

/* ************************************************************** */
/* Stop recognition of register offset addressing variable names  */
/* ************************************************************** */

StartSearch:
switch(envMod)
    {
    case AMCVOFFSET: *envTval = TOBJ(_Cv(compilerState));goto SearchAndInsert;
    case AMPVOFFSET: *envTval = TOBJ(_Pv(compilerState));goto SearchAndInsert;
    case AMTVOFFSET: *envTval = TOBJ(_Tv(compilerState));goto SearchAndInsert;
    case AMREGISTER: *envTval = TOBJ(_Rv(compilerState));goto SearchAndInsert;
    case AMSVOFFSET: *envTval = TOBJ(_Sv(compilerState));goto SearchAndInsert;
    case AMAVOFFSET: *envTval = TOBJ(_Av(compilerState));

		/*  Construct a key for the given item and if it does not exist then create it. */

        SearchAndInsert:
        tmpEnv = asStructure(envTval);
        len = tmpEnv->itsMaxItemIndex;
        for(ndx = 0; ndx < len; ndx++)
            {
            if (atHMBind(tmpEnv->itsDictionaryArray,ndx).Key == (TObject*)theSymbol)
                {
                ret->Offset = ndx;
                ret->Modifier = envMod;
                ret->u.Symbol = theSymbol;
				ret->DeclaredType = atHMBind(tmpEnv->itsDictionaryArray,ndx).Value.DeclaredType;
                ret->Tag = TYSYMBOL;
                FrameExit(*ret);
                }
            }
        key->u.Symbol = theSymbol;
        key->Tag = TYSYMBOL;
        
		if ((envMod == AMREGISTER) && (_Rv(compilerState) != NIL) && (_Rv(compilerState)->itsMaxItemIndex >= (MAXREGISTERCNT - 1)) && (strncmp(SymbolArray(*key),"__R",3) == 0))
			{
			envMod = AMTVOFFSET;
			goto StartSearch;
			}
        
		TStructure_SetIV1(gCP,gTP,*envTval, *key, gCP->TObject_VOID);

		if ((_Rv(compilerState) != NIL) && (_Rv(compilerState)->itsMaxItemIndex > MAXREGISTERCNT))
			{
			FrameExit(TERROR("!compile: an Lambda cannot have more than 50 register variables!"));
			}
        
        ret->Offset = len;
        ret->Modifier = envMod;
		ret->DeclaredType = TYVOID;
        ret->u.Symbol = theSymbol;
        ret->Tag = TYSYMBOL;
    break;
    
    case AMGVOFFSET:
        ret->Offset = 0;
        ret->Modifier = AMGVOFFSET;
		ret->DeclaredType = TYVOID;
        ret->u.Symbol = theSymbol;
        ret->Tag = TYSYMBOL;
    break;
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_FixupJumps

This function is called from FCompile_FixupRecalcs in order to adjust the absolute jump
offsets which are stored in the jump instructions of a pcode vector. To see why this happens
please refer to the header for the FCompile_FixupRecalcs function.

We maintain a TIntVector with the offset locations for every conditional and unconditional
jump instruction in a pcode vector and we use this vector to fixup the pcode vector as requested.
We actually store the offset after the instruction, this provides us with a consistent way
to get to the target location for the jump for any kind of jump with any kind of arguments.

#endif

TVAL    FCompile_FixupJumps(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, NUM delta)
{
NUM                 target;
NUM                 cn;
NUM                 max;
HMInt               pcodes;
HMInt               jumps;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  Setup handles to the instruction and jump vectors. */

pcodes = _PcP(compilerState)->itsInstructionArray;
jumps  = _JumpsP(compilerState)->itsIntArray;
max = _JumpsP(compilerState)->itsMaxItemIndex;
    
for(cn = 0; cn < max; cn++)
    {
    /*  Loop through the jump vector, extracting the location for the target of every */
    /*  jump instruction in the procedure object. */
    
    target = atHMInt(jumps,cn);
        
    /*  Fix up jump instructions in the procedure object. */
    
    atHMInt(pcodes,abs(target)) += delta;
    
    /*  Keep the jumplist up to date (it may be used to build a flow-graph later.) */
    
    if(target < 0)
        atHMInt(jumps,cn) -= delta;
    else
        atHMInt(jumps,cn) += delta;
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_WritePcode

This function provides an interface between the compiler and the FVmCode_GeneratePcode
procedure. A great deal of instruction optimization and administrative work is performed
in this function.

Notes:

	It is responsible for converting any AMWORD constant arguments into Class Variable 
	constants (duplicate constants are avoided).

	It is responsible for maintaining the jump list (see FCompile_FixupJumps and 
	FCompile_FixupRecalcs).

	It is responsible for converting memory instructions into equivalent register 
	instructions (when possible).

	The compile function never calls FVmCode_GeneratePcode directly. 

#endif

extern  TVAL    FCompile_WritePcode(LpXCONTEXT gCP, LpTHREAD gTP, TVector* state, TVAL wrdOpcode,TVAL opnd1,TVAL opnd2,TVAL opnd3);

TVAL    FCompile_WritePcode(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TVAL wrdOpcode,TVAL opnd1,TVAL opnd2,TVAL opnd3)
{
TVAL		argv[8];
StartFrame
DeclareTVAL(ret);
EndFrame

argv[0] = TOBJ(_Pc(compilerState));
argv[1] = wrdOpcode;
argv[2] = TINT(opnd1.Modifier);
argv[5] = opnd1;
argv[3] = TINT(opnd2.Modifier);
argv[6] = opnd2;
argv[4] = TINT(opnd3.Modifier);
argv[7] = opnd3;

*ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,8,argv);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_HashTval

Given some unique data in a TVAL generate a hash table index which is randomized for the
tablesize given. Return it as an integer in a tval.
 
The goal of the algorithm is to take arbitrary keys and hash them for a random distribution
over range of indices in the hashtable, i.i from 0 to tablesize.

#endif

TVAL FCompile_HashTval(LpXCONTEXT gCP, LpTHREAD gTP, NUM tableSize, NUM start, NUM len, TVAL key)
{
register NUM    cn, h = 0, h1;
LpCHAR          tstP;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  Get a pointer into the tval at the specified location */

tstP = ((LpCHAR)&(key)) + start;

for(cn = 0; cn < len; cn++)
    {
    /*  Loop through every byte in the specified range and hash together by shifting and */
    /*  exclusive-oring with the cumulative result. */
    
    h1 = h;
    h = h << 1;
    if(h1 < 0) h |= 1;
    h ^= *tstP++;
    }
    
if(h == 0)
    {
    asInt(ret) = 0;
    asTag(ret) = TYNUM;
    }
else
    {
    /*  Take the result modulo the tablesize to get the index value. */
    
    asInt(ret) = h % tableSize;
    asTag(ret) = TYNUM;
    }
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_HashString

Given some  data in a string generate a hash table index which is randomized for the
tablesize given. Return it as an integer in a tval.
 
The goal of the algorithm is to take arbitrary keys and hash them for a random distribution
over range of indices in the hashtable, i.e. from 0 to tablesize.

#endif

TVAL FCompile_HashString(LpXCONTEXT gCP, LpTHREAD gTP, NUM tableSize, LpCHAR keyIn)
{
register NUM        h = 0;
register LpCHAR     key = keyIn;
StartFrame
DeclareTVAL(ret);
EndFrame

if (key == NIL)
    {
    ret->Tag = 0;
    ret->u.Int = TYNUM;
    }
else
while (*key)
    {
    /*  Loop through every byte in the specified string and hash together by shifting and */
    /*  exclusive-oring with the cumulative result. */

    h += *key++;
    }
    
if(h == 0)
    {
    ret->Tag = 0;
    ret->u.Int = TYNUM;
    }
else
    {
    /*  Take the result modulo the tablesize to get the index value. */
    
    ret->u.Int = abs(h) % tableSize;
    ret->Tag = TYNUM;
    }
    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_Bind

Process a list of symbols possibly with initial values and bind them in the specified Structure.
The list will appear as follows:

	(x Number:y (z 45) (String:w "Hello there"))

In the above list, x and y are declared without initial values while z is declared with an initial value of 45.
Also the declared preferred type of each variable is as follows: x is Word, y is declared with a preferred type
of Number,

#endif

TVAL    FCompile_Bind(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, TStructure* whichEnv )
{
NUM                 modifier;
NUM                 pass;
NUM                 maxPass;
BOLE				allowExpressions = TRUE;
StartFrame
DeclareOBJ(TVector,sortedList);
DeclareOBJ(TPair,savPair);
DeclareOBJ(TPair,nxtPair);
DeclareTVAL(ret);
DeclareTVAL(ptype);
DeclareTVAL(name);
DeclareTVAL(constant);
DeclareTVAL(save);
DeclareTVAL(envTval);
DeclareOBJ(TPair,conPair);
DeclareOBJArray(TPair,inits,20);
EndFrame

/*  See FCompile_Lookup for a description of how the compiler manages symbol */
/*  scoping and resolution for arbitrarily nested lambda objects. */

/*	Are we binding to the regs:(), vars:(), svars:(), cvars:(), pvars:(), or global Structures? */

if(compilerState == NULL)
    {
    modifier = AMGVOFFSET;
	allowExpressions = FALSE;
    maxPass = 2;
    }
else
if(whichEnv == _Tv(compilerState))
    {
    modifier = AMTVOFFSET;
	allowExpressions = FALSE;
    maxPass = 2;
    }
else
if(whichEnv == _Sv(compilerState))
    {
    modifier = AMSVOFFSET;
    maxPass = 2;
	allowExpressions = FALSE;
    }
else
if(whichEnv == _Pv(compilerState))
    {
    modifier = AMPVOFFSET;
	allowExpressions = FALSE;
    maxPass = 1;
    }
else
if(whichEnv == _Cv(compilerState))
    {
    modifier = AMCVOFFSET;
	allowExpressions = FALSE;
    maxPass = 1;
    }
else
if(whichEnv == _Rv(compilerState))
    {
    modifier = AMREGISTER;
	allowExpressions = FALSE;
    maxPass = 1;
    }
else
    {
    maxPass = 1;
	allowExpressions = TRUE;
    modifier = AMGVOFFSET;
    }

envTval->u.Structure = whichEnv;
envTval->Tag = whichEnv->itsObjectType;
  
/* Loop through each variable type:name pair in the outter variable list */  
savPair = curPair;
for (pass = 0; pass < maxPass; pass++)
    {
    /* Each variable entry, in the outter list, has one of   */  
    /*  the following formats:                               */  
    /*     ...                                               */  
    /*     name                                              */  
    /*     type:name                                         */  
    /*     (name)                                            */  
    /*     (name constant)                                   */  
    /*     (type:name)                                       */  
    /*     (type:name constant)                              */  
    curPair = savPair;
    if (curPair != NIL)
        {
        do
            {
            /* Format:   ...                                 */
            if (curPair->itsCar.Tag == TYELLIPSES)
                {
			    /* Note: This section is only valid for args:() bindings. */
                if(whichEnv == _Av(compilerState))
                    {
                    /*  Tag the cdr with TYELLIPSES to indicate that this is a variable arglist. */                   
                    whichEnv->itsCdr = curPair->itsCar;
                    break;
                    }
                else
                    goto BadCleanUp;
                break;
                }
            else
            /* Format:   name                                */
            if (curPair->itsCar.Tag == TYSYMBOL)
                {
                /* Format:   name                              */  
                /* This is a symbol with no preferred type and */
                /*  no constant initializer. Add this binding  */
                /*  to the correct Structure.                  */
                 
				*constant = FCompile_BuildInitializer(gCP,gTP,modifier,gCP->TObject_VOID,curPair->itsCar,gCP->TObject_VOID);
				ExitOnError(*constant);  
				TStructure_SetIV1(gCP,gTP,*envTval,curPair->itsCar,*constant);

                curPair = _FCompile_CdrPair(curPair);
                }
            else
            /* Format:   type:name                            */
            if ((curPair->itsCar.Tag == TYQUOTEDSYMBOL) &&
                (curPair->itsCdr.Tag == TYPAIR) &&			    
                ((nxtPair = curPair->itsCdr.u.Pair)->itsCar.Tag == TYSYMBOL))
                {
                /* Format:   type:name                        */  
                /* This is a symbol with a preferred type and */
                /*  no constant initializer. Add this binding */
                /*  to the correct Structure.                 */
                    
				*constant = FCompile_BuildInitializer(gCP,gTP,modifier,curPair->itsCar,nxtPair->itsCar,gCP->TObject_VOID);  
				ExitOnError(*constant);  
				TStructure_SetIV1(gCP,gTP,*envTval,nxtPair->itsCar,*constant);

                curPair = _FCompile_CdrPair(nxtPair);
                }
            else
			/* Formats:   (name)                                     */  
			/*            (name constant)                            */  
			/*            (type:name)                                */  
			/*            (type:name constant)                       */  
            if ((curPair->itsCar.Tag == TYPAIR) && ((compilerState == NULL) || (whichEnv != _Av(compilerState))))
                {
                /* Convert the list into an array of Pairs for easier processing. */
                _FMacro_PairsToArray(asPair(&curPair->itsCar), inits, sizeof(inits)/sizeof(inits[0]));

				/* There should be no more than three entries in the list. */
				/* Note: The maximum should be (type: name constant)       */
				if (inits[3] != NIL) goto BadCleanUp;

				/* Format: (name)										   */
                if ((inits[0] != NIL) && (inits[1] == NIL) && (inits[0]->itsCar.Tag == TYSYMBOL))
                    {
					/* Format:   (name)                            */  
					/* This is a symbol with no preferred type and */
					/*  no constant initializer. Add this binding  */
					/*  to the correct Structure.                  */
                 
					*constant = FCompile_BuildInitializer(gCP,gTP,modifier,gCP->TObject_VOID,curPair->itsCar,gCP->TObject_VOID);  
					ExitOnError(*constant);  
					TStructure_SetIV1(gCP,gTP,*envTval,curPair->itsCar,*constant);

					curPair = _FCompile_CdrPair(curPair);
					}
				else
				/* Format: (type: name)									   */
                if ((inits[0] != NIL) && 
				    (inits[1] != NIL) && 
					(inits[2] == NIL) && 
					(inits[0]->itsCar.Tag == TYQUOTEDSYMBOL) && 
					(inits[1]->itsCar.Tag == TYSYMBOL))
                    {
					/* Format:   (type: name)                     */  
					/* This is a symbol with a preferred type and */
					/*  no constant initializer. Add this binding */
					/*  to the correct Structure.                 */
                    
					*constant = FCompile_BuildInitializer(gCP,gTP,modifier,inits[0]->itsCar,inits[1]->itsCar,gCP->TObject_VOID);  
					ExitOnError(*constant);  
					TStructure_SetIV1(gCP,gTP,*envTval,inits[1]->itsCar,*constant);

					curPair = _FCompile_CdrPair(curPair);
					}
				else
				/* Format: (type: name constant)						   */
                if ((inits[0] != NIL) && 
				    (inits[1] != NIL) && 
				    (inits[2] != NIL) && 
					(inits[3] == NIL) && 
					(inits[0]->itsCar.Tag == TYQUOTEDSYMBOL) && 
					(inits[1]->itsCar.Tag == TYSYMBOL))
                    {
					/* Format:   (type: name constant)            */  
					/* This is a symbol with a preferred type and */
					/*  a constant initializer. Add this binding  */
					/*  to the correct Structure.                 */
                    
					*ptype    = inits[0]->itsCar;
					*name     = inits[1]->itsCar;
					*constant = inits[2]->itsCar;
					conPair         = inits[2];
					goto AddBinding;
					}
				else
				/* Format: (name constant)								   */
                if ((inits[0] != NIL) && (inits[1] != NIL) && (inits[2] == NIL) &&(inits[0]->itsCar.Tag == TYSYMBOL))
                    {
					*ptype    = inits[0]->itsCar;
					*name     = inits[0]->itsCar;
					*constant = inits[1]->itsCar;
					conPair         = inits[1];

                    /*  Add this binding to the correct Structure. */
					AddBinding:
                    
                    if (isPair(constant))
                        {
                        /*  We generate new procedure objects for expressions like */
                        /*  vars:((x (lambda() (writeln "hi")))) */
                        
                        /*  Use FCompile_Resolve to create the var in the env */
                        /*  and setup the return location for the recognizer a la */
                        /*  (let ((x ...))) */
                        /*  and then let the recognizer to the job of generating code */
                        /*  for the initializing expression and placing it in the */
                        /*  var. */
                        
                        /*  Note that this should be a two pass operation */
                        /*  One pass to setup the envs and the second for */
                        /*  code generation otherwise there could be a problem */
                        /*  with the temporary allocation scheme/usage. */

						if (allowExpressions == FALSE)
							{
							*constant = FCompile_BuildInitializer(gCP,gTP,modifier,*ptype,*name,*constant);  
							ExitOnError(*constant);  
							TStructure_SetIV1(gCP,gTP,*envTval, *name, *constant);
							}
						else
							{
							*save = _Result(compilerState);
                        
							if(((modifier == AMTVOFFSET) && (pass == 0)) || (modifier != AMTVOFFSET))
								{
								_Result(compilerState) = FCompile_Resolve(gCP, gTP, compilerState, asSymbol(name), modifier);
								}
							/* install the symbol in the appropriate structure before parsing the rest of expression */
							*constant = FCompile_BuildInitializer(gCP,gTP,modifier,*ptype,*name,*constant);
							ExitOnError(*constant);  
							TStructure_SetIV1(gCP,gTP,*envTval, *name, *constant);
							if(((modifier == AMTVOFFSET) && (pass == 1)) || (modifier != AMTVOFFSET))
								{
								*ret = FCompile_Recognize(gCP, gTP, compilerState, conPair);
								_LastResult(compilerState) = *ret;
								}
                        
							_Result(compilerState) = *save;
							}
                        }
                    else
                        {
                        if(((modifier == AMTVOFFSET) && (pass == 0)) || (modifier != AMTVOFFSET))
							{
							*constant = FCompile_BuildInitializer(gCP,gTP,modifier,*ptype,*name,*constant);  
							ExitOnError(*constant);  
							TStructure_SetIV1(gCP,gTP,*envTval, *name, *constant);
							}
                        }
    
                    curPair = _FCompile_CdrPair(curPair);
                    }
                else
                    {
                    goto BadCleanUp;
                    }
                }
            else
                goto BadCleanUp;
            
            } while(curPair != NULL);
            
        *ret = gCP->TObject_TRUE;
        }
    }

if ((compilerState != NULL) && (_Rv(compilerState) != NIL) && (_Rv(compilerState)->itsMaxItemIndex > MAXREGISTERCNT))
	{
	FrameExit(TERROR("!compile: an Lambda cannot have more than 50 register variables!"));
	}
   
FrameExit(*ret);

BadCleanUp:
*ret = TERROR("!compile: invalid variable declaration!");
FrameExit(*ret);

}



/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_SetupArg

Setup a modifier tval and a value tval formatted for use in generating instructions to 
appendpcode.

#endif

TVAL    FCompile_SetupArg(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TVAL car, LpTVAL modTvalP, LpTVAL valTvalP )
{
TYPE                tag;
StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TStructure,cVars);
DeclareTVAL(ret);
EndFrame

/*  Get the type tag. */

tag = asTag(&car);

switch(tag)
    {
    case TYPAIR:

        tmpPair = TPair_New(gCP,gTP);
        tmpPair->itsCar = car;
        *ret = FCompile_Recognize(gCP, gTP, compilerState, tmpPair);
        ExitOnError(*ret);
        if(ret->Tag == TYVOID)
            {
   			*ret = TERROR("!Lisp: Reference to an undefined expression!");
			goto BadCleanUp;
            }
        _FCompile_MakeINT(*modTvalP, asModifier(ret));
        _FCompile_MakeSYM(*valTvalP, *ret);
    break;
    
    case TYSYMBOL:
        SymbolProcessing:
            
        /*  This is a symbol reference, search Av Tv Pv Gv to resolve it.  */
        
        aSymbol = asSymbol(&car);
            
        *ret = FCompile_LookUp(gCP, gTP, compilerState, aSymbol);
        ExitOnError(*ret);
        if(asTag(ret) == TYVOID)
            {
   			*ret = TERROR("!Lisp: Reference to undefined symbol or label!");
			goto BadCleanUp;
            }
        _FCompile_MakeINT(*modTvalP, asModifier(ret));
        _FCompile_MakeSYM(*valTvalP, *ret);
    break;
    
    case TYNUM:
        _FCompile_MakeINT(*modTvalP, AMWORD);
        _FCompile_MakeCON(*valTvalP, car);
		break;
    
    default:
        if(tag == TYLABELSYMBOL)
            {
   			*ret = TERROR("!Lisp: Illegal placement of label!");
			goto BadCleanUp;
            }
        else
            {
            /*  Setup the default modifier */
            
            if(tag == TYQUOTEDPAIR)
                {
                /*  We decrement the quote count for any quoted thing */
                
                if(--asQuoteCnt(&car) == 0)
                    asTag(&car) = TYPAIR;
                }
            else
            if(tag == TYQUOTEDSYMBOL)
                {
                /*  We decrement the quote count for any quoted thing */
                
                if(--asQuoteCnt(&car) == 0)
                    {
                    asTag(&car) = TYSYMBOL;
                    if(asSymbol(&car) == gCP->TLambda_nil)
                        {
                        /*  Quoted nil is handled as a special case a la LISP. */
                        
                        goto SymbolProcessing;
                        }
                    }
                }
    
            _FCompile_MakeINT(*modTvalP, AMWORD);            
            _FCompile_MakeCON(*valTvalP, car);
            }           
    break;
    
    }

/*  Set the return value for the caller. */

*ret = _LastResult(compilerState);
FrameExit(*ret);

BadCleanUp:
if(asTag(ret) != TYERROR)
    {
   	*ret = TERROR("!Lisp:Cannot Parse Argument!");
    }
FrameExit(*ret);

}


/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_CreateConstant

Create a constant reference with a modifier tval and a value tval formatted for use in generating instructions to 
appendpcode.

#endif

TVAL    FCompile_CreateConstant(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TVAL constant)
{
TYPE                tag;
NUM					envIndex;
NUM					VarLen;
CHAR				buf[200];
StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TStructure,Vars);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/*  Get the constant's type. */

tag = constant.Tag;

/*  Relocate constants only to the CV structure */
goto CreateCvConstant;

switch (tag)
	{
	case TYNUM: 
		/*  Force the relocation of all Integer constants to the _Rv Structure. */
		Vars = _Rv(compilerState);
		VarLen = Vars->itsMaxItemIndex;
		if (VarLen >= (MAXREGISTERCNT - 2)) goto CreateCvConstant;
		for (envIndex = 0; envIndex < VarLen; ++envIndex)
			{
			if (strncmp((char*)*((TSymbol*)atHMBind(Vars->itsDictionaryArray,envIndex).Key)->itsCString,"__CI",4) == 0)
				{
				if ((atHMBind(Vars->itsDictionaryArray,envIndex).Value.Tag == TYNUM) && (atHMBind(Vars->itsDictionaryArray,envIndex).Value.u.Int == constant.u.Int))
					goto BindIntegerConstant;
				}
			}

		/*  Do we need to add a new constant to the Rv Structure ? */
		BindIntegerConstant:
		if (envIndex == VarLen)
			{
			TStructure_SetMaxIndex(gCP, gTP, TOBJ(_Rv(compilerState)), VarLen + 1);
			sprintf((char*)buf, "__CI%ld", (LONG)(gTP->FCompile_tmpCount = ++gTP->FCompile_tmpCount & 0xFFFF));
			atHMBind(Vars->itsDictionaryArray,VarLen).Key = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)buf);
			atHMBind(Vars->itsDictionaryArray,VarLen).Value = constant;
			atHMBind(Vars->itsDictionaryArray,VarLen).Value.DeclaredType = constant.Tag;
			}

		/*  Set the AMREGISTER return value for the caller (bound to the constant in the Rv structure). */
		ret->Tag = TYNUM;
		ret->DeclaredType = constant.Tag;
		ret->Modifier = AMREGISTER;
		ret->Offset = envIndex;
		ret->u.Int = (envIndex * TVALARRAYITEMSIZE);
		break;
		
	case TYREAL: 
		/*  Force the relocation of all Number constants to the _Rv Structure. */
		Vars = _Rv(compilerState);
		VarLen = Vars->itsMaxItemIndex;
		if (VarLen >= (MAXREGISTERCNT - 2)) goto CreateCvConstant;
		for (envIndex = 0; envIndex < VarLen; ++envIndex)
			{
			if (strncmp((char*)*((TSymbol*)atHMBind(Vars->itsDictionaryArray,envIndex).Key)->itsCString,"__CN",4) == 0)
				{
				if ((atHMBind(Vars->itsDictionaryArray,envIndex).Value.Tag == TYREAL) && (atHMBind(Vars->itsDictionaryArray,envIndex).Value.u.Real == constant.u.Real))
					goto BindNumberConstant;
				}
			}

		/*  Do we need to add a new constant to the Rv Structure ? */
		BindNumberConstant:
		if (envIndex == VarLen)
			{
			TStructure_SetMaxIndex(gCP, gTP, TOBJ(_Rv(compilerState)), VarLen + 1);
			sprintf((char*)buf, "__CN%ld", (LONG)(gTP->FCompile_tmpCount = ++gTP->FCompile_tmpCount & 0xFFFF));
			atHMBind(Vars->itsDictionaryArray,VarLen).Key = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)buf);
			atHMBind(Vars->itsDictionaryArray,VarLen).Value = constant;
			atHMBind(Vars->itsDictionaryArray,VarLen).Value.DeclaredType = constant.Tag;
			}

		/*  Set the AMREGISTER return value for the caller (bound to the constant in the Rv structure). */
		ret->Tag = TYNUM;
		ret->DeclaredType = constant.Tag;
		ret->Modifier = AMREGISTER;
		ret->Offset = envIndex;
		ret->u.Int = (envIndex * TVALARRAYITEMSIZE);
		break;
		
	default:
		/*  Force the relocation of all non-numeric constants to the _Cv Structure. */
		CreateCvConstant:
		Vars = _Cv(compilerState);
		VarLen = Vars->itsMaxItemIndex;
		for (envIndex = 0; envIndex < VarLen; ++envIndex)
			{
			if (strncmp((char*)*((TSymbol*)atHMBind(Vars->itsDictionaryArray,envIndex).Key)->itsCString,"__C",3) == 0)
				{
				*tmp = FUtil1_CmpConstants(gCP, gTP, atHMBind(Vars->itsDictionaryArray,envIndex).Value, constant);
				if (isCompareEQ(tmp))
					goto BindConstant;
				}
			}

		/*  Do we need to add a new constant to the Cv Structure ? */
		BindConstant:
		if (envIndex == VarLen)
			{
			TStructure_SetMaxIndex(gCP, gTP, TOBJ(_Cv(compilerState)), VarLen + 1);
			sprintf((char*)buf, "__C%ld", (LONG)(gTP->FCompile_tmpCount = ++gTP->FCompile_tmpCount & 0xFFFF));
			atHMBind(Vars->itsDictionaryArray,VarLen).Key = (TObject*)TSymbol_MakeUnique(gCP,gTP,(LpCHAR)buf);
			atHMBind(Vars->itsDictionaryArray,VarLen).Value = constant;
			atHMBind(Vars->itsDictionaryArray,VarLen).Value.DeclaredType = constant.Tag;
			}

		/*  Set the AMCVOFFSET return value for the caller (bound to the constant in the Cv structure). */
		ret->Tag = TYNUM;
		ret->Modifier = AMCVOFFSET;
		ret->Offset = envIndex;
		ret->u.Int = (envIndex * BINDARRAYITEMSIZE);
		break;
	}

if ((_Rv(compilerState) != NIL) && (_Rv(compilerState)->itsMaxItemIndex > MAXREGISTERCNT))
	{
	FrameExit(TERROR("!compile: an Lambda cannot have more than 50 register variables!"));
	}

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_RegisterToTemp

Check the original argument array for any AMREGISTER variables. These must be moved to a 
temporary variable via issuing the appropriate vmregSaveInteger or vmregSaveNumber instruction.

The temp argument array is returned with the new arguments after any AMREGISTER arguments
have been saved into temporary memory variables.

#endif

TVAL    FCompile_RegisterToTemp(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TVAL original[], TVAL temp[])
{
NUM                 mod;
NUM					envIndex;
TVAL				wrdOpcode;
StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TStructure,rVars);
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareTVALArray(prmv,8);
EndFrame

/* ************************************************************************** */
/* We must examine each of the three original arguments looking for any with  */
/*  a modifier of AMREGISTER. Any such original arguments must be saved into  */
/*  a temporary memory variable. The temporary variables are named for the    */
/*  arguments location (i.e. __TR1, __TR2, __TR3).                            */
/* ************************************************************************** */

rVars = _Rv(compilerState);
for (mod = 0; mod < 3; ++mod)
	{
	if (original[mod].Modifier == AMREGISTER)
		{
		envIndex = original[mod].Offset;
		if (atHMBind(rVars->itsDictionaryArray,envIndex).Value.DeclaredType == TYREAL)
			{
			wrdOpcode.u.Int = vmregSaveNumber;
			wrdOpcode.Tag = TYNUM;
			}
		else
			{
			wrdOpcode.u.Int = vmregSaveInteger;
			wrdOpcode.Tag = TYNUM;
			}

			/* We now allocate a temporary memory variable. */
			*tmp = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
			ExitOnError(*tmp);
			_FCompile_MakeSYM(temp[mod],*tmp);
			temp[mod].u.Int = (temp[mod].Offset << BITSIZEOFTVAL);

			/*  Issue the pcode instruction to transfer from the register to the temporary variable. */
			prmv[0] = original[mod];
			prmv[1] = temp[mod];
			prmv[2].Modifier = AMVOID;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,prmv[0],prmv[1],prmv[2]);			
			ExitOnError(*ret);
		}
	else
		{
		temp[mod] = original[mod];
		}
	}

if ((_Rv(compilerState) != NIL) && (_Rv(compilerState)->itsMaxItemIndex > MAXREGISTERCNT))
	{
	FrameExit(TERROR("!compile: an Lambda cannot have more than 50 register variables!"));
	}

FrameExit(gCP->Tval_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_TempToRegister

Check the original argument for the AMREGISTER modifier when the temporary argument is
NOT. These must be moved from the temporary back into the register argument via 
issuing the appropriate vmregLoadInteger or vmregLoadNumber instruction.

#endif

TVAL    FCompile_TempToRegister(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TVAL original, TVAL temp)
{
NUM					envIndex;
TVAL				wrdOpcode;
StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TStructure,rVars);
DeclareTVAL(ret);
DeclareTVALArray(prmv,8);
EndFrame

rVars = _Rv(compilerState);
if ((original.Modifier == AMREGISTER) && (temp.Modifier != AMREGISTER))
	{
	envIndex = original.Offset;
	if (atHMBind(rVars->itsDictionaryArray,envIndex).Value.DeclaredType == TYREAL)
		{
		wrdOpcode.u.Int = vmregLoadNumber;
		wrdOpcode.Tag = TYNUM;
		}
	else
		{
		wrdOpcode.u.Int = vmregLoadInteger;
		wrdOpcode.Tag = TYNUM;
		}

		/*  Issue the pcode instruction to transfer from the temporary to the register variable. */
		prmv[0] = temp;
		prmv[1] = original;
		prmv[2].Modifier = AMVOID;
		*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,prmv[0],prmv[1],prmv[2]);			
		ExitOnError(*ret);
	}

if ((_Rv(compilerState) != NIL) && (_Rv(compilerState)->itsMaxItemIndex > MAXREGISTERCNT))
	{
	FrameExit(TERROR("!compile: an Lambda cannot have more than 50 register variables!"));
	}

FrameExit(gCP->Tval_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_Singleton

Called from FCompile_Recognize to process a single item.

Consider the Lisp expression:

    1
    
the code generated for this expression is as follows:

     0000: move    aminteg amfboff amvoid  1                __T0            
     0003: return  amfboff amvoid  amvoid   __T0            

And the move instruction is generated when singleton is called from the recognizer.
See the comments for FCompile_Push and the Lisp Ref. Guide for a description of the 
format of parameters to appendpcode.

#endif

TVAL    FCompile_Singleton(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
TYPE                tag;
_FCompile_StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
DeclareTVAL(car);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame

/*  Check to insure that this is in fact a singleton */

if(asTag(&curPair->itsCdr) != TYVOID )
    goto BadCleanUp;

/*  Load up a tval with the singleton, and get its type tag. */

*car = curPair->itsCar;
tag = asTag(car);

if(tag == TYLABELSYMBOL)
    {
    /*  Do special processing if a TYLABELSYMBOL was passed */
    
    *ret = FCompile_Recognize(gCP, gTP, compilerState, curPair);
    _FCompile_FrameChk(*ret);
    }
else
    {
    /*  Call FCompile_SetupArg to load the modifier and value TVALs for this item. It will also generate */
    /*  code for nested sub-expressions. */
    
    *ret = FCompile_SetupArg(gCP, gTP, compilerState, *car, &prmv[2], &prmv[5] );
    _FCompile_FrameChk(*ret);
    }

/* Move result into a temporary (if requested).          */
/* Note: Labels (directly) and pairs(indirectly)         */ 
/*       are handled with a call back to the recognizer, */
/*       no code will be generated by FCompile_Singleton */
/*       for these two special cases.                    */

if (tag != TYLABELSYMBOL && tag != TYPAIR)
    {
    
    if(_Result(compilerState).Tag == TYVOID)
        {
        /*  If no result location is specified then move single items into the temp location  */
        /*  which was available at entry to this procedure but leave it unincremented. */
    
        _FCompile_ResetBase;
        _Result(compilerState) = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
        _FCompile_ResetBase;
        }
    _LastResult(compilerState) = _Result(compilerState);
     
	_FCompile_MakeINT(prmv[3], asModifier(&_Result(compilerState)));
	_FCompile_MakeSYM(prmv[6], _Result(compilerState));
	_Result(compilerState) = gCP->Tval_VOID;

	/*  Complete formatting parameters for the call to FOptimize_OptimizePcode */

	_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
	_FCompile_MakeINT(prmv[1], VMMOVE);
	_FCompile_MakeINT(prmv[4], AMVOID);

	*ret = FOptimize_OptimizePcode(gCP,gTP,compilerState,7, &prmv[0]);
    _FCompile_FrameChk(*ret);
    }

/*  Set the return value for the caller. */

*ret = _LastResult(compilerState);
_FCompile_FrameExit(*ret);

BadCleanUp:
if(asTag(ret) != TYERROR)
    {
	*ret = FSmartbase_Error(gCP, gTP,"!Lisp: Missing 'begin'!");
    }
_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_LookUp

Resolve a symbol reference by looking through Av, Rv, Tv, Pv, and Gv in that order.

If we find an exact match then we construct an overloaded TVAL for the return value of 
this procedure as follows:

    tval.u.Symbol       :   variable name as a symbol object
    tval.Offset         :   offset (index) into Structure of interest
    tval.Modifier       :   DRM VM instruction modifier corresponding to the Structure where the symbol is bound


Note that every symbol will be bound somewhere. During the lex phase of the compiler all symbols
are bound in the Gv (Global) Structure. During the first phase of the compiler the bindings may 
be updated to the Av (argument variables) Structure, Rv (register variables) Structure, 
the Tv (vars: temp) Structure or the Pv (pvars: permanent) Structure.

Consider the following Lisp code:

    (defun foo (x) vars: ((v1 1)( v2 2)) pvars: (( pv1 1)( pv2 2)) (+ 1 x v1 v2 pv1 pv2))
    
For the procedure object generated the bindings would be as shown:

    Av: #{`x #void}
    Tv: #{`__T0 #void `__T1 #void `__T2 #void `__SF3 #void `v1 1 `v2 2}
    Pv: #{`pv1 1 `pv2 2}
    
You can see in this example that the Tv: contains three temporaries for use in pushing arguments
to function calls (__T0 __T1 __T2) as well as a temporary allocated for the return from 
special form processing (__SF3). All other bindings arise directly from the evaluation of the
code in the Lisp code shown above.

Note:	If the name begins with the caret ^ operator (ie ^xyz), then the name is treated
		as a global name.

#endif

TVAL    FCompile_LookUp(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TSymbol* theSymbol)
{
NUM                 len;
NUM                 ndx;
NUM					n;
REAL				x;
NUM					count;
LpCHAR				sp;
LpBIND				bp;
CHAR				buf[1024];
NUM					prmc;
TVAL				prmv[4];
StartFrame
DeclareOBJ(TSymbol,gblSymbol);
DeclareOBJ(TStructure,tmpEnv);
DeclareTVAL(offset);
DeclareTVAL(regName);
DeclareTVAL(templateName);
DeclareTVAL(template);
DeclareTVAL(index1);
DeclareTVAL(index2);
DeclareTVAL(index3);
DeclareTVAL(ret);
EndFrame

if ((_Rv(compilerState) != NIL) && (_Rv(compilerState)->itsMaxItemIndex > MAXREGISTERCNT))
	{
	FrameExit(TERROR("!compile: an Lambda cannot have more than 50 register variables!"));
	}

/* ************************************************************** */
/* Start recognition of explicit global variable name references */
/* ************************************************************** */

if (strncmp(objSymbolArray(theSymbol),"Gv:",3) == 0)
	{
	/* Return the variable name as a global reference. */
	sp = &objSymbolArray(theSymbol)[3];
	*ret = TSYMBOL(sp);
	ret->Modifier = AMGVOFFSET; 
	ret->Offset = 0; 
	ret->DeclaredType = TYVOID;

	FrameExit(*ret);
	}
	
/* ************************************************************** */
/* Start recognition of register offset addressing variable names */
/* ************************************************************** */

if (strncmp(objSymbolArray(theSymbol),"regoffset:",10) == 0)
	{
	/* Isolate and locate the register name */
	buf[0] = 0;
	sp = &objSymbolArray(theSymbol)[10];
	for (n= 0; sp[n] != '['; ++n) buf[n] = sp[n]; 
	buf[n] = 0; 
	*regName = TSYMBOL(buf);
	*ret = *regName;
	if (_Rv(compilerState) != NIL)
		{
		tmpEnv = _Rv(compilerState);
		if (tmpEnv->itsMaxItemIndex)
			{
			len = tmpEnv->itsMaxItemIndex;
			for(ndx = 0; ndx < len; ndx++)
				{
				if (atHMBind(tmpEnv->itsDictionaryArray,ndx).Key == regName->u.Object)
					{
					ret->Offset = 0;
					ret->Modifier = ndx;
				    ret->DeclaredType = TYVOID;
					goto RecognizeOffset;
					}
				}
			}
		goto BadRegOffset;
		}
	else
		{
		BadRegOffset:
		FrameExit(TERROR("!compile: invalid register offset addressing variable name!"));
		}

	/* Attempt to recognize a constant offset */
	RecognizeOffset:
	if (sp[n+1] == ':')
		{
		buf[0] = 0;
		sp = &sp[n+2];
		for (n= 0; sp[n] != ':'; ++n) buf[n] = sp[n]; 
		buf[n] = 0; 
		*offset = FProcedure_recInt(gCP,gTP,&x,&count,buf);
		if ((offset->Tag != TYBOLE) || (offset->u.Bool != TRUE)) goto BadRegOffset; 
		offset->Tag = TYNUM;
		offset->u.Int = x;
		goto FixupRegOffset;	
		}

	/* Isolate and locate the template name */
	buf[0] = 0;
	sp = &sp[n+1];
	for (n= 0; sp[n] != '('; ++n) buf[n] = sp[n]; 
	buf[n] = 0; 
	*templateName = TSYMBOL(buf);
	/* Is the template name in the Av structure? */
	if ((_Av(compilerState) != NIL) && (_Av(compilerState)->itsMaxItemIndex > 0))
		{
		bp = BindArray(TOBJ(_Av(compilerState)));
		len = _Av(compilerState)->itsMaxItemIndex;
		for (ndx = 0; ndx < len; ndx++) {if (bp[ndx].Key == templateName->u.Object) {*template = bp[ndx].Value; goto FoundTemplate;}}
		}
	/* Is the template name in the Tv structure? */
	if ((_Tv(compilerState) != NIL) && (_Tv(compilerState)->itsMaxItemIndex > 0))
		{
		bp = BindArray(TOBJ(_Tv(compilerState)));
		len = _Tv(compilerState)->itsMaxItemIndex;
		for (ndx = 0; ndx < len; ndx++) {if (bp[ndx].Key == templateName->u.Object) {*template = bp[ndx].Value; goto FoundTemplate;}}
		}
	/* Is the template name in the Sv structure? */
	if ((_Sv(compilerState) != NIL) && (_Sv(compilerState)->itsMaxItemIndex > 0))
		{
		bp = BindArray(TOBJ(_Sv(compilerState)));
		len = _Sv(compilerState)->itsMaxItemIndex;
		for (ndx = 0; ndx < len; ndx++) {if (bp[ndx].Key == templateName->u.Object) {*template = bp[ndx].Value; goto FoundTemplate;}}
		}
	/* Is the template name in the Pv structure? */
	if ((_Pv(compilerState) != NIL) && (_Pv(compilerState)->itsMaxItemIndex > 0))
		{
		bp = BindArray(TOBJ(_Pv(compilerState)));
		len = _Pv(compilerState)->itsMaxItemIndex;
		for (ndx = 0; ndx < len; ndx++) {if (bp[ndx].Key == templateName->u.Object) {*template = bp[ndx].Value; goto FoundTemplate;}}
		}
	/* Is the template name in the Cv structure? */
	if ((_Cv(compilerState) != NIL) && (_Cv(compilerState)->itsMaxItemIndex > 0))
		{
		bp = BindArray(TOBJ(_Cv(compilerState)));
		len = _Cv(compilerState)->itsMaxItemIndex;
		for (ndx = 0; ndx < len; ndx++) {if (bp[ndx].Key == templateName->u.Object) {*template = bp[ndx].Value; goto FoundTemplate;}}
		}		
	/* Is the template name a global class reference? */
	if (templateName->u.Symbol->itsUserTypeFields != NIL)
		{
		template->u.Structure = templateName->u.Symbol->itsUserTypeFields; 
		template->Tag = template->u.Structure->itsObjectType; 
		goto FoundTemplate;
		}
	/* Is the template name a globally defined variable? */
	if (templateName->u.Symbol->itsGlobalValue.Tag != TYVOID)
		{
		*template = templateName->u.Symbol->itsGlobalValue; 
		goto FoundTemplate;
		}
	goto BadRegOffset;

	/* Isolate the first index argument. */
	FoundTemplate:
	buf[0] = 0;
	sp = &sp[n+1];
	for (n= 0; ((sp[n] != ')') && (sp[n] != ',')); ++n) buf[n] = sp[n]; 
	buf[n] = 0;
	if (ISDIGIT((NUM)buf[0]))
		{ 
		*index1 = FProcedure_recInt(gCP,gTP,&x,&count,buf);
		if ((index1->Tag != TYBOLE) || (index1->u.Bool != TRUE)) goto BadRegOffset; 
		index1->Tag = TYNUM;
		index1->u.Int = x;
		}
	else
		{
		*index1 = TSYMBOL(buf);
		}
	prmc = 2;
	
	/* Isolate the second index argument. */
	if (sp[n] == ',')
		{
		buf[0] = 0;
		sp = &sp[n+1];
		for (n= 0; ((sp[n] != ')') && (sp[n] != ',')); ++n) buf[n] = sp[n]; 
		buf[n] = 0;
		if (ISDIGIT((NUM)buf[0]) == 0) goto BadRegOffset;
		*index2 = FProcedure_recInt(gCP,gTP,&x,&count,buf);
		if ((index2->Tag != TYBOLE) || (index2->u.Bool != TRUE)) goto BadRegOffset; 
		index2->Tag = TYNUM;
		index2->u.Int = x;
		prmc = 3;
		}
	
	/* Isolate the third index argument. */
	if (sp[n] == ',')
		{
		buf[0] = 0;
		sp = &sp[n+1];
		for (n= 0; sp[n] != ')'; ++n) buf[n] = sp[n]; 
		buf[n] = 0;
		if (ISDIGIT((NUM)buf[0]) == 0) goto BadRegOffset;
		*index3 = FProcedure_recInt(gCP,gTP,&x,&count,buf);
		if ((index3->Tag != TYBOLE) || (index3->u.Bool != TRUE)) goto BadRegOffset; 
		index3->Tag = TYNUM;
		index3->u.Int = x;
		prmc = 4;
		}
	
	/* Compute the offset from the template based upon the indices. */
	prmv[0] = *template; 
	prmv[1] = *index1; 
	prmv[2] = *index2; 
	prmv[3] = *index3;
	*offset = FUtil2_Offset(gCP,gTP,prmc,prmv);
	if (offset->Tag != TYNUM) goto BadRegOffset; 

	/* Return the final register offset addressing variable. */
	FixupRegOffset:
	ret->Offset = offset->u.Int;
	ret->DeclaredType = offset->DeclaredType;

	/* Make sure we fix up the offset for our favored register offsets */
	switch (ret->Modifier)
		{
		case AMSVOFFSET:
		case AMPVOFFSET:
		case AMCVOFFSET:
			ret->Offset /= BINDARRAYITEMSIZE;
			break;

		case AMAVOFFSET:
		case AMREGISTER:
		case AMTVOFFSET:
			ret->Offset /= TVALARRAYITEMSIZE;
			break;
		}

	FrameExit(*ret);
	}

/* ************************************************************** */
/* end recognition of register offset addressing variable names   */
/* ************************************************************** */

/*  First we check the Av */

if (_Av(compilerState) != NIL)
	{
	tmpEnv = _Av(compilerState);
	if(tmpEnv->itsMaxItemIndex)
		{
		len = tmpEnv->itsMaxItemIndex;
		for(ndx = 0; ndx < len; ndx++)
			{
			if (atHMBind(tmpEnv->itsDictionaryArray,ndx).Key == (TObject*)theSymbol)
				{
				ret->Offset = ndx;
				ret->Modifier = AMAVOFFSET;
				ret->DeclaredType = atHMBind(tmpEnv->itsDictionaryArray,ndx).Value.DeclaredType;
				ret->u.Symbol = theSymbol;
				ret->Tag = TYSYMBOL;
				FrameExit(*ret);
				}
			}
		}
	}
    
/*  Next we check the Rv  */

if (_Rv(compilerState) != NIL)
	{
	tmpEnv = _Rv(compilerState);
	if(tmpEnv->itsMaxItemIndex)
		{
		len = tmpEnv->itsMaxItemIndex;
		for(ndx = 0; ndx < len; ndx++)
			{
			if (atHMBind(tmpEnv->itsDictionaryArray,ndx).Key == (TObject*)theSymbol)
				{
				ret->Offset = ndx;
				ret->Modifier = AMREGISTER;
				ret->DeclaredType = atHMBind(tmpEnv->itsDictionaryArray,ndx).Value.DeclaredType;
				ret->u.Symbol = theSymbol;
				ret->Tag = TYSYMBOL;
				FrameExit(*ret);
				}
			}
		}
	}
    
/*  Next we check the Tv */

if (_Tv(compilerState) != NIL)
	{
	tmpEnv = _Tv(compilerState);
	if(tmpEnv->itsMaxItemIndex)
		{
		len = tmpEnv->itsMaxItemIndex;
		for(ndx = 0; ndx < len; ndx++)
			{
			if (atHMBind(tmpEnv->itsDictionaryArray,ndx).Key == (TObject*)theSymbol)
				{
				ret->Offset = ndx;
				ret->Modifier = AMTVOFFSET;
				ret->DeclaredType = atHMBind(tmpEnv->itsDictionaryArray,ndx).Value.DeclaredType;
				ret->u.Symbol = theSymbol;
				ret->Tag = TYSYMBOL;
				FrameExit(*ret);
				}
			}
		}
	}
    
/*  Next we check the Sv */

if (_Sv(compilerState) != NIL)
	{
	tmpEnv = _Sv(compilerState);
	if(tmpEnv->itsMaxItemIndex)
		{
		len = tmpEnv->itsMaxItemIndex;
		for(ndx = 0; ndx < len; ndx++)
			{
			if (atHMBind(tmpEnv->itsDictionaryArray,ndx).Key == (TObject*)theSymbol)
				{
				ret->Offset = ndx;
				ret->Modifier = AMSVOFFSET;
				ret->DeclaredType = atHMBind(tmpEnv->itsDictionaryArray,ndx).Value.DeclaredType;
				ret->u.Symbol = theSymbol;
				ret->Tag = TYSYMBOL;
				FrameExit(*ret);
				}
			}
		}
	}
    
/*  Next we check the Pv */

if (_Pv(compilerState) != NIL)
	{
	tmpEnv = _Pv(compilerState);
	if(tmpEnv->itsMaxItemIndex)
		{
		len = tmpEnv->itsMaxItemIndex;
		for(ndx = 0; ndx < len; ndx++)
			{
			if (atHMBind(tmpEnv->itsDictionaryArray,ndx).Key == (TObject*)theSymbol)
				{
				ret->Offset = ndx;
				ret->Modifier = AMPVOFFSET;
				ret->DeclaredType = atHMBind(tmpEnv->itsDictionaryArray,ndx).Value.DeclaredType;
				ret->u.Symbol = theSymbol;
				ret->Tag = TYSYMBOL;
				FrameExit(*ret);
				}
			}
		}
	}
    
/*  Next we check the Cv  */

if (_Cv(compilerState) != NIL)
	{
	tmpEnv = _Cv(compilerState);
	if(tmpEnv->itsMaxItemIndex)
		{
		len = tmpEnv->itsMaxItemIndex;
		for(ndx = 0; ndx < len; ndx++)
			{
			if (atHMBind(tmpEnv->itsDictionaryArray,ndx).Key == (TObject*)theSymbol)
				{
				ret->Offset = ndx;
				ret->Modifier = AMCVOFFSET;
				ret->DeclaredType = atHMBind(tmpEnv->itsDictionaryArray,ndx).Value.DeclaredType;
				ret->u.Symbol = theSymbol;
				ret->Tag = TYSYMBOL;
				FrameExit(*ret);
				}
			}
		}
	}
 
/*  By default if we do not find a var in Av Rv Tv Pv Cv then it must be bound globally. */
    
ret->Offset = 0;
ret->Modifier = AMGVOFFSET;
ret->DeclaredType = TYVOID;
ret->u.Symbol = theSymbol;
ret->Tag = TYSYMBOL;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_Push

Push arguments for a function call, recursing as required to process included function calls
and special forms.

Consider the Lisp expression:

    (sum (sum -1 -1) (sum -2 -2) (sum -3 -3) (sum -4 -4) )
    
This will generate the following instructions:

     0000: push    aminteg aminteg amvoid   -1               -1              
     0003: call    aminteg amgvoff amfboff  2                sum              __T0            
     0007: push    aminteg aminteg amvoid   -2               -2              
     0010: call    aminteg amgvoff amfboff  2                sum              __T1            
     0014: push    aminteg aminteg amvoid   -3               -3              
     0017: call    aminteg amgvoff amfboff  2                sum              __T2            
     0021: push    amfboff amfboff amfboff  __T0             __T1             __T2            
     0025: push    aminteg aminteg amvoid   -4               -4              
     0028: call    aminteg amgvoff amfboff  2                sum              __T0            
     0032: push    amfboff amvoid  amvoid   __T0            
     0034: call    aminteg amgvoff amfboff  4                sum              __T0            
     0038: return  amfboff amvoid  amvoid   __T0            

From this code we note two points of interest. First, you may see how the target values
for the function calls which are passed as parameters to the leftmost sum call place their
results in temporary variables according to the compiler temporary allocation scheme. The 
first nested call (sum 1 1) places its result in __T0, the second nested call (sum 2 2) places
its result in __T1 and the third nested call (sum 3 3) places its result in __T2. 
The second important point to note is that after the code for (sum 3 3) has been generated
then a VMPUSH is generated for these first three intermediate results (__TO __T1 __T2). This 
MUST be done because of the limitation on the number of parameters to VMPUSH. Once it has
occured then the temporaries used to store intermediate results from previous code generation
will be free for reuse, and this can be seen in the reuse of __T0 for the code generated
for (sum -4 -4).

#endif

TVAL    FCompile_Push(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair, LpNUM instruction)
{
NUM                 cn;
NUM                 ndx;
NUM                 total;
_FCompile_StartFrame
DeclareTVAL(ret);
DeclareTVAL(car);
DeclareTVALArray(prmv,8);
_FCompile_EndFrame

/*  See the Lisp Ref. Guide for a description of the setup of parameters to append pcodes to  */
/*  the instruction stream for a procedure object. */

/*  The index of the first argument tval used for arguments is 2. */

ndx = 2;

/*  We use total to keep track of how many arguments were pushed in total, this is the value */
/*  passed back to the caller (FCompile_Call()) who needs the information to format its own */
/*  instruction. */

total = 0;

/*  We setup the first to parameters for our call(s) to append VMPUSH instructions to the */
/*  current procedure object. */

_FCompile_MakeTVL(prmv[0], TOBJ(_Pc(compilerState)));
_FCompile_MakeINT(prmv[1], VMPUSH);

do 
    {
    *car = curPair->itsCar;

    /*  Call FCompile_SetupArg to load the modifier and value TVALs for this item. It will also  */
    /*  generate code for nested sub-expressions. */
    
    if(asTag(car) == TYPAIR)
        {
        /*  We call FSpecialForms1_AllocTemp to insure that a unique temp is allocated */
        /*  for the result of processing this sub-expression list. */
        
        FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);
        }

    *ret = FCompile_SetupArg(gCP, gTP, compilerState, *car, &prmv[ndx], &prmv[ndx+3] );
    _FCompile_FrameChk(*ret);
            
    ndx++;
    
    if (ndx == 5)
        {
        /*  We have reached the max number of args per push. Gen code and reset counters. */
        
        /*  Update the total number of args pushed */
        
        total += 3;
        
        /*  Append this instruction to the current procedure object. */
        
        *ret = FOptimize_OptimizePcode(gCP, gTP, compilerState, 8, &prmv[0]);
        _FCompile_FrameChk(*ret);
        
        /*  Reset the parameter index for further arguments. */
        
        ndx = 2;
        
        /*  Reset the index for the next available temporary. We may do this as a result of the */
        /*  fact that once a temp has been pushed it becomes available for reuse. */
        
        _FCompile_ResetBase;
        }
    } while ((curPair = _FCompile_CdrPair(curPair)));

/* We have no further arguments (in this batch) to push */
if (ndx != 2)
    {
    /*  Void all unused modifiers. */
    
    for(cn = ndx; cn < 5; cn++)
        {
        _FCompile_MakeINT(prmv[cn], AMVOID);
        }
        
    total += ndx - 2;

	if ((total == 1) && (*instruction == VMCALL))
		{
		/* We will not push any arguments; but instead, we will return the single   */
		/* argument to be used in the short form version of the vmcall instruction. */

		*ret = prmv[5];
		_LastResult(compilerState) = *ret;
		*instruction = VMCALLARG;

		_FCompile_FrameExit(*ret);
		}
	else
		{
		*ret = FOptimize_OptimizePcode(gCP,gTP, compilerState,5 + ndx - 2, &prmv[0]);
		_FCompile_FrameChk(*ret);
		}
    }

/* Set the lastresult for the caller. */

_LastResult(compilerState) = prmv[5];
asModifier(&_LastResult(compilerState)) = asInt(&prmv[2]);

/* We will return the number of arguments actually pushed. */

ret->Tag = TYNUM;
ret->u.Int = total;
ret->Modifier = AMINTEGER;
ret->Offset = 0;

_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_Call

When a callable symbol is encountered as the first symbol in a list then this function is 
called to manage the argument setup and generate the code for a VMCALL instruction. Function 
calls take advantage of the temporary variable allocation scheme in which temps are automatically
assigned as needed and then reused. 

Regardless of the number or type of parameters to s function call we will never need more than 
three temporaries. This is because of the fact that VMPUSH provides an upper bound on the number 
of parameters which need to be saved temps before a push instruction is generated. Once a push
instruction is executed then the data is moved from the Tv to the function call stack by the
virtual machine and the temporaries will thus be freed for reuse.

#endif

TVAL    FCompile_Call(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
NUM					vmsend = VMSEND;
NUM					vmcall = VMCALL;
_FCompile_StartFrame
DeclareOBJ(TPair,tmpPair);
DeclareOBJ(TPair,newPair);
DeclareOBJ(TStructure,cVars);
DeclareTVAL(tmp);
DeclareTVAL(final);
DeclareTVAL(sym);
DeclareTVAL(ret);
DeclareTVALArray(prmv,9);
_FCompile_EndFrame

/* Manage the ((ref ...)) special form */
if (curPair->itsCar.Tag == TYPAIR)
    {
    tmpPair = curPair->itsCar.u.Pair;
    if((tmpPair->itsCar.Tag == TYSYMBOL) && (tmpPair->itsCar.u.Symbol == gCP->FMacro_refSym))
        {
        /*  We do special processing here to identify cases like ((ref array index ) ...) */
        /*  which should compile into function calls. We will need to assume that the array */
        /*  contains executable objects and generate code to call them. */


        }
    else
        {
        /*  This is an error ... */
        
        goto BadCleanUp;
        }
    }
else
if ((curPair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCdr.Tag == TYPAIR) && 
	(strcmp(SymbolArray(curPair->itsCar),"Number") == 0))
	{
	*final = _Result(compilerState);

	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYREAL);

    /*  Is the the register name in the Rv structure of the Lambda? */
    *tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCdr.u.Pair);
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

	_Result(compilerState) = *final;
	goto Last;
	}
else
if ((curPair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCdr.Tag == TYPAIR) && 
	(strcmp(SymbolArray(curPair->itsCar),"Integer") == 0))
	{
	*final = _Result(compilerState);

	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYNUM);

    /*  Is the the register name in the Rv structure of the Lambda? */
    *tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCdr.u.Pair);
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

	_Result(compilerState) = *final;
	goto Last;
	}
else
if ((curPair->itsCar.Tag == TYQUOTEDSYMBOL) && 
	(curPair->itsCdr.Tag == TYPAIR) && 
	(strcmp(SymbolArray(curPair->itsCar),"Word") == 0))
	{
	*final = _Result(compilerState);

	/* Allocate a register variable */
	_Result(compilerState) = *ret = FSpecialForms1_AllocTemp(gCP,gTP,compilerState,TYVOID);

    /*  Is the the register name in the Rv structure of the Lambda? */
    *tmp = FCompile_Recognize(gCP, gTP, compilerState, curPair->itsCdr.u.Pair);
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

	_Result(compilerState) = *final;
	goto Last;
	}
else
/* Manage the (msg: ...) implied message send */
if (asTag(&curPair->itsCar) == TYQUOTEDSYMBOL)
    {
    /*  We will setup a message send call. */
    
    *sym = curPair->itsCar;

    /*  Save the final location, if any was requested */
    
    *final = _Result(compilerState);
    _Result(compilerState) = gCP->Tval_VOID;
        
    /*  Move to the first arg or expression */

    curPair = _FCompile_CdrPair(curPair);
    if (curPair)
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
        
        goto BadCleanUp;
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
    
    *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState, 8, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    /*  Set the return value for this function. */
    
    _FCompile_MakeTVL(*ret, _LastResult(compilerState));
    _FCompile_FrameExit(*ret);
    }
else
/* Manage all other (function ...) function calls */
    {
    /*  We will setup an overloaded TVAL in the format of FCompile_LookUp for the function */
    /*  call symbol. */
    *sym = curPair->itsCar;
    *ret = FCompile_LookUp(gCP, gTP, compilerState, asSymbol(sym));
    _FCompile_FrameChk(*ret);


    if (ret->Tag == TYVOID)
        {
		*ret = TERROR("!Lisp: Invalid function call!");
		_FCompile_FrameExit(*ret);
        }
    else
        {
        *sym = *ret;
        }
    }

/*  Move to the first arg or expression */

curPair = _FCompile_CdrPair(curPair);

/*  See if it this is a function call which can be directly replaced by virtual machine */
/*  instructions. */

*ret = FOptimize1_Calls(gCP, gTP, compilerState, curPair, asSymbol(sym));
_FCompile_FrameChk(*ret);

if(asTag(ret) == TYBOLE && asBool(ret) == FALSE)
    {
    /*  This function call was not optimized. We must generate code for a function call it. */
    
    /*  Save the final location, if any was requested */
    
    *final = _Result(compilerState);
    _Result(compilerState) = gCP->Tval_VOID;
        
    if (curPair)
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
    
    *ret = FOptimize_OptimizePcode(gCP,gTP,compilerState, 8, &prmv[0]);
    _FCompile_FrameChk(*ret);
    
    /*  Set the return value for this function. */
    
    _FCompile_MakeTVL(*ret, _LastResult(compilerState));
    }

Last:
_FCompile_FrameExit(*ret);

BadCleanUp:
if (ret->Tag != TYERROR)
    {
	*ret = TERROR("!Lisp: Invalid function call!");
    }
_FCompile_FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_ResolveGotos

Scan through Structure containing gotos to be backpatched and attempt to resolve them
through Structure containing label locations.

During the compilation process we build two Structures for the management of goto information.
The goto Structure contains the location of all unconditional jumps arising from goto statements
bound with their label symbol. The label Structure contains the locations for all identified
label symbols found in the compilation process (no code is actually generated for labels themselves so 
unused labels do not affect code generated.)

Consider the following Lisp program:

(defun  foo ()
    goto EXIT:
    (writeln "never executes" )
    EXIT:
    (writeln "EXIT foo" )
)

BEFORE we execute FCompile_ResolveGotos the code looks like this:

 0000: jump    aminteg amvoid  amvoid  -1               
 0002: push    amcvoff amvoid  amvoid   never executes 
 0004: call    aminteg amgvoff amfboff  1                writeln          __T0            
 0008: push    amcvoff amvoid  amvoid   EXIT foo        
 0013: call    aminteg amgvoff amfboff  1                writeln          __T0            


the "goto EXIT:" statement generates the instruction shown:

    goto EXIT:  ==> 0000: jump    aminteg amvoid  amvoid  -1
    
During the compilation of this statement an entry will be made in the Structure which stores
goto information as follows:    

    goto Structure ==>    #{ EXIT:    0}
    
We store the offset for each jump instruction (in this case  0000: jump ...) paired with its
offset into the procedure object (in this case zero (0) since this is the very first instruction.)
We always initialize the target location for an unresolved jump to an illegal negative value.

The "EXIT:" statement generates no code, but during the compilation of this statement an entry 
will be made in the Structure which stores label information as follows:  

    label Structure ==>   #{ EXIT:    8}
    
We store the offset for the instruction following a label paired with its
offset into the procedure object (in this case eight (8).)

In order to resolve gotos we simply scan through the goto Structure to find jump
instructions which require fixup, and check the label Structure for a matching label.
If we find one then we take the location associated with the matched label in the label
Structure and fixup the jump instruction to use that value. If we do not find one then
we have an error has occured and we have an illegal goto and an invalid program.

AFTER FCompile_ResolveGotos has executed the instructions will look like this:

 0000: jump    aminteg amvoid  amvoid  8               
 0002: push    amobjec amvoid  amvoid   never executes 
 0004: call    aminteg amgvoff amfboff  1                writeln          __T0            
 0008: push    amcvoff amvoid  amvoid   EXIT foo        
 0013: call    aminteg amgvoff amfboff  1                writeln          __T0            

#endif

TVAL    FCompile_ResolveGotos(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState)
{
NUM                 cn;
NUM                 max;
NUM                 patchLocation;
NUM                 patchValue;
NUM                 tmpInt;
COMPARE             compare;
CHAR                buf[100];
StartFrame
DeclareOBJ(TSymbol,theLabel);
DeclareOBJ(TPcodeVector,Pc);
DeclareTVAL(ret);
EndFrame

Pc = _Pc(compilerState);
max = _GotosP(compilerState)->itsMaxItemIndex;
*ret = gCP->Tval_VOID;

for(cn = 0; cn < max; cn++)
    {
    /*  We loop through the list of gotos and try to match them with their symbol labels. */
    /*  We may encounter an error if a goto is specified for a non-existant label. */
    theLabel = (TSymbol*)atHMBind(_GotosP(compilerState)->itsDictionaryArray,cn).Key;
    *ret = TStructure_BSearchKey(gCP,gTP,_LabelsP(compilerState),(TObject*)theLabel,&compare);
    ExitOnError(*ret);
    
    if(compare == 0)
        {
        tmpInt = asInt(ret);
        patchValue = atHMBind(_LabelsP(compilerState)->itsDictionaryArray,tmpInt).Value.u.Int;
        patchLocation = asNumIndex(&atHMBind(_GotosP(compilerState)->itsDictionaryArray,cn).Value);
        atHMInt(Pc->itsInstructionArray,patchLocation) = patchValue;
        }
    else
        {
        /*  We could not resolve this goto */
        
        goto BadCleanUp;
        }
    }

FrameExit(*ret);

BadCleanUp:
if(asTag(ret) != TYERROR)
    {
    strcpy(buf,"!Lisp: Cannot Resolve this goto Label:");
    strncat(buf,(char*)*theLabel->itsCString,8);
    strcat(buf,"!");
    *ret = TERROR(buf);
    }
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_args

Process a list of symbols possibly with initial values and bind them in the Av Structure.

#endif

TVAL    FCompile_args(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
return (FCompile_Bind(gCP, gTP, compilerState, curPair, _Av(compilerState) ));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_faces

Process a list of symbols possibly with initial constant values and bind them in the In Structure.

#endif

TVAL    FCompile_faces(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
StartFrame
DeclareTVAL(ret);
EndFrame

/* Make sure we have an interfaces structure for this Lambda. */

if (_In(compilerState) == NIL)
	{
	_In(compilerState) = TStructure_New(gCP,gTP);
	}

/* Bind the initial values to the interfaces structure for this Lambda. */

*ret = FCompile_Bind(gCP, gTP, compilerState, curPair, _In(compilerState));

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_rvars

Process a list of symbols possibly with initial constant values and bind them in the Rv Structure.

#endif

TVAL    FCompile_rvars(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
StartFrame
NUM				n;
NUM				N;
DeclareTVAL(ret);
EndFrame

/* Make sure we have an interfaces structure for this Lambda. */

if (_Rv(compilerState) == NIL)
	{
	*ret = FCompile_SetupRegs(gCP,gTP,_CurProcP(compilerState));
	ExitOnError(*ret);
	_Rv(compilerState) = ret->u.Structure;
	}

/* Bind the initial values to the interfaces structure for this Lambda. */

*ret = FCompile_Bind(gCP, gTP, compilerState, curPair, _Rv(compilerState));
ExitOnError(*ret);

/* Check for no more than the maximum allowed register variable declarations. */

N = _Rv(compilerState)->itsMaxItemIndex;
if (N > MAXREGISTERCNT) 
	{
	*ret = TERROR("!compile: a maximum of 75 register variables are allowed!");
	FrameExit(*ret);
	}

/* Check all register variables for valid type initializations. */

for (n = 0; n < N; ++n) 
	{
	if (BindArray(TOBJ(_Rv(compilerState)))[n].Value.DeclaredType == TYVOID) BindArray(TOBJ(_Rv(compilerState)))[n].Value.DeclaredType = TYNUM;
	if (BindArray(TOBJ(_Rv(compilerState)))[n].Value.DeclaredType == TYTVAL) BindArray(TOBJ(_Rv(compilerState)))[n].Value.DeclaredType = TYNUM;

	if (BindArray(TOBJ(_Rv(compilerState)))[n].Value.Tag == TYVOID)
		{
		if (BindArray(TOBJ(_Rv(compilerState)))[n].Value.DeclaredType == TYREAL)
			{
			BindArray(TOBJ(_Rv(compilerState)))[n].Value.Tag = TYREAL;
			BindArray(TOBJ(_Rv(compilerState)))[n].Value.u.Real = 0.0;
			}
		else
			{
			BindArray(TOBJ(_Rv(compilerState)))[n].Value.Tag = TYNUM;
			BindArray(TOBJ(_Rv(compilerState)))[n].Value.u.Int = 0;
			}
		}

	}

if ((_Rv(compilerState) != NIL) && (_Rv(compilerState)->itsMaxItemIndex > MAXREGISTERCNT))
	{
	FrameExit(TERROR("!compile: an Lambda cannot have more than 50 register variables!"));
	}

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_vars

Process a list of symbols possibly with initial values and bind them in the Tv Structure.

#endif

TVAL    FCompile_vars(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
StartFrame
DeclareTVAL(ret);
EndFrame

*ret = FCompile_Bind(gCP, gTP, compilerState, curPair, _Tv(compilerState));

/*  Now we may reset the _TempBaseN(state) for this lambda object. We do this here so that */
/*  temps may be managed as a contiguous stack residing above any user defined vars:. */

_TempBaseN(compilerState) = _TempN(compilerState) = _Tv(compilerState)->itsMaxItemIndex;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_svars

Process a list of a single type name symbol and bind its fields list to the Sv Structure.

#endif

TVAL    FCompile_svars(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
return (FCompile_Bind(gCP, gTP, compilerState, curPair, _Sv(compilerState) ));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_pvars

Process a list of symbols possibly with initial values and bind them in the Pv Structure.

#endif

TVAL    FCompile_pvars(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
return (FCompile_Bind(gCP, gTP, compilerState, curPair, _Pv(compilerState) ));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_cvars

Process a list of symbols possibly with initial values and bind them in the Cv Structure.

#endif

TVAL    FCompile_cvars(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, TPair* curPair)
{
return (FCompile_Bind(gCP, gTP, compilerState, curPair, _Cv(compilerState) ));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_NewSpecialForm

The FCompile_NewSpecialForm function converts the specified text into a symbol. 
The symbol`s value in the symbol global value member is given the value of a TYSPECIALFORM. 

#endif

TVAL FCompile_NewSpecialForm(LpXCONTEXT gCP, LpTHREAD gTP, TSymbol **symName, LpCHAR funcName)
{
(*symName) = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)funcName);

/*  Setting the GlobalValue with a non-void value will automatically PERM the symbol */
(*symName)->itsGlobalValue.u.Object = (TObject*)*symName;
(*symName)->itsGlobalValue.Tag = TYSPECIALFORM;

return(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile___Send   

The FCompile___Send function is a registered function that mimics the send special form.
This function was cloned from part of FVmscript_Eval. 
The __send function is only used by CompilerLib to translate the virtual machine code 
generated by the send special form into C. The C instruction that is generated by 
CompilerLib is:

*_result =  FSmartbase_Evalv(gCP, gTP, argc, &prmv[0]);

where  argc                    is the number of arguments 
       prmv[0]          is the target   
	   prmv[argc - 1]   is the message itself

The FCompile___Send function derives the message handler by looking up the message in the
methods dictionary for the target argument.  The proc associated with the message will be
executed with the remaining arguments. 



#endif

TVAL FCompile___Send(LpXCONTEXT gCP, LpTHREAD gTP, NUM argc, TVAL argv[])
{
NUM			 n;
NUM          topStack;               /* pointer to top of the argument stack */


StartFrame
DeclareTVAL(target);                 /*  Target  argument for the send */
DeclareTVAL(msg);                    /*  Message argument for the send */
DeclareTVAL(proc); 				     /*  The proc attached to this message  */
DeclareTVAL(retValue);               /*  return value */
EndFrame

/* Save the last item on the argument stack as the message */

*msg = argv[argc-1];

/* Save the first item on the argument stack as the target  */

*target = argv[0]; 

/* Save the argument count  */

n  = argc;

/* Compute the position of the last argument in the argument stack ie argv[topStack] */

topStack = argc;


    
/*  Load the source message procedure by combining the  */
/*  type of the first argument (target) and the symbol (msg) */
/*  to look up the procedure in the methods table for the type. */
/*  Note:   If a quoted symbol is the message, then the method */
/*          Procedure is chosen from the parents methods Dictionary. */
if ((msg->Tag == TYSYMBOL) && (n >= 1))
    { 
    /*  Load the type of the receiving argument. */
    gTP->FVmscript_type = target->Tag;
	/*  Modify the send if the target object has properties */
	/*  which may may take prescedence over the class methods */
	switch (gTP->FVmscript_type)
		{
		case TYLAMBDA:
			/*  If receiver is an Lambda, and the message matches one of its  child Lambdas, */
			/*	then the matching child Lambda takes prescedence over any class methods. */
			/*	Does the message match a child Lambda of the receiver? */
			gTP->FVmscript_iargument = TLambda_GetPvMember(gCP,gTP,*target,SymbolArray(*msg));
			if (gTP->FVmscript_iargument.Tag != TYVOID)
				{
				/* Call the child of the receiver directly instead of using the class method. */
				/* Note: Do not pass the receiver as an argument. */
				gTP->FVmscript_isource = gTP->FVmscript_iargument;
				*msg = gTP->FVmscript_isource;
				--n;
				goto CallDetail;
				}
			gTP->FVmscript_iargument = TLambda_GetCvMember(gCP,gTP,*target,SymbolArray(*msg));
			if (gTP->FVmscript_iargument.Tag != TYVOID)
				{
				/* Call the child of the receiver directly instead of using the class method. */
				/* Note: Do not pass the receiver as an argument. */
				gTP->FVmscript_isource = gTP->FVmscript_iargument;
				*msg = gTP->FVmscript_isource;
				--n;
				goto CallDetail;
				}
			goto UseClassMethod;
			break;
		
		case TYSTRUCTURE:
			/*  If receiver is a Structure, and the message matches one of its attributes, */
			/*	then the matching attribute takes prescedence over any class methods. */
			/*	Is the message an attribute of the receiver? */
			gTP->FVmscript_iargument = TStructure_GetIV1(gCP,gTP,*target,*msg);
			if ((gTP->FVmscript_iargument.Tag != TYVOID) && (gTP->FVmscript_iargument.Tag != TYERROR))
				{
				/* Call the attribute of the receiver directly instead using the class method. */
				/* Note: Do not pass the receiver as an argument. */
				gTP->FVmscript_isource = gTP->FVmscript_iargument;
				*msg = gTP->FVmscript_isource;
				--n;
				goto CallDetail;
				}
			goto UseClassMethod;
			break;
		
		case TYVECTOR:
			/*  If receiver is a Vector, and the message matches one of its attributes, */
			/*	then the matching attribute takes prescedence over any class methods. */
			/*	Is the message an attribute of the receiver? */
			gTP->FVmscript_iargument = TVector_GetIV1(gCP,gTP,*target,*msg);
			if ((gTP->FVmscript_iargument.Tag != TYVOID) && (gTP->FVmscript_iargument.Tag != TYERROR))
				{
				/* Call the attribute of the receiver directly instead using the class method. */
				/* Note: Do not pass the receiver as an argument. */
				gTP->FVmscript_isource = gTP->FVmscript_iargument;
				*msg = gTP->FVmscript_isource;
				--n;
				goto CallDetail;
				}
			goto UseClassMethod;
			break;
		
		case TYDICTIONARY:
			/*  If receiver is a Dictionary, and the message matches one of its attributes, */
			/*	then the matching attribute takes prescedence over any class methods. */
			/*	Is the message an attribute of the receiver? */
			gTP->FVmscript_iargument = TDictionary_GetIV1(gCP,gTP,*target,*msg);
			if ((gTP->FVmscript_iargument.Tag != TYVOID) && (gTP->FVmscript_iargument.Tag != TYERROR))
				{
				/* Call the attribute of the receiver directly instead using the class method. */
				/* Note: Do not pass the receiver as an argument. */
				gTP->FVmscript_isource = gTP->FVmscript_iargument;
				*msg = gTP->FVmscript_isource;
				--n;
				goto CallDetail;
				}
			goto UseClassMethod;
			break;
		
		case TYDIRECTORY:
			/*  If receiver is a Directory, and the message matches one of its attributes, */
			/*	then the matching attribute takes prescedence over any class methods. */
			/*	Is the message an attribute of the target? */
			gTP->FVmscript_iargument = TDirectory_GetIV1(gCP,gTP,*target,*msg);
			if ((gTP->FVmscript_iargument.Tag != TYVOID) && (gTP->FVmscript_iargument.Tag != TYERROR))
				{
				/* Call the attribute of the receiver directly instead using the class method. */
				/* Note: Do not pass the receiver as an argument. */
				gTP->FVmscript_isource = gTP->FVmscript_iargument;
				*msg = gTP->FVmscript_isource;
				--n;
				goto CallDetail;
				}
			goto UseClassMethod;
			break;
		
		default:
			UseClassMethod:
			/*  Load the methods Dictionary for the target type. */
			if ((gTP->FVmscript_type == TYSTRUCTURE) && (Structure(*target)->itsMethods != NIL))
				{
				gTP->FVmscript_isource = TOBJ(Structure(*target)->itsMethods->itsUserTypeMethods);
				}
			else
				{
				gTP->FVmscript_isource = _TObject_TypeMethods(gTP->FVmscript_type);
				}
			break;
		}	/* end switch */
    }	/* end if */
else
/*  If the message is a quoted symbol, then this is treated the same as a super. */
/*  Note:   If a quoted symbol is the message, then the method */
/*          Procedure is chosen from the parents methods Dictionary. */
if ((msg->Tag == TYQUOTEDSYMBOL) && (n >= 1))
    { 
    /*  Load the type of the receiving argument. */
    gTP->FVmscript_type = asTag(target);
	/*  If target is a host object, send the message to the host object. */

	/*  If receiver is an Lambda, and message is its child, then call the child Lambda directly. */
	if (gTP->FVmscript_type == TYLAMBDA)
		{
		/* Is the message a child of the receiver? */
		gTP->FVmscript_iargument = TLambda_GetPvMember(gCP,gTP,*target,SymbolArray(*msg));
		if (gTP->FVmscript_iargument.Tag != TYVOID)
			{
			/* Call the child of the receiver directly instead of sending a message. */
			/* Note: Do not pass the receiver as an argument. */
			gTP->FVmscript_isource = gTP->FVmscript_iargument;
			*msg = gTP->FVmscript_isource;
			--n;
			goto CallDetail;
			}
		}

	/*  Load the methods Dictionary for the receiving type. */
    if ((gTP->FVmscript_type == TYSTRUCTURE) && (Structure(*target)->itsMethods != NIL) &&
        (Structure(*target)->itsMethods->itsUserTypeParent != NIL))
        {
        gTP->FVmscript_isource = TOBJ(Structure(*target)->itsMethods->itsUserTypeParent->itsUserTypeMethods);
        }
    else
    if ((gTP->FVmscript_type == TYSTRUCTURE) && (Structure(*target)->itsMethods != NIL))
        {
        gTP->FVmscript_isource = TOBJ(Structure(*target)->itsMethods->itsUserTypeMethods);
        }
    else
        {
        gTP->FVmscript_isource = _TObject_TypeMethods(gTP->FVmscript_type);
        }
    }
else
    { 
    if (msg->Tag == TYERROR)
        *retValue = *msg;
    else
        *retValue = gCP->FVmScript_ERROR_VMSEND_MESSAGE;
    goto SendError;
    }	/* end else */




/*  If the methods Dictionary is empty try the default global method */
if (gTP->FVmscript_isource.Tag != TYDICTIONARY)
    {
    goto TryDefaultGlobalMethod;
    }


/*  Load the method from the methods Dictionary for the receiving type. */
/*  Use the hidden index memo as a hint in quick search. */
if ((inRange(asMemo(msg),0L,Dictionary(gTP->FVmscript_isource)->itsMaxItemIndex)) &&
    (BondArray(gTP->FVmscript_isource)[asMemo(msg)].Key == msg->u.Object))
    {
    gTP->FVmscript_isource = BondArray(gTP->FVmscript_isource)[asMemo(msg)].Value;
    }
else
    {
    gTP->FVmscript_iindex = TDictionary_BSearchKey(gCP,gTP,(TDictionary*)gTP->FVmscript_isource.u.Object, msg->u.Object, (short*)(NUM)&gTP->FVmscript_Selection);
    if (gTP->FVmscript_Selection != 0)
        {
		TryDefaultGlobalMethod:
		gTP->FVmscript_isource = msg->u.Symbol->itsGlobalValue;
		if (gTP->FVmscript_isource.Tag == TYVOID)
			{
			*retValue = gCP->FVmScript_ERROR_VMSEND_MESSAGE;
			goto SendError;
			}
        }
    else
        {
        gTP->FVmscript_isource = BondArray(gTP->FVmscript_isource)[gTP->FVmscript_iindex.u.Int].Value;
        asMemo(msg) = gTP->FVmscript_iindex.u.Int;
        }
    }



*proc = gTP->FVmscript_isource;
if (proc->Tag == TYERROR)
    goto SendError;
else
    goto CallDetail;


/* We drop here if there was any errors in processing the send command */
SendError:
ExitOnError(*retValue);



/* Evaluate the proc  */
CallDetail:


*retValue = FSmartbase_Evalv(gCP, gTP, *proc, argc-1, &argv[0]);
FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FCompile_BuildInitializer

The FCompile_BuildInitializer function converts the specified text into a symbol. 
The symbol`s value in the symbol global value member is given the value of a TYSPECIALFORM. 

#endif

TVAL FCompile_BuildInitializer(LpXCONTEXT gCP, LpTHREAD gTP, NUM modifier, TVAL type, TVAL name, TVAL constant)
{
StartFrame
DeclareTVAL(ret);
EndFrame

/* Set the result equal to the constant binding. */
*ret = constant;

/* Set the result preferred type equal to the declared type. */
switch (modifier)
	{
	case AMREGISTER:
		ret->DeclaredType = TYNUM;
		if (type.Tag == TYQUOTEDSYMBOL)
			{
			if (strcmp(SymbolArray(type),"CharPointer") == 0) ret->DeclaredType = TYCHARPOINTER;
			else if (strcmp(SymbolArray(type),"FloatPointer") == 0) ret->DeclaredType = TYFLOATPOINTER;
			else if (strcmp(SymbolArray(type),"Integer") == 0) ret->DeclaredType = TYNUM;
			else if (strcmp(SymbolArray(type),"IntPointer") == 0) ret->DeclaredType = TYINTPOINTER;
			else if (strcmp(SymbolArray(type),"JumpPointer") == 0) ret->DeclaredType = TYJUMPPOINTER;
			else if (strcmp(SymbolArray(type),"LongPointer") == 0) ret->DeclaredType = TYLONGPOINTER;
			else if (strcmp(SymbolArray(type),"Number") == 0) ret->DeclaredType = TYREAL;
			else if (strcmp(SymbolArray(type),"NumPointer") == 0) ret->DeclaredType = TYREALPOINTER;
			else if (strcmp(SymbolArray(type),"ShortPointer") == 0) ret->DeclaredType = TYSHORTPOINTER;
			else if (strcmp(SymbolArray(type),"WordPointer") == 0) ret->DeclaredType = TYWORDPOINTER;
			else goto InValidType;

			/* Register variables can only be initialized to Integers or Numbers. */
			if (ret->DeclaredType == TYREAL)
				{
				if (ret->Tag != TYREAL)
					{
					ret->u.Real = 0.0;
					ret->Tag = TYREAL;
					}
				}
			else
			if (ret->Tag != TYNUM)
				{
				ret->u.Int = 0;
				ret->Tag = TYNUM;
				}
			}
		break;

	default:
		ret->DeclaredType = TYTVAL;
		if (type.Tag == TYQUOTEDSYMBOL)
			{
			if (strcmp(SymbolArray(type),"Lambda") == 0) ret->DeclaredType = TYLAMBDA;
			else if (strcmp(SymbolArray(type),"Boolean") == 0) ret->DeclaredType = TYBOLE;
			else if (strcmp(SymbolArray(type),"BitVector") == 0) ret->DeclaredType = TYBITVECTOR;
			else if (strcmp(SymbolArray(type),"Brick") == 0) ret->DeclaredType = TYBRICK;
			else if (strcmp(SymbolArray(type),"ByteVector") == 0) ret->DeclaredType = TYBYTEVECTOR;
			else if (strcmp(SymbolArray(type),"Character") == 0) ret->DeclaredType = TYCHAR;
			else if (strcmp(SymbolArray(type),"Dictionary") == 0) ret->DeclaredType = TYDICTIONARY;
			else if (strcmp(SymbolArray(type),"Directory") == 0) ret->DeclaredType = TYDIRECTORY;
			else if (strcmp(SymbolArray(type),"FltVector") == 0) ret->DeclaredType = TYFLTVECTOR;
			else if (strcmp(SymbolArray(type),"Function") == 0) ret->DeclaredType = TYCPROCEDURE;
			else if (strcmp(SymbolArray(type),"Integer") == 0) ret->DeclaredType = TYNUM;
			else if (strcmp(SymbolArray(type),"IntVector") == 0) ret->DeclaredType = TYINTVECTOR;
			else if (strcmp(SymbolArray(type),"Matrix") == 0) ret->DeclaredType = TYMATRIX;
			else if (strcmp(SymbolArray(type),"Number") == 0) ret->DeclaredType = TYREAL;
			else if (strcmp(SymbolArray(type),"NumMatrix") == 0) ret->DeclaredType = TYNUMMATRIX;
			else if (strcmp(SymbolArray(type),"NumVector") == 0) ret->DeclaredType = TYNUMVECTOR;
			else if (strcmp(SymbolArray(type),"ObjVector") == 0) ret->DeclaredType = TYOBJVECTOR;
			else if (strcmp(SymbolArray(type),"ShortVector") == 0) ret->DeclaredType = TYSHORTVECTOR;
			else if (strcmp(SymbolArray(type),"LongVector") == 0) ret->DeclaredType = TYLONGVECTOR;
			else if (strcmp(SymbolArray(type),"String") == 0) ret->DeclaredType = TYSTRING;
			else if (strcmp(SymbolArray(type),"Structure") == 0) ret->DeclaredType = TYSTRUCTURE;
			else if (strcmp(SymbolArray(type),"Symbol") == 0) ret->DeclaredType = TYSYMBOL;
			else if (strcmp(SymbolArray(type),"Text") == 0) ret->DeclaredType = TYTEXT;
			else if (strcmp(SymbolArray(type),"Vector") == 0) ret->DeclaredType = TYVECTOR;
			else if (strcmp(SymbolArray(type),"Word") == 0) ret->DeclaredType = TYTVAL;
			else goto InValidType;

			/* All other variables must be initialized to Boolean, Character,    */
			/*  Integer or Number, if they are declared with a preferred type of */
			/*  Boolean, Character, Integer or Real.                             */
			if ((ret->DeclaredType == TYREAL) && (ret->Tag != TYREAL))
				{
				ret->u.Real = 0.0;
				ret->Tag = TYREAL;
				}
			else
			if ((ret->DeclaredType == TYNUM) && (ret->Tag != TYNUM))
				{
				ret->u.Int = 0;
				ret->Tag = TYNUM;
				}
			else
			if ((ret->DeclaredType == TYCHAR) && (ret->Tag != TYCHAR))
				{
				ret->u.Int = 0;
				ret->Tag = TYCHAR;
				}
			else
			if ((ret->DeclaredType == TYBOLE) && (ret->Tag != TYBOLE))
				{
				ret->u.Bool = FALSE;
				ret->Tag = TYBOLE;
				}
			}
		break;
	}


/* Declare this an initializer for a constant or for a variable */
if ((modifier == AMCVOFFSET) && 
    (name.Tag == TYSYMBOL) &&
    (strncmp(SymbolArray(name),"__C",3) == 0))
	{
	asTail(ret) = TRUE;		/* Declare this an initializer for a constant NOT a variable. */
	}
else
	{
	asTail(ret) = FALSE;	/* Declare this an initializer for a variable NOT a constant. */
	}

FrameExit(*ret);

InValidType:
*ret = TERROR("!compile: invalid type for variable declaration!");
FrameExit(*ret);
}


