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

#define _C_TLambda
#define _SMARTBASE
#if 0
TLambda.c

Implementation of the Lambda object.

This source file contains the main evaluation functions for the Smartbase 
Lambda Object. An Lambda object is the fundamental executable data entity
stored in the Smartbase Lambda oriented database. 


PARENT:             TObject 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "tsymbol.h"
#include "tlambda.h"
#include "fpropty.h"
#include "futil3.h"
#include "fpred2.h"
#include "terror.h"
#include "fdebug.h"
#include "fcompile.h"
#include "fproc.h"
#include "fvmscpt.h"

						 
/*--------------------------------------------------------------------------------------- */
#if 0
TLambda_Init

Initialize the TLambda class.

Note:   This function should only be called once at the beginning of the application.

#endif

void    TLambda_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ec);
DeclareTVAL(argTval);
EndFrame

/*  Don't initialize more than once. */

if (gCP->TLambda_Initialized) 
    {FrameReset; return;}
    
gCP->TLambda_Initialized  = TRUE;
TObject_Init(gCP,gTP);

/*  Initialize the new types for this class. */

FSmartbase_NewType  (gCP, gTP,
					 TYLAMBDA,
                    (LpCHAR)"Lambda",
                    _TObject_TfTOBJECT,
                    sizeof(OBJ),
                    (LpFNEW)&FProcedure_Make,
                    &TLambda_Mark,
                    &TLambda_GlobalMark,
                    &FObject_ObjAnyCnv,
                    &FObject_CompareNever,
                    &TLambda_SetIV1,
                    &TLambda_SetIV2,
                    &TLambda_SetIV3,
                    &TLambda_GetIV1,
                    &TLambda_GetIV2,
                    &TLambda_GetIV3,
                    &TObject_MapDefault,
                    &TObject_Mapc,
                    &TObject_PrintDefault,
                    &TLambda_Load,
					&TLambda_Save,
					&TLambda_ComputeSize,
					&TLambda_Copy,
					&TLambda_Doomed);

FSmartbase_NewType  (gCP,
					 gTP,
					 TYCPROCEDURE,
                    (LpCHAR)"Function",
                    _TObject_TfTOBJECT,
                    sizeof(OBJ),
                    (LpFNEW)&TObject_NewNever,
                    &TSymbol_Mark,
                    &TObject_GlobalMarkNever,
                    &FObject_ObjAnyCnv,
                    &FObject_CompareNever,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapDefault,
                    &TObject_Mapc,
                    &FProcedure_PrintcProcedure,
                    (LpFNLOAD)&FProcedure_LoadCProcedure,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType  (gCP,
					 gTP,
					 TYCMACRO,
                    (LpCHAR)"cMacro",
                    _TObject_TfTOBJECT,
                    sizeof(OBJ),
                    (LpFNEW)&TObject_NewNever,
                    &TSymbol_Mark,
                    &TObject_GlobalMarkNever,
                    &FObject_ObjAnyCnv,
                    &FObject_CompareNever,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapDefault,
                    &TObject_Mapc,
                    &FProcedure_PrintcMacro,
                    (LpFNLOAD)&FProcedure_LoadCMacro,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType  (gCP,
					 gTP,
					 TYPCODE,
                    (LpCHAR)"Pcode",
                    _TObject_TfNATIVE,
                    sizeof(SHORT),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_ShortAnyCnv,
                    &TObject_ShortAnyCmp,
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

FSmartbase_NewType  (gCP,
					 gTP,
				    TYVMEVALUATOR,
				    (LpCHAR)"VmEvaluator",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&TObject_NewNever,
					&TSymbol_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&FObject_SetIV1Never,
					&FObject_SetIV2Never,
					&FObject_SetIV3Never,
					&FObject_GetIV1Never,
					&FObject_GetIV2Never,
					&FObject_GetIV3Never,
					&TObject_MapDefault,
					&TObject_Mapc,
					&FProcedure_PrintcProcedure,
					(LpFNLOAD)&FProcedure_LoadCProcedure,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType  (gCP,
					 gTP,
					 TYMACRO,
                    (LpCHAR)"Macro",
                    _TObject_TfTOBJECT,
                    sizeof(OBJ),
                    (LpFNEW)&TObject_NewNever,
                    &TLambda_Mark,
                    &TObject_GlobalMarkNever,
                    &FObject_ObjAnyCnv,
                    &FObject_CompareNever,
                    &TLambda_SetIV1,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &TLambda_GetIV1,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapDefault,
                    &TObject_Mapc,
                    &TObject_PrintDefault,
                    (LpFNLOAD)&FProcedure_LoadMacro,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

                                
/*  Register the DRM VM emulator function. After this registration,we can now */
/*  evaluate Lambdas. Before this registration, only C functions can be evaluated. */

gCP->TLambda_LispVirtualMachine = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"drmVirtualMachine");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_LispVirtualMachine,TRUE);
FSmartbase_RegisterVMEvaluator(gCP,gTP,(LpCHAR)"drmVirtualMachine",(LpVMEVALUATOR)&FVmScript_Eval);

/*  Register the VM Native Evaluator function. After this registration,we can now */
/*  evaluate native Lambdas. Before this registration, only C functions can be evaluated. */

aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"nativeEvaluator");
FObject_Perm(gCP,gTP,(TObject*)aSymbol,TRUE);
FSmartbase_RegisterVMEvaluator(gCP,gTP,(LpCHAR)"nativeEvaluator",(LpVMEVALUATOR)&TLambda_NativeEval);

/*  Initialize related types. */

TContinuation_Init(gCP,gTP);

/*  Initialize the states and switches */

gCP->TLambda_MaxInputLen = _FSmartbase_MAXBUFFERLEN;

/*  Initialize global interpreter objects */


gTP->TLambda_TheProc = gTP->TLambda_saveProc = TLambda_New(gCP,gTP);
FObject_Perm(gCP,gTP,(TObject*)gTP->TLambda_TheProc, TRUE);
asObject(argTval) = (TObject*)gTP->TLambda_TheProc;
asTag(argTval) = gTP->TLambda_TheProc->itsObjectType;

gTP->TLambda_CurrentProcedure = NULL;
gTP->TLambda_TheContinuation = NULL;
gTP->TLambda_ThePropList = gCP->Tval_VOID;

/*  Register the keyword symbols and assign globals */

gCP->TLambda_assign = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)":=");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_assign,TRUE);
gCP->TLambda_by = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"by");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_by,TRUE);
gCP->TLambda_close = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"close");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_close,TRUE);
gCP->TLambda_console = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"console");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_console,TRUE);
gCP->TLambda_data = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"data");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_data,TRUE);
gCP->TLambda_do = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"do");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_do,TRUE);
gCP->TLambda_EditWindow = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"EditWindow");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_EditWindow,TRUE);
gCP->TLambda_else = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"else");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_else,TRUE);
gCP->TLambda_end = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"end");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_end,TRUE);
gCP->TLambda_for = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"for");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_for,TRUE);
gCP->TLambda_from = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"from");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_from,TRUE);
gCP->TLambda_globals = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_globals");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_globals,TRUE);
asTag(&VALUEOF(gCP->TLambda_globals)) = TYGLOBALS;
gCP->TLambda_nil = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"()");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_nil,TRUE);
VALUEOF(gCP->TLambda_nil) = gCP->Tval_VOID;
gCP->TLambda_include = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"include");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_include,TRUE);
gCP->TLambda_pi = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"pi");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_pi,TRUE);
asTag(&VALUEOF(gCP->TLambda_pi)) = TYREAL;
asReal(&VALUEOF(gCP->TLambda_pi)) = 3.1415926535898;
gCP->TLambda_save = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"save");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_save,TRUE);
gCP->TLambda_script = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"script");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_script,TRUE);
gCP->TLambda_select = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"select");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_select,TRUE);
gCP->TLambda_then = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"then");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_then,TRUE);
gCP->TLambda_title = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"title");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_title,TRUE);
gCP->TLambda_to = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"to");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_to,TRUE);
gCP->TLambda_until = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"until");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_until,TRUE);
gCP->TLambda_workspace = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"workspace");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_workspace,TRUE);
gCP->TLambda_otherwise = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"otherwise");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_otherwise,TRUE);
gCP->TLambda_quote = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"'");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_quote,TRUE);


gCP->TLambda_dotcdr = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)".");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_dotcdr,TRUE);
asTag(ec) = TYPCODE;
asShort(ec) = PERIODTOK;
TSymbol_SetGlobalValue(gCP,gTP,gCP->TLambda_dotcdr,*ec);

gCP->TLambda_vars = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"vars");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_vars,TRUE);
gCP->TLambda_args = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"args");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_args,TRUE);
gCP->TLambda_faces = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"faces");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_faces,TRUE);
gCP->TLambda_self = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"self");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_self,TRUE);
gCP->TLambda_svars = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"svars");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_svars,TRUE);
gCP->TLambda_pvars = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"pvars");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_pvars,TRUE);
gCP->TLambda_cvars = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"cvars");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_cvars,TRUE);
gCP->TLambda_rvars = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"regs");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_rvars,TRUE);
gCP->TLambda_goto = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"goto");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_goto,TRUE);
gCP->TLambda_label = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"label");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_label,TRUE);
gCP->TLambda_Sv = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Sv");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Sv,TRUE);
gCP->TLambda_Av = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Av");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Av,TRUE);
gCP->TLambda_Tv = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Tv");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Tv,TRUE);
gCP->TLambda_EvalWhenDoomed = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"EvalWhenDoomed");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_EvalWhenDoomed,TRUE);
gCP->TLambda_Doomed = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"doomed");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Doomed,TRUE);
gCP->TLambda_Pv = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Pv");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Pv,TRUE);
gCP->TLambda_Cv = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Cv");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Cv,TRUE);
gCP->TLambda_Rv = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Rv");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Rv,TRUE);
gCP->TLambda_Pc = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Pc");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Pc,TRUE);
gCP->TLambda_Sc = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Sc");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Sc,TRUE);
gCP->TLambda_Nc = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Nc");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Nc,TRUE);
gCP->TLambda_In = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"In");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_In,TRUE);
gCP->TLambda_TvalIn.Tag = TYSYMBOL;
gCP->TLambda_TvalIn.u.Symbol = gCP->TLambda_In;
gCP->TLambda_Binding = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Binding");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Binding,TRUE);
gCP->TLambda_BreakList = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"breakList");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_BreakList,TRUE);
gCP->TLambda_TvalBinding.Tag = TYSYMBOL;
gCP->TLambda_TvalBinding.u.Symbol = gCP->TLambda_Binding;
gCP->TLambda_Vm = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"Vm");
FObject_Perm(gCP,gTP,(TObject*)gCP->TLambda_Vm,TRUE);

FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"makeLambda",(LpFUNC)&FProcedure_Make);

FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"makeMacro",(LpFUNC)&FProcedure_MakeMacro);

FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"isLambda",(LpFUNC)&FProcedure_Procedurep);

FProcedure_NewCProcedure(gCP, gTP, &aSymbol,(LpCHAR)"makeVMLambda",(LpFUNC)&TLambda_MakeVMLambda);

{FrameReset; return;}
}

/*--------------------------------------------------------------------------------------- */
#if 0
TLambda_NativeEval

The main native machine evaluator for Lambda objects.

Note:   We may evaluate a single Lambda object with a native function.
        
#endif


TVAL TLambda_NativeEval(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* proc,NUM argc,TVAL argv[])
{

return((*((LpVMEVALUATOR)proc->NativeCodeVector))(gCP,gTP,proc,argc,argv));

}		 
	 
/*--------------------------------------------------------------------------------------- */
#if 0
TLambda_MakeVMLambda

The function which assigns an Lambda as the Virtual Machine evaluator for other Lambda objects.
The assignment is made only to a global symbol object which identifies an Lambda as the 
virtual machine for other Lambda objects.

#endif


TVAL TLambda_MakeVMLambda(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(LambdaVM);
DeclareTVAL(name);
EndFrame

if ((argc != 2) || (argv[0].Tag != TYLAMBDA) || (argv[1].Tag != TYSYMBOL))
	{
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

*LambdaVM = argv[0];
*name = argv[1];

FSmartbase_RegisterVMEvaluator(gCP,gTP,SymbolArray(*name),(LpVMEVALUATOR)&TLambda_LambdaVMEval);
name->u.Symbol->itsVMEmulator = LambdaVM->u.Lambda;

FrameExit(*LambdaVM);
}		 
		 
/*--------------------------------------------------------------------------------------- */
#if 0
Doomed

Garbage collection is about to delete this object. Dispose of the array data.

Note:   This method should only be called by mark and sweep garbage collection!
        This method warns the object that it is about to be deleted. Garbage
        collection first warns all the doomed objects, then it deletes all doomed
        objects.
        
        Do close any files and clean up anything necessary here.
        Do free any resources which you have allocated of which you are the sole owner.

        Do not send delete or dispose messages to any referenced objects,
        Let garbage collection do this.

        Do not delete or release any of your own storage here.
        Let garbage collection do this.

#endif

void    TLambda_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TLambda*    self = (TLambda*)asObject(&selfTval);

/* Evaluate the Lambda (if requested) */
if (self->EvalWhenDoomed == TRUE)
	{
	gCP->FSmartbase_Eval((POINTER)gCP,gTP,selfTval,0,NULL);
	}

self->EvalWhenDoomed = FALSE;
}


/*--------------------------------------------------------------------------------------- */
#if 0
TLambda_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL    TLambda_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
StartFrame
DeclareOBJ(TLambda,it);
DeclareTVAL(retTval);
DeclareTVAL(ret);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

/*  If the bResolve switch is zero (0), then we must create a new TLambda */
/*  and the TLambda's handle to the Load Vector returning its file id. */
/*  This registration prevents recreating the same object more than once. */
if(bResolve == 0)
    {
    it = TLambda_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    /*  Retrieve the proper TObject handle from the file  */
    /*  object id and the file Load Vector. */
    it = (TLambda*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
			
		it->ClassVariables		= ((*ret=TObject_LoadTval(gCP,gTP,TLambdaOnDiskPtr(aHMemory,0)->ClassVariables)).Tag != TYSTRUCTURE) ? NIL : ret->u.Structure;
		it->ArgumentVariables	= ((*ret=TObject_LoadTval(gCP,gTP,TLambdaOnDiskPtr(aHMemory,0)->ArgumentVariables)).Tag != TYSTRUCTURE) ? NIL : ret->u.Structure;
		it->TemporaryVariables	= ((*ret=TObject_LoadTval(gCP,gTP,TLambdaOnDiskPtr(aHMemory,0)->TemporaryVariables)).Tag != TYSTRUCTURE) ? NIL : ret->u.Structure;
		it->RegisterVariables	= ((*ret=TObject_LoadTval(gCP,gTP,TLambdaOnDiskPtr(aHMemory,0)->RegisterVariables)).Tag != TYSTRUCTURE) ? NIL : ret->u.Structure;

		it->PersistantVariables	= ((*ret=TObject_LoadTval(gCP,gTP,TLambdaOnDiskPtr(aHMemory,0)->PersistantVariables)).Tag != TYSTRUCTURE) ? NIL : ret->u.Structure;
        if (it->PersistantVariables != NIL)
            {
            /* Reset the nested invocation count */
            /* for these persistent variables. */
            it->PersistantVariables->itsCdr = gCP->Tval_VOID;
            }

		it->ConstantVariables	= ((*ret=TObject_LoadTval(gCP,gTP,TLambdaOnDiskPtr(aHMemory,0)->ConstantVariables)).Tag != TYSTRUCTURE) ? NIL : ret->u.Structure;
        if (it->ConstantVariables != NIL)
            {
            /* Reset the nested invocation count */
            /* for these persistent  constant variables. */
            it->ConstantVariables->itsCdr = gCP->Tval_VOID;
            }

		it->PcodeVector			= ((*ret=TObject_LoadTval(gCP,gTP,TLambdaOnDiskPtr(aHMemory,0)->PcodeVector)).Tag != TYPCODEVECTOR) ? NIL : ret->u.PcodeVector;
		*ret					= TObject_LoadTval(gCP,gTP,TLambdaOnDiskPtr(aHMemory,0)->DebuggerSource);
		it->DebuggerSource		= (_TObject_TypeFlag(ret->Tag) == _TObject_TfTOBJECT) ? ret->u.Object : NIL;		
		*ret					= TObject_LoadTval(gCP,gTP,TLambdaOnDiskPtr(aHMemory,0)->VirtualMachine);
		it->VirtualMachine		= ((_TObject_TypeFlag(ret->Tag) == _TObject_TfTOBJECT) && (ret->u.Object->itsObjectType == TYSYMBOL)) ? ret->u.Symbol : NIL;
        it->NativeCodeVector    = NIL;
		it->Interfaces			= ((*ret=TObject_LoadTval(gCP,gTP,TLambdaOnDiskPtr(aHMemory,0)->Interfaces)).Tag != TYSTRUCTURE) ? NIL : ret->u.Structure;
        it->EvalWhenDoomed		= (BOLE)TLambdaOnDiskPtr(aHMemory,0)->EvalWhenDoomed;
        it->InUse				= 0;
        asTag(retTval)	= it->itsObjectType;
        asObject(retTval) = (TObject*)it;
        }
    else
        *retTval = gCP->TObject_ERROR_INVALID;
    }

FrameExit(*retTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TLambda_GlobalMark

The marking function for  global objects associated with TLambda.

#endif

TVAL TLambda_GlobalMark(LpXCONTEXT gCP,LpTHREAD gTP)
{
NUM indexOf;

/*  Send mark messages to all values on the operations stack. */
/*  Note:   A stack value is current if it is below the active pointer. */

for (indexOf = 0; indexOf < TopOfStack; ++indexOf)
    {
    if (gTP->TvalStack[indexOf].Tag != TYVOID)
        {
        TObject_MarkTval(gCP,gTP,gTP->TvalStack[indexOf]);
        }
    }
    
/*  Send a mark message to base lisp script object. */

TObject_MarkObj(gCP,gTP,(TObject*)gTP->TLambda_saveProc);

/*  Send a mark message to the current lisp procedure object. */

TObject_MarkObj(gCP,gTP,(TObject*)gTP->TLambda_CurrentProcedure);

/*  Send a mark message to the current lisp continuation object. */

TObject_MarkObj(gCP,gTP,(TObject*)gTP->TLambda_TheContinuation);

/*  Send a mark message to the property list object. */

if (asTag(&gTP->TLambda_ThePropList) != TYVOID)
    {
    TObject_MarkTval(gCP,gTP,gTP->TLambda_ThePropList);
    }
    
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,

#endif

void    TLambda_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TLambda*     self = (TLambda*)asObject(&selfTval);

/* inherited::Mark(); */

/*  Mark the Lambda's items so they won't be garbage collected. */

TObject_MarkObj(gCP,gTP,(TObject*)self->ClassVariables);
TObject_MarkObj(gCP,gTP,(TObject*)self->ArgumentVariables);
TObject_MarkObj(gCP,gTP,(TObject*)self->TemporaryVariables);
TObject_MarkObj(gCP,gTP,(TObject*)self->PersistantVariables);
TObject_MarkObj(gCP,gTP,(TObject*)self->ConstantVariables);
TObject_MarkObj(gCP,gTP,(TObject*)self->RegisterVariables);
TObject_MarkObj(gCP,gTP,(TObject*)self->PcodeVector);
TObject_MarkObj(gCP,gTP,(TObject*)self->DebuggerSource);
TObject_MarkObj(gCP,gTP,(TObject*)self->VirtualMachine);
TObject_MarkObj(gCP,gTP,(TObject*)self->NativeCodeVector);
TObject_MarkObj(gCP,gTP,(TObject*)self->Interfaces);
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void    TLambda_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;

*aSize += sizeof(TLambdaOnDisk);
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself to a disk
format append yourself to the handle and return.

#endif

HMemory TLambda_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory aHMemory)
{
TLambda*         self = (TLambda*)asObject(&selfTval);
long            theOffset;

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

if (self->PersistantVariables != NIL)
    {
    /* Reset the nested invocation count */
    /* for these persistent variables. */
    self->PersistantVariables->itsCdr = gCP->Tval_VOID;
    }

if (self->ConstantVariables != NIL)
    {
    /* Reset the nested invocation count */
    /* for these persistent  constant variables. */
    self->ConstantVariables->itsCdr = gCP->Tval_VOID;
    }

if (self->RegisterVariables != NIL)
    {
    /* Reset the nested invocation count */
    /* for these persistent constant variables. */
    self->RegisterVariables->itsCdr = gCP->Tval_VOID;
    }

TLambdaOnDiskPtr(aHMemory,theOffset)->ClassVariables		 = TObject_RegisterTval(gCP,gTP,TOBJ((TObject*)self->ClassVariables));
TLambdaOnDiskPtr(aHMemory,theOffset)->ArgumentVariables   = TObject_RegisterTval(gCP,gTP,TOBJ((TObject*)self->ArgumentVariables));
TLambdaOnDiskPtr(aHMemory,theOffset)->TemporaryVariables  = TObject_RegisterTval(gCP,gTP,TOBJ((TObject*)self->TemporaryVariables));
TLambdaOnDiskPtr(aHMemory,theOffset)->PersistantVariables = TObject_RegisterTval(gCP,gTP,TOBJ((TObject*)self->PersistantVariables));
TLambdaOnDiskPtr(aHMemory,theOffset)->ConstantVariables	 = TObject_RegisterTval(gCP,gTP,TOBJ((TObject*)self->ConstantVariables));
TLambdaOnDiskPtr(aHMemory,theOffset)->RegisterVariables	 = TObject_RegisterTval(gCP,gTP,TOBJ((TObject*)self->RegisterVariables));
TLambdaOnDiskPtr(aHMemory,theOffset)->PcodeVector         = TObject_RegisterTval(gCP,gTP,TOBJ((TObject*)self->PcodeVector));
TLambdaOnDiskPtr(aHMemory,theOffset)->DebuggerSource      = TObject_RegisterTval(gCP,gTP,TOBJ((TObject*)self->DebuggerSource));
TLambdaOnDiskPtr(aHMemory,theOffset)->VirtualMachine      = TObject_RegisterTval(gCP,gTP,TOBJ((TObject*)self->VirtualMachine));
TLambdaOnDiskPtr(aHMemory,theOffset)->NativeCodeVector    = gCP->TObject_VOID;
TLambdaOnDiskPtr(aHMemory,theOffset)->Interfaces			 = TObject_RegisterTval(gCP,gTP,TOBJ((TObject*)self->Interfaces));
TLambdaOnDiskPtr(aHMemory,theOffset)->EvalWhenDoomed		 = self->EvalWhenDoomed;

return(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a parent Lambda. This function calls clone on
all the child Lambdas in the parent Lambdas persistent variables
structure. Each child Lambda is cloned with a copy of the new
parent Lambdas new persistent variables structure.

#endif

TObject*    TLambda_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TLambda,theCopy);
DeclareOBJ(TStructure,Pv);
DeclareTVAL(member);
DeclareTVAL(copyTval);
EndFrame

self = (TLambda*)asObject(&selfTval);

/*	Create a new outter structure for the copied parent Lambda. */
theCopy = TLambda_New(gCP,gTP);
copyTval->u.Object = (TObject *)theCopy;
copyTval->Tag = copyTval->u.Object->itsObjectType;

/*  Copy the most members of the original structure */
/*  to the copied Lambda, but make a copy of the Lambda */
/*  persistent variable structure. */
theCopy->ClassVariables = self->ClassVariables;
theCopy->ArgumentVariables = self->ArgumentVariables;
theCopy->TemporaryVariables = self->TemporaryVariables;
theCopy->ConstantVariables = NIL;
theCopy->PersistantVariables = NIL;
theCopy->PcodeVector = self->PcodeVector;
theCopy->DebuggerSource = self->DebuggerSource;
theCopy->VirtualMachine = self->VirtualMachine;
theCopy->NativeCodeVector = self->NativeCodeVector;
theCopy->Interfaces = self->Interfaces;
theCopy->EvalWhenDoomed = self->EvalWhenDoomed;

/* Make a copy of the interfaces belonging to this parent Lambda. */
if (self->Interfaces != NIL)
    {
    theCopy->Interfaces = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->Interfaces));
    }

/* Make a copy of the persistant  constant variables belonging to this parent Lambda. */
if (self->ConstantVariables != NIL)
    {
    theCopy->ConstantVariables = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->ConstantVariables));
    }

/* Make a copy of the persistant constant variables belonging to this parent Lambda. */
if (self->RegisterVariables != NIL)
    {
    theCopy->RegisterVariables = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->RegisterVariables));
    }

/* Make clones of all child and friend Lambdas belonging to this parent Lambda. */
/* Note: Friend Lambdas require a deep recursive copy, while child Lambdas can be cloned. */
if (self->PersistantVariables != NIL)
    {
    // theCopy->PersistantVariables.u.Object = (TObject*)Pv = TStructure_Copy(gCP,gTP,self->PersistantVariables);
	Pv = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->PersistantVariables));	
	theCopy->PersistantVariables = Pv;
    /* Reset the nested invocation count */
    /* for the new persistent variables. */
    Pv->itsCdr = gCP->Tval_VOID;

    for(indexOf = 0; indexOf <  Pv->itsMaxItemIndex; ++indexOf)
        {
        *member = atHMBind(Pv->itsDictionaryArray,indexOf).Value;
		/* Perform a simple clone of any child Lambdas */
        if ((member->Tag == TYLAMBDA) && 
			(member->u.Lambda->PersistantVariables != NIL) &&
			(member->u.Lambda->PersistantVariables == self->PersistantVariables))
           {
           atHMBind(Pv->itsDictionaryArray,indexOf).Value 
						= TLambda_Clone(gCP,gTP,*member,TOBJ(theCopy->PersistantVariables),TOBJ(theCopy->ConstantVariables));
           }

		/* Perform a deep copy of any friend Lambdas */
        if ((member->Tag == TYLAMBDA) && 
			(member->u.Lambda->ConstantVariables != NIL) &&
			(member->u.Lambda->ConstantVariables == self->PersistantVariables))
           {
           atHMBind(Pv->itsDictionaryArray,indexOf).Value 
						= TLambda_FriendCopy(gCP,gTP,*member,TOBJ(theCopy->PersistantVariables));
           }
        }
    }

/*	Return the cloned Lambda. */
FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FriendCopy

Make a copy of a friend Lambda. This function calls clone on
all the child Lambdas in the parent Lambdas persistent variables
structure. Each child Lambda is cloned with a copy of the new
parent Lambdas new persistent variables structure.

#endif

TVAL    TLambda_FriendCopy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL parentClassVariables)
{
NUM             indexOf;
StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TLambda,theCopy);
DeclareOBJ(TStructure,Pv);
DeclareTVAL(member);
DeclareTVAL(copyTval);
EndFrame

self = (TLambda*)asObject(&selfTval);

/*	Create a new outter structure for the copied friend Lambda. */
theCopy = TLambda_New(gCP,gTP);
copyTval->u.Object = (TObject *)theCopy;
copyTval->Tag = copyTval->u.Object->itsObjectType;

/*  Copy the most members of the original structure */
/*  to the copied Lambda, but make a copy of the Lambda */
/*  persistent variable structure. */
theCopy->ClassVariables = self->ClassVariables;
theCopy->ArgumentVariables = self->ArgumentVariables;
theCopy->TemporaryVariables = self->TemporaryVariables;
theCopy->ConstantVariables = NIL;
theCopy->PersistantVariables = NIL;
theCopy->PcodeVector = self->PcodeVector;
theCopy->DebuggerSource = self->DebuggerSource;
theCopy->VirtualMachine = self->VirtualMachine;
theCopy->NativeCodeVector = self->NativeCodeVector;
theCopy->Interfaces = self->Interfaces;
theCopy->EvalWhenDoomed = self->EvalWhenDoomed;

/* Make a copy of the interfaces belonging to this parent Lambda. */
if (self->Interfaces != NIL)
    {
    theCopy->Interfaces = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->Interfaces));
    }

/* Use the persistant  constant variables belonging to the parent Lambda. */
if (parentClassVariables.Tag == TYSTRUCTURE)
	{
	theCopy->ConstantVariables = parentClassVariables.u.Structure;
	}
else
	{
	theCopy->ConstantVariables = NIL;
	}

/* Make clones of all child and friend Lambdas belonging to this parent Lambda. */
/* Note: Friend Lambdas require a deep recursive copy, while child Lambdas can be cloned. */
if (self->PersistantVariables != NIL)
    {
    // theCopy->PersistantVariables.u.Object = (TObject*)Pv = TStructure_Copy(gCP,gTP,self->PersistantVariables);
	Pv = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->PersistantVariables));
	theCopy->PersistantVariables = (TStructure*)Pv;
    /* Reset the nested invocation count */
    /* for the new persistent variables. */
    Pv->itsCdr = gCP->Tval_VOID;

    for(indexOf = 0; indexOf <  Pv->itsMaxItemIndex; ++indexOf)
        {
        *member = atHMBind(Pv->itsDictionaryArray,indexOf).Value;
		/* Perform a simple clone of any child Lambdas */
        if ((member->Tag == TYLAMBDA) && 
			(member->u.Lambda->PersistantVariables != NIL) &&
			(member->u.Lambda->PersistantVariables == self->PersistantVariables))
           {
           atHMBind(Pv->itsDictionaryArray,indexOf).Value 
						= TLambda_Clone(gCP,gTP,*member,TOBJ(theCopy->PersistantVariables),TOBJ(theCopy->ConstantVariables));
           }

		/* Perform a deep copy of any friend Lambdas */
        if ((member->Tag == TYLAMBDA) && 
			(member->u.Lambda->ConstantVariables != NIL) &&
			(member->u.Lambda->ConstantVariables == self->PersistantVariables))
           {
           atHMBind(Pv->itsDictionaryArray,indexOf).Value 
						= TLambda_Clone(gCP,gTP,*member,TOBJ(theCopy->PersistantVariables),TOBJ(theCopy->ConstantVariables));
           }
        }
    }


/*	Return the copied friend Lambda. */
FrameExit(*copyTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FriendNew

Make a copy of a friend Lambda. This function calls clone on
all the child Lambdas in the parent Lambdas persistent variables
structure. Each child Lambda is cloned with a copy of the new
parent Lambdas new persistent variables structure.

#endif

TVAL    TLambda_FriendNew(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL parentClassVariables, NUM argc, TVAL argv[])
{
NUM             indexOf;
StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TLambda,theCopy);
DeclareOBJ(TStructure,Pv);
DeclareTVAL(member);
DeclareTVAL(copyTval);
EndFrame

self = (TLambda*)asObject(&selfTval);

/*	Create a new outter structure for the copied friend Lambda. */
theCopy = TLambda_New(gCP,gTP);
copyTval->u.Object = (TObject *)theCopy;
copyTval->Tag = copyTval->u.Object->itsObjectType;

/*  Copy the most members of the original structure */
/*  to the copied Lambda, but make a copy of the Lambda */
/*  persistent variable structure. */
theCopy->ClassVariables = self->ClassVariables;
theCopy->ArgumentVariables = self->ArgumentVariables;
theCopy->TemporaryVariables = self->TemporaryVariables;
theCopy->ConstantVariables = NIL;
theCopy->PersistantVariables = NIL;
theCopy->PcodeVector = self->PcodeVector;
theCopy->DebuggerSource = self->DebuggerSource;
theCopy->VirtualMachine = self->VirtualMachine;
theCopy->NativeCodeVector = self->NativeCodeVector;
theCopy->Interfaces = self->Interfaces;
theCopy->EvalWhenDoomed = self->EvalWhenDoomed;

/* Make a copy of the interfaces belonging to this parent Lambda. */
if (self->Interfaces != NIL)
    {
    theCopy->Interfaces = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->Interfaces));
    }

/* Use the persistant  constant variables belonging to the parent Lambda. */
theCopy->ConstantVariables = (parentClassVariables.Tag != TYSTRUCTURE) ? NIL : parentClassVariables.u.Structure;

/* Make clones of all child and friend Lambdas belonging to this parent Lambda. */
/* Note: Friend Lambdas require a deep recursive copy, while child Lambdas can be cloned. */
if (self->PersistantVariables != NIL)
    {
    //theCopy->PersistantVariables.u.Object = (TObject*)Pv = TStructure_Copy(gCP,gTP,self->PersistantVariables);
	Pv = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->PersistantVariables));
	theCopy->PersistantVariables = (TStructure*)Pv;
    /* Reset the nested invocation count */
    /* for the new persistent variables. */
    Pv->itsCdr = gCP->Tval_VOID;

    for(indexOf = 0; indexOf <  Pv->itsMaxItemIndex; ++indexOf)
        {
        *member = atHMBind(Pv->itsDictionaryArray,indexOf).Value;
		/* Perform a simple clone of any child Lambdas */
        if ((member->Tag == TYLAMBDA) && 
			(member->u.Lambda->PersistantVariables != NIL) &&
			(member->u.Lambda->PersistantVariables == self->PersistantVariables))
           {
           atHMBind(Pv->itsDictionaryArray,indexOf).Value 
						= TLambda_Clone(gCP,gTP,*member,TOBJ(theCopy->PersistantVariables),TOBJ(theCopy->ConstantVariables));
           }

		/* Perform a deep copy of any friend Lambdas */
        if ((member->Tag == TYLAMBDA) && 
			(member->u.Lambda->ConstantVariables != NIL) &&
			(member->u.Lambda->ConstantVariables == self->PersistantVariables))
           {
           atHMBind(Pv->itsDictionaryArray,indexOf).Value 
						= TLambda_Clone(gCP,gTP,*member,TOBJ(theCopy->PersistantVariables),TOBJ(theCopy->ConstantVariables));
           }
        }
    }

/*	Invoke the new member function from the Lambda (if any). */
/*	Reference the Permanent variables structure of the Lambda. */
*member = TLambda_GetPvMember(gCP,gTP,*copyTval,"new");
if (member->Tag == TYLAMBDA)
    {
    /* Invoke the new subfunction of the Lambda. */
    FSmartbase_Evalv(gCP,gTP,*member,argc,argv);
    }

/*	Return the copied friend Lambda. */
FrameExit(*copyTval);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Clone

Make a clone of a child Lambda. This function is called by Copy on
all the child Lambdas in the parent Lambdas persistent variables
structure. Each child Lambda is cloned with a copy of the new
parent Lambdas new persistent variables structure.

#endif

TVAL    TLambda_Clone(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL Pv,TVAL Cv)
{
TLambda*     self = (TLambda*)asObject(&selfTval);
StartFrame
DeclareOBJ(TLambda,theCopy);
EndFrame

Cv = Cv; // NOOP to hide unused parameter warning message
/*	Create a new outter structure for the cloned Lambda. */
theCopy = TLambda_New(gCP,gTP);

/*  Copy the most members of the original structure */
/*  to the cloned Lambda, but substitute the specified */
/*  Lambda persistent variable structure. */
theCopy->ClassVariables = self->ClassVariables;
theCopy->ArgumentVariables = self->ArgumentVariables;
theCopy->TemporaryVariables = self->TemporaryVariables;
theCopy->PersistantVariables = (Pv.Tag != TYSTRUCTURE) ? NIL : Pv.u.Structure;
theCopy->ConstantVariables = self->ConstantVariables;
theCopy->RegisterVariables = self->RegisterVariables;
theCopy->PcodeVector = self->PcodeVector;
theCopy->DebuggerSource = self->DebuggerSource;
theCopy->VirtualMachine = self->VirtualMachine;
theCopy->NativeCodeVector = self->NativeCodeVector;
theCopy->Interfaces = self->Interfaces;
theCopy->EvalWhenDoomed = self->EvalWhenDoomed;

/* Make a copy of the interfaces belonging to this parent Lambda. */
if (self->Interfaces != NIL)
    {
    theCopy->Interfaces = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->Interfaces));
    }

/*	Return the cloned Lambda. */
FrameExit(TOBJ(theCopy));
}

/*--------------------------------------------------------------------------------------- */

#if 0
GetIV3

Return the indexed value from the structure portion of the Lambda object.

#endif

TVAL TLambda_GetIV3(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2, TVAL index3)
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,5);

EndFrame

/* Invoke the ref member function from the Lambda (if any). */
/* Reference the Permanent variables structure of the Lambda. */
*indexProc = TLambda_GetPvMember(gCP,gTP,selfTval,"ref3");
ExitOnError(*indexProc);
/* Invoke the ref3 subfunction of the Lambda. */
prmv[0] = index1;
prmv[1] = index2;
prmv[2] = index3;
*ret = FSmartbase_Evalv(gCP,gTP,*indexProc,3,&prmv[0]);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
SetIV2

Set the indexed value in the structure portion of the Lambda object.

#endif

TVAL TLambda_SetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2,TVAL newValue)
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,5);

EndFrame

/* Invoke the set member function from the Lambda (if any). */
/* Reference the Permanent variables structure of the Lambda. */
*indexProc = TLambda_GetPvMember(gCP,gTP,selfTval,"set2");
ExitOnError(*indexProc);
/* Invoke the set2 subfunction of the Lambda. */
prmv[0] = index1;
prmv[1] = index2;
prmv[2] = newValue;
*ret = FSmartbase_Evalv(gCP,gTP,*indexProc,3,&prmv[0]);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
SetIV3

Set the indexed value in the structure portion of the Lambda object.

#endif

TVAL TLambda_SetIV3(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2, TVAL index3,TVAL newValue)
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,5);

EndFrame

/* Invoke the set member function from the Lambda (if any). */
/* Reference the Permanent variables structure of the Lambda. */
*indexProc = TLambda_GetPvMember(gCP,gTP,selfTval,"set3");
ExitOnError(*indexProc);
/* Invoke the set3 subfunction of the Lambda. */
prmv[0] = index1;
prmv[1] = index2;
prmv[2] = index3;
prmv[3] = newValue;
*ret = FSmartbase_Evalv(gCP,gTP,*indexProc,4,&prmv[0]);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Decode

The Decode method decodes the specified Smartbase Lambda object 
and returns a source string.

#endif

TVAL TLambda_Decode(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpCHAR theSource, BOLE relativeCellRefs)
{
TLambda*         self = (TLambda*)asObject(&selfTval);
CHAR                buf[1024];
NUM                 ip;
StartFrame
DeclareOBJ(TVector,srcVector);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(tmp);
DeclareTVALArray(prmv,3);

EndFrame
 
relativeCellRefs = relativeCellRefs; // NOOP to hide unused parameter warning message
/* Initialization */

theSource[0] = 0;   
ip = 0;

/* Decode each Smartbase tval in the pcode vector. */

if (_CHECKOBJ(self->DebuggerSource) && (self->DebuggerSource->itsObjectType == TYVECTOR))
    {
    srcVector = (TVector*)self->DebuggerSource;
    for(ip = 0; ip < srcVector->itsMaxItemIndex; ip++)
        {
        *tmp = atHMTval(srcVector->itsTvalArray,ip);
        *tmp = prmv[2];
        
        TObject_CnvToText(gCP,gTP,(LpCHAR)buf,sizeof(buf) -1,*tmp);
        
        strcat((char*)theSource, (const char*)buf);
        }
    }
else
/*  If there is no source available, then say so */
    {
    strcpy(theSource,"no source available");
    }

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */

#if 0
GetCvMember

Get the member value in the Cv structure portion of the Lambda object.

#endif

TVAL TLambda_GetCvMember(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpCHAR name)
{
NUM                 indexOf;
StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TStructure,pv);
DeclareOBJ(TSymbol,member);
EndFrame
 
self = (TLambda*)asObject(&selfTval);

/*  Set the Cv member value of this Lambda object. */
if (self->ConstantVariables != NIL)
    {
    /* Address the Lambdas  constant variables structure. */
    pv = self->ConstantVariables;

    /*  Locate the member variable in the Cv structure. */
    for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
        {
        member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
        if (strcmp((LpCHAR)*member->itsCString,name) == 0)
           {
           FrameExit(atHMBind(pv->itsDictionaryArray,indexOf).Value);
           }
        }
    }
        
FrameExit(gCP->Tval_VOID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
GetCvSymbol

Get the member symbol value in the Cv structure portion of the Lambda object.

#endif

TVAL TLambda_GetCvSymbol(LpXCONTEXT gCP,LpTHREAD gTP, TVAL selfTval, TVAL name)
{
NUM                 indexOf;
StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TStructure,pv);
DeclareOBJ(TSymbol,member);
DeclareTVAL(ret);
EndFrame

self = (TLambda*)asObject(&selfTval);

/*  Set the Cv member value of this Lambda object. */
if ((self->ConstantVariables != NIL) && ((name.Tag == TYSYMBOL) || (name.Tag == TYQUOTEDSYMBOL)))
    {
    /* Address the Lambdas persistant variables structure. */
    pv = self->ConstantVariables;

    /*  Locate the member variable in the Cv structure. */
    for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
        {
        member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
        if (member == Symbol(name))
           {
           FrameExit(atHMBind(pv->itsDictionaryArray,indexOf).Value);
           }
        }
    }

*ret = FDebug_GetLambdaBind(gCP,gTP,1,&selfTval);
if (ret->Tag == TYVOID) *ret = selfTval;
*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,
							"!Cannot find name: %a in the CV structure of Lambda %a!",
							&name,  
							ret);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
GetPvMember

Get the member value in the Pv structure portion of the Lambda object.

#endif

TVAL TLambda_GetPvMember(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpCHAR name)
{
NUM                 indexOf;
StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TStructure,pv);
DeclareOBJ(TSymbol,member);
EndFrame
 
self = (TLambda*)asObject(&selfTval);

/*  Set the Pv member value of this Lambda object. */
if (self->PersistantVariables != NIL)
    {
    /* Address the Lambdas Persistant variables structure. */
    pv = self->PersistantVariables;

    /*  Locate the member variable in the Pv structure. */
    for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
        {
        member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
        if (strcmp((LpCHAR)*member->itsCString,name) == 0)
           {
           FrameExit(atHMBind(pv->itsDictionaryArray,indexOf).Value);
           }
        }
    }
        
FrameExit(gCP->Tval_VOID);
}

/*--------------------------------------------------------------------------------------- */

#if 0
GetPvSymbol

Get the member symbol value in the Pv structure portion of the Lambda object.

#endif

TVAL TLambda_GetPvSymbol(LpXCONTEXT gCP,LpTHREAD gTP, TVAL selfTval, TVAL name)
{
NUM                 indexOf;
StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TStructure,pv);
DeclareOBJ(TSymbol,member);
DeclareTVAL(ret);
EndFrame

self = (TLambda*)asObject(&selfTval);

/*  Set the Pv member value of this Lambda object. */
if ((self->PersistantVariables != NIL) && ((name.Tag == TYSYMBOL) || (name.Tag == TYQUOTEDSYMBOL)))
    {
    /* Address the Lambdas persistant variables structure. */
    pv = self->PersistantVariables;

    /*  Locate the member variable in the Pv structure. */
    for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
        {
        member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
        if (member == Symbol(name))
           {
           FrameExit(atHMBind(pv->itsDictionaryArray,indexOf).Value);
           }
        }
    }

*ret = FDebug_GetLambdaBind(gCP,gTP,1,&selfTval);
if (ret->Tag == TYVOID) *ret = selfTval;
*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,
							"!Cannot find name: %a in the PV structure of Lambda %a!",
							&name,  
							ret);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
SetPvMember

Set the member value in the Pv structure portion of the Lambda object.

#endif

TVAL TLambda_SetPvMember(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, LpCHAR name, TVAL newValue)
{
NUM                 indexOf;
StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TStructure,pv);
DeclareOBJ(TSymbol,member);
DeclareTVAL(errorTval);
EndFrame

self = (TLambda*)asObject(&selfTval);

/*  Set the Pv member value of this Lambda object. */
if (self->PersistantVariables != NIL)
    {
    /* Address the Lambdas persistant variables structure. */
    pv = self->PersistantVariables;

    /*  Locate the member variable in the Pv structure. */
    for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
        {
        member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
        if (strcmp((LpCHAR)*member->itsCString,name) == 0)
           {
           atHMBind(pv->itsDictionaryArray,indexOf).Value = newValue;
           FrameExit(newValue);
           }
        }
    }

/* If we get here, the member was not found. */
*errorTval = FDebug_GetLambdaBind(gCP,gTP,1,&selfTval);
if (errorTval->Tag == TYVOID) *errorTval = selfTval;
*errorTval = TError_sprintf(gCP, gTP, gTP->TempBuffer,
							      "!Cannot find name: %a in the PV structure of Lambda %a!",
							      &name,  
							      errorTval);
FrameExit(*errorTval);
}

/*--------------------------------------------------------------------------------------- */

#if 0
SetPvSymbol

Set the member symbol value in the Pv structure portion of the Lambda object.

#endif

TVAL TLambda_SetPvSymbol(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL name, TVAL newValue)
{
NUM                 indexOf;

StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TStructure,pv);
DeclareOBJ(TSymbol,member);
DeclareTVAL(errorTval);
EndFrame

self = (TLambda*)asObject(&selfTval);

/*  Set the Pv member value of this Lambda object. */
if ((self->PersistantVariables != NIL) && ((name.Tag == TYSYMBOL) || (name.Tag == TYQUOTEDSYMBOL)))
    {
    /* Address the Lambdas persistant variables structure. */
    pv = self->PersistantVariables;

    /*  Locate the member variable in the Pv structure. */
    for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
        {
        member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
        if (member == Symbol(name))
           {
           atHMBind(pv->itsDictionaryArray,indexOf).Value = newValue;
           FrameExit(newValue);
           }
        }
    }
        
/* If we get here, the member was not found. */
*errorTval = FDebug_GetLambdaBind(gCP,gTP,1,&selfTval);
if (errorTval->Tag == TYVOID) *errorTval = selfTval;
*errorTval = TError_sprintf(gCP, gTP, gTP->TempBuffer,
								  "!Cannot find name: %a in the PV structure of Lambda %a!",
								  &name,  
								  errorTval);
FrameExit(*errorTval);
}



/*--------------------------------------------------------------------------------------- */

#if 0
SetPvCvSymbol

Set the member symbol value in the Pv/Cv structure portion of the Lambda object.

#endif

TVAL TLambda_SetPvCvSymbol(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL name, TVAL newValue)
{
NUM                 indexOf;

StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TStructure,pv);
DeclareOBJ(TSymbol,member);
DeclareTVAL(errorTval);
EndFrame

self = (TLambda*)asObject(&selfTval);

/*  Set the Pv member value of this Lambda object. */
if ((self->PersistantVariables != NIL) && ((name.Tag == TYSYMBOL) || (name.Tag == TYQUOTEDSYMBOL)))
    {
    /* Address the Lambdas persistant variables structure. */
    pv = self->PersistantVariables;

    /*  Locate the member variable in the Pv structure. */
    for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
        {
        member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
        if (member == Symbol(name))
           {
           atHMBind(pv->itsDictionaryArray,indexOf).Value = newValue;
           FrameExit(newValue);
           }
        }
    }
        
/*  Set the Cv member value of this Lambda object. */
if ((self->ConstantVariables != NIL) && ((name.Tag == TYSYMBOL) || (name.Tag == TYQUOTEDSYMBOL)))
    {
    /* Address the Lambdas  constant variables structure. */
    pv = self->ConstantVariables;

    /*  Locate the member variable in the Cv structure. */
    for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
        {
        member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
        if (member == Symbol(name))
           {
           atHMBind(pv->itsDictionaryArray,indexOf).Value = newValue;
           FrameExit(newValue);
           }
        }
    }
        
/* If we get here, the member was not found. */
*errorTval = FDebug_GetLambdaBind(gCP,gTP,1,&selfTval);
if (errorTval->Tag == TYVOID) *errorTval = selfTval;
*errorTval = TError_sprintf(gCP, gTP, gTP->TempBuffer,
								  "!Cannot find name: %a in the PV/CV structure of Lambda %a!",
								  &name,  
								  errorTval);
FrameExit(*errorTval);
}


/*--------------------------------------------------------------------------------------- */

#if 0
GetPvCvSymbol

Get the member symbol value in the Pv or Cv structure portion of the Lambda object.

Note: For class Lambdas, also check the In.macros and the In.methods structures.

#endif

TVAL TLambda_GetPvCvSymbol(LpXCONTEXT gCP,LpTHREAD gTP, TVAL selfTval, TVAL name)
{
NUM                 indexOf;
StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TStructure,pv);
DeclareOBJ(TSymbol,member);
DeclareTVAL(ret);
DeclareTVAL(methods);
DeclareTVAL(macros);
DeclareTVAL(classSW);
EndFrame

self = (TLambda*)asObject(&selfTval);

/*  Search the Pv member value of this Lambda object. */
if ((self->PersistantVariables != NIL) && ((name.Tag == TYSYMBOL) || (name.Tag == TYQUOTEDSYMBOL)))
    {
    /* Address the Lambdas persistant variables structure. */
    pv = self->PersistantVariables;

    /*  Locate the member variable in the Pv structure. */
    for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
        {
        member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
        if (member == Symbol(name))
           {
           FrameExit(atHMBind(pv->itsDictionaryArray,indexOf).Value);
           }
        }
    }

/*  Search the Cv member value of this Lambda object. */
if ((self->ConstantVariables != NIL) && ((name.Tag == TYSYMBOL) || (name.Tag == TYQUOTEDSYMBOL)))
    {
    /* Address the Lambdas  constant variables structure. */
    pv = self->ConstantVariables;

    /*  Locate the member variable in the Cv structure. */
    for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
        {
        member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
        if (member == Symbol(name))
           {
           FrameExit(atHMBind(pv->itsDictionaryArray,indexOf).Value);
           }
        }
    }

/*  Search the In entries of this Lambda object. */
if ((self->Interfaces != NIL) && ((name.Tag == TYSYMBOL) || (name.Tag == TYQUOTEDSYMBOL)))
    {
	/* Check to see if this is a class Lambda. */
	*classSW = TStructure_GetIV1(gCP,gTP,TOBJ(self->Interfaces),TSYMBOL("class"));
	ExitOnError(*classSW);
	if ((classSW->Tag != TYBOLE) || ((classSW->u.Bool != TRUE))) goto NotFound;

	/* Check to see if the name can be found in the class methods. */
	*methods = TStructure_GetIV1(gCP,gTP,TOBJ(self->Interfaces),TSYMBOL("methods"));
	ExitOnError(*methods);
	if (methods->Tag == TYSTRUCTURE)
		{
		/* Address the Lambdas class methods structure. */
		pv = Structure(*methods);

		/*  Locate the member variable in the class methods structure. */
		for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
			{
			member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
			if (member == Symbol(name))
				{
				FrameExit(atHMBind(pv->itsDictionaryArray,indexOf).Value);
				}
			}
		}

	/* Check to see if the name can be found in the class macros. */
	*macros = TStructure_GetIV1(gCP,gTP,TOBJ(self->Interfaces),TSYMBOL("macros"));
	ExitOnError(*macros);
	if (macros->Tag == TYSTRUCTURE)
		{
		/* Address the Lambdas class macros structure. */
		pv = Structure(*macros);

		/*  Locate the member variable in the class methods structure. */
		for (indexOf = 0; indexOf < pv->itsMaxItemIndex; ++indexOf)
			{
			member = (TSymbol*)atHMBind(pv->itsDictionaryArray,indexOf).Key;
			if (member == Symbol(name))
				{
				FrameExit(atHMBind(pv->itsDictionaryArray,indexOf).Value);
				}
			}
		}
    }


	
NotFound:
*ret = FDebug_GetLambdaBind(gCP,gTP,1,&selfTval);
if (ret->Tag == TYVOID) *ret = selfTval;
*ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,
							"!Cannot find name: %a in the PV/CV structures of Lambda %a!",
							&name,  
							ret);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TLambda_New

Create a new TLambda.

#endif

TLambda* TLambda_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TLambda,self);
DeclareTVAL(ret);
EndFrame

/*  This class must be initialized. */
if (!gCP->TLambda_Initialized) TLambda_Init(gCP, gTP);

self = (TLambda*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYLAMBDA;
self->VirtualMachine = gCP->TLambda_LispVirtualMachine;
self->EvalWhenDoomed = FALSE; /* Do NOT evaluate me when I am doomed */
self->itsMaxItemIndex = 0;
self->itsNilArray = (HMChar)&self->itsImmediatePtr;
self->itsImmediatePtr = NULL;
self->RegisterVariables = ((*ret=FCompile_SetupRegs(gCP,gTP,self)).Tag != TYSTRUCTURE) ? NIL : ret->u.Structure;
self->TemporaryVariables = ((*ret=FCompile_SetupVars(gCP,gTP,self)).Tag != TYSTRUCTURE) ? NIL : ret->u.Structure;
self->InUse = 0;


FrameExit(self);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TLambda_LambdaVMEval

The main Lambda Virtual Machine evaluator for Lambda objects which act as the virtual machine
for another Lambda object.

#endif


TVAL TLambda_LambdaVMEval(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* proc,NUM argc,TVAL argv[])
{
NUM		n;
TVAL	parms[21];
TVAL	retValue;

/* Create the new pass through argument list */
if (argc > 20) return(gCP->TObject_ERROR_INVALID_ARGLIST);

for (n = 0; n < argc; ++n) parms[n] = argv[n];
parms[n].u.Lambda = proc;
parms[n].Tag = TYLAMBDA;

retValue = _VmEvaluate(proc->VirtualMachine->itsVMEmulator,argc+1,parms);
return(retValue);

}

/*--------------------------------------------------------------------------------------- */
#if 0
NewCopy

Make a new copy of a parent Lambda. This function calls clone on
all the child Lambdas in the parent Lambdas persistent variables
structure. Each child Lambda is cloned with a copy of the new
parent Lambdas new persistent variables structure.

#endif

TVAL    TLambda_NewCopy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,TVAL argv[])
{
NUM             indexOf;
NUM             maxArgs = 41;
StartFrame
DeclareOBJ(TLambda,self);
DeclareOBJ(TLambda,theCopy);
DeclareOBJ(TStructure,Pv);
DeclareTVAL(member);
DeclareTVAL(copyTval);
DeclareTVAL(ec);
DeclareTVAL(methods);
DeclareTVAL(classSW);
DeclareTVAL(newObject);
DeclareTVALArray(prmv,40);

EndFrame

self = (TLambda*)asObject(&selfTval);

/*  Search the In entries of this Lambda object. */
if ((self->Interfaces != NIL) && (self->ClassVariables != NIL) && (self->ClassVariables->itsMaxItemIndex > 0))
    {
	/* Check to see if this is a class Lambda. */
	*classSW = TStructure_GetIV1(gCP,gTP,TOBJ(self->Interfaces),TSYMBOL("class"));
	ExitOnError(*classSW);
	if (classSW->Tag == TYVOID) goto CopyOfLambda;

	/* Copy the class variable Structure of this class Lambda. */
	*newObject = TOBJ(self->ClassVariables);
	*newObject = FProperty_Factory(gCP,gTP,1,newObject);
	ExitOnError(*newObject);
	newObject->u.Structure->itsCdr.u.Lambda = self;
	newObject->u.Structure->itsCdr.Tag = TYLAMBDA;

	/* Check to see if the name can be found in the class methods. */
	*methods = TStructure_GetIV1(gCP,gTP,TOBJ(self->Interfaces),TSYMBOL("methods"));
	ExitOnError(*methods);
	if (methods->Tag == TYSTRUCTURE)
		{
		/* Address the Lambdas class methods structure. */
		newObject->u.Structure->itsMethods = (TSymbol*)methods->u.Structure;
		}

	/* Invoke the new member function from the Lambda (if any). */
	/* Reference the Permanent variables structure of the Lambda. */
	*member = TLambda_GetPvMember(gCP,gTP,selfTval,"new");
	if (member->Tag == TYLAMBDA)
		{
		argc = min(argc,maxArgs);
		for (indexOf = 1; indexOf <= argc; ++indexOf) {prmv[indexOf] = argv[indexOf-1];}
		prmv[0] = *newObject;
		/* Invoke the new subfunction of the Lambda. */
		*ec = FSmartbase_Evalv(gCP,gTP,*member,argc+1,&prmv[0]);
		ExitOnError(*ec);
		}
		
	FrameExit(*newObject);
    }

/*	Create a new outter structure for the copied parent Lambda. */
CopyOfLambda:
theCopy = TLambda_New(gCP,gTP);
copyTval->u.Object = (TObject *)theCopy;
copyTval->Tag = copyTval->u.Object->itsObjectType;

/*  Copy the most members of the original structure */
/*  to the copied Lambda, but make a copy of the Lambda */
/*  persistent variable structure. */
theCopy->ClassVariables = self->ClassVariables;
theCopy->ArgumentVariables = self->ArgumentVariables;
theCopy->TemporaryVariables = self->TemporaryVariables;
theCopy->ConstantVariables = NIL;
theCopy->PersistantVariables = NIL;
theCopy->PcodeVector = self->PcodeVector;
theCopy->DebuggerSource = self->DebuggerSource;
theCopy->VirtualMachine = self->VirtualMachine;
theCopy->NativeCodeVector = self->NativeCodeVector;
theCopy->Interfaces = self->Interfaces;
theCopy->EvalWhenDoomed = self->EvalWhenDoomed;

/* Make a copy of the interfaces belonging to this parent Lambda. */
if (self->Interfaces != NIL)
    {
    theCopy->Interfaces = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->Interfaces));
    }

/* Make a copy of the persistant  constant variables belonging to this parent Lambda. */
if (self->ConstantVariables != NIL)
    {
    theCopy->ConstantVariables = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->ConstantVariables));
    }

/* Make a copy of the persistant constant variables belonging to this parent Lambda. */
if (self->RegisterVariables != NIL)
    {
    theCopy->RegisterVariables = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->RegisterVariables));
    }

/* Make clones of all child and friend Lambdas belonging to this parent Lambda. */
/* Note: Friend Lambdas require a deep recursive copy, while child Lambdas can be cloned. */
if (self->PersistantVariables != NIL)
    {
    //theCopy->PersistantVariables.u.Object = (TObject*)Pv = TStructure_Copy(gCP,gTP,self->PersistantVariables);
	Pv = (TStructure*)TStructure_Copy(gCP,gTP,TOBJ(self->PersistantVariables));
    theCopy->PersistantVariables = (TStructure*)Pv;
    /* Reset the nested invocation count */
    /* for the new persistent variables. */
    Pv->itsCdr = gCP->Tval_VOID;

    for(indexOf = 0; indexOf <  Pv->itsMaxItemIndex; ++indexOf)
        {
        *member = atHMBind(Pv->itsDictionaryArray,indexOf).Value;
		/* Perform a simple clone of any child Lambdas */
        if ((member->Tag == TYLAMBDA) && 
			(member->u.Lambda->PersistantVariables != NIL) &&
			(member->u.Lambda->PersistantVariables == self->PersistantVariables))
           {
           atHMBind(Pv->itsDictionaryArray,indexOf).Value = TLambda_Clone(gCP,gTP,*member,TOBJ(theCopy->PersistantVariables),TOBJ(theCopy->ConstantVariables));
           }

		/* Perform a deep copy of any friend Lambdas */
        if ((member->Tag == TYLAMBDA) && 
			(member->u.Lambda->ConstantVariables != NIL) &&
			(member->u.Lambda->ConstantVariables == self->PersistantVariables))
           {
           atHMBind(Pv->itsDictionaryArray,indexOf).Value = TLambda_FriendNew(gCP,gTP,*member,TOBJ(theCopy->PersistantVariables),argc,argv);
           }
        }
    }

/* Invoke the new member function from the Lambda (if any). */
/* Reference the Permanent variables structure of the Lambda. */
*member = TLambda_GetPvMember(gCP,gTP,*copyTval,"new");
if (member->Tag == TYLAMBDA)
    {
    /* Invoke the new subfunction of the Lambda. */
    *ec = FSmartbase_Evalv(gCP,gTP,*member,argc,argv);
	ExitOnError(*ec);
    }

/*	Return the cloned Lambda. */
FrameExit(*copyTval);
}

/*--------------------------------------------------------------------------------------- */

#if 0
GetIV1

Return the indexed value from the structure portion of the Lambda object.

Note:   All TObject children have a repeating portion. It may be nil
        as in the case of the parent, but the methods to support a
        repeating portion are inherited from the parent.
 
#endif

TVAL TLambda_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1)
{
TLambda*         self = (TLambda*)asObject(&selfTval);
StartFrame
DeclareTVAL(ret);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,5);

EndFrame

/*  Get the fields of this Lambda object. */
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
    {
    /*  Are we retrieving the Argument variables: Sv? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Sv)
        {
        FrameExit(TOBJ(self->ClassVariables));    
        }
    else
    /*  Are we retrieving the Argument variables: Av? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Av)
        {
        FrameExit(TOBJ(self->ArgumentVariables));    
        }
    else
    /*  Are we retrieving the Temporary variables: Tv? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Tv)
        {
        FrameExit(TOBJ(self->TemporaryVariables));   
        }
    else
    /*  Are we retrieving the evaluate when doomed switch: EvalWhenDoomed? */
    if (index1.u.Object == (TObject*)gCP->TLambda_EvalWhenDoomed)
        {
        FrameExit(TBOOL(self->EvalWhenDoomed));   
        }
    else
    /*  Are we retrieving the Persistent variables: Pv? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Pv)
        {
        FrameExit(TOBJ(self->PersistantVariables));  
        }
    else
    /*  Are we retrieving the Persistent variables: Cv? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Cv)
        {
        FrameExit(TOBJ(self->ConstantVariables));  
        }
    else
    /*  Are we retrieving the Persistent variables: Rv? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Rv)
        {
        FrameExit(TOBJ(self->RegisterVariables));  
        }
    else
    /*  Are we retrieving the Pcode vector: Pc? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Pc)
        {
        FrameExit(TOBJ(self->PcodeVector));  
        }
    else
    /*  Are we retrieving the lexical token vector: Sc? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Sc)
        {
        FrameExit(TOBJ(self->DebuggerSource));    
        }
    else
    /*  Are we retrieving the native code vector: Nc? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Nc)
        {
        FrameExit(TOBJ(self->NativeCodeVector));    
        }
    else
    /*  Are we retrieving the interfaces structure: In? */
    if (index1.u.Object == (TObject*)gCP->TLambda_In)
        {
        FrameExit(TOBJ(self->Interfaces));    
        }
    else
    /*  Are we retrieving the virtual machine evaluator: Vm? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Vm)
        {
		ret->u.Symbol = self->VirtualMachine;
		ret->Tag = TYVMEVALUATOR;
        FrameExit(*ret);    
        }
    else
        {
        /* Reference the Permanent variables structure of the Lambda. */
        /* If a "ref1" child Lambda exists, then invoke it with the index; */
        /* otherwise, reference the symbol as a persistent variable. */
        *indexProc = TLambda_GetPvMember(gCP,gTP,selfTval,"ref1");
        if (indexProc->Tag == TYLAMBDA)
            {
            /* Invoke the ref1 member function from the Lambda (if any). */
            prmv[0] = index1;
            *ret = FSmartbase_Evalv(gCP,gTP,*indexProc,1,&prmv[0]);
            }
        else
            {
            /* Reference the specified Pv variable of the Lambda (if any). */
            *ret = TLambda_GetPvCvSymbol(gCP,gTP,selfTval,index1);
            }

        FrameExit(*ret);
        }
    }

/* Manage the case where the index is not a symbol. */
/* Invoke the ref1 member function from the Lambda (if any). */
/* Reference the Permanent variables structure of the Lambda. */
*indexProc = TLambda_GetPvMember(gCP,gTP,selfTval,"ref1");
if (indexProc->Tag == TYLAMBDA)
    {
    /* Invoke the ref1 subfunction of the Lambda. */
    prmv[0] = index1;
    *ret = FSmartbase_Evalv(gCP,gTP,*indexProc,1,&prmv[0]);
    }
else
    {
	*ret = FDebug_GetLambdaBind(gCP,gTP,1,&selfTval);
    if (ret->Tag == TYVOID) *ret = selfTval;
    *ret = TError_sprintf(gCP, gTP, gTP->TempBuffer,
								"!Cannot reference index: %a in the PV structure of Lambda %a!", 
								&index1,  
								ret);
    }
 
/* Return the result value. */
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
GetIV2

Return the indexed value from the structure portion of the Lambda object.

#endif

TVAL TLambda_GetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1, TVAL index2)
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,5);

EndFrame

/* Invoke the ref member function from the Lambda (if any). */
/* Reference the Permanent variables structure of the Lambda. */
*indexProc = TLambda_GetPvMember(gCP,gTP,selfTval,"ref2");
ExitOnError(*indexProc);
/* Invoke the ref2 subfunction of the Lambda. */
prmv[0] = index1;
prmv[1] = index2;
*ret = FSmartbase_Evalv(gCP,gTP,*indexProc,2,&prmv[0]);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
SetIV1

Set the indexed value in the structure portion of the Lambda object.

Note:   All TObject children have a repeating portion. It may be nil
        as in the case of the parent, but the methods to support a
        repeating portion are inherited from the parent.
 
#endif

TVAL TLambda_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL index1,TVAL newValue)
{
StartFrame
DeclareOBJ(TLambda,self);
DeclareTVAL(ret);
DeclareTVAL(indexProc);
DeclareTVALArray(prmv,5);

EndFrame

self = (TLambda*)asObject(&selfTval);
/*  Set default return value to this Lambda object. */
*ret = selfTval;

/*  Set the fields of this Lambda object. */
if ((index1.Tag == TYSYMBOL) || (index1.Tag == TYQUOTEDSYMBOL))
    {
    /*  Are we setting the Argument variables: Sv? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Sv)
        {
		switch (newValue.Tag)
			{
			case TYVOID:
				self->ClassVariables = NIL; 
				break;

			case TYSTRUCTURE:
				self->ClassVariables = newValue.u.Structure; 
				break;

			default:
				*ret = TERROR("!Lambda.set: Class variables can only accept a Structure or void!");
			break;
			}
        FrameExit(*ret);
        }
    else
    /*  Are we setting the Argument variables: Av? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Av)
        {
		switch (newValue.Tag)
			{
			case TYVOID:
				self->ArgumentVariables = NIL; 
				break;

			case TYSTRUCTURE:
				self->ArgumentVariables = newValue.u.Structure; 
				break;

			default:
				*ret = TERROR("!Lambda.set: Argument variables can only accept a Structure or void!");
			break;
			}
        FrameExit(*ret);
        }
    else
    /*  Are we setting the Temporary variables: Tv? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Tv)
        {
		switch (newValue.Tag)
			{
			case TYVOID:
				self->TemporaryVariables = NIL; 
				break;

			case TYSTRUCTURE:
				self->TemporaryVariables = newValue.u.Structure; 
				break;

			default:
				*ret = TERROR("!Lambda.set: Temporary variables can only accept a Structure or void!");
			break;
			}
        FrameExit(*ret);
        }
    else
    /*  Are we setting the evaluate when doomed switch: EvalWhenDoomed? */
    if (index1.u.Object == (TObject*)gCP->TLambda_EvalWhenDoomed)
        {
        self->EvalWhenDoomed = newValue.u.Bool;    
        FrameExit(*ret);
        }
    else
    /*  Are we setting the Persistent variables: Pv? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Pv)
        {
		switch (newValue.Tag)
			{
			case TYVOID:
				self->PersistantVariables = NIL; 
				break;

			case TYSTRUCTURE:
				self->PersistantVariables = newValue.u.Structure; 
				break;

			default:
				*ret = TERROR("!Lambda.set: Persistant variables can only accept a Structure or void!");
			break;
			}
        FrameExit(*ret);
        }
    else
    /*  Are we setting the Persistent  constant variables: Cv? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Cv)
        {
		switch (newValue.Tag)
			{
			case TYVOID:
				self->ConstantVariables = NIL; 
				break;

			case TYSTRUCTURE:
				self->ConstantVariables = newValue.u.Structure; 
				break;

			default:
				*ret = TERROR("!Lambda.set: Constant variables can only accept a Structure or void!");
			break;
			}
        FrameExit(*ret);
        }
    else
    /*  Are we setting the Persistent  constant variables: Rv? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Rv)
        {
		switch (newValue.Tag)
			{
			case TYVOID:
				self->RegisterVariables = NIL; 
				break;

			case TYSTRUCTURE:
				self->RegisterVariables = newValue.u.Structure; 
				break;

			default:
				*ret = TERROR("!Lambda.set: Register variables can only accept a Structure or void!");
			break;
			}
        FrameExit(*ret);
        }
    else
    /*  Are we setting the Pcode vector: Pc? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Pc)
        {
		switch (newValue.Tag)
			{
			case TYVOID:
				self->PcodeVector = NIL; 
				break;

			case TYPCODEVECTOR:
				self->PcodeVector = newValue.u.PcodeVector; 
				break;

			default:
				*ret = TERROR("!Lambda.set: Pcode vector can only accept a PcodeVector or void!");
			break;
			}
        FrameExit(*ret);
        }
    else
    /*  Are we setting the lexical token vector: Sc? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Sc)
        {
		switch (newValue.Tag)
			{
			case TYVOID:
				self->DebuggerSource = NIL; 
				break;

			default:
				if (_TObject_TypeFlag(newValue.Tag) == _TObject_TfTOBJECT)
					self->DebuggerSource = newValue.u.Object;
				else
					*ret = TERROR("!Lambda.set: Debugger source can only accept a valid Object or void!");
			break;
			}
        FrameExit(*ret);
        }
    else
    /*  Are we setting the native code vector: Nc? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Nc)
        {
		switch (newValue.Tag)
			{
			case TYVOID:
				self->NativeCodeVector = NIL; 
				break;

			case TYBYTEVECTOR:
				self->NativeCodeVector = newValue.u.ByteVector; 
				break;

			default:
				*ret = TERROR("!Lambda.set: Native code vector can only accept a ByteVector or void!");
			break;
			}
        FrameExit(*ret);
        }
    else
    /*  Are we setting the interfaces: In? */
    if (index1.u.Object == (TObject*)gCP->TLambda_In)
        {
		switch (newValue.Tag)
			{
			case TYVOID:
				self->Interfaces = NIL; 
				break;

			case TYSTRUCTURE:
				self->Interfaces = newValue.u.Structure; 
				break;

			default:
				*ret = TERROR("!Lambda.set: Faces can only accept a Structure or void!");
			break;
			}
        FrameExit(*ret);
        }
    else
    /*  Are we setting the virtual machine evaluator: Vm? */
    if (index1.u.Object == (TObject*)gCP->TLambda_Vm)
        {
		switch (newValue.Tag)
			{
			case TYVMEVALUATOR:
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				self->VirtualMachine = newValue.u.Symbol; 
				break;

			default:
				*ret = TERROR("!Lambda.set: Vm property can only accept a VmEvaluator or Symbol!");
			break;
			}
        FrameExit(*ret);
        }
	else
        {
        /* Reference the Permanent variables structure of the Lambda. */
        /* If a "set1" child Lambda exists, then invoke it with the index; */
        /* otherwise, reference the symbol as a persistent variable. */
        *indexProc = TLambda_GetPvMember(gCP,gTP,selfTval,"set1");
        if (indexProc->Tag == TYLAMBDA)
            {
			/* Invoke the set member function from the Lambda (if any). */
			/* Reference the Permanent variables structure of the Lambda. */
			*indexProc = TLambda_GetPvMember(gCP,gTP,selfTval,"set1");
			ExitOnError(*indexProc);
			/* Invoke the set1 subfunction of the Lambda. */
			prmv[0] = index1;
			prmv[1] = newValue;
			*ret = FSmartbase_Evalv(gCP,gTP,*indexProc,2,&prmv[0]);
            }
        else
            {
            /* Set the specified Pv/Cv variable of the Lambda (if any). */
            *ret = TLambda_SetPvCvSymbol(gCP,gTP,selfTval,index1,newValue);
            }

        FrameExit(*ret);
        }
    }

/* Invoke the set member function from the Lambda (if any). */
/* Reference the Permanent variables structure of the Lambda. */
*indexProc = TLambda_GetPvMember(gCP,gTP,selfTval,"set1");
ExitOnError(*indexProc);
/* Invoke the set1 subfunction of the Lambda. */
prmv[0] = index1;
prmv[1] = newValue;
*ret = FSmartbase_Evalv(gCP,gTP,*indexProc,2,&prmv[0]);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
length

Determine the length of the Lambda. 

#endif

TVAL TLambda_Length(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareTVAL(ret);
DeclareTVAL(indexProc);
EndFrame

/* Invoke the length member function from the librarian Lambda (if any). */
/* Reference the Permanent variables structure of the Lambda. */
*indexProc = TLambda_GetPvMember(gCP,gTP,selfTval,"len");
ExitOnError(*indexProc);
/* Invoke the length subfunction of the Lambda. */
*ret = FSmartbase_Evalv(gCP,gTP,*indexProc,0,&selfTval);
FrameExit(*ret);
}
