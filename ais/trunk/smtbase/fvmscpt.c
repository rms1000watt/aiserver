/**********************************************************************************
    Copyright (C) 2013 Analytic Research Foundation.

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
FVmScript.c

Implementation of the Virtual Machine Procedure engine.

This source file contains the main evaluation functions for the VmScript 
Procedure object. A Procedure object contains a compiled VmScript formula. 
Adopted from Scheme and other dialects of Lisp, it is one of the main 
mechanisms for storing Lambdas in Smartbase.

PARENT:             None. 

AUTHORS:            Michael F. Korns

CHANGE HISTORY
Version     Date        Who     Change
3.0001		06/26/2007  mfk		Changes for 64bit port
1.0002		03/07/2007  rca		Bugfix for CR77
1.0001      8/01/2006   rca     Set header file for  _AMD64
												--------------- ---------------
#endif
#include	"ctype.h"
#include    "fvmscpt.h"
#ifdef _JIT
#ifdef _M32
#include    "fvmintelp3jit.h"
#endif
#ifdef _M64
#include    "fvmamd64jit.h"
#endif
#endif
#include    "tpair.h"
#include    "fcompile.h"
#include    "futil1.h"
#include    "flisp.h"
#include    "fvmscpt2.h"
#include    "tdiction.h"
#include    "tdirect.h"
#include    "tstring.h"
#include    "tsymbol.h"
#include    "tstruct.h"
#include    "tbrick.h"
#include    "tlambda.h"
#include    "fproc.h"
#include    "futil1.h"
#include    "futil2.h"
#include    "futil3.h"
#include    "fconvert.h"
#include    "tbitvec.h"
#include    "tintvec.h"
#include    "tpcodvec.h"
#include    "tbytevec.h"
#include    "tbitvec.h"
#include    "tnumvec.h"
#include    "tfltvec.h"
#include    "tobjvec.h"
#include    "fvmscpt2.h"
#include    "fdebug.h"
#include    "fpropty.h"
#include    "fpred.h"
#include    "fpred2.h"
#include    "fmake.h"
#include    "fmath1.h"
#include    "tcontin.h"
#include    "fpred2.h"
#include    "terror.h"
#include    "tmatrix.h"
#include    "tnummat.h"
#include	"tcpx.h"
#include	"tcpxvec.h"
#include	"tshortvec.h"
#include	"tlongvec.h"

/****************************************************************/
/* Start main macro definitions for the current host cpu model. */
/****************************************************************/
/* Define the Just In Time (JIT) compiler for this host cpu. */
#ifdef _JIT
//extern LpVMEVALUATOR	JIT;
/* Define the Just In Time (JIT) compiler for this host cpu. */
#ifdef _M32
#define JITNAME		FVmIntelP3Jit_Eval
#endif
#ifdef _M64
#define JITNAME		FVmAmd64Jit_Eval
#endif
#endif

/* Define the JIT set Ip from offset for this host cpu model. */
#undef  setIpFromOffset
#define setIpFromOffset(offset) &pcodes[(offset)]

/* Define the asm jump label entry for this host cpu model. */
#if _GCC
#define setLabel(lbl) asm("" #lbl ":");
#else
#define setLabel(lbl) lbl:
#endif

/* Define the load jump table entry for this host cpu model. */
#undef	loadJumpTableEntry
#define loadJumpTableEntry(lbl,opcode)

/* Define the jump to opcode entry for this host cpu model. */
#undef	jumpToOpcodeEntry
#define jumpToOpcodeEntry(opcode)

/* Define the jump into opcode entry for this host cpu model. */
#undef	jitOfflineEntry
#define jitOfflineEntry

/* Define the jump out of opcode exit for this host cpu model. */
#undef	jitOfflineExit
#define jitOfflineExit goto Fetch;

/****************************************************************/
/* End main macro definitions for the current host cpu model.   */
/****************************************************************/

LpVMEVALUATOR	JIT = 0;

/*--------------------------------------------------------------------------------------- */
#if 0
FVmScript_Init

Initialize the virtual machine test bed for the VmScript function library.  

#endif

TVAL FVmScript_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
if(gCP->FVmscript_Initialized) 
	return(gCP->TObject_OK);

gCP->FVmscript_Initialized = 1;

#ifdef _JIT
JIT = &JITNAME;
#endif

gCP->FVmScript_ERROR_ILLEGAL_INSTRUCTION	= TPERROR("!drmVM: UnknownPcode!");
gCP->FVmScript_ERROR_ILLEGAL_VALUE			= TPERROR("!drmVM: illegal argument value for instruction!");
gCP->FVmScript_ERROR_DIVIDE_BY_ZERO			= TPERROR("!drmVM: div/0!");
gCP->FVmScript_ERROR_OVERFLOW				= TPERROR("!drmVM: overflow!");
gCP->FVmScript_ERROR_MISSING_FUNCTION_NAME	= TPERROR("!drmVM: missing function name!");

gCP->FVmScript_ERROR_NOT_AN_Lambda			= TPERROR("!drmVM: not a Lambda!");
gCP->FVmScript_ERROR_MISSING_PCODES			= TPERROR("!drmVM: this nonexecutable Lambda object has missing pcodes!");
gCP->FVmScript_ERROR_VMADD_BAD_VALUE		= TPERROR("!drmVM: add bad value!");
gCP->FVmScript_ERROR_VMDIV_BAD_VALUE		= TPERROR("!drmVM: div bad value!");
gCP->FVmScript_ERROR_VMDIVR_BAD_VALUE		= TPERROR("!drmVM: divr bad value!");
gCP->FVmScript_ERROR_VMMUL_BAD_VALUE		= TPERROR("!drmVM: mul bad value!");
gCP->FVmScript_ERROR_VMSUB_BAD_VALUE		= TPERROR("!drmVM: sub bad value!");
gCP->FVmScript_ERROR_VMADDI_BAD_VALUE		= TPERROR("!drmVM: addi bad value!");
gCP->FVmScript_ERROR_VMADDU_BAD_VALUE		= TPERROR("!drmVM: addu bad value!");
gCP->FVmScript_ERROR_VMSUBI_BAD_VALUE		= TPERROR("!drmVM: subi bad value!");
gCP->FVmScript_ERROR_VMSUBU_BAD_VALUE		= TPERROR("!drmVM: subu bad value!");
gCP->FVmScript_ERROR_VMDIVI_BAD_VALUE		= TPERROR("!drmVM: divi bad value!");
gCP->FVmScript_ERROR_VMDIVU_BAD_VALUE		= TPERROR("!drmVM: divu bad value!");
gCP->FVmScript_ERROR_VMDIVRI_BAD_VALUE		= TPERROR("!drmVM: divri bad value!");
gCP->FVmScript_ERROR_VMDIVRU_BAD_VALUE		= TPERROR("!drmVM: divru bad value!");
gCP->FVmScript_ERROR_VMMULI_BAD_VALUE		= TPERROR("!drmVM: muli bad value!");
gCP->FVmScript_ERROR_VMMULU_BAD_VALUE		= TPERROR("!drmVM: mulu bad value!");
gCP->FVmScript_ERROR_VMARGFETCH_MOD			= TPERROR("!drmVM: argfetch bad modifier!");
gCP->FVmScript_ERROR_VMARGFETCH_VALUE		= TPERROR("!drmVM: argfetch bad value!");
gCP->FVmScript_ERROR_VMONERROR_MOD			= TPERROR("!drmVM: onerror bad modifier!");
gCP->FVmScript_ERROR_VMPUSH_MOD				= TPERROR("!drmVM: push bad modifier!");
gCP->FVmScript_ERROR_VMREF_MOD				= TPERROR("!drmVM: ref bad modifier!");
gCP->FVmScript_ERROR_VMRETURN_MOD			= TPERROR("!drmVM: return bad modifier!");
gCP->FVmScript_ERROR_VMSEND_MESSAGE			= TPERROR("!drmVM: send unknown message!");
gCP->FVmScript_ERROR_VMSET_MOD				= TPERROR("!drmVM: set bad modifier!");
gCP->FVmScript_ERROR_VMTARGET_MOD			= TPERROR("!drmVM: vm-assignment bad target modifier!");

gCP->FDebug_ShowRegVars = FALSE;
gCP->FDebug_ShowTempVars = FALSE;
gCP->FDebug_ShowArgVars = FALSE;
gCP->FDebug_ShowClassVars = FALSE;
gCP->FDebug_ShowConstVars = FALSE;
gCP->FDebug_ShowInterfaceVars = FALSE;
gCP->FDebug_ShowPersistentVars = FALSE;
return(gCP->TObject_OK);

}

/*--------------------------------------------------------------------------------------- */
#if 0
FVmScript_Eval

The main virtual machine instruction loop for VmScript Procedure objects.

#endif


TVAL FVmScript_Eval(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* proc,NUM argc,TVAL argv[])
{
TVAL	result;
/*  ===================================================================== */
/*  Execute the Just In Time Compiler or the Emulator.                    */
/*                                                                        */
/*  We use the just in time (JIT) compiler, for this host cpu model, if   */
/*  the debugger switches are set off. We only use the Virtual Machine    */
/*  emulator if we are in debugger mode.                                  */
/*  ===================================================================== */
++proc->InUse;
if (JITRUNNING && *JIT != NULL)
    {
	result = (*JIT)(gCP,gTP,proc,argc,argv);
	}
else
    {
	result = FVmScript_Emulator(gCP,gTP,proc,argc,argv);
	}
--proc->InUse;
return(result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FVmScript_Emulator

The main virtual machine (emulation) instruction loop for VmScript Procedure objects.

Note:   We evaluate a single VmScript Procedure object in emulation mode.
        
#endif


TVAL FVmScript_Emulator(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* proc,NUM argc,TVAL argv[])
{
/*  VIRTUAL MACHINE EMULATION INSTRUCTION JUMP TABLE */
typedef	VOID			(*LpFVOID)		();

/*  VIRTUAL EMULATION MACHINE REGISTERS */
register NUM*           Ip;                     /*  Instruction pointer */
register TVAL*          target;                 /*  Target operand address */
register TVAL*          source;                 /*  Source operand address */
register TVAL*          argument;               /*  Argument operand address */
register TVAL*          index;					/*  Index operand address */
register NUM			overFlowReg;			/*  Temporary Overflow Register */
register NUM			argIncID;				/*  Vector Set Pointer Inc ptr register id */
register NUM			srcIncID;				/*  Vector Set Pointer Inc ptr register id */
register NUM			tarIncID;				/*  Vector Set Pointer Inc ptr register id */
register NUM			argPtrID;				/*  Vector Set Pointer arg ptr register id */
register NUM			srcPtrID;				/*  Vector Set Pointer src ptr register id */
register NUM			tarPtrID;				/*  Vector Set Pointer tar ptr register id */

/*  VM LOCAL VARIABLE DECLARATIONS */
/*  Note:   These variables should be kept to an absolute necessary */
/*          minimum, as they eat up large (approc 132 bytes) of C  */
/*          system stack space with each Lambda recursion.  */
TPcodeVector*           Pc;                     /*  Pcode vector register */
TStructure*				Sv;                     /*  Self variable structure */
TStructure*				Pv;                     /*  Persistent variable structure */
TStructure*				Cv;                     /*  Persistent class variable structure */
TStructure*				Rv;                     /*  Register variable structure */
LpTVAL                  Fb;                     /*  Frame base address */
UNUM*                   pcodes;                 /*  Pcode vector address */
OPCODE                  pcode;                  /*  Current pcode value */
TLambda*					self;                   /*  Active Procedure object */
NUM                     oldSi;                  /*  Smartbase Stack Reset position  */
NUM                     saveSi;                 /*  Smartbase Stack Save position  */
TVAL                    retValue;               /*  Smartbase Return value */
NUM                     i;                      /*  Temporary index variable */
NUM                     k;                      /*  Temporary index variable */
NUM                     m;                      /*  Temporary index variable */
NUM                     n;                      /*  Temporary index variable */
LpBIND					bindPtr;				/*  Temporary pointer variable */
LpCHAR					fieldArrayPtr;			/*  Temporary pointer variable */
TVAL					onErrorHandler;			/*	Error Event Handler for current Lambda */
TVAL                    isource;				/*  Immediate source value */
TObjVector*				attv;					/*  Attribute vector used for record mode in vectors */
TCpx*					cp;						/*  Ptr to Complex target */
REAL					sr,si,ar,ai,am;			/*  Real/imag parts of source/argument */
LpCHAR					argP;					/*  Vector instruction argument pointer */
LpCHAR					srcP;					/*  Vector instruction source pointer */
LpCHAR					tarP;					/*  Vector instruction target pointer */
NUM						argInc;					/*  Vector instruction argument increment */
NUM						srcInc;					/*  Vector instruction source increment */
NUM						tarInc;					/*  Vector instruction target increment */
NUM						vecInitializeExtent;	/*  vmvecInitialize extent argument */
NUM						vecInitializeCount;		/*  vmvecInitialize internal counter register */
NUM						vecInitializeLabel;		/*  vmvecInitialize internal loop pcode address */

/*  VM VIRTUAL MACHINE REGISTERS ARRAY */
/*  Note:   These virtual machine registers are used as the    */
/*          emulated storage for all of the VM register        */
/*          instructions. They are stored as TVALs so that the */
/*          memory instructions can access them easily.        */

TVAL					Rp[MAXREGISTERCNT];		/* Virtual Machine Registers Array			  */
LpCHAR					REGP = (LpCHAR)&Rp[0];	/* Pointer to Virtual Machine Registers Array */

/*  VM VIRTUAL MACHINE INTERNAL NUMBER VECTOR PROCESSING STACK */

REAL					Vs[MAXVECTORCNT+1];		/* Internal Number Vector Processing Stack    */
NUM						VsTopOfStack = 0;		/* Internal Number Vector Processing Stack Top*/

TVAL itmpValue;

/**** Initialization. ****/

Ip = NULL;
tarP = NULL;
srcP = NULL;
argP = NULL;
tarIncID = 0;
srcIncID = 0;
argIncID = 0;
tarPtrID = 0;
srcPtrID = 0;
argPtrID = 0;
tarInc = 0;
srcInc = 0;
argInc = 0;
vecInitializeCount = 0;
vecInitializeExtent = 0;
vecInitializeLabel = 0;

retValue = gCP->TObject_VOID;

/*  Set error handler off. */

onErrorHandler = gCP->Tval_VOID;

/*  Check for system stack or recursion overflow. */

StartRecursion;

if (proc->itsObjectType == TYLAMBDA)
    self = (TLambda*)proc;
else
    {EndRecursion; goto ErrorNotAnLambda;}
    
saveSi = TopOfStack;

/*  Nested Jumps, from the vmrecalc instruction, restart here.  */
/*  Load the Pcode Vector register. */
/*  Make sure this Procedure object has a Pcode Vector. */
if (self->PcodeVector == NIL)
    {TopOfStack = saveSi; EndRecursion; goto ErrorMissingPcodes;}
else
    Pc = (TPcodeVector*)self->PcodeVector;

/*  Load the Register variable register. */
/*  Note: Initialize the current virtual */
/*        machine registers to the types */
/*        specified in the register      */
/*        variables structure.           */
if (self->RegisterVariables == NIL)
	{
    Rv = NIL;
	}
else
	{
    Rv = self->RegisterVariables;
	n = Rv->itsMaxItemIndex;
	if (Rv->itsMaxItemIndex > MAXREGISTERCNT) goto ErrorTooManyRegisters;
	for (i = 0; i < n; ++i)
		{
		Rp[i] = atHMBind(Rv->itsDictionaryArray,i).Value;
		}
	}

/*  Load the Self variable register. */
if (self->ClassVariables == NIL)
	{
    Sv = NIL;
	}
else
	{
    Sv = self->ClassVariables;
	if ((argc >= 1) && (self->ArgumentVariables->itsMaxItemIndex >= 1) && (BindArray(TOBJ(self->ArgumentVariables))[0].Key == (TObject*)gCP->TLambda_self))
		{
		if ((argv[0].Tag != TYSTRUCTURE) ||
			(self->ClassVariables->itsMaxItemIndex > argv[0].u.Structure->itsMaxItemIndex)
			)
			{
			SelfArgumentError:
			TopOfStack = saveSi; EndRecursion; goto ErrorInvalidArglist;
			}
		/* Self argument layout must match ClassVariables layout */ 
		for (n = 0; n < self->ClassVariables->itsMaxItemIndex; ++n)
			{
			if (BindArray(TOBJ(self->ClassVariables))[n].Key != BindArray(argv[0])[n].Key)
				goto SelfArgumentError;
			}
		}
	}

/*  Load the Persistent variable register. */
if (self->PersistantVariables == NIL)
    Pv = NIL;
else
    Pv = self->PersistantVariables;

/*  Load the Persistent class variable register. */
if (self->ConstantVariables == NIL)
    Cv = NIL;
else
    Cv = self->ConstantVariables;

/*  Compute the number of temporary variables in the current frame. */
if (self->TemporaryVariables == NIL)
    n = 0;
else
    n = self->TemporaryVariables->itsMaxItemIndex;

/*  Extend the Frame Base register. */

Fb = &gTP->TvalStack[TopOfStack];
TopOfStack += n;

/*  Load the current temporary variables into the Frame Base register. */

for (i = 0; i < n; ++i)
    {
    Fb[i] = atHMBind(self->TemporaryVariables->itsDictionaryArray,i).Value;
    }

/*  Match arguments with formal parameters. */
if (self->ArgumentVariables == NIL)
    {
    gTP->FVmscript_SizeOfFormals = 0;
    gTP->FVmscript_definiteArgs = FALSE;
    }
else
    {
    gTP->FVmscript_SizeOfFormals = self->ArgumentVariables->itsMaxItemIndex;
    gTP->FVmscript_definiteArgs = (self->ArgumentVariables->itsCdr.Tag == TYVOID);
    }
if ((gTP->FVmscript_SizeOfFormals != argc) && gTP->FVmscript_definiteArgs)
    {TopOfStack = saveSi; EndRecursion; goto ErrorInvalidArglist;}
else
if (gTP->FVmscript_SizeOfFormals > argc)
    {TopOfStack = saveSi; EndRecursion; goto ErrorInvalidArglist;}

if (gTP->FVmscript_definiteArgs)
	{
	for (n = 0; n < gTP->FVmscript_SizeOfFormals; ++n)
		{
		m = BindArray(TOBJ(self->ArgumentVariables))[n].Value.DeclaredType;
		if ((m != TYVOID) && (m != TYTVAL) && (m != argv[n].Tag))
			{TopOfStack = saveSi; EndRecursion; goto ErrorInvalidArglist;}
		}
	}

/*  Start the main Virtual Machine pcode fetch loop. */
/*  Note:   Load the instruction pointer and lock the pcode */
/*          vector for the duration of the fetch loop. */
if (Pc->itsMaxItemIndex <= 0)
    {
    retValue = gCP->TObject_VOID;
    goto NestedReturn;
    }

/*  Set the Instruction Pointer to the start of the pcode vector.     */
Ip = &atHMInt(Pc->itsInstructionArray,0);
pcodes = (UNUM*)&atHMInt(Pc->itsInstructionArray,0);

/*  Reload the Virtual Machine Modifier base addresses and index factors. */
/*	These registers are all set with one level of indirection (handles) */
/*	because the AMPVOFFSET must point to the Pv Structures handle, */
/*  since the Pv structure of an Lambda may resize during execution. */

Rp[AMGVOFFSET].Tag = TYNUM; Rp[AMGVOFFSET].u.Int = ((NUM)&gCP->TLambda_assign->itsGlobalValue - (NUM)gCP->TLambda_assign);
Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)(LpBIND)&argv[0].u.Structure->itsDictionaryArray;
Rp[AMAVOFFSET].Tag = TYNUM; Rp[AMAVOFFSET].u.Int = (NUM)&argv[0];
Rp[AMTVOFFSET].Tag = TYNUM; Rp[AMTVOFFSET].u.Int = (NUM)Fb;
Rp[AMPVOFFSET].Tag = TYNUM; Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)(LpBIND)&Pv->itsDictionaryArray;
Rp[AMCVOFFSET].Tag = TYNUM; Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)(LpBIND)&Cv->itsDictionaryArray;
Rp[AMREGISTER].Tag = TYNUM; Rp[AMREGISTER].u.Int = (NUM)&Rp[0];

if (TESTON)
	{
	FMemory_SystemDiagnostic(gCP,gTP,1,&gCP->Tval_FALSE);
	}

Fetch:
/*  ===================================================================== */
/*  Here we manage all of the 32 bit special cases, Each of these special */
/*  cases are managed as quickly as possible. Obviously virtual machine   */
/*  instructions interpreted in this section execute fairly fast.         */
/*  ===================================================================== */

pcode.Opcode = *(Ip++);

/* Here we manage the virtual machine's debugger facility (if necessary). */
/* Note1: This adds only one test to the fetch loop whenever the debugger */
/*        facility is NOT turned on.                                      */ 
/* Note2: This code should be removed when an interrupt debugger, using   */
/*        the vmdebugger instruction, is implemented.                     */ 
NestedDebugResume:
DebugResume:

/*  ===================================================================== */
/* Here we manage the virtual machine's debugger facility (if necessary). */
/* Note1: This adds only one test to the fetch loop whenever the debugger */
/*        facility is NOT turned on.                                      */ 
/* Note2: This code should be removed when an interrupt debugger, using   */
/*        the vmdebugger instruction, is implemented.                     */ 
/*  ===================================================================== */
if ((gTP->DebugTraceOn != NIL) && (gTP->DebugSuspended == FALSE))
    {
    if (((gTP->DebugBreakProc == NIL) && (gTP->DebugBreakExp.Tag == TYVOID)) ||
        ((gTP->DebugBreakProc == proc) && (gTP->DebugBreakIP < 0)) ||
        ((gTP->DebugBreakProc == proc) && 
         (gTP->DebugBreakIP == (Ip - 1 - &atHMInt(Pc->itsInstructionArray,0))) &&
         (--gTP->DebugBreakCount <= 0)) ||
        (FSmartbase_DebugUntilExp(gCP,gTP) == TRUE))
        {
        /*  ===================================================================== */
        /*  Here we manage the virtual machine's debugger and trace facility. */
        /*  ===================================================================== */
        
        CallVMDebugger:
		if (TESTON)
			FMemory_SystemDiagnostic(gCP,gTP,1,&gCP->Tval_FALSE);

        /*  Construct the command line for the debugDialog display. */
        FVmScript2_DebugManager(gCP,gTP,self,argc,argv,Pc,Pv,Sv,Rp,Vs,VsTopOfStack,Fb,Ip,FALSE,retValue);

		}
    }
ResumeAfterFetch:
/*  ===================================================================== */
/* End of the virtual machine's debugger facility management code section.*/
/*  ===================================================================== */


/* Main instruction emulation loop.      */
/* Note: If we have not defined the host */
/*       jumpToOpcodeEntry cpu assembler */ 
/*		 macro, then we fall through to  */
/*       the normal C switch statement.  */

jumpToOpcodeEntry(pcode.u.Pcode)
switch (pcode.u.Pcode)
    {
    case  VMADD:
		setLabel(LVMADD)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Add the argument to the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = 0;
						target->Tag = TYNUM;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = argument->u.UInt;
						target->Tag = TYUNUM;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = argument->u.Int;
						target->Tag = TYNUM;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = argument->u.Real;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = si = (double)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMADDCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadAddValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = source->u.UInt;
						target->Tag = TYUNUM;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = source->u.UInt + argument->u.UInt;
						target->Tag = TYUNUM;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.UInt + argument->u.Int;
						target->Tag = TYNUM;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (REAL)source->u.Int + argument->u.Real;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = (REAL)source->u.Int;
						si = (REAL)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMADDCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadAddValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = source->u.Int;
						target->Tag = TYNUM;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = source->u.Int + argument->u.UInt;
						target->Tag = TYNUM;
                        break;

                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.Int + argument->u.Int;
						target->Tag = TYNUM;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (REAL)source->u.Int + argument->u.Real;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = (REAL)source->u.Int;
						si = (REAL)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMADDCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadAddValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = source->u.Real;
						target->Tag = TYREAL;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = source->u.Real + (REAL)argument->u.UInt;
						target->Tag = TYREAL;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = source->u.Real + (REAL)argument->u.Int;
						target->Tag = TYREAL;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = source->u.Real + argument->u.Real;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = source->u.Real;
						si = (REAL)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMADDCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadAddValue;
                    }
                break;

			case TYCPX:
				switch (argument->Tag)
				{
				case TYVOID:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = ai = (REAL)0.0;
					goto VMADDCPX;

				case TYCPX:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = argument->u.Complex->itsReal;
					ai = argument->u.Complex->itsImag;
					goto VMADDCPX;
                    
				case TYUNUM:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = (REAL)argument->u.UInt;
					ai = (REAL)0.0;
					goto VMADDCPX;
                    
				case TYNUM:
				case TYCHAR:
				case TYBOLE:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = (REAL)argument->u.Int;
					ai = (REAL)0.0;
					goto VMADDCPX;
                    
				case TYDATE:
				case TYMONEY:
				case TYREAL:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = argument->u.Real;
					ai = (REAL)0.0;
					goto VMADDCPX;
                    
				case TYERROR:
					retValue = *argument;
					goto NestedReturn;

				default:
					goto BadSubValue;
				}
				break;

				VMADDCPX:
				cp = (target->Tag == TYCPX) ? target->u.Complex : TCpx_New(gCP, gTP);
				cp->itsReal = sr + ar;
				cp->itsImag = si + ai;
				target->Tag = TYCPX;
				target->u.Complex = cp;
				break;

			case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadAddValue:
				retValue = FUtil2_QuickAppend(gCP,gTP,source,argument);
				if (retValue.Tag == TYERROR)
					{
					retValue = gCP->FVmScript_ERROR_VMADD_BAD_VALUE;
					goto NestedReturn;
					}
				*target = retValue;
                break;
            }
        break;
        
    case  VMDIV:
		setLabel(LVMDIV)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Divide the source by the argument and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYNUM:
                    case TYUNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = 0;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = 0;
                        break;

					case TYCPX:
                        target->u.Real = 0;
                        break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) 
							goto IllegalDivide;
                        target->u.Real = (REAL)source->u.Int / (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) 
							goto IllegalDivide;
                        target->u.Real = (REAL)source->u.UInt / (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Real = (REAL)source->u.UInt / argument->u.Real;
                        break;

					case TYCPX:
						sr = (REAL)source->u.UInt;
						si = (double)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMDIVCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) 
							goto IllegalDivide;
                        target->u.Real = (REAL)source->u.Int / (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) 
							goto IllegalDivide;
                        target->u.Real = (REAL)source->u.Int / (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Real = (REAL)source->u.Int / argument->u.Real;
                        break;

					case TYCPX:
						sr = (REAL)source->u.Int;
						si = (double)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMDIVCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) 
							goto IllegalDivide;
                        target->u.Real = source->u.Real / (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) 
							goto IllegalDivide;
                        target->u.Real = source->u.Real / (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) 
							goto IllegalDivide;
                        target->u.Real = source->u.Real / argument->u.Real;
                        break;

					case TYCPX:
						sr = source->u.Real;
						si = (double)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMDIVCPX;
 
					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivValue;
                    }
                break;

			case TYCPX:
				switch (argument->Tag)
				{
				case TYVOID:
					goto IllegalDivide;

				case TYCPX:
					sr = source->u.Complex->itsReal;
					ar = argument->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ai = argument->u.Complex->itsImag;
                    goto VMDIVCPX;

				case TYUNUM:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = (REAL)argument->u.UInt;
					ai = (double)0.0;
                    goto VMDIVCPX;

				case TYNUM:
				case TYCHAR:
				case TYBOLE:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = (REAL)argument->u.Int;
					ai = (double)0.0;
                    goto VMDIVCPX;
                    
				case TYDATE:
				case TYMONEY:
				case TYREAL:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = argument->u.Real;
					ai = (double)0.0;
                    goto VMDIVCPX;
                    
				case TYERROR:
					retValue = *argument;
					goto NestedReturn;

				default:
					goto BadSubValue;
				}
				break;

			case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadDivValue:
                retValue = gCP->FVmScript_ERROR_VMDIV_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYREAL;
        break;
		VMDIVCPX:
		cp = (target->Tag == TYCPX) ? target->u.Complex : TCpx_New(gCP, gTP);
		if ((am = ar * ar + ai * ai) == (REAL)0.0)
			goto IllegalDivide;
		cp->itsReal = (sr * ar + si * ai) / am;
		cp->itsImag = (si * ar - sr * ai) / am;
		target->Tag = TYCPX;		// n.b. target can be same as a source.
		target->u.Complex = cp;
		break;

    case  VMDIVR:
		setLabel(LVMDIVR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Divide the source by the argument and assign remainder to the target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = 0;
                        break;
                    
                    case TYUNUM:
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = 0;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = 0;
                        break;
					case TYCPX:
                        target->u.Real = 0;
                        break;
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
 
                    default:
                        goto BadDivrValue;
                    }
                break;

            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = 0;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) 
							goto IllegalDivide;
                        gTP->FVmscript_Fraction = modf((REAL)source->u.UInt / (REAL)argument->u.UInt, &gTP->FVmscript_Integer);
                        target->u.Real = fabs((REAL)argument->u.UInt) * gTP->FVmscript_Fraction;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) 
							goto IllegalDivide;
                        gTP->FVmscript_Fraction = modf((REAL)source->u.UInt / (REAL)argument->u.Int, &gTP->FVmscript_Integer);
                        target->u.Real = fabs((REAL)argument->u.Int) * gTP->FVmscript_Fraction;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) 
							goto IllegalDivide;
                        gTP->FVmscript_Fraction = modf((REAL)source->u.UInt / argument->u.Real, &gTP->FVmscript_Integer);
                        target->u.Real = fabs(argument->u.Real) * gTP->FVmscript_Fraction;
                        break;

					case TYCPX:
						sr = (REAL)source->u.UInt;
						ar = argument->u.Complex->itsReal;
						goto VMDIVRCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;

                    default:
                        goto BadDivrValue;
                    }
                break;

            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = 0;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) 
							goto IllegalDivide;
                        gTP->FVmscript_Fraction = modf((REAL)source->u.Int / (REAL)argument->u.UInt, &gTP->FVmscript_Integer);
                        target->u.Real = fabs((REAL)argument->u.UInt) * gTP->FVmscript_Fraction;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) 
							goto IllegalDivide;
                        gTP->FVmscript_Fraction = modf((REAL)source->u.Int / (REAL)argument->u.Int, &gTP->FVmscript_Integer);
                        target->u.Real = fabs((REAL)argument->u.Int) * gTP->FVmscript_Fraction;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) 
							goto IllegalDivide;
                        gTP->FVmscript_Fraction = modf((REAL)source->u.Int / argument->u.Real, &gTP->FVmscript_Integer);
                        target->u.Real = fabs(argument->u.Real) * gTP->FVmscript_Fraction;
                        break;
					case TYCPX:
						sr = (REAL)source->u.Int;
						ar = argument->u.Complex->itsReal;
						goto VMDIVRCPX;
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;

                    default:
                        goto BadDivrValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = source->u.Real;
                        break;
                    
                    case TYUNUM:
						if (argument->u.Int == 0) 
							goto IllegalDivide;
                        gTP->FVmscript_Fraction = modf(source->u.Real / (REAL)argument->u.UInt, &gTP->FVmscript_Integer);
                        target->u.Real = fabs((REAL)argument->u.UInt) * gTP->FVmscript_Fraction;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) 
							goto IllegalDivide;
                        gTP->FVmscript_Fraction = modf(source->u.Real / (REAL)argument->u.Int, &gTP->FVmscript_Integer);
                        target->u.Real = fabs((REAL)argument->u.Int) * gTP->FVmscript_Fraction;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) 
							goto IllegalDivide;
                        gTP->FVmscript_Fraction = modf(source->u.Real / argument->u.Real, &gTP->FVmscript_Integer);
                        target->u.Real = fabs(argument->u.Real) * gTP->FVmscript_Fraction;
                        break;

					case TYCPX:
						sr = source->u.Real;
						ar = argument->u.Complex->itsReal;
						goto VMDIVRCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivrValue;
                    }
                break;

			case TYCPX:
				switch (argument->Tag)
				{
				case TYVOID:
					goto IllegalDivide;

				case TYCPX:
					sr = source->u.Complex->itsReal;
					ar = argument->u.Complex->itsReal;
                    goto VMDIVRCPX;

				case TYUNUM:
					sr = source->u.Complex->itsReal;
					ar = (REAL)argument->u.UInt;
                    goto VMDIVRCPX;

				case TYNUM:
				case TYCHAR:
				case TYBOLE:
					sr = source->u.Complex->itsReal;
					ar = (REAL)argument->u.Int;
                    goto VMDIVRCPX;
                    
				case TYDATE:
				case TYMONEY:
				case TYREAL:
					sr = source->u.Complex->itsReal;
					ar = argument->u.Real;
                    goto VMDIVRCPX;
                    
				case TYERROR:
					retValue = *argument;
					goto NestedReturn;

				default:
					goto BadSubValue;
				}
				break;

				VMDIVRCPX:
				if (ar == (REAL)0.0)
					goto IllegalDivide;
				sr = modf(sr / ar, &gTP->FVmscript_Integer); // real fractional part
				target->u.Real = fabs(ar) * sr;			// n.b. target can be same as a source.
				break;

			case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadDivrValue:
                retValue = gCP->FVmScript_ERROR_VMDIVR_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYREAL;
        break;

    case  VMMUL:
		setLabel(LVMMUL)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Multiply the source by the argument and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = 0;
						target->Tag = TYREAL;
                        break;
                    
                    case TYNUM:
                    case TYUNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = 0;
						target->Tag = TYREAL;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = 0;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = si = ar = ai = (double)0.0;
						goto VMMULCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMulValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = 0;
						target->Tag = TYREAL;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = source->u.UInt * argument->u.UInt;
						target->Tag = TYUNUM;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.Int * argument->u.UInt;
						target->Tag = TYNUM;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (REAL)source->u.UInt * argument->u.Real;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = (REAL)source->u.UInt;
						si = (double)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMMULCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMulValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = 0;
						target->Tag = TYREAL;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = source->u.Int * argument->u.UInt;
						target->Tag = TYNUM;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.Int * argument->u.Int;
						target->Tag = TYNUM;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (REAL)source->u.Int * argument->u.Real;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = (REAL)source->u.Int;
						si = (double)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMMULCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMulValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = 0;
						target->Tag = TYREAL;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = source->u.Real * (REAL)argument->u.UInt;
						target->Tag = TYREAL;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = source->u.Real * (REAL)argument->u.Int;
						target->Tag = TYREAL;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = source->u.Real * argument->u.Real;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = source->u.Real;
						si = (double)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMMULCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMulValue;
                    }
                break;

			case TYCPX:
				switch (argument->Tag)
				{
				case TYVOID:
					sr = si = ar = ai = (double)0.0;
					goto VMMULCPX;

				case TYCPX:
					sr = source->u.Complex->itsReal;
					ar = argument->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ai = argument->u.Complex->itsImag;
                    goto VMMULCPX;

				case TYUNUM:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = (REAL)argument->u.UInt;
					ai = (double)0.0;
                    goto VMMULCPX;

				case TYNUM:
				case TYCHAR:
				case TYBOLE:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = (REAL)argument->u.Int;
					ai = (double)0.0;
                    goto VMMULCPX;
                    
				case TYDATE:
				case TYMONEY:
				case TYREAL:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = argument->u.Real;
					ai = (double)0.0;
                    goto VMMULCPX;
                    
				case TYERROR:
					retValue = *argument;
					goto NestedReturn;

				default:
					goto BadSubValue;
				}
				break;
				VMMULCPX:
				cp = (target->Tag == TYCPX) ? target->u.Complex : TCpx_New(gCP, gTP);
				cp->itsReal = sr * ar - si * ai;
				cp->itsImag = sr * ai + si * ar;
				target->Tag = TYCPX;		// n.b. target can be same as a source.
				target->u.Complex = cp;
				break;

			case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadMulValue:
                retValue = gCP->FVmScript_ERROR_VMMUL_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        break;
        
    case  VMSUB:
		setLabel(LVMSUB)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = 0;
						target->Tag = TYNUM;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = -argument->u.UInt;
						target->Tag = TYNUM;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = -argument->u.Int;
						target->Tag = TYNUM;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = -argument->u.Real;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = si = (REAL)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMSUBCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = source->u.UInt;
						target->Tag = TYUNUM;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = source->u.UInt - argument->u.UInt;
						target->Tag = TYNUM;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.UInt - argument->u.Int;
						target->Tag = TYNUM;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (REAL)source->u.UInt - argument->u.Real;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = (REAL)source->u.UInt;
						si = (REAL)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMSUBCPX;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = source->u.Int;
						target->Tag = TYNUM;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = source->u.Int - argument->u.UInt;
						target->Tag = TYNUM;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.Int - argument->u.Int;
						target->Tag = TYNUM;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (REAL)source->u.Int - argument->u.Real;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = (REAL)source->u.Int;
						si = (REAL)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMSUBCPX;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = source->u.Real;
						target->Tag = TYREAL;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = source->u.Real - (REAL)argument->u.UInt;
						target->Tag = TYREAL;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = source->u.Real - (REAL)argument->u.Int;
						target->Tag = TYREAL;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = source->u.Real - argument->u.Real;
						target->Tag = TYREAL;
                        break;

					case TYCPX:
						sr = source->u.Real;
						si = (REAL)0.0;
						ar = argument->u.Complex->itsReal;
						ai = argument->u.Complex->itsImag;
						goto VMSUBCPX;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                
                    default:
                        goto BadSubValue;
                    }
                break;

			case TYCPX:
				switch (argument->Tag)
				{
				case TYVOID:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = ai = (REAL)0.0;
					goto VMSUBCPX;

				case TYCPX:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = argument->u.Complex->itsReal;
					ai = argument->u.Complex->itsImag;
					goto VMSUBCPX;
                    
				case TYUNUM:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = (REAL)argument->u.UInt;
					ai = (REAL)0.0;
					goto VMSUBCPX;
                    
				case TYNUM:
				case TYCHAR:
				case TYBOLE:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar = (REAL)argument->u.Int;
					ai = (REAL)0.0;
					goto VMSUBCPX;
                    
				case TYDATE:
				case TYMONEY:
				case TYREAL:
					sr = source->u.Complex->itsReal;
					si = source->u.Complex->itsImag;
					ar =  argument->u.Real;
					ai = (REAL)0.0;
					goto VMSUBCPX;
                    
				case TYERROR:
					retValue = *argument;
					goto NestedReturn;

				default:
					goto BadSubValue;
				}
				break;
				VMSUBCPX:
				cp = (target->Tag == TYCPX) ? target->u.Complex : TCpx_New(gCP, gTP);
				cp->itsReal = sr - ar;
				cp->itsImag = si - ai;
				target->Tag = TYCPX;		// n.b. target can be same as a source.
				target->u.Complex = cp;
				break;

			case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadSubValue:
                retValue = gCP->FVmScript_ERROR_VMSUB_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        break;
                
    case  VMMOVEN:
		setLabel(LVMMOVEN)
        /*  Load the address of each of the three operands */
        source    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        switch (source->Tag)
			{
			case TYVOID:
			case TYTVAL:
                target->u.Real = 0;
                target->Tag = TYREAL;
				break;
				
			case TYERROR:
				retValue = *source;
				goto NestedReturn;
				break;

			case TYUNUM:
                target->u.Real = source->u.UInt;
                target->Tag = TYREAL;
				break;

			case TYBOLE:
			case TYCHAR:
			case TYNUM:
                target->u.Real = source->u.Int;
                target->Tag = TYREAL;
				break;

			case TYDATE:
			case TYMONEY:
			case TYREAL:
                target->u.Real = source->u.Real;
                target->Tag = TYREAL;
				break;

			case TYCPX:
                target->u.Real = source->u.Complex->itsReal;
                target->Tag = TYREAL;
				break;

            default:
            BadMovenValue:
                retValue = gCP->FVmScript_ERROR_ILLEGAL_VALUE;
                goto NestedReturn;
                break;
			}
        break;

    case  VMCADD:
		setLabel(LVMCADD)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = source->u.Char + argument->u.Char;
		target->Tag = TYCHAR;
		goto Fetch;
		break;
 
    case  VMCDIV:
		setLabel(LVMCDIV)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.Char == 0) goto IllegalDivide;
		target->u.Char = source->u.Char / argument->u.Char;
		target->Tag = TYCHAR;
		goto Fetch;
        break;

    case  VMMOVEI:
		setLabel(LVMMOVEI)
        /*  Load the address of each of the three operands */
        source    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        switch (source->Tag)
			{
			case TYVOID:
			case TYTVAL:
                target->u.Int = 0;
                target->Tag = TYNUM;
				break;
				
			case TYERROR:
				retValue = *source;
				goto NestedReturn;
				break;

			case TYUNUM:
                target->u.Int = source->u.UInt;
                target->Tag = TYNUM;
				break;

			case TYBOLE:
			case TYCHAR:
			case TYNUM:
                target->u.Int = source->u.Int;
                target->Tag = TYNUM;
				break;

			case TYDATE:
			case TYMONEY:
			case TYREAL:
                target->u.Int = source->u.Real;
                target->Tag = TYNUM;
				break;

			case TYCPX:
                target->u.Int = source->u.Complex->itsReal;
                target->Tag = TYNUM;
				break;

            default:
                retValue = gCP->FVmScript_ERROR_ILLEGAL_VALUE;
                goto NestedReturn;
                break;
			}
        break;


    case  VMMOVEU:
		setLabel(LVMMOVEU)
        /*  Load the address of each of the three operands */
        source    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        switch (source->Tag)
			{
			case TYVOID:
			case TYTVAL:
                target->u.UInt = 0;
                target->Tag = TYUNUM;
				break;
				
			case TYERROR:
				retValue = *source;
				goto NestedReturn;
				break;

			case TYUNUM:
                target->u.UInt = source->u.UInt;
                target->Tag = TYUNUM;
				break;

			case TYBOLE:
			case TYCHAR:
			case TYNUM:
                target->u.UInt = source->u.Int;
                target->Tag = TYUNUM;
				break;

			case TYDATE:
			case TYMONEY:
			case TYREAL:
                target->u.UInt = source->u.Real;
                target->Tag = TYUNUM;
				break;

			case TYCPX:
                target->u.UInt = source->u.Complex->itsReal;
                target->Tag = TYUNUM;
				break;

            default:
                retValue = gCP->FVmScript_ERROR_ILLEGAL_VALUE;
                goto NestedReturn;
                break;
			}
        break;

    case  VMCMUL:
		setLabel(LVMCMUL)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = source->u.Char * argument->u.Char;
		target->Tag = TYCHAR;
		goto Fetch;
        break;

    case  VMCSUB:
		setLabel(LVMCSUB)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = source->u.Char - argument->u.Char;
		target->Tag = TYCHAR;
		goto Fetch;
        break;
 
     case  VMIADD:
		setLabel(LVMIADD)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int + argument->u.Int;
		target->Tag = TYNUM;
		goto Fetch;
		break;
 
     case  VMUADD:
		setLabel(LVMUADD)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.UInt = source->u.UInt + argument->u.UInt;
		target->Tag = TYUNUM;
		goto Fetch;
		break;
 
    case  VMIDIV:
		setLabel(LVMIDIV)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.Int == 0) goto IllegalDivide;
		target->u.Int = source->u.Int / argument->u.Int;
		target->Tag = TYNUM;
		goto Fetch;
        break;

		 
    case  VMUDIV:
		setLabel(LVMUDIV)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.UInt == 0) goto IllegalDivide;
		target->u.UInt = source->u.UInt / argument->u.UInt;
		target->Tag = TYUNUM;
		goto Fetch;
        break;

    case  VMIDIVR:
		setLabel(LVMIDIVR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.Int == 0) goto IllegalDivide;
		target->u.Int = source->u.Int % argument->u.Int;
		target->Tag = TYNUM;
		goto Fetch;
        break;

    case  VMUDIVR:
		setLabel(LVMUDIVR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.UInt == 0) goto IllegalDivide;
		target->u.UInt = source->u.UInt % argument->u.UInt;
		target->Tag = TYUNUM;
		goto Fetch;
        break;

    case  VMIMUL:
		setLabel(LVMIMUL)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int * argument->u.Int;
		target->Tag = TYNUM;
		goto Fetch;
        break;

    case  VMUMUL:
		setLabel(LVMUMUL)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.UInt = source->u.UInt * argument->u.UInt;
		target->Tag = TYUNUM;
		goto Fetch;
        break;

    case  VMISUB:
		setLabel(LVMISUB)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int - argument->u.Int;
		target->Tag = TYNUM;
		goto Fetch;
        break;

    case  VMUSUB:
		setLabel(LVMUSUB)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.UInt = source->u.UInt - argument->u.UInt;
		target->Tag = TYUNUM;
		goto Fetch;
        break;
 
    case  VMNADD:
		setLabel(LVMNADD)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = source->u.Real + argument->u.Real;
		target->Tag = TYREAL;
		goto Fetch;
        break;
 
    case  VMNDIV:
		setLabel(LVMNDIV)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.Real == 0) goto IllegalDivide;
		target->u.Real = source->u.Real / argument->u.Real;
		target->Tag = TYREAL;
		goto Fetch;
        break;

    case  VMNDIVR:
		setLabel(LVMNDIVR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.Real == 0) goto IllegalDivide;
        gTP->FVmscript_Fraction = modf(source->u.Real / argument->u.Real, &gTP->FVmscript_Integer);
        target->u.Real = argument->u.Real * gTP->FVmscript_Fraction;
		target->Tag = TYREAL;
		goto Fetch;
        break;

    case  VMNMUL:
		setLabel(LVMNMUL)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = source->u.Real * argument->u.Real;
		target->Tag = TYREAL;
		goto Fetch;
        break;

    case  VMNSUB:
		setLabel(LVMNSUB)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = source->u.Real - argument->u.Real;
		target->Tag = TYREAL;
		goto Fetch;
        break;
 
    case  VMADDI:
		setLabel(LVMADDI)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Add the argument to the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = argument->u.Real;
                        break;
                                      
                    case TYCPX:
                        target->u.Int = (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadAddiValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = source->u.UInt;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = source->u.UInt + argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.UInt + argument->u.UInt;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = source->u.UInt + (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = (NUM)source->u.Real + (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadAddiValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = source->u.UInt + argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.Int + argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = source->u.Int + (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = (NUM)source->u.Real + (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadAddiValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = (NUM)source->u.Real + argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = (NUM)source->u.Real + argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = (NUM)source->u.Real + (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = (NUM)source->u.Real + (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                
                    default:
                        goto BadAddiValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = source->u.Complex->itsReal;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = (NUM)source->u.Complex->itsReal + argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = (NUM)source->u.Complex->itsReal + argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = (NUM)source->u.Complex->itsReal + (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = (NUM)source->u.Complex->itsReal + (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                
                    default:
                        goto BadAddiValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadAddiValue:
                retValue = gCP->FVmScript_ERROR_VMADDI_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYNUM;
        break;
                        
 
    case  VMADDU:
		setLabel(LVMADDU)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Add the argument to the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = argument->u.Real;
                        break;
                                      
                    case TYCPX:
                        target->u.UInt = (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadAdduValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = source->u.UInt;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = source->u.UInt + argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = source->u.UInt + argument->u.UInt;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = source->u.UInt + (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = (NUM)source->u.Real + (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadAdduValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = source->u.UInt + argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = source->u.Int + argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = source->u.Int + (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = (NUM)source->u.Real + (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadAdduValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = (NUM)source->u.Real + argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = (NUM)source->u.Real + argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = (NUM)source->u.Real + (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = (NUM)source->u.Real + (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                
                    default:
                        goto BadAdduValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = source->u.Complex->itsReal;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = (NUM)source->u.Complex->itsReal + argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = (NUM)source->u.Complex->itsReal + argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = (NUM)source->u.Complex->itsReal + (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = (NUM)source->u.Complex->itsReal + (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                
                    default:
                        goto BadAdduValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadAdduValue:
                retValue = gCP->FVmScript_ERROR_VMADDU_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYUNUM;
        break;
                        
    case  VMADDN:
		setLabel(LVMADDN)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Add the argument to the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Real = argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = source->u.UInt;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = (REAL)source->u.UInt + (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = (REAL)source->u.UInt + (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (REAL)source->u.UInt + argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Real = (REAL)source->u.UInt + argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = (REAL)source->u.Int + (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = (REAL)source->u.Int + (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (REAL)source->u.Int + argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Real = (REAL)source->u.Int + argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = source->u.Real + (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = source->u.Real + (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = source->u.Real + argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Real = source->u.Real + argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = source->u.Complex->itsReal;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = source->u.Complex->itsReal + (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = source->u.Complex->itsReal + (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = source->u.Complex->itsReal + argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Real = source->u.Complex->itsReal + argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
                goto BadMovenValue;
                break;
            }
        target->Tag = TYREAL;
        break;
                        
    case  VMSUBI:
		setLabel(LVMSUBI)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = -argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = -argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = -argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = -argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubiValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = source->u.UInt;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = source->u.UInt - argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.UInt - argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = source->u.UInt - (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = source->u.UInt - (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubiValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = source->u.Int - argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.Int - argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = source->u.Int - (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = source->u.Int - (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubiValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = (NUM)source->u.Real - argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = (NUM)source->u.Real - argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = (NUM)source->u.Real - (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = (NUM)source->u.Real - (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubiValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = (NUM)source->u.Complex->itsReal;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = (NUM)source->u.Complex->itsReal - argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = (NUM)source->u.Complex->itsReal - argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = (NUM)source->u.Complex->itsReal - (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = (NUM)source->u.Complex->itsReal - (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubiValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadSubiValue:
                retValue = gCP->FVmScript_ERROR_VMSUBI_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYNUM;
        break;
        
                        
    case  VMSUBU:
		setLabel(LVMSUBU)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = -argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = -argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = -argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = -argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubuValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = source->u.UInt;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = source->u.UInt - argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = source->u.UInt - argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = source->u.UInt - (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = source->u.UInt - (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubuValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = source->u.Int - argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = source->u.Int - argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = source->u.Int - (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = source->u.Int - (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubuValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = (NUM)source->u.Real - argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = (NUM)source->u.Real - argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = (NUM)source->u.Real - (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = (NUM)source->u.Real - (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubuValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = (NUM)source->u.Complex->itsReal;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = (NUM)source->u.Complex->itsReal - argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = (NUM)source->u.Complex->itsReal - argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = (NUM)source->u.Complex->itsReal - (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = (NUM)source->u.Complex->itsReal - (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadSubuValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadSubuValue:
                retValue = gCP->FVmScript_ERROR_VMSUBU_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYUNUM;
        break;
        
    case  VMSUBN:
		setLabel(LVMSUBN)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = -argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = -argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = -argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Real = -argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = source->u.UInt;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = (REAL)source->u.UInt - (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = (REAL)source->u.UInt - (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (REAL)source->u.UInt - argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Real = (REAL)source->u.UInt - argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = (REAL)source->u.Int - (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = (REAL)source->u.Int - (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (REAL)source->u.Int - argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Real = (REAL)source->u.Int - argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = source->u.Int;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = source->u.Real - (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = source->u.Real - (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = source->u.Real - argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Real = source->u.Real - argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = source->u.Complex->itsReal;
                        break;
                    
                    case TYUNUM:
                        target->u.Real = source->u.Complex->itsReal - (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = source->u.Complex->itsReal - (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = source->u.Complex->itsReal - argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Real = source->u.Complex->itsReal - argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
                goto BadMovenValue;
                break;
            }
        target->Tag = TYREAL;
        break;
        
    case  VMDIVI:
		setLabel(LVMDIVI)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Int = 0;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Int = 0;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Int = 0;
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.Int = 0;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDiviValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Int = source->u.Int / argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Int = source->u.UInt / argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.UInt / (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.UInt / (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDiviValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Int = source->u.Int / argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Int = source->u.Int / argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Int / (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Int / (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDiviValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Real / (NUM)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Real / (NUM)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Real / (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Real / (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDiviValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Complex->itsReal / (NUM)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Complex->itsReal / (NUM)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Complex->itsReal / (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Complex->itsReal / (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDiviValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadDiviValue:
                retValue = gCP->FVmScript_ERROR_VMDIVI_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYNUM;
        break;
        
        
    case  VMDIVU:
		setLabel(LVMDIVU)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.UInt = 0;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.UInt = 0;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.UInt = 0;
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.UInt = 0;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivuValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.UInt = source->u.Int / argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.UInt = source->u.UInt / argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.UInt / (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.UInt / (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivuValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.UInt = source->u.Int / argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.UInt = source->u.Int / argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Int / (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Int / (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivuValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Real / (NUM)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Real / (NUM)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Real / (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Real / (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivuValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Complex->itsReal / (NUM)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Complex->itsReal / (NUM)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Complex->itsReal / (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Complex->itsReal / (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivuValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadDivuValue:
                retValue = gCP->FVmScript_ERROR_VMDIVU_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYUNUM;
        break;
        
    case  VMDIVN:
		setLabel(LVMDIVN)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Real = 0;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Real = 0;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Real = 0;
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.Real = 0;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Real = (REAL)source->u.UInt / (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Real = (REAL)source->u.UInt / (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Real = ((REAL)source->u.UInt / argument->u.Real);
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.Real = ((REAL)source->u.UInt / argument->u.Complex->itsReal);
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Real = (REAL)source->u.Int / (REAL)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Real = (REAL)source->u.Int / (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Real = ((REAL)source->u.Int / argument->u.Real);
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.Real = ((REAL)source->u.Int / argument->u.Complex->itsReal);
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Real = (source->u.Real / (REAL)argument->u.UInt);
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Real = (source->u.Real / (REAL)argument->u.Int);
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Real = (source->u.Real / argument->u.Real);
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.Real = (source->u.Real / argument->u.Complex->itsReal);
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Real = (source->u.Complex->itsReal / (REAL)argument->u.UInt);
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Real = (source->u.Complex->itsReal / (REAL)argument->u.Int);
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Real = (source->u.Complex->itsReal / argument->u.Real);
                        break;
                    
                    case TYCPX:
						if (argument->u.Complex->itsReal == 0) goto IllegalDivide;
                        target->u.Real = (source->u.Complex->itsReal / argument->u.Complex->itsReal);
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
                goto BadMovenValue;
                break;
            }
        target->Tag = TYREAL;
        break;
        
    case  VMDIVRI:
		setLabel(LVMDIVRI)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Int = 0;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Int = 0;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Int = 0;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivriValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Int = source->u.UInt % argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Int = source->u.UInt % argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Int = source->u.Int % (NUM)argument->u.Real;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivriValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Int = source->u.Int % argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Int = source->u.Int % argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Int = source->u.Int % (NUM)argument->u.Real;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivriValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Real % argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Real % argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.Int = (NUM)source->u.Real % (NUM)argument->u.Real;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDiviValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadDivriValue:
                retValue = gCP->FVmScript_ERROR_VMDIVRI_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYNUM;
        break;

        
    case  VMDIVRU:
		setLabel(LVMDIVRU)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.UInt = 0;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.UInt = 0;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.UInt = 0;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivruValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.UInt = source->u.UInt % argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.UInt = source->u.UInt % argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.UInt = source->u.Int % (NUM)argument->u.Real;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivruValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.UInt = source->u.Int % argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.UInt = source->u.Int % argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.UInt = source->u.Int % (NUM)argument->u.Real;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivruValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        goto IllegalDivide;
                        break;
                    
                    case TYUNUM:
						if (argument->u.UInt == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Real % argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
						if (argument->u.Int == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Real % argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
						if (argument->u.Real == 0) goto IllegalDivide;
                        target->u.UInt = (NUM)source->u.Real % (NUM)argument->u.Real;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadDivruValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadDivruValue:
                retValue = gCP->FVmScript_ERROR_VMDIVRU_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYUNUM;
        break;

    case  VMMULI:
		setLabel(LVMMULI)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = 0;
                        break;
                    
                    case TYNUM:
                    case TYUNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = 0;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = 0;
                        break;
                    
                    case TYCPX:
                        target->u.Int = 0;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMuliValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = source->u.UInt * argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.UInt * argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = (NUM)source->u.UInt * (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = (NUM)source->u.UInt * (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMuliValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = source->u.Int * argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = source->u.Int * argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = (NUM)source->u.Int * (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = (NUM)source->u.Int * (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMuliValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = (NUM)source->u.Real * (NUM)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = (NUM)source->u.Real * (NUM)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = (NUM)source->u.Real * (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = (NUM)source->u.Real * (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMuliValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.Int = (NUM)source->u.Complex->itsReal * (NUM)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Int = (NUM)source->u.Complex->itsReal * (NUM)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Int = (NUM)source->u.Complex->itsReal * (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.Int = (NUM)source->u.Complex->itsReal * (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMuliValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadMuliValue:
                retValue = gCP->FVmScript_ERROR_VMMULI_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYNUM;
        break;


    case  VMMULU:
		setLabel(LVMMULU)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = 0;
                        break;
                    
                    case TYNUM:
                    case TYUNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = 0;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = 0;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = 0;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMuluValue;
                    }
                break;
                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = source->u.UInt * argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = source->u.UInt * argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = (NUM)source->u.UInt * (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = (NUM)source->u.UInt * (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMuluValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = source->u.Int * argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = source->u.Int * argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = (NUM)source->u.Int * (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = (NUM)source->u.Int * (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMuluValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = (NUM)source->u.Real * (NUM)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = (NUM)source->u.Real * (NUM)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = (NUM)source->u.Real * (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = (NUM)source->u.Real * (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMuluValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.UInt = 0;
                        break;
                    
                    case TYUNUM:
                        target->u.UInt = (NUM)source->u.Complex->itsReal * (NUM)argument->u.UInt;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.UInt = (NUM)source->u.Complex->itsReal * (NUM)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.UInt = (NUM)source->u.Complex->itsReal * (NUM)argument->u.Real;
                        break;
                    
                    case TYCPX:
                        target->u.UInt = (NUM)source->u.Complex->itsReal * (NUM)argument->u.Complex->itsReal;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMuluValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
            BadMuluValue:
                retValue = gCP->FVmScript_ERROR_VMMULU_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYUNUM;
        break;

    case  VMMULN:
		setLabel(LVMMULN)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /* Subtract the argument from the source and assign result to target operand */
        switch (source->Tag)
            {
            case TYVOID:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = 0;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = 0;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = 0;
                        break;
                    
                    case TYCPX:
                        target->u.Real = 0;
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYNUM:
            case TYCHAR:
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Real = 0;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = (REAL)source->u.Int * (REAL)argument->u.Int;
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = ((REAL)source->u.Int * argument->u.Real);
                        break;
                    
                    case TYCPX:
                        target->u.Real = ((REAL)source->u.Int * argument->u.Complex->itsReal);
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = 0;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = (source->u.Real * (REAL)argument->u.Int);
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (source->u.Real * argument->u.Real);
                        break;
                    
                    case TYCPX:
                        target->u.Real = (source->u.Real * argument->u.Complex->itsReal);
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYCPX:
                switch (argument->Tag)
                    {
                    case TYVOID:
                        target->u.Int = 0;
                        break;
                    
                    case TYNUM:
                    case TYCHAR:
                    case TYBOLE:
                        target->u.Real = (source->u.Complex->itsReal * (REAL)argument->u.Int);
                        break;
                    
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        target->u.Real = (source->u.Complex->itsReal * argument->u.Real);
                        break;
                    
                    case TYCPX:
                        target->u.Real = (source->u.Complex->itsReal * argument->u.Complex->itsReal);
                        break;
                    
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
                
                    default:
                        goto BadMovenValue;
                    }
                break;
                
            case TYERROR:
                retValue = *source;
                goto NestedReturn;
                break;
                
            default:
                goto BadMovenValue;
                break;
            }
        target->Tag = TYREAL;
        break;

    case  VMAND:
		setLabel(LVMAND)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int & argument->u.Int;
        target->Tag = TYNUM;
		goto Fetch;
		break;
            
    case  VMOR:
		setLabel(LVMOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int | argument->u.Int;
        target->Tag = TYNUM;
		goto Fetch;
		break;
            
    case  VMSHL:
		setLabel(LVMSHL)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int << argument->u.Int;
        target->Tag = TYNUM;
		goto Fetch;
		break;
            
    case  VMSHR:
		setLabel(LVMSHR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int >> argument->u.Int;
        target->Tag = TYNUM;
		goto Fetch;
		break;
            
    case  VMXOR:
		setLabel(LVMXOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int ^ argument->u.Int;
        target->Tag = TYNUM;
		goto Fetch;
		break;
            
    case  VMIAND:
		setLabel(LVMIAND)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int & argument->u.Int;
        target->Tag = TYNUM;
		goto Fetch;
		break;
            
    case  VMIOR:
		setLabel(LVMIOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int | argument->u.Int;
        target->Tag = TYNUM;
		goto Fetch;
		break;
            
    case  VMIXOR:
		setLabel(LVMIXOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int ^ argument->u.Int;
        target->Tag = TYNUM;
		goto Fetch;
		break;
            
    case  VMIANDB:
		setLabel(LVMIANDB)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = !(source->u.Int & argument->u.Int);
        target->Tag = TYNUM;
		goto Fetch;
		break;
            
    case  VMIORB:
		setLabel(LVMIORB)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = !(source->u.Int | argument->u.Int);
        target->Tag = TYNUM;
		goto Fetch;
		break;
            
    case  VMIXORB:
		setLabel(LVMIXORB)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = !(source->u.Int ^ argument->u.Int);
        target->Tag = TYNUM;
		goto Fetch;
		break;
            
    case  VMARGCOUNT:
		setLabel(LVMARGCOUNT)
        /* Assign the argument count to target */
		target = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++));
        target->Tag = TYNUM;
        target->u.Int = argc;
		goto Fetch;
        break;

    case  VMARGFETCH:
		setLabel(LVMARGFETCH)
        /*  Load a pointer to the retValue argument */
        source   = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)));
        target   = ((TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)));
        *target = argv[source->u.Int];
		goto Fetch;
        break;
        
    case  VMONERROR:
		setLabel(LVMONERROR)
        /*  Load a pointer to the retValue argument */
        retValue = *((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)));
        target   = ((TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)));
        /*  Assign the result value to the target argument, and */
        /*  to the error handler variable for this current Lambda. */
        onErrorHandler = retValue;
        if (retValue.Tag == TYERROR) goto NestedReturn;
        *target = retValue;
		goto Fetch;
        break;
        
    case  VMAPPLY:
		setLabel(LVMAPPLY)
        /*  Load the address of each of the three operands */
        m = n       = *(Ip++);
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+1));Ip+=2;

        /*  Pass original arguments through */
        /*  Note:   For speed of execution, this code has been duplicated in several */
        /*          locations. Any change to this code should also be accompanied by */
        /*          similar changes in the all locations, which currently are: */ 
		if (m < 0)
			{
			n = argc + m;
			for (m = 0; m < n; ++m) gTP->TvalStack[TopOfStack++] = argv[m];
			m = n;
			}
		else
			{
			for (; m < argc; ++m) gTP->TvalStack[TopOfStack++] = argv[m];
			m = n = (argc - n);
			}
		goto CallDetail;
        break;

    case  VMPOP:
		setLabel(LVMPOP)
        if (pcode.u.Am1 != AMVOID) *((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++))) = gTP->TvalStack[--TopOfStack];
        if (pcode.u.Am2 != AMVOID) *((TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++))) = gTP->TvalStack[--TopOfStack];
        if (pcode.u.Am3 != AMVOID) *((TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip++))) = gTP->TvalStack[--TopOfStack];
		goto Fetch;
        break;
        
    case  VMPUSH:
		setLabel(LVMPUSH)
        if (pcode.u.Am1 != AMVOID) gTP->TvalStack[TopOfStack++] = *((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)));
        if (pcode.u.Am2 != AMVOID) gTP->TvalStack[TopOfStack++] = *((TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)));
        if (pcode.u.Am3 != AMVOID) gTP->TvalStack[TopOfStack++] = *((TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip++)));
		goto Fetch;
        break;
        
    case  VMCALL:
		setLabel(LVMCALL)
        /*  Load the address of each of the three operands */
        m = n       = *(Ip++);
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+1));Ip+=2;

        /*  Call the source procedure */
        /*  Note:   For speed of execution, this code has been duplicated in several */
        /*          locations. Any change to this code should also be accompanied by */
        /*          similar changes in the all locations, which currently are: */ 
        /*  FSmartbase_Eval         */
        /*  FSmartbase_Evalv        */
        /*  FVmScript_Emulator          */
        CallDetail:
		ThrowOnEscape;
        switch (source->Tag)
            {
            case TYCPROCEDURE:
                oldSi = TopOfStack;
                _TObject_RecordFrame
                retValue = (*(asSymbol(source)->itsCProcedure))(gCP,gTP,n,&gTP->TvalStack[TopOfStack-n]);
                _TObject_CheckFrame
                TopOfStack = oldSi - m;
				Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)(LpBIND)&argv[0].u.Structure->itsDictionaryArray;
				Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)(LpBIND)&Pv->itsDictionaryArray;
				Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)(LpBIND)&Cv->itsDictionaryArray;
                CallErrorMgr:
                /*  If the result value is an error, we need to construct the error tree. */
                if ((retValue.Tag == TYERROR) && (strcmp(ErrorArray(retValue),ErrorArray(gCP->TObject_ERROR_CONTINUATION)) != 0))
                    {
                    /*  Is this the first encounter with the error? */
                    /*  If yes, we need to create a new error tree. */
                    if (gTP->TObject_ErrorSwt == FALSE)
                        {
                        gCP->FSmartbase_errorMsg->itsGlobalValue = retValue;
                        gCP->FSmartbase_errorSym->itsGlobalValue = gTP->FVmscript_isource = TINT(0);
                        gCP->FSmartbase_errorSym->itsGlobalValue = gTP->TObject_ErrorTree = FMake_Vector(gCP,gTP,1,&gTP->FVmscript_isource);
                        gTP->TObject_ErrorSwt = TRUE;
                        TVector_AddNewValue(gCP,gTP,gTP->TObject_ErrorTree,retValue);
                        }
                        
                    /*  We now add the called procedure to the error tree. */
                    gTP->FVmscript_isource = *source;
                    TVector_AddNewValue(gCP,gTP,gTP->TObject_ErrorTree,gTP->FVmscript_isource);
					}
                else
                if ((retValue.Tag == TYERROR) && (strcmp(ErrorArray(retValue),ErrorArray(gCP->TObject_ERROR_CONTINUATION)) == 0))
                    {
                    gTP->FVmscript_NestedDebugSwt = FALSE;
                    goto NestedReturn;
                    }
                break;

            case TYCFUNCTION:
                oldSi = TopOfStack;
                _TObject_RecordFrame
                retValue = (*asFunction(source))(gCP,gTP,n,&gTP->TvalStack[TopOfStack-n]);
                _TObject_CheckFrame
                TopOfStack = oldSi - m;
				Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)(LpBIND)&argv[0].u.Structure->itsDictionaryArray;
				Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)(LpBIND)&Pv->itsDictionaryArray;
				Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)(LpBIND)&Cv->itsDictionaryArray;
                break;

            case TYLAMBDA:
                oldSi = TopOfStack;
                _TObject_RecordFrame
                retValue = _VmEvaluate(source->u.Lambda,n,&gTP->TvalStack[TopOfStack-n]);
                _TObject_CheckFrame
                TopOfStack = oldSi - m;
				Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)(LpBIND)&argv[0].u.Structure->itsDictionaryArray;
				Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)(LpBIND)&Pv->itsDictionaryArray;
				Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)(LpBIND)&Cv->itsDictionaryArray;
				break;

            case TYCONTINUATION:
                oldSi = TopOfStack;
                _TObject_RecordFrame
                retValue = TContinuation_Evaluate(gCP,gTP,*source, n,&gTP->TvalStack[TopOfStack-n]);
                _TObject_CheckFrame
                TopOfStack = oldSi - m;
				Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)(LpBIND)&argv[0].u.Structure->itsDictionaryArray;
				Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)(LpBIND)&Pv->itsDictionaryArray;
				Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)(LpBIND)&Cv->itsDictionaryArray;
                break;

            case TYMACRO:
                oldSi = TopOfStack;
                _TObject_RecordFrame
                retValue = _VmEvaluate(source->u.Lambda,n,&gTP->TvalStack[TopOfStack-n]);
                _TObject_CheckFrame
                TopOfStack = oldSi - m;
				Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)(LpBIND)&argv[0].u.Structure->itsDictionaryArray;
				Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)(LpBIND)&Pv->itsDictionaryArray;
				Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)(LpBIND)&Cv->itsDictionaryArray;
                break;

            case TYERROR:
                TopOfStack -= m;
                retValue = *source;
                goto CallErrorMgr;
                break;

            default:
                TopOfStack -= m;
				if ((pcode.u.Pcode == VMCALL) && (pcode.u.Am2 == AMGVOFFSET))
					{
					retValue.u.Int = (NUM)source - Rp[AMGVOFFSET].u.Int;
					retValue.Tag = TYSYMBOL;
					strcpy(gTP->TempBuffer,"!vmcall: unknown function name [");
					strcat(gTP->TempBuffer,SymbolArray(retValue));
					strcat(gTP->TempBuffer,"]!");
					retValue = TERROR(gTP->TempBuffer); 
					TSymbol_MakeUnique(gCP,gTP,"_errorMsg")->itsGlobalValue = retValue;
					}
				else
 					retValue = gCP->FVmScript_ERROR_MISSING_FUNCTION_NAME;
                goto CallErrorMgr;
                break;
            }

        /*  Check for error result value from call */
		ThrowOnEscape;
        if (retValue.Tag == TYERROR) goto NestedReturn;
		switch (target->DeclaredType)
			{
			case TYCHARPOINTER:
			case TYFLOATPOINTER:
			case TYINTPOINTER:
			case TYJUMPPOINTER:
			case TYREALPOINTER:
			case TYLONGPOINTER:
			case TYSHORTPOINTER:
			case TYWORDPOINTER:
				if (retValue.Tag > TYWORDPOINTER)
					{
					target->u.Int = ((retValue.Tag < TYTEXT) ? NIL : ((retValue.Tag == TYTEXT) ? (NUM)&retValue.u.Text[0] : (NUM)*retValue.u.Lambda->itsNilArray));
					target->Tag = TYNUM;
					}
				else
					{
					*target = retValue;
					}
				break;

			default:
				*target = retValue;
				break;
			}
        break;
        
    case  VMSEND:
		setLabel(LVMSEND)
        /*  Load the address of each of the three operands */
        m = n       = *(Ip++);
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+1));Ip+=2;
    
        /*  Load the source message procedure by combining the */
        /*  type of the first argument (receiver) and the symbol (source operand) */
        /*  to look up the procedure in the methods table for the type. */
        /*  Note:   If a quoted symbol is the message, then the method */
        /*          Procedure is chosen from the parents methods Dictionary. */
		ThrowOnEscape;
        if ((source->Tag == TYSYMBOL) && (n >= 1))
            { 
            /*  Load the type of the receiving argument. */
            gTP->FVmscript_type = gTP->TvalStack[TopOfStack-n].Tag;
			/*  Modify the send if the target object has properties */
			/*  which may may take prescedence over the class methods */
			switch (gTP->FVmscript_type)
				{
				case TYSTRUCTURE:
					/*  If receiver is a Structure, with no methods, and the message matches    */ 
					/*  one of its attributes, then the matching attribute is invoked.          */
					/*  If receiver is a Structure, with non-empty methods, then if the message */
					/*	matches a message of the receiver, the method is invoked.               */
					if (gTP->TvalStack[TopOfStack-n].u.Structure->itsMethods != NIL)
						{
						gTP->FVmscript_iargument = TOBJ(gTP->TvalStack[TopOfStack-n].u.Structure->itsMethods);

						/*  Load the method from the methods Structure for the receiving type. */
						/*  Use the hidden index position as a hint in quick search. */
						if ((inRange(asMemo(source),0,Structure(gTP->FVmscript_iargument)->itsMaxItemIndex)) &&
							(BindArray(gTP->FVmscript_iargument)[asMemo(source)].Key == source->u.Object))
							{
							gTP->FVmscript_isource = BindArray(gTP->FVmscript_iargument)[asMemo(source)].Value;
							}
						else
							{
							gTP->FVmscript_iindex = TStructure_SearchKey(gCP,gTP,(TStructure*)gTP->FVmscript_iargument.u.Object, source->u.Object, (short*)((NUM)&gTP->FVmscript_Selection));
							if (gTP->FVmscript_Selection != 0)
								{
								goto TryStructureElement;
								}
							else
								{
								gTP->FVmscript_isource = BindArray(gTP->FVmscript_iargument)[gTP->FVmscript_iindex.u.Int].Value;
								asMemo(source) = gTP->FVmscript_iindex.u.Int;
								}
							}
						source = &gTP->FVmscript_isource;
						if (source->Tag == TYERROR)
							goto BadSendValue;
						else
							goto CallDetail;
						}
					else
						{
						TryStructureElement:
						gTP->FVmscript_iargument = TStructure_GetIV1(gCP,gTP,gTP->TvalStack[TopOfStack-n],*source);
						if ((gTP->FVmscript_iargument.Tag != TYVOID) && (gTP->FVmscript_iargument.Tag != TYERROR))
							{
							/* Call the attribute of the receiver directly instead using the class method. */
							/* Note: Do not pass the receiver as an argument. */
							gTP->FVmscript_isource = gTP->FVmscript_iargument;
							source = &gTP->FVmscript_isource;
							--n;
							goto CallDetail;
							}
						}

					goto UseClassMethod;
					break;
				
				case TYLAMBDA:
					/*  If receiver is an Lambda, and the message matches one of its  child Lambdas, */
					/*	then the matching child Lambda takes prescedence over any class methods. */
					/*	Does the message match a child Lambda of the receiver? */
					gTP->FVmscript_iargument = TLambda_GetPvMember(gCP,gTP,gTP->TvalStack[TopOfStack-n],SymbolArray(*source));
					if (gTP->FVmscript_iargument.Tag != TYVOID)
						{
						/* Call the child of the receiver directly instead of using the class method. */
						/* Note: Do not pass the receiver as an argument. */
						gTP->FVmscript_isource = gTP->FVmscript_iargument;
						source = &gTP->FVmscript_isource;
						--n;
						goto CallDetail;
						}
					gTP->FVmscript_iargument = TLambda_GetCvMember(gCP,gTP,gTP->TvalStack[TopOfStack-n],SymbolArray(*source));
					if (gTP->FVmscript_iargument.Tag != TYVOID)
						{
						/* Call the child of the receiver directly instead of using the class method. */
						/* Note: Do not pass the receiver as an argument. */
						gTP->FVmscript_isource = gTP->FVmscript_iargument;
						source = &gTP->FVmscript_isource;
						--n;
						goto CallDetail;
						}
					goto UseClassMethod;
					break;
				
				case TYVECTOR:
					/*  If receiver is a Vector, and the message matches one of its attributes, */
					/*	then the matching attribute takes prescedence over any class methods. */
					/*	Is the message an attribute of the receiver? */
					gTP->FVmscript_iargument = TVector_GetIV1(gCP,gTP,gTP->TvalStack[TopOfStack-n],*source);
					if ((gTP->FVmscript_iargument.Tag != TYVOID) && (gTP->FVmscript_iargument.Tag != TYERROR))
						{
						/* Call the attribute of the receiver directly instead using the class method. */
						/* Note: Do not pass the receiver as an argument. */
						gTP->FVmscript_isource = gTP->FVmscript_iargument;
						source = &gTP->FVmscript_isource;
						--n;
						goto CallDetail;
						}
					goto UseClassMethod;
					break;
				
				case TYDICTIONARY:
					/*  If receiver is a Dictionary, and the message matches one of its attributes, */
					/*	then the matching attribute takes prescedence over any class methods. */
					/*	Is the message an attribute of the receiver? */
					gTP->FVmscript_iargument = TDictionary_GetIV1(gCP,gTP,gTP->TvalStack[TopOfStack-n],*source);
					if ((gTP->FVmscript_iargument.Tag != TYVOID) && (gTP->FVmscript_iargument.Tag != TYERROR))
						{
						/* Call the attribute of the receiver directly instead using the class method. */
						/* Note: Do not pass the receiver as an argument. */
						gTP->FVmscript_isource = gTP->FVmscript_iargument;
						source = &gTP->FVmscript_isource;
						--n;
						goto CallDetail;
						}
					goto UseClassMethod;
					break;
				
				case TYDIRECTORY:
					/*  If receiver is a Directory, and the message matches one of its attributes, */
					/*	then the matching attribute takes prescedence over any class methods. */
					/*	Is the message an attribute of the receiver? */
					gTP->FVmscript_iargument = TDirectory_GetIV1(gCP,gTP,gTP->TvalStack[TopOfStack-n],*source);
					if ((gTP->FVmscript_iargument.Tag != TYVOID) && (gTP->FVmscript_iargument.Tag != TYERROR))
						{
						/* Call the attribute of the receiver directly instead using the class method. */
						/* Note: Do not pass the receiver as an argument. */
						gTP->FVmscript_isource = gTP->FVmscript_iargument;
						source = &gTP->FVmscript_isource;
						--n;
						goto CallDetail;
						}
					goto UseClassMethod;
					break;
				
				default:
					UseClassMethod:
					/*  Load the methods Dictionary for the receiving type. */
					if ((gTP->FVmscript_type == TYSTRUCTURE) && (Structure(gTP->TvalStack[TopOfStack-n])->itsMethods != NIL))
						{
						gTP->FVmscript_isource = TOBJ(Structure(gTP->TvalStack[TopOfStack-n])->itsMethods->itsUserTypeMethods);
						}
					else
						{
						gTP->FVmscript_isource = _TObject_TypeMethods(gTP->FVmscript_type);
						}
					break;
				}	/* end switch */
            }	/* end if */
		else
            { 
            BadSendValue:
            if (source->Tag == TYERROR)
                retValue = *source;
            else
                retValue = gCP->FVmScript_ERROR_VMSEND_MESSAGE;
            goto NestedReturn;
            }	/* end else */

        /*  If the methods Structure is empty, issue an error. */
        if (gTP->FVmscript_isource.Tag != TYSTRUCTURE)
            {
            goto TryDefaultGlobalMethod;
            }
        /*  Load the method from the methods Structure for the receiving type. */
        /*  Use the hidden index position as a hint in quick search. */
        if ((inRange(asMemo(source),0,Structure(gTP->FVmscript_isource)->itsMaxItemIndex)) &&
            (BindArray(gTP->FVmscript_isource)[asMemo(source)].Key == source->u.Object))
            {
            gTP->FVmscript_isource = BindArray(gTP->FVmscript_isource)[asMemo(source)].Value;
            }
        else
            {
            gTP->FVmscript_iindex = TStructure_SearchKey(gCP,gTP,(TStructure*)gTP->FVmscript_isource.u.Object, source->u.Object, (short*)((NUM)&gTP->FVmscript_Selection));
            if (gTP->FVmscript_Selection != 0)
                {
				TryDefaultGlobalMethod:
				gTP->FVmscript_isource = source->u.Symbol->itsGlobalValue;
				if (gTP->FVmscript_isource.Tag == TYVOID)
					{
					retValue = gCP->FVmScript_ERROR_VMSEND_MESSAGE;
					goto NestedReturn;
					}
                }
            else
                {
                gTP->FVmscript_isource = BindArray(gTP->FVmscript_isource)[gTP->FVmscript_iindex.u.Int].Value;
                asMemo(source) = gTP->FVmscript_iindex.u.Int;
                }
            }
        source = &gTP->FVmscript_isource;
        if (source->Tag == TYERROR)
            goto BadSendValue;
        else
            goto CallDetail;
        break;
        
    case  vmnatJmpLEInteger:
		setLabel(LvmnatJmpLEInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int <= argument->u.Int) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpLTInteger:
		setLabel(LvmnatJmpLTInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int < argument->u.Int) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpEQInteger:
		setLabel(LvmnatJmpEQInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int == argument->u.Int) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpNEInteger:
		setLabel(LvmnatJmpNEInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int != argument->u.Int) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpGEInteger:
		setLabel(LvmnatJmpGEInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int >= argument->u.Int) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpGTInteger:
		setLabel(LvmnatJmpGTInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int > argument->u.Int) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

        
    case  vmnatJmpLEUInteger:
		setLabel(LvmnatJmpLEUInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt <= argument->u.UInt) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpLTUInteger:
		setLabel(LvmnatJmpLTUInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt < argument->u.UInt) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpEQUInteger:
		setLabel(LvmnatJmpEQUInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt == argument->u.UInt) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpNEUInteger:
		setLabel(LvmnatJmpNEUInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt != argument->u.UInt) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpGEUInteger:
		setLabel(LvmnatJmpGEUInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt >= argument->u.UInt) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpGTUInteger:
		setLabel(LvmnatJmpGTUInteger)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt > argument->u.UInt) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpLENumber:
		setLabel(LvmnatJmpLENumber)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real <= argument->u.Real) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpLTNumber:
		setLabel(LvmnatJmpLTNumber)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real < argument->u.Real) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpEQNumber:
		setLabel(LvmnatJmpEQNumber)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real == argument->u.Real) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpNENumber:
		setLabel(LvmnatJmpNENumber)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real != argument->u.Real) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpGENumber:
		setLabel(LvmnatJmpGENumber)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real >= argument->u.Real) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatJmpGTNumber:
		setLabel(LvmnatJmpGTNumber)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real > argument->u.Real) Ip = (NUM*)&pcodes[n];
		goto Fetch;
        break;

    case  vmnatAndInteger:
		setLabel(LvmnatAndInteger)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int & argument->u.Int;
		goto Fetch;
        break;

    case  vmnatOrInteger:
		setLabel(LvmnatOrInteger)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int | argument->u.Int;
		goto Fetch;
        break;

    case  vmnatShlInteger:
		setLabel(LvmnatShlInteger)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int << argument->u.Int;
		goto Fetch;
        break;

    case  vmnatShrInteger:
		setLabel(LvmnatShrInteger)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int >> argument->u.Int;
		goto Fetch;
        break;

    case  vmnatXorInteger:
		setLabel(LvmnatXorInteger)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int ^ argument->u.Int;
		goto Fetch;
        break;

    case  vmnatAddInteger:
		setLabel(LvmnatAddInteger)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int + argument->u.Int;
		goto Fetch;
        break;

    case  vmnatAddNumber:
		setLabel(LvmnatAddNumber)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Real = source->u.Real + argument->u.Real;
		goto Fetch;
        break;

    case  vmnatDivInteger:
		setLabel(LvmnatDivInteger)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int / argument->u.Int;
		goto Fetch;
        break;

    case  vmnatDivNumber:
		setLabel(LvmnatDivNumber)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Real = source->u.Real / argument->u.Real;
		goto Fetch;
        break;

    case  vmnatDivrInteger:
		setLabel(LvmnatDivrInteger)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int % argument->u.Int;
		goto Fetch;
        break;

    case  vmnatDivrNumber:
		setLabel(LvmnatDivrNumber)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        gTP->FVmscript_Fraction = modf(source->u.Real / argument->u.Real, &gTP->FVmscript_Integer);
        target->u.Real = argument->u.Real * gTP->FVmscript_Fraction;
		goto Fetch;
        break;

    case  vmnatMulInteger:
		setLabel(LvmnatMulInteger)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int * argument->u.Int;
		goto Fetch;
        break;

    case  vmnatMulNumber:
		setLabel(LvmnatMulNumber)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Real = source->u.Real * argument->u.Real;
		goto Fetch;
        break;

    case  vmnatSubInteger:
		setLabel(LvmnatSubInteger)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int - argument->u.Int;
		goto Fetch;
        break;

    case  vmnatSubNumber:
		setLabel(LvmnatSubNumber)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Real = source->u.Real - argument->u.Real;
		goto Fetch;
        break;

    case  vmnatLoadCharacter:
		setLabel(LvmnatLoadCharacter)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Int = *((LpCHAR)source);
		target->Tag = TYNUM;
		goto Fetch;
        break;

    case  vmnatLoadFloat:
		setLabel(LvmnatLoadFloat)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Real = *((LpFLOAT)source);
		target->Tag = TYREAL;
		goto Fetch;
        break;

    case  vmnatLoadInteger:
		setLabel(LvmnatLoadInteger)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Int = *((LpNUM)source);
		target->Tag = TYNUM;
		goto Fetch;
        break;

    case  vmnatLoadUInteger:
		setLabel(LvmnatLoadUInteger)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.UInt = *((LpUNUM)source);
		target->Tag = TYUNUM;
		goto Fetch;
        break;

    case  vmnatLoadLong:
		setLabel(LvmnatLoadLong)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Int = *((LpNUM32)source);
		target->Tag = TYNUM;
		goto Fetch;
        break;

    case  vmnatLoadNumber:
		setLabel(LvmnatLoadNumber)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Real = *((LpREAL)source);
		target->Tag = TYREAL;
		goto Fetch;
        break;

    case  vmnatLoadObject:
		setLabel(LvmnatLoadObject)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Obj = *((LpOBJ)source);
		target->Tag = (target->u.Obj == NIL) ? TYVOID : target->u.Object->itsObjectType;
		goto Fetch;
        break;

    case  vmnatLoadShort:
		setLabel(LvmnatLoadShort)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Int = *((LpSHORT)source);
		target->Tag = TYNUM;
		goto Fetch;
        break;

    case  vmnatSaveCharacter:
		setLabel(LvmnatSaveCharacter)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpCHAR)target) = (char)source->u.Int;
		goto Fetch;
        break;

    case  vmnatSaveFloat:
		setLabel(LvmnatSaveFloat)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpFLOAT)target) = (FLOAT)source->u.Real;
		goto Fetch;
        break;

    case  vmnatSaveInteger:
		setLabel(LvmnatSaveInteger)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpNUM)target) = (NUM)source->u.Int;
		goto Fetch;
        break;

    case  vmnatSaveLong:
		setLabel(LvmnatSaveLong)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpNUM32)target) = (NUM32)source->u.Int;
		goto Fetch;
        break;

    case  vmnatSaveNumber:
		setLabel(LvmnatSaveNumber)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpREAL)target) = (REAL)source->u.Real;
		goto Fetch;
        break;

    case  vmnatSaveObject:
		setLabel(LvmnatSaveObject)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpOBJ)target) = (source->Tag == TYVOID) ? NIL : source->u.Obj;
		goto Fetch;
        break;

    case  vmnatSaveShort:
		setLabel(LvmnatSaveShort)
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpSHORT)target) = (SHORT)source->u.Int;
		goto Fetch;
        break;

    case  VMJMPLE:
		setLabel(LVMJMPLE)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag <= argument->Tag)
                    Ip = (NUM*)&pcodes[n];
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
                    Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYBYTEVECTOR:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						if (source->u.Object == argument->u.Object) 
							Ip = (NUM*)&pcodes[n];
						else
							{
							srcP = ByteArray(*source);
							argP = ByteArray(*argument);
							i = ByteVector(*source)->itsMaxItemIndex;
							k = ByteVector(*argument)->itsMaxItemIndex;
							m = (i < k) ? i : k;
							VMJMPLEBYTVEC:
							i = *srcP++;
							k = *argP++;
							if (i > k) goto VMJMPLEBYTVEC_CONTINUE;
							if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
							if (--m > 0) goto VMJMPLEBYTVEC;
							if (ByteVector(*source)->itsMaxItemIndex < ByteVector(*argument)->itsMaxItemIndex) 
								{Ip = (NUM*)&pcodes[n]; break;}
							if (ByteVector(*source)->itsMaxItemIndex > ByteVector(*argument)->itsMaxItemIndex) goto VMJMPLEBYTVEC_CONTINUE;
							if (FPredicate2_QuickCompare(gCP,gTP,&ByteVector(*source)->itsCdr,&ByteVector(*argument)->itsCdr) <= 0)
								{Ip = (NUM*)&pcodes[n]; break;}
							VMJMPLEBYTVEC_CONTINUE:
							break;
							}
						break;

					case TYSTRING:
						srcP = ByteArray(*source);
						argP = CharArray(*argument);
						goto VMJMPLECOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = ByteArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPLECOMPARE;
						break;
				
					case TYTEXT:
						srcP = ByteArray(*source);
						argP = argument->u.Text;
						goto VMJMPLECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = SymbolArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPLECOMPARE;
						break;

					case TYSTRING:
						srcP = SymbolArray(*source);
						argP = CharArray(*argument);
						goto VMJMPLECOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						if (source->u.Symbol == argument->u.Symbol)
							{Ip = (NUM*)&pcodes[n]; break;}
						srcP = SymbolArray(*source);
						argP = CharArray(*argument);
						goto VMJMPLECOMPARE;
						break;
				
					case TYTEXT:
						srcP = SymbolArray(*source);
						argP = argument->u.Text;
						goto VMJMPLECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSTRING:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = CharArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPLECOMPARE;
						break;

					case TYSTRING:
						if (source->u.Object == argument->u.Object) 
							Ip = (NUM*)&pcodes[n];
						else
							{
							srcP = CharArray(*source);
							argP = CharArray(*argument);
							VMJMPLECOMPARE:
							VMJMPLESTRNGSTRNG:
							i = *srcP++;
							k = *argP++;
							if (i > k) goto VMJMPLESTRNGSTRNG_CONTINUE;
							if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
							if (i != 0) goto VMJMPLESTRNGSTRNG;
							Ip = (NUM*)&pcodes[n];
							VMJMPLESTRNGSTRNG_CONTINUE:
							break;
							}
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = CharArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPLECOMPARE;
						break;
				
					case TYTEXT:
						srcP = CharArray(*source);
						argP = argument->u.Text;
						goto VMJMPLECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYTEXT:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = source->u.Text;
						argP = ByteArray(*argument);
						goto VMJMPLECOMPARE;
						break;

					case TYSTRING:
						srcP = source->u.Text;
						argP = CharArray(*argument);
						goto VMJMPLECOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = source->u.Text;
						argP = SymbolArray(*argument);
						goto VMJMPLECOMPARE;
						break;
				
					case TYTEXT:
						srcP = source->u.Text;
						argP = argument->u.Text;
						goto VMJMPLECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYBITVECTOR:
				if (argument->Tag == TYBITVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						srcP = ByteArray(*source);
						argP = ByteArray(*argument);
						i = BitVector(*source)->itsMaxItemIndex;
						k = BitVector(*argument)->itsMaxItemIndex;
						m = (((i < k) ? i : k)+7)/8;
						VMJMPLEBITVEC:
						i = *srcP++;
						k = *argP++;
						if (i > k) goto VMJMPLEBITVEC_CONTINUE;
						if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPLEBITVEC;
						if (BitVector(*source)->itsMaxItemIndex < BitVector(*argument)->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (BitVector(*source)->itsMaxItemIndex > BitVector(*argument)->itsMaxItemIndex) goto VMJMPLEBITVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&BitVector(*source)->itsCdr,&BitVector(*argument)->itsCdr) <= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPLEBITVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYINTVECTOR:
				if (argument->Tag == TYINTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpNUM srcP = IntArray(*source);
						LpNUM argP = IntArray(*argument);
						i = IntVector(*source)->itsMaxItemIndex;
						k = IntVector(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPLEINTVEC:
						i = *srcP++;
						k = *argP++;
						if (i > k) goto VMJMPLEINTVEC_CONTINUE;
						if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPLEINTVEC;
						if (IntVector(*source)->itsMaxItemIndex < IntVector(*argument)->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (IntVector(*source)->itsMaxItemIndex > IntVector(*argument)->itsMaxItemIndex) goto VMJMPLEINTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&IntVector(*source)->itsCdr,&IntVector(*argument)->itsCdr) <= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPLEINTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYSHORTVECTOR:
				if (argument->Tag == TYSHORTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpSHORT srcP = ShortArray(*source);
						LpSHORT argP = ShortArray(*argument);
						i = source->u.ShortVector->itsMaxItemIndex;
						k = argument->u.ShortVector->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPLESHTVEC:
						i = *srcP++;
						k = *argP++;
						if (i > k) goto VMJMPLESHTVEC_CONTINUE;
						if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPLESHTVEC;
						if (source->u.ShortVector->itsMaxItemIndex < argument->u.ShortVector->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (source->u.ShortVector->itsMaxItemIndex > argument->u.ShortVector->itsMaxItemIndex) goto VMJMPLESHTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&source->u.ShortVector->itsCdr,&argument->u.ShortVector->itsCdr) <= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPLESHTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYLONGVECTOR:
				if (argument->Tag == TYLONGVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpNUM32 srcP = LongArray(*source);
						LpNUM32 argP = LongArray(*argument);
						i = source->u.ShortVector->itsMaxItemIndex;
						k = argument->u.ShortVector->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPLELONGVEC:
						i = *srcP++;
						k = *argP++;
						if (i > k) goto VMJMPLELONGVEC_CONTINUE;
						if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPLELONGVEC;
						if (source->u.LongVector->itsMaxItemIndex < argument->u.LongVector->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (source->u.LongVector->itsMaxItemIndex > argument->u.LongVector->itsMaxItemIndex) goto VMJMPLELONGVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&source->u.LongVector->itsCdr,&argument->u.LongVector->itsCdr) <= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPLELONGVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpFLOAT srcP = FloatArray(*source); //TM was LpREAL
						LpFLOAT argP = FloatArray(*argument); //TM was LpREAL
						i = source->u.FltVector->itsMaxItemIndex;
						k = argument->u.FltVector->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPLEFLTVEC:
						si = *srcP++;
						ai = *argP++;
						if (si > ai) goto VMJMPLEFLTVEC_CONTINUE;
						if (ai < ai) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPLEFLTVEC;
						if (FltVector(*source)->itsMaxItemIndex < FltVector(*argument)->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (FltVector(*source)->itsMaxItemIndex > FltVector(*argument)->itsMaxItemIndex) goto VMJMPLEFLTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&FltVector(*source)->itsCdr,&FltVector(*argument)->itsCdr) <= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPLEFLTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMVECTOR:
				if (argument->Tag == TYNUMVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpREAL srcP = RealArray(*source);
						LpREAL argP = RealArray(*argument);
						i = NumVector(*source)->itsMaxItemIndex;
						k = NumVector(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPLENUMVEC:
						si = *srcP++;
						ai = *argP++;
						if (si > ai) goto VMJMPLENUMVEC_CONTINUE;
						if (ai < ai) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPLENUMVEC;
						if (NumVector(*source)->itsMaxItemIndex < NumVector(*argument)->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (NumVector(*source)->itsMaxItemIndex > NumVector(*argument)->itsMaxItemIndex) goto VMJMPLENUMVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&NumVector(*source)->itsCdr,&NumVector(*argument)->itsCdr) <= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPLENUMVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMMATRIX:
				if (argument->Tag == TYNUMMATRIX)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpREAL srcP = RealArray(*source);
						LpREAL argP = RealArray(*argument);
						i = NumMatrix(*source)->itsMaxItemIndex;
						k = NumMatrix(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPLENUMMAT:
						si = *srcP++;
						ai = *argP++;
						if (si > ai) goto VMJMPLENUMMAT_CONTINUE;
						if (ai < ai) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPLENUMMAT;
						if (NumMatrix(*source)->itsMaxItemIndex < NumMatrix(*argument)->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (NumMatrix(*source)->itsMaxItemIndex > NumMatrix(*argument)->itsMaxItemIndex) goto VMJMPLENUMMAT_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&NumMatrix(*source)->itsCdr,&NumMatrix(*argument)->itsCdr) <= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPLENUMMAT_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt <= argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt <= argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.UInt <= argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt <= argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt <  cp->itsReal ||
						   ((REAL)source->u.UInt == cp->itsReal && 0.0 <= cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag <= argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;

			case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int <= argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Int <= argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.Int <= argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int <= argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int <  cp->itsReal ||
						   ((REAL)source->u.Int == cp->itsReal && 0.0 <= cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag <= argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Real <= argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Real <= argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real <= argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real <= argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;
					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real <  cp->itsReal ||
						   (source->u.Real == cp->itsReal && 0.0 <= cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag <= argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char <= argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Char <= argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.Char <= argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char <= argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char <  cp->itsReal ||
						   ((REAL)source->u.Char == cp->itsReal && 0.0 <= cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
        
					case TYBYTEVECTOR:
					case TYTEXT:
					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
					case TYSTRING:
					case TYSPECIALFORM:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
							Ip = (NUM*)&pcodes[n];
						break;

                    default:
                        if (source->Tag <= argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool <= argument->u.Bool)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag <= argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;

			case TYCPX:
				cp = source->u.Complex;
				switch (argument->Tag)
				{
                case TYUNUM:
                    if (cp->itsReal <  (REAL)argument->u.UInt ||
					   (cp->itsReal == (REAL)argument->u.UInt  && cp->itsImag <= 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYNUM:
                    if (cp->itsReal <  (REAL)argument->u.Int ||
					   (cp->itsReal == (REAL)argument->u.Int  && cp->itsImag <= 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if (cp->itsReal <  argument->u.Real ||
					   (cp->itsReal == argument->u.Real  && cp->itsImag <= 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYCHAR:
                    if (cp->itsReal <  (REAL)argument->u.Char ||
					   (cp->itsReal == (REAL)argument->u.Char && cp->itsImag <= 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

				case TYCPX:
					if (cp->itsReal <  argument->u.Complex->itsReal ||
					   (cp->itsReal == argument->u.Complex->itsReal &&
					    cp->itsImag <= argument->u.Complex->itsImag))
                        Ip = (NUM*)&pcodes[n];
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag <= argument->Tag)
                        Ip = (NUM*)&pcodes[n];
                    break;
				}
				break;

			default:
 				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
                if (isCompareLE(&retValue))
                    Ip = (NUM*)&pcodes[n];
                break;
            }
        break;

    case  VMJMPLT:
		setLabel(LVMJMPLT)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag < argument->Tag)
                    Ip = (NUM*)&pcodes[n];
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
                    Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYBYTEVECTOR:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = ByteArray(*source);
						argP = ByteArray(*argument);
						i = ByteVector(*source)->itsMaxItemIndex;
						k = ByteVector(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPLTBYTVEC:
						i = *srcP++;
						k = *argP++;
						if (i > k) goto VMJMPLTBYTVEC_CONTINUE;
						if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPLTBYTVEC;
						if (ByteVector(*source)->itsMaxItemIndex < ByteVector(*argument)->itsMaxItemIndex) 
							{Ip = (NUM*)&pcodes[n]; break;}
						if (ByteVector(*source)->itsMaxItemIndex > ByteVector(*argument)->itsMaxItemIndex) goto VMJMPLTBYTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&ByteVector(*source)->itsCdr,&ByteVector(*argument)->itsCdr) < 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPLTBYTVEC_CONTINUE:
						break;

					case TYSTRING:
						srcP = ByteArray(*source);
						argP = CharArray(*argument);
						goto VMJMPLTCOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = ByteArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPLTCOMPARE;
						break;
				
					case TYTEXT:
						srcP = ByteArray(*source);
						argP = argument->u.Text;
						goto VMJMPLTCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = SymbolArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPLTCOMPARE;
						break;

					case TYSTRING:
						srcP = SymbolArray(*source);
						argP = CharArray(*argument);
						goto VMJMPLTCOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = SymbolArray(*source);
						argP = CharArray(*argument);
						goto VMJMPLTCOMPARE;
						break;
				
					case TYTEXT:
						srcP = SymbolArray(*source);
						argP = argument->u.Text;
						goto VMJMPLTCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSTRING:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = CharArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPLTCOMPARE;
						break;

					case TYSTRING:
						srcP = CharArray(*source);
						argP = CharArray(*argument);
						VMJMPLTCOMPARE:
						VMJMPLTSTRNGSTRNG:
						i = *srcP++;
						k = *argP++;
						if (i > k) goto VMJMPLTSTRNGSTRNG_CONTINUE;
						if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
						if (i != 0) goto VMJMPLTSTRNGSTRNG;
						VMJMPLTSTRNGSTRNG_CONTINUE:
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = CharArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPLTCOMPARE;
						break;
				
					case TYTEXT:
						srcP = CharArray(*source);
						argP = argument->u.Text;
						goto VMJMPLTCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYTEXT:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = source->u.Text;
						argP = ByteArray(*argument);
						goto VMJMPLTCOMPARE;
						break;

					case TYSTRING:
						srcP = source->u.Text;
						argP = CharArray(*argument);
						goto VMJMPLTCOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = source->u.Text;
						argP = SymbolArray(*argument);
						goto VMJMPLTCOMPARE;
						break;
				
					case TYTEXT:
						srcP = source->u.Text;
						argP = argument->u.Text;
						goto VMJMPLTCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYBITVECTOR:
				if (argument->Tag == TYBITVECTOR)
					{
					srcP = ByteArray(*source);
					argP = ByteArray(*argument);
					i = BitVector(*source)->itsMaxItemIndex;
					k = BitVector(*argument)->itsMaxItemIndex;
					m = (((i < k) ? i : k) + 7)/8;
					VMJMPLTBITVEC:
					i = *srcP++;
					k = *argP++;
					if (i > k) goto VMJMPLTBITVEC_CONTINUE;
					if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPLTBITVEC;
					if (BitVector(*source)->itsMaxItemIndex < BitVector(*argument)->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (BitVector(*source)->itsMaxItemIndex > BitVector(*argument)->itsMaxItemIndex) goto VMJMPLTBITVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&BitVector(*source)->itsCdr,&BitVector(*argument)->itsCdr) < 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPLTBITVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYINTVECTOR:
				if (argument->Tag == TYINTVECTOR)
					{
					LpNUM srcP = IntArray(*source);
					LpNUM argP = IntArray(*argument);
					i = IntVector(*source)->itsMaxItemIndex;
					k = IntVector(*argument)->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPLTINTVEC:
					i = *srcP++;
					k = *argP++;
					if (i > k) goto VMJMPLTINTVEC_CONTINUE;
					if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPLTINTVEC;
					if (IntVector(*source)->itsMaxItemIndex < IntVector(*argument)->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (IntVector(*source)->itsMaxItemIndex > IntVector(*argument)->itsMaxItemIndex) goto VMJMPLTINTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&IntVector(*source)->itsCdr,&IntVector(*argument)->itsCdr) < 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPLTINTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYSHORTVECTOR:
				if (argument->Tag == TYSHORTVECTOR)
					{
					LpSHORT srcP = ShortArray(*source);
					LpSHORT argP = ShortArray(*argument);
					i = source->u.ShortVector->itsMaxItemIndex;
					k = argument->u.ShortVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPLTSHTVEC:
					i = *srcP++;
					k = *argP++;
					if (i > k) goto VMJMPLTSHTVEC_CONTINUE;
					if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPLTSHTVEC;
					if (source->u.ShortVector->itsMaxItemIndex < argument->u.ShortVector->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (source->u.ShortVector->itsMaxItemIndex > argument->u.ShortVector->itsMaxItemIndex) goto VMJMPLTSHTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.ShortVector->itsCdr,&argument->u.ShortVector->itsCdr) < 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPLTSHTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYLONGVECTOR:
				if (argument->Tag == TYLONGVECTOR)
					{
					LpNUM32 srcP = LongArray(*source);
					LpNUM32 argP = LongArray(*argument);
					i = source->u.LongVector->itsMaxItemIndex;
					k = argument->u.LongVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPLTLONGVEC:
					i = *srcP++;
					k = *argP++;
					if (i > k) goto VMJMPLTLONGVEC_CONTINUE;
					if (i < k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPLTLONGVEC;
					if (source->u.LongVector->itsMaxItemIndex < argument->u.LongVector->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (source->u.LongVector->itsMaxItemIndex > argument->u.LongVector->itsMaxItemIndex) goto VMJMPLTLONGVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.LongVector->itsCdr,&argument->u.LongVector->itsCdr) < 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPLTLONGVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					LpFLOAT srcP = FloatArray(*source); //TM was LpREAL
					LpFLOAT argP = FloatArray(*argument); //TM was LpREAL
					i = source->u.FltVector->itsMaxItemIndex;
					k = argument->u.FltVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPLTFLTVEC:
					si = *srcP++;
					ai = *argP++;
					if (si > ai) goto VMJMPLTFLTVEC_CONTINUE;
					if (si < ai) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPLTFLTVEC;
					if (FltVector(*source)->itsMaxItemIndex < FltVector(*argument)->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (FltVector(*source)->itsMaxItemIndex > FltVector(*argument)->itsMaxItemIndex) goto VMJMPLTFLTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&FltVector(*source)->itsCdr,&FltVector(*argument)->itsCdr) < 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPLTFLTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMVECTOR:
				if (argument->Tag == TYNUMVECTOR)
					{
					LpREAL srcP = RealArray(*source);
					LpREAL argP = RealArray(*argument);
					i = NumVector(*source)->itsMaxItemIndex;
					k = NumVector(*argument)->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPLTNUMVEC:
					si = *srcP++;
					ai = *argP++;
					if (si > ai) goto VMJMPLTNUMVEC_CONTINUE;
					if (si < ai) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPLTNUMVEC;
					if (NumVector(*source)->itsMaxItemIndex < NumVector(*argument)->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (NumVector(*source)->itsMaxItemIndex > NumVector(*argument)->itsMaxItemIndex) goto VMJMPLTNUMVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&NumVector(*source)->itsCdr,&NumVector(*argument)->itsCdr) < 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPLTNUMVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMMATRIX:
				if (argument->Tag == TYNUMMATRIX)
					{
					LpREAL srcP = RealArray(*source);
					LpREAL argP = RealArray(*argument);
					i = NumMatrix(*source)->itsMaxItemIndex;
					k = NumMatrix(*argument)->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPLTNUMMAT:
					si = *srcP++;
					ai = *argP++;
					if (si > ai) goto VMJMPLTNUMMAT_CONTINUE;
					if (si < ai) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPLTNUMMAT;
					if (NumMatrix(*source)->itsMaxItemIndex < NumMatrix(*argument)->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (NumMatrix(*source)->itsMaxItemIndex > NumMatrix(*argument)->itsMaxItemIndex) goto VMJMPLTNUMMAT_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&NumMatrix(*source)->itsCdr,&NumMatrix(*argument)->itsCdr) < 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPLTNUMMAT_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt < argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt < argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.UInt < argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt < argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt < cp->itsReal ||
							((REAL)source->u.UInt == cp->itsReal && 0.0 < cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag < argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                                
            case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int < argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Int < argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Int < argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int < argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int < cp->itsReal ||
							((REAL)source->u.Int == cp->itsReal && 0.0 < cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag < argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Real < argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Real < argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real < argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real < argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;
					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real < cp->itsReal ||
							(source->u.Real == cp->itsReal && 0.0 < cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag < argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char < argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Char < argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Char < argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char < argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char < cp->itsReal ||
							((REAL)source->u.Char == cp->itsReal && 0.0 < cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
					case TYBYTEVECTOR:
					case TYTEXT:
					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
					case TYSTRING:
					case TYSPECIALFORM:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
							Ip = (NUM*)&pcodes[n];
						break;

                  default:
                        if (source->Tag < argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool < argument->u.Bool)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag < argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;

			case TYCPX:
				cp = source->u.Complex;
				switch (argument->Tag)
				{
                case TYUNUM:
                    if (cp->itsReal <  (REAL)argument->u.UInt ||
					   (cp->itsReal == (REAL)argument->u.UInt && cp->itsImag < 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYNUM:
                    if (cp->itsReal <  (REAL)argument->u.Int ||
					   (cp->itsReal == (REAL)argument->u.Int && cp->itsImag < 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if (cp->itsReal <  argument->u.Real ||
					   (cp->itsReal == argument->u.Real && cp->itsImag < 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYCHAR:
                    if (cp->itsReal <  (REAL)argument->u.Char ||
					   (cp->itsReal == (REAL)argument->u.Char && cp->itsImag < 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

				case TYCPX:
					if (cp->itsReal <  argument->u.Complex->itsReal ||
					   (cp->itsReal == argument->u.Complex->itsReal &&
					    cp->itsImag < argument->u.Complex->itsImag))
                        Ip = (NUM*)&pcodes[n];
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag < argument->Tag)
                        Ip = (NUM*)&pcodes[n];
                    break;
				}
				break;

			default:
 				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
                if (isCompareLT(&retValue))
                    Ip = (NUM*)&pcodes[n];
                break;
            }
        break;

    case  VMJMPEQ:
		setLabel(LVMJMPEQ)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag == argument->Tag)
                    Ip = (NUM*)&pcodes[n];
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
                    Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYBYTEVECTOR:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						if (source->u.Object == argument->u.Object) 
							Ip = (NUM*)&pcodes[n];
						else
							{
							srcP = ByteArray(*source);
							argP = ByteArray(*argument);
							i = ByteVector(*source)->itsMaxItemIndex;
							k = ByteVector(*argument)->itsMaxItemIndex;
							m = (i < k) ? i : k;
							VMJMPEQBYTVEC:
							i = *srcP++;
							k = *argP++;
							if (i != k) goto VMJMPEQBYTVEC_CONTINUE;
							if (--m > 0) goto VMJMPEQBYTVEC;
							if ((ByteVector(*source)->itsMaxItemIndex == ByteVector(*argument)->itsMaxItemIndex) &&
								(((ByteVector(*source)->itsCdr.Tag == TYVOID) && (ByteVector(*argument)->itsCdr.Tag == TYVOID)) ||
								 (FPredicate2_QuickCompare(gCP,gTP,&ByteVector(*source)->itsCdr,&ByteVector(*argument)->itsCdr) == 0)))
								Ip = (NUM*)&pcodes[n];
							VMJMPEQBYTVEC_CONTINUE:
							break;
							}
						break;

					case TYSTRING:
						srcP = ByteArray(*source);
						argP = CharArray(*argument);
						goto VMJMPEQCOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = ByteArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPEQCOMPARE;
						break;
				
					case TYTEXT:
						srcP = ByteArray(*source);
						argP = argument->u.Text;
						goto VMJMPEQCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = SymbolArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPEQCOMPARE;
						break;

					case TYSTRING:
						srcP = SymbolArray(*source);
						argP = CharArray(*argument);
						goto VMJMPEQCOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						if (source->u.Symbol == argument->u.Symbol)
							Ip = (NUM*)&pcodes[n];
						break;
				
					case TYTEXT:
						srcP = SymbolArray(*source);
						argP = argument->u.Text;
						goto VMJMPEQCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSTRING:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = CharArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPEQCOMPARE;
						break;

					case TYSTRING:
						if (source->u.Object == argument->u.Object) 
							Ip = (NUM*)&pcodes[n];
						else
							{
							srcP = CharArray(*source);
							argP = CharArray(*argument);
							VMJMPEQCOMPARE:
							VMJMPEQSTRNGSTRNG:
							i = *srcP++;
							k = *argP++;
							if (i != k) goto VMJMPEQSTRNGSTRNG_CONTINUE;
							if (i != 0) goto VMJMPEQSTRNGSTRNG;
							Ip = (NUM*)&pcodes[n];
							VMJMPEQSTRNGSTRNG_CONTINUE:
							break;
							}
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = CharArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPEQCOMPARE;
						break;
				
					case TYTEXT:
						srcP = CharArray(*source);
						argP = argument->u.Text;
						goto VMJMPEQCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYTEXT:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = source->u.Text;
						argP = ByteArray(*argument);
						goto VMJMPEQCOMPARE;
						break;

					case TYSTRING:
						srcP = source->u.Text;
						argP = CharArray(*argument);
						goto VMJMPEQCOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = source->u.Text;
						argP = SymbolArray(*argument);
						goto VMJMPEQCOMPARE;
						break;
				
					case TYTEXT:
						srcP = source->u.Text;
						argP = argument->u.Text;
						goto VMJMPEQCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYBITVECTOR:
				if (argument->Tag == TYBITVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						srcP = ByteArray(*source);
						argP = ByteArray(*argument);
						i = BitVector(*source)->itsMaxItemIndex;
						k = BitVector(*argument)->itsMaxItemIndex;
						m = (((i < k) ? i : k) + 7)/8;
						VMJMPEQBITVEC:
						i = *srcP++;
						k = *argP++;
						if (i != k) goto VMJMPEQBITVEC_CONTINUE;
						if (--m > 0) goto VMJMPEQBITVEC;
						if ((BitVector(*source)->itsMaxItemIndex == BitVector(*argument)->itsMaxItemIndex) &&
							(((BitVector(*source)->itsCdr.Tag == TYVOID) && (BitVector(*argument)->itsCdr.Tag == TYVOID)) ||
							 (FPredicate2_QuickCompare(gCP,gTP,&BitVector(*source)->itsCdr,&BitVector(*argument)->itsCdr) == 0)))
							Ip = (NUM*)&pcodes[n];
						VMJMPEQBITVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYINTVECTOR:
				if (argument->Tag == TYINTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpNUM srcP = IntArray(*source);
						LpNUM argP = IntArray(*argument);
						i = IntVector(*source)->itsMaxItemIndex;
						k = IntVector(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPEQINTVEC:
						i = *srcP++;
						k = *argP++;
						if (i != k) goto VMJMPEQINTVEC_CONTINUE;
						if (--m > 0) goto VMJMPEQINTVEC;
						if ((IntVector(*source)->itsMaxItemIndex == IntVector(*argument)->itsMaxItemIndex) &&
							(((IntVector(*source)->itsCdr.Tag == TYVOID) && (IntVector(*argument)->itsCdr.Tag == TYVOID)) ||
							 (FPredicate2_QuickCompare(gCP,gTP,&IntVector(*source)->itsCdr,&IntVector(*argument)->itsCdr) == 0)))
							Ip = (NUM*)&pcodes[n];
						VMJMPEQINTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYSHORTVECTOR:
				if (argument->Tag == TYSHORTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpSHORT srcP = ShortArray(*source);
						LpSHORT argP = ShortArray(*argument);
						i = source->u.ShortVector->itsMaxItemIndex;
						k = argument->u.ShortVector->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPEQSHTVEC:
						i = *srcP++;
						k = *argP++;
						if (i != k) goto VMJMPEQSHTVEC_CONTINUE;
						if (--m > 0) goto VMJMPEQSHTVEC;
						if ((source->u.ShortVector->itsMaxItemIndex == argument->u.ShortVector->itsMaxItemIndex) &&
							(((source->u.ShortVector->itsCdr.Tag == TYVOID) && (argument->u.ShortVector->itsCdr.Tag == TYVOID)) ||
							 (FPredicate2_QuickCompare(gCP,gTP,&source->u.ShortVector->itsCdr,&argument->u.ShortVector->itsCdr) == 0)))
							Ip = (NUM*)&pcodes[n];
						VMJMPEQSHTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYLONGVECTOR:
				if (argument->Tag == TYLONGVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpNUM32 srcP = LongArray(*source);
						LpNUM32 argP = LongArray(*argument);
						i = source->u.LongVector->itsMaxItemIndex;
						k = argument->u.LongVector->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPEQLONGVEC:
						i = *srcP++;
						k = *argP++;
						if (i != k) goto VMJMPEQLONGVEC_CONTINUE;
						if (--m > 0) goto VMJMPEQLONGVEC;
						if ((source->u.LongVector->itsMaxItemIndex == argument->u.LongVector->itsMaxItemIndex) &&
							(((source->u.LongVector->itsCdr.Tag == TYVOID) && (argument->u.LongVector->itsCdr.Tag == TYVOID)) ||
							 (FPredicate2_QuickCompare(gCP,gTP,&source->u.LongVector->itsCdr,&argument->u.LongVector->itsCdr) == 0)))
							Ip = (NUM*)&pcodes[n];
						VMJMPEQLONGVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpFLOAT srcP = FloatArray(*source); //TM was LpREAL
						LpFLOAT argP = FloatArray(*argument); //TM was LpREAL
						i = source->u.FltVector->itsMaxItemIndex;
						k = argument->u.FltVector->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPEQFLTVEC:
						si = *srcP++;
						ai = *argP++;
						if (si != ai) goto VMJMPEQFLTVEC_CONTINUE;
						if (--m > 0) goto VMJMPEQFLTVEC;
						if ((source->u.FltVector->itsMaxItemIndex == argument->u.FltVector->itsMaxItemIndex) &&
							(((source->u.FltVector->itsCdr.Tag == TYVOID) && (argument->u.FltVector->itsCdr.Tag == TYVOID)) ||
							 (FPredicate2_QuickCompare(gCP,gTP,&source->u.FltVector->itsCdr,&argument->u.FltVector->itsCdr) == 0)))
							Ip = (NUM*)&pcodes[n];
						VMJMPEQFLTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMVECTOR:
				if (argument->Tag == TYNUMVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpREAL srcP = RealArray(*source);
						LpREAL argP = RealArray(*argument);
						i = NumVector(*source)->itsMaxItemIndex;
						k = NumVector(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPEQNUMVEC:
						si = *srcP++;
						ai = *argP++;
						if (si != ai) goto VMJMPEQNUMVEC_CONTINUE;
						if (--m > 0) goto VMJMPEQNUMVEC;
						if ((NumVector(*source)->itsMaxItemIndex == NumVector(*argument)->itsMaxItemIndex) &&
							(((NumVector(*source)->itsCdr.Tag == TYVOID) && (NumVector(*argument)->itsCdr.Tag == TYVOID)) ||
							 (FPredicate2_QuickCompare(gCP,gTP,&NumVector(*source)->itsCdr,&NumVector(*argument)->itsCdr) == 0)))
							Ip = (NUM*)&pcodes[n];
						VMJMPEQNUMVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMMATRIX:
				if (argument->Tag == TYNUMMATRIX)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpREAL srcP = RealArray(*source);
						LpREAL argP = RealArray(*argument);
						i = NumMatrix(*source)->itsMaxItemIndex;
						k = NumMatrix(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPEQNUMMAT:
						si = *srcP++;
						ai = *argP++;
						if (si != ai) goto VMJMPEQNUMMAT_CONTINUE;
						if (--m > 0) goto VMJMPEQNUMMAT;
						if ((NumMatrix(*source)->itsMaxItemIndex == NumMatrix(*argument)->itsMaxItemIndex) &&
							(((NumMatrix(*source)->itsCdr.Tag == TYVOID) && (NumMatrix(*argument)->itsCdr.Tag == TYVOID)) ||
							 (FPredicate2_QuickCompare(gCP,gTP,&NumMatrix(*source)->itsCdr,&NumMatrix(*argument)->itsCdr) == 0)))
							Ip = (NUM*)&pcodes[n];
						VMJMPEQNUMMAT_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt == argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt == argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.UInt == argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt == argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt == cp->itsReal && 0.0 == cp->itsImag)
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag == argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;

			case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int == argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Int == argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.Int == argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int == argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int == cp->itsReal && 0.0 == cp->itsImag)
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag == argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Real == argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Real == argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real == argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real == argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real == cp->itsReal && 0.0 == cp->itsImag)
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;

                    default:
                        if (source->Tag == argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char == argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Char == argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Char == argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char == argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char == cp->itsReal && 0.0 == cp->itsImag)
                            Ip = (NUM*)&pcodes[n];
						break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag == argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool == argument->u.Bool)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag == argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;

			case TYCPX:
				cp = source->u.Complex;
				switch (argument->Tag)
				{
                case TYUNUM:
                    if (cp->itsReal == (REAL)argument->u.UInt && cp->itsImag == 0.0)
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYNUM:
                    if (cp->itsReal == (REAL)argument->u.Int && cp->itsImag == 0.0)
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if ((cp->itsReal == argument->u.Real) && (cp->itsImag == 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYCHAR:
                    if (cp->itsReal == (REAL)argument->u.Char && cp->itsImag == 0.0)
                        Ip = (NUM*)&pcodes[n];
                    break;

				case TYCPX:
					if ((cp->itsReal == argument->u.Complex->itsReal) && (cp->itsImag == argument->u.Complex->itsImag))
                        Ip = (NUM*)&pcodes[n];
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag == argument->Tag)
                        Ip = (NUM*)&pcodes[n];
                    break;
				}
				break;

			default:
 				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
                if (isCompareEQ(&retValue))
                    Ip = (NUM*)&pcodes[n];
                break;
            }
        break;

	case  VMJMPNE:
		setLabel(LVMJMPNE)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag != argument->Tag)
                    Ip = (NUM*)&pcodes[n];
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
                    Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYBYTEVECTOR:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = ByteArray(*source);
						argP = ByteArray(*argument);
						i = ByteVector(*source)->itsMaxItemIndex;
						k = ByteVector(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPNEBYTVEC:
						i = *srcP++;
						k = *argP++;
						if (i != k) {Ip = (NUM*)&pcodes[n]; break;} 
						if (--m > 0) goto VMJMPNEBYTVEC;
						if (ByteVector(*source)->itsMaxItemIndex != ByteVector(*argument)->itsMaxItemIndex) 
							{Ip = (NUM*)&pcodes[n]; break;}
						if (FPredicate2_QuickCompare(gCP,gTP,&ByteVector(*source)->itsCdr,&ByteVector(*argument)->itsCdr) != 0) 
							{Ip = (NUM*)&pcodes[n]; break;}
						break;

					case TYSTRING:
						srcP = ByteArray(*source);
						argP = CharArray(*argument);
						goto VMJMPNECOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = ByteArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPNECOMPARE;
						break;
				
					case TYTEXT:
						srcP = ByteArray(*source);
						argP = argument->u.Text;
						goto VMJMPNECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = SymbolArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPNECOMPARE;
						break;

					case TYSTRING:
						srcP = SymbolArray(*source);
						argP = CharArray(*argument);
						goto VMJMPNECOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						if (source->u.Symbol != argument->u.Symbol)
							Ip = (NUM*)&pcodes[n];
						break;
				
					case TYTEXT:
						srcP = SymbolArray(*source);
						argP = argument->u.Text;
						goto VMJMPNECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSTRING:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = CharArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPNECOMPARE;
						break;

					case TYSTRING:
						srcP = CharArray(*source);
						argP = CharArray(*argument);
						VMJMPNECOMPARE:
						VMJMPNESTRNGSTRNG:
						i = *srcP++;
						k = *argP++;
						if (i != k) {Ip = (NUM*)&pcodes[n]; break;}
						if (i != 0) goto VMJMPNESTRNGSTRNG;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = CharArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPNECOMPARE;
						break;
				
					case TYTEXT:
						srcP = CharArray(*source);
						argP = argument->u.Text;
						goto VMJMPNECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYTEXT:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = source->u.Text;
						argP = ByteArray(*argument);
						goto VMJMPNECOMPARE;
						break;

					case TYSTRING:
						srcP = source->u.Text;
						argP = CharArray(*argument);
						goto VMJMPNECOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = source->u.Text;
						argP = SymbolArray(*argument);
						goto VMJMPNECOMPARE;
						break;
				
					case TYTEXT:
						srcP = source->u.Text;
						argP = argument->u.Text;
						goto VMJMPNECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYBITVECTOR:
				if (argument->Tag == TYBITVECTOR)
					{
					srcP = ByteArray(*source);
					argP = ByteArray(*argument);
					i = BitVector(*source)->itsMaxItemIndex;
					k = BitVector(*argument)->itsMaxItemIndex;
					m = (((i < k) ? i : k) + 7)/8;
					VMJMPNEBITVEC:
					i = *srcP++;
					k = *argP++;
					if (i != k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPNEBITVEC;
					if (BitVector(*source)->itsMaxItemIndex != BitVector(*argument)->itsMaxItemIndex) 
						{Ip = (NUM*)&pcodes[n]; break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&BitVector(*source)->itsCdr,&BitVector(*argument)->itsCdr) != 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYINTVECTOR:
				if (argument->Tag == TYINTVECTOR)
					{
					LpNUM srcP = IntArray(*source);
					LpNUM argP = IntArray(*argument);
					i = IntVector(*source)->itsMaxItemIndex;
					k = IntVector(*argument)->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPNEINTVEC:
					i = *srcP++;
					k = *argP++;
					if (i != k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPNEINTVEC;
					if (IntVector(*source)->itsMaxItemIndex != IntVector(*argument)->itsMaxItemIndex) 
						{Ip = (NUM*)&pcodes[n]; break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&IntVector(*source)->itsCdr,&IntVector(*argument)->itsCdr) != 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYSHORTVECTOR:
				if (argument->Tag == TYSHORTVECTOR)
					{
					LpSHORT srcP = ShortArray(*source);
					LpSHORT argP = ShortArray(*argument);
					i = source->u.ShortVector->itsMaxItemIndex;
					k = argument->u.ShortVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPNESHTVEC:
					i = *srcP++;
					k = *argP++;
					if (i != k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPNESHTVEC;
					if (source->u.ShortVector->itsMaxItemIndex != argument->u.ShortVector->itsMaxItemIndex) 
						{Ip = (NUM*)&pcodes[n]; break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.ShortVector->itsCdr,&argument->u.ShortVector->itsCdr) != 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYLONGVECTOR:
				if (argument->Tag == TYLONGVECTOR)
					{
					LpNUM32 srcP = LongArray(*source);
					LpNUM32 argP = LongArray(*argument);
					i = source->u.LongVector->itsMaxItemIndex;
					k = argument->u.LongVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPNELONGVEC:
					i = *srcP++;
					k = *argP++;
					if (i != k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPNELONGVEC;
					if (source->u.LongVector->itsMaxItemIndex != argument->u.LongVector->itsMaxItemIndex) 
						{Ip = (NUM*)&pcodes[n]; break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.LongVector->itsCdr,&argument->u.LongVector->itsCdr) != 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					LpFLOAT srcP = FloatArray(*source); //TM was LpREAL
					LpFLOAT argP = FloatArray(*argument); //TM was LpREAL
					i = source->u.FltVector->itsMaxItemIndex;
					k = argument->u.FltVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPNEFLTVEC:
					si = *srcP++;
					ai = *argP++;
					if (si != ai) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPNEFLTVEC;
					if (source->u.FltVector->itsMaxItemIndex != argument->u.FltVector->itsMaxItemIndex) 
						{Ip = (NUM*)&pcodes[n]; break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.FltVector->itsCdr,&argument->u.FltVector->itsCdr) != 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMVECTOR:
				if (argument->Tag == TYNUMVECTOR)
					{
					LpREAL srcP = RealArray(*source);
					LpREAL argP = RealArray(*argument);
					i = NumVector(*source)->itsMaxItemIndex;
					k = NumVector(*argument)->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPNENUMVEC:
					si = *srcP++;
					ai = *argP++;
					if (si != ai) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPNENUMVEC;
					if (source->u.NumVector->itsMaxItemIndex != argument->u.NumVector->itsMaxItemIndex) 
						{Ip = (NUM*)&pcodes[n]; break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.NumVector->itsCdr,&argument->u.NumVector->itsCdr) != 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMMATRIX:
				if (argument->Tag == TYNUMMATRIX)
					{
					LpREAL srcP = RealArray(*source);
					LpREAL argP = RealArray(*argument);
					i = NumMatrix(*source)->itsMaxItemIndex;
					k = NumMatrix(*argument)->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPNENUMMAT:
					si = *srcP++;
					ai = *argP++;
					if (si != ai) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPNENUMMAT;
					if (source->u.NumMatrix->itsMaxItemIndex != argument->u.NumMatrix->itsMaxItemIndex) 
						{Ip = (NUM*)&pcodes[n]; break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.NumMatrix->itsCdr,&argument->u.NumMatrix->itsCdr) != 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt != argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt != argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.UInt != argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt != argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt != cp->itsReal || 0.0 != cp->itsImag)
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag != argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                                
            case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int != argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Int != argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Int != argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int != argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int != cp->itsReal || 0.0 != cp->itsImag)
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag != argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Real != argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Real != argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real != argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real != argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real != cp->itsReal || 0.0 != cp->itsImag)
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag != argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char != argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Char != argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Char != argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char != argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char != cp->itsReal || 0.0 != cp->itsImag)
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag != argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool != argument->u.Bool)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag != argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;

			case TYCPX:
				cp = source->u.Complex;
				switch (argument->Tag)
				{
                case TYUNUM:
                    if (cp->itsReal != (REAL)argument->u.UInt || cp->itsImag != 0.0)
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYNUM:
                    if (cp->itsReal != (REAL)argument->u.Int || cp->itsImag != 0.0)
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if (cp->itsReal != argument->u.Real || cp->itsImag != 0.0)
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYCHAR:
                    if (cp->itsReal != (REAL)argument->u.Char || cp->itsImag != 0.0)
                        Ip = (NUM*)&pcodes[n];
                    break;

				case TYCPX:
					if (cp->itsReal != argument->u.Complex->itsReal ||
						cp->itsImag != argument->u.Complex->itsImag)
                        Ip = (NUM*)&pcodes[n];
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag != argument->Tag)
                        Ip = (NUM*)&pcodes[n];
                    break;
				}
				break;

			default:
				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
				if (isCompareNE(&retValue))
					Ip = (NUM*)&pcodes[n];
                break;
            }
        break;

	case  VMJMPGE:
		setLabel(LVMJMPGE)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag >= argument->Tag)
                    Ip = (NUM*)&pcodes[n];
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
                    Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYBYTEVECTOR:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						if (source->u.Object == argument->u.Object) 
							Ip = (NUM*)&pcodes[n];
						else
							{
							srcP = ByteArray(*source);
							argP = ByteArray(*argument);
							i = ByteVector(*source)->itsMaxItemIndex;
							k = ByteVector(*argument)->itsMaxItemIndex;
							m = (i < k) ? i : k;
							VMJMPGEBYTVEC:
							i = *srcP++;
							k = *argP++;
							if (i < k) goto VMJMPGEBYTVEC_CONTINUE;
							if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
							if (--m > 0) goto VMJMPGEBYTVEC;
							if (ByteVector(*source)->itsMaxItemIndex > ByteVector(*argument)->itsMaxItemIndex) 
								{Ip = (NUM*)&pcodes[n]; break;}
							if (ByteVector(*source)->itsMaxItemIndex < ByteVector(*argument)->itsMaxItemIndex) goto VMJMPGEBYTVEC_CONTINUE;
							if (FPredicate2_QuickCompare(gCP,gTP,&ByteVector(*source)->itsCdr,&ByteVector(*argument)->itsCdr) >= 0)
								{Ip = (NUM*)&pcodes[n]; break;}
							VMJMPGEBYTVEC_CONTINUE:
							break;
							}
						break;

					case TYSTRING:
						srcP = ByteArray(*source);
						argP = CharArray(*argument);
						goto VMJMPGECOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = ByteArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPGECOMPARE;
						break;
				
					case TYTEXT:
						srcP = ByteArray(*source);
						argP = argument->u.Text;
						goto VMJMPGECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = SymbolArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPGECOMPARE;
						break;

					case TYSTRING:
						srcP = SymbolArray(*source);
						argP = CharArray(*argument);
						goto VMJMPGECOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						if (source->u.Symbol == argument->u.Symbol)
							{Ip = (NUM*)&pcodes[n]; break;}
						srcP = SymbolArray(*source);
						argP = CharArray(*argument);
						goto VMJMPGECOMPARE;
						break;
				
					case TYTEXT:
						srcP = SymbolArray(*source);
						argP = argument->u.Text;
						goto VMJMPGECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSTRING:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = CharArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPGECOMPARE;
						break;

					case TYSTRING:
						if (source->u.Object == argument->u.Object) 
							Ip = (NUM*)&pcodes[n];
						else
							{
							srcP = CharArray(*source);
							argP = CharArray(*argument);
							VMJMPGECOMPARE:
							VMJMPGESTRNGSTRNG:
							i = *srcP++;
							k = *argP++;
							if (i < k) goto VMJMPGESTRNGSTRNG_CONTINUE;
							if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
							if (i != 0) goto VMJMPGESTRNGSTRNG;
							Ip = (NUM*)&pcodes[n];
							VMJMPGESTRNGSTRNG_CONTINUE:
							break;
							}
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = CharArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPGECOMPARE;
						break;
				
					case TYTEXT:
						srcP = CharArray(*source);
						argP = argument->u.Text;
						goto VMJMPGECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYTEXT:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = source->u.Text;
						argP = ByteArray(*argument);
						goto VMJMPGECOMPARE;
						break;

					case TYSTRING:
						srcP = source->u.Text;
						argP = CharArray(*argument);
						goto VMJMPGECOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = source->u.Text;
						argP = SymbolArray(*argument);
						goto VMJMPGECOMPARE;
						break;
				
					case TYTEXT:
						srcP = source->u.Text;
						argP = argument->u.Text;
						goto VMJMPGECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYBITVECTOR:
				if (argument->Tag == TYBITVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						srcP = ByteArray(*source);
						argP = ByteArray(*argument);
						i = BitVector(*source)->itsMaxItemIndex;
						k = BitVector(*argument)->itsMaxItemIndex;
						m = (((i < k) ? i : k) + 7)/8;
						VMJMPGEBITVEC:
						i = *srcP++;
						k = *argP++;
						if (i < k) goto VMJMPGEBITVEC_CONTINUE;
						if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPGEBITVEC;
						if (BitVector(*source)->itsMaxItemIndex > BitVector(*argument)->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (BitVector(*source)->itsMaxItemIndex < BitVector(*argument)->itsMaxItemIndex) goto VMJMPGEBITVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&BitVector(*source)->itsCdr,&BitVector(*argument)->itsCdr) >= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPGEBITVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYINTVECTOR:
				if (argument->Tag == TYINTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpNUM srcP = IntArray(*source);
						LpNUM argP = IntArray(*argument);
						i = IntVector(*source)->itsMaxItemIndex;
						k = IntVector(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPGEINTVEC:
						i = *srcP++;
						k = *argP++;
						if (i < k) goto VMJMPGEINTVEC_CONTINUE;
						if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPGEINTVEC;
						if (IntVector(*source)->itsMaxItemIndex > IntVector(*argument)->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (IntVector(*source)->itsMaxItemIndex < IntVector(*argument)->itsMaxItemIndex) goto VMJMPGEINTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&IntVector(*source)->itsCdr,&IntVector(*argument)->itsCdr) >= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPGEINTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYSHORTVECTOR:
				if (argument->Tag == TYSHORTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpSHORT srcP = ShortArray(*source);
						LpSHORT argP = ShortArray(*argument);
						i = source->u.ShortVector->itsMaxItemIndex;
						k = argument->u.ShortVector->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPGESHTVEC:
						i = *srcP++;
						k = *argP++;
						if (i < k) goto VMJMPGESHTVEC_CONTINUE;
						if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPGESHTVEC;
						if (source->u.ShortVector->itsMaxItemIndex > argument->u.ShortVector->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (source->u.ShortVector->itsMaxItemIndex < argument->u.ShortVector->itsMaxItemIndex) goto VMJMPGESHTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&source->u.ShortVector->itsCdr,&argument->u.ShortVector->itsCdr) >= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPGESHTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYLONGVECTOR:
				if (argument->Tag == TYLONGVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpNUM32 srcP = LongArray(*source);
						LpNUM32 argP = LongArray(*argument);
						i = source->u.LongVector->itsMaxItemIndex;
						k = argument->u.LongVector->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPGELONGVEC:
						i = *srcP++;
						k = *argP++;
						if (i < k) goto VMJMPGELONGVEC_CONTINUE;
						if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPGELONGVEC;
						if (source->u.LongVector->itsMaxItemIndex > argument->u.LongVector->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (source->u.LongVector->itsMaxItemIndex < argument->u.LongVector->itsMaxItemIndex) goto VMJMPGELONGVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&source->u.LongVector->itsCdr,&argument->u.LongVector->itsCdr) >= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPGELONGVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpFLOAT srcP = FloatArray(*source); //TM was LpREAL
						LpFLOAT argP = FloatArray(*argument); //TM was LpREAL
						i = source->u.FltVector->itsMaxItemIndex;
						k = argument->u.FltVector->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPGEFLTVEC:
						si = *srcP++;
						ai = *argP++;
						if (si < ai) goto VMJMPGEFLTVEC_CONTINUE;
						if (si > ai) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPGEFLTVEC;
						if (FltVector(*source)->itsMaxItemIndex > FltVector(*argument)->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (FltVector(*source)->itsMaxItemIndex < FltVector(*argument)->itsMaxItemIndex) goto VMJMPGEFLTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&FltVector(*source)->itsCdr,&FltVector(*argument)->itsCdr) >= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPGEFLTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMVECTOR:
				if (argument->Tag == TYNUMVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpREAL srcP = RealArray(*source);
						LpREAL argP = RealArray(*argument);
						i = NumVector(*source)->itsMaxItemIndex;
						k = NumVector(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPGENUMVEC:
						si = *srcP++;
						ai = *argP++;
						if (si < ai) goto VMJMPGENUMVEC_CONTINUE;
						if (si > ai) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPGENUMVEC;
						if (NumVector(*source)->itsMaxItemIndex > NumVector(*argument)->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (NumVector(*source)->itsMaxItemIndex < NumVector(*argument)->itsMaxItemIndex) goto VMJMPGENUMVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&NumVector(*source)->itsCdr,&NumVector(*argument)->itsCdr) >= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPGENUMVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMMATRIX:
				if (argument->Tag == TYNUMMATRIX)
					{
					if (source->u.Object == argument->u.Object) 
						Ip = (NUM*)&pcodes[n];
					else
						{
						LpREAL srcP = RealArray(*source);
						LpREAL argP = RealArray(*argument);
						i = NumMatrix(*source)->itsMaxItemIndex;
						k = NumMatrix(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPGENUMMAT:
						si = *srcP++;
						ai = *argP++;
						if (si < ai) goto VMJMPGENUMMAT_CONTINUE;
						if (si > ai) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPGENUMMAT;
						if (NumMatrix(*source)->itsMaxItemIndex > NumMatrix(*argument)->itsMaxItemIndex)
							{Ip = (NUM*)&pcodes[n]; break;}
						if (NumMatrix(*source)->itsMaxItemIndex < NumMatrix(*argument)->itsMaxItemIndex) goto VMJMPGENUMMAT_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&NumMatrix(*source)->itsCdr,&NumMatrix(*argument)->itsCdr) >= 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPGENUMMAT_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt >= argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt >= argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.UInt >= argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt >= argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt >  cp->itsReal ||
						   ((REAL)source->u.UInt == cp->itsReal && 0.0 >= cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag >= argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                                
            case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int >= argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Int >= argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Int >= argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int >= argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int >  cp->itsReal ||
						   ((REAL)source->u.Int == cp->itsReal && 0.0 >= cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag >= argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Real >= (REAL)argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Real >= (REAL)argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real >= argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real >= (REAL)argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real >  cp->itsReal ||
						   (source->u.Real == cp->itsReal && 0.0 >= cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag >= argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char >= argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Char >= argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.Char >= argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char >= argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char  > cp->itsReal ||
						   ((REAL)source->u.Char == cp->itsReal && 0.0 >= cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag >= argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool >= argument->u.Bool)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag >= argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;

			case TYCPX:
				cp = source->u.Complex;
				switch (argument->Tag)
				{
                case TYUNUM:
                    if (cp->itsReal >  (REAL)argument->u.UInt ||
					   (cp->itsReal == (REAL)argument->u.UInt && cp->itsImag >= 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYNUM:
                    if (cp->itsReal >  (REAL)argument->u.Int ||
					   (cp->itsReal == (REAL)argument->u.Int && cp->itsImag >= 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if (cp->itsReal >  argument->u.Real ||
					   (cp->itsReal == argument->u.Real && cp->itsImag >= 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYCHAR:
                    if (cp->itsReal >= (REAL)argument->u.Char ||
					   (cp->itsReal == (REAL)argument->u.Char && cp->itsImag >= 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

				case TYCPX:
					if (cp->itsReal  > argument->u.Complex->itsReal ||
					   (source->u.Complex->itsReal == argument->u.Complex->itsReal &&
						source->u.Complex->itsImag >= argument->u.Complex->itsImag))
                        Ip = (NUM*)&pcodes[n];
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag != argument->Tag)
                        Ip = (NUM*)&pcodes[n];
                    break;
				}
				break;

			default:
 				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
                if (isCompareGE(&retValue))
                    Ip = (NUM*)&pcodes[n];
				break;
            }
        break;

	case  VMJMPGT:
		setLabel(LVMJMPGT)
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag > argument->Tag)
                    Ip = (NUM*)&pcodes[n];
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
                    Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYBYTEVECTOR:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = ByteArray(*source);
						argP = ByteArray(*argument);
						i = ByteVector(*source)->itsMaxItemIndex;
						k = ByteVector(*argument)->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPGTBYTVEC:
						i = *srcP++;
						k = *argP++;
						if (i < k) goto VMJMPGTBYTVEC_CONTINUE;
						if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
						if (--m > 0) goto VMJMPGTBYTVEC;
						if (ByteVector(*source)->itsMaxItemIndex > ByteVector(*argument)->itsMaxItemIndex) 
							{Ip = (NUM*)&pcodes[n]; break;}
						if (ByteVector(*source)->itsMaxItemIndex < ByteVector(*argument)->itsMaxItemIndex) goto VMJMPGTBYTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&ByteVector(*source)->itsCdr,&ByteVector(*argument)->itsCdr) > 0)
							{Ip = (NUM*)&pcodes[n]; break;}
						VMJMPGTBYTVEC_CONTINUE:
						break;

					case TYSTRING:
						srcP = ByteArray(*source);
						argP = CharArray(*argument);
						goto VMJMPGTCOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = ByteArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPGTCOMPARE;
						break;
				
					case TYTEXT:
						srcP = ByteArray(*source);
						argP = argument->u.Text;
						goto VMJMPGTCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = SymbolArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPGTCOMPARE;
						break;

					case TYSTRING:
						srcP = SymbolArray(*source);
						argP = CharArray(*argument);
						goto VMJMPGTCOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = SymbolArray(*source);
						argP = CharArray(*argument);
						goto VMJMPGTCOMPARE;
						break;
				
					case TYTEXT:
						srcP = SymbolArray(*source);
						argP = argument->u.Text;
						goto VMJMPGTCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYSTRING:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = CharArray(*source);
						argP = ByteArray(*argument);
						goto VMJMPGTCOMPARE;
						break;

					case TYSTRING:
						srcP = CharArray(*source);
						argP = CharArray(*argument);
						VMJMPGTCOMPARE:
						VMJMPGTSTRNGSTRNG:
						i = *srcP++;
						k = *argP++;
						if (i < k) goto VMJMPGTSTRNGSTRNG_CONTINUE;
						if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
						if (i != 0) goto VMJMPGTSTRNGSTRNG;
						VMJMPGTSTRNGSTRNG_CONTINUE:
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = CharArray(*source);
						argP = SymbolArray(*argument);
						goto VMJMPGTCOMPARE;
						break;
				
					case TYTEXT:
						srcP = CharArray(*source);
						argP = argument->u.Text;
						goto VMJMPGTCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYTEXT:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						srcP = source->u.Text;
						argP = ByteArray(*argument);
						goto VMJMPGTCOMPARE;
						break;

					case TYSTRING:
						srcP = source->u.Text;
						argP = CharArray(*argument);
						goto VMJMPGTCOMPARE;
						break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						srcP = source->u.Text;
						argP = SymbolArray(*argument);
						goto VMJMPGTCOMPARE;
						break;
				
					case TYTEXT:
						srcP = source->u.Text;
						argP = argument->u.Text;
						goto VMJMPGTCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
							Ip = (NUM*)&pcodes[n];
						break;
					}
                break;
                                
            case TYBITVECTOR:
				if (argument->Tag == TYBITVECTOR)
					{
					srcP = ByteArray(*source);
					argP = ByteArray(*argument);
					i = BitVector(*source)->itsMaxItemIndex;
					k = BitVector(*argument)->itsMaxItemIndex;
					m = (((i < k) ? i : k) + 7)/8;
					VMJMPGTBITVEC:
					i = *srcP++;
					k = *argP++;
					if (i < k) goto VMJMPGTBITVEC_CONTINUE;
					if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPGTBITVEC;
					if (BitVector(*source)->itsMaxItemIndex > BitVector(*argument)->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (BitVector(*source)->itsMaxItemIndex < BitVector(*argument)->itsMaxItemIndex) goto VMJMPGTBITVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&BitVector(*source)->itsCdr,&BitVector(*argument)->itsCdr) > 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPGTBITVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYINTVECTOR:
				if (argument->Tag == TYINTVECTOR)
					{
					LpNUM srcP = IntArray(*source);
					LpNUM argP = IntArray(*argument);
					i = IntVector(*source)->itsMaxItemIndex;
					k = IntVector(*argument)->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPGTINTVEC:
					i = *srcP++;
					k = *argP++;
					if (i < k) goto VMJMPGTINTVEC_CONTINUE;
					if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPGTINTVEC;
					if (IntVector(*source)->itsMaxItemIndex > IntVector(*argument)->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (IntVector(*source)->itsMaxItemIndex < IntVector(*argument)->itsMaxItemIndex) goto VMJMPGTINTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&IntVector(*source)->itsCdr,&IntVector(*argument)->itsCdr) > 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPGTINTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYSHORTVECTOR:
				if (argument->Tag == TYSHORTVECTOR)
					{
					LpSHORT srcP = ShortArray(*source);
					LpSHORT argP = ShortArray(*argument);
					i = source->u.ShortVector->itsMaxItemIndex;
					k = argument->u.ShortVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPGTSHTVEC:
					i = *srcP++;
					k = *argP++;
					if (i < k) goto VMJMPGTSHTVEC_CONTINUE;
					if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPGTSHTVEC;
					if (source->u.ShortVector->itsMaxItemIndex > argument->u.ShortVector->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (source->u.ShortVector->itsMaxItemIndex < argument->u.ShortVector->itsMaxItemIndex) goto VMJMPGTSHTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.ShortVector->itsCdr,&argument->u.ShortVector->itsCdr) > 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPGTSHTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYLONGVECTOR:
				if (argument->Tag == TYLONGVECTOR)
					{
					LpNUM32 srcP = LongArray(*source);
					LpNUM32 argP = LongArray(*argument);
					i = source->u.LongVector->itsMaxItemIndex;
					k = argument->u.LongVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPGTLONGVEC:
					i = *srcP++;
					k = *argP++;
					if (i < k) goto VMJMPGTLONGVEC_CONTINUE;
					if (i > k) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPGTLONGVEC;
					if (source->u.LongVector->itsMaxItemIndex > argument->u.LongVector->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (source->u.LongVector->itsMaxItemIndex < argument->u.LongVector->itsMaxItemIndex) goto VMJMPGTLONGVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.LongVector->itsCdr,&argument->u.LongVector->itsCdr) > 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPGTLONGVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					LpFLOAT srcP = FloatArray(*source); //TM was LpREAL
					LpFLOAT argP = FloatArray(*argument); //TM was LpREAL
					i = source->u.FltVector->itsMaxItemIndex;
					k = argument->u.FltVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPGTFLTVEC:
					si = *srcP++;
					ai = *argP++;
					if (si < ai) goto VMJMPGTFLTVEC_CONTINUE;
					if (si > ai) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPGTFLTVEC;
					if (FltVector(*source)->itsMaxItemIndex > FltVector(*argument)->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (FltVector(*source)->itsMaxItemIndex < FltVector(*argument)->itsMaxItemIndex) goto VMJMPGTFLTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&FltVector(*source)->itsCdr,&FltVector(*argument)->itsCdr) > 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPGTFLTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMVECTOR:
				if (argument->Tag == TYNUMVECTOR)
					{
					LpREAL srcP = RealArray(*source);
					LpREAL argP = RealArray(*argument);
					i = NumVector(*source)->itsMaxItemIndex;
					k = NumVector(*argument)->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPGTNUMVEC:
					si = *srcP++;
					ai = *argP++;
					if (si < ai) goto VMJMPGTNUMVEC_CONTINUE;
					if (si > ai) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPGTNUMVEC;
					if (NumVector(*source)->itsMaxItemIndex > NumVector(*argument)->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (NumVector(*source)->itsMaxItemIndex < NumVector(*argument)->itsMaxItemIndex) goto VMJMPGTNUMVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&NumVector(*source)->itsCdr,&NumVector(*argument)->itsCdr) > 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPGTNUMVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYNUMMATRIX:
				if (argument->Tag == TYNUMMATRIX)
					{
					LpREAL srcP = RealArray(*source);
					LpREAL argP = RealArray(*argument);
					i = NumMatrix(*source)->itsMaxItemIndex;
					k = NumMatrix(*argument)->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPGTNUMMAT:
					si = *srcP++;
					ai = *argP++;
					if (si < ai) goto VMJMPGTNUMMAT_CONTINUE;
					if (si > ai) {Ip = (NUM*)&pcodes[n]; break;}
					if (--m > 0) goto VMJMPGTNUMMAT;
					if (NumMatrix(*source)->itsMaxItemIndex > NumMatrix(*argument)->itsMaxItemIndex)
						{Ip = (NUM*)&pcodes[n]; break;}
					if (NumMatrix(*source)->itsMaxItemIndex < NumMatrix(*argument)->itsMaxItemIndex) goto VMJMPGTNUMMAT_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&NumMatrix(*source)->itsCdr,&NumMatrix(*argument)->itsCdr) > 0)
						{Ip = (NUM*)&pcodes[n]; break;}
					VMJMPGTNUMMAT_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					Ip = (NUM*)&pcodes[n];
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt > argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt > argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.UInt > argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt > argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt >  cp->itsReal ||
						   ((REAL)source->u.UInt == cp->itsReal && 0.0 > cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag > argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                                
            case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int > argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Int > argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Int > argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int > argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int >  cp->itsReal ||
						   ((REAL)source->u.Int == cp->itsReal && 0.0 > cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag > argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYDATE:
            case TYMONEY:
            case TYREAL:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Real > argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Real > argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real > argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real > argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real >  cp->itsReal ||
						   (source->u.Real == cp->itsReal && 0.0 > cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag > argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char > argument->u.UInt)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYNUM:
                        if (source->u.Char > argument->u.Int)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Char > argument->u.Real)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char > argument->u.Char)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char  > cp->itsReal ||
						   ((REAL)source->u.Char == cp->itsReal && 0.0 > cp->itsImag))
                            Ip = (NUM*)&pcodes[n];
						break;

					default:
                        if (source->Tag > argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool > argument->u.Bool)
                            Ip = (NUM*)&pcodes[n];
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag > argument->Tag)
                            Ip = (NUM*)&pcodes[n];
                        break;
                    }
                break;

			case TYCPX:
				cp = source->u.Complex;
				switch (argument->Tag)
				{
                case TYUNUM:
                    if (cp->itsReal >  (REAL)argument->u.UInt ||
						(cp->itsReal == (REAL)argument->u.UInt && cp->itsImag > 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYNUM:
                    if (cp->itsReal >  (REAL)argument->u.Int ||
						(cp->itsReal == (REAL)argument->u.Int && cp->itsImag > 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if (cp->itsReal >  argument->u.Real ||
					   (cp->itsReal == argument->u.Real && cp->itsImag > 0.0))
                        Ip = (NUM*)&pcodes[n];
                    break;

                case TYCHAR:
                    if (cp->itsReal >  (REAL)argument->u.Char ||
					   (cp->itsReal == (REAL)argument->u.Char && cp->itsImag > 0.0))
						Ip = (NUM*)&pcodes[n];
                    break;

				case TYCPX:
					if (cp->itsReal >  argument->u.Complex->itsReal ||
					   (cp->itsReal == argument->u.Complex->itsReal &&
						cp->itsImag >  argument->u.Complex->itsImag))
                        Ip = (NUM*)&pcodes[n];
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag != argument->Tag)
                        Ip = (NUM*)&pcodes[n];
                    break;
				}
				break;

			default:
 				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
                if (isCompareGT(&retValue))
                    Ip = (NUM*)&pcodes[n];
                break;
            }
        break;

    case  VMJUMP:
		setLabel(LVMJUMP)
        /*  Load a pointer to the branch operand */
        /*  Branch to the source operand */
        Ip = (NUM*)&pcodes[*(Ip)];
		goto Fetch;
        break;

    case  VMMOVE:
		setLabel(LVMMOVE)
        /*  Load the address of each of the two operands */
        source    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        /* Move the source to the target operand */
        *target = *source;
		goto Fetch;
        break;
        
    case  VMDEBUGGER:
		setLabel(LVMDEBUGGER)
		/*  Place the new debugger interrupt code here. */
		retValue = FSmartbase_Ref(gCP,gTP,2,self->Interfaces,TOBJ(gCP->TLambda_BreakList));
		if (retValue.Tag != TYDIRECTORY) goto ErrorIllegalInstruction;
		retValue = FSmartbase_Ref(gCP,gTP,2,retValue,TINT(((Ip - (NUM*)pcodes)-1)));
		if (retValue.Tag != TYNUM) goto ErrorIllegalInstruction;
		pcode.Opcode = retValue.u.Int;
		if (gTP->DebugTraceOn != NIL && gTP->DebugSuspended == 0)
			{
			if (((gTP->DebugBreakProc == NIL) && (gTP->DebugBreakExp.Tag == TYVOID)) ||
				((gTP->DebugBreakProc == proc) && (gTP->DebugBreakIP < 0)) ||
				((gTP->DebugBreakProc == proc) && 
				(gTP->DebugBreakIP == (Ip - 1 - &atHMInt(Pc->itsInstructionArray,0))) &&
				(--gTP->DebugBreakCount <= 0)) ||
				(FSmartbase_DebugUntilExp(gCP,gTP) == TRUE))
				{
		        goto ResumeAfterFetch;
				}
			}
		goto CallVMDebugger;
        break;
            
	case  VMREFDICKEY:
		setLabel(LVMREFDICKEY)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Object = BondArray((*source))[argument->u.Int].Key;
		target->Tag = (target->u.Object != NIL) ? target->u.Object->itsObjectType : TYVOID;
		goto Fetch;
		break;    

	case  VMREFSTRKEY:
		setLabel(LVMREFSTRKEY)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Object = BindArray((*source))[argument->u.Int].Key;
		target->Tag = (target->u.Object != NIL) ? target->u.Object->itsObjectType : TYVOID;
		goto Fetch;
		break;    

	case  VMREFDIRKEY:
		setLabel(LVMREFDIRKEY)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = PBindArray((*source))[argument->u.Int].Key;
		goto Fetch;
		break;    

	case  VMREFDIRVALUE:
		setLabel(LVMREFDIRVALUE)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = PBindArray((*source))[argument->u.Int].Value;
		goto Fetch;
		break;    

	case  VMREFDICVALUE:
		setLabel(LVMREFDICVALUE)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = BondArray((*source))[argument->u.Int].Value;
		goto Fetch;
		break;    

	case  VMREFSTRVALUE:
		setLabel(LVMREFSTRVALUE)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = BindArray((*source))[argument->u.Int].Value;
		goto Fetch;
		break;    

	case  VMREFPCDVECTOR:
		setLabel(LVMREFPCDVECTOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = PcodeArray((*source))[argument->u.Int];
		target->Tag = TYNUM;
		goto Fetch;
		break;    

    case  VMREFNUMMATRIX:
		setLabel(LVMREFNUMMATRIX)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = RealMatrix((*source))[argument->u.Int];
		target->Tag = TYREAL;
		goto Fetch;
		break;    

    case  VMREFMATRIX:
		setLabel(LVMREFMATRIX)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = TvalMatrix((*source))[argument->u.Int];
		goto Fetch;
		break;

    case  VMREFFLTVECTOR:
		setLabel(LVMREFFLTVECTOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = FloatArray((*source))[argument->u.Int];
		target->Tag = TYREAL;
		goto Fetch;
		break;    

    case  VMREFNUMVECTOR:
		setLabel(LVMREFNUMVECTOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = RealArray((*source))[argument->u.Int];
		target->Tag = TYREAL;
		goto Fetch;
		break;    

    case  VMREFINTVECTOR:
		setLabel(LVMREFINTVECTOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = IntArray((*source))[argument->u.Int];
		target->Tag = TYNUM;
		goto Fetch;
		break;    

    case  VMREFOBJVECTOR:
		setLabel(LVMREFOBJVECTOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Object = ObjArray((*source))[argument->u.Int];
		target->Tag = (target->u.Object != NIL) ? target->u.Object->itsObjectType : TYVOID;
		goto Fetch;
		break;    

    case  VMREFBYTVECTOR:
		setLabel(LVMREFBYTVECTOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = ByteArray((*source))[argument->u.Int];
		target->Tag = TYNUM;
		goto Fetch;
		break;    

    case  VMREFTEXT:
		setLabel(LVMREFTEXT)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = source->u.Text[argument->u.Int];
		target->Tag = TYNUM;
		goto Fetch;
		break;

    case  VMREFSYMBOL:
		setLabel(LVMREFSYMBOL)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = SymbolArray((*source))[argument->u.Int];
		target->Tag = TYNUM;
		goto Fetch;
		break;

    case  VMREFSTRING:
		setLabel(LVMREFSTRING)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = CharArray((*source))[argument->u.Int];
		target->Tag = TYNUM;
		goto Fetch;
		break;

    case  VMREFVECTOR:
		setLabel(LVMREFVECTOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = TvalArray((*source))[argument->u.Int];
		goto Fetch;
		break;

    case  VMREFBITVECTOR:
		setLabel(LVMREFBITVECTOR)
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = ((BitArray((*source))[argument->u.Int/8] & gCP->TBitVector_OrMasks[argument->u.Int%8]) != 0);
		target->Tag = TYNUM;
		goto Fetch;
		break;    

	case  VMREF:
		setLabel(LVMREF)
        /*  Load the address of each of the three operands */
        index	    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        isource     = *(TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        /*  Assign the indexed source value to the target argument */
		switch (isource.Tag)
			{
			case TYVOID:
				retValue = gCP->Tval_VOID;
			break;

			case TYNUM:
				/*  Load the value from the source register pointer, indexed by the integer index. */
				/*  Note: The source DeclaredType field tells the type of register pointer. */
				if (index->Tag == TYNUM)
					{
					switch(isource.DeclaredType)
						{
						case TYCHARPOINTER:
							retValue.u.Int = (NUM)((LpCHAR)isource.u.Int)[index->u.Int];
							retValue.Tag = TYCHAR;
							break;

						case TYFLOATPOINTER:
							retValue.u.Real = (REAL)((LpFLOAT)isource.u.Int)[index->u.Int];
							retValue.Tag = TYREAL;
							break;

						case TYINTPOINTER:
						case TYJUMPPOINTER:
							retValue.u.Int = (NUM)((LpNUM)isource.u.Int)[index->u.Int];
							retValue.Tag = TYNUM;
							break;

						case TYREALPOINTER:
							retValue.u.Real = (REAL)((LpREAL)isource.u.Int)[index->u.Int];
							retValue.Tag = TYREAL;
							break;

						case TYSHORTPOINTER:
							retValue.u.Int = (NUM)((LpSHORT)isource.u.Int)[index->u.Int];
							retValue.Tag = TYNUM;
							break;

						case TYLONGPOINTER:
							retValue.u.Int = (NUM)((LpNUM32)isource.u.Int)[index->u.Int];
							retValue.Tag = TYNUM;
							break;

						case TYWORDPOINTER:
							retValue = ((TVAL*)isource.u.Int)[index->u.Int];
							break;

						default:
							retValue = gCP->Tval_VOID;
							break;
						}
					}
				else
					retValue = gCP->Tval_VOID;
			break;
			
			case TYBRICK:
				/*  Load the value from the referent Brick for the index key. */
				/*  Use the memoized index position as a hint in quick search. */
				switch (index->Tag)
					{
					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						/* We use memoizing to bypass the search (if possible). */
						if ((inRange(asMemo(index),0,isource.u.Brick->itsFieldList.u.Brick->itsMaxItemIndex)) &&
							(BindArray(isource.u.Brick->itsFieldList)[asMemo(index)].Key == index->u.Object))
							{
							i = asMemo(index);
							goto VMREF_Brick_GetIV1;
							}
						else
							{
							/* Memoizing failed, so we must perform a search. */
							n = isource.u.Brick->itsFieldList.u.Brick->itsMaxItemIndex;
							bindPtr = BindArray(isource.u.Brick->itsFieldList);
							for (i = 0; i < n; ++i)
								{
								if (bindPtr[i].Key == index->u.Object) goto VMREF_Brick_Continue;
								}
							VMREF_Brick_Continue:
							if (i < n) 
								{
								asMemo(index) = i; 
								goto VMREF_Brick_GetIV1;
								}
							else
								{
								retValue = gCP->TObject_ERROR_BADIDXORKEY;
								}
							}
					break;

					case TYNUM:
                        i = index->u.Int;
                        if (isource.u.Brick->itsRowCount >= 1)
                            { /* if number of rows is greater than 1 */
                            if ((i < 0) || (i >= isource.u.Brick->itsRowCount))
                                {
                                retValue = gCP->TObject_ERROR_BADIDXORKEY;
                                }
                            else
                                {
                                retValue.Tag = TYBRICKROW;
                                RowIdx(retValue) = index->u.Int;
                                ObjIdx(retValue) = isource.u.Brick->itsObjectIndex;
                                }
                            }
                        else
                            { /* if number of rows is just 1, treat it as a field index */
						    if ((i < 0) || (i >= isource.u.Brick->itsFieldList.u.Structure->itsMaxItemIndex))
							    {
							    retValue = gCP->TObject_ERROR_BADIDXORKEY;
							    }
						    else
							    {
							    VMREF_Brick_GetIV1:
							    /* Use the Declared Type to return the proper value. */
							    bindPtr = &BindArray(isource.u.Brick->itsFieldList)[i];
							    
							    /* **************************************************** */
							    /* NOTE: There's a special handling for CHARACTER types */
							    /* **************************************************** */
							    
							    /* If the no. of repeats is greater than 1, we will return a TBrickField */
							    if (bindPtr->Value.Modifier > 1 && bindPtr->Value.DeclaredType != TYCHAR)
							        {
						            retValue.Tag = TYBRICKFIELD;
						            ObjIdx(retValue) = isource.u.Brick->itsObjectIndex;
						            RowIdx(retValue) = 0;
						            FldIdx(retValue) = i;
							        }
							    else
							        {
							        fieldArrayPtr = asFieldArray(isource.u.Brick) + bindPtr->Value.Offset;
							        switch (bindPtr->Value.DeclaredType)
								        {
								        case TYBOLE:
									        retValue.Tag = TYBOLE;
									        retValue.u.Bool = *((LpCHAR)fieldArrayPtr);
									        break;

                                        case TYCHAR:
                                            if (bindPtr->Value.Modifier > 1)
                                                {
                                                retValue.Tag = TYSTRINGSUBSTR;
                                                ObjIdx(retValue) = isource.u.Brick->itsObjectIndex;
                                                SubOff(retValue) = bindPtr->Value.Offset;
                                                
                                                if (fieldArrayPtr[bindPtr->Value.Modifier - 1] != 0)
                                                    SubLen(retValue) = bindPtr->Value.Modifier;
                                                else
                                                    SubLen(retValue) = strlen((char*)fieldArrayPtr);                                                
                                                }
                                            else
                                                {
                                                retValue.Tag = TYCHAR;
                                                retValue.u.Char = fieldArrayPtr[0];
                                                }
                                            break;

                                        case TYDATE:
									        retValue.Tag = TYDATE;
									        retValue.u.Real = *((LpREAL)fieldArrayPtr);
                                            break;

										case TYUNUM:
									        retValue.Tag = TYUNUM;
									        retValue.u.UInt = *((LpUNUM)fieldArrayPtr);
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
									        retValue.Tag = TYNUM;
									        retValue.u.Int = *((LpNUM)fieldArrayPtr);
									        break;

								        case TYFLOAT:
									        retValue.Tag = TYREAL;
									        retValue.u.Real = *((LpFLOAT)fieldArrayPtr);
									        break;
									        
								        case TYMONEY:
									        retValue.Tag = TYMONEY;
									        retValue.u.Real = *((LpREAL)fieldArrayPtr);
									        break;

								        case TYREAL:
									        retValue.Tag = TYREAL;
									        retValue.u.Real = *((LpREAL)fieldArrayPtr);
									        break;

								        case TYOBJ:
									        retValue.u.Object = *((TObject**)fieldArrayPtr);
									        retValue.Tag = (retValue.u.Object == NIL) ? TYVOID : retValue.u.Object->itsObjectType;
									        break;

								        case TYLONG:
									        retValue.Tag = TYNUM;
									        retValue.u.Int = *((LpNUM32)fieldArrayPtr);
									        break;

								        case TYSHORT:
									        retValue.Tag = TYNUM;
									        retValue.u.Int = *((LpSHORT)fieldArrayPtr);
									        break;

								        case TYTVAL:
									        retValue = *((LpTVAL)fieldArrayPtr);
									        break;		

								        default:
									        retValue = gCP->TObject_ERROR_BADCELL;
									        break;
								        }
								    } /* if field is not repeating */
							    } /* if field index is valid */
                            } /* if number of rows is greater than 1 */
					break;

					default:
						retValue = TBrick_GetIV1(gCP,gTP,isource,*index);
					break;
					}
			break;
			
			case TYBRICKROW:
				/*  Load the value from the referent BrickRow for the index key. */
				/*  Use the memoized index position as a hint in quick search. */
				switch (index->Tag)
					{
					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						/* We use memoizing to bypass the search (if possible). */
						if ((inRange(asMemo(index),0,BrickFromRow(isource)->itsFieldList.u.Structure->itsMaxItemIndex)) &&
							(BindArray(BrickFromRow(isource)->itsFieldList)[asMemo(index)].Key == index->u.Object))
							{
							i = asMemo(index);
							goto VMREF_BrickRow_GetIV1;
							}
						else
							{
							/* Memoizing failed, so we must perform a search. */
							n = BrickFromRow(isource)->itsFieldList.u.Structure->itsMaxItemIndex;
							bindPtr = BindArray(BrickFromRow(isource)->itsFieldList);
							for (i = 0; i < n; ++i)
								{
								if (bindPtr[i].Key == index->u.Object) goto VMREF_BrickRow_Continue;
								}
							VMREF_BrickRow_Continue:
							if (i < n) 
								{
								asMemo(index) = i; 
								goto VMREF_BrickRow_GetIV1;
								}
							else
								{
								retValue = gCP->TObject_ERROR_BADIDXORKEY;
								}
							}
					break;

					case TYNUM:
                        i = index->u.Int;
                        /* special handling: if the field repeat is greater than 1, we must return a TBrickField */
                        /* i represents the field index */
                        
                        /* check if the field index is valid, check against the no. of items in the field list structure */
                        if ((i < 0) || (i >= BrickFromRow(isource)->itsFieldList.u.Structure->itsMaxItemIndex))
                            {
                            retValue = gCP->TObject_ERROR_BADIDXORKEY;
                            }
                        else
                            {
                            VMREF_BrickRow_GetIV1:

                            /* get a pointer to the field definition */
                            bindPtr = &BindArray(BrickFromRow(isource)->itsFieldList)[i];

						    /* **************************************************** */
						    /* NOTE: There's a special handling for CHARACTER types */
						    /* **************************************************** */
                            
                            /* bindPtr->Value.Modifier represents the no. of repeats */
                            /* if no. of repeats is greater than 1, return a TBrickField */
                            if (bindPtr->Value.Modifier > 1 && bindPtr->Value.DeclaredType != TYCHAR)
                                {
                                retValue.Tag = TYBRICKFIELD;
                                ObjIdx(retValue) = ObjIdx(isource);
                                RowIdx(retValue) = RowIdx(isource);
                                FldIdx(retValue) = i;
                                }
                            else
                                {
                                /* since the no. of repeats is 1, we can safely retrieve the value in the record */
                                fieldArrayPtr = asFieldArray(BrickFromRow(isource)) + 
                                                (RowIdx(isource) * BrickFromRow(isource)->itsRowByteCount) +
                                                bindPtr->Value.Offset;
                                
							    switch (bindPtr->Value.DeclaredType)
								    {
								    case TYBOLE:
									    retValue.Tag = TYBOLE;
									    retValue.u.Bool = *((LpCHAR)fieldArrayPtr);
									    break;

                                    case TYCHAR:
                                        if (bindPtr->Value.Modifier > 1)
                                            {
                                            retValue.Tag = TYSTRINGSUBSTR;
                                            ObjIdx(retValue) = ObjIdx(isource);
                                            SubOff(retValue) = RowIdx(isource) * BrickFromRow(isource)->itsRowByteCount + bindPtr->Value.Offset;

                                            if (fieldArrayPtr[bindPtr->Value.Modifier - 1] != 0)
                                                SubLen(retValue) = bindPtr->Value.Modifier;
                                            else
                                                SubLen(retValue) = strlen((char*)fieldArrayPtr);
                                            }
                                        else
                                            {
                                            retValue.Tag = TYCHAR;
                                            retValue.u.Char = fieldArrayPtr[0];
                                            }                                    
                                        break;

                                    case TYDATE:
									    retValue.Tag = TYDATE;
									    retValue.u.Real = *((LpREAL)fieldArrayPtr);
                                        break;

									case TYUNUM:
									    retValue.Tag = TYUNUM;
									    retValue.u.UInt = *((LpUNUM)fieldArrayPtr);
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
									    retValue.Tag = TYNUM;
									    retValue.u.Int = *((LpNUM)fieldArrayPtr);
									    break;

								    case TYFLOAT:
									    retValue.Tag = TYREAL;
									    retValue.u.Real = *((LpFLOAT)fieldArrayPtr);
									    break;

							        case TYMONEY:
								        retValue.Tag = TYMONEY;
								        retValue.u.Real = *((LpREAL)fieldArrayPtr);
								        break;

								    case TYREAL:
									    retValue.Tag = TYREAL;
									    retValue.u.Real = *((LpREAL)fieldArrayPtr);
									    break;

								    case TYOBJ:
									    retValue.u.Object = *((TObject**)fieldArrayPtr);
									    retValue.Tag = (retValue.u.Object == NIL) ? TYVOID : retValue.u.Object->itsObjectType;
									    break;

								    case TYLONG:
									    retValue.Tag = TYNUM;
									    retValue.u.Int = *((LpNUM32)fieldArrayPtr);
									    break;

								    case TYSHORT:
									    retValue.Tag = TYNUM;
									    retValue.u.Int = *((LpSHORT)fieldArrayPtr);
									    break;

								    case TYTVAL:
									    retValue = *((LpTVAL)fieldArrayPtr);
									    break;		

								    default:
									    retValue = gCP->TObject_ERROR_BADCELL;
									    break;
								    }                                
                                } /* no. of repeats is 1 */
                            } /* field index is valid */
					break;

					default:
						retValue = TBrickRowT_GetIV1(gCP, gTP, isource, *index);
					break;
					}
			break;

			case TYBRICKFIELD:
				/*  Load the value from the referent BrickField for the index key. */
				/*  Use the memoized index position as a hint in quick search. */
				switch (index->Tag)
					{
					case TYNUM:
					    /* i represents the repeat index */
                        i = index->u.Int;
                        
                        /* get a pointer to the field definition */
                        bindPtr = &BindArray(BrickFromFld(isource)->itsFieldList)[FldIdx(isource)];
                        
                        /* check if the repeat index is valid, check against the no. of repeat index in the field definition */
                        if ((i < 0) || (i >= bindPtr->Value.Modifier))
                            {
                            retValue = gCP->TObject_ERROR_BADIDXORKEY;
                            }
                        else
                            {
                            /* since the no. of repeats is 1, we can safely retrieve the value in the record */
                            fieldArrayPtr = asFieldArray(BrickFromFld(isource)) + (RowIdx(isource) * BrickFromFld(isource)->itsRowByteCount);
                            
						    switch (bindPtr->Value.DeclaredType)
							    {
							    case TYBOLE:
								    retValue.Tag = TYBOLE;
								    retValue.u.Bool = ((LpCHAR)(fieldArrayPtr + bindPtr->Value.Offset))[i];
								    break;

                                case TYDATE:
								    retValue.Tag = TYDATE;
								    retValue.u.Real = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[i];
                                    break;

								case TYUNUM:
								    retValue.Tag = TYUNUM;
								    retValue.u.UInt = ((LpUNUM)(fieldArrayPtr + bindPtr->Value.Offset))[i];
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
								    retValue.Tag = TYNUM;
								    retValue.u.Int = ((LpNUM)(fieldArrayPtr + bindPtr->Value.Offset))[i];
								    break;

							    case TYFLOAT:
								    retValue.Tag = TYREAL;
								    retValue.u.Real = ((LpFLOAT)(fieldArrayPtr + bindPtr->Value.Offset))[i];
								    break;

						        case TYMONEY:
							        retValue.Tag = TYMONEY;
							        retValue.u.Real = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[i];
							        break;

							    case TYREAL:
								    retValue.Tag = TYREAL;
								    retValue.u.Real = ((LpREAL)(fieldArrayPtr + bindPtr->Value.Offset))[i];
								    break;

							    case TYOBJ:
								    retValue.u.Object = ((TObject**)(fieldArrayPtr + bindPtr->Value.Offset))[i];
								    retValue.Tag = (retValue.u.Object == NIL) ? TYVOID : retValue.u.Object->itsObjectType;
								    break;

							    case TYLONG:
								    retValue.Tag = TYNUM;
								    retValue.u.Int = ((LpNUM32)(fieldArrayPtr + bindPtr->Value.Offset))[i];
								    break;

							    case TYSHORT:
								    retValue.Tag = TYNUM;
								    retValue.u.Int = ((LpSHORT)(fieldArrayPtr + bindPtr->Value.Offset))[i];
								    break;

							    case TYTVAL:
								    retValue = ((LpTVAL)(fieldArrayPtr + bindPtr->Value.Offset))[i];
								    break;		

							    default:
								    retValue = gCP->TObject_ERROR_BADCELL;
								    break;
							    }                                
                            } /* repeat index is valid */
					break;

					default:
						retValue = TBrickFieldT_GetIV1(gCP, gTP, isource, *index);
					break;
					}
			break;

			case TYSTRUCTURE:
				/*  Load the value from the referent Structure for the index key. */
				/*  Use the memoized index position as a hint in quick search. */
				switch (index->Tag)
					{
					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						if ((inRange(asMemo(index),0,Structure(isource)->itsMaxItemIndex)) &&
							(BindArray(isource)[asMemo(index)].Key == index->u.Object))
							{
							retValue = BindArray(isource)[asMemo(index)].Value;
							}
						else
							{
							gTP->FVmscript_iargument = TStructure_SearchKey(gCP,gTP,(TStructure*)isource.u.Object, index->u.Object, (short*)((NUM)&gTP->FVmscript_Selection));
							if (gTP->FVmscript_Selection != 0)
								{
								retValue = gCP->Tval_VOID;
								}
							else
								{
								/* If a the key finds a match, memoize to make the next search quicker. */
								retValue = BindArray(isource)[gTP->FVmscript_iargument.u.Int].Value;
								asMemo(index) = gTP->FVmscript_iargument.u.Int;
								}
							}
					break;

					case TYNUM:
						if (inRange(index->u.Int,0,Structure(isource)->itsMaxItemIndex))
							{
							retValue = BindArray(isource)[index->u.Int].Value;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,Structure(isource)->itsMaxItemIndex))
							{
							retValue = BindArray(isource)[(unsigned char)index->u.Char].Value;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,Structure(isource)->itsMaxItemIndex))
							{
							retValue = BindArray(isource)[(NUM)index->u.Real].Value;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYDICTIONARY:
				/*  Load the value from the referent Dictionary for the index key. */
				/*  Use the memoized index position as a hint in quick search. */
				switch (index->Tag)
					{
					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						if ((inRange(asMemo(index),0,Dictionary(isource)->itsMaxItemIndex)) &&
							(BondArray(isource)[asMemo(index)].Key == index->u.Object))
							{
							retValue = BondArray(isource)[asMemo(index)].Value;
							}
						else
							{
							gTP->FVmscript_iargument = TDictionary_BSearchKey(gCP,gTP,(TDictionary*)isource.u.Object, index->u.Object, (short*)((NUM)&gTP->FVmscript_Selection));
							if (gTP->FVmscript_Selection != 0)
								{
								retValue = gCP->Tval_VOID;
								}
							else
								{
								retValue = BondArray(isource)[gTP->FVmscript_iargument.u.Int].Value;
								asMemo(index) = gTP->FVmscript_iargument.u.Int;
								}
							}
					break;

					case TYNUM:
						if (inRange(index->u.Int,0,Dictionary(isource)->itsMaxItemIndex))
							{
							retValue = BondArray(isource)[index->u.Int].Value;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,Dictionary(isource)->itsMaxItemIndex))
							{
							retValue = BondArray(isource)[(unsigned char)index->u.Char].Value;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,Dictionary(isource)->itsMaxItemIndex))
							{
							retValue = BondArray(isource)[(NUM)index->u.Real].Value;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYVECTOR:
				switch (index->Tag)
					{
					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						if (Vector(isource)->itsAttributes != NIL)
							{
							attv = Vector(isource)->itsAttributes;
							if ((inRange(asMemo(index),0,Vector(isource)->itsMaxItemIndex)) &&
								(attv->itsMaxItemIndex > asMemo(index)) &&
								(atHMObject(attv->itsObjectArray,asMemo(index)) == index->u.Object))
								{
								retValue = TvalArray(isource)[asMemo(index)];
								break;
								}
							else
								{
								retValue = gCP->Tval_VOID;
								for (i = 0; (i < attv->itsMaxItemIndex) && (i < attv->itsMaxItemIndex); ++i)
									{
									/* Check if the index matches a symbol in the attributes vector */
									if (atHMObject(attv->itsObjectArray,i) == asObject(index))
										{
										/*  Make sure array index is in range. */
										if (i >= Vector(isource)->itsMaxItemIndex)
											{
											retValue = gCP->TObject_VOID;
											}
										else
											{
											/* If the symbolic key matches, memoize to make the next search quicker */
											/* Note: We save the position, where the matching key was found */
											/*       in the vmref instruction. This allows future references */
											/*       to locate the item much faster than the first time. */
											retValue = TvalArray(isource)[i];
											asMemo(index) = i;
											}
										}
									}
								}
							}
						else
							retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;

					case TYNUM:
						if (inRange(index->u.Int,0,Vector(isource)->itsMaxItemIndex))
							{
							retValue = TvalArray(isource)[index->u.Int];
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,Vector(isource)->itsMaxItemIndex))
							{
							retValue = TvalArray(isource)[(unsigned char)index->u.Char];
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,Vector(isource)->itsMaxItemIndex))
							{
							retValue = TvalArray(isource)[(NUM)index->u.Real];
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYMATRIX:
				switch (index->Tag)
					{
					case TYNUM:
					    if (Matrix(isource)->itsRank == 1)
					        {
					        if (inRange(index->u.Int, 0, Matrix(isource)->itsMaxItemIndex))
					            {
    					        retValue = TvalMatrix(isource)[index->u.Int];
					            }
					        else
					            retValue = gCP->Tval_VOID;
					        }
					    else
					    if (Matrix(isource)->itsRank > 1)
					        {
    					    if (inRange(index->u.Int, 0, Matrix(isource)->itsDimensions[0]))
    					        {
						        retValue.Tag = TYMATRIXROW;
						        ObjIdx(retValue) = Matrix(isource)->itsObjectIndex;
						        RowIdx(retValue) = index->u.Int;
						        FldIdx(retValue) = -1;    					    
    					        }
    					    else
    					        retValue = gCP->Tval_VOID;
    					    }
    			        else
    			            retValue = gCP->Tval_VOID;
					break;

					case TYCHAR:
					    if (Matrix(isource)->itsRank == 1)
					        {
					        if (inRange(index->u.Char, 0, Matrix(isource)->itsMaxItemIndex))
					            {
    					        retValue = TvalMatrix(isource)[(unsigned char)index->u.Char];
					            }
					        else
					            retValue = gCP->Tval_VOID;
					        }
					    else
					    if (Matrix(isource)->itsRank > 1)
					        {
    					    if (inRange(index->u.Char, 0, Matrix(isource)->itsDimensions[1]))
    					        {
						        retValue.Tag = TYMATRIXROW;
						        ObjIdx(retValue) = Matrix(isource)->itsObjectIndex;
						        RowIdx(retValue) = (unsigned char)index->u.Char;
						        FldIdx(retValue) = -1;    					    
    					        }
    					    else
    					        retValue = gCP->Tval_VOID;
    					    }
    			        else
    			            retValue = gCP->Tval_VOID;					
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
					    if (Matrix(isource)->itsRank == 1)
					        {
					        if (inRange((NUM)index->u.Real, 0, Matrix(isource)->itsMaxItemIndex))
					            {
    					        retValue = TvalMatrix(isource)[(NUM)index->u.Real];
					            }
					        else
					            retValue = gCP->Tval_VOID;
					        }
					    else
					    if (Matrix(isource)->itsRank > 1)
					        {
    					    if (inRange((NUM)index->u.Real, 0, Matrix(isource)->itsDimensions[1]))
    					        {
						        retValue.Tag = TYMATRIXROW;
						        ObjIdx(retValue) = Matrix(isource)->itsObjectIndex;
						        RowIdx(retValue) = (NUM)index->u.Real;
						        FldIdx(retValue) = -1;    					    
    					        }
    					    else
    					        retValue = gCP->Tval_VOID;
    					    }
    			        else
    			            retValue = gCP->Tval_VOID;						
					break;

					default:
						retValue = TObject_GetIV1(gCP, gTP, isource, *index);
					break;
					}
			break;

			case TYMATRIXROW:
				switch (index->Tag)
					{
					case TYNUM:
					    VMREF_MatrixRow_GetIV1:
					    
					    if (FldIdx(isource) >= 0)
					        {
    					    if (inRange(index->u.Int, 0, MatrixFromRow(isource)->itsDimensions[2]))
    					        {
    					        retValue.u.Int = (RowIdx(isource) * MatrixFromRow(isource)->itsDimensions[1] * MatrixFromRow(isource)->itsDimensions[2]) + 
    					                         (FldIdx(isource) * MatrixFromRow(isource)->itsDimensions[2]) + index->u.Int;
    					        retValue = atHMTval(MatrixFromRow(isource)->itsTvalMatrix, retValue.u.Int);
    					        }
    					    else
    					        retValue = gCP->Tval_VOID;
					        }
					    else
					    if (RowIdx(isource) >= 0)
					        {
    					    if (inRange(index->u.Int, 0, MatrixFromRow(isource)->itsDimensions[1]))
    					        {
    					        if (MatrixFromRow(isource)->itsRank > 2)
        					        {
            					    retValue.Tag = TYMATRIXROW;
            					    ObjIdx(retValue) = ObjIdx(isource);
            					    RowIdx(retValue) = RowIdx(isource);
            					    FldIdx(retValue) = index->u.Int;
        					        }
        					    else
        					        {
            					    retValue.u.Int = (RowIdx(isource) * MatrixFromRow(isource)->itsDimensions[1]) + index->u.Int;
            					    retValue = atHMTval(MatrixFromRow(isource)->itsTvalMatrix, retValue.u.Int);
        					        }
    					        }
    					    else
    					        retValue = gCP->Tval_VOID;
					        }
					    else
					        retValue = gCP->Tval_VOID;
					break;

					case TYCHAR:
					    index->u.Int = (unsigned char)index->u.Char;
					    goto VMREF_MatrixRow_GetIV1;
					
					case TYREAL:
					case TYDATE:
					case TYMONEY:
					    index->u.Int = (NUM)index->u.Real;
					    goto VMREF_MatrixRow_GetIV1;
					break;

					default:
						retValue = TMatrixRow_GetIV1(gCP, gTP, isource, *index);
					break;
					}
			break;

			case TYTEXT:
				switch (index->Tag)
					{
					case TYNUM:
						if (inRange(index->u.Int,0,(NUM)strlen(isource.u.Text)))
							{
							retValue.u.Char = isource.u.Text[index->u.Int];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if ((unsigned char)index->u.Char == 0)
							{
							retValue.u.Char = isource.u.Text[0];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,strlen(isource.u.Text)))
							{
							retValue.u.Char = isource.u.Text[(NUM)index->u.Real];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						retValue = gCP->Tval_VOID;
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYFLTVECTOR:
				switch (index->Tag)
					{
					case TYNUM:
						if (inRange(index->u.Int,0,FltVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Real = FloatArray(isource)[index->u.Int];
							retValue.Tag = TYREAL;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,FltVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Real = FloatArray(isource)[(unsigned char)index->u.Char];
							retValue.Tag = TYREAL;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,FltVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Real = FloatArray(isource)[(NUM)index->u.Real];
							retValue.Tag = TYREAL;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYNUMVECTOR:
				switch (index->Tag)
					{
					case TYNUM:
						if (inRange(index->u.Int,0,NumVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Real = RealArray(isource)[index->u.Int];
							retValue.Tag = TYREAL;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,NumVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Real = RealArray(isource)[(unsigned char)index->u.Char];
							retValue.Tag = TYREAL;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,NumVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Real = RealArray(isource)[(NUM)index->u.Real];
							retValue.Tag = TYREAL;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYNUMMATRIX:
				switch (index->Tag)
					{
					case TYNUM:
					    if (Matrix(isource)->itsRank == 1)
					        {
					        if (inRange(index->u.Int, 0, Matrix(isource)->itsMaxItemIndex))
					            {
    					        retValue.u.Real = RealMatrix(isource)[index->u.Int];
    					        retValue.Tag = TYREAL;
					            }
					        else
					            retValue = gCP->Tval_VOID;
					        }
					    else
					    if (Matrix(isource)->itsRank > 1)
					        {
    					    if (inRange(index->u.Int, 0, Matrix(isource)->itsDimensions[0]))
    					        {
						        retValue.Tag = TYNUMMATRIXROW;
						        ObjIdx(retValue) = Matrix(isource)->itsObjectIndex;
						        RowIdx(retValue) = index->u.Int;
						        FldIdx(retValue) = -1;    					    
    					        }
    					    else
    					        retValue = gCP->Tval_VOID;
    					    }
    			        else
    			            retValue = gCP->Tval_VOID;
					break;

					case TYCHAR:
					    if (Matrix(isource)->itsRank == 1)
					        {
					        if (inRange(index->u.Char, 0, Matrix(isource)->itsMaxItemIndex))
					            {
    					        retValue.u.Real = RealMatrix(isource)[(unsigned char)index->u.Char];
    					        retValue.Tag = TYREAL;
					            }
					        else
					            retValue = gCP->Tval_VOID;
					        }
					    else
					    if (Matrix(isource)->itsRank > 1)
					        {
    					    if (inRange(index->u.Char, 0, Matrix(isource)->itsDimensions[1]))
    					        {
						        retValue.Tag = TYNUMMATRIXROW;
						        ObjIdx(retValue) = Matrix(isource)->itsObjectIndex;
						        RowIdx(retValue) = (unsigned char)index->u.Char;
						        FldIdx(retValue) = -1;    					    
    					        }
    					    else
    					        retValue = gCP->Tval_VOID;
    					    }
    			        else
    			            retValue = gCP->Tval_VOID;					
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
					    if (Matrix(isource)->itsRank == 1)
					        {
					        if (inRange((NUM)index->u.Real, 0, Matrix(isource)->itsMaxItemIndex))
					            {
    					        retValue.u.Real = RealMatrix(isource)[(NUM)index->u.Real];
    					        retValue.Tag = TYREAL;
					            }
					        else
					            retValue = gCP->Tval_VOID;
					        }
					    else
					    if (Matrix(isource)->itsRank > 1)
					        {
    					    if (inRange((NUM)index->u.Real, 0, Matrix(isource)->itsDimensions[1]))
    					        {
						        retValue.Tag = TYNUMMATRIXROW;
						        ObjIdx(retValue) = Matrix(isource)->itsObjectIndex;
						        RowIdx(retValue) = (NUM)index->u.Real;
						        FldIdx(retValue) = -1;    					    
    					        }
    					    else
    					        retValue = gCP->Tval_VOID;
    					    }
    			        else
    			            retValue = gCP->Tval_VOID;						
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYNUMMATRIXROW:
				switch (index->Tag)
					{
					case TYNUM:
					    VMREF_NumMatrixRow_GetIV1:
					    
					    if (FldIdx(isource) >= 0)
					        {
    					    if (inRange(index->u.Int, 0, NumMatrixFromRow(isource)->itsDimensions[2]))
    					        {
    					        retValue.u.Int = (RowIdx(isource) * MatrixFromRow(isource)->itsDimensions[1] * MatrixFromRow(isource)->itsDimensions[2]) + 
    					                         (FldIdx(isource) * MatrixFromRow(isource)->itsDimensions[2]) + index->u.Int;
    					        retValue = TREAL(atHMReal(NumMatrixFromRow(isource)->itsRealMatrix, retValue.u.Int));
    					        }
    					    else
    					        retValue = gCP->Tval_VOID;
					        }
					    else
					    if (RowIdx(isource) >= 0)
					        {
    					    if (inRange(index->u.Int, 0, MatrixFromRow(isource)->itsDimensions[1]))
    					        {
    					        if (MatrixFromRow(isource)->itsRank > 2)
        					        {
        					        /* Return another MatrixRow */
            					    retValue.Tag = TYNUMMATRIXROW;
            					    ObjIdx(retValue) = ObjIdx(isource);
            					    RowIdx(retValue) = RowIdx(isource);
            					    FldIdx(retValue) = index->u.Int;
        					        }
        					    else
        					        {
        					        /* Return the Real value */
            					    retValue.u.Int = (RowIdx(isource) * MatrixFromRow(isource)->itsDimensions[1]) + index->u.Int;
            					    retValue = TREAL(atHMReal(NumMatrixFromRow(isource)->itsRealMatrix, retValue.u.Int));
        					        }
    					        }
    					    else
    					        retValue = gCP->Tval_VOID;
					        }
					    else
					        retValue = gCP->Tval_VOID;
					break;

					case TYCHAR:
					    index->u.Int = (unsigned char)index->u.Char;
					    goto VMREF_NumMatrixRow_GetIV1;
					
					case TYREAL:
					case TYDATE:
					case TYMONEY:
					    index->u.Int = (NUM)index->u.Real;
					    goto VMREF_NumMatrixRow_GetIV1;
					break;

					default:
						retValue = TNumMatrixRow_GetIV1(gCP, gTP, isource, *index);
					break;
					}
			break;

			case TYINTVECTOR:
				switch (index->Tag)
					{
					case TYNUM:
						if (inRange(index->u.Int,0,IntVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Int = IntArray(isource)[index->u.Int];
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,IntVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Int = IntArray(isource)[(unsigned char)index->u.Char];
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,IntVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Int = IntArray(isource)[(NUM)index->u.Real];
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYSHORTVECTOR:
				switch (index->Tag)
					{
					case TYNUM:
						if (inRange(index->u.Int,0,ShtVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Int = ShortArray(isource)[index->u.Int];
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,ShtVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Int = ShortArray(isource)[(unsigned char)index->u.Char];
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,ShtVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Int = ShortArray(isource)[(NUM)index->u.Real];
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYLONGVECTOR:
				switch (index->Tag)
					{
					case TYNUM:
						if (inRange(index->u.Int,0,LongVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Int = LongArray(isource)[index->u.Int];
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,LongVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Int = LongArray(isource)[(unsigned char)index->u.Char];
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,LongVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Int = LongArray(isource)[(NUM)index->u.Real];
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYBYTEVECTOR:
				switch (index->Tag)
					{
					case TYNUM:
						if (inRange(index->u.Int,0,ByteVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Char = ByteArray(isource)[index->u.Int];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,ByteVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Char = ByteArray(isource)[(unsigned char)index->u.Char];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,ByteVector(isource)->itsMaxItemIndex))
							{
							retValue.u.Char = ByteArray(isource)[(NUM)index->u.Real];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						/*  We only accept numeric indices. */
						if (strcmp(SymbolArray(*index),"AppendLength") == 0)
							{
							retValue.u.Int = isource.u.ByteVector->itsMemoLength;
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;						
							}
					break;
					
					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;


			case TYBITVECTOR:
				switch (index->Tag)
					{
					case TYNUM:
						if (inRange(index->u.Int,0,BitVector(isource)->itsMaxItemIndex))
							{
							gTP->FVmscript_cacheIndex = index->u.Int;
							retValue.u.Int = ((BitArray(isource)[gTP->FVmscript_cacheIndex/8] & gCP->TBitVector_OrMasks[gTP->FVmscript_cacheIndex%8]) != 0);
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,BitVector(isource)->itsMaxItemIndex))
							{
							gTP->FVmscript_cacheIndex = (unsigned char)index->u.Char;
							retValue.u.Int = ((BitArray(isource)[gTP->FVmscript_cacheIndex/8] & gCP->TBitVector_OrMasks[gTP->FVmscript_cacheIndex%8]) != 0);
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,BitVector(isource)->itsMaxItemIndex))
							{
							gTP->FVmscript_cacheIndex = index->u.Real;
							retValue.u.Int = ((BitArray(isource)[gTP->FVmscript_cacheIndex/8] & gCP->TBitVector_OrMasks[gTP->FVmscript_cacheIndex%8]) != 0);
							retValue.Tag = TYNUM;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						retValue = gCP->Tval_VOID;
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYOBJVECTOR:
				switch (index->Tag)
					{
					case TYNUM:
						if ((inRange(index->u.Int,0,ObjVector(isource)->itsMaxItemIndex)) &&
							(ObjArray(isource)[index->u.Int] != NIL))
							{
							retValue.u.Object = ObjArray(isource)[index->u.Int];
							retValue.Tag = retValue.u.Object->itsObjectType;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if ((inRange(index->u.Char,0,ObjVector(isource)->itsMaxItemIndex)) &&
							(ObjArray(isource)[(unsigned char)index->u.Char] != NIL))
							{
							retValue.u.Object = ObjArray(isource)[(unsigned char)index->u.Char];
							retValue.Tag = retValue.u.Object->itsObjectType;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if ((inRange(index->u.Real,0,ObjVector(isource)->itsMaxItemIndex)) &&
							(ObjArray(isource)[(NUM)index->u.Real] != NIL))
							{
							retValue.u.Object = ObjArray(isource)[(NUM)index->u.Real];
							retValue.Tag = retValue.u.Object->itsObjectType;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYSTRING:
				switch (index->Tag)
					{
					case TYNUM:
						if (inRange(index->u.Int,0,String(isource)->itsMaxItemIndex - 1))
							{
							retValue.u.Char = CharArray(isource)[index->u.Int];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,String(isource)->itsMaxItemIndex - 1))
							{
							retValue.u.Char = CharArray(isource)[(unsigned char)index->u.Char];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;


					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,String(isource)->itsMaxItemIndex - 1))
							{
							retValue.u.Char = CharArray(isource)[(NUM)index->u.Real];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						retValue = gCP->Tval_VOID;
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;

			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				switch (index->Tag)
					{
					case TYNUM:
						if (inRange(index->u.Int,0,String(isource)->itsMaxItemIndex))
							{
							retValue.u.Char = CharArray(isource)[index->u.Int];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYCHAR:
						if (inRange(index->u.Char,0,String(isource)->itsMaxItemIndex))
							{
							retValue.u.Char = CharArray(isource)[(unsigned char)index->u.Char];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;


					case TYREAL:
					case TYDATE:
					case TYMONEY:
						if (inRange(index->u.Real,0,String(isource)->itsMaxItemIndex))
							{
							retValue.u.Char = CharArray(isource)[(NUM)index->u.Real];
							retValue.Tag = TYCHAR;
							}
						else
							{
							retValue = gCP->Tval_VOID;
							}
					break;

					case TYSYMBOL:
					case TYQUOTEDSYMBOL:
						retValue = gCP->Tval_VOID;
					break;

					default:
						retValue = TObject_GetIV1(gCP,gTP,isource, *index);
					break;
					}
			break;
			case TYCPXVECTOR:
				retValue = TCpxVector_GetIV1(gCP,gTP,isource, *index);
			break;
			default:
				retValue = TObject_GetIV1(gCP,gTP,isource, *index);
			break;
			}

		if (retValue.Tag == TYERROR) goto NestedReturn;
		switch (target->DeclaredType)
			{
			case TYCHARPOINTER:
			case TYFLOATPOINTER:
			case TYINTPOINTER:
			case TYJUMPPOINTER:
			case TYREALPOINTER:
			case TYSHORTPOINTER:
			case TYLONGPOINTER:
			case TYWORDPOINTER:
				if (retValue.Tag > TYWORDPOINTER)
					{
					target->u.Int = ((retValue.Tag < TYTEXT) ? NIL : ((retValue.Tag == TYTEXT) ? (NUM)&retValue.u.Text[0] : (NUM)*retValue.u.Lambda->itsNilArray));
					target->Tag = TYNUM;
					}
				else
					{
					*target = retValue;
					}
				break;

			default:
				*target = retValue;
				break;
			}
		goto Fetch;
		break;
        
	case  VMRETURN:
		setLabel(LVMRETURN)
        ThrowOnEscape;
        switch (pcode.u.Am1)
            {
            case AMVOID:
                retValue = gCP->TObject_VOID;
                goto NestedReturn;
                break;
                
            case AMAVOFFSET:
                retValue = *((TVAL*)(Rp[AMAVOFFSET].u.Int + *(Ip++)));
                goto NestedReturn;
                break;
                
            case AMGVOFFSET:
                retValue = *((TVAL*)(Rp[AMGVOFFSET].u.Int + *(Ip++)));
                goto NestedReturn;
                break;
                
            case AMTVOFFSET:
                retValue = *((TVAL*)(Rp[AMTVOFFSET].u.Int + *(Ip++)));
                goto NestedReturn;
                break;
                
            case AMSVOFFSET:
                retValue = *((TVAL*)(Rp[AMSVOFFSET].u.Int + *(Ip++)));
                goto NestedReturn;
                break;
                
            case AMREGISTER:
                retValue = *((TVAL*)(Rp[AMREGISTER].u.Int + *(Ip++)));
                goto NestedReturn;
                break;
                
            case AMPVOFFSET:
                retValue = *((TVAL*)(Rp[AMPVOFFSET].u.Int + *(Ip++)));
                goto NestedReturn;
                break;
                
            case AMCVOFFSET:
                retValue = *((TVAL*)(Rp[AMCVOFFSET].u.Int + *(Ip++)));
                goto NestedReturn;
                break;
                
			case AMR07OFFST:
			case AMR08OFFST:
			case AMR09OFFST:
			case AMR10OFFST:
			case AMR11OFFST:
			case AMR12OFFST:
			case AMR13OFFST:
			case AMR14OFFST:
			case AMR15OFFST:
			case AMR16OFFST:
			case AMR17OFFST:
			case AMR18OFFST:
			case AMR19OFFST:
			case AMR20OFFST:
			case AMR21OFFST:
			case AMR22OFFST:
			case AMR23OFFST:
			case AMR24OFFST:
			case AMR25OFFST:
			case AMR26OFFST:
			case AMR27OFFST:
			case AMR28OFFST:
			case AMR29OFFST:
			case AMR30OFFST:
			case AMR31OFFST:
			case AMR32OFFST:
			case AMR33OFFST:
			case AMR34OFFST:
			case AMR35OFFST:
			case AMR36OFFST:
			case AMR37OFFST:
			case AMR38OFFST:
			case AMR39OFFST:
			case AMR40OFFST:
			case AMR41OFFST:
			case AMR42OFFST:
			case AMR43OFFST:
			case AMR44OFFST:
			case AMR45OFFST:
			case AMR46OFFST:
			case AMR47OFFST:
			case AMR48OFFST:
			case AMR49OFFST:
                retValue = *((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)));
                goto NestedReturn;
				break;

            case AMINTEGER:
                gTP->FVmscript_iargument.u.Int = *Ip;
                gTP->FVmscript_iargument.Tag = TYNUM;
                retValue = gTP->FVmscript_iargument;
                goto NestedReturn;
                break;
                
            default:
                retValue = gCP->FVmScript_ERROR_VMRETURN_MOD;
                goto NestedReturn;
                break; 
            }
        break;
        
    case  VMSELF:
		setLabel(LVMSELF)
        /* Assign the current procedure value to the target argument */
        target   = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)));
        target->Tag = self->itsObjectType;
        target->u.Object = (TObject*)self;
		goto Fetch;
        break;

	case  VMSETDICKEY:
		setLabel(LVMSETDICKEY)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (isObject(source))
			BondArray((retValue))[argument->u.Int].Key = source->u.Object;
		else
			BondArray((retValue))[argument->u.Int].Key = NIL;
		goto Fetch;
		break;
	
	case  VMSETSTRKEY:
		setLabel(LVMSETSTRKEY)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (isObject(source))
			BindArray((retValue))[argument->u.Int].Key = source->u.Object;
		else
			BindArray((retValue))[argument->u.Int].Key = NIL;
		goto Fetch;
		break;
	
	case  VMSETDIRKEY:
		setLabel(LVMSETDIRKEY)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		PBindArray((retValue))[argument->u.Int].Key = *source;
		goto Fetch;
		break;
	
	case  VMSETDIRVALUE:
		setLabel(LVMSETDIRVALUE)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		PBindArray((retValue))[argument->u.Int].Value = *source;
		goto Fetch;
		break;
	
	case  VMSETDICVALUE:
		setLabel(LVMSETDICVALUE)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		BondArray((retValue))[argument->u.Int].Value = *source;
		goto Fetch;
		break;
	
	case  VMSETSTRVALUE:
		setLabel(LVMSETSTRVALUE)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		BindArray((retValue))[argument->u.Int].Value = *source;
		goto Fetch;
		break;
	
	case  VMSETBITVECTOR:
		setLabel(LVMSETBITVECTOR)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (source->u.Int == 1)
			BitArray(retValue)[argument->u.Int/8] |= gCP->TBitVector_OrMasks[(argument->u.Int%8)];
		else
			BitArray(retValue)[argument->u.Int/8] &= gCP->TBitVector_AndMasks[(argument->u.Int%8)];
		goto Fetch;
		break;
	
	case  VMSETSTRING:
		setLabel(LVMSETSTRING)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		CharArray((retValue))[argument->u.Int] = source->u.Char;
		goto Fetch;
		break;
	
	case  VMSETBYTVECTOR:
		setLabel(LVMSETBYTVECTOR)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		ByteArray((retValue))[argument->u.Int] = source->u.Char;
		goto Fetch;
		break;
	
	case  VMSETOBJVECTOR:
		setLabel(LVMSETOBJVECTOR)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (isObject(&retValue))
			ObjArray((retValue))[argument->u.Int] = source->u.Object;
		else
			ObjArray((retValue))[argument->u.Int] = NIL;
		goto Fetch;
		break;
	
	case  VMSETPCDVECTOR:
		setLabel(LVMSETPCDVECTOR)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		PcodeArray((retValue))[argument->u.Int] = source->u.Int;
		goto Fetch;
		break;
	
	case  VMSETINTVECTOR:
		setLabel(LVMSETINTVECTOR)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		IntArray((retValue))[argument->u.Int] = source->u.Int;
		goto Fetch;
		break;
	
	case  VMSETFLTVECTOR:
		setLabel(LVMSETFLTVECTOR)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		FloatArray((retValue))[argument->u.Int] = source->u.Real;
		goto Fetch;
		break;
	
	case  VMSETNUMMATRIX:
		setLabel(LVMSETNUMMATRIX)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		RealMatrix((retValue))[argument->u.Int] = source->u.Real;
		goto Fetch;
		break;
	
	case  VMSETNUMVECTOR:
		setLabel(LVMSETNUMVECTOR)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		RealArray((retValue))[argument->u.Int] = source->u.Real;
		goto Fetch;
		break;
	
	case  VMSETMATRIX:
		setLabel(LVMSETMATRIX)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		TvalMatrix((retValue))[argument->u.Int] = *source;
		goto Fetch;
		break;
	
	case  VMSETVECTOR:
		setLabel(LVMSETVECTOR)
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		TvalArray((retValue))[argument->u.Int] = *source;
		goto Fetch;
		break;
	
	case  VMSET:
		setLabel(LVMSET)
        /*  Load the address of each of the three operands */
        index	    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source		= (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		/*  Manage the simple integer indices as fast a possible. */
		if ((index->Tag == TYNUM) && (index->u.Int >= 0))
			{
			switch (target->Tag)
				{
				case TYNUM:
					if (index->Tag == TYNUM)
						{
						switch(target->DeclaredType)
							{
							case TYCHARPOINTER:
								((LpCHAR)target->u.Int)[index->u.Int] = (CHAR)source->u.Int;
								break;

							case TYFLOATPOINTER:
								((LpFLOAT)target->u.Int)[index->u.Int] = (FLOAT)source->u.Real;
								break;

							case TYINTPOINTER:
							case TYJUMPPOINTER:
								((LpNUM)target->u.Int)[index->u.Int] = (NUM)source->u.Int;
								break;

							case TYREALPOINTER:
								((LpREAL)target->u.Int)[index->u.Int] = (REAL)source->u.Real;
								break;

							case TYSHORTPOINTER:
								((LpSHORT)target->u.Int)[index->u.Int] = (SHORT)source->u.Int;
								break;

							case TYLONGPOINTER:
								((LpNUM32)target->u.Int)[index->u.Int] = (SHORT)source->u.Int;
								break;

							case TYWORDPOINTER:
								((LpTVAL)target->u.Int)[index->u.Int] = *source;
								break;

							default:
								retValue = gCP->TObject_ERROR_INVALID_ARGLIST;
								break;
							}
						}
					else
						retValue = gCP->TObject_ERROR_INVALID_ARGLIST;
				
				goto Fetch;
				break;

				case TYBYTEVECTOR:
					if (index->u.Int < target->u.ByteVector->itsMaxItemIndex)
						{
						if ((source->Tag == TYCHAR) || (source->Tag == TYNUM))
							{
							ByteArray(*target)[index->u.Int] = (CHAR)source->u.Int;
							goto Fetch;
							}
						}
					break;

				case TYFLTVECTOR:
					if (index->u.Int < target->u.FltVector->itsMaxItemIndex)
						{						
						if (source->Tag == TYREAL)
							{
							FloatArray(*target)[index->u.Int] = (FLOAT)source->u.Real;
							goto Fetch;
							}
						else
						if (source->Tag == TYNUM)
							{
							FloatArray(*target)[index->u.Int] = (FLOAT)source->u.Int;
							goto Fetch;
							}
						}
					break;

				case TYINTVECTOR:
					if (index->u.Int < target->u.IntVector->itsMaxItemIndex)
						{						
						if (source->Tag == TYNUM)
							{
							IntArray(*target)[index->u.Int] = source->u.Int;
							goto Fetch;
							}
						else
						if (source->Tag == TYREAL)
							{
							IntArray(*target)[index->u.Int] = (NUM)source->u.Real;
							goto Fetch;
							}
						}
					break;

				case TYSHORTVECTOR:
					if (index->u.Int < target->u.ShortVector->itsMaxItemIndex)
						{
						if (source->Tag == TYNUM)
							{
							ShortArray(*target)[index->u.Int] = (SHORT)source->u.Int;
							goto Fetch;
							}
						else
						if (source->Tag == TYREAL)
							{
							ShortArray(*target)[index->u.Int] = (SHORT)source->u.Real;
							goto Fetch;
							}
						}
					break;

				case TYLONGVECTOR:
					if (index->u.Int < target->u.LongVector->itsMaxItemIndex)
						{
						if (source->Tag == TYNUM)
							{
							LongArray(*target)[index->u.Int] = (NUM32)source->u.Int;
							goto Fetch;
							}
						else
						if (source->Tag == TYREAL)
							{
							LongArray(*target)[index->u.Int] = (NUM32)source->u.Real;
							goto Fetch;
							}
						}
					break;

				case TYNUMVECTOR:
					if (index->u.Int < target->u.NumVector->itsMaxItemIndex)
						{
						if (source->Tag == TYREAL)
							{
							RealArray(*target)[index->u.Int] = source->u.Real;
							goto Fetch;
							}
						else
						if (source->Tag == TYNUM)
							{
							RealArray(*target)[index->u.Int] = (REAL)source->u.Int;
							goto Fetch;
							}
						}
					break;

				case TYSTRING:
					if (index->u.Int < (target->u.String->itsMaxItemIndex - 1))
						{
						if ((source->Tag == TYCHAR) || (source->Tag == TYNUM))
							{
							CharArray(*target)[index->u.Int] = (char)source->u.Int;
							goto Fetch;
							}
						}
					break;
                
				case TYSTRUCTURE:
					if (index->u.Int < target->u.Vector->itsMaxItemIndex)
						{
						BindArray(*target)[index->u.Int].Value = *source;
						goto Fetch;
						}
					break;

				case TYVECTOR:
					if (index->u.Int < target->u.Vector->itsMaxItemIndex)
						{
						TvalArray(*target)[index->u.Int] = *source;
						goto Fetch;
						}
					break;
				}
			}
		retValue = (*(LpF3TVALS)_TObject_TypeSetIV1(target->Tag))(gCP,gTP,*target,*index,*source);
		if (retValue.Tag == TYERROR) goto NestedReturn;
		goto Fetch;
		break;
       
	case  VMTESTESCAPE:
		setLabel(LVMTESTESCAPE)
        /*  Load the address of each of the three operands */
		if ((*gCP->_Host_Escape)((POINTER)gCP,gTP)) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);
		goto Fetch;
		break;
	
    case vmregAbsNumber:
		setLabel(LvmregAbsNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = fabs((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))));
		goto Fetch;
		break;
 
    case vmregAddImmediate:
		setLabel(LvmregAddImmediate)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) += *(Ip++);
		goto Fetch;
		break;
 
    case vmregInteger:
		setLabel(LvmregInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregNumber:
		setLabel(LvmregNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregAddInteger:
		setLabel(LvmregAddInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) += (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregAddNumber:
		setLabel(LvmregAddNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) += (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregAddPointer:
		setLabel(LvmregAddPointer)
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) += ((*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))<<*(Ip++));
		goto Fetch;
		break;
 
    case vmregAndImmediate:
		setLabel(LvmregAndImmediate)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) &= *(Ip++);
		goto Fetch;
		break;
 
    case vmregAndInteger:
		setLabel(LvmregAndInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) &= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregCosNumber:
		setLabel(LvmregCosNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = cos((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))));
		goto Fetch;
		break;
 
    case vmregDivImmediate:
		setLabel(LvmregDivImmediate)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) /= *(Ip++);
		goto Fetch;
		break;
 
    case vmregDivInteger:
		setLabel(LvmregDivInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) /= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregDivNumber:
		setLabel(LvmregDivNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) /= (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregDivrImmediate:
		setLabel(LvmregDivrImmediate)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) %= *(Ip++);
		goto Fetch;
		break;
 
    case vmregDivrInteger:
		setLabel(LvmregDivrInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) %= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregDivrNumber:
		setLabel(LvmregDivrNumber)
		source = (LpTVAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD));
		target = (LpTVAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD));
        target->u.Real = modf((target->u.Real / source->u.Real), &gTP->FVmscript_Integer);
        target->u.Real = fabs(target->u.Real) * source->u.Real;
		goto Fetch;
		break;
 
    case vmregIncPointer:
		setLabel(LvmregIncPointer)
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) += (*(Ip+1) << *(Ip));Ip+=2;
		goto Fetch;
		break;
 
    case vmregJmpEQImmediate:
		setLabel(LvmregJmpEQImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) == m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpLTImmediate:
		setLabel(LvmregJmpLTImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) < m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpGTImmediate:
		setLabel(LvmregJmpGTImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) > m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpNEImmediate:
		setLabel(LvmregJmpNEImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) != m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpGEImmediate:
		setLabel(LvmregJmpGEImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) >= m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpLEImmediate:
		setLabel(LvmregJmpLEImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) <= m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpEQUImmediate:
		setLabel(LvmregJmpEQUImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) == (UNUM)m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpLTUImmediate:
		setLabel(LvmregJmpLTUImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) < (UNUM)m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpGTUImmediate:
		setLabel(LvmregJmpGTUImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) > (UNUM)m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpNEUImmediate:
		setLabel(LvmregJmpNEUImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) != (UNUM)m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpGEUImmediate:
		setLabel(LvmregJmpGEUImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) >= (UNUM)m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpLEUImmediate:
		setLabel(LvmregJmpLEUImmediate)
		m=*(Ip++);n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) <= (UNUM)m) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;	
	
	case vmregJmpEQInteger:
		setLabel(LvmregJmpEQInteger)
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) == (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpLTInteger:
		setLabel(LvmregJmpLTInteger)
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) < (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpGTInteger:
		setLabel(LvmregJmpGTInteger)
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) > (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpNEInteger:
		setLabel(LvmregJmpNEInteger)
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) != (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpGEInteger:
		setLabel(LvmregJmpGEInteger)
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) >= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpLEInteger:
		setLabel(LvmregJmpLEInteger)
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) <= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpEQUInteger:
		setLabel(LvmregJmpEQUInteger)
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) == (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpLTUInteger:
		setLabel(LvmregJmpLTUInteger)
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) < (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpGTUInteger:
		setLabel(LvmregJmpGTUInteger)
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) > (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpNEUInteger:
		setLabel(LvmregJmpNEUInteger)
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) != (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpGEUInteger:
		setLabel(LvmregJmpGEUInteger)
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) >= (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpLEUInteger:
		setLabel(LvmregJmpLEUInteger)
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) <= (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpEQNumber:
		setLabel(LvmregJmpEQNumber)
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) == (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpLTNumber:
		setLabel(LvmregJmpLTNumber)
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) < (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpGTNumber:
		setLabel(LvmregJmpGTNumber)
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) > (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpNENumber:
		setLabel(LvmregJmpNENumber)
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) != (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpGENumber:
		setLabel(LvmregJmpGENumber)
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) >= (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJmpLENumber:
		setLabel(LvmregJmpLENumber)
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) <= (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = (NUM*)&pcodes[n];
		goto Fetch;
		break;
 
    case vmregJump:
		setLabel(LvmregJump)
		Ip = (LpNUM)(*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregLoadAddress:
		setLabel(LvmregLoadAddress)
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = (NUM)(Rp[pcode.u.Am1].u.Int + *(Ip++));
		goto Fetch;
		break;
 
    case vmregLoadInteger:
		setLabel(LvmregLoadInteger)
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)))->u.Int;
		goto Fetch;
		break;
 
    case vmregLoadTail:
		setLabel(LvmregLoadTail)
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = asTail(((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++))));
		goto Fetch;
		break;
 
    case vmregLoadDeclType:
		setLabel(LvmregLoadDeclType)
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)))->DeclaredType;
		goto Fetch;
		break;
 
    case vmregLoadType:
		setLabel(LvmregLoadType)
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)))->Tag;
		goto Fetch;
		break;
 
    case vmregLoadJmpPointer:
		setLabel(LvmregLoadJmpPointer)
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = (NUM)&pcodes[*(Ip++)];
		goto Fetch;
		break;
 
    case vmregLoadNumber:
		setLabel(LvmregLoadNumber)
		*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)))->u.Real;
		goto Fetch;
		break;
 
    case vmregLogNumber:
		setLabel(LvmregLogNumber)
		*((LpREAL)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))) = log(*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))/log(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregMoveImmediate:
		setLabel(LvmregMoveImmediate)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(Ip++);
		goto Fetch;
		break;
 
    case vmregMoveInteger:
		setLabel(LvmregMoveInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregMoveNumber:
		setLabel(LvmregMoveNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregMulImmediate:
		setLabel(LvmregMulImmediate)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) *= *(Ip++);
		goto Fetch;
		break;
 
    case vmregMulInteger:
		setLabel(LvmregMulInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) *= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregMulNumber:
		setLabel(LvmregMulNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) *= (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregObjPointer:
		setLabel(LvmregObjPointer)
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++));
        target      = (TVAL*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD));
		target->u.Int = ((argument->Tag < TYTEXT) ? NIL : ((argument->Tag == TYTEXT) ? (NUM)&argument->u.Text[0] : (NUM)*argument->u.Lambda->itsNilArray));
		target->Tag = TYNUM;
		goto Fetch;
		break;
 
    case vmregObjLength:
		setLabel(LvmregObjLength)
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++));
        target      = (TVAL*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD));
		target->u.Int = ((argument->Tag < TYTEXT) ? 0 : ((argument->Tag == TYTEXT) ? (NUM)strlen(&argument->u.Text[0]) : (NUM)argument->u.Lambda->itsMaxItemIndex));
		target->Tag = TYNUM;
		goto Fetch;
		break;
 
    case vmregOrImmediate:
		setLabel(LvmregOrImmediate)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) |= *(Ip++);
		goto Fetch;
		break;
 
    case vmregOrInteger:
		setLabel(LvmregOrInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) |= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregPwrNumber:
		setLabel(LvmregPwrNumber)
		*((LpREAL)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))) = pow(*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))),*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregRefCharacter:
		setLabel(LvmregRefCharacter)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpCHAR*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregRefFloat:
		setLabel(LvmregRefFloat)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpFLOAT*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregRefInteger:
		setLabel(LvmregRefInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpNUM*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregRefNumber:
		setLabel(LvmregRefNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpREAL*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregRefShort:
		setLabel(LvmregRefShort)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpSHORT*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregRefLong:
		setLabel(LvmregRefLong)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpNUM32*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregRefWord:
		setLabel(LvmregRefWord)
        (*((LpTVAL)(Rp[pcode.u.Am2].u.Int + *(Ip++)))) = *(*((LpTVAL*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;

    case vmregRefXCharacter:
		setLabel(LvmregRefXCharacter)
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpCHAR*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		goto Fetch;
		break;
 
    case vmregRefXFloat:
		setLabel(LvmregRefXFloat)
		(*((LpREAL)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpFLOAT*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		goto Fetch;
		break;
 
    case vmregRefXInteger:
		setLabel(LvmregRefXInteger)
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpNUM*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		goto Fetch;
		break;

     case vmregRefXLong:
		setLabel(LvmregRefXLong)
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpNUM32*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		goto Fetch;
		break;
 
    case vmregRefXNumber:
		setLabel(LvmregRefXNumber)
		(*((LpREAL)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpREAL*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		goto Fetch;
		break;
 
    case vmregRefXShort:
		setLabel(LvmregRefXShort)
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpSHORT*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		goto Fetch;
		break;
 
	case vmregRefXWord:
		setLabel(LvmregRefXWord)
        (*((LpTVAL)(Rp[pcode.u.Am3].u.Int + *(Ip++)))) = (*((LpTVAL*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		goto Fetch;
		break;

    case vmregRunInHardware:
		setLabel(LvmregRunInHardware)
		m = *(Ip++); /* Load the command argument */
		goto Fetch;
		break;

    case vmregSaveInteger:
		setLabel(LvmregSaveInteger)
		(target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))->u.Int = *((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)));
		target->Tag = TYNUM;
		goto Fetch;
		break;

    case vmregSaveUInteger:
		setLabel(LvmregSaveUInteger)
		(target = (TVAL*)(Rp[pcode.u.Am2].u.UInt + *(Ip++)))->u.UInt = *((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)));
		target->Tag = TYUNUM;
		goto Fetch;
		break;
 
    case vmregSaveTail:
		setLabel(LvmregSaveTail)
		asTail((target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))) = *((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)));
		goto Fetch;
		break;
 
    case vmregSaveTailImmediate:
		setLabel(LvmregSaveTailImmediate)
		n = *(Ip++);asTail((target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))) = n;
		goto Fetch;
		break;
 
    case vmregSaveDeclType:
		setLabel(LvmregSaveDeclType)
		(target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))->DeclaredType = (CHAR)*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)));
		goto Fetch;
		break;
 
    case vmregSaveDeclTypeImmediate:
		setLabel(LvmregSaveDeclTypeImmediate)
		n = *(Ip++);(target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))->DeclaredType = (CHAR)n;
		goto Fetch;
		break;
 
    case vmregSaveNumber:
		setLabel(LvmregSaveNumber)
		(target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))->u.Real = *((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)));
		target->Tag = TYREAL;
		goto Fetch;
		break;
 
    case vmregSetCharImmediate:
		setLabel(LvmregSetCharImmediate)
		*(*((LpCHAR*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (CHAR)(*(Ip++));
		goto Fetch;
		break;
 
    case vmregSetCharacter:
		setLabel(LvmregSetCharacter)
		*(*((LpCHAR*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSetFloat:
		setLabel(LvmregSetFloat)
		*(*((LpFLOAT*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSetIntImmediate:
		setLabel(LvmregSetIntImmediate)
		*(*((LpNUM*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (NUM)(*(Ip++));
		goto Fetch;
		break;
 
    case vmregSetInteger:
		setLabel(LvmregSetInteger)
		*(*((LpNUM*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSetLongImmediate:
		setLabel(LvmregSetLongImmediate)
		*(*((LpNUM32*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (NUM32)(*(Ip++));
		goto Fetch;
		break;
 
    case vmregSetLong:
		setLabel(LvmregSetLong)
		*(*((LpNUM32*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSetNumber:
		setLabel(LvmregSetNumber)
		*(*((LpREAL*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSetShortImmediate:
		setLabel(LvmregSetShortImmediate)
		*(*((LpSHORT*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (SHORT)(*(Ip++));
		goto Fetch;
		break;
 
    case vmregSetShort:
		setLabel(LvmregSetShort)
		*(*((LpSHORT*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSetWord:
		setLabel(LvmregSetWord)
        *(*(LpTVAL*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = *((LpTVAL)(Rp[pcode.u.Am1].u.Int + *(Ip++)));
		goto Fetch;
		break;

    case vmregSetXCharImmediate:
		setLabel(LvmregSetXCharImmediate)
		(*((LpCHAR*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (CHAR)(*(Ip++));
		goto Fetch;
		break;
 
    case vmregSetXIntImmediate:
		setLabel(LvmregSetXIntImmediate)
		(*((LpNUM*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = *(Ip++);
		goto Fetch;
		break;
 
    case vmregSetXLongImmediate:
		setLabel(LvmregSetXLongImmediate)
		(*((LpNUM32*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (NUM32)(*(Ip++));
		goto Fetch;
		break;
 
    case vmregSetXShortImmediate:
		setLabel(LvmregSetXShortImmediate)
		(*((LpSHORT*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (SHORT)(*(Ip++));
		goto Fetch;
		break;
 
     case vmregSetXCharacter:
		setLabel(LvmregSetXCharacter)
		(*((LpCHAR*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSetXFloat:
		setLabel(LvmregSetXFloat)
		(*((LpFLOAT*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
   case vmregSetXInteger:
		setLabel(LvmregSetXInteger)
		(*((LpNUM*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
   case vmregSetXLong:
		setLabel(LvmregSetXLong)
		(*((LpNUM32*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSetXNumber:
		setLabel(LvmregSetXNumber)
		(*((LpREAL*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSetXShort:
		setLabel(LvmregSetXShort)
		(*((LpSHORT*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
	case vmregSetXWord:
		setLabel(LvmregSetXWord)
        (*((LpTVAL*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpTVAL)(Rp[pcode.u.Am1].u.Int + *(Ip++))));
		goto Fetch;
		break;

    case vmregShlImmediate:
		setLabel(LvmregShlImmediate)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) << *(Ip++);
		goto Fetch;
		break;
 
    case vmregShlInteger:
		setLabel(LvmregShlInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) << (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregShrImmediate:
		setLabel(LvmregShrImmediate)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) >> *(Ip++);
		goto Fetch;
		break;
 
    case vmregShrInteger:
		setLabel(LvmregShrInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) >> (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSinNumber:
		setLabel(LvmregSinNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = sin((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))));
		goto Fetch;
		break;
 
    case vmregSqrtNumber:
		setLabel(LvmregSqrtNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = sqrt((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))));
		goto Fetch;
		break;
 
    case vmregStringCompare:
		setLabel(LvmregStringCompare)
        /*  Load the address of each of the two regoffset operands */
        source   = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
		srcP = (source->Tag == TYTEXT) ? source->u.Text : CharArray(*source);
		argP = (argument->Tag == TYTEXT) ? argument->u.Text : CharArray(*argument);
		*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))) = (NUM)strcmp(srcP,argP);
		goto Fetch;
		break;
 
    case vmregStringiCompare:
		setLabel(LvmregStringiCompare)
        /*  Load the address of each of the two regoffset operands */
        source   = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
		srcP = (source->Tag == TYTEXT) ? source->u.Text : CharArray(*source);
		argP = (argument->Tag == TYTEXT) ? argument->u.Text : CharArray(*argument);
		{	//Add code to replace stricmp which is not always included in the run-time library.
			char * s = srcP;
			char * t = argP;
			for (; tolower(*s) == tolower(*t); s++, t++) //tolower should be a macro?
			{	if (*s == '\0')
				{ *((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))) = 0;
					goto Fetch;
				}
			}
			*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))) = (NUM)((tolower(*s) - tolower(*t)) < 0) ? -1 : 1;
		}
		goto Fetch;
		break;
 
    case vmregSubImmediate:
		setLabel(LvmregSubImmediate)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) -= *(Ip++);
		goto Fetch;
		break;
 
    case vmregSubInteger:
		setLabel(LvmregSubInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) -= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSubNumber:
		setLabel(LvmregSubNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) -= (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmregSubPointer:
		setLabel(LvmregSubPointer)
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) -= ((*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))<<*(Ip++));
		goto Fetch;
		break;
 
    case vmregTanNumber:
		setLabel(LvmregTanNumber)
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = sin((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))) / cos((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))); 
		goto Fetch;
		break;
 
    case vmregXorImmediate:
		setLabel(LvmregXorImmediate)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) ^= *(Ip++);
		goto Fetch;
		break;
 
    case vmregXorInteger:
		setLabel(LvmregXorInteger)
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) ^= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		goto Fetch;
		break;
 
    case vmvecBinary:
		setLabel(LvmvecBinary)
		m = *(Ip++); /* Load the operator argument */
		/* Switch on the operator argument  */
		switch (m) /* Switch on destination argument */
			{
			case 0: /* add */
				Vs[VsTopOfStack-1] += Vs[VsTopOfStack];
				break;

			case 1: /* div */
				Vs[VsTopOfStack-1] /= Vs[VsTopOfStack];
				break;

			case 3: /* mul */
				Vs[VsTopOfStack-1] *= Vs[VsTopOfStack];
				break;

			case 4: /* sub */
				Vs[VsTopOfStack-1] -= Vs[VsTopOfStack];
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		--VsTopOfStack;
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;

    case vmvecInitialize:
		setLabel(LvmvecInitialize)
		switch ((vecInitializeExtent = *(Ip++)))
			{
			case 0: /* argument */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				vecInitializeCount = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				vecInitializeLabel = (NUM)(Ip - (NUM*)pcodes);
				break;

			case 1: /* source */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				vecInitializeCount = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				vecInitializeLabel = (NUM)(Ip - (NUM*)pcodes);
				break;

			case 2: /* target */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				tarP	= (LpCHAR)(*((LpNUM)(REGP+(tarPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				tarInc  = (*((LpNUM)(REGP+(tarIncID<<BITSIZEOFAISWORD))));
				vecInitializeCount = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				vecInitializeLabel = (NUM)(Ip - (NUM*)pcodes);
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;

    case vmvecLoop:
		setLabel(LvmvecLoop)
		switch (vecInitializeExtent)
			{
			case 0: /* argument */
				argP += argInc;
				if ((--vecInitializeCount) != 0) Ip = (NUM*)&pcodes[vecInitializeLabel];
				break;

			case 1: /* source */
				argP += argInc;
				srcP += srcInc;
				if ((--vecInitializeCount) != 0) Ip = (NUM*)&pcodes[vecInitializeLabel];
				break;

			case 2: /* target */
				argP += argInc;
				srcP += srcInc;
				tarP += tarInc;
				if ((--vecInitializeCount) != 0) Ip = (NUM*)&pcodes[vecInitializeLabel];
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;

    case vmvecPop:
		setLabel(LvmvecPop)
		m = *(Ip++); /* Load the data type argument */
		n = *(Ip++); /* Load the distination argument */
		/* Switch on the destination argument  */
		switch (n) /* Switch on destination argument */
			{
			case 0: /* argument */
				target = (LpTVAL)argP;
				break;

			case 1: /* source */
				target = (LpTVAL)srcP;
				break;

			case 2: /* target */
				target = (LpTVAL)tarP;
				break;

			case 3: /* drop */
				target = (LpTVAL)&k;
				m = TYFLOAT;
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		/* Switch on the data type  */
		switch (m) /* Switch on the data type argument */
			{
			case TYFLOAT: /* Float */
				*((LpFLOAT)target) = (FLOAT)Vs[VsTopOfStack];
				break;

			case TYREAL: /* Number */
				*((LpREAL)target) = (REAL)Vs[VsTopOfStack];
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		--VsTopOfStack;  /* Decrement the top of Stack pointer */
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;

    case vmvecPopNumber:
		setLabel(LvmvecPopNumber)
		(*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) = Vs[VsTopOfStack--];		
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;

	case vmvecPush:
		setLabel(LvmvecPush)
		m = *(Ip++); /* Load the data type argument */
		n = *(Ip++); /* Load the source argument */
		/* Switch on the source argument  */
		switch (n) /* Switch on the source argument */
			{
			case 0: /* argument */
				target = (LpTVAL)argP;
				break;

			case 1: /* source */
				target = (LpTVAL)srcP;
				break;

			case 2: /* target */
				target = (LpTVAL)tarP;
				break;

			case 3: /* dup */
				target = (LpTVAL)&Vs[VsTopOfStack];
				m = TYREAL;
				break;

			case 4: /* mone */
				Vs[++VsTopOfStack] = -1.0;
				goto Fetch;
				break;

			case 5: /* one */
				Vs[++VsTopOfStack] = 1.0;
				goto Fetch;
				break;

			case 6: /* zero */
				Vs[++VsTopOfStack] = 0.0;
				goto Fetch;
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}

		/* Switch on the data type argument */
		switch (m) /* Switch on the data type argument */
			{
			case TYFLOAT: /* Float */
				Vs[++VsTopOfStack] = *((LpFLOAT)target);
				break;

			case TYREAL: /* Number */
				Vs[++VsTopOfStack] = *((LpREAL)target);
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;

    case vmvecPushNumber:
		setLabel(LvmvecPushNumber)
		Vs[++VsTopOfStack] = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));		
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;

    case vmvecNumScalar:
		setLabel(LvmvecNumScalar)
		switch (*(Ip++))
			{
			case 0: /* distance */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				n		= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				sr      = 0.0;
				for (;n > 0; argP+=argInc,srcP+=srcInc,--n) {am = *(LpREAL)srcP - *(LpREAL)argP; sr += (am * am); };
				(*((LpREAL)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = sqrt(sr);
				break;

			case 1: /* dot product */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				n		= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				sr      = 0.0;
				for (;n > 0; argP+=argInc,srcP+=srcInc,--n) sr += (*(LpREAL)srcP * *(LpREAL)argP);
				(*((LpREAL)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = sr;
				break;

			case 2: /* sum */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				n		= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				sr		= 0.0;
				for (;n > 0; argP+=argInc,--n) sr += *(LpREAL)argP;
				(*((LpREAL)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = sr;
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;
 
    case vmvecNumVector:
		setLabel(LvmvecNumVector)
		switch (*(Ip++))
			{
			case 0: /* add */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				tarP	= (LpCHAR)(*((LpNUM)(REGP+(tarPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				tarInc  = (*((LpNUM)(REGP+(tarIncID<<BITSIZEOFAISWORD))));
				n		= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				for (;n > 0; argP+=argInc,srcP+=srcInc,tarP+=tarInc,--n) *(LpREAL)tarP = (*(LpREAL)srcP + *(LpREAL)argP);
				break;

			case 1: /* div */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				tarP	= (LpCHAR)(*((LpNUM)(REGP+(tarPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				tarInc  = (*((LpNUM)(REGP+(tarIncID<<BITSIZEOFAISWORD))));
				n		= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				for (;n > 0; argP+=argInc,srcP+=srcInc,tarP+=tarInc,--n) *(LpREAL)tarP = (*(LpREAL)srcP / *(LpREAL)argP);
				break;

			case 2: /* mov */
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				tarP	= (LpCHAR)(*((LpNUM)(REGP+(tarPtrID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				tarInc  = (*((LpNUM)(REGP+(tarIncID<<BITSIZEOFAISWORD))));
				n		= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				for (;n > 0; srcP+=srcInc,tarP+=tarInc,--n) *(LpREAL)tarP = *(LpREAL)srcP;
				break;

			case 3: /* mul */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				tarP	= (LpCHAR)(*((LpNUM)(REGP+(tarPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				tarInc  = (*((LpNUM)(REGP+(tarIncID<<BITSIZEOFAISWORD))));
				n		= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				for (;n > 0; argP+=argInc,srcP+=srcInc,tarP+=tarInc,--n) *(LpREAL)tarP = (*(LpREAL)srcP * *(LpREAL)argP);
				break;

			case 4: /* sub */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				tarP	= (LpCHAR)(*((LpNUM)(REGP+(tarPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				tarInc  = (*((LpNUM)(REGP+(tarIncID<<BITSIZEOFAISWORD))));
				n		= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				for (;n > 0; argP+=argInc,srcP+=srcInc,tarP+=tarInc,--n) *(LpREAL)tarP = (*(LpREAL)srcP - *(LpREAL)argP);
				break;

			case 5: /* swp */
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				tarP	= (LpCHAR)(*((LpNUM)(REGP+(tarPtrID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				tarInc  = (*((LpNUM)(REGP+(tarIncID<<BITSIZEOFAISWORD))));
				n		= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				for (;n > 0; srcP+=srcInc,tarP+=tarInc,--n) {k = *(LpREAL)tarP;*(LpREAL)tarP = *(LpREAL)srcP;*(LpREAL)srcP =k;};
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;
 
    case vmvecSetIncrements:
		setLabel(LvmvecSetIncrements)
		argIncID = pcode.u.Am1;
		srcIncID = pcode.u.Am2;
		tarIncID = pcode.u.Am3;
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;
 
    case vmvecSetPointers:
		setLabel(LvmvecSetPointers)
		argPtrID = pcode.u.Am1;
		srcPtrID = pcode.u.Am2;
		tarPtrID = pcode.u.Am3;
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;
 
    case vmvecSwapCC:
		setLabel(LvmvecSwapCC)
		m = *(Ip++); /* Load the condition argument */
		/* Switch on the contition argument  */
		switch (m) /* Switch on destination argument */
			{
			case 0: /* lt */
				if (Vs[VsTopOfStack] < Vs[VsTopOfStack-1])
					{
					sr = Vs[VsTopOfStack-1];
					Vs[VsTopOfStack-1] = Vs[VsTopOfStack];
					Vs[VsTopOfStack] = sr;
					}
				break;

			case 1: /* le */
				if (Vs[VsTopOfStack] <= Vs[VsTopOfStack-1])
					{
					sr = Vs[VsTopOfStack-1];
					Vs[VsTopOfStack-1] = Vs[VsTopOfStack];
					Vs[VsTopOfStack] = sr;
					}
				break;

			case 2: /* eq */
				if (Vs[VsTopOfStack] == Vs[VsTopOfStack-1])
					{
					sr = Vs[VsTopOfStack-1];
					Vs[VsTopOfStack-1] = Vs[VsTopOfStack];
					Vs[VsTopOfStack] = sr;
					}
				break;

			case 3: /* ne */
				if (Vs[VsTopOfStack] != Vs[VsTopOfStack-1])
					{
					sr = Vs[VsTopOfStack-1];
					Vs[VsTopOfStack-1] = Vs[VsTopOfStack];
					Vs[VsTopOfStack] = sr;
					}
				break;

			case 4: /* ge */
				if (Vs[VsTopOfStack] >= Vs[VsTopOfStack-1])
					{
					sr = Vs[VsTopOfStack-1];
					Vs[VsTopOfStack-1] = Vs[VsTopOfStack];
					Vs[VsTopOfStack] = sr;
					}
				break;

			case 5: /* gt */
				if (Vs[VsTopOfStack] > Vs[VsTopOfStack-1])
					{
					sr = Vs[VsTopOfStack-1];
					Vs[VsTopOfStack-1] = Vs[VsTopOfStack];
					Vs[VsTopOfStack] = sr;
					}
				break;

			case 6: /* true */
				sr = Vs[VsTopOfStack-1];
				Vs[VsTopOfStack-1] = Vs[VsTopOfStack];
				Vs[VsTopOfStack] = sr;
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;

    case vmvecUnary:
		setLabel(LvmvecUnary)
		m = *(Ip++); /* Load the operator argument */
		/* Switch on the operator argument  */
		switch (m) /* Switch on destination argument */
			{
			case 0: /* abs */
				Vs[VsTopOfStack] = fabs(Vs[VsTopOfStack]);
				break;

			case 1: /* cos */
				Vs[VsTopOfStack] = cos(Vs[VsTopOfStack]);
				break;

			case 2: /* dbl */
				Vs[VsTopOfStack] += Vs[VsTopOfStack];
				break;

			case 3: /* dec */
				Vs[VsTopOfStack] -= 1.0;
				break;

			case 4: /* inc */
				Vs[VsTopOfStack] += 1.0;
				break;

			case 5: /* cos */
				Vs[VsTopOfStack] = sin(Vs[VsTopOfStack]);
				break;

			case 6: /* sqr */
				Vs[VsTopOfStack] *= Vs[VsTopOfStack];
				break;

			case 7: /* sqrt */
				Vs[VsTopOfStack] = sqrt(Vs[VsTopOfStack]);
				break;

			case 8: /* tan */
				// Vs[VsTopOfStack] = tan(Vs[VsTopOfStack]);
				Vs[VsTopOfStack] = sin(Vs[VsTopOfStack]) / cos(Vs[VsTopOfStack]);
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		goto Fetch;
		break;
		
	default:
#ifdef _GCC
#elif _MSVC
      setLabel(IllegalInstruction)
#else
#error "The toolset for this build is not defined in the preprocessor defines for this configuration"
#endif

        retValue = gCP->FVmScript_ERROR_ILLEGAL_INSTRUCTION;
        goto NestedReturn;

#ifdef _MSVC
      IllegalValue:
        retValue = gCP->FVmScript_ERROR_ILLEGAL_VALUE;
        goto NestedReturn;
#endif

      IllegalDivide:
        retValue = gCP->FVmScript_ERROR_DIVIDE_BY_ZERO;
        goto NestedReturn;

#ifdef _MSVC
      OverFlowError:
        retValue = gCP->FVmScript_ERROR_OVERFLOW;
        goto NestedReturn;
#endif

	  /* This section was moved here when VMRECALC was removed */

	  NestedReturn:
		/* Do we have a poorly formed error object? */
		if ((retValue.Tag == TYERROR) && ((retValue.u.Error->itsMaxItemIndex == 0) || ((retValue.u.Error->itsCString == NULL) && (retValue.u.Error->itsImmediatePtr != NULL))))
			{
			retValue = TERROR("!<empty> error message encountered!");
			}
		/*  If there is an error event and an error event manager assigned. */
		/*  Note: We do this only if error trace is NOT turned on or Debug is suspended. */
		if ((retValue.Tag == TYERROR) && (onErrorHandler.Tag == TYLAMBDA))		
			{
			/* Reset the _error call tree vector. */
			gCP->FSmartbase_errorSym->itsGlobalValue = gTP->TObject_ErrorTree = gCP->Tval_VOID;
            gTP->TObject_ErrorSwt = FALSE;
			/* Call the assigned error event manager. */
			retValue = TSTRING(ErrorArray(retValue));
			retValue = FSmartbase_Evalv(gCP,gTP,onErrorHandler,1,&retValue);
			// Added the next 3 lines to avoid going into next if statement 
			TopOfStack = saveSi;
			EndRecursion;
			return(retValue);
			}
            
		/*  If the result value is an error, we need to construct the error tree. */
		if (retValue.Tag == TYERROR)
			{
			/*  Note:   We avoid trapping the Continuation in progress messages (!ConInProg!). */
			if (strcmp(ErrorArray(retValue),ErrorArray(gCP->TObject_ERROR_CONTINUATION)) != 0)
				{
				itmpValue.Tag = self->itsObjectType;
				itmpValue.u.Object = (TObject*)self;
				isource = FDebug_GetLambdaBind(gCP,gTP,1,&itmpValue);

				strcpy(gTP->TempBuffer,ErrorArray(retValue));
				// If this is the first error, append Call Chain string before appending the binding
				if( '!' == ErrorArray(retValue)[strlen(ErrorArray(retValue)) - 1] ){
					strcat(gTP->TempBuffer," Call Chain ");
				}

				if (isource.Tag == TYTEXT){
					sprintf(gTP->TempBuffer,"%s <- %s",gTP->TempBuffer,isource.u.Text);
				}
				else
				if (isource.Tag == TYSTRING){
					sprintf(gTP->TempBuffer,"%s <- %s",gTP->TempBuffer,CharArray(isource));
				}
				else
				{
					// Handle everything here: TYVOID goes here
					// Display Object Index
					sprintf(gTP->TempBuffer,"%s <- #<Lambda "INTFORMAT">",gTP->TempBuffer,asObject(&itmpValue)->itsObjectIndex);
				}

				retValue = TERROR(gTP->TempBuffer); 

	  			/*  Is this the first encounter with the error? */
				/*  If yes, we need to create a new error tree. */
				if (gTP->TObject_ErrorSwt == FALSE)
					{
					TSymbol_MakeUnique(gCP,gTP,"_errorMsg")->itsGlobalValue = retValue;
					TSymbol_MakeUnique(gCP,gTP,"_error")->itsGlobalValue = gTP->FVmscript_isource = TINT(0);
					TSymbol_MakeUnique(gCP,gTP,"_error")->itsGlobalValue = gTP->TObject_ErrorTree = FMake_Vector(gCP,gTP,1,&gTP->FVmscript_isource);
					gTP->TObject_ErrorSwt = TRUE;
					TVector_AddNewValue(gCP,gTP,gTP->TObject_ErrorTree,retValue);
					}
            
				/*  We now add our current procedure to the error tree. */
				if (proc != gTP->TLambda_TheProc)
					{
					gTP->FVmscript_iargument.Tag = proc->itsObjectType;
					gTP->FVmscript_iargument.u.Object = (TObject*)proc;
					gTP->FVmscript_isource = FDebug_GetLambdaBind(gCP,gTP,1,&gTP->FVmscript_iargument);
					if (gTP->FVmscript_isource.Tag == TYVOID)
						gTP->FVmscript_isource = gTP->FVmscript_iargument;
					gTP->FVmscript_iargument = FSmartbase_Eval(gCP,gTP,TFUNCTION(FMake_Vector),3,TINT(2),gTP->FVmscript_isource,TINT((Ip - &atHMInt(Pc->itsInstructionArray,0))));
					TVector_AddNewValue(gCP,gTP,gTP->TObject_ErrorTree,gTP->FVmscript_iargument);
					}

				/*  Throw up the debug dialog if we are in debug mode. */
				if ((gTP->DebugFlags != 0) && (gTP->DebugSuspended == 0))
		                FVmScript2_DebugManager(gCP,gTP,self,argc,argv,Pc,Pv,Sv,Rp,Vs,VsTopOfStack,Fb,Ip,TRUE,retValue);
				}
			}

		/*  We are returning from a single Procedure object */
		ThrowOnEscape;
		TopOfStack = saveSi;
		EndRecursion;
		if (TESTON)
			FMemory_SystemDiagnostic(gCP,gTP,1,&gCP->Tval_FALSE);
		return(retValue);
		break;

    }
goto Fetch;

/*  =============================================================== */
/*  Debug nested break routine									    */
/*  =============================================================== */
/*  Resume normal processing. */
if (gTP->FVmscript_NestedDebugSwt == TRUE) 
	goto NestedDebugResume;
else
	goto DebugResume;

strcpy(gTP->TempBuffer,"!JIT: found vm instruction which is invalid in hardware mode [");
n = strlen(gTP->TempBuffer);
FDebug_INSToString(gCP,gTP,pcode.u.Pcode,&gTP->TempBuffer[n]);
strcat(gTP->TempBuffer,"]!");
return(TERROR(gTP->TempBuffer));

ErrorTooManyRegisters:
return(TERROR("!VM: found Lambda with too many register variables!"));

ErrorMissingPcodes:
return(gCP->FVmScript_ERROR_MISSING_PCODES);

ErrorNotAnLambda:
return(gCP->FVmScript_ERROR_NOT_AN_Lambda);

ErrorInvalidArglist:
return(gCP->TObject_ERROR_INVALID_ARGLIST);

ErrorIllegalInstruction:
return(gCP->FVmScript_ERROR_ILLEGAL_INSTRUCTION);
}
