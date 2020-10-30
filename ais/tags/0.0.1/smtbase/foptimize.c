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

#define _C_FOPTIMIZE
#define _SMARTBASE
#if 0 
FOptimize.c

This file contains some of the procedures required to support the Lisp "compile" command.
The data structures used to control the basic compilation process are allocated and managed here.
Except for the handling of Special Forms the procedures in this file provide all support for
the generation of non-optimized VMSCRIPT procedure objects.

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "foptimize.h"
#include "fcompile.h"
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

/*  The compilation of nested lambda objects is a two phase process, we use FOptimize_lambdaSwitch */
/*  to keep track of which phase is occuring for the lambda object currently under compilation. */
/*  For a description of lambda objects see the Lisp Ref. Guide under lambda and define. */
 
/*  FOptimize_LambdaNest is a counter which is incremented everytime we start the compilation */
/*  of a nested lambda object and decremented every time we complete that process. */

/*--------------------------------------------------------------------------------------- */
#if 0
FOptimize_OptimizePcode

This function provides an optimizing interface between the compiler and the FVmCode_GeneratePcode
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

TVAL    FOptimize_OptimizePcode(LpXCONTEXT gCP, LpTHREAD gTP, TVector* compilerState, NUM argc,TVAL argx[])
{
NUM                 Opcode;
TVAL                wrdOpcode;
TVAL                movOpcode;
NUM					bitShift;
NUM                 argIndex;
NUM                 modIndex;
UNUM                modifier[3];
UNUM                offset[3];
UNUM                disp[3];
NUM                 dcltype[3];
NUM			        preftype[3];
NUM                 regtype[3];
NUM					n;
LpCHAR				symbolPtr;
NUM					targetTyp;
TStructure*			Rv;
TVAL				Rs;
TStructure*			Tv;
TVAL				Ts;
TStructure*			Av;
TVAL				As;
TStructure*			Sv;
TVAL				Ss;
TStructure*			Cv;
TVAL				Cs;
TStructure*			Pv;
TVAL				Ps;
TPcodeVector*		Pc;
TVAL				argv[8];
TVAL				opnd[3];
TVAL				lnE;
TVAL				prmv[3];
TVAL				index;
TVAL				temporary;
TVAL				pointer;
BOLE				tempReg[3];
BOLE				cons[3];
TVAL				consValue[3];
TVAL				pcodeVector;
TVAL				newOpcode;
TVAL				newOpnd[3];
TVAL				opndAMVOID;
StartFrame
DeclareOBJ(TStructure,cVars);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TPair,tmpPair);
DeclareTVAL(dbgSourceLinesInstr);
DeclareTVAL(pcodeInstr);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

/* *********************************************************************************/  
/*  Build a reference to the lnE register (see FCompile_SetupRegs)                 */
/* *********************************************************************************/ 
lnE.Tag = TYNUM;
lnE.u.Int = 10;
lnE.Modifier = AMREGISTER;
lnE.Offset = 10;

/* *******************************************************/  
/*  Build compiler debugger information (if appropriate) */
/* *******************************************************/  
if (gTP->FCompile_GenerateDebugInfo == TRUE)
	{
	if (gTP->FCompile_DebugCurrentSourceLineIndex.Tag == TYNUM)
		{
		/* Make sure there is an Interfaces Structure in place.  */
		if (_In(compilerState) == NIL)
			{
			_In(compilerState) = TStructure_New(gCP,gTP);
			}
		/* Make sure there is an In.dbgSourceLinesInstr Directory in place.  */
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
		/* Make sure there is an In.dbgSourceLinesInstr Directory in place.  */
		*pcodeInstr = FSmartbase_Ref(gCP,gTP,2,*dbgSourceLinesInstr,gTP->FCompile_DebugCurrentSourceLineIndex);
		ExitOnError(*pcodeInstr);
		if (pcodeInstr->Tag != TYNUM)
			{
			/* Associate index of the current source line with the current pcode instruction displacement.  */
			pcodeInstr->u.Int = _Pc(compilerState)->itsCurItemIndex;
			pcodeInstr->Tag = TYNUM;
			*ret = FSmartbase_Set(gCP,gTP,3,*dbgSourceLinesInstr,gTP->FCompile_DebugCurrentSourceLineIndex,*pcodeInstr);
			ExitOnError(*ret);
			}
		}
	}

/* ***********************************************************************/  
/*  Copy the incoming arguments so that any modifications will not cause */
/*  the original caller's arguments to change.                           */
/* ***********************************************************************/  

for (argIndex = 0; argIndex < argc; ++argIndex)
	{
	argv[argIndex] = argx[argIndex];
	}
newOpcode.Tag = movOpcode.Tag = wrdOpcode.Tag = TYNUM;

/* *********************************************************************************/  
/* Set up the base address pointers for use in later optimization algorithms.      */
/* *********************************************************************************/  

/*  Make sure we save the register variables pointer. */
Rs = TOBJ(_Rv(compilerState));
if ((Rs.Tag == TYSTRUCTURE) && (Rs.u.Structure->itsMaxItemIndex > MAXREGISTERCNT))
	{
	FrameExit(TERROR("!compile: an Lambda cannot have more than 50 register variables!"));
	}
if (Rs.Tag != TYSTRUCTURE)
	{
	Rv = NIL;
	Rs = FCompile_SetupRegs(gCP,gTP,_CurProc(compilerState).u.Lambda);
	Rv = Rs.u.Structure;
	}
else
	Rv = Rs.u.Structure;

/*  Make sure we save the temporary variables pointer. */
Ts = TOBJ(_Tv(compilerState));
if (Ts.Tag != TYSTRUCTURE)
	{
	Tv = NIL;
	Ts.u.Structure = TStructure_New(gCP,gTP);
	Ts.Tag = TYSTRUCTURE;
	Tv = Ts.u.Structure;
	}
else
	Tv = Ts.u.Structure;

/*  Make sure we save the argument variables pointer. */
As = TOBJ(_Av(compilerState));
if (As.Tag != TYSTRUCTURE)
	Av = NIL;
else
	Av = As.u.Structure;

/*  Make sure we save the  constant variables pointer. */
Ss = TOBJ(_Sv(compilerState));
if (Ss.Tag != TYSTRUCTURE)
	Sv = NIL;
else
	Sv = Ss.u.Structure;

/*  Make sure we save the constant variables pointer. */
Cs = TOBJ(_Cv(compilerState));
if (Cs.Tag != TYSTRUCTURE)
	Cv = NIL;
else
	Cv = Cs.u.Structure;

/*  Make sure we save the pcode vector pointer. */
Ps = TOBJ(_Pc(compilerState));
if (Ps.Tag != TYPCODEVECTOR)
	Pc = NIL;
else
	Pc = Ps.u.PcodeVector;

/*  Make sure we save the persistant variables pointer. */
Ps = TOBJ(_Pv(compilerState));
if (Ps.Tag != TYSTRUCTURE)
	Pv = NIL;
else
	Pv = Ps.u.Structure;

/* *********************************************************************************/  
/* Extract the component parts of all input arguments.                             */
/* *********************************************************************************/  

/*  Make sure there are enough arguments. */
if (argc < 5)
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

/*  Make sure we save the PcodeVector argument. */
if (argv[0].Tag != TYPCODEVECTOR)
	{
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
	pcodeVector = argv[0];

/*  Make sure we save the opcode argument. */
if (argv[1].Tag != TYNUM)
	{
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
	wrdOpcode = argv[1];

if (argv[2].Tag != TYNUM) 
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
modifier[0] = argv[2].u.Int;
offset[0] = argv[5].Offset;
disp[0] = argv[5].u.Int;
if (argc <= 5)
	{
	opnd[0].Tag = TYVOID;
	opnd[0].u.Int = 0;
	opnd[0].Modifier = AMVOID;
	opnd[0].Offset = 0;
	dcltype[0] = TYVOID;
	preftype[0] = TYNUM;
	tempReg[0] = FALSE;
	}
else
	{
	if (modifier[0] != argv[5].Modifier) 
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	opnd[0] = argv[5];
	opnd[0].Modifier = modifier[0];
	opnd[0].Offset = offset[0];
	switch (modifier[0])
		{
		case AMAVOFFSET:
			dcltype[0] = n = BindArray(As)[offset[0]].Value.DeclaredType;
			dcltype[0] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[0] = BindArray(As)[offset[0]].Value.DeclaredType;
			tempReg[0] = FALSE;
			break;

		case AMTVOFFSET:
			dcltype[0] = n = BindArray(Ts)[offset[0]].Value.DeclaredType;
			dcltype[0] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[0] = BindArray(Ts)[offset[0]].Value.DeclaredType;
			tempReg[0] = FALSE;
			break;

		case AMPVOFFSET:
			dcltype[0] = n = BindArray(Ps)[offset[0]].Value.DeclaredType;
			dcltype[0] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[0] = BindArray(Ps)[offset[0]].Value.DeclaredType;
			tempReg[0] = FALSE;
			break;

		case AMCVOFFSET:
			dcltype[0] = n = BindArray(Cs)[offset[0]].Value.DeclaredType;
			dcltype[0] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[0] = BindArray(Cs)[offset[0]].Value.DeclaredType;
			tempReg[0] = FALSE;
			break;

		case AMSVOFFSET:
			dcltype[0] = n = BindArray(Ss)[offset[0]].Value.DeclaredType;
			dcltype[0] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[0] = BindArray(Ss)[offset[0]].Value.DeclaredType;
			tempReg[0] = FALSE;
			break;

		case AMREGISTER:
			dcltype[0] = n = BindArray(Rs)[offset[0]].Value.DeclaredType;
			dcltype[0] = ((n == TYREAL) ? TYREAL : TYNUM);
			preftype[0] = BindArray(Rs)[offset[0]].Value.DeclaredType;
			tempReg[0] = (strncmp(objSymbolArray(BindArray(Rs)[offset[0]].Key),"__R",3) == 0) ? TRUE : FALSE;
			break;

		default:
			if (opnd[0].DeclaredType == TYNUM) dcltype[0] = TYNUM;
			else if (opnd[0].DeclaredType == TYREAL) dcltype[0] = TYREAL;
			else if (opnd[0].DeclaredType == TYJUMPPOINTER) dcltype[0] = TYNUM;
			else if (opnd[0].DeclaredType == TYCHARPOINTER) dcltype[0] = TYNUM;
			else if (opnd[0].DeclaredType == TYFLOATPOINTER) dcltype[0] = TYREAL;
			else if (opnd[0].DeclaredType == TYINTPOINTER) dcltype[0] = TYNUM;
			else if (opnd[0].DeclaredType == TYREALPOINTER) dcltype[0] = TYREAL;
			else if (opnd[0].DeclaredType == TYSHORTPOINTER) dcltype[0] = TYNUM;
			else if (opnd[0].DeclaredType == TYLONGPOINTER) dcltype[0] = TYNUM;
			else if (opnd[0].DeclaredType == TYWORDPOINTER) dcltype[0] = TYTVAL;
			else dcltype[0] = TYTVAL;
			preftype[0] = dcltype[0];
			tempReg[0] = FALSE;
			break;
		}
	}

if (argv[3].Tag != TYNUM) 
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
modifier[1] = argv[3].u.Int;
offset[1] = argv[6].Offset;
disp[1] = argv[6].u.Int;
if (argc <= 6)
	{
	opnd[1].Tag = TYVOID;
	opnd[1].u.Int = 0;
	opnd[1].Modifier = AMVOID;
	opnd[1].Offset = 0;
	dcltype[1] = TYVOID;
	preftype[1] = TYNUM;
	tempReg[1] = FALSE;
	}
else
	{
	if (modifier[1] != argv[6].Modifier) 
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	opnd[1] = argv[6];
	opnd[1].Modifier = modifier[1];
	opnd[1].Offset = offset[1];
	switch (modifier[1])
		{
		case AMAVOFFSET:
			dcltype[1] = n = BindArray(As)[offset[1]].Value.DeclaredType;
			dcltype[1] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[1] = BindArray(As)[offset[1]].Value.DeclaredType;
			tempReg[1] = FALSE;
			break;

		case AMTVOFFSET:
			dcltype[1] = n = BindArray(Ts)[offset[1]].Value.DeclaredType;
			dcltype[1] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[1] = BindArray(Ts)[offset[1]].Value.DeclaredType;
			tempReg[1] = FALSE;
			break;

		case AMPVOFFSET:
			dcltype[1] = n = BindArray(Ps)[offset[1]].Value.DeclaredType;
			dcltype[1] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[1] = BindArray(Ps)[offset[1]].Value.DeclaredType;
			tempReg[1] = FALSE;
			break;

		case AMCVOFFSET:
			dcltype[1] = n = BindArray(Cs)[offset[1]].Value.DeclaredType;
			dcltype[1] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[1] = BindArray(Cs)[offset[1]].Value.DeclaredType;
			tempReg[1] = FALSE;
			break;

		case AMSVOFFSET:
			dcltype[1] = n = BindArray(Ss)[offset[1]].Value.DeclaredType;
			dcltype[1] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[1] = BindArray(Ss)[offset[1]].Value.DeclaredType;
			tempReg[1] = FALSE;
			break;

		case AMREGISTER:
			dcltype[1] = n = BindArray(Rs)[offset[1]].Value.DeclaredType;
			dcltype[1] = ((n == TYREAL) ? TYREAL : TYNUM);
			preftype[1] = BindArray(Rs)[offset[1]].Value.DeclaredType;
			tempReg[1] = (strncmp(objSymbolArray(BindArray(Rs)[offset[1]].Key),"__R",3) == 0) ? TRUE : FALSE;
			break;

		default:
			if (opnd[1].DeclaredType == TYNUM) dcltype[1] = TYNUM;
			else if (opnd[1].DeclaredType == TYREAL) dcltype[1] = TYREAL;
			else if (opnd[1].DeclaredType == TYJUMPPOINTER) dcltype[1] = TYNUM;
			else if (opnd[1].DeclaredType == TYCHARPOINTER) dcltype[1] = TYNUM;
			else if (opnd[1].DeclaredType == TYFLOATPOINTER) dcltype[1] = TYREAL;
			else if (opnd[1].DeclaredType == TYINTPOINTER) dcltype[1] = TYNUM;
			else if (opnd[1].DeclaredType == TYREALPOINTER) dcltype[1] = TYREAL;
			else if (opnd[1].DeclaredType == TYSHORTPOINTER) dcltype[1] = TYNUM;
			else if (opnd[1].DeclaredType == TYLONGPOINTER) dcltype[1] = TYNUM;
			else if (opnd[1].DeclaredType == TYWORDPOINTER) dcltype[1] = TYTVAL;
			else dcltype[1] = TYTVAL;
			preftype[1] = dcltype[1];
			tempReg[1] = FALSE;
			break;
		}
	}

if (argv[4].Tag != TYNUM) 
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
modifier[2] = argv[4].u.Int;
offset[2] = argv[7].Offset;
disp[2] = argv[7].u.Int;
if (argc <= 7)
	{
	opnd[2].Tag = TYVOID;
	opnd[2].u.Int = 0;
	opnd[2].Modifier = AMVOID;
	opnd[2].Offset = 0;
	dcltype[2] = TYVOID;
	preftype[2] = TYNUM;
	tempReg[2] = FALSE;
	}
else
	{
	if (modifier[2] != argv[7].Modifier) 
		FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	opnd[2] = argv[7];
	opnd[2].Modifier = modifier[2];
	opnd[2].Offset = offset[2];
	switch (modifier[2])
		{
		case AMAVOFFSET:
			dcltype[2] = n = BindArray(As)[offset[2]].Value.DeclaredType;
			dcltype[2] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[2] = BindArray(As)[offset[2]].Value.DeclaredType;
			tempReg[2] = FALSE;
			break;

		case AMTVOFFSET:
			dcltype[2] = n = BindArray(Ts)[offset[2]].Value.DeclaredType;
			dcltype[2] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[2] = BindArray(Ts)[offset[2]].Value.DeclaredType;
			tempReg[2] = FALSE;
			break;

		case AMPVOFFSET:
			dcltype[2] = n = BindArray(Ps)[offset[2]].Value.DeclaredType;
			dcltype[2] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[2] = BindArray(Ps)[offset[2]].Value.DeclaredType;
			tempReg[2] = FALSE;
			break;

		case AMCVOFFSET:
			dcltype[2] = n = BindArray(Cs)[offset[2]].Value.DeclaredType;
			dcltype[2] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[2] = BindArray(Cs)[offset[2]].Value.DeclaredType;
			tempReg[2] = FALSE;
			break;

		case AMSVOFFSET:
			dcltype[2] = n = BindArray(Ss)[offset[2]].Value.DeclaredType;
			dcltype[2] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
			preftype[2] = BindArray(Ss)[offset[2]].Value.DeclaredType;
			tempReg[2] = FALSE;
			break;

		case AMREGISTER:
			dcltype[2] = n = BindArray(Rs)[offset[2]].Value.DeclaredType;
			dcltype[2] = ((n == TYREAL) ? TYREAL : TYNUM);
			preftype[2] = BindArray(Rs)[offset[2]].Value.DeclaredType;
			tempReg[2] = (strncmp(objSymbolArray(BindArray(Rs)[offset[2]].Key),"__R",3) == 0) ? TRUE : FALSE;
			break;

		default:
			if (opnd[2].DeclaredType == TYNUM) dcltype[2] = TYNUM;
			else if (opnd[2].DeclaredType == TYREAL) dcltype[2] = TYREAL;
			else if (opnd[2].DeclaredType == TYJUMPPOINTER) dcltype[2] = TYNUM;
			else if (opnd[2].DeclaredType == TYCHARPOINTER) dcltype[2] = TYNUM;
			else if (opnd[2].DeclaredType == TYFLOATPOINTER) dcltype[2] = TYREAL;
			else if (opnd[2].DeclaredType == TYINTPOINTER) dcltype[2] = TYNUM;
			else if (opnd[2].DeclaredType == TYREALPOINTER) dcltype[2] = TYREAL;
			else if (opnd[2].DeclaredType == TYSHORTPOINTER) dcltype[2] = TYNUM;
			else if (opnd[2].DeclaredType == TYLONGPOINTER) dcltype[2] = TYNUM;
			else if (opnd[2].DeclaredType == TYWORDPOINTER) dcltype[2] = TYTVAL;
			else dcltype[2] = TYTVAL;
			preftype[2] = dcltype[2];
			tempReg[2] = FALSE;
			break;
		}
	}

opndAMVOID.Tag = TYVOID;
opndAMVOID.u.Int = 0;
opndAMVOID.Modifier = AMVOID;
opndAMVOID.Offset = 0;

/* ***********************************************************************/  
/*  Add this latest instruction to the compiler state vector.            */
/* ***********************************************************************/  

_LastInstrN(compilerState) = Opcode = wrdOpcode.u.Int;
gTP->FCompile_LastProcID = _CurProcP(compilerState)->itsObjectIndex;
gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;

/* *********************************************************************************/  
/* Convert any AMWORD arguments to constants in the _Cv Structure.                 */
/* *********************************************************************************/  

for (modIndex = 0; modIndex < 3; ++modIndex)
	{
	switch (modifier[modIndex])
		{
		case AMWORD:
			/*  Make sure we store no quoted constants outside of vmpush instructions. */
			/*  Note: This also includes the argument to a vmcallarg instruction.      */
			if ((Opcode != VMPUSH) && ((Opcode != VMCALLARG) || (modIndex != 0)))
				{
				if (opnd[modIndex].Tag == TYQUOTEDPAIR)
					opnd[modIndex].Tag = TYPAIR;
				else
				if (opnd[modIndex].Tag == TYQUOTEDSYMBOL)
					{
					if (Opcode != VMSEND)
						{
						opnd[modIndex].Tag = TYSYMBOL;
						}
					else
					if (--opnd[modIndex].QuoteCnt <= 0)
						{
						opnd[modIndex].Tag = TYSYMBOL;
						}
					}
				}

			/*  Force the conversion of any constant arguments to constants in the _Rv or _Cv Structures. */
			*ret = FCompile_CreateConstant(gCP,gTP,compilerState, opnd[modIndex]);
			ExitOnError(*ret);
			opnd[modIndex] = *ret;
			modifier[modIndex] = ret->Modifier;
			offset[modIndex] = ret->Offset;
			disp[modIndex] = ret->u.Int;
			if (modifier[modIndex] == AMREGISTER)
				{
				dcltype[modIndex] = ret->DeclaredType;
				preftype[modIndex] = ret->DeclaredType;
				}
			break;

		default:
			break;
		}

	/* Identify the constant variables by their variable name starting with "__C".    */
	/* Note: constant variables may be relocated into registers of the _Cv Structure. */

	if ((modifier[modIndex] == AMCVOFFSET) && (Cv != NIL) && (strncmp(objSymbolArray(BindArray(Cs)[offset[modIndex]].Key),"__C",3) == 0))
		{
		cons[modIndex] = TRUE;
		consValue[modIndex] = BindArray(Cs)[offset[modIndex]].Value;
		dcltype[modIndex] = n = consValue[modIndex].DeclaredType;
		dcltype[modIndex] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
		preftype[modIndex] = consValue[modIndex].DeclaredType;
		}
	else
	if ((modifier[modIndex] == AMREGISTER) && (Rv != NIL) && (strncmp(objSymbolArray(BindArray(Rs)[offset[modIndex]].Key),"__C",3) == 0))
		{
		cons[modIndex] = TRUE;
		consValue[modIndex] = BindArray(Rs)[offset[modIndex]].Value;
		dcltype[modIndex] = n = consValue[modIndex].DeclaredType;
		dcltype[modIndex] = (((n == TYNUM) || (n == TYCHAR) || (n == TYBOLE)) ? TYNUM : n);
		preftype[modIndex] = consValue[modIndex].DeclaredType;
		}
	else
		{
		cons[modIndex] = FALSE;
		consValue[modIndex] = gCP->Tval_VOID;
		}
	}

/* *************************************************************************************/  
/* Pseudo Instruction Conversion													   */
/* Convert pseudo instructions to valid virtual machine instructions.                  */
/*                        (not part of the official optimizer)                         */
/* *************************************************************************************/  

switch (Opcode)
	{
	case VMCALLARG:
		/* Is the source argument a global variable reference? */
		if ((modifier[1] == AMGVOFFSET) && (opnd[1].Tag == TYSYMBOL))
			{
			symbolPtr = SymbolArray(opnd[1]);
			switch (symbolPtr[0])
				{
				case 'a':
					if ((strcmp(symbolPtr,"abs") == 0) && 
					    (modifier[0] == AMREGISTER) && (dcltype[0] == TYREAL) && 
					    (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL)) 
						{
						Opcode = wrdOpcode.u.Int = vmregAbsNumber;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
						goto FixJumps;
						}

					goto IssueVMcall;
					break;
				
				case 'c':
					if ((strcmp(symbolPtr,"cos") == 0) && 
					    (modifier[0] == AMREGISTER) && (dcltype[0] == TYREAL) && 
					    (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL)) 
						{
						Opcode = wrdOpcode.u.Int = vmregCosNumber;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
						goto FixJumps;
						}

					goto IssueVMcall;
					break;
				
				case 'e':
					if ((strcmp(symbolPtr,"exp") == 0) && 
					    (modifier[0] == AMREGISTER) && (dcltype[0] == TYREAL) && 
					    (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL)) 
						{
						Opcode = wrdOpcode.u.Int = vmregPwrNumber;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,lnE,opnd[0],opnd[2]);
						ExitOnError(*ret);
						gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
						goto FixJumps;
						}

					goto IssueVMcall;
					break;
				
				case 'i':
					if ((strcmp(symbolPtr,"integer") == 0) && 
					    (modifier[0] == AMREGISTER) && (dcltype[0] == TYREAL) && 
					    (modifier[2] == AMREGISTER) && (dcltype[2] != TYREAL)) 
						{
						Opcode = wrdOpcode.u.Int = vmregInteger;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
						goto FixJumps;
						}

					goto IssueVMcall;
					break;
				
				case 'l':
					if ((strcmp(symbolPtr,"length") == 0) && (modifier[2] == AMREGISTER))
						{
						if ((preftype[0] == TYVECTOR) || 
							(preftype[0] == TYINTVECTOR) || 
							(preftype[0] == TYNUMVECTOR) ||
							(preftype[0] == TYBITVECTOR) ||
							(preftype[0] == TYBYTEVECTOR) ||
							(preftype[0] == TYPCODEVECTOR) ||
							(preftype[0] == TYFLTVECTOR) ||
							(preftype[0] == TYSHORTVECTOR) ||
							(preftype[0] == TYLONGVECTOR) ||
							(preftype[0] == TYOBJVECTOR) ||
							(preftype[0] == TYCPXVECTOR) ||
							(preftype[0] == TYMATRIX) ||
							(preftype[0] == TYNUMMATRIX) ||
							(preftype[0] == TYSTRUCTURE) ||
							(preftype[0] == TYDIRECTORY) ||
							(preftype[0] == TYDICTIONARY))
							{
							Opcode = wrdOpcode.u.Int = vmregObjLength;
							*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
							ExitOnError(*ret);
							gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
							goto FixJumps;
							}
						else
						if ((preftype[0] == TYSTRING) || (preftype[0] == TYSYMBOL) || (preftype[0] == TYQUOTEDSYMBOL))
							{
							Opcode = wrdOpcode.u.Int = vmregObjLength;
							*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
							ExitOnError(*ret);
							Opcode = wrdOpcode.u.Int = vmregSubImmediate;
							
							index.Tag = TYNUM;
							index.u.Int = 1;
							index.Offset = 0;
							index.Modifier = AMINTEGER;
							Opcode = wrdOpcode.u.Int = vmregSubImmediate;
							*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[2],opndAMVOID);
							ExitOnError(*ret);
							gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
							goto FixJumps;
							}
						goto IssueVMcall;
						}
					else
					if ((strcmp(symbolPtr,"log") == 0) && 
					    (modifier[0] == AMREGISTER) && (dcltype[0] == TYREAL) && 
					    (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL)) 
						{
						Opcode = wrdOpcode.u.Int = vmregLogNumber;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],lnE,opnd[2]);
						ExitOnError(*ret);
						gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
						goto FixJumps;
						}

					goto IssueVMcall;
					break;
				
				case 'n':
					if ((strcmp(symbolPtr,"number") == 0) && 
					    (modifier[0] == AMREGISTER) && (dcltype[0] != TYREAL) && 
					    (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL)) 
						{
						Opcode = wrdOpcode.u.Int = vmregNumber;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
						goto FixJumps;
						}

					goto IssueVMcall;
					break;
				
				case 's':
					if ((strcmp(symbolPtr,"sin") == 0) && 
					    (modifier[0] == AMREGISTER) && (dcltype[0] == TYREAL) && 
					    (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL)) 
						{
						Opcode = wrdOpcode.u.Int = vmregSinNumber;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
						goto FixJumps;
						}
					else
					if ((strcmp(symbolPtr,"sqrt") == 0) && 
					    (modifier[0] == AMREGISTER) && (dcltype[0] == TYREAL) && 
					    (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL)) 
						{
						Opcode = wrdOpcode.u.Int = vmregSqrtNumber;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
						goto FixJumps;
						}

					goto IssueVMcall;
					break;				
				
				case 't':
					if ((strcmp(symbolPtr,"tan") == 0) && 
					    (modifier[0] == AMREGISTER) && (dcltype[0] == TYREAL) && 
					    (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL)) 
						{
						Opcode = wrdOpcode.u.Int = vmregTanNumber;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
						goto FixJumps;
						}

					goto IssueVMcall;
					break;				
				
				default:
					goto IssueVMcall;
					break;			
				}
			}
		
		IssueVMcall:
			
		/* Push the single argument onto the stack. */
		newOpcode.u.Int = VMPUSH;
		*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],opndAMVOID,opndAMVOID);
		ExitOnError(*ret);

		/* Issue a normal vmcall instruction with an argument count of one. */
		index.Tag = TYNUM;
		index.u.Int = 1;
		index.Offset = 0;
		index.Modifier = AMINTEGER;
		Opcode = wrdOpcode.u.Int = VMCALL;
		*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[1],opnd[2]);
		ExitOnError(*ret);

		gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
		goto FixJumps;
		break;
	}

/* *************************************************************************************/  
/* Memory Instruction Optimization													   */
/* Convert memory instructions to faster strongly typed instructions (where possible). */
/* *************************************************************************************/  

if (gTP->FCompile_OptimizeSW == FALSE) goto IssueOriginalInstructionAsIs;

switch (Opcode)
	{
	case VMADD:
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (dcltype[2] == TYNUM)) Opcode = wrdOpcode.u.Int = VMIADD;
		else
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (dcltype[2] == TYTVAL)) Opcode = wrdOpcode.u.Int = VMIADD;
		else
		if (dcltype[2] == TYNUM) Opcode = wrdOpcode.u.Int = VMADDI;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYREAL)) Opcode = wrdOpcode.u.Int = VMNADD;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYTVAL)) Opcode = wrdOpcode.u.Int = VMNADD;
		else
		if (dcltype[2] == TYREAL) Opcode = wrdOpcode.u.Int = VMADDN;
		break;

	case VMDIV:
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (dcltype[2] == TYNUM)) Opcode = wrdOpcode.u.Int = VMIDIV;
		else
		if (dcltype[2] == TYNUM) Opcode = wrdOpcode.u.Int = VMDIVI;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYREAL)) Opcode = wrdOpcode.u.Int = VMNDIV;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYTVAL)) Opcode = wrdOpcode.u.Int = VMNDIV;
		else
		if (dcltype[2] == TYREAL) Opcode = wrdOpcode.u.Int = VMDIVN;
		break;

	case VMDIVR:
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (dcltype[2] == TYNUM)) Opcode = wrdOpcode.u.Int = VMIDIVR;
		else
		if (dcltype[2] == TYNUM) Opcode = wrdOpcode.u.Int = VMDIVRI;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYREAL)) Opcode = wrdOpcode.u.Int = VMNDIVR;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYTVAL)) Opcode = wrdOpcode.u.Int = VMNDIVR;
		break;

	case VMJMPLE:
		if ((dcltype[0] == TYNUM) && (preftype[0] != TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] != TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpLEInteger;
		else
		if ((dcltype[0] == TYNUM) && (preftype[0] == TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] == TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpLEInteger;
		else
		if ((dcltype[0] == TYREAL) && (preftype[0] == TYREAL) && (dcltype[1] == TYREAL) && (preftype[1] == TYREAL)) Opcode = wrdOpcode.u.Int = vmnatJmpLENumber;
		break;

	case VMJMPLT:
		if ((dcltype[0] == TYNUM) && (preftype[0] != TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] != TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpLTInteger;
		else
		if ((dcltype[0] == TYNUM) && (preftype[0] == TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] == TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpLTInteger;
		else
		if ((dcltype[0] == TYREAL) && (preftype[0] == TYREAL) && (dcltype[1] == TYREAL) && (preftype[1] == TYREAL)) Opcode = wrdOpcode.u.Int = vmnatJmpLTNumber;
		break;

	case VMJMPEQ:
		if ((dcltype[0] == TYNUM) && (preftype[0] != TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] != TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpEQInteger;
		else
		if ((dcltype[0] == TYNUM) && (preftype[0] == TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] == TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpEQInteger;
		else
		if ((dcltype[0] == TYREAL) && (preftype[0] == TYREAL) && (dcltype[1] == TYREAL) && (preftype[1] == TYREAL)) Opcode = wrdOpcode.u.Int = vmnatJmpEQNumber;
		break;

	case VMJMPNE:
		if ((dcltype[0] == TYNUM) && (preftype[0] != TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] != TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpNEInteger;
		else
		if ((dcltype[0] == TYNUM) && (preftype[0] == TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] == TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpNEInteger;
		else
		if ((dcltype[0] == TYREAL) && (preftype[0] == TYREAL) && (dcltype[1] == TYREAL) && (preftype[1] == TYREAL)) Opcode = wrdOpcode.u.Int = vmnatJmpNENumber;
		break;

	case VMJMPGT:
		if ((dcltype[0] == TYNUM) && (preftype[0] != TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] != TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpGTInteger;
		else
		if ((dcltype[0] == TYNUM) && (preftype[0] == TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] == TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpGTInteger;
		else
		if ((dcltype[0] == TYREAL) && (preftype[0] == TYREAL) && (dcltype[1] == TYREAL) && (preftype[1] == TYREAL)) Opcode = wrdOpcode.u.Int = vmnatJmpGTNumber;
		break;

	case VMJMPGE:
		if ((dcltype[0] == TYNUM) && (preftype[0] != TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] != TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpGEInteger;
		else
		if ((dcltype[0] == TYNUM) && (preftype[0] == TYBOLE) && (dcltype[1] == TYNUM) && (preftype[1] == TYBOLE)) Opcode = wrdOpcode.u.Int = vmnatJmpGEInteger;
		else
		if ((dcltype[0] == TYREAL) && (preftype[0] == TYREAL) && (dcltype[1] == TYREAL) && (preftype[1] == TYREAL)) Opcode = wrdOpcode.u.Int = vmnatJmpGENumber;
		break;

	case VMMOVE:
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM)) Opcode = wrdOpcode.u.Int = vmnatLoadInteger;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYNUM)) Opcode = wrdOpcode.u.Int = VMMOVEI;
		else
		if ((dcltype[0] == TYVOID) && (dcltype[1] == TYNUM)) Opcode = wrdOpcode.u.Int = VMMOVEI;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYREAL)) Opcode = wrdOpcode.u.Int = vmnatLoadNumber;
		else
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYREAL)) Opcode = wrdOpcode.u.Int = VMMOVEN;
		else
		if ((dcltype[0] == TYVOID) && (dcltype[1] == TYREAL)) Opcode = wrdOpcode.u.Int = VMMOVEN;
		break;

	case VMMUL:
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (dcltype[2] == TYNUM)) Opcode = wrdOpcode.u.Int = VMIMUL;
		else
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (dcltype[2] == TYTVAL)) Opcode = wrdOpcode.u.Int = VMIMUL;
		else
		if (dcltype[2] == TYNUM) Opcode = wrdOpcode.u.Int = VMMULI;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYREAL)) Opcode = wrdOpcode.u.Int = VMNMUL;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYTVAL)) Opcode = wrdOpcode.u.Int = VMNMUL;
		else
		if (dcltype[2] == TYNUM) Opcode = wrdOpcode.u.Int = VMMULN;
		break;

	case VMREF:
		if ((dcltype[0] == TYNUM) && (preftype[1] == TYBITVECTOR) && (dcltype[2] == TYNUM)) Opcode = wrdOpcode.u.Int = VMREFBITVECTOR;
		else
		if ((dcltype[0] == TYNUM) && (preftype[1] == TYINTVECTOR) && (dcltype[2] == TYNUM)) Opcode = wrdOpcode.u.Int = VMREFINTVECTOR;
		else
		if ((dcltype[0] == TYNUM) && (preftype[1] == TYBYTEVECTOR) && (dcltype[2] == TYNUM)) Opcode = wrdOpcode.u.Int = VMREFBYTVECTOR;
		else
		if ((dcltype[0] == TYNUM) && (preftype[1] == TYSTRING) && (dcltype[2] == TYNUM)) Opcode = wrdOpcode.u.Int = VMREFSTRING;
		else
		if ((dcltype[0] == TYNUM) && (preftype[1] == TYSYMBOL) && (dcltype[2] == TYNUM)) Opcode = wrdOpcode.u.Int = VMREFSYMBOL;
		else
		if ((dcltype[0] == TYNUM) && (preftype[1] == TYTEXT) && (dcltype[2] == TYNUM)) Opcode = wrdOpcode.u.Int = VMREFTEXT;
		else
		if ((dcltype[0] == TYNUM) && (preftype[1] == TYNUMMATRIX) && (dcltype[2] == TYREAL)) Opcode = wrdOpcode.u.Int = VMREFNUMMATRIX;
		else
		if ((dcltype[0] == TYNUM) && (preftype[1] == TYNUMVECTOR) && (dcltype[2] == TYREAL)) Opcode = wrdOpcode.u.Int = VMREFNUMVECTOR;
		else
		if ((dcltype[0] == TYNUM) && (preftype[1] == TYFLTVECTOR) && (dcltype[2] == TYREAL)) Opcode = wrdOpcode.u.Int = VMREFFLTVECTOR;
		else
		if ((dcltype[0] == TYNUM) && (preftype[1] == TYVECTOR)) Opcode = wrdOpcode.u.Int = VMREFVECTOR;
		break;

	case VMSET:
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (preftype[2] == TYBITVECTOR)) Opcode = wrdOpcode.u.Int = VMSETBITVECTOR;
		else
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (preftype[2] == TYINTVECTOR)) Opcode = wrdOpcode.u.Int = VMSETINTVECTOR;
		else
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (preftype[2] == TYBYTEVECTOR)) Opcode = wrdOpcode.u.Int = VMSETBYTVECTOR;
		else
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (preftype[2] == TYSTRING)) Opcode = wrdOpcode.u.Int = VMSETSTRING;
		else
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYREAL) && (preftype[2] == TYNUMMATRIX)) Opcode = wrdOpcode.u.Int = VMSETNUMMATRIX;
		else
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYREAL) && (preftype[2] == TYNUMVECTOR)) Opcode = wrdOpcode.u.Int = VMSETNUMVECTOR;
		else
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYREAL) && (preftype[2] == TYFLTVECTOR)) Opcode = wrdOpcode.u.Int = VMSETFLTVECTOR;
		else
		if ((dcltype[0] == TYNUM) && (preftype[2] == TYVECTOR)) Opcode = wrdOpcode.u.Int = VMSETVECTOR;
		break;

	case VMSUB:
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (dcltype[2] == TYNUM)) Opcode = wrdOpcode.u.Int = VMISUB;
		else
		if ((dcltype[0] == TYNUM) && (dcltype[1] == TYNUM) && (dcltype[2] == TYTVAL)) Opcode = wrdOpcode.u.Int = VMISUB;
		else
		if (dcltype[2] == TYNUM) Opcode = wrdOpcode.u.Int = VMSUBI;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYREAL)) Opcode = wrdOpcode.u.Int = VMNSUB;
		else
		if ((dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYTVAL)) Opcode = wrdOpcode.u.Int = VMNSUB;
		else
		if (dcltype[2] == TYNUM) Opcode = wrdOpcode.u.Int = VMSUBN;
		break;
	}

/* *********************************************************************************/  
/* Register Instruction Optimization											   */
/* Convert instructions, with one or more register operands, to faster register    */
/* or memory instructions (where possible). This may require inserting register    */
/* instructions to convert three operand generic or native instructions into a     */
/* sequence of two operand register instructions.                                  */
/*                                   											   */
/* Note: This section handles only those cases where all operands are registers.   */
/* *********************************************************************************/  

if ((Rv != NIL) && ((modifier[0] == AMREGISTER) || (modifier[1] == AMREGISTER) || (modifier[2] == AMREGISTER)))
	{
	switch (Opcode)
		{
		/*	(vmadd argument source target) : target = source + argument; */
		case VMADD:
		case VMCADD:
		case VMIADD:
		case VMNADD:
		case vmnatAddInteger:
		case vmnatAddNumber:
			/* ======================================================================== */
			/* (vmadd regs(Integer:argument) regs(Integer:source) regs(Integer:target)) */
			/* ======================================================================== */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER) &&
			    (dcltype[0] != TYREAL) && (dcltype[1] != TYREAL) && (dcltype[2] != TYREAL))
				{
				/* We compute the pointer bitshift amount for these arguments.      */					
				/* Note: We must follow with either Integer or pointer subtraction. */					
				targetTyp = preftype[2];
				if (targetTyp == TYJUMPPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYFLOATPOINTER) bitShift = BITSIZEOFFLOAT;
				else if (targetTyp == TYINTPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYREALPOINTER) bitShift = BITSIZEOFREAL;
				else if (targetTyp == TYSHORTPOINTER) bitShift = BITSIZEOFSHORT;
				else if (targetTyp == TYLONGPOINTER) bitShift = BITSIZEOFNUM32;
				else if (targetTyp == TYWORDPOINTER) bitShift = BITSIZEOFTVAL;
				else bitShift = 0;
				if ((offset[0] != offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					/* Add argument to target */
					if (bitShift != 0)
						{
						/* Use pointer arithmetic and specify the bitshift amount. */
						index.Tag = TYNUM;
						index.u.Int = bitShift;
						index.Offset = AMINTEGER;
						index.Modifier = AMINTEGER;
						Opcode = wrdOpcode.u.Int = vmregAddPointer;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[0],opnd[2]);
						ExitOnError(*ret);
						}
					else
						{
						/* Use normal integer arithmetic with no bitshift amount. */
						Opcode = wrdOpcode.u.Int = vmregAddInteger;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						}
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] == offset[2]))
					{
					/* Add argument to target */
					if (bitShift != 0)
						{
						/* Use pointer arithmetic and specify the bitshift amount. */
						index.Tag = TYNUM;
						index.u.Int = bitShift;
						index.Offset = AMINTEGER;
						index.Modifier = AMINTEGER;
						Opcode = wrdOpcode.u.Int = vmregAddPointer;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[0],opnd[2]);
						ExitOnError(*ret);
						}
					else
						{
						/* Use normal integer arithmetic with no bitshift amount. */
						Opcode = wrdOpcode.u.Int = vmregAddInteger;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						}
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] != offset[2]) && (offset[1] == offset[2]))
					{
					/* Add argument to target */
					if (bitShift != 0)
						{
						/* Use pointer arithmetic and specify the bitshift amount. */
						index.Tag = TYNUM;
						index.u.Int = bitShift;
						index.Offset = AMINTEGER;
						index.Modifier = AMINTEGER;
						Opcode = wrdOpcode.u.Int = vmregAddPointer;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[0],opnd[2]);
						ExitOnError(*ret);
						}
					else
						{
						/* Use normal integer arithmetic with no bitshift amount. */
						Opcode = wrdOpcode.u.Int = vmregAddInteger;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						}
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to temporary */
					*tmp = TSYMBOL("__Ri");
					temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					temporary.Tag = TYNUM;
					temporary.u.Int = temporary.Offset;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Add argument to temporary */
					if (bitShift != 0)
						{
						/* Use pointer arithmetic and specify the bitshift amount. */
						prmv[0].Tag = TYNUM;
						index.u.Int = bitShift;
						index.Offset = AMINTEGER;
						index.Modifier = AMINTEGER;
						Opcode = wrdOpcode.u.Int = vmregAddPointer;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[0],temporary);
						ExitOnError(*ret);
						}
					else
						{
						/* Use normal integer arithmetic with no bitshift amount. */
						Opcode = wrdOpcode.u.Int = vmregAddInteger;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
						ExitOnError(*ret);
						}
					/* Move temporary to target */
					Opcode = wrdOpcode.u.Int = vmregMoveInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
 					}
				}
			else
			/* ===================================================================== */
			/* (vmadd regs(Number:argument) regs(Number:source) regs(Number:target)) */
			/* ===================================================================== */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER) &&
			    (dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYREAL))
				{
				if ((offset[0] != offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveNumber),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					/* Add argument to target. */
					Opcode = wrdOpcode.u.Int = vmregAddNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] == offset[2]))
					{
					/* Add argument to target. */
					Opcode = wrdOpcode.u.Int = vmregAddNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] != offset[2]) && (offset[1] == offset[2]))
					{
					/* Add argument to target. */
					Opcode = wrdOpcode.u.Int = vmregAddNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to temporary */
					*tmp = TSYMBOL("__RNx");
					temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					temporary.Tag = TYNUM;
					temporary.u.Int = temporary.Offset;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveNumber),opnd[1],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Add argument to temporary. */
					Opcode = wrdOpcode.u.Int = vmregAddNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Move temporary to target */
					Opcode = wrdOpcode.u.Int = vmregMoveNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
 					}
				}
			else
			/* ================================================================ */
			/* (vmadd cons(Integer) regs(Integer:source) regs(Integer:target))  */
			/* ================================================================ */
			if ((cons[0] == TRUE) && ((dcltype[0] == TYNUM) || (dcltype[0] == TYCHAR)) && 
			    (modifier[1] == AMREGISTER) && (dcltype[1] != TYREAL) &&
			    (modifier[2] == AMREGISTER) && (dcltype[2] != TYREAL))
				{
				/* Manage the case for Integer target */
				
				/* We compute the pointer bitshift amount for these arguments.      */					
				/* Note: We must follow with either Integer or pointer subtraction. */					
				targetTyp = preftype[2];
				if (targetTyp == TYJUMPPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYFLOATPOINTER) bitShift = BITSIZEOFFLOAT;
				else if (targetTyp == TYINTPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYREALPOINTER) bitShift = BITSIZEOFREAL;
				else if (targetTyp == TYSHORTPOINTER) bitShift = BITSIZEOFSHORT;
				else if (targetTyp == TYLONGPOINTER) bitShift = BITSIZEOFNUM32;
				else if (targetTyp == TYWORDPOINTER) bitShift = BITSIZEOFTVAL;
				else bitShift = 0;
				if (offset[1] != offset[2])
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				/* Add argument to target */
				if (bitShift != 0)
					{
					/* Use normal integer arithmetic with no bitshift amount. */
					index.Tag = TYNUM;
					index.u.Int = consValue[0].u.Int << bitShift;
					index.Offset = AMINTEGER;
					index.Modifier = AMINTEGER;
					Opcode = wrdOpcode.u.Int = vmregAddImmediate;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				else
					{
					/* Use normal integer arithmetic with no bitshift amount. */
					index.Tag = TYNUM;
					index.u.Int = consValue[0].u.Int;
					index.Offset = AMINTEGER;
					index.Modifier = AMINTEGER;
					Opcode = wrdOpcode.u.Int = vmregAddImmediate;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* ========================================================================= */
			/* (vmadd vars(Integer:argument) regs(Integer:source) regs(Integer:target))  */
			/* ========================================================================= */
			if ((modifier[0] != AMREGISTER) && (modifier[0] <= MAXRGOFFST) && ((dcltype[0] == TYNUM) || (dcltype[0] == TYCHAR)) && 
			    (modifier[1] == AMREGISTER) && (dcltype[1] != TYREAL) &&
			    (modifier[2] == AMREGISTER) && (dcltype[2] != TYREAL))
				{
				/* Manage the case for Integer target */
				
				/* We compute the pointer bitshift amount for these arguments.      */					
				/* Note: We must follow with either Integer or pointer subtraction. */					
				targetTyp = preftype[2];
				if (targetTyp == TYJUMPPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYFLOATPOINTER) bitShift = BITSIZEOFFLOAT;
				else if (targetTyp == TYINTPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYREALPOINTER) bitShift = BITSIZEOFREAL;
				else if (targetTyp == TYSHORTPOINTER) bitShift = BITSIZEOFSHORT;
				else if (targetTyp == TYLONGPOINTER) bitShift = BITSIZEOFNUM32;
				else if (targetTyp == TYWORDPOINTER) bitShift = BITSIZEOFTVAL;
				else bitShift = 0;
				/* Move argument to temporary */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregLoadInteger),opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);
				if (offset[1] != offset[2])
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				/* Add temporary to target */
				if (bitShift != 0)
					{
					/* Use pointer arithmetic and specify the bitshift amount. */
					index.Tag = TYNUM;
					index.u.Int = bitShift;
					index.Offset = AMINTEGER;
					index.Modifier = AMINTEGER;
					Opcode = wrdOpcode.u.Int = vmregAddPointer;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,temporary,opnd[2]);
					ExitOnError(*ret);
					}
				else
					{
					/* Use normal integer arithmetic with no bitshift amount. */
					Opcode = wrdOpcode.u.Int = vmregAddInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;
    
		/*	(vmdiv argument source target) : target = source / argument; */
		case VMDIV:
		case VMCDIV:
		case VMIDIV:
		case VMNDIV:
		case vmnatDivInteger:
		case vmnatDivNumber:
			/* ======================================================================== */
			/* (vmdiv regs(Integer:argument) regs(Integer:source) regs(Integer:target)) */
			/* ======================================================================== */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER) &&
			    (dcltype[0] != TYREAL) && (dcltype[1] != TYREAL) && (dcltype[2] != TYREAL))
				{
				if ((offset[0] != offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] == offset[2]))
					{
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] != offset[2]) && (offset[1] == offset[2]))
					{
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to temporary */
					*tmp = TSYMBOL("__Ri");
					temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					temporary.Tag = TYNUM;
					temporary.u.Int = temporary.Offset;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Move temporary to target */
					Opcode = wrdOpcode.u.Int = vmregMoveInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
 					}
				}
			else
			/* ===================================================================== */
			/* (vmdiv regs(Number:argument) regs(Number:source) regs(Number:target)) */
			/* ===================================================================== */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER) &&
			    (dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYREAL))
				{
				if ((offset[0] != offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveNumber),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] == offset[2]))
					{
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] != offset[2]) && (offset[1] == offset[2]))
					{
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to temporary */
					*tmp = TSYMBOL("__RNx");
					temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					temporary.Tag = TYNUM;
					temporary.u.Int = temporary.Offset;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveNumber),opnd[1],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Move temporary to target */
					Opcode = wrdOpcode.u.Int = vmregMoveNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
 					}
				}
			else
			/* ================================================================ */
			/* (vmdiv cons(Integer) regs(Integer:source) regs(Integer:target))  */
			/* ================================================================ */
			if ((cons[0] == TRUE) && ((dcltype[0] == TYNUM) || (dcltype[0] == TYCHAR)) && 
			    (modifier[1] == AMREGISTER) && (dcltype[1] != TYREAL) &&
			    (modifier[2] == AMREGISTER) && (dcltype[2] != TYREAL))
				{
				if (offset[1] != offset[2])
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				/* Divide argument into target */
				index.Tag = TYNUM;
				index.u.Int = consValue[0].u.Int;
				index.Offset = AMINTEGER;
				index.Modifier = AMINTEGER;
				Opcode = wrdOpcode.u.Int = vmregDivImmediate;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* ========================================================================= */
			/* (vmdiv vars(Integer:argument) regs(Integer:source) regs(Integer:target))  */
			/* ========================================================================= */
			if ((modifier[0] != AMREGISTER) && (modifier[0] <= MAXRGOFFST) && ((dcltype[0] == TYNUM) || (dcltype[0] == TYCHAR)) && 
			    (modifier[1] == AMREGISTER) && (dcltype[1] != TYREAL) &&
			    (modifier[2] == AMREGISTER) && (dcltype[2] != TYREAL))
				{
				/* Move argument to temporary */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregLoadInteger),opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);
				if (offset[1] != offset[2])
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				/* Divide argument into target */
				Opcode = wrdOpcode.u.Int = vmregDivInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmdiv argument source target) : target = source / argument; */
		case VMDIVR:
		case VMIDIVR:
		case VMNDIVR:
		case vmnatDivrInteger:
		case vmnatDivrNumber:
			/* ========================================================================= */
			/* (vmdivr regs(Integer:argument) regs(Integer:source) regs(Integer:target)) */
			/* ========================================================================= */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER) &&
			    (dcltype[0] != TYREAL) && (dcltype[1] != TYREAL) && (dcltype[2] != TYREAL))
				{
				if ((offset[0] != offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivrInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] == offset[2]))
					{
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivrInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] != offset[2]) && (offset[1] == offset[2]))
					{
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivrInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to temporary */
					*tmp = TSYMBOL("__Ri");
					temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					temporary.Tag = TYNUM;
					temporary.u.Int = temporary.Offset;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivrInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Move temporary to target */
					Opcode = wrdOpcode.u.Int = vmregMoveInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
 					}
				}
			else
			/* ====================================================================== */
			/* (vmdivr regs(Number:argument) regs(Number:source) regs(Number:target)) */
			/* ====================================================================== */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER) &&
			    (dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYREAL))
				{
				if ((offset[0] != offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveNumber),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivrNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] == offset[2]))
					{
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivrNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] != offset[2]) && (offset[1] == offset[2]))
					{
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivrNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to temporary */
					*tmp = TSYMBOL("__RNx");
					temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					temporary.Tag = TYNUM;
					temporary.u.Int = temporary.Offset;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveNumber),opnd[1],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregDivrNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Move temporary to target */
					Opcode = wrdOpcode.u.Int = vmregMoveNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
 					}
				}
			else
			/* ================================================================= */
			/* (vmdivr cons(Integer) regs(Integer:source) regs(Integer:target))  */
			/* ================================================================= */
			if ((cons[0] == TRUE) && ((dcltype[0] == TYNUM) || (dcltype[0] == TYCHAR)) && 
			    (modifier[1] == AMREGISTER) && (dcltype[1] != TYREAL) &&
			    (modifier[2] == AMREGISTER) && (dcltype[2] != TYREAL))
				{
				if (offset[1] != offset[2])
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				/* Divide argument into target */
				index.Tag = TYNUM;
				index.u.Int = consValue[0].u.Int;
				index.Offset = AMINTEGER;
				index.Modifier = AMINTEGER;
				Opcode = wrdOpcode.u.Int = vmregDivrImmediate;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* ========================================================================== */
			/* (vmdivr vars(Integer:argument) regs(Integer:source) regs(Integer:target))  */
			/* ========================================================================== */
			if ((modifier[0] != AMREGISTER) && (modifier[0] <= MAXRGOFFST) && ((dcltype[0] == TYNUM) || (dcltype[0] == TYCHAR)) && 
			    (modifier[1] == AMREGISTER) && (dcltype[1] != TYREAL) &&
			    (modifier[2] == AMREGISTER) && (dcltype[2] != TYREAL))
				{
				/* Move argument to temporary */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregLoadInteger),opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);
				if (offset[1] != offset[2])
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				/* Divide argument into target */
				Opcode = wrdOpcode.u.Int = vmregDivrInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmregJmpCCImmediate source comparitor label) */
		case vmregJmpLTImmediate:
		case vmregJmpLEImmediate:
		case vmregJmpEQImmediate:
		case vmregJmpNEImmediate:
		case vmregJmpGEImmediate:
		case vmregJmpGTImmediate:
			/*	(vmregJmpCCImmediate regs(a) __C"int" label) */
			if ((modifier[0] == AMREGISTER) && 
			    (cons[1] == TRUE) && 
				((consValue[1].Tag == TYNUM) || (consValue[1].Tag == TYCHAR) || (consValue[1].Tag == TYBOLE)) && 
				(modifier[2] == AMINTEGER))
				{
				regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
				if (regtype[0] == TRUE) FrameExit(TERROR("!compile(vmregJmpCCImmediate): incompatible register types!"));
				prmv[1].u.Int = consValue[1].u.Int;
				prmv[1].Tag = TYNUM;
				prmv[1].Offset = AMINTEGER;
				prmv[1].Modifier = AMINTEGER;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],prmv[1],opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

	/*	(vmjmplt source comparitor label) : if (source < comparitor) Ip = label; */
	case vmnatJmpLTInteger:
	case vmnatJmpLTNumber:
	case VMJMPLT:
		/*	(vmjmplt regs(a) regs(b) label) */
		if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMINTEGER))
			{
			regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
			regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
			if (regtype[0] != regtype[1]) {Opcode = wrdOpcode.u.Int = VMJMPLT; goto AssignmentCastingChecks;}
			if (regtype[1] == TRUE)
				Opcode = wrdOpcode.u.Int = vmregJmpLTNumber;
			else
				Opcode = wrdOpcode.u.Int = vmregJmpLTInteger;
			}
		else
		/*	(vmjmplt regs(a) __C"int" label) */
		if ((modifier[0] == AMREGISTER) && 
			(cons[1] == TRUE) && 
			((consValue[1].Tag == TYNUM) || (consValue[1].Tag == TYCHAR) || (consValue[1].Tag == TYBOLE)) && 
			(modifier[2] == AMINTEGER))
			{
			regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
			if (regtype[0] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPLT; goto AssignmentCastingChecks;}
			Opcode = wrdOpcode.u.Int = vmregJmpLTImmediate;
			prmv[1].u.Int = consValue[1].u.Int;
			prmv[1].Tag = TYNUM;
			prmv[1].Offset = AMINTEGER;
			prmv[1].Modifier = AMINTEGER;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],prmv[1],opnd[2]);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/*	(vmjmplt __C"int" regs(b) label) */
		if ((cons[0] == TRUE) && 
			((consValue[0].Tag == TYNUM) || (consValue[0].Tag == TYCHAR) || (consValue[0].Tag == TYBOLE)) &&
			(modifier[1] == AMREGISTER) && 
			(modifier[2] == AMINTEGER))
			{
			regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
			if (regtype[1] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPLT; goto AssignmentCastingChecks;}
			Opcode = wrdOpcode.u.Int = vmregJmpGTImmediate;
			prmv[1].u.Int = consValue[0].u.Int;
			prmv[1].Tag = TYNUM;
			prmv[1].Offset = AMINTEGER;
			prmv[1].Modifier = AMINTEGER;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],prmv[1],opnd[2]);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	/*	(vmjmple source comparitor label) : if (source <= comparitor) Ip = label; */
	case vmnatJmpLEInteger:
	case vmnatJmpLENumber:
	case VMJMPLE:
		/*	(vmjmple regs(a) regs(b)) */
		if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMINTEGER))
			{
			regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
			regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
			if (regtype[0] != regtype[1]) {Opcode = wrdOpcode.u.Int = VMJMPLE; goto AssignmentCastingChecks;}
			if (regtype[1] == TRUE)
				Opcode = wrdOpcode.u.Int = vmregJmpLENumber;
			else
				Opcode = wrdOpcode.u.Int = vmregJmpLEInteger;
			}
		else
		/*	(vmjmple regs(a) __C"int" label) */
		if ((modifier[0] == AMREGISTER) && 
			(cons[1] == TRUE) && 
			((consValue[1].Tag == TYNUM) || (consValue[1].Tag == TYCHAR) || (consValue[1].Tag == TYBOLE)) && 
			(modifier[2] == AMINTEGER))
			{
			regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
			if (regtype[0] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPLE; goto AssignmentCastingChecks;}
			Opcode = wrdOpcode.u.Int = vmregJmpLEImmediate;
			prmv[1].u.Int = consValue[1].u.Int;
			prmv[1].Tag = TYNUM;
			prmv[1].Offset = AMINTEGER;
			prmv[1].Modifier = AMINTEGER;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],prmv[1],opnd[2]);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/*	(vmjmple __C"int" regs(b) label) */
		if ((cons[0] == TRUE) && 
			((consValue[0].Tag == TYNUM) || (consValue[0].Tag == TYCHAR) || (consValue[0].Tag == TYBOLE)) &&
			(modifier[1] == AMREGISTER) && 
			(modifier[2] == AMINTEGER))
			{
			regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
			if (regtype[1] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPLE; goto AssignmentCastingChecks;}
			Opcode = wrdOpcode.u.Int = vmregJmpGEImmediate;
			prmv[1].u.Int = consValue[0].u.Int;
			prmv[1].Tag = TYNUM;
			prmv[1].Offset = AMINTEGER;
			prmv[1].Modifier = AMINTEGER;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],prmv[1],opnd[2]);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	/*	(vmjmpne source comparitor label) : if (source != comparitor) Ip = label; */
	case vmnatJmpNEInteger:
	case vmnatJmpNENumber:
	case VMJMPNE:
		/*	(vmjmpne regs(a) regs(b)) */
		if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMINTEGER))
			{
			regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
			regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
			if (regtype[0] != regtype[1]) {Opcode = wrdOpcode.u.Int = VMJMPNE; goto AssignmentCastingChecks;}
			if (regtype[1] == TRUE)
				Opcode = wrdOpcode.u.Int = vmregJmpNENumber;
			else
				Opcode = wrdOpcode.u.Int = vmregJmpNEInteger;
			}
		else
		/*	(vmjmpne regs(a) __C"int" label) */
		if ((modifier[0] == AMREGISTER) && 
			(cons[1] == TRUE) && 
			((consValue[1].Tag == TYNUM) || (consValue[1].Tag == TYCHAR) || (consValue[1].Tag == TYBOLE)) && 
			(modifier[2] == AMINTEGER))
			{
			regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
			if (regtype[0] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPNE; goto AssignmentCastingChecks;}
			Opcode = wrdOpcode.u.Int = vmregJmpNEImmediate;
			prmv[1].u.Int = consValue[1].u.Int;
			prmv[1].Tag = TYNUM;
			prmv[1].Offset = AMINTEGER;
			prmv[1].Modifier = AMINTEGER;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],prmv[1],opnd[2]);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/*	(vmjmpne __C"int" regs(b) label) */
		if ((cons[0] == TRUE) && 
			((consValue[0].Tag == TYNUM) || (consValue[0].Tag == TYCHAR) || (consValue[0].Tag == TYBOLE)) &&
			(modifier[1] == AMREGISTER) && 
			(modifier[2] == AMINTEGER))
			{
			regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
			if (regtype[1] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPNE; goto AssignmentCastingChecks;}
			Opcode = wrdOpcode.u.Int = vmregJmpNEImmediate;
			prmv[1].u.Int = consValue[0].u.Int;
			prmv[1].Tag = TYNUM;
			prmv[1].Offset = AMINTEGER;
			prmv[1].Modifier = AMINTEGER;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],prmv[1],opnd[2]);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	/*	(vmjmpeq source comparitor label) : if (source == comparitor) Ip = label; */
	case vmnatJmpEQInteger:
	case vmnatJmpEQNumber:
	case VMJMPEQ:
		/*	(vmjmpeq regs(a) regs(b)) */
		if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMINTEGER))
			{
			regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
			regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
			if (regtype[0] != regtype[1]) {Opcode = wrdOpcode.u.Int = VMJMPEQ; goto AssignmentCastingChecks;}
			if (regtype[1] == TRUE)
				Opcode = wrdOpcode.u.Int = vmregJmpEQNumber;
			else
				Opcode = wrdOpcode.u.Int = vmregJmpEQInteger;
			}
		else
		/*	(vmjmpeq regs(a) __C"int" label) */
		if ((modifier[0] == AMREGISTER) && 
			(cons[1] == TRUE) && 
			((consValue[1].Tag == TYNUM) || (consValue[1].Tag == TYCHAR) || (consValue[1].Tag == TYBOLE)) && 
			(modifier[2] == AMINTEGER))
			{
			regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
			if (regtype[0] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPEQ; goto AssignmentCastingChecks;}
			Opcode = wrdOpcode.u.Int = vmregJmpEQImmediate;
			prmv[1].u.Int = consValue[1].u.Int;
			prmv[1].Tag = TYNUM;
			prmv[1].Offset = AMINTEGER;
			prmv[1].Modifier = AMINTEGER;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],prmv[1],opnd[2]);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		/*	(vmjmpeq __C"int" regs(b) label) */
		if ((cons[0] == TRUE) && 
			((consValue[0].Tag == TYNUM) || (consValue[0].Tag == TYCHAR) || (consValue[0].Tag == TYBOLE)) &&
			(modifier[1] == AMREGISTER) && 
			(modifier[2] == AMINTEGER))
			{
			regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
			if (regtype[1] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPEQ; goto AssignmentCastingChecks;}
			Opcode = wrdOpcode.u.Int = vmregJmpEQImmediate;
			prmv[1].u.Int = consValue[0].u.Int;
			prmv[1].Tag = TYNUM;
			prmv[1].Offset = AMINTEGER;
			prmv[1].Modifier = AMINTEGER;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],prmv[1],opnd[2]);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	/*	(vmjmpge source comparitor label) : if (source >= comparitor) Ip = label; */
	case vmnatJmpGEInteger:
	case vmnatJmpGENumber:
	case VMJMPGE:
		/*	(vmjmpge regs(a) regs(b)) */
		if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMINTEGER))
			{
			regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
			regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
			if (regtype[0] != regtype[1]) {Opcode = wrdOpcode.u.Int = VMJMPGE; goto AssignmentCastingChecks;}
			if (regtype[1] == TRUE)
				Opcode = wrdOpcode.u.Int = vmregJmpGENumber;
			else
				Opcode = wrdOpcode.u.Int = vmregJmpGEInteger;
			}
		else
		/*	(vmjmpge regs(a) __C"int" label) */
		if ((modifier[0] == AMREGISTER) && 
			(cons[1] == TRUE) && 
			((consValue[1].Tag == TYNUM) || (consValue[1].Tag == TYCHAR) || (consValue[1].Tag == TYBOLE)) && 
			(modifier[2] == AMINTEGER))
			{
			regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
			if (regtype[0] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPGE; goto AssignmentCastingChecks;}
			Opcode = wrdOpcode.u.Int = vmregJmpGEImmediate;
			prmv[1].u.Int = consValue[1].u.Int;
			prmv[1].Tag = TYNUM;
			prmv[1].Offset = AMINTEGER;
			prmv[1].Modifier = AMINTEGER;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],prmv[1],opnd[2]);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/*	(vmjmpge __C"int" regs(b) label) */
		if ((cons[0] == TRUE) && 
			((consValue[0].Tag == TYNUM) || (consValue[0].Tag == TYCHAR) || (consValue[0].Tag == TYBOLE)) &&
			(modifier[1] == AMREGISTER) && 
			(modifier[2] == AMINTEGER))
			{
			regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
			if (regtype[1] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPGE; goto AssignmentCastingChecks;}
			Opcode = wrdOpcode.u.Int = vmregJmpLEImmediate;
			prmv[1].u.Int = consValue[0].u.Int;
			prmv[1].Tag = TYNUM;
			prmv[1].Offset = AMINTEGER;
			prmv[1].Modifier = AMINTEGER;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],prmv[1],opnd[2]);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

		/*	(vmjmpgt source comparitor label) : if (source > comparitor) Ip = label; */
		case vmnatJmpGTInteger:
		case vmnatJmpGTNumber:
		case VMJMPGT:
			/*	(vmjmpgt regs(a) regs(b)) */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMINTEGER))
				{
				regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
				regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
				if (regtype[0] != regtype[1]) {Opcode = wrdOpcode.u.Int = VMJMPGT; goto AssignmentCastingChecks;}
				if (regtype[1] == TRUE)
					Opcode = wrdOpcode.u.Int = vmregJmpGTNumber;
				else
					Opcode = wrdOpcode.u.Int = vmregJmpGTInteger;
				}
			else
			/*	(vmjmpgt regs(a) __C"int" label) */
			if ((modifier[0] == AMREGISTER) && 
			    (cons[1] == TRUE) && 
				((consValue[1].Tag == TYNUM) || (consValue[1].Tag == TYCHAR) || (consValue[1].Tag == TYBOLE)) && 
				(modifier[2] == AMINTEGER))
				{
				regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
				if (regtype[0] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPGT; goto AssignmentCastingChecks;}
				Opcode = wrdOpcode.u.Int = vmregJmpGTImmediate;
				prmv[1].u.Int = consValue[1].u.Int;
				prmv[1].Tag = TYNUM;
				prmv[1].Offset = AMINTEGER;
				prmv[1].Modifier = AMINTEGER;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],prmv[1],opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/*	(vmjmpgt __C"int" regs(b) label) */
			if ((cons[0] == TRUE) && 
			    ((consValue[0].Tag == TYNUM) || (consValue[0].Tag == TYCHAR) || (consValue[0].Tag == TYBOLE)) &&
				(modifier[1] == AMREGISTER) && 
				(modifier[2] == AMINTEGER))
				{
				regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
				if (regtype[1] == TRUE) {Opcode = wrdOpcode.u.Int = VMJMPGT; goto AssignmentCastingChecks;}
				Opcode = wrdOpcode.u.Int = vmregJmpLTImmediate;
				prmv[1].u.Int = consValue[0].u.Int;
				prmv[1].Tag = TYNUM;
				prmv[1].Offset = AMINTEGER;
				prmv[1].Modifier = AMINTEGER;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],prmv[1],opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;
    
		/*	(vmjump target) : Ip = target; */
		case VMJUMP:
			/*	(vjump regs(JumpPointer)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYJUMPPOINTER) && (modifier[1] == AMVOID) && (modifier[2] == AMVOID))
				{
				regtype[0] = FALSE;
				targetTyp = TYJUMPPOINTER;
				Opcode = wrdOpcode.u.Int = vmregJump;
				}
			break;
    
		/*	(vmmove source target) : target = source; */
		case VMMOVE:
		case vmnatLoadInteger:
		case vmnatLoadNumber:
			/*	(vmove regs(a) regs(b)) */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMVOID))
				{
				regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
				regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
				if (regtype[0] != regtype[1]) {Opcode = wrdOpcode.u.Int = VMMOVE; goto AssignmentCastingChecks;}
				if (regtype[0] == TRUE)
					Opcode = wrdOpcode.u.Int = vmregMoveNumber;
				else
					Opcode = wrdOpcode.u.Int = vmregMoveInteger;
				}
			else
			/*	(vmove Cv[label] regs(JumpPointer)) */
			if ((modifier[0] == AMCVOFFSET) && (modifier[1] == AMREGISTER) && (modifier[2] == AMVOID) &&
			    (BindArray(Cs)[offset[0]].Value.Tag == TYSYMBOL) &&
				(preftype[1] == TYJUMPPOINTER))
				{
				*ret = FOptimize2_AddGoto(gCP,gTP,compilerState,BindArray(Cs)[offset[0]].Value.u.Symbol,Pc->itsCurItemIndex + 1);
				newOpnd[0].Tag = TYNUM;
				newOpnd[0].u.Int = -1;
				newOpnd[0].Modifier = AMINTEGER;
				newOpnd[0].Offset = AMINTEGER;
				Opcode = wrdOpcode.u.Int = vmregLoadJmpPointer;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,newOpnd[0],opnd[1],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/*	(vmove Mvars(a) regs(b)) */
			if ((modifier[0] <= MAXRGOFFST) && (modifier[0] != AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMVOID))
				{
				regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
				targetTyp = preftype[1];
				if ((targetTyp == TYNUM) || (dcltype[0] == TYNUM)) Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				else if (targetTyp == TYREAL) Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				else if (targetTyp == TYJUMPPOINTER) Opcode = wrdOpcode.u.Int = vmregLoadJmpPointer;
				else if (targetTyp == TYCHARPOINTER) Opcode = wrdOpcode.u.Int = vmregObjPointer;
				else if (targetTyp == TYFLOATPOINTER) Opcode = wrdOpcode.u.Int = vmregObjPointer;
				else if (targetTyp == TYINTPOINTER) Opcode = wrdOpcode.u.Int = vmregObjPointer;
				else if (targetTyp == TYREALPOINTER) Opcode = wrdOpcode.u.Int = vmregObjPointer;
				else if (targetTyp == TYSHORTPOINTER) Opcode = wrdOpcode.u.Int = vmregObjPointer;
				else if (targetTyp == TYLONGPOINTER) Opcode = wrdOpcode.u.Int = vmregObjPointer;
				else if (targetTyp == TYWORDPOINTER) Opcode = wrdOpcode.u.Int = vmregObjPointer;
				else Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[1],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/*	(vmove integer(a) regs(b)) */
			if ((modifier[0] == AMINTEGER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMVOID))
				{
				regtype[1] = (dcltype[1] != TYREAL) ? FALSE : TRUE;
				if (regtype[1] == FALSE)
					Opcode = wrdOpcode.u.Int = vmregMoveImmediate;
				}
			else
			/*	(vmove regs(a) Mvars(b)) */
			if ((modifier[0] == AMREGISTER) && (modifier[1] <= MAXRGOFFST) && (modifier[1] != AMREGISTER) && (modifier[2] == AMVOID))
				{
				regtype[0] = (dcltype[0] != TYREAL) ? FALSE : TRUE;
				if (regtype[0] == TRUE)
					Opcode = wrdOpcode.u.Int = vmregSaveNumber;
				else
					Opcode = wrdOpcode.u.Int = vmregSaveInteger;
				}
			break;
    
		/*	(vmmovei source target) : target = source; */
		case VMMOVEI:
			/*	(vmovei regs(a) regs(b)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYREAL) && (modifier[1] == AMREGISTER) && (dcltype[1] != TYREAL) && (modifier[2] == AMVOID))
				{
				Opcode = wrdOpcode.u.Int = vmregInteger;
				}
			break;
    
		/*	(vmmoven source target) : target = source; */
		case VMMOVEN:
			/*	(vmoven regs(a) regs(b)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] != TYREAL) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYREAL) && (modifier[2] == AMVOID))
				{
				Opcode = wrdOpcode.u.Int = vmregNumber;
				}
			break;
    
		/*	(vmmul argument source target) : target = source * argument; */
		case VMMUL:
		case VMCMUL:
		case VMIMUL:
		case VMNMUL:
		case vmnatMulInteger:
		case vmnatMulNumber:
			/* ======================================================================== */
			/* (vmmul regs(Integer:argument) regs(Integer:source) regs(Integer:target)) */
			/* ======================================================================== */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER) &&
			    (dcltype[0] != TYREAL) && (dcltype[1] != TYREAL) && (dcltype[2] != TYREAL))
				{
				if ((offset[0] != offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					/* Multiply argument with target */
					Opcode = wrdOpcode.u.Int = vmregMulInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] == offset[2]))
					{
					/* Multiply argument with target */
					Opcode = wrdOpcode.u.Int = vmregMulInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] != offset[2]) && (offset[1] == offset[2]))
					{
					/* Multiply argument with target */
					Opcode = wrdOpcode.u.Int = vmregMulInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to temporary */
					*tmp = TSYMBOL("__Ri");
					temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					temporary.Tag = TYNUM;
					temporary.u.Int = temporary.Offset;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Multiply argument with target */
					Opcode = wrdOpcode.u.Int = vmregMulInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Move temporary to target */
					Opcode = wrdOpcode.u.Int = vmregMoveInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
 					}
				}
			else
			/* ===================================================================== */
			/* (vmmul regs(Number:argument) regs(Number:source) regs(Number:target)) */
			/* ===================================================================== */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER) &&
			    (dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYREAL))
				{
				if ((offset[0] != offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveNumber),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					/* Multiply argument with target */
					Opcode = wrdOpcode.u.Int = vmregMulNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] == offset[2]))
					{
					/* Divide argument into target */
					Opcode = wrdOpcode.u.Int = vmregMulNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] != offset[2]) && (offset[1] == offset[2]))
					{
					/* Multiply argument with target */
					Opcode = wrdOpcode.u.Int = vmregMulNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to temporary */
					*tmp = TSYMBOL("__RNx");
					temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					temporary.Tag = TYNUM;
					temporary.u.Int = temporary.Offset;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveNumber),opnd[1],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Multiply argument with target */
					Opcode = wrdOpcode.u.Int = vmregMulNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Move temporary to target */
					Opcode = wrdOpcode.u.Int = vmregMoveNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
 					}
				}
			else
			/* ================================================================ */
			/* (vmmul cons(Integer) regs(Integer:source) regs(Integer:target))  */
			/* ================================================================ */
			if ((cons[0] == TRUE) && ((dcltype[0] == TYNUM) || (dcltype[0] == TYCHAR)) && 
			    (modifier[1] == AMREGISTER) && (dcltype[1] != TYREAL) &&
			    (modifier[2] == AMREGISTER) && (dcltype[2] != TYREAL))
				{
				if (offset[1] != offset[2])
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				/* Multiply argument with target */
				index.Tag = TYNUM;
				index.u.Int = consValue[0].u.Int;
				index.Offset = AMINTEGER;
				index.Modifier = AMINTEGER;
				Opcode = wrdOpcode.u.Int = vmregMulImmediate;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* ========================================================================= */
			/* (vmmul vars(Integer:argument) regs(Integer:source) regs(Integer:target))  */
			/* ========================================================================= */
			if ((modifier[0] != AMREGISTER) && (modifier[0] <= MAXRGOFFST) && ((dcltype[0] == TYNUM) || (dcltype[0] == TYCHAR)) && 
			    (modifier[1] == AMREGISTER) && (dcltype[1] != TYREAL) &&
			    (modifier[2] == AMREGISTER) && (dcltype[2] != TYREAL))
				{
				/* Move argument to temporary */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregLoadInteger),opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);
				if (offset[1] != offset[2])
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				/* Multiply argument with target */
				Opcode = wrdOpcode.u.Int = vmregMulInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmref index pointer target) : target = pointer[index]; */
		case VMREF:
			/*	(vmref regs(i) regs(p) regs(t)) */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER))
				{
				if ((preftype[0] == TYNUM) && 
					(((preftype[1] == TYCHARPOINTER) && (preftype[2] == TYNUM)) ||
					 ((preftype[1] == TYJUMPPOINTER) && (preftype[2] == TYNUM)) ||
					 ((preftype[1] == TYFLOATPOINTER) && (preftype[2] == TYREAL)) ||
					 ((preftype[1] == TYINTPOINTER) && (preftype[2] == TYNUM)) ||
					 ((preftype[1] == TYREALPOINTER) && (preftype[2] == TYREAL)) ||
					 ((preftype[1] == TYSHORTPOINTER) && (preftype[2] == TYNUM))))
					{
					/* Convert to (vmregRefXType pointer index target */
					if (preftype[1] == TYCHARPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXCharacter;
					else if (preftype[1] == TYJUMPPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXCharacter;
					else if (preftype[1] == TYFLOATPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXFloat;
					else if (preftype[1] == TYINTPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXInteger;
					else if (preftype[1] == TYLONGPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXLong;
					else if (preftype[1] == TYREALPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXNumber;
					else if (preftype[1] == TYSHORTPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXShort;
					/* reference the pointer, using the index, placing result in the target register. */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[0],opnd[2]);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				}
			else
			/*	(vmref cvars("0") regs(p) regs(t)) */
			if ((cons[0] == TRUE) && 
				(consValue[0].Tag == TYNUM) && 
				(consValue[0].u.Int == 0) && 
				(modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER))
				{
				preftype[0] = TYNUM;
				if (((preftype[1] == TYCHARPOINTER) && (preftype[2] == TYNUM)) ||
					((preftype[1] == TYJUMPPOINTER) && (preftype[2] == TYNUM)) ||
					((preftype[1] == TYFLOATPOINTER) && (preftype[2] == TYREAL)) ||
					((preftype[1] == TYINTPOINTER) && (preftype[2] == TYNUM)) ||
					((preftype[1] == TYREALPOINTER) && (preftype[2] == TYREAL)) ||
					((preftype[1] == TYLONGPOINTER) && ((preftype[2] == TYLONG) || (preftype[2] == TYNUM ))) ||
					((preftype[1] == TYSHORTPOINTER) && (preftype[2] == TYNUM)))
					{
					/* Convert to (vmregRefType pointer target) */
					if (preftype[1] == TYCHARPOINTER) Opcode = wrdOpcode.u.Int = vmregRefCharacter;
					else if (preftype[1] == TYJUMPPOINTER) Opcode = wrdOpcode.u.Int = vmregRefCharacter;
					else if (preftype[1] == TYFLOATPOINTER) Opcode = wrdOpcode.u.Int = vmregRefFloat;
					else if (preftype[1] == TYINTPOINTER) Opcode = wrdOpcode.u.Int = vmregRefInteger;
					else if (preftype[1] == TYREALPOINTER) Opcode = wrdOpcode.u.Int = vmregRefNumber;
					else if (preftype[1] == TYSHORTPOINTER) Opcode = wrdOpcode.u.Int = vmregRefShort;
					else if (preftype[1] == TYLONGPOINTER) Opcode = wrdOpcode.u.Int = vmregRefLong;
					/* reference the pointer placing result in the target register. */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				}
			else
			/*	(vmref cvars(int) regs(p) regs(t)) */
			if ((cons[0] == TRUE) && 
				(consValue[0].Tag == TYNUM) && 
				(modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER))
				{
				preftype[0] = TYNUM;
				if (((preftype[1] == TYCHARPOINTER) && (preftype[2] == TYNUM)) ||
					((preftype[1] == TYJUMPPOINTER) && (preftype[2] == TYNUM)) ||
					((preftype[1] == TYFLOATPOINTER) && (preftype[2] == TYREAL)) ||
					((preftype[1] == TYINTPOINTER) && (preftype[2] == TYNUM)) ||
					((preftype[1] == TYREALPOINTER) && (preftype[2] == TYREAL)) ||
					((preftype[1] == TYLONGPOINTER) && ((preftype[2] == TYLONG) || (preftype[2] == TYNUM ))) ||
					((preftype[1] == TYSHORTPOINTER) && (preftype[2] == TYNUM)))
					{
					/* Convert to (vmregRefXType pointer index target) */
					if (preftype[1] == TYCHARPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXCharacter;
					else if (preftype[1] == TYJUMPPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXCharacter;
					else if (preftype[1] == TYFLOATPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXFloat;
					else if (preftype[1] == TYINTPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXInteger;
					else if (preftype[1] == TYLONGPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXLong;
					else if (preftype[1] == TYREALPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXNumber;
					else if (preftype[1] == TYSHORTPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXShort;
					/* Move the constant into the temporary Index register (__Ri). */
					*tmp = TSYMBOL("__Ri");
					index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					index.Tag = TYNUM;
					index.u.Int = index.Offset;
					movOpcode = TINT(vmregLoadInteger);
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,movOpcode,opnd[0],index,opndAMVOID);
					ExitOnError(*ret);
					/* Index reference the pointer placing result in the target register. */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],index,opnd[2]);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				}
			else
			/*	(vmref cvars(int) regs(p) vars(t)) */
			if ((cons[0] == TRUE) && 
				(consValue[0].Tag == TYNUM) && 
				(modifier[1] == AMREGISTER))
				{
				preftype[0] = TYNUM;
				if (preftype[1] == TYWORDPOINTER)
					{
					/* Convert to (vmregRefXType pointer index target) */
					if (preftype[1] == TYWORDPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXWord;
					/* Move the constant into the temporary Index register (__Ri). */
					*tmp = TSYMBOL("__Ri");
					index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					index.Tag = TYNUM;
					index.u.Int = index.Offset;
					movOpcode = TINT(vmregLoadInteger);
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,movOpcode,opnd[0],index,opndAMVOID);
					ExitOnError(*ret);
					/* Index reference the pointer placing result in the target register. */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],index,opnd[2]);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				}
			else
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] != AMREGISTER))
				{
				if (preftype[1] == TYWORDPOINTER)
					{
					/* Convert to (vmregRefXType pointer index target) */
					if (preftype[1] == TYWORDPOINTER) Opcode = wrdOpcode.u.Int = vmregRefXWord;
					/* Index reference the pointer placing result in the target register. */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[0],opnd[2]);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				}


			break;
    
		/*	(vmrefbytvector index ByteVector target) : pointer = &ByteVector; target = pointer[index]; */
		case VMREFBYTVECTOR:
			/*	(vmrefbytvector regs(Integer:i) vars(ByteVector:v) regs(Integer:t)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (dcltype[1] == TYBYTEVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[0],opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmrefbytvector cvars("0") vars(ByteVector:v) regs(Integer:t)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (dcltype[1] == TYBYTEVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmrefbytvector cvars("n") vars(ByteVector:v) regs(Integer:t)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (dcltype[1] == TYBYTEVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,index,opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmreffltvector index FltVector target) : pointer = &FltVector; target = pointer[index]; */
		case VMREFFLTVECTOR:
			/*	(vmreffltvector regs(Integer:i) vars(NumVector:v) regs(Number:t)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (dcltype[1] == TYFLTVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXFloat);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[0],opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmreffltvector cvars("0") vars(FltVector:v) regs(Number:t)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (dcltype[1] == TYFLTVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefFloat);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmreffltvector cvars("n") vars(FltVector:v) regs(Number:t)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (dcltype[1] == TYFLTVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXFloat);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,index,opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmrefintvector index IntVector target) : pointer = &IntVector; target = pointer[index]; */
		case VMREFINTVECTOR:
			/*	(vmrefintvector regs(Integer:i) vars(IntVector:v) regs(Integer:t)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (dcltype[1] == TYINTVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[0],opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmrefintvector cvars("0") vars(IntVector:v) regs(Integer:t)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (dcltype[1] == TYINTVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmrefintvector cvars("n") vars(IntVector:v) regs(Integer:t)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (dcltype[1] == TYINTVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,index,opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmrefnumvector index NumVector target) : pointer = &NumVector; target = pointer[index]; */
		case VMREFNUMVECTOR:
			/*	(vmrefnumvector regs(Integer:i) vars(NumVector:v) regs(Number:t)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (dcltype[1] == TYNUMVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXNumber);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[0],opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmrefnumvector cvars("0") vars(NumVector:v) regs(Number:t)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (dcltype[1] == TYNUMVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefNumber);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmrefnumvector cvars("n") vars(NumVector:v) regs(Number:t)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (dcltype[1] == TYNUMVECTOR) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYREAL))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXNumber);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,index,opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmrefstring index String target) : pointer = &String; target = pointer[index]; */
		case VMREFSTRING:
			/*	(vmrefstring regs(Integer:i) vars(String:v) regs(Integer:t)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (dcltype[1] == TYSTRING) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[0],opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmrefstring cvars("0") vars(String:v) regs(Integer:t)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (dcltype[1] == TYSTRING) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmrefstring cvars("n") vars(String:v) regs(Integer:t)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (dcltype[1] == TYSTRING) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,index,opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmrefsymbol index Symbol target) : pointer = &Symbol; target = pointer[index]; */
		case VMREFSYMBOL:
			/*	(vmrefsymbol regs(Integer:i) vars(Symbol:v) regs(Integer:t)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (dcltype[1] == TYSYMBOL) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[0],opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmrefsymbol cvars("0") vars(Symbol:v) regs(Integer:t)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (dcltype[1] == TYSYMBOL) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmrefsymbol cvars("n") vars(Symbol:v) regs(Integer:t)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (dcltype[1] == TYSYMBOL) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,index,opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmreftext index Text target) : pointer = &Text; target = pointer[index]; */
		case VMREFTEXT:
			/*	(vmreftext regs(Integer:i) vars(Text:v) regs(Integer:t)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (dcltype[1] == TYTEXT) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[0],opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmreftext cvars("0") vars(Text:v) regs(Integer:t)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (dcltype[1] == TYTEXT) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,opnd[2],opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmreftext cvars("n") vars(Text:v) regs(Integer:t)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (dcltype[1] == TYTEXT) && (modifier[2] == AMREGISTER) && (dcltype[2] == TYNUM))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index reference, from the pointer address, into the target. */
				newOpcode = TINT(vmregRefXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,pointer,index,opnd[2]);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmset index source pointer) : pointer[index] = source; */
		case VMSET:
			/*	(vmset regs(i) regs(s) regs(p)) */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER))
				{
				if ((preftype[0] == TYNUM) && 
					(((preftype[2] == TYCHARPOINTER) && (preftype[1] == TYNUM)) ||
					 ((preftype[2] == TYJUMPPOINTER) && (preftype[1] == TYNUM)) ||
					 ((preftype[2] == TYFLOATPOINTER) && (preftype[1] == TYREAL)) ||
					 ((preftype[2] == TYINTPOINTER) && (preftype[1] == TYNUM)) ||
					 ((preftype[2] == TYREALPOINTER) && (preftype[1] == TYREAL)) ||
					 ((preftype[2] == TYSHORTPOINTER) && (preftype[1] == TYNUM))))
					{
					/* Convert to (vmregSetXType source index pointer) */
					if (preftype[2] == TYCHARPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXCharacter;
					else if (preftype[2] == TYJUMPPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXCharacter;
					else if (preftype[2] == TYFLOATPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXFloat;
					else if (preftype[2] == TYINTPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXInteger;
					else if (preftype[2] == TYLONGPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXLong;
					else if (preftype[2] == TYREALPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXNumber;
					else if (preftype[2] == TYSHORTPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXShort;
					/* store the source, at the pointer address plus the index value. */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[0],opnd[2]);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				}
			else
			/*	(vmset cvars("0") regs(s) regs(p)) */
			if ((cons[0] == TRUE) && 
				(consValue[0].Tag == TYNUM) && 
				(consValue[0].u.Int == 0) && 
				(modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER))
				{
				preftype[0] = TYNUM;
				if (((preftype[2] == TYCHARPOINTER) && (preftype[1] == TYNUM)) ||
					((preftype[2] == TYJUMPPOINTER) && (preftype[1] == TYNUM)) ||
					((preftype[2] == TYFLOATPOINTER) && (preftype[1] == TYREAL)) ||
					((preftype[2] == TYINTPOINTER) && (preftype[1] == TYNUM)) ||
					((preftype[2] == TYREALPOINTER) && (preftype[1] == TYREAL)) ||
					((preftype[2] == TYSHORTPOINTER) && (preftype[1] == TYNUM)))
					{
					/* Convert to (vmregSetType source pointer) */
					if (preftype[2] == TYCHARPOINTER) Opcode = wrdOpcode.u.Int = vmregSetCharacter;
					else if (preftype[2] == TYJUMPPOINTER) Opcode = wrdOpcode.u.Int = vmregSetCharacter;
					else if (preftype[2] == TYFLOATPOINTER) Opcode = wrdOpcode.u.Int = vmregSetFloat;
					else if (preftype[2] == TYINTPOINTER) Opcode = wrdOpcode.u.Int = vmregSetInteger;
					else if (preftype[2] == TYREALPOINTER) Opcode = wrdOpcode.u.Int = vmregSetNumber;
					else if (preftype[2] == TYSHORTPOINTER) Opcode = wrdOpcode.u.Int = vmregSetShort;
					/* store the source, at the pointer address. */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				}
			else
			/*	(vmset cvars(INT) regs(s) regs(p)) */
			if ((cons[0] == TRUE) && 
				(consValue[0].Tag == TYNUM) && 
				(modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER))
				{
				preftype[0] = TYNUM;
				if (((preftype[2] == TYCHARPOINTER) && (preftype[1] == TYNUM)) ||
					((preftype[2] == TYJUMPPOINTER) && (preftype[1] == TYNUM)) ||
					((preftype[2] == TYFLOATPOINTER) && (preftype[1] == TYREAL)) ||
					((preftype[2] == TYINTPOINTER) && (preftype[1] == TYNUM)) ||
					((preftype[2] == TYREALPOINTER) && (preftype[1] == TYREAL)) ||
					((preftype[2] == TYSHORTPOINTER) && (preftype[1] == TYNUM)))
					{
					/* Convert to (vmregSetXType source pointer) */
					if (preftype[2] == TYCHARPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXCharacter;
					else if (preftype[2] == TYJUMPPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXCharacter;
					else if (preftype[2] == TYFLOATPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXFloat;
					else if (preftype[2] == TYINTPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXInteger;
					else if (preftype[2] == TYLONGPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXLong;
					else if (preftype[2] == TYREALPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXNumber;
					else if (preftype[2] == TYSHORTPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXShort;
					/* Move the constant into the temporary Index register (__Ri). */
					*tmp = TSYMBOL("__Ri");
					index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					index.Tag = TYNUM;
					index.u.Int = index.Offset;
					movOpcode = TINT(vmregLoadInteger);
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,movOpcode,opnd[0],index,opndAMVOID);
					ExitOnError(*ret);
					/* Index store the source, at the pointer address. */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],index,opnd[2]);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				}
			else
			/*	(vmset cvars(INT) vars(s) regs(p)) */
			if ((cons[0] == TRUE) && 
				(consValue[0].Tag == TYNUM) && 
				(modifier[2] == AMREGISTER))
				{
				if ((preftype[0] == TYNUM) && 
					 (preftype[2] == TYWORDPOINTER) )
					{
					/* Convert to (vmregSetXType source index pointer) */
					if (preftype[2] == TYWORDPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXWord;
					/* Move the constant into the temporary Index register (__Ri). */
					*tmp = TSYMBOL("__Ri");
					index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					index.Tag = TYNUM;
					index.u.Int = index.Offset;
					movOpcode = TINT(vmregLoadInteger);
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,movOpcode,opnd[0],index,opndAMVOID);
					ExitOnError(*ret);
					/* store the source, at the pointer address plus the index value. */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],index,opnd[2]);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				}
			else
			if ((modifier[0] == AMREGISTER) && (modifier[1] != AMREGISTER) && (modifier[2] == AMREGISTER))
				{
				if ((preftype[0] == TYNUM) && 
					 (preftype[2] == TYWORDPOINTER) )
					{
					/* Convert to (vmregSetXType source index pointer) */
					if (preftype[2] == TYWORDPOINTER) Opcode = wrdOpcode.u.Int = vmregSetXWord;
					/* store the source, at the pointer address plus the index value. */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[0],opnd[2]);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				}
			break;

		/*	(vmsetbytvector index source ByteVector) : pointer = &ByteVector; pointer[index] = source; */
		case VMSETBYTVECTOR:
			/*	(vmsetbytvector regs(Integer:i) regs(Integer:s) vars(ByteVector:v)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYNUM) && (dcltype[2] == TYBYTEVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],opnd[0],pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetbytvector cvars("0") regs(Integer:s) vars(ByteVector:v)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYNUM) && (dcltype[2] == TYBYTEVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Store the source, at the pointer address. */
				newOpcode = TINT(vmregSetCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetbytvector cvars("n") regs(Integer:s) vars(ByteVector:v)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYNUM) && (dcltype[2] == TYBYTEVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],index,pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmsetintvector index source IntVector) : pointer = &IntVector; pointer[index] = source; */
		case VMSETINTVECTOR:
			/*	(vmsetintvector regs(Integer:i) regs(Integer:s) vars(IntVector:v)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYNUM) && (dcltype[2] == TYINTVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],opnd[0],pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetintvector cvars("0") regs(Integer:s) vars(IntVector:v)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYNUM) && (dcltype[2] == TYINTVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Store the source, at the pointer address. */
				newOpcode = TINT(vmregSetInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetintvector cvars("n") regs(Integer:s) vars(IntVector:v)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYNUM) && (dcltype[2] == TYINTVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],index,pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmsetstring index source ByteVector) : pointer = &String; pointer[index] = source; */
		case VMSETSTRING:
			/*	(vmsetstring regs(Integer:i) regs(Integer:s) vars(String:v)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYNUM) && (dcltype[2] == TYSTRING))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],opnd[0],pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetstring cvars("0") regs(Integer:s) vars(String:v)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYNUM) && (dcltype[2] == TYSTRING))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Store the source, at the pointer address. */
				newOpcode = TINT(vmregSetCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetstring cvars("n") regs(Integer:s) vars(String:v)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYNUM) && (dcltype[2] == TYSTRING))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXCharacter);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],index,pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmsetnummatrix index source NumVector) : pointer = &NumMatrix; pointer[index] = source; */
		case VMSETNUMMATRIX:
			/*	(vmsetnummatrix regs(Integer:i) regs(Number:s) vars(NumMatrix:v)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYREAL) && (dcltype[2] == TYNUMMATRIX))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXNumber);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],opnd[0],pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetnummatrix cvars("0") regs(Number:s) vars(NumMatrix:v)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYREAL) && (dcltype[2] == TYNUMMATRIX))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Store the source, at the pointer address. */
				newOpcode = TINT(vmregSetNumber);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetnummatrix cvars("n") regs(Number:s) vars(NumMatrix:v)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYREAL) && (dcltype[2] == TYNUMMATRIX))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXNumber);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],index,pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmsetnumvector index source NumVector) : pointer = &NumVector; pointer[index] = source; */
		case VMSETNUMVECTOR:
			/*	(vmsetnumvector regs(Integer:i) regs(Number:s) vars(NumVector:v)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYREAL) && (dcltype[2] == TYNUMVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXNumber);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],opnd[0],pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetnumvector cvars("0") regs(Number:s) vars(NumVector:v)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYREAL) && (dcltype[2] == TYNUMVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Store the source, at the pointer address. */
				newOpcode = TINT(vmregSetNumber);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetnumvector cvars("n") regs(Number:s) vars(NumVector:v)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYREAL) && (dcltype[2] == TYNUMVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXNumber);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],index,pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmsetfltvector index source FltVector) : pointer = &FltVector; pointer[index] = source; */
		case VMSETFLTVECTOR:
			/*	(vmsetfltvector regs(Integer:i) regs(Number:s) vars(FltVector:v)) */
			if ((modifier[0] == AMREGISTER) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYREAL) && (dcltype[2] == TYFLTVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXFloat);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],opnd[0],pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetnumvector cvars("0") regs(Number:s) vars(FltVector:v)) */
			if ((cons[0] == TRUE) && (consValue[0].u.Int == 0) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYREAL) && (dcltype[2] == TYFLTVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Store the source, at the pointer address. */
				newOpcode = TINT(vmregSetFloat);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],pointer,opndAMVOID);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			/*	(vmsetintvector cvars("n") regs(Number:s) vars(FltVector:v)) */
			if ((cons[0] == TRUE) && (dcltype[0] == TYNUM) && (modifier[1] == AMREGISTER) && (dcltype[1] == TYREAL) && (dcltype[2] == TYFLTVECTOR))
				{
				/* Move a pointer to the object into the temporary Pointer register (__Rp). */
				*tmp = TSYMBOL("__Rp");
				pointer = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				pointer.Tag = TYNUM;
				pointer.u.Int = pointer.Offset;
				newOpcode = TINT(vmregObjPointer);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[2],pointer,opndAMVOID);
				ExitOnError(*ret);
				/* Move the index constant into the temporary Index register (__Ri). */
				*tmp = TSYMBOL("__Ri");
				index = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				index.Tag = TYNUM;
				index.u.Int = index.Offset;
				newOpcode = TINT(vmregLoadInteger);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],index,opndAMVOID);
				ExitOnError(*ret);
				/* Index store the source, at the pointer address. */
				newOpcode = TINT(vmregSetXFloat);
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[1],index,pointer);
				ExitOnError(*ret);
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;

		/*	(vmsub argument source target) : target = source - argument; */
		case VMSUB:
		case VMCSUB:
		case VMISUB:
		case VMNSUB:
		case vmnatSubInteger:
		case vmnatSubNumber:
			/* ======================================================================== */
			/* (vmsub regs(Integer:argument) regs(Integer:source) regs(Integer:target)) */
			/* ======================================================================== */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER) &&
			    (dcltype[0] != TYREAL) && (dcltype[1] != TYREAL) && (dcltype[2] != TYREAL))
				{
				/* We compute the pointer bitshift amount for these arguments.      */					
				/* Note: We must follow with either Integer or pointer subtraction. */					
				targetTyp = preftype[2];
				if (targetTyp == TYJUMPPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYFLOATPOINTER) bitShift = BITSIZEOFFLOAT;
				else if (targetTyp == TYINTPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYREALPOINTER) bitShift = BITSIZEOFREAL;
				else if (targetTyp == TYSHORTPOINTER) bitShift = BITSIZEOFSHORT;
				else if (targetTyp == TYLONGPOINTER) bitShift = BITSIZEOFNUM32;
				else if (targetTyp == TYWORDPOINTER) bitShift = BITSIZEOFTVAL;
				else bitShift = 0;
				if ((offset[0] != offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					/* Subtract argument from target */
					if (bitShift != 0)
						{
						/* Use pointer arithmetic and specify the bitshift amount. */
						index.Tag = TYNUM;
						index.u.Int = bitShift;
						index.Offset = AMINTEGER;
						index.Modifier = AMINTEGER;
						Opcode = wrdOpcode.u.Int = vmregSubPointer;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[0],opnd[2]);
						ExitOnError(*ret);
						}
					else
						{
						/* Use normal integer arithmetic with no bitshift amount. */
						Opcode = wrdOpcode.u.Int = vmregSubInteger;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						}
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] == offset[2]))
					{
					/* Subtract argument from target */
					if (bitShift != 0)
						{
						/* Use pointer arithmetic and specify the bitshift amount. */
						index.Tag = TYNUM;
						index.u.Int = bitShift;
						index.Offset = AMINTEGER;
						index.Modifier = AMINTEGER;
						Opcode = wrdOpcode.u.Int = vmregSubPointer;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[0],opnd[2]);
						ExitOnError(*ret);
						}
					else
						{
						/* Use normal integer arithmetic with no bitshift amount. */
						Opcode = wrdOpcode.u.Int = vmregSubInteger;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						}
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] != offset[2]) && (offset[1] == offset[2]))
					{
					/* Subtract argument from target */
					if (bitShift != 0)
						{
						/* Use pointer arithmetic and specify the bitshift amount. */
						index.Tag = TYNUM;
						index.u.Int = bitShift;
						index.Offset = AMINTEGER;
						index.Modifier = AMINTEGER;
						Opcode = wrdOpcode.u.Int = vmregSubPointer;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[0],opnd[2]);
						ExitOnError(*ret);
						}
					else
						{
						/* Use normal integer arithmetic with no bitshift amount. */
						Opcode = wrdOpcode.u.Int = vmregSubInteger;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
						ExitOnError(*ret);
						}
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to temporary */
					*tmp = TSYMBOL("__Ri");
					temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					temporary.Tag = TYNUM;
					temporary.u.Int = temporary.Offset;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Subtract argument from temporary */
					if (bitShift != 0)
						{
						/* Use pointer arithmetic and specify the bitshift amount. */
						prmv[0].Tag = TYNUM;
						index.u.Int = bitShift;
						index.Offset = AMINTEGER;
						index.Modifier = AMINTEGER;
						Opcode = wrdOpcode.u.Int = vmregSubPointer;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[0],temporary);
						ExitOnError(*ret);
						}
					else
						{
						/* Use normal integer arithmetic with no bitshift amount. */
						Opcode = wrdOpcode.u.Int = vmregSubInteger;
						*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
						ExitOnError(*ret);
						}
					/* Move temporary to target */
					Opcode = wrdOpcode.u.Int = vmregMoveInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
 					}
				}
			else
			/* ===================================================================== */
			/* (vmsub regs(Number:argument) regs(Number:source) regs(Number:target)) */
			/* ===================================================================== */
			if ((modifier[0] == AMREGISTER) && (modifier[1] == AMREGISTER) && (modifier[2] == AMREGISTER) &&
			    (dcltype[0] == TYREAL) && (dcltype[1] == TYREAL) && (dcltype[2] == TYREAL))
				{
				if ((offset[0] != offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveNumber),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					/* Subtract argument from target. */
					Opcode = wrdOpcode.u.Int = vmregSubNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] == offset[2]))
					{
					/* Subtract argument from target. */
					Opcode = wrdOpcode.u.Int = vmregSubNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] != offset[2]) && (offset[1] == offset[2]))
					{
					/* Subtract argument from target. */
					Opcode = wrdOpcode.u.Int = vmregSubNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
					}
				else
				if ((offset[0] == offset[2]) && (offset[1] != offset[2]))
					{
					/* Move source to temporary */
					*tmp = TSYMBOL("__RNx");
					temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
					temporary.Tag = TYNUM;
					temporary.u.Int = temporary.Offset;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveNumber),opnd[1],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Subtract argument from temporary. */
					Opcode = wrdOpcode.u.Int = vmregSubNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
					ExitOnError(*ret);
					/* Move temporary to target */
					Opcode = wrdOpcode.u.Int = vmregMoveNumber;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
					goto FixJumps;
 					}
				}
			else
			/* ================================================================ */
			/* (vmsub cons(Integer) regs(Integer:source) regs(Integer:target))  */
			/* ================================================================ */
			if ((cons[0] == TRUE) && ((dcltype[0] == TYNUM) || (dcltype[0] == TYCHAR)) && 
			    (modifier[1] == AMREGISTER) && (dcltype[1] != TYREAL) &&
			    (modifier[2] == AMREGISTER) && (dcltype[2] != TYREAL))
				{
				/* Manage the case for Integer target */
				
				/* We compute the pointer bitshift amount for these arguments.      */					
				/* Note: We must follow with either Integer or pointer subtraction. */					
				targetTyp = preftype[2];
				if (targetTyp == TYJUMPPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYFLOATPOINTER) bitShift = BITSIZEOFFLOAT;
				else if (targetTyp == TYINTPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYREALPOINTER) bitShift = BITSIZEOFREAL;
				else if (targetTyp == TYSHORTPOINTER) bitShift = BITSIZEOFSHORT;
				else if (targetTyp == TYLONGPOINTER) bitShift = BITSIZEOFNUM32;
				else if (targetTyp == TYWORDPOINTER) bitShift = BITSIZEOFTVAL;
				else bitShift = 0;
				if (offset[1] != offset[2])
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				/* Subtract argument from target */
				if (bitShift != 0)
					{
					/* Use normal integer arithmetic with no bitshift amount. */
					index.Tag = TYNUM;
					index.u.Int = consValue[0].u.Int << bitShift;
					index.Offset = AMINTEGER;
					index.Modifier = AMINTEGER;
					Opcode = wrdOpcode.u.Int = vmregSubImmediate;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				else
					{
					/* Use normal integer arithmetic with no bitshift amount. */
					index.Tag = TYNUM;
					index.u.Int = consValue[0].u.Int;
					index.Offset = AMINTEGER;
					index.Modifier = AMINTEGER;
					Opcode = wrdOpcode.u.Int = vmregSubImmediate;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* ========================================================================= */
			/* (vmsub vars(Integer:argument) regs(Integer:source) regs(Integer:target))  */
			/* ========================================================================= */
			if ((modifier[0] != AMREGISTER) && (modifier[0] <= MAXRGOFFST) && ((dcltype[0] == TYNUM) || (dcltype[0] == TYCHAR)) && 
			    (modifier[1] == AMREGISTER) && (dcltype[1] != TYREAL) &&
			    (modifier[2] == AMREGISTER) && (dcltype[2] != TYREAL))
				{
				/* Manage the case for Integer target */
				
				/* We compute the pointer bitshift amount for these arguments.      */					
				/* Note: We must follow with either Integer or pointer subtraction. */					
				targetTyp = preftype[2];
				if (targetTyp == TYJUMPPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYFLOATPOINTER) bitShift = BITSIZEOFFLOAT;
				else if (targetTyp == TYINTPOINTER) bitShift = BITSIZEOFNUM;
				else if (targetTyp == TYREALPOINTER) bitShift = BITSIZEOFREAL;
				else if (targetTyp == TYSHORTPOINTER) bitShift = BITSIZEOFSHORT;
				else if (targetTyp == TYLONGPOINTER) bitShift = BITSIZEOFNUM32;
				else if (targetTyp == TYWORDPOINTER) bitShift = BITSIZEOFTVAL;
				else bitShift = 0;
				/* Move argument to temporary */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregLoadInteger),opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);
				if (offset[1] != offset[2])
					{
					/* Move source to target */
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,TINT(vmregMoveInteger),opnd[1],opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				/* Subtract temporary from target */
				if (bitShift != 0)
					{
					/* Use pointer arithmetic and specify the bitshift amount. */
					index.Tag = TYNUM;
					index.u.Int = bitShift;
					index.Offset = AMINTEGER;
					index.Modifier = AMINTEGER;
					Opcode = wrdOpcode.u.Int = vmregSubPointer;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,index,temporary,opnd[2]);
					ExitOnError(*ret);
					}
				else
					{
					/* Use normal integer arithmetic with no bitshift amount. */
					Opcode = wrdOpcode.u.Int = vmregSubInteger;
					*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
					ExitOnError(*ret);
					}
				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			break;
   		}
	}


/* *************************************************************************************/  
/* Memory Instruction Optimization													   */
/* Convert memory instructions to faster strongly typed instructions (where possible). */
/* *************************************************************************************/  

AssignmentCastingChecks:

switch (Opcode)
	{
	case VMADD:
		/* Set the opcode based on the type of the target. */
		if ((preftype[2] == TYNUM) || (preftype[2] == TYCHARPOINTER) || (preftype[2] == TYSHORTPOINTER) || (preftype[2] == TYINTPOINTER) || (preftype[2] == TYREALPOINTER) || (preftype[2] == TYFLOATPOINTER) || (preftype[2] == TYJUMPPOINTER) || (preftype[2] == TYWORDPOINTER)) 
			Opcode = wrdOpcode.u.Int = VMADDI;
		else
		if (preftype[2] == TYREAL) 
			Opcode = wrdOpcode.u.Int = VMADDN;
		break;
		
	case VMAPPLY:
	case VMCALL:
	case VMSEND:
	case VMREF:
		/* Manage the case where the target variable is NOT of type Word. */
		if (tempReg[2] == TRUE)
			{
			/* Place the result in the Word temporary __Tx variable. */
			prmv[0].Tag = TYNUM;
			prmv[0].u.Int = 0;
			prmv[0].Offset = 0;
			prmv[0].Modifier = AMTVOFFSET;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[1],prmv[0]);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			/* Set the move opcode based on the type of the original target. */
			if (dcltype[2] == TYNUM) 
				newOpcode = TINT(VMMOVEI);
			else
			if (dcltype[2] == TYREAL) 
				newOpcode = TINT(VMMOVEN);
			else
				newOpcode = TINT(VMMOVE);
			/* Move the Word temporary __Tx variable into the original target. */
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,prmv[0],opnd[2],opndAMVOID);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	case VMARGCOUNT:
		/* Manage the case where the target variable is NOT of type Word. */
		if ((dcltype[0] != TYNUM) && (tempReg[0] == TRUE))
			{
			/* Place the result in the Word temporary __Tx variable. */
			prmv[0].Tag = TYNUM;
			prmv[0].u.Int = 0;
			prmv[0].Offset = 0;
			prmv[0].Modifier = AMTVOFFSET;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,prmv[0],opndAMVOID,opndAMVOID);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			/* Set the move opcode based on the type of the original target. */
			if (dcltype[2] == TYNUM) 
				newOpcode = TINT(VMMOVEI);
			else
			if (dcltype[2] == TYREAL) 
				newOpcode = TINT(VMMOVEN);
			else
				newOpcode = TINT(VMMOVE);
			/* Move the Word temporary __Tx variable into the original target. */
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,prmv[0],opnd[0],opndAMVOID);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	case VMARGFETCH:
		/* Manage the case where the target variable is NOT of type Word. */
		if ((dcltype[1] != TYNUM) && (tempReg[1] == TRUE))
			{
			/* Place the result in the Word temporary __Tx variable. */
			prmv[0].Tag = TYNUM;
			prmv[0].u.Int = 0;
			prmv[0].Offset = 0;
			prmv[0].Modifier = AMTVOFFSET;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],prmv[0],opndAMVOID);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			/* Set the move opcode based on the type of the original target. */
			if (dcltype[2] == TYNUM) 
				newOpcode = TINT(VMMOVEI);
			else
			if (dcltype[2] == TYREAL) 
				newOpcode = TINT(VMMOVEN);
			else
				newOpcode = TINT(VMMOVE);
			/* Move the Word temporary __Tx variable into the original target. */
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,prmv[0],opnd[1],opndAMVOID);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	case VMDIV:
		/* Set the opcode based on the type of the target. */
		if ((preftype[2] == TYNUM) || (preftype[2] == TYCHARPOINTER) || (preftype[2] == TYSHORTPOINTER) || (preftype[2] == TYINTPOINTER) || (preftype[2] == TYREALPOINTER) || (preftype[2] == TYFLOATPOINTER) || (preftype[2] == TYJUMPPOINTER) || (preftype[2] == TYWORDPOINTER)) 
			Opcode = wrdOpcode.u.Int = VMDIVI;
		else
		if (preftype[2] == TYREAL) 
			Opcode = wrdOpcode.u.Int = VMDIVN;
		break;
		
	case VMMOVE:
		/* Manage the case where the target variable is NOT of type Word. */
		if ((dcltype[0] != dcltype[1]) && (tempReg[1] == TRUE))
			{
			/* Set the move opcode based on the type of the original target. */
			if (dcltype[2] == TYNUM) 
				newOpcode = TINT(VMMOVEI);
			else
			if (dcltype[2] == TYREAL) 
				newOpcode = TINT(VMMOVEN);
			else
				newOpcode = TINT(VMMOVE);
			/* Complete the original Move into the original target temporary register. */
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,newOpcode,opnd[0],opnd[1],opndAMVOID);
			ExitOnError(*ret);
			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	case VMMUL:
		/* Set the opcode based on the type of the target. */
		if ((preftype[2] == TYNUM) || (preftype[2] == TYCHARPOINTER) || (preftype[2] == TYSHORTPOINTER) || (preftype[2] == TYINTPOINTER) || (preftype[2] == TYREALPOINTER) || (preftype[2] == TYFLOATPOINTER) || (preftype[2] == TYJUMPPOINTER) || (preftype[2] == TYWORDPOINTER)) 
			Opcode = wrdOpcode.u.Int = VMMULI;
		else
		if (preftype[2] == TYREAL) 
			Opcode = wrdOpcode.u.Int = VMMULN;
		break;

	case VMSUB:
		/* Set the opcode based on the type of the target. */
		if ((preftype[2] == TYNUM) || (preftype[2] == TYCHARPOINTER) || (preftype[2] == TYSHORTPOINTER) || (preftype[2] == TYINTPOINTER) || (preftype[2] == TYREALPOINTER) || (preftype[2] == TYFLOATPOINTER) || (preftype[2] == TYJUMPPOINTER) || (preftype[2] == TYWORDPOINTER)) 
			Opcode = wrdOpcode.u.Int = VMSUBI;
		else
		if (preftype[2] == TYREAL) 
			Opcode = wrdOpcode.u.Int = VMSUBN;
		break;
	}

/* ********************************************************************************************/  
/* Memory Instruction To Register Instruction Optimization							          */
/* Convert memory instructions to faster sequences of register instructions (where possible). */
/*                                   											              */
/* Note: This section handles only those cases where operands are mixed between registers     */
/*       operands and memory operands.                                                        */
/* ********************************************************************************************/  


switch (Opcode)
	{
	case VMADDI:
	case VMIADD:
	case vmnatAddInteger:
		/* Manage instructions where target is an Integer register and preferred types are all Integer. */
		if ((modifier[2] == AMREGISTER) && (preftype[0] == TYNUM) && (preftype[1] == TYNUM) && (preftype[2] == TYNUM)) 
			{
			/* Manage cases where the argument and target are the same. */
			/* Note: (vmiadd regA memX regA)                            */
			if ((modifier[1] != modifier[2]) && (modifier[0] == modifier[2]) && (opnd[0].Offset == opnd[2].Offset))
				{
				/* Move source to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Add temporary to target. */
				Opcode = wrdOpcode.u.Int = vmregAddInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source and target are the same. */
			/* Note: (vmiadd memX regA regA)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == modifier[2]) && (opnd[1].Offset == opnd[2].Offset))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Add temporary to target. */
				Opcode = wrdOpcode.u.Int = vmregAddInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the argument is not a register. */
			/* Note: (vmiadd memX regA regB)                      */
			if ((modifier[0] != modifier[2]) && (modifier[1] == AMREGISTER))
				{
				/* Move argument to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Add source to target. */
				Opcode = wrdOpcode.u.Int = vmregAddInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source is not a register. */
			/* Note: (vmiadd regA memX regB)                    */
			if ((modifier[0] == AMREGISTER) && (modifier[1] != modifier[2]))
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Add argument to target. */
				Opcode = wrdOpcode.u.Int = vmregAddInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Integer and Integer. */
		/* Note: (vmadd Number:regA Integer:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move argument to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			if (opnd[1].Offset != opnd[2].Offset)
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);
				}

			/* Add temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregAddInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Number and Integer. */
		/* Note: (vmadd Integer:regA Number:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move source to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Add argument temporary to temporary. */
			Opcode = wrdOpcode.u.Int = vmregAddInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Move temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregMoveInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Number and Integer. */
		/* Note: (vmadd Number:regA Number:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move source to target (converting to Integer). */
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
			ExitOnError(*ret);

			/* Move argument to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Add temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregAddInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	case VMADDN:
	case VMNADD:
	case vmnatAddNumber:
		/* Manage instructions where target is a Number register and preferred types are all Number. */
		if ((modifier[2] == AMREGISTER) && (preftype[0] == TYREAL) && (preftype[1] == TYREAL) && (preftype[2] == TYREAL)) 
			{
			/* Manage cases where the argument and target are the same. */
			/* Note: (vmiadd regA memX regA)                            */
			if ((modifier[1] != modifier[2]) && (modifier[0] == modifier[2]) && (opnd[0].Offset == opnd[2].Offset))
				{
				/* Move source to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__RNx");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Add temporary to target. */
				Opcode = wrdOpcode.u.Int = vmregAddNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source and target are the same. */
			/* Note: (vmiadd memX regA regA)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == modifier[2]) && (opnd[1].Offset == opnd[2].Offset))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__RNx");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Add temporary to target. */
				Opcode = wrdOpcode.u.Int = vmregAddNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the argument is not a register. */
			/* Note: (vmiadd memX regA regB)                      */
			if ((modifier[0] != modifier[2]) && (modifier[1] == AMREGISTER))
				{
				/* Move argument to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Add source to target. */
				Opcode = wrdOpcode.u.Int = vmregAddNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source is not a register. */
			/* Note: (vmiadd regA memX regB)                    */
			if ((modifier[0] == AMREGISTER) && (modifier[1] != modifier[2]))
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Add argument to target. */
				Opcode = wrdOpcode.u.Int = vmregAddNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Number and Number. */
		/* Note: (vmadd Integer:regA Number:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move argument to temporary (converting to Number). */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			if (opnd[1].Offset != opnd[2].Offset)
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);
				}

			/* Add temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregAddNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Integer and Number. */
		/* Note: (vmadd Number:regA Integer:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move source to temporary (converting to Number). */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Add argument temporary to temporary. */
			Opcode = wrdOpcode.u.Int = vmregAddNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Move temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregMoveNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Integer and Number. */
		/* Note: (vmadd Integer:regA Integer:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move source to target (converting to Number). */
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
			ExitOnError(*ret);

			/* Move argument to temporary (converting to Number). */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Add temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregAddNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	case VMDIVI:
	case VMIDIV:
	case vmnatDivInteger:
		/* Manage instructions where target is an Integer register and preferred types are all Integer. */
		if ((modifier[2] == AMREGISTER) && (preftype[0] == TYNUM) && (preftype[1] == TYNUM) && (preftype[2] == TYNUM)) 
			{
			/* Manage cases where the argument and target are the same. */
			/* Note: (vmidiv regA memX regA)                            */
			if ((modifier[1] != modifier[2]) && (modifier[0] == modifier[2]) && (opnd[0].Offset == opnd[2].Offset))
				{
				/* Move source to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Divide argument into temporary. */
				Opcode = wrdOpcode.u.Int = vmregDivInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Move temporary to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source and target are the same. */
			/* Note: (vmidiv memX regA regA)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == modifier[2]) && (opnd[1].Offset == opnd[2].Offset))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Divide temporary into target. */
				Opcode = wrdOpcode.u.Int = vmregDivInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the argument is not a register. */
			/* Note: (vmidiv memX regA regB)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == AMREGISTER))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Divide temporary into target. */
				Opcode = wrdOpcode.u.Int = vmregDivInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source is not a register. */
			/* Note: (vmidiv regA memX regB)                    */
			if ((modifier[0] == AMREGISTER) && (modifier[1] != modifier[2]))
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Divide argument into target. */
				Opcode = wrdOpcode.u.Int = vmregDivInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Integer and Integer. */
		/* Note: (vmdiv Number:regA Integer:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move argument to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			if (opnd[1].Offset != opnd[2].Offset)
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);
				}

			/* Divide temporary into target. */
			Opcode = wrdOpcode.u.Int = vmregDivInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Number and Integer. */
		/* Note: (vmdiv Integer:regA Number:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move source to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Divide argument temporary into temporary. */
			Opcode = wrdOpcode.u.Int = vmregDivInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Move temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregMoveInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Number and Integer. */
		/* Note: (vmdiv Number:regA Number:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move source to target (converting to Integer). */
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
			ExitOnError(*ret);

			/* Move argument to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Divide temporary into target. */
			Opcode = wrdOpcode.u.Int = vmregDivInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	case VMDIVN:
	case VMNDIV:
	case vmnatDivNumber:
		/* Manage instructions where target is a Number register and preferred types are all Number. */
		if ((modifier[2] == AMREGISTER) && (preftype[0] == TYREAL) && (preftype[1] == TYREAL) && (preftype[2] == TYREAL)) 
			{
			/* Manage cases where the argument and target are the same. */
			/* Note: (vmndiv regA memX regA)                            */
			if ((modifier[1] != modifier[2]) && (modifier[0] == modifier[2]) && (opnd[0].Offset == opnd[2].Offset))
				{
				/* Move source to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__RNx");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Divide argument into temporary. */
				Opcode = wrdOpcode.u.Int = vmregDivNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Move temporary to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source and target are the same. */
			/* Note: (vmndiv memX regA regA)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == modifier[2]) && (opnd[1].Offset == opnd[2].Offset))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__RNx");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Divide temporary into target. */
				Opcode = wrdOpcode.u.Int = vmregDivNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the argument is not a register. */
			/* Note: (vmndiv memX regA regB)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == AMREGISTER))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__RNx");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Divide temporary into target. */
				Opcode = wrdOpcode.u.Int = vmregDivNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source is not a register. */
			/* Note: (vmidiv regA memX regB)                    */
			if ((modifier[0] == AMREGISTER) && (modifier[1] != modifier[2]))
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Divide argument into target. */
				Opcode = wrdOpcode.u.Int = vmregDivNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Number and Number. */
		/* Note: (vmdiv Integer:regA Number:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move argument to temporary (converting to Number). */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			if (opnd[1].Offset != opnd[2].Offset)
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);
				}

			/* Divide temporary into target. */
			Opcode = wrdOpcode.u.Int = vmregDivNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Integer and Number. */
		/* Note: (vmdiv Number:regA Integer:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move source to temporary (converting to Number). */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Divide argument temporary into temporary. */
			Opcode = wrdOpcode.u.Int = vmregDivNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Move temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregMoveNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Integer and Number. */
		/* Note: (vmdiv Integer:regA Integer:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move source to target (converting to Number). */
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
			ExitOnError(*ret);

			/* Move argument to temporary (converting to Number). */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Divide temporary into target. */
			Opcode = wrdOpcode.u.Int = vmregDivNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	case VMMULI:
	case VMIMUL:
	case vmnatMulInteger:
		/* Manage instructions where target is an Integer register and preferred types are all Integer. */
		if ((modifier[2] == AMREGISTER) && (preftype[0] == TYNUM) && (preftype[1] == TYNUM) && (preftype[2] == TYNUM)) 
			{
			/* Manage cases where the argument and target are the same. */
			/* Note: (vmimul regA memX regA)                            */
			if ((modifier[1] != modifier[2]) && (modifier[0] == modifier[2]) && (opnd[0].Offset == opnd[2].Offset))
				{
				/* Move source to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Multiply temporary with target. */
				Opcode = wrdOpcode.u.Int = vmregMulInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source and target are the same. */
			/* Note: (vmimul memX regA regA)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == modifier[2]) && (opnd[1].Offset == opnd[2].Offset))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Multiply temporary with target. */
				Opcode = wrdOpcode.u.Int = vmregMulInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the argument is not a register. */
			/* Note: (vmimul memX regA regB)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == AMREGISTER))
				{
				/* Move argument to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Multiply source with target. */
				Opcode = wrdOpcode.u.Int = vmregMulInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source is not a register. */
			/* Note: (vmimul regA memX regB)                    */
			if ((modifier[0] == AMREGISTER) && (modifier[1] != modifier[2]))
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Multiply argument with target. */
				Opcode = wrdOpcode.u.Int = vmregAddInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Integer and Integer. */
		/* Note: (vmmul Number:regA Integer:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move argument to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			if (opnd[1].Offset != opnd[2].Offset)
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);
				}

			/* Multiply temporary with target. */
			Opcode = wrdOpcode.u.Int = vmregMulInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Number and Integer. */
		/* Note: (vmmul Integer:regA Number:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move source to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Divide argument temporary into temporary. */
			Opcode = wrdOpcode.u.Int = vmregMulInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Move temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregMoveInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Number and Integer. */
		/* Note: (vmmul Number:regA Number:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move source to target (converting to Integer). */
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
			ExitOnError(*ret);

			/* Move argument to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Multiply temporary with target. */
			Opcode = wrdOpcode.u.Int = vmregMulInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	case VMMULN:
	case VMNMUL:
	case vmnatMulNumber:
		/* Manage instructions where target is a Number register and preferred types are all Number. */
		if ((modifier[2] == AMREGISTER) && (preftype[0] == TYREAL) && (preftype[1] == TYREAL) && (preftype[2] == TYREAL)) 
			{
			/* Manage cases where the argument and target are the same. */
			/* Note: (vmnmul regA memX regA)                            */
			if ((modifier[1] != modifier[2]) && (modifier[0] == modifier[2]) && (opnd[0].Offset == opnd[2].Offset))
				{
				/* Move source to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__RNx");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Multiply temporary with target. */
				Opcode = wrdOpcode.u.Int = vmregMulNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source and target are the same. */
			/* Note: (vmnmul memX regA regA)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == modifier[2]) && (opnd[1].Offset == opnd[2].Offset))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__RNx");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Multiply temporary with target. */
				Opcode = wrdOpcode.u.Int = vmregMulNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the argument is not a register. */
			/* Note: (vmnmul memX regA regB)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == AMREGISTER))
				{
				/* Move argument to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Multiply source with target. */
				Opcode = wrdOpcode.u.Int = vmregMulNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source is not a register. */
			/* Note: (vmnmul regA memX regB)                    */
			if ((modifier[0] == AMREGISTER) && (modifier[1] != modifier[2]))
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Multiply argument with target. */
				Opcode = wrdOpcode.u.Int = vmregMulNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Number and Number. */
		/* Note: (vmmul Integer:regA Number:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move argument to temporary (converting to Number). */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			if (opnd[1].Offset != opnd[2].Offset)
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);
				}

			/* Multiply temporary with target. */
			Opcode = wrdOpcode.u.Int = vmregMulNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Integer and Number. */
		/* Note: (vmmul Number:regA Integer:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move source to temporary (converting to Number). */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Divide argument temporary into temporary. */
			Opcode = wrdOpcode.u.Int = vmregMulNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Move temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregMoveNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Integer and Number. */
		/* Note: (vmmul Integer:regA Integer:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move source to target (converting to Number). */
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
			ExitOnError(*ret);

			/* Move argument to temporary (converting to Number). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Multiply temporary with target. */
			Opcode = wrdOpcode.u.Int = vmregMulNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	case VMSUBI:
	case VMISUB:
	case vmnatSubInteger:
		/* Manage instructions where target is an Integer register and preferred types are all Integer. */
		if ((modifier[2] == AMREGISTER) && (preftype[0] == TYNUM) && (preftype[1] == TYNUM) && (preftype[2] == TYNUM)) 
			{
			/* Manage cases where the argument and target are the same. */
			/* Note: (vmisub regA memX regA)                            */
			if ((modifier[1] != modifier[2]) && (modifier[0] == modifier[2]) && (opnd[0].Offset == opnd[2].Offset))
				{
				/* Move source to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Subtract argument from temporary. */
				Opcode = wrdOpcode.u.Int = vmregSubInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Move temporary to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source and target are the same. */
			/* Note: (vmisub memX regA regA)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == modifier[2]) && (opnd[1].Offset == opnd[2].Offset))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Subtract temporary from target. */
				Opcode = wrdOpcode.u.Int = vmregSubInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the argument is not a register. */
			/* Note: (vmisub memX regA regB)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == AMREGISTER))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__Ri");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Subtract temporary from target. */
				Opcode = wrdOpcode.u.Int = vmregSubInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source is not a register. */
			/* Note: (vmisub regA memX regB)                    */
			if ((modifier[0] == AMREGISTER) && (modifier[1] != modifier[2]))
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Subtract argument from target. */
				Opcode = wrdOpcode.u.Int = vmregSubInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Integer and Integer. */
		/* Note: (vmsub Number:regA Integer:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move argument to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			if (opnd[1].Offset != opnd[2].Offset)
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveInteger;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);
				}

			/* Subtract temporary from target. */
			Opcode = wrdOpcode.u.Int = vmregSubInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Number and Integer. */
		/* Note: (vmsub Integer:regA Number:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move source to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Subtract argument temporary from temporary. */
			Opcode = wrdOpcode.u.Int = vmregSubInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Move temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregMoveInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Number and Integer. */
		/* Note: (vmsub Number:regA Number:regB Integer:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYNUM)) 
			{
			/* Move source to target (converting to Integer). */
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
			ExitOnError(*ret);

			/* Move argument to temporary (converting to Integer). */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Subtract temporary from target. */
			Opcode = wrdOpcode.u.Int = vmregSubInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	case VMSUBN:
	case VMNSUB:
	case vmnatSubNumber:
		/* Manage instructions where target is a Number register and preferred types are all Number. */
		if ((modifier[2] == AMREGISTER) && (preftype[0] == TYREAL) && (preftype[1] == TYREAL) && (preftype[2] == TYREAL)) 
			{
			/* Manage cases where the argument and target are the same. */
			/* Note: (vmnsub regA memX regA)                            */
			if ((modifier[1] != modifier[2]) && (modifier[0] == modifier[2]) && (opnd[0].Offset == opnd[2].Offset))
				{
				/* Move source to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__RNx");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Subtract argument from temporary. */
				Opcode = wrdOpcode.u.Int = vmregSubNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Move temporary to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source and target are the same. */
			/* Note: (vmnsub memX regA regA)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == modifier[2]) && (opnd[1].Offset == opnd[2].Offset))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__RNx");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Subtract temporary from target. */
				Opcode = wrdOpcode.u.Int = vmregSubNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the argument is not a register. */
			/* Note: (vmnsub memX regA regB)                          */
			if ((modifier[0] != modifier[2]) && (modifier[1] == AMREGISTER))
				{
				/* Move argument to temporary (so target is not destroyed). */
				*tmp = TSYMBOL("__RNx");
				temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
				temporary.Tag = TYNUM;
				temporary.u.Int = temporary.Offset;
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
				ExitOnError(*ret);

				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Subtract temporary from target. */
				Opcode = wrdOpcode.u.Int = vmregSubNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			else
			/* Manage cases where the source is not a register. */
			/* Note: (vmnsub regA memX regB)                    */
			if ((modifier[0] == AMREGISTER) && (modifier[1] != modifier[2]))
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregLoadNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				/* Subtract argument from target. */
				Opcode = wrdOpcode.u.Int = vmregSubNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[2],opndAMVOID);
				ExitOnError(*ret);

				gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
				goto FixJumps;
				}
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Number and Number. */
		/* Note: (vmsub Integer:regA Number:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move argument to temporary (converting to Number). */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			if (opnd[1].Offset != opnd[2].Offset)
				{
				/* Move source to target. */
				Opcode = wrdOpcode.u.Int = vmregMoveNumber;
				*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
				ExitOnError(*ret);
				}

			/* Subtract temporary from target. */
			Opcode = wrdOpcode.u.Int = vmregSubNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Number Integer and Number. */
		/* Note: (vmsub Number:regA Integer:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move source to temporary (converting to Number). */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Subtract argument temporary from temporary. */
			Opcode = wrdOpcode.u.Int = vmregSubNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Move temporary to target. */
			Opcode = wrdOpcode.u.Int = vmregMoveNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* Manage instructions where operands are all registers with types of: Integer Integer and Number. */
		/* Note: (vmsub Integer:regA Integer:regB Number:regC) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
		    (modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) && 
		    (modifier[2] == AMREGISTER) && (preftype[2] == TYREAL)) 
			{
			/* Move source to target (converting to Number). */
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],opnd[2],opndAMVOID);
			ExitOnError(*ret);

			/* Move argument to temporary (converting to Number). */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Subtract temporary from target. */
			Opcode = wrdOpcode.u.Int = vmregSubNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[2],opndAMVOID);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;
    
	/*	(vmjmplt source comparitor label) : if (source < comparitor) Ip = label; */
	case vmnatJmpLTInteger:
	case vmnatJmpLTNumber:
	case VMJMPLT:
		/* (vmjmplt regs(int:a) mem(int:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYNUM) && (cons[1] != TRUE) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpLTInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmplt mems(int:a) reg(int:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYNUM) && (cons[0] != TRUE) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpLTInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmplt regs(num:a) mem(num:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYREAL) && 
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpLTNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmplt mems(num:a) reg(num:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpLTNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	/*	(vmjmple source comparitor label) : if (source <= comparitor) Ip = label; */
	case vmnatJmpLEInteger:
	case vmnatJmpLENumber:
	case VMJMPLE:
		/* (vmjmple regs(int:a) mem(int:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYNUM) && (cons[1] != TRUE) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpLEInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmple mems(int:a) reg(int:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYNUM) && (cons[0] != TRUE) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpLEInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmple regs(num:a) mem(num:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYREAL) && 
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpLENumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmple mems(num:a) reg(num:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpLENumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	/*	(vmjmpne source comparitor label) : if (source != comparitor) Ip = label; */
	case vmnatJmpNEInteger:
	case vmnatJmpNENumber:
	case VMJMPNE:
		/* (vmjmpne regs(int:a) mem(int:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYNUM) && (cons[1] != TRUE) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpNEInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpne mems(int:a) reg(int:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYNUM) && (cons[0] != TRUE) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpNEInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpne regs(num:a) mem(num:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYREAL) && 
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpNENumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpne mems(num:a) reg(num:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpNENumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	/*	(vmjmpeq source comparitor label) : if (source == comparitor) Ip = label; */
	case vmnatJmpEQInteger:
	case vmnatJmpEQNumber:
	case VMJMPEQ:
		/* (vmjmpeq regs(int:a) mem(int:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYNUM) && (cons[1] != TRUE) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpEQInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpeq mems(int:a) reg(int:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYNUM) && (cons[0] != TRUE) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpEQInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpeq regs(num:a) mem(num:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYREAL) && 
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpEQNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpeq mems(num:a) reg(num:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpEQNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	/*	(vmjmpge source comparitor label) : if (source >= comparitor) Ip = label; */
	case vmnatJmpGEInteger:
	case vmnatJmpGENumber:
	case VMJMPGE:
		/* (vmjmpge regs(int:a) mem(int:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYNUM) && (cons[1] != TRUE) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpGEInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpge mems(int:a) reg(int:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYNUM) && (cons[0] != TRUE) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpGEInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpge regs(num:a) mem(num:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYREAL) && 
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpGENumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpge mems(num:a) reg(num:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpGENumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;

	/*	(vmjmpgt source comparitor label) : if (source > comparitor) Ip = label; */
	case vmnatJmpGTInteger:
	case vmnatJmpGTNumber:
	case VMJMPGT:
		/* (vmjmpgt regs(int:a) mem(int:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYNUM) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYNUM) && (cons[1] != TRUE) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpGTInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpgt mems(int:a) reg(int:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYNUM) && (cons[0] != TRUE) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYNUM) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__Ri");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpGTInteger;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpgt regs(num:a) mem(num:b) label) */
		if ((modifier[0] == AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] != AMREGISTER) && (preftype[1] == TYREAL) && 
			(modifier[2] == AMINTEGER)) 
			{
			/* Move comparitor to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[1],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare source with temporary. */
			Opcode = wrdOpcode.u.Int = vmregJmpGTNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		else
		/* (vmjmpgt mems(num:a) reg(num:b) label) */
		if ((modifier[0] != AMREGISTER) && (preftype[0] == TYREAL) && 
			(modifier[1] == AMREGISTER) && (preftype[1] == TYREAL) &&
			(modifier[2] == AMINTEGER)) 
			{
			/* Move source to temporary. */
			*tmp = TSYMBOL("__RNx");
			temporary = FCompile_LookUp(gCP,gTP,compilerState,tmp->u.Symbol);
			temporary.Tag = TYNUM;
			temporary.u.Int = temporary.Offset;
			Opcode = wrdOpcode.u.Int = vmregLoadNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],temporary,opndAMVOID);
			ExitOnError(*ret);

			/* Compare temporary with source. */
			Opcode = wrdOpcode.u.Int = vmregJmpGTNumber;
			*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,temporary,opnd[1],opnd[2]);
			ExitOnError(*ret);

			gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;
			goto FixJumps;
			}
		break;
	}

/* ****************************************************************/  
/*  Issue the original instruction as it was entered.             */
/*  Note:   No instruction optimization rules match the current   */
/*          instruction conditions. Issue the instruction as is.  */
/* ****************************************************************/  

IssueOriginalInstructionAsIs:

*ret = FVmCode_GeneratePcode(gCP,gTP,compilerState,wrdOpcode,opnd[0],opnd[1],opnd[2]);
ExitOnError(*ret);
gTP->FCompile_LastProcDisp = pcodeVector.u.PcodeVector->itsCurItemIndex;

/* ****************************************************************/  
/*  Add any jump instructions to the jump vector for later fixup. */
/*  Note:   The jump fixup vector allows forward and backup jump  */
/*          displacement fixup in the final compiler pass.        */
/* ****************************************************************/  

FixJumps:
switch(Opcode)
    {
    case VMJUMP:
    case VMJMPLT:
    case VMJMPLE:
    case VMJMPEQ:
    case VMJMPNE:
    case VMJMPGE:
    case VMJMPGT:
    case vmnatJmpLTInteger:
    case vmnatJmpLEInteger:
    case vmnatJmpEQInteger:
    case vmnatJmpNEInteger:
    case vmnatJmpGEInteger:
    case vmnatJmpGTInteger:
    case vmnatJmpLTNumber:
    case vmnatJmpLENumber:
    case vmnatJmpEQNumber:
    case vmnatJmpNENumber:
    case vmnatJmpGENumber:
    case vmnatJmpGTNumber:
	case vmregJmpEQInteger:		
	case vmregJmpLTInteger:
	case vmregJmpGTInteger:
	case vmregJmpNEInteger:
	case vmregJmpGEInteger:
	case vmregJmpLEInteger:
	case vmregJmpEQImmediate:		
	case vmregJmpLTImmediate:
	case vmregJmpGTImmediate:
	case vmregJmpNEImmediate:
	case vmregJmpGEImmediate:
	case vmregJmpLEImmediate:
	case vmregJmpEQNumber:		
	case vmregJmpLTNumber:
	case vmregJmpGTNumber:
	case vmregJmpNENumber:
	case vmregJmpGENumber:
	case vmregJmpLENumber:

        /*  Add the offset locations for the target of every jump instruction */
        /*  in the pcode vector under construction to the _JumpsP(compilerState) */
        /*  TIntVector */
        
        tmp->Tag = TYNUM;
        
        /*  unconditional jumps are stored as positive numbers and conditional jumps  */
        /*  are stored as negative numbers. */
        
        if (Opcode == VMJUMP)
            tmp->u.Int = _PcP(compilerState)->itsCurItemIndex - 1;
        else
            tmp->u.Int = -(_PcP(compilerState)->itsCurItemIndex - 1);
            
        TIntVector_AddNewValue(gCP,gTP,_Jumps(compilerState), *tmp);
    break;
    
    default:
    break;
    }

FrameExit(*ret);
}
