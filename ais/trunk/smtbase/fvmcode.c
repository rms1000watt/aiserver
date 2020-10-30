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

#define _C_FVmCode 
#define _SMARTBASE

#if 0
FVmCode.c

Implementation of the Virtual Machine Pcode Creation Procedures.

This source file contains the main pseudo code generation and test functions 
for the DRM Virtual Machine. An Lambda object contains a compiled VmScript formula. 
Adopted from Scheme and other dialects of Lisp, it is one of the main 
mechanisms for storing Lisp functions or scripts in AIS.

PARENT:             None. 

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include "fvmcode.h"
#include "fcompile.h"
#include "tpair.h"
#include "fmath1.h"
#include "fdebug.h"
TVAL	FVmCode_testCompiled(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#define _FVmCode_SetIVInt(v, n, i)    atHMInt(v->itsInstructionArray,n) = i
#define _FVmCode_Pointer(v, n)        &atHMInt(v->itsInstructionArray,n)

/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_Init

Initialize the virtual machine test bed for the VmScript function library.  

#endif

TVAL FVmCode_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ec);
EndFrame
 
if(gCP->FVmappend_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FVmappend_Initialized = TRUE;

/* Register the VmScript cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cMemoryInteger",(LpFUNC)&FVmCode_CMemoryInteger);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cPersistInteger",(LpFUNC)&FVmCode_CPersistInteger);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cGenericInteger",(LpFUNC)&FVmCode_CGenericInteger);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cMemoryReal",(LpFUNC)&FVmCode_CMemoryReal);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cPersistReal",(LpFUNC)&FVmCode_CPersistReal);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cGenericReal",(LpFUNC)&FVmCode_CGenericReal);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cString",(LpFUNC)&FVmCode_Testsc);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cCall",(LpFUNC)&FVmCode_Testcc);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"cLoop",(LpFUNC)&FVmCode_TestLoop);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"compiledTest",(LpFUNC)&FVmCode_testCompiled);
ExitOnError(*ec);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_GeneratePcode

The main virtual machine instruction pcode creation procedure.
        
#endif

TVAL FVmCode_GeneratePcode(LpXCONTEXT gCP,LpTHREAD gTP,TVector* compilerState,TVAL theOpcode,TVAL operand1,TVAL operand2,TVAL operand3)
{
UNUM                opcode;
OPCODE              ExtendedOpcode;
UNUM                modifier[3];
UNUM                offset[3];
UNUM                disp[3];
NUM                 modIndex;
NUM                 vecIndex;
LpCHAR				namePtr;
TVAL				argv[3];
TVAL				pcodeVector;
CHAR				bufIns[256];
StartFrame
DeclareOBJ(TPcodeVector,aVector);        
DeclareTVAL(newValue);
DeclareTVAL(newValue2);
DeclareTVAL(sts);
EndFrame       

/* ***********************************************************************/  
/*  Copy the incoming arguments so that any modifications will not cause */
/*  the original caller's arguments to change.                           */
/* ***********************************************************************/  

/*  Extract the Pcode Vector into a local copy. */
pcodeVector = TOBJ(_Pc(compilerState));
if (pcodeVector.Tag != TYPCODEVECTOR)
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
	{
    aVector = pcodeVector.u.PcodeVector;
	}

/*  Extract the opcode into a local copy. */
if (theOpcode.Tag == TYNUM)
	{
    opcode = theOpcode.u.Int;
	}
else
/*  Convert the opcode to an integer opcode (if necessary). */
if (theOpcode.Tag == TYSYMBOL) 
	{
	namePtr = SymbolArray(theOpcode);
    opcode = -1;

	if (strcmp(namePtr,"vmreturn") == 0) opcode = VMRETURN;
	else if (strcmp(namePtr,"vmadd") == 0) opcode = VMADD;	
	else if (strcmp(namePtr,"vmaddi") == 0) opcode = VMADDI;	
	else if (strcmp(namePtr,"vmaddu") == 0) opcode = VMADDU;	
	else if (strcmp(namePtr,"vmand") == 0) opcode = VMAND;	
	else if (strcmp(namePtr,"vmcall") == 0) opcode = VMCALL;	
	else if (strcmp(namePtr,"vmapply") == 0) opcode = VMAPPLY; 
	else if (strcmp(namePtr,"vmargfetch") == 0) opcode = VMARGFETCH; 
	else if (strcmp(namePtr,"vmaddn") == 0) opcode = VMADDN;	
	else if (strcmp(namePtr,"vmdivn") == 0) opcode = VMDIVN;	
	else if (strcmp(namePtr,"vmmovei") == 0) opcode = VMMOVEI;
	else if (strcmp(namePtr,"vmmoveu") == 0) opcode = VMMOVEU;
	else if (strcmp(namePtr,"vmmuln") == 0) opcode = VMMULN;	
	else if (strcmp(namePtr,"vmsubn") == 0) opcode = VMSUBN;	
	else if (strcmp(namePtr,"vmcadd") == 0) opcode = VMCADD;	
	else if (strcmp(namePtr,"vmcdiv") == 0) opcode = VMCDIV;	
	else if (strcmp(namePtr,"vmmoven") == 0) opcode = VMMOVEN;	
	else if (strcmp(namePtr,"vmcmul") == 0) opcode = VMCMUL;	
	else if (strcmp(namePtr,"vmcsub") == 0) opcode = VMCSUB;	
	else if (strcmp(namePtr,"vmcall") == 0) opcode = VMCALL;	
	else if (strcmp(namePtr,"vmdiv") == 0) opcode = VMDIV;	
	else if (strcmp(namePtr,"vmdivi") == 0) opcode = VMDIVI;	
	else if (strcmp(namePtr,"vmdivu") == 0) opcode = VMDIVU;	
	else if (strcmp(namePtr,"vmdivr") == 0) opcode = VMDIVR;	
	else if (strcmp(namePtr,"vmdivri") == 0) opcode = VMDIVRI;	
	else if (strcmp(namePtr,"vmdivru") == 0) opcode = VMDIVRU;	
	else if (strcmp(namePtr,"vmiadd") == 0) opcode = VMIADD;	
	else if (strcmp(namePtr,"vmidiv") == 0) opcode = VMIDIV;	
	else if (strcmp(namePtr,"vmidivr") == 0) opcode = VMIDIVR;	
	else if (strcmp(namePtr,"vmudivr") == 0) opcode = VMUDIVR;	
	else if (strcmp(namePtr,"vmimul") == 0) opcode = VMIMUL;	
	else if (strcmp(namePtr,"vmisub") == 0) opcode = VMISUB;	
	else if (strcmp(namePtr,"vmusub") == 0) opcode = VMUSUB;	
	else if (strcmp(namePtr,"vmnadd") == 0) opcode = VMNADD;	
	else if (strcmp(namePtr,"vmndiv") == 0) opcode = VMNDIV;	
	else if (strcmp(namePtr,"vmndivr") == 0) opcode = VMNDIVR;	
	else if (strcmp(namePtr,"vmnmul") == 0) opcode = VMNMUL;	
	else if (strcmp(namePtr,"vmnsub") == 0) opcode = VMNSUB;	
	else if (strcmp(namePtr,"vmjmplt") == 0) opcode = VMJMPLT;	
	else if (strcmp(namePtr,"vmjmple") == 0) opcode = VMJMPLE;	
	else if (strcmp(namePtr,"vmjmpeq") == 0) opcode = VMJMPEQ;	
	else if (strcmp(namePtr,"vmjmpne") == 0) opcode = VMJMPNE;	
	else if (strcmp(namePtr,"vmjmpge") == 0) opcode = VMJMPGE;	
	else if (strcmp(namePtr,"vmjmpgt") == 0) opcode = VMJMPGT;	
	else if (strcmp(namePtr,"vmjump") == 0) opcode = VMJUMP;	
	else if (strcmp(namePtr,"vmmove") == 0) opcode = VMMOVE;	
	else if (strcmp(namePtr,"vmmul") == 0) opcode = VMMUL;	
	else if (strcmp(namePtr,"vmmuli") == 0) opcode = VMMULI;	
	else if (strcmp(namePtr,"vmmulu") == 0) opcode = VMMULU;	
	else if (strcmp(namePtr,"vmiand") == 0) opcode = VMIAND;	
	else if (strcmp(namePtr,"vmiandb") == 0) opcode = VMIANDB;	
	else if (strcmp(namePtr,"vmdebugger") == 0) opcode = VMDEBUGGER; 
	else if (strcmp(namePtr,"vmior") == 0) opcode = VMIOR;	
	else if (strcmp(namePtr,"vmiorb") == 0) opcode = VMIORB;	
	else if (strcmp(namePtr,"vmixor") == 0) opcode = VMIXOR;	
	else if (strcmp(namePtr,"vmixorb") == 0) opcode = VMIXORB;	
	else if (strcmp(namePtr,"vmonerror") == 0) opcode = VMONERROR; 
	else if (strcmp(namePtr,"vmor") == 0) opcode = VMOR;		
	else if (strcmp(namePtr,"vmpop") == 0) opcode = VMPOP;	
	else if (strcmp(namePtr,"vmpush") == 0) opcode = VMPUSH;	
	else if (strcmp(namePtr,"vmref") == 0) opcode = VMREF;	
	else if (strcmp(namePtr,"vmself") == 0) opcode = VMSELF;	
	else if (strcmp(namePtr,"vmsend") == 0) opcode = VMSEND;	
	else if (strcmp(namePtr,"vmset") == 0) opcode = VMSET;	
	else if (strcmp(namePtr,"vmshl") == 0) opcode = VMSHL;	
	else if (strcmp(namePtr,"vmshr") == 0) opcode = VMSHR;	
	else if (strcmp(namePtr,"vmsub") == 0) opcode = VMSUB;	
	else if (strcmp(namePtr,"vmsubi") == 0) opcode = VMSUBI;	
	else if (strcmp(namePtr,"vmsubu") == 0) opcode = VMSUBU;	
	else if (strcmp(namePtr,"vmxor") == 0) opcode = VMXOR;	

	else if (strcmp(namePtr,"vmuadd") == 0) opcode = VMUADD;	
	else if (strcmp(namePtr,"vmudiv") == 0) opcode = VMUDIV;	
	else if (strcmp(namePtr,"vmudivr") == 0) opcode = VMUDIVR;	
	else if (strcmp(namePtr,"vmumul") == 0) opcode = VMUMUL;	
	else if (strcmp(namePtr,"vmusub") == 0) opcode = VMUSUB;	

	else if (strcmp(namePtr,"vmreftext") == 0) opcode = VMREFTEXT;		
	else if (strcmp(namePtr,"vmrefstring") == 0) opcode = VMREFSTRING;		
	else if (strcmp(namePtr,"vmsetstring") == 0) opcode = VMSETSTRING;		
	else if (strcmp(namePtr,"vmrefsymbol") == 0) opcode = VMREFSYMBOL;		
	else if (strcmp(namePtr,"vmrefvector") == 0) opcode = VMREFVECTOR;		
	else if (strcmp(namePtr,"vmsetvector") == 0) opcode = VMSETVECTOR;		
	else if (strcmp(namePtr,"vmrefstrvalue") == 0) opcode = VMREFSTRVALUE;	
	else if (strcmp(namePtr,"vmsetstrvalue") == 0) opcode = VMSETSTRVALUE;	
	else if (strcmp(namePtr,"vmrefstrkey") == 0) opcode = VMREFSTRKEY;		
	else if (strcmp(namePtr,"vmsetstrkey") == 0) opcode = VMSETSTRKEY;		
	else if (strcmp(namePtr,"vmrefdicvalue") == 0) opcode = VMREFDICVALUE;	
	else if (strcmp(namePtr,"vmsetdicvalue") == 0) opcode = VMSETDICVALUE;	
	else if (strcmp(namePtr,"vmrefdickey") == 0) opcode = VMREFDICKEY;		
	else if (strcmp(namePtr,"vmsetdickey") == 0) opcode = VMSETDICKEY;		
	else if (strcmp(namePtr,"vmrefdirvalue") == 0) opcode = VMREFDIRVALUE;	
	else if (strcmp(namePtr,"vmsetdirvalue") == 0) opcode = VMSETDIRVALUE;	
	else if (strcmp(namePtr,"vmrefdirkey") == 0) opcode = VMREFDIRKEY;		
	else if (strcmp(namePtr,"vmsetdirkey") == 0) opcode = VMSETDIRKEY;		
	else if (strcmp(namePtr,"vmrefbitvector") == 0) opcode = VMREFBITVECTOR;	
	else if (strcmp(namePtr,"vmsetbitvector") == 0) opcode = VMSETBITVECTOR;	
	else if (strcmp(namePtr,"vmrefbytevector") == 0) opcode = VMREFBYTVECTOR;	
	else if (strcmp(namePtr,"vmsetbytevector") == 0) opcode = VMSETBYTVECTOR;	
	else if (strcmp(namePtr,"vmrefpcdvector") == 0) opcode = VMREFPCDVECTOR;	
	else if (strcmp(namePtr,"vmsetpcdvector") == 0) opcode = VMSETPCDVECTOR;	
	else if (strcmp(namePtr,"vmrefobjvector") == 0) opcode = VMREFOBJVECTOR;	
	else if (strcmp(namePtr,"vmsetobjvector") == 0) opcode = VMSETOBJVECTOR;	
	else if (strcmp(namePtr,"vmrefintvector") == 0) opcode = VMREFINTVECTOR;	
	else if (strcmp(namePtr,"vmsetintvector") == 0) opcode = VMSETINTVECTOR;	
	else if (strcmp(namePtr,"vmrefnumvector") == 0) opcode = VMREFNUMVECTOR;	
	else if (strcmp(namePtr,"vmsetnumvector") == 0) opcode = VMSETNUMVECTOR;	
	else if (strcmp(namePtr,"vmreffltvector") == 0) opcode = VMREFFLTVECTOR;	
	else if (strcmp(namePtr,"vmsetfltvector") == 0) opcode = VMSETFLTVECTOR;	
	else if (strcmp(namePtr,"vmrefmatrix") == 0) opcode = VMREFMATRIX;		
	else if (strcmp(namePtr,"vmsetmatrix") == 0) opcode = VMSETMATRIX;		
	else if (strcmp(namePtr,"vmrefnummatrix") == 0) opcode = VMREFNUMMATRIX;	
	else if (strcmp(namePtr,"vmsetnummatrix") == 0) opcode = VMSETNUMMATRIX;	
	else if (strcmp(namePtr,"vmtestescape") == 0) opcode = VMTESTESCAPE;		

	else if (strcmp(namePtr,"vmnatJmpLTInteger") == 0) opcode = vmnatJmpLTInteger; 
	else if (strcmp(namePtr,"vmnatJmpLEInteger") == 0) opcode = vmnatJmpLEInteger; 
	else if (strcmp(namePtr,"vmnatJmpEQInteger") == 0) opcode = vmnatJmpEQInteger; 
	else if (strcmp(namePtr,"vmnatJmpNEInteger") == 0) opcode = vmnatJmpNEInteger; 
	else if (strcmp(namePtr,"vmnatJmpGEInteger") == 0) opcode = vmnatJmpGEInteger; 
	else if (strcmp(namePtr,"vmnatJmpGTInteger") == 0) opcode = vmnatJmpGTInteger; 
	else if (strcmp(namePtr,"vmnatJmpLTUInteger") == 0) opcode = vmnatJmpLTUInteger; 
	else if (strcmp(namePtr,"vmnatJmpLEUInteger") == 0) opcode = vmnatJmpLEUInteger; 
	else if (strcmp(namePtr,"vmnatJmpEQUInteger") == 0) opcode = vmnatJmpEQUInteger; 
	else if (strcmp(namePtr,"vmnatJmpNEUInteger") == 0) opcode = vmnatJmpNEUInteger; 
	else if (strcmp(namePtr,"vmnatJmpGEUInteger") == 0) opcode = vmnatJmpGEUInteger; 
	else if (strcmp(namePtr,"vmnatJmpGTUInteger") == 0) opcode = vmnatJmpGTUInteger; 
	else if (strcmp(namePtr,"vmnatJmpLTNumber") == 0) opcode = vmnatJmpLTNumber; 
	else if (strcmp(namePtr,"vmnatJmpLENumber") == 0) opcode = vmnatJmpLENumber; 
	else if (strcmp(namePtr,"vmnatJmpEQNumber") == 0) opcode = vmnatJmpEQNumber; 
	else if (strcmp(namePtr,"vmnatJmpNENumber") == 0) opcode = vmnatJmpNENumber; 
	else if (strcmp(namePtr,"vmnatJmpGENumber") == 0) opcode = vmnatJmpGENumber; 
	else if (strcmp(namePtr,"vmnatJmpGTNumber") == 0) opcode = vmnatJmpGTNumber; 
	else if (strcmp(namePtr,"vmnatAddInteger") == 0) opcode = vmnatAddInteger; 
	else if (strcmp(namePtr,"vmnatAddNumber") == 0) opcode = vmnatAddNumber; 
	else if (strcmp(namePtr,"vmnatDivInteger") == 0) opcode = vmnatDivInteger; 
	else if (strcmp(namePtr,"vmnatDivNumber") == 0) opcode = vmnatDivNumber; 
	else if (strcmp(namePtr,"vmnatDivrInteger") == 0) opcode = vmnatDivrInteger; 
	else if (strcmp(namePtr,"vmnatDivrNumber") == 0) opcode = vmnatDivrNumber; 
	else if (strcmp(namePtr,"vmnatMulInteger") == 0) opcode = vmnatMulInteger; 
	else if (strcmp(namePtr,"vmnatMulNumber") == 0) opcode = vmnatMulNumber; 
	else if (strcmp(namePtr,"vmnatSubInteger") == 0) opcode = vmnatSubInteger; 
	else if (strcmp(namePtr,"vmnatSubNumber") == 0) opcode = vmnatSubNumber; 
	else if (strcmp(namePtr,"vmnatLoadCharacter") == 0) opcode = vmnatLoadCharacter; 
	else if (strcmp(namePtr,"vmnatLoadFloat") == 0) opcode = vmnatLoadFloat; 
	else if (strcmp(namePtr,"vmnatLoadInteger") == 0) opcode = vmnatLoadInteger; 
	else if (strcmp(namePtr,"vmnatLoadUInteger") == 0) opcode = vmnatLoadUInteger; 
	else if (strcmp(namePtr,"vmnatLoadLong") == 0) opcode = vmnatLoadLong; 
	else if (strcmp(namePtr,"vmnatLoadNumber") == 0) opcode = vmnatLoadNumber; 
	else if (strcmp(namePtr,"vmnatLoadObject") == 0) opcode = vmnatLoadObject; 
	else if (strcmp(namePtr,"vmnatLoadShort") == 0) opcode = vmnatLoadShort; 
	else if (strcmp(namePtr,"vmnatSaveCharacter") == 0) opcode = vmnatSaveCharacter; 
	else if (strcmp(namePtr,"vmnatSaveFloat") == 0) opcode = vmnatSaveFloat; 
	else if (strcmp(namePtr,"vmnatSaveInteger") == 0) opcode = vmnatSaveInteger; 
	else if (strcmp(namePtr,"vmnatSaveLong") == 0) opcode = vmnatSaveLong; 
	else if (strcmp(namePtr,"vmnatSaveNumber") == 0) opcode = vmnatSaveNumber; 
	else if (strcmp(namePtr,"vmnatSaveObject") == 0) opcode = vmnatSaveObject; 
	else if (strcmp(namePtr,"vmnatSaveShort") == 0) opcode = vmnatSaveShort; 

	else if (strcmp(namePtr,"vmregAbsNumber") == 0) opcode = vmregAbsNumber;		
	else if (strcmp(namePtr,"vmregInteger") == 0) opcode = vmregInteger;		
	else if (strcmp(namePtr,"vmregNumber") == 0) opcode = vmregNumber;		
	else if (strcmp(namePtr,"vmregAddImmediate") == 0) opcode = vmregAddImmediate;	
	else if (strcmp(namePtr,"vmregAddInteger") == 0) opcode = vmregAddInteger;		
	else if (strcmp(namePtr,"vmregAddNumber") == 0) opcode = vmregAddNumber;		
	else if (strcmp(namePtr,"vmregAddPointer") == 0) opcode = vmregAddPointer;	
	else if (strcmp(namePtr,"vmregAndImmediate") == 0) opcode = vmregAndImmediate;	
	else if (strcmp(namePtr,"vmregAndInteger") == 0) opcode = vmregAndInteger;		
	else if (strcmp(namePtr,"vmregCosNumber") == 0) opcode = vmregCosNumber;		
	else if (strcmp(namePtr,"vmregDivImmediate") == 0) opcode = vmregDivImmediate;	
	else if (strcmp(namePtr,"vmregDivInteger") == 0) opcode = vmregDivInteger;		
	else if (strcmp(namePtr,"vmregDivNumber") == 0) opcode = vmregDivNumber;		
	else if (strcmp(namePtr,"vmregDivrImmediate") == 0) opcode = vmregDivrImmediate;	
	else if (strcmp(namePtr,"vmregDivrInteger") == 0) opcode = vmregDivrInteger;		
	else if (strcmp(namePtr,"vmregDivrNumber") == 0) opcode = vmregDivrNumber;		
	else if (strcmp(namePtr,"vmregIncPointer") == 0) opcode = vmregIncPointer;	
	else if (strcmp(namePtr,"vmregJmpEQImmediate") == 0) opcode = vmregJmpEQImmediate;	
	else if (strcmp(namePtr,"vmregJmpLTImmediate") == 0) opcode = vmregJmpLTImmediate;	
	else if (strcmp(namePtr,"vmregJmpGTImmediate") == 0) opcode = vmregJmpGTImmediate;	
	else if (strcmp(namePtr,"vmregJmpNEImmediate") == 0) opcode = vmregJmpNEImmediate;	
	else if (strcmp(namePtr,"vmregJmpGEImmediate") == 0) opcode = vmregJmpGEImmediate;	
	else if (strcmp(namePtr,"vmregJmpLEImmediate") == 0) opcode = vmregJmpLEImmediate;	
	else if (strcmp(namePtr,"vmregJmpEQUImmediate") == 0) opcode = vmregJmpEQUImmediate;	
	else if (strcmp(namePtr,"vmregJmpLTUImmediate") == 0) opcode = vmregJmpLTUImmediate;	
	else if (strcmp(namePtr,"vmregJmpGTUImmediate") == 0) opcode = vmregJmpGTUImmediate;	
	else if (strcmp(namePtr,"vmregJmpNEUImmediate") == 0) opcode = vmregJmpNEUImmediate;	
	else if (strcmp(namePtr,"vmregJmpGEUImmediate") == 0) opcode = vmregJmpGEUImmediate;	
	else if (strcmp(namePtr,"vmregJmpLEUImmediate") == 0) opcode = vmregJmpLEUImmediate;	
	else if (strcmp(namePtr,"vmregJmpEQInteger") == 0) opcode = vmregJmpEQInteger;	
	else if (strcmp(namePtr,"vmregJmpLTInteger") == 0) opcode = vmregJmpLTInteger;	
	else if (strcmp(namePtr,"vmregJmpGTInteger") == 0) opcode = vmregJmpGTInteger;	
	else if (strcmp(namePtr,"vmregJmpNEInteger") == 0) opcode = vmregJmpNEInteger;	
	else if (strcmp(namePtr,"vmregJmpGEInteger") == 0) opcode = vmregJmpGEInteger;	
	else if (strcmp(namePtr,"vmregJmpLEInteger") == 0) opcode = vmregJmpLEInteger;	
	else if (strcmp(namePtr,"vmregJmpEQUInteger") == 0) opcode = vmregJmpEQUInteger;	
	else if (strcmp(namePtr,"vmregJmpLTUInteger") == 0) opcode = vmregJmpLTUInteger;	
	else if (strcmp(namePtr,"vmregJmpGTUInteger") == 0) opcode = vmregJmpGTUInteger;	
	else if (strcmp(namePtr,"vmregJmpNEUInteger") == 0) opcode = vmregJmpNEUInteger;	
	else if (strcmp(namePtr,"vmregJmpGEUInteger") == 0) opcode = vmregJmpGEUInteger;	
	else if (strcmp(namePtr,"vmregJmpLEUInteger") == 0) opcode = vmregJmpLEUInteger;	
	else if (strcmp(namePtr,"vmregJmpEQNumber") == 0) opcode = vmregJmpEQNumber;		
	else if (strcmp(namePtr,"vmregJmpLTNumber") == 0) opcode = vmregJmpLTNumber;		
	else if (strcmp(namePtr,"vmregJmpGTNumber") == 0) opcode = vmregJmpGTNumber;		
	else if (strcmp(namePtr,"vmregJmpNENumber") == 0) opcode = vmregJmpNENumber;		
	else if (strcmp(namePtr,"vmregJmpGENumber") == 0) opcode = vmregJmpGENumber;		
	else if (strcmp(namePtr,"vmregJmpLENumber") == 0) opcode = vmregJmpLENumber;		
	else if (strcmp(namePtr,"vmregJump") == 0) opcode = vmregJump;			
	else if (strcmp(namePtr,"vmregLoadAddress") == 0) opcode = vmregLoadAddress;	
	else if (strcmp(namePtr,"vmregLoadInteger") == 0) opcode = vmregLoadInteger;		
	else if (strcmp(namePtr,"vmregLoadTail") == 0) opcode = vmregLoadTail;		
	else if (strcmp(namePtr,"vmregLoadDeclType") == 0) opcode = vmregLoadDeclType;		
	else if (strcmp(namePtr,"vmregLoadType") == 0) opcode = vmregLoadType;		
	else if (strcmp(namePtr,"vmregLoadJmpPointer") == 0) opcode = vmregLoadJmpPointer;	
	else if (strcmp(namePtr,"vmregLoadNumber") == 0) opcode = vmregLoadNumber;		
	else if (strcmp(namePtr,"vmregLogNumber") == 0) opcode = vmregLogNumber;		
	else if (strcmp(namePtr,"vmregMoveImmediate") == 0) opcode = vmregMoveImmediate;	
	else if (strcmp(namePtr,"vmregMoveInteger") == 0) opcode = vmregMoveInteger;		
	else if (strcmp(namePtr,"vmregMoveNumber") == 0) opcode = vmregMoveNumber;		
	else if (strcmp(namePtr,"vmregMulImmediate") == 0) opcode = vmregMulImmediate;	
	else if (strcmp(namePtr,"vmregMulInteger") == 0) opcode = vmregMulInteger;		
	else if (strcmp(namePtr,"vmregMulNumber") == 0) opcode = vmregMulNumber;		
	else if (strcmp(namePtr,"vmregObjLength") == 0) opcode = vmregObjLength;		
	else if (strcmp(namePtr,"vmregObjPointer") == 0) opcode = vmregObjPointer;		
	else if (strcmp(namePtr,"vmregOrImmediate") == 0) opcode = vmregOrImmediate;		
	else if (strcmp(namePtr,"vmregOrInteger") == 0) opcode = vmregOrInteger;		
	else if (strcmp(namePtr,"vmregPwrNumber") == 0) opcode = vmregPwrNumber;		
	else if (strcmp(namePtr,"vmregRefCharacter") == 0) opcode = vmregRefCharacter;	
	else if (strcmp(namePtr,"vmregRefFloat") == 0) opcode = vmregRefFloat;		
	else if (strcmp(namePtr,"vmregRefInteger") == 0)opcode = vmregRefInteger;		
	else if (strcmp(namePtr,"vmregRefNumber") == 0) opcode = vmregRefNumber;		
	else if (strcmp(namePtr,"vmregRefShort") == 0) opcode = vmregRefShort;		
	else if (strcmp(namePtr,"vmregRefLong") == 0) opcode = vmregRefLong;		
	else if (strcmp(namePtr,"vmregRefWord") == 0) opcode = vmregRefWord;
	else if (strcmp(namePtr,"vmregRefXCharacter") == 0) opcode = vmregRefXCharacter;	
	else if (strcmp(namePtr,"vmregRefXFloat") == 0) opcode = vmregRefXFloat;		
	else if (strcmp(namePtr,"vmregRefXInteger") == 0) opcode = vmregRefXInteger;		
	else if (strcmp(namePtr,"vmregRefXLong") == 0) opcode = vmregRefXLong;		
	else if (strcmp(namePtr,"vmregRefXNumber") == 0) opcode = vmregRefXNumber;		
	else if (strcmp(namePtr,"vmregRefXShort") == 0) opcode = vmregRefXShort;		
	else if (strcmp(namePtr,"vmregRefXWord") == 0) opcode = vmregRefXWord;	
	else if (strcmp(namePtr,"vmregRunInHardware") == 0) opcode = vmregRunInHardware;		
	else if (strcmp(namePtr,"vmregSaveInteger") == 0) opcode = vmregSaveInteger;		
	else if (strcmp(namePtr,"vmregSaveUInteger") == 0) opcode = vmregSaveUInteger;		
	else if (strcmp(namePtr,"vmregSaveTail") == 0) opcode = vmregSaveTail;		
	else if (strcmp(namePtr,"vmregSaveTailImmediate") == 0) opcode = vmregSaveTailImmediate;		
	else if (strcmp(namePtr,"vmregSaveDeclType") == 0) opcode = vmregSaveDeclType;		
	else if (strcmp(namePtr,"vmregSaveDeclTypeImmediate") == 0) opcode = vmregSaveDeclTypeImmediate;		
	else if (strcmp(namePtr,"vmregSaveNumber") == 0) opcode = vmregSaveNumber;		
	else if (strcmp(namePtr,"vmregSetCharacter") == 0) opcode = vmregSetCharacter;	
	else if (strcmp(namePtr,"vmregSetFloat") == 0) opcode = vmregSetFloat;		
	else if (strcmp(namePtr,"vmregSetInteger") == 0) opcode = vmregSetInteger;		
	else if (strcmp(namePtr,"vmregSetNumber") == 0) opcode = vmregSetNumber;		
	else if (strcmp(namePtr,"vmregSetShort") == 0) opcode = vmregSetShort;		
	else if (strcmp(namePtr,"vmregSetLong") == 0) opcode = vmregSetLong;		
	else if (strcmp(namePtr,"vmregSetWord") == 0) opcode = vmregSetWord;
	else if (strcmp(namePtr,"vmregSetCharImmediate") == 0) opcode = vmregSetCharImmediate;	
	else if (strcmp(namePtr,"vmregSetIntImmediate") == 0) opcode = vmregSetIntImmediate;	
	else if (strcmp(namePtr,"vmregSetLongImmediate") == 0) opcode = vmregSetLongImmediate;	
	else if (strcmp(namePtr,"vmregSetShortImmediate") == 0) opcode = vmregSetShortImmediate;	
	else if (strcmp(namePtr,"vmregSetXCharImmediate") == 0) opcode = vmregSetXCharImmediate;	
	else if (strcmp(namePtr,"vmregSetXIntImmediate") == 0) opcode = vmregSetXIntImmediate;	
	else if (strcmp(namePtr,"vmregSetXLongImmediate") == 0) opcode = vmregSetXLongImmediate;	
	else if (strcmp(namePtr,"vmregSetXShortImmediate") == 0) opcode = vmregSetXShortImmediate;	
	else if (strcmp(namePtr,"vmregSetXCharacter") == 0) opcode = vmregSetXCharacter;	
	else if (strcmp(namePtr,"vmregSetXFloat") == 0) opcode = vmregSetXFloat;		
	else if (strcmp(namePtr,"vmregSetXInteger") == 0) opcode = vmregSetXInteger;	
	else if (strcmp(namePtr,"vmregSetXLong") == 0) opcode = vmregSetXLong;		
	else if (strcmp(namePtr,"vmregSetXNumber") == 0) opcode = vmregSetXNumber;		
	else if (strcmp(namePtr,"vmregSetXShort") == 0) opcode = vmregSetXShort;		
	else if (strcmp(namePtr,"vmregSetXWord") == 0) opcode = vmregSetXWord;
	else if (strcmp(namePtr,"vmregShlImmediate") == 0) opcode = vmregShlImmediate;	
	else if (strcmp(namePtr,"vmregShlInteger") == 0) opcode = vmregShlInteger;		
	else if (strcmp(namePtr,"vmregShrImmediate") == 0) opcode = vmregShrImmediate;	
	else if (strcmp(namePtr,"vmregShrInteger") == 0) opcode = vmregShrInteger;		
	else if (strcmp(namePtr,"vmregSinNumber") == 0) opcode = vmregSinNumber;		
	else if (strcmp(namePtr,"vmregSqrtNumber") == 0) opcode = vmregSqrtNumber;		
	else if (strcmp(namePtr,"vmregStringCompare") == 0) opcode = vmregStringCompare;	
	else if (strcmp(namePtr,"vmregStringiCompare") == 0) opcode = vmregStringCompare;	
	else if (strcmp(namePtr,"vmregSubImmediate") == 0) opcode = vmregSubImmediate;		
	else if (strcmp(namePtr,"vmregSubInteger") == 0) opcode = vmregSubInteger;		
	else if (strcmp(namePtr,"vmregSubNumber") == 0) opcode = vmregSubNumber;		
	else if (strcmp(namePtr,"vmregSubPointer") == 0) opcode = vmregSubPointer;	
	else if (strcmp(namePtr,"vmregTanNumber") == 0) opcode = vmregTanNumber;		
	else if (strcmp(namePtr,"vmregXorImmediate") == 0) opcode = vmregXorImmediate;	
	else if (strcmp(namePtr,"vmregXorInteger") == 0) opcode = vmregXorInteger;		

	else if (strcmp(namePtr,"vmvecBinary") == 0) opcode = vmvecBinary;		
	else if (strcmp(namePtr,"vmvecInitialize") == 0) opcode = vmvecInitialize;		
	else if (strcmp(namePtr,"vmvecLoop") == 0) opcode = vmvecLoop;		
	else if (strcmp(namePtr,"vmvecNumScalar") == 0) opcode = vmvecNumScalar;		
	else if (strcmp(namePtr,"vmvecNumVector") == 0) opcode = vmvecNumVector;		
	else if (strcmp(namePtr,"vmvecPop") == 0) opcode = vmvecPop;		
	else if (strcmp(namePtr,"vmvecPopNumber") == 0) opcode = vmvecPopNumber;		
	else if (strcmp(namePtr,"vmvecPush") == 0) opcode = vmvecPush;		
	else if (strcmp(namePtr,"vmvecPushNumber") == 0) opcode = vmvecPushNumber;		
	else if (strcmp(namePtr,"vmvecSetIncrements") == 0) opcode = vmvecSetIncrements;		
	else if (strcmp(namePtr,"vmvecSetPointers") == 0) opcode = vmvecSetPointers;		
	else if (strcmp(namePtr,"vmvecSwapCC") == 0) opcode = vmvecSwapCC;		
	else if (strcmp(namePtr,"vmvecUnary") == 0) opcode = vmvecUnary;		
	}
else
	{
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
    
/*  Make sure the opcode is legal. */
if (opcode >= _VMMAXINSTRS)
    goto IllegalInstruction;

/*  Extract the three arguments modifiers. */
modifier[0] = operand1.Modifier;
modifier[1] = operand2.Modifier;
modifier[2] = operand3.Modifier;

/*  Extract the three arguments offsets. */
offset[0] = operand1.Offset;
offset[1] = operand2.Offset;
offset[2] = operand3.Offset;

/*  Extract the three arguments displacements. */
disp[0] = operand1.u.Int;
disp[1] = operand2.u.Int;
disp[2] = operand3.u.Int;

/*  Copy the three arguments themselves. */
argv[0] = operand1;
argv[1] = operand2;
argv[2] = operand3;

/*  Make sure we have enough room in the pcode vector. */
vecIndex = aVector->itsCurItemIndex;
if(vecIndex + 100 > aVector->itsMaxItemIndex)
    {
    *sts = TPcodeVector_SetMaxIndex(gCP, gTP, pcodeVector, vecIndex + 200);
    _TObject_ErrorChk(*sts);
    }

/*  Memory opcodes: Append to the Pcode Vector all remaining arguments. */
if (opcode < VMSTARTREGISTERINS)
	{
	/*  Extend the opcode with the modifiers and append the extended opcode to the Pcode Vector. */
	ExtendedOpcode.u.Pcode = opcode;
	ExtendedOpcode.u.Am1 = modifier[0];
	ExtendedOpcode.u.Am2 = modifier[1];
	ExtendedOpcode.u.Am3 = modifier[2];
	newValue2->Tag = TYNUM;
	newValue2->u.Int = 0;
	newValue->Tag = TYNUM;
	newValue->u.Int = ExtendedOpcode.Opcode;
	vecIndex = aVector->itsCurItemIndex++;
	_FVmCode_SetIVInt(aVector, vecIndex, newValue->u.Int);

	/*  Append each memory opcode argument to the Pcode Vector. */
	for (modIndex = 0; modIndex < 3; ++modIndex)
		{
		switch (modifier[modIndex])
			{
			case AMGVOFFSET:
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if ((argv[modIndex].Tag != TYSYMBOL) || (!_VALIDOBJ(argv[modIndex].u.Object)))
					{
					FrameExit(TERROR("!compile: illegal Gv variable reference!"));
					}
				argv[modIndex].Tag = TYNUM;
				_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[modIndex]);
				break;
            
			case AMAVOFFSET:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Av(compilerState) == NIL) || (argv[modIndex].Offset >= _Av(compilerState)->itsMaxItemIndex))
					{
					FrameExit(TERROR("!compile: illegal Av variable reference!"));
					}
				if (disp[modIndex] != (offset[modIndex] * AISWORDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * AISWORDARRAYITEMSIZE);
					}
 				_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[modIndex]);
				break;
            
			case AMTVOFFSET:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Tv(compilerState) == NIL) || (argv[modIndex].Offset >= _Tv(compilerState)->itsMaxItemIndex))
					{
					FrameExit(TERROR("!compile: illegal Tv variable reference!"));
					}
				if (disp[modIndex] != (offset[modIndex] * AISWORDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * AISWORDARRAYITEMSIZE);
					}
 				_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[modIndex]);
				break;
            
			case AMSVOFFSET:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Sv(compilerState) == NIL) || (argv[modIndex].Offset >= _Sv(compilerState)->itsMaxItemIndex) || (_CurProcP(compilerState)->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(_CurProcP(compilerState)->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self))
					{
					FrameExit(TERROR("!compile: illegal Sv reference or missing self argument!"));
					}
				if (disp[modIndex] != (offset[modIndex] * BINDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * BINDARRAYITEMSIZE);
					}
 				_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[modIndex]);
				break;
            
			case AMPVOFFSET:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Pv(compilerState) == NIL) || (argv[modIndex].Offset >= _Pv(compilerState)->itsMaxItemIndex))
					{
					FrameExit(TERROR("!compile: illegal Pv variable reference!"));
					}
				if (disp[modIndex] != (offset[modIndex] * BINDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * BINDARRAYITEMSIZE);
					}
 				_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[modIndex]);
				break;
            
			case AMCVOFFSET:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Cv(compilerState) == NIL) || (argv[modIndex].Offset >= _Cv(compilerState)->itsMaxItemIndex))
					{
					FrameExit(TERROR("!compile: illegal Cv variable reference!"));
					}
				if (disp[modIndex] != (offset[modIndex] * BINDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * BINDARRAYITEMSIZE);
					}
 				_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[modIndex]);
				break;
            
			case AMREGISTER:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMREGISTER))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Rv(compilerState) == NIL) || (argv[modIndex].Offset >= _Rv(compilerState)->itsMaxItemIndex))
					{
					FrameExit(TERROR("!compile: illegal Register variable reference!"));
					}
				if (disp[modIndex] != (offset[modIndex] * AISWORDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * AISWORDARRAYITEMSIZE);
					}
 				_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[modIndex]);
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
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
 				_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[modIndex]);
				break;

			case AMINTEGER:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMINTEGER))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
 				_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[modIndex]);
				break;
            
			case AMVOID:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMVOID))
					goto IllegalInstruction;
				break;
            
			default:
				goto IllegalInstruction;
				break;
			}
		}
	}
else
/*  Register opcodes: Append to the Pcode Vector all remaining arguments. */
if (opcode >= VMSTARTREGISTERINS)
	{
	/*  Adjust each re argument to be a direct byte displacement. */
	for (modIndex = 0; modIndex < 3; ++modIndex)
		{
		switch (modifier[modIndex])
			{
			case AMGVOFFSET:
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if ((argv[modIndex].Tag != TYSYMBOL) || (!_VALIDOBJ(argv[modIndex].u.Object)))
					{
					FrameExit(TERROR("!compile: illegal Gv variable reference!"));
					}
				argv[modIndex].Tag = TYNUM;
				break;
            
			case AMSVOFFSET:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Sv(compilerState) == NIL) || (argv[modIndex].Offset >= _Sv(compilerState)->itsMaxItemIndex) || (_CurProcP(compilerState)->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(_CurProcP(compilerState)->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self))
					{
					FrameExit(TERROR("!compile: illegal Sv reference or missing self argument!"));
					}
				if (disp[modIndex] != (offset[modIndex] * BINDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * BINDARRAYITEMSIZE);
					}
				break;
            
            
			case AMAVOFFSET:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Av(compilerState) == NIL) || (argv[modIndex].Offset >= _Av(compilerState)->itsMaxItemIndex))
					{
					FrameExit(TERROR("!compile: illegal Av variable reference!"));
					}
				if (disp[modIndex] != (offset[modIndex] * AISWORDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * AISWORDARRAYITEMSIZE);
					}
				break;
            
			case AMTVOFFSET:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Tv(compilerState) == NIL) || (argv[modIndex].Offset >= _Tv(compilerState)->itsMaxItemIndex))
					{
					FrameExit(TERROR("!compile: illegal Tv variable reference!"));
					}
				if (disp[modIndex] != (offset[modIndex] * AISWORDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * AISWORDARRAYITEMSIZE);
					}
				break;
                        
			case AMPVOFFSET:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Pv(compilerState) == NIL) || (argv[modIndex].Offset >= _Pv(compilerState)->itsMaxItemIndex))
					{
					FrameExit(TERROR("!compile: illegal Pv variable reference!"));
					}
				if (disp[modIndex] != (offset[modIndex] * BINDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * BINDARRAYITEMSIZE);
					}
				break;
            
			case AMCVOFFSET:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Cv(compilerState) == NIL) || (argv[modIndex].Offset >= _Cv(compilerState)->itsMaxItemIndex))
					{
					FrameExit(TERROR("!compile: illegal Cv variable reference!"));
					}
				if (disp[modIndex] != (offset[modIndex] * BINDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * BINDARRAYITEMSIZE);
					}
				break;
            
			case AMREGISTER:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMREGISTER))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				if ((_Rv(compilerState) == NIL) || (argv[modIndex].Offset >= _Rv(compilerState)->itsMaxItemIndex))
					{
					FrameExit(TERROR("!compile: illegal Register variable reference!"));
					}
				if (disp[modIndex] != (offset[modIndex] * AISWORDARRAYITEMSIZE))
					{
					argv[modIndex].u.Int = disp[modIndex] = (offset[modIndex] * AISWORDARRAYITEMSIZE);
					}
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
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMRGOFFSET))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				break;

			case AMINTEGER:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMINTEGER))
					goto IllegalInstruction;
				if (argv[modIndex].Tag != TYNUM ) 
					goto IllegalInstruction;
				break;
            
			case AMVOID:
				/*  Make sure the modifier is legal. */
				if (!(gCP->TPcodeVector_LegalModifiers[opcode][modIndex] & FAMVOID))
					goto IllegalInstruction;
				break;
            
			default:
				goto IllegalInstruction;
				break;
			}
		}

	switch (opcode)
		{
		/* Register opcodes with no arguments. */
		case vmvecLoop:
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = AMVOID;
			ExtendedOpcode.u.Am2 = AMVOID;
			ExtendedOpcode.u.Am3 = AMVOID;
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			break;

		/* Register opcodes with one register argument. */
		case vmregJump:
		case vmvecPopNumber:
		case vmvecPushNumber:
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = offset[0];
			ExtendedOpcode.u.Am2 = AMVOID;
			ExtendedOpcode.u.Am3 = AMVOID;
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			break;

		/* Register opcodes with two register arguments. */
		case vmregAbsNumber:
		case vmregInteger:
		case vmregNumber:
		case vmregAddInteger:
		case vmregAddNumber:
		case vmregAndInteger:
		case vmregCosNumber:
 		case vmregDivInteger:
		case vmregDivNumber:
		case vmregDivrInteger:
		case vmregDivrNumber:
		case vmregMoveInteger:
		case vmregMoveNumber:
		case vmregMulInteger:
		case vmregMulNumber:
		case vmregOrInteger:
		case vmregRefCharacter:
		case vmregRefFloat:
		case vmregRefInteger:
		case vmregRefNumber:
		case vmregRefShort:
		case vmregRefLong:
		case vmregSetCharacter:
		case vmregSetFloat:
		case vmregSetInteger:
		case vmregSetNumber:
		case vmregSetShort:
		case vmregSetLong:
		case vmregSinNumber:
		case vmregSqrtNumber:
		case vmregSubInteger:
		case vmregSubNumber:
		case vmregTanNumber:
		case vmregXorInteger:
		case vmregShlInteger:
		case vmregShrInteger:
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = offset[0];
			ExtendedOpcode.u.Am2 = offset[1];
			ExtendedOpcode.u.Am3 = AMVOID;
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			break;

		/* Register opcodes with three register arguments. */
		case vmregLogNumber:
		case vmregPwrNumber:
		case vmregRefXCharacter:
		case vmregRefXFloat:
		case vmregRefXInteger:
		case vmregRefXLong:
		case vmregRefXNumber:
		case vmregRefXShort:
		case vmregSetXCharacter:
		case vmregSetXFloat:
		case vmregSetXInteger:
		case vmregSetXLong:
		case vmregSetXNumber:
		case vmregSetXShort:
		case vmvecSetIncrements:
		case vmvecSetPointers:
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = offset[0];
			ExtendedOpcode.u.Am2 = offset[1];
			ExtendedOpcode.u.Am3 = offset[2];
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			break;

		/* Register jmpcc opcodes with two register arguments and a label displacement. */
		case vmregJmpEQInteger:
		case vmregJmpLTInteger:
		case vmregJmpGTInteger:
		case vmregJmpNEInteger:
		case vmregJmpGEInteger:
		case vmregJmpLEInteger:
		case vmregJmpEQUInteger:
		case vmregJmpLTUInteger:
		case vmregJmpGTUInteger:
		case vmregJmpNEUInteger:
		case vmregJmpGEUInteger:
		case vmregJmpLEUInteger:
		case vmregJmpEQNumber:
		case vmregJmpLTNumber:
		case vmregJmpGTNumber:
		case vmregJmpNENumber:
		case vmregJmpGENumber:
		case vmregJmpLENumber:
			if (modifier[2] != AMINTEGER) goto IllegalInstruction;
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = offset[0];
			ExtendedOpcode.u.Am2 = offset[1];
			ExtendedOpcode.u.Am3 = modifier[2];
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the jump label displacement argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[2]);
			break;

		/* Register jmpcc opcodes with one register argument one immediate argument and a label displacement. */
		case vmregJmpEQImmediate:
		case vmregJmpLTImmediate:
		case vmregJmpGTImmediate:
		case vmregJmpNEImmediate:
		case vmregJmpGEImmediate:
		case vmregJmpLEImmediate:
		case vmregJmpEQUImmediate:
		case vmregJmpLTUImmediate:
		case vmregJmpGTUImmediate:
		case vmregJmpNEUImmediate:
		case vmregJmpGEUImmediate:
		case vmregJmpLEUImmediate:
			if (modifier[1] != AMINTEGER) goto IllegalInstruction;
			if (modifier[2] != AMINTEGER) goto IllegalInstruction;
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = offset[0];
			ExtendedOpcode.u.Am2 = modifier[1];
			ExtendedOpcode.u.Am3 = modifier[2];
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the immediate argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[1]);
			/*  Append the jump label displacement argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[2]);
			break;

		/* Register opcodes with one immediate and one register argument. */
		case vmregAddImmediate:
		case vmregAndImmediate:
		case vmregDivImmediate:
		case vmregLoadJmpPointer:
		case vmregDivrImmediate:
		case vmregMoveImmediate:
		case vmregMulImmediate:
		case vmregOrImmediate:
		case vmregSetCharImmediate:
		case vmregSetIntImmediate:
		case vmregSetLongImmediate:
		case vmregSetShortImmediate:
		case vmregShlImmediate:
		case vmregShrImmediate:
		case vmregSubImmediate:
		case vmregXorImmediate:
		case vmvecNumVector:
		case vmvecInitialize:
			/*  Convert the register modifier to a register index. */
			if (modifier[0] != AMINTEGER) goto IllegalInstruction;
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = modifier[0];
			ExtendedOpcode.u.Am2 = offset[1];
			ExtendedOpcode.u.Am3 = AMVOID;
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the immediate argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[0]);
			break;

		/* Register opcodes with two immediates and one register argument. */
		case vmregIncPointer:
			/*  Convert the register modifier to a register index. */
			if (modifier[0] != AMINTEGER) goto IllegalInstruction;
			if (modifier[1] != AMINTEGER) goto IllegalInstruction;
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = modifier[0];
			ExtendedOpcode.u.Am2 = modifier[1];
			ExtendedOpcode.u.Am3 = offset[2];
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the immediate argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[0]);
			/*  Append the immediate argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[1]);
			break;

		/* Register opcodes with one immediate and two register arguments. */
		case vmregAddPointer:
		case vmregSubPointer:
		case vmregSetXCharImmediate:
		case vmregSetXIntImmediate:
		case vmregSetXLongImmediate:
		case vmregSetXShortImmediate:
		case vmvecNumScalar:
			/*  Convert the register modifier to a register index. */
			if (modifier[0] != AMINTEGER) goto IllegalInstruction;
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = modifier[0];
			ExtendedOpcode.u.Am2 = offset[1];
			ExtendedOpcode.u.Am3 = offset[2];
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the immediate argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[0]);
			break;

		/* Register opcodes with one memory and one register argument. */
		case vmregSetWord:
		case vmregLoadAddress:
		case vmregLoadInteger:
		case vmregLoadTail:
		case vmregLoadDeclType:
		case vmregLoadType:
		case vmregLoadNumber:
		case vmregObjLength:
		case vmregObjPointer:
			/*  Convert the register modifier to a register index. */
			if ((modifier[0] >= AMINTEGER) || (modifier[0] == AMREGISTER))
				goto IllegalInstruction;
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = modifier[0];
			ExtendedOpcode.u.Am2 = offset[1];
			ExtendedOpcode.u.Am3 = AMVOID;
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the memory displacement argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[0]);
			break; 

		/* Register opcodes with one immediate and one memory argument. */
		case vmregSaveTailImmediate:
		case vmregSaveDeclTypeImmediate:
			/*  Convert the register modifier to a register index. */
			if ((modifier[1] >= AMINTEGER) || (modifier[1] == AMREGISTER))
				goto IllegalInstruction;
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = modifier[0];
			ExtendedOpcode.u.Am2 = modifier[1];
			ExtendedOpcode.u.Am3 = AMVOID;
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the immediate argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[0]);
			/*  Append the memory displacement argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[1]);
			break;

		/* Register opcodes with two registers and one memory argument. */
		case vmregRefXWord:
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = offset[0];
			ExtendedOpcode.u.Am2 = offset[1];
			ExtendedOpcode.u.Am3 = modifier[2];
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the memory displacement argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[2]);
			break;
		/* Register opcodes with one memory and two registers arguments. */
		case vmregSetXWord:
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = modifier[0];
			ExtendedOpcode.u.Am2 = offset[1];
			ExtendedOpcode.u.Am3 = offset[2];
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the memory displacement argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[0]);
			break;


		/* Register opcodes with one register and one memory argument. */
		case vmregRefWord:
		case vmregSaveInteger:
		case vmregSaveUInteger:
		case vmregSaveTail:
		case vmregSaveDeclType:
		case vmregSaveNumber:
			/*  Convert the register modifier to a register index. */
			if ((modifier[1] >= AMINTEGER) || (modifier[1] == AMREGISTER))
				goto IllegalInstruction;
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = offset[0];
			ExtendedOpcode.u.Am2 = modifier[1];
			ExtendedOpcode.u.Am3 = AMVOID;
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the memory displacement argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[1]);
			break;

		/* Register opcodes with two memory and one register argument. */
		case vmregStringCompare:
		case vmregStringiCompare:
			/*  Convert the register modifier to a register index. */
			if ((modifier[0] >= AMINTEGER) || (modifier[0] == AMREGISTER))
				goto IllegalInstruction;
			if ((modifier[1] >= AMINTEGER) || (modifier[1] == AMREGISTER))
				goto IllegalInstruction;
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = modifier[0];
			ExtendedOpcode.u.Am2 = modifier[1];
			ExtendedOpcode.u.Am3 = offset[2];
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the memory displacement argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[0]);
			/*  Append the memory displacement argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[1]);
			break; 

		/* Register opcodes with two immediate arguments. */
		case vmvecPop:
		case vmvecPush:
			/*  Check for valid arguments. */
			if ((modifier[0] != AMINTEGER) || (modifier[1] != AMINTEGER) || (modifier[2] != AMVOID)) 
				goto IllegalInstruction;
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = modifier[0];
			ExtendedOpcode.u.Am2 = modifier[1];
			ExtendedOpcode.u.Am3 = AMVOID;
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the immediate argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[0]);
			/*  Append the immediate argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[1]);
			break;

		/* Register opcodes with one immediate argument. */
		case vmregRunInHardware:
		case vmvecBinary:
		case vmvecSwapCC:
		case vmvecUnary:
			/*  Check for valid arguments. */
			if ((modifier[0] != AMINTEGER) || (modifier[1] != AMVOID) || (modifier[2] != AMVOID)) 
				goto IllegalInstruction;
			/*  Extend the opcode with the modifiers. */
			ExtendedOpcode.u.Pcode = opcode;
			ExtendedOpcode.u.Am1 = modifier[0];
			ExtendedOpcode.u.Am2 = AMVOID;
			ExtendedOpcode.u.Am3 = AMVOID;
			/*  Append the extended opcode to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, ExtendedOpcode.Opcode);
			/*  Append the immediate argument to the Pcode Vector. */
			_FVmCode_SetIVInt(aVector, aVector->itsCurItemIndex++, disp[0]);
			break;

		default:
			goto IllegalInstruction;
			break;
		}

	}

FrameExit(argv[0]);

/*  Trap an illegal opcode / modifier combination here. */
IllegalInstruction:
if (opcode >= _VMMAXINSTRS)
	{
	sprintf(gTP->TempBuffer,"!codegen: compiler issued invalid opcode ["INTFORMAT"]!",opcode);
	FrameExit(TERROR(gTP->TempBuffer));
	}
else
	{
	FDebug_INSToString(gCP,gTP,opcode,(LpCHAR)bufIns);
	strcpy(gTP->TempBuffer,"!codegen: invalid argument for opcode ( ");
	strcat(gTP->TempBuffer,&bufIns[1]);
	strcat(gTP->TempBuffer,")!");
	FrameExit(TERROR(gTP->TempBuffer));
	}
FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_PCODE);
FrameExit(argv[0]);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_ArgLength

Given an argument modifier return the length (in INT`s) that the argument will take to store in a
constructed Pcode Vector.

#endif

TVAL    FVmCode_ArgLength(LpXCONTEXT gCP,LpTHREAD gTP, NUM modifier)
{
StartFrame
DeclareTVAL(ret);
EndFrame

asTag(ret) = TYNUM;

switch (modifier)
    {
    case AMVOID:
        asInt(ret) = 0;
    break;
        
    case AMGVOFFSET:
    case AMINTEGER:
    case AMAVOFFSET:
    case AMTVOFFSET:
    case AMPVOFFSET:
    case AMCVOFFSET:
    case AMREGISTER:
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
        asInt(ret) = 1;
    break;

    default:
        *ret = gCP->TObject_ERROR_INVALID_ARGLIST;
    break;
    }
    
FrameExit(*ret); 
}

/**************************************************************************************** */
/*******************************C Function Speed/Timing Tests**************************** */
/**************************************************************************************** */

/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_CMemoryInteger

Test the speed of a simple loop transfering data between two local register integer vectors.
        
#endif

TVAL FVmCode_CMemoryInteger(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM				m;
NUM				M;
NUM				n;
NUM				N;
NUM				x;
NUM				y;
StartFrame
DeclareTVAL(v1);
DeclareTVAL(v2);
DeclareTVAL(new);
DeclareTVAL(vector);
DeclareTVAL(integer);
EndFrame

/*
**  Get the looping upper limit.
*/
if (argc != 1) 
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (argv[0].Tag == TYREAL)
    N = argv[0].u.Real;
else
if (argv[0].Tag == TYNUM)
    N = argv[0].u.Int;
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Create two integer vectors of length 1000. */
*new = TGVALUE("new");
*vector = TSYMBOL("Vector");
*integer = TSYMBOL("integer");
M = 1000;
*v1 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,3,*vector,*integer,TINT(1000));
ExitOnError(*v1);
*v2 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,3,*vector,*integer,TINT(1000));
ExitOnError(*v2);
y = 10;
    
/*
**  Perform the loop.
*/
for (n = 0; n < N; ++n)
	{
	for (m = 0; m < M; ++m)
		{
		x = IntArray(*v1)[m];
		x = x + y;
		x = x * x;
		x = x / y;
		IntArray(*v2)[m] = y;
		}
	}
 
FrameExit(*v2);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_CPersistInteger

Test the speed of a simple loop transfering data between two persistent integer vectors.
        
#endif

TVAL FVmCode_CPersistInteger(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
register NUM            m;
register NUM            M;
register NUM            n;
register NUM            N;
register NUM            xIndex = 0;
register NUM            yIndex = 1;
StartFrame
DeclareTVAL(v1);
DeclareTVAL(v2);
DeclareTVAL(Pv);
DeclareTVAL(new);
DeclareTVAL(vector);
DeclareTVAL(integer);
DeclareTVAL(structure);
DeclareTVAL(xName);
DeclareTVAL(yName);
DeclareTVAL(zName);
EndFrame

/*
**  Get the looping upper limit.
*/
if (argc != 1) 
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (argv[0].Tag == TYREAL)
    N = argv[0].u.Real;
else
if (argv[0].Tag == TYNUM)
    N = argv[0].u.Int;
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Create two integer vectors of length 1000. */
*new = TGVALUE("new");
*vector = TSYMBOL("Vector");
*integer = TSYMBOL("integer");
*structure = TSYMBOL("Structure");
*xName = TSYMBOL("x");
*yName = TSYMBOL("y");
*zName = TSYMBOL("z");
M = 1000;
*v1 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,3,*vector,*integer,TINT(1000));
ExitOnError(*v1);
*v2 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,3,*vector,*integer,TINT(1000));
ExitOnError(*v2);
*Pv = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,7,*structure,*xName,TINT(10),*yName,TINT(20),*zName,TINT(30));
ExitOnError(*Pv);
BindArray(*Pv)[yIndex].Value.u.Int = 10;
BindArray(*Pv)[yIndex].Value.Tag = TYNUM;
    
/*
**  Perform the loop.
*/
for (n = 0; n < N; ++n)
	{
	for (m = 0; m < M; ++m)
		{
		BindArray(*Pv)[xIndex].Value.u.Int = IntArray(*v1)[m];
		BindArray(*Pv)[xIndex].Value.Tag = TYNUM;
		BindArray(*Pv)[xIndex].Value.u.Int = BindArray(*Pv)[xIndex].Value.u.Int + BindArray(*Pv)[yIndex].Value.u.Int;
		BindArray(*Pv)[xIndex].Value.u.Int = BindArray(*Pv)[xIndex].Value.u.Int * BindArray(*Pv)[xIndex].Value.u.Int;
		BindArray(*Pv)[xIndex].Value.u.Int = BindArray(*Pv)[xIndex].Value.u.Int / BindArray(*Pv)[yIndex].Value.u.Int;
		IntArray(*v2)[m] = BindArray(*Pv)[yIndex].Value.u.Int;
		}
	}
 
FrameExit(*v2);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_CGenericInteger

Test the speed of a simple loop transfering data between two generic integer vectors.
        
#endif

TVAL FVmCode_CGenericInteger(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
register NUM            m;
register NUM            M;
register NUM            n;
register NUM            N;
register NUM            xIndex = 0;
register NUM            yIndex = 1;
		 TVAL			fnADD =	TFUNCTION(FMath1_Plus);							
		 TVAL			fnMUL =	TFUNCTION(FMath1_Multiply);							
		 TVAL			fnDIV =	TFUNCTION(FMath1_Divide);							
StartFrame
DeclareTVAL(v1);
DeclareTVAL(v2);
DeclareTVAL(Pv);
DeclareTVAL(new);
DeclareTVAL(vector);
DeclareTVAL(integer);
DeclareTVAL(structure);
DeclareTVAL(xName);
DeclareTVAL(yName);
DeclareTVAL(zName);
DeclareTVAL(ret);
EndFrame

/*
**  Get the looping upper limit.
*/
if (argc != 1) 
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (argv[0].Tag == TYREAL)
    N = argv[0].u.Real;
else
if (argv[0].Tag == TYNUM)
    N = argv[0].u.Int;
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Create two integer vectors of length 1000. */
*new = TGVALUE("new");
*vector = TSYMBOL("Vector");
*integer = TSYMBOL("integer");
*structure = TSYMBOL("Structure");
*xName = TSYMBOL("x");
*yName = TSYMBOL("y");
*zName = TSYMBOL("z");
M = 1000;
*v1 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,(NUM)3,*vector,*integer,TINT(1000));
ExitOnError(*v1);
*v2 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,3,*vector,*integer,TINT(1000));
ExitOnError(*v2);
*Pv = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,7,*structure,*xName,TINT(10),*yName,TINT(20),*zName,TINT(30));
ExitOnError(*Pv);
BindArray(*Pv)[yIndex].Value.u.Int = 10;
BindArray(*Pv)[yIndex].Value.Tag = TYNUM;
    
/*
**  Perform the loop.
*/
for (n = 0; n < N; ++n)
	{
	for (m = 0; m < M; ++m)
		{
		*ret = FSmartbase_Ref(gCP,gTP,2,*v1,TINT(m));
		ExitOnError(*ret);
		BindArray(*Pv)[xIndex].Value = *ret;
		BindArray(*Pv)[xIndex].Value = FSmartbase_Eval(gCP,gTP,fnADD,2,BindArray(*Pv)[xIndex].Value,BindArray(*Pv)[yIndex].Value);
		ExitOnError(BindArray(*Pv)[xIndex].Value);
		BindArray(*Pv)[xIndex].Value = FSmartbase_Eval(gCP,gTP,fnMUL,2,BindArray(*Pv)[xIndex].Value,BindArray(*Pv)[xIndex].Value);
		ExitOnError(BindArray(*Pv)[xIndex].Value);
		BindArray(*Pv)[xIndex].Value = FSmartbase_Eval(gCP,gTP,fnDIV,2,BindArray(*Pv)[xIndex].Value,BindArray(*Pv)[yIndex].Value);
		ExitOnError(BindArray(*Pv)[xIndex].Value);
		*ret = FSmartbase_Set(gCP,gTP,3,*v1,TINT(m),BindArray(*Pv)[xIndex].Value);
		ExitOnError(*ret);
		}
	}
 
FrameExit(*v2);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_CMemoryReal

Test the speed of a simple loop transfering data between to floating point vectors.
        
#endif

TVAL FVmCode_CMemoryReal(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                     m;
NUM                     M;
NUM                     n;
NUM                     N;
REAL                    x;
REAL                    y;
StartFrame
DeclareTVAL(v1);
DeclareTVAL(v2);
DeclareTVAL(new);
DeclareTVAL(vector);
DeclareTVAL(number);
EndFrame

/*
**  Get the looping upper limit.
*/
if (argc != 1) 
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (argv[0].Tag == TYREAL)
    N = argv[0].u.Real;
else
if (argv[0].Tag == TYNUM)
    N = argv[0].u.Int;
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Create two number vectors of length 1000. */
*new = TGVALUE("new");
*vector = TSYMBOL("Vector");
*number = TSYMBOL("number");
M = 1000;
*v1 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,3,*vector,*number,TINT(1000));
ExitOnError(*v1);
*v2 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,3,*vector,*number,TINT(1000));
ExitOnError(*v2);
y = 10.0;
    
/*
**  Perform the loop.
*/
for (n = 0; n < N; ++n)
	{
	for (m = 0; m < M; ++m)
		{
		x = RealArray(*v1)[m];
		x += y;
		x *= x;
		x /= 10.0;
		RealArray(*v2)[m] = y;
		}
	}
 
FrameExit(*v2);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_CPersistReal

Test the speed of a simple loop transfering data between two persistent real vectors.
        
#endif

TVAL FVmCode_CPersistReal(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
register NUM            m;
register NUM            M;
register NUM            n;
register NUM            N;
register NUM            xIndex = 0;
register NUM            yIndex = 1;
StartFrame
DeclareTVAL(v1);
DeclareTVAL(v2);
DeclareTVAL(Pv);
DeclareTVAL(new);
DeclareTVAL(number);
DeclareTVAL(vector);
DeclareTVAL(structure);
DeclareTVAL(xName);
DeclareTVAL(yName);
DeclareTVAL(zName);
EndFrame

/*
**  Get the looping upper limit.
*/
if (argc != 1) 
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (argv[0].Tag == TYREAL)
    N = argv[0].u.Real;
else
if (argv[0].Tag == TYNUM)
    N = argv[0].u.Int;
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Create two integer vectors of length 1000. */
*new = TGVALUE("new");
*number = TSYMBOL("number");
*vector = TSYMBOL("Vector");
*structure = TSYMBOL("Structure");
*xName = TSYMBOL("x");
*yName = TSYMBOL("y");
*zName = TSYMBOL("z");
M = 1000;
*v1 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,3,*vector,*number,TINT(1000));
ExitOnError(*v1);
*v2 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,3,*vector,*number,TINT(1000));
ExitOnError(*v2);
*Pv = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,7,*structure,*xName,TREAL(10),*yName,TREAL(20),*zName,TREAL(30));
ExitOnError(*Pv);
BindArray(*Pv)[yIndex].Value.u.Real = 10;
BindArray(*Pv)[yIndex].Value.Tag = TYREAL;
    
/*
**  Perform the loop.
*/
for (n = 0; n < N; ++n)
	{
	for (m = 0; m < M; ++m)
		{
		BindArray(*Pv)[xIndex].Value.u.Real = RealArray(*v1)[m];
		BindArray(*Pv)[xIndex].Value.Tag = TYREAL;
		BindArray(*Pv)[xIndex].Value.u.Real = BindArray(*Pv)[xIndex].Value.u.Real + BindArray(*Pv)[yIndex].Value.u.Real;
		BindArray(*Pv)[xIndex].Value.u.Real = BindArray(*Pv)[xIndex].Value.u.Real * BindArray(*Pv)[xIndex].Value.u.Real;
		BindArray(*Pv)[xIndex].Value.u.Real = BindArray(*Pv)[xIndex].Value.u.Real / BindArray(*Pv)[yIndex].Value.u.Real;
		RealArray(*v2)[m] = BindArray(*Pv)[yIndex].Value.u.Real;
		}
	}
 
FrameExit(*v2);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_CGenericReal

Test the speed of a simple loop transfering data between two generic real vectors.
        
#endif

TVAL FVmCode_CGenericReal(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
register NUM            m;
register NUM            M;
register NUM            n;
register NUM            N;
register NUM            xIndex = 0;
register NUM            yIndex = 1;
		 TVAL			fnADD =	TFUNCTION(FMath1_Plus);							
		 TVAL			fnMUL =	TFUNCTION(FMath1_Multiply);							
		 TVAL			fnDIV =	TFUNCTION(FMath1_Divide);							
StartFrame
DeclareTVAL(v1);
DeclareTVAL(v2);
DeclareTVAL(Pv);
DeclareTVAL(new);
DeclareTVAL(vector);
DeclareTVAL(number);
DeclareTVAL(structure);
DeclareTVAL(xName);
DeclareTVAL(yName);
DeclareTVAL(zName);
DeclareTVAL(ret);
EndFrame

/*
**  Get the looping upper limit.
*/
if (argc != 1) 
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (argv[0].Tag == TYREAL)
    N = argv[0].u.Real;
else
if (argv[0].Tag == TYNUM)
    N = argv[0].u.Int;
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);

/* Create two integer vectors of length 1000. */
*new = TGVALUE("new");
*number = TSYMBOL("number");
*vector = TSYMBOL("Vector");
*structure = TSYMBOL("Structure");
*xName = TSYMBOL("x");
*yName = TSYMBOL("y");
*zName = TSYMBOL("z");
M = 1000;
*v1 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,(NUM)3,*vector,*number,TINT(1000));
ExitOnError(*v1);
*v2 = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,3,*vector,*number,TINT(1000));
ExitOnError(*v2);
*Pv = gCP->FSmartbase_Eval((char*)gCP,gTP,*new,7,*structure,*xName,TREAL(10),*yName,TREAL(20),*zName,TREAL(30));
ExitOnError(*Pv);
BindArray(*Pv)[yIndex].Value.u.Real = 10;
BindArray(*Pv)[yIndex].Value.Tag = TYREAL;
    
/*
**  Perform the loop.
*/
for (n = 0; n < N; ++n)
	{
	for (m = 0; m < M; ++m)
		{
		*ret = FSmartbase_Ref(gCP,gTP,2,*v1,TINT(m));
		ExitOnError(*ret);
		BindArray(*Pv)[xIndex].Value = *ret;
		BindArray(*Pv)[xIndex].Value = FSmartbase_Eval(gCP,gTP,fnADD,2,BindArray(*Pv)[xIndex].Value,BindArray(*Pv)[yIndex].Value);
		ExitOnError(BindArray(*Pv)[xIndex].Value);
		BindArray(*Pv)[xIndex].Value = FSmartbase_Eval(gCP,gTP,fnMUL,2,BindArray(*Pv)[xIndex].Value,BindArray(*Pv)[xIndex].Value);
		ExitOnError(BindArray(*Pv)[xIndex].Value);
		BindArray(*Pv)[xIndex].Value = FSmartbase_Eval(gCP,gTP,fnDIV,2,BindArray(*Pv)[xIndex].Value,BindArray(*Pv)[yIndex].Value);
		ExitOnError(BindArray(*Pv)[xIndex].Value);
		*ret = FSmartbase_Set(gCP,gTP,3,*v1,TINT(m),BindArray(*Pv)[xIndex].Value);
		ExitOnError(*ret);
		}
	}
 
FrameExit(*v2);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_Testsc

Test the speed of a simple string move loop such as:

        for (i = 0;i <= n; ++i) strcpy(buf,"Hello there you all");
        
#endif

TVAL FVmCode_Testsc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
NUM                     n;
NUM                     N;
NUM						k;
LpCHAR					lp;
LpCHAR					rp;
DeclareTVAL(left);
DeclareTVAL(right);
EndFrame

/*
**  Get the looping upper limit.
*/
if (argc != 1) 
    return(gCP->TObject_ERROR_INVALID_ARGLIST);
else
if (argv[0].Tag == TYREAL)
    N = argv[0].u.Real;
else
if (argv[0].Tag == TYNUM)
    N = argv[0].u.Int;
else
    return(gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*
**  Perform the string move loop.
*/
*left = TSTRING("Hello there you all");
*right = TSTRING("Hello buddy old friend");
for (n = 0; n <= N; ++n)
	{
	*left = TSTRING("Hello there you all");
	*right = TSTRING("Hello buddy old friend");

	/* Point to the left string */
	if (left->Tag == TYTEXT) lp = left->u.Text;
	else
	if (left->Tag == TYSTRING) lp = CharArray(*left);
	else
	if (left->Tag == TYSYMBOL) lp = SymbolArray(*left);
	else
	if (left->Tag == TYQUOTEDSYMBOL) lp = SymbolArray(*left);
	else
	if (left->Tag == TYBYTEVECTOR) lp = ByteArray(*left);
	else FrameExit(TERROR("!testsc: invalid left string object!"));

	/* Point to the right string */
	if (right->Tag == TYTEXT) rp = right->u.Text;
	else
	if (right->Tag == TYSTRING) rp = CharArray(*right);
	else
	if (right->Tag == TYSYMBOL) rp = SymbolArray(*right);
	else
	if (right->Tag == TYQUOTEDSYMBOL) rp = SymbolArray(*right);
	else
	if (right->Tag == TYBYTEVECTOR) rp = ByteArray(*right);
	else FrameExit(TERROR("!testsc: invalid right string object!"));

    k = strcmp(lp,rp);
	}
 
FrameExit(argv[0]);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_Testcc

Test the speed of a simple function call loop such as:

        for (i = 0;i <= n; ++i) FMath1_Plus(gCP,gTP,1,&tmpArgs[0]);
        
#endif

TVAL FVmCode_Testcc(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                     i;
NUM                     n;
StartFrame
DeclareTVAL(result);
DeclareTVALArray(tmpArgs,10);

EndFrame

/*
**  Get the looping upper limit.
*/
if (argc != 1)
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (argv[0].Tag == TYREAL)
    n = argv[0].u.Real;
else
if (argv[0].Tag == TYNUM)
    n = argv[0].u.Int;
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*
**  Perform the string move loop.
*/
for (i = 0; i <= n; ++i)
    {
    tmpArgs[0].Tag = TYREAL;
    tmpArgs[0].u.Real = 0;
    tmpArgs[1].Tag = TYREAL;
    tmpArgs[1].u.Real = 1;
    *result = FMath1_Plus(gCP,gTP,1,&tmpArgs[0]);
    }
 
FrameExit(argv[0]);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_TestLoop

Test the speed of a simple function call loop such as:

        for (i = 0;i <= n; ++i) FMath1_Plus(gCP,gTP,1,&tmpArgs[0]);
        
#endif

TVAL FVmCode_TestLoop(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                     i;
NUM                     n;
CHAR                    buf[100];
REAL                    r;
StartFrame
DeclareTVAL(result);
DeclareTVALArray(tmpArgs,10);

EndFrame

/*
**  Get the looping upper limit.
*/
tmpArgs[9].u.Int = 0;
r = 1.1;
if (argc != 1)
	{ 
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (argv[0].Tag == TYREAL)
    n = argv[0].u.Real;
else
if (argv[0].Tag == TYNUM)
    n = argv[0].u.Int;
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*
**  Perform the string move loop.
*/
tmpArgs[9].Tag = TYNUM;
tmpArgs[9].u.Int = 0;
tmpArgs[8].Tag = TYREAL;
tmpArgs[8].u.Real = 0;
for (i = 0; i <= n; ++i)
    {
    tmpArgs[9].u.Int += 1;
    tmpArgs[8].u.Real += 1;
    tmpArgs[0].Tag = TYREAL;
    tmpArgs[0].u.Real = 0;
    tmpArgs[1].Tag = TYREAL;
    tmpArgs[1].u.Real = 1;
    *result = FMath1_Plus(gCP,gTP,1,&tmpArgs[0]);
    strcpy((char*)buf,"Hello there you all");
    }
 
FrameExit(argv[0]);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FVmCode_testCompiled

Test the speed of a simple integer loop such as:

        for (i = 0;i < n; ++i) x += 1;
        
#endif

TVAL FVmCode_testCompiled(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
StartFrame
DeclareTVAL(ix);
DeclareTVAL(m);
DeclareTVAL(one);
DeclareTVAL(n);
EndFrame

/*
**  Get the looping upper limit.
*/
n->Tag = TYNUM;
if (argc != 1) 
	{
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}
else
if (argv[0].Tag == TYREAL)
    n->u.Int = argv[0].u.Real;
else
if (argv[0].Tag == TYNUM)
    n->u.Int = argv[0].u.Int;
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*
**  Perform the loop.
*/
ix->Tag = TYNUM;
ix->u.Int = 0;
m->Tag = TYNUM;
m->u.Int = 0;
one->Tag = TYNUM;
one->u.Int = 1;
#if 0
while (ix->u.Int < n->u.Int) 
    {
	addi(gCP,gTP,ix,one,ix); 
	addi(gCP,gTP,m,one,m); 
	}
#else
while (ix->u.Int < n->u.Int) 
    {
	ix->u.Int = ix->u.Int + one->u.Int;
	ix->Tag = TYNUM;
	m->u.Int = m->u.Int + one->u.Int;
	m->Tag = TYNUM;
	}
#endif
 
FrameExit(*n);
}
