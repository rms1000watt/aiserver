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

#ifdef _JIT
#ifdef _M32

#define _SMARTBASE
#if 0
FVmIntelP3Jit.c

Implementation of the Virtual Machine Procedure Intel P3 JIT.

This source file contains the main evaluation functions for the Intel P3 
Just-In-Time-Compiler (JIT). This JIT is designed to work hand-in-glove
with the FVmScript_Eval DRM virtual machine emulator.

PARENT:             None. 

AUTHORS:            Michael F. Korns

#endif

#include    "fvmintelp3jit.h"
#include    "fvmscpt.h"
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


#ifdef _MSVC
static	NUM		Jit_UseMeToKeepTheCOptimizerFromDeletingCode = 0;	
extern  NUM*	Jit_LoadJumpTable();
extern  NUM		Jit_RelativeAddress(NUM* Ip);
extern  NUM		Jit_EntryOpcode();
extern  NUM*	Jit_EntryIp();
extern  NUM		Jit_Jump(NUM* Ip);
#endif


/*--------------------------------------------------------------------------------------- */
#if 0
FVmIntelP3Jit_Eval

The main virtual machine just in time compiler for evaluating Lambdas, using the
DRM virtual machine.

Note:   This is the just in time (JIT) compiler for the Intel
		Pentium 4 cpu model.
        
#endif

/****************************************************************/
/* Start main macro definitions for the current host cpu model. */
/****************************************************************/
/* Set native execution mode on or off. */
#undef  JIT_NATIVE_EXECUTE
#define JIT_NATIVE_EXECUTE	1


/* Define the asm jump label entry for this host cpu model. */
#undef  jitSetLabel
#ifdef _GCC
#define jitSetLabel(lbl) asm("" #lbl ":");
#else
#define jitSetLabel(lbl) lbl:\
	Ip = (NUM*)Jit_LoadJumpTable();\
	if (Jit_UseMeToKeepTheCOptimizerFromDeletingCode != -1) goto RTN##lbl;
#endif

/* Define the load jump table entry for this host cpu model. */
#undef  loadJumpTableEntry
#ifdef _GCC
#define loadJumpTableEntry(lbl,opcode) \
asm( "lea " #lbl", %eax" );\
asm( "mov %%eax,%0" : "=m" (FVmIntelP4_JumpTable[opcode]));
#else
#define loadJumpTableEntry(lbl,opcode)\
    if (Jit_UseMeToKeepTheCOptimizerFromDeletingCode != -1) goto lbl;\
	RTN##lbl:\
    FVmIntelP4_JumpTable[opcode] = (LpFVOID)Ip;
#endif

/* Define the jump to opcode entry for this host cpu model. */
#undef  jumpToOpcodeEntry
#ifdef _GCC
#define jumpToOpcodeEntry(opcode) asm("jmp *%0" :: "m" (FVmIntelP4_JumpTable[opcode]));
#else
#define jumpToOpcodeEntry(opcode)\
	Jit_Jump((NUM*)FVmIntelP4_JumpTable[opcode]);
#endif

/* Define the save native jump back opcode in Native Code Vector. */
#undef	saveNativeJumpBack
#undef	saveNativeJumpBack
#define saveNativeJumpBack(opcode)\
    *Np++ = 0xBA; /* Save mov edx opcode */\
	*(LpNUM)Np = (NUM)opcode.Opcode;Np+=4;\
    *Np++ = 0xB8; /* Save mov eax returnAddr */\
	*(LpNUM)Np = (NUM)(Np+10);Np+=4;\
    *Np++ = 0xFF; /* Save jmp vminstr */\
	*Np++ = 0x25; /* Save jmp vminstr */\
	*(LpNUM)Np = (NUM)&FVmIntelP4_JumpTable[opcode.u.Pcode];Np+=4;

/* Define the JIT set Ip from offset for this host cpu model. */
#undef  setIpFromOffset
#if JIT_NATIVE_EXECUTE
#define setIpFromOffset(offset)\
	(LpNUM)(((LpCHAR)pcodes)+(offset))
#else
#define setIpFromOffset(offset)\
	&pcodes[(offset)]
#endif

/* Define the JIT offline entry for this host cpu model. */
#undef  jitOfflineEntry
#if JIT_NATIVE_EXECUTE
#ifdef _GCC
#define jitOfflineEntry\
    asm("mov %0, %%ebx" : "=m" (xsaveEBX)); \
    asm("mov %%edx, %[dst1]" : [dst1] "=m" (pcode));\
    asm("mov %%eax, %[dst2]" : [dst2] "=m" (Ip));
#else
#define jitOfflineEntry\
	Ip = Jit_EntryIp();\
	pcode.Opcode = Jit_EntryOpcode();
#endif
#else
#define jitOfflineEntry
#endif

/* Define the JIT offline exit for this host cpu model. */
#undef  jitOfflineExit
#if JIT_NATIVE_EXECUTE
#ifdef _GCC
#define jitOfflineExit\
	asm ("lea %0, %%eax" : : "m" (Ip));\
	asm ("jmp *(%eax)");
#else
#define jitOfflineExit\
	Jit_Jump(Ip);
#endif
#else
#define jitOfflineExit\
	goto Fetch;
#endif

/* Define the JIT fetch opcode for this host cpu model. */
#undef  jitFetchOpcode
#if JIT_NATIVE_EXECUTE
#ifdef _GCC
#define jitFetchOpcode\
        asm ("lea %0, %%eax" : : "m" (Ip));\
        asm ("jmp *(%eax)");
#else
#define jitFetchOpcode\
		Jit_Jump(Ip);
#endif
#else
#define jitFetchOpcode\
	pcode.Opcode = *(Ip++);
#endif

/* Define the JIT set up Instruction Pointer for this host cpu model. */
#undef  jitSetInstructionPtr
#if JIT_NATIVE_EXECUTE
#define jitSetInstructionPtr\
	Ip = (LpNUM)&atHMChar(Nc->itsByteArray,0);\
	pcodes = (LpNUM)&atHMChar(Nc->itsByteArray,0);
#else
#define jitSetInstructionPtr\
	Ip = &atHMInt(Pc->itsInstructionArray,0);\
	pcodes = &atHMInt(Pc->itsInstructionArray,0);
#endif

/* Define the compile out of stream macro for the JIT */
#undef JITREGMODEON
#define JITREGMODEON TRUE
#undef regOutStream
#define regOutStream(label,boolean)\
                    if (boolean) goto label;\
                    if ((NUM)(Np-ncodes) >= nativeLen) goto ResizeNativeCodeVector;
#undef outStream
#define outStream(label,boolean)\
					if (boolean) goto label;\
					if (autoHardwareMode == TRUE) goto ErrorRunInHardwareIllegalInstruction;\
					if (Vr[HARDWARESW] == TRUE) goto ErrorRunInHardwareIllegalInstruction;\
                    if ((NUM)(Np-ncodes) >= nativeLen) goto ResizeNativeCodeVector;

/* *************************************************************** */
/* Define the common code generation macros for the JIT translator */
/* *************************************************************** */

/********* Intel Integer register codes.  ***/
#define REAX	1
#define RECX	2
#define REBX	3
#define RESI	4
#define REDI	5
#define REDX	6

/********* iff eax is allocated to a DRM register.  ***/
/********* Save the eax into its memory reg loc     ***/
/* Code==> mov dword ptr Rp[Ir[1]],eax				***/ 
#define REGSAVEEAX		if (Ir[1] != 0){*Np++ = 0x89; *Np++ = REGRPMOD[1];*(LpNUM)Np = (RpRelAddress+(Ir[1]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff ecx is allocated to a DRM register.  ***/
/********* Save the ecx into its memory reg loc     ***/
/* Code==> mov dword ptr Rp[Ir[2]],ecx				***/ 
#define REGSAVEECX      if (Ir[2] != 0){*Np++ = 0x89; *Np++ = REGRPMOD[2];*(LpNUM)Np = (RpRelAddress+(Ir[2]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff ebx is allocated to a DRM register.  ***/
/********* Save the ebx into its memory reg loc     ***/
/* Code==> mov dword ptr Rp[Ir[3]],ebx				***/ 
#define REGSAVEEBX      if (Ir[3] != 0){*Np++ = 0x89; *Np++ = REGRPMOD[3];*(LpNUM)Np = (RpRelAddress+(Ir[3]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff esi is allocated to a DRM register.  ***/
/********* Save the esi into its memory reg loc     ***/
/* Code==> mov dword ptr Rp[Ir[4]],esi				***/ 
#define REGSAVEESI      if (Ir[4] != 0){*Np++ = 0x89; *Np++ = REGRPMOD[4];*(LpNUM)Np = (RpRelAddress+(Ir[4]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff edi is allocated to a DRM register.  ***/
/********* Save the edi into its memory reg loc     ***/
/* Code==> mov dword ptr Rp[Ir[5]],edi				***/ 
#define REGSAVEEDI      if (Ir[5] != 0){*Np++ = 0x89; *Np++ = REGRPMOD[5];*(LpNUM)Np = (RpRelAddress+(Ir[5]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff edx is allocated to a DRM register.  ***/
/********* Save the edx into its memory reg loc     ***/
/* Code==> mov dword ptr Rp[Ir[6]],edx				***/ 
#define REGSAVEEDX      if (Ir[6] != 0){*Np++ = 0x89; *Np++ = REGRPMOD[6];*(LpNUM)Np = (RpRelAddress+(Ir[6]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff RMOD is allocated to a DRM register. ***/
/********* Save the reg into its memory reg loc     ***/
/* Code==> mov dword ptr Rp[RMOD],RS				***/ 
#define REGSAVEMOD(RMOD)  if (Rp[RMOD].Offset > 0){*Np++ = 0x89; *Np++ = REGRPMOD[Rp[RMOD].Offset];*(LpNUM)Np = (RpRelAddress+(RMOD<<BITSIZEOFAISWORD));Np+=4;}

/********* iff eax is allocated to a DRM register.  ***/
/********* Restore the eax from its saved reg.      ***/
/* Code==> mov eax,dword ptr Rp[Ir[1]]				***/ 
#define REGLOADEAX      if (Ir[1] != 0){*Np++ = 0x8B; *Np++ = REGRPMOD[1];*(LpNUM)Np = (RpRelAddress+(Ir[1]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff ecx is allocated to a DRM register.  ***/
/********* Restore the ecx from its saved reg.      ***/
/* Code==> mov ecx,dword ptr Rp[Ir[2]]				***/ 
#define REGLOADECX      if (Ir[2] != 0){*Np++ = 0x8B; *Np++ = REGRPMOD[2];*(LpNUM)Np = (RpRelAddress+(Ir[2]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff ebx is allocated to a DRM register.  ***/
/********* Restore the ebx from its saved reg.      ***/
/* Code==> mov ebx,dword ptr Rp[Ir[3]]				***/ 
#define REGLOADEBX      if (Ir[3] != 0){*Np++ = 0x8B; *Np++ = REGRPMOD[3];*(LpNUM)Np = (RpRelAddress+(Ir[3]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff esi is allocated to a DRM register.  ***/
/********* Restore the esi from its saved reg.      ***/
/* Code==> mov esi,dword ptr Rp[Ir[4]]				***/ 
#define REGLOADESI      if (Ir[4] != 0){*Np++ = 0x8B; *Np++ = REGRPMOD[4];*(LpNUM)Np = (RpRelAddress+(Ir[4]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff edi is allocated to a DRM register.  ***/
/********* Restore the edi from its saved reg.      ***/
/* Code==> mov edi,dword ptr Rp[Ir[5]]				***/ 
#define REGLOADEDI      if (Ir[5] != 0){*Np++ = 0x8B; *Np++ = REGRPMOD[5];*(LpNUM)Np = (RpRelAddress+(Ir[5]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff edx is allocated to a DRM register.  ***/
/********* Restore the edx from its saved reg.      ***/
/* Code==> mov edx,dword ptr Rp[Ir[6]]				***/ 
#define REGLOADEDX      if (Ir[6] != 0){*Np++ = 0x8B; *Np++ = REGRPMOD[6];*(LpNUM)Np = (RpRelAddress+(Ir[6]<<BITSIZEOFAISWORD));Np+=4;}

/********* iff RMOD is allocated to a DRM register. ***/
/********* Restore the reg from its saved reg.      ***/
/* Code==> mov RS,dword ptr Rp[RMOD]				***/ 
#define REGLOADMOD(RMOD) if (Rp[RMOD].Offset > 0){*Np++ = 0x8B; *Np++ = REGRPMOD[Rp[RMOD].Offset];*(LpNUM)Np = (RpRelAddress+(RMOD<<BITSIZEOFAISWORD));Np+=4;}

/********* RT & RS are allocated to DRM Integer registers.  ***/
/********* Move the contents of RS into RT as an Integer.   ***/
/* Code==> mov  Reg[RT],Reg[RS]								***/
#define MOVREGREG(RT,RS)  if ((RT)!=(RS)){*Np++ = 0x8B; *Np++ = (char)(REGREGL[(RT)] + REGREGR[(RS)]);}

/********* MOV an Integer VREG into an Intel register. ***/
/*********    where VREG is the DRM Integer register   ***/
/*********    where IREG is the Intel Integer register ***/
/* Code==> mov IREG,dword ptr Rp[VREG]				   ***/ 
#define MOV_IREG_VREG(IREG,VREG) {*Np++ = 0x8B; *Np++ = REGRPMOD[IREG];*(LpNUM)Np = (RpRelAddress+(VREG<<BITSIZEOFAISWORD));Np+=4;}

/********* FLD a Number Memory Variable from an                  ***/
/*********      [Intel register indexed by a VREG displacement]  ***/
/*********    where IREG is the Intel address register           ***/
/*********    where VDSP is the DRM base register displacement   ***/
/* Code==> fld qword ptr [ebp-VDSP]				                 ***/ 
#define FLD_IREG_VDSP(IREG,VDSP) {*Np++ = 0xDD;*Np++ = FLDREG[IREG];*(LpNUM)Np = VDSP;Np+=4;}

/********* Load a DRM Integer or Number register.  ***/
#define LOADDRMREGISTER(reg)\
			if ((Vr[HARDWARESW] == TRUE) && (Rp[reg].Offset > 0))\
				{\
				if (Rp[reg].Tag != TYREAL)\
					{\
					/* mov Intel[reg],dword ptr Rp[reg]	*/\
					*Np++ = 0x8B;*Np++ = REGRPMOD[Rp[reg].Offset];*(LpNUM)Np = (RpRelAddress+(reg<<BITSIZEOFAISWORD));Np+=4;\
					}\
				else\
					{\
					/* fld qword ptr Rp[Nr[k]]			*/\
					*Np++ = 0xDD;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(reg<<BITSIZEOFAISWORD));Np+=4;\
					/* fstp st(i)						*/\
					*Np++ = 0xDD;*Np++ = (char)(0xD8 + Rp[reg].Offset);\
					}\
				}

/********* Load a DRM Offset register address.  ***/
#define LOADOFFSETREG(mode,modifier)\
			if (mode == AMREGISTER)\
				{\
				LOADDRMREGISTER((modifier >> BITSIZEOFAISWORD))\
				}\
			else\
			if (mode <= MAXRGOFFST)\
				{\
				LOADDRMREGISTER(mode)\
				}

/********* Load all DRM Integer & Number registers.  ***/
#define LOADALLREGS\
			if (Vr[HARDWARESW] == TRUE)\
				{\
				/* Load Integer hardware registers from allocated register variables. */\
				for (k = 1; k <= IREGMAX; ++k)\
					{\
					if (Ir[k] != 0)\
						{\
						/* mov reg,dword ptr Rp[Ir[k]]	*/ *Np++ = 0x8B;*Np++ = REGRPMOD[k];*(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD));Np+=4;\
						}\
					}\
				/* Load Number hardware registers from allocated register variables. */\
				for (k = NREGMAX; k >= 1; --k)\
					{\
					if (Nr[k] != 0)\
						{\
						/* fld qword ptr Rp[Nr[k]]		*/ *Np++ = 0xDD;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(Nr[k]<<BITSIZEOFAISWORD));Np+=4;\
						}\
					}\
				}

/********* Load all DRM Integer registers.  ***/
#define LOADINTREGS\
			if (Vr[HARDWARESW] == TRUE)\
				{\
				/* Load Integer hardware registers from allocated register variables. */\
				for (k = 1; k <= IREGMAX; ++k)\
					{\
					if (Ir[k] != 0)\
						{\
						/* mov reg,dword ptr Rp[Ir[k]]	*/ *Np++ = 0x8B;*Np++ = REGRPMOD[k];*(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD));Np+=4;\
						}\
					}\
				}

/********* Save a DRM Integer or Number register.  ***/
#define SAVEDRMREGISTER(reg)\
			if ((Vr[HARDWARESW] == TRUE) && (Rp[reg].Offset > 0))\
				{\
				if (Rp[reg].Tag != TYREAL)\
					{\
					/* mov	word ptr Rp[reg],Intel[reg] */\
					*Np++ = 0x89;*Np++ = REGRPMOD[Rp[reg].Offset];*(LpNUM)Np = (RpRelAddress+(reg<<BITSIZEOFAISWORD));Np+=4;\
					}\
				else\
					{\
					/* fld	st(reg)						*/\
					*Np++ = 0xD9; *Np++ = (char)(0xC0 - 1 + Rp[reg].Offset);\
					/* fstp	qword ptr Rp[reg]			*/\
					*Np++ = 0xDD;*Np++ = 0x9D;*(LpNUM)Np = (RpRelAddress+(reg<<BITSIZEOFAISWORD));Np+=4;\
					}\
				}

/********* Save a DRM Offset register address.  ***/
#define SAVEOFFSETREG(mode,modifier)\
			if (mode == AMREGISTER)\
				{\
				SAVEDRMREGISTER((modifier >> BITSIZEOFAISWORD))\
				}\
			else\
			if (mode <= MAXRGOFFST)\
				{\
				SAVEDRMREGISTER(mode)\
				}

/********* Save all DRM Integer & Number registers.  ***/
#define SAVEALLREGS\
			if (Vr[HARDWARESW] == TRUE)\
				{\
				/* Save Integer hardware registers back into allocated register variables. */\
				for (k = 1; k <= IREGMAX; ++k)\
					{\
					if (Ir[k] != 0)\
						{\
						/* mov dword ptr Rp[Ir[k]],reg	*/ *Np++ = 0x89;*Np++ = REGRPMOD[k];*(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD));Np+=4;\
						}\
					}\
				/* Save Number hardware registers back into allocated register variables. */\
				for (k = 1; k <= NREGMAX; ++k)\
					{\
					if (Nr[k] != 0)\
						{\
						/* fstp qword ptr Rp[Nr[k]]		*/ *Np++ = 0xDD;*Np++ = 0x9D;*(LpNUM)Np = (RpRelAddress+(Nr[k]<<BITSIZEOFAISWORD));Np+=4;\
						}\
					}\
				}

/********* Save all DRM Integer registers.  ***/
#define SAVEINTREGS\
			if (Vr[HARDWARESW] == TRUE)\
				{\
				/* Save Integer hardware registers back into allocated register variables. */\
				for (k = 1; k <= IREGMAX; ++k)\
					{\
					if (Ir[k] != 0)\
						{\
						/* mov dword ptr Rp[Ir[k]],reg	*/ *Np++ = 0x89;*Np++ = REGRPMOD[k];*(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD));Np+=4;\
						}\
					}\
				}

/* ******************************************************************* */
/* End Define the common code generation macros for the JIT translator */
/* ******************************************************************* */

/*** Define host P3 Integer Register Max Allocation Count.  ***/
/*** We allocate these registers:  eax ecx ebx esi edi		***/
/*** We scratchpad these registers: edx						***/
/*** Note: The register scratchpads are never allocated     ***/
/***       to a variable!                                   ***/
#undef IREGMAX
#define IREGMAX 5

/* Define host P3 Number Register Max Allocation Count.  */
/*** We allocate these registers: st0 st1 st2 st3 st4  ***/
#undef NREGMAX
#define NREGMAX 5

/*** Static modifier codes for P3 mov reg,dword ptr Rp[1] and mov reg,dword ptr Rp[1] ***/
/***                                 eax  ecx  ebx  esi  edi  edx				      ***/
static CHAR		REGRPMOD[7] = {0x00,0x85,0x8D,0x9D,0xB5,0xBD,0x95};

/*** Static modifier codes for P3 mov reg,dword ptr [edx+offset]                      ***/
/***                                  eax  ecx  ebx  esi  edi  edx				      ***/
static CHAR		REGEDXMOD[7] = {0x00,0x82,0x8A,0x9A,0xB2,0xBA,0x92};

/*** Static modifier codes for P3 mov reg1,reg2 == opbyte,(REGREGL[r1]+REGREGL[r2])   ***/
/***                                eax  ecx  ebx  esi  edi  edx				      ***/
//static CHAR		MREGEDX[7] = {0x00,0xC2,0xCA,0xDA,0xF2,0xFA,0xD2};
static CHAR		MREGIMM[7] = {0x00,0xB8,0xB9,0xBB,0xBE,0xBF,0xBA};
static CHAR		REGBEDX[7] = {0x00,0x02,0x0A,0x1A,0x32,0x3A,0x12};
static CHAR		MULREGI[7] = {0x00,0xC0,0xC9,0xDB,0xF6,0xFF,0xD2};
static CHAR		REGTARG[7] = {0x00,0x00,0x08,0x18,0x30,0x38,0x10};					

static CHAR		REGREGL[7] = {0x00,0xC0,0xC8,0xD8,0xF0,0xF8,0xD0};
static CHAR		REGREGR[7] = {0x00,0x00,0x01,0x03,0x06,0x07,0x02};

static CHAR		REGBASE[7] = {0x00,0x00,0x01,0x03,0x06,0x07,0x02};
static CHAR		REGINDX[7] = {0x00,0xC0,0xC8,0xD8,0xF0,0xF8,0xD0};					


/****************************************************************/
/* End main macro definitions for the current host cpu model.   */
/****************************************************************/

/****************************************************************/
/* X8087 Instruction Initialization.   */
/****************************************************************/
#ifdef _GCC
void JIT_Init()
{
	unsigned short aState;
	aState = 0x027F;
	asm( "fldcw %0" ::"m"(aState));
}
#endif

/****************************************************************/
/* Start main vm instruction flags for the current cpu model.   */
/****************************************************************/

static CHAR		vmInsFlags[_VMMAXINSTRS];

/****************************************************************/
/* End main vm instruction flags for the current cpu model.     */
/****************************************************************/

TVAL FVmIntelP3Jit_Eval(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* proc,NUM argc,TVAL argv[])
{
/*  VIRTUAL EMULATION INSTRUCTION JUMP TABLE */
typedef	VOID			(*LpFVOID)       ();
static	LpFVOID			FVmIntelP4_JumpTable[_VMMAXINSTRS] = {0};
static	NUM				RpRelAddress = 0;
static	NUM				VrRelAddress = 0;

static NUM                     TpRelAddress = 0;
static NUM                     ProcRelAddress = 0;
static NUM                     ArgcRelAddress = 0;
static NUM                     ArgvRelAddress = 0;

/*  VIRTUAL EMULATION MACHINE REGISTERS */
NUM*					Ip;                     /*  Instruction pointer */
TVAL*					target;                 /*  Target operand address */
TVAL*					source;                 /*  Source operand address */
TVAL*					argument;               /*  Argument operand address */
TVAL*					index;					/*  Index operand address */
NUM						overFlowReg;			/*  Temporary Overflow Register */

/*  VM LOCAL VARIABLE DECLARATIONS */
/*  Note:   These variables should be kept to an absolute necessary */
/*          minimum, as they eat up large (approc 132 bytes) of C   */
/*          system stack space with each Lambda recursion.           */
TPcodeVector*           Pc;                     /*  Pcode vector */
TStructure*				Sv;                     /*  Self variable structure */
TStructure*				Pv;                     /*  Persistent variable structure */
TStructure*				Cv;                     /*  Persistent class variable structure */
TStructure*				Rv;                     /*  Register variable structure */
TByteVector*            Nc;                     /*  Native code vector structure */
LpTVAL                  Fb;                     /*  Frame base address */
NUM                     modIndex;				/*  Modifier index for JIT */
NUM                     modifier[3];			/*  Modifier codes for JIT */
NUM*                   pcodes;                 /*  Pcode vector address */
OPCODE                  pcode;                  /*  Current pcode value */
TLambda*					self;                   /*  Active Procedure object */
NUM                     oldSi;                  /*  Smartbase Stack Reset position  */
NUM                     saveSi;		            /*  Smartbase Stack Save position  */
TVAL                    retValue;               /*  Smartbase Return value */
BOLE					autoHardwareMode;		/*  Automatic Hardware Register Allocation mode switch */
BOLE					jitHardwareMode;		/*  JIT Hardware Register Allocation mode switch */
NUM                     i;                      /*  Temporary index variable */
NUM                     k;                      /*  Temporary index variable */
NUM                     m;                      /*  Temporary index variable */
NUM                     n;                      /*  Temporary index variable */
LpBIND					bindPtr;				/*  Temporary pointer variable */
LpCHAR					fieldArrayPtr;			/*  Temporary pointer variable */
NUM                     codeBloatFactor;        /*  Maximum size of native IntelP4 code over DRM pcodes */
TVAL					onErrorHandler;			/*	Error Event Handler for current Lambda */
TVAL                    isource;				/*  Immediate source value */
TObjVector*				attv;					/*  Attribute vector used for record mode in vectors */
TCpx*					cp;						/*  Ptr to Complex target */
REAL					sr,si,ar,ai,am;			/*  Real/imag parts of source/argument */
LpTVAL					Tp;						/*  Vector pointer for use by the JIT */
LpNUM					Jp;						/*  Jump label translation vector pointer for use by the JIT */
LpNUM					Mp;						/*  Jump label fixup vector pointer for use by the JIT */
LpCHAR					Np;						/*  Native code instruction pointer for use by the JIT */
LpCHAR					ncodes;					/*  Native code vector address for use by the JIT */
LpCHAR					argP;					/*  Vector instruction argument pointer */
LpCHAR					srcP;					/*  Vector instruction source pointer */
LpCHAR					tarP;					/*  Vector instruction target pointer */
NUM						argInc;					/*  Vector instruction argument increment */
NUM						srcInc;					/*  Vector instruction source increment */
NUM						tarInc;					/*  Vector instruction target increment */
NUM						vecInitializeExtent;	/*  vmvecInitialize extent argument */
NUM						vecInitializeCount;		/*  vmvecInitialize internal counter register */
NUM						vecInitializeLabel;		/*  vmvecInitialize internal loop pcode address */

NUM						nativeLen;				/*  Native code vector final length for use by the JIT */
NUM						passCounter;			/*  Native code vector pass counter for use by the JIT */

NUM						xsaveEBX;				/*  Entry and exit EBX register place holder   */

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

CHAR					imod[10];				/*  Intel instruction byte codes		   */
CHAR					imodCount;				/*  Intel instruction bytes codes count	   */
NUM						Vr[21];					/*  Vector Data Array Setup Registers      */
#define		argZERO 	Vr[0]					/*  Vector Set Pointer zero constant       */
#define		argIncID 	Vr[1]					/*  Vector Set Increment arg register id   */
#define		srcIncID 	Vr[2]					/*  Vector Set Increment src register id   */
#define		tarIncID 	Vr[3]					/*  Vector Set Increment tar register id   */
#define		argNumID 	Vr[4]					/*  Vector Set Number argument register id */
#define		srcNumID 	Vr[5]					/*  Vector Set Number source register id   */
#define		tarNumID 	Vr[6]					/*  Vector Set Number target register id   */
#define		argPtrID 	Vr[7]					/*  Vector Set Pointer arg ptr register id */
#define		srcPtrID 	Vr[8]					/*  Vector Set Pointer src ptr register id */
#define		tarPtrID 	Vr[9]					/*  Vector Set Pointer tar ptr register id */
#define		argINC  	Vr[10]					/*  Vector Set arg increment integer value */
#define		srcINC  	Vr[11]					/*  Vector Set src increment integer value */
#define		tarINC  	Vr[12]					/*  Vector Set tar increment integer value */
#define		saveEBX		Vr[13]					/*  Code generator save EBX register       */
#define		saveEDI		Vr[14]					/*  Code generator save EDI register       */
#define		saveESI		Vr[15]					/*  Code generator save ESI register       */
#define		initEXTENT	Vr[16]					/*  Vector Initialize extent               */
#define		initCOUNTER	Vr[17]					/*  Vector Initialize counter			   */
#define		initLABEL	Vr[18]					/*  Vector Initialize label                */
#define		hardwareSW	Vr[19]					/*  Run In Hardware Mode Switch			   */
#define		immediatePH	Vr[20]					/*  Immediate double world place holder	   */
#define		ARGZERO 	0						/*  Vector Set Pointer zero constant       */
#define		ARGINCID 	1						/*  Vector Set Increment arg register id   */
#define		SRCINCID 	2						/*  Vector Set Increment src register id   */
#define		TARINCID 	3						/*  Vector Set Increment tar register id   */
#define		ARGNUMID 	4						/*  Vector Set Number argument register id */
#define		SRCNUMID 	5						/*  Vector Set Number source register id   */
#define		TARNUMID 	6						/*  Vector Set Number target register id   */
#define		ARGPTRID 	7						/*  Vector Set Pointer arg ptr register id */
#define		SRCPTRID 	8						/*  Vector Set Pointer src ptr register id */
#define		TARPTRID 	9						/*  Vector Set Pointer tar ptr register id */
#define		ARGINC 		10						/*  Vector Set arg increment integer value */
#define		SRCINC 		11						/*  Vector Set src increment integer value */
#define		TARINC 		12						/*  Vector Set tar increment integer value */
#define		SAVEEBX		13  					/*  Code generator save EBX register       */
#define		SAVEEDI		14						/*  Code generator save EDI register       */
#define		SAVEESI		15  					/*  Code generator save ESI register       */
#define		INITEXTENT	16						/*  Vector Initialize extent index         */
#define		INITCOUNTER	17						/*  Vector Initialize counter index		   */
#define		INITLABEL	18						/*  Vector Initialize label index          */
#define		HARDWARESW	19						/*  Run In Hardware Mode Switch			   */
#define		IMMEDIATEPH	20						/*  Immediate double world place holder	   */

/*  VM HARDWARE INTEGER AND NUMBER REGISTER ALLOCATION TABLES */

NUM						Ir[MAXREGALLOCATION];	/* Integer Register Hardware Allocations   */
NUM						Nr[MAXREGALLOCATION];	/* Number Register Hardware Allocations    */

// Temporary tval for call chain functionality
TVAL itmpValue;
/**** Initialization. ****/

#ifdef _GCC
JIT_Init();
#endif

/* Make sure the Lambda object is minimally well formed. */
if ((proc->itsObjectType != TYLAMBDA) || (proc->PcodeVector == NULL)) goto ErrorMissingPcodes;
retValue = gCP->TObject_VOID;

/****************************************************************/
/* Begin instream test cases during JIT construction and debug. */
/* Note: FOR TESTING CODE GENERATION IDEAS ONLY!!				*/
/****************************************************************/
#if	0	// ASMTEST
		Ip = Jit_LoadJumpTable(); 
		goto LVMADD;
#endif
/****************************************************************/
/* End instream test cases during JIT construction and debug.   */
/* Note: FOR TESTING CODE GENERATION IDEAS ONLY!!				*/
/****************************************************************/


/***********************************************/
/* Load main instruction emulation jump table. */
/***********************************************/
//if (FVmIntelP4_JumpTable[VMRETURN] == NIL) // RAJAH
	//{

#ifdef _GCC
    /* Load the relative address of the VM register array. */
    //LoadRpRelAddress:
    asm("lea %0, %%eax" : "=m" (Rp) :);       // rax = &Rp[0]
    asm("sub %ebp,%eax");
    asm("mov %%eax, %0" : "=m" (RpRelAddress));

    /* Load the relative address of the VM vector registers array. */
    //LoadVrRelAddress:
    asm("lea %0, %%eax" : "=m" (Vr) :);       // rax = &Vr[0]
    asm("sub %ebp,%eax");
    asm("mov %%eax, %0" : "=m" (VrRelAddress));

    asm("lea %0, %%eax" : "=m" (gTP) :);
    asm("sub %ebp,%eax");
    asm("mov %%eax, %0" : "=m" (TpRelAddress));

    asm("lea %0, %%eax" : "=m" (argc) :);
    asm("sub %ebp,%eax");
    asm("mov %%eax, %0" : "=m" (ArgcRelAddress));

    asm("lea %0, %%eax" : "=m" (argv) :);
    asm("sub %ebp,%eax");
    asm("mov %%eax, %0" : "=m" (ArgvRelAddress));

    asm("lea %0, %%eax" : "=m" (proc) :);
    asm("sub %ebp,%eax");
    asm("mov %%eax, %0" : "=m" (ProcRelAddress));

	asm("mov %%ebx, %0" : "=m" (xsaveEBX));
#else
	/* Load the relative address of the VM register array,		  */
	/* relative to the EBP register, for use in code generation.  */
	LoadRpRelAddress:
	RpRelAddress = (NUM)Jit_RelativeAddress((NUM*)&Rp[0]);

	/* Load the relative address of the VM vector registers array,	*/
	/* relative to the EBP register, for use in code generation.	*/
	LoadVrRelAddress:
	VrRelAddress = (NUM)Jit_RelativeAddress((NUM*)&Vr[0]);
#endif

	loadJumpTableEntry(LVMRETURN,VMRETURN)
    loadJumpTableEntry(LVMADD,VMADD)
    loadJumpTableEntry(LVMADDI,VMADDI)
    loadJumpTableEntry(LVMADDU,VMADDU)
    loadJumpTableEntry(LVMAND,VMAND)
    loadJumpTableEntry(LVMAPPLY,VMAPPLY)
    loadJumpTableEntry(LVMARGCOUNT,VMARGCOUNT)
    loadJumpTableEntry(LVMARGFETCH,VMARGFETCH)
    loadJumpTableEntry(LVMCALL,VMCALL)
    loadJumpTableEntry(LVMDIV,VMDIV)
    loadJumpTableEntry(LVMDIVI,VMDIVI)
    loadJumpTableEntry(LVMDIVU,VMDIVU)
    loadJumpTableEntry(LVMDIVR,VMDIVR)
    loadJumpTableEntry(LVMDIVRI,VMDIVRI)
    loadJumpTableEntry(LVMDIVRU,VMDIVRU)
    loadJumpTableEntry(LVMIADD,VMIADD)
    loadJumpTableEntry(LVMUADD,VMUADD)
    loadJumpTableEntry(LVMIDIV,VMIDIV)
    loadJumpTableEntry(LVMUDIV,VMUDIV)
    loadJumpTableEntry(LVMIDIVR,VMIDIVR)
    loadJumpTableEntry(LVMUDIVR,VMUDIVR)
    loadJumpTableEntry(LVMIMUL,VMIMUL)
    loadJumpTableEntry(LVMUMUL,VMUMUL)
    loadJumpTableEntry(LVMISUB,VMISUB)
    loadJumpTableEntry(LVMUSUB,VMUSUB)
    loadJumpTableEntry(LVMJMPEQ,VMJMPEQ)
    loadJumpTableEntry(LVMJMPLT,VMJMPLT)
    loadJumpTableEntry(LVMJMPGT,VMJMPGT)
    loadJumpTableEntry(LVMJMPNE,VMJMPNE)
    loadJumpTableEntry(LVMJMPGE,VMJMPGE)
    loadJumpTableEntry(LVMJMPLE,VMJMPLE)
    loadJumpTableEntry(LVMJUMP,VMJUMP)
    loadJumpTableEntry(LVMMOVE,VMMOVE)
    loadJumpTableEntry(LVMMUL,VMMUL)
    loadJumpTableEntry(LVMMULI,VMMULI)
    loadJumpTableEntry(LVMMULU,VMMULU)
    loadJumpTableEntry(LVMNADD,VMNADD)
    loadJumpTableEntry(LVMIAND,VMIAND)
    loadJumpTableEntry(LVMIANDB,VMIANDB)
    loadJumpTableEntry(LVMNDIV,VMNDIV)
    loadJumpTableEntry(LVMNDIVR,VMNDIVR)
    loadJumpTableEntry(LVMDEBUGGER,VMDEBUGGER)
    loadJumpTableEntry(LVMNMUL,VMNMUL)
    loadJumpTableEntry(LVMIOR,VMIOR)
    loadJumpTableEntry(LVMIORB,VMIORB)
    loadJumpTableEntry(LVMNSUB,VMNSUB)
    loadJumpTableEntry(LVMIXOR,VMIXOR)
    loadJumpTableEntry(LVMIXORB,VMIXORB)
    loadJumpTableEntry(LVMONERROR,VMONERROR)
    loadJumpTableEntry(LVMOR,VMOR)
    loadJumpTableEntry(LVMPUSH,VMPUSH)
    loadJumpTableEntry(LVMREF,VMREF)
    loadJumpTableEntry(LVMSELF,VMSELF)
    loadJumpTableEntry(LVMSEND,VMSEND)
    loadJumpTableEntry(LVMSET,VMSET)
    loadJumpTableEntry(LVMSHL,VMSHL)
    loadJumpTableEntry(LVMSHR,VMSHR)
    loadJumpTableEntry(LVMSUB,VMSUB)
    loadJumpTableEntry(LVMSUBI,VMSUBI)
    loadJumpTableEntry(LVMSUBU,VMSUBU)
    loadJumpTableEntry(LVMXOR,VMXOR)
    loadJumpTableEntry(LVMADDN,VMADDN)
    loadJumpTableEntry(LVMDIVN,VMDIVN)
    loadJumpTableEntry(LVMMOVEI,VMMOVEI)
    loadJumpTableEntry(LVMMOVEU,VMMOVEU)
    loadJumpTableEntry(LVMMULN,VMMULN)
    loadJumpTableEntry(LVMSUBN,VMSUBN)
    loadJumpTableEntry(LVMCADD,VMCADD)
    loadJumpTableEntry(LVMCDIV,VMCDIV)
    loadJumpTableEntry(LVMMOVEN,VMMOVEN)
    loadJumpTableEntry(LVMCMUL,VMCMUL)
    loadJumpTableEntry(LVMCSUB,VMCSUB)
    loadJumpTableEntry(LVMREFTEXT,VMREFTEXT)
    loadJumpTableEntry(LVMREFSTRING,VMREFSTRING)
    loadJumpTableEntry(LVMSETSTRING,VMSETSTRING)
    loadJumpTableEntry(LVMREFSYMBOL,VMREFSYMBOL)
    loadJumpTableEntry(LVMREFVECTOR,VMREFVECTOR)
    loadJumpTableEntry(LVMSETVECTOR,VMSETVECTOR)
    loadJumpTableEntry(LVMREFSTRVALUE,VMREFSTRVALUE)
    loadJumpTableEntry(LVMSETSTRVALUE,VMSETSTRVALUE)
    loadJumpTableEntry(LVMREFSTRKEY,VMREFSTRKEY)
    loadJumpTableEntry(LVMSETSTRKEY,VMSETSTRKEY)
    loadJumpTableEntry(LVMREFDICVALUE,VMREFDICVALUE)
    loadJumpTableEntry(LVMSETDICVALUE,VMSETDICVALUE)
    loadJumpTableEntry(LVMREFDICKEY,VMREFDICKEY)
    loadJumpTableEntry(LVMSETDICKEY,VMSETDICKEY)
    loadJumpTableEntry(LVMREFDIRVALUE,VMREFDIRVALUE)
    loadJumpTableEntry(LVMSETDIRVALUE,VMSETDIRVALUE)
    loadJumpTableEntry(LVMREFDIRKEY,VMREFDIRKEY)
    loadJumpTableEntry(LVMSETDIRKEY,VMSETDIRKEY)
    loadJumpTableEntry(LVMREFBITVECTOR,VMREFBITVECTOR)
    loadJumpTableEntry(LVMSETBITVECTOR,VMSETBITVECTOR)
    loadJumpTableEntry(LVMREFBYTVECTOR,VMREFBYTVECTOR)
    loadJumpTableEntry(LVMSETBYTVECTOR,VMSETBYTVECTOR)
    loadJumpTableEntry(LVMREFPCDVECTOR,VMREFPCDVECTOR)
    loadJumpTableEntry(LVMSETPCDVECTOR,VMSETPCDVECTOR)
    loadJumpTableEntry(LVMREFOBJVECTOR,VMREFOBJVECTOR)
    loadJumpTableEntry(LVMSETOBJVECTOR,VMSETOBJVECTOR)
    loadJumpTableEntry(LVMREFINTVECTOR,VMREFINTVECTOR)
    loadJumpTableEntry(LVMSETINTVECTOR,VMSETINTVECTOR)
    loadJumpTableEntry(LVMREFNUMVECTOR,VMREFNUMVECTOR)
    loadJumpTableEntry(LVMSETNUMVECTOR,VMSETNUMVECTOR)
    loadJumpTableEntry(LVMREFFLTVECTOR,VMREFFLTVECTOR)
    loadJumpTableEntry(LVMSETFLTVECTOR,VMSETFLTVECTOR)
    loadJumpTableEntry(LVMREFMATRIX,VMREFMATRIX)
    loadJumpTableEntry(LVMSETMATRIX,VMSETMATRIX)
    loadJumpTableEntry(LVMREFNUMMATRIX,VMREFNUMMATRIX)
    loadJumpTableEntry(LVMSETNUMMATRIX,VMSETNUMMATRIX)
    loadJumpTableEntry(LVMTESTESCAPE,VMTESTESCAPE)
    loadJumpTableEntry(LvmnatJmpEQInteger,vmnatJmpEQInteger)		
    loadJumpTableEntry(LvmnatJmpLTInteger,vmnatJmpLTInteger)
    loadJumpTableEntry(LvmnatJmpGTInteger,vmnatJmpGTInteger)
    loadJumpTableEntry(LvmnatJmpNEInteger,vmnatJmpNEInteger)
    loadJumpTableEntry(LvmnatJmpGEInteger,vmnatJmpGEInteger)
    loadJumpTableEntry(LvmnatJmpLEInteger,vmnatJmpLEInteger)
    loadJumpTableEntry(LvmnatJmpEQInteger,vmnatJmpEQUInteger)		
    loadJumpTableEntry(LvmnatJmpLTInteger,vmnatJmpLTUInteger)
    loadJumpTableEntry(LvmnatJmpGTInteger,vmnatJmpGTUInteger)
    loadJumpTableEntry(LvmnatJmpNEInteger,vmnatJmpNEUInteger)
    loadJumpTableEntry(LvmnatJmpGEInteger,vmnatJmpGEUInteger)
    loadJumpTableEntry(LvmnatJmpLEInteger,vmnatJmpLEUInteger)	
	loadJumpTableEntry(LvmnatJmpEQNumber,vmnatJmpEQNumber)
    loadJumpTableEntry(LvmnatJmpLTNumber,vmnatJmpLTNumber)
    loadJumpTableEntry(LvmnatJmpGTNumber,vmnatJmpGTNumber)
    loadJumpTableEntry(LvmnatJmpNENumber,vmnatJmpNENumber)
    loadJumpTableEntry(LvmnatJmpGENumber,vmnatJmpGENumber)
    loadJumpTableEntry(LvmnatJmpLENumber,vmnatJmpLENumber)  
    loadJumpTableEntry(LvmnatAddInteger,vmnatAddInteger)  
    loadJumpTableEntry(LvmnatAddNumber,vmnatAddNumber)  
    loadJumpTableEntry(LvmnatAndInteger,vmnatAndInteger)  
    loadJumpTableEntry(LvmnatDivInteger,vmnatDivInteger)  
    loadJumpTableEntry(LvmnatDivNumber,vmnatDivNumber)  
    loadJumpTableEntry(LvmnatDivrInteger,vmnatDivrInteger)  
    loadJumpTableEntry(LvmnatDivrNumber,vmnatDivrNumber)  
    loadJumpTableEntry(LvmnatMulInteger,vmnatMulInteger)  
    loadJumpTableEntry(LvmnatMulNumber,vmnatMulNumber)  
    loadJumpTableEntry(LvmnatOrInteger,vmnatOrInteger)  
    loadJumpTableEntry(LvmnatShlInteger,vmnatShlInteger)  
    loadJumpTableEntry(LvmnatShrInteger,vmnatShrInteger)  
    loadJumpTableEntry(LvmnatSubNumber,vmnatSubNumber)  
    loadJumpTableEntry(LvmnatXorInteger,vmnatXorInteger)  
    loadJumpTableEntry(LvmnatLoadCharacter,vmnatLoadCharacter)  
    loadJumpTableEntry(LvmnatLoadFloat,vmnatLoadFloat)  
    loadJumpTableEntry(LvmnatLoadInteger,vmnatLoadInteger)  
    loadJumpTableEntry(LvmnatLoadUInteger,vmnatLoadUInteger)  
    loadJumpTableEntry(LvmnatLoadLong,vmnatLoadLong)  
    loadJumpTableEntry(LvmnatLoadNumber,vmnatLoadNumber)  
    loadJumpTableEntry(LvmnatLoadObject,vmnatLoadObject)  
    loadJumpTableEntry(LvmnatLoadShort,vmnatLoadShort)  
    loadJumpTableEntry(LvmnatSaveCharacter,vmnatSaveCharacter)  
    loadJumpTableEntry(LvmnatSaveFloat,vmnatSaveFloat)  
    loadJumpTableEntry(LvmnatSaveInteger,vmnatSaveInteger)  
    loadJumpTableEntry(LvmnatSaveLong,vmnatSaveLong)  
    loadJumpTableEntry(LvmnatSaveNumber,vmnatSaveNumber)  
    loadJumpTableEntry(LvmnatSaveObject,vmnatSaveObject)  
    loadJumpTableEntry(LvmnatSaveShort,vmnatSaveShort)  
    loadJumpTableEntry(LvmnatSubInteger,vmnatSubInteger)  
    loadJumpTableEntry(LvmregAbsNumber,vmregAbsNumber)
    loadJumpTableEntry(LvmregNumber,vmregNumber)
    loadJumpTableEntry(LvmregInteger,vmregInteger)
    loadJumpTableEntry(LvmregAddImmediate,vmregAddImmediate)
    loadJumpTableEntry(LvmregAddInteger,vmregAddInteger)
    loadJumpTableEntry(LvmregAddNumber,vmregAddNumber)
    loadJumpTableEntry(LvmregAddPointer,vmregAddPointer)
    loadJumpTableEntry(LvmregAndImmediate,vmregAndImmediate)
    loadJumpTableEntry(LvmregAndInteger,vmregAndInteger)
    loadJumpTableEntry(LvmregCosNumber,vmregCosNumber)
    loadJumpTableEntry(LvmregDivImmediate,vmregDivImmediate)
    loadJumpTableEntry(LvmregDivInteger,vmregDivInteger)
    loadJumpTableEntry(LvmregDivNumber,vmregDivNumber)
    loadJumpTableEntry(LvmregDivrImmediate,vmregDivrImmediate)
    loadJumpTableEntry(LvmregDivrInteger,vmregDivrInteger)
    loadJumpTableEntry(LvmregDivrNumber,vmregDivrNumber)
    loadJumpTableEntry(LvmregIncPointer,vmregIncPointer)
    loadJumpTableEntry(LvmregJmpEQImmediate,vmregJmpEQImmediate)		
    loadJumpTableEntry(LvmregJmpLTImmediate,vmregJmpLTImmediate)
    loadJumpTableEntry(LvmregJmpGTImmediate,vmregJmpGTImmediate)
    loadJumpTableEntry(LvmregJmpNEImmediate,vmregJmpNEImmediate)
    loadJumpTableEntry(LvmregJmpGEImmediate,vmregJmpGEImmediate)
    loadJumpTableEntry(LvmregJmpLEImmediate,vmregJmpLEImmediate)
	loadJumpTableEntry(LvmregJmpEQUImmediate,vmregJmpEQUImmediate)		
    loadJumpTableEntry(LvmregJmpLTUImmediate,vmregJmpLTUImmediate)
    loadJumpTableEntry(LvmregJmpGTUImmediate,vmregJmpGTUImmediate)
    loadJumpTableEntry(LvmregJmpNEUImmediate,vmregJmpNEUImmediate)
    loadJumpTableEntry(LvmregJmpGEUImmediate,vmregJmpGEUImmediate)
    loadJumpTableEntry(LvmregJmpLEUImmediate,vmregJmpLEUImmediate)
	loadJumpTableEntry(LvmregJmpEQInteger,vmregJmpEQInteger)		
    loadJumpTableEntry(LvmregJmpLTInteger,vmregJmpLTInteger)
    loadJumpTableEntry(LvmregJmpGTInteger,vmregJmpGTInteger)
    loadJumpTableEntry(LvmregJmpNEInteger,vmregJmpNEInteger)
    loadJumpTableEntry(LvmregJmpGEInteger,vmregJmpGEInteger)
    loadJumpTableEntry(LvmregJmpLEInteger,vmregJmpLEInteger)
	loadJumpTableEntry(LvmregJmpEQUInteger,vmregJmpEQUInteger)		
    loadJumpTableEntry(LvmregJmpLTUInteger,vmregJmpLTUInteger)
    loadJumpTableEntry(LvmregJmpGTUInteger,vmregJmpGTUInteger)
    loadJumpTableEntry(LvmregJmpNEUInteger,vmregJmpNEUInteger)
    loadJumpTableEntry(LvmregJmpGEUInteger,vmregJmpGEUInteger)
    loadJumpTableEntry(LvmregJmpLEUInteger,vmregJmpLEUInteger)
	loadJumpTableEntry(LvmregJmpEQNumber,vmregJmpEQNumber)	
    loadJumpTableEntry(LvmregJmpLTNumber,vmregJmpLTNumber)
    loadJumpTableEntry(LvmregJmpGTNumber,vmregJmpGTNumber)
    loadJumpTableEntry(LvmregJmpNENumber,vmregJmpNENumber)
    loadJumpTableEntry(LvmregJmpGENumber,vmregJmpGENumber)
    loadJumpTableEntry(LvmregJmpLENumber,vmregJmpLENumber) 
    loadJumpTableEntry(LvmregJump,vmregJump) 
    loadJumpTableEntry(LvmregLoadAddress,vmregLoadAddress)  
    loadJumpTableEntry(LvmregLoadInteger,vmregLoadInteger) 
    loadJumpTableEntry(LvmregLoadTail,vmregLoadTail) 
    loadJumpTableEntry(LvmregLoadDeclType,vmregLoadDeclType) 
    loadJumpTableEntry(LvmregLoadType,vmregLoadType) 
    loadJumpTableEntry(LvmregLoadJmpPointer,vmregLoadJmpPointer)
    loadJumpTableEntry(LvmregLoadNumber,vmregLoadNumber)
    loadJumpTableEntry(LvmregLogNumber,vmregLogNumber)
    loadJumpTableEntry(LvmregMoveImmediate,vmregMoveImmediate)
    loadJumpTableEntry(LvmregMoveInteger,vmregMoveInteger)
    loadJumpTableEntry(LvmregMoveNumber,vmregMoveNumber)
    loadJumpTableEntry(LvmregMulImmediate,vmregMulImmediate)
    loadJumpTableEntry(LvmregMulInteger,vmregMulInteger)
    loadJumpTableEntry(LvmregMulNumber,vmregMulNumber)
    loadJumpTableEntry(LvmregObjLength,vmregObjLength)
    loadJumpTableEntry(LvmregObjPointer,vmregObjPointer)
    loadJumpTableEntry(LvmregOrImmediate,vmregOrImmediate)
    loadJumpTableEntry(LvmregOrInteger,vmregOrInteger)
    loadJumpTableEntry(LvmregPwrNumber,vmregPwrNumber)
    loadJumpTableEntry(LvmregRefCharacter,vmregRefCharacter)
    loadJumpTableEntry(LvmregRefFloat,vmregRefFloat)  
    loadJumpTableEntry(LvmregRefInteger,vmregRefInteger)
    loadJumpTableEntry(LvmregRefLong,vmregRefLong)
    loadJumpTableEntry(LvmregRefNumber,vmregRefNumber)
    loadJumpTableEntry(LvmregRefShort,vmregRefShort)  
    loadJumpTableEntry(LvmregRefWord,vmregRefWord)
    loadJumpTableEntry(LvmregRefXCharacter,vmregRefXCharacter)
    loadJumpTableEntry(LvmregRefXFloat,vmregRefXFloat)
    loadJumpTableEntry(LvmregRefXInteger,vmregRefXInteger)
    loadJumpTableEntry(LvmregRefXLong,vmregRefXLong)
    loadJumpTableEntry(LvmregRefXNumber,vmregRefXNumber)
    loadJumpTableEntry(LvmregRefXShort,vmregRefXShort)	
    loadJumpTableEntry(LvmregRefXWord,vmregRefXWord)	
    loadJumpTableEntry(LvmregRunInHardware,vmregRunInHardware)	
    loadJumpTableEntry(LvmregSaveInteger,vmregSaveInteger)
    loadJumpTableEntry(LvmregSaveUInteger,vmregSaveUInteger)
    loadJumpTableEntry(LvmregSaveTail,vmregSaveTail)
    loadJumpTableEntry(LvmregSaveTailImmediate,vmregSaveTailImmediate)
    loadJumpTableEntry(LvmregSaveDeclType,vmregSaveDeclType)
    loadJumpTableEntry(LvmregSaveDeclTypeImmediate,vmregSaveDeclTypeImmediate)
    loadJumpTableEntry(LvmregSaveNumber,vmregSaveNumber)
    loadJumpTableEntry(LvmregSetXCharImmediate,vmregSetXCharImmediate)
    loadJumpTableEntry(LvmregSetXIntImmediate,vmregSetXIntImmediate)
    loadJumpTableEntry(LvmregSetXLongImmediate,vmregSetXLongImmediate)
    loadJumpTableEntry(LvmregSetXShortImmediate,vmregSetXShortImmediate)
    loadJumpTableEntry(LvmregSetXWord,vmregSetXWord)
    loadJumpTableEntry(LvmregSetCharImmediate,vmregSetCharImmediate)
    loadJumpTableEntry(LvmregSetIntImmediate,vmregSetIntImmediate)
    loadJumpTableEntry(LvmregSetLongImmediate,vmregSetLongImmediate)
    loadJumpTableEntry(LvmregSetShortImmediate,vmregSetShortImmediate)
    loadJumpTableEntry(LvmregSetCharacter,vmregSetCharacter)
    loadJumpTableEntry(LvmregSetFloat,vmregSetFloat)  
    loadJumpTableEntry(LvmregSetInteger,vmregSetInteger)
    loadJumpTableEntry(LvmregSetLong,vmregSetLong)
    loadJumpTableEntry(LvmregSetNumber,vmregSetNumber)
    loadJumpTableEntry(LvmregSetShort,vmregSetShort)
    loadJumpTableEntry(LvmregSetWord,vmregSetWord)
    loadJumpTableEntry(LvmregSetXCharacter,vmregSetXCharacter)
    loadJumpTableEntry(LvmregSetXFloat,vmregSetXFloat)
    loadJumpTableEntry(LvmregSetXInteger,vmregSetXInteger)
    loadJumpTableEntry(LvmregSetXLong,vmregSetXLong)
    loadJumpTableEntry(LvmregSetXNumber,vmregSetXNumber)
    loadJumpTableEntry(LvmregSetXShort,vmregSetXShort)	
    loadJumpTableEntry(LvmregShlImmediate,vmregShlImmediate)
    loadJumpTableEntry(LvmregShlInteger,vmregShlInteger)  
    loadJumpTableEntry(LvmregShrImmediate,vmregShrImmediate)
    loadJumpTableEntry(LvmregShrInteger,vmregShrInteger)  
    loadJumpTableEntry(LvmregSinNumber,vmregSinNumber)
    loadJumpTableEntry(LvmregSqrtNumber,vmregSqrtNumber)
    loadJumpTableEntry(LvmregStringCompare,vmregStringCompare)  
    loadJumpTableEntry(LvmregStringiCompare,vmregStringiCompare)  
    loadJumpTableEntry(LvmregSubImmediate,vmregSubImmediate)
    loadJumpTableEntry(LvmregSubInteger,vmregSubInteger)
    loadJumpTableEntry(LvmregSubNumber,vmregSubNumber)
    loadJumpTableEntry(LvmregSubPointer,vmregSubPointer)
    loadJumpTableEntry(LvmregTanNumber,vmregTanNumber)
    loadJumpTableEntry(LvmregXorImmediate,vmregXorImmediate)
    loadJumpTableEntry(LvmregXorInteger,vmregXorInteger)
    loadJumpTableEntry(LvmvecBinary,vmvecBinary)
    loadJumpTableEntry(LvmvecInitialize,vmvecInitialize)
    loadJumpTableEntry(LvmvecLoop,vmvecLoop)
    loadJumpTableEntry(LvmvecNumScalar,vmvecNumScalar)
    loadJumpTableEntry(LvmvecNumVector,vmvecNumVector)
    loadJumpTableEntry(LvmvecPop,vmvecPop)
    loadJumpTableEntry(LvmvecPopNumber,vmvecPopNumber)
    loadJumpTableEntry(LvmvecPush,vmvecPush)
    loadJumpTableEntry(LvmvecPushNumber,vmvecPushNumber)
    loadJumpTableEntry(LvmvecSetIncrements,vmvecSetIncrements)
    loadJumpTableEntry(LvmvecSetPointers,vmvecSetPointers)
    loadJumpTableEntry(LvmvecSwapCC,vmvecSwapCC)
    loadJumpTableEntry(LvmvecUnary,vmvecUnary)

	/* Load each entry in the main VM instruction flags table. */
	for (n = 0; n < _VMMAXINSTRS; ++n)
		{
		vmInsFlags[n] = FALSE;
		}

    vmInsFlags[VMPOP] = TRUE;
    vmInsFlags[VMPUSH] = TRUE;
    vmInsFlags[VMRETURN] = TRUE;
    vmInsFlags[VMMOVE] = TRUE;
    vmInsFlags[VMAPPLY] = TRUE;
    vmInsFlags[VMCALL] = TRUE;
    vmInsFlags[VMMOVEN] = TRUE;
    vmInsFlags[VMMOVEI] = TRUE;
    vmInsFlags[VMMOVEU] = TRUE;
    vmInsFlags[VMONERROR] = TRUE;
    vmInsFlags[VMREF] = TRUE;
    vmInsFlags[VMSEND] = TRUE;
    vmInsFlags[VMSET] = TRUE;
    vmInsFlags[VMTESTESCAPE] = TRUE;
    vmInsFlags[vmregDivrInteger] = TRUE;
    vmInsFlags[vmregDivrNumber] = TRUE;
    vmInsFlags[vmregLogNumber] = TRUE;
    vmInsFlags[vmregPwrNumber] = TRUE;
    vmInsFlags[vmregLoadJmpPointer] = TRUE;
    vmInsFlags[vmregDivrImmediate] = TRUE;

	//} RAJAH
/*******************************************************/
/* End instruction emulation optimization code study.  */
/*******************************************************/

/*  ===================================================================== */
/*  Start Just In Time Compiler.                                          */
/*                                                                        */
/*  Here we perform the just in time compilation the first time we try to */
/*  evaluate an Lambda (if we have not already done so). This will occur   */
/*  only the first time the Lambda is compiled or loaded.                  */
/*  ===================================================================== */
if (proc->NativeCodeVector == NIL)
	{
	/* Set the native code vector pass counter to zero */
	/* Set the initial code bloat factor               */
	/* Set the hardware mode switch to off             */
	passCounter = 0;
	codeBloatFactor = 4;
	Vr[HARDWARESW] = FALSE;

	/* Load the Pcode Vector register. */
	/* Make sure this Procedure object has a Pcode Vector. */
	if (proc->PcodeVector == NIL) 
		{TopOfStack = saveSi; EndRecursion; goto ErrorMissingPcodes;}
	Pc = proc->PcodeVector;
	if (Pc->itsMaxItemIndex <= 0) goto EndJustInTimeCompiler;

	/* Set the Instruction Pointer to the start of the pcode vector.     */
	Ip = &atHMInt(Pc->itsInstructionArray,0);
	pcodes = &atHMInt(Pc->itsInstructionArray,0);

	/* Allocate the native code vector. */
	proc->NativeCodeVector = TByteVector_New(gCP,gTP);
	proc->NativeCodeVector->itsCdr.u.Vector = TVector_New(gCP,gTP);
	proc->NativeCodeVector->itsCdr.Tag = TYVECTOR;
	TVector_SetMaxIndex(gCP,gTP,proc->NativeCodeVector->itsCdr,3);
	Tp = TvalArray(proc->NativeCodeVector->itsCdr);

	/* Allocate the native code vector. */
	Tp[0].u.ByteVector = TByteVector_New(gCP,gTP);
	Tp[0].Tag = TYBYTEVECTOR;
	
	ResizeNativeCodeVector:
	++codeBloatFactor;
	TByteVector_SetMaxIndex(gCP,gTP,Tp[0],(Pc->itsMaxItemIndex*sizeof(NUM)*codeBloatFactor)+500);
	ncodes = Np = ByteArray(Tp[0]);
	nativeLen = (NUM)Tp[0].u.ByteVector->itsMaxItemIndex - 500;

	/* Allocate the jump label translation vector. */
	Tp[1].u.IntVector = TIntVector_New(gCP,gTP);
	Tp[1].Tag = TYINTVECTOR;
	TIntVector_SetMaxIndex(gCP,gTP,Tp[1],Pc->itsMaxItemIndex);
	Jp = IntArray(Tp[1]);

	/* Allocate the jump label fixup vector. */
	Tp[2].u.IntVector = TIntVector_New(gCP,gTP);
	Tp[2].Tag = TYINTVECTOR;
	TIntVector_SetMaxIndex(gCP,gTP,Tp[2],Pc->itsMaxItemIndex);
	Mp = IntArray(Tp[2]);

	/*  Load the Register variable bank.     */
	/*  Note: Initialize the current virtual */
	/*        machine registers to the types */
	/*        specified in the register      */
	/*        variables structure so we can  */
	/*        perform register allocation    */
	if (proc->RegisterVariables == NIL)
		{
		Rv = NIL;
		}
	else
		{
		Rv = proc->RegisterVariables;
		n = MAXREGISTERCNT;
		if (Rv->itsMaxItemIndex > MAXREGISTERCNT) goto ErrorTooManyRegisters;
		for (i = 0; i < n; ++i)
			{
			if (i < Rv->itsMaxItemIndex)
				{ 
				Rp[i] = atHMBind(Rv->itsDictionaryArray,i).Value;
				}
			else
				{
				Rp[i] = gCP->Tval_VOID;
				}				
			}
		}

	/* Start the JIT in automatic hardware register allocation mode on */
	/* Note: we simulate a (vmregRunInHardware start:) instruction     */
	jitHardwareMode = autoHardwareMode = JITREGMODEON;

	/* Start JIT First Pass: Translate the pcodes into native codes. */
	FirstPass:
	ncodes = Np = ByteArray(Tp[0]);
	n = -1;
	Vr[HARDWARESW] = autoHardwareMode;
	if (Vr[HARDWARESW] == TRUE) goto StartRunInHardware;
	for (n = 0; n < Pc->itsMaxItemIndex; ++n)
		{
		pcode.Opcode = pcodes[n];
		
		AfterOpcodeLoad:
		modifier[0] = pcode.u.Am1;
		modifier[1] = pcode.u.Am2;
		modifier[2] = pcode.u.Am3;

		/*  Memory opcodes: Append to the Pcode Vector all remaining arguments. */
		if (pcode.u.Pcode < VMSTARTREGISTERINS)
			{			
			switch (pcode.u.Pcode)
				{
				/* Ignore the VMDEBUGGER opcode and reload the original opcode. */
				case VMDEBUGGER:
					/*  Place the new debugger interrupt code here. */
					retValue = FSmartbase_Ref(gCP,gTP,2,proc->Interfaces,TOBJ(gCP->TLambda_BreakList));
					if (retValue.Tag != TYDIRECTORY) goto ErrorIllegalInstruction;
					retValue = FSmartbase_Ref(gCP,gTP,2,retValue,TINT(n));
					if (retValue.Tag != TYNUM) goto ErrorIllegalInstruction;
					pcode.Opcode = retValue.u.Int;
                    goto AfterOpcodeLoad;
					break;

				/* Register VMPOP opcode so it can run in hardware mode. */
				case VMPOP:
					regOutStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					SAVEALLREGS
					if (pcode.u.Am1 != AMVOID) 
						{
						/*** (Rp[Am1]+offset) = gTP->TvalStack[--TopOfStack] = ; ***/
						/*** ecx = target = &gTP->TvalStack[--TopOfStack];       ***/ 
						/* mov    ecx,gTP							    */ *Np++ = 0x8B;*Np++ = 0x4D;*Np++ = 0x10; 
					    /* dec    dword ptr [ecx+4]                     */ *Np++ = 0xFF;*Np++ = 0x49;*Np++ = 0x04; 
						/* mov    edx,dword ptr [ecx+4]				    */ *Np++ = 0x8B;*Np++ = 0x51;*Np++ = 0x04; 
						/* shl    edx,4             				    */ *Np++ = 0xC1;*Np++ = 0xE2;*Np++ = 0x04; 
						/* add    edx,dword ptr [ecx]	                */ *Np++ = 0x03;*Np++ = 0x11;tarPtrID = 0; 
        				/*** source = (Rp[Am1]+offset);               ***/
						/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4; 
						/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); srcPtrID = pcodes[n];
						/* mov    eax,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = tarPtrID;Np+=4;
						/* mov    dword Ptr [ecx+0x11223344],eax        */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = srcPtrID;Np+=4;

						/* mov    eax,dword Ptr [edx+4+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+4);Np+=4;
						/* mov    dword Ptr [ecx+4+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+4);Np+=4;
						/* mov    eax,dword Ptr [edx+8+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+8);Np+=4;
						/* mov    dword Ptr [ecx+8+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+8);Np+=4;
						/* mov    eax,dword Ptr [edx+12+0x11223344]     */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+12);Np+=4;
						/* mov    dword Ptr [ecx+12+0x11223344],eax     */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+12);Np+=4;
						}
					if (pcode.u.Am2 != AMVOID) 
						{
						/*** (Rp[Am2]+offset) = gTP->TvalStack[--TopOfStack] = ; ***/
						/*** ecx = target = &gTP->TvalStack[--TopOfStack];       ***/ 
						/* mov    ecx,gTP							    */ *Np++ = 0x8B;*Np++ = 0x4D;*Np++ = 0x10; 
					    /* dec    dword ptr [ecx+4]                     */ *Np++ = 0xFF;*Np++ = 0x49;*Np++ = 0x04; 
						/* mov    edx,dword ptr [ecx+4]				    */ *Np++ = 0x8B;*Np++ = 0x51;*Np++ = 0x04; 
						/* shl    edx,4             				    */ *Np++ = 0xC1;*Np++ = 0xE2;*Np++ = 0x04; 
						/* add    edx,dword ptr [ecx]	                */ *Np++ = 0x03;*Np++ = 0x11;tarPtrID = 0; 
        				/*** source = (Rp[Am2]+offset);               ***/
						/* mov    ecx,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); srcPtrID = pcodes[n];
						/* mov    eax,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = tarPtrID;Np+=4;
						/* mov    dword Ptr [ecx+0x11223344],eax        */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = srcPtrID;Np+=4;

						/* mov    eax,dword Ptr [edx+4+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+4);Np+=4;
						/* mov    dword Ptr [ecx+4+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+4);Np+=4;
						/* mov    eax,dword Ptr [edx+8+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+8);Np+=4;
						/* mov    dword Ptr [ecx+8+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+8);Np+=4;
						/* mov    eax,dword Ptr [edx+12+0x11223344]     */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+12);Np+=4;
						/* mov    dword Ptr [ecx+12+0x11223344],eax     */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+12);Np+=4;
						}
					if (pcode.u.Am3 != AMVOID) 
						{
						/*** (Rp[Am3]+offset) = gTP->TvalStack[--TopOfStack] = ; ***/
						/*** ecx = target = &gTP->TvalStack[--TopOfStack];       ***/ 
						/* mov    ecx,gTP							    */ *Np++ = 0x8B;*Np++ = 0x4D;*Np++ = 0x10; 
					    /* dec    dword ptr [ecx+4]                     */ *Np++ = 0xFF;*Np++ = 0x49;*Np++ = 0x04; 
						/* mov    edx,dword ptr [ecx+4]				    */ *Np++ = 0x8B;*Np++ = 0x51;*Np++ = 0x04; 
						/* shl    edx,4             				    */ *Np++ = 0xC1;*Np++ = 0xE2;*Np++ = 0x04; 
						/* add    edx,dword ptr [ecx]	                */ *Np++ = 0x03;*Np++ = 0x11;tarPtrID = 0; 
        				/*** source = (Rp[Am3]+offset);               ***/
						/* mov    ecx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); srcPtrID = pcodes[n];
						/* mov    eax,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = tarPtrID;Np+=4;
						/* mov    dword Ptr [ecx+0x11223344],eax        */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = srcPtrID;Np+=4;

						/* mov    eax,dword Ptr [edx+4+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+4);Np+=4;
						/* mov    dword Ptr [ecx+4+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+4);Np+=4;
						/* mov    eax,dword Ptr [edx+8+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+8);Np+=4;
						/* mov    dword Ptr [ecx+8+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+8);Np+=4;
						/* mov    eax,dword Ptr [edx+12+0x11223344]     */ *Np++ = 0x8B;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+12);Np+=4;
						/* mov    dword Ptr [ecx+12+0x11223344],eax     */ *Np++ = 0x89;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+12);Np+=4;
						}
					LOADALLREGS
					break;
				/* Register VMPUSH opcode so it can run in hardware mode. */
				case VMPUSH:
					regOutStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					SAVEALLREGS
					if (pcode.u.Am1 != AMVOID) 
						{
						/*** gTP->TvalStack[TopOfStack++] = (Rp[Am1]+offset);  ***/
						/*** ecx = target = &gTP->TvalStack[TopOfStack++];     ***/ 
						/* mov    ecx,gTP							    */ *Np++ = 0x8B;*Np++ = 0x4D;*Np++ = 0x10; 
						/* mov    edx,dword ptr [ecx+4]				    */ *Np++ = 0x8B;*Np++ = 0x51;*Np++ = 0x04; 
						/* inc    dword ptr [ecx+4]                     */ *Np++ = 0xFF;*Np++ = 0x41;*Np++ = 0x04; 
						/* shl    edx,4             				    */ *Np++ = 0xC1;*Np++ = 0xE2;*Np++ = 0x04; 
						/* add    edx,dword ptr [ecx]	                */ *Np++ = 0x03;*Np++ = 0x11;tarPtrID = 0; 
        				/*** source = (Rp[Am1]+offset);               ***/
						/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); srcPtrID = pcodes[n];
						/* mov    eax,dword Ptr [ecx+0x11223344]        */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = srcPtrID;Np+=4;
						/* mov    dword Ptr [edx+0x11223344],eax        */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = tarPtrID;Np+=4;

						/* mov    eax,dword Ptr [ecx+4+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+4);Np+=4;
						/* mov    dword Ptr [edx+4+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+4);Np+=4;
						/* mov    eax,dword Ptr [ecx+8+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+8);Np+=4;
						/* mov    dword Ptr [edx+8+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+8);Np+=4;
						/* mov    eax,dword Ptr [ecx+12+0x11223344]     */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+12);Np+=4;
						/* mov    dword Ptr [edx+12+0x11223344],eax     */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+12);Np+=4;
						}
					if (pcode.u.Am2 != AMVOID) 
						{
						/*** gTP->TvalStack[TopOfStack++] = (Rp[Am2]+offset);  ***/
						/*** ecx = target = &gTP->TvalStack[TopOfStack++];     ***/ 
						/* mov    ecx,gTP							    */ *Np++ = 0x8B;*Np++ = 0x4D;*Np++ = 0x10; 
						/* mov    edx,dword ptr [ecx+4]				    */ *Np++ = 0x8B;*Np++ = 0x51;*Np++ = 0x04; 
						/* inc    dword ptr [ecx+4]                     */ *Np++ = 0xFF;*Np++ = 0x41;*Np++ = 0x04; 
						/* shl    edx,4             				    */ *Np++ = 0xC1;*Np++ = 0xE2;*Np++ = 0x04; 
						/* add    edx,dword ptr [ecx]	                */ *Np++ = 0x03;*Np++ = 0x11;tarPtrID = 0; 
        				/*** source = (Rp[Am2]+offset);               ***/
						/* mov    ecx,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); srcPtrID = pcodes[n];
						/* mov    eax,dword Ptr [ecx+0x11223344]        */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = srcPtrID;Np+=4;
						/* mov    dword Ptr [edx+0x11223344],eax        */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = tarPtrID;Np+=4;

						/* mov    eax,dword Ptr [ecx+4+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+4);Np+=4;
						/* mov    dword Ptr [edx+4+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+4);Np+=4;
						/* mov    eax,dword Ptr [ecx+8+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+8);Np+=4;
						/* mov    dword Ptr [edx+8+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+8);Np+=4;
						/* mov    eax,dword Ptr [ecx+12+0x11223344]     */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+12);Np+=4;
						/* mov    dword Ptr [edx+12+0x11223344],eax     */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+12);Np+=4;
						}
					if (pcode.u.Am3 != AMVOID) 
						{
						/*** gTP->TvalStack[TopOfStack++] = (Rp[Am3]+offset);  ***/
						/*** ecx = target = &gTP->TvalStack[TopOfStack++];     ***/ 
						/* mov    ecx,gTP							    */ *Np++ = 0x8B;*Np++ = 0x4D;*Np++ = 0x10; 
						/* mov    edx,dword ptr [ecx+4]				    */ *Np++ = 0x8B;*Np++ = 0x51;*Np++ = 0x04; 
						/* inc    dword ptr [ecx+4]                     */ *Np++ = 0xFF;*Np++ = 0x41;*Np++ = 0x04; 
						/* shl    edx,4             				    */ *Np++ = 0xC1;*Np++ = 0xE2;*Np++ = 0x04; 
						/* add    edx,dword ptr [ecx]	                */ *Np++ = 0x03;*Np++ = 0x11;tarPtrID = 0; 
        				/*** source = (Rp[Am3]+offset);               ***/
						/* mov    ecx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); srcPtrID = pcodes[n];
						/* mov    eax,dword Ptr [ecx+0x11223344]        */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = srcPtrID;Np+=4;
						/* mov    dword Ptr [edx+0x11223344],eax        */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = tarPtrID;Np+=4;

						/* mov    eax,dword Ptr [ecx+4+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+4);Np+=4;
						/* mov    dword Ptr [edx+4+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+4);Np+=4;
						/* mov    eax,dword Ptr [ecx+8+0x11223344]      */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+8);Np+=4;
						/* mov    dword Ptr [edx+8+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+8);Np+=4;
						/* mov    eax,dword Ptr [ecx+12+0x11223344]     */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (srcPtrID+12);Np+=4;
						/* mov    dword Ptr [edx+12+0x11223344],eax     */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+12);Np+=4;
						}
					LOADALLREGS
					break;

				case VMSELF:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov    eax,proc                              */ *Np++ = 0x8B;*Np++ = 0x45;*Np++ = 0x14;
        			/*** target = (Rp[Am1]+offset);               ***/
					/* mov    edx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); tarPtrID = pcodes[n];
					/* mov    dword Ptr [edx+0x11223344],eax        */ *Np++ = 0x89;*Np++ = 0x82; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    byte ptr [edx+15+0x11223344],TYLAMBDA  */ *Np++ = 0xC6;*Np++ = 0x82; *(LpNUM)Np = (tarPtrID+15); Np+=4; *Np++ = TYLAMBDA;
					break;

				case VMARGCOUNT:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
#ifdef _GCC
					/* mov    eax,argc                              */ *Np++ = 0x8B;*Np++ = 0x45; *Np++ = (NUM)ArgcRelAddress;
#else
					/* mov    eax,argc                              */ *Np++ = 0x8B;*Np++ = 0x45; *Np++ = 0x18;
#endif
        			/*** target = (Rp[Am1]+offset);               ***/
					/* mov    edx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); tarPtrID = pcodes[n];
					/* mov    dword Ptr [edx+0x11223344],eax        */ *Np++ = 0x89;*Np++ = 0x82; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    byte ptr [edx+15+0x11223344],TYNUM    */ *Np++ = 0xC6;*Np++ = 0x82; *(LpNUM)Np = (tarPtrID+15);Np+=4; *Np++ = TYNUM;
					break;

				case VMARGFETCH:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** source = (Rp[Am1]+offset);               ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); srcPtrID = pcodes[n]; 
					/* mov    ecx,dword Ptr [ecx+0x11223344]        */ *Np++ = 0x8B;*Np++ = 0x89;*(LpNUM)Np = srcPtrID;Np+=4;
					/* shl    ecx,4								    */ *Np++ = 0xC1;*Np++ = 0xE1;*Np++ = 0x04;
					/* add    ecx,dword ptr [argv]				    */ *Np++ = 0x03;*Np++ = 0x4D;*Np++ = 0x1C;
        			/*** target = (Rp[Am2]+offset);               ***/
					/* mov    edx,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [ecx+0]                 */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = 0;Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); tarPtrID = pcodes[n];
					/* mov    dword Ptr [edx+0x11223344],eax        */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    eax,dword Ptr [ecx+4]                 */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (4);Np+=4;
					/* mov    dword Ptr [edx+4+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+4);Np+=4;
					/* mov    eax,dword Ptr [ecx+8]                 */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (8);Np+=4;
					/* mov    dword Ptr [edx+8+0x11223344],eax      */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+8);Np+=4;
					/* mov    eax,dword Ptr [ecx+12]                */ *Np++ = 0x8B;*Np++ = 0x81;*(LpNUM)Np = (12);Np+=4;
					/* mov    dword Ptr [edx+12+0x11223344],eax     */ *Np++ = 0x89;*Np++ = 0x82;*(LpNUM)Np = (tarPtrID+12);Np+=4;
					break;

				case VMIADD:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* add    eax,dword ptr [ecx]	                */ *Np++ = 0x03;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYNUM               */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;

					break;


				case VMUADD:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* add    eax,dword ptr [ecx]	                */ *Np++ = 0x03;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYUNUM              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYUNUM;

					break;

				case VMCADD:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* add    eax,dword ptr [ecx]	                */ *Np++ = 0x03;*Np++ = 0x01;
					/* and    eax,0x000000FF                        */ *Np++ = 0x25;*Np++ = 0xFF;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYCHAR              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYCHAR;
					break;

					/* fld word ptr [ecx+0x11223344]   */ *Np++ = 0xD9; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fstp qword ptr [edx+0x11223344] */ *Np++ = 0xDD; *Np++ = 0x9A; *(LpNUM)Np = pcodes[n];Np+=4;

				case VMNADD:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* fld    qword ptr [eax]                       */ *Np++ = 0xDD;*Np++ = 0x00;
					/* fadd   qword ptr [ecx]                       */ *Np++ = 0xDC;*Np++ = 0x01;
					/* fstp   qword ptr [edx]                       */ *Np++ = 0xDD;*Np++ = 0x1A;
					/* mov    byte ptr [edx+15],TYREAL              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYREAL;
					break;

				case VMISUB:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* sub    eax,dword ptr [ecx]                   */ *Np++ = 0x2B;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYNUM               */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
					break;


				case VMUSUB:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* sub    eax,dword ptr [ecx]                   */ *Np++ = 0x2B;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYUNUM              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYUNUM;
					break;

				case VMCSUB:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* sub    eax,dword ptr [ecx]                   */ *Np++ = 0x2B;*Np++ = 0x01;
					/* and    eax,0x000000FF                        */ *Np++ = 0x25;*Np++ = 0xFF;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYCHAR              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYCHAR;
					break;

				case VMNSUB:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* fld    qword ptr [eax]                       */ *Np++ = 0xDD;*Np++ = 0x00;
					/* fsub   qword ptr [ecx]                       */ *Np++ = 0xDC;*Np++ = 0x21;
					/* fstp   qword ptr [edx]                       */ *Np++ = 0xDD;*Np++ = 0x1A;
					/* mov    byte ptr [edx+15],TYREAL              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYREAL;
					break;

				case VMIAND:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* and    eax,dword ptr [ecx]                   */ *Np++ = 0x23;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYNUM               */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
					break;

				case vmnatAndInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* and    eax,dword ptr [ecx]                   */ *Np++ = 0x23;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					break;

				case VMIANDB:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* and    eax,dword ptr [ecx]                   */ *Np++ = 0x23;*Np++ = 0x01;
					/* neg    eax                                   */ *Np++ = 0xF7;*Np++ = 0xD8;
					/* sbb    eax,eax                               */ *Np++ = 0x1B;*Np++ = 0xC0;
					/* inc    eax                                   */ *Np++ = 0x40;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYNUM               */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
					break;

				case VMIOR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* or     eax,dword ptr [ecx]                   */ *Np++ = 0x0B;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYNUM               */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
					break;

				case vmnatOrInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* or     eax,dword ptr [ecx]                   */ *Np++ = 0x0B;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					break;

				case VMIORB:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* or     eax,dword ptr [ecx]                   */ *Np++ = 0x0B;*Np++ = 0x01;
					/* neg    eax                                   */ *Np++ = 0xF7;*Np++ = 0xD8;
					/* sbb    eax,eax                               */ *Np++ = 0x1B;*Np++ = 0xC0;
					/* inc    eax                                   */ *Np++ = 0x40;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYNUM               */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
					break;

				case VMIXOR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* xor    eax,dword ptr [ecx]                   */ *Np++ = 0x33;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYNUM               */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
					break;

				case vmnatXorInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* xor    eax,dword ptr [ecx]                   */ *Np++ = 0x33;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					break;

				case vmnatShlInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov    ecx,dword ptr [ecx+0x11223344]        */ *Np++ = 0x8B;*Np++ = 0x89;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov    eax,dword ptr [eax+0x11223344]        */ *Np++ = 0x8B;*Np++ = 0x80;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** target = (Rp[Am3]+offset);               ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/* shl    eax,cl								*/ *Np++ = 0xD3; *Np++ = 0xE0;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov    dword ptr [edx+0x11223344],eax        */ *Np++ = 0x89; *Np++ = 0x82;*(LpNUM)Np = pcodes[n];Np+=4;
					break;

				case vmnatShrInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov    ecx,dword ptr [ecx+0x11223344]        */ *Np++ = 0x8B;*Np++ = 0x89;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov    eax,dword ptr [eax+0x11223344]        */ *Np++ = 0x8B;*Np++ = 0x80;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** target = (Rp[Am3]+offset);               ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/* shr    eax,cl								*/ *Np++ = 0xD3; *Np++ = 0xE8;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov    dword ptr [edx+0x11223344],eax        */ *Np++ = 0x89; *Np++ = 0x82;*(LpNUM)Np = pcodes[n];Np+=4;
					break;

				case VMIXORB:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* xor    eax,dword ptr [ecx]                   */ *Np++ = 0x33;*Np++ = 0x01;
					/* neg    eax                                   */ *Np++ = 0xF7;*Np++ = 0xD8;
					/* sbb    eax,eax                               */ *Np++ = 0x1B;*Np++ = 0xC0;
					/* inc    eax                                   */ *Np++ = 0x40;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYNUM               */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
					break;

				case VMIDIV:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* cdq                        		            */ *Np++ = 0x99;
					/* idiv   dword ptr [ecx]		                */ *Np++ = 0xF7;*Np++ = 0x39;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYNUM               */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
					break;


				case VMUDIV:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* cdq                        		            */ *Np++ = 0x99;
					/* idiv   dword ptr [ecx]		                */ *Np++ = 0xF7;*Np++ = 0x39;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYUNUM              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYUNUM;
					break;

				case VMCDIV:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* cdq                        		            */ *Np++ = 0x99;
					/* idiv   dword ptr [ecx]		                */ *Np++ = 0xF7;*Np++ = 0x39;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* and    eax,0x000000FF                        */ *Np++ = 0x25;*Np++ = 0xFF;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYCHAR              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYCHAR;
					break;

				case VMNDIV:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* fld    qword ptr [eax]                       */ *Np++ = 0xDD;*Np++ = 0x00;
					/* fdiv   qword ptr [ecx]		                */ *Np++ = 0xDC;*Np++ = 0x31;
					/* fstp   qword ptr [edx]                       */ *Np++ = 0xDD;*Np++ = 0x1A;
					/* mov    byte ptr [edx+15],TYREAL              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYREAL;
					break;

				case VMIDIVR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* cdq                        		            */ *Np++ = 0x99;
					/* idiv   dword ptr [ecx]		                */ *Np++ = 0xF7;*Np++ = 0x39;
					/* mov    eax,edx	                            */ *Np++ = 0x8B;*Np++ = 0xC2;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYNUM               */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
					break;

					
				case VMUDIVR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* cdq                        		            */ *Np++ = 0x99;
					/* idiv   dword ptr [ecx]		                */ *Np++ = 0xF7;*Np++ = 0x39;
					/* mov    eax,edx	                            */ *Np++ = 0x8B;*Np++ = 0xC2;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYUNUM              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYUNUM;
					break;

				case VMIMUL:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* imul   eax,dword ptr [ecx]	                */ *Np++ = 0x0F;*Np++ = 0xAF;*Np++ = 0x01;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYNUM               */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
					break;


				case VMUMUL:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* imul   eax,dword ptr [ecx]	                */ *Np++ = 0x0F;*Np++ = 0xAF;*Np++ = 0x01;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYUNUM              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYUNUM;
					break;

				case VMCMUL:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* imul   eax,dword ptr [ecx]	                */ *Np++ = 0x0F;*Np++ = 0xAF;*Np++ = 0x01;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* and    eax,0x000000FF                        */ *Np++ = 0x25;*Np++ = 0xFF;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					/* mov    byte ptr [edx+15],TYCHAR              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYCHAR;
					break;

				case VMNMUL:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* fld    qword ptr [eax]                       */ *Np++ = 0xDD;*Np++ = 0x00;
					/* fmul   qword ptr [ecx]		                */ *Np++ = 0xDC;*Np++ = 0x09;
					/* fstp   qword ptr [edx]                       */ *Np++ = 0xDD;*Np++ = 0x1A;
					/* mov    byte ptr [edx+15],TYREAL              */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYREAL;
					break;

				case vmnatAddInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* add    eax,dword ptr [ecx]	                */ *Np++ = 0x03;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					break;

				case vmnatAddNumber:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* fld    qword ptr [eax]                       */ *Np++ = 0xDD;*Np++ = 0x00;
					/* fadd   qword ptr [ecx]                       */ *Np++ = 0xDC;*Np++ = 0x01;
					/* fstp   qword ptr [edx]                       */ *Np++ = 0xDD;*Np++ = 0x1A;
					break;

				case vmnatSubInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* sub    eax,dword ptr [ecx]                   */ *Np++ = 0x2B;*Np++ = 0x01;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					break;

				case vmnatSubNumber:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* fld    qword ptr [eax]                       */ *Np++ = 0xDD;*Np++ = 0x00;
					/* fsub   qword ptr [ebx]                       */ *Np++ = 0xDC;*Np++ = 0x21;
					/* fstp   qword ptr [edx]                       */ *Np++ = 0xDD;*Np++ = 0x1A;
					break;

				case vmnatDivInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* cdq                        		            */ *Np++ = 0x99;
					/* idiv   dword ptr [ecx]		                */ *Np++ = 0xF7;*Np++ = 0x39;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					break;

				case vmnatDivNumber:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* fld    qword ptr [eax]                       */ *Np++ = 0xDD;*Np++ = 0x00;
					/* fdiv   qword ptr [ecx]		                */ *Np++ = 0xDC;*Np++ = 0x31;
					/* fstp   qword ptr [edx]                       */ *Np++ = 0xDD;*Np++ = 0x1A;
					break;

				case vmnatDivrInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* cdq                        		            */ *Np++ = 0x99;
					/* idiv   dword ptr [ecx]		                */ *Np++ = 0xF7;*Np++ = 0x39;
					/* mov    eax,edx	                            */ *Np++ = 0x8B;*Np++ = 0xC2;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					break;

				case vmnatMulInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    eax,dword ptr [eax]                   */ *Np++ = 0x8B;*Np++ = 0x00;
					/* imul   eax,dword ptr [ecx]	                */ *Np++ = 0x0F;*Np++ = 0xAF;*Np++ = 0x01;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov    dword ptr [edx],eax                   */ *Np++ = 0x89;*Np++ = 0x02;
					break;

				case vmnatMulNumber:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
        			/*** argument = (Rp[Am1]+offset);             ***/
					/* mov    ecx,dword ptr Rp[Am1]                 */ *Np++ = 0x8B;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    ecx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC1;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/*** source = (Rp[Am2]+offset);               ***/
					/* mov    eax,dword ptr Rp[Am2]                 */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+1);
					/* add    eax,0x11223344                        */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
        			/* target = (Rp[Am3]+offset);                 ***/
					/* mov    edx,dword ptr Rp[Am3]                 */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
					/* add    edx,0x11223344                        */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
					/* fld    qword ptr [eax]                       */ *Np++ = 0xDD;*Np++ = 0x00;
					/* fmul   qword ptr [ecx]		                */ *Np++ = 0xDC;*Np++ = 0x09;
					/* fstp   qword ptr [edx]                       */ *Np++ = 0xDD;*Np++ = 0x1A;
					break;

				case vmnatLoadCharacter:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment      ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]               */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/* sub eax,eax                   */ *Np++ = 0x2B; *Np++ = 0xC0; 
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov al,[ecx+0x11223344]       */ *Np++ = 0x8A; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[Am2]               */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* mov [edx+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov [edx+15+0x11223344],TYNUM */ *Np++ = 0xC6; *Np++ = 0x82; *(LpNUM)Np = (pcodes[n]+15); Np+=4; *Np++ = TYNUM;
					break;

				case vmnatLoadShort:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment      ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]               */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/* sub eax,eax                   */ *Np++ = 0x2B; *Np++ = 0xC0; 
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ax,[ecx+0x11223344]       */ *Np++ = 0x66; *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* cwde			                 */ *Np++ = 0x98;
					/* mov edx,Rp[Am2]               */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* mov [edx+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov [edx+15+0x11223344],TYNUM */ *Np++ = 0xC6; *Np++ = 0x82; *(LpNUM)Np = (pcodes[n]+15); Np+=4; *Np++ = TYNUM;
					break;

				case vmnatLoadInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment      ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]               */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov eax,[ecx+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[Am2]               */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* mov [edx+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov [edx+15+0x11223344],TYNUM */ *Np++ = 0xC6; *Np++ = 0x82; *(LpNUM)Np = (pcodes[n]+15); Np+=4; *Np++ = TYNUM;
					break;

				case vmnatLoadUInteger:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment      ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]               */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov eax,[ecx+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[Am2]               */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* mov [edx+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov [edx+15+0x11223344],TYUNUM*/ *Np++ = 0xC6; *Np++ = 0x82; *(LpNUM)Np = (pcodes[n]+15); Np+=4; *Np++ = TYUNUM;
					break;

				case vmnatLoadLong:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment      ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]               */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov eax,[ecx+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[Am2]               */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* mov [edx+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov [edx+15+0x11223344],TYNUM */ *Np++ = 0xC6; *Np++ = 0x82; *(LpNUM)Np = (pcodes[n]+15); Np+=4; *Np++ = TYNUM;
					break;

				case vmnatLoadFloat:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment        ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]                 */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* fld word ptr [ecx+0x11223344]   */ *Np++ = 0xD9; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fstp qword ptr [edx+0x11223344] */ *Np++ = 0xDD; *Np++ = 0x9A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov [edx+15+0x11223344],TYREAL  */ *Np++ = 0xC6; *Np++ = 0x82; *(LpNUM)Np = (pcodes[n]+15); Np+=4; *Np++ = TYREAL;
					break;

				case vmnatLoadNumber:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment        ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]                 */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); srcPtrID = pcodes[n];  
					/* mov eax,[ecx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = srcPtrID; Np+=4;
					/* mov edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); tarPtrID = pcodes[n]; 
					/* mov [edx+0x11223344],eax        */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov eax,[ecx+4+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = srcPtrID+4; Np+=4;
					/* mov [edx+4+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = tarPtrID+4;Np+=4;
					/* mov [edx+15+0x11223344],TYREAL  */ *Np++ = 0xC6; *Np++ = 0x82; *(LpNUM)Np = (pcodes[n]+15); Np+=4; *Np++ = TYREAL;
					break;

				case vmnatLoadObject:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment      ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]               */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov eax,[ecx+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* mov [edx+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov ax,[eax+4]                */ *Np++ = 0x66; *Np++ = 0x8B; *Np++ = 0x40; *Np++ = 0x04; 
					/* mov [edx+15+0x11223344],al    */ *Np++ = 0x88; *Np++ = 0x82; *(LpNUM)Np = (pcodes[n]+15); Np+=4;
					break;

				case vmnatSaveInteger:
					//outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					if (autoHardwareMode == TRUE) goto ErrorRunInHardwareIllegalInstruction;
					if (Vr[HARDWARESW] == TRUE) goto ErrorRunInHardwareIllegalInstruction;
                    if ((NUM)(Np-ncodes) >= nativeLen) goto ResizeNativeCodeVector;
					/*** Set Pc/Nc allignment      ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]               */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov eax,[ecx+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[Am2]               */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* mov [edx+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = pcodes[n];Np+=4;
					break;

				case vmnatSaveLong:
					//outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					if (autoHardwareMode == TRUE) goto ErrorRunInHardwareIllegalInstruction;
					if (Vr[HARDWARESW] == TRUE) goto ErrorRunInHardwareIllegalInstruction;
                    if ((NUM)(Np-ncodes) >= nativeLen) goto ResizeNativeCodeVector;
					/*** Set Pc/Nc allignment      ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]               */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov eax,[ecx+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[Am2]               */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* mov [edx+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = pcodes[n];Np+=4;
					break;

				case vmnatSaveNumber:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment        ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]                 */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); srcPtrID = pcodes[n];  
					/* mov eax,[ecx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = srcPtrID; Np+=4;
					/* mov edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); tarPtrID = pcodes[n]; 
					/* mov [edx+0x11223344],eax        */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov eax,[ecx+4+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = srcPtrID+4; Np+=4;
					/* mov [edx+4+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = tarPtrID+4;Np+=4;
					break;

				case vmnatSaveObject:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment      ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]               */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov al,[ecx+15+0x11223344]    */ *Np++ = 0x8A; *Np++ = 0x81; *(LpNUM)Np = (pcodes[n]+15); Np+=4;
					/* cmp al,TYVOID                 */ *Np++ = 0x3C; *Np++ = TYVOID; 
					/* jne VMNATSAVEOBJ              */ *Np++ = 0x75; *Np++ = 0x04; 
					/* sub eax,eax                   */ *Np++ = 0x2B; *Np++ = 0xC0; 
					/* jmp VMNATSAVECNT              */ *Np++ = 0xEB; *Np++ = 0x06; 
					/*** VMNATSAVEOBJ:             ***/ 
					/* mov eax,[ecx+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/*** VMNATSAVECNT:             ***/ 
					/* mov edx,Rp[Am2]               */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* mov [edx+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = pcodes[n];Np+=4;
					break;

				case vmnatSaveFloat:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment        ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]                 */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* fld qword ptr [ecx+0x11223344]  */ *Np++ = 0xDD; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fstp dword ptr [edx+0x11223344] */ *Np++ = 0xD9; *Np++ = 0x9A; *(LpNUM)Np = pcodes[n];Np+=4;
					break;

				case vmnatSaveShort:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment      ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]               */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov eax,[ecx+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[Am2]               */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* mov [edx+0x11223344],ax       */ *Np++ = 0x66; *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = pcodes[n];Np+=4;
					break;

				case vmnatSaveCharacter:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment      ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[Am1]               */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov eax,[ecx+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[Am2]               */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment      ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* mov [edx+0x11223344],al       */ *Np++ = 0x88; *Np++ = 0x82; *(LpNUM)Np = pcodes[n];Np+=4;
					break;

				/* Register opcodes with NO jump label arguments. */
				case VMRETURN:
					regOutStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					Jp[n]   = (NUM)(Np-ncodes);       /*** Set Pc/Nc allignment							***/ 
					SAVEALLREGS
					saveNativeJumpBack(pcode);	      /*** Move the "call" to the native code vector.	***/
					goto JITOpWrdContinue;
					break;

				case VMMOVE:
					regOutStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                         ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment						  ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment						  ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					SAVEALLREGS
					/* mov    ecx,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [ecx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = srcPtrID;Np+=4;
					/* mov    dword Ptr [edx+0x11223344],eax        */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    eax,dword Ptr [ecx+4+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = (srcPtrID+4);Np+=4;
					/* mov    dword Ptr [edx+4+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = (tarPtrID+4);Np+=4;
					/* mov    eax,dword Ptr [ecx+8+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = (srcPtrID+8);Np+=4;
					/* mov    dword Ptr [edx+8+0x11223344],eax      */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = (tarPtrID+8);Np+=4;
					/* mov    eax,dword Ptr [ecx+12+0x11223344]     */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = (srcPtrID+12);Np+=4;
					/* mov    dword Ptr [edx+12+0x11223344],eax     */ *Np++ = 0x89; *Np++ = 0x82; *(LpNUM)Np = (tarPtrID+12);Np+=4;
					LOADALLREGS
					break;

				case VMSETMATRIX:
				case VMSETVECTOR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); argPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					/* mov    edx,dword ptr Rp[index]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x92; *(LpNUM)Np = argPtrID;Np+=4;
					/* shl    edx,4									*/ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = 0x04; 
					/* mov    ecx,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/*** Load address for &target[index]	      ***/ 
					/* mov    eax,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [eax+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x80; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    eax,dword ptr [eax+12]				*/ *Np++ = 0x8B;*Np++ = 0x40;*Np++ = 0x0C;
					/* mov    eax,dword ptr [eax]					*/ *Np++ = 0x8B;*Np++ = 0x00;
					/* add    edx,eax								*/ *Np++ = 0x03;*Np++ = 0xD0;
					/*** target[index] = source                   ***/ 
					/* mov    eax,dword Ptr [ecx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = srcPtrID;Np+=4;
					/* mov    dword Ptr [edx],eax					*/ *Np++ = 0x89; *Np++ = 0x02;
					/* mov    eax,dword Ptr [ecx+4+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = (srcPtrID+4);Np+=4;
					/* mov    dword Ptr [edx+4],eax					*/ *Np++ = 0x89; *Np++ = 0x42; *Np++ = 0x04;
					/* mov    eax,dword Ptr [ecx+8+0x11223344]      */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = (srcPtrID+8);Np+=4;
					/* mov    dword Ptr [edx+8],eax					*/ *Np++ = 0x89; *Np++ = 0x42; *Np++ = 0x08;
					/* mov    eax,dword Ptr [ecx+12+0x11223344]     */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = (srcPtrID+12);Np+=4;
					/* mov    dword Ptr [edx+12],eax				*/ *Np++ = 0x89; *Np++ = 0x42; *Np++ = 0x0C;
					break;

				case VMREFMATRIX:
				case VMREFVECTOR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); argPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					/* mov    edx,dword ptr Rp[index]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x92; *(LpNUM)Np = argPtrID;Np+=4;
					/* shl    edx,4									*/ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = 0x04; 
					/* mov    ecx,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
					/*** Load address for &source[index]	      ***/ 
					/* mov    eax,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [eax+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x80; *(LpNUM)Np = srcPtrID;Np+=4;
					/* mov    eax,dword ptr [eax+12]				*/ *Np++ = 0x8B;*Np++ = 0x40;*Np++ = 0x0C;
					/* mov    eax,dword ptr [eax]					*/ *Np++ = 0x8B;*Np++ = 0x00;
					/* add    edx,eax								*/ *Np++ = 0x03;*Np++ = 0xD0;
					/*** target = source[index]                   ***/ 
					/* mov    eax,dword Ptr [edx]				    */ *Np++ = 0x8B; *Np++ = 0x02;
					/* mov    dword Ptr [ecx+0x11223344],eax		*/ *Np++ = 0x89; *Np++ = 0x81; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    eax,dword Ptr [edx+4]				    */ *Np++ = 0x8B; *Np++ = 0x42; *Np++ = 0x04;
					/* mov    dword Ptr [ecx+4+0x11223344],eax		*/ *Np++ = 0x89; *Np++ = 0x81; *(LpNUM)Np = (tarPtrID+4);Np+=4;
					/* mov    eax,dword Ptr [edx+8]				    */ *Np++ = 0x8B; *Np++ = 0x42; *Np++ = 0x08;
					/* mov    dword Ptr [ecx+8+0x11223344],eax		*/ *Np++ = 0x89; *Np++ = 0x81; *(LpNUM)Np = (tarPtrID+8);Np+=4;
					/* mov    eax,dword Ptr [edx+12]				*/ *Np++ = 0x8B; *Np++ = 0x42; *Np++ = 0x0C;
					/* mov    dword Ptr [ecx+12+0x11223344],eax		*/ *Np++ = 0x89; *Np++ = 0x81; *(LpNUM)Np = (tarPtrID+12);Np+=4;
					break;

				case VMSETINTVECTOR:
				case VMSETPCDVECTOR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); argPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					/* mov    edx,dword ptr Rp[index]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x92; *(LpNUM)Np = argPtrID;Np+=4;
					/* shl    edx,2									*/ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = 0x02; 
					/* mov    ecx,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/*** Load address for &target[index]	      ***/ 
					/* mov    eax,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [eax+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x80; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    eax,dword ptr [eax+12]				*/ *Np++ = 0x8B;*Np++ = 0x40;*Np++ = 0x0C;
					/* mov    eax,dword ptr [eax]					*/ *Np++ = 0x8B;*Np++ = 0x00;
					/* add    edx,eax								*/ *Np++ = 0x03;*Np++ = 0xD0;
					/*** target[index] = source                   ***/ 
					/* mov    eax,dword Ptr [ecx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = srcPtrID;Np+=4;
					/* mov    dword Ptr [edx],eax					*/ *Np++ = 0x89; *Np++ = 0x02;
					break;
					
				case VMREFINTVECTOR:
				case VMREFPCDVECTOR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); argPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					/* mov    edx,dword ptr Rp[index]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x92; *(LpNUM)Np = argPtrID;Np+=4;
					/* shl    edx,2									*/ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = 0x02; 
					/* mov    ecx,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
					/*** Load address for &source[index]	      ***/ 
					/* mov    eax,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [eax+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x80; *(LpNUM)Np = srcPtrID;Np+=4;
					/* mov    eax,dword ptr [eax+12]				*/ *Np++ = 0x8B;*Np++ = 0x40;*Np++ = 0x0C;
					/* mov    eax,dword ptr [eax]					*/ *Np++ = 0x8B;*Np++ = 0x00;
					/* add    edx,eax								*/ *Np++ = 0x03;*Np++ = 0xD0;
					/*** target = source[index]                   ***/ 
					/* mov    eax,dword Ptr [edx]				    */ *Np++ = 0x8B; *Np++ = 0x02;
					/* mov    dword Ptr [ecx+0x11223344],eax		*/ *Np++ = 0x89; *Np++ = 0x81; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    byte Ptr [ecx+15+0x11223344],TYNUM 	*/ *Np++ = 0xC6; *Np++ = 0x81; *(LpNUM)Np = (tarPtrID+15);Np+=4; *Np++ = TYNUM; 
					break;

				case VMSETBYTVECTOR:
				case VMSETSTRING:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); argPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					/* mov    edx,dword ptr Rp[index]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x92; *(LpNUM)Np = argPtrID;Np+=4;
					/* mov    ecx,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/*** Load address for &target[index]	      ***/ 
					/* mov    eax,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [eax+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x80; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    eax,dword ptr [eax+12]				*/ *Np++ = 0x8B;*Np++ = 0x40;*Np++ = 0x0C;
					/* mov    eax,dword ptr [eax]					*/ *Np++ = 0x8B;*Np++ = 0x00;
					/* add    edx,eax								*/ *Np++ = 0x03;*Np++ = 0xD0;
					/*** target[index] = source                   ***/ 
					/* mov    eax,dword Ptr [ecx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x81; *(LpNUM)Np = srcPtrID;Np+=4;
					/* mov    byte Ptr [edx],eax					*/ *Np++ = 0x88; *Np++ = 0x02;
					break;

				case VMREFBYTVECTOR:
				case VMREFSTRING:
				case VMREFSYMBOL:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); argPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					/* mov    edx,dword ptr Rp[index]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x92; *(LpNUM)Np = argPtrID;Np+=4;
					/* mov    ecx,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
					/*** Load address for &source[index]	      ***/ 
					/* mov    eax,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [eax+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x80; *(LpNUM)Np = srcPtrID;Np+=4;
					/* mov    eax,dword ptr [eax+12]				*/ *Np++ = 0x8B;*Np++ = 0x40;*Np++ = 0x0C;
					/* mov    eax,dword ptr [eax]					*/ *Np++ = 0x8B;*Np++ = 0x00;
					/* add    edx,eax								*/ *Np++ = 0x03;*Np++ = 0xD0;
					/*** target = source[index]                   ***/ 
					/* movsx  eax,byte ptr [edx]					*/ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = 0x02;
					/* mov    dword Ptr [ecx+0x11223344],eax		*/ *Np++ = 0x89; *Np++ = 0x81; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    byte Ptr [ecx+15+0x11223344],TYNUM 	*/ *Np++ = 0xC6; *Np++ = 0x81; *(LpNUM)Np = (tarPtrID+15);Np+=4; *Np++ = TYNUM; 
					break;

				case VMREFTEXT:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); argPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					/* mov    edx,dword ptr Rp[index]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x92; *(LpNUM)Np = argPtrID;Np+=4;
					/* mov    ecx,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
					/*** Load address for &source[index]	      ***/ 
					/* mov    eax,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/* add    eax,0x11223344				        */ *Np++ = 0x05; *(LpNUM)Np = srcPtrID;Np+=4;
					/* add    edx,eax								*/ *Np++ = 0x03; *Np++ = 0xD0;
					/*** target = source[index]                   ***/ 
					/* movsx  eax,byte ptr [edx]					*/ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = 0x02;
					/* mov    dword Ptr [ecx+0x11223344],eax		*/ *Np++ = 0x89; *Np++ = 0x81; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    byte Ptr [ecx+15+0x11223344],TYNUM  	*/ *Np++ = 0xC6; *Np++ = 0x81; *(LpNUM)Np = (tarPtrID+15);Np+=4; *Np++ = TYNUM; 
					break;

				case VMSETNUMMATRIX:
				case VMSETNUMVECTOR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); argPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					/* mov    edx,dword ptr Rp[index]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x92; *(LpNUM)Np = argPtrID;Np+=4;
					/* mov    ecx,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/*** Load address for &target[index]	      ***/ 
					/* mov    eax,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [eax+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x80; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    eax,dword ptr [eax+12]				*/ *Np++ = 0x8B;*Np++ = 0x40;*Np++ = 0x0C;
					/* mov    eax,dword ptr [eax]					*/ *Np++ = 0x8B;*Np++ = 0x00;
					/*** target[index] = source                   ***/ 
					/* fld    qword Ptr [ecx+0x11223344]			*/ *Np++ = 0xDD; *Np++ = 0x81; *(LpNUM)Np = srcPtrID;Np+=4;
					/* fstp   qword Ptr [eax+(edx*8)]				*/ *Np++ = 0xDD; *Np++ = 0x1C; *Np++ = 0xD0;
					break;

				case VMREFNUMMATRIX:
				case VMREFNUMVECTOR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); argPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					/* mov    edx,dword ptr Rp[index]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x92; *(LpNUM)Np = argPtrID;Np+=4;
					/* mov    ecx,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
					/*** Load address for &source[index]	      ***/ 
					/* mov    eax,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [eax+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x80; *(LpNUM)Np = srcPtrID;Np+=4;
					/* mov    eax,dword ptr [eax+12]				*/ *Np++ = 0x8B; *Np++ = 0x40; *Np++ = 0x0C;
					/* mov    eax,dword ptr [eax]					*/ *Np++ = 0x8B; *Np++ = 0x00;
					/*** target = source[index]                   ***/ 
					/* fld    qword Ptr [eax+(edx*8)]				*/ *Np++ = 0xDD; *Np++ = 0x04; *Np++ = 0xD0;
					/* fstp   qword Ptr [ecx+0x11223344]			*/ *Np++ = 0xDD; *Np++ = 0x99; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    byte Ptr [ecx+15+0x11223344],TYREAL 	*/ *Np++ = 0xC6; *Np++ = 0x81; *(LpNUM)Np = (tarPtrID+15);Np+=4; *Np++ = TYREAL;
					break;

				case VMSETFLTVECTOR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); argPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					/* mov    edx,dword ptr Rp[index]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x92; *(LpNUM)Np = argPtrID;Np+=4;
					/* shl    edx,2									*/ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = 0x02; 
					/* mov    ecx,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/*** Load address for &target[index]	      ***/ 
					/* mov    eax,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [eax+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x80; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    eax,dword ptr [eax+12]				*/ *Np++ = 0x8B; *Np++ = 0x40; *Np++ = 0x0C;
					/* mov    eax,dword ptr [eax]					*/ *Np++ = 0x8B; *Np++ = 0x00;
					/* add    edx,eax								*/ *Np++ = 0x03; *Np++ = 0xD0;
					/*** target[index] = source                   ***/ 
					/* fld    qword Ptr [ecx+0x11223344]			*/ *Np++ = 0xDD; *Np++ = 0x81; *(LpNUM)Np = srcPtrID;Np+=4;
					/* fstp   dword Ptr [edx]						*/ *Np++ = 0xD9; *Np++ = 0x1A;
					break;
					
				case VMREFFLTVECTOR:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); argPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); srcPtrID = pcodes[n];
					/*** Set Pc/Nc allignment                     ***/ Jp[++n] = (NUM)(Np-ncodes); tarPtrID = pcodes[n];
					/* mov    edx,dword ptr Rp[index]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    edx,dword Ptr [edx+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x92; *(LpNUM)Np = argPtrID;Np+=4;
					/* shl    edx,2									*/ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = 0x02; 
					/* mov    ecx,dword ptr Rp[target]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
					/*** Load address for &source[index]	      ***/ 
					/* mov    eax,dword ptr Rp[source]              */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
					/* mov    eax,dword Ptr [eax+0x11223344]        */ *Np++ = 0x8B; *Np++ = 0x80; *(LpNUM)Np = srcPtrID;Np+=4;
					/* mov    eax,dword ptr [eax+12]				*/ *Np++ = 0x8B; *Np++ = 0x40; *Np++ = 0x0C;
					/* mov    eax,dword ptr [eax]					*/ *Np++ = 0x8B; *Np++ = 0x00;
					/* add    edx,eax								*/ *Np++ = 0x03; *Np++ = 0xD0;
					/*** target = source[index]                   ***/ 
					/* fld    dword Ptr [edx]						*/ *Np++ = 0xD9; *Np++ = 0x02;
					/* fstp   qword Ptr [ecx+0x11223344]			*/ *Np++ = 0xDD; *Np++ = 0x99; *(LpNUM)Np = tarPtrID;Np+=4;
					/* mov    byte Ptr [ecx+15+0x11223344],TYREAL 	*/ *Np++ = 0xC6; *Np++ = 0x81; *(LpNUM)Np = (tarPtrID+15);Np+=4; *Np++ = TYREAL; 
					break;

				/* Register opcodes with NO jump label arguments (which WILL run in hardware mode). */
				case VMAPPLY:
				case VMCALL:
				case VMMOVEI:
				case VMMOVEU:
				case VMMOVEN:
				case VMONERROR:
				case VMREF:
				case VMSEND:
				case VMSET:
				case VMTESTESCAPE:
					regOutStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					Jp[n]   = (NUM)(Np-ncodes);       /*** Set Pc/Nc allignment							***/ 
					SAVEALLREGS
					
					saveNativeJumpBack(pcode);	      /*** Move the "call" to the native code vector.	***/

					/*  Loop through the modifier patterns for arguments which need */
					/*  to be marked. */
					for (modIndex = 0; modIndex < 3; ++modIndex)
						{
						switch (modifier[modIndex])
							{
							case AMVOID:
								break;
            
							case AMGVOFFSET:
							case AMSVOFFSET:
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
							case AMINTEGER:
								Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
								*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM pcode argument to the native code vector. */
								break;
                                       
							default:
								goto JIT_IllegalInstruction;
								break;
							}       
						}

					LOADALLREGS
					break;
				
				/* Register opcodes with NO jump label arguments (which will NOT run in hardware mode). */
				case VMADD:
				case VMADDI:
				case VMADDU:
				case VMADDN:
				case VMAND:
				case VMDIV:
				case VMDIVI:
				case VMDIVU:
				case VMDIVN:
				case VMDIVR:
				case VMDIVRI:
				case VMDIVRU:
				case VMMUL:
				case VMMULI:
				case VMMULU:
				case VMMULN:
				case VMNDIVR:
				case VMOR:
				case VMSHL:
				case VMSHR:
				case VMSUB:
				case VMSUBI:
				case VMSUBU:
				case VMSUBN:
				case VMXOR:
				case VMREFSTRVALUE:
				case VMSETSTRVALUE:
				case VMREFSTRKEY:
				case VMSETSTRKEY:
				case VMREFDICVALUE:
				case VMSETDICVALUE:
				case VMREFDICKEY:
				case VMSETDICKEY:
				case VMREFDIRVALUE:
				case VMSETDIRVALUE:
				case VMREFDIRKEY:
				case VMSETDIRKEY:
				case VMREFBITVECTOR:
				case VMSETBITVECTOR:
				case VMREFOBJVECTOR:
				case VMSETOBJVECTOR:
				case vmnatDivrNumber:
					outStream(JITOpWrd,FALSE)  // FALSE if compiling this instruction instream.
					JITOpWrd:
					Jp[n]   = (NUM)(Np-ncodes);       /*** Set Pc/Nc allignment							***/ 
					saveNativeJumpBack(pcode);	      /*** Move the "call" to the native code vector.	***/
					JITOpWrdContinue:

					/*  Loop through the modifier patterns for arguments which need */
					/*  to be marked. */
					for (modIndex = 0; modIndex < 3; ++modIndex)
						{
						switch (modifier[modIndex])
							{
							case AMVOID:
								break;
            
							case AMGVOFFSET:
							case AMSVOFFSET:
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
							case AMINTEGER:
								Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
								*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM pcode argument to the native code vector. */
								break;
                                       
							default:
								goto JIT_IllegalInstruction;
								break;
							}       
						}
					break;
								
				case vmnatJmpLTInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* jge                      */ *Np++ = 0x7D; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpLEInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* jg                       */ *Np++ = 0x7F; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpEQInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* jne                      */ *Np++ = 0x75; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpNEInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* je                       */ *Np++ = 0x74; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpGEInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* jl                       */ *Np++ = 0x7C; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpGTInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* jle                      */ *Np++ = 0x7E; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

#if 1 // MFK Note: We must change jg opcode to ja, jl opcode to jb opcode, etc.
				case vmnatJmpLTuInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* jge                      */ *Np++ = 0x7D; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpLEUInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* jg                       */ *Np++ = 0x7F; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpEQUInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* jne                      */ *Np++ = 0x75; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpNEUInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* je                       */ *Np++ = 0x74; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpGEUInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* jl                       */ *Np++ = 0x7C; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpGTUInteger:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov ecx,Rp[source]       */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* mov ecx,[ecx+0x11223344] */ *Np++ = 0x8B; *Np++ = 0x89; *(LpNUM)Np = pcodes[n]; Np+=4;
					/* mov edx,Rp[target]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* cmp ecx,[edx+0x11223344] */ *Np++ = 0x3B; *Np++ = 0x8A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* jle                      */ *Np++ = 0x7E; *Np++ = 0x05;
					/*** Set Pc/Nc allignment ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl ***/ Mp[n]   = -2;                     
					/* jmp label                */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;
#endif // MFK

				case vmnatJmpLTNumber:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov    ecx,dword ptr Rp[source]   */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fld    qword ptr [ecx+0x11223344] */ *Np++ = 0xDD; *Np++ = 0x81; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov    edx,dword ptr Rp[target]   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fcomp  qword ptr [edx+0x11223344] */ *Np++ = 0xDC; *Np++ = 0x9A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* fnstsw ax                         */ *Np++ = 0xDF; *Np++ = 0xE0; 
					/* test   ah,0x01                    */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x01; 
					/* je                                */ *Np++ = 0x74; *Np++ = 0x05;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl          ***/ Mp[n]   = -2;                     
					/* jmp label                         */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpLENumber:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov    ecx,dword ptr Rp[source]   */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fld    qword ptr [ecx+0x11223344] */ *Np++ = 0xDD; *Np++ = 0x81; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov    edx,dword ptr Rp[target]   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fcomp  qword ptr [edx+0x11223344] */ *Np++ = 0xDC; *Np++ = 0x9A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* fnstsw ax                         */ *Np++ = 0xDF; *Np++ = 0xE0; 
					/* test   ah,0x41                    */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x41; 
					/* je                                */ *Np++ = 0x74; *Np++ = 0x05;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl          ***/ Mp[n]   = -2;                     
					/* jmp label                         */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpEQNumber:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov    ecx,dword ptr Rp[source]   */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fld    qword ptr [ecx+0x11223344] */ *Np++ = 0xDD; *Np++ = 0x81; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov    edx,dword ptr Rp[target]   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fcomp  qword ptr [edx+0x11223344] */ *Np++ = 0xDC; *Np++ = 0x9A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* fnstsw ax                         */ *Np++ = 0xDF; *Np++ = 0xE0; 
					/* test   ah,0x40                    */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x40; 
					/* je                                */ *Np++ = 0x74; *Np++ = 0x05;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl          ***/ Mp[n]   = -2;                     
					/* jmp label                         */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpNENumber:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov    ecx,dword ptr Rp[source]   */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fld    qword ptr [ecx+0x11223344] */ *Np++ = 0xDD; *Np++ = 0x81; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov    edx,dword ptr Rp[target]   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fcomp  qword ptr [edx+0x11223344] */ *Np++ = 0xDC; *Np++ = 0x9A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* fnstsw ax                         */ *Np++ = 0xDF; *Np++ = 0xE0; 
					/* test   ah,0x40                    */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x40; 
					/* jne                               */ *Np++ = 0x75; *Np++ = 0x05;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl          ***/ Mp[n]   = -2;                     
					/* jmp label                         */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpGENumber:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov    ecx,dword ptr Rp[source]   */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fld    qword ptr [ecx+0x11223344] */ *Np++ = 0xDD; *Np++ = 0x81; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov    edx,dword ptr Rp[target]   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fcomp  qword ptr [edx+0x11223344] */ *Np++ = 0xDC; *Np++ = 0x9A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* fnstsw ax                         */ *Np++ = 0xDF; *Np++ = 0xE0; 
					/* test   ah,0x01                    */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x01; 
					/* jne                               */ *Np++ = 0x75; *Np++ = 0x05;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl          ***/ Mp[n]   = -2;                     
					/* jmp label                         */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmnatJmpGTNumber:
					outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* mov    ecx,dword ptr Rp[source]   */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fld    qword ptr [ecx+0x11223344] */ *Np++ = 0xDD; *Np++ = 0x81; *(LpNUM)Np = pcodes[n];Np+=4;
					/* mov    edx,dword ptr Rp[target]   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2);  
					/* fcomp  qword ptr [edx+0x11223344] */ *Np++ = 0xDC; *Np++ = 0x9A; *(LpNUM)Np = pcodes[n];Np+=4;
					/* fnstsw ax                         */ *Np++ = 0xDF; *Np++ = 0xE0; 
					/* test   ah,0x41                    */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x41; 
					/* jne                               */ *Np++ = 0x75; *Np++ = 0x05;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl          ***/ Mp[n]   = -2;                     
					/* jmp label                         */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				/* Register opcodes WITH jump label arguments. */
				case VMJMPEQ:
				case VMJMPLT:
				case VMJMPGT:
				case VMJMPNE:
				case VMJMPGE:
				case VMJMPLE:
				outStream(JITOpWrdJmpCC,FALSE)  // FALSE if compiling this instruction instream.
				JITOpWrdJmpCC:
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */

					/*  Loop through the modifier patterns for arguments which need */
					/*  to be marked. */
					for (modIndex = 0; modIndex < 3; ++modIndex)
						{
						switch (modifier[modIndex])
							{
							case AMVOID:
								break;
            
							case AMINTEGER:
								Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
								if (modIndex == 2) Mp[n] = -1; /*  Mark this jump label argument as needing translation on the second pass. */
								*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM pcode argument to the native code vector. */
								break;
                                       
							case AMGVOFFSET:
							case AMSVOFFSET:
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
								Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
								*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM pcode argument to the native code vector. */
								break;
                                       
							default:
								goto JIT_IllegalInstruction;
								break;
							}       
						}
					break;

				case VMJUMP:
					regOutStream(JITJump,FALSE)  // FALSE if compiling this instruction instream.
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					Jp[++n] = (NUM)((Np-ncodes)+1); /*  Update the jump label translation vector. */ 
					Mp[n] = -2; /*  Mark this relative jump label argument as needing translation on the second pass. */
					/* jmp label						   */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				/* Register opcodes WITH one jump label argument. */
				    JITJump:
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */

					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					Mp[n] = -1; /*  Mark this jump label argument as needing translation on the second pass. */
					*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM pcode argument to the native code vector. */
 					break;

				default:
					goto JIT_IllegalInstruction;
					break;
				}
			}
		else
		if (pcode.u.Pcode >= VMSTARTREGISTERINS)
			{
			switch (pcode.u.Pcode)
				{
				/* Register opcodes with one register argument. */
				case vmregJump:
					regOutStream(JITOp,FALSE)   // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/* jmp dword ptr Rp[target]      */ *Np++ = 0xFF; *Np++ = 0xA5; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));	Np+=4;
						}
					else
						{
						if (Ir[Rp[modifier[0]].Offset] != 0){*Np++ = 0x89; *Np++ = REGRPMOD[Rp[modifier[0]].Offset];*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;}
						/* jmp dword ptr Rp[target]      */ *Np++ = 0xFF; *Np++ = 0xA5; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));	Np+=4;
						}
					break;

				case vmregAddImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					imod[0] = 0x81; imod[1] = 0x85; imod[2] = 0xC0;
					regAddImmediate:        
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+6); 
						/* add Rp[target],immediate          */ *Np++ = imod[0]; *Np++ = imod[1]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4; *(LpNUM)Np = pcodes[n]; Np+=4;
						}
					else
					if (Rp[modifier[1]].Offset == 0)
						{
						/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+6); 
						/* add Rp[target],immediate          */ *Np++ = imod[0]; *Np++ = imod[1]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4; *(LpNUM)Np = pcodes[n]; Np+=4;
						}
					else
						{
						/*** Set Pc/Nc allignment		   ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add Reg[Am1],immediate			 */ *Np++ = imod[0]; *Np++ = (imod[2]+REGREGR[Rp[modifier[1]].Offset]); *(LpNUM)Np = pcodes[n];Np+=4;
						}
					break;

				case vmregIncPointer:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); k = pcodes[n];
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2); k = pcodes[n] << k;
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* add Rp[Am3],immediate            */ *Np++ = 0x81; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4; *(LpNUM)Np = k; Np+=4;
						}
					else
					if (Rp[modifier[2]].Offset == 0)
						{
						/*** No hardware registers		  ***/
						/* add Rp[Am3],immediate            */ *Np++ = 0x81; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4; *(LpNUM)Np = k; Np+=4;
						}
					else
						{
						/*** target in i register	      ***/
						/* add Reg[Am2],0x11223344          */ *Np++ = 0x81; *Np++ = (char)(0xC0+REGBASE[Rp[modifier[2]].Offset]); *(LpNUM)Np = k; Np+=4;
						}
					break;

				case vmregAddInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					imodCount = 2; imod[0] = 0x01; imod[1] = 0x03;
					BinaryIntegerReg:
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* mov  edx,Rp[Am1]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
						/* add  Rp[Am2],edx                 */ *Np++ = imod[0]; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
						{
						/* mov  edx,Rp[Am1]					*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
						/* add  Rp[Am2],edx                 */ *Np++ = imod[0]; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in j register 		  ***/
						/*** target in i register	      ***/
						/* add Reg[Am2],Reg[Am1]            */ *Np++ = imod[1]; *Np++ = (char)(REGREGL[Rp[modifier[1]].Offset] + REGREGR[Rp[modifier[0]].Offset]);
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in memory		 	  ***/
						/*** target in i register	      ***/
						/* add  Reg[Am2],Rp[Am1]            */ *Np++ = imod[1]; *Np++ = REGRPMOD[Rp[modifier[1]].Offset]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
						{
						/*** source in  i			 	  ***/
						/*** target in memory		      ***/
						/* add  Rp[Am2],Reg[Am1]            */ *Np++ = imod[0]; *Np++ = REGRPMOD[Rp[modifier[0]].Offset]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					break;

				case vmregSubInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					imodCount = 2; imod[0] = 0x29; imod[1] = 0x2B;
					goto BinaryIntegerReg;
					break;

				case vmregSubPointer:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* mov  edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); k = pcodes[n];
						if (k != 0)
							{
							/* shl edx,imm8                 */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = (char)k;
							}
						/* sub  Rp[Am3],edx                 */ *Np++ = 0x29; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[1]].Offset == 0) && (Rp[modifier[2]].Offset == 0))
						{
						/* mov  edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); k = pcodes[n];
						if (k != 0)
							{
							/* shl edx,imm8                 */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = (char)k;
							}
						/* sub  Rp[Am3],edx             */ *Np++ = 0x29; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[1]].Offset >= 1) && (Rp[modifier[2]].Offset >= 1))
						{
						/*** source in j register 		  ***/
						/*** target in i register	      ***/
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); k = pcodes[n];
						if (k != 0)
							{
							if (Rp[modifier[1]].Offset != 6)
								{
								/* mov  edx,Reg[Am2]        */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6] + REGREGR[Rp[modifier[1]].Offset]);
								}
							/* shl  edx,imm8                */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = (char)k;
							/* sub Reg[Am3],edx             */ *Np++ = 0x2B; *Np++ = (char)(REGREGL[Rp[modifier[2]].Offset] + REGREGR[6]);
							}
						else
							{
							/* sub Reg[Am3],Reg[Am2]        */ *Np++ = 0x2B; *Np++ = (char)(REGREGL[Rp[modifier[2]].Offset] + REGREGR[Rp[modifier[1]].Offset]);
							}
						}
					else
					if ((Rp[modifier[1]].Offset == 0) && (Rp[modifier[2]].Offset >= 1))
						{
						/*** source in memory		 	  ***/
						/*** target in i register	      ***/
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); k = pcodes[n];
						if (k != 0)
							{
							/* mov  edx,Rp[Am2]             */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							/* shl  edx,imm8                */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = (char)k;
							/* sub Reg[Am3],edx             */ *Np++ = 0x2B; *Np++ = (char)(REGREGL[Rp[modifier[2]].Offset] + REGREGR[6]);
							}
						else
							{
							/* sub  Reg[Am3],Rp[Am2]        */ *Np++ = 0x2B; *Np++ = REGRPMOD[Rp[modifier[2]].Offset]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						}
					else
					if ((Rp[modifier[1]].Offset >= 1) && (Rp[modifier[2]].Offset == 0))
						{
						/*** source in  i			 	  ***/
						/*** target in memory		      ***/
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); k = pcodes[n];
						if (k != 0)
							{
							if (Rp[modifier[1]].Offset != 6)
								{
								/* mov  edx,Reg[Am2]        */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6] + REGREGR[Rp[modifier[1]].Offset]);
								}
							/* shl  edx,imm8                */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = (char)k;
							/* sub Rp[Am3],edx              */ *Np++ = 0x29; *Np++ = REGRPMOD[6]; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
							{
							/* sub  Rp[Am3],Reg[Am2]        */ *Np++ = 0x29; *Np++ = REGRPMOD[Rp[modifier[1]].Offset]; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
							}
						}
					break;

				case vmregAddPointer:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* mov  edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); k = pcodes[n];
						if (k != 0)
							{
							/* shl edx,imm8                 */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = (char)k;
							}
						/* add  Rp[Am3],edx             */ *Np++ = 0x01; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[1]].Offset == 0) && (Rp[modifier[2]].Offset == 0))
						{
						/* mov  edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); k = pcodes[n];
						if (k != 0)
							{
							/* shl edx,imm8                 */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = (char)k;
							}
						/* add  Rp[Am3],edx             */ *Np++ = 0x01; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[1]].Offset >= 1) && (Rp[modifier[2]].Offset >= 1))
						{
						/*** source in j register 		  ***/
						/*** target in i register	      ***/
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); k = pcodes[n];
						if (k != 0)
							{
							if (Rp[modifier[1]].Offset != 6)
								{
								/* mov  edx,Reg[Am2]        */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6] + REGREGR[Rp[modifier[1]].Offset]);
								}
							/* shl  edx,imm8                */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = (char)k;
							/* add Reg[Am3],edx             */ *Np++ = 0x03; *Np++ = (char)(REGREGL[Rp[modifier[2]].Offset] + REGREGR[6]);
							}
						else
							{
							/* add Reg[Am3],Reg[Am2]        */ *Np++ = 0x03; *Np++ = (char)(REGREGL[Rp[modifier[2]].Offset] + REGREGR[Rp[modifier[1]].Offset]);
							}
						}
					else
					if ((Rp[modifier[1]].Offset == 0) && (Rp[modifier[2]].Offset >= 1))
						{
						/*** source in memory		 	  ***/
						/*** target in i register	      ***/
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); k = pcodes[n];
						if (k != 0)
							{
							/* mov  edx,Rp[Am2]             */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							/* shl  edx,imm8                */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = (char)k;
							/* add Reg[Am3],edx             */ *Np++ = 0x03; *Np++ = (char)(REGREGL[Rp[modifier[2]].Offset] + REGREGR[6]);
							}
						else
							{
							/* add  Reg[Am3],Rp[Am2]        */ *Np++ = 0x03; *Np++ = REGRPMOD[Rp[modifier[2]].Offset]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						}
					else
					if ((Rp[modifier[1]].Offset >= 1) && (Rp[modifier[2]].Offset == 0))
						{
						/*** source in  i			 	  ***/
						/*** target in memory		      ***/
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); k = pcodes[n];
						if (k != 0)
							{
							if (Rp[modifier[1]].Offset != 6)
								{
								/* mov  edx,Reg[Am2]        */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6] + REGREGR[Rp[modifier[1]].Offset]);
								}
							/* shl  edx,imm8                */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = (char)k;
							/* add Rp[Am3],edx              */ *Np++ = 0x01; *Np++ = REGRPMOD[6]; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
							{
							/* add  Rp[Am3],Reg[Am2]        */ *Np++ = 0x01; *Np++ = REGRPMOD[Rp[modifier[1]].Offset]; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
							}
						}
					break;

				case vmregMoveInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* mov edx,Rp[Am1]                   */ *Np++ = 0x8B;	*Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
						/* mov Rp[Am2],edx                   */ *Np++ = 0x89;	*Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
						{
						/* mov edx,Rp[Am1]                   */ *Np++ = 0x8B;	*Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
						/* mov Rp[Am2],edx                   */ *Np++ = 0x89;	*Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in j register 		  ***/
						/*** target in i register	      ***/
						/* mov Reg[Am2],Reg[Am1]            */ MOVREGREG(Rp[modifier[1]].Offset,Rp[modifier[0]].Offset)
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in memory		 	  ***/
						/*** target in i register	      ***/
						/* mov  Reg[Am2],Rp[Am1]            */ *Np++ = 0x8B; *Np++ = REGRPMOD[Rp[modifier[1]].Offset]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
						{
						/*** source in  i			 	  ***/
						/*** target in memory		      ***/
						/* mov  Rp[Am2],Reg[Am1]            */ *Np++ = 0x89; *Np++ = REGRPMOD[Rp[modifier[0]].Offset]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					break;

				case vmregMoveNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* fld   Rp[source]                 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fstp  Rp[target]                 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
						{
						/*** No hardware registers		  ***/
						/* fld   Rp[source]                 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fstp  Rp[target]                 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 1) && (Rp[modifier[1]].Offset > 1))
						{
						/*** source in ST0 register 	  ***/
						/*** target in STi register	      ***/
						/* fst   st(i)		                */ *Np++ = 0xDD; *Np++ = (char)(0xD0 - 1 + Rp[modifier[1]].Offset);
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in STj register 	  ***/
						/*** target in STi register	      ***/
						/* fld   st(j)						*/ *Np++ = 0xD9; *Np++ = (char)(0xC0 - 1 + Rp[modifier[0]].Offset);
						/* fstp  st(i)		                */ *Np++ = 0xDD; *Np++ = (char)(0xD8 + Rp[modifier[1]].Offset);
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in memory		 	  ***/
						/*** target in STi register	      ***/
						/* fld   Rp[source]                 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fstp  st(i)		                */ *Np++ = 0xDD; *Np++ = (char)(0xD8 + Rp[modifier[1]].Offset);
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
						{
						/*** source in STi register 	  ***/
						/*** target in memory		      ***/
						/* fld   st(j)						*/ *Np++ = 0xD9; *Np++ = (char)(0xC0 - 1 + Rp[modifier[0]].Offset);
						/* fstp  Rp[target]                 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					break;

				case vmregRefInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,dword ptr Rp[Am1]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov edx,[edx]					 */ *Np++ = 0x8B;	*Np++ = 0x12;
						/* mov dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if (Rp[modifier[0]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am1]			 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[1]].Offset >= 1)
								{
								/* mov Reg[Am2],[edx]				 */ *Np++ = 0x8B;	*Np++ = (char)(0x00+REGTARG[Rp[modifier[1]].Offset]+REGREGR[6]);
								}
							else
								{
								/* mov edx,[edx]					 */ *Np++ = 0x8B;	*Np++ = 0x12;
								/* mov dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if (Rp[modifier[0]].Offset >= 1)
							{
							if (Rp[modifier[1]].Offset >= 1)
								{
								/* mov Reg[Am2],[Am1]			     */ *Np++ = 0x8B; *Np++ = (char)(0x00+REGTARG[Rp[modifier[1]].Offset]+REGREGR[Rp[modifier[0]].Offset]);
								}
							else
								{
								/* mov edx,[Am1]			         */ *Np++ = 0x8B; *Np++ = (char)(0x00+REGTARG[6]+REGREGR[Rp[modifier[0]].Offset]);
								/* mov dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						}
					break;

				case vmregRefLong:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,dword ptr Rp[Am1]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov edx,[edx]					 */ *Np++ = 0x8B;	*Np++ = 0x12;
						/* mov dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if (Rp[modifier[0]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am1]			 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[1]].Offset >= 1)
								{
								/* mov Reg[Am2],[edx]				 */ *Np++ = 0x8B;	*Np++ = (char)(0x00+REGTARG[Rp[modifier[1]].Offset]+REGREGR[6]);
								}
							else
								{
								/* mov edx,[edx]					 */ *Np++ = 0x8B;	*Np++ = 0x12;
								/* mov dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if (Rp[modifier[0]].Offset >= 1)
							{
							if (Rp[modifier[1]].Offset >= 1)
								{
								/* mov Reg[Am2],[Am1]			     */ *Np++ = 0x8B; *Np++ = (char)(0x00+REGTARG[Rp[modifier[1]].Offset]+REGREGR[Rp[modifier[0]].Offset]);
								}
							else
								{
								/* mov edx,[Am1]			         */ *Np++ = 0x8B; *Np++ = (char)(0x00+REGTARG[6]+REGREGR[Rp[modifier[0]].Offset]);
								/* mov dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						}
					break;

				case vmregRefFloat:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,dword ptr Rp[Am1]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fld dword ptr [edx]               */ *Np++ = 0xD9;	*Np++ = 0x02;
						/* fstp qword ptr Rp[Am2]			 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if (Rp[modifier[0]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am1]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* fld dword ptr [edx]               */ *Np++ = 0xD9;	*Np++ = 0x02;
							}
						else
						if (Rp[modifier[0]].Offset >= 1)
							{
							/* fld  dword ptr [Am1]				 */ *Np++ = 0xD9; *Np++ = (char)(REGBASE[Rp[modifier[0]].Offset]); 
							}

						if (Rp[modifier[1]].Offset >= 1)
							{
							/* fstp st(i)						*/ *Np++ = 0xDD; *Np++ = (char)(0xD8+Rp[modifier[1]].Offset);
							}
						else
						if (Rp[modifier[1]].Offset == 0)
							{
							/* fstp qword ptr Rp[Am2]			*/ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						}
					break;

				case vmregRefXFloat:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl edx,2                         */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFFLOAT; 	
						/* add edx,dword ptr Rp[Am1]         */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fld dword ptr [edx]               */ *Np++ = 0xD9;	*Np++ = 0x02;
						/* fstp qword ptr Rp[target]		 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* shl edx,2                         */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFFLOAT; 	
							/* add edx,dword ptr Rp[Am1]         */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* fld qword ptr [edx]               */ *Np++ = 0xD9;	*Np++ = 0x02;
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							/* fld  qword ptr [Am1+(Am2*8)]		*/ *Np++ = 0xD9; *Np++ = 0x04; *Np++ = (char)(0x80+REGBASE[Rp[modifier[0]].Offset]+REGTARG[Rp[modifier[1]].Offset]); 
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov  edx,dword ptr Rp[Am2]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* fld  qword ptr [Am1+(edx*8)]		*/ *Np++ = 0xD9; *Np++ = 0x04; *Np++ = (char)(0x80+REGBASE[Rp[modifier[0]].Offset]+REGTARG[6]); 
							}
						else
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am1]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* fld  qword ptr [edx+(Am2*8)]		*/ *Np++ = 0xD9; *Np++ = 0x04; *Np++ = (char)(0x80+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]); 
							}

						if (Rp[modifier[2]].Offset >= 1)
							{
							/* fstp st(i)						*/ *Np++ = 0xDD; *Np++ = (char)(0xD8+Rp[modifier[2]].Offset);
							}
						else
						if (Rp[modifier[2]].Offset == 0)
							{
							/* fstp qword ptr Rp[target]		*/ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							}
						}
					break;

				case vmregRefXInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl edx,2                         */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFNUM; 	
						/* add edx,dword ptr Rp[Am1]         */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov edx,[edx]					 */ *Np++ = 0x8B;	*Np++ = 0x12;
						/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* shl edx,2                         */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFNUM; 	
							/* add edx,dword ptr Rp[Am1]         */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[edx]				 */ *Np++ = 0x8B;	*Np++ = REGBEDX[Rp[modifier[2]].Offset];
								}
							else
								{
								/* mov edx,[edx]					 */ *Np++ = 0x8B;	*Np++ = 0x12;
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							if (Rp[modifier[2]].Offset >= 1)
								{
*Np++ = 0x90;
*Np++ = 0x90;
*Np++ = 0x90;
								/* mov Reg[Am3],[Am1+(Am2*4)]		 */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[0]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
*Np++ = 0x90;
*Np++ = 0x90;
*Np++ = 0x90;
								}
							else
								{
								/* mov edx,[Am1+(Am2*4)]		     */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[0]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov  edx,dword ptr Rp[Am2]		     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[Am1+(edx*4)]		 */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[0]].Offset]+REGTARG[6]);
								}
							else
								{
								/* mov edx,[Am1+(edx*4)]		     */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[0]].Offset]+REGTARG[6]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am1]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[edx+(Am2*4)]		 */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x80+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* mov edx,[edx+(Am2*4)]		     */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x80+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						}
					break;

				case vmregRefXLong:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl edx,2                         */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFNUM32; 	
						/* add edx,dword ptr Rp[Am1]         */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov edx,[edx]					 */ *Np++ = 0x8B;	*Np++ = 0x12;
						/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* shl edx,2                         */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFNUM32; 	
							/* add edx,dword ptr Rp[Am1]         */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[edx]				 */ *Np++ = 0x8B;	*Np++ = REGBEDX[Rp[modifier[2]].Offset];
								}
							else
								{
								/* mov edx,[edx]					 */ *Np++ = 0x8B;	*Np++ = 0x12;
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[Am1+(Am2*4)]		 */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[0]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* mov edx,[Am1+(Am2*4)]		     */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[0]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov  edx,dword ptr Rp[Am2]		     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[Am1+(edx*4)]		 */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[0]].Offset]+REGTARG[6]);
								}
							else
								{
								/* mov edx,[Am1+(edx*4)]		     */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[0]].Offset]+REGTARG[6]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am1]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[edx+(Am2*4)]		 */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x80+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* mov edx,[edx+(Am2*4)]		     */ *Np++ = 0x8B; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x80+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						}
					break;

				case vmregRefNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,dword ptr Rp[Am1]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fld qword ptr [edx]               */ *Np++ = 0xDD;	*Np++ = 0x02;
						/* fstp qword ptr Rp[Am2]			 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if (Rp[modifier[0]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am1]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* fld qword ptr [edx]               */ *Np++ = 0xDD;	*Np++ = 0x02;
							}
						else
						if (Rp[modifier[0]].Offset >= 1)
							{
							/* fld  qword ptr [Am1]				 */ *Np++ = 0xDD; *Np++ = (char)(REGBASE[Rp[modifier[0]].Offset]); 
							}

						if (Rp[modifier[1]].Offset >= 1)
							{
							/* fstp st(i)						*/ *Np++ = 0xDD; *Np++ = (char)(0xD8+Rp[modifier[1]].Offset);
							}
						else
						if (Rp[modifier[1]].Offset == 0)
							{
							/* fstp qword ptr Rp[Am2]			*/ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						}
					break;

				case vmregRefXNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl edx,3                         */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFREAL; 	
						/* add edx,dword ptr Rp[Am1]         */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fld qword ptr [edx]               */ *Np++ = 0xDD;	*Np++ = 0x02;
						/* fstp qword ptr Rp[target]		 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* shl edx,3                         */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFREAL; 	
							/* add edx,dword ptr Rp[Am1]         */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* fld qword ptr [edx]               */ *Np++ = 0xDD;	*Np++ = 0x02;
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							/* fld  qword ptr [Am1+(Am2*8)]		*/ *Np++ = 0xDD; *Np++ = 0x04; *Np++ = (char)(REGBASE[Rp[modifier[0]].Offset]+REGINDX[Rp[modifier[1]].Offset]); 
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov  edx,dword ptr Rp[Am2]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* fld  qword ptr [Am1+(edx*8)]		*/ *Np++ = 0xDD; *Np++ = 0x04; *Np++ = (char)(REGBASE[Rp[modifier[0]].Offset]+REGINDX[6]); 
							}
						else
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am1]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* fld  qword ptr [edx+(Am2*8)]		*/ *Np++ = 0xDD; *Np++ = 0x04; *Np++ = (char)(REGBASE[6]+REGINDX[Rp[modifier[1]].Offset]); 
							}

						if (Rp[modifier[2]].Offset >= 1)
							{
							/* fstp st(i)						*/ *Np++ = 0xDD; *Np++ = (char)(0xD8+Rp[modifier[2]].Offset);
							}
						else
						if (Rp[modifier[2]].Offset == 0)
							{
							/* fstp qword ptr Rp[target]		*/ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							}
						}
					break;

 				case vmregDivInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov  eax,dword ptr Rp[Am2]       */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* cdq                              */ *Np++ = 0x99; 
						/* idiv eax,Rp[Am1]                 */ *Np++ = 0xF7;	*Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov Rp[Am2],eax                  */ *Np++ = 0x89;	*Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
						{
						if ((Rp[modifier[0]].Offset == 1) && (Rp[modifier[1]].Offset != 1))
							{
							/*** source in eax AND target NOT in eax ***/	
							/* mov dword ptr Rp[Ir[1]],eax	           */ *Np++ = 0x89; *Np++ = REGRPMOD[1];*(LpNUM)Np = (RpRelAddress+(Ir[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
						if ((Ir[1] != 0) && (Rp[modifier[1]].Offset != 1))
							{
							/*** target in reg and NOT eax           ***/	
							/* mov dword ptr Rp[Ir[1]],eax	           */ *Np++ = 0x89; *Np++ = REGRPMOD[1];*(LpNUM)Np = (RpRelAddress+(Ir[1]<<BITSIZEOFAISWORD));Np+=4;
							}

						if (Rp[modifier[1]].Offset == 0)
							{
							/*** target in memory                    ***/	
							/* mov eax,Rp[Am2]                         */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if (Rp[modifier[1]].Offset > 1)
							{
							/*** target in reg AND NOT eax           ***/	
							/* mov eax,Reg[Am2]                        */ *Np++ = 0x8B; *Np++ = (REGREGL[1]+REGREGR[Rp[modifier[1]].Offset]);
							}

						if (Rp[modifier[0]].Offset == 0)
							{
							/* cdq                           */ *Np++ = 0x99;
							/* idiv dword ptr Rp[Am1]        */ *Np++ = 0xF7; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));  Np+=4;
							}
						else
						if ((Rp[modifier[0]].Offset == 1) && (Rp[modifier[1]].Offset != 1))
							{
							/* cdq                           */ *Np++ = 0x99;
							/* idiv dword ptr Rp[Am1]        */ *Np++ = 0xF7; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));  Np+=4;
							}
						else
						if (Rp[modifier[0]].Offset >= 1)
							{
							/* cdq                           */ *Np++ = 0x99;
							/* idiv Reg[Am1]                 */ *Np++ = 0xF7; *Np++ = (char)(0xF8 + REGREGR[Rp[modifier[0]].Offset]);  
							}

						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov Rp[Am2],eax                   */ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if (Rp[modifier[1]].Offset > 1)
							{
							/* mov Reg[Am2],eax                  */ *Np++ = 0x8B; *Np++ = (REGREGL[Rp[modifier[1]].Offset]+REGREGR[1]);
							}
						if ((Rp[modifier[0]].Offset == 1) && (Rp[modifier[1]].Offset != 1))
							{
							/*** source in eax AND target NOT in eax ***/	
							/* mov eax,dword ptr Rp[Ir[1]]	     */ *Np++ = 0x8B; *Np++ = REGRPMOD[1];*(LpNUM)Np = (RpRelAddress+(Ir[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
						if ((Ir[1] != 0) && (Rp[modifier[1]].Offset != 1))
							{	
							/* mov eax,dword ptr Rp[Ir[1]]	     */ *Np++ = 0x8B; *Np++ = REGRPMOD[1];*(LpNUM)Np = (RpRelAddress+(Ir[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						}
					break;
					
				case vmregDivImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov eax,Rp[Am2]                   */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* cdq                               */ *Np++ = 0x99;
						/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* mov ecx,immediate                 */ *Np++ = 0xB9; *(LpNUM)Np = pcodes[n]; Np+=4;
						/* idiv eax,ecx                      */ *Np++ = 0xF7; *Np++ = 0xF9;  
						/* mov Rp[Am2],eax                   */ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if ((Ir[1] != 0) && (Rp[modifier[1]].Offset != 1))
							{	
							/* mov dword ptr Rp[Ir[1]],eax	 */ *Np++ = 0x89; *Np++ = REGRPMOD[1];*(LpNUM)Np = (RpRelAddress+(Ir[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						if ((Ir[2] != 0) && (Rp[modifier[1]].Offset != 2))
							{	
							/* mov dword ptr Rp[Ir[2]],ecx	 */ *Np++ = 0x89; *Np++ = REGRPMOD[2];*(LpNUM)Np = (RpRelAddress+(Ir[2]<<BITSIZEOFAISWORD));Np+=4;
							}
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov eax,Rp[Am2]                   */ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if (Rp[modifier[1]].Offset > 1)
							{
							/* mov eax,Reg[Am2]                  */ *Np++ = 0x8B; *Np++ = (REGREGL[1]+REGREGR[Rp[modifier[1]].Offset]);
							}
						/* cdq                               */ *Np++ = 0x99;
						/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* mov ecx,immediate                 */ *Np++ = 0xB9; *(LpNUM)Np = pcodes[n]; Np+=4;
						/* idiv eax,ecx                      */ *Np++ = 0xF7; *Np++ = 0xF9;  
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov Rp[Am2],eax                   */ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if (Rp[modifier[1]].Offset > 1)
							{
							/* mov Reg[Am2],eax                  */ *Np++ = 0x8B; *Np++ = (REGREGL[Rp[modifier[1]].Offset]+REGREGR[1]);
							}
						if ((Ir[1] != 0) && (Rp[modifier[1]].Offset != 1))
							{	
							/* mov reg,dword ptr Rp[Am2]	     */ *Np++ = 0x8B; *Np++ = REGRPMOD[1];*(LpNUM)Np = (RpRelAddress+(Ir[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						if ((Ir[2] != 0) && (Rp[modifier[1]].Offset != 2))
							{	
							/* mov reg,dword ptr Rp[Am2]	     */ *Np++ = 0x8B; *Np++ = REGRPMOD[2];*(LpNUM)Np = (RpRelAddress+(Ir[2]<<BITSIZEOFAISWORD));Np+=4;
							}
						}
					break;

				case vmregMulInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* mov  edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* imul edx,Rp[Am1]                 */ *Np++ = 0x0F; *Np++ = 0xAF; *Np++ = 0x95;  *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));  Np+=4;
						/* mov  Rp[Am2],edx                 */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
						{
						/* mov  edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* imul edx,Rp[Am1]                 */ *Np++ = 0x0F; *Np++ = 0xAF; *Np++ = 0x95;  *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));  Np+=4;
						/* mov  Rp[Am2],edx                 */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in j register 		  ***/
						/*** target in i register	      ***/
						/* imul Reg[Am2],Reg[Am1]           */ *Np++ = 0x0F; *Np++ = 0xAF; *Np++ = (char)(REGREGL[Rp[modifier[1]].Offset] + REGREGR[Rp[modifier[0]].Offset]);
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in memory		 	  ***/
						/*** target in i register	      ***/
						/* imul  Reg[Am2],Rp[Am1]           */ *Np++ = 0x0F; *Np++ = 0xAF; *Np++ = REGRPMOD[Rp[modifier[1]].Offset]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
						{
						/*** source in  i			 	  ***/
						/*** target in memory		      ***/
						/* mov  edx,Rp[Am2]                 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* imul edx,Reg[Am1]                */ *Np++ = 0x0F; *Np++ = 0xAF; *Np++ = (char)(REGREGL[6] + REGREGR[Rp[modifier[0]].Offset]);
						/* mov  Rp[Am2],edx                 */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					break;

				case vmregJmpLTImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7D;
					RegJmpCCImmediate:
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,Rp[Am1]						 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment			   ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* cmp edx,immediate					 */ *Np++ = 0x81; *Np++ = 0xFA; *(LpNUM)Np = pcodes[n]; Np+=4;
						}
					else
					if (Rp[modifier[0]].Offset == 0)
						{
						/* mov edx,Rp[Am1]						 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment			   ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* cmp edx,immediate					 */ *Np++ = 0x81; *Np++ = 0xFA; *(LpNUM)Np = pcodes[n]; Np+=4;
						}
					else
						{
						/*** Set Pc/Nc allignment			   ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* cmp Reg[Am1],immediate				 */ *Np++ = 0x81; *Np++ = (char)(0xF8+REGREGR[Rp[modifier[0]].Offset]); *(LpNUM)Np = pcodes[n];Np+=4;
						}
					/* jge                               */ *Np++ = imod[1]; *Np++ = 0x05;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl          ***/ Mp[n]   = -2;                     
					/* jmp label                         */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmregJmpLEImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7F;
					goto RegJmpCCImmediate;
					break;

				case vmregJmpEQImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x75;
					goto RegJmpCCImmediate;
					break;

				case vmregJmpNEImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x74;
					goto RegJmpCCImmediate;
					break;

				case vmregJmpGEImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7C;
					goto RegJmpCCImmediate;
					break;

				case vmregJmpGTImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7E;
					goto RegJmpCCImmediate;
					break;

#if 1 // MFK Note: jg --> ja, jl --> jb, etc.
				case vmregJmpLTUImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7D;
					RegJmpCCUImmediate:
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,Rp[Am1]						 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment			   ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* cmp edx,immediate					 */ *Np++ = 0x81; *Np++ = 0xFA; *(LpNUM)Np = pcodes[n]; Np+=4;
						}
					else
					if (Rp[modifier[0]].Offset == 0)
						{
						/* mov edx,Rp[Am1]						 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment			   ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* cmp edx,immediate					 */ *Np++ = 0x81; *Np++ = 0xFA; *(LpNUM)Np = pcodes[n]; Np+=4;
						}
					else
						{
						/*** Set Pc/Nc allignment			   ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* cmp Reg[Am1],immediate				 */ *Np++ = 0x81; *Np++ = (char)(0xF8+REGREGR[Rp[modifier[0]].Offset]); *(LpNUM)Np = pcodes[n];Np+=4;
						}
					/* jge                               */ *Np++ = imod[1]; *Np++ = 0x05;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl          ***/ Mp[n]   = -2;                     
					/* jmp label                         */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmregJmpLEUImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7F;
					goto RegJmpCCUImmediate;
					break;

				case vmregJmpEQUImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x75;
					goto RegJmpCCUImmediate;
					break;

				case vmregJmpNEUImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x74;
					goto RegJmpCCUImmediate;
					break;

				case vmregJmpGEUImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7C;
					goto RegJmpCCUImmediate;
					break;

				case vmregJmpGTUImmediate:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7E;
					goto RegJmpCCUImmediate;
					break;
#endif // MFK

				case vmregJmpLTInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7D;
					RegJmpCCInteger:
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,Rp[Am1]                   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* cmp edx,Rp[Am2]                   */ *Np++ = 0x3B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,Rp[Am1]                   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* cmp edx,Rp[Am2]                   */ *Np++ = 0x3B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							/* cmp Reg[Am1],Reg[Am2]			 */ *Np++ = 0x3B; *Np++ = (char)(REGREGL[Rp[modifier[0]].Offset]+REGREGR[Rp[modifier[1]].Offset]);
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* cmp Reg[Am1],Rp[Am2]				 */ *Np++ = 0x3B; *Np++ = REGRPMOD[Rp[modifier[0]].Offset];*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov edx,Rp[Am1]                   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* cmp edx,Reg[Am2]					 */ *Np++ = 0x3B; *Np++ = (char)(0xD0+REGREGR[Rp[modifier[1]].Offset]); 
							}
						}
					/* jge                               */ *Np++ = imod[1]; *Np++ = 0x05;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl          ***/ Mp[n]   = -2;                     
					/* jmp label                         */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmregJmpLEInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7F;
					goto RegJmpCCInteger;
					break;

				case vmregJmpEQInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x75;
					goto RegJmpCCInteger;
					break;

				case vmregJmpNEInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x74;
					goto RegJmpCCInteger;
					break;

				case vmregJmpGEInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7C;
					goto RegJmpCCInteger;
					break;

				case vmregJmpGTInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7E;
					goto RegJmpCCInteger;
					break;

#if 1 // MFK Note: jg --> ja, jl --> jb, etc.
				case vmregJmpLTUInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7D;
					RegJmpCCUInteger:
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,Rp[Am1]                   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* cmp edx,Rp[Am2]                   */ *Np++ = 0x3B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,Rp[Am1]                   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* cmp edx,Rp[Am2]                   */ *Np++ = 0x3B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							/* cmp Reg[Am1],Reg[Am2]			 */ *Np++ = 0x3B; *Np++ = (char)(REGREGL[Rp[modifier[0]].Offset]+REGREGR[Rp[modifier[1]].Offset]);
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* cmp Reg[Am1],Rp[Am2]				 */ *Np++ = 0x3B; *Np++ = REGRPMOD[Rp[modifier[0]].Offset];*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov edx,Rp[Am1]                   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* cmp edx,Reg[Am2]					 */ *Np++ = 0x3B; *Np++ = (char)(0xD0+REGREGR[Rp[modifier[1]].Offset]); 
							}
						}
					/* jge                               */ *Np++ = imod[1]; *Np++ = 0x05;
					/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl          ***/ Mp[n]   = -2;                     
					/* jmp label                         */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmregJmpLEUInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7F;
					goto RegJmpCCUInteger;
					break;

				case vmregJmpEQUInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x75;
					goto RegJmpCCUInteger;
					break;

				case vmregJmpNEUInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x74;
					goto RegJmpCCUInteger;
					break;

				case vmregJmpGEUInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7C;
					goto RegJmpCCUInteger;
					break;

				case vmregJmpGTUInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x7E;
					goto RegJmpCCUInteger;
					break;
#endif // MFK
				case vmregNumber:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* fild  dword ptr Rp[source]       */ *Np++ = 0xDB; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fstp  qword ptr Rp[target]       */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
						{
						/*** Save the source from its hardware register (if any)  ***/
						REGSAVEMOD(modifier[0])
						/*** Load the source variable to ST0  ***/
						/* fild  dword ptr Rp[source]           */ *Np++ = 0xDB; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						if (Rp[modifier[1]].Offset == 0)
							{
							/* fstp  qword ptr Rp[target]       */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
							{
							/* fstp  st(target)			        */ *Np++ = 0xDD; *Np++ = (char)(0xD8 + Rp[modifier[1]].Offset);
							}
						}
					break;

				case vmregInteger:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* fld   qword ptr Rp[source]       */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fld1	             				*/ *Np++ = 0xD9; *Np++ = 0xE8;
						/* fld 	  st(1)						*/ *Np++ = 0xD9; *Np++ = 0xC1;
						/* fprem							*/ *Np++ = 0xD9; *Np++ = 0xF8;
						/* fsubp   st(2),st(0)				*/ *Np++ = 0xDE; *Np++ = 0xEA;
						/* fstp    st(0)					*/ *Np++ = 0xDD; *Np++ = 0xD8;
						/* fistp dword ptr Rp[target]       */ *Np++ = 0xDB; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
						{
						/*** Load the source variable to ST0  ***/
						if (Rp[modifier[0]].Offset == 0)
							{
							/* fld   qword ptr Rp[source]       */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* fld1	             				*/ *Np++ = 0xD9; *Np++ = 0xE8;
							/* fld 	  st(1)						*/ *Np++ = 0xD9; *Np++ = 0xC1;
							/* fprem							*/ *Np++ = 0xD9; *Np++ = 0xF8;
							/* fsubp   st(2),st(0)				*/ *Np++ = 0xDE; *Np++ = 0xEA;
							/* fstp    st(0)					*/ *Np++ = 0xDD; *Np++ = 0xD8;
							/* fistp dword ptr Rp[target]       */ *Np++ = 0xDB; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
						if (Rp[modifier[0]].Offset == 1)
							{
							/*** ST0 hardware register 		  ***/
							/* fld1	             				*/ *Np++ = 0xD9; *Np++ = 0xE8;
							/* fld 	  st(1)						*/ *Np++ = 0xD9; *Np++ = 0xC1;
							/* fprem							*/ *Np++ = 0xD9; *Np++ = 0xF8;
							/* fsubp   st(2),st(0)				*/ *Np++ = 0xDE; *Np++ = 0xEA;
							/* fstp    st(0)					*/ *Np++ = 0xDD; *Np++ = 0xD8;
							/* fist	 qword ptr Rp[target]       */ *Np++ = 0xDB; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
							{
							/* fld   st(source)                 */ *Np++ = 0xD9; *Np++ = (char)(0xBF + Rp[modifier[0]].Offset);
							/* fld1	             				*/ *Np++ = 0xD9; *Np++ = 0xE8;
							/* fld 	  st(1)						*/ *Np++ = 0xD9; *Np++ = 0xC1;
							/* fprem							*/ *Np++ = 0xD9; *Np++ = 0xF8;
							/* fsubp   st(2),st(0)				*/ *Np++ = 0xDE; *Np++ = 0xEA;
							/* fstp    st(0)					*/ *Np++ = 0xDD; *Np++ = 0xD8;
							/* fistp dword ptr Rp[target]       */ *Np++ = 0xDB; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						/*** Load the target into its hardware register (if any)  ***/
						REGLOADMOD(modifier[1])
						}
					break;

				case vmregAbsNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					imodCount = 1; imod[0] = 0xE1;
					UnaryNumberReg:
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* fld   Rp[source]                 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fabs                             */ *Np++ = 0xD9; for (i=0;i<imodCount;++i) {*Np++ = imod[i];}
						/* fstp  Rp[target]                 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
						{
						/*** No hardware registers		  ***/
						/* fld   Rp[source]                 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fabs                             */ *Np++ = 0xD9; for (i=0;i<imodCount;++i) {*Np++ = imod[i];}
						/* fstp  Rp[target]                 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 1) && (modifier[0] == modifier[1]))
						{
						/*** ST0 hardware register 		  ***/
						/* fabs                             */ *Np++ = 0xD9; for (i=0;i<imodCount;++i) {*Np++ = imod[i];}
						}
					else
						{
						/*** Load the source variable to ST0 	***/
						if (Rp[modifier[0]].Offset == 0)
							{
							/* fld   Rp[source]                 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
							{
							/* fld   st(source)                 */ *Np++ = 0xD9; *Np++ = (char)(0xBF + Rp[modifier[0]].Offset);
							}
						/*** Perform the unary op in ST0  ***/
						/* fabs                             */ *Np++ = 0xD9; for (i=0;i<imodCount;++i) {*Np++ = imod[i];}
						/*** Save the target variable from ST0***/
						if (Rp[modifier[1]].Offset == 0)
							{
							/* fstp  Rp[target]                 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
							{
							/* fstp   st(source)                */ *Np++ = 0xDD; *Np++ = (char)(0xD8 + Rp[modifier[1]].Offset);
							}
						}
					break;

				case  vmregCosNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
					imodCount = 1; imod[0] = 0xFF;
					goto UnaryNumberReg;
					break;

				case  vmregSinNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
					imodCount = 1; imod[0] = 0xFE;
					goto UnaryNumberReg;
					break;

				case  vmregSqrtNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
					imodCount = 1; imod[0] = 0xFA;
					goto UnaryNumberReg;
					break;

				case  vmregTanNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
					imodCount = 3; imod[0] = 0xFB; imod[1] = 0xDE; imod[2] = 0xF9;
					goto UnaryNumberReg;
					break;

				case vmregAddNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					imodCount = 3; imod[0] = 0x85; imod[1] = 0xBF; imod[2] = 0xC1;
					BinaryNumberReg:
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* fld   Rp[target]                 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* fadd  Rp[source]                 */ *Np++ = 0xDC; *Np++ = imod[0]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
						/* fstp  Rp[target]                 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
						{
						/*** No hardware registers		  ***/
						/* fld   Rp[target]                 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* fadd  Rp[source]                 */ *Np++ = 0xDC; *Np++ = imod[0]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
						/* fstp  Rp[target]                 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 1) && (modifier[0] == modifier[1]))
						{
						/*** source in ST0 register 	  ***/
						/*** source and target are same	  ***/
						/* fadd  st(0),st(0)                */ *Np++ = 0xD8; *Np++ = (char)(imod[1] + Rp[modifier[1]].Offset);
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in STj register 	  ***/
						/*** target in STi register	      ***/
						/* fld   st(j)						*/ *Np++ = 0xD9; *Np++ = (char)(0xBF + Rp[modifier[0]].Offset);
						/* faddp st(i),st(0)                */ *Np++ = 0xDE; *Np++ = (char)(imod[1] + Rp[modifier[1]].Offset + 1);
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in memory		 	  ***/
						/*** target in STi register	      ***/
						/* fld   Rp[source]                 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* faddp st(i),st(0)                */ *Np++ = 0xDE; *Np++ = (char)(imod[1] + Rp[modifier[1]].Offset + 1);
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
						{
						/*** source in STi			 	  ***/
						/*** target in memory		      ***/
						/* fld   st(target)                 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* fld   st(j)						*/ *Np++ = 0xD9; *Np++ = (char)(0xBF + Rp[modifier[0]].Offset + 1);
						/* faddp                            */ *Np++ = 0xDE; *Np++ = imod[2];
						/* fstp  Rp[target]                 */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					break;

				case vmregSubNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
					imodCount = 3; imod[0] = 0xA5; imod[1] = 0xE7; imod[2] = 0xE9;
					goto BinaryNumberReg;
					break;

				case vmregMulNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
					imodCount = 3; imod[0] = 0x8D; imod[1] = 0xC7; imod[2] = 0xC9;
					goto BinaryNumberReg;
					break;

				case vmregDivNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
					imodCount = 3; imod[0] = 0xB5; imod[1] = 0xF7; imod[2] = 0xF9;
					goto BinaryNumberReg;
					break;

				case vmregAndInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					imodCount = 2; imod[0] = 0x21; imod[1] = 0x23;
					goto BinaryIntegerReg;
					break;

				case vmregOrInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					imodCount = 2; imod[0] = 0x09; imod[1] = 0x0B;
					goto BinaryIntegerReg;
					break;

				case vmregXorInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					imodCount = 2; imod[0] = 0x31; imod[1] = 0x33;
					goto BinaryIntegerReg;
					break;


				case vmregRefCharacter:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov   edx,dword ptr Rp[Am1]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* movsx edx,byte ptr [edx]			 */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = 0x12;
						/* mov   dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if (Rp[modifier[0]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am1]			 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[1]].Offset >= 1)
								{
								/* movsx Reg[Am2],[edx]				 */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = (char)(0x00+REGTARG[Rp[modifier[1]].Offset]+REGREGR[6]);
								}
							else
								{
								/* movsx edx,[edx]					 */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = 0x12;
								/* mov   dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if (Rp[modifier[0]].Offset >= 1)
							{
							if (Rp[modifier[1]].Offset >= 1)
								{
								/* movsx Reg[Am2],[Am1]			     */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = (char)(0x00+REGTARG[Rp[modifier[1]].Offset]+REGREGR[Rp[modifier[0]].Offset]);
								}
							else
								{
								/* movsx edx,[Am1]			         */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = (char)(0x00+REGTARG[6]+REGREGR[Rp[modifier[0]].Offset]);
								/* mov   dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						}
					break;


				case vmregRefShort:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov   edx,dword ptr Rp[Am1]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* movsx edx,[edx]					 */ *Np++ = 0x0F; *Np++ = 0xBF; *Np++ = 0x12;
						/* mov   dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if (Rp[modifier[0]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am1]			 */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[1]].Offset >= 1)
								{
								/* movsx Reg[Am2],[edx]				 */ *Np++ = 0x0F; *Np++ = 0xBF; *Np++ = (char)(0x00+REGTARG[Rp[modifier[1]].Offset]+REGREGR[6]);
								}
							else
								{
								/* movsx edx,[edx]					 */ *Np++ = 0x0F; *Np++ = 0xBF; *Np++ = 0x12;
								/* mov   dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if (Rp[modifier[0]].Offset >= 1)
							{
							if (Rp[modifier[1]].Offset >= 1)
								{
								/* movsx Reg[Am2],[Am1]			     */ *Np++ = 0x0F; *Np++ = 0xBF; *Np++ = (char)(0x00+REGTARG[Rp[modifier[1]].Offset]+REGREGR[Rp[modifier[0]].Offset]);
								}
							else
								{
								/* movsx edx,[Am1]			         */ *Np++ = 0x0F; *Np++ = 0xBF; *Np++ = (char)(0x00+REGTARG[6]+REGREGR[Rp[modifier[0]].Offset]);
								/* mov   dword ptr Rp[Am2],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						}
					break;

				case vmregSetInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov ecx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov edx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov [ecx],edx					 */ *Np++ = 0x89;	*Np++ = 0x11;
						}
					else
						{
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am2]             */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [edx],Reg[Am1]				 */ *Np++ = 0x89;	*Np++ = REGBEDX[Rp[modifier[0]].Offset];
								}
							else
								{
								if (Ir[5] != 0) 
									{
									/* mov dword ptr Rp[Ir[5]],edi	 */ *Np++ = 0x89; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(Ir[5]<<BITSIZEOFAISWORD)); Np+=4;
									}
								/* mov edi,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [edx],edi					 */ *Np++ = 0x89;	*Np++ = 0x3A;
								if (Ir[5] != 0) 
									{
									/* mov edi,dword ptr Rp[Ir[5]]	 */ *Np++ = 0x8B; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(Ir[5]<<BITSIZEOFAISWORD)); Np+=4;
									}
								}
							}
						else
							{
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [Am2],Reg[Am1]		 		 */ *Np++ = 0x89; *Np++ = (char)(0x00+REGBASE[Rp[modifier[1]].Offset]+REGTARG[Rp[modifier[0]].Offset]);
								}
							else
								{
								/* mov edx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [Am2],Reg[Am1]		 		 */ *Np++ = 0x89; *Np++ = (char)(0x00+REGBASE[Rp[modifier[1]].Offset]+REGTARG[6]);
								}
							}
						}
					break;

				case vmregSetLong:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov ecx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov edx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov [ecx],edx					 */ *Np++ = 0x89;	*Np++ = 0x11;
						}
					else
						{
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am2]             */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [edx],Reg[Am1]				 */ *Np++ = 0x89;	*Np++ = REGBEDX[Rp[modifier[0]].Offset];
								}
							else
								{
								if (Ir[5] != 0) 
									{
									/* mov dword ptr Rp[Ir[5]],edi	 */ *Np++ = 0x89; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(Ir[5]<<BITSIZEOFAISWORD)); Np+=4;
									}
								/* mov edi,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [edx],edi					 */ *Np++ = 0x89;	*Np++ = 0x3A;
								if (Ir[5] != 0) 
									{
									/* mov edi,dword ptr Rp[Ir[5]]	 */ *Np++ = 0x8B; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(Ir[5]<<BITSIZEOFAISWORD)); Np+=4;
									}
								}
							}
						else
							{
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [Am2],Reg[Am1]		 		 */ *Np++ = 0x89; *Np++ = (char)(0x00+REGBASE[Rp[modifier[1]].Offset]+REGTARG[Rp[modifier[0]].Offset]);
								}
							else
								{
								/* mov edx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [Am2],Reg[Am1]		 		 */ *Np++ = 0x89; *Np++ = (char)(0x00+REGBASE[Rp[modifier[1]].Offset]+REGTARG[6]);
								}
							}
						}
					break;

				case vmregSetNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					imod[0]=0xDD;
					regSetNumber:        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov  edx,Rp[Am2]                  */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* fld  qword ptr Rp[Am1]			 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fstp qword ptr [edx]	             */ *Np++ = imod[0]; *Np++ = 0x1A; 
						}
					else
						{
						if (Rp[modifier[0]].Offset > 1)
							{
							/* fld  st(i)						 */ *Np++ = 0xD9; *Np++ = (char)(0xBF+Rp[modifier[0]].Offset);
							}
						else
						if (Rp[modifier[0]].Offset == 0)
							{
							/* fld  qword ptr Rp[Am1]			 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}

						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov  edx,Rp[Am2]                  */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							if (Rp[modifier[0]].Offset == 1)
								{
								/* fst  qword ptr [edx]	         */ *Np++ = imod[0]; *Np++ = 0x12; 
								}
							else
								{
								/* fstp qword ptr [edx]	         */ *Np++ = imod[0]; *Np++ = 0x1A; 
								}
							}
						else
							{
							if (Rp[modifier[0]].Offset == 1)
								{
								/* fst   qword ptr [Am2]		 */ *Np++ = imod[0]; *Np++ = (char)(0x10+REGBASE[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* fstp  qword ptr [Am2]		 */ *Np++ = imod[0]; *Np++ = (char)(0x18+REGBASE[Rp[modifier[1]].Offset]);
								}
							}
						}
					break;

				case vmregSetShort:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov ecx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov edx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov [ecx],dx						 */ *Np++ = 0x66;	*Np++ = 0x89;	*Np++ = 0x11;
						}
					else
						{
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am2]             */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [edx],Reg[Am1]				 */ *Np++ = 0x66;	*Np++ = 0x89;	*Np++ = REGBEDX[Rp[modifier[0]].Offset];
								}
							else
								{
								if (Ir[5] != 0) 
									{
									/* mov dword ptr Rp[Ir[5]],edi	 */ *Np++ = 0x89; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(Ir[5]<<BITSIZEOFAISWORD)); Np+=4;
									}
								/* mov edi,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [edx],edi					 */ *Np++ = 0x66;	*Np++ = 0x89;	*Np++ = 0x3A;
								if (Ir[5] != 0) 
									{
									/* mov edi,dword ptr Rp[Ir[5]]	 */ *Np++ = 0x8B; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(Ir[5]<<BITSIZEOFAISWORD)); Np+=4;
									}
								}
							}
						else
							{
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [Am2],Reg[Am1]		 		 */ *Np++ = 0x66; *Np++ = 0x89; *Np++ = (char)(0x00+REGBASE[Rp[modifier[1]].Offset]+REGTARG[Rp[modifier[0]].Offset]);
								}
							else
								{
								/* mov edx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [Am2],Reg[Am1]		 		 */ *Np++ = 0x66; *Np++ = 0x89; *Np++ = (char)(0x00+REGBASE[Rp[modifier[1]].Offset]+REGTARG[6]);
								}
							}
						}
					break;

				/* Register opcodes with two register arguments. */
				case vmregDivrInteger:
				case vmregDivrNumber:
				case vmregLogNumber:
				case vmregPwrNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        

					SAVEALLREGS

					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */

					LOADALLREGS
					break;

				case vmregSetXInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov ecx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl ecx,2                         */ *Np++ = 0xC1; *Np++ = 0xE1; *Np++ = BITSIZEOFNUM; 	
						/* add ecx,dword ptr Rp[Am3]         */ *Np++ = 0x03; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov edx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov [ecx],edx					 */ *Np++ = 0x89;	*Np++ = 0x11;
						}
					else
						{
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]             */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* shl edx,2                             */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFNUM; 	
							/* add edx,dword ptr Rp[Am3]             */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [edx],Reg[Am1]				 */ *Np++ = 0x89;	*Np++ = REGBEDX[Rp[modifier[0]].Offset];
								}
							else
								{
								REGSAVEEDI
								/* mov edi,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [edx],edi					 */ *Np++ = 0x89;	*Np++ = 0x3A;
								REGLOADEDI
								}
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [Am3+(Am2*4)],Reg[Am1]		 */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[Rp[modifier[0]].Offset]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* mov edx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [Am3+(Am2*4)],edx		     */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov  edx,dword ptr Rp[Am2]		     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [Am3+(edx*4)],Reg[Am1]		 */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[Rp[modifier[0]].Offset]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]);
								}
							else
								{
								if (Rp[modifier[2]].Offset != 5) k = 5; else k = 4; 
								if (Ir[k] != 0) 
									{
									/* mov dword ptr Rp[Ir[k]],Reg[n]*/ *Np++ = 0x89; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								/* mov Reg[n],dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [Am3+(edx*4)],Reg[n]		     */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[k]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]);
								if (Ir[k] != 0) 
									{
									/* mov Reg[k],dword ptr Rp[Ir[k]]*/ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								}
							}
						else
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am3]		     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [edx+(Am2*4)],Reg[Am1]		 */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[Rp[modifier[0]].Offset]); *Np++ = (char)(0x80+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								if (Rp[modifier[1]].Offset != 5) k = 5; else k = 4; 
								if (Ir[k] != 0) 
									{
									/* mov dword ptr Rp[Ir[k]],Reg[k]*/ *Np++ = 0x89; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								/* mov Reg[k],dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [edx+(Am2*4)],Reg[k]		     */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[k]); *Np++ = (char)(0x80+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								if (Ir[k] != 0) 
									{
									/* mov Reg[k],dword ptr Rp[Ir[k]]*/ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								}
							}
						}
					break;

				case vmregSetXLong:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov ecx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl ecx,2                         */ *Np++ = 0xC1; *Np++ = 0xE1; *Np++ = BITSIZEOFNUM32; 	
						/* add ecx,dword ptr Rp[Am3]         */ *Np++ = 0x03; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov edx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov [ecx],edx					 */ *Np++ = 0x89;	*Np++ = 0x11;
						}
					else
						{
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]             */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* shl edx,2                             */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFNUM32; 	
							/* add edx,dword ptr Rp[Am3]             */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [edx],Reg[Am1]				 */ *Np++ = 0x89;	*Np++ = REGBEDX[Rp[modifier[0]].Offset];
								}
							else
								{
								REGSAVEEDI
								/* mov edi,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [edx],edi					 */ *Np++ = 0x89;	*Np++ = 0x3A;
								REGLOADEDI
								}
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [Am3+(Am2*4)],Reg[Am1]		 */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[Rp[modifier[0]].Offset]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* mov edx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [Am3+(Am2*4)],edx		     */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov  edx,dword ptr Rp[Am2]		     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [Am3+(edx*4)],Reg[Am1]		 */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[Rp[modifier[0]].Offset]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]);
								}
							else
								{
								if (Rp[modifier[2]].Offset != 5) k = 5; else k = 4; 
								if (Ir[k] != 0) 
									{
									/* mov dword ptr Rp[Ir[k]],Reg[n]*/ *Np++ = 0x89; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								/* mov Reg[n],dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [Am3+(edx*4)],Reg[n]		     */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[k]); *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]);
								if (Ir[k] != 0) 
									{
									/* mov Reg[k],dword ptr Rp[Ir[k]]*/ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								}
							}
						else
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am3]		     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [edx+(Am2*4)],Reg[Am1]		 */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[Rp[modifier[0]].Offset]); *Np++ = (char)(0x80+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								if (Rp[modifier[1]].Offset != 5) k = 5; else k = 4; 
								if (Ir[k] != 0) 
									{
									/* mov dword ptr Rp[Ir[k]],Reg[k]*/ *Np++ = 0x89; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								/* mov Reg[k],dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [edx+(Am2*4)],Reg[k]		     */ *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[k]); *Np++ = (char)(0x80+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								if (Ir[k] != 0) 
									{
									/* mov Reg[k],dword ptr Rp[Ir[k]]*/ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								}
							}
						}
					break;

				case vmregSetXNumber:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					imod[0]=0xDD;imod[1]=BITSIZEOFREAL;imod[2]=0xC0;
					regSetXNumber:        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov  edx,Rp[Am2]                  */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* shl  edx,immediate                */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = imod[1];
						/* add  edx,Rp[Am3]                  */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));	Np+=4;
						/* fld  qword ptr Rp[Am1]			 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fstp qword ptr [edx]	             */ *Np++ = imod[0]; *Np++ = 0x1A; 
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0) && (Rp[modifier[2]].Offset == 0))
						{
						/* mov  edx,Rp[Am2]                  */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* shl  edx,immediate                */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = imod[1];
						/* add  edx,Rp[Am3]                  */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));	Np+=4;
						/* fld  qword ptr Rp[Am1]			 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fstp qword ptr [edx]	             */ *Np++ = imod[0]; *Np++ = 0x1A; 
						}
					else
						{
						if (Rp[modifier[0]].Offset > 1)
							{
							/* fld  st(i)						 */ *Np++ = 0xD9; *Np++ = (char)(0xBF+Rp[modifier[0]].Offset);
							}
						else
						if (Rp[modifier[0]].Offset == 0)
							{
							/* fld    qword ptr Rp[source]		 */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}

						if ((Rp[modifier[1]].Offset == 0) && (Rp[modifier[2]].Offset == 0))
							{
							/* mov  edx,Rp[Am2]                  */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							/* shl  edx,immediate                */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = imod[1];
							/* add  edx,Rp[Am3]                  */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));	Np+=4;
							if (Rp[modifier[0]].Offset == 1)
								{
								/* fst  qword ptr [edx]	         */ *Np++ = imod[0]; *Np++ = 0x12; 
								}
							else
								{
								/* fstp qword ptr [edx]	         */ *Np++ = imod[0]; *Np++ = 0x1A; 
								}
							}
						else
						if ((Rp[modifier[1]].Offset >= 1) && (Rp[modifier[2]].Offset >= 1))
							{
							if (Rp[modifier[0]].Offset == 1)
								{
								/* fst   qword ptr [Am3+(Am2*8)]	*/ *Np++ = imod[0]; *Np++ = 0x14; *Np++ = (char)(imod[2]+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]); 
								}
							else
								{
								/* fstp  qword ptr [Am3+(Am2*8)]	*/ *Np++ = imod[0]; *Np++ = 0x1C; *Np++ = (char)(imod[2]+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]); 
								}
							}
						else
						if ((Rp[modifier[1]].Offset >= 1) && (Rp[modifier[2]].Offset == 0))
							{
							/* mov  edx,dword ptr Rp[Am3]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset == 1)
								{
								/* fst  qword ptr [edx+(Am2*8)]	*/ *Np++ = imod[0]; *Np++ = 0x14; *Np++ = (char)(imod[2]+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]); 
								}
							else
								{
								/* fstp qword ptr [edx+(Am2*8)]	*/ *Np++ = imod[0]; *Np++ = 0x1C; *Np++ = (char)(imod[2]+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]); 
								}
							}
						else
						if ((Rp[modifier[1]].Offset == 0) && (Rp[modifier[2]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am2]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset == 1)
								{
								/* fst  qword ptr [Am3+(edx*8)]	*/ *Np++ = imod[0]; *Np++ = 0x14; *Np++ = (char)(imod[2]+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]); 
								}
							else
								{
								/* fstp qword ptr [Am3+(edx*8)]	*/ *Np++ = imod[0]; *Np++ = 0x1C; *Np++ = (char)(imod[2]+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]); 
								}
							}
						}
					break;

				case vmregSetFloat:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					imod[0]=0xD9;
					goto regSetNumber;        
					break;

				case vmregSetXFloat:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					imod[0]=0xD9;imod[1]=BITSIZEOFFLOAT;imod[2]=0x80;
					goto regSetXNumber;        
					break;

				case vmregRefXCharacter:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov   edx,dword ptr Rp[Am2]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* add   edx,dword ptr Rp[Am1]       */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* movzx edx,[edx]					 */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = 0x12;
						/* mov   dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0) && (Rp[modifier[2]].Offset == 0))
						{
						/* mov   edx,dword ptr Rp[Am2]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* add   edx,dword ptr Rp[Am1]       */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* movzx edx,[edx]					 */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = 0x12;
						/* mov   dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0) && (Rp[modifier[2]].Offset >= 1))
							{
							/* mov edx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* add edx,dword ptr Rp[Am1]         */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* mov Reg[Am3],[edx]				 */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = REGBEDX[Rp[modifier[2]].Offset];
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[Am1+(Am2*1)]		 */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x00+REGBASE[Rp[modifier[0]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* mov edx,[Am1+(Am2*4)]		     */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x00+REGBASE[Rp[modifier[0]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov  edx,dword ptr Rp[Am2]		     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[Am1+(edx*1)]		 */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x00+REGBASE[Rp[modifier[0]].Offset]+REGTARG[6]);
								}
							else
								{
								/* mov edx,[Am1+(edx*1)]		     */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x00+REGBASE[Rp[modifier[0]].Offset]+REGTARG[6]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am1]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[edx+(Am2*1)]		 */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x00+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* mov edx,[edx+(Am2*1)]		     */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x00+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						}
					break;

				case vmregRefXShort:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov   edx,dword ptr Rp[Am2]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl   edx,1                       */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFSHORT; 	
						/* add   edx,dword ptr Rp[Am1]       */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* movsx edx,[edx]					 */ *Np++ = 0x0F;	*Np++ = 0xBF;	*Np++ = 0x12;
						/* mov   dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0) && (Rp[modifier[2]].Offset == 0))
						{
						/* mov   edx,dword ptr Rp[Am2]       */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl   edx,1                       */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFSHORT; 	
						/* add   edx,dword ptr Rp[Am1]       */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* movsx edx,[edx]					 */ *Np++ = 0x0F;	*Np++ = 0xBF;	*Np++ = 0x12;
						/* mov   dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0) && (Rp[modifier[2]].Offset >= 1))
							{
							/* mov edx,dword ptr Rp[Am2]         */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* shl edx,1                         */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFSHORT; 	
							/* add edx,dword ptr Rp[Am1]         */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* mov Reg[Am3],[edx]				 */ *Np++ = 0x0F;	*Np++ = 0xBF;	*Np++ = REGBEDX[Rp[modifier[2]].Offset];
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[Am1+(Am2*2)]		 */ *Np++ = 0x0F;	*Np++ = 0xBF;	*Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x40+REGBASE[Rp[modifier[0]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* mov edx,[Am1+(Am2*2)]		     */ *Np++ = 0x0F;	*Np++ = 0xBF;	*Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x40+REGBASE[Rp[modifier[0]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov  edx,dword ptr Rp[Am2]		     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[Am1+(edx*2)]		 */ *Np++ = 0x0F;	*Np++ = 0xBF;	*Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x40+REGBASE[Rp[modifier[0]].Offset]+REGTARG[6]);
								}
							else
								{
								/* mov edx,[Am1+(edx*4)]		     */ *Np++ = 0x0F;	*Np++ = 0xBF;	*Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x40+REGBASE[Rp[modifier[0]].Offset]+REGTARG[6]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						else
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am1]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[2]].Offset >= 1)
								{
								/* mov Reg[Am3],[edx+(Am2*4)]		 */ *Np++ = 0x0F;	*Np++ = 0xBF;	*Np++ = (char)(0x04+REGTARG[Rp[modifier[2]].Offset]); *Np++ = (char)(0x40+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* mov edx,[edx+(Am2*2)]		     */ *Np++ = 0x0F;	*Np++ = 0xBF;	*Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x40+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								/* mov dword ptr Rp[Am3],edx	     */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								}
							}
						}
					break;

				case vmregSetCharImmediate:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov   edx,dword ptr Rp[pointer]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment			  ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* mov   byte ptr [edx],0x11    		*/ *Np++ = 0xC6; *Np++ = 0x02; *Np++ = (char)pcodes[n];
						}
					else
						{
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* mov   byte ptr [edx],0x11    			*/ *Np++ = 0xC6; *Np++ = 0x02; *Np++ = (char)pcodes[n];
							}
						else
						if (Rp[modifier[1]].Offset >= 1)
							{
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov byte ptr [Am2],0x11      			*/ *Np++ = 0xC6; *Np++ = (char)(REGBASE[Rp[modifier[1]].Offset]); *Np++ = (char)pcodes[n];
							}
						}
					break;

				case vmregSetXCharImmediate:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,Rp[Am2]                   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* add edx,Rp[Am3]                   */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));	Np+=4;
						/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* mov   byte ptr [edx],0x11         */ *Np++ = 0xC6; *Np++ = 0x02; *Np++ = (char)pcodes[n];
						}
					else
						{
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* add edx,dword ptr Rp[Am3]				*/ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* mov byte ptr [edx],0x11      			*/ *Np++ = 0xC6; *Np++ = 0x02; *Np++ = (char)pcodes[n];
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov byte ptr [Am3+(Am2*1)],0x11      	*/ *Np++ = 0xC6; *Np++ = 0x04; *Np++ = (char)(0x00+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]); *Np++ = (char)pcodes[n];
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov dword ptr [Am3+(edx*1)],0x11     	*/ *Np++ = 0xC6; *Np++ = 0x04; *Np++ = (char)(0x00+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]); *Np++ = (char)pcodes[n];
							}
						else
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am3]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov dword ptr [edx+(Am2*1)],0x11      	*/ *Np++ = 0xC6; *Np++ = 0x04; *Np++ = (char)(0x00+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]); *Np++ = (char)pcodes[n];
							}
						}
					break;

				case vmregSetIntImmediate:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov   edx,dword ptr Rp[pointer]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment			  ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* mov   dword ptr [edx],0x11223344		*/ *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (NUM)pcodes[n];Np+=4;
						}
					else
						{
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* mov   dword ptr [edx],0x11223344			*/ *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (NUM)pcodes[n];Np+=4;
							}
						else
						if (Rp[modifier[1]].Offset >= 1)
							{
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov dword ptr [Am2],0x11223344			*/ *Np++ = 0xC7; *Np++ = (char)(REGBASE[Rp[modifier[1]].Offset]); *(LpNUM)Np = (NUM)pcodes[n];Np+=4;
							}
						}
					break;

				case vmregSetXIntImmediate:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,Rp[Am2]                   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* shl edx,BITSIZEOFNUM              */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFNUM;
						/* add edx,Rp[Am3]                   */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));	Np+=4;
						/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* mov   dword ptr [edx],0x11223344  */ *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (NUM)pcodes[n];Np+=4;
						}
					else
						{
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* shl edx,BITSIZEOFNUM						*/ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFNUM; 	
							/* add edx,dword ptr Rp[Am3]				*/ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* mov   dword ptr [edx],0x11223344			*/ *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (NUM)pcodes[n];Np+=4;
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov dword ptr [Am3+(Am2*4)],0x11223344	*/ *Np++ = 0xC7; *Np++ = 0x04; *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]); *(LpNUM)Np = (NUM)pcodes[n];Np+=4;
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov dword ptr [Am3+(edx*4)],0x11223344	*/ *Np++ = 0xC7; *Np++ = 0x04; *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]); *(LpNUM)Np = (NUM)pcodes[n];Np+=4;
							}
						else
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am3]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov dword ptr [edx+(Am2*4)],0x11223344	*/ *Np++ = 0xC7; *Np++ = 0x04; *Np++ = (char)(0x80+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]); *(LpNUM)Np = (NUM)pcodes[n];Np+=4;
							}
						}
					break;

				case vmregSetLongImmediate:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov   edx,dword ptr Rp[pointer]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment			  ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* mov   dword ptr [edx],0x11223344		*/ *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (int)pcodes[n];Np+=4;
						}
					else
						{
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* mov   dword ptr [edx],0x11223344			*/ *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (int)pcodes[n];Np+=4;
							}
						else
						if (Rp[modifier[1]].Offset >= 1)
							{
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov dword ptr [Am2],0x11223344			*/ *Np++ = 0xC7; *Np++ = (char)(REGBASE[Rp[modifier[1]].Offset]); *(LpNUM)Np = (int)pcodes[n];Np+=4;
							}
						}
					break;

				case vmregSetXLongImmediate:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,Rp[Am2]                   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* shl edx,BITSIZEOFNUM              */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFNUM;
						/* add edx,Rp[Am3]                   */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));	Np+=4;
						/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* mov   dword ptr [edx],0x11223344  */ *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (int)pcodes[n];Np+=4;
						}
					else
						{
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* shl edx,BITSIZEOFNUM						*/ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFNUM; 	
							/* add edx,dword ptr Rp[Am3]				*/ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* mov   dword ptr [edx],0x11223344			*/ *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (int)pcodes[n];Np+=4;
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov dword ptr [Am3+(Am2*4)],0x11223344	*/ *Np++ = 0xC7; *Np++ = 0x04; *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]); *(LpNUM)Np = (NUM)pcodes[n];Np+=4;
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov dword ptr [Am3+(edx*4)],0x11223344	*/ *Np++ = 0xC7; *Np++ = 0x04; *Np++ = (char)(0x80+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]); *(LpNUM)Np = (NUM)pcodes[n];Np+=4;
							}
						else
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am3]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov dword ptr [edx+(Am2*4)],0x11223344	*/ *Np++ = 0xC7; *Np++ = 0x04; *Np++ = (char)(0x80+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]); *(LpNUM)Np = (NUM)pcodes[n];Np+=4;
							}
						}
					break;

				case vmregSetShortImmediate:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov   edx,dword ptr Rp[pointer]		*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment			  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
						/* mov   word ptr [edx],0x1122   		*/ *Np++ = 0x66; *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (short)pcodes[n];Np+=2;
						}
					else
						{
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov word ptr [edx],0x1122				*/ *Np++ = 0x66; *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (short)pcodes[n];Np+=2;
							}
						else
						if (Rp[modifier[1]].Offset >= 1)
							{
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov word ptr [Am2],0x1122     			*/ *Np++ = 0x66; *Np++ = 0xC7; *Np++ = (char)(REGBASE[Rp[modifier[1]].Offset]); *(LpNUM)Np = (short)pcodes[n];Np+=2;
							}
						}
					break;

				case vmregSetXShortImmediate:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,Rp[Am2]                   */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* shl edx,BITSIZEOFSHORT            */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFSHORT;
						/* add edx,Rp[Am3]                   */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));	Np+=4;
						/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
						/* mov word ptr [edx],0x1122	     */ *Np++ = 0x66; *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (short)pcodes[n];Np+=2;
						}
					else
						{
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* shl edx,BITSIZEOFSHORT					*/ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFSHORT; 	
							/* add edx,dword ptr Rp[Am3]				*/ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+3); 
							/* mov word ptr [edx],0x1122     			*/ *Np++ = 0x66; *Np++ = 0xC7; *Np++ = 0x02; *(LpNUM)Np = (short)pcodes[n];Np+=2;
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+4); 
							/* mov word ptr [Am3+(Am2*2)],0x1122     	*/ *Np++ = 0x66; *Np++ = 0xC7; *Np++ = 0x04; *Np++ = (char)(0x40+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]); *(LpNUM)Np = (short)pcodes[n];Np+=2;
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+4); 
							/* mov word ptr [Am3+(edx*2)],0x1122   	    */ *Np++ = 0x66; *Np++ = 0xC7; *Np++ = 0x04; *Np++ = (char)(0x40+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]); *(LpNUM)Np = (short)pcodes[n];Np+=2;
							}
						else
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am3]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment				  ***/ Jp[++n] = (NUM)((Np-ncodes)+4); 
							/* mov word ptr [edx+(Am2*2)],0x1122    	*/ *Np++ = 0x66; *Np++ = 0xC7; *Np++ = 0x04; *Np++ = (char)(0x40+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]); *(LpNUM)Np = (short)pcodes[n];Np+=2;
							}
						}
					break;

				case vmregSetCharacter:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov   ecx,dword ptr Rp[pointer]		*/ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/* movsx eax,byte ptr Rp[source]		*/ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov   byte ptr [ecx],al				*/ *Np++ = 0x88; *Np++ = 0x01;
						}
					else
						{
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov edx,dword ptr Rp[Am2]             */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[0]].Offset <= 3))
								{
								/* mov byte ptr [edx],Reg[Am1]		 */ *Np++ = 0x88;	*Np++ = REGBEDX[Rp[modifier[0]].Offset];
								}
							else
							if (Rp[modifier[0]].Offset >= 4)
								{
								REGSAVEEBX
								/* mov ebx,Reg[Am1]					 */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[3]+REGREGR[Rp[modifier[0]].Offset]); 
								/* mov byte ptr [edx],bl		     */ *Np++ = 0x88;	*Np++ = 0x1A;
								REGLOADEBX
								}
							else
								{
								REGSAVEEBX
								/* mov ebx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov byte ptr [edx],bl			 */ *Np++ = 0x88;	*Np++ = 0x1A;
								REGLOADEBX
								}
							}
						else
							{
							if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[0]].Offset <= 3))
								{
								/* mov byte ptr [Am2],Reg[Am1]          */ *Np++ = 0x88; *Np++ = (char)(char)(0x00+REGBASE[Rp[modifier[1]].Offset]+REGTARG[Rp[modifier[0]].Offset]);
								}
							else
							if (Rp[modifier[0]].Offset >= 4)
								{
								/* mov edx,Reg[Am1]						*/ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6]+REGREGR[Rp[modifier[0]].Offset]); 
								/* mov byte ptr [Am2],dl                */ *Np++ = 0x88; *Np++ = (char)(char)(0x00+REGBASE[Rp[modifier[1]].Offset]+REGTARG[6]);
								}
							else
								{
								/* mov edx,dword ptr Rp[Am1]			*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov byte ptr [Am2],dl                */ *Np++ = 0x88; *Np++ = (char)(char)(0x00+REGBASE[Rp[modifier[1]].Offset]+REGTARG[6]);
								}
							}
						}
					break;

				case vmregSetXCharacter:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov   ecx,Rp[index]              */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* add   ecx,Rp[pointer]            */ *Np++ = 0x03; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));	Np+=4;
						/* movsx edx,byte ptr Rp[source]    */ *Np++ = 0x0F; *Np++ = 0xBE; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov   byte ptr [ecx],al          */ *Np++ = 0x88; *Np++ = 0x11;
						}
					else
						{
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]             */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* add edx,dword ptr Rp[Am3]             */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[0]].Offset <= 3))
								{
								/* mov byte ptr [edx],Reg[Am1]		 */ *Np++ = 0x88;	*Np++ = REGBEDX[Rp[modifier[0]].Offset];
								}
							else
							if (Rp[modifier[0]].Offset >= 4)
								{
								REGSAVEEBX
								/* mov ebx,Reg[Am1]					 */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[3]+REGREGR[Rp[modifier[0]].Offset]); 
								/* mov byte ptr [edx],Reg[Am1]		 */ *Np++ = 0x88;	*Np++ = REGBEDX[Rp[modifier[0]].Offset];
								REGLOADEBX
								}
							else
								{
								REGSAVEEBX
								/* mov ebx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov byte ptr [edx],bl			 */ *Np++ = 0x88;	*Np++ = 0x1A;
								REGLOADEBX
								}
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[0]].Offset <= 3))
								{
								/* mov byte ptr [Am3+(Am2*1)],Reg[Am1]	*/ *Np++ = 0x88; *Np++ = (char)(0x04+REGTARG[Rp[modifier[0]].Offset]); *Np++ = (char)(0x00+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
							if (Rp[modifier[0]].Offset >= 4)
								{
								/* mov edx,Reg[Am1]						*/ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6]+REGREGR[Rp[modifier[0]].Offset]); 
								/* mov byte ptr [Am3+(Am2*1)],dl    	*/ *Np++ = 0x88; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x00+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* mov edx,dword ptr Rp[Am1]			*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov byte ptr [Am3+(Am2*1)],dl		*/ *Np++ = 0x88; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x00+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							}
						else
							{
							if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
								{
								/* mov  edx,dword ptr Rp[Am2]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
								/* add  edx,Reg[Am3]						*/ *Np++ = 0x03; *Np++ = (char)(REGREGL[6]+REGREGR[Rp[modifier[2]].Offset]); 
								}
							else
							if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
								{
								/* mov  edx,dword ptr Rp[Am3]				*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
								/* add  edx,Reg[Am2]						*/ *Np++ = 0x03; *Np++ = (char)(REGREGL[6]+REGREGR[Rp[modifier[1]].Offset]); 
								}

							if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[0]].Offset <= 3))
								{
								/* mov byte ptr [edx],Reg[Am1]			*/ *Np++ = 0x88; *Np++ = (char)(0x02+REGTARG[Rp[modifier[0]].Offset]);
								}
							else
							if (Rp[modifier[0]].Offset >= 4)
								{
								REGSAVEEBX
								/* mov ebx,Reg[Am1]						*/ *Np++ = 0x8B; *Np++ = (char)(REGREGL[3]+REGREGR[Rp[modifier[0]].Offset]); 
								/* mov byte ptr [edx],bl    			*/ *Np++ = 0x88; *Np++ = (char)(0x02+REGTARG[3]);
								REGLOADEBX
								}
							else
								{
								REGSAVEEBX
								/* mov ebx,dword ptr Rp[Am1]			*/ *Np++ = 0x8B; *Np++ = REGRPMOD[3]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov byte ptr [edx],bl    			*/ *Np++ = 0x88; *Np++ = (char)(0x02+REGTARG[3]);
								REGLOADEBX
								}
							}
						}
					break;

				case vmregSetXShort:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,Rp[index]                */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* shl edx,BITSIZEOFSHORT           */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFSHORT;
						/* add edx,Rp[pointer]              */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));	Np+=4;
						/* movsx eax,word ptr Rp[source]    */ *Np++ = 0x0F; *Np++ = 0xBF; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* mov   word ptr [edx],ax          */ *Np++ = 0x66; *Np++ = 0x89; *Np++ = 0x02;
						}
					else
						{
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov edx,dword ptr Rp[Am2]             */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* shl edx,BITSIZEOFSHORT                */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFSHORT; 	
							/* add edx,dword ptr Rp[Am3]             */ *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov word ptr [edx],Reg[Am1]		 */ *Np++ = 0x66; *Np++ = 0x89;	*Np++ = REGBEDX[Rp[modifier[0]].Offset];
								}
							else
								{
								REGSAVEEDI
								/* mov edi,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov word ptr [edx],edi			 */ *Np++ = 0x66;	*Np++ = 0x89;	*Np++ = 0x3A;
								REGLOADEDI
								}
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
							{
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [Am3+(Am2*4)],Reg[Am1]		 */ *Np++ = 0x66; *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[Rp[modifier[0]].Offset]); *Np++ = (char)(0x40+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								/* mov edx,dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [Am3+(Am2*4)],edx		     */ *Np++ = 0x66; *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[6]); *Np++ = (char)(0x40+REGBASE[Rp[modifier[2]].Offset]+REGTARG[Rp[modifier[1]].Offset]);
								}
							}
						else
						if ((Rp[modifier[2]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* mov  edx,dword ptr Rp[Am2]		     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [Am3+(edx*4)],Reg[Am1]		 */ *Np++ = 0x66; *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[Rp[modifier[0]].Offset]); *Np++ = (char)(0x40+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]);
								}
							else
								{
								if (Rp[modifier[2]].Offset != 5) k = 5; else k = 4; 
								if (Ir[k] != 0) 
									{
									/* mov dword ptr Rp[Ir[k]],Reg[n]*/ *Np++ = 0x89; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								/* mov Reg[n],dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [Am3+(edx*4)],Reg[n]		     */ *Np++ = 0x66; *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[k]); *Np++ = (char)(0x40+REGBASE[Rp[modifier[2]].Offset]+REGTARG[6]);
								if (Ir[k] != 0) 
									{
									/* mov Reg[k],dword ptr Rp[Ir[k]]*/ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								}
							}
						else
						if ((Rp[modifier[2]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* mov  edx,dword ptr Rp[Am3]		     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							if (Rp[modifier[0]].Offset >= 1)
								{
								/* mov [edx+(Am2*4)],Reg[Am1]		 */ *Np++ = 0x66; *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[Rp[modifier[0]].Offset]); *Np++ = (char)(0x40+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								}
							else
								{
								if (Rp[modifier[1]].Offset != 5) k = 5; else k = 4; 
								if (Ir[k] != 0) 
									{
									/* mov dword ptr Rp[Ir[k]],Reg[k]*/ *Np++ = 0x89; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								/* mov Reg[k],dword ptr Rp[Am1]	     */ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
								/* mov [edx+(Am2*4)],Reg[k]		     */ *Np++ = 0x66; *Np++ = 0x89; *Np++ = (char)(0x04+REGTARG[k]); *Np++ = (char)(0x40+REGBASE[6]+REGTARG[Rp[modifier[1]].Offset]);
								if (Ir[k] != 0) 
									{
									/* mov Reg[k],dword ptr Rp[Ir[k]]*/ *Np++ = 0x8B; *Np++ = REGRPMOD[k]; *(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD)); Np+=4;
									}
								}
							}
						}
					break;

				case vmregShlInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					imodCount = 2; imod[0] = 0xA5; imod[1] = 0xE0;
					ShlIntegerReg:
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** No hardware registers		  ***/
						/* mov  cl,byte ptr Rp[source]      */ *Np++ = 0x8A; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl  Rp[target],cl               */ *Np++ = 0xD3; *Np++ = imod[0]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
						{
						/* mov  edx,ecx						*/ *Np++ = 0x8B; *Np++ = 0xD1;
						/* mov  cl,byte ptr Rp[source]      */ *Np++ = 0x8A; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl  Rp[target],cl               */ *Np++ = 0xD3; *Np++ = imod[0]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* mov  ecx,edx						*/ *Np++ = 0x8B; *Np++ = 0xCA;
						}
					else
					if ((Rp[modifier[0]].Offset == 2) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in j register 		  ***/
						/*** target in i register	      ***/
						/* shl  Reg[target],cl              */ *Np++ = 0xD3; *Np++ = (char)(imod[1] + REGREGR[Rp[modifier[1]].Offset]);
						}
					else
					if ((Rp[modifier[0]].Offset == 2) && (Rp[modifier[1]].Offset == 0))
						{
						/*** source in j register 		  ***/
						/*** target in memory   	      ***/
						/* shl  Rp[target],cl               */ *Np++ = 0xD3; *Np++ = imod[0]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 2))
						{
						/*** source in j register 		  ***/
						/*** target in ecx register	      ***/
						/* mov  edx,ecx						*/ *Np++ = 0x8B; *Np++ = 0xD1;
						/* mov  ecx,Reg[source]				*/ *Np++ = 0x8B; *Np++ = (char)(REGREGL[2] + REGREGR[Rp[modifier[0]].Offset]); 
						/* shl  edx,cl						*/ *Np++ = 0xD3; *Np++ = (char)(imod[1] + REGREGR[6]);
						/* mov  ecx,edx						*/ *Np++ = 0x8B; *Np++ = 0xCA;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 2))
						{
						/*** source in memory	 		  ***/
						/*** target in ecx register	      ***/
						/* mov  edx,ecx						*/ *Np++ = 0x8B; *Np++ = 0xD1;
						/* mov  cl,byte ptr Rp[source]      */ *Np++ = 0x8A; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl  Rp[target],cl               */ *Np++ = 0xD3; *Np++ = imod[0]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* mov  ecx,edx						*/ *Np++ = 0x8B; *Np++ = 0xCA;
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in j register 		  ***/
						/*** target in i register	      ***/
						/* mov  edx,ecx						*/ *Np++ = 0x8B; *Np++ = 0xD1;
						/* mov  ecx,Reg[source]				*/ *Np++ = 0x8B; *Np++ = (char)(REGREGL[2] + REGREGR[Rp[modifier[0]].Offset]); 
						/* shl  Reg[target],cl              */ *Np++ = 0xD3; *Np++ = (char)(imod[1] + REGREGR[Rp[modifier[1]].Offset]);
						/* mov  ecx,edx						*/ *Np++ = 0x8B; *Np++ = 0xCA;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
						{
						/*** source in memory		 	  ***/
						/*** target in i register	      ***/
						/* mov  edx,ecx						*/ *Np++ = 0x8B; *Np++ = 0xD1;
						/* mov  cl,byte ptr Rp[source]      */ *Np++ = 0x8A; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* shl  Reg[target],cl              */ *Np++ = 0xD3; *Np++ = (char)(imod[1] + REGREGR[Rp[modifier[1]].Offset]);
						/* mov  ecx,edx						*/ *Np++ = 0x8B; *Np++ = 0xCA;
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
						{
						/*** source in j register	 	  ***/
						/*** target in memory		      ***/
						/* mov  edx,ecx						*/ *Np++ = 0x8B; *Np++ = 0xD1;
						/* mov  ecx,Reg[source]				*/ *Np++ = 0x8B; *Np++ = (char)(REGREGL[2] + REGREGR[Rp[modifier[0]].Offset]); 
						/* shl  Rp[target],cl               */ *Np++ = 0xD3; *Np++ = imod[0]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* mov  ecx,edx						*/ *Np++ = 0x8B; *Np++ = 0xCA;
						}
					break;

				case vmregShrInteger:
					regOutStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					imodCount = 2; imod[0] = 0xAD; imod[1] = 0xE8;
					goto ShlIntegerReg;
					break;

				case vmregJmpLTNumber:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x74; imod[2] = 0x01;
					RegJmpCCNumber:
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* fld    qword ptr Rp[source]			*/ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fcomp  qword ptr Rp[comparitor]		*/ *Np++ = 0xDC; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
						{
						/* fld    qword ptr Rp[source]			*/ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/* fcomp  qword ptr Rp[comparitor]		*/ *Np++ = 0xDC; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if ((Rp[modifier[0]].Offset == 1) && (Rp[modifier[1]].Offset >= 1))
							{
							/* fcom st(i)						*/ *Np++ = 0xD8; *Np++ = (char)(0xCF+Rp[modifier[1]].Offset);
							}
						else
						if ((Rp[modifier[0]].Offset > 1) && (Rp[modifier[1]].Offset >= 1))
							{
							/* fld st(i)						*/ *Np++ = 0xD9; *Np++ = (char)(0xBF+Rp[modifier[0]].Offset);
							/* fcomp st(j)						*/ *Np++ = 0xD8; *Np++ = (char)(0xD8+Rp[modifier[1]].Offset);
							}
						else
						if ((Rp[modifier[0]].Offset == 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* fcom   qword ptr Rp[comparitor]  */ *Np++ = 0xDC; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
							{
							/* fld st(i)						*/ *Np++ = 0xD9; *Np++ = (char)(0xBF+Rp[modifier[0]].Offset);
							/* fcomp  qword ptr Rp[comparitor]	*/ *Np++ = 0xDC; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
							{
							/* fld    qword ptr Rp[source]		*/ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/* fcomp  st(j)						*/ *Np++ = 0xD8; *Np++ = (char)(0xD8+Rp[modifier[1]].Offset);
							}
						}
					/* xchg eax,edx						*/ *Np++ = 0x92; 
					/* fnstsw ax                        */ *Np++ = 0xDF; *Np++ = 0xE0; 
					/* xchg eax,edx						*/ *Np++ = 0x92; 
					if( pcode.u.Pcode == vmregJmpNENumber ) {
					/* test   dh,0x44					*/ *Np++ = 0xF6; *Np++ = 0xC6; *Np++ = 0x44; 
					/* jp                               */ *Np++ = 0x7A; *Np++ = 0x05;
					}
					/* test   dh,0x01					*/ *Np++ = 0xF6; *Np++ = 0xC6; *Np++ = imod[2]; 
					/* je                               */ *Np++ = imod[1]; *Np++ = 0x05;
					/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
					/*** Set relative jmp lbl         ***/ Mp[n]   = -2;                     
					/* jmp label                        */ *Np++ = 0xE9; *(LpNUM)Np = pcodes[n]; Np+=4;
					break;

				case vmregJmpLENumber:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x74; imod[2] = 0x41;
					goto RegJmpCCNumber;
					break;

				case vmregJmpEQNumber:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x74; imod[2] = 0x40;
					goto RegJmpCCNumber;
					break;

				case vmregJmpNENumber:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x75; imod[2] = 0x40;
					goto RegJmpCCNumber;
					break;

				case vmregJmpGENumber:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x75; imod[2] = 0x01;
					goto RegJmpCCNumber;
					break;

				case vmregJmpGTNumber:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					imod[1] = 0x75; imod[2] = 0x41;
					goto RegJmpCCNumber;
					break;

				/* Register opcodes with one label and one register argument. */
				case vmregLoadJmpPointer:
					regOutStream(JITOpJa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        

					SAVEALLREGS

					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */
					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					Mp[n] = -1; /*  Mark this jump label argument as needing translation on the second pass. */
					*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM pcode argument to the native code vector. */

					LOADALLREGS
					break;

				case vmregAndImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					imod[0] = 0x81; imod[1] = 0xA5; imod[2] = 0xE0;
					goto regAddImmediate;        
					break;

				case vmregMulImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov edx,Rp[Am2]                  */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
						/* imul edx,immediate               */ *Np++ = 0x69; *Np++ = 0xD2; *(LpNUM)Np = pcodes[n]; Np+=4;
						/* mov  Rp[Am2],edx                 */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
					if (Rp[modifier[1]].Offset == 0)
						{
						/* mov edx,Rp[Am2]                  */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
						/* imul edx,immediate               */ *Np++ = 0x69; *Np++ = 0xD2; *(LpNUM)Np = pcodes[n]; Np+=4;
						/* mov  Rp[Am2],edx                 */ *Np++ = 0x89; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2);
						/* imul Reg[Am2],immediate          */ *Np++ = 0x69; *Np++ = MULREGI[Rp[modifier[1]].Offset]; *(LpNUM)Np = pcodes[n]; Np+=4;
						}
					break;

				case vmregOrImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					imod[0] = 0x81; imod[1] = 0x8D; imod[2] = 0xC8;
					goto regAddImmediate;        
					break;

				case vmregSubImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					imod[0] = 0x81; imod[1] = 0xAD; imod[2] = 0xE8;
					goto regAddImmediate;        
					break;

				case vmregXorImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					imod[0] = 0x81; imod[1] = 0xB5; imod[2] = 0xF0;
					goto regAddImmediate;        
					break;

				case vmregShlImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					imod[0] = 0xC1; imod[1] = 0xA5; imod[2] = 0xE0;
					regShlImmediate:        
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+6); 
						/* shl dword ptr Rp[Am2],immediate  */ *Np++ = imod[0]; *Np++ = imod[1]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4; *Np++ = (unsigned char)pcodes[n];
						}
					else
					if (Rp[modifier[1]].Offset == 0)
						{
						/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+6); 
						/* shl dword ptr Rp[Am2],immediate  */ *Np++ = imod[0]; *Np++ = imod[1]; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4; *Np++ = (unsigned char)pcodes[n];
						}
					else
						{
						/*** Set Pc/Nc allignment		   ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* shl Reg[Am1],immediate			 */ *Np++ = imod[0]; *Np++ = (imod[2]+REGREGR[Rp[modifier[1]].Offset]); *Np++ = (unsigned char)pcodes[n];
						}
					break;

				case vmregShrImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);
					imod[0] = 0xC1; imod[1] = 0xAD; imod[2] = 0xE8;
					goto regShlImmediate;        
					break;

				case vmregDivrImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        

					SAVEALLREGS

					saveNativeJumpBack(pcode);		/*  Move the "call" to the native code vector.						*/
					Jp[++n] = (NUM)(Np-ncodes);		/*  Update the jump label translation vector.						*/
					*(LpNUM)Np = pcodes[n];Np+=4;	/*  Move the original VM pcode argument to the native code vector.	*/

					LOADALLREGS
					break;

				case vmregSetXWord:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
							/* mov ecx,dword ptr [ebp+Am3]     */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
							/* mov edx,dword ptr [ebp+Am2]     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							/* shl edx,4                       */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = 0x04;
							/* add ecx,edx                     */ *Np++ = 0x03; *Np++ = 0xCA;
							/* mov edx,dword ptr [ebp+Am1]     */ *Np++ = 0x8B; *Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
							/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 	
							/* add edx,11223344h               */ if (pcodes[n]!=0){*Np++ = 0x81; *Np++ = 0xC2; *(LpNUM)Np = pcodes[n];Np+=4;}
							// Move the first block
							/* mov esi,dword ptr [edx+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 0; Np+=4;
							/* mov dword ptr [ecx], esi        */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,dword ptr [edx+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 4; Np+=4;
							/* mov dword ptr [ecx+4], esi      */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,dword ptr [edx+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 8; Np+=4;
							/* mov dword ptr [ecx+8], esi      */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,dword ptr [edx+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 12; Np+=4;
							/* mov dword ptr [ecx+12], esi     */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 12; Np+=4;
						}
					else
					{
						if (Rp[modifier[2]].Offset == 0)
						{
							/* mov edx,dword ptr [ebp+Am3]     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
							if (Rp[modifier[1]].Offset == 0)
							{
								REGSAVEECX
								/* mov ecx,dword ptr [ebp+Am2] */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
								/* shl ecx,4                   */ *Np++ = 0xC1; *Np++ = 0xE1; *Np++ = 0x04;
								/* add edx,ecx                 */ *Np++ = 0x03; *Np++ = 0xD1;
								REGLOADECX
							}
							else
							{
								// Shift the index value by 4 and then return it after adding the index
								/* shl Am2,4                   */ *Np++ = 0xC1; *Np++ = 0xE0 + REGREGR[Rp[modifier[1]].Offset]; *Np++ = 0x04;
								/* add edx,Am2                 */ *Np++ = 0x03; *Np++ = 0xD0 + REGREGR[Rp[modifier[1]].Offset];
								/* shr Am2,4                   */ *Np++ = 0xC1; *Np++ = 0xE8 + REGREGR[Rp[modifier[1]].Offset]; *Np++ = 0x04;
							}
							REGSAVEECX
							if (Rp[modifier[0]].Offset == 0)
							{
								/* mov ecx,dword ptr [ebp+Am1] */ *Np++ = 0x8B; *Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
							}
							else
							{
								/* mov ecx,RAm3                */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[RECX] + REGREGR[Rp[modifier[0]].Offset]);
							}
							/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 	
							/* add ecx,11223344h               */ if (pcodes[n]!=0){*Np++ = 0x81; *Np++ = 0xC1; *(LpNUM)Np = pcodes[n];Np+=4;}
							REGSAVEESI
							// Move the first block
							/* mov esi,dword ptr [ecx]         */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 0; Np+=4;
							/* mov dword ptr [edx], esi        */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,dword ptr [ecx+4]       */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 4; Np+=4;
							/* mov dword ptr [edx+4], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,dword ptr [ecx+8]       */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 8; Np+=4;
							/* mov dword ptr [edx+8], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,dword ptr [ecx+12]      */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 12; Np+=4;
							/* mov dword ptr [edx+12], esi     */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 12; Np+=4;
							REGLOADESI
							REGLOADECX
						}
						else
						{
							/* mov edx,Am3                           */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[REDX] + REGREGR[Rp[modifier[2]].Offset]);
							if (Rp[modifier[1]].Offset == 0)
							{	
								REGSAVEECX
								/* mov ecx,dword ptr [ebp+Am2]       */ *Np++ = 0x8B; *Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
								/* shl ecx,4                         */ *Np++ = 0xC1; *Np++ = 0xE1; *Np++ = 0x04;
								/* add edx,ecx                       */ *Np++ = 0x03; *Np++ = 0xD1;
								REGLOADECX	
							}
							else
							{
								// Shift the index value by 4 and then return it after adding the index
								/* shl Am2,4                         */ *Np++ = 0xC1; *Np++ = 0xE0 + REGREGR[Rp[modifier[1]].Offset]; *Np++ = 0x04;
								/* add edx,Am2                       */ *Np++ = 0x03; *Np++ = 0xD0 + REGREGR[Rp[modifier[1]].Offset];
								/* shr Am2,4                         */ *Np++ = 0xC1; *Np++ = 0xE8 + REGREGR[Rp[modifier[1]].Offset]; *Np++ = 0x04;
							}

							if (Rp[modifier[0]].Offset == 0)
							{
								REGSAVEECX
								/* mov ecx,dword ptr [ebp+Am1]       */ *Np++ = 0x8B; *Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
							}
							else
							if (Rp[modifier[0]].Offset != RECX)
							{
								REGSAVEECX
								/* mov ecx,Am1          */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[RECX] + REGREGR[Rp[modifier[0]].Offset]);
							}

							/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 	
							/* add ecx,11223344h               */ if (pcodes[n]!=0){*Np++ = 0x81; *Np++ = 0xC1; *(LpNUM)Np = pcodes[n];Np+=4;}
							REGSAVEESI
							// Move the first block
							/* mov esi,dword ptr [ecx]         */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 0; Np+=4;
							/* mov dword ptr [edx], esi        */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,dword ptr [ecx+4]       */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 4; Np+=4;
							/* mov dword ptr [edx+4], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,dword ptr [ecx+8]       */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 8; Np+=4;
							/* mov dword ptr [edx+8], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,dword ptr [ecx+12]      */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 12; Np+=4;
							/* mov dword ptr [edx+12], esi     */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 12; Np+=4;
							REGLOADESI
							// Check if we need to reload ecx
							if((Rp[modifier[0]].Offset == 0) || (Rp[modifier[0]].Offset != RECX))
							{
								REGLOADECX
							}
						}
						}
				break;

				case vmregRefXWord:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
							/* mov ecx,dword ptr [ebp+Am1]     */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
							/* mov edx,dword ptr [ebp+Am2]     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							/* shl edx,4                       */ *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = 0x04;
							/* add ecx,edx                     */ *Np++ = 0x03; *Np++ = 0xCA;
							/* mov edx,dword ptr [ebp+Am3]     */ *Np++ = 0x8B; *Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
							/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 	
							/* add edx,11223344h               */ if (pcodes[n]!=0){*Np++ = 0x81; *Np++ = 0xC2; *(LpNUM)Np = pcodes[n];Np+=4;}
							// Move the first block
							/* mov esi,dword ptr [ecx+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 0; Np+=4;
							/* mov dword ptr [edx], esi        */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,dword ptr [ecx+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 4; Np+=4;
							/* mov dword ptr [edx+4], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,dword ptr [ecx+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 8; Np+=4;
							/* mov dword ptr [edx+8], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,dword ptr [ecx+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 12; Np+=4;
							/* mov dword ptr [edx+12], esi     */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 12; Np+=4;
						}
					else
					{
						if (Rp[modifier[0]].Offset == 0)
						{
							/* mov edx,dword ptr [ebp+Am1]     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
							if (Rp[modifier[1]].Offset == 0)
							{
								REGSAVEECX
								/* mov ecx,dword ptr [ebp+Am2] */ *Np++ = 0x8B; *Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
								/* shl ecx,4                   */ *Np++ = 0xC1; *Np++ = 0xE1; *Np++ = 0x04;
								/* add edx,ecx                 */ *Np++ = 0x03; *Np++ = 0xD1;
								REGLOADECX
							}
							else
							{
								// Shift the index value by 4 and then return it after adding the index
								/* shl Am2,4                   */ *Np++ = 0xC1; *Np++ = 0xE0 + REGREGR[Rp[modifier[1]].Offset]; *Np++ = 0x04;
								/* add edx,Am2                 */ *Np++ = 0x03; *Np++ = 0xD0 + REGREGR[Rp[modifier[1]].Offset];
								/* shr Am2,4                   */ *Np++ = 0xC1; *Np++ = 0xE8 + REGREGR[Rp[modifier[1]].Offset]; *Np++ = 0x04;
							}
							REGSAVEECX
							if (Rp[modifier[2]].Offset == 0)
							{
								/* mov ecx,dword ptr [ebp+Am3] */ *Np++ = 0x8B; *Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
							}
							else
							{
								/* mov ecx,RAm3                */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[RECX] + REGREGR[Rp[modifier[2]].Offset]);
							}
							/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 	
							/* add ecx,11223344h               */ if (pcodes[n]!=0){*Np++ = 0x81; *Np++ = 0xC1; *(LpNUM)Np = pcodes[n];Np+=4;}
							REGSAVEESI
							// Move the first block
							/* mov esi,dword ptr [edx]         */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 0; Np+=4;
							/* mov dword ptr [ecx], esi        */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,dword ptr [edx+4]       */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 4; Np+=4;
							/* mov dword ptr [ecx+4], esi      */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,dword ptr [edx+8]       */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 8; Np+=4;
							/* mov dword ptr [ecx+8], esi      */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,dword ptr [edx+12]      */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 12; Np+=4;
							/* mov dword ptr [ecx+12], esi     */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 12; Np+=4;
							REGLOADESI
							REGLOADECX
						}
						else
						{
							/* mov edx,Am1                           */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[REDX] + REGREGR[Rp[modifier[0]].Offset]);
							if (Rp[modifier[1]].Offset == 0)
							{	
								REGSAVEECX
								/* mov ecx,dword ptr [ebp+Am2]       */ *Np++ = 0x8B; *Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
								/* shl ecx,4                         */ *Np++ = 0xC1; *Np++ = 0xE1; *Np++ = 0x04;
								/* add edx,ecx                       */ *Np++ = 0x03; *Np++ = 0xD1;
								REGLOADECX	
							}
							else
							{
								// Shift the index value by 4 and then return it after adding the index
								/* shl Am2,4                         */ *Np++ = 0xC1; *Np++ = 0xE0 + REGREGR[Rp[modifier[1]].Offset]; *Np++ = 0x04;
								/* add edx,Am2                       */ *Np++ = 0x03; *Np++ = 0xD0 + REGREGR[Rp[modifier[1]].Offset];
								/* shr Am2,4                         */ *Np++ = 0xC1; *Np++ = 0xE8 + REGREGR[Rp[modifier[1]].Offset]; *Np++ = 0x04;
							}

							if (Rp[modifier[2]].Offset == 0)
							{
								REGSAVEECX
								/* mov ecx,dword ptr [ebp+Am3]       */ *Np++ = 0x8B; *Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
							}
							else
							if (Rp[modifier[2]].Offset != RECX)
							{
								REGSAVEECX
								/* mov ecx,RAm3          */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[RECX] + REGREGR[Rp[modifier[2]].Offset]);
							}

							/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 	
							/* add ecx,11223344h               */ if (pcodes[n]!=0){*Np++ = 0x81; *Np++ = 0xC1; *(LpNUM)Np = pcodes[n];Np+=4;}
							REGSAVEESI
							// Move the first block
							/* mov esi,dword ptr [edx]         */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 0; Np+=4;
							/* mov dword ptr [ecx], esi        */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,dword ptr [edx+4]       */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 4; Np+=4;
							/* mov dword ptr [ecx+4], esi      */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,dword ptr [edx+8]       */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 8; Np+=4;
							/* mov dword ptr [ecx+8], esi      */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,dword ptr [edx+12]      */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 12; Np+=4;
							/* mov dword ptr [ecx+12], esi     */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 12; Np+=4;
							REGLOADESI
							// Check if we need to reload ecx
							if((Rp[modifier[2]].Offset == 0) || (Rp[modifier[2]].Offset != RECX))
							{
								REGLOADECX
							}
						}
						}
				break;

				case vmregRefWord:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
							/* mov edx,dword ptr [ebp+Am2]     */ *Np++ = 0x8B; *Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							/* mov ecx,dword ptr [ebp+Am1]     */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
							/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 	
							/* add edx,11223344h               */ if (pcodes[n]!=0){*Np++ = 0x81; *Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
							// Move the first block
							/* mov esi,dword ptr [ebp+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 0; Np+=4;
							/* mov dword ptr [edx], esi        */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,dword ptr [ebp+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 4; Np+=4;
							/* mov dword ptr [edx+4], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,dword ptr [ebp+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 8; Np+=4;
							/* mov dword ptr [edx+8], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,dword ptr [ebp+Am2]     */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 12; Np+=4;
							/* mov dword ptr [edx+12], esi     */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 12; Np+=4;
						}
					else
						{
						// Load the target to esi
						if (Rp[modifier[1]].Offset == 0)
						{
							/* mov edx,dword ptr [ebp+Am2]       */ *Np++ = 0x8B; *Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
						else
						{
							/* mov  edx,Reg[Am1]                 */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6] + REGREGR[Rp[modifier[1]].Offset]);
						}
						/*** Set Pc/Nc allignment              ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 	
						/* add edx,11223344h                     */ if(pcodes[n]!=0){*Np++ = 0x81; *Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						
						if (Rp[modifier[0]].Offset == 0)
						{
							REGSAVEESI
							REGSAVEECX
							/* mov ecx,dword ptr [ebp+Am1]     */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
							// Move the first block
							/* mov esi,dword ptr [ecx]         */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 0; Np+=4;
							/* mov dword ptr [edx], esi        */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,dword ptr [ecx+4]       */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 4; Np+=4;
							/* mov dword ptr [edx+4], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,dword ptr [ecx+8]       */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 8; Np+=4;
							/* mov dword ptr [edx+8], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,dword ptr [ecx+12]      */ *Np++ = 0x8B; *Np++ = 0xB1; *Np = 12; Np+=4;
							/* mov dword ptr [edx+12], esi     */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 12; Np+=4;
							REGLOADESI
							REGLOADECX
						}
						else
						if (Rp[modifier[0]].Offset == RESI)
						{
							REGSAVEECX
							// Move the first block
							/* mov ecx,qword ptr [esi]        */ *Np++ = 0x8B; *Np++ = 0x8E; *Np = 0; Np+=4;
							/* mov qword ptr [edx], ecx        */ *Np++ = 0x89; *Np++ = 0x8A; *Np = 0; Np+=4;
							// Move the second block
							/* mov ecx,qword ptr [esi+4]      */ *Np++ = 0x8B; *Np++ = 0x8E; *Np = 4; Np+=4;
							/* mov qword ptr [edx+4], ecx      */ *Np++ = 0x89; *Np++ = 0x8A; *Np = 4; Np+=4;
							// Move the third block
							/* mov ecx,qword ptr [esi+8]      */ *Np++ = 0x8B; *Np++ = 0x8E; *Np = 8; Np+=4;
							/* mov qword ptr [edx+8], ecx      */ *Np++ = 0x89; *Np++ = 0x8A; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov ecx,qword ptr [esi+12]     */ *Np++ = 0x8B; *Np++ = 0x8E; *Np = 12; Np+=4;
							/* mov qword ptr [edx+12], ecx     */ *Np++ = 0x89; *Np++ = 0x8A; *Np = 12; Np+=4;
							REGLOADECX
						}
						else
						{
							REGSAVEESI
							// Move the first block
							/* mov esi,qword ptr [RAm1]        */ *Np++ = 0x8B; *Np++ = (0xB0+REGREGR[Rp[modifier[0]].Offset]); *Np = 0; Np+=4;
							/* mov qword ptr [edx], esi        */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,qword ptr [RAm1+4]      */ *Np++ = 0x8B; *Np++ = (0xB0+REGREGR[Rp[modifier[0]].Offset]); *Np = 4; Np+=4;
							/* mov qword ptr [edx+4], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,qword ptr [RAm1+8]      */ *Np++ = 0x8B; *Np++ = (0xB0+REGREGR[Rp[modifier[0]].Offset]); *Np = 8; Np+=4;
							/* mov qword ptr [edx+8], esi      */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,qword ptr [RAm1+12]     */ *Np++ = 0x8B; *Np++ = (0xB0+REGREGR[Rp[modifier[0]].Offset]); *Np = 12; Np+=4;
							/* mov qword ptr [edx+12], esi     */ *Np++ = 0x89; *Np++ = 0xB2; *Np = 12; Np+=4;
							REGLOADESI
						}
						}
					break;

				case vmregSetWord:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
							/* mov edx,dword ptr [ebp+Am1]     */ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
							/* mov ecx,dword ptr [ebp+Am2]     */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							/*** Set Pc/Nc allignment        ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 	
							/* add edx,11223344h               */ if (pcodes[n]!=0){*Np++ = 0x81; *Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
							// Move the first block
							/* mov esi,dword ptr [edx]         */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 0; Np+=4;
							/* mov dword ptr [ecx], esi        */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,dword ptr [edx+4]       */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 4; Np+=4;
							/* mov dword ptr [ecx+4], esi      */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,dword ptr [edx+8]       */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 8; Np+=4;
							/* mov dword ptr [ecx+8], esi      */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,dword ptr [edx+12]      */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 12; Np+=4;
							/* mov dword ptr [ecx+12], esi     */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 12; Np+=4;
						}
					else
						{
						// Load the source to edx
						if (Rp[modifier[0]].Offset == 0)
						{
							/* mov edx,dword ptr [ebp+Am1]     */ *Np++ = 0x8B; *Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
						}
						else
						{
							/* mov  edx,Reg[Am1]               */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6] + REGREGR[Rp[modifier[0]].Offset]);
						}
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 	
						/* add edx,11223344h                   */ if(pcodes[n]!=0){*Np++ = 0x81; *Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}

						if (Rp[modifier[1]].Offset == 0)
						{
							REGSAVEESI
							REGSAVEECX
							/* mov ecx,dword ptr [ebp+Am2]     */ *Np++ = 0x8B; *Np++ = 0x8D; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							// Move the first block
							/* mov esi,dword ptr [edx]         */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 0; Np+=4;
							/* mov dword ptr [ecx], esi        */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,dword ptr [edx+4]       */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 4; Np+=4;
							/* mov dword ptr [ecx+4], esi      */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,dword ptr [edx+8]       */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 8; Np+=4;
							/* mov dword ptr [ecx+8], esi      */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,dword ptr [edx+12]      */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 12; Np+=4;
							/* mov dword ptr [ecx+12], esi     */ *Np++ = 0x89; *Np++ = 0xB1; *Np = 12; Np+=4;
							REGLOADESI
							REGLOADECX
						}
						else
						if (Rp[modifier[1]].Offset == RESI)
						{
							REGSAVEECX
							// Move the first block
							/* mov ecx,dword ptr [edx]         */ *Np++ = 0x8B; *Np++ = 0x8A; *Np = 0; Np+=4;
							/* mov dword ptr [esi], ecx        */ *Np++ = 0x89; *Np++ = 0x8E; *Np = 0; Np+=4;
							// Move the second block
							/* mov ecx,dword ptr [edx+4]       */ *Np++ = 0x8B; *Np++ = 0x8A; *Np = 4; Np+=4;
							/* mov dword ptr [esi+4], ecx      */ *Np++ = 0x89; *Np++ = 0x8E; *Np = 4; Np+=4;
							// Move the third block
							/* mov ecx,dword ptr [edx+8]       */ *Np++ = 0x8B; *Np++ = 0x8A; *Np = 8; Np+=4;
							/* mov dword ptr [esi+8], ecx      */ *Np++ = 0x89; *Np++ = 0x8E; *Np = 8; Np+=4;
							// Move the fourth block
							/* mov ecx,dword ptr [edx+12]      */ *Np++ = 0x8B; *Np++ = 0x8A; *Np = 12; Np+=4;
							/* mov dword ptr [esi+12], ecx     */ *Np++ = 0x89; *Np++ = 0x8E; *Np = 12; Np+=4;
							REGLOADECX
						}
						else
						{
							REGSAVEESI
							// Move the first block
							/* mov esi,dword ptr [edx]         */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 0; Np+=4;
							/* mov dword ptr [RAm1], esi       */ *Np++ = 0x89; *Np++ = (0xB0+REGREGR[Rp[modifier[1]].Offset]); *Np = 0; Np+=4;
							// Move the second block
							/* mov esi,dword ptr [edx+4]       */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 4; Np+=4;
							/* mov dword ptr [RAm1+4], esi     */ *Np++ = 0x89; *Np++ = (0xB0+REGREGR[Rp[modifier[1]].Offset]); *Np = 4; Np+=4;
							// Move the third block
							/* mov esi,dword ptr [edx+8]       */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 8; Np+=4;
							/* mov dword ptr [RAm1+8], esi     */ *Np++ = 0x89; *Np++ = (0xB0+REGREGR[Rp[modifier[1]].Offset]); *Np = 8; Np+=4;
							// Move the fourth block
							/* mov esi,dword ptr [edx+12]      */ *Np++ = 0x8B; *Np++ = 0xB2; *Np = 12; Np+=4;
							/* mov dword ptr [RAm1+12], esi    */ *Np++ = 0x89; *Np++ = (0xB0+REGREGR[Rp[modifier[1]].Offset]); *Np = 12; Np+=4;
							REGLOADESI
						}
						}
					break;

				case vmregLoadAddress:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
        				/*** source = (Rp[Am1]+offset);   ***/
						/* mov  eax,dword ptr Rp[Am1]       */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* add  eax,0x11223344              */ if (pcodes[n]!=0){*Np++ = 0x05;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov  dword ptr Rp[Am2],eax       */ *Np++ = 0x89;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
						{
        				/*** source = (Rp[Am1]+offset);   ***/
						/* mov  edx,dword ptr Rp[Am1]       */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add  edx,0x11223344              */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov dword ptr Rp[Am2],edx        */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
						{
						/* mov  edx,Reg[Am1]				*/ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6] + REGREGR[Rp[modifier[0]].Offset]);
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add  edx,0x11223344              */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov dword ptr Rp[Am2],edx        */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
						{
						/* mov  Reg[Am2],dword ptr Rp[Am1]  */ *Np++ = 0x8B;*Np++ = REGRPMOD[Rp[modifier[1]].Offset];*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add  Reg[Am2],0x11223344         */ if (pcodes[n]!=0){*Np++ = 0x81; *Np++ = (0xC0+REGREGR[Rp[modifier[1]].Offset]); *(LpNUM)Np = pcodes[n];Np+=4;}
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
						{
						/* mov  Reg[Am2],Reg[Am1]			*/ MOVREGREG(Rp[modifier[1]].Offset,Rp[modifier[0]].Offset)
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add  Reg[Am2],0x11223344         */ if (pcodes[n]!=0){*Np++ = 0x81; *Np++ = (0xC0+REGREGR[Rp[modifier[1]].Offset]); *(LpNUM)Np = pcodes[n];Np+=4;}
						}
					break;

				case vmregMoveImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+6); 
						/* mov Rp[Am2],immediate             */ *Np++ = 0xC7; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); *(LpNUM)Np = pcodes[n]; Np+=4;
						}
					else
						{
						if (Rp[modifier[1]].Offset == 0)
							{
							/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+6); 
							/* mov Rp[Am2],immediate             */ *Np++ = 0xC7; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); *(LpNUM)Np = pcodes[n]; Np+=4;
							}
						else
							{
							/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
							/* mov Reg[Am2],immediate			 */ *Np++ = MREGIMM[Rp[modifier[1]].Offset];*(LpNUM)Np = pcodes[n]; Np+=4;
							}
						}
					break;

				case vmregLoadInteger:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov  edx,dword ptr Rp[Am1]               */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment                 ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* mov edx,dword ptr [edx+0x11223344]	    */ *Np++ = 0x8B;*Np++ = 0x92;*(LpNUM)Np = pcodes[n]; Np+=4;
						/* mov dword ptr Rp[Am2],edx                */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					else
						{
						if (Rp[modifier[0]].Offset == 0)
							{
        					/*** source = (Rp[Am1]+offset);           ***/
							/* mov  edx,dword ptr Rp[Am1]               */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
							{
        					/*** source = (Rp[Am1]+offset);           ***/
							/* mov  edx,Reg[Am1]						*/ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6] + REGREGR[Rp[modifier[0]].Offset]);
							}

						if (Rp[modifier[1]].Offset == 0)
							{
							/*** Set Pc/Nc allignment                 ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* mov edx,dword ptr [edx+0x11223344]	    */ *Np++ = 0x8B;*Np++ = 0x92;*(LpNUM)Np = pcodes[n]; Np+=4;
							/* mov dword ptr Rp[Am2],edx                */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
							{
							/*** Set Pc/Nc allignment                 ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* mov Reg[Am2],dword ptr [edx+0x11223344]	*/ *Np++ = 0x8B;*Np++ = REGEDXMOD[Rp[modifier[1]].Offset];*(LpNUM)Np = pcodes[n]; Np+=4;
							}
						}
					break;

				case vmregLoadTail:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
        				/*** source = (Rp[Am1]+offset);   ***/
						/* mov    edx,dword ptr Rp[Am1]     */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* add    edx,0x11223344            */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    edx,dword ptr [edx+10]    */ *Np++ = 0x8B;*Np++ = 0x52;*Np++ = 0x0A;
						/* mov    dword ptr Rp[Am2],edx     */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
						{
        				if (Rp[modifier[0]].Offset == 0)
        					{
							/* mov    edx,dword ptr Rp[Am1] */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
        					{
							/* mov    edx,Reg[Am1]			*/ MOVREGREG(6,Rp[modifier[0]].Offset) 
							}
						
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* add    edx,0x11223344            */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    edx,dword ptr [edx+10]    */ *Np++ = 0x8B;*Np++ = 0x52;*Np++ = 0x0A;

        				if (Rp[modifier[1]].Offset == 0)
        					{
							/* mov    dword ptr Rp[Am2],edx */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
        					{
							/* mov    Reg[Am2],edx			*/ MOVREGREG(Rp[modifier[1]].Offset,6) 
							}
						}
					break;

				case vmregLoadType:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
        				/*** source = (Rp[Am1]+offset);   ***/
						/* mov  edx,dword ptr Rp[Am1]       */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* add  edx,0x11223344              */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov  dl,byte ptr [edx+15]        */ *Np++ = 0x8A;*Np++ = 0x52;*Np++ = 0x0F;
						/* and  edx,0x000000FF              */ *Np++ = 0x81;*Np++ = 0xE2;*Np++ = 0xFF;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;
						/* mov  dword ptr Rp[Am2],edx       */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
						{
        				if (Rp[modifier[0]].Offset == 0)
        					{
							/* mov    edx,dword ptr Rp[Am1] */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
        					{
							/* mov    edx,Reg[Am1]			*/ MOVREGREG(6,Rp[modifier[0]].Offset) 
							}
						
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* add  edx,0x11223344              */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov  dl,byte ptr [edx+15]        */ *Np++ = 0x8A;*Np++ = 0x52;*Np++ = 0x0F;
						/* and  edx,0x000000FF              */ *Np++ = 0x81;*Np++ = 0xE2;*Np++ = 0xFF;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;

        				if (Rp[modifier[1]].Offset == 0)
        					{
							/* mov    dword ptr Rp[Am2],edx */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
        					{
							/* mov    Reg[Am2],edx			*/ MOVREGREG(Rp[modifier[1]].Offset,6) 
							}
						}
					break;

				case vmregLoadDeclType:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
        				/*** source = (Rp[Am1]+offset);   ***/
						/* mov  edx,dword ptr Rp[Am1]       */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* add  edx,0x11223344              */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov  dl,byte ptr [edx+14]        */ *Np++ = 0x8A;*Np++ = 0x52;*Np++ = 0x0E;
						/* and  edx,0x000000FF              */ *Np++ = 0x81;*Np++ = 0xE2;*Np++ = 0xFF;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;
						/* mov  dword ptr Rp[Am2],edx       */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
						{
        				if (Rp[modifier[0]].Offset == 0)
        					{
							/* mov    edx,dword ptr Rp[Am1] */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
        					{
							/* mov    edx,Reg[Am1]			*/ MOVREGREG(6,Rp[modifier[0]].Offset) 
							}
						
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* add  edx,0x11223344              */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov  dl,byte ptr [edx+14]        */ *Np++ = 0x8A;*Np++ = 0x52;*Np++ = 0x0E;
						/* and  edx,0x000000FF              */ *Np++ = 0x81;*Np++ = 0xE2;*Np++ = 0xFF;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;

        				if (Rp[modifier[1]].Offset == 0)
        					{
							/* mov    dword ptr Rp[Am2],edx */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
        					{
							/* mov    Reg[Am2],edx			*/ MOVREGREG(Rp[modifier[1]].Offset,6) 
							}
						}
					break;

				case vmregLoadNumber:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
        				/*** source = (Rp[Am1]+offset);   ***/
						/* mov  eax,dword ptr Rp[Am1]       */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add  eax,0x11223344              */ *Np++ = 0x05;*(LpNUM)Np = pcodes[n]; Np+=4;
						/* mov  edx,dword ptr [eax]         */ *Np++ = 0x8B;*Np++ = 0x10;
						/* mov  ecx,dword ptr [eax+4]       */ *Np++ = 0x8B;*Np++ = 0x48;*Np++ = 0x04;
						/* mov  dword ptr Rp[Am2],edx       */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						/* mov  dword ptr (Rp[Am2])+4,ecx   */ *Np++ = 0x89;*Np++ = 0x8D;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)+4);Np+=4;
						}
					else
						{
						if (Rp[modifier[0]].Offset == 0)
							{
        					/*** source = (Rp[Am1]+offset);     ***/
							/* mov  edx,dword ptr Rp[Am1]         */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
							{
        					/*** source = (Rp[Am1]+offset);     ***/
							/* mov  edx,Reg[Am1]				  */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6] + REGREGR[Rp[modifier[0]].Offset]);
							}

						if (Rp[modifier[1]].Offset == 0)
							{
							/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* fld  qword ptr [edx+0x11223344]   */ *Np++ = 0xDD;*Np++ = 0x82;*(LpNUM)Np = pcodes[n]; Np+=4;
							/* fstp qword ptr Rp[Am2]            */ *Np++ = 0xDD;*Np++ = 0x9D;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if (Rp[modifier[1]].Offset >= 1)
							{
							/*** Set Pc/Nc allignment          ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* fld  qword ptr [edx+0x11223344]   */ *Np++ = 0xDD;*Np++ = 0x82;*(LpNUM)Np = pcodes[n]; Np+=4;
							/* fstp st(i)						 */ *Np++ = 0xDD;*Np++ = (char)(0xD8 + Rp[modifier[1]].Offset);
							}
						}
					break;

				case vmregObjPointer:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
        				/*** source = (Rp[Am1]+offset);   ***/
						/* mov  edx,dword ptr Rp[Am1]       */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add  edx,0x11223344				*/ if(pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov  al,byte ptr [edx+15]        */ *Np++ = 0x8A;*Np++ = 0x42;*Np++ = 0x0F;
						/* asm  cmp al,TYTEXT               */ *Np++ = 0x3C;*Np++ = TYTEXT;
						/* jl   VM___LOADARG                */ *Np++ = 0x7C;*Np++ = 0x0B;
						/* je   VM___LAST                   */ *Np++ = 0x74;*Np++ = 0x0B;
						/* mov  edx,dword ptr [edx]         */ *Np++ = 0x8B;*Np++ = 0x12;
						/* mov  edx,dword ptr [edx+12]      */ *Np++ = 0x8B;*Np++ = 0x52;*Np++ = 0x0C;
						/* mov  edx,dword ptr [edx]         */ *Np++ = 0x8B;*Np++ = 0x12;
						/* jmp  VM___LAST                   */ *Np++ = 0xEB;*Np++ = 0x02;
						/*** VM___LOADARG:                ***/
						/* sub  edx,edx                     */ *Np++ = 0x2B;*Np++ = 0xD2;
						/*** VM___LAST:                   ***/
						/* mov  dword ptr Rp[Am2],edx		*/ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
						{
						REGSAVEEAX
       					if (Rp[modifier[0]].Offset == 0)
        					{
							/* mov    edx,dword ptr Rp[Am1] */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
        					{
							/* mov    edx,Reg[Am1]			*/ MOVREGREG(6,Rp[modifier[0]].Offset) 
							}
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add  edx,0x11223344				*/ if(pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov  al,byte ptr [edx+15]        */ *Np++ = 0x8A;*Np++ = 0x42;*Np++ = 0x0F;
						/* asm  cmp al,TYTEXT               */ *Np++ = 0x3C;*Np++ = TYTEXT;
						/* jl   VM___LOADARG                */ *Np++ = 0x7C;*Np++ = 0x0B;
						/* je   VM___LAST                   */ *Np++ = 0x74;*Np++ = 0x0B;
						/* mov  edx,dword ptr [edx]         */ *Np++ = 0x8B;*Np++ = 0x12;
						/* mov  edx,dword ptr [edx+12]      */ *Np++ = 0x8B;*Np++ = 0x52;*Np++ = 0x0C;
						/* mov  edx,dword ptr [edx]         */ *Np++ = 0x8B;*Np++ = 0x12;
						/* jmp  VM___LAST                   */ *Np++ = 0xEB;*Np++ = 0x02;
						/*** VM___LOADARG:                ***/
						/* sub  edx,edx                     */ *Np++ = 0x2B;*Np++ = 0xD2;
						/*** VM___LAST:                   ***/
						REGLOADEAX
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov  dword ptr Rp[Am2],edx	*/ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
							{
							/* mov  Reg[Am2],edx	        */ MOVREGREG(Rp[modifier[1]].Offset,6)
							}
						}
					break;

				case vmregObjLength:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
        				/*** source = (Rp[Am1]+offset);      ***/
						/* mov    edx,dword ptr Rp[Am1]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344			   */ if(pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    al,byte ptr [edx+15]         */ *Np++ = 0x8A;*Np++ = 0x42;*Np++ = 0x0F;
						/* cmp    al,TYTEXT                    */ *Np++ = 0x3C;*Np++ = TYTEXT;
						/* jl     VM___ZERO                    */ *Np++ = 0x7C;*Np++ = 0x15;
						/* jg     VM___OBJ                     */ *Np++ = 0x7F;*Np++ = 0x0C;
						/*** VM___TEXT:                      ***/
						/* mov    ecx,edx                      */ *Np++ = 0x8B;*Np++ = 0xCA;
						/* dec    edx                          */ *Np++ = 0x4A;
						/*** VM___TEXTLOOP:                  ***/
						/* inc    edx                          */ *Np++ = 0x42;
						/* mov    al,byte ptr [edx]            */ *Np++ = 0x8A;*Np++ = 0x02;
						/* jnz    VM___TEXTLOOP                */ *Np++ = 0x75;*Np++ = 0xFB;
						/* sub    edx,ecx                      */ *Np++ = 0x2B;*Np++ = 0xD1;
						/* jmp    VM___LAST                    */ *Np++ = 0xEB;*Np++ = 0x09;
						/*** VM___OBJ:                       ***/
						/* mov    edx,dword ptr [edx]          */ *Np++ = 0x8B;*Np++ = 0x12;
						/* mov    edx,dword ptr [edx+8]        */ *Np++ = 0x8B;*Np++ = 0x52;*Np++ = 0x08;
						/* jmp    VM___LAST                    */ *Np++ = 0xEB;*Np++ = 0x02;
						/*** VM___ZERO:                      ***/
						/* sub    edx,edx                      */ *Np++ = 0x2B;*Np++ = 0xD2;
						/*** VM___LAST:                      ***/
						/* mov    dword ptr Rp[target],edx     */ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
						}
					else
						{
						REGSAVEEAX
						REGSAVEECX
       					if (Rp[modifier[0]].Offset == 0)
        					{
        					/*** source = (Rp[Am1]+offset);***/
							/* mov    edx,dword ptr Rp[Am1]  */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
        					{
							/* mov    edx,Reg[Am1]			*/ MOVREGREG(6,Rp[modifier[0]].Offset) 
							}
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344			   */ if(pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    al,byte ptr [edx+15]         */ *Np++ = 0x8A;*Np++ = 0x42;*Np++ = 0x0F;
						/* cmp    al,TYTEXT                    */ *Np++ = 0x3C;*Np++ = TYTEXT;
						/* jl     VM___ZERO                    */ *Np++ = 0x7C;*Np++ = 0x15;
						/* jg     VM___OBJ                     */ *Np++ = 0x7F;*Np++ = 0x0C;
						/*** VM___TEXT:                      ***/
						/* mov    ecx,edx                      */ *Np++ = 0x8B;*Np++ = 0xCA;
						/* dec    edx                          */ *Np++ = 0x4A;
						/*** VM___TEXTLOOP:                  ***/
						/* inc    edx                          */ *Np++ = 0x42;
						/* mov    al,byte ptr [edx]            */ *Np++ = 0x8A;*Np++ = 0x02;
						/* jnz    VM___TEXTLOOP                */ *Np++ = 0x75;*Np++ = 0xFB;
						/* sub    edx,ecx                      */ *Np++ = 0x2B;*Np++ = 0xD1;
						/* jmp    VM___LAST                    */ *Np++ = 0xEB;*Np++ = 0x09;
						/*** VM___OBJ:                       ***/
						/* mov    edx,dword ptr [edx]          */ *Np++ = 0x8B;*Np++ = 0x12;
						/* mov    edx,dword ptr [edx+8]        */ *Np++ = 0x8B;*Np++ = 0x52;*Np++ = 0x08;
						/* jmp    VM___LAST                    */ *Np++ = 0xEB;*Np++ = 0x02;
						/*** VM___ZERO:                      ***/
						/* sub    edx,edx                      */ *Np++ = 0x2B;*Np++ = 0xD2;
						/*** VM___LAST:                      ***/
						REGLOADEAX
						REGLOADECX
						if (Rp[modifier[1]].Offset == 0)
							{
							/* mov  dword ptr Rp[Am2],edx	*/ *Np++ = 0x89;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
							}
						else
							{
							/* mov  Reg[Am2],edx	        */ MOVREGREG(Rp[modifier[1]].Offset,6)
							}
						}
					break;

				/* Register opcodes with one memory and one register argument. */
					outStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */
					switch(modifier[0])
						{
						case AMGVOFFSET:
						case AMSVOFFSET:
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
							Jp[++n] = (NUM)(Np-ncodes);   /*  Update the jump label translation vector. */
							*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM regoffset argument to the native code vector. */
							break;

						default:
							goto JIT_IllegalInstruction;
							break;
						}
					break; 

				case vmregRunInHardware:
					regOutStream(JITOpImm,FALSE)	// FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment                     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					Jp[++n] = (NUM)(Np-ncodes);     /*  Update the jump label translation vector.  */
					k = pcodes[n];					/*  Recover the VM immediate command argument. */

					if (jitHardwareMode == FALSE) goto VMRUNINHARDWARECONTINUE;
					
					if (autoHardwareMode == TRUE) goto ErrorRunInHardwareIllegalInstruction;
					
					switch (k)						/*  Switch on the command argument. */
						{
						case 0:									
							/*** start ***/
							if (Vr[HARDWARESW] != FALSE) goto VMRUNINHARDWARECONTINUE;

							StartRunInHardware:

							/* Set the hardware mode switch to on */
							Vr[HARDWARESW] = TRUE;
							/* Allocate the following five Integer registers: eax ecx ebx esi edi */
							/* Allocate the following five Number registers: st0 st1 st2 st3 st4 */
							FVmIntelP3Jit_RegisterCnt(gCP,gTP,TOBJ(proc->PcodeVector),n+1,Rp,Ir,IREGMAX,Nr,NREGMAX);
							/* Load Integer hardware registers from allocated register variables. */
							i = 0;
							for (k = 1; k <= IREGMAX; ++k)
								{
								if (Ir[k] != 0)
									{
									/* mov reg,dword ptr Rp[Ir[k]]	*/ *Np++ = 0x8B;*Np++ = REGRPMOD[k];*(LpNUM)Np = (RpRelAddress+(Ir[k]<<BITSIZEOFAISWORD));Np+=4;
									++i;
									}
								}
							/* Load Number hardware registers from allocated register variables. */
							for (k = NREGMAX; k >= 1; --k)
								{
								if (Nr[k] != 0)
									{
									/* fld qword ptr Rp[Nr[k]]		*/ *Np++ = 0xDD;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(Nr[k]<<BITSIZEOFAISWORD));Np+=4;
									++i;
									}
								}
							if (i == 0) Vr[HARDWARESW] = FALSE;
							break;

						case 2:									
							/*** startNoLoad ***/
							if (Vr[HARDWARESW] != FALSE) goto VMRUNINHARDWARECONTINUE;
							/* Set the hardware mode switch to on */
							Vr[HARDWARESW] = TRUE;
							/* Allocate the following five Integer registers: eax ecx ebx esi edi */
							/* Allocate the following five Number registers: st0 st1 st2 st3 st4 */
							FVmIntelP3Jit_RegisterCnt(gCP,gTP,TOBJ(proc->PcodeVector),n+1,Rp,Ir,IREGMAX,Nr,NREGMAX);
							/* Count Integer hardware registers from allocated register variables. */
							i = 0;
							for (k = 1; k <= IREGMAX; ++k)
								{
								if (Ir[k] != 0)
									{
									++i;
									}
								}
							/* Count Number hardware registers from allocated register variables. */
							for (k = NREGMAX; k >= 1; --k)
								{
								if (Nr[k] != 0)
									{
									++i;
									}
								}
							if (i == 0) Vr[HARDWARESW] = FALSE;
							break;

						case 1:	
							/*** stop ***/
							SAVEALLREGS
								
							/* Set the hardware mode switch to off */
							Vr[HARDWARESW] = FALSE;
							break;

						case 3:									
							/*** stopNoSave ***/
							/* Set the hardware mode switch to off */
							Vr[HARDWARESW] = FALSE;
							break;

						default:
							goto ErrorIllegalInstruction;
							break;
						}
					VMRUNINHARDWARECONTINUE:
					break;

				case vmregSaveInteger:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment            ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov    eax,dword ptr Rp[Am1]        */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,dword ptr Rp[Am2]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx],eax          */ *Np++ = 0x89;*Np++ = 0x02;
						/* mov    byte ptr [edx+15],TYNUM      */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
						{
						REGSAVEEAX
						/* mov    eax,dword ptr Rp[Am1]        */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,dword ptr Rp[Am2]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx],eax          */ *Np++ = 0x89;*Np++ = 0x02;
						/* mov    byte ptr [edx+15],TYNUM      */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
						REGLOADEAX
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
						{
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,dword ptr Rp[Am2]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx],Reg[Am1]     */ *Np++ = 0x89;*Np++ = (char)(REGBASE[6]+REGTARG[Rp[modifier[0]].Offset]);
						/* mov    byte ptr [edx+15],TYNUM      */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
						{
						REGSAVEEAX
						/* mov    eax,dword ptr Rp[Am1]        */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,Reg[Am2]                 */ MOVREGREG(6,Rp[modifier[1]].Offset) 
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx],eax          */ *Np++ = 0x89;*Np++ = 0x02;
						/* mov    byte ptr [edx+15],TYNUM      */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
						REGLOADEAX
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
						{
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,Reg[Am2]                 */ MOVREGREG(6,Rp[modifier[1]].Offset) 
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx],Reg[Am1]     */ *Np++ = 0x89;*Np++ = (char)(REGBASE[6]+REGTARG[Rp[modifier[0]].Offset]);
						/* mov    byte ptr [edx+15],TYNUM      */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYNUM;
						}
					break;


				case vmregSaveUInteger:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment            ***/ Jp[n]   = (NUM)(Np-ncodes);        
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov    eax,dword ptr Rp[Am1]        */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,dword ptr Rp[Am2]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx],eax          */ *Np++ = 0x89;*Np++ = 0x02;
						/* mov    byte ptr [edx+15],TYUNUM     */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYUNUM;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset == 0))
						{
						REGSAVEEAX
						/* mov    eax,dword ptr Rp[Am1]        */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,dword ptr Rp[Am2]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx],eax          */ *Np++ = 0x89;*Np++ = 0x02;
						/* mov    byte ptr [edx+15],TYUNUM     */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYUNUM;
						REGLOADEAX
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset == 0))
						{
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,dword ptr Rp[Am2]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx],Reg[Am1]     */ *Np++ = 0x89;*Np++ = (char)(REGBASE[6]+REGTARG[Rp[modifier[0]].Offset]);
						/* mov    byte ptr [edx+15],TYUNUM     */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYUNUM;
						}
					else
					if ((Rp[modifier[0]].Offset == 0) && (Rp[modifier[1]].Offset >= 1))
						{
						REGSAVEEAX
						/* mov    eax,dword ptr Rp[Am1]        */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,Reg[Am2]                 */ MOVREGREG(6,Rp[modifier[1]].Offset) 
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx],eax          */ *Np++ = 0x89;*Np++ = 0x02;
						/* mov    byte ptr [edx+15],TYUNUM     */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYUNUM;
						REGLOADEAX
						}
					else
					if ((Rp[modifier[0]].Offset >= 1) && (Rp[modifier[1]].Offset >= 1))
						{
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,Reg[Am2]                 */ MOVREGREG(6,Rp[modifier[1]].Offset) 
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx],Reg[Am1]     */ *Np++ = 0x89;*Np++ = (char)(REGBASE[6]+REGTARG[Rp[modifier[0]].Offset]);
						/* mov    byte ptr [edx+15],TYUNUM     */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYUNUM;
						}
					break;

				case vmregSaveTail:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov    eax,dword ptr Rp[Am1]        */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,dword ptr Rp[Am2]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx+10],eax	   */ *Np++ = 0x89;*Np++ = 0x42;*Np++ = 0x0A;
						}
					else
						{
						if (Rp[modifier[0]].Offset > 1) {REGSAVEEAX}
        				if (Rp[modifier[0]].Offset == 0)
        					{
							/* mov    eax,dword ptr Rp[Am1] */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if (Rp[modifier[0]].Offset > 1)
        					{
							/* mov    eax,Reg[Am1]			*/ MOVREGREG(1,Rp[modifier[0]].Offset) 
							}
							
        				if (Rp[modifier[1]].Offset == 0)
        					{
							/* mov    edx,dword ptr Rp[Am2] */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
        					{
							/* mov    edx,Reg[Am2]			*/ MOVREGREG(6,Rp[modifier[1]].Offset) 
							}
						
						/*** Set Pc/Nc allignment         ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344            */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx+10],eax	*/ *Np++ = 0x89;*Np++ = 0x42;*Np++ = 0x0A;
						if (Rp[modifier[0]].Offset > 1) {REGLOADEAX}
						}
					break;

				case vmregSaveTailImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** Set Pc/Nc allignment			 ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* mov    eax,0x11223344			   */ *Np++ = 0xB8;*(LpNUM)Np = pcodes[n];Np+=4;
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,dword ptr Rp[Am2]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx+10],eax	   */ *Np++ = 0x89;*Np++ = 0x42;*Np++ = 0x0A;
						}
					else
						{
						REGSAVEEAX
						/*** Set Pc/Nc allignment			***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* mov    eax,0x11223344			  */ *Np++ = 0xB8;*(LpNUM)Np = pcodes[n];Np+=4;
							
        				if (Rp[modifier[1]].Offset == 0)
        					{
							/* mov    edx,dword ptr Rp[Am2]   */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
        					{
							/* mov    edx,Reg[Am2]			  */ MOVREGREG(6,Rp[modifier[1]].Offset) 
							}
						
						/*** Set Pc/Nc allignment           ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344              */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    dword ptr [edx+10],eax	  */ *Np++ = 0x89;*Np++ = 0x42;*Np++ = 0x0A;
						REGLOADEAX
						}
					break;

				case vmregSaveDeclTypeImmediate:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/*** Set Pc/Nc allignment			 ***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* mov    eax,0x11223344			   */ *Np++ = 0xB8;*(LpNUM)Np = pcodes[n];Np+=4;
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,dword ptr Rp[Am2]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    byte ptr [edx+14],al         */ *Np++ = 0x88;*Np++ = 0x42;*Np++ = 0x0E;
						}
					else
						{
						REGSAVEEAX
						/*** Set Pc/Nc allignment			***/ Jp[++n] = (NUM)((Np-ncodes)+1); 
						/* mov    eax,0x11223344			  */ *Np++ = 0xB8;*(LpNUM)Np = pcodes[n];Np+=4;
							
        				if (Rp[modifier[1]].Offset == 0)
        					{
							/* mov    edx,dword ptr Rp[Am2]   */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
        					{
							/* mov    edx,Reg[Am2]			  */ MOVREGREG(6,Rp[modifier[1]].Offset) 
							}
						
						/*** Set Pc/Nc allignment           ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344              */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    byte ptr [edx+14],al         */ *Np++ = 0x88;*Np++ = 0x42;*Np++ = 0x0E;
						REGLOADEAX
						}
					break;

				case vmregSaveDeclType:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
						/* mov    eax,dword ptr Rp[Am1]        */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,dword ptr Rp[Am2]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    byte ptr [edx+14],al         */ *Np++ = 0x88;*Np++ = 0x42;*Np++ = 0x0E;
						}
					else
						{
						if (Rp[modifier[0]].Offset > 1) {REGSAVEEAX}
        				if (Rp[modifier[0]].Offset == 0)
        					{
							/* mov    eax,dword ptr Rp[Am1]    */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if (Rp[modifier[0]].Offset > 1)
        					{
							/* mov    eax,Reg[Am1]			   */ MOVREGREG(1,Rp[modifier[0]].Offset) 
							}
							
        				if (Rp[modifier[1]].Offset == 0)
        					{
							/* mov    edx,dword ptr Rp[Am2]    */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
        					{
							/* mov    edx,Reg[Am2]			   */ MOVREGREG(6,Rp[modifier[1]].Offset) 
							}
						
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;}
						/* mov    byte ptr [edx+14],al         */ *Np++ = 0x88;*Np++ = 0x42;*Np++ = 0x0E;
						if (Rp[modifier[0]].Offset > 1) {REGLOADEAX}
						}
					break;

				case vmregSaveNumber:
					regOutStream(JITOpIa,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);
					if (Vr[HARDWARESW] == FALSE)
						{
        				/*** target = (Rp[Am2]+offset));     ***/
						/* mov    edx,dword ptr Rp[Am2]        */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
						/* add    edx,0x11223344               */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
						/* mov    eax,dword ptr Rp[Am1]        */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
						/* mov    dword ptr [edx],eax          */ *Np++ = 0x89;*Np++ = 0x02;
						/* mov    eax,dword ptr (Rp[Am1])+4    */ *Np++ = 0x8B;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)+4);Np+=4;
						/* mov    dword ptr [edx+4],eax	       */ *Np++ = 0x89;*Np++ = 0x42;*Np++ = 0x04;
						/* mov    byte ptr [edx+15],TYREAL     */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYREAL;
						}
					else
						{
						if (Rp[modifier[1]].Offset == 0)
							{
        					/*** target = (Rp[Am2]+offset);     ***/
							/* mov  edx,dword ptr Rp[Am2]         */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							}
						else
						if (Rp[modifier[1]].Offset != 6)
							{
        					/*** target = (Rp[Am2]+offset);     ***/
							/* mov  edx,Reg[Am2]				  */ *Np++ = 0x8B; *Np++ = (char)(REGREGL[6] + REGREGR[Rp[modifier[1]].Offset]);
							}

						if (Rp[modifier[0]].Offset == 0)
							{
							/* fld qword ptr Rp[Am1]              */ *Np++ = 0xDD;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
							/*** Set Pc/Nc allignment           ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* fstp qword ptr [edx+0x11223344]    */ *Np++ = 0xDD;*Np++ = 0x9A;*(LpNUM)Np = pcodes[n]; Np+=4;
							}
						else
						if (Rp[modifier[0]].Offset == 1)
							{
							/*** Set Pc/Nc allignment           ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* fst  qword ptr [edx+0x11223344]    */ *Np++ = 0xDD;*Np++ = 0x92;*(LpNUM)Np = pcodes[n]; Np+=4;
							}
						else
							{
							/* fld st(i)						  */ *Np++ = 0xD9;*Np++ = (char)(0xBF + Rp[modifier[0]].Offset);
							/*** Set Pc/Nc allignment           ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
							/* fstp qword ptr [edx+0x11223344]    */ *Np++ = 0xDD;*Np++ = 0x9A;*(LpNUM)Np = pcodes[n]; Np+=4;
							}
						/* add    edx,0x11223344                  */ *Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n]; Np+=4;
						/* mov    byte ptr [edx+15],TYREAL		  */ *Np++ = 0xC6;*Np++ = 0x42;*Np++ = 0x0F;*Np++ = TYREAL;
						}
					break;

				/* Register opcodes with one register and one memory argument. */
					/* JITOpIa_2:				  */ // FALSE if compiling this instruction instream.
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */
					switch(modifier[1])
						{
						case AMGVOFFSET:
						case AMSVOFFSET:
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
							Jp[++n] = (NUM)(Np-ncodes);   /*  Update the jump label translation vector. */
							*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM regoffset argument to the native code vector. */
							break;

						default:
							goto JIT_IllegalInstruction;
							break;
						}
					break;

				case vmregStringCompare:
					regOutStream(JITOpMmMm,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment            ***/ Jp[n]   = (NUM)(Np-ncodes);        

					SAVEINTREGS

					if ((Vr[HARDWARESW] == TRUE) && (Rp[modifier[0]].Offset >= 1))
						{
						/* mov    esi,Reg[Am1]             */ MOVREGREG(4,Rp[modifier[0]].Offset); 
						}
					else
						{
						/* mov    esi,dword ptr Rp[Am1]    */ *Np++ = 0x8B;*Np++ = 0xB5;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						}
					/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* add    esi,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC6;*(LpNUM)Np = pcodes[n];Np+=4;}
					if ((Vr[HARDWARESW] == TRUE) && (Rp[modifier[1]].Offset >= 1))
						{
						/* mov    edx,Reg[Am2]             */ MOVREGREG(6,Rp[modifier[1]].Offset); 
						}
					else
						{
						/* mov    edx,dword ptr Rp[Am2]    */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;} 

					/*** srcP = (source->Tag == TYTEXT) ? source->u.Text : CharArray(*source); ***/
					/* mov    al,byte ptr [esi+15]         */ *Np++ = 0x8A;*Np++ = 0x46;*Np++ = 0x0F;
					/* cmp al,TYTEXT                       */ *Np++ = 0x3C;*Np++ = TYTEXT;
					/* je     VM___LOADARG                 */ *Np++ = 0x74;*Np++ = 0x07;
					/* mov    esi,dword ptr [esi]          */ *Np++ = 0x8B;*Np++ = 0x36;
					/* mov    esi,dword ptr [esi+12]       */ *Np++ = 0x8B;*Np++ = 0x76;*Np++ = 0x0C;
					/* mov    esi,dword ptr [esi]          */ *Np++ = 0x8B;*Np++ = 0x36;

					/*** VM___LOADARG:                   ***/
					/* argP = (argument->Tag == TYTEXT) ? argument->u.Text : CharArray(*argument); */
					/* mov    al,byte ptr [edx+15]         */ *Np++ = 0x8A;*Np++ = 0x42;*Np++ = 0x0F;
					/* cmp al,TYTEXT                       */ *Np++ = 0x3C;*Np++ = TYTEXT;
					/* je     VM___RETRY                   */ *Np++ = 0x74;*Np++ = 0x07;
					/* mov    edx,dword ptr [edx]          */ *Np++ = 0x8B;*Np++ = 0x12;
					/* mov    edx,dword ptr [edx+12]       */ *Np++ = 0x8B;*Np++ = 0x52;*Np++ = 0x0C;
					/* mov    edx,dword ptr [edx]          */ *Np++ = 0x8B;*Np++ = 0x12;

					/*** VM___RETRY:                     ***/
					/* movzx  ax,byte ptr [esi]            */ *Np++ = 0x66;*Np++ = 0x0F;*Np++ = 0xB6;*Np++ = 0x06;
					/* movzx  cx,byte ptr [edx]            */ *Np++ = 0x66;*Np++ = 0x0F;*Np++ = 0xB6;*Np++ = 0x0A;
					/* sub    ax,cx                        */ *Np++ = 0x66;*Np++ = 0x2B;*Np++ = 0xC1;

					/* jnz    VM___CONTINUE                */ *Np++ = 0x75;*Np++ = 0x0F;
					/* inc    esi                          */ *Np++ = 0x46;
					/* inc    edx                          */ *Np++ = 0x42;
					/* cmp    cx,0                         */ *Np++ = 0x66;*Np++ = 0x83;*Np++ = 0xF9;*Np++ = 0x00;
					/* jnz    VM___RETRY                   */ *Np++ = 0x75;*Np++ = 0xEB;
					/* mov    eax,0                        */ *Np++ = 0xB8;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;
					/* jmp    VM___LAST                    */ *Np++ = 0xEB;*Np++ = 0x0C;

					/*** VM___CONTINUE:                  ***/
					/* jg     VM___HIGH                    */ *Np++ = 0x7F;*Np++ = 0x05;
					/* sub    eax,eax                      */ *Np++ = 0x2B;*Np++ = 0xC0;
					/* dec    eax                          */ *Np++ = 0x48;
					/* jmp    VM___LAST                    */ *Np++ = 0xEB;*Np++ = 0x05;

					/*** VM___HIGH:                      ***/
					/* mov    eax,1                        */ *Np++ = 0xB8;*Np++ = 0x01;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;

					/*** VM___LAST:                      ***/
					/* mov    dword ptr Rp[target],eax     */ *Np++ = 0x89;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;

					LOADINTREGS
				    break;

				case vmregStringiCompare:
					regOutStream(JITOpMmMm,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment            ***/ Jp[n]   = (NUM)(Np-ncodes);        

					SAVEINTREGS

					if ((Vr[HARDWARESW] == TRUE) && (Rp[modifier[0]].Offset >= 1))
						{
						/* mov    esi,Reg[Am1]             */ MOVREGREG(4,Rp[modifier[0]].Offset); 
						}
					else
						{
						/* mov    esi,dword ptr Rp[Am1]    */ *Np++ = 0x8B;*Np++ = 0xB5;*(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD)); Np+=4;
						}
					/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* add    esi,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC6;*(LpNUM)Np = pcodes[n];Np+=4;}
					if ((Vr[HARDWARESW] == TRUE) && (Rp[modifier[1]].Offset >= 1))
						{
						/* mov    edx,Reg[Am2]             */ MOVREGREG(6,Rp[modifier[1]].Offset); 
						}
					else
						{
						/* mov    edx,dword ptr Rp[Am2]    */ *Np++ = 0x8B;*Np++ = 0x95;*(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
						}
					/*** Set Pc/Nc allignment            ***/ Jp[++n] = (NUM)((Np-ncodes)+2); 
					/* add    edx,0x11223344               */ if (pcodes[n]!=0){*Np++ = 0x81;*Np++ = 0xC2;*(LpNUM)Np = pcodes[n];Np+=4;} 

					/*** srcP = (source->Tag == TYTEXT) ? source->u.Text : CharArray(*source); ***/
					/* mov    al,byte ptr [esi+15]         */ *Np++ = 0x8A;*Np++ = 0x46;*Np++ = 0x0F;
					/* cmp al,TYTEXT                       */ *Np++ = 0x3C;*Np++ = TYTEXT;
					/* je     VM___LOADARG                 */ *Np++ = 0x74;*Np++ = 0x07;
					/* mov    esi,dword ptr [esi]          */ *Np++ = 0x8B;*Np++ = 0x36;
					/* mov    esi,dword ptr [esi+12]       */ *Np++ = 0x8B;*Np++ = 0x76;*Np++ = 0x0C;
					/* mov    esi,dword ptr [esi]          */ *Np++ = 0x8B;*Np++ = 0x36;

					/*** VM___LOADARG:                   ***/
					/*** argP = (argument->Tag == TYTEXT) ? argument->u.Text : CharArray(*argument); ***/
					/* mov    al,byte ptr [edx+15]         */ *Np++ = 0x8A;*Np++ = 0x42;*Np++ = 0x0F;
					/* cmp al,TYTEXT                       */ *Np++ = 0x3C;*Np++ = TYTEXT;
					/* je     VM___RETRY                   */ *Np++ = 0x74;*Np++ = 0x07;
					/* mov    edx,dword ptr [edx]          */ *Np++ = 0x8B;*Np++ = 0x12;
					/* mov    edx,dword ptr [edx+12]       */ *Np++ = 0x8B;*Np++ = 0x52;*Np++ = 0x0C;
					/* mov    edx,dword ptr [edx]          */ *Np++ = 0x8B;*Np++ = 0x12;

					/*** VM___RETRY:                     ***/
					/* movzx  ax,byte ptr [esi]            */ *Np++ = 0x66;*Np++ = 0x0F;*Np++ = 0xB6;*Np++ = 0x06;
					/* movzx  cx,byte ptr [edx]            */ *Np++ = 0x66;*Np++ = 0x0F;*Np++ = 0xB6;*Np++ = 0x0A;

					/* cmp    ax,65                        */ *Np++ = 0x66;*Np++ = 0x3D;*Np++ = 0x41;*Np++ = 0x00;
					/* jl     VM___CHKARG                  */ *Np++ = 0x7C;*Np++ = 0x09;
					/* cmp    ax,90                        */ *Np++ = 0x66;*Np++ = 0x3D;*Np++ = 0x5A;*Np++ = 0x00;
					/* jg     VM___CHKARG                  */ *Np++ = 0x7F;*Np++ = 0x03;
					/* add    eax,32                       */ *Np++ = 0x83;*Np++ = 0xC0;*Np++ = 0x20;

					/*** VM___CHKARG:                    ***/
					/* cmp    cx,65                        */ *Np++ = 0x66;*Np++ = 0x83;*Np++ = 0xF9;*Np++ = 0x41;
					/* jl     VM___COMPARE                 */ *Np++ = 0x7C;*Np++ = 0x09;
					/* cmp    cx,90                        */ *Np++ = 0x66;*Np++ = 0x83;*Np++ = 0xF9;*Np++ = 0x5A;
					/* jg     VM___COMPARE                 */ *Np++ = 0x7F;*Np++ = 0x03;
					/* add    ecx,32                       */ *Np++ = 0x83;*Np++ = 0xC1;*Np++ = 0x20;

        			/*** VM___COMPARE:                   ***/
					/* sub    ax,cx                        */ *Np++ = 0x66;*Np++ = 0x2B;*Np++ = 0xC1;
					/* jnz    VM___CONTINUE                */ *Np++ = 0x75;*Np++ = 0x0F;
					/* inc    esi                          */ *Np++ = 0x46;
					/* inc    edx                          */ *Np++ = 0x42;
					/* cmp    cx,0                         */ *Np++ = 0x66;*Np++ = 0x83;*Np++ = 0xF9;*Np++ = 0x00;
					/* jnz    VM___RETRY                   */ *Np++ = 0x75;*Np++ = 0xCD;
					/* mov    eax,0                        */ *Np++ = 0xB8;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;
					/* jmp    VM___LAST                    */ *Np++ = 0xEB;*Np++ = 0x0C;

					/*** VM___CONTINUE:                  ***/
					/* jg     VM___HIGH                    */ *Np++ = 0x7F;*Np++ = 0x05;
					/* sub    eax,eax                      */ *Np++ = 0x2B;*Np++ = 0xC0;
					/* dec    eax                          */ *Np++ = 0x48;
					/* jmp    VM___LAST                    */ *Np++ = 0xEB;*Np++ = 0x05;

					/*** VM___HIGH:                      ***/
					/* mov    eax,1                        */ *Np++ = 0xB8;*Np++ = 0x01;*Np++ = 0x00;*Np++ = 0x00;*Np++ = 0x00;

					/*** VM___LAST:                      ***/
					/* mov    dword ptr Rp[target],eax     */ *Np++ = 0x89;*Np++ = 0x85;*(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;

					LOADINTREGS
				    break;

				/* Register opcodes with two memory and one register argument. */
				    JITOpMmMm:
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */
					switch(modifier[0])
						{
						case AMGVOFFSET:
						case AMSVOFFSET:
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
							Jp[++n] = (NUM)(Np-ncodes);   /*  Update the jump label translation vector. */
							*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM regoffset argument to the native code vector. */
							break;

						default:
							goto JIT_IllegalInstruction;
							break;
						}
					switch(modifier[1])
						{
						case AMGVOFFSET:
						case AMSVOFFSET:
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
							Jp[++n] = (NUM)(Np-ncodes);   /*  Update the jump label translation vector. */
							*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM regoffset argument to the native code vector. */
							break;

						default:
							goto JIT_IllegalInstruction;
							break;
						}
					break; 

				case vmvecBinary:
					outStream(JITOpImm,FALSE)	// FALSE if compiling this instruction instream.
					Jp[n] = (NUM)(Np-ncodes);		/*  Update the jump label translation vector. */
					Jp[++n] = (NUM)(Np-ncodes);     /*  Update the jump label translation vector. */
					k = pcodes[n];					/*  Recover the original VM immediate operator argument. */
					switch (k)						/*  Switch on the operator argument. */
						{
						case 0:									
							/*** add ***/
							/* faddp  st(1),st(0)			*/ *Np++ = 0xDE; *Np++ = 0xC1;
							break;

						case 1:									
							/*** div ***/
							/* fdivp  st(1),st(0)			*/ *Np++ = 0xDE; *Np++ = 0xF9;
							break;

						case 3:									
							/*** mul ***/
							/* fmulp  st(1),st(0)			*/ *Np++ = 0xDE; *Np++ = 0xC9;
							break;

						case 4:									
							/*** sub ***/
							/* fsubp  st(1),st(0)			*/ *Np++ = 0xDE; *Np++ = 0xE9;
							break;

						default:
							goto ErrorIllegalInstruction;
							break;
						}
					break;

				case vmvecInitialize:
					/*** Current Vector register assignments:	***/
					/*** counter  =  edx register				***/										
					/*** argPtr   =  ebx register				***/										
					/*** argInc   =  ecx register				***/										
					/*** srcPtr   =  esi register				***/										
					/*** srcInc   =  Vr[SRCINCID]				***/										
					/*** tarPtr   =  edi register				***/										
					/*** tarInc   =  Vr[TARINCID]				***/										
					outStream(JITOpImm,FALSE)		// FALSE if compiling this instruction instream.
					Jp[n] = (NUM)(Np-ncodes);		/*  Update the jump label translation vector. */
					Jp[++n] = (NUM)(Np-ncodes);     /*  Update the jump label translation vector. */													
					switch ((initEXTENT = pcodes[n]))
						{
						case 0: 
							/*** argument				  ***/
							/* mov   edx,dword ptr Rp[1]	*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(pcode.u.Am2<<BITSIZEOFAISWORD));Np+=4;
							/* mov   eax,Vr[ARGINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ecx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x8C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[ARGPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ebx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x9C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress;Np+=4;
							/*** Save Return Label  	  ***/ initLABEL = (NUM)Np; 					
							/*** LABEL:					  ***/
							break;

						case 1: 
						    /*** source                   ***/
							/* mov   edx,dword ptr Rp[1]	*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(pcode.u.Am2<<BITSIZEOFAISWORD)); Np+=4;
							/* mov   eax,Vr[ARGINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ecx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x8C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[ARGPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ebx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x9C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress;Np+=4;
							/* mov   eax,Vr[SRCINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   eax,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x84; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   Vr[SRCINC],eax			*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[SRCPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   esi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xB4; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress;Np+=4;
							/*** Save Return Label  	  ***/ initLABEL = (NUM)Np; 					
							/*** LABEL:					  ***/
							break;

						case 2: 
						    /*** target                   ***/
							/* mov   edx,dword ptr Rp[1]	*/ *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(pcode.u.Am2<<BITSIZEOFAISWORD));Np+=4;
							/* mov   eax,Vr[ARGINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ecx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x8C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[ARGPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ebx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x9C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[SRCINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   eax,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x84; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   Vr[SRCINC],eax			*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[SRCPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   esi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xB4; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress;Np+=4;
							/* mov   eax,Vr[TARINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   eax,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x84; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   Vr[SRCINC],eax			*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[TARPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xBC; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress;Np+=4;
							/*** Save Return Label  	  ***/ initLABEL = (NUM)Np; 					
							/*** LABEL:					  ***/
							break;

						default:
							goto ErrorIllegalInstruction;
							break;
						}
					break;

				case vmvecLoop:
					outStream(JITOpNoArgs,FALSE)  // FALSE if compiling this instruction instream.
					Jp[n] = (NUM)(Np-ncodes);		/*  Update the jump label translation vector. */
					switch (initEXTENT)
						{ 
						case 0: /* argument */
							/* add   ebx,ecx				*/ *Np++ = 0x03; *Np++ = 0xD9; 
							/* dec   edx					*/ *Np++ = 0x4A; 
							/* jz    LABEL               	*/ *Np++ = 0x74; *Np++ = 0x05; 					
							/* jmp   initLABEL				*/ *Np++ = 0xE9; *(LpNUM)Np = (initLABEL - (((NUM)Np)+4)); Np+=4;
							/*** LABEL:					  ***/
							break;

						case 1: /* source */
							/* add   ebx,ecx				*/ *Np++ = 0x03; *Np++ = 0xD9; 
							/* add   esi,Vr[ARGINC]			*/ *Np++ = 0x03; *Np++ = 0xB5; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM));			Np+=4;
							/* dec   edx					*/ *Np++ = 0x4A; 
							/* jz    LABEL               	*/ *Np++ = 0x74; *Np++ = 0x05; 					
							/* jmp   initLABEL				*/ *Np++ = 0xE9; *(LpNUM)Np = (initLABEL - (((NUM)Np)+4)); Np+=4;
							/*** LABEL:					  ***/
							break;

						case 2: /* target */
							/* add   ebx,ecx				*/ *Np++ = 0x03; *Np++ = 0xD9; 
							/* add   esi,Vr[ARGINC]			*/ *Np++ = 0x03; *Np++ = 0xB5; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM));Np+=4;
							/* add   edi,Vr[TARINC]			*/ *Np++ = 0x03; *Np++ = 0xBD; *(LpNUM)Np = (VrRelAddress+(TARINC<<BITSIZEOFNUM));Np+=4;
							/* dec   edx					*/ *Np++ = 0x4A; 
							/* jz    LABEL               	*/ *Np++ = 0x74; *Np++ = 0x05; 					
							/* jmp   initLABEL				*/ *Np++ = 0xE9; *(LpNUM)Np = (initLABEL - (((NUM)Np)+4)); Np+=4;
							/*** LABEL:					  ***/
							break;

						default:
							goto ErrorIllegalInstruction;
							break;
						}
					break;

				case vmvecPop:
					outStream(JITOpImmImm,FALSE)	// FALSE if compiling this instruction instream.
					Jp[n] = (NUM)(Np-ncodes);		/*  Update the jump label translation vector. */
					Jp[++n] = (NUM)(Np-ncodes);     /*  Update the jump label translation vector. */
					i = pcodes[n];					/*  Recover the original VM immediate type argument. */
					Jp[++n] = (NUM)(Np-ncodes);     /*  Update the jump label translation vector. */
					k = pcodes[n];					/*  Recover the original VM immediate destination argument. */
					switch (k)						/*  Switch on the destination argument. */
						{
						case 0:									
							/*** Argument pointer destination.***/
							switch (i)
								{
								case TYFLOAT:	
									/*** Float                    ***/					  
									/* fstp  dword ptr [ebx]	    */ *Np++ = 0xD9; *Np++ = 0x1B;
									break;

								case TYREAL:	
									/*** Number                   ***/					  
									/* fstp  qword ptr [ebx]	    */ *Np++ = 0xDD; *Np++ = 0x1B;
									break;

								default:
									goto ErrorIllegalInstruction;
									break;
								}
							break;	

						case 1:									
							/*** Source pointer destination.  ***/  
							switch (i)
								{
								case TYFLOAT:	
									/*** Float                    ***/					  
									/* fstp  dword ptr [esi]	    */ *Np++ = 0xD9; *Np++ = 0x1E;
									break;

								case TYREAL:	
									/*** Number                   ***/					  
									/* fstp  qword ptr [esi]	    */ *Np++ = 0xDD; *Np++ = 0x1E;
									break;

								default:
									goto ErrorIllegalInstruction;
									break;
								}
							break;	

						case 2:		
							/*** Target pointer destination.  ***/  
							switch (i)
								{
								case TYFLOAT:	
									/*** Float                    ***/					  
									/* fstp  dword ptr [edi]	    */ *Np++ = 0xD9; *Np++ = 0x1F;
									break;

								case TYREAL:	
									/*** Number                   ***/					  
									/* fstp  qword ptr [edi]	    */ *Np++ = 0xDD; *Np++ = 0x1F;
									break;

								default:
									goto ErrorIllegalInstruction;
									break;
								}
							break;	

						case 3:		
							/*** Drop the top stack item.	  ***/  
							/* fdecstp							*/	*Np++ = 0xD9; *Np++ = 0xF6; 					
							break;	

						default:
							goto ErrorIllegalInstruction;
							break;
						}
					break;

				case vmvecPopNumber:
					outStream(JITOp,FALSE)	// FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* fstp qword ptr Rp[target]         */ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));	Np+=4;
					break;

				case vmvecPush:
					outStream(JITOpImmImm,FALSE)	// FALSE if compiling this instruction instream.
					Jp[n] = (NUM)(Np-ncodes);		/*  Update the jump label translation vector. */
					Jp[++n] = (NUM)(Np-ncodes);     /*  Update the jump label translation vector. */
					i = pcodes[n];					/*  Recover the original VM immediate type argument. */
					Jp[++n] = (NUM)(Np-ncodes);     /*  Update the jump label translation vector. */
					k = pcodes[n];					/*  Recover the original VM immediate origin argument. */
					switch (k)						/*  Switch on the origin argument. */
						{
						case 0:									
							/*** Argument pointer origin.***/
							switch (i)
								{
								case TYFLOAT:	
									/*** Float                    ***/					  
									/* fld   dword ptr [ebx]	    */ *Np++ = 0xD9; *Np++ = 0x03;
									break;

								case TYREAL:	
									/*** Number                   ***/					  
									/* fld   qword ptr [ebx]	    */ *Np++ = 0xDD; *Np++ = 0x03;
									break;

								default:
									goto ErrorIllegalInstruction;
									break;
								}
							break;	

						case 1:									
							/*** Source pointer origin.  ***/  
							switch (i)
								{
								case TYFLOAT:	
									/*** Float                    ***/					  
									/* fld   dword ptr [esi]	    */ *Np++ = 0xD9; *Np++ = 0x06;
									break;

								case TYREAL:	
									/*** Number                   ***/					  
									/* fld   qword ptr [esi]	    */ *Np++ = 0xDD; *Np++ = 0x06;
									break;

								default:
									goto ErrorIllegalInstruction;
									break;
								}
							break;	

						case 2:		
							/*** Target pointer origin.  ***/  
							switch (i)
								{
								case TYFLOAT:	
									/*** Float                    ***/					  
									/* fld	  dword ptr [edi]	    */ *Np++ = 0xD9; *Np++ = 0x07;
									break;

								case TYREAL:	
									/*** Number                   ***/					  
									/* fld	  qword ptr [edi]		*/ *Np++ = 0xD9; *Np++ = 0x07;
									break;

								default:
									goto ErrorIllegalInstruction;
									break;
								}
							break;	

						case 3:		
							/*** Dup the top stack item.	  ***/  
							/* fld  st(0)						*/	*Np++ = 0xD9; *Np++ = 0xC0; 					
							break;	

						case 4:		
							/*** Push minus one on the stack  ***/  
							/* fldz								*/	*Np++ = 0xD9; *Np++ = 0xEE; 					
							/* fld1								*/	*Np++ = 0xD9; *Np++ = 0xE8; 					
							/* fsubp st(1),st(0)				*/	*Np++ = 0xDE; *Np++ = 0xE9; 					
							break;	

						case 5:		
							/*** Push one on the stack        ***/  
							/* fld1								*/	*Np++ = 0xD9; *Np++ = 0xE8; 					
							break;	

						case 6:		
							/*** Push zero on the stack       ***/  
							/* fldz								*/	*Np++ = 0xD9; *Np++ = 0xEE; 					
							break;	

						default:
							goto ErrorIllegalInstruction;
							break;
						}
					break;

				case vmvecPushNumber:
					outStream(JITOp,FALSE)	// FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment          ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* fld  qword ptr Rp[target]         */ *Np++ = 0xDD; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));	Np+=4;
					break;

				case  vmvecNumScalar:
					outStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment     ***/ Jp[++n] = (NUM)(Np-ncodes);
					switch (pcodes[n])
						{
						case 0:	/* distance */        
							/* mov   saveESI,esi            */ *Np++ = 0x89;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveEDI,edi            */ *Np++ = 0x89;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,argPtrID			*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ecx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x8C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,srcPtrID			*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x94; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,argIncID			*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   esi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xB4; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,srcIncID			*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xBC; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Rp[counter]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* fild  dword ptr argZERO      */ *Np++ = 0xDB; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGZERO<<BITSIZEOFNUM)); Np+=4;
							/*** VMVECDIFFNUMBERLOOP:     ***/
							/* fld   qword ptr [edx]        */ *Np++ = 0xDD; *Np++ = 0x02;
							/* fsub  qword ptr [ecx]        */ *Np++ = 0xDC; *Np++ = 0x21;
							/* fmul  st(0),st(0)            */ *Np++ = 0xDC; *Np++ = 0xC8;
							/* faddp st(1),st(0)            */ *Np++ = 0xDE; *Np++ = 0xC1;
							/* add   ecx,esi                */ *Np++ = 0x03; *Np++ = 0xCE;
							/* add   edx,edi                */ *Np++ = 0x03; *Np++ = 0xD7;
							/* dec   eax                    */ *Np++ = 0x48;
							/* jg    VMVECDIFFNUMBERLOOP    */ *Np++ = 0x7F; *Np++ = 0xF1;
							/* fsqrt                     	*/ *Np++ = 0xD9; *Np++ = 0xFA;
							/* fstp  qword ptr Rp[target]	*/ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							/* mov   esi,saveESI            */ *Np++ = 0x8B;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM));Np+=4;
							/* mov   edi,saveEDI            */ *Np++ = 0x8B;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM));Np+=4;
							break;

						case 1:	/* dot product */        
							/* mov   saveESI,esi            */ *Np++ = 0x89;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveEDI,edi            */ *Np++ = 0x89;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,argPtrID			*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ecx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x8C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress;Np+=4;
							/* mov   eax,srcPtrID			*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x94; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,argIncID			*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   esi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xB4; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,srcIncID			*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xBC; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Rp[counter]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* fild  dword ptr argZERO      */ *Np++ = 0xDB; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGZERO<<BITSIZEOFNUM)); Np+=4;
							/*** VMVECDOTNUMBERLOOP:      ***/
							/* fld   qword ptr [edx]        */ *Np++ = 0xDD; *Np++ = 0x02;
							/* fmul  qword ptr [ecx]        */ *Np++ = 0xDC; *Np++ = 0x09;
							/* faddp st(1),st(0)            */ *Np++ = 0xDE; *Np++ = 0xC1;
							/* add   ecx,esi                */ *Np++ = 0x03; *Np++ = 0xCE;
							/* add   edx,edi                */ *Np++ = 0x03; *Np++ = 0xD7;
							/* dec   eax                    */ *Np++ = 0x48;
							/* jg    VMVECDOTNUMBERLOOP     */ *Np++ = 0x7F; *Np++ = 0xF3;
							/* fstp  qword ptr Rp[target]	*/ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							/* mov   esi,saveESI            */ *Np++ = 0x8B;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM));Np+=4;
							/* mov   edi,saveEDI            */ *Np++ = 0x8B;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM));Np+=4;
							break;

						case 2:	/* sum */        
							/* mov   saveESI,esi            */ *Np++ = 0x89;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,argPtrID			*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ecx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x8C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,argIncID			*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   esi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xB4; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Rp[counter]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* fild  dword ptr argZERO      */ *Np++ = 0xDB; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGZERO<<BITSIZEOFNUM)); Np+=4;
							/*** VMVECSUMNUMBERLOOP:      ***/
							/* fld   qword ptr [ecx]        */ *Np++ = 0xDD; *Np++ = 0x01;
							/* faddp st(1),st(0)            */ *Np++ = 0xDE; *Np++ = 0xC1;
							/* add   ecx,esi                */ *Np++ = 0x03; *Np++ = 0xCE;
							/* dec   eax                    */ *Np++ = 0x48;
							/* jg    VMVECSUMNUMBERLOOP     */ *Np++ = 0x7F; *Np++ = 0xF7;
							/* fstp  qword ptr Rp[target]	*/ *Np++ = 0xDD; *Np++ = 0x9D; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD)); Np+=4;
							/* mov   esi,saveESI            */ *Np++ = 0x8B;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM));Np+=4;
							break;

						default:
							goto ErrorIllegalInstruction; 
							break;
						}
					break;

				case  vmvecNumVector:
					outStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment     ***/ Jp[++n] = (NUM)(Np-ncodes);
					switch (pcodes[n])
						{
						case 0:	/* add */        
							/* mov   saveEBX,ebx            */ *Np++ = 0x89;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveEDI,edi            */ *Np++ = 0x89;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveESI,esi            */ *Np++ = 0x89;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[ARGPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ecx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x8C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress;Np+=4;
							/* mov   eax,Vr[SRCPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ebx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x9C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[TARPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x94; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[ARGINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   esi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xB4; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[SRCINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   eax,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x84; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   Vr[SRCINC],eax			*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[TARINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xBC; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Rp[counter]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** VMVECADDNUMBERLOOP:      ***/
							/* fld   qword ptr [ebx]        */ *Np++ = 0xDD; *Np++ = 0x03;
							/* fadd  qword ptr [ecx]        */ *Np++ = 0xDC; *Np++ = 0x01;
							/* fstp  qword ptr [edx]        */ *Np++ = 0xDD; *Np++ = 0x1A;
							/* add   ecx,esi                */ *Np++ = 0x03; *Np++ = 0xCE;
							/* add   ebx,Vr[SRCINC]			*/ *Np++ = 0x03; *Np++ = 0x9D; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM));Np+=4;
							/* add   edx,edi                */ *Np++ = 0x03; *Np++ = 0xD7;
							/* dec   eax                    */ *Np++ = 0x48;
							/* jg    VMVECADDNUMBERLOOP     */ *Np++ = 0x7F; *Np++ = 0xED;
							/* mov   ebx,saveEBX            */ *Np++ = 0x8B;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM));Np+=4;
							/* mov   edi,saveEDI            */ *Np++ = 0x8B;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM));Np+=4;
							/* mov   esi,saveESI            */ *Np++ = 0x8B;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM));Np+=4;
							break;

						case 1:	/* div */        
							/* mov   saveEBX,ebx            */ *Np++ = 0x89;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveEDI,edi            */ *Np++ = 0x89;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveESI,esi            */ *Np++ = 0x89;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[ARGPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ecx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x8C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress;Np+=4;
							/* mov   eax,Vr[SRCPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ebx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x9C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[TARPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x94; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[ARGINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   esi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xB4; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[SRCINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   eax,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x84; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   Vr[SRCINC],eax			*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[TARINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xBC; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Rp[counter]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** VMVECDIVNUMBERLOOP:      ***/
							/* fld   qword ptr [ebx]        */ *Np++ = 0xDD; *Np++ = 0x03;
							/* fdiv  qword ptr [ecx]        */ *Np++ = 0xDC; *Np++ = 0x31;
							/* fstp  qword ptr [edx]        */ *Np++ = 0xDD; *Np++ = 0x1A;
							/* add   ecx,esi                */ *Np++ = 0x03; *Np++ = 0xCE;
							/* add   ebx,Vr[SRCINC]			*/ *Np++ = 0x03; *Np++ = 0x9D; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM));Np+=4;
							/* add   edx,edi                */ *Np++ = 0x03; *Np++ = 0xD7;
							/* dec   eax                    */ *Np++ = 0x48;
							/* jg    VMVECDIVNUMBERLOOP     */ *Np++ = 0x7F; *Np++ = 0xED;
							/* mov   ebx,saveEBX            */ *Np++ = 0x8B;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM));Np+=4;
							/* mov   edi,saveEDI            */ *Np++ = 0x8B;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM));Np+=4;
							/* mov   esi,saveESI            */ *Np++ = 0x8B;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM));Np+=4;
							break;

						case 2:	/* mov */        
							/* mov   saveEBX,ebx            */ *Np++ = 0x89;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveEDI,edi            */ *Np++ = 0x89;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveESI,esi            */ *Np++ = 0x89;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[SRCPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ebx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x9C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[TARPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x94; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[SRCINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   esi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xB4; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[TARINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xBC; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Rp[counter]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** VMVECMOVNUMBERLOOP:      ***/
							/* fld   qword ptr [ebx]        */ *Np++ = 0xDD; *Np++ = 0x03;
							/* fstp  qword ptr [edx]        */ *Np++ = 0xDD; *Np++ = 0x1A;
							/* add   ebx,esi                */ *Np++ = 0x03; *Np++ = 0xDE;
							/* add   edx,edi                */ *Np++ = 0x03; *Np++ = 0xD7;
							/* dec   eax                    */ *Np++ = 0x48;
							/* jg    VMVECMOVNUMBERLOOP     */ *Np++ = 0x7F; *Np++ = 0xF5;
							/* mov   ebx,saveEBX            */ *Np++ = 0x8B;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM));Np+=4;
							/* mov   edi,saveEDI            */ *Np++ = 0x8B;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM));Np+=4;
							/* mov   esi,saveESI            */ *Np++ = 0x8B;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM));Np+=4;
							break;

						case 3:	/* mul */        
							/* mov   saveEBX,ebx            */ *Np++ = 0x89;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveEDI,edi            */ *Np++ = 0x89;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveESI,esi            */ *Np++ = 0x89;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[ARGPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ecx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x8C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[SRCPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ebx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x9C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[TARPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x94; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[ARGINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   esi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xB4; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[SRCINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   eax,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x84; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   Vr[SRCINC],eax			*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[TARINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xBC; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Rp[counter]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** VMVECMULNUMBERLOOP:      ***/
							/* fld   qword ptr [ebx]        */ *Np++ = 0xDD; *Np++ = 0x03;
							/* fmul  qword ptr [ecx]        */ *Np++ = 0xDC; *Np++ = 0x09;
							/* fstp  qword ptr [edx]        */ *Np++ = 0xDD; *Np++ = 0x1A;
							/* add   ecx,esi                */ *Np++ = 0x03; *Np++ = 0xCE;
							/* add   ebx,Vr[SRCINC]			*/ *Np++ = 0x03; *Np++ = 0x9D; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM));Np+=4;
							/* add   edx,edi                */ *Np++ = 0x03; *Np++ = 0xD7;
							/* dec   eax                    */ *Np++ = 0x48;
							/* jg    VMVECMULNUMBERLOOP     */ *Np++ = 0x7F; *Np++ = 0xED;
							/* mov   ebx,saveEBX            */ *Np++ = 0x8B;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM));Np+=4;
							/* mov   edi,saveEDI            */ *Np++ = 0x8B;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM));Np+=4;
							/* mov   esi,saveESI            */ *Np++ = 0x8B;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM));Np+=4;
							break;

						case 4:	/* sub */        
							/* mov   saveEBX,ebx            */ *Np++ = 0x89;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveEDI,edi            */ *Np++ = 0x89;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveESI,esi            */ *Np++ = 0x89;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[ARGPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGPTRID<<BITSIZEOFNUM));Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ecx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x8C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[SRCPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ebx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x9C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[TARPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x94; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[ARGINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   esi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xB4; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[SRCINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   eax,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x84; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   Vr[SRCINC],eax			*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[TARINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xBC; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Rp[counter]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/*** VMVECSUBNUMBERLOOP:      ***/
							/* fld   qword ptr [ebx]        */ *Np++ = 0xDD; *Np++ = 0x03;
							/* fsub  qword ptr [ecx]        */ *Np++ = 0xDC; *Np++ = 0x21;
							/* fstp  qword ptr [edx]        */ *Np++ = 0xDD; *Np++ = 0x1A;
							/* add   ecx,esi                */ *Np++ = 0x03; *Np++ = 0xCE;
							/* add   ebx,Vr[SRCINC]			*/ *Np++ = 0x03; *Np++ = 0x9D; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM));Np+=4;
							/* add   edx,edi                */ *Np++ = 0x03; *Np++ = 0xD7;
							/* dec   eax                    */ *Np++ = 0x48;
							/* jg    VMVECSUBNUMBERLOOP     */ *Np++ = 0x7F; *Np++ = 0xED;
							/* mov   ebx,saveEBX            */ *Np++ = 0x8B;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM));Np+=4;
							/* mov   edi,saveEDI            */ *Np++ = 0x8B;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM));Np+=4;
							/* mov   esi,saveESI            */ *Np++ = 0x8B;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM));Np+=4;
							break;

						case 5:	/* swp */        
							/* mov   saveEBX,ebx            */ *Np++ = 0x89;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveEDI,edi            */ *Np++ = 0x89;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM)); Np+=4;
							/* mov   saveESI,esi            */ *Np++ = 0x89;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM)); Np+=4;
							/* mov   eax,Vr[SRCPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   ebx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x9C; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[TARPTRID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARPTRID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edx,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0x94; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[SRCINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   esi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xB4; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Vr[TARINCID]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARINCID<<BITSIZEOFNUM)); Np+=4;
							/* shl   eax,4					*/ *Np++ = 0xC1; *Np++ = 0xE0; *Np++ = 0x04; 
							/* mov   edi,dword ptr [eax+Rp]	*/ *Np++ = 0x8B; *Np++ = 0xBC; *Np++ = 0x28; *(LpNUM)Np = RpRelAddress; Np+=4;
							/* mov   eax,Rp[counter]		*/ *Np++ = 0x8B; *Np++ = 0x85; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD)); Np+=4;
							/* mov   Vr[SRCINC],eax			*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM)); Np+=4;
							/*** VMVECSWPNUMBERLOOP:      ***/
							/* fld   qword ptr [edx]        */ *Np++ = 0xDD; *Np++ = 0x02;
							/* fld   qword ptr [ebx]        */ *Np++ = 0xDD; *Np++ = 0x03;
							/* fstp  qword ptr [edx]        */ *Np++ = 0xDD; *Np++ = 0x1A;
							/* fstp  qword ptr [ebx]        */ *Np++ = 0xDD; *Np++ = 0x1B;
							/* add   ebx,esi                */ *Np++ = 0x03; *Np++ = 0xDE;
							/* add   edx,edi                */ *Np++ = 0x03; *Np++ = 0xD7;
							/* dec   Vr[SRCINC]             */ *Np++ = 0xFF; *Np++ = 0x8D; *(LpNUM)Np = (VrRelAddress+(SRCINC<<BITSIZEOFNUM));Np+=4;
							/* jg    VMVECSWPNUMBERLOOP     */ *Np++ = 0x7F; *Np++ = 0xEC;
							/* mov   ebx,saveEBX            */ *Np++ = 0x8B;*Np++ = 0x9D;*(LpNUM)Np = (VrRelAddress+(SAVEEBX<<BITSIZEOFNUM));Np+=4;
							/* mov   edi,saveEDI            */ *Np++ = 0x8B;*Np++ = 0xBD;*(LpNUM)Np = (VrRelAddress+(SAVEEDI<<BITSIZEOFNUM));Np+=4;
							/* mov   esi,saveESI            */ *Np++ = 0x8B;*Np++ = 0xB5;*(LpNUM)Np = (VrRelAddress+(SAVEESI<<BITSIZEOFNUM));Np+=4;
							break;

						default:
							goto ErrorIllegalInstruction; 
							break;
						}
					break;

				case  vmvecSetIncrements:
					outStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* sub   eax,eax				*/ *Np++ = 0x2B; *Np++ = 0xC0; 
					/* mov   al,01h					*/ *Np++ = 0xB0; *Np++ = pcode.u.Am1; 
					/* mov   Vr[ARGINCID],eax		*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGINCID<<BITSIZEOFNUM)); Np+=4;
					/* mov   al,01h					*/ *Np++ = 0xB0; *Np++ = pcode.u.Am2; 
					/* mov   Vr[SRCINCID],eax		*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCINCID<<BITSIZEOFNUM)); Np+=4;
					/* mov   al,01h					*/ *Np++ = 0xB0; *Np++ = pcode.u.Am3; 
					/* mov   Vr[TARINCID],eax		*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARINCID<<BITSIZEOFNUM)); Np+=4;
					Vr[ARGINCID] = pcode.u.Am1;
					Vr[SRCINCID] = pcode.u.Am2;
					Vr[TARINCID] = pcode.u.Am3;
					break;

				case  vmvecSetPointers:
					outStream(JITOp,FALSE)  // FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/* sub   eax,eax				*/ *Np++ = 0x2B; *Np++ = 0xC0; 
					/* mov   al,01h					*/ *Np++ = 0xB0; *Np++ = pcode.u.Am1; 
					/* mov   Vr[ARGPTRID],eax		*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(ARGPTRID<<BITSIZEOFNUM)); Np+=4;
					/* mov   al,01h					*/ *Np++ = 0xB0; *Np++ = pcode.u.Am2; 
					/* mov   Vr[SRCPTRID],eax		*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(SRCPTRID<<BITSIZEOFNUM)); Np+=4;
					/* mov   al,01h					*/ *Np++ = 0xB0; *Np++ = pcode.u.Am3; 
					/* mov   Vr[TARPTRID],eax		*/ *Np++ = 0x89; *Np++ = 0x85; *(LpNUM)Np = (VrRelAddress+(TARPTRID<<BITSIZEOFNUM)); Np+=4;
					Vr[ARGPTRID] = pcode.u.Am1;
					Vr[SRCPTRID] = pcode.u.Am2;
					Vr[TARPTRID] = pcode.u.Am3;
					break;

				case vmvecSwapCC:
					outStream(JITOpImm,FALSE)	// FALSE if compiling this instruction instream.
					/*** Set Pc/Nc allignment     ***/ Jp[n]   = (NUM)(Np-ncodes);        
					/*** Set Pc/Nc allignment     ***/ Jp[++n] = (NUM)(Np-ncodes);        
					m = pcodes[n];					/*  Recover the original VM immediate condition argument. */
					/* Switch on the contition argument  */
					switch (m) /* Switch on destination argument */
						{
						case 0: /* lt */
							/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
							/* fcom								*/ *Np++ = 0xD8; *Np++ = 0xD1;
							/* fnstsw ax                        */ *Np++ = 0xDF; *Np++ = 0xE0; 
							/* test   ah,0x01                   */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x01; 
							/* je                               */ *Np++ = 0x74; *Np++ = 0x08;
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fstp   st(3)  	                */ *Np++ = 0xDD; *Np++ = 0xDB; 
							/* fstp   st(1)  	                */ *Np++ = 0xDD; *Np++ = 0xD9; 
							break;

						case 1: /* le */
							/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
							/* fcom								*/ *Np++ = 0xD8; *Np++ = 0xD1;
							/* fnstsw ax                        */ *Np++ = 0xDF; *Np++ = 0xE0; 
							/* test   ah,0x41                   */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x41; 
							/* je                               */ *Np++ = 0x74; *Np++ = 0x08;
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fstp   st(3)  	                */ *Np++ = 0xDD; *Np++ = 0xDB; 
							/* fstp   st(1)  	                */ *Np++ = 0xDD; *Np++ = 0xD9; 
							break;

						case 2: /* eq */
							/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
							/* fcom								*/ *Np++ = 0xD8; *Np++ = 0xD1;
							/* fnstsw ax                        */ *Np++ = 0xDF; *Np++ = 0xE0; 
							/* test   ah,0x40                   */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x40; 
							/* je                               */ *Np++ = 0x74; *Np++ = 0x08;
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fstp   st(3)  	                */ *Np++ = 0xDD; *Np++ = 0xDB; 
							/* fstp   st(1)  	                */ *Np++ = 0xDD; *Np++ = 0xD9; 
							break;

						case 3: /* ne */
							/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
							/* fcom								*/ *Np++ = 0xD8; *Np++ = 0xD1;
							/* fnstsw ax                        */ *Np++ = 0xDF; *Np++ = 0xE0; 
							/* test   ah,0x40                   */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x40; 
							/* jne								*/ *Np++ = 0x75; *Np++ = 0x08;
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fstp   st(3)  	                */ *Np++ = 0xDD; *Np++ = 0xDB; 
							/* fstp   st(1)  	                */ *Np++ = 0xDD; *Np++ = 0xD9; 
							break;

						case 4: /* ge */
							/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
							/* fcom								*/ *Np++ = 0xD8; *Np++ = 0xD1;
							/* fnstsw ax                        */ *Np++ = 0xDF; *Np++ = 0xE0; 
							/* test   ah,0x01                   */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x01; 
							/* jne								*/ *Np++ = 0x75; *Np++ = 0x08;
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fstp   st(3)  	                */ *Np++ = 0xDD; *Np++ = 0xDB; 
							/* fstp   st(1)  	                */ *Np++ = 0xDD; *Np++ = 0xD9; 
							break;

						case 5: /* gt */
							/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
							/* fcom								*/ *Np++ = 0xD8; *Np++ = 0xD1;
							/* fnstsw ax                        */ *Np++ = 0xDF; *Np++ = 0xE0; 
							/* test   ah,0x41                   */ *Np++ = 0xF6; *Np++ = 0xC4; *Np++ = 0x41; 
							/* jne								*/ *Np++ = 0x75; *Np++ = 0x08;
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fstp   st(3)  	                */ *Np++ = 0xDD; *Np++ = 0xDB; 
							/* fstp   st(1)  	                */ *Np++ = 0xDD; *Np++ = 0xD9; 
							break;

						case 6: /* true */
							/*** Set Pc/Nc allignment         ***/ Jp[n]   = (NUM)(Np-ncodes);        
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fld    st(1)  	                */ *Np++ = 0xD9; *Np++ = 0xC1; 
							/* fstp   st(3)  	                */ *Np++ = 0xDD; *Np++ = 0xDB; 
							/* fstp   st(1)  	                */ *Np++ = 0xDD; *Np++ = 0xD9; 
							break;

						default:
							goto ErrorIllegalInstruction;
							break;
						}
						break;

				case vmvecUnary:
					outStream(JITOpImm,FALSE)	// FALSE if compiling this instruction instream.
					Jp[n] = (NUM)(Np-ncodes);		/*  Update the jump label translation vector. */
					Jp[++n] = (NUM)(Np-ncodes);     /*  Update the jump label translation vector. */
					k = pcodes[n];					/*  Recover the original VM immediate operator argument. */
					switch (k)						/*  Switch on the operator argument. */
						{
						case 0:									
							/*** abs ***/
							/* fabs             			*/ *Np++ = 0xD9; *Np++ = 0xE1;
							break;

						case 1:									
							/*** cos ***/
							/* fcos             			*/ *Np++ = 0xD9; *Np++ = 0xFF;
							break;

						case 2:									
							/*** dbl ***/
							/* fadd  st(0),st(0)			*/ *Np++ = 0xDC; *Np++ = 0xC0;
							break;

						case 3:									
							/*** dec ***/
							/* fld1             			*/ *Np++ = 0xD9; *Np++ = 0xE8;
							/* fsubp  st(1),st(0)			*/ *Np++ = 0xDE; *Np++ = 0xE9;
							break;

						case 4:									
							/*** inc ***/
							/* fld1             			*/ *Np++ = 0xD9; *Np++ = 0xE8;
							/* faddp  st(1),st(0)			*/ *Np++ = 0xDE; *Np++ = 0xC1;
							break;

						case 5:									
							/*** sin ***/
							/* fsin             			*/ *Np++ = 0xD9; *Np++ = 0xFE;
							break;

						case 6:									
							/*** sqr ***/
							/* fmul  st(0),st(0)			*/ *Np++ = 0xD8; *Np++ = 0xC8;
							break;

						case 7:									
							/*** sqrt ***/
							/* fsqrt             			*/ *Np++ = 0xD9; *Np++ = 0xFA;
							break;

						case 8:									
							/*** tan ***/        
							/* fsincos                      */ *Np++ = 0xD9; *Np++ = 0xFB;
							/* fdivp                        */ *Np++ = 0xDE; *Np++ = 0xF9;
							break;

						default:
							goto ErrorIllegalInstruction;
							break;
						}
					break;
								
				default:
				  JIT_IllegalInstruction:
					{TopOfStack = saveSi; EndRecursion; goto ErrorIllegalInstruction;}
					break;

                JITOp:  /* Out of Stream instructions: Opcode */
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */
                    break;

                JITOpIa:  /* Out of Stream instructions: Opcode,IntArg */
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					SAVEALLREGS
					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */

					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					*(LpNUM)Np = pcodes[n]; Np+=4; /*  Move the original VM Integer Argument to the native code vector. */
					SAVEALLREGS
                    break;

                /* JITOpIaIa: */ /* Out of Stream instructions: Opcode,IntArg,IntArg */
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */

					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM Integer Argument to the native code vector. */

					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM Integer Argument to the native code vector. */
					break;

                /* JITOpIaIaIa:  */ /* Out of Stream instructions: Opcode,IntArg,IntArg,IntArg */
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */

					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM Integer Argument to the native code vector. */

					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM Integer Argument to the native code vector. */

					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM Integer Argument to the native code vector. */
                    break;

                /* JITOpIaIaJa:  */ /* Out of Stream instructions: Opcode,IntArg,IntArg,JmpArg */
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */

					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM Integer Argument to the native code vector. */

					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM Integer Argument to the native code vector. */

					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					Mp[n] = -1; /*  Mark this jump label argument as needing translation on the second pass. */
					*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM Jump Argument to the native code vector. */
                    break;

                JITOpJa:  /* Out of Stream instructions: Opcode,JmpArg */
					Jp[n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode); /*  Move the "call" to the native code vector. */

					Jp[++n] = (NUM)(Np-ncodes); /*  Update the jump label translation vector. */
					Mp[n] = -1; /*  Mark this jump label argument as needing translation on the second pass. */
					*(LpNUM)Np = pcodes[n];Np+=4; /*  Move the original VM Jump Argument to the native code vector. */
                    break;
					JITOpNoArgs:
					Jp[n] = (NUM)(Np-ncodes);	/*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode);	/*  Move the "call" to the native code vector. */
					break;

				JITOpImm:
					Jp[n] = (NUM)(Np-ncodes);		/*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode);	    /*  Move the "call" to the native code vector. */
					Jp[++n] = (NUM)(Np-ncodes);     /*  Update the jump label translation vector. */
					*(LpNUM)Np = pcodes[n];Np+=4;   /*  Move the original VM immediate argument to the native code vector. */
					break;

				JITOpImmImm:
					Jp[n] = (NUM)(Np-ncodes);		/*  Update the jump label translation vector. */
					saveNativeJumpBack(pcode);	    /*  Move the "call" to the native code vector. */
					Jp[++n] = (NUM)(Np-ncodes);     /*  Update the jump label translation vector. */
					*(LpNUM)Np = pcodes[n];Np+=4;   /*  Move the original VM immediate argument to the native code vector. */
					Jp[++n] = (NUM)(Np-ncodes);     /*  Update the jump label translation vector. */
					*(LpNUM)Np = pcodes[n];Np+=4;   /*  Move the original VM immediate argument to the native code vector. */
					break;
				}
			
			}

		}
	nativeLen = (NUM)(Np - ncodes);	/* Compute the final length of the native code vector. */
	TByteVector_SetMaxIndex(gCP,gTP,Tp[0],nativeLen);
	if (ncodes != ByteArray(Tp[0])) 
		{
		++passCounter;
		if (passCounter >= 2) 
			{
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_PCODE);
			}
		goto FirstPass;
		}
	/* End JIT First Pass: Translate the pcodes into native codes. */
	
	/* Start JIT Second Pass: Fixup the jump labels in the native code vector. */
	/* SecondPass: */
	ncodes = Np = ByteArray(Tp[0]);
	for (n = 0; n < Pc->itsMaxItemIndex; ++n)
		{
		/* Do we need to fixup an absolute jump label argument at this location? */
		if (Mp[n] == -1)
			{
			m = Jp[n]; /* Load the native code index where the jump label is stored. */
			i = *((LpNUM)(ncodes+m)); /* Load the native code jump label value as originally stored. */
			*((LpNUM)(ncodes+m)) = Jp[i]; /* Save the native code jump label value after fixup. */
			}
		else
		/* Do we need to fixup a relative jump label argument at this location? */
		if (Mp[n] == -2)
			{
			m = Jp[n]; /* Load the native code index where the jump label is stored. */
			i = *((LpNUM)(ncodes+m)); /* Load the native code jump label value as originally stored. */
			*((LpNUM)(ncodes+m)) = (Jp[i] - (m+4)); /* Save the native code jump label value after fixup. */
			}
		}
	/* End JIT Second Pass: Fixup the jump labels in the native code vector. */


	EndJustInTimeCompiler:
	/* Set the native code vector. */
	proc->NativeCodeVector = Tp[0].u.ByteVector;
	}
/*  ===================================================================== */
/*  End Just In Time Compiler.                                            */
/*  ===================================================================== */

/*  Set error handler off. */

onErrorHandler = gCP->Tval_VOID;

/*  Check for system stack or recursion overflow. */

StartRecursion;

if (proc->itsObjectType == TYLAMBDA)
    self = (TLambda*)proc;
else
    {EndRecursion; goto ErrorNotAnLambda;}
    
saveSi = TopOfStack;

/*  Load the Pcode Vector register. */
/*  Make sure this Procedure object has a Pcode Vector. */
if (self->PcodeVector == NIL)
    {TopOfStack = saveSi; EndRecursion; goto ErrorMissingPcodes;}
else
    Pc = self->PcodeVector;

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
	for (i = 0; i < n; ++i)
		{
		Rp[i] = atHMBind(Rv->itsDictionaryArray,i).Value;
		}
	}

/*  Load the Native Code Vector register. */
/*  Make sure this Procedure object has a Native Code Vector. */
if (self->NativeCodeVector == NIL)
    {TopOfStack = saveSi; EndRecursion; goto ErrorMissingPcodes;}
else
    Nc = (TByteVector*)self->NativeCodeVector;

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

/*  Reload the Virtual Machine Modifier base addresses and index factors. */
/*	These registers are all set with one level of indirection (handles) */
/*	because the AMPVOFFSET must point to the Pv Structures handle, */
/*  since the Pv structure of an Lambda may resize during execution. */

Rp[AMGVOFFSET].Tag = TYNUM; Rp[AMGVOFFSET].u.Int = ((NUM)&gCP->TLambda_assign->itsGlobalValue - (NUM)gCP->TLambda_assign);
Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)&argv[0].u.Structure->itsDictionaryArray;
Rp[AMAVOFFSET].Tag = TYNUM; Rp[AMAVOFFSET].u.Int = (NUM)&argv[0];
Rp[AMTVOFFSET].Tag = TYNUM; Rp[AMTVOFFSET].u.Int = (NUM)Fb;
Rp[AMPVOFFSET].Tag = TYNUM; Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)&Pv->itsDictionaryArray;
Rp[AMCVOFFSET].Tag = TYNUM; Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)&Cv->itsDictionaryArray;
Rp[AMREGISTER].Tag = TYNUM; Rp[AMREGISTER].u.Int = (NUM)&Rp[0];
Vr[ARGZERO] = 0;

if (TESTON)
	{
	FMemory_SystemDiagnostic(gCP,gTP,1,&gCP->Tval_FALSE);
	}

/*  Set the Instruction Pointer to the start of the pcode vector.     */
jitSetInstructionPtr

//Fetch:
/*  ===================================================================== */
/*  Here we manage all of the 32 bit special cases, Each of these special */
/*  cases are managed as quickly as possible. Obviously virtual machine   */
/*  instructions interpreted in this section execute fairly fast.         */
/*  ===================================================================== */

/*  Fetch the next Instruction opcode. */
jitFetchOpcode

/* Here we manage the virtual machine's debugger facility (if necessary). */
/* Note1: This adds only one test to the fetch loop whenever the debugger */
/*        facility is NOT turned on.                                      */ 
/* Note2: This code should be removed when an interrupt debugger, using   */
/*        the vmdebugger instruction, is implemented.                     */ 
DebugResume:

/* Main instruction emulation loop.      */
/* Note: If we have not defined the host */
/*       jumpToOpcodeEntry cpu assembler */ 
/*		 macro, then we fall through to  */
/*       the normal C switch statement.  */
jumpToOpcodeEntry(pcode.u.Pcode)
switch (pcode.u.Pcode)
    {
    case  VMADD:
		jitSetLabel(LVMADD)
		jitOfflineEntry
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
		jitOfflineExit;
        break;
        
    case  VMDIV:
		jitSetLabel(LVMDIV)
		jitOfflineEntry
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
		jitOfflineExit;
		break;

    case  VMDIVR:
		jitSetLabel(LVMDIVR)
		jitOfflineEntry
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
		jitOfflineExit;
        break;

    case  VMMUL:
		jitSetLabel(LVMMUL)
		jitOfflineEntry
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
		jitOfflineExit;
        break;
        
    case  VMSUB:
		jitSetLabel(LVMSUB)
		jitOfflineEntry
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
		jitOfflineExit;
        break;
                
    case  VMMOVEN:
		jitSetLabel(LVMMOVEN)
 		jitOfflineEntry
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
 		jitOfflineExit
		break;

    case  VMCADD:
		jitSetLabel(LVMCADD)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = source->u.Char + argument->u.Char;
		target->Tag = TYCHAR;
		jitOfflineExit;
		break;
 
    case  VMCDIV:
		jitSetLabel(LVMCDIV)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.Char == 0) goto IllegalDivide;
		target->u.Char = source->u.Char / argument->u.Char;
		target->Tag = TYCHAR;
		jitOfflineExit;
        break;

    case  VMMOVEI:
		jitSetLabel(LVMMOVEI)
		jitOfflineEntry
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
            /* BadMoveiValue: */
                retValue = gCP->FVmScript_ERROR_ILLEGAL_VALUE;
                goto NestedReturn;
                break;
			}
		jitOfflineExit
        break;

    case  VMMOVEU:
		jitSetLabel(LVMMOVEU)
		jitOfflineEntry
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
            /* BadMoveuValue: */
                retValue = gCP->FVmScript_ERROR_ILLEGAL_VALUE;
                goto NestedReturn;
                break;
			}
		jitOfflineExit
        break;

    case  VMCMUL:
		jitSetLabel(LVMCMUL)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = source->u.Char * argument->u.Char;
		target->Tag = TYCHAR;
		jitOfflineExit;
        break;

    case  VMCSUB:
		jitSetLabel(LVMCSUB)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = source->u.Char - argument->u.Char;
		target->Tag = TYCHAR;
		jitOfflineExit;
        break;
 
     case  VMIADD:
		jitSetLabel(LVMIADD)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int + argument->u.Int;
		target->Tag = TYNUM;
		jitOfflineExit;
		break;
 
     case  VMUADD:
		jitSetLabel(LVMUADD)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.UInt = source->u.UInt + argument->u.UInt;
		target->Tag = TYUNUM;
		jitOfflineExit;
		break;

 
    case  VMIDIV:
		jitSetLabel(LVMIDIV)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.Int == 0) goto IllegalDivide;
		target->u.Int = source->u.Int / argument->u.Int;
		target->Tag = TYNUM;
		jitOfflineExit;
        break;
 
    case  VMUDIV:
		jitSetLabel(LVMUDIV)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.UInt == 0) goto IllegalDivide;
		target->u.UInt = source->u.UInt / argument->u.UInt;
		target->Tag = TYUNUM;
		jitOfflineExit;
        break;

    case  VMIDIVR:
		jitSetLabel(LVMIDIVR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.Int == 0) goto IllegalDivide;
		target->u.Int = source->u.Int % argument->u.Int;
		target->Tag = TYNUM;
		jitOfflineExit;
        break;

		
    case  VMUDIVR:
		jitSetLabel(LVMIDIVR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.UInt == 0) goto IllegalDivide;
		target->u.UInt = source->u.UInt % argument->u.UInt;
		target->Tag = TYUNUM;
		jitOfflineExit;
        break;

    case  VMIMUL:
		jitSetLabel(LVMIMUL)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int * argument->u.Int;
		target->Tag = TYNUM;
		jitOfflineExit;
        break;

    case  VMUMUL:
		jitSetLabel(LVMUMUL)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.UInt = source->u.UInt * argument->u.UInt;
		target->Tag = TYUNUM;
		jitOfflineExit;
        break;

    case  VMISUB:
		jitSetLabel(LVMISUB)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int - argument->u.Int;
		target->Tag = TYNUM;
		jitOfflineExit;
        break;

    case  VMUSUB:
		jitSetLabel(LVMUSUB)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.UInt = source->u.UInt - argument->u.UInt;
		target->Tag = TYUNUM;
		jitOfflineExit;
        break;
 
    case  VMNADD:
		jitSetLabel(LVMNADD)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = source->u.Real + argument->u.Real;
		target->Tag = TYREAL;
		jitOfflineExit;
        break;
 
    case  VMNDIV:
		jitSetLabel(LVMNDIV)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.Real == 0) goto IllegalDivide;
		target->u.Real = source->u.Real / argument->u.Real;
		target->Tag = TYREAL;
		jitOfflineExit;
        break;

    case  VMNDIVR:
		jitSetLabel(LVMNDIVR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (argument->u.Real == 0) goto IllegalDivide;
        gTP->FVmscript_Fraction = modf(source->u.Real / argument->u.Real, &gTP->FVmscript_Integer);
        target->u.Real = argument->u.Real * gTP->FVmscript_Fraction;
		target->Tag = TYREAL;
		jitOfflineExit;
        break;

    case  VMNMUL:
		jitSetLabel(LVMNMUL)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = source->u.Real * argument->u.Real;
		target->Tag = TYREAL;
		jitOfflineExit;
        break;

    case  VMNSUB:
		jitSetLabel(LVMNSUB)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = source->u.Real - argument->u.Real;
		target->Tag = TYREAL;
		jitOfflineExit;
        break;
 
    case  VMADDI:
		jitSetLabel(LVMADDI)
		jitOfflineEntry
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
                    
                    case TYNUM:
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
                        target->u.Int = source->u.UInt + argument->u.Int;
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
                        target->u.Int = source->u.Int + argument->u.UInt;
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
		jitOfflineExit;
        break;                        
 
    case  VMADDU:
		jitSetLabel(LVMADDU)
		jitOfflineEntry
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
                    
                    case TYNUM:
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
                        target->u.UInt = source->u.UInt + argument->u.Int;
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
                        target->u.UInt = source->u.Int + argument->u.UInt;
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
		jitOfflineExit;
        break;
                        
    case  VMADDN:
		jitSetLabel(LVMADDN)
		jitOfflineEntry
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
		jitOfflineExit;
        break;
                        
    case  VMSUBI:
		jitSetLabel(LVMSUBI)
		jitOfflineEntry
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
		jitOfflineExit;
        break;
        
                        
    case  VMSUBU:
		jitSetLabel(LVMSUBU)
		jitOfflineEntry
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
		jitOfflineExit;
        break;
        
    case  VMSUBN:
		jitSetLabel(LVMSUBN)
		jitOfflineEntry
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
		jitOfflineExit;
        break;
        
    case  VMDIVI:
		jitSetLabel(LVMDIVI)
		jitOfflineEntry
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
		jitOfflineExit;
        break;
        
        
    case  VMDIVU:
		jitSetLabel(LVMDIVU)
		jitOfflineEntry
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
            BadDiviValue:
                retValue = gCP->FVmScript_ERROR_VMDIVU_BAD_VALUE;
                goto NestedReturn;
                break;
            }
        target->Tag = TYUNUM;
        break;
		jitOfflineExit;
        break;
        
    case  VMDIVN:
		jitSetLabel(LVMDIVN)
		jitOfflineEntry
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
		jitOfflineExit;
        break;

    case  VMDIVRI:
		jitSetLabel(LVMDIVRI)
		jitOfflineEntry
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
		jitOfflineExit;
        break;


    case  VMDIVRU:
		jitSetLabel(LVMDIVRU)
		jitOfflineEntry
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
                        goto BadDivuValue;
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
		jitOfflineExit;
        break;

    case  VMMULI:
		jitSetLabel(LVMMULI)
		jitOfflineEntry
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
		jitOfflineExit;
        break;


    case  VMMULU:
		jitSetLabel(LVMMULU)
		jitOfflineEntry
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
		jitOfflineExit;
        break;

    case  VMMULN:
		jitSetLabel(LVMMULN)
		jitOfflineEntry
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
		jitOfflineExit;
        break;

    case  VMAND:
		jitSetLabel(LVMAND)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int & argument->u.Int;
        target->Tag = TYNUM;
		jitOfflineExit;
		break;
            
    case  VMOR:
		jitSetLabel(LVMOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int | argument->u.Int;
        target->Tag = TYNUM;
		jitOfflineExit;
		break;
            
    case  VMSHL:
		jitSetLabel(LVMSHL)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int << argument->u.Int;
        target->Tag = TYNUM;
		jitOfflineExit;
		break;
            
    case  VMSHR:
		jitSetLabel(LVMSHR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int >> argument->u.Int;
        target->Tag = TYNUM;
		jitOfflineExit;
		break;
            
    case  VMXOR:
		jitSetLabel(LVMXOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int ^ argument->u.Int;
        target->Tag = TYNUM;
		jitOfflineExit;
		break;
            
    case  VMIAND:
		jitSetLabel(LVMIAND)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int & argument->u.Int;
        target->Tag = TYNUM;
		jitOfflineExit;
		break;
            
    case  VMIOR:
		jitSetLabel(LVMIOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int | argument->u.Int;
        target->Tag = TYNUM;
		jitOfflineExit;
		break;
            
    case  VMIXOR:
		jitSetLabel(LVMIXOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = source->u.Int ^ argument->u.Int;
        target->Tag = TYNUM;
		jitOfflineExit;
		break;
            
    case  VMIANDB:
		jitSetLabel(LVMIANDB)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = !(source->u.Int & argument->u.Int);
        target->Tag = TYNUM;
		jitOfflineExit;
		break;
            
    case  VMIORB:
		jitSetLabel(LVMIORB)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = !(source->u.Int | argument->u.Int);
        target->Tag = TYNUM;
		jitOfflineExit;
		break;
            
    case  VMIXORB:
		jitSetLabel(LVMIXORB)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = !(source->u.Int ^ argument->u.Int);
        target->Tag = TYNUM;
		jitOfflineExit;
		break;
            
    case  VMARGCOUNT:
		jitSetLabel(LVMARGCOUNT)
		jitOfflineEntry
        /* Assign the argument count to target */
		target = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++));
        target->Tag = TYNUM;
        target->u.Int = argc;
		jitOfflineExit;
        break;

    case  VMARGFETCH:
		jitSetLabel(LVMARGFETCH)
		jitOfflineEntry
        /*  Load a pointer to the retValue argument */
        source   = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)));
        target   = ((TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)));
        *target = argv[source->u.Int];
		jitOfflineExit;
        break;
        
    case  VMONERROR:
		jitSetLabel(LVMONERROR)
		jitOfflineEntry
        /*  Load a pointer to the retValue argument */
        retValue = *((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)));
        target   = ((TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)));
        /*  Assign the result value to the target argument, and */
        /*  to the error handler variable for this current Lambda. */
        onErrorHandler = retValue;
        if (retValue.Tag == TYERROR) goto NestedReturn;
        *target = retValue;
		jitOfflineExit;
        break;
        
    case  VMAPPLY:
		jitSetLabel(LVMAPPLY)
		jitOfflineEntry
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
		LVMPOP:
		jitOfflineEntry
        if (pcode.u.Am1 != AMVOID) *((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++))) = gTP->TvalStack[--TopOfStack];
        if (pcode.u.Am2 != AMVOID) *((TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++))) = gTP->TvalStack[--TopOfStack];
        if (pcode.u.Am3 != AMVOID) *((TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip++))) = gTP->TvalStack[--TopOfStack];
        jitOfflineExit;
        break;
    case  VMPUSH:
		jitSetLabel(LVMPUSH)
		jitOfflineEntry
        if (pcode.u.Am1 != AMVOID) gTP->TvalStack[TopOfStack++] = *((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)));
        if (pcode.u.Am2 != AMVOID) gTP->TvalStack[TopOfStack++] = *((TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)));
        if (pcode.u.Am3 != AMVOID) gTP->TvalStack[TopOfStack++] = *((TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip++)));
        jitOfflineExit;
        break;
        
    case  VMCALL:
		jitSetLabel(LVMCALL)
		jitOfflineEntry
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
        /*  FVmScript_Eval          */
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
				Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)&argv[0].u.Structure->itsDictionaryArray;
				Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)&Pv->itsDictionaryArray;
				Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)&Cv->itsDictionaryArray;
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
				Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)&argv[0].u.Structure->itsDictionaryArray;
				Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)&Pv->itsDictionaryArray;
				Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)&Cv->itsDictionaryArray;
                break;

            case TYLAMBDA:
                oldSi = TopOfStack;
                _TObject_RecordFrame
                retValue = _VmEvaluate(source->u.Lambda,n,&gTP->TvalStack[TopOfStack-n]);
                _TObject_CheckFrame
                TopOfStack = oldSi - m;
				Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)&argv[0].u.Structure->itsDictionaryArray;
				Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)&Pv->itsDictionaryArray;
				Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)&Cv->itsDictionaryArray;
				break;

            case TYCONTINUATION:
                oldSi = TopOfStack;
                _TObject_RecordFrame
                retValue = TContinuation_Evaluate(gCP,gTP,*source, n,&gTP->TvalStack[TopOfStack-n]);
                _TObject_CheckFrame
                TopOfStack = oldSi - m;
				Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)&argv[0].u.Structure->itsDictionaryArray;
				Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)&Pv->itsDictionaryArray;
				Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)&Cv->itsDictionaryArray;
                break;

            case TYMACRO:
                oldSi = TopOfStack;
                _TObject_RecordFrame
                retValue = _VmEvaluate(source->u.Lambda,n,&gTP->TvalStack[TopOfStack-n]);
                _TObject_CheckFrame
                TopOfStack = oldSi - m;
				Rp[AMSVOFFSET].Tag = TYNUM; Rp[AMSVOFFSET].u.Int = ((Sv == NIL) || (argc <= 0) || (argv[0].Tag != TYSTRUCTURE) || (self->ArgumentVariables->itsMaxItemIndex < 1) || (BindArray(TOBJ(self->ArgumentVariables))[0].Key != (TObject*)gCP->TLambda_self)) ? 0 : **(NUM**)&argv[0].u.Structure->itsDictionaryArray;
				Rp[AMPVOFFSET].u.Int = (Pv == NIL) ? 0 : **(NUM**)&Pv->itsDictionaryArray;
				Rp[AMCVOFFSET].u.Int = (Cv == NIL) ? 0 : **(NUM**)&Cv->itsDictionaryArray;
                break;

            case TYERROR:
                TopOfStack -= m;
                retValue = *source;
                goto CallErrorMgr;
                break;

            default:
            //BadCallValue:
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
		jitOfflineExit;
        break;
        
    case  VMSEND:
		jitSetLabel(LVMSEND)
		jitOfflineEntry
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
			/*  Modify the send if the target object has properties		*/
			/*  which may may take prescedence over the class methods	*/
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
							gTP->FVmscript_iindex = TStructure_SearchKey(gCP,gTP,(TStructure*)gTP->FVmscript_iargument.u.Object, source->u.Object, (short*)&gTP->FVmscript_Selection);
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
            gTP->FVmscript_iindex = TStructure_SearchKey(gCP,gTP,(TStructure*)gTP->FVmscript_isource.u.Object, source->u.Object, (short*)&gTP->FVmscript_Selection);
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
		jitSetLabel(LvmnatJmpLEInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int <= argument->u.Int) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpLTInteger:
		jitSetLabel(LvmnatJmpLTInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int < argument->u.Int) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpEQInteger:
		jitSetLabel(LvmnatJmpEQInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int == argument->u.Int) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpNEInteger:
		jitSetLabel(LvmnatJmpNEInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int != argument->u.Int) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpGEInteger:
		jitSetLabel(LvmnatJmpGEInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int >= argument->u.Int) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpGTInteger:
		jitSetLabel(LvmnatJmpGTInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Int > argument->u.Int) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;
        
    case  vmnatJmpLEUInteger:
		jitSetLabel(LvmnatJmpLEUInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt <= argument->u.UInt) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpLTUInteger:
		jitSetLabel(LvmnatJmpLTUInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt < argument->u.UInt) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpEQUInteger:
		jitSetLabel(LvmnatJmpEQUInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt == argument->u.UInt) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpNEUInteger:
		jitSetLabel(LvmnatJmpNEUInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt != argument->u.UInt) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpGEUInteger:
		jitSetLabel(LvmnatJmpGEUInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt >= argument->u.UInt) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpGTUInteger:
		jitSetLabel(LvmnatJmpGTUInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.UInt > argument->u.UInt) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpLENumber:
		jitSetLabel(LvmnatJmpLENumber)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real <= argument->u.Real) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpLTNumber:
		jitSetLabel(LvmnatJmpLTNumber)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real < argument->u.Real) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpEQNumber:
		jitSetLabel(LvmnatJmpEQNumber)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real == argument->u.Real) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpNENumber:
		jitSetLabel(LvmnatJmpNENumber)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real != argument->u.Real) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpGENumber:
		jitSetLabel(LvmnatJmpGENumber)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real >= argument->u.Real) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatJmpGTNumber:
		jitSetLabel(LvmnatJmpGTNumber)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        n           = *(Ip++);
        if (source->u.Real > argument->u.Real) Ip = setIpFromOffset(n);
		jitOfflineExit;
        break;

    case  vmnatAndInteger:
		jitSetLabel(LvmnatAndInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int & argument->u.Int;
		jitOfflineExit;
        break;

    case  vmnatOrInteger:
		jitSetLabel(LvmnatOrInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int | argument->u.Int;
		jitOfflineExit;
        break;

    case  vmnatShlInteger:
		jitSetLabel(LvmnatShlInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int << argument->u.Int;
		jitOfflineExit;
        break;

    case  vmnatShrInteger:
		jitSetLabel(LvmnatShrInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int >> argument->u.Int;
		jitOfflineExit;
        break;

    case  vmnatXorInteger:
		jitSetLabel(LvmnatXorInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int ^ argument->u.Int;
		jitOfflineExit;
        break;

    case  vmnatAddInteger:
		jitSetLabel(LvmnatAddInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int + argument->u.Int;
		jitOfflineExit;
        break;

    case  vmnatAddNumber:
		jitSetLabel(LvmnatAddNumber)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Real = source->u.Real + argument->u.Real;
		jitOfflineExit;
        break;

    case  vmnatDivInteger:
		jitSetLabel(LvmnatDivInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int / argument->u.Int;
		jitOfflineExit;
        break;

    case  vmnatDivNumber:
		jitSetLabel(LvmnatDivNumber)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Real = source->u.Real / argument->u.Real;
		jitOfflineExit;
        break;

    case  vmnatDivrInteger:
		jitSetLabel(LvmnatDivrInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int % argument->u.Int;
		jitOfflineExit;
        break;

    case  vmnatDivrNumber:
		jitSetLabel(LvmnatDivrNumber)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        gTP->FVmscript_Fraction = modf(source->u.Real / argument->u.Real, &gTP->FVmscript_Integer);
        target->u.Real = argument->u.Real * gTP->FVmscript_Fraction;
		jitOfflineExit;
        break;

    case  vmnatMulInteger:
		jitSetLabel(LvmnatMulInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int * argument->u.Int;
		jitOfflineExit;
        break;

    case  vmnatMulNumber:
		jitSetLabel(LvmnatMulNumber)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Real = source->u.Real * argument->u.Real;
		jitOfflineExit;
        break;

    case  vmnatSubInteger:
		jitSetLabel(LvmnatSubInteger)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Int = source->u.Int - argument->u.Int;
		jitOfflineExit;
        break;

    case  vmnatSubNumber:
		jitSetLabel(LvmnatSubNumber)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
        target->u.Real = source->u.Real - argument->u.Real;
		jitOfflineExit;
        break;

    case  vmnatLoadCharacter:
		jitSetLabel(LvmnatLoadCharacter)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Int = *((LpCHAR)source);
		target->Tag = TYNUM;
		jitOfflineExit;
        break;

    case  vmnatLoadFloat:
		jitSetLabel(LvmnatLoadFloat)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Real = *((LpFLOAT)source);
		target->Tag = TYREAL;
		jitOfflineExit;
        break;

    case  vmnatLoadInteger:
		jitSetLabel(LvmnatLoadInteger)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Int = *((LpNUM)source);
		target->Tag = TYNUM;
		jitOfflineExit;
        break;

    case  vmnatLoadUInteger:
		jitSetLabel(LvmnatLoadUInteger)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.UInt = *((LpUNUM)source);
		target->Tag = TYUNUM;
		jitOfflineExit;
        break;

	case  vmnatLoadLong:
		jitSetLabel(LvmnatLoadLong)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Int = *((LpNUM32)source);
		target->Tag = TYNUM;
		jitOfflineExit;
        break;

    case  vmnatLoadNumber:
		jitSetLabel(LvmnatLoadNumber)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Real = *((LpREAL)source);
		target->Tag = TYREAL;
		jitOfflineExit;
        break;

    case  vmnatLoadObject:
		jitSetLabel(LvmnatLoadObject)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Obj = *((LpOBJ)source);
		target->Tag = (target->u.Obj == NIL) ? TYVOID : target->u.Object->itsObjectType;
		jitOfflineExit;
        break;

    case  vmnatLoadShort:
		jitSetLabel(LvmnatLoadShort)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        target->u.Int = *((LpSHORT)source);
		target->Tag = TYNUM;
		jitOfflineExit;
        break;

    case  vmnatSaveCharacter:
		jitSetLabel(LvmnatSaveCharacter)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpCHAR)target) = (char)source->u.Int;
		jitOfflineExit;
        break;

    case  vmnatSaveFloat:
		jitSetLabel(LvmnatSaveFloat)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpFLOAT)target) = (FLOAT)source->u.Real;
		jitOfflineExit;
        break;

    case  vmnatSaveInteger:
		jitSetLabel(LvmnatSaveInteger)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpNUM)target) = (NUM)source->u.Int;
		jitOfflineExit;
        break;

    case  vmnatSaveLong:
		jitSetLabel(LvmnatSaveLong)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpNUM32)target) = (NUM)source->u.Int;
		jitOfflineExit;
        break;

    case  vmnatSaveNumber:
		jitSetLabel(LvmnatSaveNumber)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpREAL)target) = (REAL)source->u.Real;
		jitOfflineExit;
        break;

    case  vmnatSaveObject:
		jitSetLabel(LvmnatSaveObject)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpOBJ)target) = (source->Tag == TYVOID) ? NIL : source->u.Obj;
		jitOfflineExit;
        break;

    case  vmnatSaveShort:
		jitSetLabel(LvmnatSaveShort)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        *((LpSHORT)target) = (short)source->u.Int;
		jitOfflineExit;
        break;

    case  VMJMPLE:
		jitSetLabel(LVMJMPLE)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3; JIp = 0;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag <= argument->Tag)
                    JIp = jitSetIpFromOffset(n);
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
                    JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYBYTEVECTOR:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						if (source->u.Object == argument->u.Object) 
							JIp = jitSetIpFromOffset(n);
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
							if (i < k) {JIp = jitSetIpFromOffset(n); break;}
							if (--m > 0) goto VMJMPLEBYTVEC;
							if (ByteVector(*source)->itsMaxItemIndex < ByteVector(*argument)->itsMaxItemIndex) 
								{JIp = jitSetIpFromOffset(n); break;}
							if (ByteVector(*source)->itsMaxItemIndex > ByteVector(*argument)->itsMaxItemIndex) goto VMJMPLEBYTVEC_CONTINUE;
							if (FPredicate2_QuickCompare(gCP,gTP,&ByteVector(*source)->itsCdr,&ByteVector(*argument)->itsCdr) <= 0)
								{JIp = jitSetIpFromOffset(n); break;}
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
							JIp = jitSetIpFromOffset(n);
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
							{JIp = jitSetIpFromOffset(n); break;}
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						else
							{
							srcP = CharArray(*source);
							argP = CharArray(*argument);
							VMJMPLECOMPARE:
							VMJMPLESTRNGSTRNG:
							i = *srcP++;
							k = *argP++;
							if (i > k) goto VMJMPLESTRNGSTRNG_CONTINUE;
							if (i < k) {JIp = jitSetIpFromOffset(n); break;}
							if (i != 0) goto VMJMPLESTRNGSTRNG;
							JIp = jitSetIpFromOffset(n);
							VMJMPLESTRNGSTRNG_CONTINUE:
							break;
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						break;
					}
                break;
                                
            case TYBITVECTOR:
				if (argument->Tag == TYBITVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
					else
						{
						srcP = ByteArray(*source);
						argP = ByteArray(*argument);
						i = BitVector(*source)->itsMaxItemIndex;
						k = BitVector(*argument)->itsMaxItemIndex;
						m = (((i < k) ? i : k) + 7)/8;
						VMJMPLEBITVEC:
						i = *srcP++;
						k = *argP++;
						if (i > k) goto VMJMPLEBITVEC_CONTINUE;
						if (i < k) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPLEBITVEC;
						if (BitVector(*source)->itsMaxItemIndex < BitVector(*argument)->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (BitVector(*source)->itsMaxItemIndex > BitVector(*argument)->itsMaxItemIndex) goto VMJMPLEBITVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&BitVector(*source)->itsCdr,&BitVector(*argument)->itsCdr) <= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPLEBITVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYINTVECTOR:
				if (argument->Tag == TYINTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
						if (i < k) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPLEINTVEC;
						if (IntVector(*source)->itsMaxItemIndex < IntVector(*argument)->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (IntVector(*source)->itsMaxItemIndex > IntVector(*argument)->itsMaxItemIndex) goto VMJMPLEINTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&IntVector(*source)->itsCdr,&IntVector(*argument)->itsCdr) <= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPLEINTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYSHORTVECTOR:
				if (argument->Tag == TYSHORTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
						if (i < k) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPLESHTVEC;
						if (source->u.ShortVector->itsMaxItemIndex < argument->u.ShortVector->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (source->u.ShortVector->itsMaxItemIndex > argument->u.ShortVector->itsMaxItemIndex) goto VMJMPLESHTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&source->u.ShortVector->itsCdr,&argument->u.ShortVector->itsCdr) <= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPLESHTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
					else
						{
						LpFLOAT srcP = FloatArray(*source);
						LpFLOAT argP = FloatArray(*argument);
						i = source->u.FltVector->itsMaxItemIndex;
						k = argument->u.FltVector->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPLEFLTVEC:
						si = *srcP++;
						ai = *argP++;
						if (si > ai) goto VMJMPLEFLTVEC_CONTINUE;
						if (ai < ai) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPLEFLTVEC;
						if (FltVector(*source)->itsMaxItemIndex < FltVector(*argument)->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (FltVector(*source)->itsMaxItemIndex > FltVector(*argument)->itsMaxItemIndex) goto VMJMPLEFLTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&FltVector(*source)->itsCdr,&FltVector(*argument)->itsCdr) <= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPLEFLTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYNUMVECTOR:
				if (argument->Tag == TYNUMVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
						if (ai < ai) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPLENUMVEC;
						if (NumVector(*source)->itsMaxItemIndex < NumVector(*argument)->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (NumVector(*source)->itsMaxItemIndex > NumVector(*argument)->itsMaxItemIndex) goto VMJMPLENUMVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&NumVector(*source)->itsCdr,&NumVector(*argument)->itsCdr) <= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPLENUMVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYNUMMATRIX:
				if (argument->Tag == TYNUMMATRIX)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
						if (ai < ai) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPLENUMMAT;
						if (NumMatrix(*source)->itsMaxItemIndex < NumMatrix(*argument)->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (NumMatrix(*source)->itsMaxItemIndex > NumMatrix(*argument)->itsMaxItemIndex) goto VMJMPLENUMMAT_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&NumMatrix(*source)->itsCdr,&NumMatrix(*argument)->itsCdr) <= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPLENUMMAT_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) <= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt <= argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt <= argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.UInt <= argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt <= argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt <  cp->itsReal ||
						   ((REAL)source->u.UInt == cp->itsReal && 0.0 <= cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag <= argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                                
            case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int <= argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Int <= argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.Int <= argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int <= argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int <  cp->itsReal ||
						   ((REAL)source->u.Int == cp->itsReal && 0.0 <= cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag <= argument->Tag)
                            JIp = jitSetIpFromOffset(n);
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
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Real <= argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real <= argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real <= argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real <  cp->itsReal ||
						   (source->u.Real == cp->itsReal && 0.0 <= cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag <= argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char <= argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Char <= argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.Char <= argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char <= argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char <  cp->itsReal ||
						   ((REAL)source->u.Char == cp->itsReal && 0.0 <= cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						break;

                    default:
                        if (source->Tag <= argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool <= argument->u.Bool)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag <= argument->Tag)
                            JIp = jitSetIpFromOffset(n);
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
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYNUM:
                    if (cp->itsReal <  (REAL)argument->u.Int ||
					   (cp->itsReal == (REAL)argument->u.Int  && cp->itsImag <= 0.0))
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if (cp->itsReal <  argument->u.Real ||
					   (cp->itsReal == argument->u.Real  && cp->itsImag <= 0.0))
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYCHAR:
                    if (cp->itsReal <  (REAL)argument->u.Char ||
					   (cp->itsReal == (REAL)argument->u.Char && cp->itsImag <= 0.0))
                        JIp = jitSetIpFromOffset(n);
                    break;

				case TYCPX:
					if (cp->itsReal <  argument->u.Complex->itsReal ||
					   (cp->itsReal == argument->u.Complex->itsReal &&
					    cp->itsImag <= argument->u.Complex->itsImag))
                        JIp = jitSetIpFromOffset(n);
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag <= argument->Tag)
                        JIp = jitSetIpFromOffset(n);
                    break;
				}
				break;

			default:
 				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
                if (isCompareLE(&retValue))
                    JIp = jitSetIpFromOffset(n);
                break;
            }
		jitOfflineExitJmpCC;
        break;

	case  VMJMPLT:
		jitSetLabel(LVMJMPLT)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3; JIp = 0;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag < argument->Tag)
                    JIp = jitSetIpFromOffset(n);
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
                    JIp = jitSetIpFromOffset(n);
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
						if (i < k) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPLTBYTVEC;
						if (ByteVector(*source)->itsMaxItemIndex < ByteVector(*argument)->itsMaxItemIndex) 
							{JIp = jitSetIpFromOffset(n); break;}
						if (ByteVector(*source)->itsMaxItemIndex > ByteVector(*argument)->itsMaxItemIndex) goto VMJMPLTBYTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&ByteVector(*source)->itsCdr,&ByteVector(*argument)->itsCdr) < 0)
							{JIp = jitSetIpFromOffset(n); break;}
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
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
						if (i < k) {JIp = jitSetIpFromOffset(n); break;}
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
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
					if (i < k) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPLTBITVEC;
					if (BitVector(*source)->itsMaxItemIndex < BitVector(*argument)->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (BitVector(*source)->itsMaxItemIndex > BitVector(*argument)->itsMaxItemIndex) goto VMJMPLTBITVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&BitVector(*source)->itsCdr,&BitVector(*argument)->itsCdr) < 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPLTBITVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					JIp = jitSetIpFromOffset(n);
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
					if (i < k) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPLTINTVEC;
					if (IntVector(*source)->itsMaxItemIndex < IntVector(*argument)->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (IntVector(*source)->itsMaxItemIndex > IntVector(*argument)->itsMaxItemIndex) goto VMJMPLTINTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&IntVector(*source)->itsCdr,&IntVector(*argument)->itsCdr) < 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPLTINTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					JIp = jitSetIpFromOffset(n);
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
					if (i < k) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPLTSHTVEC;
					if (source->u.ShortVector->itsMaxItemIndex < argument->u.ShortVector->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (source->u.ShortVector->itsMaxItemIndex > argument->u.ShortVector->itsMaxItemIndex) goto VMJMPLTSHTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.ShortVector->itsCdr,&argument->u.ShortVector->itsCdr) < 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPLTSHTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					LpFLOAT srcP = FloatArray(*source);
					LpFLOAT argP = FloatArray(*argument);
					i = source->u.FltVector->itsMaxItemIndex;
					k = argument->u.FltVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPLTFLTVEC:
					si = *srcP++;
					ai = *argP++;
					if (si > ai) goto VMJMPLTFLTVEC_CONTINUE;
					if (si < ai) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPLTFLTVEC;
					if (FltVector(*source)->itsMaxItemIndex < FltVector(*argument)->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (FltVector(*source)->itsMaxItemIndex > FltVector(*argument)->itsMaxItemIndex) goto VMJMPLTFLTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&FltVector(*source)->itsCdr,&FltVector(*argument)->itsCdr) < 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPLTFLTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					JIp = jitSetIpFromOffset(n);
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
					if (si < ai) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPLTNUMVEC;
					if (NumVector(*source)->itsMaxItemIndex < NumVector(*argument)->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (NumVector(*source)->itsMaxItemIndex > NumVector(*argument)->itsMaxItemIndex) goto VMJMPLTNUMVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&NumVector(*source)->itsCdr,&NumVector(*argument)->itsCdr) < 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPLTNUMVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					JIp = jitSetIpFromOffset(n);
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
					if (si < ai) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPLTNUMMAT;
					if (NumMatrix(*source)->itsMaxItemIndex < NumMatrix(*argument)->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (NumMatrix(*source)->itsMaxItemIndex > NumMatrix(*argument)->itsMaxItemIndex) goto VMJMPLTNUMMAT_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&NumMatrix(*source)->itsCdr,&NumMatrix(*argument)->itsCdr) < 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPLTNUMMAT_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt < argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt < argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.UInt < argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt < argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt < cp->itsReal ||
							((REAL)source->u.UInt == cp->itsReal && 0.0 < cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag < argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                                
            case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int < argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Int < argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Int < argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int < argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int < cp->itsReal ||
							((REAL)source->u.Int == cp->itsReal && 0.0 < cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag < argument->Tag)
                            JIp = jitSetIpFromOffset(n);
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
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Real < argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real < argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real < argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real < cp->itsReal ||
							(source->u.Real == cp->itsReal && 0.0 < cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag < argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char < argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Char < argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Char < argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char < argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char < cp->itsReal ||
							((REAL)source->u.Char == cp->itsReal && 0.0 < cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						break;

                  default:
                        if (source->Tag < argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool < argument->u.Bool)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag < argument->Tag)
                            JIp = jitSetIpFromOffset(n);
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
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYNUM:
                    if (cp->itsReal <  (REAL)argument->u.Int ||
					   (cp->itsReal == (REAL)argument->u.Int && cp->itsImag < 0.0))
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if (cp->itsReal <  argument->u.Real ||
					   (cp->itsReal == argument->u.Real && cp->itsImag < 0.0))
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYCHAR:
                    if (cp->itsReal <  (REAL)argument->u.Char ||
					   (cp->itsReal == (REAL)argument->u.Char && cp->itsImag < 0.0))
                        JIp = jitSetIpFromOffset(n);
                    break;

				case TYCPX:
					if (cp->itsReal <  argument->u.Complex->itsReal ||
					   (cp->itsReal == argument->u.Complex->itsReal &&
					    cp->itsImag < argument->u.Complex->itsImag))
                        JIp = jitSetIpFromOffset(n);
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag < argument->Tag)
                        JIp = jitSetIpFromOffset(n);
                    break;
				}
				break;

			default:
 				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
                if (isCompareLT(&retValue))
                    JIp = jitSetIpFromOffset(n);
                break;
            }
		jitOfflineExitJmpCC;
        break;

    case  VMJMPEQ:
		jitSetLabel(LVMJMPEQ)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3; JIp = 0;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag == argument->Tag)
                    JIp = jitSetIpFromOffset(n);
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
                    JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYBYTEVECTOR:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						if (source->u.Object == argument->u.Object) 
							JIp = jitSetIpFromOffset(n);
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
								JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						break;
				
					case TYTEXT:
						srcP = SymbolArray(*source);
						argP = argument->u.Text;
						goto VMJMPEQCOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
							VMJMPEQSTRNGSTRNG_CONTINUE:
							break;
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						break;
					}
                break;
                                
            case TYBITVECTOR:
				if (argument->Tag == TYBITVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						VMJMPEQBITVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYINTVECTOR:
				if (argument->Tag == TYINTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						VMJMPEQINTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYSHORTVECTOR:
				if (argument->Tag == TYSHORTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						VMJMPEQSHTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
					else
						{
						LpFLOAT srcP = FloatArray(*source);
						LpFLOAT argP = FloatArray(*argument);
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
							JIp = jitSetIpFromOffset(n);
						VMJMPEQFLTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYNUMVECTOR:
				if (argument->Tag == TYNUMVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						VMJMPEQNUMVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYNUMMATRIX:
				if (argument->Tag == TYNUMMATRIX)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						VMJMPEQNUMMAT_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) == 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt == argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt == argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.UInt == argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt == argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt == cp->itsReal && 0.0 == cp->itsImag)
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag == argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                                
            case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int == argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Int == argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.Int == argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int == argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int == cp->itsReal && 0.0 == cp->itsImag)
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag == argument->Tag)
                            JIp = jitSetIpFromOffset(n);
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
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Real == argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real == argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real == argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real == cp->itsReal && 0.0 == cp->itsImag)
                            JIp = jitSetIpFromOffset(n);
						break;
 
					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;

                    default:
                        if (source->Tag == argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char == argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Char == argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Char == argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char == argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char == cp->itsReal && 0.0 == cp->itsImag)
                            JIp = jitSetIpFromOffset(n);
						break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag == argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool == argument->u.Bool)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag == argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;

			case TYCPX:
				cp = source->u.Complex;
				switch (argument->Tag)
				{
                case TYUNUM:
                    if (cp->itsReal == (REAL)argument->u.UInt && cp->itsImag == 0.0)
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYNUM:
                    if (cp->itsReal == (REAL)argument->u.Int && cp->itsImag == 0.0)
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if (cp->itsReal == argument->u.Real && cp->itsImag == 0.0)
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYCHAR:
                    if (cp->itsReal == (REAL)argument->u.Char && cp->itsImag == 0.0)
                        JIp = jitSetIpFromOffset(n);
                    break;

				case TYCPX:
					if (cp->itsReal == argument->u.Complex->itsReal &&
						cp->itsImag == argument->u.Complex->itsImag)
                        JIp = jitSetIpFromOffset(n);
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag == argument->Tag)
                        JIp = jitSetIpFromOffset(n);
                    break;
				}
				break;

			default:
 				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
                if (isCompareEQ(&retValue))
                    JIp = jitSetIpFromOffset(n);
                break;
            }
		jitOfflineExitJmpCC;
        break;

    case  VMJMPNE:
		jitSetLabel(LVMJMPNE)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3; JIp = 0;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag != argument->Tag)
                    JIp = jitSetIpFromOffset(n);
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
                    JIp = jitSetIpFromOffset(n);
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
						if (i != k) {JIp = jitSetIpFromOffset(n); break;} 
						if (--m > 0) goto VMJMPNEBYTVEC;
						if (ByteVector(*source)->itsMaxItemIndex != ByteVector(*argument)->itsMaxItemIndex) 
							{JIp = jitSetIpFromOffset(n); break;}
						if (FPredicate2_QuickCompare(gCP,gTP,&ByteVector(*source)->itsCdr,&ByteVector(*argument)->itsCdr) != 0) 
							{JIp = jitSetIpFromOffset(n); break;}
						// VMJMPNEBYTVEC_CONTINUE:
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						break;
				
					case TYTEXT:
						srcP = SymbolArray(*source);
						argP = argument->u.Text;
						goto VMJMPNECOMPARE;
						break;

					default:
						if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
							JIp = jitSetIpFromOffset(n);
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
						if (i != k) {JIp = jitSetIpFromOffset(n); break;}
						if (i != 0) goto VMJMPNESTRNGSTRNG;
						// VMJMPNESTRNGSTRNG_CONTINUE:
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
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
					if (i != k) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPNEBITVEC;
					if (BitVector(*source)->itsMaxItemIndex != BitVector(*argument)->itsMaxItemIndex) 
						{JIp = jitSetIpFromOffset(n); break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&BitVector(*source)->itsCdr,&BitVector(*argument)->itsCdr) != 0)
						{JIp = jitSetIpFromOffset(n); break;}
					// VMJMPNEBITVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					JIp = jitSetIpFromOffset(n);
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
					if (i != k) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPNEINTVEC;
					if (IntVector(*source)->itsMaxItemIndex != IntVector(*argument)->itsMaxItemIndex) 
						{JIp = jitSetIpFromOffset(n); break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&IntVector(*source)->itsCdr,&IntVector(*argument)->itsCdr) != 0)
						{JIp = jitSetIpFromOffset(n); break;}
					// VMJMPNEINTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					JIp = jitSetIpFromOffset(n);
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
					if (i != k) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPNESHTVEC;
					if (source->u.ShortVector->itsMaxItemIndex != argument->u.ShortVector->itsMaxItemIndex) 
						{JIp = jitSetIpFromOffset(n); break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.ShortVector->itsCdr,&argument->u.ShortVector->itsCdr) != 0)
						{JIp = jitSetIpFromOffset(n); break;}
					// VMJMPNESHTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					LpFLOAT srcP = FloatArray(*source);
					LpFLOAT argP = FloatArray(*argument);
					i = source->u.FltVector->itsMaxItemIndex;
					k = argument->u.FltVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPNEFLTVEC:
					si = *srcP++;
					ai = *argP++;
					if (si != ai) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPNEFLTVEC;
					if (source->u.FltVector->itsMaxItemIndex != argument->u.FltVector->itsMaxItemIndex) 
						{JIp = jitSetIpFromOffset(n); break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.FltVector->itsCdr,&argument->u.FltVector->itsCdr) != 0)
						{JIp = jitSetIpFromOffset(n); break;}
					// VMJMPNEFLTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					JIp = jitSetIpFromOffset(n);
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
					if (si != ai) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPNENUMVEC;
					if (source->u.NumVector->itsMaxItemIndex != argument->u.NumVector->itsMaxItemIndex) 
						{JIp = jitSetIpFromOffset(n); break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.NumVector->itsCdr,&argument->u.NumVector->itsCdr) != 0)
						{JIp = jitSetIpFromOffset(n); break;}
					// VMJMPNENUMVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					JIp = jitSetIpFromOffset(n);
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
					if (si != ai) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPNENUMMAT;
					if (source->u.NumMatrix->itsMaxItemIndex != argument->u.NumMatrix->itsMaxItemIndex) 
						{JIp = jitSetIpFromOffset(n); break;}
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.NumMatrix->itsCdr,&argument->u.NumMatrix->itsCdr) != 0)
						{JIp = jitSetIpFromOffset(n); break;}
					//VMJMPNENUMMAT_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) != 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt != argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt != argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.UInt != argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt != argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt != cp->itsReal || 0.0 != cp->itsImag)
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag != argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                                
            case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int != argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Int != argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Int != argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int != argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int != cp->itsReal || 0.0 != cp->itsImag)
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag != argument->Tag)
                            JIp = jitSetIpFromOffset(n);
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
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Real != argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real != argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real != argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real != cp->itsReal || 0.0 != cp->itsImag)
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag != argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char != argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Char != argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Char != argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char != argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char != cp->itsReal || 0.0 != cp->itsImag)
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag != argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool != argument->u.Bool)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag != argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;

			case TYCPX:
				cp = source->u.Complex;
				switch (argument->Tag)
				{
                case TYUNUM:
                    if (cp->itsReal != (REAL)argument->u.UInt || cp->itsImag != 0.0)
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYNUM:
                    if (cp->itsReal != (REAL)argument->u.Int || cp->itsImag != 0.0)
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if (cp->itsReal != argument->u.Real || cp->itsImag != 0.0)
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYCHAR:
                    if (cp->itsReal != (REAL)argument->u.Char || cp->itsImag != 0.0)
                        JIp = jitSetIpFromOffset(n);
                    break;

				case TYCPX:
					if (cp->itsReal != argument->u.Complex->itsReal ||
						cp->itsImag != argument->u.Complex->itsImag)
                        JIp = jitSetIpFromOffset(n);
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag != argument->Tag)
                        JIp = jitSetIpFromOffset(n);
                    break;
				}
				break;

			default:
				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
				if (isCompareNE(&retValue))
					JIp = jitSetIpFromOffset(n);
                break;
            }
		jitOfflineExitJmpCC;
        break;

	case  VMJMPGE:
		jitSetLabel(LVMJMPGE)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3; JIp = 0;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag >= argument->Tag)
                    JIp = jitSetIpFromOffset(n);
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
                    JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYBYTEVECTOR:
				switch (argument->Tag)
					{
					case TYBYTEVECTOR:
						if (source->u.Object == argument->u.Object) 
							JIp = jitSetIpFromOffset(n);
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
							if (i > k) {JIp = jitSetIpFromOffset(n); break;}
							if (--m > 0) goto VMJMPGEBYTVEC;
							if (ByteVector(*source)->itsMaxItemIndex > ByteVector(*argument)->itsMaxItemIndex) 
								{JIp = jitSetIpFromOffset(n); break;}
							if (ByteVector(*source)->itsMaxItemIndex < ByteVector(*argument)->itsMaxItemIndex) goto VMJMPGEBYTVEC_CONTINUE;
							if (FPredicate2_QuickCompare(gCP,gTP,&ByteVector(*source)->itsCdr,&ByteVector(*argument)->itsCdr) >= 0)
								{JIp = jitSetIpFromOffset(n); break;}
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
							JIp = jitSetIpFromOffset(n);
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
							{JIp = jitSetIpFromOffset(n); break;}
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						else
							{
							srcP = CharArray(*source);
							argP = CharArray(*argument);
							VMJMPGECOMPARE:
							VMJMPGESTRNGSTRNG:
							i = *srcP++;
							k = *argP++;
							if (i < k) goto VMJMPGESTRNGSTRNG_CONTINUE;
							if (i > k) {JIp = jitSetIpFromOffset(n); break;}
							if (i != 0) goto VMJMPGESTRNGSTRNG;
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
						break;
					}
                break;
                                
            case TYBITVECTOR:
				if (argument->Tag == TYBITVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
						if (i > k) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPGEBITVEC;
						if (BitVector(*source)->itsMaxItemIndex > BitVector(*argument)->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (BitVector(*source)->itsMaxItemIndex < BitVector(*argument)->itsMaxItemIndex) goto VMJMPGEBITVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&BitVector(*source)->itsCdr,&BitVector(*argument)->itsCdr) >= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPGEBITVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYINTVECTOR:
				if (argument->Tag == TYINTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
						if (i > k) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPGEINTVEC;
						if (IntVector(*source)->itsMaxItemIndex > IntVector(*argument)->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (IntVector(*source)->itsMaxItemIndex < IntVector(*argument)->itsMaxItemIndex) goto VMJMPGEINTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&IntVector(*source)->itsCdr,&IntVector(*argument)->itsCdr) >= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPGEINTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYSHORTVECTOR:
				if (argument->Tag == TYSHORTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
						if (i > k) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPGESHTVEC;
						if (source->u.ShortVector->itsMaxItemIndex > argument->u.ShortVector->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (source->u.ShortVector->itsMaxItemIndex < argument->u.ShortVector->itsMaxItemIndex) goto VMJMPGESHTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&source->u.ShortVector->itsCdr,&argument->u.ShortVector->itsCdr) >= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPGESHTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
					else
						{
						LpFLOAT srcP = FloatArray(*source);
						LpFLOAT argP = FloatArray(*argument);
						i = source->u.FltVector->itsMaxItemIndex;
						k = argument->u.FltVector->itsMaxItemIndex;
						m = (i < k) ? i : k;
						VMJMPGEFLTVEC:
						si = *srcP++;
						ai = *argP++;
						if (si < ai) goto VMJMPGEFLTVEC_CONTINUE;
						if (si > ai) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPGEFLTVEC;
						if (FltVector(*source)->itsMaxItemIndex > FltVector(*argument)->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (FltVector(*source)->itsMaxItemIndex < FltVector(*argument)->itsMaxItemIndex) goto VMJMPGEFLTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&FltVector(*source)->itsCdr,&FltVector(*argument)->itsCdr) >= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPGEFLTVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYNUMVECTOR:
				if (argument->Tag == TYNUMVECTOR)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
						if (si > ai) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPGENUMVEC;
						if (NumVector(*source)->itsMaxItemIndex > NumVector(*argument)->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (NumVector(*source)->itsMaxItemIndex < NumVector(*argument)->itsMaxItemIndex) goto VMJMPGENUMVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&NumVector(*source)->itsCdr,&NumVector(*argument)->itsCdr) >= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPGENUMVEC_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYNUMMATRIX:
				if (argument->Tag == TYNUMMATRIX)
					{
					if (source->u.Object == argument->u.Object) 
						JIp = jitSetIpFromOffset(n);
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
						if (si > ai) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPGENUMMAT;
						if (NumMatrix(*source)->itsMaxItemIndex > NumMatrix(*argument)->itsMaxItemIndex)
							{JIp = jitSetIpFromOffset(n); break;}
						if (NumMatrix(*source)->itsMaxItemIndex < NumMatrix(*argument)->itsMaxItemIndex) goto VMJMPGENUMMAT_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&NumMatrix(*source)->itsCdr,&NumMatrix(*argument)->itsCdr) >= 0)
							{JIp = jitSetIpFromOffset(n); break;}
						VMJMPGENUMMAT_CONTINUE:
						break;
						}
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) >= 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt >= argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt >= argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.UInt >= argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt >= argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt >  cp->itsReal ||
						   ((REAL)source->u.UInt == cp->itsReal && 0.0 >= cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag >= argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                                
            case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int >= argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Int >= argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Int >= argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int >= argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int >  cp->itsReal ||
						   ((REAL)source->u.Int == cp->itsReal && 0.0 >= cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag >= argument->Tag)
                            JIp = jitSetIpFromOffset(n);
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
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Real >= (REAL)argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real >= argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real >= (REAL)argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real >  cp->itsReal ||
						   (source->u.Real == cp->itsReal && 0.0 >= cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag >= argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char >= argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Char >= argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if ((REAL)source->u.Char >= argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char >= argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char  > cp->itsReal ||
						   ((REAL)source->u.Char == cp->itsReal && 0.0 >= cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag >= argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool >= argument->u.Bool)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag >= argument->Tag)
                            JIp = jitSetIpFromOffset(n);
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
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYNUM:
                    if (cp->itsReal >  (REAL)argument->u.Int ||
					   (cp->itsReal == (REAL)argument->u.Int && cp->itsImag >= 0.0))
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if (cp->itsReal >  argument->u.Real ||
					   (cp->itsReal == argument->u.Real && cp->itsImag >= 0.0))
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYCHAR:
                    if (cp->itsReal >= (REAL)argument->u.Char ||
					   (cp->itsReal == (REAL)argument->u.Char && cp->itsImag >= 0.0))
                        JIp = jitSetIpFromOffset(n);
                    break;

				case TYCPX:
					if (cp->itsReal  > argument->u.Complex->itsReal ||
					   (source->u.Complex->itsReal == argument->u.Complex->itsReal &&
						source->u.Complex->itsImag >= argument->u.Complex->itsImag))
                        JIp = jitSetIpFromOffset(n);
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag != argument->Tag)
                        JIp = jitSetIpFromOffset(n);
                    break;
				}
				break;

			default:
 				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
                if (isCompareGE(&retValue))
                    JIp = jitSetIpFromOffset(n);
				break;
            }
		jitOfflineExitJmpCC;
        break;

	case  VMJMPGT:
		jitSetLabel(LVMJMPGT)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        source      = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        n           = *(Ip+2); Ip+=3; JIp = 0;
        /*  Perform the comparison and conditional jump */
        switch (source->Tag)
            {
            case TYVOID:
                if (source->Tag > argument->Tag)
                    JIp = jitSetIpFromOffset(n);
                break;
            
            case TYERROR:
            case TYSPECIALFORM:
                if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
                    JIp = jitSetIpFromOffset(n);
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
						if (i > k) {JIp = jitSetIpFromOffset(n); break;}
						if (--m > 0) goto VMJMPGTBYTVEC;
						if (ByteVector(*source)->itsMaxItemIndex > ByteVector(*argument)->itsMaxItemIndex) 
							{JIp = jitSetIpFromOffset(n); break;}
						if (ByteVector(*source)->itsMaxItemIndex < ByteVector(*argument)->itsMaxItemIndex) goto VMJMPGTBYTVEC_CONTINUE;
						if (FPredicate2_QuickCompare(gCP,gTP,&ByteVector(*source)->itsCdr,&ByteVector(*argument)->itsCdr) > 0)
							{JIp = jitSetIpFromOffset(n); break;}
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
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
						if (i > k) {JIp = jitSetIpFromOffset(n); break;}
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
							JIp = jitSetIpFromOffset(n);
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
							JIp = jitSetIpFromOffset(n);
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
					if (i > k) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPGTBITVEC;
					if (BitVector(*source)->itsMaxItemIndex > BitVector(*argument)->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (BitVector(*source)->itsMaxItemIndex < BitVector(*argument)->itsMaxItemIndex) goto VMJMPGTBITVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&BitVector(*source)->itsCdr,&BitVector(*argument)->itsCdr) > 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPGTBITVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
					JIp = jitSetIpFromOffset(n);
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
					if (i > k) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPGTINTVEC;
					if (IntVector(*source)->itsMaxItemIndex > IntVector(*argument)->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (IntVector(*source)->itsMaxItemIndex < IntVector(*argument)->itsMaxItemIndex) goto VMJMPGTINTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&IntVector(*source)->itsCdr,&IntVector(*argument)->itsCdr) > 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPGTINTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
					JIp = jitSetIpFromOffset(n);
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
					if (i > k) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPGTSHTVEC;
					if (source->u.ShortVector->itsMaxItemIndex > argument->u.ShortVector->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (source->u.ShortVector->itsMaxItemIndex < argument->u.ShortVector->itsMaxItemIndex) goto VMJMPGTSHTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&source->u.ShortVector->itsCdr,&argument->u.ShortVector->itsCdr) > 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPGTSHTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYFLTVECTOR:
				if (argument->Tag == TYFLTVECTOR)
					{
					LpFLOAT srcP = FloatArray(*source);
					LpFLOAT argP = FloatArray(*argument);
					i = source->u.FltVector->itsMaxItemIndex;
					k = argument->u.FltVector->itsMaxItemIndex;
					m = (i < k) ? i : k;
					VMJMPGTFLTVEC:
					si = *srcP++;
					ai = *argP++;
					if (si < ai) goto VMJMPGTFLTVEC_CONTINUE;
					if (si > ai) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPGTFLTVEC;
					if (FltVector(*source)->itsMaxItemIndex > FltVector(*argument)->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (FltVector(*source)->itsMaxItemIndex < FltVector(*argument)->itsMaxItemIndex) goto VMJMPGTFLTVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&FltVector(*source)->itsCdr,&FltVector(*argument)->itsCdr) > 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPGTFLTVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
					JIp = jitSetIpFromOffset(n);
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
					if (si > ai) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPGTNUMVEC;
					if (NumVector(*source)->itsMaxItemIndex > NumVector(*argument)->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (NumVector(*source)->itsMaxItemIndex < NumVector(*argument)->itsMaxItemIndex) goto VMJMPGTNUMVEC_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&NumVector(*source)->itsCdr,&NumVector(*argument)->itsCdr) > 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPGTNUMVEC_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) > 0)
					JIp = jitSetIpFromOffset(n);
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
					if (si > ai) {JIp = jitSetIpFromOffset(n); break;}
					if (--m > 0) goto VMJMPGTNUMMAT;
					if (NumMatrix(*source)->itsMaxItemIndex > NumMatrix(*argument)->itsMaxItemIndex)
						{JIp = jitSetIpFromOffset(n); break;}
					if (NumMatrix(*source)->itsMaxItemIndex < NumMatrix(*argument)->itsMaxItemIndex) goto VMJMPGTNUMMAT_CONTINUE;
					if (FPredicate2_QuickCompare(gCP,gTP,&NumMatrix(*source)->itsCdr,&NumMatrix(*argument)->itsCdr) > 0)
						{JIp = jitSetIpFromOffset(n); break;}
					VMJMPGTNUMMAT_CONTINUE:
					break;
					}
				else
				if (FPredicate2_QuickCompare(gCP,gTP,source,argument) < 0)
					JIp = jitSetIpFromOffset(n);
                break;
                                
            case TYUNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.UInt > argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.UInt > argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.UInt > argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.UInt > argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.UInt >  cp->itsReal ||
						   ((REAL)source->u.UInt == cp->itsReal && 0.0 > cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag > argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                                
            case TYNUM:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Int > argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Int > argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Int > argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Int > argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Int >  cp->itsReal ||
						   ((REAL)source->u.Int == cp->itsReal && 0.0 > cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag > argument->Tag)
                            JIp = jitSetIpFromOffset(n);
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
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Real > argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Real > argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Real > argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if (source->u.Real >  cp->itsReal ||
						   (source->u.Real == cp->itsReal && 0.0 > cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

					case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag > argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYCHAR:
                switch (argument->Tag)
                    {
                    case TYUNUM:
                        if (source->u.Char > argument->u.UInt)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYNUM:
                        if (source->u.Char > argument->u.Int)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYDATE:
                    case TYMONEY:
                    case TYREAL:
                        if (source->u.Char > argument->u.Real)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYCHAR:
                        if (source->u.Char > argument->u.Char)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;

					case TYCPX:
						cp = argument->u.Complex;
						if ((REAL)source->u.Char  > cp->itsReal ||
						   ((REAL)source->u.Char == cp->itsReal && 0.0 > cp->itsImag))
                            JIp = jitSetIpFromOffset(n);
						break;

					default:
                        if (source->Tag > argument->Tag)
                            JIp = jitSetIpFromOffset(n);
                        break;
                    }
                break;
                
            case TYBOLE:
                switch (argument->Tag)
                    {
                    case TYBOLE:
                        if (source->u.Bool > argument->u.Bool)
                            JIp = jitSetIpFromOffset(n);
                        break;
                        
                    case TYERROR:
                        retValue = *argument;
                        goto NestedReturn;
                        break;
        
                    default:
                        if (source->Tag > argument->Tag)
                            JIp = jitSetIpFromOffset(n);
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
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYNUM:
                    if (cp->itsReal >  (REAL)argument->u.Int ||
						(cp->itsReal == (REAL)argument->u.Int && cp->itsImag > 0.0))
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYDATE:
                case TYMONEY:
                case TYREAL:
                    if (cp->itsReal >  argument->u.Real ||
					   (cp->itsReal == argument->u.Real && cp->itsImag > 0.0))
                        JIp = jitSetIpFromOffset(n);
                    break;

                case TYCHAR:
                    if (cp->itsReal >  (REAL)argument->u.Char ||
					   (cp->itsReal == (REAL)argument->u.Char && cp->itsImag > 0.0))
						JIp = jitSetIpFromOffset(n);
                    break;

				case TYCPX:
					if (cp->itsReal >  argument->u.Complex->itsReal ||
					   (cp->itsReal == argument->u.Complex->itsReal &&
						cp->itsImag >  argument->u.Complex->itsImag))
                        JIp = jitSetIpFromOffset(n);
					break;

                case TYERROR:
                    retValue = *argument;
                    goto NestedReturn;
                    break;

                default:
                    if (source->Tag != argument->Tag)
                        JIp = jitSetIpFromOffset(n);
                    break;
				}
				break;

			default:
 				retValue = FPredicate2_FullCompare(gCP, gTP, *source, *argument);
				if (retValue.Tag == TYERROR) goto NestedReturn;
                if (isCompareGT(&retValue))
                    JIp = jitSetIpFromOffset(n);
                break;
            }
		jitOfflineExitJmpCC;
        break;

    case  VMJUMP:
		jitSetLabel(LVMJUMP)
		jitOfflineEntry
        /*  Load a pointer to the branch operand */
        /*  Branch to the source operand */
        Ip = jitSetIpFromOffset(*(Ip));
		jitOfflineExit;
        break;

    case  VMMOVE:
		jitSetLabel(LVMMOVE)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        /* Move the source to the target operand */
        *target = *source;
		jitOfflineExit;
        break;

    case  VMJUMP:
		jitSetLabel(LVMJUMP)
		jitOfflineEntry
        /*  Load a pointer to the branch operand */
        /*  Branch to the source operand */
        Ip = setIpFromOffset(*(Ip));
		jitOfflineExit;
        break;

    case  VMMOVE:
		jitSetLabel(LVMMOVE)
		jitOfflineEntry
        /*  Load the address of each of the two operands */
        source    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        target    = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
        /* Move the source to the target operand */
        *target = *source;
		jitOfflineExit;
        break;
        
    case  VMDEBUGGER:
		jitSetLabel(LVMDEBUGGER)
		jitOfflineEntry
		/*  Place the new debugger interrupt code here. */
		/*  Place the new debugger interrupt code here. */
		retValue = FSmartbase_Ref(gCP,gTP,2,self->Interfaces,TOBJ(gCP->TLambda_BreakList));
		if (retValue.Tag != TYDIRECTORY) goto ErrorIllegalInstruction;
		retValue = FSmartbase_Ref(gCP,gTP,2,retValue,TINT(((Ip-pcodes)-1)));
		if (retValue.Tag != TYNUM) goto ErrorIllegalInstruction;
		pcode.Opcode = retValue.u.Int;
		goto DebugResume;
		jitOfflineExit;
        break;
            
	case  VMREFDICKEY:
		jitSetLabel(LVMREFDICKEY)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Object = BondArray((*source))[argument->u.Int].Key;
		target->Tag = (target->u.Object != NIL) ? target->u.Object->itsObjectType : TYVOID;
		jitOfflineExit;
		break;    

	case  VMREFSTRKEY:
		jitSetLabel(LVMREFSTRKEY)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Object = BindArray((*source))[argument->u.Int].Key;
		target->Tag = (target->u.Object != NIL) ? target->u.Object->itsObjectType : TYVOID;
		jitOfflineExit;
		break;    

	case  VMREFDIRKEY:
		jitSetLabel(LVMREFDIRKEY)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = PBindArray((*source))[argument->u.Int].Key;
		jitOfflineExit;
		break;    

	case  VMREFDIRVALUE:
		jitSetLabel(LVMREFDIRVALUE)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = PBindArray((*source))[argument->u.Int].Value;
		jitOfflineExit;
		break;    

	case  VMREFDICVALUE:
		jitSetLabel(LVMREFDICVALUE)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = BondArray((*source))[argument->u.Int].Value;
		jitOfflineExit;
		break;    

	case  VMREFSTRVALUE:
		jitSetLabel(LVMREFSTRVALUE)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = BindArray((*source))[argument->u.Int].Value;
		jitOfflineExit;
		break;    

	case  VMREFPCDVECTOR:
		jitSetLabel(LVMREFPCDVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = PcodeArray((*source))[argument->u.Int];
		target->Tag = TYNUM;
		jitOfflineExit;
		break;    

    case  VMREFNUMMATRIX:
		jitSetLabel(LVMREFNUMMATRIX)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = RealMatrix((*source))[argument->u.Int];
		target->Tag = TYREAL;
		jitOfflineExit;
		break;    

    case  VMREFMATRIX:
		jitSetLabel(LVMREFMATRIX)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = TvalMatrix((*source))[argument->u.Int];
		jitOfflineExit;
		break;

    case  VMREFFLTVECTOR:
		jitSetLabel(LVMREFFLTVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = FloatArray((*source))[argument->u.Int];
		target->Tag = TYREAL;
		jitOfflineExit;
		break;    

    case  VMREFNUMVECTOR:
		jitSetLabel(LVMREFNUMVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Real = RealArray((*source))[argument->u.Int];
		target->Tag = TYREAL;
		jitOfflineExit;
		break;    

    case  VMREFINTVECTOR:
		jitSetLabel(LVMREFINTVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = IntArray((*source))[argument->u.Int];
		target->Tag = TYNUM;
		jitOfflineExit;
		break;    

    case  VMREFOBJVECTOR:
		jitSetLabel(LVMREFOBJVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Object = ObjArray((*source))[argument->u.Int];
		target->Tag = (target->u.Object != NIL) ? target->u.Object->itsObjectType : TYVOID;
		jitOfflineExit;
		break;    

    case  VMREFBYTVECTOR:
		jitSetLabel(LVMREFBYTVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = ByteArray((*source))[argument->u.Int];
		target->Tag = TYNUM;
		jitOfflineExit;
		break;    

    case  VMREFTEXT:
		jitSetLabel(LVMREFTEXT)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = source->u.Text[argument->u.Int];
		target->Tag = TYNUM;
		jitOfflineExit;
		break;

    case  VMREFSYMBOL:
		jitSetLabel(LVMREFSYMBOL)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = SymbolArray((*source))[argument->u.Int];
		target->Tag = TYNUM;
		jitOfflineExit;
		break;

    case  VMREFSTRING:
		jitSetLabel(LVMREFSTRING)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Char = CharArray((*source))[argument->u.Int];
		target->Tag = TYNUM;
		jitOfflineExit;
		break;

    case  VMREFVECTOR:
		jitSetLabel(LVMREFVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		*target = TvalArray((*source))[argument->u.Int];
		jitOfflineExit;
		break;

    case  VMREFBITVECTOR:
		jitSetLabel(LVMREFBITVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        target      = (TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		target->u.Int = ((BitArray((*source))[argument->u.Int/8] & gCP->TBitVector_OrMasks[argument->u.Int%8]) != 0);
		target->Tag = TYNUM;
		jitOfflineExit;
		break;    

	case  VMREF:
		jitSetLabel(LVMREF)
		jitOfflineEntry
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

						case TYLONGPOINTER:
							retValue.u.Int = (NUM)((LpNUM32)isource.u.Int)[index->u.Int];
							retValue.Tag = TYNUM;
							break;

						case TYSHORTPOINTER:
							retValue.u.Int = (NUM)((LpSHORT)isource.u.Int)[index->u.Int];
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
                                ObjIdx(retValue) = isource.u.Brick->itsObjectIndex;
                                RowIdx(retValue) = index->u.Int;
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
									        retValue.u.Bool = *((LpCHAR)(fieldArrayPtr));
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
							gTP->FVmscript_iargument = TStructure_SearchKey(gCP,gTP,(TStructure*)isource.u.Object, index->u.Object, (short*)&gTP->FVmscript_Selection);
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
							gTP->FVmscript_iargument = TDictionary_BSearchKey(gCP,gTP,(TDictionary*)isource.u.Object, index->u.Object, (short*)&gTP->FVmscript_Selection);
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
						retValue = TMatrix_GetIV1(gCP,gTP,isource, *index);
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
		jitOfflineExit;
		break;
        
	case  VMRETURN:
		jitSetLabel(LVMRETURN)
		jitOfflineEntry
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
		jitOfflineExit;
        break;
        
    case  VMSELF:
		jitSetLabel(LVMSELF)
		jitOfflineEntry
        /* Assign the current procedure value to the target argument */
        target   = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)));
        target->Tag = self->itsObjectType;
        target->u.Object = (TObject*)self;
		jitOfflineExit;
        break;

	case  VMSETDICKEY:
		jitSetLabel(LVMSETDICKEY)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (isObject(source))
			BondArray((retValue))[argument->u.Int].Key = source->u.Object;
		else
			BondArray((retValue))[argument->u.Int].Key = NIL;
		jitOfflineExit;
		break;
	
	case  VMSETSTRKEY:
		jitSetLabel(LVMSETSTRKEY)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (isObject(source))
			BindArray((retValue))[argument->u.Int].Key = source->u.Object;
		else
			BindArray((retValue))[argument->u.Int].Key = NIL;
		jitOfflineExit;
		break;
	
	case  VMSETDIRKEY:
		jitSetLabel(LVMSETDIRKEY)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		PBindArray((retValue))[argument->u.Int].Key = *source;
		jitOfflineExit;
		break;
	
	case  VMSETDIRVALUE:
		jitSetLabel(LVMSETDIRVALUE)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		PBindArray((retValue))[argument->u.Int].Value = *source;
		jitOfflineExit;
		break;
	
	case  VMSETDICVALUE:
		jitSetLabel(LVMSETDICVALUE)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		BondArray((retValue))[argument->u.Int].Value = *source;
		jitOfflineExit;
		break;
	
	case  VMSETSTRVALUE:
		jitSetLabel(LVMSETSTRVALUE)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		BindArray((retValue))[argument->u.Int].Value = *source;
		jitOfflineExit;
		break;
	
	case  VMSETBITVECTOR:
		jitSetLabel(LVMSETBITVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (source->u.Int == 1)
			BitArray(retValue)[argument->u.Int/8] |= gCP->TBitVector_OrMasks[(argument->u.Int%8)];
		else
			BitArray(retValue)[argument->u.Int/8] &= gCP->TBitVector_AndMasks[(argument->u.Int%8)];
		jitOfflineExit;
		break;
	
	case  VMSETSTRING:
		jitSetLabel(LVMSETSTRING)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		CharArray((retValue))[argument->u.Int] = source->u.Char;
		jitOfflineExit;
		break;
	
	case  VMSETBYTVECTOR:
		jitSetLabel(LVMSETBYTVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		ByteArray((retValue))[argument->u.Int] = source->u.Char;
		jitOfflineExit;
		break;
	
	case  VMSETOBJVECTOR:
		jitSetLabel(LVMSETOBJVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		if (isObject(&retValue))
			ObjArray((retValue))[argument->u.Int] = source->u.Object;
		else
			ObjArray((retValue))[argument->u.Int] = NIL;
		jitOfflineExit;
		break;
	
	case  VMSETPCDVECTOR:
		jitSetLabel(LVMSETPCDVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		PcodeArray((retValue))[argument->u.Int] = source->u.Int;
		jitOfflineExit;
		break;
	
	case  VMSETINTVECTOR:
		jitSetLabel(LVMSETINTVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		IntArray((retValue))[argument->u.Int] = source->u.Int;
		jitOfflineExit;
		break;
	
	case  VMSETFLTVECTOR:
		jitSetLabel(LVMSETFLTVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		FloatArray((retValue))[argument->u.Int] = source->u.Real;
		jitOfflineExit;
		break;
	
	case  VMSETNUMMATRIX:
		jitSetLabel(LVMSETNUMMATRIX)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		RealMatrix((retValue))[argument->u.Int] = source->u.Real;
		jitOfflineExit;
		break;
	
	case  VMSETNUMVECTOR:
		jitSetLabel(LVMSETNUMVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		RealArray((retValue))[argument->u.Int] = source->u.Real;
		jitOfflineExit;
		break;
	
	case  VMSETMATRIX:
		jitSetLabel(LVMSETMATRIX)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		TvalMatrix((retValue))[argument->u.Int] = *source;
		jitOfflineExit;
		break;
	
	case  VMSETVECTOR:
		jitSetLabel(LVMSETVECTOR)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
        retValue    = *(TVAL*)(Rp[pcode.u.Am3].u.Int + *(Ip+2));Ip+=3;
		TvalArray((retValue))[argument->u.Int] = *source;
		jitOfflineExit;
		break;
	
	case  VMSET:
		jitSetLabel(LVMSET)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
        index	    = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        source      = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));
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

							case TYLONGPOINTER:
								((LpNUM32)target->u.Int)[index->u.Int] = (NUM32)source->u.Int;
								break;

							case TYSHORTPOINTER:
								((LpSHORT)target->u.Int)[index->u.Int] = (SHORT)source->u.Int;
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
				
				jitOfflineExit;
				break;

				case TYBYTEVECTOR:
					if (index->u.Int < target->u.ByteVector->itsMaxItemIndex)
						{
						if ((source->Tag == TYCHAR) || (source->Tag == TYNUM))
							{
							ByteArray(*target)[index->u.Int] = (char)source->u.Int;
							jitOfflineExit;
							}
						}
					break;

				case TYFLTVECTOR:
					if (index->u.Int < target->u.FltVector->itsMaxItemIndex)
						{						
						if (source->Tag == TYREAL)
							{
							FloatArray(*target)[index->u.Int] = (float)source->u.Real;
							jitOfflineExit;
							}
						else
						if (source->Tag == TYNUM)
							{
							FloatArray(*target)[index->u.Int] = (float)source->u.Int;
							jitOfflineExit;
							}
						}
					break;

				case TYINTVECTOR:
					if (index->u.Int < target->u.IntVector->itsMaxItemIndex)
						{						
						if (source->Tag == TYNUM)
							{
							IntArray(*target)[index->u.Int] = source->u.Int;
							jitOfflineExit;
							}
						else
						if (source->Tag == TYREAL)
							{
							IntArray(*target)[index->u.Int] = (NUM)source->u.Real;
							jitOfflineExit;
							}
						}
					break;

				case TYSHORTVECTOR:
					if (index->u.Int < target->u.ShortVector->itsMaxItemIndex)
						{
						if (source->Tag == TYNUM)
							{
							ShortArray(*target)[index->u.Int] = (SHORT)source->u.Int;
							jitOfflineExit;
							}
						else
						if (source->Tag == TYREAL)
							{
							ShortArray(*target)[index->u.Int] = (SHORT)source->u.Real;
							jitOfflineExit;
							}
						}
					break;

				case TYLONGVECTOR:
					if (index->u.Int < target->u.LongVector->itsMaxItemIndex)
						{
						if (source->Tag == TYNUM)
							{
							LongArray(*target)[index->u.Int] = (NUM32)source->u.Int;
							jitOfflineExit;
							}
						else
						if (source->Tag == TYREAL)
							{
							LongArray(*target)[index->u.Int] = (NUM32)source->u.Real;
							jitOfflineExit;
							}
						}
					break;

				case TYNUMVECTOR:
					if (index->u.Int < target->u.NumVector->itsMaxItemIndex)
						{
						if (source->Tag == TYREAL)
							{
							RealArray(*target)[index->u.Int] = source->u.Real;
							jitOfflineExit;
							}
						else
						if (source->Tag == TYNUM)
							{
							RealArray(*target)[index->u.Int] = (REAL)source->u.Int;
							jitOfflineExit;
							}
						}
					break;

				case TYSTRING:
					if (index->u.Int < (target->u.String->itsMaxItemIndex - 1))
						{
						if ((source->Tag == TYCHAR) || (source->Tag == TYNUM))
							{
							CharArray(*target)[index->u.Int] = (char)source->u.Int;
							jitOfflineExit;
							}
						}
					break;

				case TYSTRUCTURE:
					if (index->u.Int < target->u.Vector->itsMaxItemIndex)
						{
						BindArray(*target)[index->u.Int].Value = *source;
						jitOfflineExit;
						}
					break;

				case TYVECTOR:
					if (index->u.Int < target->u.Vector->itsMaxItemIndex)
						{
						TvalArray(*target)[index->u.Int] = *source;
						jitOfflineExit;
						}
					break;
				}
			}
		retValue = (*(LpF3TVALS)_TObject_TypeSetIV1(target->Tag))(gCP,gTP,*target,*index,*source);
 		if (retValue.Tag == TYERROR) goto NestedReturn;
		jitOfflineExit;
		break;
       
	case  VMTESTESCAPE:
		jitSetLabel(LVMTESTESCAPE)
		jitOfflineEntry
        /*  Load the address of each of the three operands */
		if ((*gCP->_Host_Escape)((POINTER)gCP,gTP)) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);
		jitOfflineExit;
		break;
	
    case vmregAddImmediate:
		jitSetLabel(LvmregAddImmediate)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) += *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregAbsNumber:
		jitSetLabel(LvmregAbsNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = fabs((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))));
		jitOfflineExit;
		break;
  
    case vmregInteger:
		jitSetLabel(LvmregInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregNumber:
		jitSetLabel(LvmregNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregAddInteger:
		jitSetLabel(LvmregAddInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) += (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregAddNumber:
		jitSetLabel(LvmregAddNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) += (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregAddPointer:
		jitSetLabel(LvmregAddPointer)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) += ((*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))<<*(Ip++));
		jitOfflineExit;
		break;
 
    case vmregAndImmediate:
		jitSetLabel(LvmregAndImmediate)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) &= *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregAndInteger:
		jitSetLabel(LvmregAndInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) &= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregCosNumber:
		jitSetLabel(LvmregCosNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = cos((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))));
		jitOfflineExit;
		break;
 
    case vmregDivImmediate:
		jitSetLabel(LvmregDivImmediate)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) /= *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregDivInteger:
		jitSetLabel(LvmregDivInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) /= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregDivNumber:
		jitSetLabel(LvmregDivNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) /= (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregDivrImmediate:
		jitSetLabel(LvmregDivrImmediate)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) %= *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregDivrInteger:
		jitSetLabel(LvmregDivrInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) %= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregDivrNumber:
		jitSetLabel(LvmregDivrNumber)
		jitOfflineEntry
		source = (LpTVAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD));
		target = (LpTVAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD));
        target->u.Real = modf((target->u.Real / source->u.Real), &gTP->FVmscript_Integer);
        target->u.Real = fabs(target->u.Real) * source->u.Real;
		jitOfflineExit;
		break;
 
    case vmregIncPointer:
		jitSetLabel(LvmregIncPointer)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) += (*(Ip+1) << *(Ip));Ip+=2;
		jitOfflineExit;
		break;
 
    case vmregJmpEQImmediate:
		jitSetLabel(LvmregJmpEQImmediate)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) == m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpLTImmediate:
		jitSetLabel(LvmregJmpLTImmediate)
		jitOfflineEntry
		m=*(Ip++);n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) < m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpGTImmediate:
		jitSetLabel(LvmregJmpGTImmediate)
		jitOfflineEntry
		m=*(Ip++);n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) > m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpNEImmediate:
		jitSetLabel(LvmregJmpNEImmediate)
		jitOfflineEntry
		m=*(Ip++);n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) != m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpGEImmediate:
		jitSetLabel(LvmregJmpGEImmediate)
		jitOfflineEntry
		m=*(Ip++);n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) >= m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpLEImmediate:
		jitSetLabel(LvmregJmpLEImmediate)
		jitOfflineEntry
		m=*(Ip++);n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) <= m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpEQUImmediate:
		jitSetLabel(LvmregJmpEQUImmediate)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) == (UNUM)m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpLTUImmediate:
		jitSetLabel(LvmregJmpLTUImmediate)
		jitOfflineEntry
		m=*(Ip++);n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) < (UNUM)m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpGTUImmediate:
		jitSetLabel(LvmregJmpGTUImmediate)
		jitOfflineEntry
		m=*(Ip++);n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) > (UNUM)m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpNEUImmediate:
		jitSetLabel(LvmregJmpNEUImmediate)
		jitOfflineEntry
		m=*(Ip++);n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) != (UNUM)m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpGEUImmediate:
		jitSetLabel(LvmregJmpGEUImmediate)
		jitOfflineEntry
		m=*(Ip++);n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) >= (UNUM)m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpLEUImmediate:
		jitSetLabel(LvmregJmpLEUImmediate)
		jitOfflineEntry
		m=*(Ip++);n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) <= (UNUM)m) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break; 

	case vmregJmpEQInteger:
		jitSetLabel(LvmregJmpEQInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) == (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpLTInteger:
		jitSetLabel(LvmregJmpLTInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) < (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpGTInteger:
		jitSetLabel(LvmregJmpGTInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) > (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpNEInteger:
		jitSetLabel(LvmregJmpNEInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) != (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpGEInteger:
		jitSetLabel(LvmregJmpGEInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) >= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpLEInteger:
		jitSetLabel(LvmregJmpLEInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) <= (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpEQUInteger:
		jitSetLabel(LvmregJmpEQUInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) == (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpLTUInteger:
		jitSetLabel(LvmregJmpLTUInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) < (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpGTUInteger:
		jitSetLabel(LvmregJmpGTUInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) > (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpNEUInteger:
		jitSetLabel(LvmregJmpNEUInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) != (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpGEUInteger:
		jitSetLabel(LvmregJmpGEUInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) >= (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpLEUInteger:
		jitSetLabel(LvmregJmpLEUInteger)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) <= (*((LpUNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpEQNumber:
		jitSetLabel(LvmregJmpEQNumber)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) == (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpLTNumber:
		jitSetLabel(LvmregJmpLTNumber)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) < (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpGTNumber:
		jitSetLabel(LvmregJmpGTNumber)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) > (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpNENumber:
		jitSetLabel(LvmregJmpNENumber)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) != (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpGENumber:
		jitSetLabel(LvmregJmpGENumber)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) >= (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJmpLENumber:
		jitSetLabel(LvmregJmpLENumber)
		jitOfflineEntry
		n=*(Ip++);if ((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) <= (*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))) Ip = setIpFromOffset(n);
		jitOfflineExit;
		break;
 
    case vmregJump:
		jitSetLabel(LvmregJump)
		jitOfflineEntry
		Ip = (LpNUM)(*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregLoadAddress:
		jitSetLabel(LvmregLoadAddress)
		jitOfflineEntry
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = (NUM)(Rp[pcode.u.Am1].u.Int + *(Ip++));
		jitOfflineExit;
		break;
 
    case vmregLoadInteger:
		jitSetLabel(LvmregLoadInteger)
		jitOfflineEntry
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)))->u.Int;
		jitOfflineExit;
		break;
 
    case vmregLoadTail:
		jitSetLabel(LvmregLoadTail)
		jitOfflineEntry
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = asTail(((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++))));
		jitOfflineExit;
		break;
 
    case vmregLoadDeclType:
		jitSetLabel(LvmregLoadDeclType)
		jitOfflineEntry
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)))->DeclaredType;
		jitOfflineExit;
		break;
 
    case vmregLoadType:
		jitSetLabel(LvmregLoadType)
		jitOfflineEntry
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)))->Tag;
		jitOfflineExit;
		break;
 
    case vmregLoadJmpPointer:
		jitSetLabel(LvmregLoadJmpPointer)
		jitOfflineEntry
		*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = (NUM)setIpFromOffset(*(Ip++));
		jitOfflineExit;
		break;
 
    case vmregLoadNumber:
		jitSetLabel(LvmregLoadNumber)
		jitOfflineEntry
		*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = ((TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++)))->u.Real;
		jitOfflineExit;
		break;
 
    case vmregLogNumber:
		jitSetLabel(LvmregLogNumber)
		jitOfflineEntry
		*((LpREAL)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))) = log(*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))/log(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregMoveImmediate:
		jitSetLabel(LvmregMoveImmediate)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregMoveInteger:
		jitSetLabel(LvmregMoveInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregMoveNumber:
		jitSetLabel(LvmregMoveNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregMulImmediate:
		jitSetLabel(LvmregMulImmediate)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) *= *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregMulInteger:
		jitSetLabel(LvmregMulInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) *= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregMulNumber:
		jitSetLabel(LvmregMulNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) *= (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregObjPointer:
		jitSetLabel(LvmregObjPointer)
		jitOfflineEntry
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++));
        target      = (TVAL*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD));
		target->u.Int = ((argument->Tag < TYTEXT) ? NIL : ((argument->Tag == TYTEXT) ? (NUM)&argument->u.Text[0] : (NUM)*argument->u.Lambda->itsNilArray));
		jitOfflineExit;
		break;
 
    case vmregObjLength:
		jitSetLabel(LvmregObjLength)
		jitOfflineEntry
        argument	= (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip++));
        target      = (TVAL*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD));
		target->u.Int = ((argument->Tag < TYTEXT) ? 0 : ((argument->Tag == TYTEXT) ? (NUM)strlen(&argument->u.Text[0]) : (NUM)argument->u.Lambda->itsMaxItemIndex));
		jitOfflineExit;
		break;
 
    case vmregOrImmediate:
		jitSetLabel(LvmregOrImmediate)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) |= *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregOrInteger:
		jitSetLabel(LvmregOrInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) |= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregPwrNumber:
		jitSetLabel(LvmregPwrNumber)
		jitOfflineEntry
		*((LpREAL)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))) = pow(*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))),*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregRefCharacter:
		jitSetLabel(LvmregRefCharacter)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpCHAR*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregRefFloat:
		jitSetLabel(LvmregRefFloat)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpFLOAT*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregRefInteger:
		jitSetLabel(LvmregRefInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpNUM*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregRefLong:
		jitSetLabel(LvmregRefLong)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpNUM32*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;

	case vmregRefWord:
		jitSetLabel(LvmregRefWord)
		jitOfflineEntry
        (*((LpTVAL)(Rp[pcode.u.Am2].u.Int + *(Ip++)))) = *(*((LpTVAL*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregRefNumber:
		jitSetLabel(LvmregRefNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpREAL*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregRefShort:
		jitSetLabel(LvmregRefShort)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = *(*((LpSHORT*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregRefXCharacter:
		jitSetLabel(LvmregRefXCharacter)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpCHAR*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		jitOfflineExit;
		break;
 
    case vmregRefXFloat:
		jitSetLabel(LvmregRefXFloat)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpFLOAT*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		jitOfflineExit;
		break;
 
    case vmregRefXInteger:
		jitSetLabel(LvmregRefXInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpNUM*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		jitOfflineExit;
		break;
 
    case vmregRefXLong:
		jitSetLabel(LvmregRefXLong)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpNUM32*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		jitOfflineExit;
		break;
 
    case vmregRefXNumber:
		jitSetLabel(LvmregRefXNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpREAL*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		jitOfflineExit;
		break;
 
    case vmregRefXShort:
		jitSetLabel(LvmregRefXShort)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) = (*((LpSHORT*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		jitOfflineExit;
		break;

	case vmregRefXWord:
		jitSetLabel(LvmregRefXWord)
		jitOfflineEntry
        (*((LpTVAL)(Rp[pcode.u.Am3].u.Int + *(Ip++)))) = (*((LpTVAL*)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))];
		jitOfflineExit;
		break;
 
    case vmregRunInHardware:
		jitSetLabel(LvmregRunInHardware)		
		jitOfflineEntry
		m = *(Ip++); /* Load the command argument */
		jitOfflineExit;
		break;

    case vmregSaveInteger:
		jitSetLabel(LvmregSaveInteger)
		jitOfflineEntry
		(target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))->u.Int = *((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)));
		target->Tag = TYNUM;
		jitOfflineExit;
		break;

    case vmregSaveUInteger:
		jitSetLabel(LvmregSaveUInteger)
		jitOfflineEntry
		(target = (TVAL*)(Rp[pcode.u.Am2].u.UInt + *(Ip++)))->u.UInt = *((LpUNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)));
		target->Tag = TYUNUM;
		jitOfflineExit;
		break;
 
    case vmregSaveTail:
		jitSetLabel(LvmregSaveTail)
		jitOfflineEntry
		asTail((target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))) = *((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)));
		jitOfflineExit;
		break;
 
	case vmregSaveTailImmediate:
		jitSetLabel(LvmregSaveTailImmediate)
		jitOfflineEntry
		n = *(Ip++);asTail((target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))) = n;
		jitOfflineExit;
		break;
 
    case vmregSaveDeclType:
		jitSetLabel(LvmregSaveDeclType)
		jitOfflineEntry
		(target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))->DeclaredType = (CHAR)*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)));
		jitOfflineExit;
		break;
 
    case vmregSaveDeclTypeImmediate:
		jitSetLabel(LvmregSaveDeclTypeImmediate)
		jitOfflineEntry
		n = *(Ip++);(target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))->DeclaredType = (CHAR)n;
		jitOfflineExit;
		break;
 
    case vmregSaveNumber:
		jitSetLabel(LvmregSaveNumber)
		jitOfflineEntry
		(target = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip++)))->u.Real = *((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)));
		target->Tag = TYREAL;
		jitOfflineExit;
		break;
 
    case vmregSetCharImmediate:
		jitSetLabel(LvmregSetCharImmediate)
		jitOfflineEntry
		*(*((LpCHAR*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (CHAR)(*(Ip++));
		jitOfflineExit;
		break;
 
    case vmregSetCharacter:
		jitSetLabel(LvmregSetCharacter)
		jitOfflineEntry
		*(*((LpCHAR*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSetFloat:
		jitSetLabel(LvmregSetFloat)
		jitOfflineEntry
		*(*((LpFLOAT*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSetIntImmediate:
		jitSetLabel(LvmregSetIntImmediate)
		jitOfflineEntry
		*(*((LpNUM*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (NUM)(*(Ip++));
		jitOfflineExit;
		break;
 
    case vmregSetInteger:
		jitSetLabel(LvmregSetInteger)
		jitOfflineEntry
		*(*((LpNUM*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSetLongImmediate:
		jitSetLabel(LvmregSetLongImmediate)
		jitOfflineEntry
		*(*((LpNUM32*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (NUM32)(*(Ip++));
		jitOfflineExit;
		break;
 
    case vmregSetLong:
		jitSetLabel(LvmregSetLong)
		jitOfflineEntry
		*(*((LpNUM32*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSetNumber:
		jitSetLabel(LvmregSetNumber)
		jitOfflineEntry
		*(*((LpREAL*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSetShortImmediate:
		jitSetLabel(LvmregSetShortImmediate)
		jitOfflineEntry
		*(*((LpSHORT*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (SHORT)(*(Ip++));
		jitOfflineExit;
		break;
 
    case vmregSetShort:
		jitSetLabel(LvmregSetShort)
		jitOfflineEntry
		*(*((LpSHORT*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;

	case vmregSetWord:
		jitSetLabel(LvmregSetWord)
		jitOfflineEntry
        *(*(LpTVAL*)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))) = *((LpTVAL)(Rp[pcode.u.Am1].u.Int + *(Ip++)));
		jitOfflineExit;
		break;

    case vmregSetXCharImmediate:
		jitSetLabel(LvmregSetXCharImmediate)
		jitOfflineEntry
		(*((LpCHAR*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (CHAR)(*(Ip++));
		jitOfflineExit;
		break;
 
    case vmregSetXCharacter:
		jitSetLabel(LvmregSetXCharacter)
		jitOfflineEntry
		(*((LpCHAR*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSetXFloat:
		jitSetLabel(LvmregSetXFloat)
		jitOfflineEntry
		(*((LpFLOAT*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSetXIntImmediate:
		jitSetLabel(LvmregSetXIntImmediate)
		jitOfflineEntry
		(*((LpNUM*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregSetXLongImmediate:
		jitSetLabel(LvmregSetXLongImmediate)
		jitOfflineEntry
		(*((LpNUM32*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (NUM32)*(Ip++);
		jitOfflineExit;
		break;
 
    case vmregSetXInteger:
		jitSetLabel(LvmregSetXInteger)
		jitOfflineEntry
		(*((LpNUM*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSetXLong:
		jitSetLabel(LvmregSetXLong)
		jitOfflineEntry
		(*((LpNUM32*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSetXNumber:
		jitSetLabel(LvmregSetXNumber)
		jitOfflineEntry
		(*((LpREAL*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSetXShortImmediate:
		jitSetLabel(LvmregSetXShortImmediate)
		jitOfflineEntry
		(*((LpSHORT*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (SHORT)(*(Ip++));
		jitOfflineExit;
		break;

	case vmregSetXWord:
		jitSetLabel(LvmregSetXWord)
		jitOfflineEntry
        (*((LpTVAL*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpTVAL)(Rp[pcode.u.Am1].u.Int + *(Ip++))));
		jitOfflineExit;
		break;
 
    case vmregSetXShort:
		jitSetLabel(LvmregSetXShort)
		jitOfflineEntry
		(*((LpSHORT*)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))))[(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))] = (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregShlImmediate:
		jitSetLabel(LvmregShlImmediate)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) << *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregShlInteger:
		jitSetLabel(LvmregShlInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) << (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregShrImmediate:
		jitSetLabel(LvmregShrImmediate)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) >> *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregShrInteger:
		jitSetLabel(LvmregShrInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) >> (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSinNumber:
		jitSetLabel(LvmregSinNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = sin((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))));
		jitOfflineExit;
		break;
 
    case vmregSqrtNumber:
		jitSetLabel(LvmregSqrtNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = sqrt((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))));
		jitOfflineExit;
		break;
 
    case vmregStringCompare:
		jitSetLabel(LvmregStringCompare)
		jitOfflineEntry
        /*  Load the address of each of the two regoffset operands */
        source   = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
		srcP = (source->Tag == TYTEXT) ? source->u.Text : CharArray(*source);
		argP = (argument->Tag == TYTEXT) ? argument->u.Text : CharArray(*argument);
		*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))) = (NUM)strcmp(srcP,argP);
		jitOfflineExit;
		break;
 
    case vmregStringiCompare:
		jitSetLabel(LvmregStringiCompare)
		jitOfflineEntry
        /*  Load the address of each of the two regoffset operands */
        source   = (TVAL*)(Rp[pcode.u.Am1].u.Int + *(Ip));
        argument = (TVAL*)(Rp[pcode.u.Am2].u.Int + *(Ip+1));Ip+=2;
		srcP = (source->Tag == TYTEXT) ? source->u.Text : CharArray(*source);
		argP = (argument->Tag == TYTEXT) ? argument->u.Text : CharArray(*argument);
		// Modified code, replace call to stricmp to own code since stricmp may not be in run-time library
		// *((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))) = (NUM)stricmp(srcP,argP);
        for (; tolower(*srcP) == tolower(*argP); srcP++, argP++) //include ctype.h to get macro version.
        {       if (*srcP == '\0')
                {       *((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))) = 0;
                        jitOfflineExit;
                        break;
                }
        }
        *((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD))) = tolower(*srcP) - tolower(*argP);
		jitOfflineExit;
		break;
 
    case vmregSubImmediate:
		jitSetLabel(LvmregSubImmediate)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) -= *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregSubInteger:
		jitSetLabel(LvmregSubInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) -= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSubNumber:
		jitSetLabel(LvmregSubNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) -= (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmregSubPointer:
		jitSetLabel(LvmregSubPointer)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am3<<BITSIZEOFAISWORD)))) -= ((*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))))<<*(Ip++));
		jitOfflineExit;
		break;
 
    case vmregTanNumber:
		jitSetLabel(LvmregTanNumber)
		jitOfflineEntry
		(*((LpREAL)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) = tan((*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))));
		jitOfflineExit;
		break;
 
    case vmregXorImmediate:
		jitSetLabel(LvmregXorImmediate)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) ^= *(Ip++);
		jitOfflineExit;
		break;
 
    case vmregXorInteger:
		jitSetLabel(LvmregXorInteger)
		jitOfflineEntry
		(*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD)))) ^= (*((LpNUM)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));
		jitOfflineExit;
		break;
 
    case vmvecBinary:
		jitSetLabel(LvmvecBinary)		
		jitOfflineEntry
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
		jitOfflineExit;
		break;

    case vmvecInitialize:
		jitSetLabel(LvmvecInitialize)
		jitOfflineEntry
		switch ((vecInitializeExtent = *(Ip++)))
			{
			case 0: /* argument */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				vecInitializeCount = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				vecInitializeLabel = (NUM)Ip;
				break;

			case 1: /* source */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				vecInitializeCount = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				vecInitializeLabel = (NUM)Ip;
				break;

			case 2: /* target */
				argP	= (LpCHAR)(*((LpNUM)(REGP+(argPtrID<<BITSIZEOFAISWORD))));
				srcP	= (LpCHAR)(*((LpNUM)(REGP+(srcPtrID<<BITSIZEOFAISWORD))));
				tarP	= (LpCHAR)(*((LpNUM)(REGP+(tarPtrID<<BITSIZEOFAISWORD))));
				argInc  = (*((LpNUM)(REGP+(argIncID<<BITSIZEOFAISWORD))));
				srcInc  = (*((LpNUM)(REGP+(srcIncID<<BITSIZEOFAISWORD))));
				tarInc  = (*((LpNUM)(REGP+(tarIncID<<BITSIZEOFAISWORD))));
				vecInitializeCount = (*((LpNUM)(REGP+(pcode.u.Am2<<BITSIZEOFAISWORD))));
				vecInitializeLabel = (NUM)Ip;
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		jitOfflineExit;
		break;

    case vmvecLoop:
		jitSetLabel(LvmvecLoop)
		jitOfflineEntry
		switch (vecInitializeExtent)
			{
			case 0: /* argument */
				argP += argInc;
				if ((--vecInitializeCount) != 0) Ip = (NUM*)vecInitializeLabel;
				break;

			case 1: /* source */
				argP += argInc;
				srcP += srcInc;
				if ((--vecInitializeCount) != 0) Ip = (NUM*)vecInitializeLabel;
				break;

			case 2: /* target */
				argP += argInc;
				srcP += srcInc;
				tarP += tarInc;
				if ((--vecInitializeCount) != 0) Ip = (NUM*)vecInitializeLabel;
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		jitOfflineExit;
		break;

    case vmvecPop:
		jitSetLabel(LvmvecPop)		
		jitOfflineEntry;
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
		jitOfflineExit;
		break;

    case vmvecPopNumber:
		jitSetLabel(LvmvecPopNumber)
		jitOfflineEntry;
		(*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD)))) = Vs[VsTopOfStack--];;		
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		jitOfflineExit;
		break;

	case vmvecPush:
		jitSetLabel(LvmvecPush)	
		jitOfflineEntry;
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
				jitOfflineExit;
				break;

			case 5: /* one */
				Vs[++VsTopOfStack] = 1.0;
				jitOfflineExit;
				break;

			case 6: /* zero */
				Vs[++VsTopOfStack] = 0.0;
				jitOfflineExit;
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
		jitOfflineExit;
		break;

    case vmvecPushNumber:
		jitSetLabel(LvmvecPushNumber)
		jitOfflineEntry;
		Vs[++VsTopOfStack] = (*((LpREAL)(REGP+(pcode.u.Am1<<BITSIZEOFAISWORD))));		
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		jitOfflineExit;
		break;


    case vmvecNumScalar:
		jitSetLabel(LvmvecNumScalar)
		jitOfflineEntry
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
		jitOfflineExit;
		break;
 
    case vmvecNumVector:
		jitSetLabel(LvmvecNumVector)
		jitOfflineEntry
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
		jitOfflineExit;
		break;
 
    case vmvecSetIncrements:
		jitSetLabel(LvmvecSetIncrements)
		jitOfflineEntry
		argIncID = pcode.u.Am1;
		srcIncID = pcode.u.Am2;
		tarIncID = pcode.u.Am3;
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		jitOfflineExit;
		break;
 
    case vmvecSetPointers:
		jitSetLabel(LvmvecSetPointers)
		jitOfflineEntry
		argPtrID = pcode.u.Am1;
		srcPtrID = pcode.u.Am2;
		tarPtrID = pcode.u.Am3;
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		jitOfflineExit;
		break;
 
    case vmvecSwapCC:
		jitSetLabel(LvmvecSwapCC)		
		jitOfflineEntry;
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
		jitOfflineExit;
		break;

    case vmvecUnary:
		jitSetLabel(LvmvecUnary)		
		jitOfflineEntry;
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
				Vs[VsTopOfStack] = tan(Vs[VsTopOfStack]);
				break;

			default:
				goto ErrorIllegalInstruction;
				break;
			}
		if ((VsTopOfStack < 0) || (VsTopOfStack >= MAXVECTORCNT)) goto ErrorIllegalInstruction;
		jitOfflineExit;
		break;

	default:
		jitOfflineEntry
      //IllegalInstruction:
        retValue = gCP->FVmScript_ERROR_ILLEGAL_INSTRUCTION;
        goto NestedReturn;

      //IllegalValue:
        retValue = gCP->FVmScript_ERROR_ILLEGAL_VALUE;
        goto NestedReturn;

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
            
				//NestedReturnDebug:
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
				gTP->FVmscript_NestedDebugSwt = TRUE;
				if ((gTP->DebugFlags != 0) && (gTP->DebugSuspended == 0))
					goto NestedDebugBreak;
				}
            
			NestedDebugResume:
			gTP->FVmscript_NestedDebugSwt = FALSE;
			}

		/*  We are returning from a single Procedure object */
		ThrowOnEscape;
		TopOfStack = saveSi;
		EndRecursion;
		if (TESTON)
			{
			FMemory_SystemDiagnostic(gCP,gTP,1,&gCP->Tval_FALSE);
			}
		return(retValue);
		break;

    }
jitOfflineExit;

/*  =============================================================== */
/*  Debug nested break routine									    */
/*  =============================================================== */
NestedDebugBreak:
/*  Resume normal processing. */
if (gTP->FVmscript_NestedDebugSwt == TRUE) 
	goto NestedDebugResume;
else
	goto DebugResume;

ErrorTooManyRegisters:
return(TERROR("!JIT: found Lambda with too many register variables!"));

ErrorMissingPcodes:
return(gCP->FVmScript_ERROR_MISSING_PCODES);

ErrorIllegalInstruction:
return(gCP->FVmScript_ERROR_ILLEGAL_INSTRUCTION);

ErrorNotAnLambda:
return(gCP->FVmScript_ERROR_NOT_AN_Lambda);

ErrorInvalidArglist:
return(gCP->TObject_ERROR_INVALID_ARGLIST);

ErrorRunInHardwareIllegalInstruction:
if (autoHardwareMode == TRUE)
	{
	autoHardwareMode = FALSE;
	goto FirstPass;
	}
else
if (jitHardwareMode == TRUE)
	{
	jitHardwareMode = FALSE;
	goto FirstPass;
	}
strcpy(gTP->TempBuffer,"!JIT: found vm instruction which is invalid in hardware mode [");
n = strlen(gTP->TempBuffer);
FDebug_INSToString(gCP,gTP,pcode.u.Pcode,&gTP->TempBuffer[n]);
strcat(gTP->TempBuffer,"]!");
return(TERROR(gTP->TempBuffer));
}


/*--------------------------------------------------------------------------------------- */
#if 0
RegisterCnt

This function computes a reference count for all register variables referenced between a
(vmregRunInHardware start:) and a (vmregRunInHardware stop:) instruction.

#endif

void    FVmIntelP3Jit_RegisterCnt(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM vecIndex,TVAL Rp[],NUM Ir[],NUM maxIr,NUM Nr[],NUM maxNr)
{
NUM					command;
NUM					m;
NUM					n;
NUM					N;
NUM					regID;
NUM					Nrc[MAXREGALLOCATION];
NUM					Irc[MAXREGALLOCATION];
OPCODE              ExtendedOpcode;
UNUM				opcode;     
UNUM                modifier[3];
NUM                 modIndex;
StartFrame
DeclareOBJ(TPcodeVector,self);
DeclareOBJ(TObject,anObject);
EndFrame

self = selfTval.u.PcodeVector;

/*  Clear the Register Variable Array along with the Integer Register sorted array */
/*  and the Number Register sorted array. The Offset of each word in the Register  */
/*  Varaible Array contains the number of references to that register variable in  */
/*  between the current (vmregRunInHardware start:) and (vmregRunInHardware stop:) */
/*  instructions.                                                                  */

N = MAXREGISTERCNT; for (n = 0; n < N; ++n) {Rp[n].Offset = 0;}
N = MAXREGALLOCATION; for (n = 0; n < N; ++n) {Ir[n] = 0; Irc[n] = 0; Nr[n] = 0; Nrc[n] = 0;}

/*  Examine all following pcodes in the Pcode Vector looking at all the argument   */ 
/*  modifiers and count register variable references. Stop counting when the next  */
/*  (vmregRunInHardware stop:) instruction is encountered.                         */

while (vecIndex < self->itsCurItemIndex)
    {
    /*  Separate the opcode into the six modifiers. */
    ExtendedOpcode.Opcode = atHMInt(self->itsInstructionArray,vecIndex++);
	opcode = ExtendedOpcode.u.Pcode;
    modifier[0] = ExtendedOpcode.u.Am1;
    modifier[1] = ExtendedOpcode.u.Am2;
    modifier[2] = ExtendedOpcode.u.Am3;

	/*  Quit on next (vmregRunInHardware stop:) instruction. */
	if (opcode == vmregRunInHardware)
		{
		command = *(NUM*)&atHMInt(self->itsInstructionArray,vecIndex++);
		if ((command == 1) || (command == 3)) goto StopCounting;
		}
	else
	/*  Memory opcodes: Count all Register Variable References. */
	if (opcode < VMSTARTREGISTERINS)
		{

		/*  Loop through the modifier patterns for arguments which need */
		/*  to be marked. */
		for (modIndex = 0; modIndex < 3; ++modIndex)
			{
			switch (modifier[modIndex])
				{
				case AMVOID:
					break;
                
				case AMGVOFFSET:
				case AMSVOFFSET:
				case AMAVOFFSET:
				case AMTVOFFSET:
				case AMPVOFFSET:
				case AMCVOFFSET:
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
				case AMINTEGER:
					vecIndex++;
					break;
                
				case AMREGISTER:
					regID = *(NUM*)&atHMInt(self->itsInstructionArray,vecIndex++);
					regID = (regID >> BITSIZEOFAISWORD);
					++Rp[regID].Offset;
					break;
                                
				default:
					break;
				}       
			}
		}
	else
	if (opcode >= VMSTARTREGISTERINS)
		{
		switch (opcode)
			{
			/* Register opcodes with no arguments. */
			case vmvecLoop:
				break;

			/* Register opcodes with one register argument. */
			case vmregJump:
			case vmvecPopNumber:
			case vmvecPushNumber:
				++Rp[modifier[0]].Offset;
				break;

			/* Register opcodes with two register arguments. */
			case vmregAbsNumber:
			case vmregAddInteger:
			case vmregInteger:
			case vmregNumber:
			case vmregAndInteger:
			case vmregAddNumber:
			case vmregCosNumber:
			case vmregDivNumber:
			case vmregDivrInteger:
			case vmregDivrNumber:
			case vmregMoveInteger:
			case vmregMoveNumber:
			case vmregMulInteger:
			case vmregMulNumber:
			case vmregOrInteger:
			case vmregSinNumber:
			case vmregSqrtNumber:
			case vmregSubInteger:
			case vmregSubNumber:
			case vmregTanNumber:
			case vmregXorInteger:
			case vmregShlInteger:
			case vmregShrInteger:
				++Rp[modifier[0]].Offset;
				++Rp[modifier[1]].Offset;
				break;

			/* Register opcodes with two register arguments (special cases for the P3 chip). */
			case vmregRefCharacter:
			case vmregRefFloat:
			case vmregRefInteger:
			case vmregRefLong:
			case vmregRefNumber:
			case vmregRefShort:
				Rp[modifier[0]].Offset += 3;
				++Rp[modifier[1]].Offset;
				break;


			/* Register opcodes with two register arguments (special cases for the P3 chip). */
 			case vmregDivInteger:
				++Rp[modifier[0]].Offset;
				Rp[modifier[1]].Offset += 3;
				break;

			/* Register opcodes with two register arguments (special cases for the P3 chip). */
			case vmregSetFloat:
			case vmregSetInteger:
			case vmregSetLong:
			case vmregSetNumber:
			case vmregSetShort:
			case vmregSetCharacter:
				++Rp[modifier[0]].Offset;
				++Rp[modifier[1]].Offset;
				break;

			/* Register opcodes with three register arguments. */
			case vmregLogNumber:
			case vmregPwrNumber:
			case vmvecSetIncrements:
			case vmvecSetPointers:
				++Rp[modifier[0]].Offset;
				++Rp[modifier[1]].Offset;
				++Rp[modifier[2]].Offset;
				break;

			/* Register opcodes with three register arguments (special cases for the P3 chip). */
			case vmregRefXCharacter:
			case vmregRefXFloat:
			case vmregRefXInteger:
			case vmregRefXLong:
			case vmregRefXNumber:
			case vmregRefXShort:
				++Rp[modifier[0]].Offset;
				++Rp[modifier[1]].Offset;
				++Rp[modifier[2]].Offset;
				break;

			/* Register opcodes with three register arguments (special cases for the P3 chip). */
			case vmregSetXFloat:
			case vmregSetXInteger:
			case vmregSetXLong:
			case vmregSetXNumber:
			case vmregSetXShort:
			case vmregSetXCharacter:
				++Rp[modifier[0]].Offset;
				++Rp[modifier[1]].Offset;
				++Rp[modifier[2]].Offset;
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
				++Rp[modifier[0]].Offset;
				++Rp[modifier[1]].Offset;
				if (modifier[2] == AMINTEGER) vecIndex++;
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
				++Rp[modifier[0]].Offset;
				if (modifier[1] == AMINTEGER) vecIndex++;
				if (modifier[2] == AMINTEGER) vecIndex++;
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
			case vmregShlImmediate:
			case vmregShrImmediate:
			case vmregSubImmediate:
			case vmregXorImmediate:
			case vmregSetCharImmediate:
			case vmregSetIntImmediate:
			case vmregSetLongImmediate:
			case vmregSetShortImmediate:
			case vmvecNumVector:
			case vmvecInitialize:
				if (modifier[0] == AMINTEGER) vecIndex++;
				++Rp[modifier[1]].Offset;
				break;

			/* Register opcodes with one immediate and two register argument. */
			case vmregAddPointer:
			case vmregSubPointer:
			case vmregSetXCharImmediate:
			case vmregSetXIntImmediate:
			case vmregSetXLongImmediate:
			case vmregSetXShortImmediate:
			case vmvecNumScalar:
				if (modifier[0] == AMINTEGER) vecIndex++;
				++Rp[modifier[1]].Offset;
				++Rp[modifier[2]].Offset;
				break;

			/* Register opcodes with two immediate and one register argument. */
			case vmregIncPointer:
				if (modifier[0] == AMINTEGER) vecIndex++;
				if (modifier[1] == AMINTEGER) vecIndex++;
				++Rp[modifier[2]].Offset;
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
				switch(modifier[0])
					{
					case AMGVOFFSET:
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
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
					case AMINTEGER:
						++vecIndex;
						break;

					case AMREGISTER:
						regID = *(NUM*)&atHMInt(self->itsInstructionArray,vecIndex++);
						regID = (regID >> BITSIZEOFAISWORD);
						++Rp[regID].Offset;
						break;
					}
				++Rp[modifier[1]].Offset;
				break; 

			/* Register opcodes with two register and one memory argument. */
			case vmregRefXWord:
				++Rp[modifier[0]].Offset;
				++Rp[modifier[1]].Offset;
				switch(modifier[2])
					{
					case AMGVOFFSET:
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
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
					case AMINTEGER:
						++vecIndex;
						break;

					case AMREGISTER:
						regID = *(NUM*)&atHMInt(self->itsInstructionArray,vecIndex++);
						regID = (regID >> BITSIZEOFAISWORD);
						++Rp[regID].Offset;
						break;
					}
				break; 

			/* Register opcodes with one memory and two register argument. */
			case vmregSetXWord:
				switch(modifier[0])
					{
					case AMGVOFFSET:
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
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
					case AMINTEGER:
						++vecIndex;
						break;

					case AMREGISTER:
						regID = *(NUM*)&atHMInt(self->itsInstructionArray,vecIndex++);
						regID = (regID >> BITSIZEOFAISWORD);
						++Rp[regID].Offset;
						break;
					}
				++Rp[modifier[1]].Offset;
				++Rp[modifier[2]].Offset;
				break; 

				/* Mark opcodes with one immediate argument. */
				case vmregRunInHardware:
				case vmvecBinary:
				case vmvecSwapCC:
				case vmvecUnary:
					if (modifier[0] == AMINTEGER) ++vecIndex;
					break;

				/* Mark opcodes with two immediate arguments. */
				case vmvecPop:
				case vmvecPush:
					if (modifier[0] == AMINTEGER) ++vecIndex;
					if (modifier[1] == AMINTEGER) ++vecIndex;
					break;

			/* Register opcodes with one immediate and one memory argument. */
			case vmregSaveTailImmediate:
			case vmregSaveDeclTypeImmediate:
				if (modifier[0] == AMINTEGER) vecIndex++;
				switch(modifier[1])
					{
					case AMGVOFFSET:
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
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
					case AMINTEGER:
						++vecIndex;
						break;

					case AMREGISTER:
						regID = *(NUM*)&atHMInt(self->itsInstructionArray,vecIndex++);
						regID = (regID >> BITSIZEOFAISWORD);
						++Rp[regID].Offset;
						break;
					}
				break;

			/* Register opcodes with one register and one memory argument. */
			case vmregRefWord:
			case vmregSaveInteger:
			case vmregSaveUInteger:
			case vmregSaveTail:
			case vmregSaveDeclType:
			case vmregSaveNumber:
				++Rp[modifier[0]].Offset;
				switch(modifier[1])
					{
					case AMGVOFFSET:
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
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
					case AMINTEGER:
						++vecIndex;
						break;

					case AMREGISTER:
						regID = *(NUM*)&atHMInt(self->itsInstructionArray,vecIndex++);
						regID = (regID >> BITSIZEOFAISWORD);
						++Rp[regID].Offset;
						break;
					}
				break;

			/* Register opcodes with two memory and one register argument. */
			case vmregStringCompare:
			case vmregStringiCompare:
				switch(modifier[0])
					{
					case AMGVOFFSET:
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
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
					case AMINTEGER:
						++vecIndex;
						break;

					case AMREGISTER:
						regID = *(NUM*)&atHMInt(self->itsInstructionArray,vecIndex++);
						regID = (regID >> BITSIZEOFAISWORD);
						++Rp[regID].Offset;
						break;
					}
				switch(modifier[1])
					{
					case AMGVOFFSET:
					case AMSVOFFSET:
					case AMAVOFFSET:
					case AMTVOFFSET:
					case AMPVOFFSET:
					case AMCVOFFSET:
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
					case AMINTEGER:
						++vecIndex;
						break;

					case AMREGISTER:
						regID = *(NUM*)&atHMInt(self->itsInstructionArray,vecIndex++);
						regID = (regID >> BITSIZEOFAISWORD);
						++Rp[regID].Offset;
						break;
					}
				++Rp[modifier[2]].Offset;
				break; 
			}
		
		}
    }


/*  Allocate the Integer and Number Register sorted arrays with the registers with */
/*  the highest reference count first. When complete, the Integer and Number       */
/*  Register sorted arrays should contain the register variable index to which the */
/*  hardware register is allocated; and the Offset of each Register Variable must  */ 
/*  contain the index of the hardware register to which it is allocated.           */
StopCounting:

for (m = 1; m < MAXREGALLOCATION; ++m) 
	{
	for (n = 0; n < MAXREGISTERCNT; ++n) 
		{
		if ((maxNr >= m) && (Rp[n].DeclaredType == TYREAL) && (Nrc[m] < Rp[n].Offset))
			{
			Nr[m] = n;
			Nrc[m] = Rp[n].Offset;
			}
		else
		if ((maxIr >= m) && (Rp[n].DeclaredType != TYVOID) && (Rp[n].DeclaredType != TYREAL) && (Irc[m] < Rp[n].Offset))
			{
			Ir[m] = n;
			Irc[m] = Rp[n].Offset;
			}
		}
	Rp[Ir[m]].Offset = 0;
	Rp[Nr[m]].Offset = 0;
	}

/*  Clear the Register Variable Array along with the Integer Register sorted array */
/*  and the Number Register sorted array. The Offset of each word in the Register  */
/*  Varaible Array contains the number of references to that register variable in  */
/*  between the current (vmregRunInHardware start:) and (vmregRunInHardware stop:) */
/*  instructions.                                                                  */

for (n = 0; n < MAXREGISTERCNT; ++n) {Rp[n].Offset = 0;}
for (m = 0; m < MAXREGALLOCATION; ++m) 
	{
	if (Ir[m] != 0) Rp[Ir[m]].Offset = m; 
	if (Nr[m] != 0) Rp[Nr[m]].Offset = m; 
	}

FrameReturn;
}





#endif
#endif
