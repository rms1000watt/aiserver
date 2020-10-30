;/**********************************************************************************
;    Copyright (C) 2013 Analytic Research Foundation
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;***********************************************************************************/


;; *****************************************************************************
;; 
;; fvmamd64con.asm
;;
;; Implementation of the Virtual Machine Connector for the AMD64 JIT.
;;
;; This source file contains the main connector functions for the Windows AMD64 
;; Just-In-Time-Compiler (JIT). This JIT is designed to work hand-in-glove
;; with the FVmScript_Emulator DRM virtual machine emulator.
;;
;; NOTE: These assembler functions are required because some C compilers do not
;;       support inline assembler. 
;;
;; PARENT:          fvmamd64jit.c 
;; 
;; AUTHORS:         Rajah Alisan
;;
;; *****************************************************************************

.CODE

;; *****************************************************************************
;; Jit_LoadJumpTable
;; *****************************************************************************
PUBLIC Jit_LoadJumpTable
_TEXT	SEGMENT
Jit_LoadJumpTable PROC
;;Note: The last return address is passed as a return value 
pop rax			;; Save the return address
push rax		;; Restore the last return address
add rax,20		;; Increment for the 'Ip= ' assignment and conditional 'goto lbl;' instructions which follows the return address
RET				;; Return the incremented return address
Jit_LoadJumpTable ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_RelativeAddress
;; *****************************************************************************
PUBLIC Jit_RelativeAddress
_TEXT	SEGMENT
Jit_RelativeAddress PROC
;;Note1: The absolute address is passed in rcx
;;Note2: Do NOT use masm argument directives as they will alter the rbp register!!
pop  rdx			;; Save the return address
mov  rax,rcx		;; Save the absolute address argument
sub  rax,rsp		;; Convert the absolute address to a relative address argument
jmp	 rdx			;; Return the relative address argument
;; Instruction code generation example area.
;cmp rdx,0	        ;; *Np++ = 0x48; *Np++ = 0x39; *Np++ = 0xCA;
;je  NextLabel		;; *Np++ = imod[1]; *Np++ = 0x05;
;jmp rdx             ;; *Np++ = imod[1]; *Np++ = 0x05;
;NextLabel:
;movss dword ptr [r9+r10*2],xmm15
;movss dword ptr [rdx+R12*4],xmm15				;; *Np++ = 0xF3; *Np++ = 0x46; *Np++ = 0x0F; *Np++ = 0x11; *Np++ = 0x3C; *Np++ = (char)(0x80+XMMBRDX[Rp[modifier[1]].Offset]);
;mov  rdx,qword ptr [rbp+11223344]				;; *Np++ = 0x48; *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
;cvtsd2ss xmm15,xmm10							;; *Np++ = 0xF2; EXTXMMMOD(Rp[modifier[0]].Offset,16,0); *Np++ = 0x0F; *Np++ = 0x5A; *Np++ = (char)0xF8 + XMMXMMR[Rp[modifier[0]].Offset];
;cvtsd2ss xmm15,qword ptr [rbp+11223344]			;; *Np++ = 0xF2; *Np++ = 0x44; *Np++ = 0x0F; *Np++ = 0x5A; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
;movss dword ptr [r13+rdx*4],xmm15				;; *Np++ = 0xF3; *Np++ = 0x45; *Np++ = 0x0F; *Np++ = 0x11; *Np++ = 0x7C; *Np++ = 0x95; *Np++ = 0x00;
;movss dword ptr [R12+rdx*4],xmm15				;; *Np++ = 0xF3; *Np++ = 0x45; *Np++ = 0x0F; *Np++ = 0x11; *Np++ = 0x3C; *Np++ = (char)(0x90+XMMBASE[Rp[modifier[2]].Offset]);
;mov rdx,qword ptr [rbp+11223344]		;; *Np++ = 0x48; *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
;shl rdx,2								;; *Np++ = 0x48; *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFFLOAT; 	
;add rdx, qword ptr [Rbp+11223344]		;; *Np++ = 0x48; *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFAISWORD));Np+=4;
;cvtsd2ss xmm15,XMM10					;; *Np++ = 0xF2; EXTXMMMOD(Rp[modifier[0]].Offset,16,0); *Np++ = 0x0F; *Np++ = 0x5A; *Np++ = (char)0xF8 + XMMXMMR[Rp[modifier[0]].Offset];
;movss dword ptr [rdx],xmm15				;; *Np++ = 0xF3; *Np++ = 0x44; *Np++ = 0x0F; *Np++ = 0x11; *Np++ = 0x3A;
;; else
;cvtsd2ss XMM10,xmm15					;; *Np++ = 0xF2; *Np++ = 0x44; *Np++ = 0x0F; *Np++ = 0x5A; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
;movss dword ptr [rdx],xmm15				;; *Np++ = 0xF3; *Np++ = 0x44; *Np++ = 0x0F; *Np++ = 0x11; *Np++ = 0x3A;
;; old study
;mov rdx,qword ptr [Rbp+11223344]		;; *Np++ = 0x48; *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFAISWORD));Np+=4;
;cvtsd2ss xmm15,qword ptr [Rbp+11223344] ;; *Np++ = 0xF2; *Np++ = 0x44; *Np++ = 0x0F; *Np++ = 0x5A; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFAISWORD));Np+=4;
;movss dword ptr[rdx],xmm15				;; *Np++ = 0xF3; *Np++ = 0x44; *Np++ = 0x0F; *Np++ = 0x11; *Np++ = 0x3A;
;mov  r9,r13[r8]
;mov   dword ptr [rax+rcx*4],11223344h
;mov   dword ptr [r9+r13*4],11223344h
;mov dword ptr [r12],11223344h
;mov dword ptr [r13+rdx*4],11223344h
;mov dword ptr [rdx+R9*4],11223344h
Jit_RelativeAddress ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_EntryIp
;; *****************************************************************************
PUBLIC Jit_EntryIp
_TEXT	SEGMENT
Jit_EntryIp PROC
;;Note: The Ip is already loaded into the rax register 
RET				;; Return value already in rax
Jit_EntryIp ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_EntryOpcode
;; *****************************************************************************
PUBLIC Jit_EntryOpcode
_TEXT	SEGMENT
Jit_EntryOpcode PROC
;;Note: The Opcode is already loaded into the rdx register 
mov	rax,rdx
RET				;; Return value in rax
Jit_EntryOpcode ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_Jump
;; *****************************************************************************
PUBLIC Jit_Jump
_TEXT	SEGMENT
Jit_Jump PROC
;;Note: The jump destination is passed in rcx
;;Note: In 64-bit, the first argument is stored in rcx and not on the stack
add rsp,8		;; Drop the return address
mov rbp,rsp		;; Reset the frame pointer
jmp rcx			;; Jump to the destination address
Jit_Jump ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_JumpCC
;; *****************************************************************************
PUBLIC Jit_JumpCC
_TEXT	SEGMENT
Jit_JumpCC PROC
;;Note: The Ip jump destination is passed in rcx
;;Note: The JIp jump destination is passed in rdx
;;Note: In 64-bit, the first argument is stored in rcx and not on the stack
;;Note: In 64-bit, the second argument is stored in rdx and not on the stack
add rsp,8		;; Drop the return address
mov rbp,rsp		;; Reset the frame pointer
jmp rcx			;; Jump to the destination address
Jit_JumpCC ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetRBX
;; *****************************************************************************
PUBLIC Jit_GetRBX
_TEXT	SEGMENT
Jit_GetRBX PROC
;;Note: RBX is non-volatile and must be saved
mov	rax,rbx
RET				;; Return value in rax
Jit_GetRBX ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetRBX
;; *****************************************************************************
PUBLIC Jit_SetRBX
_TEXT	SEGMENT
Jit_SetRBX PROC
;;Note: RBX is non-volatile and must be saved
mov	rbx,rcx		;; rcx register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetRBX ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetRBP
;; *****************************************************************************
PUBLIC Jit_GetRBP
_TEXT	SEGMENT
Jit_GetRBP PROC
;;Note: RBP is non-volatile and must be saved
mov	rax,rbp
RET				;; Return value in rax
Jit_GetRBP ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetRBP
;; *****************************************************************************
PUBLIC Jit_SetRBP
_TEXT	SEGMENT
Jit_SetRBP PROC
;;Note: RBP is non-volatile and must be saved
mov	rbp,rcx		;; rcx register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetRBP ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetRSI
;; *****************************************************************************
PUBLIC Jit_GetRSI
_TEXT	SEGMENT
Jit_GetRSI PROC
;;Note: RSI is non-volatile and must be saved
mov	rax,rsi
RET				;; Return value in rax
Jit_GetRSI ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetRSI
;; *****************************************************************************
PUBLIC Jit_SetRSI
_TEXT	SEGMENT
Jit_SetRSI PROC
;;Note: RSI is non-volatile and must be saved
mov	rsi,rcx		;; rcx register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetRSI ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetRDI
;; *****************************************************************************
PUBLIC Jit_GetRDI
_TEXT	SEGMENT
Jit_GetRDI PROC
;;Note: RDI is non-volatile and must be saved
mov	rax,rdi
RET				;; Return value in rax
Jit_GetRDI ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetRDI
;; *****************************************************************************
PUBLIC Jit_SetRDI
_TEXT	SEGMENT
Jit_SetRDI PROC
;;Note: RDI is non-volatile and must be saved
mov	rdi,rcx		;; rcx register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetRDI ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetR12
;; *****************************************************************************
PUBLIC Jit_GetR12
_TEXT	SEGMENT
Jit_GetR12 PROC
;;Note: R12 is non-volatile and must be saved
mov	rax,r12
RET				;; Return value in rax
Jit_GetR12 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetR12
;; *****************************************************************************
PUBLIC Jit_SetR12
_TEXT	SEGMENT
Jit_SetR12 PROC
;;Note: R12 is non-volatile and must be saved
mov	r12,rcx		;; rcx register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetR12 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetR13
;; *****************************************************************************
PUBLIC Jit_GetR13
_TEXT	SEGMENT
Jit_GetR13 PROC
;;Note: R13 is non-volatile and must be saved
mov	rax,r13
RET				;; Return value in rax
Jit_GetR13 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetR13
;; *****************************************************************************
PUBLIC Jit_SetR13
_TEXT	SEGMENT
Jit_SetR13 PROC
;;Note: R13 is non-volatile and must be saved
mov	r13,rcx		;; rcx register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetR13 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetR14
;; *****************************************************************************
PUBLIC Jit_GetR14
_TEXT	SEGMENT
Jit_GetR14 PROC
;;Note: R14 is non-volatile and must be saved
mov	rax,r14
RET				;; Return value in rax
Jit_GetR14 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetR14
;; *****************************************************************************
PUBLIC Jit_SetR14
_TEXT	SEGMENT
Jit_SetR14 PROC
;;Note: R14 is non-volatile and must be saved
mov	r14,rcx		;; rcx register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetR14 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetR15
;; *****************************************************************************
PUBLIC Jit_GetR15
_TEXT	SEGMENT
Jit_GetR15 PROC
;;Note: R15 is non-volatile and must be saved
mov	rax,r15
RET				;; Return value in rax
Jit_GetR15 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetR15
;; *****************************************************************************
PUBLIC Jit_SetR15
_TEXT	SEGMENT
Jit_SetR15 PROC
;;Note: R15 is non-volatile and must be saved
mov	r15,rcx		;; rcx register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetR15 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetXMM6
;; *****************************************************************************
PUBLIC Jit_GetXMM6
_TEXT	SEGMENT
Jit_GetXMM6 PROC
;;Note: XMM6 is non-volatile and must be saved
movq	xmm0,xmm6
RET				;; Return value in xmm0
Jit_GetXMM6 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetXMM6
;; *****************************************************************************
PUBLIC Jit_SetXMM6
_TEXT	SEGMENT
Jit_SetXMM6 PROC
;;Note: XMM6 is non-volatile and must be saved
movq	xmm6,xmm0		;; xmm0 register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetXMM6 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetXMM7
;; *****************************************************************************
PUBLIC Jit_GetXMM7
_TEXT	SEGMENT
Jit_GetXMM7 PROC
;;Note: XMM7 is non-volatile and must be saved
movq	xmm0,xmm7
RET				;; Return value in xmm0
Jit_GetXMM7 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetXMM7
;; *****************************************************************************
PUBLIC Jit_SetXMM7
_TEXT	SEGMENT
Jit_SetXMM7 PROC
;;Note: XMM7 is non-volatile and must be saved
movq	xmm7,xmm0		;; xmm0 register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetXMM7 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetXMM8
;; *****************************************************************************
PUBLIC Jit_GetXMM8
_TEXT	SEGMENT
Jit_GetXMM8 PROC
;;Note: XMM8 is non-volatile and must be saved
movq	xmm0,xmm8
RET				;; Return value in xmm0
Jit_GetXMM8 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetXMM8
;; *****************************************************************************
PUBLIC Jit_SetXMM8
_TEXT	SEGMENT
Jit_SetXMM8 PROC
;;Note: XMM8 is non-volatile and must be saved
movq	xmm8,xmm0		;; xmm0 register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetXMM8 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetXMM9
;; *****************************************************************************
PUBLIC Jit_GetXMM9
_TEXT	SEGMENT
Jit_GetXMM9 PROC
;;Note: XMM9 is non-volatile and must be saved
movq	xmm0,xmm9
RET				;; Return value in xmm0
Jit_GetXMM9 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetXMM9
;; *****************************************************************************
PUBLIC Jit_SetXMM9
_TEXT	SEGMENT
Jit_SetXMM9 PROC
;;Note: XMM9 is non-volatile and must be saved
movq	xmm9,xmm0		;; xmm0 register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetXMM9 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetXMM10
;; *****************************************************************************
PUBLIC Jit_GetXMM10
_TEXT	SEGMENT
Jit_GetXMM10 PROC
;;Note: XMM10 is non-volatile and must be saved
movq	xmm0,xmm10
RET				;; Return value in xmm0
Jit_GetXMM10 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetXMM10
;; *****************************************************************************
PUBLIC Jit_SetXMM10
_TEXT	SEGMENT
Jit_SetXMM10 PROC
;;Note: XMM10 is non-volatile and must be saved
movq	xmm10,xmm0		;; xmm0 register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetXMM10 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetXMM11
;; *****************************************************************************
PUBLIC Jit_GetXMM11
_TEXT	SEGMENT
Jit_GetXMM11 PROC
;;Note: XMM11 is non-volatile and must be saved
movq	xmm0,xmm11
RET				;; Return value in xmm0
Jit_GetXMM11 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetXMM11
;; *****************************************************************************
PUBLIC Jit_SetXMM11
_TEXT	SEGMENT
Jit_SetXMM11 PROC
;;Note: XMM11 is non-volatile and must be saved
movq	xmm11,xmm0		;; xmm0 register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetXMM11 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetXMM12
;; *****************************************************************************
PUBLIC Jit_GetXMM12
_TEXT	SEGMENT
Jit_GetXMM12 PROC
;;Note: XMM12 is non-volatile and must be saved
movq	xmm0,xmm12
RET				;; Return value in xmm0
Jit_GetXMM12 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetXMM12
;; *****************************************************************************
PUBLIC Jit_SetXMM12
_TEXT	SEGMENT
Jit_SetXMM12 PROC
;;Note: XMM12 is non-volatile and must be saved
movq	xmm12,xmm0		;; xmm0 register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetXMM12 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetXMM13
;; *****************************************************************************
PUBLIC Jit_GetXMM13
_TEXT	SEGMENT
Jit_GetXMM13 PROC
;;Note: XMM13 is non-volatile and must be saved
movq	xmm0,xmm13
RET				;; Return value in xmm0
Jit_GetXMM13 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetXMM13
;; *****************************************************************************
PUBLIC Jit_SetXMM13
_TEXT	SEGMENT
Jit_SetXMM13 PROC
;;Note: XMM13 is non-volatile and must be saved
movq	xmm13,xmm0		;; xmm0 register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetXMM13 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetXMM14
;; *****************************************************************************
PUBLIC Jit_GetXMM14
_TEXT	SEGMENT
Jit_GetXMM14 PROC
;;Note: XMM14 is non-volatile and must be saved
movq	xmm0,xmm14
RET				;; Return value in xmm0
Jit_GetXMM14 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetXMM14
;; *****************************************************************************
PUBLIC Jit_SetXMM14
_TEXT	SEGMENT
Jit_SetXMM14 PROC
;;Note: XMM14 is non-volatile and must be saved
movq	xmm14,xmm0		;; xmm0 register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetXMM14 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_GetXMM15
;; *****************************************************************************
PUBLIC Jit_GetXMM15
_TEXT	SEGMENT
Jit_GetXMM15 PROC
;;Note: XMM15 is non-volatile and must be saved
movq	xmm0,xmm15
RET				;; Return value in xmm0
Jit_GetXMM15 ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_SetXMM15
;; *****************************************************************************
PUBLIC Jit_SetXMM15
_TEXT	SEGMENT
Jit_SetXMM15 PROC
;;Note: XMM15 is non-volatile and must be saved
movq	xmm15,xmm0		;; xmm0 register contains the 1st integer parameter
RET				;; Return value in rax
Jit_SetXMM15 ENDP
_TEXT	ENDS

END





