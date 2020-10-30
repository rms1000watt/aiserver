;/**********************************************************************************
;    Copyright (C) 2008 Investment Science Corp.
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
;mov  rdx,qword ptr [rbp+11223344]				;; *Np++ = 0x48; *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFTVAL));Np+=4;
;cvtsd2ss xmm15,xmm10							;; *Np++ = 0xF2; EXTXMMMOD(Rp[modifier[0]].Offset,16,0); *Np++ = 0x0F; *Np++ = 0x5A; *Np++ = (char)0xF8 + XMMXMMR[Rp[modifier[0]].Offset];
;cvtsd2ss xmm15,qword ptr [rbp+11223344]			;; *Np++ = 0xF2; *Np++ = 0x44; *Np++ = 0x0F; *Np++ = 0x5A; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFTVAL));Np+=4;
;movss dword ptr [r13+rdx*4],xmm15				;; *Np++ = 0xF3; *Np++ = 0x45; *Np++ = 0x0F; *Np++ = 0x11; *Np++ = 0x7C; *Np++ = 0x95; *Np++ = 0x00;
;movss dword ptr [R12+rdx*4],xmm15				;; *Np++ = 0xF3; *Np++ = 0x45; *Np++ = 0x0F; *Np++ = 0x11; *Np++ = 0x3C; *Np++ = (char)(0x90+XMMBASE[Rp[modifier[2]].Offset]);
;mov rdx,qword ptr [rbp+11223344]		;; *Np++ = 0x48; *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFTVAL));Np+=4;
;shl rdx,2								;; *Np++ = 0x48; *Np++ = 0xC1; *Np++ = 0xE2; *Np++ = BITSIZEOFFLOAT; 	
;add rdx, qword ptr [Rbp+11223344]		;; *Np++ = 0x48; *Np++ = 0x03; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[2]<<BITSIZEOFTVAL));Np+=4;
;cvtsd2ss xmm15,XMM10					;; *Np++ = 0xF2; EXTXMMMOD(Rp[modifier[0]].Offset,16,0); *Np++ = 0x0F; *Np++ = 0x5A; *Np++ = (char)0xF8 + XMMXMMR[Rp[modifier[0]].Offset];
;movss dword ptr [rdx],xmm15				;; *Np++ = 0xF3; *Np++ = 0x44; *Np++ = 0x0F; *Np++ = 0x11; *Np++ = 0x3A;
;; else
;cvtsd2ss XMM10,xmm15					;; *Np++ = 0xF2; *Np++ = 0x44; *Np++ = 0x0F; *Np++ = 0x5A; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFTVAL));Np+=4;
;movss dword ptr [rdx],xmm15				;; *Np++ = 0xF3; *Np++ = 0x44; *Np++ = 0x0F; *Np++ = 0x11; *Np++ = 0x3A;
;; old study
;mov rdx,qword ptr [Rbp+11223344]		;; *Np++ = 0x48; *Np++ = 0x8B; *Np++ = 0x95; *(LpNUM)Np = (RpRelAddress+(modifier[1]<<BITSIZEOFTVAL));Np+=4;
;cvtsd2ss xmm15,qword ptr [Rbp+11223344] ;; *Np++ = 0xF2; *Np++ = 0x44; *Np++ = 0x0F; *Np++ = 0x5A; *Np++ = 0xBD; *(LpNUM)Np = (RpRelAddress+(modifier[0]<<BITSIZEOFTVAL));Np+=4;
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


END





