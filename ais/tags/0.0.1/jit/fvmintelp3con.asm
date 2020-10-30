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
;; FVmIntelP3Con.asm
;;
;; Implementation of the Virtual Machine Connector for the Intel P3 JIT.
;;
;; This source file contains the main connector functions for the Intel P3 
;; Just-In-Time-Compiler (JIT). This JIT is designed to work hand-in-glove
;; with the FVmScript_Emulator DRM virtual machine emulator.
;;
;; NOTE: These assembler functions are required because some C compilers do not
;;       support inline assembler. 
;;
;; PARENT:          FVmIntelP3Jit.c 
;; 
;; AUTHORS:         Michael F. Korns, Ted Williams
;;
;; *****************************************************************************

.386 
.model flat, c 
;include listing.inc

;INCLUDELIB LIBCMTD
;INCLUDELIB OLDNAMES

.CODE

;; *****************************************************************************
;; Jit_LoadJumpTable
;; *****************************************************************************
PUBLIC Jit_LoadJumpTable
_TEXT	SEGMENT
Jit_LoadJumpTable PROC
;;Note: The last return address is passed as a return value 
pop eax			;; Save the return address
push eax		;; Restore the last return address
;; Use the following line for VS 2003
;; add eax,24		;; Increment for the 'Ip= ' assignment and conditional 'goto lbl;' instructions which follows the return address
;; Use the following line for VS 2005
add eax,17		;; Increment for the 'Ip= ' assignment and conditional 'goto lbl;' instructions which follows the return address
RET				;; Return the incremented return address
Jit_LoadJumpTable ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_RelativeAddress
;; *****************************************************************************
PUBLIC Jit_RelativeAddress
_TEXT	SEGMENT
Jit_RelativeAddress PROC
;;Note1: The absolute address is passed as an argument value 
;;Note2: Do NOT use masm argument directives as they will alter the ebp register!!
pop  ecx			;; Save the return address
pop  eax			;; Save the absolute address argument
sub  eax,ebp		;; Convert the absolute address to a relative address argument
push eax			;; Restore the absolute address argument (because C will expect this)
jmp	 ecx			;; Return the relative address argument
Jit_RelativeAddress ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_EntryIp
;; *****************************************************************************
PUBLIC Jit_EntryIp
_TEXT	SEGMENT
Jit_EntryIp PROC
;;Note: The Ip is already loaded into the eax register 
RET				;; Return value already in eax
Jit_EntryIp ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_EntryOpcode
;; *****************************************************************************
PUBLIC Jit_EntryOpcode
_TEXT	SEGMENT
Jit_EntryOpcode PROC
;;Note: The Opcode is already loaded into the edx register 
mov	eax,edx
RET				;; Return value already in eax
Jit_EntryOpcode ENDP
_TEXT	ENDS

;; *****************************************************************************
;; Jit_Jump
;; *****************************************************************************
PUBLIC Jit_Jump
_TEXT	SEGMENT
Jit_Jump PROC
;;Note: The jump distination is passed as an argument on the stack 
add esp,4		;; Drop the return address
pop eax			;; Save the jump destination address
jmp eax			;; Jumpo to the destination address
Jit_Jump ENDP
_TEXT	ENDS


END





