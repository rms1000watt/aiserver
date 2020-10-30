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

#if 0
/*
FMemory.h

Interface for the memory allocation class which supports the runtime memory management 
interface between the Smartbase High Speed Engine and the host memory manager system.
The data structures, macros, and coding conventions used in this source file are designed
to isolate and protect the Smartbase High Speed Engine from the low level differences
between the Macintosh, Windows 3.1, Windows NT, and Unix memory management practices. 

Note:   This class supports C++ coding conventions for host system independent
        memory management. The goal is to maximize portability of the Smartbase
        High Speed Engine.


AUTHORS:            Michael F. Korns, Tim May

*/
#endif
 
#ifndef _H_FMemory
#define _H_FMemory
#include "fsmtbase.h"

/*  Declare all global macros for this Class. */

#define     _FMemory_memset(ptr,fill,length)    memset(ptr,fill,length)

#define     _FMemory_memcpy(to,from,length)     memcpy(to,from,length)

/* Note: Please keep this total size to an even multiple of eight bytes. */
#define		_AllocationChunkSize	(_FSmartbase_HeapBlockSize+SIZEOF_MHANDLEINFO)

/*  Define macro which explicitly calculates the offset to the */
/*  variable portion of the data and casts it to an appropriate type. */

/* #define MHandleData(h)  h->data */

/* Define sizeof macros. */

/* _FMemory_trueSizeOf takes a memory header argument and determines the actual memory allocated to the */
/* its data portion. If the ->next element points to a subsequent block then the amount of memory allocated */
/* is simply the difference between the ->data element of the current block and the begining of the next block. */
/* If the ->next element is NULL, then no subsequent block exists and the current block's data area is considered */
/* to extend to the end of available memory. */
#define _FMemory_dataSizeOf(p) (((LpMHANDLEINFO)p)->next ?\
                                (POINTER)((LpMHANDLEINFO)p)->next - (POINTER)&((LpMHANDLEINFO)p)->data[0] :\
                                (POINTER)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated) - (POINTER)&((LpMHANDLEINFO)p)->data[0])

#define _FMemory_fullSizeOf(p) (((LpMHANDLEINFO)p)->next ?\
                                (POINTER)((LpMHANDLEINFO)p)->next - (POINTER)p :\
                                (POINTER)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated) - (POINTER)p)

/*  Check a Smartbase memory handle for validity. */
#define _VALIDHANDLE(h) (((LpCHAR)h >= (LpCHAR)gCP->FMemory_ParentPtr) && \
						 ((LpCHAR)h < (LpCHAR)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated)) && \
						 (((LpMHANDLEINFO)h)->Frame >= 0) && \
						 (((LpMHANDLEINFO)h)->Frame < _MEMMAXFRAMES) && \
						 (((((LpMHANDLEINFO)h)->Fixed == TRUE) || (((LpMHANDLEINFO)h)->Free != TRUE)) || (((LpMHANDLEINFO)h)->prev ? ((((LpMHANDLEINFO)h)->prev->Fixed == TRUE) || (((LpMHANDLEINFO)h)->prev->Free != TRUE)): TRUE)) && \
						 (((((LpMHANDLEINFO)h)->Fixed == TRUE) || (((LpMHANDLEINFO)h)->Free != TRUE)) || (((LpMHANDLEINFO)h)->next ? ((((LpMHANDLEINFO)h)->next->Fixed == TRUE) || (((LpMHANDLEINFO)h)->next->Free != TRUE)): TRUE)))
/*  Check a Smartbase memory handle for validity and throw if invalid. */
/*  Note: This macro will error out if _AllocationChunkSize is NOT an even multiple of eight bytes. */

#define _BADHANDLE(h)   (!_VALIDHANDLE(h) || (_FMemory_fullSizeOf(h) < gCP->FMemory_FrameSize[((LpMHANDLEINFO)h)->Frame]))

/*  End of global variables used in Smartbase test memory management system */

/*  Function declarations */
extern NUM				FMemory_GetFrame	(LpXCONTEXT gCP, NUM requestedBlockSize);

extern  HMemory         FMemory_Copy		(LpXCONTEXT gCP, LpTHREAD gTP, HMemory self);
extern  void            FMemory_Free		(LpXCONTEXT gCP, LpTHREAD gTP, HMemory self);
extern  NUM             FMemory_FreeSpace	(LpXCONTEXT gCP, LpTHREAD gTP);
extern  void            FMemory_memcpy		(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR to, LpCHAR from, NUM length);
extern  void            FMemory_Init		(LpXCONTEXT gCP, LpTHREAD gTP, struct FSmartbase_HostCallBackFunctions Funcs);
extern  HMemory         FMemory_New			(LpXCONTEXT gCP, LpTHREAD gTP, NUM memSize, BOLE ZeroOut);
extern  void*           FMemory_NewFixedBlock(LpXCONTEXT gCP, LpTHREAD gTP, NUM memSize);
extern  HMemory         FMemory_Resize		(LpXCONTEXT gCP, LpTHREAD gTP, HMemory self,NUM newSize);

extern  TVAL            FMemory_PreAllocateFixedBlock(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FMemory_SystemDiagnostic(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern	void			FMemory_CheckMemoryBlocks(LpXCONTEXT gCP,LpTHREAD gTP);
extern	void			FMemory_DumpMemoryBlocks(LpXCONTEXT gCP, LpTHREAD gTP);

#if defined(_C_FMEMORY)
#include "fsmtbase.h"
#include "fconio.h"
#endif
#endif

