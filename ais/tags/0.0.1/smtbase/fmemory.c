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

#define _C_FMEMORY
#define _SMARTBASE


#if 0
/*
FMemory.c

Methods for the memory allocation class which supports the runtime memory management 
interface between the Smartbase High Speed Engine and the host memory manager system.
The data structures, macros, and coding conventions used in this source file are designed
to isolate and protect the Smartbase High Speed Engine from the low level differences
between the Macintosh, Windows 3.1, Windows NT, and Unix memory management practices. 

Note:   This class supports C++ coding conventions for host system independent
        memory management. The goal is to maximize portability of the Smartbase
        High Speed Engine.


AUTHORS:            Michael F. Korns, Timothy O. May

MODIFICATIONS:  

*/
#endif

#include "fmemory.h"
#include "tobject.h"
#include "tpcodvec.h"
#include "tdiction.h"
#include "tdirect.h"
#include "tstruct.h"
#include "tintvec.h"
#include "tbytevec.h"
#include "tbitvec.h"
#include "tnumvec.h"
#include "tfltvec.h"
#include "tsymbol.h"
#include "tstring.h"
#include "tobjvec.h"
#include "tdatabas.h"
#include "futil1.h"
#include "fcompile.h"
#include "tmatrix.h"
#include "tnummat.h"
#include "tcpxvec.h"
#include "tshortvec.h"
#include "fpred.h"
#include "fpred2.h"

/* FPerfmon.h defines performance monitoring macros */
#include "fperfmon.h"

/*  Define macros to allow easy modification without affecting FMemory.h */
#define _FMemory_RoundEB(x)					((NUM)(((ALLIGNMENTFACTOR-1) + (NUM)x) & (~(ALLIGNMENTFACTOR-1))) < 0) ? MAXNUM : (((ALLIGNMENTFACTOR-1) + (NUM)x) & (~(ALLIGNMENTFACTOR-1)))
#define _FMemory_EvenHeaderMultiples(x)		(((x+_FSmartbase_HeapBlockSize-1)/_FSmartbase_HeapBlockSize)*_FSmartbase_HeapBlockSize)

extern TVAL FMath2_Sort         (LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

/* Define some stuctures for use only in this file */
typedef struct {
	POINTER	Start;
	NUM Length;
} OSBlockElement;

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_Init

Initialize the host operating system memory manager interface and our C++ memory management
extensions.

Note:   This function should only be called once at the beginning of the application.
*/
#endif

void    FMemory_Init(LpXCONTEXT gCP, LpTHREAD gTP, struct FSmartbase_HostCallBackFunctions Funcs)
{
NUM				i, j, k, n;
NUM				objSize;
NUM				newFrameSize = 0;
NUM				oldFrameSize = 0;
LpMHANDLEINFO	Block[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
OSBlockElement	OSBlock[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
NUM				MemoryInBlock = 0;

/*  Don't initialize more than once. */
if (gCP->FMemory_Initialized) return;
gCP->FMemory_Initialized     = TRUE;

gCP->FMemory_UsedBlockCount  = 0;
gCP->FMemory_UsedMemoryBytes = 0;

gCP->FMemory_BlockedBlockCount = 0;
gCP->FMemory_BlockedMemoryBytes = 0;

gCP->FMemory_FreeBlockCount = 0;
gCP->FMemory_FreeMemoryBytes = 0;

gCP->FMemory_SystemCheckCount = 0;
gCP->FMemory_JoinCount = 0;
gCP->FMemory_SplitCount = 0;
gCP->FMemory_NewCount = 0;

/*  Note:   Problem: The builtin Macintosh memory management system is too */
/*          slow for larger numbers of memory handles. */
/*    */
/*          To avoid this problem and to increase allocation speed, the  */
/*          parent memory block is subdivided into distinctly sized subblocks.  */
/*          These subblocks are sized in powers of two: 16, 32, 64, 128, etc.  */
/*			and are associated with a frameIndex.	*/
/*			The frameIndex indexes into the gCP->FMemory_FrameSize[] and gCP->FMemory_FrameNextMHandle[] */
/*			FMemory_FrameSize[frameIndex] contains the size of subblocks for that frame index */
/*			FMemory_FrameNextMHandle[frameIndex] contains a handle to the first free subblock associated */
/*			with that frame size */
/*			When memory is requested the following steps are taken: */
/*			1. Find the correct frame size the same or larger than the memory being requested. */
/*			2. If a free subblock is available then use it. */
/*			   else create a new subblock. */
/*          Whenever a handle is created, a subblock from the proper frame  */
/*          is allocated. This increases the speed of memory allocation,  */
/*          reduces fragmentation, but imposes the penalty that one always  */
/*          receives a memory block whose size is the nearest power of two  */
/*          equal to or greater than the requested size.  */
/* */
/*          Note:   Each subblock is immediately preceeded by its handle */
/*                  information prefix, LpMHANDLEINFO. */

/*  Allocate the parent memory block and set the amount of memory allocated */
/*  (FMemory_MemoryAllocated) to zero. */
/* DEBUG FSmartbase_Log(gCP, gTP, "FMemory_Init() Allocate memory blocks and stacks.\r\n");	*/ /* NDEBUG */
gCP->FMemory_MemoryAllocated = 0L; /* Add this to FMemory_ParentPtr to get next free memory location */
gCP->FMemory_PrevPtr = NULL;
gCP->FMemory_UsedBlockCount = 0;

MemoryInBlock = Funcs._Host_memBlockSize[0] - (XCONTEXTBLOCKLENGTH + (THREADBLOCKLENGTH * (gCP->maxNumberOfThreads - 1)));

if (MemoryInBlock <= 0)
    { 
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_OUT_OF_MEMORY);
    return;
    }

gCP->FMemory_ParentPtr = Funcs._Host_memBlockPtr[0] + (XCONTEXTBLOCKLENGTH + (THREADBLOCKLENGTH * (gCP->maxNumberOfThreads - 1)));

/*  Allocate the garbage collection stack. */
gTP->MaxObjStackSize = Funcs._Host_GarbageStackWords;
gTP->ObjStack = (OBJ**)gCP->FMemory_ParentPtr;
n = (NUM)((gTP->MaxObjStackSize + 100) * sizeof(OBJ**));
gCP->FMemory_ParentPtr += n;
MemoryInBlock -= n;
if (MemoryInBlock <= 0)
    { 
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_OUT_OF_MEMORY);
    return;
    }

/*  Allocate the operations stack. */
gTP->MaxTvalStackSize = Funcs._Host_OperationsStackWords;
gTP->TvalStack = (TVAL*)gCP->FMemory_ParentPtr;
n = (NUM)((gTP->MaxTvalStackSize + 100) * sizeof(TVAL));
gCP->FMemory_ParentPtr += n;
MemoryInBlock -= n;
if (MemoryInBlock <= 0)
    { 
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_OUT_OF_MEMORY);
    return;
    }

/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_Init, Initialize frame table vectors.\r\n");	*/ /* NDEBUG */
/*  Initialize the frame table vectors. To avoid fragmentation and to increase */
/*  allocation speed, the parent memory block is subdivided into fixed size */
/*  frames. These frames are sized incrementally, in multiples of the block header size. */
/*  Whenever a handle is created, a subblock from the proper frame is allocated. */
/*  This increases the speed of memory allocation, reduces fragmentation, but */
/*  imposes the penalty that one always receives a memory block whose size is the */
/*  nearest fixed frame size equal to or greater than the requested size.    */

for (n = 0; n < _MEMMAXFRAMES; ++n)
    {
    gCP->FMemory_FrameNextMHandle[n] = NULL;  /*  Set all frames empty */
    gCP->FMemory_FrameSize[n] = MAXNUM;	// Set max possible size on 32-bit system
    }

/* Make sure that FMemory_SystemDiagnostics self tests are	*/											
/* changed to reflect any changes made in these tables.		*/												
gCP->FMemory_FrameSize[0]    = _FMemory_RoundEB(_AllocationChunkSize);			
gCP->FMemory_FrameSize[1]    = _FMemory_RoundEB(_AllocationChunkSize*2);
gCP->FMemory_FrameSize[2]    = _FMemory_RoundEB(_AllocationChunkSize*3);
gCP->FMemory_FrameSize[3]    = _FMemory_RoundEB(_AllocationChunkSize*4);
gCP->FMemory_FrameSize[4]    = _FMemory_RoundEB(_AllocationChunkSize*5);
gCP->FMemory_FrameSize[5]    = _FMemory_RoundEB(_AllocationChunkSize*6);
gCP->FMemory_FrameSize[6]    = _FMemory_RoundEB(_AllocationChunkSize*7);
gCP->FMemory_FrameSize[7]    = _FMemory_RoundEB(_AllocationChunkSize*8);
gCP->FMemory_FrameSize[8]    = _FMemory_RoundEB(_AllocationChunkSize*9);
gCP->FMemory_FrameSize[9]    = _FMemory_RoundEB(_AllocationChunkSize*10);
gCP->FMemory_FrameSize[10]   = _FMemory_RoundEB(_AllocationChunkSize*12);
gCP->FMemory_FrameSize[11]   = _FMemory_RoundEB(_AllocationChunkSize*14);
gCP->FMemory_FrameSize[12]   = _FMemory_RoundEB(_AllocationChunkSize*16);	
gCP->FMemory_FrameSize[13]   = _FMemory_RoundEB(_AllocationChunkSize*18);
gCP->FMemory_FrameSize[14]   = _FMemory_RoundEB(_AllocationChunkSize*20);
gCP->FMemory_FrameSize[15]   = _FMemory_RoundEB(_AllocationChunkSize*22);
gCP->FMemory_FrameSize[16]   = _FMemory_RoundEB(_AllocationChunkSize*24);
gCP->FMemory_FrameSize[17]   = _FMemory_RoundEB(_AllocationChunkSize*28);
gCP->FMemory_FrameSize[18]   = _FMemory_RoundEB(_AllocationChunkSize*30);
gCP->FMemory_FrameSize[19]   = _FMemory_RoundEB(_AllocationChunkSize*32);
gCP->FMemory_FrameSize[20]   = _FMemory_RoundEB(_AllocationChunkSize*34);
gCP->FMemory_FrameSize[21]   = _FMemory_RoundEB(_AllocationChunkSize*36);
gCP->FMemory_FrameSize[22]   = _FMemory_RoundEB(_AllocationChunkSize*38);
gCP->FMemory_FrameSize[23]   = _FMemory_RoundEB(_AllocationChunkSize*40);
gCP->FMemory_FrameSize[24]   = _FMemory_RoundEB(_AllocationChunkSize*42);
gCP->FMemory_FrameSize[25]   = _FMemory_RoundEB(_AllocationChunkSize*44);
gCP->FMemory_FrameSize[26]   = _FMemory_RoundEB(_AllocationChunkSize*46);
gCP->FMemory_FrameSize[27]   = _FMemory_RoundEB(_AllocationChunkSize*48);
gCP->FMemory_FrameSize[28]   = _FMemory_RoundEB(_AllocationChunkSize*50);
gCP->FMemory_FrameSize[29]   = _FMemory_RoundEB(_AllocationChunkSize*52);
gCP->FMemory_FrameSize[30]   = _FMemory_RoundEB(_AllocationChunkSize*54);
gCP->FMemory_FrameSize[31]   = _FMemory_RoundEB(_AllocationChunkSize*56);
gCP->FMemory_FrameSize[32]   = _FMemory_RoundEB(_AllocationChunkSize*58);
gCP->FMemory_FrameSize[33]   = _FMemory_RoundEB(_AllocationChunkSize*60);
gCP->FMemory_FrameSize[34]   = _FMemory_RoundEB(_AllocationChunkSize*64);
gCP->FMemory_FrameSize[35]   = _FMemory_RoundEB(_AllocationChunkSize*68);
gCP->FMemory_FrameSize[36]   = _FMemory_RoundEB(_AllocationChunkSize*72);
gCP->FMemory_FrameSize[37]   = _FMemory_RoundEB(_AllocationChunkSize*76);
gCP->FMemory_FrameSize[38]   = _FMemory_RoundEB(_AllocationChunkSize*80);
gCP->FMemory_FrameSize[39]   = _FMemory_RoundEB(_AllocationChunkSize*88);
gCP->FMemory_FrameSize[40]   = _FMemory_RoundEB(_AllocationChunkSize*96);
gCP->FMemory_FrameSize[41]   = _FMemory_RoundEB(_AllocationChunkSize*104);
gCP->FMemory_FrameSize[42]   = _FMemory_RoundEB(_AllocationChunkSize*112);
gCP->FMemory_FrameSize[43]   = _FMemory_RoundEB(_AllocationChunkSize*120);
gCP->FMemory_FrameSize[44]   = _FMemory_RoundEB(_AllocationChunkSize*136);
gCP->FMemory_FrameSize[45]   = _FMemory_RoundEB(_AllocationChunkSize*152);
gCP->FMemory_FrameSize[46]   = _FMemory_RoundEB(_AllocationChunkSize*168);
gCP->FMemory_FrameSize[47]   = _FMemory_RoundEB(_AllocationChunkSize*184);
gCP->FMemory_FrameSize[48]   = _FMemory_RoundEB(_AllocationChunkSize*200);
gCP->FMemory_FrameSize[49]   = _FMemory_RoundEB(_AllocationChunkSize*232);
gCP->FMemory_FrameSize[50]   = _FMemory_RoundEB(_AllocationChunkSize*264);
gCP->FMemory_FrameSize[51]   = _FMemory_RoundEB(_AllocationChunkSize*296);
gCP->FMemory_FrameSize[52]   = _FMemory_RoundEB(_AllocationChunkSize*328);
gCP->FMemory_FrameSize[53]   = _FMemory_RoundEB(_AllocationChunkSize*392);
gCP->FMemory_FrameSize[54]   = _FMemory_RoundEB(_AllocationChunkSize*456);
gCP->FMemory_FrameSize[55]   = _FMemory_RoundEB(_AllocationChunkSize*520);
gCP->FMemory_FrameSize[56]   = _FMemory_RoundEB(_AllocationChunkSize*584);
gCP->FMemory_FrameSize[57]   = _FMemory_RoundEB(_AllocationChunkSize*712);
gCP->FMemory_FrameSize[58]   = _FMemory_RoundEB(_AllocationChunkSize*840);
gCP->FMemory_FrameSize[59]   = _FMemory_RoundEB(_AllocationChunkSize*970);
gCP->FMemory_FrameSize[60]   = _FMemory_RoundEB(_AllocationChunkSize*1100);
gCP->FMemory_FrameSize[61]   = _FMemory_RoundEB(_AllocationChunkSize*1300);
gCP->FMemory_FrameSize[62]   = _FMemory_RoundEB(_AllocationChunkSize*1500);
gCP->FMemory_FrameSize[63]   = _FMemory_RoundEB(_AllocationChunkSize*1700);
gCP->FMemory_FrameSize[64]   = _FMemory_RoundEB(_AllocationChunkSize*1900);
gCP->FMemory_FrameSize[65]   = _FMemory_RoundEB(_AllocationChunkSize*2200);
gCP->FMemory_FrameSize[66]   = _FMemory_RoundEB(_AllocationChunkSize*2500);
gCP->FMemory_FrameSize[67]   = _FMemory_RoundEB(_AllocationChunkSize*2800);
gCP->FMemory_FrameSize[68]   = _FMemory_RoundEB(_AllocationChunkSize*3100);
gCP->FMemory_FrameSize[69]   = _FMemory_RoundEB(_AllocationChunkSize*3400);
gCP->FMemory_FrameSize[70]   = _FMemory_RoundEB(_AllocationChunkSize*3700);
gCP->FMemory_FrameSize[71]   = _FMemory_RoundEB(_AllocationChunkSize*4000);
gCP->FMemory_FrameSize[72]   = _FMemory_RoundEB(_AllocationChunkSize*4300);
gCP->FMemory_FrameSize[73]   = _FMemory_RoundEB(_AllocationChunkSize*4600);
gCP->FMemory_FrameSize[74]   = _FMemory_RoundEB(_AllocationChunkSize*4900);
gCP->FMemory_FrameSize[75]   = _FMemory_RoundEB(_AllocationChunkSize*5100);
gCP->FMemory_FrameSize[76]   = _FMemory_RoundEB(_AllocationChunkSize*5400);
gCP->FMemory_FrameSize[77]   = _FMemory_RoundEB(_AllocationChunkSize*5700);
gCP->FMemory_FrameSize[78]   = _FMemory_RoundEB(_AllocationChunkSize*6000);
gCP->FMemory_FrameSize[79]   = _FMemory_RoundEB(_AllocationChunkSize*6300);
gCP->FMemory_FrameSize[80]   = _FMemory_RoundEB(_AllocationChunkSize*6600);
gCP->FMemory_FrameSize[81]   = _FMemory_RoundEB(_AllocationChunkSize*6900);
gCP->FMemory_FrameSize[82]   = _FMemory_RoundEB(_AllocationChunkSize*7200);
gCP->FMemory_FrameSize[83]   = _FMemory_RoundEB(_AllocationChunkSize*7500);
gCP->FMemory_FrameSize[84]   = _FMemory_RoundEB(_AllocationChunkSize*7800);
gCP->FMemory_FrameSize[85]   = _FMemory_RoundEB(_AllocationChunkSize*8100);
gCP->FMemory_FrameSize[86]   = _FMemory_RoundEB(_AllocationChunkSize*8400);
gCP->FMemory_FrameSize[87]   = _FMemory_RoundEB(_AllocationChunkSize*8700);
gCP->FMemory_FrameSize[88]   = _FMemory_RoundEB(_AllocationChunkSize*9000);		
gCP->FMemory_FrameSize[89]   = _FMemory_RoundEB(_AllocationChunkSize*9600);
gCP->FMemory_FrameSize[90]   = _FMemory_RoundEB(_AllocationChunkSize*10000);
gCP->FMemory_FrameSize[91]   = _FMemory_RoundEB(_AllocationChunkSize*11000);
gCP->FMemory_FrameSize[92]   = _FMemory_RoundEB(_AllocationChunkSize*13000);
gCP->FMemory_FrameSize[93]   = _FMemory_RoundEB(_AllocationChunkSize*16000);
gCP->FMemory_FrameSize[94]   = _FMemory_RoundEB(_AllocationChunkSize*20000);
gCP->FMemory_FrameSize[95]   = _FMemory_RoundEB(_AllocationChunkSize*24000);
gCP->FMemory_FrameSize[96]   = _FMemory_RoundEB(_AllocationChunkSize*28000);
gCP->FMemory_FrameSize[97]   = _FMemory_RoundEB(_AllocationChunkSize*32000);
gCP->FMemory_FrameSize[98]   = _FMemory_RoundEB(_AllocationChunkSize*36000);
gCP->FMemory_FrameSize[99]   = _FMemory_RoundEB(_AllocationChunkSize*40000);
gCP->FMemory_FrameSize[100]  = _FMemory_RoundEB(_AllocationChunkSize*44000);
gCP->FMemory_FrameSize[101]  = _FMemory_RoundEB(_AllocationChunkSize*48000);
gCP->FMemory_FrameSize[102]  = _FMemory_RoundEB(_AllocationChunkSize*52000);
gCP->FMemory_FrameSize[103]  = _FMemory_RoundEB(_AllocationChunkSize*56000);
gCP->FMemory_FrameSize[104]  = _FMemory_RoundEB(_AllocationChunkSize*60000);
gCP->FMemory_FrameSize[105]  = _FMemory_RoundEB(_AllocationChunkSize*68000);
gCP->FMemory_FrameSize[106]  = _FMemory_RoundEB(_AllocationChunkSize*72000);
gCP->FMemory_FrameSize[107]  = _FMemory_RoundEB(_AllocationChunkSize*80000);
gCP->FMemory_FrameSize[108]  = _FMemory_RoundEB(_AllocationChunkSize*90000);
gCP->FMemory_FrameSize[109]  = _FMemory_RoundEB(_AllocationChunkSize*100000);
gCP->FMemory_FrameSize[110]  = _FMemory_RoundEB(_AllocationChunkSize*110000);
gCP->FMemory_FrameSize[111]  = _FMemory_RoundEB(_AllocationChunkSize*120000);
gCP->FMemory_FrameSize[112]  = _FMemory_RoundEB(_AllocationChunkSize*130000);
gCP->FMemory_FrameSize[113]  = _FMemory_RoundEB(_AllocationChunkSize*140000);
gCP->FMemory_FrameSize[114]  = _FMemory_RoundEB(_AllocationChunkSize*150000);
gCP->FMemory_FrameSize[115]  = _FMemory_RoundEB(_AllocationChunkSize*160000);
gCP->FMemory_FrameSize[116]  = _FMemory_RoundEB(_AllocationChunkSize*170000);
gCP->FMemory_FrameSize[117]  = _FMemory_RoundEB(_AllocationChunkSize*180000);
gCP->FMemory_FrameSize[118]  = _FMemory_RoundEB(_AllocationChunkSize*190000);
gCP->FMemory_FrameSize[119]  = _FMemory_RoundEB(_AllocationChunkSize*200000);
gCP->FMemory_FrameSize[120]  = _FMemory_RoundEB(_AllocationChunkSize*220000);
gCP->FMemory_FrameSize[121]  = _FMemory_RoundEB(_AllocationChunkSize*240000);
gCP->FMemory_FrameSize[122]  = _FMemory_RoundEB(_AllocationChunkSize*260000);
gCP->FMemory_FrameSize[123]  = _FMemory_RoundEB(_AllocationChunkSize*280000);
gCP->FMemory_FrameSize[124]  = _FMemory_RoundEB(_AllocationChunkSize*320000);
gCP->FMemory_FrameSize[125]  = _FMemory_RoundEB(_AllocationChunkSize*360000);
gCP->FMemory_FrameSize[126]  = _FMemory_RoundEB(_AllocationChunkSize*400000);
gCP->FMemory_FrameSize[127]  = _FMemory_RoundEB(_AllocationChunkSize*440000);
gCP->FMemory_FrameSize[128]  = _FMemory_RoundEB(_AllocationChunkSize*480000);
gCP->FMemory_FrameSize[129]  = _FMemory_RoundEB(_AllocationChunkSize*520000);
gCP->FMemory_FrameSize[130]  = _FMemory_RoundEB(_AllocationChunkSize*560000);
gCP->FMemory_FrameSize[131]  = _FMemory_RoundEB(_AllocationChunkSize*600000);
gCP->FMemory_FrameSize[132]  = _FMemory_RoundEB(_AllocationChunkSize*650000);
gCP->FMemory_FrameSize[133]  = _FMemory_RoundEB(_AllocationChunkSize*700000);
gCP->FMemory_FrameSize[134]  = _FMemory_RoundEB(_AllocationChunkSize*800000);
gCP->FMemory_FrameSize[135]  = _FMemory_RoundEB(_AllocationChunkSize*900000);
gCP->FMemory_FrameSize[136]  = _FMemory_RoundEB(_AllocationChunkSize*1000000);	
gCP->FMemory_FrameSize[137]  = _FMemory_RoundEB(_AllocationChunkSize*1100000);
gCP->FMemory_FrameSize[138]  = _FMemory_RoundEB(_AllocationChunkSize*1200000);
gCP->FMemory_FrameSize[139]  = _FMemory_RoundEB(_AllocationChunkSize*1300000);
gCP->FMemory_FrameSize[140]  = _FMemory_RoundEB(_AllocationChunkSize*1400000);
gCP->FMemory_FrameSize[141]  = _FMemory_RoundEB(_AllocationChunkSize*1500000);
gCP->FMemory_FrameSize[142]  = _FMemory_RoundEB(_AllocationChunkSize*1600000);
gCP->FMemory_FrameSize[143]  = _FMemory_RoundEB(_AllocationChunkSize*1700000);
gCP->FMemory_FrameSize[144]  = _FMemory_RoundEB(_AllocationChunkSize*1800000);
gCP->FMemory_FrameSize[145]  = _FMemory_RoundEB(_AllocationChunkSize*1900000);
gCP->FMemory_FrameSize[146]  = _FMemory_RoundEB(_AllocationChunkSize*2000000);
gCP->FMemory_FrameSize[147]  = _FMemory_RoundEB(_AllocationChunkSize*2100000);
gCP->FMemory_FrameSize[148]  = _FMemory_RoundEB(_AllocationChunkSize*2200000);
gCP->FMemory_FrameSize[149]  = _FMemory_RoundEB(_AllocationChunkSize*2300000);
gCP->FMemory_FrameSize[150]  = _FMemory_RoundEB(_AllocationChunkSize*2400000);
gCP->FMemory_FrameSize[151]  = _FMemory_RoundEB(_AllocationChunkSize*2500000);
gCP->FMemory_FrameSize[152]  = _FMemory_RoundEB(_AllocationChunkSize*2600000);
gCP->FMemory_FrameSize[153]  = _FMemory_RoundEB(_AllocationChunkSize*2700000);
gCP->FMemory_FrameSize[154]  = _FMemory_RoundEB(_AllocationChunkSize*2800000);
gCP->FMemory_FrameSize[155]  = _FMemory_RoundEB(_AllocationChunkSize*2900000);
gCP->FMemory_FrameSize[156]  = _FMemory_RoundEB(_AllocationChunkSize*3000000);
gCP->FMemory_FrameSize[157]  = _FMemory_RoundEB(_AllocationChunkSize*3100000);
gCP->FMemory_FrameSize[158]  = _FMemory_RoundEB(_AllocationChunkSize*3200000);
gCP->FMemory_FrameSize[159]  = _FMemory_RoundEB(_AllocationChunkSize*3300000);
gCP->FMemory_FrameSize[160]  = _FMemory_RoundEB(_AllocationChunkSize*3400000);
gCP->FMemory_FrameSize[161]  = _FMemory_RoundEB(_AllocationChunkSize*3500000);
gCP->FMemory_FrameSize[162]  = _FMemory_RoundEB(_AllocationChunkSize*3600000);
gCP->FMemory_FrameSize[163]  = _FMemory_RoundEB(_AllocationChunkSize*3700000);
gCP->FMemory_FrameSize[164]  = _FMemory_RoundEB(_AllocationChunkSize*3800000);
gCP->FMemory_FrameSize[165]  = _FMemory_RoundEB(_AllocationChunkSize*3900000);
gCP->FMemory_FrameSize[166]  = _FMemory_RoundEB(_AllocationChunkSize*4000000);
gCP->FMemory_FrameSize[167]  = _FMemory_RoundEB(_AllocationChunkSize*4100000);
gCP->FMemory_FrameSize[168]  = _FMemory_RoundEB(_AllocationChunkSize*4200000);
gCP->FMemory_FrameSize[169]  = _FMemory_RoundEB(_AllocationChunkSize*4300000);
gCP->FMemory_FrameSize[170]  = _FMemory_RoundEB(_AllocationChunkSize*4400000);
gCP->FMemory_FrameSize[171]  = _FMemory_RoundEB(_AllocationChunkSize*4500000);
gCP->FMemory_FrameSize[172]  = _FMemory_RoundEB(_AllocationChunkSize*4600000);
gCP->FMemory_FrameSize[173]  = _FMemory_RoundEB(_AllocationChunkSize*4700000);
gCP->FMemory_FrameSize[174]  = _FMemory_RoundEB(_AllocationChunkSize*4800000);
gCP->FMemory_FrameSize[175]  = _FMemory_RoundEB(_AllocationChunkSize*4900000);
gCP->FMemory_FrameSize[176]  = _FMemory_RoundEB(_AllocationChunkSize*5000000);
gCP->FMemory_FrameSize[177]  = _FMemory_RoundEB(_AllocationChunkSize*5100000);
gCP->FMemory_FrameSize[178]  = _FMemory_RoundEB(_AllocationChunkSize*5200000);
gCP->FMemory_FrameSize[179]  = _FMemory_RoundEB(_AllocationChunkSize*5300000);
gCP->FMemory_FrameSize[180]  = _FMemory_RoundEB(_AllocationChunkSize*5400000);
gCP->FMemory_FrameSize[181]  = _FMemory_RoundEB(_AllocationChunkSize*5500000);
gCP->FMemory_FrameSize[182]  = _FMemory_RoundEB(_AllocationChunkSize*5600000);
gCP->FMemory_FrameSize[183]  = _FMemory_RoundEB(_AllocationChunkSize*5700000);
gCP->FMemory_FrameSize[184]  = _FMemory_RoundEB(_AllocationChunkSize*5800000);
gCP->FMemory_FrameSize[185]  = _FMemory_RoundEB(_AllocationChunkSize*5900000);
gCP->FMemory_FrameSize[186]  = _FMemory_RoundEB(_AllocationChunkSize*6000000);
gCP->FMemory_FrameSize[187]  = _FMemory_RoundEB(_AllocationChunkSize*6100000);
gCP->FMemory_FrameSize[188]  = _FMemory_RoundEB(_AllocationChunkSize*6200000);
gCP->FMemory_FrameSize[189]  = _FMemory_RoundEB(_AllocationChunkSize*6300000);
gCP->FMemory_FrameSize[190]  = _FMemory_RoundEB(_AllocationChunkSize*6400000);
gCP->FMemory_FrameSize[191]  = _FMemory_RoundEB(_AllocationChunkSize*6500000);
gCP->FMemory_FrameSize[192]  = _FMemory_RoundEB(_AllocationChunkSize*6600000);
gCP->FMemory_FrameSize[193]  = _FMemory_RoundEB(_AllocationChunkSize*6700000);
gCP->FMemory_FrameSize[194]  = _FMemory_RoundEB(_AllocationChunkSize*6800000);
gCP->FMemory_FrameSize[195]  = _FMemory_RoundEB(_AllocationChunkSize*6900000);
gCP->FMemory_FrameSize[196]  = _FMemory_RoundEB(_AllocationChunkSize*7000000);
gCP->FMemory_FrameSize[197]  = _FMemory_RoundEB(_AllocationChunkSize*7100000);
gCP->FMemory_FrameSize[198]  = _FMemory_RoundEB(_AllocationChunkSize*7200000);
gCP->FMemory_FrameSize[199]  = _FMemory_RoundEB(_AllocationChunkSize*7300000);													
gCP->FMemory_FrameSize[200]  = _FMemory_RoundEB(_AllocationChunkSize*7400000);
gCP->FMemory_FrameSize[201]  = _FMemory_RoundEB(_AllocationChunkSize*7500000);
gCP->FMemory_FrameSize[202]  = _FMemory_RoundEB(_AllocationChunkSize*7600000);
gCP->FMemory_FrameSize[203]  = _FMemory_RoundEB(_AllocationChunkSize*7700000);
gCP->FMemory_FrameSize[204]  = _FMemory_RoundEB(_AllocationChunkSize*7800000);
gCP->FMemory_FrameSize[205]  = _FMemory_RoundEB(_AllocationChunkSize*7900000);
gCP->FMemory_FrameSize[206]  = _FMemory_RoundEB(_AllocationChunkSize*8000000);
gCP->FMemory_FrameSize[207]  = _FMemory_RoundEB(_AllocationChunkSize*8100000);
gCP->FMemory_FrameSize[208]  = _FMemory_RoundEB(_AllocationChunkSize*8200000);
gCP->FMemory_FrameSize[209]  = _FMemory_RoundEB(_AllocationChunkSize*8300000);													
gCP->FMemory_FrameSize[210]  = _FMemory_RoundEB(_AllocationChunkSize*8400000);
gCP->FMemory_FrameSize[211]  = _FMemory_RoundEB(_AllocationChunkSize*8500000);
gCP->FMemory_FrameSize[212]  = _FMemory_RoundEB(_AllocationChunkSize*8600000);
gCP->FMemory_FrameSize[213]  = _FMemory_RoundEB(_AllocationChunkSize*8700000);
gCP->FMemory_FrameSize[214]  = _FMemory_RoundEB(_AllocationChunkSize*8800000);
gCP->FMemory_FrameSize[215]  = _FMemory_RoundEB(_AllocationChunkSize*8900000);
gCP->FMemory_FrameSize[216]  = _FMemory_RoundEB(_AllocationChunkSize*9000000);

oldFrameSize = gCP->FMemory_FrameSize[216];
for (n = 217; n < (_MEMMAXFRAMES-1); ++n)
    {
    newFrameSize = _FMemory_RoundEB(_AllocationChunkSize*(9000000+((n-216)*1000000)));
	if (newFrameSize <= oldFrameSize) break;
    gCP->FMemory_FrameSize[n] = newFrameSize;
    oldFrameSize = newFrameSize;
    }
if (gCP->FMemory_FrameSize[n-1] < gCP->FMemory_FrameSize[n])
	gCP->FMemory_FrameMaxIndex = n;
else
	gCP->FMemory_FrameMaxIndex = --n;

#if 0
/*
Allocate memory and dead blocks to accomodate the memory allocations received from OS
Notes:
Each element of Funcs._Host_memBlockPtr[] can contain a pointer to memory allocated by the OS
Each element of Funcs._Host_memBlockSize[] contains an allocated memory size.
We need to allocate memory blocks for each memory block allocated by the OS.
We also need to allocate fixed memory blocks for each "gap" (dead blocks) between these OS
supplied blocks so that memory in these gaps will never be used by the memory manager.
Note that we do NOT zero out the dead blocks.

Step 1:
Collapse all adjacent _Host_memBlockPtr[] elements into a new list of OSBlock[]. Don't mess
with _HostBlockPtr directly.

Consider the following diagram describing block relationships:
Note that all memory blocks are contiguious.
Condition 1 - first allocation
  OS Blocks         Memory Blocks		Rule Notes
   _____________       ___________       
  |             |     |///////////|     Block[0].Start >= OSBlock[0].Start + gCP, gTP, Stack etc.
  | OSBlock[0]  |     | Block     |
	...                 ...             See other conditions for block end rules

Condition 2 - allocating into last OSBlock
                       ___________      
   _____________      |///////////|     
  | OSBlock[j]  |     | Block[i]  |     Block[i].End <= (OSBlock[j].Start + OSBlock[j].Length)
  |             |     |___________|
  |____________ |    


Condition 3 - allocating a memory block and a dead block
   _____________       ___________
  |             |     |///////////|		
  | OSBlock[j]  |     | Block[i]  |		Block[i].End <= ((OSBlock[j].Start + OSBlock[j].Length) - HeaderSize)
  |             |     |___________|
  |-------------|     |///////////|		Block[i+1].End >= OSBlock[j+1].Start 
  | Dead        |     | Block[i+1]|		This block is marked fixed and used.
  |-------------|     |___________|		It is never dereferenced so it is never garbage collected.
  | OSBlock[j+1]|     
  |             |     
  |_____________|   

*/
#endif
/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_Init, copy OS blocks from Host_memBlocks\r\n");	*/ /* NDEBUG */

/* Clear and then copy OS blocks from Funcs._Host_memBlockSize into OSBlock[] */
for(j=0; j < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++j)
	{
	OSBlock[j].Start = NULL;
	OSBlock[j].Length = 0;
	Block[j] = NULL; /* clear the Block array as well */
	}

j = 0;
i = 1;
OSBlock[0].Start = Funcs._Host_memBlockPtr[0];
OSBlock[0].Length = Funcs._Host_memBlockSize[0];
gCP->FMemory_ParentSize = OSBlock[0].Start + OSBlock[0].Length - gCP->FMemory_ParentPtr;
while((i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS) && (Funcs._Host_memBlockPtr[i] != NULL))
	{
	if ((Funcs._Host_memBlockPtr[i] + Funcs._Host_memBlockSize[i]) != Funcs._Host_memBlockPtr[i + 1])
		OSBlock[++j].Start = Funcs._Host_memBlockPtr[i]; /* Note ++j */
	OSBlock[j].Length += Funcs._Host_memBlockSize[i];
	gCP->FMemory_ParentSize = (OSBlock[j].Start + OSBlock[j].Length) - gCP->FMemory_ParentPtr;
	i++;
	}

/* Adjust Block length to account for gCP,gTP, heap and stack allocations from the first OS memory block */
OSBlock[0].Length = OSBlock[0].Length - (NUM)(gCP->FMemory_ParentPtr - OSBlock[0].Start);
/* Adjust Block start to FMemory_ParentPtr (this is where the memory manager blocks should start) */
OSBlock[0].Start = gCP->FMemory_ParentPtr;

i = 0; /* i is index into Block[] */
for (j=0, k=1; k < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++j, ++k) /* j is index into OSBlock[], k == (j + 1) */
	if (OSBlock[j].Start == NULL)
		break; /* no more blocks to process - first NULL block terminates loop*/
	else
		{ /* memory block allocation */
		if (OSBlock[k].Start == NULL) /* Allocating into last OSBlock */
			{
			if (i == 0) /* Allocating Block[0] */
				objSize = OSBlock[j].Length - SIZEOF_MHANDLEINFO - _AllocationChunkSize;
			else /* Not allocating first memory block so use end of previous memory block in calculation of new block size */
				objSize = (OSBlock[j].Start + OSBlock[j].Length) - SIZEOF_MHANDLEINFO - _AllocationChunkSize - (gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated);
			/* Create new memory block */
			Block[i++] = (LpMHANDLEINFO)FMemory_New(gCP,gTP,objSize,TRUE);
			}
		else /* Allocating a new memory block and a dead block */
			{
			if (i == 0) /* Block[0] */
				objSize = OSBlock[j].Length - (2 * SIZEOF_MHANDLEINFO) - _AllocationChunkSize;
			else /* Not allocating first memory block so use end of previous memory block in calculation of new block size */
				objSize = (OSBlock[j].Start + OSBlock[j].Length) - (2 * SIZEOF_MHANDLEINFO) - _AllocationChunkSize - (gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated);

			/* Create new memory block */
			Block[i++] = (LpMHANDLEINFO)FMemory_New(gCP,gTP,objSize,TRUE);

			/* Create memory block across dead block */
			objSize = OSBlock[k].Start - (gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated);
			Block[i] = (LpMHANDLEINFO)FMemory_New(gCP,gTP,objSize,FALSE);
			Block[i]->Fixed = TRUE;
			gCP->FMemory_BlockedBlockCount++;
			gCP->FMemory_BlockedMemoryBytes += _FMemory_fullSizeOf(Block[i]);
			}
		} /* end of memory block allocation */

/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_Init, Free all allocated blocks.\r\n");	*/ /* NDEBUG */
/* Free all allocated blocks (except dead blocks) */
/* We can recognize dead blocks because they are the only blocks marked FIXED at this point */
/* This has the effect of placing the blocks on the frames free list */
for (i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i)
	if (Block[i] != NULL && (Block[i]->Fixed == FALSE))
		{
		FMemory_Free(gCP,gTP,(HMemory)Block[i]);
		}
/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_Init, Done.\r\n");	*/ /* NDEBUG */
}

#if 0
/* _FMemory_BlockCheck() performs a scan of the current memory blocks to ensure no errors are present 
*/
#endif
#if 0
#define _FMemory_BlockCheck \
{ LpMHANDLEINFO scanHandle = (LpMHANDLEINFO)gCP->FMemory_ParentPtr; \
long totalUsed = 0; \
while (scanHandle && (scanHandle < (LpMHANDLEINFO)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated))) \
    { \
	assert(!_BADHANDLE(scanHandle)); \
    assert(!((scanHandle->Free == FALSE) && ((NUM)scanHandle->fwd != (NUM)scanHandle->data))); \
	if (scanHandle->Free == FALSE) totalUsed += _FMemory_fullSizeOf(scanHandle); \
	assert(!((scanHandle->next == NULL) && (scanHandle != gCP->FMemory_PrevPtr))); \
    scanHandle = scanHandle->next; \
    } \
assert(totalUsed == gCP->FMemory_UsedMemoryBytes); \
}
#else
#define _FMemory_BlockCheck 
#endif

/*--------------------------------------------------------------------------------------- */
#if 0
/*
_FMemory_frameUnlink    MACRO VERSION

If this block is on a free list then unlink it. This procedure also takes responsibility to 
increment the block count by one and increment the usedbytes by the appropriate quantity.
*/
#endif

#define     _FMemory_frameUnlink(b)															\
{																							\
LpMHANDLEINFO  block, back, fwd;															\
NUM blockSize = 0L;																		\
block = (LpMHANDLEINFO)b;																	\
back = block->back;																			\
fwd = (LpMHANDLEINFO)(block->fwd);															\
if(back && fwd ) { back->fwd = (POINTER)fwd; fwd->back = (struct mh*)back; }				\
else if( fwd ) { gCP->FMemory_FrameNextMHandle[block->Frame] = fwd; fwd->back = NULL; }		\
else if( back ) { back->fwd = (POINTER)NULL; }												\
else {  gCP->FMemory_FrameNextMHandle[block->Frame] = NULL; }								\
block->fwd = (POINTER)block->data;															\
block->back = NULL;																			\
block->Free = FALSE;																		\
blockSize = _FMemory_fullSizeOf(block);														\
gCP->FMemory_UsedBlockCount++;																\
gCP->FMemory_UsedMemoryBytes +=  blockSize;													\
gCP->FMemory_FreeBlockCount--;																\
gCP->FMemory_FreeMemoryBytes -= blockSize;													\
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_memset  FUNCTION VERSION

Zero this block
*/
#endif

void FMemory_memset(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR ptr, NUM fill, NUM length)
{                       
register LpNUM  block;
register LpCHAR stop, final;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
block = (LpNUM)(ptr);
final = (LpCHAR)(ptr);
final += (NUM)(length);
if((NUM)(length) > 4)
    {
    stop = final - 4;
    while ((LpCHAR)block < stop) {*(block++) = (NUM)(fill);}
    }
else
    stop = (LpCHAR)(ptr);
while (stop < final) {*(stop++) = (NUM)(fill);}
}


/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_memcpy  FUNCTION VERSION

Copy this block
*/
#endif
void FMemory_memcpy(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR to, LpCHAR from, NUM length)
{                                                   
register LpNUM  dst;
register LpNUM  src;
register LpCHAR srcByte, final, dstByte;
register NUM    chunk;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
srcByte = (LpCHAR)from;
src = (LpNUM)from;
final = srcByte + (NUM)length;
dst = (LpNUM)to;
chunk = (NUM)length >> BITSIZEOFNUM;
while(chunk-- > 0) *dst++ = *src++;
dstByte = (LpCHAR)dst; srcByte = (LpCHAR)src;
while (srcByte < final) {*(dstByte++) = *(srcByte++);}
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
_FMemory_GetFrame   MACRO VERSION

The FMemory_GetFrame MACRO locates the frame for the argument size. Each of the
memory manager`s frames links memory HANDLES of a specific size.
*/
#endif
// TLW - Change low,mid,high to integers.  Avoids disaster when low = 0, high = 1 and compare < 0 which causes
// high = mid -1 to wrap around to the largest positive unsigned.
#define     _FMemory_GetFrame(requestedBlockSize, result)				\
{                                                                       \
register NUM    low = 0, mid = 0, high = gCP->FMemory_FrameMaxIndex;	\
register NUM compare = 0;													\
if (requestedBlockSize >= 0)                                            \
    {                                                                   \
    while ( low <= high )                                               \
        {                                                               \
        mid = (low + high) >> 1; /* divide by 2 */                      \
        compare =  requestedBlockSize - gCP->FMemory_FrameSize[mid];	\
        if ( compare < 0 ) high = mid - 1;                              \
        else                                                            \
        if ( compare > 0 ) low = mid + 1;                               \
        else  break;                                                    \
       }                                                                \
    if ( compare > 0 )                                                  \
        result = mid + 1;                                               \
    else                                                                \
        result = mid;                                                   \
    }                                                                   \
else                                                                    \
    result = high;                                                      \
}
/* TLW - Add function-version for debugging */
NUM FMemory_GetFrame(LpXCONTEXT gCP, NUM requestedBlockSize)
{
	NUM low = 0, mid = 0, high = gCP->FMemory_FrameMaxIndex, result;
	NUM compare = 0;
	if (requestedBlockSize >= 0)
	{	while ( low <= high )
		{	mid = (low + high) >> 1; /* divide by 2 */
			compare =  requestedBlockSize - gCP->FMemory_FrameSize[mid];
			if ( compare < 0 )
				high = mid - 1;
			else if ( compare > 0 )
				low = mid + 1;
			else
				break;
		}
		if ( compare > 0 )
			result = mid + 1;
		else
			result = mid;
	}
	else
		result = high;
	return result;
}                                                                                                                                                                                                                                                         
/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_New
Arguments:
	objectSize
	ZeroOut			Zero out memory?
Create a new memory block and return a pointer to it's memory block header.
*/
#endif

HMemory     FMemory_New(LpXCONTEXT gCP, LpTHREAD gTP, NUM objSize, BOLE ZeroOut)
{
NUM				userSize = 0;
NUM             excessBytes;
NUM             frameIndex = 0;
NUM             nextIndex;
NUM             excessIndex;
NUM             frameSize;
LpMHANDLEINFO   resultHandle;
LpMHANDLEINFO   excess;
BOLE            triedGC = FALSE;
BOLE            triedCompact = FALSE;
CHAR			buf[256];
TVAL			prmv;

_DEF_TIMER
_SEED_TIMER
_START_TIMER

/* Save the original number of bytes requested. */
if (objSize < 0) goto Failure;
userSize = objSize;
objSize += SIZEOF_MHANDLEINFO; /* Add header size to users requested memory */
/* Calculate even header multiples */
objSize = (((objSize+_AllocationChunkSize-1)/_AllocationChunkSize)*_AllocationChunkSize);
if ((objSize < 0) || (objSize > gCP->FMemory_FrameSize[gCP->FMemory_FrameMaxIndex])) goto Failure;

/*  Locate a frame large enough to hold the requested size. */
_FMemory_GetFrame(objSize, frameIndex); /* This macro finds a frame with the size (or next larger size) to objSize */


/*  If there is no frame large enough to accomodate the    */
/*  requested memory, generate an error					*/
if (frameIndex > gCP->FMemory_FrameMaxIndex)
	{
	/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_New Memory allocation failure!\r\n");	*/ /* NDEBUG */
	Failure:
    sprintf((char*)buf, "!!!Memory Allocation Failure!!!");
    (*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
    sprintf((char*)buf, "!!!Unable to allocate memory block of = "INTFORMAT" bytes!!!", objSize);
    (*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
    sprintf((char*)buf, "Memory allocation statistics at time of failure:");
    (*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);

    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_MEMORY_REQUEST_TOO_LARGE);
	}
/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_New Calling FMemory_FrameSize\r\n");	*/ /* NDEBUG */
frameSize = gCP->FMemory_FrameSize[frameIndex]; /* frameSize may be bigger than objSize */

TryAgain:


/*	Each FMemory_FrameNextMHandle[frameIndex] element points to the first */
/*	free block for the frame size associated with the frameIndex. */
/*  If there is a free memory block of the selected frame size, */
/*  pull this block off the frame's free list. Mark this block */
/*  as used, and return the pointer to its handle info. */
/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_New Calling FMemory_FrameNextMHandle\r\n");	*/ /* NDEBUG */
if (gCP->FMemory_FrameNextMHandle[frameIndex] != NULL)
    {
    resultHandle = gCP->FMemory_FrameNextMHandle[frameIndex];
	if (_FMemory_fullSizeOf(resultHandle) < objSize)
		{ /* It is an error for a block smaller than the frame size to be associated with that frame */
		sprintf((char*)buf, "!!!Memory Allocation Failure!!!");
		(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
		sprintf((char*)buf, "!!!Frame "INTFORMAT" free block list corrupt.!!!",frameIndex);
		(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_MEMORY_REQUEST_TOO_LARGE);
		}
	gCP->FMemory_FreeListHitCount++;
	_FMemory_frameUnlink(resultHandle);   /* FMemory_frameUnlink manages FMemory_UsedBlockCount and FMemory_UsedMemoryBytes. */
   	goto ReturnAnyExcessSpace; /* This will reclaim memory if objSize < frameSize */
    }
/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_New Try larger and smaller frames\r\n");	*/ /* NDEBUG */
/*  If there are no free memory block and we do NOT have enough  */
/*  room to create a new memory block then we try larger frames */
/*  and smaller frames to see if any other frames contain blocks */
/*  large enough for us to use. If we DO have enough room to */
/*  create a new memory block, then we will bypass this code. */

if (((gCP->FMemory_MemoryAllocated+objSize) > gCP->FMemory_ParentSize) || ((gCP->FMemory_MemoryAllocated+objSize) < 0))
    { 

	/*  Try a larger frame, if no unallocated memory available */
/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_New Try a larger frame\r\n");	*/ /* NDEBUG */
    nextIndex = frameIndex;
    while ((++nextIndex < gCP->FMemory_FrameMaxIndex) && (gCP->FMemory_FrameNextMHandle[nextIndex] == NULL)) {}
    
    if (nextIndex < gCP->FMemory_FrameMaxIndex)  
        {
		/*	We will use the free frame even though it may be larger than needed. */					
		/*  Note:	FMemory_frameUnlink manages FMemory_UsedBlockCount and FMemory_UsedMemoryBytes. */
		/* A memory block may not be smaller than the frameSize of the frame it is associated with. */
		resultHandle = gCP->FMemory_FrameNextMHandle[nextIndex];											
		if (_FMemory_fullSizeOf(resultHandle) < objSize)
			{ /* It is an error for a block smaller than the frame size to be associated with that frame */
			sprintf((char*)buf, "!!!Memory Allocation Failure!!!");
			(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
			sprintf((char*)buf, "!!!Frame "INTFORMAT" free block list corrupt.!!!",frameIndex);
			(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_MEMORY_REQUEST_TOO_LARGE);
			}

		gCP->FMemory_LargerFrameHitCount++;
		/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_New Calling _FMemory_frameUnlink\r\n"); */	/* NDEBUG */

		_FMemory_frameUnlink(resultHandle);   /* FMemory_frameUnlink manages FMemory_UsedBlockCount and FMemory_UsedMemoryBytes and block->free = FALSE. */
		
		/*	We will attempt to return any excess space if this block is larger than needed. */					
		ReturnAnyExcessSpace:
        /* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_New(), Return excess space...\r\n"); */	/* NDEBUG */
        excessBytes = (_FMemory_fullSizeOf(resultHandle) - objSize); /* remember that objSize includes allowance for memory header */
        
		/*  If the memory block is marked "fixed", then we will never resize it. */
		/* Remember that the framesizes include enough room for a memory header in addition to data */
       if ((resultHandle->Fixed == TRUE) || (excessBytes < gCP->FMemory_FrameSize[0]))
            {
            /*  Use the whole block. ( No excess large enough to care about ). */
			/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_New Use the whole block\r\n");	*/ /* NDEBUG */
            if (_BADHANDLE(resultHandle) || (_FMemory_fullSizeOf(resultHandle) < objSize))
                FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
	        resultHandle->userSize = userSize;
			if (ZeroOut == TRUE)
				_FMemory_memset(resultHandle->data,0,userSize);
		
			gCP->FMemory_ReuseCount++;


			_POST_TIMER(gCP->FMemory_NewTime);
			_FMemory_BlockCheck
			return((HMemory)resultHandle); /* Block fixed or not enough excess to reclaim */
            }
	   else /*  This is where we take a nibble of the requested size, and free the rest. */
            { 
			/*  Since this block is larger than the one which we requested, */						
			/*  we will unlink it, break it into two pieces */										
				/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_New Unlink block and break it into two pieces.\r\n"); */	/* NDEBUG */
            _FMemory_GetFrame(excessBytes, excessIndex); /* excessIndex now contains the frameIndex for frames having a frameSize >= excessBytes */
            excessIndex -= ( excessIndex && excessBytes < gCP->FMemory_FrameSize[excessIndex] ); 
                        
            /*  Get a pointer to the new block we will create from the excessBytes */
            excess = (LpMHANDLEINFO)((NUM)resultHandle + objSize); /* excess points to first byte past what we need in current block */
            
            /*  Relink the sequential pointers, and setup the block header. */
			/*  Note: The if makes sure we only set the next->prev iff the */						
			/*        resultHandle->next is NOT zero; otherwise, we do not */						
			/*        set next->prev, because there is no next!! */									
            
#ifdef MEMTEST
			excess->marker = _MARKERVALUE; 
#endif
            if ((excess->next = resultHandle->next) != 0)
                excess->next->prev = excess;
            resultHandle->next = excess;
            excess->prev = resultHandle;
			excess->userSize = 0;																	
            
            excess->Free = FALSE;		/* excess block is freed lower down in this code block */											
            excess->Fixed = FALSE;																	
            excess->Frame = excessIndex;
            excess->fwd = (POINTER)excess->data;
			excess->back = NULL;																		
            
            /*  Set the pointer to the last allocated memory block (free or otherwise) */
            
            if (excess->next == NULL)
                gCP->FMemory_PrevPtr = excess;
			
            if (_BADHANDLE(excess))
			{	/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_New Bad Excess handle\r\n");	*/		/* NDEBUG */
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
			}
            /*  Update the frame index and clear the memory for the newly allocated block. */
            
			_FMemory_GetFrame(objSize, frameIndex); 
			frameIndex -= ( frameIndex && objSize < gCP->FMemory_FrameSize[frameIndex] ); 
            resultHandle->Frame = frameIndex;
            resultHandle->Fixed = FALSE;

			if (_BADHANDLE(resultHandle))
                FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
				
			if (_FMemory_fullSizeOf(resultHandle) != objSize)
                FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

	        resultHandle->userSize = userSize;
			if (ZeroOut == TRUE)
				_FMemory_memset(resultHandle->data,0,userSize);
 	
			/*  Since we are splitting this block in two pieces the block count goes up, */			
            /*  but the used space is the same because we already allocated resultHandle. */		
            /*  Then we free the excess block and all block counts and used space are correct.*/	
            
            gCP->FMemory_UsedBlockCount ++;																
			/* gCP->FMemory_UsedMemoryBytes += 0; */

			gCP->FMemory_SplitCount++;

			_STOP_TIMER
          /*  Free the excess portion. (Takes care of block count etc.) */
  			FMemory_Free(gCP, gTP, (HMemory)excess);															
			_START_TIMER

			_POST_TIMER(gCP->FMemory_NewTime);
			_FMemory_BlockCheck            
           return((HMemory)resultHandle); /* took a nibble and freed the rest */
            }
        }
    else /* nextIndex < _gCP->FMemory_FrameMaxIndex */
    if(frameIndex && gCP->FMemory_FrameNextMHandle[frameIndex - 1] != NULL)
        {
        /*  As a last resort we will examine the truesize of all blocks associated with */
        /*  frames having a frameSize smaller than the objSize requested and see if one */
		/* would meet the allocation request. */

        resultHandle = gCP->FMemory_FrameNextMHandle[frameIndex - 1];
        while(resultHandle != NULL)
            {
            if (_FMemory_fullSizeOf(resultHandle) >= objSize)
                { /* We found one! */
                _FMemory_frameUnlink(resultHandle);
                if (_BADHANDLE(resultHandle) || (_FMemory_fullSizeOf(resultHandle) < objSize))
                    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
				goto ReturnAnyExcessSpace;
                }
            resultHandle = (LpMHANDLEINFO)resultHandle->fwd;
            }
        } 
        
    /*  We're out of luck, and have no memory left */
    
    if(triedGC == FALSE)
        {
        triedGC = TRUE;
		++gCP->FMemory_myForcedGCCounter;
        FUtil1_Gc(gCP,gTP,0, (LpTVAL)NULL);
        goto TryAgain;
        }
	else
    if(triedCompact == FALSE)
        {
        triedCompact = TRUE;
		++gCP->FMemory_myForcedGCCounter;
		strcpy(&prmv.u.Text[0],"compact");
		prmv.Tag = TYTEXT;
        FUtil1_Gc(gCP,gTP,1,(LpTVAL)&prmv);
        goto TryAgain;
        }

	if (((gCP->FMemory_ParentSize - gCP->FMemory_UsedMemoryBytes) / objSize) > 4)
		{
		sprintf((char*)buf, "!!!Memory Allocation Failure!!!");
		(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
		sprintf((char*)buf, "!!!Unable to allocate memory block of = "INTFORMAT" bytes!!!", objSize);
		(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
		sprintf((char*)buf, "Memory allocation statistics at time of failure:");
		(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);

		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_OUT_OF_MEMORY_FRAGMENTED);
		}
	else
		{
		sprintf((char*)buf, "!!!Memory Allocation Failure!!!");
		(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
		sprintf((char*)buf, "!!!Unable to allocate memory block of = "INTFORMAT" bytes!!!", objSize);
		(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
		sprintf((char*)buf, "Memory allocation statistics at time of failure:");
		(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);

		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_OUT_OF_MEMORY);
		}
    }

/*  If there is no free memory block, and we get here, then we have */
/*  enough room to create a new memory block along with a handle */
/*  describing the block. Mark this block as used. Set the new block */
/*  to not fixed and return the pointer to its handle info. */
/* DEBUG FSmartbase_Log(gCP,gTP,"FMemory_New Create a new memory block and mark it as used\r\n"); */ /* NEBUG */

if ((gCP->FMemory_MemoryAllocated+objSize) > gCP->FMemory_FrameSize[gCP->FMemory_FrameMaxIndex]) goto Failure;
resultHandle = (LpMHANDLEINFO)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated);

if (gCP->FMemory_PrevPtr != NULL)
    {
    gCP->FMemory_PrevPtr->next = resultHandle;
    if (_BADHANDLE(gCP->FMemory_PrevPtr))
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
    }

frameIndex -= ( frameIndex && objSize < gCP->FMemory_FrameSize[frameIndex] ); 

resultHandle->prev  = gCP->FMemory_PrevPtr;
gCP->FMemory_PrevPtr = resultHandle;

#ifdef MEMTEST
resultHandle->marker = _MARKERVALUE;
#endif

resultHandle->back  = NULL;
resultHandle->next  = NULL;

resultHandle->fwd   = (POINTER)resultHandle->data;
resultHandle->Frame = frameIndex;
resultHandle->Free  = FALSE;
resultHandle->Fixed = FALSE;
frameSize = gCP->FMemory_FrameSize[frameIndex];

gCP->FMemory_MemoryAllocated += objSize;

gCP->FMemory_UsedBlockCount++;
gCP->FMemory_UsedMemoryBytes += objSize;

if (!_VALIDHANDLE(resultHandle))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

if (_FMemory_fullSizeOf(resultHandle) < gCP->FMemory_FrameSize[((LpMHANDLEINFO)resultHandle)->Frame])
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

if (_BADHANDLE(resultHandle))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

if (_FMemory_fullSizeOf(resultHandle) < objSize)
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);


resultHandle->userSize = userSize;
if (ZeroOut == TRUE)
	{
	_FMemory_memset(resultHandle->data,0,userSize);
	}

_POST_TIMER(gCP->FMemory_NewTime);
_FMemory_BlockCheck
return((HMemory)resultHandle); /* allocated a new memory block */
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_PreAllocateFixedBlock
Create one or more fixed memory blocks and return true if successful or error code on failure.
Arguments:
	BlockSize	
	BlockCount		
*/
#endif

TVAL    FMemory_PreAllocateFixedBlock(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM				maxCount = 400;
LpMHANDLEINFO   self[400];
NUM				objSize;
NUM				objCount;
NUM				indexOf;

/*  This class must be initialized. */
if (!gCP->FMemory_Initialized) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_OUT_OF_MEMORY);

/*  We must get an object size and a count argument */
if ((argc != 2) || !isNumIndex(&argv[0]) || !isNumIndex(&argv[1])) 
	return(gCP->TObject_ERROR_INVALID_ARGLIST);

objSize = asNumIndex(&argv[0]);
objCount = asNumIndex(&argv[1]);

if ((objCount < 1) || (objCount >= maxCount)) 
	return(gCP->TObject_ERROR_INVALID_ARGLIST);

/*  Allocate main memory for each new fixed block. */
for (indexOf = 0; indexOf < objCount; ++indexOf)
	{ /* Note that FMemory_new can not return an error. An allocation error will cause a FSmartbase_throw */
	self[indexOf] = (LpMHANDLEINFO)FMemory_New(gCP, gTP, objSize,TRUE);
	self[indexOf]->Fixed = TRUE; /* Make sure blocks are never merged or split */
	}

/* Call FMemory_Free on each fixed block. */
/* This has the effect of placing the block headers on the frames free block list. */
/* However, the blocks will not be merged, as might normally happen, because they are */
/* marked as fixed blocks. */
for (indexOf = 0; indexOf < objCount; ++indexOf)
	{
	FMemory_Free(gCP, gTP, (HMemory)self[indexOf]);
	}

return(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_NewFixedBlock
Create a new fixed memory block and return a pointer to it's memory block header.
TM 05/27/04 - changed name from FMemory_NewFixedPtr to Fmemory_NewFixedBlock
			- set the Fixed flag (this function was not called before so this bug was not seen)
*/
#endif

void*   FMemory_NewFixedBlock(LpXCONTEXT gCP, LpTHREAD gTP, NUM objSize)
{
LpMHANDLEINFO	self;

/*  This class must be initialized. */
if (!gCP->FMemory_Initialized) 
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_OUT_OF_MEMORY);

/*  Allocate main memory for the new fixed pointer. */
self = (LpMHANDLEINFO)FMemory_New(gCP, gTP, objSize,TRUE);
self->Fixed = TRUE;	/* TM 05/27/04 Added so block is actually fixed */
return((void*)self);
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_SizeOf
Arguments:
	self	Pointer to the memory block header of an allocation.
Return the maximum size of a memory block's data area.
*/
#endif

NUM FMemory_SizeOf(LpXCONTEXT gCP, LpTHREAD gTP, HMemory self)
{
NUM             blockSize;
LpMHANDLEINFO   theHandle;

/*  Make sure the handle is not NULL. */

if (self == NULL)
    { 
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
    return(0);
    }

/*  If the handle is not valid, treat is as a Macintosh handle. */

if (!_VALIDHANDLE(self))
    { 
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
    return(0);
    }

theHandle = ((LpMHANDLEINFO)self);
blockSize = _FMemory_dataSizeOf(theHandle);

return(blockSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_Resize
	self	Pointer to the memory block header of an allocation.
	newSize	New size for block's data area.
Reset the maximum size of a memory block and return the new actual size.
This routine may merge blocks, split the current block or allocate a new block as necessary.
*/
#endif

HMemory     FMemory_Resize(LpXCONTEXT gCP, LpTHREAD gTP, HMemory self,NUM newSize)
{
NUM					oldUserSize;
NUM					userSize;
LpMHANDLEINFO       oldHandle;
LpMHANDLEINFO       newHandle;
LpMHANDLEINFO       excess;
NUM                 excessBytes;
NUM                 excessIndex;
NUM                 newIndex;
NUM                 newFrameSize;
NUM                 newFrameIndex;
NUM                 oldFrameIndex;
NUM                 oldSize;
NUM                 moveLength;
CHAR				buf[256];

NUM					sizeDifference;
NUM					sizeOfNext;
LpMHANDLEINFO		nextBlock;
LpMHANDLEINFO		oldNext;


_DEF_TIMER
_SEED_TIMER
_START_TIMER

/*  Make sure the handle is not NULL. */
if (self == NULL)
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

/*  If the handle is not valid, treat is as a Macintosh handle. */
if (_BADHANDLE(self))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
    
/* Save old and new user requested sizes. */
oldHandle = (LpMHANDLEINFO)self;
oldUserSize = oldHandle->userSize;
oldSize = _FMemory_fullSizeOf(oldHandle);
oldFrameIndex = oldHandle->Frame;

userSize = newSize;
newSize += SIZEOF_MHANDLEINFO; 
/* Calculate even header multiples */
newSize = (((newSize+_AllocationChunkSize-1)/_AllocationChunkSize)*_AllocationChunkSize);

/* Use existing block if the oldSize and newSize are the same */
if (oldSize == newSize)
	{ /* The only thing that changes is the userSize! */
	gCP->FMemory_ReuseCount++;
	oldHandle->userSize = userSize;
	_POST_TIMER(gCP->FMemory_ResizeTime);
	return((HMemory)oldHandle);
	}

/* Locate a frame large enough to hold the requested size */
_FMemory_GetFrame(newSize, newFrameIndex); /* returns a newFrameIndex with frameSize >= newSize */

/*  If there is no frame large enough to accomodate the    */
/*  requested memory, generate an error					*/
if (newFrameIndex > gCP->FMemory_FrameMaxIndex)
	{
    sprintf((char*)buf, "!!!Memory Allocation Failure!!!");
    (*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
    sprintf((char*)buf, "!!!Unable to allocate memory block of = "INTFORMAT" bytes!!!", newSize);
    (*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
    sprintf((char*)buf, "Memory allocation statistics at time of failure:");
    (*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);

    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_MEMORY_REQUEST_TOO_LARGE);
	}

newFrameSize = gCP->FMemory_FrameSize[newFrameIndex];

/*  If the frameIndex of both old and new sizes are the same, then we */
/*  do a very simple resize in place. This is very important for speed. */
/*  In addition, We want to catch not only exact matches by frame but */
/* close enough matches on oddblocks as well. Clear the new excess memory */
/* to all zeros. */
if (oldFrameIndex == newFrameIndex || (newFrameIndex > oldFrameIndex && oldSize > newSize))
    {
    oldHandle->userSize = userSize;
	if (userSize > oldUserSize)
		_FMemory_memset((LpMHANDLEINFO)((NUM)oldHandle->data + oldUserSize),0,userSize - oldUserSize);

	gCP->FMemory_ReuseCount++;
	
	_POST_TIMER(gCP->FMemory_ResizeTime);
	_FMemory_BlockCheck
	return((HMemory)oldHandle); /* resized in place */
    }

/* If the curent block is fixed and smaller than what wee need then must resize by getting a different block */
if (oldHandle->Fixed == TRUE)
	goto GetNewHandle;

/*  Try to grow this block taking space from the following memory block(s) (if they are free) */
if (newSize > oldSize)
	{ 
	/* See if subsequent free un-fixed block is available to */
	/* grow the current block in place. Note that all blocks we grow */
	/* into have to be marked free and not fixed. We only check the */
	/* next block because the FMemory_Free routine should have consolidated */
	/* any adjacent free blocks into a single free block. There should never */
	/* be any adjacent free blocks. */
	nextBlock = oldHandle->next;
	sizeDifference = (newSize - oldSize);
	if ( (nextBlock != NULL)
		&& (_FMemory_fullSizeOf(nextBlock) >= sizeDifference) 
		&& (nextBlock->Free == TRUE) 
		&& (nextBlock->Fixed == FALSE)) 
		{ /* We have enough memory in subsequent block */ 
		sizeOfNext = _FMemory_fullSizeOf(nextBlock);

		excessBytes = sizeOfNext - sizeDifference;
		/* Are we using part or all of the block nextBlock? */
		if ( excessBytes < gCP->FMemory_FrameSize[0]) /* not enough excess to worry about */ 
			{ /* Using full block so just merge */
			_FMemory_frameUnlink(nextBlock);
		    
			gCP->FMemory_UsedBlockCount--;
		    
			if ((oldHandle->next = nextBlock->next) != 0)
				oldHandle->next->prev = oldHandle;
		    
			/*  Set the pointer to the last allocated memory block (free or otherwise) */
		    if (oldHandle->next == NULL)
				gCP->FMemory_PrevPtr = oldHandle;

			gCP->FMemory_JoinCount++;

			/* See final fixup after if/else block */
			}
		else /* excessBytes  >= gCP->FMemory_FrameSize[0] */
			{ /* Using only part of the block nextBlock so do a split */
			_FMemory_frameUnlink(nextBlock); /* Remove frame from the free block list of the frame it is associated with */

			_FMemory_GetFrame(excessBytes, excessIndex);
			excessIndex -= ( excessIndex && excessBytes < gCP->FMemory_FrameSize[excessIndex] );
		    
			oldNext = nextBlock->next; /* save to avoid overwrite */
			excess = (LpMHANDLEINFO)((NUM)oldHandle + newSize);

#ifdef MEMTEST
			excess->marker = _MARKERVALUE; 
#endif

			excess->Fixed = FALSE;
			excess->Frame = excessIndex;
			excess->prev = oldHandle;
			excess->next = oldNext;
			excess->back = NULL;
			excess->fwd = (POINTER)excess->data;
			excess->Free = FALSE;
		    
			/* We fixup the surrounding block headers to show the resizing. */
			if (excess->next != NULL)
				excess->next->prev = excess;
			oldHandle->next = excess;

			/*  Set the pointer to the last allocated memory block (free or otherwise) */
			if (excess->next == NULL)
				gCP->FMemory_PrevPtr = excess;
		
			if (_BADHANDLE(excess))
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

			_STOP_TIMER
			/*  Free will drop the block count and usedbytes appropriately. */
			FMemory_Free(gCP, gTP, (HMemory)excess); 
			_START_TIMER

			/* See final fixup below */
			} /* end of using only part of nextBlock */

			/* Final fixup for block expansion */
			_FMemory_GetFrame(newSize, newIndex);
			newIndex -= ( newIndex && newSize < gCP->FMemory_FrameSize[newIndex] ); 
			oldHandle->Frame = newIndex;

			if (_BADHANDLE(oldHandle))
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

			oldHandle->userSize = userSize;
			/*  Clear the new memory */
			if (userSize > oldUserSize)
				_FMemory_memset((LpMHANDLEINFO)((NUM)oldHandle->data + oldUserSize),0,userSize - oldUserSize);

		gCP->FMemory_SplitCount++;
		gCP->FMemory_JoinCount++;

		_POST_TIMER(gCP->FMemory_ResizeTime);
		_FMemory_BlockCheck
		return((HMemory)oldHandle); /* resized by growing into next block */
		} /* end of extend current block */ 
	else 
		goto GetNewHandle; /* go get a new block because we can't expand the current block */
	} /* end of newSize > oldSize && oldHandle->Fixed == FALSE */
else
/*  Try to shrink this block giving the excess space to a new block, */
/*  which we set free and insert between this block and the next memory block */
/*  Note1: If this memory block is marked "fixed", then we will never drop space to it. */
/*  Note2: The new block may be merged with a following free block by FMemory_Free. */
if ((oldHandle->Fixed == FALSE) && ((oldSize - newSize) >= gCP->FMemory_FrameSize[0]))
    {
    /* Compute the location of the new excess memory block. */
    excess = (LpMHANDLEINFO)((NUM)oldHandle + newSize);

#ifdef MEMTEST
	excess->marker = _MARKERVALUE; 
#endif
	
	_FMemory_GetFrame((oldSize - newSize), excessIndex);
    excessIndex -= ( excessIndex && (oldSize - newSize) < gCP->FMemory_FrameSize[excessIndex] );
    
    /* Compute the new size of the downsized memory block. */
    _FMemory_GetFrame(newSize, newIndex);
    newIndex -= ( newIndex && newSize < gCP->FMemory_FrameSize[newIndex] );
    oldHandle->Frame = newIndex;
    
    /*  Set the pointer to the last allocated memory block (free or otherwise) */
    if (gCP->FMemory_PrevPtr == oldHandle)
        gCP->FMemory_PrevPtr = excess;
        
    /*  Relink in the newly created block. */
	if ((excess->next = oldHandle->next) != 0)
       excess->next->prev = excess;
    excess->prev = oldHandle;
    oldHandle->next = excess;
    excess->Free = FALSE;
    excess->Fixed = FALSE;
    excess->fwd = (char*)excess->data;
    excess->back = NULL;
    excess->Frame = excessIndex;

    /*  Since we are splitting this original block in two pieces the blockcount goes up. */
    gCP->FMemory_UsedBlockCount++;
            
    if (_BADHANDLE(excess))
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

	_STOP_TIMER
    /*  Free will drop the block count and usedbytes appropriately. */
	FMemory_Free(gCP, gTP, (HMemory)excess);
	_START_TIMER

    if (_BADHANDLE(oldHandle))
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

	oldHandle->userSize = userSize;

	gCP->FMemory_SplitCount++;

	_POST_TIMER(gCP->FMemory_ResizeTime);
	_FMemory_BlockCheck
	return((HMemory)oldHandle); /* resized by spliting current block */
    }

/*  Find a new handle of the requested size. */

GetNewHandle:

_STOP_TIMER
if ((newHandle = (LpMHANDLEINFO)FMemory_New(gCP, gTP, newSize,TRUE)) == NULL)
	{
	sprintf((char*)buf, "!!!Memory Allocation Failure!!!");
	(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
    sprintf((char*)buf, "!!!Unable to allocate memory block of = "INTFORMAT" bytes!!!", newSize);
    (*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
	sprintf((char*)buf, "Memory allocation statistics at time of failure:");
	(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);

    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_OUT_OF_MEMORY);
	}
_START_TIMER

/*  Copy the data from the old handle to the new handle. */
moveLength = (oldUserSize < userSize) ? oldUserSize : userSize;
_FMemory_memcpy(newHandle->data,oldHandle->data,moveLength);

_STOP_TIMER
/*  Drop the old handle and replace with the new handle. */
FMemory_Free(gCP, gTP, (HMemory)oldHandle); 
_START_TIMER
newHandle->userSize = userSize;

/*  Clear the new memory */
if (userSize > oldUserSize)
	_FMemory_memset((LpMHANDLEINFO)((NUM)newHandle->data + oldUserSize),0,userSize - oldUserSize);

_POST_TIMER(gCP->FMemory_ResizeTime);
_FMemory_BlockCheck
return((HMemory)newHandle); /* resized by allocating new block */
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_Copy
Arguments
	self	Pointer to the current memory blocks header.
Copy the data of a memory block and return a pointer to the the new memory blocks header.
*/
#endif

HMemory     FMemory_Copy(LpXCONTEXT gCP, LpTHREAD gTP, HMemory self)
{
LpMHANDLEINFO       oldHandle;
LpMHANDLEINFO       newHandle;
NUM                 oldSize;

/*  Make sure the handle is not NULL. */

if (self == NULL)
    { 
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
    return(0);
    }

/*  If the handle is not valid, treat it as a Macintosh handle. */

if (!_VALIDHANDLE(self))
    { 
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
    return(NULL);
    }
oldHandle = (LpMHANDLEINFO)self;
oldSize = oldHandle->userSize;

/*  Find a new handle of the requested size. */

if ((newHandle = (LpMHANDLEINFO)FMemory_New(gCP, gTP, oldSize,TRUE)) == NULL)
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_OUT_OF_MEMORY);

/*  Copy the data from the old handle to the new handle. */

_FMemory_memcpy(newHandle->data,oldHandle->data,oldSize);

gCP->FMemory_CopyCount++;

/*  Return the new handle. */

return((HMemory)newHandle);
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_Free
	self	Pointer to a memory block header.
Free a memory block. Place a memory block in a frame's free blocks list.
This routine will merge free blocks. There should never two adjacent free blocks.
*/
#endif

void    FMemory_Free(LpXCONTEXT gCP, LpTHREAD gTP, HMemory self)
{
NUM             oldSize, newSize;
NUM             frameIndex;
LpMHANDLEINFO   prev;
LpMHANDLEINFO   next;
LpMHANDLEINFO   oldHandle;
LpMHANDLEINFO   tmp;
LpMHANDLEINFO   tmpBck;

_DEF_TIMER
_SEED_TIMER
_START_TIMER

/*  Return if the handle is NULL. */
if (self == NULL) 
    return;

/*  If the handle is not valid, treat it as a Macintosh handle. */
if (_BADHANDLE(self))																				
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);														

oldHandle = (LpMHANDLEINFO)self;

/*  Return if the handle cannot be freed for any reason. */
if ((oldHandle == NULL) || (oldHandle->Free == TRUE) || (oldHandle->fwd != (char*)oldHandle->data)) 
    return;

oldSize = _FMemory_fullSizeOf(oldHandle);
newSize = oldSize;
prev = oldHandle->prev;
next = oldHandle->next;

/*  Set the pointer to the last allocated memory block (free or otherwise) */
if (oldHandle->next == NULL)
    gCP->FMemory_PrevPtr = oldHandle;

/*  Try to merge this block with the previous memory block (if it is free) */
/*  Note: If either this memory block or the previous memory block are */
/*        marked "fixed", they will never be merged together. */
if ((prev != NULL) && (prev->Free == TRUE) && (prev->Fixed == FALSE) && (oldHandle->Fixed == FALSE))
    {
    /*  If the previous block in memory is currently free then we will remove it from */
    /*  its free list so that we may merge it with the block which is being returned. */
     _FMemory_frameUnlink(prev);

	/* Bump freeMemoryBytes counter because frameUnlink will have decremented this value */
	 gCP->FMemory_FreeMemoryBytes += _FMemory_fullSizeOf(prev);
	 gCP->FMemory_UsedMemoryBytes -= _FMemory_fullSizeOf(prev);
    
    /*  Used FMemory_UsedBlockCount changes as a result of merging two blocks. */
    gCP->FMemory_UsedBlockCount--;

	gCP->FMemory_JoinCount++;
    
    /*  We must fix the link list which maintains the sequential ordering of all blocks in */
    /*  contiguous memory. */
    newSize += _FMemory_fullSizeOf(prev);
    
    /*  Set the pointer to the last allocated memory block (free or otherwise) */
    prev->next = next;
    if (next != NULL)
        next->prev = prev;
    else
        gCP->FMemory_PrevPtr = prev;
        
    /*  Very important that we reset oldHandle here so that  the logic will work when  */
    /*  we try to merge with the next contiguous block in memory. */
    oldHandle = prev;
    }

/*  Try to merge this block with the next memory block (if it is free) */
/*  Note: If either this memory block or the next memory block are */
/*        marked "fixed", they will never be merged together. */
if ((next != NULL) && (next->Free == TRUE) && (next->Fixed == FALSE) && (oldHandle->Fixed == FALSE))
    {   
    /*  If the next block in memory is currently free then we will remove it from */
    /*  its free list so that we may merge it with the block which is being returned. */
    _FMemory_frameUnlink(next);

	/* Bump freeMemoryBytes counter because frameUnlink will have decremented this value */
	 gCP->FMemory_FreeMemoryBytes += _FMemory_fullSizeOf(next);
	 gCP->FMemory_UsedMemoryBytes -= _FMemory_fullSizeOf(next);
    
    /*  Used FMemory_UsedBlockCount changes as a result of merging two blocks. */
     gCP->FMemory_UsedBlockCount--;

	gCP->FMemory_JoinCount++;

	/*  We must fix the link list which maintains the sequential ordering of all blocks in */
    /*  contiguous memory. */
    oldHandle->next = next->next;
    
    newSize += _FMemory_fullSizeOf(next);
    oldHandle->next = next->next;
    
    /*  Set the pointer to the last allocated memory block (free or otherwise) */
    if (next->next != NULL)
        next->next->prev = oldHandle;
    else
        gCP->FMemory_PrevPtr = oldHandle;
    }

/*  Locate the appropriate free handle list for this memory block. Note that since   */
/*  FMemory_GetFrame may give us the next larger block than we want we might have to */
/*  adjust downward and place this block on the PREVIOUS free list.                  */
_FMemory_GetFrame( newSize, frameIndex );
frameIndex -= ( frameIndex && newSize < gCP->FMemory_FrameSize[frameIndex] );

/*  Now add this (possibly merged) block back into the appropriate free list. */
/*  Note: We leave the free list in ascending sorted order by block address.  */
if(gCP->FMemory_FrameNextMHandle[frameIndex] == NULL)
    {
    /* Manage case where free handle list is empty. */
	oldHandle->fwd = NULL;
	oldHandle->back = NULL;
	gCP->FMemory_FrameNextMHandle[frameIndex] = oldHandle;
    }
else
    {
    /* Manage case where free handle list is not empty. */
    tmp = gCP->FMemory_FrameNextMHandle[frameIndex];
    
    /* Find the correct spot to insert the new handle into the */
    /* free handle list in ascending sort order by block address. */
    while ((oldHandle > tmp) && (tmp->fwd != NULL))
		{
		tmp = (LpMHANDLEINFO)tmp->fwd;
		}  
    
    /* Insert this block into the free handle list. */
    if (tmp == gCP->FMemory_FrameNextMHandle[frameIndex])
		{
		/* Manage case where we are inserting at head of free handle list. */
		tmp->back = oldHandle;
		oldHandle->fwd = (POINTER)tmp;
		oldHandle->back = NULL;
		gCP->FMemory_FrameNextMHandle[frameIndex] = oldHandle;
		}
    else if (tmp->fwd == NULL)
		{
		/* Manage case where we are inserting at tail of free handle list. */
		tmp->fwd = (POINTER)oldHandle;
		oldHandle->fwd = NULL;
		oldHandle->back = tmp;
		}
	else
		{
		/* Manage case where we are inserting inside the free handle list. */
		tmpBck = tmp->back;
		tmp->back = oldHandle;
		oldHandle->fwd = (POINTER)tmp;
		oldHandle->back = tmpBck;
		tmpBck->fwd = (POINTER)oldHandle;
		}
    }

/*  Mark the handle as free. */
oldHandle->Frame = frameIndex;
oldHandle->Free = TRUE;
oldHandle->userSize = 0;

/*  Finally we drop the used block and usedbyte counts, for the old piece. */
gCP->FMemory_UsedBlockCount--;
gCP->FMemory_UsedMemoryBytes -= oldSize;

/* Bump the Free counts */
gCP->FMemory_FreeBlockCount++;
gCP->FMemory_FreeMemoryBytes += oldSize;

_POST_TIMER(gCP->FMemory_FreeTime);
_FMemory_BlockCheck
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_FreeSpace
Return the maximum available host memory currently available.
*/
#endif

NUM FMemory_FreeSpace(LpXCONTEXT gCP, LpTHREAD gTP)
{
gTP = gTP; // NOOP to hide unused parameter warning message
return(gCP->FMemory_FreeMemoryBytes - (gCP->FMemory_FreeBlockCount * SIZEOF_MHANDLEINFO));
}

// TLW - Add a memory sanity check that can be used during the initialization process.
void    FMemory_CheckMemoryBlocks(LpXCONTEXT gCP,LpTHREAD gTP)
{
	LpMHANDLEINFO   scanHandle;

	gTP = gTP; // NOOP to hide unused parameter warning message
	/* Perform self tests on the current memory configuration.

	_FMemory_memset(used, 0, sizeof(used));
	_FMemory_memset(free, 0, sizeof(free));
	_FMemory_memset(usedBytes, 0, sizeof(usedBytes));
	_FMemory_memset(userBytes, 0, sizeof(userBytes));
	_FMemory_memset(freeBytes, 0, sizeof(freeBytes)); */

	/*  Scan through memory and check each memory block for errors. */
	scanHandle = (LpMHANDLEINFO)gCP->FMemory_ParentPtr;
	while (scanHandle != NULL && (scanHandle < (LpMHANDLEINFO)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated)))
	{	/*  Examine each allocated memory block and check its statistics. */
		/*  Note:   Check the each memory handle for validity. */
		if (_BADHANDLE(scanHandle))
		{	_FSMTBASE_LOG("CheckMemoryBlocks, BadHandle\r\n")
			FMemory_DumpMemoryBlocks(gCP, gTP);
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
		}
		/* Check for overwrite of header from previous block */
	#ifdef MEMTEST
		else if (scanHandle->marker != _MARKERVALUE)
		{	_FSMTBASE_LOG("CheckMemoryBlocks, Marker overwritten\r\n")
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
		}
	#endif
		/*  Check the current memory handle for overwrite. */
		else if ((scanHandle->Free == FALSE) && ((NUM)scanHandle->fwd != (NUM)scanHandle->data))
		{	_FSMTBASE_LOG("CheckMemoryBlocks, fwd not pointing to data\r\n")
			FMemory_DumpMemoryBlocks(gCP, gTP);
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
		}
		/*  Make sure last handle is really the last handle. */
		else if ((scanHandle->next == NULL) && (scanHandle != gCP->FMemory_PrevPtr))
		{	_FSMTBASE_LOG("CheckMemoryBlocks, Last scanHandle not pointing to FMemory_PrevPtr\r\n")
			FMemory_DumpMemoryBlocks(gCP, gTP);
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);
		}
		scanHandle = scanHandle->next;
	}
}

// Display all the memory handle structures
void    FMemory_DumpMemoryBlocks(LpXCONTEXT gCP, LpTHREAD gTP)
{
	LpMHANDLEINFO   sH;		// scanHandle
	LpMHANDLEINFO   scanEnd = (LpMHANDLEINFO)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated);
	char aMsg[4096];
	
gTP = gTP; // NOOP to hide unused parameter warning message

	for (sH = (LpMHANDLEINFO)gCP->FMemory_ParentPtr; sH != NULL && sH < scanEnd; sH = sH->next)
	{	//  Note the pertinent values in the memory header
#if MEMTEST
		sprintf(aMsg, "sH=%p,fwd=%p,marker=%8x,back=%p,next=%p,prev=%p,userSize=%9ld,Frame=%3d,Free=%d,Fixed=%d,data=%lx\r\n",
		sH, sH->fwd, sH->marker, sH->back, sH->next, sH->prev, sH->userSize, sH->Frame, sH->Free, sH->Fixed, sH->data[0]);
#else
		sprintf(aMsg, "sH=%p,fwd=%p,back=%p,next=%p,prev=%p,userSize=%9ld,Frame=%3d,Free=%d,Fixed=%d,data=%lx\r\n",
		sH, sH->fwd, sH->back, sH->next, sH->prev, sH->userSize, sH->Frame, sH->Free, sH->Fixed, sH->data[0]);
#endif
		_FSMTBASE_LOG(aMsg)
	}
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FMemory_SystemDiagnostic

Display the allocations according to frame size.

Note:	An unpublished argument to this diagnostics function is a second boolean
		argument. If set to true, this turns on automatic memory system self checking.
		If set to false, this turns off automatic memory system self checking.
		For example:

		(memstat false true)	;; Turns on memory self checking
		(memstat false false)	;; Turns off memory self checking
*/
#endif


TVAL    FMemory_SystemDiagnostic(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
BOLE            doPrint = TRUE;
LpMHANDLEINFO   scanHandle;
NUM				n;
NUM				objIndex;
NUM             wastedFree;
NUM             wastedUsed;
NUM             totalFree;
NUM				totalUsedBytes;
NUM				totalUserBytes;
NUM				totalFreeBytes;
NUM				blockSize;
NUM				frameIndex;
NUM             delta;
NUM             cn;
NUM             used[_MEMMAXFRAMES];
NUM				usedBytes[_MEMMAXFRAMES];
NUM				userBytes[_MEMMAXFRAMES];
NUM             free[_MEMMAXFRAMES];
NUM				freeBytes[_MEMMAXFRAMES];
CHAR            buf[256];
CHAR            buf1[256];
TObject*		obj;
TVAL            ret;
TVAL            left;
TVAL            right;
TVAL			tmpTval;
LpCHAR			oldStr;
LpCHAR			newStr;
NUM				indexOf;
TByteVector*	bvec;
TSymbol*        aSymbol;
TVAL			prmv[3];
/* Perform self tests on the current memory configuration. */

_FMemory_memset(used, 0, sizeof(used));
_FMemory_memset(free, 0, sizeof(free));
_FMemory_memset(usedBytes, 0, sizeof(usedBytes));
_FMemory_memset(userBytes, 0, sizeof(userBytes));
_FMemory_memset(freeBytes, 0, sizeof(freeBytes));

wastedFree = 0;
wastedUsed = 0;
totalFree = 0;
totalUsedBytes = 0;
totalUserBytes = 0;
totalFreeBytes = 0;
++gCP->FMemory_SystemCheckCount;

/*  Do we print memory usage statistics? */

if ((argc >= 1) && (argv[0].Tag == TYBOLE) && (argv[0].u.Bool == FALSE))
    doPrint = FALSE;

/*  Do we turn on/off memory self checking? */
if ((argc == 2) && (argv[1].Tag == TYBOLE) && (argv[1].u.Bool == TRUE))
    gCP->FMemory_SelfTest = TRUE;
else
if ((argc == 2) && (argv[1].Tag == TYBOLE) && (argv[1].u.Bool == FALSE))
    gCP->FMemory_SelfTest = FALSE;

/*  Scan through memory and check each memory block for errors. */
scanHandle = (LpMHANDLEINFO)gCP->FMemory_ParentPtr;
while ((scanHandle != NULL) && (scanHandle < (LpMHANDLEINFO)(gCP->FMemory_ParentPtr + gCP->FMemory_MemoryAllocated)))
    {
    /*  Examine each allocated memory block and check its statistics. */
    /*  Note:   Check the each memory handle for validity. */
    if (_BADHANDLE(scanHandle))
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

#ifdef MEMTEST
	/* Check for overwrite of header from previous block */
	assert(scanHandle->marker == _MARKERVALUE);
#endif

	/*  Collect statistics for each used and each free memory block. */
	blockSize = _FMemory_fullSizeOf(scanHandle);
	frameIndex = scanHandle->Frame;
    if (scanHandle->Free != NIL)
        {
        free[frameIndex]++;
		freeBytes[frameIndex] += blockSize;
		totalFreeBytes += blockSize;
        }
    else
        {
		usedBytes[frameIndex] += blockSize;
		userBytes[frameIndex] += scanHandle->userSize;
        used[frameIndex]++;
		totalUserBytes += blockSize;
		totalUsedBytes += blockSize;
        }
    
    /*  Check the current memory handle for overwrite. */
    if ((scanHandle->Free == FALSE) && ((NUM)scanHandle->fwd != (NUM)scanHandle->data))
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

    /*  Make sure last handle is really the last handle. */
    if ((scanHandle->next == NULL) && (scanHandle != gCP->FMemory_PrevPtr))
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

    scanHandle = scanHandle->next;
    }
    
/*  Check for suspicious memory loss. */
if (totalUsedBytes != gCP->FMemory_UsedMemoryBytes)
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

/*  Scan through the object vector and check each object for errors. */
for (objIndex = 1; objIndex < gCP->TObject_MaxObjectCount; ++objIndex)
    {
	/* Only used objects are checked for errors. */
    if (_TObject_ObjectFlag(objIndex) != _TObject_OfVOID)
        {
		/* Make sure all used objects pass minimal diagnostics. */
		obj = _TObject_ObjectByIndex(objIndex);
		if (!_VALIDOBJ(obj))
			FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);

		/* Make sure all objects with memory handles pass minimal diagnostics. */
		switch (obj->itsObjectType)
			{
			case TYLAMBDA:
				if ((((TLambda*)obj)->itsImmediatePtr != NULL) ||
				    ((POINTER)((TLambda*)obj)->itsNilArray != (POINTER)&((TLambda*)obj)->itsImmediatePtr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!(_CHECKOBJ(((TLambda*)obj)->ClassVariables) &&
				      _CHECKOBJ(((TLambda*)obj)->ArgumentVariables) &&
				      _CHECKOBJ(((TLambda*)obj)->TemporaryVariables) &&
				      _CHECKOBJ(((TLambda*)obj)->PersistantVariables) &&
				      _CHECKOBJ(((TLambda*)obj)->ConstantVariables) &&
				      _CHECKOBJ(((TLambda*)obj)->RegisterVariables) &&
				      _CHECKOBJ(((TLambda*)obj)->PcodeVector) &&
				      _CHECKOBJ(((TLambda*)obj)->DebuggerSource) &&
				      _CHECKOBJ(((TLambda*)obj)->NativeCodeVector) &&
				      _CHECKOBJ(((TLambda*)obj)->Interfaces) &&
				      _CHECKOBJ(((TLambda*)obj)->VirtualMachine)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYOBJREPOSITORY:
				if ((((TDatabase*)obj)->itsImmediatePtr != NULL) ||
				    ((POINTER)((TDatabase*)obj)->itsNilArray != (POINTER)&((TDatabase*)obj)->itsImmediatePtr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!(_CHECKOBJ(((TDatabase*)obj)->itsFileName) &&
				      _CHECKOBJ(((TDatabase*)obj)->itsOdbID) &&
				      _CHECKOBJ(((TDatabase*)obj)->itsIndex) &&
				      _CHECKOBJ(((TDatabase*)obj)->itsBufferKeys) &&
				      _CHECKOBJ(((TDatabase*)obj)->itsBufferValues) &&
				      _CHECKTVAL(((TDatabase*)obj)->itsCodeKey))
					)
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYCPX:
				if ((((TCpx*)obj)->itsImmediatePtr != (POINTER)&((TCpx*)obj)->itsReal) ||
				    ((POINTER)((TCpx*)obj)->itsNilArray != (POINTER)&((TCpx*)obj)->itsImmediatePtr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				break;
			case TYCONTINUATION:
				if ((((TContinuation*)obj)->itsImmediatePtr != NULL) ||
				    ((POINTER)((TContinuation*)obj)->itsNilArray != (POINTER)&((TContinuation*)obj)->itsImmediatePtr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				break;
			case TYPAIR:
				if ((((TPair*)obj)->itsImmediatePtr != (POINTER)&((TPair*)obj)->itsCar) ||
				    ((POINTER)((TPair*)obj)->itsNilArray != (POINTER)&((TPair*)obj)->itsImmediatePtr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if ((!_CHECKTVAL(((TPair*)obj)->itsCar)) ||
				    (!_CHECKTVAL(((TPair*)obj)->itsCdr)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				break;
			case TYVECTOR:
				if ((((TVector*)obj)->itsMaxItemIndex > 0) &&
				    (((TVector*)obj)->itsImmediatePtr == NULL) &&
				    ((((TVector*)obj)->itsTvalArray == NULL) || 
					 (((LpMHANDLEINFO)((TVector*)obj)->itsTvalArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				/* Make sure the object content is valid */
				for (n = 0; n < ((TVector*)obj)->itsMaxItemIndex; ++n)
					{
					if (!_CHECKTVAL(((LpTVAL)*((TVector*)obj)->itsTvalArray)[n]))
						{
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
						}
					}
				if ((!_CHECKTVAL(((TVector*)obj)->itsCdr)) ||
				    (!_CHECKOBJ(((TVector*)obj)->itsAttributes)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYMATRIX:
				if ((((TMatrix*)obj)->itsMaxItemIndex > 0) &&
				    (((TMatrix*)obj)->itsImmediatePtr == NULL) &&
				    ((((TMatrix*)obj)->itsTvalMatrix == NULL) || 
					 (((LpMHANDLEINFO)((TMatrix*)obj)->itsTvalMatrix)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				/* Make sure the object content is valid */
				for (n = 0; n < ((TMatrix*)obj)->itsMaxItemIndex; ++n)
					{
					if (!_CHECKTVAL(((LpTVAL)*((TMatrix*)obj)->itsTvalMatrix)[n]))
						{
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
						}
					}
				if (!_CHECKTVAL(((TMatrix*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYNUMMATRIX:
				if ((((TNumMatrix*)obj)->itsMaxItemIndex > 0) &&
				    (((TNumMatrix*)obj)->itsImmediatePtr == NULL) &&
				    ((((TNumMatrix*)obj)->itsRealMatrix == NULL) || 
					 (((LpMHANDLEINFO)((TNumMatrix*)obj)->itsRealMatrix)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!_CHECKTVAL(((TNumMatrix*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;

			case TYPCODEVECTOR:
				if ((((TPcodeVector*)obj)->itsMaxItemIndex > 0) &&
				    (((TPcodeVector*)obj)->itsImmediatePtr == NULL) &&
				    ((((TPcodeVector*)obj)->itsInstructionArray == NULL) || 
					 (((LpMHANDLEINFO)((TPcodeVector*)obj)->itsInstructionArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!_CHECKTVAL(((TPcodeVector*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYDICTIONARY:
				if ((((TDictionary*)obj)->itsMaxItemIndex > 0) &&
				    (((TDictionary*)obj)->itsImmediatePtr == NULL) &&
				    ((((TDictionary*)obj)->itsDictionaryArray == NULL) || 
					 (((LpMHANDLEINFO)((TDictionary*)obj)->itsDictionaryArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				/* Make sure the keys are in ascending sorted order. */
				for (n = 0; n < ((TDictionary*)obj)->itsMaxItemIndex - 1; ++n)
					{
					left = TOBJ(((LpBIND)*((TDictionary*)obj)->itsDictionaryArray)[n].Key);
					right = TOBJ(((LpBIND)*((TDictionary*)obj)->itsDictionaryArray)[n+1].Key);
					if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) >= 0)
						{
						prmv[0].u.Object = obj;
						prmv[0].Tag = TYDICTIONARY;
						prmv[1].u.Pointer = (POINTER)&FPredicate_LT;
						prmv[1].Tag = TYCFUNCTION;
						FMath2_Sort(gCP,gTP,2,&prmv[0]);
						}
					}
				/* Make sure the object content is valid */
				for (n = 0; n < ((TDictionary*)obj)->itsMaxItemIndex; ++n)
					{
					if (!_VALIDOBJ(((LpBIND)*((TDictionary*)obj)->itsDictionaryArray)[n].Key) ||
						!_VALIDTVAL(((LpBIND)*((TDictionary*)obj)->itsDictionaryArray)[n].Value))
						{
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
						}
					}
				if (!_CHECKTVAL(((TDictionary*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYDIRECTORY:
				if ((((TDirectory*)obj)->itsMaxItemIndex > 0) &&
				    (((TDirectory*)obj)->itsImmediatePtr == NULL) &&
				    ((((TDirectory*)obj)->itsDirectoryArray == NULL) || 
					 (((LpMHANDLEINFO)((TDirectory*)obj)->itsDirectoryArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				/* Make sure the keys are in ascending sorted order. */
				for (n = 0; n < ((TDirectory*)obj)->itsMaxItemIndex - 1; ++n)
					{
					left = ((LpPBIND)*((TDirectory*)obj)->itsDirectoryArray)[n].Key;
					right = ((LpPBIND)*((TDirectory*)obj)->itsDirectoryArray)[n+1].Key;
					if (FPredicate2_QuickCompare(gCP,gTP,&left,&right) >= 0)
						{
						prmv[0].u.Object = obj;
						prmv[0].Tag = TYDIRECTORY;
						prmv[1].u.Pointer = (POINTER)&FPredicate_LT;
						prmv[1].Tag = TYCFUNCTION;
						FMath2_Sort(gCP,gTP,2,&prmv[0]);
						}
					}
				/* Make sure the object content is valid */
				for (n = 0; n < ((TDirectory*)obj)->itsMaxItemIndex; ++n)
					{
					if (!_VALIDTVAL(((LpPBIND)*((TDirectory*)obj)->itsDirectoryArray)[n].Key) ||
						!_VALIDTVAL(((LpPBIND)*((TDirectory*)obj)->itsDirectoryArray)[n].Value))
						{
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
						}
					}
				if (!_CHECKTVAL(((TDirectory*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYSTRUCTURE:
				if ((((TStructure*)obj)->itsMaxItemIndex > 0) &&
				    (((TStructure*)obj)->itsImmediatePtr == NULL) &&
				    ((((TStructure*)obj)->itsDictionaryArray == NULL) || 
					 (((LpMHANDLEINFO)((TStructure*)obj)->itsDictionaryArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				/* Make sure the object content is valid */
				for (n = 0; n < ((TStructure*)obj)->itsMaxItemIndex; ++n)
					{
					if (!_CHECKOBJ(((LpBIND)*((TStructure*)obj)->itsDictionaryArray)[n].Key) ||
						!_CHECKTVAL(((LpBIND)*((TStructure*)obj)->itsDictionaryArray)[n].Value))
						{
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
						}
					}
				if ((!_CHECKTVAL(((TStructure*)obj)->itsCdr)) ||
				    (!_CHECKOBJ(((TStructure*)obj)->itsMethods)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYINTVECTOR:
				if ((((TIntVector*)obj)->itsMaxItemIndex > 0) &&
				    (((TIntVector*)obj)->itsImmediatePtr == NULL) &&
				    ((((TIntVector*)obj)->itsIntArray == NULL) || 
					 (((LpMHANDLEINFO)((TIntVector*)obj)->itsIntArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!_CHECKTVAL(((TIntVector*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYSHORTVECTOR:
				if ((((TShtVector*)obj)->itsMaxItemIndex > 0) &&
				    (((TShtVector*)obj)->itsImmediatePtr == NULL) &&
				    ((((TShtVector*)obj)->itsShortArray == NULL) || 
					 (((LpMHANDLEINFO)((TShtVector*)obj)->itsShortArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!_CHECKTVAL(((TShtVector*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYLONGVECTOR:
				if ((((TLongVector*)obj)->itsMaxItemIndex > 0) &&
				    (((TLongVector*)obj)->itsImmediatePtr == NULL) &&
				    ((((TLongVector*)obj)->itsLongArray == NULL) || 
					 (((LpMHANDLEINFO)((TLongVector*)obj)->itsLongArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!_CHECKTVAL(((TLongVector*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYBYTEVECTOR:
				if ((((TByteVector*)obj)->itsMaxItemIndex > 0) &&
				    (((TByteVector*)obj)->itsImmediatePtr == NULL) &&
				    ((((TByteVector*)obj)->itsByteArray == NULL) || 
					 (((LpMHANDLEINFO)((TByteVector*)obj)->itsByteArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!_CHECKTVAL(((TByteVector*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYBITVECTOR:
				if ((((TBitVector*)obj)->itsMaxItemIndex > 0) &&
				    (((TBitVector*)obj)->itsImmediatePtr == NULL) &&
				    ((((TBitVector*)obj)->itsBitArray == NULL) || 
					 (((LpMHANDLEINFO)((TBitVector*)obj)->itsBitArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!_CHECKTVAL(((TBitVector*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYNUMVECTOR:
				if ((((TNumVector*)obj)->itsMaxItemIndex > 0) &&
				    (((TNumVector*)obj)->itsImmediatePtr == NULL) &&
				    ((((TNumVector*)obj)->itsRealArray == NULL) || 
					 (((LpMHANDLEINFO)((TNumVector*)obj)->itsRealArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!_CHECKTVAL(((TNumVector*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYFLTVECTOR:
				if ((((TFltVector*)obj)->itsMaxItemIndex > 0) &&
				    (((TFltVector*)obj)->itsImmediatePtr == NULL) &&
				    ((((TFltVector*)obj)->itsFloatArray == NULL) || 
					 (((LpMHANDLEINFO)((TFltVector*)obj)->itsFloatArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!_CHECKTVAL(((TFltVector*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYSYMBOL:
				if ((((TSymbol*)obj)->itsMaxItemIndex > 0) &&
				    (((TSymbol*)obj)->itsImmediatePtr == NULL) &&
				    ((((TSymbol*)obj)->itsCString == NULL) || 
					 (((LpMHANDLEINFO)((TSymbol*)obj)->itsCString)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if ((!_CHECKOBJ(((TSymbol*)obj)->itsVMEmulator)) ||
				    (!_CHECKOBJ(((TSymbol*)obj)->itsUserTypeParent)) ||
				    (!_CHECKOBJ(((TSymbol*)obj)->itsUserTypeFields)) ||
				    (!_CHECKOBJ(((TSymbol*)obj)->itsUserTypeMethods)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYSTRING:
				if ((((TString*)obj)->itsMaxItemIndex > 0) &&
				    (((TString*)obj)->itsImmediatePtr == NULL) &&
				    ((((TString*)obj)->itsCString == NULL) || 
					 (((LpMHANDLEINFO)((TString*)obj)->itsCString)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYERROR:
				if ((((TError*)obj)->itsMaxItemIndex > 0) &&
				    (((TError*)obj)->itsImmediatePtr == NULL) &&
				    ((((TError*)obj)->itsCString == NULL) || 
					 (((LpMHANDLEINFO)((TError*)obj)->itsCString)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYOBJVECTOR:
			case TYWORKSPACE:
				if ((((TObjVector*)obj)->itsMaxItemIndex > 0) &&
				    (((TObjVector*)obj)->itsImmediatePtr == NULL) &&
				    ((((TObjVector*)obj)->itsObjectArray == NULL) || 
					 (((LpMHANDLEINFO)((TObjVector*)obj)->itsObjectArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				/* Make sure the object content is valid */
				for (n = 0; n < ((TObjVector*)obj)->itsMaxItemIndex; ++n)
					{
					if (!_CHECKOBJ(((LpOBJ)*((TObjVector*)obj)->itsObjectArray)[n]))
						{
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
						}
					}
				if (!_CHECKTVAL(((TObjVector*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			case TYCPXVECTOR:
				if ((((TCpxVector*)obj)->itsMaxItemIndex > 0) &&
				    (((TCpxVector*)obj)->itsImmediatePtr == NULL) &&
				    ((((TCpxVector*)obj)->itsCpxArray == NULL) || 
					 (((LpMHANDLEINFO)((TCpxVector*)obj)->itsCpxArray)->Free == TRUE)))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				if (!_CHECKTVAL(((TCpxVector*)obj)->itsCdr))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
			break;
			}

        }
	
    }

/* Check for a corrupted symbol table. */
	
newStr = "";
if (gCP->TSymbol_SymbolTable != NULL) 
    { 
    for(cn = 0; cn < gCP->TSymbol_SymbolTable_ActiveCount; cn++)
        {
        /*  Examine each symbol within the symbol table. */
        
        aSymbol = (TSymbol*)(atHMObject(gCP->TSymbol_SymbolTable->itsObjectArray,cn));

		if (aSymbol != NULL)
			{
			/* The current symbol will be considered valid as long as it                                   */
			/* meets the following criteria:	                                                           */
			/*  (1) The object type must be TYSYMBOL                                                       */
			/*	(2) There can only be one copy of the symbol in the new Alphabetically sorted symbol table */
        
			if (!_VALIDOBJ(aSymbol) || 
				(aSymbol->itsObjectType != TYSYMBOL) ||
				((aSymbol->itsImmediatePtr == NULL) && ((aSymbol->itsCString == NULL) || (((LpMHANDLEINFO)aSymbol->itsCString)->Free == TRUE))))
				{
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				}
			else
			if ((aSymbol->itsObjectType == TYSYMBOL) &&
				((aSymbol->itsImmediatePtr != NULL) || 
				 ((aSymbol->itsCString != NULL) && (((LpMHANDLEINFO)(aSymbol)->itsCString)->Free != TRUE))))
				{
				oldStr = newStr;
				newStr = &atHMChar(aSymbol->itsCString,0);

				if ((cn > 0) && (strcmp(oldStr,newStr) >= 0))
					{
					FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
					}
				}
			else
			if ((aSymbol->itsObjectType != TYSYMBOL) || (aSymbol->itsMaxItemIndex > 0))
				{
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				}
			} /* end if */
        } /* end for */
    } /* end if */

/* ************************************************ */
/* Start of abbreviated mark and sweep system check */
/* ************************************************ */

/*  Unmark all objects. */
/*  Note:   The nil object is never garbage collected. */
for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    if (_TObject_ObjectByIndex(indexOf) == (TObject*)gCP->TSymbol_SymbolTable)
		{
		_TObject_ObjectFlag(indexOf) |= _TObject_OfMARK;
		}
	else
    if (_TObject_ObjectFlag(indexOf) != _TObject_OfVOID)
        {
        _TObject_ObjectFlag(indexOf) |= _TObject_OfMARK;
        _TObject_ObjectFlag(indexOf) ^= _TObject_OfMARK;
        }
    }
    
/*  Send Mark message to all debugger objects */
_TObject_RecordFrame
TObject_MarkObj(gCP,gTP,(TObject*)gTP->DebugBreakProc);
TObject_MarkTval(gCP,gTP,gTP->DebugBreakExp);
TObject_MarkTval(gCP,gTP,gTP->TObject_ErrorTree);
_TObject_CheckFrame 

/*  Send Mark message to all permanent objects, including all Symbol */
/*  objects which are either locked or which have non-void global values. */
for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    if (_TObject_ObjectFlag(indexOf) & _TObject_OfPERM)
        {
        _TObject_RecordFrame
        TObject_MarkObj(gCP,gTP,_TObject_ObjectByIndex(indexOf));
        _TObject_CheckFrame
        }
    else
    if ((_TObject_ObjectFlag(indexOf) & _TObject_OfOBJECT) &&
        (_TObject_ObjectByIndex(indexOf)->itsObjectType == TYSYMBOL))
        {
        if ((((TSymbol*)_TObject_ObjectByIndex(indexOf))->itsGlobalLock == TRUE) ||
            (((TSymbol*)_TObject_ObjectByIndex(indexOf))->itsGlobalValue.Tag != TYVOID) ||
            (((TSymbol*)_TObject_ObjectByIndex(indexOf))->itsCProcedure != (LpFUNC)TObject_NilFunction))
            {
            _TObject_RecordFrame
            TObject_MarkObj(gCP,gTP,_TObject_ObjectByIndex(indexOf));
            _TObject_CheckFrame
            }
        }
	else
	if ((_TObject_ObjectFlag(indexOf) & _TObject_OfOBJECT) &&
		(_TObject_ObjectByIndex(indexOf)->itsObjectType == TYOBJREPOSITORY))
		{
		/* If we have an open Repository, pointing to a disk database file, */
		/* with a transaction in progress, we do not want to garbage collect. */

		if (((TDatabase*)_TObject_ObjectByIndex(indexOf))->itsOdbID != NIL)
			{
			bvec = ((TDatabase*)_TObject_ObjectByIndex(indexOf))->itsOdbID;
			if (bvec->itsCdr.Tag == TYNUM)
				{
				_TObject_RecordFrame
				TObject_MarkObj(gCP,gTP,_TObject_ObjectByIndex(indexOf));
				_TObject_CheckFrame
				}
			}
		}
    }

/*  Send Mark message to all Global objects. */
for (indexOf = 0; indexOf < gCP->TObject_MaxTypes; ++indexOf)
    {
    if(_TObject_TypeGlobalMark(indexOf) != TObject_GlobalMarkNever)
        {
        _TObject_RecordFrame
        (*(LpFGMARK)_TObject_TypeGlobalMark(indexOf))(gCP,gTP);
        _TObject_CheckFrame
        }
    }

/*  Check for system stack or recursion overflow. */

if(gTP->ObjStackIdx >= gTP->MaxObjStackSize)
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_FRAME_ERROR);
else
if (((TopOfStack - 20) >= gTP->MaxTvalStackSize) || (TopOfStack < 0))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_STACK);

/*  Clear all items on the TVAL stack which are not in use */
/*  Send Mark messages to those which are still being used. */
for (indexOf = 0; indexOf < gTP->MaxTvalStackSize; ++indexOf)
    {
    tmpTval = gTP->TvalStack[indexOf];
    if (asTag(&tmpTval) != TYVOID && indexOf < TopOfStack)
        {
        _TObject_RecordFrame
        TObject_MarkTval(gCP,gTP,tmpTval);
        _TObject_CheckFrame
        }
    else
        gTP->TvalStack[indexOf] = gCP->Tval_VOID;
    }

/*  Send Mark message to all objects in the current frame. */
for (indexOf = 0; indexOf < gTP->ObjStackIdx; ++indexOf)
    {
    _TObject_RecordFrame
    TObject_MarkObj(gCP,gTP,_TObject_FramedObject(indexOf));
    _TObject_CheckFrame
    }

/*  Send mark messages to all bound methods in the type table. */

for (indexOf = 0; indexOf < gCP->TObject_MaxTypes; ++indexOf)
    {
    if (_TObject_TypeMethods(indexOf).Tag != TYVOID)
        {
        _TObject_RecordFrame
        TObject_MarkTval(gCP,gTP,_TObject_TypeMethods(indexOf));
        _TObject_CheckFrame
        }
    }

/*  Send mark messages to all structure fields in the type table. */

for (indexOf = 0; indexOf < gCP->TObject_MaxTypes; ++indexOf)
    {
    if (_TObject_TypeFields(indexOf).Tag != TYVOID)
        {
        _TObject_RecordFrame
        TObject_MarkTval(gCP,gTP,_TObject_TypeFields(indexOf));
        _TObject_CheckFrame
        }
    }

/* ********************************************** */    
/* End of abbreviated mark and sweep system check */
/* ********************************************** */    

/*  Display the memory usage statistics. */
/* Note: Minimum Bytes is frameSize * frameBlocks */
if (doPrint == TRUE)
    {
	sprintf((char*)buf,  "Frame    Frame       Free    Minimum  Allocated   Max User       Used    Minimum  Allocated       User");
	sprintf((char*)buf1, "Index     Size     Blocks      Bytes      Bytes      Bytes     Blocks      Bytes      Bytes      Bytes");
                      /* "[000] 00000000     000000 0000000000 0000000000 0000000000     000000 0000000000 0000000000 0000000000" */   
	(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
	(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf1, 1);

    for(cn = 0; cn < gCP->FMemory_FrameMaxIndex; cn++)
        {

        sprintf((char*)buf, "[%3ld] %8ld     %6ld %10ld %10ld %10ld", 
			cn, 
			gCP->FMemory_FrameSize[cn], 
			free[cn],free[cn] * gCP->FMemory_FrameSize[cn],  /* Minimum Bytes */
			freeBytes[cn], /* Allocated bytes in free blocks */
			freeBytes[cn] - (free[cn] * SIZEOF_MHANDLEINFO) /* Maximum user bytes */
			);

        delta = used[cn] * gCP->FMemory_FrameSize[cn];
        sprintf((char*)buf1, "     %6ld %10ld %10ld %10ld",
			used[cn], 
			used[cn] * gCP->FMemory_FrameSize[cn], /* Minimum Bytes */
			usedBytes[cn], /* Allocated bytes */
			userBytes[cn]
			);

        if (free[cn] || used[cn])
            {
            strcat((char*)buf, (const char*)buf1);
            (*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
            }
        }
    
	/* Display the heap usage statistics. */
    sprintf((char*)buf, "Total heap memory = "INTFORMAT", heap memory used = "INTFORMAT" ("INTFORMAT" percent), heap memory free = "INTFORMAT".", 
		totalUsedBytes + totalFreeBytes, totalUsedBytes, max(1,(totalUsedBytes/((totalUsedBytes + totalFreeBytes)/100))),totalFreeBytes);
    (*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);

	/* Display the object usage statistics. */

	sprintf((char*)buf, "Max Objects = %8ld,  objects used = %8ld ("INTFORMAT" percent).", 
		gCP->TObject_MaxObjectCount, gCP->TObject_UsedObjectCount, max(1,(gCP->TObject_UsedObjectCount/(gCP->TObject_MaxObjectCount/100))));
	(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);

	/* Display the stack usage statistics. */

	sprintf((char*)buf, "Max recursions = %8ld, current recursions = %8ld ("INTFORMAT" percent).", 
		gTP->MaxRecursions, gTP->RecursionCount, max(1,(gTP->RecursionCount/(gTP->MaxRecursions/100))));
	(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);

	sprintf((char*)buf, "Max tval stack = %8ld, tval stack used = %8ld ("INTFORMAT" percent).", 
		gTP->MaxTvalStackSize, gTP->TvalStackIdx, max(1,(gTP->TvalStackIdx/(gTP->MaxTvalStackSize/100))));
	(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);

	sprintf((char*)buf, "Max obj stack = %8ld, used obj stack = %8ld ("INTFORMAT" percent).", 
		gTP->MaxObjStackSize, gTP->ObjStackIdx, max(1,(gTP->ObjStackIdx/(gTP->MaxObjStackSize/100))));
	(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);

    sprintf((char*)buf, "Total forced garbage collections = "INTFORMAT", Max Objects ever used = %8ld ("INTFORMAT" percent).", 
		gCP->FMemory_myForcedGCCounter, gCP->TObject_MaxUsedObjectCount, max(1,(gCP->TObject_MaxUsedObjectCount/(gCP->TObject_MaxObjectCount/100))));
    (*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)buf, 1);
    }


ret.Tag = TYNUM;
ret.u.Int = totalUsedBytes;
return(ret);
}
