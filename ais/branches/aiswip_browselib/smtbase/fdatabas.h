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
FDatabas.h

Interface for the Object Database class which supports the runtime file management 
interface between the Smartbase High Speed Engine and the host file management system.
The data structures, macros, and coding conventions used in this source file are designed
to isolate and protect the Smartbase High Speed Engine from the low level differences
between host file management systems. 

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FDatabas
#define _H_FDatabas

#include "fsmtbase.h"

#define _REPOTESTMODE FALSE


#if 0
FILE ROOT DECLARATION

The file root data record is always the first record in each Smartbase Object
Database File. Smartbase Object Database Files support file page management
for organizing pages of data within the file. The file page management table
grows and contracts as pages are added to or subtracted from the database file.
The file root record contains the current location and length of the file
page management table within the Smartbase Object Database File.

Note:   The first two fields, recordType and recordLength, provide
        compatability with the Smartbase object merge save/load file
        record conventions (see TObject.c and TObject.h).

#endif

typedef struct  {
				CHAR		AisIdentifier[4];			/* ACII null Terminaled string (must be "AIS")	*/
				CHAR		VersionIdentifier[4];		/* ACII null Terminaled numeric-only string (ie "44" see _VERSION in FSmtbase.h)	*/
				SHORT       recordVersion;
                CHAR        recordType;
                NUM         recordLength;
                REAL		SaveTimeStamp;
                NUM         PageTableFPos;
                NUM         PageTableSize;
                } FILEROOT;
                
#define _ROOTRECORDTYPE     100     /* Signature of Root file records   */ 

typedef struct  {
				CHAR		AisIdentifier[4];			/* ACII null Terminaled string (must be "AIS")	*/
				CHAR		VersionIdentifier[4];		/* ACII null Terminaled numeric-only string (ie "44" see _VERSION in FSmtbase.h)	*/
                SHORT		recordVersion;
                CHAR		recordType;
                NUM			recordLength;
				REAL		SaveTimeStamp;
                } DBRECORDHEADER;

#if 0
PAGE TABLE DECLARATION

The page table data record is the main accounting tool for file page management
in each Smartbase Object Database File. Smartbase Object Database Files support 
file page management for organizing pages of data within the file. The file page 
management table grows and contracts as pages are added to or subtracted from 
the database file.

The file root record contains the current location and length of the file
page management table within the Smartbase Object Database File. The file
root record must be updated whenever the page table changes position within 
the Database file

Note:   The first two fields, recordType and recordLength, provide
        compatability with the Smartbase object merge save/load file
        record conventions (see TObject.c and TObject.h).

#endif

#if 0
PAGE TABLE

The Smartbase repository page table uses two bit maps to keep track of which pages are vacant and which pages
are occupied. Each page is represented by a bit in the bit map. Vacant pages are marked as 0 bits. Occupied
pages are marked as 1 bits. Objects are stored in consecutive runs of pages, called frames. Each frame represents
one or more contiguous pages in the bit map.

There are two bit maps so the repository can support unlimited transaction rollback. When the page table is
read from disk, at the start of each transaction, the main page bit map is copied to the used page bit map.
All objects are saved to disk in new frames. Used frames are never reused as this might permanently destroy old
data before a transaction commit, preventing unlimited roll back. During each transaction, new pages are
allocated from the used page bit map; but, old pages are freed only in the main page bit map. Therefore,
until transaction commit, when the page table is saved, no used frames are every altered. This allows unlimited
transaction roll back for all Smartbase repository operations.

#endif
              
typedef struct  {
				CHAR			AisIdentifier[4];		/* ACII null Terminaled string (must be "AIS")				*/
				CHAR			VersionIdentifier[4];	/* ACII null Terminaled numeric-only string					*/
                SHORT			recordVersion;			/* Signature for this Smartbase version.					*/
                CHAR			recordType;				/* Signature for the page table record type.				*/
                NUM				recordLength;			/* Page table length including this header.					*/
                REAL			SaveTimeStamp;			/* Page table last saved on disk time stamp.				*/
                REAL			MetaDataObjectFrame;	/* Frame bit map index where metadata object is saved		*/
                REAL			IndexObjectFrame;		/* Frame bit map index where index directory object is saved*/
                REAL			PageTableFrame;			/* Frame bit map index where page table is saved			*/
                NUM				PageTableFPos;			/* Page table file position on disk.						*/
                NUM				PageTableSize;			/* Page table record size on disk.							*/
				NUM				baseFilePos;			/* Repository base file position (0 for parent Repository).	*/
                NUM				FileSize;				/* File size of all repository data on disk.				*/
                NUM				MaxPageCount;			/* Maximum number of pages in page bit map.					*/
                NUM				UsedPageCount;			/* Count of pages marked occupied in bit map.				*/
                NUM				UsedFileBytes;			/* Sum of bytes in pages marked occupied in bit map.		*/
                NUM				FirstPhysicalPage;		/* Page bit map index of first physical page on disk.		*/
                NUM				LastPhysicalPage;		/* Page bit map index of last physical page on disk.		*/
                BOLE			IAmDirty;				/* True if page table has been altered and not saved to disk*/
                BOLE			RollBackOn;				/* True if unlimited rollback from disk is on.				*/
                BOLE			ReadWriteOn;			/* True if permission to read AND write to disk is on.		*/
                BOLE			TreeIndexOn;			/* True if tree indexing strategy is on.					*/
                REAL			OpenTimeStamp;			/* Repository was last read from disk time stamp.			*/
                NUM				SavedObjectCount;		/* Count of objects saved into repository.					*/
                NUM				PreviousVacantPage;		/* Last bit map index where vacant frame was found.			*/
                NUM				PageMapsByteLen;		/* Byte length of both bit maps (main plus used).			*/
                NUM				PageBitMapByteLen;		/* Byte length of either bit map (main or used).			*/
                unsigned char	PageBitMaps[2];			/* Byte array for both bit maps (main and used).			*/
                } PAGETABLE;
#define SIZEOF_PAGETABLE		((NUM)&((PAGETABLE*)0)->PageBitMaps)
#define REFBIT(bitMap,bitIndex) (((bitMap)[((bitIndex)/8)] & FDatabas_OrMasks[((bitIndex)%8)]) != 0) 
                
/***  Note: all of these algorithms are sensitive to byte boundries, so we take ***/
/***		special care to assure that added pages are in byte increments.	    ***/
#define _PAGERECORDTYPE			101						/* Signature of Page Table records							*/
#define _MINPAGECOUNT			10						/* Minimum Number of Pages at file Startup					*/
#define _INITPAGECOUNT			256						/* Number of Pages at file Startup (either main or used map)*/
#define _FRAMEINDEXSHIFT		130000000				/* Frame index shift factor in frameID (shows decimal size) */
#define _MAXDISKPAGECOUNT		129999999				/* Maximum repository Page count							*/
#define _MAXPAGEMAPBYTECOUNT	32500000				/* Maximum Page Map byte count (both main plus used map)	*/
#define _PAGESIZE				256						/* Page size in bytes on disk								*/
#define _PAGEROUND				(_PAGESIZE - 1)			/* Page rounding amount in bytes on disk					*/
#define NILPAGEINDEX			-1						/* Special identifier for a nil page						*/

#define PAGETABLEFRAMECOUNTFROMSIZE(ptSize)			((NUM)(((ptSize) + _PAGEROUND) / _PAGESIZE))
#define PAGETABLEFRAMESIZEFROMSIZE(ptSize)			((NUM)((((ptSize) + _PAGEROUND) / _PAGESIZE) * _PAGESIZE))
#define PAGETABLESIZEFROMPAGES(maxPages)			PAGETABLEFRAMESIZEFROMSIZE((SIZEOF_PAGETABLE + ((((((maxPages) + 7) / 8) * 8) / 4))))
#define PAGETABLESIZEFROMFILELEN(fileSize)			PAGETABLEFRAMESIZEFROMSIZE(PAGETABLESIZEFROMPAGES(((fileSize) + _PAGEROUND) / _PAGESIZE))
#define PAGETABLEMAXPAGESFROMPAGES(pages)			((NUM)((((pages) + 7) / 8) * 8))
#define PAGETABLEMAXPAGESFROMFILELEN(flen)			PAGETABLEMAXPAGESFROMPAGES((NUM)(((flen) + _PAGEROUND) / _PAGESIZE))

#define FILESIZEFROMFILELEN(flen)					((PAGETABLEMAXPAGESFROMFILELEN(flen) * _PAGESIZE) + sizeof(FILEROOT))
#define FILESIZEFROMPAGES(pages)					((NUM)(((pages) * _PAGESIZE) + sizeof(FILEROOT)))

#define FRAMECOUNTFROMSIZE(theSize)					((NUM)(((theSize) + _PAGEROUND) / _PAGESIZE))
#define MAXPAGESFROMPAGES(pages)					((NUM)((((pages) + 7) / 8) * 8))
#define MAXPAGESFROMSIZE(theSize)					MAXPAGESFROMPAGES(FRAMECOUNTFROMSIZE(theSize))

/*  Function declarations */

extern  TVAL            FDatabas_Open				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_Close				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_NewFrameID			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_FreeFrameID		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_LoadMetaData		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_SaveMetaData		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_LoadIndexStrategy	(LpXCONTEXT gCP,LpTHREAD gTP,TDatabase* self);
extern  TVAL            FDatabas_LoadIndex			(LpXCONTEXT gCP,LpTHREAD gTP,TDatabase* self, TVAL itsCodeKey);
extern  TVAL            FDatabas_SaveIndex			(LpXCONTEXT gCP,LpTHREAD gTP,TByteVector* myOdbID, TVAL newValue, TVAL itsCodeKey);
extern  TVAL            FDatabas_Load				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_Save				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_Free				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_Inspect			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_Encode				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_Decode				(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_Compress			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_Uncompress			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL            FDatabas_Init				(LpXCONTEXT gCP,LpTHREAD gTP);
extern  PAGETABLE*      FDatabas_GetPageTable		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval);
extern  REAL			FDatabas_MakeFrameID		(LpXCONTEXT gCP,LpTHREAD gTP,NUM frameSize,NUM frameIndex);
extern  NUM 			FDatabas_FrameIDToFPos		(LpXCONTEXT gCP,LpTHREAD gTP,REAL frameID);
extern  NUM 			FDatabas_FrameIDToFSize		(LpXCONTEXT gCP,LpTHREAD gTP,REAL frameID);
extern  NUM 			FDatabas_FrameIDToBIndex	(LpXCONTEXT gCP,LpTHREAD gTP,REAL frameID);
extern  NUM 			FDatabas_FrameIDToBCount	(LpXCONTEXT gCP,LpTHREAD gTP,REAL frameID);
extern  TVAL            FDatabas_NewMF				(LpXCONTEXT gCP,LpTHREAD gTP,NUM baseFileSize);
extern  TVAL            FDatabas_AdjustPageTable	(LpXCONTEXT gCP,LpTHREAD gTP,TVAL fileVector,NUM newFileSize);
extern  NUM				FDatabas_readf				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL fileID,NUM base,NUM disp,NUM size,char* data);
extern  NUM				FDatabas_writef				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL fileID,NUM base,NUM disp,NUM size,char* data);
extern  TVAL			FDatabas_readR				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL fileID,NUM base,NUM disp,NUM size,BOLE bvector);
extern  TVAL			FDatabas_writeR				(LpXCONTEXT gCP,LpTHREAD gTP,TVAL fileID,NUM base,NUM disp,TVAL record,BOLE bvector);
extern	NUM				FDatabas_FindFrame			(LpXCONTEXT gCP,LpTHREAD gTP,PAGETABLE* pageTable, NUM requestedSize);
extern	NUM				FDatabas_FreeUsedFrame		(LpXCONTEXT gCP,LpTHREAD gTP,PAGETABLE* pageTable, NUM frameBitIndex, NUM requestedSize);
extern	NUM				FDatabas_FreeFrame			(LpXCONTEXT gCP,LpTHREAD gTP,PAGETABLE* pageTable, NUM frameBitIndex, NUM requestedSize);
extern	NUM				FDatabas_SetFrame			(LpXCONTEXT gCP,LpTHREAD gTP,PAGETABLE* pageTable, NUM frameBitIndex, NUM requestedSize);
extern	NUM				FDatabas_CheckFrame			(LpXCONTEXT gCP,LpTHREAD gTP,PAGETABLE* pageTable, NUM frameBitIndex, NUM requestedSize);
extern	NUM				FDatabas_Validate			(LpXCONTEXT gCP,LpTHREAD gTP,PAGETABLE* pageTable);

extern	void			FDatabas_fast_copy(unsigned char *pSrc,unsigned char *pDst,NUM len);
extern  void            FDatabas_lzrw1_compress	(unsigned char* pIn,NUM InLen,unsigned char* pOut,LpNUM OutLen);
extern  void            FDatabas_lzrw1_decompress(unsigned char* pIn,NUM InLen,unsigned char* pOut,LpNUM OutLen);

#endif

